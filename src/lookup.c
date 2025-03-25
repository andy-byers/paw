// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// lookup.c: Resolve HirPath to a DeclId and IrType.

#include "error.h"
#include "hir.h"
#include "ir_type.h"
#include "type_folder.h"
#include "unify.h"

#define LOOKUP_ERROR(X_, Kind_, ...) pawErr_##Kind_((X_)->C, (X_)->m->name, __VA_ARGS__)

struct QueryState {
    struct Compiler *C;
    struct ModuleInfo *m;
    struct ModuleInfo *base;
    struct HirAdtDecl *adt;
    struct HirSymtab *symtab;
    paw_Env *P;
    int index;
};

#define Q_NAME_ERROR(Q_, Loc_, ...) pawP_error((Q_)->C, PAW_ENAME, (Q_)->m->name, Loc_, __VA_ARGS__)
#define Q_TYPE_ERROR(Q_, Loc_, ...) pawP_error((Q_)->C, PAW_ETYPE, (Q_)->m->name, Loc_, __VA_ARGS__)
#define Q_VALUE_ERROR(Q_, Loc_, ...) pawP_error((Q_)->C, PAW_EVALUE, (Q_)->m->name, Loc_, __VA_ARGS__)

static int module_number(struct ModuleInfo *m)
{
    return m->hir->modno;
}

static struct ModuleInfo *get_module(struct QueryState *Q, int modno)
{
    return ModuleList_get(Q->C->modules, modno);
}

static struct HirResult result_decl(DeclId did)
{
    return (struct HirResult){
        .kind = HIR_RESULT_DECL,
        .did = did,
    };
}

static struct ModuleInfo *find_import(struct QueryState *Q, String *name)
{
    struct HirImport *im;
    K_LIST_FOREACH (Q->m->hir->imports, im) {
        if (im->has_star)
            continue; // checked later
        struct ModuleInfo *m = get_module(Q, im->modno);
        // use alias, i.e. "use mod as alias", if it exists
        String const *target = im->as.name == NULL ? m->hir->name : im->as.name;
        if (pawS_eq(name, target))
            return m;
    }
    return NULL;
}

static void validate_type_args(struct QueryState *Q, struct HirDecl *decl, struct HirSegment *seg)
{
    struct HirDeclList *generics = NULL;
    if (HirIsTraitDecl(decl)) {
        generics = HirGetTraitDecl(decl)->generics;
    } else if (HirIsFuncDecl(decl)) {
        generics = HirGetFuncDecl(decl)->generics;
    } else if (HirIsAdtDecl(decl)) {
        generics = HirGetAdtDecl(decl)->generics;
    } else if (HirIsTypeDecl(decl)) {
        generics = HirGetTypeDecl(decl)->generics;
    }
    int const m = seg->types == NULL ? 0 : seg->types->count;
    int const n = generics == NULL ? 0 : generics->count;
    if (m != n)
        LOOKUP_ERROR(Q, incorrect_type_arity, seg->ident.span.start, m, n);
}

static struct HirSymbol *resolve_global(struct QueryState *Q, struct HirIdent ident)
{
    // search the global symbols
    int const index = pawHir_find_symbol(Q->m->globals, ident);
    if (index < 0)
        return NULL;
    return &K_LIST_AT(Q->m->globals, index);
}

struct QueryBase {
    struct HirSegment *seg;
};

static void ensure_accessible(struct QueryState *Q, struct HirDecl *decl)
{
    if (!pawHir_is_pub_decl(decl) && module_number(Q->m) != module_number(Q->base))
        LOOKUP_ERROR(Q, item_visibility, decl->hdr.span.start, Q->m->name->text,
                hir_decl_ident(decl).name->text);
}

static struct QueryBase find_global_in(struct QueryState *Q, struct ModuleInfo *root, struct HirPath *path)
{
    paw_assert(path->segments->count > 0);

    Q->m = root;
    Q->index = 0;
    do {
        struct HirSegment *seg = &K_LIST_AT(path->segments, Q->index++);
        struct HirSymbol *sym = resolve_global(Q, seg->ident);
        if (sym != NULL) {
            // all globals are declarations
            paw_assert(sym->res.kind == HIR_RESULT_DECL);
            struct HirDecl *decl = pawHir_get_decl(Q->C, sym->res.did);
            ensure_accessible(Q, decl);
            seg->res = sym->res;
            return (struct QueryBase){seg};
        }
        struct ModuleInfo *m = find_import(Q, seg->ident.name);
        if (m == NULL)
            break;
        if (Q->index >= path->segments->count)
            pawE_error(ENV(Q), PAW_ETYPE, -1, "unexpected module name");
        if (module_number(Q->m) != module_number(Q->base))
            pawE_error(ENV(Q), PAW_ESYNTAX, -1, "transitive imports are not supported");
        Q->m = m;
    } while (Q->index < path->segments->count);
    return (struct QueryBase){0};
}

static struct QueryBase find_global(struct QueryState *Q, struct ModuleInfo *root, struct HirPath *path)
{
    struct QueryBase q = find_global_in(Q, root, path);
    if (q.seg != NULL)
        return q;

    struct ModuleInfo *prelude = get_module(Q, 0);
    q = find_global_in(Q, prelude, path);
    if (q.seg != NULL)
        return q;

    struct HirImport *im;
    K_LIST_FOREACH (root->hir->imports, im) {
        if (!im->has_star)
            continue;
        struct ModuleInfo *m = get_module(Q, im->modno);
        q = find_global_in(Q, m, path);
        if (q.seg != NULL)
            break;
    }
    return q;
}

static struct IrType *expect_field(struct QueryState *Q, struct IrType *adt, struct HirIdent ident)
{
    struct HirDecl *field = pawP_find_field(Q->C, adt, ident.name);
    if (field == NULL) {
        struct HirDecl *decl = pawHir_get_decl(Q->C, IrGetAdt(adt)->did);
        LOOKUP_ERROR(Q, unknown_field, ident.span.start, ident.name->text,
                hir_decl_ident(decl).name->text);
    }
    return pawP_instantiate_field(Q->C, adt, field);
}

static struct IrType *find_method(struct QueryState *Q, struct IrType *type, struct HirSegment *seg)
{
    struct IrType *method = pawP_find_method(Q->C, type, seg->ident.name);
    if (method == NULL)
        return NULL;

    pawIr_set_type(Q->C, seg->hid, method);
    seg->res = result_decl(IR_TYPE_DID(method));
    return method;
}

static struct IrType *find_enumerator(struct QueryState *Q, struct IrType *type, struct HirSegment *seg)
{
    if (seg->types != NULL)
        LOOKUP_ERROR(Q, unexpected_type_arguments, seg->ident.span.start,
                "enumerator", seg->ident.name->text);
    struct HirDecl *field = pawP_find_field(Q->C, type, seg->ident.name);
    if (field == NULL)
        return NULL;
    struct IrType *inst = pawP_instantiate_field(Q->C, type, field);
    pawIr_set_type(Q->C, seg->hid, inst);
    seg->res = result_decl(IR_TYPE_DID(inst));
    return inst;
}

static struct IrType *find_assoc_item(struct QueryState *Q, struct IrType *type, struct HirSegment *next)
{
    struct HirDecl *prev = pawHir_get_decl(Q->C, IR_TYPE_DID(type));
    if (HirIsGenericDecl(prev)) {
        struct IrType *method = pawIr_resolve_trait_method(Q->C, IrGetGeneric(type), next->ident.name);
        if (method == NULL)
            LOOKUP_ERROR(Q, unknown_method, next->ident.span.start, next->ident.name->text,
                    hir_decl_ident(prev).name->text);
        pawIr_set_type(Q->C, next->hid, method);
        next->res = result_decl(IR_TYPE_DID(method));
        return method;
    } else if (!HirGetAdtDecl(prev)->is_struct) {
        struct IrType *result = find_enumerator(Q, type, next);
        if (result != NULL)
            return result;
    }
    struct IrType *result = find_method(Q, type, next);
    if (result == NULL) {
        struct HirDecl *decl = pawHir_get_decl(Q->C, IR_TYPE_DID(type));
        char const *base_repr = pawIr_print_type(Q->C, type);
        LOOKUP_ERROR(Q, unknown_associated_item, next->ident.span.start, next->ident.name->text, base_repr);
    }
    return result;
}

static struct IrTypeList *maybe_generalize_adt(struct QueryState *Q, struct HirDecl *decl, struct IrTypeList *types)
{
    if (types != NULL)
        return types;
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    if (d->generics == NULL)
        return types;
    struct IrTypeList *generics = pawHir_collect_decl_types(Q->C, d->generics);
    struct IrTypeList *unknowns = pawU_new_unknowns(Q->C->U, generics);
    return pawP_instantiate_typelist(Q->C, generics, unknowns, generics);
}

static struct QueryBase find_local(struct QueryState *Q, struct HirSymtab *symtab, struct HirPath *path)
{
    struct HirSegment *seg = &K_LIST_FIRST(path->segments);
    for (int depth = symtab->count - 1; depth >= 0; --depth) {
        struct HirScope *scope = HirSymtab_get(symtab, depth);
        int const index = pawHir_find_symbol(scope, seg->ident);
        if (index >= 0) {
            struct IrType *type;
            struct HirSymbol symbol = HirScope_get(scope, index);
            if (symbol.res.kind == HIR_RESULT_LOCAL) {
                if (seg->types != NULL)
                    LOOKUP_ERROR(Q, unexpected_type_arguments, seg->ident.span.start,
                            "value", seg->ident.name->text);
                type = pawIr_get_type(Q->C, symbol.res.hid);
            } else {
                struct HirDecl *decl = pawHir_get_decl(Q->C, symbol.res.did);
                type = GET_NODE_TYPE(Q->C, decl);
            }

            Q->index = 1;
            pawIr_set_type(Q->C, seg->hid, type);
            seg->res = symbol.res;
            return (struct QueryBase){
                .seg = seg,
            };
        }
    }
    return (struct QueryBase){0};
}

static struct IrTypeList *new_unknowns(struct Compiler *C, struct IrTypeList *generics)
{
    struct IrTypeList *list = IrTypeList_new(C);
    IrTypeList_reserve(C, list, generics->count);

    struct IrType **pgeneric;
    K_LIST_FOREACH (generics, pgeneric) {
        struct IrTypeList *bounds = IrGetGeneric(*pgeneric)->bounds;
        struct IrType *unknown = pawU_new_unknown(C->U, bounds);
        IrTypeList_push(C, list, unknown);
    }

    return list;
}

static struct IrType *resolve_alias(struct QueryState *Q, struct HirSegment *seg, struct HirDecl *decl, struct IrTypeList *knowns)
{
    paw_assert(HirIsTypeDecl(decl));
    struct IrType *type = GET_NODE_TYPE(Q->C, decl);
    struct HirTypeDecl *d = HirGetTypeDecl(decl);

    // TODO: is this correct? prob. needs to be instantiated, "seg" likely not used later so it seems to work
    pawIr_set_type(Q->C, seg->hid, type);
    seg->res = result_decl(IR_TYPE_DID(type));
    decl = pawHir_get_decl(Q->C, seg->res.did);

    if (d->rhs == NULL)
        return type;
    struct IrType *rhs = GET_NODE_TYPE(Q->C, d->rhs);
    struct IrTypeList *types = IR_TYPE_SUBTYPES(rhs);
    if (d->generics == NULL)
        return rhs;

    struct IrTypeList *generics = pawHir_collect_decl_types(Q->C, d->generics);
    struct IrTypeList *unknowns = new_unknowns(Q->C, generics);
    struct IrTypeList *subst = pawP_instantiate_typelist(Q->C, generics, unknowns, types);
    if (knowns != NULL) {
        struct IrType **pu, **pk;
        K_LIST_ZIP (unknowns, pu, knowns, pk)
            pawU_unify(Q->C->U, *pu, *pk);
    }

    return pawP_instantiate(Q->C, rhs, subst);
}

static IrType *result_type(struct QueryState *Q, struct HirSegment *seg, IrTypeList *types, paw_Bool is_annotation)
{
    if (seg->res.kind == HIR_RESULT_LOCAL)
        return pawIr_get_type(Q->C, seg->res.hid);

    struct HirDecl *decl = pawHir_get_decl(Q->C, seg->res.did);
    if (is_annotation)
        // a type argument must be provided for each type parameter
        validate_type_args(Q, decl, seg);

    IrType *inst;
    switch (HIR_KINDOF(decl)) {
        case kHirTraitDecl:
            LOOKUP_ERROR(Q, unexpected_trait, seg->ident.span.start);
        case kHirAdtDecl:
            types = maybe_generalize_adt(Q, decl, types);
            // (fallthrough)
        case kHirFuncDecl:
            inst = pawP_instantiate(Q->C, GET_NODE_TYPE(Q->C, decl), types);
            pawIr_set_type(Q->C, seg->hid, inst);
            break;
        case kHirTypeDecl:
            inst = resolve_alias(Q, seg, decl, types);
            break;
        case kHirConstDecl:
        case kHirGenericDecl:
            inst = GET_NODE_TYPE(Q->C, decl);
            pawIr_set_type(Q->C, seg->hid, inst);
            break;
        case kHirFieldDecl:
        case kHirVariantDecl:
            PAW_UNREACHABLE();
    }
    return inst;
}

struct IrType *lookup(struct QueryState *Q, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind, paw_Bool is_annotation)
{
    struct HirSegments *segments = path->segments;
    paw_assert(segments->count > 0);
    struct QueryBase q = find_local(Q, symtab, path);
    if (q.seg == NULL) {
        q = find_global(Q, m, path);
        if (q.seg == NULL)
            return NULL;
    }
    IrTypeList *types = q.seg->types == NULL //
        ? NULL : pawP_lower_type_list(Q->C, m, symtab, q.seg->types);

    IrType *inst = result_type(Q, q.seg, types, is_annotation);
    if (Q->index < segments->count) {
        // resolve method or enumerator
        q.seg = &K_LIST_AT(segments, Q->index);
        inst = find_assoc_item(Q, inst, q.seg);
        ++Q->index;
    }
    if (Q->index < segments->count)
        LOOKUP_ERROR(Q, extra_segment, path->span.start,
                HirSegments_get(segments, Q->index).ident.name->text);

    paw_Bool const is_type = hir_is_type(Q->C, q.seg->res);
    if (kind != LOOKUP_EITHER && is_type != (kind == LOOKUP_TYPE))
        LOOKUP_ERROR(Q, incorrect_item_class, q.seg->ident.span.start,
                   kind == LOOKUP_VALUE ? "value" : "type",
                   is_type ? "type" : "value");

    return inst;
}

struct IrType *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind, paw_Bool is_annotation)
{
    struct QueryState Q = {
        .symtab = symtab,
        .P = ENV(C),
        .base = m,
        .m = m,
        .C = C,
    };

    return lookup(&Q, m, symtab, path, kind, is_annotation);
}

struct IrType *lookup_trait(struct QueryState *Q, struct ModuleInfo *root, struct HirPath *path)
{
    struct HirSegments *segments = path->segments;
    paw_assert(segments->count > 0);
    struct QueryBase q = find_global(Q, root, path);
    if (q.seg == NULL)
        return NULL;

    struct HirResult const res = q.seg->res;
    paw_assert(res.kind == HIR_RESULT_DECL);

    struct HirDecl *decl = pawHir_get_decl(Q->C, res.did);
    if (!HirIsTraitDecl(decl))
        LOOKUP_ERROR(Q, expected_trait, decl->hdr.span.start);

    if (Q->index < segments->count)
        LOOKUP_ERROR(Q, extra_segment, path->span.start,
                HirSegments_get(segments, Q->index).ident.name->text);
    validate_type_args(Q, decl, q.seg);

    if (q.seg->types == NULL)
        return GET_NODE_TYPE(Q->C, decl);

    struct IrTypeList *types = pawP_lower_type_list(Q->C, root, Q->symtab, q.seg->types);
    return pawP_instantiate(Q->C, GET_NODE_TYPE(Q->C, decl), types);
}

struct IrType *pawP_lookup_trait(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path)
{
    struct QueryState Q = {
        .symtab = symtab,
        .P = ENV(C),
        .base = m,
        .m = m,
        .C = C,
    };

    return lookup_trait(&Q, m, path);
}
