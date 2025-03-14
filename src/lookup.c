// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// lookup.c: Resolve HirPath to a DeclId and IrType.

#include "hir.h"
#include "ir_type.h"
#include "type_folder.h"
#include "unify.h"

struct QueryState {
    struct Compiler *C;
    struct ModuleInfo *m;
    struct HirAdtDecl *adt;
    struct HirSymtab *symtab;
    paw_Env *P;
    int base_modno;
    int index;
    int line; // TODO: never set
};

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
        String const *target = im->as == NULL ? m->hir->name : im->as;
        if (pawS_eq(name, target))
            return m;
    }
    return NULL;
}

static void validate_type_args(struct Compiler *C, struct HirDecl *decl, struct HirSegment *seg)
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
        TYPE_ERROR(C, "%s type arguments (expected %d but found %d)",
                   m < n ? "not enough" : "too many", n, m);
}

static struct HirSymbol *resolve_global(struct QueryState *Q, String const *name)
{
    // search the global symbols
    int const index = pawHir_find_symbol(Q->m->globals, name);
    if (index < 0)
        return NULL;
    return &K_LIST_AT(Q->m->globals, index);
}

struct QueryBase {
    struct HirSegment *seg;
};

static void ensure_accessible(struct QueryState *Q, struct HirDecl *decl)
{
    if (!pawHir_is_pub_decl(decl) && module_number(Q->m) != Q->base_modno)
        pawE_error(ENV(Q), PAW_EVALUE, -1, "item '%s' cannot be accessed from the current module",
                   decl->hdr.name->text);
}

static struct QueryBase find_global_in(struct QueryState *Q, struct ModuleInfo *root, struct HirPath *path)
{
    paw_assert(path->count > 0);

    Q->m = root;
    Q->index = 0;
    do {
        struct HirSegment *seg = &K_LIST_AT(path, Q->index++);
        struct HirSymbol *sym = resolve_global(Q, seg->name);
        if (sym != NULL) {
            // all globals are declarations
            paw_assert(sym->res.kind == HIR_RESULT_DECL);
            struct HirDecl *decl = pawHir_get_decl(Q->C, sym->res.did);
            ensure_accessible(Q, decl);
            seg->res = sym->res;
            return (struct QueryBase){seg};
        }
        struct ModuleInfo *m = find_import(Q, seg->name);
        if (m == NULL)
            break;
        if (Q->index >= path->count)
            pawE_error(ENV(Q), PAW_ETYPE, -1, "unexpected module name");
        if (module_number(Q->m) != Q->base_modno)
            pawE_error(ENV(Q), PAW_ESYNTAX, -1, "transitive imports are not supported");
        Q->m = m;
    } while (Q->index < path->count);
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

static struct IrType *expect_field(struct QueryState *Q, struct IrType *adt, String *name)
{
    struct HirDecl *field = pawP_find_field(Q->C, adt, name);
    if (field == NULL) {
        struct HirDecl *decl = pawHir_get_decl(Q->C, IrGetAdt(adt)->did);
        NAME_ERROR(Q, "field '%s' does not exist on type '%d'",
                   name->text, decl->hdr.name->text);
    }
    return pawP_instantiate_field(Q->C, adt, field);
}

static struct IrType *find_method(struct QueryState *Q, struct IrType *type, struct HirSegment *seg)
{
    struct IrType *method = pawP_find_method(Q->C, type, seg->name);
    if (method == NULL) {
        struct HirDecl *decl = pawHir_get_decl(Q->C, IR_TYPE_DID(type));
        char const *base_repr = pawIr_print_type(Q->C, type);
        NAME_ERROR(Q, "field '%s' does not exist on type '%s'",
                   seg->name->text, base_repr);
    }
    pawIr_set_type(Q->C, seg->hid, method);
    seg->res = result_decl(IR_TYPE_DID(method));
    return method;
}

static struct IrType *find_enumerator(struct QueryState *Q, struct IrType *type, struct HirSegment *seg)
{
    if (seg->types != NULL)
        NAME_ERROR(Q, "unexpected type arguments on enumerator '%s'", seg->name->text);
    struct HirDecl *field = pawP_find_field(Q->C, type, seg->name);
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
        struct IrType *method = pawIr_resolve_trait_method(Q->C, IrGetGeneric(type), next->name);
        if (method == NULL)
            NAME_ERROR(Q, "TODO: bad thing happened, figure out message");
        pawIr_set_type(Q->C, next->hid, method);
        next->res = result_decl(IR_TYPE_DID(method));
        return method;
    } else if (!HirGetAdtDecl(prev)->is_struct) {
        struct IrType *result = find_enumerator(Q, type, next);
        if (result != NULL)
            return result;
    }
    return find_method(Q, type, next);
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
    struct HirSegment *seg = &K_LIST_FIRST(path);
    for (int depth = symtab->count - 1; depth >= 0; --depth) {
        struct HirScope *scope = HirSymtab_get(symtab, depth);
        int const index = pawHir_find_symbol(scope, seg->name);
        if (index >= 0) {
            struct IrType *type;
            struct HirSymbol symbol = HirScope_get(scope, index);
            if (symbol.res.kind == HIR_RESULT_LOCAL) {
                if (seg->types != NULL)
                    TYPE_ERROR(Q->C, "type arguments applied to value '%s'", seg->name->text);
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
        struct IrType *unknown = pawU_new_unknown(C->U, -1, bounds);
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
        validate_type_args(Q->C, decl, seg);

    IrType *inst;
    switch (HIR_KINDOF(decl)) {
        case kHirTraitDecl:
            TYPE_ERROR(Q->C, "'%s' is a trait", seg->name->text);
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
        case kHirVarDecl:
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
    paw_assert(path->count > 0);
    struct QueryBase q = find_local(Q, symtab, path);
    if (q.seg == NULL) {
        q = find_global(Q, m, path);
        if (q.seg == NULL)
            return NULL;
    }
    IrTypeList *types = q.seg->types == NULL //
        ? NULL : pawP_lower_type_list(Q->C, m, symtab, q.seg->types);

    IrType *inst = result_type(Q, q.seg, types, is_annotation);
    if (Q->index < path->count) {
        // resolve method or enumerator
        q.seg = &K_LIST_AT(path, Q->index);
        inst = find_assoc_item(Q, inst, q.seg);
        ++Q->index;
    }
    if (Q->index < path->count)
        TYPE_ERROR(Q, "extraneous '::%s'", HirPath_get(path, Q->index).name->text);

    paw_Bool const is_type = hir_is_type(Q->C, q.seg->res);
    if (kind != LOOKUP_EITHER && is_type != (kind == LOOKUP_TYPE))
        TYPE_ERROR(Q, "expected %s but found %s",
                   kind == LOOKUP_VALUE ? "value" : "type",
                   is_type ? "type" : "value");

    return inst;
}

struct IrType *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind, paw_Bool is_annotation)
{
    struct QueryState Q = {
        .base_modno = module_number(m),
        .symtab = symtab,
        .P = ENV(C),
        .m = m,
        .C = C,
    };

    return lookup(&Q, m, symtab, path, kind, is_annotation);
}

struct IrType *lookup_trait(struct QueryState *Q, struct ModuleInfo *root, struct HirPath *path)
{
    paw_assert(path->count > 0);
    struct QueryBase q = find_global(Q, root, path);
    if (q.seg == NULL)
        return NULL;

    struct HirResult const res = q.seg->res;
    if (res.kind != HIR_RESULT_DECL)
        TYPE_ERROR(Q, "expected trait but found local variable");

    struct HirDecl *decl = pawHir_get_decl(Q->C, res.did);
    if (!HirIsTraitDecl(decl))
        TYPE_ERROR(Q, "expected trait");

    if (Q->index < path->count)
        TYPE_ERROR(Q, "extraneous '::%s'", HirPath_get(path, Q->index).name->text);
    validate_type_args(Q->C, decl, q.seg);

    if (q.seg->types == NULL)
        return GET_NODE_TYPE(Q->C, decl);

    struct IrTypeList *types = pawP_lower_type_list(Q->C, root, Q->symtab, q.seg->types);
    return pawP_instantiate(Q->C, GET_NODE_TYPE(Q->C, decl), types);
}

struct IrType *pawP_lookup_trait(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path)
{
    struct QueryState Q = {
        .base_modno = module_number(m),
        .symtab = symtab,
        .P = ENV(C),
        .m = m,
        .C = C,
    };

    return lookup_trait(&Q, m, path);
}
