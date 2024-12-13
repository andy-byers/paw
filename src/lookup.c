// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// lookup.c: Resolve paths referring to toplevel items

#include "hir.h"
#include "ir_type.h"

struct QueryState {
    struct Compiler *C;
    struct ModuleInfo *m;
    struct HirAdtDecl *adt;
    struct HirSymtab *symtab;
    paw_Env *P;
    int target;
    int index;
    int line; // TODO: never set
};

static struct ModuleInfo *get_module(struct QueryState *Q, int modno)
{
    paw_assert(modno < Q->C->dm->modules->count);
    return K_LIST_GET(Q->C->dm->modules, modno);
}

static struct HirSymbol *resolve_symbol(struct QueryState *Q, const String *name)
{
    // search the scoped symbols
    struct HirSymtab *scopes = Q->symtab;
    const int nscopes = scopes->count;
    for (int depth = nscopes - 1; depth >= 0; --depth) {
        struct HirScope *scope = scopes->data[depth];
        const int index = pawHir_find_symbol(scope, name);
        if (index >= 0) {
            struct HirSymbol *symbol = scope->data[index];
            return scope->data[index];
        }
    }

    // search the global symbols
    const int index = pawHir_find_symbol(Q->m->globals, name);
    if (index < 0) return NULL;
    return Q->m->globals->data[index];
}


static paw_Bool should_admit(struct QueryState *Q, struct HirUseDecl *use)
{
    const int modno = Q->m->hir->modno;
    if (use->is_pub) NAME_ERROR(Q, "TODO: 'pub' use decls"); // TODO
    if (modno == Q->target) return PAW_TRUE;
    return PAW_TRUE; // TODO: reexport: use->is_pub;
}

struct QueryBase {
    struct HirSegment *seg;
    struct HirDecl *base;
    paw_Bool is_type;
};

static struct QueryBase locate_base_in(struct QueryState *Q, struct ModuleInfo *root, struct HirPath *path)
{
    struct HirDecl *base;
    struct HirSegment *seg;
    struct HirSymbol *sym;
    for (Q->m = root, Q->index = 0; Q->index < path->count;) {
        seg = &K_LIST_GET(path, Q->index++);
        sym = resolve_symbol(Q, seg->name);
        if (sym == NULL) return (struct QueryBase){seg};
        base = sym->decl;

        // NOTE: the caller will need to set '.did' to the DeclId of the instance, if seg->types is nonnull
        seg->did = base->hdr.did;
        if (!HirIsUseDecl(base)) break;
        struct HirUseDecl *use = HirGetUseDecl(base);
        if (!should_admit(Q, use)) {
            NAME_ERROR(Q, "'%s' has private visibility", use->name->text);
        }
        Q->m = get_module(Q, use->modno);
    }
    return (struct QueryBase){seg, base, sym->is_type};
}

static struct QueryBase locate_base(struct QueryState *Q, struct ModuleInfo *m, struct HirPath *path)
{
    struct QueryBase q = locate_base_in(Q, m, path);
    if (q.base == NULL) {
        struct ModuleInfo *prelude = Q->C->dm->modules->data[0];
        q = locate_base_in(Q, prelude, path);
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

static struct IrType *locate_method(struct QueryState *Q, struct IrType *type, struct HirSegment *seg)
{
    struct IrType *method = pawP_find_method(Q->C, type, seg->name);
    if (method == NULL) {
        struct HirDecl *decl = pawHir_get_decl(Q->C, IR_TYPE_DID(type));
        const char *base_repr = pawIr_print_type(Q->C, type);
        NAME_ERROR(Q, "field '%s' does not exist on type '%s'",
                seg->name->text, base_repr);
    }
    seg->did = IR_TYPE_DID(method);
    return method;
}

static struct IrType *locate_enumerator(struct QueryState *Q, struct IrType *type, struct HirSegment *seg)
{
    if (seg->types != NULL) NAME_ERROR(Q, "unexpected type arguments on enumerator '%s'", seg->name->text);
    struct HirDecl *field = pawP_find_field(Q->C, type, seg->name);
    if (field == NULL) return NULL;
    struct IrType *inst = pawP_instantiate_field(Q->C, type, field);
    seg->did = IR_TYPE_DID(inst);
    return inst;
}

static struct IrType *locate_assoc_item(struct QueryState *Q, struct HirDecl *prev, struct IrType *type, struct HirSegment *next)
{
    struct HirDecl *decl = pawHir_get_decl(Q->C, prev->hdr.did);
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    if (!d->is_struct) {
        struct IrType *result = locate_enumerator(Q, type, next);
        if (result != NULL) return result;
    }
    return locate_method(Q, type, next);
}

static struct IrTypeList *maybe_generalize_adt(struct QueryState *Q, struct HirDecl *decl, struct IrTypeList *types)
{
    if (types != NULL) return types;
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    if (d->generics == NULL) return types;
    struct IrTypeList *result = pawIr_type_list_new(Q->C);
    for (int i = 0; i < d->generics->count; ++i) {
        struct IrType *var = pawU_new_unknown(Q->C->U, Q->line);
        K_LIST_PUSH(Q->C, result, var);
    }
    return result;
}

struct IrType *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind)
{
    struct QueryState Q = {
        .symtab = symtab,
        .P = ENV(C),
        .C = C,
    };

    paw_assert(path->count > 0);
    // sets 'Q.m' to the module containing the target declaration, and set
    // 'Q.index' to the index of the following segment
    struct QueryBase q = locate_base(&Q, m, path);
    if (q.base == NULL) return NULL;

    struct IrTypeList *types = q.seg->types != NULL
        ? pawP_lower_type_list(C, m, symtab, q.seg->types)
        : NULL;

    struct IrType *inst;
    switch (HIR_KINDOF(q.base)) {
        case kHirAdtDecl:
            types = maybe_generalize_adt(&Q, q.base, types);
            // (fallthrough)
        case kHirFuncDecl:
        case kHirTypeDecl:
            inst = pawP_instantiate(C, q.base, types);
            q.seg->did = IR_TYPE_DID(inst);
            break;
        case kHirVarDecl:
        case kHirFieldDecl:
        case kHirGenericDecl:
            inst = GET_NODE_TYPE(C, q.base);
            q.seg->did = q.base->hdr.did;
            break;
        default:
            goto invalid_path;
    }

    if (Q.index < path->count) {
        // resolve method or enumerator
        struct HirSegment *next = &K_LIST_GET(path, Q.index);
        inst = locate_assoc_item(&Q, q.base, inst, next);
        q.is_type = PAW_FALSE;
    }

    if ((kind == LOOKUP_VALUE && q.is_type) ||
            (kind == LOOKUP_TYPE && !q.is_type)) {
invalid_path:
        TYPE_ERROR(&Q, "expected %s", q.is_type ? "value" : "type");
    }
    return inst;
}

