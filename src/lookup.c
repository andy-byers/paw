// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// lookup.c: Resolve paths referring to toplevel items

#include "hir.h"

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
    return Q->C->dm->modules->data[modno];
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
        seg = pawHir_path_get(path, Q->index++);
        sym = resolve_symbol(Q, seg->name);
        if (sym == NULL) return (struct QueryBase){seg};
        base = sym->decl;
        seg->did = base->hdr.did;
        seg->modno = Q->m->hir->modno;
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

static struct HirType *expect_field(struct QueryState *Q, struct HirType *adt, String *name)
{
    struct HirDecl *field = pawP_find_field(Q->C, adt, name);
    if (field == NULL) {
        struct HirDecl *decl = pawHir_get_decl(Q->C, HirGetAdt(adt)->did);
        NAME_ERROR(Q, "field '%s' does not exist on type '%d'",
                name->text, decl->hdr.name->text);
    }
    return pawP_instantiate_field(Q->C, adt, field);
}

static struct HirType *locate_method(struct QueryState *Q, struct HirType *type, struct HirSegment *seg)
{
    struct HirType *inst = expect_field(Q, type, seg->name);
    seg->did = HIR_TYPE_DID(inst);
    seg->modno = Q->m->hir->modno;
    return inst;
}

static struct HirType *locate_enumerator(struct QueryState *Q, struct HirType *type, struct HirSegment *seg)
{
    if (seg->types != NULL) NAME_ERROR(Q, "unexpected type arguments on enumerator '%s'", seg->name->text);
    struct HirType *result = expect_field(Q, type, seg->name);
    seg->did = HIR_TYPE_DID(result);
    seg->modno = Q->m->hir->modno;
    return result;
}

static struct HirType *locate_assoc_item(struct QueryState *Q, struct HirDecl *prev, struct HirType *type, struct HirSegment *next)
{
    struct HirDecl *decl = pawHir_get_decl(Q->C, prev->hdr.did);
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    return d->is_struct
        ? locate_method(Q, type, next)
        : locate_enumerator(Q, type, next);
}

static void maybe_generalize_adt(struct QueryState *Q, struct HirDecl *decl, struct HirSegment *seg)
{
    if (seg->types != NULL) return;
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    if (d->generics == NULL) return;
    seg->types = pawHir_type_list_new(Q->C);
    for (int i = 0; i < d->generics->count; ++i) {
        struct HirType *var = pawU_new_unknown(Q->C->U, Q->line);
        pawHir_type_list_push(Q->C, seg->types, var);
    }
}

struct HirType *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind)
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

    struct HirType *inst;
    switch (HIR_KINDOF(q.base)) {
        case kHirAdtDecl:
            maybe_generalize_adt(&Q, q.base, q.seg);
            // (fallthrough)
        case kHirFuncDecl:
        case kHirTypeDecl:
            inst = pawP_instantiate(C, q.base, q.seg->types);
            q.seg->did = HIR_TYPE_DID(inst);
            break;
        case kHirVarDecl:
        case kHirFieldDecl:
        case kHirGenericDecl:
            q.seg->did = q.base->hdr.did;
            inst = q.base->hdr.type;
            break;
        default:
            goto invalid_path;
    }

    if (Q.index < path->count) {
        // resolve method or enumerator
        struct HirSegment *next = K_LIST_GET(path, Q.index);
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

