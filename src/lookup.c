// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// lookup.c: Resolve paths referring to toplevel items

#include "hir.h"

// TODO: line info, but also need module name...
#define NAME_ERROR(Q, ...) pawE_error(ENV(Q->C), PAW_ENAME, -1, __VA_ARGS__)

struct QueryState {
    struct Compiler *C;
    struct ModuleInfo *m;
    int target;
    int index;
};

static struct ModuleInfo *get_module(struct QueryState *Q, int modno)
{
    paw_assert(modno < Q->C->dm->modules->count);
    return Q->C->dm->modules->data[modno];
}


static struct HirDecl *resolve_symbol(struct QueryState *Q, struct ModuleInfo *m, const String *name)
{
    // TODO: prevent duplicate 'use' decls with different visibility, currently finds the last one added
    //       prevent infinite recursion when, say, module 'b' has 'use a::*' and module 'b' has 'use a::*'
    for (int i = 0; i < m->hir->items->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(m->hir->items, i);
        if (pawS_eq(decl->hdr.name, name)) return decl;
    }
    return NULL;
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
};

static struct QueryBase locate_base_in(struct QueryState *Q, struct ModuleInfo *root, struct HirPath *path)
{
    struct HirDecl *base;
    struct HirSegment *seg;
    for (Q->m = root, Q->index = 0; Q->index < path->count;) {
        seg = pawHir_path_get(path, Q->index++);
        base = resolve_symbol(Q, Q->m, seg->name);
        if (base == NULL) return (struct QueryBase){seg};
        seg->base = seg->did = base->hdr.did;
        seg->modno = Q->m->hir->modno;
        if (!HirIsUseDecl(base)) break;
        struct HirUseDecl *use = HirGetUseDecl(base);
        if (!should_admit(Q, use)) {
            NAME_ERROR(Q, "'%s' has private visibility", use->name->text);
        }
        Q->m = get_module(Q, use->modno);
    }
    return (struct QueryBase){seg, base};
}

static struct HirDecl *locate_base_decl(struct QueryState *Q, struct ModuleInfo *m, struct HirPath *path)
{
    struct QueryBase q = locate_base_in(Q, m, path);
    if (q.base == NULL) {
        struct ModuleInfo *prelude = Q->C->dm->modules->data[0];
        q = locate_base_in(Q, prelude, path);
        if (q.base == NULL) {
            struct HirSegment *seg = K_LIST_GET(path, path->count - 1);
            NAME_ERROR(Q, "'%s' does not exist in module '%s'",
                    seg->name->text, m->hir->name->text);
        }
    }

    struct HirDecl *inst = pawP_instantiate(Q->m->hir, q.base, q.seg->types);
    q.seg->did = inst->hdr.did;
    return inst;
}

static struct HirDecl *expect_field(struct QueryState *Q, struct HirAdtDecl *d, String *name)
{
    struct HirDecl *result = pawP_find_field(Q->C, Q->m->hir, d, name);
    if (result == NULL) {
        NAME_ERROR(Q, "field '%s' does not exist on type '%d'",
                name->text, d->name->text);
    }
    return result;
}

static struct HirDecl *locate_method(struct QueryState *Q, struct HirAdtDecl *d, struct HirSegment *seg)
{
    struct HirDecl *base = expect_field(Q, d, seg->name);
    struct HirDecl *inst = pawP_instantiate(Q->m->hir, base, seg->types);
    seg->modno = Q->m->hir->modno;
    seg->base = base->hdr.did;
    seg->did = inst->hdr.did;
    return inst;
}

static struct HirDecl *locate_enumerator(struct QueryState *Q, struct HirAdtDecl *d, struct HirSegment *seg)
{
    if (seg->types != NULL) NAME_ERROR(Q, "unexpected type arguments on enumerator '%s'", seg->name->text);
    struct HirDecl *result = expect_field(Q, d, seg->name);
    seg->base = seg->did = result->hdr.did;
    seg->modno = Q->m->hir->modno;
    return result;
}

static struct HirDecl *locate_next_decl(struct QueryState *Q, struct HirDecl *prev, struct HirSegment *next)
{
    struct HirAdtDecl *d = HirGetAdtDecl(prev);
    struct HirDecl *result = d->is_struct
        ? locate_method(Q, d, next)
        : locate_enumerator(Q, d, next);
    return result;
}

struct HirDecl *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirPath *path)
{
    struct QueryState Q = {
        .C = C,
    };

    paw_assert(path->count > 0);
    // sets 'Q.m' to the module containing the target declaration, and set
    // 'Q.index' to the index of the following segment
    struct HirDecl *result = locate_base_decl(&Q, m, path);
    for (; Q.index < path->count; ++Q.index) {
        // resolve method or enumerator (will throw an error if there is more
        // than 1 additional segment)
        struct HirSegment *seg = pawHir_path_get(path, Q.index);
        result = locate_next_decl(&Q, result, seg);
    }
    return result;
}

