// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// unify.c: type unification module

#include "compile.h"
#include "env.h"
#include "hir.h"
#include "unify.h"

#define ERROR(U, ...) pawE_error(ENV(U), PAW_ETYPE, (U)->R->line, __VA_ARGS__)

typedef struct InferenceVar {
    K_ALIGNAS_NODE struct InferenceVar *parent;
    struct HirType *type;
    int rank;
} InferenceVar;

DEFINE_LIST(struct Hir, var_list_, VarList, struct InferenceVar)

typedef struct UnificationTable {
    K_ALIGNAS_NODE struct UnificationTable *outer;
    struct VarList *ivars; // vector of type variables
    int depth; // depth of binder
} UnificationTable;

static void overwrite_type(InferenceVar *ivar, const struct HirType *src)
{
    *ivar->type = *src;
}

static InferenceVar *get_ivar(UnificationTable *table, int index)
{
    paw_assert(index < table->ivars->count);
    return table->ivars->data[index];
}

static void debug_log(const char *what, struct HirType *a, struct HirType *b)
{
#ifdef PAW_DEBUG_UNIFY
    paw_assert(a && b);
    printf("%s: ", what);
    pawAst_repr_type(stdout, a);
    fprintf(stdout, " = ");
    pawAst_repr_type(stdout, b);
    fprintf(stdout, "\n");
#else
    paw_unused(what);
    paw_unused(a);
    paw_unused(b);
#endif
}

static InferenceVar *find_root(InferenceVar *ivar)
{
    InferenceVar *up = ivar->parent;
    if (up != ivar) {
        up = find_root(up);
        ivar->parent = up;
    }
    return up;
}

static void link_roots(InferenceVar *a, InferenceVar *b)
{
    if (a->rank < b->rank) {
        a->parent = b;
    } else {
        b->parent = a;
        a->rank += a->rank == b->rank;
    }
}

static void unify_var_type(InferenceVar *ivar, struct HirType *type)
{
    debug_log("unify_var_type", ivar->type, type);

    overwrite_type(ivar, type);
}

static void unify_var_var(InferenceVar *a, InferenceVar *b)
{
    a = find_root(a);
    b = find_root(b);

    debug_log("unify_var_var", a->type, b->type);

    if (a != b) {
        link_roots(a, b);
    }
}

static struct HirType *normalize_unknown(UnificationTable *table, struct HirType *type)
{
    paw_assert(table->depth == type->unknown.depth);
    const int index = type->unknown.index;
    InferenceVar *ivar = get_ivar(table, index);
    const InferenceVar *root = find_root(ivar);
    if (!HirIsUnknown(root->type)) {
        paw_assert(ivar != root);
        overwrite_type(ivar, root->type);
    }
    return root->type;
}

static void normalize_list(UnificationTable *table, struct HirTypeList *list)
{
    if (list == NULL) {
        return;
    }
    for (int i = 0; i < list->count; ++i) {
        list->data[i] = pawU_normalize(table, list->data[i]);
    }
}

struct HirType *pawU_normalize(UnificationTable *table, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirFuncDef:
            normalize_list(table, type->fdef.types);
            // fallthrough
        case kHirFuncPtr:
            normalize_list(table, type->fdef.params);
            type->fdef.result = pawU_normalize(table, type->fdef.result);
            break;
        case kHirUnknown:
            type = normalize_unknown(table, type);
            break;
        case kHirTupleType:
            normalize_list(table, type->tuple.elems);
            break;
        case kHirAdt:
            normalize_list(table, type->adt.types);
            break;
        default:
            break;
    }
    return type;
}

static void unify_lists(Unifier *U, struct HirTypeList *a, struct HirTypeList *b)
{
    if (a->count != b->count) {
        ERROR(U, "arity mismatch");
    }
    for (int i = 0; i < a->count; ++i) {
        pawU_unify(U, a->data[i], b->data[i]);
    }
}

static void unify_adt(Unifier *U, struct HirAdt *a, struct HirAdt *b)
{
    if (a->base != b->base) {
        ERROR(U, "data types are incompatible");
    }
    paw_assert(!a->types == !b->types);
    if (a->types != NULL) {
        unify_lists(U, a->types, b->types);
    }
}

static void unify_tuple(Unifier *U, struct HirTupleType *a, struct HirTupleType *b)
{
    unify_lists(U, a->elems, b->elems);
}

static void unify_func(Unifier *U, struct HirFuncPtr *a, struct HirFuncPtr *b)
{
    unify_lists(U, a->params, b->params);
    pawU_unify(U, a->result, b->result);
}

static struct HirType *unify_generic(Unifier *U, struct HirType *a, struct HirType *b)
{
    if (a->generic.did != b->generic.did) {
        ERROR(U, "generic types are incompatible");
    }
    return a;
}

static void unify_types(Unifier *U, struct HirType *a, struct HirType *b)
{
    debug_log("unify_types", a, b);
    if (HirIsFuncType(a) && HirIsFuncType(b)) {
        // function pointer and definition types are compatible
        unify_func(U, &a->fptr, &b->fptr);
    } else if (HIR_KINDOF(a) != HIR_KINDOF(b)) {
        ERROR(U, "incompatible types");
    } else if (HirIsTupleType(a)) {
        unify_tuple(U, &a->tuple, &b->tuple);
    } else if (HirIsAdt(a)) {
        unify_adt(U, &a->adt, &b->adt);
    } else {
        paw_assert(HirIsGeneric(a));
        unify_generic(U, a, b);
    }
}

// TODO: Indicate failure rather than throw errors inside, let the caller throw,
// for better error messages
void pawU_unify(Unifier *U, struct HirType *a, struct HirType *b)
{
    UnificationTable *ut = U->table;

    // Types may have already been unified. Make sure to always use the
    // cannonical type.
    a = pawU_normalize(ut, a);
    b = pawU_normalize(ut, b);
    if (HirIsUnknown(a)) {
        InferenceVar *va = get_ivar(ut, a->unknown.index);
        if (HirIsUnknown(b)) {
            InferenceVar *vb = get_ivar(ut, b->unknown.index);
            unify_var_var(va, vb);
        } else {
            unify_var_type(va, b);
        }
    } else if (HirIsUnknown(b)) {
        InferenceVar *vb = get_ivar(ut, b->unknown.index);
        unify_var_type(vb, a);
    } else {
        // Both types are known: make sure they are compatible. This is the
        // only time pawU_unify can encounter an error.
        unify_types(U, a, b);
    }
}

struct HirType *pawU_new_unknown(Unifier *U)
{
    paw_Env *P = ENV(U);
    struct Hir *hir = U->hir;
    UnificationTable *table = U->table;

    // NOTE: inference variables require a stable address, since they point to each other
    InferenceVar *ivar = pawK_pool_alloc(P, &hir->pool, sizeof(InferenceVar));
    const int index = table->ivars->count;
    var_list_push(U->hir, table->ivars, ivar);

    struct HirType *type = pawHir_new_type(hir, -1, kHirUnknown);
    type->unknown.depth = table->depth;
    type->unknown.index = index;

    ivar->parent = ivar;
    ivar->type = type;
    return type;
}

void pawU_enter_binder(Unifier *U)
{
    paw_Env *P = ENV(U);
    struct Hir *hir = U->hir;
    UnificationTable *table = pawK_pool_alloc(P, &hir->pool, sizeof(UnificationTable));
    table->ivars = var_list_new(U->hir);
    table->depth = U->depth;
    table->outer = U->table;
    U->table = table;
    ++U->depth;
}

static void check_table(Unifier *U, UnificationTable *table)
{
    for (int i = 0; i < table->ivars->count; ++i) {
        const InferenceVar *var = get_ivar(table, i);
        pawU_normalize(table, var->type);
        if (HirIsUnknown(var->type)) {
            ERROR(U, "unable to infer type");
        }
    }
}

void pawU_leave_binder(Unifier *U)
{
    check_table(U, U->table);
    U->table = U->table->outer;
    --U->depth;
}
