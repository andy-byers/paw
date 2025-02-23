// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// unify.c: type unification module

#include "compile.h"
#include "env.h"
#include "hir.h"
#include "ir_type.h"
#include "unify.h"

#define ERROR(U, line, ...) pawE_error(ENV((U)->C), PAW_ETYPE, line, __VA_ARGS__)

typedef struct InferenceVar {
    K_ALIGNAS_NODE struct InferenceVar *parent;
    struct IrType *type;
    int rank;
} InferenceVar;

DEFINE_LIST(struct Compiler, var_list_, VarList, struct InferenceVar *)

typedef struct UnificationTable {
    K_ALIGNAS_NODE struct UnificationTable *outer;
    struct VarList *ivars; // vector of type variables
    int depth; // depth of binder
} UnificationTable;

static void overwrite_type(InferenceVar *ivar, const struct IrType *src)
{
    *ivar->type = *src;
}

static InferenceVar *get_ivar(UnificationTable *table, int index)
{
    paw_assert(index < table->ivars->count);
    return table->ivars->data[index];
}

static void debug_log(struct Unifier *U, const char *what, struct IrType *a, struct IrType *b)
{
    paw_assert(a != NULL);
    paw_assert(b != NULL);

#if defined(PAW_LOG_UNIFY)
    paw_Env *P = ENV(U->C);
    pawIr_print_type(U->C, a);
    pawIr_print_type(U->C, b);
    DLOG(U->C, "(unify) %s: %s = %s",
            what, paw_string(P, -2), paw_string(P, -1));
    paw_pop(P, 2);
#else
    PAW_UNUSED(U);
    PAW_UNUSED(what);
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

static void check_occurs(struct Unifier *U, InferenceVar *ivar, struct IrType *type)
{
    if (ivar->type == type) {
        paw_assert(IrIsInfer(type));
        ERROR(U, -1, "encountered cyclic type");
    }
    if (!IrIsAdt(type)) return;
    struct IrAdt *adt = IrGetAdt(type);
    if (adt->types == NULL) return;
    for (int i = 0; i < adt->types->count; ++i) {
        struct IrType *subtype = K_LIST_GET(adt->types, i);
        check_occurs(U, ivar, subtype);
    }
}

static void unify_var_type(struct Unifier *U, InferenceVar *ivar, struct IrType *type)
{
    debug_log(U, "unify_var_type", ivar->type, type);

    struct IrTypeList *bounds = IrGetInfer(ivar->type)->bounds;
    if (!pawP_satisfies_bounds(U->C, type, bounds)) {
        TYPE_ERROR(U->C, "trait bounds not satisfied");
    }

    check_occurs(U, ivar, type);
    overwrite_type(ivar, type);
}

static void unify_var_var(struct Unifier *U, InferenceVar *a, InferenceVar *b)
{
    a = find_root(a);
    b = find_root(b);

    debug_log(U, "unify_var_var", a->type, b->type);

    struct IrTypeList *bounds = IrGetInfer(b->type)->bounds;
    if (!pawP_satisfies_bounds(U->C, a->type, bounds)) {
        TYPE_ERROR(U->C, "trait bounds not satisfied");
    }

    if (a != b) link_roots(a, b);
}

static struct IrType *normalize_unknown(UnificationTable *table, struct IrType *type)
{
    paw_assert(table->depth == IrGetInfer(type)->depth);
    const int index = IrGetInfer(type)->index;
    InferenceVar *ivar = get_ivar(table, index);
    const InferenceVar *root = find_root(ivar);
    if (!IrIsInfer(root->type)) {
        paw_assert(ivar != root);
        overwrite_type(ivar, root->type);
    }
    return root->type;
}

static void normalize_list(UnificationTable *table, struct IrTypeList *list)
{
    if (list == NULL) return;
    for (int i = 0; i < list->count; ++i) {
        list->data[i] = pawU_normalize(table, list->data[i]);
    }
}

struct IrType *pawU_normalize(UnificationTable *table, struct IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrSignature:
            normalize_list(table, IrGetSignature(type)->types);
            // fallthrough
        case kIrFuncPtr:
            normalize_list(table, IR_FPTR(type)->params);
            IR_FPTR(type)->result = pawU_normalize(table, IR_FPTR(type)->result);
            break;
        case kIrInfer:
            type = normalize_unknown(table, type);
            break;
        case kIrTuple:
            normalize_list(table, IrGetTuple(type)->elems);
            break;
        case kIrAdt:
            normalize_list(table, IrGetAdt(type)->types);
            break;
        case kIrGeneric:
        case kIrTraitObj:
            break;
    }
    return type;
}

static int unify_lists(struct Unifier *U, struct IrTypeList *a, struct IrTypeList *b)
{
    paw_assert(a && b);
    if (a->count != b->count) return -1;
    for (int i = 0; i < a->count; ++i) {
        if (U->action(U, a->data[i], b->data[i])) {
            return -1;
        }
    }
    return 0;
}

static int unify_adt(struct Unifier *U, struct IrAdt *a, struct IrAdt *b)
{
    if (a->did.value != b->did.value) return -1;
    if (!a->types != !b->types) return -1;
    if (a->types == NULL) return 0;
    return unify_lists(U, a->types, b->types);
}

static int unify_tuple(struct Unifier *U, struct IrTuple *a, struct IrTuple *b)
{
    return unify_lists(U, a->elems, b->elems);
}

static int unify_fptr(struct Unifier *U, struct IrFuncPtr *a, struct IrFuncPtr *b)
{
    if (unify_lists(U, a->params, b->params)) return -1;
    return U->action(U, a->result, b->result);
}

static int unify_generic(struct Unifier *U, struct IrGeneric *a, struct IrGeneric *b)
{
    if (a->did.value != b->did.value) return -1;
    if (!a->bounds != !b->bounds) return -1;
    if (a->bounds == NULL) return 0;
    return unify_lists(U, a->bounds, b->bounds);
}

static int unify_trait_obj(struct Unifier *U, struct IrTraitObj *a, struct IrTraitObj *b)
{
    if (a->did.value != b->did.value) return -1;
    if (!a->types != !b->types) return -1;
    if (a->types == NULL) return 0;
    return unify_lists(U, a->types, b->types);
}

static int unify_types(struct Unifier *U, struct IrType *a, struct IrType *b)
{
    debug_log(U, "unify_types", a, b);

    if (IR_IS_FUNC_TYPE(a) && IR_IS_FUNC_TYPE(b)) {
        // function pointer and definition types are compatible
        return unify_fptr(U, IR_FPTR(a), IR_FPTR(b));
    } else if (HIR_KINDOF(a) != HIR_KINDOF(b)) {
        return -1;
    } else if (IrIsTuple(a)) {
        return unify_tuple(U, IrGetTuple(a), IrGetTuple(b));
    } else if (IrIsAdt(a)) {
        return unify_adt(U, IrGetAdt(a), IrGetAdt(b));
    } else if (IrIsGeneric(a)) {
        return unify_generic(U, IrGetGeneric(a), IrGetGeneric(b));
    } else {
        paw_assert(IrIsTraitObj(a));
        return unify_trait_obj(U, IrGetTraitObj(a), IrGetTraitObj(b));
    }
}

static int unify(struct Unifier *U, struct IrType *a, struct IrType *b)
{
    UnificationTable *ut = U->table;

    // Types may have already been unified. Make sure to always use the
    // cannonical type.
    a = pawU_normalize(ut, a);
    b = pawU_normalize(ut, b);
    if (IrIsInfer(a)) {
        InferenceVar *va = get_ivar(ut, IrGetInfer(a)->index);
        if (IrIsInfer(b)) {
            InferenceVar *vb = get_ivar(ut, IrGetInfer(b)->index);
            unify_var_var(U, va, vb);
        } else {
            unify_var_type(U, va, b);
        }
    } else if (IrIsInfer(b)) {
        InferenceVar *vb = get_ivar(ut, IrGetInfer(b)->index);
        unify_var_type(U, vb, a);
    } else {
        // Both types are known: make sure they are compatible. This is the
        // only time we can encounter an error.
        return unify_types(U, a, b);
    }
    return 0;
}

#define RUN_ACTION(U, a, b, f) ((U)->action = f)(U, a, b)

void pawU_unify(struct Unifier *U, struct IrType *a, struct IrType *b)
{
    const int rc = RUN_ACTION(U, a, b, unify);
    if (rc == 0) return;

    pawIr_print_type(U->C, a);
    pawIr_print_type(U->C, b);
    ERROR(U, -1,
            "incompatible types '%s' and '%s'",
            paw_string(ENV(U->C), -2),
            paw_string(ENV(U->C), -1));
}

static int equate(struct Unifier *U, struct IrType *a, struct IrType *b)
{
    UnificationTable *ut = U->table;

    a = pawU_normalize(ut, a);
    b = pawU_normalize(ut, b);
    return unify_types(U, a, b);
}

paw_Bool pawU_equals(struct Unifier *U, struct IrType *a, struct IrType *b)
{
    return RUN_ACTION(U, a, b, equate) == 0;
}

struct IrType *pawU_new_unknown(struct Unifier *U, int line, struct IrTypeList *bounds)
{
    UnificationTable *table = U->table;

    // NOTE: inference variables require a stable address, since they point to each other
    InferenceVar *ivar = pawP_alloc(U->C, NULL, 0, sizeof(InferenceVar));
    const int index = table->ivars->count;
    K_LIST_PUSH(U->C, table->ivars, ivar);

    struct IrType *type = pawIr_new_infer(U->C, table->depth, index, bounds);
    ivar->parent = ivar;
    ivar->type = type;
    return type;
}

struct IrTypeList *pawU_new_unknowns(struct Unifier *U, struct IrTypeList *types)
{
    struct IrType **ptype;
    struct IrTypeList *result = pawIr_type_list_new(U->C);
    K_LIST_FOREACH(types, ptype) {
        struct IrTypeList *bounds = IrIsGeneric(*ptype)
            ? IrGetGeneric(*ptype)->bounds : NULL;
        struct IrType *unknown = pawU_new_unknown(U, -1, bounds);
        K_LIST_PUSH(U->C, result, unknown);
    }
    return result;
}

void pawU_enter_binder(struct Unifier *U)
{
    UnificationTable *table = pawP_alloc(U->C, NULL, 0, sizeof(UnificationTable));
    table->ivars = var_list_new(U->C);
    table->depth = U->depth;
    table->outer = U->table;
    U->table = table;
    ++U->depth;
}

static void check_table(struct Unifier *U, UnificationTable *table)
{
    for (int i = 0; i < table->ivars->count; ++i) {
        const InferenceVar *var = get_ivar(table, i);
        pawU_normalize(table, var->type);
        if (IrIsInfer(var->type)) {
            ERROR(U, -1, "unable to infer type");
        }
    }
}

void pawU_leave_binder(struct Unifier *U)
{
    check_table(U, U->table);
    U->table = U->table->outer;
    --U->depth;
}

paw_Bool pawU_list_equals(struct Unifier *U, struct IrTypeList *lhs, struct IrTypeList *rhs)
{
    paw_assert(!lhs == !rhs);
    paw_assert(lhs == NULL || lhs->count == rhs->count);

    if (lhs == NULL) return PAW_TRUE;
    for (int i = 0; i < lhs->count; ++i) {
        struct IrType *a = K_LIST_GET(lhs, i);
        struct IrType *b = K_LIST_GET(rhs, i);
        if (!pawU_equals(U, a, b)) return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool are_lists_compat(struct Unifier *U, struct IrTypeList *a, struct IrTypeList *b)
{
    paw_assert(a->count == b->count);
    for (int i = 0; i < a->count; ++i) {
        struct IrType *x = K_LIST_GET(a, i);
        struct IrType *y = K_LIST_GET(b, i);
        if (!pawU_is_compat(U, x, y)) return PAW_FALSE;
    }
    return PAW_TRUE;
}

paw_Bool pawU_is_compat(struct Unifier *U, struct IrType *a, struct IrType *b)
{
    if (pawU_equals(U, a, b)) return PAW_TRUE;
    if (IrIsGeneric(a)) {
        struct IrTypeList *bounds = IrGetGeneric(a)->bounds;
        return pawP_satisfies_bounds(U->C, b, bounds);
    } else if (IrIsInfer(b)) {
        struct IrTypeList *bounds = IrGetInfer(b)->bounds;
        return pawP_satisfies_bounds(U->C, a, bounds);
    }
    if (IR_KINDOF(a) != IR_KINDOF(b)) return PAW_FALSE;

    switch (IR_KINDOF(a)) {
        case kIrTuple:
            if (!IrIsTuple(b)) return PAW_FALSE;
            return are_lists_compat(U, IrGetTuple(a)->elems, IrGetTuple(b)->elems);
        case kIrFuncPtr:
        case kIrSignature:
            return pawU_is_compat(U, IR_FPTR(a)->result, IR_FPTR(b)->result) &&
                are_lists_compat(U, IR_FPTR(a)->params, IR_FPTR(b)->params);
        case kIrAdt:
            return IrGetAdt(a)->did.value == IrGetAdt(b)->did.value &&
                are_lists_compat(U, IrGetAdt(a)->types, IrGetAdt(b)->types);
        case kIrTraitObj:
            return IrGetTraitObj(a)->did.value == IrGetTraitObj(b)->did.value &&
                are_lists_compat(U, IrGetTraitObj(a)->types, IrGetTraitObj(b)->types);
        default:
            return a == b;
    }
    return PAW_FALSE;
}
