// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// unify.c: type unification module

#include "unify.h"
#include "compile.h"
#include "error.h"
#include "env.h"
#include "hir.h"
#include "ir_type.h"

#define UNIFIER_ERROR(U_, Kind_, ...) pawErr_##Kind_((U_)->C, (U_)->modname, __VA_ARGS__)

typedef struct InferenceVar {
    struct InferenceVar *parent;
    struct SourceLoc loc;
    IrType *type;
    int rank;
} InferenceVar;

DEFINE_LIST(struct Compiler, VarList, struct InferenceVar *)

typedef struct UnificationTable {
    K_ALIGNAS_NODE struct UnificationTable *outer;
    struct VarList *ivars; // vector of type variables
    int depth; // depth of binder
} UnificationTable;

static void overwrite_type(InferenceVar *ivar, IrType const *src)
{
    *ivar->type = *src;
}

static InferenceVar *get_ivar(UnificationTable *table, int index)
{
    paw_assert(index < table->ivars->count);
    return table->ivars->data[index];
}

static void debug_log(struct Unifier *U, char const *what, IrType *a, IrType *b)
{
    paw_assert(a != NULL);
    paw_assert(b != NULL);

#if defined(PAW_LOG_UNIFY)
    paw_Env *P = ENV(U->C);
    pawIr_print_type(U->C, a);
    pawIr_print_type(U->C, b);
    DLOG(U->C, "(unify) %s: %s = %s",
         what, paw_str(P, -2), paw_str(P, -1));
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

static void check_occurs(struct Unifier *U, InferenceVar *ivar, IrType *type)
{
    if (ivar->type == type) {
        paw_assert(IrIsInfer(type));
        UNIFIER_ERROR(U, cyclic_type, ivar->loc);
    }
    if (!IrIsAdt(type)) return;
    struct IrAdt *adt = IrGetAdt(type);
    if (adt->types == NULL) return;

    IrType *const *ptype;
    K_LIST_FOREACH (adt->types, ptype)
        check_occurs(U, ivar, *ptype);
}

static void unify_var_type(struct Unifier *U, InferenceVar *ivar, IrType *type)
{
    debug_log(U, "unify_var_type", ivar->type, type);

    IrTypeList *bounds = IrGetInfer(ivar->type)->bounds;
    if (!pawP_satisfies_bounds(U->C, type, bounds)) {
        UNIFIER_ERROR(U, unsatisfied_trait_bounds, ivar->loc);
    }

    check_occurs(U, ivar, type);
    overwrite_type(ivar, type);
}

static void unify_var_var(struct Unifier *U, InferenceVar *a, InferenceVar *b)
{
    a = find_root(a);
    b = find_root(b);

    debug_log(U, "unify_var_var", a->type, b->type);

    IrTypeList *bounds = IrGetInfer(b->type)->bounds;
    if (!pawP_satisfies_bounds(U->C, a->type, bounds))
        UNIFIER_ERROR(U, unsatisfied_trait_bounds, a->loc);

    if (a != b) link_roots(a, b);
}

static IrType *normalize_unknown(UnificationTable *table, IrType *type)
{
    paw_assert(table->depth == IrGetInfer(type)->depth);
    int const index = IrGetInfer(type)->index;
    InferenceVar *ivar = get_ivar(table, index);
    InferenceVar const *root = find_root(ivar);
    if (!IrIsInfer(root->type)) {
        paw_assert(ivar != root);
        overwrite_type(ivar, root->type);
    }
    return root->type;
}

static void normalize_list(UnificationTable *table, IrTypeList *types)
{
    if (types != NULL) {
        IrType **ptype;
        K_LIST_FOREACH (types, ptype)
            *ptype = pawU_normalize(table, *ptype);
    }
}

IrType *pawU_normalize(UnificationTable *table, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrSignature:
            normalize_list(table, IrGetSignature(type)->types);
            // fallthrough
        case kIrFnPtr:
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
        case kIrNever:
            break;
    }
    return type;
}

static int unify_lists(struct Unifier *U, IrTypeList *a, IrTypeList *b)
{
    if (a->count != b->count) return -1;
    IrType *const *pa, *const *pb;
    K_LIST_ZIP (a, pa, b, pb) {
        if (U->action(U, *pa, *pb))
            return -1;
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

static int unify_fptr(struct Unifier *U, struct IrFnPtr *a, struct IrFnPtr *b)
{
    if (unify_lists(U, a->params, b->params))
        return -1;
    return U->action(U, a->result, b->result);
}

static int unify_generic(struct Unifier *U, struct IrGeneric *a, struct IrGeneric *b)
{
    return a->did.value != b->did.value ? -1 : 0;
}

static int unify_trait_obj(struct Unifier *U, struct IrTraitObj *a, struct IrTraitObj *b)
{
    if (a->did.value != b->did.value) return -1;
    if (!a->types != !b->types) return -1;
    if (a->types == NULL) return 0;
    return unify_lists(U, a->types, b->types);
}

static int unify_types(struct Unifier *U, IrType *a, IrType *b)
{
    debug_log(U, "unify_types", a, b);
    if (IrIsNever(a) || IrIsNever(b)) {
        return 0; // "!" is the bottom type
    } else if (IR_IS_FUNC_TYPE(a) && IR_IS_FUNC_TYPE(b)) {
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

static int unify(struct Unifier *U, IrType *a, IrType *b)
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

int pawU_unify(struct Unifier *U, IrType *a, IrType *b)
{
    return RUN_ACTION(U, a, b, unify);
}

static int equate(struct Unifier *U, IrType *a, IrType *b)
{
    UnificationTable *ut = U->table;

    a = pawU_normalize(ut, a);
    b = pawU_normalize(ut, b);

    if (IrIsNever(a) && !IrIsNever(b)) return -1;
    if (!IrIsNever(a) && IrIsNever(b)) return -1;

    return unify_types(U, a, b);
}

paw_Bool pawU_equals(struct Unifier *U, IrType *a, IrType *b)
{
    return RUN_ACTION(U, a, b, equate) == 0;
}

IrType *pawU_new_unknown(struct Unifier *U, struct SourceLoc loc, IrTypeList *bounds)
{
    UnificationTable *table = U->table;

    // NOTE: inference variables require a stable address, since they point to each other
    InferenceVar *ivar = P_ALLOC(U->C, NULL, 0, sizeof(InferenceVar));
    int const index = table->ivars->count;
    VarList_push(U->C, table->ivars, ivar);

    IrType *type = pawIr_new_infer(U->C, table->depth, index, bounds);
    ivar->parent = ivar;
    ivar->type = type;
    return type;
}

IrTypeList *pawU_new_unknowns(struct Unifier *U, IrTypeList *types)
{
    IrType **ptype;
    IrTypeList *result = IrTypeList_new(U->C);
    K_LIST_FOREACH (types, ptype) {
        IrTypeList *bounds = IrIsGeneric(*ptype)
                                        ? IrGetGeneric(*ptype)->bounds
                                        : NULL;
        IrType *unknown = pawU_new_unknown(U, (struct SourceLoc){-1}, bounds);
        IrTypeList_push(U->C, result, unknown);
    }
    return result;
}

void pawU_enter_binder(struct Unifier *U, Str const *modname)
{
    UnificationTable *table = P_ALLOC(U->C, NULL, 0, sizeof(UnificationTable));
    table->ivars = VarList_new(U->C);
    table->depth = U->depth;
    table->outer = U->table;
    U->modname = modname;
    U->table = table;
    ++U->depth;
}

static void check_table(struct Unifier *U, UnificationTable *table)
{
    for (int i = 0; i < table->ivars->count; ++i) {
        InferenceVar const *var = get_ivar(table, i);
        pawU_normalize(table, var->type);
        if (IrIsInfer(var->type))
            UNIFIER_ERROR(U, cannot_infer, var->loc);
    }
}

void pawU_leave_binder(struct Unifier *U)
{
    check_table(U, U->table);
    U->table = U->table->outer;
    --U->depth;

    U->modname = NULL;
}

static paw_Bool are_lists_compat(struct Unifier *U, IrTypeList *lhs, IrTypeList *rhs)
{
    paw_assert(lhs->count == rhs->count);
    IrType *const *pa, *const *pb;
    K_LIST_ZIP (lhs, pa, rhs, pb) {
        if (!pawU_is_compat(U, *pa, *pb))
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

paw_Bool pawU_is_compat(struct Unifier *U, IrType *a, IrType *b)
{
    if (pawU_equals(U, a, b))
        return PAW_TRUE;
    if (IrIsGeneric(a)) {
        IrTypeList *bounds = IrGetGeneric(a)->bounds;
        return pawP_satisfies_bounds(U->C, b, bounds);
    } else if (IrIsInfer(b)) {
        IrTypeList *bounds = IrGetInfer(b)->bounds;
        return pawP_satisfies_bounds(U->C, a, bounds);
    }
    if (IR_KINDOF(a) != IR_KINDOF(b))
        return PAW_FALSE;

    switch (IR_KINDOF(a)) {
        case kIrTuple:
            if (!IrIsTuple(b))
                return PAW_FALSE;
            return are_lists_compat(U, IrGetTuple(a)->elems, IrGetTuple(b)->elems);
        case kIrFnPtr:
        case kIrSignature:
            return pawU_is_compat(U, IR_FPTR(a)->result, IR_FPTR(b)->result) && are_lists_compat(U, IR_FPTR(a)->params, IR_FPTR(b)->params);
        case kIrAdt:
            return IrGetAdt(a)->did.value == IrGetAdt(b)->did.value && are_lists_compat(U, IrGetAdt(a)->types, IrGetAdt(b)->types);
        case kIrTraitObj:
            return IrGetTraitObj(a)->did.value == IrGetTraitObj(b)->did.value && are_lists_compat(U, IrGetTraitObj(a)->types, IrGetTraitObj(b)->types);
        default:
            return a == b;
    }
    return PAW_FALSE;
}
