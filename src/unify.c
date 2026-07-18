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
#include "impl.h"
#include "ir_type.h"
#include "solve.h"

#include <stdio.h>

#define UNIFIER_ERROR(U_, Kind_, ...) THROW_ERROR((U_)->C, \
        Kind_, .modname = (U_)->modname, __VA_ARGS__)

#define UID(Type_) (IrGetInfer(Type_)->index)

enum InferenceVarKind {
    IVAR_TYPE,
    IVAR_INTEGER,
    IVAR_FLOAT,
    IVAR_CONST,
};

union InferenceVarData {
    IrType *type;
    IrConst *konst;
};

union InferenceVarSave {
    IrType type;
    IrConst konst;
};

typedef struct InferenceVar {
    struct SourceSpan span;
    union InferenceVarData data;
    enum InferenceVarKind kind;
    int parent;
    int rank;
    int id;
} InferenceVar;

enum Action {
    ACTION_CREATE,
    ACTION_SET_RANK,
    ACTION_SET_PARENT,
    ACTION_SET_DATA,
};

struct UndoEntry {
    enum Action action;
    int ivar_id;
    union {
        union InferenceVarSave old_data;
        int old_parent;
        int old_rank;
    };
    struct UnificationTable *table;
};

DEFINE_LIST(struct Compiler, VarList, struct InferenceVar,)
DEFINE_LIST(struct Compiler, UndoLog, struct UndoEntry,)

typedef struct UnificationTable {
    // vector of type variables
    struct VarList *ivars;

    // depth of binder
    int depth;
} UnificationTable;

struct UnificationContext {
    struct UnificationContext *outer;
    struct UnificationTable *type_vars;
    struct UnificationTable *const_vars;
    struct UndoLog *undo;
};

static void record_create(struct Unifier *U, struct UnificationTable *table, InferenceVar ivar)
{
    UndoLog_push(U->C, U->ctx->undo, (struct UndoEntry){
                .action = ACTION_CREATE,
                .ivar_id = ivar.id,
                .table = table,
            });
}

static void record_set_parent(struct Unifier *U, struct UnificationTable *table, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->ctx->undo, (struct UndoEntry){
                .action = ACTION_SET_PARENT,
                .old_parent = ivar->parent,
                .ivar_id = ivar->id,
                .table = table,
            });
}

static void record_set_rank(struct Unifier *U, struct UnificationTable *table, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->ctx->undo, (struct UndoEntry){
                .action = ACTION_SET_RANK,
                .old_rank = ivar->rank,
                .ivar_id = ivar->id,
                .table = table,
            });
}

static void record_set_type(struct Unifier *U, struct UnificationTable *table, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->ctx->undo, (struct UndoEntry){
                .action = ACTION_SET_DATA,
                .old_data.type = *ivar->data.type,
                .ivar_id = ivar->id,
                .table = table,
            });
}

static void record_set_const(struct Unifier *U, struct UnificationTable *table, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->ctx->undo, (struct UndoEntry){
                .action = ACTION_SET_DATA,
                .old_data.konst = *ivar->data.konst,
                .ivar_id = ivar->id,
                .table = table,
            });
}

static void dump_snapshot(struct Unifier *U)
{
    printf("Unification table snapshot\n");
    for (int i = 0; i < U->ctx->type_vars->ivars->count; ++i) {
        InferenceVar const ivar = VarList_get(U->ctx->type_vars->ivars, i);
        printf("TypeVar(%d, rank=%d, parent=%d, type=%s)\n", ivar.id, ivar.rank,
                ivar.parent, pawIr_print_type(U->C, ivar.data.type));
    }
    for (int i = 0; i < U->ctx->const_vars->ivars->count; ++i) {
        InferenceVar const ivar = VarList_get(U->ctx->const_vars->ivars, i);
        printf("TypeVar(%d, rank=%d, parent=%d)\n", ivar.id, ivar.rank, ivar.parent);
    }
}

static InferenceVar *get_ivar(UnificationTable const *table, int index)
{
    paw_assert(index < table->ivars->count);
    return &K_LIST_AT(table->ivars, index);
}

static InferenceVar *get_type_var(struct UnificationContext const *ctx, int index)
{
    paw_assert(index < ctx->type_vars->ivars->count);
    return &K_LIST_AT(ctx->type_vars->ivars, index);
}

static InferenceVar *get_const_var(struct UnificationContext const *ctx, int index)
{
    paw_assert(index < ctx->const_vars->ivars->count);
    return &K_LIST_AT(ctx->const_vars->ivars, index);
}

void pawU_undo_unifications(struct Unifier *U, int position)
{
    while (U->ctx->undo->count > position) {
        struct UndoEntry const entry = UndoLog_last(U->ctx->undo);
        UndoLog_pop(U->ctx->undo);

        switch (entry.action) {
            case ACTION_CREATE:
                VarList_pop(entry.table->ivars);
                break;
            case ACTION_SET_PARENT:
                get_ivar(entry.table, entry.ivar_id)
                    ->parent = entry.old_parent;
                break;
            case ACTION_SET_RANK:
                get_ivar(entry.table, entry.ivar_id)
                    ->rank = entry.old_rank;
                break;
            case ACTION_SET_DATA: {
                struct InferenceVar const *ivar = get_ivar(entry.table, entry.ivar_id);
                if (ivar->kind == IVAR_CONST) {
                    *ivar->data.konst = entry.old_data.konst;
                } else {
                    *ivar->data.type = entry.old_data.type;
                }
                break;
            }
        }
    }
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

static InferenceVar *find_root(struct Unifier *U, struct UnificationTable *table, int id)
{
    InferenceVar *ivar = get_ivar(table, id);
    int up = ivar->parent;
    if (up != ivar->id) {
        record_set_parent(U, table, ivar);
        up = find_root(U, table, up)->id;
        ivar->parent = up;
    }
    return get_ivar(table, up);
}

static void link_roots(struct Unifier *U, struct UnificationTable *table, InferenceVar *a, InferenceVar *b)
{
    if (a->rank < b->rank) {
        record_set_parent(U, table, a);
        a->parent = b->id;
    } else {
        record_set_parent(U, table, b);
        record_set_rank(U, table, a);
        b->parent = a->id;
        a->rank += a->rank == b->rank;
    }
}

static void check_type_occurs(struct Unifier *U, InferenceVar *ivar, IrType *type)
{
    if (ivar->data.type == type) {
        paw_assert(IrIsInfer(type));
        UNIFIER_ERROR(U, CyclicType, ivar->span);
    }
    if (IrIsAdt(type)) {
        struct IrAdt *adt = IrGetAdt(type);
        if (adt->args != NULL) {
            K_LIST_XFOREACH (adt->args, IrGenericArg const, p) {
                if (IrGenericArg_is_type(*p)) {
                    IrType *t = IrGenericArg_get_type(*p);
                    check_type_occurs(U, ivar, t);
                }
            }
        }
    }
}

static void check_const_occurs(struct Unifier *U, InferenceVar *ivar, IrConst *konst)
{
    if (ivar->data.konst == konst) {
        UNIFIER_ERROR(U, CyclicType, ivar->span);
    }
}

static int unify_var_type(struct Unifier *U, InferenceVar *ivar, IrType *type)
{
    debug_log(U, "unify_var_type", ivar->data.type, type);

    if (ivar->kind != IVAR_TYPE && IrIsNever(type))
        return 0;

    if ((ivar->kind == IVAR_INTEGER && !IrIsInt(type))
            || (ivar->kind == IVAR_FLOAT && !IrIsFloat(type)))
        return -1;

    check_type_occurs(U, ivar, type);
    record_set_type(U, U->ctx->type_vars, ivar);
    *ivar->data.type = *type;
    return 0;
}

static int unify_var_const(struct Unifier *U, InferenceVar *ivar, IrConst *konst)
{
    check_const_occurs(U, ivar, konst);
    record_set_const(U, U->ctx->const_vars, ivar);
    *ivar->data.konst = *konst;
    return 0;
}

static int unify_var_var(struct Unifier *U, UnificationTable *table, InferenceVar *a, InferenceVar *b)
{
    // Const unification only calls this function when both arguments are const inference
    // variables. The type unification path needs to handle, for example, an int inference
    // variable unified with a general type inference variable.
    paw_assert((a->kind != IVAR_CONST && b->kind != IVAR_CONST)
            || (a->kind == IVAR_CONST && b->kind == IVAR_CONST));

    a = find_root(U, table, a->id);
    b = find_root(U, table, b->id);

    debug_log(U, "unify_var_var", a->data.type, b->data.type);

    if (a->kind != b->kind) {
        // If neither argument is a general inference var, then there must be an
        // integer-float mismatch. Otherwise, the general inference var is known to
        // be an integer/float, depending on the kind of the other inference var.
        if (a->kind != IVAR_TYPE && b->kind != IVAR_TYPE)
            return -1;
        return a->kind == IVAR_TYPE
            ? unify_var_type(U, a, b->data.type)
            : unify_var_type(U, b, a->data.type);
    }

    if (a != b)
        link_roots(U, table, a, b);
    return 0;
}

static IrType *normalize_unknown(struct Unifier *U, IrType *type)
{
    struct IrInfer const *target = IrGetInfer(type);
    UnificationTable *table = U->ctx->type_vars;
    paw_assert(table->depth == target->depth);
    IrType *root = find_root(U, table, UID(type))->data.type;
    if (IrIsInfer(root)) {
        enum IrInferKind const rk = IrGetInfer(root)->ikind;
        if (target->ikind != IR_INFER_TYPE
                || (rk != IR_INFER_INTEGER && rk != IR_INFER_FLOAT))
            return root;
    }
    return pawU_normalize(U, root);
}

static IrGenericArgs *normalize_args(struct Unifier *U, IrGenericArgs *args)
{
    if (args == NULL) return NULL;
    IrGenericArgs *result = IrGenericArgs_new(U->C);
    IrGenericArgs_reserve(U->C, result, args->count);
    K_LIST_XFOREACH (args, IrGenericArg const, p)
        IrGenericArgs_push(U->C, result, pawIr_normalize(U->C, *p));
    return result;
}

static IrGenericArgs *normalize_args_projections(struct Unifier *U, IrGenericArgs *args)
{
    if (args == NULL) return NULL;
    IrGenericArgs *result = IrGenericArgs_new(U->C);
    IrGenericArgs_reserve(U->C, result, args->count);
    K_LIST_XFOREACH (args, IrGenericArg const, p)
        IrGenericArgs_push(U->C, result, pawIr_normalize_projections(U->C, *p));
    return result;
}

static IrTypeList *normalize_projections_list(struct Unifier *U, IrTypeList *types)
{
    if (types == NULL) return NULL;
    IrTypeList *result = IrTypeList_new(U->C);
    IrTypeList_reserve(U->C, result, types->count);
    K_LIST_XFOREACH (types, IrType *const, p)
        IrTypeList_push(U->C, result, pawU_normalize_projections(U, *p));
    return result;
}

static IrTypeList *normalize_list(struct Unifier *U, IrTypeList *types)
{
    if (types == NULL) return NULL;
    IrTypeList *result = IrTypeList_new(U->C);
    IrTypeList_reserve(U->C, result, types->count);
    K_LIST_XFOREACH (types, IrType *const, p)
        IrTypeList_push(U->C, result, pawU_normalize(U, *p));
    return result;
}

IrConst *pawU_normalize_const(struct Unifier *U, IrConst *k)
{
    if (k->kind == IR_CONST_PENDING) {
        IrConst *const *evaluated = IrResolvedConstants_get(U->C, U->C->resolved_constants, k->pending.did);
        if (evaluated != NULL) return *evaluated;
    } else if (k->kind == IR_CONST_INFER) {
        UnificationTable *table = U->ctx->const_vars;
        paw_assert(table->depth == k->infer.depth);
        IrConst *root = find_root(U, table, k->infer.index)->data.konst;
        if (root->kind == IR_CONST_INFER) return root;
        return pawU_normalize_const(U, root);
    } else if (k->kind == IR_CONST_VALUE) {
        return pawIr_new_const_value(U->C, k->value.value,
                pawU_normalize(U, k->value.type));
    }
    return k;
}

IrType *pawU_normalize(struct Unifier *U, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrUnit:
        case kIrBool:
        case kIrChar:
        case kIrInt:
        case kIrFloat:
        case kIrString:
        case kIrGeneric:
        case kIrNever:
            return type;
        case kIrInfer:
            return normalize_unknown(U, type);
        case kIrPtr: {
            IrType *pointee = pawU_normalize(U, IrGetPtr(type)->pointee);
            return pawIr_new_ptr(U->C, pointee);
        }
        case kIrClosure: {
            struct IrClosure const *t = IrGetClosure(type);
            IrGenericArgs *args = normalize_args(U, t->args);
            return pawIr_new_closure(U->C, t->did, args);
        }
        case kIrSignature: {
            struct IrSignature const *t = IrGetSignature(type);
            IrGenericArgs *args = normalize_args(U, t->args);
            return pawIr_new_signature(U->C, t->did, args);
        }
        case kIrFnPtr: {
            struct IrFnPtr const *t = IrGetFnPtr(type);
            IrTypeList *params = normalize_list(U, t->params);
            IrType *result = pawU_normalize(U, t->result);
            return pawIr_new_fn_ptr(U->C, params, result);
        }
        case kIrArray: {
            struct IrArray const *t = IrGetArray(type);
            IrType *elem = pawU_normalize(U, t->type);
            IrConst *length = pawU_normalize_const(U, t->length);
            return pawIr_new_array(U->C, elem, length);
        }
        case kIrSlice: {
            IrType *elem = pawU_normalize(U, IrGetSlice(type)->type);
            return pawIr_new_slice(U->C, elem);
        }
        case kIrTuple: {
            IrTypeList *elems = normalize_list(U, IrGetTuple(type)->elems);
            return pawIr_new_tuple(U->C, elems);
        }
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            IrGenericArgs *args = normalize_args(U, t->args);
            return pawIr_new_adt(U->C, t->did, args);
        }
        case kIrProjection: {
            struct IrProjection const *t = IrGetProjection(type);
            IrGenericArgs *args = normalize_args(U, t->args);
            return pawIr_new_projection(U->C, t->did, args);
        }
    }
}

static IrType *normalize_projections_aux(struct Unifier *U, IrType *type)
{
    for (IrType *target; IrIsProjection(type); type = target) {
        target = pawIr_solver_get_norm_target(U->C->S, type);
        if (target == NULL) break;
    }
    return type;
}

IrType *pawU_normalize_projections(struct Unifier *U, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrUnit:
        case kIrBool:
        case kIrChar:
        case kIrInt:
        case kIrFloat:
        case kIrString:
        case kIrGeneric:
        case kIrNever:
            return type;
        case kIrInfer: {
            type = normalize_unknown(U, type);
            if (IrIsInfer(type)) return type;
            return pawU_normalize_projections(U, type);
        }
        case kIrPtr: {
            IrType *pointee = pawU_normalize_projections(U, IrGetPtr(type)->pointee);
            return pawIr_new_ptr(U->C, pointee);
        }
        case kIrClosure: {
            struct IrClosure const *t = IrGetClosure(type);
            IrGenericArgs *args = normalize_args_projections(U, t->args);
            return pawIr_new_closure(U->C, t->did, args);
        }
        case kIrSignature: {
            struct IrSignature const *t = IrGetSignature(type);
            IrGenericArgs *args = normalize_args_projections(U, t->args);
            return pawIr_new_signature(U->C, t->did, args);
        }
        case kIrFnPtr: {
            struct IrFnPtr const *t = IrGetFnPtr(type);
            IrTypeList *params = normalize_projections_list(U, t->params);
            IrType *result = pawU_normalize_projections(U, t->result);
            return pawIr_new_fn_ptr(U->C, params, result);
        }
        case kIrArray: {
            struct IrArray const *t = IrGetArray(type);
            IrType *elem = pawU_normalize_projections(U, t->type);
            IrConst *length = pawU_normalize_const(U, t->length);
            return pawIr_new_array(U->C, elem, length);
        }
        case kIrSlice: {
            IrType *elem = pawU_normalize_projections(U, IrGetSlice(type)->type);
            return pawIr_new_slice(U->C, elem);
        }
        case kIrTuple: {
            IrTypeList *elems = normalize_list(U, IrGetTuple(type)->elems);
            return pawIr_new_tuple(U->C, elems);
        }
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            IrGenericArgs *args = normalize_args_projections(U, t->args);
            return pawIr_new_adt(U->C, t->did, args);
        }
        case kIrProjection: {
            type = pawU_normalize(U, type);

            {
                struct IrProjection const *t = IrGetProjection(type);
                IrGenericArgs *args = IrGenericArgs_new(U->C);
                IrGenericArgs_reserve(U->C, args, t->args->count);
                K_LIST_XFOREACH (t->args, IrGenericArg const, p) {
                    IrGenericArg const arg = pawIr_normalize_projections(U->C, *p);
                    IrGenericArgs_push(U->C, args, arg);
                }
                type = pawIr_new_projection(U->C, t->did, args);
            }

            {
                // attempt to normalize the projection using information from the surrounding
                // environment, i.e. predicates from the enclosing item
                IrSolver *child = pawIr_push_solver(U->C);
                IrType *self = ir_projection_self(IrGetProjection(type));
                if (IrIsProjection(self)) {
                    struct IrProjection const *nested = IrGetProjection(self);
                    IrTrait *nested_trait = pawIr_get_projection_trait(U->C, nested);
                    pawIr_solver_add_predicates_from_trait(child, nested_trait, (struct IrObligationCause){0});
                }
                type = normalize_projections_aux(U, type);
                pawIr_pop_solver(U->C);
            }

            if (IrIsProjection(type)) {
                struct IrProjection const *t = IrGetProjection(type);
                IrType *self = ir_projection_self(t);
                if (IrIsInfer(self)) return type;

                IrTrait *trait = pawIr_get_projection_trait(U->C, t);
                Str const *name = pawIr_get_assoc_item(U->C, t->did)->name;
                struct IrObligationCause const UNUSED = {0};
                struct Instantiation const *assoc = pawIr_find_assoc_type_projection(
                        U->C, self, trait, name, UNUSED);
                if (assoc != NULL) return assoc->inst;
            }
            return type;
        }
    }
}

static int unify_lists(struct Unifier *U, IrTypeList *a, IrTypeList *b)
{
    if (a->count != b->count) return -1;
    IrType *const *pa, *const *pb;
    K_LIST_ZIP (a, pa, b, pb) {
        if (pawU_unify(U, *pa, *pb) != 0)
            return -1;
    }
    return 0;
}

static int unify_nominal_type(struct Unifier *U, DeclId did, IrGenericArgs *args, DeclId did2, IrGenericArgs *args2)
{
    if (did.value != did2.value)
        return -1;

    if (args != NULL) {
        IrGenericArg const *x, *y;
        paw_assert(args->count == args2->count);
        K_LIST_ZIP(args, x, args2, y) {
            if (pawIr_unify(U->C, *x, *y) != 0)
                return -1;
        }
    }

    return 0;
}

static int unify_adt(struct Unifier *U, struct IrAdt *a, struct IrAdt *b)
{
    return unify_nominal_type(U, a->did, a->args, b->did, b->args);
}

static int unify(struct Unifier *U, IrType *a, IrType *b);

static int unify_array(struct Unifier *U, struct IrArray *a, struct IrArray *b)
{
    if (pawU_unify_const(U, a->length, b->length) != 0) {
        if (a->length->kind != IR_CONST_PENDING
                && b->length->kind != IR_CONST_PENDING)
            // "const equals" obligation would fail anyway
            return -1;
        pawIr_solver_add_const_equals_obligation(U->C->S, a->length, b->length,
                (struct IrObligationCause){0});
    }
    return pawU_unify(U, a->type, b->type);
}

static int unify_slice(struct Unifier *U, struct IrSlice *a, struct IrSlice *b)
{
    return pawU_unify(U, a->type, b->type);
}

static int unify_tuple(struct Unifier *U, struct IrTuple *a, struct IrTuple *b)
{
    return unify_lists(U, a->elems, b->elems);
}

static int unify_fptr(struct Unifier *U, struct IrFnPtr *a, struct IrFnPtr *b)
{
    if (unify_lists(U, a->params, b->params))
        return -1;
    return pawU_unify(U, a->result, b->result);
}

static int unify_closure(struct Unifier *U, struct IrClosure *a, struct IrClosure *b)
{
    return unify_nominal_type(U, a->did, a->args, b->did, b->args);
}

static int unify_generic(struct Unifier *U, struct IrGeneric *a, struct IrGeneric *b)
{
    PAW_UNUSED(U);
    return a->did.value != b->did.value ? -1 : 0;
}

static int unify_projection(struct Unifier *U, struct IrProjection *a, struct IrProjection *b)
{
    if (!P_ID_EQUALS(U->C, a->did, b->did))
        return -1;

    IrGenericArg const *x, *y;
    paw_assert(a->args->count == b->args->count);
    K_LIST_ZIP(a->args, x, b->args, y) {
        if (pawIr_unify(U->C, *x, *y) != 0)
            return -1;
    }
    return 0;
}

static IrType *materialize_fn(struct Unifier *U, IrType *type)
{
    if (IrIsFnPtr(type))
        return type;

    if (IrIsClosure(type)) {
        // only closures that capture no variables can be coerced to a raw
        // function pointer type
        struct IrFnDef const *def = pawIr_get_fn_def(U->C, IR_TYPE_DID(type));
        if (def->has_captures) return type;
    }

    return pawIr_materialize_fn(U->C, IR_TYPE_DID(type), IR_GENERIC_ARGS(type));
}

static int unify_types(struct Unifier *U, IrType *a, IrType *b)
{
    debug_log(U, "unify_types", a, b);

    if (IrIsNever(a)) return 0;
    if (IrIsNever(b)) return 0;

    if (IR_IS_FUNC_TYPE(a))
        a = materialize_fn(U, a);
    if (IR_IS_FUNC_TYPE(b))
        b = materialize_fn(U, b);

    if (IR_KINDOF(a) != IR_KINDOF(b))
        return -1;

    switch (IR_KINDOF(a)) {
        case kIrInt:
            return IR_INT_KIND(a) == IR_INT_KIND(b) ? 0 : -1;
        case kIrFloat:
            return IR_FLOAT_KIND(a) == IR_FLOAT_KIND(b) ? 0 : -1;
        case kIrFnPtr:
            return unify_fptr(U, IrGetFnPtr(a), IrGetFnPtr(b));
        case kIrClosure:
            return unify_closure(U, IrGetClosure(a), IrGetClosure(b));
        case kIrArray:
            return unify_array(U, IrGetArray(a), IrGetArray(b));
        case kIrSlice:
            return unify_slice(U, IrGetSlice(a), IrGetSlice(b));
        case kIrTuple:
            return unify_tuple(U, IrGetTuple(a), IrGetTuple(b));
        case kIrAdt:
            return unify_adt(U, IrGetAdt(a), IrGetAdt(b));
        case kIrGeneric:
            return unify_generic(U, IrGetGeneric(a), IrGetGeneric(b));
        case kIrProjection:
            return unify_projection(U, IrGetProjection(a), IrGetProjection(b));
        case kIrPtr:
            return pawU_unify(U, IrGetPtr(a)->pointee, IrGetPtr(b)->pointee);
        default:
            return 0;
    }
}

int pawU_unify(struct Unifier *U, IrType *a, IrType *b)
{
    a = pawU_normalize(U, a);
    b = pawU_normalize(U, b);
    if (IrIsInfer(a)) {
        InferenceVar *va = get_type_var(U->ctx, UID(a));
        if (IrIsInfer(b)) {
            InferenceVar *vb = get_type_var(U->ctx, UID(b));
            return unify_var_var(U, U->ctx->type_vars, va, vb);
        } else {
            return unify_var_type(U, va, b);
        }
    } else if (IrIsInfer(b)) {
        InferenceVar *vb = get_type_var(U->ctx, UID(b));
        return unify_var_type(U, vb, a);
    } else {
        // Both types are known: make sure they are compatible. This is the
        // only time we can encounter an error.
        return unify_types(U, a, b);
    }
    return 0;
}

static int unify_const_value(struct Unifier *U, struct IrConstValue a, struct IrConstValue b)
{
    if (pawU_unify(U, a.type, b.type) != 0)
        return -1;

    IrType *type = pawU_normalize(U, a.type);
    switch (IR_KINDOF(type)) {
#define EQ(Lhs_, Rhs_, Field_) ((Lhs_).value.Field_ == (Rhs_).value.Field_ ? 0 : -1)

        case kIrUnit:
            return 0;
        case kIrBool:
            return EQ(a, b, b);
        case kIrChar:
            return EQ(a, b, c);
        case kIrInt:
            switch (IR_INT_KIND(type)) {
                case IR_INT8:
                case IR_UINT8:
                    return EQ(a, b, u8);
                case IR_INT16:
                case IR_UINT16:
                    return EQ(a, b, u16);
                case IR_INT32:
                case IR_UINT32:
                    return EQ(a, b, u32);
                case IR_INT64:
                case IR_UINT64:
                    return EQ(a, b, u64);
                case IR_ISIZE:
                case IR_USIZE:
                    return EQ(a, b, usize);
            }
        case kIrFloat:
            switch (IR_FLOAT_KIND(type)) {
                case IR_FLOAT32:
                    return EQ(a, b, f32);
                case IR_FLOAT64:
                    return EQ(a, b, f64);
            }
        default: {
            paw_assert(IrIsInfer(type));
            struct IrInfer const *t = IrGetInfer(type);
            if (t->ikind == IR_INFER_INTEGER) {
                return EQ(a, b, u64);
            } else {
                paw_assert(t->ikind == IR_INFER_FLOAT);
                return EQ(a, b, f64);
            }
        }

#undef EQ
    }
}

static int unify_const_param(struct IrConstDecl a, struct IrConstDecl b)
{
    return P_ID_EQUALS(NULL, a.did, b.did) ? 0 : -1;
}

static int unify_const_pending(struct IrConstPending a, struct IrConstPending b)
{
    return P_ID_EQUALS(NULL, a.did, b.did) ? 0 : -1;
}

static int unify_consts(struct Unifier *U, IrConst *a, IrConst *b)
{
    a = pawU_normalize_const(U, a);
    b = pawU_normalize_const(U, b);
    if (a->kind != b->kind)
        return -1;
    if (a->kind == IR_CONST_VALUE)
        return unify_const_value(U, a->value, b->value);
    if (a->kind == IR_CONST_PENDING)
        return unify_const_pending(a->pending, b->pending);
    paw_assert(a->kind == IR_CONST_DECL);
    return unify_const_param(a->decl, b->decl);
}

int pawU_unify_const(struct Unifier *U, IrConst *a, IrConst *b)
{
    if (a->kind == IR_CONST_INFER) {
        InferenceVar *va = get_const_var(U->ctx, a->infer.index);
        if (b->kind == IR_CONST_INFER) {
            InferenceVar *vb = get_const_var(U->ctx, b->infer.index);
            return unify_var_var(U, U->ctx->const_vars, va, vb);
        } else {
            return unify_var_const(U, va, b);
        }
    } else if (b->kind == IR_CONST_INFER) {
        InferenceVar *vb = get_const_var(U->ctx, b->infer.index);
        return unify_var_const(U, vb, a);
    } else {
        return unify_consts(U, a, b);
    }
    return 0;
}

static int equate(struct Unifier *U, IrType *a, IrType *b)
{
    a = pawU_normalize(U, a);
    b = pawU_normalize(U, b);

    if (IrIsNever(a) != IrIsNever(b)) return -1;
    if (IrIsInfer(a) || IrIsInfer(b)) return 0;

    return unify_types(U, a, b);
}

int pawU_current_position(struct Unifier *U)
{
    return U->ctx->undo->count;
}

void pawU_discard_variables(struct Unifier *U)
{
    U->ctx->undo->count
        = U->ctx->type_vars->ivars->count
        = U->ctx->const_vars->ivars->count
        = 0;
}

IrType *pawU_new_unknown(struct Unifier *U, struct SourceSpan span)
{
    return pawU_new_type_var(U, IR_INFER_TYPE, span);
}

IrType *pawU_new_type_var(struct Unifier *U, enum IrInferKind ikind, struct SourceSpan span)
{
    UnificationTable *table = U->ctx->type_vars;

    int const index = table->ivars->count;
    IrType *type = pawIr_new_infer(U->C, ikind, table->depth, index);
    InferenceVar const ivar = {
        .kind = ikind == IR_INFER_TYPE ? IVAR_TYPE :
            ikind == IR_INFER_INTEGER ? IVAR_INTEGER :
            IVAR_FLOAT,
        .id = index,
        .parent = index,
        .data.type = type,
        .rank = 0,
        .span = span,
    };
    VarList_push(U->C, table->ivars, ivar);

    record_create(U, table, ivar);
    return type;
}

IrConst *pawU_new_const_var(struct Unifier *U, struct SourceSpan span)
{
    UnificationTable *table = U->ctx->const_vars;

    int const index = table->ivars->count;
    IrConst *konst = pawIr_new_const_infer(U->C, table->depth, index);
    InferenceVar const ivar = {
        .kind = IVAR_CONST,
        .id = index,
        .parent = index,
        .data.konst = konst,
        .rank = 0,
        .span = span,
    };
    VarList_push(U->C, table->ivars, ivar);

    record_create(U, table, ivar);
    return konst;
}

static UnificationTable *new_unification_table(struct Unifier *U)
{
    UnificationTable *table = P_ALLOC(U->C, NULL, 0, sizeof(*table));
    table->ivars = VarList_new(U->C);
    table->depth = U->depth;
    return table;
}

void pawU_enter_binder(struct Unifier *U, Str const *modname)
{
    struct UnificationContext *ctx = P_ALLOC(U->C, NULL, 0, sizeof(*ctx));
    ctx->type_vars = new_unification_table(U);
    ctx->const_vars = new_unification_table(U);
    ctx->undo = UndoLog_new(U->C);
    ctx->outer = U->ctx;
    U->modname = modname;
    U->ctx = ctx;
    ++U->depth;
}

void pawU_check_context(struct Unifier *U)
{
    IrType *default_int = pawIr_new_int(U->C, IR_INT64);
    IrType *default_float = pawIr_new_float(U->C, IR_FLOAT64);
    K_LIST_XFOREACH (U->ctx->type_vars->ivars, InferenceVar const, var) {
        IrType *type = pawU_normalize(U, var->data.type);
        if (IrIsInfer(type)) {
            struct IrInfer const *t = IrGetInfer(type);
            if (t->ikind == IR_INFER_INTEGER) {
                *type = *default_int;
            } else if (t->ikind == IR_INFER_FLOAT) {
                *type = *default_float;
            }
        }
    }
    K_LIST_XFOREACH (U->ctx->type_vars->ivars, InferenceVar const, var) {
        IrType *type = pawU_normalize(U, var->data.type);
        if (IrIsInfer(type)) {
            paw_assert(var->kind == IR_INFER_TYPE);
            UNIFIER_ERROR(U, CannotInfer, var->span);
        }
    }
    K_LIST_XFOREACH (U->ctx->const_vars->ivars, InferenceVar const, var) {
        IrConst *konst = pawU_normalize_const(U, var->data.konst);
        if (konst->kind == IR_CONST_INFER)
            UNIFIER_ERROR(U, CannotInferConst, var->span);
    }
}

void pawU_leave_binder(struct Unifier *U)
{
    pawU_check_context(U);
    U->ctx = U->ctx->outer;
    --U->depth;

    U->modname = NULL;
}

void pawU_run_unit_tests(struct Unifier *U)
{
}
