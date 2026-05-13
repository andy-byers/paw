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

typedef struct InferenceVar {
    struct SourceSpan span;
    IrType *type;
    int parent;
    int rank;
    int id;
} InferenceVar;

enum Action {
    ACTION_CREATE,
    ACTION_SET_RANK,
    ACTION_SET_PARENT,
    ACTION_SET_TYPE,
};

struct UndoEntry {
    enum Action action;
    int ivar_id;
    union {
        IrType *old_type;
        int old_parent;
        int old_rank;
    };
};

DEFINE_LIST(struct Compiler, VarList, struct InferenceVar)
DEFINE_LIST(struct Compiler, UndoLog, struct UndoEntry)

typedef struct UnificationTable {
    struct UnificationTable *outer;

    UndoLog *undo;

    // vector of type variables
    struct VarList *ivars;

    // depth of binder
    int depth;
} UnificationTable;

static void record_create(struct Unifier *U, InferenceVar ivar)
{
    UndoLog_push(U->C, U->table->undo, (struct UndoEntry){
                .action = ACTION_CREATE,
                .ivar_id = ivar.id,
            });
}

static void record_set_parent(struct Unifier *U, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->table->undo, (struct UndoEntry){
                .action = ACTION_SET_PARENT,
                .old_parent = ivar->parent,
                .ivar_id = ivar->id,
            });
}

static void record_set_rank(struct Unifier *U, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->table->undo, (struct UndoEntry){
                .action = ACTION_SET_RANK,
                .old_rank = ivar->rank,
                .ivar_id = ivar->id,
            });
}

static void record_set_type(struct Unifier *U, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->table->undo, (struct UndoEntry){
                .action = ACTION_SET_TYPE,
                .old_type = ivar->type,
                .ivar_id = ivar->id,
            });
}

static void dump_snapshot(struct Unifier *U)
{
    printf("Unification table snapshot\n");
    for (int i = 0; i < U->table->ivars->count; ++i) {
        InferenceVar const ivar = VarList_get(U->table->ivars, i);
        printf("IVAR(%d, rank=%d, parent=%d, type=%s)\n", ivar.id, ivar.rank,
                ivar.parent, pawIr_print_type(U->C, ivar.type));
    }
}

static InferenceVar *get_ivar(struct Unifier *U, int index)
{
    paw_assert(index < U->table->ivars->count);
    return &K_LIST_AT(U->table->ivars, index);
}

void pawU_undo_unifications(struct Unifier *U, int position)
{
    while (U->table->undo->count > position) {
        struct UndoEntry const entry = UndoLog_last(U->table->undo);
        UndoLog_pop(U->table->undo);

        switch (entry.action) {
            case ACTION_CREATE:
                VarList_pop(U->table->ivars);
                break;
            case ACTION_SET_PARENT:
                get_ivar(U, entry.ivar_id)
                    ->parent = entry.old_parent;
                break;
            case ACTION_SET_RANK:
                get_ivar(U, entry.ivar_id)
                    ->rank = entry.old_rank;
                break;
            case ACTION_SET_TYPE:
                get_ivar(U, entry.ivar_id)
                    ->type = entry.old_type;
                break;
        }
    }
}

static void overwrite_type(struct Unifier *U, InferenceVar *ivar, IrType *src)
{
    record_set_type(U, ivar);
    ivar->type = src;
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

static InferenceVar *find_root(struct Unifier *U, int id)
{
    InferenceVar *ivar = get_ivar(U, id);
    int up = ivar->parent;
    if (up != ivar->id) {
        record_set_parent(U, ivar);
        up = find_root(U, up)->id;
        ivar->parent = up;
    }
    return get_ivar(U, up);
}

static void link_roots(struct Unifier *U, InferenceVar *a, InferenceVar *b)
{
    if (a->rank < b->rank) {
        record_set_parent(U, a);
        a->parent = b->id;
    } else {
        record_set_parent(U, b);
        record_set_rank(U, a);
        b->parent = a->id;
        a->rank += a->rank == b->rank;
    }
}

static void check_occurs(struct Unifier *U, InferenceVar *ivar, IrType *type)
{
    if (ivar->type == type) {
        paw_assert(IrIsInfer(type));
        UNIFIER_ERROR(U, CyclicType, ivar->span);
    }
    if (IrIsAdt(type)) {
        struct IrAdt *adt = IrGetAdt(type);
        if (adt->args != NULL) {
            K_LIST_XFOREACH (adt->args, IrGenericArg const, p) {
                if (IrGenericArg_is_type(*p)) {
                    IrType *t = IrGenericArg_get_type(*p);
                    check_occurs(U, ivar, t);
                }
            }
        }
    }
}

static int unify_var_type(struct Unifier *U, InferenceVar *ivar, IrType *type)
{
    debug_log(U, "unify_var_type", ivar->type, type);

    check_occurs(U, ivar, type);
    overwrite_type(U, ivar, type);
    return 0;
}

static int unify_var_var(struct Unifier *U, InferenceVar *a, InferenceVar *b)
{
    a = find_root(U, a->id);
    b = find_root(U, b->id);

    debug_log(U, "unify_var_var", a->type, b->type);

    if (a != b) link_roots(U, a, b);
    return 0;
}

static IrType *normalize_unknown(struct Unifier *U, IrType *type)
{
    UnificationTable *table = U->table;
    paw_assert(table->depth == IrGetInfer(type)->depth);
    IrType *root = find_root(U, UID(type))->type;
    if (IrIsInfer(root)) return root;
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
    PAW_UNUSED(U); // TODO: do something here...
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
            IrType *self = pawU_normalize(U, t->type);
            IrTrait *trait = pawIr_normalize_trait(U->C, t->trait);
            return pawIr_new_projection(U->C, self, trait, t->assoc);
        }
    }
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
        case kIrArray: { // TODO: normalize "length"
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
            {
                struct IrProjection const *t = IrGetProjection(type);
                IrType *self = pawU_normalize_projections(U, t->type);
                IrTrait *trait = pawIr_normalize_trait_projections(U->C, t->trait);
                type = pawIr_new_projection(U->C, self, trait, t->assoc);
            }
            for (IrType *target; IrIsProjection(type); type = target) {
                target = pawIr_solver_get_norm_target(U->C->S, type);
                if (target == NULL) break;
            }
            if (IrIsProjection(type)) {
                struct IrProjection const *t = IrGetProjection(type);
                if (!IrIsInfer(t->type)) {
                    Str const *name = pawIr_get_assoc_item(U->C, t->assoc)->name;
                    struct Instantiation *assoc = pawIr_find_assoc_type_projection(
                            U->C, t->type, t->trait, name);
                    return assoc != NULL ? assoc->inst : type;
                }
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
        if (U->action(U, *pa, *pb))
            return -1;
    }
    return 0;
}

static int unify_adt(struct Unifier *U, struct IrAdt *a, struct IrAdt *b)
{
    if (a->did.value != b->did.value) return -1;
    if (a->args == NULL) return 0;

    IrGenericArg const *x, *y;
    K_LIST_ZIP(a->args, x, b->args, y) {
        if (pawIr_unify(U->C, *x, *y))
            return -1;
    }

    return 0;
}

static int unify(struct Unifier *U, IrType *a, IrType *b);

static int unify_array(struct Unifier *U, struct IrArray *a, struct IrArray *b)
{
    // TODO: need to undo const obligations, maybe store in IrSolver. the problem is they can be long-lived
    if (U->action == unify)
        pawIr_add_const_obligation(U->C, a->length, b->length);
    return U->action(U, a->type, b->type);
}

static int unify_slice(struct Unifier *U, struct IrSlice *a, struct IrSlice *b)
{
    return U->action(U, a->type, b->type);
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
    PAW_UNUSED(U);
    return a->did.value != b->did.value ? -1 : 0;
}

static int unify_projection(struct Unifier *U, struct IrProjection *a, struct IrProjection *b)
{
    if (U->action(U, a->type, b->type) != 0) return -1;
    if (U->trait_action(U->C, a->trait, b->trait) != 0) return -1;
    return P_ID_EQUALS(U->C, a->assoc, b->assoc) ? 0 : -1;
}

static IrType *materialize_fn(struct Unifier *U, IrType *type)
{
    if (IrIsSignature(type)) {
        struct IrSignature const *t = IrGetSignature(type);
        return pawIr_materialize_fn(U->C, t->did, t->args);
    }
    return type;
}

static int unify_types(struct Unifier *U, IrType *a, IrType *b)
{
    debug_log(U, "unify_types", a, b);
    if (IrIsNever(a) || IrIsNever(b)) {
        return 0;
    } else if (IR_IS_FUNC_TYPE(a) && IR_IS_FUNC_TYPE(b)) {
        // function pointer and definition types are compatible
        IrType *x = materialize_fn(U, a);
        IrType *y = materialize_fn(U, b);
        return unify_fptr(U, IrGetFnPtr(x), IrGetFnPtr(y));
    } else if (IR_KINDOF(a) != IR_KINDOF(b)) {
        return -1;
    } else if (IrIsArray(a)) {
        return unify_array(U, IrGetArray(a), IrGetArray(b));
    } else if (IrIsSlice(a)) {
        return unify_slice(U, IrGetSlice(a), IrGetSlice(b));
    } else if (IrIsTuple(a)) {
        return unify_tuple(U, IrGetTuple(a), IrGetTuple(b));
    } else if (IrIsAdt(a)) {
        return unify_adt(U, IrGetAdt(a), IrGetAdt(b));
    } else if (IrIsGeneric(a)) {
        return unify_generic(U, IrGetGeneric(a), IrGetGeneric(b));
    } else if (IrIsProjection(a)) {
        return unify_projection(U, IrGetProjection(a), IrGetProjection(b));
    } else if (IrIsPtr(a)) {
        return U->action(U, IrGetPtr(a)->pointee, IrGetPtr(b)->pointee);
    } else {
        return 0;
    }
}

static int unify(struct Unifier *U, IrType *a, IrType *b)
{
    a = pawU_normalize(U, a);
    b = pawU_normalize(U, b);
    if (IrIsInfer(a)) {
        InferenceVar *va = get_ivar(U, UID(a));
        if (IrIsInfer(b)) {
            InferenceVar *vb = get_ivar(U, UID(b));
            return unify_var_var(U, va, vb);
        } else {
            return unify_var_type(U, va, b);
        }
    } else if (IrIsInfer(b)) {
        InferenceVar *vb = get_ivar(U, UID(b));
        return unify_var_type(U, vb, a);
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
    Unify const old_action = U->action;
    UnifyTrait const old_trait_action = U->trait_action;

    U->trait_action = pawIr_unify_traits;
    int const result = RUN_ACTION(U, a, b, unify);

    U->action = old_action;
    U->trait_action = old_trait_action;
    return result;
}

static int equate(struct Unifier *U, IrType *a, IrType *b)
{
    a = pawU_normalize(U, a);
    b = pawU_normalize(U, b);

    if (IrIsNever(a) != IrIsNever(b)) return -1;
    if (IrIsInfer(a) || IrIsInfer(b)) return 0;

    return unify_types(U, a, b);
}

static int equals_adaptor(struct Compiler *C, IrTrait *a, IrTrait *b)
{
    return pawIr_trait_equals(C, a, b) ? 0 : -1;
}

paw_Bool pawU_equals(struct Unifier *U, IrType *a, IrType *b)
{
    Unify const old_action = U->action;
    UnifyTrait const old_trait_action = U->trait_action;

    U->trait_action = equals_adaptor;
    int const result = RUN_ACTION(U, a, b, equate) == 0;

    U->action = old_action;
    U->trait_action = old_trait_action;
    return result;
}

int pawU_current_position(struct Unifier *U)
{
    return U->table->undo->count;
}

void pawU_discard_variables(struct Unifier *U)
{
    U->table->undo->count = U->table->ivars->count = 0;
}

IrType *pawU_new_unknown(struct Unifier *U, struct SourceSpan span)
{
    UnificationTable *table = U->table;

    int const index = table->ivars->count;
    IrType *type = pawIr_new_infer(U->C, table->depth, index);
    InferenceVar const ivar = {
        .id = index,
        .parent = index,
        .type = type,
        .rank = 0,
        .span = span,
    };
    VarList_push(U->C, table->ivars, ivar);

    record_create(U, ivar);
    return type;
}

void pawU_enter_binder(struct Unifier *U, Str const *modname)
{
    UnificationTable *table = P_ALLOC(U->C, NULL, 0, sizeof(UnificationTable));
    table->ivars = VarList_new(U->C);
    table->undo = UndoLog_new(U->C);
    table->depth = U->depth;
    table->outer = U->table;
    U->modname = modname;
    U->table = table;
    ++U->depth;
}

static void check_table(struct Unifier *U)
{
    UnificationTable *table = U->table;
    for (int i = 0; i < table->ivars->count; ++i) {
        InferenceVar const *var = get_ivar(U, i);
        IrType *type = pawU_normalize(U, var->type);
        if (IrIsInfer(type))
            UNIFIER_ERROR(U, CannotInfer, var->span);
    }
}

void pawU_leave_binder(struct Unifier *U)
{
    check_table(U);
    U->table = U->table->outer;
    --U->depth;

    U->modname = NULL;
}

void pawU_run_unit_tests(struct Unifier *U)
{
}
