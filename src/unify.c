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
#include "solve.h"

#include <stdio.h>

#define UNIFIER_ERROR(U_, Kind_, ...) pawErr_##Kind_((U_)->C, (U_)->modname, __VA_ARGS__)

#define UID(Type_) (IrGetInfer(Type_)->index)

typedef struct InferenceVar {
    struct SourceLoc loc;
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
    InferenceVar *ivar;
    union {
        IrType *old_type;
        int old_parent;
        int old_rank;
    };
};

DEFINE_LIST(struct Compiler, VarList, struct InferenceVar *)
DEFINE_LIST(struct Compiler, UndoLog, struct UndoEntry)

typedef struct UnificationTable {
    struct UnificationTable *outer;

    UndoLog *undo;

    // vector of type variables
    struct VarList *ivars;

    // depth of binder
    int depth;
} UnificationTable;

static void record_create(struct Unifier *U, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->table->undo, (struct UndoEntry){
                .action = ACTION_CREATE,
                .ivar = ivar,
            });
}

static void record_set_parent(struct Unifier *U, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->table->undo, (struct UndoEntry){
                .action = ACTION_SET_PARENT,
                .old_parent = ivar->parent,
                .ivar = ivar,
            });
}

static void record_set_rank(struct Unifier *U, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->table->undo, (struct UndoEntry){
                .action = ACTION_SET_RANK,
                .old_rank = ivar->rank,
                .ivar = ivar,
            });
}

static void record_set_type(struct Unifier *U, InferenceVar *ivar)
{
    UndoLog_push(U->C, U->table->undo, (struct UndoEntry){
                .action = ACTION_SET_TYPE,
                .old_type = ivar->type,
                .ivar = ivar,
            });
}

static void dump_snapshot(struct Unifier *U)
{
    printf("Unification table snapshot\n");
    for (int i = 0; i < U->table->ivars->count; ++i) {
        InferenceVar const *ivar = VarList_get(U->table->ivars, i);
        printf("IVAR(%d, rank=%d, parent=%d, type=%s)\n", ivar->id, ivar->rank,
                ivar->parent, pawIr_print_type(U->C, ivar->type));
    }
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
                entry.ivar->parent = entry.old_parent;
                break;
            case ACTION_SET_RANK:
                entry.ivar->rank = entry.old_rank;
                break;
            case ACTION_SET_TYPE:
                entry.ivar->type = entry.old_type;
                break;
        }
    }
}

static void overwrite_type(struct Unifier *U, InferenceVar *ivar, IrType *src)
{
    record_set_type(U, ivar);
    ivar->type = src;
}

static InferenceVar *get_ivar(struct Unifier *U, int index)
{
    paw_assert(index < U->table->ivars->count);
    return U->table->ivars->data[index];
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
        UNIFIER_ERROR(U, cyclic_type, ivar->loc);
    }
    if (IrIsAdt(type)) {
        struct IrAdt *adt = IrGetAdt(type);
        if (adt->types != NULL) {
            K_LIST_XFOREACH (adt->types, IrType *const, p)
                check_occurs(U, ivar, *p);
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

static IrTypeList *normalize_list(struct Unifier *U, IrTypeList *types)
{
    if (types == NULL) return NULL;
    IrTypeList *result = IrTypeList_new(U->C);
    IrTypeList_reserve(U->C, result, types->count);
    K_LIST_XFOREACH (types, IrType *const, p)
        IrTypeList_push(U->C, result, pawU_normalize(U, *p));
    return result;
}

IrType *pawU_normalize(struct Unifier *U, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrUnit:
        case kIrBool:
        case kIrChar:
        case kIrInt:
        case kIrFloat:
        case kIrStr:
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
            IrTypeList *types = normalize_list(U, t->types);
            return pawIr_new_signature(U->C, t->did, types);
        }
        case kIrFnPtr: {
            struct IrFnPtr const *t = IrGetFnPtr(type);
            IrTypeList *params = normalize_list(U, t->params);
            IrType *result = pawU_normalize(U, t->result);
            return pawIr_new_fn_ptr(U->C, params, result);
        }
        case kIrTuple: {
            IrTypeList *elems = normalize_list(U, IrGetTuple(type)->elems);
            return pawIr_new_tuple(U->C, elems);
        }
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            IrTypeList *types = normalize_list(U, t->types);
            return pawIr_new_adt(U->C, t->did, types);
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
    PAW_UNUSED(U);
    return a->did.value != b->did.value ? -1 : 0;
}

static IrType *materialize_fn(struct Unifier *U, IrType *type)
{
    if (IrIsSignature(type)) {
        struct IrSignature const *t = IrGetSignature(type);
        return pawIr_materialize_fn(U->C, t->did, t->types);
    }
    return type;
}

static int unify_types(struct Unifier *U, IrType *a, IrType *b)
{
    debug_log(U, "unify_types", a, b);
    if (IrIsNever(a) || IrIsNever(b)) {
        return 0; // "!" is the bottom type
    } else if (IR_IS_FUNC_TYPE(a) && IR_IS_FUNC_TYPE(b)) {
        // function pointer and definition types are compatible
        IrType *x = materialize_fn(U, a);
        IrType *y = materialize_fn(U, b);
        return unify_fptr(U, IrGetFnPtr(x), IrGetFnPtr(y));
    } else if (IR_KINDOF(a) != IR_KINDOF(b)) {
        return -1;
    } else if (IrIsTuple(a)) {
        return unify_tuple(U, IrGetTuple(a), IrGetTuple(b));
    } else if (IrIsAdt(a)) {
        return unify_adt(U, IrGetAdt(a), IrGetAdt(b));
    } else if (IrIsGeneric(a)) {
        return unify_generic(U, IrGetGeneric(a), IrGetGeneric(b));
    } else if (IrIsPtr(a)) {
        return unify_types(U, IrGetPtr(a)->pointee, IrGetPtr(b)->pointee);
    } else {
        return 0;
    }
}

static int unify(struct Unifier *U, IrType *a, IrType *b)
{
    // Types may have already been unified. Make sure to always use the
    // cannonical type.
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
    return RUN_ACTION(U, a, b, unify);
}

static int equate(struct Unifier *U, IrType *a, IrType *b)
{
    a = pawU_normalize(U, a);
    b = pawU_normalize(U, b);

    if (IrIsNever(a) && !IrIsNever(b)) return -1;
    if (!IrIsNever(a) && IrIsNever(b)) return -1;
    if (IrIsInfer(a) || IrIsInfer(b)) return 0;

    return unify_types(U, a, b);
}

paw_Bool pawU_equals(struct Unifier *U, IrType *a, IrType *b)
{
    return RUN_ACTION(U, a, b, equate) == 0;
}

int pawU_current_position(struct Unifier *U)
{
    return U->table->undo->count;
}

void pawU_discard_variables(struct Unifier *U)
{
    U->table->undo->count = U->table->ivars->count = 0;
}

IrType *pawU_new_unknown(struct Unifier *U, struct SourceLoc loc, IrTypeList *bounds)
{
    UnificationTable *table = U->table;

    // NOTE: inference variables require a stable address, since they point to each other
    int const index = table->ivars->count;
    IrType *type = pawIr_new_infer(U->C, table->depth, index);
    InferenceVar *ivar = P_ALLOC(U->C, NULL, 0, sizeof(InferenceVar));
    *ivar = (InferenceVar){
        .id = index,
        .parent = index,
        .type = type,
        .rank = 0,
        .loc = loc,
    };
    VarList_push(U->C, table->ivars, ivar);

    record_create(U, ivar);
    return type;
}

IrTypeList *pawU_new_unknowns(struct Unifier *U, struct SourceLoc loc, IrTypeList *types)
{
    IrType **ptype;
    IrTypeList *result = IrTypeList_new(U->C);
    K_LIST_FOREACH (types, ptype) {
        IrType *unknown = pawU_new_unknown(U, loc, NULL);
        IrTypeList_push(U->C, result, unknown);
    }
    return result;
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
            UNIFIER_ERROR(U, cannot_infer, var->loc);
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
#define ASSERT(Expr_) ((Expr_) ? (void)0 : __builtin_trap())
#define IS_BUILTIN(Type_, Kind_) (pawP_type2code(U->C, Type_) == Kind_)
#define IS_INFER(Type_, Uid_) (IrIsInfer(Type_) && UID(Type_) == (Uid_))
#define GET_BUILTIN(Kind_) pawP_builtin_type(U->C, Kind_)

    struct SourceLoc const loc = {0};
    pawU_enter_binder(U, SCAN_STR(U->C, "test"));

    {
        IrType *u = pawU_new_unknown(U, loc, NULL);
        u = pawU_normalize(U, u);
        ASSERT(IrIsInfer(u));
        pawU_undo_unifications(U, 0);
    }

    {
        // let a: _1;
        // let b: _2;
        // let t: int;
        IrType *a = pawU_new_unknown(U, loc, NULL);
        IrType *b = pawU_new_unknown(U, loc, NULL);
        IrType *t = pawP_builtin_type(U->C, BUILTIN_INT);

        // b = t;
        ASSERT(pawU_unify(U, b, t) == 0);

        int const position = pawU_current_position(U);

        // a = b;
        ASSERT(pawU_unify(U, a, b) == 0);

        ASSERT(IrIsInfer(a));
        ASSERT(IrIsInfer(b));

        ASSERT(IS_BUILTIN(pawU_normalize(U, a), BUILTIN_INT));
        ASSERT(IS_BUILTIN(pawU_normalize(U, b), BUILTIN_INT));

        pawU_undo_unifications(U, position);

        ASSERT(IrIsInfer(pawU_normalize(U, a)));
        ASSERT(IS_BUILTIN(pawU_normalize(U, b), BUILTIN_INT));

        pawU_undo_unifications(U, 0);
    }

    {
        // let a: _1;
        // let b: _2;
        // let c: _3;
        // let d: _4;
        // let e: _5;
        // let t: int;
        IrType *a = pawU_new_unknown(U, loc, NULL);
        IrType *b = pawU_new_unknown(U, loc, NULL);
        IrType *c = pawU_new_unknown(U, loc, NULL);
        IrType *d = pawU_new_unknown(U, loc, NULL);
        IrType *e = pawU_new_unknown(U, loc, NULL);
        IrType *t = pawP_builtin_type(U->C, BUILTIN_INT);

        // a = b; b = c; d = e;
        ASSERT(pawU_unify(U, a, b) == 0);
        ASSERT(pawU_unify(U, b, c) == 0);
        ASSERT(pawU_unify(U, d, e) == 0);

        int const position1 = pawU_current_position(U);

        ASSERT(IS_INFER(pawU_normalize(U, a), 0));
        ASSERT(IS_INFER(pawU_normalize(U, b), 0));
        ASSERT(IS_INFER(pawU_normalize(U, c), 0));
        ASSERT(IS_INFER(pawU_normalize(U, d), 3));
        ASSERT(IS_INFER(pawU_normalize(U, e), 3));

        // d = t;
        ASSERT(pawU_unify(U, d, t) == 0);

        int const position2 = pawU_current_position(U);

        ASSERT(IS_INFER(pawU_normalize(U, a), 0));
        ASSERT(IS_INFER(pawU_normalize(U, b), 0));
        ASSERT(IS_INFER(pawU_normalize(U, c), 0));
        ASSERT(IS_BUILTIN(pawU_normalize(U, d), BUILTIN_INT));
        ASSERT(IS_BUILTIN(pawU_normalize(U, e), BUILTIN_INT));

        // d = b;
        ASSERT(pawU_unify(U, d, b) == 0);

        ASSERT(IS_BUILTIN(pawU_normalize(U, a), BUILTIN_INT));
        ASSERT(IS_BUILTIN(pawU_normalize(U, b), BUILTIN_INT));
        ASSERT(IS_BUILTIN(pawU_normalize(U, c), BUILTIN_INT));
        ASSERT(IS_BUILTIN(pawU_normalize(U, d), BUILTIN_INT));
        ASSERT(IS_BUILTIN(pawU_normalize(U, e), BUILTIN_INT));

        pawU_undo_unifications(U, position2);

        ASSERT(IS_INFER(pawU_normalize(U, a), 0));
        ASSERT(IS_INFER(pawU_normalize(U, b), 0));
        ASSERT(IS_INFER(pawU_normalize(U, c), 0));
        ASSERT(IS_BUILTIN(pawU_normalize(U, d), BUILTIN_INT));
        ASSERT(IS_BUILTIN(pawU_normalize(U, e), BUILTIN_INT));

        pawU_undo_unifications(U, position1);

        ASSERT(IS_INFER(pawU_normalize(U, a), 0));
        ASSERT(IS_INFER(pawU_normalize(U, b), 0));
        ASSERT(IS_INFER(pawU_normalize(U, c), 0));
        ASSERT(IS_INFER(pawU_normalize(U, d), 3));
        ASSERT(IS_INFER(pawU_normalize(U, e), 3));

        pawU_undo_unifications(U, 0);
    }

    {
        // a: _1
        IrType *a = pawU_new_unknown(U, loc, NULL);

        // inst.inst: [_2]
        // t: _2
        struct Instantiation const inst = pawP_instantiate_v2(U->C, loc,
                pawP_builtin_type(U->C, BUILTIN_LIST));
        IrType *t = IrTypeList_first(inst.subst.types);

        // _1 := [_2]
        ASSERT(pawU_unify(U, a, inst.inst) == 0);

        // _2 := int
        ASSERT(pawU_unify(U, t, GET_BUILTIN(BUILTIN_INT)) == 0);

        IrType *list = pawU_normalize(U, a);
        ASSERT(IS_BUILTIN(list, BUILTIN_LIST));
        ASSERT(IS_BUILTIN(ir_adt_subtype(list, 0), BUILTIN_INT));

        pawU_undo_unifications(U, 0);
    }

    pawU_leave_binder(U);

#undef GET_BUILTIN
#undef IS_INFER
#undef IS_BUILTIN
#undef ASSERT
}
