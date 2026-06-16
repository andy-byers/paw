// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: Clean this file up. Need to rename VisitType to AcceptType or call V->VisitType in specific visitors
//       and remove call to V->VisitType in VisitType. Do the same for F->FoldType, though need to provide a
//       pre-fold hook to call collect_type in monomorphize.c. Alternatively, could use visitor instead of
//       folder to collect types and skip cannonicalizing types. We use pawIr_type_equals instead of depending
//       on cannonicalization in later phases anyway.

#include "type_folder.h"
#include "ir_type.h"

static void VisitType(struct IrTypeVisitor *, IrType *);

static void visit_type_list(struct IrTypeVisitor *V, IrTypeList *list)
{
    if (list != NULL) {
        IrType *const *ptype;
        K_LIST_FOREACH (list, ptype)
            VisitType(V, *ptype);
    }
}

static void VisitPtr(struct IrTypeVisitor *V, struct IrPtr *t)
{
    VisitType(V, t->pointee);
}

static void VisitAdt(struct IrTypeVisitor *V, struct IrAdt *t)
{
    V->VisitGenericArgs(V, t->args);
}

static void VisitSignature(struct IrTypeVisitor *V, struct IrSignature *t)
{
    V->VisitGenericArgs(V, t->args);
}

static void VisitProjection(struct IrTypeVisitor *V, struct IrProjection *t)
{
    V->VisitGenericArgs(V, t->args);
}

static void VisitFnPtr(struct IrTypeVisitor *V, struct IrFnPtr *t)
{
    V->VisitTypeList(V, t->params);
    VisitType(V, t->result);
}

static void VisitSlice(struct IrTypeVisitor *V, struct IrSlice *t)
{
    VisitType(V, t->type);
}

static void VisitTuple(struct IrTypeVisitor *V, struct IrTuple *t)
{
    V->VisitTypeList(V, t->elems);
}

static void VisitTrait(struct IrTypeVisitor *V, struct IrTrait *t)
{
    V->VisitGenericArgs(V, t->args);
}

static void VisitNever(struct IrTypeVisitor *V, struct IrNever *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void VisitType(struct IrTypeVisitor *V, IrType *node)
{
    if (node == NULL)
        return;
    if (V->VisitType != NULL)
        V->VisitType(V, node);

    switch (IR_KINDOF(node)) {
#define DEFINE_ACCEPT(X) \
    case kIr##X: \
        if (V->Visit##X != NULL) \
            V->Visit##X(V, IrGet##X(node)); \
            break;
        IR_TYPE_LIST(DEFINE_ACCEPT)
#undef DEFINE_ACCEPT
    }
}

static void visit_generic_args(struct IrTypeVisitor *F, IrGenericArgs *args)
{
    if (args != NULL) {
        K_LIST_XFOREACH (args, IrGenericArg const, p)
            F->VisitGenericArg(F, *p);
    }
}

static void visit_generic_arg(struct IrTypeVisitor *F, IrGenericArg arg)
{
    if (IrGenericArg_is_type(arg)) {
        IrType *t = IrGenericArg_get_type(arg);
        VisitType(F, t);
    } else {
        IrConst *k = IrGenericArg_get_const(arg);
        F->VisitConst(F, k);
    }
}

static void visit_const(struct IrTypeVisitor *F, IrConst *k)
{
    switch (k->kind) {
        case IR_CONST_PENDING:
            break;
        case IR_CONST_VALUE:
            VisitType(F, k->value.type);
            break;
        case IR_CONST_INFER:
        case IR_CONST_DECL:
            break;
    }
}
void pawIr_type_visitor_init(struct IrTypeVisitor *V, struct Compiler *C, void *ud)
{
    *V = (struct IrTypeVisitor){
        .ud = ud,
        .C = C,

        .VisitConst = visit_const,
        .VisitTrait = VisitTrait,
        .VisitTypeList = visit_type_list,
        .VisitGenericArg = visit_generic_arg,
        .VisitGenericArgs = visit_generic_args,

        .VisitPtr = VisitPtr,
        .VisitAdt = VisitAdt,
        .VisitProjection = VisitProjection,
        .VisitSignature = VisitSignature,
        .VisitFnPtr = VisitFnPtr,
        .VisitSlice = VisitSlice,
        .VisitTuple = VisitTuple,
        .VisitNever = VisitNever,
    };
}

void pawIr_visit_const(struct IrTypeVisitor *V, IrConst *node)
{
    V->VisitConst(V, node);
}

void pawIr_visit_type(struct IrTypeVisitor *V, IrType *node)
{
    VisitType(V, node);
}

void pawIr_visit_trait(struct IrTypeVisitor *V, IrTrait *node)
{
    V->VisitTrait(V, node);
}

void pawIr_visit_type_list(struct IrTypeVisitor *V, IrTypeList *list)
{
    if (list != NULL)
        V->VisitTypeList(V, list);
}


static IrType *FoldType(struct IrTypeFolder *, IrType *);

static IrTypeList *fold_type_list(struct IrTypeFolder *F, IrTypeList *list)
{
    if (list == NULL) return NULL;
    IrTypeList *result = IrTypeList_new(F->C);
    IrTypeList_reserve(F->C, result, list->count);

    IrType *const *ptype;
    K_LIST_FOREACH (list, ptype) {
        IrType *type = FoldType(F, *ptype);
        IrTypeList_push(F->C, result, type);
    }
    return result;
}

static IrGenericArgs *fold_generic_args(struct IrTypeFolder *F, IrGenericArgs *args)
{
    if (args == NULL)
        return NULL;

    IrGenericArgs *result = IrGenericArgs_new(F->C);
    IrGenericArgs_reserve(F->C, result, args->count);
    K_LIST_XFOREACH (args, IrGenericArg const, p) {
        IrGenericArg const r = F->FoldGenericArg(F, *p);
        IrGenericArgs_push(F->C, result, r);
    }
    return result;
}

static IrGenericArg fold_generic_arg(struct IrTypeFolder *F, IrGenericArg arg)
{
    if (IrGenericArg_is_type(arg)) {
        IrType *t = IrGenericArg_get_type(arg);
        return IrGenericArg_from_type(FoldType(F, t));
    } else {
        IrConst *k = IrGenericArg_get_const(arg);
        return IrGenericArg_from_const(F->FoldConst(F, k));
    }
}

static IrConst *fold_const(struct IrTypeFolder *F, IrConst *k)
{
    switch (k->kind) {
        case IR_CONST_PENDING:
            return pawIr_new_const_pending(F->C, k->pending.did);
        case IR_CONST_VALUE: {
            IrType *type = FoldType(F, k->value.type);
            return pawIr_new_const_value(F->C, k->value.value, type);
        }
        case IR_CONST_INFER:
            return k;
        case IR_CONST_DECL: {
            return pawIr_new_const_decl(F->C, k->decl.did);
        }
    }
}

static IrType *FoldPtr(struct IrTypeFolder *F, struct IrPtr *t)
{
    IrType *pointee = FoldType(F, t->pointee);
    return pawIr_new_ptr(F->C, pointee);
}

static IrType *FoldAdt(struct IrTypeFolder *F, struct IrAdt *t)
{
    IrGenericArgs *types = F->FoldGenericArgs(F, t->args);
    return pawIr_new_adt(F->C, t->did, types);
}

static IrType *FoldSignature(struct IrTypeFolder *F, struct IrSignature *t)
{
    IrGenericArgs *types = F->FoldGenericArgs(F, t->args);
    return pawIr_new_signature(F->C, t->did, types);
}

static IrType *FoldProjection(struct IrTypeFolder *F, struct IrProjection *t)
{
    IrGenericArgs *args = F->FoldGenericArgs(F, t->args);
    return pawIr_new_projection(F->C, t->did, args);
}

static IrType *FoldFnPtr(struct IrTypeFolder *F, struct IrFnPtr *t)
{
    IrTypeList *params = F->FoldTypeList(F, t->params);
    IrType *result = FoldType(F, t->result);
    return pawIr_new_fn_ptr(F->C, params, result);
}

static IrType *FoldSlice(struct IrTypeFolder *F, struct IrSlice *t)
{
    IrType *type = FoldType(F, t->type);
    return pawIr_new_slice(F->C, type);
}

static IrType *FoldArray(struct IrTypeFolder *F, struct IrArray *t)
{
    IrConst *length = F->FoldConst(F, t->length);
    IrType *type = FoldType(F, t->type);
    return pawIr_new_array(F->C, type, length);
}

static IrType *FoldTuple(struct IrTypeFolder *F, struct IrTuple *t)
{
    IrTypeList *elems = F->FoldTypeList(F, t->elems);
    return pawIr_new_tuple(F->C, elems);
}

static IrTrait *FoldTrait(struct IrTypeFolder *F, struct IrTrait *t)
{
    IrGenericArgs *types = F->FoldGenericArgs(F, t->args);
    return pawIr_new_trait(F->C, t->did, types);
}

static IrType *FoldNever(struct IrTypeFolder *F, struct IrNever *t)
{
    PAW_UNUSED(t);
    return pawIr_new_never(F->C);
}

static IrType *FoldType(struct IrTypeFolder *F, IrType *node)
{
    if (node == NULL)
        return NULL;
    if (F->FoldType != NULL)
        node = F->FoldType(F, node);

    switch (IR_KINDOF(node)) {
#define DEFINE_ACCEPT(X) \
    case kIr##X: \
        if (F->Fold##X == NULL) return node; \
        return F->Fold##X(F, IrGet##X(node));
        IR_TYPE_LIST(DEFINE_ACCEPT)
#undef DEFINE_ACCEPT
    }
}

void pawIr_type_folder_init(struct IrTypeFolder *F, struct Compiler *C, void *ud)
{
    *F = (struct IrTypeFolder){
        .ud = ud,
        .C = C,

        .FoldTypeList = fold_type_list,
        .FoldGenericArgs = fold_generic_args,

        .FoldConst = fold_const,
        .FoldTrait = FoldTrait,
        .FoldGenericArg = fold_generic_arg,

        .FoldPtr = FoldPtr,
        .FoldAdt = FoldAdt,
        .FoldSignature = FoldSignature,
        .FoldProjection = FoldProjection,
        .FoldFnPtr = FoldFnPtr,
        .FoldSlice = FoldSlice,
        .FoldArray = FoldArray,
        .FoldTuple = FoldTuple,
        .FoldNever = FoldNever,
    };
}

IrType *pawIr_fold_type(struct IrTypeFolder *F, IrType *node)
{
    return FoldType(F, node);
}

IrTypeList *pawIr_fold_type_list(struct IrTypeFolder *F, IrTypeList *list)
{
    // TODO: list probably should never be NULL...
    return list != NULL ? F->FoldTypeList(F, list) : NULL;
}

IrConst *pawIr_fold_const(struct IrTypeFolder *F, IrConst *node)
{
    return F->FoldConst(F, node);
}

IrGenericArg pawIr_fold_generic_arg(struct IrTypeFolder *F, IrGenericArg arg)
{
    return F->FoldGenericArg(F, arg);
}

IrGenericArgs *pawIr_fold_generic_args(struct IrTypeFolder *F, IrGenericArgs *args)
{
    return F->FoldGenericArgs(F, args);
}

IrTrait *pawIr_fold_trait(struct IrTypeFolder *F, IrTrait *node)
{
    return F->FoldTrait(F, node);
}

static void FoldPat(struct HirVisitor *V, struct HirPat *node)
{
    paw_assert(node != NULL);
    struct IrTypeFolder *F = V->ud;
    IrType *type = GET_NODE_TYPE(F->C, node);
    if (type != NULL)
        SET_NODE_TYPE(F->C, node, pawIr_fold_type(F, type));
}

static void FoldExpr(struct HirVisitor *V, struct HirExpr *node)
{
    paw_assert(node != NULL);
    struct IrTypeFolder *F = V->ud;
    IrType *type = GET_NODE_TYPE(F->C, node);
    if (type != NULL)
        SET_NODE_TYPE(F->C, node, pawIr_fold_type(F, type));
}

static void FoldDecl(struct HirVisitor *V, struct HirDecl *node)
{
    paw_assert(node != NULL);
    struct IrTypeFolder *F = V->ud;
    IrType *type = GET_NODE_TYPE(F->C, node);
    if (type != NULL)
        SET_NODE_TYPE(F->C, node, pawIr_fold_type(F, type));
}

void pawHir_type_folder_init(struct HirTypeFolder *F, struct Hir *hir, void *ud)
{
    *F = (struct HirTypeFolder){
        .hir = hir,
        .ud = ud,
    };

    pawHir_visitor_init(&F->V, hir, F);
    F->V.PostVisitExpr = FoldExpr;
    F->V.PostVisitDecl = FoldDecl;
    F->V.PostVisitPat = FoldPat;

    pawIr_type_folder_init(&F->F, hir->C, F);
}

#define DEFINE_FOLDERS(name, T)                                                         \
    void pawHir_fold_##name##_type(struct HirTypeFolder *F, struct Hir##T *node)        \
    {                                                                                   \
        paw_assert(node != NULL);                                                       \
        pawHir_visit_##name(&F->V, node);                                               \
    }                                                                                   \
    void pawHir_fold_##name##_types(struct HirTypeFolder *F, struct Hir##T##List *list) \
    {                                                                                   \
        for (int i = 0; i < list->count; ++i) {                                         \
            pawHir_visit_##name(&F->V, Hir##T##List_get(list, i));                      \
        }                                                                               \
    }
DEFINE_FOLDERS(expr, Expr)
DEFINE_FOLDERS(decl, Decl)
DEFINE_FOLDERS(stmt, Stmt)
DEFINE_FOLDERS(pat, Pat)
#undef DEFINE_FOLDERS


static void FoldPlace(struct MirVisitor *V, struct MirPlace r)
{
    struct IrTypeFolder *F = V->ud;
    if (r.kind == MIR_PLACE_REGISTER) {
        struct MirRegisterData *data = mir_reg_data(V->mir, r.r);
        data->type = pawIr_fold_type(F, data->type);
    } else if (r.kind == MIR_PLACE_UPVALUE) {
        struct MirUpvalueInfo *data = &K_LIST_AT(V->mir->upvalues, r.up);
        data->type = pawIr_fold_type(F, data->type);
    }
}

void pawMir_type_folder_init(struct MirTypeFolder *F, struct Compiler *C, struct Mir *mir, void *ud)
{
    *F = (struct MirTypeFolder){
        .ud = ud,
        .C = C,
    };

    pawMir_visitor_init(&F->V, C, mir, F);
    F->V.PostVisitPlace = FoldPlace;

    pawIr_type_folder_init(&F->F, C, F);
}

static void mir_fold_block(struct MirTypeFolder *F, MirBlock bb)
{
    pawMir_visit_block(&F->V, bb);
}

void pawMir_fold(struct MirTypeFolder *F, struct Mir *mir)
{
    mir->self = pawIr_fold_type(&F->F, mir->self);
    mir->type = pawIr_fold_type(&F->F, mir->type);
    for (int i = 0; i < mir->blocks->count; ++i) {
        mir_fold_block(F, MIR_BB(i));
    }
    struct Mir *outer = mir;
    struct Mir *const *pchild;
    K_LIST_FOREACH (mir->children, pchild) {
        F->V.mir = *pchild;
        pawMir_fold(F, F->V.mir);
    }
    F->V.mir = outer;
}
