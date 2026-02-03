// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

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
    V->VisitTypeList(V, t->types);
}

static void VisitSignature(struct IrTypeVisitor *V, struct IrSignature *t)
{
    V->VisitTypeList(V, t->types);
}

static void VisitFnPtr(struct IrTypeVisitor *V, struct IrFnPtr *t)
{
    V->VisitTypeList(V, t->params);
    VisitType(V, t->result);
}

static void VisitTuple(struct IrTypeVisitor *V, struct IrTuple *t)
{
    V->VisitTypeList(V, t->elems);
}

static void VisitTrait(struct IrTypeVisitor *V, struct IrTrait *t)
{
    V->VisitTypeList(V, t->types);
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

void pawIr_type_visitor_init(struct IrTypeVisitor *V, struct Compiler *C, void *ud)
{
    *V = (struct IrTypeVisitor){
        .ud = ud,
        .C = C,

        .VisitTrait = VisitTrait,
        .VisitTypeList = visit_type_list,

        .VisitPtr = VisitPtr,
        .VisitAdt = VisitAdt,
        .VisitSignature = VisitSignature,
        .VisitFnPtr = VisitFnPtr,
        .VisitTuple = VisitTuple,
        .VisitNever = VisitNever,
    };
}

void pawIr_visit_type(struct IrTypeVisitor *V, IrType *node)
{
    VisitType(V, node);
}

void pawIr_visit_trait(struct IrTypeVisitor *V, IrTrait *node)
{
    VisitTrait(V, node);
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

static IrType *FoldPtr(struct IrTypeFolder *F, struct IrPtr *t)
{
    IrType *pointee = FoldType(F, t->pointee);
    return pawIr_new_ptr(F->C, pointee);
}

static IrType *FoldAdt(struct IrTypeFolder *F, struct IrAdt *t)
{
    IrTypeList *types = F->FoldTypeList(F, t->types);
    return pawIr_new_adt(F->C, t->did, types);
}

static IrType *FoldSignature(struct IrTypeFolder *F, struct IrSignature *t)
{
    IrTypeList *types = F->FoldTypeList(F, t->types);
    return pawIr_new_signature(F->C, t->did, types);
}

static IrType *FoldFnPtr(struct IrTypeFolder *F, struct IrFnPtr *t)
{
    IrTypeList *params = F->FoldTypeList(F, t->params);
    IrType *result = FoldType(F, t->result);
    return pawIr_new_fn_ptr(F->C, params, result);
}

static IrType *FoldTuple(struct IrTypeFolder *F, struct IrTuple *t)
{
    IrTypeList *elems = F->FoldTypeList(F, t->elems);
    return pawIr_new_tuple(F->C, elems);
}

static IrTrait *FoldTrait(struct IrTypeFolder *F, struct IrTrait *t)
{
    IrTypeList *types = F->FoldTypeList(F, t->types);
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

        .FoldTrait = FoldTrait,
        .FoldTypeList = fold_type_list,

        .FoldPtr = FoldPtr,
        .FoldAdt = FoldAdt,
        .FoldSignature = FoldSignature,
        .FoldFnPtr = FoldFnPtr,
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
    return list != NULL ? F->FoldTypeList(F, list) : NULL;
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
    SET_NODE_TYPE(F->C, node, pawIr_fold_type(F, type));
}

static void FoldExpr(struct HirVisitor *V, struct HirExpr *node)
{
    paw_assert(node != NULL);
    struct IrTypeFolder *F = V->ud;
    IrType *type = GET_NODE_TYPE(F->C, node);
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
    if (r.kind == MIR_PLACE_LOCAL) {
        struct MirLocalData *data = mir_local_data(V->mir, r.L);
        data->type = pawIr_fold_type(F, data->type);
    } else if (r.kind == MIR_PLACE_REGISTER) {
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
