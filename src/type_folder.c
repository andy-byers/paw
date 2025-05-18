// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "type_folder.h"
#include "ir_type.h"

static struct IrType *FoldType(struct IrTypeFolder *, struct IrType *);

static struct IrTypeList *fold_type_list(struct IrTypeFolder *F, struct IrTypeList *list)
{
    if (list == NULL)
        return NULL;
    struct IrTypeList *result = IrTypeList_new(F->C);
    IrTypeList_reserve(F->C, result, list->count);

    struct IrType *const *ptype;
    K_LIST_FOREACH (list, ptype) {
        struct IrType *type = FoldType(F, *ptype);
        IrTypeList_push(F->C, result, type);
    }
    return result;
}

static struct IrType *FoldAdt(struct IrTypeFolder *F, struct IrAdt *t)
{
    struct IrTypeList *types = F->FoldTypeList(F, t->types);
    return pawIr_new_adt(F->C, t->did, types);
}

static struct IrType *FoldSignature(struct IrTypeFolder *F, struct IrSignature *t)
{
    struct IrTypeList *types = F->FoldTypeList(F, t->types);
    struct IrTypeList *params = F->FoldTypeList(F, t->params);
    struct IrType *result = FoldType(F, t->result);
    struct IrType *self = FoldType(F, t->self);
    struct IrType *r = pawIr_new_signature(F->C, t->did, types, params, result);
    IrGetSignature(r)->self = self; // TODO: maybe should store separately
    return r;
}

static struct IrType *FoldFuncPtr(struct IrTypeFolder *F, struct IrFuncPtr *t)
{
    struct IrTypeList *params = F->FoldTypeList(F, t->params);
    struct IrType *result = FoldType(F, t->result);
    return pawIr_new_func_ptr(F->C, params, result);
}

static struct IrType *FoldTuple(struct IrTypeFolder *F, struct IrTuple *t)
{
    struct IrTypeList *elems = F->FoldTypeList(F, t->elems);
    return pawIr_new_tuple(F->C, elems);
}

static struct IrType *FoldTraitObj(struct IrTypeFolder *F, struct IrTraitObj *t)
{
    struct IrTypeList *types = F->FoldTypeList(F, t->types);
    return pawIr_new_trait_obj(F->C, t->did, types);
}

static struct IrType *FoldNever(struct IrTypeFolder *F, struct IrNever *t)
{
    return pawIr_new_never(F->C);
}

static void FoldPat(struct HirVisitor *V, struct HirPat *node)
{
    paw_assert(node != NULL);
    struct IrTypeFolder *F = V->ud;
    struct IrType *type = GET_NODE_TYPE(F->C, node);
    SET_NODE_TYPE(F->C, node, pawIr_fold_type(F, type));
}

static void FoldExpr(struct HirVisitor *V, struct HirExpr *node)
{
    paw_assert(node != NULL);
    struct IrTypeFolder *F = V->ud;
    struct IrType *type = GET_NODE_TYPE(F->C, node);
    SET_NODE_TYPE(F->C, node, pawIr_fold_type(F, type));
}

static void FoldDecl(struct HirVisitor *V, struct HirDecl *node)
{
    paw_assert(node != NULL);
    struct IrTypeFolder *F = V->ud;
    struct IrType *type = GET_NODE_TYPE(F->C, node);
    SET_NODE_TYPE(F->C, node, pawIr_fold_type(F, type));
}

static struct IrType *FoldType(struct IrTypeFolder *F, struct IrType *node)
{
    if (node == NULL)
        return NULL;
    if (F->FoldType != NULL)
        node = F->FoldType(F, node);

    switch (IR_KINDOF(node)) {
#define DEFINE_ACCEPT(X)        \
    case kIr##X:                \
        if (F->Fold##X == NULL) \
            return node;        \
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

        .FoldAdt = FoldAdt,
        .FoldSignature = FoldSignature,
        .FoldFuncPtr = FoldFuncPtr,
        .FoldTuple = FoldTuple,
        .FoldTraitObj = FoldTraitObj,
        .FoldNever = FoldNever,
    };
}

struct IrType *pawIr_fold_type(struct IrTypeFolder *F, struct IrType *node)
{
    return FoldType(F, node);
}

struct IrTypeList *pawIr_fold_type_list(struct IrTypeFolder *F, struct IrTypeList *list)
{
    return list != NULL ? F->FoldTypeList(F, list) : NULL;
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
        struct MirRegisterData *data = mir_reg_data(V->mir, r.r);
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
