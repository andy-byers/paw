
#include "type_folder.h"

static struct IrType *FoldType(struct IrTypeFolder *, struct IrType *);

static struct IrTypeList *fold_type_list(struct IrTypeFolder *F, struct IrTypeList *list)
{
    if (list == NULL) return NULL;
    struct Compiler *C = F->C;
    for (int i = 0; i < list->count; ++i) {
        struct IrType *type = FoldType(F, K_LIST_GET(list, i));
        K_LIST_SET(list, i, type);
    }
    return list;
}

static struct IrType *FoldAdt(struct IrTypeFolder *F, struct IrAdt *t)
{
    t->types = F->FoldTypeList(F, t->types);
    return IR_CAST_TYPE(t);
}

static struct IrType *FoldSignature(struct IrTypeFolder *F, struct IrSignature *t)
{
    t->types = F->FoldTypeList(F, t->types);
    t->params = F->FoldTypeList(F, t->params);
    t->result = FoldType(F, t->result);
    return IR_CAST_TYPE(t);
}

static struct IrType *FoldFuncPtr(struct IrTypeFolder *F, struct IrFuncPtr *t)
{
    t->params = F->FoldTypeList(F, t->params);
    t->result = FoldType(F, t->result);
    return IR_CAST_TYPE(t);
}

static struct IrType *FoldTuple(struct IrTypeFolder *F, struct IrTuple *t)
{
    t->elems = F->FoldTypeList(F, t->elems);
    return IR_CAST_TYPE(t);
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
    if (node == NULL) return NULL;
    if (F->FoldType != NULL) node = F->FoldType(F, node);

    switch (IR_KINDOF(node)) {
#define DEFINE_ACCEPT(X) case kIr##X: \
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

        .FoldAdt = FoldAdt,
        .FoldSignature = FoldSignature,
        .FoldFuncPtr = FoldFuncPtr,
        .FoldTuple = FoldTuple,
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

void pawHir_type_folder_init(struct HirTypeFolder *F, struct Compiler *C, void *ud)
{
    *F = (struct HirTypeFolder){
        .ud = ud,
        .C = C,
    };

    pawHir_visitor_init(&F->V, C, F);
    F->V.PostVisitExpr = FoldExpr;
    F->V.PostVisitDecl = FoldDecl;
    F->V.PostVisitPat = FoldPat;

    pawIr_type_folder_init(&F->F, C, F);
}

#define DEFINE_FOLDERS(name, T) \
    void pawHir_fold_##name(struct HirTypeFolder *F, struct Hir##T *node) { \
        paw_assert(node != NULL); \
        F->line = node->hdr.line; \
        pawHir_visit_##name(&F->V, node); \
    } \
    void pawHir_fold_##name##_list(struct HirTypeFolder *F, struct Hir##T##List *list) { \
        for (int i = 0; i < list->count; ++i) { \
            pawHir_visit_##name(&F->V, K_LIST_GET(list, i)); \
        } \
    }
DEFINE_FOLDERS(expr, Expr)
DEFINE_FOLDERS(decl, Decl)
DEFINE_FOLDERS(stmt, Stmt)
DEFINE_FOLDERS(pat, Pat)
#undef DEFINE_FOLDERS


static void FoldRegister(struct MirVisitor *V, MirRegister r)
{
    struct IrTypeFolder *F = V->ud;
    struct MirRegisterData *data = mir_reg_data(V->mir, r);
    data->type = pawIr_fold_type(F, data->type);
}

void pawMir_type_folder_init(struct MirTypeFolder *F, struct Compiler *C, struct Mir *mir, void *ud)
{
    *F = (struct MirTypeFolder){
        .ud = ud,
        .C = C,
    };

    pawMir_visitor_init(&F->V, C, mir, F);
    F->V.PostVisitRegister = FoldRegister;

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
    for (int i = 0; i < mir->children->count; ++i) {
        F->V.mir = K_LIST_GET(mir->children, i);
        pawMir_fold(F, F->V.mir);
    }
    F->V.mir = outer;
}

