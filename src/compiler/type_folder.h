// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: Use Visit/Accept distinction to avoid having to manually visit subtrees inside custom Visit callbacks

#ifndef PAW_TYPE_FOLDER_H
#define PAW_TYPE_FOLDER_H

#include "hir/hir.h"
#include "ir/type.h"
#include "mir/mir.h"

struct IrTypeVisitor {
    struct Compiler *C;
    void *ud;

    void (*VisitType)(struct IrTypeVisitor *, IrType *);
    void (*VisitConst)(struct IrTypeVisitor *, IrConst *);
    void (*VisitTrait)(struct IrTypeVisitor *, IrTrait *);
    void (*VisitGenericArg)(struct IrTypeVisitor *, IrGenericArg);
    void (*VisitTypeList)(struct IrTypeVisitor *, IrTypeList *);
    void (*VisitGenericArgs)(struct IrTypeVisitor *, IrGenericArgs *);

#define DEFINE_CALLBACK(X) void (*Visit##X)(struct IrTypeVisitor *, struct Ir##X *);
    IR_TYPE_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void ir_type_visitor_init(struct IrTypeVisitor *F, struct Compiler *C, void *ud);
void ir_visit_type(struct IrTypeVisitor *F, IrType *node);
void ir_visit_const(struct IrTypeVisitor *F, IrConst *node);
void ir_visit_generic_arg(struct IrTypeVisitor *F, IrGenericArg node);
void ir_visit_type_list(struct IrTypeVisitor *F, IrTypeList *list);
void ir_visit_generic_args(struct IrTypeVisitor *F, IrGenericArgs *list);
void ir_visit_trait(struct IrTypeVisitor *F, IrTrait *node);


struct IrTypeFolder {
    struct Compiler *C;
    void *ud;

    IrType *(*FoldType)(struct IrTypeFolder *, IrType *);
    IrConst *(*FoldConst)(struct IrTypeFolder *, IrConst *);
    IrTrait *(*FoldTrait)(struct IrTypeFolder *, IrTrait *);
    IrGenericArg (*FoldGenericArg)(struct IrTypeFolder *, IrGenericArg);
    IrTypeList *(*FoldTypeList)(struct IrTypeFolder *, IrTypeList *);
    IrGenericArgs *(*FoldGenericArgs)(struct IrTypeFolder *, IrGenericArgs *);

#define DEFINE_CALLBACK(X) IrType *(*Fold##X)(struct IrTypeFolder *, struct Ir##X *);
    IR_TYPE_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void ir_type_folder_init(struct IrTypeFolder *F, struct Compiler *C, void *ud);
IrType *ir_fold_type(struct IrTypeFolder *F, IrType *node);
IrConst *ir_fold_const(struct IrTypeFolder *F, IrConst *node);
IrTypeList *ir_fold_type_list(struct IrTypeFolder *F, IrTypeList *list);
IrGenericArg ir_fold_generic_arg(struct IrTypeFolder *F, IrGenericArg arg);
IrGenericArgs *ir_fold_generic_args(struct IrTypeFolder *F, IrGenericArgs *args);
IrTrait *ir_fold_trait(struct IrTypeFolder *F, IrTrait *node);

// TODO: is this even necessary? i think it was at some point but may not be now...
struct HirTypeFolder {
    struct IrTypeFolder F;
    struct HirVisitor V;
    struct Hir *hir;
    void *ud;
};

void hir_type_folder_init(struct HirTypeFolder *F, struct Hir *hir, void *ud);

// Type folder entrypoints for HIR nodes:
void hir_fold_expr_type(struct HirTypeFolder *F, struct HirExpr *node);
void hir_fold_decl_type(struct HirTypeFolder *F, struct HirDecl *node);
void hir_fold_stmt_type(struct HirTypeFolder *F, struct HirStmt *node);
void hir_fold_pat_type(struct HirTypeFolder *F, struct HirPat *node);
void hir_fold_expr_types(struct HirTypeFolder *F, struct HirExprList *list);
void hir_fold_stmt_types(struct HirTypeFolder *F, struct HirStmtList *list);
void hir_fold_decl_types(struct HirTypeFolder *F, struct HirDeclList *list);
void hir_fold_pat_types(struct HirTypeFolder *F, struct HirPatList *list);

struct MirTypeFolder {
    struct IrTypeFolder F;
    struct MirVisitor V;
    struct Compiler *C;
    void *ud;
};

void mir_type_folder_init(struct MirTypeFolder *F, struct Compiler *C, struct Mir *mir, void *ud);

// Type folder entrypoint for MIR:
void mir_fold(struct MirTypeFolder *F, struct Mir *mir);

#endif // PAW_TYPE_FOLDER_H
