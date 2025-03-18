// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_TYPE_FOLDER_H
#define PAW_TYPE_FOLDER_H

#include "hir.h"
#include "ir_type.h"
#include "mir.h"

struct IrTypeFolder {
    struct Compiler *C;
    void *ud;

    struct IrType *(*FoldType)(struct IrTypeFolder *F, struct IrType *type);
    struct IrTypeList *(*FoldTypeList)(struct IrTypeFolder *F, struct IrTypeList *list);

#define DEFINE_CALLBACK(X) struct IrType *(*Fold##X)(struct IrTypeFolder * F, struct Ir##X * node);
    IR_TYPE_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawIr_type_folder_init(struct IrTypeFolder *F, struct Compiler *C, void *ud);
struct IrType *pawIr_fold_type(struct IrTypeFolder *F, struct IrType *node);
struct IrTypeList *pawIr_fold_type_list(struct IrTypeFolder *F, struct IrTypeList *list);

struct HirTypeFolder {
    struct IrTypeFolder F;
    struct HirVisitor V;
    struct Hir *hir;
    void *ud;
};

void pawHir_type_folder_init(struct HirTypeFolder *F, struct Hir *hir, void *ud);

// Type folder entrypoints for HIR nodes:
void pawHir_fold_expr_type(struct HirTypeFolder *F, struct HirExpr *node);
void pawHir_fold_decl_type(struct HirTypeFolder *F, struct HirDecl *node);
void pawHir_fold_stmt_type(struct HirTypeFolder *F, struct HirStmt *node);
void pawHir_fold_pat_type(struct HirTypeFolder *F, struct HirPat *node);
void pawHir_fold_expr_types(struct HirTypeFolder *F, struct HirExprList *list);
void pawHir_fold_stmt_types(struct HirTypeFolder *F, struct HirStmtList *list);
void pawHir_fold_decl_types(struct HirTypeFolder *F, struct HirDeclList *list);
void pawHir_fold_pat_types(struct HirTypeFolder *F, struct HirPatList *list);

struct MirTypeFolder {
    struct IrTypeFolder F;
    struct MirVisitor V;
    struct Compiler *C;
    void *ud;
};

void pawMir_type_folder_init(struct MirTypeFolder *F, struct Compiler *C, struct Mir *mir, void *ud);

// Type folder entrypoint for MIR:
void pawMir_fold(struct MirTypeFolder *F, struct Mir *mir);

char const *pawIr_print_type(struct Compiler *C, struct IrType *type);

#endif // PAW_TYPE_FOLDER_H
