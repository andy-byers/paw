// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <limits.h>
#include <stdlib.h>
#include "hir.h"
#include "compile.h"
#include "map.h"
#include "mem.h"

#define LIST_MIN 8
#define FIRST_ARENA_SIZE 4096
#define LARGE_ARENA_MIN 32

struct Hir *pawHir_new(struct Compiler *C)
{
    paw_Env *P = ENV(C);
    struct DynamicMem *dm = C->dm;
    dm->hir = pawM_new(P, struct Hir);
    dm->hir->dm = C->dm;
    dm->hir->P = P;

    // initialize memory pools for storing HIR components
    pawK_pool_init(P, &dm->hir->pool, FIRST_ARENA_SIZE, sizeof(void *) * LARGE_ARENA_MIN);
    dm->hir->symtab = pawHir_symtab_new(dm->hir);
    return dm->hir;
}

void pawHir_free(struct Hir *hir)
{
    if (hir != NULL) {
        paw_Env *P = ENV(hir);
        pawK_pool_uninit(P, &hir->pool);
        pawM_free(P, hir);
    }
}

#define NEW_NODE(hir, T) pawK_pool_alloc(ENV(hir), &(hir)->pool, sizeof(struct T))

#define DEFINE_NODE_CONSTRUCTOR(name, T) \
        struct T *pawHir_new_##name(struct Hir *hir, int line, enum T##Kind kind) \
        { \
            struct T *r = NEW_NODE(hir, T); \
            r->hdr.line = line; \
            r->hdr.kind = kind; \
            return r; \
        }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)

#define LIST_MIN_ALLOC 8

struct HirType *pawHir_new_type(struct Hir *hir, int line, enum HirTypeKind kind)
{
    struct HirType *r = NEW_NODE(hir, HirType);
    r->hdr.kind = kind;
    r->hdr.line = line;
    return r;
}

struct HirSegment *pawHir_segment_new(struct Hir *hir)
{
    return NEW_NODE(hir, HirSegment);
}

DefId pawHir_add_decl(struct Hir *hir, struct HirDecl *decl)
{
    paw_Env *P = ENV(hir);
    struct DynamicMem *dm = hir->dm;
    const DefId id = dm->decls->count;
    pawHir_decl_list_push(hir, dm->decls, decl);
    decl->hdr.did = id;
    return id;
}

struct HirDecl *pawHir_get_decl(struct Hir *hir, DefId did)
{
    struct DynamicMem *dm = hir->dm;
    paw_assert(did < dm->decls->count);
    return dm->decls->data[did];
}

struct HirSymbol *pawHir_new_symbol(struct Hir *hir)
{
    return pawK_pool_alloc(ENV(hir), &hir->pool, sizeof(struct HirSymbol));
}

void pawHir_add_scope(struct Hir *hir, struct HirSymtab *table, struct HirScope *scope)
{
    if (table->count == UINT16_MAX) pawM_error(ENV(hir));
    pawHir_symtab_push(hir, table, scope);
}

struct HirSymbol *pawHir_add_symbol(struct Hir *hir, struct HirScope *table)
{
    struct HirSymbol *symbol = pawHir_new_symbol(hir);
    pawHir_scope_push(hir, table, symbol);
    return symbol;
}

int pawHir_find_symbol(struct HirScope *scope, const String *name)
{
    for (int i = scope->count - 1; i >= 0; --i) {
        const struct HirSymbol *symbol = scope->data[i];
        if (pawS_eq(name, symbol->name)) {
            if (symbol->is_init) {
                return i;
            }
        }
    }
    return -1;
}

#define DEFINE_LIST_VISITOR(name, T) \
    static void visit_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            V->Visit##T(V, list->data[i]); \
        } \
    }
DEFINE_LIST_VISITOR(decl, Decl) 
DEFINE_LIST_VISITOR(expr, Expr)
DEFINE_LIST_VISITOR(stmt, Stmt)
DEFINE_LIST_VISITOR(type, Type)

static void VisitBlock(struct HirVisitor *V, struct HirBlock *s)
{
    V->VisitStmtList(V, s->stmts);
}

static void VisitLogicalExpr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);
}

static void VisitFieldExpr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    if (e->fid < 0) V->VisitExpr(V, e->key);
    V->VisitExpr(V, e->value);
}

static void VisitAssignExpr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);
}

static void VisitLiteralExpr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitComposite:
            V->VisitPath(V, e->comp.path);
            V->VisitExprList(V, e->comp.items);
            break;
        case kHirLitContainer:
            V->VisitExprList(V, e->cont.items);
            break;
        case kHirLitTuple:
            V->VisitExprList(V, e->tuple.elems);
            break;
        default:
            break;
    }
}

static void VisitChainExpr(struct HirVisitor *V, struct HirChainExpr *e)
{
    V->VisitExpr(V, e->target);
}

static void VisitUnOpExpr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    V->VisitExpr(V, e->target);
}

static void VisitBinOpExpr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);
}

static void VisitExprStmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    V->VisitExpr(V, s->expr);
}

static void VisitClosureExpr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    V->VisitDeclList(V, e->params);
    if (e->has_body) {
        V->VisitBlock(V, e->body);
    } else {
        V->VisitExpr(V, e->expr);
    }
}

static void VisitFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    paw_unused(V);
    paw_unused(d);
}

static void VisitTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    V->VisitDeclList(V, d->generics);
    V->VisitExpr(V, d->rhs);
}

static void VisitGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    paw_unused(V);
    paw_unused(d);
}

static void VisitVariantDecl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    V->VisitDeclList(V, d->fields);
}

static void VisitAdtDecl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    V->VisitDeclList(V, d->generics);
    V->VisitDeclList(V, d->fields);
}

static void VisitInstanceDecl(struct HirVisitor *V, struct HirInstanceDecl *d)
{
    V->VisitTypeList(V, d->types);
}

static void VisitVarDecl(struct HirVisitor *V, struct HirVarDecl *d)
{
    V->VisitExpr(V, d->init);
}

static void VisitReturnStmt(struct HirVisitor *V, struct HirReturnStmt *s)
{
    V->VisitExpr(V, s->expr);
}

static void VisitCallExpr(struct HirVisitor *V, struct HirCallExpr *e)
{
    V->VisitExpr(V, e->target);
    V->VisitExprList(V, e->args);
}

static void VisitConversionExpr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    V->VisitExpr(V, e->arg);
}

static void VisitPathExpr(struct HirVisitor *V, struct HirPathExpr *e)
{
    V->VisitPath(V, e->path);
}

static void VisitFuncDecl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    V->VisitDeclList(V, d->generics);
    V->VisitDeclList(V, d->params);
    if (d->body != NULL) V->VisitBlock(V, d->body);
}

static void VisitIfStmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    V->VisitExpr(V, s->cond);
    V->VisitStmt(V, s->then_arm);
    V->VisitStmt(V, s->else_arm);
}

static void VisitWhileStmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    V->VisitExpr(V, s->cond);
    V->VisitBlock(V, s->block);
}

static void VisitLabelStmt(struct HirVisitor *V, struct HirLabelStmt *s)
{
    paw_unused(V);
    paw_unused(s);
}

static void VisitForStmt(struct HirVisitor *V, struct HirForStmt *s)
{
    if (s->is_fornum) {
        V->VisitExpr(V, s->fornum.begin);
        V->VisitExpr(V, s->fornum.end);
        V->VisitExpr(V, s->fornum.step);
    } else {
        V->VisitExpr(V, s->forin.target);
    }
    V->VisitBlock(V, s->block);
}

static void VisitIndex(struct HirVisitor *V, struct HirIndex *e)
{
    V->VisitExpr(V, e->target);
    V->VisitExpr(V, e->first);
}

static void VisitSelector(struct HirVisitor *V, struct HirSelector *e)
{
    V->VisitExpr(V, e->target);
}

static void VisitPath(struct HirVisitor *V, struct HirPath *path)
{
    paw_unused(V);
    paw_unused(path);
}

static void VisitDeclStmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    V->VisitDecl(V, s->decl);
}

static void VisitFuncDef(struct HirVisitor *V, struct HirFuncDef *t)
{
    V->VisitTypeList(V, t->types);
    V->VisitTypeList(V, t->params);
    V->VisitType(V, t->result);
}

static void VisitFuncPtr(struct HirVisitor *V, struct HirFuncPtr *t)
{
    V->VisitTypeList(V, t->params);
    V->VisitType(V, t->result);
}

static void VisitTupleType(struct HirVisitor *V, struct HirTupleType *t)
{
    V->VisitTypeList(V, t->elems);
}

static void VisitAdt(struct HirVisitor *V, struct HirAdt *t)
{
    V->VisitTypeList(V, t->types);
}

static void VisitUnknown(struct HirVisitor *V, struct HirUnknown *t)
{
    paw_unused(V);
    paw_unused(t);
}

static void VisitGeneric(struct HirVisitor *V, struct HirGeneric *t)
{
    paw_unused(V);
    paw_unused(t);
}

static void VisitExpr(struct HirVisitor *V, struct HirExpr *node)
{
    if (node == NULL) return;
    V->VisitType(V, node->hdr.type);
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b); \
        break;
        HIR_EXPR_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

static void VisitDecl(struct HirVisitor *V, struct HirDecl *node)
{
    if (node == NULL) return;
    V->VisitType(V, node->hdr.type);
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b); \
        break;
        HIR_DECL_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

static void VisitStmt(struct HirVisitor *V, struct HirStmt *node)
{
    if (node == NULL) return;
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b); \
        break;
        HIR_STMT_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

static void VisitType(struct HirVisitor *V, struct HirType *node)
{
    if (node == NULL) return;
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b); \
        break;
        HIR_TYPE_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

void pawHir_visitor_init(struct HirVisitor *V, struct Hir *hir, void *ud)
{
    *V = (struct HirVisitor){
        .hir = hir,
        .ud = ud,

        .VisitExpr = VisitExpr,
        .VisitPath = VisitPath,
        .VisitDecl = VisitDecl,
        .VisitStmt = VisitStmt,
        .VisitType = VisitType,

        .VisitExprList = visit_expr_list,
        .VisitDeclList = visit_decl_list,
        .VisitStmtList = visit_stmt_list,
        .VisitTypeList = visit_type_list,

#define DEFINE_CALLBACKS(a, b) .Visit##a = Visit##a,
        HIR_EXPR_LIST(DEFINE_CALLBACKS)
        HIR_DECL_LIST(DEFINE_CALLBACKS)
        HIR_STMT_LIST(DEFINE_CALLBACKS)
        HIR_TYPE_LIST(DEFINE_CALLBACKS)
#undef DEFINE_CALLBACKS
    };
}

void pawHir_visit(struct HirVisitor *V)
{
    struct Hir *hir = V->hir;
    V->VisitDeclList(V, hir->items);
}

// Generate code for folding a list of HIR nodes
#define DEFINE_LIST_FOLDER(name, T) \
    static struct Hir##T##List *fold_##name##_list(struct HirFolder *F, struct Hir##T##List *list) \
    { \
        if (list == NULL) return NULL; \
        for (int i = 0; i < list->count; ++i) { \
            list->data[i] = F->Fold##T(F, list->data[i]); \
        } \
        return list; \
    } 
DEFINE_LIST_FOLDER(decl, Decl) 
DEFINE_LIST_FOLDER(expr, Expr)
DEFINE_LIST_FOLDER(stmt, Stmt)
DEFINE_LIST_FOLDER(type, Type)

static struct HirStmt *FoldBlock(struct HirFolder *F, struct HirBlock *s)
{
    s->stmts = F->FoldStmtList(F, s->stmts);
    return HIR_CAST_STMT(s);
}
#define FOLD_BLOCK(F, s) CAST(struct HirBlock *, (F)->FoldBlock(F, s))
#define MAYBE_FOLD_BLOCK(F, s) ((s) != NULL ? FOLD_BLOCK(F, s) : NULL)

static struct HirExpr *FoldLogicalExpr(struct HirFolder *F, struct HirLogicalExpr *e)
{
    e->lhs = F->FoldExpr(F, e->lhs);
    e->rhs = F->FoldExpr(F, e->rhs);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldFieldExpr(struct HirFolder *F, struct HirFieldExpr *e)
{
    if (e->fid < 0) e->key = F->FoldExpr(F, e->key);
    e->value = F->FoldExpr(F, e->value);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldAssignExpr(struct HirFolder *F, struct HirAssignExpr *e)
{
    e->lhs = F->FoldExpr(F, e->lhs);
    e->rhs = F->FoldExpr(F, e->rhs);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldLiteralExpr(struct HirFolder *F, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitComposite:
            e->comp.path = F->FoldPath(F, e->comp.path);
            e->comp.items = F->FoldExprList(F, e->comp.items);
            break;
        case kHirLitTuple:
            e->tuple.elems = F->FoldExprList(F, e->tuple.elems);
            break;
        default:
            break;
    }
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldChainExpr(struct HirFolder *F, struct HirChainExpr *e)
{
    e->target = F->FoldExpr(F, e->target);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldUnOpExpr(struct HirFolder *F, struct HirUnOpExpr *e)
{
    e->target = F->FoldExpr(F, e->target);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldBinOpExpr(struct HirFolder *F, struct HirBinOpExpr *e)
{
    e->lhs = F->FoldExpr(F, e->lhs);
    e->rhs = F->FoldExpr(F, e->rhs);
    return HIR_CAST_EXPR(e);
}

static struct HirStmt *FoldExprStmt(struct HirFolder *F, struct HirExprStmt *s)
{
    s->expr = F->FoldExpr(F, s->expr);
    return HIR_CAST_STMT(s);
}

static struct HirExpr *FoldClosureExpr(struct HirFolder *F, struct HirClosureExpr *e)
{
    e->params = F->FoldDeclList(F, e->params);
    if (e->has_body) {
        e->body = FOLD_BLOCK(F, e->body);
    } else {
        e->expr = F->FoldExpr(F, e->expr);
    }
    return HIR_CAST_EXPR(e);
}

static struct HirDecl *FoldFieldDecl(struct HirFolder *F, struct HirFieldDecl *d)
{
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldTypeDecl(struct HirFolder *F, struct HirTypeDecl *d)
{
    d->generics = F->FoldDeclList(F, d->generics);
    d->rhs = F->FoldExpr(F, d->rhs);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldGenericDecl(struct HirFolder *F, struct HirGenericDecl *d)
{
    paw_unused(F);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldVariantDecl(struct HirFolder *F, struct HirVariantDecl *d)
{
    d->fields = F->FoldDeclList(F, d->fields);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldAdtDecl(struct HirFolder *F, struct HirAdtDecl *d)
{
    d->generics = F->FoldDeclList(F, d->generics);
    d->fields = F->FoldDeclList(F, d->fields);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldInstanceDecl(struct HirFolder *F, struct HirInstanceDecl *d)
{
    d->types = F->FoldTypeList(F, d->types);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldVarDecl(struct HirFolder *F, struct HirVarDecl *d)
{
    d->init = F->FoldExpr(F, d->init);
    return HIR_CAST_DECL(d);
}

static struct HirStmt *FoldReturnStmt(struct HirFolder *F, struct HirReturnStmt *s)
{
    s->expr = F->FoldExpr(F, s->expr);
    return HIR_CAST_STMT(s);
}

static struct HirExpr *FoldCallExpr(struct HirFolder *F, struct HirCallExpr *e)
{
    e->target = F->FoldExpr(F, e->target);
    e->args = F->FoldExprList(F, e->args);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldConversionExpr(struct HirFolder *F, struct HirConversionExpr *e)
{
    e->arg = F->FoldExpr(F, e->arg);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldPathExpr(struct HirFolder *F, struct HirPathExpr *e)
{
    e->path = F->FoldPath(F, e->path);
    return HIR_CAST_EXPR(e);
}

static struct HirDecl *FoldFuncDecl(struct HirFolder *F, struct HirFuncDecl *d)
{
    d->generics = F->FoldDeclList(F, d->generics);
    d->params = F->FoldDeclList(F, d->params);
    if (d->body != NULL) d->body = FOLD_BLOCK(F, d->body);
    return HIR_CAST_DECL(d);
}

static struct HirStmt *FoldIfStmt(struct HirFolder *F, struct HirIfStmt *s)
{
    s->cond = F->FoldExpr(F, s->cond);
    s->then_arm = F->FoldStmt(F, s->then_arm);
    s->else_arm = F->FoldStmt(F, s->else_arm);
    return HIR_CAST_STMT(s);
}

static struct HirStmt *FoldWhileStmt(struct HirFolder *F, struct HirWhileStmt *s)
{
    s->cond = F->FoldExpr(F, s->cond);
    s->block = FOLD_BLOCK(F, s->block);
    return HIR_CAST_STMT(s);
}

static struct HirStmt *FoldLabelStmt(struct HirFolder *F, struct HirLabelStmt *s)
{
    paw_unused(F);
    return HIR_CAST_STMT(s);
}

static struct HirStmt *FoldForStmt(struct HirFolder *F, struct HirForStmt *s)
{
    if (s->is_fornum) {
        s->fornum.begin = F->FoldExpr(F, s->fornum.begin);
        s->fornum.end = F->FoldExpr(F, s->fornum.end);
        s->fornum.step = F->FoldExpr(F, s->fornum.step);
    } else {
        s->forin.target = F->FoldExpr(F, s->forin.target);
    }
    s->block = FOLD_BLOCK(F, s->block);
    return HIR_CAST_STMT(s);
}

static struct HirExpr *FoldIndex(struct HirFolder *F, struct HirIndex *e)
{
    e->target = F->FoldExpr(F, e->target);
    e->first = F->FoldExpr(F, e->first);
    e->second = F->FoldExpr(F, e->second);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldSelector(struct HirFolder *F, struct HirSelector *e)
{
    e->target = F->FoldExpr(F, e->target);
    return HIR_CAST_EXPR(e);
}

static struct HirPath *FoldPath(struct HirFolder *F, struct HirPath *path)
{
    paw_unused(F);
    return path;
}

static struct HirStmt *FoldDeclStmt(struct HirFolder *F, struct HirDeclStmt *s)
{
    s->decl = F->FoldDecl(F, s->decl);
    return HIR_CAST_STMT(s);
}

static struct HirType *FoldFuncDef(struct HirFolder *F, struct HirFuncDef *t)
{
    t->types = F->FoldTypeList(F, t->types);
    t->params = F->FoldTypeList(F, t->params);
    t->result = F->FoldType(F, t->result);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldFuncPtr(struct HirFolder *F, struct HirFuncPtr *t)
{
    t->params = F->FoldTypeList(F, t->params);
    t->result = F->FoldType(F, t->result);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldTupleType(struct HirFolder *F, struct HirTupleType *t)
{
    t->elems = F->FoldTypeList(F, t->elems);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldAdt(struct HirFolder *F, struct HirAdt *t)
{
    t->types = F->FoldTypeList(F, t->types);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldUnknown(struct HirFolder *F, struct HirUnknown *t)
{
    paw_unused(F);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldGeneric(struct HirFolder *F, struct HirGeneric *t)
{
    paw_unused(F);
    return HIR_CAST_TYPE(t);
}

static struct HirExpr *FoldExpr(struct HirFolder *F, struct HirExpr *node)
{
    if (node == NULL) return NULL;
    node->hdr.type = F->FoldType(F, node->hdr.type);
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
        return F->Fold##a(F, &node->b);
        HIR_EXPR_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
}

static struct HirDecl *FoldDecl(struct HirFolder *F, struct HirDecl *node)
{
    if (node == NULL) return NULL;
    node->hdr.type = F->FoldType(F, node->hdr.type);
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
        return F->Fold##a(F, &node->b);
        HIR_DECL_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
}

static struct HirStmt *FoldStmt(struct HirFolder *F, struct HirStmt *node)
{
    if (node == NULL) return NULL;
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
        return F->Fold##a(F, &node->b);
        HIR_STMT_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
}

static struct HirType *FoldType(struct HirFolder *F, struct HirType *node)
{
    if (node == NULL) return NULL;
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
        return F->Fold##a(F, &node->b);
        HIR_TYPE_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
}

void pawHir_folder_init(struct HirFolder *F, struct Hir *hir, void *ud)
{
    *F = (struct HirFolder){
        .hir = hir,
        .ud = ud,

        .FoldExpr = FoldExpr,
        .FoldPath = FoldPath,
        .FoldDecl = FoldDecl,
        .FoldStmt = FoldStmt,
        .FoldType = FoldType,

        .FoldExprList = fold_expr_list,
        .FoldDeclList = fold_decl_list,
        .FoldStmtList = fold_stmt_list,
        .FoldTypeList = fold_type_list,

#define DEFINE_CALLBACKS(a, b) .Fold##a = Fold##a,
        HIR_EXPR_LIST(DEFINE_CALLBACKS)
        HIR_DECL_LIST(DEFINE_CALLBACKS)
        HIR_STMT_LIST(DEFINE_CALLBACKS)
        HIR_TYPE_LIST(DEFINE_CALLBACKS)
#undef DEFINE_CALLBACKS
    };
}

void pawHir_fold(struct HirFolder *F)
{
    struct Hir *hir = F->hir;
    F->FoldDeclList(F, hir->items);
}

// ****************************
//     HIR copying routines
// ****************************

struct Copier {
    struct Hir *hir;
    paw_Env *P;
};

#define DEFINE_COPY_PREP(name, T) \
    static T *copy_prep_##name##_aux(struct HirFolder *F, T *t) \
    { \
        T *r = pawHir_new_##name(F->hir, t->hdr.line, HIR_KINDOF(t)); \
        r->hdr = t->hdr; \
        return r; \
    }
DEFINE_COPY_PREP(expr, struct HirExpr) 
DEFINE_COPY_PREP(decl, struct HirDecl)
DEFINE_COPY_PREP(stmt, struct HirStmt)

// Helpers for copying: create a new node of the given type and kind,
// and copy the common fields
#define COPY_PREP_EXPR(F, e) copy_prep_expr_aux(F, HIR_CAST_EXPR(e))
#define COPY_PREP_DECL(F, d) copy_prep_decl_aux(F, HIR_CAST_DECL(d))
#define COPY_PREP_STMT(F, s) copy_prep_stmt_aux(F, HIR_CAST_STMT(s))

#define DEFINE_COPY_LIST(name, T) \
    static struct Hir##T##List *copy_##name##_list(struct HirFolder *F, struct Hir##T##List *old_list) \
    { \
        if (old_list == NULL) return NULL; \
        struct Hir##T##List *new_list = pawHir_##name##_list_new(F->hir); \
        for (int i = 0; i < old_list->count; ++i) { \
            struct Hir##T *elem = F->Fold##T(F, old_list->data[i]); \
            pawHir_##name##_list_push(F->hir, new_list, elem); \
        } \
        return new_list; \
    }
DEFINE_COPY_LIST(decl, Decl) 
DEFINE_COPY_LIST(expr, Expr)
DEFINE_COPY_LIST(stmt, Stmt)
DEFINE_COPY_LIST(type, Type)

static struct HirStmt *copy_block_stmt(struct HirFolder *F, struct HirBlock *s)
{
    struct HirStmt *r = COPY_PREP_STMT(F, s);
    r->block.stmts = F->FoldStmtList(F, s->stmts);
    return r;
}

#define COPY_BLOCK(F, s) CAST(struct HirBlock *, (F)->FoldBlock(F, s))

static struct HirExpr *copy_logical_expr(struct HirFolder *F, struct HirLogicalExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->logical.lhs = F->FoldExpr(F, e->lhs);
    r->logical.rhs = F->FoldExpr(F, e->rhs);
    return r;
}

static struct HirExpr *copy_field_expr(struct HirFolder *F, struct HirFieldExpr *e)
{
    struct HirExpr *result = COPY_PREP_EXPR(F, e);
    struct HirFieldExpr *r = HirGetFieldExpr(result);
    r->fid = e->fid;
    if (e->fid < 0) {
        r->key = F->FoldExpr(F, e->key);
    } else {
        r->name = e->name;
    }
    r->value = F->FoldExpr(F, e->value);
    return result;
}

static struct HirExpr *copy_assign_expr(struct HirFolder *F, struct HirAssignExpr *e)
{
    struct HirExpr *result = COPY_PREP_EXPR(F, e);
    struct HirAssignExpr *r = HirGetAssignExpr(result);
    r->lhs = F->FoldExpr(F, e->lhs);
    r->rhs = F->FoldExpr(F, e->rhs);
    return result;
}

static struct HirExpr *copy_literal_expr(struct HirFolder *F, struct HirLiteralExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->literal.lit_kind = e->lit_kind;
    switch (e->lit_kind) {
        case kHirLitBasic:
            r->literal.basic = e->basic;
            break;
        case kHirLitTuple:
            r->literal.tuple.elems = F->FoldExprList(F, e->tuple.elems);
            break;
        case kHirLitContainer:
            r->literal.cont.code = e->cont.code;
            r->literal.cont.items = F->FoldExprList(F, e->cont.items);
            break;
        default:
            paw_assert(e->lit_kind == kHirLitComposite);
            r->literal.comp.path = F->FoldPath(F, e->comp.path);
            r->literal.comp.items = F->FoldExprList(F, e->comp.items);
            break;
    }
    return r;
}

static struct HirExpr *copy_chain_expr(struct HirFolder *F, struct HirChainExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->chain.target = F->FoldExpr(F, e->target);
    return r;
}

static struct HirExpr *copy_unop_expr(struct HirFolder *F, struct HirUnOpExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->unop.target = F->FoldExpr(F, e->target);
    r->unop.op = e->op;
    return r;
}

static struct HirExpr *copy_binop_expr(struct HirFolder *F, struct HirBinOpExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->binop.lhs = F->FoldExpr(F, e->lhs);
    r->binop.rhs = F->FoldExpr(F, e->rhs);
    r->binop.op = e->op;
    return r;
}

static struct HirStmt *copy_expr_stmt(struct HirFolder *F, struct HirExprStmt *s)
{
    struct HirStmt *r = COPY_PREP_STMT(F, s);
    r->expr.expr = F->FoldExpr(F, s->expr);
    return r;
}

static struct HirExpr *copy_closure_expr(struct HirFolder *F, struct HirClosureExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->clos.params = F->FoldDeclList(F, e->params);
    if (e->has_body) {
        r->clos.body = COPY_BLOCK(F, e->body);
        r->clos.has_body = PAW_TRUE;
    } else {
        r->clos.expr = F->FoldExpr(F, e->expr);
    }
    return r;
}

static struct HirDecl *copy_field_decl(struct HirFolder *F, struct HirFieldDecl *d)
{
    struct HirDecl *r = COPY_PREP_DECL(F, d);
    r->field.name = d->name;
    return r;
}

static struct HirDecl *copy_type_decl(struct HirFolder *F, struct HirTypeDecl *d)
{
    struct HirDecl *r = COPY_PREP_DECL(F, d);
    r->type.name = d->name;
    r->type.generics = F->FoldDeclList(F, d->generics);
    r->type.rhs = F->FoldExpr(F, d->rhs);
    return r;
}

static struct HirDecl *copy_generic_decl(struct HirFolder *F, struct HirGenericDecl *d)
{
    struct HirDecl *r = COPY_PREP_DECL(F, d);
    r->generic.name = d->name;
    return r;
}

static struct HirDecl *copy_adt_decl(struct HirFolder *F, struct HirAdtDecl *d)
{
    struct HirDecl *r = COPY_PREP_DECL(F, d);
    r->adt.is_struct = d->is_struct;
    r->adt.is_pub = d->is_pub;
    r->adt.name = d->name;
    r->adt.generics = F->FoldDeclList(F, d->generics);
    r->adt.fields = F->FoldDeclList(F, d->fields);
    r->adt.monos = F->FoldDeclList(F, d->monos);
    return r;
}

static struct HirDecl *copy_variant_decl(struct HirFolder *F, struct HirVariantDecl *d)
{
    struct HirDecl *r = COPY_PREP_DECL(F, d);
    r->variant.fields = F->FoldDeclList(F, d->fields);
    r->variant.index = d->index;
    r->variant.name = d->name;
    return r;
}

static struct HirDecl *copy_var_decl(struct HirFolder *F, struct HirVarDecl *d)
{
    struct HirDecl *r = COPY_PREP_DECL(F, d);
    r->var.is_const = d->is_const;
    r->var.name = d->name;
    r->var.init = F->FoldExpr(F, d->init);
    return r;
}

static struct HirStmt *copy_return_stmt(struct HirFolder *F, struct HirReturnStmt *s)
{
    struct HirStmt *r = COPY_PREP_STMT(F, s);
    r->result.expr = F->FoldExpr(F, s->expr);
    return r;
}

static struct HirExpr *copy_conversion_expr(struct HirFolder *F, struct HirConversionExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->conv.arg = F->FoldExpr(F, e->arg);
    r->conv.to = e->to;
    return r;
}

static struct HirExpr *copy_call_expr(struct HirFolder *F, struct HirCallExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->call.target = F->FoldExpr(F, e->target);
    r->call.args = F->FoldExprList(F, e->args);
    r->call.func = e->func;
    return r;
}

static struct HirPath *copy_path(struct HirFolder *F, struct HirPath *e)
{
    struct HirPath *r = pawHir_path_new(F->hir);
    for (int i = 0; i < e->count; ++i) {
        struct HirSegment *seg = pawHir_path_get(e, i);
        pawHir_path_add(F->hir, r, seg->name, seg->types, seg->result);
    }
    return r;
}

static struct HirExpr *copy_path_expr(struct HirFolder *F, struct HirPathExpr *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->path.path = F->FoldPath(F, e->path);
    return r;
}

static struct HirDecl *copy_func_decl(struct HirFolder *F, struct HirFuncDecl *d)
{
    struct HirDecl *r = COPY_PREP_DECL(F, d);
    r->func.is_pub = d->is_pub;
    r->func.receiver = NULL; // set during visit_*()
    r->func.name = d->name;
    r->func.generics = F->FoldDeclList(F, d->generics);
    r->func.params = F->FoldDeclList(F, d->params);
    if (d->body != NULL) r->func.body = COPY_BLOCK(F, d->body);
    r->func.monos = F->FoldDeclList(F, d->monos);
    r->func.fn_kind = d->fn_kind;
    return r;
}

static struct HirStmt *copy_if_stmt(struct HirFolder *F, struct HirIfStmt *s)
{
    struct HirStmt *r = COPY_PREP_STMT(F, s);
    r->if_.cond = F->FoldExpr(F, s->cond);
    r->if_.then_arm = F->FoldStmt(F, s->then_arm);
    r->if_.else_arm = F->FoldStmt(F, s->else_arm);
    return r;
}

static struct HirStmt *copy_while_stmt(struct HirFolder *F, struct HirWhileStmt *s)
{
    struct HirStmt *r = COPY_PREP_STMT(F, s);
    r->while_.is_dowhile = s->is_dowhile;
    r->while_.cond = F->FoldExpr(F, s->cond);
    r->while_.block = COPY_BLOCK(F, s->block);
    return r;
}

static struct HirStmt *copy_label_stmt(struct HirFolder *F, struct HirLabelStmt *s)
{
    struct HirStmt *r = COPY_PREP_STMT(F, s);
    r->label.label = s->label;
    return r;
}

static struct HirStmt *copy_for_stmt(struct HirFolder *F, struct HirForStmt *s)
{
    struct HirStmt *r = COPY_PREP_STMT(F, s);
    r->for_.is_fornum = s->is_fornum;
    r->for_.control = F->FoldDecl(F, s->control);
    if (s->is_fornum) {
        r->for_.fornum.begin = F->FoldExpr(F, s->fornum.begin);
        r->for_.fornum.end = F->FoldExpr(F, s->fornum.end);
        r->for_.fornum.step = F->FoldExpr(F, s->fornum.step);
    } else {
        r->for_.forin.target = F->FoldExpr(F, s->forin.target);
    }
    r->for_.block = COPY_BLOCK(F, s->block);
    return r;
}

static struct HirExpr *copy_index_expr(struct HirFolder *F, struct HirIndex *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->index.target = F->FoldExpr(F, e->target);
    r->index.first = F->FoldExpr(F, e->first);
    r->index.second = F->FoldExpr(F, e->second);
    r->index.is_slice = e->is_slice;
    return r;
}

static struct HirExpr *copy_select_expr(struct HirFolder *F, struct HirSelector *e)
{
    struct HirExpr *r = COPY_PREP_EXPR(F, e);
    r->select.is_index = e->is_index;
    r->select.target = F->FoldExpr(F, e->target);
    r->select.name = e->name;
    return r;
}

static struct HirStmt *copy_decl_stmt(struct HirFolder *F, struct HirDeclStmt *s)
{
    struct HirStmt *r = COPY_PREP_STMT(F, s);
    r->decl.decl = F->FoldDecl(F, s->decl);
    return r;
}

static void setup_copy_pass(struct HirFolder *F, struct Copier *C)
{
    pawHir_folder_init(F, C->hir, C);
    F->FoldExprList = copy_expr_list;
    F->FoldDeclList = copy_decl_list;
    F->FoldStmtList = copy_stmt_list;
    F->FoldTypeList = copy_type_list;
    F->FoldLiteralExpr = copy_literal_expr;
    F->FoldLogicalExpr = copy_logical_expr;
    F->FoldPath = copy_path;
    F->FoldPathExpr = copy_path_expr;
    F->FoldChainExpr = copy_chain_expr;
    F->FoldUnOpExpr = copy_unop_expr;
    F->FoldBinOpExpr = copy_binop_expr;
    F->FoldConversionExpr = copy_conversion_expr;
    F->FoldCallExpr = copy_call_expr;
    F->FoldIndex = copy_index_expr;
    F->FoldSelector = copy_select_expr;
    F->FoldFieldExpr = copy_field_expr;
    F->FoldAssignExpr = copy_assign_expr;
    F->FoldClosureExpr = copy_closure_expr;
    F->FoldBlock = copy_block_stmt;
    F->FoldExprStmt = copy_expr_stmt;
    F->FoldDeclStmt = copy_decl_stmt;
    F->FoldIfStmt = copy_if_stmt;
    F->FoldForStmt = copy_for_stmt;
    F->FoldWhileStmt = copy_while_stmt;
    F->FoldLabelStmt = copy_label_stmt;
    F->FoldReturnStmt = copy_return_stmt;
    F->FoldVariantDecl = copy_variant_decl;
    F->FoldVarDecl = copy_var_decl;
    F->FoldFuncDecl = copy_func_decl;
    F->FoldAdtDecl = copy_adt_decl;
    F->FoldFieldDecl = copy_field_decl;
    F->FoldGenericDecl = copy_generic_decl;
    F->FoldTypeDecl = copy_type_decl;
}

struct HirDecl *pawHir_copy_decl(struct Hir *hir, struct HirDecl *decl)
{
    struct Copier C = {
        .P = hir->P,
        .hir = hir,
    };
    struct HirFolder F;
    setup_copy_pass(&F, &C);
    return F.FoldDecl(&F, decl);
}

struct Subst {
    struct Subst *outer;
    struct HirTypeList *before;
    struct HirTypeList *after;
    struct Stenciler *S;
};

struct Expander {
    struct HirFolder copy;
    struct Subst *subst;
    struct Resolver *R;
    struct Hir *hir;
    paw_Env *P;
    int nexpand;
};

static struct HirTypeList *expand_typelist(struct HirFolder *F, struct HirTypeList *list)
{
    if (list == NULL) return NULL;
    struct HirTypeList *copy = pawHir_type_list_new(F->hir);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = F->FoldType(F, list->data[i]);
        pawHir_type_list_push(F->hir, copy, type);
    }
    return copy;
}

static struct HirType *expand_tuple(struct HirFolder *F, struct HirTupleType *t)
{
    struct Expander *E = F->ud;
    struct HirType *r = pawHir_new_type(E->hir, t->line, kHirTupleType);
    r->tuple.elems = F->FoldTypeList(F, t->elems);
    return r;
}

static struct HirType *expand_fptr(struct HirFolder *F, struct HirFuncPtr *t)
{
    struct Expander *E = F->ud;
    struct HirType *r = pawHir_new_type(E->hir, t->line, kHirFuncPtr);
    r->fptr.params = F->FoldTypeList(F, t->params);
    r->fptr.result = F->FoldType(F, t->result);
    return r;
}

static struct HirType *expand_fdef(struct HirFolder *F, struct HirFuncDef *t)
{
    struct Expander *E = F->ud;
    struct HirTypeList *types = expand_typelist(F, t->types);
    struct HirDecl *base = pawHir_get_decl(E->hir, t->base);
    struct HirDecl *inst = pawP_instantiate(E->R, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *expand_adt(struct HirFolder *F, struct HirAdt *t)
{
    struct Expander *E = F->ud;
    if (t->did <= PAW_TSTR) return HIR_CAST_TYPE(t);
    struct HirTypeList *types = expand_typelist(F, t->types);
    struct HirDecl *base = pawHir_get_decl(E->hir, t->base);
    struct HirDecl *inst = pawP_instantiate(E->R, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *expand_generic(struct HirFolder *F, struct HirGeneric *t)
{
    struct Expander *E = F->ud;
    if (E->subst == NULL) return HIR_CAST_TYPE(t);
    const struct HirTypeList *before = E->subst->before;
    const struct HirTypeList *after = E->subst->after;
    for (int i = 0; i < before->count; ++i) {
        struct HirType *search = before->data[i];
        if (HirIsGeneric(search)) {
            struct HirGeneric *g = HirGetGeneric(search);
            if (t->did == g->did) return after->data[i];
        }
    }
    return HIR_CAST_TYPE(t);
}

static struct HirFuncDecl *do_expand(struct HirFolder *F, struct HirFuncDecl *base, struct HirDecl *inst)
{
    struct Expander *E = F->ud;
    struct Subst subst = {
        .outer = E->subst,
        .before = HirGetFuncDef(base->type)->types,
        .after = HirGetInstanceDecl(inst)->types,
    };
    E->subst = &subst;

    inst->func.is_pub = base->is_pub;
    inst->func.fn_kind = base->fn_kind;
    inst->func.kind = kHirFuncDecl;
    inst->func.name = base->name;
    inst->func.generics = NULL;

    // copy the original polymorphic function subtree
    struct HirType *type = E->copy.FoldType(&E->copy, base->type);
    struct HirDeclList *params = E->copy.FoldDeclList(&E->copy, base->params);
    struct HirBlock *body = MAYBE_FOLD_BLOCK(&E->copy, base->body);

    // replace generics with concrete types and create further instantiations to
    // be expanded next round
    inst->func.type = F->FoldType(F, type);
    inst->func.params = F->FoldDeclList(F, params);
    inst->func.body = MAYBE_FOLD_BLOCK(F, body);

    E->subst = subst.outer;
    return &inst->func;
}

static struct HirDecl *find_decl(struct Expander *E, struct HirType *type)
{
    DefId did;
    switch (HIR_KINDOF(type)) {
        case kHirAdt: 
            did = HirGetAdt(type)->base;
            break;
        case kHirFuncDef: 
            did = HirGetFuncDef(type)->base;
            break;
        default: 
            did = HirGetGeneric(type)->did;
    }
    return pawHir_get_decl(E->hir, did);
}

static struct HirPath *expand_path(struct HirFolder *F, struct HirPath *path)
{
    struct Expander *E = F->ud;
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment *ps = pawHir_path_get(path, i);
        if (ps->types != NULL) {
            struct HirTypeList *types = F->FoldTypeList(F, ps->types);
            ps->result = pawP_instantiate(E->R, ps->result, types);
        }
    }
    return path;
}

static struct HirDecl *expand_func_decl(struct HirFolder *F, struct HirFuncDecl *d)
{
    struct Expander *E = F->ud;
    const int n = d->monos ? d->monos->count : 0;
    for (int i = 0; i < n; ++i) {
        struct HirDecl *decl = d->monos->data[i];
        if (HirIsInstanceDecl(decl)) {
            do_expand(F, d, decl);
            ++E->nexpand;
        }
    }
    return HIR_CAST_DECL(d);
}

static struct HirExpr *expand_call_expr(struct HirFolder *F, struct HirCallExpr *e)
{
    struct Expander *E = F->ud;
    if (HirIsFuncDef(e->func)) {
        struct HirFuncDef *fdef = HirGetFuncDef(e->func);
        struct HirTypeList *types = expand_typelist(F, fdef->types);
        struct HirDecl *base = pawHir_get_decl(F->hir, fdef->base);
        struct HirDecl *inst = pawP_instantiate(E->R, base, types);
        e->func = HIR_TYPEOF(inst);
    } else {
        e->func = F->FoldType(F, e->func);
    }
    e->target = F->FoldExpr(F, e->target);
    e->args = F->FoldExprList(F, e->args);
    return HIR_CAST_EXPR(e);
}

static struct HirDecl *expand_adt_decl(struct HirFolder *F, struct HirAdtDecl *d)
{
    paw_unused(F);
    return HIR_CAST_DECL(d);
}

void pawHir_expand(struct Resolver *R, struct Hir *hir)
{
    struct HirFolder F;
    struct Expander E = {
        .hir = R->hir,
        .P = ENV(R),
        .R = R,
    };
    struct Copier C = {
        .hir = hir,
        .P = ENV(R),
    };
    setup_copy_pass(&E.copy, &C);
    pawHir_folder_init(&F, hir, &E);
    F.FoldAdtDecl = expand_adt_decl;
    F.FoldFuncDecl = expand_func_decl;
    F.FoldCallExpr = expand_call_expr;
    F.FoldPath = expand_path;
    F.FoldTypeList = expand_typelist;
    F.FoldAdt = expand_adt;
    F.FoldTupleType = expand_tuple;
    F.FoldFuncPtr = expand_fptr;
    F.FoldFuncDef = expand_fdef;
    F.FoldGeneric = expand_generic;

    do {
        E.nexpand = 0;
        F.FoldDeclList(&F, hir->prelude);
        F.FoldDeclList(&F, hir->items);
    } while (E.nexpand > 0);
}

struct DefState {
    struct DefState *outer;
    struct HirDecl *decl;
    struct Def *def;
};

struct DefGenerator {
    struct DefState *ds;
    struct Hir *hir;
    struct HirDeclList *decls;
    struct Def *pub_list;
    paw_Env *P;
    Map *types;
    int nvals;
};

static void enter_def_(struct DefGenerator *dg, struct DefState *ds, struct HirDecl *decl, struct Def *def)
{
    *ds = (struct DefState){
        .outer = dg->ds,
        .decl = decl,
        .def = def,
    };
    dg->ds = ds;
}

#define ENTER_DEF(dg, ds, decl, def) enter_def_(dg, ds, HIR_CAST_DECL(decl), CAST(struct Def *, def))
#define LEAVE_DEF(dg) ((dg)->ds = (dg)->ds->outer)

static struct Type *create_type(struct DefGenerator *dg, struct HirType *type);
static paw_Bool match_types(struct DefGenerator *dg, struct HirType *lhs, struct Type *rhs);

static paw_Bool match_type_lists(struct DefGenerator *dg, struct HirTypeList *lhs, struct TypeList *rhs)
{
    if (lhs->count != rhs->count) return PAW_FALSE;
    for (int i = 0; i < lhs->count; ++i) {
        if (!match_types(dg, lhs->data[i], rhs->data[i])) return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool match_functions(struct DefGenerator *dg, struct HirType *lhs, struct Type *rhs)
{
    if (!HirIsFuncType(lhs) || rhs->hdr.kind != TYPE_FUNC) return PAW_FALSE;
    return match_type_lists(dg, HIR_FPTR(lhs)->params, &rhs->func.params) &&
        match_types(dg, HIR_FPTR(lhs)->result, rhs->func.result);
}

static paw_Bool match_tuples(struct DefGenerator *dg, struct HirType *lhs, struct Type *rhs)
{
    if (!HirIsTupleType(lhs) || rhs->hdr.kind != TYPE_TUPLE) return PAW_FALSE;
    return match_type_lists(dg, HirGetTupleType(lhs)->elems, &rhs->tuple.types);
}

static paw_Bool match_adts(struct DefGenerator *dg, struct HirType *lhs, struct Type *rhs)
{
    if (!HirIsAdt(lhs) || rhs->hdr.kind != TYPE_ADT) return PAW_FALSE;
    struct HirDecl *decl = dg->decls->data[rhs->adt.did];
    return HirIsAdtDecl(decl) && HIR_TYPEOF(decl)->adt.did == HirGetAdt(lhs)->did;
}

static Value *find_type(struct DefGenerator *dg, struct HirType *type)
{
    const Value key = {.p = type};
    return pawH_get(dg->types, key);
}

static paw_Bool match_types(struct DefGenerator *dg, struct HirType *lhs, struct Type *rhs)
{
    return match_functions(dg, lhs, rhs) ||
        match_tuples(dg, lhs, rhs) ||
        match_adts(dg, lhs, rhs);
}

static struct Type *search_type(struct DefGenerator *dg, struct HirType *target)
{
    paw_Env *P = ENV(dg);
    // check if this particular HirType has been seen already
    Value *pv = find_type(dg, target);
    if (pv != NULL) return pv->p;
    // check if there is an existing type that is equivalent to this one
    for (int i = 0; i < P->types.count; ++i) {
        struct Type *type = P->types.data[i]; 
        if (match_types(dg, target, type)) return type;
    }
    return NULL;
}

static paw_Type map_types(struct DefGenerator *dg, struct HirType *src, struct Type *dst)
{
    paw_Env *P = ENV(dg);
    paw_assert(find_type(dg, src) == NULL);
    pawH_insert(P, dg->types, (Value){.p = src}, (Value){.p = dst});

    dst->hdr.code = P->types.count;
    pawM_grow(P, P->types.data, P->types.count, P->types.alloc);
    P->types.data[P->types.count++] = dst;
    return P->types.count - 1;
}

static struct Type *new_type(struct DefGenerator *, struct HirType *);

static void init_type_list(struct DefGenerator *dg, struct HirTypeList *x, struct TypeList *y)
{
    if (x == NULL) return;
    pawE_init_type_list(ENV(dg), y, x->count);
    for (int i = 0; i < x->count; ++i) {
        y->data[i] = new_type(dg, x->data[i]);
    }
}

static struct Type *new_type(struct DefGenerator *dg, struct HirType *src)
{
    struct Type *dst = search_type(dg, src);
    if (dst != NULL) return dst;

    paw_Env *P = ENV(dg);
    switch (HIR_KINDOF(src)) {
        case kHirFuncDef:
        case kHirFuncPtr: {
            struct HirFuncPtr *fptr = HIR_FPTR(src);
            dst = pawE_new_type(P, TYPE_FUNC);
            dst->func.result = new_type(dg, fptr->result);
            init_type_list(dg, fptr->params, &dst->func.params);
            break;
        }
        case kHirTupleType: {
            struct HirTupleType *tuple = HirGetTupleType(src);
            dst = pawE_new_type(P, TYPE_TUPLE);
            init_type_list(dg, tuple->elems, &dst->tuple.types);
            break;
        }
        default: {
            struct HirAdt *adt = HirGetAdt(src);
            dst = pawE_new_type(P, TYPE_ADT);
            init_type_list(dg, adt->types, &dst->adt.types);
        }
    }
    map_types(dg, src, dst);
    return dst;
}

static paw_Bool handle_monos(struct HirVisitor *V, struct HirDeclList *monos)
{
    if (monos == NULL) return PAW_FALSE;
    for (int i = 0; i < monos->count; ++i) {
        V->VisitDecl(V, monos->data[i]);
    }
    return PAW_TRUE;
}

static paw_Bool has_generic(struct HirType *type)
{
    if (HirIsGeneric(type)) return PAW_TRUE;
    struct HirTypeList *types = 
        HirIsFuncDef(type) ? HirGetFuncDef(type)->types : 
        HirIsTupleType(type) ? HirGetTupleType(type)->elems : 
        HirIsAdt(type) ? HirGetAdt(type)->types : NULL;
    if (types == NULL) return PAW_FALSE;
    for (int i = 0; i < types->count; ++i) {
        struct HirType *type = types->data[i];
        if (has_generic(type)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

static struct Def *new_def_(struct DefGenerator *dg, struct HirDecl *decl)
{
    struct Def *def;
    paw_Env *P = ENV(dg);
    switch (HIR_KINDOF(decl)) {
#define LEN(L) ((L) != NULL ? (L)->count : 0)
        case kHirVarDecl: 
        case kHirFieldDecl: 
            def = pawE_new_def(P, DEF_VAR); 
            break;
        case kHirFuncDecl: 
            def = pawE_new_def(P, DEF_FUNC); 
            pawE_init_field_list(P, &def->func.params, 
                    LEN(HirGetFuncDecl(decl)->params));
            break;
        case kHirVariantDecl:
            def = pawE_new_def(P, DEF_FUNC); 
            pawE_init_field_list(P, &def->func.params, 
                    LEN(HirGetVariantDecl(decl)->fields));
            break;
        default: // kHirAdtDecl
            def = pawE_new_def(P, DEF_ADT); 
            pawE_init_field_list(P, &def->adt.fields, 
                    LEN(HirGetAdtDecl(decl)->fields));
#undef LEN
    }
    def->hdr.name = decl->hdr.name;
    def->hdr.type = new_type(dg, HIR_TYPEOF(decl));
    pawHir_decl_list_push(dg->hir, dg->decls, decl);
    return def;
}

#define NEW_DEF(dg, decl) new_def_(dg, HIR_CAST_DECL(decl))

static void define_func_decl(struct HirVisitor *V, struct HirFuncDecl *d) 
{
    struct DefGenerator *dg = V->ud;
    if (handle_monos(V, d->monos)) return;
    if (has_generic(d->type)) return;

    paw_Env *P = ENV(dg);
    struct Def *result = NEW_DEF(dg, d);
    struct FuncDef *def = &result->func;
    def->is_pub = d->is_pub;
    def->vid = dg->nvals++; // allocate value slot

    struct DefState ds;
    ENTER_DEF(dg, &ds, d, def);
    V->VisitDeclList(V, d->params);
    LEAVE_DEF(dg);

    // C functions don't have a 'body' field
    if (d->body == NULL) return;
    V->VisitBlock(V, d->body);
}

static void define_adt_decl(struct HirVisitor *V, struct HirAdtDecl *d) 
{
    struct DefGenerator *dg = V->ud;
    if (handle_monos(V, d->monos)) return;
    if (has_generic(d->type)) return;

    paw_Env *P = ENV(dg);
    const int did = P->defs.count;
    struct Def *result = NEW_DEF(dg, d);
    struct AdtDef *def = &result->adt;
    def->is_struct = d->is_struct;
    def->is_pub = d->is_pub;
    def->type->adt.did = did;

    struct DefState ds;
    ENTER_DEF(dg, &ds, d, def);
    V->VisitDeclList(V, d->fields);
    LEAVE_DEF(dg);
}

static void define_field_decl(struct HirVisitor *V, struct HirFieldDecl *d) 
{
    NEW_DEF(V->ud, d);
}

static void define_var_decl(struct HirVisitor *V, struct HirVarDecl *d) 
{
    NEW_DEF(V->ud, d);
}

static void define_variant_decl(struct HirVisitor *V, struct HirVariantDecl *d) 
{
    struct DefGenerator *dg = V->ud;
    struct Def *result = NEW_DEF(dg, d);
    struct FuncDef *def = &result->func;

    struct DefState ds;
    ENTER_DEF(dg, &ds, d, def);
    V->VisitDeclList(V, d->fields);
    LEAVE_DEF(dg);
}

struct HirDeclList *pawHir_define(struct Compiler *C, struct Hir *hir)
{
    paw_Env *P = ENV(hir);
    ENSURE_STACK(P, 1);
    Value *pv = pawC_push0(P);
    struct DefGenerator dg = {
        .decls = pawHir_decl_list_new(hir),
        .types = pawH_new(P),
        .hir = hir,
        .P = P,
    };
    V_SET_OBJECT(pv, dg.types);

    struct HirVisitor V;
    pawHir_visitor_init(&V, hir, &dg);
    V.VisitFieldDecl = define_field_decl;
    V.VisitVarDecl = define_var_decl;
    V.VisitAdtDecl = define_adt_decl;
    V.VisitFuncDecl = define_func_decl;
    V.VisitVariantDecl = define_variant_decl;

    V.VisitDeclList(&V, hir->prelude);
    V.VisitDeclList(&V, hir->items);

    pawC_pop(P);

    paw_assert(dg.decls->count == P->defs.count);
    paw_assert(P->vals.data == NULL);
    P->vals.data = pawM_new_vec(P, dg.nvals, Value);
    P->vals.count = P->vals.alloc = dg.nvals;
    return dg.decls;
}

#if defined(PAW_DEBUG_EXTRA)

struct Printer {
    struct Hir *hir;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define DUMP_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define DUMP_STRING(P, str) pawL_add_string(ENV(P), (P)->buf, str)
#define DUMP_FSTRING(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)

static void indent_line(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        DUMP_LITERAL(P, "    ");
    }
}

#define DUMP_FMT(P, ...) (indent_line(P), pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__))
#define DUMP_MSG(P, msg) (indent_line(P), pawL_add_string(ENV(P), (P)->buf, msg))

static const char *get_typename(struct DynamicMem *dm, DefId did)
{
    const struct HirDecl *decl = dm->decls->data[did];
    return decl->hdr.name ? decl->hdr.name->text : "(null)";
}

#define DEFINE_KIND_PRINTER(name, T) \
    static int print_##name##_kind(struct Printer *P, void *node) \
    { \
        if (node != NULL) { \
            struct T *typed = node; \
            DUMP_FSTRING(P, "%s {\n", k##T##Names[HIR_KINDOF(typed)]); \
            return 0; \
        } \
        DUMP_LITERAL(P, "(null)\n"); \
        return -1; \
    }
DEFINE_KIND_PRINTER(expr, HirExpr) 
DEFINE_KIND_PRINTER(decl, HirDecl) 
DEFINE_KIND_PRINTER(stmt, HirStmt) 
DEFINE_KIND_PRINTER(type, HirType) 

#define DUMP_BLOCK(P, b) CHECK_EXP(!(b) || HirIsBlock(HIR_CAST_STMT(b)), (b) ? dump_stmt(P, HIR_CAST_STMT(b)) : DUMP_LITERAL(P, "{}\n"))
#define DUMP_NAME(P, s) DUMP_FMT(P, "name: %s\n", s ? s->text : "(null)")

static void dump_expr(struct Printer *, struct HirExpr *);
static void dump_decl(struct Printer *, struct HirDecl *);
static void dump_stmt(struct Printer *, struct HirStmt *);
static void dump_type(struct Printer *, struct HirType *);

#define DEFINE_LIST_PRINTER(name, T) \
    static void dump_##name##_list(struct Printer *P, struct T##List *list, const char *name) \
    { \
        DUMP_FMT(P, "%s: " #T "List {\n", name); \
        ++P->indent; \
        if (list != NULL) { \
            for (int i = 0; i < list->count; ++i) { \
                DUMP_MSG(P, "" /* indent */); \
                dump_##name(P, list->data[i]); \
            } \
        } \
        --P->indent; \
        DUMP_MSG(P, "}\n"); \
    }
DEFINE_LIST_PRINTER(expr, HirExpr) 
DEFINE_LIST_PRINTER(decl, HirDecl)
DEFINE_LIST_PRINTER(stmt, HirStmt)
DEFINE_LIST_PRINTER(type, HirType)

static void dump_path(struct Printer *P, struct HirPath *p)
{
    for (int i = 0; i < p->count; ++i) {
        struct HirSegment *seg = p->data[i];
        DUMP_STRING(P, seg->name->text);
        if (seg->types != NULL) {
            DUMP_LITERAL(P, "<");
            for (int j = 0; j < seg->types->count; ++j) {
                dump_type(P, seg->types->data[i]);
                if (j < seg->types->count - 1) {
                    DUMP_LITERAL(P, ", ");
                }
            }
            DUMP_LITERAL(P, ">");
        }
    }
    DUMP_LITERAL(P, "\n");
}

static void dump_type(struct Printer *P, struct HirType *t)
{
    if (print_type_kind(P, t)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", t->hdr.line);
    switch (HIR_KINDOF(t)) {
        case kHirTupleType:
            dump_type_list(P, t->tuple.elems, "elems");
            break;
        case kHirFuncDef:
            DUMP_FMT(P, "base: %d\n", t->fdef.base);
            DUMP_FMT(P, "did: %d\n", t->fdef.did);
            if (t->fdef.types != NULL) {
                dump_type_list(P, t->fdef.types, "types");
            }
            // (fallthrough)
        case kHirFuncPtr:
            dump_type_list(P, t->fptr.params, "params");
            DUMP_MSG(P, "result: ");
            dump_type(P, t->fptr.result);
            break;
        case kHirAdt: 
            DUMP_FMT(P, "name: %s\n", get_typename(P->hir->dm, t->adt.did));
            DUMP_FMT(P, "base: %d\n", t->adt.base);
            DUMP_FMT(P, "did: %d\n", t->adt.did);
            if (t->adt.types != NULL) {
                dump_type_list(P, t->adt.types, "types");
            }
            break;
        case kHirUnknown:
            DUMP_FMT(P, "index: %d\n", t->unknown.index);
            break;
        case kHirGeneric:
            DUMP_FMT(P, "name: %s\n", t->generic.name->text);
            DUMP_FMT(P, "did: %d\n", t->generic.did);
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_decl(struct Printer *P, struct HirDecl *d)
{
    if (print_decl_kind(P, d)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "did: %d\n", d->hdr.did);
    DUMP_FMT(P, "line: %d\n", d->hdr.line);
    DUMP_MSG(P, "type: ");
    dump_type(P, HIR_TYPEOF(d));
    switch (HIR_KINDOF(d)) {
        case kHirFuncDecl:
            DUMP_FMT(P, "receiver: %p\n", (void *)d->func.receiver);
            DUMP_FMT(P, "name: %s\n", d->func.name->text);
            dump_decl_list(P, d->func.generics, "generics");
            dump_decl_list(P, d->func.params, "params");
            DUMP_MSG(P, "body: ");
            DUMP_BLOCK(P, d->func.body);
            dump_decl_list(P, d->func.monos, "monos");
            break;
        case kHirFieldDecl:
            DUMP_NAME(P, d->field.name);
            break;
        case kHirVarDecl:
            DUMP_NAME(P, d->var.name);
            DUMP_MSG(P, "init: ");
            dump_expr(P, d->var.init);
            break;
        case kHirVariantDecl:
            DUMP_NAME(P, d->variant.name);
            dump_decl_list(P, d->variant.fields, "fields");
            break;
        case kHirAdtDecl:
            DUMP_NAME(P, d->adt.name);
            DUMP_FMT(P, "is_struct: %d\n", d->adt.is_struct);
            dump_decl_list(P, d->adt.generics, "generics");
            dump_decl_list(P, d->adt.fields, "fields");
            dump_decl_list(P, d->func.monos, "monos");
            break;
        case kHirGenericDecl:
            DUMP_NAME(P, d->generic.name);
            break;
        case kHirTypeDecl:
            DUMP_NAME(P, d->type.name);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, d->type.rhs);
            dump_decl_list(P, d->type.generics, "generics");
            break;
        case kHirInstanceDecl:
            DUMP_NAME(P, d->type.name);
            dump_type_list(P, d->inst.types, "types");
            break;
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_stmt(struct Printer *P, struct HirStmt *s)
{
    if (print_stmt_kind(P, s)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", s->hdr.line);
    switch (HIR_KINDOF(s)) {
        case kHirExprStmt:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->expr.expr);
            break;
        case kHirBlock:
            dump_stmt_list(P, s->block.stmts, "stmts");
            break;
        case kHirDeclStmt:
            DUMP_MSG(P, "decl: ");
            dump_decl(P, s->decl.decl);
            break;
        case kHirIfStmt:
            DUMP_MSG(P, "cond: ");
            dump_expr(P, s->if_.cond);
            DUMP_MSG(P, "then_arm: ");
            dump_stmt(P, s->if_.then_arm);
            DUMP_MSG(P, "else_arm: ");
            dump_stmt(P, s->if_.else_arm);
            break;
        case kHirForStmt:
            if (s->for_.is_fornum) {
                DUMP_MSG(P, "begin: ");
                dump_expr(P, s->for_.fornum.begin);
                DUMP_MSG(P, "end: ");
                dump_expr(P, s->for_.fornum.end);
                DUMP_MSG(P, "step: ");
                dump_expr(P, s->for_.fornum.step);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, s->for_.block);
            } else {
                DUMP_MSG(P, "target: ");
                dump_expr(P, s->for_.forin.target);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, s->for_.block);
            }
            break;
        case kHirWhileStmt:
            DUMP_FMT(P, "is_dowhile: %d\n", s->while_.is_dowhile);
            DUMP_MSG(P, "cond: ");
            dump_expr(P, s->while_.cond);
            DUMP_MSG(P, "block: ");
            DUMP_BLOCK(P, s->while_.block);
            break;
        case kHirReturnStmt:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->result.expr);
            break;
        case kHirLabelStmt:
            DUMP_FMT(P, "label: %s\n", s->label.label == LBREAK ? "BREAK" : "CONTINUE");
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_expr(struct Printer *P, struct HirExpr *e)
{
    if (print_expr_kind(P, e)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", e->hdr.line);
    DUMP_MSG(P, "type: ");
    dump_type(P, HIR_TYPEOF(e));
    switch (HIR_KINDOF(e)) {
        case kHirLiteralExpr:
            switch (e->literal.lit_kind) {
                case kHirLitBasic:
                    DUMP_MSG(P, "lit_kind: BASIC\n");
                    switch (e->literal.basic.t) {
                        case PAW_TUNIT:
                            break;
                        case PAW_TBOOL:
                            DUMP_FMT(P, "value: %s\n",
                                     V_TRUE(e->literal.basic.value) ? "true"
                                                                    : "false");
                            break;
                        case PAW_TINT:
                            DUMP_FMT(P, "value: %I\n",
                                     V_INT(e->literal.basic.value));
                            break;
                        case PAW_TFLOAT:
                            DUMP_FMT(P, "value: %f\n",
                                     V_FLOAT(e->literal.basic.value));
                            break;
                        default:
                            paw_assert(e->literal.basic.t == PAW_TSTR);
                            DUMP_FMT(P, "value: %s\n",
                                     V_STRING(e->literal.basic.value)->text);
                            break;
                    }
                    break;
                case kHirLitTuple:
                    DUMP_MSG(P, "lit_kind: TUPLE\n");
                    dump_expr_list(P, e->literal.tuple.elems, "elems");
                    break;
                case kHirLitContainer:
                    DUMP_MSG(P, "lit_kind: CONTAINER\n");
                    dump_expr_list(P, e->literal.cont.items, "items");
                    break;
                case kHirLitComposite:
                    DUMP_MSG(P, "lit_kind: COMPOSITE\n");
                    DUMP_MSG(P, "target: ");
                    dump_path(P, e->literal.comp.path);
                    dump_expr_list(P, e->literal.comp.items, "items");
            }
            break;
        case kHirChainExpr:
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->chain.target);
            break;
        case kHirLogicalExpr:
            DUMP_FMT(P, "is_and: %d\n", e->logical.is_and);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->logical.lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->logical.rhs);
            break;
        case kHirClosureExpr:
            dump_decl_list(P, e->clos.params, "params");
            if (e->clos.has_body) {
                DUMP_BLOCK(P, e->clos.body);
            } else {
                dump_expr(P, e->clos.expr);
            }
            break;
        case kHirPathExpr:
            DUMP_MSG(P, "path: ");
            dump_path(P, e->path.path);
            break;
        case kHirConversionExpr:
            DUMP_FMT(P, "to: %d\n", e->conv.to);
            DUMP_MSG(P, "arg: ");
            dump_expr(P, e->conv.arg);
            break;
        case kHirUnOpExpr:
            DUMP_FMT(P, "op: %d\n", e->unop.op);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->unop.target);
            break;
        case kHirBinOpExpr:
            DUMP_FMT(P, "op: %d\n", e->binop.op);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->binop.lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->binop.rhs);
            break;
        case kHirCallExpr:
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->call.target);
            dump_expr_list(P, e->call.args, "args");
            DUMP_MSG(P, "func: ");
            dump_type(P, e->call.func);
            break;
        case kHirIndex:
            DUMP_FMT(P, "is_slice: %d\n", e->index.is_slice);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->index.target);
            DUMP_MSG(P, "first: ");
            dump_expr(P, e->index.first);
            DUMP_MSG(P, "second: ");
            dump_expr(P, e->index.second);
            break;
        case kHirSelector:
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->select.target);
            if (e->select.is_index) {
                DUMP_FMT(P, "index: %I\n", e->select.index);
            } else {
                DUMP_NAME(P, e->select.name);
            }
            break;
        case kHirFieldExpr:
            if (e->field.fid >= 0) {
                DUMP_NAME(P, e->field.name);
            } else {
                DUMP_MSG(P, "key: ");
                dump_expr(P, e->field.key);
            }
            DUMP_MSG(P, "value: ");
            dump_expr(P, e->field.value);
            break;
        case kHirAssignExpr:
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->assign.lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->assign.rhs);
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

void pawHir_dump(struct Hir *hir)
{
    Buffer buf;
    paw_Env *P = ENV(hir);
    pawL_init_buffer(P, &buf);
    struct Printer print = {
        .buf = &buf,
        .hir = hir,
        .P = P, 
    };
    for (int i = 0; i < hir->items->count; ++i) {
        dump_decl(&print, hir->items->data[i]);
    }
    pawL_push_result(P, &buf);
}

#endif
