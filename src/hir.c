// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "hir.h"
#include "compile.h"
#include "debug.h"
#include "map.h"
#include "mem.h"
#include "type.h"

#define LIST_MIN 8

struct Hir *pawHir_new(struct Compiler *C, String *name, int modno)
{
    paw_Env *P = ENV(C);
    struct Pool *pool = &C->dm->pool;
    struct Hir *hir = pawK_pool_alloc(P, pool, sizeof(struct Hir));
    *hir = (struct Hir){
        .modno = modno,
        .pool = pool,
        .name = name,
        .P = P,
        .C = C,
    };
    hir->items = pawHir_decl_list_new(hir);
    return hir;
}

void pawHir_free(struct Hir *hir)
{
    PAW_UNUSED(hir);
}

#define NEW_NODE(hir, T) pawK_pool_alloc(ENV(hir), (hir)->pool, sizeof(struct T))

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
    struct DynamicMem *dm = hir->C->dm;
    const DefId did = dm->decls->count;
    if(decl->hdr.did!=0){
    
    }
    pawHir_decl_list_push(hir, dm->decls, decl);
    decl->hdr.did = did;
    return did;
}

struct HirDecl *pawHir_get_decl(struct Hir *hir, DefId did)
{
    struct DynamicMem *dm = hir->C->dm;
    paw_assert(did < dm->decls->count);
    return dm->decls->data[did];
}

struct HirSymbol *pawHir_new_symbol(struct Hir *hir)
{
    return pawK_pool_alloc(ENV(hir), hir->pool, sizeof(struct HirSymbol));
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
    PAW_UNUSED(V);
    PAW_UNUSED(d);
}

static void VisitTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    V->VisitDeclList(V, d->generics);
    V->VisitExpr(V, d->rhs);
}

static void VisitGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    PAW_UNUSED(V);
    PAW_UNUSED(d);
}

static void VisitUseDecl(struct HirVisitor *V, struct HirUseDecl *d)
{
    V->VisitPath(V, d->path);
}

static void VisitVariantDecl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    V->VisitDeclList(V, d->fields);
}

static void VisitAdtDecl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    V->VisitDeclList(V, d->generics);
    V->VisitDeclList(V, d->fields);
    V->VisitDeclList(V, d->monos);
}

static void VisitImplDecl(struct HirVisitor *V, struct HirImplDecl *d)
{
    V->VisitDeclList(V, d->generics);
    V->VisitDeclList(V, d->methods);
    V->VisitDeclList(V, d->monos);
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
    V->VisitType(V, d->self);
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
    PAW_UNUSED(V);
    PAW_UNUSED(s);
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
    PAW_UNUSED(V);
    PAW_UNUSED(path);
}

static void VisitDeclStmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    V->VisitDecl(V, s->decl);
}

static void VisitAdt(struct HirVisitor *V, struct HirAdt *t)
{
    V->VisitTypeList(V, t->types);
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

static void VisitPathType(struct HirVisitor *V, struct HirPathType *t)
{
    for (int i = 0; i < t->path->count; ++i) {
        struct HirSegment *seg = K_LIST_GET(t->path, i);
        V->VisitTypeList(V, seg->types);
    }
}

static void VisitUnknown(struct HirVisitor *V, struct HirUnknown *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void VisitGeneric(struct HirVisitor *V, struct HirGeneric *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void VisitExpr(struct HirVisitor *V, struct HirExpr *node)
{
    if (node == NULL) return;
    struct HirType *type = HIR_TYPEOF(node);
    V->VisitType(V, type);

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
    struct HirType *type = HIR_TYPEOF(node);
    V->VisitType(V, type);

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
    PAW_UNUSED(F);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldUseDecl(struct HirFolder *F, struct HirUseDecl *d)
{
    d->path = F->FoldPath(F, d->path);
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
    d->monos = F->FoldDeclList(F, d->monos);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldImplDecl(struct HirFolder *F, struct HirImplDecl *d)
{
    d->generics = F->FoldDeclList(F, d->generics);
    d->methods = F->FoldDeclList(F, d->methods);
    d->monos = F->FoldDeclList(F, d->monos);
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
    d->self = F->FoldType(F, d->self);
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
    PAW_UNUSED(F);
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
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment *seg = K_LIST_GET(path, i);
        seg->types = F->FoldTypeList(F, seg->types);
    }
    return path;
}

static struct HirStmt *FoldDeclStmt(struct HirFolder *F, struct HirDeclStmt *s)
{
    s->decl = F->FoldDecl(F, s->decl);
    return HIR_CAST_STMT(s);
}

static struct HirType *FoldAdt(struct HirFolder *F, struct HirAdt *t)
{
    t->types = F->FoldTypeList(F, t->types);
    return HIR_CAST_TYPE(t);
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

static struct HirType *FoldPathType(struct HirFolder *F, struct HirPathType *t)
{
    for (int i = 0; i < t->path->count; ++i) {
        struct HirSegment *seg = K_LIST_GET(t->path, i);
        F->FoldTypeList(F, seg->types);
    }
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldUnknown(struct HirFolder *F, struct HirUnknown *t)
{
    PAW_UNUSED(F);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldGeneric(struct HirFolder *F, struct HirGeneric *t)
{
    PAW_UNUSED(F);
    return HIR_CAST_TYPE(t);
}

static struct HirType *NoopFoldType(struct HirFolder *F, struct HirType *type)
{
    PAW_UNUSED(F);
    return type;
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
        node = F->Fold##a(F, &node->b); \
        break;
        HIR_TYPE_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
    return F->PostFoldType(F, node);
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

        .PostFoldType = NoopFoldType,

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


// HIR copying routines

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
    struct HirDecl *result = COPY_PREP_DECL(F, d);
    struct HirFieldDecl *r = HirGetFieldDecl(result);
    r->tag = F->FoldType(F, d->tag);
    r->name = d->name;
    return result;
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

static struct HirDecl *copy_impl_decl(struct HirFolder *F, struct HirImplDecl *d)
{
    struct HirDecl *result = COPY_PREP_DECL(F, d);
    struct HirImplDecl *r = HirGetImplDecl(result);
    r->generics = F->FoldDeclList(F, d->generics);
    r->methods = F->FoldDeclList(F, d->methods);
    r->monos = F->FoldDeclList(F, d->monos);
    return result;
}

static struct HirDecl *copy_use_decl(struct HirFolder *F, struct HirUseDecl *d)
{
    struct HirDecl *result = COPY_PREP_DECL(F, d);
    struct HirUseDecl *r = HirGetUseDecl(result);
    r->path = F->FoldPath(F, d->path);
    return result;
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
        struct HirSegment *src = pawHir_path_get(e, i);
        struct HirSegment *dst = pawHir_path_add(F->hir, r, src->name, NULL);
        dst->types = F->FoldTypeList(F, src->types);
        dst->base = src->base;
        dst->did = src->did;
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
    r->func.self = d->self;
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
    F->FoldImplDecl = copy_impl_decl;
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
    struct Copier c;
    struct HirFolder fold;
    struct HirFolder copy;
    struct Subst *subst;
    struct Compiler *C;
    struct Hir *hir;
    paw_Env *P;
    int nexpand;
};

static struct HirTypeList *expand_type_list(struct HirFolder *F, struct HirTypeList *list)
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

static struct HirType *expand_func_ptr(struct HirFolder *F, struct HirFuncPtr *t)
{
    struct Expander *E = F->ud;
    struct HirType *r = pawHir_new_type(E->hir, t->line, kHirFuncPtr);
    r->fptr.params = F->FoldTypeList(F, t->params);
    r->fptr.result = F->FoldType(F, t->result);
    return r;
}

static struct HirType *expand_func_def(struct HirFolder *F, struct HirFuncDef *t)
{
    if (t->types == NULL) {
        struct HirType *result = pawHir_new_type(F->hir, t->line, kHirFuncDef);
        struct HirFuncDef *r = HirGetFuncDef(result);
        r->params = F->FoldTypeList(F, t->params);
        r->result = F->FoldType(F, t->result);
        r->base = r->did = t->did;
        r->modno = t->modno;
        return result;
    }
    struct Expander *E = F->ud;
    struct HirTypeList *types = F->FoldTypeList(F, t->types);
    struct HirDecl *base = pawHir_get_decl(E->hir, t->base);
    struct HirDecl *inst = pawP_instantiate(E->hir, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *expand_adt(struct HirFolder *F, struct HirAdt *t)
{
    struct Expander *E = F->ud;
    if (t->base <= PAW_TSTR) return HIR_CAST_TYPE(t);
    struct HirTypeList *types = F->FoldTypeList(F, t->types);
    struct HirDecl *base = pawHir_get_decl(E->hir, t->base);
    struct HirDecl *inst = pawP_instantiate(E->hir, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *expand_path_type(struct HirFolder *F, struct HirPathType *t)
{
    struct Expander *E = F->ud;
    struct HirSegment *seg = K_LIST_GET(t->path, 0);
    // paw_Type equals DefId for builtins
    if (seg->base <= PAW_TSTR) return HIR_CAST_TYPE(t);
    struct HirTypeList *types = F->FoldTypeList(F, seg->types);
    struct HirDecl *base = pawHir_get_decl(E->hir, seg->base);
    struct HirDecl *inst = pawP_instantiate(E->hir, base, types);
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

static struct HirFuncDecl *do_expand_aux(struct HirFolder *F, struct HirFuncDecl *base, struct HirDecl *inst, struct Subst *subst)
{
    struct Expander *E = F->ud;
    E->subst = subst;

    inst->func.kind = kHirFuncDecl;
    struct HirFuncDecl *r = HirGetFuncDecl(inst);
    r->is_pub = base->is_pub;
    r->fn_kind = base->fn_kind;
    r->name = base->name;
    r->generics = NULL;

    // copy the original polymorphic function subtree
    struct HirDeclList *params = E->copy.FoldDeclList(&E->copy, base->params);
    struct HirBlock *body = MAYBE_FOLD_BLOCK(&E->copy, base->body);

    // replace generics with concrete types and create further instantiations to
    // be expanded next round
    r->self = F->FoldType(F, base->self);
    r->params = F->FoldDeclList(F, params);
    r->body = MAYBE_FOLD_BLOCK(F, body);

    E->subst = subst->outer;
    return r;
}

#define MAX_EXPANSION_DEPTH 128

static void enter_binder(struct Expander *E, struct HirDecl *inst)
{
    if (E->C->nbinders >= MAX_EXPANSION_DEPTH) {
        pawHir_print_type(E->hir, HIR_TYPEOF(inst));
        pawE_error(ENV(E), PAW_ETYPE, inst->hdr.line, 
                "too many nested instantiations for '%s'", paw_string(ENV(E), -1));
    }
    ++E->C->nbinders;

}

static void leave_binder(struct Expander *E)
{
    paw_assert(E->C->nbinders > 0);
    --E->C->nbinders;
}

static struct HirFuncDecl *do_expand(struct HirFolder *F, struct HirFuncDecl *base, struct HirDecl *inst)
{
    struct Expander *E = F->ud;
    struct Subst subst = {
        .outer = E->subst,
        .before = HirGetFuncDef(base->type)->types,
        .after = HirGetInstanceDecl(inst)->types,
    };
    enter_binder(E, inst);
    do_expand_aux(F, base, inst, &subst);
    leave_binder(E);
    return &inst->func;
}

static struct HirDecl *find_decl(struct Expander *E, struct HirType *type)
{
    DefId did;
    switch (HIR_KINDOF(type)) {
        case kHirPathType: 
            did = hir_adt_did(type);
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
            struct HirDecl *base = pawHir_get_decl(E->hir, ps->base);
            struct HirDecl *inst = pawP_instantiate(E->hir, base, types);
            ps->did = inst->hdr.did;
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
        struct HirDecl *base = pawHir_get_decl(F->hir, fdef->base);
        if (HIR_IS_POLY_FUNC(base)) {
            struct HirTypeList *types = expand_type_list(F, fdef->types);
            struct HirDecl *inst = pawP_instantiate(E->hir, base, types);
            e->func = HIR_TYPEOF(inst);
        }
    }
    e->func = F->FoldType(F, e->func);
    e->target = F->FoldExpr(F, e->target);
    e->args = F->FoldExprList(F, e->args);
    return HIR_CAST_EXPR(e);
}

static struct HirDecl *expand_adt_decl_(struct HirFolder *F, struct HirAdtDecl *d)
{
    PAW_UNUSED(F);
    return HIR_CAST_DECL(d);
}

static struct HirTypeList *collect_decl_types(struct Hir *hir, struct HirDeclList *list) 
{                                                                                   
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(hir);
    for (int i = 0; i < list->count; ++i) {                                         
        struct HirType *type = HIR_TYPEOF(list->data[i]);
        pawHir_type_list_push(hir, new_list, type);
    }                                                                               
    return new_list;
}

static struct HirDecl *expand_impl_decl(struct HirFolder *F, struct HirImplDecl *d)
{
    struct Expander *E = F->ud;
    if (d->monos != NULL) {
        for (int i = 0; i < d->monos->count; ++i) {
            struct HirImplDecl *mono = HirGetImplDecl(d->monos->data[i]);
            paw_assert(mono->methods->count == d->methods->count);
            for (int j = 0; j < d->methods->count; ++j) {
                struct HirDecl *inst_decl = mono->methods->data[j];
                struct HirFuncDecl *base_func = HirGetFuncDecl(d->methods->data[j]);

                if (HirIsInstanceDecl(inst_decl)) {
                    struct Subst subst = {
                        .outer = E->subst,
                        .before = d->subst,
                        .after = mono->subst,
                    };
                    do_expand_aux(F, base_func, inst_decl, &subst);
                    ++E->nexpand;
                }
            }
        }
    }
    return HIR_CAST_DECL(d);
}

static struct HirAdtDecl *adt_expand_aux(struct HirFolder *F, struct HirAdtDecl *base, struct HirDecl *inst, struct Subst *subst)
{
    struct Expander *E = F->ud;
    E->subst = subst;

    inst->func.kind = kHirAdtDecl;
    struct HirAdtDecl *r = HirGetAdtDecl(inst);
    r->is_pub = base->is_pub;
    r->is_struct = base->is_struct;
    r->name = base->name;
    r->generics = NULL;

    struct HirDeclList *fields = E->copy.FoldDeclList(&E->copy, base->fields);
    r->fields = F->FoldDeclList(F, fields);

    E->subst = subst->outer;
    return r;
}

struct HirDecl *expand_variant_decl(struct HirFolder *F, struct HirVariantDecl *d)
{
    d->fields = F->FoldDeclList(F, d->fields);
    return HIR_CAST_DECL(d);
}

static struct HirAdtDecl *do_expand_adt(struct HirFolder *F, struct HirAdtDecl *base, struct HirDecl *inst)
{
    struct Expander *E = F->ud;
    struct Subst subst = {
        .outer = E->subst,
        .before = hir_adt_types(base->type),
        .after = HirGetInstanceDecl(inst)->types,
    };
    enter_binder(E, inst);
    adt_expand_aux(F, base, inst, &subst);
    leave_binder(E);
    return &inst->adt;
}

static void setup_expander(struct Compiler *C, struct Hir *hir, struct Expander *E)
{
    *E = (struct Expander){
        .P = ENV(C),
        .hir = hir,
        .C = C,
    };
    E->c = (struct Copier){
        .hir = hir,
        .P = ENV(C),
    };
    setup_copy_pass(&E->copy, &E->c);
    pawHir_folder_init(&E->fold, hir, E);
    E->fold.FoldAdtDecl = expand_adt_decl_;
    E->fold.FoldVariantDecl = expand_variant_decl;
    E->fold.FoldImplDecl = expand_impl_decl;
    E->fold.FoldFuncDecl = expand_func_decl;
    E->fold.FoldCallExpr = expand_call_expr;
    E->fold.FoldPath = expand_path;
    E->fold.FoldTypeList = expand_type_list;
    E->fold.FoldAdt = expand_adt;
    E->fold.FoldPathType = expand_path_type;
    E->fold.FoldTupleType = expand_tuple;
    E->fold.FoldFuncDef = expand_func_def;
    E->fold.FoldFuncPtr = expand_func_ptr;
    E->fold.FoldGeneric = expand_generic;
}

void pawHir_expand_adt(struct Hir *hir, struct HirAdtDecl *base, struct HirDecl *inst)
{
    paw_assert(HirIsInstanceDecl(inst));

    struct Expander E;
    setup_expander(hir->C, hir, &E);
    do_expand_adt(&E.fold, base, inst);
}

void pawHir_expand_bodies(struct Hir *hir)
{
    struct Expander E;
    setup_expander(hir->C, hir, &E);
    struct HirFolder *F = &E.fold;

    do {
        E.nexpand = 0;
        F->FoldDeclList(F, hir->items);
    } while (E.nexpand > 0);
}

struct DefState {
    struct DefState *outer;
    struct HirDecl *decl;
    struct Def *def;
};

struct HirType *pawHir_attach_type(struct Hir *hir, DeclId did, enum HirTypeKind kind, int line)
{
    struct DynamicMem *dm = hir->C->dm;
    struct HirType *type = pawHir_new_type(hir, line, kind);
    if (did != NO_DECL) {
        struct HirDecl *decl = dm->decls->data[did];
        decl->hdr.type = type;
    }
    if (kind == kHirFuncDef) {
        type->fdef.did = did;
    } else if (kind == kHirGeneric) {
        type->generic.did = did;
    }
    return type;
}

struct HirTypeList *pawHir_collect_generics(struct Hir *hir, struct HirDeclList *generics)
{
    struct HirTypeList *types = pawHir_type_list_new(hir);
    for (int i = 0; i < generics->count; ++i) {
        struct HirGenericDecl *d = HirGetGenericDecl(generics->data[i]);
        struct HirType *type = pawHir_attach_type(hir, d->did, kHirGeneric, d->line);
        pawHir_type_list_push(hir, types, type);
        struct HirGeneric *t = HirGetGeneric(type);
        t->name = d->name;
    }
    return types;
}

struct HirTypeList *pawHir_collect_fields(struct Hir *hir, struct HirDeclList *fields)
{
    struct HirTypeList *types = pawHir_type_list_new(hir);
    for (int i = 0; i < fields->count; ++i) {
        struct HirFieldDecl *d = HirGetFieldDecl(fields->data[i]);
        pawHir_type_list_push(hir, types, d->type);
    }
    return types;
}

struct Printer {
    struct Hir *hir;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define PRINT_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define PRINT_STRING(P, str) pawL_add_nstring(ENV(P), (P)->buf, (str)->text, (str)->length)
#define PRINT_FORMAT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define PRINT_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)

static void print_type(struct Printer *, struct HirType *);
static void print_type_list(struct Printer *P, struct HirTypeList *list)
{
    for (int i = 0; i < list->count; ++i) {
        print_type(P, list->data[i]);
        if (i < list->count - 1) PRINT_LITERAL(P, ", ");
    }
}

static void print_type(struct Printer *P, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirTupleType: {
            struct HirTupleType *tup = HirGetTupleType(type);                                
            PRINT_CHAR(P, '(');
            print_type_list(P, tup->elems);
            if (tup->elems->count == 1) PRINT_CHAR(P, ',');
            PRINT_CHAR(P, ')');
            break;
        }
        case kHirFuncDef:
        case kHirFuncPtr: {
            struct HirFuncPtr *fptr = HIR_FPTR(type);
            PRINT_LITERAL(P, "fn(");
            print_type_list(P, fptr->params);
            PRINT_CHAR(P, ')');
            if (!HirIsAdt(fptr->result)
                    || HirGetAdt(fptr->result)->did != PAW_TUNIT) {
                PRINT_LITERAL(P, " -> ");
                print_type(P, fptr->result);
            }
            break;
        }
        case kHirGeneric: {
            struct HirGeneric *gen = HirGetGeneric(type);
            PRINT_STRING(P, gen->name);
            break;
        }
        case kHirUnknown: {
            struct HirUnknown *un = HirGetUnknown(type);
            PRINT_FORMAT(P, "?%d", un->index);
            break;
        }
        case kHirPathType: {
            struct HirPathType *path = HirGetPathType(type);
            for (int i = 0; i < path->path->count; ++i) {
                if (i > 0) PRINT_LITERAL(P, "::");
                struct HirSegment *seg = K_LIST_GET(path->path, i);
                PRINT_STRING(P, seg->name);
                if (seg->types != NULL) {
                    PRINT_CHAR(P, '<');
                    print_type_list(P, seg->types);
                    PRINT_CHAR(P, '>');
                }
            }
            break;
        }
        default: {
            struct HirAdt *adt = HirGetAdt(type);
            struct HirDecl *decl = pawHir_get_decl(P->hir, adt->did);
            PRINT_STRING(P, decl->hdr.name);
            if (adt->types != NULL) {
                PRINT_CHAR(P, '<');
                print_type_list(P, adt->types);
                PRINT_CHAR(P, '>');
            }
            break;
        } 
    }
}

void pawHir_print_type(struct Hir *hir, struct HirType *type)
{
    Buffer buf;
    paw_Env *P = ENV(hir);
    pawL_init_buffer(P, &buf);

    print_type(&(struct Printer){
                .P = ENV(hir),
                .buf = &buf,
                .hir = hir,
            }, type);

    pawL_push_result(P, &buf);
}

#if defined(PAW_DEBUG_EXTRA)

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
        case kHirAdt: {
            DUMP_FMT(P, "base: %d\n", t->adt.base);
            DUMP_FMT(P, "did: %d\n", t->adt.did);
            if (t->adt.types != NULL) {
                dump_type_list(P, t->adt.types, "types");
            }
            break;
        }
        case kHirPathType: 
            for (int i = 0; i < t->path.path->count; ++i) {
                struct HirSegment *seg = K_LIST_GET(t->path.path, i);
                DUMP_FMT(P, "name: %s\n", seg->name->text);
                DUMP_FMT(P, "base: %d\n", seg->base);
                DUMP_FMT(P, "did: %d\n", seg->did);
                if (seg->types != NULL) {
                    dump_type_list(P, seg->types, "types");
                }
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
            DUMP_FMT(P, "self: %p\n", (void *)d->func.self);
            DUMP_FMT(P, "name: %s\n", d->func.name->text);
            dump_decl_list(P, d->func.generics, "generics");
            dump_decl_list(P, d->func.params, "params");
            DUMP_MSG(P, "body: ");
            DUMP_BLOCK(P, d->func.body);
            dump_decl_list(P, d->func.monos, "monos");
            break;
        case kHirUseDecl:
            DUMP_NAME(P, d->use.name);
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
            dump_decl_list(P, d->adt.monos, "monos");
            break;
        case kHirImplDecl:
            DUMP_NAME(P, d->impl.name);
            dump_type_list(P, d->impl.subst, "subst");
            dump_decl_list(P, d->impl.generics, "generics");
            dump_decl_list(P, d->impl.methods, "methods");
            dump_decl_list(P, d->impl.monos, "monos");
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

void pawHir_dump_path(struct Hir *hir, struct HirPath *path)
{
    dump_path(&(struct Printer){
                .P = ENV(hir),
                .hir = hir,
            }, path);
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
