// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "hir.h"
#include "compile.h"
#include "debug.h"
#include "ir_type.h"
#include "map.h"
#include "mem.h"
#include "type.h"
#include "type_folder.h"

#define LIST_MIN 8

#define NEW_NODE(C, T) pawP_alloc(C, NULL, 0, sizeof(T))

struct Hir *pawHir_new(struct Compiler *C, String *name, int modno)
{
    struct Hir *hir = NEW_NODE(C, struct Hir);
    *hir = (struct Hir){
        .imports = pawHir_import_list_new(C),
        .items = pawHir_decl_list_new(C),
        .pool = &C->dm->pool,
        .modno = modno,
        .name = name,
        .P = ENV(C),
        .C = C,
    };
    return hir;
}

void pawHir_free(struct Hir *hir)
{
    PAW_UNUSED(hir);
}

#define DEFINE_NODE_CONSTRUCTOR(name, T)         \
    struct T *pawHir_new_##name(struct Hir *hir) \
    {                                            \
        return NEW_NODE(hir->C, struct T);       \
    }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(type, HirType)
DEFINE_NODE_CONSTRUCTOR(pat, HirPat)
#undef DEFINE_NODE_CONSTRUCTOR

#define LIST_MIN_ALLOC 8

void pawHir_init_segment(struct Hir *hir, struct HirSegment *r, String *name, struct HirTypeList *types, DeclId did)
{
    *r = (struct HirSegment){
        .hid = pawHir_next_id(hir),
        .did = did,
        .name = name,
        .types = types,
    };
}

DeclId pawHir_register_decl(struct Hir *hir, struct HirDecl *decl)
{
    struct Compiler *C = hir->C;
    struct DynamicMem *dm = C->dm;
    DeclId const did = {
        .value = C->decls->count,
        .modno = hir->modno,
    };
    K_LIST_PUSH(C, C->decls, decl);
    decl->hdr.did = did;
    return did;
}

struct HirDecl *pawHir_get_decl(struct Compiler *C, DeclId did)
{
    struct DynamicMem *dm = C->dm;
    paw_assert(did.value < C->decls->count);
    return C->decls->data[did.value];
}

struct HirSymbol *pawHir_new_symbol(struct Compiler *C)
{
    return NEW_NODE(C, struct HirSymbol);
}

void pawHir_add_scope(struct Compiler *C, struct HirSymtab *table, struct HirScope *scope)
{
    if (table->count == UINT16_MAX)
        pawM_error(ENV(C));
    K_LIST_PUSH(C, table, scope);
}

int pawHir_declare_symbol(struct Compiler *C, struct HirScope *scope, struct HirDecl *decl, String *name)
{
    K_LIST_PUSH(C, scope, ((struct HirSymbol){
                              .is_type = HirIsAdtDecl(decl)
                                  || HirIsTypeDecl(decl)
                                  || HirIsGenericDecl(decl),
                              .decl = decl,
                              .name = name,
                          }));
    return scope->count - 1;
}

void pawHir_define_symbol(struct HirScope *scope, int index)
{
    K_LIST_GET(scope, index).is_init = PAW_TRUE;
}

int pawHir_find_symbol(struct HirScope *scope, String const *name)
{
    for (int i = scope->count - 1; i >= 0; --i) {
        struct HirSymbol const symbol = K_LIST_GET(scope, i);
        if (pawS_eq(name, symbol.name)) {
            if (symbol.is_init)
                return i;
        }
    }
    return -1;
}

#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

static void AcceptType(struct HirVisitor *V, struct HirType *node);
static void AcceptExpr(struct HirVisitor *V, struct HirExpr *node);
static void AcceptDecl(struct HirVisitor *V, struct HirDecl *node);
static void AcceptStmt(struct HirVisitor *V, struct HirStmt *node);
static void AcceptPat(struct HirVisitor *V, struct HirPat *node);

#define DEFINE_LIST_ACCEPTOR(name, T)                                                 \
    static void accept_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    {                                                                                 \
        if (list == NULL)                                                             \
            return;                                                                   \
        for (int i = 0; i < list->count; ++i) {                                       \
            Accept##T(V, list->data[i]);                                              \
        }                                                                             \
    }
DEFINE_LIST_ACCEPTOR(decl, Decl)
DEFINE_LIST_ACCEPTOR(expr, Expr)
DEFINE_LIST_ACCEPTOR(stmt, Stmt)
DEFINE_LIST_ACCEPTOR(type, Type)
DEFINE_LIST_ACCEPTOR(pat, Pat)

static void AcceptBlock(struct HirVisitor *V, struct HirBlock *e)
{
    accept_stmt_list(V, e->stmts);
    AcceptExpr(V, e->result);
}

static void AcceptLogicalExpr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptFieldExpr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    if (e->fid < 0)
        AcceptExpr(V, e->key);
    AcceptExpr(V, e->value);
}

static void AcceptAssignExpr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptMatchArm(struct HirVisitor *V, struct HirMatchArm *e)
{
    AcceptPat(V, e->pat);
    if (e->guard != NULL)
        AcceptExpr(V, e->guard);
    AcceptExpr(V, e->result);
}

static void AcceptMatchExpr(struct HirVisitor *V, struct HirMatchExpr *e)
{
    AcceptExpr(V, e->target);
    accept_expr_list(V, e->arms);
}

static void AcceptSegment(struct HirVisitor *V, struct HirSegment *seg)
{
    if (seg->types != NULL) {
        for (int j = 0; j < seg->types->count; ++j) {
            AcceptType(V, K_LIST_GET(seg->types, j));
        }
    }
    VISITOR_CALL(V, Segment, seg);
}

static void AcceptPath(struct HirVisitor *V, struct HirPath *path)
{
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment seg = K_LIST_GET(path, i);
        AcceptSegment(V, &seg);
    }
    VISITOR_CALL(V, Path, path);
}

static void AcceptLiteralExpr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitComposite:
            AcceptPath(V, e->comp.path);
            accept_expr_list(V, e->comp.items);
            break;
        case kHirLitContainer:
            accept_expr_list(V, e->cont.items);
            break;
        case kHirLitTuple:
            accept_expr_list(V, e->tuple.elems);
            break;
        case kHirLitBasic:
            break;
    }
}

static void AcceptChainExpr(struct HirVisitor *V, struct HirChainExpr *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptUnOpExpr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptBinOpExpr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptExprStmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    AcceptExpr(V, s->expr);
}

static void AcceptClosureExpr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    accept_decl_list(V, e->params);
    AcceptType(V, e->result);
    AcceptExpr(V, e->expr);
}

static void AcceptFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    if (d->tag != NULL)
        AcceptType(V, d->tag);
}

static void AcceptTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    accept_decl_list(V, d->generics);
    AcceptType(V, d->rhs);
}

static void AcceptGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    if (d->bounds != NULL) {
        struct HirGenericBound const *pbound;
        K_LIST_FOREACH(d->bounds, pbound) {
            AcceptPath(V, pbound->path);
        }
    }
}

static void AcceptVariantDecl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    accept_decl_list(V, d->fields);
}

static void AcceptAdtDecl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    accept_type_list(V, d->traits);
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->fields);
    accept_decl_list(V, d->methods);
}

static void AcceptTraitDecl(struct HirVisitor *V, struct HirTraitDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->methods);
}

static void AcceptVarDecl(struct HirVisitor *V, struct HirVarDecl *d)
{
    if (d->tag != NULL)
        AcceptType(V, d->tag);
    if (d->init != NULL)
        AcceptExpr(V, d->init);
}

static void AcceptReturnExpr(struct HirVisitor *V, struct HirReturnExpr *s)
{
    if (s->expr != NULL)
        AcceptExpr(V, s->expr);
}

static void AcceptCallExpr(struct HirVisitor *V, struct HirCallExpr *e)
{
    AcceptExpr(V, e->target);
    accept_expr_list(V, e->args);
}

static void AcceptConversionExpr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    AcceptExpr(V, e->arg);
}

static void AcceptPathExpr(struct HirVisitor *V, struct HirPathExpr *e)
{
    AcceptPath(V, e->path);
}

static void AcceptFuncDecl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->params);
    if (d->body != NULL)
        AcceptExpr(V, d->body);
}

static void AcceptIfExpr(struct HirVisitor *V, struct HirIfExpr *s)
{
    AcceptExpr(V, s->cond);
    AcceptExpr(V, s->then_arm);
    if (s->else_arm != NULL)
        AcceptExpr(V, s->else_arm);
}

static void AcceptLoopExpr(struct HirVisitor *V, struct HirLoopExpr *s)
{
    AcceptExpr(V, s->block);
}

static void AcceptJumpExpr(struct HirVisitor *V, struct HirJumpExpr *s)
{
    PAW_UNUSED(V);
    PAW_UNUSED(s);
}

static void AcceptIndex(struct HirVisitor *V, struct HirIndex *e)
{
    AcceptExpr(V, e->target);
    if (e->first != NULL)
        AcceptExpr(V, e->first);
    if (e->second != NULL)
        AcceptExpr(V, e->second);
}

static void AcceptSelector(struct HirVisitor *V, struct HirSelector *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptDeclStmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    AcceptDecl(V, s->decl);
}

static void AcceptFuncPtr(struct HirVisitor *V, struct HirFuncPtr *t)
{
    accept_type_list(V, t->params);
    AcceptType(V, t->result);
}

static void AcceptTupleType(struct HirVisitor *V, struct HirTupleType *t)
{
    accept_type_list(V, t->elems);
}

static void AcceptInferType(struct HirVisitor *V, struct HirInferType *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptPathType(struct HirVisitor *V, struct HirPathType *t)
{
    for (int i = 0; i < t->path->count; ++i) {
        struct HirSegment seg = K_LIST_GET(t->path, i);
        accept_type_list(V, seg.types);
    }
}

static void AcceptOrPat(struct HirVisitor *V, struct HirOrPat *e)
{
    accept_pat_list(V, e->pats);
}

static void AcceptFieldPat(struct HirVisitor *V, struct HirFieldPat *p)
{
    AcceptPat(V, p->pat);
}

static void AcceptStructPat(struct HirVisitor *V, struct HirStructPat *p)
{
    AcceptPath(V, p->path);
    accept_pat_list(V, p->fields);
}

static void AcceptVariantPat(struct HirVisitor *V, struct HirVariantPat *p)
{
    AcceptPath(V, p->path);
    accept_pat_list(V, p->fields);
}

static void AcceptTuplePat(struct HirVisitor *V, struct HirTuplePat *p)
{
    accept_pat_list(V, p->elems);
}

static void AcceptBindingPat(struct HirVisitor *V, struct HirBindingPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

static void AcceptPathPat(struct HirVisitor *V, struct HirPathPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

static void AcceptLiteralPat(struct HirVisitor *V, struct HirLiteralPat *p)
{
    AcceptExpr(V, p->expr);
}

static void AcceptWildcardPat(struct HirVisitor *V, struct HirWildcardPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL ? (V)->PostVisit##name(V, x) : (void)0)
#define DEFINE_VISITOR_CASES(X)             \
    case kHir##X: {                         \
        struct Hir##X *x = HirGet##X(node); \
        if (VISITOR_CALL(V, X, x))          \
            Accept##X(V, x);                \
        VISITOR_POSTCALL(V, X, x);          \
    } break;

static void AcceptExpr(struct HirVisitor *V, struct HirExpr *node)
{
    paw_assert(node != NULL);
    if (!V->VisitExpr(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_EXPR_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitExpr(V, node);
}

static void AcceptDecl(struct HirVisitor *V, struct HirDecl *node)
{
    paw_assert(node != NULL);
    if (!V->VisitDecl(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_DECL_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitDecl(V, node);
}

static void AcceptStmt(struct HirVisitor *V, struct HirStmt *node)
{
    paw_assert(node != NULL);
    if (!V->VisitStmt(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_STMT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitStmt(V, node);
}

static void AcceptType(struct HirVisitor *V, struct HirType *node)
{
    paw_assert(node != NULL);
    if (!V->VisitType(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_TYPE_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitType(V, node);
}

static void AcceptPat(struct HirVisitor *V, struct HirPat *node)
{
    paw_assert(node != NULL);
    if (!V->VisitPat(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_PAT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitPat(V, node);
}

#undef DEFINE_VISITOR_CASES
#undef VISITOR_POSTCALL
#undef VISITOR_CALL

static paw_Bool default_visit_expr(struct HirVisitor *V, struct HirExpr *node) { return PAW_TRUE; }
static paw_Bool default_visit_decl(struct HirVisitor *V, struct HirDecl *node) { return PAW_TRUE; }
static paw_Bool default_visit_stmt(struct HirVisitor *V, struct HirStmt *node) { return PAW_TRUE; }
static paw_Bool default_visit_type(struct HirVisitor *V, struct HirType *node) { return PAW_TRUE; }
static paw_Bool default_visit_pat(struct HirVisitor *V, struct HirPat *node) { return PAW_TRUE; }

static void default_post_visit_expr(struct HirVisitor *V, struct HirExpr *node) {}
static void default_post_visit_decl(struct HirVisitor *V, struct HirDecl *node) {}
static void default_post_visit_stmt(struct HirVisitor *V, struct HirStmt *node) {}
static void default_post_visit_type(struct HirVisitor *V, struct HirType *node) {}
static void default_post_visit_pat(struct HirVisitor *V, struct HirPat *node) {}

void pawHir_visitor_init(struct HirVisitor *V, struct Compiler *C, void *ud)
{
    *V = (struct HirVisitor){
        .ud = ud,
        .C = C,

        .VisitExpr = default_visit_expr,
        .VisitDecl = default_visit_decl,
        .VisitStmt = default_visit_stmt,
        .VisitType = default_visit_type,
        .VisitPat = default_visit_pat,

        .PostVisitExpr = default_post_visit_expr,
        .PostVisitDecl = default_post_visit_decl,
        .PostVisitStmt = default_post_visit_stmt,
        .PostVisitType = default_post_visit_type,
        .PostVisitPat = default_post_visit_pat,
    };
}

#define DEFINE_VISITORS(name, T)                                                     \
    void pawHir_visit_##name(struct HirVisitor *V, struct Hir##T *node)              \
    {                                                                                \
        paw_assert(node != NULL);                                                    \
        V->line = node->hdr.line;                                                    \
        Accept##T(V, node);                                                          \
    }                                                                                \
    void pawHir_visit_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    {                                                                                \
        if (list == NULL)                                                            \
            return;                                                                  \
        for (int i = 0; i < list->count; ++i) {                                      \
            pawHir_visit_##name(V, K_LIST_GET(list, i));                             \
        }                                                                            \
    }
DEFINE_VISITORS(expr, Expr)
DEFINE_VISITORS(decl, Decl)
DEFINE_VISITORS(stmt, Stmt)
DEFINE_VISITORS(type, Type)
DEFINE_VISITORS(pat, Pat)
#undef DEFINE_VISITORS


static struct HirType *FoldType(struct HirFolder *F, struct HirType *node);
static struct HirExpr *FoldExpr(struct HirFolder *F, struct HirExpr *node);
static struct HirDecl *FoldDecl(struct HirFolder *F, struct HirDecl *node);
static struct HirStmt *FoldStmt(struct HirFolder *F, struct HirStmt *node);
static struct HirPat *FoldPat(struct HirFolder *F, struct HirPat *node);

#define DEFINE_LIST_FOLDER(name, T)                                                            \
    static struct Hir##T##List *fold_##name##s(struct HirFolder *F, struct Hir##T##List *list) \
    {                                                                                          \
        if (list == NULL)                                                                      \
            return NULL;                                                                       \
        struct Hir##T##List *r = pawHir_##name##_list_new(F->C);                               \
        struct Hir##T *const *pnode;                                                           \
        K_LIST_FOREACH(list, pnode) {                                                          \
            K_LIST_PUSH(F->C, r, Fold##T(F, *pnode));                                          \
        }                                                                                      \
        return r;                                                                              \
    }
DEFINE_LIST_FOLDER(decl, Decl)
DEFINE_LIST_FOLDER(expr, Expr)
DEFINE_LIST_FOLDER(stmt, Stmt)
DEFINE_LIST_FOLDER(type, Type)
DEFINE_LIST_FOLDER(pat, Pat)

static struct HirSegment FoldSegment(struct HirFolder *F, struct HirSegment *seg)
{
    struct HirSegment r;
    struct HirTypeList *types = NULL;
    if (seg->types != NULL) {
        types = pawHir_type_list_new(F->C);
        struct HirType *const *ptype;
        K_LIST_FOREACH(seg->types, ptype) {
            struct HirType *type = FoldType(F, *ptype);
            K_LIST_PUSH(F->C, r.types, type);
        }
    }
    pawHir_init_segment(F->hir, &r, seg->name, types, seg->did);
    return r;
}

static struct HirPath *FoldPath(struct HirFolder *F, struct HirPath *path)
{
    struct HirSegment *pseg;
    struct HirPath *r = pawHir_path_new(F->C);
    K_LIST_FOREACH(path, pseg) {
        K_LIST_PUSH(F->C, r, FoldSegment(F, pseg));
    }
    return r;
}

static struct HirExpr *FoldBlock(struct HirFolder *F, struct HirBlock *e)
{
    struct HirStmtList *stmts = fold_stmts(F, e->stmts);
    struct HirExpr *result = FoldExpr(F, e->result);
    return pawHir_new_block(F->hir, e->line, stmts, result, e->never);
}

static struct HirExpr *FoldLogicalExpr(struct HirFolder *F, struct HirLogicalExpr *e)
{
    struct HirExpr *lhs = FoldExpr(F, e->lhs);
    struct HirExpr *rhs = FoldExpr(F, e->rhs);
    return pawHir_new_logical_expr(F->hir, e->line, lhs, rhs, e->is_and);
}

static struct HirExpr *FoldFieldExpr(struct HirFolder *F, struct HirFieldExpr *e)
{
    struct HirExpr *value = FoldExpr(F, e->value);
    if (e->fid < 0) {
        struct HirExpr *key = FoldExpr(F, e->key);
        return pawHir_new_keyed_field_expr(F->hir, e->line, key, value);
    } else {
        return pawHir_new_named_field_expr(F->hir, e->line, e->name, value, e->fid);
    }
}

static struct HirExpr *FoldAssignExpr(struct HirFolder *F, struct HirAssignExpr *e)
{
    struct HirExpr *lhs = FoldExpr(F, e->lhs);
    struct HirExpr *rhs = FoldExpr(F, e->rhs);
    return pawHir_new_assign_expr(F->hir, e->line, lhs, rhs);
}

static struct HirExpr *FoldMatchArm(struct HirFolder *F, struct HirMatchArm *e)
{
    struct HirPat *pat = FoldPat(F, e->pat);
    struct HirExpr *guard = e->guard != NULL ? FoldExpr(F, e->guard) : NULL;
    struct HirExpr *result = FoldExpr(F, e->result);
    return pawHir_new_match_arm(F->hir, e->line, pat, guard, result, e->never);
}

static struct HirExpr *FoldMatchExpr(struct HirFolder *F, struct HirMatchExpr *e)
{
    struct HirExpr *target = FoldExpr(F, e->target);
    struct HirExprList *arms = fold_exprs(F, e->arms);
    return pawHir_new_match_expr(F->hir, e->line, target, arms, e->never);
}

static struct HirExpr *FoldLiteralExpr(struct HirFolder *F, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitComposite: {
            struct HirPath *path = FoldPath(F, e->comp.path);
            struct HirExprList *items = fold_exprs(F, e->comp.items);
            return pawHir_new_composite_lit(F->hir, e->line, path, items);
        }
        case kHirLitContainer: {
            struct HirExprList *items = fold_exprs(F, e->cont.items);
            return pawHir_new_container_lit(F->hir, e->line, items, e->cont.code);
        }
        case kHirLitTuple: {
            struct HirExprList *elems = fold_exprs(F, e->tuple.elems);
            return pawHir_new_tuple_lit(F->hir, e->line, elems);
        }
        case kHirLitBasic: {
            return pawHir_new_basic_lit(F->hir, e->line, e->basic.value, e->basic.code);
        }
    }
}

static struct HirExpr *FoldChainExpr(struct HirFolder *F, struct HirChainExpr *e)
{
    struct HirExpr *target = FoldExpr(F, e->target);
    return pawHir_new_chain_expr(F->hir, e->line, target);
}

static struct HirExpr *FoldUnOpExpr(struct HirFolder *F, struct HirUnOpExpr *e)
{
    struct HirExpr *target = FoldExpr(F, e->target);
    return pawHir_new_unop_expr(F->hir, e->line, target, e->op);
}

static struct HirExpr *FoldBinOpExpr(struct HirFolder *F, struct HirBinOpExpr *e)
{
    struct HirExpr *lhs = FoldExpr(F, e->lhs);
    struct HirExpr *rhs = FoldExpr(F, e->rhs);
    return pawHir_new_binop_expr(F->hir, e->line, lhs, rhs, e->op);
}

static struct HirExpr *FoldClosureExpr(struct HirFolder *F, struct HirClosureExpr *e)
{
    struct HirDeclList *params = fold_decls(F, e->params);
    struct HirType *result = FoldType(F, e->result);
    struct HirExpr *expr = FoldExpr(F, e->expr);
    return pawHir_new_closure_expr(F->hir, e->line, params, result, expr);
}

static struct HirExpr *FoldReturnExpr(struct HirFolder *F, struct HirReturnExpr *e)
{
    struct HirExpr *expr = e->expr != NULL ? FoldExpr(F, e->expr) : NULL;
    return pawHir_new_return_expr(F->hir, e->line, expr);
}

static struct HirExpr *FoldCallExpr(struct HirFolder *F, struct HirCallExpr *e)
{
    struct HirExpr *target = FoldExpr(F, e->target);
    struct HirExprList *args = fold_exprs(F, e->args);
    return pawHir_new_call_expr(F->hir, e->line, target, args);
}

static struct HirExpr *FoldConversionExpr(struct HirFolder *F, struct HirConversionExpr *e)
{
    struct HirExpr *arg = FoldExpr(F, e->arg);
    return pawHir_new_conversion_expr(F->hir, e->line, arg, e->to);
}

static struct HirExpr *FoldPathExpr(struct HirFolder *F, struct HirPathExpr *e)
{
    struct HirPath *path = FoldPath(F, e->path);
    return pawHir_new_path_expr(F->hir, e->line, path);
}

static struct HirExpr *FoldIfExpr(struct HirFolder *F, struct HirIfExpr *e)
{
    struct HirExpr *cond = FoldExpr(F, e->cond);
    struct HirExpr *then_arm = FoldExpr(F, e->then_arm);
    struct HirExpr *else_arm = e->else_arm != NULL ?  FoldExpr(F, e->else_arm) : NULL;
    return pawHir_new_if_expr(F->hir, e->line, cond, then_arm, else_arm, e->never);
}

static struct HirExpr *FoldLoopExpr(struct HirFolder *F, struct HirLoopExpr *e)
{
    struct HirExpr *block = FoldExpr(F, e->block);
    return pawHir_new_loop_expr(F->hir, e->line, block);
}

static struct HirExpr *FoldJumpExpr(struct HirFolder *F, struct HirJumpExpr *e)
{
    return pawHir_new_jump_expr(F->hir, e->line, e->jump_kind);
}

static struct HirExpr *FoldIndex(struct HirFolder *F, struct HirIndex *e)
{
    struct HirExpr *target = FoldExpr(F, e->target);
    struct HirExpr *first = e->first != NULL ? FoldExpr(F, e->first) : NULL;
    struct HirExpr *second = e->second != NULL ? FoldExpr(F, e->second) : NULL;
    return pawHir_new_index_expr(F->hir, e->line, target, first, second, e->is_slice);
}

static struct HirExpr *FoldSelector(struct HirFolder *F, struct HirSelector *e)
{
    struct HirExpr *target = FoldExpr(F, e->target);
    if (e->is_index) {
        return pawHir_new_name_selector(F->hir, e->line, target, e->name);
    } else {
        return pawHir_new_index_selector(F->hir, e->line, target, e->index);
    }
}

static struct HirDecl *FoldFieldDecl(struct HirFolder *F, struct HirFieldDecl *d)
{
    struct HirType *tag = d->tag != NULL ?  FoldType(F, d->tag) : NULL;
    return pawHir_new_field_decl(F->hir, d->line, d->name, tag, d->is_pub);
}

static struct HirDecl *FoldTypeDecl(struct HirFolder *F, struct HirTypeDecl *d)
{
    struct HirDeclList *generics = fold_decls(F, d->generics);
    struct HirType *rhs = FoldType(F, d->rhs);
    return pawHir_new_type_decl(F->hir, d->line, d->name, generics, rhs, d->is_pub);
}

static struct HirDecl *FoldGenericDecl(struct HirFolder *F, struct HirGenericDecl *d)
{
    struct HirBoundList *bounds = pawHir_bound_list_new(F->C);
    struct HirGenericBound const *pbound;
    K_LIST_FOREACH(d->bounds, pbound) {
        struct HirGenericBound bound = {.path = FoldPath(F, pbound->path)};
        K_LIST_PUSH(F->C, bounds, bound);
    }
    return pawHir_new_generic_decl(F->hir, d->line, d->name, bounds);
}

static struct HirDecl *FoldVariantDecl(struct HirFolder *F, struct HirVariantDecl *d)
{
    struct HirDeclList *fields = fold_decls(F, d->fields);
    return pawHir_new_variant_decl(F->hir, d->line, d->name, fields, d->index);
}

static struct HirDecl *FoldAdtDecl(struct HirFolder *F, struct HirAdtDecl *d)
{
    struct HirDecl *self = FoldDecl(F, d->self);
    struct HirTypeList *traits = fold_types(F, d->traits);
    struct HirDeclList *generics = fold_decls(F, d->generics);
    struct HirDeclList *fields = fold_decls(F, d->fields);
    struct HirDeclList *methods = fold_decls(F, d->methods);
    return pawHir_new_adt_decl(F->hir, d->line, d->name, self, traits,
            generics, fields, methods, d->is_pub, d->is_struct);
}

static struct HirDecl *FoldTraitDecl(struct HirFolder *F, struct HirTraitDecl *d)
{
    struct HirDecl *self = FoldDecl(F, d->self);
    struct HirDeclList *generics = fold_decls(F, d->generics);
    struct HirDeclList *methods = fold_decls(F, d->methods);
    return pawHir_new_trait_decl(F->hir, d->line, d->name, self, generics, methods, d->is_pub);
}

static struct HirDecl *FoldFuncDecl(struct HirFolder *F, struct HirFuncDecl *d)
{
    struct HirDeclList *generics = fold_decls(F, d->generics);
    struct HirDeclList *params = fold_decls(F, d->params);
    struct HirType *result = FoldType(F, d->result);
    struct HirExpr *body = d->body != NULL ? FoldExpr(F, d->body) : NULL;
    return pawHir_new_func_decl(F->hir, d->line, d->name, generics,
            params, result, body, d->fn_kind, d->is_pub, d->is_assoc);
}

static struct HirDecl *FoldVarDecl(struct HirFolder *F, struct HirVarDecl *d)
{
    struct HirType *tag = d->tag != NULL ? FoldType(F, d->tag) : NULL;
    struct HirExpr *init = d->init != NULL ? FoldExpr(F, d->init) : NULL;
    return pawHir_new_var_decl(F->hir, d->line, d->name, tag, init);
}

static struct HirStmt *FoldExprStmt(struct HirFolder *F, struct HirExprStmt *s)
{
    struct HirExpr *expr = FoldExpr(F, s->expr);
    return pawHir_new_expr_stmt(F->hir, s->line, expr);
}

static struct HirStmt *FoldDeclStmt(struct HirFolder *F, struct HirDeclStmt *s)
{
    struct HirDecl *decl = FoldDecl(F, s->decl);
    return pawHir_new_decl_stmt(F->hir, s->line, decl);
}

static struct HirType *FoldFuncPtr(struct HirFolder *F, struct HirFuncPtr *t)
{
    struct HirTypeList *params = fold_types(F, t->params);
    struct HirType *result = FoldType(F, t->result);
    return pawHir_new_func_ptr(F->hir, t->line, params, result);
}

static struct HirType *FoldTupleType(struct HirFolder *F, struct HirTupleType *t)
{
    struct HirTypeList *elems = fold_types(F, t->elems);
    return pawHir_new_tuple_type(F->hir, t->line, elems);
}

static struct HirType *FoldInferType(struct HirFolder *F, struct HirInferType *t)
{
    return pawHir_new_infer_type(F->hir, t->line);
}

static struct HirType *FoldPathType(struct HirFolder *F, struct HirPathType *t)
{
    struct HirSegment *pseg;
    struct HirPath *path = pawHir_path_new(F->C);
    K_LIST_FOREACH(t->path, pseg)
        K_LIST_PUSH(F->C, path, F->FoldSegment(F, pseg));
    return pawHir_new_path_type(F->hir, t->line, path);
}

static struct HirPat *FoldOrPat(struct HirFolder *F, struct HirOrPat *e)
{
    struct HirPatList *pats = fold_pats(F, e->pats);
    return pawHir_new_or_pat(F->hir, e->line, pats);
}

static struct HirPat *FoldFieldPat(struct HirFolder *F, struct HirFieldPat *p)
{
    struct HirPat *pat = FoldPat(F, p->pat);
    return pawHir_new_field_pat(F->hir, p->line, p->name, pat, p->index);
}

static struct HirPat *FoldStructPat(struct HirFolder *F, struct HirStructPat *p)
{
    struct HirPath *path = FoldPath(F, p->path);
    struct HirPatList *fields = fold_pats(F, p->fields);
    return pawHir_new_struct_pat(F->hir, p->line, path, fields);
}

static struct HirPat *FoldVariantPat(struct HirFolder *F, struct HirVariantPat *p)
{
    struct HirPath *path = FoldPath(F, p->path);
    struct HirPatList *fields = fold_pats(F, p->fields);
    return pawHir_new_variant_pat(F->hir, p->line, path, fields, p->index);
}

static struct HirPat *FoldTuplePat(struct HirFolder *F, struct HirTuplePat *p)
{
    struct HirPatList *elems = fold_pats(F, p->elems);
    return pawHir_new_tuple_pat(F->hir, p->line, elems);
}

static struct HirPat *FoldBindingPat(struct HirFolder *F, struct HirBindingPat *p)
{
    return pawHir_new_binding_pat(F->hir, p->line, p->name);
}

static struct HirPat *FoldPathPat(struct HirFolder *F, struct HirPathPat *p)
{
    struct HirPath *path = FoldPath(F, p->path);
    return pawHir_new_path_pat(F->hir, p->line, path);
}

static struct HirPat *FoldLiteralPat(struct HirFolder *F, struct HirLiteralPat *p)
{
    struct HirExpr *expr = FoldExpr(F, p->expr);
    return pawHir_new_literal_pat(F->hir, p->line, expr);
}

static struct HirPat *FoldWildcardPat(struct HirFolder *F, struct HirWildcardPat *p)
{
    return pawHir_new_wildcard_pat(F->hir, p->line);
}

#define DEFINE_FOLDER_CASES(X)                 \
    case kHir##X: {                            \
        return F->Fold##X(F, HirGet##X(node)); \
    }

static struct HirExpr *FoldExpr(struct HirFolder *F, struct HirExpr *node)
{
    paw_assert(node != NULL);
    switch (HIR_KINDOF(node)) {
        HIR_EXPR_LIST(DEFINE_FOLDER_CASES)
    }
}

static struct HirDecl *FoldDecl(struct HirFolder *F, struct HirDecl *node)
{
    paw_assert(node != NULL);
    switch (HIR_KINDOF(node)) {
        HIR_DECL_LIST(DEFINE_FOLDER_CASES)
    }
}

static struct HirStmt *FoldStmt(struct HirFolder *F, struct HirStmt *node)
{
    paw_assert(node != NULL);
    switch (HIR_KINDOF(node)) {
        HIR_STMT_LIST(DEFINE_FOLDER_CASES)
    }
}

static struct HirType *FoldType(struct HirFolder *F, struct HirType *node)
{
    paw_assert(node != NULL);
    switch (HIR_KINDOF(node)) {
        HIR_TYPE_LIST(DEFINE_FOLDER_CASES)
    }
}

static struct HirPat *FoldPat(struct HirFolder *F, struct HirPat *node)
{
    paw_assert(node != NULL);
    switch (HIR_KINDOF(node)) {
        HIR_PAT_LIST(DEFINE_FOLDER_CASES)
    }
}

#undef DEFINE_FOLDER_CASES

void pawHir_folder_init(struct HirFolder *F, struct Hir *hir, void *ud)
{
    *F = (struct HirFolder){
        .ud = ud,
        .C = hir->C,
        .hir = hir,

        .FoldExpr = FoldExpr,
        .FoldDecl = FoldDecl,
        .FoldStmt = FoldStmt,
        .FoldType = FoldType,
        .FoldPat = FoldPat,

        .FoldSegment = FoldSegment,
        .FoldPath = FoldPath,

#define SET_DEFAULT_FOLDERS(X) .Fold##X = Fold##X,
        HIR_STMT_LIST(SET_DEFAULT_FOLDERS)
        HIR_EXPR_LIST(SET_DEFAULT_FOLDERS)
        HIR_DECL_LIST(SET_DEFAULT_FOLDERS)
        HIR_TYPE_LIST(SET_DEFAULT_FOLDERS)
        HIR_PAT_LIST(SET_DEFAULT_FOLDERS)
#undef SET_DEFAULT_FOLDERS
    };
}

#define DEFINE_FOLDERS(name, T)                                                 \
    struct Hir##T *pawHir_fold_##name(struct HirFolder *F, struct Hir##T *node) \
    {                                                                           \
        paw_assert(node != NULL);                                               \
        F->line = node->hdr.line;                                               \
        return F->Fold##T(F, node);                                             \
    }                                                                           \
    struct Hir##T##List *pawHir_fold_##name##s(struct HirFolder *F, struct Hir##T##List *list) \
    {                                                                                          \
        if (list == NULL) return NULL;                                                         \
        struct Hir##T *const *pnode;                                                           \
        struct Hir##T##List *r = pawHir_##name##_list_new(F->C);                               \
        K_LIST_FOREACH(list, pnode) {                                                          \
            K_LIST_PUSH(F->C, r, pawHir_fold_##name(F, *pnode));                               \
        }                                                                                      \
        return r;                                                                              \
    }
DEFINE_FOLDERS(expr, Expr)
DEFINE_FOLDERS(decl, Decl)
DEFINE_FOLDERS(stmt, Stmt)
DEFINE_FOLDERS(type, Type)
DEFINE_FOLDERS(pat, Pat)
#undef DEFINE_FOLDERS


struct IrTypeList *pawHir_collect_decl_types(struct Compiler *C, struct HirDeclList *list)
{
    if (list == NULL)
        return NULL;
    struct IrTypeList *types = pawIr_type_list_new(C);
    for (int i = 0; i < list->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(list, i);
        K_LIST_PUSH(C, types, GET_NODE_TYPE(C, decl));
    }
    return types;
}

enum TraitKind pawHir_kindof_trait(struct Compiler *C, struct HirTraitDecl *d)
{
    if (pawS_eq(d->name, CSTR(C, CSTR_HASH))) {
        return TRAIT_HASH;
    } else if (pawS_eq(d->name, CSTR(C, CSTR_EQUALS))) {
        return TRAIT_EQUALS;
    } else {
        return TRAIT_USER;
    }
}

paw_Bool pawHir_is_pub_decl(struct HirDecl *decl)
{
    switch (HIR_KINDOF(decl)) {
        case kHirTraitDecl:
            return HirGetTraitDecl(decl)->is_pub;
            break;
        case kHirAdtDecl:
            return HirGetAdtDecl(decl)->is_pub;
            break;
        case kHirFuncDecl:
            return HirGetFuncDecl(decl)->is_pub;
            break;
        case kHirTypeDecl:
            return HirGetTypeDecl(decl)->is_pub;
        case kHirVarDecl:
            return HirGetVarDecl(decl)->is_pub;
            break;
        case kHirFieldDecl:
        case kHirGenericDecl:
        case kHirVariantDecl:
            PAW_UNREACHABLE();
    }
}

static paw_Bool const_check_literal(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    if (e->lit_kind != kHirLitBasic) {
        VALUE_ERROR(V->C, e->line, "constant literal must be primitive type");
    }
    return PAW_TRUE;
}

static paw_Bool const_check_path(struct HirVisitor *V, struct HirPathExpr *e)
{
    return PAW_TRUE;
}

static paw_Bool const_check_chain(struct HirVisitor *V, struct HirChainExpr *e)
{
    VALUE_ERROR(V->C, e->line, "'?' outside function body");
}

static paw_Bool const_check_unop(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    return PAW_TRUE;
}

static paw_Bool const_check_binop(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    return PAW_TRUE;
}

static paw_Bool const_check_closure(struct HirVisitor *V, struct HirClosureExpr *e)
{
    VALUE_ERROR(V->C, e->line, "closures cannot be constant evaluated");
}

static paw_Bool const_check_call(struct HirVisitor *V, struct HirCallExpr *e)
{
    VALUE_ERROR(V->C, e->line, "function calls cannot be constant evaluated");
}

static paw_Bool const_check_index(struct HirVisitor *V, struct HirIndex *e)
{
    VALUE_ERROR(V->C, e->line, "index expressions cannot be constant evaluated");
}

static paw_Bool const_check_selector(struct HirVisitor *V, struct HirSelector *e)
{
    VALUE_ERROR(V->C, e->line, "selector expressions cannot be constant evaluated");
}

static paw_Bool const_check_field(struct HirVisitor *V, struct HirFieldExpr *e)
{
    VALUE_ERROR(V->C, e->line, "fields cannot be constant evaluated");
}

static paw_Bool const_check_loop(struct HirVisitor *V, struct HirLoopExpr *e)
{
    VALUE_ERROR(V->C, e->line, "loops cannot be constant evaluated");
}

static paw_Bool const_check_jump(struct HirVisitor *V, struct HirJumpExpr *e)
{
    VALUE_ERROR(V->C, e->line, "'%s' outside loop body",
            e->jump_kind == JUMP_BREAK ? "break" : "continue");
    return PAW_TRUE;
}

static paw_Bool const_check_return(struct HirVisitor *V, struct HirReturnExpr *e)
{
    VALUE_ERROR(V->C, e->line, "'return' outside function body");
    return PAW_TRUE;
}

paw_Bool pawHir_check_const(struct Hir *hir, struct HirExpr *expr)
{
    struct HirVisitor V;
    paw_Bool is_const = PAW_TRUE;
    pawHir_visitor_init(&V, hir->C, &is_const);

    V.VisitLiteralExpr = const_check_literal;
    V.VisitPathExpr = const_check_path;
    V.VisitChainExpr = const_check_chain;
    V.VisitUnOpExpr = const_check_unop;
    V.VisitBinOpExpr = const_check_binop;
    V.VisitClosureExpr = const_check_closure;
    V.VisitCallExpr = const_check_call;
    V.VisitIndex = const_check_index;
    V.VisitSelector = const_check_selector;
    V.VisitFieldExpr = const_check_field;
    V.VisitLoopExpr = const_check_loop;
    V.VisitJumpExpr = const_check_jump;
    V.VisitReturnExpr = const_check_return;

    pawHir_visit_expr(&V, expr);
    return is_const;
}


struct Printer {
    struct Compiler *C;
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
        if (i < list->count - 1)
            PRINT_LITERAL(P, ", ");
    }
}

static void print_path(struct Printer *P, struct HirPath *path)
{
    for (int i = 0; i < path->count; ++i) {
        if (i > 0)
            PRINT_LITERAL(P, "::");
        struct HirSegment seg = K_LIST_GET(path, i);
        PRINT_STRING(P, seg.name);
        if (seg.types != NULL) {
            PRINT_CHAR(P, '<');
            print_type_list(P, seg.types);
            PRINT_CHAR(P, '>');
        }
    }
}

static void print_type(struct Printer *P, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirTupleType: {
            struct HirTupleType *tup = HirGetTupleType(type);
            PRINT_CHAR(P, '(');
            print_type_list(P, tup->elems);
            if (tup->elems->count == 1)
                PRINT_CHAR(P, ',');
            PRINT_CHAR(P, ')');
            break;
        }
        case kHirInferType:
            PRINT_CHAR(P, '_');
            break;
        case kHirFuncPtr: {
            struct HirFuncPtr *fptr = HirGetFuncPtr(type);
            PRINT_LITERAL(P, "fn(");
            print_type_list(P, fptr->params);
            PRINT_CHAR(P, ')');
            if (pawP_type2code(P->C, GET_NODE_TYPE(P->C, fptr->result)) != BUILTIN_UNIT) {
                PRINT_LITERAL(P, " -> ");
                print_type(P, fptr->result);
            }
            break;
        }
        case kHirPathType: {
            struct HirPathType *path = HirGetPathType(type);
            print_path(P, path->path);
        }
    }
}

char const *pawHir_print_type(struct Compiler *C, struct HirType *type)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_type(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
        type);

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

char const *pawHir_print_path(struct Compiler *C, struct HirPath *path)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_path(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
        path);

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
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

#define DEFINE_KIND_PRINTER(name, T)                                   \
    static int print_##name##_kind(struct Printer *P, void *node)      \
    {                                                                  \
        if (node != NULL) {                                            \
            struct T *typed = node;                                    \
            DUMP_FSTRING(P, "%s {\n", k##T##Names[HIR_KINDOF(typed)]); \
            return 0;                                                  \
        }                                                              \
        DUMP_LITERAL(P, "(null)\n");                                   \
        return -1;                                                     \
    }
DEFINE_KIND_PRINTER(expr, HirExpr)
DEFINE_KIND_PRINTER(decl, HirDecl)
DEFINE_KIND_PRINTER(stmt, HirStmt)
DEFINE_KIND_PRINTER(type, HirType)
DEFINE_KIND_PRINTER(pat, HirPat)

#define DUMP_NAME(P, s) DUMP_FMT(P, "name: %s\n", s ? s->text : "(null)")

static void dump_expr(struct Printer *, struct HirExpr *);
static void dump_decl(struct Printer *, struct HirDecl *);
static void dump_stmt(struct Printer *, struct HirStmt *);
static void dump_type(struct Printer *, struct HirType *);
static void dump_pat(struct Printer *, struct HirPat *);

#define DEFINE_LIST_PRINTER(name, T)                                                          \
    static void dump_##name##_list(struct Printer *P, struct T##List *list, const char *name) \
    {                                                                                         \
        DUMP_FMT(P, "%s: " #T "List {\n", name);                                              \
        ++P->indent;                                                                          \
        if (list != NULL) {                                                                   \
            for (int i = 0; i < list->count; ++i) {                                           \
                DUMP_MSG(P, "" /* indent */);                                                 \
                dump_##name(P, list->data[i]);                                                \
            }                                                                                 \
        }                                                                                     \
        --P->indent;                                                                          \
        DUMP_MSG(P, "}\n");                                                                   \
    }
DEFINE_LIST_PRINTER(expr, HirExpr)
DEFINE_LIST_PRINTER(decl, HirDecl)
DEFINE_LIST_PRINTER(stmt, HirStmt)
DEFINE_LIST_PRINTER(type, HirType)
DEFINE_LIST_PRINTER(pat, HirPat)

static void dump_segment(struct Printer *P, struct HirSegment seg)
{
    DUMP_STRING(P, seg.name->text);
    if (seg.types != NULL) {
        DUMP_LITERAL(P, "<");
        for (int j = 0; j < seg.types->count; ++j) {
            print_type(P, seg.types->data[j]);
            if (j < seg.types->count - 1) {
                DUMP_LITERAL(P, ", ");
            }
        }
        DUMP_LITERAL(P, ">");
    }
}

static void dump_path(struct Printer *P, struct HirPath *p, char const *name)
{
    DUMP_FMT(P, "%s: ", name);
    for (int i = 0; i < p->count; ++i) {
        if (i != 0)
            DUMP_LITERAL(P, "::");
        dump_segment(P, p->data[i]);
    }
    pawL_add_char(P->P, P->buf, '\n');
}

static void dump_type(struct Printer *P, struct HirType *type)
{
    if (print_type_kind(P, type)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", type->hdr.line);
    switch (HIR_KINDOF(type)) {
        case kHirTupleType: {
            struct HirTupleType *t = HirGetTupleType(type);
            dump_type_list(P, t->elems, "elems");
            break;
        }
        case kHirFuncPtr: {
            struct HirFuncPtr *t = HirGetFuncPtr(type);
            dump_type_list(P, t->params, "params");
            DUMP_MSG(P, "result: ");
            dump_type(P, t->result);
            break;
        }
        case kHirInferType:
            break;
        case kHirPathType: {
            struct HirPathType *t = HirGetPathType(type);
            for (int i = 0; i < t->path->count; ++i) {
                struct HirSegment seg = K_LIST_GET(t->path, i);
                DUMP_FMT(P, "name: %s\n", seg.name->text);
                DUMP_FMT(P, "did: %d\n", seg.did.value);
                if (seg.types != NULL) {
                    dump_type_list(P, seg.types, "types");
                }
            }
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_decl(struct Printer *P, struct HirDecl *decl)
{
    if (print_decl_kind(P, decl)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "did: DeclId(%d, mod=%d)\n",
        decl->hdr.did.value, decl->hdr.did.modno);
    DUMP_FMT(P, "line: %d\n", decl->hdr.line);
    switch (HIR_KINDOF(decl)) {
        case kHirFuncDecl: {
            struct HirFuncDecl *d = HirGetFuncDecl(decl);
            DUMP_FMT(P, "name: %s\n", d->name->text);
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->params, "params");
            DUMP_MSG(P, "body: ");
            dump_expr(P, d->body);
            break;
        }
        case kHirFieldDecl: {
            struct HirFieldDecl *d = HirGetFieldDecl(decl);
            DUMP_NAME(P, d->name);
            break;
        }
        case kHirVarDecl: {
            struct HirVarDecl *d = HirGetVarDecl(decl);
            DUMP_NAME(P, d->name);
            DUMP_MSG(P, "init: ");
            dump_expr(P, d->init);
            break;
        }
        case kHirVariantDecl: {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            DUMP_NAME(P, d->name);
            dump_decl_list(P, d->fields, "fields");
            break;
        }
        case kHirAdtDecl: {
            struct HirAdtDecl *d = HirGetAdtDecl(decl);
            DUMP_NAME(P, d->name);
            DUMP_FMT(P, "is_struct: %d\n", d->is_struct);
            dump_type_list(P, d->traits, "traits");
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->fields, "fields");
            dump_decl_list(P, d->methods, "methods");
            break;
        }
        case kHirTraitDecl: {
            struct HirTraitDecl *d = HirGetTraitDecl(decl);
            DUMP_NAME(P, d->name);
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->methods, "methods");
            break;
        }
        case kHirGenericDecl: {
            struct HirGenericDecl *d = HirGetGenericDecl(decl);
            DUMP_NAME(P, d->name);
            break;
        }
        case kHirTypeDecl: {
            struct HirTypeDecl *d = HirGetTypeDecl(decl);
            DUMP_NAME(P, d->name);
            DUMP_MSG(P, "rhs: ");
            dump_type(P, d->rhs);
            dump_decl_list(P, d->generics, "generics");
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_stmt(struct Printer *P, struct HirStmt *stmt)
{
    if (print_stmt_kind(P, stmt)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", stmt->hdr.line);
    switch (HIR_KINDOF(stmt)) {
        case kHirExprStmt: {
            struct HirExprStmt *s = HirGetExprStmt(stmt);
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->expr);
            break;
        }
        case kHirDeclStmt: {
            struct HirDeclStmt *s = HirGetDeclStmt(stmt);
            DUMP_MSG(P, "decl: ");
            dump_decl(P, s->decl);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_pat(struct Printer *P, struct HirPat *pat)
{
    if (print_pat_kind(P, pat)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", pat->hdr.line);
    switch (HIR_KINDOF(pat)) {
        case kHirBindingPat: {
            struct HirBindingPat *p = HirGetBindingPat(pat);
            DUMP_MSG(P, "name: ");
            PRINT_STRING(P, p->name);
            break;
        }
        case kHirOrPat: {
            struct HirOrPat *p = HirGetOrPat(pat);
            dump_pat_list(P, p->pats, "pats");
            break;
        }
        case kHirFieldPat: {
            struct HirFieldPat *p = HirGetFieldPat(pat);
            DUMP_FMT(P, "target: %s", p->name->text);
            DUMP_MSG(P, "pat: ");
            dump_pat(P, p->pat);
            break;
        }
        case kHirStructPat: {
            struct HirStructPat *p = HirGetStructPat(pat);
            dump_path(P, p->path, "path");
            dump_pat_list(P, p->fields, "fields");
            break;
        }
        case kHirVariantPat: {
            struct HirVariantPat *p = HirGetVariantPat(pat);
            DUMP_FMT(P, "index: %d", p->index);
            dump_path(P, p->path, "path");
            dump_pat_list(P, p->fields, "fields");
            break;
        }
        case kHirTuplePat: {
            struct HirTuplePat *p = HirGetTuplePat(pat);
            dump_pat_list(P, p->elems, "elems");
            break;
        }
        case kHirPathPat: {
            struct HirPathPat *p = HirGetPathPat(pat);
            dump_path(P, p->path, "path");
            break;
        }
        case kHirWildcardPat: {
            struct HirWildcardPat *p = HirGetWildcardPat(pat);
            break;
        }
        case kHirLiteralPat: {
            struct HirLiteralPat *p = HirGetLiteralPat(pat);
            DUMP_MSG(P, "expr: ");
            dump_expr(P, p->expr);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_expr(struct Printer *P, struct HirExpr *expr)
{
    if (print_expr_kind(P, expr)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", expr->hdr.line);
    switch (HIR_KINDOF(expr)) {
        case kHirLiteralExpr: {
            struct HirLiteralExpr *e = HirGetLiteralExpr(expr);
            switch (e->lit_kind) {
                case kHirLitBasic:
                    DUMP_MSG(P, "lit_kind: BASIC\n");
                    switch (e->basic.code) {
                        case BUILTIN_UNIT:
                            break;
                        case BUILTIN_BOOL:
                            DUMP_FMT(P, "value: %s\n",
                                V_TRUE(e->basic.value) ? "true"
                                                       : "false");
                            break;
                        case BUILTIN_INT:
                            DUMP_FMT(P, "value: %I\n",
                                V_INT(e->basic.value));
                            break;
                        case BUILTIN_FLOAT:
                            DUMP_FMT(P, "value: %f\n",
                                V_FLOAT(e->basic.value));
                            break;
                        default:
                            paw_assert(e->basic.code == BUILTIN_STR);
                            DUMP_FMT(P, "value: %s\n",
                                V_STRING(e->basic.value)->text);
                            break;
                    }
                    break;
                case kHirLitTuple:
                    DUMP_MSG(P, "lit_kind: TUPLE\n");
                    dump_expr_list(P, e->tuple.elems, "elems");
                    break;
                case kHirLitContainer:
                    DUMP_MSG(P, "lit_kind: CONTAINER\n");
                    dump_expr_list(P, e->cont.items, "items");
                    break;
                case kHirLitComposite:
                    DUMP_MSG(P, "lit_kind: COMPOSITE\n");
                    dump_path(P, e->comp.path, "target");
                    pawL_add_char(P->P, P->buf, '\n');
                    dump_expr_list(P, e->comp.items, "items");
                    break;
            }
            break;
        }
        case kHirChainExpr: {
            struct HirChainExpr *e = HirGetChainExpr(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            break;
        }
        case kHirLogicalExpr: {
            struct HirLogicalExpr *e = HirGetLogicalExpr(expr);
            DUMP_FMT(P, "is_and: %d\n", e->is_and);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirClosureExpr: {
            struct HirClosureExpr *e = HirGetClosureExpr(expr);
            dump_decl_list(P, e->params, "params");
            dump_expr(P, e->expr);
            break;
        }
        case kHirPathExpr: {
            struct HirPathExpr *e = HirGetPathExpr(expr);
            dump_path(P, e->path, "path");
            pawL_add_char(P->P, P->buf, '\n');
            break;
        }
        case kHirConversionExpr: {
            struct HirConversionExpr *e = HirGetConversionExpr(expr);
            DUMP_FMT(P, "to: %d\n", e->to);
            DUMP_MSG(P, "arg: ");
            dump_expr(P, e->arg);
            break;
        }
        case kHirUnOpExpr: {
            struct HirUnOpExpr *e = HirGetUnOpExpr(expr);
            DUMP_FMT(P, "op: %s\n", paw_unop_name(e->op));
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            break;
        }
        case kHirBinOpExpr: {
            struct HirBinOpExpr *e = HirGetBinOpExpr(expr);
            DUMP_FMT(P, "op: %s\n", paw_binop_name(e->op));
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirCallExpr: {
            struct HirCallExpr *e = HirGetCallExpr(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            dump_expr_list(P, e->args, "args");
            break;
        }
        case kHirIndex: {
            struct HirIndex *e = HirGetIndex(expr);
            DUMP_FMT(P, "is_slice: %d\n", e->is_slice);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            DUMP_MSG(P, "first: ");
            dump_expr(P, e->first);
            DUMP_MSG(P, "second: ");
            dump_expr(P, e->second);
            break;
        }
        case kHirSelector: {
            struct HirSelector *e = HirGetSelector(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            if (e->is_index) {
                DUMP_FMT(P, "index: %I\n", e->index);
            } else {
                DUMP_NAME(P, e->name);
            }
            break;
        }
        case kHirFieldExpr: {
            struct HirFieldExpr *e = HirGetFieldExpr(expr);
            if (e->fid >= 0) {
                DUMP_NAME(P, e->name);
            } else {
                DUMP_MSG(P, "key: ");
                dump_expr(P, e->key);
            }
            DUMP_MSG(P, "value: ");
            dump_expr(P, e->value);
            break;
        }
        case kHirAssignExpr: {
            struct HirAssignExpr *e = HirGetAssignExpr(expr);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirMatchArm: {
            struct HirMatchArm *e = HirGetMatchArm(expr);
            DUMP_MSG(P, "pat: ");
            dump_pat(P, e->pat);
            DUMP_MSG(P, "result: ");
            dump_expr(P, e->result);
            break;
        }
        case kHirMatchExpr: {
            struct HirMatchExpr *e = HirGetMatchExpr(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            dump_expr_list(P, e->arms, "arms");
            break;
        }
        case kHirIfExpr: {
            struct HirIfExpr *e = HirGetIfExpr(expr);
            DUMP_MSG(P, "cond: ");
            dump_expr(P, e->cond);
            DUMP_MSG(P, "then_arm: ");
            dump_expr(P, e->then_arm);
            DUMP_MSG(P, "else_arm: ");
            dump_expr(P, e->else_arm);
            break;
        }
        case kHirLoopExpr: {
            struct HirLoopExpr *e = HirGetLoopExpr(expr);
            DUMP_MSG(P, "block: ");
            dump_expr(P, e->block);
            break;
        }
        case kHirReturnExpr: {
            struct HirReturnExpr *e = HirGetReturnExpr(expr);
            DUMP_MSG(P, "expr: ");
            dump_expr(P, e->expr);
            break;
        }
        case kHirJumpExpr: {
            struct HirJumpExpr *e = HirGetJumpExpr(expr);
            DUMP_FMT(P, "jump_kind: %s\n", e->jump_kind == JUMP_BREAK ? "BREAK" : "CONTINUE");
            break;
        }
        case kHirBlock: {
            struct HirBlock *e = HirGetBlock(expr);
            dump_stmt_list(P, e->stmts, "stmts");
            dump_expr(P, e->result);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

void pawHir_dump_path(struct Compiler *C, struct HirPath *path)
{
    Buffer buf;
    pawL_init_buffer(ENV(C), &buf);
    struct Printer P = {
        .P = ENV(C),
        .buf = &buf,
        .C = C,
    };
    for (int i = 0; i < path->count; ++i) {
        if (i != 0)
            DUMP_LITERAL(&P, "::");
        dump_segment(&P, path->data[i]);
    }
    pawL_push_result(ENV(C), &buf);
}

void pawHir_dump(struct Hir *hir)
{
    Buffer buf;
    paw_Env *P = ENV(hir);
    pawL_init_buffer(P, &buf);
    struct Printer print = {
        .buf = &buf,
        .C = hir->C,
        .P = P,
    };
    for (int i = 0; i < hir->items->count; ++i) {
        dump_decl(&print, hir->items->data[i]);
    }
    pawL_push_result(P, &buf);
}

void pawHir_dump_decls(struct Compiler *C, struct HirDeclList *decls)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);
    pawL_add_fstring(P, &buf, "did\tname\ttype\n", P->modname->text);
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = pawHir_get_decl(C, (DeclId){.value = i});
        char const *name = decl->hdr.name != NULL
                               ? decl->hdr.name->text
                               : "(null)";
        // TODO: broken, maybe just remove the whole thing
        pawL_add_fstring(P, &buf, "%d\t%s\t%s\n", i, name, paw_string(P, -1));
        paw_pop(P, 1);
    }
    pawL_add_char(P, &buf, '\0');
    pawL_push_result(P, &buf);
}

#endif
