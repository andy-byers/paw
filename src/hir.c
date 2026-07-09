// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "hir.h"
#include "compile.h"
#include "debug.h"
#include "ir_type.h"
#include "map.h"
#include "mem.h"
#include "type_folder.h"

#define NEW_NODE(C, T) P_ALLOC(C, NULL, 0, sizeof(T))

struct Hir *pawHir_new(struct Compiler *C)
{
    struct Hir *hir = NEW_NODE(C, struct Hir);
    *hir = (struct Hir){
        .pool = C->hir_pool,
        .P = ENV(C),
        .C = C,
    };
    hir->modules = HirModuleList_new(hir);
    hir->decls = HirDeclMap_new(hir);
    hir->nodes = HirNodeMap_new(hir);
    return hir;
}

void pawHir_free(struct Hir *hir)
{
    P_ALLOC(hir->C, hir, sizeof(*hir), 0);
}

#define DEFINE_NODE_CONSTRUCTOR(name, T)         \
    struct T *pawHir_new_##name(struct Hir *hir) \
    {                                            \
        if (hir->C->hir_count == INT_MAX)        \
            pawM_error(ENV(hir));                \
        return NEW_NODE(hir->C, struct T);       \
    }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(type, HirType)
DEFINE_NODE_CONSTRUCTOR(pat, HirPat)
#undef DEFINE_NODE_CONSTRUCTOR

void pawHir_init_segment(struct Hir *hir, struct HirSegment *r, NodeId id, struct HirIdent ident, struct HirGenericArgs *args, NodeId target)
{
    PAW_UNUSED(hir);
    *r = (struct HirSegment){
        .id = id,
        .target = target,
        .ident = ident,
        .args = args,
    };
}

void pawHir_register_node(struct Hir *hir, NodeId id, void *node)
{
    paw_Bool const replaced = HirNodeMap_insert(hir, hir->nodes, id, node);
    paw_assert(!replaced); PAW_UNUSED(replaced);
}

void pawHir_register_decl(struct Hir *hir, DeclId did, struct HirDecl *decl)
{
    paw_Bool const replaced = HirDeclMap_insert(hir, hir->decls, did, decl);
    paw_assert(!replaced); PAW_UNUSED(replaced);
}

void *pawHir_get_node(struct Hir *hir, NodeId id)
{
    return *HirNodeMap_get(hir, hir->nodes, id);
}

struct HirDecl *pawHir_get_decl(struct Hir *hir, DeclId did)
{
    return *HirDeclMap_get(hir, hir->decls, did);
}


#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL \
        ? (V)->Visit##name(V, x) : 1)
#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL \
        ? (V)->PostVisit##name(V, x) : (void)0)

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
        struct Hir##T *const *pnode;                                                  \
        K_LIST_FOREACH(list, pnode) {                                                 \
            Accept##T(V, *pnode);                                                     \
        }                                                                             \
    }
DEFINE_LIST_ACCEPTOR(decl, Decl)
DEFINE_LIST_ACCEPTOR(expr, Expr)
DEFINE_LIST_ACCEPTOR(stmt, Stmt)
DEFINE_LIST_ACCEPTOR(type, Type)
DEFINE_LIST_ACCEPTOR(pat, Pat)

static void accept_generic_arg(struct HirVisitor *V, struct HirGenericArg arg)
{
    if (arg.is_type) {
        AcceptType(V, arg.t);
    } else {
        AcceptExpr(V, arg.k);
    }
}

static void accept_generic_args(struct HirVisitor *V, struct HirGenericArgs *args)
{
    K_LIST_XFOREACH (args, struct HirGenericArg const, p)
        accept_generic_arg(V, *p);
}

static void AcceptBlock(struct HirVisitor *V, struct HirBlock *e)
{
    accept_stmt_list(V, e->stmts);
    if (e->result != NULL) AcceptExpr(V, e->result);
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

static void AcceptOpAssignExpr(struct HirVisitor *V, struct HirOpAssignExpr *e)
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
    if (VISITOR_CALL(V, Segment, seg)) {
        if (seg->args != NULL)
            accept_generic_args(V, seg->args);
        VISITOR_POSTCALL(V, Segment, seg);
    }
}

static void AcceptPath(struct HirVisitor *V, struct HirPath *path)
{
    if (VISITOR_CALL(V, Path, path)) {
        struct HirSegment *pseg;
        K_LIST_FOREACH (path->segments, pseg) {
            AcceptSegment(V, pseg);
        }
        VISITOR_POSTCALL(V, Path, path);
    }
}

static void AcceptLiteralExpr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case HIR_LIT_COMPOSITE:
            AcceptPath(V, &e->composite.path);
            accept_expr_list(V, e->composite.items);
            break;
        case HIR_LIT_ARRAY:
            accept_expr_list(V, e->array.elems);
            break;
        case HIR_LIT_TUPLE:
            accept_expr_list(V, e->tuple.elems);
            break;
        default:
            break;
    }
}

static void AcceptTryExpr(struct HirVisitor *V, struct HirTryExpr *e)
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

static void AcceptLetStmt(struct HirVisitor *V, struct HirLetStmt *s)
{
    if (s->pat != NULL)
        AcceptPat(V, s->pat);
    if (s->tag != NULL)
        AcceptType(V, s->tag);
    if (s->init != NULL)
        AcceptExpr(V, s->init);
}

static void AcceptExprStmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    AcceptExpr(V, s->expr);
}

static void AcceptClosureExpr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    accept_decl_list(V, e->params);
    if (e->result != NULL)
        AcceptType(V, e->result);
    AcceptExpr(V, e->expr);
}

static void AcceptFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    if (d->tag != NULL)
        AcceptType(V, d->tag);
}

static void AcceptParamDecl(struct HirVisitor *V, struct HirParamDecl *d)
{
    if (d->tag != NULL)
        AcceptType(V, d->tag);
}

static void AcceptTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    accept_decl_list(V, d->generics);
    if (d-> rhs != NULL)
        AcceptType(V, d->rhs);
}

static void AcceptGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    if (d->is_type) {
        if (d->t.bounds != NULL) {
            K_LIST_XFOREACH (d->t.bounds, struct HirGenericBound, p)
                AcceptPath(V, &p->path);
        }
    } else {
        AcceptType(V, d->k.type);
    }
}

static void AcceptVariantDecl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    accept_decl_list(V, d->fields);
}

static void AcceptImplDecl(struct HirVisitor *V, struct HirImplDecl *d)
{
    accept_decl_list(V, d->generics);
    if (d->trait != NULL) AcceptType(V, d->trait);
    AcceptType(V, d->type);
    accept_decl_list(V, d->types);
    accept_decl_list(V, d->constants);
    accept_decl_list(V, d->methods);
}

static void AcceptAdtDecl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->variants);
}

static void AcceptTraitDecl(struct HirVisitor *V, struct HirTraitDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->types);
    accept_decl_list(V, d->methods);
}

static void AcceptConstDecl(struct HirVisitor *V, struct HirConstDecl *d)
{
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
    AcceptExpr(V, e->from);
    AcceptType(V, e->to);
}

static void AcceptProjectionExpr(struct HirVisitor *V, struct HirProjectionExpr *e)
{
    AcceptType(V, e->type);
    AcceptPath(V, &e->trait);
}

static void AcceptAscriptionExpr(struct HirVisitor *V, struct HirAscriptionExpr *e)
{
    AcceptExpr(V, e->expr);
    AcceptType(V, e->type);
}

static void AcceptPathExpr(struct HirVisitor *V, struct HirPathExpr *e)
{
    AcceptPath(V, &e->path);
}

static void AcceptFnDecl(struct HirVisitor *V, struct HirFnDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->params);
    AcceptType(V, d->result);
    if (d->body != NULL)
        AcceptExpr(V, d->body);
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
    AcceptExpr(V, e->index);
}

static void AcceptSelector(struct HirVisitor *V, struct HirSelector *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptDeclStmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    AcceptDecl(V, s->decl);
}

static void AcceptFnPtr(struct HirVisitor *V, struct HirFnPtr *t)
{
    accept_type_list(V, t->params);
    AcceptType(V, t->result);
}

static void AcceptSliceType(struct HirVisitor *V, struct HirSliceType *t)
{
    AcceptType(V, t->type);
}

static void AcceptArrayType(struct HirVisitor *V, struct HirArrayType *t)
{
    AcceptExpr(V, t->length);
    AcceptType(V, t->type);
}

static void AcceptTupleType(struct HirVisitor *V, struct HirTupleType *t)
{
    accept_type_list(V, t->elems);
}

static void AcceptNeverType(struct HirVisitor *V, struct HirNeverType *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptInferType(struct HirVisitor *V, struct HirInferType *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptProjectionType(struct HirVisitor *V, struct HirProjectionType *t)
{
    AcceptType(V, t->type);
    AcceptPath(V, &t->trait);
}

static void AcceptRefType(struct HirVisitor *V, struct HirRefType *t)
{
    AcceptType(V, t->type);
}

static void AcceptPathType(struct HirVisitor *V, struct HirPathType *t)
{
    struct HirSegment const *pseg;
    K_LIST_FOREACH (t->path.segments, pseg) {
        if (pseg->args != NULL)
            accept_generic_args(V, pseg->args);
    }
}

static void AcceptOrPat(struct HirVisitor *V, struct HirOrPat *e)
{
    accept_pat_list(V, e->pats);
}

static void AcceptDerefPat(struct HirVisitor *V, struct HirDerefPat *e)
{
    AcceptPat(V, e->pointee);
}

static void AcceptFieldPat(struct HirVisitor *V, struct HirFieldPat *p)
{
    AcceptPat(V, p->pat);
}

static void AcceptStructPat(struct HirVisitor *V, struct HirStructPat *p)
{
    AcceptPath(V, &p->path);
    accept_pat_list(V, p->fields);
}

static void AcceptVariantPat(struct HirVisitor *V, struct HirVariantPat *p)
{
    AcceptPath(V, &p->path);
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

static void AcceptLiteralPat(struct HirVisitor *V, struct HirLiteralPat *p)
{
    AcceptExpr(V, p->expr);
}

static void AcceptWildcardPat(struct HirVisitor *V, struct HirWildcardPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

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

static paw_Bool default_visit_path(struct HirVisitor *V, struct HirPath *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
    return PAW_TRUE;
}

static paw_Bool default_visit_segment(struct HirVisitor *V, struct HirSegment *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
    return PAW_TRUE;
}

static paw_Bool default_visit_expr(struct HirVisitor *V, struct HirExpr *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
    return PAW_TRUE;
}

static paw_Bool default_visit_decl(struct HirVisitor *V, struct HirDecl *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
    return PAW_TRUE;
}

static paw_Bool default_visit_stmt(struct HirVisitor *V, struct HirStmt *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
    return PAW_TRUE;
}

static paw_Bool default_visit_type(struct HirVisitor *V, struct HirType *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
    return PAW_TRUE;
}

static paw_Bool default_visit_pat(struct HirVisitor *V, struct HirPat *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
    return PAW_TRUE;
}

static void default_post_visit_path(struct HirVisitor *V, struct HirPath *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
}

static void default_post_visit_segment(struct HirVisitor *V, struct HirSegment *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
}

static void default_post_visit_expr(struct HirVisitor *V, struct HirExpr *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
}

static void default_post_visit_decl(struct HirVisitor *V, struct HirDecl *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
}

static void default_post_visit_stmt(struct HirVisitor *V, struct HirStmt *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
}

static void default_post_visit_type(struct HirVisitor *V, struct HirType *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
}

static void default_post_visit_pat(struct HirVisitor *V, struct HirPat *node)
{
    (PAW_UNUSED(V), PAW_UNUSED(node));
}

void pawHir_visitor_init(struct HirVisitor *V, struct Hir *hir, void *ud)
{
    *V = (struct HirVisitor){
        .hir = hir,
        .ud = ud,

        .VisitPath = default_visit_path,
        .VisitSegment = default_visit_segment,
        .VisitExpr = default_visit_expr,
        .VisitDecl = default_visit_decl,
        .VisitStmt = default_visit_stmt,
        .VisitType = default_visit_type,
        .VisitPat = default_visit_pat,

        .PostVisitPath = default_post_visit_path,
        .PostVisitSegment = default_post_visit_segment,
        .PostVisitExpr = default_post_visit_expr,
        .PostVisitDecl = default_post_visit_decl,
        .PostVisitStmt = default_post_visit_stmt,
        .PostVisitType = default_post_visit_type,
        .PostVisitPat = default_post_visit_pat,
    };
}

void pawHir_visit_generic_args(struct HirVisitor *V, struct HirGenericArgs *args)
{
    accept_generic_args(V, args);
}

#define DEFINE_VISITORS(name, T)                                                     \
    void pawHir_visit_##name(struct HirVisitor *V, struct Hir##T *node)              \
    {                                                                                \
        paw_assert(node != NULL);                                                    \
        Accept##T(V, node);                                                          \
    }                                                                                \
    void pawHir_visit_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    {                                                                                \
        if (list == NULL)                                                            \
            return;                                                                  \
        struct Hir##T *const *pt;                                                    \
        K_LIST_FOREACH (list, pt) {                                                  \
            pawHir_visit_##name(V, *pt);                                             \
        }                                                                            \
    }
DEFINE_VISITORS(expr, Expr)
DEFINE_VISITORS(decl, Decl)
DEFINE_VISITORS(stmt, Stmt)
DEFINE_VISITORS(type, Type)
DEFINE_VISITORS(pat, Pat)
#undef DEFINE_VISITORS


#define FOLDER_CALL(F, name, x) ((F)->Fold##name != NULL \
        ? (F)->Fold##name(F, x) : 1)
#define FOLDER_POSTCALL(F, name, x) ((F)->PostFold##name != NULL \
        ? (F)->PostFold##name(F, x) : (void)0)

#define FOLD_EXPR0(F_, Expr_) ((Expr_) != NULL ? (F_)->FoldExpr(F_, Expr_) : NULL)
#define FOLD_STMT0(F_, Stmt_) ((Stmt_) != NULL ? (F_)->FoldStmt(F_, Stmt_) : NULL)
#define FOLD_DECL0(F_, Decl_) ((Decl_) != NULL ? (F_)->FoldDecl(F_, Decl_) : NULL)
#define FOLD_TYPE0(F_, Type_) ((Type_) != NULL ? (F_)->FoldType(F_, Type_) : NULL)
#define FOLD_PAT0(F_, Pat_) ((Pat_) != NULL ? (F_)->FoldPat(F_, Pat_) : NULL)

static NodeId next_node_id(struct HirFolder *F)
{
    return (NodeId){(unsigned)++F->hir->node_count};
}

static struct HirGenericArg fold_generic_arg(struct HirFolder *F, struct HirGenericArg arg)
{
    if (arg.is_type) {
        return (struct HirGenericArg){
            .is_type = PAW_TRUE,
            .t = F->FoldType(F, arg.t),
        };
    } else {
        return (struct HirGenericArg){
            .is_type = PAW_FALSE,
            .k = F->FoldExpr(F, arg.k),
        };
    }
}

static HirGenericArgs *fold_generic_args(struct HirFolder *F, HirGenericArgs *args)
{
    if (args != NULL) {
        HirGenericArgs *result = HirGenericArgs_new(F->hir);
        HirGenericArgs_reserve(F->hir, result, args->count);
        K_LIST_XFOREACH (args, struct HirGenericArg const, p)
            HirGenericArgs_push(F->hir, result, fold_generic_arg(F, *p));
        return result;
    }
    return NULL;
}

static struct HirExpr *FoldBlock(struct HirFolder *F, struct HirBlock *e)
{
    HirStmtList *stmts = pawHir_fold_stmt_list(F, e->stmts);
    struct HirExpr *result = FOLD_EXPR0(F, e->result);
    return pawHir_new_block(F->hir, e->span, next_node_id(F), stmts, result);
}

static struct HirExpr *FoldLogicalExpr(struct HirFolder *F, struct HirLogicalExpr *e)
{
    struct HirExpr *lhs = F->FoldExpr(F, e->lhs);
    struct HirExpr *rhs = F->FoldExpr(F, e->rhs);
    return pawHir_new_logical_expr(F->hir, e->span, next_node_id(F), lhs, rhs, e->is_and);
}

static struct HirExpr *FoldFieldExpr(struct HirFolder *F, struct HirFieldExpr *e)
{
    if (e->fid < 0) {
        struct HirExpr *key = F->FoldExpr(F, e->key);
        struct HirExpr *value = F->FoldExpr(F, e->value);
        return pawHir_new_keyed_field_expr(F->hir, e->span, next_node_id(F), key, value);
    } else {
        struct HirExpr *value = F->FoldExpr(F, e->value);
        return pawHir_new_named_field_expr(F->hir, e->span, next_node_id(F), e->ident, value, e->fid);
    }
}

static struct HirExpr *FoldAssignExpr(struct HirFolder *F, struct HirAssignExpr *e)
{
    struct HirExpr *lhs = F->FoldExpr(F, e->lhs);
    struct HirExpr *rhs = F->FoldExpr(F, e->rhs);
    return pawHir_new_assign_expr(F->hir, e->span, next_node_id(F), lhs, rhs);
}

static struct HirExpr *FoldOpAssignExpr(struct HirFolder *F, struct HirOpAssignExpr *e)
{
    struct HirExpr *lhs = F->FoldExpr(F, e->lhs);
    struct HirExpr *rhs = F->FoldExpr(F, e->rhs);
    return pawHir_new_assign_expr(F->hir, e->span, next_node_id(F), lhs, rhs);
}

static struct HirExpr *FoldMatchArm(struct HirFolder *F, struct HirMatchArm *e)
{
    struct HirPat *pat = F->FoldPat(F, e->pat);
    struct HirExpr *guard = FOLD_EXPR0(F, e->guard);
    struct HirExpr *result = F->FoldExpr(F, e->result);
    return pawHir_new_match_arm(F->hir, e->span, next_node_id(F), pat, guard, result);
}

static struct HirExpr *FoldMatchExpr(struct HirFolder *F, struct HirMatchExpr *e)
{
    struct HirExpr *target = F->FoldExpr(F, e->target);
    HirExprList *arms = pawHir_fold_expr_list(F, e->arms);
    return pawHir_new_match_expr(F->hir, e->span, next_node_id(F), target, arms);
}

static struct HirSegment FoldSegment(struct HirFolder *F, struct HirSegment seg)
{
    struct HirSegment result;
    HirGenericArgs *args = pawHir_fold_generic_args(F, seg.args);
    pawHir_init_segment(F->hir, &result, next_node_id(F), seg.ident, args, seg.target.id);
    return result;
}

static struct HirPath FoldPath(struct HirFolder *F, struct HirPath path)
{
    HirSegments *segments = HirSegments_new(F->hir);
    HirSegments_reserve(F->hir, segments, path.segments->count);
    K_LIST_XFOREACH (path.segments, struct HirSegment const, p) {
        struct HirSegment const seg = F->FoldSegment(F, *p);
        HirSegments_push(F->hir, segments, seg);
    }
    return pawHir_path_create(path.span, segments, path.kind);
}

static struct HirExpr *FoldLiteralExpr(struct HirFolder *F, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case HIR_LIT_BOOL:
            return pawHir_new_bool_lit(F->hir, e->span, next_node_id(F), e->b);
        case HIR_LIT_CHAR:
            return pawHir_new_char_lit(F->hir, e->span, next_node_id(F), e->c);
        case HIR_LIT_INT:
            return pawHir_new_int_lit(F->hir, e->span, next_node_id(F), e->i.value, e->i.suffix);
        case HIR_LIT_FLOAT:
            return pawHir_new_float_lit(F->hir, e->span, next_node_id(F), e->f.value, e->f.suffix);
        case HIR_LIT_STR:
            return pawHir_new_str_lit(F->hir, e->span, next_node_id(F), e->s);
        case HIR_LIT_COMPOSITE: {
            struct HirPath const path = F->FoldPath(F, e->composite.path);
            HirExprList *items = pawHir_fold_expr_list(F, e->composite.items);
            return pawHir_new_composite_lit(F->hir, e->span, next_node_id(F), path, items);
        }
        case HIR_LIT_TUPLE: {
            HirExprList *fields = pawHir_fold_expr_list(F, e->tuple.elems);
            return pawHir_new_tuple_lit(F->hir, e->span, next_node_id(F), fields);
        }
        case HIR_LIT_ARRAY: {
            HirExprList *elems = pawHir_fold_expr_list(F, e->array.elems);
            return pawHir_new_array_lit(F->hir, e->span, next_node_id(F), elems);
        }
    }
}

static struct HirExpr *FoldTryExpr(struct HirFolder *F, struct HirTryExpr *e)
{
    struct HirExpr *target = F->FoldExpr(F, e->target);
    return pawHir_new_try_expr(F->hir, e->span, next_node_id(F), target);
}

static struct HirExpr *FoldUnOpExpr(struct HirFolder *F, struct HirUnOpExpr *e)
{
    struct HirExpr *target = F->FoldExpr(F, e->target);
    return pawHir_new_unop_expr(F->hir, e->span, next_node_id(F), target, e->op);
}

static struct HirExpr *FoldBinOpExpr(struct HirFolder *F, struct HirBinOpExpr *e)
{
    struct HirExpr *lhs = F->FoldExpr(F, e->lhs);
    struct HirExpr *rhs = F->FoldExpr(F, e->rhs);
    return pawHir_new_binop_expr(F->hir, e->span, next_node_id(F), lhs, rhs, e->op);
}

static struct HirStmt *FoldLetStmt(struct HirFolder *F, struct HirLetStmt *s)
{
    struct HirPat *pat = FOLD_PAT0(F, s->pat);
    struct HirType *tag = FOLD_TYPE0(F, s->tag);
    struct HirExpr *init = FOLD_EXPR0(F, s->init);
    return pawHir_new_let_stmt(F->hir, s->span, next_node_id(F), pat, tag, init);
}

static struct HirStmt *FoldExprStmt(struct HirFolder *F, struct HirExprStmt *s)
{
    struct HirExpr *expr = F->FoldExpr(F, s->expr);
    return pawHir_new_expr_stmt(F->hir, s->span, next_node_id(F), expr);
}

static struct HirExpr *FoldClosureExpr(struct HirFolder *F, struct HirClosureExpr *e)
{
    HirDeclList *params = pawHir_fold_decl_list(F, e->params);
    struct HirType *result = FOLD_TYPE0(F, e->result);
    struct HirExpr *expr = F->FoldExpr(F, e->expr);
    return pawHir_new_closure_expr(F->hir, e->span, next_node_id(F), e->did, params, result, expr);
}

static struct HirDecl *FoldFieldDecl(struct HirFolder *F, struct HirFieldDecl *d)
{
    struct HirType *tag = FOLD_TYPE0(F, d->tag);
    return pawHir_new_field_decl(F->hir, d->span, next_node_id(F), d->did, d->ident, tag, d->is_pub);
}

static struct HirDecl *FoldParamDecl(struct HirFolder *F, struct HirParamDecl *d)
{
    struct HirType *tag = FOLD_TYPE0(F, d->tag);
    return pawHir_new_param_decl(F->hir, d->span, next_node_id(F), d->did, d->ident, tag);
}

static struct HirDecl *FoldTypeDecl(struct HirFolder *F, struct HirTypeDecl *d)
{
    HirDeclList *generics = pawHir_fold_decl_list(F, d->generics);
    struct HirType *rhs = FOLD_TYPE0(F, d->rhs);
    return pawHir_new_type_decl(F->hir, d->span, next_node_id(F), d->did, d->ident, generics, rhs, d->is_pub);
}

static struct HirBoundList *fold_bounds(struct HirFolder *F, struct HirBoundList *bounds)
{
    HirBoundList *result = HirBoundList_new(F->hir);
    HirBoundList_reserve(F->hir, result, bounds->count);
    K_LIST_XFOREACH (bounds, struct HirGenericBound const, p) {
        struct HirPath const path = F->FoldPath(F, p->path);
        HirBoundList_push(F->hir, result, (struct HirGenericBound){path});
    }
    return result;
}

static struct HirDecl *FoldGenericDecl(struct HirFolder *F, struct HirGenericDecl *d)
{
    if (d->is_type) {
        HirBoundList *bounds = d->t.bounds != NULL ? fold_bounds(F, d->t.bounds) : NULL;
        return pawHir_new_generic_type_decl(F->hir, d->span, next_node_id(F), d->did, d->t.ident, bounds);
    } else {
        struct HirType *type = F->FoldType(F, d->k.type);
        return pawHir_new_generic_const_decl(F->hir, d->span, next_node_id(F), d->did, type, d->k.ident);
    }
}

static struct HirDecl *FoldVariantDecl(struct HirFolder *F, struct HirVariantDecl *d)
{
    HirDeclList *fields = pawHir_fold_decl_list(F, d->fields);
    return pawHir_new_variant_decl(F->hir, d->span, next_node_id(F), d->did, d->ident, fields, d->index, d->base_did);
}

static struct HirDecl *FoldImplDecl(struct HirFolder *F, struct HirImplDecl *d)
{
    HirDeclList *generics = pawHir_fold_decl_list(F, d->generics);
    struct HirType *type = FOLD_TYPE0(F, d->trait);
    struct HirType *trait = F->FoldType(F, d->type);
    HirDeclList *types = pawHir_fold_decl_list(F, d->types);
    HirDeclList *constants = pawHir_fold_decl_list(F, d->constants);
    HirDeclList *methods = pawHir_fold_decl_list(F, d->methods);
    return pawHir_new_impl_decl(F->hir, d->span, next_node_id(F), d->did, type, trait, generics, types, constants, methods);
}

static struct HirDecl *FoldAdtDecl(struct HirFolder *F, struct HirAdtDecl *d)
{
    HirDeclList *generics = pawHir_fold_decl_list(F, d->generics);
    HirDeclList *variants = pawHir_fold_decl_list(F, d->variants);
    return pawHir_new_adt_decl(F->hir, d->span, next_node_id(F), d->did, d->ident, generics, variants, d->is_pub, d->is_struct);
}

static struct HirDecl *FoldTraitDecl(struct HirFolder *F, struct HirTraitDecl *d)
{
    HirDeclList *generics = pawHir_fold_decl_list(F, d->generics);
    HirDeclList *types = pawHir_fold_decl_list(F, d->types);
    HirDeclList *methods = pawHir_fold_decl_list(F, d->methods);
    return pawHir_new_trait_decl(F->hir, d->span, next_node_id(F), d->did, d->ident, generics, types, methods, d->is_pub);
}

static struct HirDecl *FoldConstDecl(struct HirFolder *F, struct HirConstDecl *d)
{
    struct HirType *tag = F->FoldType(F, d->tag);
    struct HirExpr *init = FOLD_EXPR0(F, d->init);
    return pawHir_new_const_decl(F->hir, d->span, next_node_id(F), d->did, d->ident, d->annos, tag, init, d->is_pub);
}

static struct HirExpr *FoldReturnExpr(struct HirFolder *F, struct HirReturnExpr *e)
{
    struct HirExpr *expr = FOLD_EXPR0(F, e->expr);
    return pawHir_new_return_expr(F->hir, e->span, next_node_id(F), expr);
}

static struct HirExpr *FoldCallExpr(struct HirFolder *F, struct HirCallExpr *e)
{
    struct HirExpr *target = F->FoldExpr(F, e->target);
    HirExprList *args = pawHir_fold_expr_list(F, e->args);
    return pawHir_new_call_expr(F->hir, e->span, next_node_id(F), target, args);
}

static struct HirExpr *FoldConversionExpr(struct HirFolder *F, struct HirConversionExpr *e)
{
    struct HirExpr *from = F->FoldExpr(F, e->from);
    struct HirType *to = F->FoldType(F, e->to);
    return pawHir_new_conversion_expr(F->hir, e->span, next_node_id(F), from, to);
}

static struct HirExpr *FoldProjectionExpr(struct HirFolder *F, struct HirProjectionExpr *e)
{
    struct HirType *type = F->FoldType(F, e->type);
    struct HirPath const trait = F->FoldPath(F, e->trait);
    return pawHir_new_projection_expr(F->hir, e->span, next_node_id(F), type, trait, e->name);
}

static struct HirExpr *FoldAscriptionExpr(struct HirFolder *F, struct HirAscriptionExpr *e)
{
    struct HirExpr *expr = F->FoldExpr(F, e->expr);
    struct HirType *type = F->FoldType(F, e->type);
    return pawHir_new_ascription_expr(F->hir, e->span, next_node_id(F), expr, type);
}

static struct HirExpr *FoldPathExpr(struct HirFolder *F, struct HirPathExpr *e)
{
    struct HirPath const path = F->FoldPath(F, e->path);
    return pawHir_new_path_expr(F->hir, e->span, next_node_id(F), path);
}

static struct HirDecl *FoldFnDecl(struct HirFolder *F, struct HirFnDecl *d)
{
    HirDeclList *generics = pawHir_fold_decl_list(F, d->generics);
    HirDeclList *params = pawHir_fold_decl_list(F, d->params);
    struct HirType *result = F->FoldType(F, d->result);
    struct HirExpr *body = FOLD_EXPR0(F, d->body);
    return pawHir_new_fn_decl(F->hir, d->span, next_node_id(F), d->did, d->ident,
            d->annos, generics, params, result, body, d->fn_kind,
            d->is_pub, d->is_assoc);
}

static struct HirExpr *FoldLoopExpr(struct HirFolder *F, struct HirLoopExpr *e)
{
    struct HirExpr *block = F->FoldExpr(F, e->block);
    return pawHir_new_loop_expr(F->hir, e->span, next_node_id(F), block);
}

static struct HirExpr *FoldJumpExpr(struct HirFolder *F, struct HirJumpExpr *e)
{
    return pawHir_new_jump_expr(F->hir, e->span, next_node_id(F), e->jump_kind);
}

static struct HirExpr *FoldIndex(struct HirFolder *F, struct HirIndex *e)
{
    struct HirExpr *target = F->FoldExpr(F, e->target);
    struct HirExpr *index = F->FoldExpr(F, e->index);
    return pawHir_new_index_expr(F->hir, e->span, next_node_id(F), target, index);
}

static struct HirExpr *FoldSelector(struct HirFolder *F, struct HirSelector *e)
{
    struct HirExpr *target = F->FoldExpr(F, e->target);
    if (e->is_index) {
        return pawHir_new_index_selector(F->hir, e->span, next_node_id(F), target, e->index);
    } else {
        return pawHir_new_name_selector(F->hir, e->span, next_node_id(F), target, e->ident);
    }
}

static struct HirStmt *FoldDeclStmt(struct HirFolder *F, struct HirDeclStmt *s)
{
    struct HirDecl *decl = F->FoldDecl(F, s->decl);
    return pawHir_new_decl_stmt(F->hir, s->span, next_node_id(F), decl);
}

static struct HirType *FoldFnPtr(struct HirFolder *F, struct HirFnPtr *t)
{
    HirTypeList *params = pawHir_fold_type_list(F, t->params);
    struct HirType *result = F->FoldType(F, t->result);
    return pawHir_new_fn_ptr(F->hir, t->span, next_node_id(F), params, result);
}

static struct HirType *FoldSliceType(struct HirFolder *F, struct HirSliceType *t)
{
    struct HirType *type = F->FoldType(F, t->type);
    return pawHir_new_slice_type(F->hir, t->span, next_node_id(F), type);
}

static struct HirType *FoldArrayType(struct HirFolder *F, struct HirArrayType *t)
{
    struct HirExpr *length = F->FoldExpr(F, t->length);
    struct HirType *type = F->FoldType(F, t->type);
    return pawHir_new_array_type(F->hir, t->span, next_node_id(F), type, length);
}

static struct HirType *FoldTupleType(struct HirFolder *F, struct HirTupleType *t)
{
    HirTypeList *elems = pawHir_fold_type_list(F, t->elems);
    return pawHir_new_tuple_type(F->hir, t->span, next_node_id(F), elems);
}

static struct HirType *FoldNeverType(struct HirFolder *F, struct HirNeverType *t)
{
    return pawHir_new_never_type(F->hir, t->span, next_node_id(F));
}

static struct HirType *FoldInferType(struct HirFolder *F, struct HirInferType *t)
{
    return pawHir_new_infer_type(F->hir, t->span, next_node_id(F));
}

static struct HirType *FoldProjectionType(struct HirFolder *F, struct HirProjectionType *t)
{
    struct HirType *type = F->FoldType(F, t->type);
    struct HirPath const trait = F->FoldPath(F, t->trait);
    return pawHir_new_projection_type(F->hir, t->span, next_node_id(F), type, trait, t->name);
}

static struct HirType *FoldRefType(struct HirFolder *F, struct HirRefType *t)
{
    struct HirType *type = F->FoldType(F, t->type);
    return pawHir_new_ref_type(F->hir, t->span, next_node_id(F), type, t->is_mut);
}

static struct HirType *FoldPathType(struct HirFolder *F, struct HirPathType *t)
{
    struct HirPath const path = F->FoldPath(F, t->path);
    return pawHir_new_path_type(F->hir, t->span, next_node_id(F), path);
}

static struct HirPat *FoldOrPat(struct HirFolder *F, struct HirOrPat *p)
{
    HirPatList *pats = pawHir_fold_pat_list(F, p->pats);
    return pawHir_new_or_pat(F->hir, p->span, next_node_id(F), pats);
}

static struct HirPat *FoldDerefPat(struct HirFolder *F, struct HirDerefPat *p)
{
    struct HirPat *pointee = F->FoldPat(F, p->pointee);
    return pawHir_new_deref_pat(F->hir, p->span, next_node_id(F), pointee);
}

static struct HirPat *FoldFieldPat(struct HirFolder *F, struct HirFieldPat *p)
{
    struct HirPat *pat = F->FoldPat(F, p->pat);
    return pawHir_new_field_pat(F->hir, p->span, next_node_id(F), p->ident, pat, p->index);
}

static struct HirPat *FoldStructPat(struct HirFolder *F, struct HirStructPat *p)
{
    struct HirPath const path = F->FoldPath(F, p->path);
    HirPatList *fields = pawHir_fold_pat_list(F, p->fields);
    return pawHir_new_struct_pat(F->hir, p->span, next_node_id(F), path, fields);
}

static struct HirPat *FoldVariantPat(struct HirFolder *F, struct HirVariantPat *p)
{
    struct HirPath const path = F->FoldPath(F, p->path);
    HirPatList *fields = pawHir_fold_pat_list(F, p->fields);
    return pawHir_new_variant_pat(F->hir, p->span, next_node_id(F), path, fields, p->index);
}

static struct HirPat *FoldTuplePat(struct HirFolder *F, struct HirTuplePat *p)
{
    HirPatList *elems = pawHir_fold_pat_list(F, p->elems);
    return pawHir_new_tuple_pat(F->hir, p->span, next_node_id(F), elems);
}

static struct HirPat *FoldBindingPat(struct HirFolder *F, struct HirBindingPat *p)
{
    return pawHir_new_binding_pat(F->hir, p->span, next_node_id(F), p->ident);
}

static struct HirPat *FoldLiteralPat(struct HirFolder *F, struct HirLiteralPat *p)
{
    struct HirExpr *expr = F->FoldExpr(F, p->expr);
    return pawHir_new_literal_pat(F->hir, p->span, next_node_id(F), expr);
}

static struct HirPat *FoldWildcardPat(struct HirFolder *F, struct HirWildcardPat *p)
{
    return pawHir_new_wildcard_pat(F->hir, p->span, next_node_id(F));
}

#define DEFINE_FOLDER_CASES(X) \
    case kHir##X: { \
        struct Hir##X *x = HirGet##X(node); \
        return F->Fold##X(F, x); \
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
#undef FOLDER_POSTCALL
#undef FOLDER_CALL

void pawHir_folder_init(struct HirFolder *F, struct Hir *hir, void *ud)
{
    *F = (struct HirFolder){
        .hir = hir,
        .ud = ud,

        .FoldPath = FoldPath,
        .FoldSegment = FoldSegment,
        .FoldExpr = FoldExpr,
        .FoldDecl = FoldDecl,
        .FoldStmt = FoldStmt,
        .FoldType = FoldType,
        .FoldPat = FoldPat,

#define SET_DEFAULT_FOLDERS(X) .Fold##X = Fold##X,
        HIR_EXPR_LIST(SET_DEFAULT_FOLDERS)
        HIR_STMT_LIST(SET_DEFAULT_FOLDERS)
        HIR_DECL_LIST(SET_DEFAULT_FOLDERS)
        HIR_TYPE_LIST(SET_DEFAULT_FOLDERS)
        HIR_PAT_LIST(SET_DEFAULT_FOLDERS)
#undef SET_DEFAULT_FOLDERS
    };
}

HirGenericArgs *pawHir_fold_generic_args(struct HirFolder *F, HirGenericArgs *args)
{
    return fold_generic_args(F, args);
}

#define DEFINE_FOLDERS(name, T) \
    struct Hir##T *pawHir_fold_##name(struct HirFolder *F, struct Hir##T *node) \
    { \
        paw_assert(node != NULL); \
        return F->Fold##T(F, node); \
    } \
    Hir##T##List *pawHir_fold_##name##_list(struct HirFolder *F, Hir##T##List *list) \
    { \
        if (list == NULL) return NULL; \
        Hir##T##List *result = Hir##T##List_new(F->hir); \
        Hir##T##List_reserve(F->hir, result, list->count); \
        K_LIST_XFOREACH (list, struct Hir##T *const, pt) { \
            struct Hir##T *value = pawHir_fold_##name(F, *pt); \
            Hir##T##List_push(F->hir, result, value); \
        } \
        return result; \
    }
DEFINE_FOLDERS(expr, Expr)
DEFINE_FOLDERS(decl, Decl)
DEFINE_FOLDERS(stmt, Stmt)
DEFINE_FOLDERS(type, Type)
DEFINE_FOLDERS(pat, Pat)
#undef DEFINE_FOLDERS


IrTypeList *pawHir_collect_decl_types(struct Compiler *C, struct HirDeclList *list)
{
    if (list == NULL)
        return NULL;
    IrTypeList *types = IrTypeList_new(C);
    IrTypeList_reserve(C, types, list->count);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (list, pdecl) {
        IrType *type = GET_NODE_TYPE(C, *pdecl);
        IrTypeList_push(C, types, type);
    }
    return types;
}

struct Printer {
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define DUMP_FMT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define DUMP_CSTR(P, cs) pawL_add_string(ENV(P), (P)->buf, cs)
#define DUMP_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)
#define DUMP_STR(P, s) DUMP_CSTR(P, s->text)

static paw_Bool is_unit_type(struct HirType *type)
{
    return HirIsTupleType(type)
        && HirGetTupleType(type)->elems->count == 0;
}

static paw_Bool is_unit_lit(struct HirExpr *expr)
{
    return HirIsLiteralExpr(expr)
        && HirGetLiteralExpr(expr)->lit_kind == HIR_LIT_TUPLE
        && HirGetLiteralExpr(expr)->tuple.elems->count == 0;
}

static void add_newline(struct Printer *P)
{
    DUMP_CHAR(P, '\n');
}

static void add_indentation(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        DUMP_CSTR(P, "    ");
    }
}

static void newline_indent(struct Printer *P)
{
    add_newline(P);
    add_indentation(P);
}

static void dump_type(struct Printer *P, struct HirType *type);
static void dump_expr(struct Printer *P, struct HirExpr *expr);
static void dump_decl(struct Printer *P, struct HirDecl *decl);
static void dump_stmt(struct Printer *P, struct HirStmt *stmt);
static void dump_pat(struct Printer *P, struct HirPat *pat);

static void dump_decls(struct Printer *P, struct HirDeclList *methods)
{
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH(methods, pdecl) {
        add_indentation(P);
        dump_decl(P, *pdecl);
        add_newline(P);
    }
}

static void dump_stmts(struct Printer *P, struct HirStmtList *stmts)
{
    struct HirStmt *const *pstmt;
    K_LIST_FOREACH(stmts, pstmt) {
        dump_stmt(P, *pstmt);
    }
}

static void dump_adt_variants(struct Printer *P, struct HirDeclList *variants, paw_Bool is_struct)
{
    if (is_struct) variants = HirGetVariantDecl(K_LIST_FIRST(variants))->fields;

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH(variants, pdecl) {
        add_indentation(P);
        dump_decl(P, *pdecl);
        DUMP_CHAR(P, ',');
        add_newline(P);
    }
}

static void dump_literal_fields(struct Printer *P, struct HirExprList *fields)
{
    ++P->indent;
    add_newline(P);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH(fields, pexpr) {
        add_indentation(P);
        dump_expr(P, *pexpr);
        DUMP_CHAR(P, ',');
        add_newline(P);
    }

    --P->indent;
    add_indentation(P);
}

static void dump_variant_fields(struct Printer *P, struct HirDeclList *fields)
{
    int index;
    struct HirDecl *const *pdecl;
    K_LIST_ENUMERATE (fields, index, pdecl) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_decl(P, *pdecl);
    }
}

static void dump_match_body(struct Printer *P, struct HirExprList *arms)
{
    DUMP_CSTR(P, " {");
    ++P->indent;
    add_newline(P);

    int index;
    struct HirExpr *const *pexpr;
    K_LIST_ENUMERATE (arms, index, pexpr) {
        add_indentation(P);
        dump_expr(P, *pexpr);
        DUMP_CHAR(P, ',');
        add_newline(P);
    }

    --P->indent;
    add_indentation(P);
    DUMP_CHAR(P, '}');
}

static void dump_args(struct Printer *P, struct HirExprList *args)
{
    int index;
    struct HirExpr *const *pexpr;
    K_LIST_ENUMERATE (args, index, pexpr) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_expr(P, *pexpr);
    }
}

static void dump_params(struct Printer *P, struct HirDeclList *params)
{
    int index;
    struct HirDecl *const *pdecl;
    K_LIST_ENUMERATE (params, index, pdecl) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_decl(P, *pdecl);
    }
}

static void dump_types(struct Printer *P, struct HirTypeList *types)
{
    int index;
    struct HirType *const *ptype;
    K_LIST_ENUMERATE (types, index, ptype) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_type(P, *ptype);
    }
}

static void dump_generic_args(struct Printer *P, struct HirGenericArgs *args)
{
    if (args != NULL) {
        DUMP_CHAR(P, '<');

        int index;
        struct HirGenericArg const *p;
        K_LIST_ENUMERATE (args, index, p) {
            if (index > 0) DUMP_CSTR(P, ", ");
            if (p->is_type) {
                dump_type(P, p->t);
            } else {
                dump_expr(P, p->k);
            }
        }

        DUMP_CHAR(P, '>');
    }
}

static void dump_generics(struct Printer *P, struct HirDeclList *generics)
{
    if (generics == NULL) return;
    DUMP_CHAR(P, '<');

    int index;
    struct HirDecl *const *pdecl;
    K_LIST_ENUMERATE (generics, index, pdecl) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_decl(P, *pdecl);
    }

    DUMP_CHAR(P, '>');
}

static void dump_path(struct Printer *P, struct HirPath *p, paw_Bool is_type)
{
    int index;
    struct HirSegment *pseg;
    K_LIST_ENUMERATE (p->segments, index, pseg) {
        if (index > 0) DUMP_CSTR(P, "::");
        DUMP_STR(P, pseg->ident.name);
        if (pseg->args != NULL) {
            if (!is_type)
                DUMP_CSTR(P, "::");
            dump_generic_args(P, pseg->args);
        }
    }
}

static void dump_bounds(struct Printer *P, struct HirBoundList *bounds)
{
    int index;
    struct HirGenericBound *pbound;
    K_LIST_ENUMERATE (bounds, index, pbound) {
        if (index > 0) DUMP_CSTR(P, " + ");
        dump_path(P, &pbound->path, PAW_TRUE);
    }
}

static void dump_pats(struct Printer *P, struct HirPatList *pats)
{
    int index;
    struct HirPat *const *ppat;
    K_LIST_ENUMERATE (pats, index, ppat) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_pat(P, *ppat);
    }
}

static void dump_pat(struct Printer *P, struct HirPat *pat)
{
    switch (HIR_KINDOF(pat)) {
        case kHirBindingPat: {
            struct HirBindingPat *p = HirGetBindingPat(pat);
            DUMP_STR(P, p->ident.name);
            break;
        }
        case kHirOrPat: {
            int index;
            struct HirPat *const *ppat;
            struct HirOrPat *p = HirGetOrPat(pat);
            K_LIST_ENUMERATE (p->pats, index, ppat) {
                if (index > 0) DUMP_CSTR(P, " | ");
                dump_pat(P, *ppat);
            }
            break;
        }
        case kHirDerefPat: {
            struct HirDerefPat *p = HirGetDerefPat(pat);
            DUMP_CSTR(P, "*");
            dump_pat(P, p->pointee);
            break;
        }
        case kHirFieldPat: {
            struct HirFieldPat *p = HirGetFieldPat(pat);
            if (p->ident.name != NULL) {
                DUMP_STR(P, p->ident.name);
            }
            if (p->pat != NULL) {
                if (p->ident.name != NULL)
                    DUMP_CSTR(P, ": ");
                dump_pat(P, p->pat);
            }
            break;
        }
        case kHirStructPat: {
            struct HirStructPat *p = HirGetStructPat(pat);
            dump_path(P, &p->path, PAW_FALSE);
            dump_pats(P, p->fields);
            break;
        }
        case kHirVariantPat: {
            struct HirVariantPat *p = HirGetVariantPat(pat);
            dump_path(P, &p->path, PAW_FALSE);
            if (p->fields->count > 0) {
                DUMP_CHAR(P, '(');
                dump_pats(P, p->fields);
                DUMP_CHAR(P, ')');
            }
            break;
        }
        case kHirTuplePat: {
            struct HirTuplePat *p = HirGetTuplePat(pat);
            DUMP_CHAR(P, '(');
            dump_pats(P, p->elems);
            DUMP_CHAR(P, ')');
            break;
        }
        case kHirLiteralPat: {
            struct HirLiteralPat *p = HirGetLiteralPat(pat);
            dump_expr(P, p->expr);
            break;
        }
        case kHirWildcardPat:
            DUMP_CHAR(P, '_');
            break;
    }
}

static void dump_decl(struct Printer *P, struct HirDecl *decl)
{
    switch (HIR_KINDOF(decl)) {
        case kHirConstDecl: {
            struct HirConstDecl *d = HirGetConstDecl(decl);
            DUMP_CSTR(P, "let ");
            DUMP_STR(P, d->ident.name);
            DUMP_CSTR(P, ": ");
            dump_type(P, d->tag);
            if (d->init != NULL) {
                DUMP_CSTR(P, " = ");
                dump_expr(P, d->init);
            }
            break;
        }
        case kHirTraitDecl: {
            struct HirTraitDecl *d = HirGetTraitDecl(decl);
            DUMP_CSTR(P, "trait ");
            DUMP_STR(P, d->ident.name);
            dump_generics(P, d->generics);
            DUMP_CHAR(P, '{');
            add_newline(P);
            ++P->indent;
            dump_decls(P, d->types);
            dump_decls(P, d->methods);
            --P->indent;
            DUMP_CHAR(P, '}');
            break;
        }
        case kHirFnDecl: {
            struct HirFnDecl *d = HirGetFnDecl(decl);
            if (d->is_pub) DUMP_CSTR(P, "pub ");
            DUMP_CSTR(P, "fn ");
            DUMP_STR(P, d->ident.name);
            dump_generics(P, d->generics);
            DUMP_CHAR(P, '(');
            dump_params(P, d->params);
            DUMP_CHAR(P, ')');
            if (!is_unit_type(d->result)) {
                DUMP_CSTR(P, " -> ");
                dump_type(P, d->result);
            }
            if (d->body != NULL) {
                DUMP_CHAR(P, ' ');
                dump_expr(P, d->body);
            } else {
                DUMP_CHAR(P, ';');
            }
            break;
        }
        case kHirFieldDecl: {
            struct HirFieldDecl *d = HirGetFieldDecl(decl);
            if (d->is_pub)
                DUMP_CSTR(P, "pub ");
            if (d->ident.name != NULL) {
                DUMP_STR(P, d->ident.name);
                if (d->tag != NULL)
                    DUMP_CSTR(P, ": ");
            }
            if (d->tag != NULL)
                dump_type(P, d->tag);
            break;
        }
        case kHirParamDecl: {
            struct HirParamDecl *d = HirGetParamDecl(decl);
            if (d->ident.name != NULL)
                DUMP_STR(P, d->ident.name);
            if (d->tag != NULL) {
                DUMP_CSTR(P, ": ");
                dump_type(P, d->tag);
            }
            break;
        }
        case kHirVariantDecl: {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            DUMP_STR(P, d->ident.name);
            if (d->fields != NULL) {
                DUMP_CHAR(P, '(');
                dump_variant_fields(P, d->fields);
                DUMP_CHAR(P, ')');
            }
            break;
        }
        case kHirImplDecl: {
            struct HirImplDecl *d = HirGetImplDecl(decl);
            DUMP_CSTR(P, "impl");
            dump_generics(P, d->generics);
            DUMP_CSTR(P, " ");
            if (d->trait != NULL) {
                dump_type(P, d->trait);
                DUMP_CSTR(P, " for ");
            }
            dump_type(P, d->type);
            DUMP_CSTR(P, " {");
            ++P->indent;
            add_newline(P);
            dump_decls(P, d->types);
            dump_decls(P, d->constants);
            dump_decls(P, d->methods);
            --P->indent;
            DUMP_CHAR(P, '}');
            break;
        }
        case kHirAdtDecl: {
            struct HirAdtDecl *d = HirGetAdtDecl(decl);
            if (d->is_struct) {
                DUMP_CSTR(P, "struct ");
            } else {
                DUMP_CSTR(P, "enum ");
            }
            DUMP_STR(P, d->ident.name);
            dump_generics(P, d->generics);
            DUMP_CSTR(P, " {");
            ++P->indent;
            add_newline(P);
            dump_adt_variants(P, d->variants, d->is_struct);
            --P->indent;
            DUMP_CHAR(P, '}');
            break;
        }
        case kHirGenericDecl: {
            struct HirGenericDecl *d = HirGetGenericDecl(decl);
            if (d->is_type) {
                DUMP_STR(P, d->t.ident.name);
                if (d->t.bounds != NULL) {
                    DUMP_CSTR(P, ": ");
                    dump_bounds(P, d->t.bounds);
                }
            } else {
                DUMP_CSTR(P, "const ");
                DUMP_STR(P, d->k.ident.name);
                DUMP_CSTR(P, ": ");
                dump_type(P, d->k.type);
            }
            break;
        }
        case kHirTypeDecl: {
            struct HirTypeDecl *d = HirGetTypeDecl(decl);
            DUMP_CSTR(P, "type ");
            DUMP_STR(P, d->ident.name);
            dump_generics(P, d->generics);
            if (d->rhs != NULL) {
                DUMP_CSTR(P, " = ");
                dump_type(P, d->rhs);
            }
            break;
        }
    }
}

static void dump_stmt(struct Printer *P, struct HirStmt *stmt)
{
    add_indentation(P);
    switch (HIR_KINDOF(stmt)) {
        case kHirLetStmt: {
            struct HirLetStmt *s = HirGetLetStmt(stmt);
            DUMP_CSTR(P, "let ");
            dump_pat(P, s->pat);
            if (s->tag != NULL) {
                DUMP_CSTR(P, ": ");
                dump_type(P, s->tag);
            }
            if (s->init != NULL) {
                DUMP_CSTR(P, " = ");
                dump_expr(P, s->init);
            }
            break;
        }
        case kHirExprStmt: {
            struct HirExprStmt *s = HirGetExprStmt(stmt);
            dump_expr(P, s->expr);
            if (!HirIsLoopExpr(s->expr)
                    && !HirIsMatchExpr(s->expr)
                    && !HirIsBlock(s->expr)) {
                DUMP_CHAR(P, ';');
            }
            break;
        }
        case kHirDeclStmt: {
            struct HirDeclStmt *s = HirGetDeclStmt(stmt);
            dump_decl(P, s->decl);
            DUMP_CHAR(P, ';');
            break;
        }
    }
    add_newline(P);
}

static void dump_type(struct Printer *P, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirProjectionType: {
            struct HirProjectionType *t = HirGetProjectionType(type);
            DUMP_CHAR(P, '<');
            dump_type(P, t->type);
            DUMP_CSTR(P, " as ");
            dump_path(P, &t->trait, PAW_TRUE);
            DUMP_FMT(P, ">::%s", t->name->text);
            break;
        }
        case kHirRefType: {
            struct HirRefType *t = HirGetRefType(type);
            DUMP_CHAR(P, '*');
            if (t->is_mut)
                DUMP_CSTR(P, "mut ");
            dump_type(P, t->type);
            break;
        }
        case kHirPathType: {
            struct HirPathType *t = HirGetPathType(type);
            dump_path(P, &t->path, PAW_TRUE);
            break;
        }
        case kHirSliceType: {
            struct HirSliceType *t = HirGetSliceType(type);
            DUMP_CSTR(P, "[]");
            dump_type(P, t->type);
            break;
        }
        case kHirArrayType: {
            struct HirArrayType *t = HirGetArrayType(type);
            DUMP_CHAR(P, '[');
            dump_expr(P, t->length);
            DUMP_CHAR(P, ']');
            dump_type(P, t->type);
            break;
        }
        case kHirTupleType: {
            struct HirTupleType *t = HirGetTupleType(type);
            dump_types(P, t->elems);
            break;
        }
        case kHirFnPtr: {
            struct HirFnPtr *t = HirGetFnPtr(type);
            DUMP_CSTR(P, "fn(");
            dump_types(P, t->params);
            DUMP_CHAR(P, ')');
            if (!is_unit_type(t->result)) {
                DUMP_CSTR(P, " -> ");
                dump_type(P, t->result);
            }
            break;
        }
        case kHirNeverType:
            DUMP_CHAR(P, '!');
            break;
        case kHirInferType:
            DUMP_CHAR(P, '_');
            break;
    }
}

static void dump_expr(struct Printer *P, struct HirExpr *expr)
{
    switch (HIR_KINDOF(expr)) {
        case kHirAscriptionExpr: {
            struct HirAscriptionExpr *e = HirGetAscriptionExpr(expr);
            dump_expr(P, e->expr);
            DUMP_CSTR(P, ": ");
            dump_type(P, e->type);
            break;
        }
        case kHirLogicalExpr: {
            struct HirLogicalExpr *e = HirGetLogicalExpr(expr);
            dump_expr(P, e->lhs);
            DUMP_CSTR(P, e->is_and ? " && " : " || ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirPathExpr: {
            struct HirPathExpr *e = HirGetPathExpr(expr);
            dump_path(P, &e->path, PAW_FALSE);
            break;
        }
        case kHirTryExpr: {
            struct HirTryExpr *e = HirGetTryExpr(expr);
            dump_expr(P, e->target);
            DUMP_CHAR(P, '?');
            break;
        }
        case kHirMatchExpr: {
            struct HirMatchExpr *e = HirGetMatchExpr(expr);
            DUMP_CSTR(P, "match ");
            dump_expr(P, e->target);
            dump_match_body(P, e->arms);
            break;
        }
        case kHirMatchArm: {
            struct HirMatchArm *e = HirGetMatchArm(expr);
            dump_pat(P, e->pat);
            if (e->guard != NULL) {
                DUMP_CSTR(P, " if ");
                dump_expr(P, e->guard);
            }
            DUMP_CSTR(P, " => ");
            dump_expr(P, e->result);
            break;
        }
        case kHirClosureExpr: {
            struct HirClosureExpr *e = HirGetClosureExpr(expr);
            DUMP_CHAR(P, '|');
            dump_params(P, e->params);
            DUMP_CHAR(P, '|');
            if (e->result != NULL) {
                DUMP_CSTR(P, " -> ");
                dump_type(P, e->result);
            }
            DUMP_CHAR(P, ' ');
            dump_expr(P, e->expr);
            break;
        }
        case kHirConversionExpr: {
            struct HirConversionExpr *e = HirGetConversionExpr(expr);
            dump_expr(P, e->from);
            DUMP_CSTR(P, " as ");
            dump_type(P, e->to);
            break;
        }
        case kHirProjectionExpr: {
            struct HirProjectionExpr *e = HirGetProjectionExpr(expr);
            DUMP_CHAR(P, '<');
            dump_type(P, e->type);
            DUMP_CSTR(P, " as ");
            DUMP_CSTR(P, ">::");
            DUMP_STR(P, e->name);
            break;
        }
        case kHirFieldExpr: {
            struct HirFieldExpr *e = HirGetFieldExpr(expr);
            if (e->fid < 0) {
                dump_expr(P, e->key);
            } else {
                DUMP_STR(P, e->ident.name);
            }
            DUMP_CSTR(P, ": ");
            dump_expr(P, e->value);
            break;
        }
        case kHirJumpExpr: {
            struct HirJumpExpr *e = HirGetJumpExpr(expr);
            if (e->jump_kind == JUMP_BREAK) {
                DUMP_CSTR(P, "break");
            } else {
                DUMP_CSTR(P, "continue");
            }
            break;
        }
        case kHirLiteralExpr: {
            struct HirLiteralExpr *e = HirGetLiteralExpr(expr);
            switch (e->lit_kind) {
                case HIR_LIT_BOOL:
                    DUMP_FMT(P, "%s", e->b ? "true" : "false");
                    break;
                case HIR_LIT_CHAR:
                    DUMP_FMT(P, "%c", e->c);
                    break;
                case HIR_LIT_INT:
                    DUMP_FMT(P, "%I", e->i);
                    break;
                case HIR_LIT_FLOAT:
                    DUMP_FMT(P, "%f", e->f);
                    break;
                case HIR_LIT_STR:
                    DUMP_FMT(P, "\"%s\"", e->s->text);
                    break;
                case HIR_LIT_TUPLE:
                    DUMP_CHAR(P, '(');
                    dump_args(P, e->tuple.elems);
                    if (e->tuple.elems->count == 1)
                        DUMP_CHAR(P, ',');
                    DUMP_CHAR(P, ')');
                    break;
                case HIR_LIT_ARRAY:
                    DUMP_CHAR(P, '[');
                    dump_literal_fields(P, e->array.elems);
                    DUMP_CHAR(P, ']');
                    break;
                case HIR_LIT_COMPOSITE:
                    dump_path(P, &e->composite.path, PAW_FALSE);
                    DUMP_CHAR(P, '{');
                    if (e->composite.items->count > 0) {
                        dump_literal_fields(P, e->composite.items);
                    }
                    DUMP_CHAR(P, '}');
                    break;
            }
            break;
        }
        case kHirUnOpExpr: {
            struct HirUnOpExpr *e = HirGetUnOpExpr(expr);
            DUMP_CSTR(P, paw_unop_symbol(e->op));
            dump_expr(P, e->target);
            break;
        }
        case kHirBinOpExpr: {
            struct HirBinOpExpr *e = HirGetBinOpExpr(expr);
            dump_expr(P, e->lhs);
            DUMP_CHAR(P, ' ');
            DUMP_CSTR(P, paw_binop_symbol(e->op));
            DUMP_CHAR(P, ' ');
            dump_expr(P, e->rhs);
            break;
        }
        case kHirAssignExpr: {
            struct HirAssignExpr *e = HirGetAssignExpr(expr);
            dump_expr(P, e->lhs);
            DUMP_CSTR(P, " = ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirOpAssignExpr: {
            struct HirOpAssignExpr *e = HirGetOpAssignExpr(expr);
            dump_expr(P, e->lhs);
            DUMP_FMT(P, " %s= ", paw_binop_symbol(e->op));
            dump_expr(P, e->rhs);
            break;
        }
        case kHirCallExpr: {
            struct HirCallExpr *e = HirGetCallExpr(expr);
            dump_expr(P, e->target);
            DUMP_CHAR(P, '(');
            dump_args(P, e->args);
            DUMP_CHAR(P, ')');
            break;
        }
        case kHirIndex: {
            struct HirIndex *e = HirGetIndex(expr);
            dump_expr(P, e->target);
            DUMP_CHAR(P, '[');
            dump_expr(P, e->index);
            DUMP_CHAR(P, ']');
            break;
        }
        case kHirSelector: {
            struct HirSelector *e = HirGetSelector(expr);
            dump_expr(P, e->target);
            DUMP_CHAR(P, '.');
            if (e->is_index) {
                DUMP_FMT(P, "%I", e->index);
            } else {
                DUMP_STR(P, e->ident.name);
            }
            break;
        }
        case kHirBlock: {
            struct HirBlock *e = HirGetBlock(expr);
            DUMP_CHAR(P, '{');
            if (e->stmts->count > 0 || e->result != NULL) {
                ++P->indent;
                add_newline(P);
                dump_stmts(P, e->stmts);
                if (e->result != NULL) {
                    add_indentation(P);
                    dump_expr(P, e->result);
                    add_newline(P);
                }
                --P->indent;
                add_indentation(P);
            }
            DUMP_CHAR(P, '}');
            break;
        }
        case kHirLoopExpr: {
            struct HirLoopExpr *e = HirGetLoopExpr(expr);
            DUMP_CSTR(P, "loop ");
            dump_expr(P, e->block);
            break;
        }
        case kHirReturnExpr: {
            struct HirReturnExpr *e = HirGetReturnExpr(expr);
            DUMP_CSTR(P, "return");
            if (e->expr != NULL) {
                DUMP_CHAR(P, ' ');
                dump_expr(P, e->expr);
            }
            break;
        }
    }
}

char const *pawHir_dump(struct Hir *hir)
{
    Buffer buf;
    paw_Env *P = ENV(hir);
    pawL_init_buffer(P, &buf);
    struct Printer print = {
        .buf = &buf,
        .P = P,
    };
    struct HirModule const *pm;
    K_LIST_FOREACH (hir->modules, pm) {
        int index;
        struct HirDecl *const *pitem;
        K_LIST_ENUMERATE (pm->items, index, pitem) {
            if (index > 0) add_newline(&print);
            dump_decl(&print, *pitem);
            add_newline(&print);
        }
    }
    Str const *result = pawL_buffer_finish(P, &buf);
    return result->text;
}

char const *pawHir_print_path(struct Compiler *C, struct HirPath *path)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_path(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
               },
               path, PAW_TRUE);

    Str const *result = pawL_buffer_finish(P, &buf);
    return result->text;
}

