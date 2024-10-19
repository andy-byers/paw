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
    hir->items = pawHir_decl_list_new(C);
    return hir;
}

void pawHir_free(struct Hir *hir)
{
    PAW_UNUSED(hir);
}

#define NEW_NODE(C, T) pawK_pool_alloc(ENV(C), (C)->pool, sizeof(struct T))

#define DEFINE_NODE_CONSTRUCTOR(name, T) \
        struct T *pawHir_new_##name(struct Compiler *C, int line, enum T##Kind kind) \
        { \
            struct T *r = NEW_NODE(C, T); \
            r->hdr.line = line; \
            r->hdr.kind = kind; \
            return r; \
        }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)

#define LIST_MIN_ALLOC 8

struct HirType *pawHir_new_type(struct Compiler *C, int line, enum HirTypeKind kind)
{
    struct HirType *r = NEW_NODE(C, HirType);
    r->hdr.kind = kind;
    r->hdr.line = line;
    return r;
}

struct HirSegment *pawHir_segment_new(struct Compiler *C)
{
    return NEW_NODE(C, HirSegment);
}

DefId pawHir_add_decl(struct Compiler *C, struct HirDecl *decl)
{
    paw_Env *P = ENV(C);
    struct DynamicMem *dm = C->dm;
    const DefId did = dm->decls->count;
    pawHir_decl_list_push(C, dm->decls, decl);
    decl->hdr.did = did;
    return did;
}

struct HirDecl *pawHir_get_decl(struct Compiler *C, DefId did)
{
    struct DynamicMem *dm = C->dm;
    paw_assert(did < dm->decls->count);
    return dm->decls->data[did];
}

struct HirSymbol *pawHir_new_symbol(struct Compiler *C)
{
    return pawK_pool_alloc(ENV(C), C->pool, sizeof(struct HirSymbol));
}

void pawHir_add_scope(struct Compiler *C, struct HirSymtab *table, struct HirScope *scope)
{
    if (table->count == UINT16_MAX) pawM_error(ENV(C));
    pawHir_symtab_push(C, table, scope);
}

struct HirSymbol *pawHir_add_symbol(struct Compiler *C, struct HirScope *table)
{
    struct HirSymbol *symbol = pawHir_new_symbol(C);
    pawHir_scope_push(C, table, symbol);
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

#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

static void AcceptType(struct HirVisitor *V, struct HirType *node);
static void AcceptExpr(struct HirVisitor *V, struct HirExpr *node);
static void AcceptDecl(struct HirVisitor *V, struct HirDecl *node);
static void AcceptStmt(struct HirVisitor *V, struct HirStmt *node);

#define DEFINE_LIST_ACCEPTOR(name, T) \
    static void accept_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            Accept##T(V, list->data[i]); \
        } \
    }
DEFINE_LIST_ACCEPTOR(decl, Decl)
DEFINE_LIST_ACCEPTOR(expr, Expr)
DEFINE_LIST_ACCEPTOR(stmt, Stmt)
DEFINE_LIST_ACCEPTOR(type, Type)

static void AcceptBlock(struct HirVisitor *V, struct HirBlock *s)
{
    accept_stmt_list(V, s->stmts);
}

static void AcceptLogicalExpr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptFieldExpr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    if (e->fid < 0) AcceptExpr(V, e->key);
    AcceptExpr(V, e->value);
}

static void AcceptAssignExpr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
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
        struct HirSegment *seg = K_LIST_GET(path, i);
        AcceptSegment(V, seg);
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
    if (e->has_body) {
        AcceptBlock(V, e->body);
    } else {
        AcceptExpr(V, e->expr);
    }
}

static void AcceptFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    if (d->tag != NULL) AcceptType(V, d->tag);
}

static void AcceptTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    accept_decl_list(V, d->generics);
    AcceptExpr(V, d->rhs);
}

static void AcceptGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    PAW_UNUSED(V);
    PAW_UNUSED(d);
}

static void AcceptUseDecl(struct HirVisitor *V, struct HirUseDecl *d)
{
    AcceptPath(V, d->path);
}

static void AcceptVariantDecl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    accept_decl_list(V, d->fields);
}

static void AcceptAdtDecl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->fields);
    accept_type_list(V, d->monos);
}

static void AcceptImplDecl(struct HirVisitor *V, struct HirImplDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->methods);
    accept_decl_list(V, d->monos);
}

static void AcceptInstanceDecl(struct HirVisitor *V, struct HirInstanceDecl *d)
{
    accept_type_list(V, d->types);
}

static void AcceptVarDecl(struct HirVisitor *V, struct HirVarDecl *d)
{
    if (d->tag != NULL) AcceptType(V, d->tag);
    if (d->init != NULL) AcceptExpr(V, d->init);
}

static void AcceptReturnStmt(struct HirVisitor *V, struct HirReturnStmt *s)
{
    if (s->expr != NULL) AcceptExpr(V, s->expr);
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
    if (d->self != NULL) AcceptType(V, d->self);
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->params);
    if (d->body != NULL) AcceptBlock(V, d->body);
    accept_decl_list(V, d->monos);
}

static void AcceptIfStmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    AcceptExpr(V, s->cond);
    AcceptStmt(V, s->then_arm);
    if (s->else_arm != NULL) AcceptStmt(V, s->else_arm);
}

static void AcceptWhileStmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    AcceptExpr(V, s->cond);
    AcceptBlock(V, s->block);
}

static void AcceptLabelStmt(struct HirVisitor *V, struct HirLabelStmt *s)
{
    PAW_UNUSED(V);
    PAW_UNUSED(s);
}

static void AcceptForStmt(struct HirVisitor *V, struct HirForStmt *s)
{
    if (s->is_fornum) {
        AcceptExpr(V, s->fornum.begin);
        AcceptExpr(V, s->fornum.end);
        AcceptExpr(V, s->fornum.step);
    } else {
        AcceptExpr(V, s->forin.target);
    }
    AcceptBlock(V, s->block);
}

static void AcceptIndex(struct HirVisitor *V, struct HirIndex *e)
{
    AcceptExpr(V, e->target);
    if (e->first != NULL) AcceptExpr(V, e->first);
    if (e->second != NULL) AcceptExpr(V, e->second);
}

static void AcceptSelector(struct HirVisitor *V, struct HirSelector *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptDeclStmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    AcceptDecl(V, s->decl);
}

static void AcceptAdt(struct HirVisitor *V, struct HirAdt *t)
{
    accept_type_list(V, t->types);
}

static void AcceptFuncDef(struct HirVisitor *V, struct HirFuncDef *t)
{
    accept_type_list(V, t->types);
    accept_type_list(V, t->params);
    AcceptType(V, t->result);
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

static void AcceptPathType(struct HirVisitor *V, struct HirPathType *t)
{
    for (int i = 0; i < t->path->count; ++i) {
        struct HirSegment *seg = K_LIST_GET(t->path, i);
        accept_type_list(V, seg->types);
    }
}

static void AcceptUnknown(struct HirVisitor *V, struct HirUnknown *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptGeneric(struct HirVisitor *V, struct HirGeneric *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL ? (V)->PostVisit##name(V, x) : (void)0)
#define DEFINE_VISITOR_CASES(a, b) case kHir##a: { \
        struct Hir##a *x = HirGet##a(node); \
        if (VISITOR_CALL(V, a, x)) Accept##a(V, x); \
        VISITOR_POSTCALL(V, a, x); \
    } \
    break;

static void AcceptExpr(struct HirVisitor *V, struct HirExpr *node)
{
    paw_assert(node != NULL);
    if (!V->VisitExpr(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_EXPR_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitExpr(V, node);
}

static void AcceptDecl(struct HirVisitor *V, struct HirDecl *node)
{
    paw_assert(node != NULL);
    if (!V->VisitDecl(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_DECL_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitDecl(V, node);
}

static void AcceptStmt(struct HirVisitor *V, struct HirStmt *node)
{
    paw_assert(node != NULL);
    if (!V->VisitStmt(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_STMT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitStmt(V, node);
}

static void AcceptType(struct HirVisitor *V, struct HirType *node)
{
    paw_assert(node != NULL);
    if (!V->VisitType(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_TYPE_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitType(V, node);
}

#undef DEFINE_VISITOR_CASES
#undef VISITOR_POSTCALL
#undef VISITOR_CALL

static paw_Bool default_visit_expr(struct HirVisitor *V, struct HirExpr *node) {return PAW_TRUE;}
static paw_Bool default_visit_decl(struct HirVisitor *V, struct HirDecl *node) {return PAW_TRUE;}
static paw_Bool default_visit_stmt(struct HirVisitor *V, struct HirStmt *node) {return PAW_TRUE;}
static paw_Bool default_visit_type(struct HirVisitor *V, struct HirType *node) {return PAW_TRUE;}

static void default_post_visit_expr(struct HirVisitor *V, struct HirExpr *node) {}
static void default_post_visit_decl(struct HirVisitor *V, struct HirDecl *node) {}
static void default_post_visit_stmt(struct HirVisitor *V, struct HirStmt *node) {}
static void default_post_visit_type(struct HirVisitor *V, struct HirType *node) {}

void pawHir_visitor_init(struct HirVisitor *V, struct Compiler *C, void *ud)
{
    *V = (struct HirVisitor){
        .ud = ud,
        .C = C,

        .VisitExpr = default_visit_expr,
        .VisitDecl = default_visit_decl,
        .VisitStmt = default_visit_stmt,
        .VisitType = default_visit_type,

        .PostVisitExpr = default_post_visit_expr,
        .PostVisitDecl = default_post_visit_decl,
        .PostVisitStmt = default_post_visit_stmt,
        .PostVisitType = default_post_visit_type,
    };
}

#define DEFINE_VISITORS(name, T) \
    void pawHir_visit_##name(struct HirVisitor *V, struct Hir##T *node) { \
        paw_assert(node != NULL); \
        V->line = node->hdr.line; \
        Accept##T(V, node); \
    } \
    void pawHir_visit_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            pawHir_visit_##name(V, K_LIST_GET(list, i)); \
        } \
    }
DEFINE_VISITORS(expr, Expr)
DEFINE_VISITORS(decl, Decl)
DEFINE_VISITORS(stmt, Stmt)
DEFINE_VISITORS(type, Type)
#undef DEFINE_VISITORS

static struct HirType *FoldType(struct HirTypeFolder *, struct HirType *);

static struct HirTypeList *fold_type_list(struct HirTypeFolder *F, struct HirTypeList *list)
{
    if (list == NULL) return NULL;
    struct Compiler *C = F->C;
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = FoldType(F, K_LIST_GET(list, i));
        K_LIST_SET(list, i, type);
    }
    return list;
}

static struct HirType *FoldAdt(struct HirTypeFolder *F, struct HirAdt *t)
{
    t->types = F->FoldTypeList(F, t->types);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldFuncDef(struct HirTypeFolder *F, struct HirFuncDef *t)
{
    t->types = F->FoldTypeList(F, t->types);
    t->params = F->FoldTypeList(F, t->params);
    t->result = FoldType(F, t->result);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldFuncPtr(struct HirTypeFolder *F, struct HirFuncPtr *t)
{
    t->params = F->FoldTypeList(F, t->params);
    t->result = FoldType(F, t->result);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldTupleType(struct HirTypeFolder *F, struct HirTupleType *t)
{
    t->elems = F->FoldTypeList(F, t->elems);
    return HIR_CAST_TYPE(t);
}

static struct HirSegment *FoldSegment(struct HirTypeFolder *F, struct HirSegment *seg)
{
    F->FoldTypeList(F, seg->types);
    return seg;
}

static struct HirPath *FoldPath(struct HirTypeFolder *F, struct HirPath *path)
{
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment *seg = K_LIST_GET(path, i);
        F->FoldTypeList(F, seg->types);
    }
    return path;
}

static struct HirType *FoldPathType(struct HirTypeFolder *F, struct HirPathType *t)
{
    for (int i = 0; i < t->path->count; ++i) {
        struct HirSegment *seg = K_LIST_GET(t->path, i);
        F->FoldTypeList(F, seg->types);
    }
    return HIR_CAST_TYPE(t);
}

static void FoldExpr(struct HirVisitor *V, struct HirExpr *node)
{
    paw_assert(node != NULL);
    struct HirTypeFolder *F = V->ud;
    struct HirType *type = node->hdr.type;
    node->hdr.type = pawHir_fold_type(F, type);
}

static void FoldDecl(struct HirVisitor *V, struct HirDecl *node)
{
    paw_assert(node != NULL);
    struct HirTypeFolder *F = V->ud;
    struct HirType *type = node->hdr.type;
    node->hdr.type = pawHir_fold_type(F, type);
}

static struct HirType *FoldType(struct HirTypeFolder *F, struct HirType *node)
{
    if (node == NULL) return NULL;
    switch (HIR_KINDOF(node)) {
#define DEFINE_ACCEPT(a, _) case kHir##a: \
            if (F->Fold##a == NULL) return node; \
            return F->Fold##a(F, HirGet##a(node));
        HIR_TYPE_LIST(DEFINE_ACCEPT)
#undef DEFINE_ACCEPT
    }
}

void pawHir_type_folder_init(struct HirTypeFolder *F, struct Compiler *C, void *ud)
{
    *F = (struct HirTypeFolder){
        .ud = ud,
        .C = C,

        .FoldTypeList = fold_type_list,
        .FoldSegment = FoldSegment,
        .FoldPath = FoldPath,

        .FoldAdt = FoldAdt,
        .FoldFuncDef = FoldFuncDef,
        .FoldFuncPtr = FoldFuncPtr,
        .FoldTupleType = FoldTupleType,
        .FoldPathType = FoldPathType,
    };

    pawHir_visitor_init(&F->V, C, F);
    F->V.PostVisitExpr = FoldExpr;
    F->V.PostVisitDecl = FoldDecl;
}

struct HirType *pawHir_fold_type(struct HirTypeFolder *F, struct HirType *node)
{
    return FoldType(F, node);
}

struct HirTypeList *pawHir_fold_type_list(struct HirTypeFolder *F, struct HirTypeList *list)
{
    return list != NULL ? F->FoldTypeList(F, list) : NULL;
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
#undef DEFINE_FOLDERS


// HIR copying routines

struct HirStmt *copy_stmt(struct Compiler *C, struct HirStmt *node);
struct HirDecl *copy_decl(struct Compiler *C, struct HirDecl *node);
struct HirExpr *copy_expr(struct Compiler *C, struct HirExpr *node);

#define DEFINE_COPY_LIST(name, T) \
    static struct Hir##T##List *copy_##name##_list(struct Compiler *C, struct Hir##T##List *old_list) \
    { \
        if (old_list == NULL) return NULL; \
        struct Hir##T##List *new_list = pawHir_##name##_list_new(C); \
        for (int i = 0; i < old_list->count; ++i) { \
            struct Hir##T *elem = copy_##name(C, old_list->data[i]); \
            pawHir_##name##_list_push(C, new_list, elem); \
        } \
        return new_list; \
    }
DEFINE_COPY_LIST(decl, Decl)
DEFINE_COPY_LIST(expr, Expr)
DEFINE_COPY_LIST(stmt, Stmt)

static struct HirTypeList *copy_type_list(struct Compiler *C, struct HirTypeList *old_list)
{
    if (old_list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(C);
    for (int i = 0; i < old_list->count; ++i) {
        pawHir_type_list_push(C, new_list, K_LIST_GET(old_list, i));
    }
    return new_list;
}

static void CopyBlock(struct Compiler *C, struct HirBlock *s, struct HirBlock *r)
{
    r->stmts = copy_stmt_list(C, s->stmts);
}
#define COPY_BLOCK(C, b) CAST(struct HirBlock *, copy_stmt(C, HIR_CAST_STMT(b)))

static struct HirSegment *dup_segment(struct Compiler *C, struct HirSegment *src)
{
    struct HirSegment *dst = pawHir_segment_new(C);
    *dst = (struct HirSegment){
        .name = src->name,
        .types = copy_type_list(C, src->types),
        .modno = src->modno,
        .did = src->did,
    };
    return dst;
}

static struct HirPath *dup_path(struct Compiler *C, struct HirPath *e)
{
    struct HirPath *r = pawHir_path_new(C);
    for (int i = 0; i < e->count; ++i) {
        struct HirSegment *src = pawHir_path_get(e, i);
        struct HirSegment *dst = dup_segment(C, src);
        pawHir_path_push(C, r, dst);
    }
    return r;
}

static void CopyLogicalExpr(struct Compiler *C, struct HirLogicalExpr *e, struct HirLogicalExpr *r)
{
    r->lhs = copy_expr(C, e->lhs);
    r->rhs = copy_expr(C, e->rhs);
}

static void CopyFieldExpr(struct Compiler *C, struct HirFieldExpr *e, struct HirFieldExpr *r)
{
    r->fid = e->fid;
    if (e->fid < 0) {
        r->key = copy_expr(C, e->key);
    } else {
        r->name = e->name;
    }
    r->value = copy_expr(C, e->value);
}

static void CopyAssignExpr(struct Compiler *C, struct HirAssignExpr *e, struct HirAssignExpr *r)
{
    r->lhs = copy_expr(C, e->lhs);
    r->rhs = copy_expr(C, e->rhs);
}

static void CopyLiteralExpr(struct Compiler *C, struct HirLiteralExpr *e, struct HirLiteralExpr *r)
{
    r->lit_kind = e->lit_kind;
    switch (e->lit_kind) {
        case kHirLitBasic:
            r->basic = e->basic;
            break;
        case kHirLitTuple:
            r->tuple.elems = copy_expr_list(C, e->tuple.elems);
            break;
        case kHirLitContainer:
            r->cont.code = e->cont.code;
            r->cont.items = copy_expr_list(C, e->cont.items);
            break;
        default:
            paw_assert(e->lit_kind == kHirLitComposite);
            r->comp.path = dup_path(C, e->comp.path);
            r->comp.items = copy_expr_list(C, e->comp.items);
            break;
    }
}

static void CopyChainExpr(struct Compiler *C, struct HirChainExpr *e, struct HirChainExpr *r)
{
    r->target = copy_expr(C, e->target);
}

static void CopyUnOpExpr(struct Compiler *C, struct HirUnOpExpr *e, struct HirUnOpExpr *r)
{
    r->target = copy_expr(C, e->target);
    r->op = e->op;
}

static void CopyBinOpExpr(struct Compiler *C, struct HirBinOpExpr *e, struct HirBinOpExpr *r)
{
    r->lhs = copy_expr(C, e->lhs);
    r->rhs = copy_expr(C, e->rhs);
    r->op = e->op;
}

static void CopyExprStmt(struct Compiler *C, struct HirExprStmt *s, struct HirExprStmt *r)
{
    r->expr = copy_expr(C, s->expr);
}

static void CopyClosureExpr(struct Compiler *C, struct HirClosureExpr *e, struct HirClosureExpr *r)
{
    r->params = copy_decl_list(C, e->params);
    if (e->has_body) {
        r->body = COPY_BLOCK(C, e->body);
        r->has_body = PAW_TRUE;
    } else {
        r->expr = copy_expr(C, e->expr);
    }
}

static void CopyFieldDecl(struct Compiler *C, struct HirFieldDecl *d, struct HirFieldDecl *r)
{
    r->is_pub = d->is_pub;
    r->name = d->name;
    r->tag = d->tag;
}

static void CopyTypeDecl(struct Compiler *C, struct HirTypeDecl *d, struct HirTypeDecl *r)
{
    r->name = d->name;
    r->generics = copy_decl_list(C, d->generics);
    r->rhs = copy_expr(C, d->rhs);
}

static void CopyGenericDecl(struct Compiler *C, struct HirGenericDecl *d, struct HirGenericDecl *r)
{
    r->name = d->name;
}

static void CopyAdtDecl(struct Compiler *C, struct HirAdtDecl *d, struct HirAdtDecl *r)
{
    r->is_struct = d->is_struct;
    r->is_pub = d->is_pub;
    r->name = d->name;
    r->generics = copy_decl_list(C, d->generics);
    r->fields = copy_decl_list(C, d->fields);
    r->monos = copy_type_list(C, d->monos);
}

static void CopyImplDecl(struct Compiler *C, struct HirImplDecl *d, struct HirImplDecl *r)
{
    r->generics = copy_decl_list(C, d->generics);
    r->methods = copy_decl_list(C, d->methods);
    r->monos = copy_decl_list(C, d->monos);
}

static void CopyUseDecl(struct Compiler *C, struct HirUseDecl *d, struct HirUseDecl *r)
{
    r->path = dup_path(C, d->path);
}

static void CopyVariantDecl(struct Compiler *C, struct HirVariantDecl *d, struct HirVariantDecl *r)
{
    r->fields = copy_decl_list(C, d->fields);
    r->index = d->index;
    r->name = d->name;
}

static void CopyVarDecl(struct Compiler *C, struct HirVarDecl *d, struct HirVarDecl *r)
{
    r->is_const = d->is_const;
    r->name = d->name;
    r->tag = d->tag;
    if (d->init != NULL) r->init = copy_expr(C, d->init);
}

static void CopyReturnStmt(struct Compiler *C, struct HirReturnStmt *s, struct HirReturnStmt *r)
{
    r->expr = copy_expr(C, s->expr);
}

static void CopyConversionExpr(struct Compiler *C, struct HirConversionExpr *e, struct HirConversionExpr *r)
{
    r->arg = copy_expr(C, e->arg);
    r->to = e->to;
}

static void CopyCallExpr(struct Compiler *C, struct HirCallExpr *e, struct HirCallExpr *r)
{
    r->target = copy_expr(C, e->target);
    r->args = copy_expr_list(C, e->args);
}

static void CopyPathExpr(struct Compiler *C, struct HirPathExpr *e, struct HirPathExpr *r)
{
    r->path = dup_path(C, e->path);
}

static void CopyFuncDecl(struct Compiler *C, struct HirFuncDecl *d, struct HirFuncDecl *r)
{
    r->is_pub = d->is_pub;
    r->self = d->self;
    r->name = d->name;
    r->generics = copy_decl_list(C, d->generics);
    r->params = copy_decl_list(C, d->params);
    if (d->body != NULL) r->body = COPY_BLOCK(C, d->body);
    r->monos = copy_decl_list(C, d->monos);
    r->fn_kind = d->fn_kind;
}

static void CopyIfStmt(struct Compiler *C, struct HirIfStmt *s, struct HirIfStmt *r)
{
    r->cond = copy_expr(C, s->cond);
    r->then_arm = copy_stmt(C, s->then_arm);
    if (s->else_arm != NULL) r->else_arm = copy_stmt(C, s->else_arm);
}

static void CopyWhileStmt(struct Compiler *C, struct HirWhileStmt *s, struct HirWhileStmt *r)
{
    r->is_dowhile = s->is_dowhile;
    r->cond = copy_expr(C, s->cond);
    r->block = COPY_BLOCK(C, s->block);
}

static void CopyLabelStmt(struct Compiler *C, struct HirLabelStmt *s, struct HirLabelStmt *r)
{
    r->label = s->label;
}

static void CopyForStmt(struct Compiler *C, struct HirForStmt *s, struct HirForStmt *r)
{
    r->is_fornum = s->is_fornum;
    r->control = copy_decl(C, s->control);
    if (s->is_fornum) {
        r->fornum.begin = copy_expr(C, s->fornum.begin);
        r->fornum.end = copy_expr(C, s->fornum.end);
        r->fornum.step = copy_expr(C, s->fornum.step);
    } else {
        r->forin.target = copy_expr(C, s->forin.target);
    }
    r->block = COPY_BLOCK(C, s->block);
}

static void CopyInstanceDecl(struct Compiler *C, struct HirInstanceDecl *d, struct HirInstanceDecl *r)
{
    r->is_assoc = d->is_assoc;
    r->is_pub = d->is_pub;
    r->types = copy_type_list(C, d->types);
}

static void CopyIndex(struct Compiler *C, struct HirIndex *e, struct HirIndex *r)
{
    r->target = copy_expr(C, e->target);
    if (e->first != NULL) r->first = copy_expr(C, e->first);
    if (e->second != NULL) r->second = copy_expr(C, e->second);
    r->is_slice = e->is_slice;
}

static void CopySelector(struct Compiler *C, struct HirSelector *e, struct HirSelector *r)
{
    r->is_index = e->is_index;
    r->target = copy_expr(C, e->target);
    r->name = e->name;
}

static void CopyDeclStmt(struct Compiler *C, struct HirDeclStmt *s, struct HirDeclStmt *r)
{
    r->decl = copy_decl(C, s->decl);
}

struct HirExpr *copy_expr(struct Compiler *C, struct HirExpr *node)
{
    struct HirExpr *result = pawHir_new_expr(
            C, node->hdr.line, HIR_KINDOF(node));
    result->hdr = node->hdr;
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
            Copy##a(C, HirGet##a(node), HirGet##a(result)); \
            break;
        HIR_EXPR_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
    return result;
}

struct HirDecl *copy_decl(struct Compiler *C, struct HirDecl *node)
{
    struct HirDecl *result = pawHir_new_decl(
            C, node->hdr.line, HIR_KINDOF(node));
    result->hdr = node->hdr;
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
            Copy##a(C, HirGet##a(node), HirGet##a(result)); \
            break;
        HIR_DECL_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
    return result;
}

struct HirStmt *copy_stmt(struct Compiler *C, struct HirStmt *node)
{
    struct HirStmt *result = pawHir_new_stmt(
            C, node->hdr.line, HIR_KINDOF(node));
    result->hdr = node->hdr;
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
            Copy##a(C, HirGet##a(node), HirGet##a(result)); \
            break;
        HIR_STMT_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
    return result;
}

struct Subst {
    struct Subst *outer;
    struct HirTypeList *before;
    struct HirTypeList *after;
    struct Stenciler *S;
};

struct Expander {
    struct HirTypeFolder F;
    struct Subst *subst;
    struct Compiler *C;
    struct Hir *hir;
    paw_Env *P;
    int nexpand;
};

static struct HirTypeList *expand_type_list(struct HirTypeFolder *F, struct HirTypeList *list)
{
    struct Expander *E = F->ud;
    if (list == NULL) return NULL;
    struct HirTypeList *copy = pawHir_type_list_new(E->C);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = pawHir_fold_type(F, list->data[i]);
        pawHir_type_list_push(E->C, copy, type);
    }
    return copy;
}

static struct HirType *expand_tuple(struct HirTypeFolder *F, struct HirTupleType *t)
{
    struct Expander *E = F->ud;
    struct HirType *r = pawHir_new_type(E->C, t->line, kHirTupleType);
    r->tuple.elems = pawHir_fold_type_list(F, t->elems);
    return r;
}

static struct HirType *expand_func_ptr(struct HirTypeFolder *F, struct HirFuncPtr *t)
{
    struct Expander *E = F->ud;
    struct HirType *r = pawHir_new_type(E->C, t->line, kHirFuncPtr);
    r->fptr.params = pawHir_fold_type_list(F, t->params);
    r->fptr.result = pawHir_fold_type(F, t->result);
    return r;
}

static struct HirType *expand_func_def(struct HirTypeFolder *F, struct HirFuncDef *t)
{
    struct Expander *E = F->ud;
    struct HirDecl *base = pawHir_get_decl(E->C, t->did);
    if (t->types == NULL) {
        struct HirType *result = pawHir_new_type(E->C, t->line, kHirFuncDef);
        struct HirFuncDef *r = HirGetFuncDef(result);
        r->params = pawHir_fold_type_list(F, t->params);
        r->result = pawHir_fold_type(F, t->result);
        r->did = t->did;
        r->modno = t->modno;
        return result;
    }
    struct HirFuncDecl *func = HirGetFuncDecl(base);
    struct HirTypeList *types = pawHir_fold_type_list(F, t->types);
    return pawP_instantiate(F->C, base, types);
}

static struct HirType *expand_adt(struct HirTypeFolder *F, struct HirAdt *t)
{
    struct Expander *E = F->ud;
    if (t->did <= PAW_TSTR) return HIR_CAST_TYPE(t);
    struct HirTypeList *types = pawHir_fold_type_list(F, t->types);
    struct HirDecl *base = pawHir_get_decl(E->C, t->did);
    return pawP_instantiate(F->C, base, types);
}

static struct HirType *expand_path_type(struct HirTypeFolder *F, struct HirPathType *t)
{
    struct Expander *E = F->ud;
    struct HirSegment *seg = K_LIST_GET(t->path, 0);
    // paw_Type equals DefId for builtins
    if (seg->did <= PAW_TSTR) return HIR_CAST_TYPE(t);
    struct HirTypeList *types = pawHir_fold_type_list(F, seg->types);
    struct HirDecl *base = pawHir_get_decl(E->C, seg->did);
    return pawP_instantiate(E->C, base, types);
}

static struct HirType *expand_generic(struct HirTypeFolder *F, struct HirGeneric *t)
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

static void do_expand_aux(struct HirTypeFolder *F, struct HirFuncDecl *base, struct HirDecl *inst, struct Subst *subst)
{
    struct Expander *E = F->ud;
    E->subst = subst;

    inst->func.kind = kHirFuncDecl;
    struct HirFuncDecl *r = HirGetFuncDecl(inst);
    r->is_pub = base->is_pub;
    r->fn_kind = base->fn_kind;
    r->name = base->name;
    r->generics = NULL;

    // Copy the original polymorphic function nodes. Then, replace generics with
    // concrete types and create further instantiations to be expanded next round.
    r->self = pawHir_fold_type(F, base->self);
    r->params = copy_decl_list(E->C, base->params);
    pawHir_fold_decl_list(F, r->params);

    if (base->body != NULL) {
        r->body = COPY_BLOCK(E->C, base->body);
        pawHir_fold_block(F, r->body);
    }

    E->subst = subst->outer;
}

#define MAX_EXPANSION_DEPTH 128

static void enter_binder(struct Expander *E, struct HirDecl *inst)
{
    if (E->C->nbinders >= MAX_EXPANSION_DEPTH) {
        pawHir_print_type(E->C, HIR_TYPEOF(inst));
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

static void do_expand(struct HirTypeFolder *F, struct HirFuncDecl *base, struct HirDecl *inst)
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
}

static struct HirDecl *find_decl(struct Expander *E, struct HirType *type)
{
    DefId did;
    switch (HIR_KINDOF(type)) {
        case kHirPathType:
            did = hir_adt_did(type);
            break;
        case kHirFuncDef:
            did = HirGetFuncDef(type)->did;
            break;
        default:
            did = HirGetGeneric(type)->did;
    }
    return pawHir_get_decl(E->C, did);
}

static void expand_path(struct HirVisitor *V, struct HirPath *path)
{
    struct HirTypeFolder *F = V->ud;
    struct Expander *E = F->ud;
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment *ps = pawHir_path_get(path, i);
        if (ps->types != NULL) {
            ps->types = pawHir_fold_type_list(F, ps->types);
            struct HirDecl *base = pawHir_get_decl(E->C, ps->did);
            struct HirType *inst = pawP_instantiate(E->C, base, ps->types);
            ps->did = HIR_TYPE_DID(inst);
        }
    }
}

static paw_Bool expand_adt_decl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    return PAW_FALSE;
}

static paw_Bool expand_func_decl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    struct HirTypeFolder *F = V->ud;
    struct Expander *E = F->ud;
    const int n = d->monos ? d->monos->count : 0;
    for (int i = 0; i < n; ++i) {
        struct HirDecl *decl = K_LIST_GET(d->monos, i);
        if (HirIsInstanceDecl(decl)) {
            do_expand(F, d, decl);
            ++E->nexpand;
        }
    }
    return PAW_FALSE;
}

static paw_Bool expand_selector(struct HirVisitor *V, struct HirSelector *e)
{
    struct HirTypeFolder *F = V->ud;
    struct Expander *E = F->ud;
    pawHir_fold_expr(F, e->target);

    struct HirType *self_type = HIR_TYPEOF(e->target);
    if (HirIsTupleType(self_type)) {
        struct HirTupleType *tuple = HirGetTupleType(self_type);
        e->type = K_LIST_GET(tuple->elems, e->index);
    } else {
        struct HirDecl *field = pawP_find_field(E->C, self_type, e->name);
        e->type = pawP_instantiate_field(E->C, self_type, field);
    }
    return PAW_FALSE;
}

static paw_Bool expand_call_expr(struct HirVisitor *V, struct HirCallExpr *e)
{
    struct HirTypeFolder *F = V->ud;
    struct Expander *E = F->ud;

    pawHir_fold_expr(F, e->target);
    pawHir_fold_expr_list(F, e->args);
    return PAW_FALSE;
}

static struct HirTypeList *collect_decl_types(struct Hir *hir, struct HirDeclList *list)
{
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(hir->C);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = HIR_TYPEOF(list->data[i]);
        pawHir_type_list_push(hir->C, new_list, type);
    }
    return new_list;
}

static paw_Bool expand_impl_decl(struct HirVisitor *V, struct HirImplDecl *d)
{
    struct HirTypeFolder *F = V->ud;
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
    return PAW_FALSE;
}

static void expand_decl_type(struct HirVisitor *V, struct HirDecl *decl)
{
    decl->hdr.type = pawHir_fold_type(V->ud, decl->hdr.type);
}

static void expand_expr_type(struct HirVisitor *V, struct HirExpr *expr)
{
    expr->hdr.type = pawHir_fold_type(V->ud, expr->hdr.type);
}

static void setup_expander(struct Compiler *C, struct Hir *hir, struct Expander *E)
{
    *E = (struct Expander){
        .P = ENV(C),
        .hir = hir,
        .C = C,
    };
    pawHir_type_folder_init(&E->F, C, E);

    E->F.FoldAdt = expand_adt;
    E->F.FoldPathType = expand_path_type;
    E->F.FoldTupleType = expand_tuple;
    E->F.FoldFuncDef = expand_func_def;
    E->F.FoldFuncPtr = expand_func_ptr;
    E->F.FoldGeneric = expand_generic;
    E->F.FoldTypeList = expand_type_list;

    E->F.V.VisitAdtDecl = expand_adt_decl;
    E->F.V.VisitImplDecl = expand_impl_decl;
    E->F.V.VisitFuncDecl = expand_func_decl;
    E->F.V.VisitCallExpr = expand_call_expr;
    E->F.V.VisitSelector = expand_selector;
    E->F.V.VisitPath = expand_path;

    E->F.V.PostVisitDecl = expand_decl_type;
    E->F.V.PostVisitExpr = expand_expr_type;
}

int pawHir_expand_bodies(struct Hir *hir)
{
    struct Expander E;
    setup_expander(hir->C, hir, &E);
    struct HirVisitor *V = &E.F.V;
    int total = 0;

    do {
        E.nexpand = 0;
        pawHir_visit_decl_list(V, hir->items);
        total += E.nexpand;
    } while (E.nexpand > 0);

    return total;
}

struct DefState {
    struct DefState *outer;
    struct HirDecl *decl;
    struct Def *def;
};

struct HirType *pawHir_attach_type(struct Compiler *C, DeclId did, enum HirTypeKind kind, int line)
{
    struct DynamicMem *dm = C->dm;
    struct HirType *type = pawHir_new_type(C, line, kind);
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

struct HirTypeList *pawHir_collect_generics(struct Compiler *C, struct HirDeclList *generics)
{
    struct HirTypeList *types = pawHir_type_list_new(C);
    for (int i = 0; i < generics->count; ++i) {
        struct HirGenericDecl *d = HirGetGenericDecl(generics->data[i]);
        struct HirType *type = pawHir_attach_type(C, d->did, kHirGeneric, d->line);
        pawHir_type_list_push(C, types, type);
        struct HirGeneric *t = HirGetGeneric(type);
        t->name = d->name;
    }
    return types;
}

struct HirTypeList *pawHir_collect_fields(struct Compiler *C, struct HirDeclList *fields)
{
    if (fields == NULL) return NULL;
    struct HirTypeList *types = pawHir_type_list_new(C);
    for (int i = 0; i < fields->count; ++i) {
        struct HirFieldDecl *d = HirGetFieldDecl(fields->data[i]);
        pawHir_type_list_push(C, types, d->type);
    }
    return types;
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
            struct HirDecl *decl = pawHir_get_decl(P->C, adt->did);
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

void pawHir_print_type(struct Compiler *C, struct HirType *type)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_type(&(struct Printer){
                .P = ENV(C),
                .buf = &buf,
                .C = C,
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

static void dump_segment(struct Printer *P, struct HirSegment *seg)
{
    DUMP_STRING(P, seg->name->text);
    if (seg->types != NULL) {
        DUMP_LITERAL(P, "<");
        for (int j = 0; j < seg->types->count; ++j) {
            dump_type(P, seg->types->data[j]);
            if (j < seg->types->count - 1) {
                DUMP_LITERAL(P, ", ");
            }
        }
        DUMP_LITERAL(P, ">");
    }
}

static void dump_path(struct Printer *P, struct HirPath *p)
{
    for (int i = 0; i < p->count; ++i) {
        if (i != 0) DUMP_LITERAL(P, "::");
        dump_segment(P, p->data[i]);
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
            dump_type_list(P, d->adt.monos, "monos");
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
            DUMP_FMT(P, "is_pub: %d\n", d->inst.is_pub);
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

void pawHir_dump_path(struct Compiler *C, struct HirPath *path)
{
    dump_path(&(struct Printer){
                .P = ENV(C),
                .C = C,
            }, path);
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
        struct HirDecl *decl = pawHir_get_decl(C, i);
        const char *name = decl->hdr.name != NULL
                ? decl->hdr.name->text : "(null)";
        if (HirIsUseDecl(decl)) {
            paw_push_string(P, "(use)");
        } else {
            pawHir_print_type(C, decl->hdr.type);
        }
        pawL_add_fstring(P, &buf, "%d\t%s\t%s\n", i, name, paw_string(P, -1));
        paw_pop(P, 1);
    }
    pawL_add_char(P, &buf, '\0');
    pawL_push_result(P, &buf);
}

#endif
