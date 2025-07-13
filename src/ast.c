// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ast.h"
#include "compile.h"
#include "map.h"
#include "mem.h"
#include "parse.h"

#define FIRST_ARENA_SIZE 4096
#define LARGE_ARENA_MIN 32

struct Ast *pawAst_new(struct Compiler *C)
{
    paw_Env *P = ENV(C);
    struct Ast *ast = P_ALLOC(C, NULL, 0, sizeof(struct Ast));
    *ast = (struct Ast){
        .pool = C->ast_pool,
        .C = C,
        .P = P,
    };
    ast->modules = AstDeclList_new(ast);
    ast->nodes = AstNodeMap_new(ast);
    return ast;
}

void pawAst_free(struct Ast *ast)
{
    struct Compiler *C = ast->C;
    pawP_pool_free(C, C->ast_pool);
    C->ast_pool = NULL;
}

#define DEFINE_NODE_CONSTRUCTOR(name, T)                   \
    struct T *pawAst_new_##name(struct Ast *ast)           \
    {                                                      \
        return P_ALLOC(ast->C, NULL, 0, sizeof(struct T)); \
    }
DEFINE_NODE_CONSTRUCTOR(decl, AstDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, AstStmt)
DEFINE_NODE_CONSTRUCTOR(type, AstType)
DEFINE_NODE_CONSTRUCTOR(expr, AstExpr)
DEFINE_NODE_CONSTRUCTOR(pat, AstPat)


#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

static void AcceptType(struct AstVisitor *V, struct AstType *node);
static void AcceptExpr(struct AstVisitor *V, struct AstExpr *node);
static void AcceptDecl(struct AstVisitor *V, struct AstDecl *node);
static void AcceptStmt(struct AstVisitor *V, struct AstStmt *node);
static void AcceptPat(struct AstVisitor *V, struct AstPat *node);

#define DEFINE_LIST_ACCEPTOR(name, T)                                                 \
    static void accept_##name##_list(struct AstVisitor *V, struct Ast##T##List *list) \
    {                                                                                 \
        if (list == NULL)                                                             \
            return;                                                                   \
        struct Ast##T *const *pnode;                                                  \
        K_LIST_FOREACH(list, pnode) {                                                 \
            Accept##T(V, *pnode);                                                     \
        }                                                                             \
    }
DEFINE_LIST_ACCEPTOR(decl, Decl)
DEFINE_LIST_ACCEPTOR(expr, Expr)
DEFINE_LIST_ACCEPTOR(stmt, Stmt)
DEFINE_LIST_ACCEPTOR(type, Type)
DEFINE_LIST_ACCEPTOR(pat, Pat)

static void AcceptBlock(struct AstVisitor *V, struct AstBlock *e)
{
    accept_stmt_list(V, e->stmts);
    if (e->result != NULL) AcceptExpr(V, e->result);
}

static void AcceptParenExpr(struct AstVisitor *V, struct AstParenExpr *e)
{
    AcceptExpr(V, e->expr);
}

static void AcceptStringExpr(struct AstVisitor *V, struct AstStringExpr *e)
{
    struct AstStringPart const *part;
    K_LIST_FOREACH (e->parts, part) {
        if (!part->is_str) AcceptExpr(V, part->expr);
    }
}

static void AcceptRangeExpr(struct AstVisitor *V, struct AstRangeExpr *e)
{
    if (e->lhs != NULL) AcceptExpr(V, e->lhs);
    if (e->rhs != NULL) AcceptExpr(V, e->rhs);
}

static void AcceptOpAssignExpr(struct AstVisitor *V, struct AstOpAssignExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptIfExpr(struct AstVisitor *V, struct AstIfExpr *e)
{
    AcceptExpr(V, e->cond);
    AcceptExpr(V, e->then_arm);
    if (e->else_arm != NULL)
        AcceptExpr(V, e->else_arm);
}

static void AcceptForExpr(struct AstVisitor *V, struct AstForExpr *e)
{
    AcceptPat(V, e->pat);
    AcceptExpr(V, e->target);
    AcceptExpr(V, e->block);
}

static void AcceptWhileExpr(struct AstVisitor *V, struct AstWhileExpr *e)
{
    AcceptExpr(V, e->cond);
    AcceptExpr(V, e->block);
}

static void AcceptLogicalExpr(struct AstVisitor *V, struct AstLogicalExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptFieldExpr(struct AstVisitor *V, struct AstFieldExpr *e)
{
    if (e->fid < 0)
        AcceptExpr(V, e->key);
    AcceptExpr(V, e->value);
}

static void AcceptAssignExpr(struct AstVisitor *V, struct AstAssignExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptMatchArm(struct AstVisitor *V, struct AstMatchArm *e)
{
    AcceptPat(V, e->pat);
    if (e->guard != NULL)
        AcceptExpr(V, e->guard);
    AcceptExpr(V, e->result);
}

static void AcceptMatchExpr(struct AstVisitor *V, struct AstMatchExpr *e)
{
    AcceptExpr(V, e->target);
    accept_expr_list(V, e->arms);
}

static void AcceptSegment(struct AstVisitor *V, struct AstSegment *seg)
{
    if (seg->types != NULL) {
        struct AstType *const *ptype;
        K_LIST_FOREACH (seg->types, ptype) {
            AcceptType(V, *ptype);
        }
    }
    VISITOR_CALL(V, Segment, seg);
}

static void AcceptPath(struct AstVisitor *V, struct AstPath *path)
{
    struct AstSegment *pseg;
    K_LIST_FOREACH (path->segments, pseg) {
        AcceptSegment(V, pseg);
    }
    VISITOR_CALL(V, Path, path);
}

static void AcceptLiteralExpr(struct AstVisitor *V, struct AstLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kAstCompositeLit:
            AcceptPath(V, &e->comp.path);
            accept_expr_list(V, e->comp.items);
            break;
        case kAstContainerLit:
            accept_expr_list(V, e->cont.items);
            break;
        case kAstTupleLit:
            accept_expr_list(V, e->tuple.elems);
            break;
        case kAstBasicLit:
            break;
    }
}

static void AcceptChainExpr(struct AstVisitor *V, struct AstChainExpr *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptUnOpExpr(struct AstVisitor *V, struct AstUnOpExpr *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptBinOpExpr(struct AstVisitor *V, struct AstBinOpExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptLetStmt(struct AstVisitor *V, struct AstLetStmt *s)
{
    if (s->tag != NULL)
        AcceptType(V, s->tag);
    if (s->init != NULL)
        AcceptExpr(V, s->init);
    if (s->pat != NULL)
        AcceptPat(V, s->pat);
}

static void AcceptExprStmt(struct AstVisitor *V, struct AstExprStmt *s)
{
    AcceptExpr(V, s->expr);
}

static void AcceptClosureExpr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    accept_decl_list(V, e->params);
    if (e->result != NULL)
        AcceptType(V, e->result);
    AcceptExpr(V, e->expr);
}

static void AcceptModuleDecl(struct AstVisitor *V, struct AstModuleDecl *d)
{
    accept_decl_list(V, d->items);
}

static void AcceptFieldDecl(struct AstVisitor *V, struct AstFieldDecl *d)
{
    if (d->tag != NULL)
        AcceptType(V, d->tag);
}

static void AcceptParamDecl(struct AstVisitor *V, struct AstParamDecl *d)
{
    if (d->tag != NULL)
        AcceptType(V, d->tag);
}

static void AcceptTypeDecl(struct AstVisitor *V, struct AstTypeDecl *d)
{
    accept_decl_list(V, d->generics);
    AcceptType(V, d->rhs);
}

static void AcceptGenericDecl(struct AstVisitor *V, struct AstGenericDecl *d)
{
    if (d->bounds != NULL) {
        struct AstGenericBound *pbound;
        K_LIST_FOREACH (d->bounds, pbound) {
            AcceptPath(V, &pbound->path);
        }
    }
}

static void AcceptVariantDecl(struct AstVisitor *V, struct AstVariantDecl *d)
{
    accept_decl_list(V, d->fields);
}

static void AcceptAdtDecl(struct AstVisitor *V, struct AstAdtDecl *d)
{
    accept_type_list(V, d->traits);
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->variants);
    accept_decl_list(V, d->methods);
}

static void AcceptTraitDecl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->methods);
}

static void AcceptConstDecl(struct AstVisitor *V, struct AstConstDecl *d)
{
    AcceptType(V, d->tag);
    if (d->init != NULL)
        AcceptExpr(V, d->init);
}

static void AcceptReturnExpr(struct AstVisitor *V, struct AstReturnExpr *s)
{
    if (s->expr != NULL)
        AcceptExpr(V, s->expr);
}

static void AcceptCallExpr(struct AstVisitor *V, struct AstCallExpr *e)
{
    AcceptExpr(V, e->target);
    accept_expr_list(V, e->args);
}

static void AcceptConversionExpr(struct AstVisitor *V, struct AstConversionExpr *e)
{
    AcceptExpr(V, e->arg);
}

static void AcceptPathExpr(struct AstVisitor *V, struct AstPathExpr *e)
{
    AcceptPath(V, &e->path);
}

static void AcceptFnDecl(struct AstVisitor *V, struct AstFnDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->params);
    AcceptType(V, d->result);
    if (d->body != NULL)
        AcceptExpr(V, d->body);
}

static void AcceptUseDecl(struct AstVisitor *V, struct AstUseDecl *d)
{
    AcceptPath(V, &d->path);
}

static void AcceptLoopExpr(struct AstVisitor *V, struct AstLoopExpr *s)
{
    AcceptExpr(V, s->block);
}

static void AcceptJumpExpr(struct AstVisitor *V, struct AstJumpExpr *s)
{
    PAW_UNUSED(V);
    PAW_UNUSED(s);
}

static void AcceptIndex(struct AstVisitor *V, struct AstIndex *e)
{
    AcceptExpr(V, e->target);
    AcceptExpr(V, e->index);
}

static void AcceptSelector(struct AstVisitor *V, struct AstSelector *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptDeclStmt(struct AstVisitor *V, struct AstDeclStmt *s)
{
    AcceptDecl(V, s->decl);
}

static void AcceptFnType(struct AstVisitor *V, struct AstFnType *t)
{
    accept_type_list(V, t->params);
    AcceptType(V, t->result);
}

static void AcceptTupleType(struct AstVisitor *V, struct AstTupleType *t)
{
    accept_type_list(V, t->types);
}

static void AcceptContainerType(struct AstVisitor *V, struct AstContainerType *t)
{
    AcceptType(V, t->first);
    if (t->second != NULL)
        AcceptType(V, t->second);
}

static void AcceptNeverType(struct AstVisitor *V, struct AstNeverType *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptInferType(struct AstVisitor *V, struct AstInferType *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptPathType(struct AstVisitor *V, struct AstPathType *t)
{
    struct AstSegment const *pseg;
    K_LIST_FOREACH (t->path.segments, pseg) {
        accept_type_list(V, pseg->types);
    }
}

static void AcceptOrPat(struct AstVisitor *V, struct AstOrPat *e)
{
    accept_pat_list(V, e->pats);
}

static void AcceptFieldPat(struct AstVisitor *V, struct AstFieldPat *p)
{
    AcceptPat(V, p->pat);
}

static void AcceptStructPat(struct AstVisitor *V, struct AstStructPat *p)
{
    AcceptPath(V, &p->path);
    accept_pat_list(V, p->fields);
}

static void AcceptVariantPat(struct AstVisitor *V, struct AstVariantPat *p)
{
    AcceptPath(V, &p->path);
    accept_pat_list(V, p->fields);
}

static void AcceptTuplePat(struct AstVisitor *V, struct AstTuplePat *p)
{
    accept_pat_list(V, p->elems);
}

static void AcceptIdentPat(struct AstVisitor *V, struct AstIdentPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

static void AcceptPathPat(struct AstVisitor *V, struct AstPathPat *p)
{
    AcceptPath(V, &p->path);
}

static void AcceptLiteralPat(struct AstVisitor *V, struct AstLiteralPat *p)
{
    AcceptExpr(V, p->expr);
}

static void AcceptWildcardPat(struct AstVisitor *V, struct AstWildcardPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL \
        ? (V)->PostVisit##name(V, x) : (void)0)
#define DEFINE_VISITOR_CASES(X)             \
    case kAst##X: {                         \
        struct Ast##X *x = AstGet##X(node); \
        if (VISITOR_CALL(V, X, x))          \
            Accept##X(V, x);                \
        VISITOR_POSTCALL(V, X, x);          \
        break;                              \
    }

static void AcceptExpr(struct AstVisitor *V, struct AstExpr *node)
{
    paw_assert(node != NULL);
    if (!V->VisitExpr(V, node))
        return;

    switch (AST_KINDOF(node)) {
        AST_EXPR_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitExpr(V, node);
}

static void AcceptDecl(struct AstVisitor *V, struct AstDecl *node)
{
    paw_assert(node != NULL);
    if (!V->VisitDecl(V, node))
        return;

    switch (AST_KINDOF(node)) {
        AST_DECL_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitDecl(V, node);
}

static void AcceptStmt(struct AstVisitor *V, struct AstStmt *node)
{
    paw_assert(node != NULL);
    if (!V->VisitStmt(V, node))
        return;

    switch (AST_KINDOF(node)) {
        AST_STMT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitStmt(V, node);
}

static void AcceptType(struct AstVisitor *V, struct AstType *node)
{
    paw_assert(node != NULL);
    if (!V->VisitType(V, node))
        return;

    switch (AST_KINDOF(node)) {
        AST_TYPE_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitType(V, node);
}

static void AcceptPat(struct AstVisitor *V, struct AstPat *node)
{
    paw_assert(node != NULL);
    if (!V->VisitPat(V, node))
        return;

    switch (AST_KINDOF(node)) {
        AST_PAT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitPat(V, node);
}

#undef DEFINE_VISITOR_CASES
#undef VISITOR_POSTCALL
#undef VISITOR_CALL

static paw_Bool default_visit_expr(struct AstVisitor *V, struct AstExpr *node) { return PAW_TRUE; }
static paw_Bool default_visit_decl(struct AstVisitor *V, struct AstDecl *node) { return PAW_TRUE; }
static paw_Bool default_visit_stmt(struct AstVisitor *V, struct AstStmt *node) { return PAW_TRUE; }
static paw_Bool default_visit_type(struct AstVisitor *V, struct AstType *node) { return PAW_TRUE; }
static paw_Bool default_visit_pat(struct AstVisitor *V, struct AstPat *node) { return PAW_TRUE; }

static void default_post_visit_expr(struct AstVisitor *V, struct AstExpr *node) {}
static void default_post_visit_decl(struct AstVisitor *V, struct AstDecl *node) {}
static void default_post_visit_stmt(struct AstVisitor *V, struct AstStmt *node) {}
static void default_post_visit_type(struct AstVisitor *V, struct AstType *node) {}
static void default_post_visit_pat(struct AstVisitor *V, struct AstPat *node) {}

void pawAst_visitor_init(struct AstVisitor *V, struct Ast *ast, void *ud)
{
    *V = (struct AstVisitor){
        .ast = ast,
        .ud = ud,

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
    void pawAst_visit_##name(struct AstVisitor *V, struct Ast##T *node)              \
    {                                                                                \
        if (node != NULL) Accept##T(V, node);                                        \
    }                                                                                \
    void pawAst_visit_##name##_list(struct AstVisitor *V, struct Ast##T##List *list) \
    {                                                                                \
        if (list == NULL)                                                            \
            return;                                                                  \
        struct Ast##T *const *pt;                                                    \
        K_LIST_FOREACH (list, pt) {                                                  \
            pawAst_visit_##name(V, *pt);                                             \
        }                                                                            \
    }
DEFINE_VISITORS(expr, Expr)
DEFINE_VISITORS(decl, Decl)
DEFINE_VISITORS(stmt, Stmt)
DEFINE_VISITORS(type, Type)
DEFINE_VISITORS(pat, Pat)
#undef DEFINE_VISITORS


typedef struct Printer {
    struct Ast *ast;
    Buffer *buf;
    paw_Env *P;
    int indent;
} Printer;

#define DUMP_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define DUMP_STRING(P, str) pawL_add_string(ENV(P), (P)->buf, str)
#define DUMP_FSTRING(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)

static void indent_line(Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        DUMP_LITERAL(P, "  ");
    }
}

#define DUMP_FMT(P, ...) (indent_line(P), pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__))
#define DUMP_MSG(P, msg) (indent_line(P), pawL_add_string(ENV(P), (P)->buf, msg))
#define DUMP_BLOCK(P, b) CHECK_EXP(AstIsBlock(AST_CAST_EXPR(b)), dump_expr(P, AST_CAST_EXPR(b)))
#define DUMP_NAME(P, s) DUMP_FMT(P, "name: %s\n", s ? s->text : "(null)")

static void dump_type(Printer *P, struct AstType *t);
static void dump_expr(Printer *P, struct AstExpr *e);
static void dump_decl(Printer *P, struct AstDecl *d);
static void dump_stmt(Printer *P, struct AstStmt *s);

#define DEFINE_LIST_PRINTER(name, T)                                 \
    static void dump_##name##_list(Printer *P, struct T##List *list, \
                                   const char *name)                 \
    {                                                                \
        DUMP_FMT(P, "%s: {\n", name);                                \
        ++P->indent;                                                 \
        if (list != NULL) {                                          \
            DUMP_MSG(P, "" /* indent */);                            \
            for (int i = 0; i < list->count; ++i) {                  \
                dump_##name(P, list->data[i]);                       \
            }                                                        \
        }                                                            \
        --P->indent;                                                 \
        DUMP_MSG(P, "}\n");                                          \
    }
DEFINE_LIST_PRINTER(expr, AstExpr)
DEFINE_LIST_PRINTER(type, AstType)
DEFINE_LIST_PRINTER(decl, AstDecl)
DEFINE_LIST_PRINTER(stmt, AstStmt)

#define DEFINE_KIND_PRINTER(name, T)                                   \
    static int print_##name##_kind(Printer *P, void *node)             \
    {                                                                  \
        if (node != NULL) {                                            \
            struct T *typed = node;                                    \
            DUMP_FSTRING(P, "%s {\n", k##T##Names[AST_KINDOF(typed)]); \
            return 0;                                                  \
        }                                                              \
        return -1;                                                     \
    }
DEFINE_KIND_PRINTER(expr, AstExpr)
DEFINE_KIND_PRINTER(type, AstType)
DEFINE_KIND_PRINTER(decl, AstDecl)
DEFINE_KIND_PRINTER(stmt, AstStmt)

static void add_span(Printer *P, struct SourceSpan span)
{
    pawSrc_add_location(P->P, P->buf, span.start);
    DUMP_LITERAL(P, "-");
    pawSrc_add_location(P->P, P->buf, span.end);
}

static void dump_path(Printer *P, struct AstPath *p)
{
    struct AstSegment *pseg;
    K_LIST_FOREACH(p->segments, pseg) {
        DUMP_NAME(P, pseg->ident.name);
        dump_type_list(P, pseg->types, "types");
    }
}

static void dump_decl(Printer *P, struct AstDecl *decl)
{
    if (print_decl_kind(P, decl)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    add_span(P, decl->hdr.span);
    switch (AST_KINDOF(decl)) {
        case kAstModuleDecl: {
            struct AstModuleDecl *d = AstGetModuleDecl(decl);
            dump_decl_list(P, d->items, "items");
            break;
        }
        case kAstTraitDecl: {
            struct AstTraitDecl *d = AstGetTraitDecl(decl);
            DUMP_FMT(P, "name: %s\n", d->ident.name->text);
            DUMP_FMT(P, "is_pub: %d\n", d->is_pub);
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->methods, "methods");
            break;
        }
        case kAstUseDecl: {
            struct AstUseDecl *d = AstGetUseDecl(decl);
            DUMP_FMT(P, "path: %s\n", pawAst_print_path(P->ast, d->path));
            if (d->use_kind == AST_USE_GLOB) {
                DUMP_LITERAL(P, "kind: GLOB");
            } else if (d->use_kind == AST_USE_ALIAS) {
                DUMP_LITERAL(P, "kind: ALIAS");
                DUMP_FMT(P, "as: %s\n", d->as.name->text);
            } else {
                DUMP_LITERAL(P, "kind: NORMAL");
            }
            break;
        }
        case kAstFnDecl: {
            struct AstFnDecl *d = AstGetFnDecl(decl);
            DUMP_FMT(P, "name: %s\n", d->ident.name->text);
            DUMP_FMT(P, "is_pub: %d\n", d->is_pub);
            DUMP_FMT(P, "is_method: %d\n", d->is_method);
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->params, "params");
            DUMP_MSG(P, "result: ");
            dump_type(P, d->result);
            DUMP_MSG(P, "body: ");
            dump_expr(P, d->body);
            break;
        }
        case kAstFieldDecl: {
            struct AstFieldDecl *d = AstGetFieldDecl(decl);
            DUMP_NAME(P, d->ident.name);
            DUMP_MSG(P, "tag: ");
            dump_type(P, d->tag);
            DUMP_FMT(P, "is_pub: %d\n", d->is_pub);
            break;
        }
        case kAstParamDecl: {
            struct AstParamDecl *d = AstGetParamDecl(decl);
            DUMP_NAME(P, d->ident.name);
            DUMP_MSG(P, "tag: ");
            dump_type(P, d->tag);
            break;
        }
        case kAstConstDecl: {
            struct AstConstDecl *d = AstGetConstDecl(decl);
            DUMP_NAME(P, d->ident.name);
            DUMP_MSG(P, "tag: ");
            dump_type(P, d->tag);
            DUMP_MSG(P, "init: ");
            dump_expr(P, d->init);
            DUMP_FMT(P, "is_pub: %d\n", d->is_pub);
            break;
        }
        case kAstVariantDecl: {
            struct AstVariantDecl *d = AstGetVariantDecl(decl);
            DUMP_NAME(P, d->ident.name);
            dump_decl_list(P, d->fields, "fields");
            break;
        }
        case kAstAdtDecl: {
            struct AstAdtDecl *d = AstGetAdtDecl(decl);
            DUMP_NAME(P, d->ident.name);
            DUMP_FMT(P, "is_struct: %d\n", d->is_struct);
            DUMP_FMT(P, "is_pub: %d\n", d->is_pub);
            dump_type_list(P, d->traits, "traits");
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->variants, "variants");
            break;
        }
        case kAstGenericDecl: {
            struct AstGenericDecl *d = AstGetGenericDecl(decl);
            DUMP_NAME(P, d->ident.name);
            break;
        }
        case kAstTypeDecl: {
            struct AstTypeDecl *d = AstGetTypeDecl(decl);
            DUMP_NAME(P, d->ident.name);
            DUMP_MSG(P, "rhs: ");
            dump_type(P, d->rhs);
            dump_decl_list(P, d->generics, "generics");
            DUMP_FMT(P, "is_pub: %d\n", d->is_pub);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_stmt(Printer *P, struct AstStmt *stmt)
{
    if (print_stmt_kind(P, stmt)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    add_span(P, stmt->hdr.span);
    switch (AST_KINDOF(stmt)) {
        case kAstLetStmt: {
            struct AstLetStmt *s = AstGetLetStmt(stmt);
            DUMP_MSG(P, "pat: ");
//            dump_pat(P, s->pat);
            DUMP_MSG(P, "tag: ");
            dump_type(P, s->tag);
            DUMP_MSG(P, "init: ");
            dump_expr(P, s->init);
            break;
        }
        case kAstExprStmt: {
            struct AstExprStmt *s = AstGetExprStmt(stmt);
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->expr);
            break;
        }
        case kAstDeclStmt: {
            struct AstDeclStmt *s = AstGetDeclStmt(stmt);
            DUMP_MSG(P, "decl: ");
            dump_decl(P, s->decl);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_type(Printer *P, struct AstType *type)
{
    if (print_type_kind(P, type)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    add_span(P, type->hdr.span);
    switch (AST_KINDOF(type)) {
        case kAstPathType: {
            struct AstPathType *t = AstGetPathType(type);
            dump_path(P, &t->path);
            break;
        }
        case kAstTupleType: {
            struct AstTupleType *t = AstGetTupleType(type);
            break;
        }
        case kAstContainerType: {
            struct AstContainerType *t = AstGetContainerType(type);
            DUMP_MSG(P, "first: ");
            dump_type(P, t->first);
            DUMP_MSG(P, "second: ");
            dump_type(P, t->second);
            break;
        }
        case kAstNeverType:
        case kAstInferType:
            break;
        case kAstFnType: {
            struct AstFnType *t = AstGetFnType(type);
            dump_type_list(P, t->params, "params");
            DUMP_MSG(P, "result: ");
            dump_type(P, t->result);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_expr(Printer *P, struct AstExpr *expr)
{
    if (print_expr_kind(P, expr)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    add_span(P, expr->hdr.span);
    switch (AST_KINDOF(expr)) {
        case kAstParenExpr: {
            struct AstParenExpr *e = AstGetParenExpr(expr);
            break;
        }
        case kAstLogicalExpr: {
            struct AstLogicalExpr *e = AstGetLogicalExpr(expr);
            break;
        }
        case kAstPathExpr: {
            struct AstPathExpr *e = AstGetPathExpr(expr);
            break;
        }
        case kAstChainExpr: {
            struct AstChainExpr *e = AstGetChainExpr(expr);
            break;
        }
        case kAstMatchExpr: {
            struct AstMatchExpr *e = AstGetMatchExpr(expr);
            break;
        }
        case kAstMatchArm: {
            struct AstMatchArm *e = AstGetMatchArm(expr);
            break;
        }
        case kAstClosureExpr: {
            struct AstClosureExpr *e = AstGetClosureExpr(expr);
            break;
        }
        case kAstConversionExpr: {
            struct AstConversionExpr *e = AstGetConversionExpr(expr);
            break;
        }
        case kAstFieldExpr: {
            struct AstFieldExpr *e = AstGetFieldExpr(expr);
            break;
        }
        case kAstJumpExpr: {
            struct AstJumpExpr *e = AstGetJumpExpr(expr);
            break;
        }
        case kAstLiteralExpr: {
            struct AstLiteralExpr *e = AstGetLiteralExpr(expr);
            switch (e->lit_kind) {
                case kAstBasicLit:
                    DUMP_MSG(P, "lit_kind: BASIC\n");
                    switch (e->basic.code) {
                        case BUILTIN_UNIT:
                            DUMP_MSG(P, "type: ()\n");
                            break;
                        case BUILTIN_BOOL:
                            DUMP_MSG(P, "type: bool\n");
                            DUMP_FMT(P, "value: %s\n", V_TRUE(e->basic.value) ? "true" : "false");
                            break;
                        case BUILTIN_INT:
                            DUMP_MSG(P, "type: int\n");
                            DUMP_FMT(P, "value: %I\n", V_INT(e->basic.value));
                            break;
                        case BUILTIN_FLOAT:
                            DUMP_MSG(P, "type: float\n");
                            DUMP_FMT(P, "value: %f\n", V_FLOAT(e->basic.value));
                            break;
                        default:
                            paw_assert(e->basic.code == BUILTIN_STR);
                            DUMP_MSG(P, "type: string\n");
                            DUMP_FMT(P, "value: %s\n", V_STR(e->basic.value)->text);
                            break;
                    }
                    break;
                case kAstTupleLit:
                    DUMP_MSG(P, "lit_kind: TUPLE\n");
                    dump_expr_list(P, e->tuple.elems, "elems");
                    break;
                case kAstContainerLit:
                    DUMP_MSG(P, "lit_kind: CONTAINER\n");
                    dump_expr_list(P, e->cont.items, "items");
                    break;
                case kAstCompositeLit:
                    DUMP_MSG(P, "lit_kind: COMPOSITE\n");
                    DUMP_MSG(P, "target: ");
                    dump_path(P, &e->comp.path);
                    dump_expr_list(P, e->comp.items, "items");
                    break;
            }
            break;
        }
        case kAstStringExpr: {
            struct AstStringExpr *e = AstGetStringExpr(expr);
            // TODO
            break;
        }
        case kAstUnOpExpr: {
            struct AstUnOpExpr *e = AstGetUnOpExpr(expr);
            DUMP_FMT(P, "op: %d\n", e->op);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            break;
        }
        case kAstAssignExpr: {
            struct AstAssignExpr *e = AstGetAssignExpr(expr);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kAstOpAssignExpr: {
            struct AstOpAssignExpr *e = AstGetOpAssignExpr(expr);
            DUMP_FMT(P, "op: %d\n", e->op);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kAstBinOpExpr: {
            struct AstBinOpExpr *e = AstGetBinOpExpr(expr);
            DUMP_FMT(P, "op: %d\n", e->op);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kAstRangeExpr: {
            struct AstRangeExpr *e = AstGetRangeExpr(expr);
            DUMP_FMT(P, "is_inclusive: %d\n", e->is_inclusive);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kAstCallExpr: {
            struct AstCallExpr *e = AstGetCallExpr(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            dump_expr_list(P, e->args, "args");
            break;
        }
        case kAstIndex: {
            struct AstIndex *e = AstGetIndex(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            DUMP_MSG(P, "index: ");
            dump_expr(P, e->index);
            break;
        }
        case kAstSelector: {
            struct AstSelector *e = AstGetSelector(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            if (e->is_index) {
                DUMP_FMT(P, "index: %I\n", e->index);
            } else {
                DUMP_NAME(P, e->ident.name);
            }
            break;
        }
        case kAstBlock: {
            struct AstBlock *e = AstGetBlock(expr);
            dump_stmt_list(P, e->stmts, "stmts");
            if (e->result != NULL) dump_expr(P, e->result);
            break;
        }
        case kAstIfExpr: {
            struct AstIfExpr *e = AstGetIfExpr(expr);
            DUMP_MSG(P, "cond: ");
            dump_expr(P, e->cond);
            DUMP_MSG(P, "then_arm: ");
            dump_expr(P, e->then_arm);
            DUMP_MSG(P, "else_arm: ");
            dump_expr(P, e->else_arm);
            break;
        }
        case kAstForExpr: {
            struct AstForExpr *e = AstGetForExpr(expr);
            DUMP_MSG(P, "pat: ");
//            dump_pat(P, e->pat);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            DUMP_MSG(P, "block: ");
            dump_expr(P, e->block);
            break;
        }
        case kAstLoopExpr: {
            struct AstLoopExpr *e = AstGetLoopExpr(expr);
            DUMP_MSG(P, "block: ");
            dump_expr(P, e->block);
            break;
        }
        case kAstWhileExpr: {
            struct AstWhileExpr *e = AstGetWhileExpr(expr);
            DUMP_MSG(P, "cond: ");
            dump_expr(P, e->cond);
            DUMP_MSG(P, "block: ");
            dump_expr(P, e->block);
            break;
        }
        case kAstReturnExpr: {
            struct AstReturnExpr *e = AstGetReturnExpr(expr);
            DUMP_MSG(P, "expr: ");
            dump_expr(P, e->expr);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

char const *pawAst_dump(struct Ast *ast)
{
    Buffer buf;
    paw_Env *P = ENV(ast);
    pawL_init_buffer(P, &buf);
    Printer print = {
        .ast = ast,
        .buf = &buf,
        .P = P,
    };
    struct AstDecl *const *pdecl;
    K_LIST_FOREACH (ast->modules, pdecl) {
        dump_decl(&print, *pdecl);
    }

    Str const *result = pawP_scan_nstr(ast->C, ast->C->strings, buf.data, buf.size);
    pawL_discard_result(P, &buf);
    return result->text;
}

char const *pawAst_print_path(struct Ast *ast, struct AstPath path)
{
    Buffer buf;
    paw_Env *P = ENV(ast);
    pawL_init_buffer(P, &buf);

    int index;
    struct AstSegment const *psegment;
    K_LIST_ENUMERATE (path.segments, index, psegment) {
        if (index > 0) L_ADD_LITERAL(P, &buf, "::");
        Str const *name = psegment->ident.name;
        pawL_add_nstring(P, &buf, name->text, name->length);
    }

    Str const *result = pawP_scan_nstr(ast->C, ast->C->strings, buf.data, buf.size);
    pawL_discard_result(P, &buf);
    return result->text;
}
