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

struct Ast *pawAst_new(struct Compiler *C, String *name, int modno)
{
    paw_Env *P = ENV(C);
    struct Ast *ast = pawK_pool_alloc(P, C->pool, sizeof(struct Ast));
    *ast = (struct Ast){
        .modno = modno,
        .pool = C->pool,
        .name = name,
        .C = C,
        .P = P,
    };
    ast->items = pawAst_decl_list_new(C);
    return ast;
}

void pawAst_free(struct Ast *ast)
{
    PAW_UNUSED(ast);
}

#define DEFINE_NODE_CONSTRUCTOR(name, T) \
    struct T *pawAst_new_##name(struct Ast *ast) \
    { \
        return pawK_pool_alloc(ENV(ast), (ast)->pool, sizeof(struct T)); \
    }
DEFINE_NODE_CONSTRUCTOR(decl, AstDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, AstStmt)
DEFINE_NODE_CONSTRUCTOR(type, AstType)
DEFINE_NODE_CONSTRUCTOR(expr, AstExpr)
DEFINE_NODE_CONSTRUCTOR(pat, AstPat)

struct AstSegment *pawAst_segment_new(struct Compiler *C)
{
    return pawK_pool_alloc(ENV(C), C->pool, sizeof(struct AstSegment));
}

#if defined(PAW_DEBUG_EXTRA)

typedef struct Printer {
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
        DUMP_LITERAL(P, "    ");
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

#define DEFINE_LIST_PRINTER(name, T) \
    static void dump_##name##_list(Printer *P, struct T##List *list, \
                                   const char *name) \
    { \
        DUMP_FMT(P, "%s: {\n", name); \
        ++P->indent; \
        if (list != NULL) { \
            DUMP_MSG(P, "" /* indent */); \
            for (int i = 0; i < list->count; ++i) { \
                dump_##name(P, list->data[i]); \
            } \
        } \
        --P->indent; \
        DUMP_MSG(P, "}\n"); \
    }
DEFINE_LIST_PRINTER(expr, AstExpr)
DEFINE_LIST_PRINTER(type, AstType)
DEFINE_LIST_PRINTER(decl, AstDecl)
DEFINE_LIST_PRINTER(stmt, AstStmt)

#define DEFINE_KIND_PRINTER(name, T) \
    static int print_##name##_kind(Printer *P, void *node) \
    { \
        if (node != NULL) { \
            struct T *typed = node; \
            DUMP_FSTRING(P, "%s {\n", k##T##Names[AST_KINDOF(typed)]); \
            return 0; \
        } \
        return -1; \
    }
DEFINE_KIND_PRINTER(expr, AstExpr)
DEFINE_KIND_PRINTER(type, AstType)
DEFINE_KIND_PRINTER(decl, AstDecl)
DEFINE_KIND_PRINTER(stmt, AstStmt)

static void dump_path(Printer *P, struct AstPath *p)
{
    for (int i = 0; i < p->count; ++i) {
        struct AstSegment seg = K_LIST_GET(p, i);
        DUMP_NAME(P, seg.name);
        dump_type_list(P, seg.types, "types");
    }
}

static void dump_decl(Printer *P, struct AstDecl *decl)
{
    if (print_decl_kind(P, decl)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", decl->hdr.line);
    switch (AST_KINDOF(decl)) {
        case kAstImplDecl:
        case kAstUseDecl:
            PAW_UNREACHABLE(); // TODO: write this code!!!
        case kAstFuncDecl: {
            struct AstFuncDecl *d = AstGetFuncDecl(decl);
            DUMP_FMT(P, "receiver: %p\n", CAST(void *, d->receiver));
            DUMP_FMT(P, "name: %s\n", d->name->text);
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
            DUMP_NAME(P, d->name);
            DUMP_MSG(P, "tag: ");
            dump_type(P, d->tag);
            break;
        }
        case kAstVarDecl: {
            struct AstVarDecl *d = AstGetVarDecl(decl);
            DUMP_NAME(P, d->name);
            DUMP_MSG(P, "tag: ");
            dump_type(P, d->tag);
            DUMP_MSG(P, "init: ");
            dump_expr(P, d->init);
            break;
        }
        case kAstVariantDecl: {
            struct AstVariantDecl *d = AstGetVariantDecl(decl);
            DUMP_NAME(P, d->name);
            dump_decl_list(P, d->fields, "fields");
            break;
        }
        case kAstAdtDecl: {
            struct AstAdtDecl *d = AstGetAdtDecl(decl);
            DUMP_NAME(P, d->name);
            DUMP_FMT(P, "is_struct: %d\n", d->is_struct);
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->fields, "fields");
            break;
        }
        case kAstGenericDecl: {
            struct AstGenericDecl *d = AstGetGenericDecl(decl);
            DUMP_NAME(P, d->name);
            break;
        }
        case kAstTypeDecl: {
            struct AstTypeDecl *d = AstGetTypeDecl(decl);
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

static void dump_stmt(Printer *P, struct AstStmt *stmt)
{
    if (print_stmt_kind(P, stmt)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", stmt->hdr.line);
    switch (AST_KINDOF(stmt)) {
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
    DUMP_FMT(P, "line: %d\n", type->hdr.line);
    switch (AST_KINDOF(type)) {
        case kAstPathType: {
            struct AstPathType *t = AstGetPathType(type);
            dump_path(P, t->path);
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
        case kAstFuncType: {
            struct AstFuncType *t = AstGetFuncType(type);
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
    DUMP_FMT(P, "line: %d\n", expr->hdr.line);
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
                        case PAW_TUNIT:
                            DUMP_MSG(P, "type: ()\n");
                            break;
                        case PAW_TBOOL:
                            DUMP_MSG(P, "type: bool\n");
                            DUMP_FMT(P, "value: %s\n", V_TRUE(e->basic.value) ? "true" : "false");
                            break;
                        case PAW_TINT:
                            DUMP_MSG(P, "type: int\n");
                            DUMP_FMT(P, "value: %I\n", V_INT(e->basic.value));
                            break;
                        case PAW_TFLOAT:
                            DUMP_MSG(P, "type: float\n");
                            DUMP_FMT(P, "value: %f\n", V_FLOAT(e->basic.value));
                            break;
                        default:
                            paw_assert(e->basic.code == PAW_TSTR);
                            DUMP_MSG(P, "type: string\n");
                            DUMP_FMT(P, "value: %s\n", V_STRING(e->basic.value)->text);
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
                default:
                    paw_assert(e->lit_kind == kAstCompositeLit);
                    DUMP_MSG(P, "lit_kind: COMPOSITE\n");
                    DUMP_MSG(P, "target: ");
                    dump_path(P, e->comp.path);
                    dump_expr_list(P, e->comp.items, "items");
            }
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
        case kAstBinOpExpr: {
            struct AstBinOpExpr *e = AstGetBinOpExpr(expr);
            DUMP_FMT(P, "op: %d\n", e->op);
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
            DUMP_FMT(P, "is_slice: %d\n", e->is_slice);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            DUMP_MSG(P, "first: ");
            dump_expr(P, e->first);
            DUMP_MSG(P, "second: ");
            dump_expr(P, e->second);
            break;
        }
        case kAstSelector: {
            struct AstSelector *e = AstGetSelector(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            if (e->is_index) {
                DUMP_FMT(P, "index: %I\n", e->index);
            } else {
                DUMP_NAME(P, e->name);
            }
            break;
        }
        case kAstBlock: {
            struct AstBlock *e = AstGetBlock(expr);
            dump_stmt_list(P, e->stmts, "stmts");
            dump_expr(P, e->result);
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
            DUMP_NAME(P, e->name);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
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

void pawAst_dump(struct Ast *ast)
{
    Buffer buf;
    paw_Env *P = ENV(ast);
    pawL_init_buffer(P, &buf);
    Printer print = {
        .buf = &buf,
        .P = P,
    };
    for (int i = 0; i < ast->items->count; ++i) {
        dump_decl(&print, ast->items->data[i]);
    }
    pawL_push_result(P, &buf);
}

#endif
