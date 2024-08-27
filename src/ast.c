// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ast.h"
#include "compile.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include <inttypes.h>

#define FIRST_ARENA_SIZE 4096
#define LARGE_ARENA_MIN 32

struct Ast *pawAst_new(struct Compiler *C)
{
    paw_Env *P = ENV(C);
    struct DynamicMem *dm = C->dm;
    dm->ast = pawM_new(P, struct Ast);
    *dm->ast = (struct Ast){
        .P = P,
    };
    // initialize memory pools for storing AST components
    pawK_pool_init(P, &dm->ast->pool, FIRST_ARENA_SIZE, sizeof(void *) * LARGE_ARENA_MIN);
    return dm->ast;
}

void pawAst_free(struct Ast *ast)
{
    if (ast != NULL) {
        paw_Env *P = ENV(ast);
        pawK_pool_uninit(P, &ast->pool);
        pawM_free(P, ast);
    }
}

#define DEFINE_NODE_CONSTRUCTOR(name, T) \
    struct T *pawAst_new_##name(struct Ast *ast, int line, enum T##Kind kind) \
    { \
        struct T *r = pawK_pool_alloc(ENV(ast), &(ast)->pool, sizeof(struct T)); \
        r->hdr.line = line; \
        r->hdr.kind = kind; \
        return r; \
    }
DEFINE_NODE_CONSTRUCTOR(expr, AstExpr)
DEFINE_NODE_CONSTRUCTOR(decl, AstDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, AstStmt)

struct AstSegment *pawAst_segment_new(struct Ast *ast)
{
    return pawK_pool_alloc(ENV(ast), &ast->pool, sizeof(struct AstSegment));
}

typedef struct Printer {
    FILE *out;
    int indent;
} Printer;

#define DUMP_FMT(P, fmt, ...) (indent_line(P), fprintf((P)->out, fmt, __VA_ARGS__))
#define DUMP_MSG(P, msg) (indent_line(P), fprintf((P)->out, msg))

#define DUMP_BLOCK(P, b) CHECK_EXP(AstIsBlock(AST_CAST_STMT(b)), dump_stmt(P, AST_CAST_STMT(b)))
#define DUMP_NAME(P, s) DUMP_FMT(P, "name: %s\n", s ? s->text : NULL)

static void indent_line(Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        fprintf(P->out, "    ");
    }
}

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
DEFINE_LIST_PRINTER(decl, AstDecl)
DEFINE_LIST_PRINTER(stmt, AstStmt)

#define DEFINE_KIND_PRINTER(name, T) \
    static int print_##name##_kind(Printer *P, void *node) \
    { \
        if (node != NULL) { \
            struct T *typed = node; \
            printf("%s {\n", k##T##Names[AST_KINDOF(typed)]); \
            return 0; \
        } \
        return -1; \
    }
DEFINE_KIND_PRINTER(expr, AstExpr) 
DEFINE_KIND_PRINTER(decl, AstDecl) 
DEFINE_KIND_PRINTER(stmt, AstStmt) 

static void dump_path(Printer *P, struct AstPath *p)
{
    for (int i = 0; i < p->count; ++i) {
        struct AstSegment *seg = p->data[i];
        DUMP_NAME(P, seg->name);
        dump_expr_list(P, seg->types, "types");
    }
}

static void dump_decl(Printer *P, struct AstDecl *d)
{
    if (print_decl_kind(P, d)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", d->hdr.line);
    switch (AST_KINDOF(d)) {
        case kAstFuncDecl:
            DUMP_FMT(P, "receiver: %p\n", CAST(d->func.receiver, void *));
            DUMP_FMT(P, "name: %s\n", d->func.name->text);
            dump_decl_list(P, d->func.generics, "generics");
            dump_decl_list(P, d->func.params, "params");
            DUMP_MSG(P, "result: ");
            dump_expr(P, d->func.result);
            DUMP_MSG(P, "body: ");
            DUMP_BLOCK(P, d->func.body);
            break;
        case kAstFieldDecl:
            DUMP_NAME(P, d->field.name);
            DUMP_MSG(P, "tag: ");
            dump_expr(P, d->field.tag);
            break;
        case kAstVarDecl:
            DUMP_NAME(P, d->var.name);
            DUMP_MSG(P, "tag: ");
            dump_expr(P, d->var.tag);
            DUMP_MSG(P, "init: ");
            dump_expr(P, d->var.init);
            break;
        case kAstVariantDecl:
            DUMP_NAME(P, d->variant.name);
            dump_decl_list(P, d->variant.fields, "fields");
            break;
        case kAstAdtDecl:
            DUMP_NAME(P, d->adt.name);
            DUMP_FMT(P, "is_struct: %d\n", d->adt.is_struct);
            dump_decl_list(P, d->adt.generics, "generics");
            dump_decl_list(P, d->adt.fields, "fields");
            break;
        case kAstGenericDecl:
            DUMP_NAME(P, d->generic.name);
            break;
        case kAstTypeDecl:
            DUMP_NAME(P, d->type.name);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, d->type.rhs);
            dump_decl_list(P, d->type.generics, "generics");
            break;
        default:
            paw_assert(0);
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_stmt(Printer *P, struct AstStmt *s)
{
    if (print_stmt_kind(P, s)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", s->hdr.line);
    switch (AST_KINDOF(s)) {
        case kAstExprStmt:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->expr.expr);
            break;
        case kAstBlock:
            dump_stmt_list(P, s->block.stmts, "stmts");
            break;
        case kAstDeclStmt:
            DUMP_MSG(P, "decl: ");
            dump_decl(P, s->decl.decl);
            break;
        case kAstIfStmt:
            DUMP_MSG(P, "cond: ");
            dump_expr(P, s->if_.cond);
            DUMP_MSG(P, "then_arm: ");
            dump_stmt(P, s->if_.then_arm);
            DUMP_MSG(P, "else_arm: ");
            dump_stmt(P, s->if_.else_arm);
            break;
        case kAstForStmt:
            if (s->for_.is_fornum) {
                DUMP_NAME(P, s->for_.name);
                DUMP_MSG(P, "begin: ");
                dump_expr(P, s->for_.fornum.begin);
                DUMP_MSG(P, "end: ");
                dump_expr(P, s->for_.fornum.end);
                DUMP_MSG(P, "step: ");
                dump_expr(P, s->for_.fornum.step);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, s->for_.block);
            } else {
                DUMP_NAME(P, s->for_.name);
                DUMP_MSG(P, "target: ");
                dump_expr(P, s->for_.forin.target);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, s->for_.block);
            }
            break;
        case kAstWhileStmt:
            if (s->while_.is_dowhile) {
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, s->while_.block);
                DUMP_MSG(P, "cond: ");
                dump_expr(P, s->while_.cond);
            } else {
                DUMP_MSG(P, "cond: ");
                dump_expr(P, s->while_.cond);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, s->while_.block);
            }
            break;
        case kAstReturnStmt:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->result.expr);
            break;
        default:
            break;
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_expr(Printer *P, struct AstExpr *e)
{
    if (print_expr_kind(P, e)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", e->hdr.line);
    switch (AST_KINDOF(e)) {
        case kAstLiteralExpr:
            switch (e->literal.lit_kind) {
                case kAstBasicLit:
                    DUMP_MSG(P, "lit_kind: BASIC\n");
                    switch (e->literal.basic.t) {
                        case PAW_TUNIT:
                            DUMP_MSG(P, "type: ()\n");
                            break;
                        case PAW_TBOOL:
                            DUMP_MSG(P, "type: bool\n");
                            DUMP_FMT(P, "value: %s\n",
                                     V_TRUE(e->literal.basic.value) ? "true"
                                                                    : "false");
                            break;
                        case PAW_TINT:
                            DUMP_MSG(P, "type: int\n");
                            DUMP_FMT(P, "value: %" PRId64 "\n",
                                     V_INT(e->literal.basic.value));
                            break;
                        case PAW_TFLOAT:
                            DUMP_MSG(P, "type: float\n");
                            DUMP_FMT(P, "value: %f\n",
                                     V_FLOAT(e->literal.basic.value));
                            break;
                        default:
                            paw_assert(e->literal.basic.t == PAW_TSTR);
                            DUMP_MSG(P, "type: string\n");
                            DUMP_FMT(P, "value: %s\n",
                                     V_STRING(e->literal.basic.value)->text);
                            break;
                    }
                    break;
                case kAstTupleLit:
                    DUMP_MSG(P, "lit_kind: TUPLE\n");
                    dump_expr_list(P, e->literal.tuple.elems, "elems");
                    break;
                case kAstContainerLit:
                    DUMP_MSG(P, "lit_kind: CONTAINER\n");
                    dump_expr_list(P, e->literal.cont.items, "items");
                    break;
                default:
                    paw_assert(e->literal.lit_kind == kAstCompositeLit);
                    DUMP_MSG(P, "lit_kind: COMPOSITE\n");
                    DUMP_MSG(P, "target: ");
                    dump_path(P, e->literal.comp.path);
                    dump_expr_list(P, e->literal.comp.items, "items");
            }
            break;
        case kAstUnOpExpr:
            DUMP_FMT(P, "op: %d\n", e->unop.op);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->unop.target);
            break;
        case kAstAssignExpr:
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->assign.lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->assign.rhs);
            break;
        case kAstBinOpExpr:
            DUMP_FMT(P, "op: %d\n", e->binop.op);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->binop.lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->binop.rhs);
            break;
        case kAstCallExpr:
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->call.target);
            dump_expr_list(P, e->call.args, "args");
            break;
        case kAstIndex:
            DUMP_FMT(P, "is_slice: %d\n", e->index.is_slice);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->index.target);
            DUMP_MSG(P, "first: ");
            dump_expr(P, e->index.first);
            DUMP_MSG(P, "second: ");
            dump_expr(P, e->index.second);
            break;
        case kAstSelector:
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->selector.target);
            if (e->selector.is_index) {
                DUMP_FMT(P, "index: %" PRId64 "\n", e->selector.index);
            } else {
                DUMP_NAME(P, e->selector.name);
            }
            break;
        case kAstContainerType:
            DUMP_MSG(P, "first: ");
            dump_expr(P, e->cont.first);
            DUMP_MSG(P, "second: ");
            dump_expr(P, e->cont.second);
            break;
        case kAstSignature:
            dump_expr_list(P, e->sig.params, "params");
            DUMP_MSG(P, "result: ");
            dump_expr(P, e->sig.result);
            break;
        default:
            break;
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

void pawAst_dump_decl(FILE *out, struct AstDecl *decl)
{
    Printer P = {.out = out};
    dump_decl(&P, decl);
}

void pawAst_dump_expr(FILE *out, struct AstExpr *expr)
{
    Printer P = {.out = out};
    dump_expr(&P, expr);
}

void pawAst_dump_stmt(FILE *out, struct AstStmt *stmt)
{
    Printer P = {.out = out};
    dump_stmt(&P, stmt);
}

void pawAst_dump_path(FILE *out, struct AstPath *path)
{
    Printer P = {.out = out};
    dump_path(&P, path);
}
