// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ast.h"
#include "map.h"
#include "mem.h"
#include <inttypes.h>
#include <limits.h>
#include <stdlib.h>

#define FIRST_ARENA_SIZE 512
#define LARGE_ARENA_MIN 32

struct Ast *pawAst_new(struct Lex *lex)
{
    paw_Env *P = ENV(lex);
    struct Ast *tree = pawM_new(P, struct Ast);
    tree->lex = lex;
    tree->P = P;

    // initialize memory pools for storing AST components
    pawK_pool_init(P, &tree->pool, FIRST_ARENA_SIZE, sizeof(void *) * LARGE_ARENA_MIN);
    return tree;
}

void pawAst_free(struct Ast *ast)
{
    paw_Env *P = ENV(ast);
    pawK_pool_uninit(P, &ast->pool);
    pawM_free(P, ast);
}

#define DEFINE_NODE_CONSTRUCTOR(name, T)                                         \
    struct T *pawAst_new_##name(struct Ast *ast, enum T##Kind kind)              \
    {                                                                            \
        struct T *r = pawK_pool_alloc(ENV(ast), &(ast)->pool, sizeof(struct T)); \
        r->hdr.line = (ast)->lex->line;                                          \
        r->hdr.kind = kind;                                                      \
        return r;                                                                \
    }
DEFINE_NODE_CONSTRUCTOR(expr, AstExpr)
DEFINE_NODE_CONSTRUCTOR(decl, AstDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, AstStmt)

struct AstSegment *pawAst_segment_new(struct Ast *ast)
{
    return pawK_pool_alloc(ENV((ast)->lex), &(ast)->pool, sizeof(struct AstSegment));
}

typedef struct Printer {
    FILE *out;
    int indent;
} Printer;

static void indent_line(Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        fprintf(P->out, "    ");
    }
}

#define dump_fmt(P, fmt, ...) \
    (indent_line(P), fprintf((P)->out, fmt, __VA_ARGS__))
#define dump_msg(P, msg) (indent_line(P), fprintf((P)->out, msg))

static void dump_stmt(Printer *, struct AstStmt *);
static void dump_expr(Printer *, struct AstExpr *);

static void print_decl_kind(Printer *P, void *node)
{
    struct AstDecl *d = node;
    switch (AST_KINDOF(d)) {
        case kAstFuncDecl:
            fprintf(P->out, "FuncDecl");
            break;
        case kAstFieldDecl:
            fprintf(P->out, "FieldDecl");
            break;
        case kAstVarDecl:
            fprintf(P->out, "VarDecl");
            break;
        case kAstVariantDecl:
            fprintf(P->out, "VariantDecl");
            break;
        case kAstAdtDecl:
            fprintf(P->out, "StructDecl");
            break;
        case kAstGenericDecl:
            fprintf(P->out, "GenericDecl");
            break;
        case kAstTypeDecl:
            fprintf(P->out, "TypeDecl");
            break;
        default:
            fprintf(P->out, "?");
    }
}

static void print_expr_kind(Printer *P, void *node)
{
    struct AstExpr *e = node;
    switch (AST_KINDOF(e)) {
        case kAstLiteralExpr:
            fprintf(P->out, "LiteralExpr");
            break;
        case kAstUnOpExpr:
            fprintf(P->out, "UnOpExpr");
            break;
        case kAstBinOpExpr:
            fprintf(P->out, "BinOpExpr");
            break;
        case kAstCallExpr:
            fprintf(P->out, "CallExpr");
            break;
        case kAstIndex:
            fprintf(P->out, "Index");
            break;
        case kAstSelector:
            fprintf(P->out, "Selector");
            break;
        case kAstSignature:
            fprintf(P->out, "FuncType");
            break;
        case kAstContainerType:
            fprintf(P->out, "ContainerType");
            break;
        default:
            fprintf(P->out, "?");
            break;
    }
}

static void print_stmt_kind(Printer *P, void *node)
{
    struct AstStmt *s = node;
    switch (AST_KINDOF(s)) {
        case kAstExprStmt:
            fprintf(P->out, "ExprStmt");
            break;
        case kAstDeclStmt:
            fprintf(P->out, "DeclStmt");
            break;
        case kAstBlock:
            fprintf(P->out, "Block");
            break;
        case kAstIfStmt:
            fprintf(P->out, "IfStmt");
            break;
        case kAstForStmt:
            fprintf(P->out, "ForStmt");
            break;
        case kAstWhileStmt:
            fprintf(P->out, "WhileStmt");
            break;
        case kAstReturnStmt:
            fprintf(P->out, "ReturnStmt");
            break;
        default:
            fprintf(P->out, "?");
    }
}

static int predump_node(Printer *P, void *node,
                        void (*print)(Printer *, void *))
{
    if (node != NULL) {
        print(P, node);
        fprintf(P->out, " {\n");
        return 0;
    }
    return -1;
}

#define dump_block(P, b) check_exp(AstIsBlock(AST_CAST_STMT(b)), dump_stmt(P, AST_CAST_STMT(b)))
#define dump_name(P, s) dump_fmt(P, "name: %s\n", s ? s->text : NULL)

static void dump_expr(Printer *P, struct AstExpr *e);
static void dump_decl(Printer *P, struct AstDecl *d);
static void dump_stmt(Printer *P, struct AstStmt *s);

// clang-format off
#define DEFINE_LIST_PRINTER(name, T)                                           \
    static void dump_##name##_list(Printer *P, struct T##List *list,           \
                                   const char *name)                           \
    {                                                                          \
        dump_fmt(P, "%s: {\n", name);                                          \
        ++P->indent;                                                           \
        if (list != NULL) {                                                    \
            dump_msg(P, "" /* indent */);                                      \
            for (int i = 0; i < list->count; ++i) {                            \
                dump_##name(P, list->data[i]);                                 \
            }                                                                  \
        }                                                                      \
        --P->indent;                                                           \
        dump_msg(P, "}\n");                                                    \
    }
DEFINE_LIST_PRINTER(expr, AstExpr) 
DEFINE_LIST_PRINTER(decl, AstDecl)
DEFINE_LIST_PRINTER(stmt, AstStmt)
// clang-format on

static void dump_path(Printer *P, struct AstPath *p)
{
    for (int i = 0; i < p->count; ++i) {
        struct AstSegment *seg = p->data[i];
        dump_name(P, seg->name);
        dump_expr_list(P, seg->types, "types");
    }
}

static void dump_decl(Printer *P, struct AstDecl *d)
{
    if (predump_node(P, d, print_decl_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", d->hdr.line);
    switch (AST_KINDOF(d)) {
        case kAstFuncDecl:
            dump_fmt(P, "is_global: %d\n", d->func.is_global);
            dump_fmt(P, "receiver: %p\n", (void *)d->func.receiver);
            dump_fmt(P, "name: %s\n", d->func.name->text);
            dump_decl_list(P, d->func.generics, "generics");
            dump_decl_list(P, d->func.params, "params");
            dump_msg(P, "result: ");
            dump_expr(P, d->func.result);
            dump_msg(P, "body: ");
            dump_block(P, d->func.body);
            break;
        case kAstFieldDecl:
            dump_name(P, d->field.name);
            dump_msg(P, "tag: ");
            dump_expr(P, d->field.tag);
            break;
        case kAstVarDecl:
            dump_fmt(P, "is_global: %d\n", d->var.is_global);
            dump_name(P, d->var.name);
            dump_msg(P, "tag: ");
            dump_expr(P, d->var.tag);
            dump_msg(P, "init: ");
            dump_expr(P, d->var.init);
            break;
        case kAstVariantDecl:
            dump_name(P, d->variant.name);
            dump_decl_list(P, d->variant.fields, "fields");
            break;
        case kAstAdtDecl:
            dump_name(P, d->adt.name);
            dump_fmt(P, "is_struct: %d\n", d->adt.is_struct);
            dump_decl_list(P, d->adt.generics, "generics");
            dump_decl_list(P, d->adt.fields, "fields");
            break;
        case kAstGenericDecl:
            dump_name(P, d->generic.name);
            break;
        case kAstTypeDecl:
            dump_name(P, d->type.name);
            dump_msg(P, "rhs: ");
            dump_expr(P, d->type.rhs);
            dump_decl_list(P, d->type.generics, "generics");
            break;
        default:
            paw_assert(0);
    }
    --P->indent;
    dump_msg(P, "}\n");
}

static void dump_stmt(Printer *P, struct AstStmt *s)
{
    if (predump_node(P, s, print_stmt_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", s->hdr.line);
    switch (AST_KINDOF(s)) {
        case kAstExprStmt:
            dump_msg(P, "lhs: ");
            dump_expr(P, s->expr.lhs);
            dump_msg(P, "rhs: ");
            dump_expr(P, s->expr.rhs);
            break;
        case kAstBlock:
            dump_stmt_list(P, s->block.stmts, "stmts");
            break;
        case kAstDeclStmt:
            dump_msg(P, "decl: ");
            dump_decl(P, s->decl.decl);
            break;
        case kAstIfStmt:
            dump_msg(P, "cond: ");
            dump_expr(P, s->if_.cond);
            dump_msg(P, "then_arm: ");
            dump_stmt(P, s->if_.then_arm);
            dump_msg(P, "else_arm: ");
            dump_stmt(P, s->if_.else_arm);
            break;
        case kAstForStmt:
            if (s->for_.is_fornum) {
                dump_name(P, s->for_.name);
                dump_msg(P, "begin: ");
                dump_expr(P, s->for_.fornum.begin);
                dump_msg(P, "end: ");
                dump_expr(P, s->for_.fornum.end);
                dump_msg(P, "step: ");
                dump_expr(P, s->for_.fornum.step);
                dump_msg(P, "block: ");
                dump_block(P, s->for_.block);
            } else {
                dump_name(P, s->for_.name);
                dump_msg(P, "target: ");
                dump_expr(P, s->for_.forin.target);
                dump_msg(P, "block: ");
                dump_block(P, s->for_.block);
            }
            break;
        case kAstWhileStmt:
            if (s->while_.is_dowhile) {
                dump_msg(P, "block: ");
                dump_block(P, s->while_.block);
                dump_msg(P, "cond: ");
                dump_expr(P, s->while_.cond);
            } else {
                dump_msg(P, "cond: ");
                dump_expr(P, s->while_.cond);
                dump_msg(P, "block: ");
                dump_block(P, s->while_.block);
            }
            break;
        case kAstReturnStmt:
            dump_msg(P, "expr: ");
            dump_expr(P, s->result.expr);
            break;
        default:
            break;
    }
    --P->indent;
    dump_msg(P, "}\n");
}

static void dump_expr(Printer *P, struct AstExpr *e)
{
    if (predump_node(P, e, print_expr_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", e->hdr.line);
    switch (AST_KINDOF(e)) {
        case kAstLiteralExpr:
            switch (e->literal.lit_kind) {
                case kAstBasicLit:
                    dump_msg(P, "lit_kind: BASIC\n");
                    switch (e->literal.basic.t) {
                        case PAW_TUNIT:
                            dump_msg(P, "type: ()\n");
                            break;
                        case PAW_TBOOL:
                            dump_msg(P, "type: bool\n");
                            dump_fmt(P, "value: %s\n",
                                     v_true(e->literal.basic.value) ? "true"
                                                                    : "false");
                            break;
                        case PAW_TINT:
                            dump_msg(P, "type: int\n");
                            dump_fmt(P, "value: %" PRId64 "\n",
                                     v_int(e->literal.basic.value));
                            break;
                        case PAW_TFLOAT:
                            dump_msg(P, "type: float\n");
                            dump_fmt(P, "value: %f\n",
                                     v_float(e->literal.basic.value));
                            break;
                        default:
                            paw_assert(e->literal.basic.t == PAW_TSTRING);
                            dump_msg(P, "type: string\n");
                            dump_fmt(P, "value: %s\n",
                                     v_string(e->literal.basic.value)->text);
                            break;
                    }
                    break;
                case kAstTupleLit:
                    dump_msg(P, "lit_kind: TUPLE\n");
                    dump_expr_list(P, e->literal.tuple.elems, "elems");
                    break;
                case kAstContainerLit:
                    dump_msg(P, "lit_kind: CONTAINER\n");
                    dump_expr_list(P, e->literal.cont.items, "items");
                    break;
                default:
                    paw_assert(e->literal.lit_kind == kAstCompositeLit);
                    dump_msg(P, "lit_kind: COMPOSITE\n");
                    dump_msg(P, "target: ");
                    dump_path(P, e->literal.comp.path);
                    dump_expr_list(P, e->literal.comp.items, "items");
            }
            break;
        case kAstUnOpExpr:
            dump_fmt(P, "op: %d\n", e->unop.op);
            dump_msg(P, "target: ");
            dump_expr(P, e->unop.target);
            break;
        case kAstBinOpExpr:
            dump_fmt(P, "op: %d\n", e->binop.op);
            dump_msg(P, "lhs: ");
            dump_expr(P, e->binop.lhs);
            dump_msg(P, "rhs: ");
            dump_expr(P, e->binop.rhs);
            break;
        case kAstCallExpr:
            dump_msg(P, "target: ");
            dump_expr(P, e->call.target);
            dump_expr_list(P, e->call.args, "args");
            break;
        case kAstIndex:
            dump_fmt(P, "is_slice: %d\n", e->index.is_slice);
            dump_msg(P, "target: ");
            dump_expr(P, e->index.target);
            dump_msg(P, "first: ");
            dump_expr(P, e->index.first);
            dump_msg(P, "second: ");
            dump_expr(P, e->index.second);
            break;
        case kAstSelector:
            dump_msg(P, "target: ");
            dump_expr(P, e->selector.target);
            if (e->selector.is_index) {
                dump_fmt(P, "index: %" PRId64 "\n", e->selector.index);
            } else {
                dump_name(P, e->selector.name);
            }
            break;
        case kAstContainerType:
            dump_msg(P, "first: ");
            dump_expr(P, e->cont.first);
            dump_msg(P, "second: ");
            dump_expr(P, e->cont.second);
            break;
        case kAstSignature:
            dump_expr_list(P, e->sig.params, "params");
            dump_msg(P, "result: ");
            dump_expr(P, e->sig.result);
            break;
        default:
            break;
    }
    --P->indent;
    dump_msg(P, "}\n");
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
