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
    struct T *pawAst_new_##name(struct Ast *ast, int line, enum T##Kind kind) \
    { \
        struct T *r = pawK_pool_alloc(ENV(ast), (ast)->pool, sizeof(struct T)); \
        r->hdr.line = line; \
        r->hdr.kind = kind; \
        return r; \
    }
DEFINE_NODE_CONSTRUCTOR(expr, AstExpr)
DEFINE_NODE_CONSTRUCTOR(decl, AstDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, AstStmt)
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
            DUMP_FSTRING(P, "%s {\n", k##T##Names[AST_KINDOF(typed)]); \
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
        struct AstSegment seg = K_LIST_GET(p, i);
        DUMP_NAME(P, seg.name);
        dump_expr_list(P, seg.types, "types");
    }
}

static void dump_decl(Printer *P, struct AstDecl *d)
{
    if (print_decl_kind(P, d)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", d->hdr.line);
    switch (AST_KINDOF(d)) {
        case kAstImplDecl:
        case kAstUseDecl:
            PAW_UNREACHABLE(); // TODO: write this code!!!
        case kAstFuncDecl:
            DUMP_FMT(P, "receiver: %p\n", CAST(void *, d->func.receiver));
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
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_stmt(Printer *P, struct AstStmt *s)
{
    if (print_stmt_kind(P, s)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", s->hdr.line);
    switch (AST_KINDOF(s)) {
        case kAstExprStmt:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->expr.expr);
            break;
        case kAstDeclStmt:
            DUMP_MSG(P, "decl: ");
            dump_decl(P, s->decl.decl);
            break;
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_expr(Printer *P, struct AstExpr *e)
{
    if (print_expr_kind(P, e)) {
        DUMP_LITERAL(P, "(null)\n");
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", e->hdr.line);
    switch (AST_KINDOF(e)) {
        case kAstLogicalExpr:
        case kAstPathExpr:
        case kAstChainExpr:
        case kAstMatchExpr:
        case kAstMatchArm:
        case kAstClosureExpr:
        case kAstConversionExpr:
        case kAstFieldExpr:
        case kAstJumpExpr:
        case kAstTupleType:
            PAW_UNREACHABLE(); // TODO: write this code!!!
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
                            DUMP_FMT(P, "value: %I\n",
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
                DUMP_FMT(P, "index: %I\n", e->selector.index);
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
        case kAstBlock:
            dump_stmt_list(P, e->block.stmts, "stmts");
            dump_expr(P, e->block.result);
            break;
        case kAstIfExpr:
            DUMP_MSG(P, "cond: ");
            dump_expr(P, e->if_.cond);
            DUMP_MSG(P, "then_arm: ");
            dump_expr(P, e->if_.then_arm);
            DUMP_MSG(P, "else_arm: ");
            dump_expr(P, e->if_.else_arm);
            break;
        case kAstForExpr:
            if (e->for_.is_fornum) {
                DUMP_NAME(P, e->for_.name);
                DUMP_MSG(P, "begin: ");
                dump_expr(P, e->for_.fornum.begin);
                DUMP_MSG(P, "end: ");
                dump_expr(P, e->for_.fornum.end);
                DUMP_MSG(P, "step: ");
                dump_expr(P, e->for_.fornum.step);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, e->for_.block);
            } else {
                DUMP_NAME(P, e->for_.name);
                DUMP_MSG(P, "target: ");
                dump_expr(P, e->for_.forin.target);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, e->for_.block);
            }
            break;
        case kAstWhileExpr:
            DUMP_MSG(P, "cond: ");
            dump_expr(P, e->while_.cond);
            DUMP_MSG(P, "block: ");
            DUMP_BLOCK(P, e->while_.block);
            break;
        case kAstReturnExpr:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, e->result.expr);
            break;
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
