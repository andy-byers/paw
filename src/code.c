// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "code.h"
#include "env.h"
#include "mem.h"
#include <limits.h>

static void add_line(FnState *fs)
{
    Lex *lex = fs->lex;
    Proto *p = fs->proto;
    if (fs->nlines == UINT16_MAX) {
        pawX_error(lex, "too many instructions");
    }
    pawM_grow(lex->P, p->lines, fs->nlines, p->nlines);
    p->lines[fs->nlines++] = (struct LineInfo){
        .line = lex->lastline,
        .pc = fs->pc,
    };
}

void pawK_fix_line(FnState *fs, int line)
{
    paw_assert(fs->nlines > 0);
    fs->proto->lines[fs->nlines - 1].line = line;
}

static void add_opcode(FnState *fs, OpCode code)
{
    Lex *lex = fs->lex;
    Proto *p = fs->proto;

    // While code is being generated, the pc is used to track the number of instructions, and
    // the length field the capacity. The length is set to the final pc value before execution.
    pawM_grow(lex->P, p->source, fs->pc, p->length);
    p->source[fs->pc] = code;
    ++fs->pc;
}

void pawK_code_0(FnState *fs, Op op)
{
    add_line(fs);
    add_opcode(fs, create_OP(op));
}

void pawK_code_S(FnState *fs, Op op, int s)
{
    add_line(fs);
    add_opcode(fs, create_S(op, s));
}

void pawK_code_U(FnState *fs, Op op, int u)
{
    add_line(fs);
    add_opcode(fs, create_U(op, u));
}

void pawK_code_AB(FnState *fs, Op op, int a, int b)
{
    add_line(fs);
    add_opcode(fs, create_AB(op, a, b));
}

static Arena *new_arena(paw_Env *P, size_t size)
{
    Arena *a = pawM_new_flex(P, Arena, size, 1);
    memset(a->data, 0, size);
    a->size = size;
    return a;
}

Node *pawK_add_node_aux(Lex *lex, unsigned kind, size_t size, size_t align)
{
    paw_Env *P = lex->P;
    Tree *ast = lex->ast;
    Arena *a = ast->arena;
    const size_t base = (a->used + align - 1) & ~(align - 1);
    if (base + size > a->size) {
        if (base + size > (SIZE_MAX / 2) - 1) {
            pawM_error(P); // sanity check
        }
        const size_t nbytes = (base + size + 1) * 2;
        Arena *anew = new_arena(P, nbytes);
        anew->prev = a;
        a = ast->arena = anew;
    }
    a->used = base + size;
    Node *node = (Node *)&a->data[base];
    node->line = lex->lastline;
    node->kind = kind;
    return node;
}

Node **pawK_nvec_add(paw_Env *P, NodeVec *nvec)
{
    pawM_grow(P, nvec->nodes, nvec->size, nvec->alloc);
    return &nvec->nodes[nvec->size++];
}

#define FIRST_ARENA_SIZE 512

Tree *pawK_new_ast(paw_Env *P)
{
    Tree *tree = pawM_new(P, Tree);
    tree->arena = new_arena(P, FIRST_ARENA_SIZE);
    return tree;
}

void pawK_free_ast(paw_Env *P, Tree *ast)
{
    // Free the NodeVec::nodes allocations.
    for (VecList *v = ast->vecs; v;) {
        VecList *prev = v->prev;
        NodeVec *nv = v->nvec;
        pawM_free_vec(P, nv->nodes, nv->alloc);
        v = prev;
    }
    // Free the list of arenas backing the AST.
    for (Arena *a = ast->arena; a;) {
        Arena *prev = a->prev;
        pawM_free_flex(P, a, a->size, 1);
        a = prev;
    }
    pawM_free(P, ast);
}

// ********************
//     AST visitors
// ********************

static void visit_expr_vec(Visitor *V, NodeVec nvec)
{
    for (int i = 0; i < nvec.size; ++i) {
        V->expr(V, cast_expr(nvec.nodes[i]));
    }
}

static void visit_logical_expr(Visitor *V, LogicalExpr *e)
{
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
}

static void visit_primitive_expr(Visitor *V, PrimitiveExpr *e)
{
    paw_unused(V);
    paw_unused(e);
}

static void visit_chain_expr(Visitor *V, ChainExpr *e)
{
    V->expr(V, e->target);
}

static void visit_cond_expr(Visitor *V, CondExpr *e)
{
    V->expr(V, e->cond);
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
}

static void visit_coalesce_expr(Visitor *V, CoalesceExpr *e)
{
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
}

static void visit_unop_expr(Visitor *V, UnOpExpr *e)
{
    V->expr(V, e->target);
}

static void visit_binop_expr(Visitor *V, BinOpExpr *e)
{
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
}

static void visit_assignment(Visitor *V, Expr *lhs, Expr *rhs)
{
    V->expr(V, lhs);
    V->expr(V, rhs);
}

static void visit_expr_stmt(Visitor *V, ExprStmt *s)
{
    if (s->rhs != NULL) {
        V->assign(V, s->lhs, s->rhs); // assignment
    } else {
        V->expr(V, s->lhs); // function call
    }
}

static void visit_attr_stmt(Visitor *V, AttrStmt *s)
{
    paw_unused(V);
    paw_unused(s);
}

static void visit_class_stmt(Visitor *V, ClassStmt *s)
{
    V->stmt_vec(V, s->attrs);
}

static void visit_stmt_vec(Visitor *V, NodeVec param)
{
    for (int i = 0; i < param.size; ++i) {
        V->stmt(V, cast_stmt(param.nodes[i]));
    }
}

static void visit_block_stmt(Visitor *V, Block *b)
{
    V->stmt_vec(V, b->stmts);
}

static void visit_def_stmt(Visitor *V, DefStmt *s)
{
    V->expr(V, s->init);
}

static void visit_param_stmt(Visitor *V, ParamStmt *s)
{
    paw_unused(V);
    paw_unused(s);
}

static void visit_return_stmt(Visitor *V, ReturnStmt *s)
{
    V->expr(V, s->expr);
}

static void visit_call_expr(Visitor *V, CallExpr *e)
{
    V->expr(V, e->target);
    V->expr_vec(V, e->param);
}

static void visit_fn_expr(Visitor *V, FnExpr *e)
{
    V->stmt_vec(V, e->fn.param); // parameters
    V->block_stmt(V, e->fn.body); // function body
}

static void visit_var_expr(Visitor *V, VarExpr *e)
{
    paw_unused(V);
    paw_unused(e);
}

static void visit_fn_stmt(Visitor *V, FnStmt *s)
{
    V->stmt_vec(V, s->fn.param); // parameters
    V->block_stmt(V, s->fn.body); // function body
}

static void visit_ifelse_stmt(Visitor *V, IfElseStmt *s)
{
    V->expr(V, s->cond);
    V->stmt(V, s->then_arm);
    V->stmt(V, s->else_arm);
}

static void visit_while_stmt(Visitor *V, WhileStmt *s)
{
    V->expr(V, s->cond);
    V->block_stmt(V, s->block);
}

static void visit_dowhile_stmt(Visitor *V, WhileStmt *s)
{
    V->block_stmt(V, s->block);
    V->expr(V, s->cond);
}

static void visit_label_stmt(Visitor *V, LabelStmt *s)
{
    paw_unused(V);
    paw_unused(s);
}

static void visit_for_stmt(Visitor *V, ForStmt *s)
{
    if (s->kind == STMT_FORNUM) {
        V->expr(V, s->fornum.begin);
        V->expr(V, s->fornum.end);
        V->expr(V, s->fornum.step);
    } else {
        V->expr(V, s->forin.target);
    }
    V->block_stmt(V, s->block);
}

static void visit_array_expr(Visitor *V, ArrayExpr *e)
{
    V->expr_vec(V, e->items);
}

static void visit_map_expr(Visitor *V, MapExpr *e)
{
    V->expr_vec(V, e->items);
}

static void visit_index_expr(Visitor *V, IndexExpr *e)
{
    V->expr(V, e->target);
    V->expr(V, e->first);
    V->expr(V, e->second);
}

static void visit_access_expr(Visitor *V, AccessExpr *e)
{
    V->expr(V, e->target);
}

void pawK_visit_expr(Visitor *V, Expr *expr)
{
    if (expr == NULL) {
        return;
    }
    switch (expr->kind) {
        case EXPR_PRIMITIVE:
            V->primitive_expr(V, cast_to(expr, PrimitiveExpr));
            break;
        case EXPR_CHAIN:
            V->chain_expr(V, cast_to(expr, ChainExpr));
            break;
        case EXPR_COALESCE:
            V->coalesce_expr(V, cast_to(expr, CoalesceExpr));
            break;
        case EXPR_LOGICAL:
            V->logical_expr(V, cast_to(expr, LogicalExpr));
            break;
        case EXPR_UNOP:
            V->unop_expr(V, cast_to(expr, UnOpExpr));
            break;
        case EXPR_BINOP:
            V->binop_expr(V, cast_to(expr, BinOpExpr));
            break;
        case EXPR_CALL:
            V->call_expr(V, cast_to(expr, CallExpr));
            break;
        case EXPR_COND:
            V->cond_expr(V, cast_to(expr, CondExpr));
            break;
        case EXPR_FN:
            V->fn_expr(V, cast_to(expr, FnExpr));
            break;
        case EXPR_VAR:
            V->var_expr(V, cast_to(expr, VarExpr));
            break;
        case EXPR_ARRAY:
            V->array_expr(V, cast_to(expr, ArrayExpr));
            break;
        case EXPR_MAP:
            V->map_expr(V, cast_to(expr, MapExpr));
            break;
        case EXPR_INDEX:
            V->index_expr(V, cast_to(expr, IndexExpr));
            break;
        default:
            paw_assert(expr->kind == EXPR_ACCESS);
            V->access_expr(V, cast_to(expr, AccessExpr));
    }
}

void pawK_visit_stmt(Visitor *V, Stmt *stmt)
{
    if (stmt == NULL) {
        return;
    }
    switch (stmt->kind) {
        case STMT_EXPR:
            V->expr_stmt(V, cast_to(stmt, ExprStmt));
            break;
        case STMT_RETURN:
            V->return_stmt(V, cast_to(stmt, ReturnStmt));
            break;
        case STMT_IFELSE:
            V->ifelse_stmt(V, cast_to(stmt, IfElseStmt));
            break;
        case STMT_FORIN:
        case STMT_FORNUM:
            V->for_stmt(V, cast_to(stmt, ForStmt));
            break;
        case STMT_WHILE:
            V->while_stmt(V, cast_to(stmt, WhileStmt));
            break;
        case STMT_DOWHILE:
            V->dowhile_stmt(V, cast_to(stmt, WhileStmt));
            break;
        case STMT_LABEL:
            V->label_stmt(V, cast_to(stmt, LabelStmt));
            break;
        case STMT_PARAM:
            V->param_stmt(V, cast_to(stmt, ParamStmt));
            break;
        case STMT_DEF:
            V->def_stmt(V, cast_to(stmt, DefStmt));
            break;
        case STMT_FN:
            V->fn_stmt(V, cast_to(stmt, FnStmt));
            break;
        case STMT_CLASS:
            V->class_stmt(V, cast_to(stmt, ClassStmt));
            break;
        case STMT_ATTR:
            V->attr_stmt(V, cast_to(stmt, AttrStmt));
            break;
        default:
            paw_assert(stmt->kind == STMT_BLOCK);
            V->block_stmt(V, cast_to(stmt, Block));
    }
}

void pawK_init_visitor(Visitor *V, Lex *lex)
{
    *V = (Visitor){
        .lex = lex, 

        .expr = pawK_visit_expr,
        .stmt = pawK_visit_stmt,
        .assign = visit_assignment,

        .expr_vec = visit_expr_vec,
        .primitive_expr = visit_primitive_expr,
        .chain_expr = visit_chain_expr,
        .coalesce_expr = visit_coalesce_expr,
        .logical_expr = visit_logical_expr,
        .unop_expr = visit_unop_expr,
        .binop_expr = visit_binop_expr,
        .call_expr = visit_call_expr,
        .cond_expr = visit_cond_expr,
        .fn_expr = visit_fn_expr,
        .var_expr = visit_var_expr,
        .array_expr = visit_array_expr,
        .map_expr = visit_map_expr,
        .index_expr = visit_index_expr,
        .access_expr = visit_access_expr,

        .stmt_vec = visit_stmt_vec,
        .expr_stmt = visit_expr_stmt,
        .return_stmt = visit_return_stmt,
        .ifelse_stmt = visit_ifelse_stmt,
        .for_stmt = visit_for_stmt,
        .while_stmt = visit_while_stmt,
        .dowhile_stmt = visit_dowhile_stmt,
        .label_stmt = visit_label_stmt,
        .fn_stmt = visit_fn_stmt,
        .param_stmt = visit_param_stmt,
        .def_stmt = visit_def_stmt,
        .attr_stmt = visit_attr_stmt,
        .class_stmt = visit_class_stmt,
        .block_stmt = visit_block_stmt,
    };
}

void pawK_visit(Visitor *V, Tree *tree)
{
    V->stmt_vec(V, tree->root);
}
