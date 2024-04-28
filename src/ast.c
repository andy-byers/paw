// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ast.h"
#include "mem.h"

#define FIRST_ARENA_SIZE 512

Ast *pawA_new_ast(Lex *lex)
{
    paw_Env *P = env(lex);
    Ast *tree = pawM_new(P, Ast);
    tree->lex = lex;
    // initialize memory pools for storing AST components
    pawK_pool_init(P, &tree->nodes, FIRST_ARENA_SIZE, sizeof(AstDecl));
    pawK_pool_init(P, &tree->symbols, FIRST_ARENA_SIZE, sizeof(Symbol));
    pawK_pool_init(P, &tree->sequences, FIRST_ARENA_SIZE, sizeof(void *) * 8);
    return tree;
}

void pawA_free_ast(Ast *ast)
{
    paw_Env *P = env(ast->lex);
    pawK_pool_uninit(P, &ast->nodes);
    pawK_pool_uninit(P, &ast->symbols);
    pawK_pool_uninit(P, &ast->sequences);
    pawM_free(P, ast);
}

#define make_node_constructor(name, T) \
    T *pawA_new_ ## name(Ast *ast, T ## Kind kind) \
    { \
        T *r = pawK_pool_alloc(env((ast)->lex), &(ast)->nodes, sizeof(T), paw_alignof(T)); \
        r->hdr.line = (ast)->lex->line; \
        r->hdr.kind = kind; \
        return r; \
    }
make_node_constructor(expr, AstExpr)
make_node_constructor(decl, AstDecl)
make_node_constructor(stmt, AstStmt)

#define make_list_constructor(name, T) \
    T *pawA_new_ ## name ## _list(Ast *ast) \
    { \
        return pawK_pool_alloc(env((ast)->lex), &(ast)->nodes, sizeof(T), paw_alignof(T)); \
    }
make_list_constructor(expr, AstExprList)
make_list_constructor(decl, AstDeclList)
make_list_constructor(stmt, AstStmtList)

void *pawA_new_pointer_vec(Ast *ast, int nptrs)
{
    return pawK_pool_alloc(env(ast->lex), &ast->sequences, 
                           sizeof(void *) * cast_size(nptrs), paw_alignof(void *));
}

Symbol *pawA_new_symbol(Lex *lex)
{
    return pawK_pool_alloc(env(lex), &lex->pm->ast->symbols, 
                           sizeof(Symbol), paw_alignof(Symbol));
}

#define make_list_visitor(name, base, T, List, source, link) \
    static void visit_ ## name ## _list_aux(AstVisitor *V, List *list, T ## Pass cb) \
    { \
        T *head = list->first; \
        while (head != NULL) { \
            cb(V, head); \
            head = head->source.link; \
        } \
    }
make_list_visitor(decl, decl, AstDecl, AstDeclList, hdr, next)
make_list_visitor(expr, expr, AstExpr, AstExprList, hdr, next)
make_list_visitor(stmt, stmt, AstStmt, AstStmtList, hdr, next)
make_list_visitor(method, decl, AstDecl, AstDeclList, func, sibling)

#define visit_stmts(V, list) (V)->visit_stmt_list(V, list, (V)->visit_stmt)
#define visit_exprs(V, list) (V)->visit_expr_list(V, list, (V)->visit_expr)
#define visit_decls(V, list) (V)->visit_decl_list(V, list, (V)->visit_decl)
#define visit_methods(V, list) (V)->visit_method_list(V, list, (V)->visit_decl)

static void visit_block_stmt(AstVisitor *V, Block *s)
{
    visit_stmts(V, s->stmts);
}

static void visit_logical_expr(AstVisitor *V, LogicalExpr *e)
{
    V->visit_expr(V, e->lhs);
    V->visit_expr(V, e->rhs);
}

static void visit_item_expr(AstVisitor *V, ItemExpr *e)
{
    V->visit_expr(V, e->value);
}

static void visit_literal_expr(AstVisitor *V, LiteralExpr *e)
{
    switch (e->lit_kind) {
        case LIT_BASIC:
            break;
        case LIT_TUPLE:
        case LIT_ARRAY:
            paw_assert(0); // TODO
            break; // TODO
        default:
            paw_assert(e->lit_kind == LIT_COMPOSITE);
            V->visit_expr(V, e->comp.target);
            visit_exprs(V, e->comp.items);
    }
}

static void visit_chain_expr(AstVisitor *V, ChainExpr *e)
{
    V->visit_expr(V, e->target);
}

static void visit_cond_expr(AstVisitor *V, CondExpr *e)
{
    V->visit_expr(V, e->cond);
    V->visit_expr(V, e->lhs);
    V->visit_expr(V, e->rhs);
}

static void visit_unop_expr(AstVisitor *V, UnOpExpr *e)
{
    V->visit_expr(V, e->target);
}

static void visit_binop_expr(AstVisitor *V, BinOpExpr *e)
{
    V->visit_expr(V, e->lhs);
    V->visit_expr(V, e->rhs);
}

static void visit_expr_stmt(AstVisitor *V, AstExprStmt *s)
{
    V->visit_expr(V, s->lhs);
    V->visit_expr(V, s->rhs);
}

static void visit_signature_expr(AstVisitor *V, FuncType *e)
{
    visit_exprs(V, e->params);
    V->visit_expr(V, e->return_);
}

static void visit_type_name_expr(AstVisitor *V, TypeName *e)
{
    if (e->args != NULL) {
        visit_exprs(V, e->args);
    }
}

static void visit_field_decl(AstVisitor *V, FieldDecl *d)
{
    V->visit_expr(V, d->tag);
}

static void visit_type_decl(AstVisitor *V, TypeDecl *d)
{
    if (d->generics != NULL) {
        visit_decls(V, d->generics);
    }
    V->visit_expr(V, d->rhs);
}

static void visit_generic_decl(AstVisitor *V, GenericDecl *d)
{
    paw_unused(V);
    paw_unused(d);
}

static void visit_struct_decl(AstVisitor *V, StructDecl *d)
{
    if (d->is_poly) {
        visit_decls(V, d->generics);
    }
    visit_decls(V, d->fields);
    visit_methods(V, d->methods);
}

static void visit_var_decl(AstVisitor *V, VarDecl *d)
{
    V->visit_expr(V, d->init);
    V->visit_expr(V, d->tag);
}

static void visit_return_stmt(AstVisitor *V, ReturnStmt *s)
{
    V->visit_expr(V, s->expr);
}

static void visit_call_expr(AstVisitor *V, CallExpr *e)
{
    V->visit_expr(V, e->target);
    visit_exprs(V, e->args);
}

static void visit_ident_expr(AstVisitor *V, AstIdent *e)
{
    paw_unused(V);
    paw_unused(e);
}

static void visit_func_decl(AstVisitor *V, FuncDecl *d)
{
    if (d->is_poly) {
        visit_decls(V, d->generics);
    }
    visit_decls(V, d->params);
    V->visit_expr(V, d->return_);
    V->visit_block_stmt(V, d->body);
}

static void visit_if_stmt(AstVisitor *V, IfStmt *s)
{
    V->visit_expr(V, s->cond);
    V->visit_stmt(V, s->then_arm);
    V->visit_stmt(V, s->else_arm);
}

static void visit_while_stmt(AstVisitor *V, WhileStmt *s)
{
    V->visit_expr(V, s->cond);
    V->visit_block_stmt(V, s->block);
}

static void visit_dowhile_stmt(AstVisitor *V, WhileStmt *s)
{
    V->visit_block_stmt(V, s->block);
    V->visit_expr(V, s->cond);
}

static void visit_label_stmt(AstVisitor *V, LabelStmt *s)
{
    paw_unused(V);
    paw_unused(s);
}

static void visit_for_stmt(AstVisitor *V, ForStmt *s)
{
    if (s->kind == STMT_FORNUM) {
        V->visit_expr(V, s->fornum.begin);
        V->visit_expr(V, s->fornum.end);
        V->visit_expr(V, s->fornum.step);
    } else {
        V->visit_expr(V, s->forin.target);
    }
    V->visit_block_stmt(V, s->block);
}

static void visit_index_expr(AstVisitor *V, Index *e)
{
    V->visit_expr(V, e->target);
    visit_exprs(V, e->elems);
}

static void visit_selector_expr(AstVisitor *V, Selector *e)
{
    V->visit_expr(V, e->target);
}

static void visit_expr(AstVisitor *V, AstExpr *expr)
{
    if (expr == NULL) {
        return;
    }
    switch (a_kind(expr)) {
        case EXPR_LITERAL:
            V->visit_literal_expr(V, &expr->literal);
            break;
        case EXPR_CHAIN:
            V->visit_chain_expr(V, &expr->chain);
            break;
        case EXPR_LOGICAL:
            V->visit_logical_expr(V, &expr->logical);
            break;
        case EXPR_UNOP:
            V->visit_unop_expr(V, &expr->unop);
            break;
        case EXPR_BINOP:
            V->visit_binop_expr(V, &expr->binop);
            break;
        case EXPR_CALL:
            V->visit_call_expr(V, &expr->call);
            break;
        case EXPR_COND:
            V->visit_cond_expr(V, &expr->cond);
            break;
        case EXPR_NAME:
            V->visit_ident_expr(V, &expr->name);
            break;
        case EXPR_INDEX:
            V->visit_index_expr(V, &expr->index);
            break;
        case EXPR_ITEM:
            V->visit_item_expr(V, &expr->item);
            break;
        case EXPR_FUNC_TYPE:
            V->visit_signature_expr(V, &expr->func);
            break;
        case EXPR_TYPE_NAME:
            V->visit_type_name_expr(V, &expr->type_name);
            break;
        default:
            paw_assert(a_kind(expr) == EXPR_SELECTOR);
            V->visit_selector_expr(V, &expr->selector);
    }
}

static void visit_decl(AstVisitor *V, AstDecl *decl)
{
    if (decl == NULL) {
        return;
    }
    switch (a_kind(decl)) {
        case DECL_VAR:
            V->visit_var_decl(V, &decl->var);
            break;
        case DECL_FIELD:
            V->visit_field_decl(V, &decl->field);
            break;
        case DECL_TYPE:
            V->visit_type_decl(V, &decl->type);
            break;
        case DECL_GENERIC:
            V->visit_generic_decl(V, &decl->generic);
            break;
        case DECL_FUNC:
            V->visit_func_decl(V, &decl->func);
            break;
        default:
            paw_assert(a_kind(decl) == DECL_STRUCT);
            V->visit_struct_decl(V, &decl->struct_);
    }
}

static void visit_decl_stmt(AstVisitor *V, AstDeclStmt *s)
{
    V->visit_decl(V, s->decl);
    cast_stmt(s);
}

static void visit_stmt(AstVisitor *V, AstStmt *stmt)
{
    if (stmt == NULL) {
        return;
    }
    switch (a_kind(stmt)) {
        case STMT_EXPR:
            V->visit_expr_stmt(V, &stmt->expr);
            break;
        case STMT_DECL:
            V->visit_decl_stmt(V, &stmt->decl);
            break;
        case STMT_RETURN:
            V->visit_return_stmt(V, &stmt->return_);
            break;
        case STMT_IF:
            V->visit_if_stmt(V, &stmt->if_);
            break;
        case STMT_FORIN:
        case STMT_FORNUM:
            V->visit_for_stmt(V, &stmt->for_);
            break;
        case STMT_WHILE:
            V->visit_while_stmt(V, &stmt->while_);
            break;
        case STMT_DOWHILE:
            V->visit_dowhile_stmt(V, &stmt->while_);
            break;
        case STMT_LABEL:
            V->visit_label_stmt(V, &stmt->label);
            break;
        default:
            paw_assert(a_kind(stmt) == STMT_BLOCK);
            V->visit_block_stmt(V, &stmt->block);
    }
}

void pawA_visitor_init(AstVisitor *V, Ast *ast, AstState state)
{
    *V = (AstVisitor){
        .ast = ast,
        .state = state,
        .visit_expr = visit_expr,
        .visit_decl = visit_decl,
        .visit_stmt = visit_stmt,
        .visit_expr_list = visit_expr_list_aux,
        .visit_decl_list = visit_decl_list_aux,
        .visit_stmt_list = visit_stmt_list_aux,
        .visit_method_list = visit_method_list_aux,
        .visit_literal_expr = visit_literal_expr,
        .visit_logical_expr = visit_logical_expr,
        .visit_ident_expr = visit_ident_expr,
        .visit_chain_expr = visit_chain_expr,
        .visit_unop_expr = visit_unop_expr,
        .visit_binop_expr = visit_binop_expr,
        .visit_cond_expr = visit_cond_expr,
        .visit_call_expr = visit_call_expr,
        .visit_index_expr = visit_index_expr,
        .visit_selector_expr = visit_selector_expr,
        .visit_item_expr = visit_item_expr,
        .visit_type_name_expr = visit_type_name_expr,
        .visit_signature_expr = visit_signature_expr,
        .visit_block_stmt = visit_block_stmt,
        .visit_expr_stmt = visit_expr_stmt,
        .visit_decl_stmt = visit_decl_stmt,
        .visit_if_stmt = visit_if_stmt,
        .visit_for_stmt = visit_for_stmt,
        .visit_while_stmt = visit_while_stmt,
        .visit_dowhile_stmt = visit_dowhile_stmt,
        .visit_label_stmt = visit_label_stmt,
        .visit_return_stmt = visit_return_stmt,
        .visit_var_decl = visit_var_decl,
        .visit_func_decl = visit_func_decl,
        .visit_struct_decl = visit_struct_decl,
        .visit_field_decl = visit_field_decl,
        .visit_generic_decl = visit_generic_decl,
        .visit_type_decl = visit_type_decl,
    };
}

void pawA_visit(AstVisitor *V)
{
    Ast *ast = V->ast;
    visit_stmts(V, ast->stmts);
}

// Generate code for folding a linked list of AST nodes
//
// fold_*_list_aux: Pass over the list, calling the supplied callback on each node. The
//     callback should return (a) the next node n->source.link (with text replacement) 
//     if the current node n should be removed, (b) n if no folding needs to be 
//     performed, or (c) a freshly-allocated node if creating a new list.
// visit_*_list: Run fold_*_list_aux with the default callback.
#define make_list_folder(name, base, T, List, source, link) \
    static void fold_ ## name ## _list(AstFolder *F, List *list, T ## Fold cb) \
    { \
        int count = 0; \
        T *head = list->first; \
        T *prev; \
        for (int i = 0; head != NULL; ++i) { \
            T *next = cb(F, head); \
            if (i == 0) { \
                list->first = next; \
            } else { \
                prev->source.link = next; \
            } \
            if (next != NULL) { \
                head = head->source.link; \
                ++count; \
            } else { \
                head = NULL; \
            } \
            prev = next; \
        } \
        list->count = count; \
    } \
    static void fold_ ## name ## s(AstFolder *F, List *list) \
    { \
        fold_ ## name ## _list(F, list, F->fold_ ## base);  \
    }
make_list_folder(decl, decl, AstDecl, AstDeclList, hdr, next)
make_list_folder(expr, expr, AstExpr, AstExprList, hdr, next)
make_list_folder(stmt, stmt, AstStmt, AstStmtList, hdr, next)
make_list_folder(method, decl, AstDecl, AstDeclList, func, sibling)

static AstStmt *fold_block_stmt(AstFolder *F, Block *s)
{
    fold_stmts(F, s->stmts);
    return cast_stmt(s);
}
#define fold_block(F, s) cast((F)->fold_block_stmt(F, s), Block *)

static AstExpr *fold_logical_expr(AstFolder *F, LogicalExpr *e)
{
    e->lhs = F->fold_expr(F, e->lhs);
    e->rhs = F->fold_expr(F, e->rhs);
    return cast_expr(e);
}

static AstExpr *fold_item_expr(AstFolder *F, ItemExpr *e)
{
    e->value = F->fold_expr(F, e->value);
    return cast_expr(e);
}

static AstExpr *fold_literal_expr(AstFolder *F, LiteralExpr *e)
{
    switch (e->lit_kind) {
        case LIT_BASIC:
            break;
        case LIT_TUPLE:
        case LIT_ARRAY:
            paw_assert(0); // TODO
            break; // TODO
        default:
            paw_assert(e->lit_kind == LIT_COMPOSITE);
            e->comp.target = F->fold_expr(F, e->comp.target);
            fold_exprs(F, e->comp.items);
    }
    return cast_expr(e);
}

static AstExpr *fold_chain_expr(AstFolder *F, ChainExpr *e)
{
    e->target = F->fold_expr(F, e->target);
    return cast_expr(e);
}

static AstExpr *fold_cond_expr(AstFolder *F, CondExpr *e)
{
    e->cond = F->fold_expr(F, e->cond);
    e->lhs = F->fold_expr(F, e->lhs);
    e->rhs = F->fold_expr(F, e->rhs);
    return cast_expr(e);
}

static AstExpr *fold_unop_expr(AstFolder *F, UnOpExpr *e)
{
    e->target = F->fold_expr(F, e->target);
    return cast_expr(e);
}

static AstExpr *fold_binop_expr(AstFolder *F, BinOpExpr *e)
{
    e->lhs = F->fold_expr(F, e->lhs);
    e->rhs = F->fold_expr(F, e->rhs);
    return cast_expr(e);
}

static AstStmt *fold_expr_stmt(AstFolder *F, AstExprStmt *s)
{
    s->lhs = F->fold_expr(F, s->lhs);
    s->rhs = F->fold_expr(F, s->rhs);
    return cast_stmt(s);
}

static AstExpr *fold_signature_expr(AstFolder *F, FuncType *e)
{
    fold_exprs(F, e->params);
    e->return_ = F->fold_expr(F, e->return_);
    return cast_expr(e);
}

static AstExpr *fold_type_name_expr(AstFolder *F, TypeName *e)
{
    if (e->args != NULL) {
        fold_exprs(F, e->args);
    }
    return cast_expr(e);
}

static AstDecl *fold_field_decl(AstFolder *F, FieldDecl *d)
{
    d->tag = F->fold_expr(F, d->tag);
    return cast_decl(d);
}

static AstDecl *fold_type_decl(AstFolder *F, TypeDecl *d)
{
    fold_decls(F, d->generics);
    d->rhs = F->fold_expr(F, d->rhs);
    return cast_decl(d);
}

static AstDecl *fold_generic_decl(AstFolder *F, GenericDecl *d)
{
    paw_unused(F);
    return cast_decl(d);
}

static AstDecl *fold_struct_decl(AstFolder *F, StructDecl *d)
{
    fold_decls(F, d->generics);
    fold_decls(F, d->fields);
    fold_methods(F, d->methods);
    return cast_decl(d);
}

static AstDecl *fold_var_decl(AstFolder *F, VarDecl *d)
{
    d->init = F->fold_expr(F, d->init);
    d->tag = F->fold_expr(F, d->tag);
    return cast_decl(d);
}

static AstStmt *fold_return_stmt(AstFolder *F, ReturnStmt *s)
{
    s->expr = F->fold_expr(F, s->expr);
    return cast_stmt(s);
}

static AstExpr *fold_call_expr(AstFolder *F, CallExpr *e)
{
    e->target = F->fold_expr(F, e->target);
    fold_exprs(F, e->args);
    return cast_expr(e);
}

static AstExpr *fold_ident_expr(AstFolder *F, AstIdent *e)
{
    paw_unused(F);
    return cast_expr(e);
}

static AstDecl *fold_func_decl(AstFolder *F, FuncDecl *d)
{
    fold_decls(F, d->generics);
    fold_decls(F, d->params);
    d->return_ = F->fold_expr(F, d->return_);
    d->body = fold_block(F, d->body);
    return cast_decl(d);
}

static AstStmt *fold_if_stmt(AstFolder *F, IfStmt *s)
{
    s->cond = F->fold_expr(F, s->cond);
    s->then_arm = F->fold_stmt(F, s->then_arm);
    s->else_arm = F->fold_stmt(F, s->else_arm);
    return cast_stmt(s);
}

static AstStmt *fold_while_stmt(AstFolder *F, WhileStmt *s)
{
    s->cond = F->fold_expr(F, s->cond);
    s->block = fold_block(F, s->block);
    return cast_stmt(s);
}

static AstStmt *fold_label_stmt(AstFolder *F, LabelStmt *s)
{
    paw_unused(F);
    return cast_stmt(s);
}

static AstStmt *fold_for_stmt(AstFolder *F, ForStmt *s)
{
    if (s->kind == STMT_FORNUM) {
        s->fornum.begin = F->fold_expr(F, s->fornum.begin);
        s->fornum.end = F->fold_expr(F, s->fornum.end);
        s->fornum.step = F->fold_expr(F, s->fornum.step);
    } else {
        s->forin.target = F->fold_expr(F, s->forin.target);
    }
    s->block = fold_block(F, s->block);
    return cast_stmt(s);
}

static AstExpr *fold_index_expr(AstFolder *F, Index *e)
{
    e->target = F->fold_expr(F, e->target);
    fold_exprs(F, e->elems);
    return cast_expr(e);
}

static AstExpr *fold_selector_expr(AstFolder *F, Selector *e)
{
    e->target = F->fold_expr(F, e->target);
    return cast_expr(e);
}

static AstExpr *fold_expr(AstFolder *F, AstExpr *expr)
{
    if (expr == NULL) {
        return NULL;
    }
    switch (a_kind(expr)) {
        case EXPR_LITERAL:
            return F->fold_literal_expr(F, &expr->literal);
        case EXPR_CHAIN:
            return F->fold_chain_expr(F, &expr->chain);
        case EXPR_LOGICAL:
            return F->fold_logical_expr(F, &expr->logical);
        case EXPR_UNOP:
            return F->fold_unop_expr(F, &expr->unop);
        case EXPR_BINOP:
            return F->fold_binop_expr(F, &expr->binop);
        case EXPR_CALL:
            return F->fold_call_expr(F, &expr->call);
        case EXPR_COND:
            return F->fold_cond_expr(F, &expr->cond);
        case EXPR_NAME:
            return F->fold_ident_expr(F, &expr->name);
        case EXPR_INDEX:
            return F->fold_index_expr(F, &expr->index);
        case EXPR_ITEM:
            return F->fold_item_expr(F, &expr->item);
        case EXPR_FUNC_TYPE:
            return F->fold_signature_expr(F, &expr->func);
        case EXPR_TYPE_NAME:
            return F->fold_type_name_expr(F, &expr->type_name);
        default:
            paw_assert(a_kind(expr) == EXPR_SELECTOR);
            return F->fold_selector_expr(F, &expr->selector);
    }
}

static AstDecl *fold_decl(AstFolder *F, AstDecl *decl)
{
    if (decl == NULL) {
        return NULL;
    }
    switch (a_kind(decl)) {
        case DECL_VAR:
            return F->fold_var_decl(F, &decl->var);
        case DECL_FIELD:
            return F->fold_field_decl(F, &decl->field);
        case DECL_TYPE:
            return F->fold_type_decl(F, &decl->type);
        case DECL_GENERIC:
            return F->fold_generic_decl(F, &decl->generic);
        case DECL_FUNC:
            return F->fold_func_decl(F, &decl->func);
        default:
            paw_assert(a_kind(decl) == DECL_STRUCT);
            return F->fold_struct_decl(F, &decl->struct_);
    }
}

static AstStmt *fold_decl_stmt(AstFolder *F, AstDeclStmt *s)
{
    s->decl = F->fold_decl(F, s->decl);
    return cast_stmt(s);
}

static AstStmt *fold_stmt(AstFolder *F, AstStmt *stmt)
{
    if (stmt == NULL) {
        return NULL;
    }
    switch (a_kind(stmt)) {
        case STMT_EXPR:
            return F->fold_expr_stmt(F, &stmt->expr);
        case STMT_DECL:
            return F->fold_decl_stmt(F, &stmt->decl);
        case STMT_RETURN:
            return F->fold_return_stmt(F, &stmt->return_);
        case STMT_IF:
            return F->fold_if_stmt(F, &stmt->if_);
        case STMT_FORIN:
        case STMT_FORNUM:
            return F->fold_for_stmt(F, &stmt->for_);
        case STMT_WHILE:
        case STMT_DOWHILE:
            return F->fold_while_stmt(F, &stmt->while_);
        case STMT_LABEL:
            return F->fold_label_stmt(F, &stmt->label);
        default:
            paw_assert(a_kind(stmt) == STMT_BLOCK);
            return F->fold_block_stmt(F, &stmt->block);
    }
}

void pawA_fold_init(AstFolder *F, Ast *ast, AstState state)
{
    *F = (AstFolder){
        .ast = ast,
        .state = state,
        .fold_expr = fold_expr,
        .fold_decl = fold_decl,
        .fold_stmt = fold_stmt,
        .fold_expr_list = fold_expr_list,
        .fold_decl_list = fold_decl_list,
        .fold_stmt_list = fold_stmt_list,
        .fold_method_list = fold_method_list,
        .fold_literal_expr = fold_literal_expr,
        .fold_logical_expr = fold_logical_expr,
        .fold_ident_expr = fold_ident_expr,
        .fold_chain_expr = fold_chain_expr,
        .fold_unop_expr = fold_unop_expr,
        .fold_binop_expr = fold_binop_expr,
        .fold_cond_expr = fold_cond_expr,
        .fold_call_expr = fold_call_expr,
        .fold_index_expr = fold_index_expr,
        .fold_selector_expr = fold_selector_expr,
        .fold_item_expr = fold_item_expr,
        .fold_type_name_expr = fold_type_name_expr,
        .fold_signature_expr = fold_signature_expr,
        .fold_block_stmt = fold_block_stmt,
        .fold_expr_stmt = fold_expr_stmt,
        .fold_decl_stmt = fold_decl_stmt,
        .fold_if_stmt = fold_if_stmt,
        .fold_for_stmt = fold_for_stmt,
        .fold_while_stmt = fold_while_stmt,
        .fold_label_stmt = fold_label_stmt,
        .fold_return_stmt = fold_return_stmt,
        .fold_var_decl = fold_var_decl,
        .fold_func_decl = fold_func_decl,
        .fold_struct_decl = fold_struct_decl,
        .fold_field_decl = fold_field_decl,
        .fold_generic_decl = fold_generic_decl,
        .fold_type_decl = fold_type_decl,
    };
}

void pawA_fold(AstFolder *F)
{
    Ast *ast = F->ast;
    fold_stmts(F, ast->stmts);
}

// ****************************
//     AST stenciling routines
// ****************************

typedef struct Stenciler {
    Lex *lex; // lexical state
    AstDecl *struct_; // enclosing struct AstDecl
    Ast *ast; // AST being copied
} Stenciler;

#define make_stencil_prep(name, T) \
    static T *stencil_prep_ ## name ## _aux(AstFolder *F, T *t) \
    { \
        T *r = pawA_new_ ## name(F->ast, a_kind(t)); \
        r->hdr.kind = t->hdr.kind; \
        r->hdr.line = t->hdr.line; \
        return r; \
    }
make_stencil_prep(expr, AstExpr)
make_stencil_prep(decl, AstDecl)
make_stencil_prep(stmt, AstStmt)

// Helpers for stenciling: create a new node of the given type and kind,
// and copy the common fields
#define stencil_prep_expr(F, e) stencil_prep_expr_aux(F, cast_expr(e))
#define stencil_prep_decl(F, d) stencil_prep_decl_aux(F, cast_decl(d))
#define stencil_prep_stmt(F, s) stencil_prep_stmt_aux(F, cast_stmt(s))

#define make_stencil_list(name, base, T) \
    static T ## List *stencil_ ## name ## s(AstFolder *F, T ## List *old_list) \
    { \
        if (old_list == NULL) { \
            return NULL; \
        } \
        T ## List *new_list = pawA_new_ ## base ## _list(F->ast); \
        new_list->first = old_list->first; \
        F->fold_ ## name ## _list(F, new_list, F->fold_ ## base); \
        return new_list; \
    }
make_stencil_list(decl, decl, AstDecl)
make_stencil_list(expr, expr, AstExpr)
make_stencil_list(stmt, stmt, AstStmt)
make_stencil_list(method, decl, AstDecl)

static AstStmt *stencil_block_stmt(AstFolder *F, Block *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    r->block.stmts = stencil_stmts(F, s->stmts);
    return r;
}
#define stencil_block(F, s) cast((F)->fold_block_stmt(F, s), Block *)

static AstExpr *stencil_logical_expr(AstFolder *F, LogicalExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->logical.lhs = F->fold_expr(F, e->lhs);
    r->logical.rhs = F->fold_expr(F, e->rhs);
    return r;
}

static AstExpr *stencil_item_expr(AstFolder *F, ItemExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->item.value = F->fold_expr(F, e->value);
    r->item.name = e->name;
    r->item.index = e->index;
    return r;
}

static AstExpr *stencil_literal_expr(AstFolder *F, LiteralExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->literal.lit_kind = e->lit_kind;
    switch (e->lit_kind) {
        case LIT_BASIC:
            r->literal.basic = e->basic;
            break;
        case LIT_TUPLE:
        case LIT_ARRAY:
            paw_assert(0); // TODO
            break; // TODO
        default:
            paw_assert(e->lit_kind == LIT_COMPOSITE);
            r->literal.comp.target = F->fold_expr(F, e->comp.target);
            r->literal.comp.items = stencil_exprs(F, e->comp.items);
            break;
    }
    return r;
}

static AstExpr *stencil_chain_expr(AstFolder *F, ChainExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->chain.target = F->fold_expr(F, e->target);
    return r;
}

static AstExpr *stencil_cond_expr(AstFolder *F, CondExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->cond.cond = F->fold_expr(F, e->cond);
    r->cond.lhs = F->fold_expr(F, e->lhs);
    r->cond.rhs = F->fold_expr(F, e->rhs);
    return r;
}

static AstExpr *stencil_unop_expr(AstFolder *F, UnOpExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->unop.target = F->fold_expr(F, e->target);
    r->unop.op = e->op;
    return r;
}

static AstExpr *stencil_binop_expr(AstFolder *F, BinOpExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->binop.lhs = F->fold_expr(F, e->lhs);
    r->binop.rhs = F->fold_expr(F, e->rhs);
    r->binop.op = e->op;
    return r;
}

static AstStmt *stencil_expr_stmt(AstFolder *F, AstExprStmt *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    r->expr.lhs = F->fold_expr(F, s->lhs);
    r->expr.rhs = F->fold_expr(F, s->rhs);
    return r;
}

static AstExpr *stencil_signature_expr(AstFolder *F, FuncType *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->func.params = stencil_exprs(F, e->params);
    r->func.return_ = F->fold_expr(F, e->return_);
    return r;
}

static AstExpr *stencil_type_name_expr(AstFolder *F, TypeName *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->type_name.name = e->name;
    r->type_name.args = stencil_exprs(F, e->args);
    return r;
}

static AstDecl *stencil_field_decl(AstFolder *F, FieldDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);
    r->field.name = d->name;
    r->field.tag = F->fold_expr(F, d->tag);
    return r;
}

static AstDecl *stencil_type_decl(AstFolder *F, TypeDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);
    r->type.name = d->name;
    r->type.generics = stencil_decls(F, d->generics);
    r->type.rhs = F->fold_expr(F, d->rhs);
    return r;
}

static AstDecl *stencil_generic_decl(AstFolder *F, GenericDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);
    r->generic.name = d->name;
    return r;
}

static AstDecl *stencil_struct_decl(AstFolder *F, StructDecl *d)
{
    Stenciler *S = F->state.S;
    AstDecl *r = stencil_prep_decl(F, d);

    // Keep track of the enclosing struct, so that methods can find the proper
    // receiver.
    AstDecl *enclosing = S->struct_;
    S->struct_ = r;
    {
        r->struct_.is_global = d->is_global;
        r->struct_.name = d->name;
        r->struct_.generics = stencil_decls(F, d->generics);
        r->struct_.fields = stencil_decls(F, d->fields);
        r->struct_.methods = stencil_methods(F, d->methods);
    }
    S->struct_ = enclosing;
    return r;
}

static AstDecl *stencil_var_decl(AstFolder *F, VarDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);
    r->var.is_global = d->is_global;
    r->var.is_const = d->is_const;
    r->var.name = d->name;
    r->var.init = F->fold_expr(F, d->init);
    r->var.tag = F->fold_expr(F, d->tag);
    return r;
}

static AstStmt *stencil_return_stmt(AstFolder *F, ReturnStmt *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    r->return_.expr = F->fold_expr(F, s->expr);
    return r;
}

static AstExpr *stencil_call_expr(AstFolder *F, CallExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->call.target = F->fold_expr(F, e->target);
    r->call.args = stencil_exprs(F, e->args);
    return r;
}

static AstExpr *stencil_ident_expr(AstFolder *F, AstIdent *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->name.name = e->name;
    return r;
}

static AstDecl *stencil_func_decl(AstFolder *F, FuncDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);
    r->func.is_global = d->is_global;
    r->func.receiver = NULL; // set during visit_*()
    r->func.name = d->name;
    r->func.generics = stencil_decls(F, d->generics);
    r->func.params = stencil_decls(F, d->params);
    r->func.return_ = F->fold_expr(F, d->return_);
    r->func.body = stencil_block(F, d->body);
    r->func.fn_kind = d->fn_kind;
    return r;
}

static AstStmt *stencil_if_stmt(AstFolder *F, IfStmt *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    r->if_.cond = F->fold_expr(F, s->cond);
    r->if_.then_arm = F->fold_stmt(F, s->then_arm);
    r->if_.else_arm = F->fold_stmt(F, s->else_arm);
    return r;
}

static AstStmt *stencil_while_stmt(AstFolder *F, WhileStmt *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    r->while_.cond = F->fold_expr(F, s->cond);
    r->while_.block = stencil_block(F, s->block);
    return r;
}

static AstStmt *stencil_label_stmt(AstFolder *F, LabelStmt *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    r->label.label = s->label;
    return r;
}

static AstStmt *stencil_for_stmt(AstFolder *F, ForStmt *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    if (s->kind == STMT_FORNUM) {
        r->for_.fornum.begin = F->fold_expr(F, s->fornum.begin);
        r->for_.fornum.end = F->fold_expr(F, s->fornum.end);
        r->for_.fornum.step = F->fold_expr(F, s->fornum.step);
    } else {
        r->for_.forin.target = F->fold_expr(F, s->forin.target);
    }
    r->for_.block = stencil_block(F, s->block);
    return r;
}

static AstExpr *stencil_index_expr(AstFolder *F, Index *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->index.target = F->fold_expr(F, e->target);
    r->index.elems = stencil_exprs(F, e->elems);
    return r;
}

static AstExpr *stencil_selector_expr(AstFolder *F, Selector *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->selector.target = F->fold_expr(F, e->target);
    r->selector.name = e->name;
    return r;
}

static AstStmt *stencil_decl_stmt(AstFolder *F, AstDeclStmt *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    r->decl.decl = F->fold_decl(F, s->decl);
    return r;
}

static void setup_stencil_pass(AstFolder *F, Stenciler *S)
{
    const AstState state = {.S = S};
    pawA_fold_init(F, S->ast, state);
    F->fold_literal_expr = stencil_literal_expr;
    F->fold_logical_expr = stencil_logical_expr;
    F->fold_ident_expr = stencil_ident_expr;
    F->fold_chain_expr = stencil_chain_expr;
    F->fold_unop_expr = stencil_unop_expr;
    F->fold_binop_expr = stencil_binop_expr;
    F->fold_cond_expr = stencil_cond_expr;
    F->fold_call_expr = stencil_call_expr;
    F->fold_index_expr = stencil_index_expr;
    F->fold_selector_expr = stencil_selector_expr;
    F->fold_item_expr = stencil_item_expr;
    F->fold_type_name_expr = stencil_type_name_expr;
    F->fold_signature_expr = stencil_signature_expr;
    F->fold_block_stmt = stencil_block_stmt;
    F->fold_expr_stmt = stencil_expr_stmt;
    F->fold_decl_stmt = stencil_decl_stmt;
    F->fold_if_stmt = stencil_if_stmt;
    F->fold_for_stmt = stencil_for_stmt;
    F->fold_while_stmt = stencil_while_stmt;
    F->fold_label_stmt = stencil_label_stmt;
    F->fold_return_stmt = stencil_return_stmt;
    F->fold_var_decl = stencil_var_decl;
    F->fold_func_decl = stencil_func_decl;
    F->fold_struct_decl = stencil_struct_decl;
    F->fold_field_decl = stencil_field_decl;
    F->fold_generic_decl = stencil_generic_decl;
    F->fold_type_decl = stencil_type_decl;
}

AstDecl *pawA_stencil(Ast *ast, AstDecl *decl)
{
    Stenciler S = {
        .lex = ast->lex, 
        .ast = ast,
    };
    AstFolder F;
    setup_stencil_pass(&F, &S);
    return F.fold_decl(&F, decl);
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

#define dump_fmt(P, fmt, ...) (indent_line(P), fprintf((P)->out, fmt, __VA_ARGS__))
#define dump_msg(P, msg) (indent_line(P), fprintf((P)->out, msg))

static void dump_stmt(Printer *, AstStmt *);
static void dump_expr(Printer *, AstExpr *);

static void print_decl_kind(Printer *P, void *node)
{
    AstDecl *d = node;
    switch (a_kind(d)) {
        case DECL_FUNC:
            fprintf(P->out, "FuncDecl");
            break;
        case DECL_FIELD:
            fprintf(P->out, "FieldDecl");
            break;
        case DECL_VAR:
            fprintf(P->out, "VarDecl");
            break;
        case DECL_STRUCT:
            fprintf(P->out, "StructDecl");
            break;
        case DECL_GENERIC:
            fprintf(P->out, "GenericDecl");
            break;
        case DECL_TYPE:
            fprintf(P->out, "TypeDecl");
            break;
        default:
            fprintf(P->out, "?");
    }
}

static void print_expr_kind(Printer *P, void *node)
{
    AstExpr *e = node;
    switch (a_kind(e)) {
        case EXPR_LITERAL: 
            fprintf(P->out, "LiteralExpr");
            break;
        case EXPR_UNOP:
            fprintf(P->out, "UnOpExpr");
            break;
        case EXPR_BINOP:
            fprintf(P->out, "BinOpExpr");
            break;
        case EXPR_CALL:
            fprintf(P->out, "CallExpr");
            break;
        case EXPR_COND:
            fprintf(P->out, "CondExpr");
            break;
        case EXPR_NAME: 
            fprintf(P->out, "AstIdent");
            break;
        case EXPR_FUNC_TYPE:
            fprintf(P->out, "FuncType");
            break;
        case EXPR_TYPE_NAME:
            fprintf(P->out, "TypeName");
            break;
        default:
            fprintf(P->out, "?");
            break;
    }
}

static void print_stmt_kind(Printer *P, void *node)
{
    AstStmt *s = node;
    switch (a_kind(s)) {
        case STMT_EXPR:
            fprintf(P->out, "AstExprStmt");
            break;
        case STMT_DECL:
            fprintf(P->out, "AstDeclStmt");
            break;
        case STMT_BLOCK:
            fprintf(P->out, "Block");
            break;
        case STMT_IF:
            fprintf(P->out, "IfStmt");
            break;
        case STMT_FORIN:
        case STMT_FORNUM: 
            fprintf(P->out, "ForStmt");
            break;
        case STMT_WHILE: 
        case STMT_DOWHILE: 
            fprintf(P->out, "WhileStmt");
            break;
        case STMT_RETURN: 
            fprintf(P->out, "ReturnStmt");
            break;
        default:
            fprintf(P->out, "?");
    }
}

static int predump_node(Printer *P, void *node, void (*print)(Printer *, void *))
{
    if (node != NULL) {
        print(P, node);
        fprintf(P->out, "(%p) {\n", node);
        return 0;
    }
    return -1;
}

#define dump_block(P, b) check_exp((b)->kind == STMT_BLOCK, dump_stmt(P, cast_stmt(b)))
#define dump_name(P, s) dump_fmt(P, "name: %s\n", s ? s->text : NULL)

static void dump_expr(Printer *P, AstExpr *e);
static void dump_decl(Printer *P, AstDecl *d);
static void dump_stmt(Printer *P, AstStmt *s);

#define make_list_dumper(name, T) \
    static void dump_ ## name ## _list(Printer *P, T ## List *list, const char *name) \
    { \
        dump_fmt(P, "%s: {\n", name); \
        ++P->indent; \
        if (list != NULL) { \
            dump_ ## name(P, list->first); \
        } \
        --P->indent; \
        dump_msg(P, "}\n"); \
    }
make_list_dumper(expr, AstExpr)
make_list_dumper(decl, AstDecl)
make_list_dumper(stmt, AstStmt)

static void dump_decl(Printer *P, AstDecl *d)
{
    if (predump_node(P, d, print_decl_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", d->hdr.line);
    switch (a_kind(d)) {
        case DECL_FUNC:
            dump_fmt(P, "is_global: %d\n", d->func.is_global);
            dump_fmt(P, "receiver: %p\n", (void *)d->func.receiver);
            dump_fmt(P, "name: %s\n", d->func.name->text);
            dump_decl_list(P, d->func.generics, "generics");
            dump_decl_list(P, d->func.params, "params");
            dump_msg(P, "return_: ");
            dump_expr(P, d->func.return_);
            break;
        case DECL_FIELD:
            dump_name(P, d->field.name);
            dump_msg(P, "tag: ");
            dump_expr(P, d->field.tag);
            break;
        case DECL_VAR:
            dump_fmt(P, "is_global: %d\n", d->var.is_global);
            dump_name(P, d->var.name);
            dump_msg(P, "tag: ");
            dump_expr(P, d->var.tag);
            dump_msg(P, "init: ");
            dump_expr(P, d->var.init);
            break;
        case DECL_STRUCT:
            dump_name(P, d->struct_.name);
            dump_fmt(P, "type: %d\n", d->struct_.type->hdr.def);
            dump_decl_list(P, d->struct_.generics, "generics");
            dump_decl_list(P, d->struct_.fields, "fields");
            dump_decl_list(P, d->struct_.methods, "methods");
            break;
        case DECL_GENERIC:
            dump_name(P, d->generic.name);
            break;
        case DECL_TYPE:
            dump_name(P, d->type.name);
            dump_msg(P, "rhs: ");
            dump_expr(P, d->type.rhs);
            dump_decl_list(P, d->type.generics, "generics");
            break;
        default:
            paw_assert(a_is_func_decl(d));
            dump_fmt(P, "is_global: %d\n", d->func.is_global);
            dump_name(P, d->func.name);
            dump_decl_list(P, d->func.generics, "generics");
    }
    --P->indent;
    dump_msg(P, "}\n");
    if (d->hdr.next != NULL) {
        dump_decl(P, d->hdr.next);
    }
}

static void dump_stmt(Printer *P, AstStmt *s)
{
    if (predump_node(P, s, print_stmt_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", s->hdr.line);
    switch (a_kind(s)) {
        case STMT_EXPR:
            dump_msg(P, "lhs: ");
            dump_expr(P, s->expr.lhs);
            dump_msg(P, "rhs: ");
            dump_expr(P, s->expr.rhs);
            break;
        case STMT_BLOCK:
            dump_stmt_list(P, s->block.stmts, "stmts");
            break;
        case STMT_DECL:
            dump_msg(P, "decl: ");
            dump_decl(P, s->decl.decl);
            break;
        case STMT_IF:
            dump_msg(P, "cond: ");
            dump_expr(P, s->if_.cond);
            dump_msg(P, "then_arm: ");
            dump_stmt(P, s->if_.then_arm);
            dump_msg(P, "else_arm: ");
            dump_stmt(P, s->if_.else_arm);
            break;
        case STMT_FORIN:
            dump_name(P, s->for_.name);
            dump_msg(P, "target: ");
            dump_expr(P, s->for_.forin.target);
            dump_msg(P, "block: ");
            dump_block(P, s->for_.block);
            break;
        case STMT_FORNUM: 
            dump_name(P, s->for_.name);
            dump_msg(P, "begin: ");
            dump_expr(P, s->for_.fornum.begin);
            dump_msg(P, "end: ");
            dump_expr(P, s->for_.fornum.end);
            dump_msg(P, "step: ");
            dump_expr(P, s->for_.fornum.step);
            dump_msg(P, "block: ");
            dump_block(P, s->for_.block);
            break;
        case STMT_WHILE: 
            dump_msg(P, "cond: ");
            dump_expr(P, s->while_.cond);
            dump_msg(P, "block: ");
            dump_block(P, s->while_.block);
            break;
        case STMT_DOWHILE: 
            dump_msg(P, "block: ");
            dump_block(P, s->while_.block);
            dump_msg(P, "cond: ");
            dump_expr(P, s->while_.cond);
            break;
        case STMT_RETURN: 
            dump_msg(P, "expr: ");
            dump_stmt(P, cast_stmt(s->return_.expr));
            break;
        default:
            break;
    }
    --P->indent;
    dump_msg(P, "}\n");
    if (s->hdr.next != NULL) {
        dump_stmt(P, s->hdr.next);
    }
}

static void dump_expr(Printer *P, AstExpr *e)
{
    if (predump_node(P, e, print_expr_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", e->hdr.line);
    switch (a_kind(e)) {
        case EXPR_LITERAL: 
            break;
        case EXPR_UNOP:
            dump_fmt(P, "op: %d\n", e->unop.op);
            dump_msg(P, "target: ");
            dump_expr(P, e->unop.target);
            break;
        case EXPR_BINOP:
            dump_fmt(P, "op: %d\n", e->binop.op);
            dump_msg(P, "lhs: ");
            dump_expr(P, e->binop.lhs);
            dump_msg(P, "rhs: ");
            dump_expr(P, e->binop.rhs);
            break;
        case EXPR_CALL:
            dump_msg(P, "target: ");
            dump_expr(P, e->call.target);
            dump_expr_list(P, e->call.args, "args");
            break;
        case EXPR_COND:
            dump_msg(P, "cond: ");
            dump_expr(P, e->cond.cond);
            dump_msg(P, "lhs: ");
            dump_expr(P, e->cond.lhs);
            dump_msg(P, "rhs: ");
            dump_expr(P, e->cond.rhs);
            break;
        case EXPR_NAME: 
            dump_name(P, e->name.name);
            break;
        case EXPR_FUNC_TYPE:
            dump_expr_list(P, e->func.params, "params");
            dump_msg(P, "return_: ");
            dump_expr(P, e->func.return_);
            break;
        case EXPR_TYPE_NAME:
            dump_name(P, e->type_name.name);
            dump_expr_list(P, e->type_name.args, "args");
            break;
        default:
            break;
    }
    --P->indent;
    dump_msg(P, "}\n");
    if (e->hdr.next != NULL) {
        dump_expr(P, e->hdr.next);
    }
}

void pawA_dump_decl(FILE *out, AstDecl *decl)
{
    Printer P;
    P.out = out;
    P.indent = 0;
    dump_decl(&P, decl);
}

void pawA_dump_expr(FILE *out, AstExpr *expr)
{
    Printer P;
    P.out = out;
    P.indent = 0;
    dump_expr(&P, expr);
}

void pawA_dump_stmt(FILE *out, AstStmt *stmt)
{
    Printer P;
    P.out = out;
    P.indent = 0;
    dump_stmt(&P, stmt);
}
