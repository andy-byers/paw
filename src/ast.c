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

static void add_builtin_type(Ast *ast, AstTypeKind kind, paw_Type code)
{
    ast->builtin[code] = pawA_new_type(ast, kind);
    ast->builtin[code]->adt.types = pawA_list_new(ast);
    ast->builtin[code]->adt.base = code;
    ast->builtin[code]->adt.did = code;
}

Ast *pawA_new_ast(Lex *lex)
{
    paw_Env *P = env(lex);
    Ast *tree = pawM_new(P, Ast);
    tree->lex = lex;

    // initialize memory pools for storing AST components
    pawK_pool_init(P, &tree->nodes, FIRST_ARENA_SIZE, sizeof(AstDecl));
    pawK_pool_init(P, &tree->symbols, FIRST_ARENA_SIZE, sizeof(Symbol));
    pawK_pool_init(P, &tree->sequences, FIRST_ARENA_SIZE, sizeof(void *) * 8);

    add_builtin_type(tree, AST_TYPE_ADT, PAW_TUNIT);
    add_builtin_type(tree, AST_TYPE_ADT, PAW_TBOOL);
    add_builtin_type(tree, AST_TYPE_ADT, PAW_TINT);
    add_builtin_type(tree, AST_TYPE_ADT, PAW_TFLOAT);
    add_builtin_type(tree, AST_TYPE_ADT, PAW_TSTRING);
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

// clang-format off
#define make_node_constructor(name, T)                                                             \
    T *pawA_new_##name(Ast *ast, T##Kind kind)                                                     \
    {                                                                                              \
        T *r = pawK_pool_alloc(env((ast)->lex), &(ast)->nodes, sizeof(T), paw_alignof(T));         \
        r->hdr.line = (ast)->lex->line;                                                            \
        r->hdr.kind = kind;                                                                        \
        return r;                                                                                  \
    }
make_node_constructor(expr, AstExpr)
make_node_constructor(decl, AstDecl)
make_node_constructor(stmt, AstStmt)
make_node_constructor(pat, AstPat)
// clang-format on

#define LIST_MIN_ALLOC 8

        AstType *pawA_new_type(Ast *ast, AstTypeKind kind)
{
    AstType *r = pawK_pool_alloc(env((ast)->lex), &(ast)->nodes,
                                 sizeof(AstType), paw_alignof(AstType));
    r->hdr.kind = kind;
    return r;
}

static AstList *new_list(Ast *ast, int alloc)
{
    paw_Env *P = env(ast->lex);
    pawM_check_size(P, sizeof(AstList), cast_size(alloc), sizeof(void *));
    const size_t size = sizeof(AstList) + cast_size(alloc) * sizeof(void *);
    AstList *list =
        pawK_pool_alloc(P, &ast->sequences, size, paw_alignof(AstList));
    list->alloc = alloc;
    return list;
}

AstList *pawA_list_new(Ast *ast)
{
    AstList *list = ast->freed;
    if (list == NULL) {
        list = new_list(ast, LIST_MIN_ALLOC);
    } else {
        ast->freed = list->prev;
    }
    list->count = 0;
    return list;
}

void pawA_list_free(Ast *ast, AstList *list)
{
    list->prev = ast->freed;
    ast->freed = list;
}

static AstList *next_larger_list(Ast *ast, AstList *list)
{
    AstList **p = &ast->freed;
    while (*p != NULL) {
        AstList *freed = *p;
        if (freed->alloc > list->alloc) {
            *p = freed->prev;
            return freed;
        }
        p = &freed->prev;
    }
    if (list->alloc > INT_MAX / 2) {
        pawM_error(env(ast->lex));
    }
    return new_list(ast, list->alloc * 2);
}

void pawA_list_push(Ast *ast, AstList **plist, void *node)
{
    AstList *list = *plist;
    if (list->count == list->alloc) {
        AstList *larger = next_larger_list(ast, list);
        const size_t size = cast_size(list->count) * sizeof(list->data[0]);
        memcpy(larger->data, list->data, size);
        larger->count = list->count;
        pawA_list_free(ast, list);
        list = larger;
    }
    list->data[list->count++] = node;
    *plist = list;
}

void pawA_path_push(Ast *ast, AstList **ppath, String *name, AstList *types)
{
    Lex *lex = ast->lex;
    AstPathSegment *segment = pawK_pool_alloc(env(lex), &lex->pm->ast->nodes, 
                                              sizeof(AstPathSegment),
                                              paw_alignof(Symbol));
    segment->name = name;
    segment->types = types;
    pawA_list_push(ast, ppath, segment);
}

Symbol *pawA_new_symbol(Lex *lex)
{
    return pawK_pool_alloc(env(lex), &lex->pm->ast->symbols, sizeof(Symbol),
                           paw_alignof(Symbol));
}

DefId pawA_add_decl(Ast *ast, AstDecl *decl)
{
    paw_Env *P = env(ast->lex);
    ParseMemory *pm = ast->lex->pm;
    pawM_grow(P, pm->decls.data, pm->decls.size, pm->decls.alloc);
    const DefId id = pm->decls.size++;
    pm->decls.data[id] = decl;
    decl->hdr.def = id;
    return id;
}

AstDecl *pawA_get_decl(Ast *ast, DefId id)
{
    ParseMemory *pm = ast->lex->pm;
    paw_assert(id < pm->decls.size);
    return pm->decls.data[id];
}

#define make_list_visitor(name, T)                                             \
    static void visit_##name##_list_aux(AstVisitor *V, AstList *list,          \
                                        T##Pass cb)                            \
    {                                                                          \
        for (int i = 0; i < list->count; ++i) {                                \
            cb(V, list->data[i]);                                              \
        }                                                                      \
    }
make_list_visitor(decl, AstDecl) make_list_visitor(expr, AstExpr)
    make_list_visitor(stmt, AstStmt)

#define visit_stmts(V, list) (V)->visit_stmt_list(V, list, (V)->visit_stmt)
#define visit_exprs(V, list) (V)->visit_expr_list(V, list, (V)->visit_expr)
#define visit_decls(V, list) (V)->visit_decl_list(V, list, (V)->visit_decl)

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
    V->visit_expr(V, e->result);
}

static void visit_typename_expr(AstVisitor *V, TypeName *e)
{
    visit_exprs(V, e->args);
}

static void visit_typelist_expr(AstVisitor *V, TypeList *e)
{
    visit_exprs(V, e->types);
}

static void visit_match_expr(AstVisitor *V, MatchExpr *e)
{
    V->visit_expr(V, e->target);
    visit_exprs(V, e->arms);
}

static void visit_arm_expr(AstVisitor *V, MatchArm *e)
{
    V->visit_pat(V, e->guard);
    V->visit_expr(V, e->value);
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
    if (d->generics != NULL) {
        visit_decls(V, d->generics);
    }
    visit_decls(V, d->fields);
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

static void visit_conversion_expr(AstVisitor *V, ConversionExpr *e)
{
    V->visit_expr(V, e->arg);
}

static void visit_ident_expr(AstVisitor *V, AstIdent *e)
{
    paw_unused(V);
    paw_unused(e);
}

static void visit_func_decl(AstVisitor *V, FuncDecl *d)
{
    if (d->generics != NULL) {
        visit_decls(V, d->generics);
    }
    visit_decls(V, d->params);
    V->visit_expr(V, d->result);
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

static void visit_access_expr(AstVisitor *V, Access *e)
{
    V->visit_expr(V, e->target);
}

static void visit_selector_expr(AstVisitor *V, Selector *e)
{
    V->visit_expr(V, e->target);
}

static void visit_literal_pat(AstVisitor *V, AstLiteralPat *p)
{
    V->visit_expr(V, p->expr);
}

static void visit_path_pat(AstVisitor *V, AstPathPat *p)
{
    paw_unused(V);
    paw_unused(p);
}

static void visit_tuple_pat(AstVisitor *V, AstTuplePat *p)
{
    paw_unused(V);
    paw_unused(p);
}

static void visit_field_pat(AstVisitor *V, AstFieldPat *p)
{
    paw_unused(V);
    paw_unused(p);
}

static void visit_struct_pat(AstVisitor *V, AstStructPat *p)
{
    paw_unused(V);
    paw_unused(p);
}

static void visit_variant_pat(AstVisitor *V, AstVariantPat *p)
{
    paw_unused(V);
    paw_unused(p);
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
        case EXPR_CONVERSION:
            V->visit_conversion_expr(V, &expr->conv);
            break;
        case EXPR_CALL:
            V->visit_call_expr(V, &expr->call);
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
        case EXPR_FUNCTYPE:
            V->visit_signature_expr(V, &expr->func);
            break;
        case EXPR_TYPENAME:
            V->visit_typename_expr(V, &expr->type_name);
            break;
        case EXPR_TYPELIST:
            V->visit_typelist_expr(V, &expr->typelist);
            break;
        case EXPR_ACCESS:
            V->visit_access_expr(V, &expr->access);
            break;
        case EXPR_MATCH:
            V->visit_match_expr(V, &expr->match);
            break;
        case EXPR_MATCHARM:
            V->visit_arm_expr(V, &expr->arm);
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
        case DECL_VARIANT:
            V->visit_variant_decl(V, &decl->variant);
            break;
        default:
            paw_assert(a_kind(decl) == DECL_STRUCT);
            V->visit_struct_decl(V, &decl->struct_);
    }
}

static void visit_decl_stmt(AstVisitor *V, AstDeclStmt *s)
{
    V->visit_decl(V, s->decl);
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

static void visit_pat(AstVisitor *V, AstPat *pat)
{
    if (pat == NULL) {
        return;
    }
    switch (a_kind(pat)) {
        case AST_PAT_LITERAL:
            V->visit_literal_pat(V, &pat->literal);
            break;
        case AST_PAT_PATH:
            V->visit_path_pat(V, &pat->path);
            break;
        case AST_PAT_FIELD:
            V->visit_field_pat(V, &pat->field);
            break;
        case AST_PAT_TUPLE:
            V->visit_tuple_pat(V, &pat->tuple);
            break;
        case AST_PAT_VARIANT:
            V->visit_variant_pat(V, &pat->variant);
            break;
        default:
            paw_assert(a_kind(pat) == AST_PAT_STRUCT);
            V->visit_struct_pat(V, &pat->struct_);
            break;
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
        .visit_pat = visit_pat,
        .visit_expr_list = visit_expr_list_aux,
        .visit_decl_list = visit_decl_list_aux,
        .visit_stmt_list = visit_stmt_list_aux,
        .visit_literal_expr = visit_literal_expr,
        .visit_logical_expr = visit_logical_expr,
        .visit_ident_expr = visit_ident_expr,
        .visit_chain_expr = visit_chain_expr,
        .visit_unop_expr = visit_unop_expr,
        .visit_binop_expr = visit_binop_expr,
        .visit_call_expr = visit_call_expr,
        .visit_conversion_expr = visit_conversion_expr,
        .visit_index_expr = visit_index_expr,
        .visit_access_expr = visit_access_expr,
        .visit_selector_expr = visit_selector_expr,
        .visit_item_expr = visit_item_expr,
        .visit_typename_expr = visit_typename_expr,
        .visit_typelist_expr = visit_typelist_expr,
        .visit_signature_expr = visit_signature_expr,
        .visit_match_expr = visit_match_expr,
        .visit_arm_expr = visit_arm_expr,
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
        .visit_literal_pat = visit_literal_pat,
        .visit_path_pat = visit_path_pat,
        .visit_tuple_pat = visit_tuple_pat,
        .visit_field_pat = visit_field_pat,
        .visit_struct_pat = visit_struct_pat,
        .visit_variant_pat = visit_variant_pat,
    };
}

void pawA_visit(AstVisitor *V)
{
    Ast *ast = V->ast;
    visit_stmts(V, ast->stmts);
}

// Generate code for folding a linked list of AST nodes
//
// fold_*_list_aux: Pass over the list, calling the supplied callback on each
// node. The
//     callback should return (a) the next node n->source.link (with text
//     replacement) if the current node n should be removed, (b) n if no folding
//     needs to be performed, or (c) a freshly-allocated node if creating a new
//     list.
// visit_*_list: Run fold_*_list_aux with the default callback.
#define make_list_folder(name, T)                                              \
    static void fold_##name##_list(AstFolder *F, AstList *list, T##Fold cb)    \
    {                                                                          \
        for (int i = 0; i < list->count; ++i) {                                \
            cb(F, list->data[i]);                                              \
        }                                                                      \
    }                                                                          \
    static void fold_##name##s(AstFolder *F, AstList *list)                    \
    {                                                                          \
        fold_##name##_list(F, list, F->fold_##name);                           \
    }
make_list_folder(decl, AstDecl) make_list_folder(expr, AstExpr)
    make_list_folder(stmt, AstStmt)

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
    e->result = F->fold_expr(F, e->result);
    return cast_expr(e);
}

static AstExpr *fold_typename_expr(AstFolder *F, TypeName *e)
{
    fold_exprs(F, e->args);
    return cast_expr(e);
}

static AstExpr *fold_typelist_expr(AstFolder *F, TypeList *e)
{
    fold_exprs(F, e->types);
    return cast_expr(e);
}

static AstExpr *fold_match_expr(AstFolder *F, MatchExpr *e)
{
    e->target = F->fold_expr(F, e->target);
    fold_exprs(F, e->arms);
    return cast_expr(e);
}

static AstExpr *fold_arm_expr(AstFolder *F, MatchArm *e)
{
    e->guard = F->fold_pat(F, e->guard);
    e->value = F->fold_expr(F, e->value);
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

static AstExpr *fold_conversion_expr(AstFolder *F, ConversionExpr *e)
{
    e->arg = F->fold_expr(F, e->arg);
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
    d->result = F->fold_expr(F, d->result);
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

static AstExpr *fold_access_expr(AstFolder *F, Access *e)
{
    e->target = F->fold_expr(F, e->target);
    return cast_expr(e);
}

static AstPat *fold_literal_pat(AstFolder *F, AstLiteralPat *p)
{
    p->expr = F->fold_expr(F, p->expr);
    return cast_pat(p);
}

static AstPat *fold_path_pat(AstFolder *V, AstPathPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *fold_tuple_pat(AstFolder *V, AstTuplePat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *fold_field_pat(AstFolder *V, AstFieldPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *fold_struct_pat(AstFolder *V, AstStructPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *fold_variant_pat(AstFolder *V, AstVariantPat *p)
{
    paw_unused(V);
    return cast_pat(p);
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
        case EXPR_CONVERSION:
            return F->fold_conversion_expr(F, &expr->conv);
        case EXPR_CALL:
            return F->fold_call_expr(F, &expr->call);
        case EXPR_NAME:
            return F->fold_ident_expr(F, &expr->name);
        case EXPR_INDEX:
            return F->fold_index_expr(F, &expr->index);
        case EXPR_ITEM:
            return F->fold_item_expr(F, &expr->item);
        case EXPR_FUNCTYPE:
            return F->fold_signature_expr(F, &expr->func);
        case EXPR_TYPENAME:
            return F->fold_typename_expr(F, &expr->type_name);
        case EXPR_TYPELIST:
            return F->fold_typelist_expr(F, &expr->typelist);
        case EXPR_MATCH:
            return F->fold_match_expr(F, &expr->match);
        case EXPR_MATCHARM:
            return F->fold_arm_expr(F, &expr->arm);
        case EXPR_ACCESS:
            return F->fold_access_expr(F, &expr->access);
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
        case DECL_INSTANCE:
            return F->fold_instance_decl(F, &decl->inst);
        case DECL_VARIANT:
            return F->fold_variant_decl(F, &decl->variant);
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

static AstPat *fold_pat(AstFolder *F, AstPat *pat)
{
    if (pat == NULL) {
        return NULL;
    }
    switch (a_kind(pat)) {
        case AST_PAT_LITERAL:
            return F->fold_literal_pat(F, &pat->literal);
        case AST_PAT_PATH:
            return F->fold_path_pat(F, &pat->path);
        case AST_PAT_FIELD:
            return F->fold_field_pat(F, &pat->field);
        case AST_PAT_TUPLE:
            return F->fold_tuple_pat(F, &pat->tuple);
        case AST_PAT_VARIANT:
            return F->fold_variant_pat(F, &pat->variant);
        default:
            paw_assert(a_kind(pat) == AST_PAT_STRUCT);
            return F->fold_struct_pat(F, &pat->struct_);
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
        .fold_pat = fold_pat,
        .fold_expr_list = fold_expr_list,
        .fold_decl_list = fold_decl_list,
        .fold_stmt_list = fold_stmt_list,
        .fold_literal_expr = fold_literal_expr,
        .fold_logical_expr = fold_logical_expr,
        .fold_ident_expr = fold_ident_expr,
        .fold_chain_expr = fold_chain_expr,
        .fold_unop_expr = fold_unop_expr,
        .fold_binop_expr = fold_binop_expr,
        .fold_conversion_expr = fold_conversion_expr,
        .fold_call_expr = fold_call_expr,
        .fold_index_expr = fold_index_expr,
        .fold_selector_expr = fold_selector_expr,
        .fold_access_expr = fold_access_expr,
        .fold_match_expr = fold_match_expr,
        .fold_arm_expr = fold_arm_expr,
        .fold_item_expr = fold_item_expr,
        .fold_typename_expr = fold_typename_expr,
        .fold_typelist_expr = fold_typelist_expr,
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
        .fold_literal_pat = fold_literal_pat,
        .fold_path_pat = fold_path_pat,
        .fold_tuple_pat = fold_tuple_pat,
        .fold_field_pat = fold_field_pat,
        .fold_struct_pat = fold_struct_pat,
        .fold_variant_pat = fold_variant_pat,
    };
}

void pawA_fold(AstFolder *F)
{
    Ast *ast = F->ast;
    fold_stmts(F, ast->stmts);
}

static void fold_binder(AstTypeFolder *F, AstList *binder)
{
    // NOTE: Number of types in the binder should never change.
    for (int i = 0; i < binder->count; ++i) {
        binder->data[i] = F->fold(F, binder->data[i]);
    }
}

static AstType *fold_func(AstTypeFolder *F, AstFuncDef *t)
{
    F->fold_binder(F, t->types);
    F->fold_binder(F, t->params);
    t->result = F->fold(F, t->result);
    return a_cast_type(t);
}

static AstType *fold_fptr(AstTypeFolder *F, AstFuncPtr *t)
{
    F->fold_binder(F, t->params);
    t->result = F->fold(F, t->result);
    return a_cast_type(t);
}

static AstType *fold_adt(AstTypeFolder *F, AstAdt *t)
{
    F->fold_binder(F, t->types);
    return a_cast_type(t);
}

static AstType *fold_unknown(AstTypeFolder *F, AstUnknown *t)
{
    paw_unused(F);
    return a_cast_type(t);
}

static AstType *fold_generic(AstTypeFolder *F, AstGeneric *t)
{
    paw_unused(F);
    return a_cast_type(t);
}

void pawA_type_folder_init(AstTypeFolder *F, void *state)
{
    *F = (AstTypeFolder){
        .state = state,
        .fold = pawA_fold_type,
        .fold_binder = fold_binder,
        .fold_fptr = fold_fptr,
        .fold_func = fold_func,
        .fold_adt = fold_adt,
        .fold_unknown = fold_unknown,
        .fold_generic = fold_generic,
    };
}

AstType *pawA_fold_type(AstTypeFolder *F, AstType *type)
{
    switch (y_kind(type)) {
        case AST_TYPE_GENERIC:
            return F->fold_generic(F, &type->generic);
        case AST_TYPE_UNKNOWN:
            return F->fold_unknown(F, &type->unknown);
        case AST_TYPE_ADT:
            return F->fold_adt(F, &type->adt);
        case AST_TYPE_FPTR:
            return F->fold_fptr(F, &type->fptr);
        default:
            paw_assert(a_is_fdef(type));
            return F->fold_func(F, &type->func);
    }
}

// ****************************
//     AST copying routines
// ****************************

typedef struct Copier {
    Lex *lex; // lexical state
    Ast *ast; // AST being copied
} Copier;

// clang-format off
#define make_copy_prep(name, T)                                                \
    static T *copy_prep_##name##_aux(AstFolder *F, T *t)                       \
    {                                                                          \
        T *r = pawA_new_##name(F->ast, a_kind(t));                             \
        r->hdr.kind = t->hdr.kind;                                             \
        r->hdr.line = t->hdr.line;                                             \
        return r;                                                              \
    }
make_copy_prep(expr, AstExpr) 
make_copy_prep(decl, AstDecl)
make_copy_prep(stmt, AstStmt)
make_copy_prep(pat, AstPat)

// Helpers for copying: create a new node of the given type and kind,
// and copy the common fields
#define copy_prep_expr(F, e) copy_prep_expr_aux(F, cast_expr(e))
#define copy_prep_decl(F, d) copy_prep_decl_aux(F, cast_decl(d))
#define copy_prep_stmt(F, s) copy_prep_stmt_aux(F, cast_stmt(s))
#define copy_prep_pat(F, p) copy_prep_pat_aux(F, cast_pat(p))

#define make_copy_list(name, T)                                                \
    static AstList *copy_##name##s(AstFolder *F, AstList *old_list)            \
    {                                                                          \
        AstList *new_list = pawA_list_new(F->ast);                             \
        for (int i = 0; i < old_list->count; ++i) {                            \
            pawA_list_push(F->ast, &new_list, old_list->data[i]);              \
        }                                                                      \
        F->fold_##name##_list(F, new_list, F->fold_##name);                    \
        return new_list;                                                       \
    }
make_copy_list(decl, AstDecl) 
make_copy_list(expr, AstExpr)
make_copy_list(stmt, AstStmt)

static AstStmt *copy_block_stmt(AstFolder *F, Block *s)
{
    AstStmt *r = copy_prep_stmt(F, s);
    r->block.stmts = copy_stmts(F, s->stmts);
    return r;
}
#define copy_block(F, s) cast((F)->fold_block_stmt(F, s), Block *)
// clang-format on

static AstExpr *copy_logical_expr(AstFolder *F, LogicalExpr *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->logical.lhs = F->fold_expr(F, e->lhs);
    r->logical.rhs = F->fold_expr(F, e->rhs);
    return r;
}

static AstExpr *copy_item_expr(AstFolder *F, ItemExpr *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->item.key = F->fold_expr(F, e->key);
    r->item.value = F->fold_expr(F, e->value);
    r->item.index = e->index;
    return r;
}

static AstExpr *copy_literal_expr(AstFolder *F, LiteralExpr *e)
{
    AstExpr *r = copy_prep_expr(F, e);
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
            r->literal.comp.items = copy_exprs(F, e->comp.items);
            break;
    }
    return r;
}

static AstExpr *copy_chain_expr(AstFolder *F, ChainExpr *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->chain.target = F->fold_expr(F, e->target);
    return r;
}

static AstExpr *copy_unop_expr(AstFolder *F, UnOpExpr *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->unop.target = F->fold_expr(F, e->target);
    r->unop.op = e->op;
    return r;
}

static AstExpr *copy_binop_expr(AstFolder *F, BinOpExpr *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->binop.lhs = F->fold_expr(F, e->lhs);
    r->binop.rhs = F->fold_expr(F, e->rhs);
    r->binop.op = e->op;
    return r;
}

static AstStmt *copy_expr_stmt(AstFolder *F, AstExprStmt *s)
{
    AstStmt *r = copy_prep_stmt(F, s);
    r->expr.lhs = F->fold_expr(F, s->lhs);
    r->expr.rhs = F->fold_expr(F, s->rhs);
    return r;
}

static AstExpr *copy_signature_expr(AstFolder *F, FuncType *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->func.params = copy_exprs(F, e->params);
    r->func.result = F->fold_expr(F, e->result);
    return r;
}

static AstExpr *copy_typename_expr(AstFolder *F, TypeName *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->type_name.name = e->name;
    r->type_name.args = copy_exprs(F, e->args);
    return r;
}

static AstExpr *copy_typelist_expr(AstFolder *F, TypeList *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->typelist.types = copy_exprs(F, e->types);
    return r;
}

static AstDecl *copy_field_decl(AstFolder *F, FieldDecl *d)
{
    AstDecl *r = copy_prep_decl(F, d);
    r->field.name = d->name;
    r->field.tag = F->fold_expr(F, d->tag);
    return r;
}

static AstDecl *copy_type_decl(AstFolder *F, TypeDecl *d)
{
    AstDecl *r = copy_prep_decl(F, d);
    r->type.name = d->name;
    r->type.generics = copy_decls(F, d->generics);
    r->type.rhs = F->fold_expr(F, d->rhs);
    return r;
}

static AstDecl *copy_generic_decl(AstFolder *F, GenericDecl *d)
{
    AstDecl *r = copy_prep_decl(F, d);
    r->generic.name = d->name;
    return r;
}

static AstDecl *copy_struct_decl(AstFolder *F, StructDecl *d)
{
    AstDecl *r = copy_prep_decl(F, d);
    r->struct_.is_global = d->is_global;
    r->struct_.name = d->name;
    r->struct_.generics = copy_decls(F, d->generics);
    r->struct_.fields = copy_decls(F, d->fields);
    return r;
}

static AstDecl *copy_variant_decl(AstFolder *F, VariantDecl *d)
{
    AstDecl *r = copy_prep_decl(F, d);
    r->variant.name = d->name;
    r->variant.fields = copy_decls(F, d->fields);
    r->variant.index = d->index;
    return r;
}

static AstDecl *copy_var_decl(AstFolder *F, VarDecl *d)
{
    AstDecl *r = copy_prep_decl(F, d);
    r->var.is_global = d->is_global;
    r->var.is_const = d->is_const;
    r->var.name = d->name;
    r->var.init = F->fold_expr(F, d->init);
    r->var.tag = F->fold_expr(F, d->tag);
    return r;
}

static AstStmt *copy_return_stmt(AstFolder *F, ReturnStmt *s)
{
    AstStmt *r = copy_prep_stmt(F, s);
    r->return_.expr = F->fold_expr(F, s->expr);
    return r;
}

static AstExpr *copy_conversion_expr(AstFolder *F, ConversionExpr *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->conv.arg = F->fold_expr(F, e->arg);
    return r;
}

static AstExpr *copy_call_expr(AstFolder *F, CallExpr *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->call.target = F->fold_expr(F, e->target);
    r->call.args = copy_exprs(F, e->args);
    return r;
}

static AstExpr *copy_ident_expr(AstFolder *F, AstIdent *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->name.name = e->name;
    return r;
}

static AstDecl *copy_func_decl(AstFolder *F, FuncDecl *d)
{
    AstDecl *r = copy_prep_decl(F, d);
    r->func.is_global = d->is_global;
    r->func.receiver = NULL; // set during visit_*()
    r->func.name = d->name;
    r->func.generics = copy_decls(F, d->generics);
    r->func.params = copy_decls(F, d->params);
    r->func.result = F->fold_expr(F, d->result);
    r->func.body = copy_block(F, d->body);
    r->func.fn_kind = d->fn_kind;
    return r;
}

static AstStmt *copy_if_stmt(AstFolder *F, IfStmt *s)
{
    AstStmt *r = copy_prep_stmt(F, s);
    r->if_.cond = F->fold_expr(F, s->cond);
    r->if_.then_arm = F->fold_stmt(F, s->then_arm);
    r->if_.else_arm = F->fold_stmt(F, s->else_arm);
    return r;
}

static AstStmt *copy_while_stmt(AstFolder *F, WhileStmt *s)
{
    AstStmt *r = copy_prep_stmt(F, s);
    r->while_.cond = F->fold_expr(F, s->cond);
    r->while_.block = copy_block(F, s->block);
    return r;
}

static AstStmt *copy_label_stmt(AstFolder *F, LabelStmt *s)
{
    AstStmt *r = copy_prep_stmt(F, s);
    r->label.label = s->label;
    return r;
}

static AstStmt *copy_for_stmt(AstFolder *F, ForStmt *s)
{
    AstStmt *r = copy_prep_stmt(F, s);
    if (s->kind == STMT_FORNUM) {
        r->for_.fornum.begin = F->fold_expr(F, s->fornum.begin);
        r->for_.fornum.end = F->fold_expr(F, s->fornum.end);
        r->for_.fornum.step = F->fold_expr(F, s->fornum.step);
    } else {
        r->for_.forin.target = F->fold_expr(F, s->forin.target);
    }
    r->for_.block = copy_block(F, s->block);
    return r;
}

static AstExpr *copy_index_expr(AstFolder *F, Index *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->index.target = F->fold_expr(F, e->target);
    r->index.elems = copy_exprs(F, e->elems);
    return r;
}

static AstExpr *copy_access_expr(AstFolder *F, Access *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->access.target = F->fold_expr(F, e->target);
    r->access.name = e->name;
    return r;
}

static AstExpr *copy_selector_expr(AstFolder *F, Selector *e)
{
    AstExpr *r = copy_prep_expr(F, e);
    r->selector.target = F->fold_expr(F, e->target);
    r->selector.name = e->name;
    return r;
}

static AstStmt *copy_decl_stmt(AstFolder *F, AstDeclStmt *s)
{
    AstStmt *r = copy_prep_stmt(F, s);
    r->decl.decl = F->fold_decl(F, s->decl);
    return r;
}

static AstPat *copy_path_pat(AstFolder *V, AstPathPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *copy_tuple_pat(AstFolder *V, AstTuplePat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *copy_field_pat(AstFolder *V, AstFieldPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *copy_struct_pat(AstFolder *V, AstStructPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *copy_variant_pat(AstFolder *V, AstVariantPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static void setup_copy_pass(AstFolder *F, Copier *C)
{
    const AstState state = {.C = C};
    pawA_fold_init(F, C->ast, state);
    F->fold_literal_expr = copy_literal_expr;
    F->fold_logical_expr = copy_logical_expr;
    F->fold_ident_expr = copy_ident_expr;
    F->fold_chain_expr = copy_chain_expr;
    F->fold_unop_expr = copy_unop_expr;
    F->fold_binop_expr = copy_binop_expr;
    F->fold_conversion_expr = copy_conversion_expr;
    F->fold_call_expr = copy_call_expr;
    F->fold_index_expr = copy_index_expr;
    F->fold_access_expr = copy_access_expr;
    F->fold_selector_expr = copy_selector_expr;
    F->fold_item_expr = copy_item_expr;
    F->fold_typename_expr = copy_typename_expr;
    F->fold_typelist_expr = copy_typelist_expr;
    F->fold_signature_expr = copy_signature_expr;
    F->fold_block_stmt = copy_block_stmt;
    F->fold_expr_stmt = copy_expr_stmt;
    F->fold_decl_stmt = copy_decl_stmt;
    F->fold_if_stmt = copy_if_stmt;
    F->fold_for_stmt = copy_for_stmt;
    F->fold_while_stmt = copy_while_stmt;
    F->fold_label_stmt = copy_label_stmt;
    F->fold_return_stmt = copy_return_stmt;
    F->fold_variant_decl = copy_variant_decl;
    F->fold_var_decl = copy_var_decl;
    F->fold_func_decl = copy_func_decl;
    F->fold_struct_decl = copy_struct_decl;
    F->fold_field_decl = copy_field_decl;
    F->fold_generic_decl = copy_generic_decl;
    F->fold_type_decl = copy_type_decl;
    F->fold_path_pat = copy_path_pat;
    F->fold_tuple_pat = copy_tuple_pat;
    F->fold_field_pat = copy_field_pat;
    F->fold_struct_pat = copy_struct_pat;
    F->fold_variant_pat = copy_variant_pat;
}

AstDecl *pawA_copy_decl(Ast *ast, AstDecl *decl)
{
    Copier C = {
        .lex = ast->lex,
        .ast = ast,
    };
    AstFolder F;
    setup_copy_pass(&F, &C);
    return F.fold_decl(&F, decl);
}

// *******************************
//     AST stenciling routines
// *******************************

typedef struct Subst {
    struct Stenciler *S;
    AstList *before;
    AstList *after;
} Subst;

static AstType *subst_type(AstFolder *F, AstType *type);

typedef struct ScopeState {
    struct ScopeState *outer;
    Scope *source;
    Scope *target;
} ScopeState;

typedef struct Stenciler {
    AstTypeFolder fold;
    Subst subst;
    ScopeState *scope;
    Map *decls;
    Lex *lex; // lexical state
    Ast *ast; // AST being copied
} Stenciler;

static Scope *new_scope_table(AstFolder *F)
{
    Lex *lex = F->state.S->lex;
    return pawM_new(env(lex), Scope); // TODO
}

static void enter_scope(AstFolder *F, ScopeState *state, Scope *source)
{
    Stenciler *S = F->state.S;
    state->source = source;
    state->target = new_scope_table(F);
    state->outer = S->scope;
    S->scope = state;
}

static Scope *leave_scope(AstFolder *F)
{
    Stenciler *S = F->state.S;
    ScopeState *r = S->scope;
    S->scope = r->outer;
    // stenciler should pass over all symbols
    paw_assert(r->source->nsymbols == r->target->nsymbols);
    return r->target;
}

static void add_symbol(AstFolder *F, AstDecl *decl)
{
    Stenciler *S = F->state.S;
    Scope *source = S->scope->source;
    Scope *target = S->scope->target;
    paw_assert(target->nsymbols < source->nsymbols);
    const Symbol *old_sym = source->symbols[target->nsymbols];
    Symbol *new_sym = pawP_add_symbol(S->lex, target);
    paw_assert(old_sym->is_init);
    *new_sym = (Symbol){
        .is_const = old_sym->is_const,
        .is_generic = old_sym->is_generic,
        .is_type = old_sym->is_type,
        .is_init = PAW_TRUE,
        .name = decl->hdr.name,
        .decl = decl,
    };
}

static AstDecl *find_decl(Stenciler *S, AstDecl *old_decl)
{
    const Value key = {.p = old_decl};
    Value *pvalue = pawH_get(env(S->lex), S->decls, key);
    if (pvalue == NULL) {
        return old_decl;
    }
    return pvalue->p;
}

static void link_decls(AstFolder *F, AstDecl *old_decl, AstDecl *new_decl)
{
    Stenciler *S = F->state.S;
    const Value key = {.p = old_decl};
    const Value value = {.p = new_decl};
    pawH_insert(env(S->lex), S->decls, key, value);
    if (a_kind(new_decl) != DECL_INSTANCE) {
        add_symbol(F, new_decl);
    }
}

//clang-format off
#define make_stencil_prep(name, T, body)                                       \
    static T *stencil_prep_##name##_aux(AstFolder *F, T *t)                    \
    {                                                                          \
        T *r = pawA_new_##name(F->ast, a_kind(t));                             \
        r->hdr.kind = t->hdr.kind;                                             \
        r->hdr.line = t->hdr.line;                                             \
        body return r;                                                         \
    }
make_stencil_prep(expr, AstExpr,
                  {
                      Stenciler *S = F->state.S;
                      if (a_is_adt(t)) {
                          const DefId did = a_type(t)->adt.did;
                          AstDecl *decl = pawA_get_decl(F->ast, did);
                          r->hdr.type = a_type(find_decl(S, decl));
                      } else if (a_is_fdef(t)) {
                          const DefId did = a_type(t)->func.did;
                          AstDecl *decl = pawA_get_decl(F->ast, did);
                          r->hdr.type = a_type(find_decl(S, decl));
                      } else {
                          r->hdr.type = subst_type(F, a_type(t));
                      }
                  })
make_stencil_prep(decl, AstDecl,
                  {
                      r->hdr.name = t->hdr.name;
                      r->hdr.def = pawA_add_decl(F->ast, r);
                      link_decls(F, t, r); // subst_type() is dependant
                      AstType *type = subst_type(F, a_type(t));
                      if (a_is_func_decl(r)) {
                          type->func.did = r->hdr.def;
                      } else if (a_is_struct_decl(r)) {
                          type->adt.did = r->hdr.def;
                      }
                      r->hdr.type = type;
                      return r;
                  }) 
make_stencil_prep(stmt, AstStmt, {})
//clang-format on

// Helpers for stenciling: create a new node of the given type and kind,
// and copy the common fields
#define stencil_prep_expr(F, e) stencil_prep_expr_aux(F, cast_expr(e))
#define stencil_prep_decl(F, d) stencil_prep_decl_aux(F, cast_decl(d))
#define stencil_prep_stmt(F, s) stencil_prep_stmt_aux(F, cast_stmt(s))

#define make_stencil_list(name, T)                                             \
    static AstList *stencil_##name##s(AstFolder *F, AstList *old_list)         \
    {                                                                          \
        AstList *new_list = pawA_list_new(F->ast);                             \
        for (int i = 0; i < old_list->count; ++i) {                            \
            T *decl = F->fold_##name(F, old_list->data[i]);                    \
            pawA_list_push(F->ast, &new_list, decl);                           \
        }                                                                      \
        return new_list;                                                       \
    }
make_stencil_list(expr, AstExpr) 
make_stencil_list(decl, AstDecl)
make_stencil_list(stmt, AstStmt)

static AstStmt *stencil_block_stmt(AstFolder *F, Block *s)
{
    ScopeState state;
    enter_scope(F, &state, s->scope);

    AstStmt *r = stencil_prep_stmt(F, s);
    r->block.stmts = stencil_stmts(F, s->stmts);

    r->block.scope = leave_scope(F);
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

static AstExpr *stencil_struct_key(AstFolder *F, AstIdent *e)
{
    AstExpr *r = pawA_new_expr(F->ast, EXPR_NAME);
    r->name.line = e->line;
    r->name.name = e->name;
    return r;
}

static AstExpr *stencil_item_expr(AstFolder *F, ItemExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    if (a_kind(e->key) == EXPR_NAME) {
        r->item.key = stencil_struct_key(F, &e->key->name);
    } else {
        r->item.key = F->fold_expr(F, e->key);
    }
    r->item.value = F->fold_expr(F, e->value);
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
    r->func.result = F->fold_expr(F, e->result);
    return r;
}

static AstExpr *stencil_typename_expr(AstFolder *F, TypeName *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->type_name.name = e->name;
    r->type_name.args = stencil_exprs(F, e->args);
    return r;
}

static AstExpr *stencil_typelist_expr(AstFolder *F, TypeList *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->typelist.types = stencil_exprs(F, e->types);
    return r;
}

static AstDecl *stencil_field_decl(AstFolder *F, FieldDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);
    r->field.tag = F->fold_expr(F, d->tag);
    r->field.name = d->name;
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

static AstDecl *stencil_variant_decl(AstFolder *F, VariantDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);
    r->variant.name = d->name;

    ScopeState state;
    enter_scope(F, &state, d->scope);
    r->variant.fields = stencil_decls(F, d->fields);
    r->variant.scope = leave_scope(F);
    r->variant.index = d->index;
    return r;
}

static AstDecl *stencil_struct_decl(AstFolder *F, StructDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);

    ScopeState state;
    enter_scope(F, &state, d->scope);

    r->struct_.is_global = d->is_global;
    r->struct_.is_struct = d->is_struct;
    r->struct_.name = d->name;
    r->struct_.generics = stencil_decls(F, d->generics);

    ScopeState field_state;
    enter_scope(F, &field_state, d->field_scope);
    r->struct_.fields = stencil_decls(F, d->fields);
    r->struct_.field_scope = leave_scope(F);

    r->struct_.scope = leave_scope(F);
    r->struct_.monos = stencil_decls(F, d->monos);
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

static AstExpr *stencil_conversion_expr(AstFolder *F, ConversionExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->conv.to = e->to;
    r->conv.arg = F->fold_expr(F, e->arg);
    return r;
}

static AstExpr *stencil_call_expr(AstFolder *F, CallExpr *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    if (a_is_fdef(e->func)) {
        AstDecl *old_func = pawA_get_decl(F->ast, e->func->func.did);
        AstDecl *new_func = find_decl(F->state.S, old_func);
        r->call.func = a_type(new_func);
    } else {
        r->call.func = subst_type(F, e->func);
    }
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

static AstDecl *stencil_instance_decl(AstFolder *F, InstanceDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);

    ScopeState state;
    enter_scope(F, &state, d->scope);

    r->inst.types = stencil_decls(F, d->types);
    if (d->fields != NULL) {
        ScopeState field_state;
        enter_scope(F, &field_state, d->field_scope);
        r->inst.fields = stencil_decls(F, d->fields);
        r->inst.field_scope = leave_scope(F);
    }

    r->inst.scope = leave_scope(F);
    return r;
}

static AstDecl *stencil_func_decl(AstFolder *F, FuncDecl *d)
{
    AstDecl *r = stencil_prep_decl(F, d);

    ScopeState state;
    enter_scope(F, &state, d->scope);
    add_symbol(F, r); // callee slot
    r->func.is_global = d->is_global;
    r->func.name = d->name;
    r->func.generics = stencil_decls(F, d->generics);
    r->func.params = stencil_decls(F, d->params);
    r->func.result = F->fold_expr(F, d->result);
    r->func.body = stencil_block(F, d->body);
    r->func.fn_kind = d->fn_kind;

    r->func.scope = leave_scope(F);
    r->func.monos = stencil_decls(F, d->monos);
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
    ScopeState state;
    enter_scope(F, &state, s->scope);

    AstStmt *r = stencil_prep_stmt(F, s);
    r->while_.cond = F->fold_expr(F, s->cond);
    r->while_.block = stencil_block(F, s->block);

    r->while_.scope = leave_scope(F);
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
    ScopeState state;
    enter_scope(F, &state, s->scope);

    AstStmt *r = stencil_prep_stmt(F, s);
    if (s->kind == STMT_FORNUM) {
        r->for_.fornum.begin = F->fold_expr(F, s->fornum.begin);
        r->for_.fornum.end = F->fold_expr(F, s->fornum.end);
        r->for_.fornum.step = F->fold_expr(F, s->fornum.step);
    } else {
        r->for_.forin.target = F->fold_expr(F, s->forin.target);
    }
    r->for_.block = stencil_block(F, s->block);

    r->while_.scope = leave_scope(F);
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

static AstExpr *stencil_access_expr(AstFolder *F, Access *e)
{
    AstExpr *r = stencil_prep_expr(F, e);
    r->access.target = F->fold_expr(F, e->target);
    r->access.name = e->name;
    return r;
}

static AstStmt *stencil_decl_stmt(AstFolder *F, AstDeclStmt *s)
{
    AstStmt *r = stencil_prep_stmt(F, s);
    r->decl.decl = F->fold_decl(F, s->decl);
    return r;
}

static AstPat *stencil_path_pat(AstFolder *V, AstPathPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *stencil_tuple_pat(AstFolder *V, AstTuplePat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *stencil_field_pat(AstFolder *V, AstFieldPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *stencil_struct_pat(AstFolder *V, AstStructPat *p)
{
    paw_unused(V);
    return cast_pat(p);
}

static AstPat *stencil_variant_pat(AstFolder *V, AstVariantPat *p)
{
    paw_unused(V);
    return cast_pat(p);
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
    F->fold_conversion_expr = stencil_conversion_expr;
    F->fold_call_expr = stencil_call_expr;
    F->fold_index_expr = stencil_index_expr;
    F->fold_access_expr = stencil_access_expr;
    F->fold_selector_expr = stencil_selector_expr;
    F->fold_item_expr = stencil_item_expr;
    F->fold_typename_expr = stencil_typename_expr;
    F->fold_typelist_expr = stencil_typelist_expr;
    F->fold_signature_expr = stencil_signature_expr;
    F->fold_block_stmt = stencil_block_stmt;
    F->fold_expr_stmt = stencil_expr_stmt;
    F->fold_decl_stmt = stencil_decl_stmt;
    F->fold_if_stmt = stencil_if_stmt;
    F->fold_for_stmt = stencil_for_stmt;
    F->fold_while_stmt = stencil_while_stmt;
    F->fold_label_stmt = stencil_label_stmt;
    F->fold_return_stmt = stencil_return_stmt;
    F->fold_variant_decl = stencil_variant_decl;
    F->fold_var_decl = stencil_var_decl;
    F->fold_func_decl = stencil_func_decl;
    F->fold_struct_decl = stencil_struct_decl;
    F->fold_field_decl = stencil_field_decl;
    F->fold_generic_decl = stencil_generic_decl;
    F->fold_instance_decl = stencil_instance_decl;
    F->fold_type_decl = stencil_type_decl;
    F->fold_path_pat = stencil_path_pat;
    F->fold_tuple_pat = stencil_tuple_pat;
    F->fold_field_pat = stencil_field_pat;
    F->fold_struct_pat = stencil_struct_pat;
    F->fold_variant_pat = stencil_variant_pat;
}

static AstList *prep_binder(AstTypeFolder *F, AstList *binder)
{
    Stenciler *S = F->state;
    AstList *copy = pawA_list_new(S->ast);
    for (int i = 0; i < binder->count; ++i) {
        pawA_list_push(S->ast, &copy, binder->data[i]);
    }
    F->fold_binder(F, copy);
    return copy;
}

static AstType *prep_fptr(AstTypeFolder *F, AstFuncPtr *t)
{
    Subst *subst = F->state;
    Stenciler *S = subst->S;

    AstType *r = pawA_new_type(S->ast, AST_TYPE_FPTR);
    r->fptr.params = prep_binder(F, t->params);
    r->fptr.result = F->fold(F, t->result);
    return r;
}

static AstType *prep_func(AstTypeFolder *F, AstFuncDef *t)
{
    Subst *subst = F->state;
    Stenciler *S = subst->S;

    AstType *r = pawA_new_type(S->ast, AST_TYPE_FUNC);
    AstDecl *old_base = pawA_get_decl(S->ast, t->base);
    AstDecl *new_base = find_decl(S, old_base);
    AstDecl *old_decl = pawA_get_decl(S->ast, t->did);
    AstDecl *new_decl = find_decl(S, old_decl);
    r->func.did = new_decl->hdr.def;
    r->func.base = new_base->hdr.def;

    r->func.types = prep_binder(F, t->types);
    r->func.params = prep_binder(F, t->params);
    r->func.result = F->fold(F, t->result);
    return r;
}

static AstType *prep_adt(AstTypeFolder *F, AstAdt *t)
{
    Subst *subst = F->state;
    Stenciler *S = subst->S;
    if (t->did <= PAW_TSTRING) {
        return a_cast_type(t);
    }

    AstDecl *old_base = pawA_get_decl(S->ast, t->base);
    AstDecl *new_base = find_decl(S, old_base);

    AstType *r = pawA_new_type(S->ast, AST_TYPE_ADT);
    r->adt.did = new_base->hdr.def;
    r->adt.base = new_base->hdr.def;
    r->adt.types = prep_binder(F, t->types);
    return r;
}

static AstType *maybe_subst(AstTypeFolder *F, AstType *t)
{
    Subst *s = &cast(F->state, Stenciler *)->subst;
    for (int i = 0; i < s->before->count; ++i) {
        if (t == s->before->data[i]) {
            return s->after->data[i];
        }
    }
    return t;
}

static AstType *prep_generic(AstTypeFolder *F, AstGeneric *t)
{
    return maybe_subst(F, a_cast_type(t));
}

static AstType *subst_type(AstFolder *F, AstType *type)
{
    Stenciler *S = F->state.S;
    return S->fold.fold(&S->fold, type);
}

static void add_existing_link(paw_Env *P, Map *map, AstDecl *key,
                              AstDecl *value)
{
    const Value k = {.p = key};
    const Value v = {.p = value};
    pawH_insert(P, map, k, v);
}

static void init_links(Lex *lex, Map *map, FuncDecl *base, InstanceDecl *inst)
{
    paw_Env *P = env(lex);
    for (int i = 0; i < inst->scope->nsymbols; ++i) {
        Symbol *old_sym = base->scope->symbols[i];
        Symbol *new_sym = inst->scope->symbols[i];
        add_existing_link(P, map, old_sym->decl, new_sym->decl);
    }
}

FuncDecl *pawA_stencil_func(Ast *ast, FuncDecl *base, AstDecl *inst)
{
    paw_Env *P = env(ast->lex);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    v_set_object(pv, map);

    init_links(ast->lex, map, base, &inst->inst);

    Stenciler S = {
        .decls = map,
        .lex = ast->lex,
        .ast = ast,
    };
    AstFolder F;
    setup_stencil_pass(&F, &S);

    S.subst.before = base->type->func.types;
    S.subst.after = inst->inst.type->func.types;
    S.subst.S = &S;

    pawA_type_folder_init(&S.fold, &S);
    S.fold.fold_adt = prep_adt;
    S.fold.fold_fptr = prep_fptr;
    S.fold.fold_func = prep_func;
    S.fold.fold_generic = prep_generic;

    ScopeState state = {
        .source = base->scope,
        .target = inst->inst.scope,
    };
    S.scope = &state;

    AstDecl copy = *inst;
    inst->func.kind = DECL_FUNC;
    inst->func.name = base->name;
    inst->func.generics = copy.inst.types;
    add_symbol(&F, inst); // callee slot
    inst->func.params = stencil_decls(&F, base->params);
    inst->func.result = F.fold_expr(&F, base->result);
    inst->func.body = stencil_block(&F, base->body);
    inst->func.fn_kind = base->fn_kind;

    inst->func.scope = leave_scope(&F);
    paw_assert(S.scope == NULL);
    pawC_pop(P); // pop decl. map
    return &inst->func;
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

#define dump_fmt(P, fmt, ...)                                                  \
    (indent_line(P), fprintf((P)->out, fmt, __VA_ARGS__))
#define dump_msg(P, msg) (indent_line(P), fprintf((P)->out, msg))

static void dump_type_aux(Printer *P, AstType *type);

static void dump_binder(Printer *P, AstList *binder)
{
    for (int i = 0; i < binder->count; ++i) {
        dump_type_aux(P, binder->data[i]);
        if (i < binder->count - 1) {
            printf(", ");
        }
    }
}

static void dump_type_aux(Printer *P, AstType *type)
{
    const char *basic[] = {
        "()", "bool", "int", "float", "string",
    };
    switch (y_kind(type)) {
        case AST_TYPE_UNKNOWN: {
            printf("<unknown>");
            break;
        }
        case AST_TYPE_GENERIC: {
            AstGeneric *gen = &type->generic;
            printf("%s", gen->name->text);
            break;
        }
        case AST_TYPE_ADT: {
            AstAdt *adt = &type->adt;
            printf("%d", adt->base);
            if (adt->types->count > 0) {
                printf("[");
                dump_binder(P, adt->types);
                printf("]");
            }
            break;
        }
        case AST_TYPE_FUNC: {
            AstFuncDef *func = &type->func;
            printf("%d", func->base);
            if (func->types->count > 0) {
                printf("[");
                dump_binder(P, func->types);
                printf("]");
            }
            printf("(");
            dump_binder(P, func->params);
            printf(") -> ");
            dump_type_aux(P, func->result);
            break;
        }
        default: {
            paw_assert(a_is_fptr(type));
            AstFuncPtr *fptr = &type->fptr;
            printf("fn ");
            printf("(");
            dump_binder(P, fptr->params);
            printf(") -> ");
            dump_type_aux(P, fptr->result);
        }
    }
}

static void dump_type(Printer *P, AstType *type)
{
    indent_line(P);
    printf("type: ");
    if (type != NULL) {
        dump_type_aux(P, type);
    } else {
        printf("(null)");
    }
    printf("\n");
}

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
        case DECL_VARIANT:
            fprintf(P->out, "VariantDecl");
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
        case DECL_INSTANCE:
            fprintf(P->out, "InstanceDecl");
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
        case EXPR_NAME:
            fprintf(P->out, "Ident");
            break;
        case EXPR_INDEX:
            fprintf(P->out, "Index");
            break;
        case EXPR_SELECTOR:
            fprintf(P->out, "Selector");
            break;
        case EXPR_ACCESS:
            fprintf(P->out, "Access");
            break;
        case EXPR_FUNCTYPE:
            fprintf(P->out, "FuncType");
            break;
        case EXPR_TYPENAME:
            fprintf(P->out, "TypeName");
            break;
        case EXPR_MATCH:
            fprintf(P->out, "MatchExpr");
            break;
        case EXPR_MATCHARM:
            fprintf(P->out, "MatchArm");
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
            fprintf(P->out, "ExprStmt");
            break;
        case STMT_DECL:
            fprintf(P->out, "DeclStmt");
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

static void print_pat_kind(Printer *P, void *node)
{
    AstPat *p = node;
    switch (a_kind(p)) {
        case AST_PAT_LITERAL:
            fprintf(P->out, "LiteralPat");
            break;
        case AST_PAT_PATH:
            fprintf(P->out, "PathPat");
            break;
        case AST_PAT_FIELD:
            fprintf(P->out, "FieldPat");
            break;
        case AST_PAT_TUPLE:
            fprintf(P->out, "TuplePat");
            break;
        case AST_PAT_VARIANT:
            fprintf(P->out, "VariantPat");
            break;
        case AST_PAT_STRUCT:
            fprintf(P->out, "StructPat");
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

#define dump_block(P, b) check_exp((b)->kind == STMT_BLOCK, \
                                   dump_stmt(P, cast_stmt(b)))
#define dump_name(P, s) dump_fmt(P, "name: %s\n", s ? s->text : NULL)

static void dump_expr(Printer *P, AstExpr *e);
static void dump_decl(Printer *P, AstDecl *d);
static void dump_stmt(Printer *P, AstStmt *s);
static void dump_pat(Printer *P, AstPat *p);

// clang-format off
#define make_list_dumper(name, T)                                              \
    static void dump_##name##_list(Printer *P, AstList *list,                  \
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
make_list_dumper(expr, AstExpr) 
make_list_dumper(decl, AstDecl)
make_list_dumper(stmt, AstStmt)
make_list_dumper(pat, AstPat)
// clang-format on

static void dump_decl(Printer *P, AstDecl *d)
{
    if (predump_node(P, d, print_decl_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", d->hdr.line);
    dump_type(P, a_type(d));
    switch (a_kind(d)) {
        case DECL_FUNC:
            dump_fmt(P, "is_global: %d\n", d->func.is_global);
            dump_fmt(P, "receiver: %p\n", (void *)d->func.receiver);
            dump_fmt(P, "name: %s\n", d->func.name->text);
            dump_decl_list(P, d->func.generics, "generics");
            dump_decl_list(P, d->func.params, "params");
            dump_msg(P, "result: ");
            dump_expr(P, d->func.result);
            dump_msg(P, "body: ");
            dump_block(P, d->func.body);
            dump_decl_list(P, d->func.monos, "monos");
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
        case DECL_VARIANT:
            dump_name(P, d->variant.name);
            dump_decl_list(P, d->variant.fields, "fields");
            break;
        case DECL_STRUCT:
            dump_name(P, d->struct_.name);
            dump_fmt(P, "is_struct: %d\n", d->struct_.is_struct);
            dump_decl_list(P, d->struct_.generics, "generics");
            dump_decl_list(P, d->struct_.fields, "fields");
            dump_decl_list(P, d->func.monos, "monos");
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
        case DECL_INSTANCE:
            dump_name(P, d->type.name);
            dump_decl_list(P, d->inst.fields, "fields");
            break;
        default:
            paw_assert(0);
    }
    --P->indent;
    dump_msg(P, "}\n");
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
            dump_expr(P, s->return_.expr);
            break;
        default:
            break;
    }
    --P->indent;
    dump_msg(P, "}\n");
}

static void dump_pat(Printer *P, AstPat *p)
{
    if (predump_node(P, p, print_pat_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", p->hdr.line);
    switch (a_kind(p)) {
        case AST_PAT_LITERAL:
            dump_msg(P, "value: ");
            dump_expr(P, p->literal.expr);
            break;
        case AST_PAT_PATH:
            dump_msg(P, "path: <path>\n"); // TODO: dump path
            break;
        case AST_PAT_FIELD:
            dump_name(P, p->field.name);
            dump_msg(P, "pat: ");
            dump_pat(P, p->field.pat);
            break;
        case AST_PAT_TUPLE:
            dump_pat_list(P, p->tuple.elems, "elems");
            break;
        case AST_PAT_VARIANT:
            dump_msg(P, "path: <path>\n"); // TODO: dump path
            dump_pat_list(P, p->variant.elems, "elems");
            break;
        default:
            paw_assert(a_kind(p) == AST_PAT_STRUCT);
            break;
    }
    --P->indent;
    dump_msg(P, "}\n");
}

static void dump_expr(Printer *P, AstExpr *e)
{
    if (predump_node(P, e, print_expr_kind)) {
        fprintf(P->out, "(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", e->hdr.line);
    dump_type(P, a_type(e));
    switch (a_kind(e)) {
        case EXPR_LITERAL:
            switch (e->literal.lit_kind) {
                case LIT_BASIC:
                    switch (e->literal.basic.t) {
                        case PAW_TUNIT:
                            dump_msg(P, "type: ()\n");
                            break;
                        case PAW_TBOOL:
                            dump_msg(P, "type: bool\n");
                            dump_fmt(P, "value: %s\n", v_true(e->literal.basic.value) ? "true" : "false");
                            break;
                        case PAW_TINT:
                            dump_msg(P, "type: int\n");
                            dump_fmt(P, "value: %" PRId64 "\n", v_int(e->literal.basic.value));
                            break;
                        case PAW_TFLOAT:
                            dump_msg(P, "type: float\n");
                            dump_fmt(P, "value: %f\n", v_float(e->literal.basic.value));
                            break;
                        default:
                            paw_assert(e->literal.basic.t == PAW_TSTRING);
                            dump_msg(P, "type: string\n");
                            dump_fmt(P, "value: %s\n", v_string(e->literal.basic.value)->text);
                            break;
                    }
                    break;
                default:
                    paw_assert(e->literal.lit_kind == LIT_COMPOSITE);
                    dump_msg(P, "target: ");
                    dump_expr(P, e->literal.comp.target);
                    dump_expr_list(P, e->literal.comp.items, "items");
            }
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
        case EXPR_NAME:
            dump_name(P, e->name.name);
            break;
        case EXPR_INDEX:
            dump_msg(P, "target: ");
            dump_expr(P, e->index.target);
            dump_expr_list(P, e->index.elems, "elems");
            break;
        case EXPR_SELECTOR:
            dump_msg(P, "target: ");
            dump_expr(P, e->selector.target);
            dump_name(P, e->selector.name);
            break;
        case EXPR_FUNCTYPE:
            dump_expr_list(P, e->func.params, "params");
            dump_msg(P, "result: ");
            dump_expr(P, e->func.result);
            break;
        case EXPR_TYPENAME:
            dump_name(P, e->type_name.name);
            dump_expr_list(P, e->type_name.args, "args");
            break;
        case EXPR_MATCH:
            dump_msg(P, "target: ");
            dump_expr(P, e->match.target);
            dump_expr_list(P, e->match.arms, "arms");
            break;
        case EXPR_MATCHARM:
            dump_msg(P, "guard: ");
            dump_pat(P, e->arm.guard);
            dump_msg(P, "value: ");
            dump_expr(P, e->arm.value);
        default:
            break;
    }
    --P->indent;
    dump_msg(P, "}\n");
}

void pawA_dump_type(FILE *out, AstType *type)
{
    Printer P;
    P.out = out;
    P.indent = 0;
    dump_type(&P, type);
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

void pawA_dump_pat(FILE *out, AstPat *pat)
{
    Printer P;
    P.out = out;
    P.indent = 0;
    dump_pat(&P, pat);
}
