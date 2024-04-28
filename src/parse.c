// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "parse.h"
#include "ast.h"
#include "auxlib.h"
#include "call.h"
#include "code.h"
#include "check.h"
#include "env.h"
#include "gc_aux.h"
#include "lex.h"
#include "map.h"
#include "mem.h"
#include "opcode.h"
#include "paw.h"
#include "str.h"
#include "type.h"
#include "util.h"
#include "value.h"
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define new_expr(lex, kind) pawA_new_expr((lex)->pm->ast, kind)
#define new_decl(lex, kind) pawA_new_decl((lex)->pm->ast, kind)
#define new_stmt(lex, kind) pawA_new_stmt((lex)->pm->ast, kind)
#define new_expr_list(lex) pawA_new_expr_list((lex)->pm->ast)
#define new_decl_list(lex) pawA_new_decl_list((lex)->pm->ast)
#define new_stmt_list(lex) pawA_new_stmt_list((lex)->pm->ast)

// recursive non-terminals
static AstExpr *expression(Lex *lex, unsigned prec);
static AstStmt *statement(Lex *lex);
#define expression0(x) expression(x, 0)

static void expected_symbol(Lex *lex, const char *want)
{
    pawX_error(lex, "expected %s", want);
}

static void missing_delim(Lex *lex, TokenKind want, TokenKind open, int open_line)
{
    pawX_error(lex, "expected '%c' to match '%c' on line %d",
               want, open, open_line);
}

static void delim_next(Lex *lex, TokenKind want, TokenKind open, int open_line)
{
    if (lex->t.kind != want) {
        missing_delim(lex, want, open, open_line);
    }
    pawX_next(lex);
}

// ORDER UnaryOp
typedef enum {
    UN_LEN, // #
    UN_NEG, // -
    UN_NOT, // !
    UN_BNOT, // ~

    NUNOPS
} UnOp;

// ORDER BinaryOp
typedef enum {
    INFIX_EQ, // ==
    INFIX_NE, // !=
    INFIX_LT, // <
    INFIX_LE, // <=
    INFIX_GT, // >
    INFIX_GE, // >=
    INFIX_IN, // in
    INFIX_ADD, // +
    INFIX_SUB, // -
    INFIX_MUL, // *
    INFIX_DIV, // /
    INFIX_MOD, // %
    INFIX_BXOR, // ^
    INFIX_BAND, // &
    INFIX_BOR, // |
    INFIX_SHL, // <<
    INFIX_SHR, // >>
    INFIX_AND, // &&
    INFIX_OR, // ||
    INFIX_COND, // ??::
    INFIX_COALESCE, // ?:

    NINFIX
} InfixOp;

#define NOT_UNOP NUNOPS
#define NOT_INFIX NINFIX

static const struct {
    uint8_t left;
    uint8_t right;
} kInfixPrec[NINFIX] = {
    [INFIX_MUL] = {13, 13},
    [INFIX_DIV] = {13, 13},
    [INFIX_MOD] = {13, 13},
    [INFIX_ADD] = {12, 12},
    [INFIX_SUB] = {12, 12},
    [INFIX_SHL] = {10, 10},
    [INFIX_SHR] = {10, 10},
    [INFIX_BAND] = {9, 9},
    [INFIX_BXOR] = {8, 8},
    [INFIX_BOR] = {7, 7},
    [INFIX_IN] = {6, 6},
    [INFIX_LT] = {6, 6},
    [INFIX_LE] = {6, 6},
    [INFIX_GT] = {6, 6},
    [INFIX_GE] = {6, 6},
    [INFIX_EQ] = {5, 5},
    [INFIX_NE] = {5, 5},
    [INFIX_AND] = {4, 4},
    [INFIX_OR] = {3, 3},
    [INFIX_COALESCE] = {2, 2},
    [INFIX_COND] = {1, 0}, // right-associative
};

static const uint8_t kUnOpPrecedence = 14;

static unsigned left_prec(InfixOp op)
{
    return kInfixPrec[op].left;
}

static unsigned right_prec(InfixOp op)
{
    return kInfixPrec[op].right;
}

static UnOp get_unop(TokenKind kind)
{
    switch (kind) {
        case '#':
            return UN_LEN;
        case '-':
            return UN_NEG;
        case '!':
            return UN_NOT;
        case '~':
            return UN_BNOT;
        default:
            return NOT_UNOP;
    }
}

static InfixOp get_infixop(TokenKind kind)
{
    switch (kind) {
        case '+':
            return INFIX_ADD;
        case '-':
            return INFIX_SUB;
        case '*':
            return INFIX_MUL;
        case '/':
            return INFIX_DIV;
        case '%':
            return INFIX_MOD;
        case '<':
            return INFIX_LT;
        case '>':
            return INFIX_GT;
        case '^':
            return INFIX_BXOR;
        case '&':
            return INFIX_BAND;
        case '|':
            return INFIX_BOR;
        case TK_IN:
            return INFIX_IN;
        case TK_QUESTION2:
            return INFIX_COND;
        case TK_EQUALS2:
            return INFIX_EQ;
        case TK_LESS2:
            return INFIX_SHL;
        case TK_GREATER2:
            return INFIX_SHR;
        case TK_AMPER2:
            return INFIX_AND;
        case TK_PIPE2:
            return INFIX_OR;
        case TK_BANG_EQ:
            return INFIX_NE;
        case TK_LESS_EQ:
            return INFIX_LE;
        case TK_GREATER_EQ:
            return INFIX_GE;
        default:
            return NOT_INFIX;
    }
}

static void skip(Lex *lex)
{
    pawX_next(lex);
}

static void check(Lex *lex, TokenKind want)
{
    if (lex->t.kind != want) {
        pawX_error(lex, "unexpected symbol");
    }
}

static void check_next(Lex *lex, TokenKind want)
{
    check(lex, want);
    skip(lex);
}

static paw_Bool test(Lex *lex, TokenKind kind)
{
    return lex->t.kind == kind;
}

static paw_Bool test_next(Lex *lex, TokenKind kind)
{
    if (test(lex, kind)) {
        skip(lex);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

// Eat a semicolon, if one exists
static void semicolon(Lex *lex)
{
    test_next(lex, ';');
}

static String *parse_name(Lex *lex)
{
    check(lex, TK_NAME);
    String *name = v_string(lex->t.value);
    skip(lex);
    return name;
}

static AstExpr *new_basic_lit(Lex *lex, Value v, paw_Type code)
{
    AstExpr *r = new_expr(lex, EXPR_LITERAL);
    r->literal.basic.t = code;
    r->literal.basic.value = v;
    return r;
}

static AstExpr *emit_unit(Lex *lex)
{
    AstExpr *r = new_expr(lex, EXPR_LITERAL);
    r->literal.basic.t = PAW_TUNIT;
    return r;
}

static AstExpr *emit_bool(Lex *lex, paw_Bool b)
{
    Value v;
    v_set_bool(&v, b);
    return new_basic_lit(lex, v, PAW_TBOOL);
}

#define make_link_node(suffix, T, source, link) \
    static void link_ ## suffix(T ## List *list, T **plast, T *next) \
    { \
        if (list->first == NULL) { \
            list->first = next; \
        } else { \
            (*plast)->source.link = next; \
        } \
        *plast = next; \
        ++list->count; \
    }
make_link_node(decl, AstDecl, hdr, next)
make_link_node(expr, AstExpr, hdr, next)
make_link_node(stmt, AstStmt, hdr, next)
make_link_node(method, AstDecl, func, sibling)

static AstExpr *type_expr(Lex *lex);

static AstExprList *parse_type_list(Lex *lex)
{
    ++lex->expr_depth;
    AstExprList *list = new_expr_list(lex);
    AstExpr *prev;
    do {
        if (list->count == ARGC_MAX) {
            limit_error(lex, "generic type arguments", ARGC_MAX);
        }
        AstExpr *type = type_expr(lex);
        link_expr(list, &prev, type);
    } while (test_next(lex, ','));
    --lex->expr_depth;
    return list;
}

static AstExprList *maybe_type_args(Lex *lex)
{
    AstExprList *types = NULL;
    const int line = lex->line;
    if (test_next(lex, '[')) {
        // type arguments (requires at least 1)
        types = parse_type_list(lex);
        delim_next(lex, ']', '[', line);
    }
    return types;
}

static AstExprList *parse_param(Lex *lex, int line)
{
    if (test_next(lex, ')')) {
        return new_expr_list(lex);
    }
    AstExprList *list = parse_type_list(lex);
    delim_next(lex, ')', '(', line);
    return list;
}

static void parse_signature(Lex *lex, AstExpr *pe)
{
    const int line = lex->line;
    pe->func.kind = EXPR_FUNC_TYPE;
    check_next(lex, '(');

    // function parameters
    pe->func.params = parse_param(lex, line);

    // return type annotation
    if (test_next(lex, TK_ARROW)) {
        pe->func.return_ = type_expr(lex);
    } else {
        pe->func.return_ = emit_unit(lex);
    }
}

static void parse_named_type(Lex *lex, AstExpr *pe)
{
    String *name = parse_name(lex);
    pe->type_name.name = name;
    pe->type_name.kind = EXPR_TYPE_NAME;
    // list of concrete types between '[' and ']'
    pe->type_name.args = maybe_type_args(lex);
}

static AstExpr *type_expr(Lex *lex)
{
    AstExpr *r = new_expr(lex, 0 /* set in parse_*() */);
    if (test_next(lex, TK_FN)) {
        parse_signature(lex, r); 
    } else {
        parse_named_type(lex, r);
    }
    return r;
}

static AstExpr *unit_type(Lex *lex)
{
    AstExpr *r = new_expr(lex, EXPR_TYPE_NAME);
    r->type_name.name = pawE_cstr(env(lex), CSTR_UNIT);
    return r;
}

static AstExpr *ret_annotation(Lex *lex)
{
    return test_next(lex, TK_ARROW) ? type_expr(lex) : unit_type(lex);
}

static AstExpr *var_annotation(Lex *lex)
{
    if (test_next(lex, ':')) {
        AstExpr *tn = type_expr(lex);
        if (tn == NULL) {
            pawX_error(lex, "invalid type annotation");
        }
        return tn;
    }
    return NULL; // needs inference
}

static AstDecl *param_decl(Lex *lex)
{
    AstDecl *r = new_decl(lex, DECL_FIELD);
    r->field.name = parse_name(lex);
    r->field.tag = var_annotation(lex);
    if (r->field.tag == NULL) {
        pawX_error(lex, "expected type annotation on parameter '%s'", 
                   r->field.name->text);
    }
    return r;
}

static AstDecl *var_decl(Lex *lex, int line, paw_Bool global)
{
    AstDecl *r = new_decl(lex, DECL_VAR);
    r->var.line = line; // line containing 'global' or 'let'
    r->var.name = parse_name(lex);
    r->var.tag = var_annotation(lex);
    if (!test_next(lex, '=')) {
        pawX_error(lex, "missing initializer");
    }
    r->var.init = expression0(lex);
    r->var.is_global = global;
    semicolon(lex);
    return r;
}

static AstExprList *expr_list1(Lex *lex, const char *what)
{
    AstExprList *list = new_expr_list(lex);
    AstExpr *prev;
    do {
        if (list->count == ARGC_MAX) {
            limit_error(lex, what, ARGC_MAX);
        }
        link_expr(list, &prev, expression0(lex));
    } while (test_next(lex, ','));
    return list;
}

static AstExpr *item_expr(Lex *lex)
{
    AstExpr *r = new_expr(lex, EXPR_ITEM);
    r->item.name = parse_name(lex);
    check_next(lex, ':');
    r->item.value = expression0(lex);
    return r;
}

static AstExprList *item_list0(Lex *lex, const char *what)
{
    AstExprList *list = new_expr_list(lex);
    AstExpr *prev;
    do {
        if (test(lex, '}')) {
            break;
        } else if (list->count== LOCAL_MAX) {
            limit_error(lex, what, LOCAL_MAX);
        }
        AstExpr *next = item_expr(lex);
        link_expr(list, &prev, next);
    } while (test_next(lex, ','));
    return list;
}

static AstExprList *arguments(Lex *lex)
{
    const int line = lex->line;
    skip(lex); // '(' token

    if (test_next(lex, ')')) {
        return new_expr_list(lex); // empty
    }
    AstExprList *list = expr_list1(lex, "function parameters");
    delim_next(lex, ')', '(', line);
    return list;
}

// Parse an identifier
static AstExpr *name_expr(Lex *lex)
{
    AstExpr *r = new_expr(lex, EXPR_NAME);
    r->name.name = parse_name(lex);
    return r;
}

Scope *pawP_new_scope(Lex *lex, SymbolTable *table)
{
    if (table->nscopes == UINT16_MAX) {
        limit_error(lex, "scopes", UINT16_MAX);
    }
    pawM_grow(env(lex), table->scopes, table->nscopes, table->capacity);
    Scope *scope = pawM_new(env(lex), Scope);
    table->scopes[table->nscopes++] = scope;
    return scope;
}

void pawP_add_scope(Lex *lex, SymbolTable *table, Scope *scope)
{
    if (table->nscopes == UINT16_MAX) {
        limit_error(lex, "scopes", UINT16_MAX);
    }
    pawM_grow(env(lex), table->scopes, table->nscopes, table->capacity);
    table->scopes[table->nscopes++] = scope;
}

Symbol *pawP_add_symbol(Lex *lex, Scope *table)
{
    pawM_grow(env(lex), table->symbols, table->nsymbols, table->capacity);
    Symbol *sym = pawA_new_symbol(lex);
    table->symbols[table->nsymbols++] = sym;
    return sym;
}

int pawP_find_symbol(Scope *scope, const String *name)
{
    Symbol **symbols = scope->symbols;
    for (int i = scope->nsymbols - 1; i >= 0; --i) {
        if (pawS_eq(name, symbols[i]->name)) {
            if (symbols[i]->is_init) {
                return i;
            }
        }
    }
    return -1;
}

static AstExpr *unop_expr(Lex *lex, UnOp op)
{
    AstExpr *result = new_expr(lex, EXPR_UNOP);
    UnOpExpr *r = &result->unop;
    skip(lex); // unary operator token
    r->op = (UnaryOp)op; // same order
    r->target = expression(lex, kUnOpPrecedence);
    return result;
}

// Parse either a parenthsized expression or a tuple
static AstExpr *paren_expr(Lex *lex)
{
    // Just parse and return the expression contained within the parenthesis.
    // There is no need for an extra node type.
    const int line = lex->line;
    AstExpr *elems = NULL;
    int nelems = 0;
    skip(lex); // '(' token
    if (!test(lex, ')')) {
        ++lex->expr_depth;
        elems = expression0(lex);
        --lex->expr_depth;
        if (test_next(lex, ')')) {
            return elems;
        }
    }
    pawX_error(lex, "TODO: tuples are not yet implemented");
    return NULL;

//    if (!test(lex, ')')) {
//        elems = expression0(lex);
//        if (test_next(lex, ')')) {
//            // Found a normal parenthesized exprssion: "(" AstExpr ")".
//            return elems; 
//        }
//    //    // Expect an n-tuple (1-tuple requires a trailing ',').
//    //    while (test_next(lex, ',')) {
//    //        if (nelems > UINT8_MAX) {
//    //            limit_error(lex, "tuple elements", UINT8_MAX);
//    //        }
//    //        elems->hdr.next = expression0(lex); 
//    //        elems = elems->hdr.next;
//    //        ++nelems;
//    //    }
//    }
//    --lex->expr_depth;
//    AstExpr *result = new_expr(lex, EXPR_LITERAL);
//    LiteralExpr *lit = &result->literal;
//    TupleLit *r = &lit->tuple;
//    lit->line = line;
//    r->elems = elems;
//    r->nelems = nelems;
//    delim_next(lex, ')', '(', line);
//    return result;
}

static paw_Bool end_of_block(Lex *lex)
{
    return test(lex, '}') || // found end of block
           test(lex, TK_END); // truncated block
}

static AstStmtList *stmt_list(Lex *lex)
{
    AstStmtList *list = new_stmt_list(lex);
    AstStmt *last;
    while (!end_of_block(lex)) {
        AstStmt *next = statement(lex);
        if (next != NULL) {
            link_stmt(list, &last, next);
            if (a_kind(next) == STMT_RETURN || // 'return'
                a_kind(next) == STMT_LABEL) { // 'break' | 'continue'
                break; // must be last statement in block
            }
        }
    }
    return list;
}

static AstExpr *array_expr(Lex *lex)
{
//    AstExpr *result = new_expr(lex, EXPR_ARRAY);
//    ArrayExpr *r = &result->array;
//    const int line = lex->line;
//    skip(lex); // '[' token
//
//    int nitems = 0;
//    AstExpr *items = r->items;
//    do {
//        if (test(lex, ']')) {
//            break;
//        } else if (nitems == LOCAL_MAX) {
//            limit_error(lex, "array elements", LOCAL_MAX);
//        }
//        items->next = expression0(lex);
//        items = items->next;
//        ++nitems;
//    } while (test_next(lex, ','));
//    delim_next(lex, ']', '[', line);
//    return result;
}

static AstExpr *map_expr(Lex *lex)
{
//    const int line = lex->line;
//    skip(lex); // '{' token
//    AstExpr *result = new_expr(lex, EXPR_MAP);
//    MapExpr *r = &result->map;
//
//    NodeVec *items = &r->items;
//    do {
//        if (test(lex, '}')) {
//            break;
//        } else if (items->size > LOCAL_MAX - 2) {
//            limit_error(lex, "map items", LOCAL_MAX);
//        }
//        push_node(lex, items, expression0(lex));
//        check_next(lex, ':');
//        push_node(lex, items, expression0(lex));
//    } while (test_next(lex, ','));
//    delim_next(lex, '}', '{', line);
//    return result;
    return NULL;
}

static AstExpr *index_expr(Lex *lex, AstExpr *target)
{
    AstExpr *result = new_expr(lex, EXPR_INDEX);
    Index *r = &result->index;
    const int line = lex->line;
    skip(lex); // '[' token

    r->target = target;
    r->elems = expr_list1(lex, "bracketed terms");
    delim_next(lex, ']', '[', line);
    return result;
}

// Parse a composite literal expression
static AstExpr *composite_lit(Lex *lex, AstExpr *target)
{
    AstExpr *result = new_expr(lex, EXPR_LITERAL);
    LiteralExpr *lit = &result->literal;
    lit->lit_kind = LIT_COMPOSITE;
    CompositeLit *r = &lit->comp;
    skip(lex); // '{' token
    r->items = item_list0(lex, "items");
    r->target = target;
    delim_next(lex, '}', '{', lit->line);
    return result;
}

static AstExpr *selector_expr(Lex *lex, AstExpr *target)
{
    skip(lex); // '.' token
    AstExpr *r = new_expr(lex, EXPR_SELECTOR);
    r->selector.target = target;
    r->selector.name = parse_name(lex);
    return r;
}

static AstExpr *access_expr(Lex *lex, AstExpr *target)
{
    skip(lex); // '::' token
    AstExpr *r = new_expr(lex, EXPR_ACCESS);
    r->access.target = target;
    r->access.name = parse_name(lex);
    return r;
}

static AstExpr *call_expr(Lex *lex, AstExpr *target)
{
    AstExpr *result = new_expr(lex, EXPR_CALL);
    CallExpr *r = &result->call;
    r->args = arguments(lex);
    r->target = target;
    return result;
}

static AstExpr *chain_expr(Lex *lex, AstExpr *target)
{
    AstExpr *result = new_expr(lex, EXPR_UNOP);
    ChainExpr *r = &result->chain;
    r->target = target;
    skip(lex); // '?' token
    return result;
}

static AstDeclList *parameters(Lex *lex)
{
    AstDeclList *list = new_decl_list(lex);
    const int line = lex->line;
    check_next(lex, '(');

    if (!test_next(lex, ')')) {
        AstDecl *prev;
        do {
            if (list->count == ARGC_MAX) {
                limit_error(lex, "function parameters", ARGC_MAX);
            } else if (!test(lex, TK_NAME)) {
                expected_symbol(lex, "name");
            }
            // parse function parameter of form 'name: type'
            AstDecl *next = param_decl(lex);
            link_decl(list, &prev, next);
        } while (test_next(lex, ','));
        delim_next(lex, ')', '(', line);
    }
    return list;
}

static Block *block(Lex *lex)
{
    const int line = lex->line;
    AstStmt *result = new_stmt(lex, STMT_BLOCK);
    Block *r = &result->block;
    check_next(lex, '{');
    r->stmts = stmt_list(lex);
    delim_next(lex, '}', '{', line);
    return r;
}

static AstExpr *primary_expr(Lex *lex)
{
    switch (lex->t.kind) {
        case '(':
            return paren_expr(lex);
        case TK_NAME:
            return name_expr(lex);
        case TK_STRING: {
            const Value v = lex->t.value;
            skip(lex); // string token
            return new_basic_lit(lex, v, PAW_TSTRING);
        }
        //case '[':
        //    return array_expr(lex);
        //case '{':
        //    return map_expr(lex);
        case TK_FN:
            return type_expr(lex);
        default:
            expected_symbol(lex, "name or '('");
            return NULL; // never run
    }
}

static AstExpr *suffixed_expr(Lex *lex)
{
    AstExpr *e = primary_expr(lex);
    for (;;) { // parse suffix chain
        switch (lex->t.kind) {
            case '?':
                e = chain_expr(lex, e);
                break;
            case '(':
                e = call_expr(lex, e);
                break;
            case '.':
                e = selector_expr(lex, e);
                break;
            case TK_COLON2:
                e = access_expr(lex, e);
                break;
            case '[':
                e = index_expr(lex, e);
                break;
            case '{':
                switch (a_kind(e)) {
                    case EXPR_NAME:
                    case EXPR_INDEX:
                    case EXPR_ACCESS:
                        if (lex->expr_depth < 0) {
                            return e;
                        }
                        break;
                    default:
                        return e;
                }
                e = composite_lit(lex, e);
                break;
            default:
                return e;
        }
    }
}

static AstExpr *simple_expr(Lex *lex)
{
    AstExpr *expr;
    switch (lex->t.kind) {
        case TK_TRUE:
            expr = emit_bool(lex, PAW_TRUE);
            break;
        case TK_FALSE:
            expr = emit_bool(lex, PAW_FALSE);
            break;
        case TK_INTEGER:
            expr = new_basic_lit(lex, lex->t.value, PAW_TINT);
            break;
        case TK_FLOAT:
            expr = new_basic_lit(lex, lex->t.value, PAW_TFLOAT);
            break;
        default:
            return suffixed_expr(lex);
    }
    skip(lex); // skip literal
    return expr;
}

static AstExpr *binop_expr(Lex *lex, InfixOp op, AstExpr *lhs)
{
    skip(lex); // binary operator token
    AstExpr *rhs = expression(lex, right_prec(op));
    if (rhs == NULL) {
        return NULL; // no more binops
    }
    AstExpr *result = new_expr(lex, EXPR_BINOP);
    BinOpExpr *r = &result->binop;
    r->op = (BinaryOp)op; // same order
    r->lhs = lhs;
    r->rhs = rhs;
    return result;
}

static AstExpr *logical_expr(Lex *lex, AstExpr *lhs, paw_Bool is_and)
{
    skip(lex); // '&&' or '||' token
    AstExpr *rhs = expression(lex, right_prec(INFIX_AND));
    if (rhs == NULL) {
        return NULL; // no more binops
    }
    AstExpr *result = new_expr(lex, EXPR_LOGICAL);
    LogicalExpr *r = &result->logical;
    r->is_and = is_and;
    r->lhs = lhs;
    r->rhs = rhs;
    return result;
}

static AstExpr *cond_expr(Lex *lex, AstExpr *lhs)
{
    skip(lex); // '??' token
    AstExpr *result = new_expr(lex, EXPR_COND);
    CondExpr *r = &result->cond;
    r->cond = lhs;
    r->lhs = expression(lex, right_prec(INFIX_COND));
    check_next(lex, TK_COLON2);
    r->rhs = expression0(lex);
    return result;
}

static AstExpr *infix_expr(Lex *lex, AstExpr *lhs, unsigned op)
{
    switch (op) {
        case INFIX_AND:
        case INFIX_OR:
            return logical_expr(lex, lhs, op == INFIX_AND);
        case INFIX_COND:
            return cond_expr(lex, lhs);
        default:
            return binop_expr(lex, op, lhs);
    }
}

static AstExpr *subexpr(Lex *lex, unsigned prec)
{
    unsigned op = get_unop(lex->t.kind);
    AstExpr *expr = op == NOT_UNOP
                     ? simple_expr(lex)
                     : unop_expr(lex, op);

    op = get_infixop(lex->t.kind);
    while (op != NOT_INFIX && prec < left_prec(op)) {
        expr = infix_expr(lex, expr, op);
        op = get_infixop(lex->t.kind);
    }
    return expr;
}

// TODO
static AstExpr *expression(Lex *lex, unsigned prec)
{
    return subexpr(lex, prec);
}

static AstExpr *basic_expr(Lex *lex)
{
    const int prev_depth = lex->expr_depth;
    lex->expr_depth = -1;
    AstExpr *expr = subexpr(lex, 0);
    lex->expr_depth = prev_depth;
    return expr;
}

static AstStmt *if_stmt(Lex *lex)
{
    skip(lex); // 'if' token
    AstStmt *result = new_stmt(lex, STMT_IF);
    IfStmt *r = &result->if_;
    r->cond = basic_expr(lex); // conditional
    r->then_arm = cast_stmt(block(lex)); // 'then' block

    if (test_next(lex, TK_ELSE)) {
        if (test(lex, TK_IF)) {
            // Put the rest of the chain in the else branch. This transformation looks
            // like 'if a {} else if b {} else {}' -> 'if a {} else {if b {} else {}}'.
            r->else_arm = if_stmt(lex);
        } else {
            r->else_arm = cast_stmt(block(lex));
        }
    }
    return result;
}

static AstStmt *expr_stmt(Lex *lex)
{
    AstStmt *result = new_stmt(lex, STMT_EXPR);
    AstExprStmt *r = &result->expr;
    r->lhs = suffixed_expr(lex);
    if (test_next(lex, '=')) {
        r->rhs = expression0(lex);
    }
    semicolon(lex);
    return result;
}

static AstStmt *fornum(Lex *lex, String *ivar)
{
    AstStmt *result = new_stmt(lex, STMT_FORNUM);
    ForStmt *r = &result->for_;
    ForNum *fornum = &r->fornum;
    r->name = ivar;

    // Parse the loop bounds ('begin', 'end', and 'step' expressions).
    fornum->begin = basic_expr(lex);
    check_next(lex, ',');
    fornum->end = basic_expr(lex);
    if (test_next(lex, ',')) {
        fornum->step = basic_expr(lex);
    } else {
        Value v;
        v_set_int(&v, 1); // step defaults to 1
        fornum->step = new_basic_lit(lex, v, PAW_TINT);
    }

    r->block = block(lex);
    return result;
}

static AstStmt *forin(Lex *lex, String *ivar)
{
    AstStmt *result = new_stmt(lex, STMT_FORIN);
    ForStmt *r = &result->for_;
    ForIn *forin = &r->forin;

    forin->target = basic_expr(lex);
    r->name = ivar;
    r->block = block(lex);
    return result;
}

static AstStmt *for_stmt(Lex *lex)
{
    skip(lex); // 'for' token
    String *ivar = parse_name(lex); // loop variable
    if (test_next(lex, '=')) {
        return fornum(lex, ivar);
    } else if (!test_next(lex, TK_IN)) {
        expected_symbol(lex, "'=' or 'in'"); // no return
    }
    AstStmt *stmt = forin(lex, ivar);
    return stmt;
}

static AstStmt *while_stmt(Lex *lex)
{
    AstStmt *result = new_stmt(lex, STMT_WHILE);
    WhileStmt *r = &result->while_;
    skip(lex); // 'while' token

    r->cond = basic_expr(lex);
    r->block = block(lex);
    return result;
}

static AstStmt *dowhile_stmt(Lex *lex)
{
    skip(lex); // 'do' token
    AstStmt *r = new_stmt(lex, STMT_DOWHILE);
    r->while_.block = block(lex);
    check_next(lex, TK_WHILE);
    r->while_.cond = basic_expr(lex);
    return r;
}

static AstStmt *return_stmt(Lex *lex)
{
    AstStmt *result = new_stmt(lex, STMT_RETURN);
    ReturnStmt *r = &result->return_;
    skip(lex); // 'return' token
    if (end_of_block(lex) || test(lex, ';')) {
        r->expr = NULL;
    } else {
        r->expr = expression0(lex);
    }
    // NOTE: The construct 'return [expr] [`;`]' must be followed by the TODO: auto semicolon insertion fixes this
    //       end of the block. This is necessary because the semicolon is      could relax restriction, make it easier to 'prototype' in paw
    //       optional, and we cannot easily tell if it was intended to go
    //       before or after the '[expr]' part.
    semicolon(lex);
    return result;
}

static AstStmt *label_stmt(Lex *lex, LabelKind kind)
{
    AstStmt *result = new_stmt(lex, STMT_LABEL);
    LabelStmt *r = &result->label;
    r->label = kind;
    skip(lex); // 'break' or 'continue' token
    semicolon(lex);
    return result;
}

static AstDeclList *maybe_type_param(Lex *lex)
{
    const int line = lex->line;
    if (!test_next(lex, '[')) { 
        return NULL;
    } else if (test_next(lex, ']')) {
        pawX_error(lex, "empty generic parameters");
    }

    AstDecl *prev;
    AstDeclList *list = new_decl_list(lex);
    ++lex->expr_depth;
    do {
        if (list->count == ARGC_MAX) {
            limit_error(lex, "generic type parameters", ARGC_MAX);
        }
        AstDecl *r = new_decl(lex, DECL_GENERIC);
        r->generic.name = parse_name(lex);
        link_decl(list, &prev, r);
    } while (test_next(lex, ','));
    delim_next(lex, ']', '[', line);
    --lex->expr_depth;
    return list;
}

static AstDecl *function(Lex *lex, String *name, FuncKind kind)
{
    AstDecl *d = new_decl(lex, DECL_FUNC);
    d->func.name = name;
    d->func.fn_kind = kind;
    d->func.generics = maybe_type_param(lex);
    d->func.params = parameters(lex);
    d->func.return_ = ret_annotation(lex);
    d->func.is_poly = d->func.generics != NULL;
    d->func.body = block(lex);
    return d;
}

static AstDecl *func_decl(Lex *lex, int line, paw_Bool global)
{
    skip(lex); // 'fn' token
    String *name = parse_name(lex);
    AstDecl *r = function(lex, name, FUNC_FUNCTION);
    r->func.is_global = global;
    r->func.line = line;
    return r;
}

static AstDecl *field_decl(Lex *lex, String *name)
{
    AstDecl *r = new_decl(lex, DECL_FIELD);
    r->field.name = name;
    r->field.tag = var_annotation(lex);
    semicolon(lex);
    return r;
}

static AstDecl *method_decl(Lex *lex, String *name)
{
    AstDecl *r = function(lex, name, FUNC_METHOD);
    semicolon(lex);
    return r;
}

static AstDecl *attr_decl(Lex *lex)
{
    String *name = v_string(lex->t.value);
    skip(lex); // name token
    if (test(lex, ':')) {
        return field_decl(lex, name);
    } else {
        return method_decl(lex, name);
    }
}

static void struct_body(Lex *lex, StructDecl *struct_)
{
    AstDecl *last_field;
    AstDecl *last_method;
    const int line = lex->line;
    check_next(lex, '{');

    struct_->fields = new_decl_list(lex);
    struct_->methods = new_decl_list(lex);
    while (!test(lex, '}')) {
        check(lex, TK_NAME);
        AstDecl *next = attr_decl(lex);
        if (a_kind(next) == DECL_FUNC) {
            if (struct_->methods->count == LOCAL_MAX) {
                limit_error(lex, "methods", LOCAL_MAX);
            }
            // use the method chain, 'next' link used for template instances
            link_method(struct_->methods, &last_method, next);
        } else if (struct_->fields->count == LOCAL_MAX) {
            limit_error(lex, "fields", LOCAL_MAX);
        } else {
            link_decl(struct_->fields, &last_field, next);
        }
    }
    delim_next(lex, '}', '{', line);
}

static AstDecl *struct_decl(Lex *lex, paw_Bool global)
{
    skip(lex); // 'struct' token
    AstDecl *r = new_decl(lex, DECL_STRUCT);
    r->struct_.is_global = global;
    r->struct_.name = parse_name(lex);
    r->struct_.generics = maybe_type_param(lex);
    r->struct_.is_poly = r->struct_.generics != NULL;
    struct_body(lex, &r->struct_);
    semicolon(lex);
    return r;
}

static AstDecl *type_decl(Lex *lex)
{
    AstDecl *r = new_decl(lex, DECL_TYPE);
    skip(lex); // 'type' token

    r->type.name = parse_name(lex);
    r->type.generics = maybe_type_param(lex);

    check_next(lex, '=');

    // 'type_expr()' parses function signatures, which are not allowed
    // on the RHS of a type expression. This should be caught during
    // type checking, since we also need to make sure the RHS is not
    // referring to an uninstantiated template.
    r->type.rhs = type_expr(lex);
    semicolon(lex);
    return r;
}

static AstDecl *global_decl(Lex *lex)
{
    const int line = lex->line;
    skip(lex); // 'global' token
    if (test(lex, TK_FN)) {
        skip(lex); // 'fn' token
        return func_decl(lex, line, PAW_TRUE);
    } else if (test(lex, TK_STRUCT)) {
        return struct_decl(lex, PAW_TRUE);
    } else {
        return var_decl(lex, line, PAW_TRUE);
    }
}

static AstDecl *decl(Lex *lex)
{
    switch (lex->t.kind) {
        case TK_FN:
            return func_decl(lex, lex->line, PAW_FALSE);
        case TK_STRUCT:
            return struct_decl(lex, PAW_FALSE);
        case TK_TYPE:
            return type_decl(lex);
        case TK_LET:
            skip(lex); // 'let' token
            return var_decl(lex, lex->line, PAW_FALSE);
        default:
            paw_assert(lex->t.kind == TK_GLOBAL);
            return global_decl(lex);
    }
}

static AstStmt *decl_stmt(Lex *lex)
{
    AstStmt *r = new_stmt(lex, STMT_DECL);
    r->decl.decl = decl(lex);
    return r;
}

static AstStmt *statement(Lex *lex)
{
    switch (lex->t.kind) {
        case ';':
            // empty statement
            skip(lex); // ';' token
            return NULL;
        case '{':
            return cast_stmt(block(lex));
        case TK_IF:
            return if_stmt(lex);
        case TK_FOR:
            return for_stmt(lex);
        case TK_WHILE:
            return while_stmt(lex);
        case TK_DO:
            return dowhile_stmt(lex);
        case TK_RETURN:
            return return_stmt(lex);
        case TK_BREAK:
            return label_stmt(lex, LBREAK);
        case TK_CONTINUE:
            return label_stmt(lex, LCONTINUE);
        case TK_FN:
        case TK_STRUCT:
        case TK_LET:
        case TK_GLOBAL:
        case TK_TYPE:
            return decl_stmt(lex);
        default:
            return expr_stmt(lex);
    }
}

// All paw language keywords
//
// ORDER TokenKind
static const char *kKeywords[] = {
    "fn",
    "type",
    "struct",
    "global",
    "let",
    "if",
    "else",
    "for",
    "do",
    "while",
    "break",
    "continue",
    "return",
    "in",
    "true",
    "false",
};


static String *basic_type_name(paw_Env *P, const char *name, paw_Type type)
{
    String *s = pawS_new_fixed(P, name);
    s->flag = -type - 1; // flag < 0 to distinguish from keywords
    return s;
}

void pawP_init(paw_Env *P)
{
    // Add all keywords to the interned strings table. Fix them so they are never
    // collected. Also added to the lexer string map.
    for (size_t i = 0; i < paw_countof(kKeywords); ++i) {
        const char *kw = kKeywords[i];
        String *str = pawS_new_fixed(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }
    for (Metamethod mm = 0; mm < NMETAMETHODS; ++mm) {
        const char *name = pawT_name(mm);
        String *str = pawS_new_fixed(P, name);
        v_set_object(&P->meta_keys[mm], str);
    }
    P->str_cache[CSTR_SELF] = pawS_new_str(P, "self");
    P->str_cache[CSTR_TRUE] = pawS_new_str(P, "true");
    P->str_cache[CSTR_FALSE] = pawS_new_str(P, "false");
    P->str_cache[CSTR_UNIT] = basic_type_name(P, "()", PAW_TUNIT);
    P->str_cache[CSTR_BOOL] = basic_type_name(P, "bool", PAW_TBOOL);
    P->str_cache[CSTR_INT] = basic_type_name(P, "int", PAW_TINT);
    P->str_cache[CSTR_FLOAT] = basic_type_name(P, "float", PAW_TFLOAT);
    P->str_cache[CSTR_STRING] = basic_type_name(P, "string", PAW_TSTRING);
}

static void skip_hashbang(Lex *lex)
{
    if (test_next(lex, '#') && test_next(lex, '!')) {
        char c;
        do {
            c = lex->c;
            skip(lex);
        } while (!ISNEWLINE(c));
    }
}

static Ast *parse_module(Lex *lex)
{
    Ast *ast = lex->pm->ast;
    skip_hashbang(lex);
    ast->stmts = stmt_list(lex);
    check(lex, TK_END);
    return ast;
}

Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *pm, const char *name, void *ud)
{
    // Initialize the lexical state.
    Lex lex = {
        .pm = pm,
        .ud = ud,
        .P = P,
    };
    pm->unifier.lex = &lex;
    pm->ast = pawA_new_ast(&lex);
    pm->symbols.globals = pawM_new(P, Scope);
    pawX_set_source(&lex, input);

    // TODO: AST needs to go on the stack in a Foreign object. We will leak the
    //       AST if an error is thrown between now and when it is freed below.
    //       Use a flag stored somewhere in the object to disambiguate between
    //       other Foreign objects and ASTs, call pawK_free_ast() on ASTs from
    //       the GC routine.

    // Create the main closure and push it onto the stack so that the garbage
    // collector can find it.
    Value *pv = pawC_push0(P);
    lex.main = pawV_new_closure(P, 1);
    v_set_object(pv, lex.main);
    Proto *f = pawV_new_proto(P);
    lex.main->p = f;

    // Do the same for the lexer string map. Strings are reachable from this map
    // during the parse. Once the parse is finished, all strings should be
    // anchored somewhere.
    pv = pawC_push0(P);
    lex.strings = pawH_new(P);
    v_set_object(pv, lex.strings);

    // Store the module name.
    String *modname = pawS_new_str(P, name);
    lex.modname = modname;
    f->modname = modname;
    f->name = modname;

    // Load the first token.
    skip(&lex);

    void p_check_types(Lex *lex); // from check.c
    void p_generate_code(Lex *lex); // from codegen.c

    // Compile the module.
    parse_module(&lex); // pass 1 (source -> AST)
    p_check_types(&lex); // pass 2 (AST -> graph)
                               
    pawA_dump_stmt(stdout, lex.pm->ast->stmts->first); // TODO: remove
                               
    p_generate_code(&lex); // pass 2 (graph -> bytecode)

    // Pop the lexer map. The strings it contains should be anchored elsewhere.
    // Leave the main closure on top of the stack.
    pawC_stkdec(P, 1);
    return lex.main;
}
