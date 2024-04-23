// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "parse.h"
#include "auxlib.h"
#include "call.h"
#include "code.h"
#include "check.h"
#include "env.h"
#include "gc.h"
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

// recursive non-terminals
static Expr *subexpr(Lex *lex, unsigned prec);
static Stmt *stmt(Lex *lex);
#define expr0(x) subexpr(x, 0)

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
        case TK_ELVIS:
            return INFIX_COALESCE;
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

static Expr *emit_primitive(Lex *lex, Value v, int type)
{
    PrimitiveExpr *expr = pawK_add_node(lex, EXPR_PRIMITIVE, PrimitiveExpr);
    expr->t = type;
    expr->v = v;
    return cast_expr(expr);
}

static Expr *emit_bool(Lex *lex, paw_Bool b)
{
    Value v;
    v_set_bool(&v, b);
    return emit_primitive(lex, v, PAW_TBOOL);
}

#define make_link_node(suffix, T) \
    static void link_ ## suffix(T **phead, T **plast, T *next) \
    { \
        if (*phead == NULL) { \
            *phead = next; \
        } else { \
            (*plast)->next = next; \
        } \
        *plast = next; \
    }
make_link_node(stmt, Stmt)
make_link_node(expr, Expr)

// letstmt := storage [const] name [`:` Type] `=` expr
//
//global x: int = 2
//let const x: int = 3
//global const x: int = 4
//
// storage := let | global
//
// Type := TBasic | TArray | TVector | TEnum | TFunction | TClass
//
// TArray := `[` Type `;` Length `]`
//
// TVector := `[` Type `]`
//
// TODO: TGenerics should not appear in type annotation for variable
// TGenerics := `[` name {`,` name} `]`
//
// TArgs := `(` [Type {`,` Type}] `)`
//
// TFunction := `fn` [TGenerics] TArgs [ `:` Type ]
//
// TClass :=  name [TGenerics]
//
// TBasic := bool | int | float | string
//
static TypeDecl *type_decl(Lex *lex);

static void parse_signature(Lex *lex, TypeDecl *pdecl)
{
    pdecl->group = TYPE_SIGNATURE;
    skip(lex); // '(' token

    // function parameters
    const int line = lex->lastline;
    if (!test_next(lex, ')')) {
        Expr **phead = &pdecl->sig.args;
        Expr *last;
        int nargs = 0;
        do {
            if (nargs == ARGC_MAX) {
                limit_error(lex, "function type parameters", ARGC_MAX);
            }
            check(lex, TK_NAME);
            TypeDecl *arg = type_decl(lex);
            link_expr(phead, &last, cast_expr(arg));
            ++nargs;
        } while (test_next(lex, ','));
        delim_next(lex, ')', '(', line);
        pdecl->sig.nargs = nargs;
    }

    // return type annotation
    if (test_next(lex, TK_ARROW)) {
        pdecl->sig.ret = type_decl(lex);
    }
}

static void parse_typename(Lex *lex, TypeDecl *pdecl)
{
    String *name = parse_name(lex);
    if (name->flag < 0) { // found primitive type
        const int code = -name->flag - 1;
        pdecl->group = TYPE_PRIMITIVE;
        pdecl->basic.t = code;
    } else { // found class type
        pdecl->named.name = name;
        pdecl->group = TYPE_CLASS;
    }
}

static TypeDecl *type_decl(Lex *lex)
{
    TypeDecl *decl = pawK_add_node(lex, EXPR_TYPE, TypeDecl);
    if (test_next(lex, TK_FN)) {
        parse_signature(lex, decl); 
    } else {
        parse_typename(lex, decl);
    }
    return decl;
}

static TypeDecl *ret_annotation(Lex *lex)
{
    return test_next(lex, TK_ARROW) ? type_decl(lex) : NULL;
}

static TypeDecl *var_annotation(Lex *lex)
{
    if (test_next(lex, ':')) {
        TypeDecl *tn = type_decl(lex);
        if (tn == NULL) {
            pawX_error(lex, "invalid type annotation");
        }
        return tn;
    }
    return NULL; // needs inference
}

static Stmt *parameter_def(Lex *lex)
{
    ParamStmt *result = pawK_add_node(lex, STMT_PARAM, ParamStmt);
    result->name = parse_name(lex);
    result->tag = var_annotation(lex);
    if (result->tag == NULL) {
        pawX_error(lex, "expected type annotation on parameter '%s'", 
                   result->name->text);
    }
    return cast_stmt(result);
}

static Stmt *variable_def(Lex *lex, int line, paw_Bool global)
{
    DefStmt *result = pawK_add_node(lex, STMT_DEF, DefStmt);
    result->line = line; // line containing 'global' or 'let'
    result->name = parse_name(lex);
    result->tag = var_annotation(lex);
    if (!test_next(lex, '=')) {
        pawX_error(lex, "missing initializer");
    }
    result->init = expr0(lex);
    result->flags.global = global;
    semicolon(lex);
    return cast_stmt(result);
}

static int call_parameters(Lex *lex, Expr **phead)
{
    const int line = lex->line;
    skip(lex); // '(' token

    Expr *last;
    int nargs = 0;
    if (!test_next(lex, ')')) {
        link_expr(phead, &last, expr0(lex));
        nargs = 1;
        while (test_next(lex, ',')) {
            if (nargs == ARGC_MAX) {
                limit_error(lex, "function parameters", ARGC_MAX);
            }
            link_expr(phead, &last, expr0(lex));
            ++nargs;
        }
        delim_next(lex, ')', '(', line);
    }
    return nargs;
}

// Parse a variable name
static Expr *varexpr(Lex *lex)
{
    VarExpr *result = pawK_add_node(lex, EXPR_VAR, VarExpr);
    result->name = parse_name(lex);
    return cast_expr(result);
}

Scope *pawP_add_scope(Lex *lex, SymbolTable *table)
{
    if (table->nscopes == UINT16_MAX) {
        limit_error(lex, "scopes", UINT16_MAX);
    }
    pawM_grow(env(lex), table->scopes, table->nscopes, table->capacity);
    Scope *scope = pawM_new(env(lex), Scope);
    table->scopes[table->nscopes++] = scope;
    return scope;
}

Symbol *pawP_add_symbol(Lex *lex, Scope *table)
{
    pawM_grow(env(lex), table->symbols, table->nsymbols, table->capacity);
    Symbol *sym = pawK_add_node(lex, EXPR_SYMBOL, Symbol);
    table->symbols[table->nsymbols++] = sym;
    return sym;
}

int pawP_find_symbol(Scope *scope, const String *name)
{
    Symbol **symbols = scope->symbols;
    for (int i = scope->nsymbols - 1; i >= 0; --i) {
        if (pawS_eq(name, symbols[i]->name)) {
            return i;
        }
    }
    return -1;
}

// static void code_invoke(Lex *lex, Op op, ExprState *e)
//{
//     const int name = add_name(lex, e->s);
//     discard(e);
//
//     const int argc = call_parameters(lex);
//     pawK_code_AB(lex->fs, op, name, argc);
// }
//
// static void push_special(Lex *lex, unsigned ctag)
//{
//     ExprState e;
//     if (ctag == CSTR_SELF) {
//         // 'self' is always in slot 0
//         init_expr(&e, EXPR_LOCAL, 0);
//     } else {
//         // 'super' is an upvalue
//         const Value v = pawE_cstr(lex->P, ctag);
//         find_var(lex, &e, v_string(v));
//     }
//     discharge(&e);
// }

////static void superexpr(Lex *lex, ExprState *e)
////{
////    if (!lex->cls) {
////        pawX_error(lex, "'super' used outside class body");
////    } else if (!lex->cls->has_super) {
////        pawX_error(lex, "class has no superclass");
////    }
////
////    skip(lex); // 'super' token
////    check_next(lex, '.');
////
////    const int name = parse_name(lex, e);
////    if (test(lex, '(')) {
////        const int argc = call_parameters(lex);
////        push_special(lex, CSTR_SELF);
////        push_special(lex, CSTR_SUPER);
////        pawK_code_AB(lex->fs, OP_INVOKESUPER, name, argc);
////        e->kind = EXPR_CALL;
////    } else {
////        push_special(lex, CSTR_SELF);
////        push_special(lex, CSTR_SUPER);
////        pawK_code_U(lex->fs, OP_GETSUPER, name);
////        e->kind = EXPR_ACCESS;
////    }
////}

static Expr *unop_expr(Lex *lex, UnOp op)
{
    UnOpExpr *result = pawK_add_node(lex, EXPR_UNOP, UnOpExpr);
    skip(lex); // unary operator token
    result->op = (UnaryOp)op; // same order
    result->target = subexpr(lex, kUnOpPrecedence);
    return cast_expr(result);
}

static Expr *paren_expr(Lex *lex)
{
    // Just parse and return the expression contained within the parenthesis.
    // There is no need for an extra node type.
    const int line = lex->line;
    skip(lex); // '(' token
    Expr *result = expr0(lex);
    delim_next(lex, ')', '(', line);
    return result;
}

static paw_Bool end_of_block(Lex *lex)
{
    return test(lex, '}') || // found end of block
           test(lex, TK_END); // truncated block
}

static int stmtlist(Lex *lex, Stmt **phead)
{
    Stmt *last;
    int nstmts = 0;
    while (!end_of_block(lex)) {
        const TokenKind tk = lex->t.kind;
        Stmt *next = stmt(lex);
        link_stmt(phead, &last, next);
        ++nstmts;
        if (tk == TK_RETURN) {
            break;
        }
    }
    return nstmts;
}

static Expr *array_expr(Lex *lex)
{
    ArrayExpr *result = pawK_add_node(lex, EXPR_ARRAY, ArrayExpr);
    const int line = lex->line;
    skip(lex); // '[' token

    int nitems = 0;
    Expr *items = result->items;
    do {
        if (test(lex, ']')) {
            break;
        } else if (nitems == LOCAL_MAX) {
            limit_error(lex, "array elements", LOCAL_MAX);
        }
        items->next = expr0(lex);
        items = items->next;
        ++nitems;
    } while (test_next(lex, ','));
    delim_next(lex, ']', '[', line);
    return cast_expr(result);
}

static Expr *map_expr(Lex *lex)
{
//    const int line = lex->line;
//    skip(lex); // '{' token
//    MapExpr *result = pawK_add_node(lex, EXPR_MAP, MapExpr);
//
//    NodeVec *items = &result->items;
//    do {
//        if (test(lex, '}')) {
//            break;
//        } else if (items->size > LOCAL_MAX - 2) {
//            limit_error(lex, "map items", LOCAL_MAX);
//        }
//        push_node(lex, items, expr0(lex));
//        check_next(lex, ':');
//        push_node(lex, items, expr0(lex));
//    } while (test_next(lex, ','));
//    delim_next(lex, '}', '{', line);
//    return cast_expr(result);
    return NULL;
}

static Expr *index_expr(Lex *lex, Expr *prefix)
{
    IndexExpr *result = pawK_add_node(lex, EXPR_INDEX, IndexExpr);
    const int line = lex->line;
    skip(lex); // '[' token

    result->target = prefix;
    if (test(lex, ':')) {
        result->first = NULL; // TODO: Won't work: use a special range object 
    } else {
        result->first = expr0(lex);
    }
    if (!test_next(lex, ':')) {
        result->second = NULL;
    } else if (test(lex, ']')) {
        result->second = NULL; 
    } else {
        result->second = expr0(lex); 
    }
    delim_next(lex, ']', '[', line);
    return cast_expr(result);
}

static Stmt *itemstmt(Lex *lex)
{
    check_next(lex, '.');
    ItemStmt *result = pawK_add_node(lex, STMT_ITEM, ItemStmt);
    result->name = parse_name(lex);
    check_next(lex, '=');
    result->value = expr0(lex);
    return cast_stmt(result);
}

static Expr *initexpr(Lex *lex, Expr *prefix)
{
    if (prefix->kind != EXPR_VAR) {
        pawX_error(lex, "expected class name");
    }
    InitExpr *result = pawK_add_node(lex, EXPR_INIT, InitExpr);
    result->prefix = prefix; 
    const int line = lex->line;
    skip(lex); // '{' token

    Stmt *last;
    do {
        if (test(lex, '}')) {
            break;
        } else if (result->nattrs == LOCAL_MAX) {
            limit_error(lex, "attributes", LOCAL_MAX);
        }
        Stmt *next = itemstmt(lex);
        link_stmt(&result->attrs, &last, next);
        ++result->nattrs;
    } while (test_next(lex, ','));
    delim_next(lex, '}', '{', line);
    return cast_expr(result);
}

static Expr *invokeexpr(Lex *lex, int line, Expr *prefix, String *name)
{
    InvokeExpr *result = pawK_add_node(lex, EXPR_INVOKE, InvokeExpr);
    result->nargs = call_parameters(lex, &result->args);
    result->target = prefix;
    result->line = line;
    result->name = name;
    return cast_expr(result);
}

static Expr *accessexpr(Lex *lex, int line, Expr *prefix, String *name)
{
    AccessExpr *result = pawK_add_node(lex, EXPR_ACCESS, AccessExpr);
    result->line = line;
    result->target = prefix;
    result->name = name;
    return cast_expr(result);
}

static Expr *dotexpr(Lex *lex, Expr *prefix)
{
    skip(lex); // '.' token
    const int line = lex->lastline;
    String *name = parse_name(lex);
    if (test(lex, '(')) {
        return invokeexpr(lex, line, prefix, name); 
    } else {
        return accessexpr(lex, line, prefix, name);
    }
}

static Expr *call_expr(Lex *lex, Expr *prefix)
{
    CallExpr *result = pawK_add_node(lex, EXPR_CALL, CallExpr);
    result->nargs = call_parameters(lex, &result->args);
    result->target = prefix;
    return cast_expr(result);
}

static Expr *chain_expr(Lex *lex, Expr *prefix)
{
    ChainExpr *result = pawK_add_node(lex, EXPR_UNOP, ChainExpr);
    result->target = prefix;
    skip(lex); // '?' token
    return cast_expr(result);
}

static int fn_parameters(Lex *lex, Stmt **phead)
{
    const int line = lex->line;
    check_next(lex, '(');

    int argc = 0;
    if (!test_next(lex, ')')) {
        Stmt *last;
        do {
            if (argc == ARGC_MAX) {
                limit_error(lex, "function parameters", ARGC_MAX);
            } else if (!test(lex, TK_NAME)) {
                expected_symbol(lex, "name");
            }
            // parse function parameter of form 'name: type'
            Stmt *next = parameter_def(lex);
            link_stmt(phead, &last, next);
            ++argc;
        } while (test_next(lex, ','));
        delim_next(lex, ')', '(', line);
    }
    return argc;
}

static Block *block(Lex *lex)
{
    const int line = lex->line;
    Block *result = pawK_add_node(lex, STMT_BLOCK, Block);
    check_next(lex, '{');
    result->nstmts = stmtlist(lex, &result->stmts);
    delim_next(lex, '}', '{', line);
    return result;
}

static Expr *primary_expr(Lex *lex)
{
    switch (lex->t.kind) {
        case '(':
            return paren_expr(lex);
        case TK_NAME:
            return varexpr(lex);
        case TK_STRING: {
            const Value v = lex->t.value;
            skip(lex);
            return emit_primitive(lex, v, PAW_TSTRING);
        }
        case '[':
            return array_expr(lex);
        case '{':
            return map_expr(lex);
        //case TK_SUPER:
        //    return superexpr(lex, e);
        default:
            expected_symbol(lex, "name or '('");
            return NULL; // never run
    }
}

static Expr *suffixed_expr(Lex *lex)
{
    Expr *e = primary_expr(lex);
    if (test(lex, '{')) {
        e = initexpr(lex, e);
    }
    for (;;) { // parse suffix chain
        switch (lex->t.kind) {
            case '(':
                e = call_expr(lex, e);
                break;
            case '.':
                e = dotexpr(lex, e);
                break;
            case '[':
                e = index_expr(lex, e);
                break;
            case '?':
                e = chain_expr(lex, e);
                break;
            default:
                return e;
        }
    }
}

static Expr *simple_expr(Lex *lex)
{
    Expr *expr;
    switch (lex->t.kind) {
        case TK_TRUE:
            expr = emit_bool(lex, PAW_TRUE);
            break;
        case TK_FALSE:
            expr = emit_bool(lex, PAW_FALSE);
            break;
        case TK_INTEGER:
            expr = emit_primitive(lex, lex->t.value, PAW_TINT);
            break;
        case TK_FLOAT:
            expr = emit_primitive(lex, lex->t.value, PAW_TFLOAT);
            break;
        default:
            return suffixed_expr(lex);
    }
    skip(lex); // skip literal
    return expr;
}

static Expr *binop_expr(Lex *lex, InfixOp op, Expr *lhs)
{
    skip(lex); // binary operator token
    Expr *rhs = subexpr(lex, right_prec(op));
    if (rhs == NULL) {
        return NULL; // no more binops
    }
    BinOpExpr *result = pawK_add_node(lex, EXPR_BINOP, BinOpExpr);
    result->op = (BinaryOp)op; // same order
    result->lhs = lhs;
    result->rhs = rhs;
    return cast_expr(result);
}

static Expr *logical_expr(Lex *lex, Expr *lhs, paw_Bool is_and)
{
    skip(lex); // '&&' or '||' token
    Expr *rhs = subexpr(lex, right_prec(INFIX_AND));
    if (rhs == NULL) {
        return NULL; // no more binops
    }
    LogicalExpr *result = pawK_add_node(lex, EXPR_LOGICAL, LogicalExpr);
    result->is_and = is_and;
    result->lhs = lhs;
    result->rhs = rhs;
    return cast_expr(result);
}

static Expr *cond_expr(Lex *lex, Expr *lhs)
{
    skip(lex); // '??' token
    CondExpr *result = pawK_add_node(lex, EXPR_COND, CondExpr);
    result->cond = lhs;
    result->lhs = subexpr(lex, right_prec(INFIX_COND));
    check_next(lex, TK_COLON2);
    result->rhs = expr0(lex);
    return cast_expr(result);
}

static Expr *infix_expr(Lex *lex, Expr *lhs, unsigned op)
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

static Expr *subexpr(Lex *lex, unsigned prec)
{
    unsigned op = get_unop(lex->t.kind);
    Expr *expr = op == NOT_UNOP
                     ? simple_expr(lex)
                     : unop_expr(lex, op);

    op = get_infixop(lex->t.kind);
    while (op != NOT_INFIX && prec < left_prec(op)) {
        expr = infix_expr(lex, expr, op);
        op = get_infixop(lex->t.kind);
    }
    return expr;
}

static Stmt *if_stmt(Lex *lex)
{
    skip(lex); // 'if' token
    IfElseStmt *result = pawK_add_node(lex, STMT_IFELSE, IfElseStmt);
    result->cond = expr0(lex); // conditional
    result->then_arm = cast_stmt(block(lex)); // 'then' block

    if (test_next(lex, TK_ELSE)) {
        if (test(lex, TK_IF)) {
            // Put the rest of the chain in the else branch. This transformation looks
            // like 'if a {} else if b {} else {}' -> 'if a {} else {if b {} else {}}'.
            result->else_arm = if_stmt(lex);
        } else {
            result->else_arm = cast_stmt(block(lex));
        }
    }
    return cast_stmt(result);
}

static Stmt *exprstmt(Lex *lex)
{
    ExprStmt *result = pawK_add_node(lex, STMT_EXPR, ExprStmt);
    result->lhs = suffixed_expr(lex);
    if (test_next(lex, '=')) {
        result->rhs = expr0(lex);
    }
    semicolon(lex);
    return cast_stmt(result);
}

static Expr *emit_literal(Lex *lex, const char *name, Expr *expr, paw_Type t)
{
    LiteralExpr *result = pawK_add_node(lex, EXPR_LITERAL, LiteralExpr);
    result->label = name;
    result->expr = expr;
    result->t = t;
    return cast_expr(result);
}

static Expr *literal_expr(Lex *lex, const char *name, paw_Type t)
{
    return emit_literal(lex, name, expr0(lex), t);
}

static Stmt *fornum(Lex *lex, String *ivar)
{
    ForStmt *result = pawK_add_node(lex, STMT_FORNUM, ForStmt);
    ForNum *fornum = &result->fornum;
    result->name = ivar;
    // Parse the loop bounds ('begin', 'end', and 'step' expressions).
    fornum->begin = literal_expr(lex, "(for begin)", PAW_TINT);
    check_next(lex, ',');
    fornum->end = literal_expr(lex, "(for end)", PAW_TINT);
    if (test_next(lex, ',')) {
        fornum->step = literal_expr(lex, "(for step)", PAW_TINT);
    } else {
        Value v;
        v_set_int(&v, 1); // step defaults to 1
        Expr *step = emit_primitive(lex, v, PAW_TINT);
        fornum->step = emit_literal(lex, "(for step)", step, PAW_TINT);
    }
    result->block = block(lex);
    return cast_stmt(result);
}

static Stmt *forin(Lex *lex, String *ivar)
{
    ForStmt *result = pawK_add_node(lex, STMT_FORIN, ForStmt);
    ForIn *forin = &result->forin;
    forin->target = expr0(lex);
    result->name = ivar;
    result->block = block(lex);
    return cast_stmt(result);
}

static Stmt *for_stmt(Lex *lex)
{
    skip(lex); // 'for' token
    String *ivar = parse_name(lex); // loop variable
    if (test_next(lex, '=')) {
        return fornum(lex, ivar);
    } else if (!test_next(lex, TK_IN)) {
        expected_symbol(lex, "'=' or 'in'"); // no return
    }
    return forin(lex, ivar);
}

static Stmt *while_stmt(Lex *lex)
{
    WhileStmt *result = pawK_add_node(lex, STMT_WHILE, WhileStmt);
    skip(lex); // 'while' token
    result->cond = expr0(lex);
    result->block = block(lex);
    return cast_stmt(result);
}

static Stmt *dowhile_stmt(Lex *lex)
{
    WhileStmt *result = pawK_add_node(lex, STMT_DOWHILE, WhileStmt);
    skip(lex); // 'do' token
    result->block = block(lex);
    check_next(lex, TK_WHILE);
    result->cond = expr0(lex);
    return cast_stmt(result);
}

static Stmt *return_stmt(Lex *lex)
{
    ReturnStmt *result = pawK_add_node(lex, STMT_RETURN, ReturnStmt);
    skip(lex); // 'return' token
    if (end_of_block(lex) || test(lex, ';')) {
        result->expr = NULL;
    } else {
        result->expr = expr0(lex);
    }
    // NOTE: The construct 'return [expr] [`;`]' must be followed by the
    //       end of the block. This is necessary because the semicolon is
    //       optional, and we cannot easily tell if it was intended to go
    //       before or after the '[expr]' part.
    semicolon(lex);
    return cast_stmt(result);
}

static Stmt *labelstmt(Lex *lex, LabelKind kind)
{
    LabelStmt *result = pawK_add_node(lex, STMT_LABEL, LabelStmt);
    result->label = kind;
    skip(lex); // 'break' or 'continue' token
    semicolon(lex);
    return cast_stmt(result);
}

static void function(Lex *lex, String *name, Function *pfn)
{
    pfn->nargs = fn_parameters(lex, &pfn->args);
    pfn->ret = ret_annotation(lex);
    pfn->name = name;
    pfn->body = block(lex);
}

static Stmt *fn_stmt(Lex *lex, int line, paw_Bool global)
{
    skip(lex); // 'fn' token
    FnStmt *result = pawK_add_node(lex, STMT_FN, FnStmt);
    String *name = parse_name(lex);
    result->flags.kind = FN_FUNCTION;
    result->flags.global = global;
    result->line = line;
    function(lex, name, &result->fn);
    return cast_stmt(result);
}

static Stmt *attr_def(Lex *lex, String *name)
{
    const int line = lex->lastline;
    AttrStmt *result = pawK_add_node(lex, STMT_ATTR, AttrStmt);
    result->line = line; // line containing 'global' or 'let'
    result->name = name;
    result->tag = var_annotation(lex);
    semicolon(lex);
    return cast_stmt(result);
}

static Stmt *method_def(Lex *lex, String *name)
{
    const int line = lex->lastline;
    AttrStmt *result = pawK_add_node(lex, STMT_ATTR, AttrStmt);
    result->name = name;
    result->line = line;
    result->is_fn = PAW_TRUE;
    function(lex, name, &result->fn);
    return cast_stmt(result);
}

static Stmt *attrstmt(Lex *lex)
{
    String *name = v_string(lex->t.value);
    skip(lex); // name token
    if (test(lex, ':') || test(lex, '=')) {
        return attr_def(lex, name);
    } else {
        return method_def(lex, name);
    }
}

static int class_body(Lex *lex, Stmt **phead)
{
    Stmt *last;
    const int line = lex->line;
    check_next(lex, '{');

    int nattrs = 0;
    while (!test(lex, '}')) {
        check(lex, TK_NAME);
        if (nattrs > LOCAL_MAX) {
            limit_error(lex, "attributes", LOCAL_MAX);
        }
        Stmt *next = attrstmt(lex);
        link_stmt(phead, &last, next);
        ++nattrs;
    }
    delim_next(lex, '}', '{', line);
    return nattrs;
}

static Stmt *class_stmt(Lex *lex, paw_Bool global)
{
    skip(lex); // 'class' token

    ClassStmt *s = pawK_add_node(lex, STMT_CLASS, ClassStmt);
    s->flags.global = global;
    s->name = parse_name(lex);
    if (test_next(lex, ':')) {
        // push superclass
        s->super = varexpr(lex);
    }
    s->nattrs = class_body(lex, &s->attrs);
    semicolon(lex);
    return cast_stmt(s);
}

static Stmt *global_stmt(Lex *lex)
{
    const int line = lex->lastline;
    skip(lex); // 'global' token
    if (test(lex, TK_FN)) {
        skip(lex); // 'fn' token
        return fn_stmt(lex, line, PAW_TRUE);
    } else if (test(lex, TK_CLASS)) {
        return class_stmt(lex, PAW_TRUE);
    } else {
        return variable_def(lex, line, PAW_TRUE);
    }
}

static Stmt *stmt(Lex *lex)
{
try_again:
    switch (lex->t.kind) {
        case ';':
            // empty statement
            skip(lex); // ';' token
            goto try_again;
        case '{':
            return cast_stmt(block(lex));
        case TK_FN:
            return fn_stmt(lex, lex->lastline, PAW_FALSE);
        case TK_CLASS:
            return class_stmt(lex, PAW_FALSE);
        case TK_LET:
            skip(lex); // 'let' token
            return variable_def(lex, lex->lastline, PAW_FALSE);
        case TK_GLOBAL:
            return global_stmt(lex);
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
            return labelstmt(lex, LBREAK);
        case TK_CONTINUE:
            return labelstmt(lex, LCONTINUE);
        default:
            return exprstmt(lex);
    }
}

// All paw language keywords (must be in this order, the same order as the
// keyword variants in the TokenKind enum in lex.h)
static const char *kKeywords[] = {
    "fn",
    "class",
    "super",
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

static String *new_fixed_string(paw_Env *P, const char *s)
{
    String *str = pawS_new_str(P, s);
    pawG_fix_object(P, cast_object(str));
    return str;
}

void pawP_init(paw_Env *P)
{
    // Add all keywords to the interned strings table. Fix them so they are never
    // collected. Also added to the lexer string map.
    for (size_t i = 0; i < paw_countof(kKeywords); ++i) {
        const char *kw = kKeywords[i];
        String *str = new_fixed_string(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }
    for (Metamethod mm = 0; mm < NMETAMETHODS; ++mm) {
        const char *name = pawT_name(mm);
        String *str = new_fixed_string(P, name);
        v_set_object(&P->meta_keys[mm], str);
    }
    v_set_object(&P->str_cache[CSTR_SELF], new_fixed_string(P, "self"));
    v_set_object(&P->str_cache[CSTR_INIT], pawS_new_str(P, "__init"));
    v_set_object(&P->str_cache[CSTR_SUPER], pawS_new_str(P, "super"));
    v_set_object(&P->str_cache[CSTR_TRUE], pawS_new_str(P, "true"));
    v_set_object(&P->str_cache[CSTR_FALSE], pawS_new_str(P, "false"));
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

static Tree *parse_module(Lex *lex)
{
    Tree *ast = lex->ast;
    skip_hashbang(lex);
    ast->nstmts = stmtlist(lex, &ast->stmts);
    check(lex, TK_END);
    return ast;
}

static void dump_expr(paw_Env *P, Expr *expr, int indent);

static void dump_symbols(paw_Env *P, Scope *st, int indent)
{
    for (int i = 0; i < st->nsymbols; ++i) {
        for (int i = 0; i < indent; ++i) {
            printf("    ");
        }
        const Symbol *s = st->symbols[i];
        printf("symbol %s: type = %s\n", s->name->text, pawY_name(y_id(s->type)));
    }
}

static void dump_stmt(paw_Env *P, Stmt *stmt, int indent)
{
    for (int i = 0; i < indent; ++i) {
        printf("    ");
    }
    if (stmt == NULL) {
        puts("NULL\n");
        return;
    }
    switch (stmt->kind) {
        case STMT_EXPR:
            printf("exprstmt\n");
            dump_expr(P, cast_expr(cast_to(stmt, ExprStmt)->lhs), indent + 1);
            if (cast_to(stmt, ExprStmt)->rhs) {
                dump_expr(P, cast_to(stmt, ExprStmt)->rhs, indent + 1);
            }
            break;
        case STMT_BLOCK: {
            Block *bn = cast_to(stmt, Block);
            printf("block\n");
            dump_symbols(P, bn->scope, indent + 1);
            for (Stmt *stmt = bn->stmts; stmt; stmt = stmt->next) {
                dump_stmt(P, cast_stmt(stmt), indent + 1);
            }
            break;
        }
        case STMT_IFELSE:
            printf("ifelse\n");
            break;
        case STMT_FORIN: {
            ForStmt *s = cast_to(stmt, ForStmt);
            printf("forin\n");
            dump_symbols(P, s->scope, indent + 1);
            dump_expr(P, s->forin.target, indent + 1);
            dump_stmt(P, cast_stmt(s->block), indent + 1);
            break;
        }
        case STMT_FORNUM: {
            ForStmt *s = cast_to(stmt, ForStmt);
            printf("fornum\n");
            dump_symbols(P, s->scope, indent + 1);
            dump_expr(P, s->fornum.begin, indent + 1);
            dump_expr(P, s->fornum.end, indent + 1);
            dump_expr(P, s->fornum.step, indent + 1);
            dump_stmt(P, cast_stmt(s->block), indent + 1);
            break;
        }
        case STMT_WHILE: {
            WhileStmt *s = cast_to(stmt, WhileStmt);
            printf("while\n");
            dump_expr(P, s->cond, indent + 1);
            dump_stmt(P, cast_stmt(s->block), indent + 1);
            break;
        }
        case STMT_DOWHILE: {
            WhileStmt *s = cast_to(stmt, WhileStmt);
            printf("dowhile\n");
            dump_stmt(P, cast_stmt(s->block), indent + 1);
            dump_expr(P, s->cond, indent + 1);
            break;
        }
        case STMT_FN: {
            FnStmt *fn = cast_to(stmt, FnStmt);

            printf("fn (%s) %s: %d\n", fn->flags.global ? "global" : "local", fn->fn.name->text, fn->fn.ret ? fn->fn.ret->type->hdr.id : -1);
            for (Stmt *arg = fn->fn.args; arg; arg = arg->next) {
                dump_stmt(P, cast_stmt(arg), indent + 1); // list of DefStmt
            }
            dump_stmt(P, cast_stmt(fn->fn.body), indent + 1);
            break;
        }
        case STMT_PARAM: {
            ParamStmt *in = cast_to(stmt, ParamStmt);
            printf("%s: %d\n", in->name->text, in->tag ? in->tag->type->hdr.id : 42);
            break;
        }
        case STMT_DEF: {
            DefStmt *in = cast_to(stmt, DefStmt);
            printf("%s %s: %d\n", in->flags.global ? "global" : "let", in->name->text, in->tag ? in->tag->type->hdr.id : 42);
            dump_expr(P, in->init, indent + 1);
            break;
        }
        case STMT_RETURN: {
            ReturnStmt *bn = cast_to(stmt, ReturnStmt);
            printf("return\n");
            dump_expr(P, bn->expr, indent + 1);
            break;
        }
        case STMT_ATTR: {
            AttrStmt *s = cast_to(stmt, AttrStmt);
            printf("attr '%s'", s->name->text);
            if (s->is_fn) { 
                printf("\n");
                for (Stmt *arg = s->fn.args; arg; arg = arg->next) {
                    dump_stmt(P, cast_stmt(arg), indent + 1);
                }
                dump_stmt(P, cast_stmt(s->fn.body), indent + 1);
            } else {
                printf("%d\n", s->tag->type->hdr.id);
            }
            break;
        }
        case STMT_CLASS: {
            ClassStmt *s = cast_to(stmt, ClassStmt);
            printf("class '%s'\n", s->name->text);
            for (Stmt *attr = s->attrs; attr; attr = attr->next) {
                dump_stmt(P, cast_stmt(attr), indent + 1);
            }
            break;
        }
        default:
            printf("?\n");
            break;
    }
}

static void dump_expr(paw_Env *P, Expr *expr, int indent)
{
    for (int i = 0; i < indent; ++i) {
        printf("    ");
    }
    if (expr == NULL) {
        puts("NULL\n");
        return;
    }
    if (expr->type) {
        printf("type (%s): ", pawY_name((int)expr->type->hdr.id));
    } else {
        printf("NULL ");
    }
    switch (expr->kind) {
        case EXPR_PRIMITIVE: {
            Buffer buf;
            pawL_init_buffer(P, &buf);
            PrimitiveExpr *ln = cast_to(expr, PrimitiveExpr);
            pawC_pushv(P, ln->v);
            pawL_add_value(P, &buf, P->mod->types[ln->t]->hdr.id);
            pawL_add_char(P, &buf, '\0');
            puts(buf.data);
            pawL_discard_result(P, &buf);
            break;
        }
        case EXPR_UNOP:
            printf("unop %d\n", cast_to(expr, UnOpExpr)->op);
            dump_expr(P, cast_to(expr, UnOpExpr)->target, indent + 1);
            break;
        case EXPR_BINOP:
            printf("binop %d\n", cast_to(expr, BinOpExpr)->op);
            dump_expr(P, cast_to(expr, BinOpExpr)->lhs, indent + 1);
            dump_expr(P, cast_to(expr, BinOpExpr)->rhs, indent + 1);
            break;
        case EXPR_CALL:
            printf("call\n");
            break;
        case EXPR_COND:
            printf("cond\n");
            dump_expr(P, cast_to(expr, CondExpr)->cond, indent + 1);
            dump_expr(P, cast_to(expr, CondExpr)->lhs, indent + 1);
            dump_expr(P, cast_to(expr, CondExpr)->rhs, indent + 1);
            break;
        case EXPR_VAR: {
            printf("var '%s' (%p)\n", cast_to(expr, VarExpr)->name->text, (void*)expr);
            break;
        }
        default:
            printf("?\n");
            break;
    }
}

#include <stdlib.h>
static void dump_ast(Lex *lex, Stmt *root)
{
    for (; root; root = root->next) {
        dump_stmt(lex->P, root, 0);
    }
}

Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *pm, const char *name, void *ud)
{
    // Initialize the lexical state.
    Lex lex = {
        .mod = P->mod,
        .pm = pm,
        .ud = ud,
        .P = P,
    };
    pm->st.globals = pawM_new(P, Scope);
    pawX_set_source(&lex, input);
    lex.ast = pawK_new_ast(P);
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
    p_check_types(&lex); // pass 2 (AST -> checked AST)
                               
    dump_ast(&lex, lex.ast->stmts); // TODO: remove
                               
    p_generate_code(&lex); // pass 2 (checked AST -> bytecode)

    // Pop the lexer map. The strings it contains should be anchored elsewhere.
    // Leave the main closure on top of the stack.
    pawC_stkdec(P, 1);

    // cleanup
    pawK_free_ast(P, lex.ast);
    return lex.main;
}
