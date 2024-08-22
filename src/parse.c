// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "parse.h"
#include "ast.h"
#include "compile.h"

#define NEW_EXPR(lex, kind) pawAst_new_expr((lex)->ast, (lex)->last_line, kind)
#define NEW_STMT(lex, kind) pawAst_new_stmt((lex)->ast, (lex)->last_line, kind)
#define NEW_DECL(lex, kind) pawAst_new_decl((lex)->ast, (lex)->last_line, kind)

static String *unpack_name(const struct AstExpr *expr)
{
    if (!AstIsPathExpr(expr)) {
        return NULL;
    }
    struct AstPath *path = expr->path.path;
    if (path->count == 1) {
        struct AstSegment *ps = pawAst_path_get(path, 0);
        if (ps->types == NULL) {
            return ps->name;
        }
    }
    return NULL;
}

// recursive non-terminals
static struct AstExpr *expression(struct Lex *lex, unsigned prec);
static struct AstStmt *statement(struct Lex *lex);

static struct AstExpr *expr0(struct Lex *lex) 
{
    return expression(lex, 0); 
}

static void expected_symbol(struct Lex *lex, const char *want)
{
    pawX_error(lex, "expected %s", want);
}

static void missing_delim(struct Lex *lex, TokenKind want, TokenKind open, int open_line)
{
    pawX_error(lex, "expected '%c' to match '%c' on line %d", want, open, open_line);
}

static void delim_next(struct Lex *lex, TokenKind want, TokenKind open, int open_line)
{
    const TokenKind have = lex->t.kind;
    if (have != want) {
        if (have == TK_GREATER2 && want == '>') {
            // special case: split '>>' into 2 '>'
            lex->t.kind = '>';
            return;
        }
        missing_delim(lex, want, open, open_line);
    }
    pawX_next(lex);
}

static void enter_nested(struct Lex *lex)
{
    ++lex->nest_depth;
    const int max_nesting = 1000;
    if (lex->nest_depth > max_nesting) {
        pawX_error(lex, "exceeded maximum nesting depth"); 
    }
}

static void leave_nested(struct Lex *lex)
{
    paw_assert(lex->nest_depth >= 0);
    --lex->nest_depth;
}

// ORDER UnaryOp
enum UnOp {
    UN_LEN, // #
    UN_NEG, // -
    UN_NOT, // !
    UN_BNOT, // ~

    NUNOPS
};

// ORDER BinaryOp
enum InfixOp {
    INFIX_EQ, // ==
    INFIX_NE, // !=
    INFIX_LT, // <
    INFIX_LE, // <=
    INFIX_GT, // >
    INFIX_GE, // >=
    INFIX_IN, // in
    INFIX_AS, // as
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
    INFIX_ASSIGN, // =

    NINFIX
};

#define NOT_UNOP NUNOPS
#define NOT_INFIX NINFIX

static const struct {
    uint8_t left;
    uint8_t right;
} kInfixPrec[NINFIX] = {
    [INFIX_AS] = {12, 12},
    [INFIX_MUL] = {11, 11},
    [INFIX_DIV] = {11, 11},
    [INFIX_MOD] = {11, 11},
    [INFIX_ADD] = {10, 10},
    [INFIX_SUB] = {10, 10},
    [INFIX_SHL] = {9, 9},
    [INFIX_SHR] = {9, 9},
    [INFIX_BAND] = {8, 8},
    [INFIX_BXOR] = {7, 7},
    [INFIX_BOR] = {6, 6},
    [INFIX_IN] = {5, 5},
    [INFIX_LT] = {5, 5},
    [INFIX_LE] = {5, 5},
    [INFIX_GT] = {5, 5},
    [INFIX_GE] = {5, 5},
    [INFIX_EQ] = {4, 4},
    [INFIX_NE] = {4, 4},
    [INFIX_AND] = {3, 3},
    [INFIX_OR] = {2, 2},
    [INFIX_ASSIGN] = {1, 1},
};

static const uint8_t kUnOpPrecedence = 13;

static unsigned left_prec(enum InfixOp op) 
{ 
    return kInfixPrec[op].left; 
}

static unsigned right_prec(enum InfixOp op) 
{
    return kInfixPrec[op].right; 
}

static enum UnOp get_unop(TokenKind kind)
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

static enum InfixOp get_infixop(TokenKind kind)
{
    switch (kind) {
        case '=':
            return INFIX_ASSIGN;
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
        case TK_AS:
            return INFIX_AS;
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

static void skip(struct Lex *lex) 
{
     pawX_next(lex); 
}

static void check(struct Lex *lex, TokenKind want)
{
    if (lex->t.kind != want) {
        pawX_error(lex, "unexpected symbol");
    }
}

static void check_next(struct Lex *lex, TokenKind want)
{
    check(lex, want);
    skip(lex);
}

static paw_Bool test(struct Lex *lex, TokenKind kind) 
{
    return lex->t.kind == kind; 
}

static paw_Bool test_next(struct Lex *lex, TokenKind kind)
{
    if (test(lex, kind)) {
        skip(lex);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void semicolon(struct Lex *lex) 
{
    check_next(lex, ';'); 
}

static String *parse_name(struct Lex *lex)
{
    check(lex, TK_NAME);
    String *name = V_STRING(lex->t.value);
    skip(lex);
    return name;
}

static struct AstExpr *new_basic_lit(struct Lex *lex, Value v, paw_Type code)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstLiteralExpr);
    r->literal.basic.t = code;
    r->literal.basic.value = v;
    return r;
}

static struct AstExpr *emit_unit(struct Lex *lex)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstTupleType);
    r->tuple.types = pawAst_expr_list_new(lex->ast);
    return r;
}

static struct AstExpr *emit_bool(struct Lex *lex, paw_Bool b)
{
    Value v;
    V_SET_BOOL(&v, b);
    return new_basic_lit(lex, v, PAW_TBOOL);
}

static struct AstExpr *type_expr(struct Lex *lex);

static struct AstDecl *vfield_decl(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstFieldDecl);
    r->field.name = NULL;
    r->field.tag = type_expr(lex);
    return r;
}

#define DEFINE_LIST_PARSER(name, a, b, limit, what, func, prefix, L) \
    static void parse_##name##_list(struct Lex *lex, struct L *list, int line) \
    { \
        do { \
            if (test(lex, b)) break; \
            if ((list)->count == (limit)) { \
                limit_error(lex, what, (limit)); \
            } \
            prefix##push((lex)->dm->ast, list, (func)(lex)); \
        } while (test_next(lex, ',')); \
        delim_next(lex, b, a, line); \
    }
DEFINE_LIST_PARSER(arg, '(', ')', LOCAL_MAX, "arguments", expr0, pawAst_expr_list_, AstExprList)
DEFINE_LIST_PARSER(vfield, '(', ')', LOCAL_MAX, "variant fields", vfield_decl, pawAst_decl_list_, AstDeclList)
DEFINE_LIST_PARSER(type, '<', '>', LOCAL_MAX, "type arguments", type_expr, pawAst_expr_list_, AstExprList)

static struct AstDeclList *vfield_list(struct Lex *lex, int line)
{
    ++lex->expr_depth;
    struct AstDeclList *list = pawAst_decl_list_new(lex->ast);
    parse_vfield_list(lex, list, line);
    if (list->count == 0) {
        pawX_error(lex, "expected at least 1 variant field between parenthesis "
                        "(remove parenthesis for unit variant)");
    }
    --lex->expr_depth;
    return list;
}

static struct AstExprList *type_list(struct Lex *lex, int line)
{
    ++lex->expr_depth;
    struct AstExprList *list = pawAst_expr_list_new(lex->ast);
    parse_type_list(lex, list, line);
    if (list->count == 0) {
        pawX_error(lex, "expected at least 1 type");
    }
    --lex->expr_depth;
    return list;
}

static struct AstExprList *maybe_type_args(struct Lex *lex)
{
    const int line = lex->line;
    if (test_next(lex, '<')) {
        // type arguments (requires at least 1)
        return type_list(lex, line);
    }
    return NULL;
}

static void set_unit(struct Lex *lex, struct AstExpr *pe)
{
    pe->tuple.kind = kAstTupleType;
    pe->tuple.types = pawAst_expr_list_new(lex->ast);
}

static struct AstExpr *unit_type(struct Lex *lex)
{
    struct AstExpr *r = NEW_EXPR(lex, 0);
    set_unit(lex, r);
    return r;
}

static struct AstExpr *type_expr(struct Lex *lex);

static void parse_tuple_type(struct Lex *lex, struct AstExpr *pe, int line)
{
    struct AstExpr *first = NEW_EXPR(lex, 0);
    *first = *pe;

    struct AstExprList *elems = pawAst_expr_list_new(lex->ast);
    pawAst_expr_list_push(lex->ast, elems, first);

    do {
        if (test(lex, ')')) break;
        if (elems->count == FIELD_MAX) {
            limit_error(lex, "tuple elements", FIELD_MAX);
        }
        struct AstExpr *type = type_expr(lex);
        pawAst_expr_list_push(lex->ast, elems, type);
    } while (test_next(lex, ','));
    delim_next(lex, ')', '(', line);

    pe->tuple.kind = kAstTupleType;
    pe->tuple.types = elems;
}

static struct AstExpr *parse_paren_type(struct Lex *lex)
{
    const int line = lex->last_line;
    if (test_next(lex, ')')) {
        return unit_type(lex);
    }
    struct AstExpr *e = type_expr(lex);
    if (test_next(lex, ',')) {
        parse_tuple_type(lex, e, line);
    }
    return e;
}

static struct AstExpr *parse_container_type(struct Lex *lex)
{
    const int line = lex->last_line;
    struct AstExpr *e = NEW_EXPR(lex, kAstContainerType);
    e->cont.first = type_expr(lex);
    if (test_next(lex, ':')) {
        e->cont.second = type_expr(lex);
    }
    delim_next(lex, ']', '[', line);
    return e;
}

static struct AstPath *parse_pathexpr(struct Lex *lex)
{
    struct AstPath *p = pawAst_path_new(lex->ast);
    do {
    next_segment:
        if (p->count == LOCAL_MAX) {
            limit_error(lex, "path segments", LOCAL_MAX);
        }
        String *name = parse_name(lex);
        struct AstExprList *args = NULL;
        if (test_next(lex, TK_COLON2)) {
            args = maybe_type_args(lex);
            if (args == NULL) {
                pawAst_path_add(lex->ast, p, name, NULL);
                goto next_segment;
            }
        }
        pawAst_path_add(lex->ast, p, name, args);
    } while (test_next(lex, TK_COLON2));
    return p;
}

static struct AstPath *parse_pathtype(struct Lex *lex)
{
    struct AstPath *p = pawAst_path_new(lex->ast);
    do {
        if (p->count == LOCAL_MAX) {
            limit_error(lex, "path segments", LOCAL_MAX);
        }
        String *name = parse_name(lex);
        struct AstExprList *args = maybe_type_args(lex);
        pawAst_path_add(lex->ast, p, name, args);
    } while (test_next(lex, TK_COLON2));
    return p;
}

static struct AstExpr *path_expr(struct Lex *lex)
{
    struct AstExpr *result = NEW_EXPR(lex, kAstPathExpr);
    struct AstPathExpr *r = AstGetPathExpr(result);
    r->path = parse_pathexpr(lex);
    return result;
}

static struct AstExpr *parse_signature(struct Lex *lex);

static struct AstExpr *type_expr(struct Lex *lex)
{
    if (test_next(lex, '(')) {
        return parse_paren_type(lex);
    } else if (test_next(lex, '[')) {
        return parse_container_type(lex);
    } else if (test_next(lex, TK_FN)) {
        return parse_signature(lex);
    }
    struct AstExpr *e = NEW_EXPR(lex, kAstPathExpr);
    e->path.path = parse_pathtype(lex);
    return e;
}

static struct AstExpr *type_annotation(struct Lex *lex)
{
    if (test_next(lex, ':')) {
        struct AstExpr *tn = type_expr(lex);
        if (tn == NULL) {
            pawX_error(lex, "invalid type annotation");
        }
        return tn;
    }
    return NULL; // needs inference
}

static struct AstExpr *expect_annotation(struct Lex *lex, const char *what,
                                  const String *name)
{
    struct AstExpr *type = type_annotation(lex);
    if (type == NULL) {
        pawX_error(lex, "expected type annotation on %s '%s'", what,
                   name->text);
    }
    return type;
}

static struct AstExpr *sig_param_expr(struct Lex *lex)
{
    struct AstExpr *result = NEW_EXPR(lex, kAstPathExpr);
    struct AstPathExpr *r = AstGetPathExpr(result);
    struct AstSegment *base = pawAst_segment_new(lex->ast);
    r->path = pawAst_path_new(lex->ast);
    pawAst_path_add(lex->ast, r->path, parse_name(lex), NULL);
    return result;
}

static struct AstDecl *func_param_decl(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstFieldDecl);
    r->field.name = parse_name(lex);
    r->field.tag = expect_annotation(lex, "function parameter", r->field.name);
    return r;
}

static struct AstDecl *clos_param_decl(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstFieldDecl);
    r->field.name = parse_name(lex);
    r->field.tag = type_annotation(lex);
    return r;
}

static struct AstDecl *let_decl(struct Lex *lex, int line, paw_Bool pub)
{
    struct AstDecl *r = NEW_DECL(lex, kAstVarDecl);
    skip(lex); // 'let' token
    r->var.line = line; // line containing 'pub' or 'let'
    r->var.name = parse_name(lex);
    r->var.tag = type_annotation(lex);
    if (!test_next(lex, '=')) {
        pawX_error(lex, "missing initializer");
    }
    r->var.init = expr0(lex);
    r->var.is_pub = pub;
    return r;
}

static struct AstDecl *generic_param(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstGenericDecl);
    r->generic.name = parse_name(lex);
    return r;
}

DEFINE_LIST_PARSER(func_param, '(', ')', LOCAL_MAX, "function parameters", func_param_decl, pawAst_decl_list_, AstDeclList)
DEFINE_LIST_PARSER(sig_param, '(', ')', LOCAL_MAX, "function parameters", sig_param_expr, pawAst_expr_list_, AstExprList)
DEFINE_LIST_PARSER(clos_param, '|', '|', LOCAL_MAX, "closure parameters", clos_param_decl, pawAst_decl_list_, AstDeclList)
DEFINE_LIST_PARSER(generic, '<', '>', LOCAL_MAX, "generics", generic_param, pawAst_decl_list_, AstDeclList)

static struct AstExpr *sitem_expr(struct Lex *lex)
{
    struct AstExpr *result = NEW_EXPR(lex, kAstFieldExpr);
    struct AstFieldExpr *r = AstGetFieldExpr(result);
    r->name = parse_name(lex);
    check_next(lex, ':');
    r->value = expr0(lex);
    return result;
}

DEFINE_LIST_PARSER(sitem, '{', '}', LOCAL_MAX, "struct items", sitem_expr, pawAst_expr_list_, AstExprList)

static struct AstExpr *unop_expr(struct Lex *lex, enum UnOp op)
{
    enter_nested(lex);
    struct AstExpr *result = NEW_EXPR(lex, kAstUnOpExpr);
    struct AstUnOpExpr *r = &result->unop;
    skip(lex); // unary operator token
    r->op = CAST(op, UnaryOp); // same order
    r->target = expression(lex, kUnOpPrecedence);
    leave_nested(lex);
    return result;
}

// Parse either a parenthsized expression or a tuple
static struct AstExpr *paren_expr(struct Lex *lex)
{
    // Just parse and return the expression contained within the parenthesis.
    // There is no need for an extra node type.
    const int line = lex->line;
    skip(lex); // '(' token
    if (test_next(lex, ')')) {
        const Value v = {.p = NULL};
        return new_basic_lit(lex, v, PAW_TUNIT);
    }
    ++lex->expr_depth;
    struct AstExpr *expr = expr0(lex);
    --lex->expr_depth;
    if (test_next(lex, ')')) {
        return expr;
    }
    struct AstExpr *r = NEW_EXPR(lex, kAstLiteralExpr);
    check_next(lex, ',');
    struct AstExprList *elems = pawAst_expr_list_new(lex->ast);
    pawAst_expr_list_push(lex->ast, elems, expr);
    parse_arg_list(lex, elems, line);
    r->literal.lit_kind = kAstTupleLit;
    r->literal.tuple.elems = elems;
    return r;
}

static struct AstExpr *parse_signature(struct Lex *lex)
{
    const int line = lex->last_line;
    struct AstExpr *e = NEW_EXPR(lex, kAstSignature);
    check_next(lex, '(');
    e->sig.params = pawAst_expr_list_new(lex->ast);
    if (!test_next(lex, ')')) {
        parse_sig_param_list(lex, e->sig.params, line);
    }
    if (test_next(lex, TK_ARROW)) {
        e->sig.result = type_expr(lex);
    } else {
        e->sig.result = emit_unit(lex);
    }
    return e;
}

static paw_Bool end_of_block(struct Lex *lex)
{
    return test(lex, '}') || // found proper end
           test(lex, TK_END); // truncated block
}

static struct AstExpr *index_expr(struct Lex *lex, struct AstExpr *target)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstIndex);
    skip(lex); // '[' token
    r->index.target = target;
    if (!test(lex, ':')) {
        r->index.first = expr0(lex);
    }
    if (test_next(lex, ':')) {
        r->index.is_slice = PAW_TRUE;
        if (!test(lex, ']')) {
            r->index.second = expr0(lex);
        }
    }
    delim_next(lex, ']', '[', r->index.line);
    return r;
}

static paw_Type parse_container_items(struct Lex *lex, struct AstExprList **pitems)
{
    struct AstExprList *items = *pitems;
    paw_Type code = -1;
    do {
        if (test(lex, ']')) break;
        if (items->count == LOCAL_MAX) {
            limit_error(lex, "container literal items", LOCAL_MAX);
        }
        struct AstExpr *item = expr0(lex);
        if (!test_next(lex, ':')) {
            code = PAW_TLIST;
        } else if (code == PAW_TLIST) {
            pawX_error(lex, "invalid container literal");
        } else {
            code = PAW_TMAP;
            struct AstExpr *result = NEW_EXPR(lex, kAstFieldExpr);
            struct AstFieldExpr *r = AstGetFieldExpr(result);
            r->fid = -1; // not a fixed structure field
            r->value = expr0(lex);
            r->key = item;
            item = result;
        }
        pawAst_expr_list_push(lex->ast, items, item);
    } while (test_next(lex, ','));
    *pitems = items;
    return code;
}

static struct AstExpr *container_lit(struct Lex *lex)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstLiteralExpr);
    skip(lex); // '[' token
    r->literal.lit_kind = kAstContainerLit;
    struct AstContainerLit *cont = &r->literal.cont;
    cont->items = pawAst_expr_list_new(lex->ast);
    if (test(lex, ']')) {
        cont->code = PAW_TLIST; // empty list: '[]'
    } else if (test_next(lex, ':')) {
        cont->code = PAW_TMAP; // empty map: '[:]'
    } else {
        cont->code = parse_container_items(lex, &cont->items);
    }
    delim_next(lex, ']', '[', r->hdr.line);
    return r;
}

// Parse a composite literal expression
static struct AstExpr *composite_lit(struct Lex *lex, struct AstExpr *path)
{
    // Path(path) -> Literal(Path(path), fields)
    struct AstPathExpr copy = path->path;
    struct AstLiteralExpr *lit = &path->literal;
    lit->kind = kAstLiteralExpr;
    lit->lit_kind = kAstCompositeLit;
    lit->comp.path = copy.path;

    const int line = lex->line;
    skip(lex); // '{' token
    lit->comp.items = pawAst_expr_list_new(lex->ast);
    parse_sitem_list(lex, lit->comp.items, line);
    return path;
}

static struct AstExpr *try_composite_lit(struct Lex *lex, struct AstExpr *expr)
{
    enter_nested(lex);
    if (AstIsPathExpr(expr) && lex->expr_depth >= 0) {
        expr = composite_lit(lex, expr);
    }
    leave_nested(lex);
    return expr;
}

static struct AstExpr *selector_expr(struct Lex *lex, struct AstExpr *target)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstSelector);
    skip(lex); // '.' token
    r->selector.target = target;
    if (test(lex, TK_NAME)) {
        r->selector.name = parse_name(lex);
    } else if (test(lex, TK_INTEGER)) {
        r->selector.index = V_INT(lex->t.value);
        r->selector.is_index = PAW_TRUE;
        skip(lex); // integer token
    }
    return r;
}

static paw_Bool equals_cstr(struct Lex *lex, const String *ident, unsigned cstr)
{
    return pawS_eq(ident, pawE_cstr(ENV(lex), cstr));
}

static struct AstExpr *call_expr(struct Lex *lex, struct AstExpr *target)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstCallExpr);
    skip(lex); // '(' token
    r->call.target = target;
    r->call.args = pawAst_expr_list_new(lex->ast);
    parse_arg_list(lex, r->call.args, r->call.line);
    return r;
}

static struct AstExpr *chain_expr(struct Lex *lex, struct AstExpr *target)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstChainExpr);
    r->chain.target = target;
    skip(lex); // '?' token
    return r;
}

static struct AstDeclList *func_parameters(struct Lex *lex)
{
    const int line = lex->line;
    check_next(lex, '(');
    struct AstDeclList *list = pawAst_decl_list_new(lex->ast);
    parse_func_param_list(lex, list, line);
    return list;
}

static struct AstDeclList *clos_parameters(struct Lex *lex)
{
    const int line = lex->line;
    check_next(lex, '|');
    struct AstDeclList *list = pawAst_decl_list_new(lex->ast);
    parse_clos_param_list(lex, list, line);
    return list;
}

static struct AstBlock *block(struct Lex *lex)
{
    const int line = lex->line;
    struct AstStmt *result = NEW_STMT(lex, kAstBlock);
    struct AstBlock *r = &result->block;
    check_next(lex, '{');
    r->stmts = pawAst_stmt_list_new(lex->ast);
    while (!end_of_block(lex)) {
        struct AstStmt *next = statement(lex);
        if (next != NULL) {
            pawAst_stmt_list_push(lex->ast, r->stmts, next);
            if (AstIsReturnStmt(next) || AstIsLabelStmt(next)) {
                break; // must be last statement in block
            }
        }
    }
    delim_next(lex, '}', '{', line);
    return r;
}

static struct AstExpr *basic_expr(struct Lex *lex);

static struct AstExpr *closure(struct Lex *lex)
{
    paw_Bool has_body = PAW_FALSE;
    struct AstExpr *r = NEW_EXPR(lex, kAstClosureExpr);
    r->clos.params = clos_parameters(lex);
    if (test_next(lex, TK_ARROW)) {
        r->clos.result = type_expr(lex);
        has_body = PAW_TRUE;
    }
    if (test(lex, '{') || has_body) {
        r->clos.body = block(lex);
        r->clos.has_body = PAW_TRUE;
    } else {
        r->clos.expr = expr0(lex);
    }
    return r;
}

static struct AstExpr *primary_expr(struct Lex *lex)
{
    enter_nested(lex);
    struct AstExpr *expr;
    switch (lex->t.kind) {
        case '(':
            expr = paren_expr(lex);
            break;
        case '[':
            expr = container_lit(lex);
            break;
        case TK_NAME:
            expr = path_expr(lex);
            break;
        default:
            expr = NULL;
    }
    leave_nested(lex);
    return expr;
}

static struct AstExpr *suffixed_expr(struct Lex *lex)
{
    struct AstExpr *e = primary_expr(lex);
    if (e == NULL) return NULL;
    if (lex->t.kind == '{') {
        e = try_composite_lit(lex, e);
    }
    for (int n = 1;; ++n) {
        enter_nested(lex);
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
            case '[':
                e = index_expr(lex, e);
                break;
            default:
                lex->nest_depth -= n;
                return e;
        }
    }
}

static struct AstExpr *simple_expr(struct Lex *lex)
{
    struct AstExpr *expr;
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
        case TK_STRING: {
            const Value v = lex->t.value;
            expr = new_basic_lit(lex, v, PAW_TSTRING);
            break;
        }
        case TK_PIPE2:
            lex->t.kind = '|';
            lex->t2.kind = '|';
            // (fallthrough)
        case '|':
            return closure(lex);
        default:
            return suffixed_expr(lex);
    }
    skip(lex); // skip literal
    return expr;
}

static struct AstExpr *conversion_expr(struct Lex *lex, struct AstExpr *lhs)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstConversionExpr);
    r->conv.arg = lhs;

    struct AstExpr *rhs = expression(lex, right_prec(INFIX_AS));
    if (!AstIsPathExpr(rhs) || rhs->path.path->count != 1) {
        pawX_error(lex, "expected basic type name");
    }
    struct AstPath *path = rhs->path.path;
    struct AstSegment *seg = pawAst_path_get(path, 0);
    if (equals_cstr(lex, seg->name, CSTR_BOOL)) {
        r->conv.to = PAW_TBOOL;
    } else if (equals_cstr(lex, seg->name, CSTR_INT)) {
        r->conv.to = PAW_TINT;
    } else if (equals_cstr(lex, seg->name, CSTR_FLOAT)) {
        r->conv.to = PAW_TFLOAT;
    } else {
        pawX_error(lex, "expected basic type");
    }
    return r;
}

static struct AstExpr *assignment_expr(struct Lex *lex, struct AstExpr *lhs)
{
    struct AstExpr *result = NEW_EXPR(lex, kAstAssignExpr);
    struct AstAssignExpr *r = AstGetAssignExpr(result);
    r->rhs = expression(lex, right_prec(INFIX_ASSIGN));
    r->lhs = lhs;
    return result;
}

static struct AstExpr *binop_expr(struct Lex *lex, enum InfixOp op, struct AstExpr *lhs)
{
    enter_nested(lex);
    struct AstExpr *rhs = expression(lex, right_prec(op));
    struct AstExpr *result = NEW_EXPR(lex, kAstBinOpExpr);
    struct AstBinOpExpr *r = AstGetBinOpExpr(result);
    r->op = CAST(op, BinaryOp); // same order
    r->lhs = lhs;
    r->rhs = rhs;
    leave_nested(lex);
    return result;
}

static struct AstExpr *logical_expr(struct Lex *lex, struct AstExpr *lhs, paw_Bool is_and)
{
    struct AstExpr *rhs = expression(lex, right_prec(INFIX_AND));
    struct AstExpr *r = NEW_EXPR(lex, kAstLogicalExpr);
    r->logical.is_and = is_and;
    r->logical.lhs = lhs;
    r->logical.rhs = rhs;
    return r;
}

static struct AstExpr *infix_expr(struct Lex *lex, struct AstExpr *lhs, unsigned op)
{
    skip(lex); // operator token
    switch (op) {
        case INFIX_AND:
            return logical_expr(lex, lhs, PAW_TRUE);
        case INFIX_OR:
            return logical_expr(lex, lhs, PAW_FALSE);
        case INFIX_AS:
            return conversion_expr(lex, lhs);
        case INFIX_ASSIGN:
            return assignment_expr(lex, lhs);
        default:
            return binop_expr(lex, op, lhs);
    }
}

static struct AstExpr *subexpr(struct Lex *lex, unsigned prec)
{
    unsigned op = get_unop(lex->t.kind);
    struct AstExpr *expr = op == NOT_UNOP 
        ? simple_expr(lex) 
        : unop_expr(lex, op);
    if (expr == NULL) return NULL;

    op = get_infixop(lex->t.kind);
    while (op != NOT_INFIX && prec < left_prec(op)) {
        expr = infix_expr(lex, expr, op);
        op = get_infixop(lex->t.kind);
    }
    return expr;
}

static struct AstExpr *expression(struct Lex *lex, unsigned prec)
{
    struct AstExpr *expr = subexpr(lex, prec);
    if (expr == NULL) pawX_error(lex, "expected expression");
    return expr;
}

static struct AstExpr *basic_expr(struct Lex *lex)
{
    const int prev_depth = lex->expr_depth;
    lex->expr_depth = -1;
    struct AstExpr *expr = expr0(lex);
    lex->expr_depth = prev_depth;
    return expr;
}

static struct AstStmt *if_stmt(struct Lex *lex)
{
    skip(lex); // 'if' token
    struct AstStmt *result = NEW_STMT(lex, kAstIfStmt);
    struct AstIfStmt *r = AstGetIfStmt(result);
    r->cond = basic_expr(lex);
    r->then_arm = AST_CAST_STMT(block(lex));

    if (test_next(lex, TK_ELSE)) {
        if (test(lex, TK_IF)) {
            // Put the rest of the chain in the else branch. This transformation
            // looks like 'if a {} else if b {} else {}' => 'if a {} else {if b
            // {} else {}}'.
            r->else_arm = if_stmt(lex);
        } else {
            r->else_arm = AST_CAST_STMT(block(lex));
        }
    }
    return result;
}

static struct AstStmt *expr_stmt(struct Lex *lex)
{
    struct AstStmt *result = NEW_STMT(lex, kAstExprStmt);
    struct AstExprStmt *r = &result->expr;
    r->expr = expr0(lex);
    semicolon(lex);
    return result;
}

static struct AstStmt *fornum(struct Lex *lex, String *ivar)
{
    struct AstStmt *r = NEW_STMT(lex, kAstForStmt);
    struct AstForNum *fornum = &AstGetForStmt(r)->fornum;
    r->for_.is_fornum = PAW_TRUE;
    r->for_.name = ivar;

    // Parse the loop bounds ('begin', 'end', and 'step' expressions).
    fornum->begin = basic_expr(lex);
    check_next(lex, ',');
    fornum->end = basic_expr(lex);
    if (test_next(lex, ',')) {
        fornum->step = basic_expr(lex);
    } else {
        Value v;
        V_SET_INT(&v, 1); // step defaults to 1
        fornum->step = new_basic_lit(lex, v, PAW_TINT);
    }

    r->for_.block = block(lex);
    return r;
}

static struct AstStmt *forin(struct Lex *lex, String *ivar)
{
    struct AstStmt *r = NEW_STMT(lex, kAstForStmt);
    r->for_.forin.target = basic_expr(lex);
    r->for_.name = ivar;
    r->for_.block = block(lex);
    return r;
}

static struct AstStmt *for_stmt(struct Lex *lex)
{
    skip(lex); // 'for' token
    String *ivar = parse_name(lex);
    if (test_next(lex, '=')) {
        return fornum(lex, ivar);
    } else if (!test_next(lex, TK_IN)) {
        expected_symbol(lex, "'=' or 'in'"); // no return
    }
    struct AstStmt *stmt = forin(lex, ivar);
    return stmt;
}

static struct AstStmt *while_stmt(struct Lex *lex)
{
    struct AstStmt *r = NEW_STMT(lex, kAstWhileStmt);
    skip(lex); // 'while' token
    r->while_.cond = basic_expr(lex);
    r->while_.block = block(lex);
    return r;
}

static struct AstStmt *dowhile_stmt(struct Lex *lex)
{
    struct AstStmt *r = NEW_STMT(lex, kAstWhileStmt);
    r->while_.is_dowhile = PAW_TRUE;
    skip(lex); // 'do' token
    r->while_.block = block(lex);
    check_next(lex, TK_WHILE);
    r->while_.cond = basic_expr(lex);
    semicolon(lex);
    return r;
}

static struct AstStmt *return_stmt(struct Lex *lex)
{
    struct AstStmt *result = NEW_STMT(lex, kAstReturnStmt);
    struct AstReturnStmt *r = AstGetReturnStmt(result);
    skip(lex); // 'return' token
    if (!test_next(lex, ';')) {
        r->expr = expr0(lex);
        semicolon(lex);
    }
    return result;
}

static struct AstStmt *label_stmt(struct Lex *lex, enum LabelKind kind)
{
    struct AstStmt *r = NEW_STMT(lex, kAstLabelStmt);
    skip(lex); // 'break' or 'continue' token
    r->label.label = kind;
    semicolon(lex);
    return r;
}

static struct AstDeclList *type_param(struct Lex *lex)
{
    const int line = lex->line;
    if (test_next(lex, '<')) {
        ++lex->expr_depth;
        struct AstDeclList *list = pawAst_decl_list_new(lex->ast);
        parse_generic_list(lex, list, line);
        --lex->expr_depth;
        if (list->count == 0) {
            pawX_error(lex, "expected at least 1 generic parameter");
        }
        return list;
    }
    return NULL;
}

static struct AstDecl *function(struct Lex *lex, String *name, enum FuncKind kind)
{
    struct AstDecl *r = NEW_DECL(lex, kAstFuncDecl);
    r->func.name = name;
    r->func.fn_kind = kind;
    r->func.generics = type_param(lex);
    r->func.params = func_parameters(lex);
    r->func.result = test_next(lex, TK_ARROW) 
        ? type_expr(lex) 
        : unit_type(lex);
    r->func.body = lex->in_prelude
        ? NULL
        : block(lex);
    return r;
}

static struct AstDecl *func_decl(struct Lex *lex, paw_Bool pub)
{
    const int line = lex->line;
    skip(lex); // 'fn' token
    String *name = parse_name(lex);
    struct AstDecl *r = function(lex, name, FUNC_FUNCTION);
    r->func.is_pub = pub;
    r->func.line = line;
    return r;
}

static struct AstDecl *variant_decl(struct Lex *lex, int index)
{
    struct AstDecl *r = NEW_DECL(lex, kAstVariantDecl);
    r->variant.name = parse_name(lex);
    r->variant.index = index;

    const int line = lex->line;
    if (test_next(lex, '(')) {
        r->variant.fields = vfield_list(lex, line);
    }
    return r;
}

static void parse_variant_list(struct Lex *lex, struct AstDeclList *list, int line)
{
    do {
        if (test(lex, '}')) break;
        if (list->count == LOCAL_MAX) {
            limit_error(lex, "variants", LOCAL_MAX);
        }
        // NOTE: 'variant_decl' requires a second argument, so 'DEFINE_LIST_PARSER'
        //       cannot be used as-is.
        struct AstDecl *next = variant_decl(lex, list->count);
        pawAst_decl_list_push(lex->ast, list, next);
    } while (test_next(lex, ','));
    delim_next(lex, '}', '{', line);
}

static void enum_body(struct Lex *lex, struct AstAdtDecl *adt)
{
    const int line = lex->line;
    skip(lex);

    ++lex->expr_depth;
    adt->fields = pawAst_decl_list_new(lex->ast);
    parse_variant_list(lex, adt->fields, line);
    if (adt->fields->count == 0) {
        pawX_error(lex, "expected at least 1 enum variant between curly braces "
                        "(remove curly braces for unit enumeration)");
    }
    --lex->expr_depth;
}

static struct AstDecl *enum_decl(struct Lex *lex, paw_Bool pub)
{
    skip(lex); // 'enum' token
    struct AstDecl *r = NEW_DECL(lex, kAstAdtDecl);
    r->adt.is_pub = pub;
    r->adt.is_struct = PAW_FALSE;
    r->adt.name = parse_name(lex);
    r->adt.generics = type_param(lex);
    if (test(lex, '{')) {
        enum_body(lex, &r->adt);
    } else {
        semicolon(lex);
    }
    return r;
}

static struct AstDecl *field_decl(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstFieldDecl);
    r->field.name = parse_name(lex);
    r->field.tag = expect_annotation(lex, "field", r->field.name);
    return r;
}

DEFINE_LIST_PARSER(sfield, '{', '}', LOCAL_MAX, "struct fields", field_decl, pawAst_decl_list_, AstDeclList)

static struct AstDeclList *sfield_list(struct Lex *lex, int line)
{
    ++lex->expr_depth;
    struct AstDeclList *list = pawAst_decl_list_new(lex->ast);
    parse_sfield_list(lex, list, line);
    if (list->count == 0) {
        pawX_error(lex, "expected at least 1 struct field between curly braces "
                        "(remove curly braces for unit structure)");
    }
    --lex->expr_depth;
    return list;
}

static void struct_body(struct Lex *lex, struct AstAdtDecl *adt)
{
    const int line = lex->line;
    if (test_next(lex, '{')) {
        adt->fields = sfield_list(lex, line);
    } else {
        semicolon(lex);
    }
}

static struct AstDecl *struct_decl(struct Lex *lex, paw_Bool pub)
{
    skip(lex); // 'struct' token
    struct AstDecl *r = NEW_DECL(lex, kAstAdtDecl);
    r->adt.is_pub = pub;
    r->adt.is_struct = PAW_TRUE;
    r->adt.name = parse_name(lex);
    r->adt.generics = type_param(lex);
    struct_body(lex, &r->adt);
    return r;
}

static struct AstDecl *type_decl(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstTypeDecl);
    skip(lex); // 'type' token

    r->type.name = parse_name(lex);
    r->type.generics = type_param(lex);

    check_next(lex, '=');

    // 'type_expr()' parses function signatures, which are not allowed
    // on the RHS of a type expression. This should be caught during
    // type checking, since we also need to make sure the RHS is not
    // referring to an uninstantiated template.
    r->type.rhs = type_expr(lex);
    return r;
}

static struct AstDecl *decl(struct Lex *lex)
{
    if (lex->t.kind == TK_LET) {
        return let_decl(lex, lex->line, PAW_FALSE);
    } else if (lex->t.kind == TK_TYPE) {
        return type_decl(lex);
    } else {
        pawX_error(lex, "expected 'let' or 'type' declaration");
    }
}

static struct AstStmt *decl_stmt(struct Lex *lex)
{
    struct AstStmt *r = NEW_STMT(lex, kAstDeclStmt);
    r->decl.decl = decl(lex);
    semicolon(lex);
    return r;
}

static struct AstStmt *statement(struct Lex *lex)
{
    enter_nested(lex);
    struct AstStmt *stmt;
    switch (lex->t.kind) {
        case ';':
            skip(lex); // ';' token
            stmt = NULL;
            break;
        case '{':
            stmt = AST_CAST_STMT(block(lex));
            break;
        case TK_IF:
            stmt = if_stmt(lex);
            break;
        case TK_FOR:
            stmt = for_stmt(lex);
            break;
        case TK_WHILE:
            stmt = while_stmt(lex);
            break;
        case TK_DO:
            stmt = dowhile_stmt(lex);
            break;
        case TK_RETURN:
            stmt = return_stmt(lex);
            break;
        case TK_BREAK:
            stmt = label_stmt(lex, LBREAK);
            break;
        case TK_CONTINUE:
            stmt = label_stmt(lex, LCONTINUE);
            break;
        case TK_LET:
        case TK_TYPE:
            stmt = decl_stmt(lex);
            break;
        default:
            stmt = expr_stmt(lex);
    }
    leave_nested(lex);
    return stmt;
}

static struct AstDeclList *toplevel_items(struct Lex *lex, struct AstDeclList *list)
{
    while (!test_next(lex, TK_END)) {
        struct AstDecl *item;
        const paw_Bool is_pub = test_next(lex, TK_PUB);
        switch (lex->t.kind) {
            case TK_FN:
                item = func_decl(lex, is_pub);
                break;
            case TK_ENUM:
                item = enum_decl(lex, is_pub);
                break;
            case TK_STRUCT:
                item = struct_decl(lex, is_pub);
                break;
            default:
                pawX_error(lex, "expected toplevel item");
        }
        pawAst_decl_list_push(lex->ast, list, item);
    }
    return list;
}

// TODO: Use the C API instead?
static const char kPrelude[] =
    "pub struct _List<T>;\n"
    "pub struct _Map<K, V>;\n"

    "pub enum Option<T> {\n"
    "    Some(T),        \n"
    "    None,           \n"
    "}                   \n"

    "pub enum Result<T, E> {\n"
    "    Ok(T),             \n"
    "    Err(E),            \n"
    "}                      \n"

    "pub fn print(message: str)\n"
    "pub fn assert(cond: bool) \n"

    // TODO: Replace with methods
    "pub fn _int_to_string(self: int) -> str\n"
    "pub fn _string_parse_int(self: str, base: int) -> int     \n"
    "pub fn _string_parse_float(self: str) -> float            \n"
    "pub fn _string_split(self: str, sep: str) -> [str]        \n"
    "pub fn _string_join(self: str, seq: [str]) -> str         \n"
    "pub fn _string_find(self: str, target: str) -> int        \n"
    "pub fn _string_starts_with(self: str, prefix: str) -> bool\n"
    "pub fn _string_ends_with(self: str, suffix: str) -> bool  \n"
    "pub fn _list_push<T>(self: [T], v: T)          \n"
    "pub fn _list_pop<T>(self: [T]) -> T            \n"
    "pub fn _list_insert<T>(self: [T], i: int, v: T)\n"
    "pub fn _list_erase<T>(self: [T], i: int) -> T  \n"
    "pub fn _list_clone<T>(self: [T]) -> [T]        \n";

struct PreludeReader {
    size_t size;
};

const char *prelude_reader(paw_Env *P, void *ud, size_t *size)
{
    paw_unused(P);
    struct PreludeReader *pr = ud;
    *size = pr->size;
    pr->size = 0;
    return kPrelude;
}

static void load_prelude(struct Lex *lex)
{
    struct Ast *ast = lex->ast;
    struct PreludeReader reader = {paw_lengthof(kPrelude)};
    pawX_set_source(lex, prelude_reader, &reader);
    lex->in_prelude = PAW_TRUE;
    toplevel_items(lex, ast->prelude);
    lex->in_prelude = PAW_FALSE;
    check(lex, TK_END);
}

// All paw language keywords
//
// ORDER TokenKind
static const char *kKeywords[] = {
    "pub",
    "fn",
    "type",
    "enum",
    "struct",
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
    "as",
    "true",
    "false",
};

static String *basic_type_name(paw_Env *P, const char *name, paw_Type code)
{
    String *s = pawS_new_fixed(P, name);
    s->flag = FLAG2CODE(code); // works either direction
    return s;
}

void pawP_init(paw_Env *P)
{
    // Add all keywords to the interned strings table. Fix them so they are
    // never collected. Also added to the lexer string map.
    for (size_t i = 0; i < paw_countof(kKeywords); ++i) {
        const char *kw = kKeywords[i];
        String *str = pawS_new_fixed(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }
    P->str_cache[CSTR_SELF] = pawS_new_str(P, "self");
    P->str_cache[CSTR_TRUE] = pawS_new_str(P, "true");
    P->str_cache[CSTR_FALSE] = pawS_new_str(P, "false");
    P->str_cache[CSTR_BOOL] = basic_type_name(P, "bool", PAW_TBOOL);
    P->str_cache[CSTR_INT] = basic_type_name(P, "int", PAW_TINT);
    P->str_cache[CSTR_FLOAT] = basic_type_name(P, "float", PAW_TFLOAT);
    P->str_cache[CSTR_STRING] = basic_type_name(P, "str", PAW_TSTRING);
    P->str_cache[CSTR_LIST] = basic_type_name(P, "_List", PAW_TLIST);
    P->str_cache[CSTR_MAP] = basic_type_name(P, "_Map", PAW_TMAP);
}

static void skip_hashbang(struct Lex *lex)
{
    if (test_next(lex, '#') && test_next(lex, '!')) {
        while (!test(lex, TK_END)) {
            const char c = lex->c;
            skip(lex); // skip line
            if (ISNEWLINE(c)) break;
        }
    }
}

static struct Ast *parse_module(struct Lex *lex, paw_Reader input, void *ud)
{
    load_prelude(lex);

    pawX_set_source(lex, input, ud);
    skip_hashbang(lex);

    struct Ast *ast = lex->ast;
    ast->items = pawAst_decl_list_new(ast);
    toplevel_items(lex, ast->items);
    check(lex, TK_END);
    return ast;
}

struct Ast *pawP_parse(struct Compiler *C, paw_Reader input, void *ud)
{
    struct Lex lex = {
        .modname = C->modname,
        .dm = C->dm,
        .P = C->P,
    };
    lex.ast = C->dm->ast;
    lex.strings = C->strings;
    lex.dm->ast = lex.ast;

    // convert source to AST
    parse_module(&lex, input, ud);
    return lex.dm->ast;
}
