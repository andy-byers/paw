// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "parse.h"
#include "ast.h"
#include "compile.h"
#include "env.h"
#include "map.h"

#define SELF_TYPENAME(lex) SCAN_STRING((lex)->C, "Self")
#define SELF_VARNAME(lex) CACHED_STRING(ENV(lex), CSTR_SELF)

static String *unpack_name(struct AstExpr *expr)
{
    if (!AstIsPathExpr(expr))
        return NULL;
    struct AstPath *path = AstGetPathExpr(expr)->path;
    if (path->count == 1) {
        struct AstSegment ps = K_LIST_GET(path, 0);
        if (ps.types == NULL)
            return ps.name;
    }
    return NULL;
}

// recursive non-terminals
static struct AstExpr *expression(struct Lex *lex, unsigned prec);
static struct AstStmt *statement(struct Lex *lex);
static struct AstPat *pattern(struct Lex *lex);

static struct AstExpr *expr0(struct Lex *lex)
{
    return expression(lex, 0);
}

static void expected_symbol(struct Lex *lex, char const *want)
{
    pawX_error(lex, "expected %s", want);
}

static void missing_delim(struct Lex *lex, TokenKind want, TokenKind open, int open_line)
{
    pawX_error(lex, "expected '%c' to match '%c' on line %d", want, open, open_line);
}

static void delim_next(struct Lex *lex, TokenKind want, TokenKind open, int open_line)
{
    TokenKind const have = lex->t.kind;
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
    int const max_nesting = 1000;
    if (lex->nest_depth > max_nesting) {
        pawX_error(lex, "exceeded maximum nesting depth");
    }
}

static void leave_nested(struct Lex *lex)
{
    paw_assert(lex->nest_depth >= 0);
    --lex->nest_depth;
}

static paw_Bool equals_cstr(struct Lex *lex, String const *ident, unsigned cstr)
{
    return pawS_eq(ident, CACHED_STRING(ENV(lex), cstr));
}

static paw_Bool is_underscore(struct Lex *lex, String const *ident)
{
    return equals_cstr(lex, ident, CSTR_UNDERSCORE);
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
    INFIX_AS, // as

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

static uint8_t const kUnOpPrecedence = 13;

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

static void check(struct Lex *lex, TokenKind want)
{
    if (!test(lex, want)) {
        pawX_error(lex, "unexpected symbol");
    }
}

static void check_next(struct Lex *lex, TokenKind want)
{
    check(lex, want);
    skip(lex);
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

static void ensure_not_reserved(struct Lex *lex, String const *name)
{
    if (is_underscore(lex, name)) {
        SYNTAX_ERROR(lex->C, "'%s' is a reserved identifier", name->text);
    }
}

static String *parse_toplevel_name(struct Lex *lex)
{
    String *name = parse_name(lex);
    ensure_not_reserved(lex, name);
    return name;
}

static struct AstExpr *new_basic_lit(struct Lex *lex, Value value, paw_Type code)
{
    int const line = lex->line;
    return pawAst_new_basic_lit(lex->ast, line, value, code);
}

static struct AstExpr *unit_lit(struct Lex *lex)
{
    return new_basic_lit(lex, I2V(0), BUILTIN_UNIT);
}

static struct AstType *unit_type(struct Lex *lex)
{
    struct AstTypeList *types = pawAst_type_list_new(lex->C);
    return pawAst_new_tuple_type(lex->ast, lex->line, types);
}

static struct AstExpr *emit_bool(struct Lex *lex, paw_Bool b)
{
    Value v;
    V_SET_BOOL(&v, b);
    return new_basic_lit(lex, v, BUILTIN_BOOL);
}

static struct AstType *parse_type(struct Lex *lex, paw_Bool is_strict);
static struct AstType *parse_strict_type(struct Lex *lex);
static struct AstType *parse_relaxed_type(struct Lex *lex);

static struct AstDecl *variant_field_decl(struct Lex *lex)
{
    int const line = lex->line;
    struct AstType *tag = parse_type(lex, PAW_TRUE);
    return pawAst_new_field_decl(lex->ast, line, NULL, tag, PAW_FALSE);
}

#define DEFINE_LIST_PARSER(name, a, b, limit, what, func, L)                   \
    static void parse_##name##_list(struct Lex *lex, struct L *list, int line) \
    {                                                                          \
        do {                                                                   \
            if (test(lex, b))                                                  \
                break;                                                         \
            if ((list)->count == (limit)) {                                    \
                limit_error(lex, what, (limit));                               \
            }                                                                  \
            K_LIST_PUSH((lex)->C, list, (func)(lex));                          \
        } while (test_next(lex, ','));                                         \
        delim_next(lex, b, a, line);                                           \
    }
DEFINE_LIST_PARSER(arg, '(', ')', LOCAL_MAX, "arguments", expr0, AstExprList)
DEFINE_LIST_PARSER(variant_field, '(', ')', LOCAL_MAX, "variant fields", variant_field_decl, AstDeclList)
DEFINE_LIST_PARSER(strict_type, '<', '>', LOCAL_MAX, "type arguments", parse_strict_type, AstTypeList)
DEFINE_LIST_PARSER(relaxed_type, '<', '>', LOCAL_MAX, "type arguments", parse_relaxed_type, AstTypeList)

static struct AstTypeList *strict_type_list(struct Lex *lex, int line)
{
    ++lex->expr_depth;
    struct AstTypeList *list = pawAst_type_list_new(lex->C);
    parse_strict_type_list(lex, list, line);
    if (list->count == 0) {
        pawX_error(lex, "expected at least 1 type");
    }
    --lex->expr_depth;
    return list;
}

static struct AstTypeList *relaxed_type_list(struct Lex *lex, int line)
{
    ++lex->expr_depth;
    struct AstTypeList *list = pawAst_type_list_new(lex->C);
    parse_relaxed_type_list(lex, list, line);
    if (list->count == 0) {
        pawX_error(lex, "expected at least 1 type");
    }
    --lex->expr_depth;
    return list;
}

static struct AstTypeList *maybe_strict_type_args(struct Lex *lex)
{
    return test_next(lex, '<') ? strict_type_list(lex, lex->line) : NULL;
}

static struct AstTypeList *maybe_relaxed_type_args(struct Lex *lex)
{
    return test_next(lex, '<') ? relaxed_type_list(lex, lex->line) : NULL;
}

static struct AstPath *parse_pathexpr(struct Lex *lex)
{
    struct AstPath *p = pawAst_path_new(lex->C);
    do {
    next_segment:
        if (p->count == LOCAL_MAX) {
            limit_error(lex, "path segments", LOCAL_MAX);
        }
        String *name = parse_name(lex);
        struct AstTypeList *args = NULL;
        // permit "::<types..>" between segments
        if (test_next(lex, TK_COLON2)) {
            ensure_not_reserved(lex, name);
            args = maybe_relaxed_type_args(lex);
            if (args == NULL) {
                pawAst_path_add(lex->C, p, name, NULL);
                goto next_segment;
            }
        }
        pawAst_path_add(lex->C, p, name, args);
    } while (test_next(lex, TK_COLON2));
    return p;
}

static struct AstPath *parse_pathtype(struct Lex *lex, paw_Bool is_strict)
{
    struct AstPath *p = pawAst_path_new(lex->C);
    do {
        if (p->count == LOCAL_MAX) {
            limit_error(lex, "path segments", LOCAL_MAX);
        }
        String *name = parse_name(lex);
        if (is_strict) {
            ensure_not_reserved(lex, name);
        }
        int const line = lex->line;
        struct AstTypeList *args = NULL;
        if (test_next(lex, '<')) {
            // _<types..> is not allowed
            ensure_not_reserved(lex, name);
            args = is_strict
                       ? strict_type_list(lex, line)
                       : relaxed_type_list(lex, line);
        }
        pawAst_path_add(lex->C, p, name, args);
    } while (test_next(lex, TK_COLON2));
    return p;
}

static struct AstExpr *path_expr(struct Lex *lex)
{
    int const line = lex->line;
    struct AstPath *path = parse_pathexpr(lex);
    return pawAst_new_path_expr(lex->ast, line, path);
}

static struct AstPat *new_path_pat(struct Lex *lex, String *name)
{
    int const line = lex->line;
    struct AstPath *path = pawAst_path_new(lex->C);
    pawAst_path_add(lex->C, path, name, NULL);
    return pawAst_new_path_pat(lex->ast, line, path);
}

static struct AstPat *struct_field_pat(struct Lex *lex)
{
    int const line = lex->line;
    String *name = parse_toplevel_name(lex);
    struct AstPat *pat;
    if (test_next(lex, ':')) {
        pat = pattern(lex);
    } else {
        // binds field to variable of same name
        pat = new_path_pat(lex, name);
    }
    return pawAst_new_field_pat(lex->ast, line, name, pat);
}

DEFINE_LIST_PARSER(variant_field_pat, '(', ')', LOCAL_MAX, "variant fields", pattern, AstPatList)
DEFINE_LIST_PARSER(struct_field_pat, '{', '}', LOCAL_MAX, "struct fields", struct_field_pat, AstPatList)

static paw_Bool is_wildcard_path(struct AstPath const *path)
{
    paw_assert(path->count > 0);
    if (path->count > 1)
        return PAW_FALSE;
    struct AstSegment seg = K_LIST_GET(path, 0);
    return pawS_length(seg.name) == 1 &&
           seg.name->text[0] == '_';
}

static struct AstPat *compound_pat(struct Lex *lex)
{
    int const line = lex->line;
    struct AstPath *path = parse_pathexpr(lex);
    if (test_next(lex, '(')) {
        struct AstPatList *fields = pawAst_pat_list_new(lex->C);
        parse_variant_field_pat_list(lex, fields, line);
        return pawAst_new_variant_pat(lex->ast, line, path, fields);
    } else if (test_next(lex, '{')) {
        struct AstPatList *fields = pawAst_pat_list_new(lex->C);
        parse_struct_field_pat_list(lex, fields, line);
        return pawAst_new_struct_pat(lex->ast, line, path, fields);
    } else if (is_wildcard_path(path)) {
        return pawAst_new_wildcard_pat(lex->ast, line);
    } else {
        return pawAst_new_path_pat(lex->ast, line, path);
    }
}

static struct AstPat *tuple_pat(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // '(' token
    struct AstPatList *elems = pawAst_pat_list_new(lex->C);
    parse_variant_field_pat_list(lex, elems, line);
    return pawAst_new_tuple_pat(lex->ast, line, elems);
}

static struct AstExpr *literal_expr(struct Lex *lex)
{
    paw_Bool const negative = test_next(lex, '-');

    struct AstExpr *expr;
    switch (lex->t.kind) {
        case TK_TRUE:
            expr = emit_bool(lex, PAW_TRUE);
            break;
        case TK_FALSE:
            expr = emit_bool(lex, PAW_FALSE);
            break;
        case TK_INTEGER:
            expr = new_basic_lit(lex, lex->t.value, BUILTIN_INT);
            break;
        case TK_FLOAT:
            expr = new_basic_lit(lex, lex->t.value, BUILTIN_FLOAT);
            break;
        case TK_STRING: {
            Value const v = lex->t.value;
            expr = new_basic_lit(lex, v, BUILTIN_STR);
            break;
        }
        default:
            pawX_error(lex, "expected literal pattern");
    }
    skip(lex); // literal token

    if (negative) {
        struct AstLiteralExpr *e = AstGetLiteralExpr(expr);
        if (e->basic.code == BUILTIN_INT) {
            if (e->basic.value.i == PAW_INT_MIN) {
                pawX_error(lex, "signed integer overflow ('-' applied to %I)", PAW_INT_MIN);
            }
            e->basic.value.i = -e->basic.value.i;
        } else if (e->basic.code == BUILTIN_FLOAT) {
            e->basic.value.f = -e->basic.value.f;
        } else {
            pawX_error(lex, "operator '-' applied to non-numeric value");
        }
    }
    return expr;
}

static struct AstPat *literal_pat(struct Lex *lex)
{
    int const line = lex->line;
    struct AstExpr *expr = literal_expr(lex);

    if (!AstIsLiteralExpr(expr)) {
        pawX_error(lex, "expected literal pattern");
    }
    return pawAst_new_literal_pat(lex->ast, line, expr);
}

static struct AstPat *or_pat(struct Lex *lex, struct AstPat *lhs)
{
    int const line = lex->line;
    struct AstPat *rhs = pattern(lex);
    return pawAst_new_or_pat(lex->ast, line, lhs, rhs);
}

static struct AstPat *pattern(struct Lex *lex)
{
    struct AstPat *pat;
    switch (lex->t.kind) {
        case TK_NAME:
            pat = compound_pat(lex);
            break;
        case '(':
            pat = tuple_pat(lex);
            break;
        default:
            pat = literal_pat(lex);
    }
    if (!test_next(lex, '|'))
        return pat;
    return or_pat(lex, pat);
}

static struct AstExpr *basic_expr(struct Lex *lex);

static struct AstDeclList *variant_field_list(struct Lex *lex, int line)
{
    ++lex->expr_depth;
    struct AstDeclList *list = pawAst_decl_list_new(lex->C);
    parse_variant_field_list(lex, list, line);
    if (list->count == 0) {
        pawX_error(lex, "expected at least 1 variant field between parenthesis "
                        "(remove parenthesis for unit variant)");
    }
    --lex->expr_depth;
    return list;
}

static struct AstType *parse_tuple_type(struct Lex *lex, struct AstType *first, int line, paw_Bool is_strict)
{
    struct AstTypeList *elems = pawAst_type_list_new(lex->C);
    K_LIST_PUSH(lex->C, elems, first);

    do {
        if (test(lex, ')'))
            break;
        if (elems->count == FIELD_MAX) {
            limit_error(lex, "tuple elements", FIELD_MAX);
        }
        struct AstType *type = parse_type(lex, is_strict);
        K_LIST_PUSH(lex->C, elems, type);
    } while (test_next(lex, ','));
    delim_next(lex, ')', '(', line);

    return pawAst_new_tuple_type(lex->ast, line, elems);
}

static struct AstType *parse_paren_type(struct Lex *lex, paw_Bool is_strict)
{
    int const line = lex->last_line;
    if (test_next(lex, ')')) {
        return unit_type(lex);
    }
    struct AstType *t = parse_type(lex, is_strict);
    if (test_next(lex, ',')) {
        return parse_tuple_type(lex, t, line, is_strict);
    }
    delim_next(lex, ')', '(', line);
    return t;
}

static struct AstType *parse_container_type(struct Lex *lex, paw_Bool is_strict)
{
    int const line = lex->last_line;
    struct AstType *first = parse_type(lex, is_strict);
    struct AstType *second = NULL;
    if (test_next(lex, ':')) {
        second = parse_type(lex, is_strict);
    }
    delim_next(lex, ']', '[', line);
    return pawAst_new_container_type(lex->ast, line, first, second);
}

static struct AstType *parse_signature(struct Lex *lex);

static struct AstType *parse_relaxed_type(struct Lex *lex)
{
    return parse_type(lex, PAW_FALSE);
}

static struct AstType *parse_strict_type(struct Lex *lex)
{
    return parse_type(lex, PAW_TRUE);
}

static struct AstType *parse_type(struct Lex *lex, paw_Bool is_strict)
{
    if (test_next(lex, '(')) {
        return parse_paren_type(lex, is_strict);
    } else if (test_next(lex, '[')) {
        return parse_container_type(lex, is_strict);
    } else if (test_next(lex, TK_FN)) {
        return parse_signature(lex);
    }
    int const line = lex->line;
    struct AstPath *path = parse_pathtype(lex, is_strict);
    if (is_underscore(lex, K_LIST_FIRST(path).name)) {
        if (path->count > 1)
            SYNTAX_ERROR(lex, "unexpected '::' after '_'");
        return pawAst_new_infer_type(lex->ast, line);
    }
    return pawAst_new_path_type(lex->ast, line, path);
}

static struct AstType *type_annotation(struct Lex *lex, paw_Bool is_strict)
{
    if (test_next(lex, ':')) {
        struct AstType *t = parse_type(lex, is_strict);
        if (t == NULL) {
            pawX_error(lex, "invalid type annotation");
        }
        return t;
    }
    return NULL; // needs inference
}

static struct AstType *expect_annotation(struct Lex *lex, char const *what, String const *name, paw_Bool is_strict)
{
    struct AstType *type = type_annotation(lex, is_strict);
    if (type == NULL) {
        pawX_error(lex, "expected type annotation on %s '%s'", what, name->text);
    }
    return type;
}

static struct AstType *self_type(struct Lex *lex)
{
    int const line = lex->line;
    struct AstPath *path = pawAst_path_new(lex->C);
    pawAst_path_add(lex->C, path, SELF_TYPENAME(lex), NULL);
    return pawAst_new_path_type(lex->ast, line, path);
}

static struct AstDecl *func_param_decl(struct Lex *lex)
{
    int const line = lex->line;
    String *name = parse_name(lex);
    int const param_index = lex->param_index++;
    struct AstType *tag;
    if (!lex->in_impl || param_index != 0) {
        // usual case: expect a type annotation on each parameter
        tag = expect_annotation(lex, "parameter", name, PAW_TRUE);
    } else {
        // first parameter to method: 'self' means 'self: Self'
        tag = type_annotation(lex, PAW_TRUE);
        if (tag == NULL) {
            if (!pawS_eq(name, SELF_VARNAME(lex))) {
                pawX_error(lex, "expected parameter named 'self' but found '%s'", name->text);
            }
            tag = self_type(lex);
        }
    }
    return pawAst_new_field_decl(lex->ast, line, name, tag, PAW_FALSE);
}

static struct AstDecl *closure_param_decl(struct Lex *lex)
{
    int const line = lex->line;
    String *name = parse_name(lex);
    struct AstType *tag = type_annotation(lex, PAW_FALSE);
    return pawAst_new_field_decl(lex->ast, line, name, tag, PAW_FALSE);
}

static struct AstDecl *let_decl(struct Lex *lex, int line)
{
    skip(lex); // 'let' token
    String *name = parse_name(lex);
    struct AstType *tag = type_annotation(lex, PAW_FALSE);
    struct AstExpr *init = test_next(lex, '=') ? expr0(lex) : NULL;
    semicolon(lex);
    return pawAst_new_var_decl(lex->ast, line, name, tag, init);
}

static struct AstBoundList *parse_generic_bounds(struct Lex *lex)
{
    if (!test_next(lex, ':'))
        return NULL;
    struct AstBoundList *bounds = pawAst_bound_list_new(lex->C);
    do {
        struct AstGenericBound bound;
        bound.path = parse_pathtype(lex, PAW_TRUE);
        K_LIST_PUSH(lex->C, bounds, bound);
    } while (test_next(lex, '+'));
    return bounds;
}

static struct AstDecl *generic_param(struct Lex *lex)
{
    int const line = lex->line;
    String *name = parse_toplevel_name(lex);
    struct AstBoundList *bounds = parse_generic_bounds(lex);
    return pawAst_new_generic_decl(lex->ast, line, name, bounds);
}

DEFINE_LIST_PARSER(func_param, '(', ')', LOCAL_MAX, "function parameters", func_param_decl, AstDeclList)
DEFINE_LIST_PARSER(sig_param, '(', ')', LOCAL_MAX, "function parameters", parse_strict_type, AstTypeList)
DEFINE_LIST_PARSER(closure_param, '|', '|', LOCAL_MAX, "closure parameters", closure_param_decl, AstDeclList)
DEFINE_LIST_PARSER(generic, '<', '>', LOCAL_MAX, "generics", generic_param, AstDeclList)

static struct AstExpr *sitem_expr(struct Lex *lex)
{
    int const line = lex->line;
    String *name = parse_toplevel_name(lex);
    struct AstExpr *value;
    if (test_next(lex, ':')) {
        value = expr0(lex);
    } else {
        // "name" by itself is shorthand for "name: name"
        struct AstPath *path = pawAst_path_new(lex->C);
        pawAst_path_add(lex->C, path, name, NULL);
        value = pawAst_new_path_expr(lex->ast, line, path);
    }
    int const fid = INT_MAX; // nonnegative means determine later
    return pawAst_new_named_field_expr(lex->ast, line, name, value, fid);
}

DEFINE_LIST_PARSER(sitem, '{', '}', LOCAL_MAX, "struct items", sitem_expr, AstExprList)

static struct AstExpr *unop_expr(struct Lex *lex, enum UnOp op)
{
    int const line = lex->line;
    enter_nested(lex);
    skip(lex); // unary operator token
    enum UnaryOp unop = CAST(enum UnaryOp, op); // same order
    struct AstExpr *target = expression(lex, kUnOpPrecedence);
    leave_nested(lex);
    return pawAst_new_unop_expr(lex->ast, line, unop, target);
}

// Parse either a parenthsized expression or a tuple
static struct AstExpr *paren_expr(struct Lex *lex)
{
    // Just parse and return the expression contained within the parenthesis.
    // There is no need for an extra node type.
    int const line = lex->line;
    skip(lex); // '(' token
    if (test_next(lex, ')')) {
        return unit_lit(lex);
    }
    ++lex->expr_depth;
    struct AstExpr *expr = expr0(lex);
    --lex->expr_depth;
    if (test_next(lex, ')')) {
        return pawAst_new_paren_expr(lex->ast, line, expr);
    }
    check_next(lex, ',');
    struct AstExprList *elems = pawAst_expr_list_new(lex->C);
    K_LIST_PUSH(lex->C, elems, expr);
    parse_arg_list(lex, elems, line);
    return pawAst_new_tuple_lit(lex->ast, line, elems);
}

static struct AstType *parse_signature(struct Lex *lex)
{
    int const line = lex->last_line;
    check_next(lex, '(');
    struct AstTypeList *params = pawAst_type_list_new(lex->C);
    if (!test_next(lex, ')')) {
        parse_sig_param_list(lex, params, line);
    }
    struct AstType *result;
    if (test_next(lex, TK_ARROW)) {
        result = parse_type(lex, PAW_TRUE);
    } else {
        result = unit_type(lex);
    }
    return pawAst_new_func_type(lex->ast, line, params, result);
}

static paw_Bool end_of_block(struct Lex *lex)
{
    return test(lex, '}') || // found proper end
           test(lex, TK_END); // truncated block
}

static struct AstExpr *index_expr(struct Lex *lex, struct AstExpr *target)
{
    int const line = lex->line;
    skip(lex); // '[' token
    struct AstExpr *first = NULL;
    if (!test(lex, ':')) {
        first = expr0(lex);
    }
    paw_Bool is_slice = PAW_FALSE;
    struct AstExpr *second = NULL;
    if (test_next(lex, ':')) {
        is_slice = PAW_TRUE;
        if (!test(lex, ']')) {
            second = expr0(lex);
        }
    }
    delim_next(lex, ']', '[', line);
    return pawAst_new_index(lex->ast, line, target, first, second, is_slice);
}

static paw_Type parse_container_items(struct Lex *lex, struct AstExprList *items)
{
    int const line = lex->line;
    enum BuiltinKind code = BUILTIN_UNIT;
    do {
        if (test(lex, ']'))
            break;
        if (items->count == LOCAL_MAX) {
            limit_error(lex, "container literal items", LOCAL_MAX);
        }
        struct AstExpr *item = expr0(lex);
        if (!test_next(lex, ':')) {
            if (code == BUILTIN_MAP) {
                pawX_error(lex, "expected ':' after map key");
            }
            code = BUILTIN_LIST;
        } else if (code == BUILTIN_LIST) {
            pawX_error(lex, "unexpected ':' in list literal");
        } else {
            code = BUILTIN_MAP;
            struct AstExpr *value = expr0(lex);
            item = pawAst_new_keyed_field_expr(lex->ast, line, item, value);
        }
        K_LIST_PUSH(lex->C, items, item);
    } while (test_next(lex, ','));
    // loop body is run at least once
    paw_assert(code != BUILTIN_UNIT);
    return code;
}

static struct AstExpr *container_lit(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // '[' token
    struct AstExprList *items = pawAst_expr_list_new(lex->C);
    enum BuiltinKind b_kind;
    if (test(lex, ']')) {
        b_kind = BUILTIN_LIST; // empty list: '[]'
    } else if (test_next(lex, ':')) {
        b_kind = BUILTIN_MAP; // empty map: '[:]'
    } else {
        b_kind = parse_container_items(lex, items);
    }
    delim_next(lex, ']', '[', line);
    return pawAst_new_container_lit(lex->ast, line, items, b_kind);
}

// Parse a composite literal expression
static struct AstExpr *composite_lit(struct Lex *lex, struct AstPathExpr *path)
{
    skip(lex); // '{' token
    struct AstExprList *items = pawAst_expr_list_new(lex->C);
    parse_sitem_list(lex, items, path->line);
    return pawAst_new_composite_lit(lex->ast, path->line, path->path, items);
}

static struct AstExpr *try_composite_lit(struct Lex *lex, struct AstExpr *expr)
{
    enter_nested(lex);
    if (AstIsPathExpr(expr) && lex->expr_depth >= 0) {
        expr = composite_lit(lex, AstGetPathExpr(expr));
    }
    leave_nested(lex);
    return expr;
}

static struct AstExpr *selector_expr(struct Lex *lex, struct AstExpr *target)
{
    int const line = lex->line;
    skip(lex); // '.' token
    if (test(lex, TK_NAME)) {
        String *name = parse_toplevel_name(lex);
        return pawAst_new_name_selector(lex->ast, line, target, name);
    } else if (test(lex, TK_INTEGER)) {
        int const index = V_INT(lex->t.value);
        skip(lex); // integer token
        return pawAst_new_index_selector(lex->ast, line, target, index);
    }

    pawX_error(lex, "expected identifier or integer after '.'");
}

static struct AstExpr *call_expr(struct Lex *lex, struct AstExpr *target)
{
    int const line = lex->line;
    skip(lex); // '(' token
    struct AstExprList *args = pawAst_expr_list_new(lex->C);
    parse_arg_list(lex, args, line);
    return pawAst_new_call_expr(lex->ast, line, target, args);
}

static struct AstExpr *chain_expr(struct Lex *lex, struct AstExpr *target)
{
    int const line = lex->line;
    skip(lex); // '?' token
    return pawAst_new_chain_expr(lex->ast, line, target);
}

static struct AstDeclList *func_parameters(struct Lex *lex)
{
    int const line = lex->line;
    check_next(lex, '(');
    struct AstDeclList *list = pawAst_decl_list_new(lex->C);
    lex->param_index = 0; // 'self' allowed if '.in_impl'
    parse_func_param_list(lex, list, line);
    return list;
}

static struct AstDeclList *closure_params(struct Lex *lex)
{
    int const line = lex->line;
    check_next(lex, '|');
    struct AstDeclList *list = pawAst_decl_list_new(lex->C);
    parse_closure_param_list(lex, list, line);
    return list;
}

static paw_Bool expects_semicolon(struct AstExpr *expr)
{
    switch (AST_KINDOF(expr)) {
        default:
            return PAW_TRUE;
        case kAstIfExpr:
        case kAstForExpr:
        case kAstWhileExpr:
        case kAstMatchExpr:
        case kAstBlock:
            return PAW_FALSE;
    }
}

static struct AstExpr *block_expr(struct Lex *lex)
{
    int const line = lex->line;
    check_next(lex, '{');
    struct AstStmtList *stmts = pawAst_stmt_list_new(lex->C);
    struct AstExpr *result = NULL;
    while (!end_of_block(lex)) {
        struct AstStmt *next = statement(lex);
        if (next == NULL)
            continue; // extra ';'
        K_LIST_PUSH(lex->C, stmts, next);
        if (AstIsExprStmt(next) && !test_next(lex, ';')) {
            struct AstExprStmt *s = AstGetExprStmt(next);
            if (expects_semicolon(s->expr) || test(lex, '}')) {
                K_LIST_POP(stmts);
                result = s->expr;
                break;
            }
        }
    }
    if (result == NULL)
        result = unit_lit(lex);
    delim_next(lex, '}', '{', line);
    return pawAst_new_block(lex->ast, line, stmts, result);
}

static struct AstExpr *closure(struct Lex *lex)
{
    int const line = lex->line;
    struct AstDeclList *params = closure_params(lex);
    struct AstType *result = NULL;
    struct AstExpr *expr;
    if (test_next(lex, TK_ARROW)) {
        result = parse_type(lex, PAW_FALSE);
        expr = block_expr(lex);
    } else {
        expr = expr0(lex);
    }
    return pawAst_new_closure_expr(lex->ast, line, params, result, expr);
}

static struct AstExpr *if_expr(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // 'if' token
    struct AstExpr *cond = basic_expr(lex);
    struct AstExpr *then_arm = block_expr(lex);
    struct AstExpr *else_arm = NULL;
    if (test_next(lex, TK_ELSE)) {
        // transform "else if" construct:
        //   before: "if a {A} else if b {B} else {C}"
        //    after: "if a {A} else {if b {B} else {C}}"
        else_arm = test(lex, TK_IF) ? if_expr(lex) : block_expr(lex);
    }
    return pawAst_new_if_expr(lex->ast, line, cond, then_arm, else_arm);
}

static struct AstExpr *for_expr(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // 'for' token
    String *name = parse_name(lex);
    check_next(lex, TK_IN);
    struct AstExpr *target = basic_expr(lex);
    struct AstExpr *block = block_expr(lex);
    return pawAst_new_for_expr(lex->ast, line, name, target, block);
}

static struct AstExpr *while_expr(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // 'while' token
    struct AstExpr *cond = basic_expr(lex);
    struct AstExpr *block = block_expr(lex);
    return pawAst_new_while_expr(lex->ast, line, cond, block);
}

static struct AstExpr *return_expr(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // 'return' token
    struct AstExpr *expr = NULL;
    if (!test(lex, '}') && !test(lex, ';') && !test(lex, ',')) {
        expr = expr0(lex);
    }
    return pawAst_new_return_expr(lex->ast, line, expr);
}

static struct AstExpr *jump_expr(struct Lex *lex, enum JumpKind kind)
{
    int const line = lex->line;
    skip(lex); // 'break' or 'continue' token
    return pawAst_new_jump_expr(lex->ast, line, kind);
}

static struct AstExpr *match_arm(struct Lex *lex)
{
    int const line = lex->line;
    struct AstPat *pat = pattern(lex);
    struct AstExpr *guard = NULL;
    if (test_next(lex, TK_IF)) {
        guard = basic_expr(lex);
    }
    check_next(lex, TK_FAT_ARROW);
    struct AstExpr *result = expr0(lex);
    return pawAst_new_match_arm(lex->ast, line, pat, guard, result);
}

static struct AstExpr *match_expr(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // 'match' token
    struct AstExpr *target = basic_expr(lex);
    struct AstExprList *arms = pawAst_expr_list_new(lex->C);
    check_next(lex, '{');
    do {
        if (test(lex, '}'))
            break;
        struct AstExpr *arm = match_arm(lex);
        K_LIST_PUSH(lex->C, arms, arm);
    } while (test_next(lex, ','));
    delim_next(lex, '}', '{', line);
    return pawAst_new_match_expr(lex->ast, line, target, arms);
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
        case '{':
            expr = block_expr(lex);
            break;
        case TK_TRUE:
            expr = emit_bool(lex, PAW_TRUE);
            skip(lex);
            break;
        case TK_FALSE:
            expr = emit_bool(lex, PAW_FALSE);
            skip(lex);
            break;
        case TK_INTEGER:
            expr = new_basic_lit(lex, lex->t.value, BUILTIN_INT);
            skip(lex);
            break;
        case TK_FLOAT:
            expr = new_basic_lit(lex, lex->t.value, BUILTIN_FLOAT);
            skip(lex);
            break;
        case TK_STRING: {
            Value const v = lex->t.value;
            expr = new_basic_lit(lex, v, BUILTIN_STR);
            skip(lex);
            break;
        }
        case TK_IF:
            expr = if_expr(lex);
            break;
        case TK_FOR:
            expr = for_expr(lex);
            break;
        case TK_WHILE:
            expr = while_expr(lex);
            break;
        case TK_RETURN:
            expr = return_expr(lex);
            break;
        case TK_BREAK:
            expr = jump_expr(lex, JUMP_BREAK);
            break;
        case TK_CONTINUE:
            expr = jump_expr(lex, JUMP_CONTINUE);
            break;
        case TK_MATCH:
            expr = match_expr(lex);
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
    if (e == NULL)
        return NULL;
    if (AstIsBlock(e) || AstIsIfExpr(e) || AstIsMatchExpr(e) || AstIsWhileExpr(e)) {
        return e;
    }
    if (test(lex, '{')) {
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
        case TK_PIPE2:
            lex->t.kind = '|';
            lex->t2.kind = '|';
            // (fallthrough)
        case '|':
            return closure(lex);
        default:
            return suffixed_expr(lex);
    }
    return expr;
}

static struct AstExpr *conversion_expr(struct Lex *lex, struct AstExpr *lhs)
{
    int const line = lex->line;
    struct AstExpr *rhs = expression(lex, right_prec(INFIX_AS));
    struct AstPath *path = AstGetPathExpr(rhs)->path;
    if (!AstIsPathExpr(rhs) || path->count != 1) {
        pawX_error(lex, "expected basic type name");
    }
    enum BuiltinKind to;
    struct AstSegment seg = K_LIST_GET(path, 0);
    if (equals_cstr(lex, seg.name, CSTR_BOOL)) {
        to = BUILTIN_BOOL;
    } else if (equals_cstr(lex, seg.name, CSTR_INT)) {
        to = BUILTIN_INT;
    } else if (equals_cstr(lex, seg.name, CSTR_FLOAT)) {
        to = BUILTIN_FLOAT;
    } else {
        pawX_error(lex, "expected basic type");
    }
    return pawAst_new_conversion_expr(lex->ast, line, lhs, to);
}

static struct AstExpr *assignment_expr(struct Lex *lex, struct AstExpr *lhs)
{
    int const line = lex->line;
    struct AstExpr *rhs = expression(lex, right_prec(INFIX_ASSIGN));
    return pawAst_new_assign_expr(lex->ast, line, lhs, rhs);
}

static struct AstExpr *binop_expr(struct Lex *lex, enum InfixOp op, struct AstExpr *lhs)
{
    enter_nested(lex);
    int const line = lex->line;
    struct AstExpr *rhs = expression(lex, right_prec(op));
    enum BinaryOp binop = CAST(enum BinaryOp, op); // same order
    leave_nested(lex);
    return pawAst_new_binop_expr(lex->ast, line, binop, lhs, rhs);
}

static struct AstExpr *logical_expr(struct Lex *lex, struct AstExpr *lhs, paw_Bool is_and)
{
    int const line = lex->line;
    int const prec = right_prec(is_and ? INFIX_AND : INFIX_OR);
    struct AstExpr *rhs = expression(lex, prec);
    return pawAst_new_logical_expr(lex->ast, line, lhs, rhs, is_and);
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
    if (expr == NULL)
        return NULL;

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
    if (expr == NULL)
        pawX_error(lex, "expected expression");
    return expr;
}

static struct AstExpr *basic_expr(struct Lex *lex)
{
    int const prev_depth = lex->expr_depth;
    lex->expr_depth = -1;
    struct AstExpr *expr = expr0(lex);
    lex->expr_depth = prev_depth;
    return expr;
}

static struct AstStmt *expr_stmt(struct Lex *lex)
{
    int const line = lex->line;
    struct AstExpr *expr = expr0(lex);
    return pawAst_new_expr_stmt(lex->ast, line, expr);
}

static struct AstDeclList *type_param(struct Lex *lex)
{
    int const line = lex->line;
    if (test_next(lex, '<')) {
        ++lex->expr_depth;
        struct AstDeclList *list = pawAst_decl_list_new(lex->C);
        parse_generic_list(lex, list, line);
        --lex->expr_depth;
        if (list->count == 0) {
            pawX_error(lex, "expected at least 1 generic parameter");
        }
        return list;
    }
    return NULL;
}

static struct AstDecl *function(struct Lex *lex, int line, String *name, enum FuncKind kind, paw_Bool is_pub)
{
    struct AstDeclList *generics = type_param(lex);
    struct AstDeclList *params = func_parameters(lex);
    struct AstType *result = test_next(lex, TK_ARROW) ? parse_type(lex, PAW_TRUE) : unit_type(lex);
    struct AstExpr *body = !test_next(lex, ';') ? block_expr(lex) : NULL;
    return pawAst_new_func_decl(lex->ast, line, kind, name, generics,
                                params, NULL, result, body, is_pub);
}

static struct AstDecl *use_decl(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // 'use' token
    String *name = parse_toplevel_name(lex);
    paw_Bool star = PAW_FALSE;
    String *item = NULL;
    String *as = NULL;
    if (test_next(lex, TK_COLON2)) {
        if (test_next(lex, '*'))
            star = PAW_TRUE;
        else
            item = parse_toplevel_name(lex);
    }
    if (test_next(lex, TK_AS)) {
        as = parse_toplevel_name(lex);
    }
    semicolon(lex);
    return pawAst_new_use_decl(lex->ast, line, name, star, item, as);
}

static void parse_trait_list(struct Lex *lex, struct AstTypeList *traits)
{
    do {
        if (test(lex, '{'))
            break;
        int const line = lex->line;
        struct AstPath *path = parse_pathtype(lex, PAW_TRUE);
        struct AstType *trait = pawAst_new_path_type(lex->ast, line, path);
        K_LIST_PUSH(lex->C, traits, trait);
    } while (test_next(lex, '+'));
}

static struct AstDecl *func_decl(struct Lex *lex, paw_Bool is_pub)
{
    int const line = lex->line;
    skip(lex); // 'fn' token
    String *name = parse_toplevel_name(lex);
    return function(lex, line, name, FUNC_FUNCTION, is_pub);
}

static struct AstDecl *parse_method(struct Lex *lex, paw_Bool is_pub)
{
    int const line = lex->line;
    check_next(lex, TK_FN);
    // indicate that 'self' has special meaning
    lex->in_impl = PAW_TRUE;
    String *name = parse_toplevel_name(lex);
    struct AstDecl *method = function(lex, line, name, FUNC_METHOD, is_pub);
    lex->in_impl = PAW_FALSE;
    return method;
}

static struct AstDecl *variant_decl(struct Lex *lex, int index)
{
    int const line = lex->line;
    String *name = parse_toplevel_name(lex);
    struct AstDeclList *fields = NULL;
    if (test_next(lex, '(')) {
        fields = variant_field_list(lex, line);
    }
    return pawAst_new_variant_decl(lex->ast, line, name, fields, index);
}

static void enum_body(struct Lex *lex, int line, struct AstTypeList *traits, struct AstDeclList *variants, struct AstDeclList *methods)
{
    if (test_next(lex, ':')) {
        parse_trait_list(lex, traits);
    }
    check_next(lex, '{');
    while (!end_of_block(lex)) {
        if (test(lex, TK_NAME)) {
            if (variants->count == LOCAL_MAX) {
                limit_error(lex, "variants in enum body", LOCAL_MAX);
            }
            struct AstDecl *variant = variant_decl(lex, variants->count);
            K_LIST_PUSH(lex->C, variants, variant);
            if (!test_next(lex, ',') && !test(lex, '}')) {
                SYNTAX_ERROR(lex->C, "',' required to separate enum variants from "
                                     "other items");
            }
        } else if (methods->count == LOCAL_MAX) {
            limit_error(lex, "methods in enum body", LOCAL_MAX);
        } else {
            paw_Bool const is_pub = test_next(lex, TK_PUB);
            struct AstDecl *method = parse_method(lex, is_pub);
            K_LIST_PUSH(lex->C, methods, method);
        }
    }
    delim_next(lex, '}', '{', line);

    if (variants->count == 0) {
        pawX_error(lex, "expected at least 1 enum variant");
    }
}

static struct AstDecl *enum_decl(struct Lex *lex, paw_Bool is_pub)
{
    skip(lex); // 'enum' token
    int const line = lex->line;
    String *name = parse_toplevel_name(lex);
    struct AstDeclList *generics = type_param(lex);
    struct AstTypeList *traits = pawAst_type_list_new(lex->C);
    struct AstDeclList *variants = pawAst_decl_list_new(lex->C);
    struct AstDeclList *methods = pawAst_decl_list_new(lex->C);
    enum_body(lex, line, traits, variants, methods);
    return pawAst_new_adt_decl(lex->ast, line, name, traits,
                               generics, variants, methods, is_pub, PAW_FALSE);
}

static struct AstDecl *struct_field(struct Lex *lex, paw_Bool is_pub)
{
    int const line = lex->line;
    String *name = parse_toplevel_name(lex);
    struct AstType *tag = expect_annotation(lex, "field", name, PAW_TRUE);
    return pawAst_new_field_decl(lex->ast, line, name, tag, is_pub);
}

static void struct_body(struct Lex *lex, struct AstTypeList *traits, struct AstDeclList *fields, struct AstDeclList *methods)
{
    int const line = lex->line;
    if (test_next(lex, ':')) {
        parse_trait_list(lex, traits);
    }
    if (!test_next(lex, '{')) {
        semicolon(lex);
        return;
    }

    while (!end_of_block(lex)) {
        paw_Bool const is_pub = test_next(lex, TK_PUB);
        if (test(lex, TK_NAME)) {
            if (fields->count == LOCAL_MAX) {
                limit_error(lex, "fields in struct body", LOCAL_MAX);
            }
            struct AstDecl *field = struct_field(lex, is_pub);
            K_LIST_PUSH(lex->C, fields, field);
            if (!test_next(lex, ',') && !test(lex, '}')) {
                SYNTAX_ERROR(lex->C, "',' required to separate struct fields from "
                                     "other items");
            }
        } else if (methods->count == LOCAL_MAX) {
            limit_error(lex, "methods in struct body", LOCAL_MAX);
        } else {
            struct AstDecl *method = parse_method(lex, is_pub);
            K_LIST_PUSH(lex->C, methods, method);
        }
    }
    delim_next(lex, '}', '{', line);

    if (fields->count == 0 && methods->count == 0) {
        pawX_error(lex, "expected at least 1 struct field or method between "
                        "curly braces (remove curly braces for unit structure)");
    }
}

static struct AstDecl *struct_decl(struct Lex *lex, paw_Bool is_pub)
{
    int const line = lex->line;
    skip(lex); // 'struct' token
    String *name = parse_toplevel_name(lex);
    struct AstDeclList *generics = type_param(lex);
    struct AstTypeList *traits = pawAst_type_list_new(lex->C);
    struct AstDeclList *fields = pawAst_decl_list_new(lex->C);
    struct AstDeclList *methods = pawAst_decl_list_new(lex->C);
    struct_body(lex, traits, fields, methods);
    return pawAst_new_adt_decl(lex->ast, line, name, traits,
                               generics, fields, methods, is_pub, PAW_TRUE);
}

static struct AstDecl *trait_decl(struct Lex *lex, paw_Bool is_pub)
{
    int const line = lex->line;
    skip(lex); // 'trait' token
    String *name = parse_toplevel_name(lex);
    struct AstDeclList *generics = type_param(lex);

    check_next(lex, '{');
    struct AstDeclList *methods = pawAst_decl_list_new(lex->C);
    while (!end_of_block(lex)) {
        if (methods->count == LOCAL_MAX) {
            limit_error(lex, "methods in trait body", LOCAL_MAX);
        }
        struct AstDecl *method = parse_method(lex, is_pub);
        K_LIST_PUSH(lex->C, methods, method);
    }
    delim_next(lex, '}', '{', line);
    return pawAst_new_trait_decl(lex->ast, line, name, generics, methods, is_pub);
}

static struct AstDecl *type_decl(struct Lex *lex)
{
    int const line = lex->line;
    skip(lex); // 'type' token

    String *name = parse_toplevel_name(lex);
    struct AstDeclList *generics = type_param(lex);

    check_next(lex, '=');

    struct AstType *rhs = parse_type(lex, PAW_TRUE);
    if (AstIsFuncType(rhs)) {
        pawX_error(lex, "function type cannot appear in a type declaration");
    }
    semicolon(lex);
    return pawAst_new_type_decl(lex->ast, line, name, generics, rhs);
}

static struct AstStmt *decl_stmt(struct Lex *lex)
{
    int const line = lex->line;
    struct AstDecl *decl;
    if (test(lex, TK_LET)) {
        decl = let_decl(lex, lex->line);
    } else if (test(lex, TK_TYPE)) {
        decl = type_decl(lex);
    } else {
        pawX_error(lex, "expected 'let' or 'type' declaration");
    }
    return pawAst_new_decl_stmt(lex->ast, line, decl);
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

static void ensure_not_pub(struct Lex *lex, paw_Bool has_qualifier)
{
    if (has_qualifier)
        pawX_error(lex, "visibility qualifier not allowed here");
}

static struct AstDecl *toplevel_item(struct Lex *lex, paw_Bool is_pub)
{
    switch (lex->t.kind) {
        default:
            pawX_error(lex, "expected toplevel item");
        case TK_FN:
            return func_decl(lex, is_pub);
        case TK_ENUM:
            return enum_decl(lex, is_pub);
        case TK_STRUCT:
            return struct_decl(lex, is_pub);
        case TK_TRAIT:
            return trait_decl(lex, is_pub);
        case TK_USE:
            ensure_not_pub(lex, is_pub);
            return use_decl(lex);
        case TK_TYPE:
            ensure_not_pub(lex, is_pub);
            return type_decl(lex);
    }
}

static struct AstDeclList *toplevel_items(struct Lex *lex, struct AstDeclList *list)
{
    while (!test(lex, TK_END)) {
        paw_Bool const is_pub = test_next(lex, TK_PUB);
        struct AstDecl *item = toplevel_item(lex, is_pub);
        K_LIST_PUSH(lex->C, list, item);
    }
    return list;
}

// TODO: someday, #embed should be used for this... once C23 support is better
static char const kPrelude[] =
    "pub trait Hash {\n"
    "    fn hash(self) -> int;\n"
    "}\n"

    "pub trait Equals {\n"
    "    fn eq(self, rhs: Self) -> bool;\n"
    "}\n"

    "pub struct unit: Hash + Equals {\n"
    "    pub fn hash(self) -> int { 0 }\n"
    "    pub fn eq(self, rhs: Self) -> bool { true }\n"
    "}\n"

    "pub struct bool: Hash + Equals {\n"
    "    pub fn to_string(self) -> str;\n"
    "    pub fn hash(self) -> int { self as int }\n"
    "    pub fn eq(self, rhs: Self) -> bool { self == rhs }\n"
    "}\n"

    "pub struct int: Hash + Equals {\n"
    "    pub fn to_string(self) -> str;\n"
    "    pub fn hash(self) -> int { self }\n"
    "    pub fn eq(self, rhs: Self) -> bool { self == rhs }\n"
    "}\n"

    "pub struct float: Hash + Equals {\n"
    "    pub fn to_string(self) -> str;\n"
    "    pub fn hash(self) -> int;\n"
    "    pub fn eq(self, rhs: Self) -> bool { self == rhs }\n"
    "}\n"

    "pub struct str: Hash + Equals {\n"
    "    pub fn parse_int(self, base: int) -> int;\n"
    "    pub fn parse_float(self) -> float;\n"
    "    pub fn split(self, sep: str) -> [str];\n"
    "    pub fn join(self, seq: [str]) -> str;\n"
    "    pub fn find(self, target: str) -> int;\n"
    "    pub fn starts_with(self, prefix: str) -> bool;\n"
    "    pub fn ends_with(self, suffix: str) -> bool;\n"
    "    pub fn hash(self) -> int;\n"
    "    pub fn eq(self, rhs: Self) -> bool { self == rhs }\n"
    "}\n"

    "pub struct List<T> {\n"
    "    pub fn length(self) -> int;\n"
    "    pub fn push(self, value: T) -> Self;\n"
    "    pub fn insert(self, index: int, value: T) -> Self;\n"
    "    pub fn remove(self, index: int) -> T;\n"
    "    pub fn pop(self) -> T;\n"
    "}\n"

    "pub struct Map<K: Hash + Equals, V> {\n"
    "    pub fn length(self) -> int;\n"
    "    pub fn get_or(self, key: K, default: V) -> V;\n"
    "    pub fn erase(self, key: K) -> Self;\n"
    "}\n"

    "pub enum Option<T> {\n"
    "    Some(T),\n"
    "    None,\n"

    "    pub fn is_some(self) -> bool;\n"
    "    pub fn is_none(self) -> bool;\n"
    "    pub fn unwrap(self) -> T;\n"
    "    pub fn unwrap_or(self, value: T) -> T;\n"
    "}\n"

    "pub enum Result<T, E> {\n"
    "    Ok(T),\n"
    "    Err(E),\n"

    "    pub fn is_ok(self) -> bool;\n"
    "    pub fn is_err(self) -> bool;\n"
    "    pub fn unwrap(self) -> T;\n"
    "    pub fn unwrap_err(self) -> E;\n"
    "    pub fn unwrap_or(self, value: T) -> T;\n"
    "}\n"

    "pub fn print(message: str);\n"
    "pub fn assert(cond: bool);\n"
    "pub fn range(begin: int, end: int, step: int) -> (fn() -> Option<int>);\n";

struct PreludeReader {
    size_t size;
};

char const *prelude_reader(paw_Env *P, void *ud, size_t *size)
{
    PAW_UNUSED(P);
    struct PreludeReader *pr = ud;
    *size = pr->size;
    pr->size = 0;
    return kPrelude;
}

static void parse_prelude(struct Lex *lex)
{
    struct Ast *ast = lex->ast;
    struct PreludeReader reader = {PAW_LENGTHOF(kPrelude)};
    pawX_set_source(lex, prelude_reader, &reader);
    toplevel_items(lex, ast->items);
    check(lex, TK_END);
}

static void skip_hashbang(struct Lex *lex)
{
    if (test_next(lex, '#') && test_next(lex, '!')) {
        while (!test(lex, TK_END)) {
            char const c = *lex->ptr;
            skip(lex); // skip line
            if (ISNEWLINE(c))
                break;
        }
    }
}

static struct Ast *parse_module(struct Lex *lex, paw_Reader input, void *ud)
{
    pawX_set_source(lex, input, ud);
    skip_hashbang(lex);

    struct Ast *ast = lex->ast;
    ast->items = pawAst_decl_list_new(lex->C);
    toplevel_items(lex, ast->items);
    check(lex, TK_END);
    return ast;
}

static void init_lexer(struct Compiler *C, struct Ast *ast, struct Lex *lex)
{
    *lex = (struct Lex){
        .modname = ast->name,
        .strings = C->strings,
        .ast = ast,
        .dm = C->dm,
        .P = C->P,
        .C = C,
    };
}

static struct Ast *new_ast(struct Compiler *C, String *name)
{
    int const modno = ImportMap_length(C->imports) + 1 /* skip prelude */;
    struct Ast *ast = pawAst_new(C, name, modno);
    ImportMap_insert(C, C->imports, name, ast);
    return ast;
}

struct Ast *pawP_parse_module(struct Compiler *C, String *modname, paw_Reader input, void *ud)
{
    struct Ast *ast = new_ast(C, modname);

    struct Lex lex;
    init_lexer(C, ast, &lex);

    parse_module(&lex, input, ud);
    return ast;
}

struct Ast *pawP_parse_prelude(struct Compiler *C)
{
    struct Lex lex;
    init_lexer(C, C->prelude, &lex);

    parse_prelude(&lex);
    return lex.ast;
}
