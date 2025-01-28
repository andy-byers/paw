// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "parse.h"
#include "env.h"
#include "ast.h"
#include "compile.h"
#include "map.h"

#define NEW_EXPR(lex, kind) pawAst_new_expr((lex)->ast, (lex)->last_line, kind)
#define NEW_STMT(lex, kind) pawAst_new_stmt((lex)->ast, (lex)->last_line, kind)
#define NEW_DECL(lex, kind) pawAst_new_decl((lex)->ast, (lex)->last_line, kind)
#define NEW_PAT(lex, kind) pawAst_new_pat((lex)->ast, (lex)->last_line, kind)

#define SELF_TYPENAME(lex) SCAN_STRING(lex, "Self")
#define SELF_VARNAME(lex) CACHED_STRING(ENV(lex), CSTR_SELF)

static String *unpack_name(const struct AstExpr *expr)
{
    if (!AstIsPathExpr(expr)) return NULL;
    struct AstPath *path = expr->path.path;
    if (path->count == 1) {
        struct AstSegment ps = K_LIST_GET(path, 0);
        if (ps.types == NULL) return ps.name;
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

static struct AstExpr *new_basic_lit(struct Lex *lex, Value v, paw_Type code)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstLiteralExpr);
    r->literal.basic.t = code;
    r->literal.basic.value = v;
    return r;
}

static struct AstExpr *unit_lit(struct Lex *lex)
{
    return new_basic_lit(lex, I2V(0), PAW_TUNIT);
}

static struct AstExpr *unit_type(struct Lex *lex)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstTupleType);
    r->tuple.types = pawAst_expr_list_new(lex->C);
    return r;
}

static struct AstExpr *emit_bool(struct Lex *lex, paw_Bool b)
{
    Value v;
    V_SET_BOOL(&v, b);
    return new_basic_lit(lex, v, PAW_TBOOL);
}

static struct AstExpr *type_expr(struct Lex *lex);

static struct AstDecl *variant_field_decl(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstFieldDecl);
    r->field.name = NULL;
    r->field.tag = type_expr(lex);
    return r;
}

#define DEFINE_LIST_PARSER(name, a, b, limit, what, func, L) \
    static void parse_##name##_list(struct Lex *lex, struct L *list, int line) \
    { \
        do { \
            if (test(lex, b)) break; \
            if ((list)->count == (limit)) { \
                limit_error(lex, what, (limit)); \
            } \
            K_LIST_PUSH((lex)->C, list, (func)(lex)); \
        } while (test_next(lex, ',')); \
        delim_next(lex, b, a, line); \
    }
DEFINE_LIST_PARSER(arg, '(', ')', LOCAL_MAX, "arguments", expr0, AstExprList)
DEFINE_LIST_PARSER(variant_field, '(', ')', LOCAL_MAX, "variant fields", variant_field_decl, AstDeclList)
DEFINE_LIST_PARSER(type, '<', '>', LOCAL_MAX, "type arguments", type_expr, AstExprList)

static struct AstExprList *type_list(struct Lex *lex, int line)
{
    ++lex->expr_depth;
    struct AstExprList *list = pawAst_expr_list_new(lex->C);
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

static struct AstPath *parse_pathexpr(struct Lex *lex)
{
    struct AstPath *p = pawAst_path_new(lex->C);
    do {
next_segment:
        if (p->count == LOCAL_MAX) {
            limit_error(lex, "path segments", LOCAL_MAX);
        }
        String *name = parse_name(lex);
        struct AstExprList *args = NULL;
        // permit "::<types..>" between segments
        if (test_next(lex, TK_COLON2)) {
            args = maybe_type_args(lex);
            if (args == NULL) {
                pawAst_path_add(lex->C, p, name, NULL);
                goto next_segment;
            }
        }
        pawAst_path_add(lex->C, p, name, args);
    } while (test_next(lex, TK_COLON2));
    return p;
}

static struct AstPath *parse_pathtype(struct Lex *lex)
{
    struct AstPath *p = pawAst_path_new(lex->C);
    do {
        if (p->count == LOCAL_MAX) {
            limit_error(lex, "path segments", LOCAL_MAX);
        }
        String *name = parse_name(lex);
        struct AstExprList *args = maybe_type_args(lex);
        pawAst_path_add(lex->C, p, name, args);
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

static struct AstPat *variant_field_pat(struct Lex *lex)
{
    struct AstPat *result = NEW_PAT(lex, kAstFieldPat);
    struct AstFieldPat *r = AstGetFieldPat(result);
    r->pat = pattern(lex);
    return result;
}

static struct AstPat *new_path_pat(struct Lex *lex, String *name)
{
    struct AstPat *result = NEW_PAT(lex, kAstPathPat);
    struct AstPathPat *r = AstGetPathPat(result);
    r->path = pawAst_path_new(lex->C);
    pawAst_path_add(lex->C, r->path, name, NULL);
    return result;
}

static struct AstPat *struct_field_pat(struct Lex *lex)
{
    struct AstPat *result = NEW_PAT(lex, kAstFieldPat);
    struct AstFieldPat *r = AstGetFieldPat(result);
    r->name = parse_name(lex);
    if (test_next(lex, ':')) {
        r->pat = pattern(lex);
    } else {
        // binds field to variable of same name
        r->pat = new_path_pat(lex, r->name);
    }
    return result;
}

DEFINE_LIST_PARSER(variant_field_pat, '(', ')', LOCAL_MAX, "variant fields", pattern, AstPatList)
DEFINE_LIST_PARSER(struct_field_pat, '{', '}', LOCAL_MAX, "struct fields", struct_field_pat, AstPatList)

static paw_Bool is_wildcard_path(const struct AstPath *path)
{
    paw_assert(path->count > 0);
    if (path->count > 1) return PAW_FALSE;
    struct AstSegment seg = K_LIST_GET(path, 0);
    return pawS_length(seg.name) == 1 &&
        seg.name->text[0] == '_';
}

static struct AstPat *compound_pat(struct Lex *lex)
{
    struct AstPat *result = NEW_PAT(lex, 0);
    struct AstPath *path = parse_pathexpr(lex);
    struct AstPatList *fields = pawAst_pat_list_new(lex->C);
    if (test_next(lex, '(')) {
        result->hdr.kind = kAstVariantPat;
        struct AstVariantPat *r = AstGetVariantPat(result);
        parse_variant_field_pat_list(lex, fields, r->line);
        r->path = path;
        r->fields = fields;
    } else if (test_next(lex, '{')) {
        result->hdr.kind = kAstStructPat;
        struct AstStructPat *r = AstGetStructPat(result);
        parse_struct_field_pat_list(lex, fields, r->line);
        r->path = path;
        r->fields = fields;
    } else if (is_wildcard_path(path)) {
        result->hdr.kind = kAstWildcardPat;
    } else {
        result->hdr.kind = kAstPathPat;
        struct AstPathPat *r = AstGetPathPat(result);
        r->path = path;
    }
    return result;
}

static struct AstPat *tuple_pat(struct Lex *lex)
{
    struct AstPat *result = NEW_PAT(lex, kAstTuplePat);
    struct AstTuplePat *r = AstGetTuplePat(result);
    skip(lex); // '(' token
    r->elems = pawAst_pat_list_new(lex->C);
    parse_variant_field_pat_list(lex, r->elems, r->line);
    return result;
}

static struct AstExpr *literal_expr(struct Lex *lex)
{
    const paw_Bool negative = test_next(lex, '-');

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
            expr = new_basic_lit(lex, v, PAW_TSTR);
            break;
        }
        default:
            pawX_error(lex, "expected literal pattern");
    }
    skip(lex); // literal token

    if (negative) {
        struct AstLiteralExpr *e = AstGetLiteralExpr(expr);
        if (e->basic.t == PAW_TINT) {
            if (e->basic.value.i == PAW_INT_MIN) {
                pawX_error(lex, "signed integer overflow ('-' applied to %I)", PAW_INT_MIN);
            }
            e->basic.value.i = -e->basic.value.i;
        } else if (e->basic.t == PAW_TFLOAT) {
            e->basic.value.f = -e->basic.value.f;
        } else {
            pawX_error(lex, "operator '-' applied to non-numeric value");
        }
    }
    return expr;
}

static struct AstPat *literal_pat(struct Lex *lex)
{
    struct AstPat *result = NEW_PAT(lex, kAstLiteralPat);
    struct AstLiteralPat *r = AstGetLiteralPat(result);
    r->expr = literal_expr(lex);

    if (!AstIsLiteralExpr(r->expr)) {
        pawX_error(lex, "expected literal pattern");
    }
    return result;
}

static struct AstPat *or_pat(struct Lex *lex, struct AstPat *pat)
{
    struct AstPat *result = NEW_PAT(lex, kAstOrPat);
    struct AstOrPat *r = AstGetOrPat(result);
    r->lhs = pat;
    r->rhs = pattern(lex);
    return result;
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
    if (!test_next(lex, '|')) return pat;
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

static void parse_tuple_type(struct Lex *lex, struct AstExpr *pe, int line)
{
    struct AstExpr *first = NEW_EXPR(lex, 0);
    *first = *pe;

    struct AstExprList *elems = pawAst_expr_list_new(lex->C);
    K_LIST_PUSH(lex->C, elems, first);

    do {
        if (test(lex, ')')) break;
        if (elems->count == FIELD_MAX) {
            limit_error(lex, "tuple elements", FIELD_MAX);
        }
        struct AstExpr *type = type_expr(lex);
        K_LIST_PUSH(lex->C, elems, type);
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
    } else {
        delim_next(lex, ')', '(', line);
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

static struct AstExpr *self_type(struct Lex *lex)
{
    struct AstExpr *result = NEW_EXPR(lex, kAstPathExpr);
    struct AstPathExpr *r = AstGetPathExpr(result);
    r->path = pawAst_path_new(lex->C);
    pawAst_path_add(lex->C, r->path, SELF_TYPENAME(lex), NULL);
    return result;
}

static void expect_self(struct Lex *lex, const String *name)
{
    if (pawS_eq(name, SELF_VARNAME(lex))) return;
    pawX_error(lex, "expected parameter named 'self' but found '%s'",
            name->text);
}

static struct AstDecl *func_param_decl(struct Lex *lex)
{
    struct AstDecl *result = NEW_DECL(lex, kAstFieldDecl);
    struct AstFieldDecl *r = AstGetFieldDecl(result);
    r->name = parse_name(lex);
    const int param_index = lex->param_index++;
    if (!lex->in_impl || param_index != 0) {
        // usual case: expect a type annotation on each parameter
        r->tag = expect_annotation(lex, "parameter", r->name);
        return result;
    }
    // first parameter to method: 'self' means 'self: Self'
    r->tag = type_annotation(lex);
    if (r->tag == NULL) {
        r->tag = self_type(lex);
        expect_self(lex, r->name);
    }
    return result;
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
    semicolon(lex);
    return r;
}

static struct AstDecl *generic_param(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstGenericDecl);
    r->generic.name = parse_name(lex);
    return r;
}

DEFINE_LIST_PARSER(func_param, '(', ')', LOCAL_MAX, "function parameters", func_param_decl,  AstDeclList)
DEFINE_LIST_PARSER(sig_param, '(', ')', LOCAL_MAX, "function parameters", type_expr, AstExprList)
DEFINE_LIST_PARSER(clos_param, '|', '|', LOCAL_MAX, "closure parameters", clos_param_decl, AstDeclList)
DEFINE_LIST_PARSER(generic, '<', '>', LOCAL_MAX, "generics", generic_param, AstDeclList)

static struct AstExpr *sitem_expr(struct Lex *lex)
{
    struct AstExpr *result = NEW_EXPR(lex, kAstFieldExpr);
    struct AstFieldExpr *r = AstGetFieldExpr(result);
    r->name = parse_name(lex);
    if (test_next(lex, ':')) {
        r->value = expr0(lex);
    } else {
        // "name" by itself is shorthand for "name: name"
        struct AstExpr *e = NEW_EXPR(lex, kAstPathExpr);
        struct AstPath *path = pawAst_path_new(lex->C);
        pawAst_path_add(lex->C, path, r->name, NULL);
        AstGetPathExpr(e)->path = path;
        r->value = e;
    }
    return result;
}

DEFINE_LIST_PARSER(sitem, '{', '}', LOCAL_MAX, "struct items", sitem_expr, AstExprList)

static struct AstExpr *unop_expr(struct Lex *lex, enum UnOp op)
{
    enter_nested(lex);
    struct AstExpr *result = NEW_EXPR(lex, kAstUnOpExpr);
    struct AstUnOpExpr *r = AstGetUnOpExpr(result);
    skip(lex); // unary operator token
    r->op = CAST(enum UnaryOp, op); // same order
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
        return unit_lit(lex);
    }
    ++lex->expr_depth;
    struct AstExpr *expr = expr0(lex);
    --lex->expr_depth;
    if (test_next(lex, ')')) {
        struct AstExpr *r = NEW_EXPR(lex, kAstParenExpr);
        AstGetParenExpr(r)->expr = expr;
        return r;
    }
    struct AstExpr *r = NEW_EXPR(lex, kAstLiteralExpr);
    check_next(lex, ',');
    struct AstExprList *elems = pawAst_expr_list_new(lex->C);
    K_LIST_PUSH(lex->C, elems, expr);
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
    e->sig.params = pawAst_expr_list_new(lex->C);
    if (!test_next(lex, ')')) {
        parse_sig_param_list(lex, e->sig.params, line);
    }
    if (test_next(lex, TK_ARROW)) {
        e->sig.result = type_expr(lex);
    } else {
        e->sig.result = unit_type(lex);
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
            code = BUILTIN_LIST;
        } else if (code == BUILTIN_LIST) {
            pawX_error(lex, "invalid container literal");
        } else {
            code = BUILTIN_MAP;
            struct AstExpr *result = NEW_EXPR(lex, kAstFieldExpr);
            struct AstFieldExpr *r = AstGetFieldExpr(result);
            r->fid = -1; // not a fixed structure field
            r->value = expr0(lex);
            r->key = item;
            item = result;
        }
        K_LIST_PUSH(lex->C, items, item);
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
    cont->items = pawAst_expr_list_new(lex->C);
    if (test(lex, ']')) {
        cont->code = BUILTIN_LIST; // empty list: '[]'
    } else if (test_next(lex, ':')) {
        cont->code = BUILTIN_MAP; // empty map: '[:]'
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
    lit->comp.items = pawAst_expr_list_new(lex->C);
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
    } else {
        pawX_error(lex, "expected identifier or integer after '.'");
    }
    return r;
}

static paw_Bool equals_cstr(struct Lex *lex, const String *ident, unsigned cstr)
{
    return pawS_eq(ident, CACHED_STRING(ENV(lex), cstr));
}

static struct AstExpr *call_expr(struct Lex *lex, struct AstExpr *target)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstCallExpr);
    skip(lex); // '(' token
    r->call.target = target;
    r->call.args = pawAst_expr_list_new(lex->C);
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
    struct AstDeclList *list = pawAst_decl_list_new(lex->C);
    lex->param_index = 0; // 'self' allowed if '.in_impl'
    parse_func_param_list(lex, list, line);
    return list;
}

static struct AstDeclList *clos_parameters(struct Lex *lex)
{
    const int line = lex->line;
    check_next(lex, '|');
    struct AstDeclList *list = pawAst_decl_list_new(lex->C);
    parse_clos_param_list(lex, list, line);
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

static struct AstExpr *block(struct Lex *lex)
{
    const int line = lex->line;
    struct AstExpr *result = NEW_EXPR(lex, kAstBlock);
    struct AstBlock *r = &result->block;
    check_next(lex, '{');
    r->stmts = pawAst_stmt_list_new(lex->C);
    while (!end_of_block(lex)) {
        struct AstStmt *next = statement(lex);
        if (next == NULL) continue; // extra ';'
        K_LIST_PUSH(lex->C, r->stmts, next);
        if (AstIsExprStmt(next) && !test_next(lex, ';')) {
            struct AstExprStmt *s = AstGetExprStmt(next);
            if (expects_semicolon(s->expr) || test(lex, '}')) {
                K_LIST_POP(r->stmts);
                r->result = s->expr;
                break;
            }
        }
    }
    if (r->result == NULL) {
        r->result = unit_lit(lex);
    }
    delim_next(lex, '}', '{', line);
    return result;
}

static struct AstExpr *closure(struct Lex *lex)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstClosureExpr);
    r->clos.params = clos_parameters(lex);
    if (test_next(lex, TK_ARROW)) {
        r->clos.result = type_expr(lex);
        r->clos.expr = block(lex);
    } else {
        r->clos.expr = expr0(lex);
    }
    return r;
}

static struct AstExpr *if_expr(struct Lex *lex)
{
    skip(lex); // 'if' token
    struct AstExpr *result = NEW_EXPR(lex, kAstIfExpr);
    struct AstIfExpr *r = AstGetIfExpr(result);
    r->cond = basic_expr(lex);
    r->then_arm = block(lex);

    if (test_next(lex, TK_ELSE)) {
        // transform "else if" construct:
        //     (!) "if a {A} else if b {B} else {C}"
        //     (2) "if a {A} else {if b {B} else {C}}"
        r->else_arm = test(lex, TK_IF)
            ? if_expr(lex)
            : block(lex);
    }
    return result;
}

static struct AstExpr *for_expr(struct Lex *lex)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstForExpr);
    skip(lex); // 'for' token
    AstGetForExpr(r)->name = parse_name(lex);
    check_next(lex, TK_IN);
    AstGetForExpr(r)->target = basic_expr(lex);
    AstGetForExpr(r)->block = block(lex);
    return r;
}

static struct AstExpr *while_expr(struct Lex *lex)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstWhileExpr);
    skip(lex); // 'while' token
    AstGetWhileExpr(r)->cond = basic_expr(lex);
    AstGetWhileExpr(r)->block = block(lex);
    return r;
}

static struct AstExpr *return_expr(struct Lex *lex)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstReturnExpr);
    skip(lex); // 'return' token
    if (!test(lex, ';') && !test(lex, '}')) {
        AstGetReturnExpr(r)->expr = expr0(lex);
    }
    return r;
}

static struct AstExpr *jump_expr(struct Lex *lex, enum JumpKind kind)
{
    struct AstExpr *r = NEW_EXPR(lex, kAstJumpExpr);
    skip(lex); // 'break' or 'continue' token
    AstGetJumpExpr(r)->jump_kind = kind;
    return r;
}

static struct AstExpr *match_arm(struct Lex *lex)
{
    struct AstExpr *result = NEW_EXPR(lex, kAstMatchArm);
    struct AstMatchArm *r = AstGetMatchArm(result);
    r->pat = pattern(lex);
    if (test_next(lex, TK_IF)) {
        r->guard = basic_expr(lex);
    }
    check_next(lex, TK_FAT_ARROW);
    r->result = expr0(lex);
    return result;
}

static struct AstExpr *match_expr(struct Lex *lex)
{
    const int line = lex->line;
    struct AstExpr *result = NEW_EXPR(lex, kAstMatchExpr);
    struct AstMatchExpr *r = AstGetMatchExpr(result);
    skip(lex); // 'match' token
    r->target = basic_expr(lex);
    r->arms = pawAst_expr_list_new(lex->C);
    check_next(lex, '{');
    do {
        if (test(lex, '}')) break;
        struct AstExpr *arm = match_arm(lex);
        K_LIST_PUSH(lex->C, r->arms, arm);
    } while (test_next(lex, ','));
    delim_next(lex, '}', '{', line);
    return result;
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
            expr = block(lex);
            break;
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
    if (e == NULL) return NULL;
    if (AstIsBlock(e)
            || AstIsIfExpr(e)
            || AstIsMatchExpr(e)
            || AstIsWhileExpr(e)) {
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
            expr = new_basic_lit(lex, v, PAW_TSTR);
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
    struct AstSegment seg = K_LIST_GET(path, 0);
    if (equals_cstr(lex, seg.name, CSTR_BOOL)) {
        r->conv.to = PAW_TBOOL;
    } else if (equals_cstr(lex, seg.name, CSTR_INT)) {
        r->conv.to = PAW_TINT;
    } else if (equals_cstr(lex, seg.name, CSTR_FLOAT)) {
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
    r->op = CAST(enum BinaryOp, op); // same order
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

static struct AstStmt *expr_stmt(struct Lex *lex)
{
    struct AstStmt *result = NEW_STMT(lex, kAstExprStmt);
    struct AstExprStmt *r = &result->expr;
    r->expr = expr0(lex);
    return result;
}

static struct AstDeclList *type_param(struct Lex *lex)
{
    const int line = lex->line;
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
    r->func.body = test_next(lex, ';')
        ? NULL
        : block(lex);
    return r;
}

static struct AstDecl *use_decl(struct Lex *lex)
{
    const int line = lex->line;
    skip(lex); // 'use' token
    struct AstDecl *result = NEW_DECL(lex, kAstUseDecl);
    struct AstUseDecl *r = AstGetUseDecl(result);
    r->name = parse_name(lex);
    if (test_next(lex, TK_COLON2)) {
        if (test_next(lex, '*')) r->has_star = PAW_TRUE;
        else r->item = parse_name(lex);
    }
    if (test_next(lex, TK_AS)) {
        r->as = parse_name(lex);
    }
    r->line = line;
    semicolon(lex);
    return result;
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
        r->variant.fields = variant_field_list(lex, line);
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
        K_LIST_PUSH(lex->C, list, next);
    } while (test_next(lex, ','));
    delim_next(lex, '}', '{', line);
}

static void enum_body(struct Lex *lex, struct AstAdtDecl *adt)
{
    const int line = lex->line;
    skip(lex);

    ++lex->expr_depth;
    adt->fields = pawAst_decl_list_new(lex->C);
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
    r->field.is_pub = test_next(lex, TK_PUB);
    r->field.name = parse_name(lex);
    r->field.tag = expect_annotation(lex, "field", r->field.name);
    return r;
}

DEFINE_LIST_PARSER(struct_field, '{', '}', LOCAL_MAX, "struct fields", field_decl, AstDeclList)

static struct AstDeclList *struct_field_list(struct Lex *lex, int line)
{
    ++lex->expr_depth;
    struct AstDeclList *list = pawAst_decl_list_new(lex->C);
    parse_struct_field_list(lex, list, line);
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
        adt->fields = struct_field_list(lex, line);
    } else {
        semicolon(lex);
    }
}

static struct AstDecl *struct_decl(struct Lex *lex, paw_Bool pub)
{
    skip(lex); // 'struct' token
    struct AstDecl *r = NEW_DECL(lex, kAstAdtDecl);
    AstGetAdtDecl(r)->is_pub = pub;
    AstGetAdtDecl(r)->is_struct = PAW_TRUE;
    AstGetAdtDecl(r)->name = parse_name(lex);
    AstGetAdtDecl(r)->generics = type_param(lex);
    struct_body(lex, &r->adt);
    return r;
}

static struct AstDecl *method_decl(struct Lex *lex)
{
    const int line = lex->line;
    const paw_Bool is_pub = test_next(lex, TK_PUB);
    check_next(lex, TK_FN);
    String *name = parse_name(lex);
    struct AstDecl *r = function(lex, name, FUNC_METHOD);
    AstGetFuncDecl(r)->is_pub = is_pub;
    AstGetFuncDecl(r)->line = line;
    return r;
}

static struct AstDecl *impl_decl(struct Lex *lex)
{
    skip(lex); // 'impl' token
    struct AstDecl *result = NEW_DECL(lex, kAstImplDecl);
    struct AstImplDecl *r = AstGetImplDecl(result);
    r->name = SCAN_STRING(lex, "(impl)");
    r->generics = type_param(lex);
    r->self = parse_pathtype(lex);

    const int line = lex->line;
    // indicate that 'self' has special meaning
    lex->in_impl = PAW_TRUE;
    check_next(lex, '{');
    r->methods = pawAst_decl_list_new(lex->C);
    while (!end_of_block(lex)) {
        if (r->methods->count == LOCAL_MAX) {
            limit_error(lex, "methods", LOCAL_MAX);
        }
        struct AstDecl *method = method_decl(lex);
        K_LIST_PUSH(lex->C, r->methods, method);
    }
    delim_next(lex, '}', '{', line);
    lex->in_impl = PAW_FALSE;
    return result;
}

static struct AstDecl *type_decl(struct Lex *lex)
{
    struct AstDecl *r = NEW_DECL(lex, kAstTypeDecl);
    skip(lex); // 'type' token

    AstGetTypeDecl(r)->name = parse_name(lex);
    AstGetTypeDecl(r)->generics = type_param(lex);

    check_next(lex, '=');

    // 'type_expr()' parses function signatures, which are not allowed
    // on the RHS of a type expression. This should be caught during
    // type checking, since we also need to make sure the RHS is not
    // referring to an uninstantiated template.
    AstGetTypeDecl(r)->rhs = type_expr(lex);
    semicolon(lex);
    return r;
}

static struct AstDecl *decl(struct Lex *lex)
{
    if (test(lex, TK_LET)) {
        return let_decl(lex, lex->line, PAW_FALSE);
    } else if (test(lex, TK_TYPE)) {
        return type_decl(lex);
    } else {
        pawX_error(lex, "expected 'let' or 'type' declaration");
    }
}

static struct AstStmt *decl_stmt(struct Lex *lex)
{
    struct AstStmt *r = NEW_STMT(lex, kAstDeclStmt);
    r->decl.decl = decl(lex);
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
    if (has_qualifier) pawX_error(lex, "visibility qualifier not allowed here");
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
        case TK_USE:
            ensure_not_pub(lex, is_pub);
            return use_decl(lex);
        case TK_IMPL:
            ensure_not_pub(lex, is_pub);
            return impl_decl(lex);
        case TK_TYPE:
            ensure_not_pub(lex, is_pub);
            return type_decl(lex);
    }
}

static struct AstDeclList *toplevel_items(struct Lex *lex, struct AstDeclList *list)
{
    while (!test_next(lex, TK_END)) {
        const paw_Bool is_pub = test_next(lex, TK_PUB);
        struct AstDecl *item = toplevel_item(lex, is_pub);
        K_LIST_PUSH(lex->C, list, item);
    }
    return list;
}

static const char kPrelude[] =
    "pub struct _List<T>;\n"
    "pub struct _Map<K, V>;\n"

    "pub enum Option<T> {\n"
    "    Some(T),\n"
    "    None,\n"
    "}\n"

    "pub enum Result<T, E> {\n"
    "    Ok(T),\n"
    "    Err(E),\n"
    "}\n"

    "pub fn print(message: str);\n"
    "pub fn assert(cond: bool);\n"
    "pub fn range(begin: int, end: int, step: int) -> (fn() -> Option<int>);\n"

    "impl bool {\n"
    "    pub fn to_string(self) -> str;\n"
    "}\n"

    "impl int {\n"
    "    pub fn to_string(self) -> str;\n"
    "}\n"

    "impl float {\n"
    "    pub fn to_string(self) -> str;\n"
    "}\n"

    "impl str {\n"
    "    pub fn parse_int(self, base: int) -> int;\n"
    "    pub fn parse_float(self) -> float;\n"
    "    pub fn split(self, sep: str) -> [str];\n"
    "    pub fn join(self, seq: [str]) -> str;\n"
    "    pub fn find(self, target: str) -> int;\n"
    "    pub fn starts_with(self, prefix: str) -> bool;\n"
    "    pub fn ends_with(self, suffix: str) -> bool;\n"
    "}\n"

    "impl<T> _List<T> {\n"
    "    pub fn length(self) -> int;\n"
    "    pub fn push(self, value: T) -> Self;\n"
    "    pub fn insert(self, i: int, value: T) -> Self;\n"
    "    pub fn remove(self, i: int) -> T;\n"
    "    pub fn pop(self) -> T;\n"
    "}\n"

    "impl<K, V> _Map<K, V> {\n"
    "    pub fn length(self) -> int;\n"
    "    pub fn get_or(self, key: K, default: V) -> V;\n"
    "    pub fn erase(self, key: K) -> Self;\n"
    "}\n"

    "impl<T> Option<T> {\n"
    "    pub fn is_some(self) -> bool;\n"
    "    pub fn is_none(self) -> bool;\n"
    "    pub fn unwrap(self) -> T;\n"
    "    pub fn unwrap_or(self, value: T) -> T;\n"
    "}\n"

    "impl<T, E> Result<T, E> {\n"
    "    pub fn is_ok(self) -> bool;\n"
    "    pub fn is_err(self) -> bool;\n"
    "    pub fn unwrap(self) -> T;\n"
    "    pub fn unwrap_or(self, value: T) -> T;\n"
    "}\n";

struct PreludeReader {
    size_t size;
};

const char *prelude_reader(paw_Env *P, void *ud, size_t *size)
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
            const char c = lex->c;
            skip(lex); // skip line
            if (ISNEWLINE(c)) break;
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
    const int modno = CAST(int, pawH_length(C->imports)) + 1 /* skip prelude */;
    struct Ast *ast = pawAst_new(C, name, modno);
    pawH_insert(ENV(C), C->imports, P2V(name), P2V(ast));
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
