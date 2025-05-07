// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "parse.h"
#include "ast.h"
#include "compile.h"
#include "env.h"
#include "error.h"
#include "map.h"

#define PARSE_ERROR(X_, Kind_, ...) pawErr_##Kind_((X_)->C, (X_)->modname, __VA_ARGS__)
#define LIMIT_ERROR(x, start, what, limit) PARSE_ERROR(x, too_many_elements, start, what, limit)

#define SELF_TYPENAME(Lex_) SCAN_STRING((Lex_)->C, "Self")
#define SELF_VARNAME(Lex_) CACHED_STRING(ENV(Lex_), CSTR_SELF)

#define NEW_NODE_0(Lex_, Kind_, Start_) \
    pawAst_new_##Kind_((Lex_)->ast, span_from(Lex_, Start_));
#define NEW_NODE(Lex_, Kind_, Start_, ...) \
    pawAst_new_##Kind_((Lex_)->ast, span_from(Lex_, Start_), __VA_ARGS__);

static struct SourceSpan span_from(struct Lex *lex, struct SourceLoc start)
{
    return (struct SourceSpan){
        .start = start,
        .end = lex->loc,
    };
}

// recursive non-terminals
static struct AstExpr *expression(struct Lex *lex, unsigned prec);
static struct AstStmt *statement(struct Lex *lex);
static struct AstPat *pattern(struct Lex *lex);

static struct AstExpr *expr0(struct Lex *lex)
{
    return expression(lex, 0);
}

static void missing_delim(struct Lex *lex, TokenKind want, TokenKind open, struct SourceLoc start)
{
    PARSE_ERROR(lex, expected_delimiter, lex->loc, want, open, start);
}

static void delim_next(struct Lex *lex, TokenKind want, TokenKind open, struct SourceLoc start)
{
    TokenKind const have = lex->t.kind;
    if (have != want) {
        if (have == TK_GREATER2 && want == '>') {
            // special case: split '>>' into 2 '>'
            lex->t.kind = '>';
            return;
        }
        missing_delim(lex, want, open, start);
    }
    pawX_next(lex);
}

static void enter_expression(struct Lex *lex)
{
    int const MAX_NESTING = 1000;
    if (lex->nest_depth >= MAX_NESTING)
        LIMIT_ERROR(lex, lex->loc, "nested expressions", MAX_NESTING);
    ++lex->nest_depth;
}

static void leave_expression(struct Lex *lex)
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
    INFIX_AS, // as
    INFIX_ADD, // +
    INFIX_SUB, // -
    INFIX_MUL, // *
    INFIX_DIV, // /
    INFIX_MOD, // %
    INFIX_BITXOR, // ^
    INFIX_BITAND, // &
    INFIX_BITOR, // |
    INFIX_SHL, // <<
    INFIX_SHR, // >>
    INFIX_RANGE, // ..
    INFIX_AND, // &&
    INFIX_OR, // ||
    INFIX_ASSIGN, // =
    INFIX_AADD, // +=
    INFIX_ASUB, // -=
    INFIX_AMUL, // *=
    INFIX_ADIV, // /=
    INFIX_AMOD, // %=
    INFIX_ABITXOR, // ^=
    INFIX_ABITAND, // &=
    INFIX_ABITOR, // |=
    INFIX_ASHL, // <<=
    INFIX_ASHR, // >>=

    NINFIX
};

#define NOT_UNOP NUNOPS
#define NOT_INFIX NINFIX

static const struct {
    uint8_t left;
    uint8_t right;
} kInfixPrec[NINFIX] = {
    [INFIX_AS] = {13, 13},
    [INFIX_MUL] = {12, 12},
    [INFIX_DIV] = {12, 12},
    [INFIX_MOD] = {12, 12},
    [INFIX_ADD] = {11, 11},
    [INFIX_SUB] = {11, 11},
    [INFIX_SHL] = {10, 10},
    [INFIX_SHR] = {10, 10},
    [INFIX_BITAND] = {9, 9},
    [INFIX_BITXOR] = {8, 8},
    [INFIX_BITOR] = {7, 7},
    [INFIX_LT] = {6, 6},
    [INFIX_LE] = {6, 6},
    [INFIX_GT] = {6, 6},
    [INFIX_GE] = {6, 6},
    [INFIX_EQ] = {5, 5},
    [INFIX_NE] = {5, 5},
    [INFIX_AND] = {4, 4},
    [INFIX_OR] = {3, 3},
    [INFIX_RANGE] = {2, 2},
    [INFIX_ASSIGN] = {1, 1},
    [INFIX_AADD] = {1, 1},
    [INFIX_ASUB] = {1, 1},
    [INFIX_AMUL] = {1, 1},
    [INFIX_ADIV] = {1, 1},
    [INFIX_AMOD] = {1, 1},
    [INFIX_ABITXOR] = {1, 1},
    [INFIX_ABITAND] = {1, 1},
    [INFIX_ABITOR] = {1, 1},
    [INFIX_ASHL] = {1, 1},
    [INFIX_ASHR] = {1, 1},
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
            return INFIX_BITXOR;
        case '&':
            return INFIX_BITAND;
        case '|':
            return INFIX_BITOR;
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
        case TK_DOT2:
            return INFIX_RANGE;
        case TK_PLUS_EQ:
            return INFIX_AADD;
        case TK_MINUS_EQ:
            return INFIX_ASUB;
        case TK_STAR_EQ:
            return INFIX_AMUL;
        case TK_SLASH_EQ:
            return INFIX_ADIV;
        case TK_PERCENT_EQ:
            return INFIX_AMOD;
        case TK_CARET_EQ:
            return INFIX_ABITXOR;
        case TK_AMPER_EQ:
            return INFIX_ABITAND;
        case TK_PIPE_EQ:
            return INFIX_ABITOR;
        case TK_LESS2_EQ:
            return INFIX_ASHL;
        case TK_GREATER2_EQ:
            return INFIX_ASHR;
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
    if (!test(lex, want))
        PARSE_ERROR(lex, unexpected_symbol, lex->t.span.start);
}

static void check_next(struct Lex *lex, TokenKind want)
{
    check(lex, want);
    skip(lex);
}

static void semicolon(struct Lex *lex, char const *where)
{
    if (!test_next(lex, ';')) {
        // error occurs right after the previous token
        struct SourceLoc const loc = lex->t0.span.end;
        PARSE_ERROR(lex, expected_semicolon, loc, where);
    }
}

static void add_string_part(struct Lex *lex, struct AstStringList *parts, struct SourceLoc start, Value str)
{
    AstStringList_push(lex->ast, parts, (struct AstStringPart){
                .is_str = PAW_TRUE,
                .str.span = span_from(lex, start),
                .str.value = str,
            });
}

static void add_expr_part(struct Lex *lex, struct AstStringList *parts, struct AstExpr *expr)
{
    AstStringList_push(lex->ast, parts, (struct AstStringPart){
                .is_str = PAW_FALSE,
                .expr = expr,
            });
}

static struct AstExpr *string_expr(struct Lex *lex, struct SourceLoc start, Value str)
{
    struct AstStringList *parts = AstStringList_new(lex->ast);
    add_string_part(lex, parts, start, str);
    return NEW_NODE(lex, string_expr, start, parts);
}

// Parse an interpolated string
//
// For example, the string "abc${123}" results in the following tokens:
//
//      index | kind              | payload
//     -------|-------------------|---------
//      1     | STRING_EXPR_OPEN  | "abc"
//      2     | INTEGER           | 123
//      3     | STRING_EXPR_CLOSE | -
//      4     | STRING_TEXT       |  ""
//
static struct AstExpr *string_interp_expr(struct Lex *lex, struct SourceLoc start)
{
    struct AstStringList *parts = AstStringList_new(lex->ast);
    do {
        if (test(lex, TK_END))
            break;
        Value const str = lex->t.value;
        skip(lex);

        add_string_part(lex, parts, start, str);
        add_expr_part(lex, parts, expr0(lex));
        check_next(lex, TK_STRING_EXPR_CLOSE);
    } while (test(lex, TK_STRING_EXPR_OPEN));
    check(lex, TK_STRING_TEXT);
    add_string_part(lex, parts, start, lex->t.value);
    return NEW_NODE(lex, string_expr, start, parts);
}

static struct AstIdent parse_ident(struct Lex *lex)
{
    check(lex, TK_NAME);
    struct SourceLoc start = lex->loc;
    String *name = V_STRING(lex->t.value);
    skip(lex);

    return (struct AstIdent){
        .span = span_from(lex, start),
        .name = name,
    };
}

static void ensure_not_underscore(struct Lex *lex, struct AstIdent ident)
{
    if (is_underscore(lex, ident.name))
        PARSE_ERROR(lex, unexpected_underscore, ident.span.start);
}

static struct AstIdent parse_toplevel_ident(struct Lex *lex)
{
    struct AstIdent ident = parse_ident(lex);
    ensure_not_underscore(lex, ident);
    return ident;
}

// TODO: accept source loc as arg
static struct AstExpr *new_basic_lit(struct Lex *lex, Value value, paw_Type code)
{
    struct SourceLoc start = lex->loc;
    return NEW_NODE(lex, basic_lit, start, value, code);
}

static struct AstExpr *unit_lit(struct Lex *lex)
{
    return new_basic_lit(lex, I2V(0), BUILTIN_UNIT);
}

static struct AstType *unit_type(struct Lex *lex)
{
    struct AstTypeList *types = AstTypeList_new(lex->ast);
    return NEW_NODE(lex, tuple_type, lex->loc, types);
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
    struct SourceLoc start = lex->loc;
    struct AstIdent const empty = {0};
    struct AstType *tag = parse_type(lex, PAW_TRUE);
    return NEW_NODE(lex, field_decl, start, empty, tag, PAW_FALSE);
}

#define DEFINE_LIST_PARSER(Name_, A_, B_, Limit_, What_, Func_, List_)                            \
    static void parse_##Name_##_list(struct Lex *lex, struct List_ *list, struct SourceLoc start) \
    {                                                                                             \
        do {                                                                                      \
            if (test(lex, B_))                                                                    \
                break;                                                                            \
            if (list->count == (Limit_))                                                          \
                LIMIT_ERROR(lex, start, What_, (Limit_));                                         \
            List_##_push((lex)->ast, list, (Func_)(lex));                                         \
        } while (test_next(lex, ','));                                                            \
        delim_next(lex, B_, A_, start);                                                           \
    }
DEFINE_LIST_PARSER(arg, '(', ')', LOCAL_MAX, "arguments", expr0, AstExprList)
DEFINE_LIST_PARSER(variant_field, '(', ')', LOCAL_MAX, "variant fields", variant_field_decl, AstDeclList)
DEFINE_LIST_PARSER(strict_type, '<', '>', LOCAL_MAX, "type arguments", parse_strict_type, AstTypeList)
DEFINE_LIST_PARSER(relaxed_type, '<', '>', LOCAL_MAX, "type arguments", parse_relaxed_type, AstTypeList)

static struct AstTypeList *strict_type_list(struct Lex *lex, struct SourceLoc start)
{
    ++lex->expr_depth;
    struct AstTypeList *list = AstTypeList_new(lex->ast);
    parse_strict_type_list(lex, list, start);
    if (list->count == 0)
        PARSE_ERROR(lex, empty_type_list, start);

    --lex->expr_depth;
    return list;
}

static struct AstTypeList *relaxed_type_list(struct Lex *lex, struct SourceLoc start)
{
    ++lex->expr_depth;
    struct AstTypeList *list = AstTypeList_new(lex->ast);
    parse_relaxed_type_list(lex, list, start);
    if (list->count == 0)
        PARSE_ERROR(lex, empty_type_list, start);

    --lex->expr_depth;
    return list;
}

static struct AstTypeList *maybe_strict_type_args(struct Lex *lex)
{
    return test_next(lex, '<') ? strict_type_list(lex, lex->loc) : NULL;
}

static struct AstTypeList *maybe_relaxed_type_args(struct Lex *lex)
{
    return test_next(lex, '<') ? relaxed_type_list(lex, lex->loc) : NULL;
}

static struct AstPath parse_pathexpr(struct Lex *lex)
{
    struct AstSegments *s = AstSegments_new(lex->ast);
    struct SourceLoc start = lex->loc;

    do {
    next_segment:
        if (s->count == INT_MAX)
            break; // throw error below

        struct AstIdent ident = parse_ident(lex);
        struct AstTypeList *args = NULL;
        // permit "::<types..>" between segments
        if (test_next(lex, TK_COLON2)) {
            ensure_not_underscore(lex, ident);
            args = maybe_relaxed_type_args(lex);
            if (args == NULL) {
                AstSegments_push(lex->ast, s, (struct AstSegment){
                            .ident = ident,
                        });
                goto next_segment;
            }
        }
        AstSegments_push(lex->ast, s, (struct AstSegment){
                    .ident = ident,
                    .types = args,
                });
    } while (test_next(lex, TK_COLON2));

    if (s->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "path segments", LOCAL_MAX);

    return (struct AstPath){
        .span = span_from(lex, start),
        .segments = s,
    };
}

static struct AstPath parse_pathtype(struct Lex *lex, paw_Bool is_strict)
{
    struct AstSegments *s = AstSegments_new(lex->ast);
    struct SourceLoc start = lex->loc;

    do {
        if (s->count == INT_MAX)
            break; // throw error below

        struct AstIdent ident = parse_ident(lex);
        if (is_strict)
            ensure_not_underscore(lex, ident);

        struct SourceLoc start = lex->loc;
        struct AstTypeList *args = NULL;
        if (test_next(lex, '<')) {
            // _<types..> is not allowed
            ensure_not_underscore(lex, ident);
            args = is_strict
                       ? strict_type_list(lex, start)
                       : relaxed_type_list(lex, start);
        }
        AstSegments_push(lex->ast, s, (struct AstSegment){
                    .ident = ident,
                    .types = args,
                });
    } while (test_next(lex, TK_COLON2));

    if (s->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "path segments", LOCAL_MAX);

    return (struct AstPath){
        .span = span_from(lex, start),
        .segments = s,
    };
}

static struct AstExpr *path_expr(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstPath path = parse_pathexpr(lex);
    return NEW_NODE(lex, path_expr, start, path);
}

static struct AstPat *new_path_pat(struct Lex *lex, struct AstIdent ident)
{
    struct AstPath path;
    pawAst_path_init(lex->ast, &path, ident.span);
    pawAst_path_add(lex->ast, &path, ident, NULL);
    return NEW_NODE(lex, path_pat, ident.span.start, path);
}

static struct AstPat *struct_field_pat(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstPat *pat;
    if (test_next(lex, ':')) {
        pat = pattern(lex);
    } else {
        // binds field to variable of same name
        pat = new_path_pat(lex, ident);
    }
    return NEW_NODE(lex, field_pat, start, ident, pat);
}

DEFINE_LIST_PARSER(variant_field_pat, '(', ')', LOCAL_MAX, "variant fields", pattern, AstPatList)
DEFINE_LIST_PARSER(struct_field_pat, '{', '}', LOCAL_MAX, "struct fields", struct_field_pat, AstPatList)

static paw_Bool is_wildcard_path(struct AstPath path)
{
    struct AstSegments *segments = path.segments;
    paw_assert(segments->count > 0);
    if (segments->count > 1)
        return PAW_FALSE;
    struct AstSegment seg = K_LIST_FIRST(segments);
    String const *name = seg.ident.name;
    return pawS_length(name) == 1 && name->text[0] == '_';
}

static enum BuiltinKind get_builtin_kind(struct Lex *lex, struct AstIdent ident)
{
    if (equals_cstr(lex, ident.name, CSTR_BOOL)) {
        return BUILTIN_BOOL;
    } else if (equals_cstr(lex, ident.name, CSTR_INT)) {
        return BUILTIN_INT;
    } else if (equals_cstr(lex, ident.name, CSTR_FLOAT)) {
        return BUILTIN_FLOAT;
    } else if (equals_cstr(lex, ident.name, CSTR_STR)) {
        return BUILTIN_STR;
    } else if (equals_cstr(lex, ident.name, CSTR_LIST)) {
        return BUILTIN_LIST;
    } else if (equals_cstr(lex, ident.name, CSTR_MAP)) {
        return BUILTIN_MAP;
    } else {
        return NBUILTINS;
    }
}

static paw_Bool is_reserved_path(struct Lex *lex, struct AstPath path)
{
    if (path.segments->count > 1)
        return PAW_FALSE;
    struct AstIdent ident = K_LIST_FIRST(path.segments).ident;
    return get_builtin_kind(lex, ident) != NBUILTINS;
}

static struct AstPat *compound_pat(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstPath path = parse_pathexpr(lex);
    if (test_next(lex, '(')) {
        struct AstPatList *fields = AstPatList_new(lex->ast);
        parse_variant_field_pat_list(lex, fields, start);
        return NEW_NODE(lex, variant_pat, start, path, fields);
    } else if (test_next(lex, '{')) {
        struct AstPatList *fields = AstPatList_new(lex->ast);
        parse_struct_field_pat_list(lex, fields, start);
        return NEW_NODE(lex, struct_pat, start, path, fields);
    } else if (is_wildcard_path(path)) {
        return NEW_NODE_0(lex, wildcard_pat, start);
    } else if (!is_reserved_path(lex, path)) {
        return NEW_NODE(lex, path_pat, start, path);
    } else {
        paw_assert(path.segments->count == 1);
        struct AstIdent ident = K_LIST_FIRST(path.segments).ident;
        PARSE_ERROR(lex, reserved_identifier, lex->loc, ident.name->text);
    }
}

static struct AstPat *tuple_pat(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // '(' token
    struct AstPatList *elems = AstPatList_new(lex->ast);
    parse_variant_field_pat_list(lex, elems, start);
    return NEW_NODE(lex, tuple_pat, start, elems);
}

// TODO: handle range patterns
static struct AstExpr *literal_expr(struct Lex *lex)
{
    struct SourceLoc const loc = lex->loc;
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
        case TK_STRING_TEXT:
            expr = string_expr(lex, loc, lex->t.value);
            break;
        default:
            PARSE_ERROR(lex, nonliteral_pattern, lex->loc);
    }
    skip(lex); // literal token

    if (negative) {
        if (AstIsStringExpr(expr))
            PARSE_ERROR(lex, invalid_literal_negation, lex->loc);

        struct AstLiteralExpr *e = AstGetLiteralExpr(expr);
        if (e->basic.code == BUILTIN_INT) {
            e->basic.value.i = -e->basic.value.i;
        } else if (e->basic.code == BUILTIN_FLOAT) {
            e->basic.value.f = -e->basic.value.f;
        } else {
            PARSE_ERROR(lex, invalid_literal_negation, lex->loc);
        }
    }
    return expr;
}

static struct AstPat *literal_pat(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstExpr *expr = literal_expr(lex);
    return NEW_NODE(lex, literal_pat, start, expr);
}

static struct AstPat *or_pat(struct Lex *lex, struct AstPat *lhs)
{
    struct SourceLoc start = lex->loc;
    struct AstPat *rhs = pattern(lex);
    return NEW_NODE(lex, or_pat, start, lhs, rhs);
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

static struct AstDeclList *variant_field_list(struct Lex *lex, struct SourceLoc start)
{
    ++lex->expr_depth;
    struct AstDeclList *list = AstDeclList_new(lex->ast);
    parse_variant_field_list(lex, list, start);
    if (list->count == 0)
        PARSE_ERROR(lex, empty_variant_field_list, start);

    --lex->expr_depth;
    return list;
}

static struct AstType *parse_tuple_type(struct Lex *lex, struct AstType *first, struct SourceLoc start, paw_Bool is_strict)
{
    struct AstTypeList *elems = AstTypeList_new(lex->ast);
    AstTypeList_push(lex->ast, elems, first);

    do {
        if (test(lex, ')'))
            break;
        if (elems->count == FIELD_MAX)
            LIMIT_ERROR(lex, start, "tuple elements", FIELD_MAX);

        struct AstType *type = parse_type(lex, is_strict);
        AstTypeList_push(lex->ast, elems, type);
    } while (test_next(lex, ','));
    delim_next(lex, ')', '(', start);

    return NEW_NODE(lex, tuple_type, start, elems);
}

static struct AstType *parse_paren_type(struct Lex *lex, paw_Bool is_strict)
{
    struct SourceLoc const start = lex->loc;
    if (test_next(lex, ')'))
        return unit_type(lex);

    struct AstType *t = parse_type(lex, is_strict);
    if (test_next(lex, ','))
        return parse_tuple_type(lex, t, start, is_strict);

    delim_next(lex, ')', '(', start);
    return t;
}

static struct AstType *parse_container_type(struct Lex *lex, paw_Bool is_strict)
{
    struct SourceLoc const start = lex->loc;
    struct AstType *first = parse_type(lex, is_strict);
    struct AstType *second = NULL;
    if (test_next(lex, ':'))
        second = parse_type(lex, is_strict);

    delim_next(lex, ']', '[', start);
    return NEW_NODE(lex, container_type, start, first, second);
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
    struct SourceLoc start = lex->loc;
    struct AstPath path = parse_pathtype(lex, is_strict);
    if (is_underscore(lex, K_LIST_FIRST(path.segments).ident.name)) {
        if (path.segments->count > 1) {
            struct AstSegment underscore = K_LIST_FIRST(path.segments);
            PARSE_ERROR(lex, colons_after_underscore, underscore.ident.span.start);
        }
        return NEW_NODE_0(lex, infer_type, start);
    }
    return NEW_NODE(lex, path_type, start, path);
}

static struct AstType *type_annotation(struct Lex *lex, paw_Bool is_strict)
{
    if (test_next(lex, ':'))
        return parse_type(lex, is_strict);
    return NULL; // needs inference
}

static struct AstType *expect_type_annotation(struct Lex *lex, char const *what, struct AstIdent ident, paw_Bool is_strict)
{
    struct AstType *type = type_annotation(lex, is_strict);
    if (type == NULL)
        PARSE_ERROR(lex, expected_type_annotation, ident.span.start, what, ident.name->text);
    return type;
}

static struct AstType *self_type(struct Lex *lex, struct SourceSpan span)
{
    struct AstPath path;
    struct AstIdent ident = {
        .name = SELF_TYPENAME(lex),
        .span = span,
    };
    pawAst_path_init(lex->ast, &path, span);
    pawAst_path_add(lex->ast, &path, ident, NULL);
    return NEW_NODE(lex, path_type, span.start, path);
}

static struct AstDecl *func_param_decl(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstIdent ident = parse_ident(lex);
    int const param_index = lex->param_index++;
    struct AstType *tag;
    if (!lex->in_impl || param_index != 0) {
        // usual case: expect a type annotation on each parameter
        tag = expect_type_annotation(lex, "parameter", ident, PAW_TRUE);
    } else {
        // first parameter to method: 'self' means 'self: Self'
        tag = type_annotation(lex, PAW_TRUE);
        if (tag == NULL) {
            if (!pawS_eq(ident.name, SELF_VARNAME(lex)))
                PARSE_ERROR(lex, expected_self_parameter, ident.span.start, ident.name->text);
            tag = self_type(lex, ident.span);
        }
    }
    return NEW_NODE(lex, field_decl, start, ident, tag, PAW_FALSE);
}

static struct AstDecl *closure_param_decl(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstIdent ident = parse_ident(lex);
    struct AstType *tag = type_annotation(lex, PAW_FALSE);
    return NEW_NODE(lex, field_decl, start, ident, tag, PAW_FALSE);
}

static struct AstBoundList *parse_generic_bounds(struct Lex *lex)
{
    if (!test_next(lex, ':'))
        return NULL;
    struct AstBoundList *bounds = AstBoundList_new(lex->ast);
    do {
        struct AstGenericBound bound;
        bound.path = parse_pathtype(lex, PAW_TRUE);
        AstBoundList_push(lex->ast, bounds, bound);
    } while (test_next(lex, '+'));
    return bounds;
}

static struct AstDecl *generic_param(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstBoundList *bounds = parse_generic_bounds(lex);
    return NEW_NODE(lex, generic_decl, start, ident, bounds);
}

DEFINE_LIST_PARSER(func_param, '(', ')', LOCAL_MAX, "function parameters", func_param_decl, AstDeclList)
DEFINE_LIST_PARSER(sig_param, '(', ')', LOCAL_MAX, "function parameters", parse_strict_type, AstTypeList)
DEFINE_LIST_PARSER(closure_param, '|', '|', LOCAL_MAX, "closure parameters", closure_param_decl, AstDeclList)
DEFINE_LIST_PARSER(generic, '<', '>', LOCAL_MAX, "generics", generic_param, AstDeclList)

static struct AstExpr *sitem_expr(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstExpr *value;
    if (test_next(lex, ':')) {
        value = expr0(lex);
    } else {
        // "name" by itself is shorthand for "name: name"
        struct AstPath path;
        pawAst_path_init(lex->ast, &path, ident.span);
        pawAst_path_add(lex->ast, &path, ident, NULL);
        value = NEW_NODE(lex, path_expr, start, path);
    }
    int const fid = INT_MAX; // nonnegative means determine later
    return NEW_NODE(lex, named_field_expr, start, ident, value, fid);
}

DEFINE_LIST_PARSER(sitem, '{', '}', LOCAL_MAX, "struct items", sitem_expr, AstExprList)

static struct AstExpr *unop_expr(struct Lex *lex, enum UnOp op)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // unary operator token
    enum UnaryOp const unop = CAST(enum UnaryOp, op); // same order
    struct AstExpr *target = expression(lex, kUnOpPrecedence);
    return NEW_NODE(lex, unop_expr, start, unop, target);
}

// Parse either a parenthsized expression or a tuple
static struct AstExpr *paren_expr(struct Lex *lex)
{
    // Just parse and return the expression contained within the parenthesis.
    // There is no need for an extra node type.
    struct SourceLoc start = lex->loc;
    skip(lex); // '(' token
    if (test_next(lex, ')'))
        return unit_lit(lex);

    ++lex->expr_depth;
    struct AstExpr *expr = expr0(lex);
    --lex->expr_depth;
    if (test_next(lex, ')'))
        return NEW_NODE(lex, paren_expr, start, expr);

    check_next(lex, ',');
    struct AstExprList *elems = AstExprList_new(lex->ast);
    AstExprList_push(lex->ast, elems, expr);
    parse_arg_list(lex, elems, start);
    return NEW_NODE(lex, tuple_lit, start, elems);
}

static struct AstType *parse_signature(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    check_next(lex, '(');
    struct AstTypeList *params = AstTypeList_new(lex->ast);
    if (!test_next(lex, ')'))
        parse_sig_param_list(lex, params, start);

    struct AstType *result = test_next(lex, TK_ARROW)
        ? parse_type(lex, PAW_TRUE)
        : unit_type(lex);
    return NEW_NODE(lex, func_type, start, params, result);
}

static paw_Bool end_of_block(struct Lex *lex)
{
    return test(lex, '}') || // found proper end
           test(lex, TK_END); // truncated block
}

static struct AstExpr *index_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // '[' token
    struct AstExpr *first = NULL;
    if (!test(lex, ':'))
        first = expr0(lex);

    paw_Bool is_slice = PAW_FALSE;
    struct AstExpr *second = NULL;
    if (test_next(lex, ':')) {
        paw_assert(PAW_FALSE); // TODO: use range expr. ("a[b..c]" instead of "a[b:c]")

        is_slice = PAW_TRUE;
        if (!test(lex, ']'))
            second = expr0(lex);
    }
    delim_next(lex, ']', '[', start);
    return NEW_NODE(lex, index, start, target, first, second, is_slice);
}

static paw_Type parse_container_items(struct Lex *lex, struct AstExprList *items)
{
    struct SourceLoc start = lex->loc;
    enum BuiltinKind code = BUILTIN_UNIT;
    do {
        if (test(lex, ']'))
            break;
        if (items->count == INT_MAX)
            break; // throw error below

        struct AstExpr *item = expr0(lex);
        if (!test_next(lex, ':')) {
            if (code == BUILTIN_MAP)
                PARSE_ERROR(lex, expected_colon_after_map_key, item->hdr.span.end);
            code = BUILTIN_LIST;
        } else if (code == BUILTIN_LIST) {
            PARSE_ERROR(lex, colon_after_list_element, lex->loc);
        } else {
            code = BUILTIN_MAP;
            struct AstExpr *value = expr0(lex);
            item = NEW_NODE(lex, keyed_field_expr, start, item, value);
        }
        AstExprList_push(lex->ast, items, item);
    } while (test_next(lex, ','));

    if (items->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "elements in container literal", LOCAL_MAX);

    // loop body is run at least once
    paw_assert(code != BUILTIN_UNIT);
    return code;
}

static struct AstExpr *container_lit(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // '[' token
    struct AstExprList *items = AstExprList_new(lex->ast);
    enum BuiltinKind b_kind;
    if (test(lex, ']')) {
        b_kind = BUILTIN_LIST; // empty list: '[]'
    } else if (test_next(lex, ':')) {
        b_kind = BUILTIN_MAP; // empty map: '[:]'
    } else {
        b_kind = parse_container_items(lex, items);
    }
    delim_next(lex, ']', '[', start);
    return NEW_NODE(lex, container_lit, start, items, b_kind);
}

// Parse a composite literal expression
static struct AstExpr *composite_lit(struct Lex *lex, struct AstPathExpr *path)
{
    skip(lex); // '{' token
    struct AstExprList *items = AstExprList_new(lex->ast);
    parse_sitem_list(lex, items, path->span.start);
    return pawAst_new_composite_lit(lex->ast, path->span, path->path, items);
}

static struct AstExpr *try_composite_lit(struct Lex *lex, struct AstExpr *expr)
{
    if (AstIsPathExpr(expr) && lex->expr_depth >= 0)
        expr = composite_lit(lex, AstGetPathExpr(expr));

    return expr;
}

static struct AstExpr *selector_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // '.' token
    if (test(lex, TK_NAME)) {
        struct AstIdent ident = parse_toplevel_ident(lex);
        return NEW_NODE(lex, name_selector, start, target, ident);
    } else if (test(lex, TK_INTEGER)) {
        int const index = V_INT(lex->t.value);
        skip(lex); // integer token
        return NEW_NODE(lex, index_selector, start, target, index);
    }

    PARSE_ERROR(lex, invalid_selector, start); // no return
}

static struct AstExpr *call_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // '(' token
    struct AstExprList *args = AstExprList_new(lex->ast);
    parse_arg_list(lex, args, start);
    return NEW_NODE(lex, call_expr, start, target, args);
}

static struct AstExpr *chain_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // '?' token

    if (lex->func_depth == 0)
        PARSE_ERROR(lex, chain_outside_function, start);

    return NEW_NODE(lex, chain_expr, start, target);
}

static struct AstDeclList *func_parameters(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    check_next(lex, '(');
    struct AstDeclList *list = AstDeclList_new(lex->ast);
    lex->param_index = 0; // 'self' allowed if '.in_impl'
    parse_func_param_list(lex, list, start);
    return list;
}

static struct AstDeclList *closure_params(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    check_next(lex, '|');
    struct AstDeclList *list = AstDeclList_new(lex->ast);
    parse_closure_param_list(lex, list, start);
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
    struct SourceLoc start = lex->loc;
    check_next(lex, '{');
    struct AstStmtList *stmts = AstStmtList_new(lex->ast);
    struct AstExpr *result = NULL;
    while (!end_of_block(lex)) {
        struct AstStmt *next = statement(lex);
        if (next == NULL)
            continue; // extra ';'
        AstStmtList_push(lex->ast, stmts, next);
        if (AstIsExprStmt(next) && !test_next(lex, ';')) {
            struct AstExprStmt *s = AstGetExprStmt(next);
            if (expects_semicolon(s->expr) || test(lex, '}')) {
                AstStmtList_pop(stmts);
                result = s->expr;
                break;
            }
        }
    }
    if (result == NULL)
        result = unit_lit(lex);
    delim_next(lex, '}', '{', start);
    return NEW_NODE(lex, block, start, stmts, result);
}

static struct AstExpr *closure(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstDeclList *params = closure_params(lex);
    struct AstType *result = NULL;
    struct AstExpr *expr;
    if (test_next(lex, TK_ARROW)) {
        result = parse_type(lex, PAW_FALSE);
        expr = block_expr(lex);
    } else {
        expr = expr0(lex);
    }
    return NEW_NODE(lex, closure_expr, start, params, result, expr);
}

static struct AstExpr *if_expr(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
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
    return NEW_NODE(lex, if_expr, start, cond, then_arm, else_arm);
}

static struct AstExpr *loop_block(struct Lex *lex)
{
    ++lex->loop_depth;
    struct AstExpr *block = block_expr(lex);
    --lex->loop_depth;
    return block;
}

static struct AstExpr *for_expr(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'for' token
    struct AstPat *pat = pattern(lex);
    check_next(lex, TK_IN);
    struct AstExpr *target = basic_expr(lex);
    struct AstExpr *block = loop_block(lex);
    return NEW_NODE(lex, for_expr, start, pat, target, block);
}

static struct AstExpr *while_expr(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'while' token
    struct AstExpr *cond = basic_expr(lex);
    struct AstExpr *block = loop_block(lex);
    return NEW_NODE(lex, while_expr, start, cond, block);
}

static struct AstExpr *return_expr(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'return' token

    if (lex->func_depth == 0)
        PARSE_ERROR(lex, return_outside_function, start);

    struct AstExpr *expr = NULL;
    if (!test(lex, '}')
            && !test(lex, ';')
            && !test(lex, ',')) {
        expr = expr0(lex);
    }
    return NEW_NODE(lex, return_expr, start, expr);
}

static struct AstExpr *jump_expr(struct Lex *lex, enum JumpKind kind)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'break' or 'continue' token

    if (lex->loop_depth == 0)
        PARSE_ERROR(lex, jump_outside_loop, start,
                kind == JUMP_BREAK ? "break" : "continue");

    return NEW_NODE(lex, jump_expr, start, kind);
}

static struct AstExpr *match_arm(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstPat *pat = pattern(lex);
    struct AstExpr *guard = NULL;
    if (test_next(lex, TK_IF))
        guard = basic_expr(lex);

    check_next(lex, TK_FAT_ARROW);
    struct AstExpr *result = expr0(lex);
    return NEW_NODE(lex, match_arm, start, pat, guard, result);
}

static struct AstExpr *match_expr(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'match' token
    struct AstExpr *target = basic_expr(lex);
    struct AstExprList *arms = AstExprList_new(lex->ast);
    check_next(lex, '{');
    do {
        if (test(lex, '}'))
            break;
        struct AstExpr *arm = match_arm(lex);
        AstExprList_push(lex->ast, arms, arm);
    } while (test_next(lex, ','));
    delim_next(lex, '}', '{', start);
    return NEW_NODE(lex, match_expr, start, target, arms);
}

// TODO: accept source loc as argument
static struct AstExpr *primary_expr(struct Lex *lex)
{
    struct SourceLoc const loc = lex->loc;

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
        case TK_STRING_TEXT:
            expr = string_expr(lex, loc, lex->t.value);
            skip(lex);
            break;
        case TK_STRING_EXPR_OPEN:
            expr = string_interp_expr(lex, loc);
            skip(lex);
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
    return expr;
}

static struct AstExpr *suffixed_expr(struct Lex *lex)
{
    struct AstExpr *e = primary_expr(lex);
    if (e == NULL)
        return NULL;

    if (AstIsBlock(e)
            || AstIsIfExpr(e)
            || AstIsMatchExpr(e)
            || AstIsWhileExpr(e)
            || AstIsForExpr(e))
        return e;

    if (test(lex, '{'))
        e = try_composite_lit(lex, e);

    for (int n = 1;; ++n) {
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
    struct SourceLoc start = lex->loc;
    struct AstIdent const ident = parse_ident(lex);

    enum BuiltinKind to;
    if (equals_cstr(lex, ident.name, CSTR_BOOL)) {
        to = BUILTIN_BOOL;
    } else if (equals_cstr(lex, ident.name, CSTR_INT)) {
        to = BUILTIN_INT;
    } else if (equals_cstr(lex, ident.name, CSTR_FLOAT)) {
        to = BUILTIN_FLOAT;
    } else {
        PARSE_ERROR(lex, expected_basic_type, start, ident.name->text);
    }
    return NEW_NODE(lex, conversion_expr, start, lhs, to);
}

static enum BinaryOp into_binary_op(enum InfixOp op)
{
    switch (op) {
        case INFIX_AADD:
            return BINARY_ADD;
        case INFIX_ASUB:
            return BINARY_SUB;
        case INFIX_AMUL:
            return BINARY_MUL;
        case INFIX_ADIV:
            return BINARY_DIV;
        case INFIX_AMOD:
            return BINARY_MOD;
        case INFIX_ABITXOR:
            return BINARY_BXOR;
        case INFIX_ABITAND:
            return BINARY_BAND;
        case INFIX_ABITOR:
            return BINARY_BOR;
        case INFIX_ASHL:
            return BINARY_SHL;
        case INFIX_ASHR:
            return BINARY_SHR;
        default:
            PAW_UNREACHABLE();
    }
}

static struct AstExpr *op_assignment_expr(struct Lex *lex, struct AstExpr *lhs, enum InfixOp op)
{
    struct SourceLoc start = lex->loc;
    struct AstExpr *rhs = expression(lex, right_prec(op));
    enum BinaryOp const binop = into_binary_op(op);
    return NEW_NODE(lex, op_assign_expr, start, lhs, rhs, binop);
}

static struct AstExpr *assignment_expr(struct Lex *lex, struct AstExpr *lhs)
{
    struct SourceLoc start = lex->loc;
    struct AstExpr *rhs = expression(lex, right_prec(INFIX_ASSIGN));
    return NEW_NODE(lex, assign_expr, start, lhs, rhs);
}

static struct AstExpr *binop_expr(struct Lex *lex, enum InfixOp op, struct AstExpr *lhs)
{
    struct SourceLoc start = lex->loc;
    struct AstExpr *rhs = expression(lex, right_prec(op));
    enum BinaryOp const binop = CAST(enum BinaryOp, op); // same order
    return NEW_NODE(lex, binop_expr, start, binop, lhs, rhs);
}

static struct AstExpr *logical_expr(struct Lex *lex, struct AstExpr *lhs, paw_Bool is_and)
{
    struct SourceLoc start = lex->loc;
    int const prec = right_prec(is_and ? INFIX_AND : INFIX_OR);
    struct AstExpr *rhs = expression(lex, prec);
    return NEW_NODE(lex, logical_expr, start, lhs, rhs, is_and);
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
        case INFIX_AADD:
        case INFIX_ASUB:
        case INFIX_AMUL:
        case INFIX_ADIV:
        case INFIX_AMOD:
        case INFIX_ABITXOR:
        case INFIX_ABITAND:
        case INFIX_ABITOR:
        case INFIX_ASHL:
        case INFIX_ASHR:
            return op_assignment_expr(lex, lhs, op);
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
    enter_expression(lex);
    struct SourceLoc start = lex->loc;
    struct AstExpr *expr = subexpr(lex, prec);
    leave_expression(lex);
    if (expr == NULL)
        PARSE_ERROR(lex, expected_expression, start);
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

static struct AstStmt *let_stmt(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'let' token
    struct AstPat *pat = pattern(lex);
    struct AstType *tag = type_annotation(lex, PAW_FALSE);
    struct AstExpr *init = test_next(lex, '=') ? expr0(lex) : NULL;
    semicolon(lex, "'let' declaration");
    return NEW_NODE(lex, let_stmt, start, pat, tag, init);
}

static struct AstStmt *expr_stmt(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    struct AstExpr *expr = expr0(lex);
    return NEW_NODE(lex, expr_stmt, start, expr);
}

static struct AstDeclList *type_param(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    if (test_next(lex, '<')) {
        ++lex->expr_depth;
        struct AstDeclList *list = AstDeclList_new(lex->ast);
        parse_generic_list(lex, list, start);
        --lex->expr_depth;

        if (list->count == 0)
            PARSE_ERROR(lex, empty_type_list, start);
        return list;
    }
    return NULL;
}

static struct AstExpr *function_body(struct Lex *lex)
{
    ++lex->func_depth;
    struct AstExpr *body = block_expr(lex);
    --lex->func_depth;
    return body;
}

static struct AstDecl *function(struct Lex *lex, struct SourceLoc start, struct AstIdent ident, struct Annotations *annos, enum FuncKind kind, paw_Bool is_pub)
{
    struct AstDeclList *generics = type_param(lex);
    struct AstDeclList *params = func_parameters(lex);
    struct AstType *result = test_next(lex, TK_ARROW) ? parse_type(lex, PAW_TRUE) : unit_type(lex);
    struct AstExpr *body = !test_next(lex, ';') ? function_body(lex) : NULL;

    return NEW_NODE(lex, func_decl, start, kind, ident, annos,
            generics, params, NULL, result, body, is_pub);
}

static struct AstDecl *use_decl(struct Lex *lex)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'use' token
    struct AstIdent ident = parse_toplevel_ident(lex);
    paw_Bool star = PAW_FALSE;
    struct AstIdent item = {0};
    struct AstIdent as = {0};
    if (test_next(lex, TK_COLON2)) {
        if (test_next(lex, '*')) {
            star = PAW_TRUE;
        } else {
            item = parse_toplevel_ident(lex);
        }
    }
    if (test_next(lex, TK_AS)) {
        as = parse_toplevel_ident(lex);
    }
    semicolon(lex, "'use' declaration");
    return NEW_NODE(lex, use_decl, start, ident, star, item, as);
}

static void parse_trait_list(struct Lex *lex, struct AstTypeList *traits)
{
    do {
        if (test(lex, '{'))
            break;
        struct SourceLoc start = lex->loc;
        struct AstPath path = parse_pathtype(lex, PAW_TRUE);
        struct AstType *trait = NEW_NODE(lex, path_type, start, path);
        AstTypeList_push(lex->ast, traits, trait);
    } while (test_next(lex, '+'));
}

static struct Annotations *annotations(struct Lex *lex)
{
    struct Compiler *C = lex->C;
    struct SourceLoc start = lex->loc;
    if (!test_next(lex, '#'))
        return NULL;

    StringMap *names = StringMap_new_from(C, lex->pool);
    struct Annotations *annos = Annotations_new(C);
    check_next(lex, '[');
    do {
        if (test(lex, ']'))
            break;
        struct AstIdent ident = parse_ident(lex);
        struct Annotation anno = {
            .modname = lex->modname,
            .name = ident.name,
            .span = ident.span,
        };
        if (StringMap_insert(C, names, anno.name, NULL))
            PARSE_ERROR(lex, duplicate_annotation, ident.span.start, anno.name->text);

        if (test_next(lex, '=')) {
            anno.has_value = PAW_TRUE;
            struct AstExpr *expr = expr0(lex);
            if (!AstIsLiteralExpr(expr))
                PARSE_ERROR(lex, nonliteral_annotation_value, expr->hdr.span.start, ident.name->text);

            struct AstLiteralExpr *e = AstGetLiteralExpr(expr);
            if (e->lit_kind != kAstBasicLit)
                PARSE_ERROR(lex, nonprimitive_annotation_value, expr->hdr.span.start, ident.name->text);

            anno.value = e->basic.value;
            anno.kind = e->basic.code;
        }
        Annotations_push(C, annos, anno);
    } while (test_next(lex, ','));
    delim_next(lex, ']', '[', start);
    StringMap_delete(C, names);

    return annos;
}

static struct AstDecl *func_decl(struct Lex *lex, struct Annotations *annos, paw_Bool is_pub)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'fn' token
    struct AstIdent ident = parse_toplevel_ident(lex);
    return function(lex, start, ident, annos, FUNC_FUNCTION, is_pub);
}

static struct AstDecl *parse_method(struct Lex *lex, struct Annotations *annos, paw_Bool is_pub)
{
    struct SourceLoc start = lex->loc;
    check_next(lex, TK_FN);
    // indicate that 'self' has special meaning
    lex->in_impl = PAW_TRUE;
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstDecl *method = function(lex, start, ident, annos, FUNC_METHOD, is_pub);
    lex->in_impl = PAW_FALSE;
    return method;
}

static struct AstDecl *variant_decl(struct Lex *lex, int index)
{
    struct SourceLoc start = lex->loc;
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstDeclList *fields = NULL;
    if (test_next(lex, '(')) {
        fields = variant_field_list(lex, start);
    }
    return NEW_NODE(lex, variant_decl, start, ident, fields, index);
}

static void enum_body(struct Lex *lex, struct SourceLoc start, struct AstTypeList *traits, struct AstDeclList *variants, struct AstDeclList *methods)
{
    if (test_next(lex, ':')) {
        parse_trait_list(lex, traits);
    }
    check_next(lex, '{');
    while (!end_of_block(lex)) {
        if (test(lex, TK_NAME)) {
            if (variants->count == INT_MAX)
                break; // throw error below

            struct AstDecl *variant = variant_decl(lex, variants->count);
            AstDeclList_push(lex->ast, variants, variant);
            if (!test_next(lex, ',') && !test(lex, '}'))
                PARSE_ERROR(lex, expected_comma_separator, variant->hdr.span.end, "enum variant");
        } else if (methods->count == INT_MAX) {
            break; // throw error below
        } else {
            struct Annotations *annos = annotations(lex);
            paw_Bool const is_pub = test_next(lex, TK_PUB);
            struct AstDecl *method = parse_method(lex, annos, is_pub);
            AstDeclList_push(lex->ast, methods, method);
        }
    }
    delim_next(lex, '}', '{', start);

    if (variants->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "variants in enumeration", LOCAL_MAX);

    if (methods->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "methods in enumeration", LOCAL_MAX);

    if (variants->count == 0)
        PARSE_ERROR(lex, empty_enumeration, start);
}

static struct AstDecl *enum_decl(struct Lex *lex, paw_Bool is_pub, paw_Bool is_inline)
{
    skip(lex); // 'enum' token
    struct SourceLoc start = lex->loc;
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstDeclList *generics = type_param(lex);
    struct AstTypeList *traits = AstTypeList_new(lex->ast);
    struct AstDeclList *variants = AstDeclList_new(lex->ast);
    struct AstDeclList *methods = AstDeclList_new(lex->ast);
    enum_body(lex, start, traits, variants, methods);
    return NEW_NODE(lex, adt_decl, start, ident, traits, generics,
            variants, methods, is_pub, PAW_FALSE, is_inline);
}

static struct AstDecl *struct_field(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc start = lex->loc;
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstType *tag = expect_type_annotation(lex, "field", ident, PAW_TRUE);
    return NEW_NODE(lex, field_decl, start, ident, tag, is_pub);
}

static void struct_body(struct Lex *lex, struct AstTypeList *traits, struct AstDeclList *fields, struct AstDeclList *methods)
{
    struct SourceLoc start = lex->loc;
    if (test_next(lex, ':')) {
        parse_trait_list(lex, traits);
    }
    if (!test_next(lex, '{')) {
        semicolon(lex, "body of unit struct");
        return;
    }

    while (!end_of_block(lex)) {
        struct SourceLoc field_start = lex->loc;
        struct Annotations *annos = annotations(lex);
        paw_Bool const is_pub = test_next(lex, TK_PUB);
        if (test(lex, TK_NAME)) {
            if (fields->count == INT_MAX)
                break; // throw error below

            struct AstDecl *field = struct_field(lex, is_pub);
            AstDeclList_push(lex->ast, fields, field);
            if (!test_next(lex, ',') && !test(lex, '}'))
                PARSE_ERROR(lex, expected_comma_separator, field->hdr.span.end, "struct field");

        } else if (methods->count == INT_MAX) {
            break; // throw error below
        } else {
            struct AstDecl *method = parse_method(lex, annos, is_pub);
            AstDeclList_push(lex->ast, methods, method);
        }
    }
    delim_next(lex, '}', '{', start);

    if (fields->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "fields in structure", LOCAL_MAX);

    if (methods->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "methods in structure", LOCAL_MAX);

    if (fields->count == 0 && methods->count == 0)
        PARSE_ERROR(lex, empty_struct_body, start);
}

static struct AstDecl *struct_decl(struct Lex *lex, paw_Bool is_pub, paw_Bool is_inline)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'struct' token
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstDeclList *generics = type_param(lex);
    struct AstTypeList *traits = AstTypeList_new(lex->ast);
    struct AstDeclList *fields = AstDeclList_new(lex->ast);
    struct AstDeclList *methods = AstDeclList_new(lex->ast);
    struct_body(lex, traits, fields, methods);
    return NEW_NODE(lex, adt_decl, start, ident, traits, generics,
            fields, methods, is_pub, PAW_TRUE, is_inline);
}

static struct AstDecl *trait_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'trait' token
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstDeclList *generics = type_param(lex);

    check_next(lex, '{');
    struct AstDeclList *methods = AstDeclList_new(lex->ast);
    while (!end_of_block(lex)) {
        if (methods->count == INT_MAX)
            break; // throw error below

        struct AstDecl *method = parse_method(lex, NULL, is_pub);
        AstDeclList_push(lex->ast, methods, method);
    }
    delim_next(lex, '}', '{', start);

    if (methods->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "methods in trait", LOCAL_MAX);

    return NEW_NODE(lex, trait_decl, start, ident, generics, methods, is_pub);
}

static struct AstDecl *type_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc start = lex->loc;
    skip(lex); // 'type' token

    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstDeclList *generics = type_param(lex);

    check_next(lex, '=');

    struct AstType *rhs = parse_type(lex, PAW_TRUE);
    if (AstIsFuncType(rhs))
        PARSE_ERROR(lex, function_type_decl, start);

    semicolon(lex, "type declaration");
    return NEW_NODE(lex, type_decl, start, ident, generics, rhs, is_pub);
}

static struct AstStmt *decl_stmt(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstDecl *decl = type_decl(lex, PAW_FALSE);
    return NEW_NODE(lex, decl_stmt, start, decl);
}

static struct AstStmt *statement(struct Lex *lex)
{
    struct AstStmt *stmt;
    switch (lex->t.kind) {
        case ';':
            skip(lex); // ';' token
            stmt = NULL;
            break;
        case TK_LET:
            stmt = let_stmt(lex);
            break;
        case TK_TYPE:
            stmt = decl_stmt(lex);
            break;
        default:
            stmt = expr_stmt(lex);
    }
    return stmt;
}

static struct AstDecl *const_decl(struct Lex *lex, struct Annotations *annos, paw_Bool is_pub)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'const' token
    struct AstIdent ident = parse_toplevel_ident(lex);
    struct AstType *tag = expect_type_annotation(lex, "constant", ident, PAW_TRUE);
    struct AstExpr *init = test_next(lex, '=') ? expr0(lex) : NULL;
    semicolon(lex, "constant declaration");
    return NEW_NODE(lex, const_decl, start, ident, annos, tag, init, is_pub);
}

static void ensure_not_pub(struct Lex *lex, struct SourceLoc loc, paw_Bool has_qualifier)
{
    if (has_qualifier)
        PARSE_ERROR(lex, unexpected_visibility_qualifier, loc);
}

static struct AstDecl *toplevel_item(struct Lex *lex)
{
    struct Annotations *annos = annotations(lex);
    struct SourceLoc const start = lex->loc;
    paw_Bool const is_pub = test_next(lex, TK_PUB);
    switch (lex->t.kind) {
        default:
            PARSE_ERROR(lex, expected_toplevel_item, start);
        case TK_FN:
            return func_decl(lex, annos, is_pub);
        case TK_CONST:
            return const_decl(lex, annos, is_pub);
        case TK_ENUM:
            return enum_decl(lex, is_pub, PAW_FALSE);
        case TK_STRUCT:
            return struct_decl(lex, is_pub, PAW_FALSE);
        case TK_TRAIT:
            return trait_decl(lex, is_pub);
        case TK_TYPE:
            return type_decl(lex, is_pub);
        case TK_USE:
            ensure_not_pub(lex, start, is_pub);
            return use_decl(lex);
        case TK_INLINE:
            skip(lex); // skip 'inline' token
            if (test(lex, TK_STRUCT)) {
                return struct_decl(lex, is_pub, PAW_TRUE);
            } else if (test(lex, TK_ENUM)) {
                return enum_decl(lex, is_pub, PAW_TRUE);
            } else {
                PARSE_ERROR(lex, unexpected_symbol, start);
            }
    }
}

static struct AstDeclList *toplevel_items(struct Lex *lex, struct AstDeclList *list)
{
    while (!test(lex, TK_END)) {
        struct AstDecl *item = toplevel_item(lex);
        AstDeclList_push(lex->ast, list, item);
    }
    return list;
}

// TODO: someday, #embed should be used for this... once C23 support is better
// TODO: defaulted trait methods: Equals::ne, Compare::gt, and Compare::ge
// TODO: should Equals and Compare be generic?
static char const kPrelude[] =
    "pub trait Hash {\n"
    "    fn hash(self) -> int;\n"
    "}\n"

    "pub trait Equals {\n"
    "    fn eq(self, rhs: Self) -> bool;\n"
    "}\n"

    "pub trait Compare {\n"
    "    fn lt(self, rhs: Self) -> bool;\n"
    "    fn le(self, rhs: Self) -> bool;\n"
    "}\n"

    "pub trait Increment {\n"
    "    fn incremented(self) -> Self;\n"
    "}\n"

    "pub trait Iterate<I: Advance<T>, T> {\n"
    "    fn iterator(self) -> I;\n"
    "}\n"

    "pub trait Advance<T> {\n"
    "    fn next(self) -> Option<T>;\n"
    "}\n"

    "pub struct unit: Hash + Equals {\n"
    "    pub fn hash(self) -> int { 0 }\n"
    "    pub fn eq(self, rhs: Self) -> bool { true }\n"
    "}\n"

    "pub struct bool: Hash + Equals + Compare {\n"
    "    #[extern] pub fn to_string(self) -> str;\n"
    "    pub fn hash(self) -> int { self as int }\n"
    "    pub fn eq(self, rhs: Self) -> bool { self == rhs }\n"
    "    pub fn lt(self, rhs: Self) -> bool { self as int < rhs as int }\n"
    "    pub fn le(self, rhs: Self) -> bool { self as int <= rhs as int }\n"
    "}\n"

    "pub struct int: Hash + Equals + Compare + Increment {\n"
    "    #[extern] pub fn to_string(self) -> str;\n"
    "    pub fn hash(self) -> int { self }\n"
    "    pub fn eq(self, rhs: Self) -> bool { self == rhs }\n"
    "    pub fn lt(self, rhs: Self) -> bool { self < rhs }\n"
    "    pub fn le(self, rhs: Self) -> bool { self <= rhs }\n"
    "    pub fn incremented(self) -> Self { self + 1 }\n"
    "}\n"

    "pub struct float: Hash + Equals + Compare {\n"
    "    #[extern] pub fn to_string(self) -> str;\n"
    "    #[extern] pub fn hash(self) -> int;\n"
    "    pub fn eq(self, rhs: Self) -> bool { self == rhs }\n"
    "    pub fn lt(self, rhs: Self) -> bool { self < rhs }\n"
    "    pub fn le(self, rhs: Self) -> bool { self <= rhs }\n"
    "}\n"

    "pub struct str: Hash + Equals + Compare {\n"
    "    #[extern] pub fn parse_int(self, base: int) -> int;\n"
    "    #[extern] pub fn parse_float(self) -> float;\n"
    "    #[extern] pub fn split(self, sep: str) -> [str];\n"
    "    #[extern] pub fn join(self, seq: [str]) -> str;\n"
    "    #[extern] pub fn find(self, target: str) -> int;\n"
    "    #[extern] pub fn starts_with(self, prefix: str) -> bool;\n"
    "    #[extern] pub fn ends_with(self, suffix: str) -> bool;\n"
    "    #[extern] pub fn hash(self) -> int;\n"
    "    pub fn to_string(self) -> str { self }\n"
    "    pub fn eq(self, rhs: Self) -> bool { self == rhs }\n"
    "    pub fn lt(self, rhs: Self) -> bool { self < rhs }\n"
    "    pub fn le(self, rhs: Self) -> bool { self <= rhs }\n"
    "}\n"

    "pub struct ListIterator<T>: Advance<T> {\n"
    "    list: [T],\n"
    "    index: int,\n"
    "    pub fn new(list: [T]) -> Self {\n"
    "        Self{list, index: 0}\n"
    "    }\n"
    "    pub fn next(self) -> Option<T> {\n"
    "        if self.index < #self.list {\n"
    "            let i = self.index;\n"
    "            self.index = i + 1;\n"
    "            Option::Some(self.list[i])\n"
    "        } else {\n"
    "            Option::None\n"
    "        }\n"
    "    }\n"
    "}\n"

    "pub struct List<T>: Iterate<ListIterator<T>, T> {\n"
    "    #[extern] pub fn length(self) -> int;\n"
    "    #[extern] pub fn get(self, index: int) -> Option<T>;\n"
    "    #[extern] pub fn set(self, index: int, value: T);\n"
    "    #[extern] pub fn push(self, value: T) -> Self;\n"
    "    #[extern] pub fn insert(self, index: int, value: T) -> Self;\n"
    "    #[extern] pub fn remove(self, index: int) -> T;\n"
    "    #[extern] pub fn pop(self) -> T;\n"
    "    pub fn iterator(self) -> ListIterator<T> {\n"
    "        ListIterator::new(self)\n"
    "    }\n"
    "}\n"

    "pub struct MapIterator<K, V>: Advance<K> {\n"
    "    map: [K: V],\n"
    "    index: int,\n"
    "    pub fn new(map: [K: V]) -> Self {\n"
    "        Self{map, index: -1}\n"
    "    }\n"
    "    #[extern] pub fn next(self) -> Option<K>;\n"
    "}\n"

    "pub struct Map<K: Hash + Equals, V>: Iterate<MapIterator<K, V>, K> {\n"
    "    #[extern] pub fn length(self) -> int;\n"
    "    #[extern] pub fn get(self, key: K) -> Option<V>;\n"
    "    #[extern] pub fn set(self, key: K, value: V);\n" // TODO: -> Option<V>;\n"
    "    #[extern] pub fn get_or(self, key: K, default: V) -> V;\n"
    "    #[extern] pub fn erase(self, key: K) -> Self;\n"
    "    pub fn iterator(self) -> MapIterator<K, V> {\n"
    "        MapIterator::new(self)\n"
    "    }\n"
    "}\n"

    "pub inline enum Option<T> {\n"
    "    Some(T),\n"
    "    None,\n"

    "    pub fn is_some(self) -> bool {\n"
    "        !self.is_none()\n"
    "    }\n"
    "    pub fn is_none(self) -> bool {\n"
    "        match self {\n"
    "            Option::None => true,\n"
    "            _ => false,\n"
    "        }\n"
    "    }\n"
    "    pub fn unwrap_or(self, value: T) -> T {\n"
    "        match self {\n"
    "            Option::Some(t) => t,\n"
    "            Option::None => value,\n"
    "        }\n"
    "    }\n"
    "    #[extern] pub fn unwrap(self) -> T;\n"
    "}\n"

    "pub inline enum Result<T, E> {\n"
    "    Ok(T),\n"
    "    Err(E),\n"

    "    pub fn is_ok(self) -> bool {\n"
    "        !self.is_err()\n"
    "    }\n"
    "    pub fn is_err(self) -> bool {\n"
    "        match self {\n"
    "            Result::Err(_) => true,\n"
    "            _ => false,\n"
    "        }\n"
    "    }\n"
    "    pub fn unwrap_or(self, value: T) -> T {\n"
    "        match self {\n"
    "            Result::Ok(t) => t,\n"
    "            _ => value,\n"
    "        }\n"
    "    }\n"
    "    #[extern] pub fn unwrap_err(self) -> E;\n"
    "    #[extern] pub fn unwrap(self) -> T;\n"
    "}\n"

    "pub struct RangeIterator<T: Compare + Increment>: Advance<T> {\n"
    "    end: T,\n"
    "    iter: T,\n"
    "    pub fn new(range: Range<T>) -> Self {\n"
    "        Self{iter: range.start, end: range.end}\n"
    "    }\n"
    "    pub fn next(self) -> Option<T> {\n"
    "        let iter = self.iter;\n"
    "        if iter.lt(self.end) {\n"
    "            self.iter = self.iter.incremented();\n"
    "            Option::Some(iter)\n"
    "        } else {\n"
    "            Option::None\n"
    "        }\n"
    "    }\n"
    "}\n"

    // TODO: "T: Increment" requirement is overly restrictive. Exists so that
    //       "Range<int>" can be used in "for..in" loops. Should figure out a
    //       way to do "conditional conformances" (Swift parlance) so that
    //       "Iterate" can be implemented only for "T"s that are incrementable.
    "pub struct Range<T: Compare + Increment>: Iterate<RangeIterator<T>, T> {\n"
    "    pub start: T,\n"
    "    pub end: T,\n"
    "    pub fn contains(self, value: T) -> bool {\n"
    "        self.start.le(value) && value.lt(self.end)\n"
    "    }\n"
    "    pub fn iterator(self) -> RangeIterator<T> {\n"
    "        RangeIterator::new(self)\n"
    "    }\n"
    "}\n"

    "#[extern] pub fn print(message: str);\n"
    "#[extern] pub fn panic(message: str);\n"
    "#[extern] pub fn assert(cond: bool);\n";

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
    // Special case: don't advance past the '#', since it might be the start of
    // an annotation. If the first char is not '\0', then there is guaranteed to
    // be another char, possibly '\0' itself.
    if (lex->ptr[0] == '#' && lex->ptr[1] == '!') {
        skip(lex); // skip '#' token
        skip(lex); // skip '!' token
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
    ast->items = AstDeclList_new(lex->ast);
    toplevel_items(lex, ast->items);
    check(lex, TK_END);
    return ast;
}

static void init_lexer(struct Compiler *C, struct Ast *ast, struct Lex *lex)
{
    *lex = (struct Lex){
        .pool = pawP_pool_new(C, C->aux_stats),
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

static void ast_callback(struct Compiler *C, struct Ast *ast)
{
    if (pawP_push_callback(C, "paw.on_build_ast")) {
        paw_Env *P = ENV(C);
        paw_push_rawptr(P, ast);
        paw_call(P, 1);
    }
}

struct Ast *pawP_parse_module(struct Compiler *C, String *modname, paw_Reader input, void *ud)
{
    struct Ast *ast = new_ast(C, modname);

    struct Lex lex;
    init_lexer(C, ast, &lex);

    parse_module(&lex, input, ud);
    pawP_pool_free(C, lex.pool);
    ast_callback(C, ast);
    return ast;
}

struct Ast *pawP_parse_prelude(struct Compiler *C)
{
    struct Lex lex;
    init_lexer(C, C->ast_prelude, &lex);

    parse_prelude(&lex);
    pawP_pool_free(C, lex.pool);
    ast_callback(C, lex.ast);
    return lex.ast;
}
