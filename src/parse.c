// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "parse.h"
#include "ast.h"
#include "compile.h"
#include "env.h"
#include "error.h"
#include "map.h"

// TODO: consider moving this functionality to "lib.c" or something, really doesn't belong here...
#include "os.h"
#include "lib.h"

#define PARSE_ERROR(X_, Kind_, ...) pawErr_##Kind_((X_)->C, (X_)->modname, __VA_ARGS__)
#define LIMIT_ERROR(x, start, what, limit) PARSE_ERROR(x, too_many_elements, start, what, limit)

#define NEW_NODE(Lex_, Kind_, Start_, ...) \
    pawAst_new_##Kind_((Lex_)->ast, span_from(Lex_, Start_), __VA_ARGS__)

static struct SourceSpan span_from(struct Lex *lex, struct SourceLoc start)
{
    return (struct SourceSpan){
        .start = start,
        .end = lex->loc,
    };
}

static NodeId next_id(struct Lex *lex)
{
    return (NodeId){++lex->ast->node_count};
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

static paw_Bool equals_cstr(struct Lex *lex, Str const *ident, unsigned cstr)
{
    return pawS_eq(ident, CACHED_STRING(ENV(lex), cstr));
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
    INFIX_RANGEI, // ..=
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
    [INFIX_RANGEI] = {2, 2},
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
        case TK_DOT2_EQ:
            return INFIX_RANGEI;
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
    return NEW_NODE(lex, string_expr, start, next_id(lex), parts);
}

// Parse an interpolated string
//
// For example, the string "abc\{123}" results in the following tokens:
//
//      index | kind              | payload
//     -------|-------------------|---------
//      1     | STRING_EXPR_OPEN  | "abc"
//      2     | INT               | 123
//      3     | STRING_EXPR_CLOSE | -
//      4     | STRING_TEXT       |  ""
//
static struct AstExpr *string_interp_expr(struct Lex *lex, struct SourceLoc start)
{
    struct AstStringList *parts = AstStringList_new(lex->ast);
    do {
        if (test(lex, TK_END)) break;
        Value const str = lex->t.value;
        skip(lex);

        add_string_part(lex, parts, start, str);
        add_expr_part(lex, parts, expr0(lex));
        check_next(lex, TK_STRING_EXPR_CLOSE);
    } while (test(lex, TK_STRING_EXPR_OPEN));
    check(lex, TK_STRING_TEXT);
    add_string_part(lex, parts, start, lex->t.value);
    skip(lex); // skip string text
    return NEW_NODE(lex, string_expr, start, next_id(lex), parts);
}

static struct AstIdent parse_ident(struct Lex *lex)
{
    check(lex, TK_NAME);
    struct SourceLoc const start = lex->loc;
    Str *name = V_STR(lex->t.value);
    skip(lex);

    return (struct AstIdent){
        .span = span_from(lex, start),
        .name = name,
    };
}

static struct AstIdent parse_ident_or_underscore(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    if (test_next(lex, TK_UNDERSCORE)) {
        return (struct AstIdent){
            .span = span_from(lex, start),
            .name = CSTR(lex, CSTR_UNDERSCORE),
        };
    } else {
        return parse_ident(lex);
    }
}

static void ensure_not_underscore(struct Lex *lex, struct AstIdent ident)
{
    if (equals_cstr(lex, ident.name, CSTR_UNDERSCORE))
        PARSE_ERROR(lex, unexpected_underscore, ident.span.start);
}

static struct AstExpr *new_basic_lit(struct Lex *lex, struct SourceLoc start, Value value, paw_Type code)
{
    return NEW_NODE(lex, basic_lit, start, next_id(lex), value, code);
}

static struct AstExpr *unit_lit(struct Lex *lex)
{
    return new_basic_lit(lex, lex->loc, I2V(0), BUILTIN_UNIT);
}

static struct AstType *unit_type(struct Lex *lex)
{
    struct AstTypeList *types = AstTypeList_new(lex->ast);
    return NEW_NODE(lex, tuple_type, lex->loc, next_id(lex), types);
}

static struct AstExpr *emit_bool(struct Lex *lex, struct SourceLoc loc, paw_Bool b)
{
    Value v;
    V_SET_BOOL(&v, b);
    return new_basic_lit(lex, loc, v, BUILTIN_BOOL);
}

static struct AstType *parse_type(struct Lex *lex, paw_Bool is_strict);
static struct AstType *parse_strict_type(struct Lex *lex);
static struct AstType *parse_relaxed_type(struct Lex *lex);

static struct AstDecl *variant_field_decl(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstIdent const empty = {0};
    struct AstType *tag = parse_type(lex, PAW_TRUE);
    return NEW_NODE(lex, field_decl, start, next_id(lex), empty, tag, PAW_FALSE);
}

#define DEFINE_LIST_PARSER(Name_, A_, B_, Limit_, What_, Fn_, List_)                            \
    static void parse_##Name_##_list(struct Lex *lex, struct List_ *list, struct SourceLoc start) \
    {                                                                                             \
        do {                                                                                      \
            if (test(lex, B_)) break;                                                             \
            if (list->count == (Limit_))                                                          \
                LIMIT_ERROR(lex, start, What_, (Limit_));                                         \
            List_##_push((lex)->ast, list, (Fn_)(lex));                                         \
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
    struct SourceLoc const start = lex->loc;

    do {
    next_segment:
        if (s->count == INT_MAX)
            break; // throw error below

        struct AstIdent const ident = parse_ident(lex);
        struct AstTypeList *args = NULL;
        // permit "::<types..>" between segments
        if (test_next(lex, TK_COLON2)) {
            args = maybe_relaxed_type_args(lex);
            if (args == NULL) {
                pawAst_add_segment(lex->ast, s, next_id(lex), ident, NULL);
                goto next_segment;
            }
        }
        pawAst_add_segment(lex->ast, s, next_id(lex), ident, args);
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
    struct SourceLoc const start = lex->loc;

    do {
        if (s->count == INT_MAX)
            break; // throw error below

        struct AstIdent const ident = parse_ident(lex);
        struct SourceLoc const start = lex->loc;
        struct AstTypeList *args = NULL;
        if (test_next(lex, '<')) {
            // _<types..> is not allowed
            ensure_not_underscore(lex, ident);
            args = is_strict
                       ? strict_type_list(lex, start)
                       : relaxed_type_list(lex, start);
        }
        pawAst_add_segment(lex->ast, s, next_id(lex), ident, args);
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
    struct SourceLoc const start = lex->loc;
    struct AstPath path = parse_pathexpr(lex);
    return NEW_NODE(lex, path_expr, start, next_id(lex), path);
}

static struct AstPat *struct_field_pat(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident(lex);
    struct AstPat *pat;
    if (test_next(lex, ':')) {
        pat = pattern(lex);
    } else {
        // binds field to variable of same name
        pat = NEW_NODE(lex, ident_pat, ident.span.start, next_id(lex), ident);
    }
    return NEW_NODE(lex, field_pat, start, next_id(lex), ident, pat);
}

DEFINE_LIST_PARSER(variant_field_pat, '(', ')', LOCAL_MAX, "variant fields", pattern, AstPatList)
DEFINE_LIST_PARSER(struct_field_pat, '{', '}', LOCAL_MAX, "struct fields", struct_field_pat, AstPatList)

static paw_Bool is_wildcard_path(struct AstPath path)
{
    struct AstSegments *segments = path.segments;
    paw_assert(segments->count > 0);
    if (segments->count > 1) return PAW_FALSE;
    struct AstSegment const segment = K_LIST_FIRST(segments);
    Str const *name = segment.ident.name;
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

static struct AstPat *compound_pat(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstPath const path = parse_pathexpr(lex);
    if (test_next(lex, '(')) {
        struct AstPatList *fields = AstPatList_new(lex->ast);
        parse_variant_field_pat_list(lex, fields, start);
        return NEW_NODE(lex, variant_pat, start, next_id(lex), path, fields);
    } else if (test_next(lex, '{')) {
        struct AstPatList *fields = AstPatList_new(lex->ast);
        parse_struct_field_pat_list(lex, fields, start);
        return NEW_NODE(lex, struct_pat, start, next_id(lex), path, fields);
    }
    if (path.segments->count == 1) {
        struct AstSegment const segment = K_LIST_FIRST(path.segments);
        if (get_builtin_kind(lex, segment.ident) != NBUILTINS)
            PARSE_ERROR(lex, reserved_identifier, lex->loc, segment.ident.name->text);
        if (segment.types == NULL)
            return NEW_NODE(lex, ident_pat, start, next_id(lex), segment.ident);
    }
    return NEW_NODE(lex, path_pat, start, next_id(lex), path);
}

static struct AstPat *wildcard_pat(struct Lex *lex)
{
    struct SourceLoc loc = lex->loc;
    skip(lex); // '_' token
    return NEW_NODE(lex, wildcard_pat, loc, next_id(lex));
}

static struct AstPat *tuple_pat(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // '(' token
    struct AstPatList *elems = AstPatList_new(lex->ast);
    parse_variant_field_pat_list(lex, elems, start);
    return NEW_NODE(lex, tuple_pat, start, next_id(lex), elems);
}

// TODO: handle range patterns
static struct AstExpr *literal_expr(struct Lex *lex)
{
    struct SourceLoc const loc = lex->loc;
    paw_Bool const negative = test_next(lex, '-');

    enum BuiltinKind code;
    switch (lex->t.kind) {
        case TK_TRUE:
            V_SET_BOOL(&lex->t.value, PAW_TRUE);
            code = BUILTIN_BOOL;
            break;
        case TK_FALSE:
            V_SET_BOOL(&lex->t.value, PAW_FALSE);
            code = BUILTIN_BOOL;
            break;
        case TK_CHAR:
            code = BUILTIN_CHAR;
            break;
        case TK_INT:
            code = BUILTIN_INT;
            break;
        case TK_FLOAT:
            code = BUILTIN_FLOAT;
            break;
        case TK_STRING_TEXT:
            code = BUILTIN_STR;
            break;
        default:
            PARSE_ERROR(lex, nonliteral_pattern, lex->loc);
    }
    Value const value = lex->t.value;
    struct AstExpr *expr = new_basic_lit(lex, loc, value, code);
    struct AstLiteralExpr *lit = AstGetLiteralExpr(expr);
    skip(lex); // literal token

    if (negative) {
        if (code == BUILTIN_FLOAT) {
            V_SET_FLOAT(&lit->basic.value, -V_FLOAT(value));
        } else if (code != BUILTIN_INT) {
            PARSE_ERROR(lex, invalid_literal_negation, lex->loc);
        } else if (V_UINT(value) > (paw_Uint)PAW_INT_MAX + 1) {
            PARSE_ERROR(lex, integer_out_of_range, lex->loc, value.u);
        } else if (V_UINT(value) == (paw_Uint)PAW_INT_MAX + 1) {
            V_SET_INT(&lit->basic.value, PAW_INT_MIN);
        } else {
            V_SET_INT(&lit->basic.value, -(paw_Int)V_UINT(value));
        }
    } else if (code == BUILTIN_INT && value.u > (paw_Uint)PAW_INT_MAX) {
        PARSE_ERROR(lex, integer_out_of_range, lex->loc, value.u);
    }
    return expr;
}

static struct AstPat *literal_pat(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstExpr *expr = literal_expr(lex);
    return NEW_NODE(lex, literal_pat, start, next_id(lex), expr);
}

static struct AstPat *alternative_pat(struct Lex *lex)
{
    switch (lex->t.kind) {
        case TK_NAME:
            return compound_pat(lex);
        case TK_UNDERSCORE:
            return wildcard_pat(lex);
        case '(':
            return tuple_pat(lex);
        default:
            return literal_pat(lex);
    }
}

static struct AstPat *pattern(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstPat *pat = alternative_pat(lex);
    if (!test_next(lex, '|')) return pat;

    AstPatList *pats = AstPatList_new(lex->ast);
    AstPatList_push(lex->ast, pats, pat);
    do {
        pat = alternative_pat(lex);
        AstPatList_push(lex->ast, pats, pat);
    } while (test_next(lex, '|'));

    return NEW_NODE(lex, or_pat, start, next_id(lex), pats);
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

    return NEW_NODE(lex, tuple_type, start, next_id(lex), elems);
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
    return NEW_NODE(lex, container_type, start, next_id(lex), first, second);
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
    struct SourceLoc const start = lex->loc;
    if (test_next(lex, TK_UNDERSCORE)) {
         if (is_strict) PARSE_ERROR(lex, unexpected_underscore, start);
         return NEW_NODE(lex, infer_type, start, next_id(lex));
    }
    struct AstPath path = parse_pathtype(lex, is_strict);
    return NEW_NODE(lex, path_type, start, next_id(lex), path);
}

static struct AstType *parse_return_type(struct Lex *lex, paw_Bool is_strict)
{
    if (test_next(lex, '!')) {
        // type "!" can only appear as a function return type
        return NEW_NODE(lex, never_type, lex->loc, next_id(lex));
    } else {
        return parse_type(lex, is_strict);
    }
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

#define SELF_TYPENAME(Lex_) SCAN_STR((Lex_)->C, "Self")

static struct AstType *self_type(struct Lex *lex, struct SourceSpan span)
{
    struct AstPath path;
    struct AstIdent const ident = {
        .name = SELF_TYPENAME(lex),
        .span = span,
    };
    pawAst_path_init(lex->ast, &path, span);
    pawAst_add_segment(lex->ast, path.segments, next_id(lex), ident, NULL);
    return NEW_NODE(lex, path_type, span.start, next_id(lex), path);
}

static struct AstDecl *fn_param_decl(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident_or_underscore(lex);
    struct AstType *tag = expect_type_annotation(lex, "parameter", ident, PAW_TRUE);
    return NEW_NODE(lex, param_decl, start, next_id(lex), ident, tag);
}

static struct AstDecl *closure_param_decl(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident_or_underscore(lex);
    struct AstType *tag = type_annotation(lex, PAW_FALSE);
    return NEW_NODE(lex, param_decl, start, next_id(lex), ident, tag);
}

static struct AstBoundList *parse_generic_bounds(struct Lex *lex)
{
    if (!test_next(lex, ':')) return NULL;
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
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident(lex);
    struct AstBoundList *bounds = parse_generic_bounds(lex);
    return NEW_NODE(lex, generic_decl, start, next_id(lex), ident, bounds);
}

DEFINE_LIST_PARSER(sig_param, '(', ')', LOCAL_MAX, "function parameters", parse_strict_type, AstTypeList)
DEFINE_LIST_PARSER(closure_param, '|', '|', LOCAL_MAX, "closure parameters", closure_param_decl, AstDeclList)
DEFINE_LIST_PARSER(generic, '<', '>', LOCAL_MAX, "generics", generic_param, AstDeclList)

static struct AstExpr *sitem_expr(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident(lex);
    struct AstExpr *value;
    if (test_next(lex, ':')) {
        value = expr0(lex);
    } else {
        // "name" by itself is shorthand for "name: name"
        struct AstPath path;
        pawAst_path_init(lex->ast, &path, ident.span);
        pawAst_add_segment(lex->ast, path.segments, next_id(lex), ident, NULL);
        value = NEW_NODE(lex, path_expr, start, next_id(lex), path);
    }
    int const fid = INT_MAX; // nonnegative means determine later
    return NEW_NODE(lex, named_field_expr, start, next_id(lex), ident, value, fid);
}

DEFINE_LIST_PARSER(sitem, '{', '}', LOCAL_MAX, "struct items", sitem_expr, AstExprList)

static struct AstExpr *unop_expr(struct Lex *lex, enum UnOp op)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // unary operator token
    enum UnaryOp const unop = CAST(enum UnaryOp, op); // same order
    struct AstExpr *target = expression(lex, kUnOpPrecedence);
    return NEW_NODE(lex, unop_expr, start, next_id(lex), unop, target);
}

// Parse either a parenthsized expression or a tuple
static struct AstExpr *paren_expr(struct Lex *lex)
{
    // Just parse and return the expression contained within the parenthesis.
    // There is no need for an extra node type.
    struct SourceLoc const start = lex->loc;
    skip(lex); // '(' token
    if (test_next(lex, ')'))
        return unit_lit(lex);

    ++lex->expr_depth;
    struct AstExpr *expr = expr0(lex);
    --lex->expr_depth;
    if (test_next(lex, ')'))
        return NEW_NODE(lex, paren_expr, start, next_id(lex), expr);

    check_next(lex, ',');
    struct AstExprList *elems = AstExprList_new(lex->ast);
    AstExprList_push(lex->ast, elems, expr);
    parse_arg_list(lex, elems, start);
    return NEW_NODE(lex, tuple_lit, start, next_id(lex), elems);
}

static struct AstType *parse_signature(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    check_next(lex, '(');
    struct AstTypeList *params = AstTypeList_new(lex->ast);
    if (!test_next(lex, ')'))
        parse_sig_param_list(lex, params, start);

    struct AstType *result = test_next(lex, TK_ARROW)
        ? parse_return_type(lex, PAW_TRUE)
        : unit_type(lex);
    return NEW_NODE(lex, fn_type, start, next_id(lex), params, result);
}

static paw_Bool end_of_block(struct Lex *lex)
{
    return test(lex, '}') || // found proper end
           test(lex, TK_END); // truncated block
}

static struct AstExpr *index_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // '[' token
    struct AstExpr *index = expr0(lex);
    delim_next(lex, ']', '[', start);
    return NEW_NODE(lex, index, start, next_id(lex), target, index);
}

static paw_Type parse_container_items(struct Lex *lex, struct AstExprList *items)
{
    struct SourceLoc const start = lex->loc;
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
            item = NEW_NODE(lex, keyed_field_expr, start, next_id(lex), item, value);
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
    struct SourceLoc const start = lex->loc;
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
    return NEW_NODE(lex, container_lit, start, next_id(lex), items, b_kind);
}

// Parse a composite literal expression
static struct AstExpr *composite_lit(struct Lex *lex, struct AstPathExpr *path)
{
    skip(lex); // '{' token
    struct AstExprList *items = AstExprList_new(lex->ast);
    parse_sitem_list(lex, items, path->span.start);
    return NEW_NODE(lex, composite_lit, path->span.start, next_id(lex), path->path, items);
}

static struct AstExpr *try_composite_lit(struct Lex *lex, struct AstExpr *expr)
{
    if (AstIsPathExpr(expr) && lex->expr_depth >= 0)
        expr = composite_lit(lex, AstGetPathExpr(expr));

    return expr;
}

static struct AstExpr *selector_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // '.' token
    if (test(lex, TK_NAME)) {
        struct AstIdent const ident = parse_ident(lex);
        return NEW_NODE(lex, name_selector, start, next_id(lex), target, ident);
    } else if (test(lex, TK_INT)) {
        int const index = V_INT(lex->t.value);
        skip(lex); // integer token
        return NEW_NODE(lex, index_selector, start, next_id(lex), target, index);
    }

    PARSE_ERROR(lex, invalid_selector, start); // no return
}

static struct AstExpr *call_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // '(' token
    struct AstExprList *args = AstExprList_new(lex->ast);
    parse_arg_list(lex, args, start);
    return NEW_NODE(lex, call_expr, start, next_id(lex), target, args);
}

static struct AstExpr *chain_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // '?' token

    if (lex->fn_depth == 0)
        PARSE_ERROR(lex, chain_outside_function, start);

    return NEW_NODE(lex, chain_expr, start, next_id(lex), target);
}

#define IS_SELF_TYPE(Lex_, Type_) (AstIsPathType(Type_) && AstGetPathType(Type_)->path.segments->count == 1 && \
        pawS_eq(K_LIST_FIRST(AstGetPathType(Type_)->path.segments).ident.name, SELF_TYPENAME(Lex_)))
#define IS_SELF_VAR(Lex_, Ident_) pawS_eq((Ident_).name, CACHED_STRING(ENV(Lex_), CSTR_SELF))

static struct AstDeclList *fn_parameters(struct Lex *lex, paw_Bool *is_method)
{
    *is_method = PAW_FALSE;
    struct SourceLoc const start = lex->loc;
    check_next(lex, '(');

    struct AstDeclList *params = AstDeclList_new(lex->ast);
    if (lex->in_impl && !test(lex, ')')) {
        // check for receiver parameter
        struct AstIdent const ident = parse_ident_or_underscore(lex);
        struct AstType *tag = type_annotation(lex, PAW_TRUE);
        if (tag == NULL && IS_SELF_VAR(lex, ident)) {
            // "self" means "self: Self"
            tag = self_type(lex, ident.span);
            *is_method = PAW_TRUE;
        } else if (tag == NULL) {
            PARSE_ERROR(lex, expected_type_annotation, ident.span.start,
                    "function parameter", ident.name->text);
        } else if (IS_SELF_VAR(lex, ident)) {
            // "self" must have a type compatible with "Self"
            *is_method = PAW_TRUE;
        }
        struct AstDecl *first = NEW_NODE(lex, param_decl, start, next_id(lex), ident, tag);
        AstDeclList_push(lex->ast, params, first);
        test_next(lex, ',');
    }

    do {
        if (test(lex, ')')) break;
        if (params->count == LOCAL_MAX)
            LIMIT_ERROR(lex, start, "function parameters", LOCAL_MAX);
        AstDeclList_push(lex->ast, params, fn_param_decl(lex));
    } while (test_next(lex, ','));
    delim_next(lex, ')', '(', start);
    return params;
}

static struct AstDeclList *closure_params(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
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
        case kAstLoopExpr:
        case kAstWhileExpr:
        case kAstMatchExpr:
        case kAstBlock:
            return PAW_FALSE;
    }
}

static struct AstExpr *block_expr(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
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
    delim_next(lex, '}', '{', start);
    return NEW_NODE(lex, block, start, next_id(lex), stmts, result);
}

static struct AstExpr *closure(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstDeclList *params = closure_params(lex);
    struct AstType *result = NULL;
    struct AstExpr *expr;
    if (test_next(lex, TK_ARROW)) {
        result = parse_return_type(lex, PAW_FALSE);
        expr = block_expr(lex);
    } else {
        expr = expr0(lex);
    }
    return NEW_NODE(lex, closure_expr, start, next_id(lex), params, result, expr);
}

static struct AstExpr *if_expr(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
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
    return NEW_NODE(lex, if_expr, start, next_id(lex), cond, then_arm, else_arm);
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
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'for' token
    struct AstPat *pat = pattern(lex);
    check_next(lex, TK_IN);
    struct AstExpr *target = basic_expr(lex);
    struct AstExpr *block = loop_block(lex);
    return NEW_NODE(lex, for_expr, start, next_id(lex), pat, target, block);
}

static struct AstExpr *loop_expr(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'loop' token
    struct AstExpr *block = loop_block(lex);
    return NEW_NODE(lex, loop_expr, start, next_id(lex), block);
}

static struct AstExpr *while_expr(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'while' token
    struct AstExpr *cond = basic_expr(lex);
    struct AstExpr *block = loop_block(lex);
    return NEW_NODE(lex, while_expr, start, next_id(lex), cond, block);
}

static struct AstExpr *return_expr(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'return' token

    if (lex->fn_depth == 0)
        PARSE_ERROR(lex, return_outside_function, start);

    struct AstExpr *expr = NULL;
    if (!test(lex, '}')
            && !test(lex, ']')
            && !test(lex, ')')
            && !test(lex, ';')
            && !test(lex, ',')) {
        expr = expr0(lex);
    }
    return NEW_NODE(lex, return_expr, start, next_id(lex), expr);
}

static struct AstExpr *jump_expr(struct Lex *lex, enum JumpKind kind)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'break' or 'continue' token

    if (lex->loop_depth == 0)
        PARSE_ERROR(lex, jump_outside_loop, start,
                kind == JUMP_BREAK ? "break" : "continue");

    return NEW_NODE(lex, jump_expr, start, next_id(lex), kind);
}

static struct AstExpr *match_arm(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstPat *pat = pattern(lex);
    struct AstExpr *guard = NULL;
    if (test_next(lex, TK_IF))
        guard = basic_expr(lex);

    check_next(lex, TK_FAT_ARROW);
    struct AstExpr *result = expr0(lex);
    return NEW_NODE(lex, match_arm, start, next_id(lex), pat, guard, result);
}

static struct AstExpr *match_expr(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
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
    return NEW_NODE(lex, match_expr, start, next_id(lex), target, arms);
}

// TODO: accept source loc as argument
static struct AstExpr *primary_expr(struct Lex *lex)
{
    struct SourceLoc const loc = lex->loc;
    struct Token const t = lex->t;

    switch (t.kind) {
        case '(':
            return paren_expr(lex);
        case '[':
            return container_lit(lex);
        case TK_NAME:
            return path_expr(lex);
        case '{':
            return block_expr(lex);
        case TK_TRUE:
            skip(lex);
            return emit_bool(lex, loc, PAW_TRUE);
        case TK_FALSE:
            skip(lex);
            return emit_bool(lex, loc, PAW_FALSE);
        case TK_CHAR:
            skip(lex);
            return new_basic_lit(lex, loc, t.value, BUILTIN_CHAR);
        case TK_INT:
            skip(lex);
            return new_basic_lit(lex, loc, t.value, BUILTIN_INT);
        case TK_FLOAT:
            skip(lex);
            return new_basic_lit(lex, loc, t.value, BUILTIN_FLOAT);
        case TK_STRING_TEXT:
            skip(lex);
            return string_expr(lex, loc, t.value);
        case TK_STRING_EXPR_OPEN:
            return string_interp_expr(lex, loc);
        case TK_IF:
            return if_expr(lex);
        case TK_LOOP:
            return loop_expr(lex);
        case TK_FOR:
            return for_expr(lex);
        case TK_WHILE:
            return while_expr(lex);
        case TK_RETURN:
            return return_expr(lex);
        case TK_BREAK:
            return jump_expr(lex, JUMP_BREAK);
        case TK_CONTINUE:
            return jump_expr(lex, JUMP_CONTINUE);
        case TK_MATCH:
            return match_expr(lex);
        default:
            return NULL;
    }
}

static struct AstExpr *suffixed_expr(struct Lex *lex)
{
    struct AstExpr *e = primary_expr(lex);
    if (e == NULL)
        return NULL;

    if (AstIsBlock(e)
            || AstIsIfExpr(e)
            || AstIsMatchExpr(e)
            || AstIsLoopExpr(e)
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
    switch (lex->t.kind) {
        case TK_PIPE2:
            lex->t.kind = '|';
            lex->t2.kind = '|';
            // (fallthrough)
        case '|':
            return closure(lex);
        case TK_DOT2:
            return NULL;
        default:
            return suffixed_expr(lex);
    }
}

static struct AstExpr *conversion_expr(struct Lex *lex, struct AstExpr *lhs)
{
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident(lex);

    enum BuiltinKind to;
    if (equals_cstr(lex, ident.name, CSTR_BOOL)) {
        to = BUILTIN_BOOL;
    } else if (equals_cstr(lex, ident.name, CSTR_CHAR)) {
        to = BUILTIN_CHAR;
    } else if (equals_cstr(lex, ident.name, CSTR_INT)) {
        to = BUILTIN_INT;
    } else if (equals_cstr(lex, ident.name, CSTR_FLOAT)) {
        to = BUILTIN_FLOAT;
    } else {
        PARSE_ERROR(lex, expected_basic_type, start, ident.name->text);
    }
    return NEW_NODE(lex, conversion_expr, start, next_id(lex), lhs, to);
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
    struct SourceLoc const start = lex->loc;
    struct AstExpr *rhs = expression(lex, right_prec(op));
    enum BinaryOp const binop = into_binary_op(op);
    return NEW_NODE(lex, op_assign_expr, start, next_id(lex), lhs, rhs, binop);
}

static struct AstExpr *assignment_expr(struct Lex *lex, struct AstExpr *lhs)
{
    struct SourceLoc const start = lex->loc;
    struct AstExpr *rhs = expression(lex, right_prec(INFIX_ASSIGN));
    return NEW_NODE(lex, assign_expr, start, next_id(lex), lhs, rhs);
}

static paw_Bool test_operand(struct Lex *lex)
{
    switch (lex->t.kind) {
        case '(': // parenthesized expression
        case '[': // container literal
        case TK_INT:
        case TK_CHAR:
        case TK_FLOAT:
        case TK_STRING_TEXT:
        case TK_NAME:
        case '#':
        case '-':
        case '!':
        case '~':
            return PAW_TRUE;
        default:
            return PAW_FALSE;
    }
}

static struct AstExpr *range_expr(struct Lex *lex, enum InfixOp op, struct AstExpr *lhs)
{
    struct AstExpr *rhs = NULL;
    struct SourceLoc const start = lex->loc;
    if (test_operand(lex)) rhs = expression(lex, right_prec(op));
    return NEW_NODE(lex, range_expr, start, next_id(lex), op == INFIX_RANGEI, lhs, rhs);
}

static struct AstExpr *binop_expr(struct Lex *lex, enum InfixOp op, struct AstExpr *lhs)
{
    struct SourceLoc const start = lex->loc;
    struct AstExpr *rhs = expression(lex, right_prec(op));
    enum BinaryOp const binop = CAST(enum BinaryOp, op); // same order
    return NEW_NODE(lex, binop_expr, start, next_id(lex), binop, lhs, rhs);
}

static struct AstExpr *logical_expr(struct Lex *lex, struct AstExpr *lhs, paw_Bool is_and)
{
    struct SourceLoc const start = lex->loc;
    int const prec = right_prec(is_and ? INFIX_AND : INFIX_OR);
    struct AstExpr *rhs = expression(lex, prec);
    return NEW_NODE(lex, logical_expr, start, next_id(lex), lhs, rhs, is_and);
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
        case INFIX_RANGE:
        case INFIX_RANGEI:
            return range_expr(lex, op, lhs);
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
    op = get_infixop(lex->t.kind);

    if (expr == NULL
            && op != INFIX_RANGE
            && op != INFIX_RANGEI)
        return NULL;

    while (op != NOT_INFIX && prec < left_prec(op)) {
        expr = infix_expr(lex, expr, op);
        op = get_infixop(lex->t.kind);
    }
    return expr;
}

static struct AstExpr *expression(struct Lex *lex, unsigned prec)
{
    enter_expression(lex);
    struct SourceLoc const start = lex->loc;
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
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'let' token
    struct AstPat *pat = pattern(lex);
    struct AstType *tag = type_annotation(lex, PAW_FALSE);
    struct AstExpr *init = test_next(lex, '=') ? expr0(lex) : NULL;
    semicolon(lex, "'let' declaration");
    return NEW_NODE(lex, let_stmt, start, next_id(lex), pat, tag, init);
}

static struct AstStmt *expr_stmt(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstExpr *expr = expr0(lex);
    return NEW_NODE(lex, expr_stmt, start, next_id(lex), expr);
}

static struct AstDeclList *type_param(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
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
    ++lex->fn_depth;
    struct AstExpr *body = block_expr(lex);
    --lex->fn_depth;
    return body;
}

static struct AstDecl *function(struct Lex *lex, struct SourceLoc start, struct AstIdent ident, struct Annotations *annos, enum FnKind kind, paw_Bool is_pub)
{
    paw_Bool is_method;
    struct AstDeclList *generics = type_param(lex);
    struct AstDeclList *params = fn_parameters(lex, &is_method);
    struct AstType *result = test_next(lex, TK_ARROW)
        ? parse_return_type(lex, PAW_TRUE)
        : unit_type(lex); // default to "()"
    struct AstExpr *body = !test_next(lex, ';')
        ? function_body(lex)
        : NULL;

    return NEW_NODE(lex, fn_decl, start, next_id(lex), kind, ident, annos,
            generics, params, result, body, is_pub, is_method);
}

static struct AstDecl *use_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'use' token

    enum AstUseKind kind = AST_USE_NORMAL;
    struct AstSegments *s = AstSegments_new(lex->ast);
    do {
        if (s->count == INT_MAX)
            break; // throw error below
        if (test_next(lex, '*')) {
            kind = AST_USE_GLOB;
            break;
        }

        struct AstIdent const ident = parse_ident(lex);
        pawAst_add_segment(lex->ast, s, next_id(lex), ident, NULL);
    } while (test_next(lex, TK_COLON2));

    if (s->count > LOCAL_MAX)
        LIMIT_ERROR(lex, start, "path segments", LOCAL_MAX);

    if (s->count == 0 || test(lex, TK_COLON2)) {
        paw_assert(kind == AST_USE_GLOB);
        PARSE_ERROR(lex, invalid_glob, start);
    }

    struct AstPath path = {
        .span = span_from(lex, start),
        .segments = s,
    };

    struct AstIdent as = {0};
    if (test_next(lex, TK_AS)) {
        // UseDecl containing a glob cannot also be an alias
        if (kind == AST_USE_GLOB) PARSE_ERROR(lex, invalid_glob, start);
        as = parse_ident(lex);
        kind = AST_USE_ALIAS;
    }

    semicolon(lex, "'use' declaration");
    return NEW_NODE(lex, use_decl, start, next_id(lex), path, as, kind, is_pub);
}

static void parse_trait_list(struct Lex *lex, struct AstTypeList *traits)
{
    do {
        if (test(lex, '{')) break;
        struct SourceLoc const start = lex->loc;
        struct AstPath path = parse_pathtype(lex, PAW_TRUE);
        struct AstType *trait = NEW_NODE(lex, path_type, start, next_id(lex), path);
        AstTypeList_push(lex->ast, traits, trait);
    } while (test_next(lex, '+'));
}

static struct Annotations *annotations(struct Lex *lex)
{
    struct Compiler *C = lex->C;
    struct SourceLoc const start = lex->loc;
    if (!test_next(lex, TK_HASH_BRACKET))
        return NULL;

    StringMap *names = StringMap_new_from(C, lex->pool);
    struct Annotations *annos = Annotations_new(C);
    do {
        if (test(lex, ']'))
            break;
        struct AstIdent const ident = parse_ident(lex);
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

static struct AstDecl *fn_decl(struct Lex *lex, struct Annotations *annos, paw_Bool is_pub)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'fn' token
    struct AstIdent const ident = parse_ident(lex);
    return function(lex, start, ident, annos, FUNC_FUNCTION, is_pub);
}

static struct AstDecl *parse_method(struct Lex *lex, struct Annotations *annos, paw_Bool is_pub)
{
    struct SourceLoc const start = lex->loc;
    check_next(lex, TK_FN);
    // indicate that 'self' has special meaning
    lex->in_impl = PAW_TRUE;
    struct AstIdent const ident = parse_ident(lex);
    struct AstDecl *method = function(lex, start, ident, annos, FUNC_METHOD, is_pub);
    lex->in_impl = PAW_FALSE;
    return method;
}

static struct AstDecl *variant_decl(struct Lex *lex, int index)
{
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident(lex);
    struct AstDeclList *fields = test_next(lex, '(')
        ? variant_field_list(lex, start)
        : AstDeclList_new(lex->ast);
    return NEW_NODE(lex, variant_decl, start, next_id(lex), ident, fields, index);
}

static void enum_body(struct Lex *lex, struct SourceLoc start, struct AstTypeList *traits, struct AstDeclList *variants, struct AstDeclList *methods)
{
    if (test_next(lex, ':'))
        parse_trait_list(lex, traits);

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
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident(lex);
    struct AstDeclList *generics = type_param(lex);
    struct AstTypeList *traits = AstTypeList_new(lex->ast);
    struct AstDeclList *variants = AstDeclList_new(lex->ast);
    struct AstDeclList *methods = AstDeclList_new(lex->ast);
    enum_body(lex, start, traits, variants, methods);
    return NEW_NODE(lex, adt_decl, start, next_id(lex), ident, traits, generics,
            variants, methods, is_pub, PAW_FALSE, is_inline);
}

static struct AstDecl *struct_field(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc const start = lex->loc;
    struct AstIdent const ident = parse_ident(lex);
    struct AstType *tag = expect_type_annotation(lex, "field", ident, PAW_TRUE);
    return NEW_NODE(lex, field_decl, start, next_id(lex), ident, tag, is_pub);
}

static void struct_body(struct Lex *lex, struct AstTypeList *traits, struct AstDeclList *fields, struct AstDeclList *methods)
{
    struct SourceLoc const start = lex->loc;
    if (test_next(lex, ':'))
        parse_trait_list(lex, traits);

    if (!test_next(lex, '{')) {
        semicolon(lex, "body of unit struct");
        return;
    }

    while (!end_of_block(lex)) {
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
    struct AstIdent const ident = parse_ident(lex);
    struct AstDeclList *generics = type_param(lex);
    struct AstTypeList *traits = AstTypeList_new(lex->ast);
    struct AstDeclList *fields = AstDeclList_new(lex->ast);
    struct AstDeclList *methods = AstDeclList_new(lex->ast);
    struct_body(lex, traits, fields, methods);

    struct AstDeclList *variants = AstDeclList_new(lex->ast);
    struct AstDecl *v = NEW_NODE(lex, variant_decl, start, next_id(lex), ident, fields, 0);
    AstDeclList_push(lex->ast, variants, v);

    return NEW_NODE(lex, adt_decl, start, next_id(lex), ident, traits, generics,
            variants, methods, is_pub, PAW_TRUE, is_inline);
}

static struct AstDecl *trait_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'trait' token
    struct AstIdent const ident = parse_ident(lex);
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

    return NEW_NODE(lex, trait_decl, start, next_id(lex), ident, generics, methods, is_pub);
}

static struct AstDecl *type_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc const start = lex->loc;
    skip(lex); // 'type' token

    struct AstIdent const ident = parse_ident(lex);
    struct AstDeclList *generics = type_param(lex);

    check_next(lex, '=');

    struct AstType *rhs = parse_type(lex, PAW_TRUE);
    if (AstIsFnType(rhs))
        PARSE_ERROR(lex, function_type_decl, start);

    semicolon(lex, "type declaration");
    return NEW_NODE(lex, type_decl, start, next_id(lex), ident, generics, rhs, is_pub);
}

static struct AstDecl *local_decl(struct Lex *lex)
{
    struct AstDecl *decl;
    switch (lex->t.kind) {
        case TK_TYPE:
            decl = type_decl(lex, PAW_FALSE);
            break;
        case TK_USE:
            decl = use_decl(lex, PAW_FALSE);
            break;
        default:
            PARSE_ERROR(lex, unexpected_symbol, lex->loc);
    }

    return decl;
}

static struct AstStmt *decl_stmt(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstDecl *decl = local_decl(lex);
    return NEW_NODE(lex, decl_stmt, start, next_id(lex), decl);
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
        case TK_USE:
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
    struct AstIdent const ident = parse_ident(lex);
    struct AstType *tag = expect_type_annotation(lex, "constant", ident, PAW_TRUE);
    struct AstExpr *init = test_next(lex, '=') ? expr0(lex) : NULL;
    semicolon(lex, "constant declaration");
    return NEW_NODE(lex, const_decl, start, next_id(lex), ident, annos, tag, init, is_pub);
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
            return fn_decl(lex, annos, is_pub);
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
            return use_decl(lex, is_pub);
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

static void toplevel_items(struct Lex *lex, struct AstDeclList *items)
{
    while (!test(lex, TK_END)) {
        struct AstDecl *item = toplevel_item(lex);
        AstDeclList_push(lex->ast, items, item);
    }
}

static void skip_hashbang(struct Lex *lex)
{
    // Special case: don't advance past the '#', since it might be the start of
    // an annotation. If the first char is not '\0', then there is guaranteed to
    // be another char, possibly '\0' itself.
    if (test_next(lex, TK_HASHBANG)) {
        while (!test(lex, TK_END)) {
            char const c = *lex->ptr;
            skip(lex);
            if (c == '\r' || c == '\n')
                break;
        }
    }
}

// Effectively add the following text at the top of each Paw source file:
//
//     use prelude;
//     use prelude::*;
//
static void import_prelude(struct Lex *lex, struct AstDeclList *items)
{
    if (lex->modno == PRELUDE_MODNO) return;

    struct AstIdent const ident = {
        .name = SCAN_STR(lex->C, "prelude"),
        .span = span_from(lex, lex->loc),
    };

    struct AstPath path;
    pawAst_path_init(lex->ast, &path, ident.span);
    pawAst_add_segment(lex->ast, path.segments, next_id(lex), ident, NULL);

    struct AstIdent const none = {0}; // no alias
    AstDeclList_push(lex->ast, items, NEW_NODE(lex, use_decl, lex->loc,
                next_id(lex), path, none, AST_USE_NORMAL, PAW_FALSE));
    AstDeclList_push(lex->ast, items, NEW_NODE(lex, use_decl, lex->loc,
                next_id(lex), path, none, AST_USE_GLOB, PAW_FALSE));
}

static struct AstDecl *parse_module(struct Lex *lex, paw_Reader input, void *ud)
{
    pawX_set_source(lex, input, ud);
    skip_hashbang(lex);

    struct AstDeclList *items = AstDeclList_new(lex->ast);
    struct AstDecl *decl = NEW_NODE(lex, module_decl, lex->loc, next_id(lex), lex->modname, lex->modno, items);
    AstDeclList_push(lex->ast, lex->ast->modules, decl);

    import_prelude(lex, items);
    toplevel_items(lex, items);

    if (lex->ptr != lex->end)
        PARSE_ERROR(lex, unexpected_symbol, lex->loc);
    return decl;
}

static void init_lexer(struct Compiler *C, Str *modname, struct Lex *lex)
{
    *lex = (struct Lex){
        .pool = pawP_pool_new(C, C->aux_stats),
        .modno = C->modnames->count,
        .modname = modname,
        .strings = C->strings,
        .ast = C->ast,
        .dm = C->dm,
        .P = C->P,
        .C = C,
    };
    ModuleNames_push(C, C->modnames, modname);
}

static void ast_callback(struct Compiler *C, struct Ast *ast)
{
    if (pawP_push_callback(C, "paw.on_build_ast")) {
        paw_Env *P = ENV(C);
        paw_push_rawptr(P, ast);
        paw_call(P, 1);
    }
}

struct AstDecl *pawP_parse_module(struct Compiler *C, Str *modname, paw_Reader input, void *ud)
{
    struct Lex lex;
    init_lexer(C, modname, &lex);

    struct AstDecl *decl = parse_module(&lex, input, ud);
    pawP_pool_free(C, lex.pool);
    return decl;
}
