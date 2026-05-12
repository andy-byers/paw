// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "parse.h"
#include "ast.h"
#include "compile.h"
#include "env.h"
#include "error.h"
#include "map.h"

#define MAX_ASSOC_ITEMS 10000
#define MAX_LITERAL_ELEMENTS 10000
#define MAX_PATH_SEGMENTS 10000

_Static_assert(PAW_MAX_ARGUMENTS < INT_MAX, "");
_Static_assert(PAW_MAX_VARIANTS < INT_MAX, "");
_Static_assert(PAW_MAX_FIELDS < INT_MAX, "");
_Static_assert(PAW_MAX_UPVALUES < INT_MAX, "");
_Static_assert(MAX_ASSOC_ITEMS < INT_MAX, "");
_Static_assert(MAX_LITERAL_ELEMENTS < INT_MAX, "");
_Static_assert(MAX_PATH_SEGMENTS < INT_MAX, "");

#define PARSE_ERROR(X_, Kind_, ...) THROW_ERROR((X_)->C, \
        Kind_, .modname = (X_)->modname, __VA_ARGS__)

#define NEW_NODE(Lex_, Kind_, ...) \
    pawAst_new_##Kind_((Lex_)->ast, __VA_ARGS__)
#define RANGE(From_, To_) SourceSpan_from_range(From_, To_)
#define RANGE1(Loc_) RANGE(Loc_, Loc_)
#define RANGE_START(Span_) SourceSpan_range_start(Span_)
#define RANGE_END(Span_) SourceSpan_range_end(Span_)
#define TOKEN_START(Token_) RANGE_START((Token_).span)
#define TOKEN_END(Token_) RANGE_END((Token_).span)
#define NODE_SPAN(Node_) ((Node_)->hdr.span)
#define NODE_START(Node_) RANGE_START((Node_)->hdr.span)
#define NODE_END(Node_) RANGE_END((Node_)->hdr.span)

#define LOWEST_PRECEDENCE 0

static struct SourceSpan merge_spans(struct SourceSpan first, struct SourceSpan second)
{
    return RANGE(RANGE_START(first), RANGE_END(second));
}

static Str const *scan_str(struct Lex *lex, char const *str)
{
    return SCAN_STR(lex->C, str);
}

static struct SourceSpan span_from(struct Lex *lex, struct SourceLoc start)
{
    return SourceSpan_from_range(start, RANGE_END(lex->t.span));
}

static NodeId next_id(struct Lex *lex)
{
    return (NodeId){(unsigned)++lex->ast->node_count};
}


// recursive non-terminals
static struct AstExpr *expression(struct Lex *, unsigned);
static struct AstPat *pattern(struct Lex *);

static struct SourceLoc delim_next(struct Lex *lex, TokenKind want, TokenKind open, struct SourceLoc open_loc)
{
    struct SourceLoc const loc = TOKEN_START(lex->t);
    TokenKind const have = lex->t.kind;
    if (have != want) {
        if (have == TK_GREATER2 && want == '>') {
            // special case: split '>>' into 2 '>'
            lex->t.kind = '>';
            return loc;
        }
        PARSE_ERROR(lex, ExpectedDelimiter,
                .span = RANGE1(lex->loc),
                .close = (char)want,
                .open = (char)open,
                .open_loc = open_loc);
    }
    pawX_next(lex);
    return loc;
}

static void ensure_unique_decl_names(struct Lex *lex, AstDeclList *decls, char const *what)
{
    StringMap *names = StringMap_new(lex->C);
    StringMap_reserve(lex->C, names, decls->count);
    K_LIST_XFOREACH (decls, struct AstDecl *const, decl_ptr) {
        struct AstIdent const ident =
            AstIsFnDecl(*decl_ptr) ? AstGetFnDecl(*decl_ptr)->ident :
            AstIsAdtDecl(*decl_ptr) ? AstGetAdtDecl(*decl_ptr)->ident :
            AstIsTypeDecl(*decl_ptr) ? AstGetTypeDecl(*decl_ptr)->ident :
            AstIsTraitDecl(*decl_ptr) ? AstGetTraitDecl(*decl_ptr)->ident :
            AstIsVariantDecl(*decl_ptr) ? AstGetVariantDecl(*decl_ptr)->ident :
            AstIsParamDecl(*decl_ptr) ? AstGetParamDecl(*decl_ptr)->ident :
            AstGetFieldDecl(*decl_ptr)->ident;
        if (StringMap_insert(lex->C, names, ident.name, NULL))
            PARSE_ERROR(lex, DuplicateName,
                    .what = scan_str(lex, what),
                    .name = ident.name,
                    .span = ident.span);
    }
    StringMap_delete(lex->C, names);
}

static void enter_expression(struct Lex *lex)
{
    int const MAX_NESTING = 100000;
    if (lex->nest_depth > MAX_NESTING)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "nested expressions"),
                .span = RANGE1(lex->loc),
                .limit = MAX_NESTING);
    ++lex->nest_depth;
}

static void leave_expression(struct Lex *lex)
{
    paw_assert(lex->nest_depth >= 0);
    --lex->nest_depth;
}

static struct AstExpr *expect_expr0(struct Lex *lex)
{
    enter_expression(lex);
    struct SourceLoc const start = TOKEN_START(lex->t);
    struct AstExpr *expr = expression(lex, LOWEST_PRECEDENCE);
    leave_expression(lex);

    if (expr == NULL)
        PARSE_ERROR(lex, ExpectedExpression, span_from(lex, start));
    return expr;
}

static paw_Bool equals_cstr(struct Lex *lex, Str const *ident, unsigned cstr)
{
    return pawS_eq(ident, CACHED_STRING(ENV(lex), cstr));
}

// ORDER UnaryOp
enum UnOp {
    UN_NEG, // -
    UN_NOT, // !
    UN_BNOT, // ~
    UN_DEREF, // *
    UN_ADDROF, // &

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
        case '-':
            return UN_NEG;
        case '!':
            return UN_NOT;
        case '~':
            return UN_BNOT;
        case '*':
            return UN_DEREF;
        case '&':
            return UN_ADDROF;
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
        PARSE_ERROR(lex, UnexpectedSymbol, lex->t.span);
}

static void check_next(struct Lex *lex, TokenKind want)
{
    check(lex, want);
    skip(lex);
}

static void semicolon(struct Lex *lex, char const *where)
{
    if (!test_next(lex, ';'))
        PARSE_ERROR(lex, ExpectedSemicolon,
                .what = SCAN_STR(lex->C, where),
                .span = lex->t.span);
}

static void add_string_part(struct Lex *lex, struct AstStringList *parts, struct SourceSpan span, Value str)
{
    AstStringList_push(lex->ast, parts, (struct AstStringPart){
                .is_str = PAW_TRUE,
                .str.span = span,
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

static struct AstExpr *string_expr(struct Lex *lex, struct SourceSpan span, Value str)
{
    struct AstStringList *parts = AstStringList_new(lex->ast);
    add_string_part(lex, parts, span, str);
    return NEW_NODE(lex, string_expr, span, next_id(lex), parts);
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
        struct Token const t = lex->t;
        skip(lex); // skip string text
        add_string_part(lex, parts, t.span, t.value);
        add_expr_part(lex, parts, expect_expr0(lex));
        check_next(lex, TK_STRING_EXPR_CLOSE);
    } while (test(lex, TK_STRING_EXPR_OPEN));
    check(lex, TK_STRING_TEXT);
    struct SourceSpan const span = lex->t.span;
    add_string_part(lex, parts, span, lex->t.value);
    skip(lex); // skip string text
    return NEW_NODE(lex, string_expr, RANGE(start, RANGE_END(span)), next_id(lex), parts);
}

static struct AstIdent parse_ident(struct Lex *lex)
{
    check(lex, TK_NAME);
    struct Token const t = lex->t;
    Str *name = V_STR(t.value);
    skip(lex);

    return (struct AstIdent){
        .name = name,
        .span = t.span,
    };
}

static struct AstIdent parse_ident_or_underscore(struct Lex *lex)
{
    if (test(lex, TK_UNDERSCORE)) {
        struct Token const t = lex->t;
        skip(lex); // skip "_" token
        return (struct AstIdent){
            .name = CSTR(lex, CSTR_UNDERSCORE),
            .span = t.span,
        };
    } else {
        return parse_ident(lex);
    }
}

static void ensure_not_underscore(struct Lex *lex, struct AstIdent ident)
{
    if (equals_cstr(lex, ident.name, CSTR_UNDERSCORE))
        PARSE_ERROR(lex, UnexpectedUnderscore, ident.span);
}

static struct AstExpr *new_basic_lit(struct Lex *lex, struct SourceSpan span, Value value, enum BuiltinKind code)
{
    return NEW_NODE(lex, basic_lit, span, next_id(lex), value, code);
}

static struct AstType *unit_type(struct Lex *lex, struct SourceSpan span)
{
    struct AstTypeList *types = AstTypeList_new(lex->ast);
    return NEW_NODE(lex, tuple_type, span, next_id(lex), types);
}

static struct AstExpr *emit_bool(struct Lex *lex, struct SourceSpan span, paw_Bool b)
{
    return new_basic_lit(lex, span, I2V(b), BUILTIN_BOOL);
}

static struct AstType *parse_type(struct Lex *lex);
static struct AstGenericArg generic_arg(struct Lex *lex);

static struct AstGenericArg generic_arg_bound(struct Lex *lex)
{
    if (test(lex, TK_NAME)
            && pawX_peek(lex) == '=') {
        Str const *item = lex->t.value.p;
        skip(lex); // skip (name) token
        skip(lex); // skip '=' token
        struct AstType *type = parse_type(lex);
        return (struct AstGenericArg){
            .id = next_id(lex),
            .is_type = PAW_TRUE,
            .item = item,
            .t = type,
        };
    }

    return generic_arg(lex);
}

static struct AstDecl *variant_field_decl(struct Lex *lex)
{
    struct AstIdent const empty = {0};
    struct AstType *tag = parse_type(lex);
    return NEW_NODE(lex, field_decl, NODE_SPAN(tag), next_id(lex), empty, tag, PAW_FALSE);
}

#define DEFINE_LIST_PARSER(Name_, A_, B_, Limit_, What_, Fn_, List_)                                          \
    static struct SourceLoc parse_##Name_##_list(struct Lex *lex, struct List_ *list, struct SourceLoc start) \
    {                                                                                                         \
        do {                                                                                                  \
            if (test(lex, B_)) break;                                                                         \
            if (list->count == INT_MAX) break;                                                                \
            List_##_push((lex)->ast, list, (Fn_)(lex));                                                       \
        } while (test_next(lex, ','));                                                                        \
        if (list->count > (Limit_))                                                                           \
            PARSE_ERROR(lex, LimitExceeded, \
                    .what = SCAN_STR(lex->C, What_" in list"), \
                    .span = RANGE(start, lex->loc), \
                    .limit = Limit_); \
        struct SourceLoc const end = TOKEN_END(lex->t);                                                       \
        delim_next(lex, B_, A_, start);                                                                       \
        return end;                                                                                           \
    }
DEFINE_LIST_PARSER(arg, '(', ')', PAW_MAX_ARGUMENTS, "arguments", expect_expr0, AstExprList)
DEFINE_LIST_PARSER(variant_field, '(', ')', PAW_MAX_FIELDS, "variant fields", variant_field_decl, AstDeclList)
DEFINE_LIST_PARSER(generic_arg, '<', '>', INT_MAX, "generic arguments", generic_arg, AstGenericArgs)
DEFINE_LIST_PARSER(generic_arg_bound, '<', '>', INT_MAX, "generic arguments", generic_arg_bound, AstGenericArgs)

static AstGenericArgs *generic_args(struct Lex *lex, struct SourceLoc start)
{
    ++lex->expr_depth;
    struct AstGenericArgs *list = AstGenericArgs_new(lex->ast);
    parse_generic_arg_list(lex, list, start);
    if (list->count == 0)
        PARSE_ERROR(lex, EmptyTypeList,
                .span = RANGE1(start));

    --lex->expr_depth;
    return list;
}

static AstGenericArgs *generic_args_for_bound(struct Lex *lex, struct SourceLoc start)
{
    ++lex->expr_depth;
    struct AstGenericArgs *list = AstGenericArgs_new(lex->ast);
    parse_generic_arg_bound_list(lex, list, start);
    if (list->count == 0)
        PARSE_ERROR(lex, EmptyTypeList,
                .span = span_from(lex, start));

    --lex->expr_depth;
    return list;
}

static struct AstPath parse_pathexpr(struct Lex *lex)
{
    struct AstSegments *s = AstSegments_new(lex->ast);
    struct SourceLoc const start = TOKEN_START(lex->t);

    do {
    next_segment:
        if (s->count == INT_MAX) break;
        struct AstIdent const ident = parse_ident(lex);
        struct SourceSpan span = ident.span;
        struct AstGenericArgs *args = NULL;
        // permit "::<types..>" between segments
        if (test_next(lex, TK_COLON2)) {
            if (test_next(lex, '<')) {
                args = generic_args(lex, lex->loc);
            } else {
                pawAst_add_segment(lex->ast, s, ident.span, next_id(lex), ident, NULL);
                goto next_segment;
            }
            span = span_from(lex, RANGE_START(span));
        }
        pawAst_add_segment(lex->ast, s, span, next_id(lex), ident, args);
    } while (test_next(lex, TK_COLON2));
    struct SourceLoc const finish = RANGE_END(AstSegments_last(s).span);

    if (s->count > MAX_PATH_SEGMENTS)
        PARSE_ERROR(lex, PathTooLong,
                .max_segments = MAX_PATH_SEGMENTS,
                .span = RANGE(start, finish));

    return (struct AstPath){
        .span = RANGE(start, finish),
        .segments = s,
    };
}

static struct AstPath parse_pathtype(struct Lex *lex, paw_Bool allow_item_constraints)
{
    struct AstSegments *s = AstSegments_new(lex->ast);
    struct SourceLoc const start = RANGE_START(lex->t.span);
    struct SourceLoc finish;

    do {
        if (s->count == INT_MAX) break;
        struct AstIdent const ident = parse_ident(lex);
        struct SourceSpan span = ident.span;
        struct AstGenericArgs *args = NULL;
        if (test_next(lex, '<')) {
            // _<types..> is not allowed
            ensure_not_underscore(lex, ident);
            args = allow_item_constraints
                ? generic_args_for_bound(lex, start)
                : generic_args(lex, start);
            span = span_from(lex, RANGE_START(span));
        }
        finish = RANGE_END(span);
        pawAst_add_segment(lex->ast, s, span, next_id(lex), ident, args);
    } while (test_next(lex, TK_COLON2));

    struct SourceSpan const span = RANGE(start, finish);

    if (s->count > MAX_PATH_SEGMENTS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "path segments"),
                .span = span,
                .limit = MAX_PATH_SEGMENTS);

    return (struct AstPath){
        .segments = s,
        .span = span,
    };
}

static struct AstExpr *path_expr(struct Lex *lex)
{
    struct AstPath const path = parse_pathexpr(lex);
    return NEW_NODE(lex, path_expr, path.span, next_id(lex), path);
}

static struct AstPat *struct_field_pat(struct Lex *lex)
{
    struct AstIdent const ident = parse_ident(lex);
    struct AstPat *pat = test_next(lex, ':') ? pattern(lex)
        // bind field to variable of same name
        : NEW_NODE(lex, ident_pat, ident.span, next_id(lex), ident);
    struct SourceSpan const span = merge_spans(ident.span, NODE_SPAN(pat));
    return NEW_NODE(lex, field_pat, span, next_id(lex), ident, pat);
}

DEFINE_LIST_PARSER(variant_field_pat, '(', ')', PAW_MAX_FIELDS, "variant fields", pattern, AstPatList)
DEFINE_LIST_PARSER(struct_field_pat, '{', '}', PAW_MAX_FIELDS, "struct fields", struct_field_pat, AstPatList)

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
    } else if (equals_cstr(lex, ident.name, CSTR_CHAR)) {
        return BUILTIN_CHAR;
    } else if (equals_cstr(lex, ident.name, CSTR_INT)) {
        return BUILTIN_INT;
    } else if (equals_cstr(lex, ident.name, CSTR_FLOAT)) {
        return BUILTIN_FLOAT;
    } else if (equals_cstr(lex, ident.name, CSTR_STR)) {
        return BUILTIN_STR;
    } else {
        return NBUILTINS;
    }
}

static struct AstPat *compound_pat(struct Lex *lex)
{
    struct SourceLoc const start = lex->loc;
    struct AstPath const path = parse_pathexpr(lex);
    if (test_next(lex, '(')) {
        AstPatList *fields = AstPatList_new(lex->ast);
        struct SourceLoc const end = parse_variant_field_pat_list(lex, fields, start);
        return NEW_NODE(lex, variant_pat, RANGE(start, end), next_id(lex), path, fields);
    } else if (test_next(lex, '{')) {
        AstPatList *fields = AstPatList_new(lex->ast);
        struct SourceLoc const end = parse_struct_field_pat_list(lex, fields, start);

        StringMap *names = StringMap_new(lex->C);
        K_LIST_XFOREACH (fields, struct AstPat *const, field_ptr) {
            struct AstFieldPat const *field = AstGetFieldPat(*field_ptr);
            if (StringMap_insert(lex->C, names, field->ident.name, NULL))
                PARSE_ERROR(lex, DuplicateName,
                        .what = scan_str(lex, "struct pattern field"),
                        .name = field->ident.name,
                        .span = field->ident.span);
        }
        StringMap_delete(lex->C, names);

        return NEW_NODE(lex, struct_pat, RANGE(start, end), next_id(lex), path, fields);
    }
    if (path.segments->count == 1) {
        struct AstSegment const segment = K_LIST_FIRST(path.segments);
        if (get_builtin_kind(lex, segment.ident) != NBUILTINS)
            PARSE_ERROR(lex, UseOfReservedIdentifier,
                    .span = segment.ident.span,
                    .name = segment.ident.name);
        if (segment.args == NULL)
            return NEW_NODE(lex, ident_pat, segment.span, next_id(lex), segment.ident);
    }
    return NEW_NODE(lex, path_pat, path.span, next_id(lex), path);
}

static struct AstPat *wildcard_pat(struct Lex *lex)
{
    struct SourceSpan const span = lex->t.span;
    skip(lex); // "_" token
    return NEW_NODE(lex, wildcard_pat, span, next_id(lex));
}

static struct AstPat *ref_pat(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "*" token
    struct AstPat *referent = pattern(lex);
    return NEW_NODE(lex, ref_pat, span_from(lex, start), next_id(lex), referent);
}

static struct AstPat *ptr_pat(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "*" token
    struct AstPat *pointee = pattern(lex);
    return NEW_NODE(lex, ptr_pat, span_from(lex, start), next_id(lex), pointee);
}

static struct AstPat *tuple_pat(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "(" token
    struct AstPatList *elems = AstPatList_new(lex->ast);
    struct SourceLoc const end = parse_variant_field_pat_list(lex, elems, start);
    return NEW_NODE(lex, tuple_pat, RANGE(start, end), next_id(lex), elems);
}

static struct AstExpr *literal_expr(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
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
        case TK_STR:
            code = BUILTIN_STR;
            break;
        case TK_STRING_TEXT:
            code = BUILTIN_STR;
            break;
        default:
            PARSE_ERROR(lex, NonliteralPattern,
                    .span = RANGE(start, lex->loc));
    }
    Value const value = lex->t.value;
    struct SourceLoc const end = TOKEN_END(lex->t);
    struct AstExpr *expr = new_basic_lit(lex, RANGE(start, end), value, code);
    struct AstLiteralExpr *lit = AstGetLiteralExpr(expr);
    skip(lex); // literal token

    if (negative) {
        if (code == BUILTIN_FLOAT) {
            V_SET_FLOAT(&lit->basic.value, -V_FLOAT(value));
        } else if (code != BUILTIN_INT) {
            PARSE_ERROR(lex, InvalidLiteralNegation,
                    .span = RANGE1(lex->loc));
        } else if (V_UINT(value) > (paw_Uint)PAW_INT_MAX + 1) {
            PARSE_ERROR(lex, NegativeIntegerOutOfRange,
                    .span = NODE_SPAN(expr),
                    .uint64 = value.u);
        } else if (V_UINT(value) == (paw_Uint)PAW_INT_MAX + 1) {
            V_SET_INT(&lit->basic.value, PAW_INT_MIN);
        } else {
            V_SET_INT(&lit->basic.value, -(paw_Int)V_UINT(value));
        }
    } else if (code == BUILTIN_INT && value.u > (paw_Uint)PAW_INT_MAX) {
        PARSE_ERROR(lex, IntegerOutOfRange,
                .span = NODE_SPAN(expr),
                .uint64 = value.u);
    }
    return expr;
}

static struct AstPat *literal_pat(struct Lex *lex)
{
    struct AstExpr *expr = literal_expr(lex);
    return NEW_NODE(lex, literal_pat, NODE_SPAN(expr), next_id(lex), expr);
}

static struct AstPat *alternative_pat(struct Lex *lex)
{
    switch (lex->t.kind) {
        case TK_NAME:
            return compound_pat(lex);
        case TK_UNDERSCORE:
            return wildcard_pat(lex);
        case '&':
            return ref_pat(lex);
        case '*':
            return ptr_pat(lex);
        case '(':
            return tuple_pat(lex);
        default:
            return literal_pat(lex);
    }
}

static struct AstPat *pattern(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    struct AstPat *first = alternative_pat(lex);
    if (!test_next(lex, '|')) return first;

    AstPatList *pats = AstPatList_new(lex->ast);
    AstPatList_push(lex->ast, pats, first);

    struct AstPat *next;
    do {
        next = alternative_pat(lex);
        AstPatList_push(lex->ast, pats, next);
    } while (test_next(lex, '|'));

    return NEW_NODE(lex, or_pat, RANGE(start, NODE_END(next)), next_id(lex), pats);
}

static struct AstExpr *expr_except_struct_lit(struct Lex *lex);

static struct AstDeclList *variant_field_list(struct Lex *lex, struct SourceLoc start)
{
    ++lex->expr_depth;
    struct AstDeclList *list = AstDeclList_new(lex->ast);
    parse_variant_field_list(lex, list, start);
    if (list->count == 0)
        PARSE_ERROR(lex, EmptyVariantFieldList,
                .span = span_from(lex, start));

    --lex->expr_depth;
    return list;
}

static struct AstType *parse_tuple_type(struct Lex *lex, struct AstType *first, struct SourceLoc lparen)
{
    struct AstTypeList *elems = AstTypeList_new(lex->ast);
    AstTypeList_push(lex->ast, elems, first);

    do {
        if (test(lex, ')')) break;
        if (elems->count == INT_MAX) break;
        struct AstType *type = parse_type(lex);
        AstTypeList_push(lex->ast, elems, type);
    } while (test_next(lex, ','));

    if (elems->count > PAW_MAX_FIELDS)
        PARSE_ERROR(lex, TooManyTupleElements,
                .max_elements = PAW_MAX_FIELDS,
                .span = span_from(lex, lparen));

    struct SourceLoc const rparen = delim_next(lex, ')', '(', lparen);
    struct SourceSpan const tuple_range = RANGE(lparen, rparen);
    return NEW_NODE(lex, tuple_type, tuple_range, next_id(lex), elems);
}

static struct AstType *parse_paren_type(struct Lex *lex, struct SourceLoc lparen)
{
    if (test(lex, ')')) {
        struct SourceLoc const rparen = TOKEN_START(lex->t);
        skip(lex); // skip ")" token
        return unit_type(lex, RANGE(lparen, rparen));
    }

    struct AstType *type = parse_type(lex);
    if (test_next(lex, ','))
        return parse_tuple_type(lex, type, lparen);

    delim_next(lex, ')', '(', lparen);
    return type;
}

static struct AstType *parse_pointer_type(struct Lex *lex, struct SourceLoc start)
{
    paw_Bool const is_mut = test_next(lex, TK_MUT);
    struct AstType *pointee = parse_type(lex);
    return NEW_NODE(lex, ref_type, RANGE(start, lex->loc), next_id(lex), pointee, is_mut);
}

static struct AstType *parse_signature(struct Lex *, struct SourceLoc);

static struct AstGenericArg generic_arg(struct Lex *lex)
{
    struct AstType *type = parse_type(lex);
    if (type != NULL)
        return (struct AstGenericArg){
            .id = next_id(lex),
            .is_type = PAW_TRUE,
            .t = type,
        };

    return (struct AstGenericArg){
        .id = next_id(lex),
        .is_type = PAW_FALSE,
        .k = expect_expr0(lex),
    };
}

static struct AstType *parse_type(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    if (test_next(lex, '(')) {
        return parse_paren_type(lex, start);
    } else if (test_next(lex, '*')) {
        return parse_pointer_type(lex, start);
    } else if (test_next(lex, TK_FN)) {
        return parse_signature(lex, start);
    } else if (test(lex, TK_UNDERSCORE)) {
        struct SourceSpan const span = lex->t.span;
        skip(lex); // skip "_" token
        if (lex->fn_depth == 0)
            PARSE_ERROR(lex, UnexpectedUnderscore, span);
        return NEW_NODE(lex, infer_type, span, next_id(lex));
    } else if (test_next(lex, '[')) {
        if (test_next(lex, ']')) {
            struct AstType *elem = parse_type(lex);
            return NEW_NODE(lex, slice_type,
                    RANGE(start, NODE_END(elem)),
                    next_id(lex), elem);
        } else {
            struct AstExpr *length = expect_expr0(lex);
            struct SourceLoc const rbracket = delim_next(lex, ']', '[', start);
            struct AstType *elem = parse_type(lex);
            return NEW_NODE(lex, array_type,
                    RANGE(start, rbracket),
                    next_id(lex), elem, length);
        }
    } else {
        struct AstPath path = parse_pathtype(lex, PAW_FALSE);
        return NEW_NODE(lex, path_type, path.span, next_id(lex), path);
    }
}

static struct AstType *parse_return_type(struct Lex *lex)
{
    if (test(lex, '!')) {
        struct SourceSpan const span = lex->t.span;
        skip(lex); // skip "!" token
        // type "!" can only appear as a function return type
        return NEW_NODE(lex, never_type, span, next_id(lex));
    } else {
        return parse_type(lex);
    }
}

static struct AstType *type_annotation(struct Lex *lex)
{
    if (test_next(lex, ':'))
        return parse_type(lex);
    return NULL; // needs inference
}

static struct AstType *expect_type_annotation(struct Lex *lex, char const *what, struct AstIdent ident)
{
    struct AstType *type = type_annotation(lex);
    if (type == NULL)
        PARSE_ERROR(lex, ExpectedTypeAnnotation,
                .what = SCAN_STR(lex->C, what),
                .span = ident.span,
                .name = ident.name);
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
    pawAst_add_segment(lex->ast, path.segments, span, next_id(lex), ident, NULL);
    return NEW_NODE(lex, path_type, span, next_id(lex), path);
}

static struct AstDecl *fn_param_decl(struct Lex *lex)
{
    struct AstIdent const ident = parse_ident_or_underscore(lex);
    struct AstType *tag = expect_type_annotation(lex, "parameter", ident);
    return NEW_NODE(lex, param_decl, merge_spans(ident.span, NODE_SPAN(tag)),
            next_id(lex), ident, tag);
}

static struct AstDecl *closure_param_decl(struct Lex *lex)
{
    struct AstIdent const ident = parse_ident_or_underscore(lex);
    struct AstType *tag = type_annotation(lex);
    struct SourceLoc const end = tag != NULL ? NODE_END(tag) : RANGE_END(ident.span);
    return NEW_NODE(lex, param_decl, RANGE(RANGE_START(ident.span), end),
            next_id(lex), ident, tag);
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
    // NAME [":" Trait {"+" Trait}]
    struct AstIdent const ident = parse_ident(lex);
    struct AstBoundList *bounds = parse_generic_bounds(lex);
    struct SourceLoc const end = bounds != NULL
        ? RANGE_END(K_LIST_LAST(bounds).path.span)
        : RANGE_END(ident.span);
    return NEW_NODE(lex, generic_type_decl, RANGE(RANGE_START(ident.span), end),
            next_id(lex), ident, bounds);
}

DEFINE_LIST_PARSER(sig_param, '(', ')', PAW_MAX_ARGUMENTS, "function parameters", parse_type, AstTypeList)
DEFINE_LIST_PARSER(closure_param, '|', '|', PAW_MAX_ARGUMENTS, "closure parameters", closure_param_decl, AstDeclList)
DEFINE_LIST_PARSER(generic, '<', '>', INT_MAX, "generics", generic_param, AstDeclList)

static struct AstExpr *basic_expr(struct Lex *lex, unsigned prec);

static struct AstExpr *expect_expr(struct Lex *lex, unsigned prec)
{
    enter_expression(lex);
    struct SourceLoc const start = TOKEN_START(lex->t);
    struct AstExpr *expr = expression(lex, prec);
    leave_expression(lex);

    if (expr == NULL)
        PARSE_ERROR(lex, ExpectedExpression, RANGE1(start));
    return expr;
}

static struct AstExpr *sitem_expr(struct Lex *lex)
{
    struct AstIdent const ident = parse_ident(lex);
    struct AstExpr *value;
    if (test_next(lex, ':')) {
        value = expect_expr0(lex);
    } else {
        // "name" by itself is shorthand for "name: name"
        struct AstPath path;
        pawAst_path_init(lex->ast, &path, ident.span);
        pawAst_add_segment(lex->ast, path.segments, ident.span, next_id(lex), ident, NULL);
        value = NEW_NODE(lex, path_expr, ident.span, next_id(lex), path);
    }
    int const fid = INT_MAX; // nonnegative means determine later
    return NEW_NODE(lex, named_field_expr, RANGE(RANGE_START(ident.span), NODE_END(value)),
            next_id(lex), ident, value, fid);
}

DEFINE_LIST_PARSER(sitem, '{', '}', PAW_MAX_FIELDS, "struct items", sitem_expr, AstExprList)

static struct AstExpr *unop_expr(struct Lex *lex, enum UnOp op)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // unary operator token
    enum UnaryOp const unop = CAST(enum UnaryOp, op); // same order
    struct AstExpr *target = expect_expr(lex, kUnOpPrecedence);
    return NEW_NODE(lex, unop_expr, RANGE(start, NODE_END(target)), next_id(lex), unop, target);
}

// Parse either a parenthsized expression or a tuple
static struct AstExpr *paren_expr(struct Lex *lex)
{
    // Just parse and return the expression contained within the parenthesis.
    // There is no need for an extra node type.
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "(" token
    if (test(lex, ')')) {
        struct SourceLoc const end = TOKEN_END(lex->t);
        skip(lex); // ")" token
        return new_basic_lit(lex, RANGE(start, end), I2V(0), BUILTIN_UNIT);
    }

    ++lex->expr_depth;
    struct AstExpr *expr = expect_expr0(lex);
    --lex->expr_depth;
    if (test(lex, ')')) {
        struct SourceLoc const end = TOKEN_END(lex->t);
        skip(lex); // ")" token
        return NEW_NODE(lex, paren_expr, RANGE(start, end), next_id(lex), expr);
    }

    check_next(lex, ',');
    struct AstExprList *elems = AstExprList_new(lex->ast);
    AstExprList_push(lex->ast, elems, expr);
    parse_arg_list(lex, elems, start);
    struct SourceLoc const end = NODE_END(K_LIST_LAST(elems));
    return NEW_NODE(lex, tuple_lit, RANGE(start, end), next_id(lex), elems);
}

static struct AstType *parse_signature(struct Lex *lex, struct SourceLoc start)
{
    struct SourceLoc end;
    check_next(lex, '(');
    struct AstTypeList *params = AstTypeList_new(lex->ast);
    if (!test(lex, ')')) {
        end = parse_sig_param_list(lex, params, start);
    } else {
        end = TOKEN_END(lex->t);
        skip(lex); // skip ")" token
    }

    struct AstType *result = NULL;
    if (test_next(lex, TK_ARROW)) {
        result = parse_return_type(lex);
        end = NODE_END(result);
    }
    return NEW_NODE(lex, fn_type, RANGE(start, end), next_id(lex), params, result);
}

static paw_Bool end_of_block(struct Lex *lex)
{
    return test(lex, '}') || // found proper end
           test(lex, TK_END); // truncated block
}

static struct AstExpr *index_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "[" token
    struct AstExpr *index = expect_expr0(lex);
    struct SourceLoc const end = delim_next(lex, ']', '[', start);
    return NEW_NODE(lex, index, RANGE(start, end), next_id(lex), target, index);
}

static struct AstExpr *array_lit(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "[" token

    struct AstExprList *elems = AstExprList_new(lex->ast);
    do {
        if (test(lex, ']')) break;
        if (elems->count == INT_MAX) break;
        struct AstExpr *elem = expect_expr0(lex);
        AstExprList_push(lex->ast, elems, elem);
    } while (test_next(lex, ','));

    if (elems->count == MAX_LITERAL_ELEMENTS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "elements in array literal"),
                .span = span_from(lex, start),
                .limit = MAX_LITERAL_ELEMENTS);

    struct SourceLoc const end = delim_next(lex, ']', '[', start);
    return NEW_NODE(lex, array_lit, RANGE(start, end), next_id(lex), elems);
}

// Parse a composite literal expression
static struct AstExpr *composite_lit(struct Lex *lex, struct AstPathExpr *path)
{
    skip(lex); // "{" token

    AstExprList *items = AstExprList_new(lex->ast);
    struct SourceLoc const end = parse_sitem_list(lex, items, RANGE_START(path->span));

    // make sure each field name is unique
    StringMap *names = StringMap_new(lex->C);
    K_LIST_XFOREACH (items, struct AstExpr *const, field_ptr) {
        struct AstFieldExpr const *field = AstGetFieldExpr(*field_ptr);
        if (StringMap_insert(lex->C, names, field->ident.name, NULL))
            PARSE_ERROR(lex, DuplicateName,
                    .what = scan_str(lex, "struct literal field"),
                    .name = field->ident.name,
                    .span = field->ident.span);
    }
    StringMap_delete(lex->C, names);

    struct SourceLoc const start = RANGE_START(path->span);
    return NEW_NODE(lex, composite_lit, RANGE(start, end),
            next_id(lex), path->path, items);
}

static struct AstExpr *try_composite_lit(struct Lex *lex, struct AstExpr *expr)
{
    if (AstIsPathExpr(expr) && lex->expr_depth >= 0)
        expr = composite_lit(lex, AstGetPathExpr(expr));

    return expr;
}

static struct AstExpr *selector_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc const start = NODE_START(target);
    skip(lex); // "." token
    if (test(lex, TK_NAME)) {
        struct AstIdent const ident = parse_ident(lex);
        return NEW_NODE(lex, name_selector, RANGE(start, RANGE_END(ident.span)),
                next_id(lex), target, ident);
    } else if (test(lex, TK_INT)) {
        struct Token const index = lex->t;
        skip(lex); // integer token
        return NEW_NODE(lex, index_selector, RANGE(start, RANGE_END(index.span)),
                next_id(lex), target, V_INT(index.value));
    }

    PARSE_ERROR(lex, InvalidSelector,
            .span = span_from(lex, start)); // no return
}

static struct AstExpr *call_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc const start = NODE_START(target);
    skip(lex); // "(" token
    struct AstExprList *args = AstExprList_new(lex->ast);
    struct SourceLoc const end = parse_arg_list(lex, args, start);
    return NEW_NODE(lex, call_expr, RANGE(start, end), next_id(lex), target, args);
}

static struct AstExpr *chain_expr(struct Lex *lex, struct AstExpr *target)
{
    struct SourceLoc const start = NODE_START(target);
    struct SourceLoc const end = TOKEN_START(lex->t);
    skip(lex); // "?" token

    if (lex->fn_depth == 0)
        PARSE_ERROR(lex, ChainOutsideFunction,
                .span = RANGE(start, end));

    return NEW_NODE(lex, chain_expr, RANGE(start, end), next_id(lex), target);
}

#define IS_SELF_TYPE(Lex_, Type_) (AstIsPathType(Type_) && AstGetPathType(Type_)->path.segments->count == 1 && \
        pawS_eq(K_LIST_FIRST(AstGetPathType(Type_)->path.segments).ident.name, SELF_TYPENAME(Lex_)))
#define IS_SELF_VAR(Lex_, Ident_) pawS_eq((Ident_).name, CACHED_STRING(ENV(Lex_), CSTR_SELF))

static struct AstDeclList *fn_parameters(struct Lex *lex, paw_Bool *is_method)
{
    *is_method = PAW_FALSE;
    struct SourceLoc const start = TOKEN_START(lex->t);
    check_next(lex, '(');

    struct AstDeclList *params = AstDeclList_new(lex->ast);
    if (lex->in_impl && !test(lex, ')')) {
        struct SourceLoc const param_start = TOKEN_START(lex->t);
        // check for receiver parameter
        //
        //  shorthand syntax     | mut | type
        // ----------------------|-----|-----------
        //  self                 | no  | Self
        //  mut self             | yes | Self
        //  *self                | no  | *Self
        //  mut self: *Self      | yes | *Self
        //  *mut self            | no  | *mut Self
        //  mut self: *mut Self  | yes | *mut Self
        //
        // Note that only a single "&" can appear before "self".
        struct AstType *tag;
        paw_Bool const is_shorthand_ptr = test_next(lex, '*');
        paw_Bool const is_shorthand_mut = is_shorthand_ptr && test_next(lex, TK_MUT);
        struct AstIdent const ident = parse_ident_or_underscore(lex);
        if ((IS_SELF_VAR(lex, ident) && test(lex, ':') && is_shorthand_ptr)
                || (!IS_SELF_VAR(lex, ident) && is_shorthand_ptr)) {
            PARSE_ERROR(lex, InvalidSelfPtrShorthand,
                    .span = RANGE(param_start, RANGE_END(ident.span)),
                    .is_mut = is_shorthand_mut);
        }
        if (IS_SELF_VAR(lex, ident)) {
            *is_method = PAW_TRUE;
            if (test(lex, ':')) {
                tag = type_annotation(lex);
            } else {
                tag = self_type(lex, ident.span);
            }
        } else {
            tag = type_annotation(lex);
        }
        if (is_shorthand_ptr)
            tag = NEW_NODE(lex, ref_type, RANGE(start, NODE_END(tag)), next_id(lex), tag, is_shorthand_mut);

        struct AstDecl *first = NEW_NODE(lex, param_decl,
                RANGE(start, NODE_END(tag)), next_id(lex),
                ident, tag);
        AstDeclList_push(lex->ast, params, first);
        test_next(lex, ',');
    }

    do {
        if (test(lex, ')')) break;
        if (params->count == INT_MAX) break;
        AstDeclList_push(lex->ast, params, fn_param_decl(lex));
    } while (test_next(lex, ','));

    if (params->count > PAW_MAX_ARGUMENTS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "function parameters"),
                .span = span_from(lex, start),
                .limit = PAW_MAX_ARGUMENTS);

    delim_next(lex, ')', '(', start);
    return params;
}

static struct AstDeclList *closure_params(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    check_next(lex, '|');
    struct AstDeclList *params = AstDeclList_new(lex->ast);
    parse_closure_param_list(lex, params, start);
    ensure_unique_decl_names(lex, params, "closure parameter");
    return params;
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

static struct AstExpr *block(struct Lex *lex);

static struct AstExpr *closure(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    struct AstDeclList *params = closure_params(lex);
    struct AstType *result = NULL;
    struct AstExpr *expr;
    if (test_next(lex, TK_ARROW)) {
        result = parse_return_type(lex);
        expr = block(lex);
    } else {
        expr = expect_expr0(lex);
    }
    return NEW_NODE(lex, closure_expr, RANGE(start, NODE_END(expr)),
            next_id(lex), params, result, expr);
}

static struct AstExpr *if_expr(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "if" token
    struct AstExpr *cond = expr_except_struct_lit(lex);
    struct AstExpr *then_arm = block(lex);

    struct SourceLoc end = NODE_END(then_arm);
    struct AstExpr *else_arm = NULL;
    if (test_next(lex, TK_ELSE)) {
        // transform "else if" construct:
        //   before: "if a {A} else if b {B} else {C}"
        //    after: "if a {A} else {if b {B} else {C}}"
        else_arm = test(lex, TK_IF) ? if_expr(lex) : block(lex);
        end = NODE_END(else_arm);
    }
    return NEW_NODE(lex, if_expr, RANGE(start, end), next_id(lex),
            cond, then_arm, else_arm);
}

static struct AstExpr *loop_block(struct Lex *lex)
{
    ++lex->loop_depth;
    struct AstExpr *expr = block(lex);
    --lex->loop_depth;
    return expr;
}

static struct AstExpr *for_expr(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "for" token
    struct AstPat *pat = pattern(lex);
    check_next(lex, TK_IN);
    struct AstExpr *target = expr_except_struct_lit(lex);
    struct AstExpr *expr = loop_block(lex);
    return NEW_NODE(lex, for_expr, RANGE(start, NODE_END(expr)),
            next_id(lex), pat, target, expr);
}

static struct AstExpr *loop_expr(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "loop" token
    struct AstExpr *expr = loop_block(lex);
    return NEW_NODE(lex, loop_expr, RANGE(start, NODE_END(expr)),
            next_id(lex), expr);
}

static struct AstExpr *while_expr(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "while" token
    struct AstExpr *cond = expr_except_struct_lit(lex);
    struct AstExpr *expr = loop_block(lex);
    return NEW_NODE(lex, while_expr, RANGE(start, NODE_END(expr)),
            next_id(lex), cond, expr);
}

static struct AstExpr *return_expr(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "return" token

    struct AstExpr *expr = expression(lex, LOWEST_PRECEDENCE);
    struct SourceLoc const end = expr != NULL ? NODE_END(expr) : TOKEN_END(lex->t);

    if (lex->fn_depth == 0)
        PARSE_ERROR(lex, ReturnOutsideFunction,
                .span = RANGE(start, end));

    return NEW_NODE(lex, return_expr, RANGE(start, end), next_id(lex), expr);
}

static struct AstExpr *jump_expr(struct Lex *lex, enum JumpKind kind)
{
    struct SourceSpan const span = lex->t.span;
    skip(lex); // "break" or "continue" token

    if (lex->loop_depth == 0)
        PARSE_ERROR(lex, JumpOutsideLoop,
                .what = scan_str(lex, kind == JUMP_BREAK
                    ? "break" : "continue"),
                .span = span);

    return NEW_NODE(lex, jump_expr, span, next_id(lex), kind);
}

static struct AstExpr *match_arm(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    struct AstPat *pat = pattern(lex);
    struct AstExpr *guard = NULL;
    if (test_next(lex, TK_IF))
        guard = expr_except_struct_lit(lex);

    check_next(lex, TK_FAT_ARROW);
    struct AstExpr *result = expect_expr0(lex);
    return NEW_NODE(lex, match_arm, RANGE(start, NODE_END(result)),
            next_id(lex), pat, guard, result);
}

static struct AstExpr *match_expr(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "match" token
    struct AstExpr *target = expr_except_struct_lit(lex);
    struct AstExprList *arms = AstExprList_new(lex->ast);
    check_next(lex, '{');
    do {
        if (test(lex, '}')) break;
        struct AstExpr *arm = match_arm(lex);
        AstExprList_push(lex->ast, arms, arm);
    } while (test_next(lex, ','));
    struct SourceLoc const end = delim_next(lex, '}', '{', start);
    return NEW_NODE(lex, match_expr, RANGE(start, end), next_id(lex), target, arms);
}

static struct AstExpr *primary_expr(struct Lex *lex)
{
    struct SourceSpan const span = lex->t.span;
    struct Token const t = lex->t;

    switch (t.kind) {
        case '(':
            return paren_expr(lex);
        case '[':
            return array_lit(lex);
        case TK_NAME:
            return path_expr(lex);
        case TK_TRUE:
            skip(lex);
            return emit_bool(lex, span, PAW_TRUE);
        case TK_FALSE:
            skip(lex);
            return emit_bool(lex, span, PAW_FALSE);
        case TK_CHAR:
            skip(lex);
            return new_basic_lit(lex, span, t.value, BUILTIN_CHAR);
        case TK_INT:
            skip(lex);
            return new_basic_lit(lex, span, t.value, BUILTIN_INT);
        case TK_FLOAT:
            skip(lex);
            return new_basic_lit(lex, span, t.value, BUILTIN_FLOAT);
        case TK_STR:
            skip(lex);
            return new_basic_lit(lex, span, t.value, BUILTIN_STR);
        case TK_STRING_TEXT:
            skip(lex);
            return string_expr(lex, span, t.value);
        case TK_STRING_EXPR_OPEN:
            return string_interp_expr(lex, RANGE_START(span));
        case TK_RETURN:
            return return_expr(lex);
        case TK_BREAK:
            return jump_expr(lex, JUMP_BREAK);
        case TK_CONTINUE:
            return jump_expr(lex, JUMP_CONTINUE);
        default:
            return NULL;
    }
}

static struct AstExpr *block_expr(struct Lex *lex)
{
    switch (lex->t.kind) {
        case '{':
            return block(lex);
        case TK_IF:
            return if_expr(lex);
        case TK_LOOP:
            return loop_expr(lex);
        case TK_FOR:
            return for_expr(lex);
        case TK_WHILE:
            return while_expr(lex);
        case TK_MATCH:
            return match_expr(lex);
        default:
            return NULL;
    }
}

static struct AstExpr *suffixed_expr(struct Lex *lex)
{
    struct AstExpr *expr = primary_expr(lex);
    if (expr == NULL) return NULL;

    if (AstIsBlock(expr)
            || AstIsIfExpr(expr)
            || AstIsMatchExpr(expr)
            || AstIsLoopExpr(expr)
            || AstIsWhileExpr(expr)
            || AstIsForExpr(expr))
        return expr;

    if (test(lex, '{'))
        expr = try_composite_lit(lex, expr);

    for (;;) {
        switch (lex->t.kind) {
            case '?':
                expr = chain_expr(lex, expr);
                break;
            case '(':
                expr = call_expr(lex, expr);
                break;
            case '.':
                expr = selector_expr(lex, expr);
                break;
            case '[':
                expr = index_expr(lex, expr);
                break;
            default:
                return expr;
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
    struct SourceLoc const start = NODE_START(lhs);
    struct AstType *rhs = parse_type(lex);

    return NEW_NODE(lex, conversion_expr, RANGE(start, NODE_END(rhs)),
            next_id(lex), lhs, rhs);
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

static void check_assignment_target(struct Lex *lex, struct AstExpr *target)
{
    if (!AstIsPathExpr(target)
            && !AstIsIndex(target)
            && !AstIsSelector(target)
            && !AST_IS_UNOP(target, UNARY_DEREF))
        PARSE_ERROR(lex, InvalidAssignmentTarget, NODE_SPAN(target));

}

static struct AstExpr *op_assignment_expr(struct Lex *lex, struct AstExpr *lhs, enum InfixOp op)
{
    check_assignment_target(lex, lhs);
    struct SourceLoc const start = TOKEN_START(lex->t);
    struct AstExpr *rhs = expression(lex, right_prec(INFIX_ASSIGN));
    return NEW_NODE(lex, op_assign_expr, RANGE(start, NODE_END(rhs)),
            next_id(lex), lhs, rhs, into_binary_op(op));
}

static struct AstExpr *assignment_expr(struct Lex *lex, struct AstExpr *lhs)
{
    check_assignment_target(lex, lhs);
    struct SourceLoc const start = TOKEN_START(lex->t);
    struct AstExpr *rhs = expression(lex, right_prec(INFIX_ASSIGN));
    return NEW_NODE(lex, assign_expr, RANGE(start, NODE_END(rhs)),
            next_id(lex), lhs, rhs);
}

static struct AstExpr *range_expr(struct Lex *lex, struct SourceSpan op_span, enum InfixOp op, struct AstExpr *lhs)
{
    struct AstExpr *rhs = NULL;
    if (!test(lex, '{')) rhs = expression(lex, right_prec(op));
    struct SourceLoc const start = lhs != NULL ? NODE_START(lhs) : RANGE_END(op_span);
    struct SourceLoc const end = rhs != NULL ? NODE_START(rhs) : RANGE_END(op_span);
    return NEW_NODE(lex, range_expr, RANGE(start, end), next_id(lex),
            op == INFIX_RANGEI, lhs, rhs);
}

static struct AstExpr *binop_expr(struct Lex *lex, enum InfixOp op, struct AstExpr *lhs)
{
    struct SourceLoc const start = NODE_START(lhs);
    struct AstExpr *rhs = expect_expr(lex, right_prec(op));
    enum BinaryOp const binop = CAST(enum BinaryOp, op); // same order
    return NEW_NODE(lex, binop_expr, RANGE(start, NODE_END(rhs)),
            next_id(lex), binop, lhs, rhs);
}

static struct AstExpr *logical_expr(struct Lex *lex, struct AstExpr *lhs, paw_Bool is_and)
{
    struct SourceLoc const start = NODE_START(lhs);
    unsigned const prec = right_prec(is_and ? INFIX_AND : INFIX_OR);
    struct AstExpr *rhs = expect_expr(lex, prec);
    return NEW_NODE(lex, logical_expr, RANGE(start, NODE_END(rhs)),
            next_id(lex), lhs, rhs, is_and);
}

static struct AstExpr *infix_expr(struct Lex *lex, struct AstExpr *lhs, unsigned op)
{
    struct SourceSpan const op_span = lex->t.span;
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
            return range_expr(lex, op_span, op, lhs);
        default:
            return binop_expr(lex, op, lhs);
    }
}

typedef struct AstExpr *(ExprParser)(struct Lex *);
static struct AstExpr *parse_expr(struct Lex *lex, unsigned prec, ExprParser parser)
{
    unsigned op = get_unop(lex->t.kind);
    struct AstExpr *expr = op == NOT_UNOP
                               ? parser(lex)
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

static struct AstExpr *basic_expr(struct Lex *lex, unsigned prec)
{
    return parse_expr(lex, prec, simple_expr);
}

static struct AstExpr *compound_expr(struct Lex *lex, unsigned prec)
{
    return parse_expr(lex, prec, block_expr);
}

static struct AstExpr *expr_except_struct_lit(struct Lex *lex)
{
    int const prev_depth = lex->expr_depth;
    lex->expr_depth = -1;
    struct AstExpr *expr = expect_expr0(lex);
    lex->expr_depth = prev_depth;
    return expr;
}

static struct AstDeclList *type_param(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    struct AstDeclList *list = NULL;
    if (test_next(lex, '<')) {
        ++lex->expr_depth;
        list = AstDeclList_new(lex->ast);
        parse_generic_list(lex, list, start);
        --lex->expr_depth;

        if (list->count == 0)
            PARSE_ERROR(lex, EmptyTypeList,
                    .span = span_from(lex, start));
    }
    return list;
}

static struct AstExpr *function_body(struct Lex *lex)
{
    ++lex->fn_depth;
    struct AstExpr *body = block(lex);
    --lex->fn_depth;
    return body;
}

static struct AstDecl *function(struct Lex *lex, struct SourceLoc start, struct AstIdent ident, struct Annotations *annos, enum FnKind kind, paw_Bool is_pub)
{
    paw_Bool is_method;
    struct AstDeclList *generics = type_param(lex);
    struct AstDeclList *params = fn_parameters(lex, &is_method);
    ensure_unique_decl_names(lex, params, "function parameter");
    struct AstType *result = test_next(lex, TK_ARROW)
        ? parse_return_type(lex) : NULL;

    struct SourceLoc end;
    struct AstExpr *body = NULL;
    if (test(lex, ';')) {
        end = TOKEN_END(lex->t);
        skip(lex); // skip ";" token
    } else {
        body = function_body(lex);
        end = NODE_END(body);
    }

    return NEW_NODE(lex, fn_decl, RANGE(start, end), next_id(lex), kind, ident, annos,
            generics, params, result, body, is_pub, is_method);
}

static struct AstDecl *use_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "use" token

    enum AstUseKind kind = AST_USE_NORMAL;
    struct AstSegments *s = AstSegments_new(lex->ast);
    do {
        if (s->count == INT_MAX)
            break;
        if (test_next(lex, '*')) {
            kind = AST_USE_GLOB;
            break;
        }

        struct AstIdent const ident = parse_ident(lex);
        pawAst_add_segment(lex->ast, s, ident.span, next_id(lex), ident, NULL);
    } while (test_next(lex, TK_COLON2));

    if (s->count == 0 || test(lex, TK_COLON2)) {
        paw_assert(kind == AST_USE_GLOB);
        PARSE_ERROR(lex, InvalidGlobImport,
                .span = RANGE1(start));
    }
    struct SourceLoc const finish = RANGE_END(AstSegments_last(s).span);

    if (s->count > MAX_PATH_SEGMENTS)
        PARSE_ERROR(lex, PathTooLong,
                .max_segments = MAX_PATH_SEGMENTS,
                .span = RANGE(start, finish));

    struct AstPath path = {
        .span = RANGE(start, finish),
        .segments = s,
    };

    struct AstIdent as = {0};
    if (test_next(lex, TK_AS)) {
        // UseDecl containing a glob cannot also be an alias
        if (kind == AST_USE_GLOB)
            PARSE_ERROR(lex, InvalidGlobImport,
                    .span = span_from(lex, start));
        as = parse_ident(lex);
        kind = AST_USE_ALIAS;
    }

    struct SourceLoc const end = TOKEN_END(lex->t);
    semicolon(lex, "'use' declaration");
    return NEW_NODE(lex, use_decl, RANGE(start, end), next_id(lex),
            path, as, kind, is_pub);
}

DEFINE_MAP(struct Compiler, AnnotationMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, struct AstIdent)

static struct Annotations *annotations(struct Lex *lex)
{
    struct Compiler *C = lex->C;
    struct SourceLoc const start = TOKEN_START(lex->t);
    if (!test_next(lex, TK_HASH_BRACKET))
        return NULL;

    AnnotationMap *names = AnnotationMap_new_from(C, lex->pool);
    struct Annotations *annos = Annotations_new(C);
    do {
        if (test(lex, ']')) break;
        struct AstIdent const ident = parse_ident(lex);
        struct Annotation anno = {
            .modname = lex->modname,
            .name = ident.name,
            .span = ident.span,
        };
        struct AstIdent const *previous_ident = AnnotationMap_get(C, names, anno.name);
        if (previous_ident != NULL)
            PARSE_ERROR(lex, DuplicateAnnotation,
                    .previous = previous_ident->span,
                    .span = ident.span);
        AnnotationMap_insert(C, names, anno.name, ident);

        if (test_next(lex, '=')) {
            anno.has_value = PAW_TRUE;
            struct AstExpr *expr = expect_expr0(lex);

            if (AstIsStringExpr(expr)) {
                struct AstStringExpr *e = AstGetStringExpr(expr);
                if (e->parts->count != 1)
                    PARSE_ERROR(lex, NonprimitiveAnnotationValue,
                            .span = NODE_SPAN(expr),
                            .name = ident.name);

                struct AstStringPart const p = K_LIST_FIRST(e->parts);
                paw_assert(p.is_str);
                anno.value = p.str.value;
                anno.kind = BUILTIN_STR;
            } else if (AstIsLiteralExpr(expr)) {
                struct AstLiteralExpr *e = AstGetLiteralExpr(expr);
                if (e->lit_kind != kAstBasicLit)
                    PARSE_ERROR(lex, NonprimitiveAnnotationValue,
                            .span = NODE_SPAN(expr),
                            .name = anno.name);

                anno.value = e->basic.value;
                anno.kind = e->basic.code;
            } else {
                PARSE_ERROR(lex, NonliteralAnnotationValue,
                        .span = NODE_SPAN(expr),
                        .name = anno.name);
            }
        }
        Annotations_push(C, annos, anno);
    } while (test_next(lex, ','));
    delim_next(lex, ']', '[', start);
    AnnotationMap_delete(C, names);

    return annos;
}

static struct AstDecl *fn_decl(struct Lex *lex, struct Annotations *annos, paw_Bool is_pub)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "fn" token
    struct AstIdent const ident = parse_ident(lex);
    return function(lex, start, ident, annos, FUNC_FUNCTION, is_pub);
}

static struct AstDecl *parse_method(struct Lex *lex, struct Annotations *annos, paw_Bool is_pub)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
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
    struct AstIdent const ident = parse_ident(lex);
    struct SourceLoc end = RANGE_END(ident.span);

    struct AstDeclList *fields;
    if (test_next(lex, '(')) {
        fields = variant_field_list(lex, RANGE_START(ident.span));
        end = NODE_END(K_LIST_LAST(fields));
    } else {
        fields = AstDeclList_new(lex->ast);
    }

    return NEW_NODE(lex, variant_decl, RANGE(RANGE_START(ident.span), end),
            next_id(lex), ident, fields, index);
}

static struct SourceLoc enum_body(struct Lex *lex, struct SourceLoc start, struct AstDeclList *variants)
{
    check_next(lex, '{');
    while (!end_of_block(lex)) {
        if (variants->count == INT_MAX)
            break; // throw error below

        struct AstDecl *variant = variant_decl(lex, variants->count);
        AstDeclList_push(lex->ast, variants, variant);
        if (!test_next(lex, ',') && !test(lex, '}'))
            PARSE_ERROR(lex, ExpectedCommaSeparator, NODE_SPAN(variant));
    }
    if (variants->count > PAW_MAX_VARIANTS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "variants in enumeration"),
                .span = span_from(lex, start),
                .limit = PAW_MAX_VARIANTS);

    struct SourceLoc const rbrace = delim_next(lex, '}', '{', start);
    if (variants->count == 0)
        PARSE_ERROR(lex, EmptyEnumeration, RANGE(start, rbrace));

    ensure_unique_decl_names(lex, variants, "enum variants");
    return rbrace;
}

static struct AstDecl *enum_decl(struct Lex *lex, paw_Bool is_pub)
{
    skip(lex); // "enum" token
    struct AstIdent const ident = parse_ident(lex);
    struct AstDeclList *generics = type_param(lex);
    struct AstDeclList *variants = AstDeclList_new(lex->ast);
    struct SourceLoc const end = enum_body(lex, RANGE_START(ident.span), variants);

    return NEW_NODE(lex, adt_decl, RANGE(RANGE_START(ident.span), end), next_id(lex), ident,
            generics, variants, is_pub, PAW_FALSE);
}

static struct AstDecl *struct_field(struct Lex *lex, paw_Bool is_pub)
{
    struct AstIdent const ident = parse_ident(lex);
    struct AstType *tag = expect_type_annotation(lex, "field", ident);
    return NEW_NODE(lex, field_decl, RANGE(RANGE_START(ident.span), NODE_END(tag)),
            next_id(lex), ident, tag, is_pub);
}

static struct SourceLoc struct_body(struct Lex *lex, struct AstDeclList *fields)
{
    if (!test_next(lex, '{')) {
        struct SourceLoc const loc = TOKEN_START(lex->t);
        semicolon(lex, "body of unit struct");
        return loc;
    }

    struct SourceLoc const lbrace = TOKEN_START(lex->t);
    while (!end_of_block(lex)) {
        paw_Bool const is_pub = test_next(lex, TK_PUB);
        if (fields->count == INT_MAX)
            break; // throw error below

        struct AstDecl *field = struct_field(lex, is_pub);
        AstDeclList_push(lex->ast, fields, field);
        if (!test_next(lex, ',') && !test(lex, '}'))
            PARSE_ERROR(lex, ExpectedCommaSeparator, NODE_SPAN(field));
    }

    if (fields->count > PAW_MAX_FIELDS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "fields in structure"),
                .limit = PAW_MAX_FIELDS,
                .span = span_from(lex, lbrace));

    struct SourceLoc const rbrace = delim_next(lex, '}', '{', lbrace);
    struct SourceSpan const body_range = RANGE(lbrace, rbrace);

    if (fields->count == 0)
        PARSE_ERROR(lex, EmptyStructBody, body_range);

    ensure_unique_decl_names(lex, fields, "struct field");
    return rbrace;
}

static struct AstDecl *struct_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "struct" token
    struct AstIdent const ident = parse_ident(lex);
    struct AstDeclList *generics = type_param(lex);
    struct AstDeclList *fields = AstDeclList_new(lex->ast);
    struct SourceLoc const end = struct_body(lex, fields);

    struct AstDeclList *variants = AstDeclList_new(lex->ast);
    struct AstDecl *v = NEW_NODE(lex, variant_decl, RANGE(start, end),
            next_id(lex), ident, fields, 0);
    AstDeclList_push(lex->ast, variants, v);

    return NEW_NODE(lex, adt_decl, RANGE(start, end), next_id(lex), ident,
            generics, variants, is_pub, PAW_TRUE);
}

static struct AstDecl *const_decl(struct Lex *lex, struct Annotations *annos, paw_Bool is_pub)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "const" token
    struct AstIdent const ident = parse_ident(lex);
    struct AstType *tag = expect_type_annotation(lex, "constant", ident);
    struct AstExpr *init = test_next(lex, '=') ? expect_expr0(lex) : NULL;
    struct SourceLoc const end = TOKEN_START(lex->t);
    semicolon(lex, "constant declaration");

    return NEW_NODE(lex, const_decl, RANGE(start, end), next_id(lex),
            ident, annos, tag, init, is_pub);
}

static struct SourceLoc inherent_impl_body(struct Lex *lex, struct AstDeclList *constants, struct AstDeclList *methods)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    check_next(lex, '{');
    while (!end_of_block(lex)) {
        struct Annotations *annos = annotations(lex);
        paw_Bool const is_pub = test_next(lex, TK_PUB);

        if (constants->count == INT_MAX
                || methods->count == INT_MAX)
            break; // throw error below

        if (test(lex, TK_CONST)) {
            struct AstDecl *constant = const_decl(lex, NULL, is_pub);
            AstDeclList_push(lex->ast, constants, constant);
        } else {
            struct AstDecl *method = parse_method(lex, annos, is_pub);
            AstDeclList_push(lex->ast, methods, method);
        }
    }
    if (methods->count > MAX_ASSOC_ITEMS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "functions in inherent impl block"),
                .span = span_from(lex, start),
                .limit = MAX_ASSOC_ITEMS);

    if (constants->count > MAX_ASSOC_ITEMS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "constants in inherent impl block"),
                .span = span_from(lex, start),
                .limit = MAX_ASSOC_ITEMS);

    ensure_unique_decl_names(lex, constants, "associated constant");
    ensure_unique_decl_names(lex, methods, "associated function");
    return delim_next(lex, '}', '{', start);
}

static struct AstDecl *type_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "type" token

    struct AstIdent const ident = parse_ident(lex);
    struct AstDeclList *generics = type_param(lex);

    check_next(lex, '=');

    struct AstType *rhs = parse_type(lex);
    if (AstIsFnType(rhs))
        PARSE_ERROR(lex, FunctionTypeDecl, NODE_SPAN(rhs));

    struct SourceLoc const end = TOKEN_END(lex->t);
    semicolon(lex, "type declaration");
    return NEW_NODE(lex, type_decl, RANGE(start, end), next_id(lex),
            ident, generics, rhs, is_pub);
}

static struct SourceLoc trait_impl_body(struct Lex *lex, struct AstDeclList *types, struct AstDeclList *constants, struct AstDeclList *methods)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    check_next(lex, '{');
    while (!end_of_block(lex)) {
        struct Annotations *annos = annotations(lex);
        paw_Bool const is_pub = test_next(lex, TK_PUB);

        if (is_pub) // visibility of trait item is that of the trait
            PARSE_ERROR(lex, VisibilityQualifierNotAllowed,
                    .span = lex->t0.span);

        if (types->count == INT_MAX
                || constants->count == INT_MAX
                || methods->count == INT_MAX)
            break; // throw error below

        if (test(lex, TK_TYPE)) {
            struct AstDecl *type = type_decl(lex, is_pub);
            AstDeclList_push(lex->ast, types, type);
        } else if (test(lex, TK_CONST)) {
            struct AstDecl *constant = const_decl(lex, NULL, is_pub);
            AstDeclList_push(lex->ast, methods, constant);
        } else {
            struct AstDecl *method = parse_method(lex, annos, is_pub);
            AstDeclList_push(lex->ast, methods, method);
        }
    }
    // TODO: group all together and check against MAX_ASSOC_ITEMS, maybe keep in same list
    if (types->count > MAX_ASSOC_ITEMS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "types in trait impl block"),
                .span = span_from(lex, start),
                .limit = MAX_ASSOC_ITEMS);

    if (constants->count > MAX_ASSOC_ITEMS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "constants in trait impl block"),
                .span = span_from(lex, start),
                .limit = MAX_ASSOC_ITEMS);

    if (methods->count > MAX_ASSOC_ITEMS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "methods in trait impl block"),
                .span = span_from(lex, start),
                .limit = MAX_ASSOC_ITEMS);

    ensure_unique_decl_names(lex, types, "associated types");
    ensure_unique_decl_names(lex, constants, "associated constant");
    ensure_unique_decl_names(lex, methods, "associated function");
    return delim_next(lex, '}', '{', start);
}

static struct AstDecl *impl_decl(struct Lex *lex)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "impl" token
    struct AstDeclList *generics = type_param(lex);
    struct AstType *type = parse_type(lex);
    struct AstType *trait = NULL;
    if (test_next(lex, TK_FOR)) {
        trait = type; // found trait implementation
        type = parse_type(lex);
    }
    struct AstDeclList *types = AstDeclList_new(lex->ast);
    struct AstDeclList *constants = AstDeclList_new(lex->ast);
    struct AstDeclList *methods = AstDeclList_new(lex->ast);
    struct SourceLoc const end = trait != NULL
        ? trait_impl_body(lex, types, constants, methods)
        : inherent_impl_body(lex, constants, methods);

    return NEW_NODE(lex, impl_decl, RANGE(start, end), next_id(lex),
            type, trait, generics, types, constants, methods);
}

static struct AstDecl *trait_decl(struct Lex *lex, paw_Bool is_pub)
{
    struct SourceLoc const start = TOKEN_START(lex->t);
    skip(lex); // "trait" token
    struct AstIdent const ident = parse_ident(lex);
    AstDeclList *generics = type_param(lex);
    AstBoundList *supertraits = parse_generic_bounds(lex);

    check_next(lex, '{');
    AstDeclList *types = AstDeclList_new(lex->ast);
    AstDeclList *methods = AstDeclList_new(lex->ast);
    while (!end_of_block(lex)) {
        if (types->count == INT_MAX
                || methods->count == INT_MAX)
            break; // throw error below

        if (test_next(lex, TK_TYPE)) {
            struct AstDecl *type = generic_param(lex);
            semicolon(lex, "associated type declaration");
            AstDeclList_push(lex->ast, types, type);
        } else {
            // propagate visibility qualifier from trait to methods
            struct AstDecl *method = parse_method(lex, NULL, is_pub);
            AstDeclList_push(lex->ast, methods, method);

            // prevent default trait methods from being defined
            struct AstFnDecl *fn_decl = AstGetFnDecl(method);
            if (fn_decl->body != NULL)
                PARSE_ERROR(lex, Unsupported,
                        .span = fn_decl->body->hdr.span);
        }
    }

    if (methods->count > MAX_ASSOC_ITEMS)
        PARSE_ERROR(lex, LimitExceeded,
                .what = SCAN_STR(lex->C, "methods in trait"),
                .span = span_from(lex, start),
                .limit = MAX_ASSOC_ITEMS);

    struct SourceLoc const end = delim_next(lex, '}', '{', start);
    return NEW_NODE(lex, trait_decl, RANGE(start, end), next_id(lex),
            ident, generics, supertraits, types, methods, is_pub);
}

static struct AstExpr *expression(struct Lex *lex, unsigned prec)
{
    struct AstExpr *expr = basic_expr(lex, prec);
    return expr != NULL ? expr : compound_expr(lex, prec);
}

static AstStmtList *block_inner(struct Lex *lex, struct AstExpr **presult)
{
    struct AstStmtList *stmts = AstStmtList_new(lex->ast);
    while (!end_of_block(lex)) {
        struct SourceLoc const start = TOKEN_START(lex->t);
        switch (lex->t.kind) {
            case ';':
                skip(lex); // ";" token
                break;
            case TK_LET: {
                skip(lex); // "let" token
                struct AstPat *pat = pattern(lex);
                struct AstType *tag = type_annotation(lex);
                struct AstExpr *init = test_next(lex, '=') ? expect_expr0(lex) : NULL;
                struct SourceLoc const end = TOKEN_START(lex->t);
                semicolon(lex, "\"let\" statement");
                struct AstStmt *stmt = NEW_NODE(lex, let_stmt, RANGE(start, end),
                        next_id(lex), pat, tag, init);
                AstStmtList_push(lex->ast, stmts, stmt);
                break;
            }
            case TK_TYPE: {
                struct AstDecl *decl = type_decl(lex, PAW_FALSE);
                struct AstStmt *stmt = NEW_NODE(lex, decl_stmt, decl->hdr.span,
                        next_id(lex), decl);
                AstStmtList_push(lex->ast, stmts, stmt);
                break;
            }
            default: {
                struct AstExpr *expr = block_expr(lex);
                if (expr == NULL)
                    expr = basic_expr(lex, LOWEST_PRECEDENCE);
                if (expr == NULL)
                    PARSE_ERROR(lex, ExpectedExpression, span_from(lex, start));
                if (test(lex, '}')) {
                    *presult = expr;
                    return stmts;
                }
                if (expects_semicolon(expr)) semicolon(lex, "expression statement");
                struct AstStmt *stmt = NEW_NODE(lex, expr_stmt, expr->hdr.span,
                        next_id(lex), expr);
                AstStmtList_push(lex->ast, stmts, stmt);
            }
        }
    }

    *presult = NULL;
    return stmts;
}

static struct AstExpr *block(struct Lex *lex)
{
    struct SourceLoc const lbrace = TOKEN_START(lex->t);
    check_next(lex, '{');

    struct AstExpr *result;
    AstStmtList *stmts = block_inner(lex, &result);
    struct SourceLoc const rbrace = delim_next(lex, '}', '{', lbrace);
    return NEW_NODE(lex, block, RANGE(lbrace, rbrace), next_id(lex), stmts, result);
}


static struct AstDecl *toplevel_item(struct Lex *lex)
{
    struct Annotations *annos = annotations(lex);
    struct SourceLoc const start = TOKEN_START(lex->t);
    paw_Bool const is_pub = test_next(lex, TK_PUB);
    switch (lex->t.kind) {
        case TK_FN:
            return fn_decl(lex, annos, is_pub);
        case TK_CONST:
            return const_decl(lex, annos, is_pub);
        case TK_ENUM:
            return enum_decl(lex, is_pub);
        case TK_STRUCT:
            return struct_decl(lex, is_pub);
        case TK_TRAIT:
            return trait_decl(lex, is_pub);
        case TK_TYPE:
            return type_decl(lex, is_pub);
        case TK_USE:
            return use_decl(lex, is_pub);
        case TK_IMPL:
            if (is_pub)
                PARSE_ERROR(lex, VisibilityQualifierNotAllowed,
                        .span = lex->t0.span);
            return impl_decl(lex);
        default:
            PARSE_ERROR(lex, ExpectedToplevelItem,
                    .span = span_from(lex, start));
    }
}

static void toplevel_items(struct Lex *lex, struct AstDeclList *items)
{
    while (!test(lex, TK_END)) {
        struct AstDecl *item = toplevel_item(lex);
        AstDeclList_push(lex->ast, items, item);
    }
}

// Effectively add the following text at the top of each (non-prelude) source file:
//
//     use prelude;
//     use prelude::*;
//
// If the prelude file is being parsed, inject core type definitions.
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
    pawAst_add_segment(lex->ast, path.segments, ident.span, next_id(lex), ident, NULL);

    struct SourceSpan span = {0};
    struct AstIdent const none = {0}; // no alias
    AstDeclList_push(lex->ast, items, NEW_NODE(lex, use_decl, span,
                next_id(lex), path, none, AST_USE_NORMAL, PAW_FALSE));
    AstDeclList_push(lex->ast, items, NEW_NODE(lex, use_decl, span,
                next_id(lex), path, none, AST_USE_GLOB, PAW_FALSE));
}

static struct AstDecl *generate_builtin_decl(struct Lex *lex, char const *name)
{
    struct SourceSpan span = {0};
    struct AstIdent const ident = {
        .name = SCAN_STR(lex->C, name),
        .span = span,
    };

    AstDeclList *generics = AstDeclList_new(lex->ast);
    AstDeclList *fields = AstDeclList_new(lex->ast);
    AstDeclList *variants = AstDeclList_new(lex->ast);
    struct AstDecl *v = NEW_NODE(lex, variant_decl, span,
            next_id(lex), ident, fields, 0);
    AstDeclList_push(lex->ast, variants, v);

    return NEW_NODE(lex, adt_decl, span, next_id(lex), ident,
            generics, variants, PAW_TRUE, PAW_TRUE);
}

static void generate_builtin_items(struct Lex *lex, AstDeclList *items)
{
#define GENERATE_DECL(Name_) \
        AstDeclList_push(lex->ast, items, generate_builtin_decl(lex, Name_));

    GENERATE_DECL("unit");
    GENERATE_DECL("bool");
    GENERATE_DECL("char");
    GENERATE_DECL("int");
    GENERATE_DECL("float");
    GENERATE_DECL("str");

#undef GENERATE_DECL
}

static struct AstDecl *parse_module(struct Lex *lex, paw_Reader input, void *ud)
{
    pawX_set_source(lex, input, ud);

    struct AstDeclList *items = AstDeclList_new(lex->ast);
    struct SourceLoc const start = TOKEN_START(lex->t);

    import_prelude(lex, items);
    toplevel_items(lex, items);

    if (lex->modno == PRELUDE_MODNO)
        generate_builtin_items(lex, items);

    paw_assert(lex->ptr == lex->end);
    struct AstDecl *decl = NEW_NODE(lex, module_decl, span_from(lex, start),
            next_id(lex), lex->modname, lex->modno, items);
    AstDeclList_push(lex->ast, lex->ast->modules, decl);
    return decl;
}

static void init_lexer(struct Compiler *C, Str *modname, struct Lex *lex)
{
    *lex = (struct Lex){
        .pool = pawP_pool_new(C, C->aux_stats),
        .modno = C->modinfo->count - 1,
        .modname = modname,
        .ast = C->ast,
        .dm = C->dm,
        .P = C->P,
        .C = C,
    };
}

struct AstDecl *pawP_parse_module(struct Compiler *C, Str *modname, paw_Reader input, void *ud)
{
    struct Lex lex;
    init_lexer(C, modname, &lex);

    struct AstDecl *decl = parse_module(&lex, input, ud);
    pawP_pool_free(C, lex.pool);
    return decl;
}
