// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdlib.h>

#include "lex.h"
#include "compile.h"
#include "error.h"

#define SCRATCH(X_) ((X_)->dm->scratch)
#define SOURCE(X_) ((X_)->dm->source)

#define SAVE_AND_NEXT(X_) save(X_, next(X_))
#define IS_EOF(X_) ((paw_Uint8)*(X_)->ptr == TK_END)
#define IS_NEWLINE(X_) (*(X_)->ptr == '\r' || *(X_)->ptr == '\n')
#define IS_LINE_END(X_) (IS_NEWLINE(X_) || IS_EOF(X_))

#define RANGE(From_, To_) SourceSpan_from_range(From_, To_)
#define RANGE1(Loc_) RANGE(Loc_, Loc_)

#define LEXER_ERROR(X_, Kind_, ...) THROW_ERROR((X_)->C, \
        Kind_, .modname = (X_)->modname, __VA_ARGS__)

// Check for inclusion in one of the character classes
#define ISDIGIT(Char_) (kCharClassTable[(paw_Uint8)(Char_)] & 1)
#define ISHEX(Char_) (kCharClassTable[(paw_Uint8)(Char_)] & 2)
#define ISSPACE(Char_) (kCharClassTable[(paw_Uint8)(Char_)] & 4)
#define ISLETTER(Char_) (kCharClassTable[(paw_Uint8)(Char_)] & 8)
#define ISNONASCII(Char_) (kCharClassTable[(paw_Uint8)(Char_)] & 16)
#define ISASCIIEND(Char_) (kCharClassTable[(paw_Uint8)(Char_)] & 32)
#define ISNEWLINE(Char_) ((Char_) == '\r' || (Char_) == '\n')

// Get the integer representation of a hex digit
#define HEXVAL(Char_) (kHexValueTable[(paw_Uint8)(Char_)])

const paw_Uint8 kCharClassTable[256] = {
     32, 32, 32, 32, 32, 32, 32, 32, 32, 36, 36, 36, 36, 36, 32, 32,
     32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
      4,  0, 32,  0,  0,  0,  0, 32,  0,  0,  0,  0,  0,  0,  0,  0,
      3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  0,  0,  0,  0,  0,  0,
      0, 10, 10, 10, 10, 10, 10,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  0, 32,  0,  0,  8,
      0, 10, 10, 10, 10, 10, 10,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  0,  0,  0,  0,  0,
     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
     16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 48,
};

const paw_Uint8 kHexValueTable[256] = {
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0,
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0, 10, 11, 12, 13, 14, 15,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
};

static struct SourceSpan span_from(struct Lex *lex, struct SourceLoc start)
{
    return RANGE(start, lex->loc);
}

static void increment_line(struct Lex *X)
{
    paw_assert(ISNEWLINE(*X->ptr));

    if (X->loc.line == INT_MAX)
        LEXER_ERROR(X, TooManyLines,
                .filename = X->modname,
                .max_lines = INT_MAX);

    X->loc.column = 1;
    ++X->loc.line;
}

static void increment_column(struct Lex *X)
{
    if (X->loc.column == INT_MAX)
        LEXER_ERROR(X, TooManyColumns,
                .filename = X->modname,
                .max_columns = INT_MAX);

    ++X->loc.column;
}

static Str const *scan_str(struct Lex *X, char const *str)
{
    return SCAN_STR(X->C, str);
}

#define SCAN_FMT(X_, ...) pawP_format_string((X_)->C, __VA_ARGS__)

static void adjust_braces(struct Lex *X, int adjust)
{
    if (X->parens->count == 0) return;
    int const adjusted = K_LIST_LAST(X->parens) + adjust;
    // NOTE: might have unpaired parenthesis due to a syntax error, hence the use of "PAW_MAX"
    K_LIST_LAST(X->parens) = PAW_MAX(adjusted, 0);
}

static int peek_braces(struct Lex *X)
{
    if (X->parens->count > 0)
        return K_LIST_LAST(X->parens);
    return -1;
}

static void pop_braces(struct Lex *X)
{
    paw_assert(X->parens->count > 0);
    IntStack_pop(X->parens);
}

static void push_braces(struct Lex *X)
{
    // count the opening '\{'
    IntStack_push(X, X->parens, 1);
}

static enum StringState peek_state(struct Lex *X)
{
    paw_assert(X->states->count > 0);
    return K_LIST_LAST(X->states);
}

static char pop_state(struct Lex *X)
{
    paw_assert(X->states->count > 0);
    char const state = peek_state(X);
    StateStack_pop(X->states);
    return state;
}

static void push_state(struct Lex *X, enum StringState state)
{
    StateStack_push(X, X->states, state);
}

static void save(struct Lex *X, char c)
{
    struct StringBuffer *b = &SCRATCH(X);
    pawM_grow(ENV(X), b->data, b->count, b->alloc);
    b->data[b->count++] = c;
}

static char next(struct Lex *X)
{
    if (IS_EOF(X)) return TK_END;
    if (ISNEWLINE(*X->ptr)) {
        increment_line(X);
    } else {
        increment_column(X);
    }
    return *X->ptr++;
}

static paw_Bool test(struct Lex *X, char c)
{
    return *X->ptr == c;
}

static paw_Bool test_next(struct Lex *X, char c)
{
    return test(X, c) ? (next(X), PAW_TRUE) : PAW_FALSE;
}

static paw_Bool test2(struct Lex *X, char const *c2)
{
    return *X->ptr == c2[0] || *X->ptr == c2[1];
}

static paw_Bool test_next2(struct Lex *X, char const *c2)
{
    return test2(X, c2) ? (next(X), PAW_TRUE) : PAW_FALSE;
}

static struct Token make_token(TokenKind kind, struct SourceLoc start, struct SourceLoc end)
{
    return (struct Token){
        .span = SourceSpan_from_range(start, end),
        .kind = kind,
    };
}

static struct Token make_string(struct Lex *X, struct SourceLoc start, TokenKind kind)
{
    struct StringBuffer *b = &SCRATCH(X);
    struct Token t = (struct Token){
        .span = span_from(X, start),
        .kind = kind,
    };
    Str *str = pawP_scan_nstr(X->C, b->data, CAST_SIZE(b->count));
    V_SET_OBJECT(&t.value, str);
    return t;
}

static struct Token consume_name(struct Lex *X, struct SourceLoc start)
{
    while (ISLETTER(*X->ptr) || ISDIGIT(*X->ptr))
        SAVE_AND_NEXT(X);

    struct Token t = make_string(X, start, TK_NAME);
    Str const *s = V_STR(t.value);
    if (IS_KEYWORD(s)) {
        t.kind = CAST(TokenKind, s->flag);
    } else if (s->length > PAW_NAME_MAX) {
        LEXER_ERROR(X, NameTooLong,
                .max_chars = PAW_NAME_MAX,
                .span = t.span);
    }
    return t;
}

static unsigned get_codepoint(struct Lex *X, struct SourceLoc slash)
{
    if (!test_next(X, '{'))
        LEXER_ERROR(X, InvalidUnicodeEscape,
                .reason = SCAN_FMT(X, "expected start of unicode codepoint payload '{'"),
                .span = RANGE(slash, X->loc));

    if (test(X, '}'))
        LEXER_ERROR(X, EmptyUnicodeEscape,
                .span = RANGE(slash, X->loc));

    enum {MAX_DIGITS = 6};
    char digits[MAX_DIGITS];
    int n = 0;

    do {
        if (IS_LINE_END(X))
            // the whole literal must be unterminated so just return with
            // an arbitrary codepoint and let the caller to report the
            // error
            return 0;

        if (*X->ptr == X->last_quote)
            LEXER_ERROR(X, InvalidUnicodeEscape,
                    .reason = SCAN_FMT(X, "missing end of codepoint payload '}'"),
                    .span = RANGE(slash, X->loc));

        if (!ISHEX(*X->ptr))
            LEXER_ERROR(X, InvalidCharInUnicodeEscape,
                    .span = RANGE1(X->loc));

        if (n == MAX_DIGITS)
            LEXER_ERROR(X, UnicodeEscapeTooLong,
                    .span = RANGE(slash, X->loc));

        digits[n++] = next(X);
    } while (!test_next(X, '}'));

    unsigned codepoint = 0;
    for (int i = 0; i < n; ++i)
        codepoint = (codepoint << 4) | HEXVAL(digits[i]);

    return codepoint;
}

static int get_hexcode(struct Lex *X)
{
    enum {MAX_DIGITS = 2};
    char digits[MAX_DIGITS];
    int n = 0;

    do {
        if (IS_LINE_END(X))
            // see comment in get_codepoint()
            return 0;

        if (*X->ptr == X->last_quote)
            break;

        if (!ISHEX(*X->ptr))
            LEXER_ERROR(X, InvalidCharInHexEscape,
                    .span = RANGE1(X->loc));

        digits[n++] = next(X);
    } while (n < MAX_DIGITS);

    if (n != MAX_DIGITS)
        LEXER_ERROR(X, HexEscapeTooShort,
                .span = RANGE1(X->loc));

    return (HEXVAL(digits[0]) << 4) | HEXVAL(digits[1]);
}

static void hex_escape(struct Lex *X)
{
    int const value = get_hexcode(X);
    save(X, (char)value);
}

static void unicode_escape(struct Lex *X, struct SourceLoc slash)
{
    unsigned const codepoint = get_codepoint(X, slash);
    if ((0xD800 <= codepoint && codepoint <= 0xDFFF)
            || (0xFDD0 <= codepoint && codepoint <= 0xFDEF)
            || (codepoint & 0xFFFE) == 0xFFFE
            || codepoint > 0x10FFFF)
        LEXER_ERROR(X, InvalidUnicodeCodepoint,
                .span = RANGE(slash, X->loc),
                .codepoint = codepoint);

    // Translate a UTF-32 codepoint into bytes. Modified from @Tencent/rapidjson.
    if (codepoint <= 0x7F) {
        save(X, (char)codepoint);
    } else if (codepoint <= 0x7FF) {
        save(X, (char)(0xC0 | ((codepoint >> 6) & 0xFF)));
        save(X, (char)(0x80 | ((codepoint & 0x3F))));
    } else if (codepoint <= 0xFFFF) {
        save(X, (char)(0xE0 | ((codepoint >> 12) & 0xFF)));
        save(X, (char)(0x80 | ((codepoint >> 6) & 0x3F)));
        save(X, (char)(0x80 | (codepoint & 0x3F)));
    } else {
        assert(codepoint <= 0x10FFFF);
        save(X, (char)(0xF0 | ((codepoint >> 18) & 0xFF)));
        save(X, (char)(0x80 | ((codepoint >> 12) & 0x3F)));
        save(X, (char)(0x80 | ((codepoint >> 6) & 0x3F)));
        save(X, (char)(0x80 | (codepoint & 0x3F)));
    }
}

static void escape_character(struct Lex *X, struct SourceLoc slash)
{
    char const c = next(X);
    switch (c) {
        case '0':
            save(X, '\0');
            break;
        case '"':
            save(X, '"');
            break;
        case '\\':
            save(X, '\\');
            break;
        case '/':
            save(X, '/');
            break;
        case 'b':
            save(X, '\b');
            break;
        case 'v':
            save(X, '\v');
            break;
        case 'f':
            save(X, '\f');
            break;
        case 'n':
            save(X, '\n');
            break;
        case 'r':
            save(X, '\r');
            break;
        case 't':
            save(X, '\t');
            break;
        case 'x':
            hex_escape(X);
            break;
        case 'u':
            unicode_escape(X, slash);
            break;
        default:
            LEXER_ERROR(X, UnknownEscapeChar,
                        .span = RANGE1(X->loc));
    }
}

static struct Token single_byte(struct Lex *X, struct SourceLoc quote)
{
    X->last_quote = '\'';
    for (;;) {
        struct SourceLoc const current = X->loc;
        if (IS_LINE_END(X)) {
            LEXER_ERROR(X, UnterminatedCharLiteral,
                        .span = RANGE(quote, current));
        } else if (test_next(X, '\\')) {
            escape_character(X, current);
        } else if (test(X, '\'')) {
            break;
        } else {
            SAVE_AND_NEXT(X);
        }
    }

    struct SourceLoc const quote2 = X->loc;
    next(X); // skip second quote

    struct StringBuffer const *b = &SCRATCH(X);
    if (b->count == 0)
        LEXER_ERROR(X, EmptyCharLiteral,
                .span = RANGE(quote, quote2));
    if (b->count > 1)
        LEXER_ERROR(X, CharLiteralTooLong,
                .span = RANGE(quote, quote2));

    return (struct Token){
        // make sure the bytes used by larger scalars get cleared
        .value.u = (paw_Uint)(paw_Uint8)b->data[0],
        .span = RANGE(quote, quote2),
        .kind = TK_CHAR,
    };
}

static struct Token consume_byte(struct Lex *X, struct SourceLoc quote)
{
    return single_byte(X, quote);
}

static struct Token consume_string_part(struct Lex *X, struct SourceLoc start_loc)
{
    paw_assert(peek_state(X) != STATE_NORMAL);
    X->last_quote = '"';

    for (;;) {
    handle_ascii:
        if (ISASCIIEND(*X->ptr))
            break;
        SAVE_AND_NEXT(X);
    }

    struct SourceLoc current_loc = X->loc;
    if (test_next(X, '\\')) {
        if (test_next(X, '{')) {
            // found start of string expression
            struct Token token = make_string(X, start_loc, TK_STRING_EXPR_OPEN);
            push_braces(X);
            push_state(X, STATE_NORMAL);
            return token;
        }
        escape_character(X, current_loc);
    } else if (test_next(X, '"')) {
        pop_state(X);
        return make_string(X, start_loc, TK_STRING_TEXT);
    } else if (IS_LINE_END(X)) {
        LEXER_ERROR(X, UnterminatedStrLiteral,
                .span = RANGE(start_loc, X->loc));
    } else {
        SAVE_AND_NEXT(X);
    }
    goto handle_ascii;
}

static struct Token consume_str(struct Lex *X, struct SourceLoc start_loc)
{
    X->last_quote = '"';
    for (;;) {
    handle_ascii:
        if (ISASCIIEND(*X->ptr))
            break;
        SAVE_AND_NEXT(X);
    }

    struct SourceLoc current_loc = X->loc;
    if (test_next(X, '\\')) {
        escape_character(X, current_loc);
    } else if (test_next(X, '"')) {
        return make_string(X, start_loc, TK_STR);
    } else if (IS_LINE_END(X)) {
        LEXER_ERROR(X, UnterminatedStrLiteral,
                .span = RANGE(start_loc, X->loc));
    } else {
        SAVE_AND_NEXT(X);
    }
    goto handle_ascii;
}

static struct Token consume_int_aux(struct Lex *X, struct SourceLoc start, int base)
{
    struct StringBuffer const *b = &SCRATCH(X);
    paw_Uint u;

    int const rc = pawX_parse_uint(b->data, base, &u);
    if (rc == PAW_EOVERFLOW) {
        LEXER_ERROR(X, IntegerTooBigToParse,
                .span = RANGE(start, X->loc),
                .base = base);
    } else if (rc == PAW_ESYNTAX) {
        LEXER_ERROR(X, InvalidIntegerLiteral,
                .span = RANGE(start, X->loc),
                .base = base);
    }
    return (struct Token){
        .span = span_from(X, start),
        .kind = TK_INT,
        .value.u = u,
    };
}

static struct Token consume_bin_int(struct Lex *X, struct SourceLoc start)
{
    if (!test2(X, "01"))
        LEXER_ERROR(X, ExpectedIntegerDigit,
                .span = RANGE(start, X->loc),
                .base = 2);

    while (ISLETTER(*X->ptr) || ISDIGIT(*X->ptr)) {
        if (test_next(X, '_')) {
            // ignore digit separators
        } else if (!test2(X, "01")) {
            LEXER_ERROR(X, InvalidCharInInteger,
                    .span = RANGE(start, X->loc),
                    .base = 2);
        } else {
            SAVE_AND_NEXT(X);
        }
    }

    save(X, '\0');
    return consume_int_aux(X, start, 2);
}

static struct Token consume_oct_int(struct Lex *X, struct SourceLoc start)
{
    if (*X->ptr < '0' || *X->ptr > '7')
        LEXER_ERROR(X, ExpectedIntegerDigit,
                .span = RANGE(start, X->loc),
                .base = 8);

    while (ISLETTER(*X->ptr) || ISDIGIT(*X->ptr)) {
        if (test_next(X, '_')) {
            // ignore digit separators
        } else if (*X->ptr < '0' || *X->ptr > '7') {
            LEXER_ERROR(X, InvalidCharInInteger,
                    .span = RANGE(start, X->loc),
                    .base = 8);
        } else {
            SAVE_AND_NEXT(X);
        }
    }

    save(X, '\0');
    return consume_int_aux(X, start, 8);
}

static struct Token consume_hex_int(struct Lex *X, struct SourceLoc start)
{
    if (!ISHEX(*X->ptr))
        LEXER_ERROR(X, ExpectedIntegerDigit,
                .span = RANGE(start, X->loc),
                .base = 16);

    while (ISLETTER(*X->ptr) || ISDIGIT(*X->ptr)) {
        if (test_next(X, '_')) {
            // ignore digit separators
        } else if (!ISHEX(*X->ptr)) {
            LEXER_ERROR(X, InvalidCharInInteger,
                    .span = RANGE(start, X->loc),
                    .base = 16);
        } else {
            SAVE_AND_NEXT(X);
        }
    }

    save(X, '\0');
    return consume_int_aux(X, start, 16);
}

static void save_parsed_digits(struct Lex *X, const char *begin)
{
    for (; begin < X->ptr; ++begin) {
        if (*begin != '_')
            save(X, *begin);
    }
    save(X, '\0');
}

static struct Token consume_decimal_int(struct Lex *X, struct SourceLoc start, const char *begin)
{
    save_parsed_digits(X, begin);
    return consume_int_aux(X, start, 10);
}

static struct Token consume_float(struct Lex *X, struct SourceLoc start, const char *begin)
{
    save_parsed_digits(X, begin);
    struct StringBuffer b = SCRATCH(X);
    paw_Float f;

    int const rc = pawX_parse_float(b.data, &f);
    if (rc != PAW_OK)
        LEXER_ERROR(X, InvalidFloatLiteral,
                .span = RANGE(start, X->loc));
    return (struct Token){
        .span = span_from(X, start),
        .kind = TK_FLOAT,
        .value.f = f,
    };
}

static struct Token consume_number(struct Lex *X, struct SourceLoc start)
{
    const char *begin = X->ptr;
    paw_assert(ISDIGIT(*begin));
    next(X);

    if (*begin == '0' && test_next2(X, "bB"))
        return consume_bin_int(X, start);
    if (*begin == '0' && test_next2(X, "oO"))
        return consume_oct_int(X, start);
    if (*begin == '0' && test_next2(X, "xX"))
        return consume_hex_int(X, start);

    while (ISDIGIT(*X->ptr) || test(X, '_'))
        next(X);

    if (test(X, '.')) {
        // Allow accessing fields directly on an int or float (without parenthesis),
        // as well as chained tuple selectors. "X->ptr[1]" works because the source
        // buffer ends with a '\0'.
        if (!ISDIGIT(X->ptr[1]) || X->t.kind == '.')
            return consume_decimal_int(X, start, begin);

        next(X); // skip '.'
        while (ISDIGIT(*X->ptr) || test(X, '_'))
            next(X);
    } else if (!test2(X, "eE")) {
        return consume_decimal_int(X, start, begin);
    }

    if (test_next2(X, "eE")) {
        test_next2(X, "+-");
        if (!ISDIGIT(*X->ptr))
            LEXER_ERROR(X, InvalidFloatLiteral,
                    .span = RANGE(start, X->loc));
    }

    while (ISDIGIT(*X->ptr) || test(X, '_'))
        next(X);
    return consume_float(X, start, begin);
}

static void skip_line_comment(struct Lex *X)
{
    while (!IS_EOF(X) && !ISNEWLINE(*X->ptr))
        next(X);
}

static void skip_whitespace(struct Lex *X)
{
    while (*X->ptr == ' '
           || *X->ptr == '\t'
           || *X->ptr == '\f'
           || *X->ptr == '\v'
           || IS_NEWLINE(X)) {
        next(X);
    }
}

static struct Token advance(struct Lex *X)
{
#define T(kind) make_token((TokenKind)kind, start, X->loc)
try_again:
    if (peek_state(X) != STATE_NORMAL)
        return consume_string_part(X, X->loc);

    skip_whitespace(X);
    struct SourceLoc start = X->loc;

    // cast to avoid sign extension
    struct Token token = T((unsigned char)*X->ptr);
    SCRATCH(X).count = 0;
    switch (token.kind) {
        case '\'':
            next(X);
            token = consume_byte(X, start);
            break;
        case '\"':
            next(X);
            token = consume_str(X, start);
            break;
        case '{':
            next(X);
            adjust_braces(X, 1);
            break;
        case '}':
            next(X);
            adjust_braces(X, -1);
            if (peek_braces(X) == 0) {
                // handle string state transition
                pop_state(X);
                pop_braces(X);
                return T(TK_STRING_EXPR_CLOSE);
            }
            break;
        case '=':
            next(X);
            if (test_next(X, '=')) {
                token = T(TK_EQUALS2);
            } else if (test_next(X, '>')) {
                token = T(TK_FAT_ARROW);
            }
            break;
        case '&':
            next(X);
            if (test_next(X, '&')) {
                token = T(TK_AMPER2);
            } else if (test_next(X, '=')) {
                token = T(TK_AMPER_EQ);
            }
            break;
        case '|':
            next(X);
            if (test_next(X, '|')) {
                token = T(TK_PIPE2);
            } else if (test_next(X, '=')) {
                token = T(TK_PIPE_EQ);
            }
            break;
        case '^':
            next(X);
            if (test_next(X, '=')) {
                token = T(TK_CARET_EQ);
            }
            break;
        case '-':
            next(X);
            if (test_next(X, '>')) {
                token = T(TK_ARROW);
            } else if (test_next(X, '=')) {
                token = T(TK_MINUS_EQ);
            }
            break;
        case ':':
            next(X);
            if (test_next(X, ':')) {
                token = T(TK_COLON2);
            }
            break;
        case '!':
            next(X);
            if (test_next(X, '=')) {
                token = T(TK_BANG_EQ);
            }
            break;
        case '<':
            next(X);
            if (test_next(X, '<')) {
                token = test_next(X, '=')
                    ? T(TK_LESS2_EQ)
                    : T(TK_LESS2);
            } else if (test_next(X, '=')) {
                token = T(TK_LESS_EQ);
            }
            break;
        case '>':
            next(X);
            if (test_next(X, '>')) {
                token = test_next(X, '=')
                    ? T(TK_GREATER2_EQ)
                    : T(TK_GREATER2);
            } else if (test_next(X, '=')) {
                token = T(TK_GREATER_EQ);
            }
            break;
        case '+':
            next(X);
            if (test_next(X, '+')) {
                token = test_next(X, '=')
                    ? T(TK_PLUS2_EQ)
                    : T(TK_PLUS2);
            } else if (test_next(X, '=')) {
                token = T(TK_PLUS_EQ);
            }
            break;
        case '.':
            next(X);
            if (test_next(X, '.')) {
                if (test_next(X, '.')) {
                    token = T(TK_DOT3);
                } else if (test_next(X, '=')) {
                    token = T(TK_DOT2_EQ);
                } else {
                    token = T(TK_DOT2);
                }
            }
            break;
        case '*':
            next(X);
            if (test_next(X, '=')) {
                token = T(TK_STAR_EQ);
            }
            break;
        case '%':
            next(X);
            if (test_next(X, '=')) {
                token = T(TK_PERCENT_EQ);
            }
            break;
        case '/':
            next(X);
            if (test_next(X, '/')) {
                skip_line_comment(X);
                goto try_again;
            } else if (test_next(X, '=')) {
                token = T(TK_SLASH_EQ);
            }
            break;
        case '#':
            next(X);
            if (test_next(X, '!')) {
                token = T(TK_HASHBANG);
            } else if (test_next(X, '[')) {
                token = T(TK_HASH_BRACKET);
            }
            break;
        case 'f':
            next(X);
            if (test_next(X, '\"')) {
                push_state(X, STATE_STRING);
                token = consume_string_part(X, start);
            } else {
                save(X, 'f');
                token = consume_name(X, start);
            }
            break;
        default:
            if (ISDIGIT(*X->ptr)) {
                token = consume_number(X, start);
            } else if (ISLETTER(*X->ptr)) {
                SAVE_AND_NEXT(X);
                token = consume_name(X, start);
            } else {
                next(X);
            }
    }
    return token;
#undef T
}

// Read the source code into a buffer
static void read_source(struct Lex *X)
{
    paw_Env *P = ENV(X);
    struct SourceBuffer *b = &SOURCE(X);
    size_t next, size = 0;

    for (;;) {
        const char *chunk = X->input(P, X->ud, &next);
        if (next == 0)
            break;

        if (next > b->size - size) {
            size_t alloc = PAW_MAX(b->size, 1);
            while (alloc < size + next)
                alloc *= 2;
            pawM_resize(P, b->data, b->size, alloc);
            b->size = alloc;
        }

        memcpy(b->data + size, chunk, next);
        size += next;
    }

    // shrink buffer to fit (or add space for '\0')
    pawM_resize(P, b->data, b->size, size + 1);
    b->size = size + 1;

    b->data[size] = '\0';
    X->ptr = b->data;
    X->end = b->data + size;

    pawSrc_init_location(&X->loc);
    X->last_loc = X->loc;

    X->t2.kind = TK_NONE;
}

TokenKind pawX_next(struct Lex *X)
{
    X->last_loc = X->loc;
    TokenKind const kind = pawX_peek(X);
    X->t0 = X->t;
    X->t = X->t2;
    X->t2.kind = TK_NONE;
    return kind;
}

TokenKind pawX_peek(struct Lex *X)
{
    if (X->t2.kind == TK_NONE)
        X->t2 = advance(X);
    return X->t2.kind;
}

#define INITIAL_SCRATCH 128

void pawX_set_source(struct Lex *X, paw_Reader input, void *ud)
{
    paw_Env *P = ENV(X);
    struct StringBuffer *b = &SCRATCH(X);
    if (b->alloc < INITIAL_SCRATCH) {
        pawM_resize(P, b->data, b->alloc, INITIAL_SCRATCH);
        b->alloc = INITIAL_SCRATCH;
    }
    b->count = 0;

    X->ud = ud;
    X->input = input;

    X->states = StateStack_new(X);
    X->parens = IntStack_new(X);

    read_source(X);
    push_state(X, STATE_NORMAL);
    pawX_next(X); // load first token
}

static int char2base(char c)
{
    if (c == 'b' || c == 'B') {
        return 2;
    } else if (c == 'o' || c == 'O') {
        return 8;
    } else if (c == 'x' || c == 'X') {
        return 16;
    } else {
        return -1;
    }
}

#define IS_FP(c) (c == 'e' || c == 'E' || c == '.')

static unsigned char_to_digit(char c)
{
    static const unsigned char LOOKUP[0x100] = {
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
        0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
        0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    };

    return LOOKUP[(unsigned)c];
}

int pawX_parse_uint(char const *text, int base, paw_Uint *out)
{
    paw_Uint const b = (paw_Uint)base;
    char const *p = text;

    if (b < 2 || b > 36)
        return PAW_EVALUE;

    paw_Uint value = 0;
    for (; *p; ++p) {
        paw_Uint const v = char_to_digit(*p);
        if (v >= b) {
            return PAW_ESYNTAX;
        } else if (value > (PAW_UINT_MAX - v) / b) {
            return PAW_EOVERFLOW;
        }
        value = value * b + v;
    }
    *out = value;
    return PAW_OK;
}

static paw_Bool parse_negative(char const **ptext)
{
    if (**ptext == '-') {
        ++*ptext;
        return PAW_TRUE;
    }
    *ptext += **ptext == '+';
    return PAW_FALSE;
}

int pawX_parse_int(char const *text, int base, paw_Int *out)
{
    paw_Bool const negative = parse_negative(&text);

    paw_Uint u;
    int const status = pawX_parse_uint(text, base, &u);
    if (status != PAW_OK) return status;

    if (u > (paw_Uint)PAW_INT_MAX + negative)
        return PAW_EOVERFLOW;

    *out = PAW_CAST_INT(negative ? -u : u);
    return PAW_OK;
}

#define IS_DIGIT(Char_) (char_to_digit((Char_)) < 10)

int pawX_parse_float(char const *text, paw_Float *out)
{
    paw_Bool const negative = parse_negative(&text);

    // First, validate the number format.
    char const *p = text;
    if (p[0] == '0' && p[1] != '\0' && !IS_FP(p[1]))
        return PAW_ESYNTAX;

    while (IS_DIGIT(*p)) ++p;

    if (*p == '.') {
        ++p;
        while (IS_DIGIT(*p)) ++p;
    }
    if (*p == 'e' || *p == 'E') {
        p += 1 + (p[1] == '+' || p[1] == '-');
        if (!IS_DIGIT(*p)) return PAW_ESYNTAX;
        while (IS_DIGIT(*p)) ++p;
    }
    if (*p != '\0') return PAW_ESYNTAX;
    paw_Float const f = strtod(text, NULL);
    *out = negative ? -f : f;
    return PAW_OK;
}
