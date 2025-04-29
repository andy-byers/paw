// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "lex.h"
#include "compile.h"
#include "error.h"

#define SAVE_AND_NEXT(X_) save(X_, next(X_))
#define IS_EOF(X_) (CAST(uint8_t, *(X_)->ptr) == TK_END)
#define IS_NEWLINE(X_) (*(X_)->ptr == '\r' || *(X_)->ptr == '\n')

#define LEX_ERROR(X_, Kind_, ...) pawErr_##Kind_((X_)->C, (X_)->modname, __VA_ARGS__)

static struct SourceSpan span_from(struct Lex *lex, struct SourceLoc start)
{
    return (struct SourceSpan){
        .start = start,
        .end = lex->loc,
    };
}

static void increment_line(struct Lex *X)
{
    paw_assert(ISNEWLINE(*X->ptr));
    if (X->loc.line == INT_MAX)
        LEX_ERROR(X, too_many_lines, X->loc, INT_MAX);

    X->loc.column = 1;
    ++X->loc.line;
}

static void increment_column(struct Lex *X)
{
    if (X->loc.column == INT_MAX)
        LEX_ERROR(X, too_many_columns, X->loc, INT_MAX);

    ++X->loc.column;
}

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

static char peek_state(struct Lex *X)
{
    paw_assert(X->states->count > 0);
    return K_LIST_LAST(X->states);
}

static char pop_state(struct Lex *X)
{
    paw_assert(X->states->count > 0);
    char const state = peek_state(X);
    CharStack_pop(X->states);
    return state;
}

static void push_normal_state(struct Lex *X)
{
    CharStack_push(X, X->states, '\0');
}

static void push_string_state(struct Lex *X, char quote)
{
    paw_assert(quote == '\'' || quote == '"');
    CharStack_push(X, X->states, quote);
}

static void save(struct Lex *X, char c)
{
    struct DynamicMem *dm = X->dm;
    pawM_grow(ENV(X), dm->scratch.data, dm->scratch.count, dm->scratch.alloc);
    dm->scratch.data[dm->scratch.count++] = c;
}

static char next(struct Lex *X)
{
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
        .span = {
            .start = start,
            .end = end,
        },
        .kind = kind,
    };
}

static struct Token make_int(struct Lex *X)
{
    paw_Env *P = ENV(X);
    Value const v = P->top.p[-1];
    return (struct Token){
        .kind = TK_INTEGER,
        .value = v,
    };
}

static struct Token make_float(struct Lex *X)
{
    paw_Env *P = ENV(X);
    Value const v = P->top.p[-1];
    return (struct Token){
        .kind = TK_FLOAT,
        .value = v,
    };
}

static struct Token make_string(struct Lex *X, struct SourceLoc start, TokenKind kind)
{
    struct DynamicMem *dm = X->dm;
    struct StringBuffer *b = &dm->scratch;
    struct Token t = make_token(kind, start, X->loc);
    String *s = pawP_scan_nstring(X->C, X->strings, b->data, CAST_SIZE(b->count));
    V_SET_OBJECT(&t.value, s);
    b->count = 0;
    return t;
}

static struct Token consume_name(struct Lex *X, struct SourceLoc start)
{
    SAVE_AND_NEXT(X);
    while (ISNAME(*X->ptr) || ISDIGIT(*X->ptr))
        SAVE_AND_NEXT(X);

    struct Token t = make_string(X, start, TK_NAME);
    String const *s = V_STRING(t.value);
    if (IS_KEYWORD(s)) {
        t.kind = CAST(TokenKind, s->flag);
    } else if (s->length > PAW_NAME_MAX) {
        LEX_ERROR(X, name_too_long, start, s->length, PAW_NAME_MAX);
    }
    return t;
}

// Tables are from https://arxiv.org/pdf/2010.03090.pdf
static void consume_utf8(struct Lex *X)
{
    // clang-format off
    static const uint8_t kLookup1[256] = {
         8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
         8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
         9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
        10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
        11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
        11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
         8,  8,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
         1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
         4,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  5,  2,  2,
         6,  3,  3,  3,  7,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
    };
    static const uint8_t kLookup2[108] = {
        0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 0, 0, 0,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 1, 1, 1,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 2, 2, 2,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 1,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 1, 1, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 2, 2,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 2, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    };
    // clang-format on

    unsigned state = 0;
    do {
        unsigned char const c = (unsigned char)*X->ptr;
        state = kLookup1[c] + state * 12;
        state = kLookup2[state];
        SAVE_AND_NEXT(X);
    } while (state % 8 != 0);

     // TODO: keep track of codepoint for error message
    if (state != 0)
        LEX_ERROR(X, invalid_unicode_codepoint, X->loc, -1);
}

static int get_codepoint(struct Lex *X)
{
    if (X->end - X->ptr < 4)
        return -1;

    const char c[5] = {
        next(X),
        next(X),
        next(X),
        next(X),
    };

    if (!ISHEX(c[0])
            || !ISHEX(c[1])
            || !ISHEX(c[2])
            || !ISHEX(c[3]))
        LEX_ERROR(X, invalid_unicode_escape, X->loc, c);

    return HEXVAL(c[0]) << 12
           | HEXVAL(c[1]) << 8
           | HEXVAL(c[2]) << 4
           | HEXVAL(c[3]);
}

static struct Token consume_string_part(struct Lex *X, struct SourceLoc start, char quote)
{
    for (;;) {
    handle_ascii:
        if (ISASCIIEND(*X->ptr))
            break;
        SAVE_AND_NEXT(X);
    }

    if (test_next(X, '\\')) {
        char const c = next(X);
        switch (c) {
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
            case '{': {
                // found start of string expression
                struct Token token = make_string(X, start, TK_STRING_EXPR_OPEN);
                push_braces(X);
                push_normal_state(X);
                return token;
            }
            case 'u': {
                struct SourceLoc saved = X->loc;
                unsigned const codepoint = get_codepoint(X);
                if ((0xD800 <= codepoint && codepoint < 0xE000) || codepoint >= 0x110000)
                    // codepoint is either in the surrogate range, or it is outside the valid range of
                    // a unicode codepoint
                    LEX_ERROR(X, invalid_unicode_codepoint, saved, codepoint);

                // Translate the codepoint into bytes. Modified from @Tencent/rapidjson.
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
                break;
            }
            default:
                LEX_ERROR(X, invalid_escape, X->loc, c);
        }
    } else if (test_next(X, quote)) {
        pop_state(X);
        return make_string(X, start, TK_STRING_TEXT);
    } else if (ISNEWLINE(*X->ptr)) {
        // unescaped newlines allowed in string literals
        SAVE_AND_NEXT(X);
    } else if (test(X, '\0')) {
        LEX_ERROR(X, unexpected_symbol, X->loc);
    } else {
        consume_utf8(X);
    }
    goto handle_ascii;
}

static struct Token consume_int_aux(struct Lex *X, struct SourceLoc start, char const *base_name)
{
    paw_Env *P = ENV(X);
    struct DynamicMem *dm = X->dm;
    paw_Int i;

    int const rc = pawV_parse_int(P, dm->scratch.data, 0, &i);
    if (rc == PAW_EOVERFLOW) {
        LEX_ERROR(X, integer_out_of_range, start, dm->scratch.data);
    } else if (rc == PAW_ESYNTAX) {
        LEX_ERROR(X, invalid_integer, start, base_name, dm->scratch.data);
    }
    return (struct Token){
        .span = span_from(X, start),
        .kind = TK_INTEGER,
        .value.i = i,
    };
}

static struct Token consume_bin_int(struct Lex *X, struct SourceLoc start, const char *begin)
{
    struct DynamicMem *dm = X->dm;

    save(X, begin[0]);
    save(X, begin[1]);

    if (!test2(X, "01"))
        LEX_ERROR(X, expected_integer_digit, X->loc, "binary");

    while (ISNAME(*X->ptr) || ISDIGIT(*X->ptr)) {
        if (test_next(X, '_')) {
            // ignore digit separators
        } else if (!test2(X, "01")) {
            LEX_ERROR(X, unexpected_integer_char, X->loc, *X->ptr, "binary");
        } else {
            SAVE_AND_NEXT(X);
        }
    }

    save(X, '\0');
    return consume_int_aux(X, start, "binary");
}

static struct Token consume_oct_int(struct Lex *X, struct SourceLoc start, const char *begin)
{
    struct DynamicMem *dm = X->dm;

    save(X, begin[0]);
    save(X, begin[1]);

    if (*X->ptr < '0' || *X->ptr > '7')
        LEX_ERROR(X, expected_integer_digit, X->loc, "octal");

    while (ISNAME(*X->ptr) || ISDIGIT(*X->ptr)) {
        if (test_next(X, '_')) {
            // ignore digit separators
        } else if (*X->ptr < '0' || *X->ptr > '7') {
            LEX_ERROR(X, unexpected_integer_char, X->loc, *X->ptr, "octal");
        } else {
            SAVE_AND_NEXT(X);
        }
    }

    save(X, '\0');
    return consume_int_aux(X, start, "octal");
}

static struct Token consume_hex_int(struct Lex *X, struct SourceLoc start, const char *begin)
{
    struct DynamicMem *dm = X->dm;

    save(X, begin[0]);
    save(X, begin[1]);

    if (!ISHEX(*X->ptr))
        LEX_ERROR(X, expected_integer_digit, X->loc, "hexadecimal");

    while (ISNAME(*X->ptr) || ISDIGIT(*X->ptr)) {
        if (test_next(X, '_')) {
            // ignore digit separators
        } else if (!ISHEX(*X->ptr)) {
            LEX_ERROR(X, unexpected_integer_char, X->loc, *X->ptr, "hexadecimal");
        } else {
            SAVE_AND_NEXT(X);
        }
    }

    save(X, '\0');
    return consume_int_aux(X, start, "hexadecimal");
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

    paw_Env *P = ENV(X);
    struct DynamicMem *dm = X->dm;
    paw_Int i;

    int const rc = pawV_parse_int(P, dm->scratch.data, 0, &i);
    if (rc == PAW_EOVERFLOW)
        LEX_ERROR(X, integer_out_of_range, start, dm->scratch.data);
    if (rc == PAW_ESYNTAX)
        LEX_ERROR(X, invalid_integer, start, "decimal", dm->scratch.data);
    return (struct Token){
        .span = span_from(X, start),
        .kind = TK_INTEGER,
        .value.i = i,
    };
}

static struct Token consume_float(struct Lex *X, struct SourceLoc start, const char *begin)
{
    save_parsed_digits(X, begin);

    paw_Env *P = ENV(X);
    struct DynamicMem *dm = X->dm;
    paw_Float f;

    int const rc = pawV_parse_float(ENV(X), dm->scratch.data, &f);
    if (rc != PAW_OK)
        LEX_ERROR(X, invalid_float, start, dm->scratch.data);
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
        return consume_bin_int(X, start, begin);
    if (*begin == '0' && test_next2(X, "oO"))
        return consume_oct_int(X, start, begin);
    if (*begin == '0' && test_next2(X, "xX"))
        return consume_hex_int(X, start, begin);

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
        if (!ISDIGIT(*X->ptr)) // TODO: hack to prevent '_' before first digit after "e" ["+" | "-"]
            LEX_ERROR(X, unexpected_symbol, start);
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
    if (X->ptr == X->end)
        return make_token(TK_END, X->loc, X->loc);

    if (peek_state(X) != STATE_NORMAL)
        return consume_string_part(X, X->loc, peek_state(X));

    skip_whitespace(X);
    struct SourceLoc start = X->loc;

    // cast to avoid sign extension
    struct Token token = T((uint8_t)*X->ptr);
    X->dm->scratch.count = 0;
    switch (token.kind) {
        case '\'':
        case '"':
            push_string_state(X, *X->ptr);
            token = consume_string_part(X, start, next(X));
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
                token = T(TK_PLUS2);
            } else if (test_next(X, '=')) {
                token = T(TK_PLUS_EQ);
            }
            break;
        case '.':
            next(X);
            if (test_next(X, '.')) {
                if (test_next(X, '.')) {
                    token = T(TK_DOT3);
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
        default:
            if (ISDIGIT(*X->ptr)) {
                token = consume_number(X, start);
            } else if (ISNAME(*X->ptr)) {
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
    struct SourceBuffer *b = &X->dm->source;
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
    struct DynamicMem *dm = X->dm;
    if (dm->scratch.alloc < INITIAL_SCRATCH) {
        pawM_resize(P, dm->scratch.data, dm->scratch.alloc, INITIAL_SCRATCH);
        dm->scratch.alloc = INITIAL_SCRATCH;
    }
    dm->scratch.count = 0;

    X->ud = ud;
    X->input = input;

    X->states = CharStack_new(X);
    X->parens = IntStack_new(X);

    read_source(X);
    push_normal_state(X);
    pawX_next(X); // load first token
}

