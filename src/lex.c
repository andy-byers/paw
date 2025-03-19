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

static void increment_line(struct Lex *x)
{
    paw_assert(ISNEWLINE(*x->ptr));
    if (x->loc.line == INT_MAX)
        LEX_ERROR(x, too_many_lines, x->loc, INT_MAX);

    x->loc.column = 1;
    ++x->loc.line;
}

static void increment_column(struct Lex *x)
{
    if (x->loc.column == INT_MAX)
        LEX_ERROR(x, too_many_columns, x->loc, INT_MAX);

    ++x->loc.column;
}

static void save(struct Lex *x, char c)
{
    struct DynamicMem *dm = x->dm;
    pawM_grow(ENV(x), dm->scratch.data, dm->scratch.count, dm->scratch.alloc);
    dm->scratch.data[dm->scratch.count++] = c;
}

static char next(struct Lex *x)
{
    if (ISNEWLINE(*x->ptr)) {
        increment_line(x);
    } else {
        increment_column(x);
    }
    return *x->ptr++;
}

static paw_Bool test(struct Lex *x, char c)
{
    return *x->ptr == c;
}

static paw_Bool test_next(struct Lex *x, char c)
{
    return test(x, c) ? (next(x), PAW_TRUE) : PAW_FALSE;
}

static paw_Bool test2(struct Lex *x, char const *c2)
{
    return *x->ptr == c2[0] || *x->ptr == c2[1];
}

static paw_Bool test_next2(struct Lex *x, char const *c2)
{
    return test2(x, c2) ? (next(x), PAW_TRUE) : PAW_FALSE;
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

static struct Token make_int(struct Lex *x)
{
    paw_Env *P = ENV(x);
    Value const v = P->top.p[-1];
    return (struct Token){
        .kind = TK_INTEGER,
        .value = v,
    };
}

static struct Token make_float(struct Lex *x)
{
    paw_Env *P = ENV(x);
    Value const v = P->top.p[-1];
    return (struct Token){
        .kind = TK_FLOAT,
        .value = v,
    };
}

static struct Token make_string(struct Lex *x, struct SourceLoc start, TokenKind kind)
{
    struct DynamicMem *dm = x->dm;
    struct StringBuffer *b = &dm->scratch;
    struct Token t = make_token(kind, start, x->loc);
    String *s = pawP_scan_nstring(x->C, x->strings, b->data, CAST_SIZE(b->count));
    V_SET_OBJECT(&t.value, s);
    b->count = 0;
    return t;
}

static struct Token consume_name(struct Lex *x, struct SourceLoc start)
{
    SAVE_AND_NEXT(x);
    while (ISNAME(*x->ptr) || ISDIGIT(*x->ptr))
        SAVE_AND_NEXT(x);

    struct Token t = make_string(x, start, TK_NAME);
    String const *s = V_STRING(t.value);
    if (IS_KEYWORD(s)) {
        t.kind = CAST(TokenKind, s->flag);
    } else if (s->length > PAW_NAME_MAX) {
        LEX_ERROR(x, name_too_long, start, s->length, PAW_NAME_MAX);
    }
    return t;
}

// Tables are from https://arxiv.org/pdf/2010.03090.pdf
static void consume_utf8(struct Lex *x)
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
        unsigned char const c = (unsigned char)*x->ptr;
        state = kLookup1[c] + state * 12;
        state = kLookup2[state];
        SAVE_AND_NEXT(x);
    } while (state % 8 != 0);

     // TODO: keep track of codepoint for error message
    if (state != 0)
        LEX_ERROR(x, invalid_unicode_codepoint, x->loc, -1);
}

static int get_codepoint(struct Lex *x)
{
    if (x->end - x->ptr < 4)
        return -1;

    const char c[5] = {
        next(x),
        next(x),
        next(x),
        next(x),
    };

    if (!ISHEX(c[0])
            || !ISHEX(c[1])
            || !ISHEX(c[2])
            || !ISHEX(c[3]))
        LEX_ERROR(x, invalid_unicode_escape, x->loc, c);

    return HEXVAL(c[0]) << 12
           | HEXVAL(c[1]) << 8
           | HEXVAL(c[2]) << 4
           | HEXVAL(c[3]);
}

static struct Token consume_string(struct Lex *x, struct SourceLoc start)
{
    char const quote = next(x);

    for (;;) {
    handle_ascii:
        if (ISASCIIEND(*x->ptr))
            break;
        SAVE_AND_NEXT(x);
    }

    // TODO: truncated strings
    if (test_next(x, '\\')) {
        char const c = next(x);
        switch (c) {
            case '"':
                save(x, '"');
                break;
            case '\\':
                save(x, '\\');
                break;
            case '/':
                save(x, '/');
                break;
            case 'b':
                save(x, '\b');
                break;
            case 'v':
                save(x, '\v');
                break;
            case 'f':
                save(x, '\f');
                break;
            case 'n':
                save(x, '\n');
                break;
            case 'r':
                save(x, '\r');
                break;
            case 't':
                save(x, '\t');
                break;
            case 'u': {
                struct SourceLoc saved = x->loc;
                unsigned const codepoint = get_codepoint(x);
                if ((0xD800 <= codepoint && codepoint < 0xE000) || codepoint >= 0x110000)
                    // codepoint is either in the surrogate range, or it is outside the valid range of
                    // a unicode codepoint
                    LEX_ERROR(x, invalid_unicode_codepoint, saved, codepoint);

                // Translate the codepoint into bytes. Modified from @Tencent/rapidjson.
                if (codepoint <= 0x7F) {
                    save(x, (char)codepoint);
                } else if (codepoint <= 0x7FF) {
                    save(x, (char)(0xC0 | ((codepoint >> 6) & 0xFF)));
                    save(x, (char)(0x80 | ((codepoint & 0x3F))));
                } else if (codepoint <= 0xFFFF) {
                    save(x, (char)(0xE0 | ((codepoint >> 12) & 0xFF)));
                    save(x, (char)(0x80 | ((codepoint >> 6) & 0x3F)));
                    save(x, (char)(0x80 | (codepoint & 0x3F)));
                } else {
                    assert(codepoint <= 0x10FFFF);
                    save(x, (char)(0xF0 | ((codepoint >> 18) & 0xFF)));
                    save(x, (char)(0x80 | ((codepoint >> 12) & 0x3F)));
                    save(x, (char)(0x80 | ((codepoint >> 6) & 0x3F)));
                    save(x, (char)(0x80 | (codepoint & 0x3F)));
                }
                break;
            }
            default:
                LEX_ERROR(x, invalid_escape, x->loc, c);
        }
    } else if (test_next(x, quote)) {
        return make_string(x, start, TK_STRING);
    } else if (ISNEWLINE(*x->ptr)) {
        // unescaped newlines allowed in string literals
        SAVE_AND_NEXT(x);
    } else {
        consume_utf8(x);
    }
    goto handle_ascii;
}

static struct Token consume_int_aux(struct Lex *x, struct SourceLoc start, char const *base_name)
{
    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    paw_Int i;

    int const rc = pawV_parse_int(P, dm->scratch.data, 0, &i);
    if (rc == PAW_EOVERFLOW) {
        LEX_ERROR(x, integer_out_of_range, start, dm->scratch.data);
    } else if (rc == PAW_ESYNTAX) {
        LEX_ERROR(x, invalid_integer, start, base_name, dm->scratch.data);
    }
    return (struct Token){
        .span = span_from(x, start),
        .kind = TK_INTEGER,
        .value.i = i,
    };
}

static struct Token consume_bin_int(struct Lex *x, struct SourceLoc start, const char *begin)
{
    struct DynamicMem *dm = x->dm;

    save(x, begin[0]);
    save(x, begin[1]);

    if (!test2(x, "01"))
        LEX_ERROR(x, expected_integer_digit, x->loc, "binary");

    while (ISNAME(*x->ptr) || ISDIGIT(*x->ptr)) {
        if (test_next(x, '_')) {
            // ignore digit separators
        } else if (!test2(x, "01")) {
            LEX_ERROR(x, unexpected_integer_char, x->loc, *x->ptr, "binary");
        } else {
            SAVE_AND_NEXT(x);
        }
    }

    save(x, '\0');
    return consume_int_aux(x, start, "binary");
}

static struct Token consume_oct_int(struct Lex *x, struct SourceLoc start, const char *begin)
{
    struct DynamicMem *dm = x->dm;

    save(x, begin[0]);
    save(x, begin[1]);

    if (*x->ptr < '0' || *x->ptr > '7')
        LEX_ERROR(x, expected_integer_digit, x->loc, "octal");

    while (ISNAME(*x->ptr) || ISDIGIT(*x->ptr)) {
        if (test_next(x, '_')) {
            // ignore digit separators
        } else if (*x->ptr < '0' || *x->ptr > '7') {
            LEX_ERROR(x, unexpected_integer_char, x->loc, *x->ptr, "octal");
        } else {
            SAVE_AND_NEXT(x);
        }
    }

    save(x, '\0');
    return consume_int_aux(x, start, "octal");
}

static struct Token consume_hex_int(struct Lex *x, struct SourceLoc start, const char *begin)
{
    struct DynamicMem *dm = x->dm;

    save(x, begin[0]);
    save(x, begin[1]);

    if (!ISHEX(*x->ptr))
        LEX_ERROR(x, expected_integer_digit, x->loc, "hexadecimal");

    while (ISNAME(*x->ptr) || ISDIGIT(*x->ptr)) {
        if (test_next(x, '_')) {
            // ignore digit separators
        } else if (!ISHEX(*x->ptr)) {
            LEX_ERROR(x, unexpected_integer_char, x->loc, *x->ptr, "hexadecimal");
        } else {
            SAVE_AND_NEXT(x);
        }
    }

    save(x, '\0');
    return consume_int_aux(x, start, "hexadecimal");
}

static void save_parsed_digits(struct Lex *x, const char *begin)
{
    for (; begin < x->ptr; ++begin) {
        if (*begin != '_')
            save(x, *begin);
    }
    save(x, '\0');
}

static struct Token consume_decimal_int(struct Lex *x, struct SourceLoc start, const char *begin)
{
    save_parsed_digits(x, begin);

    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    paw_Int i;

    int const rc = pawV_parse_int(P, dm->scratch.data, 0, &i);
    if (rc == PAW_EOVERFLOW)
        LEX_ERROR(x, integer_out_of_range, start, dm->scratch.data);
    if (rc == PAW_ESYNTAX)
        LEX_ERROR(x, invalid_integer, start, "decimal", dm->scratch.data);
    return (struct Token){
        .span = span_from(x, start),
        .kind = TK_INTEGER,
        .value.i = i,
    };
}

static struct Token consume_float(struct Lex *x, struct SourceLoc start, const char *begin)
{
    save_parsed_digits(x, begin);

    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    paw_Float f;

    int const rc = pawV_parse_float(ENV(x), dm->scratch.data, &f);
    if (rc != PAW_OK)
        LEX_ERROR(x, invalid_float, start, dm->scratch.data);
    return (struct Token){
        .span = span_from(x, start),
        .kind = TK_FLOAT,
        .value.f = f,
    };
}

static struct Token consume_number(struct Lex *x, struct SourceLoc start)
{
    const char *begin = x->ptr;
    paw_assert(ISDIGIT(*begin));
    next(x);

    if (*begin == '0' && test_next2(x, "bB"))
        return consume_bin_int(x, start, begin);
    if (*begin == '0' && test_next2(x, "oO"))
        return consume_oct_int(x, start, begin);
    if (*begin == '0' && test_next2(x, "xX"))
        return consume_hex_int(x, start, begin);

    while (ISDIGIT(*x->ptr) || test(x, '_'))
        next(x);

    if (test(x, '.')) {
        // Allow accessing fields directly on an int or float (without parenthesis),
        // as well as chained tuple selectors. "x->ptr[1]" works because the source
        // buffer ends with a '\0'.
        if (!ISDIGIT(x->ptr[1]) || x->t.kind == '.')
            return consume_decimal_int(x, start, begin);

        next(x); // skip '.'
        while (ISDIGIT(*x->ptr) || test(x, '_'))
            next(x);
    } else if (!test2(x, "eE")) {
        return consume_decimal_int(x, start, begin);
    }

    if (test_next2(x, "eE")) {
        test_next2(x, "+-");
        if (!ISDIGIT(*x->ptr)) // TODO: hack to prevent '_' before first digit after "e" ["+" | "-"]
            LEX_ERROR(x, unexpected_symbol, start);
    }

    while (ISDIGIT(*x->ptr) || test(x, '_'))
        next(x);
    return consume_float(x, start, begin);
}

static void skip_line_comment(struct Lex *x)
{
    while (!IS_EOF(x) && !ISNEWLINE(*x->ptr))
        next(x);
}

static void skip_whitespace(struct Lex *x)
{
    while (*x->ptr == ' '
           || *x->ptr == '\t'
           || *x->ptr == '\f'
           || *x->ptr == '\v'
           || IS_NEWLINE(x)) {
        next(x);
    }
}

static struct Token advance(struct Lex *x)
{
#define T(kind) make_token(CAST(TokenKind, kind), start, x->loc)
try_again:
    if (x->ptr == x->end)
        return make_token(TK_END, x->loc, x->loc);
    skip_whitespace(x);

    struct SourceLoc start = x->loc;

    // cast to avoid sign extension
    struct Token token = T(CAST(uint8_t, *x->ptr));
    x->dm->scratch.count = 0;
    switch (token.kind) {
        case '\'':
        case '"':
            token = consume_string(x, start);
            break;
        case ')':
        case ']':
        case '}':
            next(x);
            break;
        case '=':
            next(x);
            if (test_next(x, '=')) {
                token = T(TK_EQUALS2);
            } else if (test_next(x, '>')) {
                token = T(TK_FAT_ARROW);
            }
            break;
        case '&':
            next(x);
            if (test_next(x, '&')) {
                token = T(TK_AMPER2);
            }
            break;
        case '|':
            next(x);
            if (test_next(x, '|')) {
                token = T(TK_PIPE2);
            }
            break;
        case '-':
            next(x);
            if (test_next(x, '>')) {
                token = T(TK_ARROW);
            }
            break;
        case ':':
            next(x);
            if (test_next(x, ':')) {
                token = T(TK_COLON2);
            }
            break;
        case '!':
            next(x);
            if (test_next(x, '=')) {
                token = T(TK_BANG_EQ);
            }
            break;
        case '<':
            next(x);
            if (test_next(x, '<')) {
                token = T(TK_LESS2);
            } else if (test_next(x, '=')) {
                token = T(TK_LESS_EQ);
            }
            break;
        case '>':
            next(x);
            if (test_next(x, '>')) {
                token = T(TK_GREATER2);
            } else if (test_next(x, '=')) {
                token = T(TK_GREATER_EQ);
            }
            break;
        case '+':
            next(x);
            if (test_next(x, '+')) {
                token = T(TK_PLUS2);
            }
            break;
        case '.':
            next(x);
            if (test_next(x, '.')) {
                if (test_next(x, '.')) {
                    token = T(TK_DOT3);
                } else {
                    token = T(TK_DOT2);
                }
            }
            break;
        case '/':
            next(x);
            if (test_next(x, '/')) {
                skip_line_comment(x);
                goto try_again;
            }
            break;
        default:
            if (ISDIGIT(*x->ptr)) {
                token = consume_number(x, start);
            } else if (ISNAME(*x->ptr)) {
                token = consume_name(x, start);
            } else {
                next(x);
            }
    }
    return token;
#undef T
}

// Read the source code into a buffer
static void read_source(struct Lex *x)
{
    paw_Env *P = ENV(x);
    struct SourceBuffer *b = &x->dm->source;
    size_t next, size = 0;

    for (;;) {
        const char *chunk = x->input(P, x->ud, &next);
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
    x->ptr = b->data;
    x->end = b->data + size;

    pawSrc_init_location(&x->loc);
    x->last_loc = x->loc;

    x->t2.kind = TK_NONE;
}

TokenKind pawX_next(struct Lex *x)
{
    x->last_loc = x->loc;
    TokenKind const kind = pawX_peek(x);
    x->t0 = x->t;
    x->t = x->t2;
    x->t2.kind = TK_NONE;
    return kind;
}

TokenKind pawX_peek(struct Lex *x)
{
    if (x->t2.kind == TK_NONE)
        x->t2 = advance(x);
    return x->t2.kind;
}

#define INITIAL_SCRATCH 128

void pawX_set_source(struct Lex *x, paw_Reader input, void *ud)
{
    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    if (dm->scratch.alloc < INITIAL_SCRATCH) {
        pawM_resize(P, dm->scratch.data, dm->scratch.alloc, INITIAL_SCRATCH);
        dm->scratch.alloc = INITIAL_SCRATCH;
    }
    dm->scratch.count = 0;

    x->ud = ud;
    x->input = input;

    read_source(x);
    struct Token t2;
    pawX_next(x); // load first token
}
