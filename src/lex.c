// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "lex.h"
#include "compile.h"

#define LEX_ERROR(x) pawX_error(x, "syntax error")
#define SAVE_AND_NEXT(x) (save(x, *(x)->ptr), ++x->ptr)
#define IS_EOF(x) (CAST(uint8_t, *(x)->ptr) == TK_END)
#define IS_NEWLINE(x) (*(x)->ptr == '\r' || *(x)->ptr == '\n')

static void add_location(paw_Env *P, Buffer *print, String const *s, int line)
{
    pawL_add_nstring(P, print, s->text, s->length);
    pawL_add_fstring(P, print, ":%d: ", line);
}

_Noreturn void pawX_error(struct Lex *x, char const *fmt, ...)
{
    Buffer print;
    paw_Env *P = ENV(x);
    pawL_init_buffer(P, &print);
    add_location(P, &print, x->modname, x->line);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &print, fmt, arg);
    va_end(arg);

    pawL_push_result(P, &print);
    pawC_throw(P, PAW_ESYNTAX);
}

static void increment_line(struct Lex *x)
{
    paw_assert(ISNEWLINE(*x->ptr));
    if (x->line == INT_MAX) {
        pawX_error(x, "too many lines in module");
    }
    ++x->line;
}

static void save(struct Lex *x, char c)
{
    struct DynamicMem *dm = x->dm;
    pawM_grow(ENV(x), dm->scratch.data, dm->scratch.count, dm->scratch.alloc);
    dm->scratch.data[dm->scratch.count++] = c;
}

static char next(struct Lex *x)
{
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

static struct Token make_token(TokenKind kind)
{
    return (struct Token){
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

static struct Token make_string(struct Lex *x, TokenKind kind)
{
    struct DynamicMem *dm = x->dm;
    struct StringBuffer *b = &dm->scratch;
    struct Token t = make_token(kind);
    String *s = pawP_scan_nstring(x->C, x->strings, b->data, CAST_SIZE(b->count));
    V_SET_OBJECT(&t.value, s);
    b->count = 0;
    return t;
}

static struct Token consume_name(struct Lex *x)
{
    SAVE_AND_NEXT(x);
    while (ISNAME(*x->ptr) || ISDIGIT(*x->ptr)) {
        SAVE_AND_NEXT(x);
    }
    struct Token t = make_string(x, TK_NAME);
    String const *s = V_STRING(t.value);
    if (IS_KEYWORD(s)) {
        t.kind = CAST(TokenKind, s->flag);
    } else if (s->length > PAW_NAME_MAX) {
        pawX_error(x, "name (%I chars) is too long", PAW_CAST_INT(s->length));
    }
    return t;
}

// Tables are from https://arxiv.org/pdf/2010.03090.pdf
static int consume_utf8(struct Lex *x)
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

    uint32_t state = 0;
    do {
        uint8_t const c = CAST(uint8_t, *x->ptr);
        state = kLookup1[c] + state * 12;
        state = kLookup2[state];
        SAVE_AND_NEXT(x);
    } while (state % 8 != 0);
    return state ? -1 : 0;
}

static int get_codepoint(struct Lex *x)
{
    if (x->end - x->ptr < 4)
        return -1;
    const char c[4] = {
        next(x),
        next(x),
        next(x),
        next(x),
    };

    if (!ISHEX(c[0])
            || !ISHEX(c[1])
            || !ISHEX(c[2])
            || !ISHEX(c[3])) {
        return -1;
    }
    return HEXVAL(c[0]) << 12
        | HEXVAL(c[1]) << 8
        | HEXVAL(c[2]) << 4
        | HEXVAL(c[3]);
}

static struct Token consume_string(struct Lex *x)
{
    char const quote = next(x);

    for (;;) {
    handle_ascii:
        if (ISASCIIEND(*x->ptr)) {
            break;
        }
        SAVE_AND_NEXT(x);
    }

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
                int codepoint = get_codepoint(x);
                if (codepoint < 0)
                    LEX_ERROR(x);
                if (0xD800 <= codepoint && codepoint <= 0xDFFF) {
                    // Codepoint is part of a surrogate pair. Expect a high
                    // surrogate (U+D800–U+DBFF) followed by a low surrogate
                    // (U+DC00–U+DFFF).
                    if (codepoint <= 0xDBFF) {
                        if (!test_next(x, '\\') || !test_next(x, 'u')) {
                            LEX_ERROR(x);
                        }
                        int const codepoint2 = get_codepoint(x);
                        if (codepoint2 < 0xDC00 || codepoint2 > 0xDFFF) {
                            LEX_ERROR(x);
                        }
                        codepoint = (((codepoint - 0xD800) << 10) |
                                     (codepoint2 - 0xDC00)) +
                                    0x10000;
                    } else {
                        LEX_ERROR(x);
                    }
                }
                // Translate the codepoint into bytes. Modified from
                // @Tencent/rapidjson.
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
                LEX_ERROR(x);
        }
    } else if (test_next(x, quote)) {
        return make_string(x, TK_STRING);
    } else if (ISNEWLINE(*x->ptr)) {
        // unescaped newlines allowed in string literals
        SAVE_AND_NEXT(x);
    } else if (consume_utf8(x)) {
        LEX_ERROR(x);
    }
    goto handle_ascii;
}

static struct Token consume_int_aux(struct Lex *x)
{
    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    paw_Int i;

    int const rc = pawV_parse_int(P, dm->scratch.data, 0, &i);
    if (rc == PAW_EOVERFLOW) {
        pawX_error(x, "integer '%s' is out of range for 'int' type", dm->scratch.data);
    } else if (rc == PAW_ESYNTAX) {
        pawX_error(x, "invalid integer '%s'", dm->scratch.data);
    }
    return (struct Token){
        .kind = TK_INTEGER,
        .value.i = i,
    };
}

static struct Token consume_bin_int(struct Lex *x, const char *begin)
{
    if (!test2(x, "01"))
        pawX_error(x, "expected at least 1 binary digit");

    save(x, begin[0]);
    save(x, begin[1]);

    while (ISNAME(*x->ptr) || ISDIGIT(*x->ptr)) {
        if (test_next(x, '_')) {
            // ignore digit separators
        } else if (!test2(x, "01")) {
            pawX_error(x, "unexpected '%c' in binary integer", *x->ptr);
        } else {
            SAVE_AND_NEXT(x);
        }
    }

    save(x, '\0');
    return consume_int_aux(x);
}

static struct Token consume_oct_int(struct Lex *x, const char *begin)
{
    if (*x->ptr < '0' || *x->ptr > '7')
        pawX_error(x, "expected at least 1 octal digit");

    save(x, begin[0]);
    save(x, begin[1]);

    while (ISNAME(*x->ptr) || ISDIGIT(*x->ptr)) {
        if (test_next(x, '_')) {
            // ignore digit separators
        } else if (*x->ptr < '0' || *x->ptr > '7') {
            pawX_error(x, "unexpected '%c' in octal integer", *x->ptr);
        } else {
            SAVE_AND_NEXT(x);
        }
    }

    save(x, '\0');
    return consume_int_aux(x);
}

static struct Token consume_hex_int(struct Lex *x, const char *begin)
{
    if (!ISHEX(*x->ptr))
        pawX_error(x, "expected at least 1 hexadecimal digit");

    save(x, begin[0]);
    save(x, begin[1]);

    while (ISNAME(*x->ptr) || ISDIGIT(*x->ptr)) {
        if (test_next(x, '_')) {
            // ignore digit separators
        } else if (!ISHEX(*x->ptr)) {
            pawX_error(x, "unexpected '%c' in hexadecimal integer", *x->ptr);
        } else {
            SAVE_AND_NEXT(x);
        }
    }

    save(x, '\0');
    return consume_int_aux(x);
}

static void save_parsed_digits(struct Lex *x, const char *begin)
{
    for (; begin < x->ptr; ++begin) {
        if (*begin != '_') save(x, *begin);
    }
    save(x, '\0');
}

static struct Token consume_decimal_int(struct Lex *x, const char *begin)
{
    save_parsed_digits(x, begin);

    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    paw_Int i;

    int const rc = pawV_parse_int(P, dm->scratch.data, 0, &i);
    if (rc == PAW_EOVERFLOW)
        pawX_error(x, "integer '%s' is out of range for 'int' type", dm->scratch.data);
    if (rc == PAW_ESYNTAX)
        pawX_error(x, "invalid integer '%s'", dm->scratch.data);
    return (struct Token){
        .kind = TK_INTEGER,
        .value.i = i,
    };
}

static struct Token consume_float(struct Lex *x, const char *begin)
{
    save_parsed_digits(x, begin);

    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    paw_Float f;

    int const rc = pawV_parse_float(ENV(x), dm->scratch.data, &f);
    if (rc != PAW_OK)
        pawX_error(x, "invalid number '%s'", dm->scratch.data);
    return (struct Token){
        .kind = TK_FLOAT,
        .value.f = f,
    };
}

static struct Token consume_number(struct Lex *x)
{
    const char *begin = x->ptr++;
    paw_assert(ISDIGIT(*begin));

    if (*begin == '0' && test_next2(x, "bB"))
        return consume_bin_int(x, begin);
    if (*begin == '0' && test_next2(x, "oO"))
        return consume_oct_int(x, begin);
    if (*begin == '0' && test_next2(x, "xX"))
        return consume_hex_int(x, begin);

    while (ISDIGIT(*x->ptr) || test(x, '_'))
        next(x);

    if (test(x, '.')) {
        // Allow accessing fields directly on an int or float (without parenthesis),
        // as well as chained tuple selectors. "x->ptr[1]" works because the source
        // buffer ends with a '\0'.
        if (!ISDIGIT(x->ptr[1]) || x->t.kind == '.')
            return consume_decimal_int(x, begin);
    } else if (!test2(x, "eE")) {
        return consume_decimal_int(x, begin);
    }

    if (test_next(x, '.')) {
        while (ISDIGIT(*x->ptr) || test(x, '_'))
            next(x);
    }

    if (test_next2(x, "eE")) {
        test_next2(x, "+-");
        if (!ISDIGIT(*x->ptr))
            pawX_error(x, "malformed float");
    }

    while (ISDIGIT(*x->ptr) || test(x, '_'))
        next(x);
    return consume_float(x, begin);

//    // Save source text in a buffer until a byte is reached that cannot possibly
//    // be part of a number.
//    char const first = *x->ptr;
//    SAVE_AND_NEXT(x);
//
//    paw_Bool likely_float = PAW_FALSE;
//    paw_Bool likely_int = first == '0' &&
//                          (test_next2(x, "bB") ||
//                           test_next2(x, "oO") ||
//                           test_next2(x, "xX"));
//    paw_Bool const dot_selector = x->t.kind == '.';
//    if (dot_selector) {
//        if (likely_int) {
//            pawX_error(x, "'.' selector must be a base-10 integer");
//        }
//        likely_int = PAW_TRUE;
//    }
//
//    // Consume adjacent floating-point indicators, exponents, and fractional
//    // parts.
//    for (;;) {
//        // Make sure not to consume byte sequences like "e+" or "E-" if we have
//        // already encountered a non-decimal integer prefix. This allows expressions
//        // like "0x1e+1" to be parsed like "0x1e + 1" instead of raising a syntax
//        // error.
//        if (!likely_int && test_next2(x, "eE")) {
//            likely_float = PAW_TRUE;
//            test_next2(x, "+-");
//            continue;
//        }
//        if (ISHEX(*x->ptr)) {
//            // save digits below
//        } else if (*x->ptr == '.') {
//            if (dot_selector)
//                break;
//            likely_float = PAW_TRUE;
//        } else {
//            break;
//        }
//        SAVE_AND_NEXT(x);
//    }
//    if (ISNAME(*x->ptr)) {
//        // cause pawV_to_number() to fail
//        SAVE_AND_NEXT(x);
//    }
//    save(x, '\0');
//
//    if (likely_int && likely_float) {
//        pawX_error(x, "malformed number '%s'", x->dm->scratch.data);
//    } else if (likely_float) {
//        return consume_float(x);
//    } else {
//        return consume_int(x, begin);
//    }
}

static void skip_block_comment(struct Lex *x)
{
    for (;;) {
        if (test_next(x, '*') && test_next(x, '/')) {
            break;
        } else if (IS_EOF(x)) {
            pawX_error(x, "missing end of block comment");
        }
        next(x);
    }
}

static void skip_line_comment(struct Lex *x)
{
    while (!IS_EOF(x) && !ISNEWLINE(*x->ptr)) {
        next(x);
    }
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
#define T(kind) make_token(CAST(TokenKind, kind))

try_again:
    paw_assert(x->ptr < x->end);
    skip_whitespace(x);

    // cast to avoid sign extension
    struct Token token = T(CAST(uint8_t, *x->ptr));
    x->dm->scratch.count = 0;
    switch (token.kind) {
        case '\'':
        case '"':
            token = consume_string(x);
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
                }
                LEX_ERROR(x); // '..' not allowed
            }
            break;
        case '/':
            next(x);
            if (test_next(x, '/')) {
                skip_line_comment(x);
                goto try_again;
            } else if (test_next(x, '*')) {
                skip_block_comment(x);
                goto try_again;
            }
            break;
        case '\r':
        case '\n':
            increment_line(x);
            goto try_again;
        default:
            if (ISDIGIT(*x->ptr)) {
                token = consume_number(x);
            } else if (ISNAME(*x->ptr)) {
                token = consume_name(x);
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
        if (next == 0) break;

        if (next > b->size - size) {
            size_t alloc = PAW_MAX(b->size, 1);
            while (alloc < size + next) alloc *= 2;
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
}

TokenKind pawX_next(struct Lex *x)
{
    x->last_line = x->line;
    TokenKind const kind = pawX_peek(x);
    x->t = x->t2;
    x->t2.kind = TK_NONE;
    return kind;
}

TokenKind pawX_peek(struct Lex *x)
{
    if (x->t2.kind == TK_NONE) {
        x->t2 = advance(x);
    }
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
    x->line = 1;
    x->t2.kind = TK_NONE;

    read_source(x);
    pawX_next(x); // load first token
}
