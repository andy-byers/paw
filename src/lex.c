// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "lex.h"
#include "compile.h"

#define LEX_ERROR(x) pawX_error(x, "syntax error")
#define SAVE_AND_NEXT(x) (save(x, (x)->c), next(x))
#define IS_EOF(x) (CAST(uint8_t, (x)->c) == TK_END)
#define IS_NEWLINE(x) ((x)->c == '\r' || (x)->c == '\n')

static void add_location(paw_Env *P, Buffer *print, const String *s, int line)
{
    pawL_add_nstring(P, print, s->text, s->length);
    pawL_add_fstring(P, print, ":%d: ", line);
}

_Noreturn
void pawX_error(struct Lex *x, const char *fmt, ...)
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

static char next_raw(struct Lex *x)
{
    paw_Env *P = ENV(x);
    if (x->nchunk == 0) {
        x->chunk = x->input(P, x->ud, &x->nchunk);
    }
    if (x->nchunk > 0) {
        x->c = x->chunk[0];
        --x->nchunk;
        ++x->chunk;
    } else {
        x->c = CAST(char, TK_END);
    }
    return x->c;
}

static void increment_line(struct Lex *x)
{
    paw_assert(ISNEWLINE(x->c));
    if (x->line == INT_MAX) {
        pawX_error(x, "too many lines in module");
    }
    ++x->line;
}

static char next(struct Lex *x)
{
    char c = next_raw(x);
    if (ISNEWLINE(c)) {
        increment_line(x);
    }
    return c;
}

static void save(struct Lex *x, char c)
{
    struct DynamicMem *dm = x->dm;
    pawM_grow(ENV(x), dm->scratch.data, dm->scratch.count, dm->scratch.alloc);
    dm->scratch.data[dm->scratch.count++] = c;
}

static paw_Bool test_next(struct Lex *x, char c)
{
    if (x->c == c) {
        next(x);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool test_next2(struct Lex *x, const char *c2)
{
    if (x->c == c2[0] || x->c == c2[1]) {
        SAVE_AND_NEXT(x);
        return PAW_TRUE;
    }
    return PAW_FALSE;
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
    const Value v = P->top.p[-1];
    return (struct Token){
        .kind = TK_INTEGER,
        .value = v,
    };
}

static struct Token make_float(struct Lex *x)
{
    paw_Env *P = ENV(x);
    const Value v = P->top.p[-1];
    return (struct Token){
        .kind = TK_FLOAT,
        .value = v,
    };
}

static struct Token make_string(struct Lex *x, TokenKind kind)
{
    struct DynamicMem *dm = x->dm;
    struct CharVec *cv = &dm->scratch;
    struct Token t = make_token(kind);
    String *s = pawP_scan_nstring(ENV(x), x->strings, cv->data, CAST_SIZE(cv->count));
    V_SET_OBJECT(&t.value, s);
    cv->count = 0;
    return t;
}

static struct Token consume_name(struct Lex *x)
{
    SAVE_AND_NEXT(x);
    while (ISNAME(x->c) || ISDIGIT(x->c)) {
        SAVE_AND_NEXT(x);
    }
    struct Token t = make_string(x, TK_NAME);
    const String *s = V_STRING(t.value);
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
        const uint8_t c = CAST(uint8_t, x->c);
        state = kLookup1[c] + state * 12;
        state = kLookup2[state];
        SAVE_AND_NEXT(x);
    } while (state % 8 != 0);
    return state ? -1 : 0;
}

static int get_codepoint(struct Lex *x)
{
    char c[4];
    c[0] = x->c; next(x);
    c[1] = x->c; next(x);
    c[2] = x->c; next(x);
    c[3] = x->c; next(x);

    if (!ISHEX(c[0]) ||
            !ISHEX(c[1]) ||
            !ISHEX(c[2]) ||
            !ISHEX(c[3])) {
        return -1;
    }
    return HEXVAL(c[0]) << 12 |
        HEXVAL(c[1]) << 8 |
        HEXVAL(c[2]) << 4 |
        HEXVAL(c[3]);
}

static struct Token consume_string(struct Lex *x)
{
    const char quote = x->c;
    next(x);

    for (;;) {
    handle_ascii:
        if (ISASCIIEND(x->c)) {
            break;
        }
        SAVE_AND_NEXT(x);
    }

    if (test_next(x, '\\')) {
        const char c = x->c;
        next(x);
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
                if (codepoint < 0) LEX_ERROR(x);
                if (0xD800 <= codepoint && codepoint <= 0xDFFF) {
                    // Codepoint is part of a surrogate pair. Expect a high
                    // surrogate (U+D800–U+DBFF) followed by a low surrogate
                    // (U+DC00–U+DFFF).
                    if (codepoint <= 0xDBFF) {
                        if (!test_next(x, '\\') || !test_next(x, 'u')) {
                            LEX_ERROR(x);
                        }
                        const int codepoint2 = get_codepoint(x);
                        if (codepoint2 < 0xDC00 || codepoint2 > 0xDFFF) {
                            LEX_ERROR(x);
                        }
                        codepoint = (((codepoint - 0xD800) << 10) |
                                     (codepoint2 - 0xDC00)) + 0x10000;
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
    } else if (ISNEWLINE(x->c)) {
        // unescaped newlines allowed in string literals
        SAVE_AND_NEXT(x);
    } else if (consume_utf8(x)) {
        LEX_ERROR(x);
    }
    goto handle_ascii;
}

static struct Token consume_int(struct Lex *x)
{
    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    paw_Int i;

    const int rc = pawV_parse_int(P, dm->scratch.data, 0, &i);
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

static struct Token consume_float(struct Lex *x)
{
    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    paw_Float f;

    const int rc = pawV_parse_float(ENV(x), dm->scratch.data, &f);
    if (rc != PAW_OK) pawX_error(x, "invalid number '%s'", dm->scratch.data);
    return (struct Token){
        .kind = TK_FLOAT,
        .value.f = f,
    };
}

static struct Token consume_number(struct Lex *x)
{
    // Save source text in a buffer until a byte is reached that cannot possibly
    // be part of a number.
    const char first = x->c;
    SAVE_AND_NEXT(x);

    paw_Bool likely_float = PAW_FALSE;
    paw_Bool likely_int = first == '0' &&
            (test_next2(x, "bB") ||
             test_next2(x, "oO") ||
             test_next2(x, "xX"));
    const paw_Bool dot_selector = x->t.kind == '.';
    if (dot_selector) {
        if (likely_int) {
            pawX_error(x, "'.' selector must be a base-10 integer");
        }
        likely_int = PAW_TRUE;
    }

    // Consume adjacent floating-point indicators, exponents, and fractional
    // parts.
    for (;;) {
        // Make sure not to consume byte sequences like "e+" or "E-" if we have
        // already encountered a non-decimal integer prefix. This allows expressions
        // like "0x1e+1" to be parsed like "0x1e + 1" instead of raising a syntax
        // error.
        if (!likely_int && test_next2(x, "eE")) {
            likely_float = PAW_TRUE;
            test_next2(x, "+-");
            continue;
        }
        if (ISHEX(x->c)) {
            // save digits below
        } else if (x->c == '.') {
            if (dot_selector) break;
            likely_float = PAW_TRUE;
        } else {
            break;
        }
        SAVE_AND_NEXT(x);
    }
    if (ISNAME(x->c)) {
        // cause pawV_to_number() to fail
        SAVE_AND_NEXT(x);
    }
    save(x, '\0');

    if (likely_int && likely_float) {
        pawX_error(x, "malformed number '%s'", x->dm->scratch.data);
    } else if (likely_float) {
        return consume_float(x);
    } else {
        return consume_int(x);
    }
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
    while (!IS_EOF(x) && !ISNEWLINE(x->c)) {
        next(x);
    }
}

static void skip_whitespace(struct Lex *x)
{
    while (x->c == ' ' ||
            x->c == '\t' ||
            x->c == '\f' ||
            x->c == '\v' ||
            IS_NEWLINE(x)) {
        next(x);
    }
}

static struct Token advance(struct Lex *x)
{
try_again:
#define T(kind) make_token(CAST(TokenKind, kind))
    skip_whitespace(x);

    // cast to avoid sign extension
    struct Token token = T(CAST(uint8_t, x->c));
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
        default:
            if (ISDIGIT(x->c)) {
                token = consume_number(x);
            } else if (ISNAME(x->c)) {
                token = consume_name(x);
            } else {
                next(x);
            }
    }
    return token;
#undef T
}

TokenKind pawX_next(struct Lex *x)
{
    x->last_line = x->line;
    const TokenKind kind = pawX_peek(x);
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

    next(x); // load first chunk of text
    pawX_next(x); // load first token
}

