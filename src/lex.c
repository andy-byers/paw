// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "lex.h"
#include "auxlib.h"
#include "compile.h"
#include "gc_aux.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "str.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <limits.h>
#include <stdlib.h>

#define lex_error(x) pawX_error(x, "syntax error")
#define save_and_next(x) (save(x, (x)->c), next(x))
#define is_eof(x) (cast((x)->c, uint8_t) == TK_END)
#define is_newline(x) ((x)->c == '\r' || (x)->c == '\n')

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
        x->c = cast(TK_END, char);
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
    pawM_grow(ENV(x), dm->scratch.data, dm->scratch.size, dm->scratch.alloc);
    dm->scratch.data[dm->scratch.size++] = c;
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
        save_and_next(x);
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
    pawC_stkdec(P, 1);
    return (struct Token){
        .kind = TK_INTEGER,
        .value = v, 
    };
}

static struct Token make_float(struct Lex *x)
{
    paw_Env *P = ENV(x);
    const Value v = P->top.p[-1];
    pawC_stkdec(P, 1);
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
    String *s = pawP_scan_nstring(ENV(x), x->strings, cv->data, cast_size(cv->size));
    v_set_object(&t.value, s);
    cv->size = 0;
    return t;
}

static struct Token consume_name(struct Lex *x)
{
    save_and_next(x);
    while (ISNAME(x->c) || ISDIGIT(x->c)) {
        save_and_next(x);
    }
    struct Token t = make_string(x, TK_NAME);
    const String *s = v_string(t.value);
    if (IS_KEYWORD(s)) {
        t.kind = cast(s->flag, TokenKind);
    } else if (s->length > PAW_NAME_MAX) {
        pawX_error(x, "name (%I chars) is too long", paw_cast_int(s->length));
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
        const uint8_t c = cast(x->c, uint8_t);
        state = kLookup1[c] + state * 12;
        state = kLookup2[state];
        save_and_next(x);
    } while (state % 8 != 0);
    return state ? -1 : 0;
}

static int get_codepoint(struct Lex *x)
{
    const char c[] = {
        save_and_next(x),
        save_and_next(x),
        save_and_next(x),
        save_and_next(x),
    };
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
        save_and_next(x);
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
                if (codepoint < 0) {
                    lex_error(x);
                }
                if (0xD800 <= codepoint && codepoint <= 0xDFFF) {
                    // Codepoint is part of a surrogate pair. Expect a high
                    // surrogate (U+D800–U+DBFF) followed by a low surrogate
                    // (U+DC00–U+DFFF).
                    if (codepoint <= 0xDBFF) {
                        if (save_and_next(x) != '\\' ||
                            save_and_next(x) != 'u') {
                            lex_error(x);
                        }
                        const int codepoint2 = get_codepoint(x);
                        if (codepoint2 < 0xDC00 || codepoint2 > 0xDFFF) {
                            lex_error(x);
                        }
                        codepoint = (((codepoint - 0xD800) << 10) |
                                     (codepoint2 - 0xDC00)) +
                                    0x10000;
                    } else {
                        lex_error(x);
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
                lex_error(x);
        }
    } else if (test_next(x, quote)) {
        return make_string(x, TK_STRING);
    } else if (ISNEWLINE(x->c)) {
        // unescaped newlines allowed in string literals
        save_and_next(x);
    } else if (consume_utf8(x)) {
        lex_error(x);
    }
    goto handle_ascii;
}

static int try_consume_int(struct Lex *x, struct Token *pt)
{
    paw_Env *P = ENV(x);
    struct DynamicMem *dm = x->dm;
    const int rc = pawV_parse_uint64(P, dm->scratch.data);
    if (rc == PAW_EOVERFLOW) {
         goto handle_overflow;
    } else if (rc == PAW_ESYNTAX) {
         return -1;
    }
    const Value *pv = &P->top.p[-1];
    if (pv->u > PAW_INT_MAX) {
handle_overflow:
         pawX_error(x, "integer '%s' is out of range for 'int' type", dm->scratch.data);
    }
    *pt = make_int(x);
    return rc;
}

static struct Token consume_float(struct Lex *x)
{
    struct DynamicMem *dm = x->dm;
    const int rc = pawV_parse_float(ENV(x), dm->scratch.data);
    if (rc != PAW_OK) {
         pawX_error(x, "invalid number '%s'", dm->scratch.data);
    }
    return make_float(x);
}

static struct Token consume_number(struct Lex *x)
{
    // Save source text in a buffer until a byte is reached that cannot possibly
    // be part of a number.
    const char first = x->c;
    save_and_next(x);

    if (first == '0' && 
            (test_next2(x, "bB") || 
             test_next2(x, "oO") || 
             test_next2(x, "xX"))) {
    }

    // Consume adjacent floating-point indicators, exponents, and fractional
    // parts.
    for (;;) {
        if (test_next2(x, "eE")) {
            test_next2(x, "+-");
        } else if (ISHEX(x->c) || x->c == '.') {
            save_and_next(x);
        } else {
            break;
        }
    }
    if (ISNAME(x->c)) {
        // cause pawV_to_number() to fail
        save_and_next(x);
    }
    save(x, '\0');

    struct Token token;
    const int rc = try_consume_int(x, &token);
    return rc == 0 ? token : consume_float(x);
}

static void skip_block_comment(struct Lex *x)
{
    for (;;) {
        if (test_next(x, '*') && test_next(x, '/')) {
            break;
        } else if (is_eof(x)) {
            pawX_error(x, "missing end of block comment");
        }
        next(x);
    }
}

static void skip_line_comment(struct Lex *x)
{
    while (!is_eof(x) && !ISNEWLINE(x->c)) {
        next(x);
    }
}

static void skip_whitespace(struct Lex *x)
{
    while (x->c == ' ' || x->c == '\t' || x->c == '\f' || x->c == '\v' ||
           (is_newline(x) && !x->add_semi)) {
        next(x);
    }
}

static struct Token advance(struct Lex *x)
{
try_again:
#define T(kind) make_token(cast(kind, TokenKind))
    skip_whitespace(x);

    // cast to avoid sign extension
    struct Token token = T(cast(x->c, uint8_t));
    paw_Bool semi = PAW_FALSE;
    x->dm->scratch.size = 0;
    switch (x->c) {
        case '\n':
        case '\r':
            paw_assert(x->add_semi);
            x->add_semi = PAW_FALSE;
            next(x); // make progress
            return T(';');
        case '\'':
        case '"':
            token = consume_string(x);
            semi = PAW_TRUE;
            break;
        case ')':
        case ']':
        case '}':
            next(x);
            semi = PAW_TRUE;
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
        case '.':
            next(x);
            if (test_next(x, '.')) {
                if (test_next(x, '.')) {
                    token = T(TK_DOT3);
                }
                lex_error(x); // '..' not allowed
            }
            break;
        case '/':
            next(x);
            if (test_next(x,
                          '/')) { // TODO: comments may need consideration wrt.
                                  // auto ';' insertion
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
                semi = PAW_TRUE;
            } else if (ISNAME(x->c)) {
                token = consume_name(x);
                semi = token.kind == TK_NAME || token.kind == TK_RETURN ||
                       token.kind == TK_BREAK || token.kind == TK_CONTINUE;
            } else {
                next(x);
            }
    }
    x->add_semi = semi;
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
    pawM_resize(P, dm->scratch.data, 0, INITIAL_SCRATCH);
    dm->scratch.alloc = INITIAL_SCRATCH;

    x->ud = ud;
    x->input = input;
    x->line = 1;

    next(x); // load first chunk of text
    pawX_next(x); // load first token
}

