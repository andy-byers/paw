// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "lex.h"
#include "auxlib.h"
#include "bigint.h"
#include "gc.h"
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

static void add_location(paw_Env *P, Buffer *print, const String *s, int line)
{
    pawL_add_nstring(P, print, s->text, s->length);
    pawL_add_fstring(P, print, ":%d: ", line);
}

void pawX_error(Lex *x, const char *fmt, ...)
{
    Buffer print;
    paw_Env *P = x->P;
    pawL_init_buffer(P, &print);
    add_location(P, &print, x->modname, x->line);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(x->P, &print, fmt, arg);
    va_end(arg);

    pawL_push_result(P, &print);
    pawC_throw(x->P, PAW_ESYNTAX);
}

static struct Token make_token(TokenKind kind)
{
    return (struct Token){
        .kind = kind,
    };
}

static struct Token make_number(struct Lex *x)
{
    const Value v = x->P->top.p[-1];
    Token t = {.value = v, .kind = TK_INTEGER};
    if (pawV_is_float(v)) {
        t.kind = TK_FLOAT;
    } else if (pawV_is_bigint(t.value)) {
        // Anchor big integers in the strings table so they don't get collected. They
        // will eventually end up anchored in the constants table. Use the value
        // representation as an integer key (contains pointer info, so it is unique).
        Value key;
        pawV_set_int(&key, v.i);
        Value *slot = pawH_action(x->P, x->strings, key, MAP_ACTION_CREATE);
        *slot = t.value;
    }
    pawC_stkdec(x->P, 1);
    return t;
}

static struct Token make_string(struct Lex *x, TokenKind kind)
{
    ParseMemory *pm = x->pm;
    struct Token t = make_token(kind);
    String *s = pawX_scan_string(x, pm->temp, cast_size(pm->tsize));
    pawV_set_string(&t.value, s);
    pm->tsize = 0;
    return t;
}

static char next(struct Lex *x)
{
    paw_Env *P = x->P;
    if (x->nchunk == 0) {
        x->chunk = x->input(P, x->ud, &x->nchunk);
    }
    if (x->nchunk > 0) {
        x->c = x->chunk[0];
        --x->nchunk;
        ++x->chunk;
    } else {
        x->c = (char)TK_END;
    }
    return x->c;
}

static void save(struct Lex *x, char c)
{
    ParseMemory *pm = x->pm;
    pawM_grow(x->P, pm->temp, pm->tsize, pm->talloc);
    pm->temp[pm->tsize++] = c;
}

#define save_and_next(x) (save(x, (x)->c), next(x))
#define is_eof(x) ((uint8_t)(x)->c == TK_END)

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

static struct Token consume_name(struct Lex *x)
{
    save_and_next(x);
    while (ISNAME(x->c) || ISDIGIT(x->c)) {
        save_and_next(x);
    }
    struct Token t = make_string(x, TK_NAME);
    const String *s = pawV_get_string(t.value);
    if (s->flag > 0) {
        // Set the keyword type.
        t.kind = (TokenKind)s->flag;
    }
    if (s->length > PAW_NAME_MAX) {
        pawX_error(x, "name (%I chars) is too long",
                   paw_cast_int(s->length));
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
        const uint8_t c = (uint8_t)x->c;
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
    if (!ISHEX(c[0]) || //
        !ISHEX(c[1]) || //
        !ISHEX(c[2]) || //
        !ISHEX(c[3])) { //
        return -1;
    }
    return HEXVAL(c[0]) << 12 | //
           HEXVAL(c[1]) << 8 |  //
           HEXVAL(c[2]) << 4 |  //
           HEXVAL(c[3]);        //
}

static void increment_line(struct Lex *x)
{
    paw_assert(ISNEWLINE(x->c));
    const char first = x->c;
    if (x->line == INT_MAX) {
        pawX_error(x, "too many lines in module");
    }
    ++x->line;

    next(x);
    if (ISNEWLINE(x->c) && x->c != first) {
        next(x);
    }
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
                    // Codepoint is part of a surrogate pair. Expect a high surrogate
                    // (U+D800–U+DBFF) followed by a low surrogate (U+DC00–U+DFFF).
                    if (codepoint <= 0xDBFF) {
                        if (save_and_next(x) != '\\' ||
                            save_and_next(x) != 'u') {
                            lex_error(x);
                        }
                        const int codepoint2 = get_codepoint(x);
                        if (codepoint2 < 0xDC00 || codepoint2 > 0xDFFF) {
                            lex_error(x);
                        }
                        codepoint = (((codepoint - 0xD800) << 10) | (codepoint2 - 0xDC00)) + 0x10000;
                    } else {
                        lex_error(x);
                    }
                }
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
                lex_error(x);
        }
    } else if (test_next(x, quote)) {
        return make_string(x, TK_STRING);
    } else if (ISNEWLINE(x->c)) {
        save(x, x->c); // newlines allowed in string literals
        increment_line(x);
    } else if (consume_utf8(x)) {
        lex_error(x);
    }
    goto handle_ascii;
}

Token consume_number(struct Lex *x)
{
    // Save source text in a buffer until a byte is reached that cannot possibly
    // be part of a number.
    ParseMemory *pm = x->pm;
    const char first = x->c;
    save_and_next(x);

    // This 'if' statement will allow invalid strings like '.0x0', since we may
    // have already read a '.'. pawV_parse_float() handles those cases. Also note
    // that test_next2() saves the character.
    if (first == '0' && (test_next2(x, "bB") ||
                         test_next2(x, "oO") ||
                         test_next2(x, "xX"))) {
    }

    // Consume adjacent floating-point indicators, exponents, and fractional parts.
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

    // on success, pushes a number onto the stack
    if (pawV_parse_integer(x->P, pm->temp)) {
        if (pawV_parse_float(x->P, pm->temp)) {
            pawX_error(x, "invalid number '%s'", pm->temp);
        }
    }
    return make_number(x);
}

static void skip_block_comment(struct Lex *x)
{
    for (;;) {
        if (test_next(x, '*') && test_next(x, '-')) {
            break;
        } else if (ISNEWLINE(x->c)) {
            increment_line(x);
        } else if (is_eof(x)) {
            pawX_error(x, "missing end of block comment ('*-')");
        } else {
            next(x);
        }
    }
}

static void skip_line_comment(struct Lex *x)
{
    while (!is_eof(x) && !ISNEWLINE(x->c)) {
        next(x);
    }
}

static Token advance(struct Lex *x)
{
#define T(kind) make_token(kind)
    for (;;) {
        x->pm->tsize = 0;
        if (ISDIGIT(x->c)) {
            return consume_number(x);
        } else if (ISNAME(x->c)) {
            return consume_name(x);
        }
        switch (x->c) {
            case '\n':
            case '\r':
                increment_line(x);
                break;
            case ' ':
            case '\f':
            case '\t':
            case '\v':
                next(x);
                break;
            case '\'':
            case '"':
                return consume_string(x);
            case '=':
                next(x);
                if (test_next(x, '=')) {
                    return T(TK_EQUALS2);
                }
                return T('=');
            case '&':
                next(x);
                if (test_next(x, '&')) {
                    return T(TK_AMPER2);
                }
                return T('&');
            case '|':
                next(x);
                if (test_next(x, '|')) {
                    return T(TK_PIPE2);
                }
                return T('|');
            case '?':
                next(x);
                if (test_next(x, '?')) {
                    return T(TK_QUESTION2);
                } else if (test_next(x, ':')) {
                    return T(TK_ELVIS);
                }
                return T('?');
            case ':':
                next(x);
                if (test_next(x, ':')) {
                    return T(TK_COLON2);
                }
                return T(':');
            case '!':
                next(x);
                if (test_next(x, '=')) {
                    return T(TK_BANG_EQ);
                }
                return T('!');
            case '-':
                next(x);
                if (test_next(x, '-')) {
                    skip_line_comment(x);
                    break;
                } else if (test_next(x, '*')) {
                    skip_block_comment(x);
                    break;
                }
                return T('-');
            case '*':
                next(x);
                if (test_next(x, '*')) {
                    return T(TK_STAR2);
                }
                return T('*');
            case '<':
                next(x);
                if (test_next(x, '<')) {
                    return T(TK_LESS2);
                } else if (test_next(x, '=')) {
                    return T(TK_LESS_EQ);
                }
                return T('<');
            case '>':
                next(x);
                if (test_next(x, '>')) {
                    return T(TK_GREATER2);
                } else if (test_next(x, '=')) {
                    return T(TK_GREATER_EQ);
                }
                return T('>');
            case '+':
                next(x);
                if (test_next(x, '+')) {
                    return T(TK_PLUS2);
                }
                return T('+');
            case '/':
                next(x);
                if (test_next(x, '/')) {
                    return T(TK_SLASH2);
                }
                return T('/');
            case '.':
                save_and_next(x); // may be float
                if (test_next(x, '.')) {
                    if (test_next(x, '.')) {
                        return T(TK_DOT3);
                    }
                    lex_error(x); // '..' not allowed
                } else if (ISDIGIT(x->c)) {
                    return consume_number(x);
                }
                return T('.');
            default: {
                // Cast to uint8_t first, so we don't get sign extension when converting
                // to TokenKind. Otherwise, TK_END ends up with the wrong value.
                const uint8_t c = (uint8_t)x->c;
                next(x);
                return T(c);
            }
        }
    }
#undef T
}

TokenKind pawX_next(struct Lex *x)
{
    x->lastline = x->line;
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

void pawX_set_source(Lex *x, paw_Reader input)
{
    paw_Env *P = x->P;
    ParseMemory *pm = x->pm;
    pawM_resize(P, pm->temp, 0, INITIAL_SCRATCH);
    pm->talloc = INITIAL_SCRATCH;

    x->input = input;
    x->line = 1;
    next(x); // load first char
}

String *pawX_scan_string(Lex *x, const char *s, size_t n)
{
    paw_Env *P = x->P;
    const Value *pv = pawC_pushns(P, s, n); // anchor
    Value *value = pawH_action(P, x->strings, *pv, MAP_ACTION_CREATE);
    *value = *pv; // anchor in map
    paw_pop(P, 1);
    check_gc(P);

    return pawV_get_string(*value);
}
