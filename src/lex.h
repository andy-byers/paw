// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_LEX_H
#define PAW_LEX_H

#include "value.h"

#define FIRST_KEYWORD TK_FN
#define IS_KEYWORD(k) (k >= FIRST_KEYWORD)

void pawX_read_integer(paw_Env *P, const char *data, int base);
void pawX_read_float(paw_Env *P, const char *data);

enum MultiChar {
    TK_NONE = 0,
    TK_END = 255,

    TK_DOT3 = 128,
    TK_PLUS2,
    TK_QUESTION2,
    TK_COLON2,
    TK_ELVIS,
    TK_SLASH2,
    TK_STAR2,
    TK_LESS2,
    TK_GREATER2,
    TK_AMPER2,
    TK_EQUALS2,
    TK_PIPE2,
    TK_LESS_EQ,
    TK_GREATER_EQ,
    TK_BANG_EQ,

    // Variables and literals
    TK_NAME,
    TK_STRING,
    TK_INTEGER,
    TK_FLOAT,

    // Keywords (must be in this order)
    TK_FN,
    TK_CLASS,
    TK_SUPER,
    TK_INCLUDE,
    TK_LET,
    TK_IF,
    TK_ELSE,
    TK_FOR,
    TK_DO,
    TK_WHILE,
    TK_BREAK,
    TK_CONTINUE,
    TK_RETURN,
    TK_IN,
    TK_TRUE,
    TK_FALSE,
    TK_NULL,
};

typedef uint8_t TokenKind;

typedef struct Token {
    TokenKind kind;
    Value value;
} Token;

typedef struct Lex {
    paw_Env *P;
    struct FnState *fn;
    struct ClsState *cls;

    Map *strings;
    String *modname;

    paw_Reader input;
    const char *chunk;
    size_t nchunk;
    char c;

    struct ParseMemory *pm;

    // Current token and 1 lookahead
    Token t;
    Token t2;

    int line;
    int lastline;

    void *ud;
} Lex;

String *pawX_scan_string(Lex *lex, const char *s, size_t n);
void pawX_set_source(Lex *lex, paw_Reader input);
TokenKind pawX_next(Lex *lex);
TokenKind pawX_peek(Lex *lex);

void pawX_error(Lex *lex, const char *fmt, ...);

#endif // PAW_LEX_H
