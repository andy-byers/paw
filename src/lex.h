// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_LEX_H
#define PAW_LEX_H

#include "value.h"

#define FIRST_KEYWORD TK_PUB
#define IS_KEYWORD(s) ((s)->flag > 0)
#define IS_BUILTIN(s) ((s)->flag < 0)
#define FLAG2CODE(x) (-(x) - 1)

void pawX_read_integer(paw_Env *P, const char *data, int base);
void pawX_read_float(paw_Env *P, const char *data);

enum MultiChar {
    // Control tokens:
    TK_NONE = 0,
    TK_END = 255,

    // Multi-byte tokens:
    TK_PLUS2,
    TK_DOT3,
    TK_COLON2,
    TK_LESS2,
    TK_GREATER2,
    TK_AMPER2,
    TK_EQUALS2,
    TK_PIPE2,
    TK_ARROW,
    TK_FAT_ARROW,
    TK_LESS_EQ,
    TK_GREATER_EQ,
    TK_BANG_EQ,

    // Variables and literals:
    TK_NAME,
    TK_STRING,
    TK_INTEGER,
    TK_FLOAT,

    // Keywords (must be in this order):
    TK_PUB,
    TK_USE,
    TK_FN,
    TK_TYPE,
    TK_ENUM,
    TK_STRUCT,
    TK_TRAIT,
    TK_LET,
    TK_IF,
    TK_ELSE,
    TK_FOR,
    TK_WHILE,
    TK_MATCH,
    TK_BREAK,
    TK_CONTINUE,
    TK_RETURN,
    TK_IN,
    TK_AS,
    TK_TRUE,
    TK_FALSE,

    TK_LIMIT // must be last
};

typedef unsigned TokenKind;

struct Token {
    TokenKind kind;
    Value value;
};

struct Lex {
    struct Compiler *C;
    paw_Env *P;

    Tuple *strings;
    String *modname;
    Closure *main;
    struct Ast *ast;

    paw_Reader input;
    const char *chunk;
    size_t nchunk;
    char c;

    struct DynamicMem *dm;

    // Current token and 1 lookahead
    struct Token t;
    struct Token t2;

    void *ud;

    int line;
    int last_line;
    int expr_depth;
    int nest_depth;

    // When 'in_impl == true', the parameter at 'param_index == 0' may omit the
    // type annotation, given that it is named 'self'. 'self: Self' is implied.
    int param_index;
    paw_Bool in_impl;
};

String *pawX_scan_string(struct Lex *lex, const char *s, size_t n);
void pawX_set_source(struct Lex *lex, paw_Reader input, void *ud);
TokenKind pawX_next(struct Lex *lex);
TokenKind pawX_peek(struct Lex *lex);

_Noreturn void pawX_error(struct Lex *lex, const char *fmt, ...);

#endif // PAW_LEX_H
