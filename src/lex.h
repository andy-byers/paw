// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_LEX_H
#define PAW_LEX_H

#include "compile.h"
#include "value.h"

#define FIRST_KEYWORD TK_PUB
#define IS_KEYWORD(s) ((s)->flag > 0)
#define IS_BUILTIN(s) ((s)->flag < 0)
#define FLAG2CODE(x) (-(x) - 1)

void pawX_read_integer(paw_Env *P, char const *data, int base);
void pawX_read_float(paw_Env *P, char const *data);

enum MultiChar {
    // Control tokens:
    TK_END = 0,
    TK_NONE = 256,

    // Multi-byte tokens:
    TK_PLUS2,
    TK_DOT2,
    TK_DOT3,
    TK_DOT2_EQ,
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
    TK_PLUS_EQ,
    TK_MINUS_EQ,
    TK_STAR_EQ,
    TK_SLASH_EQ,
    TK_PERCENT_EQ,
    TK_CARET_EQ,
    TK_AMPER_EQ,
    TK_PIPE_EQ,
    TK_LESS2_EQ,
    TK_GREATER2_EQ,
    TK_HASH_BRACKET,
    TK_HASHBANG,

    // Variables and literals:
    TK_NAME,
    TK_CHAR,
    TK_INT,
    TK_FLOAT,
    TK_STRING_TEXT,
    TK_STRING_EXPR_OPEN,
    TK_STRING_EXPR_CLOSE,

    // Keywords (must be in this order):
    TK_PUB,
    TK_USE,
    TK_FN,
    TK_TYPE,
    TK_ENUM,
    TK_STRUCT,
    TK_TRAIT,
    TK_CONST,
    TK_INLINE,
    TK_LET,
    TK_IF,
    TK_ELSE,
    TK_LOOP,
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
    TK_UNDERSCORE,
};

typedef unsigned TokenKind;

enum TokenFlag {
    TF_UNICODE = 1 << 0,
};

struct Token {
    struct SourceSpan span;
    unsigned flags;
    TokenKind kind;
    Value value;
};

struct Lex {
    struct Pool *pool;
    struct Compiler *C;
    paw_Env *P;

    int modno;
    Str *modname;
    Tuple *strings;
    struct Ast *ast;

    paw_Reader input;
    char const *ptr;
    char const *end;

    // state for handling string interpolation
    struct StateStack *states;
    struct IntStack *parens;

    struct DynamicMem *dm;
    struct Token t0, t, t2;

    void *ud;

    struct SourceLoc loc;
    struct SourceLoc last_loc;

    int fn_depth;
    int loop_depth;
    int expr_depth;
    int nest_depth;

    paw_Bool in_impl;
};

enum StringState {
    STATE_NORMAL,
    STATE_STRING,
};

DEFINE_LIST(struct Lex, StateStack, enum StringState)
DEFINE_LIST(struct Lex, IntStack, int)

Str *pawX_scan_str(struct Lex *lex, char const *s, size_t n);
void pawX_set_source(struct Lex *lex, paw_Reader input, void *ud);
TokenKind pawX_next(struct Lex *lex);
TokenKind pawX_peek(struct Lex *lex);

#endif // PAW_LEX_H
