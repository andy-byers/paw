// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_PARSE_H
#define PAW_PARSE_H

#include "lex.h"
#include "util.h"
#include "value.h"

typedef enum LabeKind {
    LBREAK,
    LCONTINUE,
} LabelKind;

typedef struct Label {
    int pc;
    int line;
    int level;
    int close;
    LabelKind kind;
} Label;

typedef struct LabelList {
    Label *values;
    int length;
    int capacity;
} LabelList;

typedef enum ExprKind {
    EXPR_CONST,
    EXPR_NULL,
    EXPR_TRUE,
    EXPR_FALSE,
    EXPR_FLOAT,
    EXPR_INTEGER,
    EXPR_STRING,
    EXPR_ARRAY,
    EXPR_MAP,
    EXPR_GLOBAL,
    EXPR_LOCAL,
    EXPR_UPVALUE,
    EXPR_ITEM,
    EXPR_SLICE,
    EXPR_XSLICE,
    EXPR_ATTR,
    EXPR_STACK,
    EXPR_CALL,
    EXPR_COUNT //
} ExprKind;

typedef struct ExprState {
    ExprKind kind;
    int index;
    String *name;
    union {
        paw_Int i;
        paw_Float f;
        String *s;
        Value v;
    };
} ExprState;

typedef struct ClsState {
    struct ClsState *outer;
    paw_Bool has_super;
} ClsState;

// Node in a list of active code blocks
typedef struct BlkState {
    struct BlkState *outer;
    int level;
    int label0;
    paw_Bool is_loop;
} BlkState;

typedef enum FnKind {
    FN_MODULE,
    FN_FUNCTION,
    FN_METHOD,
    FN_INIT,
} FnKind;

#define FN_HAS_SELF(kind) (kind >= FN_METHOD)

typedef struct VarState {
    String *name;
} VarState;

typedef struct ParseMemory {
    // Buffer for accumulating strings
    char *temp;
    int tsize;
    int talloc;

    // Buffer for holding local variables
    VarState *vars;
    int vsize;
    int valloc;

    LabelList ll;
} ParseMemory;

typedef struct FnState {
    struct FnState *caller;
    BlkState *blk;
    Lex *lex;
    Proto *proto;
    String *name;
    int base;
    int level;
    int ndebug;
    int nup;
    int nk;
    int nproto;
    int nlines;
    int pc;
    enum FnKind kind;
} FnState;

void pawP_init(paw_Env *P);
Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *mem, const char *name, void *ud);

#endif // PAW_PARSE_H
