// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// Compilation phases:
//
//  Pass | Target | Purpose
// ------|--------|-----------------------------
//  1    | code   | build ast, register symbols
//  2    | tree   | check types
//  3    | tree   | generate code
//
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

// typedef struct ClsState {
//     struct ClsState *outer;
//     paw_Bool has_super;
// } ClsState;

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

#define fn_has_self(kind) (kind >= FN_METHOD)

typedef struct VarList {
    VarDesc *data;
    int size;
    int alloc;
} VarList;

typedef struct ParseMemory {
    // Buffer for accumulating strings
    struct Scratch {
        char *data;
        int size;
        int alloc;
    } scratch;

    // Buffers for holding variables
    VarList locals;
    VarList globals;

    LabelList ll;
} ParseMemory;

typedef struct FnState {
    struct FnState *caller; // FnState of the caller
    BlkState *blk; // current lexical block
    Lex *lex; // pointer back to the lexical state
    String *name; // name of the function
    struct NodeVec *param; // AST nodes for parameters
    TypeTag ret; // return type annotation
    Proto *proto; // prototype being built
    int base; // base stack index
    int level; // current stack index
    int ndebug; // number of debug entries
    int nup; // number of upvalues
    int nk; // number of constants
    int nproto; // number of nested functions
    int nlines; // number of source lines
    int pc; // number of instructions
    enum FnKind kind; // type of function
} FnState;

void pawP_init(paw_Env *P);
Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *mem, const char *name, void *ud);

#endif // PAW_PARSE_H
