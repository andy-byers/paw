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
#include "type.h"
#include "util.h"
#include "value.h"

#define env(x) ((x)->P)
#define is_toplevel(lex) ((lex)->fs->outer == NULL)
#define scan_string(lex, s) pawX_scan_string(lex, s, strlen(s))
#define limit_error(x, what, limit) \
    pawX_error(x, "too many %s (limit is %d)", what, limit)

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

// Represents a single lexical scope
typedef struct Scope {
    struct Symbol **symbols;
    int nsymbols;
    int capacity;
    int bk_depth;
    int fn_depth;
} Scope;

typedef struct SymbolTable {
    Scope *toplevel;
    Scope *globals;
    Scope **scopes;
    int nscopes;
    int capacity;

    Type base_types[PAW_NTYPES];
} SymbolTable;

#define last_scope(t) check_exp((t)->size > 0, (t)->data[(t)->size - 1])
Scope *pawP_add_scope(Lex *lex, SymbolTable *table);
struct Symbol *pawP_add_symbol(Lex *lex, Scope *table);
int pawP_find_symbol(Scope *scope, const String *name);

typedef struct ClsState {
    struct ClsState *outer;
} ClsState;

typedef struct BlkState {
    struct BlkState *outer;
    struct Block *bk; // AST representation
    uint8_t is_loop;
    int level;
    int label0;
} BlkState;

typedef enum FnKind {
    FN_MODULE,
    FN_FUNCTION,
    FN_METHOD,
    FN_INIT,
} FnKind;

typedef struct FnState {
    struct FnState *outer; // enclosing function
    SymbolTable scopes; // local scopes
    Scope locals; // local variables
    FunctionType *sig; // function signature
    BlkState *bs; // current block
    Lex *lex; // lexical state
    Proto *proto; // prototype being built
    String *name; // name of the function
    int id; // index in caller's prototype list
    int level; // current stack index (base = 0)
    int ndebug; // number of debug entries
    int nup; // number of upvalues
    int nk; // number of constants
    int nproto; // number of nested functions
    int nclasses; // number of nested classes
    int nlines; // number of source lines
    int pc; // number of instructions
    FnKind kind; // type of function
} FnState;

#define fn_has_self(kind) (kind >= FN_METHOD)

typedef struct ParseMemory {
    // Buffer for accumulating strings
    struct Scratch {
        char *data;
        int size;
        int alloc;
    } scratch;

    SymbolTable st;
    LabelList ll;
} ParseMemory;

void pawP_init(paw_Env *P);

Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *mem, const char *name, void *ud);

#endif // PAW_PARSE_H
