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

typedef enum SymbolKind {
    SYM_VARIABLE,
    SYM_PROTOTYPE,
} SymbolKind;

typedef struct Symbol {
    String *name;
    TypeTag type;
    SymbolKind kind;

    union {
        struct {
            paw_Bool init: 1;
            paw_Bool captured: 1;
        } flags;
        Class *cls; 
    };
} Symbol;

typedef struct SymbolTable {
    Symbol *data;
    int size;
    int alloc;
    int bk_depth;
    int fn_depth;
} SymbolTable;

typedef struct ScopeTable {
    SymbolTable **data;
    int size;
    int alloc;
} ScopeTable;

#define last_scope(t) check_exp((t)->size > 0, (t)->data[(t)->size - 1])
SymbolTable *pawP_add_scope(Lex *lex, ScopeTable *table);
Symbol *pawP_add_symbol(Lex *lex, SymbolTable *table);
int pawP_find_symbol(SymbolTable *table, String *name);
#define pawP_get_symbol(table, i) (&(table)->data[i])

typedef struct ClsState {
    struct ClsState *outer;
    TypeTag *tags;
    Class *cls;
    int index;
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
    ScopeTable scopes; // local scopes
    SymbolTable locals; // local variables
    struct FnState *outer; // enclosing function
    TypeTag type; // function signature
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
    int nclass; // number of nested classes
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

    ScopeTable scopes;
    LabelList ll;
    int nglobal;
} ParseMemory;

void pawP_init(paw_Env *P);
Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *mem, const char *name, void *ud);

#endif // PAW_PARSE_H
