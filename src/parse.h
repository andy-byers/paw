// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// parse.h: compiler entrypoint
//
// The compiler converts source code into bytecode that can be run in paw's
// virtual machine. It works in 3 passes:
//
//  Pass | Input       | Output    | Purpose
// ------|-------------|-----------|---------------------------
//  1    | source code | AST       | build AST
//  2    | AST         | typed AST | build symtab, unify types
//  3    | typed AST   | bytecode  | generate code
//
// TODO: rename some of these files: parse.* should maybe be called compile.*,
// and
//       it would be nice to have a separate AST module.

#ifndef PAW_PARSE_H
#define PAW_PARSE_H

#include "lex.h"
#include "type.h"
#include "unify.h"
#include "util.h"
#include "value.h"

#define env(x) ((x)->P)
#define is_toplevel(lex) ((lex)->fs->outer == NULL)
#define scan_string(lex, s) pawX_scan_string(lex, s, strlen(s))
#define limit_error(x, what, limit)                                            \
    pawX_error(x, "too many %s (limit is %d)", what, limit)

typedef enum LabelKind {
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

struct MatchState {
    struct MatchState *outer;
    struct MatchExpr *match;
    AstType *target;
    AstType *value;
};

typedef struct GenericState {
    struct GenericState *outer;
} GenericState;

typedef struct BlockState {
    struct BlockState *outer;
    paw_Bool is_loop: 1;
    int isymbol;
    int level;
    int label0;
} BlockState;

typedef struct LocalSlot {
    struct Symbol *symbol;
    int index;
    paw_Bool is_captured: 1;
} LocalSlot;

typedef struct LocalStack {
    LocalSlot *slots;
    int nslots;
    int capacity;
} LocalStack;

typedef enum FuncKind {
    FUNC_MODULE,
    FUNC_CLOSURE,
    FUNC_FUNCTION,
    FUNC_METHOD,
} FuncKind;

typedef struct FuncState {
    struct FuncState *outer; // enclosing function
    struct FuncType *type; // function signature
    struct Generator *G; // codegen state
    struct SymbolTable *scopes; // local scopes
    LocalStack locals; // local variables
    BlockState *bs; // current block
    Proto *proto; // prototype being built
    String *name; // name of the function
    int level; // current stack index (base = 0)
    int nup; // number of upvalues
    int nk; // number of constants
    int nproto; // number of nested functions
    int nstructs; // number of nested structs
    int nlines; // number of source lines
    int pc; // number of instructions
    FuncKind kind; // type of function
} FuncState;

typedef struct UniTable {
    struct UniTable *outer;
    struct UniVar **vars; // vector of type variables
    int nvars; // number of type variables
    int capacity; // capacity of vector
    int depth; // depth of binder
} UniTable;

#define fn_has_self(kind) (kind >= FUNC_METHOD)

#define MAX_BINDERS 512

// Keeps track of dynamic memory used by the compiler
typedef struct ParseMemory {
    // Buffer for accumulating strings
    struct CharVec {
        char *data;
        int size;
        int alloc;
    } scratch;

    struct {
        struct AstDecl **data;
        int size;
        int alloc;
    } decls;

    struct Ast *ast;

    Unifier unifier;
    LabelList labels;
} ParseMemory;

void pawP_init(paw_Env *P);

Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *mem,
                    const char *name, void *ud);

#endif // PAW_PARSE_H
