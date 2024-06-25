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
// TODO: rename some of these files: parse.* should maybe be called compile.*, and
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
#define limit_error(x, what, limit) \
    pawX_error(x, "too many %s (limit is %d)", what, limit)

// TODO: Use this to keep track of dynamic memory
typedef union DeferredAlloc DeferredAlloc;

typedef enum DeferredKind {
    DEFER_SCOPE,
} DeferredKind;

#define DEFERRED_HEADER        \
    DeferredAlloc *prev_alloc; \
    DeferredKind alloc_kind

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

// Represents a single lexical scope
typedef struct Scope {
    DEFERRED_HEADER;
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
} SymbolTable;

#define last_scope(t) check_exp((t)->size > 0, (t)->data[(t)->size - 1])
Scope *pawP_new_scope(Lex *lex, SymbolTable *table);
void pawP_add_scope(Lex *lex, SymbolTable *table, Scope *scope);
struct Symbol *pawP_add_symbol(Lex *lex, Scope *table);
int pawP_find_symbol(Scope *scope, const String *name);

typedef struct GenericState {
    struct GenericState *outer;
} GenericState;

typedef struct StructState {
    struct StructState *outer;
    struct StructDecl *struct_;
    struct AstDecl *method;
    struct AstDecl *field;
    int imethod;
    int ifield;
} StructState;

typedef struct BlockState {
    struct BlockState *outer;
    uint8_t is_loop;
    int isymbol;
    int level;
    int label0;
} BlockState;

typedef struct LocalSlot {
    struct Symbol *symbol;
    int index;
} LocalSlot;

typedef struct LocalStack {
    LocalSlot *slots;
    int nslots;
    int capacity;
} LocalStack;

typedef enum FuncKind {
    FUNC_MODULE,
    FUNC_FUNCTION,
    FUNC_METHOD,
} FuncKind;

// TODO: Need to keep track of scopes that get removed from the symbol table and placed in 'scopes' field.
//       Either use GC, or link in a 'defer' list.
typedef struct FuncState {
    struct FuncState *outer; // enclosing function
    struct FuncType *type; // function signature
    struct Generator *G; // codegen state
    SymbolTable scopes; // local scopes
    LocalStack locals; // local variables
    BlockState *bs; // current block
    Proto *proto; // prototype being built
    String *name; // name of the function
    int id; // index in caller's prototype list
    int level; // current stack index (base = 0)
    int ndebug; // number of debug entries
    int nup; // number of upvalues
    int nk; // number of constants
    int nproto; // number of nested functions
    int nstructs; // number of nested structs
    int nlines; // number of source lines
    int pc; // number of instructions
    FuncKind kind; // type of function
} FuncState;

// Unifies structures that require dynamic memory
union DeferredAlloc {
    Scope scope;
};

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
        Binder data[MAX_BINDERS];
        int size;
    } temp;

    struct {
        FuncSig *data;
        int size;
        int alloc;
    } sigs;

    // Operand stack, for linearizing chains of expressions.
    struct {
        struct IrOperand **data;
        int size;
        int alloc;
    } opers;

    struct {
        struct AstDecl **data;
        int size;
        int alloc;
    } decls;

    struct Ast *ast;
    struct Ir *ir;

    DeferredAlloc *defer;
    Unifier unifier;
    SymbolTable symbols;
    LabelList labels;
} ParseMemory;

void pawP_init(paw_Env *P);

Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *mem, const char *name, void *ud);

#endif // PAW_PARSE_H
