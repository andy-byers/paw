// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_PARSE_H
#define PAW_PARSE_H

#include "lex.h"
#include "type.h"
#include "unify.h"
#include "util.h"
#include "value.h"

#define is_toplevel(x) ((x)->fs->outer == NULL)
#define limit_error(x, what, limit) pawX_error(x, "too many %s (limit is %d)", what, limit)
#define ENV(x) ((x)->P)
#define SCAN_STRING(x, s) pawP_scan_string(ENV(x), (x)->strings, s)

enum LabelKind {
    LBREAK,
    LCONTINUE,
};

struct Label {
    int pc;
    int line;
    int level;
    int close;
    enum LabelKind kind;
};

struct LabelList {
    struct Label *values;
    int length;
    int capacity;
};

struct BlockState {
    struct BlockState *outer;
    paw_Bool is_loop : 1;
    int isymbol;
    int level;
    int label0;
};

struct LocalSlot {
    struct HirSymbol *symbol;
    int index;
    paw_Bool is_captured : 1;
};

struct LocalStack {
    struct LocalSlot *slots;
    int nslots;
    int capacity;
};

enum FuncKind {
    FUNC_MODULE,
    FUNC_CLOSURE,
    FUNC_FUNCTION,
    FUNC_METHOD,
};

struct FuncState {
    struct FuncState *outer; // enclosing function
    struct FuncType *type; // function signature
    struct Generator *G; // codegen state
    struct HirSymtab *scopes; // local scopes
    struct LocalStack locals; // local variables
    struct BlockState *bs; // current block
    Proto *proto; // prototype being built
    String *name; // name of the function
    int level; // current stack index (base = 0)
    int nup; // number of upvalues
    int nk; // number of constants
    int nproto; // number of nested functions
    int nlines; // number of source lines
    int pc; // number of instructions
    enum FuncKind kind; // type of function
};

// Keeps track of dynamic memory used by the compiler
struct DynamicMem {
    // Buffer for accumulating strings
    struct CharVec {
        char *data;
        int size;
        int alloc;
    } scratch;

    struct {
        struct HirDecl **data;
        int size;
        int alloc;
    } decls;

    struct Ast *ast;
    struct Hir *hir;

    Unifier unifier;
    struct LabelList labels;
};

void pawP_init(paw_Env *P);

Closure *pawP_parse(paw_Env *P, paw_Reader input, struct DynamicMem *mem,
                    const char *name, void *ud);

#endif // PAW_PARSE_H
