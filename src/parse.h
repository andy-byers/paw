// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_PARSE_H
#define PAW_PARSE_H

#include "unify.h"

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
    int level;
    int label0;
};

struct LocalSlot {
    struct HirType *type;
    String *name;
    int index;
    paw_Bool is_captured : 1;
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
    struct BlockState *bs; // current block
    Proto *proto; // prototype being built
    String *name; // name of the function
    int first_local; // index of function in DynamicMem array
    int nlocals; // number of locals
    int nup; // number of upvalues
    int nk; // number of constants
    int nproto; // number of nested functions
    int nlines; // number of source lines
    int pc; // number of instructions
    int line;        
    enum FuncKind kind; // type of function
};

// Keeps track of dynamic memory used by the compiler
struct DynamicMem {
    // Buffer for accumulating strings
    struct CharVec {
        char *data;
        int count;
        int alloc;
    } scratch;

    struct {
        struct HirDecl **data;
        int count;
        int alloc;
    } decls;

    struct {
        struct LocalSlot *data;
        int count;
        int alloc;
    } vars;

    struct Ast *ast;
    struct Hir *hir;

    Unifier unifier;
    struct LabelList labels;
};

void pawP_init(paw_Env *P);

#endif // PAW_PARSE_H
