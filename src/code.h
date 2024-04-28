// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "opcode.h"
#include "parse.h"
#include "paw.h"

typedef int NodeId;

typedef struct Arena {
    struct Arena *prev;
    size_t used;
    size_t size;

    // Must be aligned to at least the strictest alignment required
    // by an AST or IR node.
    _Alignas(void *) char data[];
} Arena;

typedef struct Pool {
    Arena *filled; // list of filled arenas
    Arena *arena; // list of available arenas
    size_t last_size; // size of last arena allocated
    size_t min_size; // minimum allocation size
} Pool;

void pawK_pool_init(paw_Env *P, Pool *pool, size_t base_size, size_t min_size);
void pawK_pool_uninit(paw_Env *P, Pool *pool);
void *pawK_pool_alloc(paw_Env *P, Pool *pool, size_t size, size_t align);

//****************************************************************
//     Code generation
//****************************************************************

typedef struct Generator {
    Lex *lex; // lexical state
    StructState *cs; // enclosing structure context
    FuncState *fs; // enclosing function context
    SymbolTable *sym; // scoped symbol table
    Scope *globals; // global symbols
    struct Ast *ast; // typed AST
} Generator;

void pawK_fix_line(FuncState *fs, int line);

// Opcode output routines
void pawK_code_0(FuncState *fs, Op op);
void pawK_code_S(FuncState *fs, Op op, int s);
void pawK_code_U(FuncState *fs, Op op, int u);
void pawK_code_AB(FuncState *fs, Op op, int a, int b);

#endif // PAW_CODE_H
