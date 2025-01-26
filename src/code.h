// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "mem.h"

struct Compiler;

#define K_ALIGNOF_NODE _Alignof(void *)
#define K_ALIGNAS_NODE _Alignas(void *)

struct FreeBlock {
    K_ALIGNAS_NODE struct FreeBlock *prev;
    int size;
    char data[];
};

typedef struct Pool {
    struct FreeBlock *free;
    struct Arena *filled;
    struct Arena *arena;
    size_t last_size;
    size_t min_size;
} Pool;

void pawK_pool_init(paw_Env *P, struct Pool *pool, size_t base_size, size_t min_size);
void pawK_pool_uninit(paw_Env *P, struct Pool *pool);
void *pawK_pool_alloc(paw_Env *P, struct Pool *pool, size_t size);
void pawK_pool_free(struct Pool *pool, void *ptr, size_t size);

#define K_LIST_MIN (1 << 3)
#define K_LIST_MAX (1 << 15)

// Generate a structure type and constructor for a list containing nodes of a
// given type T. T can be any type, so long as it is "trivially copiable".
#define DEFINE_LIST(ctx, func, L, T) \
    struct L { \
        T *data; \
        int count; \
        int alloc; \
    }; \
    _Static_assert(K_LIST_MAX < PAW_SIZE_MAX / sizeof(T), \
                   "maximum list is too large"); \
    static inline struct L *func##new(ctx *X) \
    { \
        pawM_check_size(X->P, 0, K_LIST_MIN, sizeof(T)); \
        return pawK_pool_alloc(X->P, X->pool, sizeof(struct L)); \
    } \
    static inline void func##delete(ctx *X, struct L *list) \
    { \
        pawK_pool_free(X->pool, list->data, CAST_SIZE(list->alloc) * sizeof(list->data[0])); \
        pawK_pool_free(X->pool, list, sizeof(struct L)); \
    }

// Macros for working with a list
#define K_LIST_FIRST(L) (K_LIST_GET(L, 0))
#define K_LIST_LAST(L) (K_LIST_GET(L, (L)->count - 1))
#define K_LIST_GET(L, i) ((L)->data[i])
#define K_LIST_SET(L, i, v) CHECK_EXP(0 <= (i) && (i) < (L)->count, (L)->data[i] = (v))
#define K_LIST_POP(L) CHECK_EXP((L)->count > 0, --(L)->count)
#define K_LIST_PUSH(C, L, v) ((L)->data = pawK_list_ensure_one((C)->P, (C)->pool, (L)->data, sizeof((L)->data[0]), (L)->count, &(L)->alloc), \
                              (L)->data[(L)->count++] = (v))
#define K_LIST_INSERT(C, L, v, i) CHECK_EXP(0 <= (i) && (i) <= (L)->count, \
                                            (L)->data = pawK_list_ensure_one((C)->P, (C)->pool, (L)->data, sizeof((L)->data[0]), (L)->count, &(L)->alloc), \
                                            memmove((L)->data + (i) + 1, (L)->data + (i), ((L)->count - (i)) * sizeof((L)->data[0])), \
                                            (L)->data[i] = (v))
#define K_LIST_RESERVE(C, L, n) ((L)->data = pawK_list_reserve((C)->P, (C)->pool, (L)->data, sizeof((L)->data[0]), (L)->count, &(L)->alloc, n))

#define K_LIST_FOREACH(L, p) for (p = (L)->data; p && p < (L)->data + (L)->count; ++p)
#define K_LIST_ENUMERATE(L, i, p) for (i = 0, p = (L)->data; i < (L)->count; p = &(L)->data[++i])
#define K_LIST_ZIP(A, a, B, b) for (a = (A)->data, b = (B)->data; a && b && a < (A)->data + (A)->count && b < (B)->data + (B)->count; ++a, ++b)

void *pawK_list_reserve(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc, int target);
void *pawK_list_ensure_one(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc);

enum FuncKind {
    FUNC_MODULE,
    FUNC_CLOSURE,
    FUNC_FUNCTION,
    FUNC_METHOD,
};

struct KCache {
    Map *ints;
    Map *strs;
    Map *flts;
};

struct FuncState {
    struct FuncState *outer; // enclosing function
    struct RegisterTable *regtab;
    struct Generator *G; // codegen state
    struct KCache kcache;
    struct Mir *mir;
    int bb; // TODO: should be MirBlock, move stuff around
    Proto *proto; // prototype being built
    String *name; // name of the function
    int first_local; // index of function in DynamicMem array
    int nlocals; // number of locals
    int nk; // number of constants
    int nproto; // number of nested functions
    int nlines; // number of source lines
    int pc; // number of instructions
    int line;
    int free_reg;
    enum FuncKind kind; // type of function
};

// Opcode output routines
void pawK_code_0(struct FuncState *fs, Op op);
void pawK_code_ABx(struct FuncState *fs, Op op, int a, int bc);
void pawK_code_ABC(struct FuncState *fs, Op op, int a, int b, int c);

static inline void pawK_code_AsBx(struct FuncState *fs, Op op, int a, int bc)
{
    pawK_code_ABx(fs, op, a, bc + sBx_MAX);
}

static inline void pawK_code_sBx(struct FuncState *fs, Op op, int bc)
{
    pawK_code_AsBx(fs, op, 0, bc);
}

static void pawK_code_AB(struct FuncState *fs, Op op, int a, int b)
{
    pawK_code_ABC(fs, op, a, b, 0);
}

static void pawK_code_A(struct FuncState *fs, Op op, int a)
{
    pawK_code_AB(fs, op, a, 0);
}

#endif // PAW_CODE_H
