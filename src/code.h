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
#define DEFINE_LIST_V2(ctx, func, L, T) \
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
    }

// Macros for working with a list
#define K_LIST_FIRST(L) (K_LIST_GET(L, 0))
#define K_LIST_LAST(L) (K_LIST_GET(L, (L)->count - 1))
#define K_LIST_GET(L, i) ((L)->data[i])
#define K_LIST_SET(L, i, v) ((L)->data[i] = (v))
#define K_LIST_POP(L) CHECK_EXP((L)->count > 0, --(L)->count)
#define K_LIST_PUSH(C, L, v) ((L)->data = pawK_list_ensure_one_((C)->P, (C)->pool, (L)->data, sizeof((L)->data[0]), (L)->count, &(L)->alloc), \
                              (L)->data[(L)->count++] = (v))
#define K_LIST_INSERT(C, L, v, i) ((L)->data = pawK_list_ensure_one_((C)->P, (C)->pool, (L)->data, sizeof((L)->data[0]), (L)->count, &(L)->alloc), \
                                   memmove((L)->data + (i) + 1, (L)->data + (i), ((L)->count - (i)) * sizeof((L)->data[0])), \
                                   (L)->data[i] = (v))

// TODO: remove these 2, test K_LIST_INSERT
#define K_LIST_APPEND(C, L, v) ((L)->data = pawK_list_ensure_one(C, (L)->data, (L)->count, &(L)->alloc), \
                                (L)->data[(L)->count++] = (v))
#define K_LIST_PREPEND(C, L, v) ((L)->data = pawK_list_ensure_one(C, (L)->data, (L)->count, &(L)->alloc), \
                                 memmove((L)->data + 1, (L)->data, (L)->count * sizeof((L)->data[0])), \
                                 (L)->data[0] = (v))
// TODO: remove
#define DEFINE_LIST(ctx, func, L, T) \
    struct L { \
        K_ALIGNAS_NODE T **data; \
        int count; \
        int alloc; \
    }; \
    _Static_assert(K_LIST_MAX < PAW_SIZE_MAX / sizeof(void *), \
                   "maximum list is too large"); \
    static inline struct L *func##new(ctx *X) \
    { \
        pawM_check_size(X->P, 0, K_LIST_MIN, sizeof(void *)); \
        const size_t size = CAST_SIZE(K_LIST_MIN) * sizeof(void *); \
        struct L *list = pawK_pool_alloc(X->P, X->pool, sizeof(struct L)); \
        list->data = pawK_pool_alloc(X->P, X->pool, size); \
        list->alloc = K_LIST_MIN; \
        return list; \
    } \
    static inline void func##push(ctx *X, struct L *list, T *node) \
    { \
        if (list->count == list->alloc) { \
            if (list->alloc > K_LIST_MAX / 2) pawM_error(X->P); \
            const size_t nextcap = CAST_SIZE(list->alloc) * 2; \
            void *next = pawK_pool_alloc(X->P, X->pool, nextcap * sizeof(list->data[0])); \
            memcpy(next, list->data, CAST_SIZE(list->count) * sizeof(list->data[0])); \
            list->alloc = CAST(int, nextcap); \
            list->data = next; \
        } \
        list->data[list->count++] = node; \
    } \
    static inline T *func##get(struct L *list, int index) \
    { \
        paw_assert(index < list->count); \
        return list->data[index]; \
    }

void *pawK_list_ensure_one(struct Compiler *C, void *data, int count, int *palloc); // TODO: remove
void *pawK_list_ensure_one_(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc);

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
    struct FuncType *type; // function signature
    struct Generator *G; // codegen state
    struct RegisterTable *regtab;
    struct HirSymtab *scopes; // local scopes
    struct BlockState *bs; // current block
    struct KCache kcache;
    struct MirBlock *bb;
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
    int nstack;
    int free_reg;
    enum FuncKind kind; // type of function
};

void pawK_fix_line(struct FuncState *fs, int line);

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
