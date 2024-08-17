// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "mem.h"
#include "parse.h"

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

#define K_LIST_MAX (1 << 15)
#define K_LIST_MIN 8

// Generate functions for working with a list containing nodes of a given type
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
        struct L *list = pawK_pool_alloc(X->P, &X->pool, sizeof(struct L)); \
        list->data = pawK_pool_alloc(X->P, &X->pool, size); \
        list->alloc = K_LIST_MIN; \
        return list; \
    } \
    static inline void func##push(ctx *X, struct L *list, T *node) \
    { \
        if (list->count == list->alloc) { \
            if (list->alloc > K_LIST_MAX / 2) { \
                pawM_error(X->P); \
            } \
            const size_t elemsz = sizeof(list->data[0]); \
            const size_t nextcap = CAST_SIZE(list->alloc) * 2; \
            const size_t bufsz = nextcap * elemsz; \
            void *next = pawK_pool_alloc(X->P, &X->pool, bufsz); \
            const size_t usedsz = CAST_SIZE(list->count) * elemsz; \
            memcpy(next, list->data, usedsz); \
            list->alloc = CAST(nextcap, int); \
            list->data = next; \
        } \
        list->data[list->count++] = node; \
    } \
    static inline T *func##get(struct L *list, int index) \
    { \
        paw_assert(index < list->count); \
        return list->data[index]; \
    }

void pawK_fix_line(struct FuncState *fs, int line);

// Opcode output routines
void pawK_code_0(struct FuncState *fs, Op op);
void pawK_code_S(struct FuncState *fs, Op op, int s);
void pawK_code_U(struct FuncState *fs, Op op, int u);
void pawK_code_AB(struct FuncState *fs, Op op, int a, int b);

#endif // PAW_CODE_H
