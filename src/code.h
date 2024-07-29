// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "compile.h"
#include "opcode.h"
#include "mem.h"
#include "parse.h"
#include "paw.h"

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
    struct L {                       \
        K_ALIGNAS_NODE int count;    \
        int alloc;                   \
        T *data[];                   \
    };                               \
    _Static_assert(sizeof(struct L) <= sizeof(struct FreeBlock),                    \
                   "free block too small for list");                                \
    _Static_assert(_Alignof(struct L) == K_ALIGNOF_NODE,                            \
                   "list is improperly aligned");                                   \
    _Static_assert(K_LIST_MAX < (PAW_SIZE_MAX - sizeof(struct L)) / sizeof(void *), \
                   "maximum list larger than PAW_SIZE_MAX");                        \
    static inline struct L *func##new(ctx *X)                                          \
    {                                                                                  \
        pawM_check_size(X->P, sizeof(struct L), K_LIST_MIN, sizeof(void *));           \
        const size_t size = sizeof(struct L) + cast_size(K_LIST_MIN) * sizeof(void *); \
        struct L *list = pawK_pool_alloc(X->P, &X->pool, size);                        \
        list->alloc = K_LIST_MIN;                                                      \
        return list;                                                                   \
    }                                                                                  \
    static inline void func##free(ctx *X, struct L *list)                    \
    {                                                                        \
        const size_t bufsz = cast_size(list->alloc) * sizeof(list->data[0]); \
        pawK_pool_free(&X->pool, list, sizeof(*list) + bufsz);               \
    }                                                                        \
    static inline void func##push(ctx *X, struct L **plist, T *node)                    \
    {                                                                                   \
        struct L *list = *plist;                                                        \
        if (list->count == list->alloc) {                                               \
            if (list->alloc > K_LIST_MAX / 2) {                                         \
                pawM_error(X->P);                                                       \
            }                                                                           \
            const size_t elemsz = sizeof(list->data[0]);                                \
            const size_t nextcap = cast_size(list->alloc) * 2;                          \
            const size_t bufsz = nextcap * elemsz;                                      \
            struct L *next = pawK_pool_alloc(X->P, &X->pool, sizeof(struct L) + bufsz); \
            const size_t usedsz = cast_size(list->count) * elemsz;                      \
            memcpy(next->data, list->data, usedsz);                                     \
            next->count = list->count;                                                  \
            next->alloc = nextcap;                                                      \
            pawK_pool_free(&X->pool, list, sizeof(struct L) + bufsz);                   \
            list = next;                                                                \
        }                                                                               \
        list->data[list->count++] = node;                                               \
        *plist = list;                                                                  \
    }                                                                                   \
    static inline T *func##get(struct L *list, int index) \
    {                                                     \
        paw_assert(index < list->count);                  \
        return list->data[index];                         \
    }

typedef struct Generator {
    struct Compiler *C;
    struct HirSymtab *sym;
    struct HirScope *globals;
    struct Hir *hir;
    struct FuncState *fs;
    paw_Env *P;
    int iglobal;
} Generator;

void pawK_fix_line(struct FuncState *fs, int line);

// Opcode output routines
void pawK_code_0(struct FuncState *fs, Op op);
void pawK_code_S(struct FuncState *fs, Op op, int s);
void pawK_code_U(struct FuncState *fs, Op op, int u);
void pawK_code_AB(struct FuncState *fs, Op op, int a, int b);

#endif // PAW_CODE_H
