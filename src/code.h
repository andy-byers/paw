// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "opcode.h"
#include "parse.h"
#include "paw.h"

typedef struct Pool {
    struct Arena *filled;
    struct Arena *arena;
    size_t last_size;
    size_t min_size;
} Pool;

void pawK_pool_init(paw_Env *P, struct Pool *pool, size_t base_size, size_t min_size);
void pawK_pool_uninit(paw_Env *P, struct Pool *pool);
void *pawK_pool_alloc(paw_Env *P, struct Pool *pool, size_t size, size_t align);

// TODO: There was some code that recycled unused lists to save memory.
//       This implementation allocates the lists in an arena. When a list is grown, we allocate a new list
//       and copy the old contents, leaving the old list unused. This may end up wasting a lot of memory 

// Generate declarations for a list containing pointers of a given type
#define DECLARE_LIST(ctx, func, L, T) \
    struct L {                        \
        int count;                    \
        int alloc;                    \
        T *data[];                    \
    };                                \
    struct L *func##new(ctx *x);                        \
    void func##push(ctx *x, struct L **plist, T *node); \
    static T *func##get(struct L *list, int index) \
    {                                              \
        paw_assert(index < list->count);           \
        return list->data[index];                  \
    }

// Generate code for a list declared using DECLARE_LIST
#define DEFINE_LIST(ctx, minsz, func, L, T)                                             \
    static struct L *func##new_list_(ctx *X, int alloc)                          \
    {                                                                                   \
        pawM_check_size(X->P, sizeof(struct L), cast_size(alloc), sizeof(void *));      \
        const size_t size = sizeof(struct L) + cast_size(alloc) * sizeof(void *);       \
        struct L *list = pawK_pool_alloc(X->P, &X->large, size, paw_alignof(struct L)); \
        list->alloc = alloc;                                                            \
        return list;                                                                    \
    }                                                                                   \
    struct L *func##new(ctx *X)                                                  \
    {                                                                                   \
        struct L *list = func##new_list_(X, minsz);                                     \
        pawM_check_size(X->P, sizeof(struct L), minsz, sizeof(void *));                 \
        const size_t size = sizeof(struct L) + minsz * sizeof(void *);                  \
        list->alloc = minsz;                                                            \
        return list;                                                                    \
    }                                                                                   \
    void func##push(ctx *X, struct L **plist, T *node)                  \
    {                                                                           \
        struct L *list = *plist;                                                \
        if (list->count == list->alloc) {                                       \
            if (list->alloc > INT_MAX / 2) {                                    \
                pawM_error(X->P);                                               \
            }                                                                   \
            struct L *next = func##new_list_(X, list->alloc * 2);               \
            const size_t size = cast_size(list->count) * sizeof(list->data[0]); \
            memcpy(next->data, list->data, size);                               \
            next->count = list->count;                                          \
            list = next;                                                        \
        }                                                                       \
        list->data[list->count++] = node;                                       \
        *plist = list;                                                          \
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
