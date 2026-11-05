// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_COMPILER_POOL_H
#define PAW_COMPILER_POOL_H

#include "core.h"

struct FreeBlock;

struct PoolStats {
    struct Statistic *num_alloc;
    struct Statistic *bytes_alloc;
    struct Statistic *bytes_used;
};

struct Pool {
    paw_Env *P;
    struct Pool *prev;
    struct Pool *next;
    struct FreeBlock *free;
    struct Arena *arena;
    struct Arena *full;

    // memory usage statistics
    struct PoolStats st;
};

EXTERN_C void pool_init(paw_Env *P, struct Pool *pool, size_t base_size, struct PoolStats st);
EXTERN_C void pool_uninit(struct Pool *pool);

EXTERN_C void *pool_alloc_(struct Pool *pool, void *ptr, size_t size0, size_t size);

static void *pool_alloc(void *ctx, void *ptr, size_t size0, size_t size)
{
    return pool_alloc_((struct Pool *)ctx, ptr, size0, size);
}

#define POOL_ALLOCATOR(Pool_) (struct Allocator){ \
        .P = (Pool_)->P, \
        .alloc = pool_alloc, \
        .ctx = (Pool_) }

#endif // PAW_COMPILER_POOL_H
