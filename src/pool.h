// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_POOL_H
#define PAW_POOL_H

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

void pawK_pool_init(paw_Env *P, struct Pool *pool, size_t base_size, struct PoolStats st);
void pawK_pool_uninit(struct Pool *pool);
void *pawK_pool_alloc(struct Pool *pool, void *ptr, size_t size0, size_t size);

#endif // PAW_POOL_H
