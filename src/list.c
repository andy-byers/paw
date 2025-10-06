// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "list.h"
#include "mem.h"
#include "pool.h"
#include "util.h"

static void *list_grow(struct Pool *pool, void *data, size_t zelem, int *palloc, int target)
{
    int const alloc = *palloc;
    paw_assert(alloc < target && target <= K_LIST_MAX);

    if ((size_t)target > PAW_SIZE_MAX / zelem)
        pawM_error(pool->P);

    *palloc = target;
    return pawK_pool_alloc(pool, data, CAST_SIZE(alloc) * zelem, CAST_SIZE(target) * zelem);
}

void *pawK_list_reserve(struct Pool *pool, void *data, size_t zelem, int *palloc, int target)
{
    paw_assert(0 <= *palloc && *palloc <= K_LIST_MAX);
    paw_assert(0 <= target && target <= K_LIST_MAX);
    paw_assert(zelem > 0);

    if (target <= *palloc)
        return data;
    return list_grow(pool, data, zelem, palloc, target);
}

void *pawK_list_ensure_one(struct Pool *pool, void *data, size_t zelem, int count, int *palloc)
{
    paw_assert(0 <= *palloc && *palloc <= K_LIST_MAX);
    paw_assert(0 <= count && count <= K_LIST_MAX);
    paw_assert(zelem > 0);

    if (count < *palloc)
        return data;
    if (*palloc > K_LIST_MAX / 2)
        pawM_error(pool->P);
    int const n = PAW_MAX(*palloc * 2, K_LIST_MIN);
    return list_grow(pool, data, zelem, palloc, n);
}
