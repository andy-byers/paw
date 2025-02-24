// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "code.h"
#include "compile.h"

typedef struct Arena {
    struct Arena *prev;
    size_t used;
    size_t size;

    // Must be aligned to at least the strictest alignment required
    // by an AST or HIR node.
    K_ALIGNAS_NODE char data[];
} Arena;

// Create a new arena large enough to allocate memory of the 'required_size'
// Alignment is not considered, since the start of an Arena is suitably-aligned
// for any objects created by the compiler.
static Arena *new_arena(paw_Env *P, struct Pool *pool, size_t required_size)
{
    if (required_size > SIZE_MAX / 2) {
        pawM_error(P); // sanity check
    }
    size_t size = pool->last_size;
    while (size < required_size) {
        size *= 2;
    }
    pool->last_size = size;
    Arena *a = pawM_new_flex(P, Arena, size, 1);
    memset(a->data, 0, size);
    a->size = size;
    return a;
}

void pawK_pool_init(paw_Env *P, struct Pool *pool, size_t base_size, size_t min_size)
{
    pool->filled = NULL;
    pool->last_size = base_size;
    pool->arena = new_arena(P, pool, 0);
    pool->min_size = min_size;
}

static void free_arena_list(paw_Env *P, Arena *a)
{
    while (a != NULL) {
        Arena *prev = a->prev;
        pawM_free_flex(P, a, a->size, 1);
        a = prev;
    }
}

void pawK_pool_uninit(paw_Env *P, struct Pool *pool)
{
    free_arena_list(P, pool->arena);
    free_arena_list(P, pool->filled);
}

static void *find_free_block(struct Pool *pool, size_t size)
{
#if 0
    for (struct FreeBlock **p = &pool->free; *p; p = &(*p)->prev) {
        if (size == CAST_SIZE((*p)->size)) {
            paw_assert(0 == (CAST_UPTR(*p) & (K_ALIGNOF_NODE - 1)));
            struct FreeBlock *block = *p;
            *p = (*p)->prev;
            return block;
        }
    }
#endif // 0
    return NULL;
}

static void *pool_malloc(paw_Env *P, struct Pool *pool, size_t size)
{
    paw_assert(size > 0);
    void *ptr = find_free_block(pool, size);
    if (ptr != NULL) {
        memset(ptr, 0, size);
        return ptr;
    }

    Arena *a = pool->arena;
    size_t base = (a->used + K_ALIGNOF_NODE - 1) & ~(K_ALIGNOF_NODE - 1);
    if (base + size > a->size) {
        // create a new arena, guaranteed to hold at least 'size' bytes
        a = new_arena(P, pool, size);
        a->prev = pool->arena;
        pool->arena = a;
        base = 0;
    }
    a->used = base + size;
    return a->data + base;
}

static void pool_free(struct Pool *pool, void *ptr, size_t size)
{
    // TODO: Is it worth it to get this working? May be more effective to use
    //       multiple pools: 1 for the AST, 1 for the HIR, etc.
#if 0
    if (ptr == NULL) {
        paw_assert(size == 0);
        return;
    }
    const struct FreeBlock prototype = {
        .prev = pool->free,
        .size = size,
    };
    paw_assert(PAW_IS_ALIGNED(ptr));
    paw_assert(size >= sizeof(prototype));
    memcpy(ptr, &prototype, sizeof(prototype));
    pool->free = ptr;
#endif // 0
}

void *pawK_pool_alloc(paw_Env *P, struct Pool *pool, void *ptr, size_t size0, size_t size)
{
    if (size == 0) {
        pool_free(pool, ptr, size0);
        return NULL;
    }
    void *ptr2 = pool_malloc(P, pool, size);
    if (size0 > 0) {
        memcpy(ptr2, ptr, PAW_MIN(size0, size));
    }
    if (size0 < size) {
        memset(BUMP_PTR(ptr2, size0), 0, size - size0);
    }
    return ptr2;
}

void *list_grow(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc, int target, int max)
 {
    paw_assert(*palloc < target && target <= K_LIST_MAX);
    if (target > max / CAST(int, zelem)) pawM_error(P);
    void *p = pawK_pool_alloc(P, pool, NULL, 0, CAST_SIZE(target) * zelem);
    if (count > 0) memcpy(p, data, CAST_SIZE(count) * zelem);
    pool_free(pool, data, CAST_SIZE(*palloc) * zelem);
    *palloc = target;
    return p;
}

void *pawK_list_reserve(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc, int target)
{
    paw_assert(0 <= *palloc && *palloc <= K_LIST_MAX);
    paw_assert(0 <= target && target <= K_LIST_MAX);
    paw_assert(0 <= count && count <= K_LIST_MAX);
    paw_assert(zelem > 0);

    if (target <= *palloc) return data;
    return list_grow(P, pool, data, zelem, count, palloc, target, K_LIST_MAX);
}

void *pawK_list_ensure_one(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc)
{
    paw_assert(0 <= *palloc && *palloc <= K_LIST_MAX);
    paw_assert(0 <= count && count <= K_LIST_MAX);
    paw_assert(zelem > 0);

    if (*palloc > count) return data;
    if (*palloc > K_LIST_MAX / 2) pawM_error(P);
    const int n = PAW_MAX(*palloc * 2, K_LIST_MIN);
    return list_grow(P, pool, data, zelem, count, palloc, n, K_LIST_MAX);
 }

