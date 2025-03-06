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

#define ROUND_SIZE(Size_) PAW_ROUND_UP(PAW_MAX(Size_, sizeof(struct FreeBlock)))
#define ARENA_CUTOFF (sizeof(void *) << 2)
#define ALLOC_ALIGN K_ALIGNOF_NODE
#define ARENA_MIN 512

// Create a new arena large enough to allocate memory of the 'required_size'
// Alignment is not considered, since the start of an Arena is suitably-aligned
// for any objects created by the compiler.
static Arena *new_arena(paw_Env *P, struct Pool *pool, size_t required_size)
{
    size_t const size0 = pool->arena != NULL
                             ? pool->arena->size
                             : ARENA_MIN;

    if (size0 > SIZE_MAX / 2)
        pawM_error(P); // sanity check

    size_t const size = PAW_MAX(size0 * 2, required_size);
    Arena *a = pawM_new_flex(P, Arena, size, 1);
    a->size = size;
    return a;
}

void pawK_pool_init(paw_Env *P, struct Pool *pool, size_t base_size)
{
    pool->arena = new_arena(P, pool, base_size);
    pool->full = NULL;
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
    free_arena_list(P, pool->full);
}

static void *find_free_block(struct Pool *pool, size_t size)
{
    for (struct FreeBlock **p = &pool->free; *p; p = &(*p)->prev) {
        if (size == CAST_SIZE((*p)->size)) {
            paw_assert(0 == (CAST_UPTR(*p) & (K_ALIGNOF_NODE - 1)));
            struct FreeBlock *block = *p;
            *p = (*p)->prev;
            return block;
        }
    }
    return NULL;
}

static void *pool_malloc(paw_Env *P, struct Pool *pool, size_t size)
{
    paw_assert(size > 0);
    size = ROUND_SIZE(size);

    void *const ptr = find_free_block(pool, size);
    if (ptr != NULL)
        return ptr;

    Arena **pa = &pool->arena;
    while (*pa != NULL) {
        Arena *const a = *pa;
        size_t const base = (a->used + ALLOC_ALIGN - 1) & ~(ALLOC_ALIGN - 1);
        if (size <= a->size && base <= a->size - size) {
            a->used = base + size;
            return a->data + base;
        }
        if (a->size - a->used < ARENA_CUTOFF) {
            // arena is effectively full: set it aside so it is no longer considered
            *pa = a->prev;
            a->prev = pool->full;
            pool->full = a;
        } else {
            pa = &a->prev;
        }
    }

    // create a new arena, guaranteed to hold at least 'size' bytes
    Arena *const a = new_arena(P, pool, size);
    a->prev = pool->arena;
    pool->arena = a;
    a->used = size;
    return a->data;
}

static void pool_free(struct Pool *pool, void *ptr, size_t size)
{
    if (ptr == NULL) {
        paw_assert(size == 0);
        return;
    }

    paw_assert(PAW_IS_ALIGNED(ptr));
    size = ROUND_SIZE(size);
    struct FreeBlock *hdr = ptr;
    *hdr = (struct FreeBlock){
        .prev = pool->free,
        .size = size,
    };
    pool->free = hdr;

#if PAW_STRESS > 0
    memset(hdr + 1, 0xAA, size - sizeof(*hdr));
#endif
}

static paw_Bool arena_list_contains(struct Arena *a, void *ptr)
{
#if !NDEBUG
    if (ptr == NULL)
        return PAW_TRUE;

    uintptr_t const u = CAST_UPTR(ptr);
    while (a != NULL) {
        uintptr_t const lower = CAST_UPTR(a->data);
        uintptr_t const upper = lower + a->used;
        if (lower <= u && u < upper)
            return PAW_TRUE;
        a = a->prev;
    }
    return PAW_FALSE;
#else
    PAW_UNUSED(a);
    PAW_UNUSED(ptr);
    PAW_UNREACHABLE();
#endif
}

void *pawK_pool_alloc(paw_Env *P, struct Pool *pool, void *ptr, size_t size0, size_t size)
{
    // ensure that the allocation came from this memory pool
    paw_assert(arena_list_contains(pool->arena, ptr)
               || arena_list_contains(pool->full, ptr));

    if (size == 0) {
        pool_free(pool, ptr, size0);
        return NULL;
    }
    void *ptr2 = pool_malloc(P, pool, size);
    if (size0 > 0) {
        memcpy(ptr2, ptr, PAW_MIN(size0, size));
        pool_free(pool, ptr, size0);
    }
    return ptr2;
}

void *list_grow(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc, int target, int max)
{
    size_t const alloc = *palloc;
    paw_assert(alloc < target && target <= K_LIST_MAX);

    if (target > max / CAST(int, zelem))
        pawM_error(P);

    *palloc = target;
    return pawK_pool_alloc(P, pool, data, CAST_SIZE(alloc) * zelem, CAST_SIZE(target) * zelem);
}

void *pawK_list_reserve(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc, int target)
{
    paw_assert(0 <= *palloc && *palloc <= K_LIST_MAX);
    paw_assert(0 <= target && target <= K_LIST_MAX);
    paw_assert(0 <= count && count <= K_LIST_MAX);
    paw_assert(zelem > 0);

    if (target <= *palloc)
        return data;
    return list_grow(P, pool, data, zelem, count, palloc, target, K_LIST_MAX);
}

void *pawK_list_ensure_one(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc)
{
    paw_assert(0 <= *palloc && *palloc <= K_LIST_MAX);
    paw_assert(0 <= count && count <= K_LIST_MAX);
    paw_assert(zelem > 0);

    if (*palloc > count)
        return data;
    if (*palloc > K_LIST_MAX / 2)
        pawM_error(P);
    int const n = PAW_MAX(*palloc * 2, K_LIST_MIN);
    return list_grow(P, pool, data, zelem, count, palloc, n, K_LIST_MAX);
}
