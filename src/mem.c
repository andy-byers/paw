// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "mem.h"
#include "gc.h"
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

static void *first_try(paw_Env *P, void *ptr, size_t size0, size_t size)
{
#ifdef PAW_STRESS_GC
    paw_unused(P);
    paw_unused(ptr);
    paw_unused(size0);
    if (size) {
        // Fail on the first attempt to get more memory.
        return NULL;
    }
#endif
    return P->alloc(P->ud, ptr, size0, size);
}

static void *try_again(paw_Env *P, void *ptr, size_t size0, size_t size)
{
    pawG_collect(P); // Emergency collection
    return P->alloc(P->ud, ptr, size0, size);
}

static void *alloc(paw_Env *P, void *ptr, size_t size0, size_t size)
{
    void *ptr2 = first_try(P, ptr, size0, size);
    if (size && !ptr2) {
        // Run an emergency collection and try again.
        ptr2 = try_again(P, ptr, size0, size);
    }
    return ptr2;
}

void *pawM_malloc_(paw_Env *P, size_t size)
{
    return pawM_realloc_(P, NULL, 0, size);
}

void *pawM_calloc_(paw_Env *P, size_t count, size_t elem_sz)
{
    pawM_check_size(P, count, elem_sz);
    void *ptr = pawM_malloc_(P, count * elem_sz);
    if (ptr) {
        memset(ptr, 0, count * elem_sz);
    }
    return ptr;
}

void *pawM_realloc_(paw_Env *P, void *ptr, size_t size0, size_t size)
{
    void *ptr2 = alloc(P, ptr, size0, size);
    if (!size || ptr2) {
        P->gc_bytes -= size0;
        P->gc_bytes += size;
    }
    return ptr2;
}

void pawM_free_(paw_Env *P, void *ptr, size_t size)
{
    alloc(P, ptr, size, 0);
    P->gc_bytes -= size;
}

void *pawM_new_(paw_Env *P, size_t n, size_t elem_sz)
{
    assert(n > 0);
    void *ptr = pawM_calloc_(P, n, elem_sz);
    if (!ptr) {
        pawM_error(P);
    }
    return ptr;
}

#define ARRAY_MIN 8

void *pawM_grow_(paw_Env *P, void *ptr, int n, int *p_alloc, size_t elem_sz)
{
    int cap = *p_alloc;
    if (n + 1 <= cap) {
        return ptr;
    }
    if (cap < ARRAY_MIN) {
        cap = ARRAY_MIN;
    } else if (cap > INT_MAX / 2) {
        pawM_error(P); // Overflow
    } else {
        cap *= 2;
    }
    ptr = pawM_resize_(P, ptr, cast_size(*p_alloc), cast_size(cap), elem_sz);
    *p_alloc = cap;
    return ptr;
}

void *pawM_shrink_(paw_Env *P, void *ptr, int *p_alloc, int alloc, size_t elem_sz)
{
    paw_assert(*p_alloc >= alloc);
    if (*p_alloc == alloc) {
        return ptr;
    }
    ptr = pawM_resize_(P, ptr, cast_size(*p_alloc), cast_size(alloc), elem_sz);
    *p_alloc = alloc;
    return ptr;
}

void *pawM_resize_(paw_Env *P, void *ptr, size_t alloc0, size_t alloc, size_t elem_sz)
{
    void *ptr2 = pawM_realloc_(P, ptr, alloc0 * elem_sz, alloc * elem_sz);
    if (alloc && !ptr2) {
        pawM_error(P);
    }
    return ptr2;
}
