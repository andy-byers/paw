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
#if PAW_STRESS > 0
    if (size && !P->gc_noem) {
        // Fail on the first attempt to get more memory, but only if the
        // runtime is allowed to perform emergency collections. Otherwise,
        // PAW_STRESS > 0 would not be compatible with certain operations,
        // like reallocating the stack, where emergency collections are
        // not allowed.
        return NULL;
    }
#endif
    return P->alloc(P->ud, ptr, size0, size);
}

static void *try_again(paw_Env *P, void *ptr, size_t size0, size_t size)
{
    pawG_collect(P); // emergency collection
    return P->alloc(P->ud, ptr, size0, size);
}

static void *m_alloc(paw_Env *P, void *ptr, size_t size0, size_t size)
{
    if (size == 0) {
        if (ptr == NULL) {
            return NULL;
        }
        P->gc_bytes -= size0; // 'free' never fails
        return P->alloc(P->ud, ptr, size0, 0);
    }
    // (re)allocate memory
    void *ptr2 = first_try(P, ptr, size0, size);
    if (!ptr2 && !P->gc_noem) {
        // Run an emergency collection and try again.
        ptr2 = try_again(P, ptr, size0, size);
    }
    if (ptr2) {
        P->gc_bytes += size - size0;
    }
    return ptr2;
}

void *pawM_alloc(paw_Env *P, void *ptr, size_t size0, size_t size)
{
    return m_alloc(P, ptr, size0, size);
}

void pawM_free_(paw_Env *P, void *ptr, size_t size)
{
    m_alloc(P, ptr, size, 0);
}

void *pawM_new_vec_(paw_Env *P, size_t n, size_t elem_sz)
{
    if (n == 0) {
        return NULL;
    }
    pawM_check_size(P, 0, n, elem_sz);
    void *ptr = pawM_alloc(P, NULL, 0, n * elem_sz);
    if (!ptr) {
        pawM_error(P);
    }
    memset(ptr, 0, n * elem_sz);
    return ptr;
}

void *pawM_new_flex_(paw_Env *P, size_t obj_sz, size_t n, size_t elem_sz)
{
    pawM_check_size(P, obj_sz, n, elem_sz);
    void *ptr = pawM_alloc(P, NULL, 0, obj_sz + n * elem_sz);
    if (!ptr) {
        pawM_error(P);
    }
    // clear non-flex part
    memset(ptr, 0, obj_sz);
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
        pawM_error(P); // overflow
    } else {
        cap *= 2;
    }
    ptr = pawM_resize_(P, ptr, cast_size(*p_alloc), cast_size(cap), elem_sz);
    *p_alloc = cap;
    return ptr;
}

void *pawM_shrink_(paw_Env *P, void *ptr, int *palloc0, int alloc, size_t elem_sz)
{
    paw_assert(*palloc0 >= alloc);
    if (*palloc0 == alloc) {
        return ptr;
    }
    ptr = pawM_resize_(P, ptr, cast_size(*palloc0), cast_size(alloc), elem_sz);
    *palloc0 = alloc;
    return ptr;
}

void *pawM_resize_(paw_Env *P, void *ptr, size_t alloc0, size_t alloc, size_t elem_sz)
{
    void *ptr2 = pawM_alloc(P, ptr, alloc0 * elem_sz, alloc * elem_sz);
    if (alloc && !ptr2) {
        pawM_error(P);
    }
    return ptr2;
}
