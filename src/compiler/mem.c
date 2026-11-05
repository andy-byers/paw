// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "mem.h"
#include "env.h"

static void *m_alloc(paw_Env *P, void *ptr, size_t size0, size_t size)
{
    if (size == 0) {
        if (ptr == NULL)
            return NULL;
        P->num_bytes -= size0; // 'free' never fails
        return P->alloc(P->ud, ptr, size0, 0);
    }
    // (re)allocate memory
    void *ptr2 = P->alloc(P->ud, ptr, size0, size);
    if (ptr2 != NULL)
        P->num_bytes += size - size0;
    return ptr2;
}

void *mem_alloc(paw_Env *P, void *ptr, size_t size0, size_t size)
{
    return m_alloc(P, ptr, size0, size);
}

void mem_free_(paw_Env *P, void *ptr, size_t size)
{
    m_alloc(P, ptr, size, 0);
}

void *mem_new_vec_(paw_Env *P, size_t n, size_t elem_sz)
{
    if (n == 0)
        return NULL;
    mem_check_size(P, 0, n, elem_sz);
    void *ptr = mem_alloc(P, NULL, 0, n * elem_sz);
    if (ptr == NULL)
        mem_error(P);
    memset(ptr, 0, n * elem_sz);
    return ptr;
}

void *mem_new_flex_(paw_Env *P, size_t obj_sz, size_t n, size_t elem_sz)
{
    mem_check_size(P, obj_sz, n, elem_sz);
    void *ptr = mem_alloc(P, NULL, 0, obj_sz + n * elem_sz);
    if (ptr == NULL)
        mem_error(P);

    // clear non-flex part
    memset(ptr, 0, obj_sz);
    return ptr;
}

#define ARRAY_MIN 8

void *mem_grow_(paw_Env *P, void *ptr, int n, int *p_alloc, size_t elem_sz)
{
    int cap = *p_alloc;
    if (n + 1 <= cap) {
        return ptr;
    }
    if (cap < ARRAY_MIN) {
        cap = ARRAY_MIN;
    } else if (cap > INT_MAX / 2) {
        mem_error(P); // overflow
    } else {
        cap *= 2;
    }
    ptr = mem_resize_(P, ptr, CAST_SIZE(*p_alloc), CAST_SIZE(cap), elem_sz);
    *p_alloc = cap;
    return ptr;
}

void *mem_shrink_(paw_Env *P, void *ptr, int *palloc0, int alloc, size_t elem_sz)
{
    paw_assert(*palloc0 >= alloc);
    if (*palloc0 == alloc) return ptr;
    ptr = mem_resize_(P, ptr, CAST_SIZE(*palloc0), CAST_SIZE(alloc), elem_sz);
    *palloc0 = alloc;
    return ptr;
}

void *mem_resize_(paw_Env *P, void *ptr, size_t alloc0, size_t alloc, size_t elem_sz)
{
    void *ptr2 = mem_alloc(P, ptr, alloc0 * elem_sz, alloc * elem_sz);
    if (alloc > 0 && ptr2 == NULL)
        mem_error(P);
    return ptr2;
}
