// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_MEM_H
#define PAW_MEM_H

#include "call.h"
#include "paw.h"

// Throw an 'out of memory' error
// The error message is allocated on startup, and there is always an extra
// stack slot to hold it.
#define pawM_error(P) \
    (*(P)->top.p++ = (P)->mem_errmsg, pawC_throw(P, PAW_EMEMORY))

#define pawM_new(P, type) pawM_new_vec(P, 1, type)
#define pawM_free(P, ptr) pawM_free_vec(P, ptr, 1)

#define pawM_new_vec(P, n, type) \
    (type *)pawM_new_vec_(P, CAST_SIZE(n), sizeof(type))
#define pawM_free_vec(P, ptr, n) \
    pawM_free_(P, ptr, CAST_SIZE(n) * sizeof((ptr)[0]))

#define pawM_new_flex(P, tobj, n, e) \
    (tobj *)pawM_new_flex_(P, sizeof(tobj), CAST_SIZE(n), e)
#define pawM_free_flex(P, ptr, n, e) \
    pawM_free_(P, ptr, sizeof(*(ptr)) + (CAST_SIZE(n) * CAST_SIZE(e)))

#define pawM_grow(P, ptr, size, alloc) \
    ((ptr) = pawM_grow_(P, ptr, size, &(alloc), sizeof((ptr)[0])))
#define pawM_shrink(P, ptr, alloc0, alloc) \
    ((ptr) = pawM_shrink_(P, ptr, &(alloc0), alloc, sizeof((ptr)[0])))
#define pawM_resize(P, ptr, alloc0, alloc) \
    ((ptr) = pawM_resize_aux(P, ptr, CAST_SIZE(alloc0), CAST_SIZE(alloc)))

// Ensure that the expression 'o + n * e' will not wrap
#define pawM_check_size(P, o, n, e) \
    (n) > (SIZE_MAX - o) / (e) ? pawM_error(P) : PAW_UNUSED(0)

// Resize a chunk of memory, ensuring that the new allocation size will
// not overflow
#define pawM_resize_aux(P, p, n0, n)                         \
    ((n) > (n0) && pawM_check_size(P, 0, n, sizeof((p)[0])), \
     pawM_resize_(P, p, n0, n, sizeof((p)[0])))

// Low-level memory allocation routine
void *pawM_alloc(paw_Env *P, void *ptr, size_t size0, size_t size);

void *pawM_new_vec_(paw_Env *P, size_t n, size_t elem_sz);
void *pawM_new_flex_(paw_Env *P, size_t obj_sz, size_t n, size_t elem_sz);
void *pawM_resize_(paw_Env *P, void *ptr, size_t alloc0, size_t alloc,
                   size_t elem_sz);
void *pawM_grow_(paw_Env *P, void *ptr, int size, int *p_alloc, size_t elem_sz);
void *pawM_shrink_(paw_Env *P, void *ptr, int *p_alloc, int alloc,
                   size_t elem_sz);
void pawM_free_(paw_Env *P, void *ptr, size_t size);

#endif // PAW_MEM_H
