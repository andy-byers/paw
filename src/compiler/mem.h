// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_MEM_H
#define PAW_MEM_H

#include "call.h"
#include "core.h"

// Throw an 'out of memory' error
// The error message is allocated on startup, and there is always an extra
// stack slot to hold it.
#define mem_error(P) \
    pawC_throw(P, PAW_EMEMORY)

typedef void *(*Alloc)(void *ctx, void *ptr, paw_Usize size0, paw_Usize size);

struct Allocator {
    paw_Env *P;
    Alloc alloc;
    void *ctx;
};


#define mem_new(P, type) mem_new_vec(P, 1, type)
#define mem_free(P, ptr) mem_free_vec(P, ptr, 1)

#define mem_new_vec(P, n, type) \
    (type *)mem_new_vec_(P, CAST_SIZE(n), sizeof(type))
#define mem_free_vec(P, ptr, n) \
    mem_free_(P, ptr, CAST_SIZE(n) * sizeof((ptr)[0]))

#define mem_new_flex(P, tobj, n, e) \
    (tobj *)mem_new_flex_(P, sizeof(tobj), CAST_SIZE(n), e)
#define mem_free_flex(P, ptr, n, e) \
    mem_free_(P, ptr, sizeof(*(ptr)) + (CAST_SIZE(n) * CAST_SIZE(e)))


// Ensure that the expression 'o + n * e' will not wrap
#define mem_check_size(P, o, n, e) \
    (n) > (SIZE_MAX - o) / (e) ? mem_error(P) : PAW_UNUSED(0)

// Low-level memory allocation routine
void *mem_alloc(paw_Env *P, void *ptr, size_t size0, size_t size);

void *mem_new_vec_(paw_Env *P, size_t n, size_t elem_sz);
void *mem_new_flex_(paw_Env *P, size_t obj_sz, size_t n, size_t elem_sz);
void *mem_resize_(paw_Env *P, void *ptr, size_t alloc0, size_t alloc,
                   size_t elem_sz);
void *mem_grow_(paw_Env *P, void *ptr, int size, int *p_alloc, size_t elem_sz);
void *mem_shrink_(paw_Env *P, void *ptr, int *p_alloc, int alloc,
                   size_t elem_sz);
void mem_free_(paw_Env *P, void *ptr, size_t size);

#endif // PAW_MEM_H
