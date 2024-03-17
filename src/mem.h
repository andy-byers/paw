// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_MEM_H
#define PAW_MEM_H

#include "call.h"
#include "paw.h"

#define pawM_error(P) pawC_throw(P, PAW_EMEMORY)

#define pawM_new(P, t) pawM_new_vec(P, 1, t)
#define pawM_free(P, p) pawM_free_vec(P, p, 1)

#define pawM_new_vec(P, n, t) (t *)pawM_new_(P, cast_size(n), sizeof(t))
#define pawM_free_vec(P, p, n) pawM_free_(P, p, cast_size(n) * sizeof((p)[0]))

#define pawM_new_fa(P, t, a) (t *)pawM_new_(P, 1, sizeof(t) + (a))
#define pawM_free_fa(P, p, a) pawM_free_(P, p, sizeof(*(p)) + (a))

#define pawM_grow(P, p, n, N) ((p) = pawM_grow_(P, p, n, &(N), sizeof((p)[0])))
#define pawM_shrink(P, p, N, N2) ((p) = pawM_shrink_(P, p, &(N), N2, sizeof((p)[0])))
#define pawM_resize(P, p, N0, N) ((p) = pawM_resize_aux(P, p, N0, N))

#define pawM_check_size(P, N, e) \
    (N) > SIZE_MAX / (e) ? pawM_error(P) : paw_unused(0)
#define pawM_resize_aux(P, p, N0, N)                      \
    ((N) > (N0) && pawM_check_size(P, N, sizeof((p)[0])), \
     pawM_resize_(P, p, N0, N, sizeof((p)[0])))

// Low-level memory allocation routines
void *pawM_new_(paw_Env *P, size_t n, size_t elem_sz);
void *pawM_resize_(paw_Env *P, void *ptr, size_t alloc0, size_t alloc, size_t elem_sz);
void *pawM_grow_(paw_Env *P, void *ptr, int size, int *p_alloc, size_t elem_sz);
void *pawM_shrink_(paw_Env *P, void *ptr, int *p_alloc, int alloc, size_t elem_sz);
void *pawM_realloc_(paw_Env *P, void *ptr, size_t size0, size_t size);
void pawM_free_(paw_Env *P, void *ptr, size_t size);

#endif // PAW_MEM_H
