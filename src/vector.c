// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "gc.h"
#include "mem.h"
#include "rt.h"
#include "util.h"
#include <string.h>

void pawV_index_error(paw_Env *P, paw_Int index, size_t length)
{
    pawR_error(P, PAW_EINDEX,
               "index %I is out of bounds for container of length %I", index,
               paw_cast_int(length));
}

static size_t vector_capacity(const Vector *a)
{
    return CAST_SIZE(a->upper - a->begin);
}

static void realloc_vector(paw_Env *P, Vector *a, size_t alloc0, size_t alloc)
{
    const size_t end = pawV_vec_length(a);
    pawM_resize(P, a->begin, alloc0, alloc);
    a->end = a->begin + end;
    a->upper = a->begin + alloc;
    CHECK_GC(P);
}

static void ensure_space(paw_Env *P, Vector *a, size_t have, size_t want)
{
    if (want > PAW_SIZE_MAX / sizeof(Value)) {
        pawM_error(P);
    }
    // Use the next power-of-2.
    size_t n = 1;
    while (n < want) {
        n *= 2;
    }
    realloc_vector(P, a, have, n);
}

static void reserve_extra(paw_Env *P, Vector *a, size_t extra)
{
    paw_assert(extra > 0);
    if (extra <= CAST_SIZE(a->upper - a->end)) {
        return; // Still have enough space
    }
    const size_t have = vector_capacity(a);
    ensure_space(P, a, have, have + extra);
}

static void move_items(Value *src, ptrdiff_t shift, size_t count)
{
    memmove(src + shift, src, CAST_SIZE(count) * sizeof(src[0]));
}

void pawV_vec_reserve(paw_Env *P, Vector *a, size_t want)
{
    const size_t have = vector_capacity(a);
    if (want <= have) {
        return;
    }
    ensure_space(P, a, have, want);
}

void pawV_vec_push(paw_Env *P, Vector *a, Value v)
{
    reserve_extra(P, a, 1);
    *a->end++ = v;
}

void pawV_vec_resize(paw_Env *P, Vector *a, size_t length)
{
    pawV_vec_reserve(P, a, length);
    // avoid 'Nullptr with offset' from UBSan
    a->end = length ? a->begin + length : a->begin;
}

void pawV_vec_insert(paw_Env *P, Vector *a, paw_Int index, Value v)
{
    // Clamp to the vector bounds.
    const size_t len = pawV_vec_length(a);
    const paw_Int abs = pawV_abs_index(index, len);
    const size_t i = PAW_CLAMP(CAST_SIZE(abs), 0, len);

    reserve_extra(P, a, 1);
    if (i != len) {
        move_items(a->begin + abs, 1, len - i);
    }
    a->begin[abs] = v;
    ++a->end;
}

void pawV_vec_pop(paw_Env *P, Vector *a, paw_Int index)
{
    const size_t len = pawV_vec_length(a);
    const paw_Int fixed = pawV_abs_index(index, len);
    const size_t abs = pawV_check_abs(P, fixed, len);
    if (abs != len - 1) {
        // Shift values into place
        move_items(a->begin + abs + 1, -1, len - abs - 1);
    }
    --a->end;
}

Vector *pawV_vec_new(paw_Env *P)
{
    Vector *a = pawM_new(P, Vector);
    pawG_add_object(P, CAST_OBJECT(a), VVECTOR);
    return a;
}

void pawV_vec_free(paw_Env *P, Vector *a)
{
    pawM_free_vec(P, a->begin, vector_capacity(a));
    pawM_free(P, a);
}

Vector *pawV_vec_clone(paw_Env *P, Value *pv, const Vector *a)
{
    Vector *a2 = pawV_vec_new(P);
    v_set_object(pv, a2); // anchor
    if (pawV_vec_length(a)) {
        pawV_vec_resize(P, a2, pawV_vec_length(a));
        memcpy(a2->begin, a->begin, sizeof(a->begin[0]) * pawV_vec_length(a));
    }
    return a2;
}

static paw_Bool elems_equal(Value x, Value y)
{
    // TODO: Only allowed for 'basic' types right now. Compiler set to complain
    // otherwise.
    return x.u == y.u;
}

paw_Bool pawV_vec_equals(paw_Env *P, const Vector *lhs, const Vector *rhs)
{
    const size_t len = pawV_vec_length(lhs);
    if (len != pawV_vec_length(rhs)) {
        return PAW_FALSE;
    }
    for (size_t i = 0; i < len; ++i) {
        if (!elems_equal(lhs->begin[i], rhs->begin[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

paw_Bool pawV_vec_contains(paw_Env *P, const Vector *a, const Value v)
{
    for (size_t i = 0; i < pawV_vec_length(a); ++i) {
        if (elems_equal(v, a->begin[i])) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}
