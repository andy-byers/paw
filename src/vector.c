// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "vector.h"
#include "gc_aux.h"
#include "mem.h"
#include "rt.h"
#include "util.h"
#include <string.h>

void pawA_index_error(paw_Env *P, paw_Int index, size_t length)
{
    pawR_error(P, PAW_EINDEX,
               "index %I is out of bounds for array of length %I", index,
               paw_cast_int(length) /* fits in paw_Int */);
}

static size_t array_capacity(const Vector *a)
{
    return cast_size(a->upper - a->begin);
}

static void realloc_array(paw_Env *P, Vector *a, size_t alloc0, size_t alloc)
{
    const size_t end = pawA_length(a);
    pawM_resize(P, a->begin, alloc0, alloc);
    a->end = a->begin + end;
    a->upper = a->begin + alloc;
    check_gc(P);
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
    realloc_array(P, a, have, n);
}

static void reserve_extra(paw_Env *P, Vector *a, size_t extra)
{
    paw_assert(extra > 0);
    if (extra <= cast_size(a->upper - a->end)) {
        return; // Still have enough space
    }
    const size_t have = array_capacity(a);
    ensure_space(P, a, have, have + extra);
}

static void move_items(Value *src, ptrdiff_t shift, size_t count)
{
    memmove(src + shift, src, cast_size(count) * sizeof(src[0]));
}

void pawA_reserve(paw_Env *P, Vector *a, size_t want)
{
    const size_t have = array_capacity(a);
    if (want <= have) {
        return;
    }
    ensure_space(P, a, have, want);
}

void pawA_push(paw_Env *P, Vector *a, Value v)
{
    reserve_extra(P, a, 1);
    *a->end++ = v;
}

void pawA_resize(paw_Env *P, Vector *a, size_t length)
{
    const size_t n = pawA_length(a);
    if (length > n) {
        // new items are uninitialized
        pawA_reserve(P, a, length);
    } else if (length == 0) {
        a->end = a->begin;
        return;
    }
    a->end = a->begin + length;
}

void pawA_insert(paw_Env *P, Vector *a, paw_Int index, Value v)
{
    // Clamp to the array bounds.
    const size_t len = pawA_length(a);
    const paw_Int abs = pawA_abs_index(index, len);
    const size_t i = paw_clamp(cast_size(abs), 0, len);

    reserve_extra(P, a, 1);
    if (i != len) {
        move_items(a->begin + abs, 1, len - i);
    }
    a->begin[abs] = v;
    ++a->end;
}

void pawA_pop(paw_Env *P, Vector *a, paw_Int index)
{
    const size_t len = pawA_length(a);
    const paw_Int fixed = pawA_abs_index(index, len);
    const size_t abs = pawA_check_abs(P, fixed, len);
    if (abs != len - 1) {
        // Shift values into place
        move_items(a->begin + abs + 1, -1, len - abs - 1);
    }
    --a->end;
}

Vector *pawA_new(paw_Env *P)
{
    Vector *a = pawM_new(P, Vector);
    pawG_add_object(P, cast_object(a), VVECTOR);
    return a;
}

void pawA_free(paw_Env *P, Vector *a)
{
    pawM_free_vec(P, a->begin, array_capacity(a));
    pawM_free(P, a);
}

Vector *pawA_clone(paw_Env *P, StackPtr sp, const Vector *a)
{
    Vector *a2 = pawA_new(P);
    v_set_object(sp, a2); // anchor
    if (pawA_length(a)) {
        pawA_resize(P, a2, pawA_length(a));
        memcpy(a2->begin, a->begin, sizeof(a->begin[0]) * pawA_length(a));
    }
    return a2;
}

static paw_Bool elems_equal(paw_Env *P, Value x, Value y)
{
    StackPtr p = pawC_stkinc(P, 2);
    p[0] = y;
    p[1] = x;

    // Arrays can contain any type of value. Call pawR_binop() to check
    // metamethods on objects.
    //    pawR_binop(P, BINARY_EQ);
    paw_assert(0); // FIXME

    const paw_Bool b = paw_bool(P, -1);
    paw_pop(P, 1);
    return b;
}

paw_Bool pawA_equals(paw_Env *P, const Vector *lhs, const Vector *rhs)
{
    const size_t len = pawA_length(lhs);
    if (len != pawA_length(rhs)) {
        return PAW_FALSE;
    }
    for (size_t i = 0; i < len; ++i) {
        if (!elems_equal(P, lhs->begin[i], rhs->begin[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

paw_Bool pawA_contains(paw_Env *P, const Vector *a, const Value v)
{
    for (size_t i = 0; i < pawA_length(a); ++i) {
        if (elems_equal(P, v, a->begin[i])) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}
