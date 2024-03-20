// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "array.h"
#include "gc.h"
#include "mem.h"
#include "rt.h"
#include "util.h"
#include <string.h>

static paw_Int correct_index(const Array *a, paw_Int index)
{
    return index + (index < 0 ? (a->end - a->begin) : 0);
}

static size_t check_bounds(paw_Env *P, paw_Int i, size_t n)
{
    if (i < 0 || cast_size(i) >= n) {
        pawR_error(P, PAW_EINDEX, "index %I is out of bounds for array of length %I",
                   i, paw_cast_int(n) /* fits in paw_Int */);
    }
    return cast_size(i);
}

static size_t array_capacity(const Array *a)
{
    return cast_size(a->upper - a->begin);
}

static void realloc_array(paw_Env *P, Array *a, size_t old_size, size_t new_size)
{
    const size_t end = pawA_length(a);
    pawM_resize(P, a->begin, old_size, new_size);
    a->end = a->begin + end;
    a->upper = a->begin + new_size;
    CHECK_GC(P);
}

static void ensure_space(paw_Env *P, Array *a, size_t have, size_t want)
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

static void reserve_extra(paw_Env *P, Array *a, size_t extra)
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

void pawA_reserve(paw_Env *P, Array *a, size_t want)
{
    const size_t have = array_capacity(a);
    if (want <= have) {
        return;
    }
    ensure_space(P, a, have, want);
}

void pawA_push(paw_Env *P, Array *a, Value v)
{
    reserve_extra(P, a, 1);
    *a->end++ = v;
}

void pawA_resize(paw_Env *P, Array *a, size_t length)
{
    const size_t n = pawA_length(a);
    if (length > n) {
        pawA_reserve(P, a, length);
        // Set new objects to 'null'.
        memset(a->end, (int)VNULL, sizeof(a->end[0]) * (length - n));
    } else if (length == 0) {
        a->end = a->begin;
        return;
    }
    a->end = a->begin + length;
}

void pawA_insert(paw_Env *P, Array *a, paw_Int index, Value v)
{
    // Clamp to the array bounds.
    const paw_Int fixed = correct_index(a, index);
    const size_t len = pawA_length(a);
    const size_t abs = paw_clamp(cast_size(fixed), 0, len);

    reserve_extra(P, a, 1);
    if (abs != len) {
        move_items(a->begin + abs, 1, len - abs);
    }
    a->begin[abs] = v;
    ++a->end;
}

void pawA_pop(paw_Env *P, Array *a, paw_Int index)
{
    const paw_Int fixed = correct_index(a, index);
    const size_t len = pawA_length(a);
    const size_t abs = check_bounds(P, fixed, len);
    if (abs != len - 1) {
        // Shift values into place
        move_items(a->begin + abs + 1, -1, len - abs - 1);
    }
    --a->end;
}

Array *pawA_new(paw_Env *P)
{
    Array *a = pawM_new(P, Array);
    pawG_add_object(P, cast_object(a), VARRAY);
    return a;
}

void pawA_free(paw_Env *P, Array *a)
{
    pawM_free_vec(P, a->begin, array_capacity(a));
    pawM_free(P, a);
}

void pawA_clone(paw_Env *P, StackPtr sp, const Array *a)
{
    Array *arr2 = pawA_new(P);
    pawV_set_array(sp, arr2); // Anchor
    if (pawA_length(a)) {
        pawA_resize(P, arr2, pawA_length(a));
        memcpy(arr2->begin, a->begin, sizeof(a->begin[0]) * pawA_length(a));
    }
}

Array *pawA_concat(paw_Env *P, const Array *a, const Array *arr2)
{
    Array *cat = pawA_new(P);
    reserve_extra(P, cat, pawA_length(arr2));
    for (size_t i = 0; i < pawA_length(a); ++i) {
        pawA_push(P, cat, a->begin[i]);
    }
    for (size_t i = 0; i < pawA_length(arr2); ++i) {
        pawA_push(P, cat, arr2->begin[i]);
    }
    return cat;
}

Value *pawA_get(paw_Env *P, Array *a, paw_Int index)
{
    const paw_Int fixed = correct_index(a, index);
    const size_t abs = check_bounds(P, fixed, pawA_length(a));
    return &a->begin[abs];
}

static paw_Bool elems_equal(paw_Env *P, const Value x, const Value y)
{
    StackPtr p = pawC_stkinc(P, 2);
    p[0] = y;
    p[1] = x;

    // Arrays can contain any type of value. Call pawR_equals() to check
    // metamethods on objects.
    pawR_equals(P);

    const paw_Bool b = paw_boolean(P, -1);
    paw_pop(P, 1);
    return b;
}

paw_Bool pawA_equals(paw_Env *P, Array *lhs, Array *rhs)
{
    const size_t len = pawA_length(lhs);
    if (len != pawA_length(rhs)) {
        return PAW_BFALSE;
    }
    for (size_t i = 0; i < len; ++i) {
        if (!elems_equal(P, lhs->begin[i], rhs->begin[i])) {
            return PAW_BFALSE;
        }
    }
    return PAW_BTRUE;
}

paw_Bool pawA_contains(paw_Env *P, Array *a, const Value v)
{
    for (size_t i = 0; i < pawA_length(a); ++i) {
        if (elems_equal(P, v, a->begin[i])) {
            return PAW_BTRUE;
        }
    }
    return PAW_BFALSE;
}
