// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "gc.h"
#include "mem.h"
#include "rt.h"
#include "util.h"
#include <string.h>

static size_t list_capacity(const List *a)
{
    return CAST_SIZE(a->upper - a->begin);
}

static void realloc_list(paw_Env *P, List *a, size_t alloc0, size_t alloc)
{
    const size_t end = pawV_list_length(a);
    pawM_resize(P, a->begin, alloc0, alloc);
    a->end = a->begin + end;
    a->upper = a->begin + alloc;
    CHECK_GC(P);
}

static void ensure_space(paw_Env *P, List *a, size_t have, size_t want)
{
    if (want > PAW_SIZE_MAX / sizeof(Value)) {
        pawM_error(P);
    }
    // Use the next power-of-2.
    size_t n = 1;
    while (n < want) {
        n *= 2;
    }
    realloc_list(P, a, have, n);
}

static void reserve_extra(paw_Env *P, List *a, size_t extra)
{
    paw_assert(extra > 0);
    if (extra <= CAST_SIZE(a->upper - a->end)) {
        return; // Still have enough space
    }
    const size_t have = list_capacity(a);
    ensure_space(P, a, have, have + extra);
}

static void move_items(Value *src, ptrdiff_t shift, size_t count)
{
    memmove(src + shift, src, CAST_SIZE(count) * sizeof(src[0]));
}

void pawV_list_reserve(paw_Env *P, List *a, size_t want)
{
    const size_t have = list_capacity(a);
    if (want <= have) {
        return;
    }
    ensure_space(P, a, have, want);
}

void pawV_list_push(paw_Env *P, List *a, Value v)
{
    reserve_extra(P, a, 1);
    *a->end++ = v;
}

void pawV_list_resize(paw_Env *P, List *a, size_t length)
{
    pawV_list_reserve(P, a, length);
    // avoid 'Nullptr with offset' from UBSan
    a->end = length ? a->begin + length : a->begin;
}

void pawV_list_insert(paw_Env *P, List *a, paw_Int index, Value v)
{
    // Clamp to the list bounds.
    const size_t len = pawV_list_length(a);
    const paw_Int abs = pawV_abs_index(index, len);
    const size_t i = PAW_CLAMP(CAST_SIZE(abs), 0, len);

    reserve_extra(P, a, 1);
    if (i != len) {
        move_items(a->begin + abs, 1, len - i);
    }
    a->begin[abs] = v;
    ++a->end;
}

void pawV_list_pop(paw_Env *P, List *a, paw_Int index)
{
    const size_t len = pawV_list_length(a);
    const paw_Int fixed = pawV_abs_index(index, len);
    const size_t abs = pawV_check_abs(P, fixed, len, "list");
    if (abs != len - 1) {
        // Shift values into place
        move_items(a->begin + abs + 1, -1, len - abs - 1);
    }
    --a->end;
}

List *pawV_list_new(paw_Env *P)
{
    List *a = pawM_new(P, List);
    pawG_add_object(P, CAST_OBJECT(a), VLIST);
    return a;
}

void pawV_list_free(paw_Env *P, List *a)
{
    pawM_free_vec(P, a->begin, list_capacity(a));
    pawM_free(P, a);
}

List *pawV_list_clone(paw_Env *P, Value *pv, const List *a)
{
    List *a2 = pawV_list_new(P);
    V_SET_OBJECT(pv, a2); // anchor
    if (pawV_list_length(a)) {
        pawV_list_resize(P, a2, pawV_list_length(a));
        memcpy(a2->begin, a->begin, sizeof(a->begin[0]) * pawV_list_length(a));
    }
    return a2;
}

static paw_Bool elems_equal(Value x, Value y)
{
    // TODO: Only allowed for 'basic' types right now. Compiler set to complain
    // otherwise.
    return x.u == y.u;
}

paw_Bool pawV_list_equals(paw_Env *P, const List *lhs, const List *rhs)
{
    const size_t len = pawV_list_length(lhs);
    if (len != pawV_list_length(rhs)) {
        return PAW_FALSE;
    }
    for (size_t i = 0; i < len; ++i) {
        if (!elems_equal(lhs->begin[i], rhs->begin[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

paw_Bool pawV_list_contains(paw_Env *P, const List *a, const Value v)
{
    for (size_t i = 0; i < pawV_list_length(a); ++i) {
        if (elems_equal(v, a->begin[i])) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}