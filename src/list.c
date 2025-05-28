// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: variables of type "size_t" should be used to represent sizes in bytes,
// TODO: while variables of type "paw_Int" are used for list lengths in elements.

#include "list.h"
#include "gc.h"
#include "mem.h"

#define LIST_MIN_CAPACITY 4
#define LIST_MAX_CAPACITY (PAW_SIZE_MAX / sizeof(Value))

static size_t abs_index(paw_Env *P, paw_Int index, paw_Int length)
{
    paw_Int const abs = pawV_abs_index(index, length);
    if (abs < 0 || abs > length)
        pawE_error(P, PAW_ERUNTIME, -1,
                   "index %I is out of bounds for list of length %I",
                   index, PAW_CAST_INT(length));
    return CAST_SIZE(abs);
}

Tuple *pawList_new(paw_Env *P, int element_size, paw_Int capacity, Value *out)
{
    if (capacity > LIST_MAX_CAPACITY / element_size)
        pawM_error(P);

    Tuple *t = pawV_new_tuple(P, 4);
    LIST_ZELEMENT(t) = element_size;
    // clear list components so the GC knows not to read them
    LIST_BEGIN(t) = LIST_END(t) = LIST_BOUND(t) = NULL;
    t->kind = TUPLE_LIST;
    V_SET_OBJECT(out, t);

    int const num_values = PAW_MAX(capacity, LIST_MIN_CAPACITY) * element_size;
    LIST_BEGIN(t) = LIST_END(t) = pawM_new_vec(P, num_values, Value);
    LIST_BOUND(t) = LIST_BEGIN(t) + num_values;
    return t;
}

static size_t list_capacity(Tuple const *t)
{
    return CAST_SIZE((LIST_BOUND(t) - LIST_BEGIN(t)) / LIST_ZELEMENT(t));
}

static void realloc_list(paw_Env *P, Tuple *t, size_t size0, size_t size)
{
    size0 *= LIST_ZELEMENT(t);
    size *= LIST_ZELEMENT(t);
    size_t const end = pawList_raw_length(t);
    Value *p = pawM_resize_(P, LIST_BEGIN(t), size0, size, sizeof(*p));
    LIST_BEGIN(t) = p;
    LIST_END(t) = p + end;
    LIST_BOUND(t) = p + size;
    CHECK_GC(P);
}

static void ensure_space(paw_Env *P, Tuple *t, size_t have, size_t want)
{
    // multiply by 1.5 until 'n' is large enough
    size_t n = PAW_MAX(have, LIST_MIN_CAPACITY);
    while (n < want) {
        size_t const half_n = n / 2;
        if (n > LIST_MAX_CAPACITY - half_n) {
            pawM_error(P);
        }
        n += half_n;
    }
    realloc_list(P, t, have, n);
}

static void reserve_extra(paw_Env *P, Tuple *t, size_t extra)
{
    paw_assert(extra > 0);
    if (extra > CAST_SIZE((LIST_BOUND(t) - LIST_END(t)) / LIST_ZELEMENT(t))) {
        size_t const have = list_capacity(t);
        ensure_space(P, t, have, have + extra);
    }
}

static void shift_elements(Value *src, ptrdiff_t start, ptrdiff_t shift, size_t count, int element_size)
{
    size_t const z = (size_t)element_size;
    memmove(src + (start + shift) * z,
            src + start * z,
            CAST_SIZE(count) * sizeof(src[0]) * z);
}

void pawList_reserve(paw_Env *P, Tuple *t, size_t want)
{
    size_t const have = list_capacity(t);
    if (want <= have)
        return;

    ensure_space(P, t, have, want);
}

void pawList_push(paw_Env *P, Tuple *t, Value const *pvalue)
{
    reserve_extra(P, t, 1);
    LIST_END(t) = pawV_copy(LIST_END(t), pvalue, LIST_ZELEMENT(t));
}

void pawList_resize(paw_Env *P, Tuple *t, size_t length)
{
    pawList_reserve(P, t, length);

    // avoid 'Nullptr with offset' from UBSan
    int const z = LIST_ZELEMENT(t);
    LIST_END(t) = length ? LIST_BEGIN(t) + length * z : LIST_BEGIN(t);
}

void pawList_insert(paw_Env *P, Tuple *t, paw_Int index, Value const *pvalue)
{
    int const z = LIST_ZELEMENT(t);

    size_t const len = pawList_length(P, t);
    size_t const abs = abs_index(P, index, len);

    reserve_extra(P, t, 1);
    if (abs != len)
        shift_elements(LIST_BEGIN(t), abs, 1, len - abs, z);

    pawV_copy(LIST_BEGIN(t) + abs, pvalue, z);
    ++LIST_END(t);
}

void pawList_pop(paw_Env *P, Tuple *t, paw_Int index)
{
    size_t const len = pawList_length(P, t);
    size_t const abs = abs_index(P, index, len);
    if (abs < len - 1) {
        // shift values into place
        shift_elements(LIST_BEGIN(t), abs + 1, -1, len - abs - 1, LIST_ZELEMENT(t));
    } else if (abs == len) {
        pawE_error(P, PAW_ERUNTIME, -1,
                   "popped index %I must be less than list length %I",
                   index, PAW_CAST_INT(len));
    }

    --LIST_END(t);
}

void pawList_copy(paw_Env *P, Tuple const *a, Tuple *b)
{
    paw_Int const na = pawList_length(P, a);
    if (na > 0) {
        pawList_resize(P, b, na);
        size_t const z = LIST_ZELEMENT(a) * sizeof(*LIST_BEGIN(a));
        memcpy(LIST_BEGIN(b), LIST_BEGIN(a), na * z);
    }
}

static void check_slice_bounds(paw_Env *P, size_t *pi, size_t *pj, paw_Int i, paw_Int j, paw_Int n)
{
    *pi = abs_index(P, i, n);
    *pj = abs_index(P, j, n);
    if (*pi > *pj)
        pawE_error(P, PAW_ERUNTIME, -1, "list slice \"start\" is greater than \"end\"");
}

void pawList_get_range(paw_Env *P, Tuple const *t, paw_Int lower, paw_Int upper, Tuple *out)
{
    size_t i, j;
    check_slice_bounds(P, &i, &j, lower, upper, pawList_length(P, t));
    if (i == j) return;

    size_t const n = j - i;
    pawList_resize(P, out, n);
    size_t const z = LIST_ZELEMENT(out);
    size_t const offset = i * z;
    for (size_t ii = 0; ii < n * z; ++ii)
        LIST_BEGIN(out)[ii] = LIST_BEGIN(t)[ii + offset];
}

void pawList_set_range(paw_Env *P, Tuple *a, paw_Int lower, paw_Int upper, Tuple const *b, Value *rtemp)
{
    paw_Int const na = pawList_length(P, a);
    paw_Int const nb = pawList_length(P, b);

    size_t i, j;
    check_slice_bounds(P, &i, &j, lower, upper, na);
    if (i == j && nb == 0) return;

    // "0 <= i <= j" and "j <= na" are both true, meaning the left-hand side of the
    // comparison below must be less than or equal to "na"
    if (na - j + i > PAW_SIZE_MAX - nb)
        pawM_error(P);

    if (a == b) {
        // prevent overlapping range memcpy for "list[i:j] = list"
        Tuple *temp = pawList_new(P, LIST_ZELEMENT(b), nb, rtemp);
        pawList_copy(P, b, temp);
        b = temp;
    }

    size_t const new_length = na - j + i + nb;
    pawList_resize(P, a, new_length);

    // make the distance between elements "i" and "j" equal to the length of "b"
    size_t const z = LIST_ZELEMENT(a);
    memmove(LIST_BEGIN(a) + (i + nb) * z,
            LIST_BEGIN(a) + j * z,
            (na - j) * z * sizeof(*LIST_BEGIN(a)));

    size_t const offset = i * z;
    for (size_t ii = 0; ii < nb * z; ++ii)
        LIST_BEGIN(a)[ii + offset] = LIST_BEGIN(b)[ii];
}

void pawList_concat(paw_Env *P, Tuple const *a, Tuple const *b, Value *rout)
{
    paw_Int const na = pawList_raw_length(a);
    paw_Int const nb = pawList_raw_length(b);

    // both "na" and "nb" are guaranteed to be less than PAW_SIZE_MAX
    if (na > PAW_SIZE_MAX - nb) pawM_error(P);
    size_t const nout = CAST_SIZE(na + nb);

    size_t const z = LIST_ZELEMENT(a);
    Tuple *out = pawList_new(P, z, nout, rout);
    if (nout > 0) {
        memcpy(LIST_END(out), LIST_BEGIN(a), na * z * sizeof(*LIST_BEGIN(a)));
        memcpy(LIST_END(out) + na, LIST_BEGIN(b), nb * z * sizeof(*LIST_BEGIN(a)));
        LIST_END(out) += nout * z;
    }
}

void pawList_free(paw_Env *P, Tuple *t)
{
    pawM_free_vec(P, LIST_BEGIN(t), list_capacity(t) * LIST_ZELEMENT(t));
    pawM_free_flex(P, t, t->nelems, sizeof(*LIST_BEGIN(t)));
}

