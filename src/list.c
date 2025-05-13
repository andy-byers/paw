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

Tuple *pawList_new(paw_Env *P, int zelem, paw_Int capacity, Value *out)
{
    if (capacity > LIST_MAX_CAPACITY / zelem)
        pawM_error(P);

    Tuple *t = pawV_new_tuple(P, 4);
    LIST_ZELEMENT(t) = zelem;
    // clear list components so the GC knows not to read them
    LIST_BEGIN(t) = LIST_END(t) = LIST_BOUND(t) = NULL;
    t->kind = TUPLE_LIST;
    V_SET_OBJECT(out, t);

    int const num_values = PAW_MAX(capacity, LIST_MIN_CAPACITY) * zelem;
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

static void move_items(int element_size, Value *src, ptrdiff_t start, ptrdiff_t shift, size_t count)
{
    size_t const z = (size_t)element_size;
    memmove(src + (start + shift) * z, src + start * z, CAST_SIZE(count) * sizeof(src[0]) * z);
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
    int const zelem = LIST_ZELEMENT(t);
    LIST_END(t) = pawV_copy(LIST_END(t), pvalue, zelem);
}

void pawList_resize(paw_Env *P, Tuple *t, size_t length)
{
    pawList_reserve(P, t, length);
    // avoid 'Nullptr with offset' from UBSan
    int const zelem = LIST_ZELEMENT(t);
    LIST_END(t) = length ? LIST_BEGIN(t) + length * zelem : LIST_BEGIN(t);
}

void pawList_insert(paw_Env *P, Tuple *t, paw_Int index, Value const *pvalue)
{
    int const zelem = LIST_ZELEMENT(t);

    // Clamp to the list bounds.
    size_t const len = pawList_length(P, t);
    paw_Int const abs = pawV_abs_index(index, len);
    size_t const i = PAW_CLAMP(CAST_SIZE(abs), 0, len);

    reserve_extra(P, t, 1);
    if (i != len)
        move_items(zelem, LIST_BEGIN(t), abs, 1, len - i);

    pawV_copy(LIST_BEGIN(t) + abs, pvalue, zelem);
    ++LIST_END(t);
}

void pawList_pop(paw_Env *P, Tuple *t, paw_Int index)
{
    int const zelem = LIST_ZELEMENT(t);

    size_t const len = pawList_length(P, t);
    paw_Int const fixed = pawV_abs_index(index, len);
    size_t const abs = pawV_check_abs(P, fixed, len, "list");
    if (abs != len - 1)
        // shift values into place
        move_items(zelem, LIST_BEGIN(t), abs + 1, -1, len - abs - 1);

    --LIST_END(t);
}

void pawList_copy(paw_Env *P, Tuple const *a, Tuple *b)
{
    paw_Int const na = pawList_length(P, a);
    if (na > 0) {
        pawList_resize(P, b, na);
        memcpy(LIST_BEGIN(b), LIST_BEGIN(a), na * sizeof(*LIST_BEGIN(a)));
    }
}

static paw_Int check_slice_bound(paw_Env *P, paw_Int index, paw_Int length, char const *what)
{
    paw_Int const n = PAW_CAST_INT(length);
    index = pawV_abs_index(index, length);
    if (index < 0 || index > n) {
        pawE_error(P, PAW_ERUNTIME, -1,
                   "slice %s index %I is out of bounds for list of length %I",
                   what, index, PAW_CAST_INT(length));
    }
    return index;
}

void pawList_get_range(paw_Env *P, Tuple const *t, paw_Int i, paw_Int j, Tuple *out)
{
    paw_Int const n = pawList_length(P, t);
    i = check_slice_bound(P, i, n, "start");
    j = check_slice_bound(P, j, n, "end");
    j = PAW_MAX(i, j);

    size_t const nout = i < j ? j - i : 0;
    if (nout > 0) {
        pawList_resize(P, out, nout);
        // TODO "this is broken"
        memcpy(LIST_BEGIN(out), LIST_BEGIN(t) + i, nout * sizeof(*LIST_BEGIN(out)));
    }
}

void pawList_set_range(paw_Env *P, Tuple *a, paw_Int i, paw_Int j, Tuple const *b, Value *rtemp)
{
    paw_Int const na = pawList_length(P, a);
    paw_Int const nb = pawList_length(P, b);
    i = check_slice_bound(P, i, na, "start");
    j = check_slice_bound(P, j, na, "end");
    j = PAW_MAX(i, j);

    // "0 <= i <= j" and "j <= na" are both true, meaning the left-hand side of the
    // comparison below must be less than or equal to "na"
    if (na - j + i > PAW_SIZE_MAX - nb)
        pawM_error(P);

    if (a == b) {
    // TODO na should be nb in the line below? doesn't make a difference since a==b, but looks strange
        // prevent overlapping range memcpy for "list[i:j] = list"
        Tuple *temp = pawList_new(P, LIST_ZELEMENT(b), na, rtemp);
        pawList_copy(P, b, temp);
        b = temp;
    }

    size_t const n = na - j + i + nb;
    pawList_reserve(P, a, n);

    Value *gap = LIST_BEGIN(a) + i;
    memmove(gap + nb, LIST_BEGIN(a) + j, (na - j) * sizeof(*LIST_BEGIN(a)));
    memcpy(gap, LIST_BEGIN(b), nb * sizeof(*LIST_BEGIN(a)));
    pawList_resize(P, a, n);
}

void pawList_concat(paw_Env *P, Tuple const *a, Tuple const *b, Value *rout)
{
    paw_Int const na = pawList_raw_length(a);
    paw_Int const nb = pawList_raw_length(b);

    // both "na" and "nb" are guaranteed to be less than PAW_SIZE_MAX
    if (na > PAW_SIZE_MAX - nb)
        pawM_error(P);
    size_t const nout = CAST_SIZE(na + nb);

    Tuple *out = pawList_new(P, LIST_ZELEMENT(a), nout, rout);
    if (nout > 0) {
        memcpy(LIST_END(out), LIST_BEGIN(a), na * sizeof(*LIST_BEGIN(a)));
        memcpy(LIST_END(out) + na, LIST_BEGIN(b), nb * sizeof(*LIST_BEGIN(b)));
        LIST_END(out) += nout;
    }
}

void pawList_free(paw_Env *P, Tuple *t)
{
    pawM_free_vec(P, LIST_BEGIN(t), list_capacity(t) * LIST_ZELEMENT(t));
    pawM_free_flex(P, t, t->nelems, sizeof(t->elems[0]));
}
