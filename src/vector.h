// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_ARRAY_H
#define PAW_ARRAY_H

#include "paw.h"
#include "value.h"

Vector *pawA_new(paw_Env *P);
void pawA_free(paw_Env *P, Vector *a);
paw_Bool pawA_equals(paw_Env *P, const Vector *lhs, const Vector *rhs);
paw_Bool pawA_contains(paw_Env *P, const Vector *a, Value v);
void pawA_reserve(paw_Env *P, Vector *a, size_t capacity);
void pawA_resize(paw_Env *P, Vector *a, size_t length);
void pawA_insert(paw_Env *P, Vector *a, paw_Int index, Value v);
void pawA_push(paw_Env *P, Vector *a, Value v);
void pawA_pop(paw_Env *P, Vector *a, paw_Int index);
Vector *pawA_clone(paw_Env *P, StackPtr sp, const Vector *a);
void pawA_index_error(paw_Env *P, paw_Int index, size_t length);

static paw_Int pawA_abs_index(paw_Int index, size_t length)
{
    return index + (index < 0 ? paw_cast_int(length) : 0);
}

static inline size_t pawA_check_abs(paw_Env *P, paw_Int index, size_t length)
{
    index = pawA_abs_index(index, length);
    if (index < 0 || cast_size(index) >= length) {
        pawA_index_error(P, index, length);
    }
    return cast_size(index);
}

static inline size_t pawA_length(const Vector *a)
{
    return cast_size(a->end - a->begin);
}

static inline Value *pawA_get(paw_Env *P, Vector *a, paw_Int index)
{
    const paw_Int abs = pawA_abs_index(index, cast_size(a->end - a->begin));
    const size_t i = pawA_check_abs(P, abs, pawA_length(a));
    return &a->begin[i];
}

static inline paw_Bool pawA_iter(const Vector *a, paw_Int *itr)
{
    return ++*itr < paw_cast_int(pawA_length(a));
}

#endif // PAW_ARRAY_H
