// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_ARRAY_H
#define PAW_ARRAY_H

#include "paw.h"
#include "value.h"

Array *pawA_new(paw_Env *P);
void pawA_free(paw_Env *P, Array *a);
paw_Bool pawA_equals(paw_Env *P, Array *lhs, Array *rhs);
paw_Bool pawA_contains(paw_Env *P, Array *a, const Value v);
void pawA_reserve(paw_Env *P, Array *a, size_t capacity);
void pawA_resize(paw_Env *P, Array *a, size_t length);
void pawA_insert(paw_Env *P, Array *a, paw_Int index, Value v);
void pawA_push(paw_Env *P, Array *a, Value v);
void pawA_pop(paw_Env *P, Array *a, paw_Int index);
void pawA_clone(paw_Env *P, StackPtr sp, const Array *a);
Array *pawA_concat(paw_Env *P, const Array *a, const Array *a2);
Value *pawA_get(paw_Env *P, Array *a, paw_Int index);

static inline size_t pawA_length(const Array *a)
{
    return cast_size(a->end - a->begin);
}

static inline paw_Bool pawA_iter(const Array *a, paw_Int *itr)
{
    ++*itr;
    if (*itr < paw_cast_int(pawA_length(a))) {
        return &a->begin[*itr];
    }
    return NULL;
}

#endif // PAW_ARRAY_H
