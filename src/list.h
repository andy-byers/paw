// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// list.h: List<T> API
//
// List<T> memory layout:
//
//      offset | size | name
//     --------|------|--------
//       0     | 8    | policy
//       8     | 8    | begin
//      16     | 8    | end
//      24     | 8    | bound
//

#ifndef PAW_LIST_H
#define PAW_LIST_H

#include "value.h"

#define LIST_ZELEMENT(List_) (V_INT((List_)->elems[0]))
#define LIST_ELEMENT_LIMIT(List_) (PAW_SIZE_MAX / LIST_ZELEMENT(List_))
#define LIST_BEGIN(List_) (*CAST(Value **, &(List_)->elems[1].p))
#define LIST_END(List_) (*CAST(Value **, &(List_)->elems[2].p))
#define LIST_BOUND(List_) (*CAST(Value **, &(List_)->elems[3].p))

Tuple *pawList_new(paw_Env *P, int policy, paw_Int capacity, Value *out);
void pawList_free(paw_Env *P, Tuple *t);
void pawList_reserve(paw_Env *P, Tuple *t, size_t length);
void pawList_resize(paw_Env *P, Tuple *t, size_t length);
void pawList_insert(paw_Env *P, Tuple *t, paw_Int index, Value const *pvalue);
void pawList_push(paw_Env *P, Tuple *t, Value const *pvalue);
void pawList_get_range(paw_Env *P, Tuple const *t, paw_Int i, paw_Int j, Tuple *out);
void pawList_set_range(paw_Env *P, Tuple *a, paw_Int i, paw_Int j, Tuple const *b, Value *rtemp);
void pawList_concat(paw_Env *P, Tuple const *a, Tuple const *b, Value *rout);
void pawList_pop(paw_Env *P, Tuple *t, paw_Int index);

inline static paw_Int pawList_raw_length(Tuple const *t)
{
    return PAW_CAST_INT(LIST_END(t) - LIST_BEGIN(t));
}

static inline paw_Int pawList_length(paw_Env *P, Tuple const *t)
{
    return pawList_raw_length(t) / LIST_ZELEMENT(t);
}

inline static Value *pawList_get(paw_Env *P, Tuple *t, paw_Int index)
{
    size_t const length = CAST_SIZE(pawList_length(P, t));
    size_t const absolute = pawV_check_abs(P, index, length, "list");
    return &LIST_BEGIN(t)[absolute * LIST_ZELEMENT(t)];
}

inline static paw_Bool pawList_iter(paw_Env *P, Tuple const *t, paw_Int *itr)
{
    return ++*itr < pawList_length(P, t);
}

#endif // PAW_LIST_H
