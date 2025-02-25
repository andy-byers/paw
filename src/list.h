// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// list.h: List<T> API
//
// List<T> memory layout:
//
//      offset | size | name
//     --------|------|-------
//       0     | 8    | begin
//       8     | 8    | end
//      16     | 8    | bound
//

#ifndef PAW_LIST_H
#define PAW_LIST_H

#include "value.h"

#define LIST_BEGIN(List_) (*CAST(Value **, &(List_)->elems[0].p))
#define LIST_END(List_) (*CAST(Value **, &(List_)->elems[1].p))
#define LIST_BOUND(List_) (*CAST(Value **, &(List_)->elems[2].p))

Tuple *pawList_new(paw_Env *P, paw_Int capacity, Value *out);
void pawList_free(paw_Env *P, Tuple *t);
void pawList_reserve(paw_Env *P, Tuple *t, size_t length);
void pawList_resize(paw_Env *P, Tuple *t, size_t length);
void pawList_insert(paw_Env *P, Tuple *t, paw_Int index, Value v);
void pawList_push(paw_Env *P, Tuple *t, Value v);
void pawList_get_range(paw_Env *P, Tuple const *t, paw_Int i, paw_Int j, Tuple *out);
void pawList_set_range(paw_Env *P, Tuple *a, paw_Int i, paw_Int j, Tuple const *b, Value *rtemp);
void pawList_concat(paw_Env *P, Tuple const *a, Tuple const *b, Value *rout);
void pawList_pop(paw_Env *P, Tuple *t, paw_Int index);

inline static paw_Int pawList_length(Tuple const *t)
{
    return PAW_CAST_INT(LIST_END(t) - LIST_BEGIN(t));
}

inline static Value *pawList_get(paw_Env *P, Tuple *t, paw_Int index)
{
    size_t const length = CAST_SIZE(pawList_length(t));
    size_t const absolute = pawV_check_abs(P, index, length, "list");
    return &LIST_BEGIN(t)[absolute];
}

inline static paw_Bool pawList_iter(Tuple const *t, paw_Int *itr)
{
    return ++*itr < PAW_CAST_INT(pawList_length(t));
}

#endif // PAW_LIST_H
