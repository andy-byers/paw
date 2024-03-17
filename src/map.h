// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_MAP_H
#define PAW_MAP_H

#include "paw.h"
#include "util.h"
#include "value.h"

static inline paw_Bool pawH_is_vacant(Value v)
{
    return v.u == 0;
}

static inline paw_Bool pawH_is_erased(Value v)
{
    return pawV_is_null(v);
}

static inline paw_Bool pawH_is_occupied(Value v)
{
    return !pawH_is_vacant(v) && !pawH_is_erased(v);
}

#define pawH_index(m, k) check_exp(!pawV_is_null(k), pawV_hash(k) & ((m)->capacity - 1))

// Set 'itr' to the index at which the key 'k' is located, or the first index for
// which the function-like macro  'cc' evaluates to true (if 'k' is not found).
// Must not be called if the map has length 0
#define pawH_locate(m, k, cc)                 \
    for (size_t mask = (m)->capacity - 1;;) { \
        Value ki = (m)->keys[itr];            \
        if ((cc)(ki) || pawV_equal(ki, k)) {  \
            break;                            \
        }                                     \
        itr = (itr + 1) & mask;               \
    }

typedef enum MapAction {
    MAP_ACTION_NONE,
    MAP_ACTION_CREATE,
    MAP_ACTION_REMOVE,
} MapAction;

Map *pawH_new(paw_Env *P);
void pawH_free(paw_Env *P, Map *m);
Value *pawH_action(paw_Env *P, Map *map, Value key, MapAction kind);
paw_Bool pawH_equals(paw_Env *P, Map *lhs, Map *rhs);
void pawH_extend(paw_Env *P, Map *dst, Map *src);
void pawH_clone(paw_Env *P, StackPtr sp, Map *m);
void pawH_key_error(paw_Env *P, Value key);

static inline size_t pawH_length(const Map *m)
{
    return m->length;
}

static inline paw_Bool pawH_contains(paw_Env *P, Map *m, Value key)
{
    return pawH_action(P, m, key, MAP_ACTION_NONE);
}

static inline void pawH_insert(paw_Env *P, Map *m, Value key, Value value)
{
    Value *slot = pawH_action(P, m, key, MAP_ACTION_CREATE);
    if (!slot) {
        pawH_key_error(P, key);
    }
    *slot = value;
}

static inline void pawH_remove(paw_Env *P, Map *m, Value key)
{
    if (!pawH_action(P, m, key, MAP_ACTION_REMOVE)) {
        pawH_key_error(P, key);
    }
}

static inline Value *pawH_get(paw_Env *P, Map *m, Value key)
{
    return pawH_action(P, m, key, MAP_ACTION_NONE);
}

static inline void pawH_set(paw_Env *P, Map *m, Value key, Value value)
{
    *pawH_get(P, m, key) = value;
}

static inline paw_Bool pawH_iter(const Map *m, paw_Int *itr)
{
    for (++*itr; *itr < paw_cast_int(m->capacity); ++*itr) {
        Value *k = &m->keys[*itr];
        if (pawH_is_occupied(*k)) {
            return k;
        }
    }
    return NULL;
}

#endif // PAW_MAP_H
