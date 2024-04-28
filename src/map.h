// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_MAP_H
#define PAW_MAP_H

// TODO: This code won't work properly anymore: we do not have a way to indicate that a
//       given key is a 'tombstone', without limiting what values can be used (using -1
//       value, but a valid integer could easily be -1). Before, we just used 'null'. Only
//       works for pointer keys right now.
//       Ideas:
//         (+) Just use chaining.
//         (+) Reserve a single key value to represent 'null', or 'does not exist'.
//             Use the value field to indicate either that the item never existed, or that
//             it was erased. Problematic, as it limits the keyspace.
//         (+) Use a separate array (a bitfield, really) to track which keys are nonexistent.
//         (+) Create a somewhat more complicated data structure with an 'index' (see Python
//             'dict' implementation). 

#include "paw.h"
#include "util.h"
#include "value.h"

static inline paw_Bool pawH_is_vacant(Value key)
{
    return key.u == 0;
}

static inline paw_Bool pawH_is_erased(Value key)
{
    return key.i == -1;
}

static inline paw_Bool pawH_is_occupied(Value key)
{
    return !pawH_is_vacant(key) && !pawH_is_erased(key);
}

#define pawH_index(m, k) check_exp(pawH_is_occupied(k), pawV_hash(k) & ((m)->capacity - 1))

// Set 'itr' to the index at which the key 'k' is located, or the first index for
// which the function-like macro  'cc' evaluates to true (if 'k' is not found).
// Must not be called if the map has length 0
#define pawH_locate(m, k, cc)                 \
    for (size_t mask = (m)->capacity - 1;;) { \
        Value ki = (m)->keys[itr];            \
        if ((cc)(ki) || (ki).u == (k).u) {    \
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
paw_Bool pawH_equals(paw_Env *P, Map *lhs, Map *rhs);
void pawH_extend(paw_Env *P, Map *dst, Map *src);
void pawH_clone(paw_Env *P, StackPtr sp, Map *m);
void pawH_key_error(paw_Env *P, Value key);
size_t pawH_create_aux_(paw_Env *P, Map *m, Value key);

static inline size_t pawH_length(const Map *m)
{
    return m->length;
}

//void pawH_reserve(paw_Env *P, Map *m, size_t length);
//
//static size_t pawH_search(paw_Env *P, Map *m, Value key)
//{
//    pawH_reserve(P, m, pawH_length(m) + 1);
//    size_t itr = pawH_index(m, key);
//    pawH_locate(m, key, pawH_is_vacant);
//    return itr;
//}

static inline Value *pawH_action(paw_Env *P, Map *m, Value key, MapAction action)
{
    if (action == MAP_ACTION_CREATE) {
        const size_t i = pawH_create_aux_(P, m, key);
        return &m->values[i];
    } else if (m->length == 0) {
        return NULL;
    }
    size_t itr = pawH_index(m, key);
    pawH_locate(m, key, pawH_is_vacant);
    if (!pawH_is_occupied(m->keys[itr])) {
        return NULL;
    }
    if (action == MAP_ACTION_REMOVE) {
        m->keys[itr].i = -1; // tombstone
        --m->length;

        // Return the address of the slot to indicate success.
        return &m->keys[itr];
    }
    paw_assert(action == MAP_ACTION_NONE);
    return &m->values[itr];
}

static inline paw_Bool pawH_contains(paw_Env *P, Map *m, Value key)
{
    return pawH_action(P, m, key, MAP_ACTION_NONE) != NULL;
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
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

#endif // PAW_MAP_H
