// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "map.h"
#include "error.h"
#include "gc.h"
#include "mem.h"
#include "rt.h"
#include "util.h"
#include <assert.h>
#include <string.h>

#define set_erased(p) pawV_set_null(p)
#define hash_key(m, k) (pawV_hash(k) & ((m)->capacity - 1))
#define next_idx(m, i) ((i + 1) & ((m)->capacity - 1))

static void check_key(paw_Env *P, Value key)
{
    if (!pawV_is_string(key) && !pawV_is_int(key) && !pawV_is_bigint(key) && !pawV_is_bool(key)) {
        pawE_type(P, "map access");
    }
}

static paw_Bool is_unoccupied(Value v)
{
    return pawH_is_vacant(v) || pawH_is_erased(v);
}

static size_t prepare_insert(Map *m, Value key)
{
    paw_assert(!pawV_is_null(key));
    size_t itr = pawH_index(m, key);
    pawH_locate(m, key, is_unoccupied);
    // Search for the first vacant slot
    for (; !pawH_is_vacant(m->keys[itr]); itr = next_idx(m, itr)) {
        const Value k = m->keys[itr];
        if (pawH_is_occupied(k) && pawV_equal(k, key)) {
            break;
        }
    }
    return itr;
}

static void add_item(Map *m, Value key, Value value)
{
    const size_t i = prepare_insert(m, key);
    m->keys[i] = key;
    m->values[i] = value;
    ++m->length;
}

static void grow_map(paw_Env *P, Map *m)
{
    size_t n = 4;
    while (n <= m->capacity) {
        n *= 2;
    }

    // NOTE: pawM_new*() might cause an emergency collection. m->keys and m->values
    //       are still reachable until pawM_new() returns, so they won't be freed. The
    //       calls to add_item() below will never cause an allocation.
    Value *buffer = pawM_new_vec(P, n * 2, Value);
    const size_t old_n = m->capacity;

    Value *keys = m->keys;
    Value *values = m->values;
    m->keys = buffer;
    m->values = m->keys + n;
    m->capacity = n;

    const size_t count = m->length;
    m->length = 0;

    for (size_t i = 0; m->length < count; ++i) {
        if (pawH_is_occupied(keys[i])) {
            add_item(m, keys[i], values[i]);
        }
    }
    pawM_free_vec(P, keys, old_n * 2);
    CHECK_GC(P);
}

static void grow_map_if_necessary(paw_Env *P, Map *m)
{
    if (m->length >= m->capacity / 4) {
        grow_map(P, m);
    }
}

Map *pawH_new(paw_Env *P)
{
    Map *m = pawM_new(P, Map);
    pawG_add_object(P, cast_object(m), VMAP);
    return m;
}

void pawH_free(paw_Env *P, Map *m)
{
    pawM_free_vec(P, m->keys, m->capacity * 2);
    pawM_free(P, m);
}

Value *pawH_action(paw_Env *P, Map *m, Value key, MapAction action)
{
    check_key(P, key);
    if (action == MAP_ACTION_CREATE) {
        grow_map_if_necessary(P, m);
        const size_t i = prepare_insert(m, key);
        if (!pawH_is_occupied(m->keys[i])) {
            m->keys[i] = key;
            pawV_set_null(&m->values[i]);
            ++m->length;
        }
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
        set_erased(&m->keys[itr]);
        pawV_set_null(&m->values[itr]);
        --m->length;
        // Return the address of the slot to indicate success.
        return &m->keys[itr];
    }
    paw_assert(action == MAP_ACTION_NONE);
    return &m->values[itr];
}

void pawH_clone(paw_Env *P, StackPtr sp, Map *m)
{
    Map *m2 = pawH_new(P);
    pawV_set_map(sp, m2);
    pawH_extend(P, m2, m);
}

static paw_Bool items_equal(paw_Env *P, const Value x, const Value y)
{
    StackPtr sp = pawC_stkinc(P, 2);
    sp[0] = y;
    sp[1] = x;

    pawR_eq(P);

    const paw_Bool b = paw_boolean(P, -1);
    paw_pop(P, 1);
    return b;
}

paw_Bool pawH_equals(paw_Env *P, Map *lhs, Map *rhs)
{
    if (lhs->length != rhs->length) {
        return PAW_BFALSE;
    }
    paw_Int i = PAW_ITER_INIT;
    while (pawH_iter(lhs, &i)) {
        Value *v = pawH_action(P, rhs, lhs->keys[i], MAP_ACTION_NONE);
        if (!v || !items_equal(P, lhs->values[i], *v)) {
            return PAW_BFALSE;
        }
    }
    return PAW_BTRUE;
}

void pawH_extend(paw_Env *P, Map *dst, Map *src)
{
    for (size_t i = 0; i < src->capacity; ++i) {
        Value key = src->keys[i];
        if (pawH_is_occupied(key)) {
            Value *value = pawH_action(P, dst, key, MAP_ACTION_CREATE);
            *value = src->values[i];
        }
    }
}

void pawH_key_error(paw_Env *P, Value key)
{
    pawC_stkpush(P, key);
    pawE_error(P, PAW_EKEY, pawV_get_string(key)->text);
}
