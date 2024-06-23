// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "map.h"
#include "gc_aux.h"
#include "mem.h"
#include "rt.h"
#include "util.h"
#include <assert.h>
#include <string.h>

// Total number of bytes needed for each map slot
// The map's internal buffer is divided into 3 sections: the first is an array
// of MapMeta, and the last 2 are arrays of Value. Each array has the same length, 
// equal to the map's 'capacity' field. The 'capacity' is guaranteed to be either 
// 0 or a power-of-2 greater than or equal to paw_alignof(Value), so the Value 
// arrays are suitably-aligned.
#define MAP_ITEM_TOTAL (sizeof(MapMeta) + sizeof(Value) * 2)

static inline Value *insert_aux(Map *m, Value key)
{
    MapCursor erased;
    paw_Bool found_erased = PAW_FALSE;
    MapCursor mc = h_cursor_init(m, key);
    while (!h_is_vacant(&mc)) {               
        if (h_is_erased(&mc)) {
            if (!found_erased) {
                // new item replaces the first erased item, continue searching 
                // for a duplicate
                found_erased = PAW_TRUE;
                erased = mc;
            } 
        } else if (h_cursor_key(&mc)->u == key.u) {
            // found a duplicate: replace it
            return h_cursor_value(&mc);
        }
        h_cursor_next(&mc);
    }
    ++m->length;
    mc = found_erased ? erased : mc;
    h_set_state(&mc, MAP_ITEM_OCCUPIED);
    h_cursor_key(&mc)[0] = key;
    return h_cursor_value(&mc);
}

static void add_item(Map *m, Value key, Value value)
{
    Value *pvalue = insert_aux(m, key);
    *pvalue = value;
}

static void rehash_map(Map old_m, Map *m)
{
    MapMeta *mm = old_m.data;
    const Value *ks = pawH_key(&old_m, 0);
    const Value *vs = pawH_value(&old_m, 0);
    const size_t count = m->length;
    m->length = 0;
    for (size_t i = 0; m->length < count; ++i) {
        if (mm[i].state == MAP_ITEM_OCCUPIED) {
            add_item(m, ks[i], vs[i]);
        }
    }
}

static void free_buffer(paw_Env *P, void *buffer, size_t capacity)
{
    pawM_free_(P, buffer, capacity * MAP_ITEM_TOTAL);
}

static void grow_map(paw_Env *P, Map *m)
{
    size_t n = paw_alignof(Value);
    while (n <= m->capacity) {
        n *= 2;
    }

    // NOTE: Allocation might cause an emergency collection. Keys and values are
    //       still reachable until pawM_alloc() returns, so they won't be freed.
    pawM_check_size(P, 0, n, MAP_ITEM_TOTAL);
    const size_t new_size = n * MAP_ITEM_TOTAL;
    void *buffer = pawM_alloc(P, NULL, 0, new_size);
    memset(buffer, MAP_ITEM_VACANT, n * sizeof(MapMeta));
    const Map old_m = *m;

    m->data = buffer;
    m->capacity = n;

    if (old_m.capacity > 0) {
        rehash_map(old_m, m); 
    }
    free_buffer(P, old_m.data, old_m.capacity);
    check_gc(P);
}

Map *pawH_new(paw_Env *P)
{
    Map *m = pawM_new(P, Map);
    pawG_add_object(P, cast_object(m), VMAP);
    return m;
}

void pawH_free(paw_Env *P, Map *m)
{
    free_buffer(P, m->data, m->capacity);
    pawM_free(P, m);
}

#define FILL_FACTOR 4

Value *pawH_create(paw_Env *P, Map *m, Value key)
{
    if (m->length >= m->capacity / FILL_FACTOR) {
        grow_map(P, m);
    }
    return insert_aux(m, key);
}

void pawH_clone(paw_Env *P, StackPtr sp, Map *m)
{
    Map *m2 = pawH_new(P);
    v_set_object(sp, m2);
    pawH_extend(P, m2, m);
}

static paw_Bool items_equal(paw_Env *P, const Value x, const Value y)
{
    StackPtr sp = pawC_stkinc(P, 2);
    sp[0] = y;
    sp[1] = x;

    paw_assert(0); // TODO: need value type
//    pawR_binop(P, BINARY_EQ);

    const paw_Bool b = paw_bool(P, -1);
    paw_pop(P, 1);
    return b;
}

paw_Bool pawH_equals(paw_Env *P, Map *lhs, Map *rhs)
{
    if (lhs->length != rhs->length) {
        return PAW_FALSE;
    }
    MapCursor mc = {lhs, 0};
    while (mc.index < lhs->capacity) {
        Value *v = pawH_action(P, rhs, *h_cursor_key(&mc), MAP_ACTION_NONE);
        if (!v || !items_equal(P, *h_cursor_value(&mc), *v)) {
            return PAW_FALSE;
        }
        ++mc.index;
    }
    return PAW_TRUE;
}

void pawH_extend(paw_Env *P, Map *dst, Map *src)
{
    MapCursor mc = {src, 0};
    while (mc.index < src->capacity) {
        if (h_get_state(&mc) != MAP_ITEM_OCCUPIED) {
            const Value key = *h_cursor_key(&mc);
            Value *value = pawH_action(P, dst, key, MAP_ACTION_CREATE);
            *value = *h_cursor_value(&mc);
        }
        ++mc.index;
    }
}

void pawH_key_error(paw_Env *P, Value key, paw_Type type)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_string(P, &buf, "key '");
    pawC_pushv(P, key);
    pawL_add_value(P, &buf, type);
    pawL_add_string(P, &buf, "' does not exist");
    pawL_add_char(P, &buf, '\0');
    pawL_push_result(P, &buf);
    pawC_throw(P, PAW_EKEY);
}
