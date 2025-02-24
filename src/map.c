// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "map.h"
#include "gc.h"
#include "mem.h"
#include "rt.h"
#include "util.h"
#include <string.h>

typedef struct MapCursor {
    Tuple *map;
    paw_Int index;
} MapCursor;

typedef enum MapState {
    MAP_ITEM_VACANT,
    MAP_ITEM_ERASED,
    MAP_ITEM_OCCUPIED,
} MapState;

#define MAP_FILL_FACTOR 4
#define MAP_ITEM_SIZE (1 + sizeof(Value) * 2)
#define MAP_MIN_CAPACITY PAW_ALIGNOF(Value)
#define MAP_MAX_CAPACITY (PAW_SIZE_MAX / MAP_ITEM_SIZE)

#define GET_POLICY(P, m) P->map_policies.data[MAP_POLICY(m)]

static paw_Uint map_hash(paw_Env *P, Tuple *m, Value k)
{
    const MapPolicy p = GET_POLICY(P, m);
    if (p.hash.p == NULL) return V_UINT(k);

    // call the custom hash function
    ENSURE_STACK(P, 2);
    *P->top.p++ = p.hash;
    *P->top.p++ = k;
    paw_call(P, 1);

    const paw_Uint r = paw_uint(P, -1);
    paw_pop(P, 1);
    return r;
}

static paw_Bool map_equals(paw_Env *P, Tuple *m, Value a, Value b)
{
    const MapPolicy p = GET_POLICY(P, m);
    if (p.fp) {
        // special case that handles "-0.0 == 0.0"
        return V_FLOAT(a) == V_FLOAT(b);
    } else if (p.equals.p == NULL) {
        return V_UINT(a) == V_UINT(b);
    }

    // call the custom equality comparison function
    ENSURE_STACK(P, 3);
    *P->top.p++ = p.equals;
    *P->top.p++ = a;
    *P->top.p++ = b;
    paw_call(P, 2);

    const paw_Bool r = paw_bool(P, -1);
    paw_pop(P, 1);
    return r;
}

static inline MapState cursor_get_state(MapCursor *mc)
{
    return CAST(unsigned char *, MAP_DATA(mc->map))[mc->index];
}

static inline void cursor_set_state(MapCursor *mc, MapState state)
{
    CAST(unsigned char *, MAP_DATA(mc->map))[mc->index] = state;
}

static inline MapCursor cursor_init(paw_Env *P, Tuple *t, Value key)
{
    return (MapCursor){t, map_hash(P, t, key) & (MAP_CAPACITY(t) - 1)};
}

static inline Value *cursor_key(MapCursor *mc)
{
    return pawMap_key(mc->map, mc->index);
}

static inline Value *cursor_value(MapCursor *mc)
{
    return pawMap_value(mc->map, mc->index);
}

static void cursor_next(MapCursor *mc)
{
    mc->index = (mc->index + 1) & (MAP_CAPACITY(mc->map) - 1);
}

static inline paw_Bool cursor_lookup(paw_Env *P, Tuple *t, Value key, MapCursor *pmc)
{
    *pmc = cursor_init(P, t, key);
    for (paw_Int i = 0;
            i < MAP_CAPACITY(t) && cursor_get_state(pmc) != MAP_ITEM_VACANT;
            ++i, cursor_next(pmc)) {
        if (cursor_get_state(pmc) == MAP_ITEM_OCCUPIED
                && map_equals(P, t, *cursor_key(pmc), key)) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static inline Value *insert_aux(paw_Env *P, Tuple *m, Value key)
{
    MapCursor erased;
    MapCursor mc = cursor_init(P, m, key);
    paw_Bool found_erased = PAW_FALSE;
    for (paw_Int i = 0;
            i < MAP_CAPACITY(m) && cursor_get_state(&mc) != MAP_ITEM_VACANT;
            ++i, cursor_next(&mc)) {
        if (cursor_get_state(&mc) == MAP_ITEM_ERASED) {
            if (!found_erased) {
                // new item replaces the first erased item, continue searching
                // for a duplicate
                found_erased = PAW_TRUE;
                erased = mc;
            }
        } else if (map_equals(P, m, *cursor_key(&mc), key)) {
            // found a duplicate: replace it
            return cursor_value(&mc);
        }
    }
    ++MAP_LENGTH(m);
    mc = found_erased ? erased : mc;
    cursor_set_state(&mc, MAP_ITEM_OCCUPIED);
    *cursor_key(&mc) = key;
    return cursor_value(&mc);
}

static void add_item(paw_Env *P, Tuple *m, Value key, Value value)
{
    Value *pvalue = insert_aux(P, m, key);
    *pvalue = value;
}

static void rehash_map(paw_Env *P, Tuple *m, void *buffer, size_t capacity)
{
    unsigned char *ms = pawMap_state(m, 0);
    const Value *mk = pawMap_key(m, 0);
    const Value *mv = pawMap_value(m, 0);
    const paw_Int length = MAP_LENGTH(m);
    MAP_CAPACITY(m) = capacity;
    MAP_LENGTH(m) = 0;
    MAP_DATA(m) = buffer;

    for (paw_Int i = 0; MAP_LENGTH(m) < length; ++i) {
        if (ms[i] == MAP_ITEM_OCCUPIED) {
            add_item(P, m, mk[i], mv[i]);
        }
    }
}

static void free_buffer(paw_Env *P, void *buffer, paw_Int capacity)
{
    pawM_free_vec(P, buffer, capacity * MAP_ITEM_SIZE);
}

static void resize_map(paw_Env *P, Tuple *m, size_t capacity)
{
    paw_assert(capacity > 0 && (capacity & (capacity - 1)) == 0);
    // NOTE: Allocation might cause an emergency collection. Keys and values are
    //       still reachable until pawM_alloc() returns, so they won't be freed.
    pawM_check_size(P, 0, capacity, MAP_ITEM_SIZE);
    const size_t new_size = capacity * MAP_ITEM_SIZE;
    void *data = pawM_alloc(P, NULL, 0, new_size);
    if (data == NULL) {
        pawE_error(P, PAW_EMEMORY, -1, "cannot allocate map table (out of memory)");
    }
    memset(data, MAP_ITEM_VACANT, capacity);

    void *const data0 = MAP_DATA(m);
    const paw_Int capacity0 = MAP_CAPACITY(m);
    paw_assert(capacity0 >= MAP_MIN_CAPACITY);
    rehash_map(P, m, data, capacity);
    free_buffer(P, data0, capacity0);
    CHECK_GC(P);
}

static void grow_map(paw_Env *P, Tuple *m)
{
    size_t n = PAW_ALIGNOF(Value);
    while (n <= MAP_CAPACITY(m)) n *= 2;
    resize_map(P, m, n);
}

Tuple *pawMap_new(paw_Env *P, int policy, paw_Int capacity, Value *out)
{
    if (capacity > MAP_MAX_CAPACITY) pawM_error(P);

    Tuple *t = pawV_new_tuple(P, 4);
    MAP_POLICY(t) = policy;
    // clear map components so the GC knows not to read them
    MAP_DATA(t) = NULL;
    MAP_LENGTH(t) = MAP_CAPACITY(t) = 0;
    t->kind = TUPLE_MAP;
    V_SET_OBJECT(out, t);

    capacity = PAW_MAX(capacity, MAP_MIN_CAPACITY);
    MAP_DATA(t) = pawM_new_vec(P, capacity * MAP_ITEM_SIZE, char);
    MAP_CAPACITY(t) = capacity;
    return t;
}

Value *pawMap_get(paw_Env *P, Tuple *t, Value key)
{
    MapCursor mc;
    if (MAP_LENGTH(t) == 0) return NULL;
    if (cursor_lookup(P, t, key, &mc)) {
        return cursor_value(&mc);
    }
    return NULL;
}

void pawMap_erase(Tuple *t, paw_Int index)
{
    if (MAP_LENGTH(t) == 0) return;
    MapCursor mc = {t, index};
    paw_assert(cursor_get_state(&mc) == MAP_ITEM_OCCUPIED);
    cursor_set_state(&mc, MAP_ITEM_ERASED);
    --MAP_LENGTH(t);
}

void pawMap_remove(paw_Env *P, Tuple *t, Value key)
{
    MapCursor mc;
    if (MAP_LENGTH(t) == 0) return;
    if (cursor_lookup(P, t, key, &mc)) {
        cursor_set_state(&mc, MAP_ITEM_ERASED);
        --MAP_LENGTH(t);
    }
}

Value *pawMap_insert(paw_Env *P, Tuple *t, Value key, Value value)
{
    Value *pvalue = pawMap_create(P, t, key);
    *pvalue = value;
    return pvalue;
}

paw_Bool pawMap_contains(paw_Env *P, Tuple *t, Value key)
{
    return pawMap_get(P, t, key) != NULL;
}

paw_Bool pawMap_iter(Tuple *t, paw_Int *pi)
{
    for (++*pi; *pi < PAW_CAST_INT(MAP_CAPACITY(t)); ++*pi) {
        const MapState ms = *pawMap_state(t, *pi);
        if (ms == MAP_ITEM_OCCUPIED) return PAW_TRUE;
    }
    return PAW_FALSE;
}

void pawMap_free(paw_Env *P, Tuple *t)
{
    free_buffer(P, MAP_DATA(t), MAP_CAPACITY(t));
    pawM_free_flex(P, t, t->nelems, sizeof(t->elems[0]));
}

static inline Value *map_insert(paw_Env *P, Tuple *t, Value key)
{
    MapCursor erased;
    paw_Bool found_erased = PAW_FALSE;
    MapCursor mc = cursor_init(P, t, key);
    for (paw_Int i = 0;
            i < MAP_CAPACITY(t) && cursor_get_state(&mc) != MAP_ITEM_VACANT;
            ++i, cursor_next(&mc)) {
        if (cursor_get_state(&mc) == MAP_ITEM_ERASED) {
            if (!found_erased) {
                // new item replaces the first erased item, continue searching
                // for a duplicate
                found_erased = PAW_TRUE;
                erased = mc;
            }
        } else if (map_equals(P, t, *cursor_key(&mc), key)) {
            // found a duplicate: replace it
            return cursor_value(&mc);
        }
    }
    ++MAP_LENGTH(t);
    mc = found_erased ? erased : mc;
    cursor_set_state(&mc, MAP_ITEM_OCCUPIED);
    *cursor_key(&mc) = key;
    return cursor_value(&mc);
}

void pawMap_extend(paw_Env *P, Tuple *a, const Tuple *b)
{
    MapCursor mc = {CAST(Tuple *, b), 0};
    while (mc.index < MAP_CAPACITY(b)) {
        if (cursor_get_state(&mc) == MAP_ITEM_OCCUPIED) {
            const Value key = *cursor_key(&mc);
            Value *value = pawMap_create(P, a, key);
            *value = *cursor_value(&mc);
        }
        ++mc.index;
    }
}

void pawMap_reserve(paw_Env *P, Tuple *m, paw_Int capacity)
{
    if (MAP_CAPACITY(m) < capacity) {
        size_t n = PAW_ALIGNOF(Value);
        while (n <= capacity) n *= 2;
        resize_map(P, m, n * MAP_FILL_FACTOR);
    }
}

Value *pawMap_create(paw_Env *P, Tuple *m, Value key)
{
    if (MAP_LENGTH(m) >= MAP_CAPACITY(m) / MAP_FILL_FACTOR) {
        grow_map(P, m);
    }
    return insert_aux(P, m, key);
}

