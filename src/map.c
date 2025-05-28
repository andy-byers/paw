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
#define MAP_MIN_CAPACITY PAW_ALIGNOF(Value)
#define MAP_MAX_CAPACITY(Policy_) (PAW_SIZE_MAX / MAP_ITEM_SIZE(Policy_))
#define MAP_ITEM_SIZE(Policy_) (1 + (Policy_).key_size * sizeof(Value) + (Policy_).value_size * sizeof(Value))

int pawMap_key_size(paw_Env *P, Tuple const *t)
{
    return GET_POLICY(P, t).key_size;
}

int pawMap_value_size(paw_Env *P, Tuple const *t)
{
    return GET_POLICY(P, t).value_size;
}

Value *pawMap_key(paw_Env *P, Tuple *m, paw_Int index)
{
    unsigned char *data = CAST(unsigned char *, MAP_DATA(m));
    return &CAST(Value *, data + MAP_CAPACITY(m))[index * pawMap_key_size(P, m)];
}

Value *pawMap_value(paw_Env *P, Tuple *m, paw_Int index)
{
    Value *data = pawMap_key(P, m, MAP_CAPACITY(m));
    return &data[index * pawMap_value_size(P, m)];
}

static paw_Uint map_hash(paw_Env *P, Tuple *m, Value const **pk)
{
    MapPolicy const p = GET_POLICY(P, m);
    if (p.hash.p == NULL)
        return V_UINT(**pk);

    ptrdiff_t const offset = SAVE_OFFSET(P, *pk);

    // call the custom hash function
    ENSURE_STACK(P, 1 + p.key_size);
    *P->top.p++ = p.hash;
    P->top.p = pawV_copy(P->top.p, *pk, p.key_size);
    paw_call(P, p.key_size);

    *pk = RESTORE_POINTER(P, offset);
    paw_Uint const r = paw_uint(P, -1);
    paw_pop(P, 1);
    return r;
}

inline static Value *cursor_key(paw_Env *P, MapCursor *mc);

static paw_Bool map_equals(paw_Env *P, Tuple *m, MapCursor *mc, Value const **pk)
{
    MapPolicy const p = GET_POLICY(P, m);
    Value const *a = cursor_key(P, mc);
    Value const *b = *pk;
    if (p.fp) {
        // special case that handles "-0.0 == 0.0"
        return V_FLOAT(*a) == V_FLOAT(*b);
    } else if (p.equals.p == NULL) {
        return V_UINT(*a) == V_UINT(*b);
    }
    ptrdiff_t const offset = SAVE_OFFSET(P, *pk);

    // call the custom equality comparison function
    int const param_size = p.key_size * 2;
    ENSURE_STACK(P, 1 + param_size);
    *P->top.p++ = p.equals;
    P->top.p = pawV_copy(P->top.p, a, p.key_size);
    P->top.p = pawV_copy(P->top.p, b, p.key_size);
    paw_call(P, param_size);

    *pk = RESTORE_POINTER(P, offset);
    paw_Bool const r = paw_bool(P, -1);
    paw_pop(P, 1);
    return r;
}

inline static MapState cursor_get_state(MapCursor *mc)
{
    return CAST(unsigned char *, MAP_DATA(mc->map))[mc->index];
}

inline static void cursor_set_state(MapCursor *mc, MapState state)
{
    CAST(unsigned char *, MAP_DATA(mc->map))[mc->index] = state;
}

inline static MapCursor cursor_init(paw_Env *P, Tuple *t, Value const **ppkey)
{
    return (MapCursor){t, map_hash(P, t, ppkey) & (MAP_CAPACITY(t) - 1)};
}

inline static Value *cursor_key(paw_Env *P, MapCursor *mc)
{
    return pawMap_key(P, mc->map, mc->index);
}

inline static Value *cursor_value(paw_Env *P, MapCursor *mc)
{
    return pawMap_value(P, mc->map, mc->index);
}

static void cursor_next(MapCursor *mc)
{
    mc->index = (mc->index + 1) & (MAP_CAPACITY(mc->map) - 1);
}

inline static paw_Bool cursor_lookup(paw_Env *P, Tuple *t, Value const *pkey, MapCursor *pmc)
{
    *pmc = cursor_init(P, t, &pkey);
    for (paw_Int i = 0;
         i < MAP_CAPACITY(t) && cursor_get_state(pmc) != MAP_ITEM_VACANT;
         ++i, cursor_next(pmc)) {
        if (cursor_get_state(pmc) == MAP_ITEM_OCCUPIED //
                && map_equals(P, t, pmc, &pkey)) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

inline static Value *insert_aux(paw_Env *P, Tuple *m, Value const *pkey)
{
    MapCursor erased;
    MapCursor mc = cursor_init(P, m, &pkey);
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
        } else if (map_equals(P, m, &mc, &pkey)) {
            paw_assert(cursor_get_state(&mc) == MAP_ITEM_OCCUPIED);
            // found a duplicate: replace it
            return cursor_value(P, &mc);
        }
    }
    ++MAP_LENGTH(m);
    mc = found_erased ? erased : mc;
    cursor_set_state(&mc, MAP_ITEM_OCCUPIED);
    MapPolicy const p = GET_POLICY(P, m);
    pawV_copy(cursor_key(P, &mc), pkey, p.key_size);
    return cursor_value(P, &mc);
}

static void add_item(paw_Env *P, Tuple *m, Value const *pkey, Value const *pvalue)
{
    Value *pslot = insert_aux(P, m, pkey);
    MapPolicy const p = GET_POLICY(P, m);
    pawV_copy(pslot, pvalue, p.value_size);
}

static void rehash_map(paw_Env *P, Tuple *m, void *buffer, size_t capacity)
{
    unsigned char *ms = pawMap_state(m, 0);
    Value const *mk = pawMap_key(P, m, 0);
    Value const *mv = pawMap_value(P, m, 0);
    paw_Int const length = MAP_LENGTH(m);
    MAP_CAPACITY(m) = capacity;
    MAP_LENGTH(m) = 0;
    MAP_DATA(m) = buffer;

    MapPolicy const p = GET_POLICY(P, m);
    for (paw_Int i = 0; MAP_LENGTH(m) < length; ++i) {
        if (ms[i] == MAP_ITEM_OCCUPIED)
            add_item(P, m, mk, mv);
        mk += p.key_size;
        mv += p.value_size;
    }
}

static void free_buffer(paw_Env *P, void *buffer, paw_Int capacity, int zitem)
{
    pawM_free_vec(P, buffer, capacity * zitem);
}

static void resize_map(paw_Env *P, Tuple *m, size_t capacity)
{
    paw_assert(capacity > 0 && (capacity & (capacity - 1)) == 0);
    struct MapPolicy const p = GET_POLICY(P, m);
    int const zitem = MAP_ITEM_SIZE(p);

    // NOTE: Allocation might cause an emergency collection. Keys and values are
    //       still reachable until pawM_alloc() returns, so they won't be freed.
    pawM_check_size(P, 0, capacity, zitem);
    size_t const new_size = capacity * zitem;
    void *data = pawM_alloc(P, NULL, 0, new_size);
    if (data == NULL) {
        pawE_error(P, PAW_EMEMORY, -1, "cannot allocate map table (out of memory)");
    }
    memset(data, MAP_ITEM_VACANT, capacity);

    void *const data0 = MAP_DATA(m);
    paw_Int const capacity0 = MAP_CAPACITY(m);
    paw_assert(capacity0 >= MAP_MIN_CAPACITY);
    rehash_map(P, m, data, capacity);
    free_buffer(P, data0, capacity0, zitem);
    CHECK_GC(P);
}

static void grow_map(paw_Env *P, Tuple *m)
{
    size_t n = PAW_ALIGNOF(Value);
    while (n <= MAP_CAPACITY(m))
        n *= 2;
    resize_map(P, m, n);
}

Tuple *pawMap_new(paw_Env *P, int policy, paw_Int capacity, Value *out)
{
    Tuple *t = pawV_new_tuple(P, 4);
    MAP_POLICY(t) = policy;

    // clear map components so the GC knows not to read them
    MAP_DATA(t) = NULL;
    MAP_LENGTH(t) = MAP_CAPACITY(t) = 0;
    t->kind = TUPLE_MAP;
    V_SET_OBJECT(out, t);

    struct MapPolicy const p = GET_POLICY(P, t);
    if (capacity > MAP_MAX_CAPACITY(p))
        pawM_error(P);

    capacity = PAW_MAX(capacity, MAP_MIN_CAPACITY);
    MAP_DATA(t) = pawM_new_vec(P, capacity * MAP_ITEM_SIZE(p), char);
    MAP_CAPACITY(t) = capacity;
    return t;
}

Value *pawMap_get(paw_Env *P, Tuple *t, Value const *pkey)
{
    MapCursor mc;
    if (MAP_LENGTH(t) == 0)
        return NULL;
    if (cursor_lookup(P, t, pkey, &mc)) {
        return cursor_value(P, &mc);
    }
    return NULL;
}

void pawMap_erase(Tuple *t, paw_Int index)
{
    if (MAP_LENGTH(t) == 0)
        return;
    MapCursor mc = {t, index};
    paw_assert(cursor_get_state(&mc) == MAP_ITEM_OCCUPIED);
    cursor_set_state(&mc, MAP_ITEM_ERASED);
    --MAP_LENGTH(t);
}

void pawMap_remove(paw_Env *P, Tuple *t, Value const *pkey)
{
    MapCursor mc;
    if (MAP_LENGTH(t) == 0)
        return;
    if (cursor_lookup(P, t, pkey, &mc)) {
        cursor_set_state(&mc, MAP_ITEM_ERASED);
        --MAP_LENGTH(t);
    }
}

Value *pawMap_insert(paw_Env *P, Tuple *t, Value const *pkey, Value const *pvalue)
{
    MapPolicy const p = GET_POLICY(P, t);
    Value *pslot = pawMap_create(P, t, pkey);
    pawV_copy(pslot, pvalue, p.value_size);
    return pslot;
}

paw_Bool pawMap_contains(paw_Env *P, Tuple *t, Value const *pkey)
{
    return pawMap_get(P, t, pkey) != NULL;
}

paw_Bool pawMap_iter(Tuple *t, paw_Int *pi)
{
    for (++*pi; *pi < PAW_CAST_INT(MAP_CAPACITY(t)); ++*pi) {
        MapState const ms = *pawMap_state(t, *pi);
        if (ms == MAP_ITEM_OCCUPIED)
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

void pawMap_free(paw_Env *P, Tuple *t)
{
    struct MapPolicy const p = GET_POLICY(P, t);
    free_buffer(P, MAP_DATA(t), MAP_CAPACITY(t), MAP_ITEM_SIZE(p));
    pawM_free_flex(P, t, t->nelems, sizeof(t->elems[0]));
}

inline static Value *map_insert(paw_Env *P, Tuple *t, Value const *pkey)
{
    MapCursor erased;
    paw_Bool found_erased = PAW_FALSE;
    MapCursor mc = cursor_init(P, t, &pkey);
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
        } else if (map_equals(P, t, &mc, &pkey)) {
            // found a duplicate: replace it
            return cursor_value(P, &mc);
        }
    }
    ++MAP_LENGTH(t);
    mc = found_erased ? erased : mc;
    cursor_set_state(&mc, MAP_ITEM_OCCUPIED);
    MapPolicy const p = GET_POLICY(P, t);
    pawV_copy(cursor_key(P, &mc), pkey, p.key_size);
    return cursor_value(P, &mc);
}

void pawMap_extend(paw_Env *P, Tuple *a, Tuple const *b)
{
    MapPolicy const p = GET_POLICY(P, b);
    MapCursor mc = {CAST(Tuple *, b), 0};
    while (mc.index < MAP_CAPACITY(b)) {
        if (cursor_get_state(&mc) == MAP_ITEM_OCCUPIED) {
            Value const *pkey = cursor_key(P, &mc);
            Value const *pvalue = cursor_value(P, &mc);
            Value *pslot = pawMap_create(P, a, pkey);
            pawV_copy(pslot, pvalue, p.value_size);
        }
        ++mc.index;
    }
}

void pawMap_reserve(paw_Env *P, Tuple *m, paw_Int capacity)
{
    if (MAP_CAPACITY(m) < capacity) {
        size_t n = PAW_ALIGNOF(Value);
        while (n <= capacity)
            n *= 2;
        resize_map(P, m, n * MAP_FILL_FACTOR);
    }
}

Value *pawMap_create(paw_Env *P, Tuple *m, Value const *pkey)
{
    if (MAP_LENGTH(m) >= MAP_CAPACITY(m) / MAP_FILL_FACTOR) {
        ptrdiff_t const offset = SAVE_OFFSET(P, pkey);
        grow_map(P, m);
        pkey = RESTORE_POINTER(P, offset);
    }
    return insert_aux(P, m, pkey);
}

