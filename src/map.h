// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_MAP_H
#define PAW_MAP_H

#include "paw.h"
#include "util.h"
#include "value.h"

typedef struct MapCursor {
    Map *map;
    size_t index;
} MapCursor;

#define h_is_vacant(mc) (h_get_state(mc) == MAP_ITEM_VACANT)
#define h_is_occupied(mc) (h_get_state(mc) == MAP_ITEM_OCCUPIED)
#define h_is_erased(mc) (h_get_state(mc) == MAP_ITEM_ERASED)

#define pawH_meta(m, index) (&cast((m)->data, MapMeta *)[index])

static inline Value *pawH_key(Map *m, size_t index)
{
    char *data = cast(m->data, char *);
    data += sizeof(MapMeta) * m->capacity;
    return &cast(data, Value *)[index];
}

static inline Value *pawH_value(Map *m, size_t index)
{
    return pawH_key(m, index) + m->capacity;
}

static inline MapState h_get_state(MapCursor *mc)
{
    return cast(mc->map->data, MapMeta *)[mc->index].state;
}

static inline void h_set_state(MapCursor *mc, MapState state)
{
    cast(mc->map->data, MapMeta *)[mc->index].state = state;
}

static inline Value *h_cursor_key(MapCursor *mc)
{
    return pawH_key(mc->map, mc->index);
}

static inline Value *h_cursor_value(MapCursor *mc)
{
    return pawH_value(mc->map, mc->index);
}

static void h_cursor_next(MapCursor *mc)
{
    mc->index = (mc->index + 1) & (mc->map->capacity - 1);
}

static inline MapCursor h_cursor_init(Map *m, Value key)
{
    return (MapCursor){m, pawV_hash(key) & (m->capacity - 1)};
}

static inline MapCursor h_cursor_lookup(Map *m, Value key)
{
    MapCursor mc = h_cursor_init(m, key);
    while (!h_is_vacant(&mc)) {
        if (h_is_occupied(&mc) && h_cursor_key(&mc)->u == key.u) {
            break;
        }
        h_cursor_next(&mc);
    }
    return mc;
}

Map *pawH_new(paw_Env *P);
void pawH_free(paw_Env *P, Map *m);
paw_Bool pawH_equals(paw_Env *P, Map *lhs, Map *rhs);
void pawH_extend(paw_Env *P, Map *dst, Map *src);
void pawH_clone(paw_Env *P, StackPtr sp, Map *m);
void pawH_key_error(paw_Env *P, Value key, paw_Type type);
Value *pawH_create(paw_Env *P, Map *m, Value key);

static inline size_t pawH_length(const Map *m) { return m->length; }

typedef enum MapAction {
    MAP_ACTION_NONE,
    MAP_ACTION_CREATE,
    MAP_ACTION_REMOVE,
} MapAction;

static inline Value *pawH_action(paw_Env *P, Map *m, Value key,
                                 MapAction action)
{
    if (action == MAP_ACTION_CREATE) {
        return pawH_create(P, m, key);
    } else if (m->length == 0) {
        return NULL;
    }
    MapCursor mc = h_cursor_lookup(m, key);
    if (!h_is_occupied(&mc)) {
        return NULL;
    }
    if (action == MAP_ACTION_REMOVE) {
        h_set_state(&mc, MAP_ITEM_ERASED);
        --m->length;

        // Return the address of the slot to indicate success.
        return h_cursor_key(&mc);
    }
    paw_assert(action == MAP_ACTION_NONE);
    return h_cursor_value(&mc);
}

static inline paw_Bool pawH_contains(paw_Env *P, Map *m, Value key)
{
    return pawH_action(P, m, key, MAP_ACTION_NONE) != NULL;
}

static inline void pawH_insert(paw_Env *P, Map *m, Value key, Value value)
{
    Value *slot = pawH_action(P, m, key, MAP_ACTION_CREATE);
    if (!slot) {
        pawH_key_error(P, key, PAW_TSTRING); // TODO: key type
    }
    *slot = value;
}

static inline void pawH_remove(paw_Env *P, Map *m, Value key)
{
    if (!pawH_action(P, m, key, MAP_ACTION_REMOVE)) {
        pawH_key_error(P, key, PAW_TSTRING); // TODO: key type
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
        const MapMeta *mm = pawH_meta(m, *itr);
        if (mm->state == MAP_ITEM_OCCUPIED) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

#endif // PAW_MAP_H
