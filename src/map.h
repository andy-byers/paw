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

#define pawH_meta(m, index) (&CAST((m)->data, MapMeta *)[index])

static inline Value *pawH_key(Map *m, size_t index)
{
    char *data = CAST(m->data, char *);
    data += sizeof(MapMeta) * m->capacity;
    return &CAST(data, Value *)[index];
}

static inline Value *pawH_value(Map *m, size_t index)
{
    return pawH_key(m, index) + m->capacity;
}

static inline MapState h_get_state(MapCursor *mc)
{
    return CAST(mc->map->data, MapMeta *)[mc->index].state;
}

static inline void h_set_state(MapCursor *mc, MapState state)
{
    CAST(mc->map->data, MapMeta *)[mc->index].state = state;
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
    while (h_get_state(&mc) != MAP_ITEM_VACANT) {
        if (h_get_state(&mc) == MAP_ITEM_OCCUPIED && 
                h_cursor_key(&mc)->u == key.u) {
            break;
        }
        h_cursor_next(&mc);
    }
    return mc;
}

Map *pawH_new(paw_Env *P);
void pawH_free(paw_Env *P, Map *m);
void pawH_extend(paw_Env *P, Map *dst, Map *src);
Value *pawH_create(paw_Env *P, Map *m, Value key);

static inline size_t pawH_length(const Map *m) 
{
    return m->length; 
}

static inline Value *pawH_get(Map *m, Value key)
{
    if (m->length == 0) return NULL;
    MapCursor mc = h_cursor_lookup(m, key);
    if (h_get_state(&mc) == MAP_ITEM_OCCUPIED) {
        return h_cursor_value(&mc);
    }
    return NULL;
}

static inline void pawH_erase(Map *m, Value key)
{
    if (m->length == 0) return;
    MapCursor mc = h_cursor_lookup(m, key);
    if (h_get_state(&mc) == MAP_ITEM_OCCUPIED) {
        h_set_state(&mc, MAP_ITEM_ERASED);
        --m->length;
    }
}

static inline void pawH_insert(paw_Env *P, Map *m, Value key, Value value)
{
    *pawH_create(P, m, key) = value;
}

static inline paw_Bool pawH_contains(Map *m, Value key)
{
    return pawH_get(m, key) != NULL;
}

static inline paw_Bool pawH_iter(const Map *m, paw_Int *pi)
{
    for (++*pi; *pi < PAW_CAST_INT(m->capacity); ++*pi) {
        const MapMeta *mm = pawH_meta(m, *pi);
        if (mm->state == MAP_ITEM_OCCUPIED) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

#endif // PAW_MAP_H
