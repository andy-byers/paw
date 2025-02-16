// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_MAP_H
#define PAW_MAP_H

#include "paw.h"
#include "util.h"
#include "value.h"

typedef paw_Bool (*MapEquals)(void *ctx, Value lhs, Value rhs);
typedef paw_Uint (*MapHash)(void *ctx, Value v);

#define MAP_EQUALS(m, a, b) (m)->policy->equals((m)->policy->ctx, a, b)
#define MAP_HASH(m, k) (m)->policy->hash((m)->policy->ctx, k)

typedef struct MapPolicy {
    MapEquals equals;
    MapHash hash;
    void *ctx;
} MapPolicy;

typedef struct MapCursor {
    Map *map;
    size_t index;
} MapCursor;

#define pawH_meta(m, index) (&CAST(MapMeta *, (m)->data)[index])

static inline Value *pawH_key(Map *m, size_t index)
{
    char *data = CAST(char *, m->data);
    data += sizeof(MapMeta) * m->capacity;
    return &CAST(Value *, data)[index];
}

static inline Value *pawH_value(Map *m, size_t index)
{
    return pawH_key(m, index) + m->capacity;
}

static inline MapState h_get_state(MapCursor *mc)
{
    return CAST(MapMeta *, mc->map->data)[mc->index].state;
}

static inline void h_set_state(MapCursor *mc, MapState state)
{
    CAST(MapMeta *, mc->map->data)[mc->index].state = state;
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
    return (MapCursor){m, MAP_HASH(m, key) & (m->capacity - 1)};
}

static inline paw_Bool h_cursor_lookup(Map *m, Value key, MapCursor *pmc)
{
    *pmc = h_cursor_init(m, key);
    for (size_t i = 0;
            i < m->capacity && h_get_state(pmc) != MAP_ITEM_VACANT;
            ++i, h_cursor_next(pmc)) {
        if (h_get_state(pmc) == MAP_ITEM_OCCUPIED
                && MAP_EQUALS(m, *h_cursor_key(pmc), key)) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

Map *pawH_new(paw_Env *P);
Map *pawH_new_with_policy(paw_Env *P, const MapPolicy *policy);
void pawH_free(paw_Env *P, Map *m);
void pawH_extend(paw_Env *P, Map *dst, Map *src);
void pawH_reserve(paw_Env *P, Map *dst, size_t n);
Value *pawH_create(paw_Env *P, Map *m, Value key);

static inline size_t pawH_length(const Map *m)
{
    return m->length;
}

static inline Value *pawH_get(Map *m, Value key)
{
    MapCursor mc;
    if (m->length == 0) return NULL;
    if (h_cursor_lookup(m, key, &mc)) {
        return h_cursor_value(&mc);
    }
    return NULL;
}

static inline void pawH_erase_at(Map *m, size_t index)
{
    if (m->length == 0) return;
    MapCursor mc = {m, index};
    paw_assert(h_get_state(&mc) == MAP_ITEM_OCCUPIED);
    h_set_state(&mc, MAP_ITEM_ERASED);
    --m->length;
}

static inline void pawH_erase(Map *m, Value key)
{
    MapCursor mc;
    if (m->length == 0) return;
    if (h_cursor_lookup(m, key, &mc)) {
        h_set_state(&mc, MAP_ITEM_ERASED);
        --m->length;
    }
}

static inline Value *pawH_insert(paw_Env *P, Map *m, Value key, Value value)
{
    Value *pvalue = pawH_create(P, m, key);
    *pvalue = value;
    return pvalue;
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
