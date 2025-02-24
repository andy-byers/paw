// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_MAP_H
#define PAW_MAP_H

#include "paw.h"
#include "util.h"
#include "value.h"

typedef struct MapPolicy {
    Value equals;
    Value hash;
    paw_Bool fp;
} MapPolicy;

#define MAP_POLICY(Map_) (V_INT((Map_)->elems[0]))
#define MAP_DATA(Map_) ((Map_)->elems[1].p)
#define MAP_LENGTH(Map_) (V_INT((Map_)->elems[2]))
#define MAP_CAPACITY(Map_) (V_INT((Map_)->elems[3]))

Tuple *pawMap_new(paw_Env *P, int policy, paw_Int capacity, Value *out);
void pawMap_free(paw_Env *P, Tuple *m);
void pawMap_extend(paw_Env *P, Tuple *a, const Tuple *b);
void pawMap_reserve(paw_Env *P, Tuple *m, paw_Int n);
Value *pawMap_create(paw_Env *P, Tuple *m, Value key);
Value *pawMap_get(paw_Env *P, Tuple *m, Value key);
void pawMap_erase(Tuple *m, paw_Int index);
void pawMap_remove(paw_Env *P, Tuple *m, Value key);
Value *pawMap_insert(paw_Env *P, Tuple *m, Value key, Value value);
paw_Bool pawMap_contains(paw_Env *P, Tuple *m, Value key);
paw_Bool pawMap_iter(Tuple *m, paw_Int *pi);

static inline paw_Int pawMap_length(const Tuple *m)
{
    return MAP_LENGTH(m);
}

static inline unsigned char *pawMap_state(Tuple *m, paw_Int index)
{
    return &CAST(unsigned char *, MAP_DATA(m))[index];
}

static inline Value *pawMap_key(Tuple *m, paw_Int index)
{
    unsigned char *data = CAST(unsigned char *, MAP_DATA(m));
    return &CAST(Value *, data + MAP_CAPACITY(m))[index];
}

static inline Value *pawMap_value(Tuple *m, paw_Int index)
{
    Value *data = pawMap_key(m, 0);
    return &data[MAP_CAPACITY(m) + index];
}

#endif // PAW_MAP_H
