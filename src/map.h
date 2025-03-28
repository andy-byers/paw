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
    int key_size;
    int value_size;
} MapPolicy;

#define MAP_POLICY(Map_) (V_INT((Map_)->elems[0]))
#define MAP_DATA(Map_) ((Map_)->elems[1].p)
#define MAP_LENGTH(Map_) (V_INT((Map_)->elems[2]))
#define MAP_CAPACITY(Map_) (V_INT((Map_)->elems[3]))

Tuple *pawMap_new(paw_Env *P, int policy, paw_Int capacity, Value *out);
void pawMap_free(paw_Env *P, Tuple *m);
void pawMap_extend(paw_Env *P, Tuple *a, Tuple const *b);
void pawMap_reserve(paw_Env *P, Tuple *m, paw_Int n);
Value *pawMap_create(paw_Env *P, Tuple *m, Value const *pkey);
Value *pawMap_get(paw_Env *P, Tuple *m, Value const *pkey);
void pawMap_erase(Tuple *m, paw_Int index);
void pawMap_remove(paw_Env *P, Tuple *m, Value const *pkey);
Value *pawMap_insert(paw_Env *P, Tuple *m, Value const *pkey, Value const *pvalue);
paw_Bool pawMap_contains(paw_Env *P, Tuple *m, Value const *pkey);
Value *pawMap_key(paw_Env *P, Tuple *m, paw_Int index);
Value *pawMap_value(paw_Env *P, Tuple *m, paw_Int index);
int pawMap_key_size(paw_Env *P, Tuple const *t);
int pawMap_value_size(paw_Env *P, Tuple const *t);
paw_Bool pawMap_iter(Tuple *m, paw_Int *pi);

inline static paw_Int pawMap_length(Tuple const *m)
{
    return MAP_LENGTH(m);
}

inline static unsigned char *pawMap_state(Tuple *m, paw_Int index)
{
    return &CAST(unsigned char *, MAP_DATA(m))[index];
}

#endif // PAW_MAP_H
