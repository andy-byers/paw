// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "env.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "os.h"
#include "rt.h"
#include <stdio.h>
#include <errno.h>

#define CF_BASE(i) (P->cf->base.p + i)

static void push_values(paw_Env *P, Value const *pvalue, int count)
{
    API_INCR_TOP(P, count);
    pawV_copy(P->top.p - count, pvalue, count);
}

static void push_option_some(paw_Env *P, Value const *pvalue, int count)
{
    paw_push_int(P, PAW_OPTION_SOME);
    push_values(P, pvalue, count);
}

static void push_option_none(paw_Env *P, int count)
{
    paw_push_int(P, PAW_OPTION_NONE);
    paw_push_zero(P, count);
}

static int map_get(paw_Env *P)
{
    Tuple *map = V_TUPLE(*CF_BASE(1));
    Value const *pkey = CF_BASE(2);
    int const value_size = pawMap_value_size(P, map);
    Value const *pvalue = pawMap_get(P, map, pkey);

    P->top.p = CF_BASE(0);
    if (pvalue != NULL) {
        push_option_some(P, pvalue, value_size);
    } else {
        push_option_none(P, value_size);
    }
    P->top.p = CF_BASE(value_size);
    return value_size;
}

static int map_set(paw_Env *P)
{
    Tuple *map = V_TUPLE(*CF_BASE(1));
    Value const *pkey = CF_BASE(2);
    Value const *pvalue = CF_BASE(3);

    int const value_size = pawMap_value_size(P, map);
    Value const *preplaced = pawMap_insert(P, map, pkey, pvalue);
    if (preplaced != NULL) {
        push_option_some(P, pvalue, value_size);
    } else {
        push_option_none(P, value_size);
    }
    return 1 + value_size;
}

static int map_iterator_next(paw_Env *P)
{
    paw_get_field(P, 1, 0); // 2: self.map
    paw_get_field(P, 1, 1); // 3: self.index

    int const key_size = paw_map_key_size(P, 2);
    if (paw_map_next_key(P, 2)) {
        // modify "self.index"
        paw_push_value(P, 3);
        paw_set_field(P, 1, 1);
        // construct "Option::Some(<key>)"
        paw_push_int(P, PAW_OPTION_SOME);
        paw_rotate(P, -key_size - 1, 1);
    } else {
        push_option_none(P, key_size);
    }
    return 1 + key_size;
}

static int map_length(paw_Env *P)
{
    pawR_map_length(P, P->cf, CF_BASE(1), CF_BASE(1));
    return 1;
}

static int map_get_or(paw_Env *P)
{
    int const value_size = paw_map_value_size(P, 1);

    Tuple *m = V_TUPLE(*CF_BASE(1));
    Value const *key = CF_BASE(2);
    Value const *value = pawMap_get(P, m, key);
    if (value != NULL)
        push_values(P, value, value_size);

    return value_size;
}

static int map_erase(paw_Env *P)
{
    Tuple *m = V_TUPLE(*CF_BASE(1));
    pawMap_remove(P, m, CF_BASE(2));
    paw_pop(P, 1); // return 'self'
    return 1;
}

void l_import_map(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    pawL_add_extern_method(P, "map", "Map", "length", map_length);
    pawL_add_extern_method(P, "map", "Map", "get", map_get);
    pawL_add_extern_method(P, "map", "Map", "set", map_set);
    pawL_add_extern_method(P, "map", "Map", "get_or", map_get_or);
    pawL_add_extern_method(P, "map", "Map", "erase", map_erase);
    pawL_add_extern_method(P, "map", "MapIterator", "next", map_iterator_next);
    paw_pop(P, 1); // paw.symbols

    pawL_file_reader(P, PAWL_STDLIB_PATH(PAWL_MAP_NAME));
}


