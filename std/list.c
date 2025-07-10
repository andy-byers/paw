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

static int list_length(paw_Env *P)
{
    paw_list_length(P, 1);
    return 1;
}

static int list_push(paw_Env *P)
{
    paw_list_push(P, 1);
    return 0;
}

static int list_pop(paw_Env *P)
{
    int const z = paw_list_iget(P, 1, -1);
    paw_list_iremove(P, 1, -1);
    return z;
}

static int list_insert(paw_Env *P)
{
    paw_list_insert(P, 1);
    return 0;
}

static int list_remove(paw_Env *P)
{
    paw_list_remove(P, 1);
    return 0;
}

static int list_get(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int index = V_INT(*CF_BASE(2));
    paw_Int const length = pawList_length(P, list);
    int const element_size = LIST_ZELEMENT(list);

    index = pawV_abs_index(index, length);
    if (0 <= index && index < length) {
        Value const *pvalue = pawList_get(P, list, index);
        push_option_some(P, pvalue, element_size);
    } else {
        push_option_none(P, element_size);
    }
    return 1 + element_size;
}

static int list_set(paw_Env *P)
{
    paw_list_set(P, 1);
    return 0;
}

void l_import_list(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    pawL_add_extern_method(P, "list", "List", "length", list_length);
    pawL_add_extern_method(P, "list", "List", "get", list_get);
    pawL_add_extern_method(P, "list", "List", "set", list_set);
    pawL_add_extern_method(P, "list", "List", "push", list_push);
    pawL_add_extern_method(P, "list", "List", "insert", list_insert);
    pawL_add_extern_method(P, "list", "List", "remove", list_remove);
    pawL_add_extern_method(P, "list", "List", "pop", list_pop);
    paw_pop(P, 1); // paw.symbols

    pawL_file_reader(P, PAWL_STDLIB_PATH(PAWL_LIST_NAME));
}

