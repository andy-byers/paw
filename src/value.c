// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"
#include <stdio.h>
#include <stdlib.h>

#include "env.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "value.h"


static void int_to_str(paw_Env *P, paw_Int i, Value *out)
{
    char temp[32];
    paw_Bool const negative = i < 0;
    char *end = temp + PAW_COUNTOF(temp);
    char *ptr = end - 1;

    // Don't call llabs(INT64_MIN). The result is undefined on 2s complement
    // systems.
    uint64_t u = i == INT64_MIN
                     ? UINT64_C(1) << 63
                     : CAST(uint64_t, llabs(i));
    do {
        *ptr-- = CAST(char, u % 10 + '0');
        u /= 10;
    } while (u);
    if (negative) {
        *ptr = '-';
    } else {
        ++ptr;
    }
    Str *str = pawS_new_nstr(P, ptr, CAST_SIZE(end - ptr));
    V_SET_OBJECT(out, str);
}

static void float_to_str(paw_Env *P, paw_Float f, Value *out)
{
    char temp[32];
    int const n = snprintf(temp, PAW_COUNTOF(temp), "%.*g", 17, f);
    Str *str = pawS_new_nstr(P, temp, CAST_SIZE(n));
    V_SET_OBJECT(out, str);
}

char const *pawV_to_str(paw_Env *P, Value *pv, paw_Type type, size_t *plength)
{
    switch (type) {
        case PAW_TSTR:
            break;
        case PAW_TINT:
            int_to_str(P, V_INT(*pv), pv);
            break;
        case PAW_TFLOAT:
            float_to_str(P, V_FLOAT(*pv), pv);
            break;
        default:
            paw_assert(type == PAW_TBOOL);
            V_SET_OBJECT(pv, CACHED_STRING(P, V_TRUE(*pv) ? CSTR_TRUE : CSTR_FALSE));
    }
    Str const *s = V_STR(*pv);
    if (plength != NULL)
        *plength = s->length;
    return s->text;
}

Tuple *pawV_new_tuple(paw_Env *P, int nelems)
{
    Tuple *tuple = pawM_new_flex(P, Tuple, CAST_SIZE(nelems), sizeof(tuple->elems[0]));
    tuple->objkind = VTUPLE;
    tuple->nelems = nelems;
    return tuple;
}

void pawV_free_tuple(paw_Env *P, Tuple *t)
{
    pawM_free_flex(P, t, t->nelems, sizeof(t->elems[0]));
}

