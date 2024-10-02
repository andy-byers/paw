// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include <math.h>
#include "api.h"
#include "env.h"
#include "lib.h"
#include "util.h"

static int math_sin(paw_Env *P)
{
    const paw_Float f = paw_float(P, 1);
    paw_push_float(P, sin(f));
    return 1;
}

static int math_cos(paw_Env *P)
{
    const paw_Float f = paw_float(P, 1);
    paw_push_float(P, cos(f));
    return 1;
}

static int math_tan(paw_Env *P)
{
    const paw_Float f = paw_float(P, 1);
    paw_push_float(P, tan(f));
    return 1;
}

static int math_asin(paw_Env *P)
{
    const paw_Float f = paw_float(P, 1);
    paw_push_float(P, asin(f));
    return 1;
}

static int math_acos(paw_Env *P)
{
    const paw_Float f = paw_float(P, 1);
    paw_push_float(P, acos(f));
    return 1;
}

static int math_atan(paw_Env *P)
{
    const paw_Float f = paw_float(P, 1);
    paw_push_float(P, atan(f));
    return 1;
}

static int math_atan2(paw_Env *P)
{
    const paw_Float a = paw_float(P, 1);
    const paw_Float b = paw_float(P, 2);
    paw_push_float(P, atan2(a, b));
    return 1;
}


void l_import_math(paw_Env *P)
{
    static const char s_math[] =
        "pub fn sin(f: float) -> float;\n"
        "pub fn cos(f: float) -> float;\n"
        "pub fn tan(f: float) -> float;\n"
        "pub fn asin(f: float) -> float;\n"
        "pub fn acos(f: float) -> float;\n"
        "pub fn atan(f: float) -> float;\n"
        "pub fn atan2(a: float, b: float) -> float;\n";

    pawE_push_cstr(P, CSTR_KBUILTIN);
    paw_map_getelem(P, PAW_REGISTRY_INDEX);

    pawL_add_extern_func(P, "math", "sin", math_sin);
    pawL_add_extern_func(P, "math", "cos", math_cos);
    pawL_add_extern_func(P, "math", "tan", math_tan);
    pawL_add_extern_func(P, "math", "asin", math_asin);
    pawL_add_extern_func(P, "math", "acos", math_acos);
    pawL_add_extern_func(P, "math", "atan", math_atan);
    pawL_add_extern_func(P, "math", "atan2", math_atan2);
    paw_pop(P, 1); // paw.builtin

    pawL_chunk_reader(P, s_math, PAW_LENGTHOF(s_math));
}

