// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include "api.h"
#include "env.h"
#include "lib.h"
#include "util.h"

#define _USE_MATH_DEFINES
#include <math.h>

static int math_sin(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_float(P, sin(f));
    return 1;
}

static int math_cos(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_float(P, cos(f));
    return 1;
}

static int math_tan(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_float(P, tan(f));
    return 1;
}

static int math_asin(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_float(P, asin(f));
    return 1;
}

static int math_acos(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_float(P, acos(f));
    return 1;
}

static int math_atan(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_float(P, atan(f));
    return 1;
}

static int math_atan2(paw_Env *P)
{
    paw_Float const a = paw_float(P, 1);
    paw_Float const b = paw_float(P, 2);
    paw_push_float(P, atan2(a, b));
    return 1;
}

static int math_is_nan(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_bool(P, isnan(f));
    return 1;
}

static int math_is_finite(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_bool(P, isfinite(f));
    return 1;
}

static int math_is_negative(paw_Env *P)
{
    paw_Float const f = paw_float(P, 1);
    paw_push_bool(P, signbit(f) != 0);
    return 1;
}

void l_import_math(paw_Env *P)
{
    static char const s_math[] =
        "#[extern] pub const PI: float;"
        "#[extern] pub const NAN: float;"
        "#[extern] pub const INFINITY: float;"
        "#[extern] pub fn sin(f: float) -> float;\n"
        "#[extern] pub fn cos(f: float) -> float;\n"
        "#[extern] pub fn tan(f: float) -> float;\n"
        "#[extern] pub fn asin(f: float) -> float;\n"
        "#[extern] pub fn acos(f: float) -> float;\n"
        "#[extern] pub fn atan(f: float) -> float;\n"
        "#[extern] pub fn atan2(a: float, b: float) -> float;\n"
        "#[extern] pub fn is_nan(f: float) -> bool;\n"
        "#[extern] pub fn is_finite(f: float) -> bool;\n"
        "#[extern] pub fn is_negative(f: float) -> bool;\n";

    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    paw_push_float(P, M_PI);
    pawL_add_extern_value(P, "math", "PI");
    paw_push_float(P, NAN);
    pawL_add_extern_value(P, "math", "NAN");
    paw_push_float(P, INFINITY);
    pawL_add_extern_value(P, "math", "INFINITY");

    pawL_add_extern_func(P, "math", "sin", math_sin);
    pawL_add_extern_func(P, "math", "cos", math_cos);
    pawL_add_extern_func(P, "math", "tan", math_tan);
    pawL_add_extern_func(P, "math", "asin", math_asin);
    pawL_add_extern_func(P, "math", "acos", math_acos);
    pawL_add_extern_func(P, "math", "atan", math_atan);
    pawL_add_extern_func(P, "math", "atan2", math_atan2);
    pawL_add_extern_func(P, "math", "is_nan", math_is_nan);
    pawL_add_extern_func(P, "math", "is_finite", math_is_finite);
    pawL_add_extern_func(P, "math", "is_negative", math_is_negative);
    paw_pop(P, 1); // paw.builtin

    pawL_chunk_reader(P, s_math, PAW_LENGTHOF(s_math));
}
