// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "pmath.h"
#include <math.h>

paw_Float const paw_math_PI = M_PI;
paw_Float const paw_math_NAN = NAN;
paw_Float const paw_math_INFINITY = INFINITY;

// pub fn sin(x: float) -> float
paw_Float paw_math_sin(void *env, paw_Float x)
{
    return sin(x);
}

// pub fn cos(x: float) -> float
paw_Float paw_math_cos(void *env, paw_Float x)
{
    return cos(x);
}

// pub fn tan(x: float) -> float
paw_Float paw_math_tan(void *env, paw_Float x)
{
    return tan(x);
}

// pub fn asin(x: float) -> float
paw_Float paw_math_asin(void *env, paw_Float x)
{
    return asin(x);
}

// pub fn acos(x: float) -> float
paw_Float paw_math_acos(void *env, paw_Float x)
{
    return acos(x);
}

// pub fn atan(x: float) -> float
paw_Float paw_math_atan(void *env, paw_Float x)
{
    return atan(x);
}

// pub fn atan2(x: float, y: float) -> float
paw_Float paw_math_atan2(void *env, paw_Float x, paw_Float y)
{
    return atan2(x, y);
}

// pub fn is_nan(x: float) -> bool
paw_Bool paw_math_is_nan(void *env, paw_Float x)
{
    return isnan(x);
}

// pub fn is_finite(x: float) -> bool
paw_Bool paw_math_is_finite(void *env, paw_Float x)
{
    return isfinite(x);
}

// pub fn is_negative(x: float) -> bool;
paw_Bool paw_math_is_negative(void *env, paw_Float x)
{
    return signbit(x);
}
