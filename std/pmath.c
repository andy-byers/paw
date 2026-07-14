// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "pmath.h"
#include <math.h>

paw_Float64 const paw_math_PI = M_PI;
paw_Float64 const paw_math_NAN = NAN;
paw_Float64 const paw_math_INFINITY = INFINITY;

// pub fn sin(x: float) -> float
paw_Float64 paw_math_sin(paw_Float64 x)
{
    return sin(x);
}

// pub fn cos(x: float) -> float
paw_Float64 paw_math_cos(paw_Float64 x)
{
    return cos(x);
}

// pub fn tan(x: float) -> float
paw_Float64 paw_math_tan(paw_Float64 x)
{
    return tan(x);
}

// pub fn asin(x: float) -> float
paw_Float64 paw_math_asin(paw_Float64 x)
{
    return asin(x);
}

// pub fn acos(x: float) -> float
paw_Float64 paw_math_acos(paw_Float64 x)
{
    return acos(x);
}

// pub fn atan(x: float) -> float
paw_Float64 paw_math_atan(paw_Float64 x)
{
    return atan(x);
}

// pub fn atan2(x: float, y: float) -> float
paw_Float64 paw_math_atan2(paw_Float64 x, paw_Float64 y)
{
    return atan2(x, y);
}

// pub fn is_nan(x: float) -> bool
paw_Bool paw_math_is_nan(paw_Float64 x)
{
    return isnan(x);
}

// pub fn is_finite(x: float) -> bool
paw_Bool paw_math_is_finite(paw_Float64 x)
{
    return isfinite(x);
}

// pub fn is_negative(x: float) -> bool;
paw_Bool paw_math_is_negative(paw_Float64 x)
{
    return signbit(x);
}
