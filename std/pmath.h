// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STD_MATH_H
#define PAW_STD_MATH_H

#include "paw.h"

extern paw_Float64 const paw_math_PI;
extern paw_Float64 const paw_math_NAN;
extern paw_Float64 const paw_math_INFINITY;

paw_Float64 paw_math_sin(paw_Float64);
paw_Float64 paw_math_cos(paw_Float64);
paw_Float64 paw_math_tan(paw_Float64);
paw_Float64 paw_math_asin(paw_Float64);
paw_Float64 paw_math_acos(paw_Float64);
paw_Float64 paw_math_atan(paw_Float64);
paw_Float64 paw_math_atan2(paw_Float64, paw_Float64);

paw_Bool paw_math_is_nan(paw_Float64);
paw_Bool paw_math_is_finite(paw_Float64);
paw_Bool paw_math_is_negative(paw_Float64);

#endif // PAW_STD_MATH_H
