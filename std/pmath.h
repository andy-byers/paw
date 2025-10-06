// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STD_MATH_H
#define PAW_STD_MATH_H

#include "paw.h"

extern paw_Float const paw_math_PI;
extern paw_Float const paw_math_NAN;
extern paw_Float const paw_math_INFINITY;

paw_Float paw_math_sin(void *, paw_Float);
paw_Float paw_math_cos(void *, paw_Float);
paw_Float paw_math_tan(void *, paw_Float);
paw_Float paw_math_asin(void *, paw_Float);
paw_Float paw_math_acos(void *, paw_Float);
paw_Float paw_math_atan(void *, paw_Float);
paw_Float paw_math_atan2(void *, paw_Float, paw_Float);

paw_Bool paw_math_is_nan(void *, paw_Float);
paw_Bool paw_math_is_finite(void *, paw_Float);
paw_Bool paw_math_is_negative(void *, paw_Float);

#endif // PAW_STD_MATH_H
