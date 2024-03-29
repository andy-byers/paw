// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "lib.h"
#include "util.h"

#define _USE_MATH_DEFINES
#include <math.h>

static void isqrt(paw_Env *P)
{
    paw_assert(0); // TODO
    (void)P;
}

static int math_sqrt(paw_Env *P)
{
    pawL_check_argc(P, 1);
    if (paw_is_float(P, 1)) {
        const paw_Float f = paw_float(P, 1);
        paw_push_float(P, sqrt(f));
    } else if (paw_is_integer(P, 1)) {
        isqrt(P);
    }
    return 1;
}

static const pawL_Attr kMathLib[] = {
    {"sqrt", math_sqrt},
    {0},
};

void pawL_require_mathlib(paw_Env *P)
{
    pawL_register_lib(P, MATHLIB_NAME, 0, kMathLib);

    // math constants
    paw_push_float(P, M_PI);
    paw_set_attr(P, -2, "pi");
    paw_push_float(P, nan(""));
    paw_set_attr(P, -2, "nan");
    paw_push_float(P, INFINITY);
    paw_set_attr(P, -2, "inf");
}
