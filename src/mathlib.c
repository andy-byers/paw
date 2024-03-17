#include "lib.h"
#include <math.h>

static const pawL_Attr kMathLib[] = {
    {0},
};

void pawL_require_mathlib(paw_Env *P)
{
    pawL_register_lib(P, MATHLIB_NAME, 0, kMathLib);

    // Math constants
    paw_push_float(P, M_PI);
    paw_set_attr(P, -2, "pi");
    paw_push_float(P, nan(""));
    paw_set_attr(P, -2, "nan");
    paw_push_float(P, INFINITY);
    paw_set_attr(P, -2, "inf");
}
