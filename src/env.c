// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "env.h"
#include "mem.h"
#include "rt.h"
#include <limits.h>

CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top)
{
    if (P->ncf == INT_MAX) {
        pawR_error(P, PAW_EMEMORY, "too many nested function calls");
    }
    CallFrame *cf = pawM_new(P, CallFrame);
    P->cf->next = cf;
    cf->prev = P->cf;
    cf->next = NULL;
    cf->top = top;
    ++P->ncf;
    return cf;
}
