// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <setjmp.h>
#include <stdlib.h>

#include "call.h"
#include "env.h"
#include "core.h"
#include "util.h"

// Lua-style error handling
#define THROW(P, c) longjmp((c)->jmp, 1)
#define TRY(P, c, a)         \
    if (!setjmp((c)->jmp)) { \
        a                    \
    }

struct Jump {
    struct Jump *prev;
    jmp_buf jmp;
    int volatile status;
};

int pawC_try(paw_Env *P, paw_Function call, void *arg)
{
    struct Jump jmp = {
        .status = PAW_OK,
        .prev = P->jmp,
    };
    P->jmp = &jmp;
    TRY(P, &jmp, call(P, arg);)
    P->jmp = jmp.prev;
    return jmp.status;
}

_Noreturn void pawC_throw(paw_Env *P, int error)
{
    paw_assert(P->jmp != NULL);
    P->jmp->status = error;
    THROW(P, P->jmp);
}

