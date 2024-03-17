// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CALL_H
#define PAW_CALL_H

#include "env.h"
#include "paw.h"
#include "value.h"

#define S2V(s) (*(s))

typedef void (*Call)(paw_Env *P, void *arg);

CallFrame *pawC_precall(paw_Env *P, StackPtr base, Value callable, int argc);
int pawC_try(paw_Env *P, Call call, void *arg);
void pawC_throw(paw_Env *P, int error);
void pawC_call(paw_Env *P, Value f, int argc);

StackPtr pawC_return(paw_Env *P, int nret);

// Stack setup/cleanup:
void pawC_init(paw_Env *P);
void pawC_uninit(paw_Env *P);

// Stack access/manipulation routines:
void pawC_stkgrow(paw_Env *P, int count);
void pawC_stkerror(paw_Env *P, const char *what);

static inline int pawC_stklen(paw_Env *P)
{
    return P->top - P->stack;
}

static inline StackPtr pawC_stkinc(paw_Env *P, int n)
{
    if (P->bound - P->top < n) {
        pawC_stkgrow(P, n);
    }
    StackPtr sp = P->top;
    for (int i = 0; i < n; ++i) {
        pawV_set_null(P->top++);
    }
    return sp;
}

static inline void pawC_stkdec(paw_Env *P, int n)
{
    P->top -= n;
}

static inline void pawC_stkpush(paw_Env *P, Value v)
{
    *pawC_stkinc(P, 1) = v;
}

static inline void pawC_stkpop(paw_Env *P)
{
    pawC_stkdec(P, 1);
}

static inline ptrdiff_t pawC_stksave(paw_Env *P, StackPtr sp)
{
    return sp - P->stack;
}

static inline StackPtr pawC_stkload(paw_Env *P, ptrdiff_t diff)
{
    return P->stack + diff;
}

static inline void pawC_stkcheck(paw_Env *P, int n)
{
    if (P->bound - P->top < n) {
        pawC_stkerror(P, "stack overflow");
    }
}

#endif // PAW_CALL_H
