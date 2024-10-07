// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CALL_H
#define PAW_CALL_H

#include "env.h"
#include "paw.h"
#include "value.h"

#define STACK_EXTRA 25 /* slots for error handling */
#define SAVE_OFFSET(P, ptr) ((ptr) - (P)->stack.p)
#define RESTORE_POINTER(P, ofs) ((P)->stack.p + (ofs))
#define ENSURE_STACK(P, n) \
    ((P)->bound.p - (P)->top.p < (n) + STACK_EXTRA \
         ? pawC_stack_grow(P, n + STACK_EXTRA) \
         : (void)0)

typedef void (*Call)(paw_Env *P, void *arg);

CallFrame *pawC_precall(paw_Env *P, StackPtr base, Object *callable, int argc);
int pawC_try(paw_Env *P, Call call, void *arg);
int pawC_raw_try(paw_Env *P, Call call, void *arg);
_Noreturn void pawC_throw(paw_Env *P, int error);
void pawC_call(paw_Env *P, Object *f, int argc);
void pawC_init(paw_Env *P);
void pawC_uninit(paw_Env *P);

void pawC_stack_grow(paw_Env *P, int count);
void pawC_stack_realloc(paw_Env *P, int n);
void pawC_stack_overflow(paw_Env *P);

static inline int pawC_stklen(paw_Env *P)
{
    return CAST(int, P->top.p - P->stack.p);
}

// Increase the stack size
// New slots have unspecified values and must be set before the next
// collection runs.
static inline StackPtr pawC_stkinc(paw_Env *P, int n)
{
    paw_assert(n <= CAST(int, P->bound.p - P->top.p));
    return (P->top.p += n) - n;
}

// Decrease the stack size
static inline void pawC_stkdec(paw_Env *P, int n)
{
    paw_assert(n <= pawC_stklen(P));
    P->top.p -= n;
}

static inline Value *pawC_push0(paw_Env *P)
{
    StackPtr sp = pawC_stkinc(P, 1);
    V_SET_0(sp);
    return sp;
}

Value *pawC_pushns(paw_Env *P, const char *s, size_t n);
Value *pawC_pushs(paw_Env *P, const char *s);

static inline void pawC_pop(paw_Env *P)
{
    pawC_stkdec(P, 1);
}

#endif // PAW_CALL_H
