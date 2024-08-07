// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CALL_H
#define PAW_CALL_H

#include "env.h"
#include "paw.h"
#include "value.h"

#define STACK_EXTRA 1 /* number of slots reserved for errors */
#define save_offset(P, ptr) ((ptr) - (P)->stack.p)
#define restore_pointer(P, ofs) ((P)->stack.p + (ofs))
#define ensure_stack(P, n)                         \
    ((P)->bound.p - (P)->top.p < (n) + STACK_EXTRA \
         ? pawC_stack_grow(P, n + STACK_EXTRA)     \
         : (void)0)

typedef void (*Call)(paw_Env *P, void *arg);

CallFrame *pawC_precall(paw_Env *P, StackPtr base, Object *callable, int argc);
StackPtr pawC_return(paw_Env *P, int nret);
int pawC_try(paw_Env *P, Call call, void *arg);
_Noreturn void pawC_throw(paw_Env *P, int error);
void pawC_call(paw_Env *P, Object *f, int argc);
void pawC_init(paw_Env *P);
void pawC_uninit(paw_Env *P);

void pawC_stack_grow(paw_Env *P, int count);
void pawC_stack_realloc(paw_Env *P, int n);
void pawC_stack_overflow(paw_Env *P);

static inline int pawC_stklen(paw_Env *P) { return P->top.p - P->stack.p; }

// Increase the stack size
// New slots have unspecified values and must be set before the next
// collection runs.
static inline StackPtr pawC_stkinc(paw_Env *P, int n)
{
    ensure_stack(P, n);
    StackPtr sp = P->top.p;
    P->top.p += n;
    return sp;
}

// Decrease the stack size
static inline void pawC_stkdec(paw_Env *P, int n)
{
    P->top.p -= n;

#if PAW_STRESS > 1
    // trim excess slots, respecting the slots reserved for error messages
    pawC_stack_realloc(P, pawC_stklen(P) + STACK_EXTRA);
#endif
}

static inline Value *pawC_pushv(paw_Env *P, Value v)
{
    StackPtr sp = pawC_stkinc(P, 1);
    *sp = v;
    return sp;
}

static inline Value *pawC_push0(paw_Env *P)
{
    StackPtr sp = pawC_stkinc(P, 1);
    v_set_0(sp);
    return sp;
}

static inline Value *pawC_pushi(paw_Env *P, paw_Int i)
{
    StackPtr sp = pawC_stkinc(P, 1);
    v_set_int(sp, i);
    return sp;
}

static inline Value *pawC_pushf(paw_Env *P, paw_Float f)
{
    StackPtr sp = pawC_stkinc(P, 1);
    v_set_float(sp, f);
    return sp;
}

static inline Value *pawC_pushb(paw_Env *P, paw_Bool b)
{
    StackPtr sp = pawC_stkinc(P, 1);
    v_set_bool(sp, b);
    return sp;
}

static inline Value *pawC_pusho(paw_Env *P, Object *o)
{
    StackPtr sp = pawC_stkinc(P, 1);
    v_set_object(sp, o);
    return sp;
}

Value *pawC_pushns(paw_Env *P, const char *s, size_t n);
Value *pawC_pushs(paw_Env *P, const char *s);

static inline void pawC_pop(paw_Env *P) { pawC_stkdec(P, 1); }

#endif // PAW_CALL_H
