// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "call.h"
#include "env.h"
#include "gc.h"
#include "map.h"
#include "mem.h"
#include "paw.h"
#include "rt.h"
#include "str.h"
#include "util.h"
#include <assert.h>
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

// Lua-style error handling
#define c_throw(P, c) longjmp((c)->jmp, 1)
#define c_try(P, c, a)       \
    if (!setjmp((c)->jmp)) { \
        a                    \
    }

struct Jump {
    struct Jump *prev;
    jmp_buf jmp;
    volatile int status;
};

// Given a pointer 'sp' into 'P->stack', move 'sp' to point to the same relative offset
// in 'stack'
static StackPtr moveptr(paw_Env *P, StackPtr sp, StackPtr stack)
{
    const ptrdiff_t rel = sp - P->stack;
    return stack + rel;
}

#define STACK_MIN 8

void pawC_stkerror(paw_Env *P, const char *what)
{
    pawR_error(P, PAW_ERUNTIME, what);
}

void pawC_stkgrow(paw_Env *P, int n)
{
    _Static_assert(PAW_STACK_MAX >= STACK_MIN, "stack limit is too small");
    _Static_assert(PAW_STACK_MAX <= INT_MAX, "stack limit is too large");

    const size_t nold = cast_size(P->bound - P->stack);
    const size_t nnew = paw_max(nold + cast_size(n), nold * 2);
    if (nnew > PAW_STACK_MAX) {
        pawM_error(P);
    }
    StackPtr oldstack = P->stack;

    // Allocate a new stack and correct all references into the old stack.
    StackPtr newstack = pawM_new_vec(P, nnew, Value);
    memcpy(newstack, P->stack, nold * sizeof(Value));
    for (CallFrame *cf = P->cf; cf; cf = cf->prev) {
        cf->base = moveptr(P, cf->base, newstack);
        cf->top = moveptr(P, cf->top, newstack);
    }
    for (UpValue *u = P->up_list; u; u = u->open.next) {
        u->p = moveptr(P, u->p, newstack);
    }
    P->top = moveptr(P, P->top, newstack);
    P->bound = newstack + nnew;
    P->stack = newstack;

    pawM_free_vec(P, oldstack, nold);
}

Value *pawC_pushns(paw_Env *P, const char *s, size_t n)
{
    StackPtr sp = pawC_stkinc(P, 1);
    String *str = pawS_new_nstr(P, s, n);
    pawV_set_string(sp, str);
    return sp;
}

Value *pawC_pushs(paw_Env *P, const char *s)
{
    return pawC_pushns(P, s, strlen(s));
}

static CallFrame *next_call_frame(paw_Env *P, StackPtr top)
{
    // Env always has a call frame. It may be pointing to P->main.
    paw_assert(P->cf);
    CallFrame *cf = P->cf;
    CallFrame *callee;
    if (cf->next) {
        callee = cf->next;
    } else {
        callee = pawE_extend_cf(P, top);
    }
    P->cf = callee;
    return callee;
}

static void ccall_return(paw_Env *P, StackPtr base, paw_Bool has_return)
{
    if (has_return) {
        Value ret = P->top[-1];
        P->top = base + 1;
        P->top[-1] = ret;
    } else {
        // implicit 'return null'
        P->top = base + 1;
        pawV_set_null(&P->top[-1]);
    }
    P->cf = P->cf->prev;
}

static void handle_ccall(paw_Env *P, StackPtr base, Native *ccall)
{
    // The C function may cause the stack to be reallocated. Save the relative
    // position of 'base' so it can be restored after the call.
    const ptrdiff_t pos = pawC_stksave(P, base);
    CallFrame *cf = next_call_frame(P, P->top);
    cf->flags = CFF_C;
    cf->pc = NULL;
    cf->fn = NULL;
    cf->base = base;
    cf->top = base;

    // call the C function
    const int nret = ccall->f(P); // TODO: Multi-return
    base = pawC_stkload(P, pos);
    ccall_return(P, base, nret);
}

static void check_fixed_args(paw_Env *P, Proto *f, int argc)
{
    if (argc < f->argc) {
        pawR_error(P, PAW_ERUNTIME, "not enough arguments (expected %s%d)",
                   f->is_va ? "at least " : "", f->argc);
    } else if (!f->is_va && argc > f->argc) {
        pawR_error(P, PAW_ERUNTIME, "too many arguments (expected %d)", f->argc);
    }
}

CallFrame *pawC_precall(paw_Env *P, StackPtr base, Value callable, int argc)
{
    Native *ccall;
    Closure *fn = NULL;
    switch (pawV_get_type(callable)) {
        case VNATIVE:
            ccall = pawV_get_native(callable);
            goto call_native;
        case VCLOSURE:
            fn = pawV_get_closure(callable);
            break;
        case VMETHOD: {
            Method *mtd = pawV_get_method(callable);
            *base = mtd->self; // replace with self
            if (pawV_is_native(mtd->f)) {
                ccall = pawV_get_native(mtd->f);
                goto call_native;
            } else {
                assert(pawV_is_closure(mtd->f));
                fn = pawV_get_closure(mtd->f);
            }
            break;
        }
        case VCLASS: {
            Class *cls = pawV_get_class(callable);
            pawV_new_instance(P, base, cls);
            Value name = pawE_cstr(P, CSTR_INIT); // '__init'
            Value *init = pawH_get(P, cls->attr, name);
            if (!init) {
                // There is no user-defined initializer, so just return
                // the instance.
                P->top = base + 1;
                return P->cf;
            }
            fn = pawV_get_closure(*init);
            break;
        }
        default:
            pawR_error(P, PAW_ETYPE, "type is not callable");
    }
    CallFrame *cf = next_call_frame(P, P->top);
    Proto *p = fn->p;

    cf->flags = 0;
    cf->pc = p->source;
    cf->base = base;
    cf->top = base;
    cf->fn = fn;

    check_fixed_args(P, p, argc);
    return cf;

call_native:
    handle_ccall(P, base, ccall);
    return NULL;
}

void pawC_call(paw_Env *P, Value f, int argc)
{
    StackPtr base = P->top - argc - 1; // context
    CallFrame *cf = pawC_precall(P, base, f, argc);
    if (cf) {
        cf->flags |= CFF_ENTRY;
        pawR_execute(P, cf);
    }
}

static int exceptional_call(paw_Env *P, Call call, void *arg)
{
    struct Jump jmp = {
        .status = PAW_OK,
        .prev = P->jmp,
    };
    P->jmp = &jmp;
    c_try(P, &jmp,
          call(P, arg););
    P->jmp = jmp.prev;
    return jmp.status;
}

int pawC_try(paw_Env *P, Call call, void *arg)
{
    CallFrame *save_cf = P->cf;
    const int status = exceptional_call(P, call, arg);
    if (status != PAW_OK) {
        // Jump back to the saved call frame.
        P->cf = save_cf;
    }
    return status;
}

void pawC_throw(paw_Env *P, int error)
{
    if (P->jmp) {
        P->jmp->status = error;
        c_throw(P, P->jmp);
    }
    abort();
}

void pawC_init(paw_Env *P)
{
    P->stack = pawM_new_vec(P, STACK_MIN, Value);
    P->bound = P->stack + STACK_MIN;
    P->top = P->stack;

    // Set up the main call frame.
    P->main.base = P->stack;
    P->main.top = P->main.base;
    P->main.fn = NULL;
    P->cf = &P->main;
}

void pawC_uninit(paw_Env *P)
{
    pawM_free_vec(P, P->stack, P->bound - P->stack);
}

StackPtr pawC_return(paw_Env *P, int nret)
{
    StackPtr base = CF_STACK_RETURN(P->cf);
    StackPtr top = P->top;
    for (int i = 0; i < nret; ++i) {
        base[i] = *--top;
    }
    return base;
}
