// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"
#include <setjmp.h>
#include <stdlib.h>

#include "call.h"
#include "alloc.h"
#include "env.h"
#include "gc.h"
#include "map.h"
#include "mem.h"
#include "paw.h"
#include "rt.h"
#include "str.h"
#include "util.h"

// Lua-style error handling
#define THROW(P, c) longjmp((c)->jmp, 1)
#define TRY(P, c, a) if (!setjmp((c)->jmp)) {a}

#define CFRAME_SIZE 512

struct Jump {
    struct Jump *prev;
    jmp_buf jmp;
    volatile int status;
};

static void start_resize(paw_Env *P)
{
    P->top.d = SAVE_OFFSET(P, P->top.p);
    P->bound.d = SAVE_OFFSET(P, P->bound.p);
    for (CallFrame *cf = P->cf; cf; cf = cf->prev) {
        cf->base.d = SAVE_OFFSET(P, cf->base.p);
        cf->top.d = SAVE_OFFSET(P, cf->top.p);
    }
    for (UpValue *up = P->up_list; up; up = up->open.next) {
        up->p.d = SAVE_OFFSET(P, upv_level(up));
    }
}

static void finish_resize(paw_Env *P)
{
    P->top.p = RESTORE_POINTER(P, P->top.d);
    P->bound.p = RESTORE_POINTER(P, P->bound.d);
    for (CallFrame *cf = P->cf; cf; cf = cf->prev) {
        cf->base.p = RESTORE_POINTER(P, cf->base.d);
        cf->top.p = RESTORE_POINTER(P, cf->top.d);
    }
    for (UpValue *up = P->up_list; up; up = up->open.next) {
        up->p.p = RESTORE_POINTER(P, up->p.d);
    }
}

void pawC_stack_realloc(paw_Env *P, int n)
{
    _Static_assert(PAW_STACK_MAX >= CFRAME_SIZE, "stack limit is too small");
    _Static_assert(PAW_STACK_MAX <= INT_MAX, "stack limit is too large");
    paw_assert(n >= pawC_stklen(P)); // don't lose live values

    const size_t alloc = PAW_MAX(CAST_SIZE(n), CFRAME_SIZE);
    if (alloc > PAW_STACK_MAX) pawM_error(P);
    
    // Turn off emergency GC and convert pointers into the stack into offsets
    // from P->stack.p.
    P->gc_noem = PAW_TRUE;
    start_resize(P);
    // Reallocate the stack. Call one of the low-level allocation functions that
    // doesn't throw an error. Stack references must be corrected, even if the
    // allocation fails.
    StackPtr stack = pawM_alloc(P, P->stack.p, 
                                sizeof(P->stack.p[0]) * CAST_SIZE(P->bound.d),
                                sizeof(P->stack.p[0]) * alloc);
    P->gc_noem = PAW_FALSE; // allow emergency GC
    if (stack == NULL) {
        finish_resize(P); // fix pointers
        pawM_error(P); // out of memory
    }
    // Cause the 'bound' pointer to be placed at the new end of the stack.
    P->bound.d = CAST(ptrdiff_t, alloc);
    P->stack.p = stack;
    finish_resize(P);
}

void pawC_stack_overflow(paw_Env *P)
{
     pawR_error(P, PAW_ERUNTIME, "stack overflow");
}

// When testing with PAW_STRESS > 1, allocate the exact amount of
// stack slots requested, so that each call to pawC_stack_grow causes
// the stack to be reallocated. This option also causes pawC_stkdec
// to trim the stack each time it is called.
#if PAW_STRESS > 1
# define NEXT_ALLOC(n0, dn) ((n0) + (dn))
#else
# define NEXT_ALLOC(n0, dn) PAW_MAX((n0) + (dn), (n0) * 2)
#endif

void pawC_stack_grow(paw_Env *P, int n)
{
    paw_assert(n > 0);
    paw_assert(P->bound.p >= P->stack.p);
    const int alloc = CAST(int, P->bound.p - P->stack.p);
    pawC_stack_realloc(P, NEXT_ALLOC(alloc, n));
}

Value *pawC_pushns(paw_Env *P, const char *s, size_t n)
{
    Value *pv = pawC_push0(P);
    String *str = pawS_new_nstr(P, s, n);
    V_SET_OBJECT(pv, str);
    return pv;
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
    CallFrame *callee = NULL;
    if (cf->next) {
        callee = cf->next;
    } else {
        callee = pawE_extend_cf(P, top);
    }
    P->cf = callee;
    return callee;
}

static void call_return(paw_Env *P, StackPtr base, paw_Bool has_return)
{
    if (has_return) {
        Value ret = P->top.p[-1];
        P->top.p = base + 1;
        P->top.p[-1] = ret;
    } else {
        // implicit 'return ()'
        P->top.p = base + 1;
        V_SET_0(P->top.p);
    }
    P->cf = P->cf->prev;
}

static void handle_ccall(paw_Env *P, StackPtr base, Native *ccall)
{
    const ptrdiff_t offset = SAVE_OFFSET(P, base);
    ENSURE_STACK(P, CFRAME_SIZE + STACK_EXTRA);
    base = RESTORE_POINTER(P, offset);

    // The C function may cause the stack to be reallocated. Save the relative
    // position of 'base' so it can be restored after the call.
    CallFrame *cf = next_call_frame(P, P->top.p);
    cf->base.p = base;
    cf->top.p = base + CFRAME_SIZE + STACK_EXTRA;
    cf->flags = CFF_C;
    cf->pc = NULL;
    cf->fn = NULL;

    // call the C function
    const int nret = ccall->func(P);
    call_return(P, base, nret);
    pawR_close_upvalues(P, base);
}

static void check_fixed_args(paw_Env *P, Proto *f, int argc)
{
    if (argc < f->argc) {
        pawR_error(P, PAW_ERUNTIME, "not enough arguments (expected %s%d)",
                   f->is_va ? "at least " : "", f->argc);
    } else if (!f->is_va && argc > f->argc) {
        pawR_error(P, PAW_ERUNTIME, "too many arguments (expected %d)",
                   f->argc);
    }
}

CallFrame *pawC_precall(paw_Env *P, StackPtr base, Object *callable, int argc)
{
    Native *ccall;
    Closure *fn = NULL;
    switch (O_KIND(callable)) {
        case VNATIVE:
            ccall = O_NATIVE(callable);
            goto call_native;
        case VCLOSURE:
            fn = O_CLOSURE(callable);
            break;
        default:
            pawR_error(P, PAW_ETYPE, "type is not callable");
    }
    Proto *p = fn->p;
    const ptrdiff_t offset = SAVE_OFFSET(P, base);
    const size_t frame_size = p->max_stack + STACK_EXTRA;
    ENSURE_STACK(P, frame_size);
    base = RESTORE_POINTER(P, offset);

    CallFrame *cf = next_call_frame(P, P->top.p);

    cf->flags = 0;
    cf->pc = p->source;
    cf->base.p = base;
    cf->top.p = base + frame_size;
    cf->fn = fn;

    P->modname = p->modname;
    check_fixed_args(P, p, argc);
    return cf;

call_native:
    handle_ccall(P, base, ccall);
    return NULL;
}

void pawC_call(paw_Env *P, Object *f, int argc)
{
    StackPtr base = P->top.p - argc - 1; // context
    CallFrame *cf = pawC_precall(P, base, f, argc);
    if (cf) {
        cf->flags |= CFF_ENTRY;
        pawR_execute(P, cf);
    }
}

int pawC_raw_try(paw_Env *P, Call call, void *arg)
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

int pawC_try(paw_Env *P, Call call, void *arg)
{
    CallFrame *cf = P->cf;
    const ptrdiff_t top = SAVE_OFFSET(P, P->top.p);
    const int status = pawC_raw_try(P, call, arg);
    if (status != PAW_OK) {
        paw_assert(top <= SAVE_OFFSET(P, P->top.p));
        StackPtr ptr = RESTORE_POINTER(P, top);
        pawR_close_upvalues(P, ptr);
        if (cf == &P->main) {
            // relocate the error message
            call_return(P, ptr, PAW_TRUE);
        } else {
            P->top.p = ptr;
        }
        P->cf = cf;
    }
    return status;
}

_Noreturn void pawC_throw(paw_Env *P, int error)
{
    if (P->jmp == NULL) abort();
    P->jmp->status = error;
    THROW(P, P->jmp);
}

void pawC_init(paw_Env *P)
{
    const int frame_size = CFRAME_SIZE + STACK_EXTRA;
    // allocate manually since normal error handling code requires the stack (this is the
    // stack allocation itself)
    Value *ptr = pawZ_alloc(P, NULL, frame_size * sizeof(Value));
    if (ptr == NULL) pawC_throw(P, PAW_EMEMORY);
    P->bound.p = ptr + frame_size;
    P->stack.p = ptr;
    P->top.p = ptr;

    // Set up the main call frame.
    P->main = (CallFrame){
        .base.p = P->stack.p,
        .top.p = P->bound.p,
    };
    P->cf = &P->main;
}

void pawC_uninit(paw_Env *P)
{
    pawM_free_vec(P, P->stack.p, P->bound.p - P->stack.p);
    P->bound.p = P->top.p = P->stack.p; // clear GC root
}

StackPtr pawC_return(paw_Env *P, int nret)
{
    StackPtr base = CF_STACK_RETURN(P->cf);
    StackPtr top = P->top.p;
    for (int i = 0; i < nret; ++i) {
        base[i] = *--top;
    }
    return base;
}
