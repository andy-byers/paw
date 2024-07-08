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
#define c_try(P, c, a)                                                         \
    if (!setjmp((c)->jmp)) {                                                   \
        a                                                                      \
    }

struct Jump {
    struct Jump *prev;
    jmp_buf jmp;
    volatile int status;
};

static void start_resize(paw_Env *P)
{
    P->top.d = save_offset(P, P->top.p);
    P->bound.d = save_offset(P, P->bound.p);
    for (CallFrame *cf = P->cf; cf; cf = cf->prev) {
        cf->base.d = save_offset(P, cf->base.p);
        cf->top.d = save_offset(P, cf->top.p);
    }
    for (UpValue *up = P->up_list; up; up = up->open.next) {
        up->p.d = save_offset(P, upv_level(up));
    }
}

static void finish_resize(paw_Env *P)
{
    P->top.p = restore_pointer(P, P->top.d);
    P->bound.p = restore_pointer(P, P->bound.d);
    for (CallFrame *cf = P->cf; cf; cf = cf->prev) {
        cf->base.p = restore_pointer(P, cf->base.d);
        cf->top.p = restore_pointer(P, cf->top.d);
    }
    for (UpValue *up = P->up_list; up; up = up->open.next) {
        up->p.p = restore_pointer(P, up->p.d);
    }
}

#define STACK_MIN 8

void pawC_stack_realloc(paw_Env *P, int n)
{
    _Static_assert(PAW_STACK_MAX >= STACK_MIN, "stack limit is too small");
    _Static_assert(PAW_STACK_MAX <= INT_MAX, "stack limit is too large");
    paw_assert(n >= pawC_stklen(P)); // don't lose live values

    const size_t alloc = paw_max(cast_size(n), STACK_MIN);
    if (alloc > PAW_STACK_MAX) {
        pawM_error(P);
    }
    // Turn off emergency GC and convert pointers into the stack into offsets
    // from P->stack.p.
    P->gc_noem = PAW_TRUE;
    start_resize(P);
    // Reallocate the stack. Call one of the low-level allocation functions that
    // doesn't throw an error. Stack references must be corrected, even if the
    // allocation fails.
    StackPtr stack =
        pawM_alloc(P, P->stack.p, sizeof(P->stack.p[0]) * cast_size(P->bound.d),
                   sizeof(P->stack.p[0]) * alloc);
    P->gc_noem = PAW_FALSE; // allow emergency GC
    if (!stack) {
        finish_resize(P); // fix pointers
        pawM_error(P); // out of memory
    }
    // Cause the 'bound' pointer to be placed at the new end of the stack.
    P->bound.d = (ptrdiff_t)alloc;
    P->stack.p = stack;
    finish_resize(P);
}

void pawC_stack_overflow(paw_Env *P)
{
    // pawR_error(P, PAW_ERUNTIME, "stack overflow");
}

// When testing with PAW_STRESS > 1, allocate the exact amount of
// stack slots requested, so that each call to pawC_stack_grow causes
// the stack to be reallocated. This option also causes pawC_stkdec
// to trim the stack each time it is called.
#if PAW_STRESS > 1
#define next_alloc(n0, dn) ((n0) + (dn))
#else
#define next_alloc(n0, dn) paw_max((n0) + (dn), (n0) * 2)
#endif

void pawC_stack_grow(paw_Env *P, int n)
{
    paw_assert(n > 0);
    paw_assert(P->bound.p >= P->stack.p);
    const int alloc = cast_size(P->bound.p - P->stack.p);
    pawC_stack_realloc(P, next_alloc(alloc, n));
}

Value *pawC_pushns(paw_Env *P, const char *s, size_t n)
{
    Value *pv = pawC_push0(P);
    String *str = pawS_new_nstr(P, s, n);
    v_set_object(pv, str);
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
        P->top.p->u = 0; // clear value
    }
    P->cf = P->cf->prev;
}

static void handle_ccall(paw_Env *P, StackPtr base, Native *ccall)
{
    // The C function may cause the stack to be reallocated. Save the relative
    // position of 'base' so it can be restored after the call.
    const ptrdiff_t pos = save_offset(P, base);
    CallFrame *cf = next_call_frame(P, P->top.p);
    cf->flags = CFF_C;
    cf->pc = NULL;
    cf->fn = NULL;
    cf->base.p = base;
    cf->top.p = base;

    // call the C function
    const int nret = ccall->func(P);
    base = restore_pointer(P, pos);
    call_return(P, base, nret);
    // pawR_close_upvalues(P, base);
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

// Make sure there is at least this number of stack slots available for
// the callee.
#define FRAME_EXTRA 0 /*64*/

CallFrame *pawC_precall(paw_Env *P, StackPtr base, Object *callable, int argc)
{
    const ptrdiff_t offset = save_offset(P, base);
    ensure_stack(P, FRAME_EXTRA);
    base = restore_pointer(P, offset);

    Native *ccall;
    Closure *fn = NULL;
    switch (o_kind(callable)) {
        case VNATIVE:
            ccall = o_native(callable);
            goto call_native;
        case VCLOSURE:
            fn = o_closure(callable);
            break;
            //        case VMETHOD: {
            //            Method *mtd = v_method(callable);
            //            *base = mtd->self; // replace with self
            //            if (pawV_is_native(mtd->f)) {
            //                ccall = v_native(mtd->f);
            //                goto call_native;
            //            } else {
            //                assert(pawV_is_closure(mtd->f));
            //                fn = v_closure(mtd->f);
            //            }
            //            break;
            //        }
            //        case VCLASS: {
            //            Class *cls = v_class(callable);
            //            pawV_new_instance(P, base, cls);
            //            Value name = pawE_cstr(P, CSTR_INIT); // '__init'
            //            Value *init = pawH_get(P, cls->attr, name);
            //            if (!init) {
            //                // There is no user-defined initializer, so just
            //                return
            //                // the instance.
            //                P->top.p = base + 1;
            //                return P->cf;
            //            }
            //            fn = v_closure(*init);
            //            break;
            //        }
        default:
            paw_assert(0);
            //            pawR_error(P, PAW_ETYPE, "type is not callable");
    }
    CallFrame *cf = next_call_frame(P, P->top.p);
    Proto *p = fn->p;

    cf->flags = 0;
    cf->pc = p->source;
    cf->base.p = base;
    cf->top.p = base;
    cf->fn = fn;

    P->modname = p->modname; // TODO: what about C functions called first?
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

static int exceptional_call(paw_Env *P, Call call, void *arg)
{
    struct Jump jmp = {
        .status = PAW_OK,
        .prev = P->jmp,
    };
    P->jmp = &jmp;
    c_try(P, &jmp, call(P, arg););
    P->jmp = jmp.prev;
    return jmp.status;
}

int pawC_try(paw_Env *P, Call call, void *arg)
{
    CallFrame *cf = P->cf;
    const ptrdiff_t top = save_offset(P, P->top.p);
    const int status = exceptional_call(P, call, arg);
    if (status != PAW_OK) {
        paw_assert(top <= save_offset(P, P->top.p));
        StackPtr ptr = restore_pointer(P, top);
        pawR_close_upvalues(P, ptr);
        if (cf == &P->main) {
            // relocate the error message
            call_return(P, ptr, PAW_TRUE);
        } else {
            // TODO: The error message gets ignored so that the error status
            //       can be returned in paw (no multi-return). Once that gets
            //       implemented, the error message can be the second return
            //       value.
            P->top.p = ptr;
        }
        P->cf = cf;
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
    P->stack.p = pawM_new_vec(P, STACK_MIN, Value);
    P->bound.p = P->stack.p + STACK_MIN;
    P->top.p = P->stack.p;

    // Set up the main call frame.
    P->main.base.p = P->stack.p;
    P->main.top.p = P->main.base.p;
    P->main.fn = NULL;
    P->cf = &P->main;
}

void pawC_uninit(paw_Env *P)
{
    pawM_free_vec(P, P->stack.p, P->bound.p - P->stack.p);
}

StackPtr pawC_return(paw_Env *P, int nret)
{
    StackPtr base = cf_stack_return(P->cf);
    StackPtr top = P->top.p;
    for (int i = 0; i < nret; ++i) {
        base[i] = *--top;
    }
    return base;
}
