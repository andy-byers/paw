// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "gc.h"
#include "array.h"
#include "bigint.h"
#include "env.h"
#include "map.h"
#include "mem.h"
#include "util.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef PAW_GC_LIMIT
#define PAW_GC_LIMIT (1024 * 1024)
#endif

static void gc_trace_object(const char *msg, void *ptr)
{
    if (ptr == (void *)0x104406dc0) {
    }
#ifdef PAW_TRACE_GC
    fprintf(stdout, "(gc) %s: %p\n", msg, ptr);
#else
    paw_unused(msg);
    paw_unused(ptr);
#endif
}

enum {
    GC_WHITE,
    GC_GRAY,
    GC_BLACK,
};

#define set_mark(x, m) ((x)->gc_mark = (m))
#define set_white(x) set_mark(x, GC_WHITE)
#define set_gray(x) set_mark(x, GC_GRAY)
#define set_black(x) set_mark(x, GC_BLACK)
#define is_white(x) ((x)->gc_mark == GC_WHITE)
#define is_gray(x) ((x)->gc_mark == GC_GRAY)
#define is_black(x) ((x)->gc_mark == GC_BLACK)

static Object **get_gc_list(Object *o)
{
    switch ((ValueKind)o->gc_kind) {
        case ~VFOREIGN:
            return &((Foreign *)o)->gc_list;
        case ~VARRAY:
            return &((Array *)o)->gc_list;
        case ~VMAP:
            return &((Map *)o)->gc_list;
        case ~VCLOSURE:
            return &((Closure *)o)->gc_list;
        case ~VNATIVE:
            return &((Native *)o)->gc_list;
        case ~VPROTO:
            return &((Proto *)o)->gc_list;
        case ~VCLASS:
            return &((Class *)o)->gc_list;
        case ~VINSTANCE:
            return &((Instance *)o)->gc_list;
        case ~VMETHOD:
            return &((Method *)o)->gc_list;
        default:
            paw_assert(0);
            return NULL;
    }
}

static void link_gray_(Object *o, Object **pnext, Object **list)
{
    if (!is_gray(o)) {
        set_gray(o);
        *pnext = *list;
        *list = o;
    }
}

#define LINK_GRAY(o, L) link_gray_(o, get_gc_list(o), &(L))

static void free_object(paw_Env *P, Object *o)
{
    gc_trace_object("free", o);
    switch ((ValueKind)o->gc_kind) {
        case ~VUPVALUE:
            pawV_free_upvalue(P, (UpValue *)o);
            break;
        case ~VCLOSURE:
            pawV_free_closure(P, (Closure *)o);
            break;
        case ~VNATIVE:
            pawV_free_native(P, (Native *)o);
            break;
        case ~VFOREIGN:
            pawV_free_foreign(P, (Foreign *)o);
            break;
        case ~VBIGINT:
            pawB_free(P, (BigInt *)o);
            break;
        case ~VSTRING:
            pawS_free_str(P, (String *)o);
            break;
        case ~VARRAY:
            pawA_free(P, (Array *)o);
            break;
        case ~VMAP:
            pawH_free(P, (Map *)o);
            break;
        case ~VPROTO:
            pawV_free_proto(P, (Proto *)o);
            break;
        case ~VCLASS:
            pawV_free_class(P, (Class *)o);
            break;
        case ~VINSTANCE:
            pawV_free_instance(P, (Instance *)o);
            break;
        case ~VMETHOD:
            pawV_free_method(P, (Method *)o);
            break;
        default:
            paw_assert(PAW_FALSE);
    }
}

static void mark_value(paw_Env *P, Value v);

static void mark_object(paw_Env *P, Object *o)
{
    if (!o || !is_white(o)) {
        return;
    }
    gc_trace_object("mark", o);
    switch ((ValueKind)o->gc_kind) {
        case ~VUPVALUE: {
            UpValue *u = (UpValue *)o;
            mark_value(P, *u->p.p);
            set_black(u);
            break;
        }
        case ~VBIGINT:
        case ~VSTRING:
            set_black(o);
            break;
        case ~VMETHOD:
        case ~VCLOSURE:
        case ~VNATIVE:
        case ~VCLASS:
        case ~VINSTANCE:
        case ~VPROTO:
        case ~VARRAY:
        case ~VMAP:
        case ~VFOREIGN:
            // Put in the gray list to be traversed later.
            LINK_GRAY(o, P->gc_gray);
            break;

        default:
            paw_assert(PAW_FALSE); // TODO: cases above can be the default
    }
}

static void mark_value(paw_Env *P, Value v)
{
    if (pawV_is_object(v)) {
        mark_object(P, pawV_get_object(v));
    }
}

static void traverse_proto(paw_Env *P, Proto *p)
{
    mark_object(P, cast_object(p->name));
    mark_object(P, cast_object(p->modname));
    for (int i = 0; i < p->nproto; ++i) {
        mark_object(P, cast_object(p->p[i]));
    }
    for (int i = 0; i < p->ndebug; ++i) {
        mark_object(P, cast_object(p->v[i].var.name));
    }
    for (int i = 0; i < p->nup; ++i) {
        mark_object(P, cast_object(p->u[i].var.name));
    }
    for (int i = 0; i < p->nk; ++i) {
        mark_value(P, p->k[i]);
    }
}

static void traverse_closure(paw_Env *P, Closure *c)
{
    mark_object(P, cast_object(c->p));
    for (int i = 0; i < c->nup; ++i) {
        mark_object(P, cast_object(c->up[i]));
    }
}

static void traverse_native(paw_Env *P, Native *c)
{
    for (int i = 0; i < c->nup; ++i) {
        mark_object(P, pawV_get_object(c->up[i]));
    }
}

static void traverse_class(paw_Env *P, Class *c)
{
    mark_object(P, cast_object(c->name));
    mark_object(P, cast_object(c->attr));
}

static void traverse_bindings(paw_Env *P, Value *pv, int n)
{
    for (int i = 0; i < n; ++i, pv += 2) {
        mark_value(P, pv[0]); // name
        mark_value(P, pv[1]); // function
    }
}

static void traverse_instance(paw_Env *P, Instance *i)
{
    mark_object(P, cast_object(i->self));
    mark_object(P, cast_object(i->attr));
    traverse_bindings(P, i->bound, i->nbound);
}

static void traverse_method(paw_Env *P, Method *m)
{
    mark_object(P, pawV_get_object(m->self));
    mark_object(P, pawV_get_object(m->f));
}

static void traverse_array(paw_Env *P, Array *a)
{
    paw_Int itr = PAW_ITER_INIT;
    while (pawA_iter(a, &itr)) {
        mark_value(P, *pawA_get(P, a, itr));
    }
}

static void traverse_map(paw_Env *P, Map *m)
{
    paw_Int itr = PAW_ITER_INIT;
    while (pawH_iter(m, &itr)) {
        mark_value(P, m->keys[itr]);
        mark_value(P, m->values[itr]);
    }
}

static void traverse_foreign(paw_Env *P, Foreign *u)
{
    mark_object(P, cast_object(u->attr));
    traverse_bindings(P, u->bound, u->nbound);
}

static void mark_roots(paw_Env *P)
{
    for (StackPtr p = P->stack.p; p != P->top.p; ++p) {
        mark_value(P, *p);
    }
    for (CallFrame *cf = P->cf; cf; cf = cf->prev) {
        mark_object(P, cast_object(cf->fn));
    }
    for (UpValue *u = P->up_list; u; u = u->open.next) {
        mark_object(P, cast_object(u));
    }
    mark_object(P, cast_object(P->globals));
    mark_object(P, cast_object(P->libs));
}

static void traverse_objects(paw_Env *P)
{
    for (Object **po = &P->gc_gray; *po;) {
        Object *o = *po;

        Object **list = get_gc_list(o);
        *po = *list;
        *list = NULL;
        set_black(o);

        gc_trace_object("traverse", o);
        switch ((ValueKind)o->gc_kind) {
            case ~VCLOSURE:
                traverse_closure(P, (Closure *)o);
                break;
            case ~VPROTO:
                traverse_proto(P, (Proto *)o);
                break;
            case ~VNATIVE:
                traverse_native(P, (Native *)o);
                break;
            case ~VCLASS:
                traverse_class(P, (Class *)o);
                break;
            case ~VINSTANCE:
                traverse_instance(P, (Instance *)o);
                break;
            case ~VMETHOD:
                traverse_method(P, (Method *)o);
                break;
            case ~VARRAY:
                traverse_array(P, (Array *)o);
                break;
            case ~VMAP:
                traverse_map(P, (Map *)o);
                break;
            case ~VFOREIGN:
                traverse_foreign(P, (Foreign *)o);
                break;
            default:
                paw_assert(0);
        }
    }
}

static void mark_phase(paw_Env *P)
{
    mark_roots(P);
    traverse_objects(P);
}

static void sweep_phase(paw_Env *P)
{
    for (Object **p = &P->gc_all; *p;) {
        Object *o = *p;
        if (is_white(o)) {
            *p = o->gc_next;
            free_object(P, o);
        } else {
            p = &o->gc_next;
            paw_assert(is_black(o));
            set_white(o);
        }
    }
}

void pawG_collect(paw_Env *P)
{
    mark_phase(P);
    sweep_phase(P);

    // increase the limit
    P->gc_limit = P->gc_bytes * 2;
}

static void sanity_check(paw_Env *P, Object *o)
{
#ifndef NDEBUG
    for (Object *p = P->gc_all; p; p = p->gc_next) {
        paw_assert(p != o);
    }
#else
    paw_unused(P);
    paw_unused(o);
#endif
}

void pawG_add_object(paw_Env *P, Object *o, ValueKind kind)
{
    sanity_check(P, o);

    gc_trace_object("register", o);
    o->gc_kind = ~kind;
    o->gc_mark = GC_WHITE;
    o->gc_next = P->gc_all;
    P->gc_all = o;
}

void pawG_init(paw_Env *P)
{
    P->gc_limit = PAW_GC_LIMIT;
}

void pawG_uninit(paw_Env *P)
{
    // Dispose of roots.
    P->libs = NULL;
    P->globals = NULL;
    P->up_list = NULL;
    P->top = P->stack;

    // All objects should be collected here.
    pawG_collect(P);

    // Free the call frames.
    for (CallFrame *cf = P->main.next; cf;) {
        CallFrame *next = cf->next;
        pawM_free(P, cf);
        cf = next;
    }
    P->cf = NULL;
    P->ncf = 0;

    // Free the fixed objects.
    for (Object *o = P->gc_fixed; o;) {
        Object *next = o->gc_next;
        free_object(P, o);
        o = next;
    }
    P->gc_fixed = NULL;
}

void pawG_fix_object(paw_Env *P, Object *o)
{
    // Must be the most-recently-created GC object.
    paw_assert(P->gc_all == o);
    paw_assert(is_white(o));

    set_gray(o);
    P->gc_all = o->gc_next;
    o->gc_next = P->gc_fixed;
    P->gc_fixed = o;
}
