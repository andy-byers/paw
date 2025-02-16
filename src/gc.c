// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "gc.h"
#include "alloc.h"
#include "env.h"
#include "list.h"
#include "map.h"
#include "mem.h"
#include "type.h"

#ifdef PAW_TRACE_GC
# include <stdio.h>
#endif

#ifndef PAW_GC_LIMIT
# define PAW_GC_LIMIT (1024 * 1024)
#endif

static void gc_trace_object(const char *msg, void *ptr)
{
#ifdef PAW_TRACE_GC
    fprintf(stdout, "(gc) %s: %p\n", msg, ptr);
#else
    PAW_UNUSED(msg);
    PAW_UNUSED(ptr);
#endif
}

enum {
    GC_WHITE,
    GC_GRAY,
    GC_BLACK,
};

#define SET_MARK(x, m) ((x)->gc_mark = (m))
#define SET_WHITE(x) SET_MARK(x, GC_WHITE)
#define SET_GRAY(x) SET_MARK(x, GC_GRAY)
#define SET_BLACK(x) SET_MARK(x, GC_BLACK)
#define IS_WHITE(x) ((x)->gc_mark == GC_WHITE)
#define IS_GRAY(x) ((x)->gc_mark == GC_GRAY)
#define IS_BLACK(x) ((x)->gc_mark == GC_BLACK)

static Object **get_gc_list(Object *o)
{
    switch (o->gc_kind) {
        case VFOREIGN:
            return &O_FOREIGN(o)->gc_list;
        case VMAP:
            return &O_MAP(o)->gc_list;
        case VCLOSURE:
            return &O_CLOSURE(o)->gc_list;
        case VPROTO:
            return &O_PROTO(o)->gc_list;
        case VTUPLE:
            return &O_TUPLE(o)->gc_list;
        default:
            paw_assert(o->gc_kind == VNATIVE);
            return &O_NATIVE(o)->gc_list;
    }
}

static void link_gray_(Object *o, Object **pnext, Object **list)
{
    if (!IS_GRAY(o)) {
        SET_GRAY(o);
        *pnext = *list;
        *list = o;
    }
}

#define LINK_GRAY(o, L) link_gray_(o, get_gc_list(o), &(L))

static void mark_value(paw_Env *P, Value v);

static void mark_object(paw_Env *P, Object *o)
{
    if (o == NULL) return;
    if (!IS_WHITE(o)) return;
    gc_trace_object("mark", o);
    switch (o->gc_kind) {
        case VUPVALUE: {
            UpValue *u = O_UPVALUE(o);
            mark_value(P, *u->p.p);
            SET_BLACK(u);
            break;
        }
        case VSTRING:
            SET_BLACK(o);
            break;
        default:
            LINK_GRAY(o, P->gc_gray);
    }
}

static void mark_value(paw_Env *P, Value v)
{
    if (pawZ_is_object(P->H, v.u)) {
        mark_object(P, V_OBJECT(v));
    }
}

static void traverse_proto(paw_Env *P, Proto *p)
{
    mark_object(P, CAST_OBJECT(p->name));
    mark_object(P, CAST_OBJECT(p->modname));
    for (int i = 0; i < p->nproto; ++i) {
        mark_object(P, CAST_OBJECT(p->p[i]));
    }
    for (int i = 0; i < p->nk; ++i) {
        mark_value(P, p->k[i]);
    }
}

static void traverse_closure(paw_Env *P, Closure *f)
{
    mark_object(P, CAST_OBJECT(f->p));
    for (int i = 0; i < f->nup; ++i) {
        mark_object(P, CAST_OBJECT(f->up[i]));
    }
}

static void traverse_native(paw_Env *P, Native *f)
{
    for (int i = 0; i < f->nup; ++i) {
        mark_value(P, f->up[i]);
    }
}

static void traverse_fields(paw_Env *P, Value *pv, int n)
{
    for (int i = 0; i < n; ++i) {
        mark_value(P, pv[i]);
    }
}

static void traverse_list(paw_Env *P, Tuple *a)
{
    // emergency collection while allocating the backing buffer
    if (a->elems[0].p == NULL) return;

    paw_Int itr = PAW_ITER_INIT;
    while (pawList_iter(a, &itr)) {
        mark_value(P, *pawList_get(P, a, itr));
    }
}

static void traverse_tuple(paw_Env *P, Tuple *t)
{
    if (t->gc_flag == GC_LIST_FLAG) {
        traverse_list(P, t);
    } else {
        traverse_fields(P, t->elems, t->nelems);
    }
}

static void traverse_map(paw_Env *P, Map *m)
{
    paw_Int itr = PAW_ITER_INIT;
    while (pawH_iter(m, &itr)) {
        mark_value(P, *pawH_key(m, CAST_SIZE(itr)));
        mark_value(P, *pawH_value(m, CAST_SIZE(itr)));
    }
}

static void traverse_foreign(paw_Env *P, Foreign *u)
{
    traverse_fields(P, u->fields, u->nfields);
}

static void mark_roots(paw_Env *P)
{
    for (StackPtr p = P->stack.p; p != P->top.p; ++p) {
        mark_value(P, *p);
    }
    for (CallFrame *cf = P->cf; cf; cf = cf->prev) {
        mark_object(P, CAST_OBJECT(cf->fn));
    }
    for (UpValue *u = P->up_list; u; u = u->open.next) {
        mark_object(P, CAST_OBJECT(u));
    }
    for (int i = 0; i < P->vals.count; ++i) {
        mark_value(P, P->vals.data[i]);
    }
    for (int i = 0; i < P->defs.count; ++i) {
        struct Def *def = P->defs.data[i];
        mark_object(P, CAST_OBJECT(def->hdr.name));
    }
    mark_value(P, P->registry);
}

static void traverse_objects(paw_Env *P)
{
    for (Object **po = &P->gc_gray; *po;) {
        Object *o = *po;

        Object **plist = get_gc_list(o);
        *po = *plist;
        *plist = NULL;
        SET_BLACK(o);

        gc_trace_object("traverse", o);
        switch (o->gc_kind) {
            case VCLOSURE:
                traverse_closure(P, O_CLOSURE(o));
                break;
            case VPROTO:
                traverse_proto(P, O_PROTO(o));
                break;
            case VNATIVE:
                traverse_native(P, O_NATIVE(o));
                break;
            case VTUPLE:
                traverse_tuple(P, O_TUPLE(o));
                break;
            case VMAP:
                traverse_map(P, O_MAP(o));
                break;
            default:
                traverse_foreign(P, O_FOREIGN(o));
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
        if (IS_WHITE(o)) {
            gc_trace_object("free", o);
            *p = o->gc_next;
            pawG_free_object(P, o);
        } else {
            p = &o->gc_next;
            paw_assert(IS_BLACK(o));
            SET_WHITE(o);
        }
    }
}

void pawG_collect(paw_Env *P)
{
    P->gc_gray = NULL;

    mark_phase(P);
    sweep_phase(P);

#define HEAP_GROWTH_PERCENT 50
    P->gc_limit = PAW_MIN(P->gc_bytes +
        (P->gc_bytes * HEAP_GROWTH_PERCENT / 100),
        P->heap_size);
}

void pawG_add_object(paw_Env *P, Object *o, ValueKind kind)
{
    gc_trace_object("register", o);
    pawZ_set_flag(P->H, CAST_UPTR(o));
    o->gc_kind = kind;
    o->gc_next = P->gc_all;
    P->gc_all = o;
}

void pawG_init(paw_Env *P)
{
    P->gc_limit = PAW_GC_LIMIT;
}

void pawG_uninit(paw_Env *P)
{
    // clear GC roots
    P->registry.u = 0;
    P->up_list = NULL;

    // collect all non-fixed objects
    pawG_collect(P);

    for (CallFrame *cf = P->main.next; cf;) {
        CallFrame *next = cf->next;
        pawM_free(P, cf);
        cf = next;
    }
    P->cf = NULL;
    P->ncf = 0;

    for (Object *o = P->gc_fixed; o;) {
        Object *next = o->gc_next;
        pawG_free_object(P, o);
        o = next;
    }
    P->gc_fixed = NULL;
}

void pawG_fix_object(paw_Env *P, Object *o)
{
    paw_assert(P->gc_all == o);
    paw_assert(IS_WHITE(o));

    SET_GRAY(o);
    P->gc_all = o->gc_next;
    o->gc_next = P->gc_fixed;
    P->gc_fixed = o;
}

void pawG_free_object(paw_Env *P, Object *o)
{
    pawZ_clear_flag(P->H, CAST_UPTR(o));
    switch (o->gc_kind) {
        case VUPVALUE:
            pawV_free_upvalue(P, O_UPVALUE(o));
            break;
        case VCLOSURE:
            pawV_free_closure(P, O_CLOSURE(o));
            break;
        case VFOREIGN:
            pawV_free_foreign(P, O_FOREIGN(o));
            break;
        case VSTRING:
            pawS_free_str(P, O_STRING(o));
            break;
        case VMAP:
            pawH_free(P, O_MAP(o));
            break;
        case VPROTO:
            pawV_free_proto(P, O_PROTO(o));
            break;
        case VNATIVE:
            pawV_free_native(P, O_NATIVE(o));
            break;
        default:
            pawV_free_tuple(P, O_TUPLE(o));
    }
}

