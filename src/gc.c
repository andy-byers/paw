// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "gc.h"
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
            return &o_foreign(o)->gc_list;
        case VVECTOR:
            return &o_vector(o)->gc_list;
        case VMAP:
            return &o_map(o)->gc_list;
        case VCLOSURE:
            return &o_closure(o)->gc_list;
        case VPROTO:
            return &o_proto(o)->gc_list;
        case VINSTANCE:
            return &o_instance(o)->gc_list;
        case VVARIANT:
            return &o_variant(o)->gc_list;
        case VTUPLE:
            return &o_tuple(o)->gc_list;
        case VNATIVE:
            return &o_native(o)->gc_list;
        default:
            paw_assert(o->gc_kind == VMETHOD);
            return &(o_method(o))->gc_list;
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
    if (!o || !IS_WHITE(o)) return;
    gc_trace_object("mark", o);
    switch (o->gc_kind) {
        case VUPVALUE: {
            UpValue *u = o_upvalue(o);
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
        mark_object(P, v_object(v));
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

static void traverse_tuple(paw_Env *P, Tuple *t)
{
    traverse_fields(P, t->elems, t->nelems);
}

static void traverse_instance(paw_Env *P, Instance *i)
{
    traverse_fields(P, i->attrs, i->nfields);
}

static void traverse_method(paw_Env *P, Method *m)
{
    mark_object(P, v_object(m->self));
    mark_object(P, v_object(m->f));
}

static void traverse_vector(paw_Env *P, Vector *a)
{
    paw_Int itr = PAW_ITER_INIT;
    while (pawV_vec_iter(a, &itr)) {
        mark_value(P, *pawV_vec_get(P, a, itr));
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

static void traverse_variant(paw_Env *P, Variant *v)
{
    traverse_fields(P, v->fields, v->nfields);
}

static void traverse_foreign(paw_Env *P, Foreign *u)
{
    traverse_fields(P, u->attrs, u->nfields);
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
    for (int i = 0; i < P->gv.size; ++i) {
        mark_object(P, CAST_OBJECT(P->gv.data[i].name));
        mark_value(P, P->gv.data[i].value);
    }
    mark_object(P, CAST_OBJECT(P->builtin));
    mark_object(P, CAST_OBJECT(P->libs));
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
                traverse_closure(P, o_closure(o));
                break;
            case VPROTO:
                traverse_proto(P, o_proto(o));
                break;
            case VNATIVE:
                traverse_native(P, o_native(o));
                break;
            case VTUPLE:
                traverse_tuple(P, o_tuple(o));
                break;
            case VINSTANCE:
                traverse_instance(P, o_instance(o));
                break;
            case VMETHOD:
                traverse_method(P, o_method(o));
                break;
            case VVECTOR:
                traverse_vector(P, o_vector(o));
                break;
            case VMAP:
                traverse_map(P, o_map(o));
                break;
            case VFOREIGN:
                traverse_foreign(P, o_foreign(o));
                break;
            default:
                traverse_variant(P, o_variant(o));
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
            *p = o->gc_next;
            pawG_free_object(P, o);
        } else {
            p = &o->gc_next;
            paw_assert(IS_BLACK(o));
            SET_WHITE(o);
        }
    }
}

void pawS_check(paw_Env*P);
void pawG_collect(paw_Env *P)
{
    P->gc_gray = NULL;

    mark_phase(P);
    sweep_phase(P);

    P->gc_limit = P->gc_bytes * 2;
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
    P->libs = NULL;
    P->builtin = NULL;
    P->up_list = NULL;
    P->top = P->stack;
    P->gv.size = 0;

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
            pawV_free_upvalue(P, o_upvalue(o));
            break;
        case VCLOSURE:
            pawV_free_closure(P, o_closure(o));
            break;
        case VFOREIGN:
            pawV_free_foreign(P, o_foreign(o));
            break;
        case VSTRING:
            pawS_free_str(P, o_string(o));
            break;
        case VMAP:
            pawH_free(P, o_map(o));
            break;
        case VVECTOR:
            pawV_vec_free(P, o_vector(o));
            break;
        case VPROTO:
            pawV_free_proto(P, o_proto(o));
            break;
        case VINSTANCE:
            pawV_free_instance(P, o_instance(o));
            break;
        case VMETHOD:
            pawV_free_method(P, o_method(o));
            break;
        case VNATIVE:
            pawV_free_native(P, o_native(o));
            break;
        case VVARIANT:
            pawV_free_variant(P, o_variant(o));
            break;
        default:
            pawV_free_tuple(P, o_tuple(o));
            break;
    }
}

