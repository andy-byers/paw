// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "gc_aux.h"
#include "env.h"
#include "map.h"
#include "mem.h"
#include "util.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO: Using Boehm-Demers-Weiser GC for now
#include <gc.h>

void pawG_init(paw_Env *P) { paw_unused(P); }

void pawG_uninit(paw_Env *P) { paw_unused(P); }

void pawG_collect(paw_Env *P)
{
    paw_unused(P);
    P->gc_bytes = 0;
}

void pawG_fix_object(paw_Env *P, Object *o)
{
    paw_unused(P);
    paw_unused(o);
}

void pawG_add_object(paw_Env *P, Object *o, ValueKind kind)
{
    paw_unused(P);
    o->gc_kind = kind;
}

void pawG_free_object(paw_Env *P, Object *o)
{
    paw_unused(P);
    paw_unused(o);
}

#if 0
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

#define set_mark(x, m) ((x)->gc_mark = (m))
#define set_white(x) set_mark(x, GC_WHITE)
#define set_gray(x) set_mark(x, GC_GRAY)
#define set_black(x) set_mark(x, GC_BLACK)
#define is_white(x) ((x)->gc_mark == GC_WHITE)
#define is_gray(x) ((x)->gc_mark == GC_GRAY)
#define is_black(x) ((x)->gc_mark == GC_BLACK)

static Object **get_gc_list(Object *o)
{
//    switch (o->gc_kind) {
//        case VFOREIGN:
//            return &o_foreign(o)->gc_list;
//        case VARRAY:
//            return &o_array(o)->gc_list;
//        case VMAP:
//            return &o_map(o)->gc_list;
//        case VCLOSURE:
//            return &o_closure(o)->gc_list;
//        case VPROTO:
//            return &o_proto(o)->gc_list;
//        case VCLASS:
//            return &o_class(o)->gc_list;
//        case VINSTANCE:
//            return &o_instance(o)->gc_list;
//        case VMETHOD:
//            return &(o_method(o))->gc_list;
//        default:
//            paw_assert(0);
//            return NULL;
//    }
}

static void link_gray_(Object *o, Object **pnext, Object **list)
{
  //  if (!is_gray(o)) {
  //      set_gray(o);
  //      *pnext = *list;
  //      *list = o;
  //  }
}

#define LINK_GRAY(o, L) link_gray_(o, get_gc_list(o), &(L))

static void mark_value(paw_Env *P, Value v);

static void mark_object(paw_Env *P, Object *o)
{
  //  if (!o || !is_white(o)) {
  //      return;
  //  }
  //  gc_trace_object("mark", o);
  //  switch (o->gc_kind) {
  //      case VUPVALUE: {
  //          UpValue *u = o_upvalue(o);
  //          mark_value(P, *u->p.p);
  //          set_black(u);
  //          break;
  //      }
////        case VBIGINT:
  //      case VSTRING:
  //          set_black(o);
  //          break;
  //      case VMETHOD:
  //      case VCLOSURE:
  //      case VCLASS:
  //      case VINSTANCE:
  //      case VPROTO:
  //      case VARRAY:
  //      case VMAP:
  //      case VFOREIGN:
  //          // Put in the gray list to be traversed later.
  //          LINK_GRAY(o, P->gc_gray);
  //          break;

  //      default:
  //          paw_assert(PAW_FALSE); // TODO: cases above can be the default
  //  }
}

static void mark_value(paw_Env *P, Value v)
{
//    if (pawV_is_object(v)) {
//        mark_object(P, v_object(v));
//    }
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

// TODO: Need the exact class type for this: get it from the instruction
//       that modifies the refcount
static void traverse_attrs(paw_Env *P, Value *pv, int n)
{
    for (int i = 0; i < n; ++i) {
        mark_value(P, pv[i]);
    }
}

//static void traverse_class(paw_Env *P, Class *c)
//{
////    traverse_attrs(P, &c->attrs, )
//}

static void traverse_instance(paw_Env *P, Instance *i)
{
//    traverse_attrs(P, i->attrs, i->nattrs);
}

static void traverse_method(paw_Env *P, Method *m)
{
    mark_object(P, v_object(m->self));
    mark_object(P, v_object(m->f));
}

//static void traverse_array(paw_Env *P, Array *a)
//{
//    paw_Int itr = PAW_ITER_INIT;
//    while (pawA_iter(a, &itr)) {
//        mark_value(P, *pawA_get(P, a, itr));
//    }
//}
//
//static void traverse_map(paw_Env *P, Map *m)
//{
//    paw_Int itr = PAW_ITER_INIT;
//    while (pawH_iter(m, &itr)) {
//        mark_value(P, m->keys[itr]);
//        mark_value(P, m->values[itr]);
//    }
//}

static void traverse_foreign(paw_Env *P, Foreign *u)
{
//    traverse_attrs(P, u->attrs, u->nattrs);
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
    mark_object(P, cast_object(P->libs));
}

static void traverse_objects(paw_Env *P)
{
    for (Object **po = &P->gc_gray; *po;) {
        Object *o = *po;

        Object **list = get_gc_list(o);
        *po = *list;
        *list = NULL;
 //       set_black(o);

        gc_trace_object("traverse", o);
        switch (o->gc_kind) {
            case VCLOSURE:
                traverse_closure(P, o_closure(o));
                break;
            case VPROTO:
                traverse_proto(P, o_proto(o));
                break;
//            case VCLASS:
//                traverse_class(P, o_class(o));
//                break;
            case VINSTANCE:
                traverse_instance(P, o_instance(o));
                break;
            case VMETHOD:
                traverse_method(P, o_method(o));
                break;
//            case VARRAY:
//                traverse_array(P, o_array(o));
//                break;
//            case VMAP:
//                traverse_map(P, o_map(o));
//                break;
            case VFOREIGN:
                traverse_foreign(P, o_foreign(o));
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
 //   for (Object **p = &P->gc_all; *p;) {
 //       Object *o = *p;
 //       if (is_white(o)) {
 //           *p = o->gc_next;
 //           pawG_free_object(P, o);
 //       } else {
 //           p = &o->gc_next;
 //           paw_assert(is_black(o));
 //           set_white(o);
 //       }
 //   }
}

static void free_all_objects(paw_Env *P, Object *o)
{
    for (; o; o = o->gc_next) {
        pawG_free_object(P, o);
    }
}

static void clean_dead_objects(paw_Env *P)
{
//    for (Object **p = &P->gc_all; *p;) {
//        Object *o = *p;
//        if (g_hasref(o)) {
//            p = &o->gc_next;
//        } else {
//            *p = o->gc_next;
//            pawG_free_object(P, o);
//        }
//    }
}

void pawG_collect(paw_Env *P)
{
    clean_dead_objects(P);

    // increase the limit
    P->gc_limit = P->gc_bytes * 2;
}

void pawG_add_object(paw_Env *P, Object *o, ValueKind kind)
{
    gc_trace_object("register", o);
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
    // Free the call frames.
    for (CallFrame *cf = P->main.next; cf;) {
        CallFrame *next = cf->next;
        pawM_free(P, cf);
        cf = next;
    }
    P->cf = NULL;
    P->ncf = 0;

//    // Free the managed objects.
//    free_all_objects(P, P->gc_fixed);
//    free_all_objects(P, P->gc_all);
//    P->gc_fixed = NULL;
//    P->gc_all = NULL;
}

void pawG_fix_object(paw_Env *P, Object *o)
{
    // Must be the most-recently-created GC object.
//    paw_assert(P->gc_all == o);
//    paw_assert(!g_hasref(o));
//
//    g_incref(o);
//    P->gc_all = o->gc_next;
//    o->gc_next = P->gc_fixed;
//    P->gc_fixed = o;
}

void pawG_free_object(paw_Env *P, Object *o)
{
    switch (o->gc_kind) {
        case VUPVALUE:
            pawV_free_upvalue(P, o_upvalue(o));
            break;
        case VCLOSURE:
            pawV_free_closure(P, o_closure(o));
            break;
//        case VFOREIGN:
//            pawV_free_foreign(P, o_foreign(o));
//            break;
        case VSTRING:
            pawS_free_str(P, o_string(o));
            break;
//        case VARRAY:
//            pawA_free(P, o_array(o));
//            break;
        case VPROTO:
            pawV_free_proto(P, o_proto(o));
            break;
//        case VCLASS:
//            pawV_free_class(P, o_class(o));
//            break;
//        case VINSTANCE:
//            pawV_free_instance(P, o_instance(o));
//            break;
        case VMETHOD:
            pawV_free_method(P, o_method(o));
            break;
        default:
            paw_assert(PAW_FALSE);
    }
}
#endif // 0
