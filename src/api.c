// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "api.h"
#include "alloc.h"
#include "auxlib.h"
#include "call.h"
#include "gc.h"
#include "lib.h"
#include "map.h"
#include "mem.h"
#include "compile.h"
#include "paw.h"
#include "parse.h"
#include "rt.h"
#include "str.h"
#include "value.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static void *default_alloc(void *ud, void *ptr, size_t old_size, size_t new_size)
{
    paw_unused(ud);
    if (new_size == 0) {
        free(ptr);
        return NULL;
    } 
    if (old_size == 0) return malloc(new_size);
    return realloc(ptr, new_size);
}

static StackPtr at(paw_Env *P, int index)
{
    const int i = paw_abs_index(P, index);
    paw_assert(i < paw_get_count(P));
    return &P->cf->base.p[i];
}

paw_Alloc paw_get_allocator(paw_Env *P) 
{
    return P->alloc; 
}

void paw_set_allocator(paw_Env *P, paw_Alloc alloc, void *ud)
{
    P->alloc = alloc;
    P->ud = ud;
}

size_t paw_bytes_used(const paw_Env *P) { return P->gc_bytes; }

static void open_aux(paw_Env *P, void *arg)
{
    paw_unused(arg);
    pawG_init(P);
    pawC_init(P);
    pawS_init(P);
    pawP_init(P);
    pawR_init(P);
    pawL_init(P);
    CHECK_GC(P);
}

#define OR_DEFAULT(a, b) ((a) ? (a) : (b))

paw_Env *paw_open(const struct paw_Options *o)
{
    size_t heap_size = OR_DEFAULT(o->heap_size, PAW_HEAP_DEFAULT);
    heap_size &= ~(PAW_ALIGN - 1); // round down
    if (heap_size < PAW_HEAP_MIN) return NULL;

    void *ud = OR_DEFAULT(o->ud, NULL);
    paw_Alloc alloc = OR_DEFAULT(o->alloc, default_alloc);
    void *heap = OR_DEFAULT(o->heap, alloc(ud, NULL, 0, heap_size));
    const paw_Bool owns_heap = o->heap == NULL;
    if (heap == NULL) return NULL;
    paw_assert(PAW_IS_ALIGNED(heap));
    const size_t zh = heap_size;
    void *ph = heap;

    paw_Env *P = heap;
    *P = (paw_Env){
        .heap_size = heap_size,
        .alloc = alloc, 
        .ud = ud,
    };
    heap = BUMP_PTR(heap, PAW_ROUND_UP(sizeof(*P)));
    heap_size -= PAW_ROUND_UP(sizeof(*P));

    if (pawZ_init(P, heap, heap_size, owns_heap)) {
        if (owns_heap) alloc(ud, ph, zh, 0);
        return NULL;
    }
    if (pawC_try(P, open_aux, NULL)) {
        paw_close(P);
        return NULL;
    }
    return P;
}

void paw_close(paw_Env *P)
{
    pawE_uninit(P);
    pawC_uninit(P);
    pawG_uninit(P);
    pawS_uninit(P);
    pawZ_uninit(P);
}

int paw_find_global(paw_Env *P)
{
    paw_push_string(P, "_");
    paw_binop(P, PAW_OPADD, PAW_TSTRING);
    const String *name = V_STRING(P->top.p[-1]);
    const int g = pawE_locate(P, name);
    paw_pop(P, 1);
    return g;
}

void paw_push_value(paw_Env *P, int index)
{
    const Value v = *at(P, index);
    pawC_pushv(P, v);
}

void paw_push_unit(paw_Env *P, int n)
{
    for (int i = 0; i < n; ++i) {
        pawC_push0(P);
    }
}

void paw_push_boolean(paw_Env *P, paw_Bool b)
{
    pawC_pushb(P, b);
}

void paw_push_float(paw_Env *P, paw_Float f)
{
    pawC_pushf(P, f);
}

void paw_push_int(paw_Env *P, paw_Int i)
{
    pawC_pushi(P, i);
}

void paw_push_native(paw_Env *P, paw_Function fn, int n)
{
    Value *pv = pawC_push0(P);
    Native *o = pawV_new_native(P, fn, n);
    V_SET_OBJECT(pv, o);

    StackPtr top = P->top.p;
    const StackPtr base = top - n - 1;
    for (int i = 0; i < n; ++i) {
        o->up[i] = base[i];
    }
    // replace upvalues with closure object
    base[0] = top[-1];
    P->top.p = base + 1;
}

const char *paw_push_string(paw_Env *P, const char *s)
{
    return paw_push_nstring(P, s, strlen(s));
}

const char *paw_push_nstring(paw_Env *P, const char *s, size_t n)
{
    const Value *pv = pawC_pushns(P, s, n);
    return V_TEXT(*pv);
}

const char *paw_push_vfstring(paw_Env *P, const char *fmt, va_list arg)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_vfstring(P, &buf, fmt, arg);
    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

const char *paw_push_fstring(paw_Env *P, const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    const char *s = paw_push_vfstring(P, fmt, arg);
    va_end(arg);
    return s;
}

paw_Bool paw_bool(paw_Env *P, int index)
{
    return V_TRUE(*at(P, index));
}

paw_Int paw_int(paw_Env *P, int index)
{
    return V_INT(*at(P, index));
}

paw_Float paw_float(paw_Env *P, int index)
{
    return V_FLOAT(*at(P, index));
}

const char *paw_string(paw_Env *P, int index)
{
    const String *s = V_STRING(*at(P, index));
    return s->text;
}

paw_Function paw_native(paw_Env *P, int index)
{
    const Native *f = V_NATIVE(*at(P, index));
    return f->func;
}

void *paw_pointer(paw_Env *P, int index)
{
    const Foreign *f = V_FOREIGN(*at(P, index));
    return f->data;
}

// void paw_to_float(paw_Env *P, int index)
//{
//     const int i = paw_abs_index(P, index);
//     paw_push_value(P, i);
//     pawR_to_float(P);
//     paw_replace(P, i);
// }
//
// void paw_to_int(paw_Env *P, int index)
//{
//     const int i = paw_abs_index(P, index);
//     paw_push_value(P, i);
//     pawR_to_integer(P);
//     paw_replace(P, i);
// }
//
// void paw_to_string(paw_Env *P, int index)
//{
//     size_t len;
//     const int i = paw_abs_index(P, index);
//     paw_push_value(P, i);
//     pawR_to_string(P, &len);
//     paw_replace(P, i);
// }

size_t paw_length(paw_Env *P, int index)
{
    size_t result;
    paw_push_value(P, index);
    const Value v = P->top.p[-1];
    switch (v.o->gc_kind) {
        case VSTRING:
            result = pawS_length(V_STRING(v));
            break;
        case VLIST:
            result = pawV_list_length(V_LIST(v));
            break;
        case VMAP:
            result = pawH_length(V_MAP(v));
            break;
        default:
            result = 0;
    }
    paw_pop(P, 1);
    return result;
}

void paw_pop(paw_Env *P, int n)
{
    paw_assert(n <= pawC_stklen(P));
    pawC_stkdec(P, n);
}

struct CompileState {
    paw_Reader input;
    struct DynamicMem dm;
    const char *name;
    void *ud;
};

static void compile_aux(paw_Env *P, void *arg)
{
    struct Compiler C;
    struct CompileState *p = arg;
    pawP_startup(P, &C, &p->dm, p->name);
    pawP_compile(&C, p->input, p->ud);
}

int paw_load(paw_Env *P, paw_Reader input, const char *name, void *ud)
{
    struct CompileState p = {
        .input = input,
        .name = name,
        .ud = ud,
    };
    const int status = pawC_try(P, compile_aux, &p);
    pawP_teardown(P, &p.dm);
    return status;
}

struct CallState {
    StackPtr fn;
    int argc;
};

void eval_aux(paw_Env *P, void *arg)
{
    const struct CallState *ctx = arg;
    pawC_call(P, V_OBJECT(*ctx->fn), ctx->argc);
}

int paw_call(paw_Env *P, int argc)
{
    StackPtr top = P->top.p;
    StackPtr fn = &top[-argc - 1];
    struct CallState c = {.fn = fn, .argc = argc};
    const int status = pawC_try(P, eval_aux, &c);
    return status;
}

int paw_get_count(paw_Env *P)
{
    return CAST(P->top.p - P->cf->base.p, int);
}

static int upvalue_index(int nup, int index)
{
    if (index < 0) {
        paw_assert(-index <= nup);
        return nup - index;
    }
    paw_assert(index <= nup);
    return index;
}

void paw_get_upvalue(paw_Env *P, int ifn, int index)
{
    Value *pv = pawC_push0(P);
    Object *o = at(P, ifn)->o;
    switch (o->gc_kind) {
        case VNATIVE: {
            Native *f = O_NATIVE(o);
            *pv = f->up[upvalue_index(f->nup, index)];
            break;
        }
        case VCLOSURE: {
            Closure *f = O_CLOSURE(o);
            UpValue *u = f->up[upvalue_index(f->nup, index)];
            *pv = *u->p.p;
            break;
        }
        default:
            pawR_error(P, PAW_ETYPE, "type of object has no upvalues");
    }
}

void paw_get_global(paw_Env *P, int index)
{
    pawC_pushv(P, *pawE_get_val(P, index));
}

//void paw_get_elemi(paw_Env *P, int index, paw_Int i)
//{
//     const int abs = paw_abs_index(P, index);
//     paw_push_int(P, i);
//     paw_get_elem(P, abs);
//}
//
//void paw_get_item(paw_Env *P, int index)
//{
//     if (!paw_check_item(P, index)) {
//         // If the target container is a sequence, and the integer key is
//         // out of bounds, then the runtime will throw a PAW_EINDEX before
//         // this code is reached. The key must be a string if control has
//         // made it here.
//         pawR_error(P, PAW_ENAME, "key '%s' does not exist", paw_string(P,
//         -1));
//     }
//}
//
//paw_Bool paw_check_item(paw_Env *P, int index)
//{
//     paw_push_value(P, index); // push container
//     paw_rotate(P, -2, 1); // place container below key
//     if (pawR_getelem(P)) {
//         paw_push_null(P);
//         paw_shift(P, 2);
//         return PAW_FALSE;
//     }
//     return PAW_TRUE;
//}
//
//void paw_get_field(paw_Env *P, int index, int ifield)
//{
//     if (!paw_check_attr(P, index, attr)) {
//         pawR_error(P, PAW_EATTR, "attribute '%s' does not exist", attr);
//     }
//}
//
// paw_Bool paw_check_attr(paw_Env *P, int index, const char *attr)
//{
//     paw_push_value(P, index); // push instance
//     paw_push_string(P, attr); // push attribute name
//     if (pawR_getattr_raw(P, PAW_FALSE)) {
//         paw_push_null(P);
//         paw_shift(P, 2);
//         return PAW_FALSE;
//     }
//     return PAW_TRUE;
// }
//
// void paw_set_upvalue(paw_Env *P, int ifn, int index)
//{
//     const Value fn = *at(P, ifn);
//     switch (O_KIND(fn.o)) {
//         case VNATIVE: {
//             Native *f = V_NATIVE(fn);
//             Value *v = &f->up[upvalue_index(f->nup, index)];
//             *v = P->top.p[-1];
//             paw_pop(P, 1);
//             break;
//         }
//         case VCLOSURE: {
//             Closure *f = V_CLOSURE(fn);
//             UpValue *u = f->up[upvalue_index(f->nup, index)];
//             *u->p.p = P->top.p[-1];
//             paw_pop(P, 1);
//             break;
//         }
//         default:
//             break;
//     }
// }

void paw_set_global(paw_Env *P, const char *name)
{
    paw_unused(P);
    paw_unused(name);
    //    paw_push_string(P, name);
    //    paw_rotate(P, -2, 1); // Swap
    //
    //    const Value key = P->top.p[-2];
    //    pawR_write_global(P, key, PAW_TRUE);
    //    paw_pop(P, 1); // Pop 'key'
}

// void paw_set_itemi(paw_Env *P, int index, paw_Int idx)
//{
//     const int abs = paw_abs_index(P, index);
//     paw_push_int(P, idx);
//     paw_set_item(P, abs);
// }
//
// void paw_set_item(paw_Env *P, int index)
//{
//     paw_push_value(P, index);
//     paw_rotate(P, -3, 1);
//     pawR_setitem_raw(P);
// }
//
// void paw_set_attr(paw_Env *P, int index, const char *s)
//{
//     paw_push_value(P, index);
//     paw_push_string(P, s);
//     paw_rotate(P, -3, -1);
//     pawR_setattr_raw(P);
// }
//
void paw_new_list(paw_Env *P, int n)
{
    pawR_literal_list(P, n);
}

void paw_new_map(paw_Env *P, int n)
{
    pawR_literal_map(P, n);
}

// void paw_create_struct(paw_Env *P)
//{
//    pawV_push_struct(P);
//}
//
// void paw_create_instance(paw_Env *P, int index)
//{
//    const Value cls = *at(P, index);
//    if (pawV_is_struct(cls)) {
//        Value *pv = pawC_push0(P);
//        pawV_new_instance(P, pv, V_STRUCT(cls));
//    }
//}
//
// void paw_create_native(paw_Env *P, paw_Function f, int nup)
//{
//    // Save the index of the first upvalue.
//    const int base = paw_get_count(P) - nup;
//    Value *pv = pawC_push0(P);
//    Native *nt = pawV_new_native(P, f, nup);
//    V_SET_NATIVE(pv, nt);
//
//    for (int i = 0; i < nup; ++i) {
//        paw_push_value(P, base + i);
//        paw_set_upvalue(P, -1, i);
//    }
//    paw_shift(P, nup);
//}
//
// void *paw_create_foreign(paw_Env *P, size_t size, int nbound)
//{
//    Foreign *ud = pawV_push_foreign(P, size, nbound);
//    return ud->data;
//}

int paw_abs_index(paw_Env *P, int index)
{
    return index + (index < 0 ? paw_get_count(P) : 0);
}

static void reverse(StackPtr from, StackPtr to)
{
    for (; from < to; from++, to--) {
        Value temp = *from;
        *from = *to;
        *to = temp;
    }
}

// Rotate function from Lua. Original comment:
//   Let x = AB, where A is a prefix of length 'n'. Then,
//   rotate x n == BA. But BA == (A^r . B^r)^r.
void paw_rotate(paw_Env *P, int index, int n)
{
    StackPtr t = P->top.p - 1;
    StackPtr p = at(P, index);
    paw_assert((n >= 0 ? n : -n) <= t - p + 1);
    StackPtr m = n >= 0 ? t - n : p - n - 1;
    reverse(p, m); // Reverse A to get A'
    reverse(m + 1, t); // Reverse B to get B'
    reverse(p, t); // Reverse A'B' to get BA
}

void paw_copy(paw_Env *P, int from, int to)
{
    StackPtr src = at(P, from);
    StackPtr dst = at(P, to);
    *dst = *src;
}

void paw_shift(paw_Env *P, int n)
{
    paw_copy(P, -1, -n - 1);
    paw_pop(P, n);
}

void paw_call_global(paw_Env *P, int index, int argc)
{
    paw_get_global(P, index);
    paw_insert(P, -argc - 2);
    paw_call(P, argc);
}

// void paw_call_attr(paw_Env *P, int index, const char *name, int argc)
//{
//     paw_push_value(P, index);
//     paw_push_string(P, name);
//     paw_insert(P, -argc - 3);
//     paw_call(P, argc);
// }
//
void paw_unop(paw_Env *P, int op, paw_Type type)
{
     pawR_unop(P, CAST(op, UnaryOp), type);
}

void paw_binop(paw_Env *P, int op, paw_Type type)
{
     pawR_binop(P, CAST(op, BinaryOp), type);
}

void paw_raw_equals(paw_Env *P)
{
    const Value x = P->top.p[-2];
    const Value y = P->top.p[-1];
    paw_push_boolean(P, x.u == y.u);
    paw_shift(P, 2);
}
