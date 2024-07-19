// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "gc_aux.h"
#include "lib.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "paw.h"
#include "rt.h"
#include "str.h"
#include "type.h"
#include "value.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <gc.h>

static void *default_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    paw_unused(ud);
    if (size0 == 0) {
        return GC_MALLOC(size);
        // return malloc(size);
    } else if (size == 0) {
        free(ptr);
        return NULL;
    }
    return GC_REALLOC(ptr, size);
    // return realloc(ptr, size);
}

static StackPtr access(paw_Env *P, int index)
{
    const int i = paw_abs_index(P, index);
    paw_assert(i < paw_get_count(P));
    return &P->cf->base.p[i];
}

paw_Alloc paw_get_allocator(paw_Env *P) { return P->alloc; }

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
    check_gc(P);
}

paw_Env *paw_open(paw_Alloc alloc, void *ud)
{
    alloc = alloc ? alloc : default_alloc;
    paw_Env *P = alloc(ud, NULL, 0, sizeof *P);
    if (!P) {
        return NULL;
    }
    *P = (paw_Env){
        .alloc = alloc,
        .ud = ud,
    };

    if (pawC_try(P, open_aux, NULL)) {
        paw_close(P);
        return NULL;
    }
    return P;
}

void paw_close(paw_Env *P)
{
    pawG_uninit(P);
    pawC_uninit(P);
    pawS_uninit(P);

    P->alloc(P->ud, P, sizeof *P, 0);
}

paw_Bool paw_is_bool(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TBOOL;
}

paw_Bool paw_is_int(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TINT;
}

paw_Bool paw_is_float(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TFLOAT;
}

paw_Bool paw_is_string(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TSTRING;
}

paw_Bool paw_is_function(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TFUNCTION;
}

paw_Bool paw_is_tuple(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TTUPLE;
}

paw_Bool paw_is_struct(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TSTRUCT;
}

paw_Bool paw_is_foreign(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TFOREIGN;
}

int paw_type(paw_Env *P, int index)
{
    const Value v = *access(P, index);
    return 0;
    // switch (v_type(v)) {
    //     case VNULL:
    //         return PAW_NULL;
    //     case VTRUE:
    //     case VFALSE:
    //         return PAW_TBOOL;
    //     case VCLOSURE:
    //     case VMETHOD:
    //     case VNATIVE:
    //         return PAW_TFUNCTION;
    //     case VSTRING:
    //         return PAW_TSTRING;
    //     case VARRAY:
    //         return PAW_TARRAY;
    //     case VMAP:
    //         return PAW_TMAP;
    //     case VSTRUCT:
    //         return PAW_TSTRUCT;
    //     case VBIGINT:
    //     case VNUMBER:
    //         return PAW_TINT;
    //     case VFOREIGN:
    //         return PAW_TFOREIGN;
    //     default:
    //         return PAW_TFLOAT;
    // }
}

const char *paw_typename(paw_Env *P, int index)
{
    return api_typename(paw_type(P, index));
}

void paw_push_value(paw_Env *P, int index)
{
    const Value v = *access(P, index);
    pawC_pushv(P, v);
}

void paw_push_unit(paw_Env *P, int n)
{
    for (int i = 0; i < n; ++i) {
        pawC_push0(P);
    }
}

void paw_push_boolean(paw_Env *P, paw_Bool b) { pawC_pushb(P, b); }

void paw_push_float(paw_Env *P, paw_Float f) { pawC_pushf(P, f); }

void paw_push_int(paw_Env *P, paw_Int i) { pawC_pushi(P, i); }

void paw_push_native(paw_Env *P, paw_Function fn, int n)
{
    Value *pv = pawC_push0(P);
    Native *o = pawV_new_native(P, fn, n);
    v_set_object(pv, o);

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
    return v_text(*pv);
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

paw_Bool paw_bool(paw_Env *P, int index) { return v_true(*access(P, index)); }

paw_Int paw_int(paw_Env *P, int index) { return v_int(*access(P, index)); }

paw_Float paw_float(paw_Env *P, int index)
{
    return v_float(*access(P, index));
}

const char *paw_string(paw_Env *P, int index)
{
    const String *s = v_string(*access(P, index));
    return s->text;
}

paw_Function paw_native(paw_Env *P, int index)
{
    const Native *f = v_native(*access(P, index));
    return f->func;
}

void *paw_pointer(paw_Env *P, int index)
{
    const Foreign *f = v_foreign(*access(P, index));
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
            result = pawS_length(v_string(v));
            break;
        case VVECTOR:
            result = pawV_vec_length(v_vector(v));
            break;
        case VMAP:
            result = pawH_length(v_map(v));
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

struct ParseState {
    paw_Reader input;
    ParseMemory mem;
    const char *name;
    void *ud;
};

static void parse_aux(paw_Env *P, void *arg)
{
    struct ParseState *p = arg;
    pawP_parse(P, p->input, &p->mem, p->name, p->ud);
}

int paw_load(paw_Env *P, paw_Reader input, const char *name, void *ud)
{
    struct ParseState p = {
        .input = input,
        .name = name,
        .ud = ud,
    };
    const int status = pawC_try(P, parse_aux, &p);
    pawM_free_vec(P, p.mem.scratch.data, p.mem.scratch.alloc);
    pawM_free_vec(P, p.mem.labels.values, p.mem.labels.capacity);
    return status;
}

struct CallState {
    StackPtr fn;
    int argc;
};

void eval_aux(paw_Env *P, void *arg)
{
    const struct CallState *ctx = arg;
    pawC_call(P, v_object(*ctx->fn), ctx->argc);
}

int paw_call(paw_Env *P, int argc)
{
    StackPtr top = P->top.p;
    StackPtr fn = &top[-argc - 1];
    struct CallState c = {.fn = fn, .argc = argc};
    const int status = pawC_try(P, eval_aux, &c);
    return status;
}

int paw_get_count(paw_Env *P) { return P->top.p - P->cf->base.p; }

static int upvalue_index(int nup, int index)
{
    if (index < 0) {
        paw_assert(-index <= nup);
        return nup - index;
    }
    paw_assert(index <= nup);
    return index;
}

// void paw_get_upvalue(paw_Env *P, int ifn, int index)
//{
//     Value *pv = pawC_push0(P);
//     const Value fn = *access(P, ifn);
//     switch (v_type(fn)) {
//         case VNATIVE: {
//             Native *f = v_native(fn);
//             *pv = f->up[upvalue_index(f->nup, index)];
//             break;
//         }
//         case VCLOSURE: {
//             Closure *f = v_closure(fn);
//             UpValue *u = f->up[upvalue_index(f->nup, index)];
//             *pv = *u->p.p;
//             break;
//         }
//         default:
//             pawR_error(P, PAW_ETYPE, "'%s' has no upvalues",
//                        api_typename(api_type(fn)));
//     }
// }
//
// void paw_get_global(paw_Env *P, const char *name)
//{
//     if (!paw_check_global(P, name)) {
//         pawR_error(P, PAW_ENAME, "global '%s' does not exist", name);
//     }
// }
//
// paw_Bool paw_check_global(paw_Env *P, const char *name)
//{
//     paw_push_string(P, name);
//     const Value key = *access(P, -1);
//
//     paw_Bool found = PAW_FALSE;
//     if (pawR_read_global(P, key)) {
//         paw_push_null(P);
//     } else {
//         found = PAW_TRUE;
//     }
//     paw_shift(P, 1); // replace 'key'
//     return found;
// }
//
// void paw_get_itemi(paw_Env *P, int index, paw_Int i)
//{
//     const int abs = paw_abs_index(P, index);
//     paw_push_int(P, i);
//     paw_get_item(P, abs);
// }
//
// paw_Bool paw_check_itemi(paw_Env *P, int index, paw_Int i)
//{
//     const int abs = paw_abs_index(P, index);
//     paw_push_int(P, i);
//     return paw_check_item(P, abs);
// }
//
// void paw_get_item(paw_Env *P, int index)
//{
//     if (!paw_check_item(P, index)) {
//         // If the target container is a sequence, and the integer key is
//         // out of bounds, then the runtime will throw a PAW_EINDEX before
//         // this code is reached. The key must be a string if control has
//         // made it here.
//         pawR_error(P, PAW_ENAME, "key '%s' does not exist", paw_string(P,
//         -1));
//     }
// }
//
// paw_Bool paw_check_item(paw_Env *P, int index)
//{
//     paw_push_value(P, index); // push container
//     paw_rotate(P, -2, 1); // place container below key
//     if (pawR_getitem(P)) {
//         paw_push_null(P);
//         paw_shift(P, 2);
//         return PAW_FALSE;
//     }
//     return PAW_TRUE;
// }
//
// void paw_get_attr(paw_Env *P, int index, const char *attr)
//{
//     if (!paw_check_attr(P, index, attr)) {
//         pawR_error(P, PAW_EATTR, "attribute '%s' does not exist", attr);
//     }
// }
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
//     StackPtr top = P->top.p;
//     const Value fn = *access(P, ifn);
//     switch (v_type(fn)) {
//         case VNATIVE: {
//             Native *f = v_native(fn);
//             Value *v = &f->up[upvalue_index(f->nup, index)];
//             *v = top[-1];
//             paw_pop(P, 1);
//             break;
//         }
//         case VCLOSURE: {
//             Closure *f = v_closure(fn);
//             UpValue *u = f->up[upvalue_index(f->nup, index)];
//             *u->p.p = top[-1];
//             paw_pop(P, 1);
//             break;
//         }
//         default:
//             break;
//     }
// }
//
void paw_set_global(paw_Env *P, const char *name)
{
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
void paw_create_array(paw_Env *P, int n)
{
    //    pawR_literal_array(P, n);
}
//
// void paw_create_map(paw_Env *P, int n)
//{
//    pawR_literal_map(P, n);
//}
//
// void paw_create_struct(paw_Env *P)
//{
//    pawV_push_struct(P);
//}
//
// void paw_create_instance(paw_Env *P, int index)
//{
//    const Value cls = *access(P, index);
//    if (pawV_is_struct(cls)) {
//        Value *pv = pawC_push0(P);
//        pawV_new_instance(P, pv, v_struct(cls));
//    }
//}
//
// void paw_create_native(paw_Env *P, paw_Function f, int nup)
//{
//    // Save the index of the first upvalue.
//    const int base = paw_get_count(P) - nup;
//    Value *pv = pawC_push0(P);
//    Native *nt = pawV_new_native(P, f, nup);
//    v_set_native(pv, nt);
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
    StackPtr p = access(P, index);
    paw_assert((n >= 0 ? n : -n) <= t - p + 1);
    StackPtr m = n >= 0 ? t - n : p - n - 1;
    reverse(p, m); // Reverse A to get A'
    reverse(m + 1, t); // Reverse B to get B'
    reverse(p, t); // Reverse A'B' to get BA
}

void paw_copy(paw_Env *P, int from, int to)
{
    StackPtr src = access(P, from);
    StackPtr dst = access(P, to);
    *dst = *src;
}

void paw_shift(paw_Env *P, int n)
{
    paw_copy(P, -1, -n - 1);
    paw_pop(P, n);
}

// void paw_call_global(paw_Env *P, const char *name, int argc)
//{
//     paw_get_global(P, name);
//     paw_insert(P, -argc - 2);
//     paw_call(P, argc);
// }
//
// void paw_call_attr(paw_Env *P, int index, const char *name, int argc)
//{
//     paw_push_value(P, index);
//     paw_push_string(P, name);
//     paw_insert(P, -argc - 3);
//     paw_call(P, argc);
// }
//
// void paw_unop(paw_Env *P, int op)
//{
//     pawR_unop(P, (UnaryOp)op);
// }
//
// void paw_binop(paw_Env *P, int op)
//{
//     pawR_binop(P, (BinaryOp)op);
// }

void paw_raw_equals(paw_Env *P)
{
    const Value x = P->top.p[-2];
    const Value y = P->top.p[-1];
    // paw_push_boolean(P, pawV_equal(x, y));
    paw_shift(P, 2);
}
