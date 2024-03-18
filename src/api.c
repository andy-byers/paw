// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "aux.h"
#include "bigint.h"
#include "call.h"
#include "error.h"
#include "gc.h"
#include "lib.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "paw.h"
#include "rt.h"
#include "str.h"
#include "value.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static void *default_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    paw_unused(ud);
    if (size0 == 0) {
        return malloc(cast_size(size));
    } else if (size == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, cast_size(size));
}

static StackPtr access(paw_Env *P, int index)
{
    const int i = paw_abs_index(P, index);
    paw_assert(i < paw_get_count(P));
    return &P->cf->base[i];
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

size_t paw_bytes_used(const paw_Env *P)
{
    return P->gc_bytes;
}

static void open_aux(paw_Env *P, void *arg)
{
    paw_unused(arg);
    pawG_init(P);
    pawC_init(P);

    P->globals = pawH_new(P);

    pawS_init(P);
    pawP_init(P);
    pawL_init(P);
    CHECK_GC(P);
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
        // Not enough memory
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

    paw_assert(P->gc_bytes == 0);
    P->alloc(P->ud, P, sizeof *P, 0);
}

paw_Bool paw_is_truthy(paw_Env *P, int index)
{
    return pawV_truthy(*access(P, index));
}

paw_Bool paw_is_null(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TNULL;
}

paw_Bool paw_is_boolean(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TBOOLEAN;
}

paw_Bool paw_is_float(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TFLOAT;
}

paw_Bool paw_is_integer(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TINTEGER;
}

paw_Bool paw_is_string(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TSTRING;
}

paw_Bool paw_is_function(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TFUNCTION;
}

paw_Bool paw_is_array(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TARRAY;
}

paw_Bool paw_is_map(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TMAP;
}

paw_Bool paw_is_class(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TCLASS;
}

paw_Bool paw_is_instance(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TINSTANCE;
}

paw_Bool paw_is_userdata(paw_Env *P, int index)
{
    return paw_type(P, index) == PAW_TUSERDATA;
}

paw_Bool paw_is_bigint(paw_Env *P, int index)
{
    return pawV_is_bigint(*access(P, index));
}

int paw_type(paw_Env *P, int index)
{
    const Value v = *access(P, index);
    switch (pawV_get_type(v)) {
        case VNULL:
            return PAW_TNULL;
        case VTRUE:
        case VFALSE:
            return PAW_TBOOLEAN;
        case VBIGINT:
            return PAW_TINTEGER;
        case VCLOSURE:
        case VMETHOD:
        case VNATIVE:
            return PAW_TFUNCTION;
        case VSTRING:
            return PAW_TSTRING;
        case VARRAY:
            return PAW_TARRAY;
        case VMAP:
            return PAW_TMAP;
        case VCLASS:
            return PAW_TCLASS;
        case VINSTANCE:
            return PAW_TINSTANCE;
        case VNUMBER:
            return PAW_TINTEGER;
        case VUSERDATA:
            return PAW_TUSERDATA;
        default:
            return PAW_TFLOAT;
    }
}

const char *paw_typename(paw_Env *P, int index)
{
    return api_typename(paw_type(P, index));
}

void paw_push_value(paw_Env *P, int index)
{
    const Value v = *access(P, index);
    StackPtr sp = pawC_stkinc(P, 1);
    *sp = v;
}

void paw_push_nnull(paw_Env *P, int n)
{
    pawC_stkinc(P, n);
}

void paw_push_boolean(paw_Env *P, paw_Bool b)
{
    StackPtr sp = pawC_stkinc(P, 1);
    pawV_set_bool(sp, b);
}

void paw_push_float(paw_Env *P, paw_Float f)
{
    StackPtr sp = pawC_stkinc(P, 1);
    pawV_set_float(sp, f);
}

void paw_push_int(paw_Env *P, paw_Int i)
{
    StackPtr sp = pawC_stkinc(P, 1);
    if (i < PAW_INT_MIN || i > PAW_INT_MAX) {
        // 'i' is wider than 47 bits and must go in a bigint.
        pawB_from_int(P, sp, i);
    } else {
        pawV_set_int(sp, i);
    }
}

void paw_push_native(paw_Env *P, paw_Function fn, int n)
{
    StackPtr sp = pawC_stkinc(P, 1);
    Native *o = pawV_new_native(P, fn, n);
    pawV_set_native(sp, o);

    const StackPtr base = P->top - n - 1;
    for (int i = 0; i < n; ++i) {
        o->up[i] = base[i];
    }
    // Replace upvalues with closure object
    base[0] = P->top[-1];
    P->top = base + 1;
}

const char *paw_push_string(paw_Env *P, const char *s)
{
    return paw_push_nstring(P, s, strlen(s));
}

const char *paw_push_nstring(paw_Env *P, const char *s, size_t n)
{
    StackPtr sp = pawC_stkinc(P, 1);
    String *o = pawS_new_nstr(P, s, n);
    pawV_set_string(sp, o);
    return o->text;
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

void paw_push_bigint(paw_Env *P, paw_Digit *d, int n, int neg)
{
    const BigInt bi = {
        .neg = neg,
        .size = n,
        .buf = d,
    };
    StackPtr sp = pawC_stkinc(P, 1);
    pawB_copy(P, sp, &bi, 0);
}

int paw_boolean(paw_Env *P, int index)
{
    const Value v = *access(P, index);
    return pawV_truthy(v);
}

paw_Int paw_intx(paw_Env *P, int index, paw_Bool *plossless)
{
    paw_Bool lossless;
    const Value v = *access(P, index);
    const paw_Int i = pawV_to_int64(P, v, &lossless);
    if (plossless) {
        *plossless = lossless;
    }
    return i;
}

paw_Float paw_float(paw_Env *P, int index)
{
    Value v = *access(P, index);
    return pawV_to_float(P, v);
}

const char *paw_string(paw_Env *P, int index)
{
    StackPtr sp = access(P, index);
    const String *s = pawV_get_string(*sp);
    return s->text;
}

paw_Function paw_native(paw_Env *P, int index)
{
    return pawV_get_native(*access(P, index))->f;
}

paw_Digit *paw_bigint(paw_Env *P, int index)
{
    return pawV_get_bigint(*access(P, index))->buf;
}

void *paw_pointer(paw_Env *P, int index)
{
    Value *pv = access(P, index);
    if (pawV_is_userdata(*pv)) {
        return pawV_get_userdata(*pv)->data;
    }
    return pv;
}

void paw_to_float(paw_Env *P, int index)
{
    const int i = paw_abs_index(P, index);
    paw_push_value(P, i);
    pawR_to_float(P);
    paw_replace(P, i);
}

void paw_to_integer(paw_Env *P, int index)
{
    const int i = paw_abs_index(P, index);
    paw_push_value(P, i);
    pawR_to_integer(P);
    paw_replace(P, i);
}

void paw_to_string(paw_Env *P, int index)
{
    const int i = paw_abs_index(P, index);
    paw_push_value(P, i);
    pawR_to_string(P);
    paw_replace(P, i);
}

size_t paw_length(paw_Env *P, int index)
{
    paw_push_value(P, index);
    pawR_length(P); // replace value with its length

    const paw_Int n = paw_int(P, -1);
    paw_pop(P, 1);
    return cast_size(n);
}

void paw_pop(paw_Env *P, int n)
{
    paw_assert(n <= pawC_stklen(P));
    pawC_stkdec(P, n);
}

struct Parser {
    paw_Reader input;
    ParseMemory mem;
    const char *name;
    void *ud;
};

static void parse_aux(paw_Env *P, void *arg)
{
    struct Parser *p = arg;
    pawP_parse(P, p->input, &p->mem, p->name, p->ud);
}

int paw_load(paw_Env *P, paw_Reader input, const char *name, void *ud)
{
    struct Parser p = {
        .input = input,
        .name = name,
        .ud = ud,
    };
    const int status = pawC_try(P, parse_aux, &p);
    pawM_free_vec(P, p.mem.temp, p.mem.talloc);
    pawM_free_vec(P, p.mem.vars, p.mem.valloc);
    pawM_free_vec(P, p.mem.ll.values, p.mem.ll.capacity);
    return status;
}

struct EvalCtx {
    StackPtr fn;
    int argc;
};

void eval_aux(paw_Env *P, void *arg)
{
    const struct EvalCtx *ctx = arg;
    pawC_call(P, *ctx->fn, ctx->argc);
}

int paw_call(paw_Env *P, int argc)
{
    StackPtr fn = &P->top[-argc - 1];
    struct EvalCtx ctx = {.fn = fn, .argc = argc};
    const int status = pawC_try(P, eval_aux, &ctx);
    return status;
}

int paw_get_count(paw_Env *P)
{
    return P->top - P->cf->base;
}

void paw_set_top(paw_Env *P, int index)
{
    CallFrame *cf = P->cf;
    StackPtr bp = cf->base;
    const StackPtr end = bp + index + 1;
    if (index >= 0) {
        while (P->top != end) {
            pawV_set_null(P->top);
            ++P->top;
        }
    } else {
        pawR_close_upvalues(P, end);
        P->top = end;
    }
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
    StackPtr sp = pawC_stkinc(P, 1);
    const Value fn = *access(P, ifn);
    switch (pawV_get_type(fn)) {
        case VNATIVE: {
            Native *f = pawV_get_native(fn);
            *sp = f->up[upvalue_index(f->nup, index)];
            break;
        }
        case VCLOSURE: {
            Closure *f = pawV_get_closure(fn);
            UpValue *u = f->up[upvalue_index(f->nup, index)];
            *sp = *u->p;
            break;
        }
        default:
            pawV_type_error(P, fn);
    }
}

void paw_get_global(paw_Env *P, const char *name)
{
    if (!paw_check_global(P, name)) {
        pawE_name(P, name);
    }
}

paw_Bool paw_check_global(paw_Env *P, const char *name)
{
    paw_push_string(P, name);
    const Value key = *access(P, -1);

    paw_Bool found = PAW_BFALSE;
    if (pawR_read_global(P, key)) {
        paw_push_null(P);
    } else {
        found = PAW_BTRUE;
    }
    paw_shift(P, 1); // replace 'key'
    return found;
}

void paw_get_itemi(paw_Env *P, int index, paw_Int i)
{
    const int abs = paw_abs_index(P, index);
    paw_push_int(P, i);
    paw_get_item(P, abs);
}

paw_Bool paw_check_itemi(paw_Env *P, int index, paw_Int i)
{
    const int abs = paw_abs_index(P, index);
    paw_push_int(P, i);
    return paw_check_item(P, abs);
}

void paw_get_item(paw_Env *P, int index)
{
    if (!paw_check_item(P, index)) {
        pawE_key(P, paw_string(P, -1));
    }
}

paw_Bool paw_check_item(paw_Env *P, int index)
{
    paw_push_value(P, index); // push container
    paw_rotate(P, -2, 1);     // place container below key
    if (pawR_getitem_raw(P, PAW_BFALSE)) {
        paw_push_null(P);
        paw_shift(P, 2);
        return PAW_BFALSE;
    }
    return PAW_BTRUE;
}

void paw_get_attr(paw_Env *P, int index, const char *attr)
{
    if (!paw_check_attr(P, index, attr)) {
        pawE_attr(P, paw_string(P, -1));
    }
}

paw_Bool paw_check_attr(paw_Env *P, int index, const char *attr)
{
    paw_push_value(P, index); // push instance
    paw_push_string(P, attr); // push attribute name
    if (pawR_getattr_raw(P, PAW_BFALSE)) {
        paw_push_null(P);
        paw_shift(P, 2);
        return PAW_BFALSE;
    }
    return PAW_BTRUE;
}

void paw_set_upvalue(paw_Env *P, int ifn, int index)
{
    const Value fn = *access(P, ifn);
    switch (pawV_get_type(fn)) {
        case VNATIVE: {
            Native *f = pawV_get_native(fn);
            Value *v = &f->up[upvalue_index(f->nup, index)];
            *v = P->top[-1];
            paw_pop(P, 1);
            break;
        }
        case VCLOSURE: {
            Closure *f = pawV_get_closure(fn);
            UpValue *u = f->up[upvalue_index(f->nup, index)];
            *u->p = P->top[-1];
            paw_pop(P, 1);
            break;
        }
        default:
            break;
    }
}

void paw_set_global(paw_Env *P, const char *name)
{
    paw_push_string(P, name);
    paw_rotate(P, -2, 1); // Swap

    const Value key = P->top[-2];
    pawR_write_global(P, key, PAW_BTRUE);
    paw_pop(P, 1); // Pop 'key'
}

void paw_set_itemi(paw_Env *P, int index, paw_Int idx)
{
    const int abs = paw_abs_index(P, index);
    paw_push_int(P, idx);
    paw_set_item(P, abs);
}

void paw_set_item(paw_Env *P, int index)
{
    paw_push_value(P, index);
    paw_rotate(P, -3, 1);
    pawR_setitem_raw(P);
}

void paw_set_attr(paw_Env *P, int index, const char *s)
{
    paw_push_value(P, index);
    paw_push_string(P, s);
    paw_rotate(P, -3, -1);
    pawR_setattr_raw(P);
}

void paw_create_array(paw_Env *P, int n)
{
    pawR_literal_array(P, n);
}

void paw_create_map(paw_Env *P, int n)
{
    pawR_literal_map(P, n);
}

void paw_create_class(paw_Env *P)
{
    pawV_push_class(P);
}

void paw_create_instance(paw_Env *P, int index)
{
    const Value cls = *access(P, index);
    if (pawV_is_class(cls)) {
        StackPtr sp = pawC_stkinc(P, 1);
        pawV_new_instance(P, sp, pawV_get_class(cls));
    }
}

void paw_create_native(paw_Env *P, paw_Function f, int nup)
{
    // Save the index of the first upvalue.
    const int base = paw_get_count(P) - nup;
    StackPtr sp = pawC_stkinc(P, 1);
    Native *nt = pawV_new_native(P, f, nup);
    pawV_set_native(sp, nt);

    for (int i = 0; i < nup; ++i) {
        paw_push_value(P, base + i);
        paw_set_upvalue(P, -1, i);
    }
    paw_shift(P, nup);
}

void *paw_create_userdata(paw_Env *P, size_t size)
{
    UserData *ud = pawV_push_userdata(P, size);
    return ud->data;
}

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
    StackPtr t = P->top - 1;
    StackPtr p = access(P, index);
    paw_assert((n >= 0 ? n : -n) <= t - p + 1);
    StackPtr m = n >= 0 ? t - n : p - n - 1;
    reverse(p, m);     // Reverse A to get A'
    reverse(m + 1, t); // Reverse B to get B'
    reverse(p, t);     // Reverse A'B' to get BA
}

void paw_shift(paw_Env *P, int n)
{
    P->top[-n - 1] = P->top[-1];
    paw_pop(P, n);
}

void paw_copy(paw_Env *P, int from, int to)
{
    StackPtr src = access(P, from);
    StackPtr dst = access(P, to);
    *dst = *src;
}

void paw_call_global(paw_Env *P, const char *name, int argc)
{
    paw_get_global(P, name);
    paw_insert(P, -argc - 2);
    paw_call(P, argc);
}

void paw_call_attr(paw_Env *P, int index, const char *name, int argc)
{
    paw_push_value(P, index);
    paw_push_string(P, name);
    paw_insert(P, -argc - 3);
    paw_call(P, argc);
}

void paw_arith(paw_Env *P, int op)
{
    paw_assert(0 <= op && op <= PAW_OPSHR);
    op += OP_NEG; // add opcode offset
    pawR_arith(P, (Op)op);
}

void paw_compare(paw_Env *P, int op)
{
    op += OP_EQ;
    pawR_compare(P, (Op)op);
}

void paw_raw_equals(paw_Env *P)
{
    const Value x = P->top[-2];
    const Value y = P->top[-1];
    paw_push_boolean(P, pawV_equal(x, y));
    paw_shift(P, 2);
}
