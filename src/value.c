// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "array.h"
#include "bigint.h"
#include "gc.h"
#include "map.h"
#include "mem.h"
#include "str.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static void int_to_string(paw_Env *P, paw_Int i)
{
    char temp[32];
    const paw_Bool negative = i < 0;
    char *end = temp + paw_countof(temp);
    char *ptr = end - 1;

    // Don't call llabs(INT64_MIN). The result is undefined on 2s complement systems.
    uint64_t u = i == INT64_MIN ? (1ULL << 63) : (uint64_t)llabs(i);
    do {
        *ptr-- = (char)(u % 10 + '0');
        u /= 10;
    } while (u);
    if (negative) {
        *ptr = '-';
    } else {
        ++ptr;
    }
    pawC_pushns(P, ptr, cast_size(end - ptr));
}

static void float_to_string(paw_Env *P, paw_Float f)
{
    char temp[32];
    const int n = snprintf(temp, paw_countof(temp), "%.*g", 17, f);
    pawC_pushns(P, temp, cast_size(n));
}

const char *pawV_to_string(paw_Env *P, Value v, size_t *nout)
{
    switch (pawV_get_type(v)) {
        case VSTRING:
            pawC_pushv(P, v); // copy
            break;
        case VNULL:
            pawC_pushv(P, pawE_cstr(P, CSTR_NULL));
            break;
        case VTRUE:
            pawC_pushv(P, pawE_cstr(P, CSTR_TRUE));
            break;
        case VFALSE:
            pawC_pushv(P, pawE_cstr(P, CSTR_FALSE));
            break;
        case VBIGINT: {
            const BigInt *bi = pawV_get_bigint(v);
            pawB_to_string(P, bi, PAW_FALSE, "", 10);
            break; // string on top of stack
        }
        case VNUMBER: // int
            int_to_string(P, pawV_get_int(v));
            break;
        default:
            if (pawV_is_float(v)) {
                float_to_string(P, pawV_get_float(v));
            } else {
                return NULL;
            }
    }
    const String *s = pawV_get_string(P->top.p[-1]);
    *nout = s->length;
    return s->text;
}

const char *pawV_name(ValueKind kind)
{
    switch (kind) {
        case VNULL:
            return "null";
        case VTRUE:
        case VFALSE:
            return "bool";
        case VNUMBER:
        case VBIGINT:
            return "int";
        case VNATIVE:
            return "cfunction";
        case VUPVALUE:
            return "upvalue";
        case VCLOSURE:
            return "closure";
        case VPROTO:
            return "proto";
        case VSTRING:
            return "string";
        case VARRAY:
            return "array";
        case VMAP:
            return "map";
        case VCLASS:
            return "class";
        case VINSTANCE:
            return "instance";
        case VMETHOD:
            return "method";
        case VUSERDATA:
            return "userdata";
        default:
            return "float";
    }
}

Proto *pawV_new_proto(paw_Env *P)
{
    Proto *p = pawM_new(P, Proto);
    pawG_add_object(P, cast_object(p), VPROTO);
    return p;
}

void pawV_free_proto(paw_Env *P, Proto *f)
{
    pawM_free_vec(P, f->source, f->length);
    pawM_free_vec(P, f->lines, f->nlines);
    pawM_free_vec(P, f->p, f->nproto);
    pawM_free_vec(P, f->k, f->nk);
    pawM_free_vec(P, f->v, f->ndebug);
    pawM_free_vec(P, f->u, f->nup);
    pawM_free(P, f);
}

UpValue *pawV_new_upvalue(paw_Env *P)
{
    UpValue *u = pawM_new(P, UpValue);
    pawG_add_object(P, cast_object(u), VUPVALUE);
    return u;
}

void pawV_free_upvalue(paw_Env *P, UpValue *u)
{
    if (upv_is_open(u)) {
        pawV_unlink_upvalue(u);
    }
    pawM_free(P, u);
}

void pawV_link_upvalue(paw_Env *P, UpValue *u, UpValue *prev, UpValue *next)
{
    u->open.next = next;
    if (next) {
        u->open.prev = next->open.prev;
        next->open.prev = u;
    }
    if (prev) {
        prev->open.next = u;
    } else {
        P->up_list = u;
    }
}

void pawV_unlink_upvalue(UpValue *u)
{
    UpValue *prev = u->open.prev;
    UpValue *next = u->open.next;
    if (prev) {
        prev->open.next = next;
    }
    if (next) {
        next->open.prev = prev;
    }
}

Closure *pawV_new_closure(paw_Env *P, int nup)
{
    // Tack on enough space to store 'nup' pointers to UpValue.
    Closure *f = pawM_new_flex(P, Closure, nup, sizeof(f->up[0]));
    pawG_add_object(P, cast_object(f), VCLOSURE);
    memset(f->up, 0, cast_size(nup) * sizeof(f->up[0]));
    f->nup = nup;
    return f;
}

void pawV_free_closure(paw_Env *P, Closure *f)
{
    pawM_free_flex(P, f, f->nup, sizeof(f->up[0]));
}

Native *pawV_new_native(paw_Env *P, paw_Function f, int nup)
{
    Native *nt = pawM_new_flex(P, Native, nup, sizeof(nt->up[0]));
    pawG_add_object(P, cast_object(nt), VNATIVE);
    memset(nt->up, 0, cast_size(nup) * sizeof(nt->up[0]));
    nt->nup = nup;
    nt->f = f;
    return nt;
}

void pawV_free_native(paw_Env *P, Native *nt)
{
    pawM_free_flex(P, nt, nt->nup, sizeof(nt->up[0]));
}

Instance *pawV_new_instance(paw_Env *P, StackPtr sp, Class *cls)
{
    Instance *ins = pawM_new(P, Instance);
    pawG_add_object(P, cast_object(ins), VINSTANCE);
    pawV_set_instance(sp, ins); // anchor
    ins->self = cls;
    ins->attr = pawH_new(P);
    pawH_extend(P, ins->attr, cls->attr);
    return ins;
}

void pawV_free_instance(paw_Env *P, Instance *i)
{
    // Members are managed separately.
    pawM_free(P, i);
}

Class *pawV_push_class(paw_Env *P)
{
    StackPtr sp = pawC_stkinc(P, 1);
    Class *cls = pawM_new(P, Class);
    pawG_add_object(P, cast_object(cls), VCLASS);
    pawV_set_class(sp, cls); // anchor
    cls->attr = pawH_new(P);
    return cls;
}

void pawV_free_class(paw_Env *P, Class *c)
{
    pawM_free(P, c);
}

Method *pawV_new_method(paw_Env *P, Value self, Value call)
{
    Method *mtd = pawM_new(P, Method);
    pawG_add_object(P, cast_object(mtd), VMETHOD);
    mtd->self = self;
    mtd->f = call;
    return mtd;
}

void pawV_free_method(paw_Env *P, Method *m)
{
    pawM_free(P, m);
}

UserData *pawV_push_userdata(paw_Env *P, size_t size)
{
    if (size > PAW_SIZE_MAX) {
        pawM_error(P);
    }
    StackPtr sp = pawC_stkinc(P, 1);
    UserData *o = pawM_new(P, UserData);
    pawG_add_object(P, cast_object(o), VUSERDATA);
    pawV_set_userdata(sp, o); // anchor
    o->attr = pawH_new(P);
    o->size = size;
    if (size) {
        o->data = pawM_new_vec(P, size, char);
    }
    return o;
}

void pawV_free_userdata(paw_Env *P, UserData *ud)
{
    pawM_free_vec(P, (char *)ud->data, ud->size);
    pawM_free(P, ud);
}

int pawV_num2int(Value *pv)
{
    if (pawV_is_int(*pv)) {
        // already an int
    } else if (pawV_is_float(*pv)) {
        pawV_set_int(pv, paw_cast_int(pawV_get_float(*pv)));
    } else if (pawV_is_bool(*pv)) {
        pawV_set_int(pv, pawV_get_bool(*pv));
    } else {
        return -1;
    }
    return 0;
}

int pawV_num2float(Value *pv)
{
    if (pawV_is_float(*pv)) {
        // already a float
    } else if (pawV_is_int(*pv)) {
        pawV_set_float(pv, (paw_Float)pawV_get_int(*pv));
    } else if (pawV_is_bigint(*pv)) {
        pawV_set_float(pv, pawB_get_float(pawV_get_bigint(*pv)));
    } else if (pawV_is_bool(*pv)) {
        pawV_set_float(pv, pawV_get_bool(*pv));
    } else {
        return -1;
    }
    return 0;
}

paw_Bool pawV_truthy(const Value v)
{
    if (pawV_is_bool(v)) {
        return pawV_get_bool(v);
    } else if (pawV_is_float(v)) {
        return pawV_get_float(v);
    } else if (pawV_is_int(v)) {
        return pawV_get_int(v);
    } else if (pawV_is_bigint(v)) {
        return !bi_zero(pawV_get_bigint(v));
    } else if (pawV_is_string(v)) {
        return pawS_length(pawV_get_string(v));
    } else if (pawV_is_array(v)) {
        return pawA_length(pawV_get_array(v));
    } else if (pawV_is_map(v)) {
        return pawH_length(pawV_get_map(v));
    }
    return PAW_FALSE;
}

uint32_t pawV_hash(const Value v)
{
    if (pawV_is_string(v)) {
        return pawV_get_string(v)->hash;
    }
    // from https://gist.github.com/badboy/6267743
    uint64_t u = v.u;
    u = ~u + (u << 18);
    u = u ^ (u >> 31);
    u = u * 21;
    u = u ^ (u >> 11);
    u = u + (u << 6);
    u = u ^ (u >> 22);
    return (uint32_t)u;
}

paw_Bool pawV_equal(Value x, Value y)
{
    if (pawV_is_string(x) && pawV_is_string(y)) {
        return pawV_get_string(x) == pawV_get_string(y);
    } else if (pawV_is_float(x) && pawV_is_float(y)) {
        return pawV_get_float(x) == pawV_get_float(y);
    } else if (pawV_is_int(x) && pawV_is_int(y)) {
        return pawV_get_int(x) == pawV_get_int(y);
    } else if (pawV_is_bool(x) || pawV_is_bool(y)) {
        return pawV_truthy(x) == pawV_truthy(y);
    } else if (pawV_is_null(x) || pawV_is_null(y)) {
        return pawV_is_null(x) && pawV_is_null(y);
    } else if ((pawV_is_float(x) && 0 == pawV_num2float(&y)) ||
               (pawV_is_float(y) && 0 == pawV_num2float(&x))) {
        // catches 'float x integer' and 'integer x float'
        return pawV_get_float(x) == pawV_get_float(y);
    } else if (pawV_is_bigint(x) || pawV_is_bigint(y)) {
        return pawB_equals(x, y);
    }
    // Fall back to value (type + pointer) comparison.
    return x.u == y.u;
}

static int char2base(char c)
{
    if (c == 'b' || c == 'B') {
        return 2;
    } else if (c == 'o' || c == 'O') {
        return 8;
    } else if (c == 'x' || c == 'X') {
        return 16;
    } else {
        return -1;
    }
}

static int check_suffix(const char *p, const char *text)
{
    if (*p && !ISSPACE(*p)) {
        return -1;
    }
    // If one of the pawV_parse_* functions are called on a string like " ",
    // then all of the checks will pass, despite " " not being a valid number.
    // Make sure that doesn't happen.
    if (p == text) {
        return -1;
    }
    return 0;
}

#define is_fp(c) (c == 'e' || c == 'E' || c == '.')

int pawV_parse_integer(paw_Env *P, const char *text)
{
    int base = 10;
    if (text[0] == '0') {
        if (is_fp(text[1])) {
            return -1; // maybe float
        } else if ((base = char2base(text[1])) > 0) {
            text += 2; // non-decimal integer
        } else if (text[1]) {
            return -1; // junk after '0'
        } else {
            pawC_pushi(P, 0); // exactly 0
            return 0;
        }
    }
    paw_Int value = 0;
    const char *p = text;
    for (; ISHEX(*p); ++p) {
        const int v = HEXVAL(*p);
        if (v >= base) {
            return -1;
        }
        if (value > (VINT_MAX - v) / base) {
            // Integer is too large: parse it as a BigInt. Throws an error on
            // allocation failure.
            return pawB_parse(P, text, base);
        }
        value = value * base + v;
    }
    if (check_suffix(p, text)) {
        return -1;
    }
    pawC_pushi(P, value);
    return 0;
}

#define skip_digits(p)      \
    while (ISDIGIT(*(p))) { \
        ++(p);              \
    }

int pawV_parse_float(paw_Env *P, const char *text)
{
    // First, validate the number format.
    const char *p = text;
    if (p[0] == '0' && p[1] && !is_fp(p[1])) {
        return -1;
    }
    skip_digits(p);

    if (*p == '.') {
        ++p;
        skip_digits(p);
    }
    if (*p == 'e' || *p == 'E') {
        ++p;
        if (*p == '+' || *p == '-') {
            ++p;
        }
        skip_digits(p);
    }
    if (check_suffix(p, text)) {
        return -1;
    }
    pawC_pushf(P, strtod(text, NULL));
    return 0;
}

paw_Int pawV_to_int64(Value v, paw_Bool *plossless)
{
    if (pawV_is_bigint(v)) {
        return pawB_get_int64(pawV_get_bigint(v), plossless);
    } else if (pawV_is_float(v)) {
        *plossless = PAW_FALSE;
        return paw_cast_int(pawV_get_float(v));
    } else if (pawV_is_int(v)) {
        *plossless = PAW_TRUE;
        return pawV_get_int(v);
    } else if (!pawV_is_bool(v)) {
        *plossless = PAW_FALSE;
        return 0;
    }
    *plossless = PAW_TRUE;
    return pawV_get_bool(v);
}

paw_Float pawV_to_float(Value v)
{
    if (pawV_is_float(v)) {
        return pawV_get_float(v);
    } else if (pawV_is_bool(v)) {
        return pawV_get_bool(v);
    } else if (pawV_is_int(v)) {
        return (paw_Float)pawV_get_int(v);
    } else if (!pawV_is_bigint(v)) {
        return 0.0;
    }
    return pawB_get_float(pawV_get_bigint(v));
}
