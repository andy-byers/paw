// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "array.h"
#include "bigint.h"
#include "error.h"
#include "gc.h"
#include "map.h"
#include "mem.h"
#include "str.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

void pawV_int_error(paw_Env *P, Value x)
{
    Buffer buf;
    pawL_init_buffer(&buf);
    pawL_add_string(P, &buf, "integer ");
    pawL_add_value(P, &buf, x);
    pawL_add_string(P, &buf, " is too large");
    pawL_push_result(P, &buf);
    pawC_throw(P, PAW_ERANGE);
}

void pawV_type_error(paw_Env *P, Value x)
{
    pawE_error(P, PAW_ETYPE, "unsupported operand type: '%s'",
               api_typename(api_type(x)));
}

void pawV_type_error2(paw_Env *P, Value x, Value y)
{
    pawE_error(P, PAW_ETYPE, "unsupported operand types: '%s' and '%s'",
               api_typename(api_type(x)), api_typename(api_type(y)));
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
    if (UPV_IS_OPEN(u)) {
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
    Closure *f = pawM_new_fa(P, Closure, cast_size(nup) * sizeof(f->up[0]));
    pawG_add_object(P, cast_object(f), VCLOSURE);
    f->nup = nup;
    return f;
}

void pawV_free_closure(paw_Env *P, Closure *f)
{
    pawM_free_fa(P, f, f->nup * sizeof(f->up[0]));
}

Native *pawV_new_native(paw_Env *P, paw_Function f, int nup)
{
    Native *nt = pawM_new_fa(P, Native, cast_size(nup) * sizeof(nt->up[0]));
    pawG_add_object(P, cast_object(nt), VNATIVE);
    nt->nup = nup;
    nt->f = f;
    return nt;
}

void pawV_free_native(paw_Env *P, Native *nt)
{
    pawM_free_fa(P, nt, nt->nup * sizeof(nt->up[0]));
}

Instance *pawV_new_instance(paw_Env *P, StackPtr sp, Class *cls)
{
    Instance *ins = pawM_new(P, Instance);
    pawG_add_object(P, cast_object(ins), VINSTANCE);
    pawV_set_instance(sp, ins); // Anchor
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
    pawV_set_class(sp, cls); // Anchor
    cls->attr = pawH_new(P);
    return cls;
}

void pawV_free_class(paw_Env *P, Class *c)
{
    // Members are managed separately
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
    pawV_set_userdata(sp, o); // Anchor
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
        return 0; // Already an integer
    } else if (pawV_is_float(*pv)) {
        pawV_set_int(pv, paw_cast_int(pawV_get_float(*pv)));
        return 0;
    } else if (pawV_is_bool(*pv)) {
        pawV_set_int(pv, pawV_get_bool(*pv));
        return 0;
    }
    return -1;
}

int pawV_num2float(Value *pv)
{
    if (pawV_is_float(*pv)) {
        return 0; // Already a float
    } else if (pawV_is_int(*pv)) {
        pawV_set_float(pv, (paw_Float)pawV_get_int(*pv));
        return 0;
    } else if (pawV_is_bigint(*pv)) {
        pawV_set_float(pv, pawB_get_float(pawV_get_bigint(*pv)));
        return 0;
    } else if (pawV_is_bool(*pv)) {
        pawV_set_float(pv, pawV_get_bool(*pv));
        return 0;
    }
    return -1;
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
    return 0;
}

uint32_t pawV_hash(const Value v)
{
    if (pawV_is_string(v)) {
        return pawV_get_string(v)->hash;
    }
    // From https://gist.github.com/badboy/6267743
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
    } else if (pawV_is_bigint(x) || pawV_is_bigint(y)) {
        return pawB_cmp(OP_EQ, x, y);
    } else if ((pawV_is_float(x) && pawV_is_int(y))) {
        return pawV_get_float(x) == pawV_get_int(y);
    } else if ((pawV_is_int(x) && pawV_is_float(y))) {
        return pawV_get_int(x) == pawV_get_float(y);
    }
    // Fall back to value (type + pointer) comparison
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

int pawV_parse_integer(paw_Env *P, const char *text, Value *pv)
{
    int base = 10;
    if (text[0] == '0') {
        if (is_fp(text[1])) {
            return -1; // Maybe float
        } else if ((base = char2base(text[1])) > 0) {
            text += 2; // Non-decimal integer
        } else if (text[1]) {
            return -1; // Junk after '0'
        } else {
            pawV_set_int(pv, 0); // Exactly 0
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
            pawB_parse(P, text, base);
            *pv = P->top[-1];
            pawC_stkpop(P);
            return 0;
        }
        value = value * base + v;
    }
    if (check_suffix(p, text)) {
        return -1;
    }
    pawV_set_int(pv, value);
    return 0;
}

#define skip_digits(p)      \
    while (ISDIGIT(*(p))) { \
        ++(p);              \
    }

int pawV_parse_float(const char *text, Value *pv)
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
    pawV_set_float(pv, strtod(text, NULL));
    return 0;
}

paw_Int pawV_to_int64(paw_Env *P, Value v, paw_Bool *plossless)
{
    if (pawV_is_bigint(v)) {
        return pawB_get_int64(pawV_get_bigint(v), plossless);
    } else if (pawV_is_float(v)) {
        *plossless = PAW_BFALSE;
        return paw_cast_int(pawV_get_float(v));
    } else if (pawV_is_int(v)) {
        *plossless = PAW_BTRUE;
        return pawV_get_int(v);
    } else if (!pawV_is_bool(v)) {
        pawV_type_error(P, v);
    }
    *plossless = PAW_BTRUE;
    return pawV_get_bool(v);
}

paw_Float pawV_to_float(paw_Env *P, Value v)
{
    if (pawV_is_float(v)) {
        return pawV_get_float(v);
    } else if (pawV_is_bool(v)) {
        return pawV_get_bool(v);
    } else if (pawV_is_int(v)) {
        return (paw_Float)pawV_get_int(v);
    } else if (!pawV_is_bigint(v)) {
        pawV_type_error(P, v);
    }
    return pawB_get_float(pawV_get_bigint(v));
}
