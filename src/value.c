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
#include "type.h"
#include "util.h"
#include "value.h"
#include <limits.h>
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

const char *pawV_to_string(paw_Env *P, Value v, paw_Type type, size_t *nout)
{
    switch (type) {
        case PAW_TSTRING:
            pawC_pushv(P, v); // copy
            break;
        case PAW_TBOOL:
            pawC_pushv(P, pawE_cstr(P, v_true(v) ? CSTR_TRUE : CSTR_FALSE));
            break;
        case PAW_TINT:
            int_to_string(P, v_int(v));
            break;
        case PAW_TFLOAT:
            float_to_string(P, v_float(v));
            break;
        default:
            return NULL;
    }
    const String *s = v_string(P->top.p[-1]);
    *nout = s->length;
    return s->text;
}

const char *pawV_name(ValueKind kind)
{
    switch (kind) {
        case VBOOL:
            return "bool";
        case VINT:
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
        case VFOREIGN:
            return "foreign";
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

Type *pawV_new_type(paw_Env *P)
{
    Type *t = pawM_new(P, Type);
    pawG_add_object(P, cast_object(t), VTYPE);
    return t;
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

Instance *pawV_new_instance(paw_Env *P, Class *cls, TypeTag type)
{
    const int nattrs = type->c.nattrs;
    Instance *ins = pawM_new_flex(P, Instance, cast_size(nattrs),
                                  sizeof(ins->attrs[0]));
    pawG_add_object(P, cast_object(ins), VINSTANCE);
    for (int i = 0; i < nattrs; ++i) {
        Attribute a = type->c.attrs[i];

        Value key;
        v_set_object(&key, a.name);
        Value *pv = pawH_get(P, cls->fields, key);
        if (pv == NULL) {
            pv = pawH_get(P, cls->methods, key);
            paw_assert(pv != NULL);
        }
        ins->attrs[i] = *pv;
    }
    return ins;
}

void pawV_free_instance(paw_Env *P, Instance *ins, TypeTag type)
{
    pawM_free_flex(P, ins, cast_size(type->c.nattrs), sizeof(ins->attrs[0]));
}

Value *pawV_find_attr(paw_Env *P, TypeTag type, Value *attrs, String *name)
{
    ClassType t = type->c;
    for (int i = t.nattrs - 1; i >= 0; --i) {
        Attribute *a = &t.attrs[i];
        if (pawS_eq(a->name, name)) {
            return &attrs[i];
        }
    }
    return NULL;
}

static void clear_attrs(Value *pv, int nattrs)
{
    memset(pv, 0, cast_size(nattrs) * sizeof(*pv));
}

Class *pawV_new_class(paw_Env *P)
{
    Class *c = pawM_new(P, Class);
    pawG_add_object(P, cast_object(c), VCLASS);
    c->fields = pawM_new(P, Map);
    c->methods = pawM_new(P, Map);
    return c;
}

void pawV_free_class(paw_Env *P, Class *c)
{
    pawH_free(P, c->fields);
    pawH_free(P, c->methods);
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

Native *pawV_new_native(paw_Env *P, String *name, paw_Function call)
{
    Native *nat = pawM_new(P, Native);
    pawG_add_object(P, cast_object(nat), VNATIVE);
    nat->name = name;
    nat->call = call;
    return nat;
}

void pawV_free_native(paw_Env *P, Native *nat)
{
    pawM_free(P, nat);
}

Foreign *pawV_push_foreign(paw_Env *P, size_t size, int nattrs)
{
    if (size > PAW_SIZE_MAX) {
        pawM_error(P);
    }
    Value *pv = pawC_push0(P);
    Foreign *ud = pawM_new_flex(P, Foreign, nattrs, sizeof(ud->attrs[0]));
    pawG_add_object(P, cast_object(ud), VFOREIGN);
    v_set_object(pv, ud); // anchor
    ud->size = size;
    if (size) {
        // Allocate space to hold 'size' bytes of foreign data.
        ud->data = pawM_new_vec(P, size, char);
    }
    clear_attrs(ud->attrs, nattrs);
    return ud;
}

void pawV_free_foreign(paw_Env *P, Foreign *ud, TypeTag type)
{
    pawM_free_vec(P, (char *)ud->data, ud->size);
    pawM_free_flex(P, ud, cast_size(type->c.nattrs), sizeof(ud->attrs[0]));
}

int pawV_num2int(Var *pv)
{
    if (t_is_int(pv->t)) {
        // already an int
    } else if (t_is_float(pv->t)) {
        v_set_int(&pv->v, paw_cast_int(v_float(pv->v)));
    } else if (t_is_bool(pv->t)) {
        v_set_int(&pv->v, v_true(pv->v));
    } else {
        return -1;
    }
    return 0;
}

int pawV_num2float(Var *pv)
{
    if (t_is_float(pv->t)) {
        // already a float
    } else if (t_is_int(pv->t)) {
        v_set_float(&pv->v, (paw_Float)v_int(pv->v));
    } else if (t_is_bool(pv->t)) {
        v_set_float(&pv->v, v_true(pv->v));
    } else {
        return -1;
    }
    return 0;
}

paw_Bool pawV_truthy(Value v, paw_Type type)
{
    switch (type) {
        case PAW_TBOOL:
        case PAW_TINT:
            return v_true(v);
        case PAW_TFLOAT:
            return v_float(v) != 0.0;
        case PAW_TSTRING:
            return pawS_length(v_string(v)) > 0;
        case PAW_TARRAY:
            return pawA_length(v_array(v)) > 0;
        case PAW_TMAP:
            return pawH_length(v_map(v)) > 0;
        default:
            return !v_is_null(v);
    }
}

// from https://gist.github.com/badboy/6267743
static uint32_t hash_u64(uint64_t u)
{
    u = ~u + (u << 18);
    u = u ^ (u >> 31);
    u = u * 21;
    u = u ^ (u >> 11);
    u = u + (u << 6);
    u = u ^ (u >> 22);
    return (uint32_t)u;
}

uint32_t pawV_hash(Var v)
{
    return pawV_hash_key(v.v);
}

uint32_t pawV_hash_key(Value v)
{
    return hash_u64(v.u);
}

void pawV_set_default(paw_Env *P, Value *pv, paw_Type type)
{
    switch (type) {
        case PAW_TBOOL:
            v_set_bool(pv, PAW_FALSE);
            break;
        case PAW_TINT:
            v_set_int(pv, 0);
            break;
        case PAW_TFLOAT:
            v_set_float(pv, 0.0);
            break;
        case PAW_TSTRING:
            v_set_object(pv, pawS_new_str(P, ""));
            break;
        case PAW_TARRAY:
            v_set_object(pv, pawA_new(P));
            break;
        case PAW_TMAP:
            v_set_object(pv, pawH_new(P));
            break;
        default:
            v_set_object(pv, NULL);
    }
}

// TODO: We always emit conversion operators when 'lhs' and 'rhs' types are not the same
//       for any binary operator. This only needs to take 1 TypeTag parameter.
paw_Bool pawV_equal(Var x, Var y)
{
    if (v_is_null(x.v) || v_is_null(y.v)) {
        return pawY_is_same(x.t, y.t);
    } else if (t_type(x.t) != t_type(y.t)) {
        if ((t_is_float(x.t) && 0 == pawV_num2float(&y)) ||
            (t_is_float(y.t) && 0 == pawV_num2float(&x))) {
            // catches 'float == int' and 'int == float'
            return v_float(x.v) == v_float(y.v);
//        } else if (t_is_bigint(x.t) || t_is_bigint(y.t)) {
//            paw_assert(0);
//            return pawB_equals(x, y);
        }
        // TODO: Some special cases
        return PAW_FALSE;
    } else if (t_is_string(x.t)) {
        return pawS_eq(v_string(x.v), v_string(y.v));
    } else if (t_is_float(x.t)) {
        return v_float(x.v) == v_float(y.v);
    } else if (t_is_int(x.t)) {
        return v_int(x.v) == v_int(y.v);
    } else if (t_is_bool(x.t)) {
        return v_true(x.v) == v_true(y.v);
    }
    // Fall back to pointer comparison.
    return x.v.u == y.v.u;
}

paw_Int pawV_length(Value v, paw_Type type)
{
    size_t len;
    switch (type) {
        case PAW_TSTRING:
            len = pawS_length(v_string(v));
            break;
        case PAW_TARRAY:
            len = pawA_length(v_array(v));
            break;
        case PAW_TMAP:
            len = pawH_length(v_map(v));
            break;
        default:
            len = 0;
    }
    return paw_cast_int(len);
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
        if (value > (PAW_INT_MAX - v) / base) {
            paw_assert(0);
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
