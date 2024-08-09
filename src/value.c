// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "gc.h"
#include "map.h"
#include "mem.h"
#include "rt.h"
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

    // Don't call llabs(INT64_MIN). The result is undefined on 2s complement
    // systems.
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
    pawC_pushns(P, ptr, CAST_SIZE(end - ptr));
}

static void float_to_string(paw_Env *P, paw_Float f)
{
    char temp[32];
    const int n = snprintf(temp, paw_countof(temp), "%.*g", 17, f);
    pawC_pushns(P, temp, CAST_SIZE(n));
}

const char *pawV_to_string(paw_Env *P, Value v, paw_Type type, size_t *nout)
{
    switch (type) {
        case PAW_TSTRING:
            pawC_pushv(P, v); // copy
            break;
        case PAW_TINT:
            int_to_string(P, v_int(v));
            break;
        case PAW_TFLOAT:
            float_to_string(P, v_float(v));
            break;
        case PAW_TBOOL:
            v_set_object(&v, pawE_cstr(P, v_true(v) ? CSTR_TRUE : CSTR_FALSE));
            pawC_pushv(P, v);
            break;
        default:
            return NULL;
    }
    const String *s = v_string(P->top.p[-1]);
    *nout = s->length;
    return s->text;
}

static paw_Int string_to_int(paw_Env *P, String *s)
{
    return 0; // TODO
}

static paw_Int float_to_int(paw_Env *P, paw_Float f)
{
    return 0; // TODO
}

paw_Int pawV_to_int(paw_Env *P, Value v, paw_Type type)
{
    switch (type) {
        case PAW_TBOOL:
            return v_true(v) ? 1 : 0;
        case PAW_TFLOAT:
            return float_to_int(P, v_float(v));
        case PAW_TSTRING:
            return string_to_int(P, v_string(v));
        default:
            return v_int(v);
    }
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
        case VVECTOR:
            return "vector";
        case VMAP:
            return "map";
        case VSTRUCT:
            return "struct";
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
    pawG_add_object(P, CAST_OBJECT(p), VPROTO);
    return p;
}

Type *pawV_new_type(paw_Env *P)
{
    Type *t = pawM_new(P, Type);
    pawG_add_object(P, CAST_OBJECT(t), VTYPE);
    return t;
}

void pawV_free_proto(paw_Env *P, Proto *f)
{
    pawM_free_vec(P, f->source, f->length);
    pawM_free_vec(P, f->lines, f->nlines);
    pawM_free_vec(P, f->p, f->nproto);
    pawM_free_vec(P, f->k, f->nk);
    pawM_free_vec(P, f->u, f->nup);
    pawM_free(P, f);
}

UpValue *pawV_new_upvalue(paw_Env *P)
{
    UpValue *u = pawM_new(P, UpValue);
    pawG_add_object(P, CAST_OBJECT(u), VUPVALUE);
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

Tuple *pawV_new_tuple(paw_Env *P, int nelems)
{
    Tuple *tuple = pawM_new_flex(P, Tuple, CAST_SIZE(nelems), sizeof(tuple->elems[0]));
    pawG_add_object(P, CAST_OBJECT(tuple), VTUPLE);
    tuple->nelems = nelems;
    return tuple;
}

void pawV_free_tuple(paw_Env *P, Tuple *t)
{
    pawM_free_flex(P, t, t->nelems, sizeof(t->elems[0]));
}

Closure *pawV_new_closure(paw_Env *P, int nup)
{
    // Tack on enough space to store 'nup' pointers to UpValue.
    Closure *f = pawM_new_flex(P, Closure, nup, sizeof(f->up[0]));
    pawG_add_object(P, CAST_OBJECT(f), VCLOSURE);
    memset(f->up, 0, CAST_SIZE(nup) * sizeof(f->up[0]));
    f->nup = nup;
    return f;
}

void pawV_free_closure(paw_Env *P, Closure *f)
{
    pawM_free_flex(P, f, f->nup, sizeof(f->up[0]));
}

Instance *pawV_new_instance(paw_Env *P, int nfields)
{
    Instance *ins = pawM_new_flex(P, Instance, CAST_SIZE(nfields), sizeof(ins->attrs[0]));
    pawG_add_object(P, CAST_OBJECT(ins), VINSTANCE);
    ins->nfields = nfields;
    return ins;
}

void pawV_free_instance(paw_Env *P, Instance *ins)
{
    pawM_free_flex(P, ins, CAST_SIZE(ins->nfields), sizeof(ins->attrs[0]));
}

Variant *pawV_new_variant(paw_Env *P, int k, int nfields)
{
    Variant *var = pawM_new_flex(P, Variant, CAST_SIZE(nfields), sizeof(var->fields[0]));
    pawG_add_object(P, CAST_OBJECT(var), VVARIANT);
    var->nfields = nfields;
    var->k = k;
    return var;
}

void pawV_free_variant(paw_Env *P, Variant *var)
{
    pawM_free_flex(P, var, CAST_SIZE(var->nfields), sizeof(var->fields[0]));
}

Value *pawV_find_attr(Value *attrs, String *name, Type *type)
{
    //    const CompositeType *cls = &type->cls;
    //    for (int i = 0; i < cls->nattrs; ++i) {
    //        NamedField *a = &cls->attrs[i];
    //        if (pawS_eq(a->name, name)) {
    //            return &attrs[i];
    //        }
    //    }
    return NULL;
}

static void clear_attrs(Value *pv, int nattrs)
{
    memset(pv, 0, CAST_SIZE(nattrs) * sizeof(*pv));
}

Method *pawV_new_method(paw_Env *P, Value self, Value call)
{
    Method *mtd = pawM_new(P, Method);
    pawG_add_object(P, CAST_OBJECT(mtd), VMETHOD);
    mtd->self = self;
    mtd->f = call;
    return mtd;
}

void pawV_free_method(paw_Env *P, Method *m)
{
    pawM_free(P, m); 
}

Native *pawV_new_native(paw_Env *P, paw_Function func, int nup)
{
    // TODO: nup > UINT16_MAX, check it or assert?
    Native *nat = pawM_new_flex(P, Native, nup, sizeof(nat->up[0]));
    pawG_add_object(P, CAST_OBJECT(nat), VNATIVE);
    nat->func = func;
    nat->nup = nup;
    return nat;
}

void pawV_free_native(paw_Env *P, Native *f) 
{
    pawM_free_flex(P, f, f->nup, sizeof(f->up[0])); 
}

Foreign *pawV_push_foreign(paw_Env *P, size_t size, int nfields)
{
    if (size > PAW_SIZE_MAX) {
        pawM_error(P);
    }
    Value *pv = pawC_push0(P);
    Foreign *ud = pawM_new_flex(P, Foreign, nfields, sizeof(ud->attrs[0]));
    pawG_add_object(P, CAST_OBJECT(ud), VFOREIGN);
    v_set_object(pv, ud); // anchor
    ud->nfields = nfields;
    ud->size = size;
    if (size > 0) {
        // Allocate space to hold 'size' bytes of foreign data.
        ud->data = pawM_new_vec(P, size, char);
    }
    clear_attrs(ud->attrs, nfields);
    return ud;
}

void pawV_free_foreign(paw_Env *P, Foreign *ud)
{
    pawM_free_vec(P, (char *)ud->data, ud->size); // TODO
    pawM_free_flex(P, ud, CAST_SIZE(ud->nfields), sizeof(ud->attrs[0]));
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
            //        case PAW_TARRAY:
            //            return pawA_length(v_vector(v)) > 0;
            //        case PAW_TMAP:
            //            return pawH_length(v_map(v)) > 0;
        default:
            return PAW_FALSE;
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

uint32_t pawV_hash(Value v) { return hash_u64(v.u); }

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
        case PAW_TVECTOR:
            v_set_object(pv, pawV_vec_new(P));
            break;
        case PAW_TMAP:
            v_set_object(pv, pawH_new(P));
            break;
        default:
            v_set_object(pv, NULL);
    }
}

paw_Int pawV_length(Value v, paw_Type type)
{
    size_t len;
    switch (type) {
        case PAW_TSTRING:
            len = pawS_length(v_string(v));
            break;
        case PAW_TVECTOR:
            len = pawV_vec_length(v_vector(v));
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

int pawV_parse_uint64(paw_Env *P, const char *text)
{
    int rc;
    unsigned base = 10;
    const char *p = text;
    if (p[0] == '0') {
        if ((rc = char2base(p[1])) > 0) {
            p += 2; // skip base prefix
            base = CAST(rc, unsigned);
        } else if (p[1] == '\0') {
            pawC_pushi(P, 0);
            return PAW_OK;
        } else {
            return PAW_ESYNTAX;
        }
    }
    uint64_t value = 0;
    for (; ISHEX(*p); ++p) {
        const unsigned v = HEXVAL(*p);
        if (v >= base) {
            return PAW_ESYNTAX;
        } else if (value > (UINT64_MAX - v) / base) {
            return PAW_EOVERFLOW;
        }
        value = value * base + v;
    }
    if (check_suffix(p, text)) {
        return PAW_ESYNTAX;
    }
    Value *pv = pawC_push0(P);
    pv->u = value;
    return PAW_OK;
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
        return PAW_ESYNTAX;
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
        return PAW_ESYNTAX;
    }
    pawC_pushf(P, strtod(text, NULL));
    return PAW_OK;
}
