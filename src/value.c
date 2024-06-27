// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "gc_aux.h"
#include "map.h"
#include "mem.h"
#include "rt.h"
#include "str.h"
#include "type.h"
#include "util.h"
#include "value.h"
#include "vector.h"
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
        case PAW_TINT:
            int_to_string(P, v_int(v));
            break;
        case PAW_TFLOAT:
            float_to_string(P, v_float(v));
            break;
        case PAW_TBOOL: {
            Value v;
            v_set_object(&v, pawE_cstr(P, v_true(v) ? CSTR_TRUE : CSTR_FALSE));
            pawC_pushv(P, v);
            break;
        }
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

Tuple *pawV_new_tuple(paw_Env *P, int nelems)
{
    Tuple *t = pawM_new_flex(P, Tuple, cast_size(nelems), sizeof(t->elems[0]));
    pawG_add_object(P, cast_object(t), 0 /* TODO: VTUPLE*/);
    return t;
}

void pawV_free_tuple(paw_Env *P, Tuple *t, int nelems)
{
    pawM_free_flex(P, t, nelems, sizeof(t->elems[0]));
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

Struct *pawV_new_struct(paw_Env *P, Value *pv)
{
    Struct *struct_ = pawM_new(P, Struct);
    v_set_object(pv, struct_); // anchor
    pawG_add_object(P, cast_object(struct_), VSTRUCT);
    return struct_;
}

void pawV_free_struct(paw_Env *P, Struct *struct_) { pawM_free(P, struct_); }

Instance *pawV_new_instance(paw_Env *P, int nfields)
{
    Instance *ins =
        pawM_new_flex(P, Instance, cast_size(nfields), sizeof(ins->attrs[0]));
    pawG_add_object(P, cast_object(ins), VINSTANCE);
    return ins;
}

void pawV_free_instance(paw_Env *P, Instance *ins, int nfields)
{
    pawM_free_flex(P, ins, cast_size(nfields), sizeof(ins->attrs[0]));
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
    memset(pv, 0, cast_size(nattrs) * sizeof(*pv));
}

Method *pawV_new_method(paw_Env *P, Value self, Value call)
{
    Method *mtd = pawM_new(P, Method);
    pawG_add_object(P, cast_object(mtd), VMETHOD);
    mtd->self = self;
    mtd->f = call;
    return mtd;
}

void pawV_free_method(paw_Env *P, Method *m) { pawM_free(P, m); }

Native *pawV_new_native(paw_Env *P, paw_Function func, int nup)
{
    // TODO: nup > UINT16_MAX, check it or assert?
    Native *nat = pawM_new_flex(P, Native, nup, sizeof(nat->up[0]));
    pawG_add_object(P, cast_object(nat), VNATIVE);
    nat->func = func;
    nat->nup = nup;
    return nat;
}

void pawV_free_native(paw_Env *P, Native *nat) { pawM_free(P, nat); }

Foreign *pawV_push_foreign(paw_Env *P, size_t size, int nfields)
{
    if (size > PAW_SIZE_MAX) {
        pawM_error(P);
    }
    Value *pv = pawC_push0(P);
    Foreign *ud = pawM_new_flex(P, Foreign, nfields, sizeof(ud->attrs[0]));
    pawG_add_object(P, cast_object(ud), VFOREIGN);
    v_set_object(pv, ud); // anchor
    ud->size = size;
    if (size > 0) {
        // Allocate space to hold 'size' bytes of foreign data.
        ud->data = pawM_new_vec(P, size, char);
    }
    clear_attrs(ud->attrs, nfields);
    return ud;
}

void pawV_free_foreign(paw_Env *P, Foreign *ud, int nfields)
{
    pawM_free_vec(P, (char *)ud->data, ud->size); // TODO
    pawM_free_flex(P, ud, cast_size(nfields), sizeof(ud->attrs[0]));
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
            //        case PAW_TARRAY:
            //            v_set_object(pv, pawA_new(P));
            //            break;
            //        case PAW_TMAP:
            //            v_set_object(pv, pawH_new(P));
            //            break;
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
            //        case PAW_TARRAY:
            //            len = pawA_length(v_vector(v));
            //            break;
            //        case PAW_TMAP:
            //            len = pawH_length(v_map(v));
            //            break;
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
        // TODO: Need to be able to parse exactly PAW_INT_MIN (which cannot be
        //       represented as a positive integer under 2s complement), handle
        //       as a special case. No more big integers.
        if (value > (PAW_INT_MAX - v) / base) {
            paw_assert(0);
            // Integer is too large: parse it as a BigInt. Throws an error on
            // allocation failure.
            //            return pawB_parse(P, text, base);
        }
        value = value * base + v;
    }
    if (check_suffix(p, text)) {
        return -1;
    }
    pawC_pushi(P, value);
    return 0;
}

#define skip_digits(p)                                                         \
    while (ISDIGIT(*(p))) {                                                    \
        ++(p);                                                                 \
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

static inline Value *pawV_vec_get(paw_Env *P, Vector *a, paw_Int index)
{
    const paw_Int abs = pawV_abs_index(index, cast_size(a->end - a->begin));
    const size_t i = pawV_check_abs(P, abs, pawV_vec_length(a));
    return &a->begin[i];
}

static inline paw_Bool pawV_vec_iter(const Vector *a, paw_Int *itr)
{
    return ++*itr < paw_cast_int(pawV_vec_length(a));
}

void pawV_index_error(paw_Env *P, paw_Int index, size_t length)
{
    pawR_error(P, PAW_EINDEX,
               "index %I is out of bounds for container of length %I", index,
               paw_cast_int(length));
}

static size_t array_capacity(const Vector *a)
{
    return cast_size(a->upper - a->begin);
}

static void realloc_array(paw_Env *P, Vector *a, size_t alloc0, size_t alloc)
{
    const size_t end = pawV_vec_length(a);
    pawM_resize(P, a->begin, alloc0, alloc);
    a->end = a->begin + end;
    a->upper = a->begin + alloc;
    check_gc(P);
}

static void ensure_space(paw_Env *P, Vector *a, size_t have, size_t want)
{
    if (want > PAW_SIZE_MAX / sizeof(Value)) {
        pawM_error(P);
    }
    // Use the next power-of-2.
    size_t n = 1;
    while (n < want) {
        n *= 2;
    }
    realloc_array(P, a, have, n);
}

static void reserve_extra(paw_Env *P, Vector *a, size_t extra)
{
    paw_assert(extra > 0);
    if (extra <= cast_size(a->upper - a->end)) {
        return; // Still have enough space
    }
    const size_t have = array_capacity(a);
    ensure_space(P, a, have, have + extra);
}

static void move_items(Value *src, ptrdiff_t shift, size_t count)
{
    memmove(src + shift, src, cast_size(count) * sizeof(src[0]));
}

static void vec_reserve(paw_Env *P, Vector *a, size_t want)
{
    const size_t have = array_capacity(a);
    if (want <= have) {
        return;
    }
    ensure_space(P, a, have, want);
}

void pawV_vec_push(paw_Env *P, Vector *a, Value v)
{
    reserve_extra(P, a, 1);
    *a->end++ = v;
}

void pawV_vec_resize(paw_Env *P, Vector *a, size_t length)
{
    const size_t n = pawV_vec_length(a);
    if (length > n) {
        // new items are uninitialized
        vec_reserve(P, a, length);
    } else if (length == 0) {
        a->end = a->begin;
        return;
    }
    a->end = a->begin + length;
}

void pawV_vec_insert(paw_Env *P, Vector *a, paw_Int index, Value v)
{
    // Clamp to the vector bounds.
    const size_t len = pawV_vec_length(a);
    const paw_Int abs = pawV_abs_index(index, len);
    const size_t i = paw_clamp(cast_size(abs), 0, len);

    reserve_extra(P, a, 1);
    if (i != len) {
        move_items(a->begin + abs, 1, len - i);
    }
    a->begin[abs] = v;
    ++a->end;
}

void pawV_vec_pop(paw_Env *P, Vector *a, paw_Int index)
{
    const size_t len = pawV_vec_length(a);
    const paw_Int fixed = pawV_abs_index(index, len);
    const size_t abs = pawV_check_abs(P, fixed, len);
    if (abs != len - 1) {
        // Shift values into place
        move_items(a->begin + abs + 1, -1, len - abs - 1);
    }
    --a->end;
}

Vector *pawV_vec_new(paw_Env *P)
{
    Vector *a = pawM_new(P, Vector);
    pawG_add_object(P, cast_object(a), VVECTOR);
    return a;
}

void pawV_vec_free(paw_Env *P, Vector *a)
{
    pawM_free_vec(P, a->begin, array_capacity(a));
    pawM_free(P, a);
}

Vector *pawV_vec_clone(paw_Env *P, StackPtr sp, const Vector *a)
{
    Vector *a2 = pawV_vec_new(P);
    v_set_object(sp, a2); // anchor
    if (pawV_vec_length(a)) {
        pawV_vec_resize(P, a2, pawV_vec_length(a));
        memcpy(a2->begin, a->begin, sizeof(a->begin[0]) * pawV_vec_length(a));
    }
    return a2;
}

static paw_Bool elems_equal(paw_Env *P, Value x, Value y)
{
    StackPtr p = pawC_stkinc(P, 2);
    p[0] = y;
    p[1] = x;

    // TODO: Need the type of value
    //    // Arrays can contain any type of value. Call pawR_binop() to check
    //    // metamethods on objects.
    //    pawR_binop(P, BINARY_EQ);

    const paw_Bool b = paw_bool(P, -1);
    paw_pop(P, 1);
    return b;
}

paw_Bool pawV_vec_equals(paw_Env *P, const Vector *lhs, const Vector *rhs)
{
    const size_t len = pawV_vec_length(lhs);
    if (len != pawV_vec_length(rhs)) {
        return PAW_FALSE;
    }
    for (size_t i = 0; i < len; ++i) {
        if (!elems_equal(P, lhs->begin[i], rhs->begin[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

paw_Bool pawV_vec_contains(paw_Env *P, const Vector *a, const Value v)
{
    for (size_t i = 0; i < pawV_vec_length(a); ++i) {
        if (elems_equal(P, v, a->begin[i])) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}
