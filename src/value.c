// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"
#include <stdio.h>
#include <stdlib.h>

#include "gc.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "rt.h"
#include "str.h"
#include "value.h"

#define ERROR(P, kind, ...) pawE_error(P, kind, -1, __VA_ARGS__)

void pawV_index_error(paw_Env *P, paw_Int index, size_t length, char const *what)
{
    pawR_error(P, PAW_EINDEX,
               "index %I is out of bounds for %s of length %I",
               index, what, PAW_CAST_INT(length));
}

static int check_suffix(char const *p, char const *base)
{
    while (*p != '\0') {
        if (!ISSPACE(*p++))
            return -1;
    }
    // If one of the pawV_parse_* functions are called on a string like " ",
    // then all of the checks will pass, despite " " not being a valid number.
    // Make sure that doesn't happen.
    if (p == base)
        return -1;
    return 0;
}

static void int_to_string(paw_Env *P, paw_Int i, Value *out)
{
    char temp[32];
    paw_Bool const negative = i < 0;
    char *end = temp + PAW_COUNTOF(temp);
    char *ptr = end - 1;

    // Don't call llabs(INT64_MIN). The result is undefined on 2s complement
    // systems.
    uint64_t u = i == INT64_MIN
                     ? UINT64_C(1) << 63
                     : CAST(uint64_t, llabs(i));
    do {
        *ptr-- = CAST(char, u % 10 + '0');
        u /= 10;
    } while (u);
    if (negative) {
        *ptr = '-';
    } else {
        ++ptr;
    }
    String *str = pawS_new_nstr(P, ptr, CAST_SIZE(end - ptr));
    V_SET_OBJECT(out, str);
}

static void float_to_string(paw_Env *P, paw_Float f, Value *out)
{
    char temp[32];
    int const n = snprintf(temp, PAW_COUNTOF(temp), "%.*g", 17, f);
    String *str = pawS_new_nstr(P, temp, CAST_SIZE(n));
    V_SET_OBJECT(out, str);
}

char const *pawV_to_string(paw_Env *P, Value *pv, paw_Type type, size_t *plength)
{
    switch (type) {
        case PAW_TSTR:
            break;
        case PAW_TINT:
            int_to_string(P, V_INT(*pv), pv);
            break;
        case PAW_TFLOAT:
            float_to_string(P, V_FLOAT(*pv), pv);
            break;
        default:
            paw_assert(type == PAW_TBOOL);
            V_SET_OBJECT(pv, CACHED_STRING(P, V_TRUE(*pv) ? CSTR_TRUE : CSTR_FALSE));
    }
    String const *s = V_STRING(*pv);
    if (plength != NULL)
        *plength = s->length;
    return s->text;
}

Proto *pawV_new_proto(paw_Env *P)
{
    Proto *p = pawM_new(P, Proto);
    pawG_add_object(P, CAST_OBJECT(p), VPROTO);
    return p;
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
    if (prev != NULL)
        prev->open.next = next;
    if (next != NULL)
        next->open.prev = prev;
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
    if (t->kind == TUPLE_LIST) {
        pawList_free(P, t);
    } else if (t->kind == TUPLE_MAP) {
        pawMap_free(P, t);
    } else {
        paw_assert(t->kind == TUPLE_OTHER);
        pawM_free_flex(P, t, t->nelems, sizeof(t->elems[0]));
    }
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

Native *pawV_new_native(paw_Env *P, paw_Function func, int nup)
{
    paw_assert(nup <= UINT16_MAX);
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

Foreign *pawV_new_foreign(paw_Env *P, size_t size, int nfields, uint8_t flags, Value *out)
{
    if (size > PAW_SIZE_MAX)
        pawM_error(P);
    Foreign *f = pawM_new_flex(P, Foreign, nfields, sizeof(f->fields[0]));
    pawG_add_object(P, CAST_OBJECT(f), VFOREIGN);
    V_SET_OBJECT(out, f); // anchor
    f->nfields = nfields;
    f->flags = flags;
    f->size = size;
    if (size > 0) {
        // allocate space to hold 'size' bytes of foreign data
        f->data = pawM_new_vec(P, size, char);
    }
    memset(f->fields, 0, CAST_SIZE(nfields) * sizeof(f->fields[0]));
    return f;
}

void pawV_free_foreign(paw_Env *P, Foreign *f)
{
    if (f->flags == VBOX_FILE) {
        File *file = f->data;
        pawO_close(file);
    } else if (f->flags == VBOX_LOADER) {
        pawL_close_loader(P, f->data);
    }
    pawM_free_vec(P, f->data, f->size);
    pawM_free_flex(P, f, CAST_SIZE(f->nfields), sizeof(f->fields[0]));
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

#define is_fp(c) (c == 'e' || c == 'E' || c == '.')

#define SKIP_SPACES(p)    \
    while (ISSPACE(*(p))) \
        ++(p);

int pawV_parse_uint64(paw_Env *P, char const *text, int base, uint64_t *out)
{
    int b = 10;
    SKIP_SPACES(text);
    char const *p = text;
    if (p[0] == '0') {
        if ((b = char2base(p[1])) > 0) {
            if (base != 0 && b != base) {
                return PAW_EVALUE;
            }
            p += 2; // skip base prefix
        } else if (p[1] == '\0') {
            *out = 0;
            return PAW_OK;
        } else {
            return PAW_ESYNTAX;
        }
    }
    base = b;
    uint64_t value = 0;
    for (; ISHEX(*p); ++p) {
        unsigned const v = HEXVAL(*p);
        if (v >= CAST(unsigned, base)) {
            return PAW_ESYNTAX;
        } else if (value > (UINT64_MAX - v) / base) {
            return PAW_EOVERFLOW;
        }
        value = value * base + v;
    }
    if (check_suffix(p, text)) {
        return PAW_ESYNTAX;
    }
    *out = value;
    return PAW_OK;
}

static paw_Bool parse_negative(char const **ptext)
{
    if (**ptext == '-') {
        ++*ptext;
        return PAW_TRUE;
    }
    *ptext += **ptext == '+';
    return PAW_FALSE;
}

int pawV_parse_int(paw_Env *P, char const *text, int base, paw_Int *out)
{
    char const *original = text;
    SKIP_SPACES(text);
    paw_Bool const negative = parse_negative(&text);
    if (!ISHEX(*text))
        return PAW_ESYNTAX;

    uint64_t u;
    int const status = pawV_parse_uint64(P, text, base, &u);
    if (status != PAW_OK)
        return status;
    if (u > CAST(uint64_t, PAW_INT_MAX) + negative) {
        return PAW_EOVERFLOW;
    }
    *out = negative
               ? PAW_CAST_INT(-u)
               : PAW_CAST_INT(u);
    return PAW_OK;
}

#define SKIP_DIGITS(p)      \
    while (ISDIGIT(*(p))) { \
        ++(p);              \
    }

int pawV_parse_float(paw_Env *P, char const *text, paw_Float *out)
{
    char const *original = text;
    SKIP_SPACES(text);
    paw_Bool const negative = parse_negative(&text);

    // First, validate the number format.
    char const *p = text;
    if (p[0] == '0' && p[1] != '\0' && !is_fp(p[1])) {
        return PAW_ESYNTAX;
    }
    SKIP_DIGITS(p)

    if (*p == '.') {
        ++p;
        SKIP_DIGITS(p)
    }
    if (*p == 'e' || *p == 'E') {
        p += 1 + (p[1] == '+' || p[1] == '-');
        if (!ISDIGIT(*p))
            return PAW_ESYNTAX;
        SKIP_DIGITS(p)
    }
    if (check_suffix(p, text)) {
        return PAW_ESYNTAX;
    }
    paw_Float const f = strtod(text, NULL);
    *out = negative ? -f : f;
    return PAW_OK;
}
