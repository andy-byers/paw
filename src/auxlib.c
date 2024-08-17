// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "auxlib.h"
#include "map.h"
#include "mem.h"
#include "rt.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

static void grow_buffer(paw_Env *P, Buffer *buf, int boxloc)
{
    paw_assert(buf->alloc <= SIZE_MAX / 2);
    const size_t alloc = buf->alloc * 2;
    if (pawL_boxed(buf)) {
        Foreign *ud = V_FOREIGN(P->top.p[boxloc]);
        pawM_resize(P, buf->data, buf->alloc, alloc);
        ud->data = buf->data;
        ud->size = alloc;
    } else {
        // Add a new Foreign 'box' to contain the buffer. Prevents a memory leak
        // if an error is thrown before the buffer is freed.
        Foreign *ud = pawV_push_foreign(P, alloc, 0);
        memcpy(ud->data, buf->stack, buf->size);
        buf->data = ud->data;
        Value *pbox = &P->top.p[boxloc - 1];
        V_SET_OBJECT(pbox, ud);
        paw_pop(P, 1);
    }
    buf->alloc = alloc;
}

static char *reserve_memory(paw_Env *P, Buffer *buf, size_t n, int boxloc)
{
    while (buf->size + n > buf->alloc) {
        grow_buffer(P, buf, boxloc);
    }
    char *ptr = buf->data + buf->size;
    buf->size += n;
    return ptr;
}

void pawL_init_buffer(paw_Env *P, Buffer *buf)
{
    *buf = (Buffer){.data = buf->stack, .alloc = BUFFER_LIMIT};
    paw_push_unit(P, 1); // placeholder for box
}

void pawL_discard_result(paw_Env *P, Buffer *buf)
{
    paw_unused(buf);
    paw_pop(P, 1); // pop box or placeholder
}

void pawL_push_result(paw_Env *P, Buffer *buf)
{
    paw_push_nstring(P, buf->data, buf->size);
    paw_replace(P, -2); // replace box/placeholder with string
}

void pawL_buffer_resize(paw_Env *P, Buffer *buf, size_t n)
{
    while (n > buf->alloc) {
        grow_buffer(P, buf, -1);
    }
    buf->size = n;
}

static void add_nstring(paw_Env *P, Buffer *buf, const char *str, size_t len,
                        int boxloc)
{
    char *ptr = reserve_memory(P, buf, len, boxloc);
    memcpy(ptr, str, len);
}

void pawL_add_value(paw_Env *P, Buffer *buf, paw_Type type)
{
    size_t len; // value must be on top of the stack
    const char *str = pawV_to_string(P, P->top.p[-1], type, &len);
    if (str == NULL) {
        // add the type name and address
        str = paw_push_fstring(P, "%s (%p)",
                               "<type>" /*TODO: paw_typename(P, -1)*/,
                               paw_pointer(P, -1));
        len = paw_length(P, -1);
    }
    add_nstring(P, buf, str, len, -3);
    pawC_stkdec(P, 2); // pop value and string
}

// Table and stringify algorithm modified from micropython
static const uint8_t kLogBase2[] = {
    0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5,
};

size_t pawL_integer_format_size(size_t nbits, int base)
{
    assert(2 <= base && base <= 32);
    return nbits / kLogBase2[base - 1] + 1;
}

void pawL_add_char(paw_Env *P, Buffer *buf, char c)
{
    char *ptr = reserve_memory(P, buf, 1, -1);
    ptr[0] = c;
}

void pawL_add_string(paw_Env *P, Buffer *buf, const char *s)
{
    pawL_add_nstring(P, buf, s, strlen(s));
}

void pawL_add_nstring(paw_Env *P, Buffer *buf, const char *s, size_t n)
{
    add_nstring(P, buf, s, n, -1);
}

void pawL_add_integer(paw_Env *P, Buffer *buf, paw_Int i)
{
    Value v;
    V_SET_INT(&v, i);

    size_t len;
    const char *str = pawV_to_string(P, v, PAW_TINT, &len);
    add_nstring(P, buf, str, len, -2); // string above box
    pawC_stkdec(P, 1);
}

void pawL_add_float(paw_Env *P, Buffer *buf, paw_Float f)
{
    Value v;
    V_SET_FLOAT(&v, f);

    size_t len;
    const char *str = pawV_to_string(P, v, PAW_TFLOAT, &len);
    add_nstring(P, buf, str, len, -2); // string above box
    pawC_stkdec(P, 1);
}

void pawL_add_pointer(paw_Env *P, Buffer *buf, void *p)
{
    char temp[32];
    const int n = snprintf(temp, sizeof(temp), "%p", p);
    pawL_add_nstring(P, buf, temp, CAST_SIZE(n));
}

static const char *add_non_fmt(paw_Env *P, Buffer *buf, const char *ptr)
{
    const char *p = ptr;
    while (*p && *p != '%') {
        ++p;
    }
    if (p != ptr) {
        pawL_add_nstring(P, buf, ptr, CAST_SIZE(p - ptr));
        ptr = p;
    }
    return ptr;
}

void pawL_add_vfstring(paw_Env *P, Buffer *buf, const char *fmt, va_list arg)
{
    for (;; ++fmt) {
        fmt = add_non_fmt(P, buf, fmt);
        if (!*fmt) {
            break;
        }
        // Skip '%'.
        ++fmt;

        switch (*fmt) {
            case 's': {
                const char *s = va_arg(arg, char *);
                pawL_add_nstring(P, buf, s, strlen(s));
                break;
            }
            case '%':
                pawL_add_char(P, buf, '%');
                break;
            case 'u':
                pawL_add_integer(P, buf, va_arg(arg, unsigned));
                break;
            case 'd':
                pawL_add_integer(P, buf, va_arg(arg, int));
                break;
            case 'I':
                pawL_add_integer(P, buf, va_arg(arg, int64_t));
                break;
            case 'c':
                pawL_add_char(P, buf, va_arg(arg, int));
                break;
            case 'f':
                pawL_add_float(P, buf, va_arg(arg, double));
                break;
            case 'p':
                pawL_add_pointer(P, buf, va_arg(arg, void *));
                break;
            default:
                pawR_error(P, PAW_EVALUE, "invalid format option '%%%c'", *fmt);
        }
    }
}

void pawL_add_fstring(paw_Env *P, Buffer *buf, const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, buf, fmt, arg);
    va_end(arg);
}
