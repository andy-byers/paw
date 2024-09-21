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
    if (buf->alloc > SIZE_MAX / 2) pawM_error(P);
    const size_t alloc = buf->alloc * 2;
    if (L_IS_BOXED(buf)) {
        Foreign *ud = V_FOREIGN(P->top.p[boxloc]);
        pawM_resize(P, buf->data, buf->alloc, alloc);
        ud->data = buf->data;
        ud->size = alloc;
    } else {
        // Add a new Foreign 'box' to contain the buffer. Prevents a memory leak
        // if an error is thrown before the buffer is freed.
        Value *pbox = &P->top.p[boxloc];
        Foreign *ud = pawV_new_foreign(P, alloc, 0, pbox);
        memcpy(ud->data, buf->stack, buf->size);
        buf->data = ud->data;
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
    paw_push_zero(P, 1); // placeholder for box
}

void pawL_discard_result(paw_Env *P, Buffer *buf)
{
    PAW_UNUSED(buf);
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

static void add_nstring(paw_Env *P, Buffer *buf, const char *str, size_t len, int boxloc)
{
    char *ptr = reserve_memory(P, buf, len, boxloc);
    memcpy(ptr, str, len);
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

void pawL_add_value(paw_Env *P, Buffer *buf, paw_Type type)
{
    size_t len;
    const char *str = pawV_to_string(P, &P->top.p[-1], type, &len);
    add_nstring(P, buf, str, len, -2);
    paw_pop(P, 1);
}

static void add_int(paw_Env *P, Buffer *buf, paw_Int i)
{
    size_t len;
    paw_push_int(P, i);
    const char *str = paw_to_string(P, -1, PAW_TINT, &len);
    add_nstring(P, buf, str, len, -2); // string above box
    paw_pop(P, 1);
}

static void add_float(paw_Env *P, Buffer *buf, paw_Float f)
{
    size_t len;
    paw_push_float(P, f);
    const char *str = paw_to_string(P, -1, PAW_TFLOAT, &len);
    add_nstring(P, buf, str, len, -2); // string above box
    paw_pop(P, 1);
}

static void add_pointer(paw_Env *P, Buffer *buf, void *p)
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
        if (*fmt == '\0') break;
        ++fmt; // skip '%'

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
                add_int(P, buf, va_arg(arg, unsigned));
                break;
            case 'd':
                add_int(P, buf, va_arg(arg, int));
                break;
            case 'I':
                add_int(P, buf, va_arg(arg, int64_t));
                break;
            case 'c':
                pawL_add_char(P, buf, va_arg(arg, int));
                break;
            case 'f':
                add_float(P, buf, va_arg(arg, double));
                break;
            case 'p':
                add_pointer(P, buf, va_arg(arg, void *));
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
