// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "auxlib.h"
#include "compile.h"
#include "map.h"
#include "mem.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

static void grow_buffer(paw_Env *P, Buffer *buf)
{
    if (buf->alloc > SIZE_MAX / 2)
        pawM_error(P);
    size_t const alloc = buf->alloc * 2;
    void *data = pawP_alloc(P->pool, NULL, 0, alloc);
    memcpy(data, buf->data, buf->size);
    buf->alloc = alloc;
    buf->data = data;
}

static char *reserve_memory(paw_Env *P, Buffer *buf, size_t n)
{
    while (buf->size + n > buf->alloc) {
        grow_buffer(P, buf);
    }
    char *ptr = buf->data + buf->size;
    buf->size += n;
    return ptr;
}

void pawL_init_buffer(paw_Env *P, Buffer *buf)
{
    PAW_UNUSED(P);
    *buf = (Buffer){.data = buf->stack, .alloc = BUFFER_LIMIT};
}

Str *pawL_buffer_finish(paw_Env *P, Buffer *buf)
{
    return pawP_scan_nstr(P->C, buf->data, buf->size);
}

void pawL_buffer_resize(paw_Env *P, Buffer *buf, size_t n)
{
    while (n > buf->alloc) {
        grow_buffer(P, buf);
    }
    buf->size = n;
}

static void add_nstring(paw_Env *P, Buffer *buf, char const *str, size_t len)
{
    char *ptr = reserve_memory(P, buf, len);
    memcpy(ptr, str, len);
}

void pawL_add_char(paw_Env *P, Buffer *buf, char c)
{
    char *ptr = reserve_memory(P, buf, 1);
    ptr[0] = c;
}

void pawL_add_string(paw_Env *P, Buffer *buf, char const *s)
{
    pawL_add_nstring(P, buf, s, strlen(s));
}

void pawL_add_nstring(paw_Env *P, Buffer *buf, char const *s, size_t n)
{
    add_nstring(P, buf, s, n);
}

void pawL_add_int(paw_Env *P, Buffer *buf, paw_Int i)
{
    size_t len;
    char const *str = pawV_to_str(P, &I2V(i), PAW_TINT, &len);
    add_nstring(P, buf, str, len);
}

void pawL_add_float(paw_Env *P, Buffer *buf, paw_Float f)
{
    size_t len;
    char const *str = pawV_to_str(P, &F2V(f), PAW_TFLOAT, &len);
    add_nstring(P, buf, str, len);
}

static void add_pointer(paw_Env *P, Buffer *buf, void *p)
{
    char temp[32];
    int const n = snprintf(temp, sizeof(temp), "%p", p);
    pawL_add_nstring(P, buf, temp, CAST_SIZE(n));
}

static void add_hex(paw_Env *P, Buffer *buf, unsigned u, paw_Bool capitalize)
{
    char const *letters = capitalize
        ? "0123456789ABCDEF"
        : "0123456789abcdef";
    unsigned const BASE = 16;

    char temp[32];
    char *end = temp + BASE;
    char *ptr = end - 1;

    do {
        *ptr-- = letters[u % BASE];
        u /= BASE;
    } while (u > 0);

    pawL_add_nstring(P, buf, ptr + 1, CAST_SIZE(end - ptr - 1));
}

static char const *add_non_fmt(paw_Env *P, Buffer *buf, char const *ptr)
{
    char const *p = ptr;
    while (*p && *p != '%')
        ++p;

    if (p != ptr) {
        pawL_add_nstring(P, buf, ptr, CAST_SIZE(p - ptr));
        ptr = p;
    }
    return ptr;
}

void pawL_add_vfstring(paw_Env *P, Buffer *buf, char const *fmt, va_list arg)
{
    for (;; ++fmt) {
        fmt = add_non_fmt(P, buf, fmt);
        if (*fmt == '\0')
            break;
        ++fmt; // skip '%'

        switch (*fmt) {
            case 's': {
                char const *s = va_arg(arg, char *);
                pawL_add_nstring(P, buf, s, strlen(s));
                break;
            }
            case '%':
                pawL_add_char(P, buf, '%');
                break;
            case 'u':
                pawL_add_int(P, buf, va_arg(arg, unsigned));
                break;
            case 'd':
                pawL_add_int(P, buf, va_arg(arg, int));
                break;
            case 'I':
                pawL_add_int(P, buf, va_arg(arg, int64_t));
                break;
            case 'x':
            case 'X':
                add_hex(P, buf, va_arg(arg, unsigned), *fmt == 'X');
                break;
            case 'c':
                pawL_add_char(P, buf, va_arg(arg, int));
                break;
            case 'f':
                pawL_add_float(P, buf, va_arg(arg, double));
                break;
            case 'p':
                add_pointer(P, buf, va_arg(arg, void *));
                break;
            default:
                PAW_UNREACHABLE();
        }
    }
}

void pawL_add_fstring(paw_Env *P, Buffer *buf, char const *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, buf, fmt, arg);
    va_end(arg);
}
