// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_AUX_H
#define PAW_AUX_H

#include "paw.h"
#include "util.h"
#include "value.h"
#include <string.h>

#define BUFFER_LIMIT 512

typedef struct Buffer {
    char *data;
    size_t size;
    size_t alloc;

    char stack[BUFFER_LIMIT];
} Buffer;

static inline paw_Bool pawL_boxed(const Buffer *buf)
{
    return buf->data != buf->stack;
}

void pawL_init_buffer(paw_Env *P, Buffer *buf);
void pawL_reverse_buffer(paw_Env *P, Buffer *buf);
void pawL_discard_result(paw_Env *P, Buffer *buf);
void pawL_push_result(paw_Env *P, Buffer *buf);
void pawL_buffer_resize(paw_Env *P, Buffer *buf, size_t n);

#define pawL_add_literal(P, buf, s) pawL_add_nstring(P, buf, s, paw_lengthof(s))

void pawL_add_char(paw_Env *P, Buffer *buf, char c);
void pawL_add_string(paw_Env *P, Buffer *buf, const char *s);
void pawL_add_nstring(paw_Env *P, Buffer *buf, const char *s, size_t n);
void pawL_add_integer(paw_Env *P, Buffer *buf, paw_Int i);
void pawL_add_float(paw_Env *P, Buffer *buf, paw_Float f);
void pawL_add_pointer(paw_Env *P, Buffer *buf, void *p);
void pawL_add_value(paw_Env *P, Buffer *print);
void pawL_add_vfstring(paw_Env *P, Buffer *buf, const char *fmt, va_list arg);
void pawL_add_fstring(paw_Env *P, Buffer *buf, const char *fmt, ...);

// TODO: Move to bigint.h or value.h?
size_t pawL_integer_format_size(size_t nbits, int base);

#endif // PAW_AUX_H
