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
    ptrdiff_t boxloc;
    char *data;
    size_t size;
    size_t alloc;

    char stack[BUFFER_LIMIT];
} Buffer;

#define L_IS_BOXED(buf) ((buf)->data != (buf)->stack)

void pawL_init_buffer(paw_Env *P, Buffer *buf);
void pawL_discard_result(paw_Env *P, Buffer *buf);
void pawL_push_result(paw_Env *P, Buffer *buf);
void pawL_buffer_resize(paw_Env *P, Buffer *buf, size_t n);

#define L_ADD_LITERAL(P, buf, s) pawL_add_nstring(P, buf, s, PAW_LENGTHOF(s))
#define L_ADD_STRING(P, buf, s) pawL_add_nstring(P, buf, (s)->text, pawS_length(s))

void pawL_add_char(paw_Env *P, Buffer *buf, char c);
void pawL_add_string(paw_Env *P, Buffer *buf, char const *s);
void pawL_add_nstring(paw_Env *P, Buffer *buf, char const *s, size_t n);
void pawL_add_int(paw_Env *P, Buffer *buf, paw_Int i);
void pawL_add_float(paw_Env *P, Buffer *buf, paw_Float f);
void pawL_add_vfstring(paw_Env *P, Buffer *buf, char const *fmt, va_list arg);
void pawL_add_fstring(paw_Env *P, Buffer *buf, char const *fmt, ...);

#endif // PAW_AUX_H
