// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_COMMON_AUX_H
#define PAW_COMMON_AUX_H

#include "compiler/core.h"
#include "common/util.h"
#include <string.h>

#define BUFFER_LIMIT 512

typedef struct Buffer {
    char *data;
    size_t size;
    size_t alloc;

    char stack[BUFFER_LIMIT];
} Buffer;

#define L_IS_BOXED(buf) ((buf)->data != (buf)->stack)

void lib_init_buffer(paw_Env *P, Buffer *buf);
void lib_buffer_discard(paw_Env *P, Buffer *buf);
struct Str const *lib_buffer_finish(paw_Env *P, Buffer *buf);
void lib_buffer_resize(paw_Env *P, Buffer *buf, size_t n);

#define L_ADD_LITERAL(P, buf, s) lib_add_nstring(P, buf, s "", PAW_LENGTHOF(s))
#define L_ADD_STRING(P, buf, s) lib_add_nstring(P, buf, (s)->text, pawS_length(s))

void lib_add_char(paw_Env *P, Buffer *buf, char c);
void lib_add_string(paw_Env *P, Buffer *buf, char const *s);
void lib_add_nstring(paw_Env *P, Buffer *buf, char const *s, size_t n);
void lib_add_int(paw_Env *P, Buffer *buf, paw_Int64 i);
void lib_add_float(paw_Env *P, Buffer *buf, paw_Float64 f);
void lib_add_vfstring(paw_Env *P, Buffer *buf, char const *fmt, va_list arg);
void lib_add_fstring(paw_Env *P, Buffer *buf, char const *fmt, ...);
void lib_add_hex(paw_Env *P, Buffer *buf, paw_Uint u);

#endif // PAW_COMMON_AUX_H
