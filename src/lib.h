// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_LIB_H
#define PAW_LIB_H

#include "paw.h"

#define IOLIB_NAME "io"
#define MATHLIB_NAME "math"

// From value.h
union Value;

// Load the base library
void pawL_init(paw_Env *P);

void pawL_check_argc(paw_Env *P, int argc);
int pawL_check_varargc(paw_Env *P, int min, int max);

// TODO: Look on disk for .paw files to load
void pawL_require_lib(paw_Env *P, const char *name);

// Functions for loading and compiling source code
int pawL_load_file(paw_Env *P, const char *pathname);
int pawL_load_nchunk(paw_Env *P, const char *name, const char *source,
                     size_t length);
int pawL_load_chunk(paw_Env *P, const char *name, const char *source);

#define L_GENERIC_MAX ARGC_MAX
#define L_PARAM_MAX ARGC_MAX
#define L_LIST_END INT_MIN
#define L_SELF (INT_MIN + 1)

#define l_generic(i) (-(i) - 1)
#define l_list(...)                                                            \
    (paw_Type[]) { __VA_ARGS__, L_LIST_END }
#define l_list_0()                                                             \
    (paw_Type[]) { L_LIST_END }

#endif // PAW_LIB_H
