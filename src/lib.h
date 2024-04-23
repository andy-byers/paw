// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_LIB_H
#define PAW_LIB_H

#include "paw.h"

#define IOLIB_NAME "io"
#define MATHLIB_NAME "math"

// Load the base library
void pawL_init(paw_Env *P);

void pawL_check_argc(paw_Env *P, int argc);
int pawL_check_varargc(paw_Env *P, int min, int max);

// TODO: Look on disk for .paw files to load
void pawL_require_lib(paw_Env *P, const char *name);

void pawL_register_function(paw_Env *P, const char *name, paw_Function func, paw_Type *argt, paw_Type ret);
void pawL_bind_method(paw_Env *P, int index, const char *name, paw_Function func, paw_Type *argt, paw_Type ret);

#define t_list_0() (paw_Type[]){-1}
#define t_list_1(a) (paw_Type[]){a, -1}
#define t_list_2(a, b) (paw_Type[]){a, b, -1}
#define t_list_3(a, b, c) (paw_Type[]){a, b, c, -1}

// Functions for loading and compiling source code
int pawL_load_file(paw_Env *P, const char *pathname);
int pawL_load_nchunk(paw_Env *P, const char *name, const char *source, size_t length);
int pawL_load_chunk(paw_Env *P, const char *name, const char *source);

#endif // PAW_LIB_H
