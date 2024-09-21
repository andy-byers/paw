// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_LIB_H
#define PAW_LIB_H

#include "paw.h"

// Load the base library
void pawL_init(paw_Env *P);

// Functions for loading and compiling source code
int pawL_load_file(paw_Env *P, const char *pathname);
int pawL_load_nchunk(paw_Env *P, const char *name, const char *source, size_t length);
int pawL_load_chunk(paw_Env *P, const char *name, const char *source);

void pawL_new_func(paw_Env *P, paw_Function func, int nup);
int pawL_register_func(paw_Env *P, const char *name, paw_Function func, int nup);

void *pawL_start_import(paw_Env *P, paw_Reader *preader);
void pawL_finish_import(paw_Env *P);

void *pawL_chunk_reader(paw_Env *P, const char *text, size_t length, paw_Reader *preader);
void pawL_add_extern_func(paw_Env *P, const char *modname, const char *name, paw_Function func);
void pawL_add_extern_method(paw_Env *P, const char *modname, const char *self, const char *name, paw_Function func);

#endif // PAW_LIB_H
