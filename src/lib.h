// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// Import rules:
// (1) Modules are imported by 'use' declarations in Paw code. For example, the
//     declaration 'use modname;' imports the module named 'modname'. Symbols
//     exported by 'modname' can be accessed using 'modname::symbol' syntax.
// (2) Each module mentioned in a compilation unit is imported exactly once.

#ifndef PAW_LIB_H
#define PAW_LIB_H

#include "paw.h"

#define PAWL_PRELUDE_NAME "prelude"
#define PAWL_MATH_NAME "math"
#define PAWL_IO_NAME "io"

// Evaluates to a path to the given standard library module's source code
#define PAWL_STDLIB_PATH(Module_) PAW_STDLIB_PREFIX Module_ ".paw"

struct LoaderState {
    paw_Reader f;
};

// Load the base library
void pawL_init(paw_Env *P);
void pawL_uninit(paw_Env *P);

// Functions for loading and compiling source code
int pawL_load_file(paw_Env *P, char const *pathname);
int pawL_load_nchunk(paw_Env *P, char const *name, char const *source, size_t length);
int pawL_load_chunk(paw_Env *P, char const *name, char const *source);
void pawL_load_symbols(paw_Env *P);

void pawL_push_symbols_map(paw_Env *P);
void pawL_push_modules_map(paw_Env *P);

void pawL_new_func(paw_Env *P, paw_Function func, int nup);
int pawL_register_func(paw_Env *P, char const *name, paw_Function func, int nup);

struct LoaderState *pawL_start_import(paw_Env *P);
void pawL_finish_import(paw_Env *P);

void pawL_close_loader(paw_Env *P, void *state);
void *pawL_chunk_reader(paw_Env *P, char const *text, size_t length);
void *pawL_file_reader(paw_Env *P, char const *pathname);
void pawL_add_extern_value(paw_Env *P, char const *modname, char const *name);
void pawL_add_extern_func(paw_Env *P, char const *modname, char const *name, paw_Function func);
void pawL_add_extern_method(paw_Env *P, char const *modname, char const *self, char const *name, paw_Function func);

#endif // PAW_LIB_H
