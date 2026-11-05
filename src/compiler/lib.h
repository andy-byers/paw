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

#include "core.h"

struct Str;

#define PAWL_STD_MODULES(X) \

enum lib_StdModule {
    // Core modules:
    PAWL_STD_PRELUDE,
    PAWL_STD_OPS,
    PAWL_STD_PTR,
    PAWL_STD_MEM,
    PAWL_STD_SLICE,
    PAWL_STD_ARRAY,
    PAWL_STD_OPTION,
    PAWL_STD_RESULT,
    PAWL_STD_ITER,
    PAWL_STD_CLIB,

    // Standard library modules:
    PAWL_STD_STRING,
    PAWL_STD_LIST,
    PAWL_STD_HASHMAP,
    PAWL_STD_FMT,
    PAWL_STD_IO,
    PAWL_STD_OS,
    PAWL_STD_MATH,
    PAWL_STD_STRING_BUILDER,
    PAWL_STD_RAND,

    PAWL_NUM_STD_MODULES
};

#define PAWL_NUM_CORE_MODULES (PAWL_STD_RESULT + 1)

char const *lib_std_module_name(enum lib_StdModule m);

static paw_Bool lib_is_core_module(enum lib_StdModule m)
{
    return m <= PAWL_STD_RESULT;
}

struct FileReader {
    char data[512];
    struct Str const *pathname;
    struct Str const *dirname;
    struct File *file;
    paw_Reader f;
    paw_Bool err;
};


// Load the base library
void lib_init(paw_Env *P);
void lib_uninit(paw_Env *P);

// Functions for loading and compiling source code
int lib_load_file(paw_Env *P, char const *modname, char const *pathname, char const *cwd);
int lib_load_nchunk(paw_Env *P, char const *modname, char const *source, size_t length);
int lib_load_chunk(paw_Env *P, char const *modname, char const *source);

void lib_push_symbols_map(paw_Env *P);
void lib_push_modules_map(paw_Env *P);

int lib_register_fn(paw_Env *P, char const *name, paw_Function fn, int nup);

#define IMPORT_FOUND 1
#define IMPORT_NOT_FOUND 0
int lib_start_import(paw_Env *P, struct Str const *modname, struct FileReader *result);
void lib_finish_import(paw_Env *P, struct FileReader *fr);

void lib_close_loader(paw_Env *P, void *state);

#endif // PAW_LIB_H
