// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CORE_H
#define PAW_CORE_H

#include "config.h"
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
# define EXTERN_C extern "C"
#else // __cplusplus
# define EXTERN_C
#endif // !__cplusplus

typedef int paw_Type;
typedef char paw_Char;
typedef int8_t paw_Int8;
typedef int16_t paw_Int16;
typedef int32_t paw_Int32;
typedef int64_t paw_Int64;
typedef intptr_t paw_Isize;
typedef ptrdiff_t paw_Ioffset;
typedef uint8_t paw_Uint8;
typedef uint16_t paw_Uint16;
typedef uint32_t paw_Uint32;
typedef uint64_t paw_Uint64;
typedef size_t paw_Usize;
typedef uintptr_t paw_Uoffset;
typedef float paw_Float32;
typedef double paw_Float64;

// TODO: remove
typedef long long paw_Int;
typedef unsigned long long paw_Uint;
typedef double paw_Float;

#ifdef __cplusplus
typedef bool paw_Bool;
#else // __cplusplus
typedef _Bool paw_Bool;
#endif // !__cplusplus

#define PAW_FALSE 0
#define PAW_TRUE 1

typedef struct paw_Env paw_Env;

typedef int (*paw_Function)(paw_Env *P, void *ud);
typedef void *(*paw_Alloc)(void *ud, void *ptr, size_t size0, size_t size);
typedef char const *(*paw_Reader)(paw_Env *P, void *ud, size_t *size);

struct paw_Options {
    void *ud;
    paw_Alloc alloc;
    char opt_suffix;
    char const *output_dirname;
    char const *output_filename;
    char const *include_paths;
    char const **linker_specs;
    char const **linker_paths;
    int num_linker_specs;
    int num_linker_paths;
    paw_Bool compile_only : 1;
    paw_Bool build_tests : 1;
    paw_Bool enable_asan : 1;
    paw_Bool verify_ir : 1;
    paw_Bool dump_ir : 1;
    paw_Bool no_std : 1;
};
paw_Env *paw_open(struct paw_Options const *o);
void paw_close(paw_Env *P);

paw_Alloc paw_get_allocator(paw_Env *P);
void paw_set_allocator(paw_Env *P, paw_Alloc alloc, void *ud);
void *paw_context(paw_Env const *P);

#define PAW_OK 0U
#define PAW_EMEMORY 1U
#define PAW_ESYSTEM 2U
#define PAW_EOVERFLOW 3U

// Load paw source code from the given 'input' source
int paw_load(paw_Env *P, paw_Reader input, char const *modname, char const *dirname, char const *pathname, void *ud);

// Basic types
#define PAW_TUNIT 0
#define PAW_TBOOL 1
#define PAW_TCHAR 2
#define PAW_TINT 3
#define PAW_TFLOAT 4
#define PAW_TSTR 5

#define PAW_OPTION_SOME 0
#define PAW_OPTION_NONE 1
#define PAW_RESULT_OK 0
#define PAW_RESULT_ERR 1

size_t paw_bytes_used(paw_Env const *P);

#endif // PAW_CORE_H
