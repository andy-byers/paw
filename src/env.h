// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_ENV_H
#define PAW_ENV_H

#include "error.h"
#include "list.h"
#include "map.h"
#include "pool.h"
#include "str.h"
#include "value.h"

struct Jump; // call.c

enum {
    CSTR_SELF,
    CSTR_NEW,
    CSTR_UNIT,
    CSTR_TRUE,
    CSTR_FALSE,
    CSTR_BOOL,
    CSTR_CHAR,
    CSTR_INT,
    CSTR_FLOAT,
    CSTR_STR,
    CSTR_LIST,
    CSTR_MAP,
    CSTR_STRING,
    CSTR_OPTION,
    CSTR_RESULT,
    CSTR_RANGE,
    CSTR_RANGE_TO,
    CSTR_RANGE_FROM,
    CSTR_RANGE_FULL,
    CSTR_RANGE_INCLUSIVE,
    CSTR_RANGE_TO_INCLUSIVE,
    CSTR_LIST_ITERATOR,
    CSTR_MAP_ITERATOR,
    CSTR_HASH,
    CSTR_EQUALS,
    CSTR_COMPARE,
    CSTR_EXTERN,
    CSTR_EXCLAMATION,
    CSTR_UNDERSCORE,
    CSTR_KMODULES,
    CSTR_KSYMBOLS,
    CSTR_KSEARCHERS,
    NCSTR,
};

typedef struct paw_Env {
    struct paw_Options options;
    struct Compiler *C;
    struct Pool *pool;

    struct Statistics *stats;
    struct CallbackMap *callbacks;
    struct StrMap *registry;
    StringTable strings;

    struct Jump *jmp;

    Str const *pathname;
    Str const *modname;

    // Array of commonly-used strings.
    Str *string_cache[NCSTR];

    Str const *current_errmsg;

    // Contains an error message that is served when the system runs out of
    // memory (a call to the 'alloc' field below returned NULL).
    Str const *mem_errmsg;

    paw_Alloc alloc;
    void *ud;

    size_t num_bytes;

    struct ErrorHandler error;

} paw_Env;

void pawE_register_callback(paw_Env *P, char const *name, paw_Function cb);

void pawE_init(paw_Env *P);
void pawE_uninit(paw_Env *P);
_Noreturn void pawE_error(paw_Env *P, int code, int line, char const *fmt, ...);

#define CACHED_STRING(P, k) CHECK_EXP((k) < NCSTR, (P)->string_cache[k])

#endif // PAW_ENV_H
