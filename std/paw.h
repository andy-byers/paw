// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STD_PAW_H
#define PAW_STD_PAW_H

#include <assert.h>
#include <stdint.h>

#include <gc.h>

#define PAW_MALLOC GC_MALLOC
#define PAW_REALLOC GC_REALLOC
#define PAW_FREE GC_FREE

#define paw_assert assert

#define PAW_UNUSED(X_) ((void)(X_))
#define PAW_LENGTHOF(X_) (sizeof(X_) - 1)
#define PAW_ALIGNOF(X_) _Alignof(X_)
#define PAW_COUNTOF(X_) (sizeof(X_) / sizeof((X_)[0]))

#define PAW_MIN(x, y) ((x) < (y) ? (x) : (y))
#define PAW_MAX(x, y) ((x) > (y) ? (x) : (y))
#define PAW_CLAMP(v, x, y) PAW_MIN(PAW_MAX(v, x), y)

#define PAW_INT_MAX INT64_MAX
#define PAW_INT_MIN INT64_MIN
#define PAW_INT_C(x) INT64_C(x)

#define PAW_SIZE_MAX (sizeof(size_t) < sizeof(paw_Int) \
                          ? SIZE_MAX                   \
                          : (size_t)PAW_INT_MAX)

typedef struct {
    // NOTE: requires zero-length array extension
    char _[0];
} paw_Unit;

#define PAW_UNIT() ((paw_Unit){._ = {}})

_Static_assert(sizeof(paw_Unit) == 0,
        "\"paw_Unit\" must be equivalent to LLVM empty struct \"{}\"");

typedef _Bool paw_Bool;
typedef char paw_Char;
typedef int64_t paw_Int;
typedef double paw_Float;

#define PAW_FALSE ((paw_Bool)0)
#define PAW_TRUE ((paw_Bool)1)

#define PAW_OPTION_SOME 0
#define PAW_OPTION_NONE 1
#define PAW_RESULT_OK 0
#define PAW_RESULT_ERR 1

typedef struct {
    paw_Int length;
    uint32_t hash;
    paw_Char text[];
} *paw_Str;

#define PAW_DEFINE_LIST(T) \
    typedef struct { \
        paw_##T *data; \
        paw_Int length; \
        paw_Int capacity; \
    } *paw_List_##T;

PAW_DEFINE_LIST(Char)
PAW_DEFINE_LIST(Str)

#define PAW_DEFINE_OPTION(T) \
    typedef struct { \
        paw_Int discr; \
        paw_##T value; \
    } paw_OptionSome_##T; \
    typedef struct { \
        paw_Int discr; \
    } paw_OptionNone_##T; \
    typedef struct { \
        union { \
            paw_OptionSome_##T some; \
            paw_OptionNone_##T none; \
        }; \
    } paw_Option_##T; \
    static inline paw_Option_##T paw_Option_##T##_some(paw_##T value) \
    { \
        return (paw_Option_##T){ \
            .some.discr = PAW_OPTION_SOME, \
            .some.value = value, \
        }; \
    } \
    static inline paw_Option_##T paw_Option_##T##_none(void) \
    { \
        return (paw_Option_##T){ \
            .none.discr = PAW_OPTION_NONE, \
        }; \
    }
PAW_DEFINE_OPTION(Int)
PAW_DEFINE_OPTION(Float)

#define PAW_DEFINE_RESULT(T, E) \
    typedef struct { \
        paw_Int discr; \
        paw_##T value; \
    } paw_ResultOk_##T##_##E; \
    typedef struct { \
        paw_Int discr; \
        paw_##E error; \
    } paw_ResultErr_##T##_##E; \
    typedef struct { \
        union { \
            paw_ResultOk_##T##_##E ok; \
            paw_ResultErr_##T##_##E err; \
        }; \
    } paw_Result_##T##_##E; \
    static inline paw_Result_##T##_##E paw_Result_##T##_##E##_ok(paw_##T value) \
    { \
        return (paw_Result_##T##_##E){ \
            .ok.discr = PAW_RESULT_OK, \
            .ok.value = value, \
        }; \
    } \
    static inline paw_Result_##T##_##E paw_Result_##T##_##E##_err(paw_##E error) \
    { \
        return (paw_Result_##T##_##E){ \
            .err.discr = PAW_RESULT_ERR, \
            .err.error = error, \
        }; \
    }


void paw_prelude_println(void *env, paw_Str self);
void paw_prelude_print(void *env, paw_Str self);
void paw_prelude_assert(void *env, paw_Bool self);
_Noreturn void paw_prelude_panic(void *env, paw_Str self);

paw_Str paw_prelude_char_to_str(void *env, paw_Char self);

paw_Str paw_prelude_int_to_str(void *env, paw_Int self);

paw_Int paw_prelude_float_hash(void *env, paw_Float self);
paw_Str paw_prelude_float_to_str(void *env, paw_Float self);

paw_Option_Int paw_prelude_str_parse_int(void *env, paw_Str self);
paw_Option_Int paw_prelude_str_parse_int_radix(void *env, paw_Str self, paw_Int base);
paw_Option_Float paw_prelude_str_parse_float(void *env, paw_Str self);
paw_Option_Int paw_prelude_str_find(void *env, paw_Str, paw_Str self);
paw_List_Str paw_prelude_str_split(void *env, paw_Str, paw_Str self);
paw_Str paw_prelude_str_join(void *env, paw_Str, paw_List_Str self);
paw_Bool paw_prelude_str_starts_with(void *env, paw_Str, paw_Str self);
paw_Bool paw_prelude_str_ends_with(void *env, paw_Str, paw_Str self);
paw_Int paw_prelude_str_hash(void *env, paw_Str self);
paw_Str paw_prelude_str_substr(void *env, paw_Str self, paw_Int offset, paw_Int length);

uint32_t paw_builtin_hash_bytes(paw_Char const *bytes, paw_Int length, uint32_t hash);
paw_Int paw_builtin_rawcmp(paw_Char const *lhs, paw_Int lhs_length, paw_Char const *rhs, paw_Int rhs_length);
paw_Int paw_builtin_abs_index(paw_Int index, paw_Int length);

typedef struct paw_str_builder_Builder {
    paw_List_Char buf;
} paw_str_builder_Builder;

paw_str_builder_Builder paw_str_builder_Builder_append_char(void *env, paw_str_builder_Builder b, paw_Char value);
paw_str_builder_Builder paw_str_builder_Builder_append_str(void *env, paw_str_builder_Builder b, paw_Str value);
paw_Str paw_str_builder_Builder_string(void *env, paw_str_builder_Builder b);

#endif // PAW_STD_PAW_H
