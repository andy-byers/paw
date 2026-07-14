// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STD_PAW_H
#define PAW_STD_PAW_H

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

#define PAW_MALLOC malloc
#define PAW_REALLOC realloc
#define PAW_FREE free

#define PAW_ASSERT assert
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
    paw_Char const *text;
    paw_Int length;
} paw_Str;

#define PAW_DEFINE_LIST(T) \
    typedef struct { \
        paw_##T *data; \
        paw_Int length; \
        paw_Int capacity; \
    } *paw_List_##T;

PAW_DEFINE_LIST(Char)

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

typedef struct {
    void *start;
    size_t length;
} paw_Slice;


void paw_assert(paw_Bool condition);
_Noreturn void paw_panic_(paw_Slice message);

paw_Str paw_str_from_raw_parts(char const *ptr, paw_Int len);
paw_Int paw_str_len(paw_Str self);
char const *paw_ops_str_AsPtr_as_ptr(paw_Str *self);
paw_Option_Int paw_str_find(paw_Str, paw_Str self);
paw_Bool paw_str_starts_with(paw_Str, paw_Str self);
paw_Bool paw_str_ends_with(paw_Str, paw_Str self);

paw_Bool paw_ops_str_Compare_lt(paw_Str self, paw_Str rhs);
paw_Bool paw_ops_str_Compare_le(paw_Str self, paw_Str rhs);

paw_Option_Float paw_internal_parse_float(paw_Str self);

uint64_t paw_builtin_hash_bytes(paw_Char const *bytes, size_t length, uint64_t hash);
int64_t paw_builtin_rawcmp(paw_Char const *lhs, size_t lhs_length, paw_Char const *rhs, size_t rhs_length);

void paw_builtin_check_bounds(paw_Int index, paw_Int length);

void *paw_ops_Slice_AsPtr_as_ptr(paw_Slice *self);
size_t paw_slice_Slice_len(paw_Slice self);
paw_Slice paw_slice_from_raw_parts(void *start, size_t length);

typedef struct paw_mem_OOM {
    paw_Unit _;
} paw_mem_OOM;

typedef void *paw_Ptr;
PAW_DEFINE_RESULT(Ptr, mem_OOM)

paw_Result_Ptr_mem_OOM paw_mem_raw_alloc(unsigned long size);
paw_Result_Ptr_mem_OOM paw_mem_raw_realloc(void *ptr, unsigned long size);
paw_Result_Ptr_mem_OOM paw_mem_aligned_alloc(unsigned alignment, unsigned long size);
void paw_mem_raw_dealloc(void *ptr);

void *paw_ptr_memcpy(void *dest, void *src, paw_Int size);
void *paw_ptr_memmove(void *dest, void *src, paw_Int size);
void *paw_ptr_memset(void *ptr, char value, paw_Int size);
int paw_ptr_memcmp(void *lhs, void *rhs, paw_Int size);

paw_Int paw_fmt_write_float(paw_Float value, paw_Int precision, char *output, paw_Int output_len);

#endif // PAW_STD_PAW_H
