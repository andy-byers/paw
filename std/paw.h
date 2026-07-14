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

#define PAW_INT64_MAX INT64_MAX
#define PAW_INT64_MIN INT64_MIN
#define PAW_INT64_C(x) INT64_C(x)

#define PAW_SIZE_MAX (sizeof(paw_Usize) < sizeof(paw_Int64) \
        ? SIZE_MAX : (paw_Usize)PAW_INT64_MAX)

typedef struct {
    // NOTE: Paw currently requires `sizeof(T) > 0` for all `T`
    char _;
} paw_Unit;

#define PAW_UNIT() ((paw_Unit){0})

typedef uint8_t paw_Bool;
typedef char paw_Char;
typedef int64_t paw_Int64;
typedef size_t paw_Usize;
typedef double paw_Float64;

#define PAW_FALSE ((paw_Bool)0)
#define PAW_TRUE ((paw_Bool)1)

#define PAW_OPTION_SOME 0
#define PAW_OPTION_NONE 1
#define PAW_RESULT_OK 0
#define PAW_RESULT_ERR 1

typedef struct {
    paw_Char const *text;
    paw_Usize length;
} paw_Str;

#define PAW_DEFINE_LIST(T) \
    typedef struct { \
        paw_##T *data; \
        paw_Usize length; \
        paw_Usize capacity; \
    } *paw_List_##T;

PAW_DEFINE_LIST(Char)

#define PAW_DEFINE_OPTION(T) \
    typedef struct { \
        paw_Int64 discr; \
        paw_##T value; \
    } paw_OptionSome_##T; \
    typedef struct { \
        paw_Int64 discr; \
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
PAW_DEFINE_OPTION(Int64)
PAW_DEFINE_OPTION(Float64)

#define PAW_DEFINE_RESULT(T, E) \
    typedef struct { \
        paw_Int64 discr; \
        paw_##T value; \
    } paw_ResultOk_##T##_##E; \
    typedef struct { \
        paw_Int64 discr; \
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
    paw_Usize length;
} paw_Slice;


void paw_assert(paw_Bool condition);
_Noreturn void paw_panic_(paw_Slice message);

paw_Str paw_str_from_raw_parts(char const *ptr, paw_Usize len);
paw_Usize paw_str_len(paw_Str self);
char const *paw_ops_str_AsPtr_as_ptr(paw_Str *self);
paw_Option_Int64 paw_str_find(paw_Str, paw_Str self);

paw_Option_Float64 paw_internal_parse_float(paw_Str self);

void paw_builtin_check_bounds(paw_Usize index, paw_Usize length);

void *paw_ops_Slice_AsPtr_as_ptr(paw_Slice *self);
paw_Usize paw_slice_Slice_len(paw_Slice self);
paw_Slice paw_slice_from_raw_parts(void *start, paw_Usize length);

typedef struct paw_mem_OOM {
    paw_Unit _;
} paw_mem_OOM;

typedef void *paw_Ptr;
PAW_DEFINE_RESULT(Ptr, mem_OOM)

paw_Result_Ptr_mem_OOM paw_mem_raw_alloc(paw_Usize size);
paw_Result_Ptr_mem_OOM paw_mem_raw_realloc(void *ptr, paw_Usize size);
paw_Result_Ptr_mem_OOM paw_mem_aligned_alloc(paw_Usize alignment, paw_Usize size);
void paw_mem_raw_dealloc(void *ptr);

void *paw_ptr_memcpy(void *dest, void *src, paw_Usize size);
void *paw_ptr_memmove(void *dest, void *src, paw_Usize size);
void *paw_ptr_memset(void *ptr, char value, paw_Usize size);
paw_Int64 paw_ptr_memcmp(void *lhs, void *rhs, paw_Usize size);

paw_Int64 paw_fmt_write_float(double value, paw_Int64 precision, char *output, paw_Usize output_len);

#endif // PAW_STD_PAW_H
