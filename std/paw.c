// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "paw.h"

#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void paw_assert(paw_Bool cond)
{
    if (!cond) {
        char message[] = "assertion failed";
        paw_panic_((paw_Slice){
                .start = message,
                .length = PAW_LENGTHOF(message),
            });
    }
}

_Noreturn void paw_panic_(paw_Slice message)
{
    fwrite(message.start, 1, (size_t)message.length, stderr);
    exit(EXIT_FAILURE);
}

paw_Str paw_str_from_raw_parts(char const *ptr, paw_Int len)
{
    return (paw_Str){
        .text = ptr,
        .length = len,
    };
}

paw_Int paw_str_len(paw_Str self)
{
    return self.length;
}

char const *paw_ops_str_AsPtr_as_ptr(paw_Str *self)
{
    return self->text;
}

static int parse_float(paw_Char const *text, paw_Float *result_ptr)
{
#define IS_DIGIT(Ch_) ('0' <= (Ch_) && (Ch_) <= '9')
#define IS_FP(Ch_) ((Ch_) == '.' || (Ch_) == 'e' || (Ch_) == 'E')

    paw_Bool const is_negative = *text == '-';
    text += is_negative || *text == '+';

    // First, validate the number format.
    paw_Char const *p = text;
    if (p[0] == '0' && p[1] != '\0' && !IS_FP(p[1]))
        return -1;

    while (IS_DIGIT(*p)) ++p;

    if (*p == '.') {
        ++p;
        while (IS_DIGIT(*p)) ++p;
    }
    if (*p == 'e' || *p == 'E') {
        p += 1 + (p[1] == '+' || p[1] == '-');
        if (!IS_DIGIT(*p)) return -1;
        while (IS_DIGIT(*p)) ++p;
    }
    if (*p != '\0') return -1;
    paw_Float const f = strtod(text, NULL);
    *result_ptr = is_negative ? -f : f;
    return 0;

#undef IS_FP
#undef IS_DIGIT
}

// fn parse_float(self) -> Option<float>
paw_Option_Float paw_internal_parse_float(paw_Str self)
{
    paw_Float result;
    if (parse_float(self.text, &result) == 0) {
        return paw_Option_Float_some(result);
    } else {
        return paw_Option_Float_none();
    }
}

static paw_Char const *find_substr(paw_Char const *str, paw_Int nstr, paw_Char const *sub, paw_Int nsub)
{
    if (nsub == 0)
        return str;

    paw_Char const *end = str + nstr;
    while ((str = strchr(str, sub[0]))) {
        if (nsub <= end - str
                && memcmp(str, sub, (size_t)nsub) == 0)
            return str;
        str += nsub;
    }

    return NULL;
}

// fn find(self, target: str) -> Option<int>
paw_Option_Int paw_str_find(paw_Str self, paw_Str target)
{
    paw_Char const *result = find_substr(
        self.text, self.length,
        target.text, target.length);
    if (result != NULL) {
        return paw_Option_Int_some(result - self.text);
    } else {
        return paw_Option_Int_none();
    }
}

// fn paw_slice_from_raw_parts<T>(start: *T, length: int) -> []T
paw_Slice paw_slice_from_raw_parts(void *start, size_t length)
{
    return (paw_Slice){
        .start = start,
        .length = length,
    };
}

// fn as_ptr(*self) -> *T
void *paw_ops_Slice_AsPtr_as_ptr(paw_Slice *self)
{
    return self->start;
}

// fn len(self) -> int
size_t paw_slice_Slice_len(paw_Slice self)
{
    return self.length;
}

paw_Result_Ptr_mem_OOM paw_mem_raw_alloc(unsigned long size)
{
    void *ptr = malloc(size);
    return ptr != NULL
        ? paw_Result_Ptr_mem_OOM_ok(ptr)
        : paw_Result_Ptr_mem_OOM_err((paw_mem_OOM){{}});
}

paw_Result_Ptr_mem_OOM paw_mem_raw_realloc(void *ptr, unsigned long size)
{
    ptr = realloc(ptr, size);
    return ptr != NULL
        ? paw_Result_Ptr_mem_OOM_ok(ptr)
        : paw_Result_Ptr_mem_OOM_err((paw_mem_OOM){{}});
}

paw_Result_Ptr_mem_OOM paw_mem_aligned_alloc(unsigned alignment, unsigned long size)
{
    void *ptr = aligned_alloc(alignment, size);
    return ptr != NULL
        ? paw_Result_Ptr_mem_OOM_ok(ptr)
        : paw_Result_Ptr_mem_OOM_err((paw_mem_OOM){{}});
}

void paw_mem_raw_dealloc(void *ptr)
{
    free(ptr);
}

// fn memcpy(dest: *char, src: *char, size: int) -> *char
void *paw_ptr_memcpy(void *dest, void *src, size_t size)
{
    return memcpy(dest, src, size);
}

// fn memmove(dest: *char, src: *char, size: int) -> *char
void *paw_ptr_memmove(void *dest, void *src, size_t size)
{
    return memmove(dest, src, size);
}

// fn memset(ptr: *char, value: char, size: int) -> *char
void *paw_ptr_memset(void *ptr, char value, size_t size)
{
    return memset(ptr, value, size);
}

// fn memcmp(lhs: *char, rhs: *char, size: int) -> int
int64_t paw_ptr_memcmp(void *lhs, void *rhs, size_t size)
{
    return memcmp(lhs, rhs, size);
}

void paw_builtin_check_bounds(paw_Int index, paw_Int length)
{
    if (index < 0 || index >= length) {
        char buffer[200]; // space for error message
        int const n = snprintf(buffer, PAW_COUNTOF(buffer),
                "index %" PRId64 " out of bounds for sequence of length %" PRId64 "\n",
                index, length);
        PAW_ASSERT((size_t)n < PAW_COUNTOF(buffer));
        paw_panic_((paw_Slice){
                .start = buffer,
                .length = (size_t)n,
            });
    }
}

paw_Int paw_fmt_write_float(paw_Float value, paw_Int precision, char *output, paw_Int output_len)
{
    return snprintf(output, output_len, "%.*g", (int)precision, value);
}

