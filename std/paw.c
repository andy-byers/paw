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


// TODO: recast these as functions that write to a "Format" object
//paw_Str paw_char_to_str(paw_Char *self)
//{
//    return new_str(self, 1);
//}
//
//paw_Str paw_int_to_str(paw_Int *self)
//{
//    paw_Char temp[32];
//    paw_Bool const negative = *self < 0;
//    paw_Char *end = temp + PAW_COUNTOF(temp);
//    paw_Char *ptr = end - 1;
//
//    // Don't call llabs(PAW_INT_MIN). The result is undefined on 2s complement
//    // systems.
//    uint64_t u = *self == PAW_INT_MIN
//                     ? UINT64_C(1) << 63
//                     : (uint64_t)llabs(*self);
//    do {
//        *ptr-- = (paw_Char)(u % 10 + '0');
//        u /= 10;
//    } while (u);
//
//    if (negative) {
//        *ptr = '-';
//    } else {
//        ++ptr;
//    }
//    return new_str(ptr, end - ptr);
//}
//
//paw_Str paw_float_to_str(paw_Float *self)
//{
//    paw_Char temp[32];
//    int const n = snprintf(temp, PAW_COUNTOF(temp), "%.*g", 17, *self);
//    return new_str(temp, n);
//}

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

// pub fn parse_int(self) -> Option<int>
paw_Option_Int paw_str_parse_int(paw_Str self)
{
    return paw_str_parse_int_radix(self, 10);
}

static unsigned char_to_digit(paw_Char c)
{
    static const unsigned char LOOKUP[0x100] = {
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
        0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
        0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
        0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    };

    return LOOKUP[(unsigned)c];
}

static int parse_int_radix(paw_Char const *text, paw_Int length, paw_Int base, paw_Int *result_ptr)
{
    paw_Bool const is_negative = *text == '-';
    paw_Bool const skip_first = is_negative || *text == '+';
    length -= skip_first;
    text += skip_first;

    uint64_t const b = (uint64_t)base;

    if (b < 2 || b > 36)
        return -1; // invalid "base"

    uint64_t value = 0;
    for (paw_Int i = 0; i < length; ++i) {
        uint64_t const v = char_to_digit(text[i]);
        if (v >= b || value > (UINT64_MAX - v) / b)
            return -1; // "value" too large for unsigned i64
        value = value * b + v;
    }
    if (value > (uint64_t)PAW_INT_MAX + is_negative)
        return -1; // "value" too large for Paw integer (i64)

    *result_ptr = (paw_Int)(is_negative ? -value : value);
    return 0;
}

// fn parse_int_radix(self, base: int) -> Option<int>
paw_Option_Int paw_str_parse_int_radix(paw_Str self, paw_Int base)
{
    paw_Int result;
    if (parse_int_radix(self.text, self.length, base, &result) == 0) {
        return paw_Option_Int_some(result);
    } else {
        return paw_Option_Int_none();
    }
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
paw_Option_Float paw_str_parse_float(paw_Str self)
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

//// pub fn split(self, sep: str) -> [str]
//paw_List_Str paw_str_split(paw_Str self, paw_Str sep)
//{
//#define LIST_PUSH_STR _PM4list4ListIsE4list4push
//    extern void LIST_PUSH_STR(void *, paw_List_Str, paw_Str);
//
//    if (sep->length == 0)
//        paw_panic(NEW_LITERAL("empty separator"));
//
//    size_t const INITIAL_CAPACITY = 4;
//    paw_List_Str list = PAW_MALLOC(sizeof *list);
//    list->data = PAW_MALLOC(INITIAL_CAPACITY * sizeof *list->data);
//    list->capacity = INITIAL_CAPACITY;
//    list->length = 0;
//
//    int num_parts = 0;
//    paw_Char const *part;
//    paw_Int rest_length = self->length;
//    paw_Char const *rest_text = self->text;
//    while ((part = find_substr(rest_text, rest_length, sep->text, sep->length))) {
//        if (num_parts == INT_MAX)
//            paw_panic(NEW_LITERAL("too many substrings"));
//        paw_Int const n = part - rest_text;
//        LIST_PUSH_STR(NULL, list, new_str(rest_text, n));
//        part += sep->length; // skip separator
//        rest_text = part;
//        rest_length -= n;
//        ++num_parts;
//    }
//    char const *end = self->text + self->length; // add the rest
//    LIST_PUSH_STR(NULL, list, new_str(rest_text, end - rest_text));
//
//    return list;
//
//#undef LIST_PUSH_STR
//}
//
//// pub fn join(self, parts: [str]) -> str;
//paw_Str paw_str_join(paw_Str self, paw_List_Str parts)
//{
//    paw_Str joined;
//    {
//        struct StringBuilder sb;
//        sb_init(&sb);
//        for (paw_Int i = 0; i < parts->length; ++i) {
//            sb_add_str(&sb, parts->data[i]);
//            if (i < parts->length - 1)
//                sb_add_str(&sb, self);
//        }
//        joined = sb_create_str(&sb);
//        sb_uninit(&sb);
//    }
//    return joined;
//}

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

// fn starts_with(self, prefix: str) -> bool
paw_Bool paw_str_starts_with(paw_Str self, paw_Str prefix)
{
    return self.length >= prefix.length
        && 0 == memcmp(prefix.text, self.text, (size_t)prefix.length);
}

// fn ends_with(*self, suffix: str) -> bool
paw_Bool paw_str_ends_with(paw_Str self, paw_Str suffix)
{
    if (self.length >= suffix.length) {
        paw_Char const *ptr = self.text + self.length - suffix.length;
        return 0 == memcmp(suffix.text, ptr, (size_t)suffix.length);
    }
    return PAW_FALSE;
}

typedef struct {
	uint64_t s;
} Mix64State;

// From https://en.wikipedia.org/wiki/Xorshift#Initialization
static uint64_t mix64(uint64_t x) {
	x = (x ^ (x >> 30)) * 0xBF58476D1CE4E5B9ULL;
	x = (x ^ (x >> 27)) * 0x94D049BB133111EBULL;
	return x ^ (x >> 31);
}

// From http://www.cse.yorku.ca/~oz/hash.html
uint64_t paw_builtin_hash_bytes(paw_Char const *bytes, size_t length, uint64_t hash)
{
#define N sizeof(hash)

    uint64_t x, h = mix64(hash ^ length);
    while (length >= N) {
        memcpy(&x, bytes, N);

        h ^= mix64(x);
        h = mix64(h);

        bytes += N;
        length -= N;
    }
    memcpy(&x, bytes, length);
    h ^= mix64(x);
    return mix64(h);

#undef N
}

int64_t paw_builtin_rawcmp(paw_Char const *lhs, size_t lhs_length, paw_Char const *rhs, size_t rhs_length)
{
    size_t const n = PAW_MIN(lhs_length, rhs_length);
    int const r = n == 0 ? 0 : memcmp(lhs, rhs, n);
    return r != 0 ? r : (int64_t)lhs_length - (int64_t)rhs_length;
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
void *paw_ptr_memcpy(void *dest, void *src, paw_Int size)
{
    return memcpy(dest, src, (size_t)size);
}

// fn memmove(dest: *char, src: *char, size: int) -> *char
void *paw_ptr_memmove(void *dest, void *src, paw_Int size)
{
    return memmove(dest, src, (size_t)size);
}

// fn memset(ptr: *char, value: char, size: int) -> *char
void *paw_ptr_memset(void *ptr, char value, paw_Int size)
{
    return memset(ptr, value, (size_t)size);
}

// fn memcmp(lhs: *char, rhs: *char, size: int) -> int
int paw_ptr_memcmp(void *lhs, void *rhs, paw_Int size)
{
    return memcmp(lhs, rhs, (size_t)size);
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

