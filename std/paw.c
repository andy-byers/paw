// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "paw.h"
#include "str.h"

#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void paw_prelude_println(void *env, paw_Str message)
{
    paw_Str with_newline;
    {
        struct StringBuilder sb;
        sb_init(&sb);
        sb_add_str(&sb, message);
        sb_add_char(&sb, '\n');
        with_newline = sb_create_str(&sb);
        sb_uninit(&sb);
    }

    paw_prelude_print(env, with_newline);
}

void paw_prelude_print(void *env, paw_Str message)
{
    PAW_UNUSED(env);
    fwrite(message->text, 1, (size_t)message->length, stdout);
}

void paw_prelude_assert(void *env, paw_Bool cond)
{
    if (!cond) {
        paw_Str message = NEW_LITERAL("assertion failed");
        paw_prelude_panic(env, message);
    }
}

_Noreturn void paw_prelude_panic(void *env, paw_Str message)
{
    paw_prelude_println(env, message);
    exit(EXIT_FAILURE);
}

paw_Str paw_prelude_char_to_str(void *env, paw_Char self)
{
    PAW_UNUSED(env);
    return new_str(&self, 1);
}

paw_Str paw_prelude_int_to_str(void *env, paw_Int self)
{
    PAW_UNUSED(env);
    paw_Char temp[32];
    paw_Bool const negative = self < 0;
    paw_Char *end = temp + PAW_COUNTOF(temp);
    paw_Char *ptr = end - 1;

    // Don't call llabs(PAW_INT_MIN). The result is undefined on 2s complement
    // systems.
    uint64_t u = self == PAW_INT_MIN
                     ? UINT64_C(1) << 63
                     : (uint64_t)llabs(self);
    do {
        *ptr-- = (paw_Char)(u % 10 + '0');
        u /= 10;
    } while (u);

    if (negative) {
        *ptr = '-';
    } else {
        ++ptr;
    }
    return new_str(ptr, end - ptr);
}

paw_Int paw_prelude_float_hash(void *env, paw_Float self)
{
    PAW_UNUSED(env);

    paw_Int i;
    _Static_assert(sizeof self == sizeof i, "");
    memcpy(&i, &self, sizeof i);

    return i;
}

paw_Str paw_prelude_float_to_str(void *env, paw_Float self)
{
    PAW_UNUSED(env);
    paw_Char temp[32];
    int const n = snprintf(temp, PAW_COUNTOF(temp), "%.*g", 17, self);
    return new_str(temp, n);
}

// pub fn parse_int(self) -> Option<int>;
paw_Option_Int paw_prelude_str_parse_int(void *env, paw_Str self)
{
    return paw_prelude_str_parse_int_radix(env, self, 10);
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

// pub fn parse_int_radix(self, base: int) -> Option<int>;
paw_Option_Int paw_prelude_str_parse_int_radix(void *env, paw_Str self, paw_Int base)
{
    PAW_UNUSED(env);
    paw_Int result;
    if (parse_int_radix(self->text, self->length, base, &result) == 0) {
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

// pub fn parse_float(self) -> Option<float>;
paw_Option_Float paw_prelude_str_parse_float(void *env, paw_Str self)
{
    PAW_UNUSED(env);
    paw_Float result;
    if (parse_float(self->text, &result) == 0) {
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

// pub fn split(self, sep: str) -> [str]
paw_List_Str paw_prelude_str_split(void *env, paw_Str self, paw_Str sep)
{
#define LIST_PUSH_STR _PN4list4ListIsE4push
    void LIST_PUSH_STR(void *, paw_List_Str, paw_Str);

    if (sep->length == 0)
        paw_prelude_panic(env, NEW_LITERAL("empty separator"));

    size_t const INITIAL_CAPACITY = 4;
    paw_List_Str list = PAW_MALLOC(sizeof *list);
    list->data = PAW_MALLOC(INITIAL_CAPACITY * sizeof *list->data);
    list->capacity = INITIAL_CAPACITY;
    list->length = 0;

    int num_parts = 0;
    paw_Char const *part;
    paw_Int rest_length = self->length;
    paw_Char const *rest_text = self->text;
    while ((part = find_substr(rest_text, rest_length, sep->text, sep->length))) {
        if (num_parts == INT_MAX)
            paw_prelude_panic(env, NEW_LITERAL("too many substrings"));
        paw_Int const n = part - rest_text;
        LIST_PUSH_STR(NULL, list, new_str(rest_text, n));
        part += sep->length; // skip separator
        rest_text = part;
        rest_length -= n;
        ++num_parts;
    }
    char const *end = self->text + self->length; // add the rest
    LIST_PUSH_STR(NULL, list, new_str(rest_text, end - rest_text));

    return list;

#undef LIST_PUSH_STR
}

// pub fn join(self, parts: [str]) -> str;
paw_Str paw_prelude_str_join(void *env, paw_Str self, paw_List_Str parts)
{
    PAW_UNUSED(env);
    paw_Str joined;
    {
        struct StringBuilder sb;
        sb_init(&sb);
        for (paw_Int i = 0; i < parts->length; ++i) {
            sb_add_str(&sb, parts->data[i]);
            if (i < parts->length - 1)
                sb_add_str(&sb, self);
        }
        joined = sb_create_str(&sb);
        sb_uninit(&sb);
    }
    return joined;
}

// pub fn find(self, target: str) -> Option<int>;
paw_Option_Int paw_prelude_str_find(void *env, paw_Str self, paw_Str target)
{
    PAW_UNUSED(env);
    paw_Char const *result = find_substr(
        self->text, self->length,
        target->text, target->length);
    if (result != NULL) {
        return paw_Option_Int_some(result - self->text);
    } else {
        return paw_Option_Int_none();
    }
}

// pub fn starts_with(self, prefix: str) -> bool;
paw_Bool paw_prelude_str_starts_with(void *env, paw_Str self, paw_Str prefix)
{
    PAW_UNUSED(env);
    return self->length >= prefix->length
        && 0 == memcmp(prefix->text, self->text, (size_t)prefix->length);
}

// pub fn ends_with(self, suffix: str) -> bool;
paw_Bool paw_prelude_str_ends_with(void *env, paw_Str self, paw_Str suffix)
{
    PAW_UNUSED(env);
    if (self->length >= suffix->length) {
        paw_Char const *ptr = self->text + self->length - suffix->length;
        return 0 == memcmp(suffix->text, ptr, (size_t)suffix->length);
    }
    return PAW_FALSE;
}

// pub fn hash(self) -> int;
paw_Int paw_prelude_str_hash(void *env, paw_Str self)
{
    // TODO: i32 => i64
    PAW_UNUSED(env);
    return self->hash;
}

#define LISTC_PUSH_CHAR _PN4list4ListIcE4push
void LISTC_PUSH_CHAR(void *, paw_List_Char, paw_Char);

paw_str_builder_Builder paw_str_builder_Builder_append_char(void *env, paw_str_builder_Builder b, paw_Char value)
{
    PAW_UNUSED(env);
    LISTC_PUSH_CHAR(NULL, b.buf, value);
    return b;
}

paw_str_builder_Builder paw_str_builder_Builder_append_str(void *env, paw_str_builder_Builder b, paw_Str value)
{
    PAW_UNUSED(env);
    for (paw_Int i = 0; i < value->length; ++i)
        LISTC_PUSH_CHAR(NULL, b.buf, value->text[i]);
    return b;
}

paw_Str paw_str_builder_Builder_string(void *env, paw_str_builder_Builder b)
{
    PAW_UNUSED(env);
    return new_str(b.buf->data, b.buf->length);
}

// From http://www.cse.yorku.ca/~oz/hash.html
uint32_t paw_builtin_hash_bytes(paw_Char const *bytes, paw_Int length, uint32_t hash)
{
    for (paw_Int i = 0; i < length; ++i)
        hash = (uint32_t)bytes[i] + (hash << 6) + (hash << 16) - hash;

    return hash;
}

paw_Int paw_builtin_rawcmp(paw_Char const *lhs, paw_Int lhs_length, paw_Char const *rhs, paw_Int rhs_length)
{
    paw_assert(lhs != NULL && rhs != NULL);
    int const r = memcmp(lhs, rhs, (size_t)PAW_MIN(lhs_length, rhs_length));
    return r != 0 ? r : lhs_length - rhs_length;
}

paw_Int paw_builtin_abs_index(paw_Int index, paw_Int length)
{
    paw_Int const result = index < 0 ? index + length : index;

    if (result < 0 || result >= length) {
        paw_Str message;
        {
            struct StringBuilder sb;
            sb_init(&sb);
            sb_add_format(&sb, "index %" PRId64 " out of bounds for"
                    " sequence of length %" PRId64, index, length);
            message = sb_create_str(&sb);
            sb_uninit(&sb);
        }

        paw_prelude_panic(NULL, message);
    }

    return result;
}

