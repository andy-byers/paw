// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "paw.h"

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DANGLING ((void *)16)

_Noreturn void paw_panic_(paw_Slice message)
{
    fwrite(message.start, 1, (paw_Usize)message.length, stderr);
    exit(EXIT_FAILURE);
}

static int parse_float(paw_Char const *text, double *result_ptr)
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
    double const f = strtod(text, NULL);
    *result_ptr = is_negative ? -f : f;
    return 0;

#undef IS_FP
#undef IS_DIGIT
}

// fn parse_float(self) -> Option<float64>
paw_Int32 paw_internal_parse_float(paw_Str self, paw_Float64 *out)
{
    return parse_float(self.text, out);
}

static paw_Char const *find_substr(paw_Char const *str, paw_Usize nstr, paw_Char const *sub, paw_Usize nsub)
{
    if (nsub == 0)
        return str;

    paw_Char const *end = str + nstr;
    while ((str = strchr(str, sub[0]))) {
        if ((ptrdiff_t)nsub <= end - str
                && memcmp(str, sub, (paw_Usize)nsub) == 0)
            return str;
        str += nsub;
    }

    return NULL;
}

// fn find(self, target: str) -> Option<int>
paw_Option_Int64 paw_str_find(paw_Str self, paw_Str target)
{
    paw_Char const *result = find_substr(
        self.text, self.length,
        target.text, target.length);
    if (result != NULL) {
        return paw_Option_Int64_some(result - self.text);
    } else {
        return paw_Option_Int64_none();
    }
}

void paw_builtin_check_bounds(paw_Usize index, paw_Usize length)
{
    if (index >= length) {
        char buffer[200]; // space for error message
        int const n = snprintf(buffer, PAW_COUNTOF(buffer),
                "index %llu out of bounds for sequence of length %llu\n",
                (unsigned long long)index, (unsigned long long)length);
        PAW_ASSERT((paw_Usize)n < PAW_COUNTOF(buffer));
        paw_panic_((paw_Slice){
                .start = buffer,
                .length = (paw_Usize)n,
            });
    }
}

paw_Int64 paw_fmt_write_float(double value, paw_Int64 precision, char *output, paw_Usize output_len)
{
    return snprintf(output, output_len, "%.*g", (int)precision, value);
}

