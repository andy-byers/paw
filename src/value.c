// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdio.h>
#include <stdlib.h>

#include "value.h"


static int int_to_str(paw_Int64 value, char *out, size_t out_len)
{
    char temp[64];
    paw_Bool const negative = value < 0;
    char *ptr = temp + PAW_COUNTOF(temp) - 1;

    // Don't call llabs(PAW_INT64_MIN). The result is undefined on 2s
    // complement systems.
    paw_Uint64 u = value == PAW_INT64_MIN
                     ? UINT64_C(1) << 63
                     : CAST(paw_Uint64, llabs(value));
    size_t len = 0;
    do {
        *ptr-- = (char)(u % 10) + '0';
        u /= 10;
        ++len;
    } while (u);
    if (negative) {
        *ptr = '-';
        ++len;
    } else {
        ++ptr;
    }
    if (len < out_len) {
        for (size_t i = 0; i < len; ++i)
            out[i] = ptr[i];
        out[len] = '\0';
        return (int)len;
    }
    return -1;
}

int pawV_int_to_str(paw_Int64 value, char *out, size_t out_len)
{
    char temp[64];
    paw_Bool const negative = value < 0;
    char *ptr = temp + PAW_COUNTOF(temp) - 1;

    // Don't call llabs(PAW_INT64_MIN). The result is undefined on 2s
    // complement systems.
    paw_Uint64 u = value == PAW_INT64_MIN
                     ? UINT64_C(1) << 63
                     : CAST(paw_Uint64, llabs(value));
    size_t len = 0;
    do {
        *ptr-- = (char)(u % 10) + '0';
        u /= 10;
        ++len;
    } while (u);
    if (negative) {
        *ptr = '-';
        ++len;
    } else {
        ++ptr;
    }
    if (len < out_len) {
        for (size_t i = 0; i < len; ++i)
            out[i] = ptr[i];
        out[len] = '\0';
        return (int)len;
    }
    return -1;
}

int pawV_float_to_str(paw_Float64 value, char *out, size_t out_len)
{
    return snprintf(out, out_len, "%.*g", 17, value);
}
