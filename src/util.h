// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_UTIL_H
#define PAW_UTIL_H

#include "paw.h"
#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>

#define paw_assert assert

#define PAW_UNUSED(x) ((void)(x))
#define PAW_LENGTHOF(s) (sizeof(s) - 1)
#define PAW_ALIGNOF(x) _Alignof(x)
#define PAW_COUNTOF(a) (sizeof(a) / sizeof((a)[0]))
#define PAW_MIN(x, y) ((x) < (y) ? (x) : (y))
#define PAW_MAX(x, y) ((x) > (y) ? (x) : (y))
#define PAW_CLAMP(v, x, y) PAW_MIN(PAW_MAX(v, x), y)
#define PAW_ROUND_UP(n) ((n) + (-(n) & (PAW_ALIGN - 1)))
#define PAW_IS_ALIGNED(p) (!(CAST_UPTR(p) & (PAW_ALIGN - 1)))

#define CHECK_EXP(c, e) (paw_assert(c), e)
#define CAST(t, x) ((t)(x))
#define CAST_SIZE(x) CAST(size_t, x)
#define CAST_UPTR(x) CAST(uintptr_t, x)
#define ERASE_TYPE(p) CAST(void *, p)
#define BUMP_PTR(p, n) ERASE_TYPE(CAST_UPTR(p) + (n))

// Check for inclusion in one of the character classes
#define ISDIGIT(c) (kCharClassTable[(uint8_t)(c)] & 1)
#define ISHEX(c) (kCharClassTable[(uint8_t)(c)] & 2)
#define ISSPACE(c) (kCharClassTable[(uint8_t)(c)] & 4)
#define ISNAME(c) (kCharClassTable[(uint8_t)(c)] & 8)
#define ISNONASCII(c) (kCharClassTable[(uint8_t)(c)] & 16)
#define ISASCIIEND(c) (kCharClassTable[(uint8_t)(c)] & 32)
#define ISNEWLINE(c) ((c) == '\r' || (c) == '\n')

// Get the integer representation of a hex digit
#define HEXVAL(c) (kHexValueTable[(uint8_t)(c)])

static inline int paw_raw_cmp(void *x, size_t nx, void *y, size_t ny)
{
    const size_t min = PAW_MIN(nx, ny);
    const int r = memcmp(x, y, min);
    if (r == 0) {
        if (nx < ny) {
            return -1;
        } else if (nx > ny) {
            return 1;
        }
    }
    return r;
}

extern const uint8_t kCharClassTable[256];
extern const uint8_t kHexValueTable[256];

#endif // PAW_UTIL_H
