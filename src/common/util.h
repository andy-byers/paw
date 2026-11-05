// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_COMMON_UTIL_H
#define PAW_COMMON_UTIL_H

#include "compiler/core.h"

#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <string.h>

#define paw_assert assert

#define PAW_UNUSED(x) ((void)(x))
#define PAW_LENGTHOF(s) (sizeof(s) - 1)
#define PAW_ALIGNOF(x) _Alignof(x)
#define PAW_COUNTOF(a) (sizeof(a) / sizeof((a)[0]))
#define PAW_MIN(x, y) ((x) < (y) ? (x) : (y))
#define PAW_MAX(x, y) ((x) > (y) ? (x) : (y))
#define PAW_CLAMP(v, x, y) PAW_MIN(PAW_MAX(v, x), y)
#define PAW_ROUND_UP(Value_, Pow2) ((Value_) + (-(Value_) & ((Pow2) - 1)))
#define PAW_IS_ALIGNED(p) (!(CAST_UPTR(p) & (PAW_ALIGN - 1)))

#define CHECK_EXP(c, e) (paw_assert(c), e)
#define CAST(t, x) ((t)(x))
#define CAST_SIZE(x) CAST(size_t, x)
#define CAST_UPTR(x) CAST(uintptr_t, x)
#define ERASE_TYPE(p) CAST(void *, p)
#define BUMP_PTR(p, n) ERASE_TYPE(CAST_UPTR(p) + (n))

// NOTE: Be weary of integer types that contain padding bits, i.e. standard integer
//       types excluding (un)signed char. Comparing such types with this function
//       (or "memcmp" in general) is not portable since the padding bits have
//       unspecified values (C11 section 6.2.6.2, item 5).
inline static int paw_raw_cmp(void const *x, size_t nx, void const *y, size_t ny)
{
    paw_assert(x != NULL && y != NULL);
    size_t const min = PAW_MIN(nx, ny);
    int const r = memcmp(x, y, min);
    if (r == 0) {
        if (nx < ny) {
            return -1;
        } else if (nx > ny) {
            return 1;
        }
    }
    return r;
}

extern paw_Uint8 const kCharClassTable[256];
extern paw_Uint8 const kHexValueTable[256];


typedef paw_Uint64 HashValue;

HashValue hash_uint64(paw_Uint64 u);

static HashValue hash_pointer(void const *p)
{
    return hash_uint64((paw_Uint64)p);
}

// From https://stackoverflow.com/questions/8513911
static paw_Uint64 hash_combine(paw_Uint64 seed, paw_Uint64 v)
{
    // TODO: versions for other sizes
    paw_Uint64 const MUL = 0x9DDFEA08EB382D69ULL;
    paw_Uint64 a = (v ^ seed) * MUL;
    a ^= (a >> 47);
    paw_Uint64 b = (seed ^ a) * MUL;
    b ^= (b >> 47);
    return b * MUL;
}

#endif // PAW_COMMON_UTIL_H
