// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_API_H
#define PAW_API_H

#include "paw.h"
#include "util.h"

// Public API checks based off those in Lua

#if defined(PAW_USE_API_CHECK)
# include <assert.h>
# define API_CHECK(P, e, msg) assert(e)
#else
# define API_CHECK(P, e, msg) ((void)(P), paw_assert((e) && msg))
#endif

#define API_INCR_TOP(P, n) \
    ((P)->top.p += (n), API_CHECK(P, (P)->top.p <= (P)->cf->top.p, "stack overflow"))

#define API_CHECK_PUSH(P, n) \
       API_CHECK(P, (n) < ((P)->top.p - (P)->cf->base.p), \
                         "stack is too large for push")

#define API_CHECK_POP(P, n) \
    API_CHECK(P, (n) < (P)->top.p - (P)->cf->base.p, \
              "stack is not large enough for pop")

#endif // PAW_API_H
