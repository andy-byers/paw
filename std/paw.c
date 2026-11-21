// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "paw.h"

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define DANGLING ((void *)16)

_Noreturn void paw_panic_(paw_Slice message)
{
    fwrite(message.start, 1, (paw_Usize)message.length, stderr);
    exit(EXIT_FAILURE);
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

