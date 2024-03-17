// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef SHELL_TEST_H
#define SHELL_TEST_H

#include "util.h"
#include "xmalloc.h"
#include <stdio.h>
#include <stdlib.h>

#define CHECK(expr)                                                          \
    do {                                                                     \
        if (!(expr)) {                                                       \
            fprintf(stderr, "error in %s on line %d\n", __func__, __LINE__); \
            abort();                                                         \
        }                                                                    \
    } while (0)

#endif // SHELL_TEST_H
