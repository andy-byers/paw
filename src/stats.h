// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STATS_H
#define PAW_STATS_H

#include "code.h"

struct Statistic {
    char const *name;
    unsigned long long value;
};

struct Compiler;

struct Statistic *pawStats_new(struct Compiler *C, char const *name);

#endif // PAW_STATS_H
