// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_COMPILER_STATS_H
#define PAW_COMPILER_STATS_H

#include "env.h"

struct Statistic {
    char const *name;
    unsigned long long value;
};

struct Compiler;
struct Statistics;

struct Statistic *stats_new(paw_Env *P, struct Statistics *stats, char const *name);

#endif // PAW_COMPILER_STATS_H
