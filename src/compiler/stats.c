// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "stats.h"
#include "compiler.h"

struct Statistic *stats_new(paw_Env *P, Statistics *stats, char const *name)
{
    struct Statistic *stat = pool_alloc(P->pool, NULL, 0, sizeof(struct Statistic));
    *stat = (struct Statistic){.name = name};
    Statistics_push(stats, stat);
    return stat;
}
