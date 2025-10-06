// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "stats.h"
#include "compile.h"

struct Statistic *pawStats_new(paw_Env *P, Statistics *stats, char const *name)
{
    struct Statistic *stat = pawK_pool_alloc(P->pool, NULL, 0, sizeof(struct Statistic));
    *stat = (struct Statistic){.name = name};
    Statistics_push(P, stats, stat);
    return stat;
}
