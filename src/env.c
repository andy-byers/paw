// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "env.h"
#include "api.h"
#include "map.h"
#include "mem.h"

#include <limits.h>

_Noreturn void pawE_error(paw_Env *P, int code, int line, char const *fmt, ...)
{
    if (P->current_errmsg == NULL) {
        Buffer print;
        pawL_init_buffer(P, &print);
        if (line >= 0) {
            paw_assert(P->modname != NULL);
            pawL_add_fstring(P, &print, "%s:%d: ", P->modname->text, line);
        }

        va_list arg;
        va_start(arg, fmt);
        pawL_add_vfstring(P, &print, fmt, arg);
        va_end(arg);

        P->current_errmsg = pawL_buffer_finish(P, &print);
    }
    pawC_throw(P, code);
}

void pawE_init(paw_Env *P)
{
    P->stats = Statistics_new(P);
    P->callbacks = CallbackMap_new(P);

    // Create statistics for tracking compiler memory usage. Main pool statistics
    // must be added after-the-fact, since the main pool itself is used to allocate
    // the "struct Statistic" objects.
    P->pool->st = (struct PoolStats){
                .bytes_alloc = pawStats_new(P, P->stats, "memory.main.bytes_allocated"),
                .bytes_used = pawStats_new(P, P->stats, "memory.main.bytes_used"),
                .num_alloc = pawStats_new(P, P->stats, "memory.main.num_allocations"),
            };
}

void pawE_uninit(paw_Env *P)
{
}

void pawE_register_callback(paw_Env *P, char const *name, paw_Function cb)
{
    Str const *key = pawS_new_str(P, name);
    CallbackMap_insert(P, P->callbacks, key, cb);
}

