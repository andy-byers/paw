// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_GC_H
#define PAW_GC_H

#include "paw.h"
#include "value.h"

#define CHECK_GC(P)                          \
    do {                                     \
        if ((P)->gc_bytes > (P)->gc_limit) { \
            pawG_collect(P);                 \
        }                                    \
    } while (0)

void pawG_init(paw_Env *P);
void pawG_uninit(paw_Env *P);
void pawG_collect(paw_Env *P);
void pawG_fix_object(paw_Env *P, Object *o);
void pawG_add_object(paw_Env *P, Object *o, ValueKind kind);

void pawG_free_object(paw_Env *P, Object *o);

#endif // PAW_GC_H
