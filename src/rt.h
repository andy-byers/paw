// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_RT_H
#define PAW_RT_H

#include "env.h"
#include "paw.h"

void pawR_init(paw_Env *P);

void pawR_unop(paw_Env *P, UnaryOp unop, paw_Type t);
void pawR_binop(paw_Env *P, BinaryOp binop, paw_Type t);

void pawR_error(paw_Env *P, int status, const char *fmt, ...);
void pawR_field_error(paw_Env *P, Value field);
void pawR_name_error(paw_Env *P, Value name);

void pawR_getfield(paw_Env *P, int index);
void pawR_gettuple(paw_Env *P, int index);
int pawR_getelem(paw_Env *P, paw_Type type);
void pawR_setfield(paw_Env *P, int index);
void pawR_settuple(paw_Env *P, int index);
void pawR_setelem(paw_Env *P, paw_Type type);

void pawR_execute(paw_Env *P, CallFrame *cf);
void pawR_literal_list(paw_Env *P, int n);
void pawR_literal_map(paw_Env *P, int n);

void pawR_close_upvalues(paw_Env *P, const StackPtr top);

static inline void pawR_check_argc(paw_Env *P, int argc, int expect)
{
    if (argc != expect) {
        pawR_error(P, PAW_ERUNTIME, "expected %d argument(s) but found %d",
                   expect, argc);
    }
}

static inline void pawR_check_varargc(paw_Env *P, int argc, int least, int most)
{
    if (argc < least) {
        pawR_error(P, PAW_ERUNTIME,
                   "expected at least %d argument(s) but found %d", least,
                   argc);
    } else if (argc > most) {
        pawR_error(P, PAW_ERUNTIME,
                   "expected at most %d argument(s) but found %d", most, argc);
    }
}

#endif // PAW_RT_H
