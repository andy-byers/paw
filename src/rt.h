// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_RT_H
#define PAW_RT_H

#include "env.h"
#include "paw.h"

void pawR_init(paw_Env *P);

void pawR_error(paw_Env *P, int status, const char *fmt, ...);
void pawR_field_error(paw_Env *P, Value field);
void pawR_name_error(paw_Env *P, Value name);

void pawR_execute(paw_Env *P, CallFrame *cf);

Tuple *pawR_new_tuple(paw_Env *P, CallFrame *cf, Value *ra, int b);
void pawR_tuple_get(CallFrame *cf, Value *ra, const Value *rb, int index);
void pawR_tuple_set(CallFrame *cf, Value *ra, int index, const Value *rb);

void pawR_str_length(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb);
void pawR_str_concat(paw_Env *P, CallFrame *cf, int b);
void pawR_str_get(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc);
void pawR_str_getn(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc);

Tuple *pawR_new_list(paw_Env *P, CallFrame *cf, Value *ra, int b);
void pawR_list_length(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb);
void pawR_list_concat(paw_Env *P, CallFrame *cf, int b);
void pawR_list_get(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc);
void pawR_list_getn(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc, Value *temp);
void pawR_list_set(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc);
void pawR_list_setn(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc, Value *temp);

Map *pawR_new_map(paw_Env *P, CallFrame *cf, Value *ra, int b);
void pawR_map_length(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb);
int pawR_map_get(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc);
void pawR_map_set(paw_Env *P, CallFrame *cf, Value *ra, const Value *rb, const Value *rc);

void pawR_close_upvalues(paw_Env *P, const StackPtr top);

#endif // PAW_RT_H
