// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_RT_H
#define PAW_RT_H

#include "env.h"
#include "paw.h"

void pawR_init(paw_Env *P);

void pawR_error(paw_Env *P, int status, char const *fmt, ...);
void pawR_field_error(paw_Env *P, Value field);
void pawR_name_error(paw_Env *P, Value name);

void pawR_execute(paw_Env *P, CallFrame *cf);

Tuple *pawR_new_tuple(paw_Env *P, CallFrame *cf, Value *ra, int b);
void pawR_tuple_get(CallFrame *cf, Value *ra, Value const *rb, int index);
void pawR_tuple_set(CallFrame *cf, Value *ra, int index, Value const *rb);

void pawR_str_length(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb);
void pawR_str_concat(paw_Env *P, CallFrame *cf, int b);
void pawR_str_get(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);
void pawR_str_getn(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);

Tuple *pawR_new_list(paw_Env *P, CallFrame *cf, Value *ra, int b, int c);
void pawR_list_length(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb);
void pawR_list_concat(paw_Env *P, CallFrame *cf, int b);
void pawR_list_get(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);
void pawR_list_getn(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc, Value *temp);
void pawR_list_getp(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);
void pawR_list_set(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);
void pawR_list_setn(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc, Value *temp);

Tuple *pawR_new_map(paw_Env *P, CallFrame *cf, Value *ra, int b, int c);
void pawR_map_length(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb);
int pawR_map_get(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);
int pawR_map_getp(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);
void pawR_map_newp(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);
void pawR_map_set(paw_Env *P, CallFrame *cf, Value *ra, Value const *rb, Value const *rc);

void pawR_close_upvalues(paw_Env *P, StackPtr const top);

#define I2U(i) (CAST(uint64_t, i))
#define U2I(u) PAW_CAST_INT(u)

// Generate code for int operators
// Casts to unsigned to avoid UB (signed integer overflow). Requires
// 2's complement integer representation to work properly.
#define I_UNOP(a, op) U2I(op I2U(a))
#define I_BINOP(a, b, op) U2I(I2U(a) op I2U(b))

#define INT_UNARY_OP(r, val, op) \
    V_SET_INT(r, I_UNOP(V_INT(val), op))

#define FLOAT_UNARY_OP(r, val, op) \
    V_SET_FLOAT(r, op V_FLOAT(val))

#define INT_COMPARISON(r, x, y, op) \
    V_SET_BOOL(r, V_INT(x) op V_INT(y))

#define INT_BINARY_OP(r, x, y, op) \
    V_SET_INT(r, I_BINOP(V_INT(x), V_INT(y), op))

#define FLOAT_COMPARISON(r, x, y, op) \
    V_SET_BOOL(r, V_FLOAT(x) op V_FLOAT(y))

#define FLOAT_BINARY_OP(r, x, y, op) \
    V_SET_FLOAT(r, V_FLOAT(x) op V_FLOAT(y))

#define STR_COMPARISON(r, x, y, op) \
    V_SET_BOOL(r, pawS_cmp(V_STRING(x), V_STRING(y)) op 0)

#endif // PAW_RT_H
