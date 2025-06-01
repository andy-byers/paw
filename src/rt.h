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

#define CHAR2U(X_) PAW_CAST_UCHAR(X_)
#define U2CHAR(U_) PAW_CAST_CHAR(U_)
#define INT2U(I_) PAW_CAST_UINT(I_)
#define U2INT(U_) PAW_CAST_INT(U_)

// Generate code for integer operators
// Casts to unsigned to avoid UB (signed integer overflow). Requires
// 2's complement integer representation to work properly.
#define X_UNOP(X_, Op_) U2CHAR(Op_ CHAR2U(X_))
#define X_BINOP(A_, B_, Op_) U2CHAR(CHAR2U(A_) Op_ CHAR2U(B_))
#define I_UNOP(I_, Op_) U2INT(Op_ INT2U(I_))
#define I_BINOP(A_, B_, Op_) U2INT(INT2U(A_) Op_ INT2U(B_))

#define CHAR_UNARY_OP(Result_, Value_, Op_) \
    V_SET_CHAR(Result_, X_UNOP(V_CHAR(Value_), Op_))

#define INT_UNARY_OP(Result_, Value_, Op_) \
    V_SET_INT(Result_, I_UNOP(V_INT(Value_), Op_))

#define FLOAT_UNARY_OP(Result_, Value_, Op_) \
    V_SET_FLOAT(Result_, Op_ V_FLOAT(Value_))

#define CHAR_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, V_CHAR(X_) Op_ V_CHAR(Y_))

#define CHAR_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, V_CHAR(X_) Op_ V_CHAR(Y_))

#define INT_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, V_INT(X_) Op_ V_INT(Y_))

#define FLOAT_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, V_FLOAT(X_) Op_ V_FLOAT(Y_))

#define STR_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, pawS_cmp(V_STR(X_), V_STR(Y_)) Op_ 0)

#define CHAR_BINARY_OP(Result_, X_, Y_, Op_) \
    V_SET_CHAR(Result_, X_BINOP(V_CHAR(X_), V_CHAR(Y_), Op_))

#define INT_BINARY_OP(Result_, X_, Y_, Op_) \
    V_SET_INT(Result_, I_BINOP(V_INT(X_), V_INT(Y_), Op_))

#define FLOAT_BINARY_OP(Result_, X_, Y_, Op_) \
    V_SET_FLOAT(Result_, V_FLOAT(X_) Op_ V_FLOAT(Y_))

#endif // PAW_RT_H
