// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_RT_H
#define PAW_RT_H

#include "env.h"
#include "paw.h"

void pawR_init(paw_Env *P);

void pawR_cmpi(paw_Env *P, enum CmpOp);
void pawR_cmpf(paw_Env *P, enum CmpOp);
void pawR_cmps(paw_Env *P, enum CmpOp);

void pawR_arithi1(paw_Env *P, enum ArithOp1 op);
void pawR_arithi2(paw_Env *P, enum ArithOp2 op);
void pawR_arithf1(paw_Env *P, enum ArithOp1 op);
void pawR_arithf2(paw_Env *P, enum ArithOp2 op);
void pawR_bitwi1(paw_Env *P, enum BitwOp1 op);
void pawR_bitwi2(paw_Env *P, enum BitwOp2 op);

void pawR_boolop(paw_Env *P, enum BoolOp op);
void pawR_strop(paw_Env *P, enum StrOp op);
void pawR_listop(paw_Env *P, enum ListOp op);
void pawR_mapop(paw_Env *P, enum MapOp op);

void pawR_error(paw_Env *P, int status, const char *fmt, ...);
void pawR_field_error(paw_Env *P, Value field);
void pawR_name_error(paw_Env *P, Value name);

void pawR_getfield(paw_Env *P, int index);
void pawR_setfield(paw_Env *P, int index);

void pawR_getelem_list(paw_Env *P);
void pawR_setelem_list(paw_Env *P);
void pawR_getelem_map(paw_Env *P);
void pawR_setelem_map(paw_Env *P);

void pawR_execute(paw_Env *P, CallFrame *cf);
void pawR_literal_list(paw_Env *P, int n);
void pawR_literal_map(paw_Env *P, int n);

void pawR_close_upvalues(paw_Env *P, const StackPtr top);

#endif // PAW_RT_H
