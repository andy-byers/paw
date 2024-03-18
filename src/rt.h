// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_VM_H
#define PAW_VM_H

#include "env.h"
#include "paw.h"

// Immediate operands:
#define Ib() (pc += 1, (int)pc[-1])
#define Iw() (pc += 2, (int)(pc[-2] | pc[-1] << 8))

typedef enum BuiltinKind {
    BUILTIN_FUNCTION,
    BUILTIN_METHOD,
    N_BUILTIN_KINDS
} BuiltinKind;

void pawR_to_integer(paw_Env *P);
void pawR_to_float(paw_Env *P);
void pawR_to_string(paw_Env *P);

void pawR_length(paw_Env *P);
void pawR_equals(paw_Env *P);
void pawR_arith(paw_Env *P, Op op);
void pawR_compare(paw_Env *P, Op op);

void pawR_getattr(paw_Env *P);
void pawR_getitem(paw_Env *P);
void pawR_setattr(paw_Env *P);
void pawR_setitem(paw_Env *P);

int pawR_getattr_raw(paw_Env *P, paw_Bool fallback);
int pawR_getitem_raw(paw_Env *P, paw_Bool fallback);
void pawR_setattr_raw(paw_Env *P);
void pawR_setitem_raw(paw_Env *P);

int pawR_read_global(paw_Env *P, Value name);
void pawR_write_global(paw_Env *P, Value name, paw_Bool create);

void pawR_execute(paw_Env *P, CallFrame *cf);
void pawR_literal_array(paw_Env *P, int n);
void pawR_literal_map(paw_Env *P, int n);

const char *pawR_opcode_name(Op op);
void pawR_close_upvalues(paw_Env *P, const StackPtr top);

#endif // PAW_VM_H
