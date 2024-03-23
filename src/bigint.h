// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// Multi-precision integer module. Some code was adapted from micropython.
// Descriptions of most of the algorithms can be found in chapter 14 of the
// Handbook of Applied Cryptography (HAC).
#ifndef PAW_BIGINT_H
#define PAW_BIGINT_H

#include "aux.h"
#include "opcode.h"
#include "paw.h"
#include "value.h"

#define BI_BASE 256
#define BI_MASK (BI_BASE - 1)
#define BI_MSB (1 << (BI_BITS - 1))
#define BI_BITS 8

#define bi_zero(bi) ((bi)->size == 0)

// Number of bytes needed to unpack a paw_Int into base BI_BASE
#define UNPACKED_INT_SIZE ((PAW_INT_WIDTH + BI_BITS - 1) / BI_BITS)

BigInt *pawB_new(paw_Env *P);
BigInt *pawB_copy(paw_Env *P, StackPtr sp, const BigInt *bi, int extra);
void pawB_free(paw_Env *P, BigInt *bi);
void pawB_unop(paw_Env *P, Op op, Value x);
void pawB_arith(paw_Env *P, Op op, Value x, Value y);
void pawB_bitwise(paw_Env *P, Op op, Value x, Value y);
void pawB_to_string(paw_Env *P, const BigInt *bi, paw_Bool caps, const char *prefix, int base);
int pawB_parse(paw_Env *X, const char *p, int base);
paw_Bool pawB_compare(paw_Env *P, Op op, Value lhs, Value rhs);
paw_Bool pawB_equals(Value lhs, Value rhs);

BigInt *pawB_from_int(paw_Env *P, StackPtr sp, paw_Int i);
void pawB_from_float(paw_Env *P, paw_Float f);

paw_Int pawB_get_int(const BigInt *bi);
paw_Int pawB_get_int64(const BigInt *bi, paw_Bool *lossless);
paw_Float pawB_get_float(const BigInt *bi);

#endif // PAW_BIGINT_H
