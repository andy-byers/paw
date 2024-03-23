// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_META_H
#define PAW_META_H

#include "opcode.h"
#include "paw.h"
#include "value.h"

// paw Metamethod descriptions:
//
// Unary operators:
//
//      Expression | Metamethod call
//     ------------|-----------------
//      <op> x     | x.__<unop>()
//
// Where <op> = <unop>
//       -    = __neg
//       ~    = __bnot
//       !    = __not
//       #    = __len
//
// Binary arithmetic operators:
//
//      Expression | Metamethod call
//     ------------|-----------------
//      x <op> y   | x.__<binop>(y)
//      y <op> x   | x.__<binop>r(y)
//
// Where <op> = <binop>
//       +    = __add
//       -    = __sub
//       *    = __mul
//       /    = __div
//       //   = __idiv
//       %    = __mod
//       &    = __band
//       |    = __bor
//       ^    = __bxor
//
// Binary relational operators:
//
//      Expression  | Metamethod call
//     -------------|-----------------
//      x < y       | x.__lt(y)
//      x <= y      | x.__le(y)
//      x > y       | x.__gt(y)
//      x >= y      | x.__ge(y)
//      y < x       | x.__ge(y)
//      y <= x      | x.__gt(y)
//      y > x       | x.__le(y)
//      y >= x      | x.__lt(y)
//
// Binary equality operators:
//
//      Expression | Metamethod call
//     ------------|-----------------
//      x == y     | x.__eq(y)
//      y == x     | x.__eq(y)
//      x != y     | !x.__eq(y)
//      y != x     | !x.__eq(y)

#define mm_has_r(op) ((op) >= META2)
#define mm_get_r(op) ((op) + METAR - META2)

const char *pawT_name(Op op);
Value *pawT_get_meta_(paw_Env *P, Op op, Value obj);

static inline Value *pawT_get_meta(paw_Env *P, Op op, Value obj)
{
    if (has_meta(obj)) {
        return pawT_get_meta_(P, op, obj);
    }
    return PAW_FALSE;
}

#endif // PAW_META_H
