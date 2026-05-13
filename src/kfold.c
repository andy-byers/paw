// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// kfold.c: Perform constant folding
//

#include "compile.h"
#include "error.h"
#include "mir.h"
#include <math.h>

#define KFOLD_ERROR(C_, Kind_, Modname_, ...) THROW_ERROR(C_, Kind_, .modname = Modname_, __VA_ARGS__)
#define DIVIDE_BY_0(C_, Modname_, Span_) KFOLD_ERROR(C_, ConstantDivideByZero, Modname_, Span_);
#define SHIFT_BY_NEGATIVE(C_, Modname_, Span_) KFOLD_ERROR(C_, ConstantNegativeShiftCount, Modname_, Span_);
#define IDIVMOD_OVERFLOWS(Left_, Right_) (V_INT(Left_) == PAW_INT_MIN && V_INT(Right_) == PAW_INT_C(-1))

#define INT2U(I_) PAW_CAST_UINT(I_)
#define U2INT(U_) PAW_CAST_INT(U_)

// Generate code for integer operators
// Casts to unsigned to avoid UB (signed integer overflow). Requires
// 2's complement integer representation to work properly.
#define I_UNOP(I_, Op_) U2INT(Op_ INT2U(I_))
#define I_BINOP(A_, B_, Op_) U2INT(INT2U(A_) Op_ INT2U(B_))

#define INT_UNARY_OP(Result_, Value_, Op_) \
    V_SET_INT(Result_, I_UNOP(V_INT(Value_), Op_))

#define FLOAT_UNARY_OP(Result_, Value_, Op_) \
    V_SET_FLOAT(Result_, Op_ V_FLOAT(Value_))

#define CHAR_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, V_CHAR(X_) Op_ V_CHAR(Y_))

#define INT_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, V_INT(X_) Op_ V_INT(Y_))

#define FLOAT_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, V_FLOAT(X_) Op_ V_FLOAT(Y_))

#define STR_COMPARISON(Result_, X_, Y_, Op_) \
    V_SET_BOOL(Result_, pawS_cmp(V_STR(X_), V_STR(Y_)) Op_ 0)

#define INT_BINARY_OP(Result_, X_, Y_, Op_) \
    V_SET_INT(Result_, I_BINOP(V_INT(X_), V_INT(Y_), Op_))

#define FLOAT_BINARY_OP(Result_, X_, Y_, Op_) \
    V_SET_FLOAT(Result_, V_FLOAT(X_) Op_ V_FLOAT(Y_))

paw_Bool pawP_fold_unary_op(struct Compiler *C, enum MirUnaryOpKind op, Value v, Value *pr)
{
    PAW_UNUSED(C);
    switch (op) {
        case MIR_UNARY_INEG:
            INT_UNARY_OP(pr, v, -);
            break;
        case MIR_UNARY_IBITNOT:
            INT_UNARY_OP(pr, v, ~);
            break;
        case MIR_UNARY_FNEG:
            FLOAT_UNARY_OP(pr, v, -);
            break;
        case MIR_UNARY_NOT:
            V_SET_INT(pr, !V_TRUE(v));
            break;
        default:
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

paw_Bool pawP_fold_binary_op(struct Compiler *C, Str const *modname, struct SourceSpan span, enum MirBinaryOpKind op, Value x, Value y, Value *pr)
{
    switch (op) {
        case MIR_BINARY_CEQ:
            CHAR_COMPARISON(pr, x, y, ==);
            break;
        case MIR_BINARY_IEQ:
            INT_COMPARISON(pr, x, y, ==);
            break;
        case MIR_BINARY_STREQ:
            STR_COMPARISON(pr, x, y, ==);
            break;
        case MIR_BINARY_FEQ:
            FLOAT_COMPARISON(pr, x, y, ==);
            break;
        case MIR_BINARY_CNE:
            CHAR_COMPARISON(pr, x, y, !=);
            break;
        case MIR_BINARY_INE:
            INT_COMPARISON(pr, x, y, !=);
            break;
        case MIR_BINARY_FNE:
            FLOAT_COMPARISON(pr, x, y, !=);
            break;
        case MIR_BINARY_STRNE:
            STR_COMPARISON(pr, x, y, !=);
            break;
        case MIR_BINARY_CLT:
            CHAR_COMPARISON(pr, x, y, <);
            break;
        case MIR_BINARY_ILT:
            INT_COMPARISON(pr, x, y, <);
            break;
        case MIR_BINARY_FLT:
            FLOAT_COMPARISON(pr, x, y, <);
            break;
        case MIR_BINARY_STRLT:
            STR_COMPARISON(pr, x, y, <);
            break;
        case MIR_BINARY_CLE:
            CHAR_COMPARISON(pr, x, y, <=);
            break;
        case MIR_BINARY_ILE:
            INT_COMPARISON(pr, x, y, <=);
            break;
        case MIR_BINARY_FLE:
            FLOAT_COMPARISON(pr, x, y, <=);
            break;
        case MIR_BINARY_STRLE:
            STR_COMPARISON(pr, x, y, <=);
            break;
        case MIR_BINARY_IADD:
            INT_BINARY_OP(pr, x, y, +);
            break;
        case MIR_BINARY_FADD:
            FLOAT_BINARY_OP(pr, x, y, +);
            break;
        case MIR_BINARY_ISUB:
            INT_BINARY_OP(pr, x, y, -);
            break;
        case MIR_BINARY_FSUB:
            FLOAT_BINARY_OP(pr, x, y, -);
            break;
        case MIR_BINARY_IMUL:
            INT_BINARY_OP(pr, x, y, *);
            break;
        case MIR_BINARY_FMUL:
            FLOAT_BINARY_OP(pr, x, y, *);
            break;
        case MIR_BINARY_IDIV:
            if (V_INT(y) == 0)
                DIVIDE_BY_0(C, modname, span);
            if (IDIVMOD_OVERFLOWS(x, y)) {
                V_SET_INT(pr, 0);
            } else {
                V_SET_INT(pr, V_INT(x) / V_INT(y));
            }
            break;
        case MIR_BINARY_FDIV:
            if (V_FLOAT(y) == 0.0)
                DIVIDE_BY_0(C, modname, span);
            FLOAT_BINARY_OP(pr, x, y, /);
            break;
        case MIR_BINARY_IMOD:
            if (V_INT(y) == 0)
                DIVIDE_BY_0(C, modname, span);
            if (IDIVMOD_OVERFLOWS(x, y)) {
                V_SET_INT(pr, 0);
            } else {
                V_SET_INT(pr, V_INT(x) % V_INT(y));
            }
            break;
        case MIR_BINARY_FMOD:
            if (V_FLOAT(y) == 0.0)
                DIVIDE_BY_0(C, modname, span);
            V_SET_FLOAT(pr, fmod(V_FLOAT(x), V_FLOAT(y)));
            break;
        case MIR_BINARY_IBITXOR:
            INT_BINARY_OP(pr, x, y, ^);
            break;
        case MIR_BINARY_IBITAND:
            INT_BINARY_OP(pr, x, y, &);
            break;
        case MIR_BINARY_IBITOR:
            INT_BINARY_OP(pr, x, y, |);
            break;
        case MIR_BINARY_ISHL: {
            paw_Int n = V_INT(y);
            if (n < 0) {
                SHIFT_BY_NEGATIVE(C, modname, span);
            } else if (n > 0) {
                n = PAW_MIN(n, U2INT(sizeof(x) * 8 - 1));
                V_SET_INT(pr, U2INT(V_UINT(x) << n));
            } else {
                *pr = x;
            }
            break;
        }
        case MIR_BINARY_ISHR: {
            paw_Int n = V_INT(y);
            if (n < 0) {
                SHIFT_BY_NEGATIVE(C, modname, span);
            } else if (n > 0) {
                n = PAW_MIN(n, U2INT(sizeof(x) * 8 - 1));
                V_SET_INT(pr, V_INT(x) >> n);
            } else {
                *pr = x;
            }
            break;
        }
    }
    return PAW_TRUE;
}

