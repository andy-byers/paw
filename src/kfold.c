// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// kfold.c: Perform constant folding
//

#include "compile.h"
#include "error.h"
#include "mir.h"
#include "rt.h"
#include <math.h>

#define KFOLD_ERROR(C_, Kind_, Modname_, ...) pawErr_##Kind_(C_, Modname_, __VA_ARGS__)
#define DIVIDE_BY_0(C_, Modname_, Loc_) KFOLD_ERROR(C_, constant_divide_by_zero, Modname_, Loc_);
#define SHIFT_BY_NEGATIVE(C_, Modname_, Loc_) KFOLD_ERROR(C_, constant_negative_shift_count, Modname_, Loc_);
#define DIVMOD_OVERFLOWS(Left_, Right_) (V_INT(Left_) == PAW_INT_MIN && V_INT(Right_) == -1)

static void constant_div(struct Compiler *C, String const *modname, struct SourceLoc loc, Value *pr, Value x, Value y, enum BuiltinKind kind)
{
    if (kind == BUILTIN_FLOAT) {
        if (V_FLOAT(y) == 0.0)
            DIVIDE_BY_0(C, modname, loc);
        FLOAT_BINARY_OP(pr, x, y, /);
    } else {
        paw_assert(kind == BUILTIN_INT);
        if (V_INT(y) == 0)
            DIVIDE_BY_0(C, modname, loc);
        INT_BINARY_OP(pr, x, y, /);
    }
}

static void constant_mod(struct Compiler *C, String const *modname, struct SourceLoc loc, Value *pr, Value x, Value y, enum BuiltinKind kind)
{
    if (kind == BUILTIN_FLOAT) {
        if (V_FLOAT(y) == 0.0)
            DIVIDE_BY_0(C, modname, loc);
        V_SET_FLOAT(pr, fmod(V_FLOAT(x), V_FLOAT(y)));
    } else {
        paw_assert(kind == BUILTIN_INT);
        if (V_INT(y) == 0)
            DIVIDE_BY_0(C, modname, loc);
        INT_BINARY_OP(pr, x, y, %);
    }
}

static void str_concat(struct Compiler *C, String const *x, String const *y, Value *pr)
{
    paw_Env *P = ENV(C);

    Buffer b;
    pawL_init_buffer(P, &b);
    pawL_add_nstring(P, &b, x->text, x->length);
    pawL_add_nstring(P, &b, y->text, y->length);
    String *r = pawP_scan_nstring(C, C->strings, b.data, b.size);
    pawL_discard_result(P, &b);

    V_SET_OBJECT(pr, r);
}

paw_Bool pawP_fold_unary_op(struct Compiler *C, enum MirUnaryOpKind op, Value v, Value *pr)
{
    switch (op) {
        case MIR_UNARY_SLENGTH: {
            String const *x = V_STRING(v);
            V_SET_INT(pr, x->length);
            break;
        }
        case MIR_UNARY_INEG:
            INT_UNARY_OP(pr, v, -);
            break;
        case MIR_UNARY_FNEG:
            FLOAT_UNARY_OP(pr, v, -);
            break;
        case MIR_UNARY_INOT:
            INT_UNARY_OP(pr, v, !);
            break;
        case MIR_UNARY_BITNOT:
            INT_UNARY_OP(pr, v, ~);
            break;
        default:
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

paw_Bool pawP_fold_binary_op(struct Compiler *C, String const *modname, struct SourceLoc loc, enum MirBinaryOpKind op, Value x, Value y, Value *pr)
{
    switch (op) {
        case MIR_BINARY_IEQ:
            INT_COMPARISON(pr, x, y, ==);
            break;
        case MIR_BINARY_SEQ:
            STR_COMPARISON(pr, x, y, ==);
            break;
        case MIR_BINARY_FEQ:
            FLOAT_COMPARISON(pr, x, y, ==);
            break;
        case MIR_BINARY_INE:
            INT_COMPARISON(pr, x, y, !=);
            break;
        case MIR_BINARY_FNE:
            FLOAT_COMPARISON(pr, x, y, !=);
            break;
        case MIR_BINARY_SNE:
            STR_COMPARISON(pr, x, y, !=);
            break;
        case MIR_BINARY_ILT:
            INT_COMPARISON(pr, x, y, <);
            break;
        case MIR_BINARY_FLT:
            FLOAT_COMPARISON(pr, x, y, <);
            break;
        case MIR_BINARY_SLT:
            STR_COMPARISON(pr, x, y, <);
            break;
        case MIR_BINARY_ILE:
            INT_COMPARISON(pr, x, y, <=);
            break;
        case MIR_BINARY_FLE:
            FLOAT_COMPARISON(pr, x, y, <=);
            break;
        case MIR_BINARY_SLE:
            STR_COMPARISON(pr, x, y, <=);
            break;
        case MIR_BINARY_IGT:
            INT_COMPARISON(pr, x, y, >);
            break;
        case MIR_BINARY_FGT:
            FLOAT_COMPARISON(pr, x, y, >);
            break;
        case MIR_BINARY_SGT:
            STR_COMPARISON(pr, x, y, >);
            break;
        case MIR_BINARY_IGE:
            INT_COMPARISON(pr, x, y, >=);
            break;
        case MIR_BINARY_FGE:
            FLOAT_COMPARISON(pr, x, y, >=);
            break;
        case MIR_BINARY_SGE:
            STR_COMPARISON(pr, x, y, >=);
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
                DIVIDE_BY_0(C, modname, loc);
            if (DIVMOD_OVERFLOWS(x, y)) {
                V_SET_INT(pr, 0);
            } else {
                V_SET_INT(pr, V_INT(x) / V_INT(y));
            }
            break;
        case MIR_BINARY_FDIV:
            if (V_FLOAT(y) == 0.0)
                DIVIDE_BY_0(C, modname, loc);
            FLOAT_BINARY_OP(pr, x, y, /);
            break;
        case MIR_BINARY_IMOD:
            if (V_INT(y) == 0)
                DIVIDE_BY_0(C, modname, loc);
            if (DIVMOD_OVERFLOWS(x, y)) {
                V_SET_INT(pr, 0);
            } else {
                V_SET_INT(pr, V_INT(x) % V_INT(y));
            }
            break;
        case MIR_BINARY_FMOD:
            if (V_FLOAT(y) == 0.0)
                DIVIDE_BY_0(C, modname, loc);
            V_SET_FLOAT(pr, fmod(V_FLOAT(x), V_FLOAT(y)));
            break;
        case MIR_BINARY_BITXOR:
            INT_BINARY_OP(pr, x, y, ^);
            break;
        case MIR_BINARY_BITAND:
            INT_BINARY_OP(pr, x, y, &);
            break;
        case MIR_BINARY_BITOR:
            INT_BINARY_OP(pr, x, y, |);
            break;
        case MIR_BINARY_SHL: {
            paw_Int n = V_INT(y);
            if (n < 0) {
                SHIFT_BY_NEGATIVE(C, modname, loc);
            } else if (n > 0) {
                n = PAW_MIN(n, U2I(sizeof(x) * 8 - 1));
                V_SET_INT(pr, U2I(V_UINT(x) << n));
            } else {
                *pr = x;
            }
            break;
        }
        case MIR_BINARY_SHR: {
            paw_Int n = V_INT(y);
            if (n < 0) {
                SHIFT_BY_NEGATIVE(C, modname, loc);
            } else if (n > 0) {
                n = PAW_MIN(n, U2I(sizeof(x) * 8 - 1));
                V_SET_INT(pr, V_INT(x) >> n);
            } else {
                *pr = x;
            }
            break;
        }
        case MIR_BINARY_SCONCAT:
            str_concat(C, V_STRING(x), V_STRING(y), pr);
            break;
        case MIR_BINARY_LCONCAT:
            return PAW_FALSE;
    }
    return PAW_TRUE;
}
