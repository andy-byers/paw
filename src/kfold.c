// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// kfold.c: Perform constant folding
//

#include "compile.h"
#include "error.h"
#include "rt.h"
#include <math.h>

#define KFOLD_ERROR(C_, Kind_, Modname_, ...) pawErr_##Kind_(C_, Modname_, __VA_ARGS__)
#define DIVIDE_BY_0(C_, Modname_, Loc_) KFOLD_ERROR(C_, constant_divide_by_zero, Modname_, Loc_);
#define SHIFT_BY_NEGATIVE(C_, Modname_, Loc_) KFOLD_ERROR(C_, constant_negative_shift_count, Modname_, Loc_);

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

paw_Bool pawP_fold_unary_op(struct Compiler *C, enum UnaryOp op, Value v, Value *pr, enum BuiltinKind kind)
{
    switch (op) {
        case UNARY_LEN:
            if (kind == BUILTIN_STR) {
                String const *x = V_STRING(v);
                V_SET_INT(pr, x->length);
                break;
            }
            return PAW_FALSE;
        case UNARY_NEG:
            if (kind == BUILTIN_INT) {
                INT_UNARY_OP(pr, v, -);
            } else {
                paw_assert(kind == BUILTIN_FLOAT);
                FLOAT_UNARY_OP(pr, v, -);
            }
            break;
        case UNARY_NOT:
            paw_assert(kind == BUILTIN_BOOL);
            INT_UNARY_OP(pr, v, !);
            break;
        case UNARY_BNOT:
            paw_assert(kind == BUILTIN_INT);
            INT_UNARY_OP(pr, v, ~);
            break;
    }
    return PAW_TRUE;
}

paw_Bool pawP_fold_binary_op(struct Compiler *C, String const *modname, struct SourceLoc loc, enum BinaryOp op, Value x, Value y, Value *pr, enum BuiltinKind kind)
{
    switch (op) {
        case BINARY_EQ: {
            if (kind == BUILTIN_FLOAT) {
                FLOAT_COMPARISON(pr, x, y, ==);
            } else if (kind == BUILTIN_STR) {
                STR_COMPARISON(pr, x, y, ==);
            } else if (kind == BUILTIN_BOOL) {
                INT_COMPARISON(pr, x, y, ==);
            } else if (kind == BUILTIN_INT) {
                INT_COMPARISON(pr, x, y, ==);
            } else {
                return PAW_FALSE;
            }
            break;
        }
        case BINARY_NE:
            if (kind == BUILTIN_FLOAT) {
                FLOAT_COMPARISON(pr, x, y, !=);
            } else if (kind == BUILTIN_STR) {
                STR_COMPARISON(pr, x, y, !=);
            } else if (kind == BUILTIN_BOOL) {
                INT_COMPARISON(pr, x, y, !=);
            } else if (kind == BUILTIN_INT) {
                INT_COMPARISON(pr, x, y, !=);
            } else {
                return PAW_FALSE;
            }
            break;
        case BINARY_LT:
            if (kind == BUILTIN_FLOAT) {
                FLOAT_COMPARISON(pr, x, y, <);
            } else if (kind == BUILTIN_STR) {
                STR_COMPARISON(pr, x, y, <);
            } else {
                INT_COMPARISON(pr, x, y, <);
            }
            break;
        case BINARY_LE:
            if (kind == BUILTIN_FLOAT) {
                FLOAT_COMPARISON(pr, x, y, <=);
            } else if (kind == BUILTIN_STR) {
                STR_COMPARISON(pr, x, y, <=);
            } else {
                INT_COMPARISON(pr, x, y, <=);
            }
            break;
        case BINARY_GT:
            if (kind == BUILTIN_FLOAT) {
                FLOAT_COMPARISON(pr, x, y, >);
            } else if (kind == BUILTIN_STR) {
                STR_COMPARISON(pr, x, y, >);
            } else {
                INT_COMPARISON(pr, x, y, >);
            }
            break;
        case BINARY_GE:
            if (kind == BUILTIN_FLOAT) {
                FLOAT_COMPARISON(pr, x, y, >=);
            } else if (kind == BUILTIN_STR) {
                STR_COMPARISON(pr, x, y, >=);
            } else {
                INT_COMPARISON(pr, x, y, >=);
            }
            break;
        case BINARY_ADD:
            if (kind == BUILTIN_FLOAT) {
                FLOAT_BINARY_OP(pr, x, y, +);
            } else if (kind == BUILTIN_STR) {
                str_concat(C, V_STRING(x), V_STRING(y), pr);
            } else if (kind == BUILTIN_LIST) {
                return PAW_FALSE;
            } else {
                INT_BINARY_OP(pr, x, y, +);
            }
            break;
        case BINARY_SUB:
            if (kind == BUILTIN_FLOAT) {
                FLOAT_BINARY_OP(pr, x, y, -);
            } else {
                INT_BINARY_OP(pr, x, y, -);
            }
            break;
        case BINARY_MUL:
            if (kind == BUILTIN_FLOAT) {
                FLOAT_BINARY_OP(pr, x, y, *);
            } else {
                INT_BINARY_OP(pr, x, y, *);
            }
            break;
        case BINARY_DIV:
            constant_div(C, modname, loc, pr, x, y, kind);
            break;
        case BINARY_MOD:
            constant_mod(C, modname, loc, pr, x, y, kind);
            break;
        case BINARY_BXOR:
            paw_assert(kind == BUILTIN_INT);
            INT_BINARY_OP(pr, x, y, ^);
            break;
        case BINARY_BAND:
            paw_assert(kind == BUILTIN_INT);
            INT_BINARY_OP(pr, x, y, &);
            break;
        case BINARY_BOR:
            paw_assert(kind == BUILTIN_INT);
            INT_BINARY_OP(pr, x, y, |);
            break;
        case BINARY_SHL: {
            paw_assert(kind == BUILTIN_INT);
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
        case BINARY_SHR:
            paw_assert(kind == BUILTIN_INT);
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
        case BINARY_RANGE:
            return PAW_FALSE;
        case BINARY_AS:
            PAW_UNREACHABLE();
    }
    return PAW_TRUE;
}
