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

typedef union IrValue KValue;

#define INT_KINDS(X) \
        X(paw_Int8, IR_INT8, i8) \
        X(paw_Int16, IR_INT16, i16) \
        X(paw_Int32, IR_INT32, i32) \
        X(paw_Int64, IR_INT64, i64) \
        X(paw_Isize, IR_ISIZE, isize) \
        X(paw_Uint8, IR_UINT8, u8) \
        X(paw_Uint16, IR_UINT16, u16) \
        X(paw_Uint32, IR_UINT32, u32) \
        X(paw_Uint64, IR_UINT64, u64) \
        X(paw_Usize, IR_USIZE, usize)
#define FLOAT_KINDS(X) \
        X(paw_Float32, IR_FLOAT32, f32) \
        X(paw_Float64, IR_FLOAT64, f64)
#define CREATE_SWITCH(Kind_, Cases_) \
        switch (Kind_) { Cases_ }

#define KFOLD_ERROR(C_, Kind_, Modname_, ...) THROW_ERROR(C_, Kind_, .modname = Modname_, __VA_ARGS__)
#define DIVIDE_BY_0(C_, Modname_, Span_) KFOLD_ERROR(C_, ConstantDivideByZero, Modname_, Span_);
#define SHIFT_BY_NEGATIVE(C_, Modname_, Span_) KFOLD_ERROR(C_, ConstantNegativeShiftCount, Modname_, Span_);
#define IDIVMOD_OVERFLOWS(Left_, Right_) (V_INT(Left_) == PAW_INT_MIN && V_INT(Right_) == PAW_INT_C(-1))

int pawMir_fold_unary_op(enum MirUnaryOpKind op, IrType *type, union IrValue v, union IrValue *pr)
{
    switch (op) {
        case MIR_UNARY_NEG:
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = -v.Field_; \
        break;
            if (IrIsInt(type)) {
                enum IrIntKind const ikind = IR_INT_KIND(type);
                CREATE_SWITCH(ikind, INT_KINDS(X))
            } else {
                paw_assert(IrIsFloat(type));
                enum IrFloatKind const fkind = IR_FLOAT_KIND(type);
                CREATE_SWITCH(fkind, FLOAT_KINDS(X))
            }
            break;
#undef X
        case MIR_UNARY_BITNOT: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = ~v.Field_; \
        break;
            enum IrIntKind const ikind = IR_INT_KIND(type);
            CREATE_SWITCH(ikind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_UNARY_NOT:
            pr->b = !v.b;
            break;
    }
    return 0;
}

static int fold_int_binary_op(enum MirBinaryOpKind op, union IrValue x, union IrValue y, enum IrIntKind kind, union IrValue *pr)
{
    switch (op) {
        case MIR_BINARY_EQ: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->b = x.Field_ == y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_NE: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->b = x.Field_ != y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_LT: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->b = x.Field_ < y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_LE: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->b = x.Field_ <= y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_ADD: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ + y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_SUB: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ - y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_MUL: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ * y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_DIV: {
#define X(Type_, Label_, Field_) case Label_: \
        if (y.Field_ == 0) return -1; \
        pr->Field_ = x.Field_ / y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_MOD: {
#define X(Type_, Label_, Field_) case Label_: \
        if (y.Field_ == 0) return -1; \
        pr->Field_ = x.Field_ % y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_BITAND: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ & y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_BITOR: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ | y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_BITXOR: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ ^ y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_SHL: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ << y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_SHR: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ >> y.Field_; \
        break;
            CREATE_SWITCH(kind, INT_KINDS(X))
            break;
#undef X
        }
        default:
            break;
    }
    return 0;
}

static int fold_float_binary_op(enum MirBinaryOpKind op, union IrValue x, union IrValue y, enum IrFloatKind kind, union IrValue *pr)
{
    switch (op) {
        case MIR_BINARY_EQ: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->b = x.Field_ == y.Field_; \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_NE: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->b = x.Field_ != y.Field_; \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_LT: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->b = x.Field_ < y.Field_; \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_LE: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->b = x.Field_ <= y.Field_; \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_ADD: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ + y.Field_; \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_SUB: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ - y.Field_; \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_MUL: {
#define X(Type_, Label_, Field_) case Label_: \
        pr->Field_ = x.Field_ * y.Field_; \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_DIV: {
#define X(Type_, Label_, Field_) case Label_: \
        if (y.Field_ == 0.0) return -1; \
        pr->Field_ = x.Field_ / y.Field_; \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case MIR_BINARY_MOD: {
#define X(Type_, Label_, Field_) case Label_: \
        if (y.Field_ == 0.0) return -1; \
        pr->Field_ = fmod(x.Field_, y.Field_); \
        break;
            CREATE_SWITCH(kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        default:
            break;
    }
    return 0;
}

static int fold_str_binary_op(enum MirBinaryOpKind op, union IrValue x, union IrValue y, union IrValue *pr)
{
    switch (op) {
        case MIR_BINARY_EQ:
            pr->b = pawS_cmp(x.s, y.s) == 0;
            break;
        case MIR_BINARY_NE:
            pr->b = pawS_cmp(x.s, y.s) != 0;
            break;
        case MIR_BINARY_LT:
            pr->b = pawS_cmp(x.s, y.s) < 0;
            break;
        default:
            paw_assert(op == MIR_BINARY_LE);
            pr->b = pawS_cmp(x.s, y.s) <= 0;
            break;
    }
    return 0;
}

int pawMir_fold_binary_op(enum MirBinaryOpKind op, IrType *type, union IrValue x, union IrValue y, union IrValue *pr)
{
    switch (IR_KINDOF(type)) {
        case kIrBool:
            return fold_int_binary_op(op, x, y, IR_UINT8, pr);
        case kIrChar:
            return fold_int_binary_op(op, x, y, IR_UINT8, pr);
        case kIrInt:
            return fold_int_binary_op(op, x, y, IR_INT_KIND(type), pr);
        case kIrFloat:
            return fold_float_binary_op(op, x, y, IR_FLOAT_KIND(type), pr);
        default:
            paw_assert(IrIsString(type));
            return fold_str_binary_op(op, x, y, pr);
    }
}

static void fold_cast_from_bool(union IrValue from, IrType *to_type, union IrValue *to)
{
    switch (IR_KINDOF(to_type)) {
        case kIrChar:
            to->c = (paw_Char)from.b;
            break;
        case kIrInt:
            // all integer types have the same bit pattern for 0 and 1
            to->u64 = from.b;
            break;
        case kIrFloat: {
#define X(Type_, Label_, Field_) case Label_: \
        to->Field_ = (Type_)from.b; \
        break;
            enum IrFloatKind const fkind = IR_FLOAT_KIND(to_type);
            CREATE_SWITCH(fkind, FLOAT_KINDS(X))
            break;
#undef X
        }
        default:
            paw_assert(IrIsBool(to_type));
            to->b = from.b;
    }
}

static void fold_cast_from_char(union IrValue from, IrType *to_type, union IrValue *to)
{
    switch (IR_KINDOF(to_type)) {
        case kIrBool:
            to->b = (paw_Uint8)from.c;
            break;
        case kIrInt: {
#define X(Type_, Label_, Field_) case Label_: \
        to->Field_ = (Type_)from.c; \
        break;
            enum IrIntKind const ikind = IR_INT_KIND(to_type);
            CREATE_SWITCH(ikind, INT_KINDS(X))
            break;
#undef X
        }
        case kIrFloat: {
#define X(Type_, Label_, Field_) case Label_: \
        to->Field_ = (Type_)from.c; \
        break;
            enum IrFloatKind const fkind = IR_FLOAT_KIND(to_type);
            CREATE_SWITCH(fkind, FLOAT_KINDS(X))
            break;
#undef X
        }
        default:
            paw_assert(IrIsChar(to_type));
            to->c = from.c;
    }
}

static void fold_cast_from_int(union IrValue from, enum IrIntKind from_kind, IrType *to_type, union IrValue *to)
{
    switch (IR_KINDOF(to_type)) {
        case kIrBool: {
#define X(Type_, Label_, Field_) case Label_: \
        to->b = from.Field_ != 0; \
        break;
            CREATE_SWITCH(from_kind, INT_KINDS(X))
            break;
#undef X
        }
        case kIrChar:
            to->c = (paw_Char)from.u8;
            break;

#define X(Type_, Label_, Field_) case Label_: { \
            switch (from_kind) { \
                case IR_INT8: \
                    to->Field_ = (Type_)from.i8; \
                    break; \
                case IR_INT16: \
                    to->Field_ = (Type_)from.i16; \
                    break; \
                case IR_INT32: \
                    to->Field_ = (Type_)from.i32; \
                    break; \
                case IR_INT64: \
                    to->Field_ = (Type_)from.i64; \
                    break; \
                case IR_ISIZE: \
                    to->Field_ = (Type_)from.isize; \
                    break; \
                case IR_UINT8: \
                    to->Field_ = (Type_)from.u8; \
                    break; \
                case IR_UINT16: \
                    to->Field_ = (Type_)from.u16; \
                    break; \
                case IR_UINT32: \
                    to->Field_ = (Type_)from.u32; \
                    break; \
                case IR_UINT64: \
                    to->Field_ = (Type_)from.u64; \
                    break; \
                case IR_USIZE: \
                    to->Field_ = (Type_)from.usize; \
                    break; \
            } \
            break; \
        }

        case kIrFloat: {
            enum IrFloatKind const fkind = IR_FLOAT_KIND(to_type);
            CREATE_SWITCH(fkind, FLOAT_KINDS(X))
            break;
        }
        default: {
            paw_assert(IrIsInt(to_type));
            enum IrIntKind const ikind = IR_INT_KIND(to_type);
            CREATE_SWITCH(ikind, INT_KINDS(X))
            break;
        }

#undef X
    }
}

static void fold_cast_from_float(union IrValue from, enum IrFloatKind from_kind, IrType *to_type, union IrValue *to)
{
    switch (IR_KINDOF(to_type)) {
        case kIrBool: {
#define X(Type_, Label_, Field_) case Label_: \
        to->b = from.Field_ != 0.0; \
        break;
            CREATE_SWITCH(from_kind, FLOAT_KINDS(X))
            break;
#undef X
        }
        case kIrChar: {
#define X(Type_, Label_, Field_) case Label_: \
        to->c = (paw_Char)from.Field_; \
        break;
            CREATE_SWITCH(from_kind, FLOAT_KINDS(X))
            break;
#undef X
        }
#define X(Type_, Label_, Field_) case Label_: \
        to->Field_ = (Type_)from.Field_; \
        switch (from_kind) { \
            case IR_FLOAT32: \
                to->Field_ = (Type_)from.f32; \
                break; \
            case IR_FLOAT64: \
                to->Field_ = (Type_)from.f64; \
                break; \
        } \
        break;

        case kIrInt: {
            enum IrIntKind const ikind = IR_INT_KIND(to_type);
            CREATE_SWITCH(ikind, INT_KINDS(X))
            break;
        }
        default: {
            enum IrFloatKind const fkind = IR_FLOAT_KIND(to_type);
            CREATE_SWITCH(fkind, FLOAT_KINDS(X))
            break;
        }

#undef X
    }
}

void pawMir_fold_cast(union IrValue from, IrType *from_type, IrType *to_type, union IrValue *to)
{
    switch (IR_KINDOF(from_type)) {
        case kIrBool:
            fold_cast_from_bool(from, to_type, to);
            break;
        case kIrChar:
            fold_cast_from_char(from, to_type, to);
            break;
        case kIrInt:
            fold_cast_from_int(from, IR_INT_KIND(from_type), to_type, to);
            break;
        default:
            paw_assert(IrIsFloat(from_type));
            fold_cast_from_float(from, IR_FLOAT_KIND(from_type), to_type, to);
    }
}

