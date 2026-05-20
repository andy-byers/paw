// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_LAYOUT_H
#define PAW_LAYOUT_H

#include "ir_type.h"


enum IrTypeSizeKind {
    IR_TYPESIZE_FIXED,
};

typedef struct IrTypeSize {
    enum IrTypeSizeKind kind;
    unsigned value;
} IrTypeSize;

typedef struct IrAlignment {
    unsigned exponent;
} IrAlignment;

#define IR_TYPESIZE_FIXED(Value_) (IrTypeSize){.kind = IR_TYPESIZE_FIXED, .value = Value_}
#define IR_TYPESIZE_UNSIZED() (IrTypeSize){.kind = IR_TYPESIZE_UNSIZED}
#define IR_TYPESIZE_GET_VALUE(TypeSize_) (TypeSize_).value
#define IR_TYPESIZE_GET_KIND(TypeSize_) (TypeSize_).kind

#define IR_ALIGNMENT_FROM_EXPONENT(Value_) (IrAlignment){.exponent = Value_}
#define IR_ALIGNMENT_AS_INTEGER(Alignment_) (1U << (Alignment_).exponent)

static IrTypeSize IrTypeSize_max(IrTypeSize lhs, IrTypeSize rhs)
{
    return IR_TYPESIZE_FIXED(
            PAW_MAX(lhs.value, rhs.value));
}

static IrTypeSize IrTypeSize_add_unchecked(IrTypeSize lhs, IrTypeSize rhs)
{
    return IR_TYPESIZE_FIXED(
            IR_TYPESIZE_GET_VALUE(lhs)
            + IR_TYPESIZE_GET_VALUE(rhs));
}

static paw_Bool IrTypeSize_add(IrTypeSize lhs, IrTypeSize rhs, IrTypeSize *out)
{
    if (IR_TYPESIZE_GET_VALUE(lhs) > UINT_MAX - IR_TYPESIZE_GET_VALUE(rhs))
        return PAW_FALSE;
    *out = IrTypeSize_add_unchecked(lhs, rhs);
    return PAW_TRUE;
}

static IrAlignment IrAlignment_max(IrAlignment lhs, IrAlignment rhs)
{
    return IR_ALIGNMENT_FROM_EXPONENT(
            PAW_MAX(lhs.exponent, rhs.exponent));
}

struct IrLayout {
    struct IrLayoutFields *fields;
    IrAlignment alignment;
    IrTypeSize size;
    unsigned count;
};

struct IrLayoutField {
    struct IrLayout layout;
    unsigned offset;
};

struct IrLayout pawIr_compute_layout(struct Compiler *C, IrType *type);

DEFINE_LIST(struct Compiler, IrLayouts, struct IrLayout)
DEFINE_LIST(struct Compiler, IrLayoutFields, struct IrLayoutField)
DEFINE_MAP(struct Compiler, IrTypeLayouts, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, struct IrLayout)

#endif // PAW_LAYOUT_H
