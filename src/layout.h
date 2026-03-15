// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_LAYOUT_H
#define PAW_LAYOUT_H

#include "ir_type.h"


enum IrTypeSizeKind {
    IR_TYPESIZE_FIXED,
    IR_TYPESIZE_UNSIZED,
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

struct IrLayout {
    struct IrLayouts *fields;
    IrAlignment alignment;
    IrTypeSize size;
    unsigned count;
};

struct IrLayout pawIr_compute_layout(struct Compiler *C, IrType *type);

DEFINE_LIST(struct Compiler, IrLayouts, struct IrLayout)
DEFINE_MAP(struct Compiler, IrTypeLayouts, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, struct IrLayout)

#endif // PAW_LAYOUT_H
