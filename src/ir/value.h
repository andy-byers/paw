// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_IR_VALUE_H
#define PAW_IR_VALUE_H

#include "compiler/core.h"

#define V_ISNAN32(Value_) ((Value_).f32 != (Value_).f32)
#define V_ISNAN64(Value_) ((Value_).f64 != (Value_).f64)

#define V_FALSE(Value_) (!(Value_).b)
#define V_TRUE(Value_) ((Value_).b)
#define V_CHAR(Value_) ((Value_).c)
#define V_INT64(Value_) ((Value_).i64)
#define V_UINT64(Value_) ((Value_).u64)
#define V_FLOAT64(Value_) ((Value_).f64)
#define V_STR(Value_) ((Value_).s)

#define V_TUPLE(Value_) ((struct Tuple *)(Value_).p)
#define V_TEXT(Value_) (V_STR(Value_)->text)

#define V_SET_0(Ptr_) ((Ptr_)->u64 = 0)
#define V_SET_BOOL(Ptr_, Value_) ((Ptr_)->u64 = (Value_) ? PAW_TRUE : PAW_FALSE)
#define V_SET_CHAR(Ptr_, Value_) ((Ptr_)->u64 = (paw_Uint64)(Value_))
#define V_SET_INT64(Ptr_, Value_) ((Ptr_)->i64 = (Value_))
#define V_SET_FLOAT64(Ptr_, Value_) ((Ptr_)->f64 = (Value_))

typedef enum ValueKind {
    // scalar types
    VBOOL,
    VCHAR,
    VINT,
    VFLOAT,

    // object types
    VSTR,
    VTUPLE,

    NVTYPES
} ValueKind;

typedef union IrValue {
    paw_Uint8 b;
    paw_Char c;
    paw_Int8 i8;
    paw_Int16 i16;
    paw_Int32 i32;
    paw_Int64 i64;
    paw_Int64 isize;
    paw_Uint8 u8;
    paw_Uint16 u16;
    paw_Uint32 u32;
    paw_Uint64 u64;
    paw_Uint64 usize;
    paw_Float32 f32;
    paw_Float64 f64;
    struct Str const *s;
    struct Tuple *t;
    void *p;
} IrValue;


#define P2V(Ptr_) (IrValue){ .p = (void *)(Ptr_) }
#define C2V(Char_) (IrValue){ .u64 = (paw_Uint8)(Char_) }
#define I2V(Int_) (IrValue){ .i64 = (paw_Int64)(Int_) }
#define F2V(Float_) (IrValue){ .f64 = (paw_Float64)(Float_) }


struct Tuple {
    paw_Uint64 discr;
    size_t num_fields;
    union IrValue fields[];
};

int pawV_int_to_str(paw_Int64 i, char *out, size_t out_len);
int pawV_float_to_str(paw_Float64 f, char *out, size_t out_len);

#endif // PAW_IR_VALUE_H
