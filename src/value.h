// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_VALUE_H
#define PAW_VALUE_H

#include "core.h"
#include "util.h"

#define V_ISNAN(Value_) ((Value_).f != (Value_).f)

#define V_FALSE(Value_) (!(Value_).b)
#define V_TRUE(Value_) ((Value_).b)
#define V_CHAR(Value_) ((Value_).c)
#define V_INT(Value_) ((Value_).i)
#define V_UINT(Value_) ((Value_).u)
#define V_FLOAT(Value_) ((Value_).f)
#define V_STR(Value_) ((Value_).s)

#define V_OBJECT(Value_) ((Value_).o)
#define V_TUPLE(Value_) (O_TUPLE(V_OBJECT(Value_)))
#define V_TEXT(Value_) (V_STR(Value_)->text)

#define V_SET_0(Ptr_) ((Ptr_)->u = 0)
#define V_SET_BOOL(Ptr_, Bool_) ((Ptr_)->u = (Bool_) ? PAW_TRUE : PAW_FALSE)
#define V_SET_CHAR(Ptr_, Char_) ((Ptr_)->i = (paw_Int)(Char_))
#define V_SET_INT(Ptr_, Int_) ((Ptr_)->i = (Int_))
#define V_SET_FLOAT(Ptr_, Float_) ((Ptr_)->f = (Float_))
#define V_SET_OBJECT(Ptr_, Object_) ((Ptr_)->o = (Object *)(Object_))

#define O_KIND(Object_) ((Object_)->objkind)
#define O_IS_STR(Object_) (O_KIND(Object_) == VSTR)
#define O_IS_TUPLE(Object_) (O_KIND(Object_) == VTUPLE)

#define O_STR(Object_) CHECK_EXP(O_IS_STR(Object_), (Str *)(Object_))
#define O_TUPLE(Object_) CHECK_EXP(O_IS_TUPLE(Object_), (Tuple *)(Object_))

#define CAST_OBJECT(x) ((Object *)(void *)(x))

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


#define OBJECT_HEADER ValueKind objkind : 8
typedef struct Object {
    OBJECT_HEADER;
} Object;

typedef union IrValue {
    paw_Uint8 b;
    paw_Char c;
    paw_Int8 i8;
    paw_Int16 i16;
    paw_Int32 i32;
    paw_Int64 i64;
    paw_Isize isize;
    paw_Uint8 u8;
    paw_Uint16 u16;
    paw_Uint32 u32;
    paw_Uint64 u64;
    paw_Usize usize;
    paw_Float32 f32;
    paw_Float64 f64;
    struct Str const *s;
    Object *o;
    void *p;

    // TODO: remove these and use fixed-width types
    paw_Int i;
    paw_Uint u;
    paw_Float f;
} IrValue;


#define P2V(Ptr_) (IrValue){ .p = (void *)(Ptr_) }
#define C2V(Char_) (IrValue){ .u64 = (paw_Uint8)(Char_) }
#define I2V(Int_) (IrValue){ .i64 = (paw_Int64)(Int_) }
#define F2V(Float_) (IrValue){ .f64 = (paw_Float64)(Float_) }


typedef struct Str {
    OBJECT_HEADER;
    short flag;
    unsigned hash;
    struct Str *next;
    size_t length;
    char text[];
} Str;

int pawV_int_to_str(paw_Int64 i, char *out, size_t out_len);
int pawV_float_to_str(paw_Float64 f, char *out, size_t out_len);

#endif // PAW_VALUE_H
