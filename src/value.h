// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_VALUE_H
#define PAW_VALUE_H

#include "core.h"
#include "util.h"

#define V_ISNAN(Value_) ((Value_).f != (Value_).f)

#define V_FALSE(Value_) ((Value_).u == 0)
#define V_TRUE(Value_) ((Value_).u != 0)
#define V_CHAR(Value_) ((Value_).c)
#define V_INT(Value_) ((Value_).i)
#define V_UINT(Value_) ((Value_).u)
#define V_FLOAT(Value_) ((Value_).f)

#define V_OBJECT(Value_) ((Value_).o)
#define V_STR(Value_) (O_STR(V_OBJECT(Value_)))
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


typedef union Value {
    void *p;
    paw_Char c;
    paw_Int i;
    paw_Uint u;
    paw_Float f;
    Object *o;
} Value;

#define P2V(Ptr_) (Value) { .p = (void *)(Ptr_) }
#define C2V(Char_) (Value) { .i = (paw_Char)(Char_) }
#define I2V(Int_) (Value) { .i = (paw_Int)(Int_) }
#define F2V(Float_) (Value) { .f = (paw_Float)(Float_) }


typedef struct Str {
    OBJECT_HEADER;
    short flag;
    unsigned hash;
    struct Str *next;
    size_t length;
    char text[];
} Str;

char const *pawV_to_str(paw_Env *P, Value *pv, paw_Type type, size_t *nout);


typedef struct Tuple {
    OBJECT_HEADER;
    int nelems;
    Value elems[];
} Tuple;

Tuple *pawV_new_tuple(paw_Env *P, int nelems);
void pawV_free_tuple(paw_Env *P, Tuple *t);

#endif // PAW_VALUE_H
