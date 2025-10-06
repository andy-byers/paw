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
#define V_UCHAR(Value_) ((Value_).ux)
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

#define O_KIND(Object_) ((Object_)->gc_kind)
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

#define GC_HEADER              \
    struct Object *gc_next;    \
    unsigned char gc_mark : 2; \
    ValueKind gc_kind : 6
typedef struct Object {
    GC_HEADER;
} Object;

typedef union Value {
    void *p;
    paw_Char c;
    paw_Int i;
    paw_Uint u;
    paw_Float f;
    Object *o;
} Value;

typedef Value *StackPtr;

typedef union StackRel {
    ptrdiff_t d;
    StackPtr p;
} StackRel;

#define VOBJECT0 VSTR
#define NOBJECTS (int)(NVTYPES - VOBJECT0)
#define P2V(Ptr_) (Value) { .p = (void *)(Ptr_) }
#define C2V(Char_) (Value) { .i = (paw_Char)(Char_) }
#define I2V(Int_) (Value) { .i = (paw_Int)(Int_) }
#define F2V(Float_) (Value) { .f = (paw_Float)(Float_) }

void pawV_index_error(paw_Env *P, paw_Int index, size_t length, char const *what);

inline static Value *pawV_copy(Value *dst, Value const *src, int n)
{
    while (n-- > 0)
        *dst++ = *src++;
    return dst;
}

inline static paw_Uint pawV_hash(Value v)
{
    return v.u;
}

static paw_Int pawV_abs_index(paw_Int index, size_t length)
{
    return index + (index < 0 ? PAW_CAST_INT(length) : 0);
}

inline static size_t pawV_check_abs(paw_Env *P, paw_Int index, size_t length, char const *what)
{
    index = pawV_abs_index(index, length);
    if (index < 0 || CAST_SIZE(index) >= length) {
        pawV_index_error(P, index, length, what);
    }
    return CAST_SIZE(index);
}

// Convert a null-terminated string into an unsigned integer
// Returns PAW_ESYNTAX if the integer is malformed, PAW_EOVERFLOW if it is too large to fit
// in a paw_Uint, and PAW_OK otherwise.
int pawV_parse_uint(paw_Env *P, char const *text, int base, paw_Uint *out);

int pawV_parse_int(paw_Env *P, char const *text, int base, paw_Int *out);

// Convert a null-terminated string into a float
// Returns 0 on success, -1 otherwise.
int pawV_parse_float(paw_Env *P, char const *text, paw_Float *out);

typedef struct Str {
    GC_HEADER;
    short flag;
    unsigned hash;
    struct Str *next;
    size_t length;
    char text[];
} Str;

char const *pawV_to_str(paw_Env *P, Value *pv, paw_Type type, size_t *nout);

#define TUPLE_OTHER 0
#define TUPLE_LIST 1
#define TUPLE_MAP 2

typedef struct Tuple {
    GC_HEADER;
    unsigned char kind;
    int nelems;
    Object *gc_list;
    Value elems[];
} Tuple;

Tuple *pawV_new_tuple(paw_Env *P, int nelems);
void pawV_free_tuple(paw_Env *P, Tuple *t);

#endif // PAW_VALUE_H
