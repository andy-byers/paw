// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_VALUE_H
#define PAW_VALUE_H

#include "paw.h"
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
#define V_NATIVE(Value_) (O_NATIVE(V_OBJECT(Value_)))
#define V_PROTO(Value_) (O_PROTO(V_OBJECT(Value_)))
#define V_CLOSURE(Value_) (O_CLOSURE(V_OBJECT(Value_)))
#define V_UPVALUE(Value_) (O_UPVALUE(V_OBJECT(Value_)))
#define V_STR(Value_) (O_STR(V_OBJECT(Value_)))
#define V_TUPLE(Value_) (O_TUPLE(V_OBJECT(Value_)))
#define V_FOREIGN(Value_) (O_FOREIGN(V_OBJECT(Value_)))
#define V_TEXT(Value_) (V_STR(Value_)->text)

#define V_SET_0(Ptr_) ((Ptr_)->u = 0)
#define V_SET_BOOL(Ptr_, Bool_) ((Ptr_)->u = (Bool_) ? PAW_TRUE : PAW_FALSE)
#define V_SET_CHAR(Ptr_, Char_) ((Ptr_)->i = (paw_Int)(Char_))
#define V_SET_INT(Ptr_, Int_) ((Ptr_)->i = (Int_))
#define V_SET_FLOAT(Ptr_, Float_) ((Ptr_)->f = (Float_))
#define V_SET_OBJECT(Ptr_, Object_) ((Ptr_)->o = (Object *)(Object_))

#define O_KIND(Object_) ((Object_)->gc_kind)
#define O_IS_STR(Object_) (O_KIND(Object_) == VSTR)
#define O_IS_STRING(Object_) (O_KIND(Object_) == VSTRING)
#define O_IS_NATIVE(Object_) (O_KIND(Object_) == VNATIVE)
#define O_IS_PROTO(Object_) (O_KIND(Object_) == VPROTO)
#define O_IS_CLOSURE(Object_) (O_KIND(Object_) == VCLOSURE)
#define O_IS_UPVALUE(Object_) (O_KIND(Object_) == VUPVALUE)
#define O_IS_TUPLE(Object_) (O_KIND(Object_) == VTUPLE)
#define O_IS_FOREIGN(Object_) (O_KIND(Object_) == VFOREIGN)

#define O_STR(Object_) CHECK_EXP(O_IS_STR(Object_), (Str *)(Object_))
#define O_NATIVE(Object_) CHECK_EXP(O_IS_NATIVE(Object_), (Native *)(Object_))
#define O_PROTO(Object_) CHECK_EXP(O_IS_PROTO(Object_), (Proto *)(Object_))
#define O_CLOSURE(Object_) CHECK_EXP(O_IS_CLOSURE(Object_), (Closure *)(Object_))
#define O_UPVALUE(Object_) CHECK_EXP(O_IS_UPVALUE(Object_), (UpValue *)(Object_))
#define O_TUPLE(Object_) CHECK_EXP(O_IS_TUPLE(Object_), (Tuple *)(Object_))
#define O_FOREIGN(Object_) CHECK_EXP(O_IS_FOREIGN(Object_), (Foreign *)(Object_))

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
    VFOREIGN,

    // function types
    VNATIVE,
    VCLOSURE,

    // internal types
    VPROTO,
    VUPVALUE,

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

typedef struct Proto {
    GC_HEADER;
    unsigned char is_va;

    Object *gc_list;
    Str *name;
    Str *modname;
    unsigned *source;
    int length;

    struct UpValueInfo {
        short index;
        paw_Bool is_local;
    } *u;

    struct LineInfo {
        int pc;
        int line;
    } *lines;

    Value *k; // constants
    struct Proto **p; // nested functions
    int nup; // number of upvalues
    int nlines; // number of lines
    int nk; // number of constants
    int argc; // number of fixed parameters
    int nproto; // number of nested functions
    int max_stack;
} Proto;

Proto *pawV_new_proto(paw_Env *P);
void pawV_free_proto(paw_Env *P, Proto *p);

typedef struct UpValue {
    GC_HEADER;
    StackRel p;
    union {
        struct {
            // linked list of open upvalues
            struct UpValue *prev;
            struct UpValue *next;
        } open;
        Value closed;
    };
} UpValue;

UpValue *pawV_new_upvalue(paw_Env *P);
void pawV_free_upvalue(paw_Env *P, UpValue *u);
void pawV_link_upvalue(paw_Env *P, UpValue *u, UpValue *prev, UpValue *next);
void pawV_unlink_upvalue(UpValue *u);

#define upv_is_open(up) ((up)->p.p != &(up)->closed)
#define upv_level(up) CHECK_EXP(upv_is_open(up), (StackPtr)((up)->p.p))

typedef struct Closure {
    GC_HEADER;
    short nup;
    Proto *p;
    Object *gc_list;
    UpValue *up[];
} Closure;

Closure *pawV_new_closure(paw_Env *P, int nup);
void pawV_free_closure(paw_Env *P, Closure *c);

typedef struct Native {
    GC_HEADER;
    short nup;
    Object *gc_list;
    paw_Function fn;
    Value up[];
} Native;

Native *pawV_new_native(paw_Env *P, paw_Function fn, int nup);
void pawV_free_native(paw_Env *P, Native *f);

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

enum {
    VBOX_FILE = 1,
    VBOX_LOADER,
};

typedef struct Foreign {
    GC_HEADER;
    unsigned char flags;
    int nfields;
    Object *gc_list;
    void *data;
    size_t size;
    Value fields[];
} Foreign;

Foreign *pawV_new_foreign(paw_Env *P, size_t size, int nfields, unsigned char flags, Value *out);
void pawV_free_foreign(paw_Env *P, Foreign *ud);

#endif // PAW_VALUE_H
