// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_VALUE_H
#define PAW_VALUE_H

#include "paw.h"
#include "type.h"
#include "util.h"

// Initializer for iterator state variables
#define PAW_ITER_INIT -1

#define f_is_nan(v) ((v).f != (v).f)
#define v_is_null(v) ((v).u == 0)

#define v_true(v) ((v).u != 0)
#define v_false(v) (!v_true(v))
#define v_int(v) ((v).i)
#define v_float(v) ((v).f)

#define v_check(v) check_exp(!v_is_null(v), v)
#define v_object(v) (v_check(v).o)
#define v_native(v) (o_native(v_object(v)))
#define v_proto(v) (o_proto(v_object(v))) 
#define v_closure(v) (o_closure(v_object(v))) 
#define v_upvalue(v) (o_upvalue(v_object(v))) 
#define v_string(v) (o_string(v_object(v))) 
#define v_text(v) (v_string(v)->text)
#define v_instance(v) (o_instance(v_object(v))) 
#define v_class(v) (o_class(v_object(v))) 
#define v_method(v) (o_method(v_object(v))) 
#define v_foreign(v) (o_foreign(v_object(v))) 

#define v_set_0(v) ((v)->u = 0)
#define v_set_bool(p, b) ((p)->u = (b) ? PAW_TRUE : PAW_FALSE)
#define v_set_int(p, I) ((p)->i = (I))
#define v_set_float(p, F) ((p)->f = F)
#define v_set_object(p, O) ((p)->o = (Object *)(O))

#define o_kind(o) ((o)->gc_kind)
#define o_is_string(o) (o_kind(o) == VSTRING)
#define o_is_native(o) (o_kind(o) == VNATIVE)
#define o_is_proto(o) (o_kind(o) == VPROTO)
#define o_is_closure(o) (o_kind(o) == VCLOSURE)
#define o_is_upvalue(o) (o_kind(o) == VUPVALUE)
#define o_is_instance(o) (o_kind(o) == VINSTANCE)
#define o_is_class(o) (o_kind(o) == VCLASS)
#define o_is_method(o) (o_kind(o) == VMETHOD)
#define o_is_foreign(o) (o_kind(o) == VFOREIGN)

#define o_string(o) check_exp(o_is_string(o), (String *)(o))
#define o_native(o) check_exp(o_is_native(o), (Native *)(o))
#define o_proto(o) check_exp(o_is_proto(o), (Proto *)(o))
#define o_closure(o) check_exp(o_is_closure(o), (Closure *)(o))
#define o_upvalue(o) check_exp(o_is_upvalue(o), (UpValue *)(o))
#define o_instance(o) check_exp(o_is_instance(o), (Instance *)(o))
#define o_class(o) check_exp(o_is_class(o), (Class *)(o))
#define o_method(o) check_exp(o_is_method(o), (Method *)(o))
#define o_foreign(o) check_exp(o_is_foreign(o), (Foreign *)(o))

#define GC_HEADER           \
    struct Object *gc_next; \
    uint64_t gc_nrefs;      \
    uint8_t gc_kind
#define cast_uintptr(x) ((uintptr_t)(x))
#define cast_object(x) ((Object *)(void *)(x))

typedef struct Object {
    GC_HEADER;
} Object;

typedef union Value {
    uint64_t u;
    paw_Int i;
    paw_Float f;
    Object *o;
} Value;

typedef Value *StackPtr;

typedef union StackRel {
    ptrdiff_t d;
    StackPtr p;
} StackRel;

#define s2v(s) (&(s).v)

typedef enum ValueKind {
    // scalar types
    VBOOL,
    VINT,
    VFLOAT,

    // object types
    VSTRING,
    VARRAY,
    VMAP,
    VCLASS,
    VINSTANCE,
    VMETHOD,
    VFOREIGN,
    VTYPE,

    // function types
    VNATIVE,
    VCLOSURE,

    // internal types
    VPROTO,
    VUPVALUE,

    NVTYPES
} ValueKind;

static inline int pawV_type(ValueKind vt)
{
    switch (vt) {
        case VBOOL:
            return PAW_TBOOL;
        case VINT:
            return PAW_TINT;
        case VFLOAT:
            return PAW_TFLOAT;
        case VSTRING:
            return PAW_TSTRING;
        case VNATIVE:
        case VCLOSURE:
        case VMETHOD:
            return PAW_TFUNCTION;
        case VINSTANCE:
            return PAW_TCLASS;
        case VFOREIGN:
            return PAW_TFOREIGN;
        default:
            // other types are never exposed
            return PAW_TUNIT;
    }
}

#define VOBJECT0 VSTRING
#define NOBJECTS (int)(NVTYPES - VOBJECT0)
#define obj_index(t) ((t) - VOBJECT0)

paw_Int pawV_length(Value v, paw_Type type);
paw_Bool pawV_truthy(Value v, paw_Type type);
int pawV_num2int(Value *pv, paw_Type type);
int pawV_num2float(Value *pv, paw_Type type);
uint32_t pawV_hash(Value v);

// Convert a null-terminated string into an integer
// May result in either a small integer or a big integer. Understands non-decimal
// base prefixes '0b', '0o', '0x', and their uppercase counterparts. Returns a
// nonzero integer if the integer is malformed, 0 otherwise. Throws a memory error
// if an allocation failed (bigint only). Returns 0 on success, -1 otherwise.
int pawV_parse_integer(paw_Env *P, const char *text);

// Convert a null-terminated string into a float
// Returns 0 on success, -1 otherwise.
int pawV_parse_float(paw_Env *P, const char *text);

void pawV_set_default(paw_Env *P, Value *pv, paw_Type type);

// TODO: Move to bigint.h
typedef paw_Digit BiDigit;

typedef struct BigInt {
    GC_HEADER;
    BiDigit *buf;
    int size;
    int alloc;
    int neg;
} BigInt;

#define str_is_keyword(s) ((s)->flag > 0)

typedef struct String {
    GC_HEADER;
    int16_t flag;
    uint32_t hash;
    struct String *next;
    size_t length;
    char text[];
} String;

const char *pawV_to_string(paw_Env *P, Value v, paw_Type type, size_t *nout);

typedef struct VarDesc {
    Type *type;
    String *name;
} VarDesc;

typedef struct Proto {
    GC_HEADER;
    uint8_t is_va;

    String *name;
    String *modname;
    uint32_t *source;
    int length;

    struct LocalInfo {
        VarDesc var;
        int pc0;
        int pc1;
        paw_Bool captured;
    } *v;

    struct UpValueInfo {
        VarDesc var;
        uint16_t index;
        paw_Bool is_local;
    } *u;

    struct LineInfo {
        int pc;
        int line;
    } *lines;

    struct Class *classes;

    Value *k; // constants
    struct Proto **p; // nested functions
    int nup; // number of upvalues
    int nlines; // number of lines
    int ndebug; // number of locals
    int nk; // number of constants
    int argc; // number of fixed parameters
    int nproto; // number of nested functions
    int nclasses;
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
#define upv_level(up) check_exp(upv_is_open(up), (StackPtr)((up)->p.p))

typedef struct Closure {
    GC_HEADER;
    uint16_t nup;
    Proto *p;
    UpValue *up[];
} Closure;

Closure *pawV_new_closure(paw_Env *P, int nup);
void pawV_free_closure(paw_Env *P, Closure *c);

typedef struct Native {
    GC_HEADER;
    paw_Function call;
    String *name;
} Native;

Native *pawV_new_native(paw_Env *P, String *name, paw_Function call);

typedef struct Array {
    GC_HEADER;
    Value *begin;
    Value *end;
    Value *upper;
} Array;

typedef struct Map {
    GC_HEADER;
    Value *keys;
    Value *values;
    size_t length;
    size_t capacity;
} Map;

typedef struct Class {
    GC_HEADER; // common members for GC
    Value attrs[]; // fixed array of attributes
} Class;

Class *pawV_new_class(paw_Env *P, Type *type);

// Instance of a class
typedef struct Instance {
    GC_HEADER; // common members for GC
    Value attrs[]; // fixed array of attributes
} Instance;

Instance *pawV_new_instance(paw_Env *P, int nattrs);
void pawV_free_instance(paw_Env *P, Instance *ins, Type *type);
Value *pawV_find_attr(Value *attrs, String *name, Type *type);

// Method bound to an instance
typedef struct Method {
    GC_HEADER;
    Value self;
    Value f;
} Method;

Method *pawV_new_method(paw_Env *P, Value self, Value call);
void pawV_free_method(paw_Env *P, Method *);

typedef struct Foreign {
    GC_HEADER;
    void *data;
    size_t size;
    Value attrs[]; // fixed array of attributes
} Foreign;

Foreign *pawV_push_foreign(paw_Env *P, size_t size, int nattrs);
void pawV_free_foreign(paw_Env *P, Foreign *ud, Type *type);

const char *pawV_name(ValueKind type);

#endif // PAW_VALUE_H
