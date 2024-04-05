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

#define pawV_get_bool(v) check_exp(pawV_is_bool(v), pawV_is_true(v))
#define pawV_get_int(v) check_exp(pawV_is_int(v), get_vint_aux_(v))
#define pawV_get_float(v) check_exp(pawV_is_float(v), (v).f)
#define pawV_get_native(v) check_exp(pawV_is_native(v), (Native *)pawV_get_object(v))
#define pawV_get_proto(v) check_exp(pawV_is_proto(v), (Proto *)pawV_get_object(v))
#define pawV_get_closure(v) check_exp(pawV_is_closure(v), (Closure *)pawV_get_object(v))
#define pawV_get_upvalue(v) check_exp(pawV_is_upvalue(v), (UpValue *)pawV_get_object(v))
#define pawV_get_bigint(v) check_exp(pawV_is_bigint(v), (BigInt *)pawV_get_object(v))
#define pawV_get_string(v) check_exp(pawV_is_string(v), (String *)pawV_get_object(v))
#define pawV_get_text(v) pawV_get_string(v)->text
#define pawV_get_array(v) check_exp(pawV_is_array(v), (Array *)pawV_get_object(v))
#define pawV_get_map(v) check_exp(pawV_is_map(v), (Map *)pawV_get_object(v))
#define pawV_get_class(v) check_exp(pawV_is_class(v), (Class *)pawV_get_object(v))
#define pawV_get_instance(v) check_exp(pawV_is_instance(v), (Instance *)pawV_get_object(v))
#define pawV_get_method(v) check_exp(pawV_is_method(v), (Method *)pawV_get_object(v))
#define pawV_get_foreign(v) check_exp(pawV_is_foreign(v), (Foreign *)pawV_get_object(v))
#define pawV_get_object(v) check_exp(pawV_is_object(v), cast_object(((v).u << 1) & VPTR_MASK))
#define pawV_get_type(v) ((uint32_t)((v).i >> 47))

#define pawV_is_null(v) ((v).i == -1)
#define pawV_is_true(v) (pawV_get_type(v) == VTRUE)
#define pawV_is_false(v) (pawV_get_type(v) == VFALSE)
#define pawV_is_bool(v) (pawV_is_true(v) || pawV_is_false(v))
#define pawV_is_number(v) (pawV_get_type(v) <= VNUMBER)
#define pawV_is_int(v) (pawV_get_type(v) == VNUMBER)
#define pawV_is_float(v) (pawV_get_type(v) < VNUMBER)
#define pawV_is_foreign(v) (pawV_get_type(v) == VFOREIGN)
#define pawV_is_native(v) (pawV_get_type(v) == VNATIVE)
#define pawV_is_proto(v) (pawV_get_type(v) == VPROTO)
#define pawV_is_closure(v) (pawV_get_type(v) == VCLOSURE)
#define pawV_is_upvalue(v) (pawV_get_type(v) == VUPVALUE)
#define pawV_is_bigint(v) (pawV_get_type(v) == VBIGINT)
#define pawV_is_string(v) (pawV_get_type(v) == VSTRING)
#define pawV_is_array(v) (pawV_get_type(v) == VARRAY)
#define pawV_is_map(v) (pawV_get_type(v) == VMAP)
#define pawV_is_class(v) (pawV_get_type(v) == VCLASS)
#define pawV_is_instance(v) (pawV_get_type(v) == VINSTANCE)
#define pawV_is_method(v) (pawV_get_type(v) == VMETHOD)
#define pawV_is_object(v) (pawV_get_type(v) - VOBJECT0 - 1 > VNUMBER - VOBJECT0 - 1)
#define v_isnan(v) ((v).f != (v).f)

#define obj2v_aux(o, t) \
    (Value) { .u = ((uint64_t)(o) >> 1) | ((uint64_t)(t) << 47) }
#define obj2v(o) obj2v_aux(o, ~(uint32_t)(o)->gc_kind)
#define pawV_set_object(p, o, t) (*(p) = obj2v_aux(o, t))
#define pawV_set_null(p) ((p)->i = -1)
#define pawV_set_bool(p, b) ((p)->u = ~((uint64_t)((b) + 1) << 47))
#define pawV_set_int(p, i) ((p)->u = (uint64_t)VNUMBER << VINT_WIDTH | ((uint64_t)(i) & VINT_MASK))

#define pawV_set_float(p, F) ((p)->f = F)
#define pawV_set_foreign(p, o) pawV_set_object(p, o, VFOREIGN)
#define pawV_set_native(p, o) pawV_set_object(p, o, VNATIVE)
#define pawV_set_proto(p, o) pawV_set_object(p, o, VPROTO)
#define pawV_set_closure(p, o) pawV_set_object(p, o, VCLOSURE)
#define pawV_set_upvalue(p, o) pawV_set_object(p, o, VUPVALUE)
#define pawV_set_bigint(p, o) pawV_set_object(p, o, VBIGINT)
#define pawV_set_string(p, o) pawV_set_object(p, o, VSTRING)
#define pawV_set_array(p, o) pawV_set_object(p, o, VARRAY)
#define pawV_set_map(p, o) pawV_set_object(p, o, VMAP)
#define pawV_set_class(p, o) pawV_set_object(p, o, VCLASS)
#define pawV_set_instance(p, o) pawV_set_object(p, o, VINSTANCE)
#define pawV_set_method(p, o) pawV_set_object(p, o, VMETHOD)

#define obj2string(o) check_exp((o)->gc_kind == VSTRING, (String *)(o))
#define obj2bigint(o) check_exp((o)->gc_kind == VBIGINT, (BigInt *)(o))
#define obj2native(o) check_exp((o)->gc_kind == VNATIVE, (Native *)(o))
#define obj2proto(o) check_exp((o)->gc_kind == VPROTO, (Proto *)(o))
#define obj2closure(o) check_exp((o)->gc_kind == VCLOSURE, (Closure *)(o))
#define obj2upvalue(o) check_exp((o)->gc_kind == VUPVALUE, (UpValue *)(o))
#define obj2array(o) check_exp((o)->gc_kind == VARRAY, (Array *)(o))
#define obj2map(o) check_exp((o)->gc_kind == VMAP, (Map *)(o))
#define obj2class(o) check_exp((o)->gc_kind == VCLASS, (Class *)(o))
#define obj2instance(o) check_exp((o)->gc_kind == VINSTANCE, (Instance *)(o))
#define obj2method(o) check_exp((o)->gc_kind == VMETHOD, (Method *)(o))
#define obj2foreign(o) check_exp((o)->gc_kind == VFOREIGN, (Foreign *)(o))

#define GC_HEADER           \
    struct Object *gc_next; \
    uint8_t gc_kind;        \
    uint8_t gc_mark
#define cast_uintptr(x) ((uintptr_t)(x))
#define cast_object(x) ((Object *)(void *)(x))
#define has_meta(x) (pawV_is_instance(x) || pawV_is_foreign(x))

struct String;

typedef union Value {
    uint64_t u;
    int64_t i;
    paw_Float f;
} Value;

// Type representing the kind of paw Value
typedef unsigned ValueKind;

typedef Value *StackPtr;

typedef union StackRel {
    ptrdiff_t d;
    StackPtr p;
} StackRel;

#define s2v(s) (&(s).v)

// Internal value representation:
//
//              ---------MSW------.-------LSW--------
// Primitives  |1...1|Type|1........................1|
// Objects     |1...1|Type|--------Object *----------|
// Int         |1...1|Type|---------int47_t----------|
// Float       |---------------double----------------|
//
// Object pointers are shifted right by 1, in order to preserve the 48th
// bit. Some environments seem to use pointers with this bit set to 1.

#define VNULL (~0U)
#define VFALSE (~1U)
#define VTRUE (~2U)
#define VFOREIGN (~3U)
#define VBIGINT (~4U)
#define VSTRING (~5U)
#define VNATIVE (~6U)
#define VPROTO (~7U)
#define VCLOSURE (~8U)
#define VUPVALUE (~9U)
#define VARRAY (~10U)
#define VMAP (~11U)
#define VCLASS (~12U)
#define VINSTANCE (~13U)
#define VMETHOD (~14U)
#define VNUMBER (~15U)

#define VPTR_WIDTH UINT64_C(48)
#define VINT_WIDTH UINT64_C(47)
#define VINT_MAX ((paw_int_c(1) << (VINT_WIDTH - 1)) - 1)
#define VINT_MIN (-VINT_MAX - 1)

#define VOBJECT0 VFOREIGN
#define NOBJECTS (int)(~VNUMBER - ~VOBJECT0)
#define VPTR_MASK ((UINT64_C(1) << VPTR_WIDTH) - 1)
#define VINT_MASK ((UINT64_C(1) << VINT_WIDTH) - 1)
#define obj_index(t) (~(t) - ~VOBJECT0)

int paw_value_promote(Value *v, int want);
paw_Int pawV_length(Value v);
paw_Bool pawV_truthy(Value v);
int pawV_num2int(Value *pv);
int pawV_num2float(Value *pv);
paw_Bool pawV_equal(Value x, Value y);
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

typedef struct Object {
    GC_HEADER;
} Object;

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

const char *pawV_to_string(paw_Env *P, Value v, size_t *nout);

typedef struct VarDesc {
    const Type *type;
    String *name;
} VarDesc;

typedef struct Proto {
    GC_HEADER;
    uint8_t is_va;

    FnType *type;
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

    // constants
    Value *k;

    // nested functions
    struct Proto **p;

    int nup; // number of upvalues
    int nlines; // number of lines
    int ndebug; // number of locals
    int nk; // number of constants
    int argc; // number of fixed parameters
    int nproto; // number of nested functions

    Object *gc_list;
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
    Object *gc_list;
    Proto *p;
    UpValue *up[];
} Closure;

Closure *pawV_new_closure(paw_Env *P, int nup);
void pawV_free_closure(paw_Env *P, Closure *c);

typedef struct Native {
    GC_HEADER;
    uint16_t nup;
    Object *gc_list;
    paw_Function f;
    Value up[];
} Native;

Native *pawV_new_native(paw_Env *P, paw_Function f, int nup);
void pawV_free_native(paw_Env *P, Native *nt);

typedef struct Array {
    GC_HEADER;
    Value *begin;
    Value *end;
    Value *upper;
    Object *gc_list;
} Array;

typedef struct Map {
    GC_HEADER;
    Value *keys;
    Value *values;
    size_t length;
    size_t capacity;
    Object *gc_list;
} Map;

// Class prototype object
// Created using the 'class' keyword in paw. Classes are closed at
// creation time, and can only contain methods. Methods go in the
// 'bound' variable of a class instance.
typedef struct Class {
    GC_HEADER;
    String *name;
    Map *attr;
    Object *gc_list;
} Class;

Class *pawV_push_class(paw_Env *P);
void pawV_free_class(paw_Env *P, Class *cls);

// Instance of a class
typedef struct Instance {
    GC_HEADER; // common fields for GC
    int nbound; // number of bound functions
    Class *self; // class type
    Map *attr; // runtime attributes
    Object *gc_list; // grey object list
    Value bound[]; // array of bound functions
} Instance;

Instance *pawV_new_instance(paw_Env *P, StackPtr sp, Class *cls);
void pawV_free_instance(paw_Env *P, Instance *ins);
Value *pawV_find_binding(paw_Env *P, Value obj, Value name);

// Method bound to a class or builtin object
typedef struct Method {
    GC_HEADER;
    Value self;
    Value f;
    Object *gc_list;
} Method;

Method *pawV_new_method(paw_Env *P, Value self, Value call);
void pawV_free_method(paw_Env *P, Method *);

typedef struct Foreign {
    GC_HEADER;
    int nbound;
    void *data;
    size_t size;
    Map *attr;
    Object *gc_list;
    Value bound[];
} Foreign;

Foreign *pawV_push_foreign(paw_Env *P, size_t size, int nbound);
void pawV_free_foreign(paw_Env *P, Foreign *ud);
Foreign *pawV_new_builtin(paw_Env *P, int nbound);

typedef struct Module {
    struct GlobalVec {
        VarDesc *data; 
        int size;
        int alloc;
    } globals;

    Closure *entry;
} Module;

#define ISIGNBIT (UINT64_C(1) << (VINT_WIDTH - 1))

static inline paw_Int get_vint_aux_(const Value v)
{
    Value x = {.u = v.u & VINT_MASK};
    if (x.u & ISIGNBIT) {
        // manual sign extension
        x.u |= ~VINT_MASK;
    }
    return x.i;
}

static inline paw_Bool pawV_int_fits_int(paw_Int i)
{
    return VINT_MIN < i && i < VINT_MAX;
}

static inline paw_Bool pawV_float_fits_int(paw_Float f)
{
    // FIXME
    return VINT_MIN < f && f < VINT_MAX;
}

const char *pawV_name(ValueKind kind);
paw_Int pawV_to_int64(Value v, paw_Bool *plossless);
paw_Float pawV_to_float(Value v);

#endif // PAW_VALUE_H
