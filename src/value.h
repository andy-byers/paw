// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_VALUE_H
#define PAW_VALUE_H

#include "paw.h"
#include "util.h"

// Initializer for iterator state variables
#define PAW_ITER_INIT -1

#define f_is_nan(v) ((v).f != (v).f)

#define v_true(v) ((v).u != 0)
#define v_false(v) (!v_true(v))
#define v_int(v) ((v).i)
#define v_float(v) ((v).f)

#define v_object(v) ((v).o)
#define v_native(v) (o_native(v_object(v)))
#define v_proto(v) (o_proto(v_object(v)))
#define v_closure(v) (o_closure(v_object(v)))
#define v_upvalue(v) (o_upvalue(v_object(v)))
#define v_string(v) (o_string(v_object(v)))
#define v_map(v) (o_map(v_object(v)))
#define v_vector(v) (o_vector(v_object(v)))
#define v_text(v) (v_string(v)->text)
#define v_tuple(v) (o_tuple(v_object(v)))
#define v_variant(v) (o_variant(v_object(v)))
#define v_enum(v) (o_enum(v_object(v)))
#define v_instance(v) (o_instance(v_object(v)))
#define v_struct(v) (o_struct(v_object(v)))
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
#define o_is_map(o) (o_kind(o) == VMAP)
#define o_is_vector(o) (o_kind(o) == VVECTOR)
#define o_is_tuple(o) (o_kind(o) == VTUPLE)
#define o_is_variant(o) (o_kind(o) == VVARIANT)
#define o_is_enum(o) (o_kind(o) == VENUM)
#define o_is_instance(o) (o_kind(o) == VINSTANCE)
#define o_is_struct(o) (o_kind(o) == VSTRUCT)
#define o_is_method(o) (o_kind(o) == VMETHOD)
#define o_is_foreign(o) (o_kind(o) == VFOREIGN)

#define o_string(o) CHECK_EXP(o_is_string(o), (String *)(o))
#define o_native(o) CHECK_EXP(o_is_native(o), (Native *)(o))
#define o_proto(o) CHECK_EXP(o_is_proto(o), (Proto *)(o))
#define o_closure(o) CHECK_EXP(o_is_closure(o), (Closure *)(o))
#define o_upvalue(o) CHECK_EXP(o_is_upvalue(o), (UpValue *)(o))
#define o_map(o) CHECK_EXP(o_is_map(o), (Map *)(o))
#define o_vector(o) CHECK_EXP(o_is_vector(o), (Vector *)(o))
#define o_tuple(o) CHECK_EXP(o_is_tuple(o), (Tuple *)(o))
#define o_variant(o) CHECK_EXP(o_is_variant(o), (Variant *)(o))
#define o_enum(o) CHECK_EXP(o_is_enum(o), (Enum *)(o))
#define o_instance(o) CHECK_EXP(o_is_instance(o), (Instance *)(o))
#define o_struct(o) CHECK_EXP(o_is_struct(o), (Struct *)(o))
#define o_method(o) CHECK_EXP(o_is_method(o), (Method *)(o))
#define o_foreign(o) CHECK_EXP(o_is_foreign(o), (Foreign *)(o))

#define CAST_OBJECT(x) (CAST(CAST(x, void *), Object *))

typedef enum ValueKind {
    // scalar types
    VBOOL,
    VINT,
    VFLOAT,

    // object types
    VSTRING,
    VVECTOR,
    VMAP,
    VSTRUCT,
    VENUM,
    VTUPLE,
    VINSTANCE,
    VVARIANT,
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

#define GC_HEADER \
    struct Object *gc_next; \
    uint8_t gc_mark : 2; \
    ValueKind gc_kind : 6
typedef struct Object {
    GC_HEADER;
} Object;

typedef union Value {
    uint64_t u;
    paw_Int i;
    paw_Float f;
    Object *o;
    void *p;
} Value;

typedef Value *StackPtr;

typedef union StackRel {
    ptrdiff_t d;
    StackPtr p;
} StackRel;

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
            return PAW_TSTRUCT;
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

void pawV_index_error(paw_Env *P, paw_Int index, size_t length);
paw_Int pawV_length(Value v, paw_Type type);
paw_Bool pawV_truthy(Value v, paw_Type type);
int pawV_num2int(Value *pv, paw_Type type);
int pawV_num2float(Value *pv, paw_Type type);
uint32_t pawV_hash(Value v);
const char *pawV_name(ValueKind type);

static paw_Int pawV_abs_index(paw_Int index, size_t length)
{
    return index + (index < 0 ? paw_cast_int(length) : 0);
}

static inline size_t pawV_check_abs(paw_Env *P, paw_Int index, size_t length)
{
    index = pawV_abs_index(index, length);
    if (index < 0 || CAST_SIZE(index) >= length) {
        pawV_index_error(P, index, length);
    }
    return CAST_SIZE(index);
}

// Convert a null-terminated string into a 64-bit unsigned integer
// Understands non-decimal base prefixes '0b', '0o', '0x', and their uppercase
// counterparts. Returns PAW_ESYNTAX if the integer is malformed,
// PAW_EOVERFLOW if it is too large to fit in a paw_Int, and PAW_OK otherwise.
int pawV_parse_uint64(paw_Env *P, const char *text);

// Convert a null-terminated string into a float
// Returns 0 on success, -1 otherwise.
int pawV_parse_float(paw_Env *P, const char *text);

void pawV_set_default(paw_Env *P, Value *pv, paw_Type type);

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
paw_Int pawV_to_int(paw_Env *P, Value v, paw_Type type);

typedef struct Proto {
    GC_HEADER;
    uint8_t is_va;

    Object *gc_list;
    String *name;
    String *modname;
    uint32_t *source;
    int length;

    struct UpValueInfo {
        uint16_t index;
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
    uint16_t nup;
    Proto *p;
    Object *gc_list;
    UpValue *up[];
} Closure;

Closure *pawV_new_closure(paw_Env *P, int nup);
void pawV_free_closure(paw_Env *P, Closure *c);

typedef struct Native {
    GC_HEADER;
    uint16_t nup;
    Object *gc_list;
    paw_Function func;
    Value up[];
} Native;

Native *pawV_new_native(paw_Env *P, paw_Function func, int nup);
void pawV_free_native(paw_Env *P, Native *f);

typedef struct Tuple {
    GC_HEADER;
    int nelems;
    Object *gc_list;
    Value elems[];
} Tuple;

Tuple *pawV_new_tuple(paw_Env *P, int nelems);
void pawV_free_tuple(paw_Env *P, Tuple *t);

typedef struct Vector {
    GC_HEADER;
    Object *gc_list;
    Value *begin;
    Value *end;
    Value *upper;
} Vector;

Vector *pawV_vec_new(paw_Env *P);
void pawV_vec_free(paw_Env *P, Vector *vec);
Vector *pawV_vec_clone(paw_Env *P, Value *pv, const Vector *vec);
paw_Bool pawV_vec_equals(paw_Env *P, const Vector *lhs, const Vector *rhs);
paw_Bool pawV_vec_contains(paw_Env *P, const Vector *vec, Value v);
void pawV_vec_reserve(paw_Env *P, Vector *vec, size_t length);
void pawV_vec_resize(paw_Env *P, Vector *vec, size_t length);
void pawV_vec_insert(paw_Env *P, Vector *vec, paw_Int index, Value v);
void pawV_vec_push(paw_Env *P, Vector *vec, Value v);
void pawV_vec_pop(paw_Env *P, Vector *vec, paw_Int index);

static inline size_t pawV_vec_length(const Vector *vec)
{
    return CAST_SIZE(vec->end - vec->begin);
}

static inline Value *pawV_vec_get(paw_Env *P, Vector *vec, paw_Int index)
{
    const paw_Int abs = pawV_abs_index(index, CAST_SIZE(vec->end - vec->begin));
    const size_t i = pawV_check_abs(P, abs, pawV_vec_length(vec));
    return &vec->begin[i];
}

static inline paw_Bool pawV_vec_iter(const Vector *vec, paw_Int *itr)
{
    return ++*itr < paw_cast_int(pawV_vec_length(vec));
}

typedef enum MapState {
    MAP_ITEM_VACANT,
    MAP_ITEM_ERASED,
    MAP_ITEM_OCCUPIED,
} MapState;

typedef struct MapMeta {
    MapState state : 2;
} MapMeta;

typedef struct Map {
    GC_HEADER;
    Object *gc_list;
    void *data;
    size_t length;
    size_t capacity;
} Map;

// Instance of a struct
typedef struct Instance {
    GC_HEADER; // common members for GC
    int nfields;
    Object *gc_list;
    Value attrs[]; // fixed array of attributes
    // Value fields[]; // data fields
} Instance;

Instance *pawV_new_instance(paw_Env *P, int nfields);
void pawV_free_instance(paw_Env *P, Instance *ins);

typedef struct Variant {
    GC_HEADER; // common members for GC
    uint8_t k; // discriminator
    int nfields;
    Object *gc_list;
    Value fields[]; // data fields
} Variant;

Variant *pawV_new_variant(paw_Env *P, int k, int nfields);
void pawV_free_variant(paw_Env *P, Variant *var);

// Method bound to an instance
typedef struct Method {
    GC_HEADER;
    Object *gc_list;
    Value self;
    Value f;
} Method;

Method *pawV_new_method(paw_Env *P, Value self, Value call);
void pawV_free_method(paw_Env *P, Method *);

#define BOX_PARSE_MAP 1
#define BOX_PARSE_BUFFER 2

typedef struct Foreign {
    GC_HEADER;
    uint8_t flags;
    int nfields;
    Object *gc_list;
    void *data;
    size_t size;
    Value attrs[];
} Foreign;

Foreign *pawV_push_foreign(paw_Env *P, size_t size, int nfields);
void pawV_free_foreign(paw_Env *P, Foreign *ud);

#endif // PAW_VALUE_H
