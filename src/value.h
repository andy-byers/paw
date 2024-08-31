// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_VALUE_H
#define PAW_VALUE_H

#include "paw.h"
#include "util.h"

// Initializer for iterator state variables
#define PAW_ITER_INIT -1

#define V_ISNAN(v) ((v).f != (v).f)

#define V_FALSE(v) ((v).u == 0)
#define V_TRUE(v) ((v).u != 0)
#define V_INT(v) ((v).i)
#define V_FLOAT(v) ((v).f)

#define V_OBJECT(v) ((v).o)
#define V_NATIVE(v) (O_NATIVE(V_OBJECT(v)))
#define V_PROTO(v) (O_PROTO(V_OBJECT(v)))
#define V_CLOSURE(v) (O_CLOSURE(V_OBJECT(v)))
#define V_UPVALUE(v) (O_UPVALUE(V_OBJECT(v)))
#define V_STRING(v) (O_STRING(V_OBJECT(v)))
#define V_MAP(v) (O_MAP(V_OBJECT(v)))
#define V_LIST(v) (O_LIST(V_OBJECT(v)))
#define V_TEXT(v) (V_STRING(v)->text)
#define V_TUPLE(v) (O_TUPLE(V_OBJECT(v)))
#define V_VARIANT(v) (O_VARIANT(V_OBJECT(v)))
#define V_FOREIGN(v) (O_FOREIGN(V_OBJECT(v)))

#define V_SET_0(v) ((v)->u = 0)
#define V_SET_BOOL(p, b) ((p)->u = (b) ? PAW_TRUE : PAW_FALSE)
#define V_SET_INT(p, I) ((p)->i = (I))
#define V_SET_FLOAT(p, F) ((p)->f = F)
#define V_SET_OBJECT(p, O) ((p)->o = (Object *)(O))

#define O_KIND(o) ((o)->gc_kind)
#define O_IS_STRING(o) (O_KIND(o) == VSTRING)
#define O_IS_NATIVE(o) (O_KIND(o) == VNATIVE)
#define O_IS_PROTO(o) (O_KIND(o) == VPROTO)
#define O_IS_CLOSURE(o) (O_KIND(o) == VCLOSURE)
#define O_IS_UPVALUE(o) (O_KIND(o) == VUPVALUE)
#define O_IS_MAP(o) (O_KIND(o) == VMAP)
#define O_IS_LIST(o) (O_KIND(o) == VLIST)
#define O_IS_TUPLE(o) (O_KIND(o) == VTUPLE)
#define O_IS_VARIANT(o) (O_KIND(o) == VVARIANT)
#define O_IS_FOREIGN(o) (O_KIND(o) == VFOREIGN)

#define O_STRING(o) CHECK_EXP(O_IS_STRING(o), (String *)(o))
#define O_NATIVE(o) CHECK_EXP(O_IS_NATIVE(o), (Native *)(o))
#define O_PROTO(o) CHECK_EXP(O_IS_PROTO(o), (Proto *)(o))
#define O_CLOSURE(o) CHECK_EXP(O_IS_CLOSURE(o), (Closure *)(o))
#define O_UPVALUE(o) CHECK_EXP(O_IS_UPVALUE(o), (UpValue *)(o))
#define O_MAP(o) CHECK_EXP(O_IS_MAP(o), (Map *)(o))
#define O_LIST(o) CHECK_EXP(O_IS_LIST(o), (List *)(o))
#define O_TUPLE(o) CHECK_EXP(O_IS_TUPLE(o), (Tuple *)(o))
#define O_VARIANT(o) CHECK_EXP(O_IS_VARIANT(o), (Variant *)(o))
#define O_FOREIGN(o) CHECK_EXP(O_IS_FOREIGN(o), (Foreign *)(o))

#define CAST_OBJECT(x) ((Object *)(void *)(x))

typedef enum ValueKind {
    // scalar types
    VBOOL,
    VINT,
    VFLOAT,

    // object types
    VSTRING,
    VLIST,
    VMAP,
    VTUPLE,
    VVARIANT,
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

#define VOBJECT0 VSTRING
#define NOBJECTS (int)(NVTYPES - VOBJECT0)
#define obj_index(t) ((t) - VOBJECT0)

void pawV_index_error(paw_Env *P, paw_Int index, size_t length, const char *what);
uint32_t pawV_hash(Value v);

static paw_Int pawV_abs_index(paw_Int index, size_t length)
{
    return index + (index < 0 ? PAW_CAST_INT(length) : 0);
}

static inline size_t pawV_check_abs(paw_Env *P, paw_Int index, size_t length, const char *what)
{
    index = pawV_abs_index(index, length);
    if (index < 0 || CAST_SIZE(index) >= length) {
        pawV_index_error(P, index, length, what);
    }
    return CAST_SIZE(index);
}

// Convert a null-terminated string into a 64-bit unsigned integer
// Understands non-decimal base prefixes '0b', '0o', '0x', and their uppercase
// counterparts. Returns PAW_ESYNTAX if the integer is malformed,
// PAW_EOVERFLOW if it is too large to fit in a uint64_t, and PAW_OK otherwise.
int pawV_parse_uint64(paw_Env *P, const char *text, int base, uint64_t *out);

int pawV_parse_int(paw_Env *P, const char *text, int base, paw_Int *out);

// Convert a null-terminated string into a float
// Returns 0 on success, -1 otherwise.
int pawV_parse_float(paw_Env *P, const char *text, paw_Float *out);

typedef struct String {
    GC_HEADER;
    int16_t flag;
    uint32_t hash;
    struct String *next;
    size_t length;
    char text[];
} String;

const char *pawV_to_string(paw_Env *P, Value *pv, paw_Type type, size_t *nout);

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

typedef struct List {
    GC_HEADER;
    Object *gc_list;
    Value *begin;
    Value *end;
    Value *upper;
} List;

List *pawV_list_new(paw_Env *P);
void pawV_list_free(paw_Env *P, List *vec);
void pawV_list_reserve(paw_Env *P, List *vec, size_t length);
void pawV_list_resize(paw_Env *P, List *vec, size_t length);
void pawV_list_insert(paw_Env *P, List *vec, paw_Int index, Value v);
void pawV_list_push(paw_Env *P, List *vec, Value v);
void pawV_list_pop(paw_Env *P, List *vec, paw_Int index);

static inline size_t pawV_list_length(const List *vec)
{
    return CAST_SIZE(vec->end - vec->begin);
}

static inline Value *pawV_list_get(paw_Env *P, List *vec, paw_Int index)
{
    const paw_Int abs = pawV_abs_index(index, CAST_SIZE(vec->end - vec->begin));
    const size_t i = pawV_check_abs(P, abs, pawV_list_length(vec), "vector");
    return &vec->begin[i];
}

static inline paw_Bool pawV_list_iter(const List *vec, paw_Int *itr)
{
    return ++*itr < PAW_CAST_INT(pawV_list_length(vec));
}

typedef enum MapState {
    MAP_ITEM_VACANT,
    MAP_ITEM_ERASED,
    MAP_ITEM_OCCUPIED,
} MapState;

typedef struct MapMeta {
    uint8_t state : 2;
} MapMeta;

typedef struct Map {
    GC_HEADER;
    Object *gc_list;
    void *data;
    size_t length;
    size_t capacity;
} Map;

typedef struct Variant {
    GC_HEADER; // common members for GC
    uint8_t k; // discriminator
    int nfields;
    Object *gc_list;
    Value fields[]; // data fields
} Variant;

Variant *pawV_new_variant(paw_Env *P, int k, int nfields);
void pawV_free_variant(paw_Env *P, Variant *var);

typedef struct Foreign {
    GC_HEADER;
    uint8_t flags;
    int nfields;
    Object *gc_list;
    void *data;
    size_t size;
    Value fields[];
} Foreign;

Foreign *pawV_new_foreign(paw_Env *P, size_t size, int nfields, Value *out);
void pawV_free_foreign(paw_Env *P, Foreign *ud);

#endif // PAW_VALUE_H
