// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_TYPE_H
#define PAW_TYPE_H

#include "paw.h"

typedef struct Type Type;

typedef enum TypeKind {
    TYPE_PRIMITIVE,
    TYPE_SIGNATURE,
    TYPE_CLASS,
    TYPE_MODULE,
} TypeKind;

#define TYPE_HEADER \
    Type *next; \
    TypeKind kind: 8; \
    uint8_t flags; \
    uint16_t id

typedef struct TypeHeader {
    TYPE_HEADER;
} TypeHeader;

#define FIELD_IS_METHOD 1

typedef struct NamedField {
    struct String *name; // field name
    Type *type; // field type
    uint8_t flags; // FIELD_* flags
} NamedField;

typedef struct ClassType {
    TYPE_HEADER; // common initial sequence
    struct String *name; // name of class
    Type *super; // type of 'super'
    Type *self; // type of 'self'
    NamedField *attrs; // attributes
    int nattrs; // number of attributes
} ClassType;

typedef struct FunctionType {
    TYPE_HEADER; // common initial sequence
    struct String *name; // name of function
    Type *ret; // return type
    Type **args; // argument types
    int nargs; // number of arguments
} FunctionType;

typedef struct ModuleType {
    TYPE_HEADER; // common initial sequence
    struct ModuleType *includes; // included modules
    FunctionType *functions; // global function types
    ClassType *classes; // global class types
    Type **types; // vector of unique types
    int ntypes; // number of unique types
    int capacity; // type vector capacity
} ModuleType;

struct Type {
    union {
        TypeHeader hdr;
        ClassType cls;
        FunctionType sig;
        ModuleType mod;
    };
};

#define cast_type(x) ((Type *)(x))
Type *pawY_add_type(paw_Env *P, ModuleType *mod, const Type *type);

#define y_id(t) ((t)->hdr.id)
#define y_kind(t) ((t)->hdr.kind)
#define y_cast(t, T) ((T *)&(t)->hdr)

#define y_is_unit(t) (y_id(t) == PAW_TUNIT)
#define y_is_bool(t) (y_id(t) == PAW_TBOOL)
#define y_is_int(t) (y_id(t) == PAW_TINT)
#define y_is_float(t) (y_id(t) == PAW_TFLOAT)
#define y_is_scalar(t) (y_id(t) < PAW_TSTRING)
#define y_is_string(t) (y_id(t) == PAW_TSTRING)
#define y_is_array(t) (y_id(t) == PAW_TARRAY)
#define y_is_tuple(t) (y_id(t) == PAW_TTUPLE)
#define y_is_primitive(t) (y_id(t) <= PAW_TSTRING)
#define y_is_foreign(t) (y_id(t) == PAW_TFOREIGN)

#define y_is_class(t) (y_kind(t) == TYPE_CLASS)
#define y_is_function(t) (y_kind(t) == TYPE_SIGNATURE)
#define y_is_module(t) (y_kind(t) == TYPE_MODULE)

static inline const char *pawY_name(int type)
{
    switch (type) {
        case PAW_TBOOL:
            return "boolean";
        case PAW_TINT:
            return "integer";
        case PAW_TFLOAT:
            return "float";
        case PAW_TSTRING:
            return "string";
        case PAW_TFUNCTION:
            return "function";
        case PAW_TCLASS:
            return "class";
        case PAW_TFOREIGN:
            return "foreign";
        case PAW_TMODULE:
            return "module";
        default:
            return "?";
    }
}

#define pawY_is_same(a, b) ((a) == (b))

void pawY_init(paw_Env *P);
void pawY_uninit(paw_Env *P);

Type *pawY_new_type(paw_Env *P);

#endif // PAW_TYPE_H
