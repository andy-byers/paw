// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_TYPE_H
#define PAW_TYPE_H

#include "paw.h"

struct Type;

// address of unique Type
typedef const struct Type *TypeTag;

typedef struct ArrayType {
    TypeTag elem;
} ArrayType;

typedef struct MapType {
    TypeTag key;
    TypeTag value;
} MapType;

typedef struct ClassType {
    TypeTag *fields;
    int nfields;
} ClassType;

typedef struct FnType {
    TypeTag ret; // return type
    TypeTag *param; // parameter types
    int nparam; // number of parameters
} FnType;

typedef struct BoundFnType {
    ClassType recv;
    FnType func;
} BoundFnType;

// TODO: Keep 1 TypeTag with each global variable, since they are accessible from the C API.
// Keep 1 TypeTag with each local/upvalue variable's debug info for error messages
typedef struct Type {
    int code;
    union {
        FnType f;
        ArrayType a;
        MapType m;
        ClassType c;
        BoundFnType b;
    };
} Type;

extern TypeTag kTag0;
#define is_type0(t) ((t) == kTag0)

#define pawY_is_same(a, b) ((a) == (b))
paw_Bool pawY_is_similar(TypeTag a, TypeTag b);
int pawY_common(TypeTag a, TypeTag b, TypeTag *out);

void pawY_init(paw_Env *P);
void pawY_uninit(paw_Env *P);
Type *pawY_register_type(paw_Env *P, TypeTag tt);

Type *pawY_register_method(paw_Env *P, ClassType *recv, FnType *func);
Type *pawY_register_function(paw_Env *P, TypeTag *param, int nparam, TypeTag ret);
Type *pawY_register_class(paw_Env *P, TypeTag *fields, int nfields);
Type *pawY_register_array(paw_Env *fn, TypeTag elem);
Type *pawY_register_map(paw_Env *P, TypeTag key, TypeTag value);


#endif // PAW_TYPE_H
