// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// type.h: Runtime type information for paw programs
//
// Structures in this file are created during the codegen pass and stored for
// use in error messages and RTTI queries.
#ifndef PAW_TYPE_H
#define PAW_TYPE_H

#include "opcode.h"
#include "paw.h"
#include "str.h"

typedef struct Type Type;
typedef uint16_t DefId;

typedef enum TypeKind { // type->...
    TYPE_GENERIC, // generic
    TYPE_ADT, // adt
    TYPE_FUNC_SIG, // func
    TYPE_MODULE, // mod
} TypeKind;

#define TYPE_HEADER \
    DefId def;      \
    TypeKind kind : 8
typedef struct TypeHeader {
    TYPE_HEADER;
} TypeHeader;

// Represents a generic type parameter
typedef struct Generic {
    TYPE_HEADER;
    String *name;
} Generic;

typedef struct Binder {
    Type **types;
    int count;
} Binder;

// Represents a function signature
typedef struct FuncSig {
    TYPE_HEADER; // common initial sequence
    Binder params; // parameter types
    Type *return_; // return type
} FuncSig;

// Represents a structure or enumeration type
typedef struct Adt {
    TYPE_HEADER; // common initial sequence
    Binder types;
} Adt;

// Represents the type of a Paw module
// Note that basic types ('int', 'float', etc.) are created only once, at the
// start of the root module's type vector. Included modules reference these
// Type objects in the root.
typedef struct Module {
    TYPE_HEADER; // common initial sequence
    struct Module *includes; // included modules
    Type **types;
    int ntypes;
    int capacity;
} Module;

struct Type {
    union {
        TypeHeader hdr;
        Generic generic;
        Adt adt;
        FuncSig func;
        Module mod;
    };
};

#define y_cast(x) ((Type *)(x))
#define y_code(t) ((t)->hdr.def)
#define y_kind(t) ((t)->hdr.kind)

#define y_is_unit(t) (y_is_basic(t) && y_code(t) == PAW_TUNIT)
#define y_is_bool(t) (y_is_basic(t) && y_code(t) == PAW_TBOOL)
#define y_is_int(t) (y_is_basic(t) && y_code(t) == PAW_TINT)
#define y_is_float(t) (y_is_basic(t) && y_code(t) == PAW_TFLOAT)
#define y_is_string(t) (y_is_basic(t) && y_code(t) == PAW_TSTRING)

#define y_is_basic(t) (y_kind(t) == TYPE_BASIC)
#define y_is_generic(t) (y_kind(t) == TYPE_GENERIC)
#define y_is_adt(t) (y_kind(t) == TYPE_ADT)
#define y_is_func(t) (y_kind(t) == TYPE_FUNC)
#define y_is_module(t) (y_kind(t) == TYPE_MODULE)

#endif // PAW_TYPE_H
