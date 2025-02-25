// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

// Note that for types 'struct Type' and 'struct Def' below, the active union
// member cannot be changed after it is set in one of the constructor functions.
// This is because we allocate exactly the amount of memory needed by the
// active member, which might contain a flexible array member. Common fields
// can be accessed through the '.hdr' member.

#ifndef PAW_TYPE_H
#define PAW_TYPE_H

#include "env.h"

enum TypeKind {
    TYPE_ADT,
    TYPE_FUNC_PTR,
    TYPE_SIGNATURE,
    TYPE_TUPLE,
    TYPE_TRAIT_OBJ,
};

#define TYPE_HEADER \
    paw_Type code;  \
    enum TypeKind kind : 8
struct TypeHeader {
    TYPE_HEADER;
};

struct Adt {
    TYPE_HEADER;
    ItemId iid;
    int size;
};

struct TraitObj {
    TYPE_HEADER;
};

#define FUNCTION_HEADER \
    TYPE_HEADER;        \
    paw_Type result
struct FuncPtr {
    FUNCTION_HEADER;
};

struct Signature {
    FUNCTION_HEADER;
    ItemId iid;
};

struct TupleType {
    TYPE_HEADER;
    int size;
};

struct Type {
    union {
        struct TypeHeader hdr;
        struct Adt adt;
        struct Signature sig;
        struct FuncPtr fptr;
        struct TupleType tuple;
        struct TraitObj trait;
    };
    int nsubtypes;
    paw_Type subtypes[];
};

enum DefKind {
    DEF_ADT,
    DEF_FUNC,
    DEF_VAR,
    DEF_FIELD,
    DEF_VARIANT,
    DEF_TRAIT,
};

// TODO: use these instead, don't need to be part of a union though, keep them separate like in ir_type.h
// struct FieldDef {
//    String *name;
//    ItemId iid;
//    paw_Bool is_pub : 1;
//};
//
// struct VariantDef {
//    struct FieldList *fields;
//    String *name;
//    int discr;
//    ItemId xdid;
//};
//
// struct AdtDef {
//    struct VariantList *variants;
//    String *name;
//    ItemId iid;
//    paw_Bool is_struct : 1;
//    paw_Bool is_pub : 1;
//};

#define DEF_HEADER         \
    String *name;          \
    String *modname;       \
    paw_Type code;         \
    ItemId iid;            \
    enum DefKind kind : 7; \
    paw_Bool is_pub : 1
struct DefHeader {
    DEF_HEADER;
};

struct VariantDef {
    DEF_HEADER;
    uint8_t k;
    int nfields;
    ItemId *fields;
};

struct AdtDef {
    DEF_HEADER;
    paw_Bool is_struct : 1;
    int nfields;
    ItemId *fields;
    String *mangled_name;
};

struct TraitDef {
    DEF_HEADER;
};

struct FuncDef {
    DEF_HEADER;
    ValueId vid;
    paw_Type self;
    int ntypes;
    paw_Type *types;
    String *mangled_name;
};

struct FieldDef {
    DEF_HEADER;
};

struct VarDef {
    DEF_HEADER;
    ValueId vid;
};

struct Def {
    union {
        struct DefHeader hdr;
        struct AdtDef adt;
        struct VariantDef variant;
        struct FuncDef func;
        struct VarDef var;
        struct TraitDef trait;
    };
};

#define Y_CAST_DEF(p) CAST(struct Def *, p)
#define Y_CAST_TYPE(p) CAST(struct Type *, p)

#define Y_DEF(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->defs.count, (P)->defs.data[i])
#define Y_TYPE(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->types.count, (P)->types.data[i])
#define Y_PVAL(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->vals.count, &(P)->vals.data[i])

struct Type *pawY_new_adt(paw_Env *P, ItemId iid, int ntypes);
struct Type *pawY_new_signature(paw_Env *P, ItemId iid, int nparams);
struct Type *pawY_new_func_ptr(paw_Env *P, int nparams);
struct Type *pawY_new_tuple(paw_Env *P, int nelems);
struct Type *pawY_new_trait_obj(paw_Env *P);
struct Def *pawY_new_adt_def(paw_Env *P, int nfields);
struct Def *pawY_new_trait_def(paw_Env *P);
struct Def *pawY_new_variant_def(paw_Env *P, int nfields);
struct Def *pawY_new_func_def(paw_Env *P, int ntypes);
struct Def *pawY_new_field_def(paw_Env *P);
struct Def *pawY_new_var_def(paw_Env *P);
void pawY_uninit(paw_Env *P);

void pawY_mangle_start(paw_Env *P, Buffer *buf);
void pawY_mangle_add_module(paw_Env *P, Buffer *buf, String const *name);
void pawY_mangle_add_name(paw_Env *P, Buffer *buf, String const *name);
void pawY_mangle_start_generic_args(paw_Env *P, Buffer *buf);
void pawY_mangle_finish_generic_args(paw_Env *P, Buffer *buf);
void pawY_mangle_add_arg(paw_Env *P, Buffer *buf, paw_Type code);

// Append a human-readable representation of the type with the given 'code' to the
// end of the 'buffer'
void pawY_print_type(paw_Env *P, Buffer *buffer, paw_Type code);

#endif // PAW_TYPE_H
