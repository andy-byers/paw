// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_RTTI_H
#define PAW_RTTI_H

#include "env.h"

enum RttiTypeKind {
    RTTI_TYPE_ADT,
    RTTI_TYPE_FN_PTR,
    RTTI_TYPE_FN_DEF,
    RTTI_TYPE_TUPLE,
    RTTI_TYPE_TRAIT,
    RTTI_TYPE_NEVER,
};

#define RTTI_TYPE_HEADER \
    paw_Type code;       \
    enum RttiTypeKind kind : 8
struct RttiTypeHeader {
    RTTI_TYPE_HEADER;
};

struct RttiAdt {
    RTTI_TYPE_HEADER;
    ItemId iid;
    int size;
};

struct RttiTrait {
    RTTI_TYPE_HEADER;
};

struct RttiNever {
    RTTI_TYPE_HEADER;
};

#define RTTI_FN_HEADER \
    RTTI_TYPE_HEADER;  \
    paw_Type result
struct RttiFnPtr {
    RTTI_FN_HEADER;
};

struct RttiFnDef {
    RTTI_FN_HEADER;
    ItemId iid;
};

struct RttiTuple {
    RTTI_TYPE_HEADER;
    int size;
};

typedef struct RttiType {
    union {
        struct RttiTypeHeader hdr;
        struct RttiAdt adt;
        struct RttiFnDef fdef;
        struct RttiFnPtr fptr;
        struct RttiTuple tuple;
        struct RttiTrait trait;
        struct RttiNever never;
    };
    int nsubtypes;
    paw_Type subtypes[];
} RttiType;

enum DefKind {
    DEF_ADT,
    DEF_FUNC,
    DEF_CONST,
    DEF_TRAIT,
};

struct RttiField {
    Str *name;
    paw_Type code;
    paw_Bool is_pub : 1;
};

struct RttiVariant {
    ItemId did;
    unsigned char discr;
    int num_fields;
    struct RttiField *fields;
    Str *name;
};

#define RTTI_DEF_HEADER    \
    Str *name;          \
    Str *modname;       \
    paw_Type code;         \
    ItemId iid;            \
    enum DefKind kind : 7; \
    paw_Bool is_pub : 1
struct DefHeader {
    RTTI_DEF_HEADER;
};

struct AdtDef {
    RTTI_DEF_HEADER;
    paw_Bool is_struct : 1;
    paw_Bool is_inline : 1;
    int nfields;
    ItemId *fields;
    Str *mangled_name;
};

struct TraitDef {
    RTTI_DEF_HEADER;
};

struct FuncDef {
    RTTI_DEF_HEADER;
    ValueId vid;
    paw_Type self;
    int ntypes;
    paw_Type *types;
    Str *mangled_name;
};

struct ConstDef {
    RTTI_DEF_HEADER;
    ValueId vid;
};

struct Def {
    union {
        struct DefHeader hdr;
        struct AdtDef adt;
        struct FuncDef func;
        struct TraitDef trait;
        struct ConstDef const_;
    };
};

#define RTTI_CAST_DEF(p) CAST(struct Def *, p)
#define RTTI_CAST_TYPE(p) CAST(RttiType *, p)

#define RTTI_DEF(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->defs.count, (P)->defs.data[i])
#define RTTI_TYPE(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->types.count, (P)->types.data[i])
#define RTTI_PVAL(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->vals.count, &(P)->vals.data[i])

struct RttiType *pawRtti_new_adt(paw_Env *P, ItemId iid, int ntypes, int size);
struct RttiType *pawRtti_new_signature(paw_Env *P, ItemId iid, int nparams);
struct RttiType *pawRtti_new_func_ptr(paw_Env *P, int nparams);
struct RttiType *pawRtti_new_tuple(paw_Env *P, int nelems, int size);
struct RttiType *pawRtti_new_trait(paw_Env *P);
struct RttiType *pawRtti_new_never(paw_Env *P);
struct Def *pawRtti_new_adt_def(paw_Env *P, int nfields);
struct Def *pawRtti_new_trait_def(paw_Env *P);
struct Def *pawRtti_new_func_def(paw_Env *P, int ntypes);
struct Def *pawRtti_new_const_def(paw_Env *P);
struct RttiVariant *pawRtti_new_variant(paw_Env *P, Str *name, struct RttiField *fields, int num_fields);
struct RttiField *pawRtti_new_field(paw_Env *P, Str *name, paw_Type code, paw_Bool is_pub);
void pawRtti_uninit(paw_Env *P);

void pawRtti_mangle_start(paw_Env *P, Buffer *buf);
void pawRtti_mangle_add_module(paw_Env *P, Buffer *buf, Str const *name);
void pawRtti_mangle_add_name(paw_Env *P, Buffer *buf, Str const *name);
void pawRtti_mangle_start_generic_args(paw_Env *P, Buffer *buf);
void pawRtti_mangle_finish_generic_args(paw_Env *P, Buffer *buf);
void pawRtti_mangle_add_arg(paw_Env *P, Buffer *buf, paw_Type code);

static int rtti_stack_size(paw_Env *P, RttiType const *rtti)
{
    if (rtti->hdr.kind == RTTI_TYPE_ADT) {
        // ADTs can be either inline or boxed
        struct Def *def = RTTI_DEF(P, rtti->adt.iid);
        return def->adt.is_inline ? rtti->adt.size : 1;
    } else if (rtti->hdr.kind == RTTI_TYPE_TUPLE) {
        // tuples are always inline
        return rtti->tuple.size;
    } else {
        // all other types are either primitives or boxed
        return 1;
    }
}


// Append a human-readable representation of the type with the given 'code' to the
// end of the 'buffer'
void pawRtti_print_type(paw_Env *P, Buffer *buffer, paw_Type code);

#endif // PAW_RTTI_H
