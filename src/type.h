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
    TYPE_SIGNATURE,
    TYPE_TUPLE,
};

#define TYPE_HEADER paw_Type code; \
                    enum TypeKind kind : 8
struct TypeHeader {
    TYPE_HEADER;
};

struct Adt {
    TYPE_HEADER;
    DefId did;
};

struct Signature {
    TYPE_HEADER;
    paw_Type result;
};

struct TupleType {
    TYPE_HEADER;
};

struct Type {
    union {
        struct TypeHeader hdr;
        struct Adt adt;
        struct Signature sig;
        struct TupleType tuple;
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
};

#define DEF_HEADER String *name; \
                   paw_Type code; \
                   enum DefKind kind : 7; \
                   paw_Bool is_pub : 1
struct DefHeader {
    DEF_HEADER;
};

struct VariantDef {
    DEF_HEADER;
    uint8_t k;
    int nfields;
    DefId *fields;
};

struct AdtDef {
    DEF_HEADER;
    paw_Bool is_struct : 1;
    int nfields;
    DefId *fields;
};

struct FuncDef {
    DEF_HEADER;
    ValueId vid;
    paw_Type self;
    int ntypes;
    paw_Type *types;
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
    };
    int nsubdefs;
    DefId subdefs[];
};

#define Y_CAST_DEF(p) CAST(struct Def *, p)
#define Y_CAST_TYPE(p) CAST(struct Type *, p)

#define Y_DEF(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->defs.count, (P)->defs.data[i])
#define Y_TYPE(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->types.count, (P)->types.data[i])
#define Y_PVAL(P, i) CHECK_EXP(0 <= (i) && (i) < (P)->vals.count, &(P)->vals.data[i])

struct Type *pawY_new_adt(paw_Env *P, int ntypes);
struct Type *pawY_new_signature(paw_Env *P, int nparams);
struct Type *pawY_new_tuple(paw_Env *P, int nelems);
struct Def *pawY_new_adt_def(paw_Env *P, int nfields);
struct Def *pawY_new_variant_def(paw_Env *P, int nfields);
struct Def *pawY_new_func_def(paw_Env *P, int ntypes);
struct Def *pawY_new_field_def(paw_Env *P);
struct Def *pawY_new_var_def(paw_Env *P);
void pawY_uninit(paw_Env *P);

void pawY_mangle_start(paw_Env *P, Buffer *buffer, const String *name);
void pawY_mangle_add_arg(paw_Env *P, Buffer *buffer, paw_Type code);
void pawY_mangle_finish(paw_Env *P, Buffer *buffer);

// Append a human-readable representation of the type with the given 'code' to the 
// end of the 'buffer'
void pawY_print_type(paw_Env *P, Buffer *buffer, paw_Type code);

#endif // PAW_TYPE_H
