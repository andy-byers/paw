// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_IR_TYPE_H
#define PAW_IR_TYPE_H

#include "hir.h"
#include "mir.h"

typedef struct IrType IrType;

#define IR_TYPE_LIST(X) \
    X(Adt)              \
    X(FnPtr)          \
    X(Signature)        \
    X(Tuple)            \
    X(Never)            \
    X(Infer)            \
    X(Generic)          \
    X(TraitObj)

enum IrTypeKind {
#define DEFINE_ENUM(X) kIr##X,
    IR_TYPE_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define IR_TYPE_HEADER enum IrTypeKind kind : 8
struct IrTypeHeader {
    IR_TYPE_HEADER;
};

struct IrAdt {
    IR_TYPE_HEADER;
    DeclId did;
    struct IrTypeList *types;
};

#define IR_FUNC_HEADER         \
    IR_TYPE_HEADER;            \
    struct IrTypeList *params; \
    IrType *result
struct IrFnPtr {
    IR_FUNC_HEADER;
};

struct IrSignature {
    IR_FUNC_HEADER;
    DeclId did;
    struct IrTypeList *types;
    struct IrType *self;
};

struct IrTuple {
    IR_TYPE_HEADER;
    struct IrTypeList *elems;
};

struct IrNever {
    IR_TYPE_HEADER;
};

struct IrInfer {
    IR_TYPE_HEADER;
    int depth;
    int index;
    struct IrTypeList *bounds;
};

struct IrGeneric {
    IR_TYPE_HEADER;
    DeclId did;
    struct IrTypeList *bounds;
};

struct IrTraitObj {
    IR_TYPE_HEADER;
    DeclId did;
    struct IrTypeList *types;
};

static char const *kIrTypeNames[] = {
#define DEFINE_NAME(X) "Ir" #X,
    IR_TYPE_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

struct IrType {
    union {
        struct IrTypeHeader hdr;
#define DEFINE_VARIANTS(X) struct Ir##X X##_;
        IR_TYPE_LIST(DEFINE_VARIANTS)
#undef DEFINE_VARIANTS
    };
};

#define DEFINE_ACCESS(X)                                              \
    static inline paw_Bool IrIs##X(const IrType *node)                \
    {                                                                 \
        return node->hdr.kind == kIr##X;                              \
    }                                                                 \
    static inline struct Ir##X *IrGet##X(IrType *node)                \
    {                                                                 \
        paw_assert(IrIs##X(node));                                    \
        return &node->X##_;                                           \
    }                                                                 \
    static inline struct Ir##X const *IrGet##X##K(IrType const *node) \
    {                                                                 \
        paw_assert(IrIs##X(node));                                    \
        return &node->X##_;                                           \
    }
IR_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

IrType *pawIr_new_type(struct Compiler *C);

inline static IrType *pawIr_new_adt(struct Compiler *C, DeclId did, struct IrTypeList *types)
{
    IrType *t = pawIr_new_type(C);
    t->Adt_ = (struct IrAdt){
        .kind = kIrAdt,
        .did = did,
        .types = types,
    };
    return t;
}

inline static IrType *pawIr_new_fn_ptr(struct Compiler *C, struct IrTypeList *params, IrType *result)
{
    IrType *t = pawIr_new_type(C);
    t->FnPtr_ = (struct IrFnPtr){
        .kind = kIrFnPtr,
        .params = params,
        .result = result,
    };
    return t;
}

inline static IrType *pawIr_new_signature(struct Compiler *C, DeclId did, struct IrTypeList *types, struct IrTypeList *params, IrType *result)
{
    IrType *t = pawIr_new_type(C);
    t->Signature_ = (struct IrSignature){
        .kind = kIrSignature,
        .did = did,
        .params = params,
        .result = result,
        .types = types,
    };
    return t;
}

inline static IrType *pawIr_new_tuple(struct Compiler *C, struct IrTypeList *elems)
{
    IrType *t = pawIr_new_type(C);
    t->Tuple_ = (struct IrTuple){
        .kind = kIrTuple,
        .elems = elems,
    };
    return t;
}

inline static IrType *pawIr_new_never(struct Compiler *C)
{
    IrType *t = pawIr_new_type(C);
    t->Never_ = (struct IrNever){
        .kind = kIrNever,
    };
    return t;
}

inline static IrType *pawIr_new_infer(struct Compiler *C, int depth, int index, struct IrTypeList *bounds)
{
    IrType *t = pawIr_new_type(C);
    t->Infer_ = (struct IrInfer){
        .kind = kIrInfer,
        .depth = depth,
        .index = index,
        .bounds = bounds,
    };
    return t;
}

inline static IrType *pawIr_new_generic(struct Compiler *C, DeclId did, struct IrTypeList *bounds)
{
    IrType *t = pawIr_new_type(C);
    t->Generic_ = (struct IrGeneric){
        .kind = kIrGeneric,
        .did = did,
        .bounds = bounds,
    };
    return t;
}

inline static IrType *pawIr_new_trait_obj(struct Compiler *C, DeclId did, struct IrTypeList *types)
{
    IrType *t = pawIr_new_type(C);
    t->TraitObj_ = (struct IrTraitObj){
        .kind = kIrTraitObj,
        .did = did,
        .types = types,
    };
    return t;
}

struct IrParam {
    Str *name;
    IrType *type;
};

struct IrFieldDef {
    Str *name;
    DeclId did;
    paw_Bool is_pub : 1;
};

struct IrGenericDef {
    Str *name;
    DeclId did;
};

struct IrVariantDef {
    Str *name;
    struct IrFieldDefs *fields;
    DeclId did;
    DeclId cons_did;
    DeclId base_did;
    int discr;
};

struct IrFnDef {
    Str *name;
    struct Annotations *annos;
    struct IrGenericDefs *generics;
    struct IrParams *params;
    DeclId did;
    paw_Bool is_pub : 1;
    paw_Bool is_extern : 1;
};

struct IrAdtDef {
    Str *name;
    struct IrGenericDefs *generics;
    struct IrVariantDefs *variants;
    DeclId did;
    paw_Bool is_inline : 1;
    paw_Bool is_struct : 1;
    paw_Bool is_pub : 1;
};

inline static struct IrGenericDef *pawIr_new_generic_def(struct Compiler *C, DeclId did, Str *name)
{
    struct IrGenericDef *def = P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrGenericDef){
        .did = did,
        .name = name,
    };
    return def;
}

inline static struct IrFieldDef *pawIr_new_field_def(struct Compiler *C, DeclId did, Str *name, paw_Bool is_pub)
{
    struct IrFieldDef *def = P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrFieldDef){
        .did = did,
        .name = name,
        .is_pub = is_pub,
    };
    return def;
}

inline static struct IrVariantDef *pawIr_new_variant_def(struct Compiler *C, DeclId did, DeclId cons_did, DeclId base_did, int discr, Str *name, struct IrFieldDefs *fields)
{
    struct IrVariantDef *def = P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrVariantDef){
        .did = did,
        .cons_did = cons_did,
        .base_did = base_did,
        .fields = fields,
        .discr = discr,
        .name = name,
    };
    return def;
}

inline static struct IrFnDef *pawIr_new_fn_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, struct IrParams *params, paw_Bool is_pub)
{
    struct IrFnDef *def = P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrFnDef){
        .did = did,
        .generics = generics,
        .params = params,
        .is_pub = is_pub,
        .name = name,
    };
    return def;
}

inline static struct IrAdtDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, struct IrVariantDefs *variants, paw_Bool is_pub, paw_Bool is_struct, paw_Bool is_inline)
{
    struct IrAdtDef *def = P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrAdtDef){
        .did = did,
        .generics = generics,
        .variants = variants,
        .is_inline = is_inline,
        .is_struct = is_struct,
        .is_pub = is_pub,
        .name = name,
    };
    return def;
}

#define IR_KINDOF(node) ((node)->hdr.kind)
#define IR_CAST_TYPE(p) CAST(IrType *, p)
#define IR_TYPE_DID(type) (IrIsAdt(type) ? IrGetAdt(type)->did : \
        IrIsSignature(type) ? IrGetSignature(type)->did : \
        IrIsGeneric(type) ? IrGetGeneric(type)->did : IrGetTraitObj(type)->did)
#define IR_TYPE_SUBTYPES(type) (IrIsAdt(type) ? IrGetAdt(type)->types : \
        IrIsSignature(type) ? IrGetSignature(type)->types : IrGetTraitObj(type)->types)
#define IR_IS_FUNC_TYPE(p) (IrIsFnPtr(p) || IrIsSignature(p))
#define IR_FPTR(p) CHECK_EXP(IR_IS_FUNC_TYPE(p), &(p)->FnPtr_)

DEFINE_LIST(struct Compiler, IrTypeList, IrType *)
DEFINE_LIST(struct Compiler, IrVariantDefs, struct IrVariantDef *)
DEFINE_LIST(struct Compiler, IrGenericDefs, struct IrGenericDef *)
DEFINE_LIST(struct Compiler, IrFieldDefs, struct IrFieldDef *)
DEFINE_LIST(struct Compiler, IrParams, struct IrParam)

struct IrType *pawIr_resolve_trait_method(struct Compiler *C, struct IrGeneric *target, Str *name);

DeclId pawIr_next_did(struct Compiler *C, int mod);

IrType *pawIr_get_type(struct Compiler *C, NodeId id);
void pawIr_set_type(struct Compiler *C, NodeId id, IrType *type);
struct IrVariantDef *pawIr_get_variant_def(struct Compiler *C, DeclId did);
struct IrAdtDef *pawIr_get_adt_def(struct Compiler *C, DeclId did);
struct IrFnDef *pawIr_get_fn_def(struct Compiler *C, DeclId did);
struct IrType *pawIr_get_def_type(struct Compiler *C, DeclId did);

paw_Uint pawIr_type_hash(struct Compiler *C, IrType *t);
paw_Bool pawIr_type_equals(struct Compiler *C, IrType *a, IrType *b);
#define IR_TYPE_HASH(Ctx_, Type_) pawIr_type_hash((Ctx_)->C, Type_)
#define IR_TYPE_EQUALS(Ctx_, A_, B_) pawIr_type_equals((Ctx_)->C, A_, B_)

DEFINE_MAP(struct Compiler, RttiMap, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, struct RttiType *)

void pawIr_validate_type(struct Compiler *C, IrType *type);
IrType *pawIr_substitute_self(struct Compiler *C, IrType *trait, IrType *adt, IrType *method);

static IrTypeList *ir_signature_types(IrType *type)
{
    return IrGetSignature(type)->types;
}

static IrTypeList *ir_adt_types(IrType *type)
{
    return IrGetAdt(type)->types;
}

static IrType *ir_list_elem(IrType *type)
{
    return IrTypeList_get(IrGetAdt(type)->types, 0);
}

static IrType *ir_map_key(IrType *type)
{
    return IrTypeList_get(IrGetAdt(type)->types, 0);
}

static IrType *ir_map_value(IrType *type)
{
    return IrTypeList_get(IrGetAdt(type)->types, 1);
}

static paw_Bool ir_is_boxed(struct Compiler *C, IrType *type)
{
    if (!IrIsAdt(type)) return PAW_FALSE;
    struct IrAdtDef *def = pawIr_get_adt_def(C, IR_TYPE_DID(type));
    return !def->is_inline;
}

char const *pawIr_print_type(struct Compiler *C, IrType *type);

DEFINE_LIST(struct Compiler, TraitOwnerList, struct IrTypeList *)
DEFINE_MAP(struct Compiler, TraitOwners, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, TraitOwnerList *)

#endif // PAW_IR_TYPE_H
