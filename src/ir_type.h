// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_IR_TYPE_H
#define PAW_IR_TYPE_H

#include "hir.h"
#include "mir.h"

typedef struct IrType IrType;
typedef struct IrDef IrDef;

#define IR_TYPE_LIST(X) \
    X(Adt) \
    X(FuncPtr) \
    X(Signature) \
    X(Tuple) \
    X(Infer) \
    X(Generic) \
    X(TraitObj)

#define IR_DEF_LIST(X) \
    X(AdtDef) \
    X(FuncDef) \
    X(ParamDef) \
    X(FieldDef) \
    X(VariantDef) \
    X(GenericDef)

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

#define IR_FUNC_HEADER IR_TYPE_HEADER; \
               struct IrTypeList *params; \
               IrType *result
struct IrFuncPtr {
    IR_FUNC_HEADER;
};

struct IrSignature {
    IR_FUNC_HEADER;
    DeclId did;
    struct IrTypeList *types;
};

struct IrTuple {
    IR_TYPE_HEADER;
    struct IrTypeList *elems;
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

static const char *kIrTypeNames[] = {
#define DEFINE_NAME(X) "Ir"#X,
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

#define DEFINE_ACCESS(X) \
    static inline paw_Bool IrIs##X(const IrType *node) { \
        return node->hdr.kind == kIr##X; \
    } \
    static inline struct Ir##X *IrGet##X(IrType *node) { \
        paw_assert(IrIs##X(node)); \
        return &node->X##_; \
    }
    IR_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

IrType *pawIr_new_type(struct Compiler *C);

static inline IrType *pawIr_new_adt(struct Compiler *C, DeclId did, struct IrTypeList *types)
{
    IrType *t = pawIr_new_type(C);
    t->Adt_ = (struct IrAdt){
        .kind = kIrAdt,
        .did = did,
        .types = types,
    };
    return t;
}

static inline IrType *pawIr_new_func_ptr(struct Compiler *C, struct IrTypeList *params, IrType *result)
{
    IrType *t = pawIr_new_type(C);
    t->FuncPtr_ = (struct IrFuncPtr){
        .kind = kIrFuncPtr,
        .params = params,
        .result = result,
    };
    return t;
}

static inline IrType *pawIr_new_signature(struct Compiler *C, DeclId did, struct IrTypeList *types, struct IrTypeList *params, IrType *result)
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

static inline IrType *pawIr_new_tuple(struct Compiler *C, struct IrTypeList *elems)
{
    IrType *t = pawIr_new_type(C);
    t->Tuple_ = (struct IrTuple){
        .kind = kIrTuple,
        .elems = elems,
    };
    return t;
}

static inline IrType *pawIr_new_infer(struct Compiler *C, int depth, int index, struct IrTypeList *bounds)
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

static inline IrType *pawIr_new_generic(struct Compiler *C, DeclId did, struct IrTypeList *bounds)
{
    IrType *t = pawIr_new_type(C);
    t->Generic_ = (struct IrGeneric){
        .kind = kIrGeneric,
        .did = did,
        .bounds = bounds,
    };
    return t;
}

static inline IrType *pawIr_new_trait_obj(struct Compiler *C, DeclId did, struct IrTypeList *types)
{
    IrType *t = pawIr_new_type(C);
    t->TraitObj_ = (struct IrTraitObj){
        .kind = kIrTraitObj,
        .did = did,
        .types = types,
    };
    return t;
}


enum IrDefKind {
#define DEFINE_ENUM(X) kIr##X,
    IR_DEF_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};


#define IR_DEF_HEADER enum IrDefKind kind : 8; \
                      DeclId did; \
                      String *name
struct IrDefHeader {
    IR_DEF_HEADER;
    IrType *type;
};

struct IrGenericDef {
    IR_DEF_HEADER;
};

struct IrParamDef {
    IR_DEF_HEADER;
};

struct IrFieldDef {
    IR_DEF_HEADER;
    paw_Bool is_pub : 1;
};

struct IrVariantDef {
    IR_DEF_HEADER;
    DeclId cons;
    int discr;
    struct IrFieldList *fields;
};

struct IrFuncDef {
    IR_DEF_HEADER;
    paw_Bool is_pub : 1;
    struct IrGenericList *generics;
    struct IrParamList *params;
};

struct IrAdtDef {
    IR_DEF_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_struct : 1;
    struct IrGenericList *generics;
    struct IrVariantList *variants;
};

static const char *kIrDefNames[] = {
#define DEFINE_NAME(X) "Ir"#X,
    IR_DEF_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

struct IrDef {
    union {
        struct IrDefHeader hdr;
#define DEFINE_VARIANTS(X) struct Ir##X X##_;
        IR_DEF_LIST(DEFINE_VARIANTS)
#undef DEFINE_VARIANTS
    };
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool IrIs##X(const IrDef *node) { \
        return node->hdr.kind == kIr##X; \
    } \
    static inline struct Ir##X *IrGet##X(IrDef *node) { \
        paw_assert(IrIs##X(node)); \
        return &node->X##_; \
    }
    IR_DEF_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

IrDef *pawIr_new_def(struct Compiler *C);

static inline IrDef *pawIr_new_generic_def(struct Compiler *C, DeclId did, String *name)
{
    IrDef *def = pawIr_new_def(C);
    def->GenericDef_ = (struct IrGenericDef){
        .kind = kIrGenericDef,
        .did = did,
        .name = name,
    };
    return def;
}

static inline IrDef *pawIr_new_param_def(struct Compiler *C, DeclId did, String *name)
{
    IrDef *def = pawIr_new_def(C);
    def->ParamDef_ = (struct IrParamDef){
        .kind = kIrParamDef,
        .did = did,
        .name = name,
    };
    return def;
}

static inline IrDef *pawIr_new_field_def(struct Compiler *C, DeclId did, String *name, paw_Bool is_pub)
{
    IrDef *def = pawIr_new_def(C);
    def->FieldDef_ = (struct IrFieldDef){
        .kind = kIrFieldDef,
        .did = did,
        .is_pub = is_pub,
        .name = name,
    };
    return def;
}

static inline IrDef *pawIr_new_variant_def(struct Compiler *C, DeclId did, int discr, String *name, struct IrFieldList *fields)
{
    IrDef *def = pawIr_new_def(C);
    def->VariantDef_ = (struct IrVariantDef){
        .kind = kIrVariantDef,
        .did = did,
        .fields = fields,
        .discr = discr,
        .name = name,
    };
    return def;
}

static inline IrDef *pawIr_new_func_def(struct Compiler *C, DeclId did, String *name, struct IrGenericList *generics, struct IrParamList *params, paw_Bool is_pub)
{
    IrDef *def = pawIr_new_def(C);
    def->FuncDef_ = (struct IrFuncDef){
        .kind = kIrFuncDef,
        .did = did,
        .generics = generics,
        .params = params,
        .is_pub = is_pub,
        .name = name,
    };
    return def;
}

static inline IrDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, String *name, struct IrGenericList *generics, struct IrVariantList *variants, paw_Bool is_pub, paw_Bool is_struct)
{
    IrDef *def = pawIr_new_def(C);
    def->AdtDef_ = (struct IrAdtDef){
        .kind = kIrAdtDef,
        .did = did,
        .generics = generics,
        .variants = variants,
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
#define IR_IS_FUNC_TYPE(p) (IrIsFuncPtr(p) || IrIsSignature(p))
#define IR_FPTR(p) CHECK_EXP(IR_IS_FUNC_TYPE(p), &(p)->FuncPtr_)

DEFINE_LIST(struct Compiler, pawIr_def_list_, IrDefList, IrDef *)
DEFINE_LIST(struct Compiler, pawIr_type_list_, IrTypeList, IrType *)
DEFINE_LIST(struct Compiler, pawIr_adt_list_, IrAdtList, struct IrAdtDef *)
DEFINE_LIST(struct Compiler, pawIr_field_list_, IrFieldList, struct IrFieldDef *)
DEFINE_LIST(struct Compiler, pawIr_variant_list_, IrVariantList, struct IrVariantDef *)
DEFINE_LIST(struct Compiler, pawIr_generic_list_, IrGenericList, struct IrGenericDef *)
DEFINE_LIST(struct Compiler, pawIr_param_list_, IrParamList, struct IrParamDef *)

struct IrType *pawIr_resolve_trait_method(struct Compiler *C, struct IrGeneric *target, String *name);

DeclId pawIr_next_did(struct Compiler *C, int mod);

IrType *pawIr_get_type(struct Compiler *C, HirId hid);
IrDef *pawIr_get_def(struct Compiler *C, DeclId did);
void pawIr_set_type(struct Compiler *C, HirId hid, IrType *type);
void pawIr_set_def(struct Compiler *C, DeclId did, IrDef *def);

struct IrFuncDef *pawIr_get_func_def(struct Compiler *C, DeclId did);
struct IrVariantDef *pawIr_get_variant_def(struct Compiler *C, DeclId did);
struct IrGenericDef *pawIr_get_generic_def(struct Compiler *C, DeclId did);
struct IrAdtDef *pawIr_get_adt_def(struct Compiler *C, DeclId did);
struct IrParamDef *pawIr_get_param_def(struct Compiler *C, DeclId did);
struct IrFieldDef *pawIr_get_field_def(struct Compiler *C, DeclId did);

paw_Uint pawIr_type_hash(void *ctx, Value v);
paw_Bool pawIr_type_equals(void *ctx, Value lhs, Value rhs);
Map *pawIr_new_type_map(struct Compiler *C);

void pawIr_validate_type(struct Compiler *C, struct IrType *type);

static struct IrTypeList *ir_signature_types(IrType *type)
{
    return IrGetSignature(type)->types;
}

static struct IrTypeList *ir_adt_types(IrType *type)
{
    return IrGetAdt(type)->types;
}

static IrType *ir_list_elem(IrType *type)
{
    return K_LIST_GET(IrGetAdt(type)->types, 0);
}

static IrType *ir_map_key(IrType *type)
{
    return K_LIST_GET(IrGetAdt(type)->types, 0);
}

static IrType *ir_map_value(IrType *type)
{
    return K_LIST_GET(IrGetAdt(type)->types, 1);
}

const char *pawIr_print_type(struct Compiler *C, IrType *type);

#endif // PAW_IR_TYPE_H
