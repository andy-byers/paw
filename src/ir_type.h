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
    X(Generic)

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
};

struct IrGeneric {
    IR_TYPE_HEADER;
    DeclId did;
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

IrType *pawIr_new_type_(struct Compiler *C);

static inline IrType *pawIr_new_adt(struct Compiler *C, DeclId did, struct IrTypeList *types)
{
    IrType *t = pawIr_new_type_(C);
    *t = (IrType){
        .Adt_.kind = kIrAdt,
        .Adt_.did = did,
        .Adt_.types = types,
    };
    return t;
}

static inline IrType *pawIr_new_func_ptr(struct Compiler *C, struct IrTypeList *params, IrType *result)
{
    IrType *t = pawIr_new_type_(C);
    *t = (IrType){
        .FuncPtr_.kind = kIrFuncPtr,
        .FuncPtr_.params = params,
        .FuncPtr_.result = result,
    };
    return t;
}

static inline IrType *pawIr_new_signature(struct Compiler *C, DeclId did, struct IrTypeList *types, struct IrTypeList *params, IrType *result)
{
    IrType *t = pawIr_new_type_(C);
    *t = (IrType){
        .Signature_.kind = kIrSignature,
        .Signature_.did = did,
        .Signature_.params = params,
        .Signature_.result = result,
        .Signature_.types = types,
    };
    return t;
}

static inline IrType *pawIr_new_tuple(struct Compiler *C, struct IrTypeList *elems)
{
    IrType *t = pawIr_new_type_(C);
    *t = (IrType){
        .Tuple_.kind = kIrTuple,
        .Tuple_.elems = elems,
    };
    return t;
}

static inline IrType *pawIr_new_infer(struct Compiler *C, int depth, int index)
{
    IrType *t = pawIr_new_type_(C);
    *t = (IrType){
        .Infer_.kind = kIrInfer,
        .Infer_.depth = depth,
        .Infer_.index = index,
    };
    return t;
}

static inline IrType *pawIr_new_generic(struct Compiler *C, DeclId did)
{
    IrType *t = pawIr_new_type_(C);
    *t = (IrType){
        .Generic_.kind = kIrGeneric,
        .Generic_.did = did,
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
                      String *name;
struct IrDefHeader {
    IR_DEF_HEADER;
    IrType *type;
};

struct IrGenericDef {
    IR_DEF_HEADER;
    IrType *type;
};

struct IrParamDef {
    IR_DEF_HEADER;
    IrType *type;
};

struct IrFieldDef {
    IR_DEF_HEADER;
    paw_Bool is_pub : 1;
    IrType *type;
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

static inline IrDef *pawIr_new_generic_def(struct Compiler *C, DeclId did, String *name, IrType *type)
{
    IrDef *def = pawIr_new_def(C);
    *def = (IrDef){
        .GenericDef_.kind = kIrGenericDef,
        .GenericDef_.did = did,
        .GenericDef_.name = name,
        .GenericDef_.type = type,
    };
    return def;
}

static inline IrDef *pawIr_new_param_def(struct Compiler *C, DeclId did, String *name, IrType *type)
{
    IrDef *def = pawIr_new_def(C);
    *def = (IrDef){
        .ParamDef_.kind = kIrParamDef,
        .ParamDef_.did = did,
        .ParamDef_.name = name,
        .ParamDef_.type = type,
    };
    return def;
}

static inline IrDef *pawIr_new_field_def(struct Compiler *C, DeclId did, String *name, IrType *type, paw_Bool is_pub)
{
    IrDef *def = pawIr_new_def(C);
    *def = (IrDef){
        .FieldDef_.kind = kIrFieldDef,
        .FieldDef_.did = did,
        .FieldDef_.is_pub = is_pub,
        .FieldDef_.name = name,
        .FieldDef_.type = type,
    };
    return def;
}

static inline IrDef *pawIr_new_variant_def(struct Compiler *C, DeclId did, int discr, String *name, struct IrFieldList *fields)
{
    IrDef *def = pawIr_new_def(C);
    *def = (IrDef){
        .VariantDef_.kind = kIrVariantDef,
        .VariantDef_.did = did,
        .VariantDef_.fields = fields,
        .VariantDef_.discr = discr,
        .VariantDef_.name = name,
    };
    return def;
}

static inline IrDef *pawIr_new_func_def(struct Compiler *C, DeclId did, String *name, struct IrGenericList *generics, struct IrParamList *params, paw_Bool is_pub)
{
    IrDef *def = pawIr_new_def(C);
    *def = (IrDef){
        .FuncDef_.kind = kIrFuncDef,
        .FuncDef_.did = did,
        .FuncDef_.generics = generics,
        .FuncDef_.params = params,
        .FuncDef_.is_pub = is_pub,
        .FuncDef_.name = name,
    };
    return def;
}

static inline IrDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, String *name, struct IrGenericList *generics, struct IrVariantList *variants, paw_Bool is_pub, paw_Bool is_struct)
{
    IrDef *def = pawIr_new_def(C);
    *def = (IrDef){
        .AdtDef_.kind = kIrAdtDef,
        .AdtDef_.did = did,
        .AdtDef_.generics = generics,
        .AdtDef_.variants = variants,
        .AdtDef_.is_struct = is_struct,
        .AdtDef_.is_pub = is_pub,
        .AdtDef_.name = name,
    };
    return def;
}


#define IR_KINDOF(node) ((node)->hdr.kind)
#define IR_CAST_TYPE(p) CAST(IrType *, p)
#define IR_TYPE_DID(type) (IrIsAdt(type) ? IrGetAdt(type)->did : \
        IrIsSignature(type) ? IrGetSignature(type)->did : IrGetGeneric(type)->did)
#define IR_TYPE_SUBTYPES(type) (IrIsAdt(type) ? IrGetAdt(type)->types : \
        IrGetSignature(type)->types)
#define IR_IS_FUNC_TYPE(p) (IrIsFuncPtr(p) || IrIsSignature(p))
#define IR_FPTR(p) CHECK_EXP(IR_IS_FUNC_TYPE(p), &(p)->FuncPtr_)
#define IR_IS_UNIT_T(x) (IrIsAdt(x) && IR_TYPE_DID(x).value == PAW_TUNIT)
#define IR_IS_BASIC_T(x) (IrIsAdt(x) && IR_TYPE_DID(x).value <= PAW_TSTR)

DEFINE_LIST(struct Compiler, pawIr_type_list_, IrTypeList, IrType *)
DEFINE_LIST(struct Compiler, pawIr_adt_list_, IrAdtList, struct IrAdtDef *)
DEFINE_LIST(struct Compiler, pawIr_field_list_, IrFieldList, struct IrFieldDef *)
DEFINE_LIST(struct Compiler, pawIr_variant_list_, IrVariantList, struct IrVariantDef *)
DEFINE_LIST(struct Compiler, pawIr_generic_list_, IrGenericList, struct IrGenericDef *)
DEFINE_LIST(struct Compiler, pawIr_param_list_, IrParamList, struct IrParamDef *)

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
