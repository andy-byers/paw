// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_IR_TYPE_H
#define PAW_IR_TYPE_H

#include "hir.h"
#include "mir.h"

struct IrType;

#define IR_TYPE_LIST(X) \
    X(Adt) \
    X(FuncPtr) \
    X(Signature) \
    X(Tuple) \
    X(Infer) \
    X(Generic)

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
               struct IrType *result
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

#define DEFINE_ACCESS(X) \
    static inline paw_Bool IrIs##X(const struct IrType *node) { \
        return node->hdr.kind == kIr##X; \
    } \
    static inline struct Ir##X *IrGet##X(struct IrType *node) { \
        paw_assert(IrIs##X(node)); \
        return &node->X##_; \
    }
    IR_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

#define IR_KINDOF(node) ((node)->hdr.kind)
#define IR_CAST_TYPE(p) CAST(struct IrType *, p)
#define IR_TYPE_DID(type) (IrIsAdt(type) ? IrGetAdt(type)->did : \
        IrIsSignature(type) ? IrGetSignature(type)->did : IrGetGeneric(type)->did)
#define IR_TYPE_SUBTYPES(type) (IrIsAdt(type) ? IrGetAdt(type)->types : \
        IrGetSignature(type)->types)
#define IR_IS_FUNC_TYPE(p) (IrIsFuncPtr(p) || IrIsSignature(p))
#define IR_FPTR(p) CHECK_EXP(IR_IS_FUNC_TYPE(p), &(p)->FuncPtr_)
#define IR_IS_UNIT_T(x) (IrIsAdt(x) && IR_TYPE_DID(x).value == PAW_TUNIT)
#define IR_IS_BASIC_T(x) (IrIsAdt(x) && IR_TYPE_DID(x).value <= PAW_TSTR)

DEFINE_LIST_V2(struct Compiler, pawIr_type_list_, IrTypeList, struct IrType *)

struct IrType *pawIr_new_type(struct Compiler *C, enum IrTypeKind kind);

struct IrType *pawIr_get_type(struct Compiler *C, HirId did);
void pawIr_set_type(struct Compiler *C, HirId did, struct IrType *type);

static struct IrTypeList *ir_signature_types(struct IrType *type)
{
    return IrGetSignature(type)->types;
}

static struct IrTypeList *ir_adt_types(struct IrType *type)
{
    return IrGetAdt(type)->types;
}

static struct IrType *ir_list_elem(struct IrType *type)
{
    return K_LIST_GET(IrGetAdt(type)->types, 0);
}

static struct IrType *ir_map_key(struct IrType *type)
{
    return K_LIST_GET(IrGetAdt(type)->types, 0);
}

static struct IrType *ir_map_value(struct IrType *type)
{
    return K_LIST_GET(IrGetAdt(type)->types, 1);
}

const char *pawIr_print_type(struct Compiler *C, struct IrType *type);


struct IrFieldDef {
    String *name;
    DeclId did;
    paw_Bool is_pub : 1;
};

struct IrVariantDef {
    struct IrFieldList *fields;
    String *name;
    int discr;
    DeclId xdid;
};

struct IrAdtDef {
    struct IrVariantList *variants;
    String *name;
    DeclId did;
    paw_Bool is_struct : 1;
    paw_Bool is_pub : 1;
};

DEFINE_LIST_V2(struct Compiler, pawIr_adt_list_, IrAdtList, struct IrAdtDef *)
DEFINE_LIST_V2(struct Compiler, pawIr_field_list_, IrFieldList, struct IrFieldDef *)
DEFINE_LIST_V2(struct Compiler, pawIr_variant_list_, IrVariantList, struct IrVariantDef *)

struct IrFieldDef *pawIr_new_field(struct Compiler *C, DeclId did, String *name, paw_Bool is_pub);
struct IrVariantDef *pawIr_new_variant_def(struct Compiler *C, DeclId xdid, int discr, String *name, struct IrFieldList *fields);
struct IrAdtDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, String *name, paw_Bool is_pub, paw_Bool is_struct);

#endif // PAW_IR_TYPE_H
