// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_IR_TYPE_H
#define PAW_IR_TYPE_H

#include "hir.h"
#include "mir.h"

typedef struct IrType IrType;
typedef struct IrTrait IrTrait;

#define IR_TYPE_LIST(X) \
    X(Unit)             \
    X(Bool)             \
    X(Char)             \
    X(Int)              \
    X(Float)            \
    X(Str)              \
    X(Ptr)              \
    X(Adt)              \
    X(FnPtr)            \
    X(Signature)        \
    X(Tuple)            \
    X(Never)            \
    X(Infer)            \
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

struct IrUnit {
    IR_TYPE_HEADER;
};

struct IrBool {
    IR_TYPE_HEADER;
};

struct IrChar {
    IR_TYPE_HEADER;
};

struct IrInt {
    IR_TYPE_HEADER;
};

struct IrFloat {
    IR_TYPE_HEADER;
};

struct IrStr {
    IR_TYPE_HEADER;
};

struct IrPtr {
    IR_TYPE_HEADER;
    struct IrType *pointee;
};

struct IrAdt {
    IR_TYPE_HEADER;
    DeclId did;
    struct IrTypeList *types;
};

struct IrFnPtr {
    IR_TYPE_HEADER;
    struct IrTypeList *params;
    IrType *result;
};

struct IrSignature {
    IR_TYPE_HEADER;
    DeclId did;
    struct IrTypeList *types;
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
};

struct IrGeneric {
    IR_TYPE_HEADER;
    DeclId did;
};

struct IrProjection {
    IR_TYPE_HEADER;
    DeclId did;
    IrType *type;
    IrType *assoc;
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

IrType *pawIr_new_unit(struct Compiler *C);
IrType *pawIr_new_bool(struct Compiler *C);
IrType *pawIr_new_char(struct Compiler *C);
IrType *pawIr_new_int(struct Compiler *C);
IrType *pawIr_new_float(struct Compiler *C);
IrType *pawIr_new_str(struct Compiler *C);
IrType *pawIr_new_ptr(struct Compiler *C, IrType *pointee);
IrType *pawIr_new_adt(struct Compiler *C, DeclId did, struct IrTypeList *types);
IrType *pawIr_new_fn_ptr(struct Compiler *C, struct IrTypeList *params, IrType *result);
IrType *pawIr_new_signature(struct Compiler *C, DeclId did, struct IrTypeList *types);
IrType *pawIr_new_tuple(struct Compiler *C, struct IrTypeList *elems);
IrType *pawIr_new_never(struct Compiler *C);
IrType *pawIr_new_infer(struct Compiler *C, int depth, int index);
IrType *pawIr_new_generic(struct Compiler *C, DeclId did);


struct IrTrait {
    DeclId did;
    struct IrTypeList *types;
};

IrTrait *pawIr_new_trait(struct Compiler *C, DeclId did, struct IrTypeList *types);


enum IrDefKind {
    IR_FN_DEF,
    IR_ADT_DEF,
    IR_VARIANT_DEF,
    IR_IMPL_DEF,
    IR_TRAIT_DEF,
    IR_GENERIC_DEF,
    IR_FIELD_DEF,
};

void pawIr_set_def_kind(struct Compiler *C, DeclId did, enum IrDefKind kind);


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
    struct IrTraitList *bounds;
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
    IrType *result;
    IrType *context; // TODO: remove
    DeclId did;
    DeclId parent;
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

struct IrTraitDef {
    Str *name;
    struct IrGenericDefs *generics;
    struct IrTypeList *methods;
    DeclId did;
    paw_Bool is_pub : 1;
};

struct IrImpl {
    IrTrait *trait;
    IrType *type;
    struct IrGenericDefs *generics;
    struct IrTypeList *methods;
    DeclId did;
};

struct IrGenericDef *pawIr_new_generic_def(struct Compiler *C, DeclId did, Str *name, struct IrTraitList *bounds);
struct IrFieldDef *pawIr_new_field_def(struct Compiler *C, DeclId did, Str *name, paw_Bool is_pub);
struct IrVariantDef *pawIr_new_variant_def(struct Compiler *C, DeclId did, DeclId cons_did, DeclId base_did, int discr, Str *name, struct IrFieldDefs *fields);
struct IrFnDef *pawIr_new_fn_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, IrType *result, struct IrParams *params, IrType *context, DeclId parent, paw_Bool is_pub);
struct IrAdtDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, struct IrVariantDefs *variants, paw_Bool is_pub, paw_Bool is_struct, paw_Bool is_inline);
struct IrTraitDef *pawIr_new_trait_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, struct IrTypeList *methods, paw_Bool is_pub);
struct IrImpl *pawIr_new_impl(struct Compiler *C, DeclId did, IrType *type, IrTrait *trait, struct IrGenericDefs *generics, struct IrTypeList *methods);


#define IR_KINDOF(node) ((node)->hdr.kind)
#define IR_CAST_TYPE(p) CAST(IrType *, p)
#define IR_TYPE_DID(type) (IrIsAdt(type) ? IrGetAdt(type)->did : \
        IrIsSignature(type) ? IrGetSignature(type)->did : \
        IrGetGeneric(type)->did)
#define IR_TYPE_SUBTYPES(type) (IrIsAdt(type) ? IrGetAdt(type)->types : IrIsSignature(type) ? IrGetSignature(type)->types : NULL)
#define IR_TYPE_SUBTYPES_(type) (IrIsAdt(type) ? IrGetAdt(type)->types : IrIsSignature(type) ? IrGetSignature(type)->types : NULL)
#define IR_IS_FUNC_TYPE(p) (IrIsFnPtr(p) || IrIsSignature(p))
#define IR_FPTR(p) CHECK_EXP(IR_IS_FUNC_TYPE(p), &(p)->FnPtr_)

DEFINE_LIST(struct Compiler, IrTypeList, IrType *)
DEFINE_LIST(struct Compiler, IrTraitList, IrTrait *)
DEFINE_LIST(struct Compiler, IrDefs, DeclId)
DEFINE_LIST(struct Compiler, IrVariantDefs, struct IrVariantDef *)
DEFINE_LIST(struct Compiler, IrGenericDefs, struct IrGenericDef *)
DEFINE_LIST(struct Compiler, IrFieldDefs, struct IrFieldDef *)
DEFINE_LIST(struct Compiler, IrParams, struct IrParam)

IrType *pawIr_resolve_trait_method(struct Compiler *C, struct IrGeneric *target, Str *name);

EXTERN_C IrType *pawIr_get_type(struct Compiler *C, NodeId id);
void pawIr_set_type(struct Compiler *C, NodeId id, IrType *type);
EXTERN_C struct IrVariantDef *pawIr_get_variant_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrGenericDef *pawIr_get_generic_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrTraitDef *pawIr_get_trait_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrAdtDef *pawIr_get_adt_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrFnDef *pawIr_get_fn_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrImpl *pawIr_get_impl_def(struct Compiler *C, DeclId did);
EXTERN_C IrType *pawIr_get_def_type(struct Compiler *C, DeclId did);
EXTERN_C IrTrait *pawIr_get_trait(struct Compiler *C, DeclId did);

enum IrDefKind pawIr_get_kind(struct Compiler *C, DeclId did);
EXTERN_C IrType *pawIr_get_context(struct Compiler *C, IrType *fn);
IrTrait *pawIr_get_trait_context(struct Compiler *C, IrType *fn);
IrTypeList *pawIr_get_generic_types(struct Compiler *C, DeclId did);
IrTraitList *pawIr_get_trait_bounds(struct Compiler *C, DeclId did);

void pawIr_set_generic_types(struct Compiler *C, DeclId did, IrTypeList *types);
void pawIr_set_trait_bounds(struct Compiler *C, DeclId did, IrTraitList *traits);

EXTERN_C IrType *pawIr_materialize_fn(struct Compiler *C, DeclId did, IrTypeList *type_args);
#define IR_SIGNATURE_FN(C_, Type_) pawIr_materialize_fn(C_, IR_TYPE_DID(Type_), IR_TYPE_SUBTYPES(Type_))
#define IR_GET_FN(C_, Type_) (IrIsFnPtr(Type_) ? (Type_) : \
        pawIr_materialize_fn(C_, IR_TYPE_DID(Type_), IR_TYPE_SUBTYPES(Type_)))

EXTERN_C paw_Uint pawIr_type_hash(struct Compiler *C, IrType *t);
EXTERN_C paw_Bool pawIr_type_equals(struct Compiler *C, IrType *a, IrType *b);
#define IR_TYPE_HASH(Ctx_, Type_) pawIr_type_hash((Ctx_)->C, Type_)
#define IR_TYPE_EQUALS(Ctx_, A_, B_) pawIr_type_equals((Ctx_)->C, A_, B_)

EXTERN_C paw_Uint pawIr_trait_hash(struct Compiler *C, IrTrait *t);
EXTERN_C paw_Bool pawIr_trait_equals(struct Compiler *C, IrTrait *a, IrTrait *b);
#define IR_TRAIT_HASH(Ctx_, Trait_) pawIr_trait_hash((Ctx_)->C, Trait_)
#define IR_TRAIT_EQUALS(Ctx_, A_, B_) pawIr_trait_equals((Ctx_)->C, A_, B_)


DEFINE_MAP(struct Compiler, IrGenericTypes, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, IrTypeList *)
DEFINE_MAP(struct Compiler, IrTraitBounds, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, IrTraitList *)
DEFINE_MAP(struct Compiler, IrDefKinds, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, enum IrDefKind)

DEFINE_MAP(struct Compiler, TypeCollection, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, void *)
DEFINE_MAP_ITERATOR(TypeCollection, IrType *, void *)


static IrType *ir_fn_result(struct Compiler *C, IrType *type)
{
    return IrGetFnPtr(IR_GET_FN(C, type))->result;
}

static IrTypeList *ir_fn_params(struct Compiler *C, IrType *type)
{
    return IrGetFnPtr(IR_GET_FN(C, type))->params;
}

static IrTypeList *ir_signature_types_(IrType *type)
{
    return IrGetSignature(type)->types;
}

static IrTypeList *ir_adt_types(IrType *type)
{
    return IrGetAdt(type)->types;
}

static IrType *ir_adt_subtype(IrType *adt, int index)
{
    return IrTypeList_get(IrGetAdt(adt)->types, index);
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

static inline IrType *ir_auto_deref(IrType *type)
{
    if (IrIsPtr(type))
        type = IrGetPtr(type)->pointee;
    // only 1 level of indirection allowed
    paw_assert(!IrIsPtr(type));
    return type;
}

static inline IrType *ir_remove_indirection(IrType *type)
{
    while (IrIsPtr(type))
        type = IrGetPtr(type)->pointee;
    return type;
}

static inline paw_Bool ir_is_reference_type(struct Compiler *C, IrType *type)
{
    enum BuiltinKind const kind = pawP_type2code(C, type);
    switch (kind) {
        case BUILTIN_UNIT:
        case BUILTIN_BOOL:
        case BUILTIN_CHAR:
        case BUILTIN_INT:
        case BUILTIN_FLOAT:
            return PAW_FALSE;
        case BUILTIN_STR:
        case BUILTIN_LIST:
        case BUILTIN_MAP:
            return PAW_TRUE;
        default:
            break;
    }
    if (IrIsAdt(type)) {
        struct IrAdtDef const *def = pawIr_get_adt_def(C, IR_TYPE_DID(type));
        return !def->is_inline;
    }
    return PAW_FALSE;
}

static inline paw_Bool ir_is_value_type(struct Compiler *C, IrType *type)
{
    return !IrIsPtr(type) && !ir_is_reference_type(C, type);
}

EXTERN_C char const *pawIr_print_type(struct Compiler *C, IrType *type);
EXTERN_C char const *pawIr_print_trait(struct Compiler *C, IrTrait *trait);

DEFINE_LIST(struct Compiler, TraitOwnerList, struct IrTypeList *)
DEFINE_MAP(struct Compiler, TraitOwners, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, TraitOwnerList *)

#endif // PAW_IR_TYPE_H
