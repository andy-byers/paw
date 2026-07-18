// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_IR_TYPE_H
#define PAW_IR_TYPE_H

#include "hir.h"

typedef struct IrType IrType;
typedef struct IrTrait IrTrait;
typedef struct IrConst IrConst;
typedef struct IrGenericArg IrGenericArg;

// NOTE: kIrGeneric must be the last type due to "IR_NUM_TYPE_KINDS"
//   macro below.

#define IR_TYPE_LIST(X) \
    X(Unit) \
    X(Bool) \
    X(Char) \
    X(Int) \
    X(Float) \
    X(String) \
    X(Ptr) \
    X(Adt) \
    X(FnPtr) \
    X(Closure) \
    X(Signature) \
    X(Array) \
    X(Slice) \
    X(Tuple) \
    X(Never) \
    X(Infer) \
    X(Projection) \
    X(Generic)

enum IrTypeKind {
#define DEFINE_ENUM(X) kIr##X,
    IR_TYPE_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define IR_NUM_TYPE_KINDS (kIrGeneric + 1)

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

enum IrIntKind {
    IR_INT8,
    IR_INT16,
    IR_INT32,
    IR_INT64,
    IR_ISIZE,
    IR_UINT8,
    IR_UINT16,
    IR_UINT32,
    IR_UINT64,
    IR_USIZE,
};

struct IrInt {
    IR_TYPE_HEADER;
    enum IrIntKind ikind;
};

#define IR_INT8_MIN (~IR_INT8_MAX)
#define IR_INT8_MAX 0x7F
#define IR_UINT8_MAX 0xFF

#define IR_INT16_MIN (~IR_INT16_MAX)
#define IR_INT16_MAX 0x7FFF
#define IR_UINT16_MAX 0xFFFF

#define IR_INT32_MIN (~IR_INT32_MAX)
#define IR_INT32_MAX 0x7FFFFFFF
#define IR_UINT32_MAX 0xFFFFFFFF

#define IR_INT64_MIN (~IR_INT64_MAX)
#define IR_INT64_MAX 0x7FFFFFFFFFFFFFFF
#define IR_UINT64_MAX 0xFFFFFFFFFFFFFFFF

#define IR_INT_KIND(Type_) (IrGetInt(Type_)->ikind)
#define IR_NUM_INT_KINDS ((size_t)IR_USIZE + 1)

enum IrFloatKind {
    IR_FLOAT32,
    IR_FLOAT64,
};

struct IrFloat {
    IR_TYPE_HEADER;
    enum IrFloatKind fkind;
};

#define IR_FLOAT_KIND(Type_) (IrGetFloat(Type_)->fkind)
#define IR_NUM_FLOAT_KINDS ((size_t)IR_FLOAT64 + 1)

struct IrString {
    IR_TYPE_HEADER;
};

struct IrPtr {
    IR_TYPE_HEADER;
    struct IrType *pointee;
};

struct IrAdt {
    IR_TYPE_HEADER;
    DeclId did;
    struct IrGenericArgs *args;
};

struct IrFnPtr {
    IR_TYPE_HEADER;
    struct IrTypeList *params;
    IrType *result;
};

struct IrClosure {
    IR_TYPE_HEADER;
    DeclId did;
    struct IrGenericArgs *args;
};

struct IrSignature {
    IR_TYPE_HEADER;
    DeclId did;
    struct IrGenericArgs *args;
};

struct IrArray {
    IR_TYPE_HEADER;
    IrConst *length;
    IrType *type;
};

struct IrSlice {
    IR_TYPE_HEADER;
    IrType *type;
};

struct IrTuple {
    IR_TYPE_HEADER;
    struct IrTypeList *elems;
};

struct IrNever {
    IR_TYPE_HEADER;
};

enum IrInferKind {
    IR_INFER_TYPE,
    IR_INFER_INTEGER,
    IR_INFER_FLOAT,
};

// TODO: we don't allow creating nested inference contexts so get rid of depth field
struct IrInfer {
    IR_TYPE_HEADER;
    enum IrInferKind ikind;
    int depth;
    int index;
};

// TODO: use this to represent only type schemes, i.e. the T in fn f<T>(). create a new type to represent instantiated generics, i.e. the T in "f::<T>()"
struct IrGeneric {
    IR_TYPE_HEADER;
    DeclId did;
};

// Represents an associated type
//
// The projection `<Type as Trait<T>>::Assoc` is represented by an IrProjection with the
// following fields:
//     {
//         .did = Trait::Assoc,
//         .args = [Type, T],
//     }
//
struct IrProjection {
    IR_TYPE_HEADER;

    // identifies the associated type declaration in the trait
    DeclId did;

    // generic args of the trait, including `Self`
    struct IrGenericArgs *args;
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

#define DEFINE_ACCESS(X) \
    static paw_Bool IrIs##X(const IrType *node) \
    { \
        return node->hdr.kind == kIr##X; \
    } \
    static struct Ir##X *IrGet##X(IrType *node) \
    { \
        paw_assert(IrIs##X(node)); \
        return &node->X##_; \
    } \
    static struct Ir##X const *IrGet##X##K(IrType const *node) \
    { \
        paw_assert(IrIs##X(node)); \
        return &node->X##_; \
    }
IR_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

IrType *pawIr_new_unit(struct Compiler *C);
IrType *pawIr_new_bool(struct Compiler *C);
IrType *pawIr_new_char(struct Compiler *C);
IrType *pawIr_new_int(struct Compiler *C, enum IrIntKind ikind);
IrType *pawIr_new_float(struct Compiler *C, enum IrFloatKind fkind);
IrType *pawIr_new_string(struct Compiler *C);
IrType *pawIr_new_ptr(struct Compiler *C, IrType *pointee);
IrType *pawIr_new_adt(struct Compiler *C, DeclId did, struct IrGenericArgs *args);
IrType *pawIr_new_fn_ptr(struct Compiler *C, struct IrTypeList *params, IrType *result);
IrType *pawIr_new_closure(struct Compiler *C, DeclId did, struct IrGenericArgs *args);
IrType *pawIr_new_signature(struct Compiler *C, DeclId did, struct IrGenericArgs *args);
IrType *pawIr_new_slice(struct Compiler *C, IrType *type);
IrType *pawIr_new_tuple(struct Compiler *C, struct IrTypeList *elems);
IrType *pawIr_new_array(struct Compiler *C, IrType *type, IrConst *length);
IrType *pawIr_new_never(struct Compiler *C);
IrType *pawIr_new_infer(struct Compiler *C, enum IrInferKind ikind, int depth, int index);
IrType *pawIr_new_generic(struct Compiler *C, DeclId did);
IrType *pawIr_new_projection(struct Compiler *C, DeclId did, struct IrGenericArgs *args);


struct IrTrait {
    DeclId did;
    struct IrGenericArgs *args;
};

IrTrait *pawIr_new_trait(struct Compiler *C, DeclId did, struct IrGenericArgs *args);


union IrValue {
    paw_Uint8 b;
    paw_Char c;
    paw_Int8 i8;
    paw_Int16 i16;
    paw_Int32 i32;
    paw_Int64 i64;
    paw_Isize isize;
    paw_Uint8 u8;
    paw_Uint16 u16;
    paw_Uint32 u32;
    paw_Uint64 u64;
    paw_Usize usize;
    paw_Float32 f32;
    paw_Float64 f64;
    Str const *s;
    void *p;
    // TODO: remove these and use fixed-width types
    paw_Int i;
    paw_Uint u;
    paw_Float f;
};

enum IrConstKind {
    IR_CONST_VALUE,
    IR_CONST_PENDING,
    IR_CONST_DECL,
    IR_CONST_INFER,
};

struct IrConstValue {
    union IrValue value;
    IrType *type;
};

struct IrConstPending {
    DeclId did;
};

struct IrConstParam {
    DeclId did;
};

struct IrConstDecl {
    DeclId did;
};

struct IrConstInfer {
    int depth;
    int index;
};

struct IrConst {
    enum IrConstKind kind;
    union {
        struct IrConstValue value;
        struct IrConstPending pending;
        struct IrConstParam param;
        struct IrConstDecl decl;
        struct IrConstInfer infer;
    };
};

IrConst *pawIr_new_const_value(struct Compiler *C, union IrValue value, IrType *type);
IrConst *pawIr_new_const_pending(struct Compiler *C, DeclId did);
IrConst *pawIr_new_const_decl(struct Compiler *C, DeclId did);
IrConst *pawIr_new_const_infer(struct Compiler *C, int depth, int index);


enum IrGenericArgKind {
    IR_GENERIC_ARG_TYPE,
    IR_GENERIC_ARG_CONST,
};

struct IrGenericArg {
    void *inner;
};

EXTERN_C IrGenericArg IrGenericArg_from_type(IrType *t);
EXTERN_C IrGenericArg IrGenericArg_from_const(IrConst *k);

EXTERN_C paw_Bool IrGenericArg_is_type(IrGenericArg ga);

static paw_Bool IrGenericArg_is_const(IrGenericArg ga)
{
    return !IrGenericArg_is_type(ga);
}

EXTERN_C IrType *IrGenericArg_get_type(IrGenericArg ga);
EXTERN_C IrConst *IrGenericArg_get_const(IrGenericArg ga);


enum IrDefKind {
    IR_FN_DEF,
    IR_ADT_DEF,
    IR_VARIANT_DEF,
    IR_IMPL_DEF,
    IR_TRAIT_DEF,
    IR_ALIAS_DEF,
    IR_GENERIC_DEF,
    IR_FIELD_DEF,
};

void pawIr_set_def_kind(struct Compiler *C, DeclId did, enum IrDefKind kind);


struct IrParam {
    Str *name;
    IrType *type;
};

enum IrConstraintKind {
    IR_CONSTRAINT_TYPE_EQUALS,
    IR_CONSTRAINT_IMPL_TRAIT,
};

struct IrConstraint {
    enum IrConstraintKind kind;
    struct SourceSpan span;
    DeclId parent;
    union {
        struct {
            IrType *type;
            IrTrait *trait;
        } impl;

        struct {
            IrType *lhs;
            IrType *rhs;
        } eq;
    };
};

struct IrFieldDef {
    Str *name;
    DeclId did;
    paw_Bool is_pub : 1;
};

struct IrGenericDef {
    DeclId did;
    paw_Bool is_type : 1;
    union {
        struct {
            struct IrTraitList *bounds;
            Str *name;
        } type;

        struct {
            Str *name;
            IrType *type;
        } konst;
    };
};

struct IrVariantDef {
    Str const *name;
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
    paw_Bool has_captures : 1;
};

struct IrAdtDef {
    Str *name;
    struct IrGenericDefs *generics;
    struct IrVariantDefs *variants;
    DeclId did;
    paw_Bool is_struct : 1;
    paw_Bool is_pub : 1;
};

struct IrAssocItem {
    Str const *name;
    DeclId did;
    DeclId parent;
    paw_Bool is_pub : 1;
};

struct IrTraitDef {
    Str *name;
    struct IrGenericDefs *generics;
    struct IrTypeList *methods;
    struct IrAssocItems *items;
    DeclId did;
    paw_Bool is_pub : 1;
};

struct IrImpl {
    IrTrait *trait;
    IrType *type;
    struct IrGenericDefs *generics;
    struct IrTypeList *methods;
    struct IrAssocItems *items;
    DeclId did;
};

struct IrGenericDef *pawIr_new_generic_type_def(struct Compiler *C, DeclId did, Str *name, struct IrTraitList *bounds);
struct IrGenericDef *pawIr_new_generic_const_def(struct Compiler *C, DeclId did, IrType *type, Str *name);
struct IrFieldDef *pawIr_new_field_def(struct Compiler *C, DeclId did, Str *name, paw_Bool is_pub);
struct IrVariantDef *pawIr_new_variant_def(struct Compiler *C, DeclId did, DeclId cons_did, DeclId base_did, int discr, Str const *name, struct IrFieldDefs *fields);
struct IrFnDef *pawIr_new_fn_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, IrType *result, struct IrParams *params, IrType *context, DeclId parent, paw_Bool is_pub);
struct IrAdtDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, struct IrVariantDefs *variants, paw_Bool is_pub, paw_Bool is_struct);
struct IrAssocItem *pawIr_new_assoc_item(struct Compiler *C, DeclId did, Str const *name, DeclId parent, paw_Bool is_pub);
struct IrTraitDef *pawIr_new_trait_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, struct IrTypeList *methods, struct IrAssocItems *items, paw_Bool is_pub);
struct IrImpl *pawIr_new_impl(struct Compiler *C, DeclId did, IrType *type, IrTrait *trait, struct IrGenericDefs *generics, struct IrTypeList *methods, struct IrAssocItems *items);

struct IrGenericArgs *pawIr_instantiate_args(struct Compiler *C, DeclId did);
struct IrGenericArg pawIr_instantiate(struct Compiler *C, DeclId did);


#define IR_KINDOF(node) ((node)->hdr.kind)
#define IR_CAST_TYPE(p) CAST(IrType *, p)
#define IR_TYPE_DID(type) (IrIsAdt(type) ? IrGetAdt(type)->did : \
        IrIsSignature(type) ? IrGetSignature(type)->did : \
        IrIsClosure(type) ? IrGetClosure(type)->did : \
        IrIsProjection(type) ? IrGetProjection(type)->did : \
        IrGetGeneric(type)->did)
#define IR_TYPE_IS_POLYMORPHIC(Type_) \
    ((IrIsAdt(Type_) || IrIsSignature(Type_) || IrIsClosure(Type_)) \
        && IR_GENERIC_ARGS(Type_)->count > 0)
#define IR_FIRST_GENERIC_ARG(Type_) IrGenericArgs_first(IR_GENERIC_ARGS(Type_))
#define IR_GENERIC_ARGS(Type_) (IrIsAdt(Type_) ? IrGetAdt(Type_)->args : \
        IrIsSignature(Type_) ? IrGetSignature(Type_)->args : \
        IrIsClosure(Type_) ? IrGetClosure(Type_)->args : NULL)
#define IR_IS_FUNC_TYPE(p) (IrIsFnPtr(p) || IrIsSignature(p) || IrIsClosure(p))

DEFINE_LIST(struct Compiler, IrTypeList, IrType *,)
DEFINE_LIST(struct Compiler, IrTraitList, IrTrait *,)
DEFINE_LIST(struct Compiler, IrGenericArgs, IrGenericArg,)
DEFINE_LIST(struct Compiler, IrDefs, DeclId,)
DEFINE_LIST(struct Compiler, IrAssocItems, struct IrAssocItem *,)
DEFINE_LIST(struct Compiler, IrVariantDefs, struct IrVariantDef *,)
DEFINE_LIST(struct Compiler, IrGenericDefs, struct IrGenericDef *,)
DEFINE_LIST(struct Compiler, IrConstraints, struct IrConstraint,)
DEFINE_LIST(struct Compiler, IrFieldDefs, struct IrFieldDef *,)
DEFINE_LIST(struct Compiler, IrParams, struct IrParam,)

IrType *pawIr_resolve_trait_method(struct Compiler *C, struct IrGeneric *target, Str *name);

EXTERN_C IrType *pawIr_get_type(struct Compiler *C, NodeId id);
void pawIr_set_type(struct Compiler *C, NodeId id, IrType *type);
EXTERN_C struct IrVariantDef *pawIr_get_variant_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrGenericDef *pawIr_get_generic_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrGenericDefs *pawIr_get_generic_binder(struct Compiler *C, DeclId did);
EXTERN_C struct IrTraitDef *pawIr_get_trait_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrAdtDef *pawIr_get_adt_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrFnDef *pawIr_get_fn_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrAssocItem *pawIr_get_assoc_item(struct Compiler *C, DeclId did);
EXTERN_C struct IrImpl *pawIr_get_impl_def(struct Compiler *C, DeclId did);
EXTERN_C IrType *pawIr_get_def_type(struct Compiler *C, DeclId did);
EXTERN_C IrGenericArg pawIr_get_def_arg(struct Compiler *C, DeclId did);
EXTERN_C IrTrait *pawIr_get_trait(struct Compiler *C, DeclId did);
EXTERN_C IrGenericArg *pawIr_get_generic_arg(struct Compiler *C, DeclId did);

EXTERN_C enum IrDefKind pawIr_get_kind(struct Compiler *C, DeclId did);
EXTERN_C IrType *pawIr_get_context(struct Compiler *C, IrType *fn);
EXTERN_C IrTrait *pawIr_get_trait_context(struct Compiler *C, IrType *fn);
EXTERN_C IrGenericArgs *pawIr_get_generic_args(struct Compiler *C, DeclId did);
EXTERN_C IrConstraints *pawIr_get_constraints(struct Compiler *C, DeclId did);
EXTERN_C IrTraitList *pawIr_get_trait_bounds(struct Compiler *C, DeclId did);
EXTERN_C paw_Bool pawIr_is_copyable(struct Compiler *C, IrType *type);

EXTERN_C paw_Bool pawIr_needs_drop(struct Compiler *C, IrType *type);
EXTERN_C IrType *pawIr_get_custom_drop_type(struct Compiler *C, IrType *type);
EXTERN_C IrType *pawIr_materialize_drop_type(struct Compiler *C, IrType *type);
EXTERN_C IrType *pawIr_get_drop_type(struct Compiler *C, IrType *type);


void pawIr_set_generic_args(struct Compiler *C, DeclId did, IrGenericArgs *args);
void pawIr_set_trait_bounds(struct Compiler *C, DeclId did, IrTraitList *traits);

EXTERN_C IrType *pawIr_materialize_fn(struct Compiler *C, DeclId did, IrGenericArgs *type_args);
#define IR_SIGNATURE_FN(C_, Type_) pawIr_materialize_fn(C_, IR_TYPE_DID(Type_), IR_GENERIC_ARGS(Type_))
#define IR_GET_FN(C_, Type_) (IrIsFnPtr(Type_) ? (Type_) : \
        pawIr_materialize_fn(C_, IR_TYPE_DID(Type_), IR_GENERIC_ARGS(Type_)))

EXTERN_C paw_Uint pawIr_type_hash(struct Compiler *C, IrType *t);
EXTERN_C paw_Bool pawIr_type_equals(struct Compiler *C, IrType *a, IrType *b);
#define IR_TYPE_HASH(Ctx_, Type_) pawIr_type_hash((Ctx_)->C, Type_)
#define IR_TYPE_EQUALS(Ctx_, A_, B_) pawIr_type_equals((Ctx_)->C, A_, B_)

EXTERN_C paw_Uint pawIr_trait_hash(struct Compiler *C, IrTrait *t);
EXTERN_C paw_Bool pawIr_trait_equals(struct Compiler *C, IrTrait *a, IrTrait *b);
#define IR_TRAIT_HASH(Ctx_, Trait_) pawIr_trait_hash((Ctx_)->C, Trait_)
#define IR_TRAIT_EQUALS(Ctx_, A_, B_) pawIr_trait_equals((Ctx_)->C, A_, B_)

EXTERN_C paw_Uint pawIr_const_hash(struct Compiler *C, IrConst const *k);
EXTERN_C paw_Bool pawIr_const_equals(struct Compiler *C, IrConst const *a, IrConst const *b);
#define IR_CONST_HASH(Ctx_, Const_) pawIr_const_hash((Ctx_)->C, Const_)
#define IR_CONST_EQUALS(Ctx_, A_, B_) pawIr_const_equals((Ctx_)->C, A_, B_)

EXTERN_C paw_Uint pawIr_arg_hash(struct Compiler *C, IrGenericArg g);
EXTERN_C paw_Bool pawIr_arg_equals(struct Compiler *C, IrGenericArg a, IrGenericArg b);
#define IR_ARG_HASH(Ctx_, Arg_) pawIr_trait_hash((Ctx_)->C, Arg_)
#define IR_ARG_EQUALS(Ctx_, A_, B_) pawIr_trait_equals((Ctx_)->C, A_, B_)


struct IrType2 {
    IrType *first;
    IrType *second;
};

EXTERN_C paw_Uint pawIr_type2_hash(struct Compiler *C, struct IrType2 t);
EXTERN_C paw_Bool pawIr_type2_equals(struct Compiler *C, struct IrType2 a, struct IrType2 b);
#define IR_TYPE2_HASH(Ctx_, Type_) pawIr_type2_hash((Ctx_)->C, Type_)
#define IR_TYPE2_EQUALS(Ctx_, A_, B_) pawIr_type2_equals((Ctx_)->C, A_, B_)

int pawIr_unify(struct Compiler *C, IrGenericArg a, IrGenericArg b);
IrGenericArg pawIr_normalize(struct Compiler *C, IrGenericArg g);
IrGenericArg pawIr_normalize_projections(struct Compiler *C, IrGenericArg g);

static void pawIr_unify_unchecked(struct Compiler *C, IrGenericArg a, IrGenericArg b)
{
    int const rc = pawIr_unify(C, a, b);
    paw_assert(rc == 0); PAW_UNUSED(rc);
}

static paw_Bool ir_is_capturing_closure(struct Compiler *C, IrType *type)
{
    return IrIsClosure(type)
        && pawIr_get_fn_def(C, IR_TYPE_DID(type))->has_captures;
}

struct IrPendingConstant {
    IrConst *konst;
    void *payload;
};

DEFINE_MAP(struct Compiler, IrConstraintsMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, IrConstraints *,)
DEFINE_MAP(struct Compiler, IrGenericTypes, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, IrGenericArgs *,)
DEFINE_MAP(struct Compiler, IrAssocItemMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrAssocItem *,)
DEFINE_MAP(struct Compiler, IrDeclArgs, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, IrGenericArg,)
DEFINE_MAP(struct Compiler, IrTraitBounds, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, IrTraitList *,)
DEFINE_MAP(struct Compiler, IrDefKinds, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, enum IrDefKind,)
DEFINE_MAP(struct Compiler, IrType2Map, pawP_alloc, pawIr_type2_hash, pawIr_type2_equals, struct IrType2, void *,)
DEFINE_MAP(struct Compiler, IrPendingConstants, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrPendingConstant,)
DEFINE_MAP(struct Compiler, IrResolvedConstants, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, IrConst *,)
DEFINE_MAP_ITERATOR(IrPendingConstants, DeclId, struct IrPendingConstant)

DEFINE_MAP(struct Compiler, IrTypeMap, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, void *,)
DEFINE_MAP_ITERATOR(IrTypeMap, IrType *, void *)
DEFINE_MAP(struct Compiler, TypeCollection, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, void *,)
DEFINE_MAP_ITERATOR(TypeCollection, IrType *, void *)
DEFINE_LIST(struct Compiler, IrType2List, struct IrType2,)

struct IrConstObligationCause {
    struct SourceSpan span;
};

struct IrConstObligation {
    struct IrConstObligationCause cause;
    IrConst *lhs;
    IrConst *rhs;
};

DEFINE_LIST(struct Compiler, IrConstObligations, struct IrConstObligation,)

paw_Bool pawIr_type_contains_inference_var(struct Compiler *C, IrType *type);
paw_Bool pawIr_trait_contains_inference_var(struct Compiler *C, IrTrait *trait);


EXTERN_C paw_Bool pawIr_is_unsized_type(struct Compiler *C, IrType *type);
EXTERN_C IrType *pawIr_remove_indirection(struct Compiler *C, IrType *type);
EXTERN_C IrTypeList *pawIr_autoptr_chain(struct Compiler *C, IrType *type);

static IrType *ir_projection_self(struct IrProjection const *t)
{
    return IrGenericArg_get_type(IrGenericArgs_first(t->args));
}

IrTrait *pawIr_get_projection_trait(struct Compiler *C, struct IrProjection const *t);


static IrType *ir_fn_result(struct Compiler *C, IrType *type)
{
    return IrGetFnPtr(IR_GET_FN(C, type))->result;
}

static IrTypeList *ir_fn_params(struct Compiler *C, IrType *type)
{
    return IrGetFnPtr(IR_GET_FN(C, type))->params;
}

static IrGenericArgs *ir_signature_args(IrType *type)
{
    return IrGetSignature(type)->args;
}

static IrGenericArgs *ir_adt_args(IrType *type)
{
    return IrGetAdt(type)->args;
}

static IrGenericArg ir_adt_arg(IrType *adt, int index)
{
    return IrGenericArgs_get(IrGetAdt(adt)->args, index);
}

static IrType *ir_deref(IrType *type)
{
    return IrGetPtr(type)->pointee;
}

static IrType *ir_auto_deref(IrType *type)
{
    return IrIsPtr(type) ? ir_deref(type) : type;
}

IrDefs *pawIr_inherent_impls_for(struct Compiler *C, IrType *self);
IrDefs *pawIr_trait_impls_for(struct Compiler *C, IrType *self);


EXTERN_C char const *pawIr_print_type(struct Compiler *C, IrType *type);
EXTERN_C char const *pawIr_print_trait(struct Compiler *C, IrTrait *trait);
EXTERN_C char const *pawIr_print_impl_trait_obligation(struct Compiler *C, IrType *type, IrTrait *trait);

// TODO: use these and get rid of the old ones
EXTERN_C Str const *pawIr_print_const(struct Compiler *C, IrConst *konst);
EXTERN_C Str const *pawIr_print_type_v2(struct Compiler *C, IrType *type);
EXTERN_C Str const *pawIr_print_trait_v2(struct Compiler *C, IrTrait *trait);
EXTERN_C Str const *pawIr_print_impl_trait_obligation_v2(struct Compiler *C, IrType *type, IrTrait *trait);

DEFINE_LIST(struct Compiler, TraitOwnerList, struct IrTypeList *,)
DEFINE_MAP(struct Compiler, TraitOwners, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, TraitOwnerList *,)

#endif // PAW_IR_TYPE_H
