// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_IR_TYPE_H
#define PAW_IR_TYPE_H

#include "value.h"

#include "compiler/code.h"
#include "compiler/compiler.h"

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
    IR_USIZE // must be last
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
    IR_FLOAT64 // must be last
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

DEFINE_LIST(IrTypeList, IrType *)

IrType *ir_new_unit(struct Compiler *C);
IrType *ir_new_bool(struct Compiler *C);
IrType *ir_new_char(struct Compiler *C);
IrType *ir_new_int(struct Compiler *C, enum IrIntKind ikind);
IrType *ir_new_float(struct Compiler *C, enum IrFloatKind fkind);
IrType *ir_new_string(struct Compiler *C);
IrType *ir_new_ptr(struct Compiler *C, IrType *pointee);
IrType *ir_new_adt(struct Compiler *C, DeclId did, struct IrGenericArgs *args);
IrType *ir_new_fn_ptr(struct Compiler *C, struct IrTypeList *params, IrType *result);
IrType *ir_new_closure(struct Compiler *C, DeclId did, struct IrGenericArgs *args);
IrType *ir_new_signature(struct Compiler *C, DeclId did, struct IrGenericArgs *args);
IrType *ir_new_slice(struct Compiler *C, IrType *type);
IrType *ir_new_tuple(struct Compiler *C, struct IrTypeList *elems);
IrType *ir_new_array(struct Compiler *C, IrType *type, IrConst *length);
IrType *ir_new_never(struct Compiler *C);
IrType *ir_new_infer(struct Compiler *C, enum IrInferKind ikind, int depth, int index);
IrType *ir_new_generic(struct Compiler *C, DeclId did);
IrType *ir_new_projection(struct Compiler *C, DeclId did, struct IrGenericArgs *args);


struct IrTrait {
    DeclId did;
    struct IrGenericArgs *args;
};

IrTrait *ir_new_trait(struct Compiler *C, DeclId did, struct IrGenericArgs *args);


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

IrConst *ir_new_const_value(struct Compiler *C, union IrValue value, IrType *type);
IrConst *ir_new_const_pending(struct Compiler *C, DeclId did);
IrConst *ir_new_const_decl(struct Compiler *C, DeclId did);
IrConst *ir_new_const_infer(struct Compiler *C, int depth, int index);


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

void ir_set_def_kind(struct Compiler *C, DeclId did, enum IrDefKind kind);


struct IrParam {
    Str const *name;
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
    Str const *name;
    DeclId did;
    paw_Bool is_pub : 1;
};

struct IrGenericDef {
    DeclId did;
    struct SourceSpan span;
    paw_Bool is_type : 1;
    union {
        struct {
            struct IrTraitList *bounds;
            Str const *name;
        } type;

        struct {
            Str const *name;
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
    Str const *name;
    struct Annotations *annos;
    struct IrGenericDefs *generics;
    struct IrParams *params;
    IrType *result;
    DeclId did;
    DeclId parent;
    paw_Bool is_pub : 1;
    paw_Bool is_extern : 1;
    paw_Bool has_captures : 1;
};

struct IrAdtDef {
    Str const *name;
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
    Str const *name;
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

struct IrGenericDef *ir_new_generic_type_def(struct Compiler *C, DeclId did, Str const *name, struct SourceSpan span, struct IrTraitList *bounds);
struct IrGenericDef *ir_new_generic_const_def(struct Compiler *C, DeclId did, IrType *type, Str const *name, struct SourceSpan span);
struct IrFieldDef *ir_new_field_def(struct Compiler *C, DeclId did, Str const *name, paw_Bool is_pub);
struct IrVariantDef *ir_new_variant_def(struct Compiler *C, DeclId did, DeclId cons_did, DeclId base_did, int discr, Str const *name, struct IrFieldDefs *fields);
struct IrFnDef *ir_new_fn_def(struct Compiler *C, DeclId did, Str const *name, struct IrGenericDefs *generics, IrType *result, struct IrParams *params, DeclId parent, paw_Bool is_pub);
struct IrAdtDef *ir_new_adt_def(struct Compiler *C, DeclId did, Str const *name, struct IrGenericDefs *generics, struct IrVariantDefs *variants, paw_Bool is_pub, paw_Bool is_struct);
struct IrAssocItem *ir_new_assoc_item(struct Compiler *C, DeclId did, Str const *name, DeclId parent, paw_Bool is_pub);
struct IrTraitDef *ir_new_trait_def(struct Compiler *C, DeclId did, Str const *name, struct IrGenericDefs *generics, struct IrTypeList *methods, struct IrAssocItems *items, paw_Bool is_pub);
struct IrImpl *ir_new_impl(struct Compiler *C, DeclId did, IrType *type, IrTrait *trait, struct IrGenericDefs *generics, struct IrTypeList *methods, struct IrAssocItems *items);

struct IrGenericArgs *ir_instantiate_args(struct Compiler *C, DeclId did, struct SourceSpan span);
struct IrGenericArg ir_instantiate(struct Compiler *C, DeclId did, struct SourceSpan span);


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

DEFINE_LIST(IrTraitList, IrTrait *)
DEFINE_LIST(IrGenericArgs, IrGenericArg)
DEFINE_LIST(IrDefs, DeclId)
DEFINE_LIST(IrAssocItems, struct IrAssocItem *)
DEFINE_LIST(IrVariantDefs, struct IrVariantDef *)
DEFINE_LIST(IrGenericDefs, struct IrGenericDef *)
DEFINE_LIST(IrConstraints, struct IrConstraint)
DEFINE_LIST(IrFieldDefs, struct IrFieldDef *)
DEFINE_LIST(IrParams, struct IrParam)


EXTERN_C IrType *ir_get_type(struct Compiler *C, NodeId id);
void ir_set_type(struct Compiler *C, NodeId id, IrType *type);
EXTERN_C struct IrVariantDef *ir_get_variant_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrGenericDef *ir_get_generic_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrGenericDefs *ir_get_generic_binder(struct Compiler *C, DeclId did);
EXTERN_C struct IrTraitDef *ir_get_trait_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrAdtDef *ir_get_adt_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrFnDef *ir_get_fn_def(struct Compiler *C, DeclId did);
EXTERN_C struct IrAssocItem *ir_get_assoc_item(struct Compiler *C, DeclId did);
EXTERN_C struct IrImpl *ir_get_impl_def(struct Compiler *C, DeclId did);
EXTERN_C IrType *ir_get_def_type(struct Compiler *C, DeclId did);
EXTERN_C IrGenericArg ir_get_def_arg(struct Compiler *C, DeclId did);
EXTERN_C IrTrait *ir_get_trait(struct Compiler *C, DeclId did);
EXTERN_C IrGenericArg *ir_get_generic_arg(struct Compiler *C, DeclId did);

EXTERN_C enum IrDefKind ir_get_kind(struct Compiler *C, DeclId did);
EXTERN_C IrType *ir_get_context(struct Compiler *C, IrType *fn);
EXTERN_C IrTrait *ir_get_trait_context(struct Compiler *C, IrType *fn);
EXTERN_C IrGenericArgs *ir_get_generic_args(struct Compiler *C, DeclId did);
EXTERN_C IrConstraints *ir_get_constraints(struct Compiler *C, DeclId did);
EXTERN_C IrTraitList *ir_get_trait_bounds(struct Compiler *C, DeclId did);

EXTERN_C paw_Bool ir_needs_drop(struct Compiler *C, IrType *type);
EXTERN_C IrType *ir_get_custom_drop_type(struct Compiler *C, IrType *type);
EXTERN_C IrType *ir_materialize_drop_type(struct Compiler *C, IrType *type);
EXTERN_C IrType *ir_get_drop_type(struct Compiler *C, IrType *type);


void ir_set_generic_args(struct Compiler *C, DeclId did, IrGenericArgs *args);
void ir_set_trait_bounds(struct Compiler *C, DeclId did, IrTraitList *traits);

EXTERN_C IrType *ir_materialize_fn(struct Compiler *C, DeclId did, IrGenericArgs *type_args);
#define IR_SIGNATURE_FN(C_, Type_) ir_materialize_fn(C_, IR_TYPE_DID(Type_), IR_GENERIC_ARGS(Type_))
#define IR_GET_FN(C_, Type_) (IrIsFnPtr(Type_) ? (Type_) : \
        ir_materialize_fn(C_, IR_TYPE_DID(Type_), IR_GENERIC_ARGS(Type_)))

EXTERN_C paw_Uint ir_type_hash(IrType *t);
EXTERN_C paw_Bool ir_type_equals(IrType *a, IrType *b);
#define IR_TYPE_HASH(Type_) ir_type_hash(Type_)
#define IR_TYPE_EQUALS(A_, B_) ir_type_equals(A_, B_)

EXTERN_C paw_Uint ir_trait_hash(IrTrait *t);
EXTERN_C paw_Bool ir_trait_equals(IrTrait *a, IrTrait *b);
#define IR_TRAIT_HASH(Trait_) ir_trait_hash(Trait_)
#define IR_TRAIT_EQUALS(A_, B_) ir_trait_equals(A_, B_)

EXTERN_C paw_Uint ir_const_hash(IrConst const *k);
EXTERN_C paw_Bool ir_const_equals(IrConst const *a, IrConst const *b);
#define IR_CONST_HASH(Const_) ir_const_hash(Const_)
#define IR_CONST_EQUALS(A_, B_) ir_const_equals(A_, B_)

EXTERN_C paw_Uint ir_arg_hash(IrGenericArg g);
EXTERN_C paw_Bool ir_arg_equals(IrGenericArg a, IrGenericArg b);
#define IR_ARG_HASH(Arg_) ir_trait_hash(Arg_)
#define IR_ARG_EQUALS(A_, B_) ir_trait_equals(A_, B_)


struct IrType2 {
    IrType *first;
    IrType *second;
};

EXTERN_C paw_Uint ir_type2_hash(struct IrType2 t);
EXTERN_C paw_Bool ir_type2_equals(struct IrType2 a, struct IrType2 b);
#define IR_TYPE2_HASH(Type_) ir_type2_hash(Type_)
#define IR_TYPE2_EQUALS(A_, B_) ir_type2_equals(A_, B_)

EXTERN_C int ir_unify(struct Compiler *C, IrGenericArg a, IrGenericArg b);
EXTERN_C IrGenericArg ir_normalize(struct Compiler *C, IrGenericArg g);
EXTERN_C IrGenericArg ir_normalize_projections(struct Compiler *C, IrGenericArg g);

enum IrOrdering {
    IR_ORDERING_LESS,
    IR_ORDERING_EQUAL,
    IR_ORDERING_GREATER,
};

static IrType *ir_get_ordering_type(struct Compiler *C)
{
    return ir_get_def_type(C, C->std_ops_ordering);
}

static void ir_unify_unchecked(struct Compiler *C, IrGenericArg a, IrGenericArg b)
{
    int const rc = ir_unify(C, a, b);
    paw_assert(rc == 0); PAW_UNUSED(rc);
}

static paw_Bool ir_is_capturing_closure(struct Compiler *C, IrType *type)
{
    return IrIsClosure(type)
        && ir_get_fn_def(C, IR_TYPE_DID(type))->has_captures;
}

struct IrPendingConstant {
    IrConst *konst;
    void *payload;
};

DEFINE_MAP(IrConstraintsMap, P_ID_HASH, P_ID_EQUALS, DeclId, IrConstraints *)
DEFINE_MAP(IrGenericTypes, P_ID_HASH, P_ID_EQUALS, DeclId, IrGenericArgs *)
DEFINE_MAP(IrAssocItemMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrAssocItem *)
DEFINE_MAP(IrDeclArgs, P_ID_HASH, P_ID_EQUALS, DeclId, IrGenericArg)
DEFINE_MAP(IrTraitBounds, P_ID_HASH, P_ID_EQUALS, DeclId, IrTraitList *)
DEFINE_MAP(IrDefKinds, P_ID_HASH, P_ID_EQUALS, DeclId, enum IrDefKind)
DEFINE_MAP(IrType2Map, ir_type2_hash, ir_type2_equals, struct IrType2, void *)
DEFINE_MAP(IrPendingConstants, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrPendingConstant)
DEFINE_MAP(IrResolvedConstants, P_ID_HASH, P_ID_EQUALS, DeclId, IrConst *)
DEFINE_MAP_ITERATOR(IrPendingConstants, DeclId, struct IrPendingConstant)

DEFINE_MAP(IrTypeMap, ir_type_hash, ir_type_equals, IrType *, void *)
DEFINE_MAP_ITERATOR(IrTypeMap, IrType *, void *)
DEFINE_MAP(TypeCollection, ir_type_hash, ir_type_equals, IrType *, void *)
DEFINE_MAP_ITERATOR(TypeCollection, IrType *, void *)
DEFINE_LIST(IrType2List, struct IrType2)

struct IrConstObligationCause {
    struct SourceSpan span;
};

struct IrConstObligation {
    struct IrConstObligationCause cause;
    IrConst *lhs;
    IrConst *rhs;
};

DEFINE_LIST(IrConstObligations, struct IrConstObligation)

paw_Bool ir_type_contains_inference_var(struct Compiler *C, IrType *type);
paw_Bool ir_trait_contains_inference_var(struct Compiler *C, IrTrait *trait);

DeclId ir_get_method_from_parent(struct Compiler *C, DeclId parent_did, Str const *name);

EXTERN_C paw_Bool ir_is_unsized_type(struct Compiler *C, IrType *type);
EXTERN_C IrType *ir_remove_indirection(struct Compiler *C, IrType *type);
EXTERN_C IrTypeList *ir_autoptr_chain(struct Compiler *C, IrType *type);

static IrType *ir_projection_self(struct IrProjection const *t)
{
    return IrGenericArg_get_type(IrGenericArgs_first(t->args));
}

IrTrait *ir_get_projection_trait(struct Compiler *C, struct IrProjection const *t);


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

IrDefs *ir_inherent_impls_for(struct Compiler *C, IrType *self);
IrDefs *ir_trait_impls_for(struct Compiler *C, IrType *self);

IrTraitList *ir_supertraits_of(struct Compiler *C, DeclId did);


EXTERN_C char const *ir_print_type(struct Compiler *C, IrType *type);
EXTERN_C char const *ir_print_trait(struct Compiler *C, IrTrait *trait);
EXTERN_C char const *ir_print_impl_trait_obligation(struct Compiler *C, IrType *type, IrTrait *trait);

// TODO: use these and get rid of the old ones
EXTERN_C Str const *ir_print_const(struct Compiler *C, IrConst *konst);
EXTERN_C Str const *ir_print_type_v2(struct Compiler *C, IrType *type);
EXTERN_C Str const *ir_print_trait_v2(struct Compiler *C, IrTrait *trait);
EXTERN_C Str const *ir_print_impl_trait_obligation_v2(struct Compiler *C, IrType *type, IrTrait *trait);

DEFINE_LIST(TraitOwnerList, struct IrTypeList *)
DEFINE_MAP(TraitOwners, ir_type_hash, ir_type_equals, IrType *, TraitOwnerList *)

#endif // PAW_IR_TYPE_H
