// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_HIR_H
#define PAW_HIR_H

#include "compile.h"
#include "source.h"

#define HIR_DECL_LIST(X) \
    X(FieldDecl) \
    X(ParamDecl) \
    X(FnDecl) \
    X(GenericDecl) \
    X(ImplDecl) \
    X(AdtDecl) \
    X(TypeDecl) \
    X(ConstDecl) \
    X(TraitDecl) \
    X(VariantDecl)

#define HIR_EXPR_LIST(X) \
    X(AscriptionExpr) \
    X(LiteralExpr) \
    X(LogicalExpr) \
    X(PathExpr) \
    X(ChainExpr) \
    X(UnOpExpr) \
    X(BinOpExpr) \
    X(ClosureExpr) \
    X(ConversionExpr) \
    X(ProjectionExpr) \
    X(CallExpr) \
    X(Index) \
    X(Selector) \
    X(FieldExpr) \
    X(AssignExpr) \
    X(OpAssignExpr) \
    X(Block) \
    X(LoopExpr) \
    X(JumpExpr) \
    X(ReturnExpr) \
    X(MatchArm) \
    X(MatchExpr)

#define HIR_TYPE_LIST(X) \
    X(FnPtr) \
    X(SliceType) \
    X(ArrayType) \
    X(TupleType) \
    X(RefType) \
    X(PathType) \
    X(ProjectionType) \
    X(NeverType) \
    X(InferType)

#define HIR_STMT_LIST(X) \
    X(LetStmt) \
    X(ExprStmt) \
    X(DeclStmt)

#define HIR_PAT_LIST(X) \
    X(OrPat) \
    X(RefPat) \
    X(PtrPat) \
    X(FieldPat) \
    X(StructPat) \
    X(VariantPat) \
    X(TuplePat) \
    X(BindingPat) \
    X(WildcardPat) \
    X(LiteralPat)

#define HIR_CAST_DECL(x) CAST(struct HirDecl *, x)
#define HIR_CAST_EXPR(x) CAST(struct HirExpr *, x)
#define HIR_CAST_STMT(x) CAST(struct HirStmt *, x)
#define HIR_CAST_TYPE(x) CAST(struct HirType *, x)
#define HIR_CAST_PAT(x) CAST(struct HirPat *, x)

void pawHir_register_node(struct Hir *hir, NodeId id, void *node);
void pawHir_register_decl(struct Hir *hir, DeclId did, struct HirDecl *decl);

struct HirGenericArg {
    paw_Bool is_type;
    NodeId id;
    NodeId target;
    Str const *item;
    union {
        struct HirType *t;
        struct HirExpr *k;
    };
};

struct HirIdent {
    Str *name;
    struct SourceSpan span;
};

struct HirSegment {
    struct SourceSpan span;
    struct HirIdent ident;
    struct HirGenericArgs *args;
    NodeId id, target;
};

enum HirPathKind {
    HIR_PATH_ITEM,
    HIR_PATH_ASSOC,
    HIR_PATH_LOCAL,
};

struct HirPath {
    struct SourceSpan span;
    struct HirSegments *segments;
    enum HirPathKind kind;
};

struct HirImport {
    struct HirIdent item;
    struct HirIdent as;
    paw_Bool has_star : 1;
    int modno;
};

void pawHir_init_segment(struct Hir *hir, struct HirSegment *r, NodeId id, struct HirIdent ident, struct HirGenericArgs *args, NodeId target);

struct HirVariant {
    int discr;
    struct HirIdent ident;
    struct HirFieldList *fields;
};

struct HirGeneric {
    struct HirIdent ident;
};

struct HirParam {
    struct HirIdent ident;
    struct HirType *tag;
};

struct HirField {
    paw_Bool is_pub : 1;
    struct HirIdent ident;
    struct HirType *tag;
};

struct HirGenericBound {
    struct HirPath path;
};

enum HirTypeKind {
#define X(Kind_) kHir##Kind_,
    HIR_TYPE_LIST(X)
#undef X
};

#define HIR_TYPE_HEADER \
    struct SourceSpan span; \
    NodeId id; \
    enum HirTypeKind kind : 8
struct HirTypeHeader {
    HIR_TYPE_HEADER;
};

struct HirRefType {
    HIR_TYPE_HEADER;
    paw_Bool is_mut : 1;
    struct HirType *type;
};

// Path to a type
struct HirPathType {
    HIR_TYPE_HEADER;
    struct HirPath path;
};

struct HirFnPtr {
    HIR_TYPE_HEADER;
    struct HirTypeList *params;
    struct HirType *result;
};

struct HirSliceType {
    HIR_TYPE_HEADER;
    struct HirType *type;
};

struct HirArrayType {
    HIR_TYPE_HEADER;
    struct HirType *type;
    struct HirExpr *length;
};

struct HirTupleType {
    HIR_TYPE_HEADER;
    struct HirTypeList *elems;
};

struct HirProjectionType {
    HIR_TYPE_HEADER;
    struct HirType *type;
    struct HirPath trait;
    Str const *name;
};

struct HirNeverType {
    HIR_TYPE_HEADER;
};

struct HirInferType {
    HIR_TYPE_HEADER;
};

struct HirType {
    union {
        struct HirTypeHeader hdr;
#define X(Kind_) struct Hir##Kind_ Kind_##_;
        HIR_TYPE_LIST(X)
#undef X
    };
};

#define X(Kind_) \
    static inline paw_Bool HirIs##Kind_(struct HirType const *node) \
    { \
        return node->hdr.kind == kHir##Kind_; \
    } \
    static inline struct Hir##Kind_ *HirGet##Kind_(struct HirType *node) \
    { \
        paw_assert(HirIs##Kind_(node)); \
        return &node->Kind_##_; \
    }
HIR_TYPE_LIST(X)
#undef X

struct HirType *pawHir_new_type(struct Hir *hir);

static struct HirType *pawHir_new_projection_type(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirType *type, struct HirPath trait, Str const *name)
{
    struct HirType *t = pawHir_new_type(hir);
    t->ProjectionType_ = (struct HirProjectionType){
        .id = id,
        .span = span,
        .kind = kHirProjectionType,
        .type = type,
        .trait = trait,
        .name = name,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

static struct HirType *pawHir_new_never_type(struct Hir *hir, struct SourceSpan span, NodeId id)
{
    struct HirType *t = pawHir_new_type(hir);
    t->NeverType_ = (struct HirNeverType){
        .id = id,
        .span = span,
        .kind = kHirNeverType,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

static struct HirType *pawHir_new_infer_type(struct Hir *hir, struct SourceSpan span, NodeId id)
{
    struct HirType *t = pawHir_new_type(hir);
    t->InferType_ = (struct HirInferType){
        .id = id,
        .span = span,
        .kind = kHirInferType,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

static struct HirType *pawHir_new_ref_type(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirType *type, paw_Bool is_mut)
{
    struct HirType *t = pawHir_new_type(hir);
    t->RefType_ = (struct HirRefType){
        .id = id,
        .span = span,
        .kind = kHirRefType,
        .type = type,
        .is_mut = is_mut,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

static struct HirType *pawHir_new_path_type(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPath path)
{
    struct HirType *t = pawHir_new_type(hir);
    t->PathType_ = (struct HirPathType){
        .id = id,
        .span = span,
        .kind = kHirPathType,
        .path = path,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

static struct HirType *pawHir_new_fn_ptr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirTypeList *params, struct HirType *result)
{
    struct HirType *t = pawHir_new_type(hir);
    t->FnPtr_ = (struct HirFnPtr){
        .id = id,
        .span = span,
        .kind = kHirFnPtr,
        .params = params,
        .result = result,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

static struct HirType *pawHir_new_slice_type(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirType *type)
{
    struct HirType *t = pawHir_new_type(hir);
    t->SliceType_ = (struct HirSliceType){
        .id = id,
        .span = span,
        .kind = kHirSliceType,
        .type = type,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

static struct HirType *pawHir_new_array_type(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirType *type, struct HirExpr *length)
{
    struct HirType *t = pawHir_new_type(hir);
    t->ArrayType_ = (struct HirArrayType){
        .id = id,
        .span = span,
        .kind = kHirArrayType,
        .type = type,
        .length = length,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

static struct HirType *pawHir_new_tuple_type(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirTypeList *elems)
{
    struct HirType *t = pawHir_new_type(hir);
    t->TupleType_ = (struct HirTupleType){
        .id = id,
        .span = span,
        .kind = kHirTupleType,
        .elems = elems,
    };
    pawHir_register_node(hir, id, t);
    return t;
}

enum HirDeclKind {
#define X(Kind_) kHir##Kind_,
    HIR_DECL_LIST(X)
#undef X
};

#define HIR_DECL_HEADER \
    struct SourceSpan span; \
    NodeId id; \
    DeclId did; \
    enum HirDeclKind kind : 8
struct HirDeclHeader {
    HIR_DECL_HEADER;
};

struct HirConstDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct Annotations *annos;
    struct HirIdent ident;
    struct HirExpr *init;
    struct HirType *tag;
};

// Node representing a type declaration
// Used for type aliases and builtin types.
struct HirTypeDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirIdent ident;
    struct HirType *rhs;
    struct HirDeclList *generics;
};

struct HirFnDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_assoc : 1;
    enum FnKind fn_kind : 6;
    DeclId parent_id; // set later
    struct Annotations *annos;
    struct HirIdent ident;
    struct HirDeclList *generics;
    struct HirDeclList *params;
    struct HirType *result;
    struct HirExpr *body;
};

struct HirImplDecl {
    HIR_DECL_HEADER;
    struct HirType *trait;
    struct HirType *type;
    struct HirDeclList *generics;
    struct HirDeclList *types;
    struct HirDeclList *constants;
    struct HirDeclList *methods;
};

struct HirAdtDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_struct : 1;
    struct HirIdent ident;
    struct HirDeclList *generics;
    struct HirDeclList *variants;
};

struct HirVariantDecl {
    HIR_DECL_HEADER;
    DeclId base_did;
    int index;
    struct HirIdent ident;
    struct HirDeclList *fields;
};

// Represents a generic type parameter
struct HirGenericDecl {
    HIR_DECL_HEADER;
    paw_Bool is_type : 1;
    union {
        struct {
            struct HirIdent ident;
            struct HirBoundList *bounds;
        } t;

        struct {
            struct HirType *type;
            struct HirIdent ident;
        } k;
    };
};

// HIR node representing a 'Field' production
struct HirFieldDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirIdent ident;
    struct HirType *tag;
};

struct HirParamDecl {
    HIR_DECL_HEADER;
    paw_Bool is_self : 1;
    struct HirIdent ident;
    struct HirType *tag;
};

struct HirTraitDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirIdent ident;
    struct HirDeclList *generics;
    struct HirDeclList *types;
    struct HirDeclList *methods;
};

struct HirDecl {
    union {
        struct HirDeclHeader hdr;
#define X(Kind_) struct Hir##Kind_ Kind_##_;
        HIR_DECL_LIST(X)
#undef X
    };
};

#define X(Kind_) \
    static inline paw_Bool HirIs##Kind_(struct HirDecl const *node) \
    { \
        return node->hdr.kind == kHir##Kind_; \
    } \
    static inline struct Hir##Kind_ *HirGet##Kind_(struct HirDecl *node) \
    { \
        paw_assert(HirIs##Kind_(node)); \
        return &node->Kind_##_; \
    }
HIR_DECL_LIST(X)
#undef X

struct HirDecl *pawHir_new_decl(struct Hir *hir);

static struct HirDecl *pawHir_new_const_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct Annotations *annos, struct HirType *tag, struct HirExpr *init, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->ConstDecl_ = (struct HirConstDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirConstDecl,
        .ident = ident,
        .annos = annos,
        .tag = tag,
        .init = init,
        .is_pub = is_pub,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_type_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct HirDeclList *generics, struct HirType *rhs, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->TypeDecl_ = (struct HirTypeDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirTypeDecl,
        .ident = ident,
        .generics = generics,
        .rhs = rhs,
        .is_pub = is_pub,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_fn_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct Annotations *annos, struct HirDeclList *generics, struct HirDeclList *params, struct HirType *result, struct HirExpr *body, enum FnKind fn_kind, paw_Bool is_pub, paw_Bool is_assoc)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->FnDecl_ = (struct HirFnDecl){
        .parent_id = INVALID_DECL_ID,
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirFnDecl,
        .ident = ident,
        .annos = annos,
        .generics = generics,
        .params = params,
        .result = result,
        .body = body,
        .fn_kind = fn_kind,
        .is_pub = is_pub,
        .is_assoc = is_assoc,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

inline static struct HirDecl *pawHir_new_impl_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirType *type, struct HirType *trait, struct HirDeclList *generics, struct HirDeclList *types, struct HirDeclList *constants, struct HirDeclList *methods)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->ImplDecl_ = (struct HirImplDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirImplDecl,
        .type = type,
        .trait = trait,
        .generics = generics,
        .types = types,
        .constants = constants,
        .methods = methods,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_adt_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct HirDeclList *generics, struct HirDeclList *variants, paw_Bool is_pub, paw_Bool is_struct)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->AdtDecl_ = (struct HirAdtDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirAdtDecl,
        .ident = ident,
        .generics = generics,
        .variants = variants,
        .is_pub = is_pub,
        .is_struct = is_struct,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_variant_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct HirDeclList *fields, int index, DeclId base_did)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->VariantDecl_ = (struct HirVariantDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirVariantDecl,
        .base_did = base_did,
        .ident = ident,
        .fields = fields,
        .index = index,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_generic_type_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct HirBoundList *bounds)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->GenericDecl_ = (struct HirGenericDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirGenericDecl,
        .is_type = PAW_TRUE,
        .t.ident = ident,
        .t.bounds = bounds,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_generic_const_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirType *type, struct HirIdent ident)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->GenericDecl_ = (struct HirGenericDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirGenericDecl,
        .is_type = PAW_FALSE,
        .k.type = type,
        .k.ident = ident,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_field_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct HirType *tag, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->FieldDecl_ = (struct HirFieldDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirFieldDecl,
        .ident = ident,
        .tag = tag,
        .is_pub = is_pub,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_param_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct HirType *tag)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->ParamDecl_ = (struct HirParamDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirParamDecl,
        .ident = ident,
        .tag = tag,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

static struct HirDecl *pawHir_new_trait_decl(struct Hir *hir, struct SourceSpan span, NodeId id, DeclId did, struct HirIdent ident, struct HirDeclList *generics, struct HirDeclList *types, struct HirDeclList *methods, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->TraitDecl_ = (struct HirTraitDecl){
        .id = id,
        .did = did,
        .span = span,
        .kind = kHirTraitDecl,
        .ident = ident,
        .generics = generics,
        .types = types,
        .methods = methods,
        .is_pub = is_pub,
    };
    pawHir_register_node(hir, id, d);
    pawHir_register_decl(hir, did, d);
    return d;
}

enum HirExprKind {
#define X(Kind_) kHir##Kind_,
    HIR_EXPR_LIST(X)
#undef X
};

#define HIR_EXPR_HEADER \
    struct SourceSpan span; \
    NodeId id; \
    enum HirExprKind kind : 8
struct HirExprHeader {
    HIR_EXPR_HEADER;
};

struct HirAscriptionExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *expr;
    struct HirType *type;
};

struct HirPathExpr {
    HIR_EXPR_HEADER;
    struct HirPath path;
};

enum HirLitKind {
    kHirLitBasic,
    kHirLitComposite,
    kHirLitArray,
    kHirLitTuple,
};

struct HirBasicLit {
    Value value;
    enum BuiltinKind code;
};

struct HirArrayLit {
    struct HirExprList *elems;
};

struct HirTupleLit {
    struct HirExprList *elems;
};

struct HirCompositeLit {
    struct HirPath path;
    struct HirExprList *items;
};

struct HirLiteralExpr {
    HIR_EXPR_HEADER;
    enum HirLitKind lit_kind;
    union {
        struct HirBasicLit basic;
        struct HirArrayLit array;
        struct HirTupleLit tuple;
        struct HirCompositeLit comp;
    };
};

struct HirClosureExpr {
    HIR_EXPR_HEADER;
    struct HirDeclList *params;
    struct HirType *result;
    struct HirExpr *expr;
};

struct HirFieldExpr {
    HIR_EXPR_HEADER;
    int fid;
    union {
        struct HirExpr *key;
        struct HirIdent ident;
    };
    struct HirExpr *value;
};

struct HirUnOpExpr {
    HIR_EXPR_HEADER;
    enum UnaryOp op : 8;
    struct HirExpr *target;
};

#define HIR_IS_UNOP(Expr_, Kind_) (HirIsUnOpExpr(Expr_) && HirGetUnOpExpr(Expr_)->op == (Kind_))

struct HirBinOpExpr {
    HIR_EXPR_HEADER;
    enum BinaryOp op : 8;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

#define HIR_IS_BINOP(Expr_, Kind_) (HirIsBinOpExpr(Expr_) && HirGetBinOpExpr(Expr_)->op == (Kind_))

struct HirLogicalExpr {
    HIR_EXPR_HEADER;
    paw_Bool is_and : 1;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

struct HirChainExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *target;
};

struct HirCallExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *target;
    struct HirExprList *args;
};

struct HirSelector {
    HIR_EXPR_HEADER;
    struct HirExpr *target;
    paw_Bool is_index : 1;
    union {
        struct HirIdent ident;
        int index;
    };
};

struct HirIndex {
    HIR_EXPR_HEADER;
    struct HirExpr *target;
    struct HirExpr *index;
};

struct HirConversionExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *from;
    struct HirType *to;
};

struct HirProjectionExpr {
    HIR_EXPR_HEADER;
    struct HirType *type;
    struct HirPath trait;
    struct HirIdent ident;
    struct HirExprList *args;
};

struct HirOpAssignExpr {
    HIR_EXPR_HEADER;
    enum BinaryOp op : 8;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

struct HirAssignExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

// If "never" is equal to true, then the block contains an unconditional
// jump, either directly or transitively through 1 or more nested block
// expressions (not including blocks that only execute conditionally,
// such as a loop).
struct HirBlock {
    HIR_EXPR_HEADER;
    struct HirStmtList *stmts;
    struct HirExpr *result;
};

struct HirReturnExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *expr;
};

struct HirJumpExpr {
    HIR_EXPR_HEADER;
    enum JumpKind jump_kind;
};

struct HirLoopExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *block;
};

struct HirMatchArm {
    HIR_EXPR_HEADER;
    struct HirPat *pat;
    struct HirExpr *guard;
    struct HirExpr *result;
};

struct HirMatchExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *target;
    struct HirExprList *arms;
};

struct HirExpr {
    union {
        struct HirExprHeader hdr;
#define X(Kind_) struct Hir##Kind_ Kind_##_;
        HIR_EXPR_LIST(X)
#undef X
    };
};

#define X(Kind_) \
    static inline paw_Bool HirIs##Kind_(struct HirExpr const *node) \
    { \
        return node->hdr.kind == kHir##Kind_; \
    } \
    static inline struct Hir##Kind_ *HirGet##Kind_(struct HirExpr *node) \
    { \
        paw_assert(HirIs##Kind_(node)); \
        return &node->Kind_##_; \
    }
HIR_EXPR_LIST(X)
#undef X

struct HirExpr *pawHir_new_expr(struct Hir *hir);

static struct HirExpr *pawHir_new_ascription_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *expr, struct HirType *type)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->AscriptionExpr_ = (struct HirAscriptionExpr){
        .id = id,
        .span = span,
        .kind = kHirAscriptionExpr,
        .expr = expr,
        .type = type,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_path_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPath path)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->PathExpr_ = (struct HirPathExpr){
        .id = id,
        .span = span,
        .kind = kHirPathExpr,
        .path = path,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_basic_lit(struct Hir *hir, struct SourceSpan span, NodeId id, Value value, enum BuiltinKind code)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LiteralExpr_ = (struct HirLiteralExpr){
        .id = id,
        .span = span,
        .kind = kHirLiteralExpr,
        .lit_kind = kHirLitBasic,
        .basic.value = value,
        .basic.code = code,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_array_lit(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExprList *elems)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LiteralExpr_ = (struct HirLiteralExpr){
        .id = id,
        .span = span,
        .kind = kHirLiteralExpr,
        .lit_kind = kHirLitArray,
        .tuple.elems = elems,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_tuple_lit(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExprList *elems)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LiteralExpr_ = (struct HirLiteralExpr){
        .id = id,
        .span = span,
        .kind = kHirLiteralExpr,
        .lit_kind = kHirLitTuple,
        .tuple.elems = elems,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_composite_lit(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPath path, struct HirExprList *items)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LiteralExpr_ = (struct HirLiteralExpr){
        .id = id,
        .span = span,
        .kind = kHirLiteralExpr,
        .lit_kind = kHirLitComposite,
        .comp.path = path,
        .comp.items = items,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_closure_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirDeclList *params, struct HirType *result, struct HirExpr *expr)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ClosureExpr_ = (struct HirClosureExpr){
        .id = id,
        .span = span,
        .kind = kHirClosureExpr,
        .params = params,
        .result = result,
        .expr = expr,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_named_field_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirIdent ident, struct HirExpr *value, int fid)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->FieldExpr_ = (struct HirFieldExpr){
        .id = id,
        .span = span,
        .kind = kHirFieldExpr,
        .ident = ident,
        .value = value,
        .fid = fid,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_keyed_field_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *key, struct HirExpr *value)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->FieldExpr_ = (struct HirFieldExpr){
        .id = id,
        .span = span,
        .kind = kHirFieldExpr,
        .key = key,
        .value = value,
        .fid = -1,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_unop_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *target, enum UnaryOp op)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->UnOpExpr_ = (struct HirUnOpExpr){
        .id = id,
        .span = span,
        .kind = kHirUnOpExpr,
        .target = target,
        .op = op,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_binop_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *lhs, struct HirExpr *rhs, enum BinaryOp op)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->BinOpExpr_ = (struct HirBinOpExpr){
        .id = id,
        .span = span,
        .kind = kHirBinOpExpr,
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_logical_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *lhs, struct HirExpr *rhs, paw_Bool is_and)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LogicalExpr_ = (struct HirLogicalExpr){
        .id = id,
        .span = span,
        .kind = kHirLogicalExpr,
        .is_and = is_and,
        .lhs = lhs,
        .rhs = rhs,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_chain_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *target)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ChainExpr_ = (struct HirChainExpr){
        .id = id,
        .span = span,
        .target = target,
        .kind = kHirChainExpr,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_index_selector(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *target, int index)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->Selector_ = (struct HirSelector){
        .id = id,
        .span = span,
        .kind = kHirSelector,
        .target = target,
        .index = index,
        .is_index = PAW_TRUE,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_name_selector(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *target, struct HirIdent ident)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->Selector_ = (struct HirSelector){
        .id = id,
        .span = span,
        .kind = kHirSelector,
        .target = target,
        .ident = ident,
        .is_index = PAW_FALSE,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_index_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *target, struct HirExpr *index)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->Index_ = (struct HirIndex){
        .id = id,
        .span = span,
        .kind = kHirIndex,
        .target = target,
        .index = index,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_conversion_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *from, struct HirType *to)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ConversionExpr_ = (struct HirConversionExpr){
        .id = id,
        .span = span,
        .kind = kHirConversionExpr,
        .from = from,
        .to = to,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_projection_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirType *type, struct HirPath trait, struct HirIdent ident, struct HirExprList *args)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ProjectionExpr_ = (struct HirProjectionExpr){
        .id = id,
        .span = span,
        .kind = kHirProjectionExpr,
        .type = type,
        .trait = trait,
        .ident = ident,
        .args = args,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_assign_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *lhs, struct HirExpr *rhs)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->AssignExpr_ = (struct HirAssignExpr){
        .id = id,
        .span = span,
        .kind = kHirAssignExpr,
        .lhs = lhs,
        .rhs = rhs,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_op_assign_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *lhs, struct HirExpr *rhs, enum BinaryOp op)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->OpAssignExpr_ = (struct HirOpAssignExpr){
        .id = id,
        .span = span,
        .kind = kHirOpAssignExpr,
        .lhs = lhs,
        .rhs = rhs,
        .op = op,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_return_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *expr)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ReturnExpr_ = (struct HirReturnExpr){
        .id = id,
        .span = span,
        .kind = kHirReturnExpr,
        .expr = expr,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_call_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *target, struct HirExprList *args)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->CallExpr_ = (struct HirCallExpr){
        .id = id,
        .span = span,
        .kind = kHirCallExpr,
        .target = target,
        .args = args,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_match_arm(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPat *pat, struct HirExpr *guard, struct HirExpr *result)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->MatchArm_ = (struct HirMatchArm){
        .id = id,
        .span = span,
        .kind = kHirMatchArm,
        .pat = pat,
        .guard = guard,
        .result = result,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_match_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *target, struct HirExprList *arms)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->MatchExpr_ = (struct HirMatchExpr){
        .id = id,
        .span = span,
        .kind = kHirMatchExpr,
        .target = target,
        .arms = arms,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_block(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirStmtList *stmts, struct HirExpr *result)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->Block_ = (struct HirBlock){
        .id = id,
        .span = span,
        .kind = kHirBlock,
        .stmts = stmts,
        .result = result,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_loop_expr(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *block)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LoopExpr_ = (struct HirLoopExpr){
        .id = id,
        .span = span,
        .kind = kHirLoopExpr,
        .block = block,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

static struct HirExpr *pawHir_new_jump_expr(struct Hir *hir, struct SourceSpan span, NodeId id, enum JumpKind jump_kind)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->JumpExpr_ = (struct HirJumpExpr){
        .id = id,
        .span = span,
        .kind = kHirJumpExpr,
        .jump_kind = jump_kind,
    };
    pawHir_register_node(hir, id, e);
    return e;
}

enum HirStmtKind {
#define X(Kind_) kHir##Kind_,
    HIR_STMT_LIST(X)
#undef X
};

#define HIR_STMT_HEADER \
    struct SourceSpan span; \
    NodeId id; \
    enum HirStmtKind kind : 8
struct HirStmtHeader {
    HIR_STMT_HEADER;
};

struct HirLetStmt {
    HIR_STMT_HEADER;
    struct HirPat *pat;
    struct HirExpr *init;
    struct HirType *tag;
};

struct HirDeclStmt {
    HIR_STMT_HEADER;
    struct HirDecl *decl;
};

struct HirExprStmt {
    HIR_STMT_HEADER;
    struct HirExpr *expr;
};

struct HirStmt {
    union {
        struct HirStmtHeader hdr;
#define X(Kind_) struct Hir##Kind_ Kind_##_;
        HIR_STMT_LIST(X)
#undef X
    };
};

#define X(Kind_) \
    static inline paw_Bool HirIs##Kind_(struct HirStmt const *node) \
    { \
        return node->hdr.kind == kHir##Kind_; \
    } \
    static inline struct Hir##Kind_ *HirGet##Kind_(struct HirStmt *node) \
    { \
        paw_assert(HirIs##Kind_(node)); \
        return &node->Kind_##_; \
    }
HIR_STMT_LIST(X)
#undef X

struct HirStmt *pawHir_new_stmt(struct Hir *hir);

static struct HirStmt *pawHir_new_let_stmt(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPat *pat, struct HirType *tag, struct HirExpr *init)
{
    struct HirStmt *s = pawHir_new_stmt(hir);
    s->LetStmt_ = (struct HirLetStmt){
        .id = id,
        .span = span,
        .kind = kHirLetStmt,
        .pat = pat,
        .tag = tag,
        .init = init,
    };
    pawHir_register_node(hir, id, s);
    return s;
}

static struct HirStmt *pawHir_new_expr_stmt(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *expr)
{
    struct HirStmt *s = pawHir_new_stmt(hir);
    s->ExprStmt_ = (struct HirExprStmt){
        .id = id,
        .span = span,
        .kind = kHirExprStmt,
        .expr = expr,
    };
    pawHir_register_node(hir, id, s);
    return s;
}

static struct HirStmt *pawHir_new_decl_stmt(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirDecl *decl)
{
    struct HirStmt *s = pawHir_new_stmt(hir);
    s->DeclStmt_ = (struct HirDeclStmt){
        .id = id,
        .span = span,
        .kind = kHirDeclStmt,
        .decl = decl,
    };
    pawHir_register_node(hir, id, s);
    return s;
}

enum HirPatKind {
#define X(Kind_) kHir##Kind_,
    HIR_PAT_LIST(X)
#undef X
};

#define HIR_PAT_HEADER \
    struct SourceSpan span; \
    NodeId id; \
    enum HirPatKind kind : 8

struct HirPatHeader {
    HIR_PAT_HEADER;
};

struct HirOrPat {
    HIR_PAT_HEADER;
    struct HirPatList *pats;
};

struct HirRefPat {
    HIR_PAT_HEADER;
    struct HirPat *referent;
};

struct HirPtrPat {
    HIR_PAT_HEADER;
    struct HirPat *pointee;
};

// NOTE: only used for HirStructPat
struct HirFieldPat {
    HIR_PAT_HEADER;
    int index;
    struct HirIdent ident;
    struct HirPat *pat;
};

struct HirStructPat {
    HIR_PAT_HEADER;
    struct HirPath path;
    struct HirPatList *fields; // [HirFieldPat]
};

struct HirVariantPat {
    HIR_PAT_HEADER;
    int index;
    struct HirPath path;
    struct HirPatList *fields;
};

struct HirTuplePat {
    HIR_PAT_HEADER;
    struct HirPatList *elems;
};

struct HirBindingPat {
    HIR_PAT_HEADER;
    struct HirIdent ident;
};

struct HirWildcardPat {
    HIR_PAT_HEADER;
};

struct HirLiteralPat {
    HIR_PAT_HEADER;
    struct HirExpr *expr;
};

struct HirPat {
    union {
        struct HirPatHeader hdr;
#define X(Kind_) struct Hir##Kind_ Kind_##_;
        HIR_PAT_LIST(X)
#undef X
    };
};

#define X(Kind_) \
    static inline paw_Bool HirIs##Kind_(struct HirPat const *node) \
    { \
        return node->hdr.kind == kHir##Kind_; \
    } \
    static inline struct Hir##Kind_ *HirGet##Kind_(struct HirPat *node) \
    { \
        paw_assert(HirIs##Kind_(node)); \
        return &node->Kind_##_; \
    }
HIR_PAT_LIST(X)
#undef X

struct HirPat *pawHir_new_pat(struct Hir *hir);

static struct HirPat *pawHir_new_or_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPatList *pats)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->OrPat_ = (struct HirOrPat){
        .id = id,
        .span = span,
        .kind = kHirOrPat,
        .pats = pats,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_ref_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPat *referent)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->RefPat_ = (struct HirRefPat){
        .id = id,
        .span = span,
        .kind = kHirRefPat,
        .referent = referent,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_ptr_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPat *pointee)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->PtrPat_ = (struct HirPtrPat){
        .id = id,
        .span = span,
        .kind = kHirPtrPat,
        .pointee = pointee,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_field_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirIdent ident, struct HirPat *pat, int index)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->FieldPat_ = (struct HirFieldPat){
        .id = id,
        .span = span,
        .kind = kHirFieldPat,
        .ident = ident,
        .index = index,
        .pat = pat,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_struct_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPath path, struct HirPatList *fields)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->StructPat_ = (struct HirStructPat){
        .id = id,
        .span = span,
        .kind = kHirStructPat,
        .path = path,
        .fields = fields,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_variant_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPath path, struct HirPatList *fields, int index)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->VariantPat_ = (struct HirVariantPat){
        .id = id,
        .span = span,
        .kind = kHirVariantPat,
        .path = path,
        .fields = fields,
        .index = index,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_tuple_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirPatList *elems)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->TuplePat_ = (struct HirTuplePat){
        .id = id,
        .span = span,
        .kind = kHirTuplePat,
        .elems = elems,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_binding_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirIdent ident)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->BindingPat_ = (struct HirBindingPat){
        .id = id,
        .span = span,
        .kind = kHirBindingPat,
        .ident = ident,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_wildcard_pat(struct Hir *hir, struct SourceSpan span, NodeId id)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->WildcardPat_ = (struct HirWildcardPat){
        .id = id,
        .span = span,
        .kind = kHirWildcardPat,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

static struct HirPat *pawHir_new_literal_pat(struct Hir *hir, struct SourceSpan span, NodeId id, struct HirExpr *expr)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->LiteralPat_ = (struct HirLiteralPat){
        .id = id,
        .span = span,
        .kind = kHirLiteralPat,
        .expr = expr,
    };
    pawHir_register_node(hir, id, p);
    return p;
}

struct HirVisitor {
    struct Hir *hir;
    void *ud;

    paw_Bool (*VisitPath)(struct HirVisitor *V, struct HirPath *path);
    paw_Bool (*VisitSegment)(struct HirVisitor *V, struct HirSegment *seg);
    paw_Bool (*VisitExpr)(struct HirVisitor *V, struct HirExpr *node);
    paw_Bool (*VisitStmt)(struct HirVisitor *V, struct HirStmt *node);
    paw_Bool (*VisitDecl)(struct HirVisitor *V, struct HirDecl *node);
    paw_Bool (*VisitType)(struct HirVisitor *V, struct HirType *node);
    paw_Bool (*VisitPat)(struct HirVisitor *V, struct HirPat *node);

    void (*PostVisitPath)(struct HirVisitor *V, struct HirPath *node);
    void (*PostVisitSegment)(struct HirVisitor *V, struct HirSegment *node);
    void (*PostVisitExpr)(struct HirVisitor *V, struct HirExpr *node);
    void (*PostVisitStmt)(struct HirVisitor *V, struct HirStmt *node);
    void (*PostVisitDecl)(struct HirVisitor *V, struct HirDecl *node);
    void (*PostVisitType)(struct HirVisitor *V, struct HirType *node);
    void (*PostVisitPat)(struct HirVisitor *V, struct HirPat *node);

#define X(Kind_) \
    paw_Bool (*Visit##Kind_)(struct HirVisitor * V, struct Hir##Kind_ * node); \
    void (*PostVisit##Kind_)(struct HirVisitor * V, struct Hir##Kind_ * node);
    HIR_EXPR_LIST(X)
    HIR_DECL_LIST(X)
    HIR_STMT_LIST(X)
    HIR_TYPE_LIST(X)
    HIR_PAT_LIST(X)
#undef X
};

void pawHir_visitor_init(struct HirVisitor *V, struct Hir *hir, void *ud);

// Visitor entrypoints for each kind of HIR node:
void pawHir_visit_expr(struct HirVisitor *V, struct HirExpr *node);
void pawHir_visit_stmt(struct HirVisitor *V, struct HirStmt *node);
void pawHir_visit_decl(struct HirVisitor *V, struct HirDecl *node);
void pawHir_visit_type(struct HirVisitor *V, struct HirType *node);
void pawHir_visit_pat(struct HirVisitor *V, struct HirPat *node);
void pawHir_visit_expr_list(struct HirVisitor *V, struct HirExprList *list);
void pawHir_visit_stmt_list(struct HirVisitor *V, struct HirStmtList *list);
void pawHir_visit_decl_list(struct HirVisitor *V, struct HirDeclList *list);
void pawHir_visit_type_list(struct HirVisitor *V, struct HirTypeList *list);
void pawHir_visit_pat_list(struct HirVisitor *V, struct HirPatList *list);
void pawHir_visit_generic_args(struct HirVisitor *F, struct HirGenericArgs *list);


struct HirFolder {
    struct Hir *hir;
    void *ud;

    struct HirPath (*FoldPath)(struct HirFolder *F, struct HirPath path);
    struct HirSegment (*FoldSegment)(struct HirFolder *F, struct HirSegment seg);
    struct HirExpr *(*FoldExpr)(struct HirFolder *F, struct HirExpr *node);
    struct HirStmt *(*FoldStmt)(struct HirFolder *F, struct HirStmt *node);
    struct HirDecl *(*FoldDecl)(struct HirFolder *F, struct HirDecl *node);
    struct HirType *(*FoldType)(struct HirFolder *F, struct HirType *node);
    struct HirPat *(*FoldPat)(struct HirFolder *F, struct HirPat *node);

#define X(Kind_) \
        struct HirExpr *(*Fold##Kind_)(struct HirFolder *, struct Hir##Kind_ *);
    HIR_EXPR_LIST(X)
#undef X

#define X(Kind_) \
        struct HirDecl *(*Fold##Kind_)(struct HirFolder *, struct Hir##Kind_ *);
    HIR_DECL_LIST(X)
#undef X

#define X(Kind_) \
        struct HirStmt *(*Fold##Kind_)(struct HirFolder *, struct Hir##Kind_ *);
    HIR_STMT_LIST(X)
#undef X

#define X(Kind_) \
        struct HirType *(*Fold##Kind_)(struct HirFolder *, struct Hir##Kind_ *);
    HIR_TYPE_LIST(X)
#undef X

#define X(Kind_) \
        struct HirPat *(*Fold##Kind_)(struct HirFolder *, struct Hir##Kind_ *);
    HIR_PAT_LIST(X)
#undef X
};

void pawHir_folder_init(struct HirFolder *F, struct Hir *hir, void *ud);

// Folder entrypoints for each kind of HIR node:
struct HirExpr *pawHir_fold_expr(struct HirFolder *F, struct HirExpr *node);
struct HirStmt *pawHir_fold_stmt(struct HirFolder *F, struct HirStmt *node);
struct HirDecl *pawHir_fold_decl(struct HirFolder *F, struct HirDecl *node);
struct HirType *pawHir_fold_type(struct HirFolder *F, struct HirType *node);
struct HirPat *pawHir_fold_pat(struct HirFolder *F, struct HirPat *node);
struct HirExprList *pawHir_fold_expr_list(struct HirFolder *F, struct HirExprList *list);
struct HirStmtList *pawHir_fold_stmt_list(struct HirFolder *F, struct HirStmtList *list);
struct HirDeclList *pawHir_fold_decl_list(struct HirFolder *F, struct HirDeclList *list);
struct HirTypeList *pawHir_fold_type_list(struct HirFolder *F, struct HirTypeList *list);
struct HirPatList *pawHir_fold_pat_list(struct HirFolder *F, struct HirPatList *list);
struct HirGenericArgs *pawHir_fold_generic_args(struct HirFolder *F, struct HirGenericArgs *list);


struct HirModule {
    int modno;
    struct HirDeclList *items;
    Str *name;
};

struct Hir {
    struct HirModuleList *modules;
    struct HirNodeMap *nodes;
    struct HirDeclMap *decls;
    struct Compiler *C;
    struct Pool *pool;
    paw_Env *P;
    int node_count;
};

DEFINE_LIST(struct Hir, HirModuleList, struct HirModule)
DEFINE_LIST(struct Hir, HirDeclList, struct HirDecl *)
DEFINE_LIST(struct Hir, HirExprList, struct HirExpr *)
DEFINE_LIST(struct Hir, HirStmtList, struct HirStmt *)
DEFINE_LIST(struct Hir, HirTypeList, struct HirType *)
DEFINE_LIST(struct Hir, HirPatList, struct HirPat *)
DEFINE_LIST(struct Hir, HirGenericList, struct HirGeneric *)
DEFINE_LIST(struct Hir, HirFieldList, struct HirField *)
DEFINE_LIST(struct Hir, HirParamList, struct HirParam *)
DEFINE_LIST(struct Hir, HirVariantList, struct HirVariant *)
DEFINE_LIST(struct Hir, HirImportList, struct HirImport)
DEFINE_LIST(struct Hir, HirSegments, struct HirSegment)
DEFINE_LIST(struct Hir, HirBoundList, struct HirGenericBound)
DEFINE_LIST(struct Hir, HirGenericArgs, struct HirGenericArg)

DEFINE_MAP(struct Hir, HirNodeMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, void *)
DEFINE_MAP(struct Hir, HirDeclMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct HirDecl *)
DEFINE_MAP_ITERATOR(HirDeclMap, DeclId, struct HirDecl *)

static inline struct HirPath pawHir_path_create(struct SourceSpan span, struct HirSegments *segments, enum HirPathKind kind)
{
    return (struct HirPath){
        .segments = segments,
        .span = span,
        .kind = kind,
    };
}

#define HIR_IS_POLY_FUNC(decl) (HirIsFnDecl(decl) && HirGetFnDecl(decl)->generics != NULL)
#define HIR_IS_POLY_ADT(decl) (HirIsAdtDecl(decl) && HirGetAdtDecl(decl)->generics != NULL)

#define HIR_PATH_RESULT(Path_) (K_LIST_LAST((Path_).segments).res)
#define HIR_TYPE_DID(Type_) (HirIsPathType(Type_) ? HIR_PATH_RESULT(HirGetPathType(Type_)->path) : HirGetFnDef(Type_)->did)

struct Hir *pawHir_new(struct Compiler *C);
void pawHir_free(struct Hir *hir);

void *pawHir_get_node(struct Hir *hir, NodeId id);
struct HirDecl *pawHir_get_decl(struct Hir *hir, DeclId id);

#define HIR_TYPEOF(x) ((x)->hdr.type)
#define HIR_KINDOF(x) ((x)->hdr.kind)

// NOTE: HirFnPtr is a prefix of HirFnDef
#define HIR_FPTR(t) CHECK_EXP(HirIsFnType(t), &(t)->fptr)

static inline struct HirSegment *pawHir_add_segment(struct Hir *hir, struct HirSegments *segments,
        struct SourceSpan span, NodeId id, struct HirIdent ident, struct HirGenericArgs *args, NodeId target)
{
    HirSegments_push(hir, segments, (struct HirSegment){
        .id = id,
        .span = span,
        .ident = ident,
        .target = target,
        .args = args,
    });
    return &K_LIST_LAST(segments);
}

struct HirExpr *pawHir_copy_expr(struct Hir *hir, struct HirExpr *expr);

struct IrTypeList *pawHir_collect_decl_types(struct Compiler *C, struct HirDeclList *list);

char const *pawHir_print_path(struct Compiler *C, struct HirPath *path);

char const *pawHir_dump(struct Hir *hir);

static inline struct HirVariantDecl *pawHir_struct_variant(struct HirAdtDecl const *d)
{
    paw_assert(d->is_struct);
    return HirGetVariantDecl(K_LIST_FIRST(d->variants));
}

static inline HirDeclList *pawHir_struct_fields(struct HirAdtDecl const *d)
{
    return pawHir_struct_variant(d)->fields;
}

static inline paw_Bool pawHir_is_unit_struct(struct HirAdtDecl const *d)
{
    if (d->is_struct) {
        HirDeclList *fields = pawHir_struct_fields(d);
        return fields->count == 0;
    }
    return PAW_FALSE;
}

#endif // PAW_HIR_H
