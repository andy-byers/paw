// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_HIR_H
#define PAW_HIR_H

#include "compile.h"

#define HIR_DECL_LIST(X) \
    X(FieldDecl)         \
    X(FuncDecl)          \
    X(GenericDecl)       \
    X(AdtDecl)           \
    X(TypeDecl)          \
    X(VarDecl)           \
    X(TraitDecl)         \
    X(VariantDecl)

#define HIR_EXPR_LIST(X) \
    X(LiteralExpr)       \
    X(LogicalExpr)       \
    X(PathExpr)          \
    X(ChainExpr)         \
    X(UnOpExpr)          \
    X(BinOpExpr)         \
    X(ClosureExpr)       \
    X(ConversionExpr)    \
    X(CallExpr)          \
    X(Index)             \
    X(Selector)          \
    X(FieldExpr)         \
    X(AssignExpr)        \
    X(Block)             \
    X(IfExpr)            \
    X(LoopExpr)          \
    X(JumpExpr)          \
    X(ReturnExpr)        \
    X(MatchArm)          \
    X(MatchExpr)

#define HIR_TYPE_LIST(X) \
    X(FuncPtr)           \
    X(TupleType)         \
    X(PathType)          \
    X(InferType)

#define HIR_STMT_LIST(X) \
    X(ExprStmt)          \
    X(DeclStmt)

#define HIR_PAT_LIST(X) \
    X(OrPat)            \
    X(FieldPat)         \
    X(StructPat)        \
    X(VariantPat)       \
    X(TuplePat)         \
    X(PathPat)          \
    X(BindingPat)       \
    X(WildcardPat)      \
    X(LiteralPat)

#define HIR_CAST_DECL(x) CAST(struct HirDecl *, x)
#define HIR_CAST_EXPR(x) CAST(struct HirExpr *, x)
#define HIR_CAST_STMT(x) CAST(struct HirStmt *, x)
#define HIR_CAST_TYPE(x) CAST(struct HirType *, x)
#define HIR_CAST_PAT(x) CAST(struct HirPat *, x)

struct Hir;

struct HirImport {
    String *item;
    String *as;
    paw_Bool has_star : 1;
    int modno;
};

inline static HirId pawHir_next_id(struct Hir *hir);

// Represents an entry in the symbol table
//
// During the type checking pass, a struct HirSymbol is created for each declaration that
// is encountered. When an identifier is referenced, it is looked up in the list
// of symbol tables representing the enclosing scopes (as well as the global
// symbol table).
//
// The symbol table is used for all symbols, but not every symbol will end up in a register.
// In particular, symbols with 'is_type' equal to 1 only exist in the compiler.
struct HirSymbol {
    paw_Bool is_init : 1;
    paw_Bool is_type : 1;
    String *name;
    struct HirDecl *decl;
};

int pawHir_find_symbol(struct HirScope *scope, String const *name);
int pawHir_declare_symbol(struct Hir *hir, struct HirScope *scope, struct HirDecl *decl, String *name);
void pawHir_define_symbol(struct HirScope *scope, int index);

struct HirSegment {
    String *name;
    struct HirTypeList *types;
    DeclId did;
    HirId hid;
};

void pawHir_init_segment(struct Hir *hir, struct HirSegment *r, String *name, struct HirTypeList *types, DeclId did);

struct HirVariant {
    int discr;
    String *name;
    struct HirFieldList *fields;
};

struct HirGeneric {
    String *name;
};

struct HirParam {
    String *name;
    struct HirType *tag;
};

struct HirField {
    paw_Bool is_pub : 1;
    String *name;
    struct HirType *tag;
};

struct HirGenericBound {
    struct HirPath *path;
};

enum HirTypeKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_TYPE_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_TYPE_HEADER      \
    K_ALIGNAS_NODE int line; \
    HirId hid;               \
    enum HirTypeKind kind : 8
struct HirTypeHeader {
    HIR_TYPE_HEADER;
};

// Path to a type
struct HirPathType {
    HIR_TYPE_HEADER;
    struct HirPath *path;
};

struct HirFuncPtr {
    HIR_TYPE_HEADER;
    struct HirTypeList *params;
    struct HirType *result;
};

struct HirTupleType {
    HIR_TYPE_HEADER;
    struct HirTypeList *elems;
};

struct HirInferType {
    HIR_TYPE_HEADER;
};

struct HirType {
    union {
        struct HirTypeHeader hdr;
#define DEFINE_UNION(X) struct Hir##X X##_;
        HIR_TYPE_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static char const *kHirTypeNames[] = {
#define DEFINE_NAME(X) "Hir" #X,
    HIR_TYPE_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                         \
    static inline paw_Bool HirIs##X(struct HirType const *node)  \
    {                                                            \
        return node->hdr.kind == kHir##X;                        \
    }                                                            \
    static inline struct Hir##X *HirGet##X(struct HirType *node) \
    {                                                            \
        paw_assert(HirIs##X(node));                              \
        return &node->X##_;                                      \
    }
HIR_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirType *pawHir_new_type(struct Hir *hir);

static struct HirType *pawHir_new_infer_type(struct Hir *hir, int line)
{
    struct HirType *t = pawHir_new_type(hir);
    t->InferType_ = (struct HirInferType){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirInferType,
    };
    return t;
}

static struct HirType *pawHir_new_path_type(struct Hir *hir, int line, struct HirPath *path)
{
    struct HirType *t = pawHir_new_type(hir);
    t->PathType_ = (struct HirPathType){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirPathType,
        .path = path,
    };
    return t;
}

static struct HirType *pawHir_new_func_ptr(struct Hir *hir, int line, struct HirTypeList *params, struct HirType *result)
{
    struct HirType *t = pawHir_new_type(hir);
    t->FuncPtr_ = (struct HirFuncPtr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirFuncPtr,
        .params = params,
        .result = result,
    };
    return t;
}

static struct HirType *pawHir_new_tuple_type(struct Hir *hir, int line, struct HirTypeList *elems)
{
    struct HirType *t = pawHir_new_type(hir);
    t->TupleType_ = (struct HirTupleType){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirTupleType,
        .elems = elems,
    };
    return t;
}

enum HirDeclKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_DECL_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_DECL_HEADER          \
    K_ALIGNAS_NODE String *name; \
    int line;                    \
    HirId hid;                   \
    DeclId did;                  \
    enum HirDeclKind kind : 8
struct HirDeclHeader {
    HIR_DECL_HEADER;
};

struct HirVarDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_global : 1;
    struct Annotations *annos;
    struct HirExpr *init;
    struct HirType *tag;
};

// Node representing a type declaration
// Used for type aliases and builtin types.
struct HirTypeDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirType *rhs;
    struct HirDeclList *generics;
};

struct HirFuncDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_assoc : 1;
    enum FuncKind fn_kind : 6;
    struct Annotations *annos;
    struct HirDeclList *generics;
    struct HirDeclList *params;
    struct HirType *result;
    struct HirExpr *body;
};

struct HirAdtDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_struct : 1;
    struct HirDecl *self;
    struct HirTypeList *traits;
    struct HirDeclList *generics;
    struct HirDeclList *fields;
    struct HirDeclList *methods;
};

struct HirVariantDecl {
    HIR_DECL_HEADER;
    int index;
    struct HirDeclList *fields;
};

// Represents a generic type parameter
struct HirGenericDecl {
    HIR_DECL_HEADER;
    struct HirBoundList *bounds;
};

// HIR node representing a 'Field' production
struct HirFieldDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirType *tag;
};

struct HirTraitDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirDecl *self;
    struct HirDeclList *generics;
    struct HirDeclList *methods;
};

struct HirDecl {
    union {
        struct HirDeclHeader hdr;
#define DEFINE_UNION(X) struct Hir##X X##_;
        HIR_DECL_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static char const *kHirDeclNames[] = {
#define DEFINE_NAME(X) "Hir" #X,
    HIR_DECL_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                         \
    static inline paw_Bool HirIs##X(struct HirDecl const *node)  \
    {                                                            \
        return node->hdr.kind == kHir##X;                        \
    }                                                            \
    static inline struct Hir##X *HirGet##X(struct HirDecl *node) \
    {                                                            \
        paw_assert(HirIs##X(node));                              \
        return &node->X##_;                                      \
    }
HIR_DECL_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirDecl *pawHir_new_decl(struct Hir *hir);
DeclId pawHir_register_decl(struct Hir *hir, struct HirDecl *decl);

static struct HirDecl *pawHir_new_var_decl(struct Hir *hir, int line, String *name, struct Annotations *annos, struct HirType *tag, struct HirExpr *init, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->VarDecl_ = (struct HirVarDecl){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirVarDecl,
        .name = name,
        .annos = annos,
        .tag = tag,
        .init = init,
        .is_pub = is_pub,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_type_decl(struct Hir *hir, int line, String *name, struct HirDeclList *generics, struct HirType *rhs, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->TypeDecl_ = (struct HirTypeDecl){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirTypeDecl,
        .name = name,
        .generics = generics,
        .rhs = rhs,
        .is_pub = is_pub,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_func_decl(struct Hir *hir, int line, String *name, struct Annotations *annos, struct HirDeclList *generics, struct HirDeclList *params, struct HirType *result, struct HirExpr *body, enum FuncKind fn_kind, paw_Bool is_pub, paw_Bool is_assoc)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->FuncDecl_ = (struct HirFuncDecl){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirFuncDecl,
        .name = name,
        .annos = annos,
        .generics = generics,
        .params = params,
        .result = result,
        .body = body,
        .fn_kind = fn_kind,
        .is_pub = is_pub,
        .is_assoc = is_assoc,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_adt_decl(struct Hir *hir, int line, String *name, struct HirDecl *self, struct HirTypeList *traits, struct HirDeclList *generics, struct HirDeclList *fields, struct HirDeclList *methods, paw_Bool is_pub, paw_Bool is_struct)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->AdtDecl_ = (struct HirAdtDecl){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirAdtDecl,
        .name = name,
        .self = self,
        .traits = traits,
        .generics = generics,
        .fields = fields,
        .methods = methods,
        .is_pub = is_pub,
        .is_struct = is_struct,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_variant_decl(struct Hir *hir, int line, String *name, struct HirDeclList *fields, int index)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->VariantDecl_ = (struct HirVariantDecl){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirVariantDecl,
        .name = name,
        .fields = fields,
        .index = index,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_generic_decl(struct Hir *hir, int line, String *name, struct HirBoundList *bounds)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->GenericDecl_ = (struct HirGenericDecl){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirGenericDecl,
        .name = name,
        .bounds = bounds,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_field_decl(struct Hir *hir, int line, String *name, struct HirType *tag, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->FieldDecl_ = (struct HirFieldDecl){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirFieldDecl,
        .name = name,
        .tag = tag,
        .is_pub = is_pub,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_trait_decl(struct Hir *hir, int line, String *name, struct HirDecl *self, struct HirDeclList *generics, struct HirDeclList *methods, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    d->TraitDecl_ = (struct HirTraitDecl){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirTraitDecl,
        .name = name,
        .self = self,
        .generics = generics,
        .methods = methods,
        .is_pub = is_pub,
    };
    pawHir_register_decl(hir, d);
    return d;
}

enum HirExprKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_EXPR_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_EXPR_HEADER      \
    K_ALIGNAS_NODE int line; \
    HirId hid;               \
    enum HirExprKind kind : 8
struct HirExprHeader {
    HIR_EXPR_HEADER;
};

struct HirPathExpr {
    HIR_EXPR_HEADER;
    struct HirPath *path;
};

enum HirLitKind {
    kHirLitBasic,
    kHirLitComposite,
    kHirLitContainer,
    kHirLitTuple,
};

struct HirBasicLit {
    Value value;
    enum BuiltinKind code;
};

struct HirContainerLit {
    struct HirExprList *items;
    enum BuiltinKind code;
};

struct HirTupleLit {
    struct HirExprList *elems;
};

struct HirCompositeLit {
    struct HirPath *path;
    struct HirExprList *items;
};

struct HirLiteralExpr {
    HIR_EXPR_HEADER;
    enum HirLitKind lit_kind;
    union {
        struct HirBasicLit basic;
        struct HirTupleLit tuple;
        struct HirContainerLit cont;
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
        String *name;
    };
    struct HirExpr *value;
};

struct HirUnOpExpr {
    HIR_EXPR_HEADER;
    enum UnaryOp op : 8;
    struct HirExpr *target;
};

struct HirBinOpExpr {
    HIR_EXPR_HEADER;
    enum BinaryOp op : 8;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

struct HirLogicalExpr {
    HIR_EXPR_HEADER;
    paw_Bool is_and : 1;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

#define HIR_SUFFIXED_HEADER \
    HIR_EXPR_HEADER;        \
    struct HirExpr *target
struct HirSuffixedExpr {
    HIR_SUFFIXED_HEADER;
};

struct HirChainExpr {
    HIR_SUFFIXED_HEADER;
};

struct HirCallExpr {
    HIR_SUFFIXED_HEADER;
    struct HirExprList *args;
};

struct HirSelector {
    HIR_SUFFIXED_HEADER;
    paw_Bool is_index : 1;
    union {
        String *name;
        int index;
    };
};

struct HirIndex {
    HIR_SUFFIXED_HEADER;
    paw_Bool is_slice : 1;
    struct HirExpr *first;
    struct HirExpr *second;
};

struct HirConversionExpr {
    HIR_EXPR_HEADER;
    enum BuiltinKind to;
    struct HirExpr *arg;
};

struct HirAssignExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

// If "never" is equal to true, then the block contains an unconditional
// jump, either directly or transitively through 1 or more nested block
// expressions (not including blocks that only execute conditionally,
// such as an IfExpr or loop).
struct HirBlock {
    HIR_EXPR_HEADER;
    paw_Bool never : 1;
    struct HirStmtList *stmts;
    struct HirExpr *result;
};

struct HirReturnExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *expr;
};

// If "never" is equal to true, then both arms of the IfExpr contain
// unconditional jumps. This means that regardless of which arm is
// executed, control will jump before hitting the end of the expression.
struct HirIfExpr {
    HIR_EXPR_HEADER;
    paw_Bool never : 1;
    struct HirExpr *cond;
    struct HirExpr *then_arm;
    struct HirExpr *else_arm;
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
    paw_Bool never : 1;
    struct HirPat *pat;
    struct HirExpr *guard;
    struct HirExpr *result;
};

struct HirMatchExpr {
    HIR_EXPR_HEADER;
    paw_Bool never : 1;
    struct HirExpr *target;
    struct HirExprList *arms;
};

struct HirExpr {
    union {
        struct HirExprHeader hdr;
        struct HirSuffixedExpr suffix;
#define DEFINE_UNION(X) struct Hir##X X##_;
        HIR_EXPR_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static char const *kHirExprNames[] = {
#define DEFINE_NAME(X) "Hir" #X,
    HIR_EXPR_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                         \
    static inline paw_Bool HirIs##X(struct HirExpr const *node)  \
    {                                                            \
        return node->hdr.kind == kHir##X;                        \
    }                                                            \
    static inline struct Hir##X *HirGet##X(struct HirExpr *node) \
    {                                                            \
        paw_assert(HirIs##X(node));                              \
        return &node->X##_;                                      \
    }
HIR_EXPR_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirExpr *pawHir_new_expr(struct Hir *hir);

static struct HirExpr *pawHir_new_path_expr(struct Hir *hir, int line, struct HirPath *path)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->PathExpr_ = (struct HirPathExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirPathExpr,
        .path = path,
    };
    return e;
}

static struct HirExpr *pawHir_new_basic_lit(struct Hir *hir, int line, Value value, enum BuiltinKind code)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LiteralExpr_ = (struct HirLiteralExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirLiteralExpr,
        .lit_kind = kHirLitBasic,
        .basic.value = value,
        .basic.code = code,
    };
    return e;
}

static struct HirExpr *pawHir_new_container_lit(struct Hir *hir, int line, struct HirExprList *items, enum BuiltinKind code)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LiteralExpr_ = (struct HirLiteralExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirLiteralExpr,
        .lit_kind = kHirLitContainer,
        .cont.items = items,
        .cont.code = code,
    };
    return e;
}

static struct HirExpr *pawHir_new_tuple_lit(struct Hir *hir, int line, struct HirExprList *elems)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LiteralExpr_ = (struct HirLiteralExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirLiteralExpr,
        .lit_kind = kHirLitTuple,
        .tuple.elems = elems,
    };
    return e;
}

static struct HirExpr *pawHir_new_composite_lit(struct Hir *hir, int line, struct HirPath *path, struct HirExprList *items)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LiteralExpr_ = (struct HirLiteralExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirLiteralExpr,
        .lit_kind = kHirLitComposite,
        .comp.path = path,
        .comp.items = items,
    };
    return e;
}

static struct HirExpr *pawHir_new_closure_expr(struct Hir *hir, int line, struct HirDeclList *params, struct HirType *result, struct HirExpr *expr)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ClosureExpr_ = (struct HirClosureExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirClosureExpr,
        .params = params,
        .result = result,
        .expr = expr,
    };
    return e;
}

static struct HirExpr *pawHir_new_named_field_expr(struct Hir *hir, int line, String *name, struct HirExpr *value, int fid)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->FieldExpr_ = (struct HirFieldExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirFieldExpr,
        .name = name,
        .value = value,
        .fid = fid,
    };
    return e;
}

static struct HirExpr *pawHir_new_keyed_field_expr(struct Hir *hir, int line, struct HirExpr *key, struct HirExpr *value)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->FieldExpr_ = (struct HirFieldExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirFieldExpr,
        .key = key,
        .value = value,
        .fid = -1,
    };
    return e;
}

static struct HirExpr *pawHir_new_unop_expr(struct Hir *hir, int line, struct HirExpr *target, enum UnaryOp op)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->UnOpExpr_ = (struct HirUnOpExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirUnOpExpr,
        .target = target,
        .op = op,
    };
    return e;
}

static struct HirExpr *pawHir_new_binop_expr(struct Hir *hir, int line, struct HirExpr *lhs, struct HirExpr *rhs, enum BinaryOp op)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->BinOpExpr_ = (struct HirBinOpExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirBinOpExpr,
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
    };
    return e;
}

static struct HirExpr *pawHir_new_logical_expr(struct Hir *hir, int line, struct HirExpr *lhs, struct HirExpr *rhs, paw_Bool is_and)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LogicalExpr_ = (struct HirLogicalExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirLogicalExpr,
        .is_and = is_and,
        .lhs = lhs,
        .rhs = rhs,
    };
    return e;
}

static struct HirExpr *pawHir_new_chain_expr(struct Hir *hir, int line, struct HirExpr *target)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ChainExpr_ = (struct HirChainExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .target = target,
        .kind = kHirChainExpr,
    };
    return e;
}

static struct HirExpr *pawHir_new_index_selector(struct Hir *hir, int line, struct HirExpr *target, int index)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->Selector_ = (struct HirSelector){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirSelector,
        .target = target,
        .index = index,
        .is_index = PAW_TRUE,
    };
    return e;
}

static struct HirExpr *pawHir_new_name_selector(struct Hir *hir, int line, struct HirExpr *target, String *name)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->Selector_ = (struct HirSelector){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirSelector,
        .target = target,
        .name = name,
        .is_index = PAW_FALSE,
    };
    return e;
}

static struct HirExpr *pawHir_new_index_expr(struct Hir *hir, int line, struct HirExpr *target, struct HirExpr *first, struct HirExpr *second, paw_Bool is_slice)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->Index_ = (struct HirIndex){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirIndex,
        .target = target,
        .first = first,
        .second = second,
        .is_slice = is_slice,
    };
    return e;
}

static struct HirExpr *pawHir_new_conversion_expr(struct Hir *hir, int line, struct HirExpr *from, enum BuiltinKind to)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ConversionExpr_ = (struct HirConversionExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirConversionExpr,
        .arg = from,
        .to = to,
    };
    return e;
}

static struct HirExpr *pawHir_new_assign_expr(struct Hir *hir, int line, struct HirExpr *lhs, struct HirExpr *rhs)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->AssignExpr_ = (struct HirAssignExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirAssignExpr,
        .lhs = lhs,
        .rhs = rhs,
    };
    return e;
}

static struct HirExpr *pawHir_new_return_expr(struct Hir *hir, int line, struct HirExpr *expr)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->ReturnExpr_ = (struct HirReturnExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirReturnExpr,
        .expr = expr,
    };
    return e;
}

static struct HirExpr *pawHir_new_if_expr(struct Hir *hir, int line, struct HirExpr *cond, struct HirExpr *then_arm, struct HirExpr *else_arm, paw_Bool never)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->IfExpr_ = (struct HirIfExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirIfExpr,
        .cond = cond,
        .then_arm = then_arm,
        .else_arm = else_arm,
        .never = never,
    };
    return e;
}

static struct HirExpr *pawHir_new_call_expr(struct Hir *hir, int line, struct HirExpr *target, struct HirExprList *args)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->CallExpr_ = (struct HirCallExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirCallExpr,
        .target = target,
        .args = args,
    };
    return e;
}

static struct HirExpr *pawHir_new_match_arm(struct Hir *hir, int line, struct HirPat *pat, struct HirExpr *guard, struct HirExpr *result, paw_Bool never)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->MatchArm_ = (struct HirMatchArm){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirMatchArm,
        .never = never,
        .pat = pat,
        .guard = guard,
        .result = result,
    };
    return e;
}

static struct HirExpr *pawHir_new_match_expr(struct Hir *hir, int line, struct HirExpr *target, struct HirExprList *arms, paw_Bool never)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->MatchExpr_ = (struct HirMatchExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirMatchExpr,
        .never = never,
        .target = target,
        .arms = arms,
    };
    return e;
}

static struct HirExpr *pawHir_new_block(struct Hir *hir, int line, struct HirStmtList *stmts, struct HirExpr *result, paw_Bool never)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->Block_ = (struct HirBlock){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirBlock,
        .stmts = stmts,
        .result = result,
        .never = never,
    };
    return e;
}

static struct HirExpr *pawHir_new_loop_expr(struct Hir *hir, int line, struct HirExpr *block)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->LoopExpr_ = (struct HirLoopExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirLoopExpr,
        .block = block,
    };
    return e;
}

static struct HirExpr *pawHir_new_jump_expr(struct Hir *hir, int line, enum JumpKind jump_kind)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    e->JumpExpr_ = (struct HirJumpExpr){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirJumpExpr,
        .jump_kind = jump_kind,
    };
    return e;
}

enum HirStmtKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_STMT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_STMT_HEADER      \
    K_ALIGNAS_NODE int line; \
    HirId hid;               \
    enum HirStmtKind kind : 8
struct HirStmtHeader {
    HIR_STMT_HEADER;
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
#define DEFINE_UNION(X) struct Hir##X X##_;
        HIR_STMT_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static char const *kHirStmtNames[] = {
#define DEFINE_NAME(X) "Hir" #X,
    HIR_STMT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                         \
    static inline paw_Bool HirIs##X(struct HirStmt const *node)  \
    {                                                            \
        return node->hdr.kind == kHir##X;                        \
    }                                                            \
    static inline struct Hir##X *HirGet##X(struct HirStmt *node) \
    {                                                            \
        paw_assert(HirIs##X(node));                              \
        return &node->X##_;                                      \
    }
HIR_STMT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirStmt *pawHir_new_stmt(struct Hir *hir);

static struct HirStmt *pawHir_new_expr_stmt(struct Hir *hir, int line, struct HirExpr *expr)
{
    struct HirStmt *s = pawHir_new_stmt(hir);
    s->ExprStmt_ = (struct HirExprStmt){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirExprStmt,
        .expr = expr,
    };
    return s;
}

static struct HirStmt *pawHir_new_decl_stmt(struct Hir *hir, int line, struct HirDecl *decl)
{
    struct HirStmt *s = pawHir_new_stmt(hir);
    s->DeclStmt_ = (struct HirDeclStmt){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirDeclStmt,
        .decl = decl,
    };
    return s;
}

enum HirPatKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_PAT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_PAT_HEADER       \
    K_ALIGNAS_NODE int line; \
    HirId hid;               \
    enum HirPatKind kind : 8

struct HirPatHeader {
    HIR_PAT_HEADER;
};

struct HirOrPat {
    HIR_PAT_HEADER;
    struct HirPatList *pats;
};

// NOTE: only used for HirStructPat
struct HirFieldPat {
    HIR_PAT_HEADER;
    int index;
    String *name;
    struct HirPat *pat;
};

struct HirStructPat {
    HIR_PAT_HEADER;
    struct HirPath *path;
    struct HirPatList *fields; // [HirFieldPat]
};

struct HirVariantPat {
    HIR_PAT_HEADER;
    int index;
    struct HirPath *path;
    struct HirPatList *fields;
};

struct HirTuplePat {
    HIR_PAT_HEADER;
    struct HirPatList *elems;
};

struct HirPathPat {
    HIR_PAT_HEADER;
    struct HirPath *path;
};

struct HirBindingPat {
    HIR_PAT_HEADER;
    String *name;
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
#define DEFINE_UNION(X) struct Hir##X X##_;
        HIR_PAT_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static char const *kHirPatNames[] = {
#define DEFINE_NAME(X) "Hir" #X,
    HIR_PAT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                        \
    static inline paw_Bool HirIs##X(struct HirPat const *node)  \
    {                                                           \
        return node->hdr.kind == kHir##X;                       \
    }                                                           \
    static inline struct Hir##X *HirGet##X(struct HirPat *node) \
    {                                                           \
        paw_assert(HirIs##X(node));                             \
        return &node->X##_;                                     \
    }
HIR_PAT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirPat *pawHir_new_pat(struct Hir *hir);

static struct HirPat *pawHir_new_or_pat(struct Hir *hir, int line, struct HirPatList *pats)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->OrPat_ = (struct HirOrPat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirOrPat,
        .pats = pats,
    };
    return p;
}

static struct HirPat *pawHir_new_field_pat(struct Hir *hir, int line, String *name, struct HirPat *pat, int index)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->FieldPat_ = (struct HirFieldPat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirFieldPat,
        .name = name,
        .index = index,
        .pat = pat,
    };
    return p;
}

static struct HirPat *pawHir_new_struct_pat(struct Hir *hir, int line, struct HirPath *path, struct HirPatList *fields)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->StructPat_ = (struct HirStructPat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirStructPat,
        .path = path,
        .fields = fields,
    };
    return p;
}

static struct HirPat *pawHir_new_variant_pat(struct Hir *hir, int line, struct HirPath *path, struct HirPatList *fields, int index)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->VariantPat_ = (struct HirVariantPat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirVariantPat,
        .path = path,
        .fields = fields,
        .index = index,
    };
    return p;
}

static struct HirPat *pawHir_new_tuple_pat(struct Hir *hir, int line, struct HirPatList *elems)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->TuplePat_ = (struct HirTuplePat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirTuplePat,
        .elems = elems,
    };
    return p;
}

static struct HirPat *pawHir_new_path_pat(struct Hir *hir, int line, struct HirPath *path)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->PathPat_ = (struct HirPathPat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirPathPat,
        .path = path,
    };
    return p;
}

static struct HirPat *pawHir_new_binding_pat(struct Hir *hir, int line, String *name)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->BindingPat_ = (struct HirBindingPat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirBindingPat,
        .name = name,
    };
    return p;
}

static struct HirPat *pawHir_new_wildcard_pat(struct Hir *hir, int line)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->WildcardPat_ = (struct HirWildcardPat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirWildcardPat,
    };
    return p;
}

static struct HirPat *pawHir_new_literal_pat(struct Hir *hir, int line, struct HirExpr *expr)
{
    struct HirPat *p = pawHir_new_pat(hir);
    p->LiteralPat_ = (struct HirLiteralPat){
        .hid = pawHir_next_id(hir),
        .line = line,
        .kind = kHirLiteralPat,
        .expr = expr,
    };
    return p;
}

struct HirVisitor {
    struct Compiler *C;
    void *ud;
    int line;

    void (*VisitPath)(struct HirVisitor *V, struct HirPath *path);
    void (*VisitSegment)(struct HirVisitor *V, struct HirSegment *seg);

    paw_Bool (*VisitExpr)(struct HirVisitor *V, struct HirExpr *node);
    paw_Bool (*VisitStmt)(struct HirVisitor *V, struct HirStmt *node);
    paw_Bool (*VisitDecl)(struct HirVisitor *V, struct HirDecl *node);
    paw_Bool (*VisitType)(struct HirVisitor *V, struct HirType *node);
    paw_Bool (*VisitPat)(struct HirVisitor *V, struct HirPat *node);

    void (*PostVisitExpr)(struct HirVisitor *V, struct HirExpr *node);
    void (*PostVisitStmt)(struct HirVisitor *V, struct HirStmt *node);
    void (*PostVisitDecl)(struct HirVisitor *V, struct HirDecl *node);
    void (*PostVisitType)(struct HirVisitor *V, struct HirType *node);
    void (*PostVisitPat)(struct HirVisitor *V, struct HirPat *node);

#define DEFINE_CALLBACK(X)                                             \
    paw_Bool (*Visit##X)(struct HirVisitor * V, struct Hir##X * node); \
    void (*PostVisit##X)(struct HirVisitor * V, struct Hir##X * node);
    HIR_EXPR_LIST(DEFINE_CALLBACK)
    HIR_DECL_LIST(DEFINE_CALLBACK)
    HIR_STMT_LIST(DEFINE_CALLBACK)
    HIR_TYPE_LIST(DEFINE_CALLBACK)
    HIR_PAT_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawHir_visitor_init(struct HirVisitor *V, struct Compiler *C, void *ud);

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


struct Hir {
    struct HirImportList *imports;
    struct HirDeclList *items;
    struct Compiler *C;
    struct Pool *pool;
    String *name;
    paw_Env *P;
    int modno;
};

inline static HirId pawHir_next_id(struct Hir *hir)
{
    return (HirId){hir->C->hir_count++};
}

struct HirPath *pawHir_new_path(struct Compiler *C);

DEFINE_LIST(struct Hir, HirDeclList, struct HirDecl *)
DEFINE_LIST(struct Hir, HirExprList, struct HirExpr *)
DEFINE_LIST(struct Hir, HirStmtList, struct HirStmt *)
DEFINE_LIST(struct Hir, HirTypeList, struct HirType *)
DEFINE_LIST(struct Hir, HirPatList, struct HirPat *)
DEFINE_LIST(struct Hir, HirGenericList, struct HirGeneric *)
DEFINE_LIST(struct Hir, HirFieldList, struct HirField *)
DEFINE_LIST(struct Hir, HirParamList, struct HirParam *)
DEFINE_LIST(struct Hir, HirVariantList, struct HirVariant *)
DEFINE_LIST(struct Hir, HirSymtab, struct HirScope *)
DEFINE_LIST(struct Hir, HirScope, struct HirSymbol)
DEFINE_LIST(struct Hir, HirImportList, struct HirImport)
DEFINE_LIST(struct Hir, HirPath, struct HirSegment)
DEFINE_LIST(struct Hir, HirBoundList, struct HirGenericBound)

#define HIR_IS_POLY_FUNC(decl) (HirIsFuncDecl(decl) && HirGetFuncDecl(decl)->generics != NULL)
#define HIR_IS_POLY_ADT(decl) (HirIsAdtDecl(decl) && HirGetAdtDecl(decl)->generics != NULL)

#define HIR_PATH_RESULT(path) ((path)->data[(path)->count - 1].did)
#define HIR_TYPE_DID(type) (HirIsPathType(type) ? HIR_PATH_RESULT(HirGetPathType(type)->path) : HirGetFuncDef(type)->did)

struct Hir *pawHir_new(struct Compiler *C, String *name, int modno);
void pawHir_free(struct Hir *hir);

int pawHir_expand_bodies(struct Hir *hir);
void pawHir_define(struct ModuleInfo *m, struct HirDeclList *out, int *poffset);

struct HirDecl *pawHir_get_decl(struct Compiler *C, DeclId id);

#define HIR_TYPEOF(x) ((x)->hdr.type)
#define HIR_KINDOF(x) ((x)->hdr.kind)

// NOTE: HirFuncPtr is a prefix of HirFuncDef
#define HIR_FPTR(t) CHECK_EXP(HirIsFuncType(t), &(t)->fptr)

inline static struct HirSegment *HirPath_add(struct Hir *hir, struct HirPath *path, String *name,
                                             struct HirTypeList *args)
{
    struct HirSegment seg;
    pawHir_init_segment(hir, &seg, name, args, NO_DECL);
    HirPath_push(hir, path, seg);
    return &K_LIST_LAST(path);
}

paw_Bool pawHir_is_pub_decl(struct HirDecl *decl);
struct IrTypeList *pawHir_collect_decl_types(struct Compiler *C, struct HirDeclList *list);

enum TraitKind pawHir_kindof_trait(struct Compiler *C, struct HirTraitDecl *d);

// Return true if the expression can be evaluated at compile time, false otherwise
paw_Bool pawHir_check_const(struct Hir *hir, struct HirExpr *expr);

char const *pawHir_print_type(struct Compiler *C, struct HirType *type);
char const *pawHir_print_path(struct Compiler *C, struct HirPath *path);

char const *pawHir_dump(struct Hir *hir);

inline static paw_Uint hir_id_hash(struct Compiler *C, HirId hid)
{
    PAW_UNUSED(C);
    return hid.value;
}

inline static paw_Bool hir_id_equals(struct Compiler *C, HirId a, HirId b)
{
    PAW_UNUSED(C);
    return a.value == b.value;
}

DEFINE_MAP(struct Compiler, HirTypes, pawP_alloc, hir_id_hash, hir_id_equals, HirId, struct IrType *)
DEFINE_MAP_ITERATOR(HirTypes, HirId, struct IrType *)

#endif // PAW_HIR_H
