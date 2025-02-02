// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_HIR_H
#define PAW_HIR_H

#include "compile.h"

struct Compiler;
struct Resolver;

typedef struct HirId {
    unsigned short value;
} HirId;

      //  X(StructDecl) \
      //  X(EnumDecl)
#define HIR_DECL_LIST(X) \
        X(FieldDecl) \
        X(FuncDecl) \
        X(GenericDecl) \
        X(AdtDecl) \
        X(TypeDecl) \
        X(VarDecl) \
        X(ImplDecl) \
        X(VariantDecl)

#define HIR_EXPR_LIST(X) \
        X(LiteralExpr) \
        X(LogicalExpr) \
        X(PathExpr) \
        X(ChainExpr) \
        X(UnOpExpr) \
        X(BinOpExpr) \
        X(ClosureExpr) \
        X(ConversionExpr) \
        X(CallExpr) \
        X(Index) \
        X(Selector) \
        X(FieldExpr) \
        X(AssignExpr) \
        X(Block) \
        X(IfExpr) \
        X(LoopExpr) \
        X(JumpExpr) \
        X(ReturnExpr) \
        X(MatchArm) \
        X(MatchExpr)

#define HIR_TYPE_LIST(X) \
        X(FuncPtr) \
        X(TupleType) \
        X(PathType)

#define HIR_STMT_LIST(X) \
        X(ExprStmt) \
        X(DeclStmt)

#define HIR_PAT_LIST(X) \
        X(OrPat) \
        X(FieldPat) \
        X(StructPat) \
        X(VariantPat) \
        X(TuplePat) \
        X(PathPat) \
        X(BindingPat) \
        X(WildcardPat) \
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

static inline HirId pawHir_next_id(struct Hir *hir);

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

int pawHir_find_symbol(struct HirScope *scope, const String *name);
int pawHir_declare_symbol(struct Compiler *C, struct HirScope *scope, struct HirDecl *decl, String *name);
void pawHir_define_symbol(struct HirScope *scope, int index);

struct HirSegment {
    String *name;
    struct HirTypeList *types;
    DeclId did;
};

struct HirSegment *pawHir_segment_new(struct Compiler *C);

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


enum HirTypeKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_TYPE_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_TYPE_HEADER \
    K_ALIGNAS_NODE int line; \
    HirId hid; \
    enum HirTypeKind kind: 8
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

struct HirType {
    union {
        struct HirTypeHeader hdr;
#define DEFINE_UNION(X) struct Hir##X X##_;
        HIR_TYPE_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kHirTypeNames[] = {
#define DEFINE_NAME(X) "Hir"#X,
    HIR_TYPE_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool HirIs##X(const struct HirType *node) { \
        return node->hdr.kind == kHir##X; \
    } \
    static inline struct Hir##X *HirGet##X(struct HirType *node) { \
        paw_assert(HirIs##X(node)); \
        return &node->X##_; \
    }
    HIR_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirType *pawHir_new_type(struct Hir *hir);

static struct HirType *pawHir_new_path_type(struct Hir *hir, int line, struct HirPath *path)
{
    struct HirType *t = pawHir_new_type(hir);
    *t = (struct HirType){
        .PathType_.hid = pawHir_next_id(hir),
        .PathType_.line = line,
        .PathType_.kind = kHirPathType,
        .PathType_.path = path,
    };
    return t;
}

static struct HirType *pawHir_new_func_ptr(struct Hir *hir, int line, struct HirTypeList *params, struct HirType *result)
{
    struct HirType *t = pawHir_new_type(hir);
    *t = (struct HirType){
        .FuncPtr_.hid = pawHir_next_id(hir),
        .FuncPtr_.line = line,
        .FuncPtr_.kind = kHirFuncPtr,
        .FuncPtr_.params = params,
        .FuncPtr_.result = result,
    };
    return t;
}

static struct HirType *pawHir_new_tuple_type(struct Hir *hir, int line, struct HirTypeList *elems)
{
    struct HirType *t = pawHir_new_type(hir);
    *t = (struct HirType){
        .TupleType_.hid = pawHir_next_id(hir),
        .TupleType_.line = line,
        .TupleType_.kind = kHirTupleType,
        .TupleType_.elems = elems,
    };
    return t;
}


enum HirDeclKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_DECL_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_DECL_HEADER \
    K_ALIGNAS_NODE String *name; \
    int line; \
    HirId hid; \
    DeclId did; \
    enum HirDeclKind kind : 8
struct HirDeclHeader {
    HIR_DECL_HEADER;
};

struct HirVarDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_const : 1;
    struct HirExpr *init;
    struct HirType *tag;
};

// Node representing a type declaration
// Used for type aliases and builtin types.
struct HirTypeDecl {
    HIR_DECL_HEADER;
    struct HirType *rhs;
    struct HirDeclList *generics;
};

struct HirFuncDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_assoc : 1;
    enum FuncKind fn_kind : 6;
    struct IrType *self;
    struct HirDeclList *generics;
    struct HirDeclList *params;
    struct HirType *result;
    struct HirExpr *body;
};

// TODO: struct should be represented internally as an ADT with a single variant
struct HirAdtDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_struct : 1;
    struct HirDeclList *generics;
    struct HirDeclList *fields;
};

struct HirEnumDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirGenericList *generics;
    struct HirVariantList *variants;
};

struct HirStructDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirGenericList *generics;
    struct HirFieldList *fields;
};

struct HirVariantDecl {
    HIR_DECL_HEADER;
    int index;
    struct HirDeclList *fields;
};

// Represents a generic type parameter
struct HirGenericDecl {
    HIR_DECL_HEADER;
};

// HIR node representing a 'Field' production
struct HirFieldDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirType *tag;
};

// HIR node representing an 'impl' block
struct HirImplDecl {
    HIR_DECL_HEADER;
    struct HirPath *self;
    struct HirDecl *alias;
    struct HirDeclList *generics;
    struct HirDeclList *methods;
    struct IrTypeList *subst;
};

struct HirDecl {
    union {
        struct HirDeclHeader hdr;
#define DEFINE_UNION(X) struct Hir##X X##_;
        HIR_DECL_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kHirDeclNames[] = {
#define DEFINE_NAME(X) "Hir"#X,
    HIR_DECL_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool HirIs##X(const struct HirDecl *node) { \
        return node->hdr.kind == kHir##X; \
    } \
    static inline struct Hir##X *HirGet##X(struct HirDecl *node) { \
        paw_assert(HirIs##X(node)); \
        return &node->X##_; \
    }
    HIR_DECL_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirDecl *pawHir_new_decl(struct Hir *hir);
DeclId pawHir_register_decl(struct Hir *hir, struct HirDecl *decl);

static struct HirDecl *pawHir_new_var_decl(struct Hir *hir, int line, String *name, struct HirType *tag, struct HirExpr *init)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    *d = (struct HirDecl){
        .VarDecl_.hid = pawHir_next_id(hir),
        .VarDecl_.line = line,
        .VarDecl_.kind = kHirVarDecl,
        .VarDecl_.name = name,
        .VarDecl_.tag = tag,
        .VarDecl_.init = init,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_type_decl(struct Hir *hir, int line, String *name, struct HirDeclList *generics, struct HirType *rhs)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    *d = (struct HirDecl){
        .TypeDecl_.hid = pawHir_next_id(hir),
        .TypeDecl_.line = line,
        .TypeDecl_.kind = kHirTypeDecl,
        .TypeDecl_.name = name,
        .TypeDecl_.generics = generics,
        .TypeDecl_.rhs = rhs,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_func_decl(struct Hir *hir, int line, String *name, struct IrType *self, struct HirDeclList *generics, struct HirDeclList *params, struct HirType *result, struct HirExpr *body, enum FuncKind fn_kind, paw_Bool is_pub, paw_Bool is_assoc)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    *d = (struct HirDecl){
        .FuncDecl_.hid = pawHir_next_id(hir),
        .FuncDecl_.line = line,
        .FuncDecl_.kind = kHirFuncDecl,
        .FuncDecl_.name = name,
        .FuncDecl_.self = self,
        .FuncDecl_.generics = generics,
        .FuncDecl_.params = params,
        .FuncDecl_.result = result,
        .FuncDecl_.body = body,
        .FuncDecl_.fn_kind = fn_kind,
        .FuncDecl_.is_pub = is_pub,
        .FuncDecl_.is_assoc =is_assoc,
    };
    pawHir_register_decl(hir, d);
    return d;
}

//static struct HirDecl *pawHir_new_enum_decl(struct Hir *hir, int line, String *name, struct HirDeclList *generics, struct HirDeclList *variants, paw_Bool is_pub, paw_Bool is_struct)
//{
//    struct HirDecl *d = pawHir_new_decl(hir);
//    *d = (struct HirDecl){
//        .EnumDecl_.hid = pawHir_next_id(hir),
//        .EnumDecl_.line = line,
//        .EnumDecl_.kind = kHirEnumDecl,
//        .EnumDecl_.name = name,
//        .EnumDecl_.generics = generics,
//        .EnumDecl_.variants = variants,
//        .EnumDecl_.is_pub = is_pub,
//    };
//    pawHir_register_decl(hir, d);
//    return d;
//}
//
//static struct HirDecl *pawHir_new_struct_decl(struct Hir *hir, int line, String *name, struct HirDeclList *generics, struct HirDeclList *fields, paw_Bool is_pub, paw_Bool is_struct)
//{
//    struct HirDecl *d = pawHir_new_decl(hir);
//    *d = (struct HirDecl){
//        .StructDecl_.hid = pawHir_next_id(hir),
//        .StructDecl_.line = line,
//        .StructDecl_.kind = kHirStructDecl,
//        .StructDecl_.name = name,
//        .StructDecl_.generics = generics,
//        .StructDecl_.fields = fields,
//        .StructDecl_.is_pub = is_pub,
//    };
//    pawHir_register_decl(hir, d);
//    return d;
//}

static struct HirDecl *pawHir_new_adt_decl(struct Hir *hir, int line, String *name, struct HirDeclList *generics, struct HirDeclList *fields, paw_Bool is_pub, paw_Bool is_struct)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    *d = (struct HirDecl){
        .AdtDecl_.hid = pawHir_next_id(hir),
        .AdtDecl_.line = line,
        .AdtDecl_.kind = kHirAdtDecl,
        .AdtDecl_.name = name,
        .AdtDecl_.generics = generics,
        .AdtDecl_.fields = fields,
        .AdtDecl_.is_pub = is_pub,
        .AdtDecl_.is_struct = is_struct,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_variant_decl(struct Hir *hir, int line, String *name, struct HirDeclList *fields, int index)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    *d = (struct HirDecl){
        .VariantDecl_.hid = pawHir_next_id(hir),
        .VariantDecl_.line = line,
        .VariantDecl_.kind = kHirVariantDecl,
        .VariantDecl_.name = name,
        .VariantDecl_.fields = fields,
        .VariantDecl_.index = index,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_generic_decl(struct Hir *hir, int line, String *name)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    *d = (struct HirDecl){
        .GenericDecl_.hid = pawHir_next_id(hir),
        .GenericDecl_.line = line,
        .GenericDecl_.kind = kHirGenericDecl,
        .GenericDecl_.name = name,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_field_decl(struct Hir *hir, int line, String *name, struct HirType *tag, paw_Bool is_pub)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    *d = (struct HirDecl){
        .FieldDecl_.hid = pawHir_next_id(hir),
        .FieldDecl_.line = line,
        .FieldDecl_.kind = kHirFieldDecl,
        .FieldDecl_.name = name,
        .FieldDecl_.tag = tag,
        .FieldDecl_.is_pub = is_pub,
    };
    pawHir_register_decl(hir, d);
    return d;
}

static struct HirDecl *pawHir_new_impl_decl(struct Hir *hir, int line, String *name, struct HirPath *self, struct HirDecl *alias, struct HirDeclList *generics, struct HirDeclList *methods, struct IrTypeList *subst)
{
    struct HirDecl *d = pawHir_new_decl(hir);
    *d = (struct HirDecl){
        .ImplDecl_.hid = pawHir_next_id(hir),
        .ImplDecl_.line = line,
        .ImplDecl_.kind = kHirImplDecl,
        .ImplDecl_.name = name,
        .ImplDecl_.self = self,
        .ImplDecl_.alias = alias,
        .ImplDecl_.generics = generics,
        .ImplDecl_.methods = methods,
        .ImplDecl_.subst = subst,
    };
    pawHir_register_decl(hir, d);
    return d;
}


enum HirExprKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_EXPR_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_EXPR_HEADER \
    K_ALIGNAS_NODE int line; \
    HirId hid; \
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
    paw_Type t;
};

struct HirContainerLit {
    struct HirExprList *items;
    paw_Type code;
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
    HIR_EXPR_HEADER; \
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
    paw_Type to;
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

static const char *kHirExprNames[] = {
#define DEFINE_NAME(X) "Hir"#X,
    HIR_EXPR_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool HirIs##X(const struct HirExpr *node) { \
        return node->hdr.kind == kHir##X; \
    } \
    static inline struct Hir##X *HirGet##X(struct HirExpr *node) { \
        paw_assert(HirIs##X(node)); \
        return &node->X##_; \
    }
    HIR_EXPR_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirExpr *pawHir_new_expr(struct Hir *hir);

static struct HirExpr *pawHir_new_path_expr(struct Hir *hir, int line, struct HirPath *path)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .PathExpr_.hid = pawHir_next_id(hir),
        .PathExpr_.line = line,
        .PathExpr_.kind = kHirPathExpr,
        .PathExpr_.path = path,
    };
    return e;
}

static struct HirExpr *pawHir_new_basic_lit(struct Hir *hir, int line, Value value, enum BuiltinKind code)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .LiteralExpr_.hid = pawHir_next_id(hir),
        .LiteralExpr_.line = line,
        .LiteralExpr_.kind = kHirLiteralExpr,
        .LiteralExpr_.lit_kind = kHirLitBasic,
        .LiteralExpr_.basic.value = value,
        .LiteralExpr_.basic.t = code,
    };
    return e;
}

static struct HirExpr *pawHir_new_container_lit(struct Hir *hir, int line, struct HirExprList *items, enum BuiltinKind code)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .LiteralExpr_.hid = pawHir_next_id(hir),
        .LiteralExpr_.line = line,
        .LiteralExpr_.kind = kHirLiteralExpr,
        .LiteralExpr_.lit_kind = kHirLitContainer,
        .LiteralExpr_.cont.items = items,
        .LiteralExpr_.cont.code = code,
    };
    return e;
}

static struct HirExpr *pawHir_new_tuple_lit(struct Hir *hir, int line, struct HirExprList *elems)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .LiteralExpr_.hid = pawHir_next_id(hir),
        .LiteralExpr_.line = line,
        .LiteralExpr_.kind = kHirLiteralExpr,
        .LiteralExpr_.lit_kind = kHirLitTuple,
        .LiteralExpr_.tuple.elems = elems,
    };
    return e;
}

static struct HirExpr *pawHir_new_composite_lit(struct Hir *hir, int line, struct HirPath *path, struct HirExprList *items)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .LiteralExpr_.hid = pawHir_next_id(hir),
        .LiteralExpr_.line = line,
        .LiteralExpr_.kind = kHirLiteralExpr,
        .LiteralExpr_.lit_kind = kHirLitComposite,
        .LiteralExpr_.comp.path = path,
        .LiteralExpr_.comp.items = items,
    };
    return e;
}

static struct HirExpr *pawHir_new_closure_expr(struct Hir *hir, int line, struct HirDeclList *params, struct HirType *result, struct HirExpr *expr)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .ClosureExpr_.hid = pawHir_next_id(hir),
        .ClosureExpr_.line = line,
        .ClosureExpr_.kind = kHirClosureExpr,
        .ClosureExpr_.params = params,
        .ClosureExpr_.result = result,
        .ClosureExpr_.expr = expr,
    };
    return e;
}

static struct HirExpr *pawHir_new_named_field_expr(struct Hir *hir, int line, String *name, struct HirExpr *value, int fid)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .FieldExpr_.hid = pawHir_next_id(hir),
        .FieldExpr_.line = line,
        .FieldExpr_.kind = kHirFieldExpr,
        .FieldExpr_.name = name,
        .FieldExpr_.value = value,
        .FieldExpr_.fid = fid,
    };
    return e;
}

static struct HirExpr *pawHir_new_keyed_field_expr(struct Hir *hir, int line, struct HirExpr *key, struct HirExpr *value)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .FieldExpr_.hid = pawHir_next_id(hir),
        .FieldExpr_.line = line,
        .FieldExpr_.kind = kHirFieldExpr,
        .FieldExpr_.key = key,
        .FieldExpr_.value = value,
        .FieldExpr_.fid = -1,
    };
    return e;
}

static struct HirExpr *pawHir_new_unop_expr(struct Hir *hir, int line, struct HirExpr *target, enum UnaryOp op)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .UnOpExpr_.hid = pawHir_next_id(hir),
        .UnOpExpr_.line = line,
        .UnOpExpr_.kind = kHirUnOpExpr,
        .UnOpExpr_.target = target,
        .UnOpExpr_.op = op,
    };
    return e;
}

static struct HirExpr *pawHir_new_binop_expr(struct Hir *hir, int line, struct HirExpr *lhs, struct HirExpr *rhs, enum BinaryOp op)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .BinOpExpr_.hid = pawHir_next_id(hir),
        .BinOpExpr_.line = line,
        .BinOpExpr_.kind = kHirBinOpExpr,
        .BinOpExpr_.op = op,
        .BinOpExpr_.lhs = lhs,
        .BinOpExpr_.rhs = rhs,
    };
    return e;
}

static struct HirExpr *pawHir_new_logical_expr(struct Hir *hir, int line, struct HirExpr *lhs, struct HirExpr *rhs, paw_Bool is_and)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .LogicalExpr_.hid = pawHir_next_id(hir),
        .LogicalExpr_.line = line,
        .LogicalExpr_.kind = kHirLogicalExpr,
        .LogicalExpr_.is_and = is_and,
        .LogicalExpr_.lhs = lhs,
        .LogicalExpr_.rhs = rhs,
    };
    return e;
}

static struct HirExpr *pawHir_new_chain_expr(struct Hir *hir, int line, struct HirExpr *target)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .ChainExpr_.hid = pawHir_next_id(hir),
        .ChainExpr_.line = line,
        .ChainExpr_.target = target,
        .ChainExpr_.kind = kHirChainExpr,
    };
    return e;
}

static struct HirExpr *pawHir_new_index_selector(struct Hir *hir, int line, struct HirExpr *target, int index)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .Selector_.hid = pawHir_next_id(hir),
        .Selector_.line = line,
        .Selector_.kind = kHirSelector,
        .Selector_.target = target,
        .Selector_.index = index,
        .Selector_.is_index = PAW_TRUE,
    };
    return e;
}

static struct HirExpr *pawHir_new_name_selector(struct Hir *hir, int line, struct HirExpr *target, String *name)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .Selector_.hid = pawHir_next_id(hir),
        .Selector_.line = line,
        .Selector_.kind = kHirSelector,
        .Selector_.target = target,
        .Selector_.name = name,
        .Selector_.is_index = PAW_FALSE,
    };
    return e;
}

static struct HirExpr *pawHir_new_index_expr(struct Hir *hir, int line, struct HirExpr *target, struct HirExpr *first, struct HirExpr *second, paw_Bool is_slice)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .Index_.hid = pawHir_next_id(hir),
        .Index_.line = line,
        .Index_.kind = kHirIndex,
        .Index_.target = target,
        .Index_.first = first,
        .Index_.second = second,
        .Index_.is_slice = is_slice,
    };
    return e;
}

static struct HirExpr *pawHir_new_conversion_expr(struct Hir *hir, int line, struct HirExpr *from, paw_Type to)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .ConversionExpr_.hid = pawHir_next_id(hir),
        .ConversionExpr_.line = line,
        .ConversionExpr_.kind = kHirConversionExpr,
        .ConversionExpr_.arg = from,
        .ConversionExpr_.to = to,
    };
    return e;
}

static struct HirExpr *pawHir_new_assign_expr(struct Hir *hir, int line, struct HirExpr *lhs, struct HirExpr *rhs)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .AssignExpr_.hid = pawHir_next_id(hir),
        .AssignExpr_.line = line,
        .AssignExpr_.kind = kHirAssignExpr,
        .AssignExpr_.lhs = lhs,
        .AssignExpr_.rhs = rhs,
    };
    return e;
}

static struct HirExpr *pawHir_new_return_expr(struct Hir *hir, int line, struct HirExpr *expr)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .ReturnExpr_.hid = pawHir_next_id(hir),
        .ReturnExpr_.line = line,
        .ReturnExpr_.kind = kHirReturnExpr,
        .ReturnExpr_.expr = expr,
    };
    return e;
}

static struct HirExpr *pawHir_new_if_expr(struct Hir *hir, int line, struct HirExpr *cond, struct HirExpr *then_arm, struct HirExpr *else_arm, paw_Bool never)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .IfExpr_.hid = pawHir_next_id(hir),
        .IfExpr_.line = line,
        .IfExpr_.kind = kHirIfExpr,
        .IfExpr_.cond = cond,
        .IfExpr_.then_arm = then_arm,
        .IfExpr_.else_arm = else_arm,
        .IfExpr_.never = never,
    };
    return e;
}

static struct HirExpr *pawHir_new_call_expr(struct Hir *hir, int line, struct HirExpr *target, struct HirExprList *args)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .CallExpr_.hid = pawHir_next_id(hir),
        .CallExpr_.line = line,
        .CallExpr_.kind = kHirCallExpr,
        .CallExpr_.target = target,
        .CallExpr_.args = args,
    };
    return e;
}

static struct HirExpr *pawHir_new_match_arm(struct Hir *hir, int line, struct HirPat *pat, struct HirExpr *guard, struct HirExpr *result, paw_Bool never)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .MatchArm_.hid = pawHir_next_id(hir),
        .MatchArm_.line = line,
        .MatchArm_.kind = kHirMatchArm,
        .MatchArm_.never = never,
        .MatchArm_.pat = pat,
        .MatchArm_.guard = guard,
        .MatchArm_.result = result,
    };
    return e;
}

static struct HirExpr *pawHir_new_match_expr(struct Hir *hir, int line, struct HirExpr *target, struct HirExprList *arms, paw_Bool never)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .MatchExpr_.hid = pawHir_next_id(hir),
        .MatchExpr_.line = line,
        .MatchExpr_.kind = kHirMatchExpr,
        .MatchExpr_.never = never,
        .MatchExpr_.target = target,
        .MatchExpr_.arms = arms,
    };
    return e;
}

static struct HirExpr *pawHir_new_block(struct Hir *hir, int line, struct HirStmtList *stmts, struct HirExpr *result, paw_Bool never)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .Block_.hid = pawHir_next_id(hir),
        .Block_.line = line,
        .Block_.kind = kHirBlock,
        .Block_.stmts = stmts,
        .Block_.result = result,
        .Block_.never = never,
    };
    return e;
}

static struct HirExpr *pawHir_new_loop_expr(struct Hir *hir, int line, struct HirExpr *block)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .LoopExpr_.hid = pawHir_next_id(hir),
        .LoopExpr_.line = line,
        .LoopExpr_.kind = kHirLoopExpr,
        .LoopExpr_.block = block,
    };
    return e;
}

static struct HirExpr *pawHir_new_jump_expr(struct Hir *hir, int line, enum JumpKind jump_kind)
{
    struct HirExpr *e = pawHir_new_expr(hir);
    *e = (struct HirExpr){
        .JumpExpr_.hid = pawHir_next_id(hir),
        .JumpExpr_.line = line,
        .JumpExpr_.kind = kHirJumpExpr,
        .JumpExpr_.jump_kind = jump_kind,
    };
    return e;
}


enum HirStmtKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_STMT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_STMT_HEADER \
    K_ALIGNAS_NODE int line; \
    HirId hid; \
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

static const char *kHirStmtNames[] = {
#define DEFINE_NAME(X) "Hir"#X,
    HIR_STMT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool HirIs##X(const struct HirStmt *node) { \
        return node->hdr.kind == kHir##X; \
    } \
    static inline struct Hir##X *HirGet##X(struct HirStmt *node) { \
        paw_assert(HirIs##X(node)); \
        return &node->X##_; \
    }
    HIR_STMT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirStmt *pawHir_new_stmt(struct Hir *hir);

static struct HirStmt *pawHir_new_expr_stmt(struct Hir *hir, int line, struct HirExpr *expr)
{
    struct HirStmt *s = pawHir_new_stmt(hir);
    *s = (struct HirStmt){
        .ExprStmt_.hid = pawHir_next_id(hir),
        .ExprStmt_.line = line,
        .ExprStmt_.kind = kHirExprStmt,
        .ExprStmt_.expr = expr,
    };
    return s;
}

static struct HirStmt *pawHir_new_decl_stmt(struct Hir *hir, int line, struct HirDecl *decl)
{
    struct HirStmt *s = pawHir_new_stmt(hir);
    *s = (struct HirStmt){
        .DeclStmt_.hid = pawHir_next_id(hir),
        .DeclStmt_.line = line,
        .DeclStmt_.kind = kHirDeclStmt,
        .DeclStmt_.decl = decl,
    };
    return s;
}


enum HirPatKind {
#define DEFINE_ENUM(X) kHir##X,
    HIR_PAT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_PAT_HEADER \
    K_ALIGNAS_NODE int line; \
    HirId hid; \
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

static const char *kHirPatNames[] = {
#define DEFINE_NAME(X) "Hir"#X,
        HIR_PAT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool HirIs##X(const struct HirPat *node) { \
        return node->hdr.kind == kHir##X; \
    } \
    static inline struct Hir##X *HirGet##X(struct HirPat *node) { \
        paw_assert(HirIs##X(node)); \
        return &node->X##_; \
    }
    HIR_PAT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct HirPat *pawHir_new_pat(struct Hir *hir);

static struct HirPat *pawHir_new_or_pat(struct Hir *hir, int line, struct HirPatList *pats)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .OrPat_.hid = pawHir_next_id(hir),
        .OrPat_.line = line,
        .OrPat_.kind = kHirOrPat,
        .OrPat_.pats = pats,
    };
    return p;
}

static struct HirPat *pawHir_new_field_pat(struct Hir *hir, int line, String *name, struct HirPat *pat, int index)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .FieldPat_.hid = pawHir_next_id(hir),
        .FieldPat_.line = line,
        .FieldPat_.kind = kHirFieldPat,
        .FieldPat_.name = name,
        .FieldPat_.index = index,
        .FieldPat_.pat = pat,
    };
    return p;
}

static struct HirPat *pawHir_new_struct_pat(struct Hir *hir, int line, struct HirPath *path, struct HirPatList *fields)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .StructPat_.hid = pawHir_next_id(hir),
        .StructPat_.line = line,
        .StructPat_.kind = kHirStructPat,
        .StructPat_.path = path,
        .StructPat_.fields = fields,
    };
    return p;
}

static struct HirPat *pawHir_new_variant_pat(struct Hir *hir, int line, struct HirPath *path, struct HirPatList *fields, int index)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .VariantPat_.hid = pawHir_next_id(hir),
        .VariantPat_.line = line,
        .VariantPat_.kind = kHirVariantPat,
        .VariantPat_.path = path,
        .VariantPat_.fields = fields,
        .VariantPat_.index = index,
    };
    return p;
}

static struct HirPat *pawHir_new_tuple_pat(struct Hir *hir, int line, struct HirPatList *elems)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .TuplePat_.hid = pawHir_next_id(hir),
        .TuplePat_.line = line,
        .TuplePat_.kind = kHirTuplePat,
        .TuplePat_.elems = elems,
    };
    return p;
}

static struct HirPat *pawHir_new_path_pat(struct Hir *hir, int line, struct HirPath *path)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .PathPat_.hid = pawHir_next_id(hir),
        .PathPat_.line = line,
        .PathPat_.kind = kHirPathPat,
        .PathPat_.path = path,
    };
    return p;
}

static struct HirPat *pawHir_new_binding_pat(struct Hir *hir, int line, String *name)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .BindingPat_.hid = pawHir_next_id(hir),
        .BindingPat_.line = line,
        .BindingPat_.kind = kHirBindingPat,
        .BindingPat_.name = name,
    };
    return p;
}

static struct HirPat *pawHir_new_wildcard_pat(struct Hir *hir, int line)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .WildcardPat_.hid = pawHir_next_id(hir),
        .WildcardPat_.line = line,
        .WildcardPat_.kind = kHirWildcardPat,
    };
    return p;
}

static struct HirPat *pawHir_new_literal_pat(struct Hir *hir, int line, struct HirExpr *expr)
{
    struct HirPat *p = pawHir_new_pat(hir);
    *p = (struct HirPat){
        .LiteralPat_.hid = pawHir_next_id(hir),
        .LiteralPat_.line = line,
        .LiteralPat_.kind = kHirLiteralPat,
        .LiteralPat_.expr = expr,
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

#define DEFINE_CALLBACK(X) paw_Bool (*Visit##X)(struct HirVisitor *V, struct Hir##X *node); \
                           void (*PostVisit##X)(struct HirVisitor *V, struct Hir##X *node);
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

static inline HirId pawHir_next_id(struct Hir *hir)
{
    return (HirId){hir->C->hir_count++};
}

struct HirPath *pawHir_new_path(struct Compiler *C);

DEFINE_LIST(struct Compiler, pawHir_decl_list_, HirDeclList, struct HirDecl *)
DEFINE_LIST(struct Compiler, pawHir_expr_list_, HirExprList, struct HirExpr *)
DEFINE_LIST(struct Compiler, pawHir_stmt_list_, HirStmtList, struct HirStmt *)
DEFINE_LIST(struct Compiler, pawHir_type_list_, HirTypeList, struct HirType *)
DEFINE_LIST(struct Compiler, pawHir_pat_list_, HirPatList, struct HirPat *)
DEFINE_LIST(struct Compiler, pawHir_generic_list_, HirGenericList, struct HirGeneric *)
DEFINE_LIST(struct Compiler, pawHir_field_list_, HirFieldList, struct HirField *)
DEFINE_LIST(struct Compiler, pawHir_param_list_, HirParamList, struct HirParam *)
DEFINE_LIST(struct Compiler, pawHir_variant_list_, HirVariantList, struct HirVariant *)
DEFINE_LIST(struct Compiler, pawHir_symtab_, HirSymtab, struct HirScope *)
DEFINE_LIST(struct Compiler, pawHir_scope_, HirScope, struct HirSymbol)
DEFINE_LIST(struct Compiler, pawHir_import_list_, HirImportList, struct HirImport)
DEFINE_LIST(struct Compiler, pawHir_path_, HirPath, struct HirSegment)

#define HIR_IS_UNIT_T(x) (HirIsAdt(x) && hir_adt_did(x) == PAW_TUNIT)
#define HIR_IS_BASIC_T(x) (HirIsAdt(x) && hir_adt_did(x) <= PAW_TSTR)

#define HIR_IS_POLY_FUNC(decl) (HirIsFuncDecl(decl) && HirGetFuncDecl(decl)->generics != NULL)
#define HIR_IS_POLY_ADT(decl) (HirIsAdtDecl(decl) && HirGetAdtDecl(decl)->generics != NULL)
#define HIR_IS_POLY_IMPL(decl) (HirIsImplDecl(decl) && HirGetImplDecl(decl)->generics != NULL)

#define HIR_PATH_RESULT(path) ((path)->data[(path)->count - 1].did)
#define HIR_TYPE_DID(type) (HirIsPathType(type) ? HIR_PATH_RESULT(HirGetPathType(type)->path) : \
        HirGetFuncDef(type)->did)

struct Hir *pawHir_new(struct Compiler *C, String *name, int modno);
void pawHir_free(struct Hir *hir);

int pawHir_expand_bodies(struct Hir *hir);
void pawHir_define(struct ModuleInfo *m, struct HirDeclList *out, int *poffset);

DeclId pawHir_add_decl(struct Compiler *C, struct HirDecl *decl, int modno);
struct HirDecl *pawHir_get_decl(struct Compiler *C, DeclId id);

#define HIR_TYPEOF(x) ((x)->hdr.type)
#define HIR_KINDOF(x) ((x)->hdr.kind)

// NOTE: HirFuncPtr is a prefix of HirFuncDef
#define HIR_FPTR(t) CHECK_EXP(HirIsFuncType(t), &(t)->fptr)

static inline struct HirSegment *pawHir_path_add(struct Compiler *C, struct HirPath *path, String *name,
                                                 struct HirTypeList *args)
{
    K_LIST_PUSH(C, path, ((struct HirSegment){
                .did = NO_DECL,
                .types = args,
                .name = name,
            }));
    return &K_LIST_LAST(path);
}

struct IrTypeList *pawHir_collect_decl_types(struct Compiler *C, struct HirDeclList *list);
struct IrTypeList *pawHir_collect_expr_types(struct Compiler *C, struct HirExprList *list);

const char *pawHir_print_type(struct Compiler *C, struct HirType *type);
const char *pawHir_print_path(struct Compiler *C, struct HirPath *path);

void pawHir_dump(struct Hir *hir);
void pawHir_dump_path(struct Compiler *C, struct HirPath *path);
void pawHir_dump_decls(struct Compiler *C, struct HirDeclList *decls);

#endif // PAW_HIR_H
