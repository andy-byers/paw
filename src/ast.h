// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// This module contains the abstract syntax tree (AST) for Paw. An AST is created
// for a given module during parsing, and is transformed into a HIR tree during type
// checking.

#ifndef PAW_AST_H
#define PAW_AST_H

#include "compile.h"

     //   X(StructDecl) \
     //   X(EnumDecl)
#define AST_DECL_LIST(X) \
        X(FieldDecl) \
        X(FuncDecl) \
        X(GenericDecl) \
        X(AdtDecl) \
        X(TypeDecl) \
        X(VarDecl) \
        X(ImplDecl) \
        X(UseDecl) \
        X(VariantDecl)

#define AST_TYPE_LIST(X) \
        X(PathType) \
        X(TupleType) \
        X(ContainerType) \
        X(FuncType)

#define AST_EXPR_LIST(X) \
        X(ParenExpr) \
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
        X(ForExpr) \
        X(WhileExpr) \
        X(JumpExpr) \
        X(ReturnExpr) \
        X(MatchArm) \
        X(MatchExpr)

#define AST_STMT_LIST(X) \
        X(ExprStmt) \
        X(DeclStmt)

#define AST_PAT_LIST(X) \
        X(OrPat) \
        X(FieldPat) \
        X(StructPat) \
        X(VariantPat) \
        X(TuplePat) \
        X(PathPat) \
        X(WildcardPat) \
        X(LiteralPat)

struct AstSegment {
    String *name;
    struct AstTypeList *types;
};

struct AstVariant {
    int discr;
    String *name;
    struct AstFieldList *fields;
};

struct AstGeneric {
    String *name;
};

struct AstParam {
    String *name;
    struct AstType *tag;
};

struct AstField {
    paw_Bool is_pub : 1;
    String *name;
    struct AstType *tag;
};

static inline void pawAst_init_variant(struct Ast *ast, struct AstVariant *result, int discr, String *name, struct AstFieldList *fields)
{
    *result = (struct AstVariant){
        .discr = discr,
        .name = name,
        .fields = fields,
    };
}

static inline void pawAst_init_generic(struct Ast *ast, struct AstGeneric *result, String *name)
{
    *result = (struct AstGeneric){
        .name = name,
    };
}

static inline void pawAst_init_param(struct Ast *ast, struct AstParam *result, String *name, struct AstType *tag)
{
    *result = (struct AstParam){
        .name = name,
        .tag = tag,
    };
}

static inline void pawAst_init_field(struct Ast *ast, struct AstField *result, String *name, struct AstType *tag, paw_Bool is_pub)
{
    *result = (struct AstField){
        .is_pub = is_pub,
        .name = name,
        .tag = tag,
    };
}


enum AstDeclKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_DECL_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_DECL_HEADER \
    K_ALIGNAS_NODE String *name; \
    int line; \
    enum AstDeclKind kind : 8

struct AstDeclHeader {
    AST_DECL_HEADER;
};

struct AstVarDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_const : 1;
    struct AstType *tag;
    struct AstExpr *init;
};

struct AstTypeDecl {
    AST_DECL_HEADER;
    struct AstType *rhs;
    struct AstDeclList *generics;
};

struct AstFuncDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    enum FuncKind fn_kind : 7;
    struct AstDecl *receiver;
    struct AstDeclList *generics;
    struct AstDeclList *params;
    struct AstType *result;
    struct AstExpr *body;
};

struct AstAdtDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_struct : 1;
    struct AstDeclList *generics;
    struct AstDeclList *fields;
};

struct AstUseDecl {
    AST_DECL_HEADER;
    paw_Bool has_star : 1;
    int modno;
    String *item;
    String *as;
};

struct AstEnumDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct AstDeclList *generics;
    struct AstDeclList *variants;
};

struct AstStructDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct AstDeclList *generics;
    struct AstDeclList *fields;
};

struct AstVariantDecl {
    AST_DECL_HEADER;
    struct AstDeclList *fields;
    int index;
};

struct AstGenericDecl {
    AST_DECL_HEADER;
};

struct AstFieldDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct AstType *tag;
};

struct AstImplDecl {
    AST_DECL_HEADER;
    struct AstPath *self;
    struct AstDeclList *generics;
    struct AstDeclList *methods;
};

struct AstDecl {
    union {
        struct AstDeclHeader hdr;
#define DEFINE_UNION(X) struct Ast##X X##_;
        AST_DECL_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kAstDeclNames[] = {
#define DEFINE_NAME(X) "Ast"#X,
        AST_DECL_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool AstIs##X(const struct AstDecl *node) { \
        return node->hdr.kind == kAst##X; \
    } \
    static inline struct Ast##X *AstGet##X(struct AstDecl *node) { \
        paw_assert(AstIs##X(node)); \
        return &node->X##_; \
    }
    AST_DECL_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstDecl *pawAst_new_decl(struct Ast *ast);

static struct AstDecl *pawAst_new_field_decl(struct Ast *ast, int line, String *name, struct AstType *tag, paw_Bool is_pub)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .FieldDecl_.line = line,
        .FieldDecl_.kind = kAstFieldDecl,
        .FieldDecl_.is_pub = is_pub,
        .FieldDecl_.name = name,
        .FieldDecl_.tag = tag,
    };
    return d;
}

static inline struct AstDecl *pawAst_new_func_decl(struct Ast *ast, int line, enum FuncKind fn_kind, String *name, struct AstDeclList *generics, struct AstDeclList *params, struct AstDecl *receiver, struct AstType *result, struct AstExpr *body, paw_Bool is_pub)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .FuncDecl_.line = line,
        .FuncDecl_.kind = kAstFuncDecl,
        .FuncDecl_.fn_kind = fn_kind,
        .FuncDecl_.name = name,
        .FuncDecl_.is_pub = is_pub,
        .FuncDecl_.generics = generics,
        .FuncDecl_.params = params,
        .FuncDecl_.receiver = receiver,
        .FuncDecl_.result = result,
        .FuncDecl_.body = body,
    };
    return d;
}

static inline struct AstDecl *pawAst_new_generic_decl(struct Ast *ast, int line, String *name)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .GenericDecl_.line = line,
        .GenericDecl_.kind = kAstGenericDecl,
        .GenericDecl_.name = name,
    };
    return d;
}

static inline struct AstDecl *pawAst_new_adt_decl(struct Ast *ast, int line, String *name, struct AstDeclList *generics, struct AstDeclList *fields, paw_Bool is_pub, paw_Bool is_struct)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .AdtDecl_.line = line,
        .AdtDecl_.kind = kAstAdtDecl,
        .AdtDecl_.name = name,
        .AdtDecl_.is_pub = is_pub,
        .AdtDecl_.is_struct = is_struct,
        .AdtDecl_.generics = generics,
        .AdtDecl_.fields = fields,
    };
    return d;
}

static inline struct AstDecl *pawAst_new_type_decl(struct Ast *ast, int line, String *name, struct AstDeclList *generics, struct AstType *rhs)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .TypeDecl_.line = line,
        .TypeDecl_.kind = kAstTypeDecl,
        .TypeDecl_.name = name,
        .TypeDecl_.generics = generics,
        .TypeDecl_.rhs = rhs,
    };
    return d;
}

static inline struct AstDecl *pawAst_new_var_decl(struct Ast *ast, int line, String *name, struct AstType *tag, struct AstExpr *init)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .VarDecl_.line = line,
        .VarDecl_.kind = kAstVarDecl,
        .VarDecl_.name = name,
        .VarDecl_.tag = tag,
        .VarDecl_.init = init,
    };
    return d;
}

static inline struct AstDecl *pawAst_new_impl_decl(struct Ast *ast, int line, String *name, struct AstPath *self, struct AstDeclList *generics, struct AstDeclList *methods)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .ImplDecl_.line = line,
        .ImplDecl_.kind = kAstImplDecl,
        .ImplDecl_.name = name,
        .ImplDecl_.self = self,
        .ImplDecl_.generics = generics,
        .ImplDecl_.methods = methods,

    };
    return d;
}

static inline struct AstDecl *pawAst_new_use_decl(struct Ast *ast, int line, String *name, paw_Bool has_star, String *item, String *as)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .UseDecl_.line = line,
        .UseDecl_.kind = kAstUseDecl,
        .UseDecl_.name = name,
        .UseDecl_.has_star = has_star,
        .UseDecl_.item = item,
        .UseDecl_.as = as,
    };
    return d;
}

static inline struct AstDecl *pawAst_new_variant_decl(struct Ast *ast, int line, String *name, struct AstDeclList *fields, int index)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    *d = (struct AstDecl){
        .VariantDecl_.line = line,
        .VariantDecl_.kind = kAstVariantDecl,
        .VariantDecl_.name = name,
        .VariantDecl_.fields = fields,
        .VariantDecl_.index = index,
    };
    return d;
}


enum AstTypeKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_TYPE_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_TYPE_HEADER \
    K_ALIGNAS_NODE int line; \
    enum AstTypeKind kind : 8

struct AstTypeHeader {
    AST_TYPE_HEADER;
};

struct AstPathType {
    AST_TYPE_HEADER;
    struct AstPath *path;
};

struct AstTupleType {
    AST_TYPE_HEADER;
    struct AstTypeList *types;
};

struct AstContainerType {
    AST_TYPE_HEADER;
    struct AstType *first;
    struct AstType *second;
};

struct AstFuncType {
    AST_TYPE_HEADER;
    struct AstType *result;
    struct AstTypeList *params;
};

struct AstType {
    union {
        struct AstTypeHeader hdr;
#define DEFINE_UNION(X) struct Ast##X X##_;
        AST_TYPE_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kAstTypeNames[] = {
#define DEFINE_NAME(X) "Ast"#X,
        AST_TYPE_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool AstIs##X(const struct AstType *node) { \
        return node->hdr.kind == kAst##X; \
    } \
    static inline struct Ast##X *AstGet##X(struct AstType *node) { \
        paw_assert(AstIs##X(node)); \
        return &node->X##_; \
    }
    AST_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstType *pawAst_new_type(struct Ast *ast);

static inline struct AstType *pawAst_new_path_type(struct Ast *ast, int line, struct AstPath *path)
{
    struct AstType *t = pawAst_new_type(ast);
    *t = (struct AstType){
        .PathType_.line = line,
        .PathType_.kind = kAstPathType,
        .PathType_.path = path,
    };
    return t;
}

static inline struct AstType *pawAst_new_tuple_type(struct Ast *ast, int line, struct AstTypeList *types)
{
    struct AstType *t = pawAst_new_type(ast);
    *t = (struct AstType){
        .TupleType_.line = line,
        .TupleType_.kind = kAstTupleType,
        .TupleType_.types = types,
    };
    return t;
}

static inline struct AstType *pawAst_new_func_type(struct Ast *ast, int line, struct AstTypeList *params, struct AstType *result)
{
    struct AstType *t = pawAst_new_type(ast);
    *t = (struct AstType){
        .FuncType_.line = line,
        .FuncType_.kind = kAstFuncType,
        .FuncType_.params = params,
        .FuncType_.result = result,
    };
    return t;
}

static inline struct AstType *pawAst_new_container_type(struct Ast *ast, int line, struct AstType *first, struct AstType *second)
{
    struct AstType *t = pawAst_new_type(ast);
    *t = (struct AstType){
        .ContainerType_.line = line,
        .ContainerType_.kind = kAstContainerType,
        .ContainerType_.first = first,
        .ContainerType_.second = second,
    };
    return t;
}


enum AstExprKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_EXPR_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_EXPR_HEADER \
    K_ALIGNAS_NODE int line; \
    enum AstExprKind kind : 8

struct AstExprHeader {
    AST_EXPR_HEADER;
};

struct AstPathExpr {
    AST_EXPR_HEADER;
    struct AstPath *path;
};

struct AstParenExpr {
    AST_EXPR_HEADER;
    struct AstExpr *expr;
};

struct AstLiteralExpr {
    AST_EXPR_HEADER;
    enum AstLitKind {
        kAstBasicLit,
        kAstCompositeLit,
        kAstContainerLit,
        kAstTupleLit,
    } lit_kind;

    union {
        struct AstBasicLit {
            Value value;
            paw_Type code;
        } basic;

        struct AstCompositeLit {
            struct AstPath *path;
            struct AstExprList *items;
        } comp;

        struct AstContainerLit {
            struct AstExprList *items;
            paw_Type code;
        } cont;

        struct AstTupleLit {
            struct AstExprList *elems;
        } tuple;
    };
};

struct AstClosureExpr {
    AST_EXPR_HEADER;
    struct AstDeclList *params;
    struct AstType *result;
    struct AstExpr *expr;
};

struct AstFieldExpr {
    AST_EXPR_HEADER;
    int fid;
    union {
        struct AstExpr *key;
        String *name;
    };
    struct AstExpr *value;
};

struct AstUnOpExpr {
    AST_EXPR_HEADER;
    enum UnaryOp op : 8;
    struct AstExpr *target;
};

struct AstBinOpExpr {
    AST_EXPR_HEADER;
    enum BinaryOp op : 8;
    struct AstExpr *lhs;
    struct AstExpr *rhs;
};

struct AstLogicalExpr {
    AST_EXPR_HEADER;
    paw_Bool is_and : 1;
    struct AstExpr *lhs;
    struct AstExpr *rhs;
};

struct AstSuffixedExpr {
    AST_EXPR_HEADER;
    struct AstExpr *target;
};

struct AstChainExpr {
    AST_EXPR_HEADER;
    struct AstExpr *target;
};

struct AstCallExpr {
    AST_EXPR_HEADER;
    struct AstExpr *target;
    struct AstExprList *args;
};

struct AstSelector {
    AST_EXPR_HEADER;
    paw_Bool is_index : 1;
    struct AstExpr *target;
    union {
        String *name;
        int index;
    };
};

struct AstIndex {
    AST_EXPR_HEADER;
    paw_Bool is_slice : 1;
    struct AstExpr *target;
    struct AstExpr *first;
    struct AstExpr *second;
};

struct AstConversionExpr {
    AST_EXPR_HEADER;
    paw_Type to;
    struct AstExpr *arg;
};

struct AstAssignExpr {
    AST_EXPR_HEADER;
    struct AstExpr *lhs;
    struct AstExpr *rhs;
};

struct AstBlock {
    AST_EXPR_HEADER;
    struct AstStmtList *stmts;
    struct AstExpr *result;
};

struct AstReturnExpr {
    AST_EXPR_HEADER;
    struct AstExpr *expr;
};

struct AstIfExpr {
    AST_EXPR_HEADER;
    struct AstExpr *cond;
    struct AstExpr *then_arm;
    struct AstExpr *else_arm;
};

struct AstWhileExpr {
    AST_EXPR_HEADER;
    struct AstExpr *cond;
    struct AstExpr *block;
};

struct AstJumpExpr {
    AST_EXPR_HEADER;
    enum JumpKind jump_kind;
};

struct AstForExpr {
    AST_EXPR_HEADER;
    String *name;
    struct AstExpr *target;
    struct AstExpr *block;
};

struct AstMatchArm {
    AST_EXPR_HEADER;
    struct AstPat *pat;
    struct AstExpr *guard;
    struct AstExpr *result;
};

struct AstMatchExpr {
    AST_EXPR_HEADER;
    struct AstExpr *target;
    struct AstExprList *arms;
};

struct AstExpr {
    union {
        struct AstExprHeader hdr;
#define DEFINE_UNION(X) struct Ast##X X##_;
        AST_EXPR_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kAstExprNames[] = {
#define DEFINE_NAME(X) "Ast"#X,
        AST_EXPR_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool AstIs##X(const struct AstExpr *node) { \
        return node->hdr.kind == kAst##X; \
    } \
    static inline struct Ast##X *AstGet##X(struct AstExpr *node) { \
        paw_assert(AstIs##X(node)); \
        return &node->X##_; \
    }
    AST_EXPR_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstExpr *pawAst_new_expr(struct Ast *ast);

static inline struct AstExpr *pawAst_new_paren_expr(struct Ast *ast, int line, struct AstExpr *expr)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .ParenExpr_.line = line,
        .ParenExpr_.kind = kAstParenExpr,
        .ParenExpr_.expr = expr,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_basic_lit(struct Ast *ast, int line, Value value, paw_Type code)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .LiteralExpr_.line = line,
        .LiteralExpr_.kind = kAstLiteralExpr,
        .LiteralExpr_.lit_kind = kAstBasicLit,
        .LiteralExpr_.basic.value = value,
        .LiteralExpr_.basic.code = code,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_composite_lit(struct Ast *ast, int line, struct AstPath *path, struct AstExprList *items)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .LiteralExpr_.line = line,
        .LiteralExpr_.kind = kAstLiteralExpr,
        .LiteralExpr_.lit_kind = kAstCompositeLit,
        .LiteralExpr_.comp.path = path,
        .LiteralExpr_.comp.items = items,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_container_lit(struct Ast *ast, int line, struct AstExprList *items, paw_Type code)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .LiteralExpr_.line = line,
        .LiteralExpr_.kind = kAstLiteralExpr,
        .LiteralExpr_.lit_kind = kAstContainerLit,
        .LiteralExpr_.cont.items = items,
        .LiteralExpr_.cont.code = code,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_tuple_lit(struct Ast *ast, int line, struct AstExprList *elems)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .LiteralExpr_.line = line,
        .LiteralExpr_.kind = kAstLiteralExpr,
        .LiteralExpr_.lit_kind = kAstTupleLit,
        .LiteralExpr_.tuple.elems = elems,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_logical_expr(struct Ast *ast, int line, struct AstExpr *lhs, struct AstExpr *rhs, paw_Bool is_and)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .LogicalExpr_.line = line,
        .LogicalExpr_.kind = kAstLogicalExpr,
        .LogicalExpr_.lhs = lhs,
        .LogicalExpr_.rhs = rhs,
        .LogicalExpr_.is_and = is_and,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_path_expr(struct Ast *ast, int line, struct AstPath *path)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .PathExpr_.line = line,
        .PathExpr_.kind = kAstPathExpr,
        .PathExpr_.path = path,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_chain_expr(struct Ast *ast, int line, struct AstExpr *target)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .ChainExpr_.line = line,
        .ChainExpr_.kind = kAstChainExpr,
        .ChainExpr_.target = target,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_unop_expr(struct Ast *ast, int line, enum UnaryOp op, struct AstExpr *target)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .UnOpExpr_.line = line,
        .UnOpExpr_.kind = kAstUnOpExpr,
        .UnOpExpr_.op = op,
        .UnOpExpr_.target = target,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_binop_expr(struct Ast *ast, int line, enum BinaryOp op, struct AstExpr *lhs, struct AstExpr *rhs)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .BinOpExpr_.line = line,
        .BinOpExpr_.kind = kAstBinOpExpr,
        .BinOpExpr_.op = op,
        .BinOpExpr_.lhs = lhs,
        .BinOpExpr_.rhs = rhs,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_closure_expr(struct Ast *ast, int line, struct AstDeclList *params, struct AstType *result, struct AstExpr *expr)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .ClosureExpr_.line = line,
        .ClosureExpr_.kind = kAstClosureExpr,
        .ClosureExpr_.params = params,
        .ClosureExpr_.result = result,
        .ClosureExpr_.expr = expr,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_conversion_expr(struct Ast *ast, int line, struct AstExpr *arg, paw_Type to)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .ConversionExpr_.line = line,
        .ConversionExpr_.kind = kAstConversionExpr,
        .ConversionExpr_.arg = arg,
        .ConversionExpr_.to = to,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_call_expr(struct Ast *ast, int line, struct AstExpr *target, struct AstExprList *args)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .CallExpr_.line = line,
        .CallExpr_.kind = kAstCallExpr,
        .CallExpr_.target = target,
        .CallExpr_.args = args,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_index(struct Ast *ast, int line, struct AstExpr *target, struct AstExpr *first, struct AstExpr *second, paw_Bool is_slice)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .Index_.line = line,
        .Index_.kind = kAstIndex,
        .Index_.target = target,
        .Index_.first = first,
        .Index_.second = second,
        .Index_.is_slice = is_slice,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_name_selector(struct Ast *ast, int line, struct AstExpr *target, String *name)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .Selector_.line = line,
        .Selector_.kind = kAstSelector,
        .Selector_.is_index = PAW_FALSE,
        .Selector_.target = target,
        .Selector_.name = name,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_index_selector(struct Ast *ast, int line, struct AstExpr *target, int index)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .Selector_.line = line,
        .Selector_.kind = kAstSelector,
        .Selector_.is_index = PAW_TRUE,
        .Selector_.target = target,
        .Selector_.index = index,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_named_field_expr(struct Ast *ast, int line, String *name, struct AstExpr *value, int fid)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .FieldExpr_.line = line,
        .FieldExpr_.kind = kAstFieldExpr,
        .FieldExpr_.name = name,
        .FieldExpr_.value = value,
        .FieldExpr_.fid = fid,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_keyed_field_expr(struct Ast *ast, int line, struct AstExpr *key, struct AstExpr *value)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .FieldExpr_.line = line,
        .FieldExpr_.kind = kAstFieldExpr,
        .FieldExpr_.key = key,
        .FieldExpr_.value = value,
        .FieldExpr_.fid = -1,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_assign_expr(struct Ast *ast, int line, struct AstExpr *lhs, struct AstExpr *rhs)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .AssignExpr_.line = line,
        .AssignExpr_.kind = kAstAssignExpr,
        .AssignExpr_.lhs = lhs,
        .AssignExpr_.rhs = rhs,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_block(struct Ast *ast, int line, struct AstStmtList *stmts, struct AstExpr *result)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .Block_.line = line,
        .Block_.kind = kAstBlock,
        .Block_.stmts = stmts,
        .Block_.result = result,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_if_expr(struct Ast *ast, int line, struct AstExpr *cond, struct AstExpr *then_arm, struct AstExpr *else_arm)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .IfExpr_.line = line,
        .IfExpr_.kind = kAstIfExpr,
        .IfExpr_.cond = cond,
        .IfExpr_.then_arm = then_arm,
        .IfExpr_.else_arm = else_arm,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_for_expr(struct Ast *ast, int line, String *name, struct AstExpr *target, struct AstExpr *block)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .ForExpr_.line = line,
        .ForExpr_.kind = kAstForExpr,
        .ForExpr_.name = name,
        .ForExpr_.target = target,
        .ForExpr_.block = block,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_while_expr(struct Ast *ast, int line, struct AstExpr *cond, struct AstExpr *block)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .WhileExpr_.line = line,
        .WhileExpr_.kind = kAstWhileExpr,
        .WhileExpr_.cond = cond,
        .WhileExpr_.block = block,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_jump_expr(struct Ast *ast, int line, enum JumpKind jump_kind)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .JumpExpr_.line = line,
        .JumpExpr_.kind = kAstJumpExpr,
        .JumpExpr_.jump_kind = jump_kind,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_return_expr(struct Ast *ast, int line, struct AstExpr *expr)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .ReturnExpr_.line = line,
        .ReturnExpr_.kind = kAstReturnExpr,
        .ReturnExpr_.expr = expr,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_match_arm(struct Ast *ast, int line, struct AstPat *pat, struct AstExpr *guard, struct AstExpr *result)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .MatchArm_.line = line,
        .MatchArm_.kind = kAstMatchArm,
        .MatchArm_.pat = pat,
        .MatchArm_.guard = guard,
        .MatchArm_.result = result,
    };
    return e;
}

static inline struct AstExpr *pawAst_new_match_expr(struct Ast *ast, int line, struct AstExpr *target, struct AstExprList *arms)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    *e = (struct AstExpr){
        .MatchExpr_.line = line,
        .MatchExpr_.kind = kAstMatchExpr,
        .MatchExpr_.target = target,
        .MatchExpr_.arms = arms,
    };
    return e;
}


enum AstPatKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_PAT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_PAT_HEADER \
    K_ALIGNAS_NODE int line; \
    enum AstPatKind kind : 8

struct AstPatHeader {
    AST_PAT_HEADER;
};

struct AstOrPat {
    AST_PAT_HEADER;
    struct AstPat *lhs;
    struct AstPat *rhs;
};

struct AstFieldPat {
    AST_PAT_HEADER;
    String *name;
    struct AstPat *pat;
};

struct AstStructPat {
    AST_PAT_HEADER;
    struct AstPath *path;
    struct AstPatList *fields; // [AstFieldPat]
};

struct AstVariantPat {
    AST_PAT_HEADER;
    struct AstPath *path;
    struct AstPatList *fields; // [AstPat]
};

struct AstTuplePat {
    AST_PAT_HEADER;
    struct AstPatList *elems; // [AstPat]
};

struct AstPathPat {
    AST_PAT_HEADER;
    struct AstPath *path;
};

struct AstWildcardPat {
    AST_PAT_HEADER;
};

struct AstLiteralPat {
    AST_PAT_HEADER;
    struct AstExpr *expr;
};

struct AstPat {
    union {
        struct AstPatHeader hdr;
#define DEFINE_UNION(X) struct Ast##X X##_;
        AST_PAT_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kAstPatNames[] = {
#define DEFINE_NAME(X) "Ast"#X,
        AST_PAT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool AstIs##X(const struct AstPat *node) { \
        return node->hdr.kind == kAst##X; \
    } \
    static inline struct Ast##X *AstGet##X(struct AstPat *node) { \
        paw_assert(AstIs##X(node)); \
        return &node->X##_; \
    }
    AST_PAT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstPat *pawAst_new_pat(struct Ast *ast);

static inline struct AstPat *pawAst_new_or_pat(struct Ast *ast, int line, struct AstPat *lhs, struct AstPat *rhs)
{
    struct AstPat *p = pawAst_new_pat(ast);
    *p = (struct AstPat){
        .OrPat_.line = line,
        .OrPat_.kind = kAstOrPat,
        .OrPat_.lhs = lhs,
        .OrPat_.rhs = rhs,
    };
    return p;
}

static inline struct AstPat *pawAst_new_field_pat(struct Ast *ast, int line, String *name, struct AstPat *pat)
{
    struct AstPat *p = pawAst_new_pat(ast);
    *p = (struct AstPat){
        .FieldPat_.line = line,
        .FieldPat_.kind = kAstFieldPat,
        .FieldPat_.name = name,
        .FieldPat_.pat = pat,
    };
    return p;
}

static inline struct AstPat *pawAst_new_struct_pat(struct Ast *ast, int line, struct AstPath *path, struct AstPatList *fields)
{
    struct AstPat *p = pawAst_new_pat(ast);
    *p = (struct AstPat){
        .StructPat_.line = line,
        .StructPat_.kind = kAstStructPat,
        .StructPat_.path = path,
        .StructPat_.fields = fields,
    };
    return p;
}

static inline struct AstPat *pawAst_new_variant_pat(struct Ast *ast, int line, struct AstPath *path, struct AstPatList *fields)
{
    struct AstPat *p = pawAst_new_pat(ast);
    *p = (struct AstPat){
        .VariantPat_.line = line,
        .VariantPat_.kind = kAstVariantPat,
        .VariantPat_.path = path,
        .VariantPat_.fields = fields,
    };
    return p;
}

static inline struct AstPat *pawAst_new_tuple_pat(struct Ast *ast, int line, struct AstPatList *elems)
{
    struct AstPat *p = pawAst_new_pat(ast);
    *p = (struct AstPat){
        .TuplePat_.line = line,
        .TuplePat_.kind = kAstTuplePat,
        .TuplePat_.elems = elems,
    };
    return p;
}

static inline struct AstPat *pawAst_new_path_pat(struct Ast *ast, int line, struct AstPath *path)
{
    struct AstPat *p = pawAst_new_pat(ast);
    *p = (struct AstPat){
        .PathPat_.line = line,
        .PathPat_.kind = kAstPathPat,
        .PathPat_.path = path,
    };
    return p;
}

static inline struct AstPat *pawAst_new_wildcard_pat(struct Ast *ast, int line)
{
    struct AstPat *p = pawAst_new_pat(ast);
    *p = (struct AstPat){
        .WildcardPat_.line = line,
        .WildcardPat_.kind = kAstWildcardPat,
    };
    return p;
}

static inline struct AstPat *pawAst_new_literal_pat(struct Ast *ast, int line, struct AstExpr *expr)
{
    struct AstPat *p = pawAst_new_pat(ast);
    *p = (struct AstPat){
        .LiteralPat_.line = line,
        .LiteralPat_.kind = kAstLiteralPat,
        .LiteralPat_.expr = expr,
    };
    return p;
}


enum AstStmtKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_STMT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_STMT_HEADER \
    K_ALIGNAS_NODE int line; \
    enum AstStmtKind kind : 8

struct AstStmtHeader {
    AST_STMT_HEADER;
};

struct AstDeclStmt {
    AST_STMT_HEADER;
    struct AstDecl *decl;
};

struct AstExprStmt {
    AST_STMT_HEADER;
    paw_Bool ends_block : 1;
    struct AstExpr *expr;
};

struct AstStmt {
    union {
        struct AstStmtHeader hdr;
#define DEFINE_UNION(X) struct Ast##X X##_;
        AST_STMT_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kAstStmtNames[] = {
#define DEFINE_NAME(X) "Ast"#X,
        AST_STMT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool AstIs##X(const struct AstStmt *node) { \
        return node->hdr.kind == kAst##X; \
    } \
    static inline struct Ast##X *AstGet##X(struct AstStmt *node) { \
        paw_assert(AstIs##X(node)); \
        return &node->X##_; \
    }
    AST_STMT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstStmt *pawAst_new_stmt(struct Ast *ast);

static struct AstStmt *pawAst_new_expr_stmt(struct Ast *ast, int line, struct AstExpr *expr)
{
    struct AstStmt *s = pawAst_new_stmt(ast);
    *s = (struct AstStmt){
        .ExprStmt_.line = line,
        .ExprStmt_.kind = kAstExprStmt,
        .ExprStmt_.expr = expr,
    };
    return s;
}

static struct AstStmt *pawAst_new_decl_stmt(struct Ast *ast, int line, struct AstDecl *decl)
{
    struct AstStmt *s = pawAst_new_stmt(ast);
    *s = (struct AstStmt){
        .DeclStmt_.line = line,
        .DeclStmt_.kind = kAstDeclStmt,
        .DeclStmt_.decl = decl,
    };
    return s;
}


struct Ast {
    struct AstDeclList *items;
    struct Compiler *C;
    struct Pool *pool;
    String *name;
    paw_Env *P;
    int modno;
};

#define AST_CAST_DECL(x) CAST(struct AstDecl *, x)
#define AST_CAST_EXPR(x) CAST(struct AstExpr *, x)
#define AST_CAST_STMT(x) CAST(struct AstStmt *, x)
#define AST_CAST_PAT(x) CAST(struct AstPat *, x)

DEFINE_LIST(struct Compiler, pawAst_decl_list_, AstDeclList, struct AstDecl *)
DEFINE_LIST(struct Compiler, pawAst_expr_list_, AstExprList, struct AstExpr *)
DEFINE_LIST(struct Compiler, pawAst_type_list_, AstTypeList, struct AstType *)
DEFINE_LIST(struct Compiler, pawAst_stmt_list_, AstStmtList, struct AstStmt *)
DEFINE_LIST(struct Compiler, pawAst_pat_list_, AstPatList, struct AstPat *)
DEFINE_LIST(struct Compiler, pawAst_generic_list_, AstGenericList, struct AstGeneric)
DEFINE_LIST(struct Compiler, pawAst_field_list_, AstFieldList, struct AstField)
DEFINE_LIST(struct Compiler, pawAst_param_list_, AstParamList, struct AstParam)
DEFINE_LIST(struct Compiler, pawAst_variant_list_, AstVariantList, struct AstVariant)
DEFINE_LIST(struct Compiler, pawAst_path_, AstPath, struct AstSegment)

struct Ast *pawAst_new(struct Compiler *C, String *name, int modno);
void pawAst_free(struct Ast *ast);

static inline struct AstSegment *pawAst_path_add(struct Compiler *C, struct AstPath *path, String *name, struct AstTypeList *args)
{
    K_LIST_PUSH(C, path, ((struct AstSegment){
                .types = args,
                .name = name,
            }));
    return &K_LIST_LAST(path);
}

#define AST_KINDOF(x) ((x)->hdr.kind)

void pawAst_dump(struct Ast *ast);

#endif // PAW_AST_H
