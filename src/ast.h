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

#define AST_DECL_LIST(X) \
    X(FieldDecl)         \
    X(FuncDecl)          \
    X(GenericDecl)       \
    X(AdtDecl)           \
    X(TypeDecl)          \
    X(VarDecl)           \
    X(TraitDecl)         \
    X(UseDecl)           \
    X(VariantDecl)

#define AST_TYPE_LIST(X) \
    X(PathType)          \
    X(TupleType)         \
    X(ContainerType)     \
    X(FuncType)          \
    X(InferType)

#define AST_EXPR_LIST(X) \
    X(ParenExpr)         \
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
    X(ForExpr)           \
    X(WhileExpr)         \
    X(JumpExpr)          \
    X(ReturnExpr)        \
    X(MatchArm)          \
    X(MatchExpr)

#define AST_STMT_LIST(X) \
    X(ExprStmt)          \
    X(DeclStmt)

#define AST_PAT_LIST(X) \
    X(OrPat)            \
    X(FieldPat)         \
    X(StructPat)        \
    X(VariantPat)       \
    X(TuplePat)         \
    X(PathPat)          \
    X(WildcardPat)      \
    X(LiteralPat)

struct AstSegment {
    String *name;
    struct AstTypeList *types;
};

struct AstGenericBound {
    struct AstPath *path;
};

enum AstDeclKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_DECL_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_DECL_HEADER          \
    K_ALIGNAS_NODE String *name; \
    int line;                    \
    enum AstDeclKind kind : 8

struct AstDeclHeader {
    AST_DECL_HEADER;
};

struct AstVarDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct Annotations *annos;
    struct AstType *tag;
    struct AstExpr *init;
};

struct AstTypeDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct AstType *rhs;
    struct AstDeclList *generics;
};

struct AstFuncDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    enum FuncKind fn_kind : 7;
    struct Annotations *annos;
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
    struct AstTypeList *traits;
    struct AstDeclList *generics;
    struct AstDeclList *fields;
    struct AstDeclList *methods;
};

struct AstUseDecl {
    AST_DECL_HEADER;
    paw_Bool has_star : 1;
    int modno;
    String *item;
    String *as;
};

struct AstVariantDecl {
    AST_DECL_HEADER;
    struct AstDeclList *fields;
    int index;
};

struct AstGenericDecl {
    AST_DECL_HEADER;
    struct AstBoundList *bounds;
};

struct AstFieldDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct AstType *tag;
};

struct AstTraitDecl {
    AST_DECL_HEADER;
    paw_Bool is_pub : 1;
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

static char const *kAstDeclNames[] = {
#define DEFINE_NAME(X) "Ast" #X,
    AST_DECL_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                         \
    static inline paw_Bool AstIs##X(struct AstDecl const *node)  \
    {                                                            \
        return node->hdr.kind == kAst##X;                        \
    }                                                            \
    static inline struct Ast##X *AstGet##X(struct AstDecl *node) \
    {                                                            \
        paw_assert(AstIs##X(node));                              \
        return &node->X##_;                                      \
    }
AST_DECL_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstDecl *pawAst_new_decl(struct Ast *ast);

static struct AstDecl *pawAst_new_field_decl(struct Ast *ast, int line, String *name, struct AstType *tag, paw_Bool is_pub)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->FieldDecl_ = (struct AstFieldDecl){
        .line = line,
        .kind = kAstFieldDecl,
        .is_pub = is_pub,
        .name = name,
        .tag = tag,
    };
    return d;
}

inline static struct AstDecl *pawAst_new_func_decl(struct Ast *ast, int line, enum FuncKind fn_kind, String *name, struct Annotations *annos, struct AstDeclList *generics, struct AstDeclList *params, struct AstDecl *receiver, struct AstType *result, struct AstExpr *body, paw_Bool is_pub)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->FuncDecl_ = (struct AstFuncDecl){
        .line = line,
        .kind = kAstFuncDecl,
        .fn_kind = fn_kind,
        .annos = annos,
        .name = name,
        .is_pub = is_pub,
        .generics = generics,
        .params = params,
        .receiver = receiver,
        .result = result,
        .body = body,
    };
    return d;
}

inline static struct AstDecl *pawAst_new_generic_decl(struct Ast *ast, int line, String *name, struct AstBoundList *bounds)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->GenericDecl_ = (struct AstGenericDecl){
        .line = line,
        .kind = kAstGenericDecl,
        .name = name,
        .bounds = bounds,
    };
    return d;
}

inline static struct AstDecl *pawAst_new_adt_decl(struct Ast *ast, int line, String *name, struct AstTypeList *traits, struct AstDeclList *generics, struct AstDeclList *fields, struct AstDeclList *methods, paw_Bool is_pub, paw_Bool is_struct)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->AdtDecl_ = (struct AstAdtDecl){
        .line = line,
        .kind = kAstAdtDecl,
        .name = name,
        .traits = traits,
        .generics = generics,
        .fields = fields,
        .methods = methods,
        .is_struct = is_struct,
        .is_pub = is_pub,
    };
    return d;
}

inline static struct AstDecl *pawAst_new_type_decl(struct Ast *ast, int line, String *name, struct AstDeclList *generics, struct AstType *rhs, paw_Bool is_pub)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->TypeDecl_ = (struct AstTypeDecl){
        .line = line,
        .kind = kAstTypeDecl,
        .name = name,
        .generics = generics,
        .rhs = rhs,
        .is_pub = is_pub,
    };
    return d;
}

inline static struct AstDecl *pawAst_new_var_decl(struct Ast *ast, int line, String *name, struct Annotations *annos, struct AstType *tag, struct AstExpr *init, paw_Bool is_pub)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->VarDecl_ = (struct AstVarDecl){
        .line = line,
        .kind = kAstVarDecl,
        .annos = annos,
        .name = name,
        .tag = tag,
        .init = init,
        .is_pub = is_pub,
    };
    return d;
}

inline static struct AstDecl *pawAst_new_trait_decl(struct Ast *ast, int line, String *name, struct AstDeclList *generics, struct AstDeclList *methods, paw_Bool is_pub)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->TraitDecl_ = (struct AstTraitDecl){
        .line = line,
        .kind = kAstTraitDecl,
        .name = name,
        .generics = generics,
        .methods = methods,
        .is_pub = is_pub,

    };
    return d;
}

inline static struct AstDecl *pawAst_new_use_decl(struct Ast *ast, int line, String *name, paw_Bool has_star, String *item, String *as)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->UseDecl_ = (struct AstUseDecl){
        .line = line,
        .kind = kAstUseDecl,
        .name = name,
        .has_star = has_star,
        .item = item,
        .as = as,
    };
    return d;
}

inline static struct AstDecl *pawAst_new_variant_decl(struct Ast *ast, int line, String *name, struct AstDeclList *fields, int index)
{
    struct AstDecl *d = pawAst_new_decl(ast);
    d->VariantDecl_ = (struct AstVariantDecl){
        .line = line,
        .kind = kAstVariantDecl,
        .name = name,
        .fields = fields,
        .index = index,
    };
    return d;
}

enum AstTypeKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_TYPE_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_TYPE_HEADER      \
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

struct AstInferType {
    AST_TYPE_HEADER;
};

struct AstType {
    union {
        struct AstTypeHeader hdr;
#define DEFINE_UNION(X) struct Ast##X X##_;
        AST_TYPE_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static char const *kAstTypeNames[] = {
#define DEFINE_NAME(X) "Ast" #X,
    AST_TYPE_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                         \
    static inline paw_Bool AstIs##X(struct AstType const *node)  \
    {                                                            \
        return node->hdr.kind == kAst##X;                        \
    }                                                            \
    static inline struct Ast##X *AstGet##X(struct AstType *node) \
    {                                                            \
        paw_assert(AstIs##X(node));                              \
        return &node->X##_;                                      \
    }
AST_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstType *pawAst_new_type(struct Ast *ast);

inline static struct AstType *pawAst_new_path_type(struct Ast *ast, int line, struct AstPath *path)
{
    struct AstType *t = pawAst_new_type(ast);
    t->PathType_ = (struct AstPathType){
        .line = line,
        .kind = kAstPathType,
        .path = path,
    };
    return t;
}

inline static struct AstType *pawAst_new_tuple_type(struct Ast *ast, int line, struct AstTypeList *types)
{
    struct AstType *t = pawAst_new_type(ast);
    t->TupleType_ = (struct AstTupleType){
        .line = line,
        .kind = kAstTupleType,
        .types = types,
    };
    return t;
}

inline static struct AstType *pawAst_new_func_type(struct Ast *ast, int line, struct AstTypeList *params, struct AstType *result)
{
    struct AstType *t = pawAst_new_type(ast);
    t->FuncType_ = (struct AstFuncType){
        .line = line,
        .kind = kAstFuncType,
        .params = params,
        .result = result,
    };
    return t;
}

inline static struct AstType *pawAst_new_infer_type(struct Ast *ast, int line)
{
    struct AstType *t = pawAst_new_type(ast);
    t->InferType_ = (struct AstInferType){
        .line = line,
        .kind = kAstInferType,
    };
    return t;
}

inline static struct AstType *pawAst_new_container_type(struct Ast *ast, int line, struct AstType *first, struct AstType *second)
{
    struct AstType *t = pawAst_new_type(ast);
    t->ContainerType_ = (struct AstContainerType){
        .line = line,
        .kind = kAstContainerType,
        .first = first,
        .second = second,
    };
    return t;
}

enum AstExprKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_EXPR_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_EXPR_HEADER      \
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
            enum BuiltinKind code;
        } basic;

        struct AstCompositeLit {
            struct AstPath *path;
            struct AstExprList *items;
        } comp;

        struct AstContainerLit {
            struct AstExprList *items;
            enum BuiltinKind code;
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
    enum BuiltinKind to;
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

static char const *kAstExprNames[] = {
#define DEFINE_NAME(X) "Ast" #X,
    AST_EXPR_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                         \
    static inline paw_Bool AstIs##X(struct AstExpr const *node)  \
    {                                                            \
        return node->hdr.kind == kAst##X;                        \
    }                                                            \
    static inline struct Ast##X *AstGet##X(struct AstExpr *node) \
    {                                                            \
        paw_assert(AstIs##X(node));                              \
        return &node->X##_;                                      \
    }
AST_EXPR_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstExpr *pawAst_new_expr(struct Ast *ast);

inline static struct AstExpr *pawAst_new_paren_expr(struct Ast *ast, int line, struct AstExpr *expr)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->ParenExpr_ = (struct AstParenExpr){
        .line = line,
        .kind = kAstParenExpr,
        .expr = expr,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_basic_lit(struct Ast *ast, int line, Value value, enum BuiltinKind code)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->LiteralExpr_ = (struct AstLiteralExpr){
        .line = line,
        .kind = kAstLiteralExpr,
        .lit_kind = kAstBasicLit,
        .basic.value = value,
        .basic.code = code,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_composite_lit(struct Ast *ast, int line, struct AstPath *path, struct AstExprList *items)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->LiteralExpr_ = (struct AstLiteralExpr){
        .line = line,
        .kind = kAstLiteralExpr,
        .lit_kind = kAstCompositeLit,
        .comp.path = path,
        .comp.items = items,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_container_lit(struct Ast *ast, int line, struct AstExprList *items, enum BuiltinKind code)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->LiteralExpr_ = (struct AstLiteralExpr){
        .line = line,
        .kind = kAstLiteralExpr,
        .lit_kind = kAstContainerLit,
        .cont.items = items,
        .cont.code = code,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_tuple_lit(struct Ast *ast, int line, struct AstExprList *elems)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->LiteralExpr_ = (struct AstLiteralExpr){
        .line = line,
        .kind = kAstLiteralExpr,
        .lit_kind = kAstTupleLit,
        .tuple.elems = elems,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_logical_expr(struct Ast *ast, int line, struct AstExpr *lhs, struct AstExpr *rhs, paw_Bool is_and)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->LogicalExpr_ = (struct AstLogicalExpr){
        .line = line,
        .kind = kAstLogicalExpr,
        .lhs = lhs,
        .rhs = rhs,
        .is_and = is_and,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_path_expr(struct Ast *ast, int line, struct AstPath *path)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->PathExpr_ = (struct AstPathExpr){
        .line = line,
        .kind = kAstPathExpr,
        .path = path,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_chain_expr(struct Ast *ast, int line, struct AstExpr *target)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->ChainExpr_ = (struct AstChainExpr){
        .line = line,
        .kind = kAstChainExpr,
        .target = target,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_unop_expr(struct Ast *ast, int line, enum UnaryOp op, struct AstExpr *target)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->UnOpExpr_ = (struct AstUnOpExpr){
        .line = line,
        .kind = kAstUnOpExpr,
        .op = op,
        .target = target,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_binop_expr(struct Ast *ast, int line, enum BinaryOp op, struct AstExpr *lhs, struct AstExpr *rhs)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->BinOpExpr_ = (struct AstBinOpExpr){
        .line = line,
        .kind = kAstBinOpExpr,
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_closure_expr(struct Ast *ast, int line, struct AstDeclList *params, struct AstType *result, struct AstExpr *expr)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->ClosureExpr_ = (struct AstClosureExpr){
        .line = line,
        .kind = kAstClosureExpr,
        .params = params,
        .result = result,
        .expr = expr,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_conversion_expr(struct Ast *ast, int line, struct AstExpr *arg, enum BuiltinKind to)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->ConversionExpr_ = (struct AstConversionExpr){
        .line = line,
        .kind = kAstConversionExpr,
        .arg = arg,
        .to = to,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_call_expr(struct Ast *ast, int line, struct AstExpr *target, struct AstExprList *args)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->CallExpr_ = (struct AstCallExpr){
        .line = line,
        .kind = kAstCallExpr,
        .target = target,
        .args = args,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_index(struct Ast *ast, int line, struct AstExpr *target, struct AstExpr *first, struct AstExpr *second, paw_Bool is_slice)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->Index_ = (struct AstIndex){
        .line = line,
        .kind = kAstIndex,
        .target = target,
        .first = first,
        .second = second,
        .is_slice = is_slice,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_name_selector(struct Ast *ast, int line, struct AstExpr *target, String *name)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->Selector_ = (struct AstSelector){
        .line = line,
        .kind = kAstSelector,
        .is_index = PAW_FALSE,
        .target = target,
        .name = name,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_index_selector(struct Ast *ast, int line, struct AstExpr *target, int index)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->Selector_ = (struct AstSelector){
        .line = line,
        .kind = kAstSelector,
        .is_index = PAW_TRUE,
        .target = target,
        .index = index,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_named_field_expr(struct Ast *ast, int line, String *name, struct AstExpr *value, int fid)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->FieldExpr_ = (struct AstFieldExpr){
        .line = line,
        .kind = kAstFieldExpr,
        .name = name,
        .value = value,
        .fid = fid,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_keyed_field_expr(struct Ast *ast, int line, struct AstExpr *key, struct AstExpr *value)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->FieldExpr_ = (struct AstFieldExpr){
        .line = line,
        .kind = kAstFieldExpr,
        .key = key,
        .value = value,
        .fid = -1,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_assign_expr(struct Ast *ast, int line, struct AstExpr *lhs, struct AstExpr *rhs)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->AssignExpr_ = (struct AstAssignExpr){
        .line = line,
        .kind = kAstAssignExpr,
        .lhs = lhs,
        .rhs = rhs,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_block(struct Ast *ast, int line, struct AstStmtList *stmts, struct AstExpr *result)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->Block_ = (struct AstBlock){
        .line = line,
        .kind = kAstBlock,
        .stmts = stmts,
        .result = result,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_if_expr(struct Ast *ast, int line, struct AstExpr *cond, struct AstExpr *then_arm, struct AstExpr *else_arm)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->IfExpr_ = (struct AstIfExpr){
        .line = line,
        .kind = kAstIfExpr,
        .cond = cond,
        .then_arm = then_arm,
        .else_arm = else_arm,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_for_expr(struct Ast *ast, int line, String *name, struct AstExpr *target, struct AstExpr *block)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->ForExpr_ = (struct AstForExpr){
        .line = line,
        .kind = kAstForExpr,
        .name = name,
        .target = target,
        .block = block,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_while_expr(struct Ast *ast, int line, struct AstExpr *cond, struct AstExpr *block)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->WhileExpr_ = (struct AstWhileExpr){
        .line = line,
        .kind = kAstWhileExpr,
        .cond = cond,
        .block = block,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_jump_expr(struct Ast *ast, int line, enum JumpKind jump_kind)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->JumpExpr_ = (struct AstJumpExpr){
        .line = line,
        .kind = kAstJumpExpr,
        .jump_kind = jump_kind,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_return_expr(struct Ast *ast, int line, struct AstExpr *expr)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->ReturnExpr_ = (struct AstReturnExpr){
        .line = line,
        .kind = kAstReturnExpr,
        .expr = expr,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_match_arm(struct Ast *ast, int line, struct AstPat *pat, struct AstExpr *guard, struct AstExpr *result)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->MatchArm_ = (struct AstMatchArm){
        .line = line,
        .kind = kAstMatchArm,
        .pat = pat,
        .guard = guard,
        .result = result,
    };
    return e;
}

inline static struct AstExpr *pawAst_new_match_expr(struct Ast *ast, int line, struct AstExpr *target, struct AstExprList *arms)
{
    struct AstExpr *e = pawAst_new_expr(ast);
    e->MatchExpr_ = (struct AstMatchExpr){
        .line = line,
        .kind = kAstMatchExpr,
        .target = target,
        .arms = arms,
    };
    return e;
}

enum AstPatKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_PAT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_PAT_HEADER       \
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

static char const *kAstPatNames[] = {
#define DEFINE_NAME(X) "Ast" #X,
    AST_PAT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                        \
    static inline paw_Bool AstIs##X(struct AstPat const *node)  \
    {                                                           \
        return node->hdr.kind == kAst##X;                       \
    }                                                           \
    static inline struct Ast##X *AstGet##X(struct AstPat *node) \
    {                                                           \
        paw_assert(AstIs##X(node));                             \
        return &node->X##_;                                     \
    }
AST_PAT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstPat *pawAst_new_pat(struct Ast *ast);

inline static struct AstPat *pawAst_new_or_pat(struct Ast *ast, int line, struct AstPat *lhs, struct AstPat *rhs)
{
    struct AstPat *p = pawAst_new_pat(ast);
    p->OrPat_ = (struct AstOrPat){
        .line = line,
        .kind = kAstOrPat,
        .lhs = lhs,
        .rhs = rhs,
    };
    return p;
}

inline static struct AstPat *pawAst_new_field_pat(struct Ast *ast, int line, String *name, struct AstPat *pat)
{
    struct AstPat *p = pawAst_new_pat(ast);
    p->FieldPat_ = (struct AstFieldPat){
        .line = line,
        .kind = kAstFieldPat,
        .name = name,
        .pat = pat,
    };
    return p;
}

inline static struct AstPat *pawAst_new_struct_pat(struct Ast *ast, int line, struct AstPath *path, struct AstPatList *fields)
{
    struct AstPat *p = pawAst_new_pat(ast);
    p->StructPat_ = (struct AstStructPat){
        .line = line,
        .kind = kAstStructPat,
        .path = path,
        .fields = fields,
    };
    return p;
}

inline static struct AstPat *pawAst_new_variant_pat(struct Ast *ast, int line, struct AstPath *path, struct AstPatList *fields)
{
    struct AstPat *p = pawAst_new_pat(ast);
    p->VariantPat_ = (struct AstVariantPat){
        .line = line,
        .kind = kAstVariantPat,
        .path = path,
        .fields = fields,
    };
    return p;
}

inline static struct AstPat *pawAst_new_tuple_pat(struct Ast *ast, int line, struct AstPatList *elems)
{
    struct AstPat *p = pawAst_new_pat(ast);
    p->TuplePat_ = (struct AstTuplePat){
        .line = line,
        .kind = kAstTuplePat,
        .elems = elems,
    };
    return p;
}

inline static struct AstPat *pawAst_new_path_pat(struct Ast *ast, int line, struct AstPath *path)
{
    struct AstPat *p = pawAst_new_pat(ast);
    p->PathPat_ = (struct AstPathPat){
        .line = line,
        .kind = kAstPathPat,
        .path = path,
    };
    return p;
}

inline static struct AstPat *pawAst_new_wildcard_pat(struct Ast *ast, int line)
{
    struct AstPat *p = pawAst_new_pat(ast);
    p->WildcardPat_ = (struct AstWildcardPat){
        .line = line,
        .kind = kAstWildcardPat,
    };
    return p;
}

inline static struct AstPat *pawAst_new_literal_pat(struct Ast *ast, int line, struct AstExpr *expr)
{
    struct AstPat *p = pawAst_new_pat(ast);
    p->LiteralPat_ = (struct AstLiteralPat){
        .line = line,
        .kind = kAstLiteralPat,
        .expr = expr,
    };
    return p;
}

enum AstStmtKind {
#define DEFINE_ENUM(X) kAst##X,
    AST_STMT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define AST_STMT_HEADER      \
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

static char const *kAstStmtNames[] = {
#define DEFINE_NAME(X) "Ast" #X,
    AST_STMT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(X)                                         \
    static inline paw_Bool AstIs##X(struct AstStmt const *node)  \
    {                                                            \
        return node->hdr.kind == kAst##X;                        \
    }                                                            \
    static inline struct Ast##X *AstGet##X(struct AstStmt *node) \
    {                                                            \
        paw_assert(AstIs##X(node));                              \
        return &node->X##_;                                      \
    }
AST_STMT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct AstStmt *pawAst_new_stmt(struct Ast *ast);

static struct AstStmt *pawAst_new_expr_stmt(struct Ast *ast, int line, struct AstExpr *expr)
{
    struct AstStmt *s = pawAst_new_stmt(ast);
    s->ExprStmt_ = (struct AstExprStmt){
        .line = line,
        .kind = kAstExprStmt,
        .expr = expr,
    };
    return s;
}

static struct AstStmt *pawAst_new_decl_stmt(struct Ast *ast, int line, struct AstDecl *decl)
{
    struct AstStmt *s = pawAst_new_stmt(ast);
    s->DeclStmt_ = (struct AstDeclStmt){
        .line = line,
        .kind = kAstDeclStmt,
        .decl = decl,
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

DEFINE_LIST(struct Ast, AstDeclList, struct AstDecl *)
DEFINE_LIST(struct Ast, AstExprList, struct AstExpr *)
DEFINE_LIST(struct Ast, AstTypeList, struct AstType *)
DEFINE_LIST(struct Ast, AstStmtList, struct AstStmt *)
DEFINE_LIST(struct Ast, AstPatList, struct AstPat *)
DEFINE_LIST(struct Ast, AstPath, struct AstSegment)
DEFINE_LIST(struct Ast, AstBoundList, struct AstGenericBound)

struct Ast *pawAst_new(struct Compiler *C, String *name, int modno);
void pawAst_free(struct Ast *ast);

inline static struct AstSegment *AstPath_add(struct Ast *ast, struct AstPath *path, String *name, struct AstTypeList *args)
{
    AstPath_push(ast, path, (struct AstSegment){
                                .types = args,
                                .name = name,
                            });
    return &K_LIST_AT(path, path->count - 1);
}

#define AST_KINDOF(x) ((x)->hdr.kind)

char const *pawAst_dump(struct Ast *ast);

#endif // PAW_AST_H
