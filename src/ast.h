// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// This module contains the abstract syntax tree (AST) for Paw. An AST is created 
// for a given module during parsing, and is transformed into a HIR tree during type 
// checking.

#ifndef PAW_AST_H
#define PAW_AST_H

#include "code.h"

// TODO: Make *Expr, *Stmt, *Decl, and HirType opaque (typedef and put definition in *.c file)
//       forces use of accessor functions
struct Ast;
struct Compiler;

#define AST_DECL_LIST(X) \
        X(FieldDecl,   field) \
        X(FuncDecl,    func) \
        X(GenericDecl, generic) \
        X(AdtDecl,     adt) \
        X(TypeDecl,    type) \
        X(VarDecl,     var) \
        X(VariantDecl, variant)

#define AST_EXPR_LIST(X) \
        X(LiteralExpr,    literal) \
        X(LogicalExpr,    logical) \
        X(PathExpr,       path) \
        X(ChainExpr,      chain) \
        X(UnOpExpr,       unop) \
        X(BinOpExpr,      binop) \
        X(ClosureExpr,    clos) \
        X(ConversionExpr, conv) \
        X(CallExpr,       call) \
        X(Index,          index) \
        X(Selector,       selector) \
        X(TupleType,      tuple) \
        X(FieldExpr,      field) \
        X(Signature,      sig) \
        X(ContainerType,  cont) \
        X(AssignExpr,     assign) 

#define AST_STMT_LIST(X) \
        X(Block,      block) \
        X(ExprStmt,   expr) \
        X(DeclStmt,   decl) \
        X(IfStmt,     if_) \
        X(ForStmt,    for_) \
        X(WhileStmt,  while_) \
        X(LabelStmt,  label) \
        X(ReturnStmt, result)

#define AST_SEQUENCE_LIST(X) \
        X(ExprList) \
        X(DeclList) \
        X(StmtList) \
        X(Path)

enum AstDeclKind {
#define DEFINE_ENUM(a, b) kAst##a,
    AST_DECL_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

enum AstExprKind {
#define DEFINE_ENUM(a, b) kAst##a,
    AST_EXPR_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

enum AstStmtKind {
#define DEFINE_ENUM(a, b) kAst##a,
    AST_STMT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

struct AstSegment {
    String *name;
    struct AstExprList *types;
};

#define AST_DECL_HEADER \
    K_ALIGNAS_NODE String *name; \
    int line; \
    DefId def; \
    enum AstDeclKind kind : 8

struct AstDeclHeader {
    AST_DECL_HEADER; 
};

struct AstVarDecl {
    AST_DECL_HEADER; 
    paw_Bool is_pub : 1;
    paw_Bool is_const : 1;
    struct AstExpr *tag;
    struct AstExpr *init;
};

struct AstTypeDecl {
    AST_DECL_HEADER; 
    struct AstExpr *rhs;
    struct AstDeclList *generics;
};

struct AstFuncDecl {
    AST_DECL_HEADER; 
    paw_Bool is_pub : 1;
    enum FuncKind fn_kind : 7;
    struct AstDecl *receiver;
    struct AstDeclList *generics;
    struct AstDeclList *params;
    struct AstExpr *result;
    struct AstBlock *body;
};

// TODO: Need to prevent recursive structures, or introduce the concept of
//       indirection
struct AstAdtDecl {
    AST_DECL_HEADER; 
    paw_Bool is_pub : 1;
    paw_Bool is_struct : 1;
    struct AstDeclList *fields;
    struct AstDeclList *generics;
    int location;
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
    struct AstExpr *tag;
};

struct AstDecl {
    union {
        struct AstDeclHeader hdr;
#define DEFINE_UNION(a, b) struct Ast##a b;
        AST_DECL_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kAstDeclNames[] = {
#define DEFINE_NAME(a, b) "Ast"#a,
        AST_DECL_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(a, b) \
    static inline paw_Bool AstIs##a(const struct AstDecl *node) { \
        return node->hdr.kind == kAst##a; \
    } \
    static inline struct Ast##a *AstGet##a(struct AstDecl *node) { \
        paw_assert(AstIs##a(node)); \
        return &node->b; \
    }
    AST_DECL_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

#define AST_EXPR_HEADER \
    K_ALIGNAS_NODE int line; \
    enum AstExprKind kind : 8

#define AST_SUFFIXED_HEADER \
    AST_EXPR_HEADER; \
    struct AstExpr *target

struct AstExprHeader {
    AST_EXPR_HEADER;
};

struct AstPathExpr {
    AST_EXPR_HEADER;
    struct AstPath *path;
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
            paw_Type t;
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
    paw_Bool has_body : 1;
    struct AstDeclList *params;
    struct AstExpr *result;
    union {
        struct AstExpr *expr;
        struct AstBlock *body;
    };
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
    UnaryOp op : 8;
    struct AstExpr *target;
};

struct AstBinOpExpr {
    AST_EXPR_HEADER;
    BinaryOp op : 8;
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
    AST_SUFFIXED_HEADER;
};

struct AstChainExpr {
    AST_SUFFIXED_HEADER;
};

struct AstCallExpr {
    AST_SUFFIXED_HEADER;
    struct AstExprList *args;
};

struct AstSelector {
    AST_SUFFIXED_HEADER; 
    paw_Bool is_index : 1;
    union {
        String *name;
        paw_Int index;
    };
};

struct AstIndex {
    AST_SUFFIXED_HEADER; 
    paw_Bool is_slice : 1;
    struct AstExpr *first;
    struct AstExpr *second;
};

struct AstConversionExpr {
    AST_EXPR_HEADER;
    paw_Type to;
    struct AstExpr *arg;
};

struct AstTupleType {
    AST_EXPR_HEADER;
    struct AstExprList *types;
};

struct AstSignature {
    AST_EXPR_HEADER; 
    struct AstExpr *result;
    struct AstExprList *params;
};

struct AstContainerType {
    AST_EXPR_HEADER;
    struct AstExpr *first;
    struct AstExpr *second;
};

struct AstAssignExpr {
    AST_EXPR_HEADER;
    struct AstExpr *lhs;
    struct AstExpr *rhs;
};

struct AstExpr {
    union {
        struct AstExprHeader hdr;
#define DEFINE_UNION(a, b) struct Ast##a b;
        AST_EXPR_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kAstExprNames[] = {
#define DEFINE_NAME(a, b) "Ast"#a,
        AST_EXPR_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(a, b) \
    static inline paw_Bool AstIs##a(const struct AstExpr *node) { \
        return node->hdr.kind == kAst##a; \
    } \
    static inline struct Ast##a *AstGet##a(struct AstExpr *node) { \
        paw_assert(AstIs##a(node)); \
        return &node->b; \
    }
    AST_EXPR_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

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

struct AstBlock {
    AST_STMT_HEADER;
    struct AstStmtList *stmts;
    struct AstExpr *result;
};

struct AstReturnStmt {
    AST_STMT_HEADER;
    struct AstExpr *expr;
};

struct AstIfStmt {
    AST_STMT_HEADER;
    struct AstExpr *cond;
    struct AstStmt *then_arm;
    struct AstStmt *else_arm;
};

struct AstWhileStmt {
    AST_STMT_HEADER;
    paw_Bool is_dowhile : 1;
    struct AstExpr *cond;
    struct AstBlock *block;
};

struct AstLabelStmt {
    AST_STMT_HEADER;
    enum LabelKind label;
};

struct AstForStmt {
    AST_STMT_HEADER;
    paw_Bool is_fornum : 1;
    String *name;
    union {
        struct AstForIn {
            struct AstExpr *target;
        } forin;

        struct AstForNum {
            struct AstExpr *begin;
            struct AstExpr *end;
            struct AstExpr *step;
        } fornum;
    };
    struct AstBlock *block;
};

struct AstStmt {
    union {
        struct AstStmtHeader hdr;
#define DEFINE_UNION(a, b) struct Ast##a b;
        AST_STMT_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kAstStmtNames[] = {
#define DEFINE_NAME(a, b) "Ast"#a,
        AST_STMT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(a, b) \
    static inline paw_Bool AstIs##a(const struct AstStmt *node) { \
        return node->hdr.kind == kAst##a; \
    } \
    static inline struct Ast##a *AstGet##a(struct AstStmt *node) { \
        paw_assert(AstIs##a(node)); \
        return &node->b; \
    }
    AST_STMT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

struct Ast {
    struct Pool pool;
    struct AstDeclList *prelude;
    struct AstDeclList *items;
    paw_Env *P;
};

struct AstDecl *pawAst_new_decl(struct Ast *ast, int line, enum AstDeclKind kind);
struct AstExpr *pawAst_new_expr(struct Ast *ast, int line, enum AstExprKind kind);
struct AstStmt *pawAst_new_stmt(struct Ast *ast, int line, enum AstStmtKind kind);

#define AST_CAST_DECL(x) CAST(x, struct AstDecl *)
#define AST_CAST_EXPR(x) CAST(x, struct AstExpr *)
#define AST_CAST_STMT(x) CAST(x, struct AstStmt *)

DEFINE_LIST(struct Ast, pawAst_decl_list_, AstDeclList, struct AstDecl)
DEFINE_LIST(struct Ast, pawAst_expr_list_, AstExprList, struct AstExpr)
DEFINE_LIST(struct Ast, pawAst_stmt_list_, AstStmtList, struct AstStmt)
DEFINE_LIST(struct Ast, pawAst_path_, AstPath, struct AstSegment)

struct Ast *pawAst_new(struct Compiler *C);
void pawAst_free(struct Ast *ast);

struct AstSegment *pawAst_segment_new(struct Ast *ast);

static inline struct AstSegment *pawAst_path_add(struct Ast *ast, struct AstPath *path, String *name, struct AstExprList *args)
{
    struct AstSegment *ps = pawAst_segment_new(ast);
    ps->name = name;
    ps->types = args;
    pawAst_path_push(ast, path, ps);
    return ps;
}

#define AST_KINDOF(x) ((x)->hdr.kind)

// TODO: convert AST to string instead of printing
#include <stdio.h>

void pawAst_dump_path(FILE *out, struct AstPath *path);
void pawAst_dump_decl(FILE *out, struct AstDecl *decl);
void pawAst_dump_expr(FILE *out, struct AstExpr *expr);
void pawAst_dump_stmt(FILE *out, struct AstStmt *stmt);

#endif // PAW_AST_H
