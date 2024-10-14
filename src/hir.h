// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_HIR_H
#define PAW_HIR_H

#include "compile.h"

struct Compiler;
struct Resolver;

typedef uint16_t DeclId;
#define NO_DECL UINT16_MAX

#define HIR_DECL_LIST(X) \
        X(FieldDecl,   field) \
        X(FuncDecl,    func) \
        X(GenericDecl, generic) \
        X(InstanceDecl, inst) \
        X(AdtDecl,     adt) \
        X(TypeDecl,    type) \
        X(VarDecl,     var) \
        X(ImplDecl,    impl) \
        X(UseDecl,     use) \
        X(VariantDecl, variant)

#define HIR_EXPR_LIST(X) \
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
        X(Selector,       select) \
        X(FieldExpr,      field) \
        X(AssignExpr,     assign)

#define HIR_TYPE_LIST(X) \
        X(Adt, adt) \
        X(FuncDef, fdef) \
        X(FuncPtr, fptr) \
        X(Unknown, unknown) \
        X(Generic, generic) \
        X(TupleType, tuple) \
        X(PathType, path)

#define HIR_STMT_LIST(X) \
        X(Block,      block) \
        X(ExprStmt,   expr) \
        X(DeclStmt,   decl) \
        X(IfStmt,     if_) \
        X(ForStmt,    for_) \
        X(WhileStmt,  while_) \
        X(LabelStmt,  label) \
        X(ReturnStmt, result)

#define HIR_CAST_DECL(x) CAST(struct HirDecl *, x)
#define HIR_CAST_EXPR(x) CAST(struct HirExpr *, x)
#define HIR_CAST_STMT(x) CAST(struct HirStmt *, x)
#define HIR_CAST_TYPE(x) CAST(struct HirType *, x)

struct Hir;

#define last_scope(t) CHECK_EXP((t)->size > 0, (t)->data[(t)->size - 1])
struct HirSymbol *pawHir_add_symbol(struct Compiler *C, struct HirScope *table);
int pawHir_find_symbol(struct HirScope *scope, const String *name);

// Represents an entry in the symbol table
//
// During the type checking pass, a struct HirSymbol is created for each declaration that
// is encountered. When an identifier is referenced, it is looked up in the list
// of symbol tables representing the enclosing scopes (as well as the global
// symbol table).
//
// The symbol table is used for all symbols, but not every symbol will end up on
// the stack. In particular, symbols with 'is_type' equal to 1 will not get a
// stack slot.
struct HirSymbol {
    paw_Bool is_init : 1;
    paw_Bool is_type : 1;
    paw_Bool is_const : 1;
    paw_Bool is_generic : 1;
    String *name;
    struct HirDecl *decl;
};

struct HirSegment {
    String *name;
    struct HirTypeList *types;
    int modno;
    DeclId base;
    DeclId did;
};

struct HirSegment *pawHir_segment_new(struct Compiler *C);

enum HirTypeKind {
#define DEFINE_ENUM(a, b) kHir##a,
    HIR_TYPE_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_TYPE_HEADER \
    K_ALIGNAS_NODE int line; \
    enum HirTypeKind kind: 8
struct HirTypeHeader {
    HIR_TYPE_HEADER;
};

// Represents a generic type parameter
struct HirGeneric {
    HIR_TYPE_HEADER;
    String *name;
    DeclId did;
};

// Represents a type that is in the process of being inferred
struct HirUnknown {
    HIR_TYPE_HEADER;
    int depth;
    int index;
};

struct HirAdt {
    HIR_TYPE_HEADER;
    struct HirTypeList *types;
    int modno;
    DeclId did;
};

// Represents a structure or enumeration type
struct HirPathType {
    HIR_TYPE_HEADER;
    struct HirPath *path;
};

#define HIR_FUNC_HEADER \
    HIR_TYPE_HEADER; \
    struct HirTypeList *params; \
    struct HirType *result
struct HirFuncPtr {
    HIR_FUNC_HEADER;
};

struct HirFuncDef {
    HIR_FUNC_HEADER;
    struct HirTypeList *types;
    int modno;
    DeclId base;
    DeclId did;
};

struct HirTupleType {
    HIR_TYPE_HEADER;
    struct HirTypeList *elems;
};

struct HirAssocType {
    HIR_TYPE_HEADER;
    struct HirType *self;
    struct HirTypeList *types;
    String *name;
};

struct HirType {
    union {
        struct HirTypeHeader hdr;
#define DEFINE_UNION(a, b) struct Hir##a b;
        HIR_TYPE_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kHirTypeNames[] = {
#define DEFINE_NAME(a, b) "Hir"#a,
    HIR_TYPE_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(a, b) \
    static inline paw_Bool HirIs##a(const struct HirType *node) { \
        return node->hdr.kind == kHir##a; \
    } \
    static inline struct Hir##a *HirGet##a(struct HirType *node) { \
        paw_assert(HirIs##a(node)); \
        return &node->b; \
    }
    HIR_TYPE_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

static inline paw_Bool HirIsFuncType(const struct HirType *type)
{
    return HirIsFuncDef(type) || HirIsFuncPtr(type);
}

enum HirDeclKind {
#define DEFINE_ENUM(a, b) kHir##a,
    HIR_DECL_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_DECL_HEADER \
    K_ALIGNAS_NODE struct HirType *type; \
    String *name; \
    int line; \
    DeclId did; \
    uint8_t flags : 8; \
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
    struct HirExpr *rhs;
    struct HirDeclList *generics;
};

struct HirFuncDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_assoc : 1;
    enum FuncKind fn_kind : 6;
    struct HirType *self;
    struct HirDeclList *generics;
    struct HirDeclList *params;
    struct HirType *result;
    struct HirBlock *body;
    struct HirDeclList *monos;
};

struct HirAdtDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_struct : 1;
    struct HirDeclList *generics;
    struct HirDeclList *fields;
    struct HirTypeList *monos;
};

struct HirUseDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    struct HirPath *path;
    int modno;
};

struct HirVariantDecl {
    HIR_DECL_HEADER;
    int index;
    struct HirDeclList *fields;
};

// Represents an instance of a polymorphic function
// Created during type checking and expanded during monomorphization.
struct HirInstanceDecl {
    HIR_DECL_HEADER;
    paw_Bool is_pub : 1;
    paw_Bool is_assoc : 1;
    struct HirType *self;
    struct HirTypeList *types;
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
    struct HirTypeList *subst; // TODO: remove, just use type list in HirType
    struct HirDeclList *generics;
    struct HirDeclList *methods;
    struct HirDeclList *monos;
};

struct HirDecl {
    union {
        struct HirDeclHeader hdr;
#define DEFINE_UNION(a, b) struct Hir##a b;
        HIR_DECL_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kHirDeclNames[] = {
#define DEFINE_NAME(a, b) "Hir"#a,
    HIR_DECL_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(a, b) \
    static inline paw_Bool HirIs##a(const struct HirDecl *node) { \
        return node->hdr.kind == kHir##a; \
    } \
    static inline struct Hir##a *HirGet##a(struct HirDecl *node) { \
        paw_assert(HirIs##a(node)); \
        return &node->b; \
    }
    HIR_DECL_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

enum HirExprKind {
#define DEFINE_ENUM(a, b) kHir##a,
    HIR_EXPR_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_EXPR_HEADER \
    K_ALIGNAS_NODE struct HirType *type; \
    int line; \
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
    paw_Bool has_body : 1;
    struct HirDeclList *params;
    struct HirType *result;
    union {
        struct HirExpr *expr;
        struct HirBlock *body;
    };
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
    struct HirType *func;
    struct HirExprList *args;
};

struct HirSelector {
    HIR_SUFFIXED_HEADER;
    paw_Bool is_index : 1;
    union {
        String *name;
        paw_Int index;
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

struct HirExpr {
    union {
        struct HirExprHeader hdr;
        struct HirSuffixedExpr suffix;
#define DEFINE_UNION(a, b) struct Hir##a b;
        HIR_EXPR_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kHirExprNames[] = {
#define DEFINE_NAME(a, b) "Hir"#a,
    HIR_EXPR_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(a, b) \
    static inline paw_Bool HirIs##a(const struct HirExpr *node) { \
        return node->hdr.kind == kHir##a; \
    } \
    static inline struct Hir##a *HirGet##a(struct HirExpr *node) { \
        paw_assert(HirIs##a(node)); \
        return &node->b; \
    }
    HIR_EXPR_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

enum HirStmtKind {
#define DEFINE_ENUM(a, b) kHir##a,
    HIR_STMT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_STMT_HEADER \
    K_ALIGNAS_NODE int line; \
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

struct HirBlock {
    HIR_STMT_HEADER;
    struct HirStmtList *stmts;
};

struct HirReturnStmt {
    HIR_STMT_HEADER;
    struct HirExpr *expr;
};

struct HirIfStmt {
    HIR_STMT_HEADER;
    struct HirExpr *cond;
    struct HirStmt *then_arm;
    struct HirStmt *else_arm;
};

struct HirWhileStmt {
    HIR_STMT_HEADER;
    paw_Bool is_dowhile : 1;
    struct HirExpr *cond;
    struct HirBlock *block;
};

struct HirLabelStmt {
    HIR_STMT_HEADER;
    enum LabelKind label;
};

struct HirForIn {
    struct HirExpr *target;
};

struct HirForNum {
    struct HirExpr *begin;
    struct HirExpr *end;
    struct HirExpr *step;
};

struct HirForStmt {
    HIR_STMT_HEADER;
    paw_Bool is_fornum : 1;
    struct HirDecl *control;
    struct HirBlock *block;
    union {
        struct HirForIn forin;
        struct HirForNum fornum;
    };
};

struct HirStmt {
    union {
        struct HirStmtHeader hdr;
#define DEFINE_UNION(a, b) struct Hir##a b;
        HIR_STMT_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kHirStmtNames[] = {
#define DEFINE_NAME(a, b) "Hir"#a,
    HIR_STMT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(a, b) \
    static inline paw_Bool HirIs##a(const struct HirStmt *node) { \
        return node->hdr.kind == kHir##a; \
    } \
    static inline struct Hir##a *HirGet##a(struct HirStmt *node) { \
        paw_assert(HirIs##a(node)); \
        return &node->b; \
    }
    HIR_STMT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

// Represents a single pass over an HIR
struct HirVisitor {
    struct Compiler *C;
    void *ud;
    int line;

    void (*VisitPath)(struct HirVisitor *V, struct HirPath *path);

    paw_Bool (*VisitExpr)(struct HirVisitor *V, struct HirExpr *node);
    paw_Bool (*VisitStmt)(struct HirVisitor *V, struct HirStmt *node);
    paw_Bool (*VisitDecl)(struct HirVisitor *V, struct HirDecl *node);
    paw_Bool (*VisitType)(struct HirVisitor *V, struct HirType *node);

    void (*PostVisitExpr)(struct HirVisitor *V, struct HirExpr *node);
    void (*PostVisitStmt)(struct HirVisitor *V, struct HirStmt *node);
    void (*PostVisitDecl)(struct HirVisitor *V, struct HirDecl *node);
    void (*PostVisitType)(struct HirVisitor *V, struct HirType *node);

#define DEFINE_CALLBACK(a, b) paw_Bool (*Visit##a)(struct HirVisitor *V, struct Hir##a *node); \
                              void (*PostVisit##a)(struct HirVisitor *V, struct Hir##a *node);
    HIR_EXPR_LIST(DEFINE_CALLBACK)
    HIR_DECL_LIST(DEFINE_CALLBACK)
    HIR_STMT_LIST(DEFINE_CALLBACK)
    HIR_TYPE_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawHir_visitor_init(struct HirVisitor *V, struct Compiler *C, void *ud);
void pawHir_visit_expr(struct HirVisitor *V, struct HirExpr *node);
void pawHir_visit_stmt(struct HirVisitor *V, struct HirStmt *node);
void pawHir_visit_decl(struct HirVisitor *V, struct HirDecl *node);
void pawHir_visit_type(struct HirVisitor *V, struct HirType *node);
void pawHir_visit_expr_list(struct HirVisitor *V, struct HirExprList *list);
void pawHir_visit_stmt_list(struct HirVisitor *V, struct HirStmtList *list);
void pawHir_visit_decl_list(struct HirVisitor *V, struct HirDeclList *list);
void pawHir_visit_type_list(struct HirVisitor *V, struct HirTypeList *list);

static inline void pawHir_visit_block(struct HirVisitor *V, struct HirBlock *node)
{
    pawHir_visit_stmt(V, HIR_CAST_STMT(node));
}

struct HirTypeFolder {
    struct HirVisitor V;
    struct Compiler *C;
    void *ud;
    int line;

    struct HirTypeList *(*FoldTypeList)(struct HirTypeFolder *F, struct HirTypeList *list);

#define DEFINE_CALLBACK(a, b) struct HirType *(*Fold##a)(struct HirTypeFolder *F, struct Hir##a *node);
    HIR_TYPE_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawHir_type_folder_init(struct HirTypeFolder *F, struct Compiler *C, void *ud);
struct HirType *pawHir_fold_type(struct HirTypeFolder *F, struct HirType *node);
void pawHir_fold_expr(struct HirTypeFolder *F, struct HirExpr *node);
void pawHir_fold_decl(struct HirTypeFolder *F, struct HirDecl *node);
void pawHir_fold_stmt(struct HirTypeFolder *F, struct HirStmt *node);
void pawHir_fold_expr_list(struct HirTypeFolder *F, struct HirExprList *list);
void pawHir_fold_stmt_list(struct HirTypeFolder *F, struct HirStmtList *list);
void pawHir_fold_decl_list(struct HirTypeFolder *F, struct HirDeclList *list);
struct HirTypeList *pawHir_fold_type_list(struct HirTypeFolder *F, struct HirTypeList *list);

static inline void pawHir_fold_block(struct HirTypeFolder *F, struct HirBlock *node)
{
    pawHir_fold_stmt(F, HIR_CAST_STMT(node));
}

struct Hir {
    struct HirDeclList *items;
    struct Compiler *C;
    struct Pool *pool;
    String *name;
    paw_Env *P;
    int modno;
};

struct HirSymbol *pawHir_new_symbol(struct Compiler *C);
struct HirPath *pawHir_new_path(struct Compiler *C);
struct HirType *pawHir_new_type(struct Compiler *C, int line, enum HirTypeKind kind);
struct HirDecl *pawHir_new_decl(struct Compiler *C, int line, enum HirDeclKind kind);
struct HirExpr *pawHir_new_expr(struct Compiler *C, int line, enum HirExprKind kind);
struct HirStmt *pawHir_new_stmt(struct Compiler *C, int line, enum HirStmtKind kind);

DEFINE_LIST(struct Compiler, pawHir_decl_list_, HirDeclList, struct HirDecl)
DEFINE_LIST(struct Compiler, pawHir_expr_list_, HirExprList, struct HirExpr)
DEFINE_LIST(struct Compiler, pawHir_stmt_list_, HirStmtList, struct HirStmt)
DEFINE_LIST(struct Compiler, pawHir_type_list_, HirTypeList, struct HirType)
DEFINE_LIST(struct Compiler, pawHir_scope_, HirScope, struct HirSymbol)
DEFINE_LIST(struct Compiler, pawHir_symtab_, HirSymtab, struct HirScope)
DEFINE_LIST(struct Compiler, pawHir_path_, HirPath, struct HirSegment)

#define HIR_IS_UNIT_T(x) (HirIsAdt(x) && hir_adt_did(x) == PAW_TUNIT)
#define HIR_IS_BASIC_T(x) (HirIsAdt(x) && hir_adt_did(x) <= PAW_TSTR)

#define HIR_IS_POLY_FUNC(decl) (HirIsFuncDecl(decl) && HirGetFuncDecl(decl)->generics != NULL)
#define HIR_IS_POLY_ADT(decl) (HirIsAdtDecl(decl) && HirGetAdtDecl(decl)->generics != NULL)
#define HIR_IS_POLY_IMPL(decl) (HirIsImplDecl(decl) && HirGetImplDecl(decl)->generics != NULL)

#define HIR_TYPE_DID(type) (HirIsAdt(type) ? HirGetAdt(type)->did : \
        HirIsFuncDef(type) ? HirGetFuncDef(type)->did : HirGetGeneric(type)->did)

struct Hir *pawHir_new(struct Compiler *C, String *name, int modno);
void pawHir_free(struct Hir *hir);

int pawHir_expand_bodies(struct Hir *hir);
void pawHir_define(struct ModuleInfo *m, struct HirDeclList *out, int *poffset);

DeclId pawHir_add_decl(struct Compiler *C, struct HirDecl *decl);
struct HirDecl *pawHir_get_decl(struct Compiler *C, DeclId id);

#define HIR_TYPEOF(x) ((x)->hdr.type)
#define HIR_KINDOF(x) ((x)->hdr.kind)

// NOTE: HirFuncPtr is a prefix of HirFuncDef
#define HIR_FPTR(t) CHECK_EXP(HirIsFuncType(t), &(t)->fptr)

static struct HirTypeList *hir_adt_types(struct HirType *type)
{
    return HirGetAdt(type)->types;
}

static DeclId hir_adt_did(struct HirType *type)
{
    return HirGetAdt(type)->did;
}

static inline struct HirType *hir_list_elem(struct HirType *type)
{
    return K_LIST_GET(HirGetAdt(type)->types, 0);
}

static inline struct HirType *hir_map_key(struct HirType *type)
{
    return K_LIST_GET(HirGetAdt(type)->types, 0);
}

static inline struct HirType *hir_map_value(struct HirType *type)
{
    return K_LIST_GET(HirGetAdt(type)->types, 1);
}

static inline struct HirSegment *pawHir_path_add(struct Compiler *C, struct HirPath *path, String *name,
                                                 struct HirTypeList *args)
{
    struct HirSegment *ps = pawHir_segment_new(C);
    ps->name = name;
    ps->types = args;
    pawHir_path_push(C, path, ps);
    return ps;
}

struct HirType *pawHir_attach_type(struct Compiler *C, DeclId did, enum HirTypeKind kind, int line);
struct HirTypeList *pawHir_collect_generics(struct Compiler *C, struct HirDeclList *generics);
struct HirTypeList *pawHir_collect_fields(struct Compiler *C, struct HirDeclList *fields);

void pawHir_print_type(struct Compiler *C, struct HirType *type);

void pawHir_dump(struct Hir *hir);
void pawHir_dump_path(struct Compiler *C, struct HirPath *path);
void pawHir_dump_decls(struct Compiler *C, struct HirDeclList *decls);

#endif // PAW_HIR_H
