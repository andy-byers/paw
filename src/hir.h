// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_HIR_H
#define PAW_HIR_H

#include "code.h"
#include "compile.h"
#include "paw.h"
#include "type.h"

#define HIR_DECL_LIST(X)        \
        X(FieldDecl,   field)   \
        X(FuncDecl,    func)    \
        X(GenericDecl, generic) \
        X(InstanceDecl, inst)   \
        X(AdtDecl,     adt)     \
        X(TypeDecl,    type)    \
        X(VarDecl,     var)     \
        X(VariantDecl, variant)

#define HIR_EXPR_LIST(X)          \
        X(LiteralExpr,    literal)\
        X(LogicalExpr,    logical)\
        X(PathExpr,       path)   \
        X(ChainExpr,      chain)  \
        X(UnOpExpr,       unop)   \
        X(BinOpExpr,      binop)  \
        X(ClosureExpr,    clos)   \
        X(ConversionExpr, conv)   \
        X(CallExpr,       call)   \
        X(Index,          index)  \
        X(Selector,       select) \
        X(StructItem,     sitem)  \
        X(MapItem,        mitem)

#define HIR_TYPE_LIST(X)    \
        X(Adt, adt)         \
        X(FuncDef, fdef)    \
        X(FuncPtr, fptr)    \
        X(Unknown, unknown) \
        X(Generic, generic) \
        X(TupleType, tuple)

#define HIR_STMT_LIST(X)      \
        X(Block,      block)  \
        X(ExprStmt,   expr)   \
        X(DeclStmt,   decl)   \
        X(IfStmt,     if_)    \
        X(ForStmt,    for_)   \
        X(WhileStmt,  while_) \
        X(LabelStmt,  label)  \
        X(ReturnStmt, result)

#define HIR_SEQUENCE_LIST(X) \
        X(ExprList)          \
        X(DeclList)          \
        X(StmtList)          \
        X(TypeList)          \
        X(Path)

struct Hir;

// Represents a single lexical scope
struct HirScope {
    struct HirSymbolList *symbols;
    int bk_depth;
    int fn_depth;
};

struct HirSymtab {
    struct HirScopeList *scopes;
    struct HirScope *toplevel;
    struct HirScope *globals;
};

#define last_scope(t) check_exp((t)->size > 0, (t)->data[(t)->size - 1])
struct HirScope *pawHir_new_scope(struct Hir *hir, struct HirSymtab *table);
struct HirSymtab *pawHir_new_symtab(struct Hir *hir);
void pawHir_add_scope(struct Hir *hir, struct HirSymtab *table, struct HirScope *scope);
struct HirSymbol *pawHir_add_symbol(struct Hir *hir, struct HirScope *table);
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

enum HirVarKind {
    VAR_GLOBAL,
    VAR_UPVALUE,
    VAR_LOCAL,
    VAR_FIELD,
    VAR_METHOD,
};

struct HirVarInfo {
    struct HirSymbol *symbol;
    enum HirVarKind kind;
    int index;
};

DECLARE_LIST(struct Hir, pawHir_decl_list_, HirDeclList, struct HirDecl)
DECLARE_LIST(struct Hir, pawHir_expr_list_, HirExprList, struct HirExpr)
DECLARE_LIST(struct Hir, pawHir_stmt_list_, HirStmtList, struct HirStmt)
DECLARE_LIST(struct Hir, pawHir_type_list_, HirTypeList, struct HirType)
DECLARE_LIST(struct Hir, pawHir_symbol_list_, HirSymbolList, struct HirSymbol)
DECLARE_LIST(struct Hir, pawHir_scope_list_, HirScopeList, struct HirScope)

struct HirSegment {
    String *name;
    struct HirTypeList *types;
    struct HirType *type;
};

DECLARE_LIST(struct Hir, pawHir_path_, HirPath, struct HirSegment)

struct HirSegment *pawHir_segment_new(struct Hir *hir);

static inline struct HirSegment *pawHir_path_add(struct Hir *hir, struct HirPath **ppath, String *name,
                                               struct HirTypeList *args, struct HirType *type)
{
    struct HirSegment *ps = pawHir_segment_new(hir);
    ps->name = name;
    ps->types = args;
    ps->type = type;
    pawHir_path_push(hir, ppath, ps);
    return ps;
}

enum HirTypeKind {
#define DEFINE_ENUM(a, b) kHir##a,
    HIR_TYPE_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_TYPE_HEADER enum HirTypeKind kind : 8
struct HirTypeHeader {
    HIR_TYPE_HEADER;
};

// Represents a generic type parameter
struct HirGeneric {
    HIR_TYPE_HEADER;
    String *name;
    DefId did;
};

// Represents a type that is in the process of being inferred
struct HirUnknown {
    HIR_TYPE_HEADER;
    int depth;
    int index;
};

// Represents a structure or enumeration type
struct HirAdt {
    HIR_TYPE_HEADER; 
    struct HirTypeList *types;
    DefId base;
    DefId did;
};

#define HIR_FUNC_HEADER \
    HIR_TYPE_HEADER;    \
    struct HirTypeList *params;    \
    struct HirType *result
struct HirFuncPtr {
    HIR_FUNC_HEADER; 
};

struct HirFuncDef {
    HIR_FUNC_HEADER; 
    struct HirTypeList *types;
    DefId base;
    DefId did;
};

struct HirTupleType {
    HIR_TYPE_HEADER; 
    struct HirTypeList *elems;
};

struct HirType {
    union {
        struct HirTypeHeader hdr;
#define DEFINE_UNION(a, b) struct Hir##a b;
        HIR_TYPE_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

#define DEFINE_ACCESS(a, b)                                       \
    static inline paw_Bool HirIs##a(const struct HirType *node) { \
        return node->hdr.kind == kHir##a;                         \
    }                                                             \
    static inline struct Hir##a *HirGet##a(struct HirType *node) { \
        paw_assert(HirIs##a(node));                                \
        return &node->b;                                           \
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
    struct HirType *type;  \
    String *name;   \
    int line;       \
    DefId did;      \
    enum HirDeclKind kind : 8
struct HirDeclHeader {
    HIR_DECL_HEADER; 
};

struct HirVarDecl {
    HIR_DECL_HEADER; 
    paw_Bool is_global : 1;
    paw_Bool is_const : 1;
    struct HirExpr *init;
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
    paw_Bool is_global : 1;
    enum FuncKind fn_kind : 7;
    struct HirDecl *receiver;
    struct HirScope *scope;
    struct HirDeclList *generics;
    struct HirDeclList *params;
    struct HirBlock *body;
    struct HirDeclList *monos;
};

struct HirAdtDecl {
    HIR_DECL_HEADER; 
    paw_Bool is_global : 1;
    paw_Bool is_struct : 1;
    struct HirScope *scope;
    struct HirScope *field_scope;
    struct HirDeclList *fields;
    struct HirDeclList *generics;
    struct HirDeclList *monos;
    int location;
};

struct HirVariantDecl {
    HIR_DECL_HEADER; 
    struct HirScope *scope;
    struct HirDeclList *fields;
    int index;
};

// Represents a template instance
// Created when an instantiation is found of the template that is currently
// being visited. For function templates, this node just stores the type of
// instantiated function for checking recursive calls. For structure
// templates, this node holds the type of each field, since fields have no
// direct representation in the type system (type system is 'nominal').
struct HirInstanceDecl {
    HIR_DECL_HEADER; 
    struct HirScope *scope;
    struct HirScope *field_scope;
    struct HirDeclList *types;
    struct HirDeclList *fields;
};

// Represents a generic type parameter
struct HirGenericDecl {
    HIR_DECL_HEADER; 
};

// HIR node representing a 'Field' production
struct HirFieldDecl {
    HIR_DECL_HEADER; 
};

struct HirDecl {
    union {
        struct HirDeclHeader hdr;
#define DEFINE_UNION(a, b) struct Hir##a b;
        HIR_DECL_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

#define DEFINE_ACCESS(a, b)                                       \
    static inline paw_Bool HirIs##a(const struct HirDecl *node) { \
        return node->hdr.kind == kHir##a;                         \
    }                                                             \
    static inline struct Hir##a *HirGet##a(struct HirDecl *node) { \
        paw_assert(HirIs##a(node));                                \
        return &node->b;                                           \
    }
    HIR_DECL_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

#define NO_DECL UINT16_MAX

//****************************************************************
//    Expressions
//****************************************************************

enum HirExprKind {
#define DEFINE_ENUM(a, b) kHir##a,
    HIR_EXPR_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_EXPR_HEADER   \
    int line;             \
    enum HirExprKind kind : 8; \
    struct HirType *type
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
    struct HirScope *scope;
    struct HirDeclList *params;
    struct HirBlock *body;
};

struct HirStructItem {
    HIR_EXPR_HEADER;
    int index;
    String *name;
    struct HirExpr *value;
};

struct HirMapItem {
    HIR_EXPR_HEADER;
    struct HirExpr *key;
    struct HirExpr *value;
};

struct HirUnOpExpr {
    HIR_EXPR_HEADER;
    UnaryOp op : 8;
    struct HirExpr *target;
};

struct HirBinOpExpr {
    HIR_EXPR_HEADER;
    BinaryOp op : 8;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

struct HirLogicalExpr {
    HIR_EXPR_HEADER;
    paw_Bool is_and : 1;
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

#define SUFFIXED_HEADER \
    HIR_EXPR_HEADER;        \
    struct HirExpr *target
struct HirSuffixedExpr {
    SUFFIXED_HEADER;
};

struct HirChainExpr {
    SUFFIXED_HEADER;
};

struct HirCallExpr {
    SUFFIXED_HEADER;
    struct HirType *func;
    struct HirExprList *args;
};

struct HirSelector {
    SUFFIXED_HEADER;
    paw_Bool is_index : 1;
    union {
        String *name;
        paw_Int index;
    };
};

struct HirIndex {
    SUFFIXED_HEADER;
    paw_Bool is_slice : 1;
    struct HirExpr *first;
    struct HirExpr *second;
};

struct HirConversionExpr {
    HIR_EXPR_HEADER;
    paw_Type to;
    struct HirExpr *arg;
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

#define DEFINE_ACCESS(a, b)                                       \
    static inline paw_Bool HirIs##a(const struct HirExpr *node) { \
        return node->hdr.kind == kHir##a;                         \
    }                                                             \
    static inline struct Hir##a *HirGet##a(struct HirExpr *node) { \
        paw_assert(HirIs##a(node));                                \
        return &node->b;                                           \
    }
    HIR_EXPR_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

enum HirStmtKind {
#define DEFINE_ENUM(a, b) kHir##a,
    HIR_STMT_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_STMT_HEADER \
    int line;           \
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
    struct HirExpr *lhs;
    struct HirExpr *rhs;
};

struct HirBlock {
    HIR_STMT_HEADER;
    struct HirScope *scope;
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
    struct HirScope *scope;
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
    struct HirScope *scope;
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

#define DEFINE_ACCESS(a, b)                                       \
    static inline paw_Bool HirIs##a(const struct HirStmt *node) { \
        return node->hdr.kind == kHir##a;                         \
    }                                                             \
    static inline struct Hir##a *HirGet##a(struct HirStmt *node) { \
        paw_assert(HirIs##a(node));                                \
        return &node->b;                                           \
    }
    HIR_STMT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

// Pointer to a context variable for each compilation pass.
union HirState {
    void *state;
    struct HirResolver *R;
    struct Generator *G;
    struct HirStenciler *S;
    struct HirCopier *C;
};

struct HirVisitor;
struct HirFolder;

// TODO: Should be able to use the entrypoint routines on list elements, may
// need slightly more specific nodes, like ParamDecl for parameters instead of
// overloading FieldDecl

// Represents a single pass over an HIR
struct HirVisitor {
    union HirState state;
    struct Hir *hir;

    void (*VisitExpr)(struct HirVisitor *V, struct HirExpr *expr);
    void (*VisitStmt)(struct HirVisitor *V, struct HirStmt *stmt);
    void (*VisitDecl)(struct HirVisitor *V, struct HirDecl *decl);
    void (*VisitPath)(struct HirVisitor *V, struct HirPath *path);

    void (*VisitExprList)(struct HirVisitor *V, struct HirExprList *list);
    void (*VisitDeclList)(struct HirVisitor *V, struct HirDeclList *list);
    void (*VisitStmtList)(struct HirVisitor *V, struct HirStmtList *list);

#define DEFINE_CALLBACK(a, b) void (*Visit##a)(struct HirVisitor *V, struct Hir##a *node);
    HIR_EXPR_LIST(DEFINE_CALLBACK)
    HIR_DECL_LIST(DEFINE_CALLBACK)
    HIR_STMT_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawHir_visitor_init(struct HirVisitor *V, struct Hir *hir, union HirState state);
void pawHir_visit(struct HirVisitor *V);

struct HirFolder {
    union HirState state;
    struct Hir *hir;

    struct HirPath *(*FoldPath)(struct HirFolder *F, struct HirPath *path);
    struct HirExpr *(*FoldExpr)(struct HirFolder *F, struct HirExpr *expr);
    struct HirStmt *(*FoldStmt)(struct HirFolder *F, struct HirStmt *stmt);
    struct HirDecl *(*FoldDecl)(struct HirFolder *F, struct HirDecl *decl);

    struct HirExprList *(*FoldExprList)(struct HirFolder *F, struct HirExprList *list);
    struct HirDeclList *(*FoldDeclList)(struct HirFolder *F, struct HirDeclList *list);
    struct HirStmtList *(*FoldStmtList)(struct HirFolder *F, struct HirStmtList *list);

#define DEFINE_CALLBACK(a, b) struct HirExpr *(*Fold##a)(struct HirFolder *F, struct Hir##a *node);
    HIR_EXPR_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK

#define DEFINE_CALLBACK(a, b) struct HirDecl *(*Fold##a)(struct HirFolder *F, struct Hir##a *node);
    HIR_DECL_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK

#define DEFINE_CALLBACK(a, b) struct HirStmt *(*Fold##a)(struct HirFolder *F, struct Hir##a *node);
    HIR_STMT_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawHir_folder_init(struct HirFolder *F, struct Hir *hir, union HirState state);
void pawHir_fold(struct HirFolder *F);

struct HirTypeFolder {
    void *state;

    struct HirType *(*Fold)(struct HirTypeFolder *F, struct HirType *type);
    struct HirTypeList *(*FoldList)(struct HirTypeFolder *F, struct HirTypeList *list);

#define DEFINE_CALLBACK(a, b) struct HirType *(*Fold##a)(struct HirTypeFolder *F, struct Hir##a *node);
    HIR_TYPE_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawHir_type_folder_init(struct HirTypeFolder *F, void *state);
struct HirType *pawHir_fold_type(struct HirTypeFolder *F, struct HirType *type);

struct Hir {
    struct Pool small;
    struct Pool large;
    struct HirType *builtin[7];
    struct HirSymtab *symtab;
    struct HirStmtList *prelude;
    struct HirStmtList *stmts;
    struct DynamicMem *dm;
    paw_Env *P;
};

struct HirSymbol *pawHir_new_symbol(struct Hir *hir);
struct HirType *pawHir_new_type(struct Hir *hir, enum HirTypeKind kind);
struct HirDecl *pawHir_new_decl(struct Hir *hir, enum HirDeclKind kind);
struct HirExpr *pawHir_new_expr(struct Hir *hir, enum HirExprKind kind);
struct HirStmt *pawHir_new_stmt(struct Hir *hir, enum HirStmtKind kind);

#define HIR_CAST_DECL(x) ((struct HirDecl *)(x))
#define HIR_CAST_EXPR(x) ((struct HirExpr *)(x))
#define HIR_CAST_STMT(x) ((struct HirStmt *)(x))
#define HIR_CAST_TYPE(x) ((struct HirType *)(x))

#define HIR_IS_UNIT_T(x) (HirIsAdt(x) && (x)->adt.base == PAW_TUNIT)
#define HIR_IS_BASIC_T(x) (HirIsAdt(x) && (x)->adt.base <= PAW_TSTRING)

#define HIR_IS_POLY_FUNC(decl) (HirIsFuncDecl(decl) && HirGetFuncDecl(decl)->generics != NULL)
#define HIR_IS_POLY_ADT(decl) (HirIsAdtDecl(decl) && HirGetAdtDecl(decl)->generics != NULL)

struct Hir *pawHir_new(struct Compiler *C);
void pawHir_free(struct Hir *hir);

struct HirDecl *pawHir_copy_decl(struct Hir *hir, struct HirDecl *decl);

void pawHir_stencil_stmts(struct Hir *hir, struct HirStmtList *stmts);

DefId pawHir_add_decl(struct Hir *hir, struct HirDecl *decl);
struct HirDecl *pawHir_get_decl(struct Hir *hir, DefId id);

#define HIR_TYPEOF(x) ((x)->hdr.type)
#define HIR_KINDOF(x) ((x)->hdr.kind)

static inline struct HirType *hir_vector_elem(struct HirType *t)
{
    return HirGetAdt(t)->types->data[0];
}

static inline struct HirType *hir_map_key(struct HirType *t)
{
    return HirGetAdt(t)->types->data[0];
}

static inline struct HirType *hir_map_value(struct HirType *t)
{
    return HirGetAdt(t)->types->data[1];
}

void pawHir_repr_type(struct Hir *hir, struct HirType *type);
void pawHir_dump_type(struct Hir *hir, struct HirType *type);
void pawHir_dump_path(struct Hir *hir, struct HirPath *path);
void pawHir_dump_decl(struct Hir *hir, struct HirDecl *decl);
void pawHir_dump_expr(struct Hir *hir, struct HirExpr *expr);
void pawHir_dump_stmt(struct Hir *hir, struct HirStmt *stmt);

#endif // PAW_HIR_H
