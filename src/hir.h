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

#define HIR_DECL_LIST(X) \
        X(FieldDecl, field) \
        X(FuncDecl, func) \
        X(GenericDecl, generic) \
        X(AdtDecl, adt) \
        X(TypeDecl, type) \
        X(VarDecl, var) \
        X(ImplDecl, impl) \
        X(VariantDecl, variant)

#define HIR_EXPR_LIST(X) \
        X(LiteralExpr, literal) \
        X(LogicalExpr, logical) \
        X(PathExpr, path) \
        X(ChainExpr, chain) \
        X(UnOpExpr, unop) \
        X(BinOpExpr, binop) \
        X(ClosureExpr, clos) \
        X(ConversionExpr, conv) \
        X(CallExpr, call) \
        X(Index, index) \
        X(Selector, select) \
        X(FieldExpr, field) \
        X(AssignExpr, assign) \
        X(Block, block) \
        X(IfExpr, if_) \
        X(ForExpr, for_) \
        X(WhileExpr, while_) \
        X(JumpExpr, jump) \
        X(ReturnExpr, result) \
        X(MatchArm, arm) \
        X(MatchExpr, match)

#define HIR_TYPE_LIST(X) \
        X(FuncPtr, fptr) \
        X(TupleType, tuple) \
        X(PathType, path)

#define HIR_STMT_LIST(X) \
        X(ExprStmt, expr) \
        X(DeclStmt, decl)

#define HIR_PAT_LIST(X) \
        X(OrPat, or) \
        X(FieldPat, field) \
        X(StructPat, struct_) \
        X(VariantPat, variant) \
        X(TuplePat, tuple) \
        X(PathPat, path) \
        X(BindingPat, bind) \
        X(WildcardPat, wild) \
        X(LiteralPat, lit)

#define HIR_CAST_DECL(x) CAST(struct HirDecl *, x)
#define HIR_CAST_EXPR(x) CAST(struct HirExpr *, x)
#define HIR_CAST_STMT(x) CAST(struct HirStmt *, x)
#define HIR_CAST_TYPE(x) CAST(struct HirType *, x)
#define HIR_CAST_PAT(x) CAST(struct HirPat *, x)

struct Hir;

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

enum HirTypeKind {
#define DEFINE_ENUM(a, b) kHir##a,
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

enum HirDeclKind {
#define DEFINE_ENUM(a, b) kHir##a,
    HIR_DECL_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define HIR_DECL_HEADER \
    K_ALIGNAS_NODE String *name; \
    int line; \
    HirId hid; \
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

struct HirImport {
    String *item;
    String *as;
    paw_Bool has_star : 1;
    int modno;
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

struct HirWhileExpr {
    HIR_EXPR_HEADER;
    struct HirExpr *cond;
    struct HirExpr *block;
};

struct HirJumpExpr {
    HIR_EXPR_HEADER;
    enum JumpKind jump_kind;
};

struct HirForExpr {
    HIR_EXPR_HEADER;
    struct HirDecl *control;
    struct HirExpr *target;
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

enum HirPatKind {
#define DEFINE_ENUM(a, b) kHir##a,
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
    int index;
    struct HirPath *path;
};

struct HirBindingPat {
    HIR_PAT_HEADER;
    String *name;
};

struct HirIdentPat {
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
#define DEFINE_UNION(a, b) struct Hir##a b;
        HIR_PAT_LIST(DEFINE_UNION)
#undef DEFINE_UNION
    };
};

static const char *kHirPatNames[] = {
#define DEFINE_NAME(a, b) "Hir"#a,
        HIR_PAT_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

#define DEFINE_ACCESS(a, b) \
    static inline paw_Bool HirIs##a(const struct HirPat *node) { \
        return node->hdr.kind == kHir##a; \
    } \
    static inline struct Hir##a *HirGet##a(struct HirPat *node) { \
        paw_assert(HirIs##a(node)); \
        return &node->b; \
    }
    HIR_PAT_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS


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

#define DEFINE_CALLBACK(a, b) paw_Bool (*Visit##a)(struct HirVisitor *V, struct Hir##a *node); \
                              void (*PostVisit##a)(struct HirVisitor *V, struct Hir##a *node);
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

struct HirPath *pawHir_new_path(struct Compiler *C);
struct HirType *pawHir_new_type(struct Compiler *C, int line, enum HirTypeKind kind);
struct HirDecl *pawHir_new_decl(struct Compiler *C, int line, enum HirDeclKind kind);
struct HirExpr *pawHir_new_expr(struct Compiler *C, int line, enum HirExprKind kind);
struct HirStmt *pawHir_new_stmt(struct Compiler *C, int line, enum HirStmtKind kind);
struct HirPat *pawHir_new_pat(struct Compiler *C, int line, enum HirPatKind kind);

DEFINE_LIST(struct Compiler, pawHir_decl_list_, HirDeclList, struct HirDecl *)
DEFINE_LIST(struct Compiler, pawHir_expr_list_, HirExprList, struct HirExpr *)
DEFINE_LIST(struct Compiler, pawHir_stmt_list_, HirStmtList, struct HirStmt *)
DEFINE_LIST(struct Compiler, pawHir_type_list_, HirTypeList, struct HirType *)
DEFINE_LIST(struct Compiler, pawHir_pat_list_, HirPatList, struct HirPat *)
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
