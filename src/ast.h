// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_AST_H
#define PAW_AST_H

#include "code.h"
#include "paw.h"
#include "type.h"

// TODO: Prefix the rest of the structs with 'Ast'
typedef struct Ast Ast;
typedef struct AstVisitor AstVisitor;
typedef struct AstFolder AstFolder;
typedef struct AstDecl AstDecl;
typedef struct AstExpr AstExpr;
typedef struct AstStmt AstStmt;
typedef struct Block Block;

// Represents an entry in the symbol table
//
// During the type checking pass, a Symbol is created for each declaration that is
// encountered. When an identifier is referenced, it is looked up in the list of
// symbol tables representing the enclosing scopes (as well as the global symbol
// table). 
//
// The symbol table is used for all symbols, but not every symbol will end up on the 
// stack. In particular, symbols with 'is_type' equal to 1 will not get a stack slot.
typedef struct Symbol {
    paw_Bool is_init: 1;
    paw_Bool is_type: 1;
    paw_Bool is_const: 1;
    paw_Bool is_generic: 1;
    String *name; // name of the symbol
    AstDecl *decl; // corresponding declaration
} Symbol;

typedef enum VarKind {
    VAR_GLOBAL,
    VAR_UPVALUE,
    VAR_LOCAL,
    VAR_FIELD,
    VAR_METHOD,
} VarKind;

typedef struct VarInfo {
    Symbol *symbol;
    VarKind kind;
    int index;
} VarInfo;

//****************************************************************
//    Node containers
//****************************************************************

typedef struct AstDeclList {
    AstDecl *first;
    int count;
} AstDeclList;

typedef struct AstExprList {
    AstExpr *first;
    int count;
} AstExprList;

typedef struct AstStmtList {
    AstStmt *first;
    int count;
} AstStmtList;

//****************************************************************
//    Declarations
//****************************************************************

typedef enum AstDeclKind {
    DECL_VAR,
    DECL_TYPE,
    DECL_FUNC,
    DECL_STRUCT,
    DECL_FIELD,
    DECL_GENERIC,
} AstDeclKind;

#define DECL_HEADER       \
    Type *type;           \
    struct AstDecl *next; \
    String *name;         \
    int line;             \
    DefId def;            \
    AstDeclKind kind: 8
typedef struct AstDeclHeader {
    DECL_HEADER; // common initial sequence
} AstDeclHeader;

typedef struct VarDecl {
    DECL_HEADER; // common initial sequence
    paw_Bool is_global: 1; // uses 'global' keyword
    paw_Bool is_const: 1; // uses 'const' keyword
    AstExpr *tag; // type annotation
    AstExpr *init; // initial value
} VarDecl;

// Node representing a type declaration
// Used for type aliases and builtin types.
typedef struct TypeDecl {
    DECL_HEADER; // common initial sequence
    AstExpr *rhs; // type right of '='
    AstDeclList *generics;
} TypeDecl;

typedef struct FuncDecl {
    DECL_HEADER; // common initial sequence
    paw_Bool is_global: 1; // 1 for global functions, 0 otherwise
    paw_Bool is_poly: 1; // 1 for templates, 0 otherwise
    paw_Bool is_visited: 1; // 1 if types resolved, 0 otherwise
    FuncKind fn_kind: 5; // kind of function (module, method, etc.)
    AstDecl *receiver; // pointer to receiver (StructDecl)
    AstDecl *sibling; // pointer to next method (FuncDecl)
    UniTable *unify; // unification table
    Scope *scope; // function-scoped symbols, including generics
    AstDeclList *generics; // generic type parameters (FieldDecl)
    AstDeclList *params; // parameter declarations
    AstExpr *return_; // return type
    Block *body; // function body
} FuncDecl;

// TODO: Need to prevent recursive types, or introduce the concept of indirection...
typedef struct StructDecl {
    DECL_HEADER; // common initial sequence
    paw_Bool is_global: 1; // uses 'global' keyword
    paw_Bool is_poly: 1; // 1 for templates, 0 otherwise
    paw_Bool is_visited: 1; // 1 if types resolved, 0 otherwise
    UniTable *unify; // unification table
    Scope *scope; // scope for struct-level symbols
    Scope *field_scope;
    Scope *method_scope;
    AstDeclList *fields; // list of FieldDecl
    AstDeclList *methods; // list of FuncDecl
    AstDeclList *generics; // generic type parameters (GenericDecl)
} StructDecl;

// Represents a template instance
typedef struct InstanceDecl {
    DECL_HEADER; // common initial sequence
    UniTable *unify; // unification table
} InstanceDecl;

// AST node representing a 'Field' production
typedef struct FieldDecl {
    DECL_HEADER; // common initial sequence
    AstExpr *tag; // type annotation
} FieldDecl;

typedef struct GenericDecl {
    DECL_HEADER; // common initial sequence
} GenericDecl;

typedef struct AstDecl {
    union {
        AstDeclHeader hdr;
        VarDecl var;
        FuncDecl func;
        StructDecl struct_;
        FieldDecl field;
        GenericDecl generic;
        TypeDecl type;
    };
} AstDecl;

#define NO_DECL UINT16_MAX

//****************************************************************
//    AstExpressions
//****************************************************************

typedef enum AstExprKind {
    EXPR_NAME,
    EXPR_CALL,
    EXPR_LITERAL,
    EXPR_CHAIN,
    EXPR_UNOP,
    EXPR_BINOP,
    EXPR_COALESCE,
    EXPR_LOGICAL,
    EXPR_COND,
    EXPR_INDEX,
    EXPR_ACCESS,
    EXPR_SELECTOR,
    EXPR_INVOKE,
    EXPR_SYMBOL,
    EXPR_ITEM,
    EXPR_FUNC_TYPE,
    EXPR_TYPE_NAME,
} AstExprKind;

#define EXPR_HEADER      \
    int line;            \
    AstExprKind kind: 8; \
    Type *type;          \
    struct AstExpr *next
typedef struct AstExprHeader {
    EXPR_HEADER;
} AstExprHeader;

// TODO: rename LiteralType -> LiteralKind
typedef enum LiteralType {
    LIT_BASIC,
    LIT_COMPOSITE,
    LIT_TUPLE,
    LIT_ARRAY,
} LiteralType;

typedef struct BasicLit {
    Value value;
    paw_Type t;
} BasicLit;

typedef struct ArrayLit {
    AstExprList *elems;
} ArrayLit;

typedef struct TupleLit {
    AstExprList *elems;
} TupleLit;

typedef struct CompositeLit {
    AstExpr *target;
    AstExprList *items;
} CompositeLit;

typedef struct LiteralExpr {
    EXPR_HEADER;
    LiteralType lit_kind;
    union {
        BasicLit basic;
        TupleLit tuple;
        ArrayLit array;
        CompositeLit comp;
    };
} LiteralExpr;

typedef struct AstIdent {
    EXPR_HEADER;
    String *name;
} AstIdent;

typedef struct ItemExpr {
    EXPR_HEADER;
    int index;
    String *name; // attribute name
    AstExpr *value;
} ItemExpr;

typedef struct UnOpExpr { 
    EXPR_HEADER; 
    UnaryOp op: 8;
    AstExpr *target; 
} UnOpExpr;

typedef struct BinOpExpr {
    EXPR_HEADER;
    BinaryOp op: 8;
    AstExpr *lhs;
    AstExpr *rhs;
} BinOpExpr;

typedef struct CondExpr {
    EXPR_HEADER;
    AstExpr *cond;
    AstExpr *lhs;
    AstExpr *rhs;
} CondExpr;

typedef struct LogicalExpr {
    EXPR_HEADER;
    paw_Bool is_and: 1;
    AstExpr *lhs;
    AstExpr *rhs;
} LogicalExpr;

#define SUFFIXED_HEADER EXPR_HEADER; \
                        AstExpr *target
typedef struct SuffixedExpr {
    SUFFIXED_HEADER;
} SuffixedExpr;

typedef struct ChainExpr {
    SUFFIXED_HEADER;
} ChainExpr;

typedef struct CallExpr {
    SUFFIXED_HEADER;
    Type *func;
    AstExprList *args;
} CallExpr;

typedef struct Selector {
    SUFFIXED_HEADER; // common fields
    paw_Bool is_method: 1; // 1 if selecting a method, 0 otherwise
    String *name; // name of the field
} Selector;

typedef struct Access {
    SUFFIXED_HEADER; // common fields
    String *name; // field name
} Access;

typedef struct Index {
    SUFFIXED_HEADER; // common fields
    AstExprList *elems; // list of elements
} Index;

// A valid TypeName is related to a AstDecl through the symbol table.
typedef struct TypeName {
    EXPR_HEADER; // common initial sequence
    String *name; // name of the struct or enum
    AstExprList *args;
} TypeName;

typedef struct FuncType {
    EXPR_HEADER; // common initial sequence
    AstExpr *return_; // return type annotation
    AstExprList *params; // parameter types
} FuncType;

typedef struct AstExpr {
    union {
        AstExprHeader hdr;
        LiteralExpr literal;
        LogicalExpr logical;
        AstIdent name;
        ChainExpr chain;
        UnOpExpr unop;
        BinOpExpr binop;
        CondExpr cond;
        SuffixedExpr suffix;
        CallExpr call;
        Index index;
        Access access;
        Selector selector;
        ItemExpr item;
        TypeName type_name;
        FuncType func;
    };
} AstExpr;

//****************************************************************
//    Statements
//****************************************************************

typedef enum AstStmtKind {
    STMT_EXPR,
    STMT_DECL,
    STMT_BLOCK,
    STMT_IF,
    STMT_FORIN,
    STMT_FORNUM,
    STMT_WHILE,
    STMT_DOWHILE,
    STMT_LABEL,
    STMT_RETURN,
} AstStmtKind;

#define STMT_HEADER   \
    int line;         \
    AstStmtKind kind: 8; \
    struct AstStmt *next
typedef struct AstStmtHeader {
    STMT_HEADER;
} AstStmtHeader;

typedef struct AstDeclStmt {
    STMT_HEADER;
    AstDecl *decl;
} AstDeclStmt;

typedef struct AstExprStmt {
    STMT_HEADER;
    AstExpr *lhs;
    AstExpr *rhs;
} AstExprStmt;

typedef struct Block {
    STMT_HEADER;
    Scope *scope; // scope for block 
    AstStmtList *stmts;
} Block;

typedef struct ReturnStmt {
    STMT_HEADER;
    AstExpr *expr;
} ReturnStmt;

typedef struct IfStmt {
    STMT_HEADER;
    AstExpr *cond;
    AstStmt *then_arm; // BlockStmt
    AstStmt *else_arm; // BlockStmt | IfStmt
} IfStmt;

typedef struct WhileStmt {
    STMT_HEADER;
    Scope *scope;
    AstExpr *cond;
    Block *block;
} WhileStmt;

typedef struct LabelStmt {
    STMT_HEADER;
    LabelKind label;
} LabelStmt;

typedef struct ForIn {
    AstExpr *target;
} ForIn;

typedef struct ForNum {
    AstExpr *begin;
    AstExpr *end;
    AstExpr *step;
} ForNum; 

typedef struct ForStmt {
    STMT_HEADER;
    Scope *scope; // scope for entire loop
    String *name; // loop control variable name
    Block *block; // body of loop
    union {
        ForIn forin;
        ForNum fornum; 
    };
} ForStmt;

typedef struct AstStmt {
    union {
        AstStmtHeader hdr;
        Block block;
        AstExprStmt expr;
        AstDeclStmt decl;
        IfStmt if_;
        ForStmt for_;
        WhileStmt while_;
        LabelStmt label;
        ReturnStmt return_;
    };
} AstStmt;

// Pointer to a context variable for each compilation pass.
typedef union AstState {
    void *state;
    struct Resolver *R; // symbol resolution state (pass 2)
    struct Stenciler *S; // template expansion state (pass 3)
    struct Generator *G; // code generation state (pass 4)
    struct Checker *C;
} AstState;

// TODO: Should be able to use the entrypoint routines on list elements, may need slightly more specific nodes, like ParamDecl for parameters instead of overloading FieldDecl
typedef void (*AstExprPass)(AstVisitor *pass, AstExpr *e);
typedef void (*AstStmtPass)(AstVisitor *pass, AstStmt *s);
typedef void (*AstDeclPass)(AstVisitor *pass, AstDecl *d);

// Represents a single pass over an AST
struct AstVisitor {
    AstState state;
    Ast *ast;

    // Entrypoints for each type of node
    AstExprPass visit_expr;
    AstStmtPass visit_stmt;
    AstDeclPass visit_decl;

    void (*visit_expr_list)(AstVisitor *V, AstExprList *list, AstExprPass cb);
    void (*visit_decl_list)(AstVisitor *V, AstDeclList *list, AstDeclPass cb);
    void (*visit_stmt_list)(AstVisitor *V, AstStmtList *list, AstStmtPass cb);

    // Special case for methods (FuncDecl), which are linked by the 'sibling' field,
    // rather than the 'next' field. 'next' is used by method template instances.
    void (*visit_method_list)(AstVisitor *V, AstDeclList *list, AstDeclPass cb);

    void (*visit_literal_expr)(AstVisitor *V, LiteralExpr *e);
    void (*visit_logical_expr)(AstVisitor *V, LogicalExpr *e);
    void (*visit_ident_expr)(AstVisitor *V, AstIdent *e);
    void (*visit_chain_expr)(AstVisitor *V, ChainExpr *e);
    void (*visit_unop_expr)(AstVisitor *V, UnOpExpr *e);
    void (*visit_binop_expr)(AstVisitor *V, BinOpExpr *e);
    void (*visit_cond_expr)(AstVisitor *V, CondExpr *e);
    void (*visit_suffix_expr)(AstVisitor *V, SuffixedExpr *e);
    void (*visit_call_expr)(AstVisitor *V, CallExpr *e);
    void (*visit_index_expr)(AstVisitor *V, Index *e);
    void (*visit_access_expr)(AstVisitor *V, Access *e);
    void (*visit_selector_expr)(AstVisitor *V, Selector *e);
    void (*visit_item_expr)(AstVisitor *V, ItemExpr *e);
    void (*visit_type_name_expr)(AstVisitor *V, TypeName *e);
    void (*visit_signature_expr)(AstVisitor *V, FuncType *e);

    void (*visit_block_stmt)(AstVisitor *V, Block *s);
    void (*visit_expr_stmt)(AstVisitor *V, AstExprStmt *s);
    void (*visit_decl_stmt)(AstVisitor *V, AstDeclStmt *s);
    void (*visit_if_stmt)(AstVisitor *V, IfStmt *s);
    void (*visit_for_stmt)(AstVisitor *V, ForStmt *s);
    void (*visit_while_stmt)(AstVisitor *V, WhileStmt *s);
    void (*visit_dowhile_stmt)(AstVisitor *V, WhileStmt *s);
    void (*visit_label_stmt)(AstVisitor *V, LabelStmt *s);
    void (*visit_return_stmt)(AstVisitor *V, ReturnStmt *s);

    void (*visit_var_decl)(AstVisitor *V, VarDecl *d);
    void (*visit_func_decl)(AstVisitor *V, FuncDecl *d);
    void (*visit_struct_decl)(AstVisitor *V, StructDecl *d);
    void (*visit_field_decl)(AstVisitor *V, FieldDecl *d);
    void (*visit_generic_decl)(AstVisitor *V, GenericDecl *d);
    void (*visit_type_decl)(AstVisitor *V, TypeDecl *d);
};

void pawA_visitor_init(AstVisitor *V, Ast *ast, AstState state);
void pawA_visit(AstVisitor *V);

typedef AstExpr *(*AstExprFold)(AstFolder *F, AstExpr *e);
typedef AstStmt *(*AstStmtFold)(AstFolder *F, AstStmt *s);
typedef AstDecl *(*AstDeclFold)(AstFolder *F, AstDecl *d);

struct AstFolder {
    AstState state;
    Ast *ast;

    // Entrypoints for each type of node
    AstExprFold fold_expr;
    AstStmtFold fold_stmt;
    AstDeclFold fold_decl;

    void (*fold_expr_list)(AstFolder *F, AstExprList *list, AstExprFold cb);
    void (*fold_decl_list)(AstFolder *F, AstDeclList *list, AstDeclFold cb);
    void (*fold_stmt_list)(AstFolder *F, AstStmtList *list, AstStmtFold cb);
    void (*fold_method_list)(AstFolder *F, AstDeclList *list, AstDeclFold cb);

    AstExpr *(*fold_literal_expr)(AstFolder *F, LiteralExpr *e);
    AstExpr *(*fold_logical_expr)(AstFolder *F, LogicalExpr *e);
    AstExpr *(*fold_ident_expr)(AstFolder *F, AstIdent *e);
    AstExpr *(*fold_chain_expr)(AstFolder *F, ChainExpr *e);
    AstExpr *(*fold_unop_expr)(AstFolder *F, UnOpExpr *e);
    AstExpr *(*fold_binop_expr)(AstFolder *F, BinOpExpr *e);
    AstExpr *(*fold_cond_expr)(AstFolder *F, CondExpr *e);
    AstExpr *(*fold_suffix_expr)(AstFolder *F, SuffixedExpr *e);
    AstExpr *(*fold_call_expr)(AstFolder *F, CallExpr *e);
    AstExpr *(*fold_index_expr)(AstFolder *F, Index *e);
    AstExpr *(*fold_access_expr)(AstFolder *F, Access *e);
    AstExpr *(*fold_selector_expr)(AstFolder *F, Selector *e);
    AstExpr *(*fold_item_expr)(AstFolder *F, ItemExpr *e);
    AstExpr *(*fold_type_name_expr)(AstFolder *F, TypeName *e);
    AstExpr *(*fold_signature_expr)(AstFolder *F, FuncType *e);

    AstStmt *(*fold_block_stmt)(AstFolder *F, Block *s);
    AstStmt *(*fold_expr_stmt)(AstFolder *F, AstExprStmt *s);
    AstStmt *(*fold_decl_stmt)(AstFolder *F, AstDeclStmt *s);
    AstStmt *(*fold_if_stmt)(AstFolder *F, IfStmt *s);
    AstStmt *(*fold_for_stmt)(AstFolder *F, ForStmt *s);
    AstStmt *(*fold_while_stmt)(AstFolder *F, WhileStmt *s);
    AstStmt *(*fold_label_stmt)(AstFolder *F, LabelStmt *s);
    AstStmt *(*fold_return_stmt)(AstFolder *F, ReturnStmt *s);

    AstDecl *(*fold_var_decl)(AstFolder *F, VarDecl *d);
    AstDecl *(*fold_func_decl)(AstFolder *F, FuncDecl *d);
    AstDecl *(*fold_struct_decl)(AstFolder *F, StructDecl *d);
    AstDecl *(*fold_field_decl)(AstFolder *F, FieldDecl *d);
    AstDecl *(*fold_generic_decl)(AstFolder *F, GenericDecl *d);
    AstDecl *(*fold_type_decl)(AstFolder *F, TypeDecl *d);
};

void pawA_folder_init(AstFolder *F, Ast *ast, AstState state);
void pawA_fold(AstFolder *F);

typedef struct Ast {
    Pool nodes;
    Pool symbols;
    Pool sequences;
    AstStmtList *stmts;
    Lex *lex;
} Ast;

//****************************************************************
//     Helper routines
//****************************************************************

Symbol *pawA_new_symbol(Lex *lex);
AstDecl *pawA_new_decl(Ast *ast, AstDeclKind kind);
AstExpr *pawA_new_expr(Ast *ast, AstExprKind kind);
AstStmt *pawA_new_stmt(Ast *ast, AstStmtKind kind);
AstDeclList *pawA_new_decl_list(Ast *ast);
AstExprList *pawA_new_expr_list(Ast *ast);
AstStmtList *pawA_new_stmt_list(Ast *ast);

void *pawA_new_pointer_vec(Ast *ast, int nptrs);

#define cast_decl(x) ((AstDecl *)(x))
#define cast_expr(x) ((AstExpr *)(x))
#define cast_stmt(x) ((AstStmt *)(x))

//****************************************************************
//     AST manipulation
//****************************************************************

Ast *pawA_new_ast(Lex *lex);
void pawA_free_ast(Ast *ast);

AstDecl *pawA_stencil(Ast *ast, AstDecl *decl);

//****************************************************************
//     AST helpers
//****************************************************************

#define a_type(x) ((x)->hdr.type)
#define a_kind(x) ((x)->hdr.kind)
#define a_next(x) ((x)->hdr.next)

// Macros for checking node types
#define a_is_basic(e) (a_kind(e) == EXPR_BASIC_TYPE)
#define a_is_unit(e) (a_is_basic(e) && (e)->basic.code == PAW_TUNIT)
#define a_is_bool(e) (a_is_basic(e) && (e)->basic.code == PAW_TBOOL)
#define a_is_int(e) (a_is_basic(e) && (e)->basic.code == PAW_TINT)
#define a_is_float(e) (a_is_basic(e) && (e)->basic.code == PAW_TFLOAT)
#define a_is_string(e) (a_is_basic(e) && (e)->basic.code == PAW_TSTRING)

#define a_is_struct_layout(e) (a_kind(e) == EXPR_STRUCT_LAYOUT)

#define a_is_generic_type(e) (a_kind(e) == EXPR_GENERIC_TYPE)
#define a_is_named_type(e) (a_kind(e) == EXPR_TYPE_NAME)
#define a_is_func_type(e) (a_kind(e) == EXPR_FUNC_TYPE)
#define a_is_generic_decl(e) (a_kind(e) == DECL_GENERIC)
#define a_is_struct_decl(d) (a_kind(d) == DECL_STRUCT)
#define a_is_func_decl(d) (a_kind(d) == DECL_FUNC)

#define a_has_receiver(d) (a_is_func_decl(d) && (d)->func.receiver != NULL)
#define a_is_template_decl(d) (a_is_func_template_decl(d) || \
                               a_is_struct_template_decl(d)) 

#define a_is_func_template_decl(d) (a_is_func_decl(d) && d->func.is_poly)
#define a_is_struct_template_decl(d) (a_is_struct_decl(d) && d->struct_.is_poly)

void pawA_dump_decl(FILE *out, AstDecl *decl);
void pawA_dump_expr(FILE *out, AstExpr *expr);
void pawA_dump_stmt(FILE *out, AstStmt *stmt);

#endif // PAW_AST_H
