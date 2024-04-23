// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "opcode.h"
#include "parse.h"
#include "paw.h"

typedef enum ExprType {
    EXPR_VAR,
    EXPR_PRIMITIVE,
    EXPR_LITERAL,
    EXPR_ARRAY,
    EXPR_MAP,
    EXPR_CHAIN,
    EXPR_COALESCE,
    EXPR_LOGICAL,
    EXPR_UNOP,
    EXPR_BINOP,
    EXPR_CALL,
    EXPR_COND,
    EXPR_INDEX,
    EXPR_ACCESS,
    EXPR_INVOKE,
    EXPR_SYMBOL,
    EXPR_TYPE,
    EXPR_INIT,
} ExprType;

typedef enum StmtType {
    STMT_EXPR,
    STMT_BLOCK,
    STMT_IFELSE,
    STMT_FORIN,
    STMT_FORNUM,
    STMT_WHILE,
    STMT_DOWHILE,
    STMT_LABEL,
    STMT_CLASS,
    STMT_ATTR,
    STMT_FN,
    STMT_DEF,
    STMT_ITEM,
    STMT_PARAM,
    STMT_RETURN,
    STMT_TYPENAME,
} StmtType;

#define NODE_HEADER \
    int line;       \
    uint8_t kind
#define STMT_HEADER \
    NODE_HEADER;    \
    struct Stmt *next
#define EXPR_HEADER \
    NODE_HEADER;    \
    Type *type;     \
    struct Expr *next

typedef struct Node {
    NODE_HEADER;
} Node;

typedef struct Expr {
    EXPR_HEADER;
} Expr;

typedef struct Stmt {
    STMT_HEADER;
} Stmt;

typedef struct Symbol {
    EXPR_HEADER;
    paw_Bool is_init: 1;
    paw_Bool is_type: 1;
    String *name;
} Symbol;

typedef struct Arena {
    struct Arena *prev;
    size_t used;
    size_t size;

    // Must be aligned to at least the strictest alignment required
    // by a Node.
    _Alignas(void *) char data[];
} Arena;

typedef struct Block {
    STMT_HEADER;
    Stmt *stmts;
    Scope *scope; // scope for block 
    int nstmts;
} Block;

typedef struct TypeDecl TypeDecl;

typedef struct BasicTypeDecl {
    paw_Type t;
} BasicTypeDecl;

typedef struct NamedTypeDecl {
    String *name;
} NamedTypeDecl;

typedef struct SignatureDecl {
    TypeDecl *ret;
    Expr *args;
    int nargs;
} SignatureDecl;

struct TypeDecl {
    EXPR_HEADER;
    TypeKind group;
    union {
        BasicTypeDecl basic; 
        NamedTypeDecl named;
        SignatureDecl sig;
    };
};

typedef struct Function {
    Scope *scope; // function-scoped variables                            
    Type *type;                        
    String *name; // name of the function
    Stmt *args; // AST nodes for parameters
    TypeDecl *ret; // return type annotation
    Block *body; // function body
    FnKind kind; // type of function
    int nargs;
} Function;

typedef struct Tree {
    Arena *arena;
    Stmt *stmts;
    int nstmts;
} Tree;

typedef enum VarKind {
    VAR_UNDEF,
    VAR_GLOBAL,
    VAR_UPVALUE,
    VAR_LOCAL,
} VarKind;

typedef struct VarInfo {
    Type *type;
    enum VarKind kind;
    int index;
} VarInfo;

typedef struct ItemStmt {
    STMT_HEADER;
    String *name; // attribute name
    Expr *value;
} ItemStmt;

typedef struct InitExpr { 
    EXPR_HEADER; 
    Expr *prefix;
    Stmt *attrs; // list of ItemStmt
    int nattrs;
} InitExpr;

typedef struct UnOpExpr { 
    EXPR_HEADER; 
    UnaryOp op;
    FunctionType *mm;
    Expr *target; 
} UnOpExpr;

#define make_binary_expr(a, b) \
    typedef struct a ## Expr { \
        EXPR_HEADER; \
        b \
        Expr *lhs; \
        Expr *rhs; \
        FunctionType *mm; \
    } a ## Expr;

make_binary_expr(BinOp, BinaryOp op;)
make_binary_expr(Cond, Expr *cond;)
make_binary_expr(Logical, uint8_t is_and;)
make_binary_expr(Coalesce, )

#define SUFFIXED_HEADER EXPR_HEADER; Expr *target
typedef struct SuffixedExpr {
    SUFFIXED_HEADER;
} SuffixedExpr;

typedef struct ChainExpr {
    SUFFIXED_HEADER;
} ChainExpr;

typedef struct CallExpr {
    SUFFIXED_HEADER;
    Expr *args;
    int nargs;
} CallExpr;

typedef struct IndexExpr {
    SUFFIXED_HEADER; // common fields
    Expr *first; // index/key or slice beginning
    Expr *second; // slice end, or NULL
} IndexExpr;

typedef struct AccessExpr {
    SUFFIXED_HEADER; // common fields
    uint16_t index; // index in constants table
    String *name; // field name
} AccessExpr;

typedef struct InvokeExpr {
    SUFFIXED_HEADER; // common fields
    int index;                     
    String *name; // method name
    Expr *args;
    int nargs;
} InvokeExpr;

typedef struct VarExpr {
    EXPR_HEADER; // common fields
    String *name; // name of the variable
} VarExpr;

#define CONTAINER_HEADER EXPR_HEADER; Type *t
typedef struct ContainerExpr {
    CONTAINER_HEADER;
} ContainerExpr;

typedef struct ArrayExpr {
    CONTAINER_HEADER;
    Expr *items;
    int nitems;
} ArrayExpr;

typedef struct PrimitiveExpr {
    EXPR_HEADER;
    paw_Type t;
    Value v;
} PrimitiveExpr;

typedef struct LiteralExpr {
    EXPR_HEADER;
    paw_Type t;
    const char *label;
    Expr *expr;
} LiteralExpr;

typedef struct DefStmt {
    STMT_HEADER; // common fields
    struct {
        paw_Bool global: 1; // uses 'global' keyword
    } flags;
    TypeDecl *tag; // type annotation
    String *name; // variable name
    Expr *init; // initial value
} DefStmt;

typedef struct ParamStmt {
    STMT_HEADER;
    TypeDecl *tag; // type annotation
    String *name; // variable name
} ParamStmt;

typedef struct FnStmt {
    STMT_HEADER;
    struct {
        paw_Bool global: 1; // uses 'global' keyword
        FnKind kind: 7;
    } flags;
    Function fn;
} FnStmt;

typedef struct AttrStmt {
    STMT_HEADER;
    uint8_t is_fn; // // 1 if 'fn' is valid, 0 otherwise
    String *name; // attribute name
    union {
        TypeDecl *tag; // type annotation
        Function fn; // method info
    };                  
} AttrStmt;

typedef struct ClassStmt {
    STMT_HEADER;
    Scope *scope;
    struct {
        paw_Bool global: 1; // uses 'global' keyword
    } flags;
    Expr *super;
    String *name;
    Stmt *attrs; // list of AttrStmt
    ClassType *cls;
    int nattrs;
} ClassStmt;

typedef struct ReturnStmt {
    STMT_HEADER;
    Function *fn;
    Expr *expr;
} ReturnStmt;

typedef struct ExprStmt {
    STMT_HEADER;
    Expr *lhs;
    Expr *rhs;
} ExprStmt;

typedef struct IfElseStmt {
    STMT_HEADER;
    Expr *cond;
    Stmt *then_arm;
    Stmt *else_arm;
} IfElseStmt;

typedef struct WhileStmt {
    STMT_HEADER;
    Expr *cond;
    Block *block;
} WhileStmt;

typedef struct LabelStmt {
    STMT_HEADER;
    LabelKind label;
} LabelStmt;

typedef struct ForIn {
    Expr *target;
} ForIn;

typedef struct ForNum {
    Expr *begin;
    Expr *end;
    Expr *step;
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

#define cast_node(x) ((Node *)(x))
#define cast_stmt(x) ((Stmt *)(x))
#define cast_expr(x) ((Expr *)(x))
#define cast_to(x, tp) ((tp *)(x))

void pawK_dump_ast(Lex *lex, FILE *out);

Node *pawK_add_node_aux(Lex *lex, unsigned type, size_t size, size_t align);
#define pawK_add_node(x, tt, tp) (tp *)pawK_add_node_aux(x, tt, sizeof(tp), _Alignof(tp))

Tree *pawK_new_ast(paw_Env *P);
void pawK_free_ast(paw_Env *P, Tree *ast);

typedef struct Visitor Visitor;

struct Visitor {
    Lex *lex; // lexical state
    Function *fn; // enclosing function

    void (*assign)(Visitor *V, Expr *lhs, Expr *rhs);

    void (*expr)(Visitor *V, Expr *expr);
    void (*expr_list)(Visitor *V, Expr *head);
    void (*primitive_expr)(Visitor *V, PrimitiveExpr *e);
    void (*literal_expr)(Visitor *V, LiteralExpr *e);
    void (*chain_expr)(Visitor *V, ChainExpr *e);
    void (*coalesce_expr)(Visitor *V, CoalesceExpr *e);
    void (*logical_expr)(Visitor *V, LogicalExpr *e);
    void (*unop_expr)(Visitor *V, UnOpExpr *e);
    void (*binop_expr)(Visitor *V, BinOpExpr *e);
    void (*call_expr)(Visitor *V, CallExpr *e);
    void (*cond_expr)(Visitor *V, CondExpr *e);
    void (*var_expr)(Visitor *V, VarExpr *e);
    void (*init_expr)(Visitor *V, InitExpr *e);
    void (*array_expr)(Visitor *V, ArrayExpr *e);
    void (*index_expr)(Visitor *V, IndexExpr *e);
    void (*access_expr)(Visitor *V, AccessExpr *e);
    void (*invoke_expr)(Visitor *V, InvokeExpr *e);

    void (*stmt)(Visitor *V, Stmt *stmt);
    void (*stmt_list)(Visitor *V, Stmt *head);
    void (*expr_stmt)(Visitor *V, ExprStmt *s);
    void (*return_stmt)(Visitor *V, ReturnStmt *s);
    void (*ifelse_stmt)(Visitor *V, IfElseStmt *s);
    void (*for_stmt)(Visitor *V, ForStmt *s);
    void (*while_stmt)(Visitor *V, WhileStmt *s);
    void (*dowhile_stmt)(Visitor *V, WhileStmt *s);
    void (*label_stmt)(Visitor *V, LabelStmt *s);
    void (*param_stmt)(Visitor *V, ParamStmt *s);
    void (*fn_stmt)(Visitor *V, FnStmt *s);
    void (*item_stmt)(Visitor *V, ItemStmt *s);
    void (*attr_stmt)(Visitor *V, AttrStmt *s);
    void (*class_stmt)(Visitor *V, ClassStmt *s);
    void (*def_stmt)(Visitor *V, DefStmt *s);
    void (*block_stmt)(Visitor *V, Block *b);
};

// Initialize a new visitor object
// Sets the default visitor routines, which do nothing but traverse the AST. Function
// pointers in 'V' can be replaced before 'pawK_visit' is called.
void pawK_init_visitor(Visitor *V, Lex *lex);

// visitor entrypoint
void pawK_visit(Visitor *V, Tree *tree);

void pawK_fix_line(FnState *fs, int line);

// Opcode output routines
void pawK_code_0(FnState *fs, Op op);
void pawK_code_S(FnState *fs, Op op, int s);
void pawK_code_U(FnState *fs, Op op, int u);
void pawK_code_AB(FnState *fs, Op op, int a, int b);

#endif // PAW_CODE_H
