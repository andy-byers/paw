// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "opcode.h"
#include "parse.h"
#include "paw.h"

void pawK_fix_line(FnState *fn, int line);

// Opcode output routines
void pawK_code_0(FnState *fn, Op op);
void pawK_code_S(FnState *fn, Op op, int s);
void pawK_code_U(FnState *fn, Op op, int u);
void pawK_code_AB(FnState *fn, Op op, int a, int b);

typedef enum ExprType {
    EXPR_FN,
    EXPR_VAR,
    EXPR_PRIMITIVE,
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
    EXPR_FIELD,
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
    STMT_FN,
    STMT_DEF,
    STMT_PARAM,
    STMT_RETURN,
} StmtType;

#define NODE_HEADER \
    int line;       \
    uint8_t kind
#define STMT_HEADER NODE_HEADER
#define EXPR_HEADER \
    NODE_HEADER;    \
    const Type *type

typedef struct Node {
    NODE_HEADER;
} Node;

typedef struct Expr {
    EXPR_HEADER;
} Expr;

typedef struct Stmt {
    STMT_HEADER;
} Stmt;

typedef struct Arena {
    struct Arena *prev;
    size_t used;
    size_t size;

    // Must be aligned to at least the strictest alignment required
    // by a Node.
    _Alignas(void *) char data[];
} Arena;

// Linked list of NodeVec objects
// Used to avoid traversal of the AST during cleanup.
typedef struct VecList {
    struct VecList *prev;
    struct NodeVec *nvec;
} VecList;

// Vector for storing AST nodes
// Since we cannot tell if a given node is an Expr or a Stmt, this
// container is only capable of storing one or the other. The user
// must know from context which type is stored.
typedef struct NodeVec {
    VecList list;
    Node **nodes;
    int size;
    int alloc;
} NodeVec;

#define link_nvec(x, pv) ((pv)->list.nvec = (pv), \
                          (pv)->list.prev = (x)->ast->vecs, \
                          (x)->ast->vecs = &(pv)->list)

typedef struct Tree {
    Arena *arena;
    Node *root;
    VecList *vecs;
} Tree;

typedef struct Block {
    STMT_HEADER;
    NodeVec stmts;
} Block;

typedef enum VarKind {
    VAR_UNDEF,
    VAR_GLOBAL,
    VAR_UPVALUE,
    VAR_LOCAL,
} VarKind;

typedef struct VarInfo {
    const Type *type;
    enum VarKind kind;
    int index;
} VarInfo;

typedef struct UnOpExpr { 
    EXPR_HEADER; 
    UnaryOp op;
    Expr *target; 
} UnOpExpr;

#define make_binary_expr(a, b) \
    typedef struct a ## Expr { \
        EXPR_HEADER; \
        b \
        Expr *lhs; \
        Expr *rhs; \
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
    NodeVec param;
} CallExpr;

typedef struct IndexExpr {
    SUFFIXED_HEADER; // common fields
    Expr *first; // index/key or slice beginning
    Expr *second; // slice end, or NULL
} IndexExpr;

typedef struct FieldExpr {
    SUFFIXED_HEADER; // common fields
    uint8_t call; // 1 if an invocation, 0 otherwise
    uint16_t index; // index in constants table
    String *name; // field name
} FieldExpr;

typedef struct VarExpr {
    EXPR_HEADER;
    String *name;
} VarExpr;

#define CONTAINER_HEADER EXPR_HEADER; const Type *t
typedef struct ContainerExpr {
    CONTAINER_HEADER;
} ContainerExpr;

typedef struct ArrayExpr {
    CONTAINER_HEADER;
    NodeVec items;
} ArrayExpr;

typedef struct MapExpr {
    CONTAINER_HEADER;
    const Type *t1;
    const Type *t2;
    NodeVec items;
} MapExpr;

typedef struct PrimitiveExpr {
    EXPR_HEADER;
    Value v;
} PrimitiveExpr;

#define DEF_MUTABLE 1
#define DEF_GLOBAL 2

typedef struct DefStmt {
    STMT_HEADER;
    uint8_t flags; // bitwise OR of DEF_* flags
    const Type *tag; // type information
    String *name; // variable name
    Expr *init; // initial value
} DefStmt;

typedef struct ParamStmt {
    STMT_HEADER;
    const Type *tag; // type information
    String *name; // variable name
} ParamStmt;

typedef struct FnStmt {
    STMT_HEADER;
    uint8_t global;
    uint8_t flags;
    String *name;
    NodeVec param;
    const Type *ret; // type of return value
    Block *body;
} FnStmt;

typedef struct FnExpr {
    EXPR_HEADER;
    NodeVec param;
    const Type *ret; // type of return value
    Block *body;
} FnExpr;

typedef struct ReturnStmt {
    STMT_HEADER;
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

typedef struct ForNumStmt {
    STMT_HEADER;
    String *name;
    Expr *begin;
    Expr *end;
    Expr *step;
    Block *block;
} ForNumStmt;

typedef struct ForInStmt {
    STMT_HEADER;
    String *name;
    Expr *target;
    Block *block;
} ForInStmt;

#define cast_node(x) ((Node *)(x))
#define cast_stmt(x) ((Stmt *)(x))
#define cast_expr(x) ((Expr *)(x))
#define cast_to(x, tp) ((tp *)(x))

#define emplace_node(x, L, tt, tp) ((L)->next = pawK_add_node(x, tt, tp))

#define push_node(x, nv, v) (*pawK_nvec_add(env(x), nv) = (Node *)(v))

void pawK_nvec_free(paw_Env *P, NodeVec *nvec);
Node **pawK_nvec_add(paw_Env *P, NodeVec *nvec);

void pawK_dump_ast(Lex *lex, FILE *out);

Node *pawK_add_node_aux(Lex *lex, unsigned type, size_t size, size_t align);
#define pawK_add_node(x, tt, tp) (tp *)pawK_add_node_aux(x, tt, sizeof(tp), _Alignof(tp))

Tree *pawK_new_ast(paw_Env *P);
void pawK_free_ast(paw_Env *P, Tree *ast);

#endif // PAW_CODE_H
