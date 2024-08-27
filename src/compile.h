// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// compile.h: compiler entrypoint
//
// The compiler converts source code into bytecode that can be run in Paw's
// virtual machine. It works in 4 major passes:
//
//  Name    | Input -> Output | Purpose                    
// ---------|-----------------|--------------------------- 
//  parse   | source -> AST   | syntactical analysis 
//  resolve | AST -> HIR      | resolve symbols and types 
//  expand  | HIR -> HIR      | expand polymorphic instances 
//  codegen | HIR -> bytecode | generate code 

#ifndef PAW_COMPILE_H
#define PAW_COMPILE_H

#include "code.h"
#include "env.h"
#include "mem.h"
#include "unify.h"

#define ENV(x) ((x)->P)
#define SCAN_STRING(x, s) pawP_scan_string(ENV(x), (x)->strings, s)

struct HirTypeList;

String *pawP_scan_nstring(paw_Env *P, Map *st, const char *s, size_t n);
static inline String *pawP_scan_string(paw_Env *P, Map *st, const char *s)
{
    return pawP_scan_nstring(P, st, s, strlen(s));
}

// ORDER UnaryOp
enum UnaryOp {
    UNARY_LEN, 
    UNARY_NEG, 
    UNARY_NOT, 
    UNARY_BNOT,

    NUNARYOPS
};

// ORDER BinaryOp
enum BinaryOp {
    BINARY_EQ,   
    BINARY_NE,   
    BINARY_LT,   
    BINARY_LE,   
    BINARY_GT,   
    BINARY_GE,   
    BINARY_AS,
    BINARY_ADD,  
    BINARY_SUB,  
    BINARY_MUL,  
    BINARY_DIV,  
    BINARY_MOD,  
    BINARY_BXOR,
    BINARY_BAND,
    BINARY_BOR,
    BINARY_SHL,
    BINARY_SHR,

    NBINARYOPS
};

// ORDER BuiltinKind
enum BuiltinKind {
    BUILTIN_UNIT,
    BUILTIN_BOOL,
    BUILTIN_INT,
    BUILTIN_FLOAT,
    BUILTIN_STR,

    FIRST_BUILTIN_ADT,
    BUILTIN_LIST = FIRST_BUILTIN_ADT,
    BUILTIN_MAP,
    BUILTIN_OPTION,
    BUILTIN_RESULT,

    NBUILTINS,
};

struct Builtin {
    String *name;
    DefId did;
};

struct Compiler {
    Pool pool;
    struct Builtin builtins[NBUILTINS];
    struct DynamicMem *dm;
    String *modname;
    Closure *main;
    Map *strings;
    paw_Env *P;
};

// Common state for type-checking routines
struct Resolver {
    paw_Env *P;
    Map *strings;
    struct Unifier *U; // unification tables
    struct Compiler *C; // compiler state
    struct Ast *ast; // AST being resolved
    struct Hir *hir; // HIR being built
    struct HirType *adt; // enclosing ADT
    struct HirType *result; // enclosing function return type
    struct HirSymtab *symtab; // scoped symbol table
    struct DynamicMem *dm; // dynamic memory
    int func_depth; // number of nested functions
    int nresults;
    int list_gid;
    int map_gid;
    int line;
    DefId option_did;
    DefId result_did;
    paw_Bool in_closure; // 1 if the enclosing function is a closure, else 0
};

// Keeps track of dynamic memory used by the compiler
struct DynamicMem {
    // Buffer for accumulating strings
    struct CharVec {
        char *data;
        int count;
        int alloc;
    } scratch;

    struct {
        struct LocalSlot *data;
        int count;
        int alloc;
    } vars;

    struct Ast *ast;
    struct Hir *hir;

    Unifier unifier;
    struct LabelList labels;

    // NOTE: Backing storage for this field is located in the HIR pool, so
    //       '.decls' does not need to be freed separately. It is kept here
    //       for convenience, since it is used during multiple passes.
    struct HirDeclList *decls;
};

typedef struct Generator {
    struct Compiler *C;
    struct HirSymtab *sym;
    struct HirScope *globals;
    struct Hir *hir;
    struct FuncState *fs;
    struct ToplevelList *items;
    paw_Env *P;
    int nvals;
} Generator;

// Instantiate a polymorphic function or type
// Expects that 'decl' is already resolved, meaning the type of each symbol has been
// filled in. Works by replacing each generic type with the corresponding concrete 
// type from the given 'types'. Returns a HirInstanceDecl if 'decl' is a function, 
// and a HirAdtDecl otherwise. We avoid recursively visiting the function body here, 
// since doing so might cause further instantiations due to the presence of recursion. 
// Function instance bodies are expanded in a separate pass.
struct HirDecl *pawP_instantiate(
        struct Resolver *R, 
        struct HirDecl *decl, 
        struct HirTypeList *types);

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, const char *modname);
void pawP_teardown(paw_Env *P, const struct DynamicMem *dm);

struct Ast *pawP_parse(struct Compiler *C, paw_Reader input, void *ud);
struct Hir *pawP_resolve(struct Compiler *C, struct Ast *ast);
void pawP_codegen(struct Compiler *C, struct Hir *hir);

static inline void pawP_compile(struct Compiler *C, paw_Reader input, void *ud)
{
    // compile the module (source -> AST -> HIR -> bytecode)
    struct Ast *ast = pawP_parse(C, input, ud);
    struct Hir *hir = pawP_resolve(C, ast);
    pawP_codegen(C, hir);
}

paw_Type pawP_type2code(struct Compiler *C, struct HirType *type);

#endif // PAW_COMPILE_H
