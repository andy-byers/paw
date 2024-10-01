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
//  expand  | HIR -> HIR      | monomorphize functions 
//  codegen | HIR -> bytecode | generate code 

#ifndef PAW_COMPILE_H
#define PAW_COMPILE_H

#include "code.h"
#include "env.h"
#include "mem.h"
#include "unify.h"

#define ENV(x) ((x)->P)
#define SCAN_STRING(x, s) pawP_scan_string(ENV(x), (x)->strings, s)

struct HirPath;
struct HirDecl;
struct HirAdtDecl;
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

typedef struct HirDecl *(*Instantiate)(struct Hir *, struct HirDecl *, struct HirTypeList *);

struct Compiler {
    struct Builtin builtins[NBUILTINS];
    struct DynamicMem *dm;
    struct Pool *pool;
    struct Ast *prelude;
    Instantiate finst;
    String *modname;
    Map *impls; // HirAdtDecl * => HirImplDecl *
    Map *imports;
    Map *strings;
    Map *types;
    paw_Env *P;
    int nbinders;
};

// Common state for type-checking routines
struct Resolver {
    paw_Env *P;
    Map *strings;
    struct ModuleInfo *m;
    struct Unifier *U; // unification tables
    struct Compiler *C; // compiler state
    struct HirType *adt; // enclosing ADT
    struct DynamicMem *dm; // dynamic memory
    struct ResultState *rs;
    struct HirSymtab *symtab;
    Map *impls; // '.impls' from Compiler
    int func_depth; // number of nested functions
    int line;
    paw_Bool in_closure; // 1 if the enclosing function is a closure, else 0
};

struct ModuleInfo {
    struct HirScope *globals;
    struct Hir *hir;
};

struct ModuleInfo *pawP_mi_new(struct Compiler *C, struct Hir *hir);

DEFINE_LIST(struct Compiler, pawP_mod_list_, ModuleList, struct ModuleInfo)

// Keeps track of dynamic memory used by the compiler
struct DynamicMem {
    struct Pool pool;

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

    struct Unifier unifier;
    struct LabelList labels;

    // NOTE: Backing storage for these fields are located in the compiler 
    //       memory pool, so they don't need to be freed separately. They 
    //       are kept here for convenience, since they are used during 
    //       multiple passes.
    struct ModuleList *modules;
    struct HirDeclList *decls;
};

typedef struct Generator {
    struct Compiler *C;
    struct ModuleInfo *m;
    struct FuncState *fs;
    struct ItemList *items;
    struct Pool *pool;
    paw_Env *P;
    int nvals;
} Generator;

void pawP_lower_ast(struct Compiler *C);
void pawP_collect_items(struct Compiler *C);

struct HirDecl *pawP_find_field(struct Compiler *C, struct ModuleInfo *m, struct HirAdtDecl *adt, String *name);
struct HirDecl *pawP_find_method(struct Compiler *C, struct ModuleInfo *m, struct HirType *self, String *name);

struct Generalization {
    struct HirTypeList *types;
    struct HirTypeList *fields;
    struct HirType *result;
};

struct Generalization pawP_generalize(
        struct Hir *hir, 
        struct HirDeclList *generics, 
        struct HirDeclList *fields);

// Instantiate a polymorphic function or type
// Expects that 'decl' is already resolved, meaning the type of each symbol has been
// filled in. Works by replacing each generic type with the corresponding concrete 
// type from the given 'types'. Returns a HirInstanceDecl if 'decl' is a function, 
// and 'decl' otherwise. We avoid recursively visiting the function body here, since 
// doing so might cause further instantiations due to the presence of recursion. 
// Function instance bodies are expanded in a separate pass.
struct HirDecl *pawP_instantiate(
        struct Hir *hir, 
        struct HirDecl *decl, 
        struct HirTypeList *types);

void pawP_set_instantiate(struct Compiler *C, paw_Bool full);

void pawP_collect_imports(struct Compiler *C, struct Ast *ast);

struct ItemList *pawP_define_all(struct Compiler *C, struct ModuleList *modules, int *poffset);

// Determine which toplevel declaration the 'path' refers to, relative to the
// current module 'm'
struct HirDecl *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirPath *path);

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, const char *modname);
void pawP_teardown(paw_Env *P, struct DynamicMem *dm);

struct Ast *pawP_parse_prelude(struct Compiler *C);
struct Ast *pawP_parse_module(struct Compiler *C, String *modname, paw_Reader input, void *ud);
void pawP_resolve(struct Compiler *C);
void pawP_codegen(struct Compiler *C);

static inline void pawP_compile(struct Compiler *C, paw_Reader input, void *ud)
{
    pawP_parse_prelude(C);

    // parse an AST for the target module, as well as each imported module
    struct Ast *ast = pawP_parse_module(C, C->modname, input, ud);
    pawP_collect_imports(C, ast);

    // run the type checker, then codegen
    pawP_resolve(C);
    pawP_codegen(C);
}

static inline void pawP_pool_init(struct Compiler *C, struct Pool *pool)
{
    pawK_pool_init(ENV(C), pool, 512, 8);
}

static inline void pawP_pool_uninit(struct Compiler *C, struct Pool *pool)
{
    pawK_pool_uninit(ENV(C), pool);
}

paw_Type pawP_type2code(struct Compiler *C, struct HirType *type);

enum VarKind {
    VAR_GLOBAL,
    VAR_UPVALUE,
    VAR_LOCAL,
    VAR_FIELD,
    VAR_CFUNC,
};

struct VarInfo {
    enum VarKind kind;
    int index;
};

struct ItemSlot {
    struct VarInfo info;
    struct HirDecl *decl;
    struct ModuleInfo *m;
    String *name;
};

struct ItemSlot *pawP_new_item_slot(struct Compiler *C, struct HirDecl *decl, struct ModuleInfo *m);

DEFINE_LIST(struct Compiler, pawP_item_list_, ItemList, struct ItemSlot)

#endif // PAW_COMPILE_H
