// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// compile.h: compiler entrypoint
//
// The compiler converts source code into bytecode that can be run on Paw's
// virtual machine. It performs the following passes:
//
//  Name           | Input  | Output   | Purpose
// ----------------|--------|----------|----------------------------------------
//  parse          | source | AST      | syntactical analysis
//  lower_ast      | AST    | HIR      | convert AST into HIR
//  collect_types  | HIR    | HIR      | collect ADTs and function types
//  check_types    | HIR    | HIR      | type check function bodies
//  exhaustiveness | HIR    | HIR      | ensure pattern matching exhaustiveness
//  lower_hir      | HIR    | MIR      | convert HIR into MIR
//  monomorphize   | MIR    | MIR      | monomorphize functions
//  codegen        | MIR    | bytecode | generate code

#ifndef PAW_COMPILE_H
#define PAW_COMPILE_H

#include "code.h"
#include "debug.h"
#include "env.h"
#include "mem.h"
#include "paw.h"
#include "unify.h"

#define ENV(x) ((x)->P)
#define DLOG(X, ...) PAWD_LOG(ENV(X), __VA_ARGS__)
#define SCAN_STRING(X, s) pawP_scan_string(ENV(X), (X)->strings, s)
#define NAME_ERROR(X, ...) pawE_error(ENV(X), PAW_ENAME, (X)->line, __VA_ARGS__)
#define SYNTAX_ERROR(X, ...) pawE_error(ENV(X), PAW_ESYNTAX, (X)->line, __VA_ARGS__)
#define TYPE_ERROR(X, ...) pawE_error(ENV(X), PAW_ETYPE, (X)->line, __VA_ARGS__)

#define GET_NODE_TYPE(C, p) pawIr_get_type(C, (p)->hdr.hid)
#define SET_NODE_TYPE(C, p, t) pawIr_set_type(C, (p)->hdr.hid, t)

typedef struct DeclId {
    unsigned short modno;
    unsigned short value;
} DeclId;

#define MAX_MODULES 256
#define MAX_DECLS 10000

// TODO: make 1 the first ID, let 0 be uninit. remove this and use {0} initialization
#define NO_DECL (DeclId){MAX_MODULES, MAX_DECLS}

struct Hir;
struct HirPath;
struct HirDecl;
struct HirAdtDecl;
struct HirFuncDecl;
struct HirInstanceDecl;
struct HirMatchStmt;
struct HirTypeFolder;
struct HirTypeList;
struct HirSymtab;
struct HirType;

struct IrType;
struct IrTypeList;
struct IrTypeFolder;
struct IrSignature;
enum IrTypeKind;

struct MirBodyList;
struct MirBlockList;


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
    BUILTIN_LIST,
    BUILTIN_MAP,
    BUILTIN_OPTION,
    BUILTIN_RESULT,

    NBUILTINS,
};

struct Builtin {
    String *name;
    DeclId did;
};

struct Compiler {
    struct Builtin builtins[NBUILTINS];
    struct DynamicMem *dm;
    struct Pool *pool;
    struct Ast *prelude;
    struct Unifier *U;
    String *modname;

    // '.impls' maps each ADT to a list containing all of their 'impl' blocks.
    // Includes ADTs from all modules being compiled.
    Map *impls;

    // '.imports' maps modules names to ASTs for each module being compiled.
    Map *imports;

    // '.strings' anchors all strings used during compilation so they are not
    // collected by the GC.
    Map *strings;

    Map *method_contexts; // IrType * => IrType *
    Map *method_binders; // DeclId => IrTypeList *
    Map *ir_types; // HirId => IrType *
    Map *type2rtti;

    paw_Env *P;
    int nbinders;
    int nnodes;
};

paw_Bool pawP_is_assoc_fn(struct Compiler *C, struct IrSignature *type);
struct IrTypeList *pawP_get_binder(struct Compiler *C, DeclId did);
void pawP_set_binder(struct Compiler *C, DeclId did, struct IrTypeList *binder);
void pawP_set_self(struct Compiler *C, struct IrSignature *method, struct IrType *self);
struct IrType *pawP_get_self(struct Compiler *C, struct IrSignature *method);

enum JumpKind {
    JUMP_BREAK,
    JUMP_CONTINUE,
};

struct ModuleInfo {
    struct HirScope *globals;
    struct Hir *hir;
};

struct ModuleInfo *pawP_mi_new(struct Compiler *C, struct Hir *hir);

DEFINE_LIST_V2(struct Compiler, pawP_mod_list_, ModuleList, struct ModuleInfo *)

// Keeps track of dynamic memory used by the compiler
struct DynamicMem {
    struct Pool pool;

    // Buffer for accumulating strings
    struct CharVec {
        char *data;
        int count;
        int alloc;
    } scratch;

    struct Unifier unifier;

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
    struct MirVisitor *V;
    struct RttiList *temp_rtti;
    struct JumpTable *jumps;
    struct PatchList *patch;
    struct ItemList *items;
    struct Pool *pool;
    struct Mir *mir;
    Map *builtin;
    Map *regtab;
    paw_Env *P;
    int nvals;
} Generator;

struct RegisterInfo {
    int ident;
    int index;
};

void pawP_lower_ast(struct Compiler *C);
void pawP_collect_items(struct Compiler *C);

struct IrType *pawP_lower_type(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirType *type);
struct IrTypeList *pawP_lower_type_list(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirTypeList *types);

Map *pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, int *pmax_reg);
struct Mir *pawP_lower_hir_body(struct Compiler *C, struct HirFuncDecl *func);
Map *pawP_lower_hir(struct Compiler *C);

struct IrType *pawP_attach_type(struct Compiler *C, DeclId did, enum IrTypeKind kind, int line);

struct IrTypeList *pawP_instantiate_impl_types(struct Compiler *C, struct IrType *self, struct IrTypeList *generics, struct IrTypeList *types);
struct IrType *pawP_instantiate_field(struct Compiler *C, struct IrType *self, struct HirDecl *field);
struct HirDecl *pawP_find_field(struct Compiler *C, struct IrType *self, String *name);
struct IrType *pawP_find_method(struct Compiler *C, struct IrType *self, String *name);

struct Decision *pawP_check_exhaustiveness(struct Compiler *C, struct HirMatchStmt *match);
void pawP_lower_matches(struct Compiler *C);

struct IrType *pawP_generalize(struct Compiler *C, struct IrType *type);

// Instantiate a polymorphic function or type
// Expects that 'decl' is already resolved, meaning the type of each symbol has been
// filled in. Works by replacing each generic type with the corresponding concrete
// type from the given 'types'. Returns a HirInstanceDecl if 'decl' is a function,
// and 'decl' otherwise. We avoid recursively visiting the function body here, since
// doing so might cause further instantiations due to the presence of recursion.
// Function instance bodies are expanded in a separate pass.
struct IrType *pawP_instantiate(
        struct Compiler *C,
        struct HirDecl *decl,
        struct IrTypeList *types);

struct HirDecl *pawP_instantiate_impl(
        struct Compiler *C,
        struct HirDecl *decl,
        struct IrTypeList *types);

struct IrType *pawP_instantiate_method(struct Compiler *C, struct HirDecl *base, struct IrTypeList *types, struct HirDecl *method);

struct Substitution {
    struct IrTypeList *generics;
    struct IrTypeList *types;
    struct Compiler *C;
};

void pawP_init_substitution_folder(struct IrTypeFolder *F, struct Compiler *C, struct Substitution *subst,
                                   struct IrTypeList *generics, struct IrTypeList *types);

void pawP_collect_imports(struct Compiler *C, struct Ast *ast);
void pawP_import(struct Compiler *C, void *state);

struct ItemList *pawP_define_all(struct Compiler *C, struct ModuleList *modules, int *poffset);

struct ItemList *pawP_allocate_defs(struct Compiler *C, struct MirBodyList *bodies, struct IrTypeList *types);

enum LookupKind {
    LOOKUP_TYPE,
    LOOKUP_VALUE,
    LOOKUP_EITHER,
};

// Determine which type the 'path' refers to, relative to the current module 'm'
struct IrType *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind);

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, const char *modname);
void pawP_teardown(paw_Env *P, struct DynamicMem *dm);

struct Ast *pawP_parse_prelude(struct Compiler *C);
struct Ast *pawP_parse_module(struct Compiler *C, String *modname, paw_Reader input, void *ud);

struct MonoResult {
    struct IrTypeList *types;
    struct MirBodyList *bodies;
};

struct MonoResult pawP_monomorphize(struct Compiler *C, Map *bodies);

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

paw_Type pawP_type2code(struct Compiler *C, struct IrType *type);

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
    struct Type *rtti;
    struct Mir *mir;
    String *name;
};

struct ItemSlot *pawP_new_item_slot(struct Compiler *C, struct Mir *mir, struct Type *type);

DEFINE_LIST_V2(struct Compiler, pawP_item_list_, ItemList, struct ItemSlot *)

Map *pawP_push_map(struct Compiler *C);
List *pawP_push_list(struct Compiler *C);

static inline void pawP_pop_object(struct Compiler *C, void *ptr)
{
    // make sure nothing else was left on the stack
    paw_assert(ENV(C)->top.p[-1].p == ptr);
    PAW_UNUSED(ptr);

    --ENV(C)->top.p;
}

#endif // PAW_COMPILE_H
