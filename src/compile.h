// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// compile.h: compiler entrypoint
//
// The compiler converts source code into bytecode that can be run on Paw's
// virtual machine. It performs the following passes:
//
//    name           | input  | output   | purpose
//   ----------------|--------|----------|----------------------------------------
//    parse          | source | AST      | syntactical analysis
//    lower_ast      | AST    | HIR      | convert AST into HIR
//    collect_types  | HIR    | HIR      | collect ADTs and toplevel functions
//    check_types    | HIR    | HIR      | type check function bodies
//    exhaustiveness | HIR    | HIR      | ensure pattern matching exhaustiveness
//    lower_hir      | HIR    | MIR      | convert HIR into MIR
//    monomorphize   | MIR    | MIR      | monomorphize functions
//    codegen        | MIR    | bytecode | generate code
//
// Activation frame layout (ranges are half-open):
//
//    start   | size | name      | purpose
//   ---------|------|-----------|-------------------------------------
//    0       | 1    | result    | storage for return value
//    1       | a    | arguments | arguments passed to function
//    1+a     | u    | upvalues  | upvalues captured in local closures
//    1+a+u   | w    | workspace | space for locals and temporaries
//    1+a+u+w | s    | scratch   | extra per-instruction temporaries
//

#ifndef PAW_COMPILE_H
#define PAW_COMPILE_H

#include "code.h"
#include "debug.h"
#include "env.h"
#include "map.h"
#include "mem.h"
#include "paw.h"
#include "trait.h"

#define ENV(x) ((x)->P)
#define DLOG(X, ...) PAWD_LOG(ENV(X), __VA_ARGS__)
#define SCAN_STRING(X, s) pawP_scan_string(ENV(X), (X)->strings, s)
#define NAME_ERROR(X, ...) pawE_error(ENV(X), PAW_ENAME, (X)->line, __VA_ARGS__)
#define SYNTAX_ERROR(X, ...) pawE_error(ENV(X), PAW_ESYNTAX, (X)->line, __VA_ARGS__)
#define TYPE_ERROR(X, ...) pawE_error(ENV(X), PAW_ETYPE, (X)->line, __VA_ARGS__)

#define GET_NODE_TYPE(C, p) pawIr_get_type(C, (p)->hdr.hid)
#define SET_NODE_TYPE(C, p, t) pawIr_set_type(C, (p)->hdr.hid, t)

typedef struct DefId {
    unsigned short modno;
    unsigned short value;
} DefId;

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
struct HirMatchExpr;
struct HirTypeFolder;
struct HirTypeList;
struct HirSymtab;
struct HirType;

struct IrType;
struct IrTypeList;
struct IrTypeFolder;
struct IrSignature;
enum IrTypeKind;

struct Mir;
struct MirIntervalList;
struct MirLocationList;
struct MirBodyList;
struct MirBlockList;
struct VariableList;

void *pawP_pool_alloc(paw_Env *P, struct Pool *pool, size_t size);

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

#define IS_BASIC_TYPE(code) ((code) <= BUILTIN_STR)
#define IS_BUILTIN_TYPE(code) ((code) < NBUILTINS)

struct Builtin {
    String *name;
    DeclId did;
};

struct ObjectStore {
    Map *objects;
    int offset;
};

struct Compiler {
    struct Builtin builtins[NBUILTINS];
    struct ObjectStore store;
    struct ModuleList *modules;
    struct HirDeclList *decls;
    struct DynamicMem *dm;
    struct Pool *pool;
    struct Ast *prelude;
    struct Unifier *U;
    String *modname;

    MapPolicy type_policy;

    // '.traits' maps each ADT to a list of implemented traits. Includes ADTs
    // from all modules being compiled.
    Map *traits; // DeclId => IrTypeList *

    // '.imports' maps modules names to ASTs for each module being compiled.
    Map *imports; // String * => Ast

    // '.strings' anchors all strings used during compilation so they are not
    // collected by the GC.
    Map *strings;

    Map *method_contexts; // IrType * => IrType *
    Map *method_binders; // DeclId => IrTypeList *
    Map *type2rtti;

    Map *ir_types; // HirId => IrType *
    Map *ir_defs; // DefId => IrDef *

    paw_Env *P;
    int hir_count;
    int def_count;
    int line;
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

DEFINE_LIST(struct Compiler, pawP_mod_list_, ModuleList, struct ModuleInfo *)

// Keeps track of dynamic memory used by the compiler
struct DynamicMem {
    struct Pool pool;

    // buffer for accumulating strings
    struct CharVec {
        char *data;
        int count;
        int alloc;
    } scratch;
};

void pawP_lower_ast(struct Compiler *C);
void pawP_collect_items(struct Compiler *C);

struct IrType *pawP_lower_type(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirType *type);
struct IrTypeList *pawP_lower_type_list(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirTypeList *types);

struct RegisterInfo {
    int value;
};

DEFINE_LIST(struct Compiler, regtab_, RegisterTable, struct RegisterInfo)

typedef unsigned long long BitChunk;
DEFINE_LIST(struct Compiler, raw_bitset_, BitSet, BitChunk)

struct BitSet *pawP_bitset_new(struct Compiler *C, int count);
int pawP_bitset_count(const struct BitSet *bs);
paw_Bool pawP_bitset_get(const struct BitSet *bs, int i);
void pawP_bitset_set(struct BitSet *bs, int i);
void pawP_bitset_set_range(struct BitSet *bs, int i, int j);
void pawP_bitset_clear(struct BitSet *bs, int i);
void pawP_bitset_clear_range(struct BitSet *bs, int i, int j);
void pawP_bitset_and(struct BitSet *a, const struct BitSet *b);
void pawP_bitset_or(struct BitSet *a, const struct BitSet *b);


struct RegisterTable *pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirIntervalList *intervals, struct MirLocationList *locations, int *pmax_reg);
struct Mir *pawP_lower_hir_body(struct Compiler *C, struct HirFuncDecl *func);
Map *pawP_lower_hir(struct Compiler *C);

struct IrType *pawP_attach_type(struct Compiler *C, DeclId did, enum IrTypeKind kind, int line);

struct IrTypeList *pawP_instantiate_typelist(struct Compiler *C, struct IrTypeList *before, struct IrTypeList *after, struct IrTypeList *target);
struct IrType *pawP_instantiate_field(struct Compiler *C, struct IrType *self, struct HirDecl *field);
struct HirDecl *pawP_find_field(struct Compiler *C, struct IrType *self, String *name);
struct IrType *pawP_find_method(struct Compiler *C, struct IrType *self, String *name);

struct Decision *pawP_check_exhaustiveness(struct Compiler *C, struct HirMatchExpr *match, struct VariableList *vars);
void pawP_lower_matches(struct Compiler *C);

struct IrType *pawP_generalize(struct Compiler *C, struct IrType *type);
struct IrType *pawP_generalize_self(struct Compiler *C, struct IrType *self, struct IrTypeList *base_binder, struct IrTypeList **pinst_binder);

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

struct ItemList *pawP_allocate_defs(struct Compiler *C, struct MirBodyList *bodies, struct IrTypeList *types);

enum LookupKind {
    LOOKUP_TYPE,
    LOOKUP_VALUE,
    LOOKUP_EITHER,
};

struct IrType *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind, paw_Bool is_annotation);
struct IrType *pawP_lookup_trait(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path);

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, const char *modname);
void pawP_teardown(paw_Env *P, struct DynamicMem *dm);

struct Ast *pawP_parse_prelude(struct Compiler *C);
struct Ast *pawP_parse_module(struct Compiler *C, String *modname, paw_Reader input, void *ud);

void pawP_validate_adt_traits(struct Compiler *C, struct HirAdtDecl *d);

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

    // transform AST -> HIR
    pawP_lower_ast(C);

    // determine the type of each toplevel item in each module (allows the type checker to
    // resolve paths between modules immediately)
    pawP_collect_items(C);

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

enum BuiltinKind pawP_type2code(struct Compiler *C, struct IrType *type);

struct ItemSlot {
    struct Type *rtti;
    struct Mir *mir;
    String *name;
};

DEFINE_LIST(struct Compiler, pawP_item_list_, ItemList, struct ItemSlot)

void pawP_push_store(struct Compiler *C, struct ObjectStore *store);

static inline void pawP_pop_store(struct Compiler *C, struct ObjectStore *store)
{
    // make sure nothing else was left on the stack
    paw_assert(ENV(C)->top.p[-1].p == store->objects);
    PAW_UNUSED(store);

    --ENV(C)->top.p;
}

Map *pawP_new_map(struct Compiler *C, struct ObjectStore *store);

#define MAP_KEY(map, index) pawH_key(map, index)
#define MAP_VALUE(map, index) pawH_value(map, index)
#define MAP_INSERT(X, map, key, value) pawH_insert(ENV(X), map, key, value)
#define MAP_REMOVE(map, key) pawH_erase(map, key)
#define MAP_ERASE(map, index) pawH_erase_at(map, index)
#define MAP_GET(map, key) pawH_get(map, key)
#define MAP_CONTAINS(map, key) (MAP_GET(map, key) != NULL)

Map *pawP_push_map(struct Compiler *C);

static inline void pawP_pop_object(struct Compiler *C, void *ptr)
{
    // make sure nothing else was left on the stack
    paw_assert(ENV(C)->top.p[-1].p == ptr);
    PAW_UNUSED(ptr);

    --ENV(C)->top.p;
}

#endif // PAW_COMPILE_H
