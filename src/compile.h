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
#include "source.h"
#include "stats.h"
#include "trait.h"

#define ENV(x) ((x)->P)
#define DLOG(X, ...) PAWD_LOG(ENV(X), __VA_ARGS__)
#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))
#define SCAN_STRING(X, s) pawP_scan_string(X, (X)->strings, s)
#define NAME_ERROR(X, ...) pawE_error(ENV(X), PAW_ENAME, (X)->line, __VA_ARGS__)
#define SYNTAX_ERROR(X, ...) pawE_error(ENV(X), PAW_ESYNTAX, (X)->line, __VA_ARGS__)
#define TYPE_ERROR(X, ...) pawE_error(ENV(X), PAW_ETYPE, (X)->line, __VA_ARGS__)
#define VALUE_ERROR(X, line, ...) pawE_error(ENV(X), PAW_EVALUE, line, __VA_ARGS__)

#define NAME_ERROR_(C_, Modname_, Loc_, ...) pawP_error(C_, PAW_ENAME, Modname_, Loc_, __VA_ARGS__)
#define SYNTAX_ERROR_(C_, Modname_, Loc_, ...) pawP_error(C_, PAW_ESYNTAX, Modname_, Loc_, __VA_ARGS__)
#define TYPE_ERROR_(C_, Modname_, Loc_, ...) pawP_error(C_, PAW_ETYPE, Modname_, Loc_, __VA_ARGS__)
#define VALUE_ERROR_(C_, Modname_, Loc_, ...) pawP_error(C_, PAW_EVALUE, Modname_, Loc_, __VA_ARGS__)

#define GET_NODE_TYPE(C, p) pawIr_get_type(C, (p)->hdr.hid)
#define SET_NODE_TYPE(C, p, t) pawIr_set_type(C, (p)->hdr.hid, t)

#define PRIVATE(Name_) ("(" Name_ ")")

#define MAX_MODULES 256
#define MAX_DECLS 10000

// TODO: make 1 the first ID, let 0 be uninit. remove this and use {0} initialization
#define NO_DECL \
    (DeclId) { MAX_MODULES, MAX_DECLS }

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

struct Mir;
struct MirIntervalList;
struct MirLocationList;
struct MirBodyList;
struct MirBlockList;
struct VariableList;

struct StringMap;
struct BodyList;
struct BodyMap;

void *pawP_alloc(paw_Env *P, struct Pool *pool, void *ptr, size_t size0, size_t size);
#define P_ALLOC(C, ptr, size0, size) pawP_alloc(ENV(C), (C)->pool, ptr, size0, size)

String *pawP_scan_nstring(struct Compiler *C, Tuple *map, char const *s, size_t n);
inline static String *pawP_scan_string(struct Compiler *C, Tuple *map, char const *s)
{
    return pawP_scan_nstring(C, map, s, strlen(s));
}

#define IS_BASIC_TYPE(code) ((code) <= BUILTIN_STR)
#define IS_BUILTIN_TYPE(code) ((code) < NBUILTINS)

struct Builtin {
    String *name;
    DeclId did;
};

struct GlobalInfo {
    enum BuiltinKind b_kind : 8;
    String *name;
    Value value;
    int modno;
    int index;
};

typedef struct Map Map;

struct Compiler {
    struct Builtin builtins[NBUILTINS];
    struct BuiltinMap *builtin_lookup;

    // callbacks for debugging
    Value on_build_ast;
    Value on_build_hir;
    Value on_build_mir;
    Value report_stats;

    struct Statistics *stats;
    struct PoolStats aux_stats;
    struct ModuleList *modules;
    struct HirDeclList *decls;
    struct DynamicMem *dm;
    struct Ast *ast_prelude;
    struct Hir *hir_prelude;
    struct Unifier *U;
    String *modname;

    struct Pool *pool;
    struct Pool *ast_pool;
    struct Pool *hir_pool;
    struct Pool *mir_pool;

    // '.strings' anchors all strings used during compilation so they are not
    // collected by the GC.
    Tuple *strings;

    // '.traits' maps each ADT to a list of implemented traits. Includes ADTs
    // from all modules being compiled.
    struct TraitMap *traits; // DeclId => HirDeclList *

    // '.imports' maps modules names to ASTs for each module being compiled.
    struct ImportMap *imports; // String * => Ast
    struct RttiMap *rtti;

    struct HirTypeMap *hir_types; // HirId => IrType *
    struct DefTypeMap *def_types; // DefId => IrType *
    struct VariantDefMap *variant_defs; // DefId => IrVariantDef *
    struct AdtDefMap *adt_defs; // DefId => IrAdtDef *
    struct FnDefMap *fn_defs; // DefId => IrFnDef *

    // map for quickly determining the methods implementing a given builtin
    // trait for a given type
    struct TraitOwners *trait_owners;

    struct GlobalList *globals;

    paw_Env *P;
    int hir_count;
    int def_count;
    int line;
};

_Noreturn void pawP_error(struct Compiler *C, int kind, String const *modname, struct SourceLoc loc, char const *fmt, ...);

paw_Bool pawP_push_callback(struct Compiler *C, char const *name);

paw_Bool pawP_is_assoc_fn(struct Compiler *C, struct IrSignature *type);
struct IrTypeList *pawP_get_binder(struct Compiler *C, DeclId did);
void pawP_set_binder(struct Compiler *C, DeclId did, struct IrTypeList *binder);
void pawP_set_self(struct Compiler *C, struct IrSignature *method, struct IrType *self);
struct IrType *pawP_get_self(struct Compiler *C, struct IrSignature *method);

struct ModuleInfo {
    struct HirScope *globals;
    struct Hir *hir;
    String *name;
    int modno;
};

struct ModuleInfo *pawP_mi_new(struct Compiler *C, struct Hir *hir);
void pawP_mi_delete(struct Compiler *C, struct ModuleInfo *m);

DEFINE_LIST(struct Compiler, ModuleList, struct ModuleInfo *)

// Keeps track of dynamic memory used by the compiler
struct DynamicMem {
    struct Pool pool;

    // buffer for accumulating strings
    struct StringBuffer {
        char *data;
        int count;
        int alloc;
    } scratch;

    // buffer for storing source text
    struct SourceBuffer {
        char *data;
        size_t size;
    } source;
};

void pawP_lower_ast(struct Compiler *C);
void pawP_collect_items(struct Compiler *C, struct Pool *pool);

struct IrType *pawP_lower_type(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirType *type);
struct IrTypeList *pawP_lower_type_list(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirTypeList *types);

struct RegisterInfo {
    int value;
};

DEFINE_LIST(struct Compiler, RegisterTable, struct RegisterInfo)

typedef unsigned long long BitChunk;
DEFINE_LIST(struct Compiler, BitSet, BitChunk)

struct BitSet *pawP_bitset_new(struct Compiler *C, int count);
int pawP_bitset_count(struct BitSet const *bs);
paw_Bool pawP_bitset_get(struct BitSet const *bs, int i);
void pawP_bitset_set(struct BitSet *bs, int i);
void pawP_bitset_set_range(struct BitSet *bs, int i, int j);
void pawP_bitset_clear(struct BitSet *bs, int i);
void pawP_bitset_clear_range(struct BitSet *bs, int i, int j);
void pawP_bitset_and(struct BitSet *a, struct BitSet const *b);
void pawP_bitset_or(struct BitSet *a, struct BitSet const *b);

struct RegisterTable *pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirIntervalList *intervals, struct MirLocationList *locations, int *pmax_reg);
struct Mir *pawP_lower_hir_body(struct Compiler *C, struct HirFuncDecl *func);
struct BodyMap *pawP_lower_hir(struct Compiler *C);

struct IrTypeList *pawP_instantiate_typelist(struct Compiler *C, struct IrTypeList *before, struct IrTypeList *after, struct IrTypeList *target);
struct IrType *pawP_instantiate_field(struct Compiler *C, struct IrType *self, struct HirDecl *field);
struct HirDecl *pawP_find_field(struct Compiler *C, struct IrType *self, String *name);
struct IrType *pawP_find_method(struct Compiler *C, struct IrType *self, String *name);

struct Decision *pawP_check_exhaustiveness(struct Hir *hir, struct Pool *pool, struct HirMatchExpr *match, struct VariableList *vars);
void pawP_lower_matches(struct Compiler *C);

struct IrType *pawP_generalize(struct Compiler *C, struct IrType *type);

// Instantiate a polymorphic function or type
// Works by replacing each generic type with the corresponding concrete type from
// the given list of 'types'. Returns a HirInstanceDecl if 'decl' is a function,
// and 'decl' otherwise. We avoid recursively visiting the function body here, since
// doing so might cause further instantiations due to the presence of recursion.
// Function instance bodies are expanded in a separate pass.
struct IrType *pawP_instantiate(struct Compiler *C, struct IrType *base, struct IrTypeList *types);

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

struct ItemList *pawP_allocate_defs(struct Compiler *C, struct BodyList *bodies, struct IrTypeList *types);

enum LookupKind {
    LOOKUP_TYPE,
    LOOKUP_VALUE,
    LOOKUP_EITHER,
};

struct IrType *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind, paw_Bool is_annotation);
struct IrType *pawP_lookup_trait(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path);

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, char const *modname);
void pawP_teardown(paw_Env *P, struct DynamicMem *dm);

struct Ast *pawP_parse_prelude(struct Compiler *C);
struct Ast *pawP_parse_module(struct Compiler *C, String *modname, paw_Reader input, void *ud);

void pawP_validate_adt_traits(struct Compiler *C, struct HirAdtDecl *d);

paw_Bool pawP_fold_unary_op(struct Compiler *C, enum UnaryOp op, Value v, Value *pr, enum BuiltinKind kind);
paw_Bool pawP_fold_binary_op(struct Compiler *C, enum BinaryOp op, Value x, Value y, Value *pr, enum BuiltinKind kind);

struct MonoResult {
    struct IrTypeList *types;
    struct BodyList *bodies;
};

struct MonoResult pawP_monomorphize(struct Compiler *C, struct BodyMap *bodies);

void pawP_resolve(struct Compiler *C);
void pawP_codegen(struct Compiler *C);

inline static void pawP_compile(struct Compiler *C, paw_Reader input, void *ud)
{
    pawP_parse_prelude(C);

    // parse an AST for the target module, as well as each imported module
    struct Ast *ast = pawP_parse_module(C, C->modname, input, ud);
    pawP_collect_imports(C, ast);

    // transform AST -> HIR
    pawP_lower_ast(C);

    // run the type checker, then codegen
    pawP_resolve(C);
    pawP_codegen(C);
}

struct Pool *pawP_pool_new(struct Compiler *C, struct PoolStats st);
void pawP_pool_free(struct Compiler *C, struct Pool *pool);

enum BuiltinKind pawP_type2code(struct Compiler *C, struct IrType *type);

struct ItemSlot {
    struct Type *rtti;
    struct Mir *mir;
    String *name;
    DeclId did;
};

DEFINE_LIST(struct Compiler, ItemList, struct ItemSlot)

struct Annotation {
    enum BuiltinKind kind : 7;
    paw_Bool has_value : 1;
    String *name;
    Value value;
};

DEFINE_LIST(struct Compiler, Annotations, struct Annotation)

paw_Bool pawP_check_extern(struct Compiler *C, struct Annotations *annos, struct Annotation *panno);
Value pawP_get_extern_value(struct Compiler *C, String *name);
void pawP_mangle_start(paw_Env *P, Buffer *buf, struct Compiler *G);
String *pawP_mangle_finish(paw_Env *P, Buffer *buf, struct Compiler *G);
String *pawP_mangle_name(struct Compiler *G, String const *modname, String const *name, struct IrTypeList *types);
String *pawP_mangle_attr(struct Compiler *C, String const *modname, String const *base, struct IrTypeList const *base_types, String const *attr, struct IrTypeList const *attr_types);

struct ExternInfo {
    String *name;
    Value value;
};

struct TraitOwnerList *pawP_get_trait_owners(struct Compiler *C, struct IrType *adt);

// Generate code for data structures used during compilation

#define P_ID_HASH(Ctx_, Did_) ((void)Ctx_, (paw_Uint)(Did_).value)
#define P_ID_EQUALS(Ctx_, A_, B_) ((void)Ctx_, ((A_).value == (B_).value))
#define P_PTR_HASH(Ctx_, Ptr_) ((void)Ctx_, (paw_Uint)(Ptr_))
#define P_PTR_EQUALS(Ctx_, A_, B_) ((void)Ctx_, ((A_) == (B_)))
#define P_VALUE_HASH(Ctx_, Value_) ((void)Ctx_, V_UINT(Value_))
#define P_VALUE_EQUALS(Ctx_, A_, B_) ((void)Ctx_, V_UINT(A_) == V_UINT(B_))

DEFINE_MAP(struct Compiler, FnDefMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrFnDef *)
DEFINE_MAP(struct Compiler, AdtDefMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrAdtDef *)
DEFINE_MAP(struct Compiler, VariantDefMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrVariantDef *)
DEFINE_MAP(struct Compiler, HirTypeMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, HirId, struct IrType *)
DEFINE_MAP(struct Compiler, DefTypeMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrType *)
DEFINE_MAP(struct Compiler, TraitMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrTypeList *)
DEFINE_MAP(struct Compiler, StringMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, String *, String *)
DEFINE_MAP(struct Compiler, ImportMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, String *, struct Ast *)
DEFINE_MAP(struct Compiler, ValueMap, pawP_alloc, P_VALUE_HASH, P_VALUE_EQUALS, Value, Value)
DEFINE_MAP(struct Compiler, BodyMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct Mir *)
DEFINE_MAP(struct Compiler, BuiltinMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, String *, struct Builtin *)
DEFINE_MAP_ITERATOR(HirTypeMap, HirId, struct IrType *)

DEFINE_LIST(struct Compiler, GlobalList, struct GlobalInfo)
DEFINE_LIST(struct Compiler, Statistics, struct Statistic *)
DEFINE_LIST(struct Compiler, BodyList, struct Mir *)

#endif // PAW_COMPILE_H
