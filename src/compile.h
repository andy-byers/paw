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
//   ----------------|--------|----------|----------------------------------------------
//    parse          | source | AST      | lexing and parsing, syntactical analysis
//    resolve        | AST    | -        | name and import resolution
//    lower_ast      | AST    | HIR      | convert AST into HIR
//    typeck         | HIR    | -        | type check function bodies
//    exhaustiveness | HIR    | -        | ensure pattern matching exhaustiveness
//    lower_hir      | HIR    | MIR      | convert HIR into MIR
//    monomorphize   | MIR    | -        | monomorphize polymorphic functions and types
//    unbox          | MIR    | -        | split inline types into scalars
//    codegen        | MIR    | bytecode | generate code for Paw's VM
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
#define SCAN_STR(X, s) pawP_scan_str(X, (X)->strings, s)

#define GET_NODE_TYPE(C, p) pawIr_get_type(C, (p)->hdr.id)
#define SET_NODE_TYPE(C, p, t) pawIr_set_type(C, (p)->hdr.id, t)

#define PRIVATE(Name_) ("(" Name_ ")")

#define MAX_MODULES 256
#define MAX_DECLS 10000

#define TARGET_MODNO 0
#define PRELUDE_MODNO 1

#define NO_NODE ((NodeId){0})
#define NO_DECL ((DeclId){0})

struct Hir;
struct HirModule;
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
struct IrAdt;

struct Mir;
struct MirIntervalList;
struct MirLocationList;
struct MirBodyList;
struct MirBlockList;
struct MatchVars;

struct StringMap;
struct BodyList;
struct BodyMap;

void *pawP_alloc(paw_Env *P, struct Pool *pool, void *ptr, size_t size0, size_t size);
#define P_ALLOC(C, ptr, size0, size) pawP_alloc(ENV(C), (C)->pool, ptr, size0, size)

Str *pawP_scan_nstr(struct Compiler *C, Tuple *map, char const *s, size_t n);
inline static Str *pawP_scan_str(struct Compiler *C, Tuple *map, char const *s)
{
    return pawP_scan_nstr(C, map, s, strlen(s));
}

Str *pawP_format_string(struct Compiler *C, char const *fmt, ...);

#define IS_SCALAR_TYPE(code) ((code) < BUILTIN_STR)
#define IS_BASIC_TYPE(code) ((code) <= BUILTIN_STR)
#define IS_BUILTIN_TYPE(code) ((code) < NBUILTINS)

struct Builtin {
    Str *name;
    DeclId did;
    NodeId id;
};

struct GlobalInfo {
    enum BuiltinKind b_kind : 8;
    Str *name;
    Value value;
    int modno;
    int index;
};

typedef struct Map Map;

struct Compiler {
    struct Builtin builtins[NBUILTINS];
    struct BuiltinMap *builtin_lookup;

    int list_length;
    int list_get;
    int list_set;
    int map_length;
    int map_get;
    int map_set;

    // callbacks for debugging
    Value on_build_ast;
    Value on_build_hir;
    Value on_build_mir;
    Value report_stats;

    struct Statistics *stats;
    struct PoolStats aux_stats;
    struct ModuleNames *modnames;
    struct DynamicMem *dm;
    struct Ast *ast;
    struct Hir *hir;
    struct Unifier *U;
    Str *modname;

    struct Pool *pool;
    struct Pool *ast_pool;
    struct Pool *hir_pool;
    struct Pool *mir_pool;

    struct SegmentTable *segtab;
    struct BodyMap *bodies;

    // '.strings' anchors all strings used during compilation so they are not
    // collected by the GC.
    Tuple *strings;

    // '.traits' maps each ADT to a list of implemented traits. Includes ADTs
    // from all modules being compiled.
    struct TraitMap *traits; // DeclId => HirDeclList *

    struct RttiMap *rtti;

    struct HirTypeMap *hir_types; // NodeId => IrType *
    struct DefTypeMap *def_types; // DefId => IrType *
    struct VariantDefMap *variant_defs; // DefId => IrVariantDef *
    struct AdtDefMap *adt_defs; // DefId => IrAdtDef *
    struct FnDefMap *fn_defs; // DefId => IrFnDef *

    struct IrLayoutMap *layouts;

    // map for quickly determining the methods implementing a given builtin
    // trait for a given type
    struct TraitOwners *trait_owners;

    struct GlobalList *globals;

    paw_Env *P;
    int decl_count;
    int hir_count;
    int def_count;
    int line;
};

paw_Bool pawP_push_callback(struct Compiler *C, char const *name);

void pawP_set_self(struct Compiler *C, struct IrType *method, struct IrType *self);
struct IrType *const *pawP_get_self(struct Compiler *C, struct IrType *method);

DEFINE_LIST(struct Compiler, ModuleNames, Str const *)

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

struct IrType *pawP_lower_type(struct Compiler *C, struct HirModule m, struct HirType *type);
struct IrTypeList *pawP_lower_type_list(struct Compiler *C, struct HirModule m, struct HirTypeList *types);

struct RegisterInfo {
    int value;
    int size;
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

struct IrTypeList *pawP_instantiate_typelist(struct Compiler *C, struct IrTypeList *before, struct IrTypeList *after, struct IrTypeList *target);
struct IrType *pawP_instantiate_field(struct Compiler *C, struct IrType *self, struct IrType *field);
struct IrTypeList *pawP_instantiate_struct_fields(struct Compiler *C, struct IrAdt *inst);
struct IrTypeList *pawP_instantiate_variant_fields(struct Compiler *C, struct IrAdt *inst, int index);
struct HirDecl *pawP_find_field(struct Compiler *C, struct IrType *self, Str *name);
struct IrType *pawP_find_method(struct Compiler *C, struct IrType *self, Str *name);

struct Decision *pawP_check_exhaustiveness(struct Hir *hir, struct Pool *pool, Str const *modname, struct HirMatchExpr *match, struct MatchVars *vars);
void pawP_lower_matches(struct Compiler *C);

struct IrType *pawP_generalize(struct Compiler *C, struct IrType *type);
struct IrType *pawP_generalize_assoc(struct Compiler *C, struct IrType *type, struct IrType *method);

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
void pawP_import(struct Compiler *C, void *state); // TODO: not used?

struct AstDecl *pawP_import_module(struct Compiler *C, Str *modname);

struct ItemList *pawP_allocate_defs(struct Compiler *C, struct BodyList *bodies, struct IrTypeList *types);

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, char const *modname);
void pawP_teardown(paw_Env *P, struct DynamicMem *dm);

struct AstDecl *pawP_parse_module(struct Compiler *C, Str *modname, paw_Reader input, void *ud);

void pawP_validate_adt_traits(struct Compiler *C, struct HirAdtDecl *d);

struct MonoResult {
    struct IrTypeList *types;
    struct BodyList *bodies;
};

void pawP_scalarize_registers(struct Compiler *C, struct Mir *mir);
struct MonoResult pawP_monomorphize(struct Compiler *C, struct BodyMap *bodies);

void pawP_compile(struct Compiler *C, paw_Reader input, void *ud);

struct Pool *pawP_pool_new(struct Compiler *C, struct PoolStats st);
void pawP_pool_free(struct Compiler *C, struct Pool *pool);

enum BuiltinKind pawP_type2code(struct Compiler *C, struct IrType *type);
struct Builtin *pawP_builtin_info(struct Compiler *C, enum BuiltinKind code);
struct IrType *pawP_builtin_type(struct Compiler *C, enum BuiltinKind code);

struct ItemSlot {
    struct RttiType *rtti;
    struct Mir *mir;
    Str *name;
    DeclId did;
};

DEFINE_LIST(struct Compiler, ItemList, struct ItemSlot)

struct Annotation {
    enum BuiltinKind kind : 7;
    paw_Bool has_value : 1;
    struct SourceSpan span;
    Str *modname;
    Str *name;
    Value value;
};

DEFINE_LIST(struct Compiler, Annotations, struct Annotation)

paw_Bool pawP_check_extern(struct Compiler *C, struct Annotations *annos, struct Annotation *panno);
Value const *pawP_get_extern_value(struct Compiler *C, Str const *name);
void pawP_mangle_start(paw_Env *P, Buffer *buf, struct Compiler *G);
Str *pawP_mangle_finish(paw_Env *P, Buffer *buf, struct Compiler *G);
Str *pawP_mangle_name(struct Compiler *G, Str const *modname, Str const *name, struct IrTypeList *types);
Str *pawP_mangle_attr(struct Compiler *C, Str const *modname, Str const *base, struct IrTypeList const *base_types, Str const *attr, struct IrTypeList const *attr_types);

struct ExternInfo {
    Str *name;
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
DEFINE_MAP(struct Compiler, HirTypeMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, struct IrType *)
DEFINE_MAP(struct Compiler, DefTypeMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrType *)
DEFINE_MAP(struct Compiler, TraitMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrTypeList *)
DEFINE_MAP(struct Compiler, StringMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str *, Str *)
DEFINE_MAP(struct Compiler, ValueMap, pawP_alloc, P_VALUE_HASH, P_VALUE_EQUALS, Value, Value)
DEFINE_MAP(struct Compiler, BodyMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct Mir *)
DEFINE_MAP(struct Compiler, BuiltinMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str *, struct Builtin *)
DEFINE_MAP_ITERATOR(HirTypeMap, NodeId, struct IrType *)

DEFINE_LIST(struct Compiler, GlobalList, struct GlobalInfo)
DEFINE_LIST(struct Compiler, Statistics, struct Statistic *)
DEFINE_LIST(struct Compiler, BodyList, struct Mir *)

#endif // PAW_COMPILE_H
