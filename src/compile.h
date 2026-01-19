// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// compile.h: compiler entrypoint
//
// The compiler converts source code into machine code that can be run natively
// on supported platforms. It performs the following passes:
//
//    # | name           | input  | output   | purpose
//   ---|-- -------------|--------|----------|-------------------------------------------------
//    1 | parse          | source | AST      | lexing and parsing, syntactical analysis
//    2 | resolve        | AST    | -        | name and import resolution
//    3 | lower_ast      | AST    | HIR      | convert AST into HIR
//    4 | typeck         | HIR    | -        | type check function bodies
//    5 | exhaustiveness | HIR    | -        | ensure pattern matching exhaustiveness
//    6 | lower_hir      | HIR    | MIR      | convert HIR into MIR
//    7 | monomorphize   | MIR    | -        | monomorphize polymorphic functions and types
//    8 | codegen        | MIR    | binary   | generate a native executable/library using LLVM
//
//            | 1  2  3  4  5  6  7  8
//   ---------|------------------------
//    AST     | X----->  .  .  .  .  .
//    HIR     | .  .  X-------->  .  .
//    Paw IR  | .  .  .  X----------->
//    MIR     | .  .  .  .  .  X----->
//    LLVM IR | .  .  .  .  .  .  .  X
//

#ifndef PAW_COMPILE_H
#define PAW_COMPILE_H


#include "code.h"
#include "debug.h"
#include "env.h"
#include "list.h"
#include "map.h"
#include "mem.h"
#include "core.h"
#include "source.h"
#include "stats.h"
#include "trait.h"

#define ENV(x) ((x)->P)
#define DLOG(X, ...) PAWD_LOG(ENV(X), __VA_ARGS__)
#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))
#define SCAN_STR(X, s) pawP_scan_str(X, s)

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
struct HirFnDecl;
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
struct MirIntervalMap;
struct MirLocationList;
struct MirBodyList;
struct MirBlockList;
struct MatchVars;

struct StringMap;
struct BodyList;
struct BodyMap;

void *pawP_alloc(struct Pool *pool, void *ptr, size_t size0, size_t size);
#define P_ALLOC(C, ptr, size0, size) pawP_alloc((C)->pool, ptr, size0, size)

EXTERN_C Str *pawP_scan_nstr(struct Compiler *C, char const *s, size_t n);
inline static Str *pawP_scan_str(struct Compiler *C, char const *s)
{
    return pawP_scan_nstr(C, s, strlen(s));
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

    // callbacks for debugging
    Value on_build_ast;
    Value on_build_hir;
    Value on_build_mir;
    Value report_stats;

    struct Statistics *stats;
    struct PoolStats aux_stats;
    struct ModuleInfo *modinfo;
    struct DynamicMem *dm;
    struct Ast *ast;
    struct Hir *hir;
    struct Unifier *U;

    Str const *dirname;
    Str const *pathname;
    Str const *modname;

    struct Pool *pool;
    struct Pool *ast_pool;
    struct Pool *hir_pool;
    struct Pool *mir_pool;

    struct SegmentTable *segtab;
    struct BodyMap *bodies;

    struct StringMap *strings;
    struct StringMap *symbols;
    struct Searchers *searchers;

    // '.traits' maps each ADT to a list of implemented traits. Includes ADTs
    // from all modules being compiled.
    struct TraitMap *traits; // DeclId => HirDeclList *
    struct IrLayoutMap *layouts;

    struct HirTypeMap *hir_types; // NodeId => IrType *
    struct DefTypeMap *def_types; // DefId => IrType *
    struct VariantDefMap *variant_defs; // DefId => IrVariantDef *
    struct TraitDefMap *trait_defs; // DefId => IrTraitDef *
    struct AdtDefMap *adt_defs; // DefId => IrAdtDef *
    struct FnDefMap *fn_defs; // DefId => IrFnDef *
    struct ImplMap *impl_defs; // DefId => IrImpl *

    // map for quickly determining the methods implementing a given builtin
    // trait for a given type
    struct TraitOwners *trait_owners;

    struct GlobalList *globals;

    // TODO: use this in the frontend as well?
    struct {
        struct {
            struct IrType *never_t;
            struct IrType *unit_t;
            struct IrType *bool_t;
            struct IrType *char_t;
            struct IrType *int_t;
            struct IrType *float_t;
            struct IrType *str_t;
        } primitives;

        struct {
            struct TypeCollection *list;
            struct TypeCollection *map;
        } iterators;

        struct TypeCollection *lists;
        struct TypeCollection *maps;
        struct TypeCollection *adts;
        struct TypeCollection *types;
    } typesystem;

    struct {
        struct IrImplList *blanket;
        struct IrImplOwners *inherent;
        struct IrImplOwners *trait;
    } impls;

    Str const *main_name;

    // type of the runtime string internalization table ("[str: ()]")
    struct IrType *strtab_type;
    struct IrType *main_args_type;

    paw_Env *P;
    int decl_count;
    int hir_count;
    int def_count;
    int line;
};

void pawP_callback(struct Compiler *C, char const *name, void *arg);

void pawP_set_self(struct Compiler *C, struct IrType *method, struct IrType *self);
struct IrType *const *pawP_get_self(struct Compiler *C, struct IrType *method);

struct Module {
    Str const *pathname;
    Str const *dirname;
    Str const *name;
};

DEFINE_LIST(struct Compiler, ModuleInfo, struct Module)
DEFINE_LIST(struct Compiler, Searchers, paw_Function)

// TODO: get rid of this struct and put members in Compiler or something
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

BitSet *pawP_bitset_new(struct Compiler *C, int count);
BitSet *pawP_bitset_copy(struct Compiler *C, BitSet const *bs);
int pawP_bitset_count(BitSet const *bs);
paw_Bool pawP_bitset_get(BitSet const *bs, int i);
void pawP_bitset_set(BitSet *bs, int i);
void pawP_bitset_set_range(BitSet *bs, int i, int j);
void pawP_bitset_clear(BitSet *bs, int i);
void pawP_bitset_clear_range(BitSet *bs, int i, int j);
BitSet *pawP_bitset_and(struct Compiler *C, BitSet const *a, BitSet const *b);
BitSet *pawP_bitset_or(struct Compiler *C, BitSet const *a, BitSet const *b);

struct Decision *pawP_check_exhaustiveness(struct Hir *hir, struct Pool *pool, Str const *modname, struct HirMatchExpr *match, struct MatchVars *vars);
void pawP_lower_matches(struct Compiler *C);

struct IrType *pawP_generalize(struct Compiler *C, struct SourceLoc loc, struct IrType *type);
struct IrType *pawP_generalize_assoc(struct Compiler *C, struct SourceLoc loc, struct IrType *type, struct IrType *method);

struct Substitution {
    struct IrTypeList *generics;
    struct IrTypeList *types;
};

// Type representing the result of a type instantiation
// Contains the instantiated type, as well as the substitution applied to it.
struct Instantiation {
    struct Substitution subst;
    struct IrType *inst;
};

// Replace each generic type from the binder on "type" with an inference variable
// Note that "type" is not modified by this operation.
struct Instantiation pawP_instantiate_v2(struct Compiler *C, struct SourceLoc loc, struct IrType *type);

struct Instantiation pawP_instantiate_assoc(struct Compiler *C, struct SourceLoc loc, struct IrType *type, struct IrType *method);

// Substitute types in "subst.generics" for types in "subst.types" in the context of the given "type"
// Note that "type" is not modified by this operation.
struct IrType *pawP_substitute(struct Compiler *C, struct SourceLoc loc, struct IrType *type, struct Substitution subst);

// TODO: remove this one and rename _v2
// Instantiate a polymorphic function or type
// Works by replacing each generic type in the function signature with the
// corresponding concrete type from the given list of 'types'.
struct IrType *pawP_instantiate(struct Compiler *C, struct IrType *base, struct IrTypeList *types);

struct IrType *pawP_instantiate_method(struct Compiler *C, struct IrType *self, struct IrTypeList *types, struct IrType *method);
struct IrTypeList *pawP_instantiate_typelist(struct Compiler *C, struct IrTypeList *before, struct IrTypeList *after, struct IrTypeList *target);
struct IrType *pawP_instantiate_field(struct Compiler *C, struct IrType *self, struct IrType *field);
EXTERN_C struct IrTypeList *pawP_instantiate_struct_fields(struct Compiler *C, struct IrAdt *inst);
EXTERN_C struct IrTypeList *pawP_instantiate_variant_fields(struct Compiler *C, struct IrAdt *inst, int index);

void pawP_init_substitution_folder(struct IrTypeFolder *F, struct Compiler *C, struct Substitution *subst,
                                   struct IrTypeList *generics, struct IrTypeList *types);

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, Str const *modname, Str const *pathname, Str const *dirname);
void pawP_teardown(paw_Env *P, struct DynamicMem *dm);

struct AstDecl *pawP_parse_module(struct Compiler *C, Str *modname, paw_Reader input, void *ud);

void pawP_validate_adt_traits(struct Compiler *C, struct HirAdtDecl *d);

struct MonoResult {
    struct IrTypeList *types;
    struct BodyList *bodies;
};

struct MonoResult pawP_monomorphize(struct Compiler *C, struct BodyMap *bodies);

void pawP_compile(struct Compiler *C, paw_Reader input, void *ud);

struct Pool *pawP_pool_new(struct Compiler *C, struct PoolStats st);
void pawP_pool_free(struct Compiler *C, struct Pool *pool);

EXTERN_C enum BuiltinKind pawP_type2code(struct Compiler *C, struct IrType *type);
EXTERN_C struct IrType *pawP_builtin_type(struct Compiler *C, enum BuiltinKind code);
struct Builtin *pawP_builtin_info(struct Compiler *C, enum BuiltinKind code);

struct Annotation {
    enum BuiltinKind kind : 7;
    paw_Bool has_value : 1;
    struct SourceSpan span;
    Str *modname;
    Str *name;
    Value value;
};

DEFINE_LIST(struct Compiler, Annotations, struct Annotation)

EXTERN_C paw_Bool pawP_contains_core_annotation(struct Compiler *C, Annotations const *annotations);
EXTERN_C paw_Bool pawP_check_extern(struct Compiler *C, struct Annotations *annos, struct Annotation *panno);
paw_Bool pawP_get_extern_value(struct Compiler *C, Str const *name, Value *result);
void pawP_mangle_start(paw_Env *P, Buffer *buf, struct Compiler *G);
Str *pawP_mangle_finish(paw_Env *P, Buffer *buf, struct Compiler *G);
EXTERN_C Str *pawP_mangle_name(struct Compiler *G, Str const *modname, Str const *name, struct IrTypeList *types);
EXTERN_C Str *pawP_mangle_attr(struct Compiler *C, Str const *modname, Str const *base, struct IrTypeList const *base_types, Str const *attr, struct IrTypeList const *attr_types);

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
DEFINE_MAP(struct Compiler, ImplMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrImpl *)
DEFINE_MAP(struct Compiler, TraitDefMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrTraitDef *)
DEFINE_MAP(struct Compiler, VariantDefMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrVariantDef *)
DEFINE_MAP(struct Compiler, HirTypeMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, struct IrType *)
DEFINE_MAP(struct Compiler, DefTypeMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrType *)
DEFINE_MAP(struct Compiler, TraitMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrTypeList *)
DEFINE_MAP(struct Compiler, StringMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, void *)
DEFINE_MAP(struct Compiler, ValueMap, pawP_alloc, P_VALUE_HASH, P_VALUE_EQUALS, Value, Value)
DEFINE_MAP(struct Compiler, BodyMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct Mir *)
DEFINE_MAP(struct Compiler, BuiltinMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str *, struct Builtin *)

DEFINE_MAP_ITERATOR(StringMap, Str const *, void *)
DEFINE_MAP_ITERATOR(HirTypeMap, NodeId, struct IrType *)

DEFINE_LIST(struct Compiler, GlobalList, struct GlobalInfo)
DEFINE_LIST(struct Compiler, BodyList, struct Mir *)
DEFINE_LIST(struct Compiler, LineBuffer, char const *)

// TODO: define these elsewhere, preferably in env.h but need to remove mem.h -> ... -> env.h dep
DEFINE_LIST(paw_Env, Statistics, struct Statistic *)
DEFINE_MAP(paw_Env, StrMap, pawK_pool_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, void *)
DEFINE_MAP(paw_Env, CallbackMap, pawK_pool_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, paw_Function)

#endif // PAW_COMPILE_H
