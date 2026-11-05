// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// compiler.h: compiler entrypoint
//
// The compiler converts source code into machine code that can be run natively
// on supported platforms. It performs the following passes:
//
//    # | name           | input  | output   | purpose
//   ---|----------------|--------|----------|-------------------------------------------------
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

#ifndef PAW_COMPILER_H
#define PAW_COMPILER_H

#include "code.h"
#include "debug.h"
#include "env.h"
#include "core.h"
#include "source.h"
#include "stats.h"

#include "common/list.h"
#include "common/map.h"

#define ENV(x) ((x)->P)
#define DLOG(X, ...) PAWD_LOG(ENV(X), __VA_ARGS__)
#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))
#define SCAN_STR(X, s) pawP_scan_str(X, s)

#define GET_NODE_TYPE(C, p) ir_get_type(C, (p)->hdr.id)
#define SET_NODE_TYPE(C, p, t) ir_set_type(C, (p)->hdr.id, t)

#define PRIVATE(Name_) ("(" Name_ ")")

#define MAX_MODULES 256
#define MAX_DECLS 10000

#define TARGET_MODNO 0
#define PRELUDE_MODNO 1

#define ALLOCATOR_OF(Ctx_) (&(Ctx_)->a)

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
struct HirGenericArg;
struct HirSegment;
struct HirProjectionType;

struct IrType;
struct IrTypeList;
struct IrGenericArgs;
struct IrTypeFolder;
struct IrSignature;
struct IrAdt;

struct Mir;
struct MirBodyList;
struct MirBlockList;
struct MatchVars;

struct StringMap;
struct BodyList;
struct BodyMap;

EXTERN_C void *compiler_alloc(struct Compiler *C, void *ptr, size_t size0, size_t size);

EXTERN_C Str const *pawP_scan_nstr(struct Compiler *C, char const *s, size_t n);
inline static Str const *pawP_scan_str(struct Compiler *C, char const *s)
{
    return pawP_scan_nstr(C, s, strlen(s));
}

EXTERN_C Str const *pawP_format_string(struct Compiler *C, char const *fmt, ...);

#define IS_SCALAR_TYPE(code) ((code) < BUILTIN_STR)
#define IS_BASIC_TYPE(code) ((code) <= BUILTIN_STR)
#define IS_BUILTIN_TYPE(code) ((code) < NBUILTINS)

struct Builtin {
    Str const *name;
    DeclId did;
    NodeId id;
};

struct Compiler {
    struct Builtin builtins[NBUILTINS];
    struct BuiltinMap *builtin_lookup;

    DeclId core_traits[NUM_CORE_TRAITS];

    struct IrType *default_int;
    struct IrType *default_float;

    DeclId std_math_NAN;
    DeclId std_math_INFINITY;
    DeclId std_ops_ordering;

    // callbacks for debugging
    IrValue on_build_ast;
    IrValue on_build_hir;
    IrValue on_build_mir;
    IrValue report_stats;

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

    struct Allocator a;

    struct Pool *pool;
    struct Pool *ast_pool;
    struct Pool *hir_pool;
    struct Pool *mir_pool;

    struct SegmentTable *segtab;
    struct UpvalueTable *upvtab;
    struct BodyMap *bodies;

    struct StringMap *strings;
    struct StringMap *symbols;
    struct Searchers *searchers;

    struct IrSolver *S;

    struct IrTypeLayouts *layouts;

    struct HirTypeMap *hir_types; // NodeId => IrType *
    struct DefTypeMap *def_types; // DefId => IrType *
    struct VariantDefMap *variant_defs; // DefId => IrVariantDef *
    struct TraitDefMap *trait_defs; // DefId => IrTraitDef *
    struct AdtDefMap *adt_defs; // DefId => IrAdtDef *
    struct FnDefMap *fn_defs; // DefId => IrFnDef *
    struct ImplMap *impl_defs; // DefId => IrImpl *
    struct GenericDefMap *generic_defs; // DefId => IrGenericDef *
    struct IrDefKinds *ir_def_kinds;
    struct IrGenericTypes *ir_generic_args; // TODO: rename, these are binders for polymorphic decls
    struct IrConstraintsMap *ir_constraints;
    struct IrAssocItemMap *ir_assoc_items;
    struct IrDeclArgs *ir_decl_args;
    struct IrTraitBounds *ir_trait_bounds;
    struct IrPendingConstants *pending_constants;
    struct IrResolvedConstants *resolved_constants;
    struct IrObligations *const_obligations;
    struct NodeMap *self_types;
    struct MethodSelectionMap *method_selections;

    struct SourceSpanRefs *source_span_refs;

    // TODO: use this in the frontend as well?
    struct {
        struct {
            struct IrType *never_t;
            struct IrType *unit_t;
            struct IrType *bool_t;
            struct IrType *char_t;
            struct IrTypeList *int_t;
            struct IrTypeList *float_t;
            struct IrType *str_t;
        } primitives;

        struct TypeCollection *ptrs;
        struct TypeCollection *slices;
        struct TypeCollection *arrays;
        struct TypeCollection *adts;
        struct TypeCollection *types;
    } typesystem;

    struct {
        struct IrTypeMap *inherent;
        struct IrTypeMap *trait;
        struct IrDefs *blanket;
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

DEFINE_LIST(ModuleInfo, struct Module)
DEFINE_LIST(Searchers, paw_Function)

// TODO: get rid of this struct and put members in Compiler or something
// Keeps track of dynamic memory used by the compiler
struct DynamicMem {
    struct Pool pool;
};

DeclId pawP_next_decl_id(struct Compiler *C, int modno);

void pawP_lower_ast(struct Compiler *C);
void pawP_collect_items(struct Compiler *C, struct Pool *pool);

struct IrType *pawP_lower_type(struct Compiler *C, struct HirModule m, struct HirType *type);
struct IrGenericArg pawP_lower_generic_arg(struct Compiler *C, struct HirModule m, struct HirGenericArg arg);
struct IrType *pawP_lower_type_alias(struct Compiler *C, struct HirSegment segment, struct HirDecl *decl, struct IrGenericArgs *knowns);

void pawP_solve_type_obligations(struct Hir *hir, struct HirModule m, struct HirType *type);

struct RegisterInfo {
    int value;
    int size;
};

DEFINE_LIST(RegisterTable, struct RegisterInfo)

typedef unsigned long long BitChunk;
typedef struct BitSet BitSet;

BitSet *bitset_new(struct Allocator const *a, int count);
BitSet *bitset_clone(BitSet const *set);
int bitset_len(BitSet const *set);
paw_Bool bitset_equals(BitSet const *x, BitSet const *y);
paw_Bool bitset_get(BitSet const *set, int i);
void bitset_set(BitSet *set, int i);
void bitset_set_range(BitSet *set, int i, int j);
void bitset_clear(BitSet *set, int i);
void bitset_clear_range(BitSet *set, int i, int j);
void bitset_and(BitSet *x, BitSet const *y);
void bitset_or(BitSet *x, BitSet const *y);
void bitset_xor(BitSet *x, BitSet const *y);

struct Decision *pawP_check_exhaustiveness(struct Hir *hir, struct Pool *pool, Str const *modname, struct HirMatchExpr *match, struct MatchVars *vars);
void pawP_lower_matches(struct Compiler *C);

struct Substitution {
    struct IrGenericArgs *params;
    struct IrGenericArgs *args;
};

// Type representing the result of a type instantiation
// Contains the instantiated type, as well as the substitution applied to it.
struct Instantiation {
    struct Substitution subst;
    struct IrType *inst;
};

struct Instantiation pawP_instantiate_assoc(struct Compiler *C, struct IrType *type, struct IrType *method);

// Substitute types in "subst.params" for types in "subst.args" in the context of the given "type"
// Note that "type" is not modified by this operation.
struct IrType *pawP_substitute(struct Compiler *C, struct IrType *type, struct Substitution subst);
struct IrTrait *pawP_substitute_trait(struct Compiler *C, struct IrTrait *trait, struct Substitution subst);
struct IrGenericArg pawP_substitute_arg(struct Compiler *C, struct IrGenericArg arg, struct Substitution subst);
struct IrConst *pawP_substitute_const(struct Compiler *C, struct IrConst *k, struct Substitution subst);

struct IrType *instantiate_struct_field(struct Compiler *C, struct IrType *self, struct IrType *field);
EXTERN_C struct IrTypeList *instantiate_struct_fields(struct Compiler *C, struct IrAdt const *inst);
EXTERN_C struct IrTypeList *instantiate_variant_fields(struct Compiler *C, struct IrAdt const *inst, int index);

void pawP_init_substitution_folder(struct IrTypeFolder *F, struct Compiler *C, struct Substitution *subst,
                                   struct IrGenericArgs *params, struct IrGenericArgs *args);

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, Str const *modname, Str const *pathname, Str const *dirname);
void pawP_teardown(paw_Env *P, struct DynamicMem *dm);

struct AstDecl *pawP_parse_source_file(struct Compiler *C, Str const *pkgname, paw_Reader input, void *ud);

struct MonoResult {
    struct BodyList *bodies;
    struct TypeCollection *bodies2;
};

struct MonoResult pawP_monomorphize(struct Compiler *C, struct BodyMap *bodies);

void pawP_compile(struct Compiler *C, paw_Reader input, void *ud);

struct Pool *compiler_new_pool(struct Compiler *C, struct PoolStats st);
void compiler_free_pool(struct Compiler *C, struct Pool *pool);

EXTERN_C enum BuiltinKind pawP_type2code(struct Compiler *C, struct IrType *type);
EXTERN_C struct IrType *pawP_builtin_type(struct Compiler *C, enum BuiltinKind code);
struct Builtin *pawP_builtin_info(struct Compiler *C, enum BuiltinKind code);

struct Annotation {
    enum BuiltinKind kind : 7;
    paw_Bool has_value : 1;
    struct SourceSpan span;
    Str const *modname;
    Str const *name;
    IrValue value;
};

DEFINE_LIST(Annotations, struct Annotation)

EXTERN_C paw_Bool pawP_check_extern(struct Compiler *C, struct Annotations *annos, struct Annotation *panno);
paw_Bool pawP_get_extern_value(struct Compiler *C, Str const *name, IrValue *result);

// Generate code for data structures used during compilation

#define P_ID_HASH(Did_) (hash_uint64((paw_Uint64)(Did_).value))
#define P_ID_EQUALS(A_, B_) ((A_).value == (B_).value)
#define P_PTR_HASH(Ptr_) (hash_pointer(Ptr_))
#define P_PTR_EQUALS(A_, B_) (((A_) == (B_)))

// NOTE: only works for primitive constants and padding bits must be cleared
#define P_VALUE_HASH(Value_) (hash_uint64((Value_).u64))
#define P_VALUE_EQUALS(A_, B_) ((A_).u64 == (B_).u64)

struct MethodSelection {
    struct IrTrait *trait;
    struct IrType *method;
};

DEFINE_LIST(NodeList, NodeId)
DEFINE_MAP(MethodSelectionMap, P_ID_HASH, P_ID_EQUALS, NodeId, struct MethodSelection)
DEFINE_MAP(NodeMap, P_ID_HASH, P_ID_EQUALS, NodeId, NodeId)
DEFINE_MAP(FnDefMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrFnDef *)
DEFINE_MAP(AdtDefMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrAdtDef *)
DEFINE_MAP(ImplMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrImpl *)
DEFINE_MAP(GenericDefMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrGenericDef *)
DEFINE_MAP(TraitDefMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrTraitDef *)
DEFINE_MAP(VariantDefMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrVariantDef *)
DEFINE_MAP(HirTypeMap, P_ID_HASH, P_ID_EQUALS, NodeId, struct IrType *)
DEFINE_MAP(DefTypeMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrType *)
DEFINE_MAP(TraitMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct IrTrait *)
DEFINE_MAP(StringMap, P_PTR_HASH, P_PTR_EQUALS, Str const *, void *)
DEFINE_MAP(ValueMap, P_VALUE_HASH, P_VALUE_EQUALS, IrValue, IrValue)
DEFINE_MAP(BodyMap, P_ID_HASH, P_ID_EQUALS, DeclId, struct Mir *)
DEFINE_MAP(BuiltinMap, P_PTR_HASH, P_PTR_EQUALS, Str const *, struct Builtin *)
DEFINE_MAP(SourceSpanRefs, P_ID_HASH, P_ID_EQUALS, SpanRef, struct SourceSpan)

DEFINE_MAP_ITERATOR(StringMap, Str const *, void *)
DEFINE_MAP_ITERATOR(HirTypeMap, NodeId, struct IrType *)

DEFINE_LIST(BodyList, struct Mir *)
DEFINE_LIST(LineBuffer, char const *)

// TODO: define these elsewhere, preferably in env.h but need to remove mem.h -> ... -> env.h dep
DEFINE_LIST(Statistics, struct Statistic *)
DEFINE_MAP(StrMap, P_PTR_HASH, P_PTR_EQUALS, Str const *, void *)
DEFINE_MAP(CallbackMap, P_PTR_HASH, P_PTR_EQUALS, Str const *, paw_Function)

#endif // PAW_COMPILER_H
