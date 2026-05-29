// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <math.h>
#include "compile.h"
#include "api.h"
#include "ast.h"
#include "debug.h"
#include "error.h"
#include "hir.h"
#include "ir_type.h"
#include "layout.h"
#include "lex.h"
#include "lib.h"
#include "map.h"
#include "os.h"
#include "resolve.h"
#include "solve.h"
#include "type_folder.h"
#include "unify.h"

#define COMPILER_ERROR(C_, Kind_, Modname_, ...) pawErr_##Kind_(C_, Modname_, __VA_ARGS__)

// All paw language keywords
//
// ORDER TokenKind
static char const *kKeywords[] = {
    "pub",
    "use",
    "fn",
    "type",
    "enum",
    "struct",
    "impl",
    "trait",
    "const",
    "mut",
    "let",
    "if",
    "else",
    "loop",
    "for",
    "while",
    "match",
    "break",
    "continue",
    "return",
    "in",
    "as",
    "true",
    "false",
    "_",
};

static Str *basic_type_name(paw_Env *P, char const *name, enum BuiltinKind kind)
{
    Str *s = pawS_new_fixed(P, name);
    s->flag = FLAG2CODE(kind); // works either direction
    return s;
}

void pawP_init(paw_Env *P)
{
    // Add all keywords to the interned strings table. Fix them so they are
    // never collected. Also added to the lexer string map.
    for (unsigned i = 0; i < PAW_COUNTOF(kKeywords); ++i) {
        char const *kw = kKeywords[i];
        Str *str = pawS_new_fixed(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }

    P->string_cache[CSTR_UNIT] = basic_type_name(P, "unit", BUILTIN_UNIT);
    P->string_cache[CSTR_BOOL] = basic_type_name(P, "bool", BUILTIN_BOOL);
    P->string_cache[CSTR_CHAR] = basic_type_name(P, "char", BUILTIN_CHAR);
    P->string_cache[CSTR_INT] = basic_type_name(P, "int", BUILTIN_INT);
    P->string_cache[CSTR_FLOAT] = basic_type_name(P, "float", BUILTIN_FLOAT);
    P->string_cache[CSTR_STR] = basic_type_name(P, "str", BUILTIN_STR);
    P->string_cache[CSTR_LIST] = pawS_new_fixed(P, "List");
    P->string_cache[CSTR_MAP] = pawS_new_fixed(P, "Map");
    P->string_cache[CSTR_OPTION] = pawS_new_fixed(P, "Option");
    P->string_cache[CSTR_RESULT] = pawS_new_fixed(P, "Result");
    P->string_cache[CSTR_RANGE] = pawS_new_fixed(P, "Range");
    P->string_cache[CSTR_RANGE_TO] = pawS_new_fixed(P, "RangeTo");
    P->string_cache[CSTR_RANGE_FROM] = pawS_new_fixed(P, "RangeFrom");
    P->string_cache[CSTR_RANGE_FULL] = pawS_new_fixed(P, "RangeFull");
    P->string_cache[CSTR_RANGE_INCLUSIVE] = pawS_new_fixed(P, "RangeInclusive");
    P->string_cache[CSTR_RANGE_TO_INCLUSIVE] = pawS_new_fixed(P, "RangeToInclusive");
    P->string_cache[CSTR_HASH] = pawS_new_fixed(P, "Hash");
    P->string_cache[CSTR_EQUALS] = pawS_new_fixed(P, "Equals");
    P->string_cache[CSTR_COMPARE] = pawS_new_fixed(P, "Compare");
    P->string_cache[CSTR_TRUE] = pawS_new_fixed(P, "true");
    P->string_cache[CSTR_FALSE] = pawS_new_fixed(P, "false");
    P->string_cache[CSTR_UNDERSCORE] = pawS_new_fixed(P, "_");
    P->string_cache[CSTR_EXCLAMATION] = pawS_new_fixed(P, "!");
    P->string_cache[CSTR_SELF] = pawS_new_fixed(P, "self");
    P->string_cache[CSTR_NEW] = pawS_new_fixed(P, "new");
    P->string_cache[CSTR_EXTERN] = pawS_new_fixed(P, "extern");

    P->string_cache[CSTR_KMODULES] = pawS_new_fixed(P, "paw.modules");
    P->string_cache[CSTR_KSYMBOLS] = pawS_new_fixed(P, "paw.symbols");
    P->string_cache[CSTR_KSEARCHERS] = pawS_new_fixed(P, "paw.searchers");
}

void pawP_compile(struct Compiler *C, paw_Reader input, void *ud)
{
    void pawP_resolve_names(struct Compiler *C);
    void pawP_check_types(struct Compiler *C);
    void pawP_lower_hir(struct Compiler *C);
    void pawP_generate_code(struct Compiler *C);

    pawP_parse_module(C, (Str *)C->modname, input, ud);

    pawP_resolve_names(C);
    pawP_lower_ast(C);
    pawP_check_types(C);
    pawP_lower_hir(C);
    pawP_generate_code(C);
}

enum BuiltinKind pawP_type2code(struct Compiler *C, IrType *type)
{
    if (IrIsUnit(type)) {
        return BUILTIN_UNIT;
    } else if (IrIsBool(type)) {
        return BUILTIN_BOOL;
    } else if (IrIsChar(type)) {
        return BUILTIN_CHAR;
    } else if (IrIsInt(type)) {
        return BUILTIN_INT;
    } else if (IrIsFloat(type)) {
        return BUILTIN_FLOAT;
    } else if (IrIsString(type)) {
        return BUILTIN_STR;
    } else if (IrIsPtr(type)) {
        return BUILTIN_PTR;
    } else if (IrIsSlice(type)) {
        return BUILTIN_SLICE;
    } else if (IrIsAdt(type)) {
        DeclId const base = IR_TYPE_DID(type);
        if (base.value == C->builtins[BUILTIN_OPTION].did.value) {
            return BUILTIN_OPTION;
        } else if (base.value == C->builtins[BUILTIN_RESULT].did.value) {
            return BUILTIN_RESULT;
        } else if (base.value == C->builtins[BUILTIN_RANGE].did.value) {
            return BUILTIN_RANGE;
        } else if (base.value == C->builtins[BUILTIN_RANGE_TO].did.value) {
            return BUILTIN_RANGE_TO;
        } else if (base.value == C->builtins[BUILTIN_RANGE_FROM].did.value) {
            return BUILTIN_RANGE_FROM;
        } else if (base.value == C->builtins[BUILTIN_RANGE_FULL].did.value) {
            return BUILTIN_RANGE_FULL;
        } else if (base.value == C->builtins[BUILTIN_RANGE_INCLUSIVE].did.value) {
            return BUILTIN_RANGE_INCLUSIVE;
        } else if (base.value == C->builtins[BUILTIN_RANGE_TO_INCLUSIVE].did.value) {
            return BUILTIN_RANGE_TO_INCLUSIVE;
        }
    }
    return NBUILTINS;
}

struct Builtin *pawP_builtin_info(struct Compiler *C, enum BuiltinKind code)
{
    return &C->builtins[code];
}

IrType *pawP_builtin_type(struct Compiler *C, enum BuiltinKind kind)
{
    switch (kind) {
        case BUILTIN_UNIT:
            return pawIr_new_unit(C);
        case BUILTIN_BOOL:
            return pawIr_new_bool(C);
        case BUILTIN_CHAR:
            return pawIr_new_char(C);
        case BUILTIN_INT:
            return pawIr_new_int(C);
        case BUILTIN_FLOAT:
            return pawIr_new_float(C);
        case BUILTIN_STR:
            return pawIr_new_string(C);
//TODO        case BUILTIN_SLICE:
//TODO            return pawIr_new_slice(C, pawIr_new_char(C));
        default: {
            PAW_UNREACHABLE();
//TODO            DeclId const did = pawP_builtin_info(C, kind)->did;
//TODO            return pawIr_get_def_type(C, did);
        }
    }
}

Str *pawP_scan_nstr(struct Compiler *C, char const *s, size_t n)
{
    paw_Env *P = ENV(C);
    Str *str = pawS_new_nstr(P, s, n);
    StringMap_insert(C, C->strings, str, NULL);
    return str;
}

Str *pawP_format_string(struct Compiler *C, char const *fmt, ...)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &buf, fmt, arg);
    va_end(arg);

    return pawL_buffer_finish(P, &buf);
}

static void register_builtin(struct Compiler *C, unsigned cstr, enum BuiltinKind kind)
{
    Str *s = CACHED_STRING(ENV(C), cstr);
    C->builtins[kind] = (struct Builtin){
        .did = NO_DECL,
        .name = s,
    };
    BuiltinMap_insert(C, C->builtin_lookup, s, &C->builtins[kind]);
}

void *pawP_alloc(struct Pool *pool, void *ptr, size_t size0, size_t size)
{
    return pawK_pool_alloc(pool, ptr, size0, size);
}

static void pool_free(paw_Env *P, struct Pool *pool)
{
    if (pool->next != NULL)
        pool->next->prev = pool->prev;
    if (pool->prev != NULL)
        pool->prev->next = pool->next;

    pawK_pool_uninit(pool);
    pawM_free(P, pool);
}

void pawP_pool_free(struct Compiler *C, struct Pool *pool)
{
    pool_free(ENV(C), pool);
}

struct Pool *pawP_pool_new(struct Compiler *C, struct PoolStats st)
{
    paw_Env *P = ENV(C);
    struct Pool *pool = pawM_new(P, struct Pool);

    struct Pool *head = C->P->pool;
    head->prev->next = pool;
    pool->prev = head->prev;
    pool->next = head;
    head->prev = pool;

    pawK_pool_init(P, pool, 512, st);
    return pool;
}

static void set_extern_value(struct Compiler *C, char const *name, Value value)
{
    StringMap_insert(C, C->symbols, SCAN_STR(C, name), value.p);
}

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, Str const *modname, Str const *pathname, Str const *dirname)
{
    *C = (struct Compiler){
        .pool = P->pool,
        .pathname = pathname,
        .modname = modname,
        .dirname = dirname,
        .dm = dm,
        .P = P,
    };

    C->searchers = *StrMap_get(NULL, P->registry, CACHED_STRING(P, CSTR_KSEARCHERS));
    C->symbols = *StrMap_get(NULL, P->registry, CACHED_STRING(P, CSTR_KSYMBOLS));
    C->stats = P->stats;

    C->aux_stats = (struct PoolStats){
                .bytes_alloc = pawStats_new(P, C->stats, "memory.aux.bytes_allocated"),
                .bytes_used = pawStats_new(P, C->stats, "memory.aux.bytes_used"),
                .num_alloc = pawStats_new(P, C->stats, "memory.aux.num_allocations"),
            };
    C->ast_pool = pawP_pool_new(C, (struct PoolStats){
                .num_alloc = pawStats_new(P, C->stats, "memory.ast.num_allocations"),
                .bytes_alloc = pawStats_new(P, C->stats, "memory.ast.bytes_allocated"),
                .bytes_used = pawStats_new(P, C->stats, "memory.ast.bytes_used"),
            });
    C->hir_pool = pawP_pool_new(C, (struct PoolStats){
                .num_alloc = pawStats_new(P, C->stats, "memory.hir.num_allocations"),
                .bytes_alloc = pawStats_new(P, C->stats, "memory.hir.bytes_allocated"),
                .bytes_used = pawStats_new(P, C->stats, "memory.hir.bytes_used"),
            });
    C->mir_pool = pawP_pool_new(C, (struct PoolStats){
                .num_alloc = pawStats_new(P, C->stats, "memory.mir.num_allocations"),
                .bytes_alloc = pawStats_new(P, C->stats, "memory.mir.bytes_allocated"),
                .bytes_used = pawStats_new(P, C->stats, "memory.mir.bytes_used"),
            });

    C->strings = StringMap_new(C);

    C->main_name = SCAN_STR(C, "main");

    C->modinfo = ModuleInfo_new(C);
    C->builtin_lookup = BuiltinMap_new(C);
    C->hir_types = HirTypeMap_new(C);
    C->self_types = NodeMap_new(C);
    C->layouts = IrTypeLayouts_new(C);

    C->def_types = DefTypeMap_new(C);
    C->variant_defs = VariantDefMap_new(C);
    C->trait_defs = TraitDefMap_new(C);
    C->adt_defs = AdtDefMap_new(C);
    C->fn_defs = FnDefMap_new(C);
    C->impl_defs = ImplMap_new(C);
    C->generic_defs = GenericDefMap_new(C);
    C->ir_def_kinds = IrDefKinds_new(C);
    C->ir_generic_args = IrGenericTypes_new(C);
    C->ir_decl_args = IrDeclArgs_new(C);
    C->ir_trait_bounds = IrTraitBounds_new(C);
    C->ir_assoc_items = IrAssocItemMap_new(C);
    C->ir_constraints = IrConstraintsMap_new(C);
    C->pending_constants = IrPendingConstants_new(C);
    C->resolved_constants = IrResolvedConstants_new(C);
    C->const_obligations = IrConstObligations_new(C);
    C->indexes = IrType2Map_new(C);

    C->source_span_refs = SourceSpanRefs_new(C);

    C->typesystem.ptrs = TypeCollection_new(C);
    C->typesystem.slices = TypeCollection_new(C);
    C->typesystem.arrays = TypeCollection_new(C);
    C->typesystem.adts = TypeCollection_new(C);
    C->typesystem.types = TypeCollection_new(C);

    C->impls.blanket = IrDefs_new(C);
    C->impls.inherent = IrDefs_new(C);
    C->impls.trait = IrDefs_new(C);

    C->segtab = SegmentTable_new(C);

    ModuleInfo_push(C, C->modinfo, (struct Module){
                .pathname = pathname,
                .dirname = dirname,
                .name = modname,
            });

    C->ast = pawAst_new(C);

    C->U = P_ALLOC(C, NULL, 0, sizeof(struct Unifier));
    *C->U = (struct Unifier){.C = C};

    C->S = pawIr_push_solver(C);

    // builtin primitives
    register_builtin(C, CSTR_UNIT, BUILTIN_UNIT);
    register_builtin(C, CSTR_BOOL, BUILTIN_BOOL);
    register_builtin(C, CSTR_CHAR, BUILTIN_CHAR);
    register_builtin(C, CSTR_INT, BUILTIN_INT);
    register_builtin(C, CSTR_FLOAT, BUILTIN_FLOAT);
    register_builtin(C, CSTR_STR, BUILTIN_STR);

    // builtin ADTs
    register_builtin(C, CSTR_OPTION, BUILTIN_OPTION);
    register_builtin(C, CSTR_RESULT, BUILTIN_RESULT);
    register_builtin(C, CSTR_RANGE, BUILTIN_RANGE);
    register_builtin(C, CSTR_RANGE_TO, BUILTIN_RANGE_TO);
    register_builtin(C, CSTR_RANGE_FROM, BUILTIN_RANGE_FROM);
    register_builtin(C, CSTR_RANGE_FULL, BUILTIN_RANGE_FULL);
    register_builtin(C, CSTR_RANGE_INCLUSIVE, BUILTIN_RANGE_INCLUSIVE);
    register_builtin(C, CSTR_RANGE_TO_INCLUSIVE, BUILTIN_RANGE_TO_INCLUSIVE);

    // TODO: won't work, need to get symbol from linked C math lib (use #[extern = "M_PI"])
    // external constant values must be known in the frontend
//    set_extern_value(C, "paw_math_PI", F2V(M_PI));
//    set_extern_value(C, "paw_math_NAN", F2V(NAN));
//    set_extern_value(C, "paw_math_INFINITY", F2V(INFINITY));
}

void pawP_teardown(paw_Env *P, struct DynamicMem *dm)
{
    struct Pool *pool = P->pool->next;
    while (pool != P->pool) {
        struct Pool *next = pool->next;
        pool_free(P, pool);
        pool = next;
    }

    pawM_free_vec(P, dm->source.data, dm->source.size);
    pawM_free_vec(P, dm->scratch.data, dm->scratch.alloc);

    P->modname = P->pathname = NULL;
}

#define CHUNKSZ (int)sizeof(BitChunk)

#define BITSET_NUM_CHUNKS(Set_) \
    (((Set_)->count + CHUNKSZ - 1) / CHUNKSZ)
#define BITSET_INDICES(Set_, I_, Pos_, Bit_) \
    paw_assert(0 <= (I_) && (I_) < (Set_)->count); \
    const int Pos_ = (I_) / CHUNKSZ; \
    const int Bit_ = (I_) % CHUNKSZ

BitSet *pawP_bitset_new(struct Compiler *C, int count)
{
    paw_assert(count > 0);
    BitSet *set = BitSet_new(C);
    int const n = (count + CHUNKSZ - 1) / CHUNKSZ;
    BitSet_resize(C, set, n);
    memset(set->data, 0, (size_t)n * CHUNKSZ);
    // store the number of bits, not the number of chunks
    set->count = count;
    return set;
}

BitSet *pawP_bitset_copy(struct Compiler *C, BitSet const *bs)
{
    BitSet *r = pawP_bitset_new(C, bs->count);
    for (int i = 0, n = BITSET_NUM_CHUNKS(bs); i < n; ++i)
        K_LIST_AT(r, i) = K_LIST_AT(bs, i);

    return r;
}

void pawP_bitset_set(BitSet *set, int i)
{
    BITSET_INDICES(set, i, pos, bit);
    BitChunk *bc = &K_LIST_AT(set, pos);
    *bc = *bc | (1ULL << bit);
}

void pawP_bitset_set_range(BitSet *bs, int i, int j)
{
    while (i < j) pawP_bitset_set(bs, i++);
}

int pawP_bitset_count(BitSet const *set)
{
    return set->count;
}

paw_Bool pawP_bitset_get(BitSet const *set, int i)
{
    BITSET_INDICES(set, i, pos, bit);
    BitChunk const bc = BitSet_get(set, pos);
    return (bc >> bit) & 1;
}

void pawP_bitset_clear(BitSet *set, int i)
{
    BITSET_INDICES(set, i, pos, bit);
    BitChunk *bc = &K_LIST_AT(set, pos);
    *bc = *bc & ~(1ULL << bit);
}

void pawP_bitset_clear_range(BitSet *bs, int i, int j)
{
    while (i < j) pawP_bitset_clear(bs, i++);
}

BitSet *pawP_bitset_and(struct Compiler *C, BitSet const *a, BitSet const *b)
{
    paw_assert(a->count == b->count);
    BitSet *r = pawP_bitset_new(C, a->count);
    for (int i = 0, n = BITSET_NUM_CHUNKS(a); i < n; ++i)
        K_LIST_AT(r, i) = BitSet_get(a, i) & BitSet_get(b, i);

    return r;
}

BitSet *pawP_bitset_or(struct Compiler *C, BitSet const *a, BitSet const *b)
{
    paw_assert(a->count == b->count);
    BitSet *r = pawP_bitset_new(C, a->count);
    for (int i = 0, n = BITSET_NUM_CHUNKS(a); i < n; ++i)
        K_LIST_AT(r, i) = BitSet_get(a, i) | BitSet_get(b, i);

    return r;
}

paw_Bool pawP_check_extern(struct Compiler *C, struct Annotations *annos, struct Annotation *panno)
{
    if (annos == NULL)
        return PAW_FALSE;
    struct Annotation *pa;
    K_LIST_FOREACH (annos, pa) {
        if (pawS_eq(pa->name, CSTR(C, CSTR_EXTERN))) {
            *panno = *pa;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

paw_Bool pawP_get_extern_value(struct Compiler *C, Str const *name, Value *result)
{
    void *const *pvalue = StringMap_get(C, C->symbols, name);
    if (pvalue == NULL) return PAW_FALSE;
    result->p = *pvalue;
    return PAW_TRUE;
}

 // TODO: delete all of these mangling routines, only using the ones defined in mangle.c

static void add_string_with_len(paw_Env *P, Buffer *buf, Str const *str)
{
    pawL_add_int(P, buf, PAW_CAST_INT(str->length));
    pawL_add_nstring(P, buf, str->text, str->length);
}

static void mangle_start(struct Compiler *C, Buffer *buf)
{
    L_ADD_LITERAL(ENV(C), buf, "_P");
}

static void mangle_start_generic_args(struct Compiler *C, Buffer *buf)
{
    pawL_add_char(ENV(C), buf, 'I');
}

static void mangle_finish_generic_args(struct Compiler *C, Buffer *buf)
{
    pawL_add_char(ENV(C), buf, 'E');
}

static void mangle_add_module(struct Compiler *C, Buffer *buf, Str const *name)
{
    pawL_add_char(ENV(C), buf, 'N');
    add_string_with_len(ENV(C), buf, name);
}

static void mangle_add_name(struct Compiler *C, Buffer *buf, Str const *name)
{
    add_string_with_len(ENV(C), buf, name);
}

static void mangle_add_type(struct Compiler *C, Buffer *buf, IrType *type);

static void mangle_add_const(struct Compiler *C, Buffer *buf, IrConst *konst)
{
    PAW_UNREACHABLE();
}

static void mangle_add_arg(struct Compiler *C, Buffer *buf, IrGenericArg arg)
{
    if (IrGenericArg_is_type(arg)) {
        IrType *t = IrGenericArg_get_type(arg);
        mangle_add_type(C, buf, t);
    } else {
        IrConst *k = IrGenericArg_get_const(arg);
        mangle_add_const(C, buf, k);
    }
}

static void mangle_add_type(struct Compiler *C, Buffer *buf, IrType *type)
{
    paw_Env *P = ENV(C);
    switch (type->hdr.kind) {
        case kIrUnit:
            pawL_add_char(P, buf, '0');
            break;
        case kIrBool:
            pawL_add_char(P, buf, 'b');
            break;
        case kIrChar:
            pawL_add_char(P, buf, 'c');
            break;
        case kIrInt:
            pawL_add_char(P, buf, 'i');
            break;
        case kIrFloat:
            pawL_add_char(P, buf, 'f');
            break;
        case kIrString:
            pawL_add_char(P, buf, 's');
            break;
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            struct IrAdtDef const *def = pawIr_get_adt_def(C, t->did);
            add_string_with_len(P, buf, def->name);
            // TODO: transition always allocating an IrTypeList object for IrAdt, IrSignature, etc. (remove t->args != NULL check)
            if (t->args != NULL && t->args->count > 0) {
                mangle_start_generic_args(C, buf);
                K_LIST_XFOREACH (t->args, IrGenericArg const, p)
                    mangle_add_arg(C, buf, *p);
                mangle_finish_generic_args(C, buf);
            }
            break;
        }
        case kIrSignature:
            type = IR_SIGNATURE_FN(C, type);
            // (fallthrough)
        case kIrFnPtr: {
            struct IrFnPtr const *fn = IrGetFnPtr(type);
            pawL_add_char(P, buf, 'F');
            K_LIST_XFOREACH (fn->params, IrType *const, p)
                mangle_add_type(C, buf, *p);
            pawL_add_char(P, buf, 'E');
            if (!IrIsUnit(fn->result))
                mangle_add_type(C, buf, fn->result);
            break;
        }
        case kIrTuple: {
            struct IrTuple const *t = IrGetTuple(type);
            pawL_add_char(P, buf, 'T');
            K_LIST_XFOREACH (t->elems, IrType *const, p)
                mangle_add_type(C, buf, *p);
            pawL_add_char(P, buf, 'E');
            break;
        }
        case kIrNever:
            pawL_add_char(P, buf, 'X');
            break;
        default:
            paw_assert(IrIsPtr(type));
            pawL_add_char(P, buf, 'P');
            mangle_add_type(C, buf, ir_deref(type));
    }
}

static void mangle_type(struct Compiler *C, Buffer *buf, IrType *type)
{
    mangle_add_type(C, buf, type);
}

static void mangle_types(struct Compiler *C, Buffer *buf, IrTypeList const *types)
{
    // TODO: transition always allocating an IrTypeList object for IrAdt, IrSignature, etc. (remove types != NULL check)
    if (types && types->count > 0) {
        mangle_start_generic_args(C, buf);
        K_LIST_XFOREACH (types, IrType *const, p)
            mangle_type(C, buf, *p);
        mangle_finish_generic_args(C, buf);
    }
}

void pawP_mangle_start(paw_Env *P, Buffer *buf, struct Compiler *C)
{
    pawL_init_buffer(P, buf);
    mangle_start(C, buf);
}

Str *pawP_mangle_finish(paw_Env *P, Buffer *buf, struct Compiler *C)
{
    Str *result = pawL_buffer_finish(P, buf);
    StringMap_insert(C, C->strings, result, result);
    return result;
}

Str *pawP_mangle_name(struct Compiler *C, Str const *modname, Str const *name, IrTypeList *types)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawP_mangle_start(P, &buf, C);
    if (modname != NULL)
        mangle_add_module(C, &buf, modname);
    mangle_add_name(C, &buf, name);
    mangle_types(C, &buf, types);
    return pawP_mangle_finish(P, &buf, C);
}

Str *pawP_mangle_attr(struct Compiler *C, Str const *modname, Str const *base, IrTypeList const *base_types, Str const *attr, IrTypeList const *attr_types)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawP_mangle_start(P, &buf, C);
    if (modname != NULL)
        mangle_add_module(C, &buf, modname);
    mangle_add_name(C, &buf, base);
    mangle_types(C, &buf, base_types);
    mangle_add_name(C, &buf, attr);
    mangle_types(C, &buf, attr_types);
    return pawP_mangle_finish(P, &buf, C);
}

void pawP_callback(struct Compiler *C, char const *name, void *arg)
{
    paw_Env *P = ENV(C);
    paw_Function *cb = CallbackMap_get(P, P->callbacks, SCAN_STR(C, name));
    if (cb != NULL) (*cb)(ENV(C), arg);
}

