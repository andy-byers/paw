// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compiler.h"
#include "stats.h"

#include "ast/ast.h"
#include "ir/type.h"
#include "middle/layout.h"
#include "parse/lex.h"
#include "sema/resolver.h"
#include "sema/solve.h"
#include "sema/unify.h"

#define COMPILER_ERROR(C_, Kind_, Modname_, ...) pawErr_##Kind_(C_, Modname_, __VA_ARGS__)

// All paw language keywords
//
// ORDER TokenKind
static char const *kKeywords[] = {
    "mod",
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

static Str const *basic_type_name(paw_Env *P, char const *name, enum BuiltinKind kind)
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
    P->string_cache[CSTR_INT8] = basic_type_name(P, "int8", BUILTIN_INT8);
    P->string_cache[CSTR_INT16] = basic_type_name(P, "int16", BUILTIN_INT16);
    P->string_cache[CSTR_INT32] = basic_type_name(P, "int32", BUILTIN_INT32);
    P->string_cache[CSTR_INT64] = basic_type_name(P, "int64", BUILTIN_INT64);
    P->string_cache[CSTR_ISIZE] = basic_type_name(P, "isize", BUILTIN_ISIZE);
    P->string_cache[CSTR_UINT8] = basic_type_name(P, "uint8", BUILTIN_UINT8);
    P->string_cache[CSTR_UINT16] = basic_type_name(P, "uint16", BUILTIN_UINT16);
    P->string_cache[CSTR_UINT32] = basic_type_name(P, "uint32", BUILTIN_UINT32);
    P->string_cache[CSTR_UINT64] = basic_type_name(P, "uint64", BUILTIN_UINT64);
    P->string_cache[CSTR_USIZE] = basic_type_name(P, "usize", BUILTIN_USIZE);
    P->string_cache[CSTR_FLOAT32] = basic_type_name(P, "float32", BUILTIN_FLOAT32);
    P->string_cache[CSTR_FLOAT64] = basic_type_name(P, "float64", BUILTIN_FLOAT64);
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
    P->string_cache[CSTR_MANUALLY_DROP] = pawS_new_fixed(P, "ManuallyDrop");
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

    pawP_parse_source_file(C, (Str const *)C->modname, input, ud);

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
        switch (IR_INT_KIND(type)) {
            case IR_INT8:
                return BUILTIN_INT8;
            case IR_INT16:
                return BUILTIN_INT16;
            case IR_INT32:
                return BUILTIN_INT32;
            case IR_INT64:
                return BUILTIN_INT64;
            case IR_ISIZE:
                return BUILTIN_ISIZE;
            case IR_UINT8:
                return BUILTIN_UINT8;
            case IR_UINT16:
                return BUILTIN_UINT16;
            case IR_UINT32:
                return BUILTIN_UINT32;
            case IR_UINT64:
                return BUILTIN_UINT64;
            case IR_USIZE:
                return BUILTIN_USIZE;
        }
    } else if (IrIsFloat(type)) {
        switch (IR_FLOAT_KIND(type)) {
            case IR_FLOAT32:
                return BUILTIN_FLOAT32;
            case IR_FLOAT64:
                return BUILTIN_FLOAT64;
        }
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
        } else if (base.value == C->builtins[BUILTIN_MANUALLY_DROP].did.value) {
            return BUILTIN_MANUALLY_DROP;
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
            return ir_new_unit(C);
        case BUILTIN_BOOL:
            return ir_new_bool(C);
        case BUILTIN_CHAR:
            return ir_new_char(C);
        case BUILTIN_INT8:
            return ir_new_int(C, IR_INT8);
        case BUILTIN_INT16:
            return ir_new_int(C, IR_INT16);
        case BUILTIN_INT32:
            return ir_new_int(C, IR_INT32);
        case BUILTIN_INT64:
            return ir_new_int(C, IR_INT64);
        case BUILTIN_ISIZE:
            return ir_new_int(C, IR_ISIZE);
        case BUILTIN_UINT8:
            return ir_new_int(C, IR_UINT8);
        case BUILTIN_UINT16:
            return ir_new_int(C, IR_UINT16);
        case BUILTIN_UINT32:
            return ir_new_int(C, IR_UINT32);
        case BUILTIN_UINT64:
            return ir_new_int(C, IR_UINT64);
        case BUILTIN_USIZE:
            return ir_new_int(C, IR_USIZE);
        case BUILTIN_FLOAT32:
            return ir_new_float(C, IR_FLOAT32);
        case BUILTIN_FLOAT64:
            return ir_new_float(C, IR_FLOAT64);
        case BUILTIN_STR:
            return ir_new_string(C);
        default:
            PAW_UNREACHABLE();
    }
}

Str const *pawP_scan_nstr(struct Compiler *C, char const *s, size_t n)
{
    paw_Env *P = ENV(C);
    Str *str = pawS_new_nstr(P, s, n);
    StringMap_insert(C->strings, str, NULL);
    return str;
}

Str const *pawP_format_string(struct Compiler *C, char const *fmt, ...)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    va_list arg;
    va_start(arg, fmt);
    lib_add_vfstring(P, &buf, fmt, arg);
    va_end(arg);

    return lib_buffer_finish(P, &buf);
}

static void register_builtin(struct Compiler *C, unsigned cstr, enum BuiltinKind kind)
{
    Str const *s = CACHED_STRING(ENV(C), cstr);
    C->builtins[kind] = (struct Builtin){
        .did = INVALID_DECL_ID,
        .name = s,
    };
    BuiltinMap_insert(C->builtin_lookup, s, &C->builtins[kind]);
}

void *compiler_alloc(struct Compiler *C, void *ptr, size_t size0, size_t size)
{
    return pool_alloc(C->pool, ptr, size0, size);
}

void compiler_free_pool(struct Compiler *C, struct Pool *pool)
{
    if (pool->next != NULL)
        pool->next->prev = pool->prev;
    if (pool->prev != NULL)
        pool->prev->next = pool->next;

    pool_uninit(pool);
    mem_free(ENV(C), pool);
}

struct Pool *compiler_new_pool(struct Compiler *C, struct PoolStats st)
{
    paw_Env *P = ENV(C);
    struct Pool *pool = mem_new(P, struct Pool);

    struct Pool *head = C->P->pool;
    head->prev->next = pool;
    pool->prev = head->prev;
    pool->next = head;
    head->prev = pool;

    pool_init(P, pool, 512, st);
    return pool;
}

static void set_extern_value(struct Compiler *C, char const *name, IrValue value)
{
    StringMap_insert(C->symbols, SCAN_STR(C, name), value.p);
}

DeclId pawP_next_decl_id(struct Compiler *C, int modno)
{
    return (DeclId){
        .value = (unsigned)++C->decl_count,
        .modno = (unsigned)modno,
    };
}

#if !defined(NDEBUG)
struct TestKey {int index; int value;};
#define TEST_KEY(Index_, Value_) \
    (struct TestKey){Index_, Value_}
#define TEST_HASH(Key_) ((HashValue)(Key_).index)
#define TEST_EQUALS(Lhs_, Rhs_) (\
        (Lhs_).index == (Rhs_).index \
        && (Lhs_).value == (Rhs_).value)
DEFINE_MAP(TestMap, TEST_HASH, TEST_EQUALS, struct TestKey, int)
static void test_map(paw_Env *P)
{
    TestMap *m = TestMap_new(ALLOCATOR_OF(P));

    TestMap_insert(m, TEST_KEY(1, 10), 1);
    TestMap_insert(m, TEST_KEY(2, 20), 2);
    TestMap_insert(m, TEST_KEY(2, 30), 3);
    TestMap_insert(m, TEST_KEY(3, 40), 4);
    TestMap_insert(m, TEST_KEY(3, 50), 5);

    assert(*TestMap_get(m, TEST_KEY(1, 10)) == 1);
    assert(*TestMap_get(m, TEST_KEY(2, 20)) == 2);
    assert(*TestMap_get(m, TEST_KEY(2, 30)) == 3);
    assert(*TestMap_get(m, TEST_KEY(3, 40)) == 4);
    assert(*TestMap_get(m, TEST_KEY(3, 50)) == 5);

    TestMap_remove(m, TEST_KEY(2, 20));
    TestMap_remove(m, TEST_KEY(3, 50));

    for (int i = 0; i < 100; ++i)
        TestMap_insert(m, TEST_KEY(i % 10, i + 100), i);
    for (int i = 0; i < 100; ++i)
        assert(*TestMap_get(m, TEST_KEY(i % 10, i + 100)) == i);
    for (int i = 0; i < 100; ++i)
        TestMap_remove(m, TEST_KEY(i % 10, i + 100));

    assert(*TestMap_get(m, TEST_KEY(1, 10)) == 1);
    assert(TestMap_get(m, TEST_KEY(2, 20)) == NULL);
    assert(*TestMap_get(m, TEST_KEY(2, 30)) == 3);
    assert(*TestMap_get(m, TEST_KEY(3, 40)) == 4);
    assert(TestMap_get(m, TEST_KEY(3, 50)) == NULL);
}
static void test_bitset(paw_Env *P)
{
    BitSet *a = bitset_new(ALLOCATOR_OF(P), 70);

    bitset_set(a, 10);
    bitset_set_range(a, 15, 55);
    bitset_clear(a, 50);
    bitset_clear_range(a, 25, 30);

    paw_assert(bitset_get(a, 9) == PAW_FALSE);
    paw_assert(bitset_get(a, 10) == PAW_TRUE);
    paw_assert(bitset_get(a, 11) == PAW_FALSE);
    paw_assert(bitset_get(a, 14) == PAW_FALSE);
    paw_assert(bitset_get(a, 15) == PAW_TRUE);
    paw_assert(bitset_get(a, 24) == PAW_TRUE);
    paw_assert(bitset_get(a, 25) == PAW_FALSE);
    paw_assert(bitset_get(a, 29) == PAW_FALSE);
    paw_assert(bitset_get(a, 30) == PAW_TRUE);
    paw_assert(bitset_get(a, 50) == PAW_FALSE);
    paw_assert(bitset_get(a, 54) == PAW_TRUE);
    paw_assert(bitset_get(a, 55) == PAW_FALSE);

    BitSet *b = bitset_clone(a);
    paw_assert(bitset_equals(a, b));
    bitset_set_range(b, 65, 70);
    paw_assert(!bitset_equals(a, b));
    bitset_set_range(a, 65, 70);
    paw_assert(bitset_equals(a, b));
}
#endif // !defined(NDEBUG)

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, Str const *modname, Str const *pathname, Str const *dirname)
{
#if !defined(NDEBUG)
    test_map(P);
    test_bitset(P);
#endif // !defined(NDEBUG)

    *C = (struct Compiler){
        .pool = P->pool,
        .pathname = pathname,
        .modname = modname,
        .dirname = dirname,
        .a = C->a, // already initialized
        .dm = dm,
        .P = P,

        .std_math_NAN = INVALID_DECL_ID,
        .std_math_INFINITY = INVALID_DECL_ID,
    };

    C->searchers = *StrMap_get(P->registry, CACHED_STRING(P, CSTR_KSEARCHERS));
    C->symbols = *StrMap_get(P->registry, CACHED_STRING(P, CSTR_KSYMBOLS));
    C->stats = P->stats;

    C->aux_stats = (struct PoolStats){
        .bytes_alloc = stats_new(P, C->stats, "memory.aux.bytes_allocated"),
        .bytes_used = stats_new(P, C->stats, "memory.aux.bytes_used"),
        .num_alloc = stats_new(P, C->stats, "memory.aux.num_allocations"),
    };
    C->ast_pool = compiler_new_pool(C, (struct PoolStats){
        .num_alloc = stats_new(P, C->stats, "memory.ast.num_allocations"),
        .bytes_alloc = stats_new(P, C->stats, "memory.ast.bytes_allocated"),
        .bytes_used = stats_new(P, C->stats, "memory.ast.bytes_used"),
    });
    C->hir_pool = compiler_new_pool(C, (struct PoolStats){
        .num_alloc = stats_new(P, C->stats, "memory.hir.num_allocations"),
        .bytes_alloc = stats_new(P, C->stats, "memory.hir.bytes_allocated"),
        .bytes_used = stats_new(P, C->stats, "memory.hir.bytes_used"),
    });
    C->mir_pool = compiler_new_pool(C, (struct PoolStats){
        .num_alloc = stats_new(P, C->stats, "memory.mir.num_allocations"),
        .bytes_alloc = stats_new(P, C->stats, "memory.mir.bytes_allocated"),
        .bytes_used = stats_new(P, C->stats, "memory.mir.bytes_used"),
    });

    C->strings = StringMap_new(ALLOCATOR_OF(C));

    C->main_name = SCAN_STR(C, "main");

    C->default_int = ir_new_int(C, IR_INT64);
    C->default_float = ir_new_float(C, IR_FLOAT64);

    C->modinfo = ModuleInfo_new(ALLOCATOR_OF(C));
    C->builtin_lookup = BuiltinMap_new(ALLOCATOR_OF(C));
    C->hir_types = HirTypeMap_new(ALLOCATOR_OF(C));
    C->self_types = NodeMap_new(ALLOCATOR_OF(C));
    C->layouts = IrTypeLayouts_new(ALLOCATOR_OF(C));

    C->def_types = DefTypeMap_new(ALLOCATOR_OF(C));
    C->variant_defs = VariantDefMap_new(ALLOCATOR_OF(C));
    C->trait_defs = TraitDefMap_new(ALLOCATOR_OF(C));
    C->adt_defs = AdtDefMap_new(ALLOCATOR_OF(C));
    C->fn_defs = FnDefMap_new(ALLOCATOR_OF(C));
    C->impl_defs = ImplMap_new(ALLOCATOR_OF(C));
    C->generic_defs = GenericDefMap_new(ALLOCATOR_OF(C));
    C->ir_def_kinds = IrDefKinds_new(ALLOCATOR_OF(C));
    C->ir_generic_args = IrGenericTypes_new(ALLOCATOR_OF(C));
    C->ir_decl_args = IrDeclArgs_new(ALLOCATOR_OF(C));
    C->ir_trait_bounds = IrTraitBounds_new(ALLOCATOR_OF(C));
    C->ir_assoc_items = IrAssocItemMap_new(ALLOCATOR_OF(C));
    C->ir_constraints = IrConstraintsMap_new(ALLOCATOR_OF(C));
    C->pending_constants = IrPendingConstants_new(ALLOCATOR_OF(C));
    C->resolved_constants = IrResolvedConstants_new(ALLOCATOR_OF(C));
    C->const_obligations = IrObligations_new(ALLOCATOR_OF(C));
    C->method_selections = MethodSelectionMap_new(ALLOCATOR_OF(C));

    C->source_span_refs = SourceSpanRefs_new(ALLOCATOR_OF(C));

    C->typesystem.primitives.int_t = IrTypeList_new(ALLOCATOR_OF(C));
    C->typesystem.primitives.float_t = IrTypeList_new(ALLOCATOR_OF(C));
    IrTypeList_resize(C->typesystem.primitives.int_t, IR_NUM_INT_KINDS);
    IrTypeList_resize(C->typesystem.primitives.float_t, IR_NUM_FLOAT_KINDS);
    C->typesystem.ptrs = TypeCollection_new(ALLOCATOR_OF(C));
    C->typesystem.slices = TypeCollection_new(ALLOCATOR_OF(C));
    C->typesystem.arrays = TypeCollection_new(ALLOCATOR_OF(C));
    C->typesystem.adts = TypeCollection_new(ALLOCATOR_OF(C));
    C->typesystem.types = TypeCollection_new(ALLOCATOR_OF(C));

    C->impls.inherent = IrTypeMap_new(ALLOCATOR_OF(C));
    C->impls.trait = IrTypeMap_new(ALLOCATOR_OF(C));
    C->impls.blanket = IrDefs_new(ALLOCATOR_OF(C));

    C->segtab = SegmentTable_new(ALLOCATOR_OF(C));
    C->upvtab = UpvalueTable_new(ALLOCATOR_OF(C));

    ModuleInfo_push(C->modinfo, (struct Module){
                .pathname = pathname,
                .dirname = dirname,
                .name = modname,
            });

    C->ast = ast_new(C);

    C->U = unifier_new(C);

    C->S = ir_push_solver(C);

    // builtin primitives
    register_builtin(C, CSTR_UNIT, BUILTIN_UNIT);
    register_builtin(C, CSTR_BOOL, BUILTIN_BOOL);
    register_builtin(C, CSTR_CHAR, BUILTIN_CHAR);
    register_builtin(C, CSTR_INT8, BUILTIN_INT8);
    register_builtin(C, CSTR_INT16, BUILTIN_INT16);
    register_builtin(C, CSTR_INT32, BUILTIN_INT32);
    register_builtin(C, CSTR_INT64, BUILTIN_INT64);
    register_builtin(C, CSTR_ISIZE, BUILTIN_ISIZE);
    register_builtin(C, CSTR_UINT8, BUILTIN_UINT8);
    register_builtin(C, CSTR_UINT16, BUILTIN_UINT16);
    register_builtin(C, CSTR_UINT32, BUILTIN_UINT32);
    register_builtin(C, CSTR_UINT64, BUILTIN_UINT64);
    register_builtin(C, CSTR_USIZE, BUILTIN_USIZE);
    register_builtin(C, CSTR_FLOAT32, BUILTIN_FLOAT32);
    register_builtin(C, CSTR_FLOAT64, BUILTIN_FLOAT64);
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
    register_builtin(C, CSTR_MANUALLY_DROP, BUILTIN_MANUALLY_DROP);

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
        compiler_free_pool(P->C, pool);
        pool = next;
    }

    P->modname = P->pathname = NULL;
}

struct BitSet {
    struct BitSetInner *inner;
    int num_bits;
};

#define CHUNKSZ (int)(sizeof(BitChunk) * 8)

#define BITS_TO_CHUNKS(NumBits_) \
    (((NumBits_) + CHUNKSZ - 1) / CHUNKSZ)
#define BITSET_INDICES(Set_, I_, Pos_, Bit_) \
    paw_assert(0 <= (I_) && (I_) < (Set_)->num_bits); \
    const int Pos_ = (I_) / CHUNKSZ; \
    const int Bit_ = (I_) % CHUNKSZ

DEFINE_LIST(BitSetInner, BitChunk)

BitSet *bitset_new(struct Allocator const *a, int num_bits)
{
    paw_assert(num_bits > 0);
    BitSetInner *inner = BitSetInner_new(a);
    int const num_chunks = BITS_TO_CHUNKS(num_bits);
    BitSetInner_resize(inner, num_chunks);
    memset(inner->data, 0, (size_t)num_chunks * sizeof(inner->data[0]));

    BitSet *set = a->alloc(a->ctx, NULL, 0, sizeof(*set));
    *set = (BitSet){
        .inner = inner,
        .num_bits = num_bits,
    };
    return set;
}

BitSet *bitset_clone(BitSet const *set)
{
    BitSet *r = bitset_new(set->inner->a, set->num_bits);
    for (int i = 0; i < set->inner->count; ++i)
        LIST_AT(r->inner, i) = LIST_AT(set->inner, i);
    return r;
}

paw_Bool bitset_equals(BitSet const *x, BitSet const *y)
{
    paw_assert(x->num_bits == y->num_bits);
    paw_assert(x->num_bits != 0);

    size_t const num_full_chunks = (size_t)x->num_bits / CHUNKSZ;
    size_t const remainder_bits = (size_t)x->num_bits % CHUNKSZ;
    BitChunk const remainder_mask = (1ULL << remainder_bits) - 1;
    return memcmp(LIST_BEGIN(x->inner), LIST_BEGIN(y->inner),
            num_full_chunks * sizeof(x->inner->data[0])) == 0
        // mask off unused bits in the last chunk which have indeterminate values
        && ((LIST_LAST(x->inner) ^ LIST_LAST(y->inner)) & remainder_mask) == 0;
}

void bitset_set(BitSet *set, int i)
{
    BITSET_INDICES(set, i, pos, bit);
    BitChunk *bc = &LIST_AT(set->inner, pos);
    *bc = *bc | (1ULL << bit);
}

void bitset_set_range(BitSet *bs, int i, int j)
{
    while (i < j) bitset_set(bs, i++);
}

int bitset_len(BitSet const *set)
{
    return set->num_bits;
}

paw_Bool bitset_get(BitSet const *set, int i)
{
    BITSET_INDICES(set, i, pos, bit);
    BitChunk const bc = BitSetInner_get(set->inner, pos);
    return (bc >> bit) & 1;
}

void bitset_clear(BitSet *set, int i)
{
    BITSET_INDICES(set, i, pos, bit);
    BitChunk *bc = &LIST_AT(set->inner, pos);
    *bc = *bc & ~(1ULL << bit);
}

void bitset_clear_range(BitSet *bs, int i, int j)
{
    while (i < j) bitset_clear(bs, i++);
}

void bitset_and(BitSet *x, BitSet const *y)
{
    paw_assert(x->num_bits == y->num_bits);

    BitChunk *a;
    BitChunk const *b;
    LIST_ZIP (x->inner, a, y->inner, b)
        *a &= *b;
}

void bitset_or(BitSet *x, BitSet const *y)
{
    paw_assert(x->num_bits == y->num_bits);

    BitChunk *a;
    BitChunk const *b;
    LIST_ZIP (x->inner, a, y->inner, b)
        *a |= *b;
}

void bitset_xor(BitSet *x, BitSet const *y)
{
    paw_assert(x->num_bits == y->num_bits);

    BitChunk *a;
    BitChunk const *b;
    LIST_ZIP (x->inner, a, y->inner, b)
        *a ^= *b;
}

paw_Bool pawP_check_extern(struct Compiler *C, struct Annotations *annos, struct Annotation *panno)
{
    if (annos == NULL)
        return PAW_FALSE;
    struct Annotation *pa;
    LIST_FOREACH (annos, pa) {
        if (pawS_eq(pa->name, CSTR(C, CSTR_EXTERN))) {
            *panno = *pa;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

paw_Bool pawP_get_extern_value(struct Compiler *C, Str const *name, IrValue *result)
{
    void *const *pvalue = StringMap_get(C->symbols, name);
    if (pvalue == NULL) return PAW_FALSE;
    result->p = *pvalue;
    return PAW_TRUE;
}

void pawP_callback(struct Compiler *C, char const *name, void *arg)
{
    paw_Env *P = ENV(C);
    paw_Function *cb = CallbackMap_get(P->callbacks, SCAN_STR(C, name));
    if (cb != NULL) (*cb)(ENV(C), arg);
}

