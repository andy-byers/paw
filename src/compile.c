// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "api.h"
#include "ast.h"
#include "debug.h"
#include "error.h"
#include "gc.h"
#include "hir.h"
#include "ir_type.h"
#include "layout.h"
#include "lex.h"
#include "map.h"
#include "resolve.h"
#include "rtti.h"
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
    "trait",
    "const",
    "inline",
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
    for (uint16_t i = 0; i < PAW_COUNTOF(kKeywords); ++i) {
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
    P->string_cache[CSTR_STRING] = pawS_new_fixed(P, "String");
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

    pawP_parse_module(C, C->modname, input, ud);

    pawP_resolve_names(C);
    pawP_lower_ast(C);
    pawP_check_types(C);
    pawP_lower_hir(C);
    pawP_generate_code(C);
}

enum BuiltinKind pawP_type2code(struct Compiler *C, struct IrType *type)
{
    if (IrIsAdt(type)) {
        DeclId const base = IR_TYPE_DID(type);
        if (base.value == C->builtins[BUILTIN_UNIT].did.value) {
            return BUILTIN_UNIT;
        } else if (base.value == C->builtins[BUILTIN_BOOL].did.value) {
            return BUILTIN_BOOL;
        } else if (base.value == C->builtins[BUILTIN_CHAR].did.value) {
            return BUILTIN_CHAR;
        } else if (base.value == C->builtins[BUILTIN_INT].did.value) {
            return BUILTIN_INT;
        } else if (base.value == C->builtins[BUILTIN_FLOAT].did.value) {
            return BUILTIN_FLOAT;
        } else if (base.value == C->builtins[BUILTIN_STR].did.value) {
            return BUILTIN_STR;
        } else if (base.value == C->builtins[BUILTIN_LIST].did.value) {
            return BUILTIN_LIST;
        } else if (base.value == C->builtins[BUILTIN_MAP].did.value) {
            return BUILTIN_MAP;
        } else if (base.value == C->builtins[BUILTIN_OPTION].did.value) {
            return BUILTIN_OPTION;
        } else if (base.value == C->builtins[BUILTIN_RESULT].did.value) {
            return BUILTIN_RESULT;
        } else if (base.value == C->builtins[BUILTIN_HASH].did.value) {
            return BUILTIN_HASH;
        } else if (base.value == C->builtins[BUILTIN_EQUALS].did.value) {
            return BUILTIN_EQUALS;
        } else if (base.value == C->builtins[BUILTIN_COMPARE].did.value) {
            return BUILTIN_COMPARE;
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

struct IrType *pawP_builtin_type(struct Compiler *C, enum BuiltinKind code)
{
    DeclId const did = pawP_builtin_info(C, code)->did;
    return pawIr_get_def_type(C, did);
}

Str *pawP_scan_nstr(struct Compiler *C, Tuple *map, char const *s, size_t n)
{
    paw_Env *P = ENV(C);
    Value const *pv = pawC_pushns(P, s, n);
    pawMap_insert(P, map, pv, pv);
    pawC_pop(P);
    CHECK_GC(P);
    return V_STR(*pv);
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

    Str *s = pawP_scan_nstr(C, C->strings, buf.data, buf.size);
    pawL_discard_result(P, &buf);
    return s;
}

static void define_prelude_adt(struct Compiler *C, unsigned cstr, enum BuiltinKind kind)
{
    Str *s = CACHED_STRING(ENV(C), cstr);
    C->builtins[kind] = (struct Builtin){
        .did = NO_DECL,
        .name = s,
    };
    BuiltinMap_insert(C, C->builtin_lookup, s, &C->builtins[kind]);
}

void *pawP_alloc(paw_Env *P, struct Pool *pool, void *ptr, size_t size0, size_t size)
{
    return pawK_pool_alloc(P, pool, ptr, size0, size);
}

static void pool_free(paw_Env *P, struct Pool *pool)
{
    if (pool->next != NULL)
        pool->next->prev = pool->prev;
    if (pool->prev != NULL)
        pool->prev->next = pool->next;

    pawK_pool_uninit(P, pool);
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

    struct Pool *head = &C->dm->pool;
    head->prev->next = pool;
    pool->prev = head->prev;
    pool->next = head;
    head->prev = pool;

    pawK_pool_init(P, pool, 512, st);
    return pool;
}

#define FIRST_ARENA_SIZE 4096

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, char const *modname)
{
    pawRtti_uninit(P);

    dm->pool.prev = dm->pool.next = &dm->pool;
    pawK_pool_init(P, &dm->pool, FIRST_ARENA_SIZE, (struct PoolStats){0});

    *C = (struct Compiler){
        .pool = &dm->pool,
        .dm = dm,
        .P = P,
    };
    C->stats = Statistics_new(C);

    // Create statistics for tracking compiler memory usage. Main pool statistics
    // must be added after-the-fact, since the main pool itself is used to allocate
    // the "struct Statistic" objects.
    C->pool->st = (struct PoolStats){
                .bytes_alloc = pawStats_new(C, "memory.main.bytes_allocated"),
                .bytes_used = pawStats_new(C, "memory.main.bytes_used"),
                .num_alloc = pawStats_new(C, "memory.main.num_allocations"),
            };
    C->aux_stats = (struct PoolStats){
                .bytes_alloc = pawStats_new(C, "memory.aux.bytes_allocated"),
                .bytes_used = pawStats_new(C, "memory.aux.bytes_used"),
                .num_alloc = pawStats_new(C, "memory.aux.num_allocations"),
            };
    C->ast_pool = pawP_pool_new(C, (struct PoolStats){
                .num_alloc = pawStats_new(C, "memory.ast.num_allocations"),
                .bytes_alloc = pawStats_new(C, "memory.ast.bytes_allocated"),
                .bytes_used = pawStats_new(C, "memory.ast.bytes_used"),
            });
    C->hir_pool = pawP_pool_new(C, (struct PoolStats){
                .num_alloc = pawStats_new(C, "memory.hir.num_allocations"),
                .bytes_alloc = pawStats_new(C, "memory.hir.bytes_allocated"),
                .bytes_used = pawStats_new(C, "memory.hir.bytes_used"),
            });
    C->mir_pool = pawP_pool_new(C, (struct PoolStats){
                .num_alloc = pawStats_new(C, "memory.mir.num_allocations"),
                .bytes_alloc = pawStats_new(C, "memory.mir.bytes_allocated"),
                .bytes_used = pawStats_new(C, "memory.mir.bytes_used"),
            });

    paw_new_map(P, 0, 0);
    C->strings = V_TUPLE(P->top.p[-1]);

    paw_new_map(P, 0, 0);
    P->constants = P->top.p[-1];
    paw_new_map(P, 0, 0);
    P->functions = P->top.p[-1];
    paw_pop(P, 2);

    C->globals = GlobalList_new(C);
    C->modnames = ModuleNames_new(C);
    C->builtin_lookup = BuiltinMap_new(C);
    C->hir_types = HirTypeMap_new(C);
    C->def_types = DefTypeMap_new(C);
    C->variant_defs = VariantDefMap_new(C);
    C->adt_defs = AdtDefMap_new(C);
    C->fn_defs = FnDefMap_new(C);

    C->layouts = IrLayoutMap_new(C);
    C->segtab = SegmentTable_new(C);

    C->rtti = RttiMap_new(C);
    C->traits = TraitMap_new(C);
    C->trait_owners = TraitOwners_new(C);

    C->modname = P->modname = SCAN_STR(C, modname);
    C->ast = pawAst_new(C);

    C->U = P_ALLOC(C, NULL, 0, sizeof(struct Unifier));
    C->U->C = C;

    // builtin primitives
    define_prelude_adt(C, CSTR_UNIT, BUILTIN_UNIT);
    define_prelude_adt(C, CSTR_BOOL, BUILTIN_BOOL);
    define_prelude_adt(C, CSTR_CHAR, BUILTIN_CHAR);
    define_prelude_adt(C, CSTR_INT, BUILTIN_INT);
    define_prelude_adt(C, CSTR_FLOAT, BUILTIN_FLOAT);
    define_prelude_adt(C, CSTR_STR, BUILTIN_STR);

    // builtin containers (in Paw code, List<T> can be written as [T], and
    // Map<K, V> as [K: V])
    define_prelude_adt(C, CSTR_LIST, BUILTIN_LIST);
    define_prelude_adt(C, CSTR_MAP, BUILTIN_MAP);

    // builtin ADTs
    define_prelude_adt(C, CSTR_OPTION, BUILTIN_OPTION);
    define_prelude_adt(C, CSTR_RESULT, BUILTIN_RESULT);
    define_prelude_adt(C, CSTR_RANGE, BUILTIN_RANGE);
    define_prelude_adt(C, CSTR_RANGE_TO, BUILTIN_RANGE_TO);
    define_prelude_adt(C, CSTR_RANGE_FROM, BUILTIN_RANGE_FROM);
    define_prelude_adt(C, CSTR_RANGE_FULL, BUILTIN_RANGE_FULL);
    define_prelude_adt(C, CSTR_RANGE_INCLUSIVE, BUILTIN_RANGE_INCLUSIVE);
    define_prelude_adt(C, CSTR_RANGE_TO_INCLUSIVE, BUILTIN_RANGE_TO_INCLUSIVE);

    // builtin traits
    define_prelude_adt(C, CSTR_HASH, BUILTIN_HASH);
    define_prelude_adt(C, CSTR_EQUALS, BUILTIN_EQUALS);
    define_prelude_adt(C, CSTR_COMPARE, BUILTIN_COMPARE);
}

void pawP_teardown(paw_Env *P, struct DynamicMem *dm)
{
    struct Pool *pool = dm->pool.next;
    while (pool != &dm->pool) {
        struct Pool *next = pool->next;
        pool_free(P, pool);
        pool = next;
    }

    pawK_pool_uninit(P, &dm->pool);
    pawM_free_vec(P, dm->source.data, dm->source.size);
    pawM_free_vec(P, dm->scratch.data, dm->scratch.alloc);
}

#define LOG(dg, ...) PAWD_LOG(ENV((dg)->C), __VA_ARGS__)

struct DefState {
    struct DefState *outer;
    struct IrType *type;
    struct Def *def;
    int index;
};

struct DefGenerator {
    struct DefState *ds;
    struct ModuleInfo *m;
    struct ItemList *items;
    struct Compiler *C;
};

static void enter_def(struct DefGenerator *dg, struct DefState *ds, struct IrType *type, struct Def *def)
{
    *ds = (struct DefState){
        .outer = dg->ds,
        .type = type,
        .def = def,
    };
    dg->ds = ds;
}

static void leave_def(struct DefGenerator *dg)
{
    dg->ds = dg->ds->outer;
}

static RttiType *lookup_type(struct DefGenerator *dg, struct IrType *type)
{
    if (type == NULL)
        return NULL;
    RttiType *const *ptype = RttiMap_get(dg->C, dg->C->rtti, type);
    return ptype != NULL ? *ptype : NULL;
}

static void map_types(struct DefGenerator *dg, struct IrType *type, RttiType *rtti)
{
    paw_Env *P = ENV(dg->C);
    RttiMap_insert(dg->C, dg->C->rtti, type, rtti);
}

static RttiType *new_type(struct DefGenerator *, struct IrType *, ItemId);
#define MAKE_TYPE(dg, t, did) new_type(dg, IR_CAST_TYPE(t), did)->hdr.code

static void init_type_list(struct DefGenerator *dg, struct IrTypeList *x, paw_Type *y, ItemId iid)
{
    if (x == NULL)
        return;
    for (int i = 0; i < x->count; ++i) {
        y[i] = MAKE_TYPE(dg, x->data[i], iid);
    }
}

#define LEN(L) ((L) != NULL ? (L)->count : 0)

static RttiType *new_type(struct DefGenerator *dg, struct IrType *src, ItemId iid)
{
    RttiType *dst = lookup_type(dg, src);
    if (dst != NULL) return dst;

    struct IrLayout layout = pawIr_compute_layout(dg->C, src);

    paw_Env *P = ENV(dg->C);
    switch (IR_KINDOF(src)) {
        case kIrAdt: {
            struct IrAdt *adt = IrGetAdt(src);
            dst = pawRtti_new_adt(P, iid, LEN(adt->types), layout.size);
            init_type_list(dg, adt->types, dst->subtypes, iid);
            break;
        }
        case kIrSignature: {
            struct IrSignature *fsig = IrGetSignature(src);
            dst = pawRtti_new_signature(P, iid, fsig->params->count);
            init_type_list(dg, fsig->params, dst->subtypes, iid);
            dst->fdef.result = MAKE_TYPE(dg, fsig->result, iid);
            break;
        }
        case kIrFuncPtr: {
            struct IrFuncPtr *fptr = IrGetFuncPtr(src);
            dst = pawRtti_new_func_ptr(P, fptr->params->count);
            init_type_list(dg, fptr->params, dst->subtypes, -1);
            dst->fptr.result = MAKE_TYPE(dg, fptr->result, -1);
            break;
        }
        case kIrTraitObj: {
            struct IrTraitObj *trait = IrGetTraitObj(src);
            dst = pawRtti_new_trait(P);
            break;
        }
        case kIrNever: {
            dst = pawRtti_new_never(P);
            break;
        }
        default: { // kIrTuple
            struct IrTuple *tuple = IrGetTuple(src);
            dst = pawRtti_new_tuple(P, tuple->elems->count, layout.size);
            init_type_list(dg, tuple->elems, dst->subtypes, -1);
        }
    }
    return dst;
}

static Str *get_modname(struct DefGenerator *dg, DeclId did)
{
    return HirModuleList_get(dg->C->hir->modules, did.modno).name;
}

static struct Def *new_adt_def(struct DefGenerator *dg, struct IrAdtDef *d, struct IrType *type)
{
    paw_Env *P = ENV(dg->C);
    int const n = d->is_struct
        ? LEN(K_LIST_FIRST(d->variants)->fields)
        : LEN(d->variants);
    struct Def *def = pawRtti_new_adt_def(P, n);
    RttiType *ty = new_type(dg, type, def->hdr.iid);
    def->adt.kind = DEF_ADT;
    def->adt.modname = get_modname(dg, d->did);
    def->adt.code = ty->hdr.code;
    def->adt.name = d->name;
    def->adt.is_struct = d->is_struct;
    def->adt.is_inline = d->is_inline;
    def->adt.is_pub = d->is_pub;
    return def;
}

static struct Def *new_fn_def(struct DefGenerator *dg, struct IrFnDef *d, struct IrType *type)
{
    paw_Env *P = ENV(dg->C);
    struct Def *def = pawRtti_new_func_def(P, LEN(d->params));
    RttiType *ty = new_type(dg, type, def->hdr.iid);
    def->func.kind = DEF_FUNC;
    def->func.ntypes = LEN(d->params);
    def->func.iid = def->hdr.iid;
    def->func.modname = get_modname(dg, d->did);
    def->func.code = ty->hdr.code;
    def->func.name = d->name;
    def->func.is_pub = d->is_pub;
    return def;
}

static void allocate_other_type(struct DefGenerator *dg, struct IrType *type)
{
    RttiType *rtti = new_type(dg, type, -1);
    map_types(dg, type, rtti);
}

static void allocate_adt_def(struct DefGenerator *dg, struct IrType *type)
{
    if (!IrIsAdt(type)) {
        allocate_other_type(dg, type);
        return;
    }
    struct IrAdt *t = IrGetAdt(type);
    struct IrAdtDef *d = pawIr_get_adt_def(dg->C, t->did);
    struct Def *def = new_adt_def(dg, d, type);
    RttiType *rtti = RTTI_TYPE(ENV(dg->C), def->func.code);
    map_types(dg, type, rtti);
}

static void define_params(struct DefGenerator *dg, struct IrParams *params)
{
    // TODO: define fields for RTTI
}

static void connect_adt_def(struct DefGenerator *dg, struct IrType *mono)
{
    // paw_Env *P = ENV(dg->C);
    // struct HirDecl *decl = pawHir_get_decl(dg->C, IR_TYPE_DID(mono));
    // struct HirAdtDecl *d = HirGetAdtDecl(decl);
    // RttiType *ty = lookup_type(dg, mono);
    // struct Def *def = RT_DEF(P, ty->adt.iid);

    // struct DefState ds;
    // enter_def(dg, &ds, mono, def);
    // define_decl_list(dg, d->fields);
    // leave_def(dg);
}

static void allocate_types(struct DefGenerator *dg, struct IrTypeList *types)
{
    struct IrType *const *ptype;
    K_LIST_FOREACH (types, ptype)
        allocate_adt_def(dg, *ptype);
    K_LIST_FOREACH (types, ptype)
        connect_adt_def(dg, *ptype);
}

static struct ItemSlot allocate_item(struct DefGenerator *dg, struct Mir *body)
{
    paw_Env *P = ENV(dg->C);
    struct IrSignature *t = IrGetSignature(body->type);
    int const ntypes = t->types != NULL ? t->types->count : 0;
    struct IrFnDef *d = pawIr_get_fn_def(dg->C, t->did);
    struct Def *def = new_fn_def(dg, d, body->type);
    RttiType *self = lookup_type(dg, body->self);
    def->func.self = self != NULL ? self->adt.code : -1;
    // ".vid" is the index of the Value slot where this function will live
    // at runtime. Functions are placed after the global constants section.
    def->func.vid = dg->items->count + dg->C->globals->count;

    struct DefState ds;
    enter_def(dg, &ds, body->type, def);
    define_params(dg, d->params);
    leave_def(dg);

    RttiType *rtti = RTTI_TYPE(P, def->func.code);
    rtti->fdef.iid = def->func.iid;
    return (struct ItemSlot){
        .name = body->name,
        .mir = body,
        .rtti = rtti,
        .did = d->did,
    };
}

static void allocate_items(struct DefGenerator *dg, struct BodyList *bodies)
{
    struct Mir *const *pbody;
    K_LIST_FOREACH (bodies, pbody) {
        struct Mir *body = *pbody;
        if (body->self == NULL || !IrIsTraitObj(body->self)) {
            struct ItemSlot item = allocate_item(dg, body);
            ItemList_push(dg->C, dg->items, item);
            map_types(dg, body->type, item.rtti);
        }
    }

    struct GlobalInfo const *pinfo;
    K_LIST_FOREACH (dg->C->globals, pinfo) {
    }
}

struct ItemList *pawP_allocate_defs(struct Compiler *C, struct BodyList *bodies, struct IrTypeList *types)
{
    struct DefGenerator dg = {
        .items = ItemList_new(C),
        .C = C,
    };
    allocate_types(&dg, types);
    allocate_items(&dg, bodies);
    return dg.items;
}

#define CHUNKSZ(b) sizeof(K_LIST_AT(b, 0))

struct BitSet *pawP_bitset_new(struct Compiler *C, int count)
{
    paw_assert(count > 0);
    struct BitSet *set = BitSet_new(C);
    int const n = (count + CHUNKSZ(set) - 1) / CHUNKSZ(set);
    BitSet_reserve(C, set, n);
    memset(set->data, 0, CAST_SIZE(n) * CHUNKSZ(set));
    set->count = count;
    return set;
}

#define BITSET_INDEX(set, i, pos, bit)          \
    paw_assert(0 <= (i) && (i) < (set)->count); \
    const int pos = (i) / CHUNKSZ(set);         \
    const int bit = (i) % CHUNKSZ(set)

void pawP_bitset_set(struct BitSet *set, int i)
{
    BITSET_INDEX(set, i, pos, bit);
    BitChunk *bc = &K_LIST_AT(set, pos);
    *bc = *bc | (1ULL << bit);
}

void pawP_bitset_set_range(struct BitSet *bs, int i, int j)
{
    while (i < j)
        pawP_bitset_set(bs, i++);
}

int pawP_bitset_count(struct BitSet const *set)
{
    return set->count;
}

paw_Bool pawP_bitset_get(struct BitSet const *set, int i)
{
    BITSET_INDEX(set, i, pos, bit);
    BitChunk const bc = BitSet_get(set, pos);
    return (bc >> bit) & 1;
}

void pawP_bitset_clear(struct BitSet *set, int i)
{
    BITSET_INDEX(set, i, pos, bit);
    BitChunk *bc = &K_LIST_AT(set, pos);
    *bc = *bc & ~(1ULL << bit);
}

void pawP_bitset_clear_range(struct BitSet *bs, int i, int j)
{
    while (i < j)
        pawP_bitset_clear(bs, i++);
}

void pawP_bitset_and(struct BitSet *a, struct BitSet const *b)
{
    paw_assert(a->count == b->count);
    int const n = a->count / CHUNKSZ(a);

    for (int i = 0; i < n; ++i) {
        BitChunk *bc = &K_LIST_AT(a, i);
        *bc = *bc & BitSet_get(b, i);
    }
}

void pawP_bitset_or(struct BitSet *a, struct BitSet const *b)
{
    paw_assert(a->count == b->count);
    int const n = a->count / CHUNKSZ(a);

    for (int i = 0; i < n; ++i) {
        BitChunk *bc = &K_LIST_AT(a, i);
        *bc = *bc | BitSet_get(b, i);
    }
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

Value const *pawP_get_extern_value(struct Compiler *C, Str const *name)
{
    paw_Env *P = ENV(C);
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);
    Tuple *symbols = V_TUPLE(P->top.p[-1]);

    Value const *pval = pawMap_get(P, symbols, &P2V(name));

    paw_pop(P, 1); // pop 'symbols'
    return pval;
}

static RttiType *lookup_rtti(struct Compiler *C, struct IrType *type)
{
    RttiType **prtti = RttiMap_get(C, C->rtti, type);
    return prtti != NULL ? *prtti : NULL;
}

static void mangle_type(struct Compiler *C, Buffer *buf, struct IrType *type)
{
    RttiType *t = lookup_rtti(C, type);
    paw_assert(t != NULL);

    pawRtti_mangle_add_arg(ENV(C), buf, t->hdr.code);
}

static void mangle_types(struct Compiler *C, Buffer *buf, struct IrTypeList const *types)
{
    if (types == NULL)
        return;
    pawRtti_mangle_start_generic_args(ENV(C), buf);

    struct IrType **pt;
    K_LIST_FOREACH (types, pt)
        mangle_type(C, buf, *pt);

    pawRtti_mangle_finish_generic_args(ENV(C), buf);
}

void pawP_mangle_start(paw_Env *P, Buffer *buf, struct Compiler *C)
{
    ENSURE_STACK(P, 1);
    pawL_init_buffer(P, buf);
    pawRtti_mangle_start(P, buf);
}

Str *pawP_mangle_finish(paw_Env *P, Buffer *buf, struct Compiler *C)
{
    pawL_push_result(P, buf);

    // anchor in compiler string table
    Str *str = V_STR(P->top.p[-1]);
    pawMap_insert(P, C->strings, &P2V(str), &P2V(str));
    pawC_pop(P);
    return str;
}

Str *pawP_mangle_name(struct Compiler *C, Str const *modname, Str const *name, struct IrTypeList *types)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawP_mangle_start(P, &buf, C);
    if (modname != NULL)
        pawRtti_mangle_add_module(P, &buf, modname);
    pawRtti_mangle_add_name(P, &buf, name);
    mangle_types(C, &buf, types);
    return pawP_mangle_finish(P, &buf, C);
}

Str *pawP_mangle_attr(struct Compiler *C, Str const *modname, Str const *base, struct IrTypeList const *base_types, Str const *attr, struct IrTypeList const *attr_types)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawP_mangle_start(P, &buf, C);
    if (modname != NULL)
        pawRtti_mangle_add_module(P, &buf, modname);
    pawRtti_mangle_add_name(P, &buf, base);
    mangle_types(C, &buf, base_types);
    pawRtti_mangle_add_name(P, &buf, attr);
    mangle_types(C, &buf, attr_types);
    return pawP_mangle_finish(P, &buf, C);
}

paw_Bool pawP_push_callback(struct Compiler *C, char const *name)
{
    paw_Env *P = ENV(C);
    paw_push_str(P, name);
    return paw_map_get(P, PAW_REGISTRY_INDEX) >= 0;
}

