// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "api.h"
#include "ast.h"
#include "debug.h"
#include "gc.h"
#include "hir.h"
#include "ir_type.h"
#include "lex.h"
#include "map.h"
#include "type.h"
#include "type_folder.h"
#include "unify.h"

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
    "let",
    "if",
    "else",
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
};

static String *basic_type_name(paw_Env *P, char const *name, paw_Type code)
{
    String *s = pawS_new_fixed(P, name);
    s->flag = FLAG2CODE(code); // works either direction
    return s;
}

void pawP_init(paw_Env *P)
{
    // Add all keywords to the interned strings table. Fix them so they are
    // never collected. Also added to the lexer string map.
    for (uint16_t i = 0; i < PAW_COUNTOF(kKeywords); ++i) {
        char const *kw = kKeywords[i];
        String *str = pawS_new_fixed(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }

    P->string_cache[CSTR_UNIT] = basic_type_name(P, "unit", BUILTIN_UNIT);
    P->string_cache[CSTR_BOOL] = basic_type_name(P, "bool", BUILTIN_BOOL);
    P->string_cache[CSTR_INT] = basic_type_name(P, "int", BUILTIN_INT);
    P->string_cache[CSTR_FLOAT] = basic_type_name(P, "float", BUILTIN_FLOAT);
    P->string_cache[CSTR_STR] = basic_type_name(P, "str", BUILTIN_STR);
    P->string_cache[CSTR_LIST] = pawS_new_fixed(P, "List");
    P->string_cache[CSTR_MAP] = pawS_new_fixed(P, "Map");
    P->string_cache[CSTR_OPTION] = pawS_new_fixed(P, "Option");
    P->string_cache[CSTR_RESULT] = pawS_new_fixed(P, "Result");
    P->string_cache[CSTR_RANGE] = pawS_new_fixed(P, "Range");
    P->string_cache[CSTR_HASH] = pawS_new_fixed(P, "Hash");
    P->string_cache[CSTR_EQUALS] = pawS_new_fixed(P, "Equals");
    P->string_cache[CSTR_COMPARE] = pawS_new_fixed(P, "Compare");
    P->string_cache[CSTR_TRUE] = pawS_new_fixed(P, "true");
    P->string_cache[CSTR_FALSE] = pawS_new_fixed(P, "false");
    P->string_cache[CSTR_UNDERSCORE] = pawS_new_fixed(P, "_");
    P->string_cache[CSTR_SELF] = pawS_new_fixed(P, "self");
    P->string_cache[CSTR_NEW] = pawS_new_fixed(P, "new");
    P->string_cache[CSTR_EXTERN] = pawS_new_fixed(P, "extern");

    P->string_cache[CSTR_KMODULES] = pawS_new_fixed(P, "paw.modules");
    P->string_cache[CSTR_KSYMBOLS] = pawS_new_fixed(P, "paw.symbols");
    P->string_cache[CSTR_KSEARCHERS] = pawS_new_fixed(P, "paw.searchers");
}

enum BuiltinKind pawP_type2code(struct Compiler *C, struct IrType *type)
{
    if (IrIsAdt(type)) {
        DeclId const base = IR_TYPE_DID(type);
        if (base.value == C->builtins[BUILTIN_UNIT].did.value) {
            return BUILTIN_UNIT;
        } else if (base.value == C->builtins[BUILTIN_BOOL].did.value) {
            return BUILTIN_BOOL;
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
        } else if (base.value == C->builtins[BUILTIN_HASH].did.value) {
            return BUILTIN_HASH;
        } else if (base.value == C->builtins[BUILTIN_EQUALS].did.value) {
            return BUILTIN_EQUALS;
        }
    }
    return NBUILTINS;
}

String *pawP_scan_nstring(struct Compiler *C, Tuple *map, char const *s, size_t n)
{
    paw_Env *P = ENV(C);
    Value const *pv = pawC_pushns(P, s, n);
    pawMap_insert(P, map, *pv, *pv);
    pawC_pop(P);
    CHECK_GC(P);
    return V_STRING(*pv);
}

static void define_prelude_adt(struct Compiler *C, unsigned cstr, enum BuiltinKind kind)
{
    String *s = CACHED_STRING(ENV(C), cstr);
    C->builtins[kind] = (struct Builtin){
        .did = NO_DECL,
        .name = s,
    };
    BuiltinMap_insert(C, C->builtin_lookup, s, &C->builtins[kind]);
}

void *pawP_alloc(struct Compiler *C, void *ptr, size_t size0, size_t size)
{
    return pawK_pool_alloc(ENV(C), C->pool, ptr, size0, size);
}

#define FIRST_ARENA_SIZE 4096
#define LARGE_ARENA_MIN (32 * sizeof(void *))

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, char const *modname)
{
    pawY_uninit(P);

    pawK_pool_init(P, &dm->pool, FIRST_ARENA_SIZE, LARGE_ARENA_MIN);

    *C = (struct Compiler){
        .pool = &dm->pool,
        .dm = dm,
        .P = P,
    };

    paw_new_map(P, 0, PAW_TSTR);
    C->strings = V_TUPLE(P->top.p[-1]);

    paw_new_map(P, 0, PAW_TSTR);
    P->constants = P->top.p[-1];
    paw_new_map(P, 0, PAW_TSTR);
    P->functions = P->top.p[-1];
    paw_pop(P, 2);

    C->globals = GlobalList_new(C);
    C->builtin_lookup = BuiltinMap_new(C);
    C->ir_types = HirTypes_new(C);
    C->ir_defs = DefMap_new(C);

    C->rtti = RttiMap_new(C);
    C->imports = ImportMap_new(C);
    C->traits = TraitMap_new(C);
    C->trait_owners = TraitOwners_new(C);

    C->modname = P->modname = SCAN_STRING(C, modname);
    C->prelude = pawAst_new(C, SCAN_STRING(C, "prelude"), 0);

    C->decls = pawHir_decl_list_new(C);
    C->modules = pawP_mod_list_new(C);

    C->U = pawP_alloc(C, NULL, 0, sizeof(struct Unifier));
    C->U->C = C;

    // builtin primitives
    define_prelude_adt(C, CSTR_UNIT, BUILTIN_UNIT);
    define_prelude_adt(C, CSTR_BOOL, BUILTIN_BOOL);
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

    // builtin traits
    define_prelude_adt(C, CSTR_HASH, BUILTIN_HASH);
    define_prelude_adt(C, CSTR_EQUALS, BUILTIN_EQUALS);
    define_prelude_adt(C, CSTR_COMPARE, BUILTIN_COMPARE);
}

void pawP_teardown(paw_Env *P, struct DynamicMem *dm)
{
    pawK_pool_uninit(P, &dm->pool);
    pawM_free_vec(P, dm->source.data, dm->source.size);
    pawM_free_vec(P, dm->scratch.data, dm->scratch.alloc);
}

struct ModuleInfo *pawP_mi_new(struct Compiler *C, struct Hir *hir)
{
    struct ModuleInfo *mi = pawP_alloc(C, NULL, 0, sizeof(*mi));
    *mi = (struct ModuleInfo){
        .globals = pawHir_scope_new(C),
        .hir = hir,
    };
    return mi;
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

static struct Type *lookup_type(struct DefGenerator *dg, struct IrType *type)
{
    if (type == NULL)
        return NULL;
    struct Type *const *ptype = RttiMap_get(dg->C, dg->C->rtti, type);
    return ptype != NULL ? *ptype : NULL;
}

static void map_types(struct DefGenerator *dg, struct IrType *type, struct Type *rtti)
{
    paw_Env *P = ENV(dg->C);
    RttiMap_insert(dg->C, dg->C->rtti, type, rtti);
}

static struct Type *new_type(struct DefGenerator *, struct IrType *, ItemId);
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

static struct Type *new_type(struct DefGenerator *dg, struct IrType *src, ItemId iid)
{
    struct Type *dst = lookup_type(dg, src);
    if (dst != NULL)
        return dst;

    paw_Env *P = ENV(dg->C);
    switch (IR_KINDOF(src)) {
        case kIrAdt: {
            struct IrAdt *adt = IrGetAdt(src);
            dst = pawY_new_adt(P, iid, LEN(adt->types));
            init_type_list(dg, adt->types, dst->subtypes, iid);
            break;
        }
        case kIrSignature: {
            struct IrSignature *fsig = IrGetSignature(src);
            dst = pawY_new_signature(P, iid, fsig->params->count);
            init_type_list(dg, fsig->params, dst->subtypes, iid);
            dst->sig.result = MAKE_TYPE(dg, fsig->result, iid);
            break;
        }
        case kIrFuncPtr: {
            struct IrFuncPtr *fptr = IrGetFuncPtr(src);
            dst = pawY_new_func_ptr(P, fptr->params->count);
            init_type_list(dg, fptr->params, dst->subtypes, -1);
            dst->sig.result = MAKE_TYPE(dg, fptr->result, -1);
            break;
        }
        case kIrTraitObj: {
            struct IrTraitObj *trait = IrGetTraitObj(src);
            dst = pawY_new_trait_obj(P);
            break;
        }
        default: { // kIrTuple
            struct IrTuple *tuple = IrGetTuple(src);
            dst = pawY_new_tuple(P, tuple->elems->count);
            init_type_list(dg, tuple->elems, dst->subtypes, -1);
        }
    }
    return dst;
}

static String *get_modname(struct DefGenerator *dg, DeclId did)
{
    struct ModuleList *modules = dg->C->modules;
    return K_LIST_GET(modules, did.modno)->hir->name;
}

static struct Def *new_def(struct DefGenerator *dg, DeclId did, struct IrType *type)
{
    struct HirDecl *decl = pawHir_get_decl(dg->C, did);

    struct Def *def;
    paw_Env *P = ENV(dg->C);

    switch (HIR_KINDOF(decl)) {
        case kHirVarDecl:
        case kHirFieldDecl:
            def = pawY_new_var_def(P);
            break;
        case kHirFuncDecl:
            def = pawY_new_func_def(P, LEN(HirGetFuncDecl(decl)->params));
            break;
        case kHirAdtDecl:
            def = pawY_new_adt_def(P, LEN(HirGetAdtDecl(decl)->fields));
            def->adt.is_struct = HirGetAdtDecl(decl)->is_struct;
            def->adt.is_pub = HirGetAdtDecl(decl)->is_pub;
            break;
        case kHirTraitDecl:
            return NULL;
        case kHirVariantDecl:
            def = pawY_new_func_def(P, LEN(HirGetVariantDecl(decl)->fields));
            break;
        default:
            PAW_UNREACHABLE();
    }

    struct Type *ty = new_type(dg, type, def->hdr.iid);
    def->hdr.name = decl->hdr.name;
    def->hdr.code = ty->hdr.code;
    def->hdr.modname = get_modname(dg, did);
    return def;
}

static void allocate_other_type(struct DefGenerator *dg, struct IrType *type)
{
    struct Type *rtti = new_type(dg, type, -1);
    map_types(dg, type, rtti);
}

static void allocate_adt_def(struct DefGenerator *dg, struct IrType *type)
{
    if (!IrIsAdt(type)) {
        allocate_other_type(dg, type);
        return;
    }
    struct IrAdt *t = IrGetAdt(type);
    struct HirDecl *decl = pawHir_get_decl(dg->C, t->did);
    struct HirAdtDecl *d = HirGetAdtDecl(decl);

    struct Def *def = new_def(dg, t->did, type);
    def->adt.is_struct = d->is_struct;
    def->adt.is_pub = d->is_pub;

    struct Type *rtti = Y_TYPE(ENV(dg->C), def->func.code);
    map_types(dg, type, rtti);
}

static void define_decl_list(struct DefGenerator *dg, struct HirDeclList *decls)
{
    // TODO: define fields for RTTI
}

static void connect_adt_def(struct DefGenerator *dg, struct IrType *mono)
{
    // paw_Env *P = ENV(dg->C);
    // struct HirDecl *decl = pawHir_get_decl(dg->C, IR_TYPE_DID(mono));
    // struct HirAdtDecl *d = HirGetAdtDecl(decl);
    // struct Type *ty = lookup_type(dg, mono);
    // struct Def *def = Y_DEF(P, ty->adt.iid);

    // struct DefState ds;
    // enter_def(dg, &ds, mono, def);
    // define_decl_list(dg, d->fields);
    // leave_def(dg);
}

static void allocate_types(struct DefGenerator *dg, struct IrTypeList *types)
{
    for (int i = 0; i < types->count; ++i) {
        allocate_adt_def(dg, K_LIST_GET(types, i));
    }
    for (int i = 0; i < types->count; ++i) {
        connect_adt_def(dg, K_LIST_GET(types, i));
    }
}

static struct ItemSlot allocate_item(struct DefGenerator *dg, struct Mir *body)
{
    paw_Env *P = ENV(dg->C);
    struct IrSignature *t = IrGetSignature(body->type);
    int const ntypes = t->types != NULL ? t->types->count : 0;
    struct HirFuncDecl *d = HirGetFuncDecl(pawHir_get_decl(dg->C, t->did));
    struct Def *def = new_def(dg, t->did, body->type);
    struct Type *self = lookup_type(dg, body->self);
    def->func.self = self != NULL ? self->adt.code : -1;
    // ".vid" is the index of the Value slot where this function will live
    // at runtime. Functions are placed after the global constants section.
    def->func.vid = dg->items->count + dg->C->globals->count;
    def->func.is_pub = d->is_pub;
    def->func.name = d->name;

    struct DefState ds;
    enter_def(dg, &ds, body->type, def);
    define_decl_list(dg, d->params);
    leave_def(dg);

    struct Type *rtti = Y_TYPE(P, def->func.code);
    rtti->sig.iid = def->func.iid;
    return (struct ItemSlot){
        .name = body->name,
        .mir = body,
        .rtti = rtti,
        .did = d->did,
    };
}

static void allocate_items(struct DefGenerator *dg, struct MirBodyList *bodies)
{
    struct Mir *const *pbody;
    K_LIST_FOREACH(bodies, pbody) {
        struct Mir *body = *pbody;
        if (body->self == NULL || !IrIsTraitObj(body->self)) {
            struct ItemSlot item = allocate_item(dg, body);
            K_LIST_PUSH(dg->C, dg->items, item);
            map_types(dg, body->type, item.rtti);
        }
    }

    struct GlobalInfo const *pinfo;
    K_LIST_FOREACH(dg->C->globals, pinfo) {

    }
}

struct ItemList *pawP_allocate_defs(struct Compiler *C, struct MirBodyList *bodies, struct IrTypeList *types)
{
    struct DefGenerator dg = {
        .items = pawP_item_list_new(C),
        .C = C,
    };
    allocate_types(&dg, types);
    allocate_items(&dg, bodies);
    return dg.items;
}

#define CHUNKSZ(b) sizeof(K_LIST_FIRST(b))

struct BitSet *pawP_bitset_new(struct Compiler *C, int count)
{
    paw_assert(count > 0);
    struct BitSet *set = raw_bitset_new(C);
    int const n = (count + CHUNKSZ(set) - 1) / CHUNKSZ(set);
    K_LIST_RESERVE(C, set, n);
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
    BitChunk *bc = &K_LIST_GET(set, pos);
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
    BitChunk bc = K_LIST_GET(set, pos);
    return (bc >> bit) & 1;
}

void pawP_bitset_clear(struct BitSet *set, int i)
{
    BITSET_INDEX(set, i, pos, bit);
    BitChunk *bc = &K_LIST_GET(set, pos);
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
        BitChunk *bc = &K_LIST_GET(a, i);
        *bc = *bc & K_LIST_GET(b, i);
    }
}

void pawP_bitset_or(struct BitSet *a, struct BitSet const *b)
{
    paw_assert(a->count == b->count);
    int const n = a->count / CHUNKSZ(a);

    for (int i = 0; i < n; ++i) {
        BitChunk *bc = &K_LIST_GET(a, i);
        *bc = *bc | K_LIST_GET(b, i);
    }
}

paw_Bool pawP_check_extern(struct Compiler *C, struct Annotations *annos, struct Annotation *panno)
{
    if (annos == NULL)
        return PAW_FALSE;
    struct Annotation *pa;
    K_LIST_FOREACH(annos, pa) {
        if (pawS_eq(pa->name, CSTR(C, CSTR_EXTERN))) {
            if (pa->has_value)
                VALUE_ERROR(C, -1/*TODO*/, "value not supported for 'extern' annotation");
            *panno = *pa;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

Value pawP_get_extern_value(struct Compiler *C, String *name, struct Annotation anno)
{
    paw_Env *P = ENV(C);
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);
    Tuple *symbols = V_TUPLE(P->top.p[-1]);

    Value const *pval = pawMap_get(P, symbols, P2V(name));
    if (pval == NULL)
        NAME_ERROR(C, "missing value for symbol '%s'", name->text);
    paw_pop(P, 1); // pop 'symbols'
    return *pval;
}

static struct Type *lookup_rtti(struct Compiler *C, struct IrType *type)
{
    struct Type **prtti = RttiMap_get(C, C->rtti, type);
    return prtti != NULL ? *prtti : NULL;
}

static void mangle_type(struct Compiler *C, Buffer *buf, struct IrType *type)
{
    struct Type *t = lookup_rtti(C, type);
    paw_assert(t != NULL);

    pawY_mangle_add_arg(ENV(C), buf, t->hdr.code);
}

static void mangle_types(struct Compiler *C, Buffer *buf, struct IrTypeList const *types)
{
    if (types == NULL)
        return;
    pawY_mangle_start_generic_args(ENV(C), buf);

    struct IrType **pt;
    K_LIST_FOREACH(types, pt)
    mangle_type(C, buf, *pt);

    pawY_mangle_finish_generic_args(ENV(C), buf);
}

void pawP_mangle_start(paw_Env *P, Buffer *buf, struct Compiler *C)
{
    ENSURE_STACK(P, 1);
    pawL_init_buffer(P, buf);
    pawY_mangle_start(P, buf);
}

String *pawP_mangle_finish(paw_Env *P, Buffer *buf, struct Compiler *C)
{
    pawL_push_result(P, buf);

    // anchor in compiler string table
    String *str = V_STRING(P->top.p[-1]);
    pawMap_insert(P, C->strings, P2V(str), P2V(str));
    pawC_pop(P);
    return str;
}

String *pawP_mangle_name(struct Compiler *C, String const *modname, String const *name, struct IrTypeList *types)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawP_mangle_start(P, &buf, C);
    if (modname != NULL)
        pawY_mangle_add_module(P, &buf, modname);
    pawY_mangle_add_name(P, &buf, name);
    mangle_types(C, &buf, types);
    return pawP_mangle_finish(P, &buf, C);
}

String *pawP_mangle_attr(struct Compiler *C, String const *modname, String const *base, struct IrTypeList const *base_types, String const *attr, struct IrTypeList const *attr_types)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawP_mangle_start(P, &buf, C);
    if (modname != NULL)
        pawY_mangle_add_module(P, &buf, modname);
    pawY_mangle_add_name(P, &buf, base);
    mangle_types(C, &buf, base_types);
    pawY_mangle_add_name(P, &buf, attr);
    mangle_types(C, &buf, attr_types);
    return pawP_mangle_finish(P, &buf, C);
}

