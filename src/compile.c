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
#include "ir_type.h"
#include "map.h"
#include "type.h"
#include "type_folder.h"

// All paw language keywords
//
// ORDER TokenKind
static const char *kKeywords[] = {
    "pub",
    "use",
    "fn",
    "type",
    "enum",
    "struct",
    "impl",
    "let",
    "if",
    "else",
    "for",
    "do",
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

static String *basic_type_name(paw_Env *P, const char *name, paw_Type code)
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
        const char *kw = kKeywords[i];
        String *str = pawS_new_fixed(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }
    // note that keywords are already fixed
    P->string_cache[CSTR_TRUE] = pawS_new_str(P, "true");
    P->string_cache[CSTR_FALSE] = pawS_new_str(P, "false");
    P->string_cache[CSTR_BOOL] = basic_type_name(P, "bool", PAW_TBOOL);
    P->string_cache[CSTR_INT] = basic_type_name(P, "int", PAW_TINT);
    P->string_cache[CSTR_FLOAT] = basic_type_name(P, "float", PAW_TFLOAT);
    P->string_cache[CSTR_STR] = basic_type_name(P, "str", PAW_TSTR);
    P->string_cache[CSTR_LIST] = pawS_new_fixed(P, "_List");
    P->string_cache[CSTR_MAP] = pawS_new_fixed(P, "_Map");
    P->string_cache[CSTR_OPTION] = pawS_new_fixed(P, "Option");
    P->string_cache[CSTR_RESULT] = pawS_new_fixed(P, "Result");
    P->string_cache[CSTR_SELF] = pawS_new_fixed(P, "self");

    P->string_cache[CSTR_KBUILTIN] = pawS_new_fixed(P, "paw.builtin");
    P->string_cache[CSTR_KMODULES] = pawS_new_fixed(P, "paw.modules");
    P->string_cache[CSTR_KSEARCHERS] = pawS_new_fixed(P, "paw.searchers");
}

paw_Type pawP_type2code(struct Compiler *C, struct IrType *type)
{
    if (IrIsAdt(type)) {
        const DeclId base = IR_TYPE_DID(type);
        if (base.value == C->builtins[BUILTIN_UNIT].did.value) {
            return PAW_TUNIT;
        } else if (base.value == C->builtins[BUILTIN_BOOL].did.value) {
            return PAW_TBOOL;
        } else if (base.value == C->builtins[BUILTIN_INT].did.value) {
            return PAW_TINT;
        } else if (base.value == C->builtins[BUILTIN_FLOAT].did.value) {
            return PAW_TFLOAT;
        } else if (base.value == C->builtins[BUILTIN_STR].did.value) {
            return PAW_TSTR;
        } else if (base.value == C->builtins[BUILTIN_LIST].did.value) {
            return BUILTIN_LIST;
        } else if (base.value == C->builtins[BUILTIN_MAP].did.value) {
            return BUILTIN_MAP;
        }
    }
    return -1;
}

String *pawP_scan_nstring(paw_Env *P, Map *st, const char *s, size_t n)
{
    const Value *pv = pawC_pushns(P, s, n);
    Value *value = pawH_create(P, st, *pv);
    *value = *pv; // anchor in map
    pawC_pop(P);
    CHECK_GC(P);

    return V_STRING(*value);
}

static void define_prelude_adt(struct Compiler *C, const char *name, enum BuiltinKind kind)
{
    struct Ast *ast = C->prelude;
    struct AstDecl *decl = pawAst_new_decl(ast, 0, kAstAdtDecl);
    struct AstAdtDecl *r = AstGetAdtDecl(decl);
    r->name = SCAN_STRING(C, name);
    r->is_pub = PAW_TRUE;
    r->is_struct = PAW_TRUE;
    K_LIST_PUSH(C, ast->items, decl);
    C->builtins[kind] = (struct Builtin){
        .name = r->name,
        .did = NO_DECL,
    };
}

static void define_prelude_poly_adt(struct Compiler *C, const char *name, enum BuiltinKind kind)
{
    C->builtins[kind] = (struct Builtin){
        .name = SCAN_STRING(C, name),
        .did = NO_DECL,
    };
}

#define FIRST_ARENA_SIZE 4096
#define LARGE_ARENA_MIN (32 * sizeof(void *))

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, const char *modname)
{
    pawY_uninit(P);

    pawK_pool_init(P, &dm->pool, FIRST_ARENA_SIZE, LARGE_ARENA_MIN);

    *C = (struct Compiler){
        .pool = &dm->pool,
        .U = &dm->unifier,
        .dm = dm,
        .P = P,
    };

    C->strings = pawP_push_map(C);
    C->method_contexts = pawP_push_map(C);
    C->method_binders = pawP_push_map(C);
    C->ir_types = pawP_push_map(C);
    C->type2rtti = pawP_push_map(C);
    C->imports = pawP_push_map(C);
    C->impls = pawP_push_map(C);

    C->modname = P->modname = SCAN_STRING(C, modname);
    C->prelude = pawAst_new(C, SCAN_STRING(C, "prelude"), 0);

    C->dm->decls = pawHir_decl_list_new(C);
    C->dm->modules = pawP_mod_list_new(C);
    C->dm->unifier.C = C;

    // builtin primitives
    define_prelude_adt(C, "(unit)", BUILTIN_UNIT);
    define_prelude_adt(C, "bool", BUILTIN_BOOL);
    define_prelude_adt(C, "int", BUILTIN_INT);
    define_prelude_adt(C, "float", BUILTIN_FLOAT);
    define_prelude_adt(C, "str", BUILTIN_STR);

    // builtin containers (in Paw code, _List<T> can be written as [T], and
    // _Map<K, V> as [K: V])
    define_prelude_poly_adt(C, "_List", BUILTIN_LIST);
    define_prelude_poly_adt(C, "_Map", BUILTIN_MAP);

    // builtin enumerations
    define_prelude_poly_adt(C, "Option", BUILTIN_OPTION);
    define_prelude_poly_adt(C, "Result", BUILTIN_RESULT);
}

void pawP_teardown(paw_Env *P, struct DynamicMem *dm)
{
    pawK_pool_uninit(P, &dm->pool);
    pawM_free_vec(P, dm->scratch.data, dm->scratch.alloc);
}

struct ModuleInfo *pawP_mi_new(struct Compiler *C, struct Hir *hir)
{
    struct ModuleInfo *mi = pawK_pool_alloc(ENV(C), C->pool, sizeof(*mi));
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
    Map *adts;
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
    Value *pv = pawH_get(dg->C->type2rtti, P2V(type));
    return pv != NULL ? pv->p : NULL;
}

static void map_types(struct DefGenerator *dg, struct IrType *type, struct Type *rtti)
{
    paw_Env *P = ENV(dg->C);
    pawH_insert(P, dg->C->type2rtti, P2V(type), P2V(rtti));
}

static struct Type *new_type(struct DefGenerator *, struct IrType *, DefId);
#define MAKE_TYPE(dg, t, did) new_type(dg, IR_CAST_TYPE(t), did)->hdr.code

static void init_type_list(struct DefGenerator *dg, struct IrTypeList *x, paw_Type *y, DefId did)
{
    if (x == NULL) return;
    for (int i = 0; i < x->count; ++i) {
        y[i] = MAKE_TYPE(dg, x->data[i], did);
    }
}

#define LEN(L) ((L) != NULL ? (L)->count : 0)

static struct Type *new_type(struct DefGenerator *dg, struct IrType *src, DefId did)
{
    struct Type *dst = lookup_type(dg, src);
    if (dst != NULL) return dst;

    paw_Env *P = ENV(dg->C);
    switch (IR_KINDOF(src)) {
        case kIrAdt: {
            struct IrAdt *adt = IrGetAdt(src);
            dst = pawY_new_adt(P, did, LEN(adt->types));
            init_type_list(dg, adt->types, dst->subtypes, did);
            break;
        }
        case kIrSignature: {
            struct IrSignature *fsig = IrGetSignature(src);
            dst = pawY_new_signature(P, did, fsig->params->count);
            init_type_list(dg, fsig->params, dst->subtypes, did);
            dst->sig.result = MAKE_TYPE(dg, fsig->result, did);
            break;
        }
        case kIrFuncPtr: {
            struct IrFuncPtr *fptr = IrGetFuncPtr(src);
            dst = pawY_new_func_ptr(P, fptr->params->count);
            init_type_list(dg, fptr->params, dst->subtypes, -1);
            dst->sig.result = MAKE_TYPE(dg, fptr->result, -1);
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
    struct ModuleList *modules = dg->C->dm->modules;
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
        default: // kHirVariantDecl
            def = pawY_new_func_def(P, LEN(HirGetVariantDecl(decl)->fields));
    }

    struct Type *ty = new_type(dg, type, def->hdr.did);
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

static struct IrTypeList *get_mono_list(struct DefGenerator *dg, Map *lists, DeclId did)
{
    paw_Env *P = ENV(dg->C);
    struct IrTypeList *monos;
    const Value *pval = pawH_get(lists, I2V(did.value));
    if (pval == NULL) {
        struct IrTypeList *monos = pawIr_type_list_new(dg->C);
        pawH_insert(P, lists, I2V(did.value), P2V(monos));
        return monos;
    }
    return pval->p;
}

static void define_decl_list(struct DefGenerator *dg, struct HirDeclList *decls)
{
    // TODO: define fields for RTTI
}

static void connect_adt_def(struct DefGenerator *dg, struct IrType *mono)
{
    //paw_Env *P = ENV(dg->C);
    //struct HirDecl *decl = pawHir_get_decl(dg->C, IR_TYPE_DID(mono));
    //struct HirAdtDecl *d = HirGetAdtDecl(decl);
    //struct Type *ty = lookup_type(dg, mono);
    //struct Def *def = Y_DEF(P, ty->adt.did);

    //struct DefState ds;
    //enter_def(dg, &ds, mono, def);
    //define_decl_list(dg, d->fields);
    //leave_def(dg);
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
    const int ntypes = t->types != NULL ? t->types->count : 0;
    struct HirFuncDecl *d = HirGetFuncDecl(pawHir_get_decl(dg->C, t->did));
    struct Def *def = new_def(dg, t->did, body->type);
    def->func.name = d->name;
    def->func.is_pub = d->is_pub;
    def->func.vid = dg->items->count;

    struct DefState ds;
    enter_def(dg, &ds, body->type, def);
    define_decl_list(dg, d->params);
    leave_def(dg);

    struct Type *ty = Y_TYPE(P, def->func.code);
    ty->sig.did = def->func.did;
    return (struct ItemSlot){
        .mir = body,
        .rtti = ty,
    };
}

static void allocate_items(struct DefGenerator *dg, struct MirBodyList *bodies)
{
    for (int i = 0; i < bodies->count; ++i) {
        struct Mir *body = K_LIST_GET(bodies, i);

        struct ItemSlot item = allocate_item(dg, body);
        K_LIST_PUSH(dg->C, dg->items, item);
        map_types(dg, body->type, item.rtti);
    }
}

struct ItemList *pawP_allocate_defs(struct Compiler *C, struct MirBodyList *bodies, struct IrTypeList *types)
{
    struct DefGenerator dg = {
        .items = pawP_item_list_new(C),
        .C = C,
    };
    // DeclId => [struct IrType]
    dg.adts = pawP_push_map(C),

    allocate_types(&dg, types);
    allocate_items(&dg, bodies);

    pawP_pop_object(C, dg.adts);
    return dg.items;
}

Map *pawP_push_map(struct Compiler *C)
{
    paw_Env *P = ENV(C);
    ENSURE_STACK(P, 1);
    Map *result = pawH_new(P);
    V_SET_OBJECT(P->top.p++, result);
    return result;
}

static struct IrType *get_self(struct Compiler *C, struct IrSignature *method)
{
    const Value *pval = pawH_get(C->method_contexts, P2V(method));
    return pval != NULL ? pval->p : NULL;
}

paw_Bool pawP_is_assoc_fn(struct Compiler *C, struct IrSignature *t)
{
    return get_self(C, t) != NULL;
}

struct IrTypeList *pawP_get_binder(struct Compiler *C, DeclId did)
{
    const Value *pval = pawH_get(C->method_binders, I2V(did.value));
    return pval != NULL ? pval->p : NULL;
}

void pawP_set_binder(struct Compiler *C, DeclId did, struct IrTypeList *binder)
{
    pawH_insert(ENV(C), C->method_binders, I2V(did.value), P2V(binder));
}

void pawP_set_self(struct Compiler *C, struct IrSignature *method, struct IrType *self)
{
    pawH_insert(ENV(C), C->method_contexts, P2V(method), P2V(self));
}

struct IrType *pawP_get_self(struct Compiler *C, struct IrSignature *method)
{
    paw_assert(pawP_is_assoc_fn(C, method));
    return get_self(C, method);
}

