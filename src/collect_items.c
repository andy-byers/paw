// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// collect_items.c: Definition of pawP_collect_items. Collect the type of each
//     language construct not declared within a function body. Essentially,
//     determines the declaration referenced by each path, e.g. a struct field
//     or a named function parameter. Note that all paths in an ADT definition
//     or function signature refer either to ADTs or to generics from an
//     enclosing binder, meaning only ADTs are instantiated in this module.

#include "compile.h"
#include "debug.h"
#include "hir.h"
#include "gc.h"
#include "ir_type.h"
#include "map.h"

#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))
#define GET_TYPE(X, hid) pawIr_get_type((X)->C, hid)
#define SET_TYPE(X, hid, t) pawIr_set_type((X)->C, hid, t)

struct ItemCollector {
    struct Pool *pool;
    struct HirSymtab *symtab;
    struct DynamicMem *dm;
    struct Compiler *C;
    struct ModuleInfo *m;
    struct IrTypeList *impl_binder;
    struct IrType *adt;
    Map *impls;
    paw_Env *P;
    int ndefs;
    int line;
};

struct PartialAdt {
    struct HirAdtDecl *d;
    struct HirScope *scope;
};

struct PartialMod {
    struct PartialAdtList *pal;
    struct ModuleInfo *m;
};

DEFINE_LIST_V2(struct ItemCollector, pa_list_, PartialAdtList, struct PartialAdt *)
DEFINE_LIST_V2(struct ItemCollector, pm_list_, PartialModList, struct PartialMod *)

static struct PartialAdt *new_partial_adt(struct ItemCollector *X, struct HirAdtDecl *d, struct HirScope *scope)
{
    struct PartialAdt *pa = pawK_pool_alloc(ENV(X), X->pool, sizeof(struct PartialAdt));
    *pa = (struct PartialAdt){.d = d, .scope = scope};
    return pa;
}

static struct PartialMod *new_partial_mod(struct ItemCollector *X, struct ModuleInfo *m, struct PartialAdtList *pal)
{
    struct PartialMod *pm = pawK_pool_alloc(ENV(X), X->pool, sizeof(struct PartialMod));
    *pm = (struct PartialMod){.pal = pal, .m = m};
    return pm;
}

static DeclId add_decl(struct ItemCollector *X, struct HirDecl *decl)
{
    return pawHir_add_decl(X->C, decl, X->m->hir->modno);
}

static struct HirScope *enclosing_scope(struct HirSymtab *st)
{
    paw_assert(st->count > 0);
    return K_LIST_GET(st, st->count - 1);
}

static struct HirSymbol *add_symbol(struct ItemCollector *X, struct HirScope *scope, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = pawHir_add_symbol(X->C, scope);
    symbol->name = name;
    symbol->decl = decl;
    return symbol;
}

static struct HirSymbol *declare_local(struct ItemCollector *X, String *name, struct HirDecl *decl)
{
    return add_symbol(X, enclosing_scope(X->symtab), name, decl);
}

// Allow a previously-declared variable to be accessed
static void define_local(struct HirSymbol *symbol)
{
    symbol->is_init = PAW_TRUE;
}

static struct HirSymbol *new_global(struct ItemCollector *X, struct HirDecl *decl, paw_Bool is_type)
{
    String *name = decl->hdr.name;
    struct HirScope *scope = X->m->globals;
    for (int i = 0; i < scope->count; ++i) {
        struct HirSymbol *symbol = scope->data[i];
        if (pawS_eq(symbol->name, name)) {
            NAME_ERROR(X, "duplicate global '%s' (declared previously on line %d)",
                       name->text, symbol->decl->hdr.line);
        }
    }
    struct HirSymbol *symbol = add_symbol(X, scope, name, decl);
    symbol->is_init = PAW_TRUE;
    symbol->is_type = is_type;
    return symbol;
}

static struct HirSymbol *try_resolve_symbol(struct ItemCollector *X, const String *name)
{
    // search the scoped symbols
    struct HirSymtab *scopes = X->symtab;
    const int nscopes = scopes->count;
    for (int depth = nscopes - 1; depth >= 0; --depth) {
        struct HirScope *scope = scopes->data[depth];
        const int index = pawHir_find_symbol(scope, name);
        if (index >= 0) {
            struct HirSymbol *symbol = scope->data[index];
            return scope->data[index];
        }
    }

    // search the global symbols
    const int index = pawHir_find_symbol(X->m->globals, name);
    if (index < 0) return NULL;
    return X->m->globals->data[index];
}

static struct HirSymbol *resolve_symbol(struct ItemCollector *X, const String *name)
{
    struct HirSymbol *symbol = try_resolve_symbol(X, name);
    if (symbol == NULL) NAME_ERROR(X, "undefined symbol '%s'", name->text);
    return symbol;
}

static struct HirSymbol *new_local(struct ItemCollector *X, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = declare_local(X, name, decl);
    define_local(symbol);
    return symbol;
}

static struct HirScope *leave_block(struct ItemCollector *X)
{
    struct HirSymtab *st = X->symtab;
    struct HirScope *scope = enclosing_scope(st);
    --st->count;
    return scope;
}

static void enter_block(struct ItemCollector *X, struct HirScope *scope)
{
    scope = scope != NULL ? scope : pawHir_scope_new(X->C);
    K_LIST_PUSH(X->C, X->symtab, scope);
}

static struct HirScope *leave_function(struct ItemCollector *X)
{
    struct HirScope *scope = leave_block(X);
    CHECK_GC(ENV(X));
    return scope;
}

// TODO: what about first parameter 'self', can we just rely on that? This may be from before that change was made.
static void create_context(struct ItemCollector *X, struct IrType *type, int line)
{
    String *self = CSTR(X, CSTR_SELF); // 'self'
    struct HirDecl *decl = pawHir_get_decl(X->C, IR_TYPE_DID(type));
    struct HirDecl *result = pawHir_new_decl(X->C, line, kHirVarDecl);
    HirGetVarDecl(result)->name = self;
    SET_NODE_TYPE(X->C, result, type);

    new_local(X, self, result);
    add_decl(X, result);
}

static struct HirAdtDecl *get_adt(struct ItemCollector *X, struct IrType *type)
{
    const DeclId did = IR_TYPE_DID(type);
    struct HirDecl *decl = pawHir_get_decl(X->C, did);
    return HirGetAdtDecl(decl);
}

static void enter_function(struct ItemCollector *X, struct HirFuncDecl *func)
{
    enter_block(X, NULL);
    new_local(X, func->name, HIR_CAST_DECL(func));
    if (func->fn_kind == FUNC_METHOD) {
        // methods use local slot 1 for 'self'
        create_context(X, func->self, func->line);
    }
}

static void dupcheck(struct ItemCollector *X, Map *map, String *name, const char *what)
{
    if (name == NULL) return;
    Value *pv = pawH_get(map, P2V(name));
    if (pv != NULL) NAME_ERROR(X, "duplicate %s '%s'", what, name->text);
    pawH_insert(ENV(X), map, P2V(name), P2V(name));
}

static Map *start_dupcheck(struct ItemCollector *X)
{
    paw_Env *P = ENV(X);
    ENSURE_STACK(P, 1);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    V_SET_OBJECT(pv, map);
    return map;
}

static void finish_dupcheck(struct ItemCollector *X)
{
    pawC_pop(ENV(X));
}

static struct IrType *collect_type(struct ItemCollector *X, struct HirType *type)
{
    X->line = type->hdr.line;
    struct IrType *result = pawP_lower_type(X->C, X->m, X->symtab, type);
    if (result == NULL) {
        const char *type_name = pawHir_print_type(X->C, type);
        TYPE_ERROR(X, "unrecognized type '%s'", type_name);
    }
    return result;
}

static void map_adt_to_impl(struct ItemCollector *X, struct HirDecl *adt, struct HirDecl *impl)
{
    Value *pv = pawH_get(X->impls, P2V(adt));
    if (pv == NULL) {
        pv = pawH_create(ENV(X), X->impls, P2V(adt));
        pv->p = pawHir_decl_list_new(X->C);
    }
    struct HirDeclList *impls = pv->p;
    K_LIST_PUSH(X->C, impls, impl);
}

static struct IrType *collect_path(struct ItemCollector *X, struct HirPath *path)
{
    struct IrType *type = pawP_lookup(X->C, X->m, X->symtab, path, LOOKUP_TYPE);
    if (type == NULL) NAME_ERROR(X, "bad path");
    return type;
}

static struct HirDecl *get_decl(struct ItemCollector *X, DeclId did)
{
    return pawHir_get_decl(X->C, did);
}

static struct IrType *new_type(struct ItemCollector *X, DeclId did, enum IrTypeKind kind, int line)
{
    return pawP_attach_type(X->C, did, kind, line);
}

static struct IrType *register_decl_type(struct ItemCollector *X, struct HirDecl *decl, enum IrTypeKind kind)
{
    return new_type(X, decl->hdr.did, kind, decl->hdr.line);
}

static void register_generics(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics == NULL) return;
    for (int i = 0; i < generics->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(generics, i);
        struct IrType *type = register_decl_type(X, decl, kIrGeneric);
        struct IrGeneric *t = IrGetGeneric(type);
        struct HirSymbol *symbol = new_local(X, decl->hdr.name, decl);
        symbol->is_generic = PAW_TRUE;
        symbol->is_type = PAW_TRUE;
    }
}

static struct IrTypeList *collect_generic_types(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics == NULL) return NULL;
    return pawHir_collect_decl_types(X->C, generics);
}

static struct IrTypeList *collect_field_types(struct ItemCollector *X, struct HirDeclList *fields)
{
    return pawHir_collect_decl_types(X->C, fields);
}

static struct IrType *register_func(struct ItemCollector *X, struct HirFuncDecl *d)
{
    struct IrType *type = register_decl_type(X, HIR_CAST_DECL(d), kIrSignature);
    struct IrSignature *t = IrGetSignature(type);
    t->types = collect_generic_types(X, d->generics);
    t->params = collect_field_types(X, d->params);
    t->result = collect_type(X, d->result);
    t->did = d->did;
    return type;
}

static DeclId next_did(struct ItemCollector *X)
{
    return (DeclId){
        .modno = X->m->hir->modno,
        .value = X->ndefs++,
    };
}

static struct IrType *register_adt(struct ItemCollector *X, struct HirAdtDecl *d)
{
    struct IrAdtDef *def = pawIr_new_adt_def(X->C, next_did(X), d->name, d->is_pub, d->is_struct);
    struct IrType *type = register_decl_type(X, HIR_CAST_DECL(d), kIrAdt);
    struct IrAdt *t = IrGetAdt(type);

    t->types = collect_generic_types(X, d->generics);
    t->did = d->did;
    return type;
}

static void collect_field_decl(struct ItemCollector *X, struct HirFieldDecl *d)
{
    SET_TYPE(X, d->hid, collect_type(X, d->tag));
}

static void collect_variant_decl(struct ItemCollector *, struct HirVariantDecl *);

static void collect_fields(struct ItemCollector *X, struct HirDeclList *fields)
{
    Map *map = start_dupcheck(X);
    if (fields == NULL) return;
    for (int i = 0; i < fields->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(fields, i);
        if (HirIsFieldDecl(decl)) {
            struct HirFieldDecl *d = HirGetFieldDecl(decl);
            dupcheck(X, map, d->name, "struct field");
            collect_field_decl(X, d);
        } else {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            dupcheck(X, map, d->name, "enum variant");
            collect_variant_decl(X, d);
        }
    }
    finish_dupcheck(X);
}

static void collect_variant_decl(struct ItemCollector *X, struct HirVariantDecl *d)
{
    collect_fields(X, d->fields);

    // An enum variant name can be thought of as a function from the type of the
    // variant's fields to the type of the enumeration. For example, given 'enum
    // E {X(str)}', E::X has type 'fn(str) -> E'.
    struct IrType *type = register_decl_type(X, HIR_CAST_DECL(d), kIrSignature);
    struct IrSignature *t = IrGetSignature(type);
    t->params = d->fields != NULL
        ? collect_field_types(X, d->fields)
        : pawIr_type_list_new(X->C);
    t->result = X->adt;
    SET_TYPE(X, d->hid, type);
}

static paw_Bool check_static_method(struct ItemCollector *X, struct IrType *self, struct HirDeclList *params)
{
    if (self == NULL) return PAW_FALSE; // not a method
    if (params->count == 0) return PAW_TRUE;
    struct HirDecl *first = K_LIST_GET(params, 0);
    return GET_NODE_TYPE(X->C, first) != self;
}

static void collect_func(struct ItemCollector *X, struct HirFuncDecl *d)
{
    enter_function(X, d);
    register_generics(X, d->generics);
    collect_fields(X, d->params);
    register_func(X, d);
    leave_function(X);

    d->is_assoc = check_static_method(X, X->adt, d->params);
    if (d->self != NULL) {
        struct IrType *type = pawIr_get_type(X->C, d->hid);
        pawP_set_self(X->C, IrGetSignature(type), d->self);
        if (X->impl_binder != NULL) {
            pawP_set_binder(X->C, d->did, X->impl_binder);
        }
    }
}

static void collect_func_decl(struct ItemCollector *X, struct HirFuncDecl *d)
{
    new_global(X, HIR_CAST_DECL(d), PAW_FALSE);
    collect_func(X, d);
}

static void maybe_fix_builtin(struct ItemCollector *X, String *name, DeclId did)
{
    struct Builtin *b = X->C->builtins;
    for (enum BuiltinKind k = 0; k < NBUILTINS; ++k) {
        if (pawS_eq(b[k].name, name)) {
            b[k].did = did;
            break;
        }
    }
}

#define WITH_ADT_CONTEXT(X, type, code) do { \
        (X)->adt = (type); \
        code \
        (X)->adt = NULL; \
    } while (0)

static struct HirScope *register_adt_decl(struct ItemCollector *X, struct HirAdtDecl *d)
{
    enter_block(X, NULL);
    register_generics(X, d->generics);
    register_adt(X, d);
    maybe_fix_builtin(X, d->name, d->did);
    new_global(X, HIR_CAST_DECL(d), PAW_TRUE);
    return leave_block(X);
}

static void collect_adt_decl(struct ItemCollector *X, struct PartialAdt *lazy)
{
    struct HirAdtDecl *d = lazy->d;
    enter_block(X, lazy->scope);
    WITH_ADT_CONTEXT(X, GET_TYPE(X, d->hid),
            collect_fields(X, d->fields););
    leave_block(X);
}

static void collect_methods(struct ItemCollector *X, struct HirDeclList *methods)
{
    // TODO: Need to prevent duplicates between different impl blocks on the same type
    Map *map = start_dupcheck(X);
    for (int i = 0; i < methods->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(methods, i);
        struct HirFuncDecl *d = HirGetFuncDecl(decl);
        dupcheck(X, map, d->name, "method");
        d->self = X->adt;
        collect_func(X, d);
    }
    finish_dupcheck(X);
}

static struct IrType *collect_self(struct ItemCollector *X, struct HirImplDecl *d)
{
    String *selfname = SCAN_STRING(X->C, "Self");
    struct IrType *self = collect_path(X, d->self);
    struct HirDecl *result = pawHir_new_decl(X->C, d->line, kHirTypeDecl);
    struct HirTypeDecl *r = HirGetTypeDecl(result);
    add_decl(X, result);
    SET_TYPE(X, r->hid, self);

    map_adt_to_impl(X, get_decl(X, IR_TYPE_DID(self)), HIR_CAST_DECL(d));
    struct HirSymbol *symbol = new_local(X, selfname, result);
    symbol->is_type = PAW_TRUE;
    return self;
}

static void collect_impl_decl(struct ItemCollector *X, struct HirImplDecl *d)
{
    enter_block(X, NULL);
    register_generics(X, d->generics);
    X->impl_binder = d->subst = collect_generic_types(X, d->generics);
    struct IrType *type = collect_self(X, d);
    SET_TYPE(X, d->hid, type);
    WITH_ADT_CONTEXT(X, type,
            collect_methods(X, d->methods););
    leave_block(X);
}

static void collect_use_decl(struct ItemCollector *X, struct HirUseDecl *d)
{
    new_global(X, HIR_CAST_DECL(d), PAW_FALSE);
}

static struct ModuleInfo *use_module(struct ItemCollector *X, struct ModuleInfo *m)
{
    pawU_enter_binder(&X->dm->unifier);
    X->m = m;
    return m;
}

static void finish_module(struct ItemCollector *X)
{
    pawU_leave_binder(&X->dm->unifier);
    X->m = NULL;
}

static struct PartialAdtList *register_adts(struct ItemCollector *X, struct HirDeclList *items)
{
    struct PartialAdtList *list = pa_list_new(X);
    for (int i = 0; i < items->count; ++i) {
        struct HirDecl *item = K_LIST_GET(items, i);
        if (!HirIsAdtDecl(item)) continue;
        struct HirAdtDecl *d = HirGetAdtDecl(item);
        struct HirScope *scope = register_adt_decl(X, d);
        struct PartialAdt *pa = new_partial_adt(X, d, scope);
        K_LIST_PUSH(X, list, pa);
    }
    return list;
}

static void collect_adts(struct ItemCollector *X, struct PartialAdtList *list)
{
    for (int i = 0; i < list->count; ++i) {
        struct PartialAdt *pa = K_LIST_GET(list, i);
        collect_adt_decl(X, pa);
    }
}

static struct PartialModList *collect_phase_1(struct ItemCollector *X, struct ModuleList *ml)
{
    struct PartialModList *pml = pm_list_new(X);

    // collect toplevel ADT names and type parameters
    for (int i = 0; i < ml->count; ++i) {
        struct ModuleInfo *m = use_module(X, K_LIST_GET(ml, i));
        paw_assert(m->globals->count == 0);
        struct PartialAdtList *pal = register_adts(X, m->hir->items);
        struct PartialMod *pm = new_partial_mod(X, m, pal);
        K_LIST_PUSH(X, pml, pm);
        m->globals = X->m->globals;
        finish_module(X);
    }

    // fill in toplevel ADT field types (may instantiate polymorphic ADTs)
    for (int i = 0; i < pml->count; ++i) {
        struct PartialMod *pm = K_LIST_GET(pml, i);
        use_module(X, pm->m);
        collect_adts(X, pm->pal);
        finish_module(X);
    }

    return pml;
}
static void collect_items(struct ItemCollector *X, struct Hir *hir)
{
    X->symtab = pawHir_symtab_new(X->C);
    for (int i = 0; i < hir->items->count; ++i) {
        struct HirDecl *item = K_LIST_GET(hir->items, i);
        if (HirIsFuncDecl(item)) {
            collect_func_decl(X, HirGetFuncDecl(item));
        } else if (HirIsImplDecl(item)) {
            collect_impl_decl(X, HirGetImplDecl(item));
        } else if (HirIsUseDecl(item)) {
            collect_use_decl(X, HirGetUseDecl(item));
        }
    }
}

static void collect_phase_2(struct ItemCollector *X, struct ModuleList *ml, struct PartialModList *pml)
{
    // collect toplevel function and method types
    for (int i = 0; i < ml->count; ++i) {
        struct ModuleInfo *m = use_module(X, K_LIST_GET(ml, i));
        collect_items(X, m->hir);
        finish_module(X);
    }
}

// Entrypoint to item collection
void pawP_collect_items(struct Compiler *C)
{
    struct DynamicMem *dm = C->dm;
    struct ItemCollector X = {
        .symtab = pawHir_symtab_new(C),
        .pool = &dm->pool,
        .impls = C->impls,
        .P = ENV(C),
        .dm = dm,
        .C = C,
    };

    DLOG(&X, "collecting %d module(s)", dm->modules->count);

    struct PartialModList *pml = collect_phase_1(&X, dm->modules);
    collect_phase_2(&X, dm->modules, pml);
}
