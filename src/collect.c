// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// collect.c: Definition of pawP_collect_items. Collect the type of each
//     language construct not declared within a function body. Essentially,
//     determines the declaration referenced by each path, e.g. a struct field
//     or a named function parameter. Note that all paths in an ADT definition
//     or function signature refer either to ADTs or to generics from an
//     enclosing binder, meaning only ADTs are instantiated in this module.

#include "debug.h"
#include "hir.h"
#include "gc.h"
#include "map.h"

#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))

struct Collector {
    struct Pool *pool;
    struct HirTypeFolder F;
    struct HirSymtab *symtab;
    struct DynamicMem *dm;
    struct Compiler *C;
    struct ModuleInfo *m;
    struct HirType *adt;
    Map *impls;
    paw_Env *P;
    int nexpand;
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

DEFINE_LIST(struct Collector, pa_list_, PartialAdtList, struct PartialAdt)
DEFINE_LIST(struct Collector, pm_list_, PartialModList, struct PartialMod)

static struct PartialAdt *new_partial_adt(struct Collector *X, struct HirAdtDecl *d, struct HirScope *scope)
{
    struct PartialAdt *pa = pawK_pool_alloc(ENV(X), X->pool, sizeof(struct PartialAdt));
    *pa = (struct PartialAdt){.d = d, .scope = scope};
    return pa;
}

static struct PartialMod *new_partial_mod(struct Collector *X, struct ModuleInfo *m, struct PartialAdtList *pal)
{
    struct PartialMod *pm = pawK_pool_alloc(ENV(X), X->pool, sizeof(struct PartialMod));
    *pm = (struct PartialMod){.pal = pal, .m = m};
    return pm;
}

static DefId add_decl(struct Collector *X, struct HirDecl *decl)
{
    return pawHir_add_decl(X->C, decl);
}

static struct HirScope *enclosing_scope(struct HirSymtab *st)
{
    paw_assert(st->count > 0);
    return pawHir_symtab_get(st, st->count - 1);
}

static struct HirSymbol *add_symbol(struct Collector *X, struct HirScope *scope, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = pawHir_add_symbol(X->C, scope);
    symbol->name = name;
    symbol->decl = decl;
    return symbol;
}

static struct HirSymbol *declare_local(struct Collector *X, String *name, struct HirDecl *decl)
{
    return add_symbol(X, enclosing_scope(X->symtab), name, decl);
}

// Allow a previously-declared variable to be accessed
static void define_local(struct HirSymbol *symbol)
{
    symbol->is_init = PAW_TRUE;
}

static struct HirSymbol *new_global(struct Collector *X, struct HirDecl *decl, paw_Bool is_type)
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

static struct HirSymbol *try_resolve_symbol(struct Collector *X, const String *name)
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

static struct HirSymbol *resolve_symbol(struct Collector *X, const String *name)
{
    struct HirSymbol *symbol = try_resolve_symbol(X, name);
    if (symbol == NULL) NAME_ERROR(X, "undefined symbol '%s'", name->text);
    return symbol;
}

static struct HirSymbol *new_local(struct Collector *X, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = declare_local(X, name, decl);
    define_local(symbol);
    return symbol;
}

static struct HirScope *leave_block(struct Collector *X)
{
    struct HirSymtab *st = X->symtab;
    struct HirScope *scope = enclosing_scope(st);
    --st->count;
    return scope;
}

static void enter_block(struct Collector *X, struct HirScope *scope)
{
    scope = scope != NULL ? scope : pawHir_scope_new(X->C);
    pawHir_symtab_push(X->C, X->symtab, scope);
}

static struct HirScope *leave_function(struct Collector *X)
{
    struct HirScope *scope = leave_block(X);
    CHECK_GC(ENV(X));
    return scope;
}

static void create_context(struct Collector *X, struct HirDecl *decl, int line)
{
    struct HirDecl *result = pawHir_new_decl(X->C, line, kHirVarDecl);
    struct HirVarDecl *r = HirGetVarDecl(result);
    r->name = CSTR(X, CSTR_SELF); // 'self'
    r->type = HIR_TYPEOF(decl);

    new_local(X, r->name, result);
    add_decl(X, result);
}

static struct HirAdtDecl *get_adt(struct Collector *X, struct HirType *type)
{
    const DeclId did = hir_adt_did(type);
    struct HirDecl *decl = pawHir_get_decl(X->C, did);
    return HirGetAdtDecl(decl);
}

static void enter_function(struct Collector *X, struct HirFuncDecl *func)
{
    enter_block(X, NULL);
    new_local(X, func->name, HIR_CAST_DECL(func));
    if (func->fn_kind == FUNC_METHOD) {
        // methods use local slot 1 for the implicit context variable
        const DeclId did = hir_adt_did(func->self);
        struct HirDecl *self = pawHir_get_decl(X->C, did);
        create_context(X, self, func->line);
    }
}

static void dupcheck(struct Collector *X, Map *map, String *name, const char *what)
{
    if (name == NULL) return;
    Value *pv = pawH_get(map, P2V(name));
    if (pv != NULL) NAME_ERROR(X, "duplicate %s '%s'", what, name->text);
    pawH_insert(ENV(X), map, P2V(name), P2V(name));
}

static Map *start_dupcheck(struct Collector *X)
{
    paw_Env *P = ENV(X);
    ENSURE_STACK(P, 1);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    V_SET_OBJECT(pv, map);
    return map;
}

static void finish_dupcheck(struct Collector *X)
{
    pawC_pop(ENV(X));
}

static struct HirType *collect_type(struct Collector *X, struct HirType *type)
{
    X->line = type->hdr.line;
    return pawHir_fold_type(&X->F, type);
}

static void map_adt_to_impl(struct Collector *X, struct HirDecl *adt, struct HirDecl *impl)
{
    Value *pv = pawH_get(X->impls, P2V(adt));
    if (pv == NULL) {
        pv = pawH_create(ENV(X), X->impls, P2V(adt));
        pv->p = pawHir_decl_list_new(X->C);
    }
    struct HirDeclList *impls = pv->p;
    pawHir_decl_list_push(X->C, impls, impl);
}

static struct HirType *collect_path(struct Collector *X, struct HirPath *path)
{
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment *seg = K_LIST_GET(path, i);
        seg->types = pawHir_fold_type_list(&X->F, seg->types);
    }
    struct HirType *type = pawP_lookup(X->C, X->m, X->symtab, path, LOOKUP_TYPE);
    if (type == NULL) NAME_ERROR(X, "bad path");
    return type;
}

// Resolve a toplevel path
// Note that paths existing at the toplevel must refer to ADTs: functions and
// enumeration variants can only be referenced inside function bodies. Such
// paths will look something like 'a::b::C<D, E>', with the only required
// segment being 'C'. Basically, the prefix part ('a::b::') locates the ADT
// in its containing module, and the suffix part ('<D, E>') indicate a
// particular instantiation of 'C'.
static struct HirType *fold_path_type(struct HirTypeFolder *F, struct HirPathType *t)
{
    return collect_path(F->ud, t->path);
}

static struct HirDecl *get_decl(struct Collector *X, DefId did)
{
    struct DynamicMem *dm = X->dm;
    paw_assert(did < dm->decls->count);
    return dm->decls->data[did];
}

static struct HirType *new_type(struct Collector *X, DeclId did, enum HirTypeKind kind, int line)
{
    return pawHir_attach_type(X->C, did, kind, line);
}

static struct HirType *register_decl_type(struct Collector *X, struct HirDecl *decl, enum HirTypeKind kind)
{
    const DeclId did = decl->hdr.did;
    return pawHir_attach_type(X->C, did, kind, decl->hdr.line);
}

static void register_generics(struct Collector *X, struct HirDeclList *generics)
{
    if (generics == NULL) return;
    for (int i = 0; i < generics->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(generics, i);
        struct HirType *type = register_decl_type(X, decl, kHirGeneric);
        struct HirGeneric *t = HirGetGeneric(type);
        struct HirSymbol *symbol = new_local(X, decl->hdr.name, decl);
        symbol->is_generic = PAW_TRUE;
        symbol->is_type = PAW_TRUE;
        t->name = decl->hdr.name;
    }
}

static struct HirTypeList *collect_generic_types(struct Collector *X, struct HirDeclList *generics)
{
    if (generics == NULL) return NULL;
    return pawHir_collect_generics(X->C, generics);
}

static struct HirTypeList *collect_field_types(struct Collector *X, struct HirDeclList *fields)
{
    return pawHir_collect_fields(X->C, fields);
}

static struct HirType *register_func(struct Collector *X, struct HirFuncDecl *d)
{
    struct HirType *type = register_decl_type(X, HIR_CAST_DECL(d), kHirFuncDef);
    struct HirFuncDef *t = HirGetFuncDef(type);
    t->types = collect_generic_types(X, d->generics);
    t->params = collect_field_types(X, d->params);
    t->result = collect_type(X, d->result);
    t->modno = X->m->hir->modno;
    t->base = t->did = d->did;
    return type;
}

static struct HirType *register_adt(struct Collector *X, struct HirAdtDecl *d)
{
    struct HirType *type = register_decl_type(X, HIR_CAST_DECL(d), kHirAdt);
    struct HirAdt *t = HirGetAdt(type);

    t->types = collect_generic_types(X, d->generics);
    t->modno = X->m->hir->modno;
    t->did = d->did;
    return type;
}

static void collect_field_decl(struct Collector *X, struct HirFieldDecl *d)
{
    d->tag = d->type = collect_type(X, d->tag);
}

// 'field' of enumeration has type 'struct HirVariantDecl'
static void collect_variant_decl(struct Collector *, struct HirVariantDecl *);

static void collect_fields(struct Collector *X, struct HirDeclList *fields)
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

static void collect_variant_decl(struct Collector *X, struct HirVariantDecl *d)
{
    collect_fields(X, d->fields);

    // An enum variant name can be thought of as a function from the type of the
    // variant's fields to the type of the enumeration. For example, given 'enum
    // E {X(str)}', E::X has type 'fn(str) -> E'.
    struct HirType *type = register_decl_type(X, HIR_CAST_DECL(d), kHirFuncDef);
    struct HirFuncDef *t = HirGetFuncDef(type);
    t->modno = X->m->hir->modno;
    t->base = d->did;
    t->params = d->fields != NULL
        ? collect_field_types(X, d->fields)
        : pawHir_type_list_new(X->C);
    t->result = X->adt;
    d->type = type;
}

static paw_Bool check_static_method(struct HirType *self, struct HirDeclList *params)
{
    if (self == NULL) return PAW_FALSE; // not a method
    if (params->count == 0) return PAW_TRUE;
    struct HirFieldDecl *first = HirGetFieldDecl(K_LIST_GET(params, 0));
    return first->type != self;
}

static void collect_func(struct Collector *X, struct HirFuncDecl *d)
{
    enter_function(X, d);
    register_generics(X, d->generics);
    collect_fields(X, d->params);
    d->type = register_func(X, d);
    leave_function(X);

    d->is_assoc = check_static_method(d->self, d->params);
}

static void collect_func_decl(struct Collector *X, struct HirFuncDecl *d)
{
    new_global(X, HIR_CAST_DECL(d), PAW_FALSE);
    collect_func(X, d);
}

static void maybe_fix_builtin(struct Collector *X, String *name, DefId did)
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

static struct HirScope *register_adt_decl(struct Collector *X, struct HirAdtDecl *d)
{
    enter_block(X, NULL);
    register_generics(X, d->generics);
    d->type = register_adt(X, d);
    maybe_fix_builtin(X, d->name, d->did);
    new_global(X, HIR_CAST_DECL(d), PAW_TRUE);
    return leave_block(X);
}

static void collect_adt_decl(struct Collector *X, struct PartialAdt *lazy)
{
    struct HirAdtDecl *d = lazy->d;
    enter_block(X, lazy->scope);
    WITH_ADT_CONTEXT(X, d->type,
            collect_fields(X, d->fields););
    leave_block(X);
}

static void collect_methods(struct Collector *X, struct HirDeclList *methods)
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

static struct HirType *collect_self(struct Collector *X, struct HirImplDecl *d)
{
    String *selfname = SCAN_STRING(X->C, "Self");
    struct HirType *self = collect_path(X, d->self);
    struct HirDecl *result = pawHir_new_decl(X->C, d->line, kHirTypeDecl);
    struct HirTypeDecl *r = HirGetTypeDecl(result);
    pawHir_add_decl(X->C, result);
    r->type = self;

    map_adt_to_impl(X, get_decl(X, HIR_TYPE_DID(self)), HIR_CAST_DECL(d));
    struct HirSymbol *symbol = new_local(X, selfname, result);
    symbol->is_type = PAW_TRUE;
    return self;
}

static void collect_impl_decl(struct Collector *X, struct HirImplDecl *d)
{
    enter_block(X, NULL);
    register_generics(X, d->generics);
    d->subst = collect_generic_types(X, d->generics);
    d->type = collect_self(X, d);
    WITH_ADT_CONTEXT(X, d->type,
            collect_methods(X, d->methods););
    leave_block(X);
}

static void collect_use_decl(struct Collector *X, struct HirUseDecl *d)
{
    new_global(X, HIR_CAST_DECL(d), PAW_FALSE);
}

static struct ModuleInfo *use_module(struct Collector *X, struct ModuleInfo *m)
{
    pawU_enter_binder(&X->dm->unifier);
    X->m = m;
    return m;
}

static void finish_module(struct Collector *X)
{
    pawU_leave_binder(&X->dm->unifier);
    X->m = NULL;
}

static struct PartialAdtList *register_adts(struct Collector *X, struct HirDeclList *items)
{
    struct PartialAdtList *list = pa_list_new(X);
    for (int i = 0; i < items->count; ++i) {
        struct HirDecl *item = K_LIST_GET(items, i);
        if (!HirIsAdtDecl(item)) continue;
        struct HirAdtDecl *d = HirGetAdtDecl(item);
        struct HirScope *scope = register_adt_decl(X, d);
        struct PartialAdt *pa = new_partial_adt(X, d, scope);
        pa_list_push(X, list, pa);
    }
    return list;
}

static void collect_adts(struct Collector *X, struct PartialAdtList *list)
{
    for (int i = 0; i < list->count; ++i) {
        struct PartialAdt *pa = K_LIST_GET(list, i);
        collect_adt_decl(X, pa);
    }
}

static struct PartialModList *collect_phase_1(struct Collector *X, struct ModuleList *ml)
{
    struct PartialModList *pml = pm_list_new(X);

    // collect toplevel ADT names and type parameters
    for (int i = 0; i < ml->count; ++i) {
        struct ModuleInfo *m = use_module(X, K_LIST_GET(ml, i));
        paw_assert(m->globals->count == 0);
        struct PartialAdtList *pal = register_adts(X, m->hir->items);
        struct PartialMod *pm = new_partial_mod(X, m, pal);
        pm_list_push(X, pml, pm);
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
static void collect_items(struct Collector *X, struct Hir *hir)
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

static void collect_phase_2(struct Collector *X, struct ModuleList *ml, struct PartialModList *pml)
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
    struct Collector X = {
        .symtab = pawHir_symtab_new(C),
        .pool = &dm->pool,
        .impls = C->impls,
        .P = ENV(C),
        .dm = dm,
        .C = C,
    };
    pawHir_type_folder_init(&X.F, C, &X);
    X.F.FoldPathType = fold_path_type;

    DLOG(&X, "collecting %d module(s)", dm->modules->count);

    struct PartialModList *pml = collect_phase_1(&X, dm->modules);
    collect_phase_2(&X, dm->modules, pml);
}
