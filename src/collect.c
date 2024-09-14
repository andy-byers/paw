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

#include "hir.h"
#include "gc.h"
#include "map.h"

#define NAME_ERROR(X, ...) pawE_error(ENV(X), PAW_ENAME, (X)->line, __VA_ARGS__)
#define SYNTAX_ERROR(X, ...) pawE_error(ENV(X), PAW_ESYNTAX, (X)->line, __VA_ARGS__)
#define TYPE_ERROR(X, ...) pawE_error(ENV(X), PAW_ETYPE, (X)->line, __VA_ARGS__)
#define CACHED_STR(X, i) pawE_cstr(ENV(X), CAST_SIZE(i))

struct Collector {
    struct HirFolder F;
    struct HirSymtab *symtab;
    struct HirScope *globals;
    struct DynamicMem *dm;
    struct Compiler *C;
    struct Hir *hir;
    struct HirType *adt;
    paw_Env *P;
    int line;
};

struct PartialAdt {
    struct HirAdtDecl *d;
    struct HirScope *scope;
};

static struct PartialAdt *new_partial_adt(struct Collector *X, struct HirAdtDecl *d)
{
    struct PartialAdt *pa = pawK_pool_alloc(ENV(X), &X->hir->pool, sizeof(struct PartialAdt));
    *pa = (struct PartialAdt){.d = d};
    return pa;
}

DEFINE_LIST(struct Hir, pa_list_, PartialAdtList, struct PartialAdt)

static DefId add_decl(struct Collector *X, struct HirDecl *decl)
{
    const DefId did = X->dm->decls->count;
    pawHir_decl_list_push(X->hir, X->dm->decls, decl);
    decl->hdr.did = did;
    return did;
}

static struct HirScope *enclosing_scope(struct HirSymtab *st)
{
    paw_assert(st->count > 0);
    return pawHir_symtab_get(st, st->count - 1);
}

static struct HirSymbol *add_symbol(struct Collector *X, struct HirScope *scope, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = pawHir_add_symbol(X->hir, scope);
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

static struct HirSymbol *new_global(struct Collector *X, struct HirDecl *decl)
{
    String *name = decl->hdr.name;
    struct HirScope *scope = X->globals;
    for (int i = 0; i < scope->count; ++i) {
        struct HirSymbol *symbol = scope->data[i];
        if (pawS_eq(symbol->name, name)) {
            NAME_ERROR(X, "duplicate global '%s' (declared previously on line %d)",
                       name->text, symbol->decl->hdr.line);
        }
    }
    struct HirSymbol *symbol = add_symbol(X, scope, name, decl);
    symbol->is_init = PAW_TRUE;
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
    const int index = pawHir_find_symbol(X->globals, name);
    if (index < 0) return NULL;
    return X->globals->data[index];
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
    scope = scope != NULL ? scope : pawHir_scope_new(X->hir);
    pawHir_symtab_push(X->hir, X->symtab, scope);
}

static struct HirScope *leave_function(struct Collector *X)
{
    struct HirScope *scope = leave_block(X);
    CHECK_GC(ENV(X));
    return scope;
}

static void create_context(struct Collector *X, struct HirDecl *decl, int line)
{
    struct HirDecl *result = pawHir_new_decl(X->hir, line, kHirVarDecl);
    struct HirVarDecl *r = HirGetVarDecl(result);
    r->name = pawE_cstr(ENV(X), CSTR_SELF); // 'self'
    r->type = HIR_TYPEOF(decl);

    new_local(X, r->name, result);
    add_decl(X, result);
}

static struct HirAdtDecl *get_adt(struct Collector *X, struct HirType *type)
{
    const DeclId did = hir_adt_did(type);
    struct HirDecl *decl = pawHir_get_decl(X->hir, did);
    return HirGetAdtDecl(decl);
}

static void enter_function(struct Collector *X, struct HirFuncDecl *func)
{
    enter_block(X, NULL);
    new_local(X, func->name, HIR_CAST_DECL(func));
    if (func->fn_kind == FUNC_METHOD) {
        // methods use local slot 1 for the implicit context variable
        const DeclId did = hir_adt_did(func->self);
        struct HirDecl *self = pawHir_get_decl(X->hir, did);
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
    return X->F.FoldType(&X->F, type);
}

static struct HirDecl *instantiate_adt(struct Collector *X, struct HirDecl *base, struct HirTypeList *types)
{
    paw_assert(HirIsAdtDecl(base));
    return pawP_preinstantiate(X->C, base, types);
}

static struct HirType *collect_path(struct Collector *X, struct HirPath *path)
{
    struct HirSegment *seg = K_LIST_GET(path, 0);
    seg->types = X->F.FoldTypeList(&X->F, seg->types);
    struct HirSymbol *sym = resolve_symbol(X, seg->name);
    struct HirDecl *decl = sym->decl;
    if (HirIsGenericDecl(decl)) return HIR_TYPEOF(decl); 
    seg->base = seg->did = decl->hdr.did;
    if (HirIsInstanceDecl(decl)) return HIR_TYPEOF(decl);
    if (!HirIsAdtDecl(decl)) TYPE_ERROR(X, "'%s' is not a type", sym->name->text);
    struct HirDecl *inst = instantiate_adt(X, decl, seg->types);
    seg->did = inst->hdr.did;
    return HIR_TYPEOF(inst);
}

// Resolve a toplevel path
// Note that paths existing at the toplevel must refer to ADTs: functions and
// enumeration variants can only be referenced inside function bodies. Such 
// paths will look something like 'a::b::C<D, E>', with the only required 
// segment being 'C'. Basically, the prefix part ('a::b::') locates the ADT 
// in its containing module, and the suffix part ('<D, E>') indicate a 
// particular instantiation of 'C'.
static struct HirType *fold_path_type(struct HirFolder *F, struct HirPathType *t)
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
    return pawHir_attach_type(X->hir, did, kind, line);
}

static struct HirType *register_decl_type(struct Collector *X, struct HirDecl *decl, enum HirTypeKind kind)
{
    const DeclId did = add_decl(X, decl);
    return pawHir_attach_type(X->hir, did, kind, decl->hdr.line);
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
    return pawHir_collect_generics(X->hir, generics);
}

static struct HirTypeList *collect_field_types(struct Collector *X, struct HirDeclList *fields)
{
    return pawHir_collect_fields(X->hir, fields);
}

static struct HirType *register_func(struct Collector *X, struct HirFuncDecl *d)
{
    struct HirType *type = register_decl_type(X, HIR_CAST_DECL(d), kHirFuncDef);
    struct HirFuncDef *t = HirGetFuncDef(type);
    t->types = collect_generic_types(X, d->generics);
    t->params = collect_field_types(X, d->params);
    t->result = collect_type(X, d->result);
    t->base = t->did = d->did;
    return type;
}

static struct HirType *register_adt(struct Collector *X, struct HirAdtDecl *d)
{
    struct HirType *type = register_decl_type(X, HIR_CAST_DECL(d), kHirPathType);
    struct HirPathType *t = HirGetPathType(type);
    t->path = pawHir_path_new(X->hir);

    struct HirTypeList *generics = d->generics != NULL
        ? collect_generic_types(X, d->generics)
        : NULL;

    struct HirSegment *seg = pawHir_path_add(X->hir, t->path, d->name, generics);
    seg->base = seg->did = d->did;
    return type;
}

static void collect_field_decl(struct Collector *X, struct HirFieldDecl *d)
{
    add_decl(X, HIR_CAST_DECL(d));
    d->type = collect_type(X, d->tag);
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
    t->base = d->did;
    t->params = d->fields != NULL
        ? collect_field_types(X, d->fields)
        : pawHir_type_list_new(X->hir);
    t->result = X->adt;
    d->type = type;
}

static void collect_func(struct Collector *X, struct HirFuncDecl *d)
{
    enter_function(X, d);
    register_generics(X, d->generics);
    collect_fields(X, d->params);
    d->type = register_func(X, d);
    leave_function(X);
}

static void collect_func_decl(struct Collector *X, struct HirFuncDecl *d)
{
    new_global(X, HIR_CAST_DECL(d));
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
    new_global(X, HIR_CAST_DECL(d));
    return leave_block(X);
}

static void collect_adt_decl(struct Collector *X, struct PartialAdt *lazy)
{
    struct HirAdtDecl *d = lazy->d;
    enter_block(X, lazy->scope);
    WITH_ADT_CONTEXT(X, 
            d->type, 
            collect_fields(X, d->fields););
    leave_block(X);
}

static void expand_adt(struct Collector *X, struct HirAdtDecl *d)
{
    for (int i = 0; i < d->monos->count; ++i) {
        struct HirDecl *inst = K_LIST_GET(d->monos, i);
        if (HirIsInstanceDecl(inst)) pawHir_expand_adt(X->C, d, inst);
    }
}

static void correct_adt_decl(struct Collector *X, struct HirAdtDecl *d)
{
    if (d->monos != NULL) expand_adt(X, d);
}

static void register_methods(struct Collector *X, struct HirDeclList *methods, struct HirType *self)
{
    Map *map = start_dupcheck(X);

    for (int i = 0; i < methods->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(methods, i);
        dupcheck(X, map, decl->hdr.name, "method");
    }

    finish_dupcheck(X);
}

static void collect_methods(struct Collector *X, struct HirDeclList *methods)
{
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

static struct HirType *collect_self(struct Collector *X, struct HirPath *path)
{
    String *name = SCAN_STRING(X->C, "Self");
    struct HirType *self = collect_path(X, path);
    struct HirDecl *decl = get_decl(X, hir_adt_did(self));
    struct HirSymbol *symbol = new_local(X, name, decl);
    symbol->is_type = PAW_TRUE;
    return self;
}

static void collect_impl_decl(struct Collector *X, struct HirImplDecl *d)
{
    add_decl(X, HIR_CAST_DECL(d));

    enter_block(X, NULL);
    register_generics(X, d->generics);
    d->subst = collect_generic_types(X, d->generics);
    WITH_ADT_CONTEXT(X, 
            collect_self(X, d->self), 
            collect_methods(X, d->methods););
    leave_block(X);
}

static void collect_items(struct Collector *X, struct HirDeclList *items)
{
    struct PartialAdtList *pa_list = pa_list_new(X->hir);
    for (int i = 0; i < items->count; ++i) {
        struct HirDecl *item = K_LIST_GET(items, i);
        if (!HirIsAdtDecl(item)) continue;
        struct PartialAdt *pa = new_partial_adt(X, HirGetAdtDecl(item));
        pa->scope = register_adt_decl(X, HirGetAdtDecl(item));
        pa_list_push(X->hir, pa_list, pa);
    }
    for (int i = 0; i < pa_list->count; ++i) {
        struct PartialAdt *pa = K_LIST_GET(pa_list, i);
        collect_adt_decl(X, pa);
    }
    for (int i = 0; i < items->count; ++i) {
        struct HirDecl *item = K_LIST_GET(items, i);
        if (HirIsFuncDecl(item)) {
            collect_func_decl(X, HirGetFuncDecl(item));
        } else if (HirIsImplDecl(item)) {
            collect_impl_decl(X, HirGetImplDecl(item));
        }
    }
    for (int i = 0; i < pa_list->count; ++i) {
        struct PartialAdt *pa = K_LIST_GET(pa_list, i);
        correct_adt_decl(X, pa->d);
    }
}

// Entrypoint to item collection
struct HirScope *pawP_collect_items(struct Compiler *C, struct Hir *hir)
{
    struct Collector X = {
        .globals = pawHir_scope_new(hir),
        .symtab = pawHir_symtab_new(hir),
        .dm = C->dm,
        .P = ENV(C),
        .hir = hir,
        .C = C,
    };
    pawHir_folder_init(&X.F, hir, &X);
    X.F.FoldPathType = fold_path_type;

    struct Unifier *U = &X.dm->unifier;
    pawU_enter_binder(U);
    collect_items(&X, hir->prelude);
    collect_items(&X, hir->items);
    pawU_leave_binder(U);

    return X.globals;
}
