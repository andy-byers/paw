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
#include "unify.h"

#define MOD(X) (X)->m->hir->modno
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
    Map *traits;
    paw_Env *P;
    int ndefs;
    int line;
};

struct PartialDecl {
    struct HirDecl *decl;
    struct HirScope *scope;
};

struct PartialModule {
    struct PartialDeclList *pal;
    struct ModuleInfo *m;
};

DEFINE_LIST(struct ItemCollector, pa_list_, PartialDeclList, struct PartialDecl)
DEFINE_LIST(struct ItemCollector, pm_list_, PartialModList, struct PartialModule)

static struct HirScope *enclosing_scope(struct HirSymtab *st)
{
    paw_assert(st->count > 0);
    return K_LIST_GET(st, st->count - 1);
}

static void add_symbol(struct ItemCollector *X, struct HirScope *scope, String *name, struct HirDecl *decl)
{
    const int index = pawHir_declare_symbol(X->C, scope, decl, name);
    pawHir_define_symbol(scope, index);
}

static void new_global(struct ItemCollector *X, String *name, struct HirDecl *decl)
{
    struct HirSymbol *psymbol;
    struct HirScope *scope = X->m->globals;
    K_LIST_FOREACH(scope, psymbol) {
        if (pawS_eq(psymbol->name, name)) {
            NAME_ERROR(X, "duplicate global '%s' (declared previously on line %d)",
                       name->text, psymbol->decl->hdr.line);
        }
    }
    add_symbol(X, scope, name, decl);
}

static void new_local(struct ItemCollector *X, String *name, struct HirDecl *decl)
{
    add_symbol(X, enclosing_scope(X->symtab), name, decl);
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
    struct HirDecl *result = pawHir_new_var_decl(X->m->hir, line, self, NULL, NULL);
    SET_NODE_TYPE(X->C, result, type);
    new_local(X, self, result);
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

static void ensure_unique(struct ItemCollector *X, Map *map, String *name, const char *what)
{
    if (name == NULL) return;
    Value *pv = pawH_get(map, P2V(name));
    if (pv != NULL) NAME_ERROR(X, "duplicate %s '%s'", what, name->text);
    pawH_insert(ENV(X), map, P2V(name), P2V(name));
}

static struct IrType *collect_type(struct ItemCollector *X, struct HirType *type)
{
    X->line = type->hdr.line;
    struct IrType *result = pawP_lower_type(X->C, X->m, X->symtab, type);
    if (result == NULL) {
        const char *type_name = pawHir_print_type(X->C, type);
        TYPE_ERROR(X, "unrecognized type '%s'", type_name);
    }
    pawIr_validate_type(X->C, result);
    SET_NODE_TYPE(X->C, type, result);
    return result;
}

static void map_adt_to_trait(struct ItemCollector *X, struct HirDecl *adt, struct IrType *trait)
{
    struct IrTypeList *traits;
    const DeclId did = adt->hdr.did;
    Value *pv = MAP_GET(X->traits, I2V(did.value));
    if (pv == NULL) {
        traits = pawIr_type_list_new(X->C);
        pv = MAP_INSERT(X, X->traits, I2V(did.value), P2V(traits));
    }
    K_LIST_PUSH(X->C, traits, trait);
}

static struct IrType *collect_trait_path(struct ItemCollector *X, struct HirPath *path)
{
    struct IrType *trait = pawP_lookup_trait(X->C, X->m, X->symtab, path);
    if (trait == NULL) {
        const char *trait_name = pawHir_print_path(X->C, path);
        NAME_ERROR(X, "unknown trait '%s'", trait_name);
    }
    pawIr_validate_type(X->C, trait);
    return trait;
}

static struct HirDecl *get_decl(struct ItemCollector *X, DeclId did)
{
    return pawHir_get_decl(X->C, did);
}

static struct IrTypeList *collect_bounds(struct ItemCollector *X, struct HirBoundList *bounds)
{
    if (bounds == NULL) return NULL;
    struct IrTypeList *result = pawIr_type_list_new(X->C);

    struct HirGenericBound *pbound;
    K_LIST_FOREACH(bounds, pbound) {
        struct IrType *type = collect_trait_path(X, pbound->path);
        K_LIST_PUSH(X->C, result, type);
    }
    return result;
}

static void register_generics(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics == NULL) return;

    // first pass creates a local symbol for each generic
    struct HirDecl **pdecl;
    struct IrTypeList *types = pawIr_type_list_new(X->C);
    K_LIST_FOREACH(generics, pdecl) {
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        struct IrType *type = pawIr_new_generic(X->C, d->did, NULL);
        SET_NODE_TYPE(X->C, *pdecl, type);
        new_local(X, d->name, *pdecl);
        K_LIST_PUSH(X->C, types, type);
    }

    // second pass resolves generic bounds, which might mention other generics
    // in the same binder
    struct IrType **ptype;
    K_LIST_ZIP(generics, pdecl, types, ptype) {
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        struct IrGeneric *g = IrGetGeneric(*ptype);
        g->bounds = collect_bounds(X, d->bounds);
    }
}

static struct IrTypeList *collect_generic_types(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics == NULL) return NULL;
    return pawHir_collect_decl_types(X->C, generics);
}

//static struct IrGenericList *collect_generics(struct ItemCollector *X, struct HirDeclList *generics)
//{
//    struct HirDecl **pdecl;
//    if (generics == NULL) return NULL;
//    struct IrGenericList *result = pawIr_generic_list_new(X->C);
//    K_LIST_FOREACH(generics, pdecl) {
//        const DeclId did = pawIr_next_did(X->C, MOD(X));
//        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
//        struct IrType *type = pawIr_new_generic(X->C, did);
//        struct IrDef *r = pawIr_new_generic_def(X->C, did, d->name);
//        K_LIST_PUSH(X->C, result, IrGetGenericDef(r));
//        pawIr_set_def(X->C, did, r);
//    }
//    return result;
//}
//
//static struct IrFieldList *collect_fields(struct ItemCollector *X, struct HirDeclList *fields)
//{
//    struct HirDecl **pdecl;
//    struct IrFieldList *result = pawIr_field_list_new(X->C);
//    K_LIST_FOREACH(fields, pdecl) {
//        struct HirFieldDecl *d = HirGetFieldDecl(*pdecl);
//        struct IrType *type = collect_type(X, d->tag);
//        const DeclId did = pawIr_next_did(X->C, MOD(X));
//        struct IrDef *r = pawIr_new_field_def(X->C, did, d->name, d->is_pub);
//        K_LIST_PUSH(X->C, result, IrGetFieldDef(r));
//        pawIr_set_def(X->C, did, r);
//    }
//    return result;
//}
//
//static struct IrParamList *collect_parameters(struct ItemCollector *X, struct HirDeclList *params)
//{
//    struct HirDecl **pdecl;
//    struct IrParamList *result = pawIr_param_list_new(X->C);
//    K_LIST_FOREACH(params, pdecl) {
//        struct HirFieldDecl *d = HirGetFieldDecl(*pdecl);
//        struct IrType *type = collect_type(X, d->tag);
//        const DeclId did = pawIr_next_did(X->C, MOD(X));
//        struct IrDef *r = pawIr_new_param_def(X->C, did, d->name);
//        K_LIST_PUSH(X->C, result, IrGetParamDef(r));
//        pawIr_set_def(X->C, did, r);
//    }
//    return result;
//}

static void register_func(struct ItemCollector *X, struct HirFuncDecl *d)
{
    struct IrTypeList *types = collect_generic_types(X, d->generics);
    struct IrTypeList *params_ = pawHir_collect_decl_types(X->C, d->params);
    struct IrType *result = collect_type(X, d->result);
    struct IrType *sig = pawIr_new_signature(X->C, d->did, types, params_, result);
    SET_TYPE(X, d->hid, sig);

  //  const DeclId did = pawIr_next_did(X->C, MOD(X));
  //  struct IrGenericList *generics = collect_generics(X, d->generics);
  //  struct IrParamList *params = collect_parameters(X, d->params);
  //  struct IrDef *r = pawIr_new_func_def(X->C, did, d->name, generics, params, d->is_pub);
  //  pawIr_set_def(X->C, did, r);
}

//static struct IrVariantList *create_struct_variant(struct ItemCollector *X, String *name, struct HirDeclList *decls, DeclId did)
//{
//    struct IrFieldList *fields = collect_fields(X, decls);
//    struct IrDef *r = pawIr_new_variant_def(X->C, did, 0, name, fields);
//    struct IrVariantList *variants = pawIr_variant_list_new(X->C);
//    K_LIST_PUSH(X->C, variants, IrGetVariantDef(r));
//    return variants;
//}

//static struct IrVariantList *collect_variants(struct ItemCollector *X, struct HirAdtDecl *d, DeclId base_did)
//{
//    if (d->is_struct) return create_struct_variant(X, d->name, d->fields, base_did);
//    struct IrVariantList *variants = pawIr_variant_list_new(X->C);
//
//    struct HirDecl **pdecl;
//    K_LIST_FOREACH(d->fields, pdecl) {
//        const DeclId did = pawIr_next_did(X->C, MOD(X));
//        struct HirVariantDecl *d = HirGetVariantDecl(*pdecl);
//        struct IrFieldList *fields = collect_fields(X, d->fields);
//        struct IrDef *r = pawIr_new_variant_def(X->C, did, d->index, d->name, fields);
//        K_LIST_PUSH(X->C, variants, IrGetVariantDef(r));
//        pawIr_set_def(X->C, did, r);
//    }
//    return variants;
//}

static struct IrType *register_adt(struct ItemCollector *X, struct HirAdtDecl *d)
{
//    const DeclId did = pawIr_next_did(X->C, MOD(X));
//    struct IrGenericList *generics = collect_generics(X, d->generics);
//    struct IrVariantList *variants = collect_variants(X, d, did);
//    struct IrDef *r = pawIr_new_adt_def(X->C, did, d->name, generics, variants, d->is_pub, d->is_struct);
//    pawIr_set_def(X->C, did, r);

    struct IrTypeList *types = collect_generic_types(X, d->generics);
    struct IrType *type = pawIr_new_adt(X->C, d->did, types);
    SET_TYPE(X, d->hid, type);
    return type;
}

static void collect_field_decl(struct ItemCollector *X, struct HirFieldDecl *d)
{
    SET_TYPE(X, d->hid, collect_type(X, d->tag));
}

static void collect_variant_decl(struct ItemCollector *, struct HirVariantDecl *);

static void collect_field_types(struct ItemCollector *X, struct HirDeclList *fields, Map *names)
{
    if (fields == NULL) return;
    for (int i = 0; i < fields->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(fields, i);
        if (HirIsFieldDecl(decl)) {
            struct HirFieldDecl *d = HirGetFieldDecl(decl);
            ensure_unique(X, names, d->name, "struct field");
            collect_field_decl(X, d);
        } else {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            ensure_unique(X, names, d->name, "enum variant");
            collect_variant_decl(X, d);
        }
    }
}

static void collect_variant_decl(struct ItemCollector *X, struct HirVariantDecl *d)
{
    collect_field_types(X, d->fields, NULL);

    // An enum variant name can be thought of as a function from the type of the
    // variant's fields to the type of the enumeration. For example, given 'enum
    // E {X(str)}', E::X has type 'fn(str) -> E'.
    struct IrTypeList *params = d->fields != NULL
        ? pawHir_collect_decl_types(X->C, d->fields)
        : pawIr_type_list_new(X->C);
    struct IrType *type = pawIr_new_signature(X->C, d->did, NULL, params, X->adt);
    SET_TYPE(X, d->hid, type);
}

static paw_Bool check_assoc_function(struct ItemCollector *X, struct IrType *self, struct HirDeclList *params)
{
    if (self == NULL) return PAW_FALSE; // not a method
    if (params->count == 0) return PAW_TRUE;
    struct HirDecl *first = K_LIST_GET(params, 0);
    // TODO: check for "self" or "self: Self" to determine if method
    return GET_NODE_TYPE(X->C, first) != self;
}

static void collect_func(struct ItemCollector *X, struct HirFuncDecl *d)
{
    enter_function(X, d);
    register_generics(X, d->generics);

    Map *names = pawP_push_map(X->C);
    collect_field_types(X, d->params, names);
    pawP_pop_object(X->C, names);

    register_func(X, d);
    leave_function(X);

    d->is_assoc = check_assoc_function(X, X->adt, d->params);
    if (d->self != NULL) {
        struct IrType *type = GET_TYPE(X, d->hid);
        pawP_set_self(X->C, IrGetSignature(type), d->self);
        if (X->impl_binder != NULL) {
            pawP_set_binder(X->C, d->did, X->impl_binder);
        }
    }
}

static void collect_func_decl(struct ItemCollector *X, struct HirFuncDecl *d)
{
    new_global(X, d->name, HIR_CAST_DECL(d));
    collect_func(X, d);
}

static void collect_type_decl(struct ItemCollector *X, struct HirTypeDecl *d)
{
    new_global(X, d->name, HIR_CAST_DECL(d));

    enter_block(X, NULL);
    register_generics(X, d->generics);
    struct IrType *type = collect_type(X, d->rhs);
    SET_NODE_TYPE(X->C, HIR_CAST_DECL(d), type);
    SET_NODE_TYPE(X->C, d->rhs, type);
    leave_block(X);
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

#define WITH_CONTEXT(X, type, code) do { \
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
    new_global(X, d->name, HIR_CAST_DECL(d));
    return leave_block(X);
}

static void collect_methods(struct ItemCollector *X, struct HirDeclList *methods, Map *names, paw_Bool force_pub)
{
    struct HirDecl **pdecl;
    K_LIST_FOREACH(methods, pdecl) {
        struct HirFuncDecl *d = HirGetFuncDecl(*pdecl);
        ensure_unique(X, names, d->name, "method");
        if (force_pub) d->is_pub = PAW_TRUE;
        d->self = X->adt;
        collect_func(X, d);
    }
}

static struct HirDecl *declare_self(struct ItemCollector *X, int line, struct IrType *type)
{
    String *name = SCAN_STRING(X->C, "Self");
    struct HirDecl *self = pawHir_new_type_decl(X->m->hir, line, name, NULL, NULL);
    SET_TYPE(X, self->hdr.hid, type);
    new_local(X, name, self);
    return self;
}

static struct HirDecl *copy_and_collect_method(struct ItemCollector *X, struct HirFuncDecl *d)
{
    struct HirDecl *copy = pawHir_new_func_decl(X->m->hir, d->line, d->name, X->adt, d->generics,
            d->params, d->result, d->body, d->fn_kind, d->is_pub, d->is_assoc);
    collect_func(X, HirGetFuncDecl(copy));
    return copy;
}

static void collect_default_methods(struct ItemCollector *X, Map *names, struct HirTypeList *traits, struct HirDeclList *methods)
{
    struct HirType **ptype;
    K_LIST_FOREACH(traits, ptype) {
        struct IrType *type = GET_NODE_TYPE(X->C, *ptype);
        struct HirTraitDecl *trait = HirGetTraitDecl(
                pawHir_get_decl(X->C, IR_TYPE_DID(type)));
        struct HirDecl **pmethod;
        K_LIST_FOREACH(trait->methods, pmethod) {
            struct HirFuncDecl *method = HirGetFuncDecl(*pmethod);
            if (method->body == NULL) continue; // not defaulted
            const Value *pval = MAP_GET(names, P2V(method->name));
            if (pval == NULL) { // implementation not provided
                struct HirDecl *copy = copy_and_collect_method(X, method);
                K_LIST_PUSH(X->C, methods, copy);
            }
        }
    }
}

// TODO: methods + fields should probably be unique, pass map to collect_*() from this function
//       ambiguity between method call and calling function pointer field, both are in value namespace
static void collect_adt_decl(struct ItemCollector *X, struct PartialDecl lazy)
{
    struct HirAdtDecl *d = HirGetAdtDecl(lazy.decl);
    enter_block(X, lazy.scope);
    struct IrType *type = GET_TYPE(X, d->hid);
// TODO: was previously set by impl block decls, why isn't this necessary for ADTs???
//    X->impl_binder = collect_generic_types(X, d->generics);

    struct HirType **ptype;
    K_LIST_FOREACH(d->traits, ptype) {
        struct HirPathType *path = HirGetPathType(*ptype);
        struct IrType *trait = collect_trait_path(X, path->path);
        pawP_add_trait_impl(X->C, type, trait);
        SET_NODE_TYPE(X->C, *ptype, trait);
    }

    WITH_CONTEXT(X, type,
        Map *names = pawP_push_map(X->C);
        d->self = declare_self(X, d->line, type);
        collect_field_types(X, d->fields, names);
        collect_methods(X, d->methods, names, PAW_FALSE);
        collect_default_methods(X, names, d->traits, d->methods);
        pawP_pop_object(X->C, names);
    );

    pawP_validate_adt_traits(X->C, d);
    leave_block(X);
}

static struct ModuleInfo *use_module(struct ItemCollector *X, struct ModuleInfo *m)
{
    pawU_enter_binder(X->C->U);
    X->m = m;
    return m;
}

static void finish_module(struct ItemCollector *X)
{
    pawU_leave_binder(X->C->U);
    X->m = NULL;
}

static struct HirScope *register_trait_decl(struct ItemCollector *X, struct HirTraitDecl *d)
{
    enter_block(X, NULL);
    register_generics(X, d->generics);
    X->impl_binder = collect_generic_types(X, d->generics);
    struct IrType *type = pawIr_new_trait_obj(X->C, d->did, X->impl_binder);
    new_global(X, d->name, HIR_CAST_DECL(d));
    SET_TYPE(X, d->hid, type);
    return leave_block(X);
}

static void collect_trait_decl(struct ItemCollector *X, struct PartialDecl lazy)
{
    enter_block(X, lazy.scope);
    struct HirTraitDecl *d = HirGetTraitDecl(lazy.decl);
    X->impl_binder = collect_generic_types(X, d->generics);
    struct IrType *type = GET_NODE_TYPE(X->C, lazy.decl);

    WITH_CONTEXT(X, type,
        Map *names = pawP_push_map(X->C);
        d->self = declare_self(X, d->line, type);
        collect_methods(X, d->methods, names, d->is_pub);
        pawP_pop_object(X->C, names);
    );
    leave_block(X);
}

static struct PartialDeclList *register_adts(struct ItemCollector *X, struct HirDeclList *items)
{
    struct PartialDeclList *list = pa_list_new(X);
    for (int i = 0; i < items->count; ++i) {
        struct HirDecl *item = K_LIST_GET(items, i);
        if (HirIsTraitDecl(item)) {
            struct HirTraitDecl *d = HirGetTraitDecl(item);
            struct HirScope *scope = register_trait_decl(X, d);
            struct PartialDecl pd = {.decl = item, .scope = scope};
            K_LIST_PUSH(X, list, pd);
        } else if (HirIsAdtDecl(item)) {
            struct HirAdtDecl *d = HirGetAdtDecl(item);
            struct HirScope *scope = register_adt_decl(X, d);
            struct PartialDecl pd = {.decl = item, .scope = scope};
            K_LIST_PUSH(X, list, pd);
        }
    }
    return list;
}

static void collect_adts(struct ItemCollector *X, struct PartialDeclList *list)
{
    for (int i = 0; i < list->count; ++i) {
        struct PartialDecl pd = K_LIST_GET(list, i);
        if (HirIsAdtDecl(pd.decl)) {
            collect_adt_decl(X, pd);
        } else {
            collect_trait_decl(X, pd);
        }
    }
}

static struct PartialModList *collect_phase_1(struct ItemCollector *X, struct ModuleList *ml)
{
    struct PartialModList *pml = pm_list_new(X);

    // collect toplevel ADT names and type parameters
    for (int i = 0; i < ml->count; ++i) {
        struct ModuleInfo *m = use_module(X, K_LIST_GET(ml, i));
        paw_assert(m->globals->count == 0);
        struct PartialDeclList *pal = register_adts(X, m->hir->items);
        struct PartialModule pm = {.m = m, .pal = pal};
        K_LIST_PUSH(X, pml, pm);
        m->globals = X->m->globals;
        finish_module(X);
    }

    // fill in toplevel ADT field and method types (may instantiate polymorphic ADTs)
    for (int i = 0; i < pml->count; ++i) {
        struct PartialModule pm = K_LIST_GET(pml, i);
        use_module(X, pm.m);
        collect_adts(X, pm.pal);
        finish_module(X);
    }

    // clean up scratch memory
    for (int i = 0; i < pml->count; ++i) {
        struct PartialModule pm = K_LIST_GET(pml, i);
        pa_list_delete(X, pm.pal);
    }
    pm_list_delete(X, pml);

    return pml;
}

static struct HirDecl *find_item_in(struct ItemCollector *X, int modno, String *name)
{
    struct ModuleInfo *m = K_LIST_GET(X->C->modules, modno);
    struct HirDeclList *items = m->hir->items;

    struct HirDecl **pitem;
    K_LIST_FOREACH(items, pitem) {
        const String *item_name = (*pitem)->hdr.name;
        if (pawS_eq(item_name, name)) return *pitem;
    }
    return NULL;
}

static void collect_items(struct ItemCollector *X, struct Hir *hir)
{
    X->symtab = pawHir_symtab_new(X->C);
    for (int i = 0; i < hir->items->count; ++i) {
        struct HirDecl *item = K_LIST_GET(hir->items, i);
        if (HirIsFuncDecl(item)) {
            collect_func_decl(X, HirGetFuncDecl(item));
        } else if (HirIsTypeDecl(item)) {
            collect_type_decl(X, HirGetTypeDecl(item));
        }
    }

    int index;
    struct HirImport *im;
    K_LIST_ENUMERATE(hir->imports, index, im) {
        if (im->item != NULL) {
            // handle "use mod::item;": find the item declaration in the other
            // module and add it to this module's global symbol table
            struct HirDecl *item = find_item_in(X, im->modno, im->item);
            String *name = im->as != NULL ? im->as : im->item;
            new_global(X, name, item);

            K_LIST_SET(hir->imports, index, K_LIST_LAST(hir->imports));
            K_LIST_POP(hir->imports);
            --index;
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
    struct ItemCollector X = {
        .symtab = pawHir_symtab_new(C),
        .pool = &C->dm->pool,
        .traits = C->traits,
        .P = ENV(C),
        .dm = C->dm,
        .C = C,
    };

    DLOG(&X, "collecting %d module(s)", C->modules->count);

    struct PartialModList *pml = collect_phase_1(&X, C->modules);
    collect_phase_2(&X, C->modules, pml);
}

