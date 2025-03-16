// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// collect_items.c: Definition of pawP_collect_items. Collect the type of each
//     language construct not declared within a function body. Essentially,
//     determines the declaration referenced by each path, e.g. a struct field
//     or a named function parameter. Note that all paths in an ADT definition
//     or function signature refer either to ADTs or to generics from an
//     enclosing binder, meaning only ADTs and trait objects are instantiated
//     in this module.

#include "compile.h"
#include "debug.h"
#include "gc.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "type_folder.h"
#include "unify.h"

#define MOD(X) (X)->m->hir->modno
#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))
#define GET_TYPE(X, hid) pawIr_get_type((X)->C, hid)
#define SET_TYPE(X, hid, t) pawIr_set_type((X)->C, hid, t)

#define WITH_CONTEXT(X, type, code) \
    do {                            \
        (X)->ctx = (type);          \
        code(X)->ctx = NULL;        \
    } while (0)

struct ItemCollector {
    struct Pool *pool;
    struct HirSymtab *symtab;
    struct DynamicMem *dm;
    struct Compiler *C;
    struct ModuleInfo *m;
    struct ScopeMap *scopes;
    struct IrType *ctx;
    struct Hir *hir;
    TraitMap *traits;
    paw_Env *P;
    int line;
};

DEFINE_MAP(struct ItemCollector, ScopeMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, HirScope *)

static struct HirScope *enclosing_scope(struct ItemCollector *X)
{
    paw_assert(X->symtab->count > 0);
    return HirSymtab_last(X->symtab);
}

static int add_symbol(struct ItemCollector *X, struct HirScope *scope, String *name, struct HirDecl *decl)
{
    return pawHir_declare_symbol(X->hir, scope, name, (struct HirResult){
                .kind = HIR_RESULT_DECL,
                .did = decl->hdr.did,
            });
}

static void new_global(struct ItemCollector *X, String *name, struct HirDecl *decl)
{
    struct HirSymbol *psymbol;
    struct HirScope *scope = X->m->globals;
    K_LIST_FOREACH (scope, psymbol) {
        if (pawS_eq(psymbol->name, name))
            NAME_ERROR(X, "duplicate global '%s'", name->text);
    }
    add_symbol(X, scope, name, decl);
}

static void new_local(struct ItemCollector *X, String *name, struct HirDecl *decl)
{
    add_symbol(X, enclosing_scope(X), name, decl);
}

static struct HirScope *leave_block(struct ItemCollector *X)
{
    struct HirScope *scope = enclosing_scope(X);
    HirSymtab_pop(X->symtab);
    return scope;
}

static void enter_block(struct ItemCollector *X, struct HirScope *scope)
{
    scope = scope != NULL ? scope : HirScope_new(X->hir);
    HirSymtab_push(X->hir, X->symtab, scope);
}

static struct HirScope *leave_function(struct ItemCollector *X)
{
    struct HirScope *scope = leave_block(X);
    CHECK_GC(ENV(X));
    return scope;
}

static void enter_function(struct ItemCollector *X, struct HirFuncDecl *func)
{
    enter_block(X, NULL);
    new_local(X, func->name, HIR_CAST_DECL(func));
}

static void ensure_unique(struct ItemCollector *X, StringMap *map, String *name, char const *what)
{
    if (name == NULL)
        return;
    String *const *pname = StringMap_get(X->C, map, name);
    if (pname != NULL)
        NAME_ERROR(X, "duplicate %s '%s'", what, name->text);
    StringMap_insert(X->C, map, name, NULL);
}

static struct IrType *collect_type(struct ItemCollector *X, struct HirType *type)
{
    X->line = type->hdr.line;
    struct IrType *result = pawP_lower_type(X->C, X->m, X->symtab, type);
    if (result == NULL) {
        char const *type_name = pawHir_print_type(X->C, type);
        TYPE_ERROR(X, "unrecognized type '%s'", type_name);
    }
    pawIr_validate_type(X->C, result);
    SET_NODE_TYPE(X->C, type, result);
    return result;
}

static void map_adt_to_trait(struct ItemCollector *X, struct HirDecl *adt, struct IrType *trait)
{
    struct IrTypeList *traits;
    DeclId const did = adt->hdr.did;
    struct IrTypeList **ptraits = TraitMap_get(X->C, X->traits, did);
    if (ptraits == NULL) {
        traits = IrTypeList_new(X->C);
        TraitMap_insert(X->C, X->traits, did, traits);
    } else {
        traits = *ptraits;
    }
    IrTypeList_push(X->C, traits, trait);
}

static struct IrType *collect_trait_path(struct ItemCollector *X, struct HirPath *path)
{
    struct IrType *trait = pawP_lookup_trait(X->C, X->m, X->symtab, path);
    if (trait == NULL) {
        char const *trait_name = pawHir_print_path(X->C, path);
        NAME_ERROR(X, "unknown trait '%s'", trait_name);
    }
    pawIr_validate_type(X->C, trait);
    return trait;
}

static struct IrTypeList *collect_bounds(struct ItemCollector *X, struct HirBoundList *bounds)
{
    if (bounds == NULL)
        return NULL;
    struct IrTypeList *result = IrTypeList_new(X->C);

    struct HirGenericBound *pbound;
    K_LIST_FOREACH (bounds, pbound) {
        struct IrType *type = collect_trait_path(X, pbound->path);
        IrTypeList_push(X->C, result, type);
    }
    return result;
}

static void set_def_type(struct ItemCollector *X, DeclId did, HirId hid)
{
    IrType *type = pawIr_get_type(X->C, hid);
    DefTypeMap_insert(X->C, X->C->def_types, did, type);
}

static struct IrGenericDefs *collect_generic_defs(struct ItemCollector *X, struct HirDeclList *generics)
{
    struct HirDecl **pdecl;
    if (generics == NULL) return NULL;
    struct IrGenericDefs *result = IrGenericDefs_new(X->C);
    K_LIST_FOREACH(generics, pdecl) {
        DeclId const did = (*pdecl)->hdr.did;
//        DeclId const did = pawIr_next_did(X->C, MOD(X));
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        struct IrType *type = pawIr_new_generic(X->C, did, NULL);
        struct IrGenericDef *r = pawIr_new_generic_def(X->C, did, d->name);
        IrGenericDefs_push(X->C, result, r);
        set_def_type(X, did, (*pdecl)->hdr.hid);
    }
    return result;
}

static struct IrFieldDefs *collect_field_defs(struct ItemCollector *X, struct HirDeclList *fields)
{
    struct HirDecl **pdecl;
    struct IrFieldDefs *result = IrFieldDefs_new(X->C);
    if (fields == NULL)
        return result;
    K_LIST_FOREACH(fields, pdecl) {
        DeclId const did = (*pdecl)->hdr.did;
        struct HirFieldDecl *d = HirGetFieldDecl(*pdecl);
        struct IrType *type = collect_type(X, d->tag);
//        DeclId const did = pawIr_next_did(X->C, MOD(X));
        struct IrFieldDef *def = pawIr_new_field_def(X->C, did, d->name, d->is_pub);
        IrFieldDefs_push(X->C, result, def);
        set_def_type(X, did, d->hid);
    }
    return result;
}

static struct IrParams *collect_parameters(struct ItemCollector *X, struct HirDeclList *params)
{
    struct HirDecl **pdecl;
    struct IrParams *result = IrParams_new(X->C);
    K_LIST_FOREACH(params, pdecl) {
        struct HirFieldDecl *d = HirGetFieldDecl(*pdecl);
        struct IrType *type = collect_type(X, d->tag);
        IrParams_push(X->C, result, (struct IrParam){
                    .name = d->name,
                    .type = type,
                });
    }
    return result;
}

static void transfer_fn_annotations(struct ItemCollector *X, struct HirFuncDecl *d, struct IrFnDef *def)
{
    struct Compiler *C = X->C;
    struct Annotations *annos = d->annos;
    def->annos = Annotations_new(C);

    if (annos != NULL) {
        struct Annotation *panno;
        K_LIST_FOREACH (annos, panno) {
            if (pawS_eq(panno->name, CSTR(C, CSTR_EXTERN))) {
                // Found "extern" annotation. Implementation of function will be found in
                // "paw.symbols" map during code generation.
                if (d->body != NULL)
                    VALUE_ERROR(C, d->line, "unexpected body for extern function '%s'",
                                d->name->text);
                def->is_extern = PAW_TRUE;
            } else {
                Annotations_push(C, def->annos, *panno);
            }
        }
    }

    // non-defaulted trait methods have no bodies
    paw_Bool const in_trait = X->ctx != NULL && IrIsTraitObj(X->ctx);

    if (d->body == NULL && !in_trait && !def->is_extern)
        VALUE_ERROR(C, d->line, "missing body for function '%s'", d->name->text);
}

static struct IrVariantDefs *create_struct_variant(struct ItemCollector *X, String *name, struct HirDeclList *decls, DeclId did)
{
    struct IrFieldDefs *fields = collect_field_defs(X, decls);
    struct IrVariantDef *r = pawIr_new_variant_def(X->C, did, NO_DECL, 0, name, fields);
    struct IrVariantDefs *variants = IrVariantDefs_new(X->C);
    VariantDefMap_insert(X->C, X->C->variant_defs, r->did, r);
    IrVariantDefs_push(X->C, variants, r);
    return variants;
}

static struct IrVariantDefs *collect_variant_defs(struct ItemCollector *X, struct HirAdtDecl *adt, DeclId base_did)
{
    if (adt->is_struct) return create_struct_variant(X, adt->name, adt->fields, base_did);
    struct IrVariantDefs *variants = IrVariantDefs_new(X->C);

    struct HirDecl **pdecl;
    K_LIST_FOREACH(adt->fields, pdecl) {
        DeclId const did = (*pdecl)->hdr.did;
//        const DeclId did = pawIr_next_did(X->C, MOD(X));
        struct HirVariantDecl *d = HirGetVariantDecl(*pdecl);
        struct IrFieldDefs *fields = collect_field_defs(X, d->fields);
        struct IrVariantDef *r = pawIr_new_variant_def(X->C, did, NO_DECL, d->index, d->name, fields);
        VariantDefMap_insert(X->C, X->C->variant_defs, r->did, r);
        IrVariantDefs_push(X->C, variants, r);
        set_def_type(X, did, d->hid);
    }
    return variants;
}

static void collect_field_decl(struct ItemCollector *X, struct HirFieldDecl *d)
{
    SET_TYPE(X, d->hid, collect_type(X, d->tag));
}

static void collect_variant_type(struct ItemCollector *, struct HirVariantDecl *);

static void collect_field_types(struct ItemCollector *X, struct HirDeclList *fields, StringMap *names)
{
    if (fields == NULL)
        return;
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH(fields, pdecl) {
        if (HirIsFieldDecl(*pdecl)) {
            struct HirFieldDecl *d = HirGetFieldDecl(*pdecl);
            ensure_unique(X, names, d->name, "struct field");
            collect_field_decl(X, d);
        } else {
            struct HirVariantDecl *d = HirGetVariantDecl(*pdecl);
            ensure_unique(X, names, d->name, "enum variant");
            collect_variant_type(X, d);
        }
    }
}

static void collect_variant_type(struct ItemCollector *X, struct HirVariantDecl *d)
{
    collect_field_types(X, d->fields, NULL);

    // An enum variant name can be thought of as a function from the type of the
    // variant's fields to the type of the enumeration. For example, given 'enum
    // E {X(str)}', E::X has type 'fn(str) -> E'.
    struct IrTypeList *params = d->fields != NULL
                                    ? pawHir_collect_decl_types(X->C, d->fields)
                                    : IrTypeList_new(X->C);
    struct IrType *type = pawIr_new_signature(X->C, d->did, NULL, params, X->ctx);
    SET_TYPE(X, d->hid, type);

//TODO    struct IrVariantDef *def = pawIr_get_variant_def(X->C, )
//TODO    DeclId const cons = IR_TYPE_DID(type);
}

static paw_Bool check_assoc_function(struct ItemCollector *X, struct IrType *self, struct HirDeclList *params)
{
    paw_assert(self != NULL);
    if (params->count == 0)
        return PAW_TRUE;
    struct HirDecl *first = HirDeclList_get(params, 0);
    if (pawS_eq(first->hdr.name, CSTR(X, CSTR_SELF))) {
        // parameter is named "self": make sure the type is compatible with "Self" alias
        struct IrType *type = GET_NODE_TYPE(X->C, first);
        pawU_unify(X->C->U, type, self);
        return PAW_FALSE;
    }
    return PAW_TRUE;
}

static void maybe_store_builtin(struct ItemCollector *X, String *name, DeclId did)
{
    struct Builtin *const *pb = BuiltinMap_get(X->C, X->C->builtin_lookup, name);
    if (pb != NULL) {
        paw_assert(X->hir->modno == 0);
        (*pb)->did = did;
    }
}

static struct HirDecl *declare_self(struct ItemCollector *X, int line, struct IrType *type)
{
    String *name = SCAN_STRING(X->C, "Self");
    struct HirDecl *self = pawHir_new_type_decl(X->hir, line, name, NULL, NULL, PAW_FALSE);
    SET_TYPE(X, self->hdr.hid, type);
    new_local(X, name, self);
    return self;
}

static struct ModuleInfo *use_module(struct ItemCollector *X, struct ModuleInfo *m)
{
    pawU_enter_binder(X->C->U);
    X->hir = m->hir;
    X->m = m;
    return m;
}

static void finish_module(struct ItemCollector *X)
{
    pawU_leave_binder(X->C->U);
    X->hir = NULL;
    X->m = NULL;
}

static struct HirDecl *find_item_in(struct ItemCollector *X, struct ModuleInfo *m, String *name)
{
    struct HirDecl **pitem;
    struct HirDeclList *items = m->hir->items;
    K_LIST_FOREACH (items, pitem) {
        String const *item_name = (*pitem)->hdr.name;
        if (pawS_eq(item_name, name))
            return *pitem;
    }
    return NULL;
}

static IrTypeList *collect_generic_types(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics == NULL)
        return NULL;
    struct IrTypeList *types = IrTypeList_new(X->C);

    // Create a local symbol for each generic. Generic bounds are registered
    // in a later pass, once all nominal types are known.
    struct HirDecl **pdecl;
    K_LIST_FOREACH (generics, pdecl) {
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        struct IrType *type = pawIr_new_generic(X->C, d->did, NULL);
        SET_NODE_TYPE(X->C, *pdecl, type);

        new_local(X, d->name, *pdecl);
        IrTypeList_push(X->C, types, type);
    }
    return types;
}

static DeclId collect_adt(struct ItemCollector *X, struct HirAdtDecl *d, struct HirScope *scope)
{
    enter_block(X, scope);

    DeclId const did = d->did;
//    DeclId const did = pawIr_next_did(X->C, MOD(X));
    IrTypeList *types = collect_generic_types(X, d->generics);
    IrType *type = pawIr_new_adt(X->C, did, types);
    SET_TYPE(X, d->hid, type);

    leave_block(X);
    return did;
}

static DeclId collect_trait_type(struct ItemCollector *X, struct HirTraitDecl *d, struct HirScope *scope)
{
    enter_block(X, scope);

    DeclId const did = d->did;
    IrTypeList *generics = collect_generic_types(X, d->generics);
    struct IrType *type = pawIr_new_trait_obj(X->C, did, generics);
    SET_TYPE(X, d->hid, type);

    leave_block(X);
    return did;
}

static void collect_import(struct ItemCollector *X, struct HirImport im)
{
    // handle "use mod::item;": find the item declaration in the other
    // module and add it to this module's global symbol table
    struct ModuleInfo *m = ModuleList_get(X->C->modules, im.modno);
    struct HirDecl *item = find_item_in(X, m, im.item);
    if (item == NULL)
        NAME_ERROR(X, "unrecognized item '%s::%s'", m->name->text, im.item->text);

    if (!pawHir_is_pub_decl(item))
        VALUE_ERROR(X, -1, "item '%s::%s' cannot be accessed from the current module",
                m->name->text, im.item->text);

    String *name = im.as != NULL ? im.as : im.item;
    new_global(X, name, item);
}

static void collect_nominal_types(struct ItemCollector *X, struct Hir *hir)
{
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH(hir->items, pdecl) {
        DeclId did;
        struct HirDecl *decl = *pdecl;
        HirScope *scope = HirScope_new_from(hir, X->pool);
        if (HirIsAdtDecl(decl)) {
            did = collect_adt(X, HirGetAdtDecl(decl), scope);
        } else if (HirIsTraitDecl(decl)) {
            did = collect_trait_type(X, HirGetTraitDecl(decl), scope);
        } else {
            continue;
        }
        ScopeMap_insert(X, X->scopes, did, scope);
        maybe_store_builtin(X, decl->hdr.name, did);
        new_global(X, decl->hdr.name, decl);
    }

    // register imports early, since function signatures, fields, etc., may
    // reference nominal types from other modules
    int index;
    struct HirImport *pim;
    K_LIST_ENUMERATE (hir->imports, index, pim) {
        if (pim->item != NULL) {
            collect_import(X, *pim);
            HirImportList_swap_remove(hir->imports, index);
            --index;
        }
    }
}

static void collection_phase_1(struct ItemCollector *X, struct ModuleList *mods)
{
    struct ModuleInfo *const *pmi;
    K_LIST_FOREACH (mods, pmi) {
        struct ModuleInfo *m = use_module(X, *pmi);
        collect_nominal_types(X, m->hir);
        finish_module(X);
    }
}

static void collect_generic_bounds(struct ItemCollector *X, struct HirDeclList *generics, IrType *adt)
{
    if (generics == NULL) return;

    struct IrType **ptype;
    struct HirDecl **pdecl;
    K_LIST_ZIP (generics, pdecl, IR_TYPE_SUBTYPES(adt), ptype) {
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        struct IrGeneric *g = IrGetGeneric(*ptype);
        g->bounds = collect_bounds(X, d->bounds);
    }
}

static void collect_func_decl(struct ItemCollector *X, struct HirFuncDecl *d)
{
    DeclId const did = d->did;
//     const DeclId did = pawIr_next_did(X->C, MOD(X));
    enter_function(X, d);

    struct IrTypeList *types = collect_generic_types(X, d->generics);
    StringMap *names = StringMap_new_from(X->C, X->pool);
    collect_field_types(X, d->params, names);
    StringMap_delete(X->C, names);

    struct IrTypeList *params_ = pawHir_collect_decl_types(X->C, d->params);
    struct IrType *result = collect_type(X, d->result);
    struct IrType *sig = pawIr_new_signature(X->C, d->did, types, params_, result);
    collect_generic_bounds(X, d->generics, sig);
    SET_TYPE(X, d->hid, sig);

    struct IrGenericDefs *generics = collect_generic_defs(X, d->generics);
    struct IrParams *params = collect_parameters(X, d->params);
    struct IrFnDef *r = pawIr_new_fn_def(X->C, did, d->name, generics, params, d->is_pub);
    FnDefMap_insert(X->C, X->C->fn_defs, did, r);
    transfer_fn_annotations(X, d, r);
    set_def_type(X, did, d->hid);

    leave_function(X);

    if (X->ctx != NULL) {
        d->is_assoc = check_assoc_function(X, X->ctx, d->params);
        struct IrType *type = GET_TYPE(X, d->hid);
        IrGetSignature(type)->self = X->ctx;
    } else {
        new_global(X, d->name, HIR_CAST_DECL(d));
    }
}

static void collect_method_decls(struct ItemCollector *X, struct HirDeclList *methods, StringMap *names, paw_Bool force_pub)
{
    struct HirDecl **pdecl;
    K_LIST_FOREACH (methods, pdecl) {
        struct HirFuncDecl *d = HirGetFuncDecl(*pdecl);
        ensure_unique(X, names, d->name, "method");
        if (force_pub)
            d->is_pub = PAW_TRUE;
        collect_func_decl(X, d);
    }
}

static void collect_adt_decl(struct ItemCollector *X, struct HirAdtDecl *d)
{
    DeclId const did = d->did; // TODO

    HirScope *scope = *ScopeMap_get(X, X->scopes, d->did);
    enter_block(X, scope);

    struct IrType *type = GET_TYPE(X, d->hid);
    collect_generic_bounds(X, d->generics, type);

    struct IrGenericDefs *generics = collect_generic_defs(X, d->generics);
    struct IrVariantDefs *variants = collect_variant_defs(X, d, did);
    struct IrAdtDef *r = pawIr_new_adt_def(X->C, did, d->name, generics, variants, d->is_pub, d->is_struct);
    AdtDefMap_insert(X->C, X->C->adt_defs, did, r);
    set_def_type(X, did, d->hid);

    struct HirType **ptype;
    K_LIST_FOREACH (d->traits, ptype) {
        struct HirPathType *path = HirGetPathType(*ptype);
        struct IrType *trait = collect_trait_path(X, path->path);
        pawP_add_trait_impl(X->C, type, trait);
        SET_NODE_TYPE(X->C, *ptype, trait);
    }

    WITH_CONTEXT(X, type,
                 StringMap *names = StringMap_new_from(X->C, X->pool);
                 d->self = declare_self(X, d->line, type);
                 collect_field_types(X, d->fields, names);
                 collect_method_decls(X, d->methods, names, PAW_FALSE);
                 StringMap_delete(X->C, names););
    pawP_validate_adt_traits(X->C, d);

    leave_block(X);
}

static void collect_trait_decl(struct ItemCollector *X, struct HirTraitDecl *d)
{
    DeclId const did = d->did; // TODO

    HirScope *scope = *ScopeMap_get(X, X->scopes, did);
    enter_block(X, scope);

    struct IrType *type = GET_TYPE(X, d->hid);
    collect_generic_bounds(X, d->generics, type);

    WITH_CONTEXT(X, type,
                 StringMap *names = StringMap_new_from(X->C, X->pool);
                 d->self = declare_self(X, d->line, type);
                 collect_method_decls(X, d->methods, names, d->is_pub);
                 StringMap_delete(X->C, names););

    leave_block(X);
}

static void collect_type_decl(struct ItemCollector *X, struct HirTypeDecl *d)
{
    new_global(X, d->name, HIR_CAST_DECL(d));
    enter_block(X, NULL);

    collect_generic_types(X, d->generics);
    struct IrType *type = collect_type(X, d->rhs);
    SET_TYPE(X, d->hid, type);

    leave_block(X);
}

static void collect_const_decl(struct ItemCollector *X, struct HirConstDecl *d)
{
    SET_TYPE(X, d->hid, collect_type(X, d->tag));
    new_global(X, d->name, HIR_CAST_DECL(d));
}

static void collect_other_types(struct ItemCollector *X, struct Hir *hir)
{
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH(hir->items, pdecl) {
        switch (HIR_KINDOF(*pdecl)) {
            case kHirAdtDecl:
                collect_adt_decl(X, HirGetAdtDecl(*pdecl));
                break;
            case kHirTraitDecl:
                collect_trait_decl(X, HirGetTraitDecl(*pdecl));
                break;
            case kHirFuncDecl:
                collect_func_decl(X, HirGetFuncDecl(*pdecl));
                break;
            case kHirTypeDecl:
                collect_type_decl(X, HirGetTypeDecl(*pdecl));
                break;
            case kHirConstDecl:
                collect_const_decl(X, HirGetConstDecl(*pdecl));
                break;
            default:
                PAW_UNREACHABLE();
        }
    }
}

static void collection_phase_2(struct ItemCollector *X, struct ModuleList *mods)
{
    struct ModuleInfo *const *pmi;
    K_LIST_FOREACH (mods, pmi) {
        struct ModuleInfo *m = use_module(X, *pmi);
        collect_other_types(X, m->hir);
        finish_module(X);
    }
}

// Entrypoint to item collection
void pawP_collect_items(struct Compiler *C, struct Pool *pool)
{
    struct ItemCollector X = {
        .pool = pool,
        .traits = C->traits,
        .P = ENV(C),
        .dm = C->dm,
        .C = C,
    };
    X.symtab = HirSymtab_new(C->hir_prelude);
    X.scopes = ScopeMap_new(&X);

    DLOG(&X, "collecting %d module(s)", C->modules->count);

    collection_phase_1(&X, C->modules);
    collection_phase_2(&X, C->modules);
}
