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
#include "error.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "resolve.h"
#include "type_folder.h"
#include "unify.h"

#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))
#define GET_TYPE(X, id) pawIr_get_type((X)->C, id)
#define SET_TYPE(X, id, t) pawIr_set_type((X)->C, id, t)

#define COLLECTOR_ERROR(X_, Kind_, ...) pawErr_##Kind_((X_)->C, (X_)->pm->name, __VA_ARGS__)

#define WITH_CONTEXT(X, type, code) \
    do {                            \
        (X)->ctx = (type);          \
        code(X)->ctx = NULL;        \
    } while (0)

struct ItemCollector {
    struct HirModule const *pm;
    struct Compiler *C;
    struct Pool *pool;
    IrType *ctx;
    struct Hir *hir;
    paw_Env *P;
};

static enum BuiltinKind builtin_kind(struct ItemCollector *X, IrType *type)
{
    return pawP_type2code(X->C, type);
}

static IrType *collect_type(struct ItemCollector *X, struct HirType *type)
{
    IrType *result = pawP_lower_type(X->C, *X->pm, type);
    pawIr_validate_type(X->C, result);
    return result;
}

static IrTypeList *collect_types(struct ItemCollector *X, struct HirTypeList *types)
{
    if (types == NULL) return NULL;
    IrTypeList *result = IrTypeList_new(X->C);

    struct HirType *const *ptype;
    K_LIST_FOREACH (types, ptype) {
        IrType *type = collect_type(X, *ptype);
        IrTypeList_push(X->C, result, type);
    }
    return result;
}

static IrType *collect_type_path(struct ItemCollector *X, struct HirPath path)
{
    paw_assert(path.kind == HIR_PATH_ITEM);
    paw_assert(path.segments->count == 1);
    struct HirSegment const last = K_LIST_LAST(path.segments);
    IrType *type = GET_TYPE(X, last.target);
    if (last.types == NULL) return type;

    IrTypeList *args = collect_types(X, last.types);
    return pawP_instantiate(X->C, type, args);
}

static IrType *collect_trait_path(struct ItemCollector *X, struct HirPath path)
{
    IrType *trait = collect_type_path(X, path);
    if (!IrIsTraitObj(trait)) {
        char const *repr = pawHir_print_path(X->C, &path);
        COLLECTOR_ERROR(X, expected_trait, path.span.start, repr);
    }
    pawIr_validate_type(X->C, trait);
    return trait;
}

static IrTypeList *collect_bounds(struct ItemCollector *X, struct HirBoundList *bounds)
{
    if (bounds == NULL) return NULL;
    struct HirGenericBound *pbound;
    IrTypeList *result = IrTypeList_new(X->C);
    K_LIST_FOREACH (bounds, pbound) {
        IrType *type = collect_trait_path(X, pbound->path);
        IrTypeList_push(X->C, result, type);
    }
    return result;
}

static void set_def_type(struct ItemCollector *X, DeclId did, IrType *type)
{
    DefTypeMap_insert(X->C, X->C->def_types, did, type);
}

static struct IrGenericDefs *collect_generic_defs(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics == NULL) return NULL;
    struct HirDecl *const *pdecl;
    struct IrGenericDefs *result = IrGenericDefs_new(X->C);
    K_LIST_FOREACH (generics, pdecl) {
        DeclId const did = (*pdecl)->hdr.did;
        struct HirGenericDecl const *d = HirGetGenericDecl(*pdecl);
        IrType *type = pawIr_get_type(X->C, d->id);
        struct IrGenericDef *r = pawIr_new_generic_def(X->C, did,
                d->ident.name, IrGetGeneric(type)->bounds);
        IrGenericDefs_push(X->C, result, r);
        set_def_type(X, did, type);
    }
    return result;
}

static struct IrFieldDefs *collect_field_defs(struct ItemCollector *X, struct HirDeclList *fields)
{
    struct HirDecl *const *pdecl;
    struct IrFieldDefs *result = IrFieldDefs_new(X->C);
    if (fields == NULL) return result;

    K_LIST_FOREACH (fields, pdecl) {
        struct HirFieldDecl const *d = HirGetFieldDecl(*pdecl);
        IrType *type = collect_type(X, d->tag);
        SET_TYPE(X, d->id, type);

        struct IrFieldDef *def = pawIr_new_field_def(X->C, d->did, d->ident.name, d->is_pub);
        IrFieldDefs_push(X->C, result, def);
        set_def_type(X, d->did, type);
    }
    return result;
}

static struct IrParams *collect_parameters(struct ItemCollector *X, struct HirDeclList *params)
{
    struct HirDecl *const *pdecl;
    struct IrParams *result = IrParams_new(X->C);
    K_LIST_FOREACH (params, pdecl) {
        struct HirParamDecl const *d = HirGetParamDecl(*pdecl);
        IrType *type = collect_type(X, d->tag);
        IrParams_push(X->C, result, (struct IrParam){
                    .name = d->ident.name,
                    .type = type,
                });
    }
    return result;
}

static void transfer_fn_annotations(struct ItemCollector *X, struct HirFnDecl *d, struct IrFnDef *def)
{
    struct Compiler *C = X->C;
    struct Annotations *annos = d->annos;
    def->annos = Annotations_new(C);

    if (annos != NULL) {
        struct Annotation *panno;
        K_LIST_FOREACH (annos, panno) {
            if (pawS_eq(panno->name, CSTR(C, CSTR_EXTERN))
                    || pawS_eq(panno->name, SCAN_STR(C, "extern_std"))) {
                // Found "extern" annotation. Implementation of function will be found in
                // "paw.symbols" map during code generation.
                if (d->body != NULL)
                    COLLECTOR_ERROR(X, extern_function_body, d->span.start, d->ident.name->text);
                def->is_extern = PAW_TRUE;
            } else {
                Annotations_push(C, def->annos, *panno);
            }
        }
    }

    // ensure that trait methods have no bodies
    paw_Bool const in_trait = X->ctx != NULL && IrIsTraitObj(X->ctx);
    if (d->body == NULL && !in_trait && !def->is_extern)
        COLLECTOR_ERROR(X, missing_function_body, d->span.start, d->ident.name->text);
}

static struct IrVariantDefs *create_struct_variant(struct ItemCollector *X, struct HirIdent ident, struct HirDeclList *decls)
{
    paw_assert(decls->count == 1);
    struct HirVariantDecl *v = HirGetVariantDecl(K_LIST_FIRST(decls));
    struct IrFieldDefs *fields = collect_field_defs(X, v->fields);
    struct IrVariantDef *r = pawIr_new_variant_def(X->C, v->did, NO_DECL, v->base_did, 0, ident.name, fields);
    struct IrVariantDefs *variants = IrVariantDefs_new(X->C);
    VariantDefMap_insert(X->C, X->C->variant_defs, v->did, r);
    IrVariantDefs_push(X->C, variants, r);
    return variants;
}

static struct IrVariantDefs *collect_variant_defs(struct ItemCollector *X, struct HirAdtDecl *adt)
{
    if (adt->is_struct) return create_struct_variant(X, adt->ident, adt->variants);
    struct IrVariantDefs *variants = IrVariantDefs_new(X->C);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (adt->variants, pdecl) {
        struct HirVariantDecl *d = HirGetVariantDecl(*pdecl);
        struct IrFieldDefs *fields = collect_field_defs(X, d->fields);
        struct IrVariantDef *r = pawIr_new_variant_def(X->C, d->did, NO_DECL, d->base_did, d->index, d->ident.name, fields);
        VariantDefMap_insert(X->C, X->C->variant_defs, d->did, r);
        IrVariantDefs_push(X->C, variants, r);

        IrType *type = pawIr_get_type(X->C, d->id);
        set_def_type(X, d->did, type);
    }
    return variants;
}

static void ensure_unique(struct ItemCollector *X, StringMap *map, struct HirIdent ident, char const *what)
{
    void *const *pname = StringMap_get(X->C, map, ident.name);
    if (pname != NULL)
        COLLECTOR_ERROR(X, duplicate_item, ident.span.start, what, ident.name->text);
    StringMap_insert(X->C, map, ident.name, NULL);
}

static paw_Bool is_self_param(struct ItemCollector *X, struct HirDecl *decl)
{
    struct HirParamDecl *param = HirGetParamDecl(decl);
    return pawS_eq(param->ident.name, CSTR(X, CSTR_SELF));
}

static void collect_param_types(struct ItemCollector *X, struct HirDeclList *params)
{
    if (params == NULL) return;
    StringMap *names = StringMap_new_from(X->C, X->pool);
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (params, pdecl) {
        struct HirParamDecl *d = HirGetParamDecl(*pdecl);
        ensure_unique(X, names, d->ident, "function parameter");
        IrType *type = collect_type(X, d->tag);
        SET_TYPE(X, d->id, type);
    }
    StringMap_delete(X->C, names);
}

static void collect_field_types(struct ItemCollector *X, struct HirDeclList *fields, paw_Bool is_struct)
{
    if (fields == NULL) return;
    StringMap *names = StringMap_new_from(X->C, X->pool);
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (fields, pdecl) {
        struct HirFieldDecl *d = HirGetFieldDecl(*pdecl);
        if (is_struct) ensure_unique(X, names, d->ident, "struct field");
        SET_TYPE(X, d->id, collect_type(X, d->tag));
    }
    StringMap_delete(X->C, names);
}

static void collect_variant_type(struct ItemCollector *X, struct HirVariantDecl *d, paw_Bool is_struct)
{
    struct IrVariantDef *def = pawIr_get_variant_def(X->C, d->did);
    collect_field_types(X, d->fields, is_struct);

    if (is_struct) {
        SET_TYPE(X, d->id, X->ctx);
        def->cons_did = NO_DECL;
    } else {
        // An enum variant name can be thought of as a function from the type of the
        // variant's fields to the type of the enumeration. For example, given 'enum
        // E {X(str)}', E::X(str) has type 'fn(str) -> E'.
        IrTypeList *params = d->fields != NULL
                                        ? pawHir_collect_decl_types(X->C, d->fields)
                                        : IrTypeList_new(X->C);
        IrType *type = pawIr_new_signature(X->C, d->did, NULL, params, X->ctx);
        IrGetSignature(type)->self = X->ctx;
        SET_TYPE(X, d->id, type);
        def->cons_did = IR_TYPE_DID(type);
    }
}

static void collect_variant_types(struct ItemCollector *X, struct HirDeclList *variants, paw_Bool is_struct)
{
    StringMap *names = StringMap_new_from(X->C, X->pool);
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (variants, pdecl) {
        // NOTE: uniqueness of variant names already checked
        struct HirVariantDecl *d = HirGetVariantDecl(*pdecl);
        collect_variant_type(X, d, is_struct);
    }
    StringMap_delete(X->C, names);
}

static void start_module(struct ItemCollector *X, struct HirModule const *pm)
{
    pawU_enter_binder(X->C->U, pm->name);
    X->pm = pm;
}

static void finish_module(struct ItemCollector *X)
{
    pawU_leave_binder(X->C->U);
    X->pm = NULL;
}

static IrTypeList *collect_generic_types(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics == NULL) return NULL;
    IrTypeList *types = IrTypeList_new(X->C);

    // Create a local symbol for each generic. Generic bounds are registered
    // in a later pass, once all nominal types are known.
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (generics, pdecl) {
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        IrType *type = pawIr_new_generic(X->C, d->did, NULL);
        SET_NODE_TYPE(X->C, *pdecl, type);
        IrTypeList_push(X->C, types, type);
    }
    return types;
}

static void collect_adt_type(struct ItemCollector *X, struct HirAdtDecl *d)
{
    IrTypeList *types = collect_generic_types(X, d->generics);
    IrType *type = pawIr_new_adt(X->C, d->did, types);
    SET_TYPE(X, d->id, type);

    struct IrAdtDef *r = pawIr_new_adt_def(X->C, d->did, d->ident.name,
            NULL, NULL, d->is_pub, d->is_struct, d->is_inline);
    AdtDefMap_insert(X->C, X->C->adt_defs, d->did, r);
    set_def_type(X, d->did, type);
}

static void collect_trait_type(struct ItemCollector *X, struct HirTraitDecl *d)
{
    IrTypeList *generics = collect_generic_types(X, d->generics);
    IrType *type = pawIr_new_trait_obj(X->C, d->did, generics);
    set_def_type(X, d->did, type);
    SET_TYPE(X, d->id, type);
}

static void collect_nominal_types(struct ItemCollector *X, struct HirModule m)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (m.items, pitem) {
        struct HirDecl *item = *pitem;
        if (HirIsAdtDecl(item)) {
            collect_adt_type(X, HirGetAdtDecl(item));
        } else if (HirIsTraitDecl(item)) {
            collect_trait_type(X, HirGetTraitDecl(item));
        }
    }
}

static void collect_type_decl(struct ItemCollector *X, struct HirTypeDecl *d)
{
    collect_generic_types(X, d->generics);
    IrType *type = collect_type(X, d->rhs);
    SET_TYPE(X, d->id, type);
}

static void collect_type_aliases(struct ItemCollector *X, struct HirModule m)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (m.items, pitem) {
        struct HirDecl *item = *pitem;
        if (HirIsTypeDecl(item))
            collect_type_decl(X, HirGetTypeDecl(item));
    }
}

static void collect_local_type_decl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    collect_type_decl(V->ud, d);
}

static void collect_local_type_aliases(struct ItemCollector *X, struct HirExpr *block)
{
    if (block != NULL) {
        struct HirVisitor *V = &(struct HirVisitor){0};
        pawHir_visitor_init(V, X->hir, X);
        V->PostVisitTypeDecl = collect_local_type_decl;
        pawHir_visit_expr(V, block);
    }
}

static void collection_phase_1(struct ItemCollector *X, struct Hir *hir)
{
    struct HirModule const *pm;
    K_LIST_FOREACH (hir->modules, pm) {
        start_module(X, pm);
        collect_nominal_types(X, *pm);
        finish_module(X);
    }

    K_LIST_FOREACH (hir->modules, pm) {
        start_module(X, pm);
        collect_type_aliases(X, *pm);
        finish_module(X);
    }
}

static void collect_generic_bounds(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics != NULL) {
        struct HirDecl *const *pdecl;
        K_LIST_FOREACH (generics, pdecl) {
            struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
            struct IrGeneric *g = IrGetGeneric(GET_TYPE(X, d->id));
            g->bounds = collect_bounds(X, d->bounds);
        }
    }
}


static void unify_with_self(struct ItemCollector *X, struct SourceLoc loc, IrType *self)
{
    // value type "self" is passed by reference
    if (IrIsPtr(self)) self = IrGetPtr(self)->pointee;
    if (pawU_unify(X->C->U, self, X->ctx) != 0) {
        char const *lhs = pawIr_print_type(X->C, self);
        char const *rhs = pawIr_print_type(X->C, X->ctx);
        COLLECTOR_ERROR(X, incompatible_types, loc, lhs, rhs);
    }
}

// Ensure that "main" has a signature that can be called by the
// generated driver function
static void validate_main_signature(struct ItemCollector *X, struct SourceLoc loc, IrType *type)
{
    struct IrSignature *sig = IrGetSignature(type);
    if (sig->params->count > 1)
        pawErr_exceeded_limit(ENV(X), X->pm->name, loc,
                "parameters for \"main\" function", 1);

    if (sig->params->count == 1) {
        IrType *args = IrTypeList_first(sig->params);
        if (builtin_kind(X, args) != BUILTIN_LIST
                && builtin_kind(X, ir_list_elem(args)) != BUILTIN_INT)
            pawErr_generic_error(ENV(X), X->pm->name, loc,
                    "single argument to \"main\" must have type \"[str]\"");
    }

    if (builtin_kind(X, sig->result) != BUILTIN_UNIT
            && builtin_kind(X, sig->result) != BUILTIN_INT)
        pawErr_generic_error(ENV(X), X->pm->name, loc,
                "return type of \"main\" must have type \"()\" or \"int\"");
}

static void collect_fn_decl(struct ItemCollector *X, struct HirFnDecl *d)
{
    IrTypeList *generics = collect_generic_types(X, d->generics);
    collect_local_type_aliases(X, d->body);
    collect_param_types(X, d->params);

    IrTypeList *params = pawHir_collect_decl_types(X->C, d->params);
    IrType *result = collect_type(X, d->result);
    IrType *type = pawIr_new_signature(X->C, d->did, generics, params, result);
    collect_generic_bounds(X, d->generics);
    SET_TYPE(X, d->id, type);

    {
        struct IrGenericDefs *generics = collect_generic_defs(X, d->generics);
        struct IrParams *params = collect_parameters(X, d->params);
        struct IrFnDef *r = pawIr_new_fn_def(X->C, d->did, d->ident.name,
                generics, params, X->ctx, d->is_pub);
        FnDefMap_insert(X->C, X->C->fn_defs, d->did, r);
        transfer_fn_annotations(X, d, r);
        set_def_type(X, d->did, type);
    }

    if (X->ctx != NULL) {
        if (d->params->count > 0) {
            struct HirDecl *first = K_LIST_FIRST(d->params);
            if (is_self_param(X, first)) { // make sure "self: Self" is true
                unify_with_self(X, first->hdr.span.start, K_LIST_FIRST(params));
                HirGetParamDecl(first)->is_self = PAW_TRUE;
            }
        }
        IrGetSignature(type)->self = X->ctx;
    } else if (pawS_eq(d->ident.name, X->C->main_name)) {
        validate_main_signature(X, d->span.start, type);
    }

}

static void collect_method_decls(struct ItemCollector *X, DeclId parent_id, struct HirDeclList *methods, paw_Bool force_pub)
{
    struct HirDecl *const *pdecl;
    StringMap *names = StringMap_new_from(X->C, X->pool);
    K_LIST_FOREACH (methods, pdecl) {
        struct HirFnDecl *d = HirGetFnDecl(*pdecl);
        ensure_unique(X, names, d->ident, "method");
        d->parent_id = parent_id;
        d->is_pub |= force_pub;
        collect_fn_decl(X, d);
    }
    StringMap_delete(X->C, names);
}

static void collect_adt_decl(struct ItemCollector *X, struct HirAdtDecl *d)
{
    collect_generic_bounds(X, d->generics);

    struct IrAdtDef *r = pawIr_get_adt_def(X->C, d->did);
    r->generics = collect_generic_defs(X, d->generics);
    r->variants = collect_variant_defs(X, d);

    WITH_CONTEXT(X, GET_TYPE(X, d->id),
        collect_variant_types(X, d->variants, d->is_struct);
    );
}

static void record_impl_block(struct ItemCollector *X, IrImplOwners *owners, DeclId key, struct IrImpl *value)
{
    IrImplList *impls;
    IrImplList *const *p = IrImplOwners_get(X->C, owners, key);
    if (p == NULL) {
        impls = IrImplList_new(X->C);
        IrImplOwners_insert(X->C, owners, key, impls);
    } else {
        impls = *p;
    }
    IrImplList_push(X->C, impls, value);
}

static void collect_impl_decl(struct ItemCollector *X, struct HirImplDecl *d)
{
    collect_generic_types(X, d->generics);
    collect_generic_bounds(X, d->generics);
    IrGenericDefs *generics = collect_generic_defs(X, d->generics);

    paw_Bool force_pub = PAW_FALSE;

    IrType *trait = NULL;
    if (d->trait != NULL) {
        trait = collect_type(X, d->trait);
        if (!IrIsTraitObj(trait))
            pawErr_generic_error(ENV(X), X->pm->name, d->trait->hdr.span.start, "expected trait");
        // propagate visibility qualifier from trait
        struct HirDecl *decl = pawHir_get_decl(X->hir, IR_TYPE_DID(trait));
        force_pub = HirGetTraitDecl(decl)->is_pub;
    }
    IrType *type = collect_type(X, d->type);
    if (!IrIsAdt(type) && !IrIsGeneric(type))
        pawErr_generic_error(ENV(X), X->pm->name, d->type->hdr.span.start, "expected nominal type or generic");
    if (trait == NULL && !IrIsAdt(type))
        pawErr_generic_error(ENV(X), X->pm->name, d->type->hdr.span.start, "expected nominal type for inherent implementation");
    SET_TYPE(X, d->id, type);

    WITH_CONTEXT(X, type,
        collect_method_decls(X, d->did, d->methods, force_pub);
    );
    struct HirDecl *const *pdecl;
    IrTypeList *methods = IrTypeList_new(X->C);
    K_LIST_FOREACH (d->methods, pdecl) {
        struct HirFnDecl *method = HirGetFnDecl(*pdecl);
        IrType *type = pawIr_get_def_type(X->C, method->did);
        IrTypeList_push(X->C, methods, type);
    }

    struct IrImpl *impl = pawIr_new_impl(X->C, d->did, type, trait, generics, methods);
    ImplMap_insert(X->C, X->C->impl_defs, d->did, impl);
    if (IrIsGeneric(type)) {
        if (trait == NULL)
            pawErr_generic_error(ENV(X), X->pm->name, d->span.start,
                    "generic type cannot by target of inherent impl");
        // found blanket trait implementation
        IrImplList_push(X->C, X->C->impls.blanket, impl);
    } else {
        // found inherent or trait implementation
        record_impl_block(X, trait == NULL
                    ? X->C->impls.inherent
                    : X->C->impls.trait,
                IR_TYPE_DID(type), impl);
    }
}

static void collect_trait_decl(struct ItemCollector *X, struct HirTraitDecl *d)
{
    collect_generic_bounds(X, d->generics);
    IrGenericDefs *generics = collect_generic_defs(X, d->generics);

    WITH_CONTEXT(X, GET_TYPE(X, d->id),
        collect_method_decls(X, d->did, d->methods, d->is_pub);
    );
    struct HirDecl *const *pdecl;
    IrTypeList *methods = IrTypeList_new(X->C);
    K_LIST_FOREACH (d->methods, pdecl) {
        struct HirFnDecl *method = HirGetFnDecl(*pdecl);
        IrType *type = pawIr_get_def_type(X->C, method->did);
        IrTypeList_push(X->C, methods, type);
    }

    struct IrTraitDef *r = pawIr_new_trait_def(X->C, d->did, d->ident.name, generics, methods, d->is_pub);
    TraitDefMap_insert(X->C, X->C->trait_defs, d->did, r);
}

static void collect_const_decl(struct ItemCollector *X, struct HirConstDecl *d)
{
    SET_TYPE(X, d->id, collect_type(X, d->tag));
}

static void collect_other_types(struct ItemCollector *X, struct HirModule m)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (m.items, pitem) {
        switch (HIR_KINDOF(*pitem)) {
            case kHirAdtDecl:
                collect_adt_decl(X, HirGetAdtDecl(*pitem));
                break;
            case kHirImplDecl:
                collect_impl_decl(X, HirGetImplDecl(*pitem));
                break;
            case kHirTraitDecl:
                collect_trait_decl(X, HirGetTraitDecl(*pitem));
                break;
            case kHirFnDecl:
                collect_fn_decl(X, HirGetFnDecl(*pitem));
                break;
            case kHirConstDecl:
                collect_const_decl(X, HirGetConstDecl(*pitem));
                break;
            default:
                paw_assert(HirIsTypeDecl(*pitem));
                break;
        }
    }
}

static void validate_trait_impls(struct ItemCollector *X, struct HirModule m)
{
}

static void collection_phase_2(struct ItemCollector *X, struct Hir *hir)
{
    struct HirModule const *pm;
    K_LIST_FOREACH (hir->modules, pm) {
        start_module(X, pm);
        collect_other_types(X, *pm);
        finish_module(X);
    }

    K_LIST_FOREACH (hir->modules, pm) {
        start_module(X, pm);
        validate_trait_impls(X, *pm);
        finish_module(X);
    }
}

// Entrypoint to item collection
void pawP_collect_items(struct Compiler *C, struct Pool *pool)
{
    struct ItemCollector X = {
        .hir = C->hir,
        .pool = pool,
        .P = ENV(C),
        .C = C,
    };

    DLOG(&X, "collecting %d module(s)", C->modules->count);

    collection_phase_1(&X, C->hir);
    collection_phase_2(&X, C->hir);

    DeclId const strtab_did = pawP_builtin_info(C, BUILTIN_MAP)->did;
    IrTypeList *strtab_types = IrTypeList_new(C);
    IrTypeList_push(C, strtab_types, pawP_builtin_type(C, BUILTIN_STR));
    IrTypeList_push(C, strtab_types, pawP_builtin_type(C, BUILTIN_UNIT));
    C->strtab_type = pawIr_new_adt(C, strtab_did, strtab_types);

    DeclId const main_args_did = pawP_builtin_info(C, BUILTIN_LIST)->did;
    IrTypeList *main_args_types = IrTypeList_new(C);
    IrTypeList_push(C, main_args_types, pawP_builtin_type(C, BUILTIN_STR));
    C->main_args_type = pawIr_new_adt(C, main_args_did, main_args_types);
}

