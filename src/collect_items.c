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
#include "gc.h"
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

static void set_def_type(struct ItemCollector *X, DeclId did, NodeId id)
{
    IrType *type = pawIr_get_type(X->C, id);
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
        IrType *type = pawIr_new_generic(X->C, did, NULL);
        struct IrGenericDef *r = pawIr_new_generic_def(X->C, did, d->ident.name);
        IrGenericDefs_push(X->C, result, r);
        set_def_type(X, did, (*pdecl)->hdr.id);
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
        set_def_type(X, d->did, d->id);
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
            if (pawS_eq(panno->name, CSTR(C, CSTR_EXTERN))) {
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

    // non-defaulted trait methods have no bodies
    paw_Bool const in_trait = X->ctx != NULL && IrIsTraitObj(X->ctx);

    if (d->body == NULL && !in_trait && !def->is_extern)
        COLLECTOR_ERROR(X, missing_function_body, d->span.start, d->ident.name->text);
}

static struct IrVariantDefs *create_struct_variant(struct ItemCollector *X, struct HirIdent ident, struct HirDeclList *decls)
{
    paw_assert(decls->count == 1);
    struct HirVariantDecl *v = HirGetVariantDecl(K_LIST_FIRST(decls));
    struct IrFieldDefs *fields = collect_field_defs(X, v->fields);
    struct IrVariantDef *r = pawIr_new_variant_def(X->C, v->did, NO_DECL, NO_DECL, 0, ident.name, fields);
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
        struct IrVariantDef *r = pawIr_new_variant_def(X->C, d->did, NO_DECL, NO_DECL, d->index, d->ident.name, fields);
        VariantDefMap_insert(X->C, X->C->variant_defs, d->did, r);
        IrVariantDefs_push(X->C, variants, r);
        set_def_type(X, d->did, d->id);
    }
    return variants;
}

static void ensure_unique(struct ItemCollector *X, StringMap *map, struct HirIdent ident, char const *what)
{
    Str *const *pname = StringMap_get(X->C, map, ident.name);
    if (pname != NULL)
        COLLECTOR_ERROR(X, duplicate_item, ident.span.start, what, ident.name->text);
    StringMap_insert(X->C, map, ident.name, NULL);
}

static void collect_param_types(struct ItemCollector *X, struct HirDeclList *params)
{
    if (params == NULL) return;
    StringMap *names = StringMap_new_from(X->C, X->pool);
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (params, pdecl) {
        struct HirParamDecl *d = HirGetParamDecl(*pdecl);
        ensure_unique(X, names, d->ident, "function parameter");
        SET_TYPE(X, d->id, collect_type(X, d->tag));
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
        // E {X(str)}', E::X has type 'fn(str) -> E'.
        IrTypeList *params = d->fields != NULL
                                        ? pawHir_collect_decl_types(X->C, d->fields)
                                        : IrTypeList_new(X->C);
        IrType *type = pawIr_new_signature(X->C, d->did, NULL, params, X->ctx);
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
    set_def_type(X, d->did, d->id);
}

static void collect_trait_type(struct ItemCollector *X, struct HirTraitDecl *d)
{
    IrTypeList *generics = collect_generic_types(X, d->generics);
    IrType *type = pawIr_new_trait_obj(X->C, d->did, generics);
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

static void collect_generic_bounds(struct ItemCollector *X, struct HirDeclList *generics, IrType *parent)
{
    if (generics == NULL) return;

    IrType *const *ptype;
    struct HirDecl *const *pdecl;
    K_LIST_ZIP (generics, pdecl, IR_TYPE_SUBTYPES(parent), ptype) {
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        struct IrGeneric *g = IrGetGeneric(*ptype);
        g->bounds = collect_bounds(X, d->bounds);
    }
}

static paw_Bool is_self_param(struct ItemCollector *X, struct HirDecl *decl)
{
    struct HirParamDecl *param = HirGetParamDecl(decl);
    return pawS_eq(param->ident.name, CSTR(X, CSTR_SELF));
}

static void unify_with_self(struct ItemCollector *X, struct SourceLoc loc, IrType *self)
{
    if (pawU_unify(X->C->U, self, X->ctx) != 0) {
        char const *lhs = pawIr_print_type(X->C, self);
        char const *rhs = pawIr_print_type(X->C, X->ctx);
        COLLECTOR_ERROR(X, incompatible_types, loc, lhs, rhs);
    }
}

static void collect_fn_decl(struct ItemCollector *X, struct HirFnDecl *d)
{
    IrTypeList *types = collect_generic_types(X, d->generics);
    collect_local_type_aliases(X, d->body);
    collect_param_types(X, d->params);

    IrTypeList *params = pawHir_collect_decl_types(X->C, d->params);
    IrType *result = collect_type(X, d->result);
    IrType *sig = pawIr_new_signature(X->C, d->did, types, params, result);
    collect_generic_bounds(X, d->generics, sig);
    SET_TYPE(X, d->id, sig);

    {
        struct IrGenericDefs *generics = collect_generic_defs(X, d->generics);
        struct IrParams *params = collect_parameters(X, d->params);
        struct IrFnDef *r = pawIr_new_fn_def(X->C, d->did, d->ident.name, generics, params, d->is_pub);
        FnDefMap_insert(X->C, X->C->fn_defs, d->did, r);
        transfer_fn_annotations(X, d, r);
        set_def_type(X, d->did, d->id);
    }

    if (X->ctx != NULL) {
        if (d->params->count > 0) {
            struct HirDecl *first = K_LIST_FIRST(d->params);
            if (is_self_param(X, first)) // make sure "self: Self" is true
                unify_with_self(X, first->hdr.span.start, K_LIST_FIRST(params));
        }
        IrGetSignature(sig)->self = X->ctx;
    }
}

static void collect_method_decls(struct ItemCollector *X, struct HirDeclList *methods, paw_Bool force_pub)
{
    struct HirDecl *const *pdecl;
    StringMap *names = StringMap_new_from(X->C, X->pool);
    K_LIST_FOREACH (methods, pdecl) {
        struct HirFnDecl *d = HirGetFnDecl(*pdecl);
        ensure_unique(X, names, d->ident, "method");
        if (force_pub) d->is_pub = PAW_TRUE;
        collect_fn_decl(X, d);
    }
    StringMap_delete(X->C, names);
}

static void collect_adt_decl(struct ItemCollector *X, struct HirAdtDecl *d)
{
    IrType *type = GET_TYPE(X, d->id);
    collect_generic_bounds(X, d->generics, type);

    struct IrAdtDef *r = pawIr_get_adt_def(X->C, d->did);
    r->generics = collect_generic_defs(X, d->generics);
    r->variants = collect_variant_defs(X, d);

    struct HirType *const *ptype;
    K_LIST_FOREACH (d->traits, ptype) {
        struct HirPathType const *path = HirGetPathType(*ptype);
        IrType *trait = collect_trait_path(X, path->path);
        pawP_add_trait_impl(X->C, type, trait);
        SET_NODE_TYPE(X->C, *ptype, trait);
    }

    WITH_CONTEXT(X, type,
        collect_variant_types(X, d->variants, d->is_struct);
        collect_method_decls(X, d->methods, PAW_FALSE);
    );
}

static void collect_trait_decl(struct ItemCollector *X, struct HirTraitDecl *d)
{
    IrType *type = GET_TYPE(X, d->id);
    collect_generic_bounds(X, d->generics, type);

    WITH_CONTEXT(X, type,
        collect_method_decls(X, d->methods, d->is_pub);
    );
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

static void validate_traits(struct ItemCollector *X, struct HirModule m)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (m.items, pitem) {
        if (HirIsAdtDecl(*pitem)) {
            struct HirAdtDecl *d = HirGetAdtDecl(*pitem);
            pawP_validate_adt_traits(X->C, d);
        }
    }
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
        validate_traits(X, *pm);
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
}

