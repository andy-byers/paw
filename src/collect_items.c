// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// collect_items.c: Definition of pawP_collect_items.
//
// TODO: Be careful about adding preconditions and obligations to the global
//   solver context. Works because generics have unique IDs, but will make it
//   more difficult to report decent errors. Of course, it is also terribly
//   slow because preconditions and obligations pile up quickly and the search
//   is linear.

#include "compile.h"
#include "debug.h"
#include "error.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "resolve.h"
#include "solve.h"
#include "type_folder.h"
#include "unify.h"

#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))
#define GET_TYPE(X, id) pawIr_get_type((X)->C, id)
#define SET_TYPE(X, id, t) pawIr_set_type((X)->C, id, t)

#define COLLECTOR_ERROR(X_, Kind_, ...) pawErr_##Kind_((X_)->C, (X_)->pm->name, __VA_ARGS__)

#define WITH_CONTEXT(X, type, Binder_, Code_) \
    do {                            \
        (X)->ctx = (type);          \
        (X)->binder = (Binder_);    \
        Code_                       \
        (X)->binder = NULL;         \
        (X)->ctx = NULL;            \
    } while (0)

struct ItemCollector {
    struct HirModule const *pm;
    struct Compiler *C;
    struct Pool *pool;
    IrTypeList *binder;
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
    return pawP_lower_type(X->C, *X->pm, type);
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

static void add_preconditions_from(struct ItemCollector *X, DeclId did)
{
    pawIr_solver_add_preconditions_from(X->C->S, did);
}

static IrTrait *collect_trait_path(struct ItemCollector *X, struct HirPath path, IrType *self)
{
    paw_assert(path.kind == HIR_PATH_ITEM);
    paw_assert(path.segments->count == 1);
    struct HirSegment const last = K_LIST_LAST(path.segments);
    NodeId const id = SegmentTable_get(X->C, X->C->segtab, last.id)->id;
    struct HirDecl const *decl = pawHir_get_node(X->hir, id);

    IrTypeList *args = collect_types(X, last.types);
    if (args == NULL) args = IrTypeList_new(X->C);
    IrTypeList_insert(X->C, args, 0, self);
    return pawIr_solver_instantiate_trait_with(X->C->S, decl->hdr.did, args);
}

static IrTraitList *collect_bounds(struct ItemCollector *X, struct HirBoundList *bounds, IrType *self)
{
    if (bounds == NULL) return NULL;
    struct HirGenericBound *pbound;
    IrTraitList *result = IrTraitList_new(X->C);
    K_LIST_FOREACH (bounds, pbound) {
        IrTrait *trait = collect_trait_path(X, pbound->path, self);
        IrTraitList_push(X->C, result, trait);
    }
    return result;
}

static void set_def_type(struct ItemCollector *X, DeclId did, IrType *type)
{
    DefTypeMap_insert(X->C, X->C->def_types, did, type);
}

static struct IrGenericDefs *collect_generic_defs(struct ItemCollector *X, struct HirDeclList *generics)
{
    struct IrGenericDefs *result = IrGenericDefs_new(X->C);
    if (generics != NULL) {
        K_LIST_XFOREACH (generics, struct HirDecl *const, p) {
            struct HirGenericDecl const *d = HirGetGenericDecl(*p);
            IrTraitList *bounds = pawIr_get_trait_bounds(X->C, d->did);
            struct IrGenericDef *r = pawIr_new_generic_def(X->C, d->did, d->ident.name, bounds);
            GenericDefMap_insert(X->C, X->C->generic_defs, d->did, r);
            IrGenericDefs_push(X->C, result, r);
        }
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
    paw_Bool const in_trait = X->ctx != NULL && IrIsGeneric(X->ctx);
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
        IrType *type = pawIr_new_signature(X->C, d->did, X->binder);
        SET_TYPE(X, d->id, type);
        def->cons_did = d->did;
    }

    IrTypeList *generics = IR_TYPE_SUBTYPES_(X->ctx);
    if (generics != NULL)
        pawIr_set_generic_types(X->C, d->did, generics);
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
    X->pm = pm;
}

static void finish_module(struct ItemCollector *X)
{
    X->pm = NULL;
}

static IrTypeList *collect_generic_types(struct ItemCollector *X, DeclId parent_did, struct HirDeclList *generics)
{
    IrTypeList *types = NULL;
    if (generics != NULL) {
        types = IrTypeList_new(X->C);

        // Create a local symbol for each generic. Generic bounds are registered
        // in a later pass, once all nominal types are known.
        struct HirDecl *const *pdecl;
        K_LIST_FOREACH (generics, pdecl) {
            struct HirGenericDecl const *d = HirGetGenericDecl(*pdecl);
            IrType *type = pawIr_new_generic(X->C, d->did);
            IrTypeList_push(X->C, types, type);
            SET_NODE_TYPE(X->C, *pdecl, type);
            set_def_type(X, d->did, type);
        }
    }
    pawIr_set_generic_types(X->C, parent_did, types);
    return types;
}

static void collect_generic_bounds(struct ItemCollector *X, struct HirDeclList *generics)
{
    if (generics != NULL) {
        struct HirDecl *const *pdecl;
        K_LIST_FOREACH (generics, pdecl) {
            struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
            IrType *generic = pawIr_get_def_type(X->C, d->did);
            IrTraitList *bounds = collect_bounds(X, d->bounds, generic);
            pawIr_set_trait_bounds(X->C, d->did, bounds);
        }
    }
}

static paw_Bool is_builtin(struct ItemCollector *X, DeclId did, enum BuiltinKind kind)
{
    return did.value == pawP_builtin_info(X->C, kind)->did.value;
}

static void collect_nominal_type(struct ItemCollector *X, struct HirAdtDecl *d)
{
    IrType *type;
    if (is_builtin(X, d->did, BUILTIN_UNIT)) {
        type = pawIr_new_unit(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_BOOL)) {
        type = pawIr_new_bool(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_CHAR)) {
        type = pawIr_new_char(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_INT)) {
        type = pawIr_new_int(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_FLOAT)) {
        type = pawIr_new_float(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_STR)) {
        type = pawIr_new_str(X->C);
    } else {
        IrTypeList *types = collect_generic_types(X, d->did, d->generics);
        type = pawIr_new_adt(X->C, d->did, types);

        // TODO: move this to later pass
        struct IrAdtDef *r = pawIr_new_adt_def(X->C, d->did, d->ident.name,
                NULL, NULL, d->is_pub, d->is_struct, d->is_inline);
        AdtDefMap_insert(X->C, X->C->adt_defs, d->did, r);
    }

    set_def_type(X, d->did, type);
    SET_TYPE(X, d->id, type);
}

static void collect_trait_type(struct ItemCollector *X, struct HirTraitDecl *d)
{
    collect_generic_types(X, d->did, d->generics);
}

static void collect_nominal_types(struct ItemCollector *X, struct HirModule m)
{
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p) {
        if (HirIsAdtDecl(*p)) {
            collect_nominal_type(X, HirGetAdtDecl(*p));
        } else if (HirIsTraitDecl(*p)) {
            collect_trait_type(X, HirGetTraitDecl(*p));
        }
    }
}

static void collect_type_decl(struct ItemCollector *X, struct HirTypeDecl *d)
{
    collect_generic_types(X, d->did, d->generics);
    collect_generic_bounds(X, d->generics);
    add_preconditions_from(X, d->did);

    IrType *type = collect_type(X, d->rhs);
    set_def_type(X, d->did, type);
    SET_TYPE(X, d->id, type);
}

static void collect_trait_bounds(struct ItemCollector *X, struct HirModule m)
{
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p) {
        if (HirIsAdtDecl(*p)) {
            struct HirAdtDecl const *d = HirGetAdtDecl(*p);
            collect_generic_bounds(X, d->generics);
            add_preconditions_from(X, d->did);
        } else if (HirIsTraitDecl(*p)) {
            struct HirTraitDecl const *d = HirGetTraitDecl(*p);
            collect_generic_bounds(X, d->generics);
            add_preconditions_from(X, d->did);
        }
    }
}

static void collect_type_aliases(struct ItemCollector *X, struct HirModule m)
{
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p) {
        if (HirIsTypeDecl(*p))
            collect_type_decl(X, HirGetTypeDecl(*p));
    }
}

static void collect_local_type_decl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    collect_type_decl(V->ud, d);
}

static void collect_local_type_aliases(struct ItemCollector *X, struct HirExpr *block)
{
    struct HirVisitor V;
    pawHir_visitor_init(&V, X->hir, X);
    V.PostVisitTypeDecl = collect_local_type_decl;
    pawHir_visit_expr(&V, block);
}

static void solve_all_obligations(struct ItemCollector *X)
{
    if (pawIr_solver_solve(X->C->S) != 0)
        pawErr_generic_error(ENV(X), SCAN_STR(X->C, "<module>"), (struct SourceLoc){0},
                "unable to solve trait obligation");
}

#define MAP_MODULES(X_, Modules_, Action_) \
        K_LIST_XFOREACH (Modules_, struct HirModule const, p_) { \
            start_module(X_, p_); \
            Action_(X_, *(p_)); \
            finish_module(X_); \
        }

static void collection_phase_1(struct ItemCollector *X, struct Hir *hir)
{
    pawIr_push_solver(X->C);

    MAP_MODULES(X, hir->modules, collect_nominal_types);
    MAP_MODULES(X, hir->modules, collect_trait_bounds);
    MAP_MODULES(X, hir->modules, collect_type_aliases);

    solve_all_obligations(X);
    pawIr_pop_solver(X->C);
}

static void unify_with_self(struct ItemCollector *X, struct SourceLoc loc, IrType *self)
{
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
    struct IrFnPtr *fn = IrGetFnPtr(
            IR_SIGNATURE_FN(X->C, type));
    if (fn->params->count > 1)
        pawErr_exceeded_limit(ENV(X), X->pm->name, loc,
                "parameters for \"main\" function", 1);

    if (fn->params->count == 1) {
        IrType *args = IrTypeList_first(fn->params);
        if (builtin_kind(X, args) != BUILTIN_LIST
                && builtin_kind(X, ir_list_elem(args)) != BUILTIN_INT)
            pawErr_generic_error(ENV(X), X->pm->name, loc,
                    "single argument to \"main\" must have type \"[str]\"");
    }

    if (builtin_kind(X, fn->result) != BUILTIN_UNIT
            && builtin_kind(X, fn->result) != BUILTIN_INT)
        pawErr_generic_error(ENV(X), X->pm->name, loc,
                "return type of \"main\" must have type \"()\" or \"int\"");
}

static void collect_fn_decl(struct ItemCollector *X, struct HirFnDecl *d)
{
    IrTypeList *generics = collect_generic_types(X, d->did, d->generics);
    collect_generic_bounds(X, d->generics);
    add_preconditions_from(X, d->did);
    if (d->body != NULL) collect_local_type_aliases(X, d->body);
    collect_param_types(X, d->params);

    IrTypeList *types = IrTypeList_new(X->C);
    IrTypeList *params = pawHir_collect_decl_types(X->C, d->params);
    IrType *result = collect_type(X, d->result);
    IrType *type = pawIr_new_signature(X->C, d->did, types);
    SET_TYPE(X, d->id, type);

    {
        if (X->binder != NULL) {
            // add generics from parent binder
            K_LIST_XFOREACH (X->binder, IrType *const, p)
                IrTypeList_push(X->C, types, *p);
        }
        if (generics != NULL) {
            // add generics from function binder
            K_LIST_XFOREACH (generics, IrType *const, p)
                IrTypeList_push(X->C, types, *p);
        }
        // overwrite old "generic types" entry with full binder
        IrGenericTypes_insert(X->C, X->C->ir_generic_types, d->did, types);
    }

    {
        struct IrGenericDefs *generics = collect_generic_defs(X, d->generics);
        struct IrParams *params = collect_parameters(X, d->params);
        struct IrFnDef *r = pawIr_new_fn_def(X->C, d->did, d->ident.name,
                generics, result, params, X->ctx, d->parent_id, d->is_pub);
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
    IrType *type = pawIr_get_def_type(X->C, d->did);
    // skip basic types, i.e. "int"
    if (!IrIsAdt(type)) return;

    struct IrAdtDef *def = pawIr_get_adt_def(X->C, d->did);
    add_preconditions_from(X, d->did);
    def->generics = collect_generic_defs(X, d->generics);
    def->variants = collect_variant_defs(X, d);

    WITH_CONTEXT(X, type, IR_TYPE_SUBTYPES(type),
        collect_variant_types(X, d->variants, d->is_struct);
    );
}

static void record_impl_block(struct ItemCollector *X, IrDefs *impls, DeclId did)
{
    IrDefs_push(X->C, impls, did);
}

struct MethodInfo {
    IrType *type;
    paw_Bool found;
};

DEFINE_MAP(struct ItemCollector, MethodMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, struct MethodInfo)

static MethodMap *collect_trait_methods(struct ItemCollector *X, DeclId did)
{
    struct IrTraitDef const *trait = pawIr_get_trait_def(X->C, did);
    MethodMap *methods = MethodMap_new(X);
    K_LIST_XFOREACH (trait->methods, IrType *const, p) {
        struct IrFnDef const *fn = pawIr_get_fn_def(X->C, IR_TYPE_DID(*p));
        MethodMap_insert(X, methods, fn->name, (struct MethodInfo){
                    .type = *p,
                });
    }
    return methods;
}

// TODO: already checking for duplicate method names elsewhere, probably don't do that again here
static void validate_trait_impl(struct ItemCollector *X, DeclId did)
{
    struct IrImpl const *impl = pawIr_get_impl_def(X->C, did);
    if (impl->trait == NULL) return;

    MethodMap *trait_methods = collect_trait_methods(X, impl->trait->did);
    K_LIST_XFOREACH (impl->methods, IrType *const, m) {
        // ensure that each method defined in a trait impl block can be found
        // in the corresponding trait definition
        struct IrFnDef const *fn = pawIr_get_fn_def(X->C, IR_TYPE_DID(*m));
        struct MethodInfo *info = MethodMap_get(X, trait_methods, fn->name);
        if (info == NULL)
            pawErr_generic_error(ENV(X), X->pm->name, (struct SourceLoc){0},
                    "trait impl method %s not found in trait definition",
                    fn->name->text);
        if (info->found)
            pawErr_generic_error(ENV(X), X->pm->name, (struct SourceLoc){0},
                    "duplicate method %s found in trait impl block",
                    fn->name->text);
        info->found = PAW_TRUE;

        IrTypeList *args = IrTypeList_new(X->C);
        {
            IrTypeList *trait_args = impl->trait->types;
            IrTypeList_reserve(X->C, args, trait_args->count + fn->generics->count);

            // add type arguments from instantiated trait (already has "Self")
            K_LIST_XFOREACH (trait_args, IrType *const, t)
                IrTypeList_push(X->C, args, *t);

            // add type parameters from impl block method
            K_LIST_XFOREACH (fn->generics, struct IrGenericDef *const, p)
                IrTypeList_push(X->C, args, pawIr_get_def_type(X->C, (*p)->did));
        }

        // instantiate the trait method with "Self" equal to the "Self" type
        // of the impl block
        IrType *method = pawIr_solver_instantiate_type_with(X->C->S, IR_TYPE_DID(info->type), args);
        if (pawU_unify(X->C->U, method, *m) != 0)
            pawErr_generic_error(ENV(X), X->pm->name, (struct SourceLoc){0},
                    "trait impl method %s not compatible with corresponding "
                    "method from trait definition", fn->name->text);
    }
    K_LIST_XFOREACH (impl->methods, IrType *const, m) {
        struct IrFnDef const *fn = pawIr_get_fn_def(X->C, IR_TYPE_DID(*m));
        struct MethodInfo *info = MethodMap_get(X, trait_methods, fn->name);
        if (!info->found)
            pawErr_generic_error(ENV(X), X->pm->name, (struct SourceLoc){0},
                    "trait impl missing method %s from trait definition",
                    fn->name->text);
    }
}

static void collect_impl_decl(struct ItemCollector *X, struct HirImplDecl *d)
{
    IrTypeList *binder = collect_generic_types(X, d->did, d->generics);
    collect_generic_bounds(X, d->generics);
    IrGenericDefs *generics = collect_generic_defs(X, d->generics);
    add_preconditions_from(X, d->did);

    IrType *type = collect_type(X, d->type);

    IrTrait *trait = NULL;
    paw_Bool force_pub = PAW_FALSE;
    if (d->trait != NULL) {
        struct HirPathType *t = HirGetPathType(d->trait);
        trait = collect_trait_path(X, t->path, type);
        // propagate visibility qualifier from trait
        struct HirTraitDecl const *d = HirGetTraitDecl(
                pawHir_get_decl(X->hir, trait->did));
        force_pub = d->is_pub;
    }
    SET_TYPE(X, d->id, type);

    WITH_CONTEXT(X, type, binder,
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
        IrDefs_push(X->C, X->C->impls.blanket, impl->did);
    } else if (trait == NULL) {
        // found inherent implementation
        record_impl_block(X, X->C->impls.inherent, impl->did);
    } else {
        // found trait implementation
        record_impl_block(X, X->C->impls.trait, impl->did);
    }
}

static void collect_trait_decl(struct ItemCollector *X, struct HirTraitDecl *d)
{
    IrGenericDefs *generics = collect_generic_defs(X, d->generics);
    add_preconditions_from(X, d->did);

    IrTypeList *params = pawIr_get_generic_types(X->C, d->did);
    WITH_CONTEXT(X, IrTypeList_first(params), params,
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

static void collect_item_defs(struct ItemCollector *X, struct HirModule m)
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
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p) {
        if (HirIsImplDecl(*p))
            validate_trait_impl(X, (*p)->hdr.did);
    }
}

static void collection_phase_2(struct ItemCollector *X, struct Hir *hir)
{
    pawIr_push_solver(X->C);
    MAP_MODULES(X, hir->modules, collect_item_defs);
    MAP_MODULES(X, hir->modules, validate_trait_impls);

    solve_all_obligations(X);
    pawIr_pop_solver(X->C);
}

#undef MAP_MODULES

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

    pawU_enter_binder(C->U, SCAN_STR(C, "<module>"));

    collection_phase_1(&X, C->hir);
    collection_phase_2(&X, C->hir);

    pawU_leave_binder(C->U);

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

