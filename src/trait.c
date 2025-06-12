// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "map.h"
#include "type_folder.h"
#include "unify.h"

#define TRAIT_ERROR(C_, Kind_, Modno_, ...) pawErr_##Kind_(C_, HirModuleList_get((C_)->hir->modules, Modno_).name, __VA_ARGS__)

static void unify(struct Compiler *C, int modno, struct SourceLoc loc, IrType *a, IrType *b)
{
    if (pawU_unify(C->U, a, b) != 0) {
        char const *lhs = pawIr_print_type(C, a);
        char const *rhs = pawIr_print_type(C, b);
        TRAIT_ERROR(C, incompatible_types, modno, loc, lhs, rhs);
    }
}

static IrTypeList *query_traits(struct Compiler *C, IrType *type, paw_Bool create_if_missing)
{
    if (IrIsGeneric(type))
        return IrGetGeneric(type)->bounds;
    if (IrIsInfer(type))
        return IrGetInfer(type)->bounds;
    if (!IrIsAdt(type))
        return NULL;

    struct IrAdt *t = IrGetAdt(type);
    IrTypeList *const *ptraits = TraitMap_get(C, C->traits, t->did);
    if (ptraits != NULL) return *ptraits;

    IrTypeList *traits = NULL;
    if (create_if_missing) {
        traits = IrTypeList_new(C);
        TraitMap_insert(C, C->traits, t->did, traits);
    }
    return traits;
}

IrTypeList *pawP_query_traits(struct Compiler *C, IrType *type)
{
    return query_traits(C, type, PAW_FALSE);
}

void pawP_add_trait_impl(struct Compiler *C, struct IrType *type, struct IrType *trait)
{
    struct IrTypeList *traits = query_traits(C, type, PAW_TRUE);
    IrTypeList_push(C, traits, trait);
}

static paw_Bool traits_match(struct Compiler *C, IrType *a, IrType *b)
{
    if (IR_TYPE_DID(a).value == IR_TYPE_DID(b).value) {
        unify(C, 1, (struct SourceLoc){-1}, a, b); // TODO: source location
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool implements_trait(struct Compiler *C, IrType *type, IrType *trait)
{
    IrTypeList *traits = pawP_query_traits(C, type);
    if (traits == NULL) return PAW_FALSE;

    IrType *const *pt;
    K_LIST_FOREACH (traits, pt) {
        IrType *t = *pt;
        if (IrIsAdt(type)) {
            struct HirDecl *base_decl = pawHir_get_decl(C->hir, IR_TYPE_DID(type));
            IrType *base_type = GET_NODE_TYPE(C, base_decl);
            IrTypeList *types = pawP_instantiate_typelist(C, IR_TYPE_SUBTYPES(base_type),
                                                                 IR_TYPE_SUBTYPES(type), IR_TYPE_SUBTYPES(t));
            t = pawIr_new_trait_obj(C, IR_TYPE_DID(t), types);
        }
        if (traits_match(C, t, trait))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

paw_Bool pawP_satisfies_bounds(struct Compiler *C, IrType *type, IrTypeList *bounds)
{
    if (bounds == NULL)
        return PAW_TRUE;

    IrType *const *pbound;
    K_LIST_FOREACH (bounds, pbound) {
        if (!implements_trait(C, type, *pbound))
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

struct TraitSubstitution {
    IrType *trait;
    IrType *adt;
};

static IrType *subst_trait_obj(struct IrTypeFolder *F, struct IrTraitObj *t)
{
    struct Compiler *C = F->C;
    struct TraitSubstitution *subst = F->ud;
    IrType *type = IR_CAST_TYPE(t);
    if (pawU_equals(C->U, type, subst->trait)) {
        return subst->adt;
    }
    return type;
}

IrType *pawIr_substitute_self(struct Compiler *C, IrType *trait, IrType *adt, IrType *method)
{
    struct IrTypeFolder F;
    struct TraitSubstitution subst = {
        .trait = trait,
        .adt = adt,
    };
    pawIr_type_folder_init(&F, C, &subst);
    F.FoldTraitObj = subst_trait_obj;
    struct IrSignature *fsig = IrGetSignature(method);
    IrTypeList *types = pawIr_fold_type_list(&F, fsig->types);
    IrTypeList *params = pawIr_fold_type_list(&F, fsig->params);
    IrType *result = pawIr_fold_type(&F, fsig->result);
    IrType *fn = pawIr_new_signature(C, fsig->did, types, params, result);

    IrGetSignature(fn)->self = adt;
    return fn;
}

struct TraitOwnerList *pawP_get_trait_owners(struct Compiler *C, IrType *adt)
{
    struct TraitOwnerList *owners;
    struct TraitOwnerList **pinfo = TraitOwners_get(C, C->trait_owners, adt);
    if (pinfo == NULL) {
        owners = TraitOwnerList_new(C);
        TraitOwners_insert(C, C->trait_owners, adt, owners);
        TraitOwnerList_reserve(C, owners, NBUILTIN_TRAITS);
        while (owners->count < NBUILTIN_TRAITS)
            TraitOwnerList_push(C, owners, NULL);
    } else {
        owners = *pinfo;
    }
    return owners;
}

static void register_builtin_trait_method(struct Compiler *C, IrType *adt, IrType *method, enum TraitKind tk)
{
    struct HirFuncDecl *adt_method = HirGetFuncDecl(pawHir_get_decl(C->hir, IR_TYPE_DID(method)));
    if (tk == TRAIT_EQUALS && adt_method->ident.name->text[0] != 'e') {
        return; // special case: only care about "eq" method
    }
    struct TraitOwnerList *owners = pawP_get_trait_owners(C, adt);
    IrTypeList **pmethods = &K_LIST_AT(owners, tk);
    if (*pmethods == NULL)
        *pmethods = IrTypeList_new(C);
    IrTypeList_push(C, *pmethods, method);
}

static void ensure_methods_match(struct Compiler *C, struct SourceLoc loc, IrType *adt, IrType *adt_method, IrType *trait, struct HirTraitDecl *trait_decl, struct HirFuncDecl const *trait_method)
{
    IrType *a = adt_method;
    IrType *b = pawIr_get_type(C, trait_method->id);
    if (trait_decl->generics != NULL)
        b = pawP_instantiate_method(C, HIR_CAST_DECL(trait_decl),
                IR_TYPE_SUBTYPES(trait), HIR_CAST_DECL(trait_method));

    // substitute all instances of the trait object type for the type of the implementor
    b = pawIr_substitute_self(C, trait, adt, b);
    b = pawP_generalize(C, b);
    unify(C, IR_TYPE_DID(adt).modno, loc, a, b);

    enum TraitKind const tk = pawHir_kindof_trait(C, trait_decl);
    if (tk != TRAIT_USER) register_builtin_trait_method(C, adt, a, tk);
}

DEFINE_MAP(struct Compiler, MethodMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str *, IrType *)

static void ensure_trait_implemented(struct Compiler *C, struct HirTraitDecl *trait_decl, MethodMap *methods, IrType *adt, IrType *trait)
{
    struct HirAdtDecl *d = HirGetAdtDecl(pawHir_get_decl(C->hir, IR_TYPE_DID(adt)));

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (trait_decl->methods, pdecl) {
        struct HirFuncDecl const *trait_fn = HirGetFuncDecl(*pdecl);
        IrType *const *pmethod = MethodMap_get(C, methods, trait_fn->ident.name);
        if (pmethod == NULL)
            TRAIT_ERROR(C, missing_trait_method, trait_fn->did.modno,
                    trait_fn->span.start, trait_fn->ident.name->text);

        struct HirFuncDecl const *adt_fn = HirGetFuncDecl(
                pawHir_get_decl(C->hir, IR_TYPE_DID(*pmethod)));
        if (adt_fn->is_pub != trait_fn->is_pub)
            TRAIT_ERROR(C, trait_method_visibility_mismatch, adt_fn->did.modno,
                    adt_fn->span.start, trait_fn->is_pub, adt_fn->ident.name->text);

        ensure_methods_match(C, adt_fn->span.start, adt, *pmethod, trait, trait_decl, trait_fn);
    }
}

static void add_defaulted_methods(struct Compiler *C, struct HirTraitDecl *trait, IrType *adt, MethodMap *map)
{
    IrType *trait_obj = pawIr_get_type(C, trait->id);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (trait->methods, pdecl) {
        struct HirFuncDecl *method = HirGetFuncDecl(*pdecl);
        if (method->body != NULL) {
            IrType *type = GET_NODE_TYPE(C, *pdecl);
            type = pawIr_substitute_self(C, trait_obj, adt, type);
            MethodMap_insert(C, map, method->ident.name, type);
        }
    }
}

void pawP_validate_adt_traits(struct Compiler *C, struct HirAdtDecl *d)
{
    IrType *adt = pawIr_get_type(C, d->id);
    IrTypeList *traits = pawP_query_traits(C, adt);
    if (traits == NULL) return;
    MethodMap *map = MethodMap_new(C);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (d->methods, pdecl) {
        struct HirIdent const ident = hir_decl_ident(*pdecl);
        IrType *method = GET_NODE_TYPE(C, *pdecl);
        MethodMap_insert(C, map, ident.name, method);
    }

    IrType *const *ptype;
    K_LIST_FOREACH (traits, ptype) {
        struct HirTraitDecl *trait = HirGetTraitDecl(
            pawHir_get_decl(C->hir, IR_TYPE_DID(*ptype)));
        add_defaulted_methods(C, trait, adt, map);
        ensure_trait_implemented(C, trait, map, adt, *ptype);
    }

    MethodMap_delete(C, map);
}

