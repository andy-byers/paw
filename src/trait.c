// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "map.h"
#include "type_folder.h"
#include "unify.h"

#define TRAIT_ERROR(C_, Kind_, Modno_, ...) pawErr_##Kind_(C_, ModuleList_get((C_)->modules, Modno_)->name, __VA_ARGS__)

static void unify(struct Compiler *C, int modno, struct SourceLoc loc, IrType *a, IrType *b)
{
    if (pawU_unify(C->U, a, b) != 0) {
        char const *lhs = pawIr_print_type(C, a);
        char const *rhs = pawIr_print_type(C, b);
        TRAIT_ERROR(C, incompatible_types, modno, loc, lhs, rhs);
    }
}

static struct IrTypeList *query_traits(struct Compiler *C, struct IrType *type, paw_Bool create_if_missing)
{
    if (IrIsGeneric(type))
        return IrGetGeneric(type)->bounds;
    if (IrIsInfer(type))
        return IrGetInfer(type)->bounds;
    if (!IrIsAdt(type))
        return NULL;

    struct IrAdt *t = IrGetAdt(type);
    struct IrTypeList *const *ptraits = TraitMap_get(C, C->traits, t->did);
    if (ptraits != NULL)
        return *ptraits;

    struct IrTypeList *traits = NULL;
    if (create_if_missing) {
        traits = IrTypeList_new(C);
        TraitMap_insert(C, C->traits, t->did, traits);
    }
    return traits;
}

struct IrTypeList *pawP_query_traits(struct Compiler *C, struct IrType *type)
{
    return query_traits(C, type, PAW_FALSE);
}

void pawP_add_trait_impl(struct Compiler *C, struct IrType *type, struct IrType *trait)
{
    struct IrTypeList *traits = query_traits(C, type, PAW_TRUE);
    IrTypeList_push(C, traits, trait);
}

static paw_Bool traits_match(struct Compiler *C, struct IrType *a, struct IrType *b)
{
    if (IR_TYPE_DID(a).value == IR_TYPE_DID(b).value) {
        unify(C, 1, (struct SourceLoc){-1}, a, b); // TODO: source location
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool implements_trait(struct Compiler *C, struct IrType *type, struct IrType *trait)
{
    struct IrType **pt;
    struct IrTypeList *traits = pawP_query_traits(C, type);
    if (traits == NULL)
        return PAW_FALSE;
    K_LIST_FOREACH (traits, pt) {
        struct IrType *t = *pt;
        if (IrIsAdt(type)) {
            struct HirDecl *base_decl = pawHir_get_decl(C, IR_TYPE_DID(type));
            struct IrType *base_type = GET_NODE_TYPE(C, base_decl);
            struct IrTypeList *types = pawP_instantiate_typelist(C, IR_TYPE_SUBTYPES(base_type),
                                                                 IR_TYPE_SUBTYPES(type), IR_TYPE_SUBTYPES(t));
            t = pawIr_new_trait_obj(C, IR_TYPE_DID(t), types);
        }
        if (traits_match(C, t, trait))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

paw_Bool pawP_satisfies_bounds(struct Compiler *C, struct IrType *type, struct IrTypeList *bounds)
{
    if (bounds == NULL)
        return PAW_TRUE;

    struct IrType **pbound;
    K_LIST_FOREACH (bounds, pbound) {
        if (!implements_trait(C, type, *pbound))
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

struct TraitSubstitution {
    struct IrType *trait;
    struct IrType *adt;
};

static struct IrType *subst_trait_obj(struct IrTypeFolder *F, struct IrTraitObj *t)
{
    struct Compiler *C = F->C;
    struct TraitSubstitution *subst = F->ud;
    struct IrType *type = IR_CAST_TYPE(t);
    if (pawU_equals(C->U, type, subst->trait)) {
        return subst->adt;
    }
    return type;
}

struct IrType *pawIr_substitute_self(struct Compiler *C, struct IrType *trait, struct IrType *adt, struct IrType *method)
{
    struct IrTypeFolder F;
    struct TraitSubstitution subst = {
        .trait = trait,
        .adt = adt,
    };
    pawIr_type_folder_init(&F, C, &subst);
    F.FoldTraitObj = subst_trait_obj;
    struct IrSignature *fsig = IrGetSignature(method);
    struct IrTypeList *types = pawIr_fold_type_list(&F, fsig->types);
    struct IrTypeList *params = pawIr_fold_type_list(&F, fsig->params);
    struct IrType *result = pawIr_fold_type(&F, fsig->result);
    return pawIr_new_signature(C, fsig->did, types, params, result);
}

struct TraitOwnerList *pawP_get_trait_owners(struct Compiler *C, struct IrType *adt)
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

static void register_builtin_trait_method(struct Compiler *C, struct IrType *adt, struct IrType *method, enum TraitKind tk)
{
    struct HirFuncDecl *adt_method = HirGetFuncDecl(pawHir_get_decl(C, IR_TYPE_DID(method)));
    if (tk == TRAIT_EQUALS && adt_method->ident.name->text[0] != 'e') {
        return; // special case: only care about "eq" method
    }
    struct TraitOwnerList *owners = pawP_get_trait_owners(C, adt);
    struct IrTypeList **pmethods = &K_LIST_AT(owners, tk);
    if (*pmethods == NULL)
        *pmethods = IrTypeList_new(C);
    IrTypeList_push(C, *pmethods, method);
}

static void ensure_methods_match(struct Compiler *C, struct SourceLoc loc, struct IrType *adt, struct IrType *adt_method, struct IrType *trait, struct HirTraitDecl *trait_decl, struct HirFuncDecl *trait_method)
{
    struct IrType *a = adt_method;
    struct IrType *b = pawIr_get_type(C, trait_method->hid);
    if (trait_decl->generics != NULL)
        b = pawP_instantiate_method(C, HIR_CAST_DECL(trait_decl),
                                    IR_TYPE_SUBTYPES(trait), HIR_CAST_DECL(trait_method));

    // substitute all instances of the trait object type for the type of the implementor
    b = pawIr_substitute_self(C, trait, adt, b);
    b = pawP_generalize(C, b);
    unify(C, IR_TYPE_DID(adt).modno, loc, a, b);

    enum TraitKind const tk = pawHir_kindof_trait(C, trait_decl);
    if (tk != TRAIT_USER)
        register_builtin_trait_method(C, adt, a, tk);
}

DEFINE_MAP(struct Compiler, MethodMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, String *, struct IrType *)

static void ensure_trait_implemented(struct Compiler *C, struct HirTraitDecl *trait_decl, MethodMap *methods, struct IrType *adt, struct IrType *trait)
{
    struct HirAdtDecl *d = HirGetAdtDecl(pawHir_get_decl(C, IR_TYPE_DID(adt)));

    struct HirDecl **pdecl;
    K_LIST_FOREACH (trait_decl->methods, pdecl) {
        struct HirFuncDecl *trait_method = HirGetFuncDecl(*pdecl);
        struct IrType *const *pmethod = MethodMap_get(C, methods, trait_method->ident.name);
        if (pmethod == NULL)
            TRAIT_ERROR(C, missing_trait_method, trait_method->did.modno, trait_method->span.start,
                    trait_method->ident.name->text);

        struct HirFuncDecl *adt_method = HirGetFuncDecl(pawHir_get_decl(C, IR_TYPE_DID(*pmethod)));
        if (adt_method->is_pub != trait_method->is_pub)
            TRAIT_ERROR(C, trait_method_visibility_mismatch, adt_method->did.modno, adt_method->span.start,
                    trait_method->is_pub, adt_method->ident.name->text);

        ensure_methods_match(C, adt_method->span.start, adt, *pmethod, trait, trait_decl, trait_method);
    }
}

static void add_defaulted_methods(struct Compiler *C, struct HirTraitDecl *trait, struct IrType *adt, MethodMap *map)
{
    struct IrType *trait_obj = pawIr_get_type(C, trait->hid);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (trait->methods, pdecl) {
        struct HirFuncDecl *method = HirGetFuncDecl(*pdecl);
        if (method->body != NULL) {
            struct IrType *type = GET_NODE_TYPE(C, *pdecl);
            type = pawIr_substitute_self(C, trait_obj, adt, type);
            MethodMap_insert(C, map, method->ident.name, type);
        }
    }
}

void pawP_validate_adt_traits(struct Compiler *C, struct HirAdtDecl *d)
{
    struct IrType *adt = pawIr_get_type(C, d->hid);
    struct IrTypeList *traits = pawP_query_traits(C, adt);
    if (traits == NULL)
        return;
    MethodMap *map = MethodMap_new(C);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (d->methods, pdecl) {
        struct IrType *method = GET_NODE_TYPE(C, *pdecl);
        MethodMap_insert(C, map, hir_decl_ident(*pdecl).name, method);
    }

    struct IrType **ptype;
    K_LIST_FOREACH (traits, ptype) {
        struct HirTraitDecl *trait = HirGetTraitDecl(
            pawHir_get_decl(C, IR_TYPE_DID(*ptype)));
        add_defaulted_methods(C, trait, adt, map);
        ensure_trait_implemented(C, trait, map, adt, *ptype);
    }

    MethodMap_delete(C, map);
}
