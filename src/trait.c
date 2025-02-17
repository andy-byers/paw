// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "unify.h"
#include "map.h"
#include "type_folder.h"

static struct IrTypeList *query_traits(struct Compiler *C, struct IrType *type, paw_Bool create_if_missing)
{
    if (IrIsGeneric(type)) return IrGetGeneric(type)->bounds;
    if (IrIsInfer(type)) return IrGetInfer(type)->bounds;
    if (!IrIsAdt(type)) return NULL;

    struct IrAdt *t = IrGetAdt(type);
    struct IrTypeList *const *ptraits = TraitMap_get(C, C->traits, t->did);
    if (ptraits != NULL) return*ptraits;

    struct IrTypeList *traits = NULL;
    if (create_if_missing) {
        traits = pawIr_type_list_new(C);
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
    K_LIST_PUSH(C, traits, trait);
}

static paw_Bool traits_match(struct Compiler *C, struct IrType *a, struct IrType *b)
{
    if (IR_TYPE_DID(a).value == IR_TYPE_DID(b).value) {
        pawU_unify(C->U, a, b);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool implements_trait(struct Compiler *C, struct IrType *type, struct IrType *trait)
{
    struct IrType **pt;
    struct IrTypeList *traits = pawP_query_traits(C, type);
    if (traits == NULL) return PAW_FALSE;
    K_LIST_FOREACH(traits, pt) {
        struct IrType *t = *pt;
        if (IrIsAdt(type)) {
            struct HirDecl *base_decl = pawHir_get_decl(C, IR_TYPE_DID(type));
            struct IrType *base_type = GET_NODE_TYPE(C, base_decl);
            struct IrTypeList *types = pawP_instantiate_typelist(C, IR_TYPE_SUBTYPES(base_type),
                    IR_TYPE_SUBTYPES(type), IR_TYPE_SUBTYPES(t));
            t = pawIr_new_trait_obj(C, IR_TYPE_DID(t), types);
        }
        if (traits_match(C, t, trait)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

paw_Bool pawP_satisfies_bounds(struct Compiler *C, struct IrType *type, struct IrTypeList *bounds)
{
    if (bounds == NULL) return PAW_TRUE;

    struct IrType **pbound;
    K_LIST_FOREACH(bounds, pbound) {
        if (!implements_trait(C, type, *pbound)) {
            return PAW_FALSE;
        }
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

static struct IrType *substitute_self(struct Compiler *C, struct IrType *trait, struct IrType *adt, struct IrType *method)
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

static void ensure_methods_match(struct Compiler *C, struct IrType *adt, struct HirFuncDecl *adt_method, struct IrType *trait, struct HirTraitDecl *trait_decl, struct HirFuncDecl *trait_method)
{
    if (adt_method->is_pub != trait_method->is_pub) {
        TYPE_ERROR(C, "visibility mismatch (expected %s visibility on method '%s')",
                trait_method->is_pub ? "public" : "private", adt_method->name->text);
    }
    struct IrType *a = pawIr_get_type(C, adt_method->hid);
    struct IrType *b = pawIr_get_type(C, trait_method->hid);
    if (trait_decl->generics != NULL) {
        b = pawP_instantiate_method(C, HIR_CAST_DECL(trait_decl),
                IR_TYPE_SUBTYPES(trait), HIR_CAST_DECL(trait_method));
    }
    b = substitute_self(C, trait, adt, b);
    if (!pawU_equals(C->U, a, b)) {
        TYPE_ERROR(C, "trait method incompatible with implementation");
    }
}

DEFINE_MAP(struct Compiler, MethodMap, pawP_alloc, p_hash_ptr, p_equals_ptr, String *, struct HirFuncDecl *)

static void ensure_trait_implemented(struct Compiler *C, struct HirTraitDecl *trait_decl, MethodMap *methods, struct IrType *adt, struct IrType *trait)
{
    struct HirDecl **pdecl;
    K_LIST_FOREACH(trait_decl->methods, pdecl) {
        struct HirFuncDecl *trait_method = HirGetFuncDecl(*pdecl);
        struct HirFuncDecl **pmethod = MethodMap_get(C, methods, trait_method->name);
        if (pmethod == NULL) {
            NAME_ERROR(C, "trait method '%s' not implemented",
                    trait_method->name->text);
        }
        ensure_methods_match(C, adt, *pmethod, trait, trait_decl, trait_method);
    }
}

void pawP_validate_adt_traits(struct Compiler *C, struct HirAdtDecl *d)
{
    struct IrType *adt = pawIr_get_type(C, d->hid);
    struct IrTypeList *traits = pawP_query_traits(C, adt);
    if (traits == NULL) return;
    MethodMap *map = MethodMap_new(C);

    struct HirDecl **pdecl;
    K_LIST_FOREACH(d->methods, pdecl) {
        struct HirFuncDecl *method = HirGetFuncDecl(*pdecl);
        MethodMap_insert(C, map, method->name, method);
    }

    struct IrType **ptype;
    K_LIST_FOREACH(traits, ptype) {
        struct HirTraitDecl *trait = HirGetTraitDecl(
                pawHir_get_decl(C, IR_TYPE_DID(*ptype)));
        ensure_trait_implemented(C, trait, map, adt, *ptype);
    }

    MethodMap_delete(C, map);
}

