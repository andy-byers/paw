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

// TODO: functionality of this and below function can be merged, as well as versions in typeck.c
static void instantiate_impl_trait(struct Compiler *C, struct SourceLoc loc, struct IrImpl const *impl, IrType **type_out, IrType **trait_out)
{
    if (impl->generics == NULL) {
        *type_out = impl->type;
        *trait_out = impl->trait;
        return;
    }

    struct IrGenericDef *const *p;
    IrTypeList *generics = IrTypeList_new(C);
    IrTypeList_reserve(C, generics, impl->generics->count);
    K_LIST_FOREACH (impl->generics, p) {
        IrType *generic = pawIr_get_def_type(C, (*p)->did);
        IrTypeList_push(C, generics, generic);
    }

    struct IrTypeFolder F;
    struct Substitution subst;
    IrTypeList *after = pawU_new_unknowns(C->U, loc, generics);
    pawP_init_substitution_folder(&F, C, &subst, generics, after);
    *type_out = pawIr_fold_type(&F, impl->type);
    *trait_out = pawIr_fold_type(&F, impl->trait);
}

static IrType *instantiate_impl(struct Compiler *C, struct SourceLoc loc, struct IrImpl const *impl)
{
    if (impl->generics == NULL)
        return impl->type;

    struct IrGenericDef *const *p;
    IrTypeList *generics = IrTypeList_new(C);
    IrTypeList_reserve(C, generics, impl->generics->count);
    K_LIST_FOREACH (impl->generics, p) {
        IrType *generic = pawIr_get_def_type(C, (*p)->did);
        IrTypeList_push(C, generics, generic);
    }

    struct IrTypeFolder F;
    struct Substitution subst;
    IrTypeList *after = pawU_new_unknowns(C->U, loc, generics);
    pawP_init_substitution_folder(&F, C, &subst, generics, after);
    return pawIr_fold_type(&F, impl->type);
}

static int check_impl(struct Compiler *C, struct SourceLoc loc, IrType *self, struct IrImpl const *impl)
{
    // save the current position in the unification table
    int const save = pawU_current_position(C->U);

    IrType *type = instantiate_impl(C, loc, impl);
    IrType *copy = pawIr_clone_type(C, self);
    int const status = pawU_unify(C->U, type, copy);

    // erase all inference variables created in this function
    pawU_load_position(C->U, save);
    return status;
}

static IrTypeList *collect_types(struct Compiler *C, IrTypeList *types)
{
    IrTypeList *result = IrTypeList_new(C);
    if (types != NULL) {
        struct IrType *const *p;
        K_LIST_FOREACH (types, p) {
            IrType *type = pawP_generalize(C, (struct SourceLoc){0}, *p);
            IrTypeList_push(C, result, type);
        }
    }
    return result;
}

static IrTypeList *collect_trait_impls(struct Compiler *C, IrType *self)
{
    int const modno = (int)IR_TYPE_DID(self).modno;
    IrTypeList *result = IrTypeList_new(C);
    struct SourceLoc const loc = {0};

    // add instantiated trait type from trait impl blocks where the context type is
    // compatible with "self"
    {
        IrImplList *const *impls_ptr = IrImplOwners_get(C, C->impls.trait, IR_TYPE_DID(self));
        if (impls_ptr != NULL) {
            struct IrImpl *const *p;
            K_LIST_FOREACH (*impls_ptr, p) {
                struct IrImpl const *impl = *p;
                if (check_impl(C, loc, self, impl) == 0) {
                    IrType *type, *trait;
                    instantiate_impl_trait(C, loc, impl, &type, &trait);
                    unify(C, modno, loc, type, self);
                    IrTypeList_push(C, result, trait);
                }
            }
        }
    }

    // add instantiated trait types from blanket impl blocks
    {
        struct IrImpl *const *p;
        K_LIST_FOREACH (C->impls.blanket, p) {
            struct IrImpl const *impl = *p;
            // TODO: need to check generic bounds
//            if (check_impl(C, loc, self, impl) == 0) {
                IrType *type, *trait;
                instantiate_impl_trait(C, loc, impl, &type, &trait);
                unify(C, modno, loc, type, self);
                IrTypeList_push(C, result, trait);
//            }
        }
    }
    return result;
}

paw_Bool pawIr_implements_trait(struct Compiler *C, IrType *type, IrType *trait)
{
    IrTypeList *impls;
    if (IrIsGeneric(type)) {
        impls = collect_types(C, IrGetGeneric(type)->bounds);
    } else if (IrIsInfer(type)) {
        impls = collect_types(C, IrGetInfer(type)->bounds);
    } else if (IrIsAdt(type)) {
        impls = collect_trait_impls(C, type);
    } else {
        return PAW_FALSE;
    }

    DeclId const did = IrGetTraitObj(trait)->did;

    IrType *const *p;
    K_LIST_FOREACH (impls, p) {
        if (IR_TYPE_DID(*p).value == did.value) {
//            int const save = pawU_current_position(C->U);
            if (pawU_unify(C->U, trait, *p) == 0)
                return PAW_TRUE;
//            pawU_load_position(C->U, save);
        }
    }

    return PAW_FALSE;
}

paw_Bool pawP_satisfies_bounds(struct Compiler *C, IrType *type, IrTypeList *bounds)
{
    if (bounds != NULL) {
        IrType *const *pbound;
        K_LIST_FOREACH (bounds, pbound) {
            if (!pawIr_implements_trait(C, type, *pbound))
                return PAW_FALSE;
        }
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
