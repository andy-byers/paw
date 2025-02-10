// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO:
// (1) each bound must be unique in "bounds"
// (2) if a trait is implemented for a type, the impl block must contain all non-defaulted
//     methods listed in the trait def. Copy defaulted trait methods into the impl block so
//     they can be handled identically to provided methods later on. Then all that has to
//     be checked is that the type implements the trait.
// (3) need "is non-proper subset of" query added to unification module

#include "unify.h"

static struct IrTypeList *query_traits(struct Compiler *C, struct IrType *type, paw_Bool create_if_missing)
{
    if (IrIsGeneric(type)) return IrGetGeneric(type)->bounds;
    if (IrIsInfer(type)) return IrGetInfer(type)->bounds;

    struct TODO_Implements *pimpl;
    K_LIST_FOREACH(C->traits, pimpl) {
        if (pawU_is_compat(C->U, pimpl->type, type)) return pimpl->traits;
    }
    if (create_if_missing) {
        struct IrTypeList *traits = pawIr_type_list_new(C);
        K_LIST_PUSH(C, C->traits, ((struct TODO_Implements){
                        .traits = traits,
                        .type = type,
                    }));
        return traits;
    }
    return NULL;
}

struct IrTypeList *pawP_query_traits(struct Compiler *C, struct IrType *type)
{
    return query_traits(C, type, PAW_FALSE);
}

void pawP_add_trait_impl(struct Compiler *C, struct IrType *type, struct IrType *trait)
{
    struct IrTypeList *traits = query_traits(C, type, PAW_TRUE);
    K_LIST_PUSH(C, traits, trait); // TODO: assuming type/trait pair is unique
}

static paw_Bool implements_trait(struct Compiler *C, struct IrType *type, struct IrType *trait)
{
    struct IrType **ptrait;
    struct IrTypeList *traits = query_traits(C, type, PAW_TRUE);
    K_LIST_FOREACH(traits, ptrait) {
        if (pawU_is_compat(C->U, *ptrait, trait)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

// trait Debug {}
// impl Debug for int {}
// impl Debug for [int] {}
//
// [int]: Debug
// int: Debug

// Return 1 if the given "type" satisfies the given trait "bounds", 0 otherwise
// Trait bounds on "type" must be a (non-proper) superset of "bounds".
paw_Bool pawP_satisfies_bounds(struct Compiler *C, struct IrType *type, struct IrTypeList *bounds)
{
    if (bounds == NULL) return PAW_TRUE;

    struct IrType **pbound;
    K_LIST_FOREACH(bounds, pbound) {
        if (!implements_trait(C, type, *pbound)) return PAW_FALSE;
    }
    return PAW_TRUE;
}

