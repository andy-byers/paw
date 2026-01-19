// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "impl.h"
#include "ir_type.h"
#include "type_folder.h"
#include "unify.h"

// t: T
// t.method()
//
// Search for "method" in traits from bounds on type T
// Substitute [Trait/T]method
//
// t: Adt
// t.method()
//
// Search for "method" in impl blocks defined on type Adt (look in impl blocks whose types are compatible with Adt)
//
// impl<T> Trait<T> for Type<T> {
//   fn f(t: T) -> T {t}
// }
//
// let t = Type;
// t.f(t);
//

#warning remove
#include"stdio.h"

static Str const *name_of_method(struct Compiler *C, IrType *type)
{
    struct IrSignature const *t = IrGetSignature(type);
    struct IrFnDef const *def = pawIr_get_fn_def(C, t->did);
    return def->name;
}

static struct Instantiation instantiate_impl_assoc(struct Compiler *C, struct SourceLoc loc, IrType *base, IrType *type, IrTypeList *binder, IrType *method)
{
    if (IrIsTraitObj(base)) {
        method = pawIr_substitute_self(C, base, type, method);
        base = type;
    }

    struct Substitution subst = {0};
    if (binder != NULL) {
        subst.types = pawU_new_unknowns(C->U, loc, binder);
        subst.generics = binder;

//        struct IrTypeFolder F;
//        struct Substitution subst;
//        IrTypeList *type_args = pawU_new_unknowns(C->U, loc, binder);
//        pawP_init_substitution_folder(&F, C, &subst, binder, type_args);
//
//        struct IrSignature *f = IrGetSignature(method);
//        IrTypeList *params = pawIr_fold_type_list(&F, f->params);
//        IrType *result = pawIr_fold_type(&F, f->result);
//        base = pawIr_fold_type(&F, base);
//
//        method = pawIr_new_signature(C, f->did, f->types, params, result);

        base = pawP_substitute(C, loc, base, subst);
        method = pawP_substitute(C, loc, method, subst);
//        IrGetSignature(method)->self = type;
    }

    if (pawU_unify(C->U, base, type) != 0)
        pawErr_generic_error(ENV(C), C->modname, loc, "");

    IrGetSignature(method)->self = type;
    return (struct Instantiation){
        .inst = method,
        .subst = subst,
    };
}

static paw_Bool find_method_in_list_impl(struct Compiler *C, IrType *base, IrType *self, IrGenericDefs *generics, IrTypeList *methods, Str const *name, struct Instantiation *out)
{
    struct SourceLoc const loc = {0};

    IrTypeList *binder = NULL;
    if (generics != NULL) {
        binder = IrTypeList_new(C);
        IrTypeList_reserve(C, binder, generics->count);
        struct IrGenericDef *const *p;
        K_LIST_FOREACH (generics, p) {
            IrType *g = pawIr_new_generic(C, (*p)->did, (*p)->bounds);
            IrTypeList_push(C, binder, g);
        }
    }

    IrType *const *p;
    K_LIST_FOREACH (methods, p) {
        if (pawS_eq(name, name_of_method(C, *p))) {
            *out = instantiate_impl_assoc(C, loc, base, self, binder, *p);
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static IrType *find_method_in_list(struct Compiler *C, IrType *self, IrTypeList *methods, Str const *name)
{
    struct SourceLoc const loc = {0};

    IrType *const *type_ptr;
    K_LIST_FOREACH (methods, type_ptr) {
        if (pawS_eq(name, name_of_method(C, *type_ptr)))
            return pawP_instantiate_assoc(C, loc, self, *type_ptr).inst;
    }
    return NULL;
}

// Replace generics from the impl block binder with inference types in the
// context of the receiver type
static IrType *instantiate_impl(struct Compiler *C, struct SourceLoc loc, struct IrImpl const *impl)
{
    if (impl->generics == NULL)
        return impl->type;

    // get a copy of the impl block binder
    struct IrGenericDef *const *p;
    IrTypeList *generics = IrTypeList_new(C);
    IrTypeList_reserve(C, generics, impl->generics->count);
    K_LIST_FOREACH (impl->generics, p) {
        IrType *generic = pawIr_get_def_type(C, (*p)->did);
        IrTypeList_push(C, generics, generic);
    }

    IrTypeList *types = pawU_new_unknowns(C->U, loc, generics);
    struct Substitution const subst = {generics, types};
    return pawP_substitute(C, loc, impl->type, subst);
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

DEFINE_LIST(struct Compiler, InstantiationList, struct Instantiation)

struct Instantiation *pawP_find_method(struct Compiler *C, IrType *self, Str *name)
{
    struct SourceLoc const loc = {0}; // TODO
    paw_assert(!IrIsTraitObj(self));

//    if (IrIsTraitObj(self)) {
//        struct IrTraitObj const *t = IrGetTraitObj(self);
//        struct IrTraitDef const *def = pawIr_get_trait_def(C, t->did);
//        return find_method_in_list(C, self, def->methods, name);
//    }

#define ADD_APPLICABLE_METHODS(Base_, Self_, Binder_, Methods_) do { \
            struct Instantiation inst; \
            if (find_method_in_list_impl(C, Base_, Self_, Binder_, Methods_, name, &inst)) \
                InstantiationList_push(C, result, inst); \
        } while (0)

    InstantiationList *result = InstantiationList_new(C);
    if (IrIsGeneric(self)) {
        // The receiver is a generic type. Search in traits specified by bounds on
        // the generic type parameter.
        struct IrGeneric const *t = IrGetGeneric(self);

        IrType *const *bound_ptr;
        K_LIST_FOREACH (t->bounds, bound_ptr) {
            struct IrTraitObj const *t = IrGetTraitObj(*bound_ptr);
            struct IrTraitDef const *def = pawIr_get_trait_def(C, t->did);
            IrType *base = pawIr_get_def_type(C, def->did);
            ADD_APPLICABLE_METHODS(base, self, def->generics, def->methods);
        }
    } else {
        // The receiver is a nominal type. Search in impl blocks whose "Self" is
        // compatible with the receiver type "self".
        struct IrAdt const *t = IrGetAdt(self);

        // search inherent implementations
        {
            IrImplList *const *impls_ptr = IrImplOwners_get(C, C->impls.inherent, t->did);
            if (impls_ptr != NULL) {
                struct IrImpl *const *p;
                K_LIST_FOREACH (*impls_ptr, p) {
                    struct IrImpl const *impl = *p;
                    if (check_impl(C, loc, self, impl) == 0)
                        ADD_APPLICABLE_METHODS(impl->type, self, impl->generics, impl->methods);
                }
            }
        }

        // search trait implementations
        {
            IrImplList *const *impls_ptr = IrImplOwners_get(C, C->impls.trait, t->did);
            if (impls_ptr != NULL) {
                struct IrImpl *const *p;
                K_LIST_FOREACH (*impls_ptr, p) {
                    struct IrImpl const *impl = *p;
                    if (check_impl(C, loc, self, impl) == 0)
                        ADD_APPLICABLE_METHODS(impl->type, self, impl->generics, impl->methods);
                }
            }
        }

        // search blanket implementations
        {
            struct IrImpl *const *p;
            K_LIST_FOREACH (C->impls.blanket, p)
                ADD_APPLICABLE_METHODS((*p)->type, self, (*p)->generics, (*p)->methods);
        }
    }

    // TODO: return error indicator
    if (result->count > 1)
        pawErr_generic_error(ENV(C), C->modname, loc, "multiple applicable methods");
    if (result->count == 0) return NULL;
    // allocate return value
    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = K_LIST_FIRST(result);
    return out;

#undef ADD_APPLICABLE_METHODS
}
