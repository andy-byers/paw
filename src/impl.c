// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "impl.h"
#include "ir_type.h"
#include "solve.h"
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

#define TODO (struct SourceLoc){0}

struct Candidate {
    IrType *context;
    DeclId fn_did;
};

DEFINE_LIST(struct Compiler, Candidates, struct Candidate)

static IrType *type_of_generic(struct Compiler *C, struct IrGenericDef const *g)
{
    return pawIr_new_generic(C, g->did);
}

static Str const *name_of_method(struct Compiler *C, IrType *type)
{
    struct IrSignature const *t = IrGetSignature(type);
    struct IrFnDef const *def = pawIr_get_fn_def(C, t->did);
    return def->name;
}

static struct Instantiation instantiate_impl_assoc(struct Compiler *C, IrType *base, IrType *type, IrType *method)
{
//TODO    if (IrIsTraitObj(base)) {
//TODO        method = pawIr_substitute_self(C, base, type, method);
//TODO        base = type;
//TODO    }

    method = pawIr_solver_instantiate_type(C->S, IR_TYPE_DID(method));
    struct Substitution const subst = {
        pawIr_get_generic_types(C, IR_TYPE_DID(method)),
        IR_TYPE_SUBTYPES_(method),
    };

    return (struct Instantiation){
        .inst = method,
        .subst = subst,
    };
}

static paw_Bool find_method_in_list_impl(struct Compiler *C, IrType *base, IrType *self, IrTypeList *methods, Str const *name, struct Candidate *out)
{
    IrType *const *p;
    K_LIST_FOREACH (methods, p) {
        if (pawS_eq(name, name_of_method(C, *p))) {
            out->fn_did = IR_TYPE_DID(*p);
            out->context = base;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

// Replace generics from the impl block binder with inference types in the
// context of the receiver type
static IrType *instantiate_impl(IrSolver *S, struct IrImpl const *impl)
{
    return pawIr_solver_instantiate_impl(S, impl->did).type;
}

// Learn about predicates from the ADT definition
static void add_context_preconditions(struct Compiler *C, IrSolver *S, IrType *self)
{
    IrTypeList *args = IR_TYPE_SUBTYPES_(self);
    if (args != NULL) {
        IrTypeList *params = pawIr_get_generic_types(C, IR_TYPE_DID(self));
        struct Substitution const subst = {params, args};

        IrType *const *p, *const *a;
        K_LIST_ZIP (params, p, args, a) {
            IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(*p));
            if (bounds != NULL) {
                K_LIST_XFOREACH (bounds, IrTrait *const, b) {
                    IrTrait *t = pawP_substitute_trait(C, TODO, *b, subst);
                    pawIr_solver_add_precondition(S, *a, t);
                }
            }
        }
    }
}

static paw_Bool impl_is_compatible(struct Compiler *C, IrType *self, struct IrImpl const *impl)
{
    // save the current position in the unification table
    int const save = pawU_current_position(C->U);
    IrSolver *S = pawIr_push_solver(C);
    add_context_preconditions(C, S, self);

    IrType *context = instantiate_impl(S, impl);
    paw_Bool const matches =
        pawU_unify(C->U, self, context) == 0
        // only exclude an impl block from search if there is a trait obligation that
        // is known to be unsatisfiable (pending obligations might be solved later,
        // once more types have been inferred)
        && pawIr_solver_solve(S) >= 0;

    // undo all changes to the environment made in this function
    pawU_undo_unifications(C->U, save);
    pawIr_solver_rollback(S);
    pawIr_pop_solver(C);
    return matches;
}

struct Instantiation *pawP_find_method(struct Compiler *C, IrType *self, Str *name)
{
#define ADD_APPLICABLE_METHODS(Base_, Self_, Methods_) do { \
            struct Candidate c_; \
            if (find_method_in_list_impl(C, Base_, Self_, Methods_, name, &c_)) \
                Candidates_push(C, candidates, c_); \
        } while (0)

    Candidates *candidates = Candidates_new(C);
    if (IrIsGeneric(self)) {
        // The receiver is a generic type. Search in traits specified by bounds on
        // the generic type parameter.
        IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(self));
        if (bounds != NULL) {
            K_LIST_XFOREACH (bounds, IrTrait *const, p) {
                struct IrTraitDef const *def = pawIr_get_trait_def(C, (*p)->did);
                IrType *base = pawIr_get_def_type(C, (*p)->did);
                ADD_APPLICABLE_METHODS(base, self, def->methods);
            }
        }
    } else {
        // The receiver is a concrete type. Search in impl blocks whose "Self" is
        // compatible with the receiver type "self".

        // search inherent implementations
        K_LIST_XFOREACH (C->impls.inherent, DeclId const, p) {
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            if (impl_is_compatible(C, self, impl))
                ADD_APPLICABLE_METHODS(impl->type, self, impl->methods);
        }

        // search trait implementations
        K_LIST_XFOREACH (C->impls.trait, DeclId const, p) {
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            if (impl_is_compatible(C, self, impl))
                ADD_APPLICABLE_METHODS(impl->type, self, impl->methods);
        }

        // search blanket implementations
        K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            ADD_APPLICABLE_METHODS(impl->type, self, impl->methods);
        }
    }

    if (candidates->count == 0)
        return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        pawErr_generic_error(ENV(C), C->modname, TODO, "multiple applicable methods");

    // allocate return value
    struct Candidate const result = Candidates_first(candidates);
    IrType *method = pawIr_solver_instantiate_type(C->S, result.fn_did);

    // apply information known about the context type
    IrType *context = pawIr_get_context(C, method);
    int const unused = pawU_unify(C->U, context, self);
    paw_assert(unused == 0); PAW_UNUSED(unused);
    method = pawU_normalize(C->U, method);

    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.generics = pawIr_get_generic_types(C, result.fn_did),
        .subst.types = IR_TYPE_SUBTYPES_(method),
        .inst = method,
    };
    return out;

#undef ADD_APPLICABLE_METHODS
}

