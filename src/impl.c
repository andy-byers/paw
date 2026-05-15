// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "impl.h"
#include "ir_type.h"
#include "solve.h"
#include "type_folder.h"
#include "unify.h"

#define TODO (struct SourceSpan){0}

struct QueryState {
    int save_point;
    IrSolver *S;
};

static struct QueryState start_query(struct Compiler *C)
{
    int const save_point = pawU_current_position(C->U);
    IrSolver *S = pawIr_push_solver(C);
    return (struct QueryState){
        .save_point = save_point,
        .S = S,
    };
}

void finish_query(struct Compiler *C, struct QueryState q)
{
    pawU_undo_unifications(C->U, q.save_point);
    pawIr_solver_rollback(q.S);
    pawIr_pop_solver(C);
}

struct Candidate {
    struct IrImpl const *impl;
    DeclId target;
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

static paw_Bool find_method_in_list(struct Compiler *C, IrTypeList *methods, Str const *name, struct Candidate *out)
{
    IrType *const *p;
    K_LIST_FOREACH (methods, p) {
        if (pawS_eq(name, name_of_method(C, *p))) {
            out->target = IR_TYPE_DID(*p);
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
static void add_context_predicates(struct Compiler *C, IrSolver *S, IrType *self)
{
    IrGenericArgs *args = IR_GENERIC_ARGS(self);
    if (args != NULL) { // TODO: should be non-NULL
        IrGenericArgs *params = pawIr_get_generic_args(C, IR_TYPE_DID(self));
        struct Substitution const subst = {params, args};

        IrGenericArg const *p, *a;
        K_LIST_ZIP (params, p, args, a) {
            if (IrGenericArg_is_type(*p)) {
                paw_assert(IrGenericArg_is_type(*a));
                IrType *pt = IrGenericArg_get_type(*p);
                IrType *at = IrGenericArg_get_type(*a);
                IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(pt));
                if (bounds != NULL) {
                    K_LIST_XFOREACH (bounds, IrTrait *const, b) {
                        IrTrait *t = pawP_substitute_trait(C, *b, subst);
                        pawIr_solver_add_predicate(S, at, t, (struct IrObligationCause){0});
                    }
                }
            }
        }
    }
}

static paw_Bool types_are_compatible(struct Compiler *C, IrType *self, IrType *context)
{
    return pawU_unify(C->U, self, context) == 0
        // only exclude an impl block from search if there is a trait obligation that
        // is known to be unsatisfiable (pending obligations might be solved later,
        // once more types have been inferred)
        && pawIr_solver_solve(C->S).status == IR_SOLVER_OK;
}

static paw_Bool impl_is_compatible(struct Compiler *C, IrType *self, struct IrImpl const *impl)
{
    // save the current position in the unification table
    int const save = pawU_current_position(C->U);
    IrSolver *S = pawIr_push_solver(C);
    add_context_predicates(C, S, self);

    IrType *context = instantiate_impl(S, impl);
    paw_Bool const matches =
        pawU_unify(C->U, self, context) == 0
        // only exclude an impl block from search if there is a trait obligation that
        // is known to be unsatisfiable (pending obligations might be solved later,
        // once more types have been inferred)
        && pawIr_solver_solve(S).status == IR_SOLVER_OK;

    // undo all changes to the environment made in this function
    pawU_undo_unifications(C->U, save);
    pawIr_solver_rollback(S);
    pawIr_pop_solver(C);
    return matches;
}

struct Instantiation *pawP_find_method(struct Compiler *C, IrType *self, Str const *name)
{
#define ADD_APPLICABLE_METHODS(ImplDid_) do { \
            struct Candidate c_; \
            struct IrImpl const *impl_def = pawIr_get_impl_def(C, ImplDid_); \
            if (find_method_in_list(C, impl_def->methods, name, &c_)) { \
                c_.impl = impl_def; \
                Candidates_push(C, candidates, c_); \
            } \
        } while (0)

    Candidates *candidates = Candidates_new(C);
    if (IrIsProjection(self)) {
        IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(self));
        if (bounds != NULL) {
            K_LIST_XFOREACH (bounds, IrTrait *const, ptrait) {
                struct IrTraitDef const *def = pawIr_get_trait_def(C, (*ptrait)->did);
                K_LIST_XFOREACH (def->methods, IrType *const, pmethod) {
                    struct IrFnDef const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(*pmethod));
                    if (pawS_eq(fn_def->name, name)) {
                        Candidates_push(C, candidates, (struct Candidate){
                                    .target = fn_def->did,
                                });
                    }
                }
            }
        }
    } else {
        paw_assert(!IrIsGeneric(self));
        // The receiver is a concrete type. Search in impl blocks whose "Self" is
        // compatible with the receiver type "self".

        // search inherent implementations
        K_LIST_XFOREACH (C->impls.inherent, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            if (types_are_compatible(C, self, inst.type))
                ADD_APPLICABLE_METHODS(*p);
            finish_query(C, q);
        }

        // search trait implementations
        K_LIST_XFOREACH (C->impls.trait, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            if (types_are_compatible(C, self, inst.type))
                ADD_APPLICABLE_METHODS(*p);
            finish_query(C, q);
        }

        // search blanket implementations
        K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
            ADD_APPLICABLE_METHODS(*p);
        }
    }

    if (candidates->count == 0)
        return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        pawErr_generic_error(ENV(C), C->modname, TODO, "multiple applicable methods");

    // allocate return value
    struct Candidate const result = Candidates_first(candidates);
    IrType *method = pawIr_solver_instantiate_type(C->S, result.target);

    // TODO: may need to prove constraints from here later once fn args have been unified
//    struct IrImplInstance const impl = pawIr_solver_instantiate_impl(C->S, result.impl->did);

    // apply information known about the context type
    IrType *context = pawIr_get_context(C, method);
//    pawU_unify_unchecked(C->U, impl.type, self);
    pawU_unify_unchecked(C->U, context, self);
    method = pawU_normalize(C->U, method);

    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.params = pawIr_get_generic_args(C, result.target),
        .subst.args = IR_GENERIC_ARGS(method),
        .inst = method,
    };
    return out;

#undef ADD_APPLICABLE_METHODS
}

static paw_Bool traits_are_compatible(struct Compiler *C, IrSolver *S, IrTrait *a, IrTrait *b)
{
    return pawIr_unify_traits(C, a, b) == 0
        && pawIr_solver_solve(S).status == IR_SOLVER_OK;
}

static paw_Bool impls_are_compatible(struct Compiler *C, IrType *self, IrTrait *trait, struct IrImpl const *impl)
{
    paw_Bool matches = PAW_FALSE;
    struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, impl->did);
    if (pawU_unify(C->U, inst.type, self) == 0
            && pawIr_unify_traits(C, inst.trait, trait) == 0) {
        pawIr_solver_add_obligations_from(C->S, impl->did, inst.args);
        struct IrSolverResult const result = pawIr_solver_solve(C->S);
        matches = result.status == IR_SOLVER_OK
            && result.num_unsolved == 0;
    }
    pawIr_solver_rollback(C->S);

    return matches;
}

struct Instantiation *pawP_find_trait_method(struct Compiler *C, IrType *self, IrTrait *trait, Str const *name)
{
#define ADD_APPLICABLE_METHODS(Methods_) do { \
            struct Candidate c_; \
            if (find_method_in_list(C, Methods_, name, &c_)) \
                Candidates_push(C, candidates, c_); \
        } while (0)

    Candidates *candidates = Candidates_new(C);
    if (IrIsGeneric(self)) {
        // The receiver is a generic type. Search in traits specified by bounds on
        // the generic type parameter.
        IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(self));
        if (bounds != NULL) {
            K_LIST_XFOREACH (bounds, IrTrait *const, p) {
                struct QueryState const q = start_query(C);
                // TODO: replace generics w/ inference vars in p? e.g. in fn f<T: Trait<X>, X>(), Trait<X> => Trait<_>
                if (traits_are_compatible(C, q.S, trait, *p)) {
                    struct IrTraitDef const *def = pawIr_get_trait_def(C, (*p)->did);
                    ADD_APPLICABLE_METHODS(def->methods);
                }
                finish_query(C, q);
            }
        }
    } else if (IrIsProjection(self)) {
        IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(self));
        if (bounds != NULL) {
            K_LIST_XFOREACH (bounds, IrTrait *const, p) {
                struct QueryState const q = start_query(C);
                // TODO: instantiate p? e.g. in fn f<T: Trait<X>, X>(), Trait<X> => Trait<_>
                if (traits_are_compatible(C, q.S, trait, *p)) {
                    struct IrTraitDef const *def = pawIr_get_trait_def(C, (*p)->did);
                    ADD_APPLICABLE_METHODS(def->methods);
                }
                finish_query(C, q);
            }
        }
    } else {
        // The receiver is a concrete type. Search in impl blocks whose "Self" is
        // compatible with the receiver type "self".

        // search trait implementations
        K_LIST_XFOREACH (C->impls.trait, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            if (impls_are_compatible(C, self, trait, impl)) {
                struct IrImpl const *def = pawIr_get_impl_def(C, *p);
                ADD_APPLICABLE_METHODS(def->methods);
            }
            finish_query(C, q);
        }

        // search blanket implementations
        K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            if (impl->trait != NULL && impls_are_compatible(C, self, trait, impl)) {
                struct IrImpl const *def = pawIr_get_impl_def(C, *p);
                ADD_APPLICABLE_METHODS(def->methods);
            }
            finish_query(C, q);
        }
    }

    if (candidates->count == 0)
        return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        pawErr_generic_error(ENV(C), C->modname, TODO, "multiple applicable methods");

    // allocate return value
    struct Candidate const result = Candidates_first(candidates);

    IrType *method = pawIr_solver_instantiate_type(C->S, result.target);
    IrTrait *result_trait = pawIr_get_trait_context(C, method);
    pawIr_unify_traits_unchecked(C, trait, result_trait);

    // apply information known about the context type
    IrType *context = pawIr_get_context(C, method);
    pawU_unify_unchecked(C->U, context, self);
    IrTrait *trait_context = pawIr_get_trait_context(C, method);
    pawIr_unify_traits_unchecked(C, trait_context, trait);
    method = pawU_normalize(C->U, method);

    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.params = pawIr_get_generic_args(C, result.target),
        .subst.args = IR_GENERIC_ARGS(method),
        .inst = method,
    };
    return out;

#undef ADD_APPLICABLE_METHODS
}

static Str const *name_of_type(struct Compiler *C, IrType *type)
{
    struct IrSignature const *t = IrGetSignature(type);
    struct IrFnDef const *def = pawIr_get_fn_def(C, t->did);
    return def->name;
}

static paw_Bool find_type_in_list(IrAssocItems *items, Str const *name, struct Candidate *out)
{
    K_LIST_XFOREACH (items, struct IrAssocItem *const, p) {
        if (pawS_eq(name, (*p)->name)) {
            out->target = (*p)->did;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

struct Instantiation *pawIr_find_assoc_type_generic(struct Compiler *C, IrType *self, Str const *name)
{
#define ADD_APPLICABLE_TYPES(Trait_, Methods_) do { \
            struct Candidate c_; \
            if (find_type_in_list(Methods_, name, &c_)) { \
                Candidates_push(C, candidates, c_); \
                trait = Trait_; \
            } \
        } while (0)

    IrTrait *trait;
    paw_assert(IrIsGeneric(self));
    Candidates *candidates = Candidates_new(C);
    {
        // The receiver is a generic type. Search in traits specified by bounds on
        // the generic type parameter.
        IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(self));
        if (bounds == NULL) bounds = IrTraitList_new(C);
//        if (trait != NULL) IrTraitList_push(C, bounds, trait);
        if (bounds != NULL) {
            K_LIST_XFOREACH (bounds, IrTrait *const, p) {
                struct IrTraitDef const *def = pawIr_get_trait_def(C, (*p)->did);
                ADD_APPLICABLE_TYPES(*p, def->items);
            }
        }
   }

    if (candidates->count == 0)
        return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        pawErr_generic_error(ENV(C), C->modname, TODO, "multiple applicable associated types");

    struct Candidate const result = Candidates_first(candidates);
    IrType *assoc = pawIr_get_def_type(C, result.target);

    struct IrProjection *p = IrGetProjection(assoc);
    IrTrait *trait2 = pawIr_solver_instantiate_trait(C->S, trait->did);
    IrType *first = IrGenericArg_get_type(
            IrGenericArgs_first(trait->args));
    pawU_unify_unchecked(C->U, first, self);
    pawIr_unify_traits_unchecked(C, trait, trait2);
    assoc = pawIr_new_projection(C, self, trait, p->assoc);

    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.params = pawIr_get_generic_args(C, result.target),
        .subst.args = IR_GENERIC_ARGS(assoc),
        .inst = assoc,
    };
    return out;

#undef ADD_APPLICABLE_TYPES
}

struct Instantiation *pawIr_find_assoc_type_projection(struct Compiler *C, IrType *self, IrTrait *trait, Str const *name)
{
#define ADD_APPLICABLE_TYPES(Impl_, Methods_) do { \
            struct Candidate c_; \
            if (find_type_in_list(Methods_, name, &c_)) { \
                Candidates_push(C, candidates, c_); \
                target_impl = Impl_; \
            } \
        } while (0)

    struct IrImpl const *target_impl;
    Candidates *candidates = Candidates_new(C);
    {
        // search trait implementations
        K_LIST_XFOREACH (C->impls.trait, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            if (traits_are_compatible(C, C->S, trait, inst.trait)
                    && types_are_compatible(C, self, inst.type))
                ADD_APPLICABLE_TYPES(impl, impl->items);
            finish_query(C, q);
        }

        // search blanket implementations
        K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            if (impl->trait != NULL
                    && traits_are_compatible(C, C->S, trait, inst.trait))
                ADD_APPLICABLE_TYPES(impl, impl->items);
            finish_query(C, q);
        }
    }

    if (candidates->count == 0)
        return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        pawErr_generic_error(ENV(C), C->modname, TODO, "multiple applicable associated types");

    // allocate return value
    struct Candidate const result = Candidates_first(candidates);
    IrType *assoc = pawIr_get_def_type(C, result.target);

    self = pawIr_remove_indirection(C, self);
    IrType *impl_type = pawIr_remove_indirection(C, target_impl->type);
    if (IrIsGeneric(self)) {
        assoc = pawIr_solver_instantiate_type_with(C->S, IR_TYPE_DID(self), IR_GENERIC_ARGS(self));
    } else if (IrIsAdt(self)){
        struct Substitution const subst = {IR_GENERIC_ARGS(impl_type), IR_GENERIC_ARGS(self)};
        assoc = pawP_substitute(C, assoc, subst);
    }

    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.params = pawIr_get_generic_args(C, result.target),
        .subst.args = IR_GENERIC_ARGS(assoc),
        .inst = assoc,
    };
    return out;

#undef ADD_APPLICABLE_TYPES
}

