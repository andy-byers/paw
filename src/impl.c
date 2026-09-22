// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "impl.h"
#include "ir_type.h"
#include "solve.h"
#include "trait.h"
#include "unify.h"

#warning
#include"stdio.h"

#define LOOKUP_ERROR(C_, Kind_, ...) THROW_ERROR(C_, \
        Kind_, __VA_ARGS__)

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
    pawIr_pop_solver(C);
}

struct Candidate {
    IrGenericArgs *impl_args;
    DeclId impl_did;
    DeclId target;
};

DEFINE_LIST(struct Compiler, Candidates, struct Candidate,)

static Str const *name_of_method(struct Compiler *C, IrType *type)
{
    struct IrSignature const *t = IrGetSignature(type);
    struct IrFnDef const *def = pawIr_get_fn_def(C, t->did);
    return def->name;
}

static IrGenericArgs *clone_args(struct Compiler *C, IrGenericArgs const *args)
{
    IrGenericArgs *rewrite = IrGenericArgs_new(C);
    IrGenericArgs_reserve(C, rewrite, args->count);
    K_LIST_XFOREACH (args, IrGenericArg const, p)
        IrGenericArgs_push(C, rewrite, *p);
    return rewrite;
}

static paw_Bool call_args_match(struct Compiler *C, IrType *method, IrType *self, IrGenericArgs *generic_args, IrTypeList *call_args)
{
    DeclId const did = IR_TYPE_DID(method);
    {
        IrGenericArgs *rewrite = clone_args(C, generic_args);
        IrGenericArgs const *rest = pawIr_get_generic_args(C, did);
        while (rewrite->count < rest->count) {
            paw_Bool is_type_var = IrGenericArg_is_type(IrGenericArgs_get(rest, rewrite->count));
            IrGenericArg const type_arg = is_type_var
                ? IrGenericArg_from_type(pawU_new_type_var(C->U, IR_INFER_TYPE, (struct SourceSpan){0}))
                : IrGenericArg_from_const(pawU_new_const_var(C->U, (struct SourceSpan){0}));
            IrGenericArgs_push(C, rewrite, type_arg);
        }
        generic_args = rewrite;
    }

    method = pawIr_solver_instantiate_type_with(C->S, did, generic_args);
    // TODO: always pass call args
    if (call_args == NULL) {
        IrType *context = pawIr_get_context(C, method);
        return pawU_unify(C->U, context, self) == 0;
    }

    struct IrFnPtr const *fn = IrGetFnPtr(IR_SIGNATURE_FN(C, method));
    if (fn->params->count != call_args->count)
        return PAW_FALSE;

    IrType *const *param, *const *arg;
    K_LIST_ZIP(fn->params, param, call_args, arg) {
        if (pawU_unify(C->U, *param, *arg) != 0)
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool find_method_in_list_(struct Compiler *C, IrTypeList *methods, Str const *name, struct Candidate *out)
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

static paw_Bool solve_query_obligations(struct QueryState q)
{
    // only exclude an impl block from search if there is a trait obligation that
    // is known to be unsatisfiable (ambiguous obligations might be solved later,
    // once more types have been inferred)
    return pawIr_solver_solve(q.S).status != IR_SOLVER_ERROR;
}

static paw_Bool find_method_in_list(struct Compiler *C, struct QueryState q, IrTypeList *methods, IrType *self, Str const *name, IrGenericArgs *generic_args, IrTypeList *call_args, struct Candidate *out)
{
    IrType *const *p;
    K_LIST_FOREACH (methods, p) {
        if (pawS_eq(name, name_of_method(C, *p))
                && call_args_match(C, *p, self, generic_args, call_args)
                && solve_query_obligations(q)) {
            out->target = IR_TYPE_DID(*p);
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static paw_Bool types_are_compatible(struct Compiler *C, struct QueryState q, IrType *self, IrType *context)
{
    return pawU_unify(C->U, self, context) == 0
        && solve_query_obligations(q);
}

static paw_Bool traits_are_compatible(struct Compiler *C, struct QueryState q, IrTrait *a, IrTrait *b)
{
    return pawIr_unify_traits(C, a, b) == 0
        && solve_query_obligations(q);
}

static paw_Bool impls_are_compatible(struct Compiler *C, struct QueryState q, IrType *self, IrTrait *trait, struct IrImplInstance impl)
{
    return pawU_unify(C->U, self, impl.type) == 0
        && pawIr_unify_traits(C, trait, impl.trait) == 0
        && solve_query_obligations(q);
}

static Candidates *select_candidates(
        struct Compiler *C,
        IrType *self,
        Str const *name,
        IrTypeList *call_args,
        struct IrObligationCause cause)
{

#define SELECT_CANDIDATES_FROM(Impls_, Candidates_, Cause_) \
        K_LIST_XFOREACH (Impls_, DeclId const, p) { \
            struct QueryState const q = start_query(C); \
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p); \
            pawIr_solver_add_obligations_from(q.S, *p, inst.args, Cause_); \
            struct Candidate c = {.impl_did = *p, .impl_args = inst.args}; \
            struct IrImpl const *impl_def = pawIr_get_impl_def(C, *p); \
            if (find_method_in_list(C, q, impl_def->methods, self, name, inst.args, call_args, &c)) \
                Candidates_push(C, Candidates_, c); \
            finish_query(C, q); \
        }

    self = pawU_normalize_projections(C->U, self);
    Candidates *candidates = Candidates_new(C);
    IrType *base = pawIr_remove_indirection(C, self);

    if (IrIsProjection(base) || IrIsGeneric(base)) {
        IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(base));
        if (bounds != NULL) {
            // search through bounds on the generic parameter or associated type declaration
            K_LIST_XFOREACH (bounds, IrTrait *const, ptrait) {
                IrTrait const *trait = *ptrait;
                IrGenericArgs *trait_args = clone_args(C, trait->args);
                IrGenericArgs_set(trait_args, 0, IrGenericArg_from_type(base));
                struct QueryState const q = start_query(C);
                pawIr_solver_add_obligations_from(q.S, trait->did, trait_args, cause);
                struct Candidate c = {.impl_did = INVALID_DECL_ID, .impl_args = NULL};
                struct IrTraitDef const *trait_def = pawIr_get_trait_def(C, trait->did);
                if (find_method_in_list(C, q, trait_def->methods, self, name, trait_args, call_args, &c))
                    Candidates_push(C, candidates, c);
                finish_query(C, q);
            }
        }
    } else {
        // The receiver is a concrete type. Search in impl blocks whose "Self" is
        // compatible with the provided receiver type.

        // search inherent implementations
        IrDefs const *inherent_defs = pawIr_inherent_impls_for(C, base);
        SELECT_CANDIDATES_FROM(inherent_defs, candidates, cause);

        // search trait implementations
        IrDefs const *trait_defs = pawIr_trait_impls_for(C, base);
        SELECT_CANDIDATES_FROM(trait_defs, candidates, cause);
    }

    // search blanket implementations
    SELECT_CANDIDATES_FROM(C->impls.blanket, candidates, cause);

    return candidates;

#undef SELECT_CANDIDATES_FROM
}

struct Instantiation *pawP_find_method(struct Compiler *C, IrType *self, Str const *name, IrTypeList *call_args, struct IrObligationCause cause)
{
    Candidates const *candidates = select_candidates(C, self, name, call_args, cause);
    if (candidates->count == 0) return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        LOOKUP_ERROR(C, MultipleApplicableItems,
                .modname = SCAN_STR(C, ""),
                .span = TODO);

    struct Candidate const result = Candidates_first(candidates);
    IrType *method = pawIr_solver_instantiate_type(C->S, result.target);

    if (call_args != NULL) {
        struct IrFnPtr const *fn = IrGetFnPtr(IR_SIGNATURE_FN(C, method));
        paw_assert(fn->params->count == call_args->count);

        IrType *const *param, *const *arg;
        K_LIST_ZIP(fn->params, param, call_args, arg)
            pawU_unify_unchecked(C->U, *param, *arg);
    } else {
        IrType *context = pawIr_get_context(C, method);
        pawU_unify_unchecked(C->U, context, self);
    }

    method = pawU_normalize(C->U, method);

    // allocate return value
    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.params = pawIr_get_generic_args(C, result.target),
        .subst.args = IR_GENERIC_ARGS(method),
        .inst = method,
    };
    if (DECL_ID_EXISTS(result.impl_did))
        pawIr_solver_add_obligations_from(C->S, result.impl_did, result.impl_args, cause);
    pawIr_solver_add_obligations_from(C->S, IR_TYPE_DID(method), IR_GENERIC_ARGS(method), cause);
    return out;
}

struct Instantiation *pawP_find_trait_method(struct Compiler *C, IrType *self, IrTrait *trait, Str const *name, struct IrObligationCause cause)
{
#define ADD_APPLICABLE_METHOD(Methods_) do { \
            struct Candidate c_; \
            if (find_method_in_list_(C, Methods_, name, &c_)) \
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
                pawIr_solver_add_obligations_from_trait(q.S, *p, cause);
                if (traits_are_compatible(C, q, trait, *p)) {
                    struct IrTraitDef const *def = pawIr_get_trait_def(C, (*p)->did);
                    ADD_APPLICABLE_METHOD(def->methods);
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
                pawIr_solver_add_obligations_from_trait(q.S, *p, cause);
                if (traits_are_compatible(C, q, trait, *p)) {
                    struct IrTraitDef const *def = pawIr_get_trait_def(C, (*p)->did);
                    ADD_APPLICABLE_METHOD(def->methods);
                }
                finish_query(C, q);
            }
        }
    } else {
        // The receiver is a concrete type. Search in impl blocks whose "Self" is
        // compatible with the receiver type "self".

        // search trait implementations
        IrDefs const *trait_defs = pawIr_trait_impls_for(C, self);
        K_LIST_XFOREACH (trait_defs, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            pawIr_solver_add_obligations_from(q.S, *p, inst.args, cause);
            if (impls_are_compatible(C, q, self, trait, inst)) {
                struct IrImpl const *def = pawIr_get_impl_def(C, *p);
                ADD_APPLICABLE_METHOD(def->methods);
            }
            finish_query(C, q);
        }

        // search blanket implementations
        K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            pawIr_solver_add_obligations_from(q.S, *p, inst.args, cause);
            paw_assert(inst.trait != NULL); // blanket inherent impls are not allowed
            if (impls_are_compatible(C, q, self, trait, inst)) {
                struct IrImpl const *def = pawIr_get_impl_def(C, *p);
                ADD_APPLICABLE_METHOD(def->methods);
            }
            finish_query(C, q);
        }
    }

    if (candidates->count == 0)
        return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        LOOKUP_ERROR(C, MultipleApplicableItems,
                .modname = SCAN_STR(C, ""),
                .span = TODO);

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

#undef ADD_APPLICABLE_METHOD
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

struct Instantiation *pawIr_find_assoc_type_generic(struct Compiler *C, IrType *self, Str const *name, struct IrObligationCause cause)
{
#define ADD_APPLICABLE_TYPE(Trait_, Methods_) do { \
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
        if (bounds != NULL) {
            K_LIST_XFOREACH (bounds, IrTrait *const, p) {
                struct IrTraitDef const *def = pawIr_get_trait_def(C, (*p)->did);
                ADD_APPLICABLE_TYPE(*p, def->items);
            }
        }
   }

    if (candidates->count == 0)
        return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        LOOKUP_ERROR(C, MultipleApplicableItems,
                .modname = SCAN_STR(C, ""),
                .span = TODO);

    struct Candidate const result = Candidates_first(candidates);
    IrType *assoc = pawIr_get_def_type(C, result.target);

    struct IrProjection *p = IrGetProjection(assoc);
    IrTrait *trait2 = pawIr_solver_instantiate_trait(C->S, trait->did);
    IrType *first = IrGenericArg_get_type(
            IrGenericArgs_first(trait->args));
    pawU_unify_unchecked(C->U, first, self);
    pawIr_unify_traits_unchecked(C, trait, trait2);
    assoc = pawIr_new_projection(C, p->did, trait->args);

    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.params = pawIr_get_generic_args(C, result.target),
        .subst.args = IR_GENERIC_ARGS(assoc),
        .inst = assoc,
    };
    return out;

#undef ADD_APPLICABLE_TYPE
}

struct Instantiation *pawIr_find_assoc_type_projection(struct Compiler *C, IrType *self, IrTrait *trait, Str const *name, struct IrObligationCause cause)
{
#define ADD_APPLICABLE_TYPE(ImplDid_, ImplArgs_, Methods_) do { \
            struct Candidate c_ = {.impl_did = INVALID_DECL_ID}; \
            if (find_type_in_list(Methods_, name, &c_)) { \
                c_.impl_did = ImplDid_; \
                c_.impl_args = ImplArgs_; \
                Candidates_push(C, candidates, c_); \
            } \
        } while (0)

    Candidates *candidates = Candidates_new(C);
    {
        // search trait implementations
        IrDefs const *trait_defs = pawIr_trait_impls_for(C, self);
        K_LIST_XFOREACH (trait_defs, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            pawIr_solver_add_obligations_from(q.S, impl->did, inst.args, cause);
            if (impls_are_compatible(C, q, self, trait, inst))
                ADD_APPLICABLE_TYPE(*p, inst.args, impl->items);
            finish_query(C, q);
        }

        // search blanket implementations
        K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            pawIr_solver_add_obligations_from(q.S, *p, inst.args, cause);
            paw_assert(inst.trait != NULL); // blanket inherent impls are not allowed
            if (impls_are_compatible(C, q, self, trait, inst))
                ADD_APPLICABLE_TYPE(*p, inst.args, impl->items);
            finish_query(C, q);
        }
    }

    if (candidates->count != 1)
        return NULL;

    struct Candidate const result = Candidates_first(candidates);
    struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, result.impl_did);

    self = pawIr_remove_indirection(C, self);
    IrType *impl_type = pawIr_remove_indirection(C, inst.type);
    pawU_unify_unchecked(C->U, impl_type, self);

    IrGenericArgs *params = pawIr_get_generic_args(C, result.impl_did);
    struct Substitution const subst = {params, inst.args};
    IrType *assoc = pawIr_get_def_type(C, result.target);
    assoc = pawP_substitute(C, assoc, subst);

    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.params = params,
        .subst.args = inst.args,
        .inst = pawU_normalize(C->U, assoc),
    };
    return out;

#undef ADD_APPLICABLE_TYPE
}

static paw_Bool find_assoc_fn_in_list(struct Compiler *C, IrTypeList *methods, Str const *name, struct Candidate *out)
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
struct Instantiation *pawP_find_assoc_fn(struct Compiler *C, IrType *self, Str const *name, struct IrObligationCause cause)
{
#define ADD_APPLICABLE_METHOD(ImplDid_, ImplArgs_) do { \
            struct Candidate c_ = {.impl_did = INVALID_DECL_ID}; \
            struct IrImpl const *impl_def = pawIr_get_impl_def(C, ImplDid_); \
            if (find_assoc_fn_in_list(C, impl_def->methods, name, &c_)) { \
                c_.impl_did = ImplDid_; \
                c_.impl_args = ImplArgs_; \
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
                                    .impl_did = INVALID_DECL_ID,
                                    .impl_args = NULL,
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
        IrDefs const *inherent_defs = pawIr_inherent_impls_for(C, self);
        K_LIST_XFOREACH (inherent_defs, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            pawIr_solver_add_obligations_from(q.S, *p, inst.args, cause);
            if (types_are_compatible(C, q, self, inst.type))
                ADD_APPLICABLE_METHOD(*p, inst.args);
            finish_query(C, q);
        }

        // search trait implementations
        IrDefs const *trait_defs = pawIr_trait_impls_for(C, self);
        K_LIST_XFOREACH (trait_defs, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            pawIr_solver_add_obligations_from(q.S, *p, inst.args, cause);
            if (types_are_compatible(C, q, self, inst.type))
                ADD_APPLICABLE_METHOD(*p, inst.args);
            finish_query(C, q);
        }

        // search blanket implementations
        K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
            struct QueryState const q = start_query(C);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, *p);
            pawIr_solver_add_obligations_from(q.S, *p, inst.args, cause);
            if (types_are_compatible(C, q, self, inst.type))
                ADD_APPLICABLE_METHOD(*p, inst.args);
            finish_query(C, q);
        }
    }

    if (candidates->count == 0)
        return NULL;

    // TODO: return error indicator
    if (candidates->count > 1)
        LOOKUP_ERROR(C, MultipleApplicableItems,
                .modname = SCAN_STR(C, ""),
                .span = TODO);

    struct Candidate const result = Candidates_first(candidates);
    IrType *method = pawIr_solver_instantiate_type(C->S, result.target);

    // TODO: call this and use `inst.args` instead of `tesult.impl_args`, won't work when `self` is a generic or projection
//    struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, result.impl_did);

    // apply information known about the context type
    IrType *context = pawIr_get_context(C, method);
//    pawU_unify_unchecked(C->U, inst.type, self);
    pawU_unify_unchecked(C->U, context, self);
    method = pawU_normalize(C->U, method);

    // allocate return value
    struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
    *out = (struct Instantiation){
        .subst.params = pawIr_get_generic_args(C, result.target),
        .subst.args = IR_GENERIC_ARGS(method),
        .inst = method,
    };
    if (DECL_ID_EXISTS(result.impl_did))
        pawIr_solver_add_obligations_from(C->S, result.impl_did, result.impl_args, cause);
    pawIr_solver_add_obligations_from(C->S, IR_TYPE_DID(method), IR_GENERIC_ARGS(method), cause);
    return out;

#undef ADD_APPLICABLE_METHOD
}
