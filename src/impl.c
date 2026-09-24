// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: should support specifying the trait, basically a projection where the assoc item is a function
//       necessary to distinguish between methods with the same name from different traits

#include "impl.h"
#include "ir_type.h"
#include "solve.h"
#include "trait.h"
#include "unify.h"

#define LOOKUP_ERROR(C_, Kind_, ...) THROW_ERROR(C_, \
        Kind_, __VA_ARGS__)

#define TODO (struct SourceSpan){0}

struct QueryState {
    int save_point;
    IrSolver *S;
};

struct LookupState {
    struct Compiler *C;
    IrType *self;
    IrTrait *trait;
    IrType *raw_self;
    Str const *name;
    IrTypeList *call_args;
    struct QueryState q;
    struct IrObligationCause cause;
    struct Candidates *candidates;
};

static paw_Bool self_is_concrete(struct LookupState *L)
{
    return !IrIsGeneric(L->raw_self) && !IrIsProjection(L->raw_self);
}

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

enum CandidateKind {
    CK_INHERENT_IMPL,
    CK_TRAIT_IMPL,
    CK_TRAIT_BOUND,
};

struct Candidate {
    enum CandidateKind kind;
    DeclId target;
    union {
        IrTrait *trait;
        DeclId impl;
    };
};

DEFINE_LIST(struct Compiler, Candidates, struct Candidate,)

struct Candidate_ {
    DeclId target;
    IrGenericArgs *impl_args;
    DeclId impl_did;
};

DEFINE_LIST(struct Compiler, Candidates_, struct Candidate_,)

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

static paw_Bool find_method_in_list(struct LookupState *L, IrTypeList *methods, IrGenericArgs *generic_args, enum CandidateKind kind, struct Candidate *out)
{
    IrType *const *p;
    K_LIST_FOREACH (methods, p) {
        if (pawS_eq(L->name, name_of_method(L->C, *p))
                && call_args_match(L->C, *p, L->self, generic_args, L->call_args)
                && solve_query_obligations(L->q)) {
            out->kind = kind;
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

#define SELECT_CANDIDATES_FROM(Kind_, Impls_, Candidates_, Cause_) \
        K_LIST_XFOREACH (Impls_, DeclId const, p) { \
            L->q = start_query(L->C); \
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(L->q.S, *p); \
            pawIr_solver_add_obligations_from(L->q.S, *p, inst.args, Cause_); \
            struct Candidate c = {.impl = *p}; \
            struct IrImpl const *impl_def = pawIr_get_impl_def(L->C, *p); \
            /* NOTE: If `L->trait` is nonnull then only trait impls are searched. */ \
            paw_assert(L->trait == NULL || inst.trait != NULL); \
            if ((L->trait == NULL || pawIr_unify_traits(L->C, L->trait, inst.trait) == 0) \
                    && find_method_in_list(L, impl_def->methods, inst.args, Kind_, &c)) \
                Candidates_push(L->C, Candidates_, c); \
            finish_query(L->C, L->q); \
        }

static void select_inherent_candidates(struct LookupState *L)
{
    // search inherent implementations
    IrDefs const *inherent_impls = pawIr_inherent_impls_for(L->C, L->raw_self);
    SELECT_CANDIDATES_FROM(CK_INHERENT_IMPL, inherent_impls, L->candidates, L->cause);
}

static IrTraitList *elaborate_trait_bounds(struct LookupState *L, IrTraitList const *bounds)
{
    IrTraitList *result = IrTraitList_new(L->C);
    IrTraitList_reserve(L->C, result, bounds->count);
    K_LIST_XFOREACH (bounds, IrTrait *const, ptrait) {
        IrTraitList_push(L->C, result, *ptrait);
        IrTraitList const *supertraits = pawIr_supertraits_of(L->C, (*ptrait)->did);
        K_LIST_XFOREACH (supertraits, IrTrait *const, psupertrait)
            IrTraitList_push(L->C, result, *psupertrait);
    }
    return result;
}

static void select_trait_candidates(struct LookupState *L)
{
    struct Compiler *C = L->C;
    IrType *raw_self = L->raw_self;

    if (!self_is_concrete(L)) {
        IrTraitList const *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(raw_self));
        if (bounds != NULL) {
            bounds = elaborate_trait_bounds(L, bounds);
            // search through bounds on the generic parameter or associated type declaration
            K_LIST_XFOREACH (bounds, IrTrait *const, ptrait) {
                IrTrait *trait = *ptrait;
                IrGenericArgs *trait_args = clone_args(C, trait->args);
                IrGenericArgs_set(trait_args, 0, IrGenericArg_from_type(raw_self));
                L->q = start_query(C);
                pawIr_solver_add_obligations_from(L->q.S, trait->did, trait_args, L->cause);
                struct Candidate c = {.trait = pawIr_new_trait(L->C, trait->did, trait_args)};
                struct IrTraitDef const *trait_def = pawIr_get_trait_def(C, trait->did);
                if ((L->trait == NULL || pawIr_unify_traits(C, trait, L->trait) == 0)
                        && find_method_in_list(L, trait_def->methods, trait_args, CK_TRAIT_BOUND, &c))
                    Candidates_push(C, L->candidates, c);
                finish_query(C, L->q);
            }
        }
    } else {
        // search trait implementations
        IrDefs const *trait_impls = pawIr_trait_impls_for(C, raw_self);
        SELECT_CANDIDATES_FROM(CK_TRAIT_IMPL, trait_impls, L->candidates, L->cause);

        // search blanket implementations
        SELECT_CANDIDATES_FROM(CK_TRAIT_IMPL, C->impls.blanket, L->candidates, L->cause);
    }
}

#undef SELECT_CANDIDATES_FROM

static DeclId get_target_decl_id(struct Compiler *C, DeclId did)
{
    struct IrFnDef const *fn_def = pawIr_get_fn_def(C, did);
    enum IrDefKind const parent_kind = pawIr_get_kind(C, fn_def->parent);
    if (parent_kind == IR_TRAIT_DEF) return did;

    paw_assert(parent_kind == IR_IMPL_DEF);
    struct IrImpl const *impl_def = pawIr_get_impl_def(C, fn_def->parent);
    if (impl_def->trait == NULL) return did;

    return pawIr_get_method_from_parent(C, impl_def->trait->did, fn_def->name);
}

static struct Instantiation *finalize_selection(struct LookupState *L)
{
    Candidates const *candidates = L->candidates;
    IrTypeList *call_args = L->call_args;
    struct Compiler *C = L->C;

    if (candidates->count == 0)
        return NULL;

     if (candidates->count > 1)
         LOOKUP_ERROR(C, MultipleApplicableItems,
                 .modname = SCAN_STR(C, ""),
                 .span = TODO);

     struct Candidate const result = Candidates_first(candidates);
     DeclId const target = get_target_decl_id(C, result.target);
     IrType *method = pawIr_solver_instantiate_type(C->S, target);

     if (call_args != NULL) {
         struct IrFnPtr const *fn = IrGetFnPtr(IR_SIGNATURE_FN(C, method));
         paw_assert(fn->params->count == call_args->count);

         IrType *const *param, *const *arg;
         K_LIST_ZIP(fn->params, param, call_args, arg)
             pawU_unify_unchecked(C->U, *param, *arg);
     }

     IrType *context = pawU_normalize_projections(C->U,
             pawIr_remove_indirection(C, pawIr_get_context(C, method)));
     pawU_unify_unchecked(C->U, context, L->raw_self);
     if (result.kind == CK_TRAIT_IMPL) {
         struct IrImplInstance const inst = pawIr_solver_instantiate_impl(C->S, result.impl);
         IrTrait *trait_context = pawIr_get_trait_context(C, method);
         pawIr_unify_traits_unchecked(C, trait_context, inst.trait);
         pawIr_solver_add_obligations_from(C->S, result.impl, inst.args, L->cause);
     } else if (result.kind == CK_TRAIT_BOUND) {
         IrTrait *trait_context = pawIr_get_trait_context(C, method);
         pawIr_unify_traits_unchecked(C, trait_context, result.trait);
         pawIr_solver_add_obligations_from(C->S, trait_context->did, trait_context->args, L->cause);
     } else {
     }

     method = pawU_normalize(C->U, method);

     // allocate return value
     struct Instantiation *out = P_ALLOC(C, NULL, 0, sizeof(*out));
     *out = (struct Instantiation){
         .subst.params = pawIr_get_generic_args(C, target),
         .subst.args = IR_GENERIC_ARGS(method),
         .inst = method,
     };
     pawIr_solver_add_obligations_from(C->S, IR_TYPE_DID(method), IR_GENERIC_ARGS(method), L->cause);
     return out;
}

struct Instantiation *pawP_find_method(struct Compiler *C, IrType *self, IrTrait *trait, Str const *name, IrTypeList *call_args, struct IrObligationCause cause)
{
    self = pawU_normalize_projections(C->U, self);
    IrType *raw_self = pawIr_remove_indirection(C, self);
    if (IrIsInfer(raw_self)) return NULL;

    struct LookupState L = {
        .C = C,
        .self = self,
        .trait = trait,
        .raw_self = raw_self,
        .name = name,
        .call_args = call_args,
        .cause = cause,
        .candidates = Candidates_new(C),
    };
    Candidates *candidates = Candidates_new(C);
    // Inherent impl blocks can only be defined on concrete types. Inherent methods
    // are given precedence over trait methods. For example, if there is both an
    // inherent method and a trait method with the given name on the given receiver,
    // then the inherent method will be chosen.
    if (self_is_concrete(&L) && trait == NULL)
        select_inherent_candidates(&L);
    if (candidates->count == 0)
        select_trait_candidates(&L);
    return finalize_selection(&L);
}

static Str const *name_of_type(struct Compiler *C, IrType *type)
{
    struct IrSignature const *t = IrGetSignature(type);
    struct IrFnDef const *def = pawIr_get_fn_def(C, t->did);
    return def->name;
}

static paw_Bool find_type_in_list(IrAssocItems *items, Str const *name, struct Candidate_ *out)
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
            struct Candidate_ c_; \
            if (find_type_in_list(Methods_, name, &c_)) { \
                Candidates__push(C, candidates, c_); \
                trait = Trait_; \
            } \
        } while (0)

    IrTrait *trait;
    paw_assert(IrIsGeneric(self));
    Candidates_ *candidates = Candidates__new(C);
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

    struct Candidate_ const result = Candidates__first(candidates);
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
    // TODO
    pawIr_solver_add_obligations_from(C->S, trait2->did, trait2->args, cause);
    return out;

#undef ADD_APPLICABLE_TYPE
}

struct Instantiation *pawIr_find_assoc_type_projection(struct Compiler *C, IrType *self, IrTrait *trait, Str const *name, struct IrObligationCause cause)
{
#define ADD_APPLICABLE_TYPE(ImplDid_, ImplArgs_, Methods_) do { \
            struct Candidate_ c_ = {.impl_did = INVALID_DECL_ID}; \
            if (find_type_in_list(Methods_, name, &c_)) { \
                c_.impl_did = ImplDid_; \
                c_.impl_args = ImplArgs_; \
                Candidates__push(C, candidates, c_); \
            } \
        } while (0)

    Candidates_ *candidates = Candidates__new(C);
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

    struct Candidate_ const result = Candidates__first(candidates);
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

     if (DECL_ID_EXISTS(result.impl_did))
         pawIr_solver_add_obligations_from(C->S, result.impl_did, result.impl_args, cause);
    return out;

#undef ADD_APPLICABLE_TYPE
}

static paw_Bool find_assoc_fn_in_list(struct Compiler *C, IrTypeList *methods, Str const *name, struct Candidate_ *out)
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
