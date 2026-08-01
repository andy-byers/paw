// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "solve.h"
#include "impl.h"
#include "ir_type.h"
#include "resolve.h" // UpvalueList
#include "trait.h"
#include "type_folder.h"
#include "unify.h"

#include"stdio.h"

#define TODO ((struct SourceSpan){0})

#if defined(PAW_SOLVER_DEBUG)
# define LOGLN(Fmt_, ...) fprintf(stderr, "(paw_solver) "Fmt_ "\n", __VA_ARGS__)
#else
# define LOGLN(...)
#endif

#define WITH_SOLVER_CTX(C_, SolverName_, Code_) do { \
            int const save_ = pawU_current_position((C_)->U); \
            IrSolver *SolverName_ = pawIr_push_solver(C_); \
            (void)SolverName_; \
            Code_ \
            pawU_undo_unifications((C_)->U, save_); \
            pawIr_pop_solver(C_); \
        } while (0)

struct IrSolver {
    IrSolver *outer;
    IrObligations *obligations;
    IrObligations *predicates;
    IrObligations *norm_targets;
    struct Compiler *C;
    struct Unifier *U;
};

static void dump_obligations(IrSolver const *S)
{
    if (S->obligations->count > 0) {
        printf("obligations:\n");
        K_LIST_XFOREACH(S->obligations, struct IrObligation const, p){
            switch (p->kind) {
                case IR_OBLIGATION_WELL_FORMED: {
                    enum IrDefKind const def_kind = pawIr_get_kind(S->C, p->wf.did);
                    if (def_kind == IR_TRAIT_DEF) {
                        IrTrait *trait = pawIr_new_trait(S->C, p->wf.did, p->wf.args);
                        printf("  WellFormed(%s)\n", pawIr_print_trait(S->C, trait));
                    } else {
                        paw_assert(def_kind == IR_ADT_DEF);
                        IrType *type = pawIr_new_adt(S->C, p->wf.did, p->wf.args);
                        printf("  WellFormed(%s)\n", pawIr_print_type(S->C, type));
                    }
                    break;
                }
                case IR_OBLIGATION_IMPL_TRAIT: {
                    IrType *type = pawU_normalize_projections(S->C->U, p->impl.type);
                    IrTrait *trait = pawIr_normalize_trait(S->C, p->impl.trait);
                    printf("  %s: %s\n", pawIr_print_type(S->C, type), pawIr_print_trait(S->C, trait));
                    break;
                }
                case IR_OBLIGATION_TYPE_EQUALS:
                    printf("  %s := %s\n", pawIr_print_type(S->C, p->eq.lhs), pawIr_print_type(S->C, p->eq.rhs));
                    break;
            }
        }
    }
}

IrSolver *pawIr_push_solver(struct Compiler *C)
{
    IrSolver *S = P_ALLOC(C, NULL, 0, sizeof(IrSolver));
    *S = (IrSolver){
        .obligations = IrObligations_new(C),
        .predicates = IrObligations_new(C),
        .norm_targets = IrObligations_new(C),
        .outer = C->S,
        .U = C->U,
        .C = C,
    };
    C->S = S;
    return S;
}

void pawIr_pop_solver(struct Compiler *C)
{
    C->S = C->S->outer;
}

static IrType *find_norm_target(IrSolver *S, IrType *type)
{
    K_LIST_XFOREACH (S->norm_targets, struct IrObligation const, p) {
        int const position = pawU_current_position(S->U);
        if (pawU_unify(S->U, type, p->eq.lhs) == 0) return p->eq.rhs;
        pawU_undo_unifications(S->U, position);
    }
    return NULL;
}

void pawIr_solver_add_norm_target(IrSolver *S, IrType *type, IrType *target, struct IrObligationCause cause)
{
    IrObligations_push(S->C, S->norm_targets, (struct IrObligation){
                .kind = IR_OBLIGATION_TYPE_EQUALS,
                .eq.lhs = type,
                .eq.rhs = target,
                .cause = cause,
            });

    LOGLN("SOLVER:%p: add norm target `%s = %s`",
            (void *)S, pawIr_print_type_v2(S->C, type)->text,
            pawIr_print_type_v2(S->C, target)->text);
}

IrType *pawIr_solver_get_norm_target(IrSolver *S, IrType *type)
{
    paw_assert(IrIsProjection(type));
    type = pawU_normalize(S->U, type);
    if (!pawIr_type_contains_inference_var(S->C, type)) {
        do {
            IrType *target = find_norm_target(S, type);
            if (target != NULL) return target;
            S = S->outer;
        } while (S != NULL);
    }
    return NULL;
}

DEFINE_MAP(struct Compiler, PredicateCache, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, void *,)

void pawIr_solver_add_predicate(IrSolver *S, IrType *type, IrTrait *trait, struct IrObligationCause cause)
{
    K_LIST_XFOREACH (S->predicates, struct IrObligation const, p) {
        int const position = pawU_current_position(S->U);
        paw_Bool const matches = pawU_unify(S->U, type, p->impl.type) == 0
                && pawIr_unify_traits(S->C, trait, p->impl.trait) == 0;
        pawU_undo_unifications(S->U, position);
        if (matches) return;
    }

    IrObligations_push(S->C, S->predicates, (struct IrObligation){
                .kind = IR_OBLIGATION_IMPL_TRAIT,
                .cause = cause,
                .impl.type = type,
                .impl.trait = trait,
            });

    LOGLN("SOLVER:%p: add predicate `%s: %s`",
            (void *)S, pawIr_print_type_v2(S->C, type)->text,
            pawIr_print_trait_v2(S->C, trait)->text);
}

static IrTrait *substitute_trait(struct Compiler *C, IrTrait *trait, struct Substitution subst)
{
    IrGenericArgs *types = IrGenericArgs_new(C);
    IrGenericArgs_reserve(C, types, trait->args->count);
    K_LIST_XFOREACH (trait->args, IrGenericArg const, p) {
        if (IrGenericArg_is_type(*p)) {
            IrType *t = pawP_substitute(C, IrGenericArg_get_type(*p), subst);
            IrGenericArgs_push(C, types, IrGenericArg_from_type(t));
        } else {
            IrGenericArgs_push(C, types, *p);
        }
    }
    return pawIr_new_trait(C, trait->did, types);
}

void pawIr_solver_add_well_formed_obligation(IrSolver *S, DeclId did, IrGenericArgs *args, struct IrObligationCause cause)
{
    IrObligations_push(S->C, S->obligations, (struct IrObligation){
            .kind = IR_OBLIGATION_WELL_FORMED,
            .cause = cause,
            .wf.did = did,
            .wf.args = args,
        });
}

#define ENSURE_TYPE_IS_WELL_FORMED(S_, Type_, Cause_) \
    (IrIsAdt(Type_) ? pawIr_solver_add_well_formed_obligation(S_, IR_TYPE_DID(Type_), \
            IR_GENERIC_ARGS(Type_), Cause_) : (void)0)
#define ENSURE_TRAIT_IS_WELL_FORMED(S_, Trait_, Cause_) \
    pawIr_solver_add_well_formed_obligation(S_, (Trait_)->did, (Trait_)->args, Cause_)

void pawIr_solver_add_impl_trait_obligation(IrSolver *S, struct IrType *type, struct IrTrait *trait, struct IrObligationCause cause)
{
    IrObligations_push(S->C, S->obligations, (struct IrObligation){
            .kind = IR_OBLIGATION_IMPL_TRAIT,
            .cause = cause,
            .impl.trait = trait,
            .impl.type = type,
        });
}

void pawIr_solver_add_type_equals_obligation(IrSolver *S, struct IrType *lhs, struct IrType *rhs, struct IrObligationCause cause)
{
    IrObligations_push(S->C, S->obligations, (struct IrObligation){
            .kind = IR_OBLIGATION_TYPE_EQUALS,
            .cause = cause,
            .eq.lhs = lhs,
            .eq.rhs = rhs,
        });
}

void pawIr_solver_add_const_equals_obligation(IrSolver *S, struct IrConst *lhs, struct IrConst *rhs, struct IrObligationCause cause)
{
    IrObligations_push(S->C, S->obligations, (struct IrObligation){
            .kind = IR_OBLIGATION_CONST_EQUALS,
            .cause = cause,
            .keq.lhs = lhs,
            .keq.rhs = rhs,
        });
}

static void add_obligations(IrSolver *S, DeclId did, struct Substitution subst, PredicateCache *cache, struct IrObligationCause cause)
{
    if (PredicateCache_insert(S->C, cache, did, NULL))
        return;

    IrConstraints const *bounds = pawIr_get_constraints(S->C, did);
    K_LIST_XFOREACH (bounds, struct IrConstraint, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT: {
                IrType *type = pawP_substitute(S->C, p->impl.type, subst);
                IrTrait *trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                pawIr_solver_add_impl_trait_obligation(S, type, trait, cause);
                LOGLN("SOLVER:%p: add obligation `%s: %s`",
                        (void *)S, pawIr_print_type_v2(S->C, type)->text,
                        pawIr_print_trait_v2(S->C, trait)->text);
                break;
            }
            case IR_CONSTRAINT_TYPE_EQUALS: {
                IrType *lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                IrType *rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                pawIr_solver_add_type_equals_obligation(S, lhs, rhs, cause);
                LOGLN("SOLVER:%p: add obligation `%s = %s`",
                        (void *)S, pawIr_print_type_v2(S->C, lhs)->text,
                        pawIr_print_type_v2(S->C, rhs)->text);
                break;
            }
        }
    }
}

void pawIr_solver_add_obligations_from(IrSolver *S, DeclId parent_did, IrGenericArgs *args, struct IrObligationCause cause)
{
    PredicateCache *cache = PredicateCache_new(S->C);
    IrGenericArgs *params = pawIr_get_generic_args(S->C, parent_did);
    struct Substitution const subst = {params, args};
    add_obligations(S, parent_did, subst, cache, cause);
}

void pawIr_solver_add_obligations_from_type(IrSolver *S, IrType *type, struct IrObligationCause cause)
{
    if (IrIsAdt(type)) {
        struct IrAdt const *adt = IrGetAdt(type);
        pawIr_solver_add_obligations_from(S, adt->did, adt->args, cause);
    } else if (IrIsSignature(type)) {
        struct IrSignature const *fn = IrGetSignature(type);
        pawIr_solver_add_obligations_from(S, fn->did, fn->args, cause);
    }
}

void pawIr_solver_add_obligations_from_trait(IrSolver *S, IrTrait *trait, struct IrObligationCause cause)
{
    pawIr_solver_add_obligations_from(S, trait->did, trait->args, cause);
}

static paw_Bool impl_is_compatible(struct Compiler *C, IrType *self, IrTrait *trait, struct IrImpl const *impl, struct IrObligationCause cause)
{
    paw_Bool matches = PAW_FALSE;
    WITH_SOLVER_CTX(C, child, {
        struct IrImplInstance const inst = pawIr_solver_instantiate_impl(child, impl->did);
        if (pawU_unify(C->U, inst.type, self) == 0
                && pawIr_unify_traits(C, inst.trait, trait) == 0) {
            pawIr_solver_add_obligations_from(child, impl->did, inst.args, cause);
            struct IrSolverResult const result = pawIr_solver_solve(child);
            matches = result.status != IR_SOLVER_ERROR;
        }
    });

    return matches;
}

static paw_Bool matches_impl_predicate(IrSolver *S, IrType *type, IrTrait *trait)
{
    paw_assert(!IrIsInfer(type));
    K_LIST_XFOREACH (S->predicates, struct IrObligation const, p) {
        switch (p->kind) {
            case IR_OBLIGATION_IMPL_TRAIT: {
                    int const position = pawU_current_position(S->U);
                    if (pawU_unify(S->U, type, p->impl.type) == 0
                            && pawIr_unify_traits(S->C, trait, p->impl.trait) == 0) {
                        LOGLN("SOLVER:%p: proved `%s: %s` using predicate",
                                (void *)S, pawIr_print_type(S->C, type),
                                pawIr_print_trait(S->C, trait));
                        return PAW_TRUE;
                    }
                    pawU_undo_unifications(S->U, position);
                    break;
            }
            case IR_OBLIGATION_TYPE_EQUALS:
            case IR_OBLIGATION_CONST_EQUALS:
            case IR_OBLIGATION_WELL_FORMED:
                break;
        }
    }
    return PAW_FALSE;
}

struct Candidate {
    DeclId impl_did;
    DeclId trait_did;
};

DEFINE_LIST(struct Compiler, Candidates, struct Candidate,)

static enum IrSolverStatus type_implements_trait(IrSolver *S, IrType *self, IrTrait *impl_trait, struct IrObligationCause cause)
{
    IrSolver *cursor = S;
    while (cursor != NULL) {
        if (matches_impl_predicate(cursor, self, impl_trait))
            return IR_SOLVER_SOLVED;
        cursor = cursor->outer;
    }

    struct Compiler *C = S->C;
    Candidates *candidates = Candidates_new(C);

    // add instantiated trait type from trait impl blocks where the context type is
    // compatible with "self"
    IrDefs const *trait_defs = pawIr_trait_impls_for(C, self);
    K_LIST_XFOREACH (trait_defs, DeclId const, p) {
        struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
        if (impl_is_compatible(C, self, impl_trait, impl, cause)) {
            Candidates_push(C, candidates, (struct Candidate){
                        .trait_did = impl->trait->did,
                        .impl_did = impl->did,
                    });

            LOGLN("SOLVER:%p: proved `%s: %s` using trait impl block DefId(%d)",
                    (void *)S, pawIr_print_type(S->C, pawU_normalize_projections(S->U, impl->type)),
                    pawIr_print_trait(S->C, pawIr_normalize_trait(S->C, impl->trait)),
                    impl->did.value);
        }
    }

    // add instantiated trait types from blanket impl blocks
    K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
        struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
        if (impl_is_compatible(C, self, impl_trait, impl, cause)) {
            Candidates_push(C, candidates, (struct Candidate){
                        .trait_did = impl->trait->did,
                        .impl_did = impl->did,
                    });
            LOGLN("SOLVER:%p: proved `%s: %s` using blanket impl block DefId(%d)",
                    (void *)S, pawIr_print_type(S->C, impl->type),
                    pawIr_print_trait(S->C, impl->trait),
                    impl->did.value);
        }
    }

    if (candidates->count == 0)
        return IR_SOLVER_ERROR;
    if (candidates->count > 1)
        return IR_SOLVER_AMBIGUOUS;

    {
        IrSolver *child = pawIr_push_solver(S->C);
        struct Candidate const c = Candidates_first(candidates);
        struct IrImplInstance const inst = pawIr_solver_instantiate_impl(child, c.impl_did);
        pawIr_solver_add_obligations_from(child, c.impl_did, inst.args, cause);
        pawU_unify_unchecked(C->U, inst.type, self);
        pawIr_unify_traits_unchecked(C, inst.trait, impl_trait);
        struct IrSolverResult const r = pawIr_solver_solve(child);
        paw_assert(r.status != IR_SOLVER_ERROR); PAW_UNUSED(r);
        pawIr_pop_solver(child->C);
    }
    return IR_SOLVER_SOLVED;
}

static IrTrait *adjust_trait(IrSolver *S, IrTrait *trait, IrType *self)
{
    IrGenericArgs *args = IrGenericArgs_new(S->C);
    IrGenericArgs_push(S->C, args, IrGenericArg_from_type(self));
    for (int i = 1; i < trait->args->count; ++i)
        IrGenericArgs_push(S->C, args, IrGenericArgs_get(trait->args, i));
    return pawIr_new_trait(S->C, trait->did, args);
}

paw_Bool pawIr_type_implements_trait(IrSolver *S, IrType *type, IrTrait *trait)
{
    type = pawU_normalize_projections(S->U, type);
    trait = pawIr_normalize_trait_projections(S->C, trait);

    if (pawIr_type_contains_inference_var(S->C, type)
            || pawIr_trait_contains_inference_var(S->C, trait))
        return PAW_FALSE; // not enough information
    // search for evidence in the form of a compatible impl block
    struct IrObligationCause const cause = {0};
    return type_implements_trait(S, type, trait, cause) == IR_SOLVER_SOLVED;
}

static enum IrSolverStatus solve_normalizes_to_obligation(IrSolver *S, IrType *projection, IrType *target)
{
    {
        struct IrProjection const *p = IrGetProjection(projection);
        if (IrIsInfer(ir_projection_self(p)))
            return IR_SOLVER_AMBIGUOUS;
    }

    IrType *type = pawU_normalize_projections(S->U, projection);
    target = pawU_normalize_projections(S->U, target);

    if (IrIsProjection(type)) {
        struct IrProjection const *p = IrGetProjection(type);
        if (IrIsInfer(ir_projection_self(p)))
            return IR_SOLVER_AMBIGUOUS;
    }

    int const position = pawU_current_position(S->U);
    if (pawU_unify(S->U, type, target) != 0) {
        pawU_undo_unifications(S->U, position);
        return IR_SOLVER_ERROR;
    }
    return IR_SOLVER_SOLVED;
}

#define RESULT_SOLVED() ((struct IrSolverResult){ \
        .status = IR_SOLVER_SOLVED, \
    })
#define RESULT_AMBIGUOUS(NumUnsolved_) ((struct IrSolverResult){ \
        .status = IR_SOLVER_AMBIGUOUS, \
        .ambiguous.num_unsolved = NumUnsolved_, \
    })
#define RESULT_ERROR(Obligation_) ((struct IrSolverResult){ \
        .status = IR_SOLVER_ERROR, \
        .error.obligation = Obligation_, \
    })

struct IrSolverResult pawIr_solver_solve(IrSolver *S)
{
    if (S->obligations->count == 0) {
        LOGLN("SOLVER:%p: nothing to solve", (void *)S);
        return RESULT_SOLVED();
    }

    LOGLN("SOLVER:%p: starting solver invocation", (void *)S);

    paw_Bool solved_any;
    do {
        solved_any = PAW_FALSE;
        for (int i = 0; i < S->obligations->count; ++i) {
            paw_Bool solved = PAW_FALSE;
            struct IrObligation const o = IrObligations_get(S->obligations, i);
            switch (o.kind) {
                case IR_OBLIGATION_WELL_FORMED: {
                    LOGLN("SOLVER:%p: encountered obligation `WellFormed(%s)`",
                            (void *)S, pawIr_get_kind(S->C, o.wf.did) == IR_TRAIT_DEF
                                ? pawIr_print_trait(S->C, pawIr_new_trait(S->C, o.wf.did, o.wf.args))
                                : pawIr_print_type(S->C, pawIr_new_adt(S->C, o.wf.did, o.wf.args)));
                    // construct is considered well formed if all of the obligations it imposes
                    // can be proved true
                    IrSolver *child = pawIr_push_solver(S->C);
                    pawIr_solver_add_obligations_from(child, o.wf.did, o.wf.args, o.cause);
                    K_LIST_XFOREACH (o.wf.args, IrGenericArg const, arg) {
                        if (IrGenericArg_is_type(*arg)) {
                            IrType *arg_type = IrGenericArg_get_type(*arg);
                            if (IrIsAdt(arg_type))
                                pawIr_solver_add_well_formed_obligation(child, IR_TYPE_DID(arg_type), IR_GENERIC_ARGS(arg_type), o.cause);
                        }
                    }
                    struct IrSolverResult const r = pawIr_solver_solve(child);
                    pawIr_pop_solver(S->C);

                    if (r.status == IR_SOLVER_ERROR)
                        return RESULT_ERROR(r.error.obligation);
                    solved = r.status == IR_SOLVER_SOLVED;
                    break;
                }
                case IR_OBLIGATION_IMPL_TRAIT: {
                    IrType *type = pawU_normalize_projections(S->U, o.impl.type);
                    IrTrait *trait = pawIr_normalize_trait_projections(S->C, o.impl.trait);
                    LOGLN("SOLVER:%p: encountered impl trait obligation `%s`",
                            (void *)S, pawIr_print_impl_trait_obligation(S->C, type, trait));

                    if (IrIsInfer(type))
                        break;

                    IrSolver *child = pawIr_push_solver(S->C);
                    if (IrIsProjection(type)) {
                        struct IrProjection const *p = IrGetProjection(type);
                        IrTrait *trait = pawIr_get_projection_trait(S->C, p);
                        pawIr_solver_add_predicates_from_trait(child, trait, o.cause);
                    }
                    enum IrSolverStatus const status = type_implements_trait(child, type, trait, o.cause);
                    pawIr_pop_solver(S->C);

                    if (status == IR_SOLVER_SOLVED) {
                        LOGLN("SOLVER:%p: proved impl trait obligation `%s`",
                                (void *)S, pawIr_print_impl_trait_obligation(S->C, type, trait));

                        solved = PAW_TRUE;
                    } else if (status == IR_SOLVER_ERROR) {
                        LOGLN("SOLVER:%p: unprovable impl trait obligation \"%s\"",
                                (void *)S, pawIr_print_impl_trait_obligation(S->C, type, trait));
                        return RESULT_ERROR(o);
                    }
                    break;
                }
                case IR_OBLIGATION_TYPE_EQUALS: {
                    IrType *lhs = pawU_normalize(S->U, o.eq.lhs);
                    IrType *rhs = pawU_normalize(S->U, o.eq.rhs);

                    switch (solve_normalizes_to_obligation(S, lhs, rhs)) {
                        case IR_SOLVER_SOLVED: {
                            LOGLN("SOLVER:%p: proved type equals obligation `%s = %s`",
                                    (void *)S, pawIr_print_type(S->C, lhs),
                                    pawIr_print_type(S->C, rhs));

                            solved = PAW_TRUE;
                            break;
                        }
                        case IR_SOLVER_AMBIGUOUS:
                            break;
                        case IR_SOLVER_ERROR: {
                            LOGLN("SOLVER:%p: unable to solve type equals obligation `%s = %s`",
                                    (void *)S, pawIr_print_type(S->C, lhs),
                                    pawIr_print_type(S->C, rhs));
                            return RESULT_ERROR(o);
                        }
                    }
                    break;
                }
                case IR_OBLIGATION_CONST_EQUALS: {
                    IrConst *lhs = pawU_normalize_const(S->U, o.keq.lhs);
                    IrConst *rhs = pawU_normalize_const(S->U, o.keq.rhs);
                    if (lhs->kind == IR_CONST_PENDING
                            || rhs->kind == IR_CONST_PENDING)
                        break; // ambiguous
                    if (pawU_unify_const(S->U, lhs, rhs) != 0)
                        return RESULT_ERROR(o);
                    solved = PAW_TRUE;
                    break;
                }
            }
            if (solved) {
                IrObligations_swap_remove(S->obligations, i--);
                solved_any = PAW_TRUE;
            }
        }
    } while (solved_any);

    if (S->obligations->count > 0) {
        LOGLN("SOLVER:%p: finished with AMBIGUOUS status (%d unsolved obligations)",
                (void *)S, S->obligations->count);
        return RESULT_AMBIGUOUS(S->obligations->count);
    }

    LOGLN("SOLVER:%p: finished with SUCCESS status", (void *)S);
    return RESULT_SOLVED();
}

void pawIr_solver_solve_all_or_error(IrSolver *S)
{
    struct IrSolverResult const r = pawIr_solver_solve(S);
    if (r.status != IR_SOLVER_SOLVED)
        THROW_ERROR(S->C, Unsupported,
                .modname = S->C->modname,
                .span = {0});
}

IrObligations *pawIr_solver_remove_const_obligations(IrSolver *S)
{
    IrObligations *result = IrObligations_new(S->C);
    for (int i = 0; i < S->obligations->count;) {
        struct IrObligation const o = IrObligations_get(S->obligations, i);
        if (o.kind == IR_OBLIGATION_CONST_EQUALS) {
            IrObligations_push(S->C, result, o);
            IrObligations_swap_remove(S->obligations, i);
        } else {
            ++i;
        }
    }
    return result;
}

int pawIr_solver_num_obligations(IrSolver const *S)
{
    return S->obligations->count;
}

struct IrObligation pawIr_solver_first_obligation(IrSolver const *S)
{
    return IrObligations_first(S->obligations);
}

IrType *pawIr_solver_instantiate_type(IrSolver *S, DeclId did)
{
    IrGenericArgs *params = pawIr_get_generic_args(S->C, did);
    if (params == NULL) return pawIr_get_def_type(S->C, did);
    IrGenericArgs *args = pawIr_instantiate_args(S->C, did);
    return pawIr_solver_instantiate_type_with(S, did, args);
}

IrType *pawIr_solver_instantiate_type_with(IrSolver *S, DeclId did, IrGenericArgs *args)
{
    IrGenericArgs *params = pawIr_get_generic_args(S->C, did);
    struct Substitution const subst = {params, args};
    IrType *base = pawIr_get_def_type(S->C, did);
    return pawP_substitute(S->C, base, subst);
}

IrTrait *pawIr_solver_instantiate_trait(IrSolver *S, DeclId did)
{
    IrTrait *trait = pawIr_get_trait(S->C, did);
    IrGenericArgs *args = pawIr_instantiate_args(S->C, did);
    struct Substitution const subst = {trait->args, args};
    return substitute_trait(S->C, trait, subst);
}

IrTrait *pawIr_solver_instantiate_trait_with(IrSolver *S, DeclId did, IrGenericArgs *args)
{
    IrTrait *trait = pawIr_get_trait(S->C, did);
    struct Substitution const subst = {trait->args, args};
    return substitute_trait(S->C, trait, subst);
}

struct IrImplInstance pawIr_solver_instantiate_impl(IrSolver *S, DeclId did)
{
    IrGenericArgs *params = pawIr_get_generic_args(S->C, did);
    if (params == NULL) {
        struct IrImpl const *def = pawIr_get_impl_def(S->C, did);
        return (struct IrImplInstance){
                .type = def->type,
                .trait = def->trait,
                .args = NULL,
            };
    }
    IrGenericArgs *args = pawIr_instantiate_args(S->C, did);
    return pawIr_solver_instantiate_impl_with(S, did, args);
}

struct IrImplInstance pawIr_solver_instantiate_impl_with(IrSolver *S, DeclId did, IrGenericArgs *args)
{
    struct IrImpl const *impl = pawIr_get_impl_def(S->C, did);
    IrGenericArgs *params = pawIr_get_generic_args(S->C, did);
    struct Substitution const subst = {params, args};
    return (struct IrImplInstance){
        .type = pawP_substitute(S->C, impl->type, subst),
        .trait = impl->trait == NULL ? NULL :
            substitute_trait(S->C, impl->trait, subst),
        .args = args,
    };
}

static IrGenericArgs *replace_self_in_trait_args(struct Compiler *C, IrGenericArgs *args, IrType *target)
{
    IrGenericArgs *result = IrGenericArgs_new(C);
    IrGenericArgs_reserve(C, result, args->count);
    K_LIST_XFOREACH (args, IrGenericArg const, arg)
        IrGenericArgs_push(C, result, *arg);

    IrGenericArgs_set(result, 0, IrGenericArg_from_type(target));
    return result;
}

static paw_Bool target_is_self(IrSolver *S, IrType *target, struct IrGeneric const *self)
{
    if (IrIsProjection(target)) {
        struct IrProjection const *t = IrGetProjection(target);
        return target_is_self(S, ir_projection_self(t), self);
    } else if (IrIsGeneric(target)) {
        return P_ID_EQUALS(S->C, IR_TYPE_DID(target), self->did);
    } else {
        return PAW_FALSE;
    }
}

static IrConstraints *add_supertrait_constraints(IrSolver *S, IrTrait *trait, struct IrObligationCause cause)
{
    IrGenericArgs *params = pawIr_get_generic_args(S->C, trait->did);
    struct Substitution const subst = {params, trait->args};
    struct IrGeneric const *self_param = IrGetGeneric(
            IrGenericArg_get_type(IrGenericArgs_first(params)));
    IrConstraints const *constraints = pawIr_get_constraints(S->C, trait->did);
    IrConstraints *result = IrConstraints_new(S->C);
    K_LIST_XFOREACH (constraints, struct IrConstraint const, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT:
                if (target_is_self(S, p->impl.type, self_param)) {
                    IrType *impl_type = pawP_substitute(S->C, p->impl.type, subst);
                    IrTrait *impl_trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                    pawIr_solver_add_predicate(S, impl_type, impl_trait, cause);
                }
                break;
            case IR_CONSTRAINT_TYPE_EQUALS:
                if (target_is_self(S, p->eq.lhs, self_param)) {
                    IrType *eq_lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                    IrType *eq_rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                    pawIr_solver_add_norm_target(S, eq_lhs, eq_rhs, cause);
                }
                break;
        }
    }
    return result;
}

static void add_predicates(IrSolver *S, DeclId did, struct Substitution subst, PredicateCache *cache, struct IrObligationCause cause)
{
    if (PredicateCache_insert(S->C, cache, did, NULL))
        return;

    IrConstraints const *bounds = pawIr_get_constraints(S->C, did);
    if (bounds == NULL) return;

    K_LIST_XFOREACH (bounds, struct IrConstraint const, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT: {
                IrType *type = pawP_substitute(S->C, p->impl.type, subst);
                IrTrait *trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                type = pawU_normalize(S->U, type);
                trait = pawIr_normalize_trait(S->C, trait);
                pawIr_solver_add_predicate(S, type, trait, cause);

                add_supertrait_constraints(S, trait, cause);
                break;
            }
            case IR_CONSTRAINT_TYPE_EQUALS: {
                IrType *lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                IrType *rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                lhs = pawU_normalize(S->U, lhs);
                rhs = pawU_normalize(S->U, rhs);
                pawIr_solver_add_norm_target(S, lhs, rhs, cause);
                break;
            }
        }
    }
}

void pawIr_solver_add_predicates_from(IrSolver *S, DeclId did, IrGenericArgs *args, struct IrObligationCause cause)
{
    PredicateCache *cache = PredicateCache_new(S->C);
    IrGenericArgs *params = pawIr_get_generic_args(S->C, did);
    struct Substitution const subst = {params, args};
    add_predicates(S, did, subst, cache, cause);
}

void pawIr_solver_add_predicates_from_type(IrSolver *S, IrType *type, struct IrObligationCause cause)
{
    if (IrIsAdt(type) || IrIsSignature(type))
        pawIr_solver_add_predicates_from(S, IR_TYPE_DID(type), IR_GENERIC_ARGS(type), cause);
}

void pawIr_solver_add_predicates_from_trait(IrSolver *S, IrTrait *trait, struct IrObligationCause cause)
{
    pawIr_solver_add_predicates_from(S, trait->did, trait->args, cause);
}

void pawIr_solver_add_copy_obligation_for(IrSolver *S, IrType *type)
{
    struct Compiler *C = S->C;
    switch (IR_KINDOF(type)) {
        case kIrUnit:
        case kIrBool:
        case kIrChar:
        case kIrInt:
        case kIrFloat:
        case kIrString:
        case kIrSlice:
        case kIrPtr:
        case kIrSignature:
        case kIrFnPtr:
            // values of this type are trivially copyable
            break;
        case kIrTuple: {
            struct IrTuple const *t = IrGetTuple(type);
            K_LIST_XFOREACH (t->elems, IrType *const, p)
                pawIr_solver_add_copy_obligation_for(S, *p);
            break;
        }
        case kIrArray: {
            struct IrArray const *t = IrGetArray(type);
            pawIr_solver_add_copy_obligation_for(S, t->type);
            break;
        }
        case kIrClosure:
            if (ir_is_capturing_closure(C, type)) {
                UpvalueList const *upvalues = *UpvalueTable_get(C, C->upvtab, IR_TYPE_DID(type));
                K_LIST_XFOREACH (upvalues, struct UpvalueInfo const, u) {
                    IrType *upvalue = pawIr_get_type(C, u->id);
                    pawIr_solver_add_copy_obligation_for(S, upvalue);
                }
            }
            break;
        default: {
            DeclId const copy_did = C->core_traits[CORE_TRAIT_COPY];
            IrGenericArgs *copy_args = IrGenericArgs_new(C);
            IrGenericArgs_push(C, copy_args, IrGenericArg_from_type(type));
            IrTrait *copy = pawIr_solver_instantiate_trait_with(S, copy_did, copy_args);
            pawIr_solver_add_impl_trait_obligation(S, type, copy,
                    (struct IrObligationCause){0});
            break;
        }
    }
}

Str const *pawIr_print_obligation_cause(struct Compiler *C, struct IrObligationCause cause)
{
    Buffer b;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &b);

    switch (cause.kind) {
        case IR_OBLIGATION_CAUSE_WF_CHECKING:
            L_ADD_LITERAL(P, &b, "well-formedness checking");
            break;
        case IR_OBLIGATION_CAUSE_INSTANTIATION:
            L_ADD_LITERAL(P, &b, "instantiation of type `");
            L_ADD_STRING(P, &b, pawIr_print_type_v2(C, pawU_normalize(C->U, cause.instantiation.type)));
            pawL_add_char(P, &b, '`');
            break;
        case IR_OBLIGATION_CAUSE_ASSOC_ITEM_LOOKUP:
            L_ADD_LITERAL(P, &b, "lookup of associated item `");
            L_ADD_STRING(P, &b, cause.assoc_item_lookup.name);
            L_ADD_LITERAL(P, &b, "` on type `");
            L_ADD_STRING(P, &b, pawIr_print_type_v2(C, pawU_normalize(C->U, cause.assoc_item_lookup.self)));
            pawL_add_char(P, &b, '`');
            break;
        case IR_OBLIGATION_CAUSE_PREDICATE:
            // TODO: this variant shouldn't really exist, predicates should exist as some other data type instead of `struct IrObligation`
            PAW_UNREACHABLE();
    }

    return pawL_buffer_finish(P, &b);
}


Str const *pawIr_print_obligation_(struct Compiler *C, struct IrObligation obligation)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    switch (obligation.kind) {
        case IR_OBLIGATION_CONST_EQUALS:
            pawL_add_fstring(P, &buf, "ConstEq(...)");
            break;
        case IR_OBLIGATION_WELL_FORMED: {
            enum IrDefKind const def_kind = pawIr_get_kind(C, obligation.wf.did);
            if (def_kind == IR_TRAIT_DEF) {
                IrTrait *trait = pawIr_normalize_trait(C,
                        pawIr_new_trait(C, obligation.wf.did, obligation.wf.args));
                pawL_add_fstring(P, &buf, "WellFormed(%s)",
                        pawIr_print_trait(C, trait));
            } else {
                paw_assert(def_kind == IR_ADT_DEF);
                IrType *type = pawU_normalize(C->U,
                        pawIr_new_adt(C, obligation.wf.did, obligation.wf.args));
                pawL_add_fstring(P, &buf, "WellFormed(%s)",
                        pawIr_print_type(C, type));
            }
            break;
        }
        case IR_OBLIGATION_IMPL_TRAIT: {
            IrType *type = pawU_normalize(C->U, obligation.impl.type);
            IrTrait *trait = pawIr_normalize_trait(C, obligation.impl.trait);
            pawL_add_fstring(P, &buf, "%s: %s",
                    pawIr_print_type_v2(C, type)->text,
                    pawIr_print_trait_v2(C, trait)->text);
            break;
        }
        case IR_OBLIGATION_TYPE_EQUALS: {
            IrType *lhs = pawU_normalize(C->U, obligation.eq.lhs);
            IrType *rhs = pawU_normalize(C->U, obligation.eq.rhs);
            pawL_add_fstring(P, &buf, "%s = %s",
                    pawIr_print_type_v2(C, lhs)->text,
                    pawIr_print_type_v2(C, rhs)->text);
            break;
        }
    }

    return pawL_buffer_finish(P, &buf);
}

static void print_obligation(IrSolver *S, Buffer *buf, struct IrObligation o)
{
    paw_Env *P = ENV(S->C);
    switch (o.kind) {
        case IR_OBLIGATION_WELL_FORMED: {
            enum IrDefKind const def_kind = pawIr_get_kind(S->C, o.wf.did);
            if (def_kind == IR_TRAIT_DEF) {
                IrTrait *trait = pawIr_new_trait(S->C, o.wf.did, o.wf.args);
                pawL_add_fstring(P, buf, "  WellFormed(%s)\n",
                        pawIr_print_trait(S->C, trait));
            } else {
                paw_assert(def_kind == IR_ADT_DEF);
                IrType *type = pawIr_new_adt(S->C, o.wf.did, o.wf.args);
                pawL_add_fstring(P, buf, "  WellFormed(%s)\n",
                        pawIr_print_type(S->C, type));
            }
            break;
        }
        case IR_OBLIGATION_IMPL_TRAIT: {
            IrType *type = pawU_normalize_projections(S->C->U, o.impl.type);
            IrTrait *trait = pawIr_normalize_trait_projections(S->C, o.impl.trait);
            pawL_add_fstring(P, buf, "  %s: %s\n",
                    pawIr_print_type_v2(S->C, type)->text,
                    pawIr_print_trait_v2(S->C, trait)->text);
            break;
        }
        case IR_OBLIGATION_TYPE_EQUALS: {
            IrType *lhs = pawU_normalize_projections(S->C->U, o.eq.lhs);
            IrType *rhs = pawU_normalize_projections(S->C->U, o.eq.rhs);
            pawL_add_fstring(P, buf, "  %s = %s\n",
                    pawIr_print_type_v2(S->C, lhs)->text,
                    pawIr_print_type_v2(S->C, rhs)->text);
            break;
        }
    }
}

char const *debug_solver(IrSolver* S)
{
    Buffer buf;
    paw_Env *P = ENV(S->C);
    pawL_init_buffer(P, &buf);
    L_ADD_LITERAL(P, &buf, "obligations:\n");
    K_LIST_XFOREACH (S->obligations, struct IrObligation const, p) {
        print_obligation(S, &buf, *p);
    }
    L_ADD_LITERAL(P, &buf, "impl trait predicates:\n");
    K_LIST_XFOREACH (S->predicates, struct IrObligation const, p) {
        print_obligation(S, &buf, *p);
    }
    L_ADD_LITERAL(P, &buf, "type eq predicates:\n");
    K_LIST_XFOREACH (S->norm_targets, struct IrObligation const, p) {
        pawL_add_fstring(P, &buf, "  %s = %s\n",
                pawIr_print_type_v2(S->C, p->eq.lhs)->text,
                pawIr_print_type_v2(S->C, p->eq.rhs)->text);
    }
    return pawL_buffer_finish(P, &buf)->text;
}

void debug_typesystem(struct Compiler *C)
{
    for (int i = 0; i < C->hir->modules->count; ++i) {
        struct HirModule const m = HirModuleList_get(C->hir->modules, i);
        K_LIST_XFOREACH (m.items, struct HirDecl *const, pdecl) {
            DeclId const did = (*pdecl)->hdr.did;
            paw_Bool print_solver = PAW_TRUE;
            switch (HIR_KINDOF(*pdecl)) {
                case kHirAdtDecl: {
                    IrGenericArgs *args = pawIr_get_generic_args(C, did);
                    IrType *type = pawIr_solver_instantiate_type_with(C->S, did, args);
                    printf("ADT %s\n", pawIr_print_type_v2(C, type)->text);
                    break;
                }
                case kHirTraitDecl: {
                    IrGenericArgs *args = pawIr_get_generic_args(C, did);
                    IrTrait *trait = pawIr_new_trait(C, did, args);
                    printf("Trait %s\n", pawIr_print_trait_v2(C, trait)->text);
                    break;
                }
                case kHirImplDecl: {
                    IrGenericArgs *args = pawIr_get_generic_args(C, did);
                    struct IrImplInstance const inst = pawIr_solver_instantiate_impl_with(C->S, did, args);
                    printf("Impl %s for %s\n", pawIr_print_trait_v2(C, inst.trait)->text,
                            pawIr_print_type_v2(C, inst.type)->text);
                    break;
                }
                case kHirFnDecl: {
                    IrType *type = pawIr_get_def_type(C, did);
                    printf("Fn %s\n", pawIr_print_type_v2(C, type)->text);
                    break;
                }
                default:
                    print_solver = PAW_FALSE;
            }
            if (print_solver) {
                IrSolver *S = pawIr_push_solver(C);
                IrGenericArgs *args = pawIr_get_generic_args(C, did);
                pawIr_solver_add_obligations_from(S, did, args, (struct IrObligationCause){0});
                pawIr_solver_add_predicates_from(S, did, args, (struct IrObligationCause){0});
                puts(debug_solver(S));
                pawIr_pop_solver(C);
            }
        }
    }
}
