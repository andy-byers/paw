// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "solve.h"
#include "impl.h"
#include "ir_type.h"
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
    IrObligations *preconditions;
    IrType2List *norm_targets;
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
        .preconditions = IrObligations_new(C),
        .norm_targets = IrType2List_new(C),
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

void pawIr_solver_add_norm_target(IrSolver *S, IrType *type, IrType *target, struct IrObligationCause cause)
{
    IrType2List_push(S->C, S->norm_targets, (struct IrType2){type, target});
}

IrType *pawIr_solver_get_norm_target(IrSolver *S, IrType *type)
{
    paw_assert(IrIsProjection(type));
    do {
        K_LIST_XFOREACH (S->norm_targets, struct IrType2 const, p) {
            struct IrProjection const *x = IrGetProjection(type);
            struct IrProjection const *y = IrGetProjection(
                    pawU_normalize(S->U, p->first));
            struct IrAssocItem const *xitem = pawIr_get_assoc_item(S->C, x->assoc);
            struct IrAssocItem const *yitem = pawIr_get_assoc_item(S->C, y->assoc);
            if (pawS_eq(xitem->name, yitem->name)) {
                int const position = pawU_current_position(S->U);
                IrType *x_type = pawU_normalize_projections(S->U, x->type);
                IrType *y_type = pawU_normalize_projections(S->U, y->type);
                if (pawU_unify(S->U, x_type, y_type) == 0
                        && pawIr_unify_traits(S->C, x->trait, y->trait) == 0)
                    return pawU_normalize(S->U, p->second);
                pawU_undo_unifications(S->U, position);
            }
        }
        S = S->outer;
    } while (S != NULL);
    return NULL;
}

DEFINE_MAP(struct Compiler, PreconditionCache, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, void *)

static void add_precondition(IrSolver*S, IrType *type, IrTrait *trait, struct IrObligationCause cause, PreconditionCache *cache)
{
    if (PreconditionCache_insert(S->C, cache, trait->did, NULL))
        return;

    IrObligations_push(S->C, S->preconditions, (struct IrObligation){
                .kind = IR_OBLIGATION_IMPL_TRAIT,
                .cause = cause,
                .impl.type = type,
                .impl.trait = trait,
            });

    // If `type: trait` is true, then we also know any facts declared on associated
    // types in the definition of `trait`.
    IrGenericArgs *params = pawIr_get_generic_args(S->C, trait->did);
    struct Substitution const subst = {params, trait->args};
    IrConstraints const *constraints = pawIr_get_constraints(S->C, trait->did);
    K_LIST_XFOREACH (constraints, struct IrConstraint const, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT: {
                if (IrIsProjection(p->impl.type)) {
                    IrType *type = pawP_substitute(S->C, p->impl.type, subst);
                    IrTrait *trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                    add_precondition(S, type, trait, (struct IrObligationCause){0}, cache);
                }
                break;
            }
            case IR_CONSTRAINT_TYPE_EQUALS: {
                if (IrIsProjection(p->eq.lhs)) {
                    IrType *lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                    IrType *rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                    pawIr_solver_add_norm_target(S, lhs, rhs, (struct IrObligationCause){0});
                }
                break;
            }
        }
    }
}

void pawIr_solver_add_precondition(IrSolver *S, IrType *type, IrTrait *trait, struct IrObligationCause cause)
{
    PreconditionCache *cache = PreconditionCache_new(S->C);
    add_precondition(S, type, trait, cause, cache);
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

static void add_obligations(IrSolver *S, DeclId did, struct Substitution subst, PreconditionCache *cache)
{
    if (PreconditionCache_insert(S->C, cache, did, NULL))
        return;

    IrConstraints const *bounds = pawIr_get_constraints(S->C, did);
    K_LIST_XFOREACH (bounds, struct IrConstraint, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT: {
                IrType *type = pawP_substitute(S->C, p->impl.type, subst);
                IrTrait *trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                pawIr_solver_add_impl_trait_obligation(S, type, trait, (struct IrObligationCause){0});
                LOGLN("SOLVER:%p: add obligation `%s: %s`",
                        (void *)S, pawIr_print_type_v2(S->C, type)->text,
                        pawIr_print_trait_v2(S->C, trait)->text);
                break;
            }
            case IR_CONSTRAINT_TYPE_EQUALS: {
                IrType *lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                IrType *rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                pawIr_solver_add_type_equals_obligation(S, lhs, rhs, (struct IrObligationCause){0});
                LOGLN("SOLVER:%p: add obligation `%s = %s`",
                        (void *)S, pawIr_print_type_v2(S->C, lhs)->text,
                        pawIr_print_type_v2(S->C, rhs)->text);
                break;
            }
        }
    }
}

void pawIr_solver_add_obligations_from(IrSolver *S, DeclId parent_did, IrGenericArgs *args)
{
    PreconditionCache *cache = PreconditionCache_new(S->C);
    IrGenericArgs *params = pawIr_get_generic_args(S->C, parent_did);
    struct Substitution const subst = {params, args};
    add_obligations(S, parent_did, subst, cache);
}

void pawIr_solver_add_obligations_from_type(IrSolver *S, IrType *type)
{
    if (IrIsAdt(type)) {
        struct IrAdt const *adt = IrGetAdt(type);
        pawIr_solver_add_obligations_from(S, adt->did, adt->args);
    }
}

void pawIr_solver_add_obligations_from_trait(IrSolver *S, IrTrait *trait)
{
    pawIr_solver_add_obligations_from(S, trait->did, trait->args);
}

static paw_Bool impl_is_compatible(struct Compiler *C, IrType *self, struct IrImpl const *impl)
{
    paw_Bool matches = PAW_FALSE;
    WITH_SOLVER_CTX(C, child, {
        struct IrImplInstance const inst = pawIr_solver_instantiate_impl(child, impl->did);
        if (pawU_unify(C->U, inst.type, self) == 0) {
            struct IrSolverResult const result = pawIr_solver_solve(child);
            matches = result.status == IR_SOLVER_OK
                && result.num_unsolved == 0;
        }
        pawIr_solver_rollback(child);
    });

    return matches;
}

static paw_Bool matches_impl_precondition(IrSolver *S, IrType *type, IrTrait *trait)
{
    K_LIST_XFOREACH (S->preconditions, struct IrObligation const, p) {
        switch (p->kind) {
            case IR_OBLIGATION_IMPL_TRAIT:
                if (pawU_equals(S->U, type, p->impl.type)
                        && pawIr_trait_equals(S->C, trait, p->impl.trait)) {
                    LOGLN("SOLVER:%p: proved `%s: %s` using precondition",
                            (void *)S, pawIr_print_type(S->C, type),
                            pawIr_print_trait(S->C, trait));
                    return PAW_TRUE;
                }
                break;
            case IR_OBLIGATION_TYPE_EQUALS:
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

DEFINE_LIST(struct Compiler, Candidates, struct Candidate)

#define IMPL_ERROR -1
#define IMPL_FOUND 0
#define IMPL_NOT_FOUND 1

// NOTE: this function returns "IMPL_ERROR" on error (always indicates "multiple
//     applicable trait impls"), "IMPL_FOUND" if the type "self" implements the
//     trait "impl_trait", and "IMPL_NOT_FOUND" otherwise.
static int type_implements_trait(IrSolver *S, IrType *self, IrTrait *impl_trait, IrDefs **traits_for_error)
{
    IrSolver *cursor = S;
    while (cursor != NULL) {
        if (matches_impl_precondition(cursor, self, impl_trait))
            return IMPL_FOUND;
        cursor = cursor->outer;
    }

    struct Compiler *C = S->C;
    Candidates *candidates = Candidates_new(C);

    // add instantiated trait type from trait impl blocks where the context type is
    // compatible with "self"
    K_LIST_XFOREACH (C->impls.trait, DeclId const, p) {
        struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
        if (impl_is_compatible(C, self, impl)) {
            IrSolver *child = pawIr_push_solver(C);
            int const snapshot = pawU_current_position(C->U);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(child, impl->did);
            if (pawIr_unify_traits(C, inst.trait, impl_trait) == 0) {
                Candidates_push(C, candidates, (struct Candidate){
                            .trait_did = inst.trait->did,
                            .impl_did = impl->did,
                        });
                LOGLN("SOLVER:%p: proved `%s: %s` using trait impl block DefId(%d)",
                        (void *)S, pawIr_print_type(S->C, pawU_normalize_projections(S->U, inst.type)),
                        pawIr_print_trait(S->C, pawIr_normalize_trait(S->C, inst.trait)),
                        impl->did.value);
            }
            pawU_undo_unifications(C->U, snapshot);
            pawIr_solver_rollback(child);
            pawIr_pop_solver(C);
        }
    }

    // add instantiated trait types from blanket impl blocks
    K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
        struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
        if (impl_is_compatible(C, self, impl)) {
            IrSolver *child = pawIr_push_solver(C);
            int const snapshot = pawU_current_position(C->U);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(S, impl->did);
            pawU_unify_unchecked(C->U, inst.type, self);
            if (pawIr_unify_traits(C, inst.trait, impl_trait) == 0) {
                Candidates_push(C, candidates, (struct Candidate){
                            .trait_did = inst.trait->did,
                            .impl_did = impl->did,
                        });
                LOGLN("SOLVER:%p: proved `%s: %s` using blanket impl block DefId(%d)",
                        (void *)S, pawIr_print_type(S->C, impl->type),
                        pawIr_print_trait(S->C, impl->trait),
                        impl->did.value);
            }
            pawU_undo_unifications(C->U, snapshot);
            pawIr_solver_rollback(child);
            pawIr_pop_solver(C);
        }
    }

    if (candidates->count != 1)
        return IMPL_NOT_FOUND;

    struct Candidate const c = Candidates_first(candidates);
    struct IrImplInstance const inst = pawIr_solver_instantiate_impl(S, c.impl_did);
    pawU_unify_unchecked(C->U, inst.type, self);
    pawIr_unify_traits_unchecked(C, inst.trait, impl_trait);
    return IMPL_FOUND;
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
    // search for evidence in the form of a compatible impl block
    IrDefs *traits_for_error;
    int const result = type_implements_trait(S, type, trait, &traits_for_error);
    if (result == IMPL_ERROR)
        // TODO: refactor to avoid throwing an error here, let the caller do it
        pawErr_generic_error(ENV(S->C), SCAN_STR(S->C, "(solver)"),
                (struct SourceSpan){0}, "multiple applicable traits");
    return result == IMPL_FOUND;
}

static int solve_type_equals_obligation(IrSolver *S, IrType *lhs, IrType *rhs)
{
    int const position = pawU_current_position(S->U);
    if (pawU_unify(S->U, lhs, rhs) != 0) {
        pawU_undo_unifications(S->U, position);
        return -1;
    }
    return 0;
}

struct IrSolverResult pawIr_solver_solve(IrSolver *S)
{
    LOGLN("SOLVER:%p: starting solver invocation", (void *)S);

    struct IrSolverResult result = {
        .status = IR_SOLVER_OK,
    };
    paw_Bool solved_any;
    do {
        solved_any = PAW_FALSE;
        result.num_unsolved = S->obligations->count;
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
                    pawIr_solver_add_obligations_from(S, o.wf.did, o.wf.args);
                    solved = PAW_TRUE;
                    break;
                }
                case IR_OBLIGATION_IMPL_TRAIT: {
                    IrType *type = pawU_normalize_projections(S->U, o.impl.type);
                    IrTrait *trait = pawIr_normalize_trait_projections(S->C, o.impl.trait);
                    LOGLN("SOLVER:%p: encountered impl trait obligation `%s`",
                            (void *)S, pawIr_print_impl_trait_obligation(S->C, type, trait));
                    IrDefs *traits_for_error;
                    int status = IMPL_NOT_FOUND;
                    if (!IrIsInfer(type) && (status = type_implements_trait(
                                    S, type, trait, &traits_for_error)) == IMPL_FOUND) {
                        LOGLN("SOLVER:%p: proved impl trait obligation `%s`",
                                (void *)S, pawIr_print_impl_trait_obligation(S->C, type, trait));

                        solved = PAW_TRUE;
                    } else if (status == IMPL_NOT_FOUND
                            && !pawIr_type_contains_inference_var(S->C, type)
                            && !pawIr_trait_contains_inference_var(S->C, trait)) {
                        // solver was not blocked by the unifier, indicating an
                        // unprovable obligation
                        struct IrSolverResult result;
                        LOGLN("SOLVER:%p: unprovable impl trait obligation \"%s\"",
                                (void *)S, pawIr_print_impl_trait_obligation(S->C, type, trait));
                        result.status = IR_SOLVER_CANNOT_PROVE_OBLIGATION;
                        result.cpo.obligation = o;
                        return result;
                    } else if (status == IMPL_ERROR) {
                        result.status = IR_SOLVER_MULTIPLE_APPLICABLE_TRAITS;
                        result.mat.traits = traits_for_error;
                        return result;
                    }
                    break;
                }
                case IR_OBLIGATION_TYPE_EQUALS: {
                    IrType *lhs = pawU_normalize_projections(S->U, o.eq.lhs);
                    IrType *rhs = pawU_normalize_projections(S->U, o.eq.rhs);
                    if (solve_type_equals_obligation(S, lhs, rhs) == 0) {
                        LOGLN("SOLVER:%p: proved type equals obligation `%s := %s`",
                                (void *)S, pawIr_print_type(S->C, lhs),
                                pawIr_print_type(S->C, rhs));

                        solved = PAW_TRUE;
                    } else if (!pawIr_type_contains_inference_var(S->C, lhs)
                            && !pawIr_type_contains_inference_var(S->C, rhs)) {
                        LOGLN("SOLVER:%p: unable to solve type equals obligation `%s := %s`",
                                (void *)S, pawIr_print_type(S->C, lhs),
                                pawIr_print_type(S->C, rhs));
                        result.status = IR_SOLVER_CANNOT_PROVE_OBLIGATION;
                        result.cpo.obligation = o;
                        return result;
                    }
                    break;
                }
            }
            if (solved) {
                IrObligations_swap_remove(S->obligations, i--);
                solved_any = PAW_TRUE;
            }
        }
    } while (solved_any);

    paw_assert(result.status == IR_SOLVER_OK);
    paw_assert(result.num_unsolved == S->obligations->count);
    LOGLN("SOLVER:%p: finishing solver invocation successfully (unsolved obligations = %d)",
            (void *)S, result.num_unsolved);
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

void pawIr_solver_rollback(IrSolver *S)
{
    S->obligations->count = 0;
    S->preconditions->count = 0;
    S->norm_targets->count = 0;
}

void pawIr_solver_commit(IrSolver *S)
{
    if (S->outer != NULL) {
        // TODO: likely going to end up with many duplicates. should probably do something about that as performance could get very bad
        K_LIST_XFOREACH (S->obligations, struct IrObligation const, p)
            IrObligations_push(S->C, S->outer->obligations, *p);
        K_LIST_XFOREACH (S->preconditions, struct IrObligation const, p)
            IrObligations_push(S->C, S->outer->preconditions, *p);
        K_LIST_XFOREACH (S->norm_targets, struct IrType2 const, p)
            pawIr_solver_add_norm_target(S->outer, p->first, p->second, (struct IrObligationCause){0});
    }

    S->obligations->count = 0;
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
//    pawIr_solver_add_preconditions_from(S, did, args);
    IrType *base = pawIr_get_def_type(S->C, did);
    return pawP_substitute(S->C, base, subst);
}

IrTrait *pawIr_solver_instantiate_trait(IrSolver *S, DeclId did)
{
    IrTrait *trait = pawIr_get_trait(S->C, did);
    IrGenericArgs *args = pawIr_instantiate_args(S->C, did);
    struct Substitution const subst = {trait->args, args};
//    pawIr_solver_add_preconditions_from(S, did, args);
    return substitute_trait(S->C, trait, subst);
}

IrTrait *pawIr_solver_instantiate_trait_with(IrSolver *S, DeclId did, IrGenericArgs *args)
{
    IrTrait *trait = pawIr_get_trait(S->C, did);
    struct Substitution const subst = {trait->args, args};
//    pawIr_solver_add_preconditions_from(S, did, args);
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
                .items = def->items,
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
//    pawIr_solver_add_preconditions_from(S, did, args);
    return (struct IrImplInstance){
        .type = pawP_substitute(S->C, impl->type, subst),
        .trait = impl->trait == NULL ? NULL :
            substitute_trait(S->C, impl->trait, subst),
        .items = NULL, // TODO
    };
}

struct Instance {
    DeclId did;
    IrGenericArgs *args;
};

static paw_Uint instance_hash(struct Compiler *C, struct Instance inst)
{
    paw_Uint hash = P_ID_HASH(C, inst.did);
    K_LIST_XFOREACH (inst.args, IrGenericArg const, p)
        hash = hash_combine(hash, pawIr_arg_hash(C, *p));
    return hash;
}

static paw_Bool instance_equals(struct Compiler *C, struct Instance lhs, struct Instance rhs)
{
    if (!P_ID_EQUALS(C, lhs.did, rhs.did))
        return PAW_FALSE;
    IrGenericArg const *x, *y;
    K_LIST_ZIP(lhs.args, x, rhs.args, y) {
        if (!pawIr_arg_equals(C, *x, *y))
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

static void add_type_preconditions(IrSolver *S, IrType *type, PreconditionCache *cache);
static void add_trait_preconditions(IrSolver *S, IrTrait *trait, PreconditionCache *cache);

static void add_preconditions(IrSolver *S, DeclId did, struct Substitution subst, PreconditionCache *cache)
{
    if (PreconditionCache_insert(S->C, cache, did, NULL))
        return;

    IrConstraints const *bounds = pawIr_get_constraints(S->C, did);
    K_LIST_XFOREACH (bounds, struct IrConstraint, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT: {
                IrType *type = pawP_substitute(S->C, p->impl.type, subst);
                IrTrait *trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                type = pawU_normalize(S->U, type);
                trait = pawIr_normalize_trait(S->C, trait);
                add_type_preconditions(S, type, cache);
                add_trait_preconditions(S, trait, cache);
                pawIr_solver_add_precondition(S, type, trait, (struct IrObligationCause){0});
                break;
            }
            case IR_CONSTRAINT_TYPE_EQUALS: {
                IrType *lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                IrType *rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                lhs = pawU_normalize(S->U, lhs);
                rhs = pawU_normalize(S->U, rhs);
                add_type_preconditions(S, lhs, cache);
                add_type_preconditions(S, rhs, cache);
                pawIr_solver_add_norm_target(S, lhs, rhs, (struct IrObligationCause){0});
                break;
            }
        }
    }
}

static void add_type_preconditions(IrSolver *S, IrType *type, PreconditionCache *cache)
{
    if (!IrIsAdt(type)) return;
    struct IrAdt const *adt = IrGetAdt(type);
    IrGenericArgs *params = pawIr_get_generic_args(S->C, adt->did);
    struct Substitution const subst = {params, adt->args};
    add_preconditions(S, adt->did, subst, cache);
}

static void add_trait_preconditions(IrSolver *S, IrTrait *trait, PreconditionCache *cache)
{
    IrGenericArgs *params = pawIr_get_generic_args(S->C, trait->did);
    struct Substitution const subst = {params, trait->args};
    add_preconditions(S, trait->did, subst, cache);
}

void pawIr_solver_add_preconditions_from(IrSolver *S, DeclId did, IrGenericArgs *args)
{
    PreconditionCache *cache = PreconditionCache_new(S->C);
    IrGenericArgs *params = pawIr_get_generic_args(S->C, did);
    struct Substitution const subst = {params, args};
    add_preconditions(S, did, subst, cache);
}

Str const *pawIr_print_obligation_(struct Compiler *C, struct IrObligation obligation)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    switch (obligation.kind) {
        case IR_OBLIGATION_WELL_FORMED: {
            enum IrDefKind const def_kind = pawIr_get_kind(C, obligation.wf.did);
            if (def_kind == IR_TRAIT_DEF) {
                IrTrait *trait = pawIr_new_trait(C, obligation.wf.did, obligation.wf.args);
                pawL_add_fstring(P, &buf, "  WellFormed(%s)\n",
                        pawIr_print_trait(C, trait));
            } else {
                paw_assert(def_kind == IR_ADT_DEF);
                IrType *type = pawIr_new_adt(C, obligation.wf.did, obligation.wf.args);
                pawL_add_fstring(P, &buf, "  WellFormed(%s)\n",
                        pawIr_print_type(C, type));
            }
            break;
        }
        case IR_OBLIGATION_IMPL_TRAIT: {
            IrType *type = pawU_normalize_projections(C->U, obligation.impl.type);
            IrTrait *trait = pawIr_normalize_trait_projections(C, obligation.impl.trait);
            pawL_add_fstring(P, &buf, "%s: %s",
                    pawIr_print_type_v2(C, type)->text,
                    pawIr_print_trait_v2(C, trait)->text);
            break;
        }
        case IR_OBLIGATION_TYPE_EQUALS: {
            IrType *lhs = pawU_normalize_projections(C->U, obligation.eq.lhs);
            IrType *rhs = pawU_normalize_projections(C->U, obligation.eq.rhs);
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
    K_LIST_XFOREACH (S->preconditions, struct IrObligation const, p) {
        print_obligation(S, &buf, *p);
    }
    L_ADD_LITERAL(P, &buf, "type eq predicates:\n");
    K_LIST_XFOREACH (S->norm_targets, struct IrType2 const, p) {
        pawL_add_fstring(P, &buf, "  %s = %s\n",
                pawIr_print_type_v2(S->C, p->first)->text,
                pawIr_print_type_v2(S->C, p->second)->text);
    }
    return pawL_buffer_finish(P, &buf)->text;
}

