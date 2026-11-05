// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "solve.h"
#include "impl.h"
#include "resolver.h" // UpvalueList
#include "trait.h"
#include "unify.h"

#include "hir/hir.h"
#include "ir/type.h"

#include <stdio.h>

#define TODO ((struct SourceSpan){0})

#if defined(PAW_SOLVER_DEBUG)
# define LOGLN(Fmt_, ...) fprintf(stderr, "(paw_solver) "Fmt_ "\n", __VA_ARGS__)
#else
# define LOGLN(...)
#endif

#define WITH_SOLVER_CTX(C_, SolverName_, Code_) do { \
            struct UnifierState const save_ = unifier_state((C_)->U); \
            IrSolver *SolverName_ = ir_push_solver(C_); \
            (void)SolverName_; \
            Code_ \
            undo_unifications((C_)->U, save_); \
            ir_pop_solver(C_); \
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
        LIST_XFOREACH(S->obligations, struct IrObligation const, p){
            switch (p->kind) {
                case IR_OBLIGATION_WELL_FORMED: {
                    enum IrDefKind const def_kind = ir_get_kind(S->C, p->wf.did);
                    if (def_kind == IR_TRAIT_DEF) {
                        IrTrait *trait = ir_new_trait(S->C, p->wf.did, p->wf.args);
                        printf("  WellFormed(%s)\n", ir_print_trait(S->C, trait));
                    } else {
                        paw_assert(def_kind == IR_ADT_DEF);
                        IrType *type = ir_new_adt(S->C, p->wf.did, p->wf.args);
                        printf("  WellFormed(%s)\n", ir_print_type(S->C, type));
                    }
                    break;
                }
                case IR_OBLIGATION_IMPL_TRAIT: {
                    IrType *type = normalize_projections(S->C->U, p->impl.type);
                    IrTrait *trait = ir_normalize_trait(S->C, p->impl.trait);
                    printf("  %s: %s\n", ir_print_type(S->C, type), ir_print_trait(S->C, trait));
                    break;
                }
                case IR_OBLIGATION_TYPE_EQUALS:
                    printf("  %s := %s\n", ir_print_type(S->C, p->eq.lhs), ir_print_type(S->C, p->eq.rhs));
                    break;
                case IR_OBLIGATION_CONST_EQUALS:
                    printf("  %s := %s\n", ir_print_const(S->C, p->keq.lhs)->text, ir_print_const(S->C, p->keq.rhs)->text);
                    break;
            }
        }
    }
}

IrSolver *ir_push_solver(struct Compiler *C)
{
    IrSolver *S = compiler_alloc(C, NULL, 0, sizeof(*S));
    *S = (IrSolver){
        .obligations = IrObligations_new(ALLOCATOR_OF(C)),
        .predicates = IrObligations_new(ALLOCATOR_OF(C)),
        .norm_targets = IrObligations_new(ALLOCATOR_OF(C)),
        .outer = C->S,
        .U = C->U,
        .C = C,
    };
    C->S = S;
    return S;
}

void ir_pop_solver(struct Compiler *C)
{
    C->S = C->S->outer;
}

static IrType *find_norm_target(IrSolver *S, IrType *type)
{
    LIST_XFOREACH (S->norm_targets, struct IrObligation const, p) {
        struct UnifierState const state = unifier_state(S->U);
        if (unify_types(S->U, type, p->eq.lhs) == 0) return p->eq.rhs;
        undo_unifications(S->U, state);
    }
    return NULL;
}

void ir_solver_add_norm_target(IrSolver *S, IrType *type, IrType *target, struct IrObligationCause cause)
{
    IrObligations_push(S->norm_targets, (struct IrObligation){
                .kind = IR_OBLIGATION_TYPE_EQUALS,
                .eq.lhs = type,
                .eq.rhs = target,
                .cause = cause,
            });

    LOGLN("SOLVER:%p: add norm target `%s = %s`",
            (void *)S, ir_print_type_v2(S->C, type)->text,
            ir_print_type_v2(S->C, target)->text);
}

IrType *ir_solver_get_norm_target(IrSolver *S, IrType *type)
{
    paw_assert(IrIsProjection(type));
    type = normalize_type(S->U, type);
    if (!ir_type_contains_inference_var(S->C, type)) {
        do {
            IrType *target = find_norm_target(S, type);
            if (target != NULL) return target;
            S = S->outer;
        } while (S != NULL);
    }
    return NULL;
}

DEFINE_MAP(PredicateCache, P_ID_HASH, P_ID_EQUALS, DeclId, void *)

void ir_solver_add_predicate(IrSolver *S, IrType *type, IrTrait *trait, struct IrObligationCause cause)
{
    LIST_XFOREACH (S->predicates, struct IrObligation const, p) {
        struct UnifierState const state = unifier_state(S->U);
        paw_Bool const matches = unify_types(S->U, type, p->impl.type) == 0
                && ir_unify_traits(S->C, trait, p->impl.trait) == 0;
        undo_unifications(S->U, state);
        if (matches) return;
    }

    IrObligations_push(S->predicates, (struct IrObligation){
                .kind = IR_OBLIGATION_IMPL_TRAIT,
                .cause = cause,
                .impl.type = type,
                .impl.trait = trait,
            });

    LOGLN("SOLVER:%p: add predicate `%s: %s`",
            (void *)S, ir_print_type_v2(S->C, type)->text,
            ir_print_trait_v2(S->C, trait)->text);
}

static IrTrait *substitute_trait(struct Compiler *C, IrTrait *trait, struct Substitution subst)
{
    IrGenericArgs *types = IrGenericArgs_new(ALLOCATOR_OF(C));
    IrGenericArgs_reserve(types, trait->args->count);
    LIST_XFOREACH (trait->args, IrGenericArg const, p) {
        if (IrGenericArg_is_type(*p)) {
            IrType *t = pawP_substitute(C, IrGenericArg_get_type(*p), subst);
            IrGenericArgs_push(types, IrGenericArg_from_type(t));
        } else {
            IrGenericArgs_push(types, *p);
        }
    }
    return ir_new_trait(C, trait->did, types);
}

void ir_solver_add_well_formed_obligation(IrSolver *S, DeclId did, IrGenericArgs *args, struct IrObligationCause cause)
{
    IrObligations_push(S->obligations, (struct IrObligation){
            .kind = IR_OBLIGATION_WELL_FORMED,
            .cause = cause,
            .wf.did = did,
            .wf.args = args,
        });
}

#define ENSURE_TYPE_IS_WELL_FORMED(S_, Type_, Cause_) \
    (IrIsAdt(Type_) ? ir_solver_add_well_formed_obligation(S_, IR_TYPE_DID(Type_), \
            IR_GENERIC_ARGS(Type_), Cause_) : (void)0)
#define ENSURE_TRAIT_IS_WELL_FORMED(S_, Trait_, Cause_) \
    ir_solver_add_well_formed_obligation(S_, (Trait_)->did, (Trait_)->args, Cause_)

void ir_solver_add_impl_trait_obligation(IrSolver *S, struct IrType *type, struct IrTrait *trait, struct IrObligationCause cause)
{
    IrObligations_push(S->obligations, (struct IrObligation){
            .kind = IR_OBLIGATION_IMPL_TRAIT,
            .cause = cause,
            .impl.trait = trait,
            .impl.type = type,
        });
}

void ir_solver_add_type_equals_obligation(IrSolver *S, struct IrType *lhs, struct IrType *rhs, struct IrObligationCause cause)
{
    IrObligations_push(S->obligations, (struct IrObligation){
            .kind = IR_OBLIGATION_TYPE_EQUALS,
            .cause = cause,
            .eq.lhs = lhs,
            .eq.rhs = rhs,
        });
}

void ir_solver_add_const_equals_obligation(IrSolver *S, struct IrConst *lhs, struct IrConst *rhs, struct IrObligationCause cause)
{
    IrObligations_push(S->obligations, (struct IrObligation){
            .kind = IR_OBLIGATION_CONST_EQUALS,
            .cause = cause,
            .keq.lhs = lhs,
            .keq.rhs = rhs,
        });
}

static void add_obligations(IrSolver *S, DeclId did, struct Substitution subst, PredicateCache *cache, struct IrObligationCause cause)
{
    if (PredicateCache_insert(cache, did, NULL))
        return;

    IrConstraints const *bounds = ir_get_constraints(S->C, did);
    LIST_XFOREACH (bounds, struct IrConstraint, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT: {
                IrType *type = pawP_substitute(S->C, p->impl.type, subst);
                IrTrait *trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                ir_solver_add_impl_trait_obligation(S, type, trait, cause);
                LOGLN("SOLVER:%p: add obligation `%s: %s`",
                        (void *)S, ir_print_type_v2(S->C, type)->text,
                        ir_print_trait_v2(S->C, trait)->text);
                break;
            }
            case IR_CONSTRAINT_TYPE_EQUALS: {
                IrType *lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                IrType *rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                ir_solver_add_type_equals_obligation(S, lhs, rhs, cause);
                LOGLN("SOLVER:%p: add obligation `%s = %s`",
                        (void *)S, ir_print_type_v2(S->C, lhs)->text,
                        ir_print_type_v2(S->C, rhs)->text);
                break;
            }
        }
    }
}

void ir_solver_add_obligations_from(IrSolver *S, DeclId parent_did, IrGenericArgs *args, struct IrObligationCause cause)
{
    PredicateCache *cache = PredicateCache_new(ALLOCATOR_OF(S->C));
    IrGenericArgs *params = ir_get_generic_args(S->C, parent_did);
    struct Substitution const subst = {params, args};
    add_obligations(S, parent_did, subst, cache, cause);
}

void ir_solver_add_obligations_from_type(IrSolver *S, IrType *type, struct IrObligationCause cause)
{
    if (IrIsAdt(type)) {
        struct IrAdt const *adt = IrGetAdt(type);
        ir_solver_add_obligations_from(S, adt->did, adt->args, cause);
    } else if (IrIsSignature(type)) {
        struct IrSignature const *fn = IrGetSignature(type);
        ir_solver_add_obligations_from(S, fn->did, fn->args, cause);
    }
}

void ir_solver_add_obligations_from_trait(IrSolver *S, IrTrait *trait, struct IrObligationCause cause)
{
    ir_solver_add_obligations_from(S, trait->did, trait->args, cause);
}

static paw_Bool impl_is_compatible(struct Compiler *C, IrType *self, IrTrait *trait, struct IrImpl const *impl, struct IrObligationCause cause)
{
    paw_Bool matches = PAW_FALSE;
    WITH_SOLVER_CTX(C, child, {
        struct IrImplInstance const inst = ir_solver_instantiate_impl(child, impl->did, cause.span);
        if (unify_types(C->U, inst.type, self) == 0
                && ir_unify_traits(C, inst.trait, trait) == 0) {
            ir_solver_add_obligations_from(child, impl->did, inst.args, cause);
            struct IrSolverResult const result = ir_solver_solve(child);
            matches = result.status != IR_SOLVER_ERROR;
        }
    });

    return matches;
}

static paw_Bool matches_impl_predicate(IrSolver *S, IrType *type, IrTrait *trait)
{
    paw_assert(!IrIsInfer(type));
    LIST_XFOREACH (S->predicates, struct IrObligation const, p) {
        switch (p->kind) {
            case IR_OBLIGATION_IMPL_TRAIT: {
                    struct UnifierState const state = unifier_state(S->U);
                    if (unify_types(S->U, type, p->impl.type) == 0
                            && ir_unify_traits(S->C, trait, p->impl.trait) == 0) {
                        LOGLN("SOLVER:%p: proved `%s: %s` using predicate",
                                (void *)S, ir_print_type(S->C, type),
                                ir_print_trait(S->C, trait));
                        return PAW_TRUE;
                    }
                    undo_unifications(S->U, state);
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

DEFINE_LIST(Candidates, struct Candidate)

static enum IrSolverStatus type_implements_trait(IrSolver *S, IrType *self, IrTrait *impl_trait, struct IrObligationCause cause)
{
    IrSolver *cursor = S;
    while (cursor != NULL) {
        if (matches_impl_predicate(cursor, self, impl_trait))
            return IR_SOLVER_SOLVED;
        cursor = cursor->outer;
    }

    struct Compiler *C = S->C;
    Candidates *candidates = Candidates_new(ALLOCATOR_OF(C));

    // add instantiated trait type from trait impl blocks where the context type is
    // compatible with "self"
    IrDefs const *trait_defs = ir_trait_impls_for(C, self);
    LIST_XFOREACH (trait_defs, DeclId const, p) {
        struct IrImpl const *impl = ir_get_impl_def(C, *p);
        if (impl_is_compatible(C, self, impl_trait, impl, cause)) {
            Candidates_push(candidates, (struct Candidate){
                        .trait_did = impl->trait->did,
                        .impl_did = impl->did,
                    });

            LOGLN("SOLVER:%p: proved `%s: %s` using trait impl block DefId(%d)",
                    (void *)S, ir_print_type(S->C, normalize_projections(S->U, impl->type)),
                    ir_print_trait(S->C, ir_normalize_trait(S->C, impl->trait)),
                    impl->did.value);
        }
    }

    // add instantiated trait types from blanket impl blocks
    LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
        struct IrImpl const *impl = ir_get_impl_def(C, *p);
        if (impl_is_compatible(C, self, impl_trait, impl, cause)) {
            Candidates_push(candidates, (struct Candidate){
                        .trait_did = impl->trait->did,
                        .impl_did = impl->did,
                    });
            LOGLN("SOLVER:%p: proved `%s: %s` using blanket impl block DefId(%d)",
                    (void *)S, ir_print_type(S->C, impl->type),
                    ir_print_trait(S->C, impl->trait),
                    impl->did.value);
        }
    }

    if (candidates->count == 0)
        return IR_SOLVER_ERROR;
    if (candidates->count > 1)
        return IR_SOLVER_AMBIGUOUS;

    {
        IrSolver *child = ir_push_solver(S->C);
        struct Candidate const c = Candidates_first(candidates);
        struct IrImplInstance const inst = ir_solver_instantiate_impl(child, c.impl_did, cause.span);
        ir_solver_add_obligations_from(child, c.impl_did, inst.args, cause);
        unify_types_unchecked(C->U, inst.type, self);
        ir_unify_traits_unchecked(C, inst.trait, impl_trait);
        struct IrSolverResult const r = ir_solver_solve(child);
        paw_assert(r.status != IR_SOLVER_ERROR); PAW_UNUSED(r);
        ir_pop_solver(child->C);
    }
    return IR_SOLVER_SOLVED;
}

static IrTrait *adjust_trait(IrSolver *S, IrTrait *trait, IrType *self)
{
    IrGenericArgs *args = IrGenericArgs_new(ALLOCATOR_OF(S->C));
    IrGenericArgs_push(args, IrGenericArg_from_type(self));
    for (int i = 1; i < trait->args->count; ++i)
        IrGenericArgs_push(args, IrGenericArgs_get(trait->args, i));
    return ir_new_trait(S->C, trait->did, args);
}

paw_Bool ir_type_implements_trait(IrSolver *S, IrType *type, IrTrait *trait)
{
    type = normalize_projections(S->U, type);
    trait = ir_normalize_trait_projections(S->C, trait);

    if (ir_type_contains_inference_var(S->C, type)
            || ir_trait_contains_inference_var(S->C, trait))
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

    IrType *type = normalize_projections(S->U, projection);
    target = normalize_projections(S->U, target);

// TODO: probably need to loop until steady state reached in p_normalize_projections
type = normalize_projections(S->U, projection);
target = normalize_projections(S->U, target);

    if (IrIsProjection(type)) {
        struct IrProjection const *p = IrGetProjection(type);
        if (IrIsInfer(ir_projection_self(p)))
            return IR_SOLVER_AMBIGUOUS;
    }

    struct UnifierState const state = unifier_state(S->U);
    if (unify_types(S->U, type, target) != 0) {
        undo_unifications(S->U, state);
        return IR_SOLVER_ERROR;
    }
    return IR_SOLVER_SOLVED;
}

static enum CoreTrait check_core_trait_in_range(struct Compiler *C, DeclId did, enum CoreTrait lower_bound, enum CoreTrait upper_bound)
{
    for (enum CoreTrait t = lower_bound; t < upper_bound; ++t) {
        if (P_ID_EQUALS(did, C->core_traits[t]))
            return t;
    }
    return NUM_CORE_TRAITS;
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

struct IrSolverResult ir_solver_solve(IrSolver *S)
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
                            (void *)S, ir_get_kind(S->C, o.wf.did) == IR_TRAIT_DEF
                                ? ir_print_trait(S->C, ir_new_trait(S->C, o.wf.did, o.wf.args))
                                : ir_print_type(S->C, ir_new_adt(S->C, o.wf.did, o.wf.args)));
                    // construct is considered well formed if all of the obligations it imposes
                    // can be proved true
                    IrSolver *child = ir_push_solver(S->C);
                    ir_solver_add_obligations_from(child, o.wf.did, o.wf.args, o.cause);
                    LIST_XFOREACH (o.wf.args, IrGenericArg const, arg) {
                        if (IrGenericArg_is_type(*arg)) {
                            IrType *arg_type = IrGenericArg_get_type(*arg);
                            if (IrIsAdt(arg_type))
                                ir_solver_add_well_formed_obligation(child, IR_TYPE_DID(arg_type), IR_GENERIC_ARGS(arg_type), o.cause);
                        }
                    }
                    struct IrSolverResult const r = ir_solver_solve(child);
                    ir_pop_solver(S->C);

                    if (r.status == IR_SOLVER_ERROR)
                        return RESULT_ERROR(r.error.obligation);
                    solved = r.status == IR_SOLVER_SOLVED;
                    break;
                }
                case IR_OBLIGATION_IMPL_TRAIT: {
                    IrType *type = normalize_projections(S->U, o.impl.type);
                    IrTrait *trait = ir_normalize_trait_projections(S->C, o.impl.trait);
                    LOGLN("SOLVER:%p: encountered impl trait obligation `%s`",
                            (void *)S, ir_print_impl_trait_obligation(S->C, type, trait));

                    if (IrIsInfer(type)) {
                        struct IrInfer const *infer = IrGetInfer(type);
                        enum CoreTrait const LOWER_BOUND = CORE_TRAIT_EQ;
                        enum CoreTrait const upper_bound = infer->ikind == IR_INFER_INTEGER ? NUM_CORE_TRAITS :
                            infer->ikind == IR_INFER_FLOAT ? CORE_TRAIT_MOD + 1 : LOWER_BOUND;
                        enum CoreTrait const core_trait = check_core_trait_in_range(S->C, trait->did, LOWER_BOUND, upper_bound);
                        if (core_trait != NUM_CORE_TRAITS) {
                            paw_assert(trait->args->count == 2);
                            IrType *rhs = IrGenericArg_get_type(IrGenericArgs_get(trait->args, 1));
                            solved = unify_types(S->U, type, rhs) == 0;
                        }
                        break;
                    }

                    IrSolver *child = ir_push_solver(S->C);
                    if (IrIsProjection(type)) {
                        struct IrProjection const *p = IrGetProjection(type);
                        IrTrait *trait = ir_get_projection_trait(S->C, p);
                        ir_solver_add_predicates_from_trait(child, trait, o.cause);
                    }
                    enum IrSolverStatus const status = type_implements_trait(child, type, trait, o.cause);
                    ir_pop_solver(S->C);

                    if (status == IR_SOLVER_SOLVED) {
                        LOGLN("SOLVER:%p: proved impl trait obligation `%s`",
                                (void *)S, ir_print_impl_trait_obligation(S->C, type, trait));

                        solved = PAW_TRUE;
                    } else if (status == IR_SOLVER_ERROR) {
                        LOGLN("SOLVER:%p: unprovable impl trait obligation \"%s\"",
                                (void *)S, ir_print_impl_trait_obligation(S->C, type, trait));
                        return RESULT_ERROR(o);
                    }
                    break;
                }
                case IR_OBLIGATION_TYPE_EQUALS: {
                    IrType *lhs = normalize_type(S->U, o.eq.lhs);
                    IrType *rhs = normalize_type(S->U, o.eq.rhs);

                    switch (solve_normalizes_to_obligation(S, lhs, rhs)) {
                        case IR_SOLVER_SOLVED: {
                            LOGLN("SOLVER:%p: proved type equals obligation `%s = %s`",
                                    (void *)S, ir_print_type(S->C, lhs),
                                    ir_print_type(S->C, rhs));

                            solved = PAW_TRUE;
                            break;
                        }
                        case IR_SOLVER_AMBIGUOUS:
                            break;
                        case IR_SOLVER_ERROR: {
                            LOGLN("SOLVER:%p: unable to solve type equals obligation `%s = %s`",
                                    (void *)S, ir_print_type(S->C, lhs),
                                    ir_print_type(S->C, rhs));
                            return RESULT_ERROR(o);
                        }
                    }
                    break;
                }
                case IR_OBLIGATION_CONST_EQUALS: {
                    IrConst *lhs = normalize_const(S->U, o.keq.lhs);
                    IrConst *rhs = normalize_const(S->U, o.keq.rhs);
                    if (lhs->kind == IR_CONST_PENDING
                            || rhs->kind == IR_CONST_PENDING)
                        break; // ambiguous
                    if (unify_consts(S->U, lhs, rhs) != 0)
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

void ir_solver_solve_all_or_error(IrSolver *S)
{
    struct IrSolverResult const r = ir_solver_solve(S);
    if (r.status != IR_SOLVER_SOLVED)
        THROW_ERROR(S->C, Unsupported,
                .modname = S->C->modname,
                .span = {0});
}

IrObligations *ir_solver_remove_const_obligations(IrSolver *S)
{
    IrObligations *result = IrObligations_new(ALLOCATOR_OF(S->C));
    for (int i = 0; i < S->obligations->count;) {
        struct IrObligation const o = IrObligations_get(S->obligations, i);
        if (o.kind == IR_OBLIGATION_CONST_EQUALS) {
            IrObligations_push(result, o);
            IrObligations_swap_remove(S->obligations, i);
        } else {
            ++i;
        }
    }
    return result;
}

int ir_solver_num_obligations(IrSolver const *S)
{
    return S->obligations->count;
}

struct IrObligation ir_solver_first_obligation(IrSolver const *S)
{
    return IrObligations_first(S->obligations);
}

IrType *ir_solver_instantiate_type(IrSolver *S, DeclId did, struct SourceSpan span)
{
    IrGenericArgs *params = ir_get_generic_args(S->C, did);
    if (params == NULL) return ir_get_def_type(S->C, did);
    IrGenericArgs *args = ir_instantiate_args(S->C, did, span);
    return ir_solver_instantiate_type_with(S, did, args);
}

IrType *ir_solver_instantiate_type_with(IrSolver *S, DeclId did, IrGenericArgs *args)
{
    IrGenericArgs *params = ir_get_generic_args(S->C, did);
    struct Substitution const subst = {params, args};
    IrType *base = ir_get_def_type(S->C, did);
    return pawP_substitute(S->C, base, subst);
}

IrTrait *ir_solver_instantiate_trait(IrSolver *S, DeclId did, struct SourceSpan span)
{
    IrTrait *trait = ir_get_trait(S->C, did);
    IrGenericArgs *args = ir_instantiate_args(S->C, did, span);
    struct Substitution const subst = {trait->args, args};
    return substitute_trait(S->C, trait, subst);
}

IrTrait *ir_solver_instantiate_trait_with(IrSolver *S, DeclId did, IrGenericArgs *args)
{
    IrTrait *trait = ir_get_trait(S->C, did);
    struct Substitution const subst = {trait->args, args};
    return substitute_trait(S->C, trait, subst);
}

struct IrImplInstance ir_solver_instantiate_impl(IrSolver *S, DeclId did, struct SourceSpan span)
{
    IrGenericArgs *params = ir_get_generic_args(S->C, did);
    if (params == NULL) {
        struct IrImpl const *def = ir_get_impl_def(S->C, did);
        return (struct IrImplInstance){
                .type = def->type,
                .trait = def->trait,
                .args = NULL,
            };
    }
    IrGenericArgs *args = ir_instantiate_args(S->C, did, span);
    return ir_solver_instantiate_impl_with(S, did, args);
}

struct IrImplInstance ir_solver_instantiate_impl_with(IrSolver *S, DeclId did, IrGenericArgs *args)
{
    struct IrImpl const *impl = ir_get_impl_def(S->C, did);
    IrGenericArgs *params = ir_get_generic_args(S->C, did);
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
    IrGenericArgs *result = IrGenericArgs_new(ALLOCATOR_OF(C));
    IrGenericArgs_reserve(result, args->count);
    LIST_XFOREACH (args, IrGenericArg const, arg)
        IrGenericArgs_push(result, *arg);

    IrGenericArgs_set(result, 0, IrGenericArg_from_type(target));
    return result;
}

static paw_Bool target_is_self(IrSolver *S, IrType *target, struct IrGeneric const *self)
{
    if (IrIsProjection(target)) {
        struct IrProjection const *t = IrGetProjection(target);
        return target_is_self(S, ir_projection_self(t), self);
    } else if (IrIsGeneric(target)) {
        return P_ID_EQUALS(IR_TYPE_DID(target), self->did);
    } else {
        return PAW_FALSE;
    }
}

static IrConstraints *add_supertrait_constraints(IrSolver *S, IrTrait *trait, struct IrObligationCause cause)
{
    IrGenericArgs *params = ir_get_generic_args(S->C, trait->did);
    struct Substitution const subst = {params, trait->args};
    struct IrGeneric const *self_param = IrGetGeneric(
            IrGenericArg_get_type(IrGenericArgs_first(params)));
    IrConstraints const *constraints = ir_get_constraints(S->C, trait->did);
    IrConstraints *result = IrConstraints_new(ALLOCATOR_OF(S->C));
    LIST_XFOREACH (constraints, struct IrConstraint const, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT:
                if (target_is_self(S, p->impl.type, self_param)) {
                    IrType *impl_type = pawP_substitute(S->C, p->impl.type, subst);
                    IrTrait *impl_trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                    ir_solver_add_predicate(S, impl_type, impl_trait, cause);
                }
                break;
            case IR_CONSTRAINT_TYPE_EQUALS:
                if (target_is_self(S, p->eq.lhs, self_param)) {
                    IrType *eq_lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                    IrType *eq_rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                    ir_solver_add_norm_target(S, eq_lhs, eq_rhs, cause);
                }
                break;
        }
    }
    return result;
}

static void add_predicates(IrSolver *S, DeclId did, struct Substitution subst, PredicateCache *cache, struct IrObligationCause cause)
{
    if (PredicateCache_insert(cache, did, NULL))
        return;

    IrConstraints const *bounds = ir_get_constraints(S->C, did);
    if (bounds == NULL) return;

    LIST_XFOREACH (bounds, struct IrConstraint const, p) {
        switch (p->kind) {
            case IR_CONSTRAINT_IMPL_TRAIT: {
                IrType *type = pawP_substitute(S->C, p->impl.type, subst);
                IrTrait *trait = pawP_substitute_trait(S->C, p->impl.trait, subst);
                type = normalize_type(S->U, type);
                trait = ir_normalize_trait(S->C, trait);
                ir_solver_add_predicate(S, type, trait, cause);

                add_supertrait_constraints(S, trait, cause);
                break;
            }
            case IR_CONSTRAINT_TYPE_EQUALS: {
                IrType *lhs = pawP_substitute(S->C, p->eq.lhs, subst);
                IrType *rhs = pawP_substitute(S->C, p->eq.rhs, subst);
                lhs = normalize_type(S->U, lhs);
                rhs = normalize_type(S->U, rhs);
                ir_solver_add_norm_target(S, lhs, rhs, cause);
                break;
            }
        }
    }
}

void ir_solver_add_predicates_from(IrSolver *S, DeclId did, IrGenericArgs *args, struct IrObligationCause cause)
{
    PredicateCache *cache = PredicateCache_new(ALLOCATOR_OF(S->C));
    IrGenericArgs *params = ir_get_generic_args(S->C, did);
    struct Substitution const subst = {params, args};
    add_predicates(S, did, subst, cache, cause);
}

void ir_solver_add_predicates_from_type(IrSolver *S, IrType *type, struct IrObligationCause cause)
{
    if (IrIsAdt(type) || IrIsSignature(type))
        ir_solver_add_predicates_from(S, IR_TYPE_DID(type), IR_GENERIC_ARGS(type), cause);
}

void ir_solver_add_predicates_from_trait(IrSolver *S, IrTrait *trait, struct IrObligationCause cause)
{
    ir_solver_add_predicates_from(S, trait->did, trait->args, cause);
}

void ir_solver_add_copy_obligation_for(IrSolver *S, IrType *type)
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
            LIST_XFOREACH (t->elems, IrType *const, p)
                ir_solver_add_copy_obligation_for(S, *p);
            break;
        }
        case kIrArray: {
            struct IrArray const *t = IrGetArray(type);
            ir_solver_add_copy_obligation_for(S, t->type);
            break;
        }
        case kIrClosure:
            if (ir_is_capturing_closure(C, type)) {
                UpvalueList const *upvalues = *UpvalueTable_get(C->upvtab, IR_TYPE_DID(type));
                LIST_XFOREACH (upvalues, struct UpvalueInfo const, u) {
                    IrType *upvalue = ir_get_type(C, u->id);
                    ir_solver_add_copy_obligation_for(S, upvalue);
                }
            }
            break;
        default: {
            DeclId const copy_did = C->core_traits[CORE_TRAIT_COPY];
            IrGenericArgs *copy_args = IrGenericArgs_new(ALLOCATOR_OF(C));
            IrGenericArgs_push(copy_args, IrGenericArg_from_type(type));
            IrTrait *copy = ir_solver_instantiate_trait_with(S, copy_did, copy_args);
            ir_solver_add_impl_trait_obligation(S, type, copy,
                    (struct IrObligationCause){0});
            break;
        }
    }
}

Str const *ir_print_obligation_cause(struct Compiler *C, struct IrObligationCause cause)
{
    Buffer b;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &b);

    switch (cause.kind) {
        case IR_OBLIGATION_CAUSE_WF_CHECKING:
            L_ADD_LITERAL(P, &b, "well-formedness checking");
            break;
        case IR_OBLIGATION_CAUSE_INSTANTIATION:
            L_ADD_LITERAL(P, &b, "instantiation of type `");
            L_ADD_STRING(P, &b, ir_print_type_v2(C, normalize_type(C->U, cause.instantiation.type)));
            lib_add_char(P, &b, '`');
            break;
        case IR_OBLIGATION_CAUSE_ASSOC_ITEM_LOOKUP:
            L_ADD_LITERAL(P, &b, "lookup of associated item `");
            L_ADD_STRING(P, &b, cause.assoc_item_lookup.name);
            L_ADD_LITERAL(P, &b, "` on type `");
            L_ADD_STRING(P, &b, ir_print_type_v2(C, normalize_type(C->U, cause.assoc_item_lookup.self)));
            lib_add_char(P, &b, '`');
            break;
        case IR_OBLIGATION_CAUSE_PREDICATE:
            // TODO: this variant shouldn't really exist, predicates should exist as some other data type instead of `struct IrObligation`
            PAW_UNREACHABLE();
    }

    return lib_buffer_finish(P, &b);
}


Str const *ir_print_obligation_(struct Compiler *C, struct IrObligation obligation)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    switch (obligation.kind) {
        case IR_OBLIGATION_CONST_EQUALS:
            lib_add_fstring(P, &buf, "ConstEq(...)");
            break;
        case IR_OBLIGATION_WELL_FORMED: {
            enum IrDefKind const def_kind = ir_get_kind(C, obligation.wf.did);
            if (def_kind == IR_TRAIT_DEF) {
                IrTrait *trait = ir_normalize_trait(C,
                        ir_new_trait(C, obligation.wf.did, obligation.wf.args));
                lib_add_fstring(P, &buf, "WellFormed(%s)",
                        ir_print_trait(C, trait));
            } else {
                paw_assert(def_kind == IR_ADT_DEF);
                IrType *type = normalize_type(C->U,
                        ir_new_adt(C, obligation.wf.did, obligation.wf.args));
                lib_add_fstring(P, &buf, "WellFormed(%s)",
                        ir_print_type(C, type));
            }
            break;
        }
        case IR_OBLIGATION_IMPL_TRAIT: {
            IrType *type = normalize_type(C->U, obligation.impl.type);
            IrTrait *trait = ir_normalize_trait(C, obligation.impl.trait);
            lib_add_fstring(P, &buf, "%s: %s",
                    ir_print_type_v2(C, type)->text,
                    ir_print_trait_v2(C, trait)->text);
            break;
        }
        case IR_OBLIGATION_TYPE_EQUALS: {
            IrType *lhs = normalize_type(C->U, obligation.eq.lhs);
            IrType *rhs = normalize_type(C->U, obligation.eq.rhs);
            lib_add_fstring(P, &buf, "%s = %s",
                    ir_print_type_v2(C, lhs)->text,
                    ir_print_type_v2(C, rhs)->text);
            break;
        }
    }

    return lib_buffer_finish(P, &buf);
}

static void print_obligation(IrSolver *S, Buffer *buf, struct IrObligation o)
{
    paw_Env *P = ENV(S->C);
    switch (o.kind) {
        case IR_OBLIGATION_WELL_FORMED: {
            enum IrDefKind const def_kind = ir_get_kind(S->C, o.wf.did);
            if (def_kind == IR_TRAIT_DEF) {
                IrTrait *trait = ir_new_trait(S->C, o.wf.did, o.wf.args);
                lib_add_fstring(P, buf, "  WellFormed(%s)\n",
                        ir_print_trait(S->C, trait));
            } else {
                paw_assert(def_kind == IR_ADT_DEF);
                IrType *type = ir_new_adt(S->C, o.wf.did, o.wf.args);
                lib_add_fstring(P, buf, "  WellFormed(%s)\n",
                        ir_print_type(S->C, type));
            }
            break;
        }
        case IR_OBLIGATION_IMPL_TRAIT: {
            IrType *type = normalize_projections(S->C->U, o.impl.type);
            IrTrait *trait = ir_normalize_trait_projections(S->C, o.impl.trait);
            lib_add_fstring(P, buf, "  %s: %s\n",
                    ir_print_type_v2(S->C, type)->text,
                    ir_print_trait_v2(S->C, trait)->text);
            break;
        }
        case IR_OBLIGATION_TYPE_EQUALS: {
            IrType *lhs = normalize_projections(S->C->U, o.eq.lhs);
            IrType *rhs = normalize_projections(S->C->U, o.eq.rhs);
            lib_add_fstring(P, buf, "  %s = %s\n",
                    ir_print_type_v2(S->C, lhs)->text,
                    ir_print_type_v2(S->C, rhs)->text);
            break;
        }
        case IR_OBLIGATION_CONST_EQUALS: {
            lib_add_fstring(P, buf, "  %s = %s\n",
                    ir_print_const(S->C, o.keq.lhs)->text,
                    ir_print_const(S->C, o.keq.rhs)->text);
            break;
        }
    }
}

char const *debug_solver(IrSolver* S)
{
    Buffer buf;
    paw_Env *P = ENV(S->C);
    lib_init_buffer(P, &buf);
    L_ADD_LITERAL(P, &buf, "obligations:\n");
    LIST_XFOREACH (S->obligations, struct IrObligation const, p) {
        print_obligation(S, &buf, *p);
    }
    L_ADD_LITERAL(P, &buf, "impl trait predicates:\n");
    LIST_XFOREACH (S->predicates, struct IrObligation const, p) {
        print_obligation(S, &buf, *p);
    }
    L_ADD_LITERAL(P, &buf, "type eq predicates:\n");
    LIST_XFOREACH (S->norm_targets, struct IrObligation const, p) {
        lib_add_fstring(P, &buf, "  %s = %s\n",
                ir_print_type_v2(S->C, p->eq.lhs)->text,
                ir_print_type_v2(S->C, p->eq.rhs)->text);
    }
    return lib_buffer_finish(P, &buf)->text;
}

void debug_typesystem(struct Compiler *C)
{
    for (int i = 0; i < C->hir->modules->count; ++i) {
        struct HirModule const m = HirModuleList_get(C->hir->modules, i);
        LIST_XFOREACH (m.items, struct HirDecl *const, pdecl) {
            DeclId const did = (*pdecl)->hdr.did;
            paw_Bool print_solver = PAW_TRUE;
            switch (HIR_KINDOF(*pdecl)) {
                case kHirAdtDecl: {
                    IrGenericArgs *args = ir_get_generic_args(C, did);
                    IrType *type = ir_solver_instantiate_type_with(C->S, did, args);
                    printf("ADT %s\n", ir_print_type_v2(C, type)->text);
                    break;
                }
                case kHirTraitDecl: {
                    IrGenericArgs *args = ir_get_generic_args(C, did);
                    IrTrait *trait = ir_new_trait(C, did, args);
                    printf("Trait %s\n", ir_print_trait_v2(C, trait)->text);
                    break;
                }
                case kHirImplDecl: {
                    IrGenericArgs *args = ir_get_generic_args(C, did);
                    struct IrImplInstance const inst = ir_solver_instantiate_impl_with(C->S, did, args);
                    printf("Impl %s for %s\n", ir_print_trait_v2(C, inst.trait)->text,
                            ir_print_type_v2(C, inst.type)->text);
                    break;
                }
                case kHirFnDecl: {
                    IrType *type = ir_get_def_type(C, did);
                    printf("Fn %s\n", ir_print_type_v2(C, type)->text);
                    break;
                }
                default:
                    print_solver = PAW_FALSE;
            }
            if (print_solver) {
                IrSolver *S = ir_push_solver(C);
                IrGenericArgs *args = ir_get_generic_args(C, did);
                ir_solver_add_obligations_from(S, did, args, (struct IrObligationCause){0});
                ir_solver_add_predicates_from(S, did, args, (struct IrObligationCause){0});
                puts(debug_solver(S));
                ir_pop_solver(C);
            }
        }
    }
}
