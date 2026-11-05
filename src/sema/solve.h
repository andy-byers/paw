// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_SOLVE_H
#define PAW_SOLVE_H

#include "compiler/compiler.h"

typedef struct IrSolver IrSolver;
struct IrGenericArgs;

enum IrObligationKind {
    IR_OBLIGATION_IMPL_TRAIT,
    IR_OBLIGATION_TYPE_EQUALS,
    IR_OBLIGATION_CONST_EQUALS,
    IR_OBLIGATION_WELL_FORMED,
};

enum IrObligationCauseKind {
    IR_OBLIGATION_CAUSE_WF_CHECKING,
    IR_OBLIGATION_CAUSE_PREDICATE,
    IR_OBLIGATION_CAUSE_INSTANTIATION,
    IR_OBLIGATION_CAUSE_ASSOC_ITEM_LOOKUP,
};

struct IrObligationCause {
    enum IrObligationCauseKind kind;
    struct SourceSpan span;
    union {
        DeclId did;
        struct {
            struct IrType *self;
            Str const *name;
        } assoc_item_lookup;
        struct {
            struct IrType *type;
        } instantiation;
    };
};

Str const *ir_print_obligation_cause(struct Compiler *C, struct IrObligationCause cause);


struct IrObligation {
    enum IrObligationKind kind;
    struct IrObligationCause cause;
    union {
        struct {
            struct IrType *type;
            struct IrTrait *trait;
        } impl;

        struct {
            struct IrType *lhs;
            struct IrType *rhs;
        } eq;

        struct {
            struct IrConst *lhs;
            struct IrConst *rhs;
        } keq;

        struct {
            DeclId did;
            struct IrGenericArgs *args;
        } wf;
    };
};

DEFINE_LIST(IrObligations, struct IrObligation)

IrSolver *ir_push_solver(struct Compiler *C);
void ir_pop_solver(struct Compiler *C);

void ir_solver_add_well_formed_obligation(IrSolver *S, DeclId did, struct IrGenericArgs *args, struct IrObligationCause cause);
void ir_solver_add_impl_trait_obligation(IrSolver *S, struct IrType *type, struct IrTrait *trait, struct IrObligationCause cause);
void ir_solver_add_type_equals_obligation(IrSolver *S, struct IrType *lhs, struct IrType *rhs, struct IrObligationCause cause);
void ir_solver_add_const_equals_obligation(IrSolver *S, struct IrConst *lhs, struct IrConst *rhs, struct IrObligationCause cause);

IrObligations *ir_solver_remove_const_obligations(IrSolver *S);

struct IrType *ir_solver_get_norm_target(IrSolver *S, struct IrType *type);
void ir_solver_add_norm_target(IrSolver *S, struct IrType *type, struct IrType *target, struct IrObligationCause cause);

void ir_solver_add_predicate(IrSolver *S, struct IrType *type, struct IrTrait *trait, struct IrObligationCause cause);

void ir_solver_add_obligations_from(IrSolver *S, DeclId parent_did, struct IrGenericArgs *args, struct IrObligationCause cause);
void ir_solver_add_obligations_from_type(IrSolver *S, struct IrType *type, struct IrObligationCause cause);
void ir_solver_add_obligations_from_trait(IrSolver *S, struct IrTrait *trait, struct IrObligationCause cause);

void ir_solver_add_copy_obligation_for(IrSolver *S, struct IrType *type);

void ir_solver_add_predicates_from(IrSolver *S, DeclId did, struct IrGenericArgs *args, struct IrObligationCause cause);
void ir_solver_add_predicates_from_type(IrSolver *S, struct IrType *type, struct IrObligationCause cause);
void ir_solver_add_predicates_from_trait(IrSolver *S, struct IrTrait *trait, struct IrObligationCause cause);


struct IrType *ir_solver_instantiate_type(IrSolver *S, DeclId did, struct SourceSpan span);
struct IrTrait *ir_solver_instantiate_trait(IrSolver *S, DeclId did, struct SourceSpan span);
struct IrType *ir_solver_instantiate_type_with(IrSolver *S, DeclId did, struct IrGenericArgs *args);
struct IrTrait *ir_solver_instantiate_trait_with(IrSolver *S, DeclId did, struct IrGenericArgs *args);

struct IrImplInstance {
    struct IrType *type;
    struct IrTrait *trait;
    struct IrGenericArgs *args;
};

struct IrImplInstance ir_solver_instantiate_impl(IrSolver *S, DeclId did, struct SourceSpan span);
struct IrImplInstance ir_solver_instantiate_impl_with(IrSolver *S, DeclId did, struct IrGenericArgs *args);

enum IrSolverStatus {
    IR_SOLVER_SOLVED,
    IR_SOLVER_AMBIGUOUS,
    IR_SOLVER_ERROR,
};

struct IrSolverResult {
    enum IrSolverStatus status;
    union {
        struct {
            int num_unsolved;
        } ambiguous;

        struct {
            struct IrObligation obligation;
        } error;
    };
};

struct IrSolverResult ir_solver_solve(IrSolver *S);
void ir_solver_solve_all_or_error(IrSolver *S);

static paw_Bool ir_solver_solve_all(IrSolver *S)
{
    return ir_solver_solve(S).status == IR_SOLVER_SOLVED;
}

static paw_Bool ir_solver_is_copyable(struct Compiler *C, struct IrType *type)
{
    IrSolver *S = ir_push_solver(C);
    ir_solver_add_copy_obligation_for(S, type);
    struct IrSolverResult const r = ir_solver_solve(S);
    ir_pop_solver(C);
    return r.status == IR_SOLVER_SOLVED;
}

int ir_solver_num_obligations(IrSolver const *S);
struct IrObligation ir_solver_first_obligation(IrSolver const *S);

paw_Bool ir_type_implements_trait(IrSolver *S, struct IrType *type, struct IrTrait *trait);

EXTERN_C Str const *ir_print_obligation_(struct Compiler *C, struct IrObligation obligation);

char const *debug_solver(IrSolver* S);
void debug_impl(struct Compiler *C, DeclId did, struct IrGenericArgs *args);

#endif // PAW_SOLVE_H
