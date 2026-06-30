// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_SOLVE_H
#define PAW_SOLVE_H

#include "compile.h"

typedef struct IrSolver IrSolver;
struct IrGenericArgs;

enum IrObligationKind {
    IR_OBLIGATION_IMPL_TRAIT,
    IR_OBLIGATION_TYPE_EQUALS,
    IR_OBLIGATION_WELL_FORMED,
};

struct IrObligationCause {
    struct SourceSpan span;
};

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
            DeclId did;
            struct IrGenericArgs *args;
        } wf;
    };
};

DEFINE_LIST(struct Compiler, IrObligations, struct IrObligation)

IrSolver *pawIr_push_solver(struct Compiler *C);
void pawIr_pop_solver(struct Compiler *C);

void pawIr_solver_add_well_formed_obligation(IrSolver *S, DeclId did, struct IrGenericArgs *args, struct IrObligationCause cause);
void pawIr_solver_add_impl_trait_obligation(IrSolver *S, struct IrType *type, struct IrTrait *trait, struct IrObligationCause cause);
void pawIr_solver_add_type_equals_obligation(IrSolver *S, struct IrType *lhs, struct IrType *rhs, struct IrObligationCause cause);

struct IrType *pawIr_solver_get_norm_target(IrSolver *S, struct IrType *type);
void pawIr_solver_add_norm_target(IrSolver *S, struct IrType *type, struct IrType *target, struct IrObligationCause cause);

void pawIr_solver_add_predicate(IrSolver *S, struct IrType *type, struct IrTrait *trait, struct IrObligationCause cause);

void pawIr_solver_add_obligations_from(IrSolver *S, DeclId parent_did, struct IrGenericArgs *args);
void pawIr_solver_add_obligations_from_type(IrSolver *S, struct IrType *type);
void pawIr_solver_add_obligations_from_trait(IrSolver *S, struct IrTrait *trait);

void pawIr_solver_add_predicates_from(IrSolver *S, DeclId did, struct IrGenericArgs *args);
void pawIr_solver_add_predicates_from_type(IrSolver *S, struct IrType *type);
void pawIr_solver_add_predicates_from_trait(IrSolver *S, struct IrTrait *trait);


struct IrType *pawIr_solver_instantiate_type(IrSolver *S, DeclId did);
struct IrTrait *pawIr_solver_instantiate_trait(IrSolver *S, DeclId did);
struct IrType *pawIr_solver_instantiate_type_with(IrSolver *S, DeclId did, struct IrGenericArgs *args);
struct IrTrait *pawIr_solver_instantiate_trait_with(IrSolver *S, DeclId did, struct IrGenericArgs *args);

struct IrImplInstance {
    struct IrType *type;
    struct IrTrait *trait;
    struct IrGenericArgs *args;
};

struct IrImplInstance pawIr_solver_instantiate_impl(IrSolver *S, DeclId did);
struct IrImplInstance pawIr_solver_instantiate_impl_with(IrSolver *S, DeclId did, struct IrGenericArgs *args);

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

struct IrSolverResult pawIr_solver_solve(IrSolver *S);

int pawIr_solver_num_obligations(IrSolver const *S);
struct IrObligation pawIr_solver_first_obligation(IrSolver const *S);

paw_Bool pawIr_type_implements_trait(IrSolver *S, struct IrType *type, struct IrTrait *trait);

EXTERN_C Str const *pawIr_print_obligation_(struct Compiler *C, struct IrObligation obligation);

char const *debug_solver(IrSolver* S);
void debug_impl(struct Compiler *C, DeclId did, struct IrGenericArgs *args);

#endif // PAW_SOLVE_H
