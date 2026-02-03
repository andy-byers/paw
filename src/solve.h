// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_SOLVE_H
#define PAW_SOLVE_H

#include "compile.h"

typedef struct IrSolver IrSolver;

// ".type" implements ".trait", written ".type: .trait"
struct IrObligation {
    struct IrType *type;
    struct IrTrait *trait;
};

DEFINE_LIST(struct Compiler, IrObligations, struct IrObligation)

IrSolver *pawIr_push_solver(struct Compiler *C);
void pawIr_pop_solver(struct Compiler *C);

void pawIr_solver_add_obligation(IrSolver *S, struct IrType *type, struct IrTrait *trait);

void pawIr_solver_add_precondition(IrSolver *S, struct IrType *type, struct IrTrait *trait);
void pawIr_solver_add_preconditions_from(IrSolver *S, DeclId did);

// Instantiate a type definition
// "did" must refer to an ADT or a function definition. Returns the type of
// definition with any type parameters replaced with inference variables.
// Stores trait obligations to be proven later, once concrete types are
// known.
struct IrType *pawIr_solver_instantiate_type(IrSolver *S, DeclId did);

struct IrTrait *pawIr_solver_instantiate_trait(IrSolver *S, DeclId did);

struct IrType *pawIr_solver_instantiate_type_with(IrSolver *S, DeclId did, struct IrTypeList *args);

struct IrTrait *pawIr_solver_instantiate_trait_with(IrSolver *S, DeclId did, struct IrTypeList *args);

struct IrImplInstance {
    struct IrType *type;
    struct IrTrait *trait;
};

struct IrImplInstance pawIr_solver_instantiate_impl(IrSolver *S, DeclId did);

struct IrImplInstance pawIr_solver_instantiate_impl_with(IrSolver *S, DeclId did, struct IrTypeList *args);

// TODO: result structure to indicate errors
int pawIr_solver_solve(IrSolver *S);

void pawIr_solver_rollback(IrSolver *S);

void pawIr_solver_commit(IrSolver *S);

#endif // PAW_SOLVE_H
