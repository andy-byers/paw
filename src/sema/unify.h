// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_SEMA_UNIFY_H
#define PAW_SEMA_UNIFY_H

#include "ir/type.h"

typedef struct Unifier Unifier;

struct UnifierState {
    int value;
};

typedef int (*Unify)(Unifier *, IrType *, IrType *);
typedef int (*UnifyTrait)(struct Compiler *, IrTrait *, IrTrait *);

Unifier *unifier_new(struct Compiler *C);

IrType *normalize_type(Unifier *U, IrType *a);
EXTERN_C IrType *normalize_projections(Unifier *U, IrType *type);
IrConst *normalize_const(Unifier *U, IrConst *k);

// Impose the constraint that entities `a` and `b` are equal
EXTERN_C int unify_types(Unifier *U, IrType *a, IrType *b);
EXTERN_C int unify_consts(Unifier *U, IrConst *a, IrConst *b);

// Create new inference variables
IrType *create_type_var(Unifier *U, enum IrInferKind ikind, struct SourceSpan span);
IrConst *create_const_var(Unifier *U, struct SourceSpan span);

void discard_type_vars(Unifier *U);

// Inference context handling
void enter_unification_ctx(Unifier *U, Str const *modname);
void leave_unification_ctx(Unifier *U);
void check_unification_ctx(Unifier *U);

// Return a token representing the current state of the unifier
struct UnifierState unifier_state(Unifier *U);

// Discard all inference information obtained after given `state` token
// was produced
void undo_unifications(Unifier *U, struct UnifierState state);

// Infer the type of each unresolved integer or floating point inference
// variable as the default integer or floating point type, respectively
void assign_default_literal_types(Unifier *U);


static inline void unify_types_unchecked(Unifier *U, IrType *a, IrType *b)
{
    int const unused = unify_types(U, a, b);
    paw_assert(unused == 0); PAW_UNUSED(unused);
}

#endif // PAW_SEMA_UNIFY_H
