// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_TRAIT_H
#define PAW_TRAIT_H

#include "common/util.h"

struct Compiler;
struct IrTrait;

int ir_unify_traits(struct Compiler *C, struct IrTrait *a, struct IrTrait *b);
struct IrTrait *ir_normalize_trait(struct Compiler *C, struct IrTrait *trait);
struct IrTrait *ir_normalize_trait_projections(struct Compiler *C, struct IrTrait *trait);

static void ir_unify_traits_unchecked(struct Compiler *C, struct IrTrait *a, struct IrTrait *b)
{
    int const unused = ir_unify_traits(C, a, b);
    paw_assert(unused == 0); PAW_UNUSED(unused);
}

#endif // PAW_TRAIT_H
