// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "solve.h"
#include "ir/type.h"

static int equals_adaptor(struct Compiler *C, IrGenericArg a, IrGenericArg b)
{
    PAW_UNUSED(C);
    return ir_arg_equals(a, b) ? 0 : -1;
}

static int aux(struct Compiler *C, IrTrait *a, IrTrait *b, int (*callback)(struct Compiler *C, IrGenericArg, IrGenericArg))
{
    if (a->did.value != b->did.value)
        return -1;

    IrGenericArg const *x;
    IrGenericArg const *y;
    paw_assert(a->args->count == b->args->count);
    LIST_ZIP (a->args, x, b->args, y) {
        if (callback(C, *x, *y) != 0)
            return -1;
    }
    return 0;
}

int ir_unify_traits(struct Compiler *C, IrTrait *a, IrTrait *b)
{
    return aux(C, a, b, ir_unify);
}

paw_Bool ir_trait_equals(IrTrait *a, IrTrait *b)
{
    return aux(NULL, a, b, equals_adaptor) == 0;
}

IrTrait *ir_normalize_trait(struct Compiler *C, IrTrait *trait)
{
    IrGenericArgs *args = IrGenericArgs_new(ALLOCATOR_OF(C));
    if (trait->args != NULL) {
        IrGenericArgs_reserve(args, trait->args->count);
        LIST_XFOREACH (trait->args, IrGenericArg const, p)
            IrGenericArgs_push(args, ir_normalize(C, *p));
    }
    return ir_new_trait(C, trait->did, args);
}

IrTrait *ir_normalize_trait_projections(struct Compiler *C, IrTrait *trait)
{
    IrGenericArgs *args = IrGenericArgs_new(ALLOCATOR_OF(C));
    if (trait->args != NULL) {
        IrGenericArgs_reserve(args, trait->args->count);
        LIST_XFOREACH (trait->args, IrGenericArg const, p)
            IrGenericArgs_push(args, ir_normalize_projections(C, *p));
    }
    return ir_new_trait(C, trait->did, args);
}
