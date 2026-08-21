// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "solve.h"
#include "ir_type.h"

static int equals_adaptor(struct Compiler *C, IrGenericArg a, IrGenericArg b)
{
    return pawIr_arg_equals(C, a, b) ? 0 : -1;
}

static int aux(struct Compiler *C, IrTrait *a, IrTrait *b, int (*callback)(struct Compiler *, IrGenericArg, IrGenericArg))
{
    if (a->did.value != b->did.value)
        return -1;

    IrGenericArg const *x;
    IrGenericArg const *y;
    paw_assert(a->args->count == b->args->count);
    K_LIST_ZIP (a->args, x, b->args, y) {
        if (callback(C, *x, *y) != 0)
            return -1;
    }
    return 0;
}

int pawIr_unify_traits(struct Compiler *C, IrTrait *a, IrTrait *b)
{
    return aux(C, a, b, pawIr_unify);
}

paw_Bool pawIr_trait_equals(struct Compiler *C, IrTrait *a, IrTrait *b)
{
    return aux(C, a, b, equals_adaptor) == 0;
}

IrTrait *pawIr_normalize_trait(struct Compiler *C, IrTrait *trait)
{
    IrGenericArgs *args = IrGenericArgs_new(C);
    if (trait->args != NULL) {
        IrGenericArgs_reserve(C, args, trait->args->count);
        K_LIST_XFOREACH (trait->args, IrGenericArg const, p)
            IrGenericArgs_push(C, args, pawIr_normalize(C, *p));
    }
    return pawIr_new_trait(C, trait->did, args);
}

IrTrait *pawIr_normalize_trait_projections(struct Compiler *C, IrTrait *trait)
{
    IrGenericArgs *args = IrGenericArgs_new(C);
    if (trait->args != NULL) {
        IrGenericArgs_reserve(C, args, trait->args->count);
        K_LIST_XFOREACH (trait->args, IrGenericArg const, p)
            IrGenericArgs_push(C, args, pawIr_normalize_projections(C, *p));
    }
    return pawIr_new_trait(C, trait->did, args);
}
