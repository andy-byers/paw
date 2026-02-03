// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "map.h"
#include "solve.h"
#include "type_folder.h"
#include "unify.h"

static int equals_adaptor(struct Unifier *U, IrType *a, IrType *b)
{
    return pawU_equals(U, a, b) ? 0 : -1;
}

static int aux(struct Compiler *C, IrTrait *a, IrTrait *b, int (*callback)(struct Unifier *, IrType *, IrType *))
{
    if (a->did.value != b->did.value)
        return -1;

    IrType *const *x, *const *y;
    paw_assert(a->types->count == b->types->count);
    K_LIST_ZIP (a->types, x, b->types, y) {
        if (callback(C->U, *x, *y) != 0)
            return -1;
    }
    return 0;
}

int pawIr_unify_traits(struct Compiler *C, IrTrait *a, IrTrait *b)
{
    return aux(C, a, b, pawU_unify);
}

paw_Bool pawIr_trait_equals(struct Compiler *C, IrTrait *a, IrTrait *b)
{
    return aux(C, a, b, equals_adaptor) == 0;
}

IrTrait *pawIr_normalize_trait(struct Compiler *C, IrTrait *trait)
{
    IrTypeList *types = IrTypeList_new(C);
    if (trait->types != NULL) {
        IrTypeList_reserve(C, types, trait->types->count);
        K_LIST_XFOREACH (trait->types, IrType *const, p)
            IrTypeList_push(C, types, pawU_normalize(C->U, *p));
    }
    return pawIr_new_trait(C, trait->did, types);
}
