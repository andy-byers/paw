// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compiler/compiler.h"
#include "mir/mir.h"

static paw_Bool block_set_contains(struct MirBlockList *set, MirBlock b)
{
    MirBlock const *pb;
    LIST_FOREACH (set, pb) {
        if (MIR_ID_EQUALS(b, *pb))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

inline static int find_local(struct MirPlacePtrList const *places, MirRegister r)
{
    int index;
    struct MirPlace *const *ppp;
    LIST_ENUMERATE (places, index, ppp) {
        if ((*ppp)->kind == MIR_PLACE_REGISTER
                && MIR_ID_EQUALS(r, (*ppp)->r))
            return index;
    }
    return -1;
}

MirBlockList *mir_compute_live_in(struct Mir *mir, MirBlockList *uses, MirBlockList *defs, MirRegister r)
{
    // algorithm is from LLVM "mem2reg" pass
    MirBlock const *pb;
    MirBlockList *result = MirBlockList_new(ALLOCATOR_OF(mir));
    MirBlockList *W = MirBlockList_new(ALLOCATOR_OF(mir));
    MirBlockList_reserve(W, uses->count);
    LIST_FOREACH (uses, pb) {
        MirBlockList_push(W, *pb);
    }

    int index;
    LIST_ENUMERATE (W, index, pb) {
        if (!block_set_contains(defs, *pb)) continue;
        struct MirBlockData const *bb = mir_bb_data(mir, *pb);

        struct MirInstruction **pinstr;
        LIST_FOREACH (bb->instructions, pinstr) {
            // If there is a store before a load for "r" in basic block "b", then "r"
            // is not live-in to "b". The load/store checks are performed in reverse
            // compared to the LLVM algorithm, because some instructions both load
            // and store a particular variable (this routine is run before SSA is
            // constructed). Instructions read their operands before writing their
            // output, so loads must be checked before stores. e.g. "x = x + 1"
            // loads "x" before writing to it.
            MirPlacePtrList const *loads = mir_get_loads(mir, *pinstr);
            if (find_local(loads, r) >= 0) break;

            MirPlacePtrList const *stores = mir_get_stores(mir, *pinstr);
            if (find_local(stores, r) >= 0) {
                MirBlockList_swap_remove(W, index);
                break;
            }
        }
    }

    while (W->count > 0) {
        MirBlock const b = LIST_LAST(W);
        MirBlockList_pop(W);

        if (block_set_contains(result, b))
            continue;
        MirBlockList_push(result, b);

        MirBlock const *pp;
        struct MirBlockData *bb = mir_bb_data(mir, b);
        LIST_FOREACH (bb->predecessors, pp) {
            if (!block_set_contains(defs, *pp))
                MirBlockList_push(W, *pp);
        }
    }
    return result;
}

