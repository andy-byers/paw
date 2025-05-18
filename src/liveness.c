// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// liveness.c: Live variable analysis
//
// References:
// (1) Wimmer, C., & Franz, M. (2010). Linear Scan Register Allocation on SSA Form.

#include "compile.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include <stdlib.h>

struct Liveness {
    struct Pool *pool;
    struct MirIntervalList *intervals;
    struct MirLocationList *locations;
    struct RegisterSetList *live;
    struct IntervalMap *mapping;
    struct Compiler *C;
    struct Mir *mir;
    paw_Env *P;
    int index;
};

DEFINE_LIST(struct Liveness, RegisterSetList, struct MirRegisterList *)
DEFINE_MAP(struct Liveness, IntervalMap, pawP_alloc, MIR_ID_HASH, MIR_ID_EQUALS, MirRegister, struct MirLiveInterval *)

static void map_reg_to_interval(struct Liveness *L, MirRegister r, struct MirLiveInterval *it)
{
    IntervalMap_insert(L, L->mapping, r, it);
}

static struct MirLiveInterval *interval_for_reg(struct Liveness *L, MirRegister r)
{
    struct MirLiveInterval *const *pit = IntervalMap_get(L, L->mapping, r);
    return pit != NULL ? *pit : NULL;
}

static int bb_first_loc(struct Liveness *L, struct MirBlockData *block)
{
    return pawMir_get_location(L->locations, mir_bb_first(block));
}

static int bb_last_loc(struct Liveness *L, struct MirBlockData *block)
{
    return pawMir_get_location(L->locations, mir_bb_last(block)) + 2;
}

static int instr_loc(struct Liveness *L, struct MirInstruction *instr)
{
    return pawMir_get_location(L->locations, instr->hdr.mid);
}

static void set_from(struct Liveness *L, MirRegister opd, int from, struct MirBlockData *block)
{
    struct MirLiveInterval *it = interval_for_reg(L, opd);
    pawP_bitset_clear_range(it->ranges, bb_first_loc(L, block), from);
    pawP_bitset_set(it->ranges, from);
    it->last = PAW_MAX(it->last, from);
    it->first = from;
}

static void add_range(struct Liveness *L, MirRegister opd, int from, int to)
{
    struct MirLiveInterval *it = interval_for_reg(L, opd);
    pawP_bitset_set_range(it->ranges, from, to);
    it->first = PAW_MIN(it->first, from);
    it->last = PAW_MAX(it->last, to);
}

static void add_live_reg(struct Liveness *L, struct MirRegisterList *set, MirRegister r)
{
    MirRegister *pr;
    K_LIST_FOREACH (set, pr) {
        if (MIR_REG_EQUALS(r, *pr))
            return;
    }
    MirRegisterList_push(L->mir, set, r);
}

static void remove_live_reg(struct Liveness *L, struct MirRegisterList *set, MirRegister r)
{
    for (int i = 0; i < set->count; ++i) {
        MirRegister const r2 = MirRegisterList_get(set, i);
        if (MIR_REG_EQUALS(r, r2)) {
            MirRegisterList_set(set, i, K_LIST_LAST(set));
            MirRegisterList_pop(set);
            return;
        }
    }
}

// Indicate that register "r" is defined by instruction "x"
#define OUTPUT(L, loc, r)           \
    do {                            \
        set_from(L, r, loc, block); \
        remove_live_reg(L, set, r); \
    } while (0)

// Indicate that register "r" is used by instruction "x"
#define INPUT(L, loc, r)                              \
    do {                                              \
        add_range(L, r, bb_first_loc(L, block), loc); \
        add_live_reg(L, set, r);                      \
    } while (0)

static void step_instruction(struct Liveness *L, struct MirRegisterList *set, struct MirBlockData *block, struct MirInstruction *instr)
{
    struct MirPlace *const *ppr;
    MirPlacePtrList const *stores = pawMir_get_stores(L->mir, instr);
    K_LIST_FOREACH (stores, ppr)
        OUTPUT(L, instr_loc(L, instr), (*ppr)->r);

    MirPlacePtrList const *ploads = pawMir_get_loads(L->mir, instr);
    K_LIST_FOREACH (ploads, ppr)
        INPUT(L, instr_loc(L, instr), (*ppr)->r);
}

#undef INPUT
#undef OUTPUT

#if defined(PAW_DEBUG_EXTRA)

#include <stdio.h>

static void dump_live_intervals(struct Liveness *L, struct MirIntervalList *intervals)
{
    struct MirLiveInterval **iter;
    printf("live_intervals = {\n");
    K_LIST_FOREACH (intervals, iter) {
        struct MirLiveInterval *live = *iter;
        printf("  _%d: ", live->r.value);
        for (int i = 0; i < live->ranges->count; ++i) {
            if (i > 0)
                printf(" ");
            putchar(pawP_bitset_get(live->ranges, i) ? '*' : ' ');
        }
        printf("]\n");
    }
    printf("}\n");
}

#endif // PAW_DEBUG_EXTRA

// Return a list of all blocks containing back edges to a loop header
static struct MirBlockList *determine_loop_ends(struct Liveness *L, MirBlock header)
{
    struct MirBlockData *data = mir_bb_data(L->mir, header);
    struct MirBlockList *loop_ends = MirBlockList_new_from(L->mir, L->pool);

    MirBlock const *pb;
    K_LIST_FOREACH (data->predecessors, pb) {
        if (pb->value >= header.value) {
            MirBlockList_push(L->mir, loop_ends, *pb);
        }
    }
    return loop_ends;
}

static void compute_liveness(struct Liveness *L, struct Mir *mir, struct MirBlockList *order)
{
    for (int ib = order->count - 1; ib >= 0; --ib) {
        MirBlock const b = MirBlockList_get(order, ib);
        struct MirBlockData *block = mir_bb_data(mir, b);

        MirBlock *ps;
        MirRegister *pr;
        struct MirInstruction **pinstr;

        // "live" is the union of the sets of live registers from the successors of "b"
        struct MirRegisterList *live = MirRegisterList_new_from(L->mir, L->pool);
        K_LIST_FOREACH (block->successors, ps) {
            struct MirRegisterList *set = RegisterSetList_get(L->live, ps->value);
            K_LIST_FOREACH (set, pr) {
                add_live_reg(L, live, *pr);
            }
        }

        // account for inputs to phi functions in successor blocks, which are live from
        // where they are defined until the end of this block
        K_LIST_FOREACH (block->successors, ps) {
            struct MirBlockData *sblock = mir_bb_data(mir, *ps);
            K_LIST_FOREACH (sblock->joins, pinstr) {
                struct MirPhi *phi = MirGetPhi(*pinstr);
                int const p = mir_which_pred(L->mir, *ps, b);
                add_live_reg(L, live, MirPlaceList_get(phi->inputs, p).r);
            }
        }

        // initialize ranges to span the whole block (may be refined in the next loop,
        // if a definition for the given variable is encountered in this block)
        int const from = bb_first_loc(L, block);
        int const to = bb_last_loc(L, block);
        K_LIST_FOREACH (live, pr) {
            add_range(L, *pr, from, to);
        }

        for (int io = block->instructions->count - 1; io >= 0; --io) {
            struct MirInstruction *op = MirInstructionList_get(block->instructions, io);
            step_instruction(L, live, block, op);
        }

        // account for phi function outputs, which are live from the start of the current
        // block to the location they are last used
        K_LIST_FOREACH (block->joins, pinstr) {
            struct MirPhi *phi = MirGetPhi(*pinstr);
            remove_live_reg(L, live, phi->output.r);
        }

        // Special handling for loop headers. Variables live at the loop header must be
        // live until the end of the loop, since they are needed in subsequent iterations.
        MirBlock const *pb;
        struct MirBlockList *loop_ends = determine_loop_ends(L, b);
        K_LIST_FOREACH (loop_ends, pb) {
            struct MirBlockData *end = mir_bb_data(mir, *pb);
            K_LIST_FOREACH (live, pr)
                add_range(L, *pr, from, bb_last_loc(L, end));
        }
        RegisterSetList_set(L->live, b.value, live);
    }
}

static void add_live_interval(struct Liveness *L, MirRegister r, MirId mid, int npositions, struct MirInstruction *instr)
{
    int const location = pawMir_get_location(L->locations, mid);
    struct MirLiveInterval *it = pawMir_new_interval(L->C, r, npositions);
    map_reg_to_interval(L, r, it);
    it->instr = instr;
    it->first = location;
    it->last = location + 2;
    pawP_bitset_set_range(it->ranges, it->first, it->last);
    MirIntervalList_push(L->mir, L->intervals, it);
}

static void init_live_intervals(struct Liveness *L, struct MirBlockList *order, int npos)
{
    MirIntervalList_reserve(L->mir, L->intervals, L->mir->registers->count);

    MirBlock *b;
    K_LIST_FOREACH (order, b) {
        struct MirInstruction **pinstr;
        struct MirBlockData *block = mir_bb_data(L->mir, *b);

        // add an interval for each phi node (all phi nodes define a variable)
        K_LIST_FOREACH (block->joins, pinstr) {
            struct MirPhi *phi = MirGetPhi(*pinstr);
            add_live_interval(L, phi->output.r, block->mid, npos, *pinstr);
        }

        // add an interval for each variable
        K_LIST_FOREACH (block->instructions, pinstr) {
            struct MirPlace *const *ppstore;
            MirPlacePtrList const *stores = pawMir_get_stores(L->mir, *pinstr);
            K_LIST_FOREACH (stores, ppstore)
                add_live_interval(L, (*ppstore)->r, block->mid, npos, *pinstr);
        }
    }
}

static paw_Bool block_set_contains(struct MirBlockList *set, MirBlock b)
{
    MirBlock const *pb;
    K_LIST_FOREACH (set, pb) {
        if (MIR_BB_EQUALS(b, *pb))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

inline static int find_place(struct MirPlacePtrList const *place, MirRegister r)
{
    int index;
    struct MirPlace *const *ppp;
    K_LIST_ENUMERATE (place, index, ppp) {
        if (MIR_REG_EQUALS(r, (*ppp)->r))
            return index;
    }
    return -1;
}

struct MirBlockList *pawMir_compute_live_in(struct Mir *mir, struct MirBlockList *uses, struct MirBlockList *defs, MirRegister r)
{
    // algorithm is from LLVM "mem2reg" pass
    struct Compiler *C = mir->C;

    MirBlock const *pb;
    struct MirBlockList *result = MirBlockList_new(mir);
    struct MirBlockList *W = MirBlockList_new(mir);
    MirBlockList_reserve(mir, W, uses->count);
    K_LIST_FOREACH (uses, pb) {
        MirBlockList_push(mir, W, *pb);
    }

    int index;
    K_LIST_ENUMERATE (W, index, pb) {
        if (!block_set_contains(defs, *pb))
            continue;
        struct MirBlockData *bb = mir_bb_data(mir, *pb);

        struct MirInstruction **pinstr;
        K_LIST_FOREACH (bb->instructions, pinstr) {
            // If there is a store before a load for "r" in basic block "b", then "r"
            // is not live-in to "b". The load/store checks are performed in reverse
            // compared to the LLVM algorithm, because some instructions both load
            // and store a particular variable (this routine is run before SSA is
            // constructed). Instructions read their operands before writing their
            // output, so loads must be checked before stores. e.g. "x = x + 1"
            // loads "x" before writing to it.
            MirPlacePtrList const *loads = pawMir_get_loads(mir, *pinstr);
            if (find_place(loads, r) >= 0)
                goto found_access;

            MirPlacePtrList const *stores = pawMir_get_stores(mir, *pinstr);
            if (find_place(stores, r) >= 0) {
                MirBlockList_swap_remove(W, index);
                goto found_access;
            }
        }
    found_access:;
    }

    while (W->count > 0) {
        MirBlock const b = K_LIST_LAST(W);
        MirBlockList_pop(W);

        if (block_set_contains(result, b))
            continue;
        MirBlockList_push(mir, result, b);

        MirBlock const *pp;
        struct MirBlockData *bb = mir_bb_data(mir, b);
        K_LIST_FOREACH (bb->predecessors, pp) {
            if (!block_set_contains(defs, *pp))
                MirBlockList_push(mir, W, *pp);
        }
    }
    return result;
}

static void extend_captured_intervals(struct Liveness *L, struct Mir *mir, int npositions)
{
    struct MirLiveInterval **pit;
    K_LIST_FOREACH (L->intervals, pit) {
        struct MirLiveInterval *it = *pit;
        struct MirRegisterData *data = mir_reg_data(mir, it->r);
        if (data->is_captured) {
            pawP_bitset_set_range(it->ranges, 0, npositions);
            it->last = npositions;
            it->first = 0;
        }
    }
}

struct MirIntervalList *pawMir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirLocationList *locations)
{
    struct Liveness L = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .intervals = MirIntervalList_new(mir),
        .locations = locations,
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    L.live = RegisterSetList_new(&L);
    L.mapping = IntervalMap_new(&L);

    struct MirBlockData *last = mir_bb_data(mir, K_LIST_LAST(order));
    int const nparameters = IR_FPTR(mir->type)->params->count;
    int const npositions = bb_last_loc(&L, last) + 2;
    int const ncaptured = mir->captured->count;
    int const nregisters = mir->registers->count;
    int const nblocks = mir->blocks->count;

    init_live_intervals(&L, order, npositions);

    // initialize liveness sets
    RegisterSetList_reserve(&L, L.live, nblocks);
    while (L.live->count < nblocks)
        RegisterSetList_push(&L, L.live, MirRegisterList_new_from(mir, L.pool));

    // arguments and captured variables get dedicated registers
    {
        for (int i = 0; i < 1 + nparameters; ++i)
            add_range(&L, MIR_REG(i), 0, npositions);

        struct MirCaptureInfo *pci;
        K_LIST_FOREACH (mir->captured, pci) {
            add_range(&L, pci->r, 0, npositions);
        }
    }

    // determine live intervals
    compute_liveness(&L, mir, order);
    extend_captured_intervals(&L, mir, npositions);

    pawP_pool_free(C, L.pool);
    return L.intervals;
}
