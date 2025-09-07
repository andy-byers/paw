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
    struct MirIntervalMap *mapping;
    struct Compiler *C;
    struct Mir *mir;
    paw_Env *P;
    int index;
};

DEFINE_LIST(struct Liveness, RegisterSetList, struct MirRegisterList *)

static void map_reg_to_interval(struct Liveness *L, MirRegister r, struct MirLiveInterval *it)
{
    MirIntervalMap_insert(L->mir, L->mapping, r, it);
}

static struct MirLiveInterval *interval_for_reg(struct Liveness *L, MirRegister r)
{
    struct MirLiveInterval *const *pit = MirIntervalMap_get(L->mir, L->mapping, r);
    return pit != NULL ? *pit : NULL;
}

static int bb_first_loc(struct Liveness *L, struct MirBlockData *block)
{
    return pawMir_get_location(L->locations, mir_bb_first(block));
}

static int bb_last_loc(struct Liveness *L, struct MirBlockData *block)
{
    return pawMir_get_location(L->locations, mir_bb_last(block));
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
    pawP_bitset_set_range(it->ranges, from, to + 1);
    it->first = PAW_MIN(it->first, from);
    it->last = PAW_MAX(it->last, to);
}

static void add_live_reg(struct Liveness *L, struct MirRegisterList *set, MirRegister r)
{
    MirRegister *pr;
    K_LIST_FOREACH (set, pr) {
        if (MIR_ID_EQUALS(r, *pr))
            return;
    }
    MirRegisterList_push(L->mir, set, r);
}

static void remove_live_reg(struct Liveness *L, struct MirRegisterList *set, MirRegister r)
{
    for (int i = 0; i < set->count; ++i) {
        MirRegister const r2 = MirRegisterList_get(set, i);
        if (MIR_ID_EQUALS(r, r2)) {
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
#define INPUT(L, loc, r)                                  \
    do {                                                  \
        add_range(L, r, bb_first_loc(L, block), loc - 1); \
        add_live_reg(L, set, r);                          \
    } while (0)

static void step_instruction(struct Liveness *L, struct MirRegisterList *set, struct MirBlockData *block, struct MirInstruction *instr)
{
    struct MirPlace *const *ppr;
    MirPlacePtrList const *stores = pawMir_get_stores(L->mir, instr);
    K_LIST_FOREACH (stores, ppr)
        OUTPUT(L, instr_loc(L, instr), (*ppr)->r);

    MirPlacePtrList const *ploads = pawMir_get_loads(L->mir, instr);
    K_LIST_FOREACH (ploads, ppr) {
        if ((*ppr)->kind == MIR_PLACE_LOCAL)
            INPUT(L, instr_loc(L, instr), (*ppr)->r);
    }
}

#undef INPUT
#undef OUTPUT

#if defined(PAW_DEBUG_EXTRA)

#include <stdio.h>

static void dump_live_intervals(struct Liveness *L, struct MirIntervalMap *intervals, int max_position)
{
    for (int r = 0; r < L->mir->registers->count; ++r) {
        struct MirLiveInterval *const *pit = MirIntervalMap_get(L->mir, intervals, MIR_REG(r));
        char padding[] = "  ";
        padding[(r < 10) + (r < 100)] = '\0';
        printf("  %s_%d: ", padding, r);
        if (pit != NULL) {
            struct MirLiveInterval const *it = *pit;
            for (int i = 0; i <= max_position; ++i) {
                if (i > 0) printf(" ");
                if (pawP_bitset_get(it->ranges, i)) {
                    putchar('*');
                } else {
                    putchar('.');
                }
            }
        } else {
            for (int i = 0; i <= max_position; ++i) {
                printf(". ");
            }
        }
        printf("\n");
    }
}

#endif // PAW_DEBUG_EXTRA

static paw_Bool contains_block(MirBlockList *blocks, MirBlock b)
{
    MirBlock const *pb;
    K_LIST_FOREACH (blocks, pb) {
        if (MIR_ID_EQUALS(*pb, b))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void add_loop_ends(struct Liveness *L, MirBlock header, MirBlockList *loop_ends)
{
    struct MirBlockData *data = mir_bb_data(L->mir, header);

    MirBlock const *pb;
    K_LIST_FOREACH (data->predecessors, pb) {
        if (pb->value >= header.value && !contains_block(loop_ends, *pb)) {
            MirBlockList_push(L->mir, loop_ends, *pb);
            add_loop_ends(L, *pb, loop_ends);
        }
    }
}

// Return a list of all blocks containing back edges to a loop header
static MirBlockList *determine_loop_ends(struct Liveness *L, MirBlock header)
{
    MirBlockList *loop_ends = MirBlockList_new_from(L->mir, L->pool);
    add_loop_ends(L, header, loop_ends);
    return loop_ends;
}

static void compute_liveness(struct Liveness *L, struct Mir *mir, struct MirBlockList *order)
{
    for (int ib = order->count - 1; ib >= 0; --ib) {
        MirBlock const b = MirBlockList_get(order, ib);
        struct MirBlockData *block = mir_bb_data(mir, b);

        MirBlock const *ps;
        MirRegister const *pr;
        struct MirInstruction *const *pinstr;

        // "live" is the union of the sets of live registers from the successors of "b"
        struct MirRegisterList *live = MirRegisterList_new_from(L->mir, L->pool);
        K_LIST_FOREACH (block->successors, ps) {
            struct MirRegisterList const *set = RegisterSetList_get(L->live, ps->value);
            K_LIST_FOREACH (set, pr) {
                add_live_reg(L, live, *pr);
            }
        }

        // account for inputs to phi functions in successor blocks, which are live from
        // where they are defined until the end of this block
        K_LIST_FOREACH (block->successors, ps) {
            struct MirBlockData const *sblock = mir_bb_data(mir, *ps);
            K_LIST_FOREACH (sblock->joins, pinstr) {
                struct MirPhi const *phi = MirGetPhi(*pinstr);
                int const p = mir_which_pred(L->mir, *ps, b);
                struct MirPlace const input = MirPlaceList_get(phi->inputs, p);
                if (input.kind == MIR_PLACE_LOCAL) add_live_reg(L, live, input.r);
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
            struct MirPhi const *phi = MirGetPhi(*pinstr);
            remove_live_reg(L, live, phi->output.r);
        }

        // Special handling for loop headers. Variables live at the loop header must be
        // live until the end of the loop, since they are needed in subsequent iterations.
        MirBlock const *pb;
        struct MirBlockList const *loop_ends = determine_loop_ends(L, b);
        K_LIST_FOREACH (loop_ends, pb) {
            struct MirBlockData *end = mir_bb_data(mir, *pb);
            K_LIST_FOREACH (live, pr)
                add_range(L, *pr, from, bb_last_loc(L, end));
        }
        RegisterSetList_set(L->live, b.value, live);
    }
}

static void init_live_interval(struct Liveness *L, MirRegister r, MirId mid, int max_position, struct MirInstruction *instr)
{
    int const location = pawMir_get_location(L->locations, mid);
    struct MirLiveInterval *it = pawMir_new_interval(L->C, r, max_position + 1);
    pawP_bitset_set_range(it->ranges, location, location + 1);
    it->first = it->last = location;
    map_reg_to_interval(L, r, it);
    it->instr = instr;
}

static void init_live_intervals(struct Liveness *L, struct MirBlockList *order, int max_position)
{
    MirBlock *b;
    K_LIST_FOREACH (order, b) {
        struct MirInstruction *const *pinstr;
        struct MirBlockData *block = mir_bb_data(L->mir, *b);

        // add an interval for each phi node (all phi nodes define a variable)
        K_LIST_FOREACH (block->joins, pinstr) {
            struct MirPhi *phi = MirGetPhi(*pinstr);
            init_live_interval(L, phi->output.r, block->mid, max_position, *pinstr);
        }

        // add an interval for each variable
        K_LIST_FOREACH (block->instructions, pinstr) {
            struct MirPlace *const *ppstore;
            MirPlacePtrList const *stores = pawMir_get_stores(L->mir, *pinstr);
            K_LIST_FOREACH (stores, ppstore)
                init_live_interval(L, (*ppstore)->r, block->mid, max_position, *pinstr);
        }
    }
}

static paw_Bool block_set_contains(struct MirBlockList *set, MirBlock b)
{
    MirBlock const *pb;
    K_LIST_FOREACH (set, pb) {
        if (MIR_ID_EQUALS(b, *pb))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

inline static int find_place(struct MirPlacePtrList const *places, MirRegister r)
{
    int index;
    struct MirPlace *const *ppp;
    K_LIST_ENUMERATE (places, index, ppp) {
        if (MIR_ID_EQUALS(r, (*ppp)->r))
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
            if (find_place(loads, r) >= 0) break;

            MirPlacePtrList const *stores = pawMir_get_stores(mir, *pinstr);
            if (find_place(stores, r) >= 0) {
                MirBlockList_swap_remove(W, index);
                break;
            }
        }
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

struct MirIntervalMap *pawMir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirLocationList *locations)
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
    L.mapping = MirIntervalMap_new(mir);

    int const max_position = bb_last_loc(&L, mir_bb_data(mir, K_LIST_LAST(order)));
    int const nparameters = mir->param_size;
    int const ncaptured = mir->captured->count;
    int const nregisters = mir->registers->count;
    int const nblocks = mir->blocks->count;

    init_live_intervals(&L, order, max_position);

    // initialize liveness sets
    RegisterSetList_reserve(&L, L.live, nblocks);
    while (L.live->count < nblocks)
        RegisterSetList_push(&L, L.live, MirRegisterList_new_from(mir, L.pool));

    // determine live intervals
    compute_liveness(&L, mir, order);

    // The callee, arguments, and captured variables get dedicated registers. The
    // callee needs to be live for the duration of the function call so it is not
    // collected by the GC.
    {
        for (int i = 0; i < 1 + nparameters; ++i) {
            struct MirLiveInterval const *it = interval_for_reg(&L, MIR_REG(i));
            add_range(&L, MIR_REG(i), it->first, max_position);
        }

        struct MirCaptureInfo *pci;
        K_LIST_FOREACH (mir->captured, pci) {
            struct MirLiveInterval const *it = interval_for_reg(&L, pci->r);
            add_range(&L, pci->r, it->first, max_position);
        }
    }

    pawP_pool_free(C, L.pool);
    return L.mapping;
}
