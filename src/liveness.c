// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// liveness.c: Live variable analysis
//
// References:
// (1) Wimmer, C., & Franz, M. (2010). Linear Scan Register Allocation on SSA Form.

#include <stdlib.h>
#include "compile.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"

struct Liveness {
    struct MirIntervalList *intervals;
    struct MirLocationList *locations;
    struct RegisterSetList *live;
    struct Compiler *C;
    struct Mir *mir;
    Map *mapping;
    int index;
};

DEFINE_LIST(struct Compiler, regset_list_, RegisterSetList, struct MirRegisterList *)

static void map_reg_to_interval(struct Liveness *L, MirRegister r, struct MirLiveInterval *it)
{
    pawH_insert(ENV(L->C), L->mapping, I2V(r.value), P2V(it));
}

static struct MirLiveInterval *interval_for_reg(struct Liveness *L, MirRegister r)
{
    return pawH_get(L->mapping, I2V(r.value))->p;
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
    K_LIST_FOREACH(set, pr) {
        if (MIR_REG_EQUALS(r, *pr)) return;
    }
    K_LIST_PUSH(L->C, set, r);
}

static void remove_live_reg(struct Liveness *L, struct MirRegisterList *set, MirRegister r)
{
    for (int i = 0; i < set->count; ++i) {
        const MirRegister r2 = K_LIST_GET(set, i);
        if (MIR_REG_EQUALS(r, r2)) {
            K_LIST_SET(set, i, K_LIST_LAST(set));
            K_LIST_POP(set);
            return;
        }
    }
}

// Indicate that register "r" is defined by instruction "x"
#define OUTPUT(L, loc, r) do { \
        set_from(L, r, loc, block); \
        remove_live_reg(L, set, r); \
    } while (0)

// Indicate that register "r" is used by instruction "x"
#define INPUT(L, loc, r) do { \
        add_range(L, r, bb_first_loc(L, block), loc); \
        add_live_reg(L, set, r); \
    } while (0)

static void step_instruction(struct Liveness *L, struct MirRegisterList *set, struct MirBlockData *block, struct MirInstruction *instr)
{
    MirRegister *pstore = pawMir_get_store(L->C, instr);
    if (pstore != NULL) OUTPUT(L, instr_loc(L, instr), *pstore);

    MirRegister *const *ppr;
    struct MirRegisterPtrList *ploads = pawMir_get_loads(L->C, instr);
    K_LIST_FOREACH(ploads, ppr) INPUT(L, instr_loc(L, instr), **ppr);
}

#undef INPUT
#undef OUTPUT

// TODO
#include <stdio.h>

static void dump_live_intervals(struct Liveness *L, struct MirIntervalList *intervals)
{
    struct MirLiveInterval **iter;
    printf("live_intervals = {\n");
    K_LIST_FOREACH(intervals, iter) {
        struct MirLiveInterval *live = *iter;
        printf("  _%d: ", live->r.value);
        for (int i = 0; i < live->ranges->count; ++i) {
            if (i > 0) printf(" ");
            putchar(pawP_bitset_get(live->ranges, i) ? '*' : ' ');
        }
        printf("]\n");
    }
    printf("}\n");
}

const char *pawP_print_live_intervals_pretty(struct Compiler *C, struct Mir *mir, struct MirIntervalList *intervals)
{
    return "TODO";
//    Buffer buf;
//    paw_Env *P = ENV(C);
//    pawL_init_buffer(P, &buf);
//    const int nr = mir->registers->count;
//    struct MirBlockList *order = pawMir_traverse_rpo(C, mir);
//    struct MirBlockData *last = mir_bb_data(mir, K_LIST_LAST(order));
//    const int npositions = bb_last_loc(last) + 2;
//
//#define PAD_DECIMAL(i) { \
//        if ((i) < 10) { \
//            L_ADD_LITERAL(P, &buf, "  "); \
//        } else if ((i) < 100) { \
//            L_ADD_LITERAL(P, &buf, " "); \
//        } \
//    }
//
//    L_ADD_LITERAL(P, &buf, " i  ");
//    for (int i = 0; i < nr; ++i) {
//        if (i > 0) L_ADD_LITERAL(P, &buf, " ");
//        const MirRegister r = MIR_REG(i);
//        pawL_add_fstring(P, &buf, "_%d", r.value);
//        if (i < nr) PAD_DECIMAL(r.value);
//    }
//    pawL_add_char(P, &buf, '\n');
//    for (int i = 0; i < 1 + nr; ++i) {
//        L_ADD_LITERAL(P, &buf, "-----");
//    }
//    pawL_add_char(P, &buf, '\n');
//
//    int ninstr = 0;
//    char *buffer = calloc(npositions, mir->registers->count);
//    struct MirLiveInterval **iter;
//    K_LIST_FOREACH(intervals, iter) {
//        struct MirLiveInterval *it = *iter;
//        ninstr = PAW_MAX(it->last, ninstr);
//        for (int i = 0; i < pawP_bitset_count(it->ranges); ++i) {
//            if (pawP_bitset_get(it->ranges, i)) {
//                buffer[i * nr + it->r.value] = '|';
//            }
//        }
//    }
//    for (int i = 0; i < ninstr; ++i) {
//        pawL_add_fstring(P, &buf, " %d ", i);
//        PAD_DECIMAL(i);
//        for (int j = 0; j < nr; ++j) {
//            if (j > 0) L_ADD_LITERAL(P, &buf, "    ");
//            const char c = buffer[i * nr + j];
//            if (c != '\0') pawL_add_char(P, &buf, c);
//            else pawL_add_char(P, &buf, '.');
//        }
//        pawL_add_char(P, &buf, '\n');
//    }
//    pawL_push_result(P, &buf);
//    free(buffer);
//    return paw_string(P, -1);
}

// Return a list of all blocks containing back edges to a loop header
static struct MirBlockList *determine_loop_ends(struct Liveness *L, MirBlock header)
{
    struct MirBlockData *data = mir_bb_data(L->mir, header);
    struct MirBlockList *loop_ends = pawMir_block_list_new(L->C);

    const MirBlock *pb;
    K_LIST_FOREACH(data->predecessors, pb) {
        if (pb->value >= header.value) {
            K_LIST_PUSH(L->C, loop_ends, *pb);
        }
    }
    return loop_ends;
}

static void compute_liveness(struct Liveness *L, struct Mir *mir, struct MirBlockList *order)
{
    for (int ib = order->count - 1; ib >= 0; --ib) {
        const MirBlock b = K_LIST_GET(order, ib);
        struct MirBlockData *block = mir_bb_data(mir, b);

        MirBlock *ps;
        MirRegister *pr;
        struct MirInstruction **pinstr;

        // "live" is the union of the sets of live registers from the successors of "b"
        struct MirRegisterList *live = pawMir_register_list_new(L->C);
        K_LIST_FOREACH(block->successors, ps) {
            struct MirRegisterList *set = K_LIST_GET(L->live, ps->value);
            K_LIST_FOREACH(set, pr) add_live_reg(L, live, *pr);
        }

        // account for inputs to phi functions in successor blocks, which are live from
        // where they are defined until the end of this block
        K_LIST_FOREACH(block->successors, ps) {
            struct MirBlockData *sblock = mir_bb_data(mir, *ps);
            K_LIST_FOREACH(sblock->joins, pinstr) {
                struct MirPhi *phi = MirGetPhi(*pinstr);
                const int p = mir_which_pred(L->mir, *ps, b);
                add_live_reg(L, live, K_LIST_GET(phi->inputs, p));
            }
        }

        // initialize ranges to span the whole block (may be refined in the next loop,
        // if a definition for the given variable is encountered in this block)
        const int from = bb_first_loc(L, block);
        const int to = bb_last_loc(L, block);
        K_LIST_FOREACH(live, pr) {
            add_range(L, *pr, from, to);
        }

        for (int io = block->instructions->count - 1; io >= 0; --io) {
            struct MirInstruction *op = K_LIST_GET(block->instructions, io);
            step_instruction(L, live, block, op);
        }

        // account for phi function outputs, which are live from the start of the current
        // block to the location they are last used
        K_LIST_FOREACH(block->joins, pinstr) {
            struct MirPhi *phi = MirGetPhi(*pinstr);
            remove_live_reg(L, live, phi->output);
        }

        // Special handling for loop headers. Variables live at the loop header must be
        // live until the end of the loop, since they are needed in subsequent iterations.
        const MirBlock *pb;
        struct MirBlockList *loop_ends = determine_loop_ends(L, b);
        K_LIST_FOREACH(loop_ends, pb) {
            struct MirBlockData *end = mir_bb_data(mir, *pb);
            K_LIST_FOREACH(live, pr) add_range(L, *pr, from, bb_last_loc(L, end));
        }
        K_LIST_SET(L->live, b.value, live);
    }
}

static void add_live_interval(struct Liveness*L, MirRegister r, MirId mid, int npositions, struct MirInstruction *instr)
{
    const int location = pawMir_get_location(L->locations, mid);
    struct MirLiveInterval *it = pawMir_new_interval(L->C, r, npositions);
    map_reg_to_interval(L, r, it);
    it->instr = instr;
    it->first = location;
    it->last = location + 2;
    pawP_bitset_set_range(it->ranges, it->first, it->last);
    K_LIST_PUSH(L->C, L->intervals, it);
}

static void init_live_intervals(struct Liveness *L, struct MirBlockList *order, int npos)
{
    K_LIST_RESERVE(L->C, L->intervals, L->mir->registers->count);

    MirBlock *b;
    K_LIST_FOREACH(order, b) {
        struct MirInstruction **pinstr;
        struct MirBlockData *block = mir_bb_data(L->mir, *b);

        // add an interval for each phi node (all phi nodes define a variable)
        K_LIST_FOREACH(block->joins, pinstr) {
            struct MirPhi *phi = MirGetPhi(*pinstr);
            add_live_interval(L, phi->output, block->mid, npos, *pinstr);
        }

        // add an interval for each variable
        K_LIST_FOREACH(block->instructions, pinstr) {
            const MirRegister *pstore = pawMir_get_store(L->C, *pinstr);
            if (pstore == NULL) continue;
            add_live_interval(L, *pstore, block->mid, npos, *pinstr);
        }
    }
}

static paw_Bool block_set_contains(struct MirBlockList *set, MirBlock b)
{
    const MirBlock *pb;
    K_LIST_FOREACH(set, pb) {
        if (MIR_BB_EQUALS(b, *pb)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

static inline int reglist_find(const struct MirRegisterPtrList *regs, MirRegister r)
{
    int index;
    MirRegister *const *ppr;
    K_LIST_ENUMERATE(regs, index, ppr) {
        if (MIR_REG_EQUALS(r, **ppr)) return index;
    }
    return -1;
}

struct MirBlockList *pawMir_compute_live_in(struct Mir *mir, struct MirBlockList *uses, struct MirBlockList *defs, MirRegister r)
{
    // algorithm is from LLVM "mem2reg" pass
    struct Compiler *C = mir->C;

    const MirBlock *pb;
    struct MirBlockList *result = pawMir_block_list_new(C);
    struct MirBlockList *W = pawMir_block_list_new(C);
    K_LIST_RESERVE(C, W, uses->count);
    K_LIST_FOREACH(uses, pb) K_LIST_PUSH(C, W, *pb);

    for (int i = 0; i < W->count; ++i) {
        const MirBlock b = K_LIST_GET(W, i);
        if (!block_set_contains(defs, b)) continue;
        struct MirBlockData *bb = mir_bb_data(mir, b);

        struct MirInstruction **pinstr;
        K_LIST_FOREACH(bb->instructions, pinstr) {
            // If there is a store before a load for "r" in basic block "b", then "r"
            // is not live-in to "b". The load/store checks are performed in reverse
            // compared to the LLVM algorithm, because some instructions both load
            // and store a particular variable (this routine is run before SSA is
            // constructed). Instructions read their operands before writing their
            // output, so loads must be checked before stores. e.g. "x = x + 1"
            // loads "x" before writing to it.
            struct MirRegisterPtrList *ploads = pawMir_get_loads(C, *pinstr);
            const int index = reglist_find(ploads, r);
            if (index >= 0) goto found_use;

            const MirRegister *pstore = pawMir_get_store(C, *pinstr);
            if (pstore != NULL && MIR_REG_EQUALS(*pstore, r)) {
                K_LIST_SET(W, i, K_LIST_LAST(W));
                K_LIST_POP(W);
                --i;
                break;
            }
        }
found_use:;
   }

    while (W->count > 0) {
        const MirBlock b = K_LIST_LAST(W);
        K_LIST_POP(W);

        if (block_set_contains(result, b)) continue;
        K_LIST_PUSH(C, result, b);

        const MirBlock *pp;
        struct MirBlockData *bb = mir_bb_data(mir, b);
        K_LIST_FOREACH(bb->predecessors, pp) {
            if (!block_set_contains(defs, *pp)) K_LIST_PUSH(C, W, *pp);
        }
    }
    return result;
}

static void extend_captured_intervals(struct Liveness *L, struct Mir *mir)
{
    struct MirLiveInterval **pit;
    K_LIST_FOREACH(L->intervals, pit) {
        struct MirLiveInterval *it = *pit;
        struct MirRegisterData *data = mir_reg_data(mir, it->r);
        if (data->is_captured) {
            pawP_bitset_set_range(it->ranges, it->first, it->last);
        }
    }
}

struct MirIntervalList *pawMir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirLocationList *locations)
{
    struct Liveness L = {
        .intervals = pawMir_interval_list_new(C),
        .locations = locations,
        .live = regset_list_new(C),
        .mir = mir,
        .C = C,
    };
    L.mapping = pawP_push_map(C);

    struct MirBlockData *last = mir_bb_data(mir, K_LIST_LAST(order));
    const int nparameters = IR_FPTR(mir->type)->params->count;
    const int npositions = bb_last_loc(&L, last) + 2;
    const int ncaptured = mir->captured->count;
    const int nregisters = mir->registers->count;
    const int nblocks = mir->blocks->count;

    init_live_intervals(&L, order, npositions);

    // initialize liveness sets
    K_LIST_RESERVE(C, L.live, nblocks);
    for (int i = 0; i < nblocks; ++i) {
        K_LIST_PUSH(C, L.live, pawMir_register_list_new(C));
    }

    // arguments and captured variables get dedicated registers
    {
        for (int i = 0; i < 1 + nparameters; ++i) {
            add_range(&L, MIR_REG(i), 0, npositions);
        }

        struct MirCaptureInfo *pci;
        K_LIST_FOREACH(mir->captured, pci) {
            add_range(&L, pci->r, 0, npositions);
        }
    }

    // determine live intervals
    compute_liveness(&L, mir, order);
    extend_captured_intervals(&L, mir);

    pawP_pop_object(C, L.mapping);
    return L.intervals;
}

