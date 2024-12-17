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

static void set_from(struct Liveness *L, MirRegister opd, int from, struct MirBlockData *block)
{
    struct MirLiveInterval *it = interval_for_reg(L, opd);
    pawP_bitset_clear_range(it->ranges, mir_bb_first(block), from);
    pawP_bitset_set(it->ranges, from);
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
            for (; i < set->count - 1; ++i) {
                const MirRegister r2 = K_LIST_GET(set, i + 1);
                K_LIST_SET(set, i, r2);
            }
            --set->count;
            break;
        }
    }
}

// Indicate that register "r" is defined by instruction "x"
#define OUTPUT(L, x, r) set_from(L, r, (x)->location, block); \
                        remove_live_reg(L, set, r);

// Indicate that register "r" is used by instruction "x"
#define INPUT(L, x, r) add_range(L, r, mir_bb_first(block), (x)->location); \
                       add_live_reg(L, set, r);

static void step_instruction(struct Liveness *L, struct MirRegisterList *set, struct MirBlockData *block, struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirPhi:
            PAW_UNREACHABLE();

       case kMirMove: {
            struct MirMove *x = MirGetMove(instr);
            OUTPUT(L, x, x->output);
            INPUT(L, x, x->target);
            break;
        }
        case kMirUpvalue: {
            struct MirUpvalue *x = MirGetUpvalue(instr);
            OUTPUT(L, x, x->output);
            break;
        }
        case kMirGlobal: {
            struct MirGlobal *x = MirGetGlobal(instr);
            OUTPUT(L, x, x->output);
            break;
        }
        case kMirAllocLocal: {
            struct MirAllocLocal *x = MirGetAllocLocal(instr);
            OUTPUT(L, x, x->output);
            break;
        }
        case kMirFreeLocal: {
            struct MirFreeLocal *x = MirGetFreeLocal(instr);
            INPUT(L, x, x->reg);
            break;
        }
        case kMirSetLocal: {
            struct MirSetLocal *x = MirGetSetLocal(instr);
            OUTPUT(L, x, x->target);
            INPUT(L, x, x->value);
            break;
        }
        case kMirSetUpvalue: {
            struct MirSetUpvalue *x = MirGetSetUpvalue(instr);
            INPUT(L, x, x->value);
            break;
        }
        case kMirConstant: {
            struct MirConstant *x = MirGetConstant(instr);
            OUTPUT(L, x, x->output);
            break;
        }
        case kMirAggregate: {
            struct MirAggregate *x = MirGetAggregate(instr);
            OUTPUT(L, x, x->output);
            break;
        }
        case kMirContainer: {
            struct MirContainer *x = MirGetContainer(instr);
            OUTPUT(L, x, x->output);
            break;
        }
        case kMirCall: {
            struct MirCall *x = MirGetCall(instr);
            OUTPUT(L, x, x->output);
            INPUT(L, x, x->target);
            for (int i = 0; i < x->args->count; ++i) {
                INPUT(L, x, K_LIST_GET(x->args, i));
            }
            break;
        }
        case kMirCast: {
            struct MirCast *x = MirGetCast(instr);
            OUTPUT(L, x, x->output);
            INPUT(L, x, x->target);
            break;
        }
        case kMirClose: {
            struct MirClose *x = MirGetClose(instr);
            INPUT(L, x, x->target);
            break;
        }
        case kMirClosure: {
            struct MirClosure *x = MirGetClosure(instr);
            OUTPUT(L, x, x->output);
            break;
        }
        case kMirSetElement: {
            struct MirSetElement *x = MirGetSetElement(instr);
            INPUT(L, x, x->object);
            INPUT(L, x, x->key);
            INPUT(L, x, x->value);
            break;
        }
        case kMirGetElement: {
            struct MirGetElement *x = MirGetGetElement(instr);
            OUTPUT(L, x, x->output);
            INPUT(L, x, x->object);
            INPUT(L, x, x->key);
            break;
        }
        case kMirSetRange: {
            struct MirSetRange *x = MirGetSetRange(instr);
            INPUT(L, x, x->object);
            INPUT(L, x, x->lower);
            INPUT(L, x, x->upper);
            INPUT(L, x, x->value);
            break;
        }
        case kMirGetRange: {
            struct MirGetRange *x = MirGetGetRange(instr);
            OUTPUT(L, x, x->output);
            INPUT(L, x, x->object);
            INPUT(L, x, x->lower);
            INPUT(L, x, x->upper);
            break;
        }
        case kMirSetField: {
            struct MirSetField *x = MirGetSetField(instr);
            INPUT(L, x, x->object);
            INPUT(L, x, x->value);
            break;
        }
        case kMirGetField: {
            struct MirGetField *x = MirGetGetField(instr);
            OUTPUT(L, x, x->output);
            INPUT(L, x, x->object);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *x = MirGetUnaryOp(instr);
            OUTPUT(L, x, x->output);
            INPUT(L, x, x->val);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *x = MirGetBinaryOp(instr);
            OUTPUT(L, x, x->output);
            INPUT(L, x, x->lhs);
            INPUT(L, x, x->rhs);
            break;
        }
        case kMirGoto:
            break;
        case kMirReturn: {
            struct MirReturn *x = MirGetReturn(instr);
            INPUT(L, x, x->value);
            break;
        }
        case kMirBranch: {
            struct MirBranch *x = MirGetBranch(instr);
            INPUT(L, x, x->cond);
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *x = MirGetSwitch(instr);
            INPUT(L, x, x->discr);
            break;
        }
        case kMirForLoop: {
            struct MirForLoop *x = MirGetForLoop(instr);
            switch (x->for_kind) {
                case MIR_FORNUM_PREP:
                    INPUT(L, x, x->fornum.begin);
                    INPUT(L, x, x->fornum.end);
                    INPUT(L, x, x->fornum.step);
                    OUTPUT(L, x, x->fornum.var);
                    break;
                case MIR_FORNUM_LOOP:
                    INPUT(L, x, x->fornum.begin);
                    INPUT(L, x, x->fornum.end);
                    INPUT(L, x, x->fornum.step);
                    INPUT(L, x, x->fornum.var);
            }
            break;
        }
    }
}

#undef INPUT
#undef OUTPUT

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

static const char *dump_live_intervals_pretty(struct Liveness *L, struct MirIntervalList *intervals, int npositions)
{
    Buffer buf;
    paw_Env *P = ENV(L->C);
    pawL_init_buffer(P, &buf);
    const int nr = L->mir->registers->count;

#define PAD_DECIMAL(i) { \
        if ((i) < 10) { \
            L_ADD_LITERAL(P, &buf, "  "); \
        } else if ((i) < 100) { \
            L_ADD_LITERAL(P, &buf, " "); \
        } \
    }

    L_ADD_LITERAL(P, &buf, " i  ");
    for (int i = 0; i < nr; ++i) {
        if (i > 0) L_ADD_LITERAL(P, &buf, " ");
        const MirRegister r = MIR_REG(i);
        pawL_add_fstring(P, &buf, "_%d", r.value);
        if (i < nr) PAD_DECIMAL(r.value);
    }
    pawL_add_char(P, &buf, '\n');
    for (int i = 0; i < 1 + nr; ++i) {
        L_ADD_LITERAL(P, &buf, "-----");
    }
    pawL_add_char(P, &buf, '\n');

    int ninstr = 0;
    char *buffer = calloc(npositions, L->mir->registers->count);
    struct MirLiveInterval **iter;
    K_LIST_FOREACH(intervals, iter) {
        struct MirLiveInterval *it = *iter;
        ninstr = PAW_MAX(it->last, ninstr);
        for (int i = 0; i < pawP_bitset_count(it->ranges); ++i) {
            if (pawP_bitset_get(it->ranges, i)) {
                buffer[i * nr + it->r.value] = '|';
            }
        }
    }
    for (int i = 0; i < ninstr; ++i) {
        pawL_add_fstring(P, &buf, " %d ", i);
        PAD_DECIMAL(i);
        for (int j = 0; j < nr; ++j) {
            if (j > 0) L_ADD_LITERAL(P, &buf, "    ");
            const char c = buffer[i * nr + j];
            if (c != '\0') pawL_add_char(P, &buf, c);
            else pawL_add_char(P, &buf, '.');
        }
        pawL_add_char(P, &buf, '\n');
    }
    pawL_push_result(P, &buf);
    free(buffer);
    return paw_string(P, -1);
}

static void compute_liveness(struct Liveness *L, struct Mir *mir, struct MirBlockList *order)
{
    for (int ib = order->count - 1; ib >= 0; --ib) {
        const MirBlock b = K_LIST_GET(order, ib);
        struct MirBlockData *block = mir_bb_data(mir, b);

        MirBlock *ps;
        MirRegister *pr;
        struct MirInstruction **instr;

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
            K_LIST_FOREACH(sblock->joins, instr) {
                struct MirPhi *phi = MirGetPhi(*instr);
                const int p = mir_which_pred(L->mir, *ps, b);
                add_live_reg(L, live, K_LIST_GET(phi->inputs, p));
            }
        }

        // initialize ranges to span the whole block (may be refined in the next loop,
        // if a definition for the given variable is encountered in this block)
        const int from = mir_bb_first(block);
        const int to = mir_bb_last(block);
        K_LIST_FOREACH(live, pr) {
            add_range(L, *pr, from, to);
        }

        for (int io = block->instructions->count - 1; io >= 0; --io) {
            struct MirInstruction *op = K_LIST_GET(block->instructions, io);
            step_instruction(L, live, block, op);
        }

        // account for phi function outputs, which are live from the start of the current
        // block to the location they are last used
        K_LIST_FOREACH(block->joins, instr) {
            struct MirPhi *phi = MirGetPhi(*instr);
            remove_live_reg(L, live, phi->output);
        }

        // Special handling for loop headers. Variables live at the loop header must be
        // live until the end of the loop, since they are needed in subsequent iterations.
        if (MIR_BB_EXISTS(block->loop_end)) {
            struct MirBlockData *loop_end = mir_bb_data(mir, block->loop_end);
            K_LIST_FOREACH(live, pr) add_range(L, *pr, from, mir_bb_last(loop_end));
        }
        K_LIST_SET(L->live, b.value, live);
    }
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
            struct MirLiveInterval *it = pawMir_new_interval(L->C, phi->output, npos);
            map_reg_to_interval(L, it->r, it);
            K_LIST_PUSH(L->C, L->intervals, it);
            it->first = block->location;
            it->instr = *pinstr;
        }

        // add an interval for each variable
        K_LIST_FOREACH(block->instructions, pinstr) {
            struct MirStore store;
            if (pawMir_check_store(L->C, *pinstr, &store)) {
                MirRegister **ppr;
                K_LIST_FOREACH(store.outputs, ppr) {
                    struct MirLiveInterval *it = pawMir_new_interval(L->C, **ppr, npos);
                    K_LIST_PUSH(L->C, L->intervals, it);
                    map_reg_to_interval(L, **ppr, it);
                    it->instr = *pinstr;
                }
            }
        }
    }
}

static paw_Bool set_contains(struct MirBlockList *set, MirBlock b)
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

struct MirBlockList *pawMir_compute_live_in(struct Compiler *C, struct Mir *mir, struct MirBlockList *uses, struct MirBlockList *defs, MirRegister r)
{
    // algorithm is from LLVM "mem2reg" pass

    const MirBlock *pb;
    struct MirBlockList *result = pawMir_block_list_new(C);
    struct MirBlockList *W = pawMir_block_list_new(C);
    K_LIST_RESERVE(C, W, uses->count);
    K_LIST_FOREACH(uses, pb) K_LIST_PUSH(C, W, *pb);

    for (int i = 0; i < W->count; ++i) {
        const MirBlock b = K_LIST_GET(W, i);
        if (!set_contains(defs, b)) continue;
        struct MirBlockData *bb = mir_bb_data(mir, b);

        struct MirInstruction **pinstr;
        K_LIST_FOREACH(bb->instructions, pinstr) {
            struct MirStore store;
            if (pawMir_check_store(C, *pinstr, &store)) {
                const int index = reglist_find(store.outputs, r);
                if (index < 0) continue;
                K_LIST_SET(W, i, K_LIST_LAST(W));
                K_LIST_POP(W);
                --i;
                break;
            }

            struct MirLoad load;
            if (pawMir_check_load(C, *pinstr, &load)) {
                const int index = reglist_find(load.inputs, r);
                if (index >= 0) goto found_use;
            }
        }
found_use:;
   }

    while (W->count > 0) {
        const MirBlock b = K_LIST_LAST(W);
        K_LIST_POP(W);

        if (set_contains(result, b)) continue;
        K_LIST_PUSH(C, result, b);

        const MirBlock *pp;
        struct MirBlockData *bb = mir_bb_data(mir, b);
        K_LIST_FOREACH(bb->predecessors, pp) {
            if (!set_contains(defs, *pp)) K_LIST_PUSH(C, W, *pp);
        }
    }
    return result;
}

struct MirIntervalList *pawMir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order)
{
    struct Liveness L = {
        .intervals = pawMir_interval_list_new(C),
        .live = regset_list_new(C),
        .mir = mir,
        .C = C,
    };
    L.mapping = pawP_push_map(C);

    struct MirBlockData *last = mir_bb_data(mir, K_LIST_LAST(order));
    const int nparameters = IR_FPTR(mir->type)->params->count;
    const int npositions = mir_bb_last(last) + 2;
    const int ncaptured = mir->captured->count;
    const int nregisters = mir->registers->count;
    const int nblocks = mir->blocks->count;

printf("%s\n", pawMir_dump(C, mir));--ENV(C)->top.p;
    init_live_intervals(&L, order, npositions);

    // initialize liveness sets
    K_LIST_RESERVE(C, L.live, nblocks);
    for (int i = 0; i < nblocks; ++i) {
        K_LIST_PUSH(C, L.live, pawMir_register_list_new(C));
    }

    for (int i = 0; i < 1 + nparameters; ++i) {
        add_range(&L, MIR_REG(i), 0, npositions);
    }
    // TODO: something like this, store captured variables in a single register
    //       for the whole function. captured vars should be in order of capture time
    //       so that OP_CLOSE can be used normally.
    for (int i = 0; i < ncaptured; ++i) {
        struct MirCaptureInfo c = K_LIST_GET(mir->captured, i);
        add_range(&L, c.r, 0, npositions);
    }

printf("%s\n", pawMir_dump(C, mir));--ENV(C)->top.p;

    // determine live intervals
    compute_liveness(&L, mir, order);

    pawP_pop_object(C, L.mapping);
    return L.intervals;
}

