// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// regalloc.c: Register allocator
//
// Performs register allocation for code running on Paw's VM. There are ~250
// registers, and spills to memory are not considered (an error is thrown if a
// spill is needed). Registers reside within the activation frame of the
// current function, and are numbered starting from 0.
//
// TODO: look into irreducible loops (loops w/ > 1 entry node); the Wimmer
//       SSA paper says to insert phi nodes at loop headers for variables
//       not used inside the loop, so that liveness info is propagated to
//       all loop headers. currently, Paw cannot produce this type of loop,
//       so this isn't as important as the other TODO's
//
// References:
// (1) Wimmer, C., & Moessenboeck, H. (). Optimized Interval Splitting in a Linear
//     Scan Register Allocator.

#include "compile.h"
#include "map.h"
#include "mir.h"
#include "ir_type.h"
#include "ssa.h"

#define ERROR(R, code, ...) pawE_error(ENV((R)->C), code, -1, __VA_ARGS__)

enum AllocationKind {
    ALLOCATE_NEXT,
    ALLOCATE_ANY,
};

struct RegisterAllocator {
    struct MirIntervalList *intervals;
    struct MirInstructionList *code;
    struct Compiler *C;
    struct Mir *mir;

    // result: stores the mapping from virtual to physical registers
    // handled: intervals ending before "position"
    // unhandled: intervals starting after "position"
    // active: intervals assigned to registers
    // inactive: intervals with a hole at "position"
    Map *handled, *active, *inactive; // struct MirLiveRange * => NULL
    struct MirIntervalList *unhandled;
    struct MirLiveInterval *current;
    struct RegisterTable *result;
    int max_position;
    int position;

    // ID of the highest-numbered free register, or the top of the register
    // stack
    int free_reg;

    // largest register ID encountered
    int max_reg;
};

#define SET_FOREACH(S, iter, it, code) do { \
        paw_Int iter = PAW_ITER_INIT; \
        while (pawH_iter(S, &iter)) { \
            const Value *pk_ = pawH_key(S, iter); \
            struct MirLiveInterval *it = pk_->p; \
            code; \
        } \
    } while (0)

#define SET_ENUMERATE(S, iter, i, it, code) do { \
        int i = 0; \
        paw_Int iter = PAW_ITER_INIT; \
        while (pawH_iter(S, &iter)) { \
            const Value *pk_ = pawH_key(S, iter); \
            struct MirLiveInterval *it = pk_->p; \
            code; \
            ++i; \
        } \
    } while (0)

#define REG(R, it) (K_LIST_GET((R)->result, (it)->r.value).value)
#define REG_EQUALS(a, b) ((a).value == (b).value)
#define REGINFO(x) ((struct RegisterInfo){x})
#define REG_EXISTS(r) (!REG_EQUALS(r, REGINFO(-1)))

static paw_Bool is_covered(struct MirLiveInterval *it, int pos)
{
    return pawP_bitset_get(it->ranges, pos);
}

static int decimal_padding(int d)
{
    paw_assert(0 <= d && d < 256);
    return d < 10 ? 2 : d < 100 ? 1 : 0;
}

static void dump_interval(struct MirLiveInterval *it, int start, int indent)
{
    printf("%*c", indent + decimal_padding(it->r.value), ' ');
    printf("_%d: ", it->r.value);

    paw_Bool first = PAW_TRUE;
    for (int i = start; i < it->ranges->count; ++i) {
        const paw_Bool b = pawP_bitset_get(it->ranges, i);
        if (i > start) putchar(' ');
        putchar(b ? '*' : '.');
    }
    printf("\n");
}

static void dump_set(Map *set, const char *name)
{
    printf("%s = [", name);
    SET_ENUMERATE(set, _, i, it, {
        if (i > 0) printf(", ");
        printf("_%d", it->r.value);
    });
    printf("]\n");
}

static void dump_result(struct RegisterTable *table)
{
    int i;
    struct RegisterInfo *pr;

    printf("result = [\n");
    K_LIST_ENUMERATE(table, i, pr) {
        printf("    _%d => ", i);
        if (pr->value < 0) putchar('-');
        else printf("r%d", pr->value);
        putchar('\n');
    };
    printf("]\n");
}

static void debug(struct RegisterAllocator *R)
{
    printf("position = %d\n", R->position);
    dump_set(R->handled, "handled");
    dump_set(R->active, "active");
    dump_set(R->inactive, "inactive");

    int i;
    struct MirLiveInterval **iter;
    printf("unhandled = {\n");
    K_LIST_ENUMERATE(R->unhandled, i, iter) {
        struct MirLiveInterval *it = *iter;
        if (!MIR_REG_EXISTS(it->r)) continue;
        if (it->last < R->position) continue;
        dump_interval(it, R->position, 4);
    }
    printf("}\n");

    dump_result(R->result);
}

static void debug_registers(struct RegisterAllocator *R, MirRegister *regs, int n)
{
    for (int i = 0; i < n; ++i) {
        const MirRegister r = regs[i];
        if (i > 0) fputc(' ', stderr);
        if (MIR_REG_EXISTS(r)) fputc('*', stderr);
        else fputc('.', stderr);
    }
}

static paw_Bool set_contains(struct RegisterAllocator *R, Map *set, struct MirLiveInterval *it)
{
    return pawH_get(set, P2V(it)) != NULL;
}

static void set_add(struct RegisterAllocator *R, Map *set, struct MirLiveInterval *it)
{
    pawH_insert(ENV(R->C), set, P2V(it), P2V(NULL));
}

static void set_erase(Map *set, paw_Int index)
{
    pawH_erase_at(set, index);
}

static void split_current(struct RegisterAllocator *R, int pos)
{
    paw_assert(pos > 0);
    struct MirLiveInterval *it = R->current;
    const int n = it->ranges->count;
    struct MirLiveInterval *split = pawMir_new_interval(R->C, it->r, n);
    split->instr = pawMir_new_instruction(R->C, 0);
    split->last = it->last;

    pawP_bitset_or(split->ranges, it->ranges);
    pawP_bitset_clear_range(it->ranges, 0, pos);
    pawP_bitset_clear_range(split->ranges, pos, n);
    *split->instr = *it->instr;

    for (int i = pos - 1; i >= 0; --i) {
        if (pawP_bitset_get(it->ranges, i)) {
            it->last = i;
            break;
        }
    }
    for (int i = pos; i < n; ++i) {
        if (pawP_bitset_get(split->ranges, i)) {
            split->first = i;
            break;
        }
    }

    // preserve the interval ordering by start position (odd positions are skipped
    // during MIR creation)
    const int new_pos = it->instr->hdr.location + 1;
    split->instr->hdr.location = new_pos;
    K_LIST_SET(R->unhandled, new_pos, split);
}

static struct RegisterInfo max_free_reg(struct RegisterAllocator *R, const int *free_until_pos, int *pmax_value)
{
    int max_index = 0;
    *pmax_value = free_until_pos[0];
    for (int i = 1; i < NREGISTERS; ++i) {
        if (*pmax_value < free_until_pos[i]) {
            *pmax_value = free_until_pos[i];
            max_index = i;
        }
    }
    return REGINFO(max_index);
}

// Determine the first position at which live intervals "a" and "b" intersect
static int next_intersection(struct MirLiveInterval *a, struct MirLiveInterval *b)
{
    paw_assert(a->ranges->count == b->ranges->count);
    for (int pos = 0; pos < a->ranges->count; ++pos) {
        const paw_Bool x = pawP_bitset_get(a->ranges, pos);
        const paw_Bool y = pawP_bitset_get(b->ranges, pos);
        if (x && y) return pos;
    }
    return -1;
}

static void not_enough_registers(struct RegisterAllocator *R)
{
    pawE_error(ENV(R->C), PAW_EOVERFLOW, -1, "not enough registers");
}

static struct RegisterInfo get_result(struct RegisterAllocator *R, MirRegister r)
{
    return K_LIST_GET(R->result, r.value);
}

static void try_allocate_free_reg(struct RegisterAllocator *R)
{
    struct MirLiveInterval *current = R->current;
    const struct RegisterInfo result = get_result(R, current->r);
    if (result.value >= 0) return;

    const struct MirRegisterData *data = mir_reg_data(R->mir, current->r);
    if (data->is_captured) {
        const struct RegisterInfo result = get_result(R, data->hint);
        K_LIST_SET(R->result, current->r.value, result);
        return;
    }

    int free_until_pos[NREGISTERS];
    for (int i = 0; i < NREGISTERS; ++i) {
        free_until_pos[i] = R->max_position;
    }

    // ignore registers already containing variables
    SET_FOREACH(R->active, _, it, {
        free_until_pos[REG(R, it)] = 0;
    });

    // TODO: this loop can be skipped if "it" was not split off of another
    //       interval, per the Wimmer SSA paper
    // determine where lifetime holes end, i.e. the next position where the
    // variable is expected to be in a register
    SET_FOREACH(R->inactive, _, it, {
        const int p = next_intersection(it, current);
        if (p >= 0) free_until_pos[REG(R, it)] = p;
    });

    int pos; // "reg" is free until this instruction
    const struct RegisterInfo reg = max_free_reg(R, free_until_pos, &pos);
    if (pos == 0) not_enough_registers(R); // no return
    R->max_reg = PAW_MAX(R->max_reg, reg.value);
    K_LIST_SET(R->result, current->r.value, reg);
    if (R->position >= pos) split_current(R, pos);
}

static void allocate_registers(struct RegisterAllocator *R)
{
    struct Mir *mir = R->mir;
    struct MirLiveInterval **pit;
    K_LIST_FOREACH(R->unhandled, pit) {
        R->current = *pit;
        R->position = R->current->first;
        if (!MIR_REG_EXISTS(R->current->r)) continue;

        SET_FOREACH(R->active, iter, it, {
            if (it->last < R->position) {
                set_add(R, R->handled, it);
                set_erase(R->active, iter);
            } else if (!is_covered(it, R->position)) {
                set_add(R, R->inactive, it);
                set_erase(R->active, iter);
            }
        });

        SET_FOREACH(R->inactive, iter, it, {
            if (it->last < R->position) {
                set_add(R, R->handled, it);
                set_erase(R->inactive, iter);
            } else if (is_covered(it, R->position)) {
                set_add(R, R->active, it);
                set_erase(R->inactive, iter);
            }
        });

        // allocate a register for the current interval
        try_allocate_free_reg(R);
        set_add(R, R->active, R->current);
    }
}

// TODO: determine live-in set for each basic block in liveness.c. use that instead of
//       this silly function
static void filter_live_intervals(struct RegisterAllocator *R, struct MirIntervalList *scratch, MirBlock b)
{
    struct MirBlockData *block = mir_bb_data(R->mir, b);
    const int location = mir_bb_first(block);
    scratch->count = 0;

    struct MirLiveInterval **pit;
    K_LIST_FOREACH(R->intervals, pit) {
        struct MirLiveInterval *it = *pit;
        if (pawP_bitset_get(it->ranges, location)) {
            K_LIST_PUSH(R->C, scratch, it);
        }
    }
}

struct Copy {
    MirRegister from;
    MirRegister to;
    int location;
};

DEFINE_LIST(struct Compiler, copy_list_, CopyList, struct Copy)

static MirRegister scratch_register(struct RegisterAllocator *R, MirRegister old)
{
    if (R->max_reg >= NREGISTERS) not_enough_registers(R);
    const MirRegister temp = MIR_REG(R->mir->registers->count);
    K_LIST_PUSH(R->C, R->mir->registers, *mir_reg_data(R->mir, old));
    K_LIST_PUSH(R->C, R->result, REGINFO(++R->max_reg));
    return temp;
}

static paw_Bool check_copy_conflict(struct RegisterAllocator *R, struct CopyList *copies, struct Copy copy, int skip)
{
    const struct RegisterInfo b = get_result(R, copy.to);

    int index;
    struct Copy *pcopy;
    K_LIST_ENUMERATE(copies, index, pcopy) {
        // NOTE: "skip" prevents NOOP copies ("x -> x") from being classified
        //       as a conflict
        const struct RegisterInfo a = get_result(R, pcopy->from);
        if (index != skip && REG_EQUALS(a, b)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void order_and_insert_copies(struct RegisterAllocator *R, struct MirBlockData *block, struct CopyList *pcopies)
{
    struct CopyList *seq = copy_list_new(R->C);
    int nscratch = 0;
    struct Copy *pcopy;
    int index;

    while (pcopies->count > 0) {
        K_LIST_ENUMERATE(pcopies, index, pcopy) {
            if (check_copy_conflict(R, pcopies, *pcopy, index)) {
                const MirRegister scratch = scratch_register(R, pcopy->to);
                K_LIST_PUSH(R->C, seq, ((struct Copy){pcopy->from, scratch}));
                pcopy->from = scratch;
                ++nscratch;
            } else {
                K_LIST_PUSH(R->C, seq, *pcopy);
                K_LIST_SET(pcopies, index, K_LIST_LAST(pcopies));
                --pcopies->count;
            }
        }
    }

    // copies must be added before the terminator
    struct MirInstruction *terminator = K_LIST_LAST(block->instructions);
    K_LIST_POP(block->instructions);

    K_LIST_FOREACH(seq, pcopy) {
        struct MirInstruction *move = pawMir_new_instruction(R->C, kMirMove);
        MirGetMove(move)->location = pcopy->location;
        MirGetMove(move)->target = pcopy->from;
        MirGetMove(move)->output = pcopy->to;
        K_LIST_PUSH(R->C, block->instructions, move);
    }
    K_LIST_PUSH(R->C, block->instructions, terminator);

    // temporary registers only used for this batch of moves
    R->max_reg -= nscratch;
    pcopies->count = 0;
}

static void resolve_registers(struct RegisterAllocator *R, struct MirBlockList *order)
{
    struct MirIntervalList *scratch = pawMir_interval_list_new(R->C);
    struct CopyList *resolved = copy_list_new(R->C);

    const MirBlock *b, *s;
    K_LIST_FOREACH(order, b) {
        struct MirInstruction **pinstr;
        struct MirBlockData *bb = mir_bb_data(R->mir, *b);
        K_LIST_FOREACH(bb->successors, s) {
            // for each control flow edge "b -> s", transform phi functions in "s"
            // into moves at the end of "b"
            struct MirBlockData *bs = mir_bb_data(R->mir, *s);
            K_LIST_FOREACH(bs->joins, pinstr) {
                struct MirPhi *phi = MirGetPhi(*pinstr);
                const int index = mir_which_pred(R->mir, *s, *b);
                const MirRegister opd = K_LIST_GET(phi->inputs, index);

                K_LIST_PUSH(R->C, resolved, ((struct Copy){
                                .location = mir_bb_last(bb),
                                .to = phi->output,
                                .from = opd,
                            }));
            }
            order_and_insert_copies(R, bb, resolved);
        }
    }

    // phi nodes have been transformed into moves
    K_LIST_FOREACH(order, b) {
        struct MirBlockData *block = mir_bb_data(R->mir, *b);
        block->joins->count = 0;
    }
}

static int compare_first(const void *lhs, const void *rhs)
{
    const struct MirLiveInterval *a = *CAST(const struct MirLiveInterval **, lhs);
    const struct MirLiveInterval *b = *CAST(const struct MirLiveInterval **, rhs);
    if (a->first < b->first) return -1;
    if (a->first > b->first) return 1;
    return 0;
}

static struct MirIntervalList *expand_with_placeholders(struct RegisterAllocator *R, struct MirIntervalList *intervals, int nskip)
{
    const int npositions = intervals->count * 2;
    struct MirIntervalList *result = pawMir_interval_list_new(R->C);
    K_LIST_RESERVE(R->C, result, npositions);

    int i;
    struct MirLiveInterval **pit;
    K_LIST_ENUMERATE(intervals, i, pit) {
        K_LIST_PUSH(R->C, result, *pit);

        // add empty interval to make splitting easier
        struct MirLiveInterval *empty = pawMir_new_interval(R->C,
                MIR_INVALID_REG, npositions);
        K_LIST_PUSH(R->C, result, empty);
    }

#undef PUSH_PLACEHOLDER

    return result;
}

struct RegisterTable *pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirIntervalList *intervals, int *pmax_reg)
{
    struct MirBlockData *last = K_LIST_LAST(mir->blocks);
    struct RegisterAllocator R = {
        .code = pawMir_instruction_list_new(C),
        .max_position = mir_bb_last(last),
        .intervals = intervals,
        .result = regtab_new(C),
        .mir = mir,
        .C = C,
    };

    R.handled = pawP_push_map(C);
    R.active = pawP_push_map(C);
    R.inactive = pawP_push_map(C);

    K_LIST_RESERVE(C, R.result, mir->registers->count);
    for (int i = 0; i < mir->registers->count; ++i) {
        K_LIST_PUSH(C, R.result, REGINFO(-1));
    }

    const int nparameters = IR_FPTR(mir->type)->params->count;
    const int ncaptured = mir->captured->count;

    // TODO: linked list threaded through set elements? build
    //       temp array to qsort, eventually implement mergesort
    //       to avoid temp array. avoids weird placeholder thing and is more flexible

    {
        // assign registers for result and arguments
        const int offset = 1 + nparameters;
        for (int i = 0; i < offset; ++i) {
            set_add(&R, R.active, K_LIST_GET(intervals, i));
            K_LIST_SET(R.result, i, REGINFO(i));
        }

        // assign registers for captured variables
        for (int i = 0; i < ncaptured; ++i) {
            struct MirCaptureInfo c = K_LIST_GET(mir->captured, i);
            if (c.r.value >= offset) {
                K_LIST_SET(R.result, c.r.value, REGINFO(offset + i));
            }
        }

        // extend live ranges for captured variables
        int rid = offset;
        struct MirLiveInterval **pit;
        K_LIST_FOREACH(intervals, pit) {
            struct MirLiveInterval *it = *pit;
            struct MirRegisterData *data = mir_reg_data(mir, it->r);
            if (!data->is_captured) continue;
            if (it->r.value < offset) continue;
            if (MIR_REG_EQUALS(it->r, data->hint)) {
                K_LIST_SET(R.result, it->r.value, REGINFO(rid++));
            } else {
                struct RegisterInfo result = get_result(&R, data->hint);
                K_LIST_SET(R.result, it->r.value, result);
            }
            const int npositions = K_LIST_LAST(intervals)->last;
            pawP_bitset_set_range(it->ranges, 0, npositions);
            it->last = npositions;
            it->first = 0;
        }
    }

    // sort live intervals by start point
    qsort(intervals->data, CAST_SIZE(intervals->count),
            sizeof(struct MirLiveInterval *), compare_first);
    R.unhandled = expand_with_placeholders(&R, intervals,
            nparameters + ncaptured);

    allocate_registers(&R);
    resolve_registers(&R, order);
    *pmax_reg = R.max_reg;

    pawP_pop_object(C, R.inactive);
    pawP_pop_object(C, R.active);
    pawP_pop_object(C, R.handled);

dump_result(R.result);
    return R.result;
}

