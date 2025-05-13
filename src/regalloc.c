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
#include "error.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include "ssa.h"
#include <stdlib.h>

#define REGALLOC_ERROR(R_, Kind_, ...) pawErr_##Kind_((R_)->C, (R_)->mir->modname, __VA_ARGS__)
#define PLACE(Reg_) ((struct MirPlace){.r = Reg_})

enum AllocationKind {
    ALLOCATE_NEXT,
    ALLOCATE_ANY,
};

struct RegisterAllocator {
    struct Pool *pool;
    struct MirIntervalList *intervals;
    struct MirLocationList *locations;
    struct MirInstructionList *code;
    struct Compiler *C;
    struct Mir *mir;
    paw_Env *P;

    // result: stores the mapping from virtual to physical registers
    // handled: intervals ending before "position"
    // unhandled: intervals starting after "position"
    // active: intervals assigned to registers
    // inactive: intervals with a hole at "position"
    struct IntervalMap *handled, *active, *inactive;
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

DEFINE_MAP(struct RegisterAllocator, IntervalMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, struct MirLiveInterval *, void *)
DEFINE_MAP_ITERATOR(IntervalMap, struct MirLiveInterval *, void *)

#define SET_ITER(S, iter, it, code)                                      \
    do {                                                                 \
        IntervalMapIterator iter;                                        \
        IntervalMapIterator_init(S, &iter);                              \
        while (IntervalMapIterator_is_valid(&iter)) {                    \
            struct MirLiveInterval *it = IntervalMapIterator_key(&iter); \
            code;                                                        \
        }                                                                \
    } while (0)

static paw_Bool set_contains(struct RegisterAllocator *R, IntervalMap *set, struct MirLiveInterval *it)
{
    return IntervalMap_get(R, set, it) != NULL;
}

static void set_add(struct RegisterAllocator *R, IntervalMap *set, struct MirLiveInterval *it)
{
    IntervalMap_insert(R, set, it, NULL);
}

static void iset_erase(IntervalMapIterator *iter)
{
    IntervalMapIterator_erase(iter);
}

static void iset_next(IntervalMapIterator *iter)
{
    IntervalMapIterator_next(iter);
}

#define REG(R, it) (RegisterTable_get((R)->result, (it)->r.value).value)
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
    return d < 10 ? 2 : d < 100 ? 1
                                : 0;
}

// TODO
#include <stdio.h>

static void dump_interval(struct MirLiveInterval *it, int start, int indent)
{
    printf("%*c", indent + decimal_padding(it->r.value), ' ');
    printf("_%d: ", it->r.value);

    paw_Bool first = PAW_TRUE;
    for (int i = start; i < it->ranges->count; ++i) {
        paw_Bool const b = pawP_bitset_get(it->ranges, i);
        if (i > start)
            putchar(' ');
        putchar(b ? '*' : '.');
    }
    printf("\n");
}

static void dump_set(IntervalMap *set, char const *name)
{
    printf("%s = [", name);

    int i = 0;
    SET_ITER(set, iter, it, {
        if (i > 0)
            printf(", ");
        printf("_%d", it->r.value);
        iset_next(&iter);
    });
    printf("]\n");
}

static void dump_result(struct RegisterTable *table)
{
    int i;
    struct RegisterInfo *pr;

    printf("result = [\n");
    K_LIST_ENUMERATE (table, i, pr) {
        printf("    _%d => ", i);
        if (pr->value < 0)
            putchar('-');
        else
            printf("r%d", pr->value);
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
    K_LIST_ENUMERATE (R->unhandled, i, iter) {
        struct MirLiveInterval *it = *iter;
        if (!MIR_REG_EXISTS(it->r))
            continue;
        if (it->last < R->position)
            continue;
        dump_interval(it, R->position, 4);
    }
    printf("}\n");

    dump_result(R->result);
}

static void debug_registers(struct RegisterAllocator *R, MirRegister *regs, int n)
{
    for (int i = 0; i < n; ++i) {
        MirRegister const r = regs[i];
        if (i > 0)
            fputc(' ', stderr);
        if (MIR_REG_EXISTS(r))
            fputc('*', stderr);
        else
            fputc('.', stderr);
    }
}

static void split_current(struct RegisterAllocator *R, int pos)
{
    paw_assert(pos > 0);
    struct MirLiveInterval *it = R->current;
    int const n = it->ranges->count;
    struct MirLiveInterval *split = pawMir_new_interval(R->C, it->r, n);
    split->instr = pawMir_new_instruction(R->mir);
    split->last = it->last;

    pawP_bitset_or(split->ranges, it->ranges);
    pawP_bitset_clear_range(it->ranges, 0, pos);
    pawP_bitset_clear_range(split->ranges, pos, n);
    *split->instr = *it->instr;
    split->instr->hdr.mid = pawMir_next_id(R->mir);

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
    int const new_pos = pawMir_get_location(R->locations, it->instr->hdr.mid) + 1;
    pawMir_set_location(R->mir, R->locations, split->instr->hdr.mid, new_pos);
    MirIntervalList_set(R->unhandled, new_pos, split);
}

static struct RegisterInfo max_free_reg(struct RegisterAllocator *R, int const *free_until_pos, int *pmax_value)
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
        paw_Bool const x = pawP_bitset_get(a->ranges, pos);
        paw_Bool const y = pawP_bitset_get(b->ranges, pos);
        if (x && y)
            return pos;
    }
    return -1;
}

static void not_enough_registers(struct RegisterAllocator *R)
{
    REGALLOC_ERROR(R, too_many_variables, R->mir->span.start, NREGISTERS);
}

static struct RegisterInfo get_result(struct RegisterAllocator *R, MirRegister r)
{
    return RegisterTable_get(R->result, r.value);
}

static void try_allocate_free_reg(struct RegisterAllocator *R)
{
    struct MirLiveInterval *current = R->current;
    struct RegisterInfo const result = get_result(R, current->r);
    if (result.value >= 0)
        return;

    struct MirRegisterData const *data = mir_reg_data(R->mir, current->r);
    if (data->is_captured) {
        struct RegisterInfo const result = get_result(R, data->hint);
        RegisterTable_set(R->result, current->r.value, result);
        return;
    }

    int free_until_pos[NREGISTERS];
    for (int i = 0; i < NREGISTERS; ++i) {
        free_until_pos[i] = R->max_position;
    }
    // ignore registers already containing variables
    SET_ITER(R->active, iter, it, {
        free_until_pos[REG(R, it)] = 0;
        iset_next(&iter);
    });

    // TODO: this loop can be skipped if "it" was not split off of another
    //       interval, per the Wimmer SSA paper
    // determine where lifetime holes end, i.e. the next position where the
    // variable is expected to be in a register
    SET_ITER(R->inactive, iter, it, {
        int const p = next_intersection(it, current);
        if (p >= 0)
            free_until_pos[REG(R, it)] = p;
        IntervalMapIterator_next(&iter);
    });

    int pos; // "reg" is free until this instruction
    struct RegisterInfo const reg = max_free_reg(R, free_until_pos, &pos);
    if (pos == 0)
        not_enough_registers(R); // no return
    R->max_reg = PAW_MAX(R->max_reg, reg.value);
    RegisterTable_set(R->result, current->r.value, reg);
    if (R->position >= pos)
        split_current(R, pos);
}

static void allocate_registers(struct RegisterAllocator *R)
{
    struct Mir *mir = R->mir;
    struct MirLiveInterval **pit;
    K_LIST_FOREACH (R->unhandled, pit) {
        R->current = *pit;
        R->position = R->current->first;
        if (!MIR_REG_EXISTS(R->current->r))
            continue;

        SET_ITER(R->active, iter, it, {
            if (it->last < R->position) {
                set_add(R, R->handled, it);
                iset_erase(&iter);
            } else if (!is_covered(it, R->position)) {
                set_add(R, R->inactive, it);
                iset_erase(&iter);
            } else {
                iset_next(&iter);
            }
        });

        SET_ITER(R->inactive, iter, it, {
            if (it->last < R->position) {
                set_add(R, R->handled, it);
                iset_erase(&iter);
            } else if (is_covered(it, R->position)) {
                set_add(R, R->active, it);
                iset_erase(&iter);
            } else {
                iset_next(&iter);
            }
        });

        // allocate a register for the current interval
        try_allocate_free_reg(R);
        set_add(R, R->active, R->current);
    }
}

struct Copy {
    MirRegister from;
    MirRegister to;
    int location;
};

DEFINE_LIST(struct RegisterAllocator, CopyList, struct Copy)

static MirRegister scratch_register(struct RegisterAllocator *R, MirRegister old)
{
    if (R->max_reg >= NREGISTERS)
        not_enough_registers(R);
    MirRegister const temp = MIR_REG(R->mir->registers->count);
    MirRegisterDataList_push(R->mir, R->mir->registers, *mir_reg_data(R->mir, old));
    RegisterTable_push(R->C, R->result, REGINFO(++R->max_reg));
    return temp;
}

static paw_Bool check_copy_conflict(struct RegisterAllocator *R, struct CopyList *copies, struct Copy copy, int skip)
{
    struct RegisterInfo const b = get_result(R, copy.to);

    int index;
    struct Copy *pcopy;
    K_LIST_ENUMERATE (copies, index, pcopy) {
        // NOTE: "skip" prevents NOOP copies ("x -> x") from being classified
        //       as a conflict
        struct RegisterInfo const a = get_result(R, pcopy->from);
        if (index != skip && REG_EQUALS(a, b))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void order_and_insert_copies(struct RegisterAllocator *R, struct MirBlockData *block, struct CopyList *pcopies)
{
    struct CopyList *seq = CopyList_new(R);
    int nscratch = 0;
    struct Copy *pcopy;
    int index;

    while (pcopies->count > 0) {
        K_LIST_ENUMERATE (pcopies, index, pcopy) {
            if (check_copy_conflict(R, pcopies, *pcopy, index)) {
                MirRegister const scratch = scratch_register(R, pcopy->to);
                CopyList_push(R, seq, ((struct Copy){pcopy->from, scratch}));
                pcopy->from = scratch;
                ++nscratch;
            } else {
                CopyList_push(R, seq, *pcopy);
                CopyList_swap_remove(pcopies, index);
            }
        }
    }

    // copies must be added before the terminator
    struct MirInstruction *terminator = MirInstructionList_last(block->instructions);
    MirInstructionList_pop(block->instructions);

    K_LIST_FOREACH (seq, pcopy) {
        struct MirInstruction *move = pawMir_new_move(R->mir, (struct SourceLoc){-1}, PLACE(pcopy->to), PLACE(pcopy->from));
        pawMir_set_location(R->mir, R->locations, move->hdr.mid, pcopy->location);
        MirInstructionList_push(R->mir, block->instructions, move);
    }
    MirInstructionList_push(R->mir, block->instructions, terminator);

    // temporary registers only used for this batch of moves
    R->max_reg -= nscratch;
    pcopies->count = 0;
}

static void resolve_registers(struct RegisterAllocator *R, struct MirBlockList *order)
{
    struct CopyList *resolved = CopyList_new(R);

    MirBlock const *b, *s;
    K_LIST_FOREACH (order, b) {
        struct MirInstruction **pinstr;
        struct MirBlockData *bb = mir_bb_data(R->mir, *b);
        K_LIST_FOREACH (bb->successors, s) {
            // for each control flow edge "b -> s", transform phi functions in "s"
            // into moves at the end of "b"
            struct MirBlockData *bs = mir_bb_data(R->mir, *s);
            K_LIST_FOREACH (bs->joins, pinstr) {
                struct MirPhi *phi = MirGetPhi(*pinstr);
                int const index = mir_which_pred(R->mir, *s, *b);
                MirRegister const opd = MirPlaceList_get(phi->inputs, index).r;

                int const location = pawMir_get_location(R->locations, mir_bb_last(bb));
                CopyList_push(R, resolved, ((struct Copy){
                                                  .location = location + 2,
                                                  //                                .location = mir_bb_last(bb),
                                                  .to = phi->output.r,
                                                  .from = opd,
                                              }));
            }
            order_and_insert_copies(R, bb, resolved);
        }
    }

    // phi nodes have been transformed into moves
    K_LIST_FOREACH (order, b) {
        struct MirBlockData *block = mir_bb_data(R->mir, *b);
        block->joins->count = 0;
    }
}

static int compare_first(void const *lhs, void const *rhs)
{
    struct MirLiveInterval const *a = *CAST(struct MirLiveInterval const **, lhs);
    struct MirLiveInterval const *b = *CAST(struct MirLiveInterval const **, rhs);
    if (a->first < b->first)
        return -1;
    if (a->first > b->first)
        return 1;
    return 0;
}

static struct MirIntervalList *expand_with_placeholders(struct RegisterAllocator *R, struct MirIntervalList *intervals, int nskip)
{
    int const npositions = intervals->count * 2;
    struct MirIntervalList *result = MirIntervalList_new_from(R->mir, R->pool);
    MirIntervalList_reserve(R->mir, result, npositions);

    int i;
    struct MirLiveInterval **pit;
    K_LIST_ENUMERATE (intervals, i, pit) {
        MirIntervalList_push(R->mir, result, *pit);

        // add empty interval to make splitting easier
        struct MirLiveInterval *empty = pawMir_new_interval(R->C,
                                                            MIR_INVALID_REG, npositions);
        MirIntervalList_push(R->mir, result, empty);
    }

#undef PUSH_PLACEHOLDER

    return result;
}

struct RegisterTable *pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirIntervalList *intervals, struct MirLocationList *locations, int *pmax_reg)
{
    struct MirBlockData *last = MirBlockDataList_last(mir->blocks);
    struct RegisterAllocator R = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .code = MirInstructionList_new(mir),
        .max_position = pawMir_get_location(locations, mir_bb_last(last)) + 2,
        .locations = locations,
        .intervals = intervals,
        .result = RegisterTable_new(C),
        .P = ENV(C),
        .mir = mir,
        .C = C,
    };

    R.handled = IntervalMap_new(&R);
    R.active = IntervalMap_new(&R);
    R.inactive = IntervalMap_new(&R);

    RegisterTable_reserve(C, R.result, mir->registers->count);
    for (int i = 0; i < mir->registers->count; ++i) {
        RegisterTable_push(C, R.result, REGINFO(-1));
    }

    int const nparameters = mir->param_size;
    int const ncaptured = mir->captured->count;

    // TODO: linked list threaded through set elements? build
    //       temp array to qsort, eventually implement mergesort
    //       to avoid temp array. avoids weird placeholder thing and is more flexible

    {
        int const npositions = R.max_position + 1;

        // assign registers for result and arguments
        int const offset = 1 + nparameters;
        for (int i = 0; i < offset; ++i) {
            struct MirLiveInterval *it = MirIntervalList_get(intervals, i);
            set_add(&R, R.active, it);
            RegisterTable_set(R.result, i, REGINFO(i));
            pawP_bitset_set_range(it->ranges, 0, npositions);
            it->last = npositions;
            it->first = 0;
        }

        // assign registers for captured variables
        for (int i = 0; i < ncaptured; ++i) {
            struct MirCaptureInfo c = MirCaptureList_get(mir->captured, i);
            if (c.r.value >= offset) {
                RegisterTable_set(R.result, c.r.value, REGINFO(offset + i));
            }
        }

        // extend live ranges for captured variables
        // TODO: This is an unfortunate consequence of having an IR in SSA form and also supporting upvalues.
        // TODO: Variables captured in a closure must stay in the same VM register until they are closed. As a
        // TODO: workaround, we just allocate all versions of the captured variable to the same VM register.
        // TODO: I think it would be better to prevent multiple versions from being created in the first place.
        int rid = offset;
        struct MirLiveInterval **pit;
        K_LIST_FOREACH (intervals, pit) {
            struct MirLiveInterval *it = *pit;
            struct MirRegisterData *data = mir_reg_data(mir, it->r);
            if (!data->is_captured)
                continue;
            if (it->r.value < offset)
                continue;
            if (!MIR_REG_EQUALS(it->r, data->hint)) {
                struct RegisterInfo result = get_result(&R, data->hint);
                RegisterTable_set(R.result, it->r.value, result);
            }
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

    pawP_pool_free(C, R.pool);
    return R.result;
}

