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
// Activation frame layout:
//
//    name      | purpose
//   -----------|-------------------------------------
//    callee    | function object being called
//    arguments | arguments passed to function
//    upvalues  | upvalues captured in local closures
//    workspace | space for locals and temporaries
//    scratch   | extra per-instruction temporaries
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
#define REG(R_, Id_) (RegisterTable_get((R_)->result, (Id_).value).value)
#define REG_EQUALS(A_, B_) ((A_).value == (B_).value)
#define REGINFO(Value_) ((struct RegisterInfo){Value_})
#define REG_EXISTS(Reg_) (!REG_EQUALS(Reg_, REGINFO(-1)))

#define MAX_CONSTANTS 8

struct RegisterAllocator {
    struct Pool *pool;
    struct MirLocationList *locations;
    struct MirInstructionList *code;
    struct Compiler *C;
    struct Mir *mir;
    paw_Env *P;

    struct MirIntervalMap *intervals;

    // result: stores the mapping from virtual to physical registers
    // handled: intervals ending before "position"
    // active: intervals assigned to registers
    // inactive: intervals with a hole at "position"
    struct IntervalMap *handled, *active, *inactive;
    struct MirLiveInterval *current;
    struct RegisterTable *result;
    int max_position;
    int position;

    // largest register ID encountered
    int max_reg;
};

DEFINE_MAP(struct RegisterAllocator, IntervalMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, struct MirLiveInterval *, void *)
DEFINE_MAP_ITERATOR(IntervalMap, struct MirLiveInterval *, void *)

static void not_enough_registers(struct RegisterAllocator *R)
{
    REGALLOC_ERROR(R, too_many_variables, R->mir->span.start, NREGISTERS);
}

static MirRegister new_register(struct RegisterAllocator *R, IrType *type)
{
    if (R->max_reg >= NREGISTERS) not_enough_registers(R);
    MirRegister const r = MIR_REG(R->mir->registers->count);
    MirRegisterDataList_push(R->mir, R->mir->registers,
        (struct MirRegisterData){
            .type = type,
            .size = 1,
        });
    paw_assert(r.value == R->result->count);
    RegisterTable_push(R->C, R->result, REGINFO(-1));
    return r;
}

static paw_Bool materialize_k(struct RegisterAllocator *R, MirConstant k, struct MirPlace *pplace)
{
    struct MirConstantData const *kdata = mir_const_data(R->mir, k);
    IrType *ktype = pawP_builtin_type(R->C, kdata->kind);
    MirRegister const r = new_register(R, ktype);

    struct MirLiveInterval *it = pawMir_new_interval(R->C, r, R->max_position + 1);
    it->first = it->last = R->position;
    pawP_bitset_set(it->ranges, R->position);
    MirIntervalMap_insert(R->mir, R->intervals, r, it);

    *pplace = (struct MirPlace){
        .kind = MIR_PLACE_LOCAL,
        .r = r,
    };
    return PAW_TRUE;
}

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

static paw_Bool is_covered(struct MirLiveInterval *it, int pos)
{
    return pos < it->ranges->count && pawP_bitset_get(it->ranges, pos);
}

#if defined(PAW_DEBUG_EXTRA)

static int decimal_padding(int d)
{
    paw_assert(0 <= d && d < 256);
    return d < 10 ? 2 : d < 100 ? 1 : 0;
}

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

//    int i;
//    struct MirLiveInterval **iter;
//    printf("unhandled = {\n");
//    K_LIST_ENUMERATE (R->unhandled, i, iter) {
//        struct MirLiveInterval *it = *iter;
//        if (!MIR_ID_EXISTS(it->r))
//            continue;
//        if (it->last < R->position)
//            continue;
//        dump_interval(it, R->position, 4);
//    }
//    printf("}\n");

    dump_result(R->result);
}

static void debug_registers(struct RegisterAllocator *R, MirRegister *regs, int n)
{
    for (int i = 0; i < n; ++i) {
        MirRegister const r = regs[i];
        if (i > 0)
            fputc(' ', stderr);
        if (MIR_ID_EXISTS(r))
            fputc('*', stderr);
        else
            fputc('.', stderr);
    }
}

#endif // defined(PAW_DEBUG_EXTRA)

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

static struct RegisterInfo get_result(struct RegisterAllocator *R, MirRegister r)
{
    return RegisterTable_get(R->result, r.value);
}

static void try_allocate_free_reg(struct RegisterAllocator *R)
{
    struct MirLiveInterval *current = R->current;
    struct RegisterInfo const result = get_result(R, current->r);
    if (result.value >= 0) return;

    int free_until_pos[NREGISTERS];
    for (int i = 0; i < NREGISTERS; ++i) {
        free_until_pos[i] = R->max_position + 1;
    }
    // ignore registers containing live variables
    SET_ITER(R->active, iter, it, {
        free_until_pos[REG(R, it->r)] = 0;
        iset_next(&iter);
    });

    // TODO: this loop can be skipped if "it" was not split off of another
    //       interval, per the Wimmer SSA paper
    // determine where lifetime holes end, i.e. the next position where the
    // variable is expected to be in a register
    SET_ITER(R->inactive, iter, it, {
        int const p = next_intersection(it, current);
        if (p >= 0) free_until_pos[REG(R, it->r)] = p;
        iset_next(&iter);
    });

    int pos; // "reg" is free until this location
    struct RegisterInfo const reg = max_free_reg(R, free_until_pos, &pos);
    if (pos == 0 || is_covered(current, pos)) not_enough_registers(R);

    RegisterTable_set(R->result, current->r.value, reg);
    R->max_reg = PAW_MAX(R->max_reg, reg.value);
}

static struct MirLiveInterval *get_interval(struct RegisterAllocator *R, MirRegister r)
{
    struct MirLiveInterval *const *pit = MirIntervalMap_get(R->mir, R->intervals, r);
    paw_assert(pit != NULL && "unrecognized register");
    return *pit;
}

static void allocate_register(struct RegisterAllocator *R, MirRegister r)
{
    struct Mir *mir = R->mir;
    struct MirLiveInterval *it;

    R->current = get_interval(R, r);
    R->position = R->current->first;

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

static void materialize_load(struct RegisterAllocator *R, struct SourceLoc loc, struct MirPlace *pload, MirInstructionList *rewrite)
{
    paw_assert(pload->projection == NULL);
    if (pload->kind == MIR_PLACE_CONSTANT) {
        MirConstant const k = pload->k;
        struct MirConstantData const *kdata = mir_const_data(R->mir, k);
        if (materialize_k(R, k, pload)) {
            allocate_register(R, pload->r);
            // add an instruction that materializes the constant in a register
            struct MirInstruction *loadk = pawMir_new_load_constant(R->mir, loc, k, *pload);
            MirInstructionList_push(R->mir, rewrite, loadk);
        }
        pload->kind = MIR_PLACE_LOCAL;
    }
}

static void alloc_other_regs(struct RegisterAllocator *R, struct MirInstruction *instr, MirInstructionList *rewrite)
{
    {
        struct MirPlace *const *ppload;
        struct MirPlacePtrList *loads = pawMir_get_loads(R->mir, instr);
        K_LIST_FOREACH (loads, ppload)
            materialize_load(R, instr->hdr.loc, *ppload, rewrite);
    }

    {
        struct MirPlace *const *ppstore;
        struct MirPlacePtrList *stores = pawMir_get_stores(R->mir, instr);
        K_LIST_FOREACH (stores, ppstore) {
            struct MirPlace const store = **ppstore;
            allocate_register(R, store.r);
        }
    }

    MirInstructionList_push(R->mir, rewrite, instr);
}

static void allocate_registers(struct RegisterAllocator *R)
{
    struct MirBlockData *const *pbb;
    K_LIST_FOREACH (R->mir->blocks, pbb) {
        struct MirBlockData *bb = *pbb;

        {
            struct MirInstruction *const *pjoin;
            K_LIST_FOREACH (bb->joins, pjoin) {
                struct MirPhi const *phi = MirGetPhi(*pjoin);
                allocate_register(R, phi->output.r);
            }
        }

        {
            MirInstructionList *rewrite = MirInstructionList_new(R->mir);

            struct MirInstruction *const *pinstr;
            K_LIST_FOREACH (bb->instructions, pinstr) {
                alloc_other_regs(R, *pinstr, rewrite);
            }

            MirInstructionList_delete(R->mir, bb->instructions);
            bb->instructions = rewrite;
        }
    }
}

struct Copy {
    struct MirPlace from;
    MirRegister to;
    int location;
};

DEFINE_LIST(struct RegisterAllocator, CopyList, struct Copy)

static MirRegister scratch_register(struct RegisterAllocator *R, MirRegister old)
{
    if (R->max_reg >= NREGISTERS) not_enough_registers(R);
    MirRegister const temp = MIR_REG(R->mir->registers->count);
    MirRegisterDataList_push(R->mir, R->mir->registers, *mir_reg_data(R->mir, old));
    RegisterTable_push(R->C, R->result, REGINFO(++R->max_reg));
    return temp;
}

static paw_Bool check_copy_conflict(struct RegisterAllocator *R, struct CopyList *copies, struct Copy copy, int skip)
{
    paw_assert(copy.from.kind == MIR_PLACE_LOCAL);
    struct RegisterInfo const b = get_result(R, copy.to);

    int index;
    struct Copy *pcopy;
    K_LIST_ENUMERATE (copies, index, pcopy) {
        if (pcopy->from.kind == MIR_PLACE_LOCAL) {
            // NOTE: "skip" prevents NOOP copies ("x -> x") from being classified
            //       as a conflict
            struct RegisterInfo const a = get_result(R, pcopy->from.r);
            if (index != skip && REG_EQUALS(a, b)) return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static void order_and_insert_copies(struct RegisterAllocator *R, struct MirBlockData *block, struct CopyList *pcopies)
{
    struct CopyList *seq = CopyList_new(R);
    int nscratch = 0;
    struct Copy *pcopy;
    int index;

    // instructions must be added before the terminator
    struct MirInstruction *terminator = MirInstructionList_last(block->instructions);
    MirInstructionList_pop(block->instructions);

    while (pcopies->count > 0) {
        K_LIST_ENUMERATE (pcopies, index, pcopy) {
            if (pcopy->from.kind == MIR_PLACE_CONSTANT) {
                struct MirInstruction *load = pawMir_new_load_constant(R->mir,
                        (struct SourceLoc){-1}, pcopy->from.k, PLACE(pcopy->to));
                pawMir_set_location(R->mir, R->locations, load->hdr.mid, pcopy->location);
                MirInstructionList_push(R->mir, block->instructions, load);
                CopyList_swap_remove(pcopies, index);
            } else if (check_copy_conflict(R, pcopies, *pcopy, index)) {
                MirRegister const scratch = scratch_register(R, pcopy->to);
                CopyList_push(R, seq, (struct Copy){pcopy->from, scratch});
                pcopy->from.r = scratch;
                ++nscratch;
            } else {
                CopyList_push(R, seq, *pcopy);
                CopyList_swap_remove(pcopies, index);
            }
        }
    }

    K_LIST_FOREACH (seq, pcopy) {
        struct MirInstruction *move = pawMir_new_move(R->mir, (struct SourceLoc){-1}, PLACE(pcopy->to), PLACE(pcopy->from.r));
        pawMir_set_location(R->mir, R->locations, move->hdr.mid, pcopy->location);
        MirInstructionList_push(R->mir, block->instructions, move);
    }

    // replace the terminator
    MirInstructionList_push(R->mir, block->instructions, terminator);

    // temporary registers only used for this batch of moves
    R->max_reg -= nscratch; // TODO: probably not a good thing to do...
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
                struct MirPhi const *phi = MirGetPhi(*pinstr);
                int const index = mir_which_pred(R->mir, *s, *b);
                struct MirPlace const opd = MirPlaceList_get(phi->inputs, index);

                int const location = pawMir_get_location(R->locations, mir_bb_last(bb));
                CopyList_push(R, resolved, (struct Copy){
                    .location = location + 2,
                    .to = phi->output.r,
                    .from = opd,
                });
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

struct RegisterTable *pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirIntervalMap *intervals, struct MirLocationList *locations, int *pmax_reg)
{
    struct MirBlockData const *last = mir_bb_data(mir, K_LIST_LAST(order));
    int const max_position = pawMir_get_location(locations, mir_bb_last(last));
    struct RegisterAllocator R = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .code = MirInstructionList_new(mir),
        .max_position = max_position,
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

    {
        // assign registers for result and arguments
        int const offset = 1 + mir->param_size;
        for (int i = 0; i < offset; ++i) {
            RegisterTable_set(R.result, i, REGINFO(i));
            set_add(&R, R.active, get_interval(&R, MIR_REG(i)));
        }

        // assign registers for captured variables
        int index = 0;
        struct MirCaptureInfo const *pcapture;
        K_LIST_FOREACH (mir->captured, pcapture) {
            if (pcapture->r.value >= offset) { // not callee or argument
                RegisterTable_set(R.result, pcapture->r.value, REGINFO(offset + index++));
                set_add(&R, R.inactive, get_interval(&R, pcapture->r));
            }
        }
    }

    allocate_registers(&R);
    resolve_registers(&R, order);
    *pmax_reg = R.max_reg;

    pawP_pool_free(C, R.pool);
    return R.result;
}

