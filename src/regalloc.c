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
#include "regstack.h"
#include "ssa.h"
#include <stdlib.h>

#define REGALLOC_ERROR(R_, Kind_, ...) pawErr_##Kind_((R_)->C, (R_)->mir->modname, __VA_ARGS__)
#define PLACE(Reg_) ((struct MirPlace){.r = Reg_})
#define REG(R_, Id_) (RegisterTable_get((R_)->result, (Id_).value).value)
#define REG_EQUALS(A_, B_) ((A_).value == (B_).value)
#define REGINFO(Value_) ((struct RegisterInfo){Value_})
#define REG_EXISTS(Reg_) (!REG_EQUALS(Reg_, REGINFO(-1)))

// Ensure that there is always a scratch register
#define MAX_REGISTERS (NREGISTERS - 1)

struct RegisterAllocator {
    struct Pool *pool;
    struct MirLocationList *locations;
    struct Compiler *C;
    struct Mir *mir;
    paw_Env *P;

    RegstackMap *regstack;
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

    struct StackRegions *stacks;

    // largest register ID encountered
    int max_reg;
};

struct StackRegion {
    struct RegisterInfo base;
    int label;
};

DEFINE_MAP(struct RegisterAllocator, IntervalMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, struct MirLiveInterval *, void *)
DEFINE_MAP_ITERATOR(IntervalMap, struct MirLiveInterval *, void *)
DEFINE_MAP(struct RegisterAllocator, StackRegions, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirStack, struct RegisterInfo)

static void not_enough_registers(struct RegisterAllocator *R)
{
    REGALLOC_ERROR(R, too_many_variables, R->mir->span.start, NREGISTERS);
}

//static MirRegister new_register(struct RegisterAllocator *R, IrType *type, enum MirConstraintKind info)
//{
//    if (R->max_reg >= MAX_REGISTERS) not_enough_registers(R);
//    MirRegister const r = MIR_REG(R->mir->registers->count);
//    MirRegisterDataList_push(R->mir, R->mir->registers,
//        (struct MirRegisterData){
//            .type = type,
//            .constraint.kind = info,
//            .size = 1,
//        });
//    paw_assert(r.value == R->result->count);
//    RegisterTable_push(R->C, R->result, REGINFO(-1));
//    return r;
//}
//
//static paw_Bool materialize_k(struct RegisterAllocator *R, MirConstant k, int position, enum MirConstraintKind info, struct MirPlace *pplace)
//{
//    struct MirConstantData const *kdata = mir_const_data(R->mir, k);
//    IrType *ktype = pawP_builtin_type(R->C, kdata->kind);
//    MirRegister const r = new_register(R, ktype, info);
//
//    struct MirLiveInterval *it = pawMir_new_interval(R->C, r, R->max_position + 1);
//    pawP_bitset_set(it->ranges, position);
//    it->first = it->last = position;
//    MirIntervalMap_insert(R->mir, R->intervals, r, it);
//
//    *pplace = (struct MirPlace){
//        .kind = MIR_PLACE_LOCAL,
//        .r = r,
//    };
//    return PAW_TRUE;
//}

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

// Determine the first position at which live intervals "a" and "b" intersect
static int next_intersection(struct MirLiveInterval *a, struct MirLiveInterval *b)
{
    paw_assert(a->ranges->count == b->ranges->count);
    for (int pos = 0; pos < a->ranges->count; ++pos) {
        paw_Bool const x = pawP_bitset_get(a->ranges, pos);
        paw_Bool const y = pawP_bitset_get(b->ranges, pos);
        if (x && y) return pos;
    }
    return -1;
}

static void compute_free_until_pos(struct RegisterAllocator *R, int *free_until_pos)
{
    for (int i = 0; i < MAX_REGISTERS; ++i)
        free_until_pos[i] = R->max_position + 1;

    // ignore registers containing live variables
    SET_ITER(R->active, iter, it, {
        free_until_pos[REG(R, it->r)] = 0;
        iset_next(&iter);
    });

    if (R->current != NULL) {
        // TODO: this loop can be skipped if "it" was not split off of another
        //       interval, per the Wimmer SSA paper
        // determine where lifetime holes end, i.e. the next position where the
        // variable is expected to be in a register
        SET_ITER(R->inactive, iter, it, {
            int const p = next_intersection(it, R->current);
            if (p >= 0) free_until_pos[REG(R, it->r)] = p;
            iset_next(&iter);
        });
    }

    // TODO: This code causes registers containing captured registers to be ignored,
    // TODO: which is wasteful. It should be possible to use such a register before
    // TODO: and/or after it is needed for the captured variable
    for (int i = 0; i < R->mir->captured->count; ++i)
        free_until_pos[1 + R->mir->param_size + i] = 0;
}

static struct RegisterInfo compute_free_reg(struct RegisterAllocator *R, int const *free_until_pos)
{
    int i = MAX_REGISTERS;
    for (; i > 0 && free_until_pos[i - 1] > 0; --i);
    if (i == MAX_REGISTERS) not_enough_registers(R);
    return REGINFO(i);
}

static struct RegisterInfo get_stack_base(struct RegisterAllocator *R, struct MirConstraint con)
{
    paw_assert(con.kind == MIR_CONSTRAINT_STACK);
    struct RegisterInfo const *pinfo = StackRegions_get(R, R->stacks, con.stack.id);
    if (pinfo != NULL) return *pinfo;

    paw_assert(con.stack.index == 0);
    int free_until_pos[MAX_REGISTERS];
    compute_free_until_pos(R, free_until_pos);
    struct RegisterInfo const base = compute_free_reg(R, free_until_pos);
    StackRegions_insert(R, R->stacks, con.stack.id, base);
    return base;
}

static struct MirLiveInterval *get_interval(struct RegisterAllocator *R, MirRegister r)
{
    struct MirLiveInterval *const *pit = MirIntervalMap_get(R->mir, R->intervals, r);
    paw_assert(pit != NULL && "unrecognized register");
    return *pit;
}

static void set_result(struct RegisterAllocator *R, MirRegister r, struct RegisterInfo result)
{
    struct MirConstraint const con = mir_reg_data(R->mir, r)->con;
    if (con.kind == MIR_CONSTRAINT_STACK && con.stack.index == 0)
        StackRegions_insert(R, R->stacks, con.stack.id, result);

    while (r.value >= R->result->count)
        RegisterTable_push(R->C, R->result, REGINFO(-1));
    RegisterTable_set(R->result, r.value, result);
    R->max_reg = PAW_MAX(R->max_reg, result.value);

    set_add(R, R->active, get_interval(R, r));
}

static struct RegisterInfo get_result(struct RegisterAllocator *R, MirRegister r)
{
    return RegisterTable_get(R->result, r.value);
}

static void push_stack_region(struct RegisterAllocator *R, MirStack id)
{
    int free_until_pos[MAX_REGISTERS];
    compute_free_until_pos(R, free_until_pos);

    struct RegisterInfo const base = compute_free_reg(R, free_until_pos);
    StackRegions_insert(R, R->stacks, id, base);
}

static void pop_stack_region(struct RegisterAllocator *R, MirStack id)
{
    StackRegions_remove(R, R->stacks, id);
}

static void pop_stack_containing(struct RegisterAllocator *R, struct MirPlace base)
{
    paw_assert(base.kind == MIR_PLACE_LOCAL);
    pop_stack_region(R, mir_reg_data(R->mir, base.r)->con.stack.id);
}

static struct RegisterInfo next_stack_reg(struct RegisterAllocator *R)
{
    int free_until_pos[MAX_REGISTERS];
    compute_free_until_pos(R, free_until_pos);
    return compute_free_reg(R, free_until_pos);
}

static struct RegisterInfo max_free_reg(struct RegisterAllocator *R, int const *free_until_pos, int *pmax_value)
{
    int max_index = 0;
    *pmax_value = free_until_pos[0];
    for (int i = 1; i < MAX_REGISTERS; ++i) {
        if (*pmax_value < free_until_pos[i]) {
            *pmax_value = free_until_pos[i];
            max_index = i;
        }
    }
    return REGINFO(max_index);
}

// Attempt to allocate a VM register for a stack-constrained variable
static void try_allocate_stack_slot(struct RegisterAllocator *R, MirRegister r, struct MirConstraint con, int const *free_until_pos)
{
    struct RegisterInfo base = get_stack_base(R, con);
    if (con.stack.index >= MAX_REGISTERS - base.value)
        not_enough_registers(R);
    set_result(R, r, REGINFO(base.value + con.stack.index));
}

static void try_allocate_free_reg(struct RegisterAllocator *R)
{
    struct MirLiveInterval *current = R->current;
    struct RegisterInfo const result = get_result(R, current->r);
    if (result.value >= 0) return;

    int free_until_pos[MAX_REGISTERS];
    compute_free_until_pos(R, free_until_pos);

    struct MirRegisterData const *rdata = mir_reg_data(R->mir, current->r);
    if (rdata->con.kind == MIR_CONSTRAINT_STACK) {
        try_allocate_stack_slot(R, current->r, rdata->con, free_until_pos);
        return;
    }

    int pos; // "reg" is free until this location
    struct RegisterInfo const reg = max_free_reg(R, free_until_pos, &pos);
    if (pos == 0 || is_covered(current, pos)) not_enough_registers(R);

    set_result(R, current->r, reg);
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

static MirRegister new_register(struct RegisterAllocator *R, IrType *type)
{
    int const id = R->mir->registers->count;
    MirRegisterDataList_push(R->mir, R->mir->registers,
        (struct MirRegisterData){
            .type = type,
            .size = 1,
        });
    return  MIR_REG(id);
}

static struct MirInstruction *new_move(struct RegisterAllocator *R, struct SourceLoc loc, MirRegister from, MirRegister to)
{
    return pawMir_new_move(R->mir, loc, PLACE(to), PLACE(from));
}

static void alloc_concat(struct RegisterAllocator *R, struct MirConcat *x)
{
    struct RegisterInfo result = get_result(R, K_LIST_FIRST(x->inputs).r);
    set_result(R, x->output.r, result);
    pop_stack_region(R, x->stack);
}

// Allocate registers for a function call
static void alloc_call(struct RegisterAllocator *R, struct MirCall *x)
{
    // callee and argument(s) are replaced by return value(s)
    struct RegisterInfo result = get_result(R, x->target.r);

    struct MirPlace *presult;
    K_LIST_FOREACH (x->outputs, presult) {
        set_result(R, presult->r, result);
        ++result.value;
    }

    pop_stack_region(R, x->stack);
}

static void alloc_unboxed_aggregate_fields(struct RegisterAllocator *R, struct MirAggregate *x)
{
    struct RegisterInfo const base = get_result(R, K_LIST_FIRST(x->fields).r);

    int index;
    struct MirPlace const *poutput;
    K_LIST_ENUMERATE (x->outputs, index, poutput)
        set_result(R, poutput->r, REGINFO(base.value + index));
}

static void alloc_boxed_aggregate_fields(struct RegisterAllocator *R, struct MirAggregate *x)
{
    struct MirPlace const output = K_LIST_FIRST(x->outputs);
    if (x->fields->count == 0) {
        allocate_register(R, output.r);
    } else {
        struct MirPlace const first = K_LIST_FIRST(x->fields);
        struct RegisterInfo const result =  get_result(R, first.r);
        set_result(R, output.r, result);
    }
}

static void alloc_aggregate(struct RegisterAllocator *R, struct MirAggregate *x)
{
    if (x->is_boxed) {
        alloc_boxed_aggregate_fields(R, x);
    } else {
        alloc_unboxed_aggregate_fields(R, x);
    }

    pop_stack_region(R, x->stack);
}

static void alloc_unpack(struct RegisterAllocator *R, struct MirUnpack *x)
{
    // pointer is replaced by pointed-to value(s)
    struct RegisterInfo result = get_result(R, x->object.r);

    struct MirPlace *presult;
    K_LIST_FOREACH (x->outputs, presult) {
        set_result(R, presult->r, result);
        ++result.value;
    }

    pop_stack_region(R, x->stack);
}

static void alloc_container(struct RegisterAllocator *R, struct MirContainer *x)
{
    struct RegisterInfo result = x->elems->count > 0
        ? get_result(R, K_LIST_FIRST(x->elems).r)
        : next_stack_reg(R);

    set_result(R, x->output.r, result);
    pop_stack_region(R, x->stack);
}

static void alloc_closure(struct RegisterAllocator *R, struct MirClosure *x)
{
    set_result(R, x->output.r, next_stack_reg(R));
}

static void alloc_other(struct RegisterAllocator *R, struct MirInstruction *instr)
{
    int const position = pawMir_get_location(R->locations, instr->hdr.mid);

    {
        struct MirPlace *const *ppstore;
        struct MirPlacePtrList *stores = pawMir_get_stores(R->mir, instr);
        K_LIST_FOREACH (stores, ppstore) {
            struct MirPlace const store = **ppstore;
            allocate_register(R, store.r);
        }
    }
}

static void allocate_instruction(struct RegisterAllocator *R, struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirConcat:
            alloc_concat(R, MirGetConcat(instr));
            break;
        case kMirCall:
            alloc_call(R, MirGetCall(instr));
            break;
        case kMirAggregate:
            alloc_aggregate(R, MirGetAggregate(instr));
            break;
        case kMirUnpack:
            alloc_unpack(R, MirGetUnpack(instr));
            break;
        case kMirContainer:
            alloc_container(R, MirGetContainer(instr));
            break;
        case kMirClosure:
            alloc_closure(R, MirGetClosure(instr));
            break;

#define BASIC_STACK_INSTR(Kind_) \
        case kMir##Kind_: { \
        alloc_other(R, instr); \
        pop_stack_region(R, MirGet##Kind_(instr)->stack); \
        break; \
    }

        BASIC_STACK_INSTR(Return)
        BASIC_STACK_INSTR(GetElement)
        BASIC_STACK_INSTR(GetElementPtr)
        BASIC_STACK_INSTR(SetElement)
        BASIC_STACK_INSTR(SetElementV2)
        BASIC_STACK_INSTR(GetRange)
        BASIC_STACK_INSTR(SetRange)

#undef BASIC_STACK_INSTR

        default:
            alloc_other(R, instr);
    }
}

static void allocate_registers(struct RegisterAllocator *R)
{
    int index;
    struct MirBlockData *const *pbb;
    K_LIST_ENUMERATE (R->mir->blocks, index, pbb) {
        struct MirBlockData *bb = *pbb;

        {
            struct MirInstruction *const *pjoin;
            K_LIST_FOREACH (bb->joins, pjoin) {
                struct MirPhi const *phi = MirGetPhi(*pjoin);
                allocate_register(R, phi->output.r);
            }
        }

        {
            struct MirInstruction *const *pinstr;
            K_LIST_FOREACH (bb->instructions, pinstr)
                allocate_instruction(R, *pinstr);
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
    if (R->max_reg >= MAX_REGISTERS) not_enough_registers(R);
    MirRegister const temp = MIR_REG(R->mir->registers->count);
    MirRegisterDataList_push(R->mir, R->mir->registers, *mir_reg_data(R->mir, old));
    // TODO: don't increase ".max_reg" each time this is called
    RegisterTable_push(R->C, R->result, REGINFO(++R->max_reg));
    return temp;
}

static paw_Bool check_copy_conflict(struct RegisterAllocator *R, struct CopyList *copies, MirRegister to, int skip)
{
    struct RegisterInfo const b = get_result(R, to);

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
            if (check_copy_conflict(R, pcopies, pcopy->to, index)) {
                MirRegister const scratch = scratch_register(R, pcopy->to);
                CopyList_push(R, seq, (struct Copy){pcopy->from, scratch});
                pcopy->from = (struct MirPlace){
                    .kind = MIR_PLACE_LOCAL,
                    .r = scratch,
                };
                ++nscratch;
            } else {
                CopyList_push(R, seq, *pcopy);
                CopyList_swap_remove(pcopies, index);
            }
        }
    }

    K_LIST_FOREACH (seq, pcopy) {
        if (pcopy->from.kind == MIR_PLACE_LOCAL) {
            struct MirInstruction *move = pawMir_new_move(R->mir, (struct SourceLoc){-1}, PLACE(pcopy->to), PLACE(pcopy->from.r));
            pawMir_set_location(R->mir, R->locations, move->hdr.mid, pcopy->location);
            MirInstructionList_push(R->mir, block->instructions, move);
        } else {
            paw_assert(pcopy->from.kind == MIR_PLACE_CONSTANT);
            struct MirInstruction *load = pawMir_new_load_constant(R->mir,
                    (struct SourceLoc){-1}, pcopy->from.k, PLACE(pcopy->to));
            pawMir_set_location(R->mir, R->locations, load->hdr.mid, pcopy->location);
            MirInstructionList_push(R->mir, block->instructions, load);
        }
    }

    // replace the terminator
    MirInstructionList_push(R->mir, block->instructions, terminator);
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
            resolved->count = 0; // reuse list
        }
    }

    // phi nodes have been transformed into moves
    K_LIST_FOREACH (order, b) {
        struct MirBlockData *block = mir_bb_data(R->mir, *b);
        block->joins->count = 0;
    }
}

struct RegisterTable *pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, MirIntervalMap *intervals, RegstackMap *regstack, MirLocationList *locations, int *pmax_reg)
{
    struct Pool *pool = pawP_pool_new(C, C->aux_stats);

    struct MirBlockData const *last = mir_bb_data(mir, K_LIST_LAST(order));
    int const max_position = pawMir_get_location(locations, mir_bb_last(last));
    struct RegisterAllocator R = {
        .max_position = max_position,
        .locations = locations,
        .intervals = intervals,
        .result = RegisterTable_new(C),
        .regstack = regstack,
        .pool = pool,
        .P = ENV(C),
        .mir = mir,
        .C = C,
    };

    R.stacks = StackRegions_new(&R);
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
            // TODO: add to inactive instead
            set_add(&R, R.active, get_interval(&R, MIR_REG(i)));
            set_result(&R, MIR_REG(i), REGINFO(i));
        }

        // assign registers for captured variables
        int index = 0;
        struct MirCaptureInfo const *pcapture;
        K_LIST_FOREACH (mir->captured, pcapture) {
            if (pcapture->r.value >= offset) { // not callee or argument
                set_add(&R, R.inactive, get_interval(&R, pcapture->r));
                set_result(&R, pcapture->r, REGINFO(offset + index++));
            }
        }
    }

    allocate_registers(&R);
    resolve_registers(&R, order);
    *pmax_reg = R.max_reg;

    pawP_pool_free(C, R.pool);
    return R.result;
}

