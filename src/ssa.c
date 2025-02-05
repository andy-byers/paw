// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include "ssa.h"

struct SsaConverter {
    struct Compiler *C;
    struct Mir *mir;
    struct MirRegisterDataList *registers;
    struct MirRegisterList *locals;
    struct MirBlockList *idom;
    struct MirBucketList *df;

    // data structures for phi node placement
    struct IntegerList *has;
    struct IntegerList *work;

    // data structures for variable renaming
    struct NameStackList *stacks;
    struct MirRegisterList *changes;

    Map *capture; // MirRegister => MirRegister
    Map *rename; // MirRegister => MirRegister
    Map *uses; // MirRegister => [MirBlockList]
    Map *defs; // MirRegister => [MirBlockList]
    int location;
    int N;
};

static void add_captured_reg(struct SsaConverter *S, MirRegister r1, MirRegister r2)
{
    paw_assert(!MAP_CONTAINS(S->capture, I2V(r1.value)));
    MAP_INSERT(S->C, S->capture, I2V(r1.value), I2V(r2.value));
}

static MirRegister captured_reg(struct SsaConverter *S, MirRegister r)
{
    const Value *pval = pawH_get(S->capture, I2V(r.value));
    return pval != NULL ? MIR_REG(CAST(int, pval->i)) : MIR_INVALID_REG;
}

static MirRegister last_reg_name(struct SsaConverter *S, MirRegister r)
{
    // NOTE: if a register is not in "S->rename", then all of its uses/defs are in
    //       unreachable basic blocks
    const Value *pval = pawH_get(S->rename, I2V(r.value));
    return pval != NULL ? MIR_REG(CAST(int, pval->i)) : MIR_INVALID_REG;
}

static struct MirPhi *add_phi_node(struct SsaConverter *S, MirBlock b, MirRegister r)
{
    struct MirBlockData *bb = mir_bb_data(S->mir, b);
    for (int i = 0; i < bb->joins->count; ++i) {
        struct MirPhi *phi = MirGetPhi(K_LIST_GET(bb->joins, i));
        if (MIR_REG_EQUALS(phi->output, r)) return phi; // already exists
    }
    struct IrType *type = mir_reg_data(S->mir, r)->type;
    struct MirInstruction *phi = pawMir_new_instruction(S->C, kMirPhi);
    struct MirRegisterList *inputs = pawMir_register_list_new(S->C);
    MirGetPhi(phi)->var_id = r.value;
    MirGetPhi(phi)->inputs = inputs;
    MirGetPhi(phi)->output = r;
    K_LIST_PUSH(S->C, bb->joins, phi);

    int ninputs = bb->predecessors->count;
    K_LIST_RESERVE(S->C, inputs, ninputs);
    while (ninputs-- > 0) K_LIST_PUSH(S->C, inputs, MIR_INVALID_REG);

    return MirGetPhi(phi);
}

DEFINE_LIST(struct Compiler, name_stack_list_, NameStackList, struct MirRegisterList *)
DEFINE_LIST(struct Compiler, integer_list_, IntegerList, int)

static void rename_input(struct SsaConverter *S, MirRegister *pr)
{
    struct MirRegisterList *names = K_LIST_GET(S->stacks, pr->value);
    if (names->count <= 0) {
        pawE_error(ENV(S->C), PAW_EVALUE, -1, "use before initialization");
    }
//    paw_assert(names->count > 0);
    *pr = K_LIST_LAST(names);
}

static void rename_output(struct SsaConverter *S, MirRegister *pr, paw_Bool is_alloc)
{
    const MirRegister old = *pr;
    struct MirRegisterList *names = K_LIST_GET(S->stacks, old.value);
    struct MirRegisterData *old_data = mir_reg_data(S->mir, old);
    K_LIST_PUSH(S->C, S->changes, old);

    const int reg_id = S->registers->count;
    struct MirRegisterData data = *old_data;
    // If the instruction that generated this register is an AllocLocal, and the
    // "MirRegisterData::is_uninit" flag is set, then the instruction refers to a local
    // variable declaration without an initializer. Propagate the flag so that fix_aux_info()
    // can check for "use before initialization" errors.
    data.is_uninit = is_alloc ? data.is_uninit : PAW_FALSE;
    K_LIST_PUSH(S->C, S->registers, data);
    *pr = MIR_REG(reg_id);

    K_LIST_PUSH(S->C, names, *pr);
    MAP_INSERT(S->C, S->rename, I2V(old.value), I2V(reg_id));

    if (old_data->is_captured) {
        struct MirRegisterData *reg = &K_LIST_GET(S->registers, pr->value);
        const MirRegister capture = captured_reg(S, old);
        if (!MIR_REG_EXISTS(capture)) {
            add_captured_reg(S, old, *pr);
            reg->hint = *pr;
        } else {
            reg->hint = capture;
        }
    }
}

static void rename_join(struct SsaConverter *S, struct MirInstruction *instr)
{
    struct MirPhi *x = MirGetPhi(instr);
    rename_output(S, &x->output, PAW_FALSE);
}

static int next_location(struct SsaConverter *S)
{
    return S->location++ * 2;
}

static void rename_instruction(struct SsaConverter *S, struct MirInstruction *instr)
{
    MirRegister **ppr;
    struct MirLoad load;
    if (pawMir_check_load(S->C, instr, &load)) {
        K_LIST_FOREACH(load.inputs, ppr) rename_input(S, *ppr);
    }

    struct MirStore store;
    if (pawMir_check_store(S->C, instr, &store)) {
        K_LIST_FOREACH(store.outputs, ppr) rename_output(S, *ppr, MirIsAllocLocal(instr));
    }
}

static paw_Bool list_includes_block(const struct MirBlockList *blocks, MirBlock b)
{
    const MirBlock *pb;
    K_LIST_FOREACH(blocks, pb) {
        if (MIR_BB_EQUALS(b, *pb)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

static struct MirBlockList *compute_live_in(struct SsaConverter *S, struct MirBlockList *defs, MirRegister r)
{
    struct MirBlockList *uses = pawH_get(S->uses, I2V(r.value))->p;
    return pawMir_compute_live_in(S->C, S->mir, uses, defs, r);
}

static void place_phi_nodes(struct SsaConverter *S)
{
    const int nblocks = S->mir->blocks->count;
    // "has" indicates the last time a phi function was inserted for each
    // node, and "work" indicates the last time each block was added to
    // the worklist "W". Values are compared with the iteration count to
    // determine if the aformentioned action occurred during the current
    // iteration (of the outer loop) or not.
    struct IntegerList *has = integer_list_new(S->C);
    struct IntegerList *work = integer_list_new(S->C);
    K_LIST_RESERVE(S->C, has, nblocks);
    K_LIST_RESERVE(S->C, work, nblocks);
    for (int i = 0; i < nblocks; ++i) {
        K_LIST_PUSH(S->C, has, 0);
        K_LIST_PUSH(S->C, work, 0);
    }

    int nstacks = 0;
    Map *definitions = S->defs;
    paw_Int iter = PAW_ITER_INIT;
    // "W" is the worklist of nodes to be processed
    struct MirBlockList *W = pawMir_block_list_new(S->C);
    for (int iterations = 1; pawH_iter(definitions, &iter); ++iterations) {
        const MirRegister r = {pawH_key(definitions, iter)->i};
        nstacks = PAW_MAX(nstacks, r.value + 1);
        // consider each assignment of the variable
        struct MirBlockList *defs = pawH_value(definitions, iter)->p;
        if (defs->count < 2) continue; // variable has single version

        const MirBlock *pb;
        K_LIST_FOREACH(defs, pb) {
            K_LIST_SET(work, pb->value, iterations);
            K_LIST_PUSH(S->C, W, *pb);
        }

        // use the live in set for "r" to avoid adding dead phi functions, i.e. phi functions
        // for variables that are not live in at the join node
        const struct MirBlockList *live_in = compute_live_in(S, defs, r);

        while (W->count > 0) {
            // variable "r" has a definition in basic block "x"
            const MirBlock x = K_LIST_LAST(W);
            K_LIST_POP(W);

            const struct MirBlockList *df = K_LIST_GET(S->df, x.value);

            const MirBlock *y;
            K_LIST_FOREACH(df, y) {
                // Each node "y" in the dominance frontier of "x" has a predecessor dominated
                // by "x" (possibly "x" itself) but are not themselves dominated by "x", meaning
                // there is a path to "y" that avoids "x". "y" requires a phi function to join
                // the multiple flows of control.
                if (K_LIST_GET(has, y->value) >= iterations) continue;
                if (!list_includes_block(live_in, *y)) continue;
                // place a trivial phi node "r = phi(NULL, .., NULL)" in basic block "y"
                struct MirPhi *phi = add_phi_node(S, *y, r);
                K_LIST_SET(has, y->value, iterations);
                // transitive step/relation to the iterated dominance frontier
                if (K_LIST_GET(work, y->value) < iterations) {
                    K_LIST_SET(work, y->value, iterations);
                    K_LIST_PUSH(S->C, W, *y);
                }
            }
        }
    }

    // allocate stacks for renaming pass
    while (S->stacks->count < nstacks) {
        struct MirRegisterList *names = pawMir_register_list_new(S->C);
        K_LIST_PUSH(S->C, S->stacks, names);
    }
}

static void rename_vars(struct SsaConverter *S, MirBlock x)
{
    struct MirBlockData *block = mir_bb_data(S->mir, x);
    const int first_change = S->changes->count;
    struct MirInstruction **instr;
    MirBlock *y;

    // location of phi nodes at the start of this block
    K_LIST_FOREACH(block->joins, instr) {
        struct MirPhi *phi = MirGetPhi(*instr);
        phi->location = block->location;
    }

    // fix references to the old name
    K_LIST_FOREACH(block->joins, instr) rename_join(S, *instr);
    K_LIST_FOREACH(block->instructions, instr) rename_instruction(S, *instr);

    // determine inputs to phi nodes
    K_LIST_FOREACH(block->successors, y) {
        const struct MirBlockData *data = mir_bb_data(S->mir, *y);
        K_LIST_FOREACH(data->joins, instr) {
            // for each phi node in each successor of the current basic block
            struct MirPhi *phi = MirGetPhi(*instr);
            const struct MirRegisterList *stack = K_LIST_GET(S->stacks, phi->var_id);
            if (stack->count > 0) {
                const int index = mir_which_pred(S->mir, *y, x);
                const MirRegister input = K_LIST_LAST(stack);
                K_LIST_SET(phi->inputs, index, input);
            }
        }
    }

    int b;
    // recur on nodes immediately dominated by the current node
    K_LIST_ENUMERATE(S->idom, b, y) {
        if (MIR_BB_EQUALS(x, *y)) rename_vars(S, MIR_BB(b));
    }

    // undo changes to the name stacks
    while (S->changes->count > first_change) {
        const MirRegister r = K_LIST_LAST(S->changes);
        struct MirRegisterList *names = K_LIST_GET(S->stacks, r.value);
        K_LIST_POP(S->changes);
        K_LIST_POP(names);
    }
}

// Ensure that the instruction does not use any variables before they are initialized
static void ensure_init(struct SsaConverter *S, struct MirInstruction *instr)
{
    struct MirLoad load;
    if (!pawMir_check_load(S->C, instr, &load)) return;

    MirRegister *const *ppr;
    K_LIST_FOREACH(load.inputs, ppr) {
        struct MirRegisterData *data = mir_reg_data(S->mir, **ppr);
        if (data->is_uninit) {
            pawE_error(ENV(S->C), PAW_EVALUE, -1, "use before initialization");
        }
    }
}

static void fix_aux_info(struct SsaConverter *S, struct Mir *mir)
{
    // write instruction numbers
    struct MirBlockData **pdata;
    K_LIST_FOREACH(mir->blocks, pdata) {
        struct MirBlockData *data = *pdata;
        struct MirInstruction **pinstr;
        data->location = next_location(S);
        K_LIST_FOREACH(data->joins, pinstr) {
            ensure_init(S, *pinstr);
        }
        K_LIST_FOREACH(data->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            instr->hdr.location = next_location(S);
            ensure_init(S, *pinstr);
        }
    }

    MirRegister *pr;
    struct MirCaptureInfo *pci;
    K_LIST_FOREACH(mir->locals, pr) *pr = last_reg_name(S, *pr);
    K_LIST_FOREACH(mir->captured, pci) pci->r = captured_reg(S, pci->r);
}

// TODO
#include <stdio.h>

static void debug(struct Compiler *C, struct MirBlockList *idom, struct MirBucketList *df)
{
    int i;
    MirBlock *b;
    printf("idom = [\n");
    K_LIST_ENUMERATE(idom, i, b) {
        printf("%d,\n", b->value);
    }
    printf("]\n");
    printf("df = [\n");
    struct MirBlockList **bl;
    K_LIST_ENUMERATE(df, i, bl) {
        printf("[");
        K_LIST_FOREACH(*bl, b) {
            printf("%d, ", b->value);
        }
        printf("]\n");
    }
    printf("]\n");
}

void pawSsa_construct(struct Compiler *C, struct Mir *mir, Map *uses, Map *defs)
{
    struct MirBlockList *idom = pawMir_compute_dominance_tree(C, mir);
    struct MirBucketList *df = pawMir_compute_dominance_frontiers(C, mir, idom);

    struct SsaConverter S = {
        .N = pawH_length(defs),
        .registers = pawMir_register_data_list_new(C),
        .changes = pawMir_register_list_new(C),
        .stacks = name_stack_list_new(C),
        .locals = mir->locals,
        .idom = idom,
        .uses = uses,
        .defs = defs,
        .mir = mir,
        .df = df,
        .C = C,
    };

    K_LIST_RESERVE(C, S.stacks, pawH_length(defs));
    S.capture = pawP_push_map(C);
    S.rename = pawP_push_map(C);

    place_phi_nodes(&S);
    rename_vars(&S, MIR_ROOT_BB);
    mir->registers = S.registers;
    fix_aux_info(&S, mir);
printf("%s\n", pawMir_dump(C, mir));--ENV(C)->top.p;

    pawP_pop_object(C, S.rename);
    pawP_pop_object(C, S.capture);
}

