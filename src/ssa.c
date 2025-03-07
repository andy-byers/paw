// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ssa.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"

struct SsaConverter {
    struct Compiler *C;
    struct Mir *mir;
    struct Pool *pool;
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

    struct RegisterMap *capture; // MirRegister => MirRegister
    struct RegisterMap *rename; // MirRegister => MirRegister
    UseDefMap *uses; // MirRegister => [MirBlockList]
    UseDefMap *defs; // MirRegister => [MirBlockList]
    paw_Env *P;
};

DEFINE_MAP(struct SsaConverter, RegisterMap, pawP_alloc, MIR_ID_HASH, MIR_ID_EQUALS, MirRegister, MirRegister)

static void add_captured_reg(struct SsaConverter *S, MirRegister r1, MirRegister r2)
{
    paw_assert(RegisterMap_get(S, S->capture, r1) == NULL);
    RegisterMap_insert(S, S->capture, r1, r2);
}

static MirRegister captured_reg(struct SsaConverter *S, MirRegister r)
{
    MirRegister const *pr = RegisterMap_get(S, S->capture, r);
    return pr != NULL ? *pr : MIR_INVALID_REG;
}

static MirRegister last_reg_name(struct SsaConverter *S, MirRegister r)
{
    // NOTE: if a register is not in "S->rename", then all of its uses/defs are in
    //       unreachable basic blocks
    MirRegister const *pr = RegisterMap_get(S, S->rename, r);
    return pr != NULL ? *pr : MIR_INVALID_REG;
}

static struct MirPhi *add_phi_node(struct SsaConverter *S, MirBlock b, MirRegister r)
{
    struct MirBlockData *bb = mir_bb_data(S->mir, b);
    for (int i = 0; i < bb->joins->count; ++i) {
        struct MirPhi *phi = MirGetPhi(MirInstructionList_get(bb->joins, i));
        if (MIR_REG_EQUALS(phi->output, r))
            return phi; // already exists
    }
    struct IrType *type = mir_reg_data(S->mir, r)->type;
    struct MirRegisterList *inputs = MirRegisterList_new(S->mir);
    struct MirInstruction *phi = pawMir_new_phi(S->mir, -1, inputs, r, r.value);
    MirInstructionList_push(S->mir, bb->joins, phi);

    int ninputs = bb->predecessors->count;
    MirRegisterList_reserve(S->mir, inputs, ninputs);
    while (ninputs-- > 0)
        MirRegisterList_push(S->mir, inputs, MIR_INVALID_REG);

    return MirGetPhi(phi);
}

DEFINE_LIST(struct SsaConverter, NameStackList, struct MirRegisterList *)
DEFINE_LIST(struct SsaConverter, IntegerList, int)

static void rename_input(struct SsaConverter *S, MirRegister *pr)
{
    struct MirRegisterList *names = NameStackList_get(S->stacks, pr->value);
    *pr = K_LIST_LAST(names);
}

static void rename_output(struct SsaConverter *S, MirRegister *pr, paw_Bool is_alloc)
{
    MirRegister const old = *pr;
    struct MirRegisterList *names = NameStackList_get(S->stacks, old.value);
    struct MirRegisterData *old_data = mir_reg_data(S->mir, old);
    MirRegisterList_push(S->mir, S->changes, old);

    int const reg_id = S->registers->count;
    struct MirRegisterData data = *old_data;
    // If the instruction that generated this register is an AllocLocal, and the
    // "MirRegisterData::is_uninit" flag is set, then the instruction refers to a local
    // variable declaration without an initializer. Propagate the flag so that fix_aux_info()
    // can check for "use before initialization" errors.
    data.is_uninit = is_alloc ? data.is_uninit : PAW_FALSE;
    MirRegisterDataList_push(S->mir, S->registers, data);
    *pr = MIR_REG(reg_id);

    MirRegisterList_push(S->mir, names, *pr);
    RegisterMap_insert(S, S->rename, old, MIR_REG(reg_id));

    if (old_data->is_captured) {
        struct MirRegisterData *reg = &K_LIST_AT(S->registers, pr->value);
        MirRegister const capture = captured_reg(S, old);
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

static void rename_instruction(struct SsaConverter *S, struct MirInstruction *instr)
{
    MirRegister **ppr;
    struct MirRegisterPtrList *loads = pawMir_get_loads(S->mir, instr);
    K_LIST_FOREACH (loads, ppr)
        rename_input(S, *ppr);

    MirRegister *pstore = pawMir_get_store(S->mir, instr);
    if (pstore != NULL)
        rename_output(S, pstore, MirIsAllocLocal(instr));
}

static paw_Bool list_includes_block(struct MirBlockList const *blocks, MirBlock b)
{
    MirBlock const *pb;
    K_LIST_FOREACH (blocks, pb) {
        if (MIR_BB_EQUALS(b, *pb))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

static struct MirBlockList *compute_live_in(struct SsaConverter *S, struct MirBlockList *defs, MirRegister r)
{
    struct MirBlockList *uses = *UseDefMap_get(S->mir, S->uses, r);
    return pawMir_compute_live_in(S->mir, uses, defs, r);
}

static void place_phi_nodes(struct SsaConverter *S)
{
    int const nblocks = S->mir->blocks->count;
    // "has" indicates the last time a phi function was inserted for each
    // node, and "work" indicates the last time each block was added to
    // the worklist "W". Values are compared with the iteration count to
    // determine if the aformentioned action occurred during the current
    // iteration (of the outer loop) or not.
    struct IntegerList *has = IntegerList_new(S);
    struct IntegerList *work = IntegerList_new(S);
    IntegerList_reserve(S, has, nblocks);
    IntegerList_reserve(S, work, nblocks);
    for (int i = 0; i < nblocks; ++i) {
        IntegerList_push(S, has, 0);
        IntegerList_push(S, work, 0);
    }

    int nstacks = 0;
    UseDefMap *definitions = S->defs;
    UseDefMapIterator iter;
    UseDefMapIterator_init(S->defs, &iter);
    // "W" is the worklist of nodes to be processed
    struct MirBlockList *W = MirBlockList_new(S->mir);
    for (int iterations = 1; UseDefMapIterator_is_valid(&iter);
         ++iterations, UseDefMapIterator_next(&iter)) {
        MirRegister const r = UseDefMapIterator_key(&iter);
        nstacks = PAW_MAX(nstacks, r.value + 1);
        // consider each assignment of the variable
        struct MirBlockList *defs = *UseDefMapIterator_valuep(&iter);
        if (defs->count < 2)
            continue; // variable has single version

        MirBlock const *pb;
        K_LIST_FOREACH (defs, pb) {
            IntegerList_set(work, pb->value, iterations);
            MirBlockList_push(S->mir, W, *pb);
        }

        // use the live in set for "r" to avoid adding dead phi functions, i.e. phi functions
        // for variables that are not live in at the join node
        struct MirBlockList const *live_in = compute_live_in(S, defs, r);

        while (W->count > 0) {
            // variable "r" has a definition in basic block "x"
            MirBlock const x = K_LIST_LAST(W);
            MirBlockList_pop(W);

            struct MirBlockList const *df = MirBucketList_get(S->df, x.value);

            MirBlock const *y;
            K_LIST_FOREACH (df, y) {
                // Each node "y" in the dominance frontier of "x" has a predecessor dominated
                // by "x" (possibly "x" itself) but are not themselves dominated by "x", meaning
                // there is a path to "y" that avoids "x". "y" requires a phi function to join
                // the multiple flows of control.
                if (IntegerList_get(has, y->value) >= iterations)
                    continue;
                if (!list_includes_block(live_in, *y))
                    continue;
                // place a trivial phi node "r = phi(NULL, .., NULL)" in basic block "y"
                struct MirPhi *phi = add_phi_node(S, *y, r);
                IntegerList_set(has, y->value, iterations);
                // transitive step/relation to the iterated dominance frontier
                if (IntegerList_get(work, y->value) < iterations) {
                    IntegerList_set(work, y->value, iterations);
                    MirBlockList_push(S->mir, W, *y);
                }
            }
        }
    }

    // allocate stacks for renaming pass
    while (S->stacks->count < nstacks) {
        struct MirRegisterList *names = MirRegisterList_new(S->mir);
        NameStackList_push(S, S->stacks, names);
    }
}

static void rename_vars(struct SsaConverter *S, MirBlock x)
{
    struct MirBlockData *block = mir_bb_data(S->mir, x);
    int const first_change = S->changes->count;
    struct MirInstruction **instr;
    MirBlock *y;

    // fix references to the old name
    K_LIST_FOREACH (block->joins, instr) {
        rename_join(S, *instr);
    }
    K_LIST_FOREACH (block->instructions, instr) {
        rename_instruction(S, *instr);
    }

    // determine inputs to phi nodes
    K_LIST_FOREACH (block->successors, y) {
        struct MirBlockData const *data = mir_bb_data(S->mir, *y);
        K_LIST_FOREACH (data->joins, instr) {
            // for each phi node in each successor of the current basic block
            struct MirPhi *phi = MirGetPhi(*instr);
            struct MirRegisterList const *stack = NameStackList_get(S->stacks, phi->var_id);
            if (stack->count > 0) {
                int const index = mir_which_pred(S->mir, *y, x);
                MirRegister const input = K_LIST_LAST(stack);
                MirRegisterList_set(phi->inputs, index, input);
            }
        }
    }

    int b;
    // recur on nodes immediately dominated by the current node
    K_LIST_ENUMERATE (S->idom, b, y) {
        if (MIR_BB_EQUALS(x, *y))
            rename_vars(S, MIR_BB(b));
    }

    // undo changes to the name stacks
    while (S->changes->count > first_change) {
        MirRegister const r = K_LIST_LAST(S->changes);
        struct MirRegisterList *names = NameStackList_get(S->stacks, r.value);
        MirRegisterList_pop(S->changes);
        MirRegisterList_pop(names);
    }
}

// Ensure that the instruction does not use any variables before they are initialized
static void ensure_init(struct SsaConverter *S, struct MirInstruction *instr)
{
    struct MirRegisterPtrList *ploads = pawMir_get_loads(S->mir, instr);

    MirRegister *const *ppr;
    K_LIST_FOREACH (ploads, ppr) {
        struct MirRegisterData *data = mir_reg_data(S->mir, **ppr);
        if (data->is_uninit)
            VALUE_ERROR(S, -1, "use before initialization");
    }
}

static void fix_aux_info(struct SsaConverter *S, struct Mir *mir)
{
    // check for use before initialization
    struct MirBlockData **pdata;
    K_LIST_FOREACH (mir->blocks, pdata) {
        struct MirBlockData *data = *pdata;
        struct MirInstruction **pinstr;
        K_LIST_FOREACH (data->joins, pinstr) {
            ensure_init(S, *pinstr);
        }
        K_LIST_FOREACH (data->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            ensure_init(S, *pinstr);
        }
    }

    // TODO: Consider not storing ".locals" in the MIR, since each local is spread
    //       across multiple versions after this pass, and ".locals" is only capable
    //       of storing a single version. ".locals" is not used anymore IIRC, but
    //       need to grep to make sure. ".captured" is fine. See comment below for why.
    MirRegister *pr;
    struct MirCaptureInfo *pci;
    // Rename local and captured variable definitions. Note that ".locals" stores the
    // register containing the first version of each local, and captured variables
    // only have a single logical version (additional versions are placed in the same
    // register as the first version, since closures expect their captures to remain
    // stationary until they are closed.
    K_LIST_FOREACH (mir->locals, pr)
        *pr = last_reg_name(S, *pr);
    K_LIST_FOREACH (mir->captured, pci)
        pci->r = captured_reg(S, pci->r);
}

// TODO
#include <stdio.h>

static void debug(struct Compiler *C, struct MirBlockList *idom, struct MirBucketList *df)
{
    int i;
    MirBlock *b;
    printf("idom = [\n");
    K_LIST_ENUMERATE (idom, i, b) {
        printf("%d,\n", b->value);
    }
    printf("]\n");
    printf("df = [\n");
    struct MirBlockList **bl;
    K_LIST_ENUMERATE (df, i, bl) {
        printf("[");
        K_LIST_FOREACH (*bl, b) {
            printf("%d, ", b->value);
        }
        printf("]\n");
    }
    printf("]\n");
}

void pawSsa_construct(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct MirBlockList *idom = pawMir_compute_dominance_tree(C, mir);
    struct MirBucketList *df = pawMir_compute_dominance_frontiers(C, mir, idom);

    struct SsaConverter S = {
        .pool = pawP_pool_new(C, C->pool_stats),
        .registers = MirRegisterDataList_new(mir),
        .changes = MirRegisterList_new(mir),
        .locals = mir->locals,
        .idom = idom,
        .mir = mir,
        .df = df,
        .C = C,
        .P = ENV(C),
    };

    S.defs = UseDefMap_new_from(mir, S.pool);
    S.uses = UseDefMap_new_from(mir, S.pool);
    S.capture = RegisterMap_new(&S);
    S.rename = RegisterMap_new(&S);
    S.stacks = NameStackList_new(&S);

    pawMir_collect_per_block_usedefs(mir, S.uses, S.defs);
    NameStackList_reserve(&S, S.stacks, UseDefMap_length(S.defs));

    place_phi_nodes(&S);
    rename_vars(&S, MIR_ROOT_BB);
    mir->registers = S.registers;
    fix_aux_info(&S, mir);

    pawP_pool_free(C, S.pool);
}
