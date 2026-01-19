// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ssa.h"
#include "analysis.h"
#include "error.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"

#define SSA_ERROR(S_, Kind_, ...) pawErr_##Kind_((S_)->C, (S_)->mir->modname, __VA_ARGS__)
#define REGISTER(Reg_, Type_) ((struct MirPlace){.r = Reg_, .kind = MIR_PLACE_REGISTER, .type = Type_})
#define LOCAL(Local_, Type_) ((struct MirPlace){.L = Local_, .type = Type_})

struct SsaConverter {
    struct Compiler *C;
    struct Mir *mir;
    struct Pool *pool;
    struct MirPlaceList *locals;
    struct MirBlockList *idom;
    struct MirBucketList *df;

    // data structures for phi node placement
    struct IntegerList *has;
    struct IntegerList *work;

    // data structures for variable renaming
    struct NameStackList *stacks;
    struct MirLocalList *changes;
    struct PhiMap *phis;

    struct RenameMap *rename; // MirRegister => MirRegister
    UseDefMap *uses; // MirRegister => [MirBlockList]
    UseDefMap *defs; // MirRegister => [MirBlockList]
    paw_Env *P;
};

DEFINE_MAP(struct SsaConverter, RenameMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirLocal, MirRegister)
DEFINE_MAP(struct SsaConverter, PhiMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirId, MirLocal)

static paw_Bool is_trivial_local(struct SsaConverter *S, MirLocal L)
{
    // keep result and arguments
    struct IrFnPtr const *fptr = IR_FPTR(S->mir->type);
    if (L.value <= fptr->params->count) return PAW_FALSE;

    struct MirLocalData const data = *mir_local_data(S->mir, L);
    // being captured implies being non-SSA, but a local can be non-SSA for reasons
    // other than being captured
    paw_assert(!data.is_captured || data.is_nontrivial);
    return !data.is_nontrivial;
}

static struct MirPhi *place_trivial_phi_node(struct SsaConverter *S, MirBlock b, MirLocal L)
{
    struct MirBlockData *bb = mir_bb_data(S->mir, b);
    for (int i = 0; i < bb->joins->count; ++i) {
        struct MirPhi *phi = MirGetPhi(MirInstructionList_get(bb->joins, i));
        if (MIR_ID_EQUALS(phi->output.L, L)) return phi; // already exists
    }
    IrType *type = mir_local_data(S->mir, L)->type;
    struct MirPlaceList *inputs = MirPlaceList_new(S->mir);
    struct MirInstruction *phi = pawMir_new_phi(S->mir, (struct SourceLoc){0},
            inputs, LOCAL(L, type), L.value);
    MirInstructionList_push(S->mir, bb->joins, phi);
    PhiMap_insert(S, S->phis, phi->hdr.mid, L);

    int ninputs = bb->predecessors->count;
    MirPlaceList_reserve(S->mir, inputs, ninputs);
    while (ninputs-- > 0)
        MirPlaceList_push(S->mir, inputs, REGISTER(MIR_INVALID_REG, type));

    return MirGetPhi(phi);
}

DEFINE_LIST(struct SsaConverter, NameStackList, MirPlaceList *)
DEFINE_LIST(struct SsaConverter, IntegerList, int)

MirPlaceList *get_name_stack(struct SsaConverter *S, MirLocal L)
{
    return NameStackList_get(S->stacks, L.value);
}

static struct MirPlace next_register(struct SsaConverter *S, IrType *type)
{
    int const reg_id = S->mir->registers->count;
    struct MirRegisterData const data = {type};
    MirRegisterDataList_push(S->mir, S->mir->registers, data);
    return (struct MirPlace){
        .kind = MIR_PLACE_REGISTER,
        .r = MIR_REG(reg_id),
        .type = type,
        // TODO .loc = ???,
    };
}

static void set_local_name(struct SsaConverter *S, MirLocal L, struct MirPlace name)
{
    MirPlaceList *names = get_name_stack(S, L);
    MirLocalList_push(S->mir, S->changes, L);
    MirPlaceList_push(S->mir, names, name);
}

static struct MirPlace get_local_name(struct SsaConverter *S, MirLocal L)
{
    MirPlaceList const *names = get_name_stack(S, L);
    paw_assert(names != NULL);
    return K_LIST_LAST(names);
}

static void rename_input(struct SsaConverter *S, struct MirPlace *pp)
{
    if (pp->kind == MIR_PLACE_LOCAL
            && is_trivial_local(S, pp->L))
        *pp = get_local_name(S, pp->L);
}

static IrType *deref(IrType *type)
{
    return IrGetPtr(type)->pointee;
}

static void rename_join(struct SsaConverter *S, struct MirInstruction *instr)
{
    struct MirPhi *x = MirGetPhi(instr);
    MirLocal const local = *PhiMap_get(S, S->phis, x->mid);
    x->output = next_register(S, x->output.type);
    set_local_name(S, local, x->output);
}

static void into_move(struct MirInstruction *instr, struct MirPlace target, struct MirPlace output)
{
    instr->Move_.kind = kMirMove;
    instr->Move_.target = target;
    instr->Move_.output = output;
}

static void rename_instruction(struct SsaConverter *S, struct MirInstruction *instr)
{
    if (MirIsLoad(instr)) {
        struct MirLoad *load = MirGetLoad(instr);
        if (load->pointer.kind == MIR_PLACE_LOCAL
                && is_trivial_local(S, load->pointer.L)) {
            struct MirPlace const input = get_local_name(S, load->pointer.L);
            into_move(instr, input, load->output);
        }
    } else if (MirIsStore(instr)) {
        struct MirStore *store = MirGetStore(instr);
        if (store->pointer.kind == MIR_PLACE_LOCAL
                && is_trivial_local(S, store->pointer.L)) {
            struct MirPlace const output = next_register(S, store->value.type);
            set_local_name(S, store->pointer.L, output);
            into_move(instr, store->value, output);
        }
    } else if (MirIsAllocLocal(instr)) {
        struct MirAllocLocal *alloc = MirGetAllocLocal(instr);
        if (is_trivial_local(S, alloc->output.L)) {
            struct MirPlace const output = next_register(S, deref(alloc->output.type));
            set_local_name(S, alloc->output.L, output);
            instr->hdr.kind = kMirNoop;
        }
    } else {
        struct MirPlace *const *ppp;
        MirPlacePtrList const *loads = pawMir_get_loads(S->mir, instr);
        K_LIST_FOREACH (loads, ppp) rename_input(S, *ppp);
    }
}

static paw_Bool list_includes_block(struct MirBlockList const *blocks, MirBlock b)
{
    MirBlock const *pb;
    K_LIST_FOREACH (blocks, pb) {
        if (MIR_ID_EQUALS(b, *pb))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

static struct MirBlockList *compute_live_in(struct SsaConverter *S, struct MirBlockList *defs, MirLocal L)
{
    struct MirBlockList *uses = *UseDefMap_get(S->mir, S->uses, L);
    return pawMir_compute_live_in(S->mir, uses, defs, L);
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
    UseDefMapIterator iter;
    UseDefMapIterator_init(S->defs, &iter);
    // "W" is the worklist of nodes to be processed
    struct MirBlockList *W = MirBlockList_new_from(S->mir, S->pool);
    for (int iterations = 1; UseDefMapIterator_is_valid(&iter);
         ++iterations, UseDefMapIterator_next(&iter)) {
        MirLocal const v = UseDefMapIterator_key(&iter);
        nstacks = PAW_MAX(nstacks, v.value + 1);
        if (!is_trivial_local(S, v)) continue;
        // consider each assignment of the variable
        struct MirBlockList *defs = *UseDefMapIterator_valuep(&iter);
        if (defs->count < 2) continue; // variable has single version

        MirBlock const *pb;
        K_LIST_FOREACH (defs, pb) {
            IntegerList_set(work, pb->value, iterations);
            MirBlockList_push(S->mir, W, *pb);
        }

        // use the live in set for "v" to avoid adding dead phi functions, i.e. phi functions
        // for variables that are not live in at the join node
        struct MirBlockList const *live_in = compute_live_in(S, defs, v);

        while (W->count > 0) {
            // variable "v" has a definition in basic block "x"
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
                // place a trivial phi node "v = phi(NULL, .., NULL)" in basic block "y"
                place_trivial_phi_node(S, *y, v);
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
        MirPlaceList *names = MirPlaceList_new_from(S->mir, S->pool);
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
    K_LIST_FOREACH (block->joins, instr)
        rename_join(S, *instr);
    K_LIST_FOREACH (block->instructions, instr)
        rename_instruction(S, *instr);

    // determine inputs to phi nodes
    K_LIST_FOREACH (block->successors, y) {
        struct MirBlockData const *data = mir_bb_data(S->mir, *y);
        K_LIST_FOREACH (data->joins, instr) {
            // for each phi node in each successor of the current basic block
            struct MirPhi *phi = MirGetPhi(*instr);
            MirLocal const L = *PhiMap_get(S, S->phis, phi->mid);
            MirPlaceList const *stack = get_name_stack(S, L);
            if (stack->count > 0) {
                int const index = mir_which_pred(S->mir, *y, x);
                struct MirPlace const input = K_LIST_LAST(stack);
                MirPlaceList_set(phi->inputs, index, input);
            }
        }
    }

    int b;
    // recur on nodes immediately dominated by the current node
    K_LIST_ENUMERATE (S->idom, b, y) {
        if (MIR_ID_EQUALS(x, *y))
            rename_vars(S, MIR_BB(b));
    }

    // undo changes to the name stacks
    while (S->changes->count > first_change) {
        MirLocal const v = K_LIST_LAST(S->changes);
        MirPlaceList *names = get_name_stack(S, v);
        MirLocalList_pop(S->changes);
        MirPlaceList_pop(names);
    }
}

#ifdef PAW_DEBUG_EXTRA
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

#endif // PAW_DEBUG_EXTRA

static void ssa_construct(struct Pool *pool, struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct MirBlockList *idom = pawMir_compute_dominance_tree(C, mir);
    struct MirBucketList *df = pawMir_compute_dominance_frontiers(C, mir, idom);

    struct SsaConverter S = {
        .locals = mir->locals,
        .pool = pool,
        .idom = idom,
        .mir = mir,
        .df = df,
        .C = C,
        .P = ENV(C),
    };

    S.changes = MirLocalList_new_from(mir, S.pool);
    S.defs = UseDefMap_new_from(mir, S.pool);
    S.uses = UseDefMap_new_from(mir, S.pool);
    S.rename = RenameMap_new(&S);
    S.stacks = NameStackList_new(&S);
    S.phis = PhiMap_new(&S);

    pawMir_collect_per_block_usedefs(mir, S.uses, S.defs);
    NameStackList_reserve(&S, S.stacks, UseDefMap_length(S.defs));

    place_phi_nodes(&S);
    rename_vars(&S, MIR_ENTRY_BB);
}

void pawSsa_construct(struct Mir *mir)
{
    struct Compiler *C = mir->C;

    // put basic blocks in reverse postorder
    pawMir_renumber_basic_blocks(mir);

    // make sure all locals are initialized before they are used
    pawA_validate(mir);

    struct Pool *pool = pawP_pool_new(C, C->aux_stats);
    ssa_construct(pool, mir);
    pawP_pool_free(C, pool);
}
