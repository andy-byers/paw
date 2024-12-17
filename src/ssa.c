// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include "ssa.h"
#include "utest.h"

#warning "remove me"
#/*TODO:remove*/include"stdio.h"

struct SsaConverter {
    struct Compiler *C;
    struct Mir *mir;
    struct MirRegisterDataList *registers;
    struct MirRegisterList *locals;
    struct MirBlockList *idom;
    struct MirBucketList *df;

    // data structures for Phi node placement
    struct IntegerList *has_already;
    struct IntegerList *work;

    // data structures for variable renaming
    struct NameStackList *stacks;
    struct IntegerList *changes;

    Map *rename; // MirRegister => MirRegister
    Map *vars; // MirRegister => vid
    Map *defs; // vid => [defs..]
    int N;
};

static MirRegister new_register(struct SsaConverter *S, struct IrType *type, MirRegister old)
{
    struct MirRegisterDataList *regs = S->registers;
    const int reg_id = regs->count;
    K_LIST_PUSH(S->C, regs, ((struct MirRegisterData){type}));
    pawH_insert(ENV(S->C), S->rename, I2V(old.value), I2V(reg_id));
    return MIR_REG(reg_id);
}

static struct MirPhi *add_phi_node(struct SsaConverter *S, MirBlock b, paw_Int k)
{
printf("Phi(%lld) in bb%d\n", k, b.value);
    struct MirBlockData *data = mir_bb_data(S->mir, b);
    for (int i = 0; i < data->joins->count; ++i) {
        struct MirPhi *phi = MirGetPhi(K_LIST_GET(data->joins, i));
        if (phi->var_id == k) return phi; // already exists
    }
    struct HirDecl *decl = pawHir_get_decl(S->C, (DeclId){.value = k});
    struct IrType *type = GET_NODE_TYPE(S->C, decl);
    struct MirInstruction *phi = pawMir_new_instruction(S->C, kMirPhi);
    struct MirRegisterList *inputs = pawMir_register_list_new(S->C);
    const MirRegister var = K_LIST_GET(S->locals, k);
    MirGetPhi(phi)->inputs = inputs;
    MirGetPhi(phi)->output = var;
    MirGetPhi(phi)->var_id = k;
    K_LIST_PUSH(S->C, data->joins, phi);

    K_LIST_RESERVE(S->C, inputs, data->predecessors->count);
    for (int k = 0; k < data->predecessors->count; ++k) {
        K_LIST_PUSH(S->C, inputs, var);
    }
    return MirGetPhi(phi);
}

DEFINE_LIST(struct Compiler, name_stack_list_, NameStackList, struct MirRegisterList *)
DEFINE_LIST(struct Compiler, integer_list_, IntegerList, int)

static int find_stack_by_register(struct SsaConverter *S, MirRegister r)
{
    const Value *pval = pawH_get(S->vars, I2V(r.value));
    if (pval == NULL) return -1;
    return pval->i;
}

static void rename_var_output(struct SsaConverter *S, MirRegister *pr, int var_id)
{
    struct MirRegisterList *names = K_LIST_GET(S->stacks, var_id);
    struct MirRegisterData *data = mir_reg_data(S->mir, *pr);
    K_LIST_PUSH(S->C, S->changes, var_id);
    *pr = new_register(S, data->type, *pr);
    K_LIST_PUSH(S->C, names, *pr);
}

static void rename_temporary(struct SsaConverter *S, MirRegister *pr)
{
    struct MirRegisterData *data = mir_reg_data(S->mir, *pr);
    const Value *pval = pawH_get(S->rename, I2V(pr->value));
    if (pval != NULL) pr->value = CAST(int, pval->i);
    else *pr = new_register(S, data->type, *pr);
}

static void rename_output(struct SsaConverter *S, MirRegister *pr)
{
    const int index = find_stack_by_register(S, *pr);
    if (index >= 0) {
        rename_var_output(S, pr, index);
        return;
    }
    rename_temporary(S, pr);
}

static void rename_input(struct SsaConverter *S, MirRegister *pr)
{
    const int index = find_stack_by_register(S, *pr);
    if (index >= 0) {
        struct MirRegisterList *names = K_LIST_GET(S->stacks, index);
        paw_assert(names->count > 0);
        *pr = K_LIST_LAST(names);
    } else {
        rename_temporary(S, pr);
    }
}

static void rename_instruction(struct SsaConverter *S, struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirPhi: {
            struct MirPhi *x = MirGetPhi(instr);
            rename_var_output(S, &x->output, x->var_id);
            break;
        }
        case kMirMove: {
            struct MirMove *x = MirGetMove(instr);
            rename_input(S, &x->target);
            rename_output(S, &x->output);
            break;
        }
        case kMirUpvalue: {
            struct MirUpvalue *x = MirGetUpvalue(instr);
            rename_output(S, &x->output);
            break;
        }
        case kMirGlobal: {
            struct MirGlobal *x = MirGetGlobal(instr);
            rename_output(S, &x->output);
            break;
        }
        case kMirAllocLocal: {
            struct MirAllocLocal *x = MirGetAllocLocal(instr);
            rename_output(S, &x->output);
            break;
        }
        case kMirFreeLocal: {
            struct MirFreeLocal *x = MirGetFreeLocal(instr);
            rename_input(S, &x->reg);
            break;
        }
        case kMirSetLocal: {
            struct MirSetLocal *x = MirGetSetLocal(instr);
            rename_input(S, &x->value);
            rename_output(S, &x->target);
            rename_output(S, &x->output);
            break;
        }
        case kMirSetUpvalue: {
            struct MirSetUpvalue *x = MirGetSetUpvalue(instr);
            rename_input(S, &x->value);
            rename_output(S, &x->output);
            break;
        }
        case kMirConstant: {
            struct MirConstant *x = MirGetConstant(instr);
            rename_output(S, &x->output);
            break;
        }
        case kMirAggregate: {
            struct MirAggregate *x = MirGetAggregate(instr);
            rename_output(S, &x->output);
            break;
        }
        case kMirContainer: {
            struct MirContainer *x = MirGetContainer(instr);
            rename_output(S, &x->output);
            break;
        }
        case kMirCall: {
            struct MirCall *x = MirGetCall(instr);
            rename_input(S, &x->target);
            for (int i = 0; i < x->args->count; ++i) {
                MirRegister *parg = &K_LIST_GET(x->args, i);
                rename_input(S, parg);
            }
            rename_output(S, &x->output);
            break;
        }
        case kMirCast: {
            struct MirCast *x = MirGetCast(instr);
            rename_input(S, &x->target);
            rename_output(S, &x->output);
            break;
        }
        case kMirClose: {
            struct MirClose *x = MirGetClose(instr);
            rename_input(S, &x->target);
            break;
        }
        case kMirClosure: {
            struct MirClosure *x = MirGetClosure(instr);
            rename_output(S, &x->output);
            break;
        }
        case kMirSetElement: {
            struct MirSetElement *x = MirGetSetElement(instr);
            rename_input(S, &x->object);
            rename_input(S, &x->key);
            rename_input(S, &x->value);
            rename_output(S, &x->output);
            break;
        }
        case kMirGetElement: {
            struct MirGetElement *x = MirGetGetElement(instr);
            rename_input(S, &x->object);
            rename_input(S, &x->key);
            rename_output(S, &x->output);
            break;
        }
        case kMirSetRange: {
            struct MirSetRange *x = MirGetSetRange(instr);
            rename_input(S, &x->object);
            rename_input(S, &x->lower);
            rename_input(S, &x->upper);
            rename_input(S, &x->value);
            rename_output(S, &x->output);
            break;
        }
        case kMirGetRange: {
            struct MirGetRange *x = MirGetGetRange(instr);
            rename_input(S, &x->object);
            rename_input(S, &x->lower);
            rename_input(S, &x->upper);
            rename_output(S, &x->output);
            break;
        }
        case kMirSetField: {
            struct MirSetField *x = MirGetSetField(instr);
            rename_input(S, &x->object);
            rename_input(S, &x->value);
            rename_output(S, &x->output);
            break;
        }
        case kMirGetField: {
            struct MirGetField *x = MirGetGetField(instr);
            rename_input(S, &x->object);
            rename_output(S, &x->output);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *x = MirGetUnaryOp(instr);
            rename_input(S, &x->val);
            rename_output(S, &x->output);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *x = MirGetBinaryOp(instr);
            rename_input(S, &x->lhs);
            rename_input(S, &x->rhs);
            rename_output(S, &x->output);
            break;
        }
    }
}

static void rename_terminator(struct SsaConverter *S, struct MirTerminator *term)
{
    switch (MIR_KINDOF(term)) {
        case kMirReturn: {
            struct MirReturn *x = MirGetReturn(term);
            rename_input(S, &x->value);
            break;
        }
        case kMirForLoop: {
            struct MirForLoop *x = MirGetForLoop(term);
            rename_input(S, &x->iter);
            rename_input(S, &x->end);
            rename_input(S, &x->step);
            rename_input(S, &x->var);
            break;
        }
        case kMirBranch: {
            struct MirBranch *x = MirGetBranch(term);
            rename_input(S, &x->cond);
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *x = MirGetSwitch(term);
            rename_input(S, &x->discr);
            break;
        }
        case kMirGoto:
            break;
    }
}

static void place_phi_nodes(struct SsaConverter *S)
{
    const int nblocks = S->mir->blocks->count;
    struct IntegerList *has_already = integer_list_new(S->C);
    struct IntegerList *work = integer_list_new(S->C);
    K_LIST_RESERVE(S->C, has_already, nblocks);
    K_LIST_RESERVE(S->C, work, nblocks);
    for (int i = 0; i < nblocks; ++i) {
        K_LIST_PUSH(S->C, has_already, 0);
        K_LIST_PUSH(S->C, work, 0);
    }

    Map *definitions = S->defs;
    paw_Int iter = PAW_ITER_INIT;
    struct MirBlockList *W = pawMir_block_list_new(S->C);
    for (int iterations = 1; pawH_iter(definitions, &iter); ++iterations) {
        const paw_Int V = pawH_key(definitions, iter)->i;
        // consider each assignment of the variable
        struct MirBlockList *defs = pawH_value(definitions, iter)->p;
        for (int i = 0; i < defs->count; ++i) {
            const MirBlock x = K_LIST_GET(defs, i);
            K_LIST_SET(work, x.value, iterations);
            K_LIST_PUSH(S->C, W, x);
        }

        while (W->count > 0) {
            const MirBlock x = K_LIST_LAST(W);
            K_LIST_POP(W);
            const struct MirBlockList *df = K_LIST_GET(S->df, x.value);
            for (int i = 0; i < df->count; ++i) {
                const MirBlock y = K_LIST_GET(df, i);
                if (K_LIST_GET(has_already, y.value) >= iterations
                        || defs->count <= 1) continue;
                // place a trivial phi node "V = phi(V, .., V)" in basic block "y"
                struct MirPhi *phi = add_phi_node(S, y, V);
                K_LIST_SET(has_already, y.value, iterations);
                // transitive step/relation to the iterated dominance frontier
                if (K_LIST_GET(work, y.value) < iterations) {
                    K_LIST_SET(work, y.value, iterations);
                    K_LIST_PUSH(S->C, W, y);
                }
            }
        }

        // allocate stack for renaming pass
        struct MirRegisterList *names = pawMir_register_list_new(S->C);
        K_LIST_PUSH(S->C, S->stacks, names);
    }
}

// Determine the index of "x" in the predecessor list of "y"
static int which_pred(struct Mir *mir, MirBlock y, MirBlock x)
{
    const struct MirBlockData *data = mir_bb_data(mir, y);
    for (int i = 0; i < data->predecessors->count; ++i) {
        const MirBlock p = K_LIST_GET(data->predecessors, i);
        if (MIR_BB_EQUALS(x, p)) return i;
    }

    PAW_UNREACHABLE();
}

static void rename_vars(struct SsaConverter *S, MirBlock x)
{
    struct MirBlockData *block = mir_bb_data(S->mir, x);
    const int first_change = S->changes->count;

    // fix references to the old name
    for (int i = 0; i < block->joins->count; ++i) rename_instruction(S, K_LIST_GET(block->joins, i));
    for (int i = 0; i < block->instructions->count; ++i) rename_instruction(S, K_LIST_GET(block->instructions, i));
    rename_terminator(S, block->terminator);

    // determine inputs to Phi nodes
    struct MirBlockList *succ = pawMir_get_successors(S->C, block->terminator);
    for (int i = 0; i < succ->count; ++i) {
        const MirBlock y = K_LIST_GET(succ, i);
        const struct MirBlockData *data = mir_bb_data(S->mir, y);
        for (int j = 0; j < data->joins->count; ++j) {
            struct MirInstruction *instr = K_LIST_GET(data->joins, i);
            // for each Phi node in each successor of the current basic block
            struct MirPhi *phi = MirGetPhi(instr);
            const struct MirRegisterList *stack = K_LIST_GET(S->stacks, phi->var_id);
            if (stack->count > 0) {
                const int index = which_pred(S->mir, y, x);
                K_LIST_SET(phi->inputs, index, K_LIST_LAST(stack));
            }
        }
    }

    // recur on nodes immediately dominated by the current node
    for (int i = 0; i < S->idom->count; ++i) {
        const MirBlock y = K_LIST_GET(S->idom, i);
        if (MIR_BB_EQUALS(x, y)) rename_vars(S, MIR_BB(i));
    }

    // undo changes to the name stacks
    while (S->changes->count > first_change) {
        const int var_id = K_LIST_LAST(S->changes);
        struct MirRegisterList *names = K_LIST_GET(S->stacks, var_id);
printf("undo _%d for var_id %d\n",K_LIST_LAST(names).value,var_id);
        K_LIST_POP(S->changes);
        K_LIST_POP(names);
    }
}

#include"stdio.h"
void pawSsa_convert(struct Compiler *C, struct Mir *mir, Map *vars, Map *defs, struct MirRegisterList *locals)
{
    pawMir_remove_unreachable_blocks(C, mir);
    struct MirBlockList *idom = pawMir_compute_dominance_tree(C, mir);
    struct MirBucketList *df = pawMir_compute_dominance_frontiers(C, mir, idom);

    struct SsaConverter S = {
        .N = pawH_length(defs),
        .registers = pawMir_register_data_list_new(C),
        .changes = integer_list_new(C),
        .stacks = name_stack_list_new(C),
        .locals = locals,
        .idom = idom,
        .vars = vars,
        .defs = defs,
        .mir = mir,
        .df = df,
        .C = C,
    };
    S.rename = pawP_push_map(C);

    place_phi_nodes(&S);
printf("%s\n",pawMir_dump(C, mir));--ENV(C)->top.p;

    rename_vars(&S, MIR_ROOT_BB);
    mir->registers = S.registers;
printf("%s\n",pawMir_dump(C, mir));--ENV(C)->top.p;

    pawP_pop_object(C, S.rename);
}

struct PhiEliminator {
    struct Compiler *C;
    struct Mir *mir;
    Map *copies;
    int N;
};

struct ParallelCopy {
    struct MirRegisterList *inputs;
    struct MirRegisterList *outputs;
    MirBlock bb;
};

DEFINE_LIST(struct Compiler, pcopy_list_, PCopyList, struct ParallelCopy *)

static struct ParallelCopy *new_pcopy(struct PhiEliminator *P)
{
    struct ParallelCopy *pcopy = pawK_pool_alloc(ENV(P->C), P->C->pool, sizeof(struct ParallelCopy));
    *pcopy = (struct ParallelCopy){
        .inputs = pawMir_register_list_new(P->C),
        .outputs = pawMir_register_list_new(P->C),
        .bb = MIR_INVALID_BB,
    };
    return pcopy;
}

//static struct PCopyList *get_pcopy_list(struct PhiEliminator *P, MirBlock bb)
//{
//    const Value *pval = pawH_get(P->copies, I2V(bb.value));
//    if (pval != NULL) return pval->p;
//    struct PCopyList *list = pcopy_list_new(P->C);
//    pawH_insert(ENV(P->C), P->copies, I2V(bb.value), P2V(list));
//    return list;
//}
//
//static void add_pcopy(struct PhiEliminator *P, MirBlock bb, struct ParallelCopy *pcopy)
//{
//    struct PCopyList *list = get_pcopy_list(P, bb);
//    K_LIST_PUSH(P->C, list, pcopy);
//}

static int is_split_node(struct PhiEliminator *P, struct MirBlockData *data)
{
    const struct MirBlockList *succ = pawMir_get_successors(P->C, data->terminator);
    return succ->count > 1;
}

static MirBlock new_split_bb(struct PhiEliminator *P)
{
    struct MirBlockData *data = pawMir_new_block(P->C);
    data->terminator = pawMir_new_terminator(P->C, kMirGoto);
    MirGetGoto(data->terminator)->target = MIR_INVALID_BB;
    K_LIST_PUSH(P->C, P->mir->blocks, data);
    return MIR_BB(P->mir->blocks->count - 1);
}

static void set_terminator_target(struct MirTerminator *term, int index, MirBlock target)
{
    paw_assert(index >= 0);
    switch (MIR_KINDOF(term)) {
        case kMirReturn:
            PAW_UNREACHABLE();
        case kMirGoto:
            paw_assert(index == 0);
            MirGetGoto(term)->target = target;
            break;
        case kMirBranch:
            paw_assert(index <= 2);
            if (index == 0) MirGetBranch(term)->then_arm = target;
            else MirGetBranch(term)->else_arm = target;
            break;
        case kMirForLoop:
            paw_assert(index <= 2);
            if (index == 0) MirGetForLoop(term)->then_arm = target;
            else MirGetForLoop(term)->else_arm = target;
            break;
        case kMirSwitch: {
            struct MirSwitch *switch_ = MirGetSwitch(term);
            if (index < switch_->arms->count) {
                K_LIST_GET(switch_->arms, index).bid = target;
            } else {
                paw_assert(switch_->has_otherwise);
                paw_assert(index == switch_->arms->count);
                switch_->otherwise = target;
            }
            break;
        }
    }
}

static int which_succ(struct PhiEliminator *P, MirBlock y, MirBlock x)
{
    const struct MirBlockData *data = mir_bb_data(P->mir, x);
    const struct MirBlockList *succ = pawMir_get_successors(P->C, data->terminator);
    for (int i = 0; i < succ->count; ++i) {
        const MirBlock p = K_LIST_GET(succ, i);
        if (MIR_BB_EQUALS(y, p)) return i;
    }

    PAW_UNREACHABLE();
}

static void insert_between_blocks(struct PhiEliminator *P, MirBlock a, MirBlock b, MirBlock c)
{
    struct MirBlockData *A = mir_bb_data(P->mir, a);
    struct MirBlockData *B = mir_bb_data(P->mir, b);
    struct MirBlockData *C = mir_bb_data(P->mir, c);
    const int a2c = which_succ(P, c, a);
    const int c2a = which_pred(P->mir, c, a);
    set_terminator_target(A->terminator, a2c, b);
    set_terminator_target(B->terminator, 0, c);
    K_LIST_PUSH(P->C, B->predecessors, a);
    K_LIST_SET(C->predecessors, c2a, b);
}

static void destroy_ssa(struct PhiEliminator *P)
{
    for (MirBlock b = MIR_ROOT_BB; b.value < P->N; ++b.value) {
        struct PCopyList *pcopies = pcopy_list_new(P->C);
        struct MirBlockData *B = mir_bb_data(P->mir, b);
        if (B->predecessors->count < 2) continue;

        // split critical edges (edges where the predecessor has multiple successors, and
        // the successor multiple predecessors) and add empty parallel copy pseudoinstructions
        for (int i = 0; i < B->predecessors->count; ++i) {
            const MirBlock bi = K_LIST_GET(B->predecessors, i);
            struct ParallelCopy *pcopy = new_pcopy(P);
            struct MirBlockData *Bi = mir_bb_data(P->mir, bi);
            if (is_split_node(P, Bi)) { // critical edge
                const MirBlock bpi = new_split_bb(P);
                insert_between_blocks(P, bi, bpi, b);
                pcopy->bb = bpi;
            } else {
                pcopy->bb = bi;
            }
            K_LIST_PUSH(P->C, pcopies, pcopy);
        }

        for (int i = 0; i < B->joins->count; ++i) {
            struct MirPhi *phi = MirGetPhi(K_LIST_GET(B->joins, i));
            for (int j = 0; j < phi->inputs->count; ++j) {
                // TODO: sequentialize parallel copies using algo 3.6 in SSA book
                struct ParallelCopy *pcopy = K_LIST_GET(pcopies, j);
                const MirRegister ai = K_LIST_GET(phi->inputs, j);
                struct MirBlockData *data = mir_bb_data(P->mir, pcopy->bb);
                struct MirInstruction *move = pawMir_new_instruction(P->C, kMirMove);
                MirGetMove(move)->output = phi->output;
                MirGetMove(move)->target = ai;
                K_LIST_PUSH(P->C, data->instructions, move);
            }
        }
        B->joins->count = 0;
    }
}

void pawSsa_destroy(struct Compiler *C, struct Mir *mir)
{
    struct PhiEliminator P = {
        .N = mir->blocks->count,
        .mir = mir,
        .C = C,
    };
    P.copies = pawP_push_map(C);

    destroy_ssa(&P);

    pawP_pop_object(C, P.copies);
}

PAW_TEST(ssa)
{
}
