// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "mir.h"
#include "ir_type.h"
#include "map.h"

struct Mir *pawMir_new(struct Compiler *C, String *name, struct IrType *type, struct IrType *self, enum FuncKind fn_kind, paw_Bool is_pub, paw_Bool is_poly)
{
    struct Mir *mir = P_ALLOC(C, NULL, 0, sizeof(*mir));
    *mir = (struct Mir){
        .pool = pawP_pool_new(C),
        .captured = MirCaptureList_new(C),
        .registers = MirRegisterDataList_new(C),
        .locals = MirRegisterList_new(C),
        .blocks = MirBlockDataList_new(C),
        .upvalues = MirUpvalueList_new(C),
        .children = MirBodyList_new(C),
        .is_poly = is_poly,
        .is_pub = is_pub,
        .fn_kind = fn_kind,
        .name = name,
        .type = type,
        .self = self,
        .P = ENV(C),
        .C = C,
    };
    return mir;
}

struct MirLiveInterval *pawMir_new_interval(struct Compiler *C, MirRegister r, int npositions)
{
    struct MirLiveInterval *it = P_ALLOC(C, NULL, 0, sizeof(struct MirLiveInterval));
    *it = (struct MirLiveInterval){
        .ranges = pawP_bitset_new(C, npositions),
        .r = r,
    };
    return it;
}

struct MirRegisterData *pawMir_new_register(struct Compiler *C, int value, struct IrType *type)
{
    struct MirRegisterData *r = P_ALLOC(C, NULL, 0, sizeof(struct MirRegisterData));
    *r = (struct MirRegisterData){
        .type = type,
    };
    return r;
}

struct MirInstruction *pawMir_new_instruction(struct Mir *mir)
{
    return P_ALLOC(mir->C, NULL, 0, sizeof(struct MirInstruction));
}

struct MirBlockData *pawMir_new_block(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct MirBlockData *block = P_ALLOC(C, NULL, 0, sizeof(struct MirBlockData));
    *block = (struct MirBlockData){
        .predecessors = MirBlockList_new(C),
        .successors = MirBlockList_new(C),
        .joins = MirInstructionList_new(C),
        .instructions = MirInstructionList_new(C),
        .mid = pawMir_next_id(mir),
    };
    return block;
}

static void AcceptPhi(struct MirVisitor *V, struct MirPhi *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register_list(V, t->inputs);
}

static void AcceptMove(struct MirVisitor *V, struct MirMove *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->target);
}

static void AcceptGlobal(struct MirVisitor *V, struct MirGlobal *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptConstant(struct MirVisitor *V, struct MirConstant *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptAggregate(struct MirVisitor *V, struct MirAggregate *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptContainer(struct MirVisitor *V, struct MirContainer *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptUpvalue(struct MirVisitor *V, struct MirUpvalue *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptSetLocal(struct MirVisitor *V, struct MirSetLocal *t)
{
    pawMir_visit_register(V, t->target);
    pawMir_visit_register(V, t->value);
}

static void AcceptSetUpvalue(struct MirVisitor *V, struct MirSetUpvalue *t)
{
    pawMir_visit_register(V, t->value);
}

static void AcceptAllocLocal(struct MirVisitor *V, struct MirAllocLocal *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptFreeLocal(struct MirVisitor *V, struct MirFreeLocal *t)
{
    pawMir_visit_register(V, t->reg);
}

static void AcceptCall(struct MirVisitor *V, struct MirCall *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->target);
    pawMir_visit_register_list(V, t->args);
}

static void AcceptCast(struct MirVisitor *V, struct MirCast *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptClose(struct MirVisitor *V, struct MirClose *t)
{
    pawMir_visit_register(V, t->target);
}

static void AcceptClosure(struct MirVisitor *V, struct MirClosure *t)
{
    pawMir_visit_register(V, t->output);
}

static void AcceptGetElement(struct MirVisitor *V, struct MirGetElement *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->key);
}

static void AcceptSetElement(struct MirVisitor *V, struct MirSetElement *t)
{
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->key);
    pawMir_visit_register(V, t->value);
}

static void AcceptGetRange(struct MirVisitor *V, struct MirGetRange *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->lower);
    pawMir_visit_register(V, t->upper);
}

static void AcceptSetRange(struct MirVisitor *V, struct MirSetRange *t)
{
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->lower);
    pawMir_visit_register(V, t->upper);
    pawMir_visit_register(V, t->value);
}

static void AcceptGetField(struct MirVisitor *V, struct MirGetField *t)
{
    pawMir_visit_register(V, t->output);
    pawMir_visit_register(V, t->object);
}

static void AcceptSetField(struct MirVisitor *V, struct MirSetField *t)
{
    pawMir_visit_register(V, t->object);
    pawMir_visit_register(V, t->value);
}

static void AcceptUnaryOp(struct MirVisitor *V, struct MirUnaryOp *t)
{
    pawMir_visit_register(V, t->val);
    pawMir_visit_register(V, t->output);
}

static void AcceptBinaryOp(struct MirVisitor *V, struct MirBinaryOp *t)
{
    pawMir_visit_register(V, t->lhs);
    pawMir_visit_register(V, t->rhs);
    pawMir_visit_register(V, t->output);
}

static void AcceptReturn(struct MirVisitor *V, struct MirReturn *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptGoto(struct MirVisitor *V, struct MirGoto *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptBranch(struct MirVisitor *V, struct MirBranch *t)
{
    pawMir_visit_register(V, t->cond);
}

static void AcceptSwitch(struct MirVisitor *V, struct MirSwitch *t)
{
    pawMir_visit_register(V, t->discr);
}

#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL ? (V)->PostVisit##name(V, x) : (void)0)
#define DEFINE_VISITOR_CASES(X)             \
    case kMir##X: {                         \
        struct Mir##X *v = MirGet##X(node); \
        if (VISITOR_CALL(V, X, v))          \
            Accept##X(V, v);                \
        VISITOR_POSTCALL(V, X, v);          \
    } break;

void pawMir_visit_register(struct MirVisitor *V, MirRegister r)
{
    if (V->VisitRegister(V, r)) {
        V->PostVisitRegister(V, r);
    }
}

void pawMir_visit_instruction(struct MirVisitor *V, struct MirInstruction *node)
{
    paw_assert(node != NULL);
    if (!V->VisitInstruction(V, node))
        return;

    switch (MIR_KINDOF(node)) {
        MIR_INSTRUCTION_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitInstruction(V, node);
}

void pawMir_visit_block(struct MirVisitor *V, MirBlock bb)
{
    struct MirBlockData *block = mir_bb_data(V->mir, bb);
    paw_assert(block != NULL);

    if (!V->VisitBlock(V, bb))
        return;

    pawMir_visit_instruction_list(V, block->instructions);

    V->PostVisitBlock(V, bb);
}

paw_Bool default_visit_instruction(struct MirVisitor *V, struct MirInstruction *node) { return PAW_TRUE; }
paw_Bool default_visit_block(struct MirVisitor *V, MirBlock node) { return PAW_TRUE; }
paw_Bool default_visit_register(struct MirVisitor *V, MirRegister node) { return PAW_TRUE; }
void default_post_visit_instruction(struct MirVisitor *V, struct MirInstruction *node) {}
void default_post_visit_block(struct MirVisitor *V, MirBlock node) {}
void default_post_visit_register(struct MirVisitor *V, MirRegister node) {}

void pawMir_visitor_init(struct MirVisitor *V, struct Compiler *C, struct Mir *mir, void *ud)
{
    *V = (struct MirVisitor){
        .mir = mir,
        .ud = ud,
        .C = C,

        .VisitBlock = default_visit_block,
        .VisitInstruction = default_visit_instruction,
        .VisitRegister = default_visit_register,

        .PostVisitBlock = default_post_visit_block,
        .PostVisitInstruction = default_post_visit_instruction,
        .PostVisitRegister = default_post_visit_register,
    };
}

void pawMir_visit(struct MirVisitor *V)
{
    struct Mir *mir = V->mir;
    for (int i = 0; i < mir->blocks->count; ++i) {
        // callee will look up the block data struct using the provided block ID
        pawMir_visit_block(V, MIR_BB(i));
    }
    // TODO: closures should be unnested so children can be visited separately
    struct Mir *const *pchild;
    K_LIST_FOREACH (mir->children, pchild) {
        V->mir = *pchild;
        pawMir_visit(V);
    }
    V->mir = mir;
}

#define DEFINE_LIST_VISITOR(name, T)                                                 \
    void pawMir_visit_##name##_list(struct MirVisitor *V, struct Mir##T##List *list) \
    {                                                                                \
        if (list == NULL)                                                            \
            return;                                                                  \
        for (int i = 0; i < list->count; ++i) {                                      \
            pawMir_visit_##name(V, Mir##T##List_get(list, i));                       \
        }                                                                            \
    }
DEFINE_LIST_VISITOR(block, Block)
DEFINE_LIST_VISITOR(instruction, Instruction)
DEFINE_LIST_VISITOR(register, Register)
#undef DEFINE_LIST_VISITOR

struct Successors {
    struct MirBlockList *successors;
    MirBlock block;
};

struct Traversal {
    struct Pool *pool;
    struct Compiler *C;
    struct Mir *mir;
    struct VisitedMap *visited;
    paw_Env *P;
};

DEFINE_LIST(struct Traversal, VisitStack, struct Successors)
DEFINE_MAP(struct Traversal, VisitedMap, pawP_alloc, MIR_ID_HASH, MIR_ID_EQUALS, MirBlock, void *)
DEFINE_MAP(struct Traversal, BlockMap, pawP_alloc, MIR_ID_HASH, MIR_ID_EQUALS, MirBlock, MirBlock)

static paw_Bool check_visited(struct Traversal *X, VisitedMap *visited, MirBlock bb)
{
    if (VisitedMap_get(X, visited, bb) != NULL)
        return PAW_TRUE;
    VisitedMap_insert(X, visited, bb, NULL);
    return PAW_FALSE;
}

static void reverse_blocks(struct MirBlockList *blocks)
{
    if (blocks->count <= 0)
        return;
    MirBlock *a = &K_LIST_FIRST(blocks);
    MirBlock *b = &K_LIST_LAST(blocks);
    for (; a < b; a++, b--) {
        MirBlock const t = *a;
        *a = *b;
        *b = t;
    }
}

static struct MirBlockList *copy_blocks(struct Traversal *X, struct MirBlockList *blocks)
{
    MirBlock *b;
    struct MirBlockList *result = MirBlockList_new(X->C);
    MirBlockList_reserve(X->C, result, blocks->count);
    K_LIST_FOREACH (blocks, b) {
        MirBlockList_push(X->C, result, *b);
    }
    return result;
}

static void visit_postorder(struct Traversal *X, VisitedMap *visited, struct VisitStack *stack, MirBlock bb)
{
    if (check_visited(X, visited, bb))
        return;
    struct MirBlockData *data = mir_bb_data(X->mir, bb);
    VisitStack_push(X, stack, ((struct Successors){
                                  .successors = copy_blocks(X, data->successors),
                                  .block = bb,
                              }));
}

static void traverse_postorder(struct Traversal *X, VisitedMap *visited, struct VisitStack *stack)
{
    while (stack->count > 0) {
        struct Successors *top = &K_LIST_LAST(stack);
        if (top->successors->count == 0)
            break;
        MirBlock const bb = K_LIST_LAST(top->successors);
        MirBlockList_pop(top->successors);
        visit_postorder(X, visited, stack, bb);
    }
}

struct MirBlockList *pawMir_traverse_rpo(struct Compiler *C, struct Mir *mir)
{
    struct Traversal X = {
        .pool = pawP_pool_new(C),
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    struct VisitStack *stack = VisitStack_new(&X);
    X.visited = VisitedMap_new(&X, X.pool);
    paw_assert(mir->blocks->count > 0);
    visit_postorder(&X, X.visited, stack, MIR_ROOT_BB);
    traverse_postorder(&X, X.visited, stack);

    struct MirBlockList *order = MirBlockList_new(C);
    while (stack->count > 0) {
        struct Successors s = K_LIST_LAST(stack);
        VisitStack_pop(stack);
        traverse_postorder(&X, X.visited, stack);
        MirBlockList_push(C, order, s.block);
    }
    reverse_blocks(order);
    pawP_pool_free(C, X.pool);
    return order;
}

static void renumber_ref(struct Traversal *X, BlockMap *map, MirBlock *pfrom)
{
    paw_assert(MIR_BB_EXISTS(*pfrom));
    *pfrom = *BlockMap_get(X, map, *pfrom);
}

static void renumber_or_clear_ref(struct Traversal *X, BlockMap *map, MirBlock *pfrom)
{
    paw_assert(MIR_BB_EXISTS(*pfrom));
    MirBlock const *pto = BlockMap_get(X, map, *pfrom);
    if (pto != NULL) {
        *pfrom = *pto;
    } else {
        *pfrom = MIR_INVALID_BB;
    }
}

static void remove_join(struct Mir *mir, struct MirInstructionList *joins, struct MirInstructionList *instrs, int index)
{
    if (joins == NULL)
        return;

    // TODO: instead of transforming into MirMove, remove the phi node and somehow indicate that
    //       uses of the phi's output register should be replaced with the single input register
    //       (more or less copy prop).
    int ijoin;
    struct MirInstruction **pinstr;
    K_LIST_ENUMERATE (joins, ijoin, pinstr) {
        struct MirPhi *phi = MirGetPhi(*pinstr);
        paw_assert(phi->inputs->count > 1);
        // remove phi node input corresponding to unreachable basic block,
        // maintaining the 1-to-1 correspondence between phi node inputs and
        // predecessor basic blocks
        MirRegisterList_remove(phi->inputs, index);
        if (phi->inputs->count == 1) {
            // a phi node with a single input is really just a move: transfer
            // to the ".instructions" list
            *pinstr = pawMir_new_move(mir, phi->line,
                                      phi->output, K_LIST_FIRST(phi->inputs));
            MirInstructionList_insert(mir->C, instrs, 0, *pinstr);
            MirInstructionList_swap_remove(joins, ijoin);
        }
    }
}

static void rename_and_filter(struct Traversal *X, BlockMap *map, struct MirBlockList *blocks, struct MirBlockData *bb)
{
    struct Mir *mir = X->mir;

    int index;
    int removed = 0;
    MirBlock const *pfrom;
    K_LIST_ENUMERATE (blocks, index, pfrom) {
        MirBlock const *pto = BlockMap_get(X, map, *pfrom);
        if (pto == NULL) {
            remove_join(mir, bb->joins, bb->instructions, index);
            ++removed;
            continue;
        }
        MirBlockList_set(blocks, index - removed, *pto);
    }
    blocks->count -= removed;
}

// Renumber basic blocks so that "mir_bb_data" continues to work
static void renumber_block_refs(struct Traversal *X, BlockMap *map, struct MirBlockData *data)
{
    rename_and_filter(X, map, data->predecessors, data);
    rename_and_filter(X, map, data->successors, NULL);

    struct MirInstruction *terminator = K_LIST_LAST(data->instructions);
    switch (MIR_KINDOF(terminator)) {
        case kMirReturn:
            break;
        case kMirBranch: {
            struct MirBranch *t = MirGetBranch(terminator);
            renumber_ref(X, map, &t->then_arm);
            renumber_ref(X, map, &t->else_arm);
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *t = MirGetSwitch(terminator);
            for (int i = 0; i < t->arms->count; ++i)
                renumber_ref(X, map, &K_LIST_AT(t->arms, i).bid);
            if (MIR_BB_EXISTS(t->otherwise))
                renumber_ref(X, map, &t->otherwise);
            break;
        }
        case kMirGoto: {
            struct MirGoto *t = MirGetGoto(terminator);
            renumber_ref(X, map, &t->target);
            break;
        }
        default:
            PAW_UNREACHABLE();
    }
}

//// TODO
// #include <stdio.h>
//
// static void dump_usedef(struct Compiler *C, Map *uses, Map *defs)
//{
//     paw_Int iter = PAW_ITER_INIT;
//     printf("uses = {\n");
//     while (pawH_iter(uses, &iter)) {
//         printf("  _%lld: [", pawH_key(uses, iter)->i);
//         struct MirBlockList *bl = pawH_value(uses, iter)->p;
//         int i;
//         const MirBlock *pb;
//         K_LIST_ENUMERATE(bl, i, pb) {
//             if (i > 0) printf(", ");
//             printf("bb%d", pb->value);
//         }
//         printf("]\n");
//     }
//     iter = PAW_ITER_INIT;
//     printf("}\ndefs = {\n");
//     while (pawH_iter(uses, &iter)) {
//         printf("  _%lld: [", pawH_key(defs, iter)->i);
//         struct MirBlockList *bl = pawH_value(defs, iter)->p;
//         int i;
//         const MirBlock *pb;
//         K_LIST_ENUMERATE(bl, i, pb) {
//             if (i > 0) printf(", ");
//             printf("bb%d", pb->value);
//         }
//         printf("]\n");
//     }
// }

void pawMir_remove_unreachable_blocks(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct Traversal X = {
        .pool = pawP_pool_new(C),
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    // create a mapping from old to new basic block numbers
    BlockMap *map = BlockMap_new(&X, X.pool);
    struct MirBlockList *order = pawMir_traverse_rpo(C, mir);

    int index;
    MirBlock const *pb;
    K_LIST_ENUMERATE (order, index, pb) {
        BlockMap_insert(&X, map, *pb, MIR_BB(index));
    }

    struct MirBlockDataList *blocks = MirBlockDataList_new(C);
    MirBlockDataList_reserve(C, blocks, order->count);
    K_LIST_FOREACH (order, pb) {
        struct MirBlockData *bb = mir_bb_data(mir, *pb);
        renumber_block_refs(&X, map, bb);
        MirBlockDataList_push(C, blocks, bb);
    }

    mir->blocks = blocks;
    pawP_pool_free(C, X.pool);
}

struct MirRegisterPtrList *pawMir_get_loads(struct Compiler *C, struct MirInstruction *instr)
{
#define ADD_INPUT(x) MirRegisterPtrList_push(C, inputs, &(x))
#define ADD_INPUTS(xs)      \
    K_LIST_FOREACH (xs, pr) \
    ADD_INPUT(*pr)

    MirRegister *pr;
    struct MirRegisterPtrList *inputs = MirRegisterPtrList_new(C);

    switch (MIR_KINDOF(instr)) {
        case kMirPhi: {
            struct MirPhi *x = MirGetPhi(instr);
            ADD_INPUTS(x->inputs);
            break;
        }
        case kMirMove: {
            struct MirMove *x = MirGetMove(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirSetLocal: {
            struct MirSetLocal *x = MirGetSetLocal(instr);
            ADD_INPUT(x->value);
            break;
        }
        case kMirCall: {
            struct MirCall *x = MirGetCall(instr);
            ADD_INPUT(x->target);
            ADD_INPUTS(x->args);
            break;
        }
        case kMirCast: {
            struct MirCast *x = MirGetCast(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirGetElement: {
            struct MirGetElement *x = MirGetGetElement(instr);
            ADD_INPUT(x->object);
            ADD_INPUT(x->key);
            break;
        }
        case kMirGetRange: {
            struct MirGetRange *x = MirGetGetRange(instr);
            ADD_INPUT(x->object);
            ADD_INPUT(x->lower);
            ADD_INPUT(x->upper);
            break;
        }
        case kMirGetField: {
            struct MirGetField *x = MirGetGetField(instr);
            ADD_INPUT(x->object);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *x = MirGetUnaryOp(instr);
            ADD_INPUT(x->val);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *x = MirGetBinaryOp(instr);
            ADD_INPUT(x->lhs);
            ADD_INPUT(x->rhs);
            break;
        }
        case kMirSetUpvalue: {
            struct MirSetUpvalue *x = MirGetSetUpvalue(instr);
            ADD_INPUT(x->value);
            break;
        }
        case kMirSetElement: {
            struct MirSetElement *x = MirGetSetElement(instr);
            ADD_INPUT(x->object);
            ADD_INPUT(x->key);
            ADD_INPUT(x->value);
            break;
        }
        case kMirSetRange: {
            struct MirSetRange *x = MirGetSetRange(instr);
            ADD_INPUT(x->object);
            ADD_INPUT(x->lower);
            ADD_INPUT(x->upper);
            ADD_INPUT(x->value);
            break;
        }
        case kMirSetField: {
            struct MirSetField *x = MirGetSetField(instr);
            ADD_INPUT(x->object);
            ADD_INPUT(x->value);
            break;
        }
        case kMirClose: {
            struct MirClose *x = MirGetClose(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirReturn: {
            struct MirReturn *x = MirGetReturn(instr);
            ADD_INPUT(x->value);
            break;
        }
        case kMirBranch: {
            struct MirBranch *x = MirGetBranch(instr);
            ADD_INPUT(x->cond);
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *x = MirGetSwitch(instr);
            ADD_INPUT(x->discr);
            break;
        }
        default:
            break;
    }
    return inputs;

#undef ADD_INPUTS
#undef ADD_INPUT
}

MirRegister *pawMir_get_store(struct Compiler *C, struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirPhi:
            return &MirGetPhi(instr)->output;
        case kMirMove:
            return &MirGetMove(instr)->output;
        case kMirUpvalue:
            return &MirGetUpvalue(instr)->output;
        case kMirGlobal:
            return &MirGetGlobal(instr)->output;
        case kMirAllocLocal:
            return &MirGetAllocLocal(instr)->output;
        case kMirSetLocal:
            return &MirGetSetLocal(instr)->target;
        case kMirConstant:
            return &MirGetConstant(instr)->output;
        case kMirAggregate:
            return &MirGetAggregate(instr)->output;
        case kMirContainer:
            return &MirGetContainer(instr)->output;
        case kMirCall:
            return &MirGetCall(instr)->output;
        case kMirCast:
            return &MirGetCast(instr)->output;
        case kMirClosure:
            return &MirGetClosure(instr)->output;
        case kMirGetElement:
            return &MirGetGetElement(instr)->output;
        case kMirGetRange:
            return &MirGetGetRange(instr)->output;
        case kMirGetField:
            return &MirGetGetField(instr)->output;
        case kMirUnaryOp:
            return &MirGetUnaryOp(instr)->output;
        case kMirBinaryOp:
            return &MirGetBinaryOp(instr)->output;
        default:
            return NULL;
    }
}

static void indicate_access(struct Compiler *C, AccessMap *map, struct MirInstruction *instr, MirRegister r, MirBlock where)
{
    struct MirAccessList *accesses = *AccessMap_get(C, map, r);
    MirAccessList_push(C, accesses, ((struct MirAccess){
                                        .instr = instr,
                                        .b = where,
                                    }));
}

static void account_for_uses(struct Compiler *C, struct MirInstruction *instr, AccessMap *uses, MirBlock where)
{
    MirRegister *const *ppr;
    struct MirRegisterPtrList *loads = pawMir_get_loads(C, instr);
    K_LIST_FOREACH (loads, ppr) {
        indicate_access(C, uses, instr, **ppr, where);
    }
}

static void account_for_defs(struct Compiler *C, struct MirInstruction *instr, AccessMap *defs, MirBlock where)
{
    MirRegister const *store = pawMir_get_store(C, instr);
    if (store != NULL) {
        indicate_access(C, defs, instr, *store, where);
    }
}

typedef void (*AccountForAccesses)(struct Compiler *, struct MirInstruction *, AccessMap *, MirBlock);

static void collect_accesses(struct Mir *mir, AccessMap *map, AccountForAccesses cb)
{
    struct Compiler *C = mir->C;

    int index;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE (mir->registers, index, pdata) {
        AccessMap_insert(C, map, MIR_REG(index), MirAccessList_new(C));
    }

    struct MirBlockData **pblock;
    K_LIST_ENUMERATE (mir->blocks, index, pblock) {
        struct MirBlockData *block = *pblock;
        MirBlock const b = MIR_BB(index);

        struct MirInstruction **pinstr;
        K_LIST_FOREACH (block->joins, pinstr)
            cb(C, *pinstr, map, b);
        K_LIST_FOREACH (block->instructions, pinstr)
            cb(C, *pinstr, map, b);
    }
}

void pawMir_collect_per_instr_uses(struct Mir *mir, AccessMap *uses)
{
    collect_accesses(mir, uses, account_for_uses);
}

void pawMir_collect_per_instr_defs(struct Mir *mir, AccessMap *defs)
{
    collect_accesses(mir, defs, account_for_defs);
}

static void indicate_usedef(struct Compiler *C, UseDefMap *map, MirRegister r, MirBlock where)
{
    struct MirBlockList *blocks = *UseDefMap_get(C, map, r);

    MirBlock const *pb;
    K_LIST_FOREACH (blocks, pb) {
        if (MIR_BB_EQUALS(*pb, where))
            return;
    }
    MirBlockList_push(C, blocks, where);
}

static void indicate_usedefs(struct Compiler *C, struct MirInstruction *instr, UseDefMap *uses, UseDefMap *defs, MirBlock where)
{
    MirRegister *const *ppr;
    struct MirRegisterPtrList *ploads = pawMir_get_loads(C, instr);
    K_LIST_FOREACH (ploads, ppr)
        indicate_usedef(C, uses, **ppr, where);

    MirRegister const *pstore = pawMir_get_store(C, instr);
    if (pstore != NULL)
        indicate_usedef(C, defs, *pstore, where);
}

void pawMir_collect_per_block_usedefs(struct Mir *mir, UseDefMap *uses, UseDefMap *defs)
{
    struct Compiler *C = mir->C;

    int index;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE (mir->registers, index, pdata) {
        UseDefMap_insert(C, uses, MIR_REG(index), MirBlockList_new(C));
        UseDefMap_insert(C, defs, MIR_REG(index), MirBlockList_new(C));
    }

    struct MirBlockData **pblock;
    K_LIST_ENUMERATE (mir->blocks, index, pblock) {
        struct MirBlockData *block = *pblock;
        MirBlock const b = MIR_BB(index);

        struct MirInstruction **pinstr;
        K_LIST_FOREACH (block->joins, pinstr)
            indicate_usedefs(C, *pinstr, uses, defs, b);
        K_LIST_FOREACH (block->instructions, pinstr)
            indicate_usedefs(C, *pinstr, uses, defs, b);
    }
}

void pawMir_set_location(struct Mir *mir, struct MirLocationList *locations, MirId mid, int location)
{
    while (mid.value >= locations->count) {
        MirLocationList_push(mir->C, locations, -1);
    }
    MirLocationList_set(locations, mid.value, location);
}

struct MirLocationList *pawMir_compute_locations(struct Mir *mir)
{
    int location = 0;
    struct MirLocationList *locations = MirLocationList_new(mir->C);

    struct MirBlockData **pblock;
    struct MirInstruction **pinstr;
    K_LIST_FOREACH (mir->blocks, pblock) {
        struct MirBlockData *block = *pblock;
        pawMir_set_location(mir, locations, block->mid, location);

        // phi functions have the same location as the containing block
        K_LIST_FOREACH (block->joins, pinstr) {
            struct MirInstruction *instr = *pinstr;
            pawMir_set_location(mir, locations, instr->hdr.mid, location);
        }

        // each instruction bumps the location by 2 to allow insertion of new instructions
        // without breaking the ordering
        K_LIST_FOREACH (block->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            location += 2;
            pawMir_set_location(mir, locations, instr->hdr.mid, location);
        }
        location += 2;
    }
    return locations;
}

void pawMir_merge_redundant_blocks(struct Mir *mir)
{
    struct Compiler *C = mir->C;

    int index;
    struct MirBlockData *const *pbb;
    K_LIST_ENUMERATE (mir->blocks, index, pbb) {
        if (index < 2)
            continue; // keep entry block
        MirBlock const bto = MIR_BB(index);
        struct MirBlockData *to = *pbb;
        if (to->predecessors->count != 1)
            continue;
        MirBlock const bfrom = K_LIST_FIRST(to->predecessors);
        struct MirBlockData *from = mir_bb_data(mir, bfrom);
        if (from->successors->count != 1)
            continue;
        // remove goto and merge instruction lists
        --from->instructions->count;
        paw_assert(to->joins->count == 0);
        struct MirInstruction *const *pinstr;
        K_LIST_FOREACH (to->instructions, pinstr) {
            MirInstructionList_push(C, from->instructions, *pinstr);
        }
        // fix back references of removed block's successors
        MirBlock *pp;
        MirBlock const *ps;
        K_LIST_FOREACH (to->successors, ps) {
            struct MirBlockData *s = mir_bb_data(mir, *ps);
            K_LIST_FOREACH (s->predecessors, pp) {
                if (MIR_BB_EQUALS(*pp, bto))
                    *pp = bfrom;
            }
        }
        from->successors = to->successors;
        to->successors = MirBlockList_new(C);
        to->predecessors->count = 0;
    }

    pawMir_remove_unreachable_blocks(mir);
}

#if defined(PAW_DEBUG_EXTRA)

struct Printer {
    struct Compiler *C;
    struct Mir *mir;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define PRINT_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define PRINT_STRING(P, str) pawL_add_nstring(ENV(P), (P)->buf, (str)->text, (str)->length)
#define PRINT_FORMAT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define PRINT_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)

static void dump_instruction(struct Printer *, struct MirInstruction *);

static void indentation(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        PRINT_LITERAL(P, "  ");
    }
}

#define DUMP_FMT(P, ...) (indentation(P), pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__))
#define DUMP_MSG(P, msg) (indentation(P), pawL_add_string(ENV(P), (P)->buf, msg))

static void dump_instruction_list(struct Printer *P, struct MirInstructionList *list)
{
    for (int i = 0; i < list->count; ++i) {
        dump_instruction(P, list->data[i]);
    }
}

static void dump_instruction(struct Printer *P, struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirAllocLocal: {
            struct MirAllocLocal *t = MirGetAllocLocal(instr);
            DUMP_FMT(P, "alloc _%d <- %s\n", t->output.value, t->name->text);
            break;
        }
        case kMirFreeLocal: {
            struct MirFreeLocal *t = MirGetFreeLocal(instr);
            DUMP_FMT(P, "free _%d\n", t->reg.value);
            break;
        }
        case kMirPhi: {
            struct MirPhi *t = MirGetPhi(instr);
            DUMP_FMT(P, "_%d = phi [", t->output.value);
            for (int i = 0; i < t->inputs->count; ++i) {
                if (i > 0)
                    L_ADD_LITERAL(P->P, P->buf, ", ");
                pawL_add_fstring(P->P, P->buf, "_%d", MirRegisterList_get(t->inputs, i).value);
            }
            L_ADD_LITERAL(P->P, P->buf, "]\n");
            break;
        }
        case kMirMove: {
            struct MirMove *t = MirGetMove(instr);
            DUMP_FMT(P, "_%d = move _%d\n", t->output.value, t->target.value);
            break;
        }
        case kMirUpvalue: {
            struct MirUpvalue *t = MirGetUpvalue(instr);
            DUMP_FMT(P, "_%d = up%d\n", t->output.value, t->index);
            break;
        }
        case kMirGlobal: {
            struct MirGlobal *t = MirGetGlobal(instr);
            struct HirDecl *decl = pawHir_get_decl(P->C, IR_TYPE_DID(mir_reg_data(P->mir, t->output)->type));
            char const *type = pawIr_print_type(P->C, mir_reg_data(P->mir, t->output)->type);
            DUMP_FMT(P, "_%d = global %s (%s)\n", t->output.value, decl->hdr.name->text, type);
            --ENV(P->C)->top.p; // pop 'type'
            break;
        }
        case kMirConstant: {
            struct MirConstant *t = MirGetConstant(instr);
            DUMP_FMT(P, "_%d = const ", t->output.value);
            switch (t->b_kind) {
                case BUILTIN_UNIT:
                    L_ADD_LITERAL(P->P, P->buf, "()\n");
                    break;
                case BUILTIN_BOOL:
                    pawL_add_fstring(P->P, P->buf, "%s\n", V_TRUE(t->value) ? "true" : "false");
                    break;
                case BUILTIN_INT:
                    pawL_add_fstring(P->P, P->buf, "%I\n", V_INT(t->value));
                    break;
                case BUILTIN_FLOAT:
                    pawL_add_fstring(P->P, P->buf, "%f\n", V_FLOAT(t->value));
                    break;
                case BUILTIN_STR:
                    pawL_add_fstring(P->P, P->buf, "\"%s\"\n", V_TEXT(t->value));
                    break;
                default:
                    L_ADD_LITERAL(P->P, P->buf, "?\n");
            }
            break;
        }
        case kMirSetLocal: {
            struct MirSetLocal *t = MirGetSetLocal(instr);
            DUMP_FMT(P, "_%d = _%d\n", t->target.value, t->value.value);
            break;
        }
        case kMirSetUpvalue: {
            struct MirSetUpvalue *t = MirGetSetUpvalue(instr);
            DUMP_FMT(P, "up%d = _%d\n", t->index, t->value.value);
            break;
        }
        case kMirContainer: {
            struct MirContainer *t = MirGetContainer(instr);
            DUMP_FMT(P, "container _%d\n", t->output.value);
            break;
        }
        case kMirAggregate: {
            struct MirAggregate *t = MirGetAggregate(instr);
            DUMP_FMT(P, "aggregate _%d\n", t->output.value);
            break;
        }
        case kMirCall: {
            struct MirCall *t = MirGetCall(instr);
            DUMP_FMT(P, "_%d = _%d(", t->output.value, t->target.value);
            for (int i = 0; i < t->args->count; ++i) {
                if (i > 0)
                    L_ADD_LITERAL(P->P, P->buf, ", ");
                pawL_add_fstring(P->P, P->buf, "_%d", MirRegisterList_get(t->args, i).value);
            }
            L_ADD_LITERAL(P->P, P->buf, ")\n");
            break;
        }
        case kMirCast: {
            struct MirCast *t = MirGetCast(instr);
            DUMP_FMT(P, "_%d = ", t->output.value);
            switch (t->to) {
                case BUILTIN_BOOL:
                    L_ADD_LITERAL(P->P, P->buf, "(bool)");
                    break;
                case BUILTIN_INT:
                    L_ADD_LITERAL(P->P, P->buf, "(int)");
                    break;
                case BUILTIN_FLOAT:
                    L_ADD_LITERAL(P->P, P->buf, "(float)");
                    break;
                case BUILTIN_STR:
                    L_ADD_LITERAL(P->P, P->buf, "(str)");
                    break;
                default:
                    L_ADD_LITERAL(P->P, P->buf, "(?)");
            }
            pawL_add_fstring(P->P, P->buf, "_%d\n", t->target.value);
            break;
        }
        case kMirClose: {
            struct MirClose *t = MirGetClose(instr);
            DUMP_FMT(P, "close _%d\n", t->target.value);
            break;
        }
        case kMirClosure: {
            struct MirClosure *t = MirGetClosure(instr);
            DUMP_FMT(P, "closure _%d\n", t->output.value);
            break;
        }
        case kMirGetElement: {
            struct MirGetElement *t = MirGetGetElement(instr);
            DUMP_FMT(P, "_%d = _%d[_%d]\n", t->output.value, t->object.value, t->key.value);
            break;
        }
        case kMirSetElement: {
            struct MirSetElement *t = MirGetSetElement(instr);
            DUMP_FMT(P, "_%d[_%d] = _%d\n", t->object.value, t->key.value, t->value.value);
            break;
        }
        case kMirGetRange: {
            struct MirGetRange *t = MirGetGetRange(instr);
            DUMP_FMT(P, "_%d = _%d[_%d:_%d]\n", t->output.value, t->object.value, t->lower.value, t->upper.value);
            break;
        }
        case kMirSetRange: {
            struct MirSetRange *t = MirGetSetRange(instr);
            DUMP_FMT(P, "_%d[_%d:_%d] = _%d\n", t->object.value, t->lower.value, t->upper.value, t->value.value);
            break;
        }
        case kMirGetField: {
            struct MirGetField *t = MirGetGetField(instr);
            DUMP_FMT(P, "_%d = _%d.%d\n", t->output.value, t->object.value, t->index);
            break;
        }
        case kMirSetField: {
            struct MirSetField *t = MirGetSetField(instr);
            DUMP_FMT(P, "_%d.%d = _%d\n", t->object.value, t->index, t->value.value);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *t = MirGetUnaryOp(instr);
            DUMP_FMT(P, "_%d = %s _%d\n", t->output.value, paw_unop_name(t->op), t->val.value);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *t = MirGetBinaryOp(instr);
            DUMP_FMT(P, "_%d = %s _%d _%d\n", t->output.value, paw_binop_name(t->op), t->lhs.value, t->rhs.value);
            break;
        }
        case kMirReturn: {
            struct MirReturn *t = MirGetReturn(instr);
            DUMP_FMT(P, "return _%d\n", t->value.value);
            break;
        }
        case kMirBranch: {
            struct MirBranch *t = MirGetBranch(instr);
            DUMP_FMT(P, "branch _%d", t->cond.value);
            pawL_add_fstring(P->P, P->buf, " => [0: bb%d, 1: bb%d]\n", t->else_arm.value, t->then_arm.value);
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *t = MirGetSwitch(instr);
            DUMP_FMT(P, "switch _%d", t->discr.value);
            pawL_add_fstring(P->P, P->buf, " => [");
            for (int i = 0; i < t->arms->count; ++i) {
                if (i > 0)
                    L_ADD_LITERAL(P->P, P->buf, ", ");
                struct MirSwitchArm arm = MirSwitchArmList_get(t->arms, i);
                pawL_add_fstring(P->P, P->buf, "%d: bb%d", arm.value, arm.bid.value);
            }
            if (MIR_BB_EXISTS(t->otherwise)) {
                if (t->arms->count > 0)
                    L_ADD_LITERAL(P->P, P->buf, ", ");
                pawL_add_fstring(P->P, P->buf, "_: bb%d", t->otherwise.value);
            }
            L_ADD_LITERAL(P->P, P->buf, "]\n");
            break;
        }
        case kMirGoto: {
            DUMP_FMT(P, "goto bb%d\n", MirGetGoto(instr)->target.value);
            break;
        }
    }
}

static void dump_block(struct Printer *P, MirBlock bb)
{
    struct MirBlockData *block = mir_bb_data(P->mir, bb);
    DUMP_FMT(P, "bb%d {\n", bb.value);
    ++P->indent;

    dump_instruction_list(P, block->joins);
    dump_instruction_list(P, block->instructions);

    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_mir(struct Printer *P, struct Mir *mir)
{
    DUMP_MSG(P, "Mir {\n");
    ++P->indent;

    for (int i = 0; i < mir->blocks->count; ++i) {
        dump_block(P, MIR_BB(i));
    }

    --P->indent;
    DUMP_MSG(P, "}\n");
}

char const *pawMir_dump(struct Compiler *C, struct Mir *mir)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_mir(&(struct Printer){
                 .P = ENV(C),
                 .mir = mir,
                 .buf = &buf,
                 .C = C,
             },
             mir);

    for (int i = 0; i < mir->children->count; ++i) {
        struct Mir *child = MirBodyList_get(mir->children, i);
        dump_mir(&(struct Printer){
                     .P = ENV(C),
                     .mir = child,
                     .buf = &buf,
                     .C = C,
                 },
                 child);
    }

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

static void dump_block_list(struct Printer *P, struct MirBlockList *blocks)
{
    for (int i = 0; i < blocks->count; ++i) {
        if (i > 0)
            L_ADD_LITERAL(P->P, P->buf, ", ");
        pawL_add_fstring(P->P, P->buf, "bb%d", MirBlockList_get(blocks, i).value);
    }
}

static void dump_graph(struct Printer *P, struct Mir *mir)
{
    DUMP_MSG(P, "Mir {\n");
    ++P->indent;

    for (int i = 0; i < mir->blocks->count; ++i) {
        struct MirBlockData const *data = mir_bb_data(mir, MIR_BB(i));
        DUMP_FMT(P, "bb%d: {\n", i);
        ++P->indent;
        DUMP_FMT(P, "pred: [");
        dump_block_list(P, data->predecessors);
        L_ADD_LITERAL(P->P, P->buf, "]\n");
        DUMP_FMT(P, "succ: [");
        dump_block_list(P, data->successors);
        L_ADD_LITERAL(P->P, P->buf, "]\n");
        --P->indent;
        DUMP_MSG(P, "}\n");
    }

    --P->indent;
    DUMP_MSG(P, "}\n");
}

char const *pawMir_dump_graph(struct Compiler *C, struct Mir *mir)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_graph(&(struct Printer){
                   .P = ENV(C),
                   .mir = mir,
                   .buf = &buf,
                   .C = C,
               },
               mir);

    for (int i = 0; i < mir->children->count; ++i) {
        struct Mir *child = MirBodyList_get(mir->children, i);
        dump_graph(&(struct Printer){
                       .P = ENV(C),
                       .mir = mir,
                       .buf = &buf,
                       .C = C,
                   },
                   child);
    }

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

static void dump_info(struct Printer *P, struct Mir *mir)
{
    int index;
    struct MirCaptureInfo *pci;
    PRINT_LITERAL(P, "captured: [\n");
    K_LIST_ENUMERATE (mir->captured, index, pci) {
        if (index > 0)
            PRINT_LITERAL(P, ", ");
        PRINT_FORMAT(P, "_%d", pci->r.value);
    }
    PRINT_LITERAL(P, "]\n");
}

char const *pawMir_dump_info(struct Compiler *C, struct Mir *mir)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_info(&(struct Printer){
                  .P = ENV(C),
                  .mir = mir,
                  .buf = &buf,
                  .C = C,
              },
              mir);

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

#endif // PAW_DEBUG_EXTRA
