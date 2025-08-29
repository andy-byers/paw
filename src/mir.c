// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "mir.h"
#include "ir_type.h"
#include "layout.h"
#include "map.h"

struct Mir *pawMir_new(struct Compiler *C, Str *modname, struct SourceSpan span, Str *name, IrType *type, IrType *self, enum FnKind fn_kind, paw_Bool is_pub, paw_Bool is_poly)
{
    struct Mir *mir = P_ALLOC(C, NULL, 0, sizeof(*mir));
    *mir = (struct Mir){
        .pool = C->mir_pool,
        .is_poly = is_poly,
        .is_pub = is_pub,
        .fn_kind = fn_kind,
        .modname = modname,
        .name = name,
        .span = span,
        .type = type,
        .self = self,
        .P = ENV(C),
        .C = C,
    };
    mir->captured = MirCaptureList_new(mir);
    mir->kcache = pawMir_kcache_new(mir);
    mir->registers = MirRegisterDataList_new(mir);
    mir->locals = MirRegisterList_new(mir);
    mir->blocks = MirBlockDataList_new(mir);
    mir->upvalues = MirUpvalueList_new(mir);
    mir->children = MirBodyList_new(mir);
    return mir;
}

void pawMir_free(struct Mir *mir)
{
    // reclaim memory used by the MIR
    MirCaptureList_delete(mir, mir->captured);
    MirRegisterDataList_delete(mir, mir->registers);
    MirRegisterList_delete(mir, mir->locals);
    MirBlockDataList_delete(mir, mir->blocks);
    MirUpvalueList_delete(mir, mir->upvalues);
    MirBodyList_delete(mir, mir->children);
    pawMir_kcache_delete(mir, mir->kcache);
    P_ALLOC(mir->C, mir, sizeof(*mir), 0);
}

struct MirConstantCache *pawMir_kcache_new(struct Mir *mir)
{
    struct MirConstantCache *kcache = P_ALLOC(mir, NULL, 0, sizeof(struct MirConstantCache));
    *kcache = (struct MirConstantCache){
        .data = MirConstantDataList_new(mir),
        .ints = ValueMap_new_from(mir->C, mir->pool),
        .floats = ValueMap_new_from(mir->C, mir->pool),
        .strs = ValueMap_new_from(mir->C, mir->pool),
    };

#define LOADK(Value_, Kind_) (MirConstantDataList_push(mir, kcache->data, \
        (struct MirConstantData){.kind = (Kind_), .value = (Value_)}), \
        MIR_CONST(kcache->data->count - 1));
    kcache->unitk = LOADK(P2V(NULL), BUILTIN_UNIT);
    kcache->boolk[PAW_FALSE] = LOADK(I2V(PAW_FALSE), BUILTIN_BOOL);
    kcache->boolk[PAW_TRUE] = LOADK(I2V(PAW_TRUE), BUILTIN_BOOL);
#undef LOADK

    return kcache;
}

void pawMir_kcache_delete(struct Mir *mir, struct MirConstantCache *kcache)
{
    MirConstantDataList_delete(mir, kcache->data);
    ValueMap_delete(mir->C, kcache->ints);
    ValueMap_delete(mir->C, kcache->floats);
    ValueMap_delete(mir->C, kcache->strs);

    P_ALLOC(mir, kcache, sizeof(struct MirConstantCache), 0);
}

MirConstant pawMir_kcache_add(struct Mir *mir, struct MirConstantCache *kcache, Value k, enum BuiltinKind kind)
{
    if (kind == BUILTIN_UNIT) return kcache->unitk;
    if (kind == BUILTIN_BOOL) return kcache->boolk[V_TRUE(k)];
    ValueMap *map = kind == BUILTIN_FLOAT ? kcache->floats :
        kind == BUILTIN_STR ? kcache->strs : kcache->ints;
    Value const *pvalue = ValueMap_get(mir->C, map, k);
    if (pvalue != NULL) return MIR_CONST((int)V_INT(*pvalue));

    int const id = kcache->data->count;
    ValueMap_insert(mir->C, map, k, I2V(id));
    MirConstantDataList_push(mir, kcache->data,
        (struct MirConstantData){
            .kind = kind,
            .value = k,
        });

    return MIR_CONST(id);
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

MirProjection *MirProjection_new(struct Mir *mir)
{
    return P_ALLOC(mir->C, NULL, 0, sizeof(MirProjection));
}

MirInstruction *pawMir_new_instruction(struct Mir *mir)
{
    return P_ALLOC(mir->C, NULL, 0, sizeof(MirInstruction));
}

struct MirBlockData *pawMir_new_block(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct MirBlockData *block = P_ALLOC(C, NULL, 0, sizeof(struct MirBlockData));
    *block = (struct MirBlockData){
        .predecessors = MirBlockList_new(mir),
        .successors = MirBlockList_new(mir),
        .joins = MirInstructionList_new(mir),
        .instructions = MirInstructionList_new(mir),
        .mid = pawMir_next_id(mir),
    };
    return block;
}

struct MirPlace pawMir_copy_place(struct Mir *mir, struct MirPlace place)
{
    struct MirPlace copy = place;
    MirProjection *const *pp;
    copy.projection = MirProjectionList_new(mir);
    K_LIST_FOREACH(place.projection, pp) {
        MirProjection *p = MirProjection_new(mir);
        *p = **pp;
        MirProjectionList_push(mir, copy.projection, p);
    }
    return copy;
}

struct IrLayout pawMir_get_layout(struct Mir *mir, MirRegister r)
{
    return pawIr_compute_layout(mir->C, mir_reg_data(mir, r)->type);
}

static void AcceptNoop(struct MirVisitor *V, struct MirNoop *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptPhi(struct MirVisitor *V, struct MirPhi *t)
{
    pawMir_visit_place(V, t->output);
    pawMir_visit_place_list(V, t->inputs);
}

static void AcceptMove(struct MirVisitor *V, struct MirMove *t)
{
    pawMir_visit_place(V, t->output);
    pawMir_visit_place(V, t->target);
}

static void AcceptWrite(struct MirVisitor *V, struct MirWrite *t)
{
    pawMir_visit_place(V, t->target);
    pawMir_visit_place(V, t->value);
}

static void AcceptGlobal(struct MirVisitor *V, struct MirGlobal *t)
{
    pawMir_visit_place(V, t->output);
}

static void AcceptLoadConstant(struct MirVisitor *V, struct MirLoadConstant *t)
{
    pawMir_visit_place(V, t->output);
}

static void AcceptAggregate(struct MirVisitor *V, struct MirAggregate *t)
{
    pawMir_visit_place(V, t->output);
}

static void AcceptContainer(struct MirVisitor *V, struct MirContainer *t)
{
    pawMir_visit_place(V, t->output);
}

static void AcceptUpvalue(struct MirVisitor *V, struct MirUpvalue *t)
{
    pawMir_visit_place(V, t->output);
}

static void AcceptSetCapture(struct MirVisitor *V, struct MirSetCapture *t)
{
    pawMir_visit_place(V, t->target);
    pawMir_visit_place(V, t->value);
}

static void AcceptSetUpvalue(struct MirVisitor *V, struct MirSetUpvalue *t)
{
    pawMir_visit_place(V, t->value);
}

static void AcceptAllocLocal(struct MirVisitor *V, struct MirAllocLocal *t)
{
    pawMir_visit_place(V, t->output);
}

static void AcceptCall(struct MirVisitor *V, struct MirCall *t)
{
    pawMir_visit_place_list(V, t->outputs);
    pawMir_visit_place(V, t->target);
    pawMir_visit_place_list(V, t->args);
}

static void AcceptCast(struct MirVisitor *V, struct MirCast *t)
{
    pawMir_visit_place(V, t->output);
}

static void AcceptCapture(struct MirVisitor *V, struct MirCapture *t)
{
    pawMir_visit_place(V, t->target);
}

static void AcceptClose(struct MirVisitor *V, struct MirClose *t)
{
    pawMir_visit_place(V, t->target);
}

static void AcceptClosure(struct MirVisitor *V, struct MirClosure *t)
{
    pawMir_visit_place(V, t->output);
}

static void AcceptGetElement(struct MirVisitor *V, struct MirGetElement *t)
{
    pawMir_visit_place(V, t->output);
    pawMir_visit_place(V, t->object);
    pawMir_visit_place(V, t->key);
}

static void AcceptSetElement(struct MirVisitor *V, struct MirSetElement *t)
{
    pawMir_visit_place(V, t->object);
    pawMir_visit_place(V, t->key);
    pawMir_visit_place(V, t->value);
}

static void AcceptGetElementPtr(struct MirVisitor *V, struct MirGetElementPtr *t)
{
    pawMir_visit_place(V, t->output);
    pawMir_visit_place(V, t->object);
    pawMir_visit_place(V, t->key);
}

static void AcceptGetRange(struct MirVisitor *V, struct MirGetRange *t)
{
    pawMir_visit_place(V, t->output);
    pawMir_visit_place(V, t->object);
    pawMir_visit_place(V, t->lower);
    pawMir_visit_place(V, t->upper);
}

static void AcceptSetRange(struct MirVisitor *V, struct MirSetRange *t)
{
    pawMir_visit_place(V, t->object);
    pawMir_visit_place(V, t->lower);
    pawMir_visit_place(V, t->upper);
    pawMir_visit_place(V, t->value);
}

static void AcceptUnpack(struct MirVisitor *V, struct MirUnpack *t)
{
    pawMir_visit_place_list(V, t->outputs);
    pawMir_visit_place(V, t->object);
}

static void AcceptGetField(struct MirVisitor *V, struct MirGetField *t)
{
    pawMir_visit_place(V, t->output);
    pawMir_visit_place(V, t->object);
}

static void AcceptSetField(struct MirVisitor *V, struct MirSetField *t)
{
    pawMir_visit_place(V, t->object);
    pawMir_visit_place(V, t->value);
}

static void AcceptUnaryOp(struct MirVisitor *V, struct MirUnaryOp *t)
{
    pawMir_visit_place(V, t->val);
    pawMir_visit_place(V, t->output);
}

static void AcceptBinaryOp(struct MirVisitor *V, struct MirBinaryOp *t)
{
    pawMir_visit_place(V, t->lhs);
    pawMir_visit_place(V, t->rhs);
    pawMir_visit_place(V, t->output);
}

static void AcceptReturn(struct MirVisitor *V, struct MirReturn *t)
{
    pawMir_visit_place_list(V, t->values);
}

static void AcceptUnreachable(struct MirVisitor *V, struct MirUnreachable *t)
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
    pawMir_visit_place(V, t->cond);
}

static void AcceptSwitch(struct MirVisitor *V, struct MirSwitch *t)
{
    pawMir_visit_place(V, t->discr);
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

void pawMir_visit_place(struct MirVisitor *V, struct MirPlace p)
{
    if (V->VisitPlace(V, p))
        V->PostVisitPlace(V, p);
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
    struct MirBlockData const *block = mir_bb_data(V->mir, bb);
    paw_assert(block != NULL);

    if (!V->VisitBlock(V, bb))
        return;

    pawMir_visit_instruction_list(V, block->instructions);

    V->PostVisitBlock(V, bb);
}

paw_Bool default_visit_instruction(struct MirVisitor *V, struct MirInstruction *node) { return PAW_TRUE; }
paw_Bool default_visit_block(struct MirVisitor *V, MirBlock node) { return PAW_TRUE; }
paw_Bool default_visit_place(struct MirVisitor *V, struct MirPlace node) { return PAW_TRUE; }
void default_post_visit_instruction(struct MirVisitor *V, struct MirInstruction *node) {}
void default_post_visit_block(struct MirVisitor *V, MirBlock node) {}
void default_post_visit_place(struct MirVisitor *V, struct MirPlace node) {}

void pawMir_visitor_init(struct MirVisitor *V, struct Compiler *C, struct Mir *mir, void *ud)
{
    *V = (struct MirVisitor){
        .mir = mir,
        .ud = ud,
        .C = C,

        .VisitBlock = default_visit_block,
        .VisitInstruction = default_visit_instruction,
        .VisitPlace = default_visit_place,

        .PostVisitBlock = default_post_visit_block,
        .PostVisitInstruction = default_post_visit_instruction,
        .PostVisitPlace = default_post_visit_place,
    };
}

void pawMir_visit(struct MirVisitor *V)
{
    struct Mir *mir = V->mir;
    for (int i = 0; i < mir->blocks->count; ++i) {
        // callee will look up the block data struct using the provided block ID
        pawMir_visit_block(V, MIR_BB(i));
    }

//    // TODO: closures should be unnested so children can be visited separately
//    struct Mir *const *pchild;
//    K_LIST_FOREACH (mir->children, pchild) {
//        V->mir = *pchild;
//        pawMir_visit(V);
//    }
//    V->mir = mir;
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
DEFINE_LIST_VISITOR(place, Place)
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
DEFINE_MAP(struct Traversal, VisitedMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirBlock, void *)
DEFINE_MAP(struct Traversal, BlockMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirBlock, MirBlock)

static paw_Bool check_visited(struct Traversal *X, VisitedMap *visited, MirBlock bb)
{
    if (VisitedMap_get(X, visited, bb) != NULL)
        return PAW_TRUE;
    VisitedMap_insert(X, visited, bb, NULL);
    return PAW_FALSE;
}

static void reverse_blocks(struct MirBlockList *blocks)
{
    paw_assert(blocks->count > 0);
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
    struct MirBlockList *result = MirBlockList_new(X->mir);
    MirBlockList_reserve(X->mir, result, blocks->count);
    K_LIST_FOREACH (blocks, b) {
        MirBlockList_push(X->mir, result, *b);
    }
    return result;
}

static void visit_postorder(struct Traversal *X, VisitedMap *visited, struct VisitStack *stack, MirBlock bb)
{
    if (check_visited(X, visited, bb)) return;
    struct MirBlockData const *data = mir_bb_data(X->mir, bb);
    VisitStack_push(X, stack, (struct Successors){
        .successors = copy_blocks(X, data->successors),
        .block = bb,
    });
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
        .pool = pawP_pool_new(C, C->aux_stats),
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    struct VisitStack *stack = VisitStack_new(&X);
    X.visited = VisitedMap_new(&X);
    paw_assert(mir->blocks->count > 0);
    visit_postorder(&X, X.visited, stack, MIR_ROOT_BB);
    traverse_postorder(&X, X.visited, stack);

    struct MirBlockList *order = MirBlockList_new(mir);
    while (stack->count > 0) {
        struct Successors s = K_LIST_LAST(stack);
        VisitStack_pop(stack);
        traverse_postorder(&X, X.visited, stack);
        MirBlockList_push(mir, order, s.block);
    }
    reverse_blocks(order);
    pawP_pool_free(C, X.pool);
    return order;
}

static void renumber_ref(struct Traversal *X, BlockMap *map, MirBlock *pfrom)
{
    paw_assert(MIR_ID_EXISTS(*pfrom));
    *pfrom = *BlockMap_get(X, map, *pfrom);
}

static void renumber_or_clear_ref(struct Traversal *X, BlockMap *map, MirBlock *pfrom)
{
    paw_assert(MIR_ID_EXISTS(*pfrom));
    MirBlock const *pto = BlockMap_get(X, map, *pfrom);
    if (pto != NULL) {
        *pfrom = *pto;
    } else {
        *pfrom = MIR_INVALID_BB;
    }
}

static void prune_joins(struct Mir *mir, struct MirInstructionList *joins, struct MirInstructionList *instrs, int index)
{
    int ijoin;
    struct MirInstruction **pinstr;
    K_LIST_ENUMERATE (joins, ijoin, pinstr) {
        struct MirPhi *phi = MirGetPhi(*pinstr);
        paw_assert(phi->inputs->count > 1);
        // remove phi node input corresponding to unreachable basic block,
        // maintaining the 1-to-1 correspondence between phi node inputs and
        // predecessor basic blocks
        MirPlaceList_remove(phi->inputs, index);
        if (phi->inputs->count == 1) {
            // a phi node with a single input is really just a move: transfer
            // to the ".instructions" list
            *pinstr = pawMir_new_move(mir, phi->loc,
                                      phi->output, K_LIST_FIRST(phi->inputs));
            MirInstructionList_insert(mir, instrs, 0, *pinstr);
            MirInstructionList_swap_remove(joins, ijoin);
            --ijoin;
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
            if (bb != NULL)
                prune_joins(mir, bb->joins, bb->instructions, index - removed);
            ++removed;
            continue;
        }
        MirBlockList_set(blocks, index - removed, *pto);
    }
    blocks->count -= removed;
}

// Renumber basic blocks so that "mir_bb_data" continues to work after a call
// to pawMir_remove_unreachable_blocks
static void renumber_block_refs(struct Traversal *X, BlockMap *map, struct MirBlockData *data)
{
    rename_and_filter(X, map, data->predecessors, data);
    rename_and_filter(X, map, data->successors, NULL);
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
        .pool = pawP_pool_new(C, C->aux_stats),
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    // create a mapping from old to new basic block numbers
    BlockMap *map = BlockMap_new(&X);
    struct MirBlockList *order = pawMir_traverse_rpo(C, mir);

    int index;
    MirBlock const *pb;
    K_LIST_ENUMERATE (order, index, pb) {
        BlockMap_insert(&X, map, *pb, MIR_BB(index));
    }

    struct MirBlockDataList *blocks = MirBlockDataList_new(mir);
    MirBlockDataList_reserve(mir, blocks, order->count);
    K_LIST_FOREACH (order, pb) {
        struct MirBlockData *bb = mir_bb_data(mir, *pb);
        renumber_block_refs(&X, map, bb);
        MirBlockDataList_push(mir, blocks, bb);
    }

    mir->blocks = blocks;
    pawP_pool_free(C, X.pool);
}

MirPlacePtrList *pawMir_get_loads(struct Mir *mir, struct MirInstruction *instr)
{
#define ADD_INPUT(x) MirPlacePtrList_push(mir, inputs, &(x))
#define ADD_INPUTS(xs)      \
    K_LIST_FOREACH (xs, pp) \
        ADD_INPUT(*pp)

    struct MirPlace *pp;
    struct MirPlacePtrList *inputs = MirPlacePtrList_new(mir);

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
        case kMirWrite: {
            struct MirWrite *x = MirGetWrite(instr);
            ADD_INPUT(x->target);
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
        case kMirGetElementPtr: {
            struct MirGetElementPtr *x = MirGetGetElementPtr(instr);
            ADD_INPUT(x->object);
            ADD_INPUT(x->key);
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
        case kMirUnpack: {
            struct MirUnpack *x = MirGetUnpack(instr);
            ADD_INPUT(x->object);
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
        case kMirSetCapture: {
            struct MirSetCapture *x = MirGetSetCapture(instr);
            ADD_INPUT(x->target);
            ADD_INPUT(x->value);
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
        case kMirCapture: {
            struct MirCapture *x = MirGetCapture(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirClose: {
            struct MirClose *x = MirGetClose(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirReturn: {
            struct MirReturn *x = MirGetReturn(instr);
            ADD_INPUTS(x->values);
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

MirPlacePtrList *pawMir_get_stores(struct Mir *mir, struct MirInstruction *instr)
{
#define ADD_OUTPUT(x) MirPlacePtrList_push(mir, outputs, &(x))
#define ADD_OUTPUTS(xs)      \
    K_LIST_FOREACH (xs, pp) \
        ADD_OUTPUT(*pp)

    struct MirPlace *pp;
    struct MirPlacePtrList *outputs = MirPlacePtrList_new(mir);

    switch (MIR_KINDOF(instr)) {
        case kMirPhi:
            ADD_OUTPUT(MirGetPhi(instr)->output);
            break;
        case kMirMove:
            ADD_OUTPUT(MirGetMove(instr)->output);
            break;
        case kMirUpvalue:
            ADD_OUTPUT(MirGetUpvalue(instr)->output);
            break;
        case kMirGlobal:
            ADD_OUTPUT(MirGetGlobal(instr)->output);
            break;
        case kMirAllocLocal:
            ADD_OUTPUT(MirGetAllocLocal(instr)->output);
            break;
        case kMirLoadConstant:
            ADD_OUTPUT(MirGetLoadConstant(instr)->output);
            break;
        case kMirAggregate:
            ADD_OUTPUT(MirGetAggregate(instr)->output);
            break;
        case kMirContainer:
            ADD_OUTPUT(MirGetContainer(instr)->output);
            break;
        case kMirCall:
            ADD_OUTPUTS(MirGetCall(instr)->outputs);
            break;
        case kMirCast:
            ADD_OUTPUT(MirGetCast(instr)->output);
            break;
        case kMirClosure:
            ADD_OUTPUT(MirGetClosure(instr)->output);
            break;
        case kMirGetElementPtr:
            ADD_OUTPUT(MirGetGetElementPtr(instr)->output);
            break;
        case kMirGetElement:
            ADD_OUTPUT(MirGetGetElement(instr)->output);
            break;
        case kMirGetRange:
            ADD_OUTPUT(MirGetGetRange(instr)->output);
            break;
        case kMirUnpack:
            ADD_OUTPUTS(MirGetUnpack(instr)->outputs);
            break;
        case kMirGetField:
            ADD_OUTPUT(MirGetGetField(instr)->output);
            break;
        case kMirUnaryOp:
            ADD_OUTPUT(MirGetUnaryOp(instr)->output);
            break;
        case kMirBinaryOp:
            ADD_OUTPUT(MirGetBinaryOp(instr)->output);
            break;
        default:
            break;
    }
    return outputs;

#undef ADD_OUTPUTS
#undef ADD_OUTPUT
}

static void indicate_access(struct Mir *mir, AccessMap *map, struct MirInstruction *instr, MirRegister r, MirBlock where)
{
    struct MirAccessList *accesses = *AccessMap_get(mir, map, r);
    MirAccessList_push(mir->C, accesses, (struct MirAccess){
                                        .instr = instr,
                                        .b = where,
                                    });
}

static void account_for_uses(struct Mir *mir, struct MirInstruction *instr, AccessMap *uses, MirBlock where)
{
    struct MirPlace *const *ppp;
    struct MirPlacePtrList const *loads = pawMir_get_loads(mir, instr);
    K_LIST_FOREACH (loads, ppp) {
        if ((*ppp)->kind == MIR_PLACE_LOCAL)
            indicate_access(mir, uses, instr, (*ppp)->r, where);
    }
}

static void account_for_defs(struct Mir *mir, struct MirInstruction *instr, AccessMap *defs, MirBlock where)
{
    struct MirPlace *const *ppp;
    MirPlacePtrList const *stores = pawMir_get_stores(mir, instr);
    K_LIST_FOREACH (stores, ppp)
        indicate_access(mir, defs, instr, (*ppp)->r, where);
}

typedef void (*AccountForAccesses)(struct Mir *, struct MirInstruction *, AccessMap *, MirBlock);

static void collect_accesses(struct Mir *mir, AccessMap *map, AccountForAccesses cb)
{
    struct Compiler *C = mir->C;

    int index;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE (mir->registers, index, pdata) {
        AccessMap_insert(mir, map, MIR_REG(index), MirAccessList_new(C));
    }

    struct MirBlockData *const *pblock;
    K_LIST_ENUMERATE (mir->blocks, index, pblock) {
        struct MirBlockData const *block = *pblock;
        MirBlock const b = MIR_BB(index);

        struct MirInstruction *const *pinstr;
        K_LIST_FOREACH (block->joins, pinstr)
            cb(mir, *pinstr, map, b);
        K_LIST_FOREACH (block->instructions, pinstr)
            cb(mir, *pinstr, map, b);
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

static void indicate_usedef(struct Mir *mir, UseDefMap *map, MirRegister r, MirBlock where)
{
    struct MirBlockList *blocks = *UseDefMap_get(mir, map, r);

    MirBlock const *pb;
    K_LIST_FOREACH (blocks, pb) {
        if (MIR_ID_EQUALS(*pb, where))
            return;
    }
    MirBlockList_push(mir, blocks, where);
}

static void indicate_usedefs(struct Mir *mir, struct MirInstruction *instr, UseDefMap *uses, UseDefMap *defs, MirBlock where)
{
    struct Compiler *C = mir->C;

    struct MirPlace *const *ppp;
    struct MirPlacePtrList const *loads = pawMir_get_loads(mir, instr);
    K_LIST_FOREACH (loads, ppp) {
        if ((*ppp)->kind == MIR_PLACE_LOCAL)
            indicate_usedef(mir, uses, (*ppp)->r, where);
    }

    if (MirIsMove(instr)) {
        struct MirMove *move = MirGetMove(instr);
        struct MirRegisterData *data = mir_reg_data(mir, move->output.r);
        if (data->is_captured) {
            // The output register is captured, meaning the SSA construction pass will transform this move into
            // a SetCapture instruction. SetCapture accepts 2 input operands.
            indicate_usedef(mir, uses, move->output.r, where);
            return;
        }
    }

    struct MirPlacePtrList const *stores = pawMir_get_stores(mir, instr);
    K_LIST_FOREACH (stores, ppp)
        indicate_usedef(mir, defs, (*ppp)->r, where);
}

void pawMir_collect_per_block_usedefs(struct Mir *mir, UseDefMap *uses, UseDefMap *defs)
{
    int index;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE (mir->registers, index, pdata) {
        UseDefMap_insert(mir, uses, MIR_REG(index), MirBlockList_new(mir));
        UseDefMap_insert(mir, defs, MIR_REG(index), MirBlockList_new(mir));
    }

    struct MirBlockData *const *pblock;
    K_LIST_ENUMERATE (mir->blocks, index, pblock) {
        struct MirBlockData *block = *pblock;
        MirBlock const b = MIR_BB(index);

        struct MirInstruction **pinstr;
        K_LIST_FOREACH (block->joins, pinstr)
            indicate_usedefs(mir, *pinstr, uses, defs, b);
        K_LIST_FOREACH (block->instructions, pinstr)
            indicate_usedefs(mir, *pinstr, uses, defs, b);
    }
}

void pawMir_set_location(struct Mir *mir, struct MirLocationList *locations, MirId mid, int location)
{
    while (mid.value >= locations->count) {
        MirLocationList_push(mir, locations, -1);
    }
    MirLocationList_set(locations, mid.value, location);
}

struct MirLocationList *pawMir_compute_locations(struct Mir *mir)
{
    int location = 0;
    struct MirLocationList *locations = MirLocationList_new(mir);

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

DEFINE_MAP(struct Mir, BlockCounts, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirBlock, int)

static MirBlock get_common_block(MirBlockList *blocks)
{
    MirBlock const first = K_LIST_FIRST(blocks);
    for (int i = 0; i < blocks->count; ++i) {
        if (!MIR_ID_EQUALS(first, MirBlockList_get(blocks, i)))
           return MIR_INVALID_BB;
    }
    return first;
}

static void detach_unused_block(struct Mir *mir, struct MirBlockData *bb)
{
    paw_assert(bb->joins->count == 0 && bb->instructions->count > 0);
    K_LIST_FIRST(bb->instructions) = K_LIST_LAST(bb->instructions);
    K_LIST_FIRST(bb->instructions)->hdr.kind = kMirUnreachable;
    bb->instructions->count = 1;
    bb->predecessors->count = 0;
    bb->successors->count = 0;
}

static void merge_adjacent_blocks(struct Mir *mir, MirBlock bfrom, struct MirBlockData *from, MirBlock bto, struct MirBlockData *to)
{
    struct MirInstruction *unused_term = K_LIST_LAST(from->instructions);
    MirBlockList *unused_succ = from->successors;

    // remove goto and merge instruction lists
    --from->instructions->count;
    paw_assert(to->joins->count == 0);
    struct MirInstruction *const *pinstr;
    K_LIST_FOREACH (to->instructions, pinstr)
        MirInstructionList_push(mir, from->instructions, *pinstr);

    // fix back references of removed block's successors
    MirBlock *pp;
    MirBlock const *ps;
    K_LIST_FOREACH (to->successors, ps) {
        struct MirBlockData *s = mir_bb_data(mir, *ps);
        K_LIST_FOREACH (s->predecessors, pp) {
            if (MIR_ID_EQUALS(*pp, bto)) *pp = bfrom;
        }
    }
    from->successors = to->successors;

    // detach the unused basic block, keeping it valid and being careful to avoid
    // modifying components transferred to "from"
    K_LIST_LAST(to->instructions) = unused_term;
    to->successors = unused_succ;
    detach_unused_block(mir, to);
}

static void thread_jump_through(struct Mir *mir, MirBlock bfrom, struct MirBlockData *from, MirBlock bto, struct MirBlockData *to)
{
    int const target = mir_which_pred(mir, bto, bfrom);

    {
        // modify references to "bfrom" so they point to "bto"
        MirBlock const *ppred;
        K_LIST_FOREACH (from->predecessors, ppred) {
            struct MirBlockData const *predecessor = mir_bb_data(mir, *ppred);
            int const index = mir_which_succ(mir, *ppred, bfrom);
            MirBlockList_set(predecessor->successors, index, bto);
        }
    }

    {
        // rewrite predecessors of successor
        int index;
        MirBlock const *ppred, *ppred2;
        MirBlockList *predecessors = MirBlockList_new(mir);
        K_LIST_ENUMERATE (to->predecessors, index, ppred) {
            if (index == target) {
                K_LIST_FOREACH (from->predecessors, ppred2)
                    MirBlockList_push(mir, predecessors, *ppred2);
            } else {
                MirBlockList_push(mir, predecessors, *ppred);
            }
        }
        to->predecessors = predecessors;
    }

    detach_unused_block(mir, from);
    MIR_VALIDATE_GRAPH(mir);
}

void pawMir_merge_redundant_blocks(struct Mir *mir)
{
    paw_Bool changed;
    do {
        changed = PAW_FALSE;
        MirBlockList *order = pawMir_traverse_rpo(mir->C, mir);
        for (int i = 1; i < order->count; ++i) {
            MirBlock const p = MirBlockList_get(order, i);
            struct MirBlockData *pred = mir_bb_data(mir, p);
            if (pred->predecessors->count == 0) continue; // disconnected
            struct MirInstruction *term = K_LIST_LAST(pred->instructions);
            if (MirIsBranch(term) || MirIsSwitch(term)) {
                // Attempt to convert a branch into a jump. This is possible when all cases
                // jump to the same basic block.
                MirBlock const common = get_common_block(pred->successors);
                if (MIR_ID_EXISTS(common)) {
                    struct MirBlockData *succ = mir_bb_data(mir, common);
                    if (succ->joins->count == 0) {
                        term->hdr.kind = kMirGoto; // convert to jump
                        K_LIST_FIRST(pred->successors) = common;
                        pred->successors->count = 1;
                        changed = PAW_TRUE;
                        // remove redundant copies of "common"
                        int count = 0;
                        MirBlock const *pp;
                        MirBlockList *predecessors = MirBlockList_new(mir);
                        K_LIST_FOREACH (succ->predecessors, pp) {
                            if (!MIR_ID_EQUALS(p, *pp) || count++ == 0)
                                MirBlockList_push(mir, predecessors, *pp);
                        }
                        MirBlockList_delete(mir, succ->predecessors);
                        succ->predecessors = predecessors;
                    }
                }
            }
            if (MirIsGoto(term)) {
                // Attempt to simplify a jump from "p" to "s". Note that the edge "p -> s" may
                // have been created by the branch-to-jump conversion above.
                paw_assert(pred->successors->count == 1);
                MirBlock const s = K_LIST_LAST(pred->successors);
                struct MirBlockData *succ = mir_bb_data(mir, s);
                if (succ->predecessors->count == 1) {
                    // "p" and "s" can be trivially merged
                    merge_adjacent_blocks(mir, p, pred, s, succ);
                    changed = PAW_TRUE;
                } else if (pred->joins->count == 0 && succ->joins->count == 0
                        && pred->instructions->count == 1) {
                    // predecessor is empty except for the jump
                    thread_jump_through(mir, p, pred, s, succ);
                    changed = PAW_TRUE;
                } else if (succ->joins->count == 0 && succ->instructions->count == 1
                        && !MirIsGoto(K_LIST_LAST(succ->instructions))) {
                    // successor is empty except for a branch or exit
                    MirBlockList *unused = pred->successors;
                    // overwrite jump with other terminator
                    K_LIST_LAST(pred->instructions) = K_LIST_LAST(succ->instructions);
                    pred->successors = succ->successors;
                    // successor is unreachable and leads nowhere
                    K_LIST_LAST(succ->instructions) = term;
                    term->hdr.kind = kMirUnreachable;
                    succ->successors = unused;
                    detach_unused_block(mir, succ);
                    changed = PAW_TRUE;
                }
            }
        }
    } while (changed);

    pawMir_remove_unreachable_blocks(mir);
    MIR_VALIDATE_GRAPH(mir);
}


struct Printer {
    struct Compiler *C;
    struct MirBlockData *bb;
    struct Mir *mir;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define PRINT_LITERAL(P_, Lit_) L_ADD_LITERAL(ENV(P_), (P_)->buf, Lit_)
#define PRINT_STRING(P_, Str_) pawL_add_nstring(ENV(P_), (P_)->buf, (Str_)->text, (Str_)->length)
#define PRINT_FORMAT(P_, ...) pawL_add_fstring(ENV(P_), (P_)->buf, __VA_ARGS__)
#define PRINT_CHAR(P_, Char_) pawL_add_char(ENV(P_), (P_)->buf, Char_)

static void dump_instruction(struct Printer *, struct MirInstruction *);

static void indentation(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        PRINT_LITERAL(P, "  ");
    }
}

#define DUMP_FORMAT(P_, ...) (indentation(P_), PRINT_FORMAT(P_, __VA_ARGS__))
#define DUMP_LITERAL(P_, Lit_) (indentation(P_), PRINT_LITERAL(P_, Lit_))

static char const *unop_name(enum MirUnaryOpKind op)
{
    switch (op) {
        case MIR_UNARY_STRLEN:
            return "STRLEN";
        case MIR_UNARY_LISTLEN:
            return "LISTLEN";
        case MIR_UNARY_MAPLEN:
            return "MAPLEN";
        case MIR_UNARY_INEG:
            return "INEG";
        case MIR_UNARY_FNEG:
            return "FNEG";
        case MIR_UNARY_NOT:
            return "NOT";
        case MIR_UNARY_IBITNOT:
            return "IBITNOT";
    }
}

static char const *binop_name(enum MirBinaryOpKind op)
{
    switch (op) {
        case MIR_BINARY_CEQ:
            return "CEQ";
        case MIR_BINARY_CNE:
            return "CNE";
        case MIR_BINARY_CLT:
            return "CLT";
        case MIR_BINARY_CLE:
            return "CLE";
        case MIR_BINARY_IEQ:
            return "IEQ";
        case MIR_BINARY_INE:
            return "INE";
        case MIR_BINARY_ILT:
            return "ILT";
        case MIR_BINARY_ILE:
            return "ILE";
        case MIR_BINARY_FEQ:
            return "FEQ";
        case MIR_BINARY_FNE:
            return "FNE";
        case MIR_BINARY_FLT:
            return "FLT";
        case MIR_BINARY_FLE:
            return "FLE";
        case MIR_BINARY_STREQ:
            return "STREQ";
        case MIR_BINARY_STRNE:
            return "STRNE";
        case MIR_BINARY_STRLT:
            return "STRLT";
        case MIR_BINARY_STRLE:
            return "STRLE";
        case MIR_BINARY_STRCAT:
            return "STRCAT";
        case MIR_BINARY_LISTCAT:
            return "LISTCAT";
        case MIR_BINARY_IADD:
            return "IADD";
        case MIR_BINARY_ISUB:
            return "ISUB";
        case MIR_BINARY_IMUL:
            return "IMUL";
        case MIR_BINARY_IDIV:
            return "IDIV";
        case MIR_BINARY_IMOD:
            return "IMOD";
        case MIR_BINARY_FADD:
            return "FADD";
        case MIR_BINARY_FSUB:
            return "FSUB";
        case MIR_BINARY_FMUL:
            return "FMUL";
        case MIR_BINARY_FDIV:
            return "FDIV";
        case MIR_BINARY_FMOD:
            return "FMOD";
        case MIR_BINARY_IBITAND:
            return "IBITAND";
        case MIR_BINARY_IBITOR:
            return "IBITOR";
        case MIR_BINARY_IBITXOR:
            return "IBITXOR";
        case MIR_BINARY_ISHL:
            return "ISHL";
        case MIR_BINARY_ISHR:
            return "ISHR";
    }
}

static void print_constant(struct Printer *P, Value value, enum BuiltinKind kind)
{
    switch (kind) {
        case BUILTIN_UNIT:
            PRINT_LITERAL(P, "()");
            break;
        case BUILTIN_BOOL:
            PRINT_FORMAT(P, "%s", V_TRUE(value) ? "true" : "false");
            break;
        case BUILTIN_CHAR:
            PRINT_FORMAT(P, "%d", V_CHAR(value));
            break;
        case BUILTIN_INT:
            PRINT_FORMAT(P, "%I", V_INT(value));
            break;
        case BUILTIN_FLOAT:
            PRINT_FORMAT(P, "%f", V_FLOAT(value));
            break;
        default:
            paw_assert(kind == BUILTIN_STR);
            PRINT_FORMAT(P, "\"%s\"", V_TEXT(value));
    }
}

static void print_place(struct Printer *P, struct MirPlace place)
{
    if (place.kind == MIR_PLACE_LOCAL) {
        PRINT_FORMAT(P, "_%d", place.r.value);
        if (mir_reg_data(P->mir, place.r)->info == MIR_REGINFO_ARGUMENT)
            PRINT_CHAR(P, '^');
    } else if (place.kind == MIR_PLACE_CONSTANT) {
        struct MirConstantData const *k = mir_const_data(P->mir, place.k);
        print_constant(P, k->value, k->kind);
    } else {
        PRINT_FORMAT(P, "up%d", place.up);
    }
    if (place.projection == NULL)
        return;

    int index;
    MirProjection *const *pp;
    K_LIST_ENUMERATE (place.projection, index, pp) {
        switch (MIR_KINDOF(*pp)) {
            case kMirDeref:
                PRINT_LITERAL(P, ".*");
                break;
            case kMirField: {
                struct MirField *p = MirGetField(*pp);
                PRINT_FORMAT(P, ".%d", p->index);
                break;
            }
            case kMirIndex: {
                struct MirIndex *p = MirGetIndex(*pp);
                PRINT_CHAR(P, '[');
                print_place(P, p->index);
                PRINT_CHAR(P, ']');
                break;
            }
            case kMirRange: {
                struct MirRange *p = MirGetRange(*pp);
                PRINT_CHAR(P, '[');
                print_place(P, p->lower);
                PRINT_LITERAL(P, "..");
                print_place(P, p->upper);
                PRINT_CHAR(P, ']');
                break;
            }
        }
    }
}

static void print_place_list(struct Printer *P, MirPlaceList *places)
{
    int index;
    struct MirPlace *p;
    K_LIST_ENUMERATE (places, index, p) {
        if (index > 0)
            PRINT_LITERAL(P, ", ");
        print_place(P, *p);
    }
}

static void dump_instruction_list(struct Printer *P, struct MirInstructionList *list)
{
    for (int i = 0; i < list->count; ++i) {
        dump_instruction(P, list->data[i]);
    }
}

static void dump_instruction(struct Printer *P, struct MirInstruction *instr)
{
    indentation(P);

    switch (MIR_KINDOF(instr)) {
        case kMirAllocLocal: {
            struct MirAllocLocal *t = MirGetAllocLocal(instr);
            struct IrLayout layout = pawMir_get_layout(P->mir, t->output.r);
            IrType *type = mir_reg_data(P->mir, t->output.r)->type;
            PRINT_LITERAL(P, "alloc ");
            print_place(P, t->output);
            PRINT_FORMAT(P, " [size %d] (\"%s\")", layout.size, pawIr_print_type(P->C, type));
            break;
        }
        case kMirNoop: {
            PRINT_LITERAL(P, "noop");
            break;
        }
        case kMirPhi: {
            struct MirPhi *t = MirGetPhi(instr);
            print_place(P, t->output);
            PRINT_FORMAT(P, " = phi [", t->output.r.value);
            print_place_list(P, t->inputs);
            PRINT_LITERAL(P, "]");
            break;
        }
        case kMirMove: {
            struct MirMove *t = MirGetMove(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = move ");
            print_place(P, t->target);
            break;
        }
        case kMirWrite: {
            struct MirWrite *t = MirGetWrite(instr);
            PRINT_LITERAL(P, "write ");
            print_place(P, t->target);
            PRINT_CHAR(P, ' ');
            print_place(P, t->value);
            break;
        }
        case kMirUpvalue: {
            struct MirUpvalue *t = MirGetUpvalue(instr);
            PRINT_LITERAL(P, " upvalue ");
            print_place(P, t->output);
            PRINT_FORMAT(P, " = up%d", t->index);
            break;
        }
        case kMirGlobal: {
            struct MirGlobal *t = MirGetGlobal(instr);
            // TODO: HIR not available, probably crashes
            struct HirDecl *decl = pawHir_get_decl(P->C->hir, IR_TYPE_DID(mir_reg_data(P->mir, t->output.r)->type));
            char const *type = pawIr_print_type(P->C, mir_reg_data(P->mir, t->output.r)->type);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = global ");
            PRINT_STRING(P, HirGetFnDecl(decl)->ident.name);
            PRINT_FORMAT(P, " (%s)", type);
            break;
        }
        case kMirLoadConstant: {
            struct MirLoadConstant *t = MirGetLoadConstant(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = const ");
            struct MirConstantData const *kdata = mir_const_data(P->mir, t->k);
            print_constant(P, kdata->value, kdata->kind);
            break;
        }
        case kMirSetUpvalue: {
            struct MirSetUpvalue *t = MirGetSetUpvalue(instr);
            PRINT_FORMAT(P, "setupvalue up%d = ", t->index);
            print_place(P, t->value);
            break;
        }
        case kMirContainer: {
            struct MirContainer *t = MirGetContainer(instr);
            PRINT_LITERAL(P, "container ");
            print_place(P, t->output);
            break;
        }
        case kMirAggregate: {
            struct MirAggregate *t = MirGetAggregate(instr);
            PRINT_LITERAL(P, "aggregate ");
            print_place(P, t->output);
            PRINT_FORMAT(P, " (%d fields)", t->nfields);
            break;
        }
        case kMirCall: {
            struct MirCall *t = MirGetCall(instr);
            print_place_list(P, t->outputs);
            PRINT_LITERAL(P, " = ");
            print_place(P, t->target);
            PRINT_CHAR(P, '(');
            print_place_list(P, t->args);
            PRINT_LITERAL(P, ")");
            break;
        }
        case kMirCast: {
            struct MirCast *t = MirGetCast(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = ");
            switch (t->to) {
                case BUILTIN_BOOL:
                    PRINT_LITERAL(P, "(bool)");
                    break;
                case BUILTIN_INT:
                    PRINT_LITERAL(P, "(int)");
                    break;
                case BUILTIN_FLOAT:
                    PRINT_LITERAL(P, "(float)");
                    break;
                case BUILTIN_STR:
                    PRINT_LITERAL(P, "(str)");
                    break;
                default:
                    PRINT_LITERAL(P, "(?)");
            }
            print_place(P, t->target);
            break;
        }
        case kMirCapture: {
            struct MirCapture *t = MirGetCapture(instr);
            PRINT_LITERAL(P, "capture ");
            print_place(P, t->target);
            break;
        }
        case kMirSetCapture: {
            struct MirSetCapture *t = MirGetSetCapture(instr);
            PRINT_LITERAL(P, " setcapture ");
            print_place(P, t->target);
            PRINT_CHAR(P, ' ');
            print_place(P, t->value);
            break;
        }
        case kMirClose: {
            struct MirClose *t = MirGetClose(instr);
            PRINT_LITERAL(P, "close");
            print_place(P, t->target);
            break;
        }
        case kMirClosure: {
            struct MirClosure *t = MirGetClosure(instr);
            PRINT_LITERAL(P, "closure ");
            print_place(P, t->output);
            break;
        }
        case kMirGetElement: {
            struct MirGetElement *t = MirGetGetElement(instr);
            PRINT_FORMAT(P, "get _%d = _%d[_%d]", t->output.r.value, t->object.r.value, t->key.r.value);
            break;
        }
        case kMirSetElement: {
            struct MirSetElement *t = MirGetSetElement(instr);
            PRINT_FORMAT(P, "set _%d[_%d] = _%d", t->object.r.value, t->key.r.value, t->value.r.value);
            break;
        }
        case kMirGetElementPtr: {
            struct MirGetElementPtr *t = MirGetGetElementPtr(instr);
            PRINT_FORMAT(P, "get _%d = &_%d[_%d]", t->output.r.value, t->object.r.value, t->key.r.value);
            break;
        }
        case kMirGetRange: {
            struct MirGetRange *t = MirGetGetRange(instr);
            PRINT_FORMAT(P, "_%d = _%d[_%d:_%d]", t->output.r.value, t->object.r.value, t->lower.r.value, t->upper.r.value);
            break;
        }
        case kMirSetRange: {
            struct MirSetRange *t = MirGetSetRange(instr);
            PRINT_FORMAT(P, "_%d[_%d:_%d] = _%d", t->object.r.value, t->lower.r.value, t->upper.r.value, t->value.r.value);
            break;
        }
        case kMirUnpack: {
            struct MirUnpack *t = MirGetUnpack(instr);
            print_place_list(P, t->outputs);
            PRINT_LITERAL(P, " = unpack ");
            print_place(P, t->object);
            PRINT_FORMAT(P, " at %d", t->offset);
            break;
        }
        case kMirGetField: {
            struct MirGetField *t = MirGetGetField(instr);
            PRINT_LITERAL(P, "get ");
            print_place(P, t->output);
            PRINT_LITERAL(P, " = ");
            print_place(P, t->object);
            PRINT_FORMAT(P, ".%d", t->index);
            break;
        }
        case kMirSetField: {
            struct MirSetField *t = MirGetSetField(instr);
            PRINT_LITERAL(P, "set ");
            print_place(P, t->object);
            PRINT_FORMAT(P, ".%d = ", t->index);
            print_place(P, t->value);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *t = MirGetUnaryOp(instr);
            print_place(P, t->output);
            PRINT_FORMAT(P, " = %s ", unop_name(t->op));
            print_place(P, t->val);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *t = MirGetBinaryOp(instr);
            print_place(P, t->output);
            PRINT_FORMAT(P, " = %s ", binop_name(t->op));
            print_place(P, t->lhs);
            PRINT_CHAR(P, ' ');
            print_place(P, t->rhs);
            break;
        }
        case kMirUnreachable:
            PRINT_LITERAL(P, "unreachable");
            break;
        case kMirReturn: {
            struct MirReturn *t = MirGetReturn(instr);
            PRINT_LITERAL(P, "return ");
            if (t->values->count > 1) {
                PRINT_CHAR(P, '(');
                print_place_list(P, t->values);
                PRINT_CHAR(P, ')');
            } else {
                print_place_list(P, t->values);
            }
            break;
        }
        case kMirBranch: {
            struct MirBranch *t = MirGetBranch(instr);
            PRINT_LITERAL(P, "branch ");
            print_place(P, t->cond);
            PRINT_FORMAT(P, " => [0: bb%d, 1: bb%d]", MirBlockList_get(P->bb->successors, 0),
                    MirBlockList_get(P->bb->successors, 1));
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *t = MirGetSwitch(instr);
            PRINT_LITERAL(P, "switch ");
            print_place(P, t->discr);
            PRINT_LITERAL(P, " => [");
            for (int i = 0; i < t->arms->count; ++i) {
                if (i > 0) PRINT_LITERAL(P, ", ");
                struct MirSwitchArm arm = MirSwitchArmList_get(t->arms, i);
                PRINT_FORMAT(P, "k%d: bb%d", arm.k.value, MirBlockList_get(P->bb->successors, i).value);
            }
            if (t->has_otherwise) {
                if (t->arms->count > 0)
                    PRINT_LITERAL(P, ", ");
                PRINT_FORMAT(P, "_: bb%d", K_LIST_LAST(P->bb->successors).value);
            }
            PRINT_LITERAL(P, "]");
            break;
        }
        case kMirGoto: {
            PRINT_FORMAT(P, "goto bb%d", K_LIST_FIRST(P->bb->successors).value);
            break;
        }
    }
    PRINT_CHAR(P, '\n');
}

static void dump_block(struct Printer *P, MirBlock bb)
{
    struct MirBlockData *block = mir_bb_data(P->mir, bb);
    DUMP_FORMAT(P, "bb%d {\n", bb.value);
    ++P->indent;

    P->bb = block;
    dump_instruction_list(P, block->joins);
    dump_instruction_list(P, block->instructions);

    --P->indent;
    DUMP_LITERAL(P, "}\n");
}

static void dump_mir(struct Printer *P, struct Mir *mir)
{
    DUMP_LITERAL(P, "Mir {\n");
    ++P->indent;

    for (int i = 0; i < mir->blocks->count; ++i) {
        dump_block(P, MIR_BB(i));
    }

    DUMP_FORMAT(P, "registers {\n");
    ++P->indent;
    for (int i = 0; i < mir->registers->count; ++i) {
        struct MirRegisterData const data = MirRegisterDataList_get(mir->registers, i);
        DUMP_FORMAT(P, "_%d: %s\n", i, pawIr_print_type(P->C, data.type));
    }
    --P->indent;
    DUMP_FORMAT(P, "}\n");

    DUMP_FORMAT(P, "locals {\n");
    ++P->indent;
    for (int i = 0; i < mir->locals->count; ++i) {
        MirRegister const r = MirRegisterList_get(mir->locals, i);
        if (MIR_ID_EXISTS(r)) {
            IrType *type = mir_reg_data(mir, r)->type;
            DUMP_FORMAT(P, "%d: _%d: %s\n", i, r.value, pawIr_print_type(P->C, type));
        } else {
            DUMP_FORMAT(P, "%d: NULL\n", i);
        }
    }
    --P->indent;
    DUMP_FORMAT(P, "}\n");

    DUMP_FORMAT(P, "upvalues {\n");
    ++P->indent;
    for (int i = 0; i < mir->upvalues->count; ++i) {
        struct MirUpvalueInfo up = MirUpvalueList_get(mir->upvalues, i);
        DUMP_FORMAT(P, "up%d (%s %d): %s\n", i, up.is_local ? "local" : "nonlocal", up.index, pawIr_print_type(P->C, up.type));
    }
    --P->indent;
    DUMP_FORMAT(P, "}\n");

    DUMP_FORMAT(P, "captured {\n");
    ++P->indent;
    for (int i = 0; i < mir->captured->count; ++i) {
        struct MirCaptureInfo ci = MirCaptureList_get(mir->captured, i);
        DUMP_FORMAT(P, "%d: _%d\n", i, ci.r.value);
    }
    --P->indent;
    DUMP_FORMAT(P, "}\n");

    --P->indent;
    DUMP_LITERAL(P, "}\n");
}

char const *pawMir_dump(struct Mir *mir)
{
    struct Compiler *C = mir->C;

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

    Str const *s = pawP_scan_nstr(C, C->strings, buf.data, buf.size);
    pawL_discard_result(P, &buf);
    return s->text;
}

#ifdef PAW_DEBUG_EXTRA

static void dump_block_list(struct Printer *P, struct MirBlockList *blocks)
{
    for (int i = 0; i < blocks->count; ++i) {
        if (i > 0) PRINT_LITERAL(P, ", ");
        PRINT_FORMAT(P, "bb%d", MirBlockList_get(blocks, i).value);
    }
}

static void dump_graph(struct Printer *P, struct Mir *mir)
{
    DUMP_LITERAL(P, "Mir {\n");
    ++P->indent;

    for (int i = 0; i < mir->blocks->count; ++i) {
        struct MirBlockData const *data = mir_bb_data(mir, MIR_BB(i));
        DUMP_FORMAT(P, "bb%d: {\n", i);
        ++P->indent;
        DUMP_FORMAT(P, "pred: [");
        dump_block_list(P, data->predecessors);
        PRINT_LITERAL(P, "]\n");
        DUMP_FORMAT(P, "succ: [");
        dump_block_list(P, data->successors);
        PRINT_LITERAL(P, "]\n");
        --P->indent;
        DUMP_LITERAL(P, "}\n");
    }

    --P->indent;
    DUMP_LITERAL(P, "}\n");
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

    Str const *s = pawP_scan_nstr(C, C->strings, buf.data, buf.size);
    pawL_discard_result(P, &buf);
    return s->text;
}

#endif // PAW_DEBUG_EXTRA

void pawMir_validate_graph(struct Mir *mir)
{
    int index;
    struct MirBlockData *const *ppred;
    K_LIST_ENUMERATE(mir->blocks, index, ppred) {
        struct MirBlockData const *pred = *ppred;
        MirBlock const p = MIR_BB(index);

        paw_assert(pred->instructions->count > 0);
        struct MirInstruction const *term = K_LIST_LAST(pred->instructions);
        switch (MIR_KINDOF(term)) {
            case kMirGoto:
                paw_assert(pred->successors->count == 1);
                break;
            case kMirBranch:
                paw_assert(pred->successors->count == 2);
                break;
            case kMirSwitch:
                paw_assert(pred->successors->count >= 1);
                break;
            default:
                paw_assert(MirIsReturn(term) || MirIsUnreachable(term));
                paw_assert(pred->successors->count == 0);
        }

        MirBlock const *ps;
        K_LIST_FOREACH (pred->successors, ps) {
            struct MirBlockData const *succ = mir_bb_data(mir, *ps);
            mir_which_pred(mir, *ps, p); // crashes if not found
        }
    }
}
