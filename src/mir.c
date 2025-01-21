// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "map.h"
#include "mir.h"

struct MirScope *pawMir_get_scope(struct Mir *mir, MirScopeId id)
{
    return &K_LIST_GET(mir->scopes, id.value);
}

struct Mir *pawMir_new(struct Compiler *C, String *name, struct IrType *type, struct IrType *self, enum FuncKind fn_kind, paw_Bool is_native, paw_Bool is_pub, paw_Bool is_poly)
{
    struct Mir *mir = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct Mir));
    *mir = (struct Mir){
        .captured = pawMir_capture_list_new(C),
        .registers = pawMir_register_data_list_new(C),
        .blocks = pawMir_block_data_list_new(C),
        .scopes = pawMir_scope_list_new(C),
        .upvalues = pawMir_upvalue_list_new(C),
        .children = pawMir_body_list_new(C),
        .is_native = is_native,
        .is_poly = is_poly,
        .is_pub = is_pub,
        .fn_kind = fn_kind,
        .name = name,
        .type = type,
        .self = self,
    };
    return mir;
}

struct MirLiveInterval *pawMir_new_interval(struct Compiler *C, MirRegister r, int npositions)
{
    struct MirLiveInterval *it = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirLiveInterval));
    *it = (struct MirLiveInterval){
        .ranges = pawP_bitset_new(C, npositions),
        .r = r,
    };
    return it;
}

struct MirRegisterData *pawMir_new_register(struct Compiler *C, int value, struct IrType *type)
{
    struct MirRegisterData *r = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirRegisterData));
    *r = (struct MirRegisterData){
        .type = type,
    };
    return r;
}

struct MirInstruction *pawMir_new_instruction(struct Compiler *C, enum MirInstructionKind kind)
{
    struct MirInstruction *instr = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirInstruction));
    *instr = (struct MirInstruction){
        .hdr.kind = kind,
    };
    return instr;
}

struct MirBlockData *pawMir_new_block(struct Compiler *C)
{
    struct MirBlockData *block = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirBlockData));
    *block = (struct MirBlockData){
        .predecessors = pawMir_block_list_new(C),
        .successors = pawMir_block_list_new(C),
        .joins = pawMir_instruction_list_new(C),
        .instructions = pawMir_instruction_list_new(C),
        .loop_end = MIR_INVALID_BB,
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

static void AcceptForLoop(struct MirVisitor *V, struct MirForLoop *t)
{
    switch (t->for_kind) {
        case MIR_FORNUM_PREP:
        case MIR_FORNUM_LOOP:
            pawMir_visit_register(V, t->fornum.begin);
            pawMir_visit_register(V, t->fornum.end);
            pawMir_visit_register(V, t->fornum.step);
            pawMir_visit_register(V, t->fornum.var);
    }
}

#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL ? (V)->PostVisit##name(V, x) : (void)0)
#define DEFINE_VISITOR_CASES(X) case kMir##X: { \
        struct Mir##X *v = MirGet##X(node); \
        if (VISITOR_CALL(V, X, v)) Accept##X(V, v); \
        VISITOR_POSTCALL(V, X, v); \
    } \
    break;

void pawMir_visit_register(struct MirVisitor *V, MirRegister r)
{
    if (V->VisitRegister(V, r)) {
        V->PostVisitRegister(V, r);
    }
}

void pawMir_visit_instruction(struct MirVisitor *V, struct MirInstruction *node)
{
    paw_assert(node != NULL);
    if (!V->VisitInstruction(V, node)) return;

    switch (MIR_KINDOF(node)) {
        MIR_INSTRUCTION_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitInstruction(V, node);
}

void pawMir_visit_block(struct MirVisitor *V, MirBlock bb)
{
    struct MirBlockData *block = mir_bb_data(V->mir, bb);
    paw_assert(block != NULL);

    if (!V->VisitBlock(V, bb)) return;

    pawMir_visit_instruction_list(V, block->instructions);

    V->PostVisitBlock(V, bb);
}

paw_Bool default_visit_instruction(struct MirVisitor *V, struct MirInstruction *node) {return PAW_TRUE;}
paw_Bool default_visit_block(struct MirVisitor *V, MirBlock node) {return PAW_TRUE;}
paw_Bool default_visit_register(struct MirVisitor *V, MirRegister node) {return PAW_TRUE;}
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
    for (int i = 0; i < mir->children->count; ++i) {
        struct Mir *child = K_LIST_GET(mir->children, i);
        V->mir = child;
        pawMir_visit(V);
    }
    V->mir = mir;
}

#define DEFINE_LIST_VISITOR(name, T) \
    void pawMir_visit_##name##_list(struct MirVisitor *V, struct Mir##T##List *list) { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            pawMir_visit_##name(V, K_LIST_GET(list, i)); \
        } \
    }
DEFINE_LIST_VISITOR(block, Block)
DEFINE_LIST_VISITOR(instruction, Instruction)
DEFINE_LIST_VISITOR(register, Register)
#undef DEFINE_LIST_VISITOR

struct Successors {
    struct MirBlockList *successors;
    MirBlock block;
};

DEFINE_LIST(struct Compiler, visit_stack_, VisitStack, struct Successors)

struct Traversal {
    struct Compiler *C;
    struct Mir *mir;
    Map *visited;
};

static paw_Bool check_visited(struct Traversal *X, Map *visited, MirBlock bb)
{
    if (pawH_get(visited, I2V(bb.value)) != NULL) return PAW_TRUE;
    pawH_insert(ENV(X->C), visited, I2V(bb.value), P2V(NULL));
    return PAW_FALSE;
}

static void reverse_blocks(struct MirBlockList *blocks)
{
    if (blocks->count <= 0) return;
    MirBlock *a = &K_LIST_FIRST(blocks);
    MirBlock *b = &K_LIST_LAST(blocks);
    for (; a < b; a++, b--) {
        const MirBlock t = *a;
        *a = *b;
        *b = t;
    }
}

static struct MirBlockList *copy_blocks(struct Traversal *X, struct MirBlockList *blocks)
{
    MirBlock *b;
    struct MirBlockList *result = pawMir_block_list_new(X->C);
    K_LIST_RESERVE(X->C, result, blocks->count);
    K_LIST_FOREACH(blocks, b) K_LIST_PUSH(X->C, result, *b);
    return result;
}

static void visit_postorder(struct Traversal *X, Map *visited, struct VisitStack *stack, MirBlock bb)
{
    if (check_visited(X, visited, bb)) return;
    struct MirBlockData *data = mir_bb_data(X->mir, bb);
    K_LIST_PUSH(X->C, stack, ((struct Successors){
                    .successors = copy_blocks(X, data->successors),
                    .block = bb,
                }));
}

static void traverse_postorder(struct Traversal *X, Map *visited, struct VisitStack *stack)
{
    while (stack->count > 0) {
        struct Successors *top = &K_LIST_LAST(stack);
        if (top->successors->count == 0) break;
        const MirBlock bb = K_LIST_LAST(top->successors);
        K_LIST_POP(top->successors);
        visit_postorder(X, visited, stack, bb);
    }
}

struct MirBlockList *pawMir_traverse_rpo(struct Compiler *C, struct Mir *mir)
{
    struct VisitStack *stack = visit_stack_new(C);
    Map *visited = pawP_push_map(C); // push 'visited'
    struct Traversal X = {
        .visited = visited,
        .mir = mir,
        .C = C,
    };
    paw_assert(mir->blocks->count > 0);
    visit_postorder(&X, visited, stack, MIR_ROOT_BB);
    traverse_postorder(&X, visited, stack);

    struct MirBlockList *order = pawMir_block_list_new(C);
    while (stack->count > 0) {
        struct Successors s = K_LIST_LAST(stack);
        K_LIST_POP(stack);
        traverse_postorder(&X, visited, stack);
        K_LIST_PUSH(C, order, s.block);
    }
    pawP_pop_object(C, visited);
    reverse_blocks(order);

    return order;
}

static void renumber_ref(Map *map, MirBlock *pb)
{
    paw_assert(MIR_BB_EXISTS(*pb));
    const Value *pval = pawH_get(map, I2V(pb->value));
    *pb = MIR_BB(pval->i);
}

static void renumber_or_clear_ref(Map *map, MirBlock *pb)
{
    paw_assert(MIR_BB_EXISTS(*pb));
    const Value *pval = pawH_get(map, I2V(pb->value));
    if (pval != NULL) *pb = MIR_BB(pval->i);
    else *pb = MIR_INVALID_BB;
}

static void rename_and_filter(struct Compiler *C, Map *map, struct MirBlockList *blocks)
{
    int i;
    int removed = 0;
    const MirBlock *b;
    K_LIST_ENUMERATE(blocks, i, b) {
        const Value *pval = pawH_get(map, I2V(b->value));
        if (pval != NULL) {
            K_LIST_SET(blocks, i - removed, MIR_BB(pval->i));
        } else {
            ++removed;
        }
    }
    blocks->count -= removed;
}

// Renumber basic blocks so that "mir_bb_data" continues to work
static void renumber_block_refs(struct Compiler *C, Map *map, struct MirBlockData *data)
{
    rename_and_filter(C, map, data->predecessors);
    rename_and_filter(C, map, data->successors);

    struct MirInstruction *terminator = K_LIST_LAST(data->instructions);
    switch (MIR_KINDOF(terminator)) {
        case kMirReturn:
            break;
        case kMirBranch: {
            struct MirBranch *t = MirGetBranch(terminator);
            renumber_ref(map, &t->then_arm);
            renumber_ref(map, &t->else_arm);
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *t = MirGetSwitch(terminator);
            for (int i = 0; i < t->arms->count; ++i) {
                renumber_ref(map, &K_LIST_GET(t->arms, i).bid);
            }
            if (MIR_BB_EXISTS(t->otherwise)) {
                renumber_ref(map, &t->otherwise);
            }
            break;
        }
        case kMirForLoop: {
            struct MirForLoop *t = MirGetForLoop(terminator);
            renumber_ref(map, &t->then_arm);
            renumber_ref(map, &t->else_arm);
            break;
        }
        case kMirGoto: {
            struct MirGoto *t = MirGetGoto(terminator);
            renumber_ref(map, &t->target);
            break;
        }
        default:
            PAW_UNREACHABLE();
    }
}

static void rename_usedef_blocks(struct Compiler *C, Map *mapping, Map *usedef)
{
    int index;
    MirBlock *pb;
    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(usedef, &iter)) {
        struct MirBlockList *blocks = pawH_value(usedef, iter)->p;
        K_LIST_ENUMERATE(blocks, index, pb) {
            const Value *pval = pawH_get(mapping, I2V(pb->value));
            if (pval != NULL) {
                *pb = MIR_BB(pval->i);
            } else {
                *pb = K_LIST_LAST(blocks);
                K_LIST_POP(blocks);
                --index;
            }
        }
    }
}

static void dump_usedef(struct Compiler *C, Map *uses, Map *defs)
{
    paw_Int iter = PAW_ITER_INIT;
    printf("uses = {\n");
    while (pawH_iter(uses, &iter)) {
        printf("  _%lld: [", pawH_key(uses, iter)->i);
        struct MirBlockList *bl = pawH_value(uses, iter)->p;
        int i;
        const MirBlock *pb;
        K_LIST_ENUMERATE(bl, i, pb) {
            if (i > 0) printf(", ");
            printf("bb%d", pb->value);
        }
        printf("]\n");
    }
    iter = PAW_ITER_INIT;
    printf("}\ndefs = {\n");
    while (pawH_iter(uses, &iter)) {
        printf("  _%lld: [", pawH_key(defs, iter)->i);
        struct MirBlockList *bl = pawH_value(defs, iter)->p;
        int i;
        const MirBlock *pb;
        K_LIST_ENUMERATE(bl, i, pb) {
            if (i > 0) printf(", ");
            printf("bb%d", pb->value);
        }
        printf("]\n");
    }
}

void pawMir_remove_unreachable_blocks(struct Compiler *C, struct Mir *mir, Map *uses, Map *defs)
{
    // create a mapping from old to new basic block numbers
    Map *map = pawP_push_map(C);
    struct MirBlockList *order = pawMir_traverse_rpo(C, mir);
    for (int i = 0; i < order->count; ++i) {
        const MirBlock old_bb = K_LIST_GET(order, i);
        pawH_insert(ENV(C), map, I2V(old_bb.value), I2V(i));
    }

    const MirBlock *pb;
    struct MirBlockDataList *blocks = pawMir_block_data_list_new(C);
    K_LIST_RESERVE(C, blocks, order->count);
    K_LIST_FOREACH(order, pb) {
        struct MirBlockData *bb = mir_bb_data(mir, *pb);
        if (MIR_BB_EXISTS(bb->loop_end)) {
            // The "loop_end" is unreachable if there is an unconditional break in the loop.
            // There are no additional iterations where loop header variables need to be
            // preserved, so the "loop_end" can be ignored (used for SSA construction).
            renumber_or_clear_ref(map, &bb->loop_end);
        }
        renumber_block_refs(C, map, bb);
        K_LIST_PUSH(C, blocks, bb);
    }

    rename_usedef_blocks(C, map, uses);
    rename_usedef_blocks(C, map, defs);

    mir->blocks = blocks;
    pawP_pop_object(C, map);
}

paw_Bool pawMir_check_load(struct Compiler *C, struct MirInstruction *instr, struct MirLoad *pload)
{
#define ADD_INPUT(x) K_LIST_PUSH(C, pload->inputs, &(x))
#define ADD_INPUTS(xs) K_LIST_FOREACH(xs, pr) ADD_INPUT(*pr)

    MirRegister *pr;
    pload->inputs = pawMir_register_ptr_list_new(C);

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
        case kMirForLoop: {
            struct MirForLoop *x = MirGetForLoop(instr);
            switch (x->for_kind) {
                case MIR_FORNUM_LOOP:
                    ADD_INPUT(x->fornum.var);
                    // fallthrough
                case MIR_FORNUM_PREP:
                    ADD_INPUT(x->fornum.begin);
                    ADD_INPUT(x->fornum.end);
                    ADD_INPUT(x->fornum.step);
            }
            break;
        }
        default:
            return PAW_FALSE;
    }
    return PAW_TRUE;

#undef ADD_INPUTS
#undef ADD_INPUT
}

paw_Bool pawMir_check_store(struct Compiler *C, struct MirInstruction *instr, struct MirStore *pstore)
{
#define ADD_OUTPUT(x) K_LIST_PUSH(C, pstore->outputs, &(x))
#define ADD_OUTPUTS(xs) K_LIST_FOREACH(xs, pr) ADD_OUTPUT(*pr)

    const MirRegister *pr;
    pstore->outputs = pawMir_register_ptr_list_new(C);

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
        case kMirSetLocal:
            ADD_OUTPUT(MirGetSetLocal(instr)->target);
            break;
        case kMirConstant:
            ADD_OUTPUT(MirGetConstant(instr)->output);
            break;
        case kMirAggregate:
            ADD_OUTPUT(MirGetAggregate(instr)->output);
            break;
        case kMirContainer:
            ADD_OUTPUT(MirGetContainer(instr)->output);
            break;
        case kMirCall:
            ADD_OUTPUT(MirGetCall(instr)->output);
            break;
        case kMirCast:
            ADD_OUTPUT(MirGetCast(instr)->output);
            break;
        case kMirClosure:
            ADD_OUTPUT(MirGetClosure(instr)->output);
            break;
        case kMirGetElement:
            ADD_OUTPUT(MirGetGetElement(instr)->output);
            break;
        case kMirGetRange:
            ADD_OUTPUT(MirGetGetRange(instr)->output);
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
        case kMirForLoop: {
            struct MirForLoop *x = MirGetForLoop(instr);
            switch (x->for_kind) {
                case MIR_FORNUM_PREP:
                    ADD_OUTPUT(x->fornum.var);
                    break;
                case MIR_FORNUM_LOOP:
                    break;
            }
            break;
        }
        default:
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

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
static void dump_terminator(struct Printer *, struct MirTerminator *);

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
    // TODO: remove, just for debugging
#warning "remove this"
    if (!MirIsPhi(instr)) DUMP_FMT(P, "%d: ", instr->hdr.location);

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
            for (int i = 0;i < t->inputs->count; ++i) {
                if (i > 0) L_ADD_LITERAL(P->P, P->buf, ", ");
                pawL_add_fstring(P->P, P->buf, "_%d", K_LIST_GET(t->inputs, i).value);
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
            const char *type = pawIr_print_type(P->C, mir_reg_data(P->mir, t->output)->type);
            DUMP_FMT(P, "_%d = global %s (%s)\n", t->output.value, decl->hdr.name->text, type);
            --ENV(P->C)->top.p; // pop 'type'
            break;
        }
        case kMirConstant: {
            struct MirConstant *t = MirGetConstant(instr);
            DUMP_FMT(P, "_%d = const ", t->output.value);
            switch (t->code) {
                case PAW_TUNIT:
                    L_ADD_LITERAL(P->P, P->buf, "()\n");
                    break;
                case PAW_TBOOL:
                    pawL_add_fstring(P->P, P->buf, "%s\n", V_TRUE(t->value) ? "true" : "false");
                    break;
                case PAW_TINT:
                    pawL_add_fstring(P->P, P->buf, "%I\n", V_INT(t->value));
                    break;
                case PAW_TFLOAT:
                    pawL_add_fstring(P->P, P->buf, "%f\n", V_FLOAT(t->value));
                    break;
                case PAW_TSTR:
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
                if (i > 0) L_ADD_LITERAL(P->P, P->buf, ", ");
                pawL_add_fstring(P->P, P->buf, "_%d", K_LIST_GET(t->args, i).value);
            }
            L_ADD_LITERAL(P->P, P->buf, ")\n");
            break;
        }
        case kMirCast: {
            struct MirCast *t = MirGetCast(instr);
            DUMP_FMT(P, "_%d = ", t->output.value);
            switch (t->to) {
                case PAW_TBOOL:
                    L_ADD_LITERAL(P->P, P->buf, "(bool)");
                    break;
                case PAW_TINT:
                    L_ADD_LITERAL(P->P, P->buf, "(int)");
                    break;
                case PAW_TFLOAT:
                    L_ADD_LITERAL(P->P, P->buf, "(float)");
                    break;
                case PAW_TSTR:
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
                if (i > 0) L_ADD_LITERAL(P->P, P->buf, ", ");
                struct MirSwitchArm arm = K_LIST_GET(t->arms, i);
                pawL_add_fstring(P->P, P->buf, "%d: bb%d", arm.value, arm.bid.value);
            }
            if (MIR_BB_EXISTS(t->otherwise)) {
                if (t->arms->count > 0) L_ADD_LITERAL(P->P, P->buf, ", ");
                pawL_add_fstring(P->P, P->buf, "_: bb%d", t->otherwise.value);
            }
            L_ADD_LITERAL(P->P, P->buf, "]\n");
            break;
        }
        case kMirForLoop: {
            struct MirForLoop *t = MirGetForLoop(instr);
            switch (t->for_kind) {
                case MIR_FORNUM_PREP:
                case MIR_FORNUM_LOOP:
                    DUMP_FMT(P, "fornum _%d _%d _%d _%d", t->fornum.begin.value, t->fornum.end.value,
                            t->fornum.step.value, t->fornum.var.value);
            }
            pawL_add_fstring(P->P, P->buf, " => [0: bb%d, 1: bb%d]\n", t->else_arm.value, t->then_arm.value);
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
    DUMP_FMT(P, "bb%d [%d, %d] {\n", bb.value, block->location, mir_bb_last(block));
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

const char *pawMir_dump(struct Compiler *C, struct Mir *mir)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_mir(&(struct Printer){
                .P = ENV(C),
                .mir = mir,
                .buf = &buf,
                .C = C,
            }, mir);

    for (int i = 0; i < mir->children->count; ++i) {
        struct Mir *child = K_LIST_GET(mir->children, i);
        dump_mir(&(struct Printer){
                    .P = ENV(C),
                    .mir = child,
                    .buf = &buf,
                    .C = C,
                }, child);
    }

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

static void dump_block_list(struct Printer *P, struct MirBlockList *blocks)
{
    for (int i = 0; i < blocks->count; ++i) {
        if (i > 0) L_ADD_LITERAL(P->P, P->buf, ", ");
        pawL_add_fstring(P->P, P->buf, "bb%d", K_LIST_GET(blocks, i).value);
    }
}

static void dump_graph(struct Printer *P, struct Mir *mir)
{
    DUMP_MSG(P, "Mir {\n");
    ++P->indent;

    for (int i = 0; i < mir->blocks->count; ++i) {
        const struct MirBlockData *data = mir_bb_data(mir, MIR_BB(i));
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

const char *pawMir_dump_graph(struct Compiler *C, struct Mir *mir)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_graph(&(struct Printer){
                .P = ENV(C),
                .mir = mir,
                .buf = &buf,
                .C = C,
            }, mir);

    for (int i = 0; i < mir->children->count; ++i) {
        struct Mir *child = K_LIST_GET(mir->children, i);
        dump_graph(&(struct Printer){
                    .P = ENV(C),
                    .mir = mir,
                    .buf = &buf,
                    .C = C,
                }, child);
    }

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

static void dump_info(struct Printer *P, struct Mir *mir)
{
    int index;
    struct MirCaptureInfo *pci;
    PRINT_LITERAL(P, "captured: [\n");
    K_LIST_ENUMERATE(mir->captured, index, pci) {
        if (index > 0) PRINT_LITERAL(P, ", ");
        PRINT_FORMAT(P, "_%d", pci->r.value);
    }
    PRINT_LITERAL(P, "]\n");
}

const char *pawMir_dump_info(struct Compiler *C, struct Mir *mir)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_info(&(struct Printer){
                .P = ENV(C),
                .mir = mir,
                .buf = &buf,
                .C = C,
            }, mir);

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}
