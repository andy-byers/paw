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
// The register allocator uses a simplified "reverse linear scan" algorithm.
// As noted above, spills are not considered, meaning each virtual register
// allocated during HIR lowering can be associated with exactly 1 physical
// register.

#include "compile.h"
#include "map.h"
#include "mir.h"
#include "ir_type.h"
#include "ssa.h"

#define ERROR(R, code, ...) pawE_error(ENV((R)->C), code, -1, __VA_ARGS__)
#define ASSIGN(r, info) ((r).value = (info).value)

struct RegisterInfo {
    int value;
};

struct MoveInfo {
    MirRegister from;
    MirRegister to;
};

DEFINE_LIST(struct Compiler, regtab_, RegisterTable, struct RegisterInfo)
DEFINE_LIST(struct Compiler, move_list_, MoveList, struct MoveInfo)

enum AllocationKind {
    ALLOCATE_NEXT,
    ALLOCATE_ANY,
};

struct RegisterAllocator {
    // stores the virtual register currently assigned to each real register
    MirRegister registers[NREGISTERS];

    // table indexed on virtual register number, containing real register IDs
    struct RegisterTable *active;

    struct MirBlockData *current;

    struct MirInstructionList *code;
    struct ScopeStack *scopes;
    struct Compiler *C;
    struct Mir *mir;
    Map *moves;

    // ID of the highest-numbered free register, or the top of the register
    // stack
    int free_reg;

    // largest register ID encountered
    int max_reg;
};

static struct MirInstruction *new_instruction(struct RegisterAllocator *R, enum MirInstructionKind kind)
{
    struct MirInstruction *instr = pawMir_new_instruction(R->C, kind);
    K_LIST_PUSH(R->C, R->code, instr);
    return instr;
}

//static struct MoveList *get_phi_moves(struct RegisterAllocator *R, MirBlock bb)
//{
//    const Value *pval = pawH_get(R->moves, I2V(bb.value));
//    if (pval == NULL) {
//        struct MoveList *moves = move_list_new(R->C);
//        pawH_insert(ENV(R->C), R->moves, I2V(bb.value), P2V(moves));
//        return moves;
//    }
//    return pval->p;
//}

static struct MirBlockData *enter_basic_block(struct RegisterAllocator *R, MirBlock bb)
{
    R->current = mir_bb_data(R->mir, bb);
    R->code = pawMir_instruction_list_new(R->C);
    return R->current;
}

static void leave_basic_block(struct RegisterAllocator *R)
{
    struct MirInstructionList *code = R->code;
    R->current->instructions = code;
    R->code = NULL;

    if (code->count <= 0) return;
    struct MirInstruction **a = &K_LIST_FIRST(code);
    struct MirInstruction **b = &K_LIST_LAST(code);
    for (; a < b; ++a, --b) {
        struct MirInstruction *t = *a;
        *a = *b;
        *b = t;
    }
}

#define REG_EQUALS(a, b) ((a).value == (b).value)
#define REG_EXISTS(r) (!REG_EQUALS(r, ((struct RegisterInfo){-1})))

DEFINE_LIST(struct Compiler, scope_stack_, ScopeStack, int)

//#if 0

#include <stdio.h>

static void debug_registers(struct RegisterAllocator *R, MirRegister *regs, int n)
{
    for (int i = 0; i < n; ++i) {
        const MirRegister r = regs[i];
        if (i > 0) fputc(' ', stderr);
        if (MIR_REG_EXISTS(r)) fputc('*', stderr);
        else fputc('.', stderr);
    }
}

static void sanity_check(struct RegisterAllocator *R)
{
    return; /// TODO
    for (int i = 0; i < NREGISTERS; ++i) {
        const MirRegister r = R->registers[i];
        paw_assert(i < R->free_reg || !MIR_REG_EXISTS(r));
    }
    for (int i = 0; i < R->active->count; ++i) {
        const struct RegisterInfo info = K_LIST_GET(R->active, i);
        if (REG_EXISTS(info)) {
            const MirRegister r = R->registers[info.value];
            paw_assert(r.value == i);
        }
    }
}

static void initialize_logger(struct RegisterAllocator *R, int n)
{
    fputs("free  max  ", stderr);
    for (int i = 0; i < n; ++i) {
        if (i > 0) fputc(' ', stderr);
        fprintf(stderr, "%d", i);
    }
    fputc('\n', stderr);
}

void debug_log(struct RegisterAllocator *R, int n)
{
    fprintf(stderr, "%4d  %3d  ", R->free_reg, R->max_reg);
    debug_registers(R, R->registers, n);
    fputc('\n', stderr);
}

//#endif // 0

#define REGINFO(R, r) (&K_LIST_GET((R)->active, (r).value))

// Return the index of the first unoccupied register
// If "kind" equals ALLOCATE_NEXT, returns the lowest-numbered register guaranteed to
// be above all other occupied registers (the top of the register stack).
static struct RegisterInfo get_unoccupied_reg(struct RegisterAllocator *R, enum AllocationKind kind)
{
    paw_assert(R->free_reg >= 0);

    for (int i = kind == ALLOCATE_NEXT ? R->free_reg : 0;
            i < PAW_COUNTOF(R->registers); ++i) {
        if (!MIR_REG_EXISTS(R->registers[i])) return (struct RegisterInfo){i};
    }

    pawE_error(ENV(R->C), PAW_EOVERFLOW, -1, "too many variables");
}

static void release_reg(struct RegisterAllocator *R, MirRegister r);

static void bind_reg(struct RegisterAllocator *R, MirRegister r, int rid)
{
    paw_assert(rid >= 0);
    MirRegister *pr = &R->registers[rid];
    if (REG_EXISTS(*pr)) release_reg(R, *pr);
    paw_assert(!REG_EXISTS(*pr));
    *pr = r;

    struct RegisterInfo *pinfo = REGINFO(R, r);
    paw_assert(!REG_EXISTS(*pinfo));
    pinfo->value = rid;

    R->free_reg = PAW_MAX(R->free_reg, rid + 1);
    R->max_reg = PAW_MAX(R->max_reg, rid);

    debug_log(R,10);
    sanity_check(R);
}

static void adjust_free_reg(struct RegisterAllocator *R)
{
    int r = R->free_reg;
    if (r >= NREGISTERS) return;
    paw_assert(!REG_EXISTS(R->registers[r]));
    for (; r > 0 && !REG_EXISTS(R->registers[r - 1]); --r);
    paw_assert(!REG_EXISTS(R->registers[r]));
    R->free_reg = r;
}

static void unbind_reg(struct RegisterAllocator *R, MirRegister r, struct RegisterInfo *pinfo)
{
    if (!REG_EXISTS(*pinfo)) {
        // TODO: This happens for dead stores (variables not used in the result of the function, or
        //       consumed by some other instruction).
        const struct RegisterInfo info = get_unoccupied_reg(R, ALLOCATE_ANY);
        bind_reg(R, r, info.value);
    }
    paw_assert(REG_EXISTS(*pinfo));
    paw_assert(R->registers[pinfo->value].value == r.value);

    R->registers[pinfo->value] = MIR_INVALID_REG;
    pinfo->value = -1;
    adjust_free_reg(R);

    debug_log(R,10);
    sanity_check(R);
}

static void release_reg(struct RegisterAllocator *R, MirRegister r)
{
    struct RegisterInfo *pinfo = REGINFO(R, r);
    unbind_reg(R, r, pinfo);
}

static struct RegisterInfo allocate_reg(struct RegisterAllocator *R, MirRegister r, enum AllocationKind kind)
{
    const struct RegisterInfo info = get_unoccupied_reg(R, kind);
    bind_reg(R, r, info.value);
    return info;
}

static struct RegisterInfo ensure_reg(struct RegisterAllocator *R, MirRegister r, enum AllocationKind kind)
{
    const struct RegisterInfo *info = REGINFO(R, r);
    if (REG_EXISTS(*info)) return *info;
    return allocate_reg(R, r, kind);
}

static void register_input_op(struct RegisterAllocator *R, MirRegister input, enum AllocationKind kind)
{
    allocate_reg(R, input, kind);
}

static void register_output_op(struct RegisterAllocator *R, MirRegister output)
{
    release_reg(R, output);
}

static struct RegisterInfo *get_output_reg(struct RegisterAllocator *R, MirRegister r, enum AllocationKind alloc_kind)
{
    struct RegisterInfo *output = REGINFO(R, r);
    if (!REG_EXISTS(*output)) {
        const struct RegisterInfo info = get_unoccupied_reg(R, alloc_kind);
        bind_reg(R, r, info.value); // updates "output"
    }
    return output;
}

struct Registers {
    struct RegisterInfo out;
    struct RegisterInfo lhs;
    struct RegisterInfo rhs;
};

// Allocate registers for an operation with a single "input" and a single "output"
// "output" is already in a register, since control is moving backward. If "input" is not also in
// a register, then the "output" register can be used.
static struct Registers register_unary_op(struct RegisterAllocator *R, MirRegister input, MirRegister output, enum AllocationKind alloc_kind)
{
    struct RegisterInfo *in = REGINFO(R, input);
    struct RegisterInfo *out = get_output_reg(R, output, alloc_kind);
    struct Registers result = {*out};

    if (!REG_EXISTS(*in)) {
        // "input" is not in a register, so put it where "output" was
        bind_reg(R, input, out->value);
        result.lhs = *in;
    } else {
        result.lhs = *out;
        release_reg(R, output);
    }
    result.rhs.value = -1;
    return result;
}

static struct Registers register_binary_op(struct RegisterAllocator *R, MirRegister lhs, MirRegister rhs, MirRegister output, enum AllocationKind alloc_kind)
{
    struct RegisterInfo left = *REGINFO(R, lhs);
    struct RegisterInfo right = *REGINFO(R, rhs);
    struct RegisterInfo out = *get_output_reg(R, output, alloc_kind);
    struct Registers result = {out};

    if (!REG_EXISTS(left)) {
        if (REG_EXISTS(right)) {
            // !left && right
            result.lhs = out;
            result.rhs = right;
            bind_reg(R, lhs, out.value);
        } else if (!REG_EQUALS(lhs, rhs)) {
            // !left && !right && lhs != rhs
            const struct RegisterInfo extra = get_unoccupied_reg(R, alloc_kind);
            result.lhs = out;
            result.rhs = extra;
            bind_reg(R, lhs, out.value);
            bind_reg(R, rhs, extra.value);
        } else {
            // !left && !right && lhs == rhs
            result.lhs = out;
            result.rhs = out;
            bind_reg(R, lhs, out.value);
        }
    } else if (!REG_EXISTS(right)) {
        // left && !right
        result.lhs = left;
        result.rhs = out;
        bind_reg(R, rhs, out.value);
    } else {
        // left && right
        result.lhs = left;
        result.rhs = right;
        release_reg(R, output);
    }
    return result;
}

static void alloc_alloc_local(struct MirVisitor *V, struct MirAllocLocal *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo out = *get_output_reg(R, x->output, ALLOCATE_ANY);
    struct MirInstruction *alloc = new_instruction(R, kMirAllocLocal);
    ASSIGN(MirGetAllocLocal(alloc)->output, out);
    MirGetAllocLocal(alloc)->name = x->name;

    release_reg(V->ud, x->output);
}

//static void move_result(struct RegisterAllocator *R, MirRegister from, MirRegister to)
//{
//    struct RegisterInfo a = *REGINFO(R, from);
//    if (!REG_EXISTS(a)) {
//        allocate_reg(R, from, ALLOCATE_ANY);
//    }
//}

//static void alloc_phi(struct MirVisitor *V, struct MirPhi *x)
//{
//    struct RegisterAllocator *R = V->ud;
//    struct RegisterInfo *out = get_output_reg(R, x->output, ALLOCATE_ANY);
//    unbind_reg(R, x->output, out);
//
////    for (int i = 0; i < x->inputs->count; ++i) {
////        const MirRegister input = K_LIST_GET(x->inputs, i);
////        struct RegisterInfo *in = REGINFO(R, input);
////        if (!REG_EXISTS(*in)) allocate_reg(R, input, ALLOCATE_ANY);
////    }
//}

static void alloc_move(struct MirVisitor *V, struct MirMove *x)
{
    struct RegisterAllocator *R = V->ud;
    struct Registers r = register_unary_op(R, x->target, x->output, ALLOCATE_ANY);
    struct MirInstruction *move = new_instruction(R, kMirMove);
    ASSIGN(MirGetMove(move)->output, r.out);
    ASSIGN(MirGetMove(move)->target, r.lhs);
}

static void alloc_global(struct MirVisitor *V, struct MirGlobal *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo out = *get_output_reg(R, x->output, ALLOCATE_ANY);

    struct MirInstruction *global = new_instruction(R, kMirGlobal);
    ASSIGN(MirGetGlobal(global)->output, out);
    MirGetGlobal(global)->type = x->type;

    unbind_reg(R, x->output, &out);
}

static void alloc_constant(struct MirVisitor *V, struct MirConstant *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo out = *get_output_reg(R, x->output, ALLOCATE_ANY);

    struct MirInstruction *constant = new_instruction(R, kMirConstant);
    ASSIGN(MirGetConstant(constant)->output, out);
    MirGetConstant(constant)->value = x->value;
    MirGetConstant(constant)->code = x->code;

    unbind_reg(R, x->output, &out);
}

static void alloc_upvalue(struct MirVisitor *V, struct MirUpvalue *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo out = *get_output_reg(R, x->output, ALLOCATE_ANY);

    struct MirInstruction *upvalue = new_instruction(R, kMirUpvalue);
    ASSIGN(MirGetUpvalue(upvalue)->output, out);
    MirGetUpvalue(upvalue)->index = x->index;

    unbind_reg(R, x->output, &out);
}

static void alloc_set_local(struct MirVisitor *V, struct MirSetLocal *x)
{
    struct RegisterAllocator *R = V->ud;
    struct Registers r = register_unary_op(R, x->value, x->target, ALLOCATE_ANY);
    struct RegisterInfo *output = get_output_reg(R, x->output, ALLOCATE_ANY); // TODO
    struct MirInstruction *setter = new_instruction(R, kMirSetLocal);
    ASSIGN(MirGetSetLocal(setter)->target, r.out);
    ASSIGN(MirGetSetLocal(setter)->value, r.lhs);
    ASSIGN(MirGetSetLocal(setter)->output, *output);
    unbind_reg(R, x->output, output);
}

static void alloc_set_upvalue(struct MirVisitor *V, struct MirSetUpvalue *x)
{
    register_input_op(V->ud, x->value, ALLOCATE_ANY);
}

static void alloc_aggregate(struct MirVisitor *V, struct MirAggregate *x)
{
    register_output_op(V->ud, x->output);
}

static void alloc_container(struct MirVisitor *V, struct MirContainer *x)
{
    register_output_op(V->ud, x->output);
}

// Allocate registers for a function call
// This operation has special constraints: the function object must be on top of the stack, followed
// by the arguments. After the function is called, the function object is replaced with the return
// value, which becomes the new top of the stack.
// Constraints are handled during code generation. An extra move is added to place the return value
// in the location expected by future operations.
static void alloc_call(struct MirVisitor *V, struct MirCall *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo out = *get_output_reg(R, x->output, ALLOCATE_NEXT);
    release_reg(R, x->output);

    // function object goes on top of the stack, followed by the arguments
    allocate_reg(R, x->target, ALLOCATE_NEXT);
    for (int i = 0; i < x->args->count; ++i) {
        MirRegister r = K_LIST_GET(x->args, i);
        allocate_reg(R, r, ALLOCATE_NEXT);
    }
}

static void alloc_cast(struct MirVisitor *V, struct MirCast *x)
{
//    struct RegisterAllocator *R = V->ud;
//    register_unary_op(R, x->target, x->output, ALLOCATE_ANY);
}

static void alloc_closure(struct MirVisitor *V, struct MirClosure *x)
{
    struct RegisterAllocator *R = V->ud;
    register_output_op(R, x->output);
}

static void alloc_get_element(struct MirVisitor *V, struct MirGetElement *x)
{
//    struct RegisterAllocator *R = V->ud;
//    register_binary_op(R, x->key, x->object, x->output, ALLOCATE_NEXT);
}

static void alloc_set_element(struct MirVisitor *V, struct MirSetElement *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo value = *REGINFO(R, x->value);
    struct RegisterInfo key = *REGINFO(R, x->key);
    get_output_reg(R, x->object, ALLOCATE_ANY);

    if (!REG_EXISTS(key)) allocate_reg(R, x->key, ALLOCATE_ANY);
    if (!REG_EXISTS(value)) allocate_reg(R, x->value, ALLOCATE_ANY);
}

static void alloc_get_range(struct MirVisitor *V, struct MirGetRange *x)
{
    // TODO: use a method instead of an operation, otherwise, do something similar to OP_CALL, which has similar constraints
    //       "lower" and "upper" need to be contiguous
//    struct RegisterAllocator *R = V->ud;
//    allocate_reg(R, x->upper);
//    allocate_reg(R, x->lower);
//    allocate_reg(R, x->object);
//    release_reg(R, x->output);
}

static void alloc_set_range(struct MirVisitor *V, struct MirSetRange *x)
{
    // TODO: use a method instead of an operation
//    struct RegisterAllocator *R = V->ud;
//    allocate_reg(R, x->value);
//    allocate_reg(R, x->upper);
//    allocate_reg(R, x->lower);
//    allocate_reg(R, x->object);
}

static void alloc_get_field(struct MirVisitor *V, struct MirGetField *x)
{
//    register_unary_op(V->ud, x->object, x->output, ALLOCATE_ANY);
}

static void alloc_set_field(struct MirVisitor *V, struct MirSetField *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo value = *REGINFO(R, x->value);
    get_output_reg(R, x->object, ALLOCATE_ANY);

    if (!REG_EXISTS(value)) allocate_reg(R, x->value, ALLOCATE_ANY);
}

static void alloc_unop(struct MirVisitor *V, struct MirUnaryOp *x)
{
    struct RegisterAllocator *R = V->ud;
    struct Registers r = register_unary_op(R, x->val, x->output, ALLOCATE_ANY);
    struct MirInstruction *unop = new_instruction(R, kMirBinaryOp);
    ASSIGN(MirGetUnaryOp(unop)->output, r.out);
    ASSIGN(MirGetUnaryOp(unop)->val, r.lhs);
    MirGetUnaryOp(unop)->type = x->type;
    MirGetUnaryOp(unop)->op = x->op;
}

static void alloc_binop(struct MirVisitor *V, struct MirBinaryOp *x)
{
    struct RegisterAllocator *R = V->ud;
    struct MirRegisterData *data = mir_reg_data(V->mir, x->lhs);
    const paw_Type code = pawP_type2code(R->C, data->type);
    if (x->op == BINARY_ADD && (code == BUILTIN_STR || code == BUILTIN_LIST)) {
        // TODO: handle this specially, possibly create a separate node type to avoid check
    }
    struct Registers r = register_binary_op(R, x->lhs, x->rhs, x->output, ALLOCATE_ANY);
    struct MirInstruction *binop = new_instruction(R, kMirBinaryOp);
    ASSIGN(MirGetBinaryOp(binop)->output, r.out);
    ASSIGN(MirGetBinaryOp(binop)->lhs, r.lhs);
    ASSIGN(MirGetBinaryOp(binop)->rhs, r.rhs);
    MirGetBinaryOp(binop)->type = x->type;
    MirGetBinaryOp(binop)->op = x->op;
}

static paw_Bool alloc_return(struct MirVisitor *V, struct MirReturn *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo r = allocate_reg(R, x->value, ALLOCATE_ANY);
    struct MirTerminator *ret = R->current->terminator;
    ASSIGN(MirGetReturn(ret)->value, r);
    return PAW_FALSE;
}

static paw_Bool alloc_for_loop(struct MirVisitor *V, struct MirForLoop *x)
{
    struct RegisterAllocator *R = V->ud;
    if (x->for_kind == MIR_FOR_LOOP) {
        struct RegisterInfo step = allocate_reg(R, x->step, ALLOCATE_NEXT);
        struct RegisterInfo end = allocate_reg(R, x->end, ALLOCATE_NEXT);
        struct RegisterInfo iter = allocate_reg(R, x->iter, ALLOCATE_NEXT);
        struct RegisterInfo var = allocate_reg(R, x->var, ALLOCATE_NEXT);

        struct MirTerminator *loop = R->current->terminator;
        ASSIGN(MirGetForLoop(loop)->step, step);
        ASSIGN(MirGetForLoop(loop)->end, end);
        ASSIGN(MirGetForLoop(loop)->iter, iter);
        ASSIGN(MirGetForLoop(loop)->var, var);
    }
    return PAW_FALSE;
}

static paw_Bool alloc_branch(struct MirVisitor *V, struct MirBranch *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo r = ensure_reg(R, x->cond, ALLOCATE_ANY);
    struct MirTerminator *branch = R->current->terminator;
    ASSIGN(MirGetBranch(branch)->cond, r);
    return PAW_FALSE;
}

static paw_Bool alloc_switch(struct MirVisitor *V, struct MirSwitch *x)
{
    struct RegisterAllocator *R = V->ud;
    struct RegisterInfo r = ensure_reg(R, x->discr, ALLOCATE_ANY);
    struct MirTerminator *switch_ = R->current->terminator;
    ASSIGN(MirGetSwitch(switch_)->discr, r);
    return PAW_FALSE;
}

static void debug_instruction(struct MirVisitor *V, struct MirInstruction *instr)
{
    PAW_UNUSED(instr);
    debug_log(V->ud, 10);
}

static void allocate_registers(struct RegisterAllocator *R, struct MirVisitor *V, struct MirBlockList *order)
{
    struct Mir *mir = R->mir;
    // allocate registers for return value and function arguments
    allocate_reg(R, MIR_RESULT_REG, ALLOCATE_NEXT);
    for (int i = 0; i < IR_FPTR(mir->type)->params->count; ++i) {
        allocate_reg(R, MIR_REG(1 + i), ALLOCATE_NEXT);
    }

    // visit instructions in reverse to get live ranges for free
    for (int ib = order->count - 1; ib >= 0; --ib) {
        const MirBlock bb = K_LIST_GET(order, ib);

        struct MirBlockData *data = enter_basic_block(R, bb);
        pawMir_visit_terminator(V, data->terminator);
        for (int ii = data->instructions->count - 1; ii >= 0; --ii) {
            pawMir_visit_instruction(V, K_LIST_GET(data->instructions, ii));
        }
        leave_basic_block(R);
    }
}

void pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, int *pmax_reg)
{
    struct MirVisitor V;
    struct RegisterAllocator R = {
        .code = pawMir_instruction_list_new(C),
        .active = regtab_new(C),
        .scopes = scope_stack_new(C),
        .mir = mir,
        .C = C,
    };
    R.moves = pawP_push_map(C);

    K_LIST_RESERVE(C, R.active, mir->registers->count);
    for (int i = 0; i < mir->registers->count; ++i) {
        K_LIST_PUSH(C, R.active, ((struct RegisterInfo){-1}));
    }
    for (int i = 0; i < NREGISTERS; ++i) {
        R.registers[i] = MIR_INVALID_REG;
    }

    pawMir_visitor_init(&V, C, mir, &R);
    V.PostVisitInstruction = debug_instruction;
//    V.PostVisitPhi = alloc_phi;
    V.PostVisitMove = alloc_move;
    V.PostVisitAllocLocal = alloc_alloc_local;
    V.PostVisitUpvalue = alloc_upvalue;
    V.PostVisitGlobal = alloc_global;
    V.PostVisitConstant = alloc_constant;
    V.PostVisitSetUpvalue = alloc_set_upvalue;
    V.PostVisitSetLocal = alloc_set_local;
    V.PostVisitAggregate = alloc_aggregate;
    V.PostVisitContainer = alloc_container;
    V.PostVisitCall = alloc_call;
    V.PostVisitCast = alloc_cast;
    V.PostVisitClosure = alloc_closure;
    V.PostVisitGetElement = alloc_get_element;
    V.PostVisitSetElement = alloc_set_element;
    V.PostVisitGetRange = alloc_get_range;
    V.PostVisitSetRange = alloc_set_range;
    V.PostVisitGetField = alloc_get_field;
    V.PostVisitSetField = alloc_set_field;
    V.PostVisitUnaryOp = alloc_unop;
    V.PostVisitBinaryOp = alloc_binop;

    V.VisitReturn = alloc_return;
    V.VisitForLoop = alloc_for_loop;
    V.VisitBranch = alloc_branch;
    V.VisitSwitch = alloc_switch;

    initialize_logger(&R, 10);

printf("%s\n", pawMir_dump(C, mir));--C->P->top.p;

    allocate_registers(&R, &V, order);
    *pmax_reg = R.max_reg;

    pawP_pop_object(C, R.moves);

printf("%s\n", pawMir_dump(C, mir));--C->P->top.p;
}

