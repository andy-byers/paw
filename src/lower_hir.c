// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// lower_hir.c: Translation from the high-level intermediate representation
//     (HIR) to the mid-level IR (MIR)
//
// MIR is a control-flow graph (CFG) of basic blocks, containing instructions
// in static single assignment (SSA) form.
//
// TODO: Implementation would be a bit nicer if closures were hoisted out of
//       their enclosing functions. Could keep a list of MIR bodies: functions,
//       methods, and closures. Closures also need to store the ".child_id" so
//       they can be placed in their enclosing Proto (so they can be found at
//       runtime).

#include "compile.h"
#include "api.h"
#include "lex.h"
#include "match.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include "ssa.h"

struct FunctionState {
    struct FunctionState *outer;
    struct MirRegisterDataList *registers;
    struct MirCaptureList *captured;
    struct MirRegisterList *locals;
    struct MirUpvalueList *up;
    struct BlockState *bs;
    struct LowerHir *L;
    struct Mir *mir;
    MirBlock current;
    Map *uses;
    Map *defs;
    Map *vars;
    paw_Bool sequential;
    int reg_level;
    int nlocals;
    int level;
};

struct LocalVar {
    MirRegister reg;
    String *name;
    DeclId did;
    int depth;
    int vid;
};

struct BlockState {
    struct BlockState *outer;
    int depth;
    int label0;
    int nvars;
    paw_Bool has_upvalue : 1;
    paw_Bool is_loop : 1;
};

struct MatchState {
    Map *var_mapping;
    struct MatchState *outer;
    struct MirRegisterList *regs;
    struct VariableList *vars;
    int offset;
};

struct Label {
    int nvars;
    MirBlock from;
    paw_Bool needs_close : 1;
    enum JumpKind kind : 7;
};

DEFINE_LIST(struct Compiler, var_stack_, VarStack, struct LocalVar)
DEFINE_LIST(struct Compiler, label_list_, LabelList, struct Label)

struct LowerHir {
    struct HirVisitor V;
    struct Compiler *C;
    Map *match_vars;
    Map *upvalues;
    paw_Env *P;

    struct MatchState *ms;
    struct FunctionState *fs;
    struct LabelList *labels;
    struct VarStack *stack;
};

static void enter_match(struct LowerHir *L, struct MatchState *ms, Map *var_mapping)
{
    *ms = (struct MatchState){
        .var_mapping = var_mapping,
        .regs = pawMir_register_list_new(L->C),
        .vars = variable_list_new(L->C),
        .outer = L->ms,
    };
    L->ms = ms;
}

static void leave_match(struct LowerHir *L)
{
    L->ms = L->ms->outer;
}

static struct IrType *get_type(struct LowerHir *L, paw_Type code)
{
    return GET_NODE_TYPE(L->C, pawHir_get_decl(L->C, (DeclId){.value = code}));
}

static struct IrType *basic_type(struct LowerHir *L, paw_Type code)
{
    // paw_Type == HirId for primitives only
    return pawIr_get_type(L->C, (HirId){code});
}

static struct MirBlockDataList *bb_list(struct LowerHir *L)
{
    paw_assert(L->fs->mir->blocks != NULL);
    return L->fs->mir->blocks;
}

static struct MirBlockData *get_bb(struct LowerHir *L, MirBlock bb)
{
    return K_LIST_GET(bb_list(L), bb.value);
}

static MirBlock current_bb(struct LowerHir *L)
{
    paw_assert(bb_list(L)->count > 0);
    return L->fs->current;
}

static struct MirBlockData *current_bb_data(struct LowerHir *L)
{
    paw_assert(bb_list(L)->count > 0);
    return K_LIST_GET(bb_list(L), current_bb(L).value);
}

static MirRegister new_register(struct LowerHir *L, struct IrType *type)
{
    struct MirRegisterDataList *regs = L->fs->mir->registers;
    K_LIST_PUSH(L->C, regs, ((struct MirRegisterData){
                    .type = type,
                }));
    return MIR_REG(regs->count - 1);
}

static MirRegister new_literal_reg(struct LowerHir *L, paw_Type code)
{
    return new_register(L, get_type(L, code));
}

static void add_edge(struct LowerHir *L, MirBlock from, MirBlock to)
{
    struct MirBlockData *source = get_bb(L, from);
    struct MirBlockData *target = get_bb(L, to);
    K_LIST_PUSH(L->C, source->successors, to);
    K_LIST_PUSH(L->C, target->predecessors, from);
}

// TODO: kinda pointless, since a new block is immediately entered after "return", "break", or "continue"
static paw_Bool is_unterminated(struct MirBlockData *block)
{
    if (block->instructions->count == 0) return PAW_TRUE;
    switch (MIR_KINDOF(K_LIST_LAST(block->instructions))) {
        case kMirReturn:
        case kMirBranch:
        case kMirSwitch:
        case kMirGoto:
            return PAW_FALSE;
        default:
            return PAW_TRUE;
    }
}

static void add_instruction(struct LowerHir *L, struct MirInstruction *instr)
{
    struct MirBlockData *block = current_bb_data(L);
    K_LIST_PUSH(L->C, block->instructions, instr);
}

static struct MirInstruction *new_instruction(struct LowerHir *L, enum MirInstructionKind kind)
{
    struct MirInstruction *instr = pawMir_new_instruction(L->C, kind);
    add_instruction(L, instr);
    return instr;
}

static struct MirInstruction *terminate_goto(struct LowerHir *L, MirBlock target)
{
    struct MirInstruction *r = new_instruction(L, kMirGoto);
    MirGetGoto(r)->target = target;
    return r;
}

static void maybe_goto(struct LowerHir *L, MirBlock to)
{
    struct MirBlockData *block = current_bb_data(L);
    if (is_unterminated(block)) {
        add_edge(L, current_bb(L), to);
        terminate_goto(L, to);
    }
}

static void set_current_bb(struct LowerHir *L, MirBlock b)
{
    L->fs->current = b;
}

static MirBlock new_bb(struct LowerHir *L)
{
    const int id = bb_list(L)->count;
    struct MirBlockData *bb = pawMir_new_block(L->C);
    K_LIST_PUSH(L->C, bb_list(L), bb);
    return (MirBlock){id};
}

static struct LocalVar *get_local_slot(struct FunctionState *fs, int index)
{
    return &K_LIST_GET(fs->L->stack, fs->level + index);
}

static void close_until_loop(struct FunctionState *fs)
{
    MirRegister lowest_upvalue;
    paw_Bool needs_close = PAW_FALSE;
    struct VarStack *stack = fs->L->stack;
    int index = stack->count - 1;
    struct BlockState *bs = fs->bs;
    while (bs != NULL) {
        needs_close |= bs->has_upvalue;
        if (bs->has_upvalue) {
            // find the upvalue with the smallest index in block "bs"
            for (; index >= bs->nvars; --index) {
                struct LocalVar var = K_LIST_GET(stack, index);
                struct MirRegisterData *data = mir_reg_data(fs->mir, var.reg);
                if (data->is_captured) lowest_upvalue = var.reg;
            }
        }
        if (bs->is_loop) break;
        bs = bs->outer;
    }
    if (needs_close) {
        struct MirInstruction *close = pawMir_new_close(fs->mir, -1, lowest_upvalue);
        add_instruction(fs->L, close);
    }
}

static void add_label(struct LowerHir *L, enum JumpKind kind)
{
    // MirClose must be added here if "continue" causes control to leave a scope containing
    // a captured variable, since the MirClose at the end of the loop body will not be reached.
    if (kind == JUMP_CONTINUE) close_until_loop(L->fs);
    struct MirBlockData *block = current_bb_data(L);
    terminate_goto(L, MIR_INVALID_BB);

    K_LIST_PUSH(L->C, L->labels, ((struct Label){
        .nvars = L->fs->nlocals,
        .from = current_bb(L),
        .kind = kind,
    }));
}

static void adjust_labels(struct LowerHir *L, struct BlockState *bs)
{
    struct LabelList *ll = L->labels;
    for (int i = bs->label0; i < ll->count; ++i) {
        struct Label *lb = &K_LIST_GET(ll, i);
        if (lb->nvars > bs->nvars) {
            lb->needs_close |= bs->has_upvalue;
        }
        lb->nvars = bs->nvars;
    }
}

static void remove_label(struct LabelList *ll, int index)
{
    paw_assert(ll->count > 0);
    K_LIST_SET(ll, index, K_LIST_LAST(ll));
    K_LIST_POP(ll);
}

static void set_goto(struct LowerHir *L, MirBlock from, MirBlock to)
{
    struct MirBlockData *bb = get_bb(L, from);
    struct MirInstruction *jump = K_LIST_LAST(bb->instructions);
    MirGetGoto(jump)->target = to;
}

static paw_Bool adjust_from(struct LowerHir *L, enum JumpKind kind)
{
    int needs_close = 0;
    struct BlockState *bs = L->fs->bs;
    struct LabelList *ll = L->labels;
    for (int i = bs->label0; i < ll->count;) {
        struct Label lb = K_LIST_GET(ll, i);
        if (lb.kind == kind) {
            needs_close |= lb.needs_close;
            set_goto(L, lb.from, current_bb(L));
            add_edge(L, lb.from, current_bb(L));
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
    return needs_close;
}

static void adjust_to(struct LowerHir *L, enum JumpKind kind, MirBlock to)
{
    struct BlockState *bs = L->fs->bs;
    struct LabelList *ll = L->labels;
    for (int i = bs->label0; i < ll->count;) {
        struct Label lb = K_LIST_GET(ll, i);
        if (lb.kind == kind) {
            set_goto(L, lb.from, to);
            add_edge(L, lb.from, to);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static struct MirInstruction *move_to(struct LowerHir *L, MirRegister target, MirRegister output)
{
    struct MirInstruction *r = new_instruction(L, kMirMove);
    MirGetMove(r)->output = output;
    MirGetMove(r)->target = target;
    return r;
}

static enum BuiltinKind kind_of_builtin(struct LowerHir *L, struct HirExpr *expr)
{
    struct IrType *type = GET_NODE_TYPE(L->C, expr);
    return pawP_type2code(L->C, type);
}

// Represents a local variable or an upvalue
struct NonGlobal {
    MirRegister r;
    int index;
    int vid;
    paw_Bool is_capture : 1;
    paw_Bool is_upvalue : 1;
};

static paw_Bool resolve_local(struct LowerHir *L, struct FunctionState *fs, DeclId did, const String *name, struct NonGlobal *pinfo)
{
    // condition is "i > 0" to cause the result register to be ignored
    for (int i = fs->nlocals - 1; i > 0; --i) {
        struct LocalVar *item = get_local_slot(fs, i);
        if (item->did.value == did.value
                || pawS_eq(name, item->name)) {
            *pinfo = (struct NonGlobal){
                .r = item->reg,
                .vid = item->vid,
                .index = i,
            };
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

// Mark a block as containing an upvalue
static void mark_upvalue(struct FunctionState *fs, int target, MirRegister r)
{
    struct BlockState *bs = fs->bs;
    while (bs->nvars > target) bs = bs->outer;
    bs->has_upvalue = PAW_TRUE;

    struct MirRegisterData *data = mir_reg_data(fs->mir, r);
    if (!data->is_captured) {
        K_LIST_PUSH(fs->L->C, fs->captured, ((struct MirCaptureInfo){r}));
        data->is_captured = PAW_TRUE;
        data->hint = r;
    }
}

static void add_upvalue(struct LowerHir *L, struct FunctionState *fs, struct NonGlobal *info, paw_Bool is_local)
{
    info->is_upvalue = PAW_TRUE;
    for (int i = 0; i < fs->up->count; ++i) {
        struct MirUpvalueInfo up = K_LIST_GET(fs->up, i);
        if (is_local != up.is_local) continue;
        if (is_local && up.index == info->vid) {
            info->vid = up.index;
            info->index = i;
            return;
        }
        if (!is_local && up.index == info->index) {
            info->index = i;
            info->vid = -1;
            return;
        }
    }
    if (fs->up->count == UPVALUE_MAX) {
        pawE_error(ENV(L->C), PAW_ESYNTAX, -1, "too many upvalues");
    }

    K_LIST_PUSH(L->C, fs->up, ((struct MirUpvalueInfo){
        .index = is_local ? info->vid : info->index,
        .is_local = is_local,
    }));
    // indicate new upvalue index
    info->index = fs->up->count - 1;
}

static paw_Bool resolve_upvalue(struct LowerHir *L, struct FunctionState *fs, DeclId did, String *name, struct NonGlobal *pinfo)
{
    struct FunctionState *caller = fs->outer;
    if (caller == NULL) return PAW_FALSE;
    if (resolve_local(L, caller, did, name, pinfo)) {
        mark_upvalue(caller, pinfo->index, pinfo->r);
        add_upvalue(L, fs, pinfo, PAW_TRUE);
        return PAW_TRUE;
    }
    if (resolve_upvalue(L, caller, did, name, pinfo)) {
        add_upvalue(L, fs, pinfo, PAW_FALSE);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void enter_block(struct LowerHir *L, struct BlockState *bs, paw_Bool is_loop)
{
    struct FunctionState *fs = L->fs;
    *bs = (struct BlockState){
        .depth = fs->bs == NULL ? 0 : fs->bs->depth + 1,
        .label0 = L->labels->count,
        .nvars = fs->nlocals,
        .is_loop = is_loop,
        .outer = fs->bs,
    };
    fs->bs = bs;
}

static void maybe_close(struct LowerHir *L, MirRegister r)
{
    struct MirRegisterData *data = mir_reg_data(L->fs->mir, r);
    if (data->is_captured) {
        struct MirInstruction *close = pawMir_new_close(L->fs->mir, -1, r);
        add_instruction(L, close);
    }
}

static void close_variables(struct LowerHir *L, int nvars)
{
    for (int i = L->fs->nlocals - 1; i >= nvars; --i) {
        const struct LocalVar local = K_LIST_GET(L->stack, L->fs->level + i);
        maybe_close(L, local.reg);
    }
}

static void leave_block(struct LowerHir *L)
{
    struct BlockState *bs = L->fs->bs;
    int needs_close = bs->has_upvalue && bs->outer != NULL;
    if (bs->is_loop) needs_close |= adjust_from(L, JUMP_BREAK);
    close_variables(L, bs->nvars);
    if (bs->outer != NULL) adjust_labels(L, bs);

    struct VarStack old = *L->stack;
    const int limit = L->fs->level + bs->nvars;
    L->fs->nlocals = bs->nvars;
    L->stack->count = limit;
    L->fs->bs = bs->outer;
}

static struct LocalVar *add_local(struct LowerHir *L, String *name, MirRegister r, DeclId did)
{
    K_LIST_PUSH(L->C, L->stack, ((struct LocalVar){
                .vid = L->fs->locals->count,
                .depth = L->fs->bs->depth,
                .name = name,
                .did = did,
                .reg = r,
            }));
    K_LIST_PUSH(L->C, L->fs->locals, r);
    ++L->fs->nlocals;
    return &K_LIST_LAST(L->stack);
}

static struct LocalVar *add_local_literal(struct LowerHir *L, const char *name, MirRegister r)
{
    return add_local(L, SCAN_STRING(L->C, name), r, NO_DECL);
}

static struct LocalVar *alloc_local(struct LowerHir *L, String *name, struct IrType *type, DeclId did)
{
    const MirRegister output = new_register(L, type);
    struct MirInstruction *instr = new_instruction(L, kMirAllocLocal);
    MirGetAllocLocal(instr)->output = output;
    MirGetAllocLocal(instr)->name = name;
    return add_local(L, name, output, did);
}

static paw_Bool resolve_nonglobal(struct LowerHir *L, DeclId did, String *name, struct NonGlobal *png)
{
    if (resolve_local(L, L->fs, did, name, png)) return PAW_TRUE;
    return resolve_upvalue(L, L->fs, did, name, png);
}

struct MirInstruction *terminate_return(struct LowerHir *L, const MirRegister *pvalue)
{
    MirRegister value;
    if (pvalue == NULL) {
        struct MirInstruction *k = new_instruction(L, kMirConstant);
        value = new_literal_reg(L, PAW_TUNIT);
        MirGetConstant(k)->output = value;
        MirGetConstant(k)->code = PAW_TUNIT;
    } else {
        value = *pvalue;
    }
    struct MirInstruction *r = new_instruction(L, kMirReturn);
    MirGetReturn(r)->value = value;
    return r;
}

struct MirInstruction *terminate_branch(struct LowerHir *L, MirRegister cond, MirBlock then_arm, MirBlock else_arm)
{
    struct MirInstruction *r = new_instruction(L, kMirBranch);
    MirGetBranch(r)->then_arm = then_arm;
    MirGetBranch(r)->else_arm = else_arm;
    MirGetBranch(r)->cond = cond;
    return r;
}

struct MirInstruction *terminate_switch(struct LowerHir *L, MirRegister discr, struct MirSwitchArmList *arms, MirBlock otherwise)
{
    struct MirInstruction *r = new_instruction(L, kMirSwitch);
    MirGetSwitch(r)->discr = discr;
    MirGetSwitch(r)->arms = arms;
    MirGetSwitch(r)->otherwise = otherwise;
    return r;
}

static MirRegister register_for_node(struct LowerHir *L, HirId hid)
{
    return new_register(L, pawIr_get_type(L->C, hid));
}

static void enter_function(struct LowerHir *L, struct FunctionState *fs, struct BlockState *bs,
        struct Mir *mir, Map *uses, Map *defs, DeclId did)
{
    *fs = (struct FunctionState){
        .captured = mir->captured,
        .registers = mir->registers,
        .up = pawMir_upvalue_list_new(L->C),
        .reg_level = mir->registers->count,
        .level = L->stack->count,
        .locals = mir->locals,
        .outer = L->fs,
        .uses = uses,
        .defs = defs,
        .mir = mir,
        .L = L,
    };
    L->fs = fs;

    // create the first basic block
    const MirBlock bb = new_bb(L);
    set_current_bb(L, bb);
    enter_block(L, bs, PAW_FALSE);

    // register 0 is used for the return value TODO: "did" should be "NO_DECL", this is not the function object anymore, recursion uses the global function object
    alloc_local(L, SCAN_STRING(L->C, "(result)"),
            IR_FPTR(mir->type)->result, did);
}

static void leave_function(struct LowerHir *L)
{
    struct MirBlockData *block = current_bb_data(L);
    if (is_unterminated(block)) terminate_return(L, NULL);
    L->stack->count = L->fs->level;
    L->fs = L->fs->outer;
}

static MirRegister last_register(struct LowerHir *L)
{
    return (MirRegister){L->fs->registers->count - 1};
}

static MirRegister lower_operand_(struct HirVisitor *V, struct HirExpr *expr);

static MirRegister lower_operand(struct HirVisitor *V, struct HirExpr *expr)
{
    return lower_operand_(V, expr);
}

#define LOWER_BLOCK(L, b) lower_operand(&(L)->V, HIR_CAST_EXPR(b))

static MirRegister operand_to_reg(struct HirVisitor *V, struct HirExpr *expr)
{
    const MirRegister oper = lower_operand(V, expr);
    const MirRegister r = register_for_node(V->ud, expr->hdr.hid);
    move_to(V->ud, oper, r);
    return r;
}

static struct MirInstruction *into_fresh_reg(struct LowerHir *L, MirRegister source)
{
    struct MirRegisterData data = K_LIST_GET(L->fs->mir->registers, source.value);
    const MirRegister target = new_register(L, data.type);
    return move_to(L, source, target);
}

static void lower_operand_list(struct HirVisitor *V, struct HirExprList *exprs, struct MirRegisterList *result)
{
    struct HirExpr **pexpr;
    struct LowerHir *L = V->ud;
    K_LIST_FOREACH(exprs, pexpr) {
        const MirRegister r = lower_operand(V, *pexpr);
        K_LIST_PUSH(L->C, result, r);
    }
}

static MirRegister result_reg(struct FunctionState *fs)
{
    return (MirRegister){fs->reg_level};
}

static struct MirInstruction *add_constant(struct LowerHir *L, Value value, paw_Type code)
{
    struct MirInstruction *r = new_instruction(L, kMirConstant);
    const MirRegister target = new_register(L, get_type(L, code));
    MirGetConstant(r)->output = target;
    MirGetConstant(r)->value = value;
    MirGetConstant(r)->code = code;
    return r;
}

static MirRegister unit_literal(struct LowerHir *L)
{
    struct MirInstruction *k = add_constant(L, I2V(0), PAW_TUNIT);
    return MirGetConstant(k)->output;
}

static paw_Bool visit_field_decl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    struct LowerHir *L = V->ud;
    struct IrType *type = pawIr_get_type(L->C, d->hid);
    alloc_local(V->ud, d->name, type, d->did);
    return PAW_FALSE;
}

static paw_Bool visit_var_decl(struct HirVisitor *V, struct HirVarDecl *d)
{
    struct LowerHir *L = V->ud;
    struct IrType *type = pawIr_get_type(L->C, d->hid);
    const MirRegister r = operand_to_reg(V, d->init);
    add_local(V->ud, d->name, r, d->did);
    return PAW_FALSE;
}

static MirRegister lower_basic_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = add_constant(L, e->basic.value, e->basic.t);
    return MirGetConstant(r)->output;
}

static MirRegister lower_tuple_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetAggregate(r)->nfields = e->tuple.elems->count;
    MirGetAggregate(r)->output = output;

    for (int i = 0; i < e->tuple.elems->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->tuple.elems, i);
        const MirRegister mir_field = lower_operand(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->object = output;
        MirGetSetField(setter)->value = mir_field;
        MirGetSetField(setter)->index = i;
    }

    return output;
}

static MirRegister lower_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetAggregate(r)->nfields = e->comp.items->count;
    MirGetAggregate(r)->output = output;

    for (int i = 0; i < e->comp.items->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->comp.items, i);
        const MirRegister mir_field = lower_operand(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->index = HirGetFieldExpr(hir_field)->fid;
        MirGetSetField(setter)->object = output;
        MirGetSetField(setter)->value = mir_field;
    }

    return output;
}

static MirRegister lower_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirContainer);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetContainer(r)->b_kind = kind_of_builtin(L, HIR_CAST_EXPR(e));
    MirGetContainer(r)->nelems = e->cont.items->count;
    MirGetContainer(r)->output = output;

    for (int i = 0; i < e->cont.items->count; ++i) {
        struct HirExpr *hir_elem = K_LIST_GET(e->cont.items, i);
        MirRegister mir_key, mir_value;
        if (HirIsFieldExpr(hir_elem)) {
            struct HirFieldExpr *elem = HirGetFieldExpr(hir_elem);
            mir_key = lower_operand(V, elem->key);
            mir_value = lower_operand(V, elem->value);
        } else {
            struct MirInstruction *index = add_constant(L, I2V(i), PAW_TINT);
            mir_key = MirGetConstant(index)->output;
            mir_value = lower_operand(V, hir_elem);
        }
        struct MirInstruction *setter = new_instruction(L, kMirSetElement);
        MirGetSetElement(setter)->b_kind = MirGetContainer(r)->b_kind;
        MirGetSetElement(setter)->object = MirGetContainer(r)->output;
        MirGetSetElement(setter)->key = mir_key;
        MirGetSetElement(setter)->value = mir_value;
    }

    return output;
}

static MirRegister lower_literal_expr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitBasic:
            return lower_basic_lit(V, e);
        case kHirLitTuple:
            return lower_tuple_lit(V, e);
        case kHirLitComposite:
            return lower_composite_lit(V, e);
        case kHirLitContainer:
            return lower_container_lit(V, e);
    }
}

static MirRegister lower_logical_expr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    struct LowerHir *L = V->ud;
    const MirBlock before_bb = current_bb(L);
    const MirBlock test_bb = new_bb(L);
    const MirBlock lhs_bb = new_bb(L);
    const MirBlock rhs_bb = new_bb(L);
    const MirBlock after_bb = new_bb(L);
    add_edge(L, before_bb, test_bb);
    terminate_goto(L, test_bb);
    set_current_bb(L, test_bb);

    const MirRegister result = new_literal_reg(L, PAW_TBOOL);
    const MirRegister first = lower_operand(V, e->lhs);
    add_edge(L, current_bb(L), lhs_bb);
    add_edge(L, current_bb(L), rhs_bb);

    struct MirInstruction *r = e->is_and
        ? terminate_branch(L, first, rhs_bb, lhs_bb)
        : terminate_branch(L, first, lhs_bb, rhs_bb);

    set_current_bb(L, lhs_bb);
    move_to(L, first, result);
    maybe_goto(L, after_bb);

    set_current_bb(L, rhs_bb);
    const MirRegister second = lower_operand(V, e->rhs);
    move_to(L, second, result);
    maybe_goto(L, after_bb);

    set_current_bb(L, after_bb);
    return result;
}

static MirRegister lower_unit_struct(struct HirVisitor *V, struct HirPathExpr *e, struct HirAdtDecl *d)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    const MirRegister object = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = object;
    MirGetAggregate(r)->nfields = 0;
    return object;
}

static MirRegister lower_unit_variant(struct HirVisitor *V, struct HirPathExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    const MirRegister object = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = object;
    MirGetAggregate(r)->nfields = 1;

    struct MirInstruction *discr = add_constant(L, I2V(d->index), PAW_TINT);
    struct MirInstruction *setter = new_instruction(L, kMirSetField);
    MirGetSetField(setter)->value = MirGetConstant(discr)->output;
    MirGetSetField(setter)->object = object;
    MirGetSetField(setter)->index = 0;
    return object;
}

static MirRegister lower_path_expr(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct LowerHir *L = V->ud;
    const DeclId did = HIR_PATH_RESULT(e->path);
    struct HirDecl *decl = pawHir_get_decl(L->C, did);
    struct NonGlobal ng;
    // TODO: The code that adds bindings to the locals stack doesn't know about DeclIds,
    //       so we have to provide the name for those cases. Figure out a better way: this
    //       is too hacky. There is a HirVarDecl for the binding, we just don't know where
    //       to find it in "declare_match_bindings". Consider finding a way to smuggle the
    //       DeclId in the match binding.
    if (resolve_nonglobal(L, did, decl->hdr.name, &ng)) {
        if (ng.is_upvalue) {
            const MirRegister target = register_for_node(L, e->hid);
            struct MirInstruction *r = new_instruction(L, kMirUpvalue);
            MirGetUpvalue(r)->output = target;
            MirGetUpvalue(r)->index = ng.index;
            return target;
        } else {
            return ng.r;
        }
    }
    const MirRegister output = register_for_node(L, e->hid);
    if (HirIsVariantDecl(decl)) {
        return lower_unit_variant(V, e, HirGetVariantDecl(decl));
    } else if (HirIsAdtDecl(decl)) {
        return lower_unit_struct(V, e, HirGetAdtDecl(decl));
    }
    struct MirInstruction *r = new_instruction(L, kMirGlobal);
    MirGetGlobal(r)->output = output;
    return output;
}

static void emit_get_field(struct LowerHir *L, MirRegister object, int index, MirRegister output)
{
    struct MirInstruction *r = new_instruction(L, kMirGetField);
    MirGetGetField(r)->output = output;
    MirGetGetField(r)->object = object;
    MirGetGetField(r)->index = index;
}

static struct MirSwitchArmList *allocate_switch_arms(struct LowerHir *L, MirBlock discr_bb, int count)
{
    struct MirSwitchArmList *arms = pawMir_switch_list_new(L->C);
    K_LIST_RESERVE(L->C, arms, count);
    for (int i = 0; i < count; ++i) {
        const MirBlock case_bb = new_bb(L);
        add_edge(L, discr_bb, case_bb);
        K_LIST_PUSH(L->C, arms, ((struct MirSwitchArm){
                        .bid = case_bb,
                    }));
    }
    return arms;
}

// Transformation:
//     opt?  =>  opt = if opt.0 != 0 {return opt;} else {opt.1}
static MirRegister lower_chain_expr(struct HirVisitor *V, struct HirChainExpr *e)
{
    struct LowerHir *L = V->ud;
    const MirBlock input_bb = current_bb(L);
    const MirBlock none_bb = new_bb(L);
    const MirBlock after_bb = new_bb(L);
    add_edge(L, input_bb, none_bb);

    const MirRegister object = lower_operand(V, e->target);
    const MirRegister discr = new_literal_reg(L, PAW_TINT);
    emit_get_field(L, object, 0, discr);

    struct MirSwitchArmList *arms = allocate_switch_arms(L, input_bb, 1);
    struct MirInstruction *switch_ = terminate_switch(L, discr, arms, none_bb);
    struct MirSwitchArm *arm = &K_LIST_GET(arms, 0);

    set_current_bb(L, arm->bid);
    const MirRegister value = register_for_node(L, e->hid);
    emit_get_field(L, object, 1, value);
    maybe_goto(L, after_bb);

    set_current_bb(L, none_bb);
    terminate_return(L, &object);

    set_current_bb(L, after_bb);
    return value;
}

static MirRegister lower_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct LowerHir *L = V->ud;
    const MirRegister val = lower_operand(V, e->target);
    struct MirInstruction *r = new_instruction(L, kMirUnaryOp);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetUnaryOp(r)->output = output;
    MirGetUnaryOp(r)->op = e->op;
    MirGetUnaryOp(r)->val = val;
    return output;
}

static MirRegister lower_concat_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    // TODO: in regalloc.c, detect this case and make sure registers are on top of register stack
    const MirRegister lhs = operand_to_reg(V, e->lhs);
    const MirRegister rhs = operand_to_reg(V, e->rhs);
    struct MirInstruction *r = new_instruction(L, kMirBinaryOp);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetBinaryOp(r)->output = output;
    MirGetBinaryOp(r)->op = e->op;
    MirGetBinaryOp(r)->lhs = lhs;
    MirGetBinaryOp(r)->rhs = rhs;
    return output;
}

static MirRegister lower_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    const paw_Type code = pawP_type2code(L->C, GET_NODE_TYPE(L->C, e->lhs));
    // OP_*CONCAT needs to be special-cased, since it causes memory allocations
    if (e->op == BINARY_ADD && (code == BUILTIN_STR || code == BUILTIN_LIST)) {
        return lower_concat_expr(V, e);
    }
    const MirRegister lhs = lower_operand(V, e->lhs);
    const MirRegister rhs = lower_operand(V, e->rhs);
    struct MirInstruction *r = new_instruction(L, kMirBinaryOp);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetBinaryOp(r)->output = output;
    MirGetBinaryOp(r)->op = e->op;
    MirGetBinaryOp(r)->lhs = lhs;
    MirGetBinaryOp(r)->rhs = rhs;
    return output;
}

static void lower_function_block(struct LowerHir *L, struct HirExpr *block)
{
    const MirRegister result = lower_operand(&L->V, block);
    const struct MirRegisterData *data = mir_reg_data(L->fs->mir, result);
    if (!HirGetBlock(block)->never) terminate_return(L, &result);
}

static void indicate_usedef(struct LowerHir *L, Map *map, MirRegister r, MirBlock where)
{
    struct MirBlockList *usedef = pawH_get(map, I2V(r.value))->p;
    for (int i = 0; i < usedef->count; ++i) {
        const MirBlock b = K_LIST_GET(usedef, i);
        if (MIR_BB_EQUALS(b, where)) return;
    }
    K_LIST_PUSH(L->C, usedef, where);
}

static void account_for_usedefs(struct LowerHir *L, struct MirInstruction *instr, Map *uses, Map *defs, MirBlock where)
{
    struct MirLoad load;
    struct MirStore store;
    MirRegister *const *ppr;

    if (pawMir_check_store(L->C, instr, &store)) {
        K_LIST_FOREACH(store.outputs, ppr) {
            indicate_usedef(L, defs, **ppr, where);
        }
    }
    if (pawMir_check_load(L->C, instr, &load)) {
        K_LIST_FOREACH(load.inputs, ppr) {
            indicate_usedef(L, uses, **ppr, where);
        }
    }
}

static void collect_uses_and_defs(struct LowerHir *L, struct Mir *mir, Map *uses, Map *defs)
{
    int index;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE(mir->registers, index, pdata) {
        MAP_INSERT(L, uses, I2V(index), P2V(pawMir_block_list_new(L->C)));
        MAP_INSERT(L, defs, I2V(index), P2V(pawMir_block_list_new(L->C)));
    }

    struct MirBlockData **pblock;
    K_LIST_ENUMERATE(mir->blocks, index, pblock) {
        struct MirBlockData *block = *pblock;
        const MirBlock b = MIR_BB(index);

        struct MirInstruction **pinstr;
        K_LIST_FOREACH(block->joins, pinstr) {
            account_for_usedefs(L, *pinstr, uses, defs, b);
        }
        K_LIST_FOREACH(block->instructions, pinstr) {
            account_for_usedefs(L, *pinstr, uses, defs, b);
        }
    }
}

static MirRegister lower_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct LowerHir *L = V->ud;
    struct Mir *outer = L->fs->mir;
    struct IrType *type = pawIr_get_type(L->C, e->hid);

    String *name = SCAN_STRING(L->C, "(closure)");
    struct Mir *result = pawMir_new(L->C, name, type, NULL,
            FUNC_CLOSURE, PAW_FALSE, PAW_FALSE, PAW_FALSE);

    struct BlockState bs;
    struct FunctionState fs;
    Map *uses = pawP_push_map(L->C);
    Map *defs = pawP_push_map(L->C);
    enter_function(L, &fs, &bs, result, uses, defs, NO_DECL);

    pawHir_visit_decl_list(&L->V, e->params);
    if (HirIsBlock(e->expr)) {
        lower_function_block(L, e->expr);
    } else {
        // evaluate and return the expression
        const MirRegister result = lower_operand(&L->V, e->expr);
        terminate_return(L, &result);
    }
    result->upvalues = L->fs->up;
    leave_function(L);

    pawMir_remove_unreachable_blocks(L->C, result);
    collect_uses_and_defs(L, result, uses, defs);
    pawSsa_construct(L->C, result, uses, defs);

    const MirRegister output = new_register(L, type);
    struct MirInstruction *r = new_instruction(L, kMirClosure);
    MirGetClosure(r)->child_id = outer->children->count;
    MirGetClosure(r)->output = output;
    K_LIST_PUSH(L->C, outer->children, result);
    pawP_pop_object(L->C, defs);
    pawP_pop_object(L->C, uses);
    return output;
}

static MirRegister lower_conversion_expr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    struct LowerHir *L = V->ud;
    const MirRegister target = lower_operand(V, e->arg);
    struct MirInstruction *r = new_instruction(L, kMirCast);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetCast(r)->output = output;
    MirGetCast(r)->target = target;
    MirGetCast(r)->from = kind_of_builtin(L, e->arg);
    MirGetCast(r)->to = e->to;
    return output;
}

static MirRegister lower_variant_constructor(struct HirVisitor *V, struct HirCallExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    const MirRegister object = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = object;
    MirGetAggregate(r)->nfields = 1 + e->args->count;

    struct MirInstruction *discr = add_constant(L, I2V(d->index), PAW_TINT);
    MirGetConstant(discr)->output = MirGetConstant(discr)->output;

    // set the discriminant: an 'int' residing in the first Value slot of the ADT
    struct MirInstruction *setter = new_instruction(L, kMirSetField);
    MirGetSetField(setter)->value = MirGetConstant(discr)->output;
    MirGetSetField(setter)->object = object;
    MirGetSetField(setter)->index = 0;

    for (int i = 0; i < e->args->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->args, i);
        const MirRegister mir_field = lower_operand(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->object = object;
        MirGetSetField(setter)->index = i + 1;
        MirGetSetField(setter)->value = mir_field;
    }

    return object;
}

static MirRegister lower_callee_and_args(struct HirVisitor *V, struct HirExpr *callee, struct HirExprList *args_in, struct MirRegisterList *args_out)
{
    struct LowerHir *L = V->ud;
    MirRegister result = register_for_node(L, callee->hdr.hid);
    if (HirIsSelector(callee) && !HirGetSelector(callee)->is_index) {
        struct HirSelector *select = HirGetSelector(callee);
        // method call: place function object before 'self'
        struct MirInstruction *r = new_instruction(L, kMirGlobal);
        MirGetGlobal(r)->output = result;
        const MirRegister self = lower_operand(V, select->target);
        K_LIST_PUSH(L->C, args_out, self);
    } else {
        result = lower_operand(V, callee);
    }
    lower_operand_list(V, args_in, args_out);
    return result;
}

static MirRegister lower_call_expr(struct HirVisitor *V, struct HirCallExpr *e)
{
    struct LowerHir *L = V->ud;
    struct IrType *target_type = GET_NODE_TYPE(L->C, e->target);
    if (IrIsSignature(target_type)) {
        struct HirDecl *decl = pawHir_get_decl(L->C, IR_TYPE_DID(target_type));
        if (HirIsVariantDecl(decl)) return lower_variant_constructor(V, e, HirGetVariantDecl(decl));
    }

    struct MirRegisterList *args = pawMir_register_list_new(L->C);
    const MirRegister target = lower_callee_and_args(V, e->target, e->args, args);
    struct MirInstruction *r = new_instruction(L, kMirCall);
    const MirRegister result = register_for_node(L, e->hid);
    MirGetCall(r)->output = result;
    MirGetCall(r)->target = target;
    MirGetCall(r)->args = args;
    return result;
}

static MirRegister lower_field_expr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    struct LowerHir *L = V->ud;
    if (e->fid < 0) lower_operand(V, e->key);
    return lower_operand(V, e->value);
}

static MirRegister first_slice_index(struct HirVisitor *V, struct HirExpr *e)
{
    if (e != NULL) return operand_to_reg(V, e);
    struct LowerHir *L = V->ud; // default to integer 0
    struct MirInstruction *r = add_constant(V->ud, I2V(0), PAW_TINT);
    return MirGetConstant(r)->output;
}

static MirRegister second_slice_index(struct HirVisitor *V, struct HirExpr *e, MirRegister object)
{
    if (e != NULL) return operand_to_reg(V, e);
    struct LowerHir *L = V->ud; // default to the number of elements
    struct MirInstruction *r = new_instruction(L, kMirUnaryOp);
    const MirRegister output = new_literal_reg(L, PAW_TINT);
    MirGetUnaryOp(r)->op = UNARY_LEN;
    MirGetUnaryOp(r)->output = output;
    MirGetUnaryOp(r)->val = object;
    return output;
}

static MirRegister lower_assign_expr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    if (HirIsPathExpr(e->lhs)) {
        struct NonGlobal ng;
        struct HirPathExpr *x = HirGetPathExpr(e->lhs);
        const paw_Bool expect_found = resolve_nonglobal(L, HIR_PATH_RESULT(x->path), NULL, &ng);
        paw_assert(expect_found); PAW_UNUSED(expect_found); // must be local or upvalue
        if (ng.is_upvalue) {
            const MirRegister rhs = lower_operand(V, e->rhs);
            struct MirInstruction *r = new_instruction(L, kMirSetUpvalue);
            MirGetSetUpvalue(r)->index = ng.index;
            MirGetSetUpvalue(r)->value = rhs;
        } else {
            const MirRegister value = lower_operand(V, e->rhs);
            struct MirInstruction *r = new_instruction(L, kMirSetLocal);
            MirGetSetLocal(r)->target = ng.r;
            MirGetSetLocal(r)->value = value;
        }
    } else if (HirIsSelector(e->lhs)) {
        struct HirSelector *x = HirGetSelector(e->lhs);
        const MirRegister target = lower_operand(V, x->target);
        const MirRegister rhs = lower_operand(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirSetField);
        MirGetSetField(r)->object = target;
        MirGetSetField(r)->index = x->index;
        MirGetSetField(r)->value = rhs;
    } else if (!HirGetIndex(e->lhs)->is_slice) {
        struct HirIndex *x = HirGetIndex(e->lhs);
        const MirRegister target = lower_operand(V, x->target);
        const MirRegister key = lower_operand(V, x->first);
        const MirRegister rhs = lower_operand(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirSetElement);
        MirGetSetElement(r)->b_kind = kind_of_builtin(L, x->target);
        MirGetSetElement(r)->object = target;
        MirGetSetElement(r)->key = key;
        MirGetSetElement(r)->value = rhs;
    } else {
        struct HirIndex *x = HirGetIndex(e->lhs);
        const MirRegister target = lower_operand(V, x->target);
        const MirRegister lower = first_slice_index(V, x->first);
        const MirRegister upper = second_slice_index(V, x->second, target);
        const MirRegister rhs = operand_to_reg(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirSetRange);
        MirGetSetRange(r)->b_kind = kind_of_builtin(L, x->target);
        MirGetSetRange(r)->object = target;
        MirGetSetRange(r)->lower = lower;
        MirGetSetRange(r)->upper = upper;
        MirGetSetRange(r)->value = rhs;
    }
    // setters are expressions that evaluate to "()"
    struct MirInstruction *k = new_instruction(L, kMirConstant);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetConstant(k)->output = output;
    return output;
}

static MirRegister lower_index_expr(struct HirVisitor *V, struct HirIndex *e)
{
    struct LowerHir *L = V->ud;
    const MirRegister object = lower_operand(V, e->target);
    if (!e->is_slice) {
        const MirRegister first = lower_operand(V, e->first);
        const MirRegister output = register_for_node(L, e->hid);
        struct MirInstruction *r = new_instruction(L, kMirGetElement);
        MirGetGetElement(r)->b_kind = kind_of_builtin(L, e->target);
        MirGetGetElement(r)->output = output;
        MirGetGetElement(r)->object = object;
        MirGetGetElement(r)->key = first;
        return output;
    } else { // stack: .. first second ..
        const MirRegister first = first_slice_index(V, e->first);
        const MirRegister second = second_slice_index(V, e->second, object);
        const MirRegister output = register_for_node(L, e->hid);
        struct MirInstruction *r = new_instruction(L, kMirGetRange);
        MirGetGetRange(r)->b_kind = kind_of_builtin(L, e->target);
        MirGetGetRange(r)->output = output;
        MirGetGetRange(r)->object = object;
        MirGetGetRange(r)->lower = first;
        MirGetGetRange(r)->upper = second;
        return output;
    }
}

static MirRegister lower_selector_expr(struct HirVisitor *V, struct HirSelector *e)
{
    paw_assert(e->is_index);
    struct LowerHir *L = V->ud;
    const MirRegister output = register_for_node(L, e->hid);
    const MirRegister object = lower_operand(V, e->target);
    emit_get_field(L, object, e->index, output);
    return output;
}

static MirRegister lower_block(struct HirVisitor *V, struct HirBlock *e)
{
    struct BlockState bs;
    struct LowerHir *L = V->ud;
    enter_block(L, &bs, PAW_FALSE);
    pawHir_visit_stmt_list(V, e->stmts);
    const MirRegister result = lower_operand(V, e->result);

    leave_block(L);
    return result;
}

static MirRegister lower_if_expr(struct HirVisitor *V, struct HirIfExpr *e)
{
    struct LowerHir *L = V->ud;
    const MirRegister cond = lower_operand(V, e->cond);
    const MirBlock cond_bb = current_bb(L);
    const MirBlock then_bb = new_bb(L);
    const MirBlock else_bb = new_bb(L);
    const MirBlock join_bb = new_bb(L);
    add_edge(L, cond_bb, then_bb);
    add_edge(L, cond_bb, else_bb);

    const MirRegister result = register_for_node(L, e->hid);
    struct MirInstruction *r = terminate_branch(L, cond, then_bb, else_bb);
    set_current_bb(L, then_bb);
    const MirRegister first = lower_operand(V, e->then_arm);
    move_to(L, first, result);
    maybe_goto(L, join_bb);

    set_current_bb(L, else_bb);
    if (e->else_arm == NULL) {
        const MirRegister second = unit_literal(L);
        move_to(L, second, result);
    } else {
        const MirRegister second = lower_operand(V, e->else_arm);
        move_to(L, second, result);
    }
    maybe_goto(L, join_bb);

    set_current_bb(L, join_bb);
    return result;
}

static void emit_call0(struct LowerHir *L, MirRegister target, MirRegister output)
{
    struct MirInstruction *r = new_instruction(L, kMirCall);
    MirGetCall(r)->args = pawMir_register_list_new(L->C);
    MirGetCall(r)->target = target;
    MirGetCall(r)->output = output;
}

static void emit_call1(struct LowerHir *L, MirRegister target, MirRegister arg, MirRegister output)
{
    struct MirInstruction *r = new_instruction(L, kMirCall);
    struct MirRegisterList *args = pawMir_register_list_new(L->C);
    K_LIST_PUSH(L->C, args, arg);
    MirGetCall(r)->args = args;
    MirGetCall(r)->target = target;
    MirGetCall(r)->output = output;
}

static MirRegister lower_loop_expr(struct HirVisitor *V, struct HirLoopExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    const MirRegister result = unit_literal(L);
    const MirBlock before_bb = current_bb(L);
    const MirBlock header_bb = new_bb(L);
    const MirBlock after_bb = new_bb(L);
    add_edge(L, before_bb, header_bb);
    terminate_goto(L, header_bb);

    struct BlockState bs;
    enter_block(L, &bs, PAW_TRUE);

    set_current_bb(L, header_bb);
    lower_operand(V, e->block);

    const MirBlock loop_bb = current_bb(L);
    maybe_goto(L, header_bb);

    adjust_to(L, JUMP_CONTINUE, header_bb);
    set_current_bb(L, after_bb);

    leave_block(L);
    return result;
}

static paw_Bool is_within_loop(struct LowerHir *L)
{
    struct BlockState *bs = L->fs->bs;
    while (bs != NULL) {
        if (bs->is_loop) return PAW_TRUE;
        bs = bs->outer;
    }
    return PAW_FALSE;
}

static MirRegister lower_jump_expr(struct HirVisitor *V, struct HirJumpExpr *e)
{
    struct LowerHir *L = V->ud;
    if (!is_within_loop(L)) {
        pawE_error(ENV(L->C), PAW_ESYNTAX, e->line, "%s outside loop",
                e->jump_kind == JUMP_BREAK ? "break" : "continue");
    }
    add_label(L, e->jump_kind);
    set_current_bb(L, new_bb(L));
    return unit_literal(L);
}

static MirRegister lower_return_expr(struct HirVisitor *V, struct HirReturnExpr *e)
{
    // TODO: save blocks ending with return, use a goto to jump control to the
    //       end of the function, place a single return there
    struct LowerHir *L = V->ud;
    if (e->expr != NULL) {
        const MirRegister value = lower_operand(V, e->expr);
        terminate_return(L, &value);
    } else {
        // creates a dummy register for a value of type "()"
        terminate_return(L, NULL);
    }
    // NOTE: This block is needed if there is unreachable code after the "return".
    //       It will be eliminated in a later pass.
    const MirBlock next_bb = new_bb(L);
    set_current_bb(L, next_bb);
    return unit_literal(L);
}

static MirRegister lower_match_expr(struct HirVisitor *V, struct HirMatchExpr *e);

static MirRegister lower_operand_(struct HirVisitor *V, struct HirExpr *expr)
{
    switch (HIR_KINDOF(expr)) {
        case kHirLiteralExpr:
            return lower_literal_expr(V, HirGetLiteralExpr(expr));
        case kHirLogicalExpr:
            return lower_logical_expr(V, HirGetLogicalExpr(expr));
        case kHirPathExpr:
            return lower_path_expr(V, HirGetPathExpr(expr));
        case kHirChainExpr:
            return lower_chain_expr(V, HirGetChainExpr(expr));
        case kHirUnOpExpr:
            return lower_unop_expr(V, HirGetUnOpExpr(expr));
        case kHirBinOpExpr:
            return lower_binop_expr(V, HirGetBinOpExpr(expr));
        case kHirClosureExpr:
            return lower_closure_expr(V, HirGetClosureExpr(expr));
        case kHirConversionExpr:
            return lower_conversion_expr(V, HirGetConversionExpr(expr));
        case kHirCallExpr:
            return lower_call_expr(V, HirGetCallExpr(expr));
        case kHirIndex:
            return lower_index_expr(V, HirGetIndex(expr));
        case kHirSelector:
            return lower_selector_expr(V, HirGetSelector(expr));
        case kHirFieldExpr:
            return lower_field_expr(V, HirGetFieldExpr(expr));
        case kHirAssignExpr:
            return lower_assign_expr(V, HirGetAssignExpr(expr));
        case kHirIfExpr:
            return lower_if_expr(V, HirGetIfExpr(expr));
        case kHirReturnExpr:
            return lower_return_expr(V, HirGetReturnExpr(expr));
        case kHirJumpExpr:
            return lower_jump_expr(V, HirGetJumpExpr(expr));
        case kHirLoopExpr:
            return lower_loop_expr(V, HirGetLoopExpr(expr));
        case kHirMatchExpr:
            return lower_match_expr(V, HirGetMatchExpr(expr));
        case kHirBlock:
            return lower_block(V, HirGetBlock(expr));
        case kHirMatchArm:
            PAW_UNREACHABLE();
    }
}

static paw_Bool visit_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    lower_operand(V, s->expr);
    return PAW_FALSE;
}

static void patch_to_here(struct LowerHir *L, struct MirBlockList *sources)
{
    const MirBlock before = current_bb(L);
    const MirBlock to = new_bb(L);
    for (int i = 0; i < sources->count; ++i) {
        const MirBlock from = K_LIST_GET(sources, i);
        add_edge(L, from, to);
    }
}

static void save_patch_source(struct LowerHir *L, struct MirBlockList *sources)
{
    struct MirBlockData *block = current_bb_data(L);
    if (is_unterminated(block)) {
        const MirBlock bb = current_bb(L);
        terminate_goto(L, MIR_INVALID_BB);
        K_LIST_PUSH(L->C, sources, bb);
    }
}

static MirRegister get_test_reg(struct LowerHir *L, struct MatchVar v)
{
    const Value *pval = pawH_get(L->ms->var_mapping, I2V(v.id));
    paw_assert(pval != NULL);
    return (MirRegister){CAST(int, pval->i)};
}

static void declare_match_bindings(struct LowerHir *L, struct BindingList *bindings)
{
    for (int i = 0; i < bindings->count; ++i) {
        struct Binding b = K_LIST_GET(bindings, i);
        const MirRegister r = get_test_reg(L, b.var);
        into_fresh_reg(L, r);

        add_local(L, b.name, r, NO_DECL);
    }
}

static MirBlock lower_match_body(struct HirVisitor *V, struct MatchBody body, MirRegister result);
static void visit_decision(struct HirVisitor *V, struct Decision *d, MirRegister result);

static void visit_success(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;
    lower_match_body(V, d->success.body, result);
}

static void visit_guard(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;

    // steal bindings from the body of the guard, since they may be referenced in
    // the conditional expression
    struct BindingList *bindings = d->guard.body.bindings;
    declare_match_bindings(L, bindings);
    bindings->count = 0;

    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    const MirRegister cond = lower_operand(V, d->guard.cond);
    const MirBlock before_bb = current_bb(L);
    const MirBlock then_bb = new_bb(L);
    const MirBlock else_bb = new_bb(L);
    const MirBlock join_bb = new_bb(L);
    add_edge(L, before_bb, then_bb);
    add_edge(L, before_bb, else_bb);

    struct MirInstruction *branch = terminate_branch(L, cond, then_bb, else_bb);
    set_current_bb(L, then_bb);
    lower_match_body(V, d->guard.body, result);
    maybe_goto(L, join_bb);

    set_current_bb(L, else_bb);
    visit_decision(V, d->guard.rest, result);
    maybe_goto(L, join_bb);

    set_current_bb(L, join_bb);
}

static MirBlock lower_match_body(struct HirVisitor *V, struct MatchBody body, MirRegister result)
{
    struct LowerHir *L = V->ud;
    const MirBlock bid = current_bb(L);
    declare_match_bindings(L, body.bindings);
    const MirRegister r = lower_operand(V, body.result);
    move_to(L, r, result);
    return bid;
}

static struct IrType *get_field_type(struct LowerHir *L, struct IrType *type, int index)
{
    if (IrIsTuple(type)) {
        struct IrTuple *tuple = IrGetTuple(type);
        return K_LIST_GET(tuple->elems, index);
    }
    struct HirDecl *decl = pawHir_get_decl(L->C, IR_TYPE_DID(type));
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    struct HirDecl *field = K_LIST_GET(d->fields, index);
    return GET_NODE_TYPE(L->C, field);
}

static struct IrTypeList *collect_field_types(struct LowerHir *L, struct IrType *type)
{
    if (IrIsTuple(type)) {
        struct IrTuple *tuple = IrGetTuple(type);
        return tuple->elems;
    } else if (IR_IS_BASIC_T(type)) {
        return NULL;
    }
    struct HirDecl *decl = pawHir_get_decl(L->C, IR_TYPE_DID(type));
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    struct IrTypeList *result = pawIr_type_list_new(L->C);
    if (!d->is_struct) {
        // first field of an enumerator is the integer discriminant
        K_LIST_PUSH(L->C, result, get_type(L, PAW_TINT));
    }
    for (int i = 0; i < d->fields->count; ++i) {
        struct HirDecl *field_decl = K_LIST_GET(d->fields, i);
        struct IrType *field_type = pawP_instantiate_field(L->C, type, field_decl);
        K_LIST_PUSH(L->C, result, field_type);
    }
    return result;
}

static struct MirRegisterList *allocate_registers(struct LowerHir *L, struct IrTypeList *types)
{
    struct MirRegisterList *result = pawMir_register_list_new(L->C);
    for (int i = 0; i < types->count; ++i) {
        const MirRegister r = new_register(L, K_LIST_GET(types, i));
        K_LIST_PUSH(L->C, result, r);
    }
    return result;
}

static void map_var_to_reg(struct LowerHir *L, struct MatchVar var, MirRegister reg)
{
    pawH_insert(ENV(L->C), L->ms->var_mapping, I2V(var.id), I2V(reg.value));
}

static void allocate_match_vars(struct LowerHir *L, MirRegister discr, struct MatchCase mc, paw_Bool is_enum)
{
    if (mc.vars->count == 0) return;
    struct MirRegisterList *regs = L->ms->regs;
    for (int i = 0; i < mc.vars->count; ++i) {
        struct MatchVar v = K_LIST_GET(mc.vars, i);
        const MirRegister r = new_register(L, v.type);
        map_var_to_reg(L, v, r);
        emit_get_field(L, discr, is_enum + i, r);
        add_local_literal(L, "(match variable)", r);
    }
}

static void visit_sparse_cases(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;
    struct CaseList *cases = d->multi.cases;
    const MirBlock discr_bb = current_bb(L);
    const MirBlock otherwise_bb = new_bb(L);
    const MirBlock join_bb = new_bb(L);
    add_edge(L, discr_bb, otherwise_bb);

    const MirRegister test = get_test_reg(L, d->multi.test);
    struct MirSwitchArmList *arms = allocate_switch_arms(L, discr_bb, cases->count);
    struct MirInstruction *switch_ = terminate_switch(L, test, arms, otherwise_bb);

    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    for (int i = 0; i < cases->count; ++i) {
        struct MatchCase mc = K_LIST_GET(cases, i);
        struct MirSwitchArm *arm = &K_LIST_GET(arms, i);
        set_current_bb(L, arm->bid);
        arm->value = mc.cons.value;

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        visit_decision(V, mc.dec, result);
        leave_block(L);

        maybe_goto(L, join_bb);
    }

    // this implementation requires an "otherwise" case (binding or wildcard) to ensure
    // exhaustivness
    paw_assert(d->multi.rest != NULL);
    set_current_bb(L, otherwise_bb);
    visit_decision(V, d->multi.rest, result);
    save_patch_source(L, patch);
    maybe_goto(L, join_bb);

    set_current_bb(L, join_bb);
}

static void visit_variant_cases(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;
    struct CaseList *cases = d->multi.cases;

    const MirBlock discr_bb = current_bb(L);
    const MirBlock join_bb = new_bb(L);
    const MirRegister variant = get_test_reg(L, d->multi.test);
    const MirRegister test = new_literal_reg(L, PAW_TINT);
    add_local_literal(L, "(match discriminant)", test); // keep alive
    emit_get_field(L, variant, 0, test);

    struct MirSwitchArmList *arms = allocate_switch_arms(L, discr_bb, cases->count);
    struct MirInstruction *switch_ = terminate_switch(L, test, arms, MIR_INVALID_BB);

    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    for (int i = 0; i < cases->count; ++i) {
        struct MatchCase mc = K_LIST_GET(cases, i);
        struct MirSwitchArm *arm = &K_LIST_GET(arms, i);
        arm->value.i = mc.cons.variant.index;
        set_current_bb(L, arm->bid);

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        allocate_match_vars(L, variant, mc, PAW_TRUE);
        visit_decision(V, mc.dec, result);
        leave_block(L);

        maybe_goto(L, join_bb);
    }
    paw_assert(d->multi.rest == NULL);

    set_current_bb(L, join_bb);
}

static void visit_tuple_case(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;
    const MirRegister discr = get_test_reg(L, d->multi.test);

    paw_assert(d->multi.rest == NULL);
    paw_assert(d->multi.cases->count == 1);
    struct MatchCase mc = K_LIST_FIRST(d->multi.cases);

    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);
    allocate_match_vars(L, discr, mc, PAW_FALSE);
    visit_decision(V, mc.dec, result);
    leave_block(L);
}

static void visit_struct_case(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;
    const MirRegister discr = get_test_reg(L, d->multi.test);

    paw_assert(d->multi.rest == NULL);
    paw_assert(d->multi.cases->count == 1);
    struct MatchCase mc = K_LIST_FIRST(d->multi.cases);

    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);
    allocate_match_vars(L, discr, mc, PAW_FALSE);
    visit_decision(V, mc.dec, result);
    leave_block(L);
}

static void visit_multiway(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;

    // there must exist at least 1 case; all cases have the same kind of constructor
    struct MatchCase first_case = K_LIST_FIRST(d->multi.cases);
    switch (first_case.cons.kind) {
        case CONS_WILDCARD:
            break;
        case CONS_BOOL:
        case CONS_INT:
        case CONS_FLOAT:
        case CONS_STR:
            visit_sparse_cases(V, d, result);
            break;
        case CONS_VARIANT:
            visit_variant_cases(V, d, result);
            break;
        case CONS_TUPLE:
            visit_tuple_case(V, d, result);
            break;
        case CONS_STRUCT:
            visit_struct_case(V, d, result);
            break;
        case CONS_REST:
            PAW_UNREACHABLE();
    }
}

static void visit_decision(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;

    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);

    switch (d->kind) {
        case DECISION_FAILURE:
            paw_assert(0);
            return;
        case DECISION_SUCCESS:
            visit_success(V, d, result);
            break;
        case DECISION_GUARD:
            visit_guard(V, d, result);
            break;
        case DECISION_MULTIWAY:
            visit_multiway(V, d, result);
            break;
    }

    leave_block(L);
}

static MirRegister lower_match_expr(struct HirVisitor *V, struct HirMatchExpr *e)
{
    struct LowerHir *L = V->ud;
    Map *var_mapping = pawP_push_map(L->C);

    struct BlockState bs;
    struct MatchState ms;
    enter_block(L, &bs, PAW_FALSE);
    enter_match(L, &ms, var_mapping);

    struct Decision *d = pawP_check_exhaustiveness(L->C, e, ms.vars);
    paw_assert(ms.vars->count > 0);
    const MirRegister discr = lower_operand(V, e->target);
    const MirRegister result = register_for_node(L, e->hid);
    map_var_to_reg(L, K_LIST_FIRST(ms.vars), discr);
    add_local_literal(L, "(match target)", discr);
    K_LIST_PUSH(L->C, ms.regs, discr);

    visit_decision(V, d, result);

    leave_match(L);
    leave_block(L);

    pawP_pop_object(L->C, var_mapping);
    return result;
}

static void lower_hir_body(struct LowerHir *L, struct HirFuncDecl *func, Map *uses, Map *defs, struct Mir *mir)
{
    struct BlockState bs;
    struct FunctionState fs;
    enter_function(L, &fs, &bs, mir, uses, defs, func->did);

    pawHir_visit_decl_list(&L->V, func->params);
    lower_function_block(L, func->body);

    leave_function(L);
}

struct Mir *pawP_lower_hir_body(struct Compiler *C, struct HirFuncDecl *func)
{
    struct IrType *type = pawIr_get_type(C, func->hid);
    const paw_Bool is_polymorphic = func->generics != NULL
        || (func->self != NULL && ir_adt_types(func->self) != NULL);
    struct Mir *result = pawMir_new(C, func->name, type,
            func->self, func->fn_kind, func->body == NULL,
            func->is_pub, is_polymorphic);
    if (func->body == NULL) return result;

    struct LowerHir L = {
        .labels = label_list_new(C),
        .stack = var_stack_new(C),
        .P = ENV(C),
        .C = C,
    };
    pawHir_visitor_init(&L.V, C, &L);

    L.V.VisitFieldDecl = visit_field_decl;
    L.V.VisitVarDecl = visit_var_decl;
    L.V.VisitExprStmt = visit_expr_stmt;

    L.match_vars = pawP_push_map(C);
    L.upvalues = pawP_push_map(C);
    Map *uses = pawP_push_map(C);
    Map *defs = pawP_push_map(C);

    lower_hir_body(&L, func, uses, defs, result);
    pawMir_remove_unreachable_blocks(C, result);
    collect_uses_and_defs(&L, result, uses, defs);
    pawSsa_construct(C, result, uses, defs);

    pawP_pop_object(C, defs);
    pawP_pop_object(C, uses);
    pawP_pop_object(C, L.upvalues);
    pawP_pop_object(C, L.match_vars);
    return result;
}

Map *pawP_lower_hir(struct Compiler *C)
{
    Map *result = pawP_push_map(C);
    struct HirDeclList *decls = C->decls;
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(decls, i);
        if (HirIsFuncDecl(decl)) {
            struct HirFuncDecl *d = HirGetFuncDecl(decl);
            struct Mir *r = pawP_lower_hir_body(C, d);
            pawH_insert(ENV(C), result, I2V(d->did.value), P2V(r));
        }
    }
    // 'result' should be on top of the stack
    paw_assert(ENV(C)->top.p[-1].p == result);
    return result;
}

