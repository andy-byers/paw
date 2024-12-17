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
// TODO: just broke closures... ".defs" map is stored in the LowerHir context
//       rather than FunctionState, since it is needed after leave_function
//       is called. closures need to convert themselves to ssa also. need to
//       handle closures like any other function: they cannot be stored nested
//       inside other functions (.children field).

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
    struct MirUpvalueList *up;
    struct BlockState *bs;
    struct LowerHir *L;
    paw_Bool sequential;
    MirScopeId scope_id;
    int reg_level;
    int nlocals;
    int level;
};

struct LocalVar {
    MirRegister reg;
    String *name;
    DeclId did;
    int vid;
};

struct BlockState {
    struct BlockState *outer;
    int label0;
    int nvars;
    MirScopeId scope_id;
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
    MirBlock *ptarget;
    MirBlock source;
    int nvars;
    paw_Bool needs_close : 1;
    enum JumpKind kind : 7;
};

DEFINE_LIST(struct Compiler, var_stack_, VarStack, struct LocalVar)
DEFINE_LIST(struct Compiler, label_list_, LabelList, struct Label)

struct LowerHir {
    struct HirVisitor V;
    struct Compiler *C;
    struct Mir *mir;
    Map *match_vars;
    Map *defs;
    Map *vars;
    paw_Env *P;

    struct MirRegisterList *locals;
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

static MirRegister new_register(struct LowerHir *L, struct IrType *type)
{
    struct MirRegisterDataList *regs = L->mir->registers;
    K_LIST_PUSH(L->C, regs, ((struct MirRegisterData){type}));
    return MIR_REG(regs->count - 1);
}

static MirRegister new_literal_reg(struct LowerHir *L, paw_Type code)
{
    return new_register(L, get_type(L, code));
}

static struct MirBlockDataList *bb_list(struct LowerHir *L)
{
    paw_assert(L->mir->blocks != NULL);
    return L->mir->blocks;
}

static struct MirBlockData *get_bb(struct LowerHir *L, MirBlock bb)
{
    return K_LIST_GET(bb_list(L), bb.value);
}

static struct MirBlockData *current_bb_data(struct LowerHir *L)
{
    paw_assert(bb_list(L)->count > 0);
    return K_LIST_LAST(bb_list(L));
}

static MirBlock current_bb(struct LowerHir *L)
{
    paw_assert(bb_list(L)->count > 0);
    return (MirBlock){bb_list(L)->count - 1};
}

static void terminate_bb(struct LowerHir *L, struct MirTerminator *terminator)
{
    struct MirBlockData *bb = get_bb(L, current_bb(L));
    paw_assert(bb->terminator == NULL);
    bb->terminator = terminator;
}

static struct MirTerminator *terminate_goto(struct LowerHir *L, MirBlock bid)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirGoto);
    MirGetGoto(r)->target = bid;
    terminate_bb(L, r);
    return r;
}

static MirBlock next_bb(struct LowerHir *L)
{
    const int id = bb_list(L)->count;
    struct MirBlockData *bb = pawMir_new_block(L->C);
    K_LIST_PUSH(L->C, bb_list(L), bb);
    return (MirBlock){id};
}

static void add_predecessor(struct LowerHir *L, MirBlock target, MirBlock predecessor)
{
    struct MirBlockData *t = get_bb(L, target);
    K_LIST_PUSH(L->C, t->predecessors, predecessor);
}

static MirBlock advance_bb(struct LowerHir *L)
{
    MirBlock *target = NULL;
    const MirBlock current = current_bb(L);
    struct MirBlockData *current_bb = get_bb(L, current);
    if (current_bb->terminator == NULL) {
        struct MirTerminator *terminator = terminate_goto(L, MIR_INVALID_BB);
        target = &MirGetGoto(terminator)->target;
    }
    struct MirBlock next = next_bb(L);
    if (target != NULL) {
        add_predecessor(L, next, current);
        *target = next;
    }
    return next;
}

static void add_label(struct LowerHir *L, enum JumpKind kind)
{
    struct MirTerminator *terminator = terminate_goto(L, MIR_INVALID_BB);
    K_LIST_PUSH(L->C, L->labels, ((struct Label){
        .ptarget = &MirGetGoto(terminator)->target,
        .nvars = L->fs->nlocals,
        .source = current_bb(L),
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
    for (int i = index; i < ll->count - 1; ++i) {
        ll->data[i] = ll->data[i + 1];
    }
    --ll->count;
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
            *lb.ptarget = current_bb(L);
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
            *lb.ptarget = to;
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static struct MirTerminator *current_terminator(struct LowerHir *L)
{
    return current_bb_data(L)->terminator;
}

static struct MirInstruction *new_instruction(struct LowerHir *L, enum MirInstructionKind kind)
{
    struct MirInstruction *r = pawMir_new_instruction(L->C, kind);
    K_LIST_PUSH(L->C, current_bb_data(L)->instructions, r);
    return r;
}

static struct MirInstruction *move_to(struct LowerHir *L, MirRegister source, MirRegister target)
{
    struct MirInstruction *r = new_instruction(L, kMirMove);
    MirGetMove(r)->output = target;
    MirGetMove(r)->target = source;
    return r;
}

// Represents the most-recent version of a local variable, or an upvalue
struct NonGlobal {
    MirRegister r;
    int index;
    paw_Bool is_upvalue : 1;
};

static struct LocalVar *get_local_slot(struct FunctionState *fs, int index)
{
    return &K_LIST_GET(fs->L->stack, fs->level + index);
}

static paw_Bool resolve_local(struct LowerHir *L, struct FunctionState *fs, DeclId did, const String *name, struct NonGlobal *pinfo)
{
    for (int i = fs->nlocals - 1; i >= 0; --i) {
        struct LocalVar *item = get_local_slot(fs, i);
        if (item->did.value == did.value
                || pawS_eq(name, item->name)) {
            *pinfo = (struct NonGlobal){
                .r = item->reg,
                .index = i,
            };
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

// TODO: this whole thing will only work if upvalues are kept in order
//       otherwise, upvalues will need to be closed individually

// Mark a block as containing an upvalue
static void mark_upvalue(struct FunctionState *fs, int nvars)
{
    struct BlockState *bs = fs->bs;
    while (bs->nvars > nvars) {
        bs = bs->outer;
    }
    bs->has_upvalue = PAW_TRUE;
}

static void add_upvalue(struct LowerHir *L, struct FunctionState *fs, struct NonGlobal *info, paw_Bool is_local)
{
    info->is_upvalue = PAW_TRUE;
    for (int i = 0; i < fs->up->count; ++i) {
        struct MirUpvalueInfo up = K_LIST_GET(fs->up, i);
        if (up.index == info->index && up.is_local == is_local) {
            info->index = i;
            return;
        }
    }
    if (fs->up->count == UPVALUE_MAX) {
        pawE_error(ENV(L->C), PAW_ESYNTAX, -1, "too many upvalues");
    }

    K_LIST_PUSH(L->C, fs->up, ((struct MirUpvalueInfo){
        .index = info->index,
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
        mark_upvalue(caller, pinfo->index);
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
        .scope_id = fs->scope_id,
        .label0 = L->labels->count,
        .nvars = fs->nlocals,
        .is_loop = is_loop,
        .outer = fs->bs,
    };
    fs->bs = bs;

    struct MirScope scope = {
        .nlocals = fs->nlocals,
        .outer = bs->outer != NULL
            ? bs->outer->scope_id
            : MIR_NO_SCOPE,
        .id = bs->scope_id,
        .is_loop = is_loop,
    };
    K_LIST_PUSH(L->C, L->mir->scopes, scope);
    ++fs->scope_id.value;
}

static void leave_block(struct LowerHir *L)
{
    struct BlockState *bs = L->fs->bs;
    int needs_close = bs->has_upvalue && bs->outer != NULL;
    if (bs->is_loop) needs_close |= adjust_from(L, JUMP_BREAK);
    if (bs->outer != NULL) adjust_labels(L, bs);
    struct MirScope *scope = pawMir_get_scope(L->mir, bs->scope_id);
    scope->needs_close = needs_close;

    struct VarStack old = *L->stack;
    const int limit = L->fs->level + bs->nvars;
    L->fs->nlocals = bs->nvars;
    L->stack->count = limit;
    L->fs->bs = bs->outer;
}

static struct LocalVar *add_local(struct LowerHir *L, String *name, MirRegister r, DeclId did)
{
    struct MirBlockList *bucket = pawMir_block_list_new(L->C);
    K_LIST_PUSH(L->C, bucket, current_bb(L));
    pawH_insert(ENV(L), L->defs, I2V(L->fs->nlocals), P2V(bucket));
    pawH_insert(ENV(L), L->vars, I2V(r.value), I2V(L->fs->nlocals));

    K_LIST_PUSH(L->C, L->stack, ((struct LocalVar){
                .vid = L->fs->nlocals,
                .name = name,
                .did = did,
                .reg = r,
            }));
    K_LIST_PUSH(L->C, L->locals, r);
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

static void terminate(struct LowerHir *L, MirBlock bb, struct MirTerminator *terminator)
{
    get_bb(L, bb)->terminator = terminator;
}

struct MirTerminator *terminate_return(struct LowerHir *L, const MirRegister *pvalue)
{
    const MirRegister value = pvalue == NULL ? new_literal_reg(L, PAW_TUNIT) : *pvalue;
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirReturn);
    MirGetReturn(r)->value = value;
    terminate_bb(L, r);
    return r;
}

struct MirTerminator *terminate_branch(struct LowerHir *L, MirRegister cond)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirBranch);
    MirGetBranch(r)->cond = cond;
    terminate_bb(L, r);
    return r;
}

struct MirTerminator *terminate_for_loop(struct LowerHir *L, enum MirForKind for_kind, MirRegister var, MirRegister iter, MirRegister end, MirRegister step)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirForLoop);
    MirGetForLoop(r)->for_kind = for_kind;
    MirGetForLoop(r)->var = var;
    MirGetForLoop(r)->iter = iter;
    MirGetForLoop(r)->end = end;
    MirGetForLoop(r)->step = step;
    terminate_bb(L, r);
    return r;
}

struct MirTerminator *terminate_switch(struct LowerHir *L, MirRegister discr)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirSwitch);
    MirGetSwitch(r)->arms = pawMir_switch_list_new(L->C);
    MirGetSwitch(r)->discr = discr;
    terminate_bb(L, r);
    return r;
}

static struct MirSwitchArm *add_switch_arm(struct LowerHir *L, struct MirTerminator *switch_, Value value, MirBlock bid)
{
    struct MirSwitchArmList *arms = MirGetSwitch(switch_)->arms;
    K_LIST_PUSH(L->C, arms, ((struct MirSwitchArm){
                    .value = value,
                    .bid = bid,
                }));
    return &K_LIST_LAST(arms);
}

static MirRegister register_for_node(struct LowerHir *L, HirId hid)
{
    return new_register(L, pawIr_get_type(L->C, hid));
}

static struct Mir *enter_function(struct LowerHir *L, struct FunctionState *fs, struct BlockState *bs,
        struct IrType *type, struct Mir *mir, DeclId did)
{
    *fs = (struct FunctionState){
        .registers = mir->registers,
        .up = pawMir_upvalue_list_new(L->C),
        .reg_level = mir->registers->count,
        .level = L->stack->count,
        .outer = L->fs,
        .L = L,
    };
    L->fs = fs;

    struct Mir *outer = L->mir;
    L->mir = mir;

    // create the first basic block
    const MirBlock bb = next_bb(L);
    enter_block(L, bs, PAW_FALSE);

    // register 0 is used for the return value
    alloc_local(L, SCAN_STRING(L->C, "(result)"), IR_FPTR(type)->result, did);
    return outer;
}

static void ensure_final_terminator(struct LowerHir *L)
{
    if (current_terminator(L) == NULL) terminate_return(L, NULL);
}

static void leave_function(struct LowerHir *L, struct Mir *outer)
{
    ensure_final_terminator(L);
    L->stack->count = L->fs->level;
    L->fs = L->fs->outer;
    L->mir = outer;
}

static MirRegister last_register(struct LowerHir *L)
{
    return (MirRegister){L->fs->registers->count - 1};
}

static MirRegister lower_operand_(struct HirVisitor *V, struct HirExpr *expr, paw_Bool move_local);

static MirRegister lower_operand(struct HirVisitor *V, struct HirExpr *expr)
{
    return lower_operand_(V, expr, PAW_FALSE);
}

static MirRegister operand_to_reg(struct HirVisitor *V, struct HirExpr *expr)
{
    return lower_operand_(V, expr, PAW_TRUE);
}

static struct MirInstruction *into_fresh_reg(struct LowerHir *L, MirRegister source)
{
    struct MirRegisterData data = K_LIST_GET(L->mir->registers, source.value);
    const MirRegister target = new_register(L, data.type);
    return move_to(L, source, target);
}

static void lower_operand_list_into(struct HirVisitor *V, struct HirExprList *exprs, struct MirRegisterList *result)
{
    struct LowerHir *L = V->ud;
    for (int i = 0; i < exprs->count; ++i) {
        const MirRegister r = operand_to_reg(V, K_LIST_GET(exprs, i));
        K_LIST_PUSH(L->C, result, r);
    }
}

static struct MirRegisterList *lower_operand_list(struct HirVisitor *V, struct HirExprList *exprs)
{
    struct LowerHir *L = V->ud;
    struct MirRegisterList *result = pawMir_register_list_new(L->C);
    lower_operand_list_into(V, exprs, result);
    return result;
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
    MirGetAggregate(r)->output = output;
    MirGetAggregate(r)->nfields = e->tuple.elems->count;

    for (int i = 0; i < e->tuple.elems->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->tuple.elems, i);
        const MirRegister mir_field = lower_operand(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->object = MirGetAggregate(r)->output;
        MirGetSetField(setter)->index = i;
        MirGetSetField(setter)->value = mir_field;
        MirGetSetField(setter)->is_init = PAW_TRUE;
    }

    return output;
}

static MirRegister lower_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = output;
    MirGetAggregate(r)->nfields = e->comp.items->count;

    for (int i = 0; i < e->comp.items->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->comp.items, i);
        const MirRegister mir_field = lower_operand(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->object = MirGetAggregate(r)->output;
        MirGetSetField(setter)->index = HirGetFieldExpr(hir_field)->fid;
        MirGetSetField(setter)->value = mir_field;
        MirGetSetField(setter)->is_init = PAW_TRUE;
    }

    return output;
}

static MirRegister lower_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirContainer);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetContainer(r)->output = output;
    MirGetContainer(r)->nelems = e->cont.items->count;

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
        MirGetSetElement(setter)->key = mir_key;
        MirGetSetElement(setter)->value = mir_value;
        MirGetSetElement(setter)->object = MirGetContainer(r)->output;
        MirGetSetElement(setter)->is_init = PAW_TRUE;
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

static MirRegister lower_logical(struct HirVisitor *V, struct HirExpr *lhs, struct HirExpr *rhs, HirId hid, paw_Bool is_and)
{
    struct LowerHir *L = V->ud;
    const MirBlock before = current_bb(L);
    const MirRegister first = lower_operand(V, lhs);
    struct MirTerminator *r = terminate_branch(L, first);
    if (is_and) {
        MirBlock rhs_bb = next_bb(L);
        MirGetBranch(r)->then_arm = rhs_bb;
        add_predecessor(L, rhs_bb, before);
        // TODO: first/second
        const MirRegister second = lower_operand(V, rhs);

        MirBlock after_bb = advance_bb(L);
        MirGetBranch(r)->else_arm = after_bb;
        add_predecessor(L, after_bb, before);
    } else {
        MirBlock rhs_bb = next_bb(L);
        MirGetBranch(r)->then_arm = rhs_bb;
        add_predecessor(L, rhs_bb, before);
        struct MirTerminator *finish = terminate_goto(L, MIR_INVALID_BB);

        MirBlock extra_bb = next_bb(L);
        MirGetBranch(r)->else_arm = extra_bb;
        add_predecessor(L, extra_bb, before);
        const MirRegister second = lower_operand(V, rhs);

        MirBlock after_bb = advance_bb(L);
        MirGetGoto(finish)->target = after_bb;
        add_predecessor(L, after_bb, rhs_bb);
    }
    return first; // TODO
}

static MirRegister lower_logical_expr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    return lower_logical(V, e->lhs, e->rhs, e->hid, e->is_and);
}

static MirRegister lower_unit_struct(struct HirVisitor *V, struct HirPathExpr *e, struct HirAdtDecl *d)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    const MirRegister target = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = target;
    MirGetAggregate(r)->nfields = 0;
    return target;
}

static MirRegister lower_unit_variant(struct HirVisitor *V, struct HirPathExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = output;
    MirGetAggregate(r)->nfields = 1;

    struct MirInstruction *discr = add_constant(L, I2V(d->index), PAW_TINT);
    MirGetConstant(discr)->output = MirGetConstant(discr)->output;
    struct MirInstruction *set_discr = new_instruction(L, kMirSetField);
    MirGetSetField(set_discr)->value = MirGetConstant(discr)->output;
    MirGetSetField(set_discr)->object = output;
    MirGetSetField(set_discr)->index = 0;
    MirGetSetField(set_discr)->is_init = PAW_TRUE;
    return output;
}

static MirRegister lower_path_expr(struct HirVisitor *V, struct HirPathExpr *e, paw_Bool use_fresh_reg)
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
        } else if (use_fresh_reg) {
            const MirRegister target = register_for_node(L, e->hid);
            struct MirInstruction *r = new_instruction(L, kMirMove);
            MirGetMove(r)->output = target;
            MirGetMove(r)->target = ng.r;
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
    MirGetGlobal(r)->type = GET_NODE_TYPE(L->C, decl);
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

// Transformation:
//     opt?  =>  opt = if opt.0 != 0 {return opt;} else {opt.1}
static MirRegister lower_chain_expr(struct HirVisitor *V, struct HirChainExpr *e)
{
    // TODO: this won't work...
    paw_assert(0);
//    struct LowerHir *L = V->ud;
//    const MirRegister input = lower_operand(V, e->target);
//    struct MirInstruction *explode = new_instruction(L, kMirExplode);
//    struct MirRegisterList *exploded = pawMir_register_list_new(L->C);
//    MirGetExplode(explode)->outputs = exploded;
//    MirGetExplode(explode)->input = input;
//
//    const MirRegister discr = new_literal_reg(L, PAW_TINT);
//    const MirRegister value = register_for_node(L, e->hid);
//    K_LIST_PUSH(L->C, exploded, discr);
//    K_LIST_PUSH(L->C, exploded, value);
//
//    struct MirTerminator *switch_ = terminate_switch(L, discr);
//
//    MirBlock some_bb = next_bb(L);
//    struct MirSwitchArm *some_arm = add_switch_arm(L, switch_, I2V(0), some_bb);
//    struct MirInstruction *finalize = into_fresh_reg(L, value);
//    struct MirTerminator *goto_ = terminate_goto(L, MIR_INVALID_BB);
//
//    MirBlock none_bb = next_bb(L);
//    MirGetSwitch(switch_)->otherwise = none_bb;
//    MirGetSwitch(switch_)->has_otherwise = PAW_TRUE;
//    struct MirTerminator *return_ = terminate_return(L);
//    MirGetReturn(return_)->value = object;
//
//    MirBlock after_bb = next_bb(L);
//    MirGetGoto(goto_)->target = after_bb;
//
//    L->target = output;
//    return PAW_FALSE;

    PAW_UNREACHABLE();
}

static MirRegister lower_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct LowerHir *L = V->ud;
    const MirRegister val = lower_operand(V, e->target);
    struct MirInstruction *r = new_instruction(L, kMirUnaryOp);
    const MirRegister output = register_for_node(L, e->hid);
    MirGetUnaryOp(r)->type = GET_NODE_TYPE(L->C, e->target);
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
    MirGetBinaryOp(r)->type = GET_NODE_TYPE(L->C, e->lhs);
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
    MirGetBinaryOp(r)->type = GET_NODE_TYPE(L->C, e->lhs);
    MirGetBinaryOp(r)->output = output;
    MirGetBinaryOp(r)->op = e->op;
    MirGetBinaryOp(r)->lhs = lhs;
    MirGetBinaryOp(r)->rhs = rhs;
    return output;
}

static MirRegister lower_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct LowerHir *L = V->ud;
    struct IrType *type = pawIr_get_type(L->C, e->hid);

    String *name = SCAN_STRING(L->C, "(closure)");
    struct Mir *result = pawMir_new(L->C, name, type, NULL,
            FUNC_CLOSURE, PAW_FALSE, PAW_FALSE, PAW_FALSE);

    struct BlockState bs;
    struct FunctionState fs;
    struct Mir *outer_mir = enter_function(L, &fs, &bs, type, result, NO_DECL);

    pawHir_visit_decl_list(&L->V, e->params);
    if (e->has_body) {
        pawHir_visit_block(&L->V, e->body);
    } else {
        // evaluate and return the expression
        const MirRegister result = lower_operand(&L->V, e->expr);
        terminate_return(L, &result);
    }
    result->upvalues = L->fs->up;
    leave_function(L, outer_mir);

    const MirRegister output = new_register(L, type);
    struct MirInstruction *r = new_instruction(L, kMirClosure);
    MirGetClosure(r)->child_id = L->mir->children->count;
    MirGetClosure(r)->output = output;
    K_LIST_PUSH(L->C, L->mir->children, result);
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
    MirGetCast(r)->type = e->to;
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
    struct MirInstruction *set_discr = new_instruction(L, kMirSetField);
    MirGetSetField(set_discr)->value = MirGetConstant(discr)->output;
    MirGetSetField(set_discr)->object = object;
    MirGetSetField(set_discr)->index = 0;
    MirGetSetField(set_discr)->is_init = PAW_TRUE;

    for (int i = 0; i < e->args->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->args, i);
        const MirRegister mir_field = lower_operand(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->object = object;
        MirGetSetField(setter)->index = i + 1;
        MirGetSetField(setter)->value = mir_field;
        MirGetSetField(setter)->is_init = PAW_TRUE;
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
        const MirRegister self = operand_to_reg(V, select->target);
        K_LIST_PUSH(L->C, args_out, self);
    } else {
        result = operand_to_reg(V, callee);
    }
    lower_operand_list_into(V, args_in, args_out);
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

static void add_definition(struct LowerHir *L, struct MirBlockList *bucket, MirBlock bb)
{
    for (int i = 0; i < bucket->count; ++i) {
        const MirBlock bb = K_LIST_GET(bucket, i);
        if (MIR_BB_EQUALS(bb, current_bb(L))) return;
    }
    K_LIST_PUSH(L->C, bucket, bb);
}

// Indicate that the given variable was written to in this basic block
static void indicate_def(struct LowerHir *L, int var_id, MirRegister r)
{
    const Value *pval = pawH_get(L->defs, I2V(var_id));
    paw_assert(pval != NULL);
    add_definition(L, pval->p, current_bb(L));
}

//// Lower the right-hand-side of an assignment expression
//static void lower_assignment_rhs(struct HirVisitor *V, struct LocalVar var, struct HirExpr *rhs)
//{
//    struct LowerHir *L = V->ud;
//    lower_operand(V, rhs); // final result register is not used
//    struct MirInstructionList *instrs = current_bb_data(L)->instructions;
//    struct MirInstruction *last = K_LIST_LAST(instrs);
//    switch (MIR_KINDOF(last)) {
//        case kMirMove:
//            MirGetMove(last)->output = var.reg;
//            break;
//        case kMirUpvalue:
//            MirGetUpvalue(last)->output = var.reg;
//            break;
//        case kMirGlobal:
//            MirGetGlobal(last)->output = var.reg;
//            break;
//        case kMirAllocLocal:
//            MirGetAllocLocal(last)->output = var.reg;
//            break;
//        case kMirConstant:
//            MirGetConstant(last)->output = var.reg;
//            break;
//        case kMirAggregate:
//            MirGetAggregate(last)->output = var.reg;
//            break;
//        case kMirContainer:
//            MirGetContainer(last)->output = var.reg;
//            break;
//        case kMirCall:
//            MirGetCall(last)->output = var.reg;
//            break;
//        case kMirCast:
//            MirGetCast(last)->output = var.reg;
//            break;
//        case kMirClosure:
//            MirGetClosure(last)->output = var.reg;
//            break;
//        case kMirGetElement:
//            MirGetGetElement(last)->output = var.reg;
//            break;
//        case kMirGetRange:
//            MirGetGetRange(last)->output = var.reg;
//            break;
//        case kMirGetField:
//            MirGetGetField(last)->output = var.reg;
//            break;
//        case kMirUnaryOp:
//            MirGetUnaryOp(last)->output = var.reg;
//            break;
//        default:
//            MirGetBinaryOp(last)->output = var.reg;
//    }
//    --L->fs->registers->count;
//}

static MirRegister lower_assign_expr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    MirRegister output;
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
            output = register_for_node(L, e->hid);
            MirGetSetUpvalue(r)->output = output;
            MirGetSetUpvalue(r)->index = ng.index;
            MirGetSetUpvalue(r)->value = rhs;
        } else {
            struct LocalVar var = K_LIST_GET(L->stack, fs->level + ng.index);
            const MirRegister value = operand_to_reg(V, e->rhs);
            struct MirInstruction *r = new_instruction(L, kMirSetLocal);
            output = register_for_node(L, e->hid);
            MirGetSetLocal(r)->output = output;
            MirGetSetLocal(r)->target = ng.r;
            MirGetSetLocal(r)->value = value;
            indicate_def(L, var.vid, ng.r);
        }
    } else if (HirIsSelector(e->lhs)) {
        struct HirSelector *x = HirGetSelector(e->lhs);
        const MirRegister target = lower_operand(V, x->target);
        const MirRegister rhs = lower_operand(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirSetField);
        output = register_for_node(L, e->hid);
        MirGetSetField(r)->output = output;
        MirGetSetField(r)->object = target;
        MirGetSetField(r)->index = x->index;
        MirGetSetField(r)->value = rhs;
    } else if (!HirGetIndex(e->lhs)->is_slice) {
        struct HirIndex *x = HirGetIndex(e->lhs);
        const MirRegister target = lower_operand(V, x->target);
        const MirRegister key = lower_operand(V, x->first);
        const MirRegister rhs = lower_operand(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirSetElement);
        output = register_for_node(L, e->hid);
        MirGetSetElement(r)->output = output;
        MirGetSetElement(r)->object = target;
        MirGetSetElement(r)->key = key;
        MirGetSetElement(r)->value = rhs;
    } else {
        struct HirIndex *x = HirGetIndex(e->lhs);
        const MirRegister target = lower_operand(V, x->target);
        const MirRegister lower = operand_to_reg(V, x->first);
        const MirRegister upper = operand_to_reg(V, x->second);
        const MirRegister rhs = operand_to_reg(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirSetRange);
        output = register_for_node(L, e->hid);
        MirGetSetRange(r)->output = output;
        MirGetSetRange(r)->object = target;
        MirGetSetRange(r)->lower = lower;
        MirGetSetRange(r)->upper = upper;
        MirGetSetRange(r)->value = rhs;
    }
    return output;
}


static MirRegister first_slice_index(struct HirVisitor *V, struct HirExpr *e)
{
    if (e != NULL) return operand_to_reg(V, e);
    // default to integer 0
    struct MirInstruction *r = add_constant(V->ud, I2V(0), PAW_TINT);
    return MirGetConstant(r)->output;
}

static MirRegister second_slice_index(struct HirVisitor *V, struct HirExpr *e, MirRegister object)
{
    if (e != NULL) return operand_to_reg(V, e);
    // default to the number of elements
    struct LowerHir *L = V->ud;
    struct MirInstruction *m = into_fresh_reg(L, object);
    struct MirInstruction *r = new_instruction(L, kMirUnaryOp);
    MirGetUnaryOp(r)->output = new_literal_reg(L, PAW_TINT);
    MirGetUnaryOp(r)->val = MirGetMove(m)->output;
    return MirGetUnaryOp(r)->output;
}

static MirRegister lower_index_expr(struct HirVisitor *V, struct HirIndex *e)
{
    struct LowerHir *L = V->ud;
    const MirRegister object = lower_operand(V, e->target);
    if (!e->is_slice) {
        const MirRegister first = lower_operand(V, e->first);
        const MirRegister output = register_for_node(L, e->hid);
        struct MirInstruction *r = new_instruction(L, kMirGetElement);
        MirGetGetElement(r)->output = output;
        MirGetGetElement(r)->object = object;
        MirGetGetElement(r)->key = first;
        return output;
    } else { // stack: .. first second ..
        const MirRegister first = first_slice_index(V, e->first);
        const MirRegister second = second_slice_index(V, e->second, object);
        const MirRegister output = register_for_node(L, e->hid);
        struct MirInstruction *r = new_instruction(L, kMirGetRange);
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
    struct MirInstruction *r = new_instruction(L, kMirGetField);
    MirGetGetField(r)->output = output;
    MirGetGetField(r)->object = object;
    MirGetGetField(r)->index = e->index;
    return output;
}

static MirRegister lower_operand_(struct HirVisitor *V, struct HirExpr *expr, paw_Bool move_local)
{
    switch (HIR_KINDOF(expr)) {
        case kHirLiteralExpr:
            return lower_literal_expr(V, HirGetLiteralExpr(expr));
        case kHirLogicalExpr:
            return lower_logical_expr(V, HirGetLogicalExpr(expr));
        case kHirPathExpr:
            return lower_path_expr(V, HirGetPathExpr(expr), move_local);
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
    }
}


static void post_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    lower_operand(V, s->expr);
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

//// TODO: This function, as well as close_until_loop may be needed after all. Depends on if registers
///        containing upvalues can be kept in order on the stack or not. If not, then each upvalue will
///        need to be closed separately.
////       for continue, just use the OP_CLOSE that upvalues inside the loop use
//static void close_vars(struct FunctionState *fs, const struct BlockState *bs)
//{
//    for (int i = fs->nlocals - 1; i >= bs->nvars; --i) {
//        const struct LocalVar *var = get_local_slot(fs, i);
//        if (!var->is_captured) continue;
//        struct MirInstruction *close = new_instruction(fs->L, kMirClose);
//        MirGetClose(close)->target = var->reg;
//    }
//}
//
//static void close_until_loop(struct FunctionState *fs)
//{
//    struct BlockState *bs = fs->bs;
//    while (bs->outer) {
//        // Emit close/pop instructions, but don't end any lifetimes. Code
//        // paths that doesn't hit this label may still need those locals.
//        struct BlockState *outer = bs->outer;
//        if (outer->is_loop) {
//            close_vars(fs, bs);
//            return;
//        }
//        bs = outer;
//    }
//    pawE_error(ENV(fs->L), PAW_ESYNTAX, -1, "label outside loop");
//}

static paw_Bool visit_jump_stmt(struct HirVisitor *V, struct HirJumpStmt *s)
{
    struct LowerHir *L = V->ud;
    if (!is_within_loop(L)) {
        pawE_error(ENV(L->C), PAW_ESYNTAX, s->line, "%s outside loop",
                s->jump_kind == JUMP_BREAK ? "break" : "continue");
    }
    add_label(L, s->jump_kind);
    return PAW_FALSE;
}

static paw_Bool visit_return_stmt(struct HirVisitor *V, struct HirReturnStmt *s)
{
    struct LowerHir *L = V->ud;
    if (s->expr != NULL) {
        const MirRegister value = lower_operand(V, s->expr);
        terminate_return(L, &value);
    } else {
        // creates a dummy register for a value of type "()"
        terminate_return(L, NULL);
    }
    next_bb(L);
    return PAW_FALSE;
}

static MirBlock patch_to_here(struct LowerHir *L, struct MirBlockList *sources)
{
    const MirBlock here = advance_bb(L);
    for (int i = 0; i < sources->count; ++i) {
        const MirBlock bb = K_LIST_GET(sources, i);
        const struct MirBlockData *data = get_bb(L, bb);
        MirGetGoto(data->terminator)->target = here;
        add_predecessor(L, here, bb);
    }
    return here;
}

static void save_patch_source(struct LowerHir *L, struct MirBlockList *sources)
{
    if (current_terminator(L) == NULL) {
        MirBlock bb = current_bb(L);
        K_LIST_PUSH(L->C, sources, bb);
        terminate_goto(L, MIR_INVALID_BB); // placeholder
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

static MirBlock lower_match_body(struct HirVisitor *V, struct MatchBody body);
static void visit_decision(struct HirVisitor *V, struct Decision *d);

static void visit_success(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    lower_match_body(V, d->success.body);
}

static void visit_guard(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;

    // steal bindings from the body of the guard, since they may be referenced in
    // the conditional expression
    struct BindingList *bindings = d->guard.body.bindings;
    declare_match_bindings(L, bindings);
    bindings->count = 0;


    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    const MirRegister cond = lower_operand(V, d->guard.cond);
    const MirBlock before = current_bb(L);
    struct MirTerminator *branch = terminate_branch(L, cond);

    MirBlock then_bb = next_bb(L);
    MirGetBranch(branch)->then_arm = then_bb;
    add_predecessor(L, then_bb, before);
    lower_match_body(V, d->guard.body);
    save_patch_source(L, patch);

    MirBlock else_bb = next_bb(L);
    MirGetBranch(branch)->else_arm = else_bb;
    add_predecessor(L, else_bb, before);
    visit_decision(V, d->guard.rest);

    patch_to_here(L, patch);
}

static MirBlock lower_match_body(struct HirVisitor *V, struct MatchBody body)
{
    struct LowerHir *L = V->ud;
    const MirBlock bid = current_bb(L);
    declare_match_bindings(L, body.bindings);
    pawHir_visit_block(V, body.block);
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

static MirRegister emit_multiway_test(struct LowerHir *L, struct Decision *d)
{
    const MirRegister output = new_register(L, d->multi.test.type);
    struct MirInstruction *move = new_instruction(L, kMirMove);
    add_local_literal(L, "(match discriminator)", output); // keep alive
    MirGetMove(move)->target = get_test_reg(L, d->multi.test);
    MirGetMove(move)->output = output;
    return output;
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

static void visit_sparse_cases(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    const MirRegister test = emit_multiway_test(L, d);
    struct MirTerminator *switch_ = terminate_switch(L, test);

    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    for (int i = 0; i < d->multi.cases->count; ++i) {
        struct MatchCase mc = K_LIST_GET(d->multi.cases, i);
        MirBlock block = next_bb(L);

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        struct MirSwitchArm *arm = add_switch_arm(L, switch_, mc.cons.value, block);
        visit_decision(V, mc.dec);
        save_patch_source(L, patch);

        leave_block(L);
    }

    // this implementation requires an "otherwise" case (binding or wildcard) to ensure
    // exhaustivness
    paw_assert(d->multi.rest != NULL);
    MirBlock otherwise_bb = next_bb(L);
    MirGetSwitch(switch_)->otherwise = otherwise_bb;
    MirGetSwitch(switch_)->has_otherwise = PAW_TRUE;
    visit_decision(V, d->multi.rest);
    K_LIST_PUSH(L->C, patch, current_bb(L));

    patch_to_here(L, patch);
}

static void visit_variant_cases(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    const MirRegister variant = get_test_reg(L, d->multi.test);
    const MirRegister test = new_literal_reg(L, PAW_TINT);
    add_local_literal(L, "(match discriminant)", test); // keep alive
    emit_get_field(L, variant, 0, test);
    struct MirTerminator *switch_ = terminate_switch(L, test);

    struct CaseList *cases = d->multi.cases;
    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    for (int i = 0; i < cases->count; ++i) {
        struct MatchCase mc = K_LIST_GET(cases, i);
        MirBlock block = next_bb(L);

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        struct MirSwitchArm *arm = add_switch_arm(L, switch_, I2V(i), block);
        arm->value.i = mc.cons.variant.index;
        allocate_match_vars(L, variant, mc, PAW_TRUE);
        visit_decision(V, mc.dec);
        save_patch_source(L, patch);

        leave_block(L);
    }
    paw_assert(d->multi.rest == NULL);

    patch_to_here(L, patch);
}

static void visit_tuple_case(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    const MirRegister discr = get_test_reg(L, d->multi.test);

    paw_assert(d->multi.rest == NULL);
    paw_assert(d->multi.cases->count == 1);
    struct MatchCase mc = K_LIST_FIRST(d->multi.cases);

    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);
    allocate_match_vars(L, discr, mc, PAW_FALSE);
    visit_decision(V, mc.dec);
    leave_block(L);
    advance_bb(L);
}

static void visit_struct_case(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    const MirRegister discr = get_test_reg(L, d->multi.test);

    paw_assert(d->multi.rest == NULL);
    paw_assert(d->multi.cases->count == 1);
    struct MatchCase mc = K_LIST_FIRST(d->multi.cases);
    MirBlock case_bb = advance_bb(L);

    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);
    allocate_match_vars(L, discr, mc, PAW_FALSE);
    visit_decision(V, mc.dec);
    leave_block(L);
    advance_bb(L);
}

static void visit_multiway(struct HirVisitor *V, struct Decision *d)
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
            visit_sparse_cases(V, d);
            break;
        case CONS_VARIANT:
            visit_variant_cases(V, d);
            break;
        case CONS_TUPLE:
            visit_tuple_case(V, d);
            break;
        case CONS_STRUCT:
            visit_struct_case(V, d);
            break;
        case CONS_REST:
            PAW_UNREACHABLE();
    }
}

static void visit_decision(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;

    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);

    switch (d->kind) {
        case DECISION_FAILURE:
            paw_assert(0);
            return;
        case DECISION_SUCCESS:
            visit_success(V, d);
            break;
        case DECISION_GUARD:
            visit_guard(V, d);
            break;
        case DECISION_MULTIWAY:
            visit_multiway(V, d);
            break;
    }

    leave_block(L);
}

static paw_Bool visit_match_stmt(struct HirVisitor *V, struct HirMatchStmt *s)
{
    struct LowerHir *L = V->ud;
    Map *var_mapping = pawP_push_map(L->C);

    struct BlockState bs;
    struct MatchState ms;
    enter_block(L, &bs, PAW_FALSE);
    enter_match(L, &ms, var_mapping);

    struct Decision *d = pawP_check_exhaustiveness(L->C, s, ms.vars);
    paw_assert(ms.vars->count > 0);
    const MirRegister discr = lower_operand(V, s->target);
    map_var_to_reg(L, K_LIST_FIRST(ms.vars), discr);
    add_local_literal(L, "(match target)", discr);
    K_LIST_PUSH(L->C, ms.regs, discr);

    visit_decision(V, d);

    leave_match(L);
    leave_block(L);

    pawP_pop_object(L->C, var_mapping);
    return PAW_FALSE;
}

static paw_Bool visit_block(struct HirVisitor *V, struct HirBlock *s)
{
    struct BlockState bs;
    struct LowerHir *L = V->ud;
    enter_block(L, &bs, PAW_FALSE);

    pawHir_visit_stmt_list(V, s->stmts);

    leave_block(L);
    return PAW_FALSE;
}

static paw_Bool visit_if_stmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    struct LowerHir *L = V->ud;
    const MirRegister cond = lower_operand(V, s->cond);
    const MirBlock cond_bb = current_bb(L);
    struct MirTerminator *r = terminate_branch(L, cond);

    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    const MirBlock then_bb = next_bb(L);
    add_predecessor(L, then_bb, cond_bb);
    MirGetBranch(r)->then_arm = then_bb;
    pawHir_visit_stmt(V, s->then_arm);
    save_patch_source(L, patch);

    const MirBlock else_bb = next_bb(L);
    add_predecessor(L, else_bb, cond_bb);
    MirGetBranch(r)->else_arm = else_bb;
    if (s->else_arm != NULL) pawHir_visit_stmt(V, s->else_arm);
    save_patch_source(L, patch);

    patch_to_here(L, patch);
    return PAW_FALSE;
}

static void lower_fornum(struct HirVisitor *V, struct HirForNum fornum, struct HirVarDecl *control, struct HirBlock *body)
{
    struct LowerHir *L = V->ud;
    struct BlockState outer;
    enter_block(L, &outer, PAW_TRUE);

    const MirRegister step = operand_to_reg(V, fornum.step);
    const MirRegister end = operand_to_reg(V, fornum.end);
    const MirRegister iter = operand_to_reg(V, fornum.begin);
    const MirRegister var = new_literal_reg(L, PAW_TINT);

    struct LocalVar *local_step = add_local_literal(L, "(for step)", step);
    struct LocalVar *local_end = add_local_literal(L, "(for end)", end);
    struct LocalVar *local_iter = add_local_literal(L, "(for iter)", end);

    struct BlockState inner;
    enter_block(L, &inner, PAW_FALSE);
    const MirBlock prep_bb = advance_bb(L);
    struct LocalVar *local_var = add_local(L, control->name, var, control->did);
    struct MirTerminator *skip = terminate_for_loop(L, MIR_FOR_PREP, var, iter, end, step);
    MirBlock top_bb = next_bb(L);
    MirGetForLoop(skip)->then_arm = top_bb;
    add_predecessor(L, top_bb, prep_bb);

    pawHir_visit_block(V, body); // body of loop
    const MirBlock bottom_bb = advance_bb(L);
    adjust_from(L, JUMP_CONTINUE);
    leave_block(L);

    struct MirTerminator *loop = terminate_for_loop(L, MIR_FOR_LOOP, var, iter, end, step);
    MirGetForLoop(loop)->then_arm = top_bb;
    add_predecessor(L, top_bb, bottom_bb);

    MirBlock after_bb = advance_bb(L);
    MirGetForLoop(loop)->else_arm = after_bb;
    MirGetForLoop(skip)->else_arm = after_bb;
    add_predecessor(L, after_bb, bottom_bb);
    add_predecessor(L, after_bb, prep_bb);

    leave_block(L);
}

static void lower_forin(struct HirVisitor *V, struct HirForIn forin, struct HirVarDecl *control, struct HirBlock *body)
{
//    pawHir_visit_block(V, body);
}

static paw_Bool visit_for_stmt(struct HirVisitor *V, struct HirForStmt *s)
{
    if (s->is_fornum) {
        lower_fornum(V, s->fornum, HirGetVarDecl(s->control), s->block);
    } else {
        lower_forin(V, s->forin, HirGetVarDecl(s->control), s->block);
    }
    return PAW_FALSE;
}

static paw_Bool visit_dowhile_stmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    struct LowerHir *L = V->ud;

    struct BlockState bs;
    enter_block(L, &bs, PAW_TRUE);

    const MirBlock top_bb = advance_bb(L);
    pawHir_visit_block(V, s->block);

    const MirBlock before = advance_bb(L);
    adjust_from(L, JUMP_CONTINUE);

    const MirRegister cond = lower_operand(V, s->cond);
    const MirBlock bottom_bb = current_bb(L);
    struct MirTerminator *branch = terminate_branch(L, cond);
    MirGetBranch(branch)->then_arm = top_bb;
    add_predecessor(L, top_bb, bottom_bb);

    // TODO: see what happens in visit_while_stmt
    const MirBlock after_bb = next_bb(L);
    MirGetBranch(branch)->else_arm = after_bb;
    add_predecessor(L, after_bb, bottom_bb);

    leave_block(L);
    return PAW_FALSE;
}

static paw_Bool visit_while_stmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    if (s->is_dowhile) return visit_dowhile_stmt(V, s);
    struct LowerHir *L = V->ud;

    struct BlockState bs;
    enter_block(L, &bs, PAW_TRUE);

    const MirBlock before = advance_bb(L);
    const MirRegister cond = lower_operand(V, s->cond);
    const MirBlock loop_bb = current_bb(L);
    struct MirTerminator *branch = terminate_branch(L, cond);

    const MirBlock body_bb = next_bb(L);
    MirGetBranch(branch)->then_arm = body_bb;
    add_predecessor(L, body_bb, loop_bb);
    pawHir_visit_block(V, s->block);

    if (current_terminator(L) == NULL) {
        add_predecessor(L, loop_bb, body_bb);
        terminate_goto(L, loop_bb);
    }
    adjust_to(L, JUMP_CONTINUE, loop_bb);
    const MirBlock after_bb = advance_bb(L);
    MirGetBranch(branch)->else_arm = after_bb;
    add_predecessor(L, after_bb, loop_bb);

    leave_block(L);
    return PAW_FALSE;
}

static void lower_hir_body(struct LowerHir *L, struct HirFuncDecl *func, struct Mir *result)
{
    String *name = L->mir->name;
    struct IrType *type = L->mir->type;

    struct BlockState bs;
    struct FunctionState fs;
    enter_function(L, &fs, &bs, type, result, func->did);

    pawHir_visit_decl_list(&L->V, func->params);
    pawHir_visit_block(&L->V, func->body);

    leave_function(L, NULL);
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
        .mir = result,
        .locals = pawMir_register_list_new(C),
        .labels = label_list_new(C),
        .stack = var_stack_new(C),
        .P = ENV(C),
        .C = C,
    };
    L.match_vars = pawP_push_map(C);
    L.defs = pawP_push_map(C);
    L.vars = pawP_push_map(C);
    pawHir_visitor_init(&L.V, C, &L);

    L.V.VisitFieldDecl = visit_field_decl;
    L.V.VisitVarDecl = visit_var_decl;

    L.V.PostVisitExprStmt = post_expr_stmt;
    L.V.VisitBlock = visit_block;
    L.V.VisitReturnStmt = visit_return_stmt;
    L.V.VisitJumpStmt = visit_jump_stmt;
    L.V.VisitIfStmt = visit_if_stmt;
    L.V.VisitForStmt = visit_for_stmt;
    L.V.VisitWhileStmt = visit_while_stmt;
    L.V.VisitMatchStmt = visit_match_stmt;

    lower_hir_body(&L, func, result);
    pawSsa_convert(C, result, L.vars, L.defs, L.locals);

    pawP_pop_object(C, L.vars);
    pawP_pop_object(C, L.defs);
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

