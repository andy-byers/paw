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
//       their enclosing functions. Closures also need to store the ".child_id" so
//       they can be placed in their enclosing Proto (so they can be found at
//       runtime).

#include "api.h"
#include "compile.h"
#include "hir.h"
#include "ir_type.h"
#include "lex.h"
#include "match.h"
#include "mir.h"
#include "ssa.h"
#include "unify.h"

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
    struct VarMap *var_mapping;
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

struct LowerHir {
    struct HirVisitor V;
    struct Compiler *C;
    paw_Env *P;

    struct MatchState *ms;
    struct FunctionState *fs;
    struct LabelList *labels;
    struct VarStack *stack;
};

static paw_Uint var_hash(struct Compiler *C, struct MatchVar v)
{
    PAW_UNUSED(C);
    return v.id;
}

static paw_Bool var_equals(struct Compiler *C, struct MatchVar a, struct MatchVar b)
{
    PAW_UNUSED(C);
    return a.id == b.id;
}

DEFINE_LIST(struct Compiler, var_stack_, VarStack, struct LocalVar)
DEFINE_LIST(struct Compiler, label_list_, LabelList, struct Label)
DEFINE_MAP(struct Compiler, VarMap, pawP_alloc, var_hash, var_equals, struct MatchVar, MirRegister)

static void enter_match(struct LowerHir *L, struct MatchState *ms, VarMap *var_mapping)
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
    DeclId const did = L->C->builtins[code].did;
    return GET_NODE_TYPE(L->C, pawHir_get_decl(L->C, did));
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

static struct MirInstruction *add_instruction_(struct LowerHir *L, struct MirInstruction *instr)
{
    struct MirBlockData *block = current_bb_data(L);
    K_LIST_PUSH(L->C, block->instructions, instr);
    return instr;
}

static struct MirInstruction *add_instruction(struct FunctionState *fs, struct MirInstruction *instr)
{
    struct MirBlockData *block = current_bb_data(fs->L);
    K_LIST_PUSH(fs->L->C, block->instructions, instr);
    return instr;
}

#define NEW_INSTR(fs, kind, ...) add_instruction(fs, pawMir_new_##kind((fs)->mir, __VA_ARGS__))

static struct MirInstruction *terminate_goto(struct LowerHir *L, int line, MirBlock target)
{
    return NEW_INSTR(L->fs, goto, -1, target);
}

static void set_goto_edge(struct LowerHir *L, int line, MirBlock to)
{
    add_edge(L, current_bb(L), to);
    terminate_goto(L, line, to);
}

static void set_current_bb(struct LowerHir *L, MirBlock b)
{
    L->fs->current = b;
}

static MirBlock new_bb(struct LowerHir *L)
{
    int const id = bb_list(L)->count;
    struct MirBlockData *bb = pawMir_new_block(L->fs->mir);
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
                if (data->is_captured)
                    lowest_upvalue = var.reg;
            }
        }
        if (bs->is_loop)
            break;
        bs = bs->outer;
    }
    if (needs_close) {
        NEW_INSTR(fs, close, -1, lowest_upvalue);
    }
}

static void add_label(struct LowerHir *L, int line, enum JumpKind kind)
{
    // MirClose must be added here if "continue" causes control to leave a scope containing
    // a captured variable, since the MirClose at the end of the loop body will not be reached.
    if (kind == JUMP_CONTINUE)
        close_until_loop(L->fs);
    struct MirBlockData *block = current_bb_data(L);
    terminate_goto(L, line, MIR_INVALID_BB);

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
    return NEW_INSTR(L->fs, move, -1, output, target);
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

static paw_Bool resolve_local(struct LowerHir *L, struct FunctionState *fs, String const *name, struct NonGlobal *pinfo)
{
    // condition is "i > 0" to cause the result register to be ignored
    for (int i = fs->nlocals - 1; i > 0; --i) {
        struct LocalVar *item = get_local_slot(fs, i);
        if (pawS_eq(name, item->name)) {
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
    while (bs->nvars > target)
        bs = bs->outer;
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
        if (is_local != up.is_local)
            continue;
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

static paw_Bool resolve_upvalue(struct LowerHir *L, struct FunctionState *fs, String *name, struct NonGlobal *pinfo)
{
    struct FunctionState *caller = fs->outer;
    if (caller == NULL)
        return PAW_FALSE;
    if (resolve_local(L, caller, name, pinfo)) {
        mark_upvalue(caller, pinfo->index, pinfo->r);
        add_upvalue(L, fs, pinfo, PAW_TRUE);
        return PAW_TRUE;
    }
    if (resolve_upvalue(L, caller, name, pinfo)) {
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
    if (data->is_captured)
        NEW_INSTR(L->fs, close, -1, r);
}

static void close_variables(struct LowerHir *L, int nvars)
{
    for (int i = L->fs->nlocals - 1; i >= nvars; --i) {
        struct LocalVar const local = K_LIST_GET(L->stack, L->fs->level + i);
        maybe_close(L, local.reg);
    }
}

static void leave_block(struct LowerHir *L)
{
    struct BlockState *bs = L->fs->bs;
    int needs_close = bs->has_upvalue && bs->outer != NULL;
    if (bs->is_loop)
        needs_close |= adjust_from(L, JUMP_BREAK);
    close_variables(L, bs->nvars);
    if (bs->outer != NULL)
        adjust_labels(L, bs);

    struct VarStack old = *L->stack;
    int const limit = L->fs->level + bs->nvars;
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

static struct LocalVar *add_local_literal(struct LowerHir *L, char const *name, MirRegister r)
{
    return add_local(L, SCAN_STRING(L->C, name), r, NO_DECL);
}

static struct LocalVar *alloc_local(struct LowerHir *L, String *name, struct IrType *type, DeclId did)
{
    MirRegister const output = new_register(L, type);
    NEW_INSTR(L->fs, alloc_local, -1, name, output);
    return add_local(L, name, output, did);
}

static paw_Bool resolve_nonglobal(struct LowerHir *L, DeclId did, String *name, struct NonGlobal *png)
{
    if (resolve_local(L, L->fs, name, png))
        return PAW_TRUE;
    return resolve_upvalue(L, L->fs, name, png);
}

static MirRegister add_constant(struct LowerHir *L, Value value, enum BuiltinKind b_kind)
{
    paw_assert(b_kind != NBUILTINS);
    struct FunctionState *fs = L->fs;
    MirRegister const target = new_register(L, get_type(L, b_kind));
    NEW_INSTR(fs, constant, -1, b_kind, value, target);
    return target;
}

static MirRegister unit_literal(struct LowerHir *L)
{
    return add_constant(L, I2V(0), BUILTIN_UNIT);
}

struct MirInstruction *terminate_return(struct LowerHir *L, MirRegister const *pvalue)
{
    struct FunctionState *fs = L->fs;
    MirRegister const value = pvalue != NULL ? *pvalue
                                             : add_constant(L, P2V(NULL), BUILTIN_UNIT);
    return NEW_INSTR(fs, return, -1, value);
}

struct MirInstruction *terminate_branch(struct LowerHir *L, MirRegister cond, MirBlock then_arm, MirBlock else_arm)
{
    struct FunctionState *fs = L->fs;
    return NEW_INSTR(fs, branch, -1, cond, then_arm, else_arm);
}

struct MirInstruction *terminate_switch(struct LowerHir *L, MirRegister discr, struct MirSwitchArmList *arms, MirBlock otherwise)
{
    struct FunctionState *fs = L->fs;
    return NEW_INSTR(fs, switch, -1, discr, arms, otherwise);
}

static MirRegister register_for_node(struct LowerHir *L, HirId hid)
{
    return new_register(L, pawIr_get_type(L->C, hid));
}

static void enter_function(struct LowerHir *L, struct FunctionState *fs, struct BlockState *bs, int line, struct Mir *mir)
{
    *fs = (struct FunctionState){
        .captured = mir->captured,
        .registers = mir->registers,
        .up = pawMir_upvalue_list_new(L->C),
        .level = L->stack->count,
        .locals = mir->locals,
        .outer = L->fs,
        .mir = mir,
        .L = L,
    };
    L->fs = fs;

    MirBlock const start = new_bb(L);
    MirBlock const first = new_bb(L);
    set_current_bb(L, start);
    terminate_goto(L, line, first);
    add_edge(L, start, first);

    set_current_bb(L, first);
    enter_block(L, bs, PAW_FALSE);

    alloc_local(L, SCAN_STRING(L->C, "(result)"),
        IR_FPTR(mir->type)->result, NO_DECL);
}

static void leave_function(struct LowerHir *L)
{
    struct MirBlockData *block = current_bb_data(L);
    terminate_return(L, NULL);
    L->stack->count = L->fs->level;
    L->fs = L->fs->outer;
}

static MirRegister last_register(struct LowerHir *L)
{
    return (MirRegister){L->fs->registers->count - 1};
}

static MirRegister lower_operand(struct HirVisitor *V, struct HirExpr *expr);

#define LOWER_BLOCK(L, b) lower_operand(&(L)->V, HIR_CAST_EXPR(b))

static struct MirInstruction *into_fresh_reg(struct LowerHir *L, MirRegister source)
{
    struct MirRegisterData data = K_LIST_GET(L->fs->mir->registers, source.value);
    MirRegister const target = new_register(L, data.type);
    return move_to(L, source, target);
}

static void lower_operand_list(struct HirVisitor *V, struct HirExprList *exprs, struct MirRegisterList *result)
{
    struct HirExpr **pexpr;
    struct LowerHir *L = V->ud;
    K_LIST_FOREACH(exprs, pexpr)
    {
        MirRegister const r = lower_operand(V, *pexpr);
        K_LIST_PUSH(L->C, result, r);
    }
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

    MirRegister r;
    if (d->init != NULL) {
        MirRegister const r = register_for_node(V->ud, d->init->hdr.hid);
        MirRegister const init = lower_operand(V, d->init);
        add_local(L, d->name, r, d->did);
        move_to(L, init, r);
    } else {
        struct LocalVar *var = alloc_local(L, d->name, type, d->did);
        struct MirRegisterData *data = mir_reg_data(L->fs->mir, var->reg);
        data->is_uninit = PAW_TRUE;
        r = var->reg;
    }
    return PAW_FALSE;
}

static MirRegister lower_basic_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    return add_constant(L, e->basic.value, e->basic.code);
}

static MirRegister lower_tuple_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const output = register_for_node(L, e->hid);
    NEW_INSTR(fs, aggregate, e->line, e->tuple.elems->count, output);

    int index;
    struct HirExpr **pexpr;
    K_LIST_ENUMERATE(e->tuple.elems, index, pexpr)
    {
        MirRegister const field = lower_operand(V, *pexpr);
        NEW_INSTR(fs, set_field, -1, index, output, field);
    }
    return output;
}

static MirRegister lower_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const output = register_for_node(L, e->hid);
    NEW_INSTR(fs, aggregate, e->line, e->comp.items->count, output);

    struct HirExpr **pexpr;
    K_LIST_FOREACH(e->comp.items, pexpr)
    {
        int const index = HirGetFieldExpr(*pexpr)->fid;
        MirRegister const field = lower_operand(V, *pexpr);
        NEW_INSTR(fs, set_field, -1, index, output, field);
    }
    return output;
}

static MirRegister lower_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const output = register_for_node(L, e->hid);
    enum BuiltinKind b_kind = kind_of_builtin(L, HIR_CAST_EXPR(e));
    NEW_INSTR(fs, container, -1, b_kind, e->cont.items->count, output);

    int index;
    struct HirExpr **pexpr;
    K_LIST_ENUMERATE(e->cont.items, index, pexpr)
    {
        MirRegister mir_key, mir_value;
        if (HirIsFieldExpr(*pexpr)) {
            struct HirFieldExpr *elem = HirGetFieldExpr(*pexpr);
            mir_key = lower_operand(V, elem->key);
            mir_value = lower_operand(V, elem->value);
        } else {
            mir_key = add_constant(L, I2V(index), BUILTIN_INT);
            mir_value = lower_operand(V, *pexpr);
        }
        NEW_INSTR(fs, set_element, -1, b_kind, output, mir_key, mir_value);
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
    MirBlock const before_bb = current_bb(L);
    MirBlock const test_bb = new_bb(L);
    MirBlock const lhs_bb = new_bb(L);
    MirBlock const rhs_bb = new_bb(L);
    MirBlock const after_bb = new_bb(L);
    add_edge(L, before_bb, test_bb);
    terminate_goto(L, e->line, test_bb);
    set_current_bb(L, test_bb);

    MirRegister const result = new_literal_reg(L, BUILTIN_BOOL);
    MirRegister const first = lower_operand(V, e->lhs);
    add_edge(L, current_bb(L), lhs_bb);
    add_edge(L, current_bb(L), rhs_bb);

    struct MirInstruction *r = e->is_and
                                   ? terminate_branch(L, first, rhs_bb, lhs_bb)
                                   : terminate_branch(L, first, lhs_bb, rhs_bb);

    set_current_bb(L, lhs_bb);
    move_to(L, first, result);
    set_goto_edge(L, e->line, after_bb);

    set_current_bb(L, rhs_bb);
    MirRegister const second = lower_operand(V, e->rhs);
    move_to(L, second, result);
    set_goto_edge(L, e->line, after_bb);

    set_current_bb(L, after_bb);
    return result;
}

static MirRegister lower_unit_struct(struct HirVisitor *V, struct HirPathExpr *e, struct HirAdtDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const output = register_for_node(L, e->hid);
    NEW_INSTR(fs, aggregate, e->line, 0, output);
    return output;
}

static MirRegister lower_unit_variant(struct HirVisitor *V, struct HirPathExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const object = register_for_node(L, e->hid);
    NEW_INSTR(fs, aggregate, e->line, 1, object);

    MirRegister const discr = add_constant(L, I2V(d->index), BUILTIN_INT);
    NEW_INSTR(fs, set_field, e->line, 0, object, discr);
    return object;
}

static MirRegister lower_path_expr(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    DeclId const did = HIR_PATH_RESULT(e->path);
    struct HirDecl *decl = pawHir_get_decl(L->C, did);
    struct NonGlobal ng;
    if (resolve_nonglobal(L, did, decl->hdr.name, &ng)) {
        MirRegister const output = register_for_node(L, e->hid);
        if (ng.is_upvalue) {
            NEW_INSTR(fs, upvalue, e->line, output, ng.index);
        } else {
            move_to(L, ng.r, output);
        }
        return output;
    }
    MirRegister const output = register_for_node(L, e->hid);
    if (HirIsVariantDecl(decl)) {
        return lower_unit_variant(V, e, HirGetVariantDecl(decl));
    } else if (HirIsAdtDecl(decl)) {
        return lower_unit_struct(V, e, HirGetAdtDecl(decl));
    }
    NEW_INSTR(fs, global, e->line, output);
    return output;
}

static void emit_get_field(struct LowerHir *L, int line, MirRegister object, int index, MirRegister output)
{
    struct FunctionState *fs = L->fs;
    NEW_INSTR(fs, get_field, line, index, output, object);
}

static struct MirSwitchArmList *allocate_switch_arms(struct LowerHir *L, MirBlock discr_bb, int count)
{
    struct MirSwitchArmList *arms = pawMir_switch_list_new(L->C);
    K_LIST_RESERVE(L->C, arms, count);
    for (int i = 0; i < count; ++i) {
        MirBlock const case_bb = new_bb(L);
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
    MirBlock const input_bb = current_bb(L);
    MirBlock const none_bb = new_bb(L);
    MirBlock const after_bb = new_bb(L);
    add_edge(L, input_bb, none_bb);

    MirRegister const object = lower_operand(V, e->target);
    MirRegister const discr = new_literal_reg(L, BUILTIN_INT);
    emit_get_field(L, e->line, object, 0, discr);

    struct MirSwitchArmList *arms = allocate_switch_arms(L, input_bb, 1);
    struct MirInstruction *switch_ = terminate_switch(L, discr, arms, none_bb);
    struct MirSwitchArm *arm = &K_LIST_GET(arms, 0);

    set_current_bb(L, arm->bid);
    MirRegister const value = register_for_node(L, e->hid);
    emit_get_field(L, e->line, object, 1, value);
    set_goto_edge(L, e->line, after_bb);

    set_current_bb(L, none_bb);
    terminate_return(L, &object);

    set_current_bb(L, after_bb);
    return value;
}

static MirRegister lower_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const val = lower_operand(V, e->target);
    MirRegister const output = register_for_node(L, e->hid);
    NEW_INSTR(fs, unary_op, e->line, e->op, val, output);
    return output;
}

static MirRegister lower_concat_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const lhs = lower_operand(V, e->lhs);
    MirRegister const rhs = lower_operand(V, e->rhs);
    MirRegister const output = register_for_node(L, e->hid);
    NEW_INSTR(fs, binary_op, e->line, e->op, lhs, rhs, output);
    return output;
}

static MirRegister lower_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    paw_Type const code = pawP_type2code(L->C, GET_NODE_TYPE(L->C, e->lhs));
    // OP_*CONCAT needs to be special-cased, since it causes memory allocations
    if (e->op == BINARY_ADD && (code == BUILTIN_STR || code == BUILTIN_LIST)) {
        return lower_concat_expr(V, e);
    }
    MirRegister const lhs = lower_operand(V, e->lhs);
    MirRegister const rhs = lower_operand(V, e->rhs);
    MirRegister const output = register_for_node(L, e->hid);
    NEW_INSTR(fs, binary_op, e->line, e->op, lhs, rhs, output);
    return output;
}

static void lower_function_block(struct LowerHir *L, struct HirExpr *block)
{
    MirRegister const result = lower_operand(&L->V, block);
    struct MirRegisterData const *data = mir_reg_data(L->fs->mir, result);
    if (!HirGetBlock(block)->never)
        terminate_return(L, &result);
}

static MirRegister lower_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *outer = L->fs;
    struct IrType *type = pawIr_get_type(L->C, e->hid);

    String *name = SCAN_STRING(L->C, "(closure)");
    struct Mir *result = pawMir_new(L->C, name, type, NULL,
        FUNC_CLOSURE, PAW_FALSE, PAW_FALSE, PAW_FALSE);

    struct BlockState bs;
    struct FunctionState fs;
    enter_function(L, &fs, &bs, e->line, result);

    pawHir_visit_decl_list(&L->V, e->params);
    if (HirIsBlock(e->expr)) {
        lower_function_block(L, e->expr);
    } else {
        // evaluate and return the expression
        MirRegister const result = lower_operand(&L->V, e->expr);
        terminate_return(L, &result);
    }
    result->upvalues = L->fs->up;
    leave_function(L);

    pawMir_remove_unreachable_blocks(result);
    pawSsa_construct(result);
    pawMir_propagate_constants(result);

    MirRegister const output = new_register(L, type);
    struct MirBodyList *children = outer->mir->children;
    NEW_INSTR(outer, closure, e->line, children->count, output);
    K_LIST_PUSH(L->C, children, result);
    return output;
}

static MirRegister lower_conversion_expr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    enum BuiltinKind from = kind_of_builtin(L, e->arg);
    MirRegister const output = register_for_node(L, e->hid);
    MirRegister const target = lower_operand(V, e->arg);
    if (from != e->to) {
        NEW_INSTR(fs, cast, e->line, target, output, from, e->to);
    } else {
        move_to(L, target, output);
    }
    return output;
}

static MirRegister lower_variant_constructor(struct HirVisitor *V, struct HirCallExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const object = register_for_node(L, e->hid);
    NEW_INSTR(fs, aggregate, e->line, 1 + e->args->count, object);

    MirRegister const discr = add_constant(L, I2V(d->index), BUILTIN_INT);

    // set the discriminant: an 'int' residing in the first Value slot of the ADT
    NEW_INSTR(fs, set_field, e->line, 0, object, discr);

    int index;
    struct HirExpr **pexpr;
    K_LIST_ENUMERATE(e->args, index, pexpr)
    {
        MirRegister const field = lower_operand(V, *pexpr);
        NEW_INSTR(fs, set_field, e->line, 1 + index, object, field);
    }

    return object;
}

static MirRegister lower_callee_and_args(struct HirVisitor *V, struct HirExpr *callee, struct HirExprList *args_in, struct MirRegisterList *args_out)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct IrType *recv = NULL; // receiver or owner of associated fn
    MirRegister result = register_for_node(L, callee->hdr.hid);
    if (HirIsSelector(callee) && !HirGetSelector(callee)->is_index) {
        // method call: place function object before 'self'
        struct HirSelector *select = HirGetSelector(callee);
        struct MirInstruction *instr = NEW_INSTR(fs, global, callee->hdr.line, result);
        struct MirGlobal *global = MirGetGlobal(instr);

        MirRegister const self = lower_operand(V, select->target);
        K_LIST_PUSH(L->C, args_out, self);

        recv = GET_NODE_TYPE(L->C, select->target);
    } else {
        result = lower_operand(V, callee);
        if (HirIsPathExpr(callee)) {
            struct HirPath *path = HirGetPathExpr(callee)->path;
            if (path->count > 1) {
                // "seg" is the owner of the associated function
                struct HirSegment seg = K_LIST_GET(path, path->count - 2);
                recv = pawIr_get_type(L->C, seg.hid);
            }
        }
    }
    if (recv != NULL && IrIsGeneric(recv)) {
        // determine actual receiver during monomorphization: need to look up
        // the method on the type that the generic is replaced with
        mir_reg_data(fs->mir, result)->self = recv;
    }
    lower_operand_list(V, args_in, args_out);
    return result;
}

static MirRegister lower_call_expr(struct HirVisitor *V, struct HirCallExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct IrType *target_type = GET_NODE_TYPE(L->C, e->target);
    if (IrIsSignature(target_type)) {
        struct HirDecl *decl = pawHir_get_decl(L->C, IR_TYPE_DID(target_type));
        if (HirIsVariantDecl(decl))
            return lower_variant_constructor(V, e, HirGetVariantDecl(decl));
    }

    struct MirRegisterList *args = pawMir_register_list_new(L->C);
    MirRegister const target = lower_callee_and_args(V, e->target, e->args, args);
    MirRegister const result = register_for_node(L, e->hid);
    NEW_INSTR(fs, call, e->line, target, args, result);
    return result;
}

static MirRegister lower_field_expr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    struct LowerHir *L = V->ud;
    if (e->fid < 0)
        lower_operand(V, e->key);
    return lower_operand(V, e->value);
}

static MirRegister first_slice_index(struct HirVisitor *V, struct HirExpr *e, int line)
{
    if (e != NULL)
        return lower_operand(V, e);
    struct LowerHir *L = V->ud; // default to integer 0
    return add_constant(V->ud, I2V(0), BUILTIN_INT);
}

static MirRegister second_slice_index(struct HirVisitor *V, struct HirExpr *e, MirRegister object, int line)
{
    if (e != NULL)
        return lower_operand(V, e);
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const output = new_literal_reg(L, BUILTIN_INT);
    NEW_INSTR(fs, unary_op, line, UNARY_LEN, object, output);
    return output;
}

static MirRegister lower_assign_expr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    if (HirIsPathExpr(e->lhs)) {
        struct NonGlobal ng;
        struct HirPathExpr *x = HirGetPathExpr(e->lhs);
        paw_Bool const expect_found = resolve_nonglobal(L, NO_DECL, K_LIST_LAST(x->path).name, &ng);
        paw_assert(expect_found);
        PAW_UNUSED(expect_found); // must be local or upvalue
        if (ng.is_upvalue) {
            MirRegister const rhs = lower_operand(V, e->rhs);
            NEW_INSTR(fs, set_upvalue, e->line, ng.index, rhs);
        } else {
            MirRegister const value = lower_operand(V, e->rhs);
            NEW_INSTR(fs, set_local, e->line, ng.r, value);
        }
    } else if (HirIsSelector(e->lhs)) {
        struct HirSelector *x = HirGetSelector(e->lhs);
        MirRegister const target = lower_operand(V, x->target);
        MirRegister const rhs = lower_operand(V, e->rhs);
        NEW_INSTR(fs, set_field, e->line, x->index, target, rhs);
    } else if (!HirGetIndex(e->lhs)->is_slice) {
        struct HirIndex *x = HirGetIndex(e->lhs);
        MirRegister const target = lower_operand(V, x->target);
        MirRegister const key = lower_operand(V, x->first);
        MirRegister const rhs = lower_operand(V, e->rhs);
        enum BuiltinKind b_kind = kind_of_builtin(L, x->target);
        NEW_INSTR(fs, set_element, e->line, b_kind, target, key, rhs);
    } else {
        struct HirIndex *x = HirGetIndex(e->lhs);
        MirRegister const target = lower_operand(V, x->target);
        MirRegister const lower = first_slice_index(V, x->first, e->line);
        MirRegister const upper = second_slice_index(V, x->second, target, e->line);
        MirRegister const rhs = lower_operand(V, e->rhs);
        enum BuiltinKind b_kind = kind_of_builtin(L, x->target);
        NEW_INSTR(fs, set_range, e->line, b_kind, target, lower, upper, rhs);
    }

    // setters are expressions that evaluate to "()"
    return unit_literal(L);
}

static MirRegister lower_index_expr(struct HirVisitor *V, struct HirIndex *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    enum BuiltinKind b_kind = kind_of_builtin(L, e->target);
    MirRegister const output = register_for_node(L, e->hid);
    MirRegister const object = lower_operand(V, e->target);
    if (!e->is_slice) {
        MirRegister const first = lower_operand(V, e->first);
        NEW_INSTR(fs, get_element, e->line, b_kind, output, object, first);
    } else { // stack: .. first second ..
        MirRegister const first = first_slice_index(V, e->first, e->line);
        MirRegister const second = second_slice_index(V, e->second, object, e->line);
        NEW_INSTR(fs, get_range, e->line, b_kind, output, object, first, second);
    }
    return output;
}

static MirRegister lower_selector_expr(struct HirVisitor *V, struct HirSelector *e)
{
    paw_assert(e->is_index);
    struct LowerHir *L = V->ud;
    MirRegister const output = register_for_node(L, e->hid);
    MirRegister const object = lower_operand(V, e->target);
    emit_get_field(L, e->line, object, e->index, output);
    return output;
}

static MirRegister lower_block(struct HirVisitor *V, struct HirBlock *e)
{
    struct BlockState bs;
    struct LowerHir *L = V->ud;
    enter_block(L, &bs, PAW_FALSE);
    pawHir_visit_stmt_list(V, e->stmts);
    MirRegister const result = lower_operand(V, e->result);

    leave_block(L);
    return result;
}

static MirRegister lower_if_expr(struct HirVisitor *V, struct HirIfExpr *e)
{
    struct LowerHir *L = V->ud;
    MirRegister const cond = lower_operand(V, e->cond);
    MirBlock const cond_bb = current_bb(L);
    MirBlock const then_bb = new_bb(L);
    MirBlock const else_bb = new_bb(L);
    MirBlock const join_bb = new_bb(L);
    add_edge(L, cond_bb, then_bb);
    add_edge(L, cond_bb, else_bb);

    MirRegister const result = register_for_node(L, e->hid);
    struct MirInstruction *r = terminate_branch(L, cond, then_bb, else_bb);
    set_current_bb(L, then_bb);
    MirRegister const first = lower_operand(V, e->then_arm);
    move_to(L, first, result);
    set_goto_edge(L, e->line, join_bb);

    set_current_bb(L, else_bb);
    if (e->else_arm == NULL) {
        MirRegister const second = unit_literal(L);
        move_to(L, second, result);
    } else {
        MirRegister const second = lower_operand(V, e->else_arm);
        move_to(L, second, result);
    }
    set_goto_edge(L, e->line, join_bb);

    set_current_bb(L, join_bb);
    return result;
}

static MirRegister lower_loop_expr(struct HirVisitor *V, struct HirLoopExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirRegister const result = unit_literal(L);
    MirBlock const before_bb = current_bb(L);
    MirBlock const header_bb = new_bb(L);
    MirBlock const after_bb = new_bb(L);
    add_edge(L, before_bb, header_bb);
    terminate_goto(L, e->line, header_bb);

    struct BlockState bs;
    enter_block(L, &bs, PAW_TRUE);

    set_current_bb(L, header_bb);
    lower_operand(V, e->block);

    MirBlock const loop_bb = current_bb(L);
    set_goto_edge(L, e->line, header_bb);

    adjust_to(L, JUMP_CONTINUE, header_bb);
    set_current_bb(L, after_bb);

    leave_block(L);
    return result;
}

static paw_Bool is_within_loop(struct LowerHir *L)
{
    struct BlockState *bs = L->fs->bs;
    while (bs != NULL) {
        if (bs->is_loop)
            return PAW_TRUE;
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
    add_label(L, e->line, e->jump_kind);
    set_current_bb(L, new_bb(L));
    return unit_literal(L);
}

static MirRegister lower_return_expr(struct HirVisitor *V, struct HirReturnExpr *e)
{
    struct LowerHir *L = V->ud;
    if (e->expr != NULL) {
        MirRegister const value = lower_operand(V, e->expr);
        terminate_return(L, &value);
    } else {
        // creates a dummy register for a value of type "()"
        terminate_return(L, NULL);
    }
    MirBlock const next_bb = new_bb(L);
    set_current_bb(L, next_bb);
    return unit_literal(L);
}

static MirRegister lower_match_expr(struct HirVisitor *V, struct HirMatchExpr *e);

static MirRegister lower_operand(struct HirVisitor *V, struct HirExpr *expr)
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

static MirRegister get_test_reg(struct LowerHir *L, struct MatchVar v)
{
    MirRegister const *pr = VarMap_get(L->C, L->ms->var_mapping, v);
    paw_assert(pr != NULL);
    return *pr;
}

static void declare_match_bindings(struct LowerHir *L, struct BindingList *bindings)
{
    for (int i = 0; i < bindings->count; ++i) {
        struct Binding b = K_LIST_GET(bindings, i);
        MirRegister const r = get_test_reg(L, b.var);
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

    MirRegister const cond = lower_operand(V, d->guard.cond);
    MirBlock const before_bb = current_bb(L);
    MirBlock const then_bb = new_bb(L);
    MirBlock const else_bb = new_bb(L);
    MirBlock const join_bb = new_bb(L);
    add_edge(L, before_bb, then_bb);
    add_edge(L, before_bb, else_bb);

    struct MirInstruction *branch = terminate_branch(L, cond, then_bb, else_bb);
    set_current_bb(L, then_bb);
    lower_match_body(V, d->guard.body, result);
    set_goto_edge(L, -1, join_bb);

    set_current_bb(L, else_bb);
    visit_decision(V, d->guard.rest, result);
    set_goto_edge(L, -1, join_bb);

    set_current_bb(L, join_bb);
}

static MirBlock lower_match_body(struct HirVisitor *V, struct MatchBody body, MirRegister result)
{
    struct LowerHir *L = V->ud;
    MirBlock const bid = current_bb(L);
    declare_match_bindings(L, body.bindings);
    MirRegister const r = lower_operand(V, body.result);
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
    }
    if (IS_BASIC_TYPE(pawP_type2code(L->C, type)))
        return NULL;
    struct HirDecl *decl = pawHir_get_decl(L->C, IR_TYPE_DID(type));
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    struct IrTypeList *result = pawIr_type_list_new(L->C);
    if (!d->is_struct) {
        // first field of an enumerator is the integer discriminant
        K_LIST_PUSH(L->C, result, get_type(L, BUILTIN_INT));
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
        MirRegister const r = new_register(L, K_LIST_GET(types, i));
        K_LIST_PUSH(L->C, result, r);
    }
    return result;
}

static void map_var_to_reg(struct LowerHir *L, struct MatchVar var, MirRegister reg)
{
    VarMap_insert(L->C, L->ms->var_mapping, var, reg);
}

static void allocate_match_vars(struct LowerHir *L, MirRegister discr, struct MatchCase mc, paw_Bool is_enum)
{
    if (mc.vars->count == 0)
        return;
    struct MirRegisterList *regs = L->ms->regs;
    for (int i = 0; i < mc.vars->count; ++i) {
        struct MatchVar v = K_LIST_GET(mc.vars, i);
        MirRegister const r = new_register(L, v.type);
        map_var_to_reg(L, v, r);
        emit_get_field(L, -1, discr, is_enum + i, r);
        add_local_literal(L, "(match variable)", r);
    }
}

static void visit_sparse_cases(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;
    struct CaseList *cases = d->multi.cases;
    MirBlock const discr_bb = current_bb(L);
    MirBlock const otherwise_bb = new_bb(L);
    MirBlock const join_bb = new_bb(L);
    add_edge(L, discr_bb, otherwise_bb);

    MirRegister const test = get_test_reg(L, d->multi.test);
    struct MirSwitchArmList *arms = allocate_switch_arms(L, discr_bb, cases->count);
    struct MirInstruction *switch_ = terminate_switch(L, test, arms, otherwise_bb);

    for (int i = 0; i < cases->count; ++i) {
        struct MatchCase mc = K_LIST_GET(cases, i);
        struct MirSwitchArm *arm = &K_LIST_GET(arms, i);
        set_current_bb(L, arm->bid);
        arm->value = mc.cons.value;

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        visit_decision(V, mc.dec, result);
        leave_block(L);

        set_goto_edge(L, -1, join_bb);
    }

    // this implementation requires an "otherwise" case (binding or wildcard) to ensure
    // exhaustivness
    paw_assert(d->multi.rest != NULL);
    set_current_bb(L, otherwise_bb);
    visit_decision(V, d->multi.rest, result);
    set_goto_edge(L, -1, join_bb);

    set_current_bb(L, join_bb);
}

static void visit_variant_cases(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;
    struct CaseList *cases = d->multi.cases;

    MirBlock const discr_bb = current_bb(L);
    MirBlock const join_bb = new_bb(L);
    MirRegister const variant = get_test_reg(L, d->multi.test);
    MirRegister const test = new_literal_reg(L, BUILTIN_INT);
    add_local_literal(L, "(match discriminant)", test); // keep alive
    emit_get_field(L, -1, variant, 0, test);

    struct MirSwitchArmList *arms = allocate_switch_arms(L, discr_bb, cases->count);
    struct MirInstruction *switch_ = terminate_switch(L, test, arms, MIR_INVALID_BB);

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

        set_goto_edge(L, -1, join_bb);
    }
    paw_assert(d->multi.rest == NULL);

    set_current_bb(L, join_bb);
}

static void visit_tuple_case(struct HirVisitor *V, struct Decision *d, MirRegister result)
{
    struct LowerHir *L = V->ud;
    MirRegister const discr = get_test_reg(L, d->multi.test);

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
    MirRegister const discr = get_test_reg(L, d->multi.test);

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
        case DECISION_SUCCESS:
            visit_success(V, d, result);
            break;
        case DECISION_GUARD:
            visit_guard(V, d, result);
            break;
        case DECISION_MULTIWAY:
            visit_multiway(V, d, result);
            break;
        case DECISION_FAILURE:
            PAW_UNREACHABLE();
    }

    leave_block(L);
}

static MirRegister lower_match_expr(struct HirVisitor *V, struct HirMatchExpr *e)
{
    struct LowerHir *L = V->ud;
    VarMap *var_mapping = VarMap_new(L->C);

    struct BlockState bs;
    struct MatchState ms;
    enter_block(L, &bs, PAW_FALSE);
    enter_match(L, &ms, var_mapping);

    struct Decision *d = pawP_check_exhaustiveness(L->C, e, ms.vars);
    paw_assert(ms.vars->count > 0);
    MirRegister const discr = lower_operand(V, e->target);
    MirRegister const result = register_for_node(L, e->hid);
    map_var_to_reg(L, K_LIST_FIRST(ms.vars), discr);
    add_local_literal(L, "(match target)", discr);
    K_LIST_PUSH(L->C, ms.regs, discr);

    visit_decision(V, d, result);

    leave_match(L);
    leave_block(L);

    VarMap_delete(L->C, var_mapping);
    return result;
}

static void lower_hir_body(struct LowerHir *L, struct HirFuncDecl *func, struct Mir *mir)
{
    struct BlockState bs;
    struct FunctionState fs;
    enter_function(L, &fs, &bs, func->line, mir);

    pawHir_visit_decl_list(&L->V, func->params);
    lower_function_block(L, func->body);

    leave_function(L);
}

struct Mir *pawP_lower_hir_body(struct Compiler *C, struct HirFuncDecl *func)
{
    struct IrType *type = pawIr_get_type(C, func->hid);
    struct IrSignature *fsig = IrGetSignature(type);
    paw_Bool const is_polymorphic = func->generics != NULL
        || (fsig->self != NULL && IR_TYPE_SUBTYPES(fsig->self) != NULL);
    struct Mir *result = pawMir_new(C, func->name, type,
                                    fsig->self, func->fn_kind, func->body == NULL,
                                    func->is_pub, is_polymorphic);
    if (func->body == NULL)
        return result;

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

    lower_hir_body(&L, func, result);
    pawMir_remove_unreachable_blocks(result);
    pawSsa_construct(result);
    pawMir_propagate_constants(result);
    return result;
}

BodyMap *pawP_lower_hir(struct Compiler *C)
{
    BodyMap *result = BodyMap_new(C);
    struct HirDeclList *decls = C->decls;
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(decls, i);
        if (HirIsFuncDecl(decl)) {
            struct IrType *type = GET_NODE_TYPE(C, decl);
            struct IrSignature *fsig = IrGetSignature(type);
            if (fsig->self == NULL || IrIsAdt(fsig->self)) {
                struct HirFuncDecl *d = HirGetFuncDecl(decl);
                struct Mir *r = pawP_lower_hir_body(C, d);
                BodyMap_insert(C, result, d->did, r);
            }
        }
    }
    return result;
}
