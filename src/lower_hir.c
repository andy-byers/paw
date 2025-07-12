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
// TODO: Use information collected during name resolution instead of "resolve_*"
//       functions. Figure out OrPat stuff (maybe expand into multiple match arms
//       during AST lowering or something)
//
// TODO: Implementation would be a bit nicer if closures were hoisted out of
//       their enclosing functions. Closures also need to store the ".child_id" so
//       they can be placed in their enclosing Proto (so they can be found at
//       runtime).

#include "api.h"
#include "error.h"
#include "hir.h"
#include "ir_type.h"
#include "layout.h"
#include "match.h"
#include "mir.h"
#include "ssa.h"
#include "unify.h"

#define LOWERING_ERROR(L_, Kind_, ...) pawErr_##Kind_((L_)->C, (L_)->pm->name, __VA_ARGS__)
#define REG_AT(Base_, Offset_) MIR_REG((Base_).value + (Offset_))
#define NODE_START(Node_) ((Node_)->hdr.span.start)
#define NODE_END(Node_) ((Node_)->hdr.span.end)
#define PLACE(Reg_) ((struct MirPlace){.r = Reg_})

struct MatchState {
    struct VarPlaces *var_mapping;
    struct MatchState *outer;
    struct MatchVars *vars;
};

struct FunctionState {
    struct FunctionState *outer;
    struct MirRegisterDataList *registers;
    struct MirCaptureList *captured;
    struct MirRegisterList *locals;
    struct LocalMap *mapping;
    struct MirUpvalueList *up;
    struct LabelList *labels;
    struct VarStack *stack;
    struct MatchState *ms;
    struct BlockState *bs;
    struct LowerHir *L;
    struct Compiler *C;
    struct Mir *mir;
    IrType *result;
    MirBlock current;
    int nlocals;
    int level;
};

struct ConstantContext {
    struct ConstantContext *outer;
    DeclId did;
};

struct LocalVar {
    struct FunctionState *fs;
    struct MirPlace r;
    Str *name;
    int depth;
    int index;
};

struct BlockState {
    struct BlockState *outer;
    struct SourceSpan span;
    int depth;
    int label0;
    int nvars;
    paw_Bool has_upvalue : 1;
    paw_Bool is_loop : 1;
};

struct Label {
    int nvars;
    MirBlock from;
    paw_Bool needs_close : 1;
    enum JumpKind kind : 7;
};

struct LowerHir {
    struct HirVisitor *V;
    struct Compiler *C;
    struct Pool *pool;
    struct Hir *hir;
    paw_Env *P;

    struct HirModule *pm;
    struct ConstantContext *cctx;
    struct FunctionState *fs;
    struct LabelList *labels;
    struct VarStack *stack;
    struct LocalMap *locals;
    struct GlobalMap *globals;
};

static paw_Uint var_hash(struct LowerHir *L, struct MatchVar v)
{
    PAW_UNUSED(L);
    return v.id;
}

static paw_Bool var_equals(struct LowerHir *L, struct MatchVar a, struct MatchVar b)
{
    PAW_UNUSED(L);
    return a.id == b.id;
}

static paw_Uint type_hash(struct LowerHir *L, IrType *t)
{
    return pawIr_type_hash(L->C, t);
}

static paw_Bool type_equals(struct LowerHir *L, IrType *a, IrType *b)
{
    return pawIr_type_equals(L->C, a, b);
}

DEFINE_LIST(struct LowerHir, VarStack, struct LocalVar)
DEFINE_LIST(struct LowerHir, LabelList, struct Label)
DEFINE_MAP(struct LowerHir, LocalMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, int)
DEFINE_MAP(struct LowerHir, GlobalMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, int)
DEFINE_MAP(struct LowerHir, VarPlaces, pawP_alloc, var_hash, var_equals, struct MatchVar, struct MirPlace)

static void postprocess(struct Mir *mir)
{
    pawMir_remove_unreachable_blocks(mir);
}

static void enter_constant_ctx(struct LowerHir *L, struct ConstantContext *cctx, struct SourceSpan span, DeclId did)
{
    struct ConstantContext *cursor = L->cctx;
    while (cursor != NULL) {
        if (did.value == cursor->did.value) {
            struct HirDecl *decl = pawHir_get_decl(L->hir, did);
            LOWERING_ERROR(L, global_constant_cycle, span.start,
                    hir_decl_ident(decl).name->text);
        }
        cursor = cursor->outer;
    }
    *cctx = (struct ConstantContext){
        .outer = L->cctx,
        .did = did,
    };
    L->cctx = cctx;
}

static void leave_constant_ctx(struct LowerHir *L)
{
    paw_assert(L->cctx != NULL);
    L->cctx = L->cctx->outer;
}

static void enter_match(struct FunctionState *fs, struct MatchState *ms, VarPlaces *var_mapping)
{
    *ms = (struct MatchState){
        .var_mapping = var_mapping,
        .vars = MatchVars_new(fs->C),
        .outer = fs->ms,
    };
    fs->ms = ms;
}

static void leave_match(struct FunctionState *fs)
{
    fs->ms = fs->ms->outer;
}

static enum BuiltinKind builtin_kind(struct LowerHir *L, IrType *type)
{
    return pawP_type2code(L->C, type);
}

static IrType *get_type(struct LowerHir *L, enum BuiltinKind kind)
{
    return pawP_builtin_type(L->C, kind);
}

static struct MirBlockDataList *bb_list(struct FunctionState *fs)
{
    paw_assert(fs->mir->blocks != NULL);
    return fs->mir->blocks;
}

static struct MirBlockData *get_bb(struct FunctionState *fs, MirBlock bb)
{
    return MirBlockDataList_get(bb_list(fs), bb.value);
}

static MirBlock current_bb(struct FunctionState *fs)
{
    paw_assert(bb_list(fs)->count > 0);
    return fs->current;
}

static struct MirBlockData *current_bb_data(struct FunctionState *fs)
{
    paw_assert(bb_list(fs)->count > 0);
    return MirBlockDataList_get(bb_list(fs), current_bb(fs).value);
}

// Allocate virtual registers for a value of the given "type"
// Populates a description of the value's memory layout, and returns the lowest-numbered
// virtual register. Registers allocated in each invocation of this function are contiguous.
static struct MirPlace new_place(struct FunctionState *fs, IrType *type)
{
    struct IrLayout const layout = pawIr_compute_layout(fs->C, type);
    int const num_registers = fs->mir->registers->count;
    MirRegisterDataList_push(fs->mir, fs->mir->registers, (struct MirRegisterData){
        .size = layout.size,
        .type = type,
    });
    return (struct MirPlace){
        .projection = MirProjectionList_new(fs->mir),
        .r = MIR_REG(num_registers),
    };
}

static struct MirPlace new_literal_place(struct FunctionState *fs, paw_Type code)
{
    return new_place(fs, get_type(fs->L, code));
}

static void add_edge(struct FunctionState *fs, MirBlock from, MirBlock to)
{
    struct MirBlockData *source = get_bb(fs, from);
    struct MirBlockData *target = get_bb(fs, to);
    MirBlockList_push(fs->mir, source->successors, to);
    MirBlockList_push(fs->mir, target->predecessors, from);
}

static struct MirInstruction *add_instruction(struct FunctionState *fs, struct MirInstruction *instr)
{
    struct MirBlockData *block = current_bb_data(fs);
    MirInstructionList_push(fs->mir, block->instructions, instr);
    return instr;
}

#define NEW_INSTR(Fs_, Kind_, ...) add_instruction(Fs_, pawMir_new_##Kind_((Fs_)->mir, __VA_ARGS__))

static struct MirInstruction *terminate_unreachable(struct FunctionState *fs, struct SourceLoc loc)
{
    return NEW_INSTR(fs, unreachable, loc);
}

static struct MirInstruction *terminate_goto(struct FunctionState *fs, struct SourceLoc loc, MirBlock target)
{
    return NEW_INSTR(fs, goto, loc, target);
}

static void set_goto_edge(struct FunctionState *fs, struct SourceLoc loc, MirBlock to)
{
    add_edge(fs, current_bb(fs), to);
    terminate_goto(fs, loc, to);
}

static void set_current_bb(struct FunctionState *fs, MirBlock b)
{
    fs->current = b;
}

static MirBlock new_bb(struct FunctionState *fs)
{
    int const id = bb_list(fs)->count;
    struct MirBlockData *bb = pawMir_new_block(fs->mir);
    MirBlockDataList_push(fs->mir, bb_list(fs), bb);
    return MIR_BB(id);
}

static struct LocalVar *get_local_slot(struct FunctionState *fs, int index)
{
    return &K_LIST_AT(fs->stack, fs->level + index);
}

static void close_until_loop(struct FunctionState *fs)
{
    MirRegister lowest_upvalue;
    paw_Bool needs_close = PAW_FALSE;
    struct VarStack *stack = fs->stack;
    int index = stack->count - 1;
    struct BlockState *bs = fs->bs;
    while (bs != NULL) {
        needs_close |= bs->has_upvalue;
        if (bs->has_upvalue) {
            // find the upvalue with the smallest index in block "bs"
            for (; index >= bs->nvars; --index) {
                struct LocalVar const var = VarStack_get(stack, index);
                struct MirRegisterData const *data = mir_reg_data(fs->mir, var.r.r);
                if (data->is_captured)
                    lowest_upvalue = var.r.r;
            }
        }
        if (bs->is_loop)
            break;
        bs = bs->outer;
    }
    if (needs_close) {
        struct MirPlace p = {.r = lowest_upvalue};
        NEW_INSTR(fs, close, (struct SourceLoc){-1}, p);
    }
}

static void add_label(struct FunctionState *fs, struct SourceLoc loc, enum JumpKind kind)
{
    // MirClose must be added here if "continue" causes control to leave a scope containing
    // a captured variable, since the MirClose at the end of the loop body will not be reached.
    if (kind == JUMP_CONTINUE)
        close_until_loop(fs);
    struct MirBlockData *block = current_bb_data(fs);
    terminate_goto(fs, loc, MIR_INVALID_BB);

    LabelList_push(fs->L, fs->labels, (struct Label){
        .nvars = fs->nlocals,
        .from = current_bb(fs),
        .kind = kind,
    });
}

static void adjust_labels(struct FunctionState *fs, struct BlockState *bs)
{
    struct LabelList *ll = fs->labels;
    for (int i = bs->label0; i < ll->count; ++i) {
        struct Label *lb = &K_LIST_AT(ll, i);
        if (lb->nvars > bs->nvars) {
            lb->needs_close |= bs->has_upvalue;
        }
        lb->nvars = bs->nvars;
    }
}

static void remove_label(struct LabelList *ll, int index)
{
    LabelList_swap_remove(ll, index);
}

static void set_goto(struct FunctionState *fs, MirBlock from, MirBlock to)
{
    struct MirBlockData *bb = get_bb(fs, from);
    struct MirInstruction *jump = K_LIST_LAST(bb->instructions);
    MirGetGoto(jump)->target = to;
}

static paw_Bool adjust_from(struct FunctionState *fs, enum JumpKind kind)
{
    int needs_close = 0;
    struct BlockState *bs = fs->bs;
    struct LabelList *ll = fs->labels;
    for (int i = bs->label0; i < ll->count;) {
        struct Label lb = LabelList_get(ll, i);
        if (lb.kind == kind) {
            needs_close |= lb.needs_close;
            set_goto(fs, lb.from, current_bb(fs));
            add_edge(fs, lb.from, current_bb(fs));
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
    return needs_close;
}

static void adjust_to(struct FunctionState *fs, enum JumpKind kind, MirBlock to)
{
    struct BlockState *bs = fs->bs;
    struct LabelList *ll = fs->labels;
    for (int i = bs->label0; i < ll->count;) {
        struct Label lb = LabelList_get(ll, i);
        if (lb.kind == kind) {
            set_goto(fs, lb.from, to);
            add_edge(fs, lb.from, to);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static void move_to(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace from, struct MirPlace to)
{
    NEW_INSTR(fs, move, loc, to, from);
}

static enum BuiltinKind kind_of_builtin(struct LowerHir *L, struct HirExpr *expr)
{
    IrType *type = GET_NODE_TYPE(L->C, expr);
    return builtin_kind(L, type);
}

// Represents a local variable or an upvalue
struct NonGlobal {
    IrType *type;
    struct MirPlace r;
    int index;
    paw_Bool is_upvalue;
};

// Mark a block as containing an upvalue
static void mark_upvalue(struct FunctionState *fs, int target, struct MirPlace r)
{
    struct BlockState *bs = fs->bs;
    while (bs->nvars > target)
        bs = bs->outer;
    bs->has_upvalue = PAW_TRUE;

    struct MirRegisterData *data = mir_reg_data(fs->mir, r.r);
    if (!data->is_captured) {
        struct MirCaptureInfo const ci = {r.r};
        MirCaptureList_push(fs->mir, fs->captured, ci);
        NEW_INSTR(fs, capture, (struct SourceLoc){-1}, r);
        data->is_captured = PAW_TRUE;
    }
}

static void add_upvalue(struct FunctionState *fs, struct NonGlobal *info, paw_Bool is_local)
{
    info->is_upvalue = PAW_TRUE;
    info->r.kind = MIR_PLACE_UPVALUE;

    int index;
    struct MirUpvalueInfo *pup;
    K_LIST_ENUMERATE (fs->up, index, pup) {
        if (is_local == pup->is_local && pup->index == info->index) {
            info->index = index;
            info->r.up = index;
            return;
        }
    }
    if (fs->up->count == UPVALUE_MAX)
        LOWERING_ERROR(fs->L, too_many_upvalues, fs->mir->span.start, UPVALUE_MAX);

    MirUpvalueList_push(fs->mir, fs->up, (struct MirUpvalueInfo){
        .is_local = is_local,
        .index = info->index,
        .type = info->type,
    });
    // indicate new upvalue index
    info->index = fs->up->count - 1;
    info->r.up = fs->up->count - 1;
}

static void enter_block(struct FunctionState *fs, struct BlockState *bs, struct SourceSpan span, paw_Bool is_loop)
{
    *bs = (struct BlockState){
        .depth = fs->bs == NULL ? 0 : fs->bs->depth + 1,
        .label0 = fs->labels->count,
        .nvars = fs->nlocals,
        .is_loop = is_loop,
        .outer = fs->bs,
        .span = span,
    };
    fs->bs = bs;
}

static void maybe_close(struct FunctionState *fs, struct MirPlace r)
{
    struct MirRegisterData *data = mir_reg_data(fs->mir, r.r);
    if (data->is_captured)
        NEW_INSTR(fs, close, (struct SourceLoc){-1}, r);
}

static void close_variables(struct FunctionState *fs, int nvars)
{
    for (int i = fs->nlocals - 1; i >= nvars; --i) {
        struct LocalVar const local = VarStack_get(fs->stack, fs->level + i);
        maybe_close(fs, local.r);
    }
}

static void leave_block(struct FunctionState *fs)
{
    struct BlockState *bs = fs->bs;
    int needs_close = bs->has_upvalue && bs->outer != NULL;
    if (bs->is_loop)
        needs_close |= adjust_from(fs, JUMP_BREAK);
    if (needs_close)
        close_variables(fs, bs->nvars);
    if (bs->outer != NULL)
        adjust_labels(fs, bs);

    fs->stack->count = fs->level + bs->nvars;
    fs->nlocals = bs->nvars;
    fs->bs = bs->outer;
}

static struct LocalVar alloc_local(struct FunctionState *fs, struct HirIdent ident, NodeId id, IrType *type)
{
    struct MirPlace const output = new_place(fs, type);
    NEW_INSTR(fs, alloc_local, ident.span.start, ident.name, output);

    LocalMap_insert(fs->L, fs->mapping, id, fs->stack->count);
    VarStack_push(fs->L, fs->stack, (struct LocalVar){
        .index = fs->locals->count,
        .depth = fs->bs->depth,
        .name = ident.name,
        .r = output,
        .fs = fs,
    });
    MirRegisterList_push(fs->mir, fs->locals, output.r);
    ++fs->nlocals;
    return K_LIST_LAST(fs->stack);
}

static paw_Bool resolve_upvalue(struct FunctionState *fs, struct LocalVar local, struct NonGlobal *png)
{
    struct FunctionState *caller = fs->outer;
    if (caller == NULL) return PAW_FALSE;

    if (caller == local.fs) {
        mark_upvalue(caller, png->index, png->r);
        add_upvalue(fs, png, PAW_TRUE);
    } else if (resolve_upvalue(caller, local, png)) {
        add_upvalue(fs, png, PAW_FALSE);
    }
    return PAW_TRUE;
}

static paw_Bool resolve_nonglobal(struct FunctionState *fs, NodeId id, struct NonGlobal *png)
{
    int const *pindex = LocalMap_get(fs->L, fs->mapping, id);
    if (pindex == NULL) return PAW_FALSE;

    struct LocalVar const local = VarStack_get(fs->stack, *pindex);
    struct MirRegisterData const *data = mir_reg_data(local.fs->mir, local.r.r);
    *png = (struct NonGlobal){
        .type = data->type,
        .index = local.index,
        .r = local.r,
    };

    if (local.fs != fs)
        resolve_upvalue(fs, local, png);
    return PAW_TRUE;
}

static struct MirPlace add_constant(struct FunctionState *fs, struct SourceLoc loc, Value value, enum BuiltinKind b_kind)
{
    paw_assert(b_kind != NBUILTINS);
    struct MirPlace const target = new_place(fs, get_type(fs->L, b_kind));
    NEW_INSTR(fs, load_constant, loc, b_kind, value, target);
    return target;
}

static struct MirPlace unit_literal(struct FunctionState *fs, struct SourceLoc loc)
{
    return add_constant(fs, loc, I2V(0), BUILTIN_UNIT);
}

struct MirInstruction *terminate_return(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace value)
{
    MirPlaceList *values = MirPlaceList_new(fs->mir);
    struct MirPlace const result = MIR_ID_EXISTS(value.r) ? value : unit_literal(fs, loc);
    MirPlaceList_push(fs->mir, values, result);
    return NEW_INSTR(fs, return, loc, values);
}

struct MirInstruction *terminate_branch(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace cond, MirBlock then_arm, MirBlock else_arm)
{
    return NEW_INSTR(fs, branch, loc, cond, then_arm, else_arm);
}

struct MirInstruction *terminate_switch(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace discr, struct MirSwitchArmList *arms, MirBlock otherwise)
{
    return NEW_INSTR(fs, switch, loc, discr, arms, otherwise);
}

static struct MirPlace place_for_node(struct FunctionState *fs, NodeId id)
{
    return new_place(fs, pawIr_get_type(fs->C, id));
}

// TODO: consider not entering a block in this function, declare parameters inside body block
static MirBlock enter_function(struct LowerHir *L, struct FunctionState *fs, struct BlockState *bs, struct Mir *mir)
{
    *fs = (struct FunctionState){
        .result = IR_FPTR(mir->type)->result,
        .captured = mir->captured,
        .registers = mir->registers,
        .up = MirUpvalueList_new(mir),
        .level = L->stack->count,
        .mapping = L->locals,
        .locals = mir->locals,
        .labels = L->labels,
        .stack = L->stack,
        .outer = L->fs,
        .mir = mir,
        .C = L->C,
        .L = L,
    };
    L->fs = fs;

    struct IrFnPtr *fptr = IR_FPTR(mir->type);
    MirBlock const entry = new_bb(fs);
    set_current_bb(fs, entry);
    enter_block(fs, bs, mir->span, PAW_FALSE);

    struct HirIdent const ident = {
        .name = SCAN_STR(L->C, PRIVATE("callable")),
        .span = mir->span.start,
    };
    alloc_local(fs, ident, NO_NODE, mir->type);

    IrType *const *pparam;
    K_LIST_FOREACH (fptr->params, pparam) {
        struct IrLayout const layout = pawIr_compute_layout(fs->C, *pparam);
        mir->param_size += ir_is_boxed(fs->C, *pparam) ? 1 : layout.size;
    }

    return entry;
}

static void leave_function(struct LowerHir *L)
{
    struct FunctionState *fs = L->fs;
    struct MirBlockData *block = current_bb_data(fs);
    if (block->instructions->count == 0
            || !MirIsReturn(K_LIST_LAST(block->instructions))) {
        terminate_return(fs, fs->mir->span.end, PLACE(MIR_INVALID_REG));
    }
    fs->stack->count = fs->level;
    L->fs = fs->outer;
}

#define LOWER_BLOCK(L, b) lower_place((L)->V, HIR_CAST_EXPR(b))

static void auto_deref(struct FunctionState *fs, struct MirPlace *pplace, IrType *target)
{
    if (!IrIsAdt(target))
        return;

    struct IrAdtDef *def = pawIr_get_adt_def(fs->C, IR_TYPE_DID(target));
    if (!def->is_inline) {
        MirProjection *deref = MirProjection_new_deref(fs->mir);
        MirProjectionList_push(fs->mir, pplace->projection, deref);
    }
}

static struct MirPlace select_field(struct FunctionState *fs, struct MirPlace place, IrType *target, int index, int variant)
{
    struct MirPlace p = pawMir_copy_place(fs->mir, place);

    auto_deref(fs, &p, target);
    MirProjection *field = MirProjection_new_field(fs->mir, index, variant);
    MirProjectionList_push(fs->mir, p.projection, field);
    return p;
}

static struct MirPlace select_element(struct FunctionState *fs, struct MirPlace place, IrType *target, MirRegister index)
{
    struct MirPlace p = pawMir_copy_place(fs->mir, place);

    auto_deref(fs, &p, target);
    MirProjection *elem = MirProjection_new_index(fs->mir, index);
    MirProjectionList_push(fs->mir, p.projection, elem);
    return p;
}

static struct MirPlace select_range(struct FunctionState *fs, struct MirPlace place, IrType *target, MirRegister lower, MirRegister upper)
{
    struct MirPlace p = pawMir_copy_place(fs->mir, place);

    auto_deref(fs, &p, target);
    MirProjection *elems = MirProjection_new_range(fs->mir, lower, upper);
    MirProjectionList_push(fs->mir, p.projection, elems);
    return p;
}

static void lower_place_into(struct HirVisitor *V, struct HirExpr *expr, struct MirPlace *pplace);
static struct MirPlace lower_place(struct HirVisitor *V, struct HirExpr *expr);
static struct MirPlace lower_path_expr(struct HirVisitor *V, struct HirPathExpr *e);

static struct MirPlace first_slice_index(struct FunctionState *fs, struct HirVisitor *V, struct SourceLoc loc, struct HirExpr *e)
{
    if (e != NULL) return lower_place(V, e);

    // default to integer 0
    return add_constant(fs, loc, I2V(0), BUILTIN_INT);
}

static struct MirPlace second_slice_index(struct FunctionState *fs, struct HirVisitor *V, struct SourceLoc loc, struct HirExpr *e, struct MirPlace object, int offset)
{
    if (e == NULL) {
        // default to the length of the container
        struct MirPlace const output = new_literal_place(fs, BUILTIN_INT);
        enum BuiltinKind const kind = pawP_type2code(fs->C, mir_reg_data(fs->mir, object.r)->type);
        enum MirUnaryOpKind op = kind == BUILTIN_STR ? MIR_UNARY_STRLEN : MIR_UNARY_LISTLEN;
        NEW_INSTR(fs, unary_op, loc, op, object, output);
        return output;
    } else if (offset != 0) {
        struct MirPlace index = lower_place(V, e);
        struct MirPlace one = add_constant(fs, loc, I2V(offset), BUILTIN_INT);
        NEW_INSTR(fs, binary_op, loc, MIR_BINARY_IADD, index, one, index);
        return index;
    } else {
        return lower_place(V, e);
    }
}

static void lower_range_index(struct HirVisitor *V, struct HirExpr *index, struct MirPlace object, struct MirPlace *plower, struct MirPlace *pupper)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace first, second;
    IrType *type = GET_NODE_TYPE(fs->C, index);
    switch (builtin_kind(L, type)) {
        case BUILTIN_RANGE: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *lower = HirGetFieldExpr(K_LIST_FIRST(lit.items))->value;
            struct HirExpr *upper = HirGetFieldExpr(K_LIST_LAST(lit.items))->value;
            first = first_slice_index(fs, V, NODE_START(index), lower);
            second = second_slice_index(fs, V, NODE_START(index), upper, object, 0);
            break;
        }
        case BUILTIN_RANGE_FROM: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *lower = HirGetFieldExpr(K_LIST_FIRST(lit.items))->value;
            first = first_slice_index(fs, V, NODE_START(index), lower);
            second = second_slice_index(fs, V, NODE_START(index), NULL, object, 0);
            break;
        }
        case BUILTIN_RANGE_TO: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *upper = HirGetFieldExpr(K_LIST_LAST(lit.items))->value;
            first = first_slice_index(fs, V, NODE_START(index), NULL);
            second = second_slice_index(fs, V, NODE_START(index), upper, object, 0);
            break;
        }
        case BUILTIN_RANGE_FULL: {
            first = first_slice_index(fs, V, NODE_START(index), NULL);
            second = second_slice_index(fs, V, NODE_START(index), NULL, object, 0);
            break;
        }
        case BUILTIN_RANGE_INCLUSIVE: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *lower = HirGetFieldExpr(K_LIST_FIRST(lit.items))->value;
            struct HirExpr *upper = HirGetFieldExpr(K_LIST_LAST(lit.items))->value;
            first = first_slice_index(fs, V, NODE_START(index), lower);
            second = second_slice_index(fs, V, NODE_START(index), upper, object, 1);
            break;
        }
        case BUILTIN_RANGE_TO_INCLUSIVE: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *upper = HirGetFieldExpr(K_LIST_LAST(lit.items))->value;
            first = first_slice_index(fs, V, NODE_START(index), NULL);
            second = second_slice_index(fs, V, NODE_START(index), upper, object, 1);
            break;
        }
        default:
            PAW_UNREACHABLE();
    }

    *plower = new_place(fs, get_type(L, BUILTIN_INT));
    *pupper = new_place(fs, get_type(L, BUILTIN_INT));
    move_to(fs, NODE_START(index), first, *plower);
    move_to(fs, NODE_START(index), second, *pupper);
}

static struct MirPlace lower_sequence_index(struct HirVisitor *V, struct HirIndex *e, struct MirPlace object)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *target_type = GET_NODE_TYPE(fs->C, e->target);
    IrType *index_type = GET_NODE_TYPE(fs->C, e->index);
    if (builtin_kind(L, index_type) == BUILTIN_INT) {
        struct MirPlace const index = new_place(fs, index_type);
        move_to(fs, NODE_START(e->index), lower_place(V, e->index), index);

        return select_element(fs, object, target_type, index.r);
    } else {
        struct MirPlace first, second;
        lower_range_index(V, e->index, object, &first, &second);

        return select_range(fs, object, target_type, first.r, second.r);
    }
}

static struct MirPlace lower_mapping_index(struct HirVisitor *V, struct HirIndex *e, struct MirPlace object)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *target_type = GET_NODE_TYPE(fs->C, e->target);
    IrType *index_type = GET_NODE_TYPE(fs->C, e->index);
    struct MirPlace const index = new_place(fs, index_type);
    move_to(fs, NODE_START(e->index), lower_place(V, e->index), index);

    return select_element(fs, object, target_type, index.r);
}

static paw_Bool visit_param_decl(struct HirVisitor *V, struct HirParamDecl *d)
{
    struct LowerHir *L = V->ud;
    IrType *type = pawIr_get_type(L->C, d->id);
    alloc_local(L->fs, d->ident, d->id, type);
    return PAW_FALSE;
}

static paw_Bool visit_let_stmt(struct HirVisitor *V, struct HirLetStmt *s)
{
    struct LowerHir *L = V->ud;
    IrType *type = pawIr_get_type(L->C, s->id);
    struct HirBindingPat const *p = HirGetBindingPat(s->pat);

    if (s->init != NULL) {
        struct MirPlace const init = lower_place(V, s->init);
        struct LocalVar const local = alloc_local(L->fs, p->ident, p->id, type);
        move_to(L->fs, p->span.start, init, local.r);
    } else {
        // create an uninitialized virtual register to hold the variable
        struct LocalVar const local = alloc_local(L->fs, p->ident, p->id, type);
        struct MirRegisterData *data = mir_reg_data(L->fs->mir, local.r.r);
        data->is_uninit = PAW_TRUE;
    }
    return PAW_FALSE;
}

static struct MirPlace lower_basic_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    return add_constant(fs, e->span.start, e->basic.value, e->basic.code);
}

static struct MirPlace lower_tuple_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *type = pawIr_get_type(fs->C, e->id);
    struct MirPlace const output = place_for_node(fs, e->id);
    NEW_INSTR(fs, aggregate, e->span.start, e->tuple.elems->count, output);

    int index;
    struct HirExpr **pexpr;
    K_LIST_ENUMERATE (e->tuple.elems, index, pexpr) {
        struct MirPlace place = select_field(fs, output, type, index, 0);
        move_to(fs, e->span.start, lower_place(V, *pexpr), place);
    }
    return output;
}

static struct MirPlace lower_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *type = pawIr_get_type(fs->C, e->id);
    struct MirPlace const output = place_for_node(fs, e->id);
    NEW_INSTR(fs, aggregate, e->span.start, e->comp.items->count, output);

    struct HirExpr **pexpr;
    K_LIST_FOREACH (e->comp.items, pexpr) {
        // fields are evaluated in lexical order because of possible side effects
        struct HirFieldExpr *field = HirGetFieldExpr(*pexpr);
        struct MirPlace place = select_field(fs, output, type, field->fid, 0);
        move_to(fs, e->span.start, lower_place(V, field->value), place);
    }

    return output;
}

static struct MirPlace lower_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *type = pawIr_get_type(fs->C, e->id);
    enum BuiltinKind b_kind = builtin_kind(L, type);
    struct MirPlace const output = place_for_node(fs, e->id);
    NEW_INSTR(fs, container, e->span.start, b_kind, e->cont.items->count, output);

    int index;
    struct HirExpr **pexpr;
    K_LIST_ENUMERATE (e->cont.items, index, pexpr) {
        struct MirPlace key, value;
        if (HirIsFieldExpr(*pexpr)) {
            struct HirFieldExpr *elem = HirGetFieldExpr(*pexpr);
            key = lower_place(V, elem->key);
            value = lower_place(V, elem->value);
        } else {
            key = add_constant(fs, e->span.start, I2V(index), BUILTIN_INT);
            value = lower_place(V, *pexpr);
        }

        struct MirPlace place = select_element(fs, output, type, key.r);
        move_to(fs, e->span.start, value, place);
    }
    return output;
}

static struct MirPlace lower_literal_expr(struct HirVisitor *V, struct HirLiteralExpr *e)
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

static struct MirPlace lower_logical_expr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirBlock const before_bb = current_bb(fs);
    MirBlock const test_bb = new_bb(fs);
    MirBlock const lhs_bb = new_bb(fs);
    MirBlock const rhs_bb = new_bb(fs);
    MirBlock const after_bb = new_bb(fs);
    add_edge(fs, before_bb, test_bb);
    terminate_goto(fs, e->span.start, test_bb);
    set_current_bb(fs, test_bb);

    struct MirPlace const result = new_literal_place(fs, BUILTIN_BOOL);
    struct MirPlace const first = lower_place(V, e->lhs);
    add_edge(fs, current_bb(fs), lhs_bb);
    add_edge(fs, current_bb(fs), rhs_bb);

    terminate_branch(fs, e->span.start, first, //
            e->is_and ? rhs_bb : lhs_bb, //
            e->is_and ? lhs_bb : rhs_bb);

    set_current_bb(fs, lhs_bb);
    move_to(fs, NODE_START(e->lhs), first, result);
    set_goto_edge(fs, e->span.start, after_bb);

    set_current_bb(fs, rhs_bb);
    struct MirPlace const second = lower_place(V, e->rhs);
    move_to(fs, NODE_START(e->rhs), second, result);
    set_goto_edge(fs, e->span.start, after_bb);

    set_current_bb(fs, after_bb);
    return result;
}

static struct MirPlace lower_unit_struct(struct HirVisitor *V, struct HirPathExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace const output = place_for_node(fs, e->id);
    NEW_INSTR(fs, aggregate, e->span.start, 0, output);
    return output;
}

static struct MirPlace lower_unit_variant(struct HirVisitor *V, struct HirPathExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *type = pawIr_get_type(fs->C, e->id);
    struct MirPlace const object = place_for_node(fs, e->id);
    NEW_INSTR(fs, aggregate, e->span.start, 1, object);

    struct MirPlace const discr = add_constant(fs, d->span.start, I2V(d->index), BUILTIN_INT);
    struct MirPlace place = select_field(fs, object, type, 0, 0);
    move_to(fs, e->span.start, discr, place);
    return object;
}

// Routine for lowering a global constant
static void lower_global_constant(struct LowerHir *L, struct HirConstDecl *d);

static struct MirPlace lookup_global_constant(struct LowerHir *L, struct HirConstDecl *d)
{
    int const *pid = GlobalMap_get(L, L->globals, d->did);
    if (pid != NULL) {
        struct FunctionState *fs = L->fs;
        struct GlobalInfo info = GlobalList_get(L->C->globals, *pid);
        return add_constant(fs, d->span.start, info.value, info.b_kind);
    }
    lower_global_constant(L, d);
    return lookup_global_constant(L, d);
}

static struct MirPlace lower_ascription_expr(struct HirVisitor *V, struct HirAscriptionExpr *e)
{
    return lower_place(V, e->expr);
}

static struct MirPlace lower_path_expr(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct NonGlobal ng;
    struct HirSegment const last = K_LIST_LAST(e->path.segments);
    if (resolve_nonglobal(fs, last.target, &ng))
        return pawMir_copy_place(fs->mir, ng.r);

    struct HirDecl *decl = pawHir_get_node(L->hir, last.target);
    struct MirPlace const output = place_for_node(fs, e->id);

    if (HirIsVariantDecl(decl)) {
        struct HirVariantDecl *v = HirGetVariantDecl(decl);
        struct HirDecl *base = pawHir_get_decl(L->hir, v->base_did);
        if (HirGetAdtDecl(base)->is_struct) {
            return lower_unit_struct(V, e, v);
        } else {
            return lower_unit_variant(V, e, v);
        }
    } else if (HirIsConstDecl(decl)) {
        return lookup_global_constant(L, HirGetConstDecl(decl));
    }
    NEW_INSTR(fs, global, e->span.start, output, -1);
    return output;
}

static void emit_get_field(struct FunctionState *fs, struct SourceLoc loc, IrType *type, struct MirPlace object, int index, int discr, struct MirPlace output)
{
    struct MirPlace const field = select_field(fs, object, type, index, discr);
    move_to(fs, loc, field, output);
}

static struct MirSwitchArmList *allocate_switch_arms(struct FunctionState *fs, MirBlock discr_bb, int count)
{
    struct MirSwitchArmList *arms = MirSwitchArmList_new(fs->mir);
    MirSwitchArmList_reserve(fs->mir, arms, count);
    for (int i = 0; i < count; ++i) {
        MirBlock const case_bb = new_bb(fs);
        add_edge(fs, discr_bb, case_bb);
        MirSwitchArmList_push(fs->mir, arms, (struct MirSwitchArm){
            .bid = case_bb,
        });
    }
    return arms;
}

static struct MirPlace option_chain_error(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace object, IrType *target)
{
    struct MirPlace place = new_place(fs, fs->result);
    struct IrLayout layout = pawIr_compute_layout(fs->C, fs->result);
    NEW_INSTR(fs, aggregate, loc, layout.size, place);

    struct MirPlace const k = add_constant(fs, loc, I2V(PAW_OPTION_NONE), BUILTIN_INT);
    struct MirPlace discr = select_field(fs, place, fs->result, 0, PAW_OPTION_NONE);
    move_to(fs, loc, k, discr);
    return place;
}

static struct MirPlace result_chain_error(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace object, IrType *target)
{
    struct MirPlace place = new_place(fs, fs->result);
    struct IrLayout layout = pawIr_compute_layout(fs->C, fs->result);
    NEW_INSTR(fs, aggregate, loc, layout.size, place);

    struct MirPlace const k = add_constant(fs, loc, I2V(PAW_OPTION_NONE), BUILTIN_INT);
    struct MirPlace discr = select_field(fs, place, fs->result, 0, PAW_OPTION_NONE);
    move_to(fs, loc, k, discr);

    struct MirPlace e = select_field(fs, object, fs->result, 1, PAW_OPTION_NONE);
    struct MirPlace error = select_field(fs, place, fs->result, 1, PAW_OPTION_NONE);
    move_to(fs, loc, e, error);
    return place;
}

// Transformation:
//     opt?  =>  match opt {Some(x) => x, None => return None}
//     opt?  =>  match opt {Ok(x) => x, Err(e) => return Err(e)}
//
static struct MirPlace lower_chain_expr(struct HirVisitor *V, struct HirChainExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *target = GET_NODE_TYPE(fs->C, e->target);
    enum BuiltinKind const kind = builtin_kind(L, target);

    MirBlock const input_bb = current_bb(fs);
    MirBlock const none_bb = new_bb(fs);
    MirBlock const after_bb = new_bb(fs);
    add_edge(fs, input_bb, none_bb);

    struct MirPlace const object = lower_place(V, e->target);
    struct MirPlace const discr = new_literal_place(fs, BUILTIN_INT);
    emit_get_field(fs, e->span.start, target, object, 0, PAW_OPTION_NONE, discr);

    struct MirSwitchArmList *arms = allocate_switch_arms(fs, input_bb, 1);
    struct MirInstruction *switch_ = terminate_switch(fs, e->span.start, discr, arms, none_bb);
    struct MirSwitchArm *arm = &K_LIST_AT(arms, 0);

    set_current_bb(fs, arm->bid);
    struct MirPlace const value = place_for_node(fs, e->id);
    emit_get_field(fs, e->span.start, target, object, 1, PAW_OPTION_SOME, value);
    set_goto_edge(fs, e->span.start, after_bb);

    set_current_bb(fs, none_bb);
    struct MirPlace const error = kind == BUILTIN_OPTION
        ? option_chain_error(fs, e->span.start, object, target)
        : result_chain_error(fs, e->span.start, object, target);
    terminate_return(fs, e->span.start, error);

    set_current_bb(fs, after_bb);
    return value;
}

static enum MirUnaryOpKind unop2op_bool(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_NOT:
            return MIR_UNARY_NOT;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirUnaryOpKind unop2op_int(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_NEG:
            return MIR_UNARY_INEG;
        case UNARY_BNOT:
            return MIR_UNARY_IBITNOT;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirBinaryOpKind binop2op_bool(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return MIR_BINARY_IEQ;
        case BINARY_NE:
            return MIR_BINARY_INE;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirBinaryOpKind binop2op_char(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return MIR_BINARY_CEQ;
        case BINARY_NE:
            return MIR_BINARY_CNE;
        case BINARY_LT:
            return MIR_BINARY_CLT;
        case BINARY_LE:
            return MIR_BINARY_CLE;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirBinaryOpKind binop2op_int(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return MIR_BINARY_IEQ;
        case BINARY_NE:
            return MIR_BINARY_INE;
        case BINARY_LT:
            return MIR_BINARY_ILT;
        case BINARY_LE:
            return MIR_BINARY_ILE;
        case BINARY_ADD:
            return MIR_BINARY_IADD;
        case BINARY_SUB:
            return MIR_BINARY_ISUB;
        case BINARY_MUL:
            return MIR_BINARY_IMUL;
        case BINARY_DIV:
            return MIR_BINARY_IDIV;
        case BINARY_MOD:
            return MIR_BINARY_IMOD;
        case BINARY_BAND:
            return MIR_BINARY_IBITAND;
        case BINARY_BOR:
            return MIR_BINARY_IBITOR;
        case BINARY_BXOR:
            return MIR_BINARY_IBITXOR;
        case BINARY_SHL:
            return MIR_BINARY_ISHL;
        case BINARY_SHR:
            return MIR_BINARY_ISHR;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirUnaryOpKind unop2op_float(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_NEG:
            return MIR_UNARY_FNEG;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirBinaryOpKind binop2op_float(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return MIR_BINARY_FEQ;
        case BINARY_NE:
            return MIR_BINARY_FNE;
        case BINARY_LT:
            return MIR_BINARY_FLT;
        case BINARY_LE:
            return MIR_BINARY_FLE;
        case BINARY_ADD:
            return MIR_BINARY_FADD;
        case BINARY_SUB:
            return MIR_BINARY_FSUB;
        case BINARY_MUL:
            return MIR_BINARY_FMUL;
        case BINARY_DIV:
            return MIR_BINARY_FDIV;
        case BINARY_MOD:
            return MIR_BINARY_FMOD;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirUnaryOpKind unop2op_str(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return MIR_UNARY_STRLEN;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirBinaryOpKind binop2op_str(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return MIR_BINARY_STREQ;
        case BINARY_NE:
            return MIR_BINARY_STRNE;
        case BINARY_LT:
            return MIR_BINARY_STRLT;
        case BINARY_LE:
            return MIR_BINARY_STRLE;
        case BINARY_ADD:
            return MIR_BINARY_STRCAT;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirUnaryOpKind unop2op_list(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return MIR_UNARY_LISTLEN;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirBinaryOpKind binop2op_list(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_ADD:
            return MIR_BINARY_LISTCAT;
        default:
            PAW_UNREACHABLE();
    }
}

static enum MirUnaryOpKind unop2op_map(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return MIR_UNARY_MAPLEN;
        default:
            PAW_UNREACHABLE();
    }
}

static struct MirPlace lower_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace value = lower_place(V, e->target);
    struct MirPlace const output = place_for_node(fs, e->id);
    const enum BuiltinKind kind = kind_of_builtin(L, e->target);
    if (!IS_BUILTIN_TYPE(kind)) return output; // must be "!"

    enum MirUnaryOpKind const op = kind == BUILTIN_BOOL ? unop2op_bool(e->op) : //
        kind == BUILTIN_INT ? unop2op_int(e->op) : //
        kind == BUILTIN_FLOAT ? unop2op_float(e->op) : //
        kind == BUILTIN_STR ? unop2op_str(e->op) : //
        kind == BUILTIN_LIST ? unop2op_list(e->op) : //
        unop2op_map(e->op);
    NEW_INSTR(fs, unary_op, e->span.start, op, value, output);
    return output;
}

static void new_binary_op(struct HirVisitor *V, struct SourceSpan span, enum BinaryOp op, enum BuiltinKind kind, struct MirPlace lhs, struct MirPlace rhs, struct MirPlace output)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    if (op == BINARY_GT || op == BINARY_GE) {
        // only use relational comparisons "LT" and "LE"
        op = op == BINARY_GT ? BINARY_LT : BINARY_LE;
        struct MirPlace const temp = lhs;
        lhs = rhs;
        rhs = temp;
    }

    enum MirBinaryOpKind const binop =
        kind == BUILTIN_CHAR ? binop2op_char(op) : //
        kind == BUILTIN_INT ? binop2op_int(op) : //
        kind == BUILTIN_BOOL ? binop2op_bool(op) : //
        kind == BUILTIN_FLOAT ? binop2op_float(op) : //
        kind == BUILTIN_STR ? binop2op_str(op) : //
        binop2op_list(op);

    NEW_INSTR(fs, binary_op, span.start, binop, lhs, rhs, output);
}

static struct MirPlace lower_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace const lhs = lower_place(V, e->lhs);
    struct MirPlace const rhs = lower_place(V, e->rhs);
    const enum BuiltinKind kind = kind_of_builtin(L, e->lhs);
    struct MirPlace const output = place_for_node(fs, e->id);
    if (!IS_BUILTIN_TYPE(kind)) return output; // must be "!"

    new_binary_op(V, e->span, e->op, kind, lhs, rhs, output);
    return output;
}

static void lower_function_block(struct LowerHir *L, struct HirExpr *block)
{
    struct FunctionState *fs = L->fs;
    struct MirPlace const result = lower_place(L->V, block);
    struct MirRegisterData const *data = mir_reg_data(fs->mir, result.r);
    terminate_return(fs, fs->mir->span.end, result);
}

static struct MirPlace lower_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *outer = L->fs;
    IrType *type = pawIr_get_type(L->C, e->id);

    Str *name = SCAN_STR(L->C, PRIVATE("closure"));
    struct Mir *result = pawMir_new(L->C, L->pm->name, e->span, name, type, NULL,
            FUNC_CLOSURE, PAW_FALSE, PAW_FALSE);

    struct BlockState bs;
    struct FunctionState fs;
    MirBlock const entry = enter_function(L, &fs, &bs, result);
    MirBlock const first = new_bb(&fs);

    pawHir_visit_decl_list(L->V, e->params);
    terminate_goto(&fs, e->span.start, first);
    add_edge(&fs, entry, first);
    set_current_bb(&fs, first);

    if (HirIsBlock(e->expr)) {
        lower_function_block(L, e->expr);
    } else {
        // evaluate and return the expression
        struct MirPlace const result = lower_place(V, e->expr);
        terminate_return(&fs, e->span.end, result);
    }
    result->upvalues = fs.up;
    leave_function(L);

    postprocess(result);

    struct MirPlace const output = new_place(L->fs, type);
    struct MirBodyList *children = outer->mir->children;
    NEW_INSTR(outer, closure, e->span.start, children->count, output);
    MirBodyList_push(outer->mir, children, result);
    return output;
}

static struct MirPlace lower_conversion_expr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    static int const REQUIRED_CASTS[NBUILTIN_SCALARS][NBUILTIN_SCALARS] = {
        //          to  = {0, b, x, i, f}
        [BUILTIN_BOOL]  = {0, 0, 0, 0, 1},
        [BUILTIN_CHAR]  = {0, 1, 0, 1, 0},
        [BUILTIN_INT]   = {0, 1, 1, 0, 1},
        [BUILTIN_FLOAT] = {0, 1, 0, 1, 0},
    };

    enum BuiltinKind from = kind_of_builtin(L, e->arg);
    struct MirPlace const output = place_for_node(fs, e->id);
    struct MirPlace const target = lower_place(V, e->arg);
    if (REQUIRED_CASTS[from][e->to]) {
        NEW_INSTR(fs, cast, e->span.start, target, output, from, e->to);
    } else {
        move_to(fs, e->span.start, target, output);
    }
    return output;
}

static struct MirPlace lower_variant_constructor(struct HirVisitor *V, struct HirCallExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *type = pawIr_get_type(fs->C, e->id);
    struct MirPlace const object = place_for_node(fs, e->id);
    struct IrLayout layout = pawMir_get_layout(fs->mir, object.r);
    NEW_INSTR(fs, aggregate, e->span.start, layout.size, object);

    // set the discriminant: an 'int' residing in the first Value slot of the variant
    struct MirPlace const discr = add_constant(fs, d->span.start, I2V(d->index), BUILTIN_INT);
    struct MirPlace place = select_field(fs, object, type, 0, d->index);
    move_to(fs, e->span.start, discr, place);

    int index;
    struct HirExpr **pexpr;
    K_LIST_ENUMERATE (e->args, index, pexpr) {
        struct MirPlace const field = lower_place(V, *pexpr);
        struct MirPlace place = select_field(fs, object, type, 1 + index, d->index);
        move_to(fs, e->span.start, field, place);
    }

    return object;
}

static void lower_args(struct HirVisitor *V, struct HirExprList *exprs, struct MirPlaceList *result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct HirExpr **pexpr;
    K_LIST_FOREACH (exprs, pexpr) {
        IrType *type = GET_NODE_TYPE(fs->C, *pexpr);
        struct MirPlace const copy = new_place(fs, type);
        struct MirPlace const arg = lower_place(V, *pexpr);
        move_to(fs, NODE_START(*pexpr), arg, copy);
        MirPlaceList_push(L->fs->mir, result, copy);
    }
}

// TODO: what about function object stored in ADT field (not a method)?
static struct MirPlace lower_callee_and_args(struct HirVisitor *V, struct HirExpr *callee, struct HirExprList *args_in, MirPlaceList *args_out)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace result;
    if (HirIsSelector(callee) && !HirGetSelector(callee)->is_index) {
        result = place_for_node(fs, callee->hdr.id);
        struct HirSelector *select = HirGetSelector(callee);
        NEW_INSTR(fs, global, callee->hdr.span.start, result, -1);

        // add context argument for method call
        struct MirPlace const self = lower_place(V, select->target);
        MirPlaceList_push(fs->mir, args_out, self);
    } else {
        result = lower_place(V, callee);
    }

    lower_args(V, args_in, args_out);
    return result;
}

static struct MirPlace lower_call_expr(struct HirVisitor *V, struct HirCallExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    IrType *target_type = GET_NODE_TYPE(L->C, e->target);
    IrType *return_type = IR_FPTR(target_type)->result;
    if (IrIsSignature(target_type)) {
        struct HirDecl *decl = pawHir_get_decl(L->hir, IR_TYPE_DID(target_type));
        if (HirIsVariantDecl(decl))
            return lower_variant_constructor(V, e, HirGetVariantDecl(decl));
    }

    MirPlaceList *args = MirPlaceList_new(fs->mir);
    struct MirPlace const target = lower_callee_and_args(V, e->target, e->args, args);
    MirPlaceList *results = MirPlaceList_new(fs->mir);
    MirPlaceList_push(fs->mir, results, place_for_node(fs, e->id));
    NEW_INSTR(fs, call, e->span.start, target, args, results);
    if (IrIsNever(return_type)) {
        // this function never returns
        terminate_unreachable(fs, e->span.start);
        set_current_bb(fs, new_bb(fs));
    }
    return K_LIST_FIRST(results);
}

static struct MirPlace lower_field_expr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    struct LowerHir *L = V->ud;
    if (e->fid < 0)
        lower_place(V, e->key);
    return lower_place(V, e->value);
}

static struct MirPlace lower_assign_expr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace lhs = lower_place(V, e->lhs);
    struct MirPlace rhs = lower_place(V, e->rhs);
    move_to(fs, e->span.start, rhs, lhs);

    // setters are expressions that evaluate to "()"
    return unit_literal(fs, e->span.start);
}

static struct MirPlace lower_op_assign_expr(struct HirVisitor *V, struct HirOpAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace lhs = lower_place(V, e->lhs);
    struct MirPlace rhs = lower_place(V, e->rhs);
    const enum BuiltinKind kind = kind_of_builtin(L, e->lhs);
    struct MirPlace const temp = place_for_node(fs, e->id);
    new_binary_op(V, e->span, e->op, kind, lhs, rhs, temp);
    move_to(fs, e->span.start, temp, lhs);

    // setters are expressions that evaluate to "()"
    return unit_literal(fs, e->span.start);
}

static struct MirPlace lower_block(struct HirVisitor *V, struct HirBlock *e)
{
    struct BlockState bs;
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    enter_block(fs, &bs, e->span, PAW_FALSE);
    pawHir_visit_stmt_list(V, e->stmts);
    struct MirPlace const result = e->result != NULL
        ? lower_place(V, e->result)
        : unit_literal(fs, e->span.start);

    leave_block(fs);
    return result;
}

static struct MirPlace lower_loop_expr(struct HirVisitor *V, struct HirLoopExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct MirPlace const result = unit_literal(fs, e->span.start);
    MirBlock const before_bb = current_bb(fs);
    MirBlock const header_bb = new_bb(fs);
    MirBlock const after_bb = new_bb(fs);
    add_edge(fs, before_bb, header_bb);
    terminate_goto(fs, e->span.start, header_bb);

    struct BlockState bs;
    enter_block(fs, &bs, e->span, PAW_TRUE);

    set_current_bb(fs, header_bb);
    lower_place(V, e->block);

    set_goto_edge(fs, e->span.start, header_bb);
    adjust_to(fs, JUMP_CONTINUE, header_bb);
    set_current_bb(fs, after_bb);

    leave_block(fs);
    return result;
}

static struct MirPlace lower_jump_expr(struct HirVisitor *V, struct HirJumpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    add_label(fs, e->span.start, e->jump_kind);
    set_current_bb(fs, new_bb(fs));
    return unit_literal(fs, e->span.start);
}

static struct MirPlace lower_return_expr(struct HirVisitor *V, struct HirReturnExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    terminate_return(fs, e->span.start, e->expr != NULL
            ? lower_place(V, e->expr)
            : PLACE(MIR_INVALID_REG)); // return "()"

    MirBlock const next_bb = new_bb(fs);
    set_current_bb(fs, next_bb);
    return unit_literal(fs, e->span.start);
}

static struct MirPlace lower_place(struct HirVisitor *V, struct HirExpr *expr)
{
    struct LowerHir *L = V->ud;
    struct MirPlace place = {
        .projection = MirProjectionList_new(L->fs->mir),
    };
    lower_place_into(V, expr, &place);
    return place;
}

static paw_Bool visit_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    lower_place(V, s->expr);
    return PAW_FALSE;
}

static struct MirPlace get_test_reg(struct FunctionState *fs, struct MatchVar v)
{
    struct MirPlace const *pr = VarPlaces_get(fs->L, fs->ms->var_mapping, v);
    paw_assert(pr != NULL);
    return pawMir_copy_place(fs->mir, *pr);
}

static void declare_match_bindings(struct FunctionState *fs, struct BindingList *bindings)
{
    struct Binding const *pb;
    K_LIST_FOREACH(bindings, pb) {
        struct MirPlace const test = get_test_reg(fs, pb->var);
        struct HirIdent const ident = {
            .span = pb->var.span,
            .name = pb->name,
        };

        struct LocalVar local = alloc_local(fs, ident, pb->id, pb->var.type);
        move_to(fs, ident.span.start, test, local.r);
    }
}

static MirBlock lower_match_body(struct HirVisitor *V, struct MatchBody body, struct MirPlace result);
static void visit_decision(struct HirVisitor *V, struct Decision *d, struct MirPlace result);

static void visit_success(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    lower_match_body(V, d->success.body, result);
}

static void visit_guard(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    // steal bindings from the body of the guard, since they may be referenced in
    // the conditional expression
    struct BindingList *bindings = d->guard.body.bindings;
    declare_match_bindings(fs, bindings);
    bindings->count = 0;

    struct MirPlace const cond = lower_place(V, d->guard.cond);
    MirBlock const before_bb = current_bb(fs);
    MirBlock const then_bb = new_bb(fs);
    MirBlock const else_bb = new_bb(fs);
    MirBlock const join_bb = new_bb(fs);
    add_edge(fs, before_bb, then_bb);
    add_edge(fs, before_bb, else_bb);

    struct SourceLoc loc = NODE_START(d->guard.cond);
    struct MirInstruction *branch = terminate_branch(fs, loc, cond, then_bb, else_bb);
    set_current_bb(fs, then_bb);
    lower_match_body(V, d->guard.body, result);
    set_goto_edge(fs, loc, join_bb);

    set_current_bb(fs, else_bb);
    visit_decision(V, d->guard.rest, result);
    set_goto_edge(fs, loc, join_bb);

    set_current_bb(fs, join_bb);
}

static MirBlock lower_match_body(struct HirVisitor *V, struct MatchBody body, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    MirBlock const bid = current_bb(fs);
    declare_match_bindings(fs, body.bindings);
    struct MirPlace const r = lower_place(V, body.result);
    move_to(fs, NODE_START(body.result), r, result);
    return bid;
}

static void map_var_to_reg(struct FunctionState *fs, struct MatchVar var, struct MirPlace r)
{
    VarPlaces_insert(fs->L, fs->ms->var_mapping, var, r);
}

static void allocate_match_vars(struct FunctionState *fs, struct MirPlace object, struct MatchCase mc, paw_Bool is_enum, int discr)
{
    if (mc.vars->count == 0)
        return;

    IrType *const object_type = mir_reg_data(fs->mir, object.r)->type;

    int index;
    struct MatchVar const *pv;
    K_LIST_ENUMERATE (mc.vars, index, pv) {
        Str *const name = SCAN_STR(fs->C, PRIVATE("variable"));
        struct HirIdent const ident = {.name = name, .span = pv->span};
//        struct LocalVar const local = alloc_local(fs, ident, pv->type);
        struct MirPlace const place = new_place(fs, pv->type);
        map_var_to_reg(fs, *pv, place);

        struct MirPlace source = select_field(fs, object, object_type, is_enum + index, discr);
        move_to(fs, pv->span.start, source, place);
    }
}

static void visit_sparse_cases(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct CaseList *cases = d->multi.cases;
    MirBlock const discr_bb = current_bb(fs);
    MirBlock const otherwise_bb = new_bb(fs);
    MirBlock const join_bb = new_bb(fs);
    add_edge(fs, discr_bb, otherwise_bb);

    struct SourceLoc loc = d->multi.test.span.start;
    struct MirPlace const test = get_test_reg(fs, d->multi.test);
    struct MirSwitchArmList *arms = allocate_switch_arms(fs, discr_bb, cases->count);
    struct MirInstruction *switch_ = terminate_switch(fs, loc, test, arms, otherwise_bb);

    struct MatchCase const *pmc;
    struct MirSwitchArm *parm;
    K_LIST_ZIP (cases, pmc, arms, parm) {
        set_current_bb(fs, parm->bid);
        parm->value = pmc->cons.value;

        visit_decision(V, pmc->dec, result);
        set_goto_edge(fs, loc, join_bb);
    }

    // this implementation requires an "otherwise" case (binding or wildcard) to ensure
    // exhaustivness
    paw_assert(d->multi.rest != NULL);
    set_current_bb(fs, otherwise_bb);
    visit_decision(V, d->multi.rest, result);
    set_goto_edge(fs, loc, join_bb);

    set_current_bb(fs, join_bb);
}

static void visit_variant_cases(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct CaseList *cases = d->multi.cases;

    MirBlock const discr_bb = current_bb(fs);
    MirBlock const join_bb = new_bb(fs);
    struct SourceLoc loc = d->multi.test.span.start;
    struct MirPlace const variant = get_test_reg(fs, d->multi.test);
    struct MirPlace const test = new_literal_place(fs, BUILTIN_INT);

    emit_get_field(fs, loc, d->multi.test.type, variant, 0, 0, test);

    struct MirSwitchArmList *arms = allocate_switch_arms(fs, discr_bb, cases->count);
    struct MirInstruction *switch_ = terminate_switch(fs, loc, test, arms, MIR_INVALID_BB);

    struct MatchCase const *pmc;
    struct MirSwitchArm *parm;
    K_LIST_ZIP (cases, pmc, arms, parm) {
        parm->value.i = pmc->cons.variant.index;
        set_current_bb(fs, parm->bid);

        allocate_match_vars(fs, variant, *pmc, PAW_TRUE, pmc->cons.variant.index);
        visit_decision(V, pmc->dec, result);
        set_goto_edge(fs, loc, join_bb);
    }
    paw_assert(d->multi.rest == NULL);

    set_current_bb(fs, join_bb);
}

static void visit_tuple_case(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct MirPlace const discr = get_test_reg(fs, d->multi.test);

    paw_assert(d->multi.rest == NULL);
    paw_assert(d->multi.cases->count == 1);
    struct MatchCase mc = K_LIST_FIRST(d->multi.cases);

    allocate_match_vars(fs, discr, mc, PAW_FALSE, 0);
    visit_decision(V, mc.dec, result);
}

static void visit_struct_case(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct MirPlace const discr = get_test_reg(fs, d->multi.test);

    paw_assert(d->multi.rest == NULL);
    paw_assert(d->multi.cases->count == 1);
    struct MatchCase mc = K_LIST_FIRST(d->multi.cases);

    allocate_match_vars(fs, discr, mc, PAW_FALSE, 0);
    visit_decision(V, mc.dec, result);
}

static void visit_multiway(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;

    // there must exist at least 1 case; all cases have the same kind of constructor
    struct MatchCase first_case = K_LIST_FIRST(d->multi.cases);
    switch (first_case.cons.kind) {
        case CONS_WILDCARD:
            break;
        case CONS_BOOL:
        case CONS_CHAR:
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

static void visit_decision(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

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
}

static struct MirPlace lower_match_expr(struct HirVisitor *V, struct HirMatchExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    VarPlaces *var_mapping = VarPlaces_new(L);

    struct MatchState ms;
    enter_match(fs, &ms, var_mapping);

    struct Decision *d = pawP_check_exhaustiveness(L->hir, L->pool, L->pm->name, e, ms.vars);
    paw_assert(ms.vars->count > 0);

    struct MirPlace const target = lower_place(V, e->target);
    struct MirPlace const discr = place_for_node(fs, e->target->hdr.id);
    struct MirPlace const result = place_for_node(fs, e->id);
    move_to(fs, NODE_START(e->target), target, discr);
    map_var_to_reg(fs, K_LIST_FIRST(ms.vars), discr);

    visit_decision(V, d, result);

    leave_match(fs);
    VarPlaces_delete(L, var_mapping);
    return result;
}

// Lower an expression representing a location in memory
// Note that nested selectors on value types are flattened into a single access relative
// to the start of the object (the whole object is stored in a single virtual register).
static void lower_place_into(struct HirVisitor *V, struct HirExpr *expr, struct MirPlace *pplace)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    switch (HIR_KINDOF(expr)) {
        case kHirSelector: {
            struct HirSelector *x = HirGetSelector(expr);
            lower_place_into(V, x->target, pplace);

            IrType *target = GET_NODE_TYPE(fs->C, x->target);
            *pplace = select_field(fs, *pplace, target, x->index, 0);
            break;
        }
        case kHirIndex: {
            struct HirIndex *x = HirGetIndex(expr);
            lower_place_into(V, x->target, pplace);

            IrType *target = GET_NODE_TYPE(fs->C, x->target);
            if (builtin_kind(L, target) == BUILTIN_MAP) {
                *pplace = lower_mapping_index(V, x, *pplace);
            } else {
                *pplace = lower_sequence_index(V, x, *pplace);
            }
            break;
        }
        case kHirAscriptionExpr:
            *pplace = lower_ascription_expr(V, HirGetAscriptionExpr(expr));
            break;
        case kHirPathExpr:
            *pplace = lower_path_expr(V, HirGetPathExpr(expr));
            break;
        case kHirLiteralExpr:
            *pplace = lower_literal_expr(V, HirGetLiteralExpr(expr));
            break;
        case kHirLogicalExpr:
            *pplace = lower_logical_expr(V, HirGetLogicalExpr(expr));
            break;
        case kHirChainExpr:
            *pplace = lower_chain_expr(V, HirGetChainExpr(expr));
            break;
        case kHirUnOpExpr:
            *pplace = lower_unop_expr(V, HirGetUnOpExpr(expr));
            break;
        case kHirBinOpExpr:
            *pplace = lower_binop_expr(V, HirGetBinOpExpr(expr));
            break;
        case kHirClosureExpr:
            *pplace = lower_closure_expr(V, HirGetClosureExpr(expr));
            break;
        case kHirConversionExpr:
            *pplace = lower_conversion_expr(V, HirGetConversionExpr(expr));
            break;
        case kHirCallExpr:
            *pplace = lower_call_expr(V, HirGetCallExpr(expr));
            break;
        case kHirFieldExpr:
            *pplace = lower_field_expr(V, HirGetFieldExpr(expr));
            break;
        case kHirAssignExpr:
            *pplace = lower_assign_expr(V, HirGetAssignExpr(expr));
            break;
        case kHirOpAssignExpr:
            *pplace = lower_op_assign_expr(V, HirGetOpAssignExpr(expr));
            break;
        case kHirReturnExpr:
            *pplace = lower_return_expr(V, HirGetReturnExpr(expr));
            break;
        case kHirJumpExpr:
            *pplace = lower_jump_expr(V, HirGetJumpExpr(expr));
            break;
        case kHirLoopExpr:
            *pplace = lower_loop_expr(V, HirGetLoopExpr(expr));
            break;
        case kHirMatchExpr:
            *pplace = lower_match_expr(V, HirGetMatchExpr(expr));
            break;
        case kHirBlock:
            *pplace = lower_block(V, HirGetBlock(expr));
            break;
        default:
            PAW_UNREACHABLE();
    }
}

static void lower_hir_body_aux(struct LowerHir *L, struct HirFnDecl *fn, struct Mir *mir)
{
    struct BlockState bs;
    struct FunctionState fs;
    MirBlock const entry = enter_function(L, &fs, &bs, mir);
    MirBlock const first = new_bb(&fs);

    pawHir_visit_decl_list(L->V, fn->params);
    terminate_goto(&fs, fn->span.start, first);
    add_edge(&fs, entry, first);
    set_current_bb(&fs, first);

    lower_function_block(L, fn->body);

    leave_function(L);
}

static struct Mir *lower_hir_body(struct LowerHir *L, struct HirFnDecl *fn)
{
    IrType *type = pawIr_get_type(L->C, fn->id);
    struct IrSignature *fsig = IrGetSignature(type);
    paw_Bool const is_polymorphic = fn->generics != NULL
            || (fsig->self != NULL && IR_TYPE_SUBTYPES(fsig->self) != NULL);
    struct Mir *result = pawMir_new(L->C, L->pm->name, fn->span, fn->ident.name, type, fsig->self,
            fn->fn_kind, fn->is_pub, is_polymorphic);
    if (fn->body == NULL)
        return result;

    lower_hir_body_aux(L, fn, result);
    postprocess(result);

    if (pawP_push_callback(L->C, "paw.on_build_mir")) {
        paw_Env *P = ENV(L);
        paw_push_rawptr(P, result);
        paw_call(P, 1);
    }

    return result;
}

static void register_global_constant(struct LowerHir *L, struct HirConstDecl *d, Value value, enum BuiltinKind b_kind)
{
    const int global_id = L->globals->count;
    GlobalMap_insert(L, L->globals, d->did, global_id);
    GlobalList_push(L->C, L->C->globals, (struct GlobalInfo){
                                             .name = d->ident.name,
                                             .modno = d->did.modno,
                                             .index = global_id,
                                             .b_kind = b_kind,
                                             .value = value,
                                         });
}

static void lower_global_constant(struct LowerHir *L, struct HirConstDecl *d)
{
    // Make sure this constant hasn't already been lowered. Note that this is okay to do before
    // the cycle detection logic. If a constant has been fully evaluated already, then it must
    // not be part of a cycle. Evaluating any constant participating in a cycle will cause the
    // other constants in the cycle to be evaluated immediately, which will cause the call to
    // enter_constant_ctx to fail below.
    if (GlobalMap_get(L, L->globals, d->did) != NULL)
        return;

    // Enter module where the constant is defined. Module context must be restored before this
    // function returns.
    struct HirModule *outer = L->pm;
    L->pm = &K_LIST_AT(L->hir->modules, d->did.modno);

    // attempt to find the constant in the pre-registered symbol table
    struct Annotation anno;
    if (pawP_check_extern(L->C, d->annos, &anno)) {
        if (d->init != NULL)
            LOWERING_ERROR(L, initialized_extern_constant, d->span.start, d->ident.name->text);

        struct HirModule m = HirModuleList_get(L->hir->modules, d->did.modno);
        Str *modname = d->did.modno == TARGET_MODNO ? NULL : m.name;
        Str *name = pawP_mangle_name(L->C, modname, d->ident.name, NULL);
        Value const *pvalue = pawP_get_extern_value(L->C, name);
        if (pvalue == NULL)
            LOWERING_ERROR(L, missing_extern_value, d->span.start, d->ident.name->text);

        IrType *type = pawIr_get_type(L->C, d->id);
        enum BuiltinKind const kind = builtin_kind(L, type);
        register_global_constant(L, d, *pvalue, kind);
        L->pm = outer; // restore context
        return;
    }
    if (d->init == NULL)
        LOWERING_ERROR(L, uninitialized_constant, d->span.start, d->ident.name->text);

    // artificial MIR body so that toplevel constants can be lowered normally, i.e. using
    // "lower_place" routine
    IrTypeList *artificial_params = IrTypeList_new(L->C);
    IrType *artificial_result = pawP_builtin_type(L->C, BUILTIN_UNIT);
    IrType *artificial_type = pawIr_new_fn_ptr(L->C, artificial_params, artificial_result);
    struct Mir *artificial = pawMir_new(L->C, L->pm->name, d->span, SCAN_STR(L->C, PRIVATE("toplevel")), artificial_type,
                                        NULL, FUNC_MODULE, PAW_FALSE, PAW_FALSE);

    // prevent cycles between global constants
    struct ConstantContext cctx;
    enter_constant_ctx(L, &cctx, d->span, d->did);

    struct BlockState bs;
    struct FunctionState fs;
    MirBlock const entry = enter_function(L, &fs, &bs, artificial);
    MirBlock const first = new_bb(&fs);
    terminate_goto(&fs, d->span.start, first);
    add_edge(&fs, entry, first);
    set_current_bb(&fs, first);

    struct MirPlace const result = lower_place(L->V, d->init);
    terminate_return(&fs, d->span.start, result); // use variable to avoid DCE

    leave_constant_ctx(L);
    leave_function(L);

    // Perform constant folding (and maybe propagation) on the initializer expression. The
    // goal is to transform it into a single literal, which should always be possible, due
    // to the constantness checks performed in an earlier compilation phase.
    postprocess(artificial);
    pawSsa_construct(artificial);
    pawMir_propagate_constants(artificial);

    // after SCCP and DCE, the constant result will be in register 1
    paw_assert(artificial->blocks->count == 2); // entry and first block
    struct MirBlockData *bb = MirBlockDataList_get(artificial->blocks, 1);
    paw_assert(bb->instructions->count == 2); // Constant, and, Return
    struct MirLoadConstant *k = MirGetLoadConstant(K_LIST_FIRST(bb->instructions));

    register_global_constant(L, d, k->value, k->b_kind);

    pawMir_free(artificial);
    L->pm = outer;
}

static void lower_global_constants(struct LowerHir *L)
{
    // resolve constants, making sure there are no cyclic dependencies
    HirDeclMapIterator iter;
    HirDeclMapIterator_init(L->hir->decls, &iter);
    while (HirDeclMapIterator_is_valid(&iter)) {
        struct HirDecl *decl = *HirDeclMapIterator_valuep(&iter);
        if (HirIsConstDecl(decl))
            lower_global_constant(L, HirGetConstDecl(decl));
        HirDeclMapIterator_next(&iter);
    }
}

void pawP_lower_hir(struct Compiler *C)
{
    BodyMap *result = BodyMap_new(C);

    struct LowerHir L = {
        .V = &(struct HirVisitor){0},
        .pool = pawP_pool_new(C, C->aux_stats),
        .hir = C->hir,
        .P = ENV(C),
        .C = C,
    };
    L.locals = LocalMap_new(&L);
    L.globals = GlobalMap_new(&L);
    L.labels = LabelList_new(&L);
    L.stack = VarStack_new(&L);

    pawHir_visitor_init(L.V, L.hir, &L);
    L.V->VisitParamDecl = visit_param_decl;
    L.V->VisitLetStmt = visit_let_stmt;
    L.V->VisitExprStmt = visit_expr_stmt;

    lower_global_constants(&L);

    HirDeclMapIterator iter;
    HirDeclMapIterator_init(L.hir->decls, &iter);
    while (HirDeclMapIterator_is_valid(&iter)) {
        struct HirDecl *decl = *HirDeclMapIterator_valuep(&iter);
        if (HirIsFnDecl(decl)) {
            IrType *type = GET_NODE_TYPE(C, decl);
            struct IrSignature *fsig = IrGetSignature(type);
            if (fsig->self == NULL || IrIsAdt(fsig->self)) {
                struct HirFnDecl *d = HirGetFnDecl(decl);
                L.pm = &K_LIST_AT(L.hir->modules, d->did.modno);
                struct Mir *r = lower_hir_body(&L, d);
                BodyMap_insert(C, result, d->did, r);
            }
        }
        HirDeclMapIterator_next(&iter);
    }

    pawP_pool_free(C, L.pool);
    C->bodies = result;
}

