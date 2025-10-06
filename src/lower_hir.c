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
#define LOCAL(Reg_) ((struct MirPlace){.r = Reg_})

#define TODO (struct SourceLoc){0}

struct MatchResult {
    BindingList *bindings;
    MirBlock b;
};

struct MatchState {
    struct VarPlaces *places;
    struct MatchResults *results;
    struct MatchState *outer;
    struct MatchVars *vars;
};

struct FunctionState {
    struct FunctionState *outer;
    struct MirConstantDataList *constants;
    struct MirRegisterDataList *registers;
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
DEFINE_MAP(struct LowerHir, MatchResults, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, struct MatchResult)

static void postprocess(struct Mir *mir)
{
    pawMir_remove_unreachable_blocks(mir);
}

static void enter_constant_ctx(struct LowerHir *L, struct ConstantContext *cctx, struct HirConstDecl *d)
{
    struct ConstantContext *cursor = L->cctx;
    while (cursor != NULL) {
        if (d->did.value == cursor->did.value)
            LOWERING_ERROR(L, global_constant_cycle, d->span.start, d->ident.name->text);
        cursor = cursor->outer;
    }
    *cctx = (struct ConstantContext){
        .outer = L->cctx,
        .did = d->did,
    };
    L->cctx = cctx;
}

static void leave_constant_ctx(struct LowerHir *L)
{
    paw_assert(L->cctx != NULL);
    L->cctx = L->cctx->outer;
}

static void enter_match(struct FunctionState *fs, struct MatchState *ms)
{
    *ms = (struct MatchState){
        .results = MatchResults_new(fs->L),
        .places = VarPlaces_new(fs->L),
        .vars = MatchVars_new(fs->C),
        .outer = fs->ms,
    };
    fs->ms = ms;
}

static void leave_match(struct FunctionState *fs)
{
    VarPlaces_delete(fs->L, fs->ms->places);
    fs->ms = fs->ms->outer;
}

static enum BuiltinKind builtin_kind(struct LowerHir *L, IrType *type)
{
    return pawP_type2code(L->C, type);
}

static IrType *get_builtin_type(struct LowerHir *L, enum BuiltinKind kind)
{
    return pawP_builtin_type(L->C, kind);
}

static IrType *get_type(struct LowerHir *L, NodeId id)
{
    return pawIr_get_type(L->C, id);
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

static MirBlock const *get_successors(struct FunctionState *fs)
{
    return current_bb_data(fs)->successors->data;
}

static MirBlock get_last_successor(struct FunctionState *fs)
{
    return K_LIST_LAST(current_bb_data(fs)->successors);
}

static struct MirPlace new_constant(struct FunctionState *fs, Value value, enum BuiltinKind kind)
{
    MirConstant const k = pawMir_kcache_add(fs->mir, fs->mir->kcache, value, kind);

    return (struct MirPlace){
        .kind = MIR_PLACE_CONSTANT,
        .projection = MirProjectionList_new(fs->mir),
        .type = pawP_builtin_type(fs->C, kind),
        .k = k,
    };
}

// Allocate virtual registers for a value of the given "type"
// Populates a description of the value's memory layout, and returns the lowest-numbered
// virtual register. Registers allocated in each invocation of this function are contiguous.
static struct MirPlace new_local(struct FunctionState *fs, IrType *type)
{
    struct IrLayout const layout = pawIr_compute_layout(fs->C, type);
    int const id = fs->mir->registers->count;
    MirRegisterDataList_push(fs->mir, fs->mir->registers,
        (struct MirRegisterData){
            .size = layout.size,
            .type = type,
        });
    return (struct MirPlace){
        .kind = MIR_PLACE_LOCAL,
        .projection = MirProjectionList_new(fs->mir),
        .r = MIR_REG(id),
        .type = type,
    };
}

static struct MirPlace new_local_literal(struct FunctionState *fs, paw_Type code)
{
    return new_local(fs, get_builtin_type(fs->L, code));
}

static void add_edge(struct FunctionState *fs, MirBlock from, MirBlock to)
{
    struct MirBlockData const *source = get_bb(fs, from);
    struct MirBlockData const *target = get_bb(fs, to);
    MirBlockList_push(fs->mir, source->successors, to);
    MirBlockList_push(fs->mir, target->predecessors, from);
}

static struct MirInstruction *add_instruction(struct FunctionState *fs, struct MirInstruction *instr)
{
    struct MirBlockData const *block = current_bb_data(fs);
    MirInstructionList_push(fs->mir, block->instructions, instr);
    return instr;
}

#define NEW_INSTR(Fs_, Kind_, ...) add_instruction(Fs_, pawMir_new_##Kind_((Fs_)->mir, __VA_ARGS__))

static struct MirInstruction *terminate_unreachable(struct FunctionState *fs, struct SourceLoc loc)
{
    return NEW_INSTR(fs, unreachable, loc);
}

static struct MirInstruction *terminate_goto(struct FunctionState *fs, struct SourceLoc loc)
{
    return NEW_INSTR(fs, goto, loc);
}

static void set_goto_edge(struct FunctionState *fs, struct SourceLoc loc, MirBlock to)
{
    add_edge(fs, current_bb(fs), to);
    terminate_goto(fs, loc);
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
                struct MirRegisterData const *rdata = mir_reg_data(fs->mir, var.r.r);
                if (rdata->is_captured) lowest_upvalue = var.r.r;
            }
        }
        if (bs->is_loop)
            break;
        bs = bs->outer;
    }
    if (needs_close) {
        struct MirPlace const p = {.r = lowest_upvalue};
        NEW_INSTR(fs, close, (struct SourceLoc){-1}, p);
    }
}

static void add_label(struct FunctionState *fs, struct SourceLoc loc, enum JumpKind kind)
{
    // MirClose must be added here if "continue" causes control to leave a scope containing
    // a captured variable, since the MirClose at the end of the loop body will not be reached.
    if (kind == JUMP_CONTINUE)
        close_until_loop(fs);
    terminate_goto(fs, loc);

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

static paw_Bool adjust_from(struct FunctionState *fs, enum JumpKind kind)
{
    int needs_close = 0;
    struct BlockState *bs = fs->bs;
    struct LabelList *ll = fs->labels;
    for (int i = bs->label0; i < ll->count;) {
        struct Label lb = LabelList_get(ll, i);
        if (lb.kind == kind) {
            needs_close |= lb.needs_close;
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
            add_edge(fs, lb.from, to);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static struct MirPlace load_from(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace pointer)
{
    struct MirPlace const output = new_local(fs, pointer.type);
    NEW_INSTR(fs, load, loc, pointer, output);
    return output;
}

static void store_to(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace value, struct MirPlace pointer)
{
    NEW_INSTR(fs, store, loc, value, pointer);
}

static void move_to(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace from, struct MirPlace to)
{
    NEW_INSTR(fs, move, loc, to, from);
}

// TODO: Handle squashing the case where an instruction writes to some unconstrained register,
//       then there is a MOVE to the stack during constant propagation. This probably shows up
//       in other places so this may not be very helpful.
static paw_Bool is_local_var(struct FunctionState *fs, MirRegister r)
{
    MirRegister const *pr;
    K_LIST_FOREACH (fs->locals, pr) {
        if (MIR_ID_EQUALS(r, *pr))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void lower_place_into(struct HirVisitor *V, struct HirExpr *expr, struct MirPlace *pplace);
static struct MirPlace lower_rvalue(struct HirVisitor *V, struct HirExpr *expr);
static struct MirPlace lower_lvalue(struct HirVisitor *V, struct HirExpr *expr);
static struct MirPlace lower_path_expr(struct HirVisitor *V, struct HirPathExpr *e);

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
    struct MirRegisterData const *data = mir_reg_data(fs->mir, r.r);
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
    struct MirPlace const output = new_local(fs, type);
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

    // TODO: use local.r.type instead of mir_reg_data(...)->type
    struct LocalVar const local = VarStack_get(fs->stack, *pindex);
    struct MirRegisterData const *rdata = mir_reg_data(local.fs->mir, local.r.r);
    *png = (struct NonGlobal){
        .type = rdata->type,
        .index = local.index,
        .r = local.r,
    };

    if (local.fs != fs)
        resolve_upvalue(fs, local, png);
    return PAW_TRUE;
}

static struct MirPlace unit_literal(struct FunctionState *fs, struct SourceLoc loc)
{
    return new_constant(fs, I2V(0), BUILTIN_UNIT);
}

static paw_Bool is_local(struct FunctionState *fs, MirRegister r)
{
    MirRegister const *pr;
    K_LIST_FOREACH (fs->locals, pr) {
        if (MIR_ID_EQUALS(r, *pr)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

struct MirInstruction *terminate_return(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace value)
{
    if (!MIR_ID_EXISTS(value.r)) value = unit_literal(fs, loc);
    return NEW_INSTR(fs, return, loc, value);
}

struct MirInstruction *terminate_branch(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace cond)
{
    return NEW_INSTR(fs, branch, loc, cond);
}

struct MirInstruction *terminate_switch(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace discr, struct MirSwitchArmList *arms, paw_Bool has_otherwise)
{
    return NEW_INSTR(fs, switch, loc, discr, arms, has_otherwise);
}

static MirBlock enter_function(struct LowerHir *L, struct FunctionState *fs, struct BlockState *bs, struct Mir *mir)
{
    *fs = (struct FunctionState){
        .result = IR_FPTR(mir->type)->result,
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

    struct IrFnPtr const *fptr = IR_FPTR(mir->type);
    MirBlock const entry = new_bb(fs);
    set_current_bb(fs, entry);
    struct MirBlockData const *bb = get_bb(fs, entry);
    MirBlockList_push(mir, bb->predecessors, MIR_INVALID_BB);
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
    fs->stack->count = fs->level;
    L->fs = fs->outer;

    MirRegister const *pr;
    struct Mir *mir = fs->mir;
    // write capture list in order that locals were allocated
    K_LIST_FOREACH (fs->locals, pr) {
        if (mir_reg_data(mir, *pr)->is_captured)
            MirCaptureList_push(mir, mir->captured,
                (struct MirCaptureInfo){*pr});
    }
}

#define LOWER_BLOCK(L, b) lower_rvalue((L)->V, HIR_CAST_EXPR(b))

static struct MirPlace select_field(struct FunctionState *fs, struct MirPlace object, IrType *target, int field, int discr, IrType *result)
{
    struct MirPlace output = new_local(fs, result);
    output.is_element_ptr = PAW_TRUE;
    NEW_INSTR(fs, struct_gep, TODO, output, object, field, discr);
    return output;
}

static struct MirPlace select_element(struct FunctionState *fs, struct MirPlace place, IrType *target, struct MirPlace index, IrType *result)
{
    struct MirPlace output = new_local(fs, result);
    output.is_element_ptr = PAW_TRUE;
    enum BuiltinKind const kind = pawP_type2code(fs->C, target);
    if (kind == BUILTIN_STR) {
        NEW_INSTR(fs, str_gep, TODO, output, place, index);
    } else if (kind == BUILTIN_LIST) {
        NEW_INSTR(fs, list_gep, TODO, output, place, index);
    } else {
        paw_assert(kind == BUILTIN_MAP);
        NEW_INSTR(fs, map_gep, TODO, output, place, index, PAW_FALSE);
    }
    return output;
}

static struct MirPlace select_range(struct FunctionState *fs, struct MirPlace place, IrType *target, MirRegister lower, MirRegister upper)
{
    struct MirPlace p = pawMir_copy_place(fs->mir, place);
    p.type = target;

//    MirProjection *range = MirProjection_new_range(fs->mir, lower, upper);
//    MirProjectionList_push(fs->mir, p.projection, range);
    return p;
}

static struct MirPlace first_slice_index(struct FunctionState *fs, struct HirVisitor *V, struct SourceLoc loc, struct HirExpr *e)
{
    if (e != NULL) return lower_rvalue(fs->L->V, e);

    // default to integer 0
    return new_constant(fs, I2V(0), BUILTIN_INT);
}

static struct MirPlace second_slice_index(struct FunctionState *fs, struct HirVisitor *V, struct SourceLoc loc, struct HirExpr *e, struct MirPlace object, int offset)
{
    struct MirPlace output;
    if (e == NULL) {
        // default to the length of the container
        output = new_local_literal(fs, BUILTIN_INT);
        enum BuiltinKind const kind = pawP_type2code(fs->C, mir_reg_data(fs->mir, object.r)->type);
        enum MirUnaryOpKind const op = kind == BUILTIN_STR ? MIR_UNARY_STRLEN : MIR_UNARY_LISTLEN;
        NEW_INSTR(fs, unary_op, loc, op, object, output);
    } else if (offset != 0) {
        output = new_local_literal(fs, BUILTIN_INT);
        struct MirPlace const index = lower_rvalue(V, e);
        struct MirPlace const one = new_constant(fs, I2V(offset), BUILTIN_INT);
        NEW_INSTR(fs, binary_op, loc, MIR_BINARY_IADD, index, one, output);
    } else {
        output = lower_rvalue(V, e);
    }
    return output;
}

static void lower_range_index(struct HirVisitor *V, struct HirExpr *index, struct MirPlace object, struct MirPlace *plower, struct MirPlace *pupper)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *type = GET_NODE_TYPE(fs->C, index);
    switch (builtin_kind(L, type)) {
        case BUILTIN_RANGE: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *lower = HirGetFieldExpr(K_LIST_FIRST(lit.items))->value;
            struct HirExpr *upper = HirGetFieldExpr(K_LIST_LAST(lit.items))->value;
            *plower = first_slice_index(fs, V, NODE_START(index), lower);
            *pupper = second_slice_index(fs, V, NODE_START(index), upper, object, 0);
            break;
        }
        case BUILTIN_RANGE_FROM: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *lower = HirGetFieldExpr(K_LIST_FIRST(lit.items))->value;
            *plower = first_slice_index(fs, V, NODE_START(index), lower);
            *pupper = second_slice_index(fs, V, NODE_START(index), NULL, object, 0);
            break;
        }
        case BUILTIN_RANGE_TO: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *upper = HirGetFieldExpr(K_LIST_LAST(lit.items))->value;
            *plower = first_slice_index(fs, V, NODE_START(index), NULL);
            *pupper = second_slice_index(fs, V, NODE_START(index), upper, object, 0);
            break;
        }
        case BUILTIN_RANGE_FULL: {
            *plower = first_slice_index(fs, V, NODE_START(index), NULL);
            *pupper = second_slice_index(fs, V, NODE_START(index), NULL, object, 0);
            break;
        }
        case BUILTIN_RANGE_INCLUSIVE: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *lower = HirGetFieldExpr(K_LIST_FIRST(lit.items))->value;
            struct HirExpr *upper = HirGetFieldExpr(K_LIST_LAST(lit.items))->value;
            *plower = first_slice_index(fs, V, NODE_START(index), lower);
            *pupper = second_slice_index(fs, V, NODE_START(index), upper, object, 1);
            break;
        }
        case BUILTIN_RANGE_TO_INCLUSIVE: {
            struct HirCompositeLit lit = HirGetLiteralExpr(index)->comp;
            struct HirExpr *upper = HirGetFieldExpr(K_LIST_LAST(lit.items))->value;
            *plower = first_slice_index(fs, V, NODE_START(index), NULL);
            *pupper = second_slice_index(fs, V, NODE_START(index), upper, object, 1);
            break;
        }
        default:
            PAW_UNREACHABLE();
    }
}

static struct MirPlace lower_sequence_index(struct HirVisitor *V, struct HirIndex const *e, struct MirPlace object)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *object_type = GET_NODE_TYPE(fs->C, e->target);
    IrType *index_type = GET_NODE_TYPE(fs->C, e->index);
    IrType *element_type = pawIr_get_type(fs->C, e->id);
    if (builtin_kind(L, index_type) == BUILTIN_INT) {
        struct MirPlace const index = new_local(fs, index_type);
        move_to(fs, NODE_START(e->index), lower_rvalue(V, e->index), index);
        return select_element(fs, object, object_type, index, element_type);
    } else {
        struct MirPlace first, second;
        lower_range_index(V, e->index, object, &first, &second);
        return select_range(fs, object, object_type, first.r, second.r);
    }
}

static struct MirPlace lower_mapping_index(struct HirVisitor *V, struct HirIndex const *e, struct MirPlace object)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *target_type = GET_NODE_TYPE(fs->C, e->target);
    IrType *index_type = GET_NODE_TYPE(fs->C, e->index);
    IrType *element_type = pawIr_get_type(fs->C, e->id);
    struct MirPlace const index = new_local(fs, index_type);
    move_to(fs, NODE_START(e->index), lower_rvalue(V, e->index), index);

    return select_element(fs, object, target_type, index, element_type);
}

static paw_Bool visit_param_decl(struct HirVisitor *V, struct HirParamDecl *d)
{
    struct LowerHir *L = V->ud;
    IrType *type = pawIr_get_type(L->C, d->id);
    alloc_local(L->fs, d->ident, d->id, type);
    return PAW_FALSE;
}

static void inline_copy(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace from, struct MirPlace to)
{
    NEW_INSTR(fs, inline_copy, loc, from, to);
}

static paw_Bool visit_let_stmt(struct HirVisitor *V, struct HirLetStmt *s)
{
    struct LowerHir *L = V->ud;
    IrType *type = pawIr_get_type(L->C, s->id);
    struct HirBindingPat const *p = HirGetBindingPat(s->pat);

    if (s->init != NULL) {
        struct MirPlace const init = lower_rvalue(V, s->init);
        struct LocalVar const local = alloc_local(L->fs, p->ident, p->id, type);
        if (mir_is_inline_aggregate(L->C, init.type)) {
            inline_copy(L->fs, s->span.start, init, local.r);
        } else {
            move_to(L->fs, p->span.start, init, local.r);
        }
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

    return new_constant(fs, e->basic.value, e->basic.code);
}

static struct MirPlace lower_tuple_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    int index;
    struct HirExpr *const *pexpr;
    MirPlaceList *elems = MirPlaceList_new(fs->mir);
    K_LIST_ENUMERATE (e->tuple.elems, index, pexpr) {
        struct MirPlace const place = lower_rvalue(V, *pexpr);
        MirPlaceList_push(fs->mir, elems, place);
    }

    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    NEW_INSTR(fs, aggregate, e->span.start, elems, output, PAW_FALSE);

    return output;
}

static struct MirPlace lower_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    MirPlaceList_reserve(fs->mir, fields, e->comp.items->count);
    {
        struct HirExpr *const *pexpr;
        K_LIST_FOREACH (e->comp.items, pexpr) {
            struct HirFieldExpr const *e = HirGetFieldExpr(*pexpr);
            struct MirPlace const place = lower_rvalue(V, e->value);
            MirPlaceList_push(fs->mir, fields, place);
        }
    }

    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    struct IrAdtDef const *def = pawIr_get_adt_def(L->C, IR_TYPE_DID(output.type));
    NEW_INSTR(fs, aggregate, e->span.start, fields, output, !def->is_inline);

    return output;
}

static struct MirPlace lower_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    enum BuiltinKind const kind = builtin_kind(L, get_type(L, e->id));
    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    MirPlaceList *elems = MirPlaceList_new(fs->mir);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (e->cont.items, pexpr) {
        if (HirIsFieldExpr(*pexpr)) {
            struct HirFieldExpr const *elem = HirGetFieldExpr(*pexpr);
            MirPlaceList_push(fs->mir, elems, lower_rvalue(V, elem->key));
            MirPlaceList_push(fs->mir, elems, lower_rvalue(V, elem->value));
        } else {
            MirPlaceList_push(fs->mir, elems, lower_rvalue(V, *pexpr));
        }
    }

    NEW_INSTR(fs, container, e->span.start, kind, e->cont.items->count, elems, output);
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
    terminate_goto(fs, e->span.start);
    set_current_bb(fs, test_bb);

    struct MirPlace const result = new_local_literal(fs, BUILTIN_BOOL);
    struct MirPlace const first = lower_rvalue(V, e->lhs);
    add_edge(fs, current_bb(fs), e->is_and ? rhs_bb : lhs_bb); // "then" block
    add_edge(fs, current_bb(fs), e->is_and ? lhs_bb : rhs_bb); // "else" block
    terminate_branch(fs, e->span.start, first);

    set_current_bb(fs, lhs_bb);
    move_to(fs, NODE_START(e->lhs), first, result);
    set_goto_edge(fs, e->span.start, after_bb);

    set_current_bb(fs, rhs_bb);
    struct MirPlace const second = lower_rvalue(V, e->rhs);
    move_to(fs, NODE_START(e->rhs), second, result);
    set_goto_edge(fs, e->span.start, after_bb);

    set_current_bb(fs, after_bb);
    return result;
}

static struct MirPlace lower_unit_struct(struct HirVisitor *V, struct HirPathExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    NEW_INSTR(fs, aggregate, e->span.start, MirPlaceList_new(fs->mir),
            output, PAW_FALSE);

    return output;
}

static struct MirPlace lower_unit_variant(struct HirVisitor *V, struct HirPathExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    struct MirPlace const discr = new_constant(fs, I2V(d->index), BUILTIN_INT);
    MirPlaceList_push(fs->mir, fields, discr);

    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    struct IrAdtDef const *def = pawIr_get_adt_def(L->C, IR_TYPE_DID(output.type));
    NEW_INSTR(fs, aggregate, e->span.start, fields, output, !def->is_inline);

    return output;
}

// Routine for lowering a global constant
static void lower_global_constant(struct LowerHir *L, struct HirConstDecl *d);

static struct MirPlace lookup_global_constant(struct LowerHir *L, struct HirConstDecl *d)
{
    int const *pid = GlobalMap_get(L, L->globals, d->did);
    if (pid != NULL) {
        struct GlobalInfo const info = GlobalList_get(L->C->globals, *pid);
        return new_constant(L->fs, info.value, info.b_kind);
    }
    lower_global_constant(L, d);
    return lookup_global_constant(L, d);
}

static struct MirPlace lower_ascription_expr(struct HirVisitor *V, struct HirAscriptionExpr *e)
{
    return lower_rvalue(V, e->expr);
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
    struct MirPlace const output = new_local(fs, get_type(L, e->id));

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
    NEW_INSTR(fs, global, e->span.start, output);
    return output;
}

static void emit_get_field(struct FunctionState *fs, struct SourceLoc loc, IrType *type, struct MirPlace object, int index, int discr, struct MirPlace output)
{
    struct MirPlace const field = select_field(fs, object, type, index, discr, output.type);
    move_to(fs, loc, field, output);
}

static struct MirSwitchArmList *allocate_switch_arms(struct FunctionState *fs, MirBlock discr_bb, int count)
{
    struct MirSwitchArmList *arms = MirSwitchArmList_new(fs->mir);
    MirSwitchArmList_reserve(fs->mir, arms, count);
    arms->count = count;

    for (int i = 0; i < count; ++i)
        add_edge(fs, discr_bb, new_bb(fs));

    return arms;
}

static struct MirPlace option_chain_error(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace object, IrType *target)
{
    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    struct MirPlace const discr = new_constant(fs, I2V(PAW_OPTION_NONE), BUILTIN_INT);
    MirPlaceList_push(fs->mir, fields, discr);

    struct MirPlace const output = new_local(fs, fs->result);
    NEW_INSTR(fs, aggregate, loc, fields, output, PAW_FALSE);

    return output;
}

static struct MirPlace result_chain_error(struct FunctionState *fs, struct SourceLoc loc, struct MirPlace object, IrType *target)
{
    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    struct MirPlace const k = new_constant(fs, I2V(PAW_RESULT_ERR), BUILTIN_INT);
    MirPlaceList_push(fs->mir, fields, k);

    IrType *error_type = K_LIST_LAST(IrGetAdt(fs->result)->types);
    struct MirPlace const e = select_field(fs, object, fs->result, 1, PAW_RESULT_ERR, error_type);
    MirPlaceList_push(fs->mir, fields, e);

    struct MirPlace const output = new_local(fs, fs->result);
    NEW_INSTR(fs, aggregate, loc, fields, output, PAW_FALSE);

    return output;
}

// Transformation:
//     opt?  =>  match opt {Some(x) => x, None => return None}
//     opt?  =>  match opt {Ok(x) => x, Err(e) => return Err(e)}
//
static struct MirPlace lower_chain_expr(struct HirVisitor *V, struct HirChainExpr *e)
{
    _Static_assert(PAW_OPTION_SOME == PAW_RESULT_OK && PAW_OPTION_NONE == PAW_RESULT_ERR,
            "Option and Result discriminants must have the same values for success vs. failure");
    int const EXISTS = PAW_OPTION_SOME;
    int const MISSING = PAW_OPTION_NONE;

    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *target = GET_NODE_TYPE(fs->C, e->target);
    enum BuiltinKind const kind = builtin_kind(L, target);

    MirBlock const input_bb = current_bb(fs);
    MirBlock const none_bb = new_bb(fs);
    MirBlock const after_bb = new_bb(fs);

    struct MirPlace const object = lower_rvalue(V, e->target);
    struct MirPlace const discr = new_local_literal(fs, BUILTIN_INT);
    emit_get_field(fs, e->span.start, target, object, 0, MISSING, discr);

    struct MirSwitchArmList *arms = allocate_switch_arms(fs, input_bb, 1);
    terminate_switch(fs, e->span.start, discr, arms, PAW_TRUE);
    struct MirSwitchArm *arm = &K_LIST_FIRST(arms);
    arm->k = new_constant(fs, I2V(EXISTS), BUILTIN_INT).k;

    set_current_bb(fs, get_last_successor(fs));
    struct MirPlace const value = new_local(fs, get_type(L, e->id));
    emit_get_field(fs, e->span.start, target, object, 1, EXISTS, value);
    set_goto_edge(fs, e->span.start, after_bb);

    set_current_bb(fs, none_bb);
    add_edge(fs, input_bb, none_bb);
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

    struct MirPlace const value = lower_rvalue(V, e->target);
    struct MirPlace const output = new_local(fs, get_type(L, e->id));
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
        binop2op_str(op);

    NEW_INSTR(fs, binary_op, span.start, binop, lhs, rhs, output);
}

static void lower_concat_args(struct HirVisitor *V, struct HirExpr *expr, MirPlaceList *result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    if (HirIsBinOpExpr(expr)) {
        struct HirBinOpExpr const *e = HirGetBinOpExpr(expr);
        paw_assert(e->op == BINARY_CONCAT);
        MirPlaceList_push(fs->mir, result, lower_rvalue(V, e->lhs));
        MirPlaceList_push(fs->mir, result, lower_rvalue(V, e->rhs));
    } else {
        MirPlaceList_push(fs->mir, result, lower_rvalue(V, expr));
    }
}

static struct MirPlace lower_as_concat(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    // TODO: rewrite OP_LISTCAT so it can handle more than 2 input operands, then remove this "if" statement
    if (builtin_kind(L, get_type(L, e->id)) == BUILTIN_LIST) {
        struct MirPlace const output = new_local(fs, get_type(L, e->id));
        MirPlaceList *args = MirPlaceList_new(fs->mir);
        MirPlaceList_push(fs->mir, args, lower_rvalue(V, e->lhs));
        MirPlaceList_push(fs->mir, args, lower_rvalue(V, e->rhs));

        NEW_INSTR(fs, concat, e->span.start, args, output, BUILTIN_LIST);
        return output;
    }

    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    MirPlaceList *args = MirPlaceList_new(fs->mir);
    lower_concat_args(V, e->lhs, args);
    lower_concat_args(V, e->rhs, args);

    NEW_INSTR(fs, concat, e->span.start, args, output, BUILTIN_STR);
    return output;
}

static struct MirPlace lower_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    if (e->op == BINARY_CONCAT)
        return lower_as_concat(V, e);

    enum BuiltinKind const kind = kind_of_builtin(L, e->lhs);
    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    struct MirPlace const lhs = lower_rvalue(V, e->lhs);
    struct MirPlace const rhs = lower_rvalue(V, e->rhs);
    if (!IS_BUILTIN_TYPE(kind)) return output; // must be "!"

    new_binary_op(V, e->span, e->op, kind, lhs, rhs, output);
    return output;
}

static void lower_function_block(struct LowerHir *L, struct HirExpr *block)
{
    struct FunctionState *fs = L->fs;
    struct MirPlace const result = lower_rvalue(L->V, block);
    terminate_return(fs, fs->mir->span.end, result);
}

static struct MirPlace lower_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *outer = L->fs;
    IrType *type = pawIr_get_type(L->C, e->id);

    struct Mir *result = pawMir_new(L->C, L->pm->name, e->span,
            SCAN_STR(L->C, PRIVATE("closure")), type, NULL,
            FUNC_CLOSURE, PAW_FALSE, PAW_FALSE);

    {
        struct BlockState bs;
        struct FunctionState fs;
        MirBlock const entry = enter_function(L, &fs, &bs, result);
        MirBlock const first = new_bb(&fs);

        pawHir_visit_decl_list(L->V, e->params);
        terminate_goto(&fs, e->span.start);
        add_edge(&fs, entry, first);
        set_current_bb(&fs, first);

        if (HirIsBlock(e->expr)) {
            lower_function_block(L, e->expr);
        } else {
            // evaluate and return the expression
            struct MirPlace const result = lower_rvalue(V, e->expr);
            terminate_return(&fs, e->span.end, result);
        }
        result->upvalues = fs.up;
        leave_function(L);

        postprocess(result);
    }

    struct MirPlace const output = new_local(L->fs, type);
    NEW_INSTR(outer, closure, e->span.start, outer->mir->children->count, output);
    MirBodyList_push(outer->mir, outer->mir->children, result);
    return output;
}

static struct MirPlace lower_conversion_expr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    static int const NEEDS_CAST[NBUILTIN_SCALARS][NBUILTIN_SCALARS] = {
        //          to  = {0, b, c, i, f}
        [BUILTIN_BOOL]  = {0, 0, 1, 1, 1},
        [BUILTIN_CHAR]  = {0, 1, 0, 1, 0},
        [BUILTIN_INT]   = {0, 1, 1, 0, 1},
        [BUILTIN_FLOAT] = {0, 1, 0, 1, 0},
    };

    enum BuiltinKind from = kind_of_builtin(L, e->arg);
    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    struct MirPlace const target = lower_rvalue(V, e->arg);
    if (NEEDS_CAST[from][e->to]) {
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

    // set the discriminant: an "int" residing in the first Value slot of the variant
    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    struct MirPlace const discr = new_constant(fs, I2V(d->index), BUILTIN_INT);
    MirPlaceList_push(fs->mir, fields, discr);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (e->args, pexpr) {
        struct MirPlace const field = lower_rvalue(V, *pexpr);
        MirPlaceList_push(fs->mir, fields, field);
    }
    struct MirPlace const output = new_local(fs, get_type(L, e->id));
    struct IrVariantDef const *variant_def = pawIr_get_variant_def(L->C, d->did);
    struct IrAdtDef const *adt_def = pawIr_get_adt_def(L->C, variant_def->base_did);
    NEW_INSTR(fs, aggregate, e->span.start, fields, output, !adt_def->is_inline);
    return output;
}

static void lower_args(struct LowerHir *L, struct HirExprList *exprs, struct MirPlaceList *result)
{
    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (exprs, pexpr) {
        struct MirPlace const arg = lower_rvalue(L->V, *pexpr);
        MirPlaceList_push(L->fs->mir, result, arg);
    }
}

static struct MirPlace lower_callee_and_args(struct HirVisitor *V, struct HirExpr *callee, struct HirExprList *args_in, MirPlaceList *args_out)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace result;
    if (HirIsSelector(callee) && !HirGetSelector(callee)->is_index) {
        // must be a method call since "is_index" is set to 1 for field selectors
        result = new_local(fs, get_type(L, callee->hdr.id));
        struct HirSelector const *select = HirGetSelector(callee);
        NEW_INSTR(fs, global, callee->hdr.span.start, result);

        // add context argument for method call
        struct MirPlace const self = lower_rvalue(V, select->target);
        MirPlaceList_push(fs->mir, args_out, self);
    } else {
        result = lower_rvalue(V, callee);
    }

    lower_args(L, args_in, args_out);
    return result;
}

static struct MirPlace lower_call_expr(struct HirVisitor *V, struct HirCallExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *target_type = GET_NODE_TYPE(L->C, e->target);
    if (IrIsSignature(target_type)) {
        struct HirDecl *decl = pawHir_get_decl(L->hir, IR_TYPE_DID(target_type));
        if (HirIsVariantDecl(decl))
            return lower_variant_constructor(V, e, HirGetVariantDecl(decl));
    }

    MirPlaceList *args = MirPlaceList_new(fs->mir);
    struct MirPlace const target = lower_callee_and_args(V, e->target, e->args, args);

    struct MirPlace const result = new_local(fs, get_type(L, e->id));
    NEW_INSTR(fs, call, e->span.start, target, args, result);

    IrType *return_type = IR_FPTR(target_type)->result;
    if (IrIsNever(return_type)) {
        // this function never returns
        terminate_unreachable(fs, e->span.start);
        set_current_bb(fs, new_bb(fs));
    }
    return result;
}

static struct MirPlace lower_field_expr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    if (e->fid < 0) lower_rvalue(V, e->key);
    return lower_rvalue(V, e->value);
}

static struct MirPlace lower_assign_expr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace const lhs = lower_lvalue(V, e->lhs);
    struct MirPlace const rhs = lower_rvalue(V, e->rhs);
    if (lhs.is_element_ptr) {
        store_to(fs, e->span.start, rhs, lhs);
    } else if (mir_is_inline_aggregate(L->C, lhs.type)) {
        inline_copy(fs, e->span.start, rhs, lhs);
    } else {
        move_to(fs, e->span.start, rhs, lhs);
    }

    // setters are expressions that evaluate to "()"
    return unit_literal(fs, e->span.start);
}

static struct MirPlace lower_as_concat_assign(struct HirVisitor *V, struct HirOpAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    MirPlaceList *args = MirPlaceList_new(fs->mir);
    struct MirPlace const lhs = lower_rvalue(V, e->lhs);
    MirPlaceList_push(fs->mir, args, lhs);
    lower_concat_args(V, e->rhs, args);

    struct MirPlace const temp = new_local(fs, lhs.type);
    NEW_INSTR(fs, concat, e->span.start, args, temp, builtin_kind(L, lhs.type));
    move_to(fs, e->span.start, temp, lhs);

    // setters are expressions that evaluate to "()"
    return unit_literal(fs, e->span.start);
}

static struct MirPlace lower_op_assign_expr(struct HirVisitor *V, struct HirOpAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    if (e->op == BINARY_CONCAT)
        return lower_as_concat_assign(V, e);

    enum BuiltinKind const kind = kind_of_builtin(L, e->lhs);
    struct MirPlace const lhs = lower_rvalue(V, e->lhs);
    struct MirPlace const rhs = lower_rvalue(V, e->rhs);
    struct MirPlace const temp = new_local(fs, lhs.type);
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
        ? lower_rvalue(V, e->result)
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
    terminate_goto(fs, e->span.start);

    struct BlockState bs;
    enter_block(fs, &bs, e->span, PAW_TRUE);

    set_current_bb(fs, header_bb);
    lower_rvalue(V, e->block);

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
            ? lower_rvalue(V, e->expr) // "return" Expr
            : LOCAL(MIR_INVALID_REG)); // "return" "()"

    MirBlock const next_bb = new_bb(fs);
    set_current_bb(fs, next_bb);
    return unit_literal(fs, e->span.start);
}

//static struct MirPlace lower_place_raw(struct HirVisitor *V, struct HirExpr *expr)
//{
//    struct LowerHir *L = V->ud;
//    struct MirPlace place = {
//        .projection = MirProjectionList_new(L->fs->mir),
//    };
//    lower_place_into(V, expr, &place);
//    return place;
//}
//
static struct MirPlace discharge(struct FunctionState *fs, struct MirPlace place)
{
    if (place.kind == MIR_PLACE_LOCAL && place.is_element_ptr
            && mir_is_boxed_aggregate(fs->C, place.type)) {
        struct MirPlace const output = new_local(fs, place.type);
        NEW_INSTR(fs, load, TODO, place, output);
        return output;
    }
    return place;
}
//
//static struct MirPlace lower_rvalue(struct HirVisitor *V, struct HirExpr *expr)
//{
//    struct LowerHir *L = V->ud;
//    struct FunctionState *fs = L->fs;
//
//    struct MirPlace place = lower_place_raw(V, expr);
//
////    discharge(fs, &place);
//    if (place.kind == MIR_PLACE_LOCAL && place.is_element_ptr) {
//        struct MirPlace const output = new_local(fs, place.type);
//        NEW_INSTR(fs, load, TODO, place, output);
//        place = output;
//    }
//    return place;
//}
//
//static struct MirPlace lower_lvalue(struct HirVisitor *V, struct HirExpr *expr)
//{
//    return lower_place_raw(V, expr);
//}

static paw_Bool visit_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    lower_rvalue(V, s->expr);
    return PAW_FALSE;
}

static struct MirPlace get_test_reg(struct FunctionState *fs, struct MatchVar v)
{
    struct MirPlace const *pr = VarPlaces_get(fs->L, fs->ms->places, v);
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

        struct LocalVar const local = alloc_local(fs, ident, pb->id, pb->var.type);
        move_to(fs, ident.span.start, test, local.r);
    }
}

static void lower_match_body(struct HirVisitor *V, struct MatchBody body, struct MirPlace result);
static void visit_decision(struct HirVisitor *V, struct Decision *d, struct MirPlace result);

static void visit_success(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
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

    struct MirPlace const cond = lower_rvalue(V, d->guard.cond);
    MirBlock const then_bb = new_bb(fs);
    MirBlock const else_bb = new_bb(fs);
    MirBlock const join_bb = new_bb(fs);

    MirBlock const before_bb = current_bb(fs);
    add_edge(fs, before_bb, then_bb);
    add_edge(fs, before_bb, else_bb);

    struct SourceLoc loc = NODE_START(d->guard.cond);
    struct MirInstruction *branch = terminate_branch(fs, loc, cond);
    set_current_bb(fs, then_bb);
    lower_match_body(V, d->guard.body, result);
    set_goto_edge(fs, loc, join_bb);

    set_current_bb(fs, else_bb);
    visit_decision(V, d->guard.rest, result);
    set_goto_edge(fs, loc, join_bb);

    set_current_bb(fs, join_bb);
}

static paw_Bool bindings_are_compatible(struct BindingList *lhs, struct BindingList *rhs)
{
    return lhs->count == 0 && rhs->count == 0; // TODO

    struct Binding const *a, *b;
    K_LIST_ZIP(lhs, a, rhs, b) {
        if (a->id.value != b->id.value)
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

static void lower_match_body(struct HirVisitor *V, struct MatchBody body, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct MatchState const *ms = fs->ms;
    struct SourceLoc const loc = NODE_START(body.result);

    struct MatchResult const *presult = MatchResults_get(L, ms->results, body.result->hdr.id);
    if (presult != NULL && bindings_are_compatible(presult->bindings, body.bindings)) {
        // The match body has already been lowered, and it declares the same bindings as this one.
        // Jump to it instead of lowering the expression again.
        set_goto_edge(fs, loc, presult->b);
        set_current_bb(fs, new_bb(fs));
        return;
    }

    MirBlock const b = new_bb(fs);
    set_goto_edge(fs, loc, b);
    set_current_bb(fs, b);

    declare_match_bindings(fs, body.bindings);

    struct MirPlace const r = lower_rvalue(V, body.result);
    move_to(fs, NODE_START(body.result), r, result);
    MatchResults_insert(L, ms->results, body.result->hdr.id,
        (struct MatchResult){
            .bindings = body.bindings,
            .b = b,
        });
}

static void map_var_to_reg(struct FunctionState *fs, struct MatchVar var, struct MirPlace r)
{
    VarPlaces_insert(fs->L, fs->ms->places, var, r);
}

static void allocate_match_vars(struct FunctionState *fs, struct MirPlace object, struct MatchCase mc, paw_Bool is_enum, int discr)
{
    if (mc.vars->count == 0)
        return;

    int index;
    struct MatchVar const *pv;
    K_LIST_ENUMERATE (mc.vars, index, pv) {
        Str *const name = SCAN_STR(fs->C, PRIVATE("variable"));
        struct HirIdent const ident = {.name = name, .span = pv->span};
        struct MirPlace const place = new_local(fs, pv->type);
        map_var_to_reg(fs, *pv, place);

#warning maybe need store
        struct MirPlace const source = select_field(fs, object, object.type,
                is_enum + index, discr, pv->type);
        move_to(fs, pv->span.start, source, place);
    }
}

static enum BuiltinKind cons_kind(enum ConstructorKind kind)
{
    switch (kind) {
        case CONS_BOOL:
            return BUILTIN_BOOL;
        case CONS_CHAR:
            return BUILTIN_CHAR;
        case CONS_INT:
            return BUILTIN_INT;
        case CONS_FLOAT:
            return BUILTIN_FLOAT;
        case CONS_STR:
            return BUILTIN_STR;
        case CONS_TUPLE:
        case CONS_STRUCT:
        case CONS_VARIANT:
        case CONS_WILDCARD:
        case CONS_REST:
            return NBUILTINS;
    }
}

static void visit_sparse_cases(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct CaseList *cases = d->multi.cases;
    MirBlock const discr_bb = current_bb(fs);
    MirBlock const join_bb = new_bb(fs);

    struct SourceLoc loc = d->multi.test.span.start;
    struct MirPlace const test = get_test_reg(fs, d->multi.test);
    struct MirSwitchArmList *arms = allocate_switch_arms(fs, discr_bb, cases->count);
    paw_Bool const has_otherwise = d->multi.rest != NULL;
    terminate_switch(fs, loc, test, arms, has_otherwise);

    int index = 0;
    struct MatchCase const *pmc;
    struct MirSwitchArm *parm;
    MirBlock const *psucc = get_successors(fs);
    K_LIST_ZIP (cases, pmc, arms, parm) {
        set_current_bb(fs, *psucc++);
        enum BuiltinKind const kind = cons_kind(pmc->cons.kind);
        parm->k = new_constant(fs, pmc->cons.value, kind).k;

        visit_decision(V, pmc->dec, result);
        set_goto_edge(fs, loc, join_bb);
    }

    // this implementation requires an "otherwise" case (binding or wildcard) to ensure
    // exhaustivness (expect for matches on values of type "bool")
    if (has_otherwise) {
        MirBlock const otherwise_bb = new_bb(fs);
        add_edge(fs, discr_bb, otherwise_bb);
        set_current_bb(fs, otherwise_bb);
        visit_decision(V, d->multi.rest, result);
        set_goto_edge(fs, loc, join_bb);
    }

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
    struct MirPlace const test = new_local_literal(fs, BUILTIN_INT);

    emit_get_field(fs, loc, d->multi.test.type, variant, 0, 0, test);

    struct MirSwitchArmList *arms = allocate_switch_arms(fs, discr_bb, cases->count);
    terminate_switch(fs, loc, test, arms, PAW_FALSE);

    struct MirSwitchArm *parm;
    struct MatchCase const *pmc;
    MirBlock const *psucc = get_successors(fs);
    K_LIST_ZIP (cases, pmc, arms, parm) {
        Value const discr = I2V(pmc->cons.variant.index);
        parm->k = new_constant(fs, discr, BUILTIN_INT).k;
        set_current_bb(fs, *psucc++);

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
    struct MatchCase const mc = K_LIST_FIRST(d->multi.cases);

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
    struct MatchCase const mc = K_LIST_FIRST(d->multi.cases);

    allocate_match_vars(fs, discr, mc, PAW_FALSE, 0);
    visit_decision(V, mc.dec, result);
}

static void visit_multiway(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;

    // there must exist at least 1 case; all cases have the same kind of constructor
    struct MatchCase const first_case = K_LIST_FIRST(d->multi.cases);
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

    struct MatchState ms;
    enter_match(fs, &ms);

    struct Decision *d = pawP_check_exhaustiveness(L->hir, L->pool, L->pm->name, e, ms.vars);
    paw_assert(ms.vars->count > 0);

    struct MirPlace const target = lower_rvalue(V, e->target);
    struct MirPlace const discr = new_local(fs, get_type(L, e->target->hdr.id));
    struct MirPlace const result = new_local(fs, get_type(L, e->id));
    move_to(fs, NODE_START(e->target), target, discr);
    map_var_to_reg(fs, K_LIST_FIRST(ms.vars), discr);

    visit_decision(V, d, result);

    leave_match(fs);
    return result;
}

static struct MirPlace lower_lvalue(struct HirVisitor *V, struct HirExpr *expr)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    switch (HIR_KINDOF(expr)) {
        case kHirSelector: {
            struct HirSelector const *x = HirGetSelector(expr);
            struct MirPlace const place = lower_lvalue(V, x->target);

            IrType *target_type = GET_NODE_TYPE(fs->C, x->target);
            IrType *field_type = pawIr_get_type(fs->C, x->id);
            return select_field(fs, place, target_type, x->index, 0, field_type);
        }
        case kHirIndex: {
            struct HirIndex const *x = HirGetIndex(expr);
            struct MirPlace const place = lower_rvalue(V, x->target);

            IrType *target_type = GET_NODE_TYPE(fs->C, x->target);
            return builtin_kind(L, target_type) != BUILTIN_MAP
                ? lower_sequence_index(V, x, place)
                : lower_mapping_index(V, x, place);
        }
        default: // kHirPathExpr
            return lower_path_expr(V, HirGetPathExpr(expr));
    }
}

static struct MirPlace lower_rvalue(struct HirVisitor *V, struct HirExpr *expr)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    switch (HIR_KINDOF(expr)) {
        case kHirSelector: {
            struct HirSelector const *x = HirGetSelector(expr);
            struct MirPlace const target = lower_rvalue(V, x->target);

            IrType *field_type = GET_NODE_TYPE(fs->C, expr);
            struct MirPlace const field = select_field(fs, target,
                    target.type, x->index, 0, field_type);
            return load_from(fs, x->span.start, field);
        }
        case kHirIndex: {
            struct HirIndex *x = HirGetIndex(expr);
            struct MirPlace const target = lower_rvalue(V, x->target);

            enum BuiltinKind const target_kind = builtin_kind(L, target.type);
            struct MirPlace const element = target_kind != BUILTIN_MAP
                ? lower_sequence_index(V, x, target)
                : lower_mapping_index(V, x, target);
            return load_from(fs, x->span.start, element);
        }
        case kHirAscriptionExpr:
            return lower_ascription_expr(V, HirGetAscriptionExpr(expr));
        case kHirPathExpr:
            return lower_path_expr(V, HirGetPathExpr(expr));
        case kHirLiteralExpr:
            return lower_literal_expr(V, HirGetLiteralExpr(expr));
        case kHirLogicalExpr:
            return lower_logical_expr(V, HirGetLogicalExpr(expr));
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
        case kHirFieldExpr:
            return lower_field_expr(V, HirGetFieldExpr(expr));
        case kHirAssignExpr:
            return lower_assign_expr(V, HirGetAssignExpr(expr));
        case kHirOpAssignExpr:
            return lower_op_assign_expr(V, HirGetOpAssignExpr(expr));
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
        default:
            PAW_UNREACHABLE();
    }
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
            struct HirSelector const *x = HirGetSelector(expr);
            lower_place_into(V, x->target, pplace);
//            discharge(fs, pplace);

            IrType *target_type = GET_NODE_TYPE(fs->C, x->target);
            IrType *element_type = pawIr_get_type(fs->C, x->id);
            *pplace = select_field(fs, *pplace, target_type, x->index, 0, element_type);
            break;
        }
        case kHirIndex: {
            struct HirIndex *x = HirGetIndex(expr);
            lower_place_into(V, x->target, pplace);
    //        discharge(fs, pplace);
    if (pplace->kind == MIR_PLACE_LOCAL && pplace->is_element_ptr) {
        struct MirPlace const output = new_local(fs, pplace->type);
        NEW_INSTR(fs, load, TODO, *pplace, output);
        *pplace = output;
    }

            IrType *target_type = GET_NODE_TYPE(fs->C, x->target);
            *pplace = builtin_kind(L, target_type) != BUILTIN_MAP
                ? lower_sequence_index(V, x, *pplace)
                : lower_mapping_index(V, x, *pplace);
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
    terminate_goto(&fs, fn->span.start);
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
    if (fn->body == NULL) return result;

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
    // "lower_rvalue" routine
    IrTypeList *artificial_params = IrTypeList_new(L->C);
    IrType *artificial_result = pawP_builtin_type(L->C, BUILTIN_UNIT);
    IrType *artificial_type = pawIr_new_fn_ptr(L->C, artificial_params, artificial_result);
    struct Mir *artificial = pawMir_new(L->C, L->pm->name, d->span, SCAN_STR(L->C, PRIVATE("toplevel")),
            artificial_type, NULL, FUNC_MODULE, PAW_FALSE, PAW_FALSE);

    // prevent cycles between global constants
    struct ConstantContext cctx;
    enter_constant_ctx(L, &cctx, d);

    struct BlockState bs;
    struct FunctionState fs;
    MirBlock const entry = enter_function(L, &fs, &bs, artificial);
    MirBlock const first = new_bb(&fs);
    terminate_goto(&fs, d->span.start);
    add_edge(&fs, entry, first);
    set_current_bb(&fs, first);

    struct MirPlace const result = lower_rvalue(L->V, d->init);
    terminate_return(&fs, d->span.start, result); // use variable to avoid DCE

    leave_constant_ctx(L);
    leave_function(L);

    // Perform constant folding (and maybe propagation) on the initializer expression. The
    // goal is to transform it into a single literal, which should always be possible, due
    // to the constantness checks performed in an earlier compilation phase.
    postprocess(artificial);
    pawSsa_construct(artificial);
    pawMir_propagate_constants(artificial);

    paw_assert(artificial->blocks->count == 2); // entry and exit blocks
    struct MirConstantData const *kdata = mir_const_data(artificial, MirGetLoadConstant(
            K_LIST_FIRST(K_LIST_LAST(artificial->blocks)->instructions))->k);
    register_global_constant(L, d, kdata->value, kdata->kind);

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

