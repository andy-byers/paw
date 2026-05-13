// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// lower_hir.c: Translation from the high-level intermediate representation
//     (HIR) to the mid-level IR (MIR)
//
// MIR is a control-flow graph (CFG) of basic blocks.

#include "api.h"
#include "error.h"
#include "hir.h"
#include "impl.h"
#include "ir_type.h"
#include "lib.h"
#include "match.h"
#include "mir.h"
#include "ssa.h"
#include "unify.h"

#define LOWERING_ERROR(L_, Kind_, ...) THROW_ERROR((L_)->C, \
        Kind_, .modname = (L_)->pm->name, __VA_ARGS__)
#define NODE_SPAN(Node_) ((Node_)->hdr.span)

#define TODO (struct SourceSpan){0}

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
    struct MirPlaceList *locals;
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
    MirScope scope;
    MirBlock current;
    MirBlock exit;
    struct MirPlace ret;
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
    return (paw_Uint)v.id;
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

static Str const *get_modname(struct FunctionState *fs)
{
    return ModuleInfo_get(fs->C->modinfo, fs->mir->modno).name;
}

static void mark_nontrivial(struct FunctionState *fs, struct MirPlace place)
{
    if (place.kind == MIR_PLACE_REGISTER)
        mir_reg_data(fs->mir, place.r)->is_nontrivial = PAW_TRUE;
}

static enum BuiltinKind builtin_kind(struct LowerHir *L, IrType *type)
{
    return pawP_type2code(L->C, type);
}

static void postprocess(struct Mir *mir)
{
    pawSsa_construct(mir);
    pawMir_propagate_constants(mir);
}

static void enter_constant_ctx(struct LowerHir *L, struct ConstantContext *cctx, struct HirConstDecl *d)
{
    struct ConstantContext *cursor = L->cctx;
    while (cursor != NULL) {
        if (d->did.value == cursor->did.value)
            LOWERING_ERROR(L, GlobalConstantCycle,
                    .name = d->ident.name,
                    .span = d->span);
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

static IrType *get_builtin_type(struct LowerHir *L, enum BuiltinKind kind)
{
    return pawP_builtin_type(L->C, kind);
}

static IrType *get_type(struct LowerHir *L, NodeId id)
{
    return pawIr_get_type(L->C, id);
}

static IrType *type_of(struct LowerHir *L, struct HirType *type)
{
    return get_type(L, type->hdr.id);
}

static paw_Bool is_scalar_type(struct FunctionState *fs, IrType *type)
{
    return IS_SCALAR_TYPE(builtin_kind(fs->L, type));
}

static struct LocalVar *alloc_local(struct FunctionState *fs, struct HirIdent ident, NodeId id, IrType *type);
static struct MirPlace lower_rvalue(struct HirVisitor *V, struct HirExpr *expr);
static struct MirPlace lower_lvalue(struct HirVisitor *V, struct HirExpr *expr);
static struct MirPlace lower_path_expr(struct HirVisitor *V, struct HirPathExpr *e);

static struct MirPlace addr_of(struct FunctionState *fs, struct MirPlace place);
static struct MirPlace new_register(struct FunctionState *fs, IrType *type);
static struct MirInstruction *add_instruction(struct FunctionState *fs, struct MirInstruction *instr);

#define NEW_INSTR(Fs_, Kind_, ...) add_instruction(Fs_, pawMir_new_##Kind_((Fs_)->mir, __VA_ARGS__))

static struct MirPlace load_from(struct FunctionState *fs, struct SourceSpan span, struct MirPlace pointer)
{
    struct MirPlace const output = new_register(fs, ir_deref(pointer.type));
    NEW_INSTR(fs, load, span, pointer, output);
    return output;
}

static void store_to(struct FunctionState *fs, struct SourceSpan span, struct MirPlace value, struct MirPlace pointer)
{
    NEW_INSTR(fs, store, span, value, pointer);
}

static void move_to(struct FunctionState *fs, struct SourceSpan span, struct MirPlace from, struct MirPlace to)
{
    NEW_INSTR(fs, move, span, to, from);
}

static int get_indirection_level(IrType *type)
{
    int level = 0;
    while (IrIsPtr(type)) {
        type = ir_deref(type);
        ++level;
    }
    return level;
}

static struct MirPlace self_arg(struct FunctionState *fs, struct HirExpr *expr, IrType *param_type)
{
    struct MirPlace arg = lower_lvalue(fs->L->V, expr);
    int const have_level = get_indirection_level(arg.type);
    int const want_level = get_indirection_level(param_type);
    for (int i = have_level; i < want_level; ++i)
        arg = addr_of(fs, arg);
    for (int i = have_level; i > want_level; --i)
        arg = load_from(fs, arg.span, arg);
    return arg;
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

static IrType *new_ptr(struct FunctionState *fs, IrType *type)
{
    return pawIr_new_ptr(fs->C, type);
}

static struct MirPlace new_constant(struct FunctionState *fs, struct SourceSpan span, Value value, enum BuiltinKind kind)
{
    MirConstant const k = pawMir_kcache_add(fs->mir, fs->mir->kcache, value, kind);

    return (struct MirPlace){
        .kind = MIR_PLACE_CONSTANT,
        .type = get_builtin_type(fs->L, kind),
        .span = span,
        .k = k,
    };
}

static struct MirPlace new_local(struct FunctionState *fs, Str const *name, IrType *type)
{
    paw_assert(type != NULL);
    int const id = fs->mir->registers->count;
    MirRegisterDataList_push(fs->mir, fs->mir->registers,
        (struct MirRegisterData){
            .is_nontrivial = !IS_BASIC_TYPE(builtin_kind(fs->L, type)),
            .is_captured = PAW_FALSE,
            .type = type,
            .name = name,
        });
    return (struct MirPlace){
        .kind = MIR_PLACE_REGISTER,
        .type = type,
        .r.value = id,
    };
}

// Allocate virtual registers for a value of the given "type"
// Populates a description of the value's memory layout, and returns the lowest-numbered
// virtual register. Registers allocated in each invocation of this function are contiguous.
static struct MirPlace new_register(struct FunctionState *fs, IrType *type)
{
    paw_assert(type != NULL);
    int const id = fs->mir->registers->count;
    MirRegisterDataList_push(fs->mir, fs->mir->registers,
        (struct MirRegisterData){
            .is_nontrivial = !IS_BASIC_TYPE(pawP_type2code(fs->C, type)),
            .type = type,
        });
    return (struct MirPlace){
        .kind = MIR_PLACE_REGISTER,
        .r = MIR_REG(id),
        .type = type,
    };
}

static struct MirPlace new_register_literal(struct FunctionState *fs, enum BuiltinKind kind)
{
    return new_register(fs, get_builtin_type(fs->L, kind));
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
    instr->hdr.scope = fs->scope;
    struct MirBlockData const *block = current_bb_data(fs);
    MirInstructionList_push(fs->mir, block->instructions, instr);
    return instr;
}

static struct MirPlace addr_of(struct FunctionState *fs, struct MirPlace place)
{
    if (place.kind == MIR_PLACE_CONSTANT) {
        // spill rvalues to memory so we have an address to use
        struct HirIdent const ident = {
            .name = SCAN_STR(fs->C, "(temporary)"),
            .span = place.span,
        };
        struct LocalVar const local = *alloc_local(fs, ident, NO_NODE, place.type);
        move_to(fs, place.span, place, local.r);
        place = local.r;
    }
    mark_nontrivial(fs, place);
    struct MirPlace const addr = new_register(fs, new_ptr(fs, place.type));
    NEW_INSTR(fs, addr_of, place.span, place, addr);
    return addr;
}

static struct MirInstruction *terminate_unreachable(struct FunctionState *fs, struct SourceSpan span)
{
    return NEW_INSTR(fs, unreachable, span);
}

static struct MirInstruction *terminate_goto(struct FunctionState *fs, struct SourceSpan span)
{
    return NEW_INSTR(fs, goto, span);
}

static void set_goto_edge(struct FunctionState *fs, struct SourceSpan span, MirBlock to)
{
    add_edge(fs, current_bb(fs), to);
    terminate_goto(fs, span);
}

static void set_current_bb(struct FunctionState *fs, MirBlock b)
{
    fs->current = b;
}

static MirBlock new_bb(struct FunctionState *fs)
{
    int const id = bb_list(fs)->count;
    struct MirBlockData *bb = pawMir_new_block(fs->mir, fs->scope);
    MirBlockDataList_push(fs->mir, bb_list(fs), bb);
    bb->scope = fs->scope;
    return MIR_BB(id);
}

static struct LocalVar *get_local_slot(struct FunctionState *fs, int index)
{
    return &K_LIST_AT(fs->stack, fs->level + index);
}

static void add_label(struct FunctionState *fs, struct SourceSpan span, enum JumpKind kind)
{
    terminate_goto(fs, span);

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
        lb->nvars = bs->nvars;
    }
}

static void remove_label(struct LabelList *ll, int index)
{
    LabelList_swap_remove(ll, index);
}

static void adjust_from(struct FunctionState *fs, enum JumpKind kind)
{
    struct BlockState *bs = fs->bs;
    struct LabelList *ll = fs->labels;
    for (int i = bs->label0; i < ll->count;) {
        struct Label lb = LabelList_get(ll, i);
        if (lb.kind == kind) {
            add_edge(fs, lb.from, current_bb(fs));
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
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

    paw_assert(r.kind == MIR_PLACE_REGISTER);
    struct MirRegisterData *data = mir_reg_data(fs->mir, r.r);
    if (!data->is_captured) {
        NEW_INSTR(fs, capture, TODO, r);
        data->is_nontrivial = PAW_TRUE;
        data->is_captured = PAW_TRUE;

        IrType *pointee = ir_deref(r.type);
        if (!pawIr_is_copyable(fs->C, pointee))
            pawErr_generic_error(ENV(fs->C), get_modname(fs),
                    r.span, "captured variable must be copyable");
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
    if (fs->up->count > PAW_MAX_UPVALUES)
        LOWERING_ERROR(fs->L, TooManyUpvalues,
                .limit = PAW_MAX_UPVALUES,
                .span = fs->mir->span);

    MirUpvalueList_push(fs->mir, fs->up, (struct MirUpvalueInfo){
        .is_local = is_local,
        .index = info->index,
        .type = info->type,
    });
    // indicate new upvalue index
    info->index = fs->up->count - 1;
    info->r.up = fs->up->count - 1;
}

static void enter_scope(struct FunctionState *fs)
{
    fs->scope = pawMir_new_scope(fs->mir, fs->scope);
}

static void leave_scope(struct FunctionState *fs)
{
    struct MirScopeInfo const info = pawMir_get_scope_info(fs->mir, fs->scope);
    fs->scope = info.outer;
}

static void drop_if_necessary(struct FunctionState *fs, struct MirPlace p, IrType *type)
{
    if (pawIr_needs_drop(fs->C, type))
        NEW_INSTR(fs, drop, TODO, p);
}

static void drop_locals(struct FunctionState *fs, struct BlockState *bs)
{
    for (int i = fs->stack->count - 1; i >= bs->nvars; --i) {
        if (i == 0) return; // never drop return value
        struct LocalVar const var = VarStack_get(fs->stack, i);
        paw_assert(var.depth == bs->depth);
        drop_if_necessary(fs, var.r, var.r.type);
    }
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

static void leave_block(struct FunctionState *fs)
{
    struct BlockState *bs = fs->bs;
    drop_locals(fs, bs);
    if (bs->is_loop)
        adjust_from(fs, JUMP_BREAK);
    if (bs->outer != NULL)
        adjust_labels(fs, bs);

    fs->stack->count = fs->level + bs->nvars;
    fs->nlocals = bs->nvars;
    fs->bs = bs->outer;
}

// TODO: accept extra args for flags stored in MirPlace and return LocalVar by value
static struct LocalVar *alloc_local(struct FunctionState *fs, struct HirIdent ident, NodeId id, IrType *type)
{
    struct MirPlace const output = new_local(fs, ident.name, type);
    NEW_INSTR(fs, alloc_local, ident.span, ident.name, output);

    LocalMap_insert(fs->L, fs->mapping, id, fs->stack->count);
    VarStack_push(fs->L, fs->stack, (struct LocalVar){
        .index = fs->nlocals,
        .depth = fs->bs->depth,
        .name = ident.name,
        .r = output,
        .fs = fs,
    });
    ++fs->nlocals;
    return &K_LIST_LAST(fs->stack);
}

static struct LocalVar *alloc_anon_local(struct FunctionState *fs, struct SourceSpan span, IrType *type)
{
    return alloc_local(fs,
            (struct HirIdent){
                .name = pawP_format_string(fs->C, "(local%d)", fs->nlocals),
                .span = span,
            }, (NodeId){(unsigned)-1}, type);
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
    *png = (struct NonGlobal){
        .type = local.r.type,
        .index = local.index,
        .r = local.r,
    };

    if (local.fs != fs)
        resolve_upvalue(fs, local, png);
    return PAW_TRUE;
}

static struct MirPlace unit_literal(struct FunctionState *fs, struct SourceSpan span)
{
    return new_constant(fs, span, I2V(0), BUILTIN_UNIT);
}

struct MirInstruction *terminate_return(struct FunctionState *fs, struct SourceSpan span, struct MirPlace value)
{
    move_to(fs, span, value, fs->ret);
    add_edge(fs, fs->current, fs->exit);
    return terminate_goto(fs, span);
}

struct MirInstruction *terminate_branch(struct FunctionState *fs, struct SourceSpan span, struct MirPlace cond)
{
    return NEW_INSTR(fs, branch, span, cond);
}

struct MirInstruction *terminate_switch(struct FunctionState *fs, struct SourceSpan span, struct MirPlace discr, struct MirSwitchArmList *arms, paw_Bool has_otherwise)
{
    return NEW_INSTR(fs, switch, span, discr, arms, has_otherwise);
}

static MirBlock enter_function(struct LowerHir *L, struct FunctionState *fs, struct BlockState *bs, struct Mir *mir)
{
    struct IrFnPtr const *fn = IrGetFnPtr(IR_GET_FN(L->C, mir->type));
    *fs = (struct FunctionState){
        .result = fn->result,
        .registers = mir->registers,
        .up = MirUpvalueList_new(mir),
        .level = L->stack->count,
        .scope = MIR_INVALID_SCOPE,
        .mapping = L->locals,
        .locals = MirPlaceList_new(mir),
        .labels = L->labels,
        .stack = L->stack,
        .outer = L->fs,
        .mir = mir,
        .C = L->C,
        .L = L,
    };
    L->fs = fs;

    enter_scope(fs);
    enter_block(fs, bs, mir->span, PAW_FALSE);
    MirBlock const entry = new_bb(fs);
    set_current_bb(fs, entry);
    struct MirBlockData const *bb = get_bb(fs, entry);
    MirBlockList_push(mir, bb->predecessors, MIR_INVALID_BB);

    struct HirIdent const ident = {
        .name = SCAN_STR(L->C, PRIVATE("return")),
        .span = mir->span,
    };
    fs->ret = alloc_local(fs, ident, NO_NODE, fn->result)->r;
    paw_assert(fs->ret.r.value == 0);
    fs->exit = new_bb(fs);
    return entry;
}

static void leave_function(struct LowerHir *L)
{
    struct FunctionState *fs = L->fs;
    struct Mir *mir = fs->mir;

    set_current_bb(fs, fs->exit);
    drop_locals(fs, fs->bs);
    NEW_INSTR(fs, return, mir->span);

    leave_scope(fs);
    fs->stack->count = fs->level;
    L->fs = fs->outer;

    // TODO: fs->locals no longer used so this won't do anything. closures are broken and need to be refactored anyway
    struct MirPlace const *plocal;
    // write capture list in order that locals were allocated
    K_LIST_FOREACH (fs->locals, plocal) {
        if (mir_reg_data(mir, plocal->r)->is_captured)
            MirCaptureList_push(mir, mir->captured,
                (struct MirCaptureInfo){.local = plocal->r});
    }
}

#define LOWER_BLOCK(L, b) lower_rvalue((L)->V, HIR_CAST_EXPR(b))

static IrType *auto_deref_full(IrType *type)
{
    while (IrIsPtr(type))
        type = ir_deref(type);
    return type;
}

static struct MirPlace auto_deref_object(struct FunctionState *fs, struct MirPlace place)
{
    if (IrIsPtr(place.type)) {
        for (;;) {
            struct IrPtr const *p = IrGetPtr(place.type);
            if (!IrIsPtr(p->pointee)) break;
            place = load_from(fs, place.span, place);
        }
    }
    return place;
}

static struct MirPlace select_field(struct FunctionState *fs, struct MirPlace object, int field, int discr, IrType *result)
{
    struct MirPlace const output = new_register(fs, new_ptr(fs, result));
    object = auto_deref_object(fs, object);
    NEW_INSTR(fs, struct_gep, TODO, output, object, field, discr);
    return output;
}

static struct MirPlace lower_selector(struct HirVisitor *V, struct HirSelector *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace const target = lower_lvalue(V, e->target);
    return select_field(fs, target, e->index, 0, get_type(L, e->id));
}

static struct MirPlace lower_array_index(struct FunctionState *fs, struct MirPlace result, struct MirPlace array, struct MirPlace index)
{
    NEW_INSTR(fs, array_gep, array.span, result, array, index);
    return result;
}

// Lower a HIR index expression to its MIR representation
// Given a target expression "target" of type "Target", and an index expression
// "index" of type "Idx", where the result "target[index]" has type "Elem", this
// function performs the following transformation:
//     before: Index(target, index)
//     after:  Call(<Target as Index<Idx, Elem>>::index, target, index)
//
static struct MirPlace lower_index(struct HirVisitor *V, struct HirIndex *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace target = lower_lvalue(V, e->target);
    target = auto_deref_object(fs, target);
    struct MirPlace const index = lower_rvalue(V, e->index);
    IrType *raw_target_type = auto_deref_full(target.type);

    IrType *result_type = pawIr_new_ptr(L->C, get_type(L, e->id));
    struct MirPlace const result = new_register(fs, result_type);

    if (IrIsArray(raw_target_type)) {
        if (!IrIsPtr(target.type)) target = addr_of(fs, target);
        return lower_array_index(fs, result, target, index);
    }

    // Determine the concrete type of the "Index::index" method that will
    // be used to represent this indexing operation.
    struct IrType2 const type2 = {raw_target_type, index.type};
    IrType *fn_type = *IrType2Map_get(L->C, L->C->indexes, type2);

    if (!IrIsPtr(target.type))
        target = addr_of(fs, target);

    struct MirPlace const fn = new_register(fs, fn_type);
    NEW_INSTR(fs, global, e->span, fn);

    MirPlaceList *args = MirPlaceList_new(fs->mir);
    MirPlaceList_push(fs->mir, args, target);
    MirPlaceList_push(fs->mir, args, index);

    NEW_INSTR(fs, call, e->span, fn, args, result);
    return result;
}

static paw_Bool visit_param_decl(struct HirVisitor *V, struct HirParamDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *type = get_type(L, d->id);
    fs->mir->is_method |= d->is_self;
    struct LocalVar *local = alloc_local(L->fs, d->ident, d->id, type);
    // TODO: this prevents arguments from being copy propagated or made into SSA variables. remove this once args are made into SSA variables (when addr not used)
    mir_reg_data(fs->mir, local->r.r)->is_nontrivial = PAW_TRUE;
    return PAW_FALSE;
}

static paw_Bool visit_let_stmt(struct HirVisitor *V, struct HirLetStmt *s)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *type = get_type(L, s->pat->hdr.id);
    struct HirBindingPat const *p = HirGetBindingPat(s->pat);

    if (s->init != NULL) {
        struct MirPlace const init = lower_rvalue(V, s->init);
        struct LocalVar const local = *alloc_local(fs, p->ident, p->id, type);
        move_to(fs, p->span, init, local.r);
    } else {
        // create an uninitialized virtual register to hold the variable
        alloc_local(fs, p->ident, p->id, type);
    }
    return PAW_FALSE;
}

static struct MirPlace lower_basic_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    return new_constant(fs, e->span, e->basic.value, e->basic.code);
}

static struct MirPlace lower_tuple_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    int index;
    struct HirExpr *const *pexpr;
    MirPlaceList *elems = MirPlaceList_new(fs->mir);
    K_LIST_ENUMERATE (e->tuple.elems, index, pexpr) {
        struct MirPlace const expr = lower_rvalue(V, *pexpr);
        struct MirPlace const elem = new_register(fs, expr.type);
        move_to(fs, expr.span, expr, elem);
        MirPlaceList_push(fs->mir, elems, elem);
    }

    struct MirPlace const output = new_register(fs, get_type(L, e->id));
    NEW_INSTR(fs, aggregate, e->span, elems, output, 0, PAW_FALSE);

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
            struct MirPlace const expr = lower_rvalue(V, e->value);
            struct MirPlace const field = new_register(fs, expr.type);
            move_to(fs, expr.span, expr, field);
            MirPlaceList_push(fs->mir, fields, field);
        }
    }

    struct MirPlace const output = new_register(fs, get_type(L, e->id));
    NEW_INSTR(fs, aggregate, e->span, fields, output, 0, PAW_FALSE);

    return output;
}

static struct MirPlace lower_array_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    MirPlaceList *elems = MirPlaceList_new(fs->mir);
    MirPlaceList_reserve(fs->mir, elems, e->array.elems->count);
    K_LIST_XFOREACH (e->array.elems, struct HirExpr *const, p) {
        struct MirPlace const expr = lower_rvalue(V, *p);
        struct MirPlace const elem = new_register(fs, expr.type);
        move_to(fs, expr.span, expr, elem);
        MirPlaceList_push(fs->mir, elems, elem);
    }

    struct MirPlace const output = new_register(fs, get_type(L, e->id));
    NEW_INSTR(fs, array, e->span, elems, output);
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
        case kHirLitArray:
            return lower_array_lit(V, e);
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
    terminate_goto(fs, e->span);
    set_current_bb(fs, test_bb);

    struct LocalVar const result = *alloc_anon_local(fs, e->span,
            get_builtin_type(L, BUILTIN_BOOL));
    struct MirPlace const first = lower_rvalue(V, e->lhs);
    add_edge(fs, current_bb(fs), e->is_and ? rhs_bb : lhs_bb); // "then" block
    add_edge(fs, current_bb(fs), e->is_and ? lhs_bb : rhs_bb); // "else" block
    terminate_branch(fs, e->span, first);

    set_current_bb(fs, lhs_bb);
    move_to(fs, NODE_SPAN(e->lhs), first, result.r);
    set_goto_edge(fs, e->span, after_bb);

    set_current_bb(fs, rhs_bb);
    struct MirPlace const second = lower_rvalue(V, e->rhs);
    move_to(fs, NODE_SPAN(e->rhs), second, result.r);
    set_goto_edge(fs, e->span, after_bb);

    set_current_bb(fs, after_bb);
    return result.r;
}

static struct MirPlace lower_unit_struct(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace const output = new_register(fs, get_type(L, e->id));
    NEW_INSTR(fs, aggregate, e->span, MirPlaceList_new(fs->mir),
            output, 0, PAW_FALSE);

    return output;
}

static struct MirPlace lower_unit_variant(struct HirVisitor *V, struct HirPathExpr *e, int index)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    struct MirPlace const discr = new_constant(fs, e->span, I2V(index), BUILTIN_INT);
    MirPlaceList_push(fs->mir, fields, discr);

    struct MirPlace const output = new_register(fs, get_type(L, e->id));
    NEW_INSTR(fs, aggregate, e->span, fields, output, index, PAW_FALSE);

    return output;
}

// Routine for lowering a global constant
static void lower_global_constant(struct LowerHir *L, struct HirConstDecl *d);

static struct MirPlace lookup_global_constant(struct LowerHir *L, struct HirConstDecl *d)
{
    int const *pid = GlobalMap_get(L, L->globals, d->did);
    if (pid != NULL) {
        struct GlobalInfo const info = GlobalList_get(L->C->globals, *pid);
        return new_constant(L->fs, d->span, info.value, info.b_kind);
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
    if (resolve_nonglobal(fs, last.target, &ng)) return ng.r;

    struct HirDecl *decl = pawHir_get_node(L->hir, last.target);
    struct MirPlace const output = new_register(fs, get_type(L, e->id));

    if (HirIsVariantDecl(decl)) {
        struct HirVariantDecl const *v = HirGetVariantDecl(decl);
        struct IrAdtDef const *def = pawIr_get_adt_def(L->C, v->base_did);
        if (def->is_struct) {
            return lower_unit_struct(V, e);
        } else {
            return lower_unit_variant(V, e, v->index);
        }
    } else if (HirIsConstDecl(decl)) {
        return lookup_global_constant(L, HirGetConstDecl(decl));
    } else if (HirIsAdtDecl(decl)) {
        return lower_unit_struct(V, e);
    }
    NEW_INSTR(fs, global, e->span, output);
    return output;
}

static struct MirPlace emit_get_field(struct FunctionState *fs, struct SourceSpan span, struct MirPlace object, int index, int discr, IrType *field_type)
{
    struct MirPlace const field = select_field(fs, object, index, discr, field_type);
    return load_from(fs, span, field);
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

static struct MirPlace option_chain_error(struct FunctionState *fs, struct SourceSpan span)
{
    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    struct MirPlace const discr = new_constant(fs, span, I2V(PAW_OPTION_NONE), BUILTIN_INT);
    MirPlaceList_push(fs->mir, fields, discr);

    struct MirPlace const output = new_register(fs, fs->result);
    NEW_INSTR(fs, aggregate, span, fields, output, PAW_OPTION_NONE, PAW_FALSE);

    return output;
}

static struct MirPlace result_chain_error(struct FunctionState *fs, struct SourceSpan span, struct MirPlace object)
{
    IrType *result_type = auto_deref_full(object.type);
    IrType *from_error_type = IrGenericArg_get_type(IrGenericArgs_last(IrGetAdt(result_type)->args));
    IrType *into_error_type = IrGenericArg_get_type(IrGenericArgs_last(IrGetAdt(fs->result)->args));
    struct MirPlace const from_error = select_field(fs, object, 1, PAW_RESULT_ERR, from_error_type);
    struct MirPlace const into_error = new_register(fs, into_error_type);

    // Determine the type of method `<E as Into<E2>>::into`, where `Result<_, E>` is the type of
    // the operand to `?` and `Result<_, E2>` is the return type of the enclosing function. Note
    // that the blanket implementation (`impl<T> Into<T> for T`) is used if a more specific impl
    // has not been provided.
    struct MirPlace into_fn;
    {
        IrGenericArgs *args = IrGenericArgs_new(fs->C);
        IrGenericArgs_push(fs->C, args, IrGenericArg_from_type(into_error_type));

        DeclId const trait_did = fs->C->core_traits[CORE_TRAIT_INTO];
        IrGenericArgs *trait_args = IrGenericArgs_new(fs->C);
        IrGenericArgs_push(fs->C, trait_args, IrGenericArg_from_type(from_error_type));
        IrGenericArgs_push(fs->C, trait_args, IrGenericArg_from_type(into_error_type));
        IrTrait *into_trait = pawIr_new_trait(fs->C, trait_did, trait_args);
        struct Instantiation const *inst = pawP_find_trait_method(fs->C, from_error_type,
                into_trait, SCAN_STR(fs->C, "into"));
        if (inst == NULL)
            __builtin_trap();

        into_fn = new_register(fs, inst->inst);
        NEW_INSTR(fs, global, span, into_fn);
    }

    // convert to the type of the error variant payload from the function return type
    MirPlaceList *into_args = MirPlaceList_new(fs->mir);
    MirPlaceList_push(fs->mir, into_args, from_error);
    NEW_INSTR(fs, call, span, into_fn, into_args, into_error);

    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    struct MirPlace const error_discr = new_constant(fs, span, I2V(PAW_RESULT_ERR), BUILTIN_INT);
    MirPlaceList_push(fs->mir, fields, error_discr);
    MirPlaceList_push(fs->mir, fields, into_error);

    struct MirPlace const output = new_register(fs, fs->result);
    NEW_INSTR(fs, aggregate, span, fields, output, PAW_RESULT_ERR, PAW_FALSE);

    return output;
}

// Given a try expression `x?`, this function performs the following
// transformation (depending on the type of `x`):
//   if (x: Option<T>)    => match x {Some(v) => v, None => return None}
//   if (x: Result<T, E>) => match x {Ok(v) => v, Err(e) => return e.into()}
static struct MirPlace lower_chain_expr(struct HirVisitor *V, struct HirChainExpr *e)
{
    _Static_assert(PAW_OPTION_SOME == PAW_RESULT_OK && PAW_OPTION_NONE == PAW_RESULT_ERR,
            "Option and Result discriminants must have the same values for success and failure variants");
    int const EXISTS = PAW_OPTION_SOME;
    int const MISSING = PAW_OPTION_NONE;

    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *target = GET_NODE_TYPE(fs->C, e->target);
    enum BuiltinKind const kind = builtin_kind(L, target);

    struct SourceSpan const expr_span = SourceSpan_from_ref(
            pawSrc_create_ref(L->C, e->span), SPAN_REF_QUESTION_MARK);

    struct MirPlace const object = lower_rvalue(V, e->target);
    struct MirPlace const discr = emit_get_field(fs, expr_span,
            object, 0, MISSING, get_builtin_type(L, BUILTIN_INT));

    MirBlock const input_bb = current_bb(fs);
    MirBlock const none_bb = new_bb(fs);
    MirBlock const after_bb = new_bb(fs);

    struct MirSwitchArmList *arms = allocate_switch_arms(fs, input_bb, 1);
    terminate_switch(fs, expr_span, discr, arms, PAW_TRUE);
    struct MirSwitchArm *arm = &K_LIST_FIRST(arms);
    arm->k = new_constant(fs, TODO, I2V(EXISTS), BUILTIN_INT).k;

    set_current_bb(fs, get_last_successor(fs));
    struct MirPlace const value = emit_get_field(fs, expr_span,
            object, 1, EXISTS, get_type(L, e->id));
    set_goto_edge(fs, expr_span, after_bb);

    set_current_bb(fs, none_bb);
    add_edge(fs, input_bb, none_bb);
    struct MirPlace const error = kind == BUILTIN_OPTION
        ? option_chain_error(fs, expr_span)
        : result_chain_error(fs, expr_span, object);
    terminate_return(fs, expr_span, error);

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

// TODO handle address of global/constant
static struct MirPlace lower_addrof(struct HirVisitor *V, struct HirExpr *target)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace const input = lower_lvalue(V, target);
    if (HirIsSelector(target)
            || HirIsIndex(target))
        return input;
    return addr_of(fs, input);
}

static struct MirPlace lower_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    if (e->op == UNARY_ADDROF)
        return lower_addrof(V, e->target);

    enum BuiltinKind const kind = kind_of_builtin(L, e->target);
    struct MirPlace const value = lower_rvalue(V, e->target);

    if (e->op == UNARY_DEREF)
        return load_from(fs, value.span, value);

    struct MirPlace const output = new_register(fs, get_type(L, e->id));
    if (!IS_BUILTIN_TYPE(kind)) return output; // must have type "!"

    enum MirUnaryOpKind const op =
        kind == BUILTIN_BOOL ? unop2op_bool(e->op) :
        kind == BUILTIN_INT ? unop2op_int(e->op) :
        unop2op_float(e->op);
    NEW_INSTR(fs, unary_op, e->span, op, value, output);
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

    NEW_INSTR(fs, binary_op, span, binop, lhs, rhs, output);
}

static struct MirPlace lower_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    enum BuiltinKind const kind = kind_of_builtin(L, e->lhs);
    struct MirPlace const output = new_register(fs, get_type(L, e->id));
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
    terminate_return(fs, fs->mir->span, result);
}

static struct MirPlace get_register(struct FunctionState *fs, int index)
{
    struct MirRegisterData const *data = mir_reg_data(fs->mir, MIR_REG(index));
    return (struct MirPlace){
        .kind = MIR_PLACE_REGISTER,
        .r = MIR_REG(index),
        .type = data->type,
    };
}

static void visit_params(struct HirVisitor *V, HirDeclList *params)
{
    // allocate a local variable for each function argument
    pawHir_visit_decl_list(V, params);
}

static struct MirPlace lower_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *outer = L->fs;
    IrType *type = pawIr_get_type(L->C, e->id);

    struct Mir *result = pawMir_new(L->C, L->pm->modno, e->span,
            SCAN_STR(L->C, PRIVATE("closure")), NULL, type, NULL,
            outer->mir->children->count, NO_DECL, FUNC_CLOSURE,
            PAW_FALSE, PAW_FALSE);

    {
        struct BlockState bs;
        struct FunctionState fs;
        MirBlock const entry = enter_function(L, &fs, &bs, result);
        MirBlock const first = new_bb(&fs);

        visit_params(L->V, e->params);
        terminate_goto(&fs, e->span);
        add_edge(&fs, entry, first);
        set_current_bb(&fs, first);

        if (HirIsBlock(e->expr)) {
            lower_function_block(L, e->expr);
        } else {
            // evaluate and return the expression
            struct MirPlace const result = lower_rvalue(V, e->expr);
            terminate_return(&fs, e->span, result);
        }
        result->upvalues = fs.up;
        leave_function(L);

        postprocess(result);
    }

    struct MirPlace const output = new_register(L->fs, type);
    NEW_INSTR(outer, closure, e->span, outer->mir->children->count, output);
    MirBodyList_push(outer->mir, outer->mir->children, result);
    return output;
}

static struct MirPlace lower_conversion_expr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    static int const NEEDS_CAST[NBUILTINS][NBUILTINS] = {
        //          to  = {0, b, c, i, f, p}
        [BUILTIN_BOOL]  = {0, 0, 1, 1, 1, 0},
        [BUILTIN_CHAR]  = {0, 1, 0, 1, 0, 0},
        [BUILTIN_INT]   = {0, 1, 1, 0, 1, 1},
        [BUILTIN_FLOAT] = {0, 1, 0, 1, 0, 0},
        [BUILTIN_PTR]   = {0, 0, 0, 1, 0, 0},
    };
    struct MirPlace const target = lower_rvalue(V, e->from);
    IrType *output_type = GET_NODE_TYPE(L->C, e->to);
    if (IrIsPtr(target.type) && IrIsPtr(output_type))
        return target;
    enum BuiltinKind const to = builtin_kind(L, output_type);
    enum BuiltinKind const from = builtin_kind(L, target.type);
    if (NEEDS_CAST[from][to]) {
        struct MirPlace const output = new_register(fs, get_type(L, e->id));
        NEW_INSTR(fs, cast, e->span, target, output, from, to);
        return output;
    }
    return target;
}

static struct MirPlace lower_variant_constructor(struct HirVisitor *V, struct HirCallExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    // set the discriminant: an "int" residing in the first Value slot of the variant
    MirPlaceList *fields = MirPlaceList_new(fs->mir);
    struct MirPlace const discr = new_constant(fs, d->span, I2V(d->index), BUILTIN_INT);
    MirPlaceList_push(fs->mir, fields, discr);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (e->args, pexpr) {
        struct MirPlace const expr = lower_rvalue(V, *pexpr);
        struct MirPlace const field = new_register(fs, expr.type);
        move_to(fs, expr.span, expr, field);
        MirPlaceList_push(fs->mir, fields, field);
    }
    struct MirPlace const output = new_register(fs, get_type(L, e->id));
    struct IrVariantDef const *variant_def = pawIr_get_variant_def(L->C, d->did);
    NEW_INSTR(fs, aggregate, e->span, fields, output, variant_def->discr, PAW_FALSE);
    return output;
}

static struct MirPlace lower_callee_and_args(struct HirVisitor *V, struct HirExpr *callee, struct HirExprList *args_in, MirPlaceList *args_out)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    paw_Bool const is_method = HirIsSelector(callee)
        && !HirGetSelector(callee)->is_index;

    struct MirPlace target;
    if (is_method) {
        IrType *fn_type = get_type(L, callee->hdr.id);

        // must be a method call since "is_index" is set to 1 for field selectors
        target = new_register(fs, fn_type);
        struct HirSelector const *select = HirGetSelector(callee);
        NEW_INSTR(fs, global, callee->hdr.span, target);

        // add context argument for method call
        IrType *self_type = K_LIST_FIRST(ir_fn_params(L->C, fn_type));
        struct MirPlace self = self_arg(fs, select->target, self_type);
        MirPlaceList_push(fs->mir, args_out, self);
    } else {
        target = lower_rvalue(V, callee);
    }

    int offset;
    struct HirExpr *const *pexpr;
    K_LIST_ENUMERATE (args_in, offset, pexpr) {
        struct MirPlace const arg_in = lower_rvalue(L->V, *pexpr);
        struct MirPlace const arg_out = new_register(fs, arg_in.type);
        move_to(fs, arg_in.span, arg_in, arg_out);
        MirPlaceList_push(L->fs->mir, args_out, arg_out);
    }

    return target;
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
    struct MirPlace const result = new_register(fs, get_type(L, e->id));
    NEW_INSTR(fs, call, e->span, target, args, result);

    if (IrIsNever(ir_fn_result(L->C, target_type))) {
        // this function never returns
        terminate_unreachable(fs, e->span);
        set_current_bb(fs, new_bb(fs));
    }
    return result;
}

static struct MirPlace lower_projection_expr(struct HirVisitor *V, struct HirProjectionExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    IrType *fn_type = get_type(L, e->id);
    struct MirPlace const target = new_register(fs, fn_type);
    NEW_INSTR(fs, global, e->span, target);
    return target;
}

static struct MirPlace lower_field_expr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    if (e->fid < 0) lower_rvalue(V, e->key);
    return lower_rvalue(V, e->value);
}

static enum LhsKind {
    LHS_POINTER,
    LHS_VALUE,
}
lower_lhs(struct HirVisitor *V, struct HirExpr *lhs, struct MirPlace *result)
{
    if (HIR_IS_UNOP(lhs, UNARY_DEREF)) {
        *result = lower_rvalue(V, HirGetUnOpExpr(lhs)->target);
    } else if (HirIsSelector(lhs) || HirIsIndex(lhs)) {
        *result = lower_lvalue(V, lhs);
    } else {
        *result = lower_lvalue(V, lhs);
        return LHS_VALUE;
    }
    return LHS_POINTER;
}

static void write_to_lhs(struct HirVisitor *V, struct SourceSpan span, struct MirPlace lhs, struct MirPlace rhs, enum LhsKind lhs_kind)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    switch (lhs_kind) {
        case LHS_VALUE:
            drop_if_necessary(fs, lhs, lhs.type);
            move_to(fs, span, rhs, lhs);
            break;
        case LHS_POINTER:
            drop_if_necessary(fs, lhs, ir_deref(lhs.type));
            store_to(fs, span, rhs, lhs);
            break;
    }
}

static struct MirPlace lower_assign_expr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace lhs;
    enum LhsKind const lhs_kind = lower_lhs(V, e->lhs, &lhs);
    struct MirPlace const rhs = lower_rvalue(V, e->rhs);
    write_to_lhs(V, e->span, lhs, rhs, lhs_kind);

    // setters are expressions that evaluate to "()"
    return unit_literal(fs, e->span);
}

static struct MirPlace lower_op_assign_expr(struct HirVisitor *V, struct HirOpAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    enum BuiltinKind const kind = kind_of_builtin(L, e->lhs);

    struct MirPlace output;
    enum LhsKind const lhs_kind = lower_lhs(V, e->lhs, &output);
    struct MirPlace const first = lhs_kind == LHS_POINTER
        ? load_from(fs, output.span, output) : output;
    struct MirPlace const second = lower_rvalue(V, e->rhs);
    struct MirPlace const temp = new_register(fs, first.type);
    new_binary_op(V, e->span, e->op, kind, first, second, temp);
    write_to_lhs(V, e->span, output, temp, lhs_kind);

    // setters are expressions that evaluate to "()"
    return unit_literal(fs, e->span);
}

static struct MirPlace lower_block(struct HirVisitor *V, struct HirBlock *e)
{
    struct BlockState bs;
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    enter_scope(fs);
    enter_block(fs, &bs, e->span, PAW_FALSE);
    pawHir_visit_stmt_list(V, e->stmts);
    struct MirPlace const result = e->result != NULL
        ? lower_rvalue(V, e->result)
        : unit_literal(fs, e->span);

    leave_block(fs);
    leave_scope(fs);
    return result;
}

static struct MirPlace lower_loop_expr(struct HirVisitor *V, struct HirLoopExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct MirPlace const result = unit_literal(fs, e->span);
    MirBlock const before_bb = current_bb(fs);
    MirBlock const header_bb = new_bb(fs);
    MirBlock const after_bb = new_bb(fs);
    add_edge(fs, before_bb, header_bb);
    terminate_goto(fs, e->span);

    struct BlockState bs;
    enter_block(fs, &bs, e->span, PAW_TRUE);

    set_current_bb(fs, header_bb);
    lower_rvalue(V, e->block);

    set_goto_edge(fs, e->span, header_bb);
    adjust_to(fs, JUMP_CONTINUE, header_bb);
    set_current_bb(fs, after_bb);

    leave_block(fs);
    return result;
}

static struct MirPlace lower_jump_expr(struct HirVisitor *V, struct HirJumpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    add_label(fs, e->span, e->jump_kind);
    set_current_bb(fs, new_bb(fs));
    return unit_literal(fs, e->span);
}

static struct MirPlace lower_return_expr(struct HirVisitor *V, struct HirReturnExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    terminate_return(fs, e->span, e->expr != NULL
            ? lower_rvalue(V, e->expr) // "return" Expr
            : unit_literal(fs, e->span)); // "return" "()"

    MirBlock const next_bb = new_bb(fs);
    set_current_bb(fs, next_bb);
    return unit_literal(fs, e->span);
}

static paw_Bool visit_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    struct MirPlace const place = lower_rvalue(V, s->expr);
    if (pawIr_needs_drop(fs->C, place.type)) {
        struct BlockState bs;
        enter_block(fs, &bs, s->expr->hdr.span, PAW_FALSE);

        struct LocalVar const local = *alloc_anon_local(fs, s->expr->hdr.span, place.type);
        move_to(fs, NODE_SPAN(s->expr), place, local.r);

        leave_block(fs);
    }

    return PAW_FALSE;
}

static struct MirPlace get_test_reg(struct FunctionState *fs, struct MatchVar v)
{
    struct MirPlace const *pr = VarPlaces_get(fs->L, fs->ms->places, v);
    paw_assert(pr != NULL);
    // TODO: this check doesn't make much sense...
    return v.deref || IrIsPtr(pr->type) ? load_from(fs, pr->span, *pr) : *pr;
}

static struct MirPlace get_binding_reg(struct FunctionState *fs, struct MatchVar v)
{
    struct MirPlace const *pr = VarPlaces_get(fs->L, fs->ms->places, v);
    paw_assert(pr != NULL);
    return v.deref ? load_from(fs, pr->span, *pr) : *pr;
}

static void declare_match_bindings(struct FunctionState *fs, struct BindingList *bindings)
{
    struct Binding const *pb;
    K_LIST_FOREACH(bindings, pb) {
        struct MirPlace const place = get_binding_reg(fs, pb->var);
        struct HirIdent const ident = {
            .span = pb->var.span,
            .name = pb->name,
        };

        struct LocalVar const local = *alloc_local(fs, ident, pb->id, place.type);
        move_to(fs, ident.span, place, local.r);
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

    struct BlockState bs;
    enter_block(fs, &bs, fs->mir->span, PAW_FALSE); // TODO: necessary?

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

    struct SourceSpan const span = NODE_SPAN(d->guard.cond);
    terminate_branch(fs, span, cond);
    set_current_bb(fs, then_bb);
    lower_match_body(V, d->guard.body, result);

    leave_block(fs); // TODO

    set_goto_edge(fs, span, join_bb);

    set_current_bb(fs, else_bb);
    visit_decision(V, d->guard.rest, result);
    set_goto_edge(fs, span, join_bb);

    set_current_bb(fs, join_bb);
}

static void lower_match_body(struct HirVisitor *V, struct MatchBody body, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct MatchState const *ms = fs->ms;
    struct SourceSpan const span = NODE_SPAN(body.result);

    MirBlock const b = new_bb(fs);
    set_goto_edge(fs, span, b);
    set_current_bb(fs, b);

    declare_match_bindings(fs, body.bindings);

    struct MirPlace const r = lower_rvalue(V, body.result);
    move_to(fs, NODE_SPAN(body.result), r, result);
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
        struct MirPlace const pointer = select_field(fs, object, is_enum + index, discr, pv->type);
        struct MirPlace value = load_from(fs, pointer.span, pointer);
//TODO if (pv->deref) value = load_from(fs, value.span, value);
        struct LocalVar const local = *alloc_anon_local(fs, (struct SourceSpan){0}, value.type);
                //TODO pv->type);
        move_to(fs, value.span, value, local.r);
        map_var_to_reg(fs, *pv, local.r);
    }

    NEW_INSTR(fs, kill, object.span, object);
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

    struct SourceSpan const span = d->multi.test.span;
    struct MirPlace const test = get_test_reg(fs, d->multi.test);
    struct MirSwitchArmList *arms = allocate_switch_arms(fs, discr_bb, cases->count);
    paw_Bool const has_otherwise = d->multi.rest != NULL;
    terminate_switch(fs, span, test, arms, has_otherwise);

    struct MatchCase const *pmc;
    struct MirSwitchArm *parm;
    MirBlock const *psucc = get_successors(fs);
    K_LIST_ZIP (cases, pmc, arms, parm) {
        set_current_bb(fs, *psucc++);
        enum BuiltinKind const kind = cons_kind(pmc->cons.kind);
        parm->k = new_constant(fs, TODO, pmc->cons.value, kind).k;

        visit_decision(V, pmc->dec, result);
        set_goto_edge(fs, span, join_bb);
    }

    // this implementation requires an "otherwise" case (binding or wildcard) to ensure
    // exhaustivness (expect for matches on values of type "bool")
    if (has_otherwise) {
        MirBlock const otherwise_bb = new_bb(fs);
        add_edge(fs, discr_bb, otherwise_bb);
        set_current_bb(fs, otherwise_bb);
        visit_decision(V, d->multi.rest, result);
        set_goto_edge(fs, span, join_bb);
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
    struct SourceSpan span = d->multi.test.span;
    struct MirPlace const variant = get_test_reg(fs, d->multi.test);
    struct MirPlace const test = emit_get_field(fs, span,
            variant, 0, 0, get_builtin_type(L, BUILTIN_INT));

    struct MirSwitchArmList *arms = allocate_switch_arms(fs, discr_bb, cases->count);
    terminate_switch(fs, span, test, arms, PAW_FALSE);

    struct MirSwitchArm *parm;
    struct MatchCase const *pmc;
    MirBlock const *psucc = get_successors(fs);
    K_LIST_ZIP (cases, pmc, arms, parm) {
        Value const discr = I2V(pmc->cons.variant.index);
        parm->k = new_constant(fs, TODO, discr, BUILTIN_INT).k;
        set_current_bb(fs, *psucc++);

        struct BlockState bs;
        enter_block(fs, &bs, fs->mir->span, PAW_FALSE);

        allocate_match_vars(fs, variant, *pmc, PAW_TRUE, pmc->cons.variant.index);
        visit_decision(V, pmc->dec, result);

        leave_block(fs);

        set_goto_edge(fs, span, join_bb);
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

    struct BlockState bs;
    enter_block(fs, &bs, fs->mir->span, PAW_FALSE);

    allocate_match_vars(fs, discr, mc, PAW_FALSE, 0);
    visit_decision(V, mc.dec, result);

    leave_block(fs);
}

static void visit_struct_case(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct MirPlace const discr = get_test_reg(fs, d->multi.test);

    paw_assert(d->multi.rest == NULL);
    paw_assert(d->multi.cases->count == 1);
    struct MatchCase const mc = K_LIST_FIRST(d->multi.cases);

    struct BlockState bs;
    enter_block(fs, &bs, fs->mir->span, PAW_FALSE);

    allocate_match_vars(fs, discr, mc, PAW_FALSE, 0);
    visit_decision(V, mc.dec, result);

    leave_block(fs);
}

static void visit_multiway(struct HirVisitor *V, struct Decision *d, struct MirPlace result)
{
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

    struct MirPlace const discr = lower_lvalue(V, e->target);
    struct LocalVar const result = *alloc_anon_local(fs, e->span, get_type(L, e->id));
    map_var_to_reg(fs, K_LIST_FIRST(ms.vars), discr);

    visit_decision(V, d, result.r);

    leave_match(fs);
    return result.r;
}

#define GENERATE_COMMON_CASES(V_, Expr_) \
        case kHirAscriptionExpr: \
            return lower_ascription_expr(V_, HirGetAscriptionExpr(Expr_)); \
        case kHirLiteralExpr: \
            return lower_literal_expr(V_, HirGetLiteralExpr(Expr_)); \
        case kHirLogicalExpr: \
            return lower_logical_expr(V_, HirGetLogicalExpr(Expr_)); \
        case kHirChainExpr: \
            return lower_chain_expr(V_, HirGetChainExpr(Expr_)); \
        case kHirUnOpExpr: \
            return lower_unop_expr(V_, HirGetUnOpExpr(Expr_)); \
        case kHirBinOpExpr: \
            return lower_binop_expr(V_, HirGetBinOpExpr(Expr_)); \
        case kHirClosureExpr: \
            return lower_closure_expr(V_, HirGetClosureExpr(Expr_)); \
        case kHirConversionExpr: \
            return lower_conversion_expr(V_, HirGetConversionExpr(Expr_)); \
        case kHirCallExpr: \
            return lower_call_expr(V_, HirGetCallExpr(Expr_)); \
        case kHirProjectionExpr: \
            return lower_projection_expr(V_, HirGetProjectionExpr(Expr_)); \
        case kHirFieldExpr: \
            return lower_field_expr(V_, HirGetFieldExpr(Expr_)); \
        case kHirAssignExpr: \
            return lower_assign_expr(V_, HirGetAssignExpr(Expr_)); \
        case kHirOpAssignExpr: \
            return lower_op_assign_expr(V_, HirGetOpAssignExpr(Expr_)); \
        case kHirReturnExpr: \
            return lower_return_expr(V_, HirGetReturnExpr(Expr_)); \
        case kHirJumpExpr: \
            return lower_jump_expr(V_, HirGetJumpExpr(Expr_)); \
        case kHirLoopExpr: \
            return lower_loop_expr(V_, HirGetLoopExpr(Expr_)); \
        case kHirMatchExpr: \
            return lower_match_expr(V_, HirGetMatchExpr(Expr_)); \
        case kHirBlock: \
            return lower_block(V_, HirGetBlock(Expr_)); \
        default: \
            PAW_UNREACHABLE();

static struct MirPlace lower_lvalue(struct HirVisitor *V, struct HirExpr *expr)
{
    switch (HIR_KINDOF(expr)) {
        case kHirSelector:
            return lower_selector(V, HirGetSelector(expr));
        case kHirIndex:
            return lower_index(V, HirGetIndex(expr));
        case kHirPathExpr: {
            return lower_path_expr(V, HirGetPathExpr(expr));
        }

        GENERATE_COMMON_CASES(V, expr);
    }
}

static struct MirPlace lower_rvalue(struct HirVisitor *V, struct HirExpr *expr)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;

    switch (HIR_KINDOF(expr)) {
        case kHirSelector:
            return load_from(fs, NODE_SPAN(expr),
                    lower_selector(V, HirGetSelector(expr)));
        case kHirIndex:
            return load_from(fs, NODE_SPAN(expr),
                    lower_index(V, HirGetIndex(expr)));
        case kHirPathExpr: {
            struct MirPlace const place = lower_path_expr(V, HirGetPathExpr(expr));
            if (mir_is_lvalue(place)) // TODO
                return load_from(fs, place.span, place);
            return place;
        }

        GENERATE_COMMON_CASES(V, expr);
    }
}

#undef GENERATE_COMMON_CASES

static void lower_hir_body_aux(struct LowerHir *L, struct HirFnDecl *fn, struct Mir *mir)
{
    struct BlockState bs;
    struct FunctionState fs;

    MirBlock const entry = enter_function(L, &fs, &bs, mir);
    MirBlock const first = new_bb(&fs);

    visit_params(L->V, fn->params);
    terminate_goto(&fs, fn->span);
    add_edge(&fs, entry, first);
    set_current_bb(&fs, first);

    lower_function_block(L, fn->body);

    leave_function(L);
}

static void validate_fn_annotations(struct LowerHir *L, struct Mir const *mir)
{
    struct Annotation a;
    if (pawP_check_extern(L->C, mir->annotations, &a)) {
        if (a.has_value && a.kind != BUILTIN_STR)
            LOWERING_ERROR(L, InvalidAnnotationType,
                    .name = a.name,
                    .span = a.span);
    }
}

static paw_Bool is_polymorphic_fn(struct LowerHir *L, struct HirFnDecl *fn)
{
    if (fn->generics != NULL)
        return PAW_TRUE;

    if (DECL_ID_EXISTS(fn->parent_id)) {
        // check for binder on parent impl block
        struct HirImplDecl *parent = HirGetImplDecl(
                pawHir_get_decl(L->hir, fn->parent_id));
        return parent->generics != NULL;
    }

    return PAW_FALSE;
}

static struct Mir *lower_hir_body(struct LowerHir *L, struct HirFnDecl *fn)
{
    IrType *type = pawIr_get_def_type(L->C, fn->did);
    struct Mir *result = pawMir_new(L->C, L->pm->modno, fn->span, fn->ident.name,
            fn->annos, type, pawIr_get_context(L->C, type), -1, fn->parent_id,
            fn->fn_kind, fn->is_pub, is_polymorphic_fn(L, fn));
    if (fn->body != NULL) {
        pawU_enter_binder(L->C->U, SCAN_STR(L->C, ""));

        validate_fn_annotations(L, result);
        lower_hir_body_aux(L, fn, result);
        postprocess(result);

        pawU_leave_binder(L->C->U);
    }
    pawP_callback(L->C, "paw.on_build_mir", result);
    return result;
}

static void register_global_constant(struct LowerHir *L, struct HirConstDecl *d, Value value, enum BuiltinKind b_kind)
{
    const int global_id = L->globals->count;
    GlobalMap_insert(L, L->globals, d->did, global_id);
    GlobalList_push(L->C, L->C->globals, (struct GlobalInfo){
        .modno = (int)d->did.modno,
        .name = d->ident.name,
        .index = global_id,
        .b_kind = b_kind,
        .value = value,
    });
}

static struct MirConstantData find_constant_result(struct Mir *mir)
{
    struct MirBlockData *const *pbb;
    K_LIST_FOREACH (mir->blocks, pbb) {
        struct MirInstruction *const *pinstr;
        K_LIST_FOREACH ((*pbb)->instructions, pinstr) {
            if (MirIsMove(*pinstr)) {
                struct MirMove const *move = MirGetMove(*pinstr);
                if (move->output.kind == MIR_PLACE_REGISTER
                        && move->output.r.value == 0) {
                    paw_assert(move->target.kind == MIR_PLACE_CONSTANT);
                    return *mir_const_data(mir, move->target.k);
                }
            }
        }
    }

    PAW_UNREACHABLE();
}

// TODO: consider reusing the same Mir object for each constant expression
static struct MirConstantData lower_constant_expression(struct LowerHir *L, struct HirExpr *expr)
{
    // artificial MIR body so that toplevel constants can be lowered normally, i.e. using
    // "lower_rvalue" routine
    IrTypeList *artificial_params = IrTypeList_new(L->C);
    IrType *artificial_result = pawIr_get_type(L->C, expr->hdr.id);
    IrType *artificial_type = pawIr_new_fn_ptr(L->C, artificial_params, artificial_result);
    struct Mir *artificial = pawMir_new(L->C, L->pm->modno, expr->hdr.span, SCAN_STR(L->C, PRIVATE("toplevel")),
            NULL, artificial_type, NULL, -1, NO_DECL, FUNC_MODULE, PAW_FALSE, PAW_FALSE);

    struct BlockState bs;
    struct FunctionState fs;
    MirBlock const entry = enter_function(L, &fs, &bs, artificial);
    MirBlock const first = new_bb(&fs);
    terminate_goto(&fs, NODE_SPAN(expr));
    add_edge(&fs, entry, first);
    set_current_bb(&fs, first);

    struct MirPlace const result = lower_rvalue(L->V, expr);
    terminate_return(&fs, NODE_SPAN(expr), result); // use variable to avoid DCE

    leave_function(L);

    // Perform constant folding (and maybe propagation) on the initializer expression. The
    // goal is to transform it into a single literal, which should always be possible, due
    // to the constantness checks performed in an earlier compilation phase.
    postprocess(artificial);

    struct MirConstantData kdata = find_constant_result(artificial);
    pawMir_free(artificial);

    return kdata;
}

static void lower_global_constant(struct LowerHir *L, struct HirConstDecl *d)
{
    // Make sure this constant hasn't already been lowered. Note that this is okay to do before
    // the cycle detection logic: if a constant has been fully evaluated already, then it must
    // not be part of a cycle. Evaluating any constant participating in a cycle will cause the
    // other constants in the cycle to be evaluated immediately, which will cause the call to
    // enter_constant_ctx to fail below.
    if (GlobalMap_get(L, L->globals, d->did) != NULL)
        return;

    // Enter module where the constant is defined. Module context must be restored before this
    // function returns.
    struct HirModule *outer = L->pm;
    L->pm = &K_LIST_AT(L->hir->modules, d->did.modno);

    // TODO: handle extern constants
    if (d->init == NULL)
        LOWERING_ERROR(L, UninitializedConstant,
                .name = d->ident.name,
                .span = d->span);

    // prevent cycles between global constants
    struct ConstantContext cctx;
    enter_constant_ctx(L, &cctx, d);

    struct MirConstantData const kdata = lower_constant_expression(L, d->init);
    register_global_constant(L, d, kdata.value, kdata.kind);

    leave_constant_ctx(L);
    L->pm = outer;
}

static void lower_global_constants(struct LowerHir *L)
{
    struct HirModule const *pmod;
    K_LIST_FOREACH (L->hir->modules, pmod) {
        struct HirDecl *const *pdecl;
        K_LIST_FOREACH (pmod->items, pdecl) {
            if (HirIsConstDecl(*pdecl))
                lower_global_constant(L, HirGetConstDecl(*pdecl));
        }
    }
}

static void lower_pending_constants(struct LowerHir *L)
{
    IrPendingConstantsIterator iter;
    IrPendingConstantsIterator_init(L->C->pending_constants, &iter);
    while (IrPendingConstantsIterator_is_valid(&iter)) {
        IrPendingConstantsIterator_key(&iter);
        struct IrPendingConstant *p = IrPendingConstantsIterator_valuep(&iter);

        struct MirConstantData const kdata = lower_constant_expression(L, p->payload);
        paw_assert(p->konst->kind == IR_CONST_PENDING);
        *p->konst = (IrConst){
            .kind = IR_CONST_VALUE,
            .value.value.i = kdata.value.i,
            .value.type = get_builtin_type(L, kdata.kind),
        };

        IrPendingConstantsIterator_next(&iter);
    }
}

static paw_Bool is_entrypoint(struct Compiler *C, DeclId did)
{
    struct IrFnDef const *def = pawIr_get_fn_def(C, did);
    if (!DECL_ID_EXISTS(def->parent)) return PAW_TRUE;
    return pawIr_get_kind(C, def->parent) != IR_TRAIT_DEF;
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
        if (HirIsFnDecl(decl) && is_entrypoint(C, decl->hdr.did)) {
            struct HirFnDecl *d = HirGetFnDecl(decl);
            L.pm = &K_LIST_AT(L.hir->modules, d->did.modno);
            struct Mir *r = lower_hir_body(&L, d);
            BodyMap_insert(C, result, d->did, r);
        }
        HirDeclMapIterator_next(&iter);
    }

    lower_pending_constants(&L);

    pawP_pool_free(C, L.pool);
    C->bodies = result;
}

