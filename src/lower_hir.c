// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "api.h"
#include "lex.h"
#include "match.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"

// TODO
#include"stdio.h"

struct FunctionState {
    struct FunctionState *outer;
    struct MirRegisterList *registers;
    struct MirUpvalueList *up;
    struct BlockState *bs;
    struct LowerHir *L;
    paw_Bool sequential;
    int nlocals;
    int level;
};

struct LocalVar {
    struct MirRegister *reg;
    int depth;

    // TODO: store struct IrType *
    DeclId did;

    paw_Bool is_captured : 1;
};

DEFINE_LIST(struct Compiler, var_list_, VarList, struct LocalVar)

struct BlockState {
    struct BlockState *outer;
    int label0;
    int nvars;
    paw_Bool is_loop : 1;
};

struct PendingJump {
    MirBlockId from;
    int level;
};

DEFINE_LIST(struct Compiler, patch_list_, PatchList, struct PendingJump)

struct CaseState {
    struct CaseState *outer;
    struct MirRegisterList *regs;
    struct VariableList *vars;
};

struct MatchState {
    struct MatchState *outer;
    struct CaseState *cs;
};

struct Label {
    MirBlockId *ptarget;
    MirBlockId source;
    int level;
    enum JumpKind kind : 8;
    paw_Bool close : 1;
};

DEFINE_LIST(struct Compiler, label_list_, LabelList, struct Label)

struct LowerHir {
    struct HirVisitor V;
    struct Compiler *C;
    struct Mir *mir;
    Map *match_vars;
    paw_Env *P;

    struct MirRegister *target;
    struct MatchState *ms;
    struct CaseState *cs;
    struct FunctionState *fs;
    struct LabelList *labels;
    struct VarList *stack;
    struct PatchList *patch;
    int depth;
};

static void enter_match(struct LowerHir *L, struct MatchState *ms)
{
    *ms = (struct MatchState){
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

static struct MirRegister *new_register(struct LowerHir *L, struct IrType *type)
{
    struct MirRegisterList *regs = L->fs->registers;
    struct MirRegister *r = pawMir_new_register(L->C, regs->count, type);
    K_LIST_APPEND(L->C, regs, r);
    return r;
}

static struct MirBlockList *bb_list(struct LowerHir *L)
{
    paw_assert(L->mir->blocks != NULL);
    return L->mir->blocks;
}

static struct MirBlock *get_bb(struct LowerHir *L, MirBlockId bid)
{
    return bb_list(L)->data[bid.value];
}

static struct MirBlock *current_bb(struct LowerHir *L)
{
    paw_assert(bb_list(L)->count > 0);
    return K_LIST_LAST(bb_list(L));
}

static void set_current_bb(struct LowerHir *L, struct MirBlock *bb)
{
    K_LIST_PUSH(L->C, bb_list(L), bb);
}

static struct MirBlock *new_bb(struct LowerHir *L)
{
    const MirBlockId bid = {bb_list(L)->count};
    struct MirBlock *bb = pawMir_new_block(L->C, bid);
    K_LIST_PUSH(L->C, bb_list(L), bb);
    return bb;
}

static void finish_bb(struct LowerHir *L, struct MirTerminator *term)
{
    paw_assert(current_bb(L)->term == NULL);
    current_bb(L)->term = term;
}

static struct MirTerminator *finish_goto(struct LowerHir *L)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirGoto);
    finish_bb(L, r);
    return r;
}

static void add_label(struct LowerHir *L, enum JumpKind kind)
{
    struct LabelList *ll = L->labels;
    struct Label *lb = pawK_pool_alloc(ENV(L->C), L->C->pool, sizeof(struct Label));
    struct MirTerminator *term = finish_goto(L);
    MirGetGoto(term)->target.value = -1;
    *lb = (struct Label){
        .level = L->fs->nlocals - L->fs->bs->nvars,
        .ptarget = &MirGetGoto(term)->target,
        .source = current_bb(L)->bid,
        .kind = kind,
    };
    K_LIST_PUSH(L->C, ll, lb);
}

static void adjust_labels(struct LowerHir *L, struct BlockState *bs)
{
    struct LabelList *ll = L->labels;
    for (int i = bs->label0; i < ll->count; ++i) {
        struct Label *lb = K_LIST_GET(ll, i);
        lb->level = bs->nvars;
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

static void adjust_from(struct LowerHir *L, enum JumpKind kind)
{
    struct BlockState *bs = L->fs->bs;
    struct LabelList *ll = L->labels;
    for (int i = bs->label0; i < ll->count;) {
        struct Label *lb = K_LIST_GET(ll, i);
        if (lb->kind == kind) {
            *lb->ptarget = current_bb(L)->bid;
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static void adjust_to(struct LowerHir *L, enum JumpKind kind, MirBlockId to)
{
    struct BlockState *bs = L->fs->bs;
    struct LabelList *ll = L->labels;
    for (int i = bs->label0; i < ll->count;) {
        struct Label *lb = K_LIST_GET(ll, i);
        if (lb->kind == kind) {
            *lb->ptarget = to;
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static struct MirUpvalueInfo *new_upvalue_state(struct Compiler *C, uint16_t index, paw_Bool is_local)
{
    struct MirUpvalueInfo *item = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct MirUpvalueInfo));
    *item = (struct MirUpvalueInfo){
        .is_local = is_local,
        .index = index,
    };
    return item;
}

static struct LocalVar *new_scope_item(struct Compiler *C, struct MirRegister *r, int depth, DeclId did)
{
    struct LocalVar *item = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct LocalVar));
    *item = (struct LocalVar){
        .depth = depth,
        .did = did,
        .reg = r,
    };
    return item;
}

static struct MirTerminator *current_term(struct LowerHir *L)
{
    return current_bb(L)->term;
}

static paw_Bool has_return(struct LowerHir *L)
{
    return current_term(L) != NULL && MirIsReturn(current_term(L));
}

static struct MirInstruction *new_instruction(struct LowerHir *L, enum MirInstructionKind kind)
{
    struct MirInstruction *r = pawMir_new_instruction(L->C, kind);
    K_LIST_APPEND(L->C, current_bb(L)->code, r);
    return r;
}

struct NonGlobal {
    struct MirRegister *r;
    int index;
    paw_Bool is_upvalue;
};

static struct LocalVar *get_local_slot(struct LowerHir *L, struct FunctionState *fs, int index)
{
    return L->stack->data[fs->level + index];
}

static paw_Bool resolve_local(struct LowerHir *L, struct FunctionState *fs, DeclId did, struct NonGlobal *pinfo)
{
    for (int i = fs->nlocals - 1; i >= 0; --i) {
        struct LocalVar *item = get_local_slot(L, fs, i);
        if (item->did.value == did.value) {
            *pinfo = (struct NonGlobal){
                .r = item->reg,
                .index = i,
            };
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static void add_upvalue(struct LowerHir *L, struct FunctionState *fs, struct NonGlobal *info, paw_Bool is_local)
{
    for (int i = 0; i < fs->up->count; ++i) {
        struct MirUpvalueInfo *up = K_LIST_GET(fs->up, i);
        if (up->index == info->index && up->is_local == is_local) {
            info->is_upvalue = PAW_TRUE;
            info->index = i;
            return;
        }
    }
    if (fs->up->count == UPVALUE_MAX) {
        pawE_error(ENV(L->C), PAW_ESYNTAX, -1, "too many upvalues");
    }

    struct MirUpvalueInfo *up = new_upvalue_state(L->C, info->index, is_local);
    info->is_upvalue = PAW_TRUE;
    info->index = fs->up->count;
    K_LIST_PUSH(L->C, fs->up, up);
}

static paw_Bool resolve_upvalue(struct LowerHir *L, struct FunctionState *fs, DeclId did, struct NonGlobal *pinfo)
{
    struct FunctionState *caller = fs->outer;
    if (caller == NULL) return PAW_FALSE;
    if (resolve_local(L, caller, did, pinfo)) {
        get_local_slot(L, caller, pinfo->index)
            ->is_captured = PAW_TRUE;
        add_upvalue(L, fs, pinfo, PAW_TRUE);
        return PAW_TRUE;
    }
    if (resolve_upvalue(L, caller, did, pinfo)) {
        add_upvalue(L, fs, pinfo, PAW_FALSE);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void enter_block(struct LowerHir *L, struct BlockState *bs, paw_Bool is_loop)
{
    *bs = (struct BlockState){
        .label0 = L->labels->count,
        .nvars = L->fs->nlocals,
        .is_loop = is_loop,
        .outer = L->fs->bs,
    };
    L->fs->bs = bs;
}

static void leave_block(struct LowerHir *L)
{
    struct BlockState *bs = L->fs->bs;
    if (bs->is_loop) adjust_from(L, JUMP_BREAK);
//    if (bs->has_upvalue) {
//        struct MirInstruction *r = new_instruction(L, kMirClose);
//        MirGetClose(r)->level = item->reg;
//    }

    struct VarList old = *L->stack;
    const int limit = bs->nvars + L->fs->level;
    L->fs->nlocals = bs->nvars;
    L->stack->count = limit;
    L->fs->bs = bs->outer;

    // uninitialize local variables (handled by return)
    if (bs->outer == NULL) return;
    while (old.count > limit) {
        struct LocalVar *item = K_LIST_LAST(&old);
        K_LIST_POP(&old);
        struct MirInstruction *r = new_instruction(L, kMirFreeLocal);
        MirGetFreeLocal(r)->is_captured = item->is_captured;
        MirGetFreeLocal(r)->reg = item->reg;
    }

    adjust_labels(L, bs);
}

static struct LocalVar *add_local(struct LowerHir *L, String *name, struct MirRegister *r, DeclId did)
{
    struct LocalVar *item = new_scope_item(L->C, r, L->depth, did);
    K_LIST_APPEND(L->C, L->stack, item);
    ++L->fs->nlocals;

    struct MirInstruction *instr = new_instruction(L, kMirAllocLocal);
    MirGetAllocLocal(instr)->name = name;
    MirGetAllocLocal(instr)->output = r;
    return item;
}

static struct LocalVar *add_local_literal(struct LowerHir *L, const char *name, struct MirRegister *r)
{
    return add_local(L, SCAN_STRING(L->C, name), r, NO_DECL);
}

static paw_Bool resolve_nonglobal(struct LowerHir *L, DeclId did, struct NonGlobal *png)
{
    if (resolve_local(L, L->fs, did, png)) return PAW_TRUE;
    return resolve_upvalue(L, L->fs, did, png);
}

static void add_pending_jump(struct LowerHir *L, MirBlockId from, int level)
{
    struct PendingJump *item = pawK_pool_alloc(ENV(L->C), L->C->pool, sizeof(struct PendingJump));
    *item = (struct PendingJump){
        .from = from,
        .level = level,
    };
}

static void resolve_pending_jumps(struct LowerHir *L, MirBlockId to)
{
    struct PatchList *list = L->patch;
    while (list->count > 0) {
        struct PendingJump *pj = K_LIST_GET(list, list->count - 1);
        if (pj->level < L->fs->bs->nvars) break;
        --list->count;

        struct MirBlock *block = get_bb(L, pj->from);
        block->term = pawMir_new_terminator(L->C, kMirGoto);
        MirGetGoto(block->term)->target = to;
    }
}

static void terminate(struct LowerHir *L, MirBlockId bb, struct MirTerminator *term)
{
    get_bb(L, bb)->term = term;
}

struct MirTerminator *finish_return(struct LowerHir *L)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirReturn);
    finish_bb(L, r);
    return r;
}

struct MirTerminator *finish_branch(struct LowerHir *L, struct MirRegister *cond)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirBranch);
    MirGetBranch(r)->cond = cond;
    finish_bb(L, r);
    return r;
}

struct MirTerminator *finish_for_loop(struct LowerHir *L, enum MirForKind for_kind, struct MirRegister *var, struct MirRegister *iter, struct MirRegister *end, struct MirRegister *step)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirForLoop);
    MirGetForLoop(r)->for_kind = for_kind;
    MirGetForLoop(r)->var = var;
    MirGetForLoop(r)->iter = iter;
    MirGetForLoop(r)->end = end;
    MirGetForLoop(r)->step = step;
    finish_bb(L, r);
    return r;
}

struct MirTerminator *finish_switch(struct LowerHir *L, struct MirRegister *discr)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirSwitch);
    MirGetSwitch(r)->arms = pawMir_switch_list_new(L->C);
    MirGetSwitch(r)->discr = discr;
    finish_bb(L, r);
    return r;
}

static struct MirSwitchArm *add_switch_arm(struct LowerHir *L, struct MirTerminator *switch_, int value, MirBlockId bid)
{
    struct MirSwitchArm *arm = pawMir_new_switch_arm(L->C, value, bid);
    K_LIST_PUSH(L->C, MirGetSwitch(switch_)->arms, arm);
    return arm;
}

static struct MirRegister *register_for_node(struct LowerHir *L, HirId hid)
{
    return new_register(L, pawIr_get_type(L->C, hid));
}

static struct Mir *enter_function(struct LowerHir *L, struct FunctionState *fs, struct BlockState *bs, struct IrType *type, struct Mir *mir, DeclId did)
{
    *fs = (struct FunctionState){
        .registers = pawMir_register_list_new(L->C),
        .up = pawMir_upvalue_list_new(L->C),
        .level = L->stack->count,
        .outer = L->fs,
        .L = L,
    };
    L->fs = fs;
    ++L->depth;

    struct Mir *outer = L->mir;
    L->mir = mir;

    new_bb(L); // create the first basic block
    enter_block(L, bs, PAW_FALSE);

    // Slot 0 holds the function object being called, and is where the first return value will
    // eventually be placed. Prevent recursive calls from using this function object (by using
    // an invalid name), since that would cause some recursive polymorphic functions to call
    // the wrong instance. For example, fn f<A, B>(a: A, b: B) { f(b, a); }  will always call
    // itself if slot 0 is used for recursion, but that would be incorrect for instances where
    // A != B.
    struct MirRegister *r = new_register(L, type);
    add_local(L, SCAN_STRING(L->C, "(callee)"), r, did);
    return outer;
}

static void leave_function(struct LowerHir *L, struct Mir *outer)
{
    if (current_term(L) == NULL) finish_return(L);
    L->stack->count = L->fs->level;
    L->fs = L->fs->outer;
    L->mir = outer;
}

static struct MirRegister *last_register(struct LowerHir *L)
{
    return K_LIST_LAST(L->fs->registers);
}

static struct MirRegister *operand_to_any(struct HirVisitor *V, struct HirExpr *expr)
{
    struct LowerHir *L = V->ud;
    pawHir_visit_expr(V, expr);
    return L->target;
}

static struct MirRegister *operand_to_next(struct HirVisitor *V, struct HirExpr *expr)
{
    struct LowerHir *L = V->ud;
    const paw_Bool seq = L->fs->sequential;
    L->fs->sequential = PAW_TRUE;
    pawHir_visit_expr(V, expr);
    L->fs->sequential = seq;
    return L->target;
}

static void lower_operand_list_into(struct HirVisitor *V, struct HirExprList *exprs, struct MirRegisterList *result)
{
    struct LowerHir *L = V->ud;
    for (int i = 0; i < exprs->count; ++i) {
        struct MirRegister *r = operand_to_next(V, K_LIST_GET(exprs, i));
        K_LIST_APPEND(L->C, result, r);
    }
}

static struct MirRegisterList *lower_operand_list(struct HirVisitor *V, struct HirExprList *exprs)
{
    struct LowerHir *L = V->ud;
    struct MirRegisterList *result = pawMir_register_list_new(L->C);
    lower_operand_list_into(V, exprs, result);
    return result;
}

static paw_Bool visit_field_decl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    struct LowerHir *L = V->ud;
    struct IrType *type = pawIr_get_type(L->C, d->hid);
    struct MirRegister *r = new_register(L, type);
    add_local(V->ud, d->name, r, d->did);
    return PAW_FALSE;
}

static paw_Bool visit_var_decl(struct HirVisitor *V, struct HirVarDecl *d)
{
    struct LowerHir *L = V->ud;
    L->target = operand_to_next(V, d->init);
    add_local(V->ud, d->name, L->target, d->did);
    return PAW_FALSE;
}

static void lower_basic_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirConstant);
    L->target = register_for_node(L, e->hid);
    MirGetConstant(r)->output = L->target;
    MirGetConstant(r)->value = e->basic.value;
    MirGetConstant(r)->code = e->basic.t;
}

static void lower_tuple_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    struct MirRegister *output = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = output;
    MirGetAggregate(r)->nfields = e->tuple.elems->count;

    for (int i = 0; i < e->tuple.elems->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->tuple.elems, i);
        struct MirRegister *mir_field = operand_to_any(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->object = MirGetAggregate(r)->output;
        MirGetSetField(setter)->index = i;
        MirGetSetField(setter)->value = mir_field;
        MirGetSetField(setter)->is_init = PAW_TRUE;
    }

    L->target = output;
}

static void lower_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    struct MirRegister *output = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = output;
    MirGetAggregate(r)->nfields = e->comp.items->count;

    for (int i = 0; i < e->comp.items->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->comp.items, i);
        struct MirRegister *mir_field = operand_to_any(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->object = MirGetAggregate(r)->output;
        MirGetSetField(setter)->index = HirGetFieldExpr(hir_field)->fid;
        MirGetSetField(setter)->value = mir_field;
        MirGetSetField(setter)->is_init = PAW_TRUE;
    }

    L->target = output;
}

static void lower_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirContainer);
    struct MirRegister *output = register_for_node(L, e->hid);
    MirGetContainer(r)->output = output;
    MirGetContainer(r)->nelems = e->cont.items->count;

    for (int i = 0; i < e->cont.items->count; ++i) {
        struct HirExpr *hir_elem = K_LIST_GET(e->cont.items, i);
        struct MirRegister *mir_key, *mir_value;
        if (HirIsFieldExpr(hir_elem)) {
            struct HirFieldExpr *elem = HirGetFieldExpr(hir_elem);
            mir_key = operand_to_any(V, elem->key);
            mir_value = operand_to_any(V, elem->value);
        } else {
            mir_key = new_register(L, basic_type(L, PAW_TINT));
            struct MirInstruction *index = new_instruction(L, kMirConstant);
            MirGetConstant(index)->code = PAW_TINT;
            MirGetConstant(index)->output = mir_key;
            MirGetConstant(index)->value.i = i;
            mir_value = operand_to_any(V, hir_elem);
        }
        struct MirInstruction *setter = new_instruction(L, kMirSetElement);
        MirGetSetElement(setter)->key = mir_key;
        MirGetSetElement(setter)->value = mir_value;
        MirGetSetElement(setter)->object = MirGetContainer(r)->output;
        MirGetSetElement(setter)->is_init = PAW_TRUE;
    }

    L->target = output;
}

static paw_Bool visit_literal_expr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitBasic:
            lower_basic_lit(V, e);
            break;
        case kHirLitTuple:
            lower_tuple_lit(V, e);
            break;
        case kHirLitComposite:
            lower_composite_lit(V, e);
            break;
        case kHirLitContainer:
            lower_container_lit(V, e);
            break;
    }
    return PAW_FALSE;
}

static void lower_logical(struct HirVisitor *V, struct HirExpr *lhs, struct HirExpr *rhs, HirId hid, paw_Bool is_and)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *first = operand_to_any(V, lhs);
    struct MirTerminator *r = finish_branch(L, first);

    if (is_and) {
        struct MirBlock *rhs_bb = new_bb(L);
        MirGetBranch(r)->then_arm = rhs_bb->bid;
        pawHir_visit_expr(V, rhs);
        struct MirTerminator *finish = finish_goto(L);

        struct MirBlock *after_bb = new_bb(L);
        MirGetBranch(r)->else_arm = MirGetGoto(finish)->target = after_bb->bid;
    } else {
        struct MirBlock *rhs_bb = new_bb(L);
        MirGetBranch(r)->then_arm = rhs_bb->bid;
        struct MirTerminator *finish = finish_goto(L);

        struct MirBlock *extra_bb = new_bb(L);
        MirGetBranch(r)->else_arm = extra_bb->bid;
        pawHir_visit_expr(V, rhs);
        struct MirTerminator *finish2 = finish_goto(L);

        struct MirBlock *after_bb = new_bb(L);
        MirGetGoto(finish)->target = after_bb->bid;
        MirGetGoto(finish2)->target = after_bb->bid;
    }
}

static paw_Bool visit_logical_expr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    lower_logical(V, e->lhs, e->rhs, e->hid, e->is_and);
    return PAW_FALSE;
}

static paw_Bool visit_unit_struct(struct HirVisitor *V, struct HirPathExpr *e, struct HirAdtDecl *d)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    L->target = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = L->target;
    MirGetAggregate(r)->nfields = 0;
    return PAW_FALSE;
}

static paw_Bool visit_unit_variant(struct HirVisitor *V, struct HirPathExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    struct MirRegister *output = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = output;
    MirGetAggregate(r)->nfields = 1;

    struct MirInstruction *discr = new_instruction(L, kMirConstant);
    MirGetConstant(discr)->value.i = d->index;
    MirGetConstant(discr)->code = PAW_TINT;
    MirGetConstant(discr)->output = new_register(L, basic_type(L, PAW_TINT));
    struct MirInstruction *set_discr = new_instruction(L, kMirSetField);
    MirGetSetField(set_discr)->value = MirGetConstant(discr)->output;
    MirGetSetField(set_discr)->object = output;
    MirGetSetField(set_discr)->index = 0;
    MirGetSetField(set_discr)->is_init = PAW_TRUE;
    L->target = output;
    return PAW_FALSE;
}

static paw_Bool visit_path_expr(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct LowerHir *L = V->ud;
    const DeclId did = HIR_PATH_RESULT(e->path);
    struct NonGlobal ng;
    if (resolve_nonglobal(L, did, &ng)) {
        if (ng.is_upvalue) {
            L->target = register_for_node(L, e->hid);
            struct MirInstruction *r = new_instruction(L, kMirUpvalue);
            MirGetUpvalue(r)->output = L->target;
            MirGetUpvalue(r)->index = ng.index;
        } else if (L->fs->sequential) {
            L->target = register_for_node(L, e->hid);
            struct MirInstruction *r = new_instruction(L, kMirLocal);
            MirGetLocal(r)->output = L->target;
            MirGetLocal(r)->target = ng.r;
        } else {
            L->target = ng.r;
        }
        return PAW_FALSE;
    }
    struct MirRegister *output = register_for_node(L, e->hid);
    struct HirDecl *decl = pawHir_get_decl(L->C, did);
    if (HirIsVariantDecl(decl)) {
        return visit_unit_variant(V, e, HirGetVariantDecl(decl));
    } else if (HirIsAdtDecl(decl)) {
        return visit_unit_struct(V, e, HirGetAdtDecl(decl));
    }
    struct MirInstruction *r = new_instruction(L, kMirGlobal);
    MirGetGlobal(r)->output = output;
    L->target = output;
    return PAW_FALSE;
}

static void emit_get_field(struct LowerHir *L, struct MirRegister *object, int index, struct MirRegister *output)
{
    struct MirInstruction *r = new_instruction(L, kMirGetField);
    MirGetGetField(r)->output = output;
    MirGetGetField(r)->object = object;
    MirGetGetField(r)->index = index;
}

// Transformation:
//     opt?  =>  opt = if opt.0 != 0 {return opt;} else {opt.1}
static paw_Bool visit_chain_expr(struct HirVisitor *V, struct HirChainExpr *e)
{
    struct LowerHir *L = V->ud;
    // TODO: either copy into a different virtual register, or find a way to not release this register until we are actually done with it
    //       (necessary for checking discr. and also getting the value if the discr. is "Some"/"Ok")
    struct MirRegister *object = operand_to_any(V, e->target);
    struct MirRegister *output = register_for_node(L, e->hid);

    // put the discriminator in a register and switch on it
    struct MirRegister *discr = new_register(L, get_type(L, PAW_TINT));
    emit_get_field(L, object, 0, discr);
    struct MirTerminator *switch_ = finish_switch(L, discr);

    struct MirBlock *some_bb = new_bb(L);
    struct MirSwitchArm *some_arm = add_switch_arm(L, switch_, 0, some_bb->bid);
    emit_get_field(L, object, 1, output);
    struct MirTerminator *goto_ = finish_goto(L);

    struct MirBlock *none_bb = new_bb(L);
    struct MirSwitchArm *none_arm = add_switch_arm(L, switch_, 1, none_bb->bid);
    struct MirTerminator *return_ = finish_return(L);
    MirGetReturn(return_)->value = object;

    struct MirBlock *after_bb = new_bb(L);
    MirGetGoto(goto_)->target = after_bb->bid;

    L->target = output;
    return PAW_FALSE;
}

static paw_Bool visit_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *val = operand_to_any(V, e->target);
    struct MirInstruction *r = new_instruction(L, kMirUnaryOp);
    L->target = register_for_node(L, e->hid);
    MirGetUnaryOp(r)->output = L->target;
    MirGetUnaryOp(r)->op = e->op;
    MirGetUnaryOp(r)->val = val;
    return PAW_FALSE;
}

static paw_Bool visit_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *lhs = operand_to_any(V, e->lhs);
    struct MirRegister *rhs = operand_to_any(V, e->rhs);
    struct MirInstruction *r = new_instruction(L, kMirBinaryOp);
    L->target = register_for_node(L, e->hid);
    MirGetBinaryOp(r)->output = L->target;
    MirGetBinaryOp(r)->op = e->op;
    MirGetBinaryOp(r)->lhs = lhs;
    MirGetBinaryOp(r)->rhs = rhs;
    return PAW_FALSE;
}

static paw_Bool visit_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
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
        struct MirRegister *result = operand_to_any(&L->V, e->expr);
        struct MirTerminator *ret = finish_return(L);
        MirGetReturn(ret)->value = result;
    }
    result->upvalues = L->fs->up;
    leave_function(L, outer_mir);

    L->target = new_register(L, type);
    struct MirInstruction *r = new_instruction(L, kMirClosure);
    MirGetClosure(r)->child_id = L->mir->children->count;
    MirGetClosure(r)->output = L->target;
    K_LIST_PUSH(L->C, L->mir->children, result);
    return PAW_FALSE;
}

static paw_Bool visit_conversion_expr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *target = operand_to_any(V, e->arg);
    struct MirInstruction *r = new_instruction(L, kMirCast);
    L->target = register_for_node(L, e->hid);
    MirGetCast(r)->output = L->target;
    MirGetCast(r)->target = target;
    MirGetCast(r)->type = e->to;
    return PAW_FALSE;
}

static paw_Bool visit_variant_constructor(struct HirVisitor *V, struct HirCallExpr *e, struct HirVariantDecl *d)
{
    struct LowerHir *L = V->ud;
    struct MirInstruction *r = new_instruction(L, kMirAggregate);
    struct MirRegister *object = register_for_node(L, e->hid);
    MirGetAggregate(r)->output = object;
    MirGetAggregate(r)->nfields = 1 + e->args->count;

    struct MirInstruction *discr = new_instruction(L, kMirConstant);
    MirGetConstant(discr)->value.i = d->index;
    MirGetConstant(discr)->code = PAW_TINT;
    MirGetConstant(discr)->output = new_register(L, basic_type(L, PAW_TINT));

    // set the variant discriminator: an 'int' residing in the first field of the ADT
    struct MirInstruction *set_discr = new_instruction(L, kMirSetField);
    MirGetSetField(set_discr)->value = MirGetConstant(discr)->output;
    MirGetSetField(set_discr)->object = object;
    MirGetSetField(set_discr)->index = 0;
    MirGetSetField(set_discr)->is_init = PAW_TRUE;

    for (int i = 0; i < e->args->count; ++i) {
        struct HirExpr *hir_field = K_LIST_GET(e->args, i);
        struct MirRegister *mir_field = operand_to_any(V, hir_field);
        struct MirInstruction *setter = new_instruction(L, kMirSetField);
        MirGetSetField(setter)->object = object;
        MirGetSetField(setter)->index = i + 1;
        MirGetSetField(setter)->value = mir_field;
        MirGetSetField(setter)->is_init = PAW_TRUE;
    }

    L->target = object;
    return PAW_FALSE;
}

static struct MirRegister *lower_callee_and_args(struct HirVisitor *V, struct HirExpr *callee, struct HirExprList *args_in, struct MirRegisterList *args_out)
{
    struct LowerHir *L = V->ud;
    L->target = register_for_node(L, callee->hdr.hid);
    struct MirRegister *result = L->target;
    if (HirIsSelector(callee) && !HirGetSelector(callee)->is_index) {
        struct HirSelector *select = HirGetSelector(callee);
        // method call: place function object before 'self'
        struct MirInstruction *r = new_instruction(L, kMirGlobal);
        MirGetGlobal(r)->output = L->target;
        struct MirRegister *self = operand_to_next(V, select->target);
        K_LIST_PUSH(L->C, args_out, self);
    } else {
        result = operand_to_next(V, callee);
    }
    lower_operand_list_into(V, args_in, args_out);
    return result;
}

static paw_Bool visit_call_expr(struct HirVisitor *V, struct HirCallExpr *e)
{
    struct LowerHir *L = V->ud;
    struct IrType *target_type = GET_NODE_TYPE(L->C, e->target);
    if (IrIsSignature(target_type)) {
        struct HirDecl *decl = pawHir_get_decl(L->C, IR_TYPE_DID(target_type));
        if (HirIsVariantDecl(decl)) return visit_variant_constructor(V, e, HirGetVariantDecl(decl));
    }

    struct MirRegisterList *args = pawMir_register_list_new(L->C);
    struct MirRegister *target = lower_callee_and_args(V, e->target, e->args, args);
    struct MirInstruction *r = new_instruction(L, kMirCall);
    L->target = register_for_node(L, e->hid);
    MirGetCall(r)->output = L->target;
    MirGetCall(r)->target = target;
    MirGetCall(r)->args = args;
    return PAW_FALSE;
}

static paw_Bool visit_index(struct HirVisitor *V, struct HirIndex *e)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *object = operand_to_any(V, e->target);
    if (!e->is_slice) {
        struct MirRegister *first = operand_to_any(V, e->first);
        struct MirRegister *output = register_for_node(L, e->hid);
        struct MirInstruction *r = new_instruction(L, kMirGetElement);
        MirGetGetElement(r)->output = output;
        MirGetGetElement(r)->object = object;
        MirGetGetElement(r)->key = first;
        L->target = output;
    } else { // stack: .. first second ..
        struct MirRegister *first = operand_to_next(V, e->first);
        struct MirRegister *second = operand_to_next(V, e->second);
        struct MirRegister *output = register_for_node(L, e->hid);
        struct MirInstruction *r = new_instruction(L, kMirGetRange);
        MirGetGetElement(r)->output = output;
        MirGetGetRange(r)->lower = first;
        MirGetGetRange(r)->upper = second;
        L->target = output;
    }
    return PAW_FALSE;
}

static paw_Bool visit_selector(struct HirVisitor *V, struct HirSelector *e)
{
    paw_assert(e->is_index);
    struct LowerHir *L = V->ud;
    struct MirRegister *output = register_for_node(L, e->hid);
    struct MirRegister *object = operand_to_any(V, e->target);
    struct MirInstruction *r = new_instruction(L, kMirGetField);
    MirGetGetField(r)->output = output;
    MirGetGetField(r)->object = object;
    MirGetGetField(r)->index = e->index;
    L->target = output;
    return PAW_FALSE;
}

static paw_Bool visit_field_expr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    struct LowerHir *L = V->ud;
    if (e->fid < 0) operand_to_next(V, e->key);
    operand_to_next(V, e->value);
    return PAW_FALSE;
}

static paw_Bool visit_assign_expr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *target;
    if (HirIsPathExpr(e->lhs)) {
        struct NonGlobal ng;
        struct HirPathExpr *x = HirGetPathExpr(e->lhs);
        const paw_Bool expect_found = resolve_nonglobal(L, HIR_PATH_RESULT(x->path), &ng);
        paw_assert(expect_found); PAW_UNUSED(expect_found); // must be local or upvalue
        struct MirRegister *rhs = operand_to_any(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirAssign);
        target = register_for_node(L, e->hid);
        MirGetAssign(r)->is_upvalue = ng.is_upvalue;
        MirGetAssign(r)->place = ng.is_upvalue ? ng.index : ng.r->value;
        MirGetAssign(r)->rhs = rhs;
    } else if (HirIsSelector(e->lhs)) {
        struct HirSelector *x = HirGetSelector(e->lhs);
        target = operand_to_any(V, x->target);
        struct MirRegister *rhs = operand_to_any(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirSetField);
        MirGetSetField(r)->object = target;
        MirGetSetField(r)->index = x->index;
        MirGetSetField(r)->value = rhs;
    } else if (!HirGetIndex(e->lhs)->is_slice) {
        struct HirIndex *x = HirGetIndex(e->lhs);
        target = operand_to_any(V, x->target);
        struct MirRegister *key = operand_to_any(V, x->first);
        struct MirRegister *rhs = operand_to_any(V, e->rhs);
        struct MirInstruction *r = new_instruction(L, kMirSetElement);
        MirGetSetElement(r)->object = target;
        MirGetSetElement(r)->key = key;
        MirGetSetElement(r)->value = rhs;
    } else {
        struct HirIndex *x = HirGetIndex(e->lhs);
        target = operand_to_any(V, x->target);
        struct MirRegister *rhs = operand_to_any(V, e->rhs);
        struct MirRegister *lower = operand_to_next(V, x->first);
        struct MirRegister *upper = operand_to_next(V, x->second);
        struct MirInstruction *r = new_instruction(L, kMirSetRange);
        MirGetSetRange(r)->object = target;
        MirGetSetRange(r)->lower = lower;
        MirGetSetRange(r)->upper = upper;
        MirGetSetRange(r)->value = rhs;
    }
    L->target = target;
    return PAW_FALSE;
}

static void post_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    struct LowerHir *L = V->ud;
    if (!HirIsAssignExpr(s->expr)) {
        // discard an unused temporary (used during register allocation)
        struct MirInstruction *r = new_instruction(L, kMirDiscard);
        MirGetDiscard(r)->reg = last_register(L);
    }
}

static void close_vars(struct LowerHir *L, const struct BlockState *bs)
{
    for (int i = L->fs->nlocals - 1; i >= bs->nvars; --i) {
        const struct LocalVar *slot = get_local_slot(L, L->fs, i);
        struct MirInstruction *free = new_instruction(L, kMirFreeLocal);
        MirGetFreeLocal(free)->is_captured = slot->is_captured;
        MirGetFreeLocal(free)->reg = K_LIST_GET(L->fs->registers, i);
    }
}

static void close_until_loop(struct FunctionState *fs)
{
    struct BlockState *bs = fs->bs;
    while (bs->outer != NULL) {
        // Emit close instructions, but don't end any lifetimes. Code
        // paths that doesn't hit this label may still need those locals.
        struct BlockState *outer = bs->outer;
        if (outer->is_loop) {
            close_vars(fs->L, bs);
            return;
        }
        bs = outer;
    }
    pawE_error(ENV(fs->L), PAW_ESYNTAX, -1, "label outside loop");
}

static paw_Bool visit_jump_stmt(struct HirVisitor *V, struct HirJumpStmt *s)
{
    struct LowerHir *L = V->ud;
    // emit a jump, to be patched later
    add_label(L, s->jump_kind);
    new_bb(L);
    return PAW_FALSE;
}

static paw_Bool visit_return_stmt(struct HirVisitor *V, struct HirReturnStmt *s)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *value = s->expr != NULL
        ? operand_to_any(V, s->expr) : NULL;
    struct MirTerminator *r = finish_return(L);
    MirGetReturn(r)->value = value;
    new_bb(L);
    return PAW_FALSE;
}

static MirBlockId lower_match_body(struct HirVisitor *V, struct MatchBody *body)
{
    struct LowerHir *L = V->ud;
    const MirBlockId bid = current_bb(L)->bid;
    pawHir_visit_block(V, body->block);
    return bid;
}

static MirBlockId visit_decision(struct HirVisitor *V, struct Decision *d, struct MirRegister *target);

static MirBlockId visit_success(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    return lower_match_body(V, d->success.body);
}

static MirBlockId visit_guard(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    const MirBlockId guard_bid = current_bb(L)->bid;
    struct MirRegister *cond = operand_to_any(V, d->guard.cond);
    struct MirTerminator *branch = finish_branch(L, cond);

    MirGetBranch(branch)->then_arm = lower_match_body(V, d->guard.body);
    MirGetBranch(branch)->else_arm = visit_decision(V, d->guard.rest, NULL);

    // TODO: connect end of previous blocks with here if necessary
    return guard_bid;
}

//static struct MirRegister *register_match_var(struct LowerHir *L, struct MatchVar *var)
//{
//    const Value *pval = pawH_get(L->match_vars, I2V(var->id));
//    if (pval != NULL) return pval->p;
//    struct MirRegister *r = new_register(L, var->type);
//    pawH_insert(ENV(L->C), L->match_vars, I2V(var->id), P2V(r));
//    return r;
//}

static void prepend_var_bindings(struct LowerHir *L, struct BindingList *bindings, MirBlockId patch)
{

}

static void declare_match_bindings(struct LowerHir *L, struct BindingList *bindings)
{
    // TODO: declare variables, put into (virtual) registers
}

static paw_Bool is_enumerator(struct LowerHir *L, struct MirRegister *test)
{
    if (!IrIsAdt(test->type)) return PAW_FALSE;
    struct HirDecl *decl = pawHir_get_decl(L->C, IR_TYPE_DID(test->type));
    return !HirGetAdtDecl(decl)->is_struct;
}

// Determine the field index of the variable being tested
static int locate_test_field(struct LowerHir *L, struct MatchVar *test)
{
    if (L->cs == NULL) return 0;
    struct VariableList *vars = L->cs->vars;
    for (int i = 0; i < vars->count; ++i) {
        struct MatchVar *var = K_LIST_GET(vars, i);
        if (var->id == test->id) return i;
    }
    PAW_UNREACHABLE();
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
        struct MirRegister *r = new_register(L, K_LIST_GET(types, i));
        K_LIST_PUSH(L->C, result, r);
    }
    return result;
}

static void allocate_match_vars(struct LowerHir *L, struct VariableList *vars)
{

}

static struct MirRegister *emit_multiway_test(struct LowerHir *L, struct Decision *d, struct MirRegister *discr)
{
    if (L->cs != NULL && L->cs->regs->count > 0) {
        struct MatchVar *test = d->multi.test;
        const int index = locate_test_field(L, test);
        struct MirRegister *output = new_register(L, test->type);
        struct MirInstruction *getter = new_instruction(L, kMirLocal);
        MirGetLocal(getter)->target = K_LIST_GET(L->cs->regs, index);
        MirGetLocal(getter)->output = output;
        return output;
    }
    if (is_enumerator(L, discr)) {
        struct MirRegister *output = new_register(L, get_type(L, PAW_TINT));
        struct MirInstruction *getter = new_instruction(L, kMirGetField);
        MirGetGetField(getter)->output = output;
        MirGetGetField(getter)->object = discr;
        MirGetGetField(getter)->index = 0;
        return output;
    }
    return discr;
    //   TODO: do roughly the following somewhere, when the constructor variant is known (indicates number of fields)
 //   struct MirRegister *target = discr;
 //   struct MatchVar *test = d->multi.test;
 //   struct IrTypeList *fields = collect_field_types(L, test->type);
 //   if (fields != NULL) {
 //       // unpack all fields of the object being tested
 //       struct MirInstruction *explode = new_instruction(L, kMirExplode);
 //       struct MirRegisterList *exploded = allocate_registers(L, fields);
 //       MirGetExplode(explode)->outputs = exploded;
 //       MirGetExplode(explode)->input = discr;
 //       const int index = locate_test_field(L, test);
 //       target = K_LIST_GET(exploded, index);
 //   }
 //   struct MirRegister *output = new_register(L, target->type);

 //   // move the value being tested to a known location
 //   struct MirInstruction *getter = new_instruction(L, kMirLocal);
 //   MirGetLocal(getter)->output = output;
 //   MirGetLocal(getter)->target = target;
 //   return output;
}

static void enter_case(struct LowerHir *L, struct CaseState *cs, struct VariableList *vars)
{
    *cs = (struct CaseState){
        .regs = pawMir_register_list_new(L->C),
        .outer = L->cs,
        .vars = vars,
    };
    L->cs = cs;
}

static void leave_case(struct LowerHir *L)
{
    L->cs = L->cs->outer;
}

static void allocate_case_fields(struct LowerHir *L, struct MirRegister *discr, struct MatchCase *mc)
{
    if (mc->vars->count == 0) return;
    for (int i = 0; i < mc->vars->count; ++i) {
        struct MatchVar *var = K_LIST_GET(mc->vars, i);
        struct MirRegister *r = new_register(L, var->type);
        K_LIST_PUSH(L->C, L->cs->regs, r);
    }
    struct MirInstruction *explode = new_instruction(L, kMirExplode);
    MirGetExplode(explode)->outputs = L->cs->regs;
    MirGetExplode(explode)->input = discr;

    for (int i = 0; i < mc->vars->count; ++i) {
        struct MirInstruction *r = new_instruction(L, kMirAllocLocal);
        MirGetAllocLocal(r)->name = SCAN_STRING(L->C, "(match variable)");
        MirGetAllocLocal(r)->output = K_LIST_GET(L->cs->regs, i);
    }
}

static MirBlockId visit_multiway(struct HirVisitor *V, struct Decision *d, struct MirRegister *discr)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *test = emit_multiway_test(L, d, discr);
    const MirBlockId multiway_bid = current_bb(L)->bid;
    struct MirTerminator *switch_ = finish_switch(L, discr);
    MirGetSwitch(switch_)->discr = test;

    struct CaseList *cases = d->multi.cases;
    struct MirBlockList *targets = pawMir_block_list_new(L->C);
    for (int i = 0; i < cases->count; ++i) {
        struct MatchCase *mc = K_LIST_GET(cases, i);
        struct MirBlock *block = new_bb(L);

        struct CaseState cs;
        struct BlockState bs;
        enter_case(L, &cs, mc->vars);
        enter_block(L, &bs, PAW_FALSE);

        struct MirSwitchArm *arm = add_switch_arm(L, switch_, i, block->bid);
        if (mc->cons.kind == CONS_VARIANT) {
            // enumerators store their discriminant in the first value slot
            struct MirRegister *r = new_register(L, get_type(L, PAW_TINT));
            K_LIST_PUSH(L->C, L->cs->regs, r);
            arm->value = mc->cons.variant.index;
        }
        allocate_case_fields(L, discr, mc);
        visit_decision(V, mc->dec, discr);
        // if the last match arm block is dangling (doesn't have a terminator), it needs to
        // be connected to the block after the multiway branch
        if (current_term(L) == NULL) K_LIST_PUSH(L->C, targets, current_bb(L));

        leave_block(L);
        leave_case(L);
    }

    if (d->multi.rest != NULL) {
        struct MirBlock *otherwise_bb = new_bb(L);
        MirGetSwitch(switch_)->otherwise = otherwise_bb->bid;
        MirGetSwitch(switch_)->has_otherwise = PAW_TRUE;
        visit_decision(V, d->multi.rest, discr);
        K_LIST_PUSH(L->C, targets, current_bb(L));
    }

    struct MirBlock *after_bb = new_bb(L);
    for (int i = 0; i < targets->count; ++i) {
        struct MirBlock *bb = K_LIST_GET(targets, i);
        bb->term = pawMir_new_terminator(L->C, kMirGoto);
        MirGetGoto(bb->term)->target = after_bb->bid;
    }
    return multiway_bid;
}

static MirBlockId visit_decision(struct HirVisitor *V, struct Decision *d, struct MirRegister *target)
{
    struct LowerHir *L = V->ud;
    switch (d->kind) {
        case DECISION_FAILURE:
            paw_assert(0);
            return (MirBlockId){-1};
        case DECISION_SUCCESS:
            return visit_success(V, d);
        case DECISION_GUARD:
            return visit_guard(V, d);
        case DECISION_MULTIWAY:
            return visit_multiway(V, d, target);
    }
}

static paw_Bool visit_match_stmt(struct HirVisitor *V, struct HirMatchStmt *s)
{
    struct LowerHir *L = V->ud;
    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);

    struct MirRegister *discr = operand_to_any(V, s->target);
    struct LocalVar *local = add_local_literal(L, "(match target)", discr);

    struct MatchState ms;
    enter_match(L, &ms);

    struct Decision *d = pawP_check_exhaustiveness(L->C, s);

const char *xx=pawP_print_decision(L->C,d);printf("%s\n",xx);paw_pop(L->C->P,1);

    visit_decision(V, d, discr);

    leave_match(L);
    leave_block(L);
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

static MirBlockId lower_as_bb(struct HirVisitor *V, struct HirStmt *s, struct MirBlock **bb_out)
{
    struct LowerHir *L = V->ud;
    struct MirBlock *block = new_bb(V->ud);
    if (s != NULL) pawHir_visit_stmt(V, s);
    if (bb_out != NULL) *bb_out = current_bb(L);
    return block->bid;
}

static paw_Bool visit_if_stmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *cond = operand_to_any(V, s->cond);
    struct MirTerminator *r = finish_branch(L, cond);

    struct MirBlock *then_end, *else_end;
    MirGetBranch(r)->then_arm = lower_as_bb(V, s->then_arm, &then_end);
    MirGetBranch(r)->else_arm = lower_as_bb(V, s->else_arm, &else_end);
    struct MirBlock *after_bb = new_bb(L);

    if (then_end->term == NULL) then_end->term = pawMir_new_goto(L->C, after_bb->bid);
    if (else_end->term == NULL) else_end->term = pawMir_new_goto(L->C, after_bb->bid);
    return PAW_FALSE;
}

static void lower_fornum(struct HirVisitor *V, struct HirForNum fornum, struct HirVarDecl *control, struct HirBlock *body)
{
    struct LowerHir *L = V->ud;
    struct MirTerminator *before_bb = finish_goto(L);

    struct MirBlock *prep_bb = new_bb(L);
    MirGetGoto(before_bb)->target = prep_bb->bid;

    struct BlockState outer;
    enter_block(L, &outer, PAW_TRUE);

    struct MirRegister *step = operand_to_next(V, fornum.step);
    struct MirRegister *end = operand_to_next(V, fornum.end);
    struct MirRegister *iter = operand_to_next(V, fornum.begin);
    struct MirRegister *var = new_register(L, get_type(L, PAW_TINT));

    struct LocalVar *local_step = add_local_literal(L, "(for step)", step);
    struct LocalVar *local_end = add_local_literal(L, "(for end)", end);
    struct LocalVar *local_iter = add_local_literal(L, "(for iter)", end);

    struct BlockState inner;
    enter_block(L, &inner, PAW_FALSE);
    struct LocalVar *local_var = add_local(L, control->name, var, control->did);
    struct MirTerminator *skip = finish_for_loop(L, MIR_FOR_PREP, var, iter, end, step);
    struct MirBlock *top_bb = new_bb(L);
    MirGetForLoop(skip)->then_arm = top_bb->bid;
    pawHir_visit_block(V, body); // body of loop
    struct MirTerminator *bottom = finish_goto(L);
    struct MirBlock *continue_bb = new_bb(L);
    MirGetGoto(bottom)->target = continue_bb->bid;
    adjust_from(L, JUMP_CONTINUE);
    leave_block(L);

    struct MirTerminator *loop = finish_for_loop(L, MIR_FOR_LOOP, var, iter, end, step);
    MirGetForLoop(loop)->then_arm = top_bb->bid;

    struct MirBlock *next_bb = new_bb(L);
    MirGetForLoop(skip)->else_arm = next_bb->bid;
    MirGetForLoop(loop)->else_arm = next_bb->bid;

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
    struct MirTerminator *prev = finish_goto(L);

    struct BlockState bs;
    enter_block(L, &bs, PAW_TRUE);

    struct MirBlock *top_bb = new_bb(L);
    MirGetGoto(prev)->target = top_bb->bid;
    pawHir_visit_block(V, s->block);

    adjust_from(L, JUMP_CONTINUE);
    struct MirRegister *cond = operand_to_any(V, s->cond);
    struct MirTerminator *branch = finish_branch(L, cond);
    MirGetBranch(branch)->then_arm = top_bb->bid;

    struct MirBlock *next_bb = new_bb(L);
    MirGetBranch(branch)->else_arm = next_bb->bid;

    leave_block(L);
    return PAW_FALSE;
}

static paw_Bool visit_while_stmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    if (s->is_dowhile) return visit_dowhile_stmt(V, s);

    struct LowerHir *L = V->ud;
    struct MirTerminator *prev = finish_goto(L);

    struct BlockState bs;
    enter_block(L, &bs, PAW_TRUE);

    struct MirBlock *loop_bb = new_bb(L);
    MirGetGoto(prev)->target = loop_bb->bid;

    struct MirRegister *cond = operand_to_any(V, s->cond);
    struct MirTerminator *branch = finish_branch(L, cond);

    struct MirBlock *body_bb = new_bb(L);
    MirGetBranch(branch)->then_arm = body_bb->bid;
    pawHir_visit_block(V, s->block);

    if (current_term(L) == NULL) {
        struct MirTerminator *loop = finish_goto(L);
        MirGetGoto(loop)->target = loop_bb->bid;
    }

    adjust_to(L, JUMP_CONTINUE, loop_bb->bid);
    struct MirBlock *next_bb = new_bb(L);
    MirGetBranch(branch)->else_arm = next_bb->bid;

    leave_block(L);
    return PAW_FALSE;
}

void lower_hir_body(struct LowerHir *L, struct HirFuncDecl *func, struct Mir *result)
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
    struct Mir *result = pawMir_new(C, func->name, type, func->self,
            func->fn_kind, func->body == NULL, func->is_pub, func->generics != NULL);
    if (func->body == NULL) return result;

    struct LowerHir L = {
        .mir = result,
        .labels = label_list_new(C),
        .stack = var_list_new(C),
        .patch = patch_list_new(C),
        .P = ENV(C),
        .C = C,
    };
    L.match_vars = pawP_push_map(C);
    pawHir_visitor_init(&L.V, C, &L);

    L.V.VisitFieldDecl = visit_field_decl;
    L.V.VisitVarDecl = visit_var_decl;

    L.V.VisitLiteralExpr = visit_literal_expr;
    L.V.VisitLogicalExpr = visit_logical_expr;
    L.V.VisitPathExpr = visit_path_expr;
    L.V.VisitChainExpr = visit_chain_expr;
    L.V.VisitUnOpExpr = visit_unop_expr;
    L.V.VisitBinOpExpr = visit_binop_expr;
    L.V.VisitClosureExpr = visit_closure_expr;
    L.V.VisitConversionExpr = visit_conversion_expr;
    L.V.VisitCallExpr = visit_call_expr;
    L.V.VisitIndex = visit_index;
    L.V.VisitSelector = visit_selector;
    L.V.VisitFieldExpr = visit_field_expr;
    L.V.VisitAssignExpr = visit_assign_expr;

    L.V.PostVisitExprStmt = post_expr_stmt;
    L.V.VisitBlock = visit_block;
    L.V.VisitReturnStmt = visit_return_stmt;
    L.V.VisitJumpStmt = visit_jump_stmt;
    L.V.VisitIfStmt = visit_if_stmt;
    L.V.VisitForStmt = visit_for_stmt;
    L.V.VisitWhileStmt = visit_while_stmt;
    L.V.VisitMatchStmt = visit_match_stmt;

    lower_hir_body(&L, func, result);

    pawP_pop_object(C, L.match_vars);
    return result;
}

Map *pawP_lower_hir(struct Compiler *C)
{
    Map *result = pawP_push_map(C);
    struct HirDeclList *decls = C->dm->decls;
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(decls, i);
        if (!HirIsFuncDecl(decl)) continue;
        struct HirFuncDecl *d = HirGetFuncDecl(decl);
        struct Mir *r = pawP_lower_hir_body(C, d);
        pawH_insert(ENV(C), result, I2V(d->did.value), P2V(r));
    }
    // 'result' should be on top of the stack
    paw_assert(ENV(C)->top.p[-1].p == result);
    return result;
}

