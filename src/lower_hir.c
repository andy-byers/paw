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

struct FunctionState {
    struct FunctionState *outer;
    struct MirRegisterList *registers;
    struct MirUpvalueList *up;
    struct BlockState *bs;
    struct LowerHir *L;
    paw_Bool sequential;
    MirScopeId scope_id;
    int nlocals;
    int level;
};

struct LocalVar {
    struct MirRegister *reg;
    String *name;
    DeclId did;
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
    MirBlockId *ptarget;
    MirBlockId source;
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
    paw_Env *P;

    struct MirRegister *target;
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

static struct MirRegister *new_register(struct LowerHir *L, struct IrType *type)
{
    struct MirRegisterList *regs = L->fs->registers;
    struct MirRegister *r = pawMir_new_register(L->C, regs->count, type);
    K_LIST_PUSH(L->C, regs, r);
    return r;
}

static struct MirRegister *new_literal_register(struct LowerHir *L, paw_Type code)
{
    return new_register(L, get_type(L, code));
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

static void finish_bb(struct LowerHir *L, struct MirTerminator *term)
{
    paw_assert(current_bb(L)->term == NULL);
    current_bb(L)->term = term;
}

static struct MirTerminator *finish_goto(struct LowerHir *L, MirBlockId bid)
{
    struct MirTerminator *r = pawMir_new_terminator(L->C, kMirGoto);
    MirGetGoto(r)->target = bid;
    finish_bb(L, r);
    return r;
}

static void set_current_bb(struct LowerHir *L, struct MirBlock *bb)
{
    K_LIST_PUSH(L->C, bb_list(L), bb);
}

static struct MirBlock *new_bb(struct LowerHir *L)
{
    const MirBlockId bid = {bb_list(L)->count};
    return pawMir_new_block(L->C, bid);
}

static struct MirBlock *next_bb(struct LowerHir *L)
{
    struct MirBlock *bb = new_bb(L);
    set_current_bb(L, bb);
    return bb;
}

static struct MirBlock *advance_bb(struct LowerHir *L)
{
    MirBlockId junk, *target = &junk;
    if (current_bb(L)->term == NULL) {
        struct MirTerminator *term = finish_goto(L, MIR_NO_BLOCK);
        target = &MirGetGoto(term)->target;
    }
    struct MirBlock *bb = next_bb(L);
    *target = bb->bid;
    return bb;
}

static void add_label(struct LowerHir *L, enum JumpKind kind)
{
    struct MirTerminator *term = finish_goto(L, MIR_NO_BLOCK);
    K_LIST_PUSH(L->C, L->labels, ((struct Label){
        .nvars = L->fs->nlocals,
        .source = current_bb(L)->bid,
        .ptarget = &MirGetGoto(term)->target,
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
            *lb.ptarget = current_bb(L)->bid;
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
    return needs_close;
}

static void adjust_to(struct LowerHir *L, enum JumpKind kind, MirBlockId to)
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
    K_LIST_PUSH(L->C, current_bb(L)->code, r);
    return r;
}

struct NonGlobal {
    struct MirRegister *r;
    int index;
    paw_Bool is_upvalue;
};

static struct LocalVar *get_local_slot(struct LowerHir *L, struct FunctionState *fs, int index)
{
    return &K_LIST_GET(L->stack, fs->level + index);
}

static paw_Bool resolve_local(struct LowerHir *L, struct FunctionState *fs, DeclId did, const String *name, struct NonGlobal *pinfo)
{
    for (int i = fs->nlocals - 1; i >= 0; --i) {
        struct LocalVar *item = get_local_slot(L, fs, i);
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
    for (int i = 0; i < fs->up->count; ++i) {
        struct MirUpvalueInfo up = K_LIST_GET(fs->up, i);
        if (up.index == info->index && up.is_local == is_local) {
            info->is_upvalue = PAW_TRUE;
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
    info->is_upvalue = PAW_TRUE;
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

    struct MirInstruction *enter = new_instruction(L, kMirEnterScope);
    MirGetEnterScope(enter)->scope_id = fs->scope_id;

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
    struct MirInstruction *leave = new_instruction(L, kMirLeaveScope);
    MirGetLeaveScope(leave)->scope_id = bs->scope_id;
    if (bs->outer != NULL) adjust_labels(L, bs);
    struct MirScope *scope = pawMir_get_scope(L->mir, bs->scope_id);
    scope->needs_close = needs_close;

    struct VarStack old = *L->stack;
    const int limit = L->fs->level + bs->nvars;
    L->fs->nlocals = bs->nvars;
    L->stack->count = limit;
    L->fs->bs = bs->outer;
}

static struct LocalVar *add_local(struct LowerHir *L, String *name, struct MirRegister *r, DeclId did)
{
    K_LIST_PUSH(L->C, L->stack, ((struct LocalVar){
                .name = name,
                .did = did,
                .reg = r,
            }));
    ++L->fs->nlocals;

    struct MirInstruction *instr = new_instruction(L, kMirAllocLocal);
    MirGetAllocLocal(instr)->name = name;
    MirGetAllocLocal(instr)->output = r;
    return &K_LIST_LAST(L->stack);
}

static struct LocalVar *add_local_literal(struct LowerHir *L, const char *name, struct MirRegister *r)
{
    return add_local(L, SCAN_STRING(L->C, name), r, NO_DECL);
}

static paw_Bool resolve_nonglobal(struct LowerHir *L, DeclId did, String *name, struct NonGlobal *png)
{
    if (resolve_local(L, L->fs, did, name, png)) return PAW_TRUE;
    return resolve_upvalue(L, L->fs, did, name, png);
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

static struct MirSwitchArm *add_switch_arm(struct LowerHir *L, struct MirTerminator *switch_, Value value, MirBlockId bid)
{
    struct MirSwitchArmList *arms = MirGetSwitch(switch_)->arms;
    K_LIST_PUSH(L->C, arms, ((struct MirSwitchArm){
                    .value = value,
                    .bid = bid,
                }));
    return &K_LIST_LAST(arms);
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

    struct Mir *outer = L->mir;
    L->mir = mir;

    // create the first basic block
    struct MirBlock *bb = next_bb(L);
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

static struct MirInstruction *move_to_top(struct LowerHir *L, struct MirRegister *source)
{
    struct MirInstruction *r = new_instruction(L, kMirLocal);
    L->target = new_register(L, source->type);
    MirGetLocal(r)->output = L->target;
    MirGetLocal(r)->target = source;
    return r;
}

static struct MirInstruction *add_constant(struct LowerHir *L, Value value, paw_Type code)
{
    struct MirInstruction *r = new_instruction(L, kMirConstant);
    L->target = new_register(L, get_type(L, code));
    MirGetConstant(r)->output = L->target;
    MirGetConstant(r)->value = value;
    MirGetConstant(r)->code = code;
    return r;
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
    struct MirInstruction *r = add_constant(L, e->basic.value, e->basic.t);
    L->target = MirGetConstant(r)->output;
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
            struct MirInstruction *index = add_constant(L, I2V(i), PAW_TINT);
            mir_key = MirGetConstant(index)->output;
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
// TODO: bad stuff here
    if (is_and) {
        struct MirBlock *rhs_bb = next_bb(L);
        MirGetBranch(r)->then_arm = rhs_bb->bid;
        pawHir_visit_expr(V, rhs);

        struct MirBlock *after_bb = advance_bb(L);
        MirGetBranch(r)->else_arm = after_bb->bid;
    } else {
        struct MirBlock *rhs_bb = next_bb(L);
        MirGetBranch(r)->then_arm = rhs_bb->bid;
        struct MirTerminator *finish = finish_goto(L, MIR_NO_BLOCK);

        struct MirBlock *extra_bb = next_bb(L);
        MirGetBranch(r)->else_arm = extra_bb->bid;
        pawHir_visit_expr(V, rhs);

        struct MirBlock *after_bb = advance_bb(L);
        MirGetGoto(finish)->target = after_bb->bid;
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

    struct MirInstruction *discr = add_constant(L, I2V(d->index), PAW_TINT);
    MirGetConstant(discr)->output = MirGetConstant(discr)->output;
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
    struct HirDecl *decl = pawHir_get_decl(L->C, did);
    struct NonGlobal ng;
    // TODO: The code that adds bindings to the locals stack doesn't know about DeclIds,
    //       so we have to provide the name for those cases. Figure out a better way: this
    //       is too hacky. There is a HirVarDecl for the binding, we just don't know where
    //       to find it in "declare_match_bindings". Consider finding a way to smuggle the
    //       DeclId in the match binding.
    if (resolve_nonglobal(L, did, decl->hdr.name, &ng)) {
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
    // TODO: this won't work...
    paw_assert(0);
//    struct LowerHir *L = V->ud;
//    struct MirRegister *input = operand_to_any(V, e->target);
//    struct MirInstruction *explode = new_instruction(L, kMirExplode);
//    struct MirRegisterList *exploded = pawMir_register_list_new(L->C);
//    MirGetExplode(explode)->outputs = exploded;
//    MirGetExplode(explode)->input = input;
//
//    struct MirRegister *discr = new_literal_register(L, PAW_TINT);
//    struct MirRegister *value = register_for_node(L, e->hid);
//    K_LIST_PUSH(L->C, exploded, discr);
//    K_LIST_PUSH(L->C, exploded, value);
//
//    struct MirTerminator *switch_ = finish_switch(L, discr);
//
//    struct MirBlock *some_bb = next_bb(L);
//    struct MirSwitchArm *some_arm = add_switch_arm(L, switch_, I2V(0), some_bb->bid);
//    struct MirInstruction *finalize = move_to_top(L, value);
//    struct MirTerminator *goto_ = finish_goto(L, MIR_NO_BLOCK);
//
//    struct MirBlock *none_bb = next_bb(L);
//    MirGetSwitch(switch_)->otherwise = none_bb->bid;
//    MirGetSwitch(switch_)->has_otherwise = PAW_TRUE;
//    struct MirTerminator *return_ = finish_return(L);
//    MirGetReturn(return_)->value = object;
//
//    struct MirBlock *after_bb = next_bb(L);
//    MirGetGoto(goto_)->target = after_bb->bid;
//
//    L->target = output;
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
    const paw_Type code = pawP_type2code(L->C, GET_NODE_TYPE(L->C, e->lhs));
    // OP_*CONCAT expects contiguous operands on top of the stack
    const paw_Bool is_concat = e->op == BINARY_ADD
        && (code == BUILTIN_STR || code == BUILTIN_LIST);
    struct MirRegister *lhs = is_concat ? operand_to_next(V, e->lhs) : operand_to_any(V, e->lhs);
    struct MirRegister *rhs = is_concat ? operand_to_next(V, e->rhs) : operand_to_any(V, e->rhs);
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

static struct MirRegister *first_slice_index(struct HirVisitor *V, struct HirExpr *e)
{
    if (e != NULL) return operand_to_next(V, e);
    // default to integer 0
    struct MirInstruction *r = add_constant(V->ud, I2V(0), PAW_TINT);
    return MirGetConstant(r)->output;
}

static struct MirRegister *second_slice_index(struct HirVisitor *V, struct HirExpr *e, struct MirRegister *object)
{
    if (e != NULL) return operand_to_next(V, e);
    // default to the number of elements
    struct LowerHir *L = V->ud;
    struct MirInstruction *m = move_to_top(L, object);
    struct MirInstruction *r = new_instruction(L, kMirUnaryOp);
    MirGetUnaryOp(r)->output = new_literal_register(L, PAW_TINT);
    MirGetUnaryOp(r)->val = MirGetLocal(m)->output;
    return MirGetUnaryOp(r)->output;
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
        struct MirRegister *first = first_slice_index(V, e->first);
        struct MirRegister *second = second_slice_index(V, e->second, object);
        struct MirRegister *output = register_for_node(L, e->hid);
        struct MirInstruction *r = new_instruction(L, kMirGetRange);
        MirGetGetRange(r)->output = output;
        MirGetGetRange(r)->object = object;
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
        const paw_Bool expect_found = resolve_nonglobal(L, HIR_PATH_RESULT(x->path), NULL, &ng);
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
        struct MirRegister *lower = operand_to_next(V, x->first);
        struct MirRegister *upper = operand_to_next(V, x->second);
        struct MirRegister *rhs = operand_to_next(V, e->rhs);
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

static paw_Bool is_within_loop(struct LowerHir *L)
{
    struct BlockState *bs = L->fs->bs;
    while (bs != NULL) {
        if (bs->is_loop) return PAW_TRUE;
        bs = bs->outer;
    }
    return PAW_FALSE;
}

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
    struct MirRegister *value = s->expr != NULL
        ? operand_to_any(V, s->expr) : NULL;
    struct MirTerminator *r = finish_return(L);
    MirGetReturn(r)->value = value;
    next_bb(L);
    return PAW_FALSE;
}

static struct MirBlock *patch_to_here(struct LowerHir *L, struct MirBlockList *sources)
{
    struct MirBlock *next_bb = advance_bb(L);
    for (int i = 0; i < sources->count; ++i) {
        struct MirBlock *bb = K_LIST_GET(sources, i);
        MirGetGoto(bb->term)->target = next_bb->bid;
    }
    return next_bb;
}

static void save_patch_source(struct LowerHir *L, struct MirBlockList *sources)
{
    if (current_term(L) == NULL) {
        struct MirBlock *bb = current_bb(L);
        K_LIST_PUSH(L->C, sources, bb);
        finish_goto(L, MIR_NO_BLOCK); // placeholder
    }
}

static struct MirRegister *get_test_reg(struct LowerHir *L, struct MatchVar v)
{
    const Value *pval = pawH_get(L->ms->var_mapping, I2V(v.id));
    paw_assert(pval != NULL);
    return pval->p;
}

static void declare_match_bindings(struct LowerHir *L, struct BindingList *bindings)
{
    for (int i = 0; i < bindings->count; ++i) {
        struct Binding b = K_LIST_GET(bindings, i);
        struct MirRegister *r = get_test_reg(L, b.var);
        move_to_top(L, r);

        add_local(L, b.name, r, NO_DECL);
    }
}

static MirBlockId lower_match_body(struct HirVisitor *V, struct MatchBody body);
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
    struct MirRegister *cond = operand_to_any(V, d->guard.cond);
    struct MirTerminator *branch = finish_branch(L, cond);

    MirGetBranch(branch)->then_arm = next_bb(L)->bid;
    lower_match_body(V, d->guard.body);
    save_patch_source(L, patch);

    MirGetBranch(branch)->else_arm = next_bb(L)->bid;
    visit_decision(V, d->guard.rest);

    struct MirBlock *after_bb = advance_bb(L);
    patch_to_here(L, patch);
}

static MirBlockId lower_match_body(struct HirVisitor *V, struct MatchBody body)
{
    struct LowerHir *L = V->ud;
    const MirBlockId bid = current_bb(L)->bid;
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
        struct MirRegister *r = new_register(L, K_LIST_GET(types, i));
        K_LIST_PUSH(L->C, result, r);
    }
    return result;
}

static struct MirRegister *emit_multiway_test(struct LowerHir *L, struct Decision *d)
{
    struct MirRegister *output = new_register(L, d->multi.test.type);
    struct MirInstruction *move = new_instruction(L, kMirLocal);
    add_local_literal(L, "(match discriminator)", output); // keep alive
    MirGetLocal(move)->target = get_test_reg(L, d->multi.test);
    MirGetLocal(move)->output = output;
    return output;
}

static void map_var_to_reg(struct LowerHir *L, struct MatchVar var, struct MirRegister *reg)
{
    pawH_insert(ENV(L->C), L->ms->var_mapping, I2V(var.id), P2V(reg));
}

static void allocate_match_vars(struct LowerHir *L, struct MirRegister *discr, struct MatchCase mc, paw_Bool is_enum)
{
    if (mc.vars->count == 0) return;
    struct MirRegisterList *regs = L->ms->regs;
    for (int i = 0; i < mc.vars->count; ++i) {
        struct MatchVar v = K_LIST_GET(mc.vars, i);
        struct MirRegister *r = new_register(L, v.type);
        map_var_to_reg(L, v, r);
        emit_get_field(L, discr, is_enum + i, r);
        add_local_literal(L, "(match variable)", r);
    }
}

static void visit_sparse_cases(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *test = emit_multiway_test(L, d);
    struct MirTerminator *switch_ = finish_switch(L, test);

    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    for (int i = 0; i < d->multi.cases->count; ++i) {
        struct MatchCase mc = K_LIST_GET(d->multi.cases, i);
        struct MirBlock *block = next_bb(L);

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        struct MirSwitchArm *arm = add_switch_arm(L, switch_, mc.cons.value, block->bid);
        visit_decision(V, mc.dec);
        save_patch_source(L, patch);

        leave_block(L);
    }

    // this implementation requires an "otherwise" case (binding or wildcard) to ensure
    // exhaustivness
    paw_assert(d->multi.rest != NULL);
    struct MirBlock *otherwise_bb = next_bb(L);
    MirGetSwitch(switch_)->otherwise = otherwise_bb->bid;
    MirGetSwitch(switch_)->has_otherwise = PAW_TRUE;
    visit_decision(V, d->multi.rest);
    K_LIST_PUSH(L->C, patch, current_bb(L));

    patch_to_here(L, patch);
}

static void visit_variant_cases(struct HirVisitor *V, struct Decision *d)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *variant = get_test_reg(L, d->multi.test);
    struct MirRegister *test = new_literal_register(L, PAW_TINT);
    add_local_literal(L, "(match discriminant)", test); // keep alive
    emit_get_field(L, variant, 0, test);
    struct MirTerminator *switch_ = finish_switch(L, test);

    struct CaseList *cases = d->multi.cases;
    struct MirBlockList *patch = pawMir_block_list_new(L->C);
    for (int i = 0; i < cases->count; ++i) {
        struct MatchCase mc = K_LIST_GET(cases, i);
        struct MirBlock *block = next_bb(L);

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        struct MirSwitchArm *arm = add_switch_arm(L, switch_, I2V(i), block->bid);
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
    struct MirRegister *discr = get_test_reg(L, d->multi.test);

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
    struct MirRegister *discr = get_test_reg(L, d->multi.test);

    paw_assert(d->multi.rest == NULL);
    paw_assert(d->multi.cases->count == 1);
    struct MatchCase mc = K_LIST_FIRST(d->multi.cases);
    struct MirBlock *case_bb = advance_bb(L);

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
    struct MirRegister *discr = operand_to_any(V, s->target);
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

static MirBlockId lower_branch_bb(struct HirVisitor *V, struct HirStmt *s, struct MirBlock **bb_out)
{
    struct LowerHir *L = V->ud;
    struct MirBlock *block = next_bb(V->ud);
    if (s != NULL) pawHir_visit_stmt(V, s);
    *bb_out = current_bb(L);
    return block->bid;
}

static paw_Bool visit_if_stmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    struct LowerHir *L = V->ud;
    struct MirRegister *cond = operand_to_any(V, s->cond);
    struct MirTerminator *r = finish_branch(L, cond);

    struct MirBlock *then_end, *else_end;
    MirGetBranch(r)->then_arm = lower_branch_bb(V, s->then_arm, &then_end);
    MirGetBranch(r)->else_arm = lower_branch_bb(V, s->else_arm, &else_end);
    struct MirBlock *after_bb = advance_bb(L);

    if (then_end->term == NULL) then_end->term = pawMir_new_goto(L->C, after_bb->bid);
    if (else_end->term == NULL) else_end->term = pawMir_new_goto(L->C, after_bb->bid);
    return PAW_FALSE;
}

static void lower_fornum(struct HirVisitor *V, struct HirForNum fornum, struct HirVarDecl *control, struct HirBlock *body)
{
    struct LowerHir *L = V->ud;
    struct MirBlock *prep_bb = advance_bb(L);

    struct BlockState outer;
    enter_block(L, &outer, PAW_TRUE);

    struct MirRegister *step = operand_to_next(V, fornum.step);
    struct MirRegister *end = operand_to_next(V, fornum.end);
    struct MirRegister *iter = operand_to_next(V, fornum.begin);
    struct MirRegister *var = new_literal_register(L, PAW_TINT);

    struct LocalVar *local_step = add_local_literal(L, "(for step)", step);
    struct LocalVar *local_end = add_local_literal(L, "(for end)", end);
    struct LocalVar *local_iter = add_local_literal(L, "(for iter)", end);

    struct BlockState inner;
    enter_block(L, &inner, PAW_FALSE);
    struct LocalVar *local_var = add_local(L, control->name, var, control->did);
    struct MirTerminator *skip = finish_for_loop(L, MIR_FOR_PREP, var, iter, end, step);
    struct MirBlock *top_bb = next_bb(L);
    MirGetForLoop(skip)->then_arm = top_bb->bid;
    pawHir_visit_block(V, body); // body of loop
    struct MirBlock *continue_bb = advance_bb(L);
    adjust_from(L, JUMP_CONTINUE);
    leave_block(L);

    struct MirTerminator *loop = finish_for_loop(L, MIR_FOR_LOOP, var, iter, end, step);
    MirGetForLoop(loop)->then_arm = top_bb->bid;

    struct MirBlock *after_bb = advance_bb(L);
    MirGetForLoop(skip)->else_arm = after_bb->bid;
    MirGetForLoop(loop)->else_arm = after_bb->bid;

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

    struct MirBlock *top_bb = advance_bb(L);
    pawHir_visit_block(V, s->block);

    // TODO: put this somewhere in adjust_from? that's the only reason this is here,
    //       so that this block can be targeted by a jump.
    struct MirBlock *bottom_bb = advance_bb(L);

    adjust_from(L, JUMP_CONTINUE);
    struct MirRegister *cond = operand_to_any(V, s->cond);
    struct MirTerminator *branch = finish_branch(L, cond);
    MirGetBranch(branch)->then_arm = top_bb->bid;

    struct MirBlock *after_bb = next_bb(L);
    MirGetBranch(branch)->else_arm = after_bb->bid;

    leave_block(L);
    return PAW_FALSE;
}

static paw_Bool visit_while_stmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    if (s->is_dowhile) return visit_dowhile_stmt(V, s);
    struct LowerHir *L = V->ud;

    struct BlockState bs;
    enter_block(L, &bs, PAW_TRUE);

    struct MirBlock *loop_bb = advance_bb(L);
    struct MirRegister *cond = operand_to_any(V, s->cond);
    struct MirTerminator *branch = finish_branch(L, cond);

    struct MirBlock *body_bb = next_bb(L);
    MirGetBranch(branch)->then_arm = body_bb->bid;
    pawHir_visit_block(V, s->block);
// TODO: don't we need the continue path to leave a scope block each iteration?? test that
    if (current_term(L) == NULL) finish_goto(L, loop_bb->bid);
    adjust_to(L, JUMP_CONTINUE, loop_bb->bid);
    struct MirBlock *after_bb = advance_bb(L);
    MirGetBranch(branch)->else_arm = after_bb->bid;

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

static paw_Bool is_polymorphic(const struct HirFuncDecl *func)
{
    return func->generics != NULL
        || (func->self != NULL && ir_adt_types(func->self) != NULL);
}

struct Mir *pawP_lower_hir_body(struct Compiler *C, struct HirFuncDecl *func)
{
    struct IrType *type = pawIr_get_type(C, func->hid);
    struct Mir *result = pawMir_new(C, func->name, type,
            func->self, func->fn_kind, func->body == NULL,
            func->is_pub, is_polymorphic(func));
    if (func->body == NULL) return result;

    struct LowerHir L = {
        .mir = result,
        .labels = label_list_new(C),
        .stack = var_stack_new(C),
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
    struct HirDeclList *decls = C->decls;
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

