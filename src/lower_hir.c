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

struct ConstantContext {
    struct ConstantContext *outer;
    DeclId did;
};

struct LocalVar {
    MirRegister reg;
    String *name;
    HirId hid;
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
    struct Pool *pool;
    struct Hir *hir;
    paw_Env *P;

    struct ConstantContext *cctx;
    struct MatchState *ms;
    struct FunctionState *fs;
    struct LabelList *labels;
    struct VarStack *stack;
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

DEFINE_LIST(struct LowerHir, VarStack, struct LocalVar)
DEFINE_LIST(struct LowerHir, LabelList, struct Label)
DEFINE_MAP(struct LowerHir, GlobalMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, int)
DEFINE_MAP(struct LowerHir, VarMap, pawP_alloc, var_hash, var_equals, struct MatchVar, MirRegister)

static void postprocess(struct LowerHir *L, struct Mir *mir)
{
    pawMir_remove_unreachable_blocks(mir);
    pawSsa_construct(mir);

    paw_Bool altered;
    do {
        altered = pawMir_propagate_constants(mir);
    } while (altered);
}

static void enter_constant_ctx(struct LowerHir *L, struct ConstantContext *cctx, int line, DeclId did)
{
    struct ConstantContext *cursor = L->cctx;
    while (cursor != NULL) {
        // TODO: report constant names
        if (did.value == cursor->did.value)
            VALUE_ERROR(L, line, "cycle detected between global constants");
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

static void enter_match(struct LowerHir *L, struct MatchState *ms, VarMap *var_mapping)
{
    *ms = (struct MatchState){
        .var_mapping = var_mapping,
        .regs = MirRegisterList_new(L->fs->mir),
        .vars = VariableList_new(L->C),
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
    return MirBlockDataList_get(bb_list(L), bb.value);
}

static MirBlock current_bb(struct LowerHir *L)
{
    paw_assert(bb_list(L)->count > 0);
    return L->fs->current;
}

static struct MirBlockData *current_bb_data(struct LowerHir *L)
{
    paw_assert(bb_list(L)->count > 0);
    return MirBlockDataList_get(bb_list(L), current_bb(L).value);
}

static MirRegister new_register(struct LowerHir *L, struct IrType *type)
{
    struct FunctionState *fs = L->fs;
    struct MirRegisterDataList *regs = fs->mir->registers;
    MirRegisterDataList_push(fs->mir, regs, (struct MirRegisterData){
                                             .type = type,
                                         });
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
    MirBlockList_push(L->fs->mir, source->successors, to);
    MirBlockList_push(L->fs->mir, target->predecessors, from);
}

static struct MirInstruction *add_instruction(struct FunctionState *fs, struct MirInstruction *instr)
{
    struct MirBlockData *block = current_bb_data(fs->L);
    MirInstructionList_push(fs->mir, block->instructions, instr);
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
    struct FunctionState *fs = L->fs;
    struct MirBlockData *bb = pawMir_new_block(fs->mir);
    MirBlockDataList_push(fs->mir, bb_list(L), bb);
    return MIR_BB(id);
}

static struct LocalVar *get_local_slot(struct FunctionState *fs, int index)
{
    return &K_LIST_AT(fs->L->stack, fs->level + index);
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
                struct LocalVar var = VarStack_get(stack, index);
                struct MirRegisterData *data = mir_reg_data(fs->mir, var.reg);
                if (data->is_captured)
                    lowest_upvalue = var.reg;
            }
        }
        if (bs->is_loop)
            break;
        bs = bs->outer;
    }
    if (needs_close)
        NEW_INSTR(fs, close, -1, lowest_upvalue);
}

static void add_label(struct LowerHir *L, int line, enum JumpKind kind)
{
    // MirClose must be added here if "continue" causes control to leave a scope containing
    // a captured variable, since the MirClose at the end of the loop body will not be reached.
    if (kind == JUMP_CONTINUE)
        close_until_loop(L->fs);
    struct MirBlockData *block = current_bb_data(L);
    terminate_goto(L, line, MIR_INVALID_BB);

    LabelList_push(L, L->labels, (struct Label){
                                     .nvars = L->fs->nlocals,
                                     .from = current_bb(L),
                                     .kind = kind,
                                 });
}

static void adjust_labels(struct LowerHir *L, struct BlockState *bs)
{
    struct LabelList *ll = L->labels;
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
        struct Label lb = LabelList_get(ll, i);
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
        struct Label lb = LabelList_get(ll, i);
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

static paw_Bool resolve_local(struct LowerHir *L, struct FunctionState *fs, String *name, struct NonGlobal *pinfo)
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
        MirCaptureList_push(fs->mir, fs->captured, (struct MirCaptureInfo){r});
        data->is_captured = PAW_TRUE;
        data->hint = r;
    }
}

static void add_upvalue(struct LowerHir *L, struct FunctionState *fs, struct NonGlobal *info, paw_Bool is_local)
{
    info->is_upvalue = PAW_TRUE;

    int index;
    struct MirUpvalueInfo *pup;
    K_LIST_ENUMERATE (fs->up, index, pup) {
        if (is_local != pup->is_local)
            continue;
        if (is_local && pup->index == info->vid) {
            info->vid = pup->index;
            info->index = index;
            return;
        }
        if (!is_local && pup->index == info->index) {
            info->index = index;
            info->vid = -1;
            return;
        }
    }
    if (fs->up->count == UPVALUE_MAX) {
        pawE_error(ENV(L->C), PAW_ESYNTAX, -1, "too many upvalues");
    }

    MirUpvalueList_push(fs->mir, fs->up, (struct MirUpvalueInfo){
                                          .index = is_local ? info->vid : info->index,
                                          .is_local = is_local,
                                      });
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
        struct LocalVar const local = VarStack_get(L->stack, L->fs->level + i);
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

static struct LocalVar *add_local(struct LowerHir *L, String *name, MirRegister r, HirId hid)
{
    VarStack_push(L, L->stack, (struct LocalVar){
                                   .vid = L->fs->locals->count,
                                   .depth = L->fs->bs->depth,
                                   .name = name,
                                   .hid = hid,
                                   .reg = r,
                               });
    MirRegisterList_push(L->fs->mir, L->fs->locals, r);
    ++L->fs->nlocals;
    return &K_LIST_LAST(L->stack);
}

static struct LocalVar *add_local_literal(struct LowerHir *L, char const *name, MirRegister r)
{
    return add_local(L, SCAN_STRING(L->C, name), r, (HirId){-1});
}

static struct LocalVar *alloc_local(struct LowerHir *L, String *name, struct IrType *type, HirId hid)
{
    MirRegister const output = new_register(L, type);
    NEW_INSTR(L->fs, alloc_local, -1, name, output);
    return add_local(L, name, output, hid);
}

// NOTE: It is easier to use the name when searching for locals and upvalues, due to the
//       way bindings in OR patterns are implemented.
static paw_Bool resolve_nonglobal(struct LowerHir *L, String *name, struct NonGlobal *png)
{
    if (!resolve_local(L, L->fs, name, png))
        return resolve_upvalue(L, L->fs, name, png);
    return PAW_TRUE;
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
    MirRegister const value = pvalue != NULL
                                  ? *pvalue
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

static MirBlock enter_function(struct LowerHir *L, struct FunctionState *fs, struct BlockState *bs, int line, struct Mir *mir)
{
    *fs = (struct FunctionState){
        .captured = mir->captured,
        .registers = mir->registers,
        .up = MirUpvalueList_new(mir),
        .level = L->stack->count,
        .locals = mir->locals,
        .outer = L->fs,
        .mir = mir,
        .L = L,
    };
    L->fs = fs;

    MirBlock const entry = new_bb(L);
    set_current_bb(L, entry);
    enter_block(L, bs, PAW_FALSE);

    alloc_local(L, SCAN_STRING(L->C, PRIVATE("result")),
                IR_FPTR(mir->type)->result, (HirId){-1});
    return entry;
}

static void leave_function(struct LowerHir *L)
{
    struct MirBlockData *block = current_bb_data(L);
    if (block->instructions->count == 0
            || !MirIsReturn(K_LIST_LAST(block->instructions)))
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
    struct MirRegisterData data = MirRegisterDataList_get(L->fs->mir->registers, source.value);
    MirRegister const target = new_register(L, data.type);
    return move_to(L, source, target);
}

static void lower_operand_list(struct HirVisitor *V, struct HirExprList *exprs, struct MirRegisterList *result)
{
    struct HirExpr **pexpr;
    struct LowerHir *L = V->ud;
    K_LIST_FOREACH (exprs, pexpr) {
        MirRegister const r = lower_operand(V, *pexpr);
        MirRegisterList_push(L->fs->mir, result, r);
    }
}

static paw_Bool visit_field_decl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    struct LowerHir *L = V->ud;
    struct IrType *type = pawIr_get_type(L->C, d->hid);
    alloc_local(V->ud, d->name, type, d->hid);
    return PAW_FALSE;
}

struct Unpacking {
    struct UnpackingList *info;
    struct LowerHir *L;
    int line;
};

struct UnpackingInfo {
    struct HirBindingPat *p;
    String *name, *temp;
    MirRegister r;
};

DEFINE_LIST(struct LowerHir, UnpackingList, struct UnpackingInfo)

static MirRegister lower_match_expr(struct HirVisitor *V, struct HirMatchExpr *e);

static String *unpack_temp_name(struct LowerHir *L, HirId hid)
{
    struct Buffer b;
    paw_Env *P = ENV(L);
    pawL_init_buffer(P, &b);

    pawL_add_char(P, &b, '(');
    pawL_add_fstring(P, &b, "%d", hid.value);
    pawL_add_char(P, &b, ')');

    String *name = SCAN_STRING(L->C, b.data);
    pawL_discard_result(P, &b);
    return name;
}

static void collect_binding(struct HirVisitor *V, struct HirBindingPat *p)
{
    struct Unpacking *ctx = V->ud;
    struct LowerHir *L = ctx->L;

    MirRegister const r = register_for_node(L, p->hid);
    String *temp = unpack_temp_name(L, p->hid);
    add_local(L, temp, r, p->hid);

    UnpackingList_push(L, ctx->info, (struct UnpackingInfo){
                .name = p->name,
                .temp = temp,
                .p = p,
                .r = r,
            });
}

static struct HirExpr *new_local_path(struct LowerHir *L, int line, String *name)
{
    struct HirPath *path = HirPath_new(L->hir);
    HirPath_add(L->hir, path, name, NULL);
    return pawHir_new_path_expr(L->hir, line, path);
}

static struct HirStmtList *create_unpackers(struct Unpacking *ctx)
{
    struct LowerHir *L = ctx->L;
    struct Hir *hir = L->hir;

    struct UnpackingInfo *pinfo;
    struct HirStmtList *setters = HirStmtList_new(hir);
    K_LIST_FOREACH(ctx->info, pinfo) {
        struct HirExpr *lhs = new_local_path(L, ctx->line, pinfo->temp);
        struct HirExpr *rhs = new_local_path(L, ctx->line, pinfo->name);
        struct HirExpr *setter = pawHir_new_assign_expr(hir, ctx->line, lhs, rhs);
        HirStmtList_push(hir, setters, pawHir_new_expr_stmt(hir, ctx->line, setter));
    }
    return setters;
}

// Performs the following transformation, the result of which is passed to the pattern
// matching compiler. As usual, the resulting match expression must be exhaustive. The
// extra temporary variables ("_a" and "_b") are required due to the fact that variable
// names are used to find registers containing locals. If "a" and "b" were assigned
// directly, then shadowing/rebinding variables would not work properly (if "rhs"
// mentioned "a" or "b", it would find uninitialized variables, rather than versions of
// "a" or "b" defined in earlier code, not to mention the fact that the bindings inside
// the match arm would need to be renamed. The code that searches for locals should use
// HirIds instead of names, however, the code that expands OR patterns would need to
// be changed to account for the fact that bindings with the same name in different
// OR clauses refer to the same variable.
//
// let (a, Adt{b}) = rhs;
// ...
//
// let _a;
// let _b;
// match rhs {
//     (a, Adt{b}) => {
//         _a = a;
//         _b = b;
//     }
// }
// let a = _a;
// let b = _b;
//
static void unpack_bindings(struct HirVisitor *V, struct HirPat *lhs, struct HirExpr *rhs)
{
    struct LowerHir *L = V->ud;
    struct Unpacking ctx = {
        .info = UnpackingList_new(L),
        .line = lhs->hdr.line,
        .L = L,
    };

    // declare temporaries for each binding that appear in the pattern
    struct HirVisitor collector;
    pawHir_visitor_init(&collector, L->C, &ctx);
    collector.PostVisitBindingPat = collect_binding;
    pawHir_visit_pat(&collector, lhs);

    // create the match expression containing an assignment to each binding
    struct Hir *hir = L->hir;
    paw_Bool const IGNORE = PAW_FALSE;
    struct HirStmtList *setters = create_unpackers(&ctx);
    struct HirExpr *unit = pawHir_new_basic_lit(hir, ctx.line, I2V(0), BUILTIN_UNIT);
    struct HirExpr *block = pawHir_new_block(hir, ctx.line, setters, unit, IGNORE);
    struct HirExprList *arms = HirExprList_new(hir);
    struct HirExpr *arm = pawHir_new_match_arm(hir, ctx.line, lhs, NULL, block, IGNORE);
    HirExprList_push(hir, arms, arm);
    struct HirExpr *match = pawHir_new_match_expr(hir, ctx.line, rhs, arms, IGNORE);

    lower_match_expr(V, HirGetMatchExpr(match));

    struct UnpackingInfo *pinfo;
    K_LIST_FOREACH(ctx.info, pinfo) {
        struct HirBindingPat *p = pinfo->p;
        MirRegister const r = register_for_node(L, p->hid);
        add_local(L, p->name, r, p->hid);
        move_to(L, pinfo->r, r);
    }
}

static paw_Bool visit_var_decl(struct HirVisitor *V, struct HirVarDecl *d)
{
    struct LowerHir *L = V->ud;
    struct IrType *type = pawIr_get_type(L->C, d->hid);

    MirRegister r;
    if (d->init != NULL) {
        unpack_bindings(V, d->pat, d->init);
    } else if (HirIsBindingPat(d->pat)) {
        String *const name = HirGetBindingPat(d->pat)->name;
        struct LocalVar *var = alloc_local(L, name, type, d->hid);
        struct MirRegisterData *data = mir_reg_data(L->fs->mir, var->reg);
        data->is_uninit = PAW_TRUE;
        r = var->reg;
    } else {
        VALUE_ERROR(L, d->line, "variables using deferred initialization cannot use destructuring");
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
    K_LIST_ENUMERATE (e->tuple.elems, index, pexpr) {
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
    K_LIST_FOREACH (e->comp.items, pexpr) {
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
    K_LIST_ENUMERATE (e->cont.items, index, pexpr) {
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

// Routine for lowering a global constant
static void lower_global_constant(struct LowerHir *L, struct HirConstDecl *d);

static MirRegister lookup_global_constant(struct LowerHir *L, struct HirConstDecl *d)
{
    int const *pid = GlobalMap_get(L, L->globals, d->did);
    if (pid != NULL) {
        struct GlobalInfo info = GlobalList_get(L->C->globals, *pid);
        return add_constant(L, info.value, info.b_kind);
    }
    lower_global_constant(L, d);
    return lookup_global_constant(L, d);
}

static MirRegister lower_path_expr(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct LowerHir *L = V->ud;
    struct FunctionState *fs = L->fs;
    struct NonGlobal ng;
    if (resolve_nonglobal(L, K_LIST_LAST(e->path).name, &ng)) {
        MirRegister const output = register_for_node(L, e->hid);
        if (ng.is_upvalue) {
            NEW_INSTR(fs, upvalue, e->line, output, ng.index);
        } else {
            move_to(L, ng.r, output);
        }
        return output;
    }
    struct HirResult const res = HIR_PATH_RESULT(e->path);
    paw_assert(res.kind == HIR_RESULT_DECL);
    struct HirDecl *decl = pawHir_get_decl(L->C, res.did);
    MirRegister const output = register_for_node(L, e->hid);

    if (HirIsVariantDecl(decl)) {
        return lower_unit_variant(V, e, HirGetVariantDecl(decl));
    } else if (HirIsAdtDecl(decl)) {
        return lower_unit_struct(V, e, HirGetAdtDecl(decl));
    } else if (HirIsConstDecl(decl)) {
        return lookup_global_constant(L, HirGetConstDecl(decl));
    }
    int const *pid = GlobalMap_get(L, L->globals, res.did);
    NEW_INSTR(fs, global, e->line, output, pid != NULL ? *pid : -1);
    return output;
}

static void emit_get_field(struct LowerHir *L, int line, MirRegister object, int index, MirRegister output)
{
    struct FunctionState *fs = L->fs;
    NEW_INSTR(fs, get_field, line, index, output, object);
}

static struct MirSwitchArmList *allocate_switch_arms(struct LowerHir *L, MirBlock discr_bb, int count)
{
    struct FunctionState *fs = L->fs;
    struct MirSwitchArmList *arms = MirSwitchArmList_new(fs->mir);
    MirSwitchArmList_reserve(fs->mir, arms, count);
    for (int i = 0; i < count; ++i) {
        MirBlock const case_bb = new_bb(L);
        add_edge(L, discr_bb, case_bb);
        MirSwitchArmList_push(fs->mir, arms, (struct MirSwitchArm){
                                              .bid = case_bb,
                                          });
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
    struct MirSwitchArm *arm = &K_LIST_AT(arms, 0);

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

    String *name = SCAN_STRING(L->C, PRIVATE("closure"));
    struct Mir *result = pawMir_new(L->C, name, type, NULL, FUNC_CLOSURE, PAW_FALSE, PAW_FALSE);

    struct BlockState bs;
    struct FunctionState fs;
    MirBlock const entry = enter_function(L, &fs, &bs, e->line, result);
    MirBlock const first = new_bb(L);

    pawHir_visit_decl_list(&L->V, e->params);
    terminate_goto(L, e->line, first);
    add_edge(L, entry, first);
    set_current_bb(L, first);

    if (HirIsBlock(e->expr)) {
        lower_function_block(L, e->expr);
    } else {
        // evaluate and return the expression
        MirRegister const result = lower_operand(&L->V, e->expr);
        terminate_return(L, &result);
    }
    result->upvalues = L->fs->up;
    leave_function(L);

    postprocess(L, result);

    MirRegister const output = new_register(L, type);
    struct MirBodyList *children = outer->mir->children;
    NEW_INSTR(outer, closure, e->line, children->count, output);
    MirBodyList_push(outer->mir, children, result);
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
    K_LIST_ENUMERATE (e->args, index, pexpr) {
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
        struct MirInstruction *instr = NEW_INSTR(fs, global, callee->hdr.line, result, -1);
        struct MirGlobal *global = MirGetGlobal(instr);

        MirRegister const self = lower_operand(V, select->target);
        MirRegisterList_push(fs->mir, args_out, self);

        recv = GET_NODE_TYPE(L->C, select->target);
    } else {
        result = lower_operand(V, callee);
        if (HirIsPathExpr(callee)) {
            struct HirPath *path = HirGetPathExpr(callee)->path;
            if (path->count > 1) {
                // "seg" is the owner of the associated function
                struct HirSegment seg = HirPath_get(path, path->count - 2);
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

    struct MirRegisterList *args = MirRegisterList_new(fs->mir);
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
        paw_Bool const found = resolve_nonglobal(L, K_LIST_LAST(x->path).name, &ng);
        if (!found) // must be a global constant
            VALUE_ERROR(L, e->line, "assignment to 'const' item");
        if (ng.is_upvalue) {
            MirRegister const rhs = lower_operand(V, e->rhs);
            NEW_INSTR(fs, set_upvalue, e->line, ng.index, rhs);
        } else {
            MirRegister const value = lower_operand(V, e->rhs);
            NEW_INSTR(fs, move, e->line, ng.r, value);
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
    MirRegister const *pr = VarMap_get(L, L->ms->var_mapping, v);
    paw_assert(pr != NULL);
    return *pr;
}

static void declare_match_bindings(struct LowerHir *L, struct BindingList *bindings)
{
    for (int i = 0; i < bindings->count; ++i) {
        struct Binding b = BindingList_get(bindings, i);
        MirRegister const r = get_test_reg(L, b.var);
        into_fresh_reg(L, r);

        add_local(L, b.name, r, b.hid);
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
        return IrTypeList_get(tuple->elems, index);
    }
    struct HirDecl *decl = pawHir_get_decl(L->C, IR_TYPE_DID(type));
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    struct HirDecl *field = HirDeclList_get(d->fields, index);
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
    struct HirAdtDecl *d = HirGetAdtDecl(pawHir_get_decl(L->C, IR_TYPE_DID(type)));
    struct IrTypeList *result = IrTypeList_new(L->C);
    IrTypeList_reserve(L->C, result, !d->is_struct + d->fields->count);

    if (!d->is_struct) {
        // first field of an enumerator is the integer discriminant
        IrTypeList_push(L->C, result, get_type(L, BUILTIN_INT));
    }
    struct HirDecl *const *pfield_decl;
    K_LIST_FOREACH (d->fields, pfield_decl) {
        struct IrType *field_type = pawP_instantiate_field(L->C, type, *pfield_decl);
        IrTypeList_push(L->C, result, field_type);
    }
    return result;
}

static struct MirRegisterList *allocate_registers(struct LowerHir *L, struct IrTypeList *types)
{
    struct FunctionState *fs = L->fs;
    struct MirRegisterList *result = MirRegisterList_new(fs->mir);
    MirRegisterList_reserve(fs->mir, result, types->count);

    struct IrType *const *ptype;
    K_LIST_FOREACH (types, ptype) {
        MirRegister const r = new_register(L, *ptype);
        MirRegisterList_push(fs->mir, result, r);
    }
    return result;
}

static void map_var_to_reg(struct LowerHir *L, struct MatchVar var, MirRegister reg)
{
    VarMap_insert(L, L->ms->var_mapping, var, reg);
}

static void allocate_match_vars(struct LowerHir *L, MirRegister discr, struct MatchCase mc, paw_Bool is_enum)
{
    if (mc.vars->count == 0)
        return;

    int index;
    struct MatchVar const *pv;
    struct MirRegisterList *regs = L->ms->regs;
    K_LIST_ENUMERATE (mc.vars, index, pv) {
        MirRegister const r = new_register(L, pv->type);
        map_var_to_reg(L, *pv, r);
        emit_get_field(L, -1, discr, is_enum + index, r);
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

    struct MatchCase const *pmc;
    struct MirSwitchArm *parm;
    K_LIST_ZIP (cases, pmc, arms, parm) {
        set_current_bb(L, parm->bid);
        parm->value = pmc->cons.value;

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        visit_decision(V, pmc->dec, result);
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

    struct MatchCase const *pmc;
    struct MirSwitchArm *parm;
    K_LIST_ZIP (cases, pmc, arms, parm) {
        parm->value.i = pmc->cons.variant.index;
        set_current_bb(L, parm->bid);

        struct BlockState bs;
        enter_block(L, &bs, PAW_FALSE);

        allocate_match_vars(L, variant, *pmc, PAW_TRUE);
        visit_decision(V, pmc->dec, result);
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
    VarMap *var_mapping = VarMap_new(L);

    struct BlockState bs;
    struct MatchState ms;
    enter_block(L, &bs, PAW_FALSE);
    enter_match(L, &ms, var_mapping);

    struct Decision *d = pawP_check_exhaustiveness(L->hir, L->pool, e, ms.vars);
    paw_assert(ms.vars->count > 0);
    MirRegister const discr = lower_operand(V, e->target);
    MirRegister const result = register_for_node(L, e->hid);
    map_var_to_reg(L, K_LIST_FIRST(ms.vars), discr);
    add_local_literal(L, "(match target)", discr);
    MirRegisterList_push(L->fs->mir, ms.regs, discr);

    visit_decision(V, d, result);

    leave_match(L);
    leave_block(L);

    VarMap_delete(L, var_mapping);
    return result;
}

static void lower_hir_body_aux(struct LowerHir *L, struct HirFuncDecl *func, struct Mir *mir)
{
    struct BlockState bs;
    struct FunctionState fs;
    MirBlock const entry = enter_function(L, &fs, &bs, func->line, mir);
    MirBlock const first = new_bb(L);

    pawHir_visit_decl_list(&L->V, func->params);
    terminate_goto(L, func->line, first);
    add_edge(L, entry, first);
    set_current_bb(L, first);

    lower_function_block(L, func->body);

    leave_function(L);
}

static struct Mir *lower_hir_body(struct LowerHir *L, struct Hir *hir, struct HirFuncDecl *func)
{
    L->hir = hir;

    struct IrType *type = pawIr_get_type(L->C, func->hid);
    struct IrSignature *fsig = IrGetSignature(type);
    paw_Bool const is_polymorphic = func->generics != NULL
            || (fsig->self != NULL && IR_TYPE_SUBTYPES(fsig->self) != NULL);
    struct Mir *result = pawMir_new(L->C, func->name, type, fsig->self,
            func->fn_kind, func->is_pub, is_polymorphic);
    if (func->body == NULL)
        return result;

    lower_hir_body_aux(L, func, result);
    postprocess(L, result);

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
                                             .name = d->name,
                                             .index = global_id,
                                             .modno = d->did.modno,
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

    // attempt to find the constant in the pre-registered symbol table
    struct Annotation anno;
    if (pawP_check_extern(L->C, d->annos, &anno)) {
        if (d->init != NULL)
            VALUE_ERROR(L, d->line, "unexpected initializer for extern constant '%s'", d->name->text);

        struct ModuleInfo *m = ModuleList_get(L->C->modules, d->did.modno);
        String *modname = d->did.modno <= 1 ? NULL : m->hir->name;
        String *name = pawP_mangle_name(L->C, modname, d->name, NULL);
        Value const value = pawP_get_extern_value(L->C, name);
        struct IrType *type = pawIr_get_type(L->C, d->hid);
        enum BuiltinKind const kind = pawP_type2code(L->C, type);
        register_global_constant(L, d, value, kind);
        return;
    }
    if (d->init == NULL)
        VALUE_ERROR(L, d->line, "missing initializer for constant '%s'", d->name->text);

    // artificial MIR body so that toplevel constants can be lowered normally, i.e. using
    // "lower_operand" routine
    struct IrTypeList *artificial_params = IrTypeList_new(L->C);
    struct IrType *artificial_result = pawIr_get_type(L->C, (HirId){0});
    struct IrType *artificial_type = pawIr_new_func_ptr(L->C, artificial_params, artificial_result);
    struct Mir *artificial = pawMir_new(L->C, SCAN_STRING(L->C, PRIVATE("toplevel")), artificial_type,
                                        NULL, FUNC_MODULE, PAW_FALSE, PAW_FALSE);

    // prevent cycles between global constants
    struct ConstantContext cctx;
    enter_constant_ctx(L, &cctx, d->line, d->did);

    struct BlockState bs;
    struct FunctionState fs;
    MirBlock const entry = enter_function(L, &fs, &bs, 0, artificial);
    MirBlock const first = new_bb(L);
    terminate_goto(L, d->line, first);
    add_edge(L, entry, first);
    set_current_bb(L, first);

    MirRegister const result = lower_operand(&L->V, d->init);
    terminate_return(L, &result); // use variable to avoid DCE

    leave_constant_ctx(L);
    leave_function(L);

    // Perform constant folding (and maybe propagation) on the initializer expression. The
    // goal is to transform it into a single literal, which should always be possible, due
    // to the constantness checks performed in an earlier compilation phase.
    postprocess(L, artificial);

    // after SCCP and DCE, the constant result will be in register 1
    paw_assert(artificial->blocks->count == 2); // entry and first block
    struct MirBlockData *bb = MirBlockDataList_get(artificial->blocks, 1);
    paw_assert(bb->instructions->count == 2); // Constant, and, Return
    struct MirConstant *k = MirGetConstant(K_LIST_FIRST(bb->instructions));

    register_global_constant(L, d, k->value, k->b_kind);

    pawMir_free(artificial);
}

static void lower_global_constants(struct LowerHir *L)
{
    // resolve constants, making sure there are no cyclic dependencies
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (L->C->decls, pdecl) {
        if (HirIsConstDecl(*pdecl))
            lower_global_constant(L, HirGetConstDecl(*pdecl));
    }
}

BodyMap *pawP_lower_hir(struct Compiler *C)
{
    BodyMap *result = BodyMap_new(C);

    struct LowerHir L = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .hir = C->hir_prelude,
        .P = ENV(C),
        .C = C,
    };
    L.globals = GlobalMap_new(&L);
    L.labels = LabelList_new(&L);
    L.stack = VarStack_new(&L);

    pawHir_visitor_init(&L.V, C, &L);
    L.V.VisitFieldDecl = visit_field_decl;
    L.V.VisitVarDecl = visit_var_decl;
    L.V.VisitExprStmt = visit_expr_stmt;

    lower_global_constants(&L);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (C->decls, pdecl) {
        if (HirIsFuncDecl(*pdecl)) {
            struct IrType *type = GET_NODE_TYPE(C, *pdecl);
            struct IrSignature *fsig = IrGetSignature(type);
            if (fsig->self == NULL || IrIsAdt(fsig->self)) {
                struct HirFuncDecl *d = HirGetFuncDecl(*pdecl);
                struct ModuleInfo *m = ModuleList_get(C->modules, d->did.modno);
                struct Mir *r = lower_hir_body(&L, m->hir, d);
                BodyMap_insert(C, result, d->did, r);
            }
        }
    }

    pawP_pool_free(C, L.pool);
    return result;
}
