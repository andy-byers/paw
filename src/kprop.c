// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// kprop.c: Perform constant and copy propagation
//
// Uses the sparse conditional constant (SCC) algorithm from (1) for constant
// propagation.
//
// References:
// (1) Wegman, M., & Zadeck, F. K. (1991). Constant Propagation with Conditional
//     Branches.

#include "error.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include <math.h>

#define KPROP_ERROR(K_, Kind_, ...) pawErr_##Kind_((K_)->C, (K_)->mir->modname, __VA_ARGS__)

enum CellKind {
    CELL_TOP,
    CELL_BOTTOM,
    CELL_CONSTANT,
};

struct Cell {
    struct CellInfo {
        enum CellKind kind : 8;
        Value v;
        int k;
    } info;

    struct IrType *type;
    MirRegister r;
};

struct SsaEdge {
    MirBlock b;
    MirRegister def;
    struct MirInstruction *use;
};

struct FlowEdge {
    MirBlock from;
    MirBlock to;
};

struct ExecFlag {
    MirBlock from;
    paw_Bool exec : 1;
};

#define SSA_EDGE(def, use, b) ((struct SsaEdge){b, def, use})
#define FLOW_EDGE(from, to) ((struct FlowEdge){from, to})
#define EXEC_FLAG(from, exec) ((struct ExecFlag){from, exec})

struct KProp {
    struct FlowWorklist *flow;
    struct SsaWorklist *ssa;
    struct Lattice *lattice;
    struct KCache kcache;
    struct GotoMap *gotos;
    struct ExecMap *exec;
    AccessMap *uses;
    int nk;

    paw_Bool altered;

    struct Pool *pool;
    struct Mir *mir;
    struct Compiler *C;
    paw_Env *P;
};

DEFINE_LIST(struct KProp, FlowWorklist, struct FlowEdge)
DEFINE_LIST(struct KProp, SsaWorklist, struct SsaEdge)
DEFINE_LIST(struct KProp, ExecList, struct ExecFlag)
DEFINE_LIST(struct KProp, Lattice, struct Cell)

DEFINE_MAP(struct KProp, UseCountMap, pawP_alloc, MIR_ID_HASH, MIR_ID_EQUALS, MirRegister, int)
DEFINE_MAP(struct KProp, GotoMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, struct MirInstruction *, MirBlock)
DEFINE_MAP(struct KProp, ExecMap, pawP_alloc, MIR_ID_HASH, MIR_ID_EQUALS, MirBlock, struct ExecList *)

#if defined(PAW_DEBUG_EXTRA)

#include <stdio.h>
void dump_lattice(struct KProp *K)
{
    int index;
    struct Cell *pcell;
    printf("lattice = {\n");
    K_LIST_ENUMERATE (K->lattice, index, pcell) {
        printf("_%d: ", index);
        if (pcell->info.kind == CELL_TOP) {
            printf("⊤");
        } else if (pcell->info.kind == CELL_BOTTOM) {
            printf("⊥");
        } else if (pcell->type != NULL) {
            char const *type = pawIr_print_type(K->C, pcell->type);
            printf("k%d (%s): ", pcell->info.k, type);
            --ENV(K)->top.p;
            enum BuiltinKind b_kind = pawP_type2code(K->C, pcell->type);
            switch (b_kind) {
                case BUILTIN_UNIT:
                    printf("()");
                    break;
                case BUILTIN_BOOL:
                    printf("%s", V_TRUE(pcell->info.v) ? "true" : "false");
                    break;
                case BUILTIN_INT:
                    printf("%lld", V_INT(pcell->info.v));
                    break;
                case BUILTIN_FLOAT:
                    printf("%f", V_FLOAT(pcell->info.v));
                    break;
                default:
                    paw_assert(b_kind == BUILTIN_STR);
                    printf("\"%s\"", V_TEXT(pcell->info.v));
                    break;
            }
        }
        printf("\n");
    }
    printf("}\n");
}

#endif // PAW_DEBUG_EXTRA

// Check if a register is captured by a closure
// A captured register cannot participate in constant or copy propagation. The capturing
// closure might mutate such a register, even if it appears to have a constant value.
static paw_Bool is_captured(struct KProp *K, MirRegister r)
{
    return mir_reg_data(K->mir, r)->is_captured;
}

static struct MirAccessList *get_uses(struct KProp *K, MirRegister r)
{
    paw_assert(MIR_REG_EXISTS(r));
    return *AccessMap_get(K->mir, K->uses, r);
}

static struct Cell *get_cell(struct KProp *K, MirRegister r)
{
    paw_assert(MIR_REG_EXISTS(r));
    return &K_LIST_AT(K->lattice, r.value);
}

static void save_goto(struct KProp *K, struct MirInstruction *instr, MirBlock to)
{
    GotoMap_insert(K, K->gotos, instr, to);
}

static struct ExecList *get_exec_list(struct KProp *K, MirBlock to)
{
    paw_assert(MIR_BB_EXISTS(to));
    struct ExecList *const *plist = ExecMap_get(K, K->exec, to);
    if (plist != NULL)
        return *plist;
    struct ExecList *list = ExecList_new(K);
    ExecMap_insert(K, K->exec, to, list);
    return list;
}

// WARNING: calls to check_exec() invalidate flags returned by prior calls
static struct ExecFlag *check_exec(struct KProp *K, MirBlock from, MirBlock to)
{
    struct ExecFlag *pflag;
    struct ExecList *list = get_exec_list(K, to);
    K_LIST_FOREACH (list, pflag) {
        if (MIR_BB_EQUALS(pflag->from, from))
            return pflag;
    }

    ExecList_push(K, list, EXEC_FLAG(from, PAW_FALSE));
    return &K_LIST_LAST(list);
}

static void visit_phi(struct KProp *K, struct MirPhi *phi, MirBlock to)
{
    struct MirBlockData *data = mir_bb_data(K->mir, to);
    struct Cell def = Lattice_get(K->lattice, phi->output.r.value);

    MirBlock const *pfrom;
    struct MirPlace const *pinput;
    paw_assert(phi->inputs->count == data->predecessors->count);
    K_LIST_ZIP (phi->inputs, pinput, data->predecessors, pfrom) {
        struct ExecFlag *pflag = check_exec(K, *pfrom, to);
        struct Cell *pcell = &K_LIST_AT(K->lattice, phi->output.r.value);
        pcell->info.kind = pflag->exec ? def.info.kind : CELL_TOP;
    }
}

static struct CellInfo meet(struct Cell *a, struct Cell *b)
{
    if (a->info.kind == CELL_TOP || b->info.kind == CELL_BOTTOM)
        return b->info;
    if (b->info.kind == CELL_TOP || a->info.kind == CELL_BOTTOM)
        return a->info;
    paw_assert(a->info.kind == CELL_CONSTANT);
    paw_assert(b->info.kind == CELL_CONSTANT);

    if (a->info.k == b->info.k)
        return a->info;
    return (struct CellInfo){.kind = CELL_BOTTOM};
}

static void apply_meet_rules(struct KProp *K, struct Cell *output, struct MirRegisterPtrList *inputs)
{
    MirRegister *const *ppr;
    K_LIST_FOREACH (inputs, ppr) {
        struct Cell *input = get_cell(K, **ppr);
        output->info = meet(input, output);
    }
}

// TODO: share this code with codegen.c
static ValueMap *kcache_map(struct KProp *K, enum BuiltinKind kind)
{
    if (kind == BUILTIN_INT) {
        return K->kcache.ints;
    } else if (kind == BUILTIN_FLOAT) {
        return K->kcache.flts;
    } else {
        paw_assert(kind == BUILTIN_STR);
        return K->kcache.strs;
    }
}

static int add_constant(struct KProp *K, Value v, enum BuiltinKind kind)
{
    paw_assert(kind != NBUILTINS);
    if (kind <= BUILTIN_BOOL)
        kind = BUILTIN_INT;
    ValueMap *map = kcache_map(K, kind);
    Value const *pk = ValueMap_get(K->C, map, v);
    if (pk != NULL)
        return CAST(int, V_INT(*pk));
    ValueMap_insert(K->C, map, v, I2V(K->nk));
    return K->nk++;
}

#define BOTTOM_INFO() ((struct CellInfo){.kind = CELL_BOTTOM})
#define TOP_INFO() ((struct CellInfo){.kind = CELL_TOP})
#define CONST_INFO(num, val) ((struct CellInfo){.kind = CELL_CONSTANT, .k = (num), .v = (val)})

static struct CellInfo constant_unary_op(struct KProp *K, struct Cell *val, enum MirUnaryOpKind op)
{
    Value const v = val->info.v;
    Value r;

    if (pawP_fold_unary_op(K->C, op, v, &r)) {
        enum BuiltinKind const kind = pawP_type2code(K->C, val->type);
        int const k = add_constant(K, r, kind);
        return CONST_INFO(k, r);
    }
    return BOTTOM_INFO();
}

static struct CellInfo constant_binary_op(struct KProp *K, struct SourceLoc loc, struct Cell *lhs, struct Cell *rhs, enum MirBinaryOpKind op)
{
    Value const x = lhs->info.v;
    Value const y = rhs->info.v;
    Value r;

    if (pawP_fold_binary_op(K->C, K->mir->modname, loc, op, x, y, &r)) {
        enum BuiltinKind const kind = pawP_type2code(K->C, lhs->type);
        int const k = add_constant(K, r, kind);
        return CONST_INFO(k, r);
    }
    return BOTTOM_INFO();
}

static struct CellInfo binop_to_move(struct MirBinaryOp *binop, struct MirPlace from)
{
    struct MirPlace const to = binop->output;
    struct MirInstruction *instr = MIR_CAST_INSTRUCTION(binop);
    instr->hdr.kind = kMirMove;
    MirGetMove(instr)->target = from;
    MirGetMove(instr)->output = to;
    return TOP_INFO();
}

static struct CellInfo const_value(struct KProp *K, Value r, enum BuiltinKind kind)
{
    int const k = add_constant(K, r, kind);
    return CONST_INFO(k, r);
}

static struct CellInfo const_zero(struct KProp *K, enum BuiltinKind kind)
{
    return const_value(K, (Value){0}, kind);
}

static struct CellInfo const_nan(struct KProp *K)
{
    Value r;
    V_SET_FLOAT(&r, NAN);
    int const k = add_constant(K, r, BUILTIN_FLOAT);
    return CONST_INFO(k, r);
}

#define EQUALS_CONST_INT(Cell_, Int_) ((Cell_)->info.kind == CELL_CONSTANT && V_INT((Cell_)->info.v) == (Int_))
#define EQUALS_CONST_FLOAT(Cell_, Float_) ((Cell_)->info.kind == CELL_CONSTANT && V_FLOAT((Cell_)->info.v) == (Float_))
#define EQUALS_CONST_STR0(Cell_) ((Cell_)->info.kind == CELL_CONSTANT && V_STRING((Cell_)->info.v)->length == 0)

#define IS_NAN(Cell_) ((Cell_)->info.kind == CELL_CONSTANT && isnan(V_FLOAT((Cell_)->info.v)))
#define IS_INFINITY(Cell_) ((Cell_)->info.kind == CELL_CONSTANT && !isfinite(V_FLOAT((Cell_)->info.v)))

#define DIVIDE_BY_0(K, loc) KPROP_ERROR(K, constant_divide_by_zero, loc);
#define SHIFT_BY_NEGATIVE(K, loc) KPROP_ERROR(K, constant_negative_shift_count, loc);

// Fold binary operations where one of the operands is a compile-time constant
static struct CellInfo special_binary_op(struct KProp *K, struct Cell *lhs, struct Cell *rhs, struct MirBinaryOp *binop)
{
    enum BuiltinKind kind = pawP_type2code(K->C, lhs->type);
    enum MirBinaryOpKind const op = binop->op;

    // handle NaN propagation
    if (kind == BUILTIN_FLOAT && (IS_NAN(lhs) || IS_NAN(rhs)))
        return const_nan(K);

    switch (binop->op) {
        case MIR_BINARY_IADD:
            if (EQUALS_CONST_INT(lhs, 0)) {
                return binop_to_move(binop, binop->rhs);
            } else if (EQUALS_CONST_INT(rhs, 0)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_FADD:
            if (EQUALS_CONST_FLOAT(lhs, 0.0)) {
                return binop_to_move(binop, binop->rhs);
            } else if (EQUALS_CONST_FLOAT(rhs, 0.0)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_ISUB:
            if (EQUALS_CONST_INT(rhs, 0)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_FSUB:
            if (EQUALS_CONST_FLOAT(rhs, 0.0)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_IMUL:
            if (EQUALS_CONST_INT(lhs, 0) || EQUALS_CONST_INT(rhs, 0)) {
                return const_zero(K, BUILTIN_INT);
            } else if (EQUALS_CONST_INT(lhs, 1)) {
                return binop_to_move(binop, binop->rhs);
            } else if (EQUALS_CONST_INT(rhs, 1)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_FMUL:
            // NOTE: not possible to constant fold float multiplication due to -0.0
            break;
        case MIR_BINARY_IDIV:
            if (EQUALS_CONST_INT(rhs, 0)) {
                DIVIDE_BY_0(K, binop->loc);
            } else if (EQUALS_CONST_INT(lhs, 0)) {
                return const_zero(K, BUILTIN_INT);
            } else if (EQUALS_CONST_INT(rhs, 1)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_FDIV:
            // NOTE: the result of "0.0 / f" cannot be folded, since it depends on the
            //       sign of "f"
            if (EQUALS_CONST_FLOAT(rhs, 0.0)) {
                DIVIDE_BY_0(K, binop->loc);
            } else if (EQUALS_CONST_FLOAT(rhs, 1.0)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_IMOD:
            if (EQUALS_CONST_INT(rhs, 0)) {
                DIVIDE_BY_0(K, binop->loc);
            } else if (EQUALS_CONST_INT(lhs, 0) || EQUALS_CONST_INT(rhs, 1)) {
                return const_zero(K, BUILTIN_INT);
            }
            break;
        case MIR_BINARY_FMOD:
            if (EQUALS_CONST_FLOAT(rhs, 0.0)) {
                DIVIDE_BY_0(K, binop->loc);
            }
            break;
        case MIR_BINARY_BITAND:
            if (EQUALS_CONST_INT(lhs, 0) || EQUALS_CONST_INT(rhs, 0))
                return const_zero(K, BUILTIN_INT);
            break;
        case MIR_BINARY_BITOR:
        case MIR_BINARY_BITXOR:
            if (EQUALS_CONST_INT(lhs, 0)) {
                return binop_to_move(binop, binop->rhs);
            } else if (EQUALS_CONST_INT(rhs, 0)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_SHL:
        case MIR_BINARY_SHR:
            if (rhs->info.kind == CELL_CONSTANT && V_INT(rhs->info.v) < 0) {
                SHIFT_BY_NEGATIVE(K, binop->loc);
            } else if (EQUALS_CONST_INT(lhs, 0)) { // 0 shift n == 0
                return const_zero(K, BUILTIN_INT);
            } else if (EQUALS_CONST_INT(rhs, 0)) { // n shift 0 == n
                return binop_to_move(binop, binop->lhs);
            }
            break;
        case MIR_BINARY_SCONCAT:
            if (EQUALS_CONST_STR0(lhs)) {
                return binop_to_move(binop, binop->rhs);
            } else if (EQUALS_CONST_STR0(rhs)) {
                return binop_to_move(binop, binop->lhs);
            }
            break;
        default:
            break;
    }
    return BOTTOM_INFO();
}

// Fold binary operations where the operands are equal
static struct CellInfo self_binary_op(struct KProp *K, struct Cell *lhs, struct Cell *rhs, struct MirBinaryOp *binop)
{
    enum MirBinaryOpKind const op = binop->op;
    switch (binop->op) {
        case MIR_BINARY_ISUB:
        case MIR_BINARY_IMOD:
        case MIR_BINARY_BITXOR:
            return const_zero(K, BUILTIN_INT);
        case MIR_BINARY_FSUB:
        case MIR_BINARY_FMOD:
            return const_zero(K, BUILTIN_FLOAT);
        case MIR_BINARY_IDIV:
            return const_value(K, I2V(1), BUILTIN_INT);
        case MIR_BINARY_FDIV:
            return const_value(K, F2V(1.0), BUILTIN_FLOAT);
        case MIR_BINARY_BITAND:
        case MIR_BINARY_BITOR:
            return binop_to_move(binop, binop->lhs);
        default:
            return BOTTOM_INFO();
    }
}

// Remove the control flow edge between "from" and "to"
static void remove_edge(struct KProp *K, MirBlock from, MirBlock to)
{
    struct MirBlockData *bfrom = mir_bb_data(K->mir, from);
    struct MirBlockData *bto = mir_bb_data(K->mir, to);
    int const ipred = mir_which_pred(K->mir, to, from);
    int const isucc = mir_which_succ(K->mir, from, to);
    MirBlockList_swap_remove(bfrom->successors, isucc);
    MirBlockList_swap_remove(bto->predecessors, ipred);
}

static MirBlock single_branch_target(struct KProp *K, struct MirBranch *b, struct Cell *pcell, MirBlock from)
{
    paw_assert(pcell->info.kind == CELL_CONSTANT);
    return V_TRUE(pcell->info.v) ? b->then_arm : b->else_arm;
}

static MirBlock single_switch_target(struct KProp *K, struct MirSwitch *s, struct Cell *pcell, MirBlock from)
{
    paw_assert(pcell->info.kind == CELL_CONSTANT);
    enum BuiltinKind kind = pawP_type2code(K->C, pcell->type);
    Value const target = pcell->info.v;
    MirBlock result;

    struct MirSwitchArm *parm;
    K_LIST_FOREACH (s->arms, parm) {
        if ((kind != BUILTIN_FLOAT && V_UINT(parm->value) == V_UINT(target)) //
            || (kind == BUILTIN_FLOAT && V_FLOAT(parm->value) == V_FLOAT(target)))
            return parm->bid;
    }
    if (MIR_BB_EXISTS(s->otherwise))
        return s->otherwise;

    PAW_UNREACHABLE();
}

// Add def-use edges starting at the given definition to the SSA worklist
static void add_use_edges(struct KProp *K, MirRegister def)
{
    struct MirAccess *puse;
    struct MirAccessList *uses = get_uses(K, def);
    K_LIST_FOREACH (uses, puse) {
        struct SsaEdge edge = SSA_EDGE(def, puse->instr, puse->b);
        SsaWorklist_push(K, K->ssa, edge);
    }
}

static void into_goto(struct KProp *K, struct MirInstruction *instr, MirBlock b)
{
    instr->Goto_ = (struct MirGoto){
        .kind = kMirGoto,
        .loc = instr->hdr.loc,
        .mid = instr->hdr.mid,
        .target = b,
    };
}

static void into_constant(struct KProp *K, struct MirInstruction *instr, struct Cell cell)
{
    instr->Constant_ = (struct MirConstant){
        .kind = kMirConstant,
        .loc = instr->hdr.loc,
        .b_kind = pawP_type2code(K->C, cell.type),
        .mid = instr->hdr.mid,
        .value = cell.info.v,
        .output.r = cell.r,
    };
}

static void visit_expr(struct KProp *K, struct MirInstruction *instr, MirBlock b)
{
    // TODO: somewhat of a hack. shouldn't it be possible to just write CELL_BOTTOM to lattice cells
    //       that correspond to captured registers? doesn't work, but may be due to a different problem
    MirRegister *const *ppload;
    MirRegisterPtrList *loads = pawMir_get_loads(K->mir, instr);
    K_LIST_FOREACH (loads, ppload) {
        if (!is_captured(K, **ppload))
            continue;

        MirRegister *const *ppstore;
        MirRegisterPtrList *stores = pawMir_get_stores(K->mir, instr);
        K_LIST_FOREACH (stores, ppstore) {
            struct Cell *cell = get_cell(K, **ppstore);
            cell->info = BOTTOM_INFO();
        }
        return;
    }

    switch (MIR_KINDOF(instr)) {
        case kMirMove: {
            struct MirMove *x = MirGetMove(instr);
            struct Cell *target = get_cell(K, x->target.r);
            struct Cell *output = get_cell(K, x->output.r);
            enum CellKind old = output->info.kind;
            if (target->info.kind == CELL_CONSTANT) {
                output->info = target->info;
            } else {
                output->info = BOTTOM_INFO();
            }
            if (old != output->info.kind)
                add_use_edges(K, output->r);
            break;
        }
        case kMirConstant: {
            struct MirConstant *x = MirGetConstant(instr);
            struct MirRegisterData *data = mir_reg_data(K->mir, x->output.r);
            int const k = add_constant(K, x->value, x->b_kind);
            struct Cell *output = get_cell(K, x->output.r);
            enum CellKind old = output->info.kind;
            output->info = CONST_INFO(k, x->value);
            if (old != CELL_CONSTANT)
                add_use_edges(K, output->r);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *x = MirGetUnaryOp(instr);
            struct Cell *val = get_cell(K, x->val.r);
            struct Cell *output = get_cell(K, x->output.r);
            enum CellKind old = output->info.kind;
            if (val->info.kind == CELL_CONSTANT) {
                output->info = constant_unary_op(K, val, x->op);
            } else {
                output->info = BOTTOM_INFO();
            }
            if (old != output->info.kind)
                add_use_edges(K, output->r);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *x = MirGetBinaryOp(instr);
            struct Cell *lhs = get_cell(K, x->lhs.r);
            struct Cell *rhs = get_cell(K, x->rhs.r);
            struct Cell *output = get_cell(K, x->output.r);
            enum CellKind old = output->info.kind;
            if (lhs->info.kind == CELL_CONSTANT && rhs->info.kind == CELL_CONSTANT) {
                // handle "const1 op const2"
                output->info = constant_binary_op(K, instr->hdr.loc, lhs, rhs, x->op);
            } else if (lhs->info.kind == CELL_CONSTANT || rhs->info.kind == CELL_CONSTANT) {
                // handle "reg op const" or "const op reg"
                output->info = special_binary_op(K, lhs, rhs, x);
            } else if (MIR_REG_EQUALS(lhs->r, rhs->r)) {
                // handle "reg op reg"
                output->info = self_binary_op(K, lhs, rhs, x);
            } else {
                output->info = BOTTOM_INFO();
            }
            if (old != output->info.kind)
                add_use_edges(K, output->r);
            break;
        }
        case kMirBranch: {
            struct MirBranch *x = MirGetBranch(instr);
            struct Cell *cond = get_cell(K, x->cond.r);
            if (cond->info.kind == CELL_CONSTANT) {
                MirBlock const s = single_branch_target(K, x, cond, b);
                FlowWorklist_push(K, K->flow, FLOW_EDGE(b, s));
                save_goto(K, instr, s);
            }
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *x = MirGetSwitch(instr);
            struct Cell *discr = get_cell(K, x->discr.r);
            if (discr->info.kind == CELL_CONSTANT) {
                MirBlock const s = single_switch_target(K, x, discr, b);
                FlowWorklist_push(K, K->flow, FLOW_EDGE(b, s));
                save_goto(K, instr, s);
            }
            break;
        }
        case kMirFreeLocal:
        case kMirClosure:
        case kMirClose:
        case kMirSetElement:
        case kMirSetRange:
        case kMirSetField:
        case kMirReturn:
        case kMirGoto:
            return; // no output
        default: {
            MirRegister *const *ppstore;
            MirRegisterPtrList const *stores = pawMir_get_stores(K->mir, instr);
            K_LIST_FOREACH (stores, ppstore) {
                struct Cell *output = get_cell(K, **ppstore);
                output->info = BOTTOM_INFO();
                add_use_edges(K, **ppstore);
            }
        }
    }
}

static void process_ssa_edge(struct KProp *K)
{
    struct SsaEdge edge = SsaWorklist_last(K->ssa);
    SsaWorklist_pop(K->ssa);
    if (MirIsPhi(edge.use)) {
        visit_phi(K, MirGetPhi(edge.use), edge.b);
        return;
    }

    struct ExecFlag const *pflag;
    struct ExecList *exec = get_exec_list(K, edge.b);
    K_LIST_FOREACH (exec, pflag) {
        if (pflag->exec) {
            visit_expr(K, edge.use, edge.b);
            break;
        }
    }
}

static paw_Bool has_single_incoming_edge(struct KProp *K, MirBlock b)
{
    int found = 0;
    MirBlock const *pb;
    struct MirBlockData *block = mir_bb_data(K->mir, b);
    K_LIST_FOREACH (block->predecessors, pb) {
        struct ExecFlag *flag = check_exec(K, *pb, b);
        if (flag->exec && ++found > 1)
            return PAW_FALSE;
    }
    return found == 1;
}

static void process_flow_edge(struct KProp *K)
{
    struct FlowEdge edge = FlowWorklist_last(K->flow);
    struct MirBlockData *target = mir_bb_data(K->mir, edge.to);
    FlowWorklist_pop(K->flow);

    struct ExecFlag *flag = check_exec(K, edge.from, edge.to);
    if (flag->exec)
        return;
    flag->exec = PAW_TRUE;

    struct MirInstruction *const *pjoin;
    K_LIST_FOREACH (target->joins, pjoin) {
        visit_phi(K, MirGetPhi(*pjoin), edge.to);
    }

    if (has_single_incoming_edge(K, edge.to)) {
        struct MirInstruction *const *pinstr;
        struct MirBlockData *block = mir_bb_data(K->mir, edge.to);
        K_LIST_FOREACH (block->instructions, pinstr) {
            visit_expr(K, *pinstr, edge.to);
        }
    }

    if (target->successors->count == 1) {
        MirBlock const s = K_LIST_FIRST(target->successors);
        FlowWorklist_push(K, K->flow, FLOW_EDGE(edge.to, s));
    }
}

static void propagate_constants(struct KProp *K)
{
    {
        // initialize the flow worklist with edges leading out of the start node
        MirBlock const *pb;
        struct MirBlockData *start = K_LIST_FIRST(K->mir->blocks);
        K_LIST_FOREACH (start->successors, pb) {
            FlowWorklist_push(K, K->flow, FLOW_EDGE(MIR_ROOT_BB, *pb));
        }
    }

    while (K->flow->count > 0 || K->ssa->count > 0) {
        if (K->flow->count > 0)
            process_flow_edge(K);
        if (K->ssa->count > 0)
            process_ssa_edge(K);
    }
}

static void init_lattice(struct KProp *K)
{
    int rid;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE (K->mir->registers, rid, pdata) {
        MirRegister const r = MIR_REG(rid);
        Lattice_set(K->lattice, r.value, ((struct Cell){
                                             .info.kind = CELL_TOP,
                                             .type = pdata->type,
                                             .r = r,
                                         }));
    }
}

static void account_for_uses(struct KProp *K, struct MirInstruction *instr, UseCountMap *uses)
{
    MirRegister *const *ppr;
    struct MirRegisterPtrList *loads = pawMir_get_loads(K->mir, instr);
    K_LIST_FOREACH (loads, ppr) {
        int *pcount = UseCountMap_get(K, uses, **ppr);
        ++*pcount;
    }
}

static void count_uses(struct KProp *K, UseCountMap *uses)
{
    struct Mir *mir = K->mir;

    int index;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE (mir->registers, index, pdata) {
        // being captured in 1 or more closures counts as a single usage
        UseCountMap_insert(K, uses, MIR_REG(index), pdata->is_captured);
    }

    struct MirBlockData **pblock;
    K_LIST_FOREACH (mir->blocks, pblock) {
        struct MirBlockData *block = *pblock;
        struct MirInstruction **pinstr;
        K_LIST_FOREACH (block->joins, pinstr)
            account_for_uses(K, *pinstr, uses);
        K_LIST_FOREACH (block->instructions, pinstr)
            account_for_uses(K, *pinstr, uses);
    }
}

static void remove_operand_uses(struct KProp *K, UseCountMap *counts, struct MirInstruction *instr)
{
    MirRegister *const *ppr;
    struct MirRegisterPtrList *loads = pawMir_get_loads(K->mir, instr);
    K_LIST_FOREACH (loads, ppr) {
        int *pcount = UseCountMap_get(K, counts, **ppr);
        paw_assert(*pcount > 0);
        --*pcount;
    }
}

static paw_Bool is_pure(struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirAllocLocal:
        case kMirPhi:
        case kMirMove:
        case kMirConstant:
        case kMirCast:
        case kMirUpvalue:
        case kMirAggregate:
        case kMirContainer:
        case kMirGetField:
        case kMirGetElement:
        case kMirGetRange:
        case kMirUnaryOp:
        case kMirBinaryOp:
            return PAW_TRUE;
        default:
            return PAW_FALSE;
    }
}

static void prune_dead_successors(struct KProp *K, MirBlock from, MirBlock target)
{
    int index, itarget;
    MirBlock const *pto;
    struct MirBlockData *bfrom = mir_bb_data(K->mir, from);
    K_LIST_ENUMERATE (bfrom->successors, index, pto) {
        if (!MIR_REG_EQUALS(*pto, target)) {
            int const ipred = mir_which_pred(K->mir, *pto, from);
            struct MirBlockData *bto = mir_bb_data(K->mir, *pto);
            MirBlockList_remove(bto->predecessors, ipred);
        } else {
            itarget = index;
        }
    }
    MirBlockList_set(bfrom->successors, 0, target);
    bfrom->successors->count = 1;
    K->altered = PAW_TRUE;
}

static void transform_instr(struct KProp *K, struct MirInstruction *instr)
{
    if (MirIsConstant(instr) || !is_pure(instr))
        return;

    MirRegisterPtrList const *stores = pawMir_get_stores(K->mir, instr);
    paw_assert(stores->count == 1);

    struct Cell *cell = get_cell(K, *K_LIST_FIRST(stores));
    if (cell->info.kind == CELL_CONSTANT) {
        // all operands to this instruction are constant
        into_constant(K, instr, *cell);
        K->altered = PAW_TRUE;
    }
}

static void transform_code(struct KProp *K)
{
    int index = 0;
    struct MirBlockData **pblock;
    struct MirInstruction **pinstr;
    struct MirBlockDataList *blocks = K->mir->blocks;
    K_LIST_ENUMERATE (blocks, index, pblock) {
        MirBlock const from = MIR_BB(index);
        struct MirBlockData *block = *pblock;
        K_LIST_FOREACH (block->joins, pinstr)
            transform_instr(K, *pinstr);
        // TODO: probably should remove phi instructions if any were transformed here...

        K_LIST_FOREACH (block->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            MirBlock const *pto = GotoMap_get(K, K->gotos, instr);
            if (pto != NULL) {
                // instruction is a branch/switch controlled by a constant
                prune_dead_successors(K, from, *pto);
                into_goto(K, instr, *pto);
            } else {
                transform_instr(K, instr);
            }
        }
    }
}

static paw_Bool filter_code(struct KProp *K, UseCountMap *counts, struct MirInstructionList *code)
{
    int const num_params = IR_FPTR(K->mir->type)->params->count;

    int index;
    int num_removed = 0;
    struct MirInstruction **pinstr;
    K_LIST_ENUMERATE (code, index, pinstr) {
        struct MirInstruction *instr = *pinstr;
        if (is_pure(instr)) {
            // instruction has no side-effects
            int num_unused = 0;
            MirRegister *const *ppstore;
            MirRegisterPtrList const *stores = pawMir_get_stores(K->mir, instr);
            K_LIST_FOREACH (stores, ppstore) {
                MirRegister const store = **ppstore;
                if (store.value > num_params) {
                    int const *pcount = UseCountMap_get(K, counts, store);
                    num_unused += *pcount == 0;
                }
            }
            if (num_unused == stores->count) {
                // all stores are unused, meaning the instruction itself is unused
                remove_operand_uses(K, counts, instr);
                ++num_removed;
                continue;
            }
        }
        struct MirInstruction *keep = MirInstructionList_get(code, index);
        MirInstructionList_set(code, index - num_removed, keep);
    }
    code->count -= num_removed;
    return num_removed > 0;
}

static paw_Bool remove_dead_code(struct KProp *K, UseCountMap *counts)
{
    paw_Bool removed_any = PAW_FALSE;
    struct MirBlockDataList *blocks = K->mir->blocks;
    for (int i = blocks->count - 1; i >= 0; --i) {
        struct MirBlockData *block = MirBlockDataList_get(blocks, i);
        removed_any |= filter_code(K, counts, block->joins);
        removed_any |= filter_code(K, counts, block->instructions);
    }
    return removed_any;
}

static void clean_up_code(struct KProp *K)
{
    pawMir_remove_unreachable_blocks(K->mir);
    pawMir_merge_redundant_blocks(K->mir);

    UseCountMap *counts = UseCountMap_new(K);
    count_uses(K, counts);

    paw_Bool removed_code;
    do {
        removed_code = remove_dead_code(K, counts);
        K->altered |= removed_code;
    } while (removed_code);
}

static void propagate_copy(struct KProp *K, struct MirMove *move)
{
    struct MirAccess *puse;
    MirRegister *const *ppr;
    struct MirAccessList *uses = get_uses(K, move->output.r);
    K_LIST_FOREACH (uses, puse) {
        struct MirRegisterPtrList *loads = pawMir_get_loads(K->mir, puse->instr);
        K_LIST_FOREACH (loads, ppr) {
            if (MIR_REG_EQUALS(**ppr, move->output.r)) {
                K->altered = PAW_TRUE;
                // replace output with operand
                **ppr = move->target.r;
                break;
            }
        }
    }
}

static void propagate_copies(struct KProp *K)
{
    struct MirBlockData **pblock;
    struct MirInstruction **pinstr;
    K_LIST_FOREACH (K->mir->blocks, pblock) {
        struct MirBlockData *block = *pblock;
        struct MirInstructionList *instrs = MirInstructionList_new(K->mir);
        MirInstructionList_reserve(K->mir, instrs, block->instructions->count);
        K_LIST_FOREACH (block->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            if (MirIsMove(instr)) {
                struct MirMove *move = MirGetMove(instr);
                if (!is_captured(K, move->target.r)
                        && !is_captured(K, move->output.r)) {
                    propagate_copy(K, move);
                    continue;
                }
            }
            MirInstructionList_push(K->mir, instrs, instr);
        }
        block->instructions = instrs;
    }
}

static paw_Bool propagate(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct KProp K = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    K.uses = AccessMap_new_from(mir, K.pool);
    K.lattice = Lattice_new(&K);
    K.flow = FlowWorklist_new(&K);
    K.ssa = SsaWorklist_new(&K);
    K.exec = ExecMap_new(&K);
    K.gotos = GotoMap_new(&K);

    Lattice_reserve(&K, K.lattice, mir->registers->count);
    K.lattice->count = mir->registers->count;

    K.kcache.ints = ValueMap_new_from(C, K.pool);
    K.kcache.strs = ValueMap_new_from(C, K.pool);
    K.kcache.flts = ValueMap_new_from(C, K.pool);
    pawMir_collect_per_instr_uses(mir, K.uses);

    init_lattice(&K);
    propagate_copies(&K);
    propagate_constants(&K);
    transform_code(&K);
    clean_up_code(&K);

    pawP_pool_free(C, K.pool);
    return K.altered;
}

void pawMir_propagate_constants(struct Mir *mir)
{
    // TODO: "force" flag to force propagation for global constants
#ifdef PAW_KPROP_OFF
    return;
#endif

    paw_Bool altered;
    do {
        altered = propagate(mir);
    } while (altered);
}
