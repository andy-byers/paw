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
#define DIVIDE_BY_0(K, loc) KPROP_ERROR(K, constant_divide_by_zero, loc);
#define SHIFT_BY_NEGATIVE(K, loc) KPROP_ERROR(K, constant_negative_shift_count, loc);

enum CellKind {
    CELL_TOP,
    CELL_BOTTOM,
    CELL_CONSTANT,
};

struct Cell {
    struct CellInfo {
        enum CellKind kind;
        int k;
        Value v;
    } info;

    IrType *type;
    MirRegister r;
};

#define BOTTOM_INFO() ((struct CellInfo){.kind = CELL_BOTTOM})
#define TOP_INFO() ((struct CellInfo){.kind = CELL_TOP})
#define CONST_INFO(num, val) ((struct CellInfo){.kind = CELL_CONSTANT, .k = (num), .v = (val)})

struct SsaEdge {
    MirBlock b;
    MirRegister def;
    struct MirInstruction *use;
};

struct FlowEdge {
    MirBlock from;
    MirBlock to;
};

#define SSA_EDGE(def, use, b) ((struct SsaEdge){b, def, use})
#define FLOW_EDGE(from, to) ((struct FlowEdge){from, to})

struct KProp {
    struct FlowWorklist *flow;
    struct SsaWorklist *ssa;
    struct Lattice *lattice;
    struct KCache kcache;
    struct ExecMap *exec;
    AccessMap *uses;

    paw_Bool altered;

    struct Pool *pool;
    struct Mir *mir;
    struct Compiler *C;
    paw_Env *P;
};

DEFINE_LIST(struct KProp, FlowWorklist, struct FlowEdge)
DEFINE_LIST(struct KProp, SsaWorklist, struct SsaEdge)
DEFINE_LIST(struct KProp, Lattice, struct Cell)

DEFINE_MAP(struct KProp, UseCountMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirRegister, int)
DEFINE_MAP(struct KProp, ExecMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirBlock, MirBlockList *)

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
            enum BuiltinKind b_kind = pawP_type2code(K->C, pcell->type);
            switch (b_kind) {
                case BUILTIN_UNIT:
                    printf("()");
                    break;
                case BUILTIN_BOOL:
                    printf("%s", V_TRUE(pcell->info.v) ? "true" : "false");
                    break;
                case BUILTIN_CHAR:
                    printf("%c", V_CHAR(pcell->info.v));
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
    printf("exec = {\n");
    for (int to = 0; to < K->mir->blocks->count; ++to) {
        MirBlockList *const *plist = ExecMap_get(K, K->exec, MIR_BB(to));
        if (plist != NULL) {
            MirBlockList const *list = *plist;
            MirBlock const *pflag;
            K_LIST_FOREACH (list, pflag) {
                printf("  bb%d => bb%d\n", pflag->value, to);
            }
        }
    }
    printf("}\n");
}

#endif // PAW_DEBUG_EXTRA

static paw_Bool is_stack_reg(struct KProp *K, MirRegister r)
{
    return mir_reg_data(K->mir, r)->info == MIR_REGINFO_ARGUMENT;
}

// Check if a register is captured by a closure
// A captured register cannot participate in constant or copy propagation. The capturing
// closure might mutate such a register, even if it appears to have a constant value.
static paw_Bool is_captured(struct KProp *K, MirRegister r)
{
    return mir_reg_data(K->mir, r)->is_captured;
}

static struct MirAccessList *get_uses(struct KProp *K, MirRegister r)
{
    paw_assert(MIR_ID_EXISTS(r));
    return *AccessMap_get(K->mir, K->uses, r);
}

#define LATTICE_KEY(K_, Place_) ((Place_).kind == MIR_PLACE_LOCAL ? (Place_).r.value \
        : (Place_).k.value + (K_)->mir->registers->count)

static struct Cell *get_cell(struct KProp *K, struct MirPlace place)
{
    return &K_LIST_AT(K->lattice, LATTICE_KEY(K, place));
}

static MirBlockList *get_exec_list(struct KProp *K, MirBlock to)
{
    paw_assert(MIR_ID_EXISTS(to));
    MirBlockList *const *plist = ExecMap_get(K, K->exec, to);
    if (plist != NULL) return *plist;

    MirBlockList *list = MirBlockList_new(K->mir);
    ExecMap_insert(K, K->exec, to, list);
    return list;
}

static paw_Bool is_exec(struct KProp *K, MirBlock from, MirBlock to)
{
    MirBlock *pflag;
    MirBlockList *list = get_exec_list(K, to);
    K_LIST_FOREACH (list, pflag) {
        if (MIR_ID_EQUALS(*pflag, from))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool set_exec(struct KProp *K, MirBlock from, MirBlock to)
{
    MirBlock *pflag;
    MirBlockList *list = get_exec_list(K, to);
    K_LIST_FOREACH (list, pflag) {
        if (MIR_ID_EQUALS(*pflag, from))
            return PAW_TRUE;
    }
    MirBlockList_push(K->mir, list, from);
    return PAW_FALSE;
}

static struct CellInfo meet(struct Cell const *a, struct Cell const *b)
{
    if (a->info.kind == CELL_TOP || b->info.kind == CELL_BOTTOM) return b->info;
    if (b->info.kind == CELL_TOP || a->info.kind == CELL_BOTTOM) return a->info;
    paw_assert(a->info.kind == CELL_CONSTANT);
    paw_assert(b->info.kind == CELL_CONSTANT);

    return a->info.k == b->info.k ? a->info : BOTTOM_INFO();
}

// Add def-use edges starting at the given definition to the SSA worklist
static void add_use_edges(struct KProp *K, MirRegister def)
{
    struct MirAccess const *puse;
    struct MirAccessList const *uses = get_uses(K, def);
    K_LIST_FOREACH (uses, puse) {
        struct SsaEdge const edge = SSA_EDGE(def, puse->instr, puse->b);
        SsaWorklist_push(K, K->ssa, edge);
    }
}

static void visit_phi(struct KProp *K, struct MirPhi *phi, MirBlock to)
{
    struct Cell *presult = get_cell(K, phi->output);
    if (presult->info.kind == CELL_BOTTOM) return;
    enum CellKind const kind = presult->info.kind;

    MirBlock const *pfrom;
    struct MirPlace const *pinput;
    struct MirBlockData const *data = mir_bb_data(K->mir, to);
    paw_assert(phi->inputs->count == data->predecessors->count);
    K_LIST_ZIP (phi->inputs, pinput, data->predecessors, pfrom) {
        if (is_exec(K, *pfrom, to)) {
            presult->info = meet(presult, get_cell(K, *pinput));
            if (presult->info.kind == CELL_BOTTOM) break;
        }
    }

    if (presult->info.kind != kind)
        add_use_edges(K, presult->r);
}

static ValueMap *kcache_map(struct KProp *K, enum BuiltinKind kind)
{
    if (kind <= BUILTIN_INT) {
        return K->kcache.ints;
    } else if (kind == BUILTIN_FLOAT) {
        return K->kcache.floats;
    } else {
        paw_assert(kind == BUILTIN_STR);
        return K->kcache.strs;
    }
}

static int add_constant(struct KProp *K, Value v, enum BuiltinKind kind)
{
    paw_assert(kind != NBUILTINS);
    ValueMap *map = kcache_map(K, kind);
    Value const *pk = ValueMap_get(K->C, map, v);
    if (pk != NULL) return (int)V_INT(*pk);

    int const k = K->mir->constants->count;
    ValueMap_insert(K->C, map, v, I2V(k));
    MirConstantDataList_push(K->mir, K->mir->constants,
        (struct MirConstantData){
            .kind = kind,
            .value = v,
        });
    return k;
}

static struct CellInfo constant_unary_op(struct KProp *K, struct Cell *val, struct Cell *output, enum MirUnaryOpKind op)
{
    Value const v = val->info.v;
    Value r;

    if (pawP_fold_unary_op(K->C, op, v, &r)) {
        enum BuiltinKind const kind = pawP_type2code(K->C, output->type);
        int const k = add_constant(K, r, kind);
        return CONST_INFO(k, r);
    }
    return BOTTOM_INFO();
}

static struct CellInfo constant_binary_op(struct KProp *K, struct SourceLoc loc, struct Cell *lhs, struct Cell *rhs, struct Cell const *output, enum MirBinaryOpKind op)
{
    Value const x = lhs->info.v;
    Value const y = rhs->info.v;
    Value r;

    if (pawP_fold_binary_op(K->C, K->mir->modname, loc, op, x, y, &r)) {
        enum BuiltinKind const kind = pawP_type2code(K->C, output->type);
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
    return const_value(K, I2V(0), kind);
}

static struct CellInfo const_nan(struct KProp *K)
{
    Value r;
    V_SET_FLOAT(&r, NAN);
    int const k = add_constant(K, r, BUILTIN_FLOAT);
    return CONST_INFO(k, r);
}

#define EQUALS_CONST_CHAR(Cell_, Char_) ((Cell_)->info.kind == CELL_CONSTANT && V_CHAR((Cell_)->info.v) == (Char_))
#define EQUALS_CONST_INT(Cell_, Int_) ((Cell_)->info.kind == CELL_CONSTANT && V_INT((Cell_)->info.v) == (Int_))
#define EQUALS_CONST_FLOAT(Cell_, Float_) ((Cell_)->info.kind == CELL_CONSTANT && V_FLOAT((Cell_)->info.v) == (Float_))
#define EQUALS_CONST_STR0(Cell_) ((Cell_)->info.kind == CELL_CONSTANT && V_STR((Cell_)->info.v)->length == 0)

#define IS_NAN(Cell_) ((Cell_)->info.kind == CELL_CONSTANT && isnan(V_FLOAT((Cell_)->info.v)))
#define IS_INFINITY(Cell_) ((Cell_)->info.kind == CELL_CONSTANT && !isfinite(V_FLOAT((Cell_)->info.v)))

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
                return rhs->info;
            } else if (EQUALS_CONST_INT(rhs, 0)) {
                return lhs->info;
            }
            break;
        case MIR_BINARY_FADD:
            if (EQUALS_CONST_FLOAT(lhs, 0.0)) {
                return rhs->info;
            } else if (EQUALS_CONST_FLOAT(rhs, 0.0)) {
                return lhs->info;
            }
            break;
        case MIR_BINARY_ISUB:
            if (EQUALS_CONST_INT(rhs, 0)) {
                return lhs->info;
            }
            break;
        case MIR_BINARY_FSUB:
            if (EQUALS_CONST_FLOAT(rhs, 0.0)) {
                return lhs->info;
            }
            break;
        case MIR_BINARY_IMUL:
            if (EQUALS_CONST_INT(lhs, 0) || EQUALS_CONST_INT(rhs, 0)) {
                return const_zero(K, BUILTIN_INT);
            } else if (EQUALS_CONST_INT(lhs, 1)) {
                return rhs->info;
            } else if (EQUALS_CONST_INT(rhs, 1)) {
                return lhs->info;
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
                return lhs->info;
            }
            break;
        case MIR_BINARY_FDIV:
            // NOTE: the result of "0.0 / f" cannot be folded, since it depends on the
            //       sign of "f"
            if (EQUALS_CONST_FLOAT(rhs, 0.0)) {
                DIVIDE_BY_0(K, binop->loc);
            } else if (EQUALS_CONST_FLOAT(rhs, 1.0)) {
                return lhs->info;
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
        case MIR_BINARY_IBITAND:
            if (EQUALS_CONST_INT(lhs, 0) || EQUALS_CONST_INT(rhs, 0))
                return const_zero(K, BUILTIN_INT);
            break;
        case MIR_BINARY_IBITOR:
        case MIR_BINARY_IBITXOR:
            if (EQUALS_CONST_INT(lhs, 0)) {
                return rhs->info;
            } else if (EQUALS_CONST_INT(rhs, 0)) {
                return lhs->info;
            }
            break;
        case MIR_BINARY_ISHL:
        case MIR_BINARY_ISHR:
            if (rhs->info.kind == CELL_CONSTANT && V_INT(rhs->info.v) < 0) {
                SHIFT_BY_NEGATIVE(K, binop->loc);
            } else if (EQUALS_CONST_INT(lhs, 0)) { // 0 shift n == 0
                return const_zero(K, BUILTIN_INT);
            } else if (EQUALS_CONST_INT(rhs, 0)) { // n shift 0 == n
                return lhs->info;
            }
            break;
        case MIR_BINARY_STRCAT:
            if (EQUALS_CONST_STR0(lhs)) {
                return rhs->info;
            } else if (EQUALS_CONST_STR0(rhs)) {
                return lhs->info;
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
        case MIR_BINARY_IBITXOR:
            return const_zero(K, BUILTIN_INT);
        case MIR_BINARY_FSUB:
        case MIR_BINARY_FMOD:
            return const_zero(K, BUILTIN_FLOAT);
        case MIR_BINARY_IDIV:
            return const_value(K, I2V(1), BUILTIN_INT);
        case MIR_BINARY_FDIV:
            return const_value(K, F2V(1.0), BUILTIN_FLOAT);
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

static MirBlock single_branch_target(struct KProp *K, struct MirBranch *b, struct Cell *pcell, struct MirBlockData const *bb)
{
    paw_assert(pcell->info.kind == CELL_CONSTANT && bb->successors->count == 2);
    return MirBlockList_get(bb->successors, !V_TRUE(pcell->info.v));
}

static MirBlock single_switch_target(struct KProp *K, struct MirSwitch *s, struct Cell *pcell, struct MirBlockData const *bb)
{
    paw_assert(pcell->info.kind == CELL_CONSTANT);
    enum BuiltinKind kind = pawP_type2code(K->C, pcell->type);
    Value const target = pcell->info.v;

    MirBlock const *pb;
    struct MirSwitchArm *parm;
    K_LIST_ZIP (s->arms, parm, bb->successors, pb) {
        struct MirConstantData const *kdata = mir_const_data(K->mir, parm->k);
        if ((kind != BUILTIN_FLOAT && V_UINT(kdata->value) == V_UINT(target))
                || (kind == BUILTIN_FLOAT && V_FLOAT(kdata->value) == V_FLOAT(target)))
            return *pb;
    }

    // Matches are exhaustive, so if a matching case was not found above, then there
    // must exist a catch-all case.
    paw_assert(s->has_otherwise && s->arms->count == bb->successors->count - 1);
    return *pb; // pointer left on last element
}

static void into_load_k(struct MirInstruction *instr, MirConstant k, struct MirPlace output)
{
    instr->hdr.kind = kMirLoadConstant;
    MirGetLoadConstant(instr)->output = output;
    MirGetLoadConstant(instr)->k = k;
}

static void into_goto(struct KProp *K, struct MirInstruction *instr)
{
    instr->Goto_ = (struct MirGoto){
        .kind = kMirGoto,
        .loc = instr->hdr.loc,
        .mid = instr->hdr.mid,
    };

    K->altered = PAW_TRUE;
}

static void into_constant(struct KProp *K, struct MirPlace *place, struct Cell cell)
{
    *place = (struct MirPlace){
        .projection = place->projection,
        .kind = MIR_PLACE_CONSTANT,
        .k = MIR_CONST(cell.info.k),
    };

    K->altered = PAW_TRUE;
}

static void visit_expr(struct KProp *K, struct MirInstruction *instr, MirBlock b)
{
    // TODO: somewhat of a hack. shouldn't it be possible to just write CELL_BOTTOM to lattice cells
    //       that correspond to captured registers? doesn't work, but may be due to a different problem
    struct MirPlace *const *ppload;
    struct MirPlacePtrList *loads = pawMir_get_loads(K->mir, instr);
    K_LIST_FOREACH (loads, ppload) {
        if ((*ppload)->kind == MIR_PLACE_LOCAL && is_captured(K, (*ppload)->r)) {
            struct MirPlace *const *ppstore;
            struct MirPlacePtrList *stores = pawMir_get_stores(K->mir, instr);
            K_LIST_FOREACH (stores, ppstore) {
                struct Cell *cell = get_cell(K, **ppstore);
                cell->info = BOTTOM_INFO();
            }
            return;
        }
    }
    struct MirBlockData const *bb = mir_bb_data(K->mir, b);

    switch (MIR_KINDOF(instr)) {
        case kMirMove: {
            struct MirMove const *x = MirGetMove(instr);
            struct Cell const *target = get_cell(K, x->target);
            struct Cell *output = get_cell(K, x->output);
            enum CellKind const old = output->info.kind;
            if (target->info.kind == CELL_CONSTANT) {
                output->info = target->info;
            } else {
                output->info = BOTTOM_INFO();
            }
            if (old != output->info.kind)
                add_use_edges(K, output->r);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp const *x = MirGetUnaryOp(instr);
            struct Cell *val = get_cell(K, x->val);
            struct Cell *output = get_cell(K, x->output);
            enum CellKind const old = output->info.kind;
            if (val->info.kind == CELL_CONSTANT) {
                output->info = constant_unary_op(K, val, output, x->op);
            } else {
                output->info = BOTTOM_INFO();
            }
            if (old != output->info.kind)
                add_use_edges(K, output->r);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *x = MirGetBinaryOp(instr);
            struct Cell *lhs = get_cell(K, x->lhs);
            struct Cell *rhs = get_cell(K, x->rhs);
            struct Cell *output = get_cell(K, x->output);
            enum CellKind const old = output->info.kind;
            if (lhs->info.kind == CELL_CONSTANT && rhs->info.kind == CELL_CONSTANT) {
                // handle "const1 op const2"
                output->info = constant_binary_op(K, instr->hdr.loc, lhs, rhs, output, x->op);
            } else if (lhs->info.kind == CELL_CONSTANT || rhs->info.kind == CELL_CONSTANT) {
                // handle "reg op const" or "const op reg"
                output->info = special_binary_op(K, lhs, rhs, x);
            } else if (MIR_ID_EQUALS(lhs->r, rhs->r)) {
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
            struct Cell *cond = get_cell(K, x->cond);
            if (cond->info.kind == CELL_CONSTANT) {
                MirBlock const s = single_branch_target(K, x, cond, bb);
                FlowWorklist_push(K, K->flow, FLOW_EDGE(b, s));
            } else {
                paw_assert(cond->info.kind == CELL_BOTTOM);
                FlowWorklist_push(K, K->flow, FLOW_EDGE(b, MirBlockList_get(bb->successors, 0)));
                FlowWorklist_push(K, K->flow, FLOW_EDGE(b, MirBlockList_get(bb->successors, 1)));
            }
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *x = MirGetSwitch(instr);
            struct Cell *discr = get_cell(K, x->discr);
            if (discr->info.kind == CELL_CONSTANT) {
                MirBlock const s = single_switch_target(K, x, discr, bb);
                FlowWorklist_push(K, K->flow, FLOW_EDGE(b, s));
            } else {
                paw_assert(discr->info.kind == CELL_BOTTOM);
                MirBlock const *pb;
                K_LIST_FOREACH (bb->successors, pb)
                    FlowWorklist_push(K, K->flow, FLOW_EDGE(b, *pb));
            }
            break;
        }
        default: {
            struct MirPlace *const *ppstore;
            MirPlacePtrList const *stores = pawMir_get_stores(K->mir, instr);
            K_LIST_FOREACH (stores, ppstore) {
                struct Cell *output = get_cell(K, **ppstore);
                if (output->info.kind != CELL_BOTTOM) {
                    output->info = BOTTOM_INFO();
                    add_use_edges(K, (*ppstore)->r);
                }
            }
        }
    }
}

static void process_ssa_edge(struct KProp *K, struct SsaEdge edge)
{
    if (MirIsPhi(edge.use)) {
        visit_phi(K, MirGetPhi(edge.use), edge.b);
        return;
    }

    if (get_exec_list(K, edge.b)->count > 0)
        visit_expr(K, edge.use, edge.b);
}

static paw_Bool has_single_incoming_edge(struct KProp *K, MirBlock b)
{
    int found = 0;
    MirBlock const *pb;
    struct MirBlockData const *bb = mir_bb_data(K->mir, b);
    K_LIST_FOREACH (bb->predecessors, pb) {
        if (is_exec(K, *pb, b) && ++found > 1)
            return PAW_FALSE;
    }
    return found == 1;
}

static void process_flow_edge(struct KProp *K, struct FlowEdge edge)
{
    if (set_exec(K, edge.from, edge.to)) return; // already visited
    struct MirBlockData const *target = mir_bb_data(K->mir, edge.to);

    struct MirInstruction *const *pjoin;
    K_LIST_FOREACH (target->joins, pjoin)
        visit_phi(K, MirGetPhi(*pjoin), edge.to);

    if (has_single_incoming_edge(K, edge.to)) {
        struct MirInstruction *const *pinstr;
        K_LIST_FOREACH (target->instructions, pinstr)
            visit_expr(K, *pinstr, edge.to);
    }

    if (target->successors->count == 1) {
        MirBlock const s = K_LIST_FIRST(target->successors);
        FlowWorklist_push(K, K->flow, FLOW_EDGE(edge.to, s));
    }
}

static paw_Bool is_pure(struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirNoop:
        case kMirPhi:
        case kMirMove:
        case kMirAllocLocal:
        case kMirLoadConstant:
        case kMirCast:
        case kMirUpvalue:
        case kMirAggregate:
        case kMirContainer:
        case kMirGetField:
        case kMirUnaryOp:
        case kMirBinaryOp:
            return PAW_TRUE;
        default:
            return PAW_FALSE;
    }
}

static void transform_instr(struct KProp *K, struct MirInstruction *instr)
{
    MirPlacePtrList const *stores = pawMir_get_stores(K->mir, instr);
    if (is_pure(instr) && !MirIsPhi(instr) && stores->count == 1) {
        struct MirPlace const store = *K_LIST_FIRST(stores);
        struct Cell const cell = *get_cell(K, store);
        if (cell.info.kind == CELL_CONSTANT && is_stack_reg(K, store.r)) {
            into_load_k(instr, MIR_CONST(cell.info.k), store);
            return;
        }
    }

    MirPlacePtrList const *loads = pawMir_get_loads(K->mir, instr);
    struct MirPlace *const *ppload;
    K_LIST_FOREACH (loads, ppload) {
        struct Cell *cell = get_cell(K, **ppload);
        if ((*ppload)->kind == MIR_PLACE_LOCAL
                && cell->info.kind == CELL_CONSTANT
                && !is_stack_reg(K, (*ppload)->r)) {
            into_constant(K, *ppload, *cell);
        }
    }
}

static int prune_dead_successors(struct KProp *K, MirBlock from, MirBlock *ptarget)
{
    MirBlockList *successors = mir_bb_data(K->mir, from)->successors;
    int live_count = 0;

    if (successors->count < 2)
        return PAW_FALSE;

    int index;
    MirBlock const *pto;
    K_LIST_ENUMERATE (successors, index, pto) {
        if (is_exec(K, from, *pto)) {
            *ptarget = *pto;
            ++live_count;
        } else {
            int const ipred = mir_which_pred(K->mir, *pto, from);
            struct MirBlockData const *bto = mir_bb_data(K->mir, *pto);
            MirBlockList_remove(bto->predecessors, ipred);
        }
    }

    if (live_count == 1) {
        MirBlockList_set(successors, 0, *ptarget);
        successors->count = 1;
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void transform_code(struct KProp *K)
{
    int index = 0;
    struct MirBlockData *const *pblock;
    struct MirBlockDataList *blocks = K->mir->blocks;
    K_LIST_ENUMERATE (blocks, index, pblock) {
        struct MirBlockData const *block = *pblock;
        MirBlock const from = MIR_BB(index);

        {
            struct MirInstruction *const *pjoin;
            K_LIST_FOREACH (block->joins, pjoin)
                transform_instr(K, *pjoin);
        }

        {
            struct MirInstruction *const *pinstr;
            K_LIST_FOREACH (block->instructions, pinstr)
                transform_instr(K, *pinstr);
        }

        MirBlock to; // single live successor
        if (prune_dead_successors(K, from, &to))
            into_goto(K, K_LIST_LAST(block->instructions));
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
        while (K->flow->count > 0) {
            struct FlowEdge const edge = FlowWorklist_last(K->flow);
            FlowWorklist_pop(K->flow);
            process_flow_edge(K, edge);
        }
        while (K->ssa->count > 0) {
            struct SsaEdge const edge = SsaWorklist_last(K->ssa);
            SsaWorklist_pop(K->ssa);
            process_ssa_edge(K, edge);
        }
    }

    transform_code(K);
}

static void init_lattice(struct KProp *K)
{
    struct Mir const *mir = K->mir;
    int const cell_count = mir->registers->count + mir->constants->count;
    Lattice_reserve(K, K->lattice, cell_count);
    K->lattice->count = cell_count;

    int key = 0;
    int index;

    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE (mir->registers, index, pdata) {
        Lattice_set(K->lattice, key++,
            (struct Cell){
                .info.kind = CELL_TOP,
                .type = pdata->type,
                .r = MIR_REG(index),
            });
    }

    struct MirConstantData *kdata;
    K_LIST_ENUMERATE (mir->constants, index, kdata) {
        Lattice_set(K->lattice, key++,
            (struct Cell){
                .type = pawP_builtin_type(K->C, kdata->kind),
                .info.kind = CELL_CONSTANT,
                .info.v = kdata->value,
                .info.k = index,
            });

        ValueMap *map = kcache_map(K, kdata->kind);
        paw_Bool const replaced = ValueMap_insert(K->C, map, kdata->value, I2V(index));
        paw_assert(!replaced); PAW_UNUSED(replaced);
    }

    paw_assert(key == cell_count);

    // callee and arguments cannot be constant
    for (int i = 0; i < 1 + mir->param_size; ++i)
        K_LIST_AT(K->lattice, i).info.kind = CELL_BOTTOM;
}

static void account_for_uses(struct KProp *K, struct MirInstruction *instr, UseCountMap *uses)
{
    struct MirPlace *const *ppp;
    struct MirPlacePtrList *loads = pawMir_get_loads(K->mir, instr);
    K_LIST_FOREACH (loads, ppp) {
        if ((*ppp)->kind == MIR_PLACE_LOCAL) {
            int *pcount = UseCountMap_get(K, uses, (*ppp)->r);
            ++*pcount;
        }
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
    struct MirPlace *const *ppp;
    struct MirPlacePtrList const *loads = pawMir_get_loads(K->mir, instr);
    K_LIST_FOREACH (loads, ppp) {
        if ((*ppp)->kind == MIR_PLACE_LOCAL) {
            int *pcount = UseCountMap_get(K, counts, (*ppp)->r);
            paw_assert(*pcount > 0);
            --*pcount;
        }
    }
}

static paw_Bool filter_code(struct KProp *K, UseCountMap *counts, struct MirInstructionList *code)
{
    int const num_params = K->mir->param_size;

    int index;
    int num_removed = 0;
    struct MirInstruction *const *pinstr;
    K_LIST_ENUMERATE (code, index, pinstr) {
        struct MirInstruction *instr = *pinstr;
        if (is_pure(instr)) {
            // instruction has no side-effects
            int num_unused = 0;
            struct MirPlace *const *ppstore;
            MirPlacePtrList const *stores = pawMir_get_stores(K->mir, instr);
            K_LIST_FOREACH (stores, ppstore) {
                struct MirPlace const store = **ppstore;
                if (store.r.value > num_params) {
                    int const *pcount = UseCountMap_get(K, counts, store.r);
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

// Make the given register-to-register move instruction obsolete
// Accomplished by replacing uses of the destination register with the source register.
static void propagate_copy(struct KProp *K, struct MirMove const *move)
{
    struct MirAccess const *puse;
    struct MirAccessList const *uses = get_uses(K, move->output.r);
    K_LIST_FOREACH (uses, puse) {
        struct MirPlace *const *ppload;
        MirPlacePtrList const *loads = pawMir_get_loads(K->mir, puse->instr);
        K_LIST_FOREACH (loads, ppload) {
            if (MIR_ID_EQUALS((*ppload)->r, move->output.r)) {
                (*ppload)->r = move->target.r;
                K->altered = PAW_TRUE;
                break;
            }
        }
    }
}

static paw_Bool can_propagate(struct KProp *K, struct MirMove const *move)
{
    paw_assert(move->target.kind != MIR_PLACE_UPVALUE);
    paw_assert(move->output.kind == MIR_PLACE_LOCAL);
    if (move->target.kind == MIR_PLACE_CONSTANT)
        return PAW_FALSE; // loading a constant

    if (is_captured(K, move->target.r)) return PAW_FALSE;
    if (is_captured(K, move->output.r)) return PAW_FALSE;
    return !is_stack_reg(K, move->output.r);
}

static void propagate_copies(struct KProp *K)
{
    struct MirBlockData *const *pblock;
    struct MirInstruction *const *pinstr;
    K_LIST_FOREACH (K->mir->blocks, pblock) {
        struct MirBlockData *block = *pblock;
        struct MirInstructionList *instrs = MirInstructionList_new(K->mir);
        MirInstructionList_reserve(K->mir, instrs, block->instructions->count);
        K_LIST_FOREACH (block->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            if (MirIsMove(instr) && can_propagate(K, MirGetMove(instr))) {
                propagate_copy(K, MirGetMove(instr));
            } else {
                MirInstructionList_push(K->mir, instrs, instr);
            }
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

    K.kcache.ints = ValueMap_new_from(C, mir->pool);
    K.kcache.strs = ValueMap_new_from(C, mir->pool);
    K.kcache.floats = ValueMap_new_from(C, mir->pool);
    pawMir_collect_per_instr_uses(mir, K.uses);

    init_lattice(&K);
    propagate_copies(&K);
    propagate_constants(&K);
    clean_up_code(&K);

    mir->kcache = K.kcache;
    pawP_pool_free(C, K.pool);
    return K.altered;
}

void pawMir_propagate_constants(struct Mir *mir)
{
    paw_Bool altered;
    do {
        altered = propagate(mir);
    } while (altered);
}
