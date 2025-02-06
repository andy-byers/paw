// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// propagate.c: Perform constant and copy propagation
//
// Uses the sparse conditional constant (SCC) algorithm from (1) for constant
// propagation.
//
// References:
// (1) Wegman, M., & Zadeck, F. K. (1991). Constant Propagation with Conditional
//     Branches.

#include <math.h>
#include "ir_type.h"
#include "map.h"
#include "mir.h"

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

DEFINE_LIST(struct Compiler, flow_worklist_, FlowWorklist, struct FlowEdge)
DEFINE_LIST(struct Compiler, ssa_worklist_, SsaWorklist, struct SsaEdge)
DEFINE_LIST(struct Compiler, exec_list_, ExecList, struct ExecFlag)
DEFINE_LIST(struct Compiler, lattice_, Lattice, struct Cell)

struct KProp {
    struct FlowWorklist *flow;
    struct SsaWorklist *ssa;
    struct Lattice *lattice;
    struct KCache kcache;
    Map *exec, *uses, *gotos;
    int nk;

    struct Mir *mir;
    struct Compiler *C;
    paw_Env *P;
};

#include <stdio.h>
#include <stdio.h>
static void dump_lattice(struct KProp *K)
{
    int index;
    struct Cell *pcell;
    printf("lattice = {\n");
    K_LIST_ENUMERATE(K->lattice, index, pcell) {
        printf("_%d: ", index);
        if (pcell->info.kind == CELL_TOP) {
            printf("⊤");
        } else if (pcell->info.kind == CELL_BOTTOM) {
            printf("⊥");
        } else {
            const char *type = pawIr_print_type(K->C, pcell->type);
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

static struct MirAccessList *get_uses(struct KProp *K, MirRegister r)
{
    paw_assert(MIR_REG_EXISTS(r));
    return MAP_GET(K->uses, I2V(r.value))->p;
}

static struct Cell *get_cell(struct KProp *K, MirRegister r)
{
    paw_assert(MIR_REG_EXISTS(r));
    return &K_LIST_GET(K->lattice, r.value);
}

static void save_goto(struct KProp *K, struct MirInstruction *instr, MirBlock to)
{
    MAP_INSERT(K->C, K->gotos, P2V(instr), I2V(to.value));
}

static struct ExecList *get_exec_list(struct KProp *K, MirBlock to)
{
    paw_assert(MIR_BB_EXISTS(to));
    const Value *pval = MAP_GET(K->exec, I2V(to.value));
    struct ExecList *list;
    if (pval == NULL) {
        list = exec_list_new(K->C);
        MAP_INSERT(K, K->exec, I2V(to.value), P2V(list));
    } else {
        list = pval->p;
    }
    return list;
}

// WARNING: calls to check_exec() invalidate flags returned by prior calls
static struct ExecFlag *check_exec(struct KProp *K, MirBlock from, MirBlock to)
{
    struct ExecFlag *pflag;
    struct ExecList *list = get_exec_list(K, to);
    K_LIST_FOREACH(list, pflag) {
        if (MIR_BB_EQUALS(pflag->from, from)) {
            return pflag;
        }
    }

    K_LIST_PUSH(K->C, list, EXEC_FLAG(from, PAW_FALSE));
    return &K_LIST_LAST(list);
}

static void visit_phi(struct KProp *K, struct MirPhi *phi, MirBlock to)
{
    struct MirBlockData *data = mir_bb_data(K->mir, to);
    struct Cell def = K_LIST_GET(K->lattice, phi->output.value);

    const MirBlock *pfrom;
    const MirRegister *pinput;
    K_LIST_ZIP(phi->inputs, pinput, data->predecessors, pfrom) {
        struct ExecFlag *pflag = check_exec(K, *pfrom, to);
        struct Cell *pcell = &K_LIST_GET(K->lattice, phi->output.value);
        pcell->info.kind = pflag->exec ? def.info.kind : CELL_TOP;
    }
}

static struct CellInfo meet(struct Cell *a, struct Cell *b)
{
    if (a->info.kind == CELL_TOP || b->info.kind == CELL_BOTTOM) return b->info;
    if (b->info.kind == CELL_TOP || a->info.kind == CELL_BOTTOM) return a->info;
    paw_assert(a->info.kind == CELL_CONSTANT);
    paw_assert(b->info.kind == CELL_CONSTANT);

    if (a->info.k == b->info.k) return a->info;
    return (struct CellInfo){.kind = CELL_BOTTOM};
}

const char *cellkind(const struct Cell *pcell)
{
    if (pcell->info.kind == CELL_BOTTOM) return "B";
    else if (pcell->info.kind == CELL_TOP) return "T";
    paw_assert(pcell->info.kind == CELL_CONSTANT);
    static char buffer[64];
    snprintf(buffer, sizeof(buffer), "%d", pcell->info.k);
    return buffer;
}

static void apply_meet_rules(struct KProp *K, struct Cell *output, struct MirRegisterPtrList *inputs)
{
    MirRegister *const *ppr;
    K_LIST_FOREACH(inputs, ppr) {
        struct Cell *input = get_cell(K, **ppr);
        output->info = meet(input, output);
    }
}

// TODO: share this code with codegen.c
static Map *kcache_map(struct KProp *K, enum BuiltinKind kind)
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
    if (kind <= BUILTIN_BOOL) kind = BUILTIN_INT;
    Map *map = kcache_map(K, kind);
    const Value *pk = MAP_GET(map, v);
    if (pk != NULL) return CAST(int, V_INT(*pk));
    MAP_INSERT(K, map, v, I2V(K->nk));
    return K->nk++;
}

#define BOTTOM_INFO() ((struct CellInfo){.kind = CELL_BOTTOM})
#define TOP_INFO() ((struct CellInfo){.kind = CELL_TOP})
#define CONST_INFO(num, val) ((struct CellInfo){.kind = CELL_CONSTANT, .k = (num), .v = (val)})

// TODO: move these macros somewhere they can be included here and in rt.c
//       in rt.c, define VM_*() operations that adapt the ones below to the VM

#define I2U(i) (CAST(uint64_t, i))
#define U2I(u) PAW_CAST_INT(u)

// Generate code for int operators
// Casts to unsigned to avoid UB (signed integer overflow). Requires
// 2's complement integer representation to work properly.
#define I_UNOP(a, op) U2I(op I2U(a))
#define I_BINOP(a, b, op) U2I(I2U(a) op I2U(b))

#define INT_UNARY_OP(r, val, op) \
        V_SET_INT(r, I_UNOP(V_INT(val), op))

#define FLOAT_UNARY_OP(r, val, op) \
        V_SET_FLOAT(r, op V_FLOAT(val))

#define INT_COMPARISON(r, x, y, op) \
        V_SET_BOOL(r, V_INT(x) op V_INT(y))

#define INT_BINARY_OP(r, x, y, op) \
        V_SET_INT(r, I_BINOP(V_INT(x), V_INT(y), op))

#define FLOAT_COMPARISON(r, x, y, op) \
        V_SET_BOOL(r, V_FLOAT(x) op V_FLOAT(y))

#define FLOAT_BINARY_OP(r, x, y, op) \
        V_SET_FLOAT(r, V_FLOAT(x) op V_FLOAT(y))

#define STR_COMPARISON(r, x, y, op) \
        V_SET_BOOL(r, pawS_cmp(V_STRING(x), V_STRING(y)) op 0)

static struct CellInfo visit_unary_op(struct KProp *K, struct Cell *val, enum UnaryOp op)
{
    enum BuiltinKind kind = pawP_type2code(K->C, val->type);
    const Value v = val->info.v;
    Value r;

    switch (op) {
        case UNARY_LEN:
            if (kind == BUILTIN_STR) {
                const String *x = V_STRING(v);
                V_SET_INT(&r, x->length);
                break;
            }
            return BOTTOM_INFO();
        case UNARY_NEG:
            if (kind == BUILTIN_INT) {
                INT_UNARY_OP(&r, v, -);
            } else {
                paw_assert(kind == BUILTIN_FLOAT);
                FLOAT_UNARY_OP(&r, v, -);
            }
            break;
        case UNARY_NOT:
            paw_assert(kind == BUILTIN_BOOL);
            INT_UNARY_OP(&r, v, !);
            break;
        case UNARY_BNOT:
            paw_assert(kind == BUILTIN_INT);
            INT_UNARY_OP(&r, v, ~);
            break;
    }
    const int k = add_constant(K, r, kind);
    return CONST_INFO(k, r);
}

static void constant_mul(Value *pr, Value x, Value y, enum BuiltinKind kind)
{
    if (kind == BUILTIN_FLOAT) {
        // need to use float comparison to handle -0.0
        if (V_FLOAT(x) == 0.0 || V_FLOAT(y) == 0.0) V_SET_0(pr);
        else FLOAT_BINARY_OP(pr, x, y, *);
    } else {
        if (V_INT(x) == 0 || V_INT(y) == 0) V_SET_0(pr);
        else INT_BINARY_OP(pr, x, y, *);
    }
}

#define DIVIDE_BY_0(K) pawE_error(ENV(K), PAW_EVALUE, -1, "divide by 0");

static void constant_div(struct KProp *K, Value *pr, Value x, Value y, enum BuiltinKind kind)
{

    if (kind == BUILTIN_FLOAT) {
        if (V_FLOAT(y) == 0.0) DIVIDE_BY_0(K);
        FLOAT_BINARY_OP(pr, x, y, /);
    } else {
        paw_assert(kind == BUILTIN_INT);
        if (V_INT(y) == 0) DIVIDE_BY_0(K);
        INT_BINARY_OP(pr, x, y, /);
    }
}

static void constant_mod(struct KProp *K, Value *pr, Value x, Value y, enum BuiltinKind kind)
{

    if (kind == BUILTIN_FLOAT) {
        if (V_FLOAT(y) == 0.0) DIVIDE_BY_0(K);
        V_SET_FLOAT(pr, fmod(V_FLOAT(x), V_FLOAT(y)));
    } else {
        paw_assert(kind == BUILTIN_INT);
        if (V_INT(y) == 0) DIVIDE_BY_0(K);
        INT_BINARY_OP(pr, x, y, %);
    }
}

static struct CellInfo visit_binary_op(struct KProp *K, struct Cell *lhs, struct Cell *rhs, enum BinaryOp op)
{
    enum BuiltinKind kind = pawP_type2code(K->C, lhs->type);
    const Value x = lhs->info.v;
    const Value y = rhs->info.v;
    Value r;

    switch (op) {
        case BINARY_EQ: {
            if (kind == BUILTIN_FLOAT) FLOAT_COMPARISON(&r, x, y, ==);
            else if (kind == BUILTIN_STR) STR_COMPARISON(&r, x, y, ==);
            else if (kind == BUILTIN_BOOL) INT_COMPARISON(&r, x, y, ==);
            else if (kind == BUILTIN_INT) INT_COMPARISON(&r, x, y, ==);
            else return BOTTOM_INFO();
            break;
        }
        case BINARY_NE:
            if (kind == BUILTIN_FLOAT) FLOAT_COMPARISON(&r, x, y, !=);
            else if (kind == BUILTIN_STR) STR_COMPARISON(&r, x, y, !=);
            else if (kind == BUILTIN_BOOL) INT_COMPARISON(&r, x, y, !=);
            else if (kind == BUILTIN_INT) INT_COMPARISON(&r, x, y, !=);
            else return BOTTOM_INFO();
            break;
        case BINARY_LT:
            if (kind == BUILTIN_FLOAT) FLOAT_COMPARISON(&r, x, y, <);
            else if (kind == BUILTIN_STR) STR_COMPARISON(&r, x, y, <);
            else INT_COMPARISON(&r, x, y, <);
            break;
        case BINARY_LE:
            if (kind == BUILTIN_FLOAT) FLOAT_COMPARISON(&r, x, y, <=);
            else if (kind == BUILTIN_STR) STR_COMPARISON(&r, x, y, <=);
            else INT_COMPARISON(&r, x, y, <=);
            break;
        case BINARY_GT:
            if (kind == BUILTIN_FLOAT) FLOAT_COMPARISON(&r, x, y, >);
            else if (kind == BUILTIN_STR) STR_COMPARISON(&r, x, y, >);
            else INT_COMPARISON(&r, x, y, >);
            break;
        case BINARY_GE:
            if (kind == BUILTIN_FLOAT) FLOAT_COMPARISON(&r, x, y, >=);
            else if (kind == BUILTIN_STR) STR_COMPARISON(&r, x, y, >=);
            else INT_COMPARISON(&r, x, y, >=);
            break;
        case BINARY_AS:
            return BOTTOM_INFO();
        case BINARY_ADD:
            // TODO: concatenation should use a different operator, it's annoying do deal with the binary operator special case
            if (kind == BUILTIN_FLOAT) FLOAT_BINARY_OP(&r, x, y, +);
            else if (kind == BUILTIN_STR) return BOTTOM_INFO();
            else if (kind == BUILTIN_LIST) return BOTTOM_INFO();
            else INT_BINARY_OP(&r, x, y, +);
            break;
        case BINARY_SUB:
            if (kind == BUILTIN_FLOAT) FLOAT_BINARY_OP(&r, x, y, -);
            else INT_BINARY_OP(&r, x, y, -);
            break;
        case BINARY_MUL:
            constant_mul(&r, x, y, kind);
            break;
        case BINARY_DIV:
            constant_div(K, &r, x, y, kind);
            break;
        case BINARY_MOD:
            constant_mod(K, &r, x, y, kind);
            break;
        case BINARY_BXOR:
            paw_assert(kind == BUILTIN_INT);
            INT_BINARY_OP(&r, x, y, ^);
            break;
        case BINARY_BAND:
            paw_assert(kind == BUILTIN_INT);
            INT_BINARY_OP(&r, x, y, &);
            break;
        case BINARY_BOR:
            paw_assert(kind == BUILTIN_INT);
            INT_BINARY_OP(&r, x, y, |);
            break;
        case BINARY_SHL: {
            paw_assert(kind == BUILTIN_INT);
            paw_Int n = V_INT(y);
            if (n < 0) {
                pawE_error(ENV(K), PAW_EVALUE, -1, "negative shift count");
            } else if (n > 0) {
                n = PAW_MIN(n, U2I(sizeof(x) * 8 - 1));
                V_SET_INT(&r, U2I(V_UINT(x) << n));
            } else {
                r = x;
            }
            break;
        }
        case BINARY_SHR:
            paw_assert(kind == BUILTIN_INT);
            paw_Int n = V_INT(y);
            if (n < 0) {
                pawE_error(ENV(K), PAW_EVALUE, -1, "negative shift count");
            } else if (n > 0) {
                n = PAW_MIN(n, U2I(sizeof(x) * 8 - 1));
                V_SET_INT(&r, V_INT(x) >> n);
            } else {
                r = x;
            }
            break;
    }
    const int k = add_constant(K, r, kind);
    return CONST_INFO(k, r);
}

#define LIST_SWAP_REMOVE(list, i) (K_LIST_SET(list, i, K_LIST_LAST(list)), \
                              K_LIST_POP(list))

// Remove the control flow edge between "from" and "to"
static void remove_edge(struct KProp *K, MirBlock from, MirBlock to)
{
    struct MirBlockData *bfrom = mir_bb_data(K->mir, from);
    struct MirBlockData *bto = mir_bb_data(K->mir, to);
    const int ipred = mir_which_pred(K->mir, to, from);
    const int isucc = mir_which_succ(K->mir, from, to);
    LIST_SWAP_REMOVE(bfrom->successors, isucc);
    LIST_SWAP_REMOVE(bto->predecessors, ipred);
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
    const Value target = pcell->info.v;
    MirBlock result;

    struct MirSwitchArm *parm;
    K_LIST_FOREACH(s->arms, parm)  {
        if ((kind != BUILTIN_FLOAT && V_UINT(parm->value) == V_UINT(target))
                || (kind == BUILTIN_FLOAT && V_FLOAT(parm->value) == V_FLOAT(target))) {
            return parm->bid; // comparison needs to handle "-0.0 == 0.0"
        }
    }
    if (MIR_BB_EXISTS(s->otherwise)) return s->otherwise;

    PAW_UNREACHABLE();
}

// Add def-use edges starting at the given definition to the SSA worklist
static void add_use_edges(struct KProp *K, MirRegister def)
{
    struct MirAccess *puse;
    struct MirAccessList *uses = get_uses(K, def);
    K_LIST_FOREACH(uses, puse) {
        struct SsaEdge edge = SSA_EDGE(def, puse->instr, puse->b);
        K_LIST_PUSH(K->C, K->ssa, edge);
    }
}

static void into_goto(struct KProp *K, struct MirInstruction *instr, MirBlock b)
{
    instr->Goto_ = (struct MirGoto){
        .kind = kMirGoto,
        .line = instr->hdr.line,
        .mid = instr->hdr.mid,
        .target = b,
    };
}

static void into_constant(struct KProp *K, struct MirInstruction *instr, struct Cell cell)
{
    instr->Constant_ = (struct MirConstant){
        .kind = kMirConstant,
        .line = instr->hdr.line,
        .b_kind = pawP_type2code(K->C, cell.type),
        .mid = instr->hdr.mid,
        .value = cell.info.v,
        .output = cell.r,
    };
}

static void visit_expr(struct KProp *K, struct MirInstruction *instr, MirBlock b)
{
    switch (MIR_KINDOF(instr)) {
        case kMirMove: {
            struct MirMove *x = MirGetMove(instr);
            struct Cell *target = get_cell(K, x->target);
            struct Cell *output = get_cell(K, x->output);
            enum CellKind old = output->info.kind;
            if (target->info.kind == CELL_CONSTANT) {
                output->info = target->info;
            } else {
                output->info = BOTTOM_INFO();
            }
            if (old != output->info.kind) {
                add_use_edges(K, output->r);
            }
            break;
        }
        case kMirConstant: {
            struct MirConstant *x = MirGetConstant(instr);
            const int k = add_constant(K, x->value, x->b_kind);
            struct Cell *output = get_cell(K, x->output);
            enum CellKind old = output->info.kind;
            output->info = CONST_INFO(k, x->value);
            if (old != CELL_CONSTANT) {
                add_use_edges(K, output->r);
            }
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *x = MirGetUnaryOp(instr);
            struct Cell *val = get_cell(K, x->val);
            struct Cell *output = get_cell(K, x->output);
            enum CellKind old = output->info.kind;
            if (val->info.kind == CELL_CONSTANT) {
                output->info = visit_unary_op(K, val, x->op);
            } else {
                output->info = BOTTOM_INFO();
            }
            if (old != output->info.kind) {
                add_use_edges(K, output->r);
            }
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *x = MirGetBinaryOp(instr);
            struct Cell *lhs = get_cell(K, x->lhs);
            struct Cell *rhs = get_cell(K, x->rhs);
            struct Cell *output = get_cell(K, x->output);
            enum CellKind old = output->info.kind;
            if (lhs->info.kind == CELL_CONSTANT && rhs->info.kind == CELL_CONSTANT) {
                output->info = visit_binary_op(K, lhs, rhs, x->op);
            } else {
                output->info = BOTTOM_INFO();
            }
            if (old != output->info.kind) {
                add_use_edges(K, output->r);
            }
            break;
        }
        case kMirBranch: {
            struct MirBranch *x = MirGetBranch(instr);
            struct Cell *cond = get_cell(K, x->cond);
            if (cond->info.kind == CELL_CONSTANT) {
                const MirBlock s = single_branch_target(K, x, cond, b);
                K_LIST_PUSH(K->C, K->flow, FLOW_EDGE(b, s));
                save_goto(K, instr, s);
            }
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *x = MirGetSwitch(instr);
            struct Cell *discr = get_cell(K, x->discr);
            if (discr->info.kind == CELL_CONSTANT) {
                const MirBlock s = single_switch_target(K, x, discr, b);
                K_LIST_PUSH(K->C, K->flow, FLOW_EDGE(b, s));
                save_goto(K, instr, s);
            }
            break;
        }
        case kMirFreeLocal:
        case kMirClose:
        case kMirSetElement:
        case kMirSetRange:
        case kMirSetField:
        case kMirReturn:
        case kMirGoto:
             return; // no output
        default: {
            const MirRegister *pstore = pawMir_get_store(K->C, instr);
            if (pstore != NULL) {
                struct Cell *output = get_cell(K, *pstore);
                output->info = BOTTOM_INFO();
                add_use_edges(K, *pstore);
            }
        }
    }
}

static void process_ssa_edge(struct KProp *K)
{
    struct SsaEdge edge = K_LIST_LAST(K->ssa);
    K_LIST_POP(K->ssa);
    if (MirIsPhi(edge.use)) {
        visit_phi(K, MirGetPhi(edge.use), edge.b);
        return;
    }

    const struct ExecFlag *pflag;
    struct ExecList *exec = get_exec_list(K, edge.b);
    K_LIST_FOREACH(exec, pflag) {
        if (pflag->exec) {
            visit_expr(K, edge.use, edge.b);
            break;
        }
    }
}

static paw_Bool has_single_incoming_edge(struct KProp *K, MirBlock b)
{
    int found = 0;
    const MirBlock *pb;
    struct MirBlockData *block = mir_bb_data(K->mir, b);
    K_LIST_FOREACH(block->predecessors, pb) {
        struct ExecFlag *flag = check_exec(K, *pb, b);
        if (flag->exec && ++found > 1) return PAW_FALSE;
    }
    return found == 1;
}

static void process_flow_edge(struct KProp *K)
{
    struct FlowEdge edge = K_LIST_LAST(K->flow);
    struct MirBlockData *target = mir_bb_data(K->mir, edge.to);
    K_LIST_POP(K->flow);

    struct ExecFlag *flag = check_exec(K, edge.from, edge.to);
    if (flag->exec) return;
    flag->exec = PAW_TRUE;

    struct MirInstruction *const *pjoin;
    K_LIST_FOREACH(target->joins, pjoin) {
        visit_phi(K, MirGetPhi(*pjoin), edge.to);
    }

    if (has_single_incoming_edge(K, edge.to)) {
        struct MirInstruction *const *pinstr;
        struct MirBlockData *block = mir_bb_data(K->mir, edge.to);
        K_LIST_FOREACH(block->instructions, pinstr) {
            visit_expr(K, *pinstr, edge.to);
        }
    }

    if (target->successors->count == 1) {
        const MirBlock s = K_LIST_FIRST(target->successors);
        K_LIST_PUSH(K->C, K->flow, FLOW_EDGE(edge.to, s));
    }
}

static void propagate_constants(struct KProp *K)
{
    {
        // initialize the flow worklist with edges leading out of the start node
        const MirBlock *pb;
        struct MirBlockData *start = K_LIST_FIRST(K->mir->blocks);
        K_LIST_FOREACH(start->successors, pb) {
            K_LIST_PUSH(K->C, K->flow, FLOW_EDGE(MIR_ROOT_BB, *pb));
        }
    }

    while (K->flow->count > 0 || K->ssa->count > 0) {
        if (K->flow->count > 0) process_flow_edge(K);
        if (K->ssa->count > 0) process_ssa_edge(K);
    }
}

static void init_lattice(struct KProp *K)
{
    int rid;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE(K->mir->registers, rid, pdata) {
        const MirRegister r = MIR_REG(rid);
        K_LIST_SET(K->lattice, r.value, ((struct Cell){
            .info.kind = CELL_TOP,
            .type = pdata->type,
            .r = r,
        }));
    }
}

static void account_for_uses(struct Compiler *C, struct MirInstruction *instr, Map *uses)
{
    MirRegister *const *ppr;
    struct MirRegisterPtrList *loads = pawMir_get_loads(C, instr);
    K_LIST_FOREACH(loads, ppr) {
        Value *pval = MAP_GET(uses, I2V((*ppr)->value));
        ++V_INT(*pval);
    }
}

static void count_uses(struct Mir *mir, Map *uses)
{
    struct Compiler *C = mir->C;

    int index;
    struct MirRegisterData *pdata;
    K_LIST_ENUMERATE(mir->registers, index, pdata) {
        // being captured in 1 or more closures counts as a single usage
        MAP_INSERT(C, uses, I2V(index), I2V(pdata->is_captured));
    }

    struct MirBlockData **pblock;
    K_LIST_FOREACH(mir->blocks, pblock) {
        struct MirBlockData *block = *pblock;
        struct MirInstruction **pinstr;
        K_LIST_FOREACH(block->joins, pinstr) account_for_uses(C, *pinstr, uses);
        K_LIST_FOREACH(block->instructions, pinstr) account_for_uses(C, *pinstr, uses);
    }
}

static void remove_operand_uses(struct KProp *K, Map *counts, struct MirInstruction *instr)
{
    MirRegister *const *ppr;
    struct MirRegisterPtrList *loads = pawMir_get_loads(K->C, instr);
    K_LIST_FOREACH(loads, ppr) {
        Value *pval = MAP_GET(counts, I2V((*ppr)->value));
        paw_assert(V_INT(*pval) > 0);
        --V_INT(*pval);
    }
}

static paw_Bool is_pure(struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirPhi:
        case kMirMove:
        case kMirConstant:
        case kMirCast:
        case kMirAggregate:
        case kMirContainer:
        case kMirSetLocal:
        case kMirGetElement:
        case kMirGetRange:
        case kMirGetField:
        case kMirUnaryOp:
        case kMirBinaryOp:
            return PAW_TRUE;
            // TODO: once this is figured out, squish the below cases into "default"
        case kMirUpvalue:
        case kMirGlobal:
        case kMirAllocLocal:
        case kMirFreeLocal:
        case kMirSetUpvalue:
        case kMirCall:
        case kMirClose:
        case kMirClosure:
        case kMirSetElement:
        case kMirSetRange:
        case kMirSetField:
        case kMirReturn:
        case kMirBranch:
        case kMirSwitch:
        case kMirGoto:
            return PAW_FALSE;
    }
}

static void prune_dead_successors(struct KProp *K, MirBlock from, MirBlock target)
{
    int index, itarget;
    const MirBlock *pto;
    struct MirBlockData *bfrom = mir_bb_data(K->mir, from);
    K_LIST_ENUMERATE(bfrom->successors, index, pto) {
        if (!MIR_REG_EQUALS(*pto, target)) {
            const int ipred = mir_which_pred(K->mir, *pto, from);
            struct MirBlockData *bto = mir_bb_data(K->mir, *pto);
            K_LIST_REMOVE(bto->predecessors, ipred);
        } else {
            itarget = index;
        }
    }
    K_LIST_SET(bfrom->successors, 0, target);
    bfrom->successors->count = 1;
}

static void transform_instr(struct KProp *K, struct MirInstruction *instr)
{
    const MirRegister *pstore = pawMir_get_store(K->C, instr);
    if (pstore == NULL) return;
    struct Cell *cell = get_cell(K, *pstore);
    if (cell->info.kind == CELL_CONSTANT) {
        // all operands to this instruction are constant
        into_constant(K, instr, *cell);
    }
}

static void transform_code(struct KProp *K)
{
    int index = 0;
    struct MirBlockData **pblock;
    struct MirInstruction **pinstr;
    struct MirBlockDataList *blocks = K->mir->blocks;
    K_LIST_ENUMERATE(blocks, index, pblock) {
        const MirBlock from = MIR_BB(index);
        struct MirBlockData *block = *pblock;
        K_LIST_FOREACH(block->joins, pinstr) {
            transform_instr(K, *pinstr);
        }
        K_LIST_FOREACH(block->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            const Value *pval = MAP_GET(K->gotos, P2V(instr));
            if (pval != NULL) {
                // instruction is a branch/switch controlled by a constant
                const MirBlock to = MIR_BB(pval->i);
                prune_dead_successors(K, from, to);
                into_goto(K, instr, to);
            } else {
                transform_instr(K, instr);
            }
        }
    }
}

static paw_Bool filter_code(struct KProp *K, Map *counts, struct MirInstructionList *code)
{
    int index;
    int removed = 0;
    paw_Bool removed_any = PAW_FALSE;
    struct MirInstruction **pinstr;
    K_LIST_ENUMERATE(code, index, pinstr) {
        struct MirInstruction *instr = *pinstr;
        if (is_pure(instr)) {
            const MirRegister *pstore = pawMir_get_store(K->C, instr);
            if (pstore != NULL) {
                const Value *pval = MAP_GET(counts, I2V(pstore->value));
                if (V_INT(*pval) == 0) {
                    remove_operand_uses(K, counts, instr);
                    ++removed;
                    continue;
                }
            }
        }
        struct MirInstruction *keep = K_LIST_GET(code, index);
        K_LIST_SET(code, index - removed, keep);
        removed_any |= removed > 0;
    }
    code->count -= removed;
    return removed_any;
}

static paw_Bool remove_dead_code(struct KProp *K, Map *counts)
{
    paw_Bool removed_any = PAW_FALSE;
    struct MirBlockDataList *blocks = K->mir->blocks;
    for (int i = blocks->count - 1; i >= 0; --i) {
        struct MirBlockData *block = K_LIST_GET(blocks, i);
        removed_any |= filter_code(K, counts, block->joins);
        removed_any |= filter_code(K, counts, block->instructions);
    }
    return removed_any;
}

static void clean_up_code(struct KProp *K, struct ObjectStore *store)
{
    pawMir_remove_unreachable_blocks(K->mir);

    Map *counts = pawP_new_map(K->C, store);
    count_uses(K->mir, counts);

    paw_Bool removed_code;
    do {
        removed_code = remove_dead_code(K, counts);
    } while (removed_code);
}

static void propagate_copy(struct KProp *K, struct MirMove *move)
{
    struct MirAccess *puse;
    MirRegister *const *ppr;
    struct MirAccessList *uses = get_uses(K, move->output);
    K_LIST_FOREACH(uses, puse) {
        struct MirRegisterPtrList *loads = pawMir_get_loads(K->C, puse->instr);
        K_LIST_FOREACH(loads, ppr) {
            if (MIR_REG_EQUALS(**ppr, move->output)) {
                // replace output with operand
                **ppr = move->target;
                break;
            }
        }
    }
}

static paw_Bool can_propagate_copy(struct KProp *K, struct MirInstruction *instr)
{
    if (!MirIsMove(instr)) return PAW_FALSE;
    const MirRegister output = MirGetMove(instr)->output;
    const struct MirRegisterData *data = mir_reg_data(K->mir, output);
    return !data->is_captured;
}

static void propagate_copies(struct KProp *K)
{
    struct MirBlockData **pblock;
    struct MirInstruction **pinstr;
    K_LIST_FOREACH(K->mir->blocks, pblock) {
        struct MirBlockData *block = *pblock;
        struct MirInstructionList *instrs = pawMir_instruction_list_new(K->C);
        K_LIST_RESERVE(K->C, instrs, block->instructions->count);
        K_LIST_FOREACH(block->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            if (MirIsMove(instr)) {
                struct MirMove *move = MirGetMove(instr);
                struct MirRegisterData *data = mir_reg_data(K->mir, move->output);
                if (!data->is_captured) {
                    propagate_copy(K, move);
                    continue;
                }
            }
            K_LIST_PUSH(K->C, instrs, instr);
        }
        block->instructions = instrs;
    }
}

void pawMir_propagate_constants(struct Mir *mir)
{
#ifdef PAW_KPROP_OFF
    return;
#endif

    struct Compiler *C = mir->C;
    struct KProp K = {
        .lattice = lattice_new(C),
        .flow = flow_worklist_new(C),
        .ssa = ssa_worklist_new(C),
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    K_LIST_RESERVE(C, K.lattice, mir->registers->count);
    K.lattice->count = mir->registers->count;

    struct ObjectStore store;
    pawP_push_store(C, &store);
    K.exec = pawP_new_map(C, &store);
    K.uses = pawP_new_map(C, &store);
    K.gotos = pawP_new_map(C, &store);
    K.kcache.ints = pawP_new_map(C, &store);
    K.kcache.strs = pawP_new_map(C, &store);
    K.kcache.flts = pawP_new_map(C, &store);
    pawMir_collect_per_instr_uses(mir, K.uses);

    init_lattice(&K);
    propagate_copies(&K);
    propagate_constants(&K);
    transform_code(&K);
    clean_up_code(&K, &store);

    --ENV(C)->top.p; // pop "store" map
}
