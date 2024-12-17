// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_MIR_H
#define PAW_MIR_H

#include "code.h"
#include "compile.h"

struct Mir;

// TODO: consider adopting rustc's Statement MIR node, could remove MirDiscard node
//       MirAllocLocal, MirFreeLocal should be statements
//       it would also be nice to be able to use constants directly, rather than
//       having to use OP_LOADK to put them in registers before they can be used
//       see Lua's RK[..] thing

#define MIR_INSTRUCTION_LIST(X) \
    X(Phi) \
    X(Move) \
    X(Upvalue) \
    X(Global) \
    X(AllocLocal) \
    X(FreeLocal) \
    X(SetLocal) \
    X(SetUpvalue) \
    X(Constant) \
    X(Aggregate) \
    X(Container) \
    X(Call) \
    X(Cast) \
    X(Close) \
    X(Closure) \
    X(SetElement) \
    X(GetElement) \
    X(SetRange) \
    X(GetRange) \
    X(SetField) \
    X(GetField) \
    X(UnaryOp) \
    X(BinaryOp) \
    X(Return) \
    X(Branch) \
    X(Switch) \
    X(ForLoop) \
    X(Goto)

 // TODO: MirBlock and MirRegister should start at 1 rather than 0 so that zero initialization can be used
 //       to instantiate lists containing these identifiers.

#define MIR_INVALID_BB MIR_BB(-1)
#define MIR_ROOT_BB MIR_BB(0)
#define MIR_BB_EQUALS(x, y) ((x).value == (y).value)
#define MIR_BB_EXISTS(x) (!MIR_BB_EQUALS(x, MIR_INVALID_BB))
#define MIR_BB(x) ((MirBlock){x})
typedef struct MirBlock {
    int value;
} MirBlock;

#define MIR_INVALID_REG MIR_REG(-1)
#define MIR_RESULT_REG MIR_REG(0)
#define MIR_REG_EQUALS(x, y) ((x).value == (y).value)
#define MIR_REG_EXISTS(x) (!MIR_REG_EQUALS(x, MIR_INVALID_REG))
#define MIR_REG(x) ((MirRegister){x})
typedef struct MirRegister {
    int value;
} MirRegister;

struct MirCaptureInfo {
    MirRegister r;
};

struct MirUpvalueInfo {
    paw_Bool is_local : 1;
    unsigned short index;
};

#define MIR_NO_SCOPE (struct MirScopeId){-1}
#define MIR_SCOPE_EQ(a, b) ((a).value == (b).value)
typedef struct MirScopeId {
    int value;
} MirScopeId;

struct MirScope {
    paw_Bool is_loop : 1;
    paw_Bool needs_close : 1;
    MirScopeId outer;
    MirScopeId id;
    int nlocals;
};

enum MirInstructionKind {
#define DEFINE_ENUM(X) kMir##X,
    MIR_INSTRUCTION_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define MIR_INSTRUCTION_HEADER K_ALIGNAS_NODE int line; \
                               int location; \
                               enum MirInstructionKind kind : 8

struct MirInstructionHeader {
    MIR_INSTRUCTION_HEADER;
};

struct MirMove {
    MIR_INSTRUCTION_HEADER;
    MirRegister output;
    MirRegister target;
};

struct MirUpvalue {
    MIR_INSTRUCTION_HEADER;
    int index;
    MirRegister output;
};

struct MirGlobal {
    MIR_INSTRUCTION_HEADER;
    MirRegister output;
};

struct MirPhi {
    MIR_INSTRUCTION_HEADER;
    struct MirRegisterList *inputs;
    MirRegister output;
    int var_id;
};

struct MirAllocLocal {
    MIR_INSTRUCTION_HEADER;
    MirRegister output;
    String *name;
};

struct MirFreeLocal {
    MIR_INSTRUCTION_HEADER;
    MirRegister reg;
};

struct MirSetLocal {
    MIR_INSTRUCTION_HEADER;
    MirRegister target;
    MirRegister value;
};

struct MirSetUpvalue {
    MIR_INSTRUCTION_HEADER;
    int index;
    MirRegister value;
};

struct MirConstant {
    MIR_INSTRUCTION_HEADER;
    paw_Type code;
    Value value;
    MirRegister output;
};

struct MirContainer {
    MIR_INSTRUCTION_HEADER;
    enum BuiltinKind b_kind : 8;
    int nelems;
    MirRegister output;
};

struct MirAggregate {
    MIR_INSTRUCTION_HEADER;
    int discr;
    int nfields;
    MirRegister output;
};

struct MirClose {
    MIR_INSTRUCTION_HEADER;
    MirRegister target;
};

struct MirClosure {
    MIR_INSTRUCTION_HEADER;
    int child_id;
    MirRegister output;
};

struct MirGetElement {
    MIR_INSTRUCTION_HEADER;
    enum BuiltinKind b_kind : 8;
    MirRegister output;
    MirRegister object;
    MirRegister key;
};

struct MirSetElement {
    MIR_INSTRUCTION_HEADER;
    enum BuiltinKind b_kind : 8;
    MirRegister object;
    MirRegister key;
    MirRegister value;
};

struct MirGetRange {
    MIR_INSTRUCTION_HEADER;
    enum BuiltinKind b_kind : 8;
    MirRegister output;
    MirRegister object;
    MirRegister lower;
    MirRegister upper;
};

struct MirSetRange {
    MIR_INSTRUCTION_HEADER;
    enum BuiltinKind b_kind : 8;
    MirRegister object;
    MirRegister lower;
    MirRegister upper;
    MirRegister value;
};

struct MirGetField {
    MIR_INSTRUCTION_HEADER;
    int index;
    MirRegister output;
    MirRegister object;
};

struct MirSetField {
    MIR_INSTRUCTION_HEADER;
    int index;
    MirRegister object;
    MirRegister value;
};

struct MirUnaryOp {
    MIR_INSTRUCTION_HEADER;
    enum UnaryOp op : 8;
    MirRegister val;
    MirRegister output;
};

struct MirBinaryOp {
    MIR_INSTRUCTION_HEADER;
    enum BinaryOp op : 8;
    MirRegister lhs;
    MirRegister rhs;
    MirRegister output;
};

struct MirCast {
    MIR_INSTRUCTION_HEADER;
    MirRegister target;
    MirRegister output;
    enum BuiltinKind from;
    enum BuiltinKind to;
};

struct MirCall {
    MIR_INSTRUCTION_HEADER;
    MirRegister target;
    struct MirRegisterList *args;
    MirRegister output;
};

struct MirBranch {
    MIR_INSTRUCTION_HEADER;
    MirRegister cond;
    MirBlock then_arm;
    MirBlock else_arm;
};

struct MirSwitchArm {
    MirBlock bid;
    Value value;
};

struct MirSwitch {
    MIR_INSTRUCTION_HEADER;
    MirRegister discr;
    struct MirSwitchArmList *arms;
    MirBlock otherwise;
};

enum MirForKind {
    MIR_FORNUM_PREP,
    MIR_FORNUM_LOOP,
};

struct MirForLoop {
    MIR_INSTRUCTION_HEADER;
    enum MirForKind for_kind;
    union {
        struct {
            MirRegister var;
            MirRegister begin;
            MirRegister end;
            MirRegister step;
        } fornum;
    };
    MirBlock then_arm;
    MirBlock else_arm;
};

struct MirGoto {
    MIR_INSTRUCTION_HEADER;
    MirBlock target;
};

struct MirReturn {
    MIR_INSTRUCTION_HEADER;
    MirRegister value;
};

struct MirInstruction {
    union {
        struct MirInstructionHeader hdr;
#define DEFINE_VARIANTS(X) struct Mir##X X##_;
        MIR_INSTRUCTION_LIST(DEFINE_VARIANTS)
#undef DEFINE_VARIANTS
    };
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool MirIs##X(const struct MirInstruction *node) { \
        return node->hdr.kind == kMir##X; \
    } \
    static inline struct Mir##X *MirGet##X(struct MirInstruction *node) { \
        paw_assert(MirIs##X(node)); \
        return &node->X##_; \
    }
    MIR_INSTRUCTION_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

static const char *kMirInstructionNames[] = {
#define DEFINE_NAME(X) "Mir" #X,
    MIR_INSTRUCTION_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

struct MirRegisterData {
    struct IrType *type;
    MirRegister hint;
};

struct MirBlockData {
    struct MirBlockList *predecessors;
    struct MirBlockList *successors;
    struct MirInstructionList *joins;
    struct MirInstructionList *instructions;
    MirBlock loop_end;
    int location;
};

// TODO: nested closures should be hoisted out into separate Mir objects, but this is complicated
//       for a few reasons, namely upvalues, generics, and naming.
struct Mir {
    struct MirRegisterDataList *registers;
    struct MirBlockDataList *blocks;
    struct MirScopeList *scopes;
    struct MirUpvalueList *upvalues;
    struct MirCaptureList *captured;
    struct MirBodyList *children;
    struct IrType *type;
    struct IrType *self;
    String *name;
    int ncaptured;
    enum FuncKind fn_kind : 8;
    paw_Bool is_native : 1;
    paw_Bool is_poly : 1;
    paw_Bool is_pub : 1;
};

#define MIR_KINDOF(node) ((node)->hdr.kind)
#define MIR_CAST_INSTRUCTION(p) CAST(struct MirInstruction *, p)

struct MirScope *pawMir_get_scope(struct Mir *mir, MirScopeId id);

DEFINE_LIST(struct Compiler, pawMir_capture_list_, MirCaptureList, struct MirCaptureInfo)
DEFINE_LIST(struct Compiler, pawMir_upvalue_list_, MirUpvalueList, struct MirUpvalueInfo)
DEFINE_LIST(struct Compiler, pawMir_scope_list_, MirScopeList, struct MirScope)
DEFINE_LIST(struct Compiler, pawMir_switch_list_, MirSwitchArmList, struct MirSwitchArm)
DEFINE_LIST(struct Compiler, pawMir_instruction_list_, MirInstructionList, struct MirInstruction *)
DEFINE_LIST(struct Compiler, pawMir_register_list_, MirRegisterList, MirRegister)
DEFINE_LIST(struct Compiler, pawMir_block_list_, MirBlockList, MirBlock)
DEFINE_LIST(struct Compiler, pawMir_bucket_list_, MirBucketList, struct MirBlockList *)
DEFINE_LIST(struct Compiler, pawMir_register_data_list_, MirRegisterDataList, struct MirRegisterData)
DEFINE_LIST(struct Compiler, pawMir_register_ptr_list_, MirRegisterPtrList, MirRegister *)
DEFINE_LIST(struct Compiler, pawMir_block_data_list_, MirBlockDataList, struct MirBlockData *)
DEFINE_LIST(struct Compiler, pawMir_body_list_, MirBodyList, struct Mir *)

struct Mir *pawMir_new(struct Compiler *C, String *name, struct IrType *type, struct IrType *self, enum FuncKind fn_kind, paw_Bool is_native, paw_Bool is_pub, paw_Bool is_poly);
struct MirLiveInterval *pawMir_new_interval(struct Compiler *C, MirRegister r, int npositions);
struct MirRegisterData *pawMir_new_register(struct Compiler *C, int value, struct IrType *type);
struct MirInstruction *pawMir_new_instruction(struct Compiler *C, enum MirInstructionKind kind);
struct MirTerminator *pawMir_new_return(struct Compiler *C, MirRegister value);
struct MirTerminator *pawMir_new_goto(struct Compiler *C, MirBlock bid);
struct MirBlockData *pawMir_new_block(struct Compiler *C);

struct MirLoad {
    struct MirRegisterPtrList *inputs;
};

struct MirStore {
    struct MirRegisterPtrList *outputs;
};

// Get a pointer to each variable read or written by a given instruction
paw_Bool pawMir_check_load(struct Compiler *C, struct MirInstruction *instr, struct MirLoad *pload);
paw_Bool pawMir_check_store(struct Compiler *C, struct MirInstruction *instr, struct MirStore *pstore);

static inline int mir_bb_first(const struct MirBlockData *block)
{
    return block->location;
}

static inline int mir_bb_last(const struct MirBlockData *block)
{
    if (block->instructions->count <= 0) return block->location + 2;
    return K_LIST_LAST(block->instructions)->hdr.location + 2;
}

static inline struct MirBlockData *mir_bb_data(struct Mir *mir, MirBlock bb)
{
    return K_LIST_GET(mir->blocks, bb.value);
}

static inline struct MirRegisterData *mir_reg_data(struct Mir *mir, MirRegister r)
{
    return &K_LIST_GET(mir->registers, r.value);
}

// Determine the index of "x" in the predecessor list of "y"
static int mir_which_pred(struct Mir *mir, MirBlock y, MirBlock x)
{
    const struct MirBlockData *data = mir_bb_data(mir, y);
    for (int i = 0; i < data->predecessors->count; ++i) {
        const MirBlock p = K_LIST_GET(data->predecessors, i);
        if (MIR_BB_EQUALS(x, p)) return i;
    }

    PAW_UNREACHABLE();
}

MirRegister pawMir_output_reg(struct MirInstruction *instr);

struct MirVisitor {
    struct Compiler *C;
    struct Mir *mir;
    void *ud;

    paw_Bool (*VisitTerminator)(struct MirVisitor *V, struct MirTerminator *node);
    paw_Bool (*VisitInstruction)(struct MirVisitor *V, struct MirInstruction *node);
    paw_Bool (*VisitBlock)(struct MirVisitor *V, MirBlock node);
    paw_Bool (*VisitRegister)(struct MirVisitor *V, MirRegister node);

    void (*PostVisitTerminator)(struct MirVisitor *V, struct MirTerminator *node);
    void (*PostVisitInstruction)(struct MirVisitor *V, struct MirInstruction *node);
    void (*PostVisitBlock)(struct MirVisitor *V, MirBlock node);
    void (*PostVisitRegister)(struct MirVisitor *V, MirRegister node);

#define DEFINE_CALLBACK(X) paw_Bool (*Visit##X)(struct MirVisitor *V, struct Mir##X *node); \
                           void (*PostVisit##X)(struct MirVisitor *V, struct Mir##X *node);
    MIR_INSTRUCTION_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawMir_visitor_init(struct MirVisitor *V, struct Compiler *C, struct Mir *mir, void *ud);
void pawMir_visit(struct MirVisitor *V);

// Visitor entrypoints for each kind of HIR node:
void pawMir_visit_instruction(struct MirVisitor *V, struct MirInstruction *node);
void pawMir_visit_block(struct MirVisitor *V, MirBlock node);
void pawMir_visit_register(struct MirVisitor *V, MirRegister node);
void pawMir_visit_instruction_list(struct MirVisitor *V, struct MirInstructionList *list);
void pawMir_visit_block_list(struct MirVisitor *V, struct MirBlockList *list);
void pawMir_visit_register_list(struct MirVisitor *V, struct MirRegisterList *list);

// Compute the immediate dominator of each basic block
struct MirBlockList *pawMir_compute_dominance_tree(struct Compiler *C, struct Mir *mir);

// Compute the dominance frontier set of each basic block
struct MirBucketList *pawMir_compute_dominance_frontiers(struct Compiler *C, struct Mir *mir, struct MirBlockList *idom);

// Return the reverse postorder (RPO) traversal of the CFG
// If there is a path from node X to node Y in the CFG, then X must have a lower RPO
// number than node Y.
struct MirBlockList *pawMir_traverse_rpo(struct Compiler *C, struct Mir *mir);

// Remove all basic blocks that cannot be reached from the entry
void pawMir_remove_unreachable_blocks(struct Compiler *C, struct Mir *mir, Map *uses, Map *defs);

// Approximation of the live range of a variable
// The variable corresponding to a given MirLiveInterval is live between instruction
// numbers "first" and "last", inclusive. Additionally, there might be lifetime holes,
// specified by regions of 0 bits in "ranges".
struct MirLiveInterval {
    struct MirInstruction *instr;
    struct BitSet *ranges;
    int first, last;
    MirRegister r;
};

DEFINE_LIST(struct Compiler, pawMir_interval_list_, MirIntervalList, struct MirLiveInterval *)

struct MirBlockList *pawMir_compute_live_in(struct Compiler *C, struct Mir *mir, struct MirBlockList *uses, struct MirBlockList *defs, MirRegister r);
struct MirIntervalList *pawMir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order);

// Push a human-readable representation of the MIR on to the stack
// Returns a pointer to the buffer containing null-terminated text.
const char *pawMir_dump(struct Compiler *C, struct Mir *mir);
const char *pawMir_dump_info(struct Compiler *C, struct Mir *mir);
const char *pawMir_dump_graph(struct Compiler *C, struct Mir *mir);

#endif // PAW_MIR_H

