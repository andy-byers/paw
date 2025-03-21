// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_MIR_H
#define PAW_MIR_H

#include "compile.h"

struct Mir;

#define MIR_INSTRUCTION_LIST(X) \
    X(Phi)                      \
    X(Move)                     \
    X(Upvalue)                  \
    X(Global)                   \
    X(AllocLocal)               \
    X(FreeLocal)                \
    X(SetUpvalue)               \
    X(Constant)                 \
    X(Aggregate)                \
    X(Container)                \
    X(Call)                     \
    X(Cast)                     \
    X(Capture)                  \
    X(Close)                    \
    X(Closure)                  \
    X(SetElement)               \
    X(GetElement)               \
    X(SetRange)                 \
    X(GetRange)                 \
    X(SetField)                 \
    X(GetField)                 \
    X(UnaryOp)                  \
    X(BinaryOp)                 \
    X(Return)                   \
    X(Branch)                   \
    X(Switch)                   \
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

typedef struct MirId {
    int value;
} MirId;

inline static MirId pawMir_next_id(struct Mir *mir);

struct MirCaptureInfo {
    MirRegister r;
};

struct MirUpvalueInfo {
    paw_Bool is_local : 1;
    unsigned short index;
};

enum MirInstructionKind {
#define DEFINE_ENUM(X) kMir##X,
    MIR_INSTRUCTION_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define MIR_INSTRUCTION_HEADER \
    struct SourceLoc loc;      \
    MirId mid;                 \
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
    int global_id;
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

struct MirSetUpvalue {
    MIR_INSTRUCTION_HEADER;
    int index;
    MirRegister value;
};

struct MirConstant {
    MIR_INSTRUCTION_HEADER;
    enum BuiltinKind b_kind : 8;
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
    int nfields;
    MirRegister output;
};

struct MirCapture {
    MIR_INSTRUCTION_HEADER;
    MirRegister target;
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
    MIR_FOR_PREP,
    MIR_FOR_LOOP,
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

#define DEFINE_ACCESS(X)                                                \
    static inline paw_Bool MirIs##X(struct MirInstruction const *node)  \
    {                                                                   \
        return node->hdr.kind == kMir##X;                               \
    }                                                                   \
    static inline struct Mir##X *MirGet##X(struct MirInstruction *node) \
    {                                                                   \
        paw_assert(MirIs##X(node));                                     \
        return &node->X##_;                                             \
    }
MIR_INSTRUCTION_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

static char const *kMirInstructionNames[] = {
#define DEFINE_NAME(X) "Mir" #X,
    MIR_INSTRUCTION_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

struct MirInstruction *pawMir_new_instruction(struct Mir *mir);

inline static struct MirInstruction *pawMir_new_move(struct Mir *mir, struct SourceLoc loc, MirRegister output, MirRegister target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Move_ = (struct MirMove){
        .mid = pawMir_next_id(mir),
        .kind = kMirMove,
        .loc = loc,
        .output = output,
        .target = target,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_upvalue(struct Mir *mir, struct SourceLoc loc, MirRegister output, int index)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Upvalue_ = (struct MirUpvalue){
        .mid = pawMir_next_id(mir),
        .kind = kMirUpvalue,
        .loc = loc,
        .index = index,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_global(struct Mir *mir, struct SourceLoc loc, MirRegister output, int global_id)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Global_ = (struct MirGlobal){
        .mid = pawMir_next_id(mir),
        .kind = kMirGlobal,
        .loc = loc,
        .output = output,
        .global_id = global_id,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_phi(struct Mir *mir, struct SourceLoc loc, struct MirRegisterList *inputs, MirRegister output, int var_id)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Phi_ = (struct MirPhi){
        .mid = pawMir_next_id(mir),
        .kind = kMirPhi,
        .loc = loc,
        .inputs = inputs,
        .output = output,
        .var_id = var_id,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_alloc_local(struct Mir *mir, struct SourceLoc loc, String *name, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->AllocLocal_ = (struct MirAllocLocal){
        .mid = pawMir_next_id(mir),
        .kind = kMirAllocLocal,
        .loc = loc,
        .output = output,
        .name = name,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_free_local(struct Mir *mir, struct SourceLoc loc, MirRegister reg)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->FreeLocal_ = (struct MirFreeLocal){
        .mid = pawMir_next_id(mir),
        .kind = kMirFreeLocal,
        .loc = loc,
        .reg = reg,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_set_upvalue(struct Mir *mir, struct SourceLoc loc, int index, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetUpvalue_ = (struct MirSetUpvalue){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetUpvalue,
        .loc = loc,
        .index = index,
        .value = value,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_constant(struct Mir *mir, struct SourceLoc loc, enum BuiltinKind b_kind, Value value, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Constant_ = (struct MirConstant){
        .mid = pawMir_next_id(mir),
        .kind = kMirConstant,
        .loc = loc,
        .b_kind = b_kind,
        .value = value,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_container(struct Mir *mir, struct SourceLoc loc, enum BuiltinKind b_kind, int nelems, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Container_ = (struct MirContainer){
        .mid = pawMir_next_id(mir),
        .kind = kMirContainer,
        .loc = loc,
        .b_kind = b_kind,
        .nelems = nelems,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_aggregate(struct Mir *mir, struct SourceLoc loc, int nfields, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Aggregate_ = (struct MirAggregate){
        .mid = pawMir_next_id(mir),
        .kind = kMirAggregate,
        .loc = loc,
        .nfields = nfields,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_capture(struct Mir *mir, struct SourceLoc loc, MirRegister target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Capture_ = (struct MirCapture){
        .mid = pawMir_next_id(mir),
        .kind = kMirCapture,
        .loc = loc,
        .target = target,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_close(struct Mir *mir, struct SourceLoc loc, MirRegister target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Close_ = (struct MirClose){
        .mid = pawMir_next_id(mir),
        .kind = kMirClose,
        .loc = loc,
        .target = target,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_closure(struct Mir *mir, struct SourceLoc loc, int child_id, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Closure_ = (struct MirClosure){
        .mid = pawMir_next_id(mir),
        .kind = kMirClosure,
        .loc = loc,
        .child_id = child_id,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_get_element(struct Mir *mir, struct SourceLoc loc, enum BuiltinKind b_kind, MirRegister output, MirRegister object, MirRegister key)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->GetElement_ = (struct MirGetElement){
        .mid = pawMir_next_id(mir),
        .kind = kMirGetElement,
        .loc = loc,
        .b_kind = b_kind,
        .output = output,
        .object = object,
        .key = key,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_set_element(struct Mir *mir, struct SourceLoc loc, enum BuiltinKind b_kind, MirRegister object, MirRegister key, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetElement_ = (struct MirSetElement){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetElement,
        .loc = loc,
        .b_kind = b_kind,
        .object = object,
        .key = key,
        .value = value,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_get_range(struct Mir *mir, struct SourceLoc loc, enum BuiltinKind b_kind, MirRegister output, MirRegister object, MirRegister lower, MirRegister upper)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->GetRange_ = (struct MirGetRange){
        .mid = pawMir_next_id(mir),
        .kind = kMirGetRange,
        .loc = loc,
        .b_kind = b_kind,
        .output = output,
        .object = object,
        .lower = lower,
        .upper = upper,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_set_range(struct Mir *mir, struct SourceLoc loc, enum BuiltinKind b_kind, MirRegister object, MirRegister lower, MirRegister upper, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetRange_ = (struct MirSetRange){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetRange,
        .loc = loc,
        .b_kind = b_kind,
        .object = object,
        .lower = lower,
        .upper = upper,
        .value = value,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_get_field(struct Mir *mir, struct SourceLoc loc, int index, MirRegister output, MirRegister object)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->GetField_ = (struct MirGetField){
        .mid = pawMir_next_id(mir),
        .kind = kMirGetField,
        .loc = loc,
        .index = index,
        .output = output,
        .object = object,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_set_field(struct Mir *mir, struct SourceLoc loc, int index, MirRegister object, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetField_ = (struct MirSetField){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetField,
        .loc = loc,
        .index = index,
        .object = object,
        .value = value,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_unary_op(struct Mir *mir, struct SourceLoc loc, enum UnaryOp op, MirRegister val, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->UnaryOp_ = (struct MirUnaryOp){
        .mid = pawMir_next_id(mir),
        .kind = kMirUnaryOp,
        .loc = loc,
        .op = op,
        .val = val,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_binary_op(struct Mir *mir, struct SourceLoc loc, enum BinaryOp op, MirRegister lhs, MirRegister rhs, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->BinaryOp_ = (struct MirBinaryOp){
        .mid = pawMir_next_id(mir),
        .kind = kMirBinaryOp,
        .loc = loc,
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_cast(struct Mir *mir, struct SourceLoc loc, MirRegister target, MirRegister output, enum BuiltinKind from, enum BuiltinKind to)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Cast_ = (struct MirCast){
        .mid = pawMir_next_id(mir),
        .kind = kMirCast,
        .loc = loc,
        .target = target,
        .output = output,
        .from = from,
        .to = to,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_call(struct Mir *mir, struct SourceLoc loc, MirRegister target, struct MirRegisterList *args, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Call_ = (struct MirCall){
        .mid = pawMir_next_id(mir),
        .kind = kMirCall,
        .loc = loc,
        .target = target,
        .args = args,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_goto(struct Mir *mir, struct SourceLoc loc, MirBlock b)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Goto_ = (struct MirGoto){
        .mid = pawMir_next_id(mir),
        .kind = kMirGoto,
        .loc = loc,
        .target = b,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_branch(struct Mir *mir, struct SourceLoc loc, MirRegister cond, MirBlock then_arm, MirBlock else_arm)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Branch_ = (struct MirBranch){
        .mid = pawMir_next_id(mir),
        .kind = kMirBranch,
        .loc = loc,
        .cond = cond,
        .then_arm = then_arm,
        .else_arm = else_arm,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_switch(struct Mir *mir, struct SourceLoc loc, MirRegister discr, struct MirSwitchArmList *arms, MirBlock otherwise)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Switch_ = (struct MirSwitch){
        .mid = pawMir_next_id(mir),
        .kind = kMirSwitch,
        .loc = loc,
        .discr = discr,
        .arms = arms,
        .otherwise = otherwise,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_return(struct Mir *mir, struct SourceLoc loc, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Return_ = (struct MirReturn){
        .mid = pawMir_next_id(mir),
        .kind = kMirReturn,
        .loc = loc,
        .value = value,
    };
    return instr;
}

struct MirRegisterData {
    paw_Bool is_uninit : 1;
    paw_Bool is_captured : 1;
    MirRegister hint;
    struct IrType *type;
    struct IrType *self;
};

struct MirBlockData {
    struct MirBlockList *predecessors;
    struct MirBlockList *successors;
    struct MirInstructionList *joins;
    struct MirInstructionList *instructions;
    MirId mid;
};

// TODO: nested closures should be hoisted out into separate Mir objects, but this is complicated
//       for a few reasons, namely upvalues, generics, and naming.
struct Mir {
    struct Pool *pool;
    struct MirRegisterDataList *registers;
    struct MirRegisterList *locals;
    struct MirBlockDataList *blocks;
    struct MirUpvalueList *upvalues;
    struct MirCaptureList *captured;
    struct MirBodyList *children;
    struct SourceSpan span;
    struct IrType *type;
    struct IrType *self;
    struct Compiler *C;
    paw_Env *P;
    int mir_count;
    String *modname, *name;
    enum FuncKind fn_kind : 8;
    paw_Bool is_poly : 1;
    paw_Bool is_pub : 1;
};

#define MIR_KINDOF(node) ((node)->hdr.kind)
#define MIR_CAST_INSTRUCTION(p) CAST(struct MirInstruction *, p)

DEFINE_LIST(struct Mir, MirCaptureList, struct MirCaptureInfo)
DEFINE_LIST(struct Mir, MirUpvalueList, struct MirUpvalueInfo)
DEFINE_LIST(struct Mir, MirSwitchArmList, struct MirSwitchArm)
DEFINE_LIST(struct Mir, MirInstructionList, struct MirInstruction *)
DEFINE_LIST(struct Mir, MirRegisterList, MirRegister)
DEFINE_LIST(struct Mir, MirBlockList, MirBlock)
DEFINE_LIST(struct Mir, MirBucketList, struct MirBlockList *)
DEFINE_LIST(struct Mir, MirRegisterDataList, struct MirRegisterData)
DEFINE_LIST(struct Mir, MirRegisterPtrList, MirRegister *)
DEFINE_LIST(struct Mir, MirBlockDataList, struct MirBlockData *)
DEFINE_LIST(struct Mir, MirBodyList, struct Mir *)

struct Mir *pawMir_new(struct Compiler *C, String *modname, struct SourceSpan span, String *name, struct IrType *type, struct IrType *self, enum FuncKind fn_kind, paw_Bool is_pub, paw_Bool is_poly);
void pawMir_free(struct Mir *mir);

struct MirLiveInterval *pawMir_new_interval(struct Compiler *C, MirRegister r, int npositions);
struct MirRegisterData *pawMir_new_register(struct Compiler *C, int value, struct IrType *type);
struct MirBlockData *pawMir_new_block(struct Mir *mir);

// Get a pointer to each variable read or written by a given instruction
struct MirRegisterPtrList *pawMir_get_loads(struct Mir *mir, struct MirInstruction *instr);
MirRegister *pawMir_get_store(struct Mir *mir, struct MirInstruction *instr);

inline static MirId pawMir_next_id(struct Mir *mir)
{
    return (MirId){mir->mir_count++};
}

inline static MirId mir_bb_first(struct MirBlockData const *block)
{
    return block->mid;
}

inline static MirId mir_bb_last(struct MirBlockData const *block)
{
    if (block->instructions->count <= 0)
        return block->mid;
    return MirInstructionList_last(block->instructions)->hdr.mid;
}

inline static struct MirBlockData *mir_bb_data(struct Mir *mir, MirBlock bb)
{
    return MirBlockDataList_get(mir->blocks, bb.value);
}

inline static struct MirRegisterData *mir_reg_data(struct Mir *mir, MirRegister r)
{
    return &K_LIST_AT(mir->registers, r.value);
}

// Determine the index of "x" in the predecessor list of "y"
static int mir_which_pred(struct Mir *mir, MirBlock y, MirBlock x)
{
    int index;
    MirBlock const *pb;
    struct MirBlockData const *data = mir_bb_data(mir, y);
    K_LIST_ENUMERATE (data->predecessors, index, pb) {
        if (MIR_BB_EQUALS(x, *pb))
            return index;
    }

    PAW_UNREACHABLE();
}

static int mir_which_succ(struct Mir *mir, MirBlock x, MirBlock y)
{
    int index;
    MirBlock const *pb;
    struct MirBlockData const *data = mir_bb_data(mir, x);
    K_LIST_ENUMERATE (data->successors, index, pb) {
        if (MIR_BB_EQUALS(y, *pb))
            return index;
    }

    PAW_UNREACHABLE();
}

MirRegister pawMir_output_reg(struct MirInstruction *instr);

struct MirVisitor {
    struct Compiler *C;
    struct Mir *mir;
    void *ud;

    paw_Bool (*VisitInstruction)(struct MirVisitor *V, struct MirInstruction *node);
    paw_Bool (*VisitBlock)(struct MirVisitor *V, MirBlock node);
    paw_Bool (*VisitRegister)(struct MirVisitor *V, MirRegister node);

    void (*PostVisitInstruction)(struct MirVisitor *V, struct MirInstruction *node);
    void (*PostVisitBlock)(struct MirVisitor *V, MirBlock node);
    void (*PostVisitRegister)(struct MirVisitor *V, MirRegister node);

#define DEFINE_CALLBACK(X)                                             \
    paw_Bool (*Visit##X)(struct MirVisitor * V, struct Mir##X * node); \
    void (*PostVisit##X)(struct MirVisitor * V, struct Mir##X * node);
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
void pawMir_remove_unreachable_blocks(struct Mir *mir);

struct MirAccess {
    struct MirInstruction *instr;
    MirBlock b;
};

DEFINE_LIST(struct Compiler, MirAccessList, struct MirAccess)

struct AccessMap;
struct UseDefMap;

void pawMir_collect_per_instr_uses(struct Mir *mir, struct AccessMap *uses);
void pawMir_collect_per_instr_defs(struct Mir *mir, struct AccessMap *defs);

void pawMir_collect_per_block_usedefs(struct Mir *mir, struct UseDefMap *uses, struct UseDefMap *defs);

paw_Bool pawMir_propagate_constants(struct Mir *mir);
void pawMir_merge_redundant_blocks(struct Mir *mir);

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

DEFINE_LIST(struct Mir, MirIntervalList, struct MirLiveInterval *)
DEFINE_LIST(struct Mir, MirLocationList, int)

struct MirLocationList *pawMir_compute_locations(struct Mir *mir);
void pawMir_set_location(struct Mir *mir, struct MirLocationList *locations, MirId mid, int location);
inline static int pawMir_get_location(struct MirLocationList *locations, MirId mid)
{
    return MirLocationList_get(locations, mid.value);
}

struct MirBlockList *pawMir_compute_live_in(struct Mir *mir, struct MirBlockList *uses, struct MirBlockList *defs, MirRegister r);
struct MirIntervalList *pawMir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirLocationList *locations);

#define MIR_ID_HASH(Ctx_, Bb_) ((void)(Ctx_), (unsigned)(Bb_).value)
#define MIR_ID_EQUALS(Ctx_, A_, B_) ((void)(Ctx_), (A_).value == (B_).value)

DEFINE_MAP(struct Mir, AccessMap, pawP_alloc, MIR_ID_HASH, MIR_ID_EQUALS, MirRegister, struct MirAccessList *)
DEFINE_MAP(struct Mir, UseDefMap, pawP_alloc, MIR_ID_HASH, MIR_ID_EQUALS, MirRegister, struct MirBlockList *)
DEFINE_MAP_ITERATOR(UseDefMap, MirRegister, struct MirBlockList *)
DEFINE_MAP_ITERATOR(BodyMap, DeclId, struct Mir *)

char const *pawP_print_live_intervals_pretty(struct Compiler *C, struct Mir *mir, struct MirIntervalList *intervals);

// Push a human-readable representation of the MIR on to the stack
// Returns a pointer to the buffer containing null-terminated text.
char const *pawMir_dump(struct Mir *mir);
char const *pawMir_dump_info(struct Compiler *C, struct Mir *mir);
char const *pawMir_dump_graph(struct Compiler *C, struct Mir *mir);

#endif // PAW_MIR_H
