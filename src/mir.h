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

static inline MirId pawMir_next_id(struct Mir *mir);

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

#define MIR_INSTRUCTION_HEADER K_ALIGNAS_NODE int line; \
                               MirId mid; \
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

struct MirInstruction *pawMir_new_instruction(struct Mir *mir);

static inline struct MirInstruction *pawMir_new_move(struct Mir *mir, int line, MirRegister output, MirRegister target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Move_ = (struct MirMove){
        .mid = pawMir_next_id(mir),
        .kind = kMirMove,
        .line = line,
        .output = output,
        .target = target,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_upvalue(struct Mir *mir, int line, MirRegister output, int index)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Upvalue_ = (struct MirUpvalue){
        .mid = pawMir_next_id(mir),
        .kind = kMirUpvalue,
        .line = line,
        .index = index,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_global(struct Mir *mir, int line, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Global_ = (struct MirGlobal){
        .mid = pawMir_next_id(mir),
        .kind = kMirGlobal,
        .line = line,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_phi(struct Mir *mir, int line, struct MirRegisterList *inputs, MirRegister output, int var_id)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Phi_ = (struct MirPhi){
        .mid = pawMir_next_id(mir),
        .kind = kMirPhi,
        .line = line,
        .inputs = inputs,
        .output = output,
        .var_id = var_id,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_alloc_local(struct Mir *mir, int line, String *name, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->AllocLocal_ = (struct MirAllocLocal){
        .mid = pawMir_next_id(mir),
        .kind = kMirAllocLocal,
        .line = line,
        .output = output,
        .name = name,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_free_local(struct Mir *mir, int line, MirRegister reg)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->FreeLocal_ = (struct MirFreeLocal){
        .mid = pawMir_next_id(mir),
        .kind = kMirFreeLocal,
        .line = line,
        .reg = reg,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_set_local(struct Mir *mir, int line, MirRegister target, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetLocal_ = (struct MirSetLocal){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetLocal,
        .line = line,
        .target = target,
        .value = value,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_set_upvalue(struct Mir *mir, int line, int index, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetUpvalue_ = (struct MirSetUpvalue){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetUpvalue,
        .line = line,
        .index = index,
        .value = value,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_constant(struct Mir *mir, int line, enum BuiltinKind b_kind, Value value, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Constant_ = (struct MirConstant){
        .mid = pawMir_next_id(mir),
        .kind = kMirConstant,
        .line = line,
        .b_kind = b_kind,
        .value = value,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_container(struct Mir *mir, int line, enum BuiltinKind b_kind, int nelems, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Container_ = (struct MirContainer){
        .mid = pawMir_next_id(mir),
        .kind = kMirContainer,
        .line = line,
        .b_kind = b_kind,
        .nelems = nelems,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_aggregate(struct Mir *mir, int line, int nfields, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Aggregate_ = (struct MirAggregate){
        .mid = pawMir_next_id(mir),
        .kind = kMirAggregate,
        .line = line,
        .nfields = nfields,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_close(struct Mir *mir, int line, MirRegister target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Close_ = (struct MirClose){
        .mid = pawMir_next_id(mir),
        .kind = kMirClose,
        .line = line,
        .target = target,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_closure(struct Mir *mir, int line, int child_id, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Closure_ = (struct MirClosure){
        .mid = pawMir_next_id(mir),
        .kind = kMirClosure,
        .line = line,
        .child_id = child_id,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_get_element(struct Mir *mir, int line, enum BuiltinKind b_kind, MirRegister output, MirRegister object, MirRegister key)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->GetElement_ = (struct MirGetElement){
        .mid = pawMir_next_id(mir),
        .kind = kMirGetElement,
        .line = line,
        .b_kind = b_kind,
        .output = output,
        .object = object,
        .key = key,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_set_element(struct Mir *mir, int line, enum BuiltinKind b_kind, MirRegister object, MirRegister key, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetElement_ = (struct MirSetElement){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetElement,
        .line = line,
        .b_kind = b_kind,
        .object = object,
        .key = key,
        .value = value,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_get_range(struct Mir *mir, int line, enum BuiltinKind b_kind, MirRegister output, MirRegister object, MirRegister lower, MirRegister upper)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->GetRange_ = (struct MirGetRange){
        .mid = pawMir_next_id(mir),
        .kind = kMirGetRange,
        .line = line,
        .b_kind = b_kind,
        .output = output,
        .object = object,
        .lower = lower,
        .upper = upper,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_set_range(struct Mir *mir, int line, enum BuiltinKind b_kind, MirRegister object, MirRegister lower, MirRegister upper, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetRange_ = (struct MirSetRange){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetRange,
        .line = line,
        .b_kind = b_kind,
        .object = object,
        .lower = lower,
        .upper = upper,
        .value = value,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_get_field(struct Mir *mir, int line, int index, MirRegister output, MirRegister object)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->GetField_ = (struct MirGetField){
        .mid = pawMir_next_id(mir),
        .kind = kMirGetField,
        .line = line,
        .index = index,
        .output = output,
        .object = object,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_set_field(struct Mir *mir, int line, int index, MirRegister object, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetField_ = (struct MirSetField){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetField,
        .line = line,
        .index = index,
        .object = object,
        .value = value,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_unary_op(struct Mir *mir, int line, enum UnaryOp op, MirRegister val, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->UnaryOp_ = (struct MirUnaryOp){
        .mid = pawMir_next_id(mir),
        .kind = kMirUnaryOp,
        .line = line,
        .op = op,
        .val = val,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_binary_op(struct Mir *mir, int line, enum BinaryOp op, MirRegister lhs, MirRegister rhs, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->BinaryOp_ = (struct MirBinaryOp){
        .mid = pawMir_next_id(mir),
        .kind = kMirBinaryOp,
        .line = line,
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_cast(struct Mir *mir, int line, MirRegister target, MirRegister output, enum BuiltinKind from, enum BuiltinKind to)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Cast_ = (struct MirCast){
        .mid = pawMir_next_id(mir),
        .kind = kMirCast,
        .line = line,
        .target = target,
        .output = output,
        .from = from,
        .to = to,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_call(struct Mir *mir, int line, MirRegister target, struct MirRegisterList *args, MirRegister output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Call_ = (struct MirCall){
        .mid = pawMir_next_id(mir),
        .kind = kMirCall,
        .line = line,
        .target = target,
        .args = args,
        .output = output,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_goto(struct Mir *mir, int line, MirBlock b)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Goto_ = (struct MirGoto){
        .mid = pawMir_next_id(mir),
        .kind = kMirGoto,
        .line = line,
        .target = b,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_branch(struct Mir *mir, int line, MirRegister cond, MirBlock then_arm, MirBlock else_arm)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Branch_ = (struct MirBranch){
        .mid = pawMir_next_id(mir),
        .kind = kMirBranch,
        .line = line,
        .cond = cond,
        .then_arm = then_arm,
        .else_arm = else_arm,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_switch(struct Mir *mir, int line, MirRegister discr, struct MirSwitchArmList *arms, MirBlock otherwise)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Switch_ = (struct MirSwitch){
        .mid = pawMir_next_id(mir),
        .kind = kMirSwitch,
        .line = line,
        .discr = discr,
        .arms = arms,
        .otherwise = otherwise,
    };
    return instr;
}

static inline struct MirInstruction *pawMir_new_return(struct Mir *mir, int line, MirRegister value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Return_ = (struct MirReturn){
        .mid = pawMir_next_id(mir),
        .kind = kMirReturn,
        .line = line,
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
    struct MirRegisterDataList *registers;
    struct MirRegisterList *locals;
    struct MirBlockDataList *blocks;
    struct MirUpvalueList *upvalues;
    struct MirCaptureList *captured;
    struct MirBodyList *children;
    struct IrType *type;
    struct IrType *self;
    struct Compiler *C;
    String *name;
    int mir_count;
    enum FuncKind fn_kind : 8;
    paw_Bool is_native : 1;
    paw_Bool is_poly : 1;
    paw_Bool is_pub : 1;
};

#define MIR_KINDOF(node) ((node)->hdr.kind)
#define MIR_CAST_INSTRUCTION(p) CAST(struct MirInstruction *, p)

DEFINE_LIST(struct Compiler, pawMir_capture_list_, MirCaptureList, struct MirCaptureInfo)
DEFINE_LIST(struct Compiler, pawMir_upvalue_list_, MirUpvalueList, struct MirUpvalueInfo)
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
struct MirBlockData *pawMir_new_block(struct Mir *mir);

// Get a pointer to each variable read or written by a given instruction
struct MirRegisterPtrList *pawMir_get_loads(struct Compiler *C, struct MirInstruction *instr);
MirRegister *pawMir_get_store(struct Compiler *C, struct MirInstruction *instr);

static inline MirId pawMir_next_id(struct Mir *mir)
{
    return (MirId){mir->mir_count++};
}

static inline MirId mir_bb_first(const struct MirBlockData *block)
{
    return block->mid;
}

static inline MirId mir_bb_last(const struct MirBlockData *block)
{
    if (block->instructions->count <= 0) return block->mid;
    return K_LIST_LAST(block->instructions)->hdr.mid;
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
    int index;
    const MirBlock *pb;
    const struct MirBlockData *data = mir_bb_data(mir, y);
    K_LIST_ENUMERATE(data->predecessors, index, pb) {
        if (MIR_BB_EQUALS(x, *pb)) return index;
    }

    PAW_UNREACHABLE();
}

static int mir_which_succ(struct Mir *mir, MirBlock x, MirBlock y)
{
    int index;
    const MirBlock *pb;
    const struct MirBlockData *data = mir_bb_data(mir, x);
    K_LIST_ENUMERATE(data->successors, index, pb) {
        if (MIR_BB_EQUALS(y, *pb)) return index;
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
void pawMir_remove_unreachable_blocks(struct Mir *mir);

struct MirAccess {
    struct MirInstruction *instr;
    MirBlock b;
};

DEFINE_LIST(struct Compiler, pawMir_access_list_, MirAccessList, struct MirAccess)

void pawMir_collect_per_instr_uses(struct Mir *mir, Map *uses);
void pawMir_collect_per_instr_defs(struct Mir *mir, Map *defs);

void pawMir_collect_per_block_usedefs(struct Mir *mir, Map *uses, Map *defs);

void pawMir_propagate_constants(struct Mir *mir);

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
DEFINE_LIST(struct Compiler, pawMir_location_list_, MirLocationList, int)

struct MirLocationList *pawMir_compute_locations(struct Mir *mir);
void pawMir_set_location(struct Mir *mir, struct MirLocationList *locations, MirId mid, int location);
static inline int pawMir_get_location(struct MirLocationList *locations, MirId mid)
{
    return K_LIST_GET(locations, mid.value);
}

struct MirBlockList *pawMir_compute_live_in(struct Mir *mir, struct MirBlockList *uses, struct MirBlockList *defs, MirRegister r);
struct MirIntervalList *pawMir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirLocationList *locations);

const char *pawP_print_live_intervals_pretty(struct Compiler *C, struct Mir *mir, struct MirIntervalList *intervals);

// Push a human-readable representation of the MIR on to the stack
// Returns a pointer to the buffer containing null-terminated text.
const char *pawMir_dump(struct Compiler *C, struct Mir *mir);
const char *pawMir_dump_info(struct Compiler *C, struct Mir *mir);
const char *pawMir_dump_graph(struct Compiler *C, struct Mir *mir);

#endif // PAW_MIR_H

