// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_MIR_H
#define PAW_MIR_H

#include "compile.h"
#include "ir_type.h"

struct Mir;

// TODO: Rename *GEP to *Gep (*GEP is not camel case)
#define MIR_INSTRUCTION_LIST(X) \
    X(Noop)                     \
    X(Phi)                      \
    X(Move)                     \
    X(Load)                     \
    X(Store)                    \
    X(AddrOf)                   \
    X(Global)                   \
    X(AllocLocal)               \
    X(Aggregate)                \
    X(Array)                    \
    X(ArrayGep)                 \
    X(StructGEP)                \
    X(SetRange)                 \
    X(GetRange)                 \
    X(Kill)                     \
    X(Drop)                     \
    X(Call)                     \
    X(Cast)                     \
    X(Capture)                  \
    X(Close)                    \
    X(Closure)                  \
    X(UnaryOp)                  \
    X(BinaryOp)                 \
    X(Unreachable)              \
    X(Return)                   \
    X(Branch)                   \
    X(Switch)                   \
    X(Goto)

#define MIR_PROJECTION_LIST(X) \
    X(Deref)                   \
    X(Field)                   \
    X(Index)                   \
    X(Range)

#define MIR_ID_EQUALS(X_, Y_) ((X_).value == (Y_).value)
#define MIR_ID_EXISTS(X_) ((X_).value >= 0)

#define MIR_INVALID_BB MIR_BB(-1)
#define MIR_ENTRY_BB MIR_BB(0)
#define MIR_BB(X_) ((MirBlock){X_})
typedef struct MirBlock {
    int value;
} MirBlock;

#define MIR_INVALID_REG MIR_REG(-1)
#define MIR_RESULT_REG MIR_REG(0)
#define MIR_REG(X_) ((MirRegister){X_})
typedef struct MirRegister {
    int value;
} MirRegister;

#define MIR_INVALID_CONST MIR_CONST(-1)
#define MIR_BASE_CONST MIR_CONST(0)
#define MIR_CONST(X_) ((MirConstant){X_})
typedef struct MirConstant {
    int value;
} MirConstant;

#define MIR_INVALID_SCOPE MIR_SCOPE(-1)
#define MIR_BASE_SCOPE MIR_SCOPE(0)
#define MIR_SCOPE(X_) ((MirScope){X_})
typedef struct MirScope {
    int value;
} MirScope;

MirScope pawMir_new_scope(struct Mir *mir, MirScope outer);
struct MirScopeInfo pawMir_get_scope_info(struct Mir *mir, MirScope scope);

typedef struct MirId {
    int value;
} MirId;

inline static MirId pawMir_next_id(struct Mir *mir);


struct MirScopeInfo {
    MirScope outer;
    int depth;
};

struct MirCaptureInfo {
    MirRegister local;
};

struct MirUpvalueInfo {
    struct IrType *type;
    paw_Bool is_local : 1;
    unsigned short index;
};

enum MirPlaceKind {
    MIR_PLACE_REGISTER,
    MIR_PLACE_UPVALUE,
    MIR_PLACE_CONSTANT,
};

struct MirPlace {
    enum MirPlaceKind kind;
    union {
        int value;
        MirRegister r;
        MirConstant k;
        int up;
    };
    struct SourceSpan span;
    struct IrType *type;
};


enum MirInstructionKind {
#define DEFINE_ENUM(X) kMir##X,
    MIR_INSTRUCTION_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define MIR_INSTRUCTION_HEADER \
    struct SourceSpan span; \
    MirScope scope; \
    MirId mid; \
    enum MirInstructionKind kind : 8

struct MirInstructionHeader {
    MIR_INSTRUCTION_HEADER;
};

struct MirNoop {
    MIR_INSTRUCTION_HEADER;
};

struct MirPhi {
    MIR_INSTRUCTION_HEADER;
    struct MirPlaceList *inputs;
    struct MirPlace output;
    int var_id;
};

struct MirMove {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace output;
    struct MirPlace target;
};

struct MirLoad {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace pointer;
    struct MirPlace output;
};

struct MirStore {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace value;
    struct MirPlace pointer;
};

struct MirAddrOf {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace input;
    struct MirPlace output;
};

struct MirGlobal {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace output;
};

struct MirAllocLocal {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace output;
    Str *name;
};

struct MirArray {
    MIR_INSTRUCTION_HEADER;
    struct MirPlaceList *elems;
    struct MirPlace output;
};

struct MirAggregate {
    MIR_INSTRUCTION_HEADER;
    paw_Bool is_boxed : 1;
    int discr;
    struct MirPlaceList *fields;
    struct MirPlace output;
};

struct MirCapture {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace target;
};

struct MirClose {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace target;
};

struct MirClosure {
    MIR_INSTRUCTION_HEADER;
    int child_id;
    struct MirPlace output;
};

struct MirStructGEP {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace output;
    struct MirPlace object;
    int field;
    int discr;
};

struct MirArrayGep {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace output;
    struct MirPlace array;
    struct MirPlace index;
    int field;
};

struct MirGetRange {
    MIR_INSTRUCTION_HEADER;
    enum BuiltinKind b_kind : 8;
    struct MirPlace output;
    struct MirPlace object;
    struct MirPlace lower;
    struct MirPlace upper;
};

struct MirSetRange {
    MIR_INSTRUCTION_HEADER;
    enum BuiltinKind b_kind : 8;
    struct MirPlace object;
    struct MirPlace lower;
    struct MirPlace upper;
    struct MirPlace value;
};

enum MirUnaryOpKind {
    MIR_UNARY_INEG,
    MIR_UNARY_IBITNOT,
    MIR_UNARY_FNEG,
    MIR_UNARY_NOT,
};

struct MirUnaryOp {
    MIR_INSTRUCTION_HEADER;
    enum MirUnaryOpKind op : 8;
    struct MirPlace val;
    struct MirPlace output;
};

enum MirBinaryOpKind {
    MIR_BINARY_CEQ,
    MIR_BINARY_CNE,
    MIR_BINARY_CLT,
    MIR_BINARY_CLE,
    MIR_BINARY_IEQ,
    MIR_BINARY_INE,
    MIR_BINARY_ILT,
    MIR_BINARY_ILE,
    MIR_BINARY_FEQ,
    MIR_BINARY_FNE,
    MIR_BINARY_FLT,
    MIR_BINARY_FLE,
    MIR_BINARY_STREQ,
    MIR_BINARY_STRNE,
    MIR_BINARY_STRLT,
    MIR_BINARY_STRLE,
    MIR_BINARY_IADD,
    MIR_BINARY_ISUB,
    MIR_BINARY_IMUL,
    MIR_BINARY_IDIV,
    MIR_BINARY_IMOD,
    MIR_BINARY_FADD,
    MIR_BINARY_FSUB,
    MIR_BINARY_FMUL,
    MIR_BINARY_FDIV,
    MIR_BINARY_FMOD,
    MIR_BINARY_IBITAND,
    MIR_BINARY_IBITOR,
    MIR_BINARY_IBITXOR,
    MIR_BINARY_ISHL,
    MIR_BINARY_ISHR,
};

struct MirBinaryOp {
    MIR_INSTRUCTION_HEADER;
    enum MirBinaryOpKind op : 8;
    struct MirPlace lhs;
    struct MirPlace rhs;
    struct MirPlace output;
};

struct MirCast {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace target;
    struct MirPlace output;
    enum BuiltinKind from;
    enum BuiltinKind to;
};

// Special instruction that ends the lifetime of the "target" place
// Used to prevent "drop" from being called on an object after it
// was consumed in a destructuring operation as part of a match
// expression.
struct MirKill {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace target;
};

struct MirDrop {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace target;
};

struct MirCall {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace target;
    struct MirPlaceList *args;
    struct MirPlace output;
};

struct MirBranch {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace cond;
};

struct MirSwitchArm {
    MirConstant k;
};

struct MirSwitch {
    MIR_INSTRUCTION_HEADER;
    paw_Bool has_otherwise : 1;
    struct MirPlace discr;
    struct MirSwitchArmList *arms;
};

struct MirGoto {
    MIR_INSTRUCTION_HEADER;
};

struct MirUnreachable {
    MIR_INSTRUCTION_HEADER;
};

struct MirReturn {
    MIR_INSTRUCTION_HEADER;
};

typedef struct MirInstruction {
    union {
        struct MirInstructionHeader hdr;
#define DEFINE_VARIANTS(X) struct Mir##X X##_;
        MIR_INSTRUCTION_LIST(DEFINE_VARIANTS)
#undef DEFINE_VARIANTS
    };
} MirInstruction;

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

inline static struct MirInstruction *pawMir_new_move(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Move_ = (struct MirMove){
        .mid = pawMir_next_id(mir),
        .kind = kMirMove,
        .span = span,
        .output = output,
        .target = target,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_load(struct Mir *mir, struct SourceSpan span, struct MirPlace pointer, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Load_ = (struct MirLoad){
        .mid = pawMir_next_id(mir),
        .kind = kMirLoad,
        .span = span,
        .pointer = pointer,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_store(struct Mir *mir, struct SourceSpan span, struct MirPlace value, struct MirPlace pointer)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Store_ = (struct MirStore){
        .mid = pawMir_next_id(mir),
        .kind = kMirStore,
        .span = span,
        .value = value,
        .pointer = pointer,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_addr_of(struct Mir *mir, struct SourceSpan span, struct MirPlace input, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->AddrOf_ = (struct MirAddrOf){
        .mid = pawMir_next_id(mir),
        .kind = kMirAddrOf,
        .span = span,
        .input = input,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_global(struct Mir *mir, struct SourceSpan span, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Global_ = (struct MirGlobal){
        .mid = pawMir_next_id(mir),
        .kind = kMirGlobal,
        .span = span,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_noop(struct Mir *mir, struct SourceSpan span)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Noop_ = (struct MirNoop){
        .mid = pawMir_next_id(mir),
        .kind = kMirNoop,
        .span = span,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_phi(struct Mir *mir, struct SourceSpan span, struct MirPlaceList *inputs, struct MirPlace output, int var_id)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Phi_ = (struct MirPhi){
        .mid = pawMir_next_id(mir),
        .kind = kMirPhi,
        .span = span,
        .inputs = inputs,
        .output = output,
        .var_id = var_id,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_alloc_local(struct Mir *mir, struct SourceSpan span, Str *name, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->AllocLocal_ = (struct MirAllocLocal){
        .mid = pawMir_next_id(mir),
        .kind = kMirAllocLocal,
        .span = span,
        .output = output,
        .name = name,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_array(struct Mir *mir, struct SourceSpan span, struct MirPlaceList *elems, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Array_ = (struct MirArray){
        .mid = pawMir_next_id(mir),
        .kind = kMirArray,
        .span = span,
        .elems = elems,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_aggregate(struct Mir *mir, struct SourceSpan span, struct MirPlaceList *fields, struct MirPlace output, int discr, paw_Bool is_boxed)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Aggregate_ = (struct MirAggregate){
        .mid = pawMir_next_id(mir),
        .kind = kMirAggregate,
        .span = span,
        .discr = discr,
        .fields = fields,
        .output = output,
        .is_boxed = is_boxed,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_capture(struct Mir *mir, struct SourceSpan span, struct MirPlace target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Capture_ = (struct MirCapture){
        .mid = pawMir_next_id(mir),
        .kind = kMirCapture,
        .span = span,
        .target = target,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_close(struct Mir *mir, struct SourceSpan span, struct MirPlace target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Close_ = (struct MirClose){
        .mid = pawMir_next_id(mir),
        .kind = kMirClose,
        .span = span,
        .target = target,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_closure(struct Mir *mir, struct SourceSpan span, int child_id, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Closure_ = (struct MirClosure){
        .mid = pawMir_next_id(mir),
        .kind = kMirClosure,
        .span = span,
        .child_id = child_id,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_struct_gep(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace object, int field, int discr)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->StructGEP_ = (struct MirStructGEP){
        .mid = pawMir_next_id(mir),
        .kind = kMirStructGEP,
        .span = span,
        .output = output,
        .object = object,
        .field = field,
        .discr = discr,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_array_gep(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace array, struct MirPlace index)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->ArrayGep_ = (struct MirArrayGep){
        .mid = pawMir_next_id(mir),
        .kind = kMirArrayGep,
        .span = span,
        .output = output,
        .array = array,
        .index = index,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_get_range(struct Mir *mir, struct SourceSpan span, enum BuiltinKind b_kind, struct MirPlace output, struct MirPlace object, struct MirPlace lower, struct MirPlace upper)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->GetRange_ = (struct MirGetRange){
        .mid = pawMir_next_id(mir),
        .kind = kMirGetRange,
        .span = span,
        .b_kind = b_kind,
        .output = output,
        .object = object,
        .lower = lower,
        .upper = upper,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_set_range(struct Mir *mir, struct SourceSpan span, enum BuiltinKind b_kind, struct MirPlace object, struct MirPlace lower, struct MirPlace upper, struct MirPlace value)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->SetRange_ = (struct MirSetRange){
        .mid = pawMir_next_id(mir),
        .kind = kMirSetRange,
        .span = span,
        .b_kind = b_kind,
        .object = object,
        .lower = lower,
        .upper = upper,
        .value = value,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_unary_op(struct Mir *mir, struct SourceSpan span, enum MirUnaryOpKind op, struct MirPlace val, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->UnaryOp_ = (struct MirUnaryOp){
        .mid = pawMir_next_id(mir),
        .kind = kMirUnaryOp,
        .span = span,
        .op = op,
        .val = val,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_binary_op(struct Mir *mir, struct SourceSpan span, enum MirBinaryOpKind op, struct MirPlace lhs, struct MirPlace rhs, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->BinaryOp_ = (struct MirBinaryOp){
        .mid = pawMir_next_id(mir),
        .kind = kMirBinaryOp,
        .span = span,
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_cast(struct Mir *mir, struct SourceSpan span, struct MirPlace target, struct MirPlace output, enum BuiltinKind from, enum BuiltinKind to)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Cast_ = (struct MirCast){
        .mid = pawMir_next_id(mir),
        .kind = kMirCast,
        .span = span,
        .target = target,
        .output = output,
        .from = from,
        .to = to,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_kill(struct Mir *mir, struct SourceSpan span, struct MirPlace target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Kill_ = (struct MirKill){
        .mid = pawMir_next_id(mir),
        .kind = kMirKill,
        .span = span,
        .target = target,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_drop(struct Mir *mir, struct SourceSpan span, struct MirPlace target)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Drop_ = (struct MirDrop){
        .mid = pawMir_next_id(mir),
        .kind = kMirDrop,
        .span = span,
        .target = target,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_call(struct Mir *mir, struct SourceSpan span, struct MirPlace target, struct MirPlaceList *args, struct MirPlace output)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Call_ = (struct MirCall){
        .mid = pawMir_next_id(mir),
        .kind = kMirCall,
        .span = span,
        .target = target,
        .args = args,
        .output = output,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_goto(struct Mir *mir, struct SourceSpan span)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Goto_ = (struct MirGoto){
        .mid = pawMir_next_id(mir),
        .kind = kMirGoto,
        .span = span,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_branch(struct Mir *mir, struct SourceSpan span, struct MirPlace cond)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Branch_ = (struct MirBranch){
        .mid = pawMir_next_id(mir),
        .kind = kMirBranch,
        .span = span,
        .cond = cond,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_switch(struct Mir *mir, struct SourceSpan span, struct MirPlace discr, struct MirSwitchArmList *arms, paw_Bool has_otherwise)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Switch_ = (struct MirSwitch){
        .mid = pawMir_next_id(mir),
        .kind = kMirSwitch,
        .span = span,
        .discr = discr,
        .arms = arms,
        .has_otherwise = has_otherwise,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_unreachable(struct Mir *mir, struct SourceSpan span)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Unreachable_ = (struct MirUnreachable){
        .mid = pawMir_next_id(mir),
        .kind = kMirUnreachable,
        .span = span,
    };
    return instr;
}

inline static struct MirInstruction *pawMir_new_return(struct Mir *mir, struct SourceSpan span)
{
    struct MirInstruction *instr = pawMir_new_instruction(mir);
    instr->Return_ = (struct MirReturn){
        .mid = pawMir_next_id(mir),
        .kind = kMirReturn,
        .span = span,
    };
    return instr;
}

struct MirConstantData {
    IrConst *data;
    IrType *type;
};

struct MirRegisterData {
    paw_Bool is_captured : 1;
    paw_Bool is_nontrivial : 1;
    struct IrType *type;
    Str const *name;
};

struct MirBlockData {
    struct MirBlockList *predecessors;
    struct MirBlockList *successors;
    struct MirInstructionList *joins;
    struct MirInstructionList *instructions;
    MirScope scope;
    MirId mid;
};

// Note that ValueMap for floats considers "-0.0" and "0.0" to be different values, while
// normal floating point equality comparison considers them to be equal. This shouldn't cause
// any problems, provided that the runtime generates floating point comparisons correctly.
struct MirConstantCache {
    struct MirConstantDataList *data;

    struct MirConstMap *chars;
    struct MirConstMap *ints;
    struct MirConstMap *floats;
    struct MirConstMap *strs;
    struct MirConstMap *params;

    MirConstant unitk;
    MirConstant boolk[2];
};

DEFINE_MAP(struct Compiler, MirConstMap, pawP_alloc, pawIr_const_hash, pawIr_const_equals, IrConst *, MirConstant)

struct MirConstantCache *pawMir_kcache_new(struct Mir *mir);
void pawMir_kcache_delete(struct Mir *mir, struct MirConstantCache *kcache);
MirConstant pawMir_kcache_add_value(struct Mir *mir, struct MirConstantCache *kcache, union IrValue value, IrType *type);
MirConstant pawMir_kcache_add_param(struct Mir *mir, struct MirConstantCache *kcache, DeclId did);

// TODO: nested closures should be hoisted out into separate Mir objects, but this is complicated
//       for a few reasons, namely upvalues, generics, and naming.
struct Mir {
    struct Pool *pool;
    struct Annotations *annotations;
    struct MirRegisterDataList *registers;
    struct MirBlockDataList *blocks;
    struct MirUpvalueList *upvalues;
    struct MirCaptureList *captured;
    struct MirBodyList *children;
    struct MirConstantCache *kcache;
    struct MirScopeInfoList *scopes;
    struct SourceSpan span;
    struct IrType *type;
    struct IrType *self;
    struct Compiler *C;
    paw_Env *P;
    DeclId parent_id; // TODO: rename to impl_id
    int child_id;
    int mir_count;
    int modno;
    Str *name;
    enum FnKind fn_kind : 8;
    paw_Bool is_method : 1;
    paw_Bool is_poly : 1;
    paw_Bool is_pub : 1;
};

EXTERN_C paw_Bool pawMir_is_main(struct Mir const *mir);
EXTERN_C struct MirPlace pawMir_get_register(struct Mir const *mir, MirRegister r);

#define MIR_KINDOF(node) ((node)->hdr.kind)
#define MIR_CAST_INSTRUCTION(p) CAST(struct MirInstruction *, p)

DEFINE_LIST(struct Mir, MirScopeInfoList, struct MirScopeInfo)
DEFINE_LIST(struct Mir, MirCaptureList, struct MirCaptureInfo)
DEFINE_LIST(struct Mir, MirUpvalueList, struct MirUpvalueInfo)
DEFINE_LIST(struct Mir, MirSwitchArmList, struct MirSwitchArm)
DEFINE_LIST(struct Mir, MirProjectionList, struct MirProjection *)
DEFINE_LIST(struct Mir, MirInstructionList, struct MirInstruction *)
DEFINE_LIST(struct Mir, MirPlaceList, struct MirPlace)
DEFINE_LIST(struct Mir, MirPlacePtrList, struct MirPlace *)
DEFINE_LIST(struct Mir, MirConstantList, MirConstant)
DEFINE_LIST(struct Mir, MirRegisterList, MirRegister)
DEFINE_LIST(struct Mir, MirBlockList, MirBlock)
DEFINE_LIST(struct Mir, MirBucketList, struct MirBlockList *)
DEFINE_LIST(struct Mir, MirConstantDataList, struct MirConstantData)
DEFINE_LIST(struct Mir, MirRegisterDataList, struct MirRegisterData)
DEFINE_LIST(struct Mir, MirRegisterPtrList, MirRegister *)
DEFINE_LIST(struct Mir, MirBlockDataList, struct MirBlockData *)
DEFINE_LIST(struct Mir, MirBodyList, struct Mir *)

struct Mir *pawMir_new(struct Compiler *C, int modno, struct SourceSpan span, Str *name, Annotations *annotations, struct IrType *type, struct IrType *self, int child_id, DeclId parent_id, enum FnKind fn_kind, paw_Bool is_pub, paw_Bool is_poly);
void pawMir_free(struct Mir *mir);

struct MirLiveInterval *pawMir_new_interval(struct Compiler *C, MirRegister r, int npositions);
struct MirBlockData *pawMir_new_block(struct Mir *mir, MirScope scope);

// Get a pointer to each variable read or written by a given instruction
struct MirPlacePtrList *pawMir_get_loads(struct Mir *mir, struct MirInstruction *instr);
struct MirPlacePtrList *pawMir_get_stores(struct Mir *mir, struct MirInstruction *instr);

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
    if (block->instructions->count == 0) return block->mid;
    return MirInstructionList_last(block->instructions)->hdr.mid;
}

inline static struct MirBlockData *mir_bb_data(struct Mir *mir, MirBlock bb)
{
    return MirBlockDataList_get(mir->blocks, bb.value);
}

inline static struct MirConstantData *mir_const_data(struct Mir *mir, MirConstant k)
{
    paw_assert(0 <= k.value && k.value < mir->kcache->data->count);
    return &K_LIST_AT(mir->kcache->data, k.value);
}

inline static struct MirRegisterData *mir_reg_data(struct Mir *mir, MirRegister r)
{
    paw_assert(0 <= r.value && r.value < mir->registers->count);
    return &K_LIST_AT(mir->registers, r.value);
}

// Determine the index of "x" in the predecessor list of "y"
static int mir_which_pred(struct Mir *mir, MirBlock y, MirBlock x)
{
    int index;
    MirBlock const *pb;
    struct MirBlockData const *data = mir_bb_data(mir, y);
    K_LIST_ENUMERATE (data->predecessors, index, pb) {
        if (MIR_ID_EQUALS(x, *pb))
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
        if (MIR_ID_EQUALS(y, *pb))
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
    paw_Bool (*VisitPlace)(struct MirVisitor *V, struct MirPlace node);

    void (*PostVisitInstruction)(struct MirVisitor *V, struct MirInstruction *node);
    void (*PostVisitBlock)(struct MirVisitor *V, MirBlock node);
    void (*PostVisitPlace)(struct MirVisitor *V, struct MirPlace node);

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
void pawMir_visit_place(struct MirVisitor *V, struct MirPlace node);
void pawMir_visit_instruction_list(struct MirVisitor *V, struct MirInstructionList *list);
void pawMir_visit_block_list(struct MirVisitor *V, struct MirBlockList *list);
void pawMir_visit_place_list(struct MirVisitor *V, struct MirPlaceList *list);

// Compute the immediate dominator of each basic block
struct MirBlockList *pawMir_compute_dominance_tree(struct Compiler *C, struct Mir *mir);

// Compute the dominance frontier set of each basic block
struct MirBucketList *pawMir_compute_dominance_frontiers(struct Compiler *C, struct Mir *mir, struct MirBlockList *idom);

// Return the reverse postorder (RPO) traversal of the CFG
// If there is a path from node X to node Y in the CFG, then X must have a lower RPO
// number than node Y.
struct MirBlockList *pawMir_traverse_rpo(struct Compiler *C, struct Mir *mir);

// Order basic blocks in reverse postorder
// Removes all basic blocks that cannot be reached from the entry.
void pawMir_renumber_basic_blocks(struct Mir *mir);

struct MirAccess {
    struct MirInstruction *instr;
    MirBlock b;
};

DEFINE_LIST(struct Compiler, MirAccessList, struct MirAccess)

struct AccessMap;
struct UseDefMap;

// NOTE: Creates duplicate "MirAccess" objects when a register is accessed multiple
//       times by the same instruction.
void pawMir_collect_per_instr_uses(struct Mir *mir, struct AccessMap *uses);
void pawMir_collect_per_instr_defs(struct Mir *mir, struct AccessMap *defs);

void pawMir_collect_per_block_usedefs(struct Mir *mir, struct UseDefMap *uses, struct UseDefMap *defs);

void pawMir_generate_drops(struct Mir *mir);

void pawMir_propagate_constants(struct Mir *mir);
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
struct MirIntervalMap *pawMir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirLocationList *locations);

static paw_Bool mir_is_lvalue(struct MirPlace place)
{
    return place.kind == MIR_PLACE_UPVALUE;
}

static paw_Bool mir_is_rvalue(struct MirPlace place)
{
    return !mir_is_lvalue(place);
}

static paw_Uint mir_place_hash(struct Mir *mir, struct MirPlace place)
{
    PAW_UNUSED(mir);
    return (place.kind + 1) * (paw_Uint)place.value;
}

static paw_Bool mir_place_equals(struct Mir *mir, struct MirPlace lhs, struct MirPlace rhs)
{
    PAW_UNUSED(mir);
    return lhs.kind == rhs.kind
        && lhs.value == rhs.value;
}

DEFINE_MAP(struct Mir, MirIntervalMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirRegister, struct MirLiveInterval *)
DEFINE_MAP(struct Mir, AccessMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirRegister, struct MirAccessList *)
DEFINE_MAP(struct Mir, UseDefMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirRegister, struct MirBlockList *)
DEFINE_MAP_ITERATOR(UseDefMap, MirRegister, struct MirBlockList *)
DEFINE_MAP_ITERATOR(BodyMap, DeclId, struct Mir *)

paw_Bool pawP_fold_unary_op(struct Compiler *C, enum MirUnaryOpKind op, union IrValue v, union IrValue *pr);
paw_Bool pawP_fold_binary_op(struct Compiler *C, Str const *modname, struct SourceSpan span, enum MirBinaryOpKind op, union IrValue x, union IrValue y, union IrValue *pr);

// Push a human-readable representation of the MIR on to the stack
// Returns a pointer to the buffer containing null-terminated text.
EXTERN_C char const *pawMir_dump(struct Mir *mir);
EXTERN_C char const *pawMir_dump_graph(struct Mir *mir);

#if NDEBUG
# define MIR_VALIDATE_GRAPH(Mir_)
#else
void pawMir_validate_graph(struct Mir *mir);
# define MIR_VALIDATE_GRAPH(Mir_) pawMir_validate_graph(Mir_)
#endif // !NDEBUG

#endif // PAW_MIR_H
