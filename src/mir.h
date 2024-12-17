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
    X(BinaryOp)

#define MIR_TERMINATOR_LIST(X) \
    X(Return) \
    X(ForLoop) \
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
    struct IrType *type;
};

struct MirPhi {
    MIR_INSTRUCTION_HEADER;
    int var_id;
    struct MirRegisterList *inputs;
    MirRegister output;
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
    MirRegister output;
};

struct MirSetUpvalue {
    MIR_INSTRUCTION_HEADER;
    int index;
    MirRegister value;
    MirRegister output;
};

struct MirConstant {
    MIR_INSTRUCTION_HEADER;
    paw_Type code;
    Value value;
    MirRegister output;
};

struct MirContainer {
    MIR_INSTRUCTION_HEADER;
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
    MirRegister output;
    MirRegister object;
    MirRegister key;
};

struct MirSetElement {
    MIR_INSTRUCTION_HEADER;
    paw_Bool is_init : 1;
    MirRegister object;
    MirRegister key;
    MirRegister value;
    MirRegister output;
};

struct MirGetRange {
    MIR_INSTRUCTION_HEADER;
    MirRegister output;
    MirRegister object;
    MirRegister lower;
    MirRegister upper;
};

struct MirSetRange {
    MIR_INSTRUCTION_HEADER;
    MirRegister object;
    MirRegister lower;
    MirRegister upper;
    MirRegister value;
    MirRegister output;
};

struct MirGetField {
    MIR_INSTRUCTION_HEADER;
    int index;
    MirRegister output;
    MirRegister object;
};

struct MirSetField {
    MIR_INSTRUCTION_HEADER;
    paw_Bool is_init : 1;
    int index;
    MirRegister object;
    MirRegister value;
    MirRegister output;
};

struct MirUnaryOp {
    MIR_INSTRUCTION_HEADER;
    enum UnaryOp op : 8;
    MirRegister val;
    MirRegister output;
    struct IrType *type;
};

struct MirBinaryOp {
    MIR_INSTRUCTION_HEADER;
    enum BinaryOp op : 8;
    MirRegister lhs;
    MirRegister rhs;
    MirRegister output;
    struct IrType *type;
};

struct MirCast {
    MIR_INSTRUCTION_HEADER;
    MirRegister target;
    paw_Type type;
    MirRegister output;
};

struct MirCall {
    MIR_INSTRUCTION_HEADER;
    MirRegister target;
    struct MirRegisterList *args;
    MirRegister output;
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

enum MirTerminatorKind {
#define DEFINE_ENUM(X) kMir##X,
    MIR_TERMINATOR_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define MIR_TERMINATOR_HEADER K_ALIGNAS_NODE int line; \
                              enum MirTerminatorKind kind: 8

struct MirTerminatorHeader {
    MIR_TERMINATOR_HEADER;
};

enum MirForKind {
    MIR_FOR_PREP,
    MIR_FOR_LOOP,
};

struct MirForLoop {
    MIR_TERMINATOR_HEADER;
    enum MirForKind for_kind : 8;
    MirRegister var;
    MirRegister iter;
    MirRegister end;
    MirRegister step;
    MirBlock then_arm;
    MirBlock else_arm;
};

struct MirBranch {
    MIR_TERMINATOR_HEADER;
    MirRegister cond;
    MirBlock then_arm;
    MirBlock else_arm;
};

struct MirSwitchArm {
    MirBlock bid;
    Value value;
};

struct MirSwitch {
    MIR_TERMINATOR_HEADER;
    paw_Bool has_otherwise : 1;
    MirRegister discr;
    struct MirSwitchArmList *arms;
    MirBlock otherwise;
};

struct MirGoto {
    MIR_TERMINATOR_HEADER;
    MirBlock target;
};

struct MirReturn {
    MIR_TERMINATOR_HEADER;
    MirRegister value;
};

struct MirTerminator {
    union {
        struct MirTerminatorHeader hdr;
#define DEFINE_VARIANTS(X) struct Mir##X X##_;
        MIR_TERMINATOR_LIST(DEFINE_VARIANTS)
#undef DEFINE_VARIANTS
    };
};

#define DEFINE_ACCESS(X) \
    static inline paw_Bool MirIs##X(const struct MirTerminator *node) { \
        return node->hdr.kind == kMir##X; \
    } \
    static inline struct Mir##X *MirGet##X(struct MirTerminator *node) { \
        paw_assert(MirIs##X(node)); \
        return &node->X##_; \
    }
    MIR_TERMINATOR_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

static const char *kMirTerminatorNames[] = {
#define DEFINE_NAME(X) "Mir" #X,
    MIR_TERMINATOR_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

struct MirRegisterData {
    struct IrType *type;
};

struct MirBlockData {
    struct MirBlockList *predecessors;
    struct MirInstructionList *joins;
    struct MirInstructionList *instructions;
    struct MirTerminator *terminator;
};

// TODO: nested closures should be hoisted out into separate Mir objects, but this is complicated
//       for a few reasons, namely upvalues, generics, and naming.
struct Mir {
    struct MirRegisterDataList *registers;
    struct MirBlockDataList *blocks;
    struct MirScopeList *scopes;
    struct MirUpvalueList *upvalues;
    struct MirBodyList *children;
    struct IrType *type;
    struct IrType *self;
    String *name;
    enum FuncKind fn_kind : 8;
    paw_Bool is_native : 1;
    paw_Bool is_poly : 1;
    paw_Bool is_pub : 1;
};

#define MIR_KINDOF(node) ((node)->hdr.kind)
#define MIR_CAST_TERMINATOR(p) CAST(struct MirTerminator *, p)
#define MIR_CAST_INSTRUCTION(p) CAST(struct MirInstruction *, p)

struct MirScope *pawMir_get_scope(struct Mir *mir, MirScopeId id);

DEFINE_LIST(struct Compiler, pawMir_upvalue_list_, MirUpvalueList, struct MirUpvalueInfo)
DEFINE_LIST(struct Compiler, pawMir_scope_list_, MirScopeList, struct MirScope)
DEFINE_LIST(struct Compiler, pawMir_switch_list_, MirSwitchArmList, struct MirSwitchArm)
DEFINE_LIST(struct Compiler, pawMir_instruction_list_, MirInstructionList, struct MirInstruction *)
DEFINE_LIST(struct Compiler, pawMir_register_list_, MirRegisterList, MirRegister)
DEFINE_LIST(struct Compiler, pawMir_block_list_, MirBlockList, MirBlock)
DEFINE_LIST(struct Compiler, pawMir_bucket_list_, MirBucketList, struct MirBlockList *)
DEFINE_LIST(struct Compiler, pawMir_register_data_list_, MirRegisterDataList, struct MirRegisterData)
DEFINE_LIST(struct Compiler, pawMir_block_data_list_, MirBlockDataList, struct MirBlockData *)
DEFINE_LIST(struct Compiler, pawMir_body_list_, MirBodyList, struct Mir *)

struct Mir *pawMir_new(struct Compiler *C, String *name, struct IrType *type, struct IrType *self, enum FuncKind fn_kind, paw_Bool is_native, paw_Bool is_pub, paw_Bool is_poly);
struct MirRegisterData *pawMir_new_register(struct Compiler *C, int value, struct IrType *type);
struct MirInstruction *pawMir_new_instruction(struct Compiler *C, enum MirInstructionKind kind);
struct MirTerminator *pawMir_new_terminator(struct Compiler *C, enum MirTerminatorKind kind);
struct MirTerminator *pawMir_new_return(struct Compiler *C, MirRegister value);
struct MirTerminator *pawMir_new_goto(struct Compiler *C, MirBlock bid);
struct MirBlockData *pawMir_new_block(struct Compiler *C);

static inline struct MirBlockData *mir_bb_data(struct Mir *mir, MirBlock bb)
{
    return K_LIST_GET(mir->blocks, bb.value);
}

static inline struct MirRegisterData *mir_reg_data(struct Mir *mir, MirRegister r)
{
    return &K_LIST_GET(mir->registers, r.value);
}

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
    MIR_TERMINATOR_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawMir_visitor_init(struct MirVisitor *V, struct Compiler *C, struct Mir *mir, void *ud);
void pawMir_visit(struct MirVisitor *V);

// Visitor entrypoints for each kind of HIR node:
void pawMir_visit_instruction(struct MirVisitor *V, struct MirInstruction *node);
void pawMir_visit_terminator(struct MirVisitor *V, struct MirTerminator *node);
void pawMir_visit_block(struct MirVisitor *V, MirBlock node);
void pawMir_visit_register(struct MirVisitor *V, MirRegister node);
void pawMir_visit_instruction_list(struct MirVisitor *V, struct MirInstructionList *list);
void pawMir_visit_block_list(struct MirVisitor *V, struct MirBlockList *list);
void pawMir_visit_register_list(struct MirVisitor *V, struct MirRegisterList *list);

// Compute the immediate dominator for each basic block
struct MirBlockList *pawMir_compute_dominance_tree(struct Compiler *C, struct Mir *mir);

// Compute the dominance frontier set for each basic block
struct MirBucketList *pawMir_compute_dominance_frontiers(struct Compiler *C, struct Mir *mir, struct MirBlockList *idom);

void pawMir_add_successors(struct Compiler *C, struct MirTerminator *terminator, struct MirBlockList *result);
struct MirBlockList *pawMir_collect_preorder(struct Compiler *C, struct Mir *mir);
struct MirBlockList *pawMir_collect_postorder(struct Compiler *C, struct Mir *mir);

void pawMir_remove_unreachable_blocks(struct Compiler *C, struct Mir *mir);

static inline struct MirBlockList *pawMir_get_successors(struct Compiler *C, struct MirTerminator *terminator)
{
    struct MirBlockList *result = pawMir_block_list_new(C);
    pawMir_add_successors(C, terminator, result);
    return result;
}

// Push a human-readable representation of the MIR on to the stack
// Returns a pointer to the buffer containing null-terminated text.
const char *pawMir_dump(struct Compiler *C, struct Mir *mir);
const char *pawMir_dump_graph(struct Compiler *C, struct Mir *mir);

#endif // PAW_MIR_H

