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
    X(Local) \
    X(Upvalue) \
    X(Global) \
    X(AllocLocal) \
    X(FreeLocal) \
    X(EnterScope) \
    X(LeaveScope) \
    X(SetUpvalue) \
    X(Discard) \
    X(Constant) \
    X(Aggregate) \
    X(Container) \
    X(Explode) \
    X(Assign) \
    X(Call) \
    X(Cast) \
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

#define MIR_NO_BLOCK (struct MirBlockId){-1}
#define MIR_BLOCK_EQ(a, b) ((a)->value == (b)->value)
typedef struct MirBlockId {
    int value;
} MirBlockId;

#define MIR_NO_SCOPE (struct MirScopeId){-1}
#define MIR_SCOPE_EQ(a, b) ((a)->value == (b)->value)
typedef struct MirScopeId {
    int value;
} MirScopeId;

struct MirRegister {
    struct IrType *type;
    int value;
};

struct MirUpvalueInfo {
    paw_Bool is_local : 1;
    unsigned short index;
};

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

struct MirLocal {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *output;
    struct MirRegister *target;
};

struct MirUpvalue {
    MIR_INSTRUCTION_HEADER;
    int index;
    struct MirRegister *output;
};

struct MirGlobal {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *output;
};

struct MirAllocLocal {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *output;
    String *name;
};

struct MirFreeLocal {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *reg;
};

struct MirEnterScope {
    MIR_INSTRUCTION_HEADER;
    MirScopeId scope_id;
};

struct MirLeaveScope {
    MIR_INSTRUCTION_HEADER;
    MirScopeId scope_id;
};

struct MirOpenScope {
    MIR_INSTRUCTION_HEADER;
    int id;
};

struct MirCloseScope {
    MIR_INSTRUCTION_HEADER;
    int id;
};

struct MirSetUpvalue {
    MIR_INSTRUCTION_HEADER;
    int index;
    struct MirRegister *value;
};

// TODO: find a way to get rid of this!
struct MirDiscard {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *reg;
};

struct MirConstant {
    MIR_INSTRUCTION_HEADER;
    paw_Type code;
    Value value;
    struct MirRegister *output;
};

struct MirExplode {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *input;
    struct MirRegisterList *outputs;
};

struct MirContainer {
    MIR_INSTRUCTION_HEADER;
    int nelems;
    struct MirRegister *output;
};

struct MirAggregate {
    MIR_INSTRUCTION_HEADER;
    int discr;
    int nfields;
    struct MirRegister *output;
};

struct MirAssign {
    MIR_INSTRUCTION_HEADER;
    paw_Bool is_upvalue : 1;
    int place;
    struct MirRegister *rhs;
};

struct MirClosure {
    MIR_INSTRUCTION_HEADER;
    int child_id;
    struct MirRegister *output;
};

struct MirGetElement {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *output;
    struct MirRegister *object;
    struct MirRegister *key;
};

struct MirSetElement {
    MIR_INSTRUCTION_HEADER;
    paw_Bool is_init : 1;
    struct MirRegister *object;
    struct MirRegister *key;
    struct MirRegister *value;
};

struct MirGetRange {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *output;
    struct MirRegister *object;
    struct MirRegister *lower;
    struct MirRegister *upper;
};

struct MirSetRange {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *object;
    struct MirRegister *lower;
    struct MirRegister *upper;
    struct MirRegister *value;
};

struct MirGetField {
    MIR_INSTRUCTION_HEADER;
    int index;
    struct MirRegister *output;
    struct MirRegister *object;
};

struct MirSetField {
    MIR_INSTRUCTION_HEADER;
    paw_Bool is_init : 1;
    int index;
    struct MirRegister *object;
    struct MirRegister *value;
};

struct MirUnaryOp {
    MIR_INSTRUCTION_HEADER;
    enum UnaryOp op : 8;
    struct MirRegister *val;
    struct MirRegister *output;
};

struct MirBinaryOp {
    MIR_INSTRUCTION_HEADER;
    enum BinaryOp op : 8;
    struct MirRegister *lhs;
    struct MirRegister *rhs;
    struct MirRegister *output;
};

struct MirCast {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *target;
    paw_Type type;
    struct MirRegister *output;
};

struct MirCall {
    MIR_INSTRUCTION_HEADER;
    struct MirRegister *target;
    struct MirRegisterList *args;
    struct MirRegister *output;
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
    struct MirRegister *var;
    struct MirRegister *iter;
    struct MirRegister *end;
    struct MirRegister *step;
    MirBlockId then_arm;
    MirBlockId else_arm;
};

struct MirBranch {
    MIR_TERMINATOR_HEADER;
    struct MirRegister *cond;
    MirBlockId then_arm;
    MirBlockId else_arm;
};

struct MirSwitchArm {
    MirBlockId bid;
    Value value;
};

struct MirSwitch {
    MIR_TERMINATOR_HEADER;
    paw_Bool has_otherwise : 1;
    struct MirRegister *discr;
    struct MirSwitchArmList *arms;
    MirBlockId otherwise;
};

struct MirGoto {
    MIR_TERMINATOR_HEADER;
    MirBlockId target;
};

struct MirReturn {
    MIR_TERMINATOR_HEADER;
    struct MirRegister *value;
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

struct MirBlock {
    MirBlockId bid;
    struct MirInstructionList *code;
    struct MirTerminator *term;
};

struct Mir {
    struct MirScopeList *scopes;
    struct MirUpvalueList *upvalues;
    struct MirBodyList *children;
    struct MirBlockList *blocks;
    struct IrType *type;
    struct IrType *self;
    String *name;
    enum FuncKind fn_kind;
    paw_Bool is_native : 1;
    paw_Bool is_poly : 1;
    paw_Bool is_pub : 1;
};

#define MIR_KINDOF(node) ((node)->hdr.kind)
#define MIR_CAST_TERMINATOR(p) CAST(struct MirTerminator *, p)
#define MIR_CAST_INSTRUCTION(p) CAST(struct MirInstruction *, p)

struct MirScope *pawMir_get_scope(struct Mir *mir, MirScopeId id);

// TODO: registers and switch arms don't need stable addresses
DEFINE_LIST_V2(struct Compiler, pawMir_upvalue_list_, MirUpvalueList, struct MirUpvalueInfo)
DEFINE_LIST_V2(struct Compiler, pawMir_scope_list_, MirScopeList, struct MirScope)
DEFINE_LIST_V2(struct Compiler, pawMir_switch_list_, MirSwitchArmList, struct MirSwitchArm)
DEFINE_LIST_V2(struct Compiler, pawMir_register_list_, MirRegisterList, struct MirRegister *)
DEFINE_LIST_V2(struct Compiler, pawMir_instruction_list_, MirInstructionList, struct MirInstruction *)
DEFINE_LIST_V2(struct Compiler, pawMir_block_list_, MirBlockList, struct MirBlock *)
DEFINE_LIST_V2(struct Compiler, pawMir_body_list_, MirBodyList, struct Mir *)

struct Mir *pawMir_new(struct Compiler *C, String *name, struct IrType *type, struct IrType *self, enum FuncKind fn_kind, paw_Bool is_native, paw_Bool is_pub, paw_Bool is_poly);
struct MirRegister *pawMir_new_register(struct Compiler *C, int value, struct IrType *type);
struct MirInstruction *pawMir_new_instruction(struct Compiler *C, enum MirInstructionKind kind);
struct MirTerminator *pawMir_new_terminator(struct Compiler *C, enum MirTerminatorKind kind);
struct MirTerminator *pawMir_new_return(struct Compiler *C, struct MirRegister *value);
struct MirTerminator *pawMir_new_goto(struct Compiler *C, MirBlockId bid);
struct MirBlock *pawMir_new_block(struct Compiler *C, MirBlockId bid);

struct MirVisitor {
    struct Compiler *C;
    void *ud;

    paw_Bool (*VisitTerminator)(struct MirVisitor *V, struct MirTerminator *node);
    paw_Bool (*VisitInstruction)(struct MirVisitor *V, struct MirInstruction *node);
    paw_Bool (*VisitBlock)(struct MirVisitor *V, struct MirBlock *node);
    paw_Bool (*VisitRegister)(struct MirVisitor *V, struct MirRegister *node);

    void (*PostVisitTerminator)(struct MirVisitor *V, struct MirTerminator *node);
    void (*PostVisitInstruction)(struct MirVisitor *V, struct MirInstruction *node);
    void (*PostVisitBlock)(struct MirVisitor *V, struct MirBlock *node);
    void (*PostVisitRegister)(struct MirVisitor *V, struct MirRegister *node);

#define DEFINE_CALLBACK(X) paw_Bool (*Visit##X)(struct MirVisitor *V, struct Mir##X *node); \
                           void (*PostVisit##X)(struct MirVisitor *V, struct Mir##X *node);
    MIR_INSTRUCTION_LIST(DEFINE_CALLBACK)
    MIR_TERMINATOR_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void pawMir_visitor_init(struct MirVisitor *V, struct Compiler *C, void *ud);
void pawMir_visit(struct MirVisitor *V, struct Mir *mir);

// Visitor entrypoints for each kind of HIR node:
void pawMir_visit_instruction(struct MirVisitor *V, struct MirInstruction *node);
void pawMir_visit_terminator(struct MirVisitor *V, struct MirTerminator *node);
void pawMir_visit_block(struct MirVisitor *V, struct MirBlock *node);
void pawMir_visit_register(struct MirVisitor *V, struct MirRegister *node);
void pawMir_visit_instruction_list(struct MirVisitor *V, struct MirInstructionList *list);
void pawMir_visit_block_list(struct MirVisitor *V, struct MirBlockList *list);
void pawMir_visit_register_list(struct MirVisitor *V, struct MirRegisterList *list);

const char *pawMir_dump(struct Compiler *C, struct Mir *mir);

#endif // PAW_MIR_H

