// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_MIR_H
#define PAW_MIR_H

#include "compiler/compiler.h"
#include "ir/type.h"

struct Mir;

#define MIR_INSTRUCTION_LIST(X) \
    X(Noop) \
    X(Move) \
    X(Load) \
    X(Store) \
    X(AddrOf) \
    X(Global) \
    X(AllocLocal) \
    X(Aggregate) \
    X(Array) \
    X(ArrayGEP) \
    X(StructGEP) \
    X(GetConstant) \
    X(GetDiscriminant) \
    X(SetRange) \
    X(GetRange) \
    X(Kill) \
    X(Drop) \
    X(Call) \
    X(Cast) \
    X(Capture) \
    X(Closure) \
    X(UnaryOp) \
    X(BinaryOp) \
    X(Unreachable) \
    X(Return) \
    X(Branch) \
    X(Switch) \
    X(Goto)

#define MIR_PROJECTION_LIST(X) \
    X(Deref) \
    X(Field) \
    X(Index) \
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

typedef struct MirId {
    int value;
} MirId;

static MirId mir_next_id(struct Mir *mir);


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
};


enum MirInstructionKind {
#define DEFINE_ENUM(X) kMir##X,
    MIR_INSTRUCTION_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define MIR_INSTRUCTION_HEADER \
    struct SourceSpan span; \
    MirId mid; \
    enum MirInstructionKind kind : 8

struct MirInstructionHeader {
    MIR_INSTRUCTION_HEADER;
};

struct MirNoop {
    MIR_INSTRUCTION_HEADER;
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
    struct Str const *name;
};

struct MirArray {
    MIR_INSTRUCTION_HEADER;
    struct MirPlaceList *elems;
    struct MirPlace output;
};

struct MirAggregate {
    MIR_INSTRUCTION_HEADER;
    int discr;
    struct MirPlaceList *fields;
    struct MirPlace output;
};

struct MirCapture {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace target;
};

struct MirClosure {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace output;
};

struct MirStructGEP {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace output;
    struct MirPlace object;
    int field;
    int discr;
};

struct MirArrayGEP {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace output;
    struct MirPlace array;
    struct MirPlace index;
    int field;
};

struct MirGetConstant {
    MIR_INSTRUCTION_HEADER;
    MirConstant k;
    struct MirPlace output;
};

struct MirGetDiscriminant {
    MIR_INSTRUCTION_HEADER;
    struct MirPlace object;
    struct MirPlace output;
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
    MIR_UNARY_NEG,
    MIR_UNARY_BITNOT,
    MIR_UNARY_NOT,
};

struct MirUnaryOp {
    MIR_INSTRUCTION_HEADER;
    enum MirUnaryOpKind op : 8;
    struct MirPlace val;
    struct MirPlace output;
};

enum MirBinaryOpKind {
    MIR_BINARY_EQ,
    MIR_BINARY_NE,
    MIR_BINARY_LT,
    MIR_BINARY_LE,
    MIR_BINARY_ADD,
    MIR_BINARY_SUB,
    MIR_BINARY_MUL,
    MIR_BINARY_DIV,
    MIR_BINARY_MOD,
    MIR_BINARY_BITAND,
    MIR_BINARY_BITOR,
    MIR_BINARY_BITXOR,
    MIR_BINARY_SHL,
    MIR_BINARY_SHR,
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

#define DEFINE_ACCESS(X) \
    static inline paw_Bool MirIs##X(struct MirInstruction const *node) \
    { \
        return node->hdr.kind == kMir##X; \
    } \
    static inline struct Mir##X *MirGet##X(struct MirInstruction *node) \
    { \
        paw_assert(MirIs##X(node)); \
        return &node->X##_; \
    }
MIR_INSTRUCTION_LIST(DEFINE_ACCESS)
#undef DEFINE_ACCESS

static char const *kMirInstructionNames[] = {
#define DEFINE_NAME(X) "Mir" #X,
    MIR_INSTRUCTION_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

struct MirInstruction *mir_new_instruction(struct Mir *mir);

struct MirInstruction *mir_new_move(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace target);

struct MirInstruction *mir_new_load(struct Mir *mir, struct SourceSpan span, struct MirPlace pointer, struct MirPlace output);

struct MirInstruction *mir_new_store(struct Mir *mir, struct SourceSpan span, struct MirPlace value, struct MirPlace pointer);

struct MirInstruction *mir_new_addr_of(struct Mir *mir, struct SourceSpan span, struct MirPlace input, struct MirPlace output);

struct MirInstruction *mir_new_global(struct Mir *mir, struct SourceSpan span, struct MirPlace output);

struct MirInstruction *mir_new_noop(struct Mir *mir, struct SourceSpan span);

struct MirInstruction *mir_new_alloc_local(struct Mir *mir, struct SourceSpan span, struct Str const *name, struct MirPlace output);

struct MirInstruction *mir_new_array(struct Mir *mir, struct SourceSpan span, struct MirPlaceList *elems, struct MirPlace output);

struct MirInstruction *mir_new_aggregate(struct Mir *mir, struct SourceSpan span, struct MirPlaceList *fields, struct MirPlace output, int discr);

struct MirInstruction *mir_new_capture(struct Mir *mir, struct SourceSpan span, struct MirPlace target);

struct MirInstruction *mir_new_close(struct Mir *mir, struct SourceSpan span, struct MirPlace target);

struct MirInstruction *mir_new_closure(struct Mir *mir, struct SourceSpan span, struct MirPlace output);

struct MirInstruction *mir_new_struct_gep(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace object, int field, int discr);

struct MirInstruction *mir_new_array_gep(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace array, struct MirPlace index);

struct MirInstruction *mir_new_get_constant(struct Mir *mir, struct SourceSpan span, MirConstant k, struct MirPlace output);

struct MirInstruction *mir_new_get_discriminant(struct Mir *mir, struct SourceSpan span, struct MirPlace object, struct MirPlace output);

struct MirInstruction *mir_new_set_discriminant(struct Mir *mir, struct SourceSpan span, struct MirPlace object, paw_Uint64 value);

struct MirInstruction *mir_new_get_range(struct Mir *mir, struct SourceSpan span, enum BuiltinKind b_kind, struct MirPlace output, struct MirPlace object, struct MirPlace lower, struct MirPlace upper);

struct MirInstruction *mir_new_set_range(struct Mir *mir, struct SourceSpan span, enum BuiltinKind b_kind, struct MirPlace object, struct MirPlace lower, struct MirPlace upper, struct MirPlace value);

struct MirInstruction *mir_new_unary_op(struct Mir *mir, struct SourceSpan span, enum MirUnaryOpKind op, struct MirPlace val, struct MirPlace output);

struct MirInstruction *mir_new_binary_op(struct Mir *mir, struct SourceSpan span, enum MirBinaryOpKind op, struct MirPlace lhs, struct MirPlace rhs, struct MirPlace output);

struct MirInstruction *mir_new_cast(struct Mir *mir, struct SourceSpan span, struct MirPlace target, struct MirPlace output);

struct MirInstruction *mir_new_kill(struct Mir *mir, struct SourceSpan span, struct MirPlace target);

struct MirInstruction *mir_new_drop(struct Mir *mir, struct SourceSpan span, struct MirPlace target);

struct MirInstruction *mir_new_call(struct Mir *mir, struct SourceSpan span, struct MirPlace target, struct MirPlaceList *args, struct MirPlace output);

struct MirInstruction *mir_new_goto(struct Mir *mir, struct SourceSpan span);

struct MirInstruction *mir_new_branch(struct Mir *mir, struct SourceSpan span, struct MirPlace cond);

struct MirInstruction *mir_new_switch(struct Mir *mir, struct SourceSpan span, struct MirPlace discr, struct MirSwitchArmList *arms, paw_Bool has_otherwise);

struct MirInstruction *mir_new_unreachable(struct Mir *mir, struct SourceSpan span);

struct MirInstruction *mir_new_return(struct Mir *mir, struct SourceSpan span);

struct MirConstantData {
    IrConst *data;
    IrType *type;
};

struct MirRegisterData {
    paw_Bool is_captured : 1;
    struct SourceSpan span;
    struct IrType *type;
    struct Str const *name;
};

struct MirBlockData {
    struct MirBlockList *predecessors;
    struct MirBlockList *successors;
    struct MirInstructionList *instructions;
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

DEFINE_MAP(MirConstMap, ir_const_hash, ir_const_equals, IrConst *, MirConstant)

struct MirConstantCache *mir_kcache_new(struct Mir *mir);
void mir_kcache_delete(struct Mir *mir, struct MirConstantCache *kcache);
MirConstant mir_kcache_add_value(struct Mir *mir, struct MirConstantCache *kcache, union IrValue value, IrType *type);
MirConstant mir_kcache_add_param(struct Mir *mir, struct MirConstantCache *kcache, DeclId did);

struct Mir {
    struct Pool *pool;
    struct Allocator a;
    struct Annotations *annotations;
    struct MirRegisterDataList *registers;
    struct MirBlockDataList *blocks;
    struct MirUpvalueList *upvalues;
    struct MirCaptureList *captured;
    struct MirConstantCache *kcache;
    struct SourceSpan span;
    struct IrTypeList *param_types;
    struct IrType *result_type;
    struct IrType *env_type;
    struct IrType *type;
    struct IrType *self;
    struct Compiler *C;
    paw_Env *P;
    IrGenericArgs *args;
    DeclId did;
    DeclId parent_id;
    int mir_count;
    int modno;
    struct Str const *name;
    enum FnKind fn_kind : 8;
    paw_Bool is_method : 1;
    paw_Bool is_poly : 1;
    paw_Bool is_pub : 1;
};

EXTERN_C paw_Bool mir_is_main(struct Mir const *mir);
EXTERN_C struct MirPlace mir_get_register(struct Mir const *mir, MirRegister r);

#define MIR_KINDOF(node) ((node)->hdr.kind)
#define MIR_CAST_INSTRUCTION(p) CAST(struct MirInstruction *, p)

DEFINE_LIST(MirCaptureList, struct MirCaptureInfo)
DEFINE_LIST(MirUpvalueList, struct MirUpvalueInfo)
DEFINE_LIST(MirSwitchArmList, struct MirSwitchArm)
DEFINE_LIST(MirProjectionList, struct MirProjection *)
DEFINE_LIST(MirInstructionList, struct MirInstruction *)
DEFINE_LIST(MirPlaceList, struct MirPlace)
DEFINE_LIST(MirPlacePtrList, struct MirPlace *)
DEFINE_LIST(MirConstantList, MirConstant)
DEFINE_LIST(MirRegisterList, MirRegister)
DEFINE_LIST(MirBlockList, MirBlock)
DEFINE_LIST(MirBucketList, struct MirBlockList *)
DEFINE_LIST(MirConstantDataList, struct MirConstantData)
DEFINE_LIST(MirRegisterDataList, struct MirRegisterData)
DEFINE_LIST(MirRegisterPtrList, MirRegister *)
DEFINE_LIST(MirBlockDataList, struct MirBlockData *)
DEFINE_LIST(MirBodyList, struct Mir *)

struct Mir *mir_new(struct Compiler *C, int modno, struct SourceSpan span, struct Str const *name, DeclId did, IrGenericArgs *args, IrTypeList *param_types, IrType *result_type, Annotations *annotations, struct IrType *type, struct IrType *self, DeclId parent_id, enum FnKind fn_kind, paw_Bool is_pub, paw_Bool is_poly);
void mir_free(struct Mir *mir);

struct MirBlockData *mir_new_block(struct Mir *mir);

// Get a pointer to each variable read or written by a given instruction
struct MirPlacePtrList *mir_get_loads(struct Mir *mir, struct MirInstruction *instr);
struct MirPlacePtrList *mir_get_stores(struct Mir *mir, struct MirInstruction *instr);

static MirId mir_next_id(struct Mir *mir)
{
    return (MirId){mir->mir_count++};
}

static MirId mir_bb_first(struct MirBlockData const *block)
{
    return block->mid;
}

static MirId mir_bb_last(struct MirBlockData const *block)
{
    if (block->instructions->count == 0) return block->mid;
    return MirInstructionList_last(block->instructions)->hdr.mid;
}

static struct MirBlockData *mir_bb_data(struct Mir const *mir, MirBlock bb)
{
    return MirBlockDataList_get(mir->blocks, bb.value);
}

static struct MirConstantData *mir_const_data(struct Mir const *mir, MirConstant k)
{
    paw_assert(0 <= k.value && k.value < mir->kcache->data->count);
    return &LIST_AT(mir->kcache->data, k.value);
}

static struct MirRegisterData *mir_reg_data(struct Mir const *mir, MirRegister r)
{
    paw_assert(0 <= r.value && r.value < mir->registers->count);
    return &LIST_AT(mir->registers, r.value);
}

// Determine the index of "x" in the predecessor list of "y"
static int mir_which_pred(struct Mir const *mir, MirBlock y, MirBlock x)
{
    int index;
    MirBlock const *pb;
    struct MirBlockData const *data = mir_bb_data(mir, y);
    LIST_ENUMERATE (data->predecessors, index, pb) {
        if (MIR_ID_EQUALS(x, *pb))
            return index;
    }

    PAW_UNREACHABLE();
}

static int mir_which_succ(struct Mir const *mir, MirBlock x, MirBlock y)
{
    int index;
    MirBlock const *pb;
    struct MirBlockData const *data = mir_bb_data(mir, x);
    LIST_ENUMERATE (data->successors, index, pb) {
        if (MIR_ID_EQUALS(y, *pb))
            return index;
    }

    PAW_UNREACHABLE();
}

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

#define DEFINE_CALLBACK(X) \
    paw_Bool (*Visit##X)(struct MirVisitor * V, struct Mir##X * node); \
    void (*PostVisit##X)(struct MirVisitor * V, struct Mir##X * node);
    MIR_INSTRUCTION_LIST(DEFINE_CALLBACK)
#undef DEFINE_CALLBACK
};

void mir_visitor_init(struct MirVisitor *V, struct Compiler *C, struct Mir *mir, void *ud);
void mir_visit(struct MirVisitor *V);

// Visitor entrypoints for each kind of HIR node:
void mir_visit_instruction(struct MirVisitor *V, struct MirInstruction *node);
void mir_visit_block(struct MirVisitor *V, MirBlock node);
void mir_visit_place(struct MirVisitor *V, struct MirPlace node);
void mir_visit_instruction_list(struct MirVisitor *V, struct MirInstructionList *list);
void mir_visit_block_list(struct MirVisitor *V, struct MirBlockList *list);
void mir_visit_place_list(struct MirVisitor *V, struct MirPlaceList *list);

// Compute the immediate dominator of each basic block
struct MirBlockList *mir_compute_dominance_tree(struct Compiler *C, struct Mir *mir);

// Compute the dominance frontier set of each basic block
struct MirBucketList *mir_compute_dominance_frontiers(struct Compiler *C, struct Mir *mir, struct MirBlockList *idom);

// Return the reverse postorder (RPO) traversal of the CFG
// If there is a path from node X to node Y in the CFG, then X must have a lower RPO
// number than node Y.
struct MirBlockList *mir_traverse_rpo(struct Compiler *C, struct Mir *mir);

// Order basic blocks in reverse postorder
// Removes all basic blocks that cannot be reached from the entry.
void mir_renumber_basic_blocks(struct Mir *mir);

struct MirAccess {
    struct MirInstruction *instr;
    MirBlock b;
};

DEFINE_LIST(MirAccessList, struct MirAccess)

struct AccessMap;
struct UseDefMap;

// NOTE: Creates duplicate "MirAccess" objects when a register is accessed multiple
//       times by the same instruction.
void mir_collect_per_instr_uses(struct Mir *mir, struct AccessMap *uses);
void mir_collect_per_instr_defs(struct Mir *mir, struct AccessMap *defs);

void mir_collect_per_block_usedefs(struct Mir *mir, struct UseDefMap *uses, struct UseDefMap *defs);

void mir_generate_drops(struct Mir *mir);

void mir_propagate_constants(struct Mir *mir);
void mir_merge_redundant_blocks(struct Mir *mir);

DEFINE_LIST(MirLocationList, int)

struct MirLocationList *mir_compute_locations(struct Mir *mir);
void mir_set_location(struct Mir *mir, struct MirLocationList *locations, MirId mid, int location);
static int mir_get_location(struct MirLocationList *locations, MirId mid)
{
    return MirLocationList_get(locations, mid.value);
}

struct MirBlockList *mir_compute_live_in(struct Mir *mir, struct MirBlockList *uses, struct MirBlockList *defs, MirRegister r);
struct MirIntervalMap *mir_compute_liveness(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, struct MirLocationList *locations);

static IrType *mir_place_type(struct Mir *mir, struct MirPlace place)
{
    switch (place.kind) {
        case MIR_PLACE_REGISTER:
            return mir_reg_data(mir, place.r)->type;
        case MIR_PLACE_CONSTANT:
            return mir_const_data(mir, place.k)->type;
        case MIR_PLACE_UPVALUE:
            return MirUpvalueList_get(mir->upvalues, place.up).type;
    }
}

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

DEFINE_MAP(AccessMap, P_ID_HASH, P_ID_EQUALS, MirRegister, struct MirAccessList *)
DEFINE_MAP(UseDefMap, P_ID_HASH, P_ID_EQUALS, MirRegister, struct MirBlockList *)
DEFINE_MAP_ITERATOR(UseDefMap, MirRegister, struct MirBlockList *)
DEFINE_MAP_ITERATOR(BodyMap, DeclId, struct Mir *)

enum MirFoldResult {
    MIR_FOLD_FOLDED,
    MIR_FOLD_OVERFLOW,
    MIR_FOLD_DIVIDE_BY_ZERO,
};

enum MirFoldResult mir_fold_unary_op(enum MirUnaryOpKind op, IrType *type, union IrValue v, union IrValue *pr);
enum MirFoldResult mir_fold_binary_op(enum MirBinaryOpKind op, IrType *type, union IrValue x, union IrValue y, union IrValue *pr);

void mir_fold_cast(union IrValue from, IrType *from_type, IrType *to_type, union IrValue *to);

// Push a human-readable representation of the MIR on to the stack
// Returns a pointer to the buffer containing null-terminated text.
EXTERN_C char const *mir_dump(struct Mir *mir);
EXTERN_C char const *mir_dump_graph(struct Mir *mir);

#if NDEBUG
# define MIR_VALIDATE_GRAPH(Mir_)
#else
void mir_validate_graph(struct Mir *mir);
# define MIR_VALIDATE_GRAPH(Mir_) mir_validate_graph(Mir_)
#endif // !NDEBUG

#endif // PAW_MIR_H
