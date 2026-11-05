// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "mir.h"

#include "common/map.h"
#include "ir/type.h"

struct Mir *mir_new(struct Compiler *C, int modno, struct SourceSpan span, Str const *name, DeclId did, IrGenericArgs *args, IrTypeList *param_types, IrType *result_type, Annotations *annotations, IrType *type, IrType *self, DeclId parent_id, enum FnKind fn_kind, paw_Bool is_pub, paw_Bool is_poly)
{
    struct Mir *mir = pool_alloc(C->mir_pool, NULL, 0, sizeof(*mir));
    *mir = (struct Mir){
        .a = POOL_ALLOCATOR(C->mir_pool),
        .pool = C->mir_pool,
        .is_poly = is_poly,
        .is_pub = is_pub,
        .fn_kind = fn_kind,
        .parent_id = parent_id,
        .annotations = annotations,
        .param_types = param_types,
        .result_type = result_type,
        .args = args,
        .did = did,
        .modno = modno,
        .name = name,
        .span = span,
        .type = type,
        .self = self,
        .P = ENV(C),
        .C = C,
    };
    mir->captured = MirCaptureList_new(ALLOCATOR_OF(mir));
    mir->kcache = mir_kcache_new(mir);
    mir->registers = MirRegisterDataList_new(ALLOCATOR_OF(mir));
    mir->blocks = MirBlockDataList_new(ALLOCATOR_OF(mir));
    mir->upvalues = MirUpvalueList_new(ALLOCATOR_OF(mir));
    return mir;
}

void mir_free(struct Mir *mir)
{
    // reclaim memory used by the MIR
    MirCaptureList_delete(mir->captured);
    MirRegisterDataList_delete(mir->registers);
    MirBlockDataList_delete(mir->blocks);
    MirUpvalueList_delete(mir->upvalues);
    mir_kcache_delete(mir, mir->kcache);
    pool_alloc(mir->pool, mir, sizeof(*mir), 0);
}

struct MirInstruction *mir_new_move(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace target)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Move_ = (struct MirMove){
        .mid = mir_next_id(mir),
        .kind = kMirMove,
        .span = span,
        .output = output,
        .target = target,
    };
    return instr;
}

struct MirInstruction *mir_new_load(struct Mir *mir, struct SourceSpan span, struct MirPlace pointer, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Load_ = (struct MirLoad){
        .mid = mir_next_id(mir),
        .kind = kMirLoad,
        .span = span,
        .pointer = pointer,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_store(struct Mir *mir, struct SourceSpan span, struct MirPlace value, struct MirPlace pointer)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Store_ = (struct MirStore){
        .mid = mir_next_id(mir),
        .kind = kMirStore,
        .span = span,
        .value = value,
        .pointer = pointer,
    };
    return instr;
}

struct MirInstruction *mir_new_addr_of(struct Mir *mir, struct SourceSpan span, struct MirPlace input, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->AddrOf_ = (struct MirAddrOf){
        .mid = mir_next_id(mir),
        .kind = kMirAddrOf,
        .span = span,
        .input = input,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_global(struct Mir *mir, struct SourceSpan span, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Global_ = (struct MirGlobal){
        .mid = mir_next_id(mir),
        .kind = kMirGlobal,
        .span = span,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_noop(struct Mir *mir, struct SourceSpan span)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Noop_ = (struct MirNoop){
        .mid = mir_next_id(mir),
        .kind = kMirNoop,
        .span = span,
    };
    return instr;
}

struct MirInstruction *mir_new_alloc_local(struct Mir *mir, struct SourceSpan span, Str const *name, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->AllocLocal_ = (struct MirAllocLocal){
        .mid = mir_next_id(mir),
        .kind = kMirAllocLocal,
        .span = span,
        .output = output,
        .name = name,
    };
    return instr;
}

struct MirInstruction *mir_new_array(struct Mir *mir, struct SourceSpan span, struct MirPlaceList *elems, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Array_ = (struct MirArray){
        .mid = mir_next_id(mir),
        .kind = kMirArray,
        .span = span,
        .elems = elems,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_aggregate(struct Mir *mir, struct SourceSpan span, struct MirPlaceList *fields, struct MirPlace output, int discr)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Aggregate_ = (struct MirAggregate){
        .mid = mir_next_id(mir),
        .kind = kMirAggregate,
        .span = span,
        .discr = discr,
        .fields = fields,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_capture(struct Mir *mir, struct SourceSpan span, struct MirPlace target)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Capture_ = (struct MirCapture){
        .mid = mir_next_id(mir),
        .kind = kMirCapture,
        .span = span,
        .target = target,
    };
    return instr;
}

struct MirInstruction *mir_new_closure(struct Mir *mir, struct SourceSpan span, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Closure_ = (struct MirClosure){
        .mid = mir_next_id(mir),
        .kind = kMirClosure,
        .span = span,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_struct_gep(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace object, int field, int discr)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->StructGEP_ = (struct MirStructGEP){
        .mid = mir_next_id(mir),
        .kind = kMirStructGEP,
        .span = span,
        .output = output,
        .object = object,
        .field = field,
        .discr = discr,
    };
    return instr;
}

struct MirInstruction *mir_new_array_gep(struct Mir *mir, struct SourceSpan span, struct MirPlace output, struct MirPlace array, struct MirPlace index)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->ArrayGEP_ = (struct MirArrayGEP){
        .mid = mir_next_id(mir),
        .kind = kMirArrayGEP,
        .span = span,
        .output = output,
        .array = array,
        .index = index,
    };
    return instr;
}

struct MirInstruction *mir_new_get_constant(struct Mir *mir, struct SourceSpan span, MirConstant k, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->GetConstant_ = (struct MirGetConstant){
        .mid = mir_next_id(mir),
        .kind = kMirGetConstant,
        .span = span,
        .k = k,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_get_discriminant(struct Mir *mir, struct SourceSpan span, struct MirPlace object, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->GetDiscriminant_ = (struct MirGetDiscriminant){
        .mid = mir_next_id(mir),
        .kind = kMirGetDiscriminant,
        .span = span,
        .object = object,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_get_range(struct Mir *mir, struct SourceSpan span, enum BuiltinKind b_kind, struct MirPlace output, struct MirPlace object, struct MirPlace lower, struct MirPlace upper)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->GetRange_ = (struct MirGetRange){
        .mid = mir_next_id(mir),
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

struct MirInstruction *mir_new_set_range(struct Mir *mir, struct SourceSpan span, enum BuiltinKind b_kind, struct MirPlace object, struct MirPlace lower, struct MirPlace upper, struct MirPlace value)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->SetRange_ = (struct MirSetRange){
        .mid = mir_next_id(mir),
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

struct MirInstruction *mir_new_unary_op(struct Mir *mir, struct SourceSpan span, enum MirUnaryOpKind op, struct MirPlace val, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->UnaryOp_ = (struct MirUnaryOp){
        .mid = mir_next_id(mir),
        .kind = kMirUnaryOp,
        .span = span,
        .op = op,
        .val = val,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_binary_op(struct Mir *mir, struct SourceSpan span, enum MirBinaryOpKind op, struct MirPlace lhs, struct MirPlace rhs, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->BinaryOp_ = (struct MirBinaryOp){
        .mid = mir_next_id(mir),
        .kind = kMirBinaryOp,
        .span = span,
        .op = op,
        .lhs = lhs,
        .rhs = rhs,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_cast(struct Mir *mir, struct SourceSpan span, struct MirPlace target, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Cast_ = (struct MirCast){
        .mid = mir_next_id(mir),
        .kind = kMirCast,
        .span = span,
        .target = target,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_kill(struct Mir *mir, struct SourceSpan span, struct MirPlace target)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Kill_ = (struct MirKill){
        .mid = mir_next_id(mir),
        .kind = kMirKill,
        .span = span,
        .target = target,
    };
    return instr;
}

struct MirInstruction *mir_new_drop(struct Mir *mir, struct SourceSpan span, struct MirPlace target)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Drop_ = (struct MirDrop){
        .mid = mir_next_id(mir),
        .kind = kMirDrop,
        .span = span,
        .target = target,
    };
    return instr;
}

struct MirInstruction *mir_new_call(struct Mir *mir, struct SourceSpan span, struct MirPlace target, struct MirPlaceList *args, struct MirPlace output)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Call_ = (struct MirCall){
        .mid = mir_next_id(mir),
        .kind = kMirCall,
        .span = span,
        .target = target,
        .args = args,
        .output = output,
    };
    return instr;
}

struct MirInstruction *mir_new_goto(struct Mir *mir, struct SourceSpan span)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Goto_ = (struct MirGoto){
        .mid = mir_next_id(mir),
        .kind = kMirGoto,
        .span = span,
    };
    return instr;
}

struct MirInstruction *mir_new_branch(struct Mir *mir, struct SourceSpan span, struct MirPlace cond)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Branch_ = (struct MirBranch){
        .mid = mir_next_id(mir),
        .kind = kMirBranch,
        .span = span,
        .cond = cond,
    };
    return instr;
}

struct MirInstruction *mir_new_switch(struct Mir *mir, struct SourceSpan span, struct MirPlace discr, struct MirSwitchArmList *arms, paw_Bool has_otherwise)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Switch_ = (struct MirSwitch){
        .mid = mir_next_id(mir),
        .kind = kMirSwitch,
        .span = span,
        .discr = discr,
        .arms = arms,
        .has_otherwise = has_otherwise,
    };
    return instr;
}

struct MirInstruction *mir_new_unreachable(struct Mir *mir, struct SourceSpan span)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Unreachable_ = (struct MirUnreachable){
        .mid = mir_next_id(mir),
        .kind = kMirUnreachable,
        .span = span,
    };
    return instr;
}

struct MirInstruction *mir_new_return(struct Mir *mir, struct SourceSpan span)
{
    struct MirInstruction *instr = mir_new_instruction(mir);
    instr->Return_ = (struct MirReturn){
        .mid = mir_next_id(mir),
        .kind = kMirReturn,
        .span = span,
    };
    return instr;
}
paw_Bool mir_is_main(struct Mir const *mir)
{
    return mir->self == NULL && pawS_eq(mir->name, SCAN_STR(mir->C, "main"));
}

struct MirPlace mir_get_register(struct Mir const *mir, MirRegister r)
{
    struct MirRegisterData const *rdata = mir_reg_data((struct Mir *)mir, r);
    return (struct MirPlace){
        .kind = MIR_PLACE_REGISTER,
        .span = rdata->span,
        .r = r,
    };
}

struct MirConstantCache *mir_kcache_new(struct Mir *mir)
{
    struct MirConstantCache *kcache = pool_alloc(mir->pool, NULL, 0, sizeof(*kcache));
    *kcache = (struct MirConstantCache){
        .data = MirConstantDataList_new(ALLOCATOR_OF(mir)),
        .chars = MirConstMap_new(ALLOCATOR_OF(mir)),
        .ints = MirConstMap_new(ALLOCATOR_OF(mir)),
        .floats = MirConstMap_new(ALLOCATOR_OF(mir)),
        .strs = MirConstMap_new(ALLOCATOR_OF(mir)),
        .params = MirConstMap_new(ALLOCATOR_OF(mir)),
    };

#define LOADK(Kind_, Value_) (MirConstantDataList_push(kcache->data, \
            (struct MirConstantData){.data = ir_new_const_value(mir->C, Value_, \
                        ir_new_##Kind_(mir->C)), \
                    .type = ir_new_##Kind_(mir->C)}), \
        MIR_CONST(kcache->data->count - 1));
    kcache->unitk = LOADK(unit, I2V(0));
    kcache->boolk[PAW_FALSE] = LOADK(bool, I2V(PAW_FALSE));
    kcache->boolk[PAW_TRUE] = LOADK(bool, I2V(PAW_TRUE));
#undef LOADK

    return kcache;
}

void mir_kcache_delete(struct Mir *mir, struct MirConstantCache *kcache)
{
    MirConstantDataList_delete(kcache->data);
    MirConstMap_delete(kcache->ints);
    MirConstMap_delete(kcache->floats);
    MirConstMap_delete(kcache->strs);

    pool_alloc(mir->pool, kcache, sizeof(*kcache), 0);
}

MirConstant mir_kcache_add_value(struct Mir *mir, struct MirConstantCache *kcache, union IrValue value, IrType *type)
{
    if (IrIsUnit(type)) return kcache->unitk;
    if (IrIsBool(type)) return kcache->boolk[value.b];
    MirConstMap *map = IrIsChar(type) ? kcache->chars :
        IrIsFloat(type) ? kcache->floats :
        IrIsString(type) ? kcache->strs :
        kcache->ints;
    IrConst *k = ir_new_const_value(mir->C, value, type);
    MirConstant const *pk = MirConstMap_get(map, k);
    if (pk != NULL) return *pk;

    MirConstant const id = MIR_CONST(kcache->data->count);
    MirConstantDataList_push(kcache->data,
        (struct MirConstantData){
            .data = k,
            .type = type,
        });

    MirConstMap_insert(map, k, id);
    return id;
}

MirConstant mir_kcache_add_param(struct Mir *mir, struct MirConstantCache *kcache, DeclId did)
{
    IrConst *konst = ir_new_const_decl(mir->C, did);
    MirConstant const *pk = MirConstMap_get(mir->kcache->params, konst);
    if (pk != NULL) return *pk;

    MirConstant const id = MIR_CONST(kcache->data->count);
    MirConstantDataList_push(kcache->data,
        (struct MirConstantData){
            .type = ir_get_def_type(mir->C, did),
            .data = konst,
        });
    MirConstMap_insert(mir->kcache->params, konst, id);
    return id;
}

MirInstruction *mir_new_instruction(struct Mir *mir)
{
    return pool_alloc(mir->pool, NULL, 0, sizeof(MirInstruction));
}

struct MirBlockData *mir_new_block(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct MirBlockData *block = pool_alloc(mir->pool, NULL, 0, sizeof(struct MirBlockData));
    *block = (struct MirBlockData){
        .predecessors = MirBlockList_new(ALLOCATOR_OF(mir)),
        .successors = MirBlockList_new(ALLOCATOR_OF(mir)),
        .instructions = MirInstructionList_new(ALLOCATOR_OF(mir)),
        .mid = mir_next_id(mir),
    };
    return block;
}

static void AcceptNoop(struct MirVisitor *V, struct MirNoop *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptMove(struct MirVisitor *V, struct MirMove *t)
{
    mir_visit_place(V, t->output);
    mir_visit_place(V, t->target);
}

static void AcceptLoad(struct MirVisitor *V, struct MirLoad *t)
{
    mir_visit_place(V, t->pointer);
    mir_visit_place(V, t->output);
}

static void AcceptStore(struct MirVisitor *V, struct MirStore *t)
{
    mir_visit_place(V, t->value);
    mir_visit_place(V, t->pointer);
}

static void AcceptAddrOf(struct MirVisitor *V, struct MirAddrOf *t)
{
    mir_visit_place(V, t->input);
    mir_visit_place(V, t->output);
}

static void AcceptGlobal(struct MirVisitor *V, struct MirGlobal *t)
{
    mir_visit_place(V, t->output);
}

static void AcceptAggregate(struct MirVisitor *V, struct MirAggregate *t)
{
    mir_visit_place_list(V, t->fields);
    mir_visit_place(V, t->output);
}

static void AcceptArray(struct MirVisitor *V, struct MirArray *t)
{
    mir_visit_place_list(V, t->elems);
    mir_visit_place(V, t->output);
}

static void AcceptAllocLocal(struct MirVisitor *V, struct MirAllocLocal *t)
{
    mir_visit_place(V, t->output);
}

static void AcceptKill(struct MirVisitor *V, struct MirKill *t)
{
    mir_visit_place(V, t->target);
}

static void AcceptDrop(struct MirVisitor *V, struct MirDrop *t)
{
    mir_visit_place(V, t->target);
}

static void AcceptCall(struct MirVisitor *V, struct MirCall *t)
{
    mir_visit_place(V, t->output);
    mir_visit_place(V, t->target);
    mir_visit_place_list(V, t->args);
}

static void AcceptCast(struct MirVisitor *V, struct MirCast *t)
{
    mir_visit_place(V, t->output);
}

static void AcceptCapture(struct MirVisitor *V, struct MirCapture *t)
{
    mir_visit_place(V, t->target);
}

static void AcceptClosure(struct MirVisitor *V, struct MirClosure *t)
{
    mir_visit_place(V, t->output);
}

static void AcceptStructGEP(struct MirVisitor *V, struct MirStructGEP *t)
{
    mir_visit_place(V, t->output);
    mir_visit_place(V, t->object);
}

static void AcceptArrayGEP(struct MirVisitor *V, struct MirArrayGEP *t)
{
    mir_visit_place(V, t->output);
    mir_visit_place(V, t->array);
    mir_visit_place(V, t->index);
}

static void AcceptGetConstant(struct MirVisitor *V, struct MirGetConstant *t)
{
    mir_visit_place(V, t->output);
}

static void AcceptGetDiscriminant(struct MirVisitor *V, struct MirGetDiscriminant *t)
{
    mir_visit_place(V, t->output);
    mir_visit_place(V, t->object);
}

static void AcceptGetRange(struct MirVisitor *V, struct MirGetRange *t)
{
    mir_visit_place(V, t->output);
    mir_visit_place(V, t->object);
    mir_visit_place(V, t->lower);
    mir_visit_place(V, t->upper);
}

static void AcceptSetRange(struct MirVisitor *V, struct MirSetRange *t)
{
    mir_visit_place(V, t->object);
    mir_visit_place(V, t->lower);
    mir_visit_place(V, t->upper);
    mir_visit_place(V, t->value);
}

static void AcceptUnaryOp(struct MirVisitor *V, struct MirUnaryOp *t)
{
    mir_visit_place(V, t->val);
    mir_visit_place(V, t->output);
}

static void AcceptBinaryOp(struct MirVisitor *V, struct MirBinaryOp *t)
{
    mir_visit_place(V, t->lhs);
    mir_visit_place(V, t->rhs);
    mir_visit_place(V, t->output);
}

static void AcceptReturn(struct MirVisitor *V, struct MirReturn *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptUnreachable(struct MirVisitor *V, struct MirUnreachable *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptGoto(struct MirVisitor *V, struct MirGoto *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptBranch(struct MirVisitor *V, struct MirBranch *t)
{
    mir_visit_place(V, t->cond);
}

static void AcceptSwitch(struct MirVisitor *V, struct MirSwitch *t)
{
    mir_visit_place(V, t->discr);
}

#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL ? (V)->PostVisit##name(V, x) : (void)0)
#define DEFINE_VISITOR_CASES(X) \
    case kMir##X: { \
        struct Mir##X *v = MirGet##X(node); \
        if (VISITOR_CALL(V, X, v)) \
            Accept##X(V, v); \
        VISITOR_POSTCALL(V, X, v); \
    } break;

void mir_visit_place(struct MirVisitor *V, struct MirPlace p)
{
    if (V->VisitPlace(V, p))
        V->PostVisitPlace(V, p);
}

void mir_visit_instruction(struct MirVisitor *V, struct MirInstruction *node)
{
    paw_assert(node != NULL);
    if (!V->VisitInstruction(V, node))
        return;

    switch (MIR_KINDOF(node)) {
        MIR_INSTRUCTION_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitInstruction(V, node);
}

void mir_visit_block(struct MirVisitor *V, MirBlock bb)
{
    struct MirBlockData const *block = mir_bb_data(V->mir, bb);
    paw_assert(block != NULL);

    if (!V->VisitBlock(V, bb))
        return;

    mir_visit_instruction_list(V, block->instructions);

    V->PostVisitBlock(V, bb);
}

paw_Bool default_visit_instruction(struct MirVisitor *V, struct MirInstruction *node)
{
    PAW_UNUSED(V);
    PAW_UNUSED(node);
    return PAW_TRUE;
}

paw_Bool default_visit_block(struct MirVisitor *V, MirBlock node)
{
    PAW_UNUSED(V);
    PAW_UNUSED(node);
    return PAW_TRUE;
}

paw_Bool default_visit_place(struct MirVisitor *V, struct MirPlace node)
{
    PAW_UNUSED(V);
    PAW_UNUSED(node);
    return PAW_TRUE;
}

void default_post_visit_instruction(struct MirVisitor *V, struct MirInstruction *node)
{
    PAW_UNUSED(V);
    PAW_UNUSED(node);
}

void default_post_visit_block(struct MirVisitor *V, MirBlock node)
{
    PAW_UNUSED(V);
    PAW_UNUSED(node);
}

void default_post_visit_place(struct MirVisitor *V, struct MirPlace node)
{
    PAW_UNUSED(V);
    PAW_UNUSED(node);
}

void mir_visitor_init(struct MirVisitor *V, struct Compiler *C, struct Mir *mir, void *ud)
{
    *V = (struct MirVisitor){
        .mir = mir,
        .ud = ud,
        .C = C,

        .VisitBlock = default_visit_block,
        .VisitInstruction = default_visit_instruction,
        .VisitPlace = default_visit_place,

        .PostVisitBlock = default_post_visit_block,
        .PostVisitInstruction = default_post_visit_instruction,
        .PostVisitPlace = default_post_visit_place,
    };
}

void mir_visit(struct MirVisitor *V)
{
    struct Mir *mir = V->mir;
    for (int i = 0; i < mir->blocks->count; ++i) {
        // callee will look up the block data struct using the provided block ID
        mir_visit_block(V, MIR_BB(i));
    }
}

#define DEFINE_LIST_VISITOR(name, T) \
    void mir_visit_##name##_list(struct MirVisitor *V, struct Mir##T##List *list) \
    { \
        if (list == NULL) \
            return; \
        for (int i = 0; i < list->count; ++i) { \
            mir_visit_##name(V, Mir##T##List_get(list, i)); \
        } \
    }
DEFINE_LIST_VISITOR(block, Block)
DEFINE_LIST_VISITOR(instruction, Instruction)
DEFINE_LIST_VISITOR(place, Place)
#undef DEFINE_LIST_VISITOR

struct Successors {
    struct MirBlockList *successors;
    MirBlock block;
};

struct Traversal {
    struct Allocator a;
    struct Pool *pool;
    struct Compiler *C;
    struct Mir *mir;
    struct VisitedMap *visited;
    paw_Env *P;
};

DEFINE_LIST(VisitStack, struct Successors)
DEFINE_MAP(VisitedMap, P_ID_HASH, P_ID_EQUALS, MirBlock, void *)
DEFINE_MAP(BlockMap, P_ID_HASH, P_ID_EQUALS, MirBlock, MirBlock)

static paw_Bool check_visited(struct Traversal *X, VisitedMap *visited, MirBlock bb)
{
    if (VisitedMap_get(visited, bb) != NULL)
        return PAW_TRUE;
    VisitedMap_insert(visited, bb, NULL);
    return PAW_FALSE;
}

static void reverse_blocks(struct MirBlockList *blocks)
{
    paw_assert(blocks->count > 0);
    MirBlock *a = &LIST_FIRST(blocks);
    MirBlock *b = &LIST_LAST(blocks);
    for (; a < b; a++, b--) {
        MirBlock const t = *a;
        *a = *b;
        *b = t;
    }
}

static struct MirBlockList *copy_blocks(struct Mir *mir, struct MirBlockList *blocks)
{
    MirBlock *b;
    struct MirBlockList *result = MirBlockList_new(ALLOCATOR_OF(mir));
    MirBlockList_reserve(result, blocks->count);
    LIST_FOREACH (blocks, b) MirBlockList_push(result, *b);
    return result;
}

static void visit_postorder(struct Traversal *X, VisitedMap *visited, struct VisitStack *stack, MirBlock bb)
{
    if (check_visited(X, visited, bb)) return;
    struct MirBlockData const *data = mir_bb_data(X->mir, bb);
    VisitStack_push(stack, (struct Successors){
        .successors = copy_blocks(X->mir, data->successors),
        .block = bb,
    });
}

static void traverse_postorder(struct Traversal *X, VisitedMap *visited, struct VisitStack *stack)
{
    while (stack->count > 0) {
        struct Successors *top = &LIST_LAST(stack);
        if (top->successors->count == 0) break;
        MirBlock const bb = LIST_LAST(top->successors);
        MirBlockList_pop(top->successors);
        visit_postorder(X, visited, stack, bb);
    }
}

struct MirBlockList *mir_traverse_rpo(struct Compiler *C, struct Mir *mir)
{
    struct Pool *pool = compiler_new_pool(C, C->aux_stats);
    struct Traversal X = {
        .a = POOL_ALLOCATOR(pool),
        .pool = pool,
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    struct VisitStack *stack = VisitStack_new(ALLOCATOR_OF(&X));
    X.visited = VisitedMap_new(ALLOCATOR_OF(&X));
    paw_assert(mir->blocks->count > 0);
    visit_postorder(&X, X.visited, stack, MIR_ENTRY_BB);
    traverse_postorder(&X, X.visited, stack);

    struct MirBlockList *order = MirBlockList_new(ALLOCATOR_OF(mir));
    while (stack->count > 0) {
        struct Successors s = LIST_LAST(stack);
        VisitStack_pop(stack);
        traverse_postorder(&X, X.visited, stack);
        MirBlockList_push(order, s.block);
    }
    reverse_blocks(order);
    compiler_free_pool(C, X.pool);
    return order;
}

static void renumber_ref(struct Traversal *X, BlockMap *map, MirBlock *pfrom)
{
    paw_assert(MIR_ID_EXISTS(*pfrom));
    *pfrom = *BlockMap_get(map, *pfrom);
}

static void renumber_or_clear_ref(struct Traversal *X, BlockMap *map, MirBlock *pfrom)
{
    paw_assert(MIR_ID_EXISTS(*pfrom));
    MirBlock const *pto = BlockMap_get(map, *pfrom);
    if (pto != NULL) {
        *pfrom = *pto;
    } else {
        *pfrom = MIR_INVALID_BB;
    }
}

static void rename_and_filter(struct Traversal *X, BlockMap *map, struct MirBlockList *blocks, struct MirBlockData *bb)
{
    int index;
    int removed = 0;
    MirBlock const *pfrom;
    LIST_ENUMERATE (blocks, index, pfrom) {
        MirBlock const *pto = BlockMap_get(map, *pfrom);
        if (pto == NULL) {
            ++removed;
            continue;
        }
        MirBlockList_set(blocks, index - removed, *pto);
    }
    blocks->count -= removed;
}

// Renumber basic blocks so that "mir_bb_data" continues to work after a call
// to mir_remove_unreachable_blocks
static void renumber_block_refs(struct Traversal *X, BlockMap *map, struct MirBlockData *data)
{
    rename_and_filter(X, map, data->predecessors, data);
    rename_and_filter(X, map, data->successors, NULL);
}

void mir_renumber_basic_blocks(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct Pool *pool = compiler_new_pool(C, C->aux_stats);
    struct Traversal X = {
        .a = POOL_ALLOCATOR(pool),
        .pool = pool,
        .mir = mir,
        .P = ENV(C),
        .C = C,
    };
    // create a mapping from old to new basic block numbers
    BlockMap *map = BlockMap_new(ALLOCATOR_OF(&X));
    struct MirBlockList *order = mir_traverse_rpo(C, mir);

    int index;
    MirBlock const *pb;
    LIST_ENUMERATE (order, index, pb) {
        BlockMap_insert(map, *pb, MIR_BB(index));
    }

    struct MirBlockDataList *blocks = MirBlockDataList_new(ALLOCATOR_OF(mir));
    MirBlockDataList_reserve(blocks, order->count);
    LIST_FOREACH (order, pb) {
        struct MirBlockData *bb = mir_bb_data(mir, *pb);
        renumber_block_refs(&X, map, bb);
        MirBlockDataList_push(blocks, bb);
    }

    mir->blocks = blocks;
    compiler_free_pool(C, X.pool);
}

MirPlacePtrList *mir_get_loads(struct Mir *mir, struct MirInstruction *instr)
{
#define ADD_INPUT(x) MirPlacePtrList_push(inputs, &(x))
#define ADD_INPUTS(xs) \
    LIST_FOREACH (xs, pp) \
        ADD_INPUT(*pp)

    struct MirPlace *pp;
    struct MirPlacePtrList *inputs = MirPlacePtrList_new(ALLOCATOR_OF(mir));

    switch (MIR_KINDOF(instr)) {
        case kMirMove: {
            struct MirMove *x = MirGetMove(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirLoad: {
            struct MirLoad *x = MirGetLoad(instr);
            ADD_INPUT(x->pointer);
            break;
        }
        case kMirStore: {
            struct MirStore *x = MirGetStore(instr);
            ADD_INPUT(x->pointer);
            ADD_INPUT(x->value);
            break;
        }
        case kMirAddrOf: {
            struct MirAddrOf *x = MirGetAddrOf(instr);
            ADD_INPUT(x->input);
            break;
        }
        case kMirAggregate: {
            struct MirAggregate *x = MirGetAggregate(instr);
            ADD_INPUTS(x->fields);
            break;
        }
        case kMirKill: {
            struct MirKill *x = MirGetKill(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirDrop: {
            struct MirDrop *x = MirGetDrop(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirCall: {
            struct MirCall *x = MirGetCall(instr);
            ADD_INPUT(x->target);
            ADD_INPUTS(x->args);
            break;
        }
        case kMirCast: {
            struct MirCast *x = MirGetCast(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirArrayGEP: {
            struct MirArrayGEP *x = MirGetArrayGEP(instr);
            ADD_INPUT(x->array);
            ADD_INPUT(x->index);
            break;
        }
        case kMirStructGEP: {
            struct MirStructGEP *x = MirGetStructGEP(instr);
            ADD_INPUT(x->object);
            break;
        }
        case kMirGetDiscriminant: {
            struct MirGetDiscriminant *x = MirGetGetDiscriminant(instr);
            ADD_INPUT(x->object);
            break;
        }
        case kMirGetRange: {
            struct MirGetRange *x = MirGetGetRange(instr);
            ADD_INPUT(x->object);
            ADD_INPUT(x->lower);
            ADD_INPUT(x->upper);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *x = MirGetUnaryOp(instr);
            ADD_INPUT(x->val);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *x = MirGetBinaryOp(instr);
            ADD_INPUT(x->lhs);
            ADD_INPUT(x->rhs);
            break;
        }
        case kMirArray: {
            struct MirArray *x = MirGetArray(instr);
            ADD_INPUTS(x->elems);
            break;
        }
        case kMirSetRange: {
            struct MirSetRange *x = MirGetSetRange(instr);
            ADD_INPUT(x->object);
            ADD_INPUT(x->lower);
            ADD_INPUT(x->upper);
            ADD_INPUT(x->value);
            break;
        }
        case kMirCapture: {
            struct MirCapture *x = MirGetCapture(instr);
            ADD_INPUT(x->target);
            break;
        }
        case kMirBranch: {
            struct MirBranch *x = MirGetBranch(instr);
            ADD_INPUT(x->cond);
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *x = MirGetSwitch(instr);
            ADD_INPUT(x->discr);
            break;
        }
        case kMirNoop:
        case kMirGetConstant:
        case kMirGlobal:
        case kMirAllocLocal:
        case kMirClosure:
        case kMirReturn:
        case kMirGoto:
        case kMirUnreachable:
            break;
    }
    return inputs;

#undef ADD_INPUTS
#undef ADD_INPUT
}

MirPlacePtrList *mir_get_stores(struct Mir *mir, struct MirInstruction *instr)
{
#define ADD_OUTPUT(x) MirPlacePtrList_push(outputs, &(x))
#define ADD_OUTPUTS(xs) \
    LIST_FOREACH (xs, pp) \
        ADD_OUTPUT(*pp)

    struct MirPlacePtrList *outputs = MirPlacePtrList_new(ALLOCATOR_OF(mir));

    switch (MIR_KINDOF(instr)) {
        case kMirMove:
            ADD_OUTPUT(MirGetMove(instr)->output);
            break;
        case kMirLoad:
            ADD_OUTPUT(MirGetLoad(instr)->output);
            break;
        case kMirAddrOf:
            ADD_OUTPUT(MirGetAddrOf(instr)->output);
            break;
        case kMirGlobal:
            ADD_OUTPUT(MirGetGlobal(instr)->output);
            break;
        case kMirAllocLocal:
            // TODO: this should not cause `output` to be defined since this hides use before init errors... in fact, there is not much need for AllocLocal anymore... (was used to allocate a heap slot for captured vars)
//            ADD_OUTPUT(MirGetAllocLocal(instr)->output);
            break;
        case kMirAggregate:
            ADD_OUTPUT(MirGetAggregate(instr)->output);
            break;
        case kMirArray:
            ADD_OUTPUT(MirGetArray(instr)->output);
            break;
        case kMirCall:
            ADD_OUTPUT(MirGetCall(instr)->output);
            break;
        case kMirCast:
            ADD_OUTPUT(MirGetCast(instr)->output);
            break;
        case kMirClosure:
            ADD_OUTPUT(MirGetClosure(instr)->output);
            break;
        case kMirStructGEP:
            ADD_OUTPUT(MirGetStructGEP(instr)->output);
            break;
        case kMirArrayGEP:
            ADD_OUTPUT(MirGetArrayGEP(instr)->output);
            break;
        case kMirGetConstant:
            ADD_OUTPUT(MirGetGetConstant(instr)->output);
            break;
        case kMirGetDiscriminant:
            ADD_OUTPUT(MirGetGetDiscriminant(instr)->output);
            break;
        case kMirGetRange:
            ADD_OUTPUT(MirGetGetRange(instr)->output);
            break;
        case kMirSetRange:
            ADD_OUTPUT(MirGetSetRange(instr)->object);
            break;
        case kMirUnaryOp:
            ADD_OUTPUT(MirGetUnaryOp(instr)->output);
            break;
        case kMirBinaryOp:
            ADD_OUTPUT(MirGetBinaryOp(instr)->output);
            break;
        case kMirNoop:
        case kMirReturn:
        case kMirGoto:
        case kMirUnreachable:
        case kMirCapture:
        case kMirDrop:
        case kMirKill:
        case kMirStore:
        case kMirBranch:
        case kMirSwitch:
            break;
    }
    return outputs;

#undef ADD_OUTPUTS
#undef ADD_OUTPUT
}

static void indicate_access(struct Mir *mir, AccessMap *map, struct MirInstruction *instr, MirRegister r, MirBlock where)
{
    struct MirAccessList *accesses = *AccessMap_get(map, r);
    MirAccessList_push(accesses, (struct MirAccess){
        .instr = instr,
        .b = where,
    });
}

static void account_for_uses(struct Mir *mir, struct MirInstruction *instr, AccessMap *uses, MirBlock where)
{
    struct MirPlace *const *ppp;
    struct MirPlacePtrList const *loads = mir_get_loads(mir, instr);
    LIST_FOREACH (loads, ppp) {
        if ((*ppp)->kind == MIR_PLACE_REGISTER)
            indicate_access(mir, uses, instr, (*ppp)->r, where);
    }
}

static void account_for_defs(struct Mir *mir, struct MirInstruction *instr, AccessMap *defs, MirBlock where)
{
    struct MirPlace *const *ppp;
    MirPlacePtrList const *stores = mir_get_stores(mir, instr);
    LIST_FOREACH (stores, ppp) {
        if ((*ppp)->kind == MIR_PLACE_REGISTER)
            indicate_access(mir, defs, instr, (*ppp)->r, where);
    }
}

typedef void (*AccountForAccesses)(struct Mir *, struct MirInstruction *, AccessMap *, MirBlock);

static void collect_accesses(struct Mir *mir, AccessMap *map, AccountForAccesses cb)
{
    struct Compiler *C = mir->C;

    int index;
    struct MirRegisterData *pdata;
    LIST_ENUMERATE (mir->registers, index, pdata) {
        AccessMap_insert(map, MIR_REG(index), MirAccessList_new(ALLOCATOR_OF(C)));
    }

    struct MirBlockData *const *pblock;
    LIST_ENUMERATE (mir->blocks, index, pblock) {
        struct MirBlockData const *block = *pblock;
        MirBlock const b = MIR_BB(index);

        struct MirInstruction *const *pinstr;
        LIST_FOREACH (block->instructions, pinstr)
            cb(mir, *pinstr, map, b);
    }
}

void mir_collect_per_instr_uses(struct Mir *mir, AccessMap *uses)
{
    collect_accesses(mir, uses, account_for_uses);
}

void mir_collect_per_instr_defs(struct Mir *mir, AccessMap *defs)
{
    collect_accesses(mir, defs, account_for_defs);
}

static void indicate_usedef(struct Mir *mir, UseDefMap *map, MirRegister r, MirBlock where)
{
    struct MirBlockList *blocks = *UseDefMap_get(map, r);

    MirBlock const *pb;
    LIST_FOREACH (blocks, pb) {
        if (MIR_ID_EQUALS(*pb, where))
            return;
    }
    MirBlockList_push(blocks, where);
}

static void indicate_usedefs(struct Mir *mir, struct MirInstruction *instr, UseDefMap *uses, UseDefMap *defs, MirBlock where)
{
    MirPlacePtrList const *loads = mir_get_loads(mir, instr);
    LIST_XFOREACH (loads, struct MirPlace *const, ppp) {
        if ((*ppp)->kind == MIR_PLACE_REGISTER)
            indicate_usedef(mir, uses, (*ppp)->r, where);
    }
    MirPlacePtrList const *stores = mir_get_stores(mir, instr);
    LIST_XFOREACH (stores, struct MirPlace *const, ppp) {
        if ((*ppp)->kind == MIR_PLACE_REGISTER)
            indicate_usedef(mir, defs, (*ppp)->r, where);
    }
}

void mir_collect_per_block_usedefs(struct Mir *mir, UseDefMap *uses, UseDefMap *defs)
{
    {
        int index = 0;
        LIST_XFOREACH (mir->registers, struct MirRegisterData const, p) {
            UseDefMap_insert(uses, MIR_REG(index), MirBlockList_new(ALLOCATOR_OF(mir)));
            UseDefMap_insert(defs, MIR_REG(index), MirBlockList_new(ALLOCATOR_OF(mir)));
            ++index;
        }
    }

    {
        int index = 0;
        LIST_XFOREACH (mir->blocks, struct MirBlockData *const, pblock) {
            struct MirBlockData const *block = *pblock;
            MirBlock const b = MIR_BB(index++);

            struct MirInstruction **pinstr;
            LIST_FOREACH (block->instructions, pinstr)
                indicate_usedefs(mir, *pinstr, uses, defs, b);
        }
    }
}

void mir_set_location(struct Mir *mir, struct MirLocationList *locations, MirId mid, int location)
{
    while (mid.value >= locations->count) {
        MirLocationList_push(locations, -1);
    }
    MirLocationList_set(locations, mid.value, location);
}

struct MirLocationList *mir_compute_locations(struct Mir *mir)
{
    int location = 0;
    struct MirLocationList *locations = MirLocationList_new(ALLOCATOR_OF(mir));

    struct MirBlockData **pblock;
    struct MirInstruction **pinstr;
    LIST_FOREACH (mir->blocks, pblock) {
        struct MirBlockData *block = *pblock;
        mir_set_location(mir, locations, block->mid, location);

        // each instruction bumps the location by 2 to allow insertion of new instructions
        // without breaking the ordering
        LIST_FOREACH (block->instructions, pinstr) {
            struct MirInstruction *instr = *pinstr;
            location += 2;
            mir_set_location(mir, locations, instr->hdr.mid, location);
        }
        location += 2;
    }
    return locations;
}

DEFINE_MAP(BlockCounts, P_ID_HASH, P_ID_EQUALS, MirBlock, int)

static MirBlock get_common_block(MirBlockList *blocks)
{
    MirBlock const first = LIST_FIRST(blocks);
    for (int i = 0; i < blocks->count; ++i) {
        if (!MIR_ID_EQUALS(first, MirBlockList_get(blocks, i)))
           return MIR_INVALID_BB;
    }
    return first;
}

static void detach_unused_block(struct MirBlockData *bb)
{
    paw_assert(bb->instructions->count > 0);
    LIST_FIRST(bb->instructions) = LIST_LAST(bb->instructions);
    LIST_FIRST(bb->instructions)->hdr.kind = kMirUnreachable;
    bb->instructions->count = 1;
    bb->predecessors->count = 0;
    bb->successors->count = 0;
}

static void merge_adjacent_blocks(struct Mir *mir, MirBlock bfrom, struct MirBlockData *from, MirBlock bto, struct MirBlockData *to)
{
    struct MirInstruction *unused_term = LIST_LAST(from->instructions);
    MirBlockList *unused_succ = from->successors;

    // remove goto and merge instruction lists
    --from->instructions->count;
    struct MirInstruction *const *pinstr;
    LIST_FOREACH (to->instructions, pinstr)
        MirInstructionList_push(from->instructions, *pinstr);

    // fix back references of removed block's successors
    MirBlock *pp;
    MirBlock const *ps;
    LIST_FOREACH (to->successors, ps) {
        struct MirBlockData const *s = mir_bb_data(mir, *ps);
        LIST_FOREACH (s->predecessors, pp) {
            if (MIR_ID_EQUALS(*pp, bto)) *pp = bfrom;
        }
    }
    from->successors = to->successors;
    to->successors = unused_succ;

    // detach the unused basic block, keeping it valid and being careful to avoid
    // modifying components transferred to "from"
    LIST_LAST(to->instructions) = unused_term;
    detach_unused_block(to);
}

static void thread_jump_through(struct Mir *mir, MirBlock bfrom, struct MirBlockData *from, MirBlock bto, struct MirBlockData *to)
{
    int const target = mir_which_pred(mir, bto, bfrom);

    {
        // modify references to "bfrom" so they point to "bto"
        MirBlock const *pp;
        LIST_FOREACH (from->predecessors, pp) {
            struct MirBlockData const *pred = mir_bb_data(mir, *pp);
            int const index = mir_which_succ(mir, *pp, bfrom);
            MirBlockList_set(pred->successors, index, bto);
        }
    }

    {
        // rewrite predecessors of successor
        int index;
        MirBlock const *ppred, *ppred2;
        MirBlockList *predecessors = MirBlockList_new(ALLOCATOR_OF(mir));
        LIST_ENUMERATE (to->predecessors, index, ppred) {
            if (index == target) {
                LIST_FOREACH (from->predecessors, ppred2)
                    MirBlockList_push(predecessors, *ppred2);
            } else {
                MirBlockList_push(predecessors, *ppred);
            }
        }
        to->predecessors = predecessors;
    }

    detach_unused_block(from);
    MIR_VALIDATE_GRAPH(mir);
}

void mir_merge_redundant_blocks(struct Mir *mir)
{
    return; // TODO maybe missing transfer of phi nodes from removed block to other block

    paw_Bool changed;
    do {
        changed = PAW_FALSE;
        MirBlockList *order = mir_traverse_rpo(mir->C, mir);
        for (int i = order->count - 1; i > 0; --i) {
            MirBlock const p = MirBlockList_get(order, i);
            struct MirBlockData *pred = mir_bb_data(mir, p);
            if (pred->predecessors->count == 0) continue; // disconnected
#define CHECK_TERMINATOR(Kind_) \
        MirIs##Kind_(MirInstructionList_last(pred->instructions))

            if (CHECK_TERMINATOR(Branch) || CHECK_TERMINATOR(Switch)) {
                // Attempt to convert a branch into a jump. This is possible when all cases
                // jump to the same basic block.
                MirBlock const common = get_common_block(pred->successors);
                if (MIR_ID_EXISTS(common)) {
                    struct MirBlockData *succ = mir_bb_data(mir, common);
                    LIST_LAST(pred->instructions)->hdr.kind = kMirGoto;
                    LIST_FIRST(pred->successors) = common;
                    pred->successors->count = 1;
                    changed = PAW_TRUE;
                    // remove redundant copies of "common"
                    int count = 0;
                    MirBlock const *pp;
                    MirBlockList *predecessors = MirBlockList_new(ALLOCATOR_OF(mir));
                    LIST_FOREACH (succ->predecessors, pp) {
                        if (!MIR_ID_EQUALS(p, *pp) || count++ == 0)
                            MirBlockList_push(predecessors, *pp);
                    }
                    MirBlockList_delete(succ->predecessors);
                    succ->predecessors = predecessors;
                }
            }

            if (CHECK_TERMINATOR(Goto)) {
                MirBlock const s = LIST_LAST(pred->successors);
                struct MirBlockData *succ = mir_bb_data(mir, s);
                if (pred->instructions->count == 1) {
                   // predecessor is empty except for the jump
                   thread_jump_through(mir, p, pred, s, succ);
                   changed = PAW_TRUE;
                }
            }

            if (CHECK_TERMINATOR(Goto)) {
                MirBlock const s = LIST_LAST(pred->successors);
                struct MirBlockData *succ = mir_bb_data(mir, s);
                if (succ->predecessors->count == 1) {
                   // "p" and "s" can be trivially merged
                   merge_adjacent_blocks(mir, p, pred, s, succ);
                   changed = PAW_TRUE;
                }
            }

            if (CHECK_TERMINATOR(Goto)) {
                MirBlock const s = LIST_LAST(pred->successors);
                struct MirBlockData *succ = mir_bb_data(mir, s);
                if (succ->instructions->count == 1 && succ->successors->count > 1) {
                    // overwrite jump with other terminator
                    LIST_LAST(pred->instructions) = LIST_LAST(succ->instructions);
                    pred->successors = copy_blocks(mir, succ->successors);
                    int const n = mir_which_pred(mir, s, p);
                    MirBlockList_remove(succ->predecessors, n);
                    changed = PAW_TRUE;
                }
            }

#undef CHECK_TERMINATOR
        }
    } while (changed);

    mir_renumber_basic_blocks(mir);
    MIR_VALIDATE_GRAPH(mir);
}


struct Printer {
    struct Compiler *C;
    struct MirBlockData *bb;
    struct Mir *mir;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define PRINT_LITERAL(P_, Lit_) L_ADD_LITERAL(ENV(P_), (P_)->buf, Lit_)
#define PRINT_STRING(P_, Str_) lib_add_nstring(ENV(P_), (P_)->buf, (Str_)->text, (Str_)->length)
#define PRINT_FORMAT(P_, ...) lib_add_fstring(ENV(P_), (P_)->buf, __VA_ARGS__)
#define PRINT_CHAR(P_, Char_) lib_add_char(ENV(P_), (P_)->buf, Char_)

static void dump_instruction(struct Printer *, struct MirInstruction *);

static void indentation(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        PRINT_LITERAL(P, "  ");
    }
}

#define DUMP_FORMAT(P_, ...) (indentation(P_), PRINT_FORMAT(P_, __VA_ARGS__))
#define DUMP_LITERAL(P_, Lit_) (indentation(P_), PRINT_LITERAL(P_, Lit_))

static char const *unop_name(enum MirUnaryOpKind op)
{
    switch (op) {
        case MIR_UNARY_NEG:
            return "NEG";
        case MIR_UNARY_NOT:
            return "NOT";
        case MIR_UNARY_BITNOT:
            return "BITNOT";
    }
}

static char const *binop_name(enum MirBinaryOpKind op)
{
    switch (op) {
        case MIR_BINARY_EQ:
            return "EQ";
        case MIR_BINARY_NE:
            return "NE";
        case MIR_BINARY_LT:
            return "LT";
        case MIR_BINARY_LE:
            return "LE";
        case MIR_BINARY_ADD:
            return "ADD";
        case MIR_BINARY_SUB:
            return "SUB";
        case MIR_BINARY_MUL:
            return "MUL";
        case MIR_BINARY_DIV:
            return "DIV";
        case MIR_BINARY_MOD:
            return "MOD";
        case MIR_BINARY_BITAND:
            return "BITAND";
        case MIR_BINARY_BITOR:
            return "BITOR";
        case MIR_BINARY_BITXOR:
            return "BITXOR";
        case MIR_BINARY_SHL:
            return "SHL";
        case MIR_BINARY_SHR:
            return "SHR";
    }
}

static void print_constant(struct Printer *P, IrConst *konst)
{
    Str const *s = ir_print_const(P->C, konst);
    PRINT_STRING(P, s);
}

static void print_place(struct Printer *P, struct MirPlace place)
{
    if (place.kind == MIR_PLACE_REGISTER) {
        if (place.r.value < 0) {
            PRINT_LITERAL(P, "%?");
            return;
        }
        struct MirRegisterData const *data = mir_reg_data(P->mir, place.r);
        if (data->name != NULL) {
            PRINT_FORMAT(P, "%s", data->name->text);
        } else {
            PRINT_FORMAT(P, "%%%d", place.r.value);
        }
    } else if (place.kind == MIR_PLACE_CONSTANT) {
        struct MirConstantData const *k = mir_const_data(P->mir, place.k);
        print_constant(P, k->data);
    } else {
        PRINT_FORMAT(P, "up%d", place.up);
    }
}

static void print_place_list(struct Printer *P, MirPlaceList *places)
{
    int index;
    struct MirPlace *p;
    LIST_ENUMERATE (places, index, p) {
        if (index > 0)
            PRINT_LITERAL(P, ", ");
        print_place(P, *p);
    }
}

static void dump_instruction_list(struct Printer *P, struct MirInstructionList *list)
{
    for (int i = 0; i < list->count; ++i) {
        dump_instruction(P, list->data[i]);
    }
}

static void print_constant(struct Printer *P, IrConst *konst);

static void dump_instruction(struct Printer *P, struct MirInstruction *instr)
{
    indentation(P);

    switch (MIR_KINDOF(instr)) {
        case kMirAllocLocal: {
            struct MirAllocLocal *t = MirGetAllocLocal(instr);
            IrType *output_type = mir_place_type(P->mir, t->output);
            PRINT_LITERAL(P, "alloc ");
            print_place(P, t->output);
            PRINT_FORMAT(P, " (\"%s\")", ir_print_type(P->C, output_type));
            break;
        }
        case kMirNoop: {
            PRINT_LITERAL(P, "noop");
            break;
        }
        case kMirAddrOf: {
            struct MirAddrOf *t = MirGetAddrOf(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = &");
            print_place(P, t->input);
            break;
        }
        case kMirMove: {
            struct MirMove *t = MirGetMove(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = move ");
            print_place(P, t->target);
            break;
        }
        case kMirLoad: {
            struct MirLoad *t = MirGetLoad(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = ");
            PRINT_CHAR(P, '*');
            print_place(P, t->pointer);
            break;
        }
        case kMirStore: {
            struct MirStore *t = MirGetStore(instr);
            PRINT_CHAR(P, '*');
            print_place(P, t->pointer);
            PRINT_LITERAL(P, " = ");
            print_place(P, t->value);
            break;
        }
        case kMirGlobal: {
            struct MirGlobal *t = MirGetGlobal(instr);
            IrType *output_type = mir_place_type(P->mir, t->output);
            char const *type = ir_print_type(P->C, output_type);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = global ");
            DeclId const did = IR_TYPE_DID(output_type);
            struct IrFnDef const *fn_def = ir_get_fn_def(P->C, did);
            PRINT_STRING(P, fn_def->name);
            PRINT_FORMAT(P, " (%s)", type);
            break;
        }
        case kMirArray: {
            struct MirArray *t = MirGetArray(instr);
            print_place(P, t->output);
            PRINT_FORMAT(P, " = [");
            print_place_list(P, t->elems);
            PRINT_CHAR(P, ']');
            break;
        }
        case kMirAggregate: {
            struct MirAggregate *t = MirGetAggregate(instr);
            PRINT_LITERAL(P, "aggregate ");
            print_place(P, t->output);
            PRINT_LITERAL(P, " = [");
            print_place_list(P, t->fields);
            PRINT_CHAR(P, ']');
            break;
        }
        case kMirKill: {
            struct MirKill *t = MirGetKill(instr);
            PRINT_LITERAL(P, "kill ");
            print_place(P, t->target);
            break;
        }
        case kMirDrop: {
            struct MirDrop *t = MirGetDrop(instr);
            PRINT_LITERAL(P, "drop ");
            print_place(P, t->target);
            break;
        }
        case kMirCall: {
            struct MirCall *t = MirGetCall(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = ");
            print_place(P, t->target);
            PRINT_CHAR(P, '(');
            print_place_list(P, t->args);
            PRINT_LITERAL(P, ")");
            break;
        }
        case kMirCast: {
            struct MirCast *t = MirGetCast(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = (");
            IrType *to_type = mir_place_type(P->mir, t->output);
            PRINT_STRING(P, ir_print_type_v2(P->C, to_type));
            PRINT_CHAR(P, ')');
            print_place(P, t->target);
            break;
        }
        case kMirCapture: {
            struct MirCapture *t = MirGetCapture(instr);
            PRINT_LITERAL(P, "capture ");
            print_place(P, t->target);
            break;
        }
        case kMirClosure: {
            struct MirClosure *t = MirGetClosure(instr);
            PRINT_LITERAL(P, "closure ");
            print_place(P, t->output);
            break;
        }
        case kMirStructGEP: {
            struct MirStructGEP *t = MirGetStructGEP(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = &");
            print_place(P, t->object);
            PRINT_FORMAT(P, ".%d", t->field);
            break;
        }
        case kMirArrayGEP: {
            struct MirArrayGEP *t = MirGetArrayGEP(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = &");
            print_place(P, t->array);
            PRINT_LITERAL(P, "[");
            print_place(P, t->index);
            PRINT_LITERAL(P, "]");
            break;
        }
        case kMirGetConstant: {
            struct MirGetConstant *t = MirGetGetConstant(instr);
            print_place(P, t->output);
            PRINT_FORMAT(P, " = getk %d", t->k.value);
            break;
        }
        case kMirGetDiscriminant: {
            struct MirGetDiscriminant *t = MirGetGetDiscriminant(instr);
            print_place(P, t->output);
            PRINT_LITERAL(P, " = getdiscr");
            print_place(P, t->object);
            break;
        }
        case kMirGetRange: {
            struct MirGetRange *t = MirGetGetRange(instr);
            PRINT_LITERAL(P, "get ");
            print_place(P, t->output);
            PRINT_LITERAL(P, " = ");
            print_place(P, t->object);
            PRINT_CHAR(P, '[');
            print_place(P, t->lower);
            PRINT_CHAR(P, ':');
            print_place(P, t->upper);
            PRINT_CHAR(P, ']');
            break;
        }
        case kMirSetRange: {
            struct MirSetRange *t = MirGetSetRange(instr);
            PRINT_LITERAL(P, "set ");
            print_place(P, t->object);
            PRINT_CHAR(P, '[');
            print_place(P, t->lower);
            PRINT_CHAR(P, ':');
            print_place(P, t->upper);
            PRINT_LITERAL(P, "] = ");
            print_place(P, t->value);
            break;
        }
        case kMirUnaryOp: {
            struct MirUnaryOp *t = MirGetUnaryOp(instr);
            print_place(P, t->output);
            PRINT_FORMAT(P, " = %s ", unop_name(t->op));
            print_place(P, t->val);
            break;
        }
        case kMirBinaryOp: {
            struct MirBinaryOp *t = MirGetBinaryOp(instr);
            print_place(P, t->output);
            PRINT_FORMAT(P, " = %s ", binop_name(t->op));
            print_place(P, t->lhs);
            PRINT_CHAR(P, ' ');
            print_place(P, t->rhs);
            break;
        }
        case kMirUnreachable:
            PRINT_LITERAL(P, "unreachable");
            break;
        case kMirReturn:
            PRINT_LITERAL(P, "return");
            break;
        case kMirBranch: {
            struct MirBranch *t = MirGetBranch(instr);
            PRINT_LITERAL(P, "branch ");
            print_place(P, t->cond);
            PRINT_FORMAT(P, " => [true: bb%d, false: bb%d]", MirBlockList_get(P->bb->successors, 0),
                    MirBlockList_get(P->bb->successors, 1));
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *t = MirGetSwitch(instr);
            PRINT_LITERAL(P, "switch ");
            print_place(P, t->discr);
            PRINT_LITERAL(P, " => [");
            for (int i = 0; i < t->arms->count; ++i) {
                if (i > 0) PRINT_LITERAL(P, ", ");
                struct MirSwitchArm arm = MirSwitchArmList_get(t->arms, i);
                struct MirConstantData const *kdata = mir_const_data(P->mir, arm.k);
                print_constant(P, kdata->data);
                PRINT_FORMAT(P, ": bb%d", MirBlockList_get(P->bb->successors, i).value);
            }
            if (t->has_otherwise) {
                if (t->arms->count > 0)
                    PRINT_LITERAL(P, ", ");
                PRINT_FORMAT(P, "_: bb%d", LIST_LAST(P->bb->successors).value);
            }
            PRINT_LITERAL(P, "]");
            break;
        }
        case kMirGoto: {
            PRINT_FORMAT(P, "goto bb%d", LIST_FIRST(P->bb->successors).value);
            break;
        }
    }
    PRINT_CHAR(P, '\n');
}

static void dump_block(struct Printer *P, MirBlock bb)
{
    struct MirBlockData *block = mir_bb_data(P->mir, bb);
    {
        indentation(P);
        PRINT_FORMAT(P, "bb%d: // preds = [", bb.value);
        int index = 0;
        LIST_XFOREACH (mir_bb_data(P->mir, bb)->predecessors, MirBlock const, p) {
            if (index++ != 0) PRINT_LITERAL(P, ", ");
            PRINT_FORMAT(P, "bb%d", p->value);
        }
        PRINT_LITERAL(P, "]\n");
    }
    ++P->indent;

    P->bb = block;
    dump_instruction_list(P, block->instructions);

    --P->indent;
}

static void dump_mir(struct Printer *P, struct Mir *mir)
{
    DUMP_FORMAT(P, "%sfn %s:\n", mir->is_pub ? "pub " : "", mir->name->text);
    ++P->indent;

    for (int i = 0; i < mir->blocks->count; ++i) {
        dump_block(P, MIR_BB(i));
    }

    DUMP_FORMAT(P, "registers {\n");
    ++P->indent;
    for (int i = 0; i < mir->registers->count; ++i) {
        struct MirRegisterData const data = MirRegisterDataList_get(mir->registers, i);
        Str const *type = ir_print_type_v2(P->C, data.type);
        if (data.name != NULL) {
            DUMP_FORMAT(P, "%s: %s\n", data.name->text, type->text);
        } else {
            DUMP_FORMAT(P, "_%d: %s\n", i, type->text);
        }
    }
    --P->indent;
    DUMP_FORMAT(P, "}\n");

    DUMP_FORMAT(P, "upvalues {\n");
    ++P->indent;
    for (int i = 0; i < mir->upvalues->count; ++i) {
        struct MirUpvalueInfo up = MirUpvalueList_get(mir->upvalues, i);
        DUMP_FORMAT(P, "up%d (%s %d): %s\n", i, up.is_local ? "local" : "nonlocal", up.index, ir_print_type(P->C, up.type));
    }
    --P->indent;
    DUMP_FORMAT(P, "}\n");

    DUMP_FORMAT(P, "captured {\n");
    ++P->indent;
    for (int i = 0; i < mir->captured->count; ++i) {
        struct MirCaptureInfo ci = MirCaptureList_get(mir->captured, i);
        DUMP_FORMAT(P, "%d: _%d\n", i, ci.local.value);
    }
    --P->indent;
    DUMP_FORMAT(P, "}\n");

    --P->indent;
    DUMP_LITERAL(P, "}\n");
}

char const *mir_dump(struct Mir *mir)
{
    struct Compiler *C = mir->C;

    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    dump_mir(&(struct Printer){
                 .P = ENV(C),
                 .mir = mir,
                 .buf = &buf,
                 .C = C,
             },
             mir);

    Str const *result = lib_buffer_finish(P, &buf);
    return result->text;
}

static void dump_block_list(struct Printer *P, struct MirBlockList *blocks)
{
    for (int i = 0; i < blocks->count; ++i) {
        if (i > 0) PRINT_LITERAL(P, ", ");
        PRINT_FORMAT(P, "bb%d", MirBlockList_get(blocks, i).value);
    }
}

static void dump_graph(struct Printer *P, struct Mir *mir)
{
    DUMP_LITERAL(P, "Mir {\n");
    ++P->indent;

    for (int i = 0; i < mir->blocks->count; ++i) {
        struct MirBlockData const *data = mir_bb_data(mir, MIR_BB(i));
        DUMP_FORMAT(P, "bb%d: {\n", i);
        ++P->indent;
        DUMP_FORMAT(P, "pred: [");
        dump_block_list(P, data->predecessors);
        PRINT_LITERAL(P, "]\n");
        DUMP_FORMAT(P, "succ: [");
        dump_block_list(P, data->successors);
        PRINT_LITERAL(P, "]\n");
        --P->indent;
        DUMP_LITERAL(P, "}\n");
    }

    --P->indent;
    DUMP_LITERAL(P, "}\n");
}

char const *mir_dump_graph(struct Mir *mir)
{
    struct Compiler *C = mir->C;

    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    dump_graph(&(struct Printer){
                   .P = ENV(C),
                   .mir = mir,
                   .buf = &buf,
                   .C = C,
               },
               mir);

    Str const *result = lib_buffer_finish(P, &buf);
    return result->text;
}


void mir_validate_graph(struct Mir *mir)
{
    int index;
    struct MirBlockData *const *ppred;
    LIST_ENUMERATE(mir->blocks, index, ppred) {
        struct MirBlockData const *pred = *ppred;
        MirBlock const p = MIR_BB(index);

        paw_assert(pred->instructions->count > 0);
        struct MirInstruction const *term = LIST_LAST(pred->instructions);
        switch (MIR_KINDOF(term)) {
            case kMirGoto:
                paw_assert(pred->successors->count == 1);
                break;
            case kMirBranch:
                paw_assert(pred->successors->count == 2);
                break;
            case kMirSwitch:
                paw_assert(pred->successors->count >= 1);
                break;
            default:
                paw_assert(MirIsReturn(term) || MirIsUnreachable(term));
                paw_assert(pred->successors->count == 0);
        }

        MirBlock const *ps;
        LIST_FOREACH (pred->successors, ps) {
            mir_which_pred(mir, *ps, p); // crashes if not found
        }
    }
}
