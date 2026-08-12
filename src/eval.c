// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "eval.h"
#include "mir.h"

#warning
#include"stdio.h"

struct EvalCtx {
    struct ValueTable *values;
    struct Compiler *C;
    struct Pool *pool;
    struct Mir *mir;
};

DEFINE_LIST(struct EvalCtx, ValueTable, union IrValue,)

struct EvalStep {
    enum EvalStepKind {
        ESK_GOTO,
        ESK_RETURN,
        ESK_PANIC,
        ESK_OVERFLOW,
        ESK_DIVIDE0,
    } kind;
    union {
        MirBlock target;
        Str const *message;
    };
};

#define ES_RETURN() (struct EvalStep){.kind = ESK_RETURN}
#define ES_OVERFLOW() (struct EvalStep){.kind = ESK_OVERFLOW}
#define ES_DIVIDE0() (struct EvalStep){.kind = ESK_DIVIDE0}
#define ES_PANIC(Message_) (struct EvalStep){.kind = ESK_PANIC, .message = Message_}
#define ES_GOTO(Target_) (struct EvalStep){.kind = ESK_GOTO, .target = Target_}

static IrValue get_rvalue(struct EvalCtx *E, struct MirPlace place)
{
    if (place.kind == MIR_PLACE_REGISTER) {
        return ValueTable_get(E->values, place.r.value);
    } else {
        paw_assert(place.kind == MIR_PLACE_CONSTANT);
        IrConst *k = mir_const_data(E->mir, place.k)->data;
        paw_assert(k->kind == IR_CONST_VALUE);
        return k->value.value;
    }
}

static IrValue *get_lvalue(struct EvalCtx *E, struct MirPlace place)
{

    paw_assert(place.kind == MIR_PLACE_REGISTER);
    paw_assert(0 <= place.r.value && place.r.value < E->values->count);
    return &K_LIST_AT(E->values, place.r.value);
}

static struct EvalStep evaluate_basic_block(struct EvalCtx *E, struct MirBlockData bb)
{
    K_LIST_XFOREACH (bb.instructions, struct MirInstruction *const, pinstr) {
#define LVALUE(Place_) get_lvalue(E, Place_)
#define RVALUE(Place_) get_rvalue(E, Place_)

        switch (MIR_KINDOF(*pinstr)) {
            case kMirNoop:
                break;
            case kMirMove: {
                struct MirMove const *x = MirGetMove(*pinstr);
                *LVALUE(x->output) = RVALUE(x->target);
                break;
            }
//            case kMirLoad: {
//                struct MirLoad const *x = MirGetLoad(*pinstr);
//                break;
//            }
//            case kMirStore: {
//                struct MirStore const *x = MirGetStore(*pinstr);
//                break;
//            }
//            case kMirAddrOf: {
//                struct MirAddrOf const *x = MirGetAddrOf(*pinstr);
//                break;
//            }
//            case kMirGlobal: {
//                struct MirGlobal const *x = MirGetGlobal(*pinstr);
//                break;
//            }
//            case kMirAggregate: {
//                struct MirAggregate const *x = MirGetAggregate(*pinstr);
//                break;
//            }
//            case kMirArray: {
//                struct MirArray const *x = MirGetArray(*pinstr);
//                break;
//            }
//            case kMirArrayGep: {
//                struct MirArrayGep const *x = MirGetArrayGep(*pinstr);
//                break;
//            }
//            case kMirStructGEP: {
//                struct MirStructGEP const *x = MirGetStructGEP(*pinstr);
//                break;
//            }
//            case kMirKill: {
//                struct MirKill const *x = MirGetKill(*pinstr);
//                break;
//            }
//            case kMirDrop: {
//                struct MirDrop const *x = MirGetDrop(*pinstr);
//                break;
//            }
//            case kMirCall: {
//                struct MirCall const *x = MirGetCall(*pinstr);
//                break;
//            }
            case kMirCast: {
                struct MirCast const *x = MirGetCast(*pinstr);
                pawMir_fold_cast(RVALUE(x->target), x->target.type,
                        x->output.type, LVALUE(x->output));
                break;
            }
            case kMirUnaryOp: {
                struct MirUnaryOp const *x = MirGetUnaryOp(*pinstr);
                IrValue const val = RVALUE(x->val);
                enum MirFoldResult const r = pawMir_fold_unary_op(x->op,
                        x->val.type, val, LVALUE(x->output));
                switch (r) {
                    case MIR_FOLD_FOLDED:
                        break;
                    case MIR_FOLD_OVERFLOW:
                        return ES_OVERFLOW();
                    case MIR_FOLD_DIVIDE_BY_ZERO:
                        return ES_DIVIDE0();
                }
                break;
            }
            case kMirBinaryOp: {
                struct MirBinaryOp const *x = MirGetBinaryOp(*pinstr);
                IrValue const lhs = RVALUE(x->lhs);
                IrValue const rhs = RVALUE(x->rhs);
                enum MirFoldResult const r = pawMir_fold_binary_op(x->op,
                        x->lhs.type, lhs, rhs, LVALUE(x->output));
                switch (r) {
                    case MIR_FOLD_FOLDED:
                        break;
                    case MIR_FOLD_OVERFLOW:
                        return ES_OVERFLOW();
                    case MIR_FOLD_DIVIDE_BY_ZERO:
                        return ES_DIVIDE0();
                }
                break;
            }
            case kMirReturn: {
                return ES_RETURN();
            }
            case kMirBranch: {
                struct MirBranch const *x = MirGetBranch(*pinstr);
                MirBlock const left = MirBlockList_get(bb.successors, 0);
                MirBlock const right = MirBlockList_get(bb.successors, 1);
                return ES_GOTO(RVALUE(x->cond).b ? left : right);
            }
            case kMirSwitch: {
                struct MirSwitch const *x = MirGetSwitch(*pinstr);
                paw_Int64 const discr = RVALUE(x->discr).i64;
                return ES_GOTO(MirBlockList_get(bb.successors, discr));
            }
            case kMirGoto: {
                return ES_GOTO(MirBlockList_first(bb.successors));
            }
            default:
                PAW_UNREACHABLE();
        }

#undef RVALUE
#undef LVALUE
    }

    PAW_UNREACHABLE();
}

static struct MirEvalResult evaluate(struct EvalCtx *E)
{
    struct Mir *mir = E->mir;
    // set up scratch memory containing IR values
    ValueTable_reserve(E, E->values, mir->registers->count);
    K_LIST_XFOREACH (mir->registers, struct MirRegisterData const, p)
        ValueTable_push(E, E->values, (IrValue){0});

    MirBlock cursor = MIR_ENTRY_BB;
    for (;;) {
        struct MirBlockData const bb = *mir_bb_data(mir, cursor);
        struct EvalStep const step = evaluate_basic_block(E, bb);
        switch (step.kind) {
            case ESK_GOTO:
                cursor = step.target;
                break;
            case ESK_RETURN:
                return (struct MirEvalResult){
                    .status = MES_EVALUATED,
                    .value = ValueTable_first(E->values),
                };
            case ESK_PANIC:
                return (struct MirEvalResult){
                    .status = MES_PANICKED,
                    .message = step.message,
                };
            case ESK_OVERFLOW:
                return (struct MirEvalResult){
                    .status = MES_OVERFLOW,
                    .message = step.message,
                };
            case ESK_DIVIDE0:
                return (struct MirEvalResult){
                    .status = MES_DIVIDE0,
                    .message = step.message,
                };
        }
    }
}

struct MirEvalResult pawMir_eval(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct MirEvalResult result;
    {
        struct Pool *pool = pawP_pool_new(C, C->aux_stats);
        struct EvalCtx E = { .pool = pool, .mir = mir, .C = C };
        E.values = ValueTable_new(&E);
        result = evaluate(&E);
        pawP_pool_free(C, pool);
    }
    return result;
}
