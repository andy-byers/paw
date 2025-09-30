// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "regstack.h"
#include "compile.h"
#include "error.h"
#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include "ssa.h"
#include <stdlib.h>

#define REGSTACK_ERROR(R_, Kind_, ...) pawErr_##Kind_((R_)->C, (R_)->mir->modname, __VA_ARGS__)
#define INSTR_LOC(Instr_) ((Instr_)->hdr.loc)

struct RegstackEnforcer {
    struct Pool *pool;
    struct Compiler *C;
    struct Mir *mir;
    paw_Env *P;

    RegstackMap *regs;
    AccessMap *uses, *defs;
    MirStack current;
    MirBlock b;
};

static void simulate_new_stack(struct RegstackEnforcer *R)
{
    ++R->current.value;
}

static void assign_stack_slots(struct RegstackEnforcer *R, struct MirInstruction *instr, int offset)
{
    MirPlacePtrList *loads = pawMir_get_loads(R->mir, instr);

    for (int i = offset; i < loads->count; ++i) {
        struct MirPlace const *pload = MirPlacePtrList_get(loads, i);
        paw_assert(pload->kind == MIR_PLACE_LOCAL);
        struct MirConstraint *pcon = &mir_reg_data(R->mir, pload->r)->con;
        pcon->kind = MIR_CONSTRAINT_STACK;
        pcon->stack.id = R->current;
        pcon->stack.index = i - offset;
    }

    MirPlacePtrList_delete(R->mir, loads);
}

static void enforce_constraint(struct RegstackEnforcer *R)
{
    int index;
    struct MirBlockData *const *pbb;
    K_LIST_ENUMERATE (R->mir->blocks, index, pbb) {
        struct MirBlockData *bb = *pbb;
        R->b = MIR_BB(index);

        {
            struct MirInstruction *const *pinstr;
            K_LIST_FOREACH (bb->instructions, pinstr) {
                struct MirInstruction *instr = *pinstr;

                int offset = 0;
                enum MirConstraintKind kind;
                switch (MIR_KINDOF(instr)) {
                    case kMirGetElement:
                    case kMirGetElementPtr:
                    case kMirGetRange:
                    case kMirSetElement:
                    case kMirSetRange:
                        offset = 1;
                        // (fallthrough)
                    case kMirCall:
                    case kMirConcat:
                    case kMirUnpack:
                    case kMirReturn:
                    case kMirContainer:
                    case kMirAggregate:
                        simulate_new_stack(R);
                        assign_stack_slots(R, instr, offset);
                        *mir_pstack(instr) = R->current;
                        break;
                    default:
                        break;
                }
            }
        }
    }
}

RegstackMap *pawRegstack_enforce_constraint(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct Pool *pool = pawP_pool_new(C, C->aux_stats);
    struct RegstackEnforcer *R = &(struct RegstackEnforcer){
        .uses = AccessMap_new_from(mir, pool),
        .defs = AccessMap_new_from(mir, pool),
        .regs = RegstackMap_new(mir),
        .pool = pool,
        .P = ENV(C),
        .mir = mir,
        .C = C,
    };

    pawMir_collect_per_instr_uses(mir, R->uses);
    pawMir_collect_per_instr_defs(mir, R->defs);

    enforce_constraint(R);
    return R->regs;
}
