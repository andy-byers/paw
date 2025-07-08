// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "mir.h"

#define KBUFFER_CAPACITY 5

struct Materializer {
    struct Pool *pool;
    struct MirVisitor *V;
    struct Mir *mir;
    paw_Env *P;

    struct ConstantMap *constants;
    struct MirBlockData *current;
    MirRegisterDataList *registers;
    MirInstructionList *rewrite;
};

struct ConstantInfo {
    struct MirPlace place;
};

DEFINE_MAP(struct Materializer, ConstantMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirConstant, struct ConstantInfo)

#define ADD_INSTR(M_, Instr_) MirInstructionList_push((M_)->mir, (M_)->rewrite, Instr_)
#define NEW_INSTR(M_, Kind_, ...) ADD_INSTR(M_, pawMir_new_##Kind_((M_)->mir, __VA_ARGS__))

static struct MirPlace new_local_for_constant(struct Materializer *M, enum BuiltinKind kind)
{
    int const id = M->mir->registers->count;
    MirRegisterDataList_push(M->mir, M->mir->registers,
        (struct MirRegisterData){
            .type = pawP_builtin_type(M->mir->C, kind),
            .size = 1,
        });
    return (struct MirPlace){
        .kind = MIR_PLACE_LOCAL,
        .r = MIR_REG(id),
    };
}

static struct ConstantInfo load_constant(struct Materializer *M, struct SourceLoc loc, MirConstant k, struct MirConstantData const *data)
{
    struct MirPlace const place = new_local_for_constant(M, data->kind);
    struct MirInstruction *instr = pawMir_new_load_constant(M->mir, loc, data->kind, data->value, place);
    ADD_INSTR(M, instr);
    return (struct ConstantInfo){
        .place = place,
    };
}

static void ensure_materialized(struct Materializer *M, struct SourceLoc loc, struct MirPlace *pplace)
{
    if (pplace->kind == MIR_PLACE_CONSTANT) {
        struct MirConstantData *data = mir_const_data(M->mir, pplace->k);
        struct ConstantInfo const *pinfo = ConstantMap_get(M, M->constants, pplace->k);
        if (pinfo == NULL) {
            // materialize the constant in a new virtual register
            struct ConstantInfo const info = load_constant(M, loc, pplace->k, data);
            ConstantMap_insert(M, M->constants, pplace->k, info);
            *pplace = info.place;
        } else {
            // constant is already materialized
            *pplace = pinfo->place;
        }
    }
}

static paw_Bool enter_block(struct MirVisitor *V, MirBlock b)
{
    struct Materializer *M = V->ud;
    M->rewrite = MirInstructionList_new(M->mir);
    M->current = mir_bb_data(M->mir, b);
    M->constants = ConstantMap_new(M);
    return PAW_TRUE;
}

static void leave_block(struct MirVisitor *V, MirBlock b)
{
    struct Materializer *M = V->ud;
    ConstantMap_delete(M, M->constants);
    MirInstructionList_delete(M->mir, M->current->instructions);
    M->current->instructions = M->rewrite;
}

static void visit_instruction(struct MirVisitor *V, struct MirInstruction *instr)
{
    struct Materializer *M = V->ud;
    switch (MIR_KINDOF(instr)) {
        // TODO: special cases go here, e.g. integer addition where 1 operand is a constant


        default: {
            MirPlacePtrList *loads = pawMir_get_loads(M->mir, instr);

            struct MirPlace *const *ppload;
            K_LIST_FOREACH (loads, ppload)
                ensure_materialized(M, instr->hdr.loc, *ppload);

            MirPlacePtrList_delete(M->mir, loads);
        }
    }
    MirInstructionList_push(M->mir, M->rewrite, instr);
}

static void materialize(struct Materializer *M, struct Mir *mir)
{
    M->mir = mir;
    M->registers = mir->registers;

    pawMir_visit(M->V);

    struct Mir *const *pchild;
    K_LIST_FOREACH (mir->children, pchild)
        materialize(M, *pchild);
}

void pawMir_materialize_constants(struct Mir *mir)
{
    struct Materializer M = {
        .V = &(struct MirVisitor){0},
        .mir = mir,
    };
    pawMir_visitor_init(M.V, mir->C, mir, &M);
    M.V->PostVisitInstruction = visit_instruction;
    M.V->VisitBlock = enter_block;
    M.V->PostVisitBlock = leave_block;

    materialize(&M, mir);
}
