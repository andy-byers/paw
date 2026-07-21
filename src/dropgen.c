// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "mir.h"

#define TODO (struct SourceSpan){0}

struct DropGenerator {
    struct Compiler *C;
};

static struct MirPlace new_register(struct Mir *mir, IrType *type)
{
    MirRegisterDataList_push(mir, mir->registers,
            (struct MirRegisterData){
                .is_nontrivial = PAW_TRUE,
                .type = type,
            });
    return (struct MirPlace){
        .r.value = mir->registers->count - 1,
        .kind = MIR_PLACE_REGISTER,
        .type = type,
    };
}
static struct MirPlace new_constant(MirConstant k, IrType *type)
{
    return (struct MirPlace){
        .kind = MIR_PLACE_CONSTANT,
        .type = type,
        .k = k,
    };
}

static IrType *new_ptr(struct DropGenerator *G, IrType *pointee)
{
    return pawIr_new_ptr(G->C, pointee);
}

static struct MirInstruction *select_field(struct Mir *mir, struct MirPlace object, int discr, int field, struct MirPlace output)
{
    return pawMir_new_struct_gep(mir, TODO, output, object, field, discr);
}

static struct MirInstruction *select_element(struct Mir *mir, struct MirPlace object, paw_Uint64 index, struct MirPlace output)
{
    IrType *usize = pawIr_new_int(mir->C, IR_USIZE);
    MirConstant const k = pawMir_kcache_add_value(mir, mir->kcache, (union IrValue){.u = index}, usize);
    return pawMir_new_array_gep(mir, TODO, output, object, new_constant(k, usize));
}

static void pushi(struct Mir *mir, struct MirBlockData const *data, struct MirInstruction *instr)
{
    MirInstructionList_push(mir, data->instructions, instr);
}

static void drop_fields_in_reverse(struct DropGenerator *G, struct Mir *mir, struct MirBlockData const *data, struct MirPlace place, int discr, IrTypeList *field_types, int field_offset)
{
    for (int i = field_types->count - 1; i >= 0; --i) {
        IrType *field_type = IrTypeList_get(field_types, i);
        if (pawIr_needs_drop(G->C, field_type)) {
            struct MirPlace const field = new_register(mir, new_ptr(G, field_type));
            pushi(mir, data, select_field(mir, place, discr, field_offset + i, field));
            pushi(mir, data, pawMir_new_drop(mir, TODO, field));
        }
    }
}

static void drop_elements_in_reverse(struct DropGenerator *G, struct Mir *mir, struct MirBlockData const *data, struct MirPlace place, IrType *element_type, paw_Uint64 length)
{
    if (pawIr_needs_drop(G->C, element_type)) {
        for (paw_Uint64 ri = 0; ri < length; ++ri) {
            struct MirPlace const element = new_register(mir, new_ptr(G, element_type));
            pushi(mir, data, select_element(mir, place, length - ri - 1, element));
            pushi(mir, data, pawMir_new_drop(mir, TODO, element));
        }
    }
}

static struct MirInstruction *popi(struct MirBlockData const *data)
{
    struct MirInstruction *last = MirInstructionList_last(data->instructions);
    MirInstructionList_pop(data->instructions);
    return last;
}

static struct MirPlace push_deref(struct Mir *mir, struct MirBlockData const *data, struct MirPlace pointer)
{
    struct MirPlace pointee = new_register(mir, ir_deref(pointer.type));
    struct MirInstruction *deref = pawMir_new_load(mir, TODO, pointer, pointee);
    MirInstructionList_push(mir, data->instructions, deref);
    return pointee;
}

static void drop_tuple_fields(struct DropGenerator *G, struct Mir *mir, struct MirPlace local)
{
    struct MirBlockData const *data = MirBlockDataList_last(mir->blocks);
    struct MirInstruction *terminator = popi(data);

    struct IrTuple *t = IrGetTuple(ir_deref(local.type));
    drop_fields_in_reverse(G, mir, data, local, 0, t->elems, 0);

    MirInstructionList_push(mir, data->instructions, terminator);
}

static void drop_array_elements(struct DropGenerator *G, struct Mir *mir, struct MirPlace local)
{
    struct MirBlockData const *data = MirBlockDataList_last(mir->blocks);
    struct MirInstruction *terminator = popi(data);

    struct IrArray const *t = IrGetArray(ir_deref(local.type));

    paw_assert(t->length->kind == IR_CONST_VALUE);
    struct IrConstValue const len = t->length->value;
    paw_assert(IrIsInt(len.type) && IR_INT_KIND(len.type) == IR_USIZE);
    drop_elements_in_reverse(G, mir, data, local, t->type, len.value.usize);

    MirInstructionList_push(mir, data->instructions, terminator);
}

static void drop_struct_fields(struct DropGenerator *G, struct Mir *mir, struct MirPlace local)
{
    struct MirBlockData const *data = MirBlockDataList_last(mir->blocks);
    struct MirInstruction *terminator = popi(data);

    struct IrAdt *t = IrGetAdt(ir_deref(local.type));
    IrTypeList *fields = pawP_instantiate_struct_fields(G->C, t);
    drop_fields_in_reverse(G, mir, data, local, 0, fields, 0);

    MirInstructionList_push(mir, data->instructions, terminator);
}

static MirBlock push_basic_block(struct Mir *mir, struct MirBlockData const **data_ptr)
{
    struct MirBlockData *data = pawMir_new_block(mir);
    MirBlockDataList_push(mir, mir->blocks, data);
    if (data_ptr != NULL) *data_ptr = data;
    return MIR_BB(mir->blocks->count - 1);
}

static void push_return(struct Mir *mir, struct MirBlockData const *data)
{
    pushi(mir, data, pawMir_new_return(mir, TODO));
}

static void push_goto(struct Mir *mir, struct MirBlockData const *data)
{
    pushi(mir, data, pawMir_new_goto(mir, TODO));
}

static void drop_enum_variants(struct DropGenerator *G, struct Mir *mir, struct MirPlace local)
{
    MirBlock const last = MIR_BB(mir->blocks->count - 1);
    struct MirBlockData const *last_data = MirBlockDataList_last(mir->blocks);
    struct MirInstruction *terminator = popi(last_data);
    struct MirInstruction *store_ret = popi(last_data);

    MirSwitchArmList *arms = MirSwitchArmList_new(mir);
    {
        // transform the return into a switch on the discriminant
        struct MirPlace const discr_addr = new_register(mir, new_ptr(G, pawIr_new_int(G->C, IR_INT64)));
        struct MirPlace const discr_value = new_register(mir, ir_deref(discr_addr.type));
        pushi(mir, last_data, select_field(mir, local, 0, 0, discr_addr));
        pushi(mir, last_data, pawMir_new_load(mir, TODO, discr_addr, discr_value));
        terminator->Switch_ = (struct MirSwitch){
            .kind = kMirSwitch,
            .discr = discr_value,
            .has_otherwise = PAW_FALSE,
            .arms = arms,
        };
        pushi(mir, last_data, terminator);
    }

    struct MirBlockData const *exit_data;
    MirBlock const exit = push_basic_block(mir, &exit_data);
    pushi(mir, exit_data, store_ret);
    push_return(mir, exit_data);

    struct IrAdt *t = IrGetAdt(ir_deref(local.type));
    struct IrAdtDef const *def = pawIr_get_adt_def(G->C, t->did);
    K_LIST_XFOREACH (def->variants, struct IrVariantDef *const, v) {
        int const discr = (*v)->discr;

        struct MirBlockData const *block_data;
        MirBlock const block = push_basic_block(mir, &block_data);

        MirSwitchArmList_push(mir, arms, (struct MirSwitchArm){
                .k = pawMir_kcache_add_value(mir, mir->kcache, I2V(discr), pawIr_new_int(mir->C, IR_INT64)),
            });

        MirBlockList_push(mir, last_data->successors, block);
        MirBlockList_push(mir, block_data->predecessors, last);
        MirBlockList_push(mir, block_data->successors, exit);
        MirBlockList_push(mir, exit_data->predecessors, block);

        // drop fields from this particular variant (indicated by "discr")
        IrTypeList *fields = pawP_instantiate_variant_fields(G->C, t, discr);
        drop_fields_in_reverse(G, mir, block_data, local, discr, fields, 1);

        push_goto(mir, block_data);
    }
}

// A type needs to be dropped if
// (a) there exists an implementation of Drop for the type, or
// (b) a subcomponent (field or element) of type needs to be dropped
//
// At this phase of compilation, there exists a MIR object representing
// the "drop" routine for each type that needs to be dropped. Add code
// to each "drop" routine that drops each field of the object in reverse.
static void generate_drops_for_fields(struct DropGenerator *G, BodyList *bodies)
{
    DeclId const drop_did = G->C->core_traits[CORE_TRAIT_DROP];
    K_LIST_XFOREACH (bodies, struct Mir *, p) {
        if (IrIsSignature((*p)->type)) {
            struct IrSignature const *t = IrGetSignature((*p)->type);
            struct IrFnDef const *def = pawIr_get_fn_def(G->C, t->did);
            if ((*p)->blocks->count > 0 && DECL_ID_EXISTS(def->parent)) {
                struct IrImpl const *impl = pawIr_get_impl_def(G->C, def->parent);
                if (impl->trait != NULL && MIR_ID_EQUALS(impl->trait->did, drop_did)) {
                    struct MirPlace const local = pawMir_get_register(*p, MIR_REG(1));
                    if (IrIsTuple((*p)->self)) {
                        drop_tuple_fields(G, *p, local);
                    } else if (IrIsArray((*p)->self)) {
                        drop_array_elements(G, *p, local);
                    } else if (IrIsAdt((*p)->self)) {
                        struct IrAdt const *t = IrGetAdt((*p)->self);
                        struct IrAdtDef const *def = pawIr_get_adt_def(G->C, t->did);
                        if (def->is_struct) {
                            drop_struct_fields(G, *p, local);
                        } else {
                            drop_enum_variants(G, *p, local);
                        }
                    }
                }
            }
        }
    }
}

void pawIr_generate_drops(struct Compiler *C, BodyList *bodies)
{
    struct DropGenerator G = {
        .C = C,
    };

    generate_drops_for_fields(&G, bodies);
}

