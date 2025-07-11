// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// unbox.c: Make all virtual registers have a size of 1 VM register
//
// The process involves splitting each register of size N into N registers of
// size 1, and adding move instructions to compensate. The register allocator
// and code generation module expect each virtual register to correspond to
// a single actual register in a runtime activation frame (a VM register).
// Unboxing also expands objects nested inside other objects.
//
// After unboxing, containers like lists and maps may have elements that are
// wider than a single Value. To account for this, the size of an element is
// stored inside the data structure at runtime, and fields of composite
// elements are accessed through "element pointers".
//
// At present, Paw has no concept of pointers/references in the language
// itself, making it awkward to express some memory accesses. For example,
// getting an element of some container cannot be expressed using a function,
// since it would not be possible to set a field on a composite element (i.e.
// writing "container[index].field = 123;" would set a field on a copy of
// the element returned by "container[index]" rather than the element itself).
// At runtime, the "GETP" instruction is used to get a pointer to the start
// of the element. Then, "SETVALUE" is used to set the field Value through
// the element pointer.

#include "ir_type.h"
#include "layout.h"
#include "mir.h"

// TODO: store source location for projections? or at least pass in the source loc of the expression
#define TODO ((struct SourceLoc){-1})

#define ENUM_BASE (-1)

#define NEW_INSTR(Ctx_, Kind_, ...) \
    MirInstructionList_push((Ctx_)->fs->mir, (Ctx_)->instructions, \
            pawMir_new_##Kind_((Ctx_)->fs->mir, __VA_ARGS__));

#define REGISTER_AT(Group_, Offset_) MIR_REG((Group_).base + Offset_)
#define UPVALUE_AT(Group_, Offset_) ((Group_).base + Offset_)
#define PLACE(Reg_) ((struct MirPlace){.r = Reg_})

struct FunctionState {
    struct FunctionState *outer;
    MirRegisterDataList *registers;
    MirUpvalueList *upvalues;
    MirRegisterList *old_locals;
    struct UpvalueGroupMap *upvalue_map;
    struct LocalGroupMap *local_map;
    struct Mir *mir;
};

struct Unboxer {
    MirInstructionList *instructions;
    struct FunctionState *fs;
    struct Pool *pool;
    struct Compiler *C;
    paw_Env *P;
};

enum MemoryKind {
    MEMORY_LOCAL,
    MEMORY_UPVALUE,
};

struct MemoryGroup {
    enum MemoryKind kind;
    int base;
    int count;
};

enum AccessKind {
    ACCESS_NONE,
    ACCESS_FIELD,
    ACCESS_INDEX,
};

// Represents a runtime memory access
// All runtime memory accesses target objects located on the stack. Accessing a field
// on a value type located on the stack is translated into a move. Field accesses on
// boxed objects are translated into (GET | SET)FIELD instructions.
//
// When both ".has_field" and ".has_element" flags are set, then the object describes
// a field access on an element in some container. If an element of a container
// located in some other object is required, then the container is first moved into
// a register before its element is accessed.
//
// let list: [(int, str)] = [(1, "a"), (2, "b"), (3, "c")];
//
// let element = list[0];
struct MemoryAccess {
    struct MemoryGroup group;
    IrType *type;

    paw_Bool has_field : 1;
    paw_Bool has_element : 1;
    paw_Bool has_range : 1;

    struct FieldAccess {
        IrType *type;
        int offset;
        int discr;
    } field;

    struct ElementAccess {
        IrType *type;
        MirRegister index;
    } element;

    struct RangeAccess {
        MirRegister lower;
        MirRegister upper;
    } range;
};

DEFINE_MAP(struct Unboxer, LocalGroupMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirRegister, struct MemoryGroup)
DEFINE_MAP(struct Unboxer, UpvalueGroupMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, int, struct MemoryGroup)
DEFINE_LIST(struct Unboxer, MemoryGroupList, struct MemoryGroup)

static MirId next_mid(struct FunctionState *fs)
{
    return (MirId){fs->mir->mir_count++};
}

static paw_Bool is_composite(struct Unboxer *U, IrType *type)
{
    if (IrIsTuple(type)) return PAW_TRUE;
    if (!IrIsAdt(type)) return PAW_FALSE;

    struct IrAdtDef *def = pawIr_get_adt_def(U->C, IR_TYPE_DID(type));
    return def->is_inline;
}

static IrType *builtin_type(struct Unboxer *U, enum BuiltinKind kind)
{
    return pawP_builtin_type(U->C, kind);
}

static IrTypeList *get_base_field_types(struct Unboxer *U, IrType *parent)
{
    if (IrIsTuple(parent))
        return IrGetTuple(parent)->elems;

    struct IrAdt *adt = IrGetAdt(parent);
    struct IrAdtDef *def = pawIr_get_adt_def(U->C, adt->did);
    if (def->is_struct)
        return pawP_instantiate_struct_fields(U->C, IrGetAdt(parent));

    IrTypeList *types = IrTypeList_new(U->C);
    IrTypeList_push(U->C, types, builtin_type(U, PAW_TINT));
    // "()" is used to represent unknown field types (the discriminant is not known, so the fields
    // could be those of any variant), but the choice is arbitrary so long as the type has a size
    // of 1. The fields will not be accessed until the discriminant is known.
    struct IrLayout layout = pawIr_compute_layout(U->C, parent);
    for (int i = 1; i < layout.size; ++i)
        IrTypeList_push(U->C, types, builtin_type(U, PAW_TUNIT));
    return types;
}

static IrTypeList *instantiate_variant_fields(struct Unboxer *U, IrType *type, int discr)
{
    struct IrAdt *parent = IrGetAdt(type);
    IrTypeList *fields = pawP_instantiate_variant_fields(U->C, parent, discr);

    IrTypeList *result = IrTypeList_new(U->C);
    IrTypeList_reserve(U->C, result, 1 + fields->count);
    IrTypeList_push(U->C, result, builtin_type(U, BUILTIN_INT));

    int size = 1;
    IrType *const *ptype;
    K_LIST_FOREACH (fields, ptype) {
        IrTypeList_push(U->C, result, *ptype);
        struct IrLayout const layout = pawIr_compute_layout(U->C, type);
        size += layout.size;
    }

    if (is_composite(U, type)) {
        // pad the object so it can contain any variant of this ADT
        struct IrLayout layout = pawIr_compute_layout(U->C, type);
        for (; size < layout.size; ++size)
            IrTypeList_push(U->C, result, pawP_builtin_type(U->C, BUILTIN_UNIT));
    }

    return result;
}

static IrTypeList *get_variant_field_types(struct Unboxer *U, IrType *parent, int discr)
{
    if (IrIsTuple(parent))
        return IrGetTuple(parent)->elems;

    struct IrAdt *adt = IrGetAdt(parent);
    struct IrAdtDef *def = pawIr_get_adt_def(U->C, adt->did);
    if (def->is_struct) {
        return pawP_instantiate_struct_fields(U->C, IrGetAdt(parent));
    } else {
        return instantiate_variant_fields(U, parent, discr);
    }
}

typedef void (*ValueAllocator)(struct FunctionState *fs, IrType *type, void *ctx);

static int allocate_values(struct Unboxer *U, IrType *type, int discr, ValueAllocator alloc, void *ctx)
{
    if (!is_composite(U, type)) {
        // scalar or boxed type
        alloc(U->fs, type, ctx);
        return 1;
    }

    IrTypeList *types = discr >= 0
        ? get_variant_field_types(U, type, discr)
        : get_base_field_types(U, type);

    int count = 0;
    IrType *const *ptype;
    K_LIST_FOREACH (types, ptype)
        count += allocate_values(U, *ptype, discr, alloc, ctx);

    if (count == 0) {
        // every value must occupy at least 1 register
        alloc(U->fs, pawP_builtin_type(U->C, BUILTIN_UNIT), ctx);
        return 1;
    }
    return count;
}

static struct MemoryGroup get_registers(struct Unboxer *U, MirRegister r, int discr);

struct RegisterState {
    paw_Bool is_captured;
    paw_Bool is_uninit;
};

static void allocate_register(struct FunctionState *fs, IrType *type, void *ctx)
{
    struct RegisterState *rs = ctx;
    MirRegisterDataList_push(fs->mir, fs->registers, (struct MirRegisterData){
                .is_captured = rs->is_captured,
                .is_uninit = rs->is_uninit,
                .type = type,
                .size = 1,
            });
}

struct UpvalueState {
    paw_Bool is_local;
    int up;
};

static void allocate_upvalue(struct FunctionState *fs, IrType *type, void *ctx)
{
    struct UpvalueState *us = ctx;
    MirUpvalueList_push(fs->mir, fs->upvalues, (struct MirUpvalueInfo){
                .is_local = us->is_local,
                .index = us->up++,
                .type = type,
            });
}

// TODO: combine with alloc_scalar_registers
static struct MemoryGroup new_registers(struct Unboxer *U, IrType *type, int discr)
{
    struct RegisterState rs = {0};
    int const base = U->fs->registers->count;
    int const count = allocate_values(U, type, discr, allocate_register, &rs);
    return (struct MemoryGroup){
        .count = count,
        .base = base,
    };
}

static struct MirPlace new_rawptr_place(struct Unboxer *U, IrType *type)
{
    struct FunctionState *fs = U->fs;
    MirRegisterDataList_push(fs->mir, fs->registers,
            (struct MirRegisterData){
                .is_pointer = PAW_TRUE,
                .type = type,
                .size = 1,
            });
    return PLACE(MIR_REG(fs->registers->count - 1));
}

static struct MemoryGroup get_registers(struct Unboxer *U, MirRegister r, int discr)
{
    struct FunctionState *fs = U->fs;
    struct MemoryGroup const *pgroup = LocalGroupMap_get(U, fs->local_map, r);
    if (pgroup != NULL)
        return *pgroup;

    struct MirRegisterData *data = mir_reg_data(fs->mir, r);
    int const num_registers = fs->registers->count;

    struct RegisterState rs = {.is_captured = data->is_captured, .is_uninit = data->is_uninit};
    int const count = allocate_values(U, data->type, discr, allocate_register, &rs);
    struct MemoryGroup const group = {
        .kind = MEMORY_LOCAL,
        .base = num_registers,
        .count = count,
    };
    LocalGroupMap_insert(U, fs->local_map, r, group);
    return group;
}

static struct MemoryGroup get_upvalues(struct Unboxer *U, int up)
{
    struct FunctionState *fs = U->fs;
    struct MemoryGroup const *pgroup = UpvalueGroupMap_get(U, fs->upvalue_map, up);
    paw_assert(pgroup != NULL);
    return *pgroup;
}

static struct MemoryGroup new_nonlocal_upvalues(struct Unboxer *U, int up, struct MemoryGroup mg)
{
    struct FunctionState *fs = U->fs;
    struct MirUpvalueInfo info = MirUpvalueList_get(fs->mir->upvalues, up);
    int const num_upvalues = fs->upvalues->count;

    struct UpvalueState us = {.up = mg.base};
    int const count = allocate_values(U, info.type, ENUM_BASE, allocate_upvalue, &us);
    struct MemoryGroup const group = {
        .kind = MEMORY_UPVALUE,
        .base = num_upvalues,
        .count = count,
    };
    UpvalueGroupMap_insert(U, fs->upvalue_map, up, group);
    return group;
}

static struct MemoryGroup new_local_upvalues(struct Unboxer *U, int up, struct MemoryGroup mg, int local)
{
    struct FunctionState *fs = U->fs;
    struct MirUpvalueInfo info = MirUpvalueList_get(fs->mir->upvalues, up);
    int const num_upvalues = fs->upvalues->count;

    struct UpvalueState us = {.up = local, .is_local = PAW_TRUE};
    int const count = allocate_values(U, info.type, ENUM_BASE, allocate_upvalue, &us);
    struct MemoryGroup const group = {
        .kind = MEMORY_UPVALUE,
        .base = num_upvalues,
        .count = count,
    };
    UpvalueGroupMap_insert(U, fs->upvalue_map, up, group);
    return group;
}

static struct MemoryGroup get_location(struct Unboxer *U, struct MirPlace p)
{
    if (p.kind == MIR_PLACE_UPVALUE)
        return get_upvalues(U, p.up);
    return get_registers(U, p.r, ENUM_BASE);
}

static struct MemoryGroup split_group(struct MemoryGroup group, int offset, int count)
{
    paw_assert(offset <= group.count - count);
    return (struct MemoryGroup){
        .kind = group.kind,
        .base = group.base + offset,
        .count = count,
    };
}

static int compute_field_offset(struct IrLayout object, int index)
{
    if (object.fields->count == 0) return 0;
    paw_assert(0 <= index && index < object.fields->count);

    int offset = 0;
    while (index-- > 0) {
        struct IrLayout const field = IrLayoutList_get(object.fields, index);
        offset += field.size;
    }
    return offset;
}

// Handles the following type(s) of memory accesses:
//     object.*.field
static void discharge_indirect_field(struct Unboxer *U, struct MemoryAccess *pa)
{
    IrTypeList const *field_types = get_variant_field_types(U, pa->type, pa->field.discr);
    struct MemoryGroup const output = new_registers(U, pa->field.type, pa->field.discr);

    struct MirPlace const object = PLACE(pa->group.base);
    for (int i = 0; i < output.count; ++i) {
        struct MirPlace const result = PLACE(REGISTER_AT(output, i));
        NEW_INSTR(U, get_field, TODO, pa->field.offset + i, result, object);
    }

    pa->group = output;
    pa->type = pa->field.type;
    pa->has_field = PAW_FALSE;
    pa->field = (struct FieldAccess){0};
}

static IrType *get_element_type(struct Unboxer *U, IrType *type)
{
    enum BuiltinKind const kind = pawP_type2code(U->C, type);

    if (kind == BUILTIN_LIST) {
        return ir_list_elem(type);
    } else if (kind == BUILTIN_MAP) {
        return ir_map_value(type);
    } else {
        paw_assert(kind == BUILTIN_STR);
        return pawP_builtin_type(U->C, BUILTIN_CHAR);
    }
}

static IrType *get_access_type(struct MemoryAccess *pa)
{
    return pa->has_field ? pa->field.type :
        pa->has_element ? pa->element.type :
        pa->type;
}

static void discharge_indirect_element(struct Unboxer *U, struct MemoryAccess *pa)
{
    enum BuiltinKind kind = pawP_type2code(U->C, pa->type);

    IrType *element_type = pa->element.type;
    IrType *access_type = get_access_type(pa);
    int const field_discr = pa->has_field ? pa->field.discr : ENUM_BASE;
    int const field_offset = pa->has_field ? pa->field.offset : 0;
    struct MemoryGroup const output = new_registers(U, access_type, field_discr);

    struct MirPlace const object = PLACE(pa->group.base);
    if (output.count > 1 || field_offset > 0) {
        struct MirPlace const pointer = new_rawptr_place(U, element_type);
        NEW_INSTR(U, get_element_ptr, TODO, kind, pointer, object,
                PLACE(pa->element.index), PAW_FALSE);

        for (int i = 0; i < output.count; ++i) {
            struct MirPlace const result = PLACE(REGISTER_AT(output, i));
            NEW_INSTR(U, get_field, TODO, field_offset + i, result, pointer);
        }
    } else {
        struct MirPlace const result = PLACE(REGISTER_AT(output, 0));
        NEW_INSTR(U, get_element, TODO, kind, result, object, PLACE(pa->element.index));
    }

    pa->group = output;
    pa->type = access_type;
    pa->has_field = PAW_FALSE;
    pa->has_element = PAW_FALSE;
    pa->field = (struct FieldAccess){0};
    pa->element = (struct ElementAccess){0};
}

static void discharge_indirect_range(struct Unboxer *U, struct MemoryAccess *pa)
{
    enum BuiltinKind kind = pawP_type2code(U->C, pa->type);
    struct MemoryGroup const output = new_registers(U, pa->type, ENUM_BASE);

    struct MirPlace const object = PLACE(pa->group.base);
    struct MirPlace const result = PLACE(REGISTER_AT(output, 0));
    NEW_INSTR(U, get_range, TODO, kind, result, object, PLACE(pa->range.lower), PLACE(pa->range.upper));

    pa->group = output;
    pa->has_range = PAW_FALSE;
    pa->range = (struct RangeAccess){0};
}

static void discharge_upvalue(struct Unboxer *U, struct MemoryAccess *pa)
{
    struct MemoryGroup const output = new_registers(U, pa->type, ENUM_BASE);
    for (int i = 0; i < pa->group.count; ++i) {
        struct MirPlace const result = PLACE(REGISTER_AT(output, i));
        NEW_INSTR(U, upvalue, TODO, result, UPVALUE_AT(pa->group, i));
    }
    pa->group = output;
}

// Place the result of a memory access into a register group
// Used when a nested indirect access is required, since the object being accessed must
// be in a register.
static void discharge_access(struct Unboxer *U, struct MemoryAccess *pa)
{
    if (pa->group.kind == MEMORY_UPVALUE)
        discharge_upvalue(U, pa);
    if (pa->has_range) {
        discharge_indirect_range(U, pa);
    } else if (pa->has_element) {
        discharge_indirect_element(U, pa);
    } else if (pa->has_field) {
        discharge_indirect_field(U, pa);
    }
}

static paw_Bool is_enum(struct Unboxer *U, IrType *type)
{
    if (IrIsAdt(type)) {
        struct IrAdtDef *def = pawIr_get_adt_def(U->C, IR_TYPE_DID(type));
        return !def->is_struct;
    }
    return PAW_FALSE;
}

// Apply a direct projection (no indirection)
static void apply_field_access(struct Unboxer *U, struct MemoryAccess *pa, MirProjection *p)
{
    struct MirField *field = MirGetField(p);
    IrType *target_type = get_access_type(pa);
    struct IrLayout target_layout = pawIr_compute_layout(U->C, target_type);
    IrTypeList const *field_types = get_variant_field_types(U, target_type, field->discr);
    IrType *field_type = IrTypeList_get(field_types, field->index);

    if (is_enum(U, target_type)) {
        // select layout of variant
        target_layout = IrLayoutList_get(target_layout.fields, field->discr);
    }
    struct IrLayout field_layout = IrLayoutList_get(target_layout.fields, field->index);

    if (pa->has_element || pa->has_field) {
        // Handle field access on a value type nested in another object.
        pa->field.offset += compute_field_offset(target_layout, field->index);
        pa->field.discr = field->discr;
        pa->field.type = field_type;
        pa->has_field = PAW_TRUE;
    } else {
        // Handle field access on a value type located on the stack.
        int const offset = compute_field_offset(target_layout, field->index);
        pa->group = split_group(pa->group, offset, field_layout.size);
        pa->type = field_type;
    }
}

// Apply an indirect projection
static void apply_indirect_access(struct Unboxer *U, struct MemoryAccess *pa, MirProjection *p)
{
    discharge_access(U, pa);

    if (MirIsField(p)) {
        struct MirField *field = MirGetField(p);
        struct IrAdtDef *def = pawIr_get_adt_def(U->C, IR_TYPE_DID(pa->type));
        struct IrLayout target_layout = pawIr_compute_layout(U->C, pa->type);
        target_layout = !def->is_struct
            ? IrLayoutList_get(target_layout.fields, field->discr)
            : CHECK_EXP(field->discr == 0, target_layout);
        IrTypeList const *field_types = get_variant_field_types(U, pa->type, field->discr);
        pa->field.type = IrTypeList_get(field_types, field->index);
        pa->field.offset = compute_field_offset(target_layout, field->index);
        pa->has_field = PAW_TRUE;
    } else if (MirIsIndex(p)){
        struct MirIndex *index = MirGetIndex(p);
        struct MemoryGroup const index_group = get_registers(U, index->index, ENUM_BASE);
        paw_assert(index_group.count == 1);
        pa->element.index.value = index_group.base;
        pa->element.type = get_element_type(U, pa->type);
        pa->has_element = PAW_TRUE;
    } else {
        struct MirRange *range = MirGetRange(p);
        struct MemoryGroup const lower_group = get_registers(U, range->lower, ENUM_BASE);
        struct MemoryGroup const upper_group = get_registers(U, range->upper, ENUM_BASE);
        paw_assert(lower_group.count == 1 && upper_group.count == 1);
        pa->range.lower.value = lower_group.base;
        pa->range.upper.value = upper_group.base;
        pa->has_range = PAW_TRUE;
    }
}

// Apply the projections associated with the given place
// Essentially, this function determines the memory location referred to by a MIR place
// construct. A memory location can be either a set of virtual registers (each with a size
// of 1 Paw value), or a contiguous group of values inside another object.
static struct MemoryAccess unbox_place(struct Unboxer *U, struct MirPlace *pplace)
{
    struct Mir *mir = U->fs->mir;
    struct MemoryAccess access = {
        .group = get_location(U, *pplace),
    };
    if (pplace->kind == MIR_PLACE_LOCAL) {
        access.type = mir_reg_data(mir, pplace->r)->type;
    } else {
        paw_assert(pplace->kind == MIR_PLACE_UPVALUE);
        access.type = MirUpvalueList_get(mir->upvalues, pplace->up).type;
    }
    MirProjectionList const *ps = pplace->projection;
    paw_assert(ps != NULL);

    for (int i = 0; i < ps->count;) {
        MirProjection *p = MirProjectionList_get(ps, i++);
        if (MirIsDeref(p)) {
            // handle indirect accesses
            p = MirProjectionList_get(ps, i++);
            apply_indirect_access(U, &access, p);
        } else {
            // handle direct field access
            apply_field_access(U, &access, p);
        }
    }

    // projections have been transformed into instructions
// TODO: breaks things for some reason ("trash_memory" overwrites something important, indicating a memory error somewhere)
// TODO:    MirProjectionList_delete(mir, pplace->projection);
    pplace->kind = MIR_PLACE_LOCAL;
    pplace->projection = NULL;
    return access;
}

static void create_move(struct Unboxer *U, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    struct MemoryGroup const object = lhs.group;
    struct MemoryGroup const value = rhs.group;

    for (int i = 0; i < value.count; ++i) {
        MirRegister const a = REGISTER_AT(value, i);
        MirRegister const b = REGISTER_AT(object, i);
        NEW_INSTR(U, move, TODO, PLACE(b), PLACE(a));
    }
}

static void create_field_setter(struct Unboxer *U, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    struct MemoryGroup const object = lhs.group;
    struct MemoryGroup const value = rhs.group;

    for (int i = 0; i < value.count; ++i) {
        MirRegister const a = REGISTER_AT(value, i);
        MirRegister const b = REGISTER_AT(object, lhs.field.offset + i);
        NEW_INSTR(U, move, TODO, PLACE(b), PLACE(a));
    }
}

static void create_indirect_field_setter(struct Unboxer *U, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    struct MemoryGroup const object = lhs.group;
    struct MemoryGroup const value = rhs.group;
    paw_assert(object.count == 1);

    for (int i = 0; i < value.count; ++i) {
        MirRegister const r = REGISTER_AT(value, i);
        NEW_INSTR(U, set_field, TODO, lhs.field.offset + i, PLACE(object.base), PLACE(r));
    }
}

static void create_indirect_element_setter(struct Unboxer *U, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    enum BuiltinKind kind = pawP_type2code(U->C, lhs.type);
    int const field_offset = lhs.has_field ? lhs.field.offset : 0;
    IrType *element_type = lhs.element.type;
    struct IrLayout element_layout = pawIr_compute_layout(U->C, element_type);

    struct MirPlace const object = PLACE(lhs.group.base);
    if (element_layout.size > 1) {
        struct MirPlace const pointer = new_rawptr_place(U, element_type);
        NEW_INSTR(U, get_element_ptr, TODO, kind, pointer, object,
                PLACE(lhs.element.index), kind == BUILTIN_MAP && !lhs.has_field);

        for (int i = 0; i < rhs.group.count; ++i) {
            struct MirPlace const value = PLACE(REGISTER_AT(rhs.group, i));
            NEW_INSTR(U, set_field, TODO, field_offset + i, pointer, value);
        }
    } else {
        struct MirPlace const value = PLACE(REGISTER_AT(rhs.group, 0));
        NEW_INSTR(U, set_element, TODO, kind, object, PLACE(lhs.element.index), value);
    }
}

static void create_indirect_range_setter(struct Unboxer *U, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    enum BuiltinKind const kind = pawP_type2code(U->C, lhs.type);
    struct MirPlace const object = PLACE(lhs.group.base);
    struct MirPlace const value = PLACE(REGISTER_AT(rhs.group, 0));
    NEW_INSTR(U, set_range, TODO, kind, object, PLACE(lhs.range.lower), PLACE(lhs.range.upper), value);
}

static void create_upvalue_setter(struct Unboxer *U, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    struct MemoryGroup const value = rhs.group;

    for (int i = 0; i < value.count; ++i) {
        struct MirPlace const v = PLACE(REGISTER_AT(value, i));
        NEW_INSTR(U, set_upvalue, TODO, lhs.group.base + i, v);
    }
}

static void unbox_move(struct Unboxer *U, struct MirMove *x)
{
    struct MemoryAccess rhs = unbox_place(U, &x->target);
    struct MemoryAccess lhs = unbox_place(U, &x->output);
    discharge_access(U, &rhs); // put into registers

    if (lhs.has_range) {
        create_indirect_range_setter(U, lhs, rhs);
    } else if (lhs.has_element) {
        create_indirect_element_setter(U, lhs, rhs);
    } else if (lhs.has_field) {
        create_indirect_field_setter(U, lhs, rhs);
    } else if (lhs.group.kind == MEMORY_UPVALUE) {
        create_upvalue_setter(U, lhs, rhs);
    } else {
        create_move(U, lhs, rhs);
    }
}

static void unbox_upvalue(struct Unboxer *U, struct MirUpvalue *x)
{
    struct MemoryAccess output = unbox_place(U, &x->output);
    discharge_access(U, &output);

    for (int i = 0; i < output.group.count; ++i) {
        MirRegister const r = REGISTER_AT(output.group, i);
        NEW_INSTR(U, upvalue, x->loc, PLACE(r), x->index + i);
    }
}

static void unbox_alloclocal(struct Unboxer *U, struct MirAllocLocal *x)
{
    struct MemoryGroup const output = get_registers(U, x->output.r, ENUM_BASE);
    for (int i = 0; i < output.count; ++i) {
        MirRegister const r = REGISTER_AT(output, i);
        NEW_INSTR(U, alloc_local, x->loc, x->name, PLACE(r));
    }
}

static void unbox_setupvalue(struct Unboxer *U, struct MirSetUpvalue *x)
{
    struct MemoryGroup const value = get_registers(U, x->value.r, ENUM_BASE);
    for (int i = 0; i < value.count; ++i) {
        MirRegister const r = REGISTER_AT(value, i);
        NEW_INSTR(U, set_upvalue, x->loc, x->index + i, PLACE(r));
    }
}

static void unbox_aggregate(struct Unboxer *U, struct MirAggregate *x)
{
    struct Mir *mir = U->fs->mir;
    struct MirRegisterData *data = mir_reg_data(mir, x->output.r);
    if (!is_composite(U, data->type)) {
        struct MemoryGroup const output = get_registers(U, x->output.r, ENUM_BASE);
        paw_assert(output.count == 1);
        struct MirPlace const r = PLACE(output.base);
        NEW_INSTR(U, aggregate, x->loc, x->nfields, r);
        return;
    }

    struct MemoryGroup const output = get_registers(U, x->output.r, ENUM_BASE);
    for (int i = 0; i < output.count; ++i) {
        struct MirPlace const r = PLACE(REGISTER_AT(output, i));
        Str *name = pawP_format_string(U->C, PRIVATE("value_%d"), i);
        NEW_INSTR(U, alloc_local, x->loc, name, r);
    }
}

static void unbox_container(struct Unboxer *U, struct MirContainer *x)
{
    NEW_INSTR(U, container, x->loc, x->b_kind, x->nelems, x->output);
}

static void unbox_call(struct Unboxer *U, struct MirCall *x)
{
    struct Mir *mir = U->fs->mir;
    struct MemoryAccess callable = unbox_place(U, &x->target);
    discharge_access(U, &callable);

    paw_assert(callable.group.count == 1);
    struct MirPlace const target = PLACE(callable.group.base);

    struct MirPlace *pr;
    MirPlaceList *args = MirPlaceList_new(mir);
    K_LIST_FOREACH (x->args, pr) {
        struct MemoryAccess a = unbox_place(U, pr);
        discharge_access(U, &a);

        for (int i = 0; i < a.group.count; ++i) {
            MirRegister const r = REGISTER_AT(a.group, i);
            MirPlaceList_push(mir, args, PLACE(r));
        }
    }

    // split composite results into individual field values
    paw_assert(x->outputs->count == 1);
    struct MirPlace const first = K_LIST_FIRST(x->outputs);
    struct MemoryGroup const output = get_registers(U, first.r, ENUM_BASE);
    MirPlaceList *outputs = MirPlaceList_new(mir);
    MirPlaceList_reserve(mir, outputs, output.count);
    outputs->count = output.count;
    for (int i = 0; i < output.count; ++i) {
        MirRegister const r = REGISTER_AT(output, i);
        MirPlaceList_set(outputs, i, PLACE(r));
    }

    NEW_INSTR(U, call, x->loc, target, args, outputs);
}


static void unbox_capture(struct Unboxer *U, struct MirCapture *x)
{
    struct MemoryGroup const target = get_registers(U, x->target.r, ENUM_BASE);
    for (int i = 0; i < target.count; ++i) {
        MirRegister const r = REGISTER_AT(target, i);
        NEW_INSTR(U, capture, x->loc, PLACE(r));
    }
}

static void unbox_close(struct Unboxer *U, struct MirClose *x)
{
    struct MemoryGroup const target = get_registers(U, x->target.r, ENUM_BASE);
    for (int i = 0; i < target.count; ++i) {
        MirRegister const r = REGISTER_AT(target, i);
        NEW_INSTR(U, close, x->loc, PLACE(r));
    }
}

static void unbox_unaryop(struct Unboxer *U, struct MirUnaryOp *x)
{
    struct MemoryAccess value = unbox_place(U, &x->val);
    discharge_access(U, &value);
    paw_assert(value.group.count == 1);
    x->val = PLACE(value.group.base);

    struct MemoryAccess output = unbox_place(U, &x->output);
    discharge_access(U, &output);
    paw_assert(output.group.count == 1);
    x->output = PLACE(output.group.base);

    x->mid = next_mid(U->fs);
    NEW_INSTR(U, unary_op, x->loc, x->op, x->val, x->output);
}

static void unbox_closure(struct Unboxer *U, struct MirClosure *x)
{
    x->mid = next_mid(U->fs);
    NEW_INSTR(U, closure, x->loc, x->child_id, x->output);
}

static void unbox_return(struct Unboxer *U, struct MirReturn *x)
{
    struct Mir *mir = U->fs->mir;
    paw_assert(x->values->count == 1);
    struct MirPlace *first = &K_LIST_FIRST(x->values);
    struct MemoryAccess value = unbox_place(U, first);
    discharge_access(U, &value);

    MirPlaceList *values = MirPlaceList_new(mir);
    MirPlaceList_reserve(mir, values, value.group.count);
    values->count = value.group.count;
    for (int i = 0; i < value.group.count; ++i) {
        MirRegister const r = REGISTER_AT(value.group, i);
        MirPlaceList_set(values, i, PLACE(r));
    }

    NEW_INSTR(U, return, x->loc, values);
}

// Handle instructions that are known to operate on scalars
static void unbox_other(struct Unboxer *U, struct MirInstruction *instr)
{
    struct Mir *mir = U->fs->mir;
    struct MirPlace *const *ppp;
    MirPlacePtrList *loads = pawMir_get_loads(mir, instr);
    K_LIST_FOREACH (loads, ppp) {
        struct MemoryAccess a = unbox_place(U, *ppp);
        discharge_access(U, &a);
        paw_assert(a.group.count == 1);
        (*ppp)->r.value = a.group.base;
    }

    MirPlacePtrList *const stores = pawMir_get_stores(mir, instr);
    K_LIST_FOREACH (stores, ppp) {
        struct MemoryAccess a = unbox_place(U, *ppp);
        discharge_access(U, &a);
        paw_assert(a.group.count == 1);
        (*ppp)->r.value = a.group.base;
    }

    instr->hdr.mid = next_mid(U->fs);
    MirInstructionList_push(U->fs->mir, U->instructions, instr);
}

static void unbox_instruction(struct Unboxer *U, struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirAllocLocal:
            unbox_alloclocal(U, MirGetAllocLocal(instr));
            break;
        case kMirMove:
            unbox_move(U, MirGetMove(instr));
            break;
        case kMirCall:
            unbox_call(U, MirGetCall(instr));
            break;
        case kMirReturn:
            unbox_return(U, MirGetReturn(instr));
            break;
        case kMirAggregate:
            unbox_aggregate(U, MirGetAggregate(instr));
            break;
        case kMirUpvalue:
            unbox_upvalue(U, MirGetUpvalue(instr));
            break;
        case kMirSetUpvalue:
            unbox_setupvalue(U, MirGetSetUpvalue(instr));
            break;
        case kMirCapture:
            unbox_capture(U, MirGetCapture(instr));
            break;
        case kMirClose:
            unbox_close(U, MirGetClose(instr));
            break;
        case kMirUnaryOp:
            unbox_unaryop(U, MirGetUnaryOp(instr));
            break;
        case kMirGoto: {
            struct MirGoto *x = MirGetGoto(instr);
            NEW_INSTR(U, goto, x->loc, x->target);
            break;
        }
        default:
            unbox_other(U, instr);
    }
}

static void unbox_function(struct Unboxer *U, struct Mir *mir)
{
    struct FunctionState fs = {
        .registers = MirRegisterDataList_new(mir),
        .upvalues = MirUpvalueList_new(mir),
        .local_map = LocalGroupMap_new(U),
        .upvalue_map = UpvalueGroupMap_new(U),
        .old_locals = mir->locals,
        .outer = U->fs,
        .mir = mir,
    };
    U->fs = &fs;

    MirRegister const *pr;
    MirRegisterList *locals = MirRegisterList_new(mir);
    K_LIST_FOREACH (mir->locals, pr) {
        struct MemoryGroup const group = get_registers(U, *pr, ENUM_BASE);
        for (int i = 0; i < group.count; ++i) {
            MirRegisterList_push(mir, locals, REGISTER_AT(group, i));
        }
    }

    struct MirCaptureInfo const *pc;
    MirCaptureList *captured = MirCaptureList_new(mir);
    K_LIST_FOREACH (mir->captured, pc) {
        struct MemoryGroup const group = get_registers(U, pc->r, ENUM_BASE);
        for (int i = 0; i < group.count; ++i) {
            MirCaptureList_push(mir, captured, (struct MirCaptureInfo){
                        .r = REGISTER_AT(group, i),
                    });
        }
    }

    int index;
    struct MirUpvalueInfo const *pu;
    struct FunctionState *outer = fs.outer;
    K_LIST_ENUMERATE (mir->upvalues, index, pu) {
        struct MemoryGroup const *backing;
        if (pu->is_local) {
            // upvalue is local to enclosing function
            MirRegister const local = MirRegisterList_get(outer->old_locals, pu->index);
            backing = LocalGroupMap_get(U, outer->local_map, local);

            int local_id;
            MirRegister const *plocal;
            K_LIST_ENUMERATE(outer->mir->locals, local_id, plocal) {
                if (plocal->value == backing->base)
                    break;
            }
            paw_assert(local_id < outer->mir->locals->count);
            new_local_upvalues(U, index, *backing, local_id);
        } else {
            // upvalue is also an upvalue in the enclosing function
            backing = UpvalueGroupMap_get(U, outer->upvalue_map, pu->index);
            new_nonlocal_upvalues(U, index, *backing);
        }
    }

    mir->locals = locals;
    mir->captured = captured;

    // unbox the current function
    struct MirBlockData *const *pbb;
    struct MirInstruction *const *pinstr;
    K_LIST_FOREACH (mir->blocks, pbb) {
        struct MirBlockData *const bb = *pbb;
        bb->mid = next_mid(&fs);
        U->instructions = MirInstructionList_new(mir);
        K_LIST_FOREACH (bb->instructions, pinstr)
            unbox_instruction(U, *pinstr);
        bb->instructions = U->instructions;
    }
    mir->registers = fs.registers;
    mir->upvalues = fs.upvalues;

    // unbox closures
    struct Mir *const *pchild;
    K_LIST_FOREACH (mir->children, pchild){
        unbox_function(U, *pchild);
    }

    U->fs = fs.outer;
}

void pawP_scalarize_registers(struct Compiler *C, struct Mir *mir)
{
    struct Unboxer U = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .P = ENV(C),
        .C = C,
    };

    mir->mir_count = 0;
    unbox_function(&U, mir);
}

