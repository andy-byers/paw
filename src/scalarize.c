// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// scalarize.c: Make all virtual registers have a size of 1
//
// The process involves splitting registers of size N into N registers of size
// 1, and adding move instructions to account for the extra registers. The
// register allocator and code generation module expect each virtual register
// to correspond to a single actual register in a runtime activation frame.

#include "ir_type.h"
#include "layout.h"
#include "mir.h"

#warning"remove"
#include"stdio.h"

// TODO: store source location for projections? or at least pass in the source loc of the expression
#define TODO ((struct SourceLoc){-1})

#define ADD_INSTRUCTION(Ctx_, Instr_) \
    MirInstructionList_push((Ctx_)->fs->mir, (Ctx_)->instructions, MIR_CAST_INSTRUCTION(Instr_));

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

struct Scalarizer {
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

//  access kind | object kind | location | updates
// -------------|-------------|----------|---------
//  field       | value       | stack    | .group
//  field       | value       | nested   | .field
//  field       | reference   | stack    | .field
//  field       | reference   | nested   | .field (pushes old field)
//
// Example:
//   Source code:
//
//     let pair = Pair{
//         a: (1, 2),
//         b: (3, (4, 5)),
//     };
//     pair.b.1.1 = 123;
//
//   Memory accesses:
//
//     a: 1
//        2
//     b: 3
//        4
//        5 <- 123
//
//     (1) pair.b
//         .type: Pair
//         .group: _0
//         .kind: ACCESS_FIELD
//         .field.type: (int, (int, int))
//         .field.index: 1
//         .field.offset: 0
//     (1) pair.b.1
//         .type: Pair
//         .group: _0
//         .kind: ACCESS_FIELD
//         .field.type: (int, int)
//         .field.index: 1
//         .field.offset: 2
//     (1) pair.b.1.1
//         .type: Pair
//         .group: _0
//         .kind: ACCESS_FIELD
//         .field.type: int
//         .field.index: 1
//         .field.offset: 3
//
// container[element].field  getelement(s)
// value.field[element]      refine group, getelement(s)
// container[element].field.field[element]
//
// All memory accesses target objects located on the runtime stack. Accessing a field
// on a value type located on the stack is translated into a move. Field accesses on
// boxed objects are translated into (GET | SET)FIELD instructions.
struct MemoryAccess {
    struct MemoryGroup group;
    IrType *type;

    paw_Bool has_field : 1;
    paw_Bool has_element : 1;

    struct FieldAccess {
        IrType *type;
        int offset;
    } field;

    struct ElementAccess {
        IrType *type;
        MirRegister index;
        int offset;
    } element;
};

DEFINE_MAP(struct Scalarizer, LocalGroupMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirRegister, struct MemoryGroup)
DEFINE_MAP(struct Scalarizer, UpvalueGroupMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, int, struct MemoryGroup)
DEFINE_LIST(struct Scalarizer, MemoryGroupList, struct MemoryGroup)

static int size_on_stack(struct IrLayout layout)
{
    return layout.is_boxed ? 1 : layout.size;
}

void dump_access(struct Scalarizer *S, struct MemoryAccess *pa)
{
    printf("access {\n");
    printf("  type: %s\n", pawIr_print_type(S->C, pa->type));
    printf("  group: %s%d:%s%d\n",pa->group.kind==MEMORY_UPVALUE?"up":"_", pa->group.base,pa->group.kind==MEMORY_UPVALUE?"up":"_", pa->group.base+pa->group.count);
    if (pa->has_field) {
        printf("  field_offset: %d\n", pa->field.offset);
        printf("  field_type: %s\n", pawIr_print_type(S->C, pa->field.type));
    }
    if (pa->has_element) {
        printf("  element_index: _%d (offset=%d)\n", pa->element.index.value, pa->element.offset);
        printf("  element_type: %s\n", pawIr_print_type(S->C, pa->element.type));
    }
}

typedef void (*TraverseField)(struct Scalarizer *S, IrType *type);
static int traverse_scalar_fields(struct Scalarizer *S, IrType *type, TraverseField traverse)
{
    struct FunctionState *fs = S->fs;
    struct IrTypeList *types;
    if (IrIsTuple(type)) {
        types = IrGetTuple(type)->elems;
    } else {
        // scalar or boxed type
        traverse(S, type);
        return 1;
    }
    int count = 0;
    struct IrType *const *ptype;
    K_LIST_FOREACH (types, ptype)
        count += traverse_scalar_fields(S, *ptype, traverse);
    return count;
}

static int alloc_scalar_registers(struct Scalarizer *S, struct IrType *type, paw_Bool is_captured, paw_Bool is_uninit)
{
    struct FunctionState *fs = S->fs;
    struct IrTypeList *types;
    if (IrIsTuple(type)) {
        types = IrGetTuple(type)->elems;
    } else {
        // scalar or boxed type
        MirRegisterDataList_push(fs->mir, fs->registers,
                (struct MirRegisterData){
                    .is_captured = is_captured,
                    .is_uninit = is_uninit,
                    .type = type,
                    .size = 1,
                });
        return 1;
    }
    int count = 0;
    struct IrType *const *ptype;
    K_LIST_FOREACH (types, ptype)
        count += alloc_scalar_registers(S, *ptype, is_captured, is_uninit);
    return count;
}

static int alloc_scalar_upvalues(struct Scalarizer *S, struct IrType *type, int up, paw_Bool is_local)
{
    struct FunctionState *fs = S->fs;
    struct IrTypeList *types;
    if (IrIsTuple(type)) {
        types = IrGetTuple(type)->elems;
    } else {
        // scalar or boxed type
        MirUpvalueList_push(fs->mir, fs->upvalues,
                (struct MirUpvalueInfo){.index = up, .type = type, .is_local = is_local});
        return 1;
    }
    int count = 0;
    struct IrType *const *ptype;
    K_LIST_FOREACH (types, ptype)
        count += alloc_scalar_upvalues(S, *ptype, up + 1, is_local);
    return count;
}

// TODO: combine with alloc_scalar_registers
static struct MemoryGroup new_registers(struct Scalarizer *S, IrType *type)
{
    int const num_registers = S->fs->registers->count;
    int const count = alloc_scalar_registers(S, type, PAW_FALSE, PAW_FALSE);
    return (struct MemoryGroup){
        .base = num_registers,
        .count = count,
    };
}

static struct MemoryGroup get_registers(struct Scalarizer *S, MirRegister r)
{
    struct FunctionState *fs = S->fs;
    struct MemoryGroup *pgroup = LocalGroupMap_get(S, fs->local_map, r);
    if (pgroup != NULL)
        return *pgroup;

    struct MirRegisterData *data = mir_reg_data(fs->mir, r);
    int const num_registers = fs->registers->count;
    int const count = alloc_scalar_registers(S, data->type, data->is_captured, data->is_uninit);
    struct MemoryGroup group = {
        .kind = MEMORY_LOCAL,
        .base = num_registers,
        .count = count,
    };
    LocalGroupMap_insert(S, fs->local_map, r, group);
    return group;
}

static struct MemoryGroup get_upvalues(struct Scalarizer *S, int up)
{
    struct FunctionState *fs = S->fs;
    struct MemoryGroup *pgroup = UpvalueGroupMap_get(S, fs->upvalue_map, up);
    paw_assert(pgroup != NULL);
    return *pgroup;
}

static struct MemoryGroup new_nonlocal_upvalues(struct Scalarizer *S, int up, struct MemoryGroup mg)
{
    struct FunctionState *fs = S->fs;
    struct MirUpvalueInfo info = MirUpvalueList_get(fs->mir->upvalues, up);
    int const num_upvalues = fs->upvalues->count;
    int const count = alloc_scalar_upvalues(S, info.type, mg.base, mg.kind == MEMORY_LOCAL);
    struct MemoryGroup group = {
        .kind = MEMORY_UPVALUE,
        .base = num_upvalues,
        .count = count,
    };
    UpvalueGroupMap_insert(S, fs->upvalue_map, up, group);
    return group;
}

// TODO: local variable number needs to be updated since some locals need to be expanded
static struct MemoryGroup new_local_upvalues(struct Scalarizer *S, int up, struct MemoryGroup mg, int local)
{
    struct FunctionState *fs = S->fs;
    struct MirUpvalueInfo info = MirUpvalueList_get(fs->mir->upvalues, up);
    int const num_upvalues = fs->upvalues->count;
    int const count = alloc_scalar_upvalues(S, info.type, local, mg.kind == MEMORY_LOCAL);
    struct MemoryGroup group = {
        .kind = MEMORY_UPVALUE,
        .base = num_upvalues,
        .count = count,
    };
    UpvalueGroupMap_insert(S, fs->upvalue_map, up, group);
    return group;
}

static struct MemoryGroup get_location(struct Scalarizer *S, struct MirPlace p)
{
    if (p.kind == MIR_PLACE_UPVALUE)
        return get_upvalues(S, p.up);
    return get_registers(S, p.r);
}

static struct MemoryGroup split_group(struct MemoryGroup group, int offset, int count)
{
    paw_assert(offset <= group.count - count);
    return (struct MemoryGroup){
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
        offset += size_on_stack(field);
    }
    return offset;
}

static IrTypeList *get_field_types(struct Scalarizer *S, IrType *parent)
{
    if (IrIsTuple(parent))
        return IrGetTuple(parent)->elems;

    struct IrAdt *adt = IrGetAdt(parent);
    struct IrAdtDef *def = pawIr_get_adt_def(S->C, adt->did);
    if (def->is_struct) {
        return pawP_instantiate_struct_fields(S->C, IrGetAdt(parent));
    } else {
        return pawP_instantiate_variant_fields(S->C, IrGetAdt(parent), 0); // TODO need discriminator
    }
}

// Handles the following type(s) of memory accesses:
//     object.*.field
static void discharge_indirect_field(struct Scalarizer *S, struct MemoryAccess *pa)
{
    IrTypeList const *field_types = get_field_types(S, pa->type);
    struct IrLayout target_layout = pawIr_compute_layout(S->C, pa->type);
    struct IrLayout field_layout = pawIr_compute_layout(S->C, pa->field.type);
    struct MemoryGroup output = new_registers(S, pa->field.type);

    struct MirPlace const object = PLACE(pa->group.base);
    for (int i = 0; i < output.count; ++i) {
        struct MirPlace const result = PLACE(REGISTER_AT(output, i));
        ADD_INSTRUCTION(S, pawMir_new_get_field(S->fs->mir, TODO,
                    pa->field.offset + i, result, object));
    }

    pa->group = output;
    pa->type = pa->field.type;
    pa->has_field = PAW_FALSE;
    pa->field = (struct FieldAccess){0};
}

static enum BuiltinKind get_container_kind(IrType *type)
{
    struct IrAdt *adt = IrGetAdt(type);
    return adt->types->count == 1
        ? BUILTIN_LIST
        : BUILTIN_MAP;
}

static IrType *get_element_type(IrType *type)
{
    struct IrAdt *adt = IrGetAdt(type);
    return adt->types->count == 1
        ? IrTypeList_get(adt->types, 0) // List<T>
        : IrTypeList_get(adt->types, 1); // Map<K, V>
}

static IrType *builtin_type(struct Scalarizer *S, enum BuiltinKind kind)
{
    return pawIr_get_def_type(S->C, S->C->builtins[kind].did);
}

static MirRegister discharge_element_base(struct Scalarizer *S, MirRegister index, int factor)
{
    if (factor == 1) return index;
    IrType *type = pawIr_get_def_type(S->C, S->C->builtins[BUILTIN_INT].did);
    MirRegister const k = MIR_REG(new_registers(S, builtin_type(S, BUILTIN_INT)).base);
    MirRegister const r = MIR_REG(new_registers(S, builtin_type(S, BUILTIN_INT)).base);
    ADD_INSTRUCTION(S, pawMir_new_constant(S->fs->mir, TODO,
                BUILTIN_INT, I2V(factor), PLACE(k)));
    ADD_INSTRUCTION(S, pawMir_new_binary_op(S->fs->mir, TODO,
                BINARY_MUL, PLACE(index), PLACE(k), PLACE(r)));
    return r;
}

static MirRegister discharge_element_offset(struct Scalarizer *S, MirRegister index, int offset)
{
    if (offset == 0) return index;
    IrType *type = pawIr_get_def_type(S->C, S->C->builtins[BUILTIN_INT].did);
    MirRegister const k = MIR_REG(new_registers(S, builtin_type(S, BUILTIN_INT)).base);
    MirRegister const r = MIR_REG(new_registers(S, builtin_type(S, BUILTIN_INT)).base);
    ADD_INSTRUCTION(S, pawMir_new_constant(S->fs->mir, TODO,
                BUILTIN_INT, I2V(offset), PLACE(k)));
    ADD_INSTRUCTION(S, pawMir_new_binary_op(S->fs->mir, TODO,
                BINARY_ADD, PLACE(index), PLACE(k), PLACE(r)));
    return r;
}

static IrType *get_access_type(struct MemoryAccess *pa)
{
    return pa->has_field ? pa->field.type :
        pa->has_element ? pa->element.type :
        pa->type;
}

// Handles the following type(s) of memory accesses:
//     container[index]
//     container[index].field
//     container[index].field.field2
static void discharge_indirect_index(struct Scalarizer *S, struct MemoryAccess *pa)
{
    enum BuiltinKind kind = get_container_kind(pa->type);


    IrType *element_type = pa->element.type;
    IrType *access_type = get_access_type(pa);
    struct IrLayout element_layout = pawIr_compute_layout(S->C, pa->element.type);
    int const field_offset = pa->has_field ? pa->field.offset : 0;
    struct MemoryGroup output = new_registers(S, access_type);

    struct MirPlace const object = PLACE(pa->group.base);
    MirRegister const base = discharge_element_base(S, pa->element.index, element_layout.size);
    for (int i = 0; i < output.count; ++i) {
        struct MirPlace const result = PLACE(REGISTER_AT(output, i));
        MirRegister const index = discharge_element_offset(S, base, field_offset + i);
        ADD_INSTRUCTION(S, pawMir_new_get_element(S->fs->mir, TODO,
                    kind, result, object, PLACE(index)));
    }

    pa->group = output;
    pa->type = access_type;
    pa->has_field = PAW_FALSE;
    pa->has_element = PAW_FALSE;
    pa->field = (struct FieldAccess){0};
    pa->element = (struct ElementAccess){0};
}

static void discharge_upvalue(struct Scalarizer *S, struct MemoryAccess *pa)
{
    struct MemoryGroup output = new_registers(S, pa->type);
    for (int i = 0; i < pa->group.count; ++i) {
        struct MirPlace const result = PLACE(REGISTER_AT(output, i));
        ADD_INSTRUCTION(S, pawMir_new_upvalue(S->fs->mir, TODO, result, UPVALUE_AT(pa->group, i)));
    }
    pa->group = output;
}

// Place the result of a memory access into a register group
// Used when a nested indirect access is required, since the object being accessed must
// be in a register.
static void discharge_access(struct Scalarizer *S, struct MemoryAccess *pa)
{
    if (pa->group.kind == MEMORY_UPVALUE)
        discharge_upvalue(S, pa);
    if (pa->has_element) {
        discharge_indirect_index(S, pa);
    } else if (pa->has_field) {
        discharge_indirect_field(S, pa);
    }
}

// Apply a direct projection (no indirection)
static void apply_access(struct Scalarizer *S, struct MemoryAccess *pa, MirProjection *p)
{
    struct MirField *field = MirGetField(p);
    if (pa->has_element) {
        // TODO: maybe combine with ".has_field" case
        IrType *target_type = get_access_type(pa);
        IrTypeList const *field_types = get_field_types(S, target_type);
        IrType *field_type = IrTypeList_get(field_types, field->index);
        struct IrLayout target_layout = pawIr_compute_layout(S->C, target_type);

        pa->field.offset += compute_field_offset(target_layout, field->index);
        pa->field.type = field_type;
        pa->has_field = PAW_TRUE;
    } else if (pa->has_field) {
        IrTypeList const *field_types = get_field_types(S, pa->field.type);
        IrType *field_type = IrTypeList_get(field_types, field->index);
        struct IrLayout target_layout = pawIr_compute_layout(S->C, pa->field.type);

        // Handle field access on a value type nested in another object.
        pa->field.offset += compute_field_offset(target_layout, field->index);
        pa->field.type = field_type;
    } else {
        IrTypeList const *field_types = get_field_types(S, pa->type);
        IrType *field_type = IrTypeList_get(field_types, field->index);
        struct IrLayout target_layout = pawIr_compute_layout(S->C, pa->type);
        struct IrLayout field_layout = IrLayoutList_get(target_layout.fields, field->index);

        // Handle field access on a value type located on the stack.
        int const offset = compute_field_offset(target_layout, field->index);
        pa->group = split_group(pa->group, offset, size_on_stack(field_layout));
        pa->type = field_type;
    }
}

// Apply an indirect projection
static void apply_indirect_access(struct Scalarizer *S, struct MemoryAccess *pa, MirProjection *p)
{
    discharge_access(S, pa);

    if (MirIsField(p)) {
        struct MirField *field = MirGetField(p);

        struct IrLayout target_layout = pawIr_compute_layout(S->C, pa->type);
        IrTypeList const *field_types = get_field_types(S, pa->type);
        pa->field.type = IrTypeList_get(field_types, field->index);
        pa->field.offset = compute_field_offset(target_layout, field->index);
        pa->has_field = PAW_TRUE;
    } else {
        struct MirIndex *index = MirGetIndex(p);

        struct MemoryGroup index_group = get_registers(S, index->index);
        paw_assert(index_group.count == 1);
        pa->element.index.value = index_group.base;
        pa->element.type = get_element_type(pa->type);
        pa->has_element = PAW_TRUE;
    }
}

// Apply the projections associated with the given place
// Essentially, this function determines the memory location referred to by a MIR place
// construct. A memory location can be either a set of virtual registers (each with a size
// of 1 Paw value), or a contiguous group of values inside another object.
static struct MemoryAccess scalarize_place(struct Scalarizer *S, struct MirPlace *pplace)
{
    struct MemoryAccess access = {
        .group = get_location(S, *pplace),
    };
    if (pplace->kind == MIR_PLACE_LOCAL) {
        access.type = mir_reg_data(S->fs->mir, pplace->r)->type;
    } else {
        access.type = MirUpvalueList_get(S->fs->mir->upvalues, pplace->up).type;
    }
    MirProjectionList const *ps = pplace->projection;
    if (ps == NULL) // TODO: should never be true at this point
        return access;

    for (int i = 0; i < ps->count;) {
        MirProjection *p = MirProjectionList_get(ps, i++);
        if (MirIsDeref(p)) {
            // handle indirect accesses
            paw_assert(i < ps->count);
            p = MirProjectionList_get(ps, i++);
            apply_indirect_access(S, &access, p);
        } else {
            // handle direct field access
            apply_access(S, &access, p);
        }
    }

    // projections have been transformed into instructions
// TODO: breaks things for some reason ("trash_memory" overwrites something important, indicating a memory error somewhere)
// TODO:    MirProjectionList_delete(S->fs->mir, pplace->projection);
    pplace->projection = NULL;
    return access;
}

static void create_move(struct Scalarizer *S, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    struct MemoryGroup object = lhs.group;
    struct MemoryGroup value = rhs.group;

    struct IrLayout layout = pawIr_compute_layout(S->C, lhs.type);
    for (int i = 0; i < value.count; ++i) {
        MirRegister const a = REGISTER_AT(value, i);
        MirRegister const b = REGISTER_AT(object, lhs.field.offset + i);
        ADD_INSTRUCTION(S, pawMir_new_move(S->fs->mir, TODO, PLACE(b), PLACE(a)));
    }
}

static void create_field_setter(struct Scalarizer *S, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    struct MemoryGroup object = lhs.group;
    struct MemoryGroup value = rhs.group;

    struct IrLayout layout = pawIr_compute_layout(S->C, lhs.type);
    for (int i = 0; i < value.count; ++i) {
        MirRegister const a = REGISTER_AT(value, i);
        MirRegister const b = REGISTER_AT(object, lhs.field.offset + i);
        ADD_INSTRUCTION(S, pawMir_new_move(S->fs->mir, TODO, PLACE(b), PLACE(a)));
    }
}

static void create_indirect_field_setter(struct Scalarizer *S, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    struct MemoryGroup object = lhs.group;
    struct MemoryGroup value = rhs.group;
    paw_assert(object.count == 1);

    struct IrLayout layout = pawIr_compute_layout(S->C, lhs.type);
    for (int i = 0; i < value.count; ++i) {
        MirRegister const r = REGISTER_AT(value, i);
        ADD_INSTRUCTION(S, pawMir_new_set_field(S->fs->mir, TODO,
                    lhs.field.offset + i, PLACE(object.base), PLACE(r)));
    }
}

static void create_indirect_element_setter(struct Scalarizer *S, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
//    struct MemoryGroup object = lhs.group;
//    struct MemoryGroup value = rhs.group;
//    paw_assert(object.count == 1);
//
//    struct IrLayout layout = pawIr_compute_layout(S->C, lhs.type);
//    for (int i = 0; i < value.count; ++i) {
//        MirRegister const r = REGISTER_AT(value, i);
//        ADD_INSTRUCTION(S, pawMir_new_set_element(S->fs->mir, TODO,
//                    lhs.field.offset + i, PLACE(object.base), PLACE(r)));
//    }
    enum BuiltinKind kind = get_container_kind(lhs.type);
    IrType *element_type = lhs.element.type;
    IrType *access_type = get_access_type(&lhs);
    struct IrLayout element_layout = pawIr_compute_layout(S->C, lhs.element.type);
    int const field_offset = lhs.has_field ? lhs.field.offset : 0;
    struct MemoryGroup output = new_registers(S, access_type);

    struct MirPlace const object = PLACE(lhs.group.base);
    MirRegister const base = discharge_element_base(S, lhs.element.index, element_layout.size);
    for (int i = 0; i < output.count; ++i) {
        struct MirPlace const value = PLACE(REGISTER_AT(rhs.group, i));
        struct MirPlace const result = PLACE(REGISTER_AT(output, i));
        MirRegister const index = discharge_element_offset(S, base, field_offset + i);
        ADD_INSTRUCTION(S, pawMir_new_set_element(S->fs->mir, TODO,
                    kind, object, PLACE(index), value));
    }
}

static void create_upvalue_setter(struct Scalarizer *S, struct MemoryAccess lhs, struct MemoryAccess rhs)
{
    struct MemoryGroup object = lhs.group;
    struct MemoryGroup value = rhs.group;

    struct IrLayout layout = pawIr_compute_layout(S->C, lhs.type);
    for (int i = 0; i < value.count; ++i) {
        MirRegister const v = REGISTER_AT(value, i);
        ADD_INSTRUCTION(S, pawMir_new_set_upvalue(S->fs->mir, TODO, lhs.group.base + i, PLACE(v)));
    }
}

static void scalarize_move(struct Scalarizer *S, struct MirMove *x)
{
    struct MemoryAccess rhs = scalarize_place(S, &x->target);
    struct MemoryAccess lhs = scalarize_place(S, &x->output);
    discharge_access(S, &rhs); // put into registers

    if (lhs.has_element) {
        create_indirect_element_setter(S, lhs, rhs);
    } else if (lhs.has_field) {
        create_indirect_field_setter(S, lhs, rhs);
    } else if (lhs.group.kind == MEMORY_UPVALUE) {
        create_upvalue_setter(S, lhs, rhs);
    } else if (lhs.has_field) {
        create_move(S, lhs, rhs);
    }
}

static void scalarize_upvalue(struct Scalarizer *S, struct MirUpvalue *x)
{
    struct MemoryAccess output = scalarize_place(S, &x->output);
    discharge_access(S, &output);

    for (int i = 0; i < output.group.count; ++i) {
        MirRegister const r = REGISTER_AT(output.group, i);
        struct MirInstruction *instr = pawMir_new_upvalue(S->fs->mir, x->loc, PLACE(r), x->index + i);
        ADD_INSTRUCTION(S, instr);
    }
}

static void scalarize_alloclocal(struct Scalarizer *S, struct MirAllocLocal *x)
{
    struct MemoryGroup output = get_registers(S, x->output.r);
    for (int i = 0; i < output.count; ++i) {
        MirRegister const r = REGISTER_AT(output, i);
        struct MirInstruction *instr = pawMir_new_alloc_local(S->fs->mir, x->loc, x->name, PLACE(r));
        ADD_INSTRUCTION(S, instr);
    }
}

static void scalarize_setupvalue(struct Scalarizer *S, struct MirSetUpvalue *x)
{
    struct MemoryGroup value = get_registers(S, x->value.r);
    for (int i = 0; i < value.count; ++i) {
        MirRegister const r = REGISTER_AT(value, i);
        struct MirInstruction *instr = pawMir_new_set_upvalue(S->fs->mir, x->loc, x->index + i, PLACE(r));
        ADD_INSTRUCTION(S, instr);
    }
}

static paw_Bool is_unary(IrType *type)
{
    if (IrIsTuple(type))
        return PAW_FALSE;

    // TODO: unbox other types
    return PAW_TRUE;
}

static void scalarize_aggregate(struct Scalarizer *S, struct MirAggregate *x)
{
    struct MirRegisterData *data = mir_reg_data(S->fs->mir, x->output.r);
    if (is_unary(data->type)) {
        struct MemoryGroup output = get_registers(S, x->output.r);
        paw_assert(output.count == 1);
        x->output = PLACE(output.base);
        ADD_INSTRUCTION(S, x);
        return;
    }

    struct MemoryGroup output = get_registers(S, x->output.r);
    for (int i = 0; i < output.count; ++i) {
        MirRegister const r = REGISTER_AT(output, i);
        struct MirInstruction *instr = pawMir_new_alloc_local(S->fs->mir, x->loc, SCAN_STRING(S->C, "TODO: remove this field"), PLACE(r));
        ADD_INSTRUCTION(S, instr);
    }
}

static void scalarize_container(struct Scalarizer *S, struct MirContainer *x)
{

    ADD_INSTRUCTION(S, x);
}

static void scalarize_call(struct Scalarizer *S, struct MirCall *x)
{
    struct MemoryGroup target = get_registers(S, x->target.r);
    paw_assert(target.count == 1);
    x->target = PLACE(target.base);

    struct MirPlace *pr;
    MirPlaceList *args = MirPlaceList_new(S->fs->mir);
    K_LIST_FOREACH (x->args, pr) {
        struct MemoryAccess a = scalarize_place(S, pr);
        discharge_access(S, &a);

        for (int i = 0; i < a.group.count; ++i) {
            MirRegister const r = REGISTER_AT(a.group, i);
            MirPlaceList_push(S->fs->mir, args, PLACE(r));
        }
    }
    x->args = args;

    paw_assert(x->outputs->count == 1);
    struct MemoryGroup output = get_registers(S, K_LIST_FIRST(x->outputs).r);
    MirPlaceList_reserve(S->fs->mir, x->outputs, output.count);
    x->outputs->count = output.count;
    for (int i = 0; i < output.count; ++i) {
        MirRegister const r = REGISTER_AT(output, i);
        MirPlaceList_set(x->outputs, i, PLACE(r));
    }

    ADD_INSTRUCTION(S, x);
}


static void scalarize_capture(struct Scalarizer *S, struct MirCapture *x)
{
    struct MemoryGroup target = get_registers(S, x->target.r);
    for (int i = 0; i < target.count; ++i) {
        MirRegister const r = REGISTER_AT(target, i);
        struct MirInstruction *instr = pawMir_new_capture(S->fs->mir, x->loc, PLACE(r));
        ADD_INSTRUCTION(S, instr);
    }
}

static void scalarize_close(struct Scalarizer *S, struct MirClose *x)
{
    struct MemoryGroup target = get_registers(S, x->target.r);
    for (int i = 0; i < target.count; ++i) {
        MirRegister const r = REGISTER_AT(target, i);
        struct MirInstruction *instr = pawMir_new_close(S->fs->mir, x->loc, PLACE(r));
        ADD_INSTRUCTION(S, instr);
    }
}

static void scalarize_closure(struct Scalarizer *S, struct MirClosure *x)
{

    ADD_INSTRUCTION(S, x);
}

static void scalarize_return(struct Scalarizer *S, struct MirReturn *x)
{
    paw_assert(x->values->count == 1);
    struct MemoryAccess value = scalarize_place(S, &K_LIST_FIRST(x->values));
    discharge_access(S, &value);

    MirPlaceList_reserve(S->fs->mir, x->values, value.group.count);
    x->values->count = value.group.count;
    for (int i = 0; i < value.group.count; ++i) {
        MirRegister const r = REGISTER_AT(value.group, i);
        MirPlaceList_set(x->values, i, PLACE(r));
    }

    ADD_INSTRUCTION(S, x);
}

// Handle instructions that are known to operate on scalars
static void scalarize_other(struct Scalarizer *S, struct MirInstruction *instr)
{
    struct MirPlace *const *ppp;
    MirPlacePtrList *loads = pawMir_get_loads_v2(S->fs->mir, instr);
    K_LIST_FOREACH (loads, ppp) {
        struct MemoryAccess a = scalarize_place(S, *ppp);
        discharge_access(S, &a);
        paw_assert(a.group.count == 1);
        (*ppp)->r.value = a.group.base;
    }

    MirPlacePtrList *const stores = pawMir_get_stores_v2(S->fs->mir, instr);
    K_LIST_FOREACH (stores, ppp) {
        struct MemoryAccess a = scalarize_place(S, *ppp);
        discharge_access(S, &a);
        paw_assert(a.group.count == 1);
        (*ppp)->r.value = a.group.base;
    }

    ADD_INSTRUCTION(S, instr);
}

static void scalarize_instruction(struct Scalarizer *S, struct MirInstruction *instr)
{
    switch (MIR_KINDOF(instr)) {
        case kMirAllocLocal:
            scalarize_alloclocal(S, MirGetAllocLocal(instr));
            break;
        case kMirMove:
            scalarize_move(S, MirGetMove(instr));
            break;
        case kMirCall:
            scalarize_call(S, MirGetCall(instr));
            break;
        case kMirReturn:
            scalarize_return(S, MirGetReturn(instr));
            break;
        case kMirAggregate:
            scalarize_aggregate(S, MirGetAggregate(instr));
            break;
        case kMirUpvalue:
            scalarize_upvalue(S, MirGetUpvalue(instr));
            break;
        case kMirSetUpvalue:
            scalarize_setupvalue(S, MirGetSetUpvalue(instr));
            break;
        case kMirCapture:
            scalarize_capture(S, MirGetCapture(instr));
            break;
        case kMirClose:
            scalarize_close(S, MirGetClose(instr));
            break;
        case kMirGoto:
            ADD_INSTRUCTION(S, instr);
            break;
        default:
            // NOTE: Phi instructions have not yet been created
            scalarize_other(S, instr);
    }
}

static void scalarize_function(struct Scalarizer *S, struct Mir *mir)
{
    struct FunctionState fs = {
        .registers = MirRegisterDataList_new(mir),
        .upvalues = MirUpvalueList_new(mir),
        .local_map = LocalGroupMap_new(S),
        .upvalue_map = UpvalueGroupMap_new(S),
        .old_locals = mir->locals,
        .outer = S->fs,
        .mir = mir,
    };
    S->fs = &fs;

    // TODO: mapping from old local number to new one for correcting local upvalues
    // TODO: mapping from old upvalue number to new one for correcting nonlocal upvalues
    MirRegister const *pr;
    MirRegisterList *locals = MirRegisterList_new(mir);
    K_LIST_FOREACH (mir->locals, pr) {
        struct MemoryGroup group = get_registers(S, *pr);
        for (int i = 0; i < group.count; ++i) {
            MirRegisterList_push(mir, locals, REGISTER_AT(group, i));
        }
    }

    struct MirCaptureInfo const *pc;
    MirCaptureList *captured = MirCaptureList_new(mir);
    K_LIST_FOREACH (mir->captured, pc) {
        struct MemoryGroup group = get_registers(S, pc->r);
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
        struct MemoryGroup *backing;
        if (pu->is_local) {
            // TODO: will need to modify pu->index based on how locals in this function expanded
            //
            // upvalue is local to enclosing function
            MirRegister const local = MirRegisterList_get(outer->old_locals, pu->index);
            backing = LocalGroupMap_get(S, outer->local_map, local);
            new_local_upvalues(S, index, *backing, pu->index);
        } else {
            // upvalue is also an upvalue in the enclosing function
            backing = UpvalueGroupMap_get(S, outer->upvalue_map, pu->index);
            new_nonlocal_upvalues(S, index, *backing);
        }
    }

    mir->locals = locals;
    mir->captured = captured;

    // scalarize the current function
    struct MirBlockData *const *pbb;
    struct MirInstruction *const *pinstr;
    K_LIST_FOREACH (mir->blocks, pbb) {
        struct MirBlockData *const bb = *pbb;
        S->instructions = MirInstructionList_new(mir);
        K_LIST_FOREACH (bb->instructions, pinstr)
            scalarize_instruction(S, *pinstr);
        bb->instructions = S->instructions;
    }
    mir->registers = fs.registers;
    mir->upvalues = fs.upvalues;

    // scalarize closures
    struct Mir *const *pchild;
    K_LIST_FOREACH (mir->children, pchild)
        scalarize_function(S, *pchild);

    S->fs = fs.outer;
}

void pawP_scalarize_registers(struct Compiler *C, struct Mir *mir)
{
if(mir->name->length>=4&&0==memcmp(mir->name->text,"test",4)){ printf("before: %s\n", pawMir_dump(mir)); }

    struct Scalarizer S = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .P = ENV(C),
        .C = C,
    };

for(int i=0;i<mir->upvalues->count;++i){
    struct MirUpvalueInfo u = mir->upvalues->data[i];
    printf(" %d: index=%d, %s, type=%s\n", i, u.index, u.is_local ? "local" : "nonlocal", pawIr_print_type(C,u.type));
}

    scalarize_function(&S, mir);


if(mir->name->length>=4&&0==memcmp(mir->name->text,"test",4)){ printf("after: %s\n", pawMir_dump(mir)); }
}

