// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "layout.h"

// TODO: module name for error messages, or error return value for pawIr_compute_layout
#define LAYOUT_ERROR(C_, Kind_, ...) pawErr_##Kind_(C_, SCAN_STR(C_, "(TODO: module name goes here)"), __VA_ARGS__)

static struct IrLayout compute_outer_layout(struct Compiler *C, IrType *type);

static struct IrLayout compute_scalar_layout(struct Compiler *C)
{
    return (struct IrLayout){
        .fields = IrLayoutList_new(C),
        .size = 1,
    };
}

static struct IrLayout compute_typelist_layout(struct Compiler *C, IrTypeList const *types)
{
    struct IrLayout layout = {
        .fields = IrLayoutList_new(C),
    };

    IrType *const *ptype;
    K_LIST_FOREACH (types, ptype) {
        struct IrLayout lo = compute_outer_layout(C, *ptype);
        IrLayoutList_push(C, layout.fields, lo);
        layout.size += lo.size;
    }

    return layout;
}

static struct IrLayout compute_struct_layout(struct Compiler *C, struct IrAdt *t)
{
    IrTypeList const *types = pawP_instantiate_struct_fields(C, t);
    struct IrLayout layout = compute_typelist_layout(C, types);
    layout.size = PAW_MAX(layout.size, 1);
    return layout;
}

// Compute the memory layout of an enumeration
// An enumeration value, i.e. a tagged union, must have enough space for the largest of
// its variants, as well as the integer discriminant.
static struct IrLayout compute_enum_layout(struct Compiler *C, struct IrAdt *t)
{
    IrType *type = IR_CAST_TYPE(t);
    struct IrAdtDef *def = pawIr_get_adt_def(C, t->did);
    struct IrLayout *playout = IrLayoutMap_get(C, C->layouts, type);
    if (playout != NULL) {
        if (playout->size < 0)
            LAYOUT_ERROR(C, infinite_size_object, (struct SourceLoc){0}, def->name->text);
        return *playout;
    }

    struct IrLayout layout = {
        .fields = IrLayoutList_new(C),
        .size = -1,
    };
    // insert sentinel with negative ".size" to track recursive types
    IrLayoutMap_insert(C, C->layouts, type, layout);

    int index;
    struct IrVariantDef *const *pvariant;
    K_LIST_ENUMERATE (def->variants, index, pvariant) {
        IrTypeList const *fields = pawP_instantiate_variant_fields(C, t, index);
        struct IrLayout lo = compute_typelist_layout(C, fields);

        struct IrLayout discr = compute_scalar_layout(C);
        IrLayoutList_insert(C, lo.fields, 0, discr);
        paw_assert(discr.size == 1);
        ++lo.size;

        IrLayoutList_push(C, layout.fields, lo);
        layout.size = PAW_MAX(layout.size, lo.size);
    }

    // overwrite sentinal with computed layout
    IrLayoutMap_insert(C, C->layouts, type, layout);
    return layout;
}

static struct IrLayout compute_tuple_layout(struct Compiler *C, struct IrTuple *t)
{
    struct IrLayout layout = {
        .fields = IrLayoutList_new(C),
    };

    IrType *const *pelem;
    K_LIST_FOREACH (t->elems, pelem) {
        struct IrLayout lo = compute_outer_layout(C, *pelem);
        IrLayoutList_push(C, layout.fields, lo);
        layout.size += lo.size;
    }

    return layout;
}

static paw_Bool is_enum(struct Compiler *C, IrType *type)
{
    if (IrIsAdt(type)) {
        struct IrAdtDef *def = pawIr_get_adt_def(C, IR_TYPE_DID(type));
        return !def->is_struct;
    }
    return PAW_FALSE;
}

static struct IrLayout compute_outer_layout(struct Compiler *C, IrType *type)
{
    if (IrIsTuple(type)) {
        return compute_tuple_layout(C, IrGetTuple(type));
    } else if (ir_is_boxed(C, type) || !IrIsAdt(type)) {
        return compute_scalar_layout(C);
    } else if (is_enum(C, type)) {
        return compute_enum_layout(C, IrGetAdt(type));
    } else {
        return compute_struct_layout(C, IrGetAdt(type));
    }
}

struct IrLayout pawIr_compute_layout(struct Compiler *C, IrType *type)
{
    struct IrLayout *playout = IrLayoutMap_get(C, C->layouts, type);
    if (playout != NULL)
        return *playout;

    struct IrLayout layout;
    if (IrIsTuple(type)) {
        layout = compute_tuple_layout(C, IrGetTuple(type));
    } else if (is_enum(C, type)) {
        layout = compute_enum_layout(C, IrGetAdt(type));
    } else if (IrIsAdt(type)) {
        layout = compute_struct_layout(C, IrGetAdt(type));
    } else {
        layout = compute_scalar_layout(C);
    }

    IrLayoutMap_insert(C, C->layouts, type, layout);
    return layout;
}

