// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "layout.h"

// TODO: module name for error messages, or error return value for pawIr_compute_layout
#define LAYOUT_ERROR(C_, Kind_, ...) pawErr_##Kind_(C_, SCAN_STR(C_, "(TODO: module name goes here)"), __VA_ARGS__)

struct LayoutState {
    struct Compiler *C;
};

static struct IrLayout const SCALAR_LAYOUTS[] = {
    [kIrNever] = {
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(0),
        .size = IR_TYPESIZE_FIXED(0),
        .count = 1,
    },

    [kIrUnit] = {
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(0),
        .size = IR_TYPESIZE_FIXED(0),
        .count = 1,
    },

    [kIrBool] = {
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(0),
        .size = IR_TYPESIZE_FIXED(1),
        .count = 1,
    },

    [kIrChar] = {
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(0),
        .size = IR_TYPESIZE_FIXED(1),
        .count = 1,
    },

    [kIrFloat] = {
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(3),
        .size = IR_TYPESIZE_FIXED(8),
        .count = 1,
    },

    [kIrInt] = {
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(3),
        .size = IR_TYPESIZE_FIXED(8),
        .count = 1,
    },

    [kIrPtr] = {
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(3),
        .size = IR_TYPESIZE_FIXED(8),
        .count = 1,
    },
};


static struct IrLayout compute_field_layout(struct LayoutState *L, IrType *type);

static IrTypeSize align_up(IrTypeSize size, IrAlignment alignment)
{
    unsigned const value = IR_TYPESIZE_GET_VALUE(size);
    unsigned const align = IR_ALIGNMENT_AS_INTEGER(alignment);
    return IR_TYPESIZE_FIXED(value + (-value & (align - 1)));
}

static struct IrLayout compute_typelist_layout(struct LayoutState *L, IrTypeList const *types)
{
    IrLayoutFields *fields = IrLayoutFields_new(L->C);
    IrAlignment max_alignment = IR_ALIGNMENT_FROM_EXPONENT(0);
    IrTypeSize total_size = IR_TYPESIZE_FIXED(0);

    K_LIST_XFOREACH (types, IrType *const, p) {
        struct IrLayout const lo = compute_field_layout(L, *p);
        total_size = align_up(total_size, lo.alignment);
        IrLayoutFields_push(L->C, fields, (struct IrLayoutField){
                    .offset = IR_TYPESIZE_GET_VALUE(total_size),
                    .layout = lo,
                });

        total_size = IrTypeSize_add_unchecked(total_size, lo.size);
        max_alignment = IrAlignment_max(max_alignment, lo.alignment);
    }

    return (struct IrLayout){
        .size = align_up(total_size, max_alignment),
        .alignment = max_alignment,
        .fields = fields,
        .count = 1,
    };

}

static struct IrLayout fatptr_layout(struct LayoutState *L, IrType *pointee)
{
    IrTypeList *fields = IrTypeList_new(L->C);
    IrTypeList_push(L->C, fields, pawIr_new_ptr(L->C, pointee));
    IrTypeList_push(L->C, fields, pawIr_new_int(L->C));
    return compute_typelist_layout(L, fields);
}

static struct IrLayout compute_struct_layout(struct LayoutState *L, struct IrAdt *t)
{
    IrTypeList const *types = pawP_instantiate_struct_fields(L->C, t);
    return compute_typelist_layout(L, types);
}

// Compute the memory layout of an enumeration
// An enumeration value, i.e. a tagged union, must have enough space for the largest of
// its variants, as well as the integer discriminant.
static struct IrLayout compute_enum_layout(struct LayoutState *L, struct IrAdt *t)
{
    struct IrAdtDef const *def = pawIr_get_adt_def(L->C, t->did);

    struct IrLayout enum_lo = {
        .fields = IrLayoutFields_new(L->C),
    };

    int index;
    struct IrVariantDef *const *pvariant;
    K_LIST_ENUMERATE (def->variants, index, pvariant) {
        IrTypeList *fields = pawP_instantiate_variant_fields(L->C, t, index);
        IrTypeList_insert(L->C, fields, 0, pawIr_new_int(L->C));
        struct IrLayout const variant_lo = compute_typelist_layout(L, fields);

        IrLayoutFields_push(L->C, enum_lo.fields,
                (struct IrLayoutField){
                    .layout = variant_lo,
                    .offset = 0,
                });
        enum_lo.alignment = IrAlignment_max(enum_lo.alignment, variant_lo.alignment);
        enum_lo.size = IrTypeSize_max(enum_lo.size, variant_lo.size);
    }

    return enum_lo;
}

static struct IrLayout compute_tuple_layout(struct LayoutState *L, struct IrTuple *t)
{
    return compute_typelist_layout(L, t->elems);
}

static struct IrLayout compute_adt_layout(struct LayoutState *L, struct IrAdt *t)
{
    struct IrAdtDef const *def = pawIr_get_adt_def(L->C, t->did);

    return def->is_struct
        ? compute_struct_layout(L, t)
        : compute_enum_layout(L, t);
}

static struct IrLayout compute_array_layout(struct LayoutState *L, struct IrArray *t)
{
    struct IrLayout const elem = compute_field_layout(L, t->type);
    return (struct IrLayout){
        .count = (unsigned)t->length->value.value.i,
        .alignment = elem.alignment,
        .fields = elem.fields,
        .size = elem.size,
    };
}

static struct IrLayout compute_field_layout(struct LayoutState *L, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrNever:
        case kIrUnit:
        case kIrBool:
        case kIrChar:
        case kIrInt:
        case kIrFloat:
            return SCALAR_LAYOUTS[IR_KINDOF(type)];
        case kIrPtr:
            return SCALAR_LAYOUTS[kIrPtr];
        case kIrString:
            return fatptr_layout(L, pawIr_new_char(L->C));
        case kIrSlice:
            return fatptr_layout(L, IrGetSlice(type)->type);
        case kIrAdt:
            return compute_adt_layout(L, IrGetAdt(type));
        case kIrFnPtr:
        case kIrSignature:
            return SCALAR_LAYOUTS[kIrPtr];
        case kIrArray:
            return compute_array_layout(L, IrGetArray(type));
        case kIrClosure:
            // TODO: layout of env?
            return SCALAR_LAYOUTS[kIrPtr];
        default:
            paw_assert(IrIsTuple(type));
            return compute_tuple_layout(L, IrGetTuple(type));
    }
}

struct IrLayout pawIr_compute_layout(struct Compiler *C, IrType *type)
{
    struct IrLayout const *p = IrTypeLayouts_get(C, C->layouts, type);
    if (p != NULL) return *p;

    struct LayoutState L = {.C = C};
    struct IrLayout const layout = compute_field_layout(&L, type);
    IrTypeLayouts_insert(C, C->layouts, type, layout);
    return layout;
}

