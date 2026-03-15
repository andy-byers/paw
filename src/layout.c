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

static struct IrLayout fatptr_layout(struct LayoutState *L)
{
    IrLayouts *fields = IrLayouts_new(L->C);
    IrLayouts_push(L->C, fields, SCALAR_LAYOUTS[kIrPtr]);
    IrLayouts_push(L->C, fields, SCALAR_LAYOUTS[kIrInt]);

    unsigned size = 0;
    K_LIST_XFOREACH (fields, struct IrLayout const, p)
        size += IR_TYPESIZE_GET_VALUE(p->size);

    return (struct IrLayout){
        .alignment = SCALAR_LAYOUTS[kIrPtr].alignment,
        .size = IR_TYPESIZE_FIXED(size),
        .fields = fields,
        .count = 1,
    };
}

static struct IrLayout compute_field_layout(struct LayoutState *L, IrType *type);

static struct IrLayout leaf_layout(struct LayoutState *L, unsigned size, unsigned alignment_exponent)
{
    return (struct IrLayout){
        .fields = IrLayouts_new(L->C),
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(alignment_exponent),
        .size = IR_TYPESIZE_FIXED(size),
    };
}

static unsigned bump_to_alignment(unsigned offset, IrAlignment alignment)
{
    unsigned const align = IR_ALIGNMENT_AS_INTEGER(alignment);
    return (offset + (-offset & (align - 1)));
}

static struct IrLayout compute_typelist_layout(struct LayoutState *L, IrTypeList const *types)
{
    IrLayouts *fields = IrLayouts_new(L->C);
    unsigned max_align_exp = 0;
    unsigned total_size = 0;

    K_LIST_XFOREACH (types, IrType *const, p) {
        struct IrLayout const field = compute_field_layout(L, *p);
        unsigned const field_offset = bump_to_alignment(total_size, field.alignment);
        IrLayouts_push(L->C, fields, field);

        unsigned const field_size = IR_TYPESIZE_GET_VALUE(field.size);
        max_align_exp = PAW_MAX(max_align_exp, field.alignment.exponent);
        total_size = field_offset + field_size;
    }

    return (struct IrLayout){
        .alignment = IR_ALIGNMENT_FROM_EXPONENT(max_align_exp),
        .size = IR_TYPESIZE_FIXED(total_size),
        .fields = fields,
        .count = 1,
    };

}

static struct IrLayout compute_struct_layout(struct LayoutState *L, struct IrAdt *t)
{
    IrTypeList const *types = pawP_instantiate_struct_fields(L->C, t);
    struct IrLayout layout = compute_typelist_layout(L, types);
    layout.size.value = PAW_MAX(layout.size.value, 1); // TODO
    return layout;
}

// Compute the memory layout of an enumeration
// An enumeration value, i.e. a tagged union, must have enough space for the largest of
// its variants, as well as the integer discriminant.
static struct IrLayout compute_enum_layout(struct LayoutState *L, struct IrAdt *t)
{
    struct IrAdtDef const *def = pawIr_get_adt_def(L->C, t->did);

    struct IrLayout layout = {
        .fields = IrLayouts_new(L->C),
    };

    int index;
    struct IrVariantDef *const *pvariant;
    K_LIST_ENUMERATE (def->variants, index, pvariant) {
        IrTypeList const *fields = pawP_instantiate_variant_fields(L->C, t, index);
        struct IrLayout lo = compute_typelist_layout(L, fields);

        struct IrLayout const discr = SCALAR_LAYOUTS[kIrInt];
        IrLayouts_insert(L->C, lo.fields, 0, discr);
        lo.size.value += IR_TYPESIZE_GET_VALUE(discr.size);

        IrLayouts_push(L->C, layout.fields, lo);
        layout.size.value = PAW_MAX(layout.size.value, lo.size.value);
    }

    return layout;
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
        case kIrSlice:
            return fatptr_layout(L);
        case kIrAdt:
            return compute_adt_layout(L, IrGetAdt(type));
        case kIrFnPtr:
        case kIrSignature:
            return SCALAR_LAYOUTS[kIrPtr];
        case kIrArray:
            return compute_array_layout(L, IrGetArray(type));
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

