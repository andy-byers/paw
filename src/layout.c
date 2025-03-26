// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"

static struct IrLayout variant_layout(struct Compiler *C, struct IrVariantDef *def)
{
    struct IrLayout layout = {0};
    struct IrFieldDef *const *pf;
    K_LIST_FOREACH(def->fields, pf) {
        IrType *type = pawIr_get_def_type(C, (*pf)->did);
        struct IrLayout current = pawIr_compute_layout(C, type);
        layout.size += current.size;
    }
    return layout;
}

struct IrLayout pawIr_compute_layout(struct Compiler *C, IrType *type)
{
    struct IrLayout layout = {.size = 1};
    if (IrIsAdt(type)) {
        struct IrAdtDef *def = pawIr_get_adt_def(C, IR_TYPE_DID(type));
        if (def->is_struct)
            return layout;

        struct IrVariantDef *const *pv;
        K_LIST_FOREACH(def->variants, pv) {
            struct IrLayout current = variant_layout(C, *pv);
            layout.size = 1 + PAW_MAX(current.size, layout.size);
        }
    }
    return layout;
}

