// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_LAYOUT_H
#define PAW_LAYOUT_H

#include "ir_type.h"

struct IrLayout {
    struct IrLayoutList *fields;
    int size;
};

struct IrLayout pawIr_compute_layout(struct Compiler *C, IrType *type);

DEFINE_LIST(struct Compiler, IrLayoutList, struct IrLayout)
DEFINE_MAP(struct Compiler, IrLayoutMap, pawP_alloc, pawIr_type_hash, pawIr_type_equals, IrType *, struct IrLayout)

#endif // PAW_LAYOUT_H
