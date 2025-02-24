// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_TRAIT_H
#define PAW_TRAIT_H

#include "paw.h"

struct Compiler;
struct IrType;
struct IrTypeList;

struct IrTypeList *pawP_query_traits(struct Compiler *C, struct IrType *type);
void pawP_add_trait_impl(struct Compiler *C, struct IrType *type, struct IrType *trait);
paw_Bool pawP_satisfies_bounds(struct Compiler *C, struct IrType *type, struct IrTypeList *bounds);

#endif // PAW_TRAIT_H
