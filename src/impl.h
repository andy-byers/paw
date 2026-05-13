// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"

struct Instantiation *pawP_find_method(struct Compiler *C, struct IrType *self, Str const *name);
struct Instantiation *pawP_find_trait_method(struct Compiler *C, struct IrType *self, struct IrTrait *trait, Str const *name);

struct Instantiation *pawIr_find_assoc_type_projection(struct Compiler *C, struct IrType *self, struct IrTrait *trait, Str const *name);
struct Instantiation *pawIr_find_assoc_type_generic(struct Compiler *C, struct IrType *self, Str const *name);
