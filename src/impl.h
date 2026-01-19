// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"

struct ContainingImplResult {
    struct IrTypeList *before;
    struct IrTypeList *after;
    struct IrType *trait;
    struct IrType *type;
};

struct Instantiation *pawP_find_method(struct Compiler *C, struct IrType *self, Str *name);
