// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_MANGLE_H
#define PAW_MANGLE_H

#include "core.h"

struct Compiler;
struct IrType;

EXTERN_C struct Str *mangle_type(struct Compiler *C, struct IrType *type);

#endif // PAW_MANGLE_H



