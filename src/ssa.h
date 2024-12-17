// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_SSA_H
#define PAW_SSA_H

#include "compile.h"

struct MirRegisterList;

// Convert the CFG to static single assignment (SSA) form
void pawSsa_convert(struct Compiler *C, struct Mir *mir, Map *vars, Map *defs, struct MirRegisterList *locals);

// Remove SSA constraints from the CFG
void pawSsa_destroy(struct Compiler *C, struct Mir *mir);

#endif // PAW_SSA_H
