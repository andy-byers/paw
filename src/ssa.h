// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_SSA_H
#define PAW_SSA_H

#include "mir.h"

// Convert the CFG to static single assignment (SSA) form
void pawSsa_construct(struct Mir *mir);

#endif // PAW_SSA_H
