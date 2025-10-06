// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_REGSTACK_H
#define PAW_REGSTACK_H

#include "mir.h"

struct RegstackInfo {
    int index;
};

DEFINE_MAP(struct Mir, RegstackMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirRegister, struct RegstackInfo)

// Enforce the register stack constraint
// Rewrites the "mir" so there is a producing instruction in the correct location
// to load each temporary value on to the stack to be consumed by some other
// instruction (required by instructions that accept more operands/produce more
// results than can be stored in a single opcode).
// Returns a mapping from virtual register number to stack slot for each stack-
// constrained register.
RegstackMap *pawRegstack_enforce_constraint(struct Mir *mir);

#endif // PAW_REGSTACK_H
