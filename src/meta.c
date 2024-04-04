// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "meta.h"
#include "call.h"
#include "env.h"
#include "map.h"

const char *pawT_name(Metamethod mm)
{
    // NOTE: This array must be kept in the same order as the opcodes enum
    //       in opcode.h.
    static const char *kMetaNames[] = {
        "__call",

        "__getattr",
        "__setattr",
        "__getitem",
        "__setitem",
        "__getslice",
        "__setslice",

        "__len",
        "__neg",
        "__not",
        "__bnot",

        "__eq",
        "__ne",
        "__lt",
        "__le",
        "__gt",
        "__ge",
        "__contains",

        "__add",
        "__sub",
        "__mul",
        "__div",
        "__idiv",
        "__mod",
        "__pow",
        "__concat",
        "__bxor",
        "__band",
        "__bor",
        "__shl",
        "__shr",

        "__radd",
        "__rsub",
        "__rmul",
        "__rdiv",
        "__ridiv",
        "__rmod",
        "__rpow",
        "__rconcat",
        "__rbxor",
        "__rband",
        "__rbor",
        "__rshl",
        "__rshr",

        "__init",
        "__null",
        "__str",
        "__int",
        "__float",
        "__bool",
        "__array",
        "__map",
    };
    _Static_assert(paw_countof(kMetaNames) == NMETAMETHODS,
                   "metamethod names re inconsistent");
    paw_assert(mm < NMETAMETHODS);
    return kMetaNames[mm];
}
