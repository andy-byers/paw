// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "meta.h"
#include "call.h"
#include "env.h"
#include "map.h"

const char *pawT_name(Op op)
{
    // NOTE: This array must be kept in the same order as the opcodes enum
    //       in opcode.h.
    static const char *kMetaNames[] = {
        "__null",
        "__str",
        "__int",
        "__float",
        "__bool",
        "__array",
        "__map",

        "__call",
        "__iter",
        "__next",

        "__getattr",
        "__setattr",
        "__getitem",
        "__setitem",
        "__getslice",
        "__setslice",

        "__eq",
        "__lt",
        "__le",
        "__contains",

        "__len",
        "__neg",
        "__not",
        "__bnot",

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
    };
    return kMetaNames[op2meta(op)];
}

Value *pawT_get_meta_(paw_Env *P, Op op, Value obj)
{
    const Value key = P->meta_keys[op2meta(op)];
    Value *bound = pawV_find_binding(P, obj, key);
    if (bound) {
        return bound;
    }
    Map *meta;
    if (pawV_is_instance(obj)) {
        meta = pawV_get_instance(obj)->attr;
    } else if (pawV_is_foreign(obj)) {
        paw_assert(pawV_is_foreign(obj));
        meta = pawV_get_foreign(obj)->attr;
    } else {
        return NULL;
    }
    return pawH_action(P, meta, key, MAP_ACTION_NONE);
}
