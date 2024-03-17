// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "meta.h"
#include "call.h"
#include "env.h"
#include "map.h"

#define MM_OP2MM(op) ((op)-MM_FIRST)

const char *pawT_name(Op op)
{
    static const char *kMetaNames[] = {
        "__len",
        "__neg",
        "__not",
        "__bnot",

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

        "__eq",
        "__lt",
        "__le",
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
    };
    return kMetaNames[MM_OP2MM(op)];
}

Value *pawT_get_meta_(paw_Env *P, Op op, Value obj)
{
    Map *meta;
    if (pawV_is_instance(obj)) {
        meta = pawV_get_instance(obj)->attr;
    } else if (pawV_is_userdata(obj)) {
        paw_assert(pawV_is_userdata(obj));
        meta = pawV_get_userdata(obj)->attr;
    } else {
        return NULL;
    }
    const Value key = P->meta_keys[op - MM_FIRST];
    return pawH_action(P, meta, key, MAP_ACTION_NONE);
}
