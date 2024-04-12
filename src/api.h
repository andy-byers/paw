// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_API_H
#define PAW_API_H
#include "paw.h"
#include "value.h"

static inline const char *api_typename(int type)
{
    switch (type) {
        case PAW_NULL:
            return "null";
        case PAW_TBOOL:
            return "boolean";
        case PAW_TINT:
            return "integer";
        case PAW_TFLOAT:
            return "float";
        case PAW_TSTRING:
            return "string";
        case PAW_TARRAY:
            return "array";
        case PAW_TMAP:
            return "map";
        case PAW_TFUNCTION:
            return "function";
        case PAW_TCLASS:
            return "class";
        case PAW_TFOREIGN:
            return "foreign";
        default:
            return "?";
    }
}

#endif // PAW_API_H
