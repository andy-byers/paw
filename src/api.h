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

static inline int api_type(Value v)
{
    switch (pawV_get_type(v)) {
        case VNULL:
            return PAW_NULL;
        case VTRUE:
        case VFALSE:
            return PAW_TBOOL;
        case VBIGINT:
        case VNUMBER:
            return PAW_TINT;
        case VCLOSURE:
        case VMETHOD:
        case VNATIVE:
            return PAW_TFUNCTION;
        case VSTRING:
            return PAW_TSTRING;
        case VARRAY:
            return PAW_TARRAY;
        case VMAP:
            return PAW_TMAP;
        case VCLASS:
            return PAW_TCLASS;
        case VFOREIGN:
            return PAW_TFOREIGN;
        default:
            return PAW_TFLOAT;
    }
}

#endif // PAW_API_H
