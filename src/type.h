// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_TYPE_H
#define PAW_TYPE_H

#include "paw.h"
#include "value.h"

// TODO: Have this return a String, generate the actual type name for arrays and maps,
//       like '[int]', or 'int[string]'.
static inline const char *pawY_name(int type)
{
    switch (type) {
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

#define pawY_is_same(a, b) ((a) == (b))
paw_Bool pawY_is_similar(TypeTag a, TypeTag b);
int pawY_common(TypeTag a, TypeTag b, TypeTag *out);
TypeTag pawY_unwrap(paw_Env *P, TypeTag t);

void pawY_init(paw_Env *P);
void pawY_uninit(paw_Env *P);

Type *pawY_register_method(paw_Env *P, ClassType *recv, TypeTag func);
Type *pawY_register_function(paw_Env *P, TypeTag *param, int nparam, TypeTag ret);
Type *pawY_register_class(paw_Env *P, String *name, Attribute *attrs, int nattrs);
Type *pawY_register_array(paw_Env *fn, TypeTag elem);
Type *pawY_register_map(paw_Env *P, TypeTag key, TypeTag value);

TypeTag *pawY_new_taglist(paw_Env *P, paw_Type *types, int ntypes);
void pawY_free_taglist(paw_Env *P, TypeTag *tags, int ntags);

#endif // PAW_TYPE_H
