// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "type.h"
#include "env.h"
#include "mem.h"
#include "util.h"

static void add_basic_type(paw_Env *P, paw_Type code)
{
    Type *r = pawY_type_new(P, P->mod);
    r->hdr.kind = TYPE_BASIC;
    r->hdr.def = code;
}

void pawY_init(paw_Env *P)
{
    P->mod = pawY_module_new(P);

    add_basic_type(P, PAW_TUNIT);
    add_basic_type(P, PAW_TBOOL);
    add_basic_type(P, PAW_TINT);
    add_basic_type(P, PAW_TFLOAT);
    add_basic_type(P, PAW_TSTRING);
}

void pawY_uninit(paw_Env *P)
{
    pawY_module_free(P, P->mod);
}

Module *pawY_module_new(paw_Env *P)
{
    return pawM_new(P, Module);
}

static void free_type(paw_Env *P, Type *type)
{
    switch (y_kind(type)) {
        case TYPE_FUNC:
            pawM_free_vec(P, type->func.params.types, type->func.params.count);
            pawM_free_vec(P, type->func.types.types, type->func.types.count);
            break;
        case TYPE_ADT:
            pawM_free_vec(P, type->adt.types.types, type->adt.types.count);
            break;
        case TYPE_MODULE:
            pawY_module_free(P, &type->mod); 
            break;
        default:
            break;

    }
    pawM_free(P, type);
}

void pawY_module_free(paw_Env *P, Module *mod)
{
    for (int i = 0; i < mod->ntypes; ++i) {
        free_type(P, mod->types[i]);
    }
    pawM_free_vec(P, mod->types, mod->capacity);
}

Type *pawY_type_new(paw_Env *P, Module *mod)
{
    pawM_grow(P, mod->types, mod->ntypes, mod->capacity);
    const int code = mod->ntypes++;
    Type **ptarget = &mod->types[code];
    *ptarget = pawM_new(P, Type);
    (*ptarget)->hdr.def = code; // TODO: Not correct...
    return *ptarget;
}

