// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "type.h"
#include "mem.h"

static void free_def(paw_Env *P, struct Def *def)
{
    switch (def->hdr.kind) {
        case DEF_ADT:
            pawM_free_vec(P, def->adt.fields, def->adt.nfields);
            break;
        case DEF_VARIANT:
            pawM_free_vec(P, def->variant.fields, def->variant.nfields);
            break;
        case DEF_FUNC:
            pawM_free_vec(P, def->func.types, def->func.ntypes);
            break;
        case DEF_VAR:
        case DEF_FIELD:
            break;
    }
    pawM_free(P, def);
}

static void free_type(paw_Env *P, struct Type *type)
{
    pawM_free_flex(P, type, type->nsubtypes, sizeof(type->subtypes[0]));
}

void pawY_uninit(paw_Env *P)
{
    for (int i = 0; i < P->types.count; ++i) free_type(P, Y_TYPE(P, i));
    for (int i = 0; i < P->defs.count; ++i) free_def(P, Y_DEF(P, i));
    pawM_free_vec(P, P->types.data, P->types.alloc);
    pawM_free_vec(P, P->defs.data, P->defs.alloc);
    pawM_free_vec(P, P->vals.data, P->vals.alloc);
    memset(&P->types, 0, sizeof(P->types));
    memset(&P->defs, 0, sizeof(P->defs));
    memset(&P->vals, 0, sizeof(P->vals));
}

static struct Type *add_type(paw_Env *P, struct Type *type)
{
    type->hdr.code = P->types.count;
    pawM_grow(P, P->types.data, P->types.count, P->types.alloc);
    P->types.data[P->types.count++] = type;
    return type;
}

#define NEW_TYPE(P, n) \
    pawM_new_flex(P, struct Type, n, sizeof(paw_Type))

struct Type *pawY_new_adt(paw_Env *P, int ntypes)
{
    struct Type *type = NEW_TYPE(P, ntypes);
    type->hdr.kind = TYPE_ADT;
    return add_type(P, type);
}

struct Type *pawY_new_signature(paw_Env *P, int nparams)
{
    struct Type *type = NEW_TYPE(P, nparams);
    type->hdr.kind = TYPE_SIGNATURE;
    return add_type(P, type);
}

struct Type *pawY_new_tuple(paw_Env *P, int nelems)
{
    struct Type *type = NEW_TYPE(P, nelems);
    type->hdr.kind = TYPE_TUPLE;
    return add_type(P, type);
}

static struct Def *new_def(paw_Env *P, enum DefKind kind)
{
    pawM_grow(P, P->defs.data, P->defs.count, P->defs.alloc);
    struct Def *def = pawM_new(P, struct Def);
    P->defs.data[P->defs.count++] = def;
    def->hdr.kind = kind;
    return def;
}

struct Def *pawY_new_adt_def(paw_Env *P, int nfields)
{
    struct Def *def = new_def(P, DEF_ADT);
    def->adt.fields = pawM_new_vec(P, nfields, DefId);
    def->adt.nfields = nfields;
    return def;
}

struct Def *pawY_new_variant_def(paw_Env *P, int nfields)
{
    struct Def *def = new_def(P, DEF_VARIANT);
    def->variant.fields = pawM_new_vec(P, nfields, DefId);
    def->variant.nfields = nfields;
    return def;
}

struct Def *pawY_new_func_def(paw_Env *P, int ntypes)
{
    struct Def *def = new_def(P, DEF_FUNC);
    def->func.types = pawM_new_vec(P, ntypes, paw_Type);
    def->func.ntypes = ntypes;
    return def;
}

struct Def *pawY_new_field_def(paw_Env *P)
{
    return new_def(P, DEF_FIELD);
}

struct Def *pawY_new_var_def(paw_Env *P)
{
    return new_def(P, DEF_VAR);
}

static int print_subtypes_(paw_Env *P, Buffer *buffer, struct Type *type)
{
    for (int i = 0; i < type->nsubtypes; ++i) {
        pawY_print_type(P, buffer, type->subtypes[i]);
        if (i == type->nsubtypes - 1) break;
        pawL_add_string(P, buffer, ", ");
    }
    return type->nsubtypes;
}
#define PRINT_SUBTYPES(P, buf, type) print_subtypes_(P, buf, CAST(struct Type *, type))

static void print_func_type(paw_Env *P, Buffer *buffer, struct Signature *type)
{
    pawL_add_string(P, buffer, "fn(");
    PRINT_SUBTYPES(P, buffer, type);
    pawL_add_char(P, buffer, ')');
    if (type->result > 0) {
        pawL_add_string(P, buffer, " -> ");
        pawY_print_type(P, buffer, type->result); 
    }
}

static void print_tuple_type(paw_Env *P, Buffer *buffer, struct TupleType *type)
{
    pawL_add_char(P, buffer, '(');
    const int n = PRINT_SUBTYPES(P, buffer, type);
    if (n == 1) pawL_add_char(P, buffer, ',');
    pawL_add_char(P, buffer, ')');
}

static void print_adt(paw_Env *P, Buffer *buffer, struct Adt *type)
{
    struct Def *def = Y_DEF(P, type->did);
    const String *name = def->hdr.name;
    struct Type *base = Y_CAST_TYPE(type);
    pawL_add_nstring(P, buffer, name->text, name->length);
    if (base->nsubtypes > 0) {
        pawL_add_char(P, buffer, '<');
        print_subtypes_(P, buffer, base);
        pawL_add_char(P, buffer, '>');
    }
}

void pawY_print_type(paw_Env *P, Buffer *buffer, paw_Type code)
{
    struct Type *type = Y_TYPE(P, code);
    switch (type->hdr.kind) {
        case TYPE_SIGNATURE:
            print_func_type(P, buffer, &type->sig);
            break;
        case TYPE_TUPLE:
            print_tuple_type(P, buffer, &type->tuple);
            break;
        case TYPE_ADT:
            print_adt(P, buffer, &type->adt);
    }
}

void pawY_mangle_start(paw_Env *P, Buffer *buffer, const String *name)
{
    L_ADD_STRING(P, buffer, name);
}

void pawY_mangle_add_arg(paw_Env *P, Buffer *buffer, paw_Type code)
{
    struct Type *type = Y_TYPE(P, code);
    switch (type->hdr.kind) {
        case TYPE_ADT:
            if (type->adt.code == PAW_TUNIT) {
                L_ADD_LITERAL(P, buffer, "0");
            } else if (type->adt.code == PAW_TBOOL) {
                L_ADD_LITERAL(P, buffer, "b");
            } else if (type->adt.code == PAW_TINT) {
                L_ADD_LITERAL(P, buffer, "i");
            } else if (type->adt.code == PAW_TFLOAT) {
                L_ADD_LITERAL(P, buffer, "f");
            } else if (type->adt.code == PAW_TSTR) {
                L_ADD_LITERAL(P, buffer, "s");
            } else {
                const struct Def *def = Y_DEF(P, type->adt.did);
                const struct Type *adt = Y_TYPE(P, def->hdr.code);
                L_ADD_STRING(P, buffer, def->hdr.name);
                if (adt->nsubtypes > 0) {
                    for (int i = 0; i < adt->nsubtypes; ++i) {
                        pawY_mangle_add_arg(P, buffer, adt->subtypes[i]);
                    }
                }
            }
            break;
        case TYPE_SIGNATURE: {
            const struct Signature func = type->sig;
            pawL_add_char(P, buffer, 'F');
            for (int i = 0; i < type->nsubtypes; ++i) {
                pawY_mangle_add_arg(P, buffer, type->subtypes[i]);
            }
            pawL_add_char(P, buffer, '_');
            const struct Type *result = Y_TYPE(P, func.result);
            if (result->hdr.kind != TYPE_ADT || 
                    result->adt.code != PAW_TUNIT) {
                pawY_mangle_add_arg(P, buffer, func.result);
            }
            break;
        }
        case TYPE_TUPLE: {
            pawL_add_char(P, buffer, 't');
            for (int i = 0; i < type->nsubtypes; ++i) {
                pawY_mangle_add_arg(P, buffer, type->subtypes[i]);
            }
            pawL_add_char(P, buffer, '_');
        }
    }
}

void pawY_mangle_finish(paw_Env *P, Buffer *buffer)
{
    pawL_add_char(P, buffer, '_');
}

