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
    type->nsubtypes = ntypes;
    return add_type(P, type);
}

struct Type *pawY_new_signature(paw_Env *P, int nparams)
{
    struct Type *type = NEW_TYPE(P, nparams);
    type->hdr.kind = TYPE_SIGNATURE;
    type->nsubtypes = nparams;
    return add_type(P, type);
}

struct Type *pawY_new_tuple(paw_Env *P, int nelems)
{
    struct Type *type = NEW_TYPE(P, nelems);
    type->hdr.kind = TYPE_TUPLE;
    type->nsubtypes = nelems;
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

static int print_subtypes_(paw_Env *P, Buffer *buf, struct Type *type)
{
    for (int i = 0; i < type->nsubtypes; ++i) {
        pawY_print_type(P, buf, type->subtypes[i]);
        if (i == type->nsubtypes - 1) break;
        pawL_add_string(P, buf, ", ");
    }
    return type->nsubtypes;
}
#define PRINT_SUBTYPES(P, buf, type) print_subtypes_(P, buf, CAST(struct Type *, type))

static void print_func_type(paw_Env *P, Buffer *buf, struct Signature *type)
{
    pawL_add_string(P, buf, "fn(");
    PRINT_SUBTYPES(P, buf, type);
    pawL_add_char(P, buf, ')');
    if (type->result > 0) {
        pawL_add_string(P, buf, " -> ");
        pawY_print_type(P, buf, type->result);
    }
}

static void print_tuple_type(paw_Env *P, Buffer *buf, struct TupleType *type)
{
    pawL_add_char(P, buf, '(');
    const int n = PRINT_SUBTYPES(P, buf, type);
    if (n == 1) pawL_add_char(P, buf, ',');
    pawL_add_char(P, buf, ')');
}

static void print_adt(paw_Env *P, Buffer *buf, struct Adt *type)
{
    struct Def *def = Y_DEF(P, type->did);
    const String *name = def->hdr.name;
    struct Type *base = Y_CAST_TYPE(type);
    pawL_add_nstring(P, buf, name->text, name->length);
    if (base->nsubtypes > 0) {
        pawL_add_char(P, buf, '<');
        print_subtypes_(P, buf, base);
        pawL_add_char(P, buf, '>');
    }
}

void pawY_print_type(paw_Env *P, Buffer *buf, paw_Type code)
{
    struct Type *type = Y_TYPE(P, code);
    switch (type->hdr.kind) {
        case TYPE_SIGNATURE:
            print_func_type(P, buf, &type->sig);
            break;
        case TYPE_TUPLE:
            print_tuple_type(P, buf, &type->tuple);
            break;
        case TYPE_ADT:
            print_adt(P, buf, &type->adt);
    }
}

static void add_string_with_len(paw_Env *P, Buffer *buf, const String *str)
{
    paw_push_int(P, str->length);
    pawL_add_value(P, buf, PAW_TINT);
    pawL_add_nstring(P, buf, str->text, str->length);
}

void pawY_mangle_start(paw_Env *P, Buffer *buf, const String *modname, const String *name)
{
    L_ADD_LITERAL(P, buf, "_P");
    pawY_mangle_add_self(P, buf, modname, name);
}

void pawY_mangle_start_generic_args(paw_Env *P, Buffer *buf)
{
    pawL_add_char(P, buf, 'I');
}

void pawY_mangle_finish_generic_args(paw_Env *P, Buffer *buf)
{
    pawL_add_char(P, buf, 'E');
}

void pawY_mangle_add_self(paw_Env *P, Buffer *buf, const String *modname, const String *name)
{
    if (modname != NULL) add_string_with_len(P, buf, modname);
    add_string_with_len(P, buf, name);
}

void pawY_mangle_add_arg(paw_Env *P, Buffer *buf, paw_Type code)
{
    struct Type *type = Y_TYPE(P, code);
    switch (type->hdr.kind) {
        case TYPE_ADT:
            switch (type->adt.code) {
                case PAW_TUNIT:
                    pawL_add_char(P, buf, '0');
                    break;
                case PAW_TBOOL:
                    pawL_add_char(P, buf, 'b');
                    break;
                case PAW_TINT:
                    pawL_add_char(P, buf, 'i');
                    break;
                case PAW_TFLOAT:
                    pawL_add_char(P, buf, 'f');
                    break;
                case PAW_TSTR:
                    pawL_add_char(P, buf, 's');
                    break;
                default: {
                    const struct Def *def = Y_DEF(P, type->adt.did);
                    add_string_with_len(P, buf, def->hdr.name);
                    if (type->nsubtypes > 0) {
                        pawY_mangle_start_generic_args(P, buf);
                        for (int i = 0; i < type->nsubtypes; ++i) {
                            pawY_mangle_add_arg(P, buf, type->subtypes[i]);
                        }
                        pawY_mangle_finish_generic_args(P, buf);
                    }
                }
            }
            break;
        case TYPE_SIGNATURE: {
            const struct Signature func = type->sig;
            pawL_add_char(P, buf, 'F');
            for (int i = 0; i < type->nsubtypes; ++i) {
                pawY_mangle_add_arg(P, buf, type->subtypes[i]);
            }
            pawL_add_char(P, buf, 'E');
            const struct Type *result = Y_TYPE(P, func.result);
            if (result->hdr.kind != TYPE_ADT ||
                    result->adt.code != PAW_TUNIT) {
                pawY_mangle_add_arg(P, buf, func.result);
            }
            break;
        }
        case TYPE_TUPLE: {
            pawL_add_char(P, buf, 'T');
            for (int i = 0; i < type->nsubtypes; ++i) {
                pawY_mangle_add_arg(P, buf, type->subtypes[i]);
            }
            pawL_add_char(P, buf, 'E');
        }
    }
}

void pawY_mangle_finish(paw_Env *P, Buffer *buf)
{
}

