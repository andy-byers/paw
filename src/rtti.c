// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "rtti.h"
#include "mem.h"

static void free_def(paw_Env *P, struct Def *def)
{
    switch (def->hdr.kind) {
        case DEF_ADT:
            pawM_free_vec(P, def->adt.fields, def->adt.nfields);
            break;
        case DEF_FUNC:
            pawM_free_vec(P, def->func.types, def->func.ntypes);
            break;
        case DEF_CONST:
        case DEF_TRAIT:
            break;
    }
    pawM_free(P, def);
}

static void free_type(paw_Env *P, struct RttiType *type)
{
    pawM_free_flex(P, type, type->nsubtypes, sizeof(type->subtypes[0]));
}

void pawRtti_uninit(paw_Env *P)
{
    for (int i = 0; i < P->types.count; ++i)
        free_type(P, RTTI_TYPE(P, i));
    for (int i = 0; i < P->defs.count; ++i)
        free_def(P, RTTI_DEF(P, i));
    pawM_free_vec(P, P->types.data, P->types.alloc);
    pawM_free_vec(P, P->defs.data, P->defs.alloc);
    pawM_free_vec(P, P->vals.data, P->vals.alloc);
    memset(&P->types, 0, sizeof(P->types));
    memset(&P->defs, 0, sizeof(P->defs));
    memset(&P->vals, 0, sizeof(P->vals));
}

static struct RttiType *add_type(paw_Env *P, struct RttiType *type)
{
    type->hdr.code = P->types.count;
    pawM_grow(P, P->types.data, P->types.count, P->types.alloc);
    P->types.data[P->types.count++] = type;
    return type;
}

#define NEW_TYPE(P, n) \
    pawM_new_flex(P, struct RttiType, n, sizeof(paw_Type))

struct RttiType *pawRtti_new_adt(paw_Env *P, ItemId iid, int ntypes, int size)
{
    struct RttiType *type = NEW_TYPE(P, ntypes);
    *type = (struct RttiType){
        .adt.kind = RTTI_TYPE_ADT,
        .adt.iid = iid,
        .adt.size = size,
        .nsubtypes = ntypes,
    };
    return add_type(P, type);
}

struct RttiType *pawRtti_new_trait(paw_Env *P)
{
    struct RttiType *type = NEW_TYPE(P, 0);
    *type = (struct RttiType){
        .trait.kind = RTTI_TYPE_TRAIT,
        .nsubtypes = 0,
    };
    return add_type(P, type);
}

struct RttiType *pawRtti_new_never(paw_Env *P)
{
    struct RttiType *type = NEW_TYPE(P, 0);
    *type = (struct RttiType){
        .never.kind = RTTI_TYPE_NEVER,
        .nsubtypes = 0,
    };
    return add_type(P, type);
}

struct RttiType *pawRtti_new_signature(paw_Env *P, ItemId iid, int nparams)
{
    struct RttiType *type = NEW_TYPE(P, nparams);
    *type = (struct RttiType){
        .fdef.kind = RTTI_TYPE_FN_DEF,
        .nsubtypes = nparams,
    };
    return add_type(P, type);
}

struct RttiType *pawRtti_new_func_ptr(paw_Env *P, int nparams)
{
    struct RttiType *type = NEW_TYPE(P, nparams);
    *type = (struct RttiType){
        .fptr.kind = RTTI_TYPE_FN_PTR,
        .nsubtypes = nparams,
    };
    return add_type(P, type);
}

struct RttiType *pawRtti_new_tuple(paw_Env *P, int nelems, int size)
{
    struct RttiType *type = NEW_TYPE(P, nelems);
    *type = (struct RttiType){
        .tuple.kind = RTTI_TYPE_TUPLE,
        .tuple.size = size,
        .nsubtypes = nelems,
    };
    return add_type(P, type);
}

static struct Def *new_def(paw_Env *P, enum DefKind kind)
{
    pawM_grow(P, P->defs.data, P->defs.count, P->defs.alloc);
    struct Def *def = pawM_new(P, struct Def);
    *def = (struct Def){
        .hdr.iid = P->defs.count,
        .hdr.kind = kind,
    };
    P->defs.data[P->defs.count++] = def;
    return def;
}

struct Def *pawRtti_new_adt_def(paw_Env *P, int nfields)
{
    struct Def *def = new_def(P, DEF_ADT);
    def->adt.fields = pawM_new_vec(P, nfields, ItemId);
    def->adt.nfields = nfields;
    return def;
}

struct Def *pawRtti_new_trait_def(paw_Env *P)
{
    return new_def(P, DEF_TRAIT);
}

struct RttiVariant *pawRtti_new_variant(paw_Env *P, String *name, struct RttiField *fields, int num_fields)
{
    struct RttiVariant *variant = pawM_new(P, struct RttiVariant);
    *variant = (struct RttiVariant){
        .num_fields = num_fields,
        .fields = fields,
        .name = name,
    };
    return variant;
}

struct RttiField *pawRtti_new_field(paw_Env *P, String *name, paw_Type code, paw_Bool is_pub)
{
    struct RttiField *field = pawM_new(P, struct RttiField);
    *field = (struct RttiField){
        .is_pub = is_pub,
        .name = name,
        .code = code,
    };
    return field;
}

struct Def *pawRtti_new_func_def(paw_Env *P, int ntypes)
{
    struct Def *def = new_def(P, DEF_FUNC);
    def->func.types = pawM_new_vec(P, ntypes, paw_Type);
    def->func.ntypes = ntypes;
    return def;
}

struct Def *pawRtti_new_const_def(paw_Env *P)
{
    return new_def(P, DEF_CONST);
}

static int print_subtypes_(paw_Env *P, Buffer *buf, struct RttiType *type)
{
    for (int i = 0; i < type->nsubtypes; ++i) {
        if (i > 0) pawL_add_string(P, buf, ", ");
        pawRtti_print_type(P, buf, type->subtypes[i]);
    }
    return type->nsubtypes;
}
#define PRINT_SUBTYPES(P, buf, type) print_subtypes_(P, buf, CAST(struct RttiType *, type))

static void print_func_type(paw_Env *P, Buffer *buf, struct RttiFnPtr *type)
{
    pawL_add_string(P, buf, "fn(");
    PRINT_SUBTYPES(P, buf, type);
    pawL_add_char(P, buf, ')');
    if (type->result > 0) {
        pawL_add_string(P, buf, " -> ");
        pawRtti_print_type(P, buf, type->result);
    }
}

static void print_tuple_type(paw_Env *P, Buffer *buf, struct RttiTuple *type)
{
    pawL_add_char(P, buf, '(');
    int const n = PRINT_SUBTYPES(P, buf, type);
    if (n == 1)
        pawL_add_char(P, buf, ',');
    pawL_add_char(P, buf, ')');
}

static void print_adt(paw_Env *P, Buffer *buf, struct RttiAdt *type)
{
    struct Def *def = RTTI_DEF(P, type->iid);
    String const *name = def->hdr.name;
    struct RttiType *base = RTTI_CAST_TYPE(type);
    pawL_add_nstring(P, buf, name->text, name->length);
    if (base->nsubtypes > 0) {
        pawL_add_char(P, buf, '<');
        print_subtypes_(P, buf, base);
        pawL_add_char(P, buf, '>');
    }
}

static void print_trait(paw_Env *P, Buffer *buf, struct RttiTrait *type)
{
    // TODO
    pawL_add_fstring(P, buf, "TODO: type.c:print_trait_obj");
}

void pawRtti_print_type(paw_Env *P, Buffer *buf, paw_Type code)
{
    struct RttiType *type = RTTI_TYPE(P, code);
    switch (type->hdr.kind) {
        case RTTI_TYPE_FN_DEF:
        case RTTI_TYPE_FN_PTR:
            print_func_type(P, buf, &type->fptr);
            break;
        case RTTI_TYPE_TUPLE:
            print_tuple_type(P, buf, &type->tuple);
            break;
        case RTTI_TYPE_ADT:
            print_adt(P, buf, &type->adt);
            break;
        case RTTI_TYPE_NEVER:
            pawL_add_char(P, buf, '!');
            break;
        case RTTI_TYPE_TRAIT:
            break;
    }
}

static void add_string_with_len(paw_Env *P, Buffer *buf, String const *str)
{
    pawL_add_int(P, buf, PAW_CAST_INT(str->length));
    pawL_add_nstring(P, buf, str->text, str->length);
}

void pawRtti_mangle_start(paw_Env *P, Buffer *buf)
{
    L_ADD_LITERAL(P, buf, "_P");
}

void pawRtti_mangle_start_generic_args(paw_Env *P, Buffer *buf)
{
    pawL_add_char(P, buf, 'I');
}

void pawRtti_mangle_finish_generic_args(paw_Env *P, Buffer *buf)
{
    pawL_add_char(P, buf, 'E');
}

void pawRtti_mangle_add_module(paw_Env *P, Buffer *buf, String const *name)
{
    pawL_add_char(P, buf, 'N');
    add_string_with_len(P, buf, name);
}

void pawRtti_mangle_add_name(paw_Env *P, Buffer *buf, String const *name)
{
    add_string_with_len(P, buf, name);
}

void pawRtti_mangle_add_arg(paw_Env *P, Buffer *buf, paw_Type code)
{
    struct RttiType *type = RTTI_TYPE(P, code);
    switch (type->hdr.kind) {
        case RTTI_TYPE_TRAIT:
            PAW_UNREACHABLE();
        case RTTI_TYPE_ADT:
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
                    struct Def const *def = RTTI_DEF(P, type->adt.iid);
                    add_string_with_len(P, buf, def->hdr.name);
                    if (type->nsubtypes > 0) {
                        pawRtti_mangle_start_generic_args(P, buf);
                        for (int i = 0; i < type->nsubtypes; ++i) {
                            pawRtti_mangle_add_arg(P, buf, type->subtypes[i]);
                        }
                        pawRtti_mangle_finish_generic_args(P, buf);
                    }
                }
            }
            break;
        case RTTI_TYPE_FN_PTR:
        case RTTI_TYPE_FN_DEF: {
            struct RttiFnPtr const func = type->fptr;
            pawL_add_char(P, buf, 'F');
            for (int i = 0; i < type->nsubtypes; ++i) {
                pawRtti_mangle_add_arg(P, buf, type->subtypes[i]);
            }
            pawL_add_char(P, buf, 'E');
            struct RttiType const *result = RTTI_TYPE(P, func.result);
            if (result->hdr.kind != RTTI_TYPE_ADT || result->adt.code != PAW_TUNIT) {
                pawRtti_mangle_add_arg(P, buf, func.result);
            }
            break;
        }
        case RTTI_TYPE_TUPLE: {
            pawL_add_char(P, buf, 'T');
            for (int i = 0; i < type->nsubtypes; ++i) {
                pawRtti_mangle_add_arg(P, buf, type->subtypes[i]);
            }
            pawL_add_char(P, buf, 'E');
            break;
        }
        case RTTI_TYPE_NEVER:
            pawL_add_char(P, buf, 'X');
            break;
    }
}
