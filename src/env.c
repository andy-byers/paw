// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "env.h"
#include "mem.h"
#include "rt.h"
#include <limits.h>

void pawE_error(paw_Env *P, int code, int line, const char *fmt, ...)
{
    Buffer print;
    pawL_init_buffer(P, &print);
    if (line >= 0) {
        paw_assert(P->modname != NULL);
        pawL_add_fstring(P, &print, "%s:%d: ", P->modname->text, line);
    }

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &print, fmt, arg);
    va_end(arg);

    pawL_push_result(P, &print);
    pawC_throw(P, code);
}

void pawE_uninit(paw_Env *P)
{
    for (int i = 0; i < P->types.count; ++i) pawM_free(P, P->types.data[i]);
    for (int i = 0; i < P->defs.count; ++i) pawM_free(P, P->defs.data[i]);
    pawM_free_vec(P, P->types.data, P->types.alloc);
    pawM_free_vec(P, P->defs.data, P->defs.alloc);
    pawM_free_vec(P, P->vals.data, P->vals.alloc);
    memset(&P->types, 0, sizeof(P->types));
    memset(&P->defs, 0, sizeof(P->defs));
    memset(&P->vals, 0, sizeof(P->vals));
}

CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top)
{
    if (P->ncf >= ITEM_MAX) {
        pawR_error(P, PAW_EOVERFLOW, "too many nested function calls");
    }
    CallFrame *cf = pawM_new(P, CallFrame);
    P->cf->next = cf;
    cf->prev = P->cf;
    cf->next = NULL;
    cf->top.p = top;
    ++P->ncf;
    return cf;
}

int pawE_locate(paw_Env *P, const String *name, paw_Bool only_pub)
{
    const struct DefList defs = P->defs;
    for (int i = 0; i < defs.count; ++i) {
        const struct Def *def = P->defs.data[i];
        if (pawS_eq(name, def->hdr.name) && 
                def->hdr.is_pub >= only_pub) {
            return i;
        }
    }
    return -1;
} 

void pawE_init_type_list(paw_Env *P, struct TypeList *plist, int count)
{
    *plist = (struct TypeList){
        .data = pawM_new_vec(P, count, struct Type *),
        .count = count,
    };
}

void pawE_init_field_list(paw_Env *P, struct FieldList *plist, int count)
{
    *plist = (struct FieldList){
        .data = pawM_new_vec(P, count, struct Field),
        .count = count,
    };
}

struct Type *pawE_new_type(paw_Env *P, enum TypeKind kind)
{
    struct Type *type = pawM_new(P, struct Type); 
    type->hdr.kind = kind;
    return type;
}

struct Def *pawE_new_def(paw_Env *P, enum DefKind kind)
{
    pawM_grow(P, P->defs.data, P->defs.count, P->defs.alloc);
    struct Def *def = pawM_new(P, struct Def); 
    def->hdr.kind = kind;
    P->defs.data[P->defs.count++] = def; 
    return def;
}

static void print_type_list(paw_Env *P, Buffer *buffer, struct TypeList *list)
{
    for (int i = 0; i < list->count; ++i) {
        pawE_print_type(P, buffer, list->data[i]);
        if (i == list->count - 1) break;
        pawL_add_string(P, buffer, ", ");
    }
}

static void print_func_type(paw_Env *P, Buffer *buffer, struct FuncType *type)
{
    pawL_add_string(P, buffer, "fn(");
    print_type_list(P, buffer, &type->params);
    pawL_add_char(P, buffer, ')');
    if (type->result != NULL) {
        pawL_add_string(P, buffer, " -> ");
        pawE_print_type(P, buffer, type->result); 
    }
}

static void print_tuple_type(paw_Env *P, Buffer *buffer, struct TupleType *type)
{
    pawL_add_char(P, buffer, '(');
    print_type_list(P, buffer, &type->types);
    if (type->types.count == 1) pawL_add_char(P, buffer, ',');
    pawL_add_char(P, buffer, ')');
}

static void print_adt(paw_Env *P, Buffer *buffer, struct Adt *type)
{
    struct Def *def = pawE_get_def(P, type->did);
    const String *name = def->adt.name;

    pawL_add_nstring(P, buffer, name->text, name->length);
    if (type->types.count > 0) {
        pawL_add_char(P, buffer, '<');
        print_type_list(P, buffer, &type->types);
        pawL_add_char(P, buffer, '>');
    }
}

void pawE_print_type(paw_Env *P, Buffer *buffer, struct Type *type)
{
    switch (type->hdr.kind) {
        case TYPE_FUNC:
            print_func_type(P, buffer, &type->func);
            break;
        case TYPE_TUPLE:
            print_tuple_type(P, buffer, &type->tuple);
            break;
        case TYPE_ADT:
            print_adt(P, buffer, &type->adt);
    }
}

void pawE_mangle_start(paw_Env *P, Buffer *buffer, const String *name)
{
    L_ADD_STRING(P, buffer, name);
}

void pawE_mangle_add_arg(paw_Env *P, Buffer *buffer, const struct Type *type)
{
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
                const struct Def *def = pawE_get_def(P, type->adt.did);
                const struct Adt adt = def->hdr.type->adt;
                L_ADD_STRING(P, buffer, def->hdr.name);
                if (adt.types.count > 0) {
                    for (int i = 0; i < adt.types.count; ++i) {
                        pawE_mangle_add_arg(P, buffer, adt.types.data[i]);
                    }
                }
            }
            break;
        case TYPE_FUNC: {
            const struct FuncType func = type->func;
            pawL_add_char(P, buffer, 'F');
            for (int i = 0; i < func.params.count; ++i) {
                pawE_mangle_add_arg(P, buffer, func.params.data[i]);
            }
            pawL_add_char(P, buffer, '_');
            if (func.result->hdr.kind != TYPE_ADT || 
                    func.result->adt.code != PAW_TUNIT) {
                pawE_mangle_add_arg(P, buffer, func.result);
            }
            break;
        }
        case TYPE_TUPLE: {
            struct TupleType tuple = type->tuple;
            pawL_add_char(P, buffer, 't');
            for (int i = 0; i < tuple.types.count; ++i) {
                pawE_mangle_add_arg(P, buffer, tuple.types.data[i]);
            }
            pawL_add_char(P, buffer, '_');
        }
    }
}

void pawE_mangle_finish(paw_Env *P, Buffer *buffer)
{
    pawL_add_char(P, buffer, '_');
}

