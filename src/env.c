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

int pawE_locate(paw_Env *P, const String *name)
{
    const struct DefList defs = P->defs;
    for (int i = 0; i < defs.count; ++i) {
        const struct Def *def = P->defs.data[i];
        if (pawS_eq(name, def->hdr.name)) {
            return i;
        }
    }
    return -1;
} 

#define BAD_DEF UINT16_C(-1)
#define BAD_VALUE UINT16_C(-1)
#define INIT_LIST(P, list, n, T) ((list).data = pawM_new_vec(P, n, T), \
                                  (list).count = (n))

static struct Type *new_type(paw_Env *P, enum TypeKind kind)
{
    // TODO: GC types
    struct Type *type = pawM_new(P, struct Type); 
    type->hdr = (struct TypeHeader){
        .kind = kind, 
    };
    return type;
}

struct Type *pawE_new_adt(paw_Env *P, int ntypes)
{
    struct Type *type = new_type(P, TYPE_ADT); 
    INIT_LIST(P, type->adt.types, ntypes, struct Type *);
    type->adt.did = BAD_DEF;
    return type;
}

struct Type *pawE_new_func(paw_Env *P, int nparams)
{
    struct Type *type = new_type(P, TYPE_FUNC); 
    INIT_LIST(P, type->func.params, nparams, struct Type *);
    return type;
}

struct Type *pawE_new_tuple(paw_Env *P, int ntypes)
{
    struct Type *type = new_type(P, TYPE_TUPLE); 
    INIT_LIST(P, type->tuple.types, ntypes, struct Type *);
    return type;
}

static struct Def *new_def(paw_Env *P, enum DefKind kind, String *name)
{
    struct Def *def = pawM_new(P, struct Def); 
    def->hdr = (struct DefHeader){
        .name = name, 
        .kind = kind,
    };
    pawM_grow(P, P->defs.data, P->defs.count, P->defs.alloc);
    P->defs.data[P->defs.count++] = def;
    return def;
}

struct AdtDef *pawE_define_adt(paw_Env *P, String *name, int nfields)
{
    struct Def *def = new_def(P, DEF_ADT, name);
    INIT_LIST(P, def->adt.fields, nfields, struct Field);
    return &def->adt;
}

struct FuncDef *pawE_define_func(paw_Env *P, String *name, int nparams)
{
    struct Def *def = new_def(P, DEF_FUNC, name);
    INIT_LIST(P, def->func.params, nparams, struct Field);
    def->func.vid = BAD_VALUE;
    return &def->func;
}

struct VarDef *pawE_define_var(paw_Env *P, String *name)
{
    struct Def *def = new_def(P, DEF_VAR, name);
    def->func.vid = BAD_VALUE;
    return &def->var;
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
    // TODO: make Type a GC'd type
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
