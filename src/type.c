// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "type.h"
#include "env.h"
#include "mem.h"
#include "util.h"

// Storage for dummy 'null' type constant
static const Type kType0 = {.code = -1};
TypeTag kTag0 = &kType0;

paw_Bool pawY_is_similar(TypeTag type, TypeTag tag)
{
    paw_assert(type && tag); // inferred/annotated
    if (type == kTag0) {
        // any type of value can be set to 'null'
        return PAW_TRUE; 
    }
    switch (tag->code) {
        case PAW_TBOOL:
        case PAW_TINT:
        case PAW_TFLOAT:
            return type->code == PAW_TBOOL ||
                   type->code == PAW_TINT ||
                   type->code == PAW_TFLOAT;
        default:
            // types are internalized
            return type == tag;
    }
}

int pawY_common(TypeTag a, TypeTag b, TypeTag *out)
{
    if (!pawY_is_similar(a, b)) {
        return -1;
    }
    switch (a->code) {
        case PAW_TBOOL:
            *out = b;
            break;
        case PAW_TINT:
            // float takes precedence over int
            *out = b->code == PAW_TFLOAT ? b : a;
            break;
        default:
            // 'a' is float or other (a == b for other types)
            *out = a;
    }
    return 0;
}

#define same_vecs(a, b, kind) same_vecs_aux((a).kind, (a).n ## kind, \
                                            (b).kind, (b).n ## kind)
static paw_Bool same_vecs_aux(TypeTag *va, int na, TypeTag *vb, int nb)
{
    if (na != nb) {
        return PAW_FALSE;
    }
    for (int i = 0; i < na; ++i) {
        if (!pawY_is_same(va[i], vb[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

// Return PAW_TRUE if type tags 'a' and 'b' are the same, PAW_FALSE
// otherwise
// Helper for internalizing compound types: either 'a' or 'b' is a 
// copy of the Type struct in automatic memory, and the other is
// internalized. Sub types (parameters, fields, etc.) are already 
// internalized.
static paw_Bool same_tags(TypeTag a, TypeTag b)
{
    if (!a || !b) {
        return a == b;
    } else if (a->code != b->code) {
        return PAW_FALSE;
    } 
    
    switch (a->code) {
        case PAW_TARRAY:
            if (pawY_is_same(a->a.elem, b->a.elem)) {
                return a;
            }
            break;
        case PAW_TMAP:
            if (pawY_is_same(a->m.key, b->m.key) && 
                pawY_is_same(a->m.value, b->m.value)) {
                return a;
            }
            break;
        case PAW_TFUNCTION:
            if (pawY_is_same(a->f.ret, b->f.ret) &&
                same_vecs(a->f, b->f, param)) {
                return PAW_TRUE;
            }
            break;
        case PAW_TCLASS:
            if (same_vecs(a->c, b->c, fields)) {
                return PAW_TRUE;
            }
            break;
        default:
            break;
    }
    return PAW_FALSE;
    return PAW_FALSE;
}

static Type *find_compound_type(paw_Env *P, TypeTag type)
{
    struct TypeVec *tv = &P->tv;
    for (int i = 0; i < tv->size; ++i) {
        Type *t = &tv->data[i];
        if (same_tags(t, type)) {
            return t;
        }
    }
    return NULL;
}

static void set_builtin_type(paw_Env *P, const char *name, int code)
{
    String *str = pawS_new_fixed(P, name);
    str->flag = -code - 1; // encode type index
    P->tv.data[code] = (Type){.code = code};
    ++P->tv.size;
}

// TODO: Probably need to GC the types, or choose a fixed large number. Should be
// heap-allocated, since it is large. (realloc() messes up the pointers!)
#define INITIAL_SIZE 4096

void pawY_init(paw_Env *P)
{
    P->tv.data = pawM_new_vec(P, INITIAL_SIZE, Type);
    P->tv.alloc = INITIAL_SIZE;

    set_builtin_type(P, "bool", PAW_TBOOL);
    set_builtin_type(P, "int", PAW_TINT);
    set_builtin_type(P, "float", PAW_TFLOAT);
    set_builtin_type(P, "string", PAW_TSTRING);
}

void pawY_uninit(paw_Env *P)
{
    pawM_free_vec(P, P->tv.data, P->tv.alloc);
}

static Type *register_type(paw_Env *P, TypeTag tt)
{
    Type *t = find_compound_type(P, tt);
    if (t == NULL) {
        struct TypeVec *tv = &P->tv;
        if (tv->size == tv->alloc) {
            paw_assert(0); // TODO: cannot realloc, since it would mess up the pointers!
        }
        t = &tv->data[tv->size++];
        *t = *tt;
    }
    return t;
}

Type *pawY_register_function(paw_Env *P, TypeTag *param, int nparam, TypeTag ret)
{
    return register_type(P, &(Type){
        .code = PAW_TFUNCTION,
        .f = {
            .param = param,
            .nparam = nparam,
            .ret = ret,
        }, 
    });
}

Type *pawY_register_class(paw_Env *P, TypeTag *fields, int nfields)
{
    return register_type(P, &(Type){
        .code = PAW_TCLASS,
        .c = {
            .fields = fields,
            .nfields = nfields,
        }, 
    });
}

Type *pawY_register_array(paw_Env *P, TypeTag elem)
{
    return register_type(P, &(Type){
        .code = PAW_TARRAY,
        .a = {
            .elem = elem,
        }, 
    });
}

Type *pawY_register_map(paw_Env *P, TypeTag key, TypeTag value)
{
    return register_type(P, &(Type){
        .code = PAW_TMAP,
        .m = {
            .key = key,
            .value = value,
        }, 
    });
}
