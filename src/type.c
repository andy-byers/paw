// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "type.h"
#include "env.h"
#include "mem.h"
#include "util.h"

paw_Bool pawY_is_similar(TypeTag type, TypeTag tag)
{
    paw_assert(type && tag);
    switch (t_type(tag)) {
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

TypeTag pawY_unwrap(paw_Env *P, TypeTag t)
{
    TypeTag inner = NULL;
    switch (t_base(t)) {
        case PAW_TSTRING:
            inner = e_string(P);
            break;
        case PAW_TARRAY:
            inner = t->a.elem;
            break;
        case PAW_TMAP:
            inner = t->m.value;
            break;
        case PAW_TCLASS:
            // TODO: Lookup return value of __getitem attr, if it exists. need type info for 'first' and 'second' to determine overload
            paw_assert(0);
            break;
        default:
            paw_assert(0);
//            pawY_error(P, "expected container or instance type");
    }
    return inner;
}

static paw_Bool same_vecs(TypeTag *va, int na, TypeTag *vb, int nb)
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

static paw_Bool same_attrs(Attribute *va, int na, Attribute *vb, int nb)
{
    if (na != nb) {
        return PAW_FALSE;
    }
    for (int i = 0; i < na; ++i) {
        if (!pawS_eq(va[i].name, vb[i].name) ||
            !pawY_is_same(va[i].attr, vb[i].attr)) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

// Return PAW_TRUE if type tags 'a' and 'b' are the same, PAW_FALSE
// otherwise
// Helper for internalizing compound types: either 'a' or 'b' is a 
// copy of the Type struct in automatic memory, and the other is
// internalized. Sub types (parameters, attributes, etc.) are already 
// internalized.
static paw_Bool same_tags(TypeTag a, TypeTag b)
{
    if (t_base(a) != t_base(b)) {
        return PAW_FALSE;
    } 
    
    switch (t_base(a)) {
        case PAW_TARRAY:
            if (a->a.level == b->a.level &&
                pawY_is_same(a->a.elem, b->a.elem)) {
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
                same_vecs(a->f.param, a->f.nparam, b->f.param, b->f.nparam)) {
                return PAW_TRUE;
            }
            break;
        case PAW_TCLASS:
            if (pawS_eq(a->c.name, b->c.name) &&
                same_attrs(a->c.attrs, a->c.nattrs, b->c.attrs, b->c.nattrs)) {
                return PAW_TRUE;
            }
            break;
        default:
            break;
    }
    return PAW_FALSE;
}

static Type *find_compound_type(paw_Env *P, TypeTag type)
{
    struct TypeVec *tv = &P->tv;
    for (int i = 0; i < tv->size; ++i) {
        Type *t = tv->data[i];
        if (same_tags(t, type)) {
            return t;
        }
    }
    return NULL;
}

static void set_base_type(paw_Env *P, const char *name, int code)
{
    String *str = pawS_new_fixed(P, name);
    str->flag = -code - 1; // encode type index
    Type *t = pawV_new_type(P);
    P->tv.data[code] = t;
    t->code = code;
    t->base = code;
    ++P->tv.size;
}

void pawY_init(paw_Env *P)
{
    P->tv.data = pawM_new_vec(P, PAW_NTYPES, Type *);
    P->tv.alloc = PAW_NTYPES;

    set_base_type(P, "bool", PAW_TBOOL);
    set_base_type(P, "int", PAW_TINT);
    set_base_type(P, "float", PAW_TFLOAT);
    set_base_type(P, "string", PAW_TSTRING);
    set_base_type(P, "array", PAW_TARRAY);
    set_base_type(P, "map", PAW_TMAP);
    set_base_type(P, "class", PAW_TCLASS);
    set_base_type(P, "foreign", PAW_TFOREIGN);
    set_base_type(P, "type", PAW_TTYPE);
    set_base_type(P, "function", PAW_TFUNCTION);

    paw_assert(P->tv.size == PAW_NTYPES);
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
        pawM_grow(P, tv->data, tv->size, tv->alloc);
        t = pawV_new_type(P);
        const int code = tv->size++;
        tv->data[code] = t;
        *t = *tt; // set 'base' and payload
        t->code = code; // set unique 'code'
    }
    return t;
}

Type *pawY_register_function(paw_Env *P, TypeTag *param, int nparam, TypeTag ret)
{
    return register_type(P, &(Type){
        .base = PAW_TFUNCTION,
        .f = {
            .param = param,
            .nparam = nparam,
            .ret = ret,
        }, 
    });
}

Type *pawY_register_class(paw_Env *P, String *name, Attribute *attrs, int nattrs)
{
    return register_type(P, &(Type){
        .base = PAW_TCLASS,
        .c = {
            .name = name,
            .attrs = attrs,
            .nattrs = nattrs,
        }, 
    });
}

Type *pawY_register_array(paw_Env *P, TypeTag elem)
{
    ArrayType at = {.elem = elem};
    if (t_is_array(elem)) {
        at.level = elem->a.level + 1;
        at.elem = elem->a.elem;
    }
    return register_type(P, &(Type){
        .base = PAW_TARRAY,
        .a = at, 
    });
}

Type *pawY_register_map(paw_Env *P, TypeTag key, TypeTag value)
{
    return register_type(P, &(Type){
        .base = PAW_TMAP,
        .m = {
            .key = key,
            .value = value,
        }, 
    });
}

TypeTag *pawY_new_taglist(paw_Env *P, paw_Type *types, int ntypes)
{
    TypeTag *tags = pawM_new_vec(P, ntypes, TypeTag);
    for (int i = 0; i < ntypes; ++i) {
        tags[i] = e_tag(P, types[i]);
    }
    return tags;
}

void pawY_free_taglist(paw_Env *P, TypeTag *tags, int ntags)
{
    pawM_free_vec(P, tags, ntags);
}
