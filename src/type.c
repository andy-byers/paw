// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "type.h"
#include "env.h"
#include "mem.h"
#include "util.h"

// Helpers for cannonicalizing types. 
// Subtypes of arguments to match_types() must be cannonicalized via prior
// invocations of pawY_add_type(). The helpers will not recur into subtypes,
// since there are likely cycles in the type graph.

static paw_Bool match_types(const Type *a, const Type *b);

static paw_Bool match_args(Type *const *a, Type *const *b, int nargs)
{
    for (int i = 0; i < nargs; ++i) {
        if (!pawY_is_same(a[i], b[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

static paw_Bool match_fields(const NamedField *a, const NamedField *b, int nattrs)
{
    for (int i = 0; i < nattrs; ++i) {
        if (a[i].flags != b[i].flags ||
            !pawS_eq(a[i].name, b[i].name) ||
            !pawY_is_same(a[i].type, b[i].type)) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

static paw_Bool match_signature(const FunctionType *a, const FunctionType *b)
{
    return a->flags == b->flags &&
           pawY_is_same(a->ret, b->ret) &&
           a->nargs == b->nargs &&
           match_args(a->args, b->args, a->nargs);
}

static paw_Bool match_class(const ClassType *a, const ClassType *b)
{
    return a->flags == b->flags &&
           pawS_eq(a->name, b->name) &&
           pawY_is_same(a->super, b->super) &&
           a->nattrs == b->nattrs &&
           match_fields(a->attrs, b->attrs, a->nattrs);
}

static paw_Bool match_types(const Type *a, const Type *b)
{
    if (a == NULL) {
        return b == NULL;
    } else if (b == NULL) {
        return a == NULL;
    } else if (y_kind(a) != y_kind(b)) {
        return PAW_FALSE;
    }
    switch (y_kind(a)) {
        case TYPE_SIGNATURE:
            return match_signature(&a->sig, &b->sig);
        case TYPE_CLASS:
            return match_class(&a->cls, &b->cls);
        default:
            return y_id(a) == y_id(b);
    }
}

//paw_Bool pawY_is_similar(Type *type, Type *tag)
//{
//    paw_assert(type && tag);
//    switch (t_type(tag)) {
//        case PAW_TBOOL:
//        case PAW_TINT:
//        case PAW_TFLOAT:
//            return type->code == PAW_TBOOL ||
//                   type->code == PAW_TINT ||
//                   type->code == PAW_TFLOAT;
//        default:
//            // types are internalized
//            return type == tag;
//    }
//}
//
//int pawY_common(Type *a, Type *b, Type **out)
//{
//    if (!pawY_is_similar(a, b)) {
//        return -1;
//    }
//    switch (a->code) {
//        case PAW_TBOOL:
//            *out = b;
//            break;
//        case PAW_TINT:
//            // float takes precedence over int
//            *out = b->code == PAW_TFLOAT ? b : a;
//            break;
//        default:
//            // 'a' is float or other (a == b for other types)
//            *out = a;
//    }
//    return 0;
//}
//
//Type *pawY_unwrap(paw_Env *P, Type *t)
//{
//    Type *inner = NULL;
//    switch (t_base(t)) {
//        case PAW_TSTRING:
//            inner = e_string(P);
//            break;
//        case PAW_TARRAY:
//            inner = t->a.elem;
//            break;
//        case PAW_TMAP:
//            inner = t->m.value;
//            break;
//        case PAW_TCLASS:
//            // TODO: Lookup return value of __getitem attr, if it exists. need type info for 'first' and 'second' to determine overload
//            paw_assert(0);
//            break;
//        default:
//            paw_assert(0);
////            pawY_error(P, "expected container or instance type");
//    }
//    return inner;
//}
//
//static paw_Bool same_vecs(Type **va, int na, Type **vb, int nb)
//{
//    if (na != nb) {
//        return PAW_FALSE;
//    }
//    for (int i = 0; i < na; ++i) {
//        if (!pawY_is_same(va[i], vb[i])) {
//            return PAW_FALSE;
//        }
//    }
//    return PAW_TRUE;
//}
//
//static paw_Bool same_attrs(Attribute *va, int na, Attribute *vb, int nb)
//{
//    if (na != nb) {
//        return PAW_FALSE;
//    }
//    for (int i = 0; i < na; ++i) {
//        if (!pawS_eq(va[i].name, vb[i].name) ||
//            !pawY_is_same(va[i].attr, vb[i].attr)) {
//            return PAW_FALSE;
//        }
//    }
//    return PAW_TRUE;
//}
//
//// Return PAW_TRUE if type tags 'a' and 'b' are the same, PAW_FALSE
//// otherwise
//// Helper for internalizing compound types: either 'a' or 'b' is a 
//// copy of the Type struct in automatic memory, and the other is
//// internalized. Sub types (parameters, attributes, etc.) are already 
//// internalized.
//static paw_Bool same_tags(Type *a, Type *b)
//{
//    if (t_base(a) != t_base(b)) {
//        return PAW_FALSE;
//    } 
//    
//    switch (t_kind(a)) {
//        case PAW_TFUNCTION:
//            if (pawY_is_same(a->f.ret, b->f.ret) &&
//                same_vecs(a->f.param, a->f.nparam, b->f.param, b->f.nparam)) {
//                return PAW_TRUE;
//            }
//            break;
//        case PAW_TCLASS:
//            if (pawS_eq(a->c.name, b->c.name) &&
//                same_attrs(a->c.attrs, a->c.nattrs, b->c.attrs, b->c.nattrs)) {
//                return PAW_TRUE;
//            }
//            break;
//        default:
//            break;
//    }
//    return PAW_FALSE;
//}
//
//static Type *find_compound_type(paw_Env *P, Type *type)
//{
//    struct TypeVec *tv = &P->tv;
//    for (int i = 0; i < tv->size; ++i) {
//        Type *t = tv->data[i];
//        if (same_tags(t, type)) {
//            return t;
//        }
//    }
//    return NULL;
//}

static void set_base_type(paw_Env *P, const char *name, int id, TypeKind kind)
{
    String *str = pawS_new_fixed(P, name);
    str->flag = -id - 1; // encode type index
    Type *t = pawY_new_type(P);
    P->mod->types[id] = t;
    t->hdr.kind = kind;
    t->hdr.id = id;
    ++P->mod->ntypes;
}

void pawY_init(paw_Env *P)
{
    P->mod = pawM_new(P, ModuleType);
    P->mod->types = pawM_new_vec(P, PAW_NTYPES, Type *);
    P->mod->capacity = PAW_NTYPES;

    set_base_type(P, "unit", PAW_TUNIT, TYPE_PRIMITIVE);
    set_base_type(P, "bool", PAW_TBOOL, TYPE_PRIMITIVE);
    set_base_type(P, "int", PAW_TINT, TYPE_PRIMITIVE);
    set_base_type(P, "float", PAW_TFLOAT, TYPE_PRIMITIVE);
    set_base_type(P, "string", PAW_TSTRING, TYPE_PRIMITIVE);
    set_base_type(P, "array", PAW_TARRAY, 0);
    set_base_type(P, "enum", PAW_TENUM, 0);
    set_base_type(P, "tuple", PAW_TTUPLE, 0);
    set_base_type(P, "function", PAW_TFUNCTION, TYPE_SIGNATURE);
    set_base_type(P, "class", PAW_TCLASS, TYPE_CLASS);
    set_base_type(P, "foreign", PAW_TFOREIGN, TYPE_CLASS);
    set_base_type(P, "module", PAW_TMODULE, 0);

    paw_assert(P->mod->ntypes == PAW_NTYPES);
}

void pawY_uninit(paw_Env *P)
{
//    pawM_free_vec(P, P->tv.data, P->tv.alloc);
}

Type *pawY_new_type(paw_Env *P)
{
    return pawM_new(P, Type);
}

Type *pawY_add_type(paw_Env *P, ModuleType *mod, const Type *type)
{
    for (int i = 0; i < mod->ntypes; ++i) {
        if (match_types(type, mod->types[i])) {
            return mod->types[i];
        }
    }
    pawM_grow(P, mod->types, mod->ntypes, mod->capacity);
    const int index = mod->ntypes++;
    Type **ptarget = &mod->types[index];
    *ptarget = pawM_new(P, Type); // cannonical version
    **ptarget = *type; // copy dummy data
    (*ptarget)->hdr.id = index;
    return *ptarget;
}

