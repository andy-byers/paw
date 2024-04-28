// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// unify.c: type unification module

#include "code.h"
#include "parse.h"
#include "mem.h"

#define PAW_DEBUG_UNIFY 1

#ifdef PAW_DEBUG_UNIFY
# define debug_log(what, ...) log_unification(what, __VA_ARGS__)
#else
# define debug_log(what, ...)
#endif

#define unpack_var(v) \
    (TypeVar){ \
        .type = (v)->type, \
        .resolved = (v)->resolved, \
    }
#define pack_type(t) \
    (TypeVar){ \
        .type = (t), \
        .resolved = PAW_TRUE, \
    }

typedef struct UniVar {
    struct UniVar *parent;
    Type *type;
    int rank;
    int depth;
    paw_Bool resolved: 1;
} UniVar;

typedef struct UniTable {
    struct UniTable *outer;
    struct UniVar **vars; // vector of type variables
    int nvars; // number of type variables
    int capacity; // capacity of vector
} UniTable;

static void dump_type(FILE *out, const Type *type)
{
    switch (y_kind(type)) {
        case TYPE_BASIC:
            switch (type->hdr.def) {
                case PAW_TUNIT:
                    fprintf(out, "()");
                    break;
                case PAW_TBOOL:
                    fprintf(out, "bool");
                    break;
                case PAW_TINT:
                    fprintf(out, "int");
                    break;
                case PAW_TFLOAT:
                    fprintf(out, "float");
                    break;
                default:
                    paw_assert(type->hdr.def == PAW_TSTRING);
                    fprintf(out, "string");
            }
            break;
        case TYPE_FUNC:
            fprintf(out, "fn(");
            for (int i = 0; i < type->func.params.count; ++i) {
                dump_type(out, type->func.params.types[i]); 
                if (i < type->func.params.count - 1) {
                    fprintf(out, ", ");
                }
            }
            fprintf(out, ") -> ");
            dump_type(out, type->func.return_);
            break;
        case TYPE_ADT:
            fprintf(out, "%d", type->adt.target); // TODO: Print the name
            if (type->adt.types.count > 0) {
                fprintf(out, "[");
                const Binder *binder = &type->adt.types;
                for (int i = 0; i < binder->count; ++i) {
                    dump_type(out, binder->types[i]); 
                    if (i < binder->count - 1) {
                        fprintf(out, ", ");
                    }
                }
                fprintf(out, "]");
            }
            break;
        default:
            paw_assert(y_is_type_var(type));
            fprintf(out, "?%s", type->var.name->text);
    }
}

static void log_unification(const char *what, Type *a, Type *b)
{
    paw_assert(a && b);
    printf("%s: ", what);
    dump_type(stdout, a);
    fprintf(stdout, " = ");
    dump_type(stdout, b);
    fprintf(stdout, "\n");
}

static UniVar *find_root(UniVar *uvar)
{
    UniVar *up = uvar->parent;
    if (up != uvar) {
        up = uvar->parent = find_root(up);
    }
    return up;
}

static void link_roots(UniVar *a, UniVar *b)
{
    if (a->rank < b->rank) {
        a->parent = b;
    } else {
        b->parent = a;
        a->rank += a->rank == b->rank;
    }
}

static void unify_var_type(UniVar *uvar, Type *type)
{
    debug_log("unify_var_type", uvar->type, type);

    uvar->type = type; 
}

static void unify_var_var(UniVar *a, UniVar *b)
{
    a = find_root(a);
    b = find_root(b);

    debug_log("unify_var_var", a->type, b->type); 

    if (a != b) {
        link_roots(a, b);
    }
}

// Check if a type is resolved, that is, if it is a concrete type or a free 
// type variable
#define is_resolved(U, v) (!y_is_type_var(v) || !p_is_bound(U, v))

Type *pawP_normalize(Unifier *U, Type *type)
{
    if (!is_resolved(U, type)) {
        const int index = type->var.index;
        UniVar *uvar = U->table->vars[index];
        uvar = find_root(uvar); // normalize
        return uvar->type;
    }
    return type;
}

static void unify_binders(Unifier *U, Binder *a, Binder *b)
{
    if (a->count != b->count) {
        pawX_error(U->lex, "arity mismatch");
    }
    for (int i = 0; i < a->count; ++i) {
        pawP_unify(U, a->types[i], b->types[i]);
    }
}

static void unify_adt(Unifier *U, Adt *a, Type *b)
{
    if (y_is_type_var(b)) {
        return; // ignore free type variables
    } else if (!y_is_adt(b)) {
        pawX_error(U->lex, "expected struct or enum type");
    } else if (a->target != b->adt.target) {
        pawX_error(U->lex, "data types are incompatible");
    }
    unify_binders(U, &a->types, &b->adt.types);
}

static void unify_func_sig(Unifier *U, FuncSig *a, Type *b)
{
    if (!y_is_func(b)) {
        pawX_error(U->lex, "expected function type");
    }
    // NOTE: 'types' field not unified (not part of function signature)
    unify_binders(U, &a->params, &b->func.params);
    pawP_unify(U, a->return_, b->func.return_);
}

static Type *unify_basic(Unifier *U, Type *a, Type *b)
{
    // basic types are cannonicalized
    if (a != b) {
        pawX_error(U->lex, "basic types are incompatible");
    }
    return a;
}

static void unify_types(Unifier *U, Type *a, Type *b)
{
    debug_log("unify_types", a, b); 
    if (y_is_type_var(a) || y_is_type_var(b)) {
        // Don't worry about free generics, they will be type checked during
        // instantiation of their containing template.
    } else if (y_is_func(a)) {
        unify_func_sig(U, &a->func, b);
    } else if (y_is_adt(a)) {
        unify_adt(U, &a->adt, b);
    } else {
        unify_basic(U, a, b);
    }
}

// TODO: Indicate failure rather than throw errors inside, let the caller throw, for better error messages
void pawP_unify(Unifier *U, Type *a, Type *b)
{
    UniTable *ut = U->table;

    // Types may have already been unified. Make sure to always use the
    // cannonical type.
    a = pawP_normalize(U, a);
    b = pawP_normalize(U, b);
    if (!is_resolved(U, a)) {
        UniVar *va = ut->vars[a->var.index];
        if (!is_resolved(U, b)) {
            UniVar *vb = ut->vars[b->var.index];
            unify_var_var(va, vb);
        } else {
            unify_var_type(va, b);
        }
    } else if (!is_resolved(U, b)) {
        UniVar *vb = ut->vars[b->var.index];
        unify_var_type(vb, a);
    } else {
        // Both types are known: make sure they are compatible. This is the
        // only time pawP_unify can encounter an error.
        unify_types(U, a, b);
    }
}

void pawP_new_type_var(Unifier *U, Type *type)
{
    debug_log("new_type_var", type, type);

    paw_Env *P = env(U->lex);
    UniTable *table = U->table;

    // add a new set to the forest
    pawM_grow(P, table->vars, table->nvars, table->capacity);
    UniVar *uvar = pawM_new(P, UniVar);
    const int index = table->nvars++;
    table->vars[index] = uvar;

    // set contains only 'type'
    uvar->parent = uvar;
    uvar->type = type;

    type->var.depth = U->depth;
    type->var.index = index;
}

void pawP_unifier_enter(Unifier *U, UniTable *table)
{
    paw_Env *P = env(U->lex);
    if (table == NULL) {
        table = pawM_new(P, UniTable);
    }
    table->outer = U->table;
    U->table = table;
    ++U->depth;
}

static void free_uni_table(Unifier *U, UniTable *table)
{
    paw_Env *P = env(U->lex);
    for (int i = 0; i < table->nvars; ++i) {
        pawM_free(P, table->vars[i]);
    }
    pawM_free(P, table);
}

void pawP_unifier_replace(Unifier *U, UniTable *table)
{
    table->outer = U->table->outer;
    free_uni_table(U, U->table);
    U->table = table;    
}

UniTable *pawP_unifier_leave(Unifier *U)
{
    UniTable *table = U->table;
    U->table = U->table->outer;
    --U->depth;
    return table;
}
