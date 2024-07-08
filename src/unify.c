// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// unify.c: type unification module

#include "ast.h"
#include "code.h"
#include "mem.h"
#include "parse.h"

#define PAW_DEBUG_UNIFY

#ifdef PAW_DEBUG_UNIFY
#define debug_log(what, ...) log_unification(what, __VA_ARGS__)
#else
#define debug_log(what, ...)
#endif

#define error(U, ...) pawE_error(env(U->lex), PAW_ETYPE, -1, __VA_ARGS__)

typedef struct UniVar {
    struct UniVar *parent;
    AstType *type;
    int rank;
} UniVar;

static void dump_type(FILE *out, const AstType *type)
{
    switch (a_kind(type)) {
        case AST_TYPE_TUPLE:
            fprintf(out, "(");
            for (int i = 0; i < type->tuple.elems->count; ++i) {
                dump_type(out, type->tuple.elems->data[i]);
                if (i < type->tuple.elems->count - 1) {
                    fprintf(out, ", ");
                }
            }
            fprintf(out, ")");
            break;
        case AST_TYPE_FPTR:
        case AST_TYPE_FUNC:
            fprintf(out, "fn(");
            for (int i = 0; i < type->fptr.params->count; ++i) {
                dump_type(out, type->fptr.params->data[i]);
                if (i < type->fptr.params->count - 1) {
                    fprintf(out, ", ");
                }
            }
            fprintf(out, ") -> ");
            dump_type(out, type->fptr.result);
            break;
        case AST_TYPE_ADT:
            fprintf(out, "%d", type->adt.base); // TODO: Print the name
            if (type->adt.types != NULL) {
                fprintf(out, "<");
                const AstList *binder = type->adt.types;
                for (int i = 0; i < binder->count; ++i) {
                    dump_type(out, binder->data[i]);
                    if (i < binder->count - 1) {
                        fprintf(out, ", ");
                    }
                }
                fprintf(out, ">");
            }
            break;
        case AST_TYPE_UNKNOWN:
            fprintf(out, "?%d", type->unknown.index);
            break;
        default:
            paw_assert(a_is_generic(type));
            fprintf(out, "?%s", type->generic.name->text);
    }
}

static void log_unification(const char *what, AstType *a, AstType *b)
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

static void unify_var_type(UniVar *uvar, AstType *type)
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

AstType *pawU_normalize(UniTable *table, AstType *type)
{
    if (a_is_unknown(type)) {
        paw_assert(table->depth == type->unknown.depth);
        const int index = type->unknown.index;
        UniVar *uvar = table->vars[index];
        uvar = find_root(uvar); // normalize
        return uvar->type;
    }
    return type;
}

static void unify_binders(Unifier *U, AstList *a, AstList *b)
{
    if (a->count != b->count) {
        error(U, "arity mismatch");
    }
    for (int i = 0; i < a->count; ++i) {
        pawU_unify(U, a->data[i], b->data[i]);
    }
}

static void unify_adt(Unifier *U, AstAdt *a, AstAdt *b)
{
    if (a->base != b->base) {
        error(U, "data types are incompatible");
    }
    paw_assert(!a->types == !b->types); // TODO: may need to materialize type args for one side in some situations
    if (a->types != NULL) {
        unify_binders(U, a->types, b->types);
    }
}

static void unify_tuple(Unifier *U, AstTupleType *a, AstTupleType *b)
{
    unify_binders(U, a->elems, b->elems);
}

static void unify_func(Unifier *U, AstFuncPtr *a, AstFuncPtr *b)
{
    unify_binders(U, a->params, b->params);
    pawU_unify(U, a->result, b->result);
}

static AstType *unify_basic(Unifier *U, AstType *a, AstType *b)
{
    // basic types are cannonicalized
    if (a != b) {
        error(U, "basic types are incompatible");
    }
    return a;
}

static void unify_types(Unifier *U, AstType *a, AstType *b)
{
    debug_log("unify_types", a, b);
    if (a_is_func(a) && a_is_func(b)) {
        // function pointer and definition types are compatible
        unify_func(U, &a->fptr, &b->fptr);
    } else if (a_kind(a) != a_kind(b)) {
        error(U, "incompatible types");
    } else if (a_is_tuple(a)) {
        unify_tuple(U, &a->tuple, &b->tuple);
    } else if (a_is_adt(a)) {
        unify_adt(U, &a->adt, &b->adt);
    } else {
        unify_basic(U, a, b);
    }
}

// TODO: Indicate failure rather than throw errors inside, let the caller throw,
// for better error messages
void pawU_unify(Unifier *U, AstType *a, AstType *b)
{
    UniTable *ut = U->table;

    // Types may have already been unified. Make sure to always use the
    // cannonical type.
    a = pawU_normalize(ut, a);
    b = pawU_normalize(ut, b);
    if (a_is_unknown(a)) {
        UniVar *va = ut->vars[a->unknown.index];
        if (a_is_unknown(b)) {
            UniVar *vb = ut->vars[b->unknown.index];
            unify_var_var(va, vb);
        } else {
            unify_var_type(va, b);
        }
    } else if (a_is_unknown(b)) {
        UniVar *vb = ut->vars[b->unknown.index];
        unify_var_type(vb, a);
    } else {
        // Both types are known: make sure they are compatible. This is the
        // only time pawU_unify can encounter an error.
        unify_types(U, a, b);
    }
}

AstType *pawU_new_unknown(Unifier *U)
{
    paw_Env *P = env(U->lex);
    Ast *ast = U->lex->pm->ast;
    UniTable *table = U->table;

    pawM_grow(P, table->vars, table->nvars, table->capacity);
    UniVar *uvar = pawM_new(P, UniVar);
    const int index = table->nvars++;
    table->vars[index] = uvar;

    AstType *type = pawA_new_type(ast, AST_TYPE_UNKNOWN);
    type->unknown.depth = table->depth;
    type->unknown.index = index;

    uvar->parent = uvar;
    uvar->type = type;
    return type;
}

void pawU_enter_binder(Unifier *U)
{
    paw_Env *P = env(U->lex);
    UniTable *table = pawM_new(P, UniTable);
    table->depth = U->depth;
    table->outer = U->table;
    U->table = table;
    ++U->depth;
}

// static void free_uni_table(Unifier *U, UniTable *table)
//{
//     paw_Env *P = env(U->lex);
//     for (int i = 0; i < table->nvars; ++i) {
//         pawM_free(P, table->vars[i]);
//     }
//     pawM_free(P, table);
// }

UniTable *pawU_leave_binder(Unifier *U)
{
    UniTable *table = U->table;
    U->table = U->table->outer;
    --U->depth;
    return table;
}

void pawU_check_table(Unifier *U, UniTable *table)
{
    for (int i = 0; i < table->nvars; ++i) {
        const UniVar *var = table->vars[i];
        const AstType *type = pawU_normalize(table, var->type);
        if (a_is_unknown(type)) {
            error(U, "unable to infer type");
        }
    }
}
