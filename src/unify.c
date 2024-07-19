// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// unify.c: type unification module

#include "ast.h"
#include "code.h"
#include "env.h"
#include "parse.h"

#define error(U, ...) pawE_error(env(U->lex), PAW_ETYPE, -1, __VA_ARGS__)

typedef struct InferenceVar {
    struct InferenceVar *parent;
    AstType *type;
    int rank;
} InferenceVar;

typedef struct UnificationTable {
    struct UnificationTable *outer;
    AstList *ivars; // vector of type variables
    int depth; // depth of binder
} UnificationTable;

static void overwrite_type(InferenceVar *ivar, const AstType *src)
{
    *ivar->type = *src;
}

static InferenceVar *get_ivar(UnificationTable *table, int index)
{
    paw_assert(index < table->ivars->count);
    return table->ivars->data[index];
}

static void debug_log(const char *what, AstType *a, AstType *b)
{
#ifdef PAW_DEBUG_UNIFY
    paw_assert(a && b);
    printf("%s: ", what);
    pawA_repr_type(stdout, a);
    fprintf(stdout, " = ");
    pawA_repr_type(stdout, b);
    fprintf(stdout, "\n");
#else
    paw_unused(what);
    paw_unused(a);
    paw_unused(b);
#endif
}

static InferenceVar *find_root(InferenceVar *ivar)
{
    InferenceVar *up = ivar->parent;
    if (up != ivar) {
        up = find_root(up);
        ivar->parent = up;
    }
    return up;
}

static void link_roots(InferenceVar *a, InferenceVar *b)
{
    if (a->rank < b->rank) {
        a->parent = b;
    } else {
        b->parent = a;
        a->rank += a->rank == b->rank;
    }
}

static void unify_var_type(InferenceVar *ivar, AstType *type)
{
    debug_log("unify_var_type", ivar->type, type);

    overwrite_type(ivar, type);
}

static void unify_var_var(InferenceVar *a, InferenceVar *b)
{
    a = find_root(a);
    b = find_root(b);

    debug_log("unify_var_var", a->type, b->type);

    if (a != b) {
        link_roots(a, b);
    }
}

static AstType *normalize_unknown(UnificationTable *table, AstType *type)
{
    paw_assert(table->depth == type->unknown.depth);
    const int index = type->unknown.index;
    InferenceVar *ivar = get_ivar(table, index);
    const InferenceVar *root = find_root(ivar);
    if (!a_is_unknown(root->type)) {
        paw_assert(ivar != root);
        overwrite_type(ivar, root->type);
    }
    return root->type;
}

static void normalize_list(UnificationTable *table, AstList *list)
{
    if (list == NULL) {
        return;
    }
    for (int i = 0; i < list->count; ++i) {
        list->data[i] = pawU_normalize(table, list->data[i]);
    }
}

AstType *pawU_normalize(UnificationTable *table, AstType *type)
{
    switch (a_kind(type)) {
        case AST_TYPE_FUNC:
            normalize_list(table, type->func.types);
            // fallthrough
        case AST_TYPE_FPTR:
            normalize_list(table, type->func.params);
            type->func.result = pawU_normalize(table, type->func.result);
            break;
        case AST_TYPE_UNKNOWN:
            type = normalize_unknown(table, type);
            break;
        case AST_TYPE_TUPLE:
            normalize_list(table, type->tuple.elems);
            break;
        case AST_TYPE_ADT:
            normalize_list(table, type->adt.types);
            break;
        default:
            break;
    }
    return type;
}

static void unify_lists(Unifier *U, AstList *a, AstList *b)
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
    paw_assert(!a->types == !b->types);
    if (a->types != NULL) {
        unify_lists(U, a->types, b->types);
    }
}

static void unify_tuple(Unifier *U, AstTupleType *a, AstTupleType *b)
{
    unify_lists(U, a->elems, b->elems);
}

static void unify_func(Unifier *U, AstFuncPtr *a, AstFuncPtr *b)
{
    unify_lists(U, a->params, b->params);
    pawU_unify(U, a->result, b->result);
}

static AstType *unify_generic(Unifier *U, AstType *a, AstType *b)
{
    if (a->generic.did != b->generic.did) {
        error(U, "generic types are incompatible");
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
        paw_assert(a_kind(a) == AST_TYPE_GENERIC);
        unify_generic(U, a, b);
    }
}

// TODO: Indicate failure rather than throw errors inside, let the caller throw,
// for better error messages
void pawU_unify(Unifier *U, AstType *a, AstType *b)
{
    UnificationTable *ut = U->table;

    // Types may have already been unified. Make sure to always use the
    // cannonical type.
    a = pawU_normalize(ut, a);
    b = pawU_normalize(ut, b);
    if (a_is_unknown(a)) {
        InferenceVar *va = get_ivar(ut, a->unknown.index);
        if (a_is_unknown(b)) {
            InferenceVar *vb = get_ivar(ut, b->unknown.index);
            unify_var_var(va, vb);
        } else {
            unify_var_type(va, b);
        }
    } else if (a_is_unknown(b)) {
        InferenceVar *vb = get_ivar(ut, b->unknown.index);
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
    UnificationTable *table = U->table;

    // NOTE: inference variables require a stable address, since they point to
    //       each other
    InferenceVar *ivar = pawK_pool_alloc(P, &U->ast->sequences, sizeof(InferenceVar),
                                         paw_alignof(InferenceVar));
    const int index = table->ivars->count;
    pawA_list_push(U->ast, &table->ivars, ivar);

    AstType *type = pawA_new_type(ast, AST_TYPE_UNKNOWN);
    type->unknown.depth = table->depth;
    type->unknown.index = index;

    ivar->parent = ivar;
    ivar->type = type;
    return type;
}

void pawU_enter_binder(Unifier *U)
{
    paw_Env *P = env(U->lex);
    UnificationTable *table = pawK_pool_alloc(
            P, &U->ast->symbols, 
            sizeof(UnificationTable), 
            paw_alignof(UnificationTable));
    table->ivars = pawA_list_new(U->ast);
    table->depth = U->depth;
    table->outer = U->table;
    U->table = table;
    ++U->depth;
}

static void check_table(Unifier *U, UnificationTable *table)
{
    for (int i = 0; i < table->ivars->count; ++i) {
        const InferenceVar *var = get_ivar(table, i);
        pawU_normalize(table, var->type);
        if (a_is_unknown(var->type)) {
            error(U, "unable to infer type");
        }
    }
}

void pawU_leave_binder(Unifier *U)
{
    check_table(U, U->table);
    U->table = U->table->outer;
    --U->depth;
}
