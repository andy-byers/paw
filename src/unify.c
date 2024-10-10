// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// unify.c: type unification module

#include "compile.h"
#include "env.h"
#include "hir.h"
#include "unify.h"

#define ERROR(U, line, ...) pawE_error(ENV((U)->C), PAW_ETYPE, line, __VA_ARGS__)

typedef struct InferenceVar {
    K_ALIGNAS_NODE struct InferenceVar *parent;
    struct HirType *type;
    int rank;
} InferenceVar;

DEFINE_LIST(struct Compiler, var_list_, VarList, struct InferenceVar)

typedef struct UnificationTable {
    K_ALIGNAS_NODE struct UnificationTable *outer;
    struct VarList *ivars; // vector of type variables
    int depth; // depth of binder
} UnificationTable;

static void overwrite_type(InferenceVar *ivar, const struct HirType *src)
{
    *ivar->type = *src;
}

static InferenceVar *get_ivar(UnificationTable *table, int index)
{
    paw_assert(index < table->ivars->count);
    return table->ivars->data[index];
}

static void debug_log(struct Unifier *U, const char *what, struct HirType *a, struct HirType *b)
{
    paw_assert(a != NULL);
    paw_assert(b != NULL);

#if defined(PAW_DEBUG_LOG)
    paw_Env *P = ENV(U->C);
    pawHir_print_type(U->C, a);
    pawHir_print_type(U->C, b);
    DLOG(U->C, "(unify) %s: %s = %s",
            what, paw_string(P, -2), paw_string(P, -1));
    paw_pop(P, 2);
#else
    PAW_UNUSED(U);
    PAW_UNUSED(what);
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

static void check_occurs(struct Unifier *U, InferenceVar *ivar, struct HirType *type)
{
    if (ivar->type == type) {
        paw_assert(HirIsUnknown(type));
        ERROR(U, type->hdr.line, "encountered cyclic type");
    }
    if (!HirIsAdt(type)) return;
    struct HirAdt *adt = HirGetAdt(type);
    if (adt->types == NULL) return;
    for (int i = 0; i < adt->types->count; ++i) {
        struct HirType *subtype = K_LIST_GET(adt->types, i);
        check_occurs(U, ivar, subtype);
    }
}

static void unify_var_type(struct Unifier *U, InferenceVar *ivar, struct HirType *type)
{
    debug_log(U, "unify_var_type", ivar->type, type);

    check_occurs(U, ivar, type);
    overwrite_type(ivar, type);
}

static void unify_var_var(struct Unifier *U, InferenceVar *a, InferenceVar *b)
{
    a = find_root(a);
    b = find_root(b);

    debug_log(U, "unify_var_var", a->type, b->type);

    if (a != b) {
        link_roots(a, b);
    }
}

static struct HirType *normalize_unknown(UnificationTable *table, struct HirType *type)
{
    paw_assert(table->depth == type->unknown.depth);
    const int index = type->unknown.index;
    InferenceVar *ivar = get_ivar(table, index);
    const InferenceVar *root = find_root(ivar);
    if (!HirIsUnknown(root->type)) {
        paw_assert(ivar != root);
        overwrite_type(ivar, root->type);
    }
    return root->type;
}

static void normalize_list(UnificationTable *table, struct HirTypeList *list)
{
    if (list == NULL) return;
    for (int i = 0; i < list->count; ++i) {
        list->data[i] = pawU_normalize(table, list->data[i]);
    }
}

struct HirType *pawU_normalize(UnificationTable *table, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirFuncDef:
            normalize_list(table, HirGetFuncDef(type)->types);
            // fallthrough
        case kHirFuncPtr:
            normalize_list(table, HIR_FPTR(type)->params);
            type->fdef.result = pawU_normalize(table, HIR_FPTR(type)->result);
            break;
        case kHirUnknown:
            type = normalize_unknown(table, type);
            break;
        case kHirTupleType:
            normalize_list(table, HirGetTupleType(type)->elems);
            break;
        case kHirAdt:
            normalize_list(table, HirGetAdt(type)->types);
            break;
        case kHirGeneric:
            break;
        case kHirPathType:
            // paths are resolved
            PAW_UNREACHABLE();
    }
    return type;
}

static int unify_lists(struct Unifier *U, struct HirTypeList *a, struct HirTypeList *b)
{
    paw_assert(a && b);
    if (a->count != b->count) return -1;
    for (int i = 0; i < a->count; ++i) {
        if (U->action(U, a->data[i], b->data[i])) {
            return -1;
        }
    }
    return 0;
}

static int unify_adt(struct Unifier *U, struct HirAdt *a, struct HirAdt *b)
{
    if (a->base != b->base) return -1;
    if (!a->types != !b->types) return -1;
    if (a->types == NULL) return 0;
    return unify_lists(U, a->types, b->types);
}

static int unify_tuple(struct Unifier *U, struct HirTupleType *a, struct HirTupleType *b)
{
    return unify_lists(U, a->elems, b->elems);
}

static int unify_func(struct Unifier *U, struct HirFuncPtr *a, struct HirFuncPtr *b)
{
    if (unify_lists(U, a->params, b->params)) return -1;
    return U->action(U, a->result, b->result);
}

static int unify_generic(struct Unifier *U, struct HirGeneric *a, struct HirGeneric *b)
{
    return a->did == b->did ? 0 : -1;
}

static int unify_types(struct Unifier *U, struct HirType *a, struct HirType *b)
{
    debug_log(U, "unify_types", a, b);

    if (HirIsFuncType(a) && HirIsFuncType(b)) {
        // function pointer and definition types are compatible
        return unify_func(U, HIR_FPTR(a), HIR_FPTR(b));
    } else if (HIR_KINDOF(a) != HIR_KINDOF(b)) {
        return -1;
    } else if (HirIsTupleType(a)) {
        return unify_tuple(U, HirGetTupleType(a), HirGetTupleType(b));
    } else if (HirIsAdt(a)) {
        return unify_adt(U, HirGetAdt(a), HirGetAdt(b));
    } else if (HirIsGeneric(a)) {
        return unify_generic(U, HirGetGeneric(a), HirGetGeneric(b));
    } else {
        paw_assert(HirIsUnknown(a));
        return a == b ? 0 : -1;
    }
}

static int unify(struct Unifier *U, struct HirType *a, struct HirType *b)
{
    UnificationTable *ut = U->table;

    // Types may have already been unified. Make sure to always use the
    // cannonical type.
    a = pawU_normalize(ut, a);
    b = pawU_normalize(ut, b);
    if (HirIsUnknown(a)) {
        InferenceVar *va = get_ivar(ut, HirGetUnknown(a)->index);
        if (HirIsUnknown(b)) {
            InferenceVar *vb = get_ivar(ut, HirGetUnknown(b)->index);
            unify_var_var(U, va, vb);
        } else {
            unify_var_type(U, va, b);
        }
    } else if (HirIsUnknown(b)) {
        InferenceVar *vb = get_ivar(ut, HirGetUnknown(b)->index);
        unify_var_type(U, vb, a);
    } else {
        // Both types are known: make sure they are compatible. This is the
        // only time we can encounter an error.
        return unify_types(U, a, b);
    }
    return 0;
}

#define RUN_ACTION(U, a, b, f) ((U)->action = f)(U, a, b)

// TODO: return 0 or -1 so caller can provide better error message? or just display the type names in the error message
void pawU_unify(struct Unifier *U, struct HirType *a, struct HirType *b)
{
    const int rc = RUN_ACTION(U, a, b, unify);
    if (rc == 0) return;

    pawHir_print_type(U->C, a);
    pawHir_print_type(U->C, b);
    ERROR(U, a->hdr.line,
            "incompatible types '%s' and '%s'",
            paw_string(ENV(U->C), -2),
            paw_string(ENV(U->C), -1));
}

static int equate(struct Unifier *U, struct HirType *a, struct HirType *b)
{
    UnificationTable *ut = U->table;

    a = pawU_normalize(ut, a);
    b = pawU_normalize(ut, b);
    return unify_types(U, a, b);
}

paw_Bool pawU_equals(struct Unifier *U, struct HirType *a, struct HirType *b)
{
    return RUN_ACTION(U, a, b, equate) == 0;
}

// TODO: consider accepting the line number, or the type (containing the line number)
//       this unknown is being used to infer
struct HirType *pawU_new_unknown(struct Unifier *U, int line)
{
    paw_Env *P = ENV(U->C);
    UnificationTable *table = U->table;

    // NOTE: inference variables require a stable address, since they point to each other
    InferenceVar *ivar = pawK_pool_alloc(P, U->C->pool, sizeof(InferenceVar));
    const int index = table->ivars->count;
    var_list_push(U->C, table->ivars, ivar);

    struct HirType *type = pawHir_new_type(U->C, line, kHirUnknown);
    type->unknown.depth = table->depth;
    type->unknown.index = index;

    ivar->parent = ivar;
    ivar->type = type;
    return type;
}

void pawU_enter_binder(struct Unifier *U)
{
    paw_Env *P = ENV(U->C);
    UnificationTable *table = pawK_pool_alloc(P, U->C->pool, sizeof(UnificationTable));
    table->ivars = var_list_new(U->C);
    table->depth = U->depth;
    table->outer = U->table;
    U->table = table;
    ++U->depth;
}

static void check_table(struct Unifier *U, UnificationTable *table)
{
    for (int i = 0; i < table->ivars->count; ++i) {
        const InferenceVar *var = get_ivar(table, i);
        pawU_normalize(table, var->type);
        if (HirIsUnknown(var->type)) {
            ERROR(U, var->type->hdr.line, "unable to infer type");
        }
    }
}

void pawU_leave_binder(struct Unifier *U)
{
    check_table(U, U->table);
    U->table = U->table->outer;
    --U->depth;
}
