// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "solve.h"
#include "ir_type.h"
#include "type_folder.h"
#include "unify.h"

#include"stdio.h"

#define TODO ((struct SourceLoc){0})

#define PAW_SOLVER_DEBUG
#if defined(PAW_SOLVER_DEBUG)
# define LOGLN(Fmt_, ...) fprintf(stderr, "(paw_solver) "Fmt_ "\n", __VA_ARGS__)
#else
# define LOGLN(...)
#endif

struct IrSolver {
    IrSolver *outer;
    IrObligations *obligations;
    IrObligations *preconditions;
    struct Compiler *C;
    struct Unifier *U;
};

static void dump_obligations(IrSolver const *S)
{
    if (S->obligations->count > 0) {
        printf("obligations:\n");
        K_LIST_XFOREACH(S->obligations, struct IrObligation const, p){
            printf("  %s: %s\n", pawIr_print_type(S->C, p->type), pawIr_print_trait(S->C, p->trait));
        }
    }
}

IrSolver *pawIr_push_solver(struct Compiler *C)
{
    IrSolver *S = P_ALLOC(C, NULL, 0, sizeof(IrSolver));
    *S = (IrSolver){
        .obligations = IrObligations_new(C),
        .preconditions = IrObligations_new(C),
        .outer = C->S,
        .U = C->U,
        .C = C,
    };
    C->S = S;
    return S;
}

void pawIr_pop_solver(struct Compiler *C)
{
    C->S = C->S->outer;
}

void pawIr_solver_add_precondition(IrSolver *S, IrType *type, IrTrait *trait)
{
    IrObligations_push(S->C, S->preconditions, (struct IrObligation){
                .type = type,
                .trait = trait,
            });
}

static IrTrait *substitute_trait(struct Compiler *C, IrTrait *trait, struct Substitution subst)
{
    IrTypeList *types = IrTypeList_new(C);
    IrTypeList_reserve(C, types, trait->types->count);
    K_LIST_XFOREACH (trait->types, IrType *const, p) {
        IrType *t = pawP_substitute(C, TODO, *p, subst);
        IrTypeList_push(C, types, t);
    }
    return pawIr_new_trait(C, trait->did, types);
}

void pawIr_solver_add_obligation(IrSolver *S, IrType *type, IrTrait *trait)
{
    IrObligations_push(S->C, S->obligations, (struct IrObligation){
            .trait = trait,
            .type = type,
        });
}

// Collect all trait obligations specified by a binder
// For example, given the binder
//     <A: First<B> + Second, B: Third>
//
// this function will produce obligations
//     A: First<B>
//     A: Second
//     B: Third
static void collect_obligations_from(IrSolver *S, IrTypeList *params, IrTypeList *args)
{
    if (params != NULL) {
        IrType *const *pp, *const *pa;
        K_LIST_ZIP(params, pp, args, pa) {
            DeclId const did = IrGetGeneric(*pp)->did;
            IrTraitList *bounds = pawIr_get_trait_bounds(S->C, did);
            if (bounds != NULL) {
                K_LIST_XFOREACH(bounds, IrTrait *const, t) {
                    struct Substitution const subst = {params, args};
                    IrTrait *trait = substitute_trait(S->C, *t, subst);
                    IrObligations_push(S->C, S->obligations, (struct IrObligation){
                            .trait = trait,
                            .type = *pa,
                        });
                }
            }
        }
    }
}

static void unify(struct Compiler *C, int modno, struct SourceLoc loc, IrType *a, IrType *b)
{
    if (pawU_unify(C->U, a, b) != 0) {
        char const *lhs = pawIr_print_type(C, a);
        char const *rhs = pawIr_print_type(C, b);
        printf("%s:%d:%d: \"%s := %s\" is false\n", ModuleInfo_get(C->modinfo, modno).name->text, loc.line, loc.column, lhs, rhs);
        __builtin_trap();
    }
}

static paw_Bool impl_is_compatible(struct Compiler *C, IrType *self, struct IrImpl const *impl)
{
    // save the current position in the unification table
    int const save = pawU_current_position(C->U);
    IrSolver *child = pawIr_push_solver(C);

    struct IrImplInstance const inst = pawIr_solver_instantiate_impl(child, impl->did);
    paw_Bool const matches =
        pawU_unify(C->U, inst.type, self) == 0
        && pawIr_solver_solve(child) >= 0;
    pawIr_solver_rollback(child);

    // erase all inference variables created in this function
    pawU_undo_unifications(C->U, save);
    pawIr_pop_solver(C);
    return matches;
}

static IrTypeList *collect_types(struct Compiler *C, IrTypeList *types)
{
    IrTypeList *result = IrTypeList_new(C);
    if (types != NULL) {
        struct IrType *const *p;
        K_LIST_FOREACH (types, p) {
            IrType *type = pawP_generalize(C, (struct SourceLoc){0}, *p);
            IrTypeList_push(C, result, type);
        }
    }
    return result;
}

static paw_Bool trait_equals(IrSolver *S, IrType *x, IrType *y)
{
    if (IR_TYPE_DID(x).value != IR_TYPE_DID(y).value)
        return PAW_FALSE;

    IrTypeList *x_subtypes = IR_TYPE_SUBTYPES_(x);
    IrTypeList *y_subtypes = IR_TYPE_SUBTYPES_(y);
    if (x_subtypes != NULL) {
        paw_assert(y_subtypes != NULL);
        IrType *const *px, *const *py;
        K_LIST_ZIP(x_subtypes, px, y_subtypes, py) {
            if (!pawU_equals(S->U, *px, *py)) {
                return PAW_FALSE;
            }
        }
    }

    return PAW_TRUE;
}

//TODO static int unify_traits(IrSolver *S, IrType *x, IrType *y)
//TODO {
//TODO     if (IR_TYPE_DID(x).value != IR_TYPE_DID(y).value)
//TODO         return -1;
//TODO
//TODO     int const position = pawU_current_position(S->U);
//TODO     IrTypeList *x_subtypes = IR_TYPE_SUBTYPES_(x);
//TODO     IrTypeList *y_subtypes = IR_TYPE_SUBTYPES_(y);
//TODO     if (x_subtypes != NULL) {
//TODO         paw_assert(y_subtypes != NULL);
//TODO         IrType *const *px, *const *py;
//TODO         K_LIST_ZIP(x_subtypes, px, y_subtypes, py) {
//TODO             if (pawU_unify(S->U, *px, *py) != 0) {
//TODO                 pawU_undo_unifications(S->U, position);
//TODO                 return -1;
//TODO             }
//TODO         }
//TODO     }
//TODO
//TODO     return 0;
//TODO }

static paw_Bool type_implements_trait(IrSolver *S, IrType *self, IrTrait *impl_trait)
{
    struct Compiler *C = S->C;
    int const modno = 0; // TODO
    Str const *modname = ModuleInfo_get(C->modinfo, modno).name;
    struct SourceLoc const loc = {0};
    IrType *result = NULL;

    // add instantiated trait type from trait impl blocks where the context type is
    // compatible with "self"
    K_LIST_XFOREACH (C->impls.trait, DeclId const, p) {
        struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
        if (impl_is_compatible(C, self, impl)) {
            IrSolver *child = pawIr_push_solver(C);
            int const snapshot = pawU_current_position(C->U);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(child, impl->did);
            unify(C, modno, loc, inst.type, self);
            if (pawIr_unify_traits(C, inst.trait, impl_trait) == 0) {
                if (result != NULL)
                    pawErr_generic_error(ENV(C), modname, loc, "multiple applicable traits");
                pawIr_solver_commit(child);
                result = inst.type;
                LOGLN("proved \"%s: %s\" using trait impl block DefId(%d)",
                        pawIr_print_type(S->C, pawU_normalize(S->U, inst.type)),
                        pawIr_print_trait(S->C, pawIr_normalize_trait(S->C, inst.trait)),
                        impl->did.value);
            } else {
                pawU_undo_unifications(C->U, snapshot);
                pawIr_solver_rollback(child);
            }
            pawIr_pop_solver(C);
        }
    }

    // add instantiated trait types from blanket impl blocks
    K_LIST_XFOREACH (C->impls.blanket, DeclId const, p) {
        struct IrImpl const *impl = pawIr_get_impl_def(C, *p);
        if (impl_is_compatible(C, self, impl)) {
            IrSolver *child = pawIr_push_solver(C);
            int const snapshot = pawU_current_position(C->U);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(S, impl->did);
            unify(C, modno, loc, inst.type, self);
            if (pawIr_unify_traits(C, inst.trait, impl_trait) == 0) {
                if (result != NULL)
                    pawErr_generic_error(ENV(C), modname, loc, "multiple applicable traits");
                pawIr_solver_commit(child);
                result = inst.type;
                LOGLN("proved \"%s: %s\" using blanket impl block DefId(%d)",
                        pawIr_print_type(S->C, impl->type),
                        pawIr_print_trait(S->C, impl->trait),
                        impl->did.value);
            } else {
                pawU_undo_unifications(C->U, snapshot);
                pawIr_solver_rollback(child);
            }
            pawIr_pop_solver(C);
        }
    }

    return result != NULL;
}

static paw_Bool implements_trait(IrSolver *S, IrType *type, IrTrait *trait)
{
    IrSolver *cursor = S;
    while (cursor != NULL) {
        K_LIST_XFOREACH (cursor->preconditions, struct IrObligation const, p) {
            if (pawU_equals(cursor->U, type, p->type)
                    && pawIr_trait_equals(S->C, trait, p->trait)) {
//TODO                LOGLN("proved \"%s: %s\" using precondition from %s binder",
//TODO                        pawIr_print_type(S->C, type), pawIr_print_trait(S->C, trait),
//TODO                        cursor == S ? "current" : "outer");
                return PAW_TRUE;
            }
        }
        cursor = cursor->outer;
    }

    // search for evidence in the form of a compatible impl block
    return type_implements_trait(S, type, trait);
}

static void report_inference_var(struct IrTypeVisitor *V, struct IrInfer *t)
{
    PAW_UNUSED(t);
    *((paw_Bool *)V->ud) = PAW_TRUE;
}

static paw_Bool contains_inference_var(IrSolver *S, struct IrObligation o)
{
    paw_Bool found_inference_var = PAW_FALSE;

    struct IrTypeVisitor V;
    pawIr_type_visitor_init(&V, S->C, &found_inference_var);
    V.VisitInfer = report_inference_var;

    pawIr_visit_type(&V, pawU_normalize(S->U, o.type));
    pawIr_visit_trait(&V, pawIr_normalize_trait(S->C, o.trait));
    return found_inference_var;
}

int pawIr_solver_solve(IrSolver *S)
{
    paw_Bool solved_any;
    do {
        solved_any = PAW_FALSE;
        for (int i = 0; i < S->obligations->count; ++i) {
            struct IrObligation const o = IrObligations_get(S->obligations, i);
            IrType *type = pawU_normalize(S->U, o.type);
            if (!IrIsInfer(type) && implements_trait(S, type, o.trait)) {
                IrObligations_swap_remove(S->obligations, i--);
                solved_any = PAW_TRUE;
            } else if (!contains_inference_var(S, o)) {
                // solver was not blocked by the unifier, indicating an
                // unprovable obligation
                return -1;
            }
        }
    } while (solved_any);

    return S->obligations->count;
}

void pawIr_solver_rollback(IrSolver *S)
{
    S->obligations->count = 0;
}

void pawIr_solver_commit(IrSolver *S)
{
    if (S->outer != NULL) {
        K_LIST_XFOREACH (S->obligations, struct IrObligation const, p)
            IrObligations_push(S->C, S->outer->obligations, *p);
    }

    S->obligations->count = 0;
}

IrType *pawIr_solver_instantiate_type(IrSolver *S, DeclId did)
{
    IrTypeList *params = pawIr_get_generic_types(S->C, did);
    if (params == NULL) return pawIr_get_def_type(S->C, did);
    IrTypeList *args = pawU_new_unknowns(S->U, TODO, params);
    return pawIr_solver_instantiate_type_with(S, did, args);
}

IrType *pawIr_solver_instantiate_type_with(IrSolver *S, DeclId did, IrTypeList *args)
{
    IrTypeList *params = pawIr_get_generic_types(S->C, did);
    collect_obligations_from(S, params, args);
    struct Substitution const subst = {params, args};
    IrType *base = pawIr_get_def_type(S->C, did);
    return pawP_substitute(S->C, TODO, base, subst);
}

IrTrait *pawIr_solver_instantiate_trait(IrSolver *S, DeclId did)
{
    IrTrait *trait = pawIr_get_trait(S->C, did);
    IrTypeList *args = pawU_new_unknowns(S->U, TODO, trait->types);
    collect_obligations_from(S, trait->types, args);
    struct Substitution const subst = {trait->types, args};
    return substitute_trait(S->C, trait, subst);
}

IrTrait *pawIr_solver_instantiate_trait_with(IrSolver *S, DeclId did, IrTypeList *args)
{
    IrTrait *trait = pawIr_get_trait(S->C, did);
    collect_obligations_from(S, trait->types, args);
    struct Substitution const subst = {trait->types, args};
    return substitute_trait(S->C, trait, subst);
}

struct IrImplInstance pawIr_solver_instantiate_impl(IrSolver *S, DeclId did)
{
    IrTypeList *params = pawIr_get_generic_types(S->C, did);
    if (params == NULL) {
        struct IrImpl const *def = pawIr_get_impl_def(S->C, did);
        return (struct IrImplInstance){
                .type = def->type,
                .trait = def->trait,
            };
    }
    IrTypeList *args = pawU_new_unknowns(S->U, TODO, params);
    return pawIr_solver_instantiate_impl_with(S, did, args);
}

struct IrImplInstance pawIr_solver_instantiate_impl_with(IrSolver *S, DeclId did, IrTypeList *args)
{
    struct IrImpl const *impl = pawIr_get_impl_def(S->C, did);
    IrTypeList *params = pawIr_get_generic_types(S->C, did);
    collect_obligations_from(S, params, args);
    struct Substitution const subst = {params, args};
    return (struct IrImplInstance){
        .type = pawP_substitute(S->C, TODO, impl->type, subst),
        .trait = impl->trait == NULL ? NULL :
            substitute_trait(S->C, impl->trait, subst),
    };
}

void pawIr_solver_add_preconditions_from(IrSolver *S, DeclId did)
{
    IrTypeList *params = pawIr_get_generic_types(S->C, did);
    if (params != NULL) {
        K_LIST_XFOREACH (params, IrType *const, p) {
            IrTraitList *bounds = pawIr_get_trait_bounds(S->C, IR_TYPE_DID(*p));
            if (bounds != NULL) {
                K_LIST_XFOREACH (bounds, IrTrait *const, b)
                    pawIr_solver_add_precondition(S, *p, *b);
            }
        }
    }
}


