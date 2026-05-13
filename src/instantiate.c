// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// instantiate.c: Code for instantiating polymorphic functions and ADTs.
//     Polymorphic items must be instantiated each time they are referenced
//     outside of their original definitions. There are 2 cases that need to
//     be handled: explicit instantiation and type inference. Explicit
//     instantiation occurs when type arguments are provided on the item.
//     This is the simpler case: each generic type in the item's type
//     signature is replaced with the corresponding type argument. If no
//     type arguments are provided, then the type of each generic must be
//     inferred from subsequent uses of the item (calling a function/method
//     or accessing a struct field). To accomplish this, each generic in the
//     is replaced with a unique type variable. The type variables are filled
//     in as the instance type is unified with other types (see unify.c).

#include "compile.h"
#include "error.h"
#include "ir_type.h"
#include "map.h"
#include "solve.h"
#include "type_folder.h"
#include "unify.h"

#define INSTANTIATION_ERROR(I_, Kind_, ...) THROW_ERROR((I_)->C, \
        Kind_, .modname = ModuleInfo_get((I_)->C->modinfo, (I_)->modno).name, \
        __VA_ARGS__)

#define TODO (struct SourceLoc){0}

struct InstanceState {
    struct Compiler *C;
    struct Unifier *U;
    paw_Env *P;
    int modno;
};

IrGenericArgs *pawP_instantiate_typelist(struct Compiler *C, IrGenericArgs *before,
                                             IrGenericArgs *after, IrGenericArgs *target)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, C, &subst, before, after);
    return pawIr_fold_generic_args(&F, target);
}

static void prep_fn_instance(struct InstanceState *I, IrGenericArgs *before, IrGenericArgs *after,
                               struct IrSignature *t)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, I->C, &subst, before, after);
    t->args = pawIr_fold_generic_args(&F, t->args);
}

static IrType *instantiate_fn_aux(struct InstanceState *I, struct IrSignature *base, IrGenericArgs *args)
{
    struct Substitution const subst = {base->args, args};
    return pawP_substitute(I->C, IR_CAST_TYPE(base), subst);
}

static void check_type_param(struct InstanceState *I, IrGenericArgs *params, IrGenericArgs *args)
{
    if (params->count != args->count)
        INSTANTIATION_ERROR(I, IncorrectTypeArity,
                .want = params->count,
                .have = args->count,
                .span = {0});
}

static void normalize_type_list(struct InstanceState *I, IrGenericArgs *types)
{
    IrGenericArg const *p;
    K_LIST_FOREACH (types, p)
        pawIr_normalize(I->C, *p);
}

static IrTrait *instantiate_trait(struct InstanceState *I, struct IrTrait *base, IrGenericArgs *types)
{
    IrGenericArgs *generics = base->args;
    if (generics == NULL) {
        struct HirDecl *decl = pawHir_get_decl(I->C->hir, base->did);
        INSTANTIATION_ERROR(I, UnexpectedTypeArguments,
                .what = SCAN_STR(I->C, "trait"),
                .name = HirGetTraitDecl(decl)->ident.name,
                .span = {0});
    }
    check_type_param(I, generics, types);
    normalize_type_list(I, types);
    return pawIr_new_trait(I->C, base->did, types);
}

static IrType *instantiate_adt(struct InstanceState *I, struct IrAdt *base, IrGenericArgs *types)
{
    paw_assert(base->args != NULL);
    check_type_param(I, base->args, types);
    normalize_type_list(I, types);
    return pawIr_new_adt(I->C, base->did, types);
}

static IrType *instantiate_fn(struct InstanceState *I, struct IrSignature *base, IrGenericArgs *types)
{
    paw_assert(base->args != NULL);
    check_type_param(I, base->args, types);
    normalize_type_list(I, types);
    return instantiate_fn_aux(I, base, types);
}

static IrTypeList *substitute_list(struct IrTypeFolder *F, IrTypeList *list)
{
    struct Compiler *C = F->C;
    if (list == NULL) return NULL;

    IrTypeList *copy = IrTypeList_new(C);
    IrTypeList_reserve(C, copy, list->count);

    IrType *const *ptype;
    K_LIST_FOREACH (list, ptype) {
        IrType *type = pawIr_fold_type(F, *ptype);
        IrTypeList_push(C, copy, type);
    }
    return copy;
}

static IrType *substitute_fn_ptr(struct IrTypeFolder *F, struct IrFnPtr *t)
{
    IrTypeList *params = pawIr_fold_type_list(F, t->params);
    IrType *result = pawIr_fold_type(F, t->result);
    return pawIr_new_fn_ptr(F->C, params, result);
}

static IrType *substitute_signature(struct IrTypeFolder *F, struct IrSignature *t)
{
    // TODO: cannot be NULL
    IrGenericArgs *args = t->args == NULL ? NULL : pawIr_fold_generic_args(F, t->args);
    return pawIr_new_signature(F->C, t->did, args);
}

static IrType *substitute_adt(struct IrTypeFolder *F, struct IrAdt *t)
{
    if (t->args == NULL) return IR_CAST_TYPE(t);
    IrGenericArgs *args = pawIr_fold_generic_args(F, t->args);
    return pawIr_solver_instantiate_type_with(F->C->S, t->did, args);
}

static IrType *substitute_slice(struct IrTypeFolder *F, struct IrSlice *t)
{
    IrType *elem = pawIr_fold_type(F, t->type);
    return pawIr_new_slice(F->C, elem);
}

static IrType *substitute_tuple(struct IrTypeFolder *F, struct IrTuple *t)
{
    IrTypeList *elems = pawIr_fold_type_list(F, t->elems);
    return pawIr_new_tuple(F->C, elems);
}

static IrTrait *substitute_trait(struct IrTypeFolder *F, struct IrTrait *t)
{
    if (t->args == NULL) return t;
    IrGenericArgs *args = pawIr_fold_generic_args(F, t->args);
    return pawIr_new_trait(F->C, t->did, args);
}

static IrType *substitute_generic(struct IrTypeFolder *F, struct IrGeneric *t)
{
    struct Substitution *subst = F->ud;

    IrGenericArg const *pg, *pt;
    K_LIST_ZIP (subst->params, pg, subst->args, pt) {
        if (IrGenericArg_is_type(*pg)) {
            IrType *type = IrGenericArg_get_type(*pg);
            struct IrGeneric *g = IrGetGeneric(type);
            if (t->did.value == g->did.value)
                return IrGenericArg_get_type(*pt);
        }
    }
    return IR_CAST_TYPE(t);
}

void pawP_init_substitution_folder(struct IrTypeFolder *F, struct Compiler *C, struct Substitution *subst,
                                   IrGenericArgs *generics, IrGenericArgs *types)
{
    *subst = (struct Substitution){
        .params = generics,
        .args = types,
    };
    pawIr_type_folder_init(F, C, subst);
    F->FoldGeneric = substitute_generic;
}

static IrTypeList *instantiate_variant_fields(struct Compiler *C, struct IrVariantDef *def, IrGenericArgs *before, IrGenericArgs *after)
{
    struct IrFieldDef *const *pfield;
    IrTypeList *fields = IrTypeList_new(C);
    IrTypeList_reserve(C, fields, def->fields->count);
    K_LIST_FOREACH (def->fields, pfield) {
        IrType *field = pawIr_get_def_type(C, (*pfield)->did);
        IrTypeList_push(C, fields, field);
    }

    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, C, &subst, before, after);
    return pawIr_fold_type_list(&F, fields);
}

IrTypeList *pawP_instantiate_struct_fields(struct Compiler *C, struct IrAdt *inst)
{
    struct IrAdt *base = IrGetAdt(pawIr_get_def_type(C, inst->did));
    struct IrAdtDef *def = pawIr_get_adt_def(C, inst->did);
    struct IrVariantDef *variant = K_LIST_FIRST(def->variants);

    return instantiate_variant_fields(C, variant, base->args, inst->args);
}

IrTypeList *pawP_instantiate_variant_fields(struct Compiler *C, struct IrAdt *inst, int index)
{
    paw_assert(index >= 0); // expects valid discriminant number
    struct IrAdt *base = IrGetAdt(pawIr_get_def_type(C, inst->did));
    struct IrAdtDef *def = pawIr_get_adt_def(C, inst->did);
    struct IrVariantDef *variant = IrVariantDefs_get(def->variants, index);

    return instantiate_variant_fields(C, variant, base->args, inst->args);
}

struct Instantiation pawP_instantiate_v2(struct Compiler *C, IrType *type)
{
    IrGenericArgs *before = IR_GENERIC_ARGS(type);
    if (before == NULL) return (struct Instantiation){.inst = type};
    IrGenericArgs *after = pawIr_instantiate_args(C, IR_TYPE_DID(type));
    struct Substitution const subst = {before, after};
    IrType *instance = pawP_substitute(C, type, subst);
    return (struct Instantiation){
        .inst = instance,
        .subst = subst,
    };
}

IrTrait *pawP_substitute_trait(struct Compiler *C, IrTrait *trait, struct Substitution subst)
{
    IrGenericArgs *types = NULL;
    if (trait->args != NULL) {
        types = IrGenericArgs_new(C);
        IrGenericArgs_reserve(C, types, trait->args->count);
        K_LIST_XFOREACH (trait->args, IrGenericArg const, p)
            IrGenericArgs_push(C, types, pawP_substitute_arg(C, *p, subst));
    }

    return pawIr_new_trait(C, trait->did, types);
}

IrType *pawP_substitute(struct Compiler *C, IrType *type, struct Substitution subst)
{
    struct IrTypeFolder F;
    pawIr_type_folder_init(&F, C, &subst);
    F.FoldGeneric = substitute_generic;
    return pawIr_fold_type(&F, type);
}

IrGenericArg pawP_substitute_arg(struct Compiler *C, IrGenericArg arg, struct Substitution subst)
{
    if (IrGenericArg_is_type(arg)) {
        IrType *t = IrGenericArg_get_type(arg);
        return IrGenericArg_from_type(
                pawP_substitute(C, t, subst));
    } else {
        IrConst *k = IrGenericArg_get_const(arg);
        return IrGenericArg_from_const(
                pawP_substitute_const(C, k, subst));
    }
}

IrConst *pawP_substitute_const(struct Compiler *C, IrConst *k, struct Substitution subst)
{
    switch (k->kind) {
        case IR_CONST_DECL:
            return pawIr_new_const_decl(C, k->decl.did);
        case IR_CONST_PENDING:
            return pawIr_new_const_pending(C, k->pending.did);
        case IR_CONST_VALUE:
            return pawIr_new_const_value(C, k->value.value,
                    pawP_substitute(C, k->value.type, subst));
        case IR_CONST_INFER:
            return pawIr_new_const_infer(C);
    }
}

struct Instantiation pawP_instantiate_assoc(struct Compiler *C, IrType *type, IrType *method)
{
    IrType *base = pawIr_get_context(C, method);

    struct Substitution subst;
    if (IrIsGeneric(base)) {
        subst.params = IrGenericArgs_new(C);
        subst.args = IrGenericArgs_new(C);
        IrGenericArgs_push(C, subst.params, IrGenericArg_from_type(base));
        IrGenericArgs_push(C, subst.args, IrGenericArg_from_type(type));
    } else {
        subst.params = IR_GENERIC_ARGS(base);
        subst.args = IR_GENERIC_ARGS(type);
    }

    if (subst.params != NULL) {
        paw_assert(subst.args != NULL);
        method = pawP_substitute(C, method, subst);
    }

    return (struct Instantiation){
        .inst = method,
        .subst = subst,
    };
}
