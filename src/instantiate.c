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
#include "ir_type.h"
#include "map.h"
#include "type_folder.h"
#include "unify.h"

struct InstanceState {
    struct Compiler *C;
    struct Unifier *U;
    paw_Env *P;
    int line;
};

static struct IrTypeList *collect_generic_types(struct InstanceState *I, struct HirDeclList *generics)
{
    if (generics == NULL) return NULL;
    return pawHir_collect_decl_types(I->C, generics);
}

static struct IrTypeList *collect_field_types(struct InstanceState *I, struct HirDeclList *fields)
{
    return pawHir_collect_decl_types(I->C, fields);
}

static struct IrType *func_result(struct InstanceState *I, struct HirFuncDecl *d)
{
    return IR_FPTR(pawIr_get_type(I->C, d->hid))->result;
}

struct IrTypeList *pawP_instantiate_typelist(struct Compiler *C, struct IrTypeList *before,
                                             struct IrTypeList *after, struct IrTypeList *target)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, C, &subst, before, after);
    return pawIr_fold_type_list(&F, target);
}

static void prep_func_instance(struct InstanceState *I, struct IrTypeList *before, struct IrTypeList *after,
                               struct IrSignature *t)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, I->C, &subst, before, after);

    t->params = pawIr_fold_type_list(&F, t->params);
    t->result = pawIr_fold_type(&F, t->result);
}

static struct IrType *instantiate_func_aux(struct InstanceState *I, struct HirFuncDecl *base, struct IrTypeList *types)
{
    struct IrTypeList *params = collect_field_types(I, base->params);
    struct IrType *result = func_result(I, base);
    struct IrType *inst = pawIr_new_signature(I->C, base->did, types, params, result);
    struct IrType *base_type = pawIr_get_type(I->C, base->hid);
    prep_func_instance(I, IR_TYPE_SUBTYPES(base_type), types, IrGetSignature(inst));
    return inst;
}

static void check_type_param(struct InstanceState *I, struct IrTypeList *params, struct IrTypeList *args)
{
    if (args->count > params->count) {
        TYPE_ERROR(I, "too many generics");
    } else if (args->count < params->count) {
        TYPE_ERROR(I, "not enough generics");
    }
}

static void normalize_type_list(struct InstanceState *I, struct IrTypeList *types)
{
    for (int i = 0; i < types->count; ++i) {
        pawU_normalize(I->U->table, types->data[i]);
    }
}

static struct IrType *instantiate_trait(struct InstanceState *I, struct HirTraitDecl *base, struct IrTypeList *types)
{
    struct IrType *base_type = pawIr_get_type(I->C, base->hid);
    struct IrTypeList *generics = IR_TYPE_SUBTYPES(base_type);
    if (generics == NULL) TYPE_ERROR(I, "trait is not polymorphic");
    check_type_param(I, generics, types);
    normalize_type_list(I, types);
    return pawIr_new_trait_obj(I->C, base->did, types);
}

static struct IrType *instantiate_adt(struct InstanceState *I, struct HirAdtDecl *base, struct IrTypeList *types)
{
    struct IrType *base_type = pawIr_get_type(I->C, base->hid);
    check_type_param(I, ir_adt_types(base_type), types);
    normalize_type_list(I, types);
    return pawIr_new_adt(I->C, base->did, types);
}

static struct IrType *instantiate_func(struct InstanceState *I, struct HirFuncDecl *base, struct IrTypeList *types)
{
    struct IrSignature *func = IrGetSignature(
            pawIr_get_type(I->C, base->hid));
    check_type_param(I, func->types, types);
    normalize_type_list(I, types);
    return instantiate_func_aux(I, base, types);
}

static struct IrType *instantiate_method_aux(struct InstanceState *I, struct IrTypeList *generics, struct IrTypeList *types, struct IrType *self, struct HirDecl *method)
{
    struct HirFuncDecl *func = HirGetFuncDecl(method);
    struct IrTypeList *func_types = collect_generic_types(I, func->generics);
    struct IrTypeList *params = collect_field_types(I, func->params);
    struct IrType *result = func_result(I, func);
    struct IrType *inst = pawIr_new_signature(I->C, method->hdr.did, func_types, params, result);
    struct IrSignature *r = IrGetSignature(inst);
    prep_func_instance(I, generics, types, r);

    pawP_set_binder(I->C, method->hdr.did, generics);
    return inst;
}

static struct IrType *instantiate_method(struct InstanceState *I, struct IrType *obj, struct HirDeclList *generics_, struct IrTypeList *types, struct HirDecl *method)
{
    paw_assert(types->count == IR_TYPE_SUBTYPES(obj)->count);
    struct IrTypeList *generics = collect_generic_types(I, generics_);
    struct IrTypeList *unknowns = pawU_new_unknowns(I->U, generics);

    // Substitute the polymorphic impl block's generics for inference variables (unknowns)
    // in the context of its 'self' ADT. For example:
    //     impl<X, Y> A<int, Y, X> => impl<?0, ?1> A<int, ?1, ?0>
    // where unknowns = [?0, ?1] and subst = [int, ?1, ?0]. Unifying with the given ADTs
    // type arguments yields a concrete type for each of the impl block's generics.
    struct IrTypeList *subst = pawP_instantiate_typelist(I->C, generics, unknowns, IR_TYPE_SUBTYPES(obj));
    for (int i = 0; i < subst->count; ++i) {
        pawU_unify(I->U, K_LIST_GET(subst, i), K_LIST_GET(types, i));
    }

    struct HirDecl *base = pawHir_get_decl(I->C, IR_TYPE_DID(obj));
    struct IrType *inst = pawP_instantiate(I->C, base, subst);
    struct IrType *r = instantiate_method_aux(I, generics, unknowns, inst, method);
    pawP_set_self(I->C, IrGetSignature(r), inst);
    return r;
}

struct IrType *pawP_instantiate_method(struct Compiler *C, struct HirDecl *base, struct IrTypeList *types, struct HirDecl *method)
{
    struct InstanceState I = {
        .U = C->U,
        .P = ENV(C),
        .C = C,
    };

    if (types == NULL) return GET_NODE_TYPE(C, base);
    if (HirIsAdtDecl(base)) {
        struct HirAdtDecl *adt = HirGetAdtDecl(base);
        struct IrType *type = pawIr_get_type(C, adt->hid);
        return instantiate_method(&I, type, adt->generics, types, method);
    } else {
        struct HirTraitDecl *trait = HirGetTraitDecl(base);
        struct IrType *type = pawIr_get_type(C, trait->hid);
        return instantiate_method(&I, type, trait->generics, types, method);
    }
}

struct IrType *pawP_instantiate(struct Compiler *C, struct HirDecl *base, struct IrTypeList *types)
{
    if (types == NULL) return GET_NODE_TYPE(C, base);

    struct InstanceState I = {
        .U = C->U,
        .P = ENV(C),
        .C = C,
    };

    if (HIR_IS_POLY_ADT(base)) {
        return instantiate_adt(&I, HirGetAdtDecl(base), types);
    } else if (HIR_IS_POLY_FUNC(base)) {
        return instantiate_func(&I, HirGetFuncDecl(base), types);
    }
    return instantiate_trait(&I, HirGetTraitDecl(base), types);
// TODO:     return GET_NODE_TYPE(C, base);
}

static struct IrType *generalize_adt(struct Compiler *C, struct IrAdt *t)
{
    if (t->types == NULL) return IR_CAST_TYPE(t);

    struct InstanceState I = {
        .U = C->U,
        .P = ENV(C),
        .C = C,
    };

    struct IrTypeList *unknowns = pawU_new_unknowns(I.U, t->types);
    struct HirDecl *base = pawHir_get_decl(C, t->did);
    return pawP_instantiate(C, base, unknowns);
}

static struct IrType *generalize_func(struct Compiler *C, struct IrSignature *t)
{
    if (t->types == NULL) return IR_CAST_TYPE(t);

    struct InstanceState I = {
        .U = C->U,
        .P = ENV(C),
        .C = C,
    };

    struct IrTypeList *unknowns = pawU_new_unknowns(I.U, t->types);
    struct HirDecl *base = pawHir_get_decl(C, t->did);
    return pawP_instantiate(C, base, unknowns);
}

// Replace generic parameters with inference variables (struct IrInfer). The
// resulting type can be unified with another type in order to fill in a type
// for each inference variable.
struct IrType *pawP_generalize(struct Compiler *C, struct IrType *type)
{
    return IrIsAdt(type) ? generalize_adt(C, IrGetAdt(type)) :
        IrIsSignature(type) ? generalize_func(C, IrGetSignature(type)) : type;
}

static struct IrTypeList *generalize_list(struct Compiler *C, struct IrTypeList *types)
{
    struct IrType **ptype;
    struct IrTypeList *result = pawIr_type_list_new(C);
    K_LIST_FOREACH(types, ptype) {
        struct IrType *r = pawP_generalize(C, *ptype);
        K_LIST_PUSH(C, result, r);
    }
    return result;
}

struct IrType *pawP_generalize_self(struct Compiler *C, struct IrType *self, struct IrTypeList *base_binder, struct IrTypeList **pinst_binder)
{
    struct InstanceState I = {
        .U = C->U,
        .P = ENV(C),
        .C = C,
    };

    struct IrTypeList *unknowns = pawU_new_unknowns(I.U, base_binder);
    struct HirDecl *base = pawHir_get_decl(C, IR_TYPE_DID(self));

    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, C, &subst, base_binder, unknowns);

    *pinst_binder = unknowns;
    return pawIr_fold_type(&F, self);
}

static struct IrTypeList *substitute_list(struct IrTypeFolder *F, struct IrTypeList *list)
{
    struct Substitution *subst = F->ud;
    struct Compiler *C = subst->C;
    if (list == NULL) return NULL;

    struct IrType **ptype;
    struct IrTypeList *copy = pawIr_type_list_new(C);
    K_LIST_FOREACH(list, ptype) {
        struct IrType *type = pawIr_fold_type(F, *ptype);
        K_LIST_PUSH(C, copy, type);
    }
    return copy;
}

static struct IrType *substitute_func_ptr(struct IrTypeFolder *F, struct IrFuncPtr *t)
{
    struct IrTypeList *params = pawIr_fold_type_list(F, t->params);
    struct IrType *result = pawIr_fold_type(F, t->result);
    return pawIr_new_func_ptr(F->C, params, result);
}

static struct IrType *substitute_signature(struct IrTypeFolder *F, struct IrSignature *t)
{
    if (t->types == NULL) {
        struct IrTypeList *params = pawIr_fold_type_list(F, t->params);
        struct IrType *result = pawIr_fold_type(F, t->result);
        return pawIr_new_signature(F->C, t->did, NULL, params, result);
    }
    struct IrTypeList *types = pawIr_fold_type_list(F, t->types);
    struct HirDecl *base = pawHir_get_decl(F->C, t->did);
    return pawP_instantiate(F->C, base, types);
}

static struct IrType *substitute_adt(struct IrTypeFolder *F, struct IrAdt *t)
{
    if (t->types == NULL) return IR_CAST_TYPE(t);
    struct HirDecl *base = pawHir_get_decl(F->C, t->did);
    struct IrTypeList *types = pawIr_fold_type_list(F, t->types);
    return pawP_instantiate(F->C, base, types);
}

static struct IrType *substitute_tuple(struct IrTypeFolder *F, struct IrTuple *t)
{
    struct IrTypeList *elems = pawIr_fold_type_list(F, t->elems);
    return pawIr_new_tuple(F->C, elems);
}

static struct IrType *substitute_generic(struct IrTypeFolder *F, struct IrGeneric *t)
{
    struct Substitution *subst = F->ud;

    struct IrType **pg, **pt;
    K_LIST_ZIP(subst->generics, pg, subst->types, pt) {
        struct IrGeneric *g = IrGetGeneric(*pg);
        if (t->did.value == g->did.value) {
            if (IrIsGeneric(*pt)) {
                struct IrGeneric *g = IrGetGeneric(*pt);
                g->bounds = pawIr_fold_type_list(F, g->bounds);
            } else if (IrIsInfer(*pt)) {
                struct IrInfer *i = IrGetInfer(*pt);
                i->bounds = pawIr_fold_type_list(F, i->bounds);
            }
            return *pt;
        }
    }
    return IR_CAST_TYPE(t);
}

void pawP_init_substitution_folder(struct IrTypeFolder *F, struct Compiler *C, struct Substitution *subst,
                                   struct IrTypeList *generics, struct IrTypeList *types)
{
//    // TODO: consider replacing generic with inference vars and unifying instead of doing this
//    //       generic bounds are checked in pawU_unify
//    //       likely could get rid of this function and subsequent call to pawIr_fold*()
//    struct IrType **pa, **pb;
//    K_LIST_ZIP(generics, pa, types, pb) {
//        struct IrTypeList *bounds = IrGetGeneric(*pa)->bounds;
//        if (!pawP_satisfies_bounds(C, *pb, bounds)) {
//            TYPE_ERROR(C, "trait bounds not satisfied");
//        }
//    }

    *subst = (struct Substitution){
        .generics = generics,
        .types = types,
        .C = C,
    };
    pawIr_type_folder_init(F, C, subst);
    F->FoldAdt = substitute_adt;
    F->FoldFuncPtr = substitute_func_ptr;
    F->FoldSignature = substitute_signature;
    F->FoldGeneric = substitute_generic;
    F->FoldTuple = substitute_tuple;
    F->FoldTypeList = substitute_list;
}

