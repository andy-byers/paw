// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// instantiate.c: Code for instantiating polymorphic functions, ADTs, and
//     impl blocks. HirInstanceDecl nodes are used to store the type of each
//     instance for type checking.

#include "compile.h"
#include "ir_type.h"
#include "map.h"
#include "type_folder.h"

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

static struct IrTypeList *new_unknowns(struct InstanceState *I, int count)
{
    struct IrTypeList *list = pawIr_type_list_new(I->C);
    for (int i = 0; i < count; ++i) {
        struct IrType *unknown = pawU_new_unknown(I->U, I->line);
        K_LIST_PUSH(I->C, list, unknown);
    }
    return list;
}

static struct IrTypeList *instantiate_typelist(struct InstanceState *I, struct IrTypeList *before,
                                               struct IrTypeList *after, struct IrTypeList *target)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, I->C, &subst, before, after);
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
    struct IrType *result = pawIr_new_type(I->C, kIrSignature);
    struct IrSignature *r = IrGetSignature(result);
    r->params = collect_field_types(I, base->params);
    r->result = func_result(I, base);
    r->did = base->did;
    r->types = types;

    struct IrType *base_fdef = pawIr_get_type(I->C, base->hid);
    prep_func_instance(I, IR_TYPE_SUBTYPES(base_fdef), types, r);
    return result;
}

static void instantiate_adt_aux(struct InstanceState *I, struct HirAdtDecl *base,
                                struct IrAdt *inst, struct IrTypeList *types)
{
    inst->did = base->did;
    inst->types = types;
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

static struct IrType *instantiate_adt(struct InstanceState *I, struct HirAdtDecl *base, struct IrTypeList *types)
{
    struct IrType *base_type = pawIr_get_type(I->C, base->hid);
    check_type_param(I, ir_adt_types(base_type), types);
    normalize_type_list(I, types);

    struct IrType *inst = pawIr_new_type(I->C, kIrAdt);
    instantiate_adt_aux(I, base, IrGetAdt(inst), types);
    return inst;
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
    struct IrType *result = pawIr_new_type(I->C, kIrSignature);
    struct IrSignature *r = IrGetSignature(result);
    r->did = method->hdr.did;
    r->types = collect_generic_types(I, func->generics);
    r->params = collect_field_types(I, func->params);
    r->result = func_result(I, func);
    prep_func_instance(I, generics, types, r);

    pawH_insert(ENV(I), I->C->method_binders, I2V(method->hdr.did.value), P2V(generics));
    return result;
}

static struct IrType *instantiate_method(struct InstanceState *I, struct HirImplDecl *impl, struct IrTypeList *types, struct HirDecl *method)
{
    struct IrType *adt = pawIr_get_type(I->C, impl->hid);
    paw_assert(types->count == ir_adt_types(adt)->count);
    struct IrTypeList *generics = collect_generic_types(I, impl->generics);
    struct IrTypeList *unknowns = new_unknowns(I, generics->count);

    // Substitute the polymorphic impl block's generics for inference variables (unknowns)
    // in the context of its 'self' ADT. For example:
    //     impl<X, Y> A<int, Y, X> => impl<?0, ?1> A<int, ?1, ?0>
    // where unknowns = [?0, ?1] and subst = [int, ?1, ?0]. Unifying with the given ADTs
    // type arguments yields a concrete type for each of the impl block's generics.
    struct IrTypeList *subst = instantiate_typelist(I, generics, unknowns, ir_adt_types(adt));
    for (int i = 0; i < subst->count; ++i) {
        pawU_unify(I->U, K_LIST_GET(subst, i), K_LIST_GET(types, i));
    }

    struct HirDecl *base = pawHir_get_decl(I->C, IR_TYPE_DID(adt));
    struct IrType *inst = pawP_instantiate(I->C, base, subst);
    struct IrType *r = instantiate_method_aux(I, generics, unknowns, inst, method);
    pawP_set_self(I->C, IrGetSignature(r), inst);
    return r;
}

struct IrType *pawP_instantiate_method(struct Compiler *C, struct HirDecl *base, struct IrTypeList *types, struct HirDecl *method)
{
    struct InstanceState I = {
        .U = &C->dm->unifier,
        .P = ENV(C),
        .C = C,
    };

    if (types == NULL) return GET_NODE_TYPE(C, base);
    return instantiate_method(&I, &base->impl, types, method);
}


struct IrType *pawP_instantiate(struct Compiler *C, struct HirDecl *base, struct IrTypeList *types)
{
    paw_assert(!HirIsImplDecl(base));
    if (types == NULL) goto instantiated;

    struct InstanceState I = {
        .U = &C->dm->unifier,
        .P = ENV(C),
        .C = C,
    };

    if (HIR_IS_POLY_ADT(base)) {
        return instantiate_adt(&I, &base->adt, types);
    } else if (HIR_IS_POLY_FUNC(base)) {
        return instantiate_func(&I, &base->func, types);
    }
instantiated:
    return GET_NODE_TYPE(C, base);
}

static struct IrType *generalize_adt(struct Compiler *C, struct IrAdt *t)
{
    if (t->types == NULL) return IR_CAST_TYPE(t);

    struct InstanceState I = {
        .U = &C->dm->unifier,
        .P = ENV(C),
        .C = C,
    };

    struct IrTypeList *unknowns = new_unknowns(&I, t->types->count);
    struct HirDecl *base = pawHir_get_decl(C, t->did);
    return pawP_instantiate(C, base, unknowns);
}

static struct IrType *generalize_func(struct Compiler *C, struct IrSignature *t)
{
    if (t->types == NULL) return IR_CAST_TYPE(t);

    struct InstanceState I = {
        .U = &C->dm->unifier,
        .P = ENV(C),
        .C = C,
    };

    struct IrTypeList *unknowns = new_unknowns(&I, t->types->count);
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

struct IrType *pawP_generalize_self(struct Compiler *C, struct IrType *self, struct IrTypeList *base_binder, struct IrTypeList **pinst_binder)
{
    struct InstanceState I = {
        .U = &C->dm->unifier,
        .P = ENV(C),
        .C = C,
    };

    struct IrTypeList *unknowns = new_unknowns(&I, base_binder->count);
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

    struct IrTypeList *copy = pawIr_type_list_new(C);
    for (int i = 0; i < list->count; ++i) {
        struct IrType *type = pawIr_fold_type(F, list->data[i]);
        K_LIST_PUSH(C, copy, type);
    }
    return copy;
}

static struct IrType *substitute_func_ptr(struct IrTypeFolder *F, struct IrFuncPtr *t)
{
    struct Substitution *subst = F->ud;
    struct Compiler *C = subst->C;

    struct IrType *r = pawIr_new_type(C, kIrFuncPtr);
    IrGetFuncPtr(r)->params = pawIr_fold_type_list(F, t->params);
    IrGetFuncPtr(r)->result = pawIr_fold_type(F, t->result);
    return r;
}

static struct IrType *substitute_signature(struct IrTypeFolder *F, struct IrSignature *t)
{
    struct Substitution *subst = F->ud;
    struct Compiler *C = subst->C;

    if (t->types == NULL) {
        struct IrType *result = pawIr_new_type(C, kIrSignature);
        struct IrSignature *r = IrGetSignature(result);
        r->params = pawIr_fold_type_list(F, t->params);
        r->result = pawIr_fold_type(F, t->result);
        r->did = t->did;
        return result;
    }
    struct IrTypeList *types = pawIr_fold_type_list(F, t->types);
    struct HirDecl *base = pawHir_get_decl(C, t->did);
    return pawP_instantiate(C, base, types);
}

static struct IrType *substitute_adt(struct IrTypeFolder *F, struct IrAdt *t)
{
    struct Substitution *subst = F->ud;
    struct Compiler *C = subst->C;

    if (t->types == NULL) return IR_CAST_TYPE(t);
    struct HirDecl *base = pawHir_get_decl(C, t->did);
    struct IrTypeList *types = pawIr_fold_type_list(F, t->types);
    return pawP_instantiate(C, base, types);
}

static struct IrType *substitute_tuple(struct IrTypeFolder *F, struct IrTuple *t)
{
    struct Substitution *subst = F->ud;
    struct Compiler *C = subst->C;

    struct IrType *result = pawIr_new_type(C, kIrTuple);
    struct IrTuple *r = IrGetTuple(result);
    r->elems = pawIr_fold_type_list(F, t->elems);
    return result;
}

static struct IrType *substitute_generic(struct IrTypeFolder *F, struct IrGeneric *t)
{
    struct Substitution *subst = F->ud;
    struct Compiler *C = subst->C;

    for (int i = 0; i < subst->generics->count; ++i) {
        struct IrGeneric *g = IrGetGeneric(K_LIST_GET(subst->generics, i));
        if (t->did.value == g->did.value) return K_LIST_GET(subst->types, i);
    }
    return IR_CAST_TYPE(t);
}

void pawP_init_substitution_folder(struct IrTypeFolder *F, struct Compiler *C, struct Substitution *subst,
                                   struct IrTypeList *generics, struct IrTypeList *types)
{
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

