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
#include "type_folder.h"
#include "unify.h"

#define INSTANTIATION_ERROR(I_, Kind_, ...) pawErr_##Kind_((I_)->C, ModuleList_get((I_)->C->modules, (I_)->modno)->name, __VA_ARGS__)

struct InstanceState {
    struct Compiler *C;
    struct Unifier *U;
    paw_Env *P;
    int modno;
};

static struct IrTypeList *collect_generic_types(struct InstanceState *I, struct HirDeclList *generics)
{
    if (generics == NULL)
        return NULL;
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

static struct IrType *instantiate_func_aux(struct InstanceState *I, struct IrSignature *base, struct IrTypeList *types)
{
    struct IrType *inst = pawIr_new_signature(I->C, base->did, types, base->params, base->result);
    IrGetSignature(inst)->self = base->self;
    prep_func_instance(I, base->types, types, IrGetSignature(inst));
    return inst;
}

static void check_type_param(struct InstanceState *I, struct IrTypeList *params, struct IrTypeList *args)
{
    if (params->count != args->count)
        INSTANTIATION_ERROR(I, incorrect_type_arity, (struct SourceLoc){-1}, params->count, args->count);
}

static void normalize_type_list(struct InstanceState *I, struct IrTypeList *types)
{
    struct IrType *const *ptype;
    K_LIST_FOREACH (types, ptype)
        pawU_normalize(I->U->table, *ptype);
}

static struct IrType *instantiate_trait(struct InstanceState *I, struct IrTraitObj *base, struct IrTypeList *types)
{
    struct IrTypeList *generics = base->types;
    if (generics == NULL) {
        struct HirDecl *decl = pawHir_get_decl(I->C, base->did);
        INSTANTIATION_ERROR(I, unexpected_type_arguments, (struct SourceLoc){-1},
                "trait", hir_decl_ident(decl).name->text);
    }
    check_type_param(I, generics, types);
    normalize_type_list(I, types);
    return pawIr_new_trait_obj(I->C, base->did, types);
}

static struct IrType *instantiate_adt(struct InstanceState *I, struct IrAdt *base, struct IrTypeList *types)
{
    paw_assert(base->types != NULL);
    check_type_param(I, base->types, types);
    normalize_type_list(I, types);
    return pawIr_new_adt(I->C, base->did, types);
}

static struct IrType *instantiate_func(struct InstanceState *I, struct IrSignature *base, struct IrTypeList *types)
{
    paw_assert(base->types != NULL);
    check_type_param(I, base->types, types);
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
    prep_func_instance(I, generics, types, IrGetSignature(inst));
    return inst;
}

static struct IrType *instantiate_method(struct InstanceState *I, struct IrType *obj, struct HirDeclList *generics_, struct IrTypeList *types, struct HirDecl *method)
{
    paw_assert(types->count == IR_TYPE_SUBTYPES(obj)->count);
    struct IrTypeList *generics = collect_generic_types(I, generics_);
    struct IrTypeList *unknowns = pawU_new_unknowns(I->U, generics);

    struct IrTypeList *subst = pawP_instantiate_typelist(I->C, generics, unknowns, IR_TYPE_SUBTYPES(obj));

    struct IrType *const *pa, *const *pb;
    K_LIST_ZIP (subst, pa, types, pb) {
        // unification with an IrInfer never fails due to incompatible types
        int const rc = pawU_unify(I->U, *pa, *pb);
        paw_assert(rc == 0); PAW_UNUSED(rc);
    }

    struct IrType *inst = pawP_instantiate(I->C, obj, subst);
    struct IrType *r = instantiate_method_aux(I, generics, unknowns, inst, method);
    IrGetSignature(r)->self = inst;
    return r;
}

struct IrType *pawP_instantiate_method(struct Compiler *C, struct HirDecl *base, struct IrTypeList *types, struct HirDecl *method)
{
    struct InstanceState I = {
        .modno = base->hdr.did.modno,
        .U = C->U,
        .P = ENV(C),
        .C = C,
    };

    if (types == NULL)
        return GET_NODE_TYPE(C, base);
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

struct IrType *pawP_instantiate(struct Compiler *C, struct IrType *base, struct IrTypeList *types)
{
    if (types == NULL)
        return base;

    struct InstanceState I = {
        .modno = IR_TYPE_DID(base).modno,
        .U = C->U,
        .P = ENV(C),
        .C = C,
    };

    if (IrIsAdt(base)) {
        return instantiate_adt(&I, IrGetAdt(base), types);
    } else if (IrIsSignature(base)) {
        return instantiate_func(&I, IrGetSignature(base), types);
    }
    return instantiate_trait(&I, IrGetTraitObj(base), types);
}

static struct IrType *generalize_adt(struct Compiler *C, struct IrAdt *t)
{
    if (t->types == NULL)
        return IR_CAST_TYPE(t);

    struct InstanceState I = {
        .modno = t->did.modno,
        .U = C->U,
        .P = ENV(C),
        .C = C,
    };

    struct IrTypeList *unknowns = pawU_new_unknowns(I.U, t->types);
    return instantiate_adt(&I, t, unknowns);
}

static struct IrType *generalize_func(struct Compiler *C, struct IrSignature *t)
{
    if (t->types == NULL)
        return IR_CAST_TYPE(t);
    struct IrTypeList *unknowns = pawU_new_unknowns(C->U, t->types);
    return pawP_instantiate(C, IR_CAST_TYPE(t), unknowns);
}

// Replace generic parameters with inference variables (struct IrInfer). The
// resulting type can be unified with another type in order to fill in a type
// for each inference variable.
struct IrType *pawP_generalize(struct Compiler *C, struct IrType *type)
{
    return IrIsAdt(type) ? generalize_adt(C, IrGetAdt(type)) : IrIsSignature(type) //
        ? generalize_func(C, IrGetSignature(type)) : type;
}

static struct IrTypeList *generalize_list(struct Compiler *C, struct IrTypeList *types)
{
    struct IrTypeList *result = IrTypeList_new(C);
    IrTypeList_reserve(C, result, types->count);

    struct IrType **ptype;
    K_LIST_FOREACH (types, ptype) {
        struct IrType *r = pawP_generalize(C, *ptype);
        IrTypeList_push(C, result, r);
    }
    return result;
}

static struct IrTypeList *substitute_list(struct IrTypeFolder *F, struct IrTypeList *list)
{
    struct Substitution *subst = F->ud;
    struct Compiler *C = subst->C;
    if (list == NULL)
        return NULL;

    struct IrTypeList *copy = IrTypeList_new(C);
    IrTypeList_reserve(C, copy, list->count);

    struct IrType **ptype;
    K_LIST_FOREACH (list, ptype) {
        struct IrType *type = pawIr_fold_type(F, *ptype);
        IrTypeList_push(C, copy, type);
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
    return pawP_instantiate(F->C, IR_CAST_TYPE(t), types);
}

static struct IrType *substitute_adt(struct IrTypeFolder *F, struct IrAdt *t)
{
    if (t->types == NULL)
        return IR_CAST_TYPE(t);
    struct IrTypeList *types = pawIr_fold_type_list(F, t->types);
    return pawP_instantiate(F->C, IR_CAST_TYPE(t), types);
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
    K_LIST_ZIP (subst->generics, pg, subst->types, pt) {
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

static IrTypeList *instantiate_variant_fields(struct Compiler *C, struct IrVariantDef *def, IrTypeList *before, IrTypeList *after)
{
    struct IrFieldDef *const *pfield;
    IrTypeList *fields = IrTypeList_new(C);
    IrTypeList_reserve(C, fields, def->fields->count);
    K_LIST_FOREACH (def->fields, pfield) {
        IrType *field = pawIr_get_def_type(C, (*pfield)->did);
        IrTypeList_push(C, fields, field);
    }

    return pawP_instantiate_typelist(C, before, after, fields);
}

IrTypeList *pawP_instantiate_struct_fields(struct Compiler *C, struct IrAdt *inst)
{
    struct IrAdt *base = IrGetAdt(pawIr_get_def_type(C, inst->did));
    struct IrAdtDef *def = pawIr_get_adt_def(C, inst->did);
    struct IrVariantDef *variant = K_LIST_FIRST(def->variants);

    return instantiate_variant_fields(C, variant, base->types, inst->types);
}

IrTypeList *pawP_instantiate_variant_fields(struct Compiler *C, struct IrAdt *inst, int index)
{
    struct IrAdt *base = IrGetAdt(pawIr_get_def_type(C, inst->did));
    struct IrAdtDef *def = pawIr_get_adt_def(C, inst->did);
    struct IrVariantDef *variant = IrVariantDefs_get(def->variants, index);

    return instantiate_variant_fields(C, variant, base->types, inst->types);
}
