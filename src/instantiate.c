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

#define INSTANTIATION_ERROR(I_, Kind_, ...) pawErr_##Kind_((I_)->C, ModuleInfo_get((I_)->C->modinfo, (I_)->modno).name, __VA_ARGS__)

#define GET_SUBTYPES(Type_) (IrIsAdt(Type_) ? IrGetAdt(Type_)->types : \
        IrIsSignature(Type_) ? IrGetSignature(Type_)->types : \
        IrIsTraitObj(Type_) ? IrGetTraitObj(Type_)->types : NULL)

struct InstanceState {
    struct Compiler *C;
    struct Unifier *U;
    paw_Env *P;
    int modno;
};

static IrTypeList *collect_generic_types(struct InstanceState *I, struct HirDeclList *generics)
{
    if (generics == NULL) return NULL;
    return pawHir_collect_decl_types(I->C, generics);
}

static IrTypeList *collect_field_types(struct InstanceState *I, struct HirDeclList *fields)
{
    return pawHir_collect_decl_types(I->C, fields);
}

static IrType *fn_result(struct InstanceState *I, struct HirFnDecl *d)
{
    IrType const *type = pawIr_get_type(I->C, d->id);
    return IR_FPTR(type)->result;
}

IrTypeList *pawP_instantiate_typelist(struct Compiler *C, IrTypeList *before,
                                             IrTypeList *after, IrTypeList *target)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, C, &subst, before, after);
    return pawIr_fold_type_list(&F, target);
}

static void prep_fn_instance(struct InstanceState *I, IrTypeList *before, IrTypeList *after,
                               struct IrSignature *t)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, I->C, &subst, before, after);

    t->params = pawIr_fold_type_list(&F, t->params);
    t->result = pawIr_fold_type(&F, t->result);
}

static IrType *instantiate_fn_aux(struct InstanceState *I, struct IrSignature *base, IrTypeList *types)
{
    IrType *inst = pawIr_new_signature(I->C, base->did, types, base->params, base->result);
    IrGetSignature(inst)->self = base->self;
    prep_fn_instance(I, base->types, types, IrGetSignature(inst));
    return inst;
}

static void check_type_param(struct InstanceState *I, IrTypeList *params, IrTypeList *args)
{
    if (params->count != args->count)
        INSTANTIATION_ERROR(I, incorrect_type_arity, (struct SourceLoc){0}, params->count, args->count);
}

static void normalize_type_list(struct InstanceState *I, IrTypeList *types)
{
    IrType *const *ptype;
    K_LIST_FOREACH (types, ptype)
        pawU_normalize(I->U->table, *ptype);
}

static IrType *instantiate_trait(struct InstanceState *I, struct IrTraitObj *base, IrTypeList *types)
{
    IrTypeList *generics = base->types;
    if (generics == NULL) {
        struct HirDecl *decl = pawHir_get_decl(I->C->hir, base->did);
        INSTANTIATION_ERROR(I, unexpected_type_arguments, (struct SourceLoc){0},
                "trait", HirGetTraitDecl(decl)->ident.name->text);
    }
    check_type_param(I, generics, types);
    normalize_type_list(I, types);
    return pawIr_new_trait_obj(I->C, base->did, types);
}

static IrType *instantiate_adt(struct InstanceState *I, struct IrAdt *base, IrTypeList *types)
{
    paw_assert(base->types != NULL);
    check_type_param(I, base->types, types);
    normalize_type_list(I, types);
    return pawIr_new_adt(I->C, base->did, types);
}

static IrType *instantiate_fn(struct InstanceState *I, struct IrSignature *base, IrTypeList *types)
{
    paw_assert(base->types != NULL);
    check_type_param(I, base->types, types);
    normalize_type_list(I, types);
    return instantiate_fn_aux(I, base, types);
}

static IrType *instantiate_method_aux(struct InstanceState *I, IrTypeList *generics, IrTypeList *types, IrType *method)
{
    struct IrSignature *fn = IrGetSignature(method);
    IrType *inst = pawIr_new_signature(I->C, fn->did, fn->types, fn->params, fn->result);
    prep_fn_instance(I, generics, types, IrGetSignature(inst));
    return inst;
}

static IrType *instantiate_method(struct InstanceState *I, IrType *obj, IrTypeList *types, IrType *method)
{
    paw_assert(types != NULL);
    IrTypeList *generics = IR_TYPE_SUBTYPES(obj);
    paw_assert(types->count == generics->count);
    IrTypeList *unknowns = pawU_new_unknowns(I->U, (struct SourceLoc){0}, generics);

    IrTypeList *subst = pawP_instantiate_typelist(I->C, generics, unknowns, IR_TYPE_SUBTYPES(obj));

    IrType *const *pa, *const *pb;
    K_LIST_ZIP (subst, pa, types, pb) {
        // unification with an IrInfer never fails due to incompatible types
        int const rc = pawU_unify(I->U, *pa, *pb);
        paw_assert(rc == 0); PAW_UNUSED(rc);
    }

    IrType *inst = pawP_instantiate(I->C, obj, subst);
    IrType *r = instantiate_method_aux(I, generics, unknowns, method);
    IrGetSignature(r)->self = inst;
    return r;
}

IrType *pawP_instantiate_method(struct Compiler *C, IrType *base, IrTypeList *types, IrType *method)
{
    struct InstanceState I = {
        .modno = (int)IR_TYPE_DID(base).modno,
        .P = ENV(C),
        .U = C->U,
        .C = C,
    };

    return instantiate_method(&I, base, types, method);
}

IrType *pawP_instantiate(struct Compiler *C, IrType *base, IrTypeList *types)
{
    paw_assert(types != NULL);
    struct InstanceState I = {
        .modno = (int)IR_TYPE_DID(base).modno,
        .P = ENV(C),
        .U = C->U,
        .C = C,
    };

    if (IrIsAdt(base)) {
        return instantiate_adt(&I, IrGetAdt(base), types);
    } else if (IrIsSignature(base)) {
        return instantiate_fn(&I, IrGetSignature(base), types);
    }
    return instantiate_trait(&I, IrGetTraitObj(base), types);
}

// Replace generic parameters with inference variables (struct IrInfer). The
// resulting type can be unified with another type in order to fill in a type
// for each inference variable.
IrType *pawP_generalize(struct Compiler *C, struct SourceLoc loc, IrType *type)
{
    IrTypeList *generics = GET_SUBTYPES(type);
    if (generics == NULL) return type;

    IrTypeList *unknowns = pawU_new_unknowns(C->U, loc, generics);
    return pawP_instantiate(C, type, unknowns);
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
    IrTypeList *types = t->types == NULL ? NULL : pawIr_fold_type_list(F, t->types);
    IrType *self = t->self == NULL ? NULL : pawIr_fold_type(F, t->self);
    IrTypeList *params = pawIr_fold_type_list(F, t->params);
    IrType *result = pawIr_fold_type(F, t->result);
    IrType *signature = pawIr_new_signature(F->C, t->did, types, params, result);
    IrGetSignature(signature)->self = self;
    return signature;
//    if (t->types == NULL) {
//        IrTypeList *params = pawIr_fold_type_list(F, t->params);
//        IrType *result = pawIr_fold_type(F, t->result);
//        return pawIr_new_signature(F->C, t->did, NULL, params, result);
//    }
//    IrTypeList *types = pawIr_fold_type_list(F, t->types);
//    return pawP_instantiate(F->C, IR_CAST_TYPE(t), types);
}

static IrType *substitute_adt(struct IrTypeFolder *F, struct IrAdt *t)
{
    if (t->types == NULL) return IR_CAST_TYPE(t);
    IrTypeList *types = pawIr_fold_type_list(F, t->types);
    return pawP_instantiate(F->C, IR_CAST_TYPE(t), types);
}

static IrType *substitute_tuple(struct IrTypeFolder *F, struct IrTuple *t)
{
    IrTypeList *elems = pawIr_fold_type_list(F, t->elems);
    return pawIr_new_tuple(F->C, elems);
}

static IrType *substitute_generic(struct IrTypeFolder *F, struct IrGeneric *t)
{
    struct Substitution *subst = F->ud;

    IrType *const *pg, *const *pt;
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
                                   IrTypeList *generics, IrTypeList *types)
{
    *subst = (struct Substitution){
        .generics = generics,
        .types = types,
    };
    pawIr_type_folder_init(F, C, subst);
    F->FoldAdt = substitute_adt;
    F->FoldFnPtr = substitute_fn_ptr;
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
    paw_assert(index >= 0); // expects valid discriminant number
    struct IrAdt *base = IrGetAdt(pawIr_get_def_type(C, inst->did));
    struct IrAdtDef *def = pawIr_get_adt_def(C, inst->did);
    struct IrVariantDef *variant = IrVariantDefs_get(def->variants, index);

    return instantiate_variant_fields(C, variant, base->types, inst->types);
}

#define TODO (struct SourceLoc){0}

struct Instantiation pawP_instantiate_v2(struct Compiler *C, struct SourceLoc loc, IrType *type)
{
    IrTypeList *before = IR_TYPE_SUBTYPES(type);
    IrTypeList *after = pawU_new_unknowns(C->U, loc, before);
    struct Substitution const subst = {before, after};
    IrType *instance = pawP_substitute(C, loc, type, subst);
    return (struct Instantiation){
        .inst = instance,
        .subst = subst,
    };
}

IrType *pawP_substitute(struct Compiler *C, struct SourceLoc loc, IrType *type, struct Substitution subst)
{
    struct IrTypeFolder F;
    pawIr_type_folder_init(&F, C, &subst);
    F.FoldAdt = substitute_adt;
    F.FoldFnPtr = substitute_fn_ptr;
    F.FoldSignature = substitute_signature;
    F.FoldGeneric = substitute_generic;
    F.FoldTuple = substitute_tuple;
    F.FoldTypeList = substitute_list;
    return pawIr_fold_type(&F, type);
}

struct Instantiation pawP_instantiate_assoc(struct Compiler *C, struct SourceLoc loc, IrType *type, IrType *method)
{
    IrType *base = IrGetSignature(method)->self;

    struct Substitution subst;
    if (IrIsGeneric(base)) {
        subst.generics = IrTypeList_new(C);
        subst.types = IrTypeList_new(C);
        IrTypeList_push(C, subst.generics, base);
        IrTypeList_push(C, subst.types, type);
    } else {
        subst.generics = IR_TYPE_SUBTYPES(base);
        subst.types = IR_TYPE_SUBTYPES(type);
    }

    if (subst.generics != NULL) {
        paw_assert(subst.types != NULL);
        method = pawP_substitute(C, loc, method, subst);
    }

    IrGetSignature(method)->self = type;
    return (struct Instantiation){
        .inst = method,
        .subst = subst,
    };
}
