// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// instantiate.c: Code for instantiating polymorphic functions, ADTs, and
//     impl blocks. HirInstanceDecl nodes are used to store the type of each
//     instance for type checking.

#include "hir.h"

struct InstanceState {
    struct Compiler *C;
    struct Unifier *U;
    struct Hir *hir;
    paw_Env *P;
    int line;
};

static struct HirDecl *get_decl(struct InstanceState *I, DefId did)
{
    struct DynamicMem *dm = I->C->dm;
    paw_assert(did < dm->decls->count);
    return dm->decls->data[did];
}

static struct HirType *new_type(struct InstanceState *I, DeclId did, enum HirTypeKind kind, int line)
{
    return pawHir_attach_type(I->C, did, kind, line);
}

static struct HirTypeList *collect_generic_types(struct InstanceState *I, struct HirDeclList *generics)
{
    if (generics == NULL) return NULL;
    return pawHir_collect_generics(I->C, generics);
}

static struct HirTypeList *collect_field_types(struct InstanceState *I, struct HirDeclList *fields)
{
    return pawHir_collect_fields(I->C, fields);
}

static struct HirType *register_decl_type(struct InstanceState *I, struct HirDecl *decl, enum HirTypeKind kind)
{
    const DeclId did = pawHir_add_decl(I->C, decl);
    return pawHir_attach_type(I->C, did, kind, decl->hdr.line);
}

static struct HirType *func_result(struct HirFuncDecl *d)
{
    return HIR_FPTR(d->type)->result;
}

static struct HirTypeList *new_unknowns(struct InstanceState *I, int count)
{
    struct HirTypeList *list = pawHir_type_list_new(I->C);
    for (int i = 0; i < count; ++i) {
        struct HirType *unknown = pawU_new_unknown(I->U, I->line);
        pawHir_type_list_push(I->C, list, unknown);
    }
    return list;
}

static paw_Bool test_lists(struct InstanceState *I, struct HirTypeList *lhs, struct HirTypeList *rhs)
{
    if (!lhs != !rhs) return PAW_FALSE;
    if (lhs->count != rhs->count) return PAW_FALSE;
    for (int i = 0; i < lhs->count; ++i) {
        struct HirType *a = K_LIST_GET(lhs, i);
        struct HirType *b = K_LIST_GET(rhs, i);
        if (!pawU_equals(I->U, a, b)) return PAW_FALSE;
    }
    return PAW_TRUE;
}

static struct HirDecl *find_func_instance(struct InstanceState *I, struct HirFuncDecl *base, struct HirTypeList *types)
{
    struct HirFuncDef *fdef = HirGetFuncDef(base->type);
    if (test_lists(I, types, fdef->types)) return HIR_CAST_DECL(base);
    for (int i = 0; i < base->monos->count; ++i) {
        struct HirDecl *inst = base->monos->data[i];
        const struct HirType *type = HIR_TYPEOF(inst);
        if (test_lists(I, types, type->fdef.types)) return inst;
    }
    return NULL;
}

#define TEST_SELF_ADT(I, base, types) test_lists(I, types, HirGetAdt((base)->type)->types)

static struct HirDecl *find_adt_instance(struct InstanceState *I, struct HirAdtDecl *base, struct HirTypeList *types)
{
    if (TEST_SELF_ADT(I, base, types)) return HIR_CAST_DECL(base);
    for (int i = 0; i < base->monos->count; ++i) {
        struct HirDecl *inst = base->monos->data[i];
        const struct HirAdt *adt = HirGetAdt(HIR_TYPEOF(inst));
        if (test_lists(I, types, adt->types)) return inst;
    }
    return NULL;
}

static struct HirDecl *find_impl_instance(struct InstanceState *I, struct HirImplDecl *base, struct HirTypeList *types)
{
    if (TEST_SELF_ADT(I, base, types)) return HIR_CAST_DECL(base);
    for (int i = 0; i < base->monos->count; ++i) {
        struct HirDecl *inst = base->monos->data[i];
        struct HirImplDecl *d = HirGetImplDecl(inst);
        if (test_lists(I, types, d->subst)) return inst;
    }
    return NULL;
}

struct Subst {
    struct HirTypeList *before;
    struct HirTypeList *after;
    struct InstanceState *I;
};

#define MAYBE_SUBST_LIST(F, list) ((list) != NULL ? subst_list(F, list) : NULL)

static struct HirTypeList *subst_list(struct HirFolder *F, struct HirTypeList *list)
{
    struct Subst *subst = F->ud;
    struct InstanceState *I = subst->I;
    struct HirTypeList *copy = pawHir_type_list_new(I->C);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = F->FoldType(F, list->data[i]);
        pawHir_type_list_push(I->C, copy, type);
    }
    return copy;
}

static struct HirType *subst_func_ptr(struct HirFolder *F, struct HirFuncPtr *t)
{
    struct Subst *subst = F->ud;
    struct InstanceState *I = subst->I;
    struct HirType *r = new_type(I, NO_DECL, kHirFuncPtr, t->line);
    r->fptr.params = subst_list(F, t->params);
    r->fptr.result = F->FoldType(F, t->result);
    return r;
}

static struct HirType *subst_func_def(struct HirFolder *F, struct HirFuncDef *t)
{
    struct Subst *subst = F->ud;
    struct InstanceState *I = subst->I;

    if (t->types == NULL) {
        struct HirType *result = new_type(I, NO_DECL, kHirFuncDef, t->line);
        struct HirFuncDef *r = HirGetFuncDef(result);
        r->params = subst_list(F, t->params);
        r->result = F->FoldType(F, t->result);
        r->modno = t->modno;
        r->base = t->did;
        return result;
    }
    struct HirTypeList *types = subst_list(F, t->types);
    struct HirDecl *base = get_decl(I, t->base);
    struct HirDecl *inst = pawP_instantiate(I->hir, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *subst_adt(struct HirFolder *F, struct HirAdt *t)
{
    struct Subst *subst = F->ud;
    struct InstanceState *I = subst->I;

    if (t->types == NULL) return HIR_CAST_TYPE(t);
    struct HirDecl *base = get_decl(I, t->base);
    struct HirTypeList *types = subst_list(F, t->types);
    struct HirDecl *inst = pawP_instantiate(I->hir, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *subst_tuple(struct HirFolder *F, struct HirTupleType *t)
{
    struct Subst *subst = F->ud;
    struct InstanceState *I = subst->I;

    struct HirType *result = new_type(I, NO_DECL, kHirTupleType, t->line);
    struct HirTupleType *r = HirGetTupleType(result);
    r->elems = subst_list(F, t->elems);
    return result;
}

static paw_Bool type_equals(struct InstanceState *I, struct HirType *a, struct HirType *b)
{
    return pawU_equals(I->U, a, b);
}

static struct HirType *maybe_subst(struct HirFolder *F, struct HirType *t)
{
    struct Subst *s = F->ud;
    struct InstanceState *I = s->I;
    for (int i = 0; i < s->before->count; ++i) {
        struct HirType *t2 = K_LIST_GET(s->before, i);
        if (type_equals(I, t, t2)) return K_LIST_GET(s->after, i);
    }
    return t;
}

static struct HirType *subst_generic(struct HirFolder *F, struct HirGeneric *t)
{
    return maybe_subst(F, HIR_CAST_TYPE(t));
}

static struct HirType *subst_unknown(struct HirFolder *F, struct HirUnknown *t)
{
    return maybe_subst(F, HIR_CAST_TYPE(t));
}

static void init_subst_folder(struct HirFolder *F, struct Subst *subst, struct InstanceState *I,
                              struct HirTypeList *before, struct HirTypeList *after)
{
    *subst = (struct Subst){
        .before = before,
        .after = after,
        .I = I,
    };
    pawHir_folder_init(F, I->hir, subst);
    F->FoldAdt = subst_adt;
    F->FoldFuncPtr = subst_func_ptr;
    F->FoldFuncDef = subst_func_def;
    F->FoldGeneric = subst_generic;
    F->FoldUnknown = subst_unknown;
    F->FoldTupleType = subst_tuple;
    F->FoldTypeList = subst_list;
}

static struct HirTypeList *instantiate_typelist(struct InstanceState *I, struct HirTypeList *before,
                                                struct HirTypeList *after, struct HirTypeList *target)
{
    struct Subst subst;
    struct HirFolder F;
    init_subst_folder(&F, &subst, I, before, after);
    return F.FoldTypeList(&F, target);
}

static void prep_func_instance(struct InstanceState *I, struct HirTypeList *before, struct HirTypeList *after,
                               struct HirInstanceDecl *d, struct HirFuncDef *t)
{
    struct Subst subst;
    struct HirFolder F;
    init_subst_folder(&F, &subst, I, before, after);

    struct HirTypeList *params = t->params;
    for (int i = 0; i < params->count; ++i) {
        params->data[i] = F.FoldType(&F, params->data[i]);
    }
    t->result = F.FoldType(&F, t->result);
}

static void instantiate_func_aux(struct InstanceState *I, struct HirFuncDecl *base, struct HirInstanceDecl *inst, struct HirTypeList *types)
{
    inst->is_pub = base->is_pub;
    inst->name = base->name;
    inst->types = types;

    struct HirType *result = register_decl_type(I, HIR_CAST_DECL(inst), kHirFuncDef);
    struct HirFuncDef *r = HirGetFuncDef(result);
    r->modno = HirGetFuncDef(base->type)->modno;
    r->base = base->did;
    r->params = collect_field_types(I, base->params);
    r->result = func_result(base);
    r->types = types;
    inst->type = result;

    struct HirFuncDef *base_fdef = HirGetFuncDef(base->type);
    prep_func_instance(I, base_fdef->types, types, inst, r);
}

// TODO: polymorphic methods
//static struct HirDecl *instantiate_method(struct InstanceState *I, struct HirFuncDecl *base, struct HirTypeList *types)
//{
//    struct HirDecl *inst = pawHir_new_decl(I->hir, base->line, kHirInstanceDecl);
//    instantiate_func_aux(I, base, HirGetInstanceDecl(inst), types);
//    return inst;
//}

static void instantiate_adt_aux(struct InstanceState *I, struct HirAdtDecl *base,
                                struct HirInstanceDecl *inst, struct HirTypeList *types)
{
    inst->name = base->name;
    inst->types = types;

    struct HirType *result = register_decl_type(I, HIR_CAST_DECL(inst), kHirAdt);
    struct HirAdt *r = HirGetAdt(result);
    r->modno = HirGetAdt(base->type)->modno;
    r->base = base->did;
    r->did = inst->did;
    r->types = types;
}

static void check_type_param(struct InstanceState *I, struct HirTypeList *params, struct HirTypeList *args)
{
    if (args->count > params->count) {
        TYPE_ERROR(I, "too many generics");
    } else if (args->count < params->count) {
        TYPE_ERROR(I, "not enough generics");
    }
}

static void normalize_type_list(struct InstanceState *I, struct HirTypeList *types)
{
    for (int i = 0; i < types->count; ++i) {
        pawU_normalize(I->U->table, types->data[i]);
    }
}

static struct HirDecl *instantiate_impl_method(struct InstanceState *I, struct HirFuncDecl *func, struct HirTypeList *generics, struct HirTypeList *types)
{
    struct HirDecl *result = pawHir_new_decl(I->C, func->line, kHirInstanceDecl);
    struct HirInstanceDecl *r = HirGetInstanceDecl(result);
    r->is_pub = func->is_pub;
    r->is_assoc = func->is_assoc;
//    r->self = func->self;
    r->name = func->name;
    r->types = types;

    const struct HirAdt *self = HirGetAdt(func->self);
    struct HirDecl *base = get_decl(I, self->base);
    struct HirTypeList *args = instantiate_typelist(I, generics, types,
            hir_adt_types(HIR_TYPEOF(base)));
    struct HirDecl *inst = pawP_instantiate(I->hir, base, args);
    r->self = HIR_TYPEOF(inst);

    struct HirType *type = register_decl_type(I, result, kHirFuncDef);
    struct HirFuncDef *t = HirGetFuncDef(type);
    t->types = collect_generic_types(I, func->generics);
    t->params = collect_field_types(I, func->params);
    t->result = func_result(func);
    t->base = func->did;
    t->modno = self->modno;
    r->type = type;

    prep_func_instance(I, generics, types, r, t);
    return result;
}

static struct HirDecl *instantiate_impl_aux(struct InstanceState *I, struct HirImplDecl *base, struct HirTypeList *types, struct HirType *self)
{
    paw_assert(HirIsAdt(self));
    struct HirDecl *result = find_impl_instance(I, base, types);
    if (result != NULL) return result;
    result = pawHir_new_decl(I->C, base->line, kHirImplDecl);
    struct HirImplDecl *r = HirGetImplDecl(result);
    r->methods = pawHir_decl_list_new(I->C);
    r->name = base->name;
    r->subst = types;
    r->type = self;

    struct HirTypeList *generics = collect_generic_types(I, base->generics);
    paw_assert(generics->count == types->count);
    for (int i = 0; i < base->methods->count; ++i) {
        struct HirDecl *src = pawHir_decl_list_get(base->methods, i);
        struct HirDecl *dst = instantiate_impl_method(I, HirGetFuncDecl(src), generics, types);
        pawHir_decl_list_push(I->C, r->methods, dst);
    }
    pawHir_decl_list_push(I->C, base->monos, result);
    return result;
}

static struct HirDecl *instantiate_adt(struct InstanceState *I, struct HirAdtDecl *base, struct HirTypeList *types)
{
    struct HirAdt *adt = HirGetAdt(base->type);
    check_type_param(I, adt->types, types);
    normalize_type_list(I, types);
    struct HirDecl *inst = find_adt_instance(I, base, types);
    if (inst != NULL) return inst;
    inst = pawHir_new_decl(I->C, base->line, kHirInstanceDecl);
    pawHir_decl_list_push(I->C, base->monos, inst);
    instantiate_adt_aux(I, base, HirGetInstanceDecl(inst), types);
    return inst;
}

static struct HirDecl *instantiate_func(struct InstanceState *I, struct HirFuncDecl *base, struct HirTypeList *types)
{
    struct HirFuncDef *func = HirGetFuncDef(base->type);
    check_type_param(I, func->types, types);
    normalize_type_list(I, types);
    struct HirDecl *inst = find_func_instance(I, base, types);
    if (inst != NULL) return inst;
    inst = pawHir_new_decl(I->C, base->line, kHirInstanceDecl);
    pawHir_decl_list_push(I->C, base->monos, inst);
    instantiate_func_aux(I, base, HirGetInstanceDecl(inst), types);
    return inst;
}

static struct HirDecl *instantiate_impl(struct InstanceState *I, struct HirImplDecl *impl, struct HirTypeList *types)
{
    struct HirType *type = impl->type;
    paw_assert(types->count == hir_adt_types(type)->count);
    struct HirTypeList *generics = collect_generic_types(I, impl->generics);
    struct HirTypeList *unknowns = new_unknowns(I, generics->count);

    // Substitute the polymorphic impl block's generics for inference variables (unknowns)
    // in the context of its 'self' ADT. For example:
    //     impl<X, Y> A<int, Y, X> => impl<?0, ?1> A<int, ?1, ?0>
    // where unknowns = [?0, ?1] and subst = [int, ?1, ?0]. Unifying with the given ADTs
    // type arguments yields a concrete type for each of the impl block's generics.
    struct HirTypeList *subst = instantiate_typelist(I, generics, unknowns, hir_adt_types(type));
    for (int i = 0; i < subst->count; ++i) {
        pawU_unify(I->U, subst->data[i], types->data[i]);
    }

    struct HirDecl *base = get_decl(I, hir_adt_base(type));
    struct HirDecl *inst = pawP_instantiate(I->hir, base, subst);
    return instantiate_impl_aux(I, impl, unknowns, HIR_TYPEOF(inst));
}

static struct HirDecl *instantiate_partial(struct Hir *hir, struct HirDecl *base, struct HirTypeList *types)
{
    struct InstanceState I = {
        .U = &hir->C->dm->unifier,
        .P = ENV(hir->C),
        .C = hir->C,
        .hir = hir,
    };

    if (types == NULL) return base;
    if (HIR_IS_POLY_ADT(base)) {
        return instantiate_adt(&I, &base->adt, types);
    } else if (HIR_IS_POLY_FUNC(base)) {
        return instantiate_func(&I, &base->func, types);
    } else if (HIR_IS_POLY_IMPL(base)) {
        return instantiate_impl(&I, &base->impl, types);
    }
    return base;
}

static struct HirDecl *instantiate_full(struct Hir *hir, struct HirDecl *base, struct HirTypeList *types)
{
    struct HirDecl *inst = instantiate_partial(hir, base, types);
    if (HirIsAdtDecl(base) && HirIsInstanceDecl(inst)) {
        pawHir_expand_adt(hir, HirGetAdtDecl(base), inst);
        pawP_instantiate_impls_for(hir, HirGetAdtDecl(base), HirGetAdtDecl(inst), types);
    }
    return inst;
}

// Replace generic parameters with inference variables (struct HirUnknown). The
// resulting '.fields' list can be unified with another list of types (argument or
// struct field types) to infer a concrete type for each unknown.
struct Generalization pawP_generalize(struct Hir *hir, struct HirDeclList *generics, struct HirDeclList *fields)
{
    struct InstanceState I = {
        .U = &hir->C->dm->unifier,
        .P = ENV(hir->C),
        .C = hir->C,
        .hir = hir,
    };

    if (generics == NULL) return (struct Generalization){0};
    struct HirTypeList *gtypes = collect_generic_types(&I, generics);
    struct HirTypeList *ftypes = collect_field_types(&I, fields);
    struct HirTypeList *unknowns = new_unknowns(&I, generics->count);
    struct HirTypeList *replaced = instantiate_typelist(&I, gtypes, unknowns, ftypes);
    return (struct Generalization){
        .types = unknowns,
        .fields = replaced,
    };
}

struct HirDecl *pawP_instantiate(struct Hir *hir, struct HirDecl *base, struct HirTypeList *types)
{
    return hir->C->finst(hir, base, types);
}

void pawP_set_instantiate(struct Compiler *C, paw_Bool full)
{
    C->finst = full ? instantiate_full : instantiate_partial;
}
