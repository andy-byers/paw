// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "hir.h"
#include "ir_type.h"
#include "resolve.h"
#include "unify.h"

#define LOWERING_ERROR(L_, Kind_, ...) pawErr_##Kind_((L_)->C, (L_)->m->name, __VA_ARGS__)

struct LowerType {
    struct HirModule m;
    struct Compiler *C;
    struct Hir *hir;
};

static IrType *lower_type(struct LowerType *L, struct HirType *type);
static IrTypeList *lower_type_list(struct LowerType *L, struct HirTypeList *types);

static IrType *lower_fn_ptr(struct LowerType *L, struct HirFnPtr *t)
{
    IrTypeList *params = lower_type_list(L, t->params);
    IrType *result = lower_type(L, t->result);
    return pawIr_new_fn_ptr(L->C, params, result);
}

static IrType *lower_tuple_type(struct LowerType *L, struct HirTupleType *t)
{
    IrTypeList *elems = lower_type_list(L, t->elems);
    return pawIr_new_tuple(L->C, elems);
}

static IrTypeList *new_unknowns(struct Compiler *C, struct HirDeclList *params, IrTypeList **out)
{
    IrTypeList *unknowns = IrTypeList_new(C);
    IrTypeList *generics = IrTypeList_new(C);
    IrTypeList_reserve(C, generics, params->count);
    IrTypeList_reserve(C, unknowns, params->count);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (params, pdecl) {
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        IrType *type = pawIr_get_type(C, d->id);
        IrTypeList *bounds = IrGetGeneric(type)->bounds;
        IrType *unknown = pawU_new_unknown(C->U, d->span.start, bounds);
        IrTypeList_push(C, unknowns, unknown);
        IrTypeList_push(C, generics, type);
    }

    *out = generics;
    return unknowns;
}

IrType *lower_type_alias(struct Compiler *C, struct HirSegment segment, struct HirDecl *decl, IrTypeList *knowns)
{
    paw_assert(HirIsTypeDecl(decl));
    IrType *type = GET_NODE_TYPE(C, decl);
    struct HirTypeDecl *d = HirGetTypeDecl(decl);

    // TODO: is this correct? prob. needs to be instantiated, "seg" likely not used later so it seems to work
    pawIr_set_type(C, segment.id, type);
    decl = pawHir_get_decl(C->hir, IR_TYPE_DID(type));

    if (d->rhs == NULL) return type;
    IrType *rhs = GET_NODE_TYPE(C, d->rhs);
    IrTypeList *types = IR_TYPE_SUBTYPES(rhs);
    if (d->generics == NULL) return rhs;

    IrTypeList *generics;
    IrTypeList *unknowns = new_unknowns(C, d->generics, &generics);
    IrTypeList *subst = pawP_instantiate_typelist(C, generics, unknowns, types);
    if (knowns != NULL) {
        IrType **pu, **pk;
        K_LIST_ZIP (unknowns, pu, knowns, pk) {
            // unification with an IrInfer never fails due to incompatible types
            int const rc = pawU_unify(C->U, *pu, *pk);
            paw_assert(rc == 0); PAW_UNUSED(rc);
        }
    }

    return pawP_instantiate(C, rhs, subst);
}

static IrType *lower_path_type(struct LowerType *L, struct HirPathType *t)
{
    paw_assert(t->path.segments->count == 1);
    paw_assert(t->path.kind == HIR_PATH_ITEM);
    struct HirSegment const segment = K_LIST_LAST(t->path.segments);
    struct HirDecl *decl = pawHir_get_node(L->hir, segment.target);
    if (HirIsTypeDecl(decl)) {
        IrTypeList *args = segment.types != NULL ? lower_type_list(L, segment.types) : NULL;
        return lower_type_alias(L->C, segment, decl, args);
    }
    IrType *type = pawIr_get_type(L->C, segment.target);
    if (segment.types != NULL) {
        IrTypeList *args = lower_type_list(L, segment.types);
        type = pawP_instantiate(L->C, type, args);
    }
    pawIr_set_type(L->C, segment.id, type);
    return type;
}

static IrType *lower_infer_type(struct LowerType *L, struct HirInferType *t)
{
    return pawU_new_unknown(L->C->U, t->span.start, NULL);
}

static IrType *lower_never_type(struct LowerType *L, struct HirNeverType *t)
{
    return pawIr_new_never(L->C);
}

static IrType *lower_type(struct LowerType *L, struct HirType *type)
{
    IrType *result;
    switch (HIR_KINDOF(type)) {
        case kHirFnPtr:
            result = lower_fn_ptr(L, HirGetFnPtr(type));
            break;
        case kHirTupleType:
            result = lower_tuple_type(L, HirGetTupleType(type));
            break;
        case kHirPathType:
            result = lower_path_type(L, HirGetPathType(type));
            break;
        case kHirInferType:
            result = lower_infer_type(L, HirGetInferType(type));
            break;
        case kHirNeverType:
            result = lower_never_type(L, HirGetNeverType(type));
            break;
    }

    pawIr_set_type(L->C, type->hdr.id, result);
    return result;
}

IrType *pawP_lower_type(struct Compiler *C, struct HirModule m, struct HirType *type)
{
    struct LowerType L = {
        .hir = C->hir,
        .C = C,
        .m = m,
    };
    return lower_type(&L, type);
}

static IrTypeList *lower_type_list(struct LowerType *L, struct HirTypeList *types)
{
    IrTypeList *result = IrTypeList_new(L->C);
    IrTypeList_reserve(L->C, result, types->count);

    struct HirType *const *ptype;
    K_LIST_FOREACH (types, ptype) {
        IrType *type = lower_type(L, *ptype);
        IrTypeList_push(L->C, result, type);
    }
    return result;
}

IrTypeList *pawP_lower_type_list(struct Compiler *C, struct HirModule m, struct HirTypeList *types)
{
    struct LowerType L = {
        .hir = C->hir,
        .C = C,
        .m = m,
    };
    return lower_type_list(&L, types);
}

