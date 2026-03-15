// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "error.h"
#include "hir.h"
#include "impl.h"
#include "ir_type.h"
#include "resolve.h"
#include "solve.h"
#include "unify.h"

struct LowerType {
    struct HirModule m;
    struct Compiler *C;
    struct Hir *hir;
};

static DeclId next_did(struct LowerType *L)
{
    return (DeclId){
        .value = (unsigned)++L->C->decl_count,
        .modno = (unsigned)L->m.modno,
    };
}

static IrType *lower_type(struct LowerType *L, struct HirType *type);
static IrTypeList *lower_type_list(struct LowerType *L, struct HirTypeList *types);
static IrGenericArgs *lower_generic_args(struct LowerType *L, struct HirGenericArgs *types);

static IrType *lower_fn_ptr(struct LowerType *L, struct HirFnPtr *t)
{
    IrTypeList *params = lower_type_list(L, t->params);
    IrType *result = lower_type(L, t->result);
    return pawIr_new_fn_ptr(L->C, params, result);
}

static IrType *lower_array_type(struct LowerType *L, struct HirArrayType *t)
{
    DeclId const did = next_did(L);
    IrType *type = lower_type(L, t->type);
    SET_NODE_TYPE(L->C, t->length, pawIr_new_int(L->C));

    IrConst *length = pawIr_new_const_pending(L->C, did);
    IrPendingConstants_insert(L->C, L->C->pending_constants, did,
            (struct IrPendingConstant){
                .payload = t->length,
                .konst = length,
            });
    return pawIr_new_array(L->C, type, length);
}

static IrType *lower_projection_type(struct LowerType *L, struct HirProjectionType *t)
{
    PAW_UNUSED(L); PAW_UNUSED(t); PAW_UNREACHABLE(); // TODO: unimplemented
}

static IrType *lower_slice_type(struct LowerType *L, struct HirSliceType *t)
{
    IrType *type = lower_type(L, t->type);
    return pawIr_new_slice(L->C, type);
}

static IrType *lower_tuple_type(struct LowerType *L, struct HirTupleType *t)
{
    IrTypeList *elems = lower_type_list(L, t->elems);
    return pawIr_new_tuple(L->C, elems);
}

static IrGenericArgs *collect_generic_args(struct Compiler *C, struct HirDeclList *params)
{
    IrGenericArgs *generics = IrGenericArgs_new(C);
    IrGenericArgs_reserve(C, generics, params->count);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (params, pdecl) {
        struct HirGenericDecl const *d = HirGetGenericDecl(*pdecl);
        IrGenericArgs_push(C, generics, *pawIr_get_generic_arg(C, d->did));
    }

    return generics;
}

static IrGenericArgs *new_unknowns(struct Compiler *C, struct HirDeclList *params)
{
    IrGenericArgs *unknowns = IrGenericArgs_new(C);
    IrGenericArgs_reserve(C, unknowns, params->count);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (params, pdecl) {
        struct HirGenericDecl const *d = HirGetGenericDecl(*pdecl);
        IrGenericArg const unknown = pawIr_instantiate(C, d->did);
        IrGenericArgs_push(C, unknowns, unknown);
    }

    return unknowns;
}

IrType *lower_type_alias(struct Compiler *C, struct HirSegment segment, struct HirDecl *decl, IrGenericArgs *knowns)
{
    paw_assert(HirIsTypeDecl(decl));
    IrType *type = GET_NODE_TYPE(C, decl);
    struct HirTypeDecl *d = HirGetTypeDecl(decl);

    // TODO: is this correct? prob. needs to be instantiated, "seg" likely not used later so it seems to work
    pawIr_set_type(C, segment.id, type);

    if (d->rhs == NULL) return type;
    IrType *rhs = GET_NODE_TYPE(C, d->rhs);
    if (d->generics == NULL) return rhs;
    IrGenericArgs *types = IR_GENERIC_ARGS(rhs);
    if (IrIsArray(rhs) || IrIsSlice(rhs)) {
        // TODO: make this work. need more general version of IR_GENERIC_ARGS that generates generic args for an array or slice
        pawErr_generic_error(ENV(C), ModuleInfo_get(C->modinfo, (int)decl->hdr.did.modno).name,
                decl->hdr.span, "type aliases are not supported on arrays or slices");
    }

    IrGenericArgs *generics = collect_generic_args(C, d->generics);
    IrGenericArgs *unknowns = new_unknowns(C, d->generics);
    IrGenericArgs *args = pawP_instantiate_typelist(C, generics, unknowns, types);
    if (knowns != NULL) {
        IrGenericArg const *pu;
        IrGenericArg const *pk;
        K_LIST_ZIP (unknowns, pu, knowns, pk) {
            // unification with an IrInfer never fails due to incompatible types
            int const rc = pawIr_unify(C, *pu, *pk);
            paw_assert(rc == 0); PAW_UNUSED(rc);
        }
        K_LIST_XFOREACH (args, IrGenericArg, p)
            *p = pawIr_normalize(C, *p);
    }

    struct Substitution const subst = {generics, args};
    return pawP_substitute(C, rhs, subst);
}

static IrType *lower_ptr_type(struct LowerType *L, struct HirRefType *t)
{
    return pawIr_new_ptr(L->C, lower_type(L, t->type));
}

static IrType *instantiate_segment(struct LowerType *L, struct HirSegment segment, IrType *type)
{
    if (segment.args != NULL) {
        IrGenericArgs *params = pawIr_get_generic_args(L->C, IR_TYPE_DID(type));
        IrGenericArgs *args = lower_generic_args(L, segment.args);
        struct Substitution const subst = {params, args};
        type = pawP_substitute(L->C, type, subst);
    } else if (!pawS_eq(segment.ident.name, SCAN_STR(L->C, "Self"))
            && IR_TYPE_IS_POLYMORPHIC(type)) {
        pawErr_generic_error(ENV(L->C), L->m.name, segment.span,
                "expected type arguments on polymorphic type");
    }
    pawIr_set_type(L->C, segment.id, type);
    return type;
}

static IrType *lower_path_type(struct LowerType *L, struct HirPathType *t)
{
    struct HirSegment const segment = HirSegments_first(t->path.segments);
    struct HirDecl *base_decl = pawHir_get_node(L->hir, segment.target);
    if (HirIsTypeDecl(base_decl)) {
        IrGenericArgs *args = segment.args != NULL
            ? lower_generic_args(L, segment.args) : NULL;
        return lower_type_alias(L->C, segment, base_decl, args);
    }
    IrType *type = pawIr_get_type(L->C, segment.target);
    if (type == NULL)
        pawErr_generic_error(ENV(L->C), L->m.name, segment.span,
                "trait \"%s\" cannot be used in place of a type",
                segment.ident.name);
    type = instantiate_segment(L, segment, type);
    for (int i = 1; i < t->path.segments->count; ++i) {
        struct HirSegment const segment = HirSegments_get(t->path.segments, i);
        if (NODE_ID_EXISTS(segment.target)) {
            paw_assert(segment.args == NULL);
            type = pawIr_get_type(L->C, segment.target);
        } else {
            struct Instantiation *assoc = pawIr_find_assoc_type_generic(L->C,
                    type, segment.ident.name);
            if (assoc == NULL)
                pawErr_generic_error(ENV(L->C), L->m.name, segment.span,
                        "associated type \"%s\" does not exist on \"%s\"",
                        segment.ident.name, pawIr_print_type(L->C, type));
            type = instantiate_segment(L, segment, assoc->inst);
        }
    }
    return type;

//    struct HirSegment const segment = K_LIST_FIRST(t->path.segments);
//    struct HirDecl *decl = pawHir_get_node(L->hir, segment.target);
//    if (HirIsTypeDecl(decl)) {
//        IrGenericArgs *args = segment.args != NULL ? lower_generic_args(L, segment.args) : NULL;
//        return lower_type_alias(L->C, segment, decl, args);
//    }
//    IrType *type = pawIr_get_type(L->C, segment.target);
//    if (segment.args != NULL) {
//        IrGenericArgs *args = lower_generic_args(L, segment.args);
//        type = pawIr_solver_instantiate_type_with(L->C->S, IR_TYPE_DID(type), args);
//    } else if (!pawS_eq(segment.ident.name, SCAN_STR(L->C, "Self"))
//            && IR_TYPE_IS_POLYMORPHIC(type)) {
//        pawErr_generic_error(ENV(L->C), L->m.name, t->span,
//                "expected type arguments on polymorphic type");
//    }
//    pawIr_set_type(L->C, segment.id, type);
//    return type;
}

static IrType *lower_infer_type(struct LowerType *L, struct HirInferType *t)
{
    return pawU_new_unknown(L->C->U, t->span);
}

static IrType *lower_never_type(struct LowerType *L, struct HirNeverType *t)
{
    PAW_UNUSED(t);
    return pawIr_new_never(L->C);
}

static IrType *lower_type(struct LowerType *L, struct HirType *type)
{
    IrType *result;
    switch (HIR_KINDOF(type)) {
        case kHirFnPtr:
            result = lower_fn_ptr(L, HirGetFnPtr(type));
            break;
        case kHirArrayType:
            result = lower_array_type(L, HirGetArrayType(type));
            break;
        case kHirProjectionType:
            result = lower_projection_type(L, HirGetProjectionType(type));
            break;
        case kHirSliceType:
            result = lower_slice_type(L, HirGetSliceType(type));
            break;
        case kHirTupleType:
            result = lower_tuple_type(L, HirGetTupleType(type));
            break;
        case kHirRefType:
            result = lower_ptr_type(L, HirGetRefType(type));
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

static IrConst *lower_const(struct LowerType *L, struct HirExpr *expr)
{
    __builtin_trap();
    return pawIr_new_const_pending(L->C, (DeclId){0}); // TODO
}

static IrGenericArg lower_generic_arg(struct LowerType *L, struct HirGenericArg arg)
{
    if (arg.is_type) {
        return IrGenericArg_from_type(lower_type(L, arg.t));
    } else {
        return IrGenericArg_from_const(lower_const(L, arg.k));
    }
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

IrGenericArg pawP_lower_generic_arg(struct Compiler *C, struct HirModule m, struct HirGenericArg arg)
{
    struct LowerType L = {
        .hir = C->hir,
        .C = C,
        .m = m,
    };
    return lower_generic_arg(&L, arg);
}

static IrGenericArgs *lower_generic_args(struct LowerType *L, struct HirGenericArgs *types)
{
    IrGenericArgs *result = IrGenericArgs_new(L->C);
    IrGenericArgs_reserve(L->C, result, types->count);

    struct HirGenericArg const *p;
    K_LIST_FOREACH (types, p) {
        IrGenericArg const arg = lower_generic_arg(L, *p);
        IrGenericArgs_push(L->C, result, arg);
    }
    return result;
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
