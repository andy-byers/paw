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

#define LOWERING_ERROR(X_, Kind_, ...) THROW_ERROR((X_)->C, \
        Kind_, .modname = (X_)->m.name, __VA_ARGS__)

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

static IrConst *lower_const(struct LowerType *L, struct HirExpr *expr);

static IrType *lower_array_type(struct LowerType *L, struct HirArrayType *t)
{
    IrType *type = lower_type(L, t->type);

    // note that the type of `t->length` is determined in typeck
    IrConst *length = lower_const(L, t->length);
    if (length->kind == IR_CONST_VALUE) {
        IrType *usize = pawIr_new_int(L->C, IR_USIZE);
        if (pawU_unify(L->C->U, length->value.type, usize) != 0)
            LOWERING_ERROR(L, IncompatibleTypes,
                    .lhs = pawIr_print_type_v2(L->C, length->value.type),
                    .rhs = pawIr_print_type_v2(L->C, usize),
                    .span = t->length->hdr.span);
    }
    return pawIr_new_array(L->C, type, length);
}

static DeclId const *locate_assoc_type(struct LowerType *L, IrTrait *trait, Str const *name)
{
    struct IrTraitDef const *def = pawIr_get_trait_def(L->C, trait->did);
    K_LIST_XFOREACH (def->items, struct IrAssocItem *const, pitem) {
        struct IrAssocItem const *item = *pitem;
        if (pawS_eq(item->name, name))
            return &item->did;
    }
    return NULL;
}

static IrType *lower_projection_type(struct LowerType *L, struct HirProjectionType *t)
{
    IrType *type = lower_type(L, t->type);

    struct HirSegment const segment = HirSegments_last(t->trait.segments);
    struct HirDecl *trait_decl = pawHir_get_node(L->hir, segment.target.id);
    if (!HirIsTraitDecl(trait_decl)) {
        LOWERING_ERROR(L, ExpectedTrait,
                .path = segment.ident.name,
                .span = segment.span);
    }

    IrGenericArgs *args = lower_generic_args(L, segment.args);
    IrGenericArgs_insert(L->C, args, 0, IrGenericArg_from_type(type));
    IrTrait *trait = pawIr_new_trait(L->C, trait_decl->hdr.did, args);

    IrType *assoc = NULL;
    if (IrIsGeneric(type) || IrIsProjection(type)) {
        DeclId const *pdid = locate_assoc_type(L, trait, t->name);
        if (pdid != NULL)
            assoc = pawIr_new_projection(L->C, *pdid, args);
    } else {
        struct Instantiation const *inst = pawIr_find_assoc_type_projection(
                L->C, type, trait, t->name);
        if (inst != NULL)
            assoc = inst->inst;
    }

    if (assoc == NULL)
        LOWERING_ERROR(L, UnknownAssociatedItem,
                .type = pawIr_print_type_v2(L->C, type),
                .item = t->name,
                .span = t->span);
    return assoc;
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

static IrType *lower_type_alias(struct LowerType *L, struct HirSegment segment, struct HirDecl *decl, IrGenericArgs *args)
{
    struct Compiler *C = L->C;
    paw_assert(HirIsTypeDecl(decl));
    IrType *type = GET_NODE_TYPE(C, decl);
    struct HirTypeDecl *d = HirGetTypeDecl(decl);
    pawIr_set_type(C, segment.id, type);

    if (d->rhs == NULL) return type;
    IrType *rhs = GET_NODE_TYPE(C, d->rhs);
    if (d->generics == NULL) return rhs;

    IrGenericArgs *params = collect_generic_args(C, d->generics);
    if (args == NULL) args = new_unknowns(C, d->generics);

    struct Substitution const subst = {params, args};
    rhs = pawP_substitute(C, rhs, subst);
    return pawU_normalize(C->U, rhs);
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
        LOWERING_ERROR(L, ExpectedTypeArguments,
                .name = segment.ident.name,
                .span = segment.span);
    }
    pawIr_set_type(L->C, segment.id, type);
    return type;
}

static IrType *lower_path_type(struct LowerType *L, struct HirPathType *t)
{
    struct HirSegment const segment = HirSegments_first(t->path.segments);
    struct HirDecl *base_decl = pawHir_get_node(L->hir, segment.target.id);
    if (HirIsTypeDecl(base_decl)) {
        IrGenericArgs *args = segment.args != NULL
            ? lower_generic_args(L, segment.args) : NULL;
        return lower_type_alias(L, segment, base_decl, args);
    }

    IrType *type = pawIr_get_type(L->C, segment.target.id);
    if (type == NULL)
        LOWERING_ERROR(L, UnexpectedTrait,
                .span = segment.span);
    type = instantiate_segment(L, segment, type);
    for (int i = 1; i < t->path.segments->count; ++i) {
        struct HirSegment const segment = HirSegments_get(t->path.segments, i);
        if (NODE_ID_EXISTS(segment.target.id)) {
            paw_assert(segment.args == NULL);
            type = pawIr_get_type(L->C, segment.target.id);
        } else {
            // Encountered a construct like `T::Type` where `T` is a generic type. There
            // must exist a single trait bound on `T` that declares an associated type
            // named `Type` (`T::Type` must be unambiguous, otherwise a projection type
            // is required to disambiguate).
            struct Instantiation *assoc = pawIr_find_assoc_type_generic(L->C,
                    type, segment.ident.name);
            if (assoc == NULL)
                LOWERING_ERROR(L, UnknownAssociatedItem,
                        .type = pawIr_print_type_v2(L->C, type),
                        .item = segment.ident.name,
                        .span = segment.span);
            type = instantiate_segment(L, segment, assoc->inst);
        }
    }
    return type;
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

static void contains_const_param_callback(struct HirVisitor *V, struct HirPathExpr *e)
{
    DeclId *pdid = V->ud;
    if (!DECL_ID_EXISTS(*pdid) && e->path.kind == HIR_PATH_ITEM) {
        struct HirSegment const segment = HirSegments_last(e->path.segments);
        struct HirDecl *item = pawHir_get_node(V->hir, segment.target.id);
        if (HirIsGenericDecl(item)) *pdid = item->hdr.did;
    }
}

static paw_Bool contains_const_param(struct LowerType *L, struct HirExpr *expr)
{
    struct HirVisitor V;
    DeclId param_did = INVALID_DECL_ID;
    pawHir_visitor_init(&V, L->hir, &param_did);
    V.PostVisitPathExpr = contains_const_param_callback;
    pawHir_visit_expr(&V, expr);
    return DECL_ID_EXISTS(param_did);
}

static IrConst *lower_const(struct LowerType *L, struct HirExpr *expr)
{
    if (HirIsLiteralExpr(expr)) {
        struct HirLiteralExpr const *t = HirGetLiteralExpr(expr);
        union IrValue value; IrType *type;
        switch (t->lit_kind) {
            case HIR_LIT_BOOL:
                value.b = t->b;
                type = pawIr_new_bool(L->C);
                break;
            case HIR_LIT_CHAR:
                value.c = t->c;
                type = pawIr_new_char(L->C);
                break;
            case HIR_LIT_INT:
                value.i64 = t->i.value;
                switch (t->i.suffix) {
                    case NS_I8:
                        type = pawIr_new_int(L->C, IR_INT8);
                        break;
                    case NS_I16:
                        type = pawIr_new_int(L->C, IR_INT16);
                        break;
                    case NS_I32:
                        type = pawIr_new_int(L->C, IR_INT32);
                        break;
                    case NS_I64:
                        type = pawIr_new_int(L->C, IR_INT64);
                        break;
                    case NS_ISIZE:
                        type = pawIr_new_int(L->C, IR_ISIZE);
                        break;
                    case NS_U8:
                        type = pawIr_new_int(L->C, IR_UINT8);
                        break;
                    case NS_U16:
                        type = pawIr_new_int(L->C, IR_UINT16);
                        break;
                    case NS_U32:
                        type = pawIr_new_int(L->C, IR_UINT32);
                        break;
                    case NS_U64:
                        type = pawIr_new_int(L->C, IR_UINT64);
                        break;
                    case NS_USIZE:
                        type = pawIr_new_int(L->C, IR_USIZE);
                        break;
                    default:
                        type = pawU_new_type_var(L->C->U, IR_INFER_INTEGER, expr->hdr.span);
                        break;
                }
                break;
            default:
                paw_assert(t->lit_kind == HIR_LIT_FLOAT);
                switch (t->f.suffix) {
                    case NS_F32:
                        value.f32 = t->f.value;
                        type = pawIr_new_float(L->C, IR_FLOAT32);
                        break;
                    case NS_F64:
                        value.f64 = t->f.value;
                        type = pawIr_new_float(L->C, IR_FLOAT64);
                        break;
                    default:
                        value.f64 = t->f.value;
                        type = pawU_new_type_var(L->C->U, IR_INFER_FLOAT, expr->hdr.span);
                        break;
                }
        }
        return pawIr_new_const_value(L->C, value, type);
    } else if (HirIsPathExpr(expr)) {
        struct HirPathExpr const *e = HirGetPathExpr(expr);
        paw_assert(e->path.kind != HIR_PATH_UPVALUE);
        struct HirSegment const segment = HirSegments_last(e->path.segments);
        struct HirDecl *item = pawHir_get_node(L->hir, segment.target.id);
        if (HirIsGenericDecl(item))
            return pawIr_new_const_decl(L->C, item->hdr.did);
    } else if (HirIsBlock(expr)) {
        struct HirBlock const *e = HirGetBlock(expr);
        return lower_const(L, e->result);
    } else if (contains_const_param(L, expr)) {
        // const generic used in expression
        LOWERING_ERROR(L, Unsupported, expr->hdr.span);
    }
    DeclId const did = next_did(L);
    IrConst *konst = pawIr_new_const_pending(L->C, did);
    IrPendingConstants_insert(L->C, L->C->pending_constants, did,
            (struct IrPendingConstant){
                .payload = expr,
                .konst = konst,
            });
    return konst;
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

IrType *pawP_lower_type_alias(struct Compiler *C, struct HirSegment segment, struct HirDecl *decl, IrGenericArgs *knowns)
{
    struct HirModule m = HirModuleList_get(C->hir->modules, (int)decl->hdr.did.modno);

    struct LowerType L = {
        .hir = C->hir,
        .C = C,
        .m = m,
    };

    return lower_type_alias(&L, segment, decl, knowns);
}

static IrGenericArgs *lower_generic_args(struct LowerType *L, struct HirGenericArgs *types)
{
    IrGenericArgs *result = IrGenericArgs_new(L->C);
    if (types != NULL) {
        IrGenericArgs_reserve(L->C, result, types->count);
        K_LIST_XFOREACH (types, struct HirGenericArg const, p) {
            IrGenericArg const arg = lower_generic_arg(L, *p);
            IrGenericArgs_push(L->C, result, arg);
        }
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

static void check_projection_type(struct HirVisitor *V, struct HirProjectionType *t)
{
    struct LowerType *L = V->ud;
    IrType *maybe_projection = pawIr_get_type(L->C, t->id);
    if (!IrIsProjection(maybe_projection)) return;
    struct IrProjection *p = IrGetProjection(maybe_projection);
    IrType *type = ir_projection_self(p);
    IrTrait *trait = pawIr_get_projection_trait(L->C, p);

    // prove that the trait is implemented by the type
    IrSolver *child = pawIr_push_solver(L->C);
    if (IrIsAdt(type))
        pawIr_solver_add_well_formed_obligation(child, trait->did, p->args,
                (struct IrObligationCause){.span = t->trait.span});
    pawIr_solver_add_well_formed_obligation(child, trait->did, p->args,
            (struct IrObligationCause){.span = t->trait.span});
    pawIr_solver_add_impl_trait_obligation(child, type, trait,
            (struct IrObligationCause){.span = t->span});
    struct IrSolverResult const result = pawIr_solver_solve(child);

    switch (result.status) {
        case IR_SOLVER_SOLVED:
            break;
        case IR_SOLVER_AMBIGUOUS: {
            struct IrObligation const example = pawIr_solver_first_obligation(L->C->S);
            LOWERING_ERROR(L, UnsatisfiedObligation,
                    .example = pawIr_print_obligation_(L->C, example),
                    .num_unsolved = result.ambiguous.num_unsolved,
                    .span = example.cause.span);
            break;
        }
        case IR_SOLVER_ERROR:
            LOWERING_ERROR(L, FalseObligation,
                    .obligation = pawIr_print_obligation_(L->C, result.error.obligation),
                    .span = result.error.obligation.cause.span);
    }

    pawIr_pop_solver(L->C);
}

void pawP_solve_type_obligations(struct Hir *hir, struct HirModule m, struct HirType *type)
{
    struct LowerType L = {
        .hir = hir,
        .C = hir->C,
        .m = m,
    };

    struct HirVisitor V;
    pawHir_visitor_init(&V, hir, &L);
    V.PostVisitProjectionType = check_projection_type;
    pawHir_visit_type(&V, type);
}

