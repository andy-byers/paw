// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "ir_type.h"
#include "map.h"
#include "solve.h"
#include "resolve.h"
#include "type_folder.h"
#include "unify.h"

#define NEW_NODE(C, T) (T *)P_ALLOC(C, NULL, 0, sizeof(T))

#define TODO (struct SourceSpan){0}

static paw_Bool is_unsized_type(IrType *type)
{
    return 0;
    return IrIsSlice(type)
        || IrIsString(type);
}

paw_Bool pawIr_is_unsized_type(struct Compiler *C, IrType *type)
{
    return 0;
    if (!IrIsAdt(type))
        return IrIsSlice(type) || IrIsString(type);

    paw_Bool is_unsized = PAW_FALSE;
    struct IrAdt *t = IrGetAdt(type);
    struct IrAdtDef const *def = pawIr_get_adt_def(C, t->did);
    for (int discr = 0; discr < def->variants->count; ++discr) {
        IrTypeList *fields = pawP_instantiate_variant_fields(C, t, discr);
        paw_Bool is_unsized_variant = PAW_FALSE;
        K_LIST_XFOREACH (fields, IrType *const, p) {
            if (pawIr_is_unsized_type(C, *p)) {
                if (is_unsized_variant)
                   pawErr_generic_error(ENV(C), SCAN_STR(C, ""), (struct SourceSpan){0},
                           "only a single unsized type is allowed in an ADT variant");
            } else if (is_unsized_variant) {
               pawErr_generic_error(ENV(C), SCAN_STR(C, ""), (struct SourceSpan){0},
                       "unsized field must be last");
            }
        }
        is_unsized |= is_unsized_variant;
    }
    return is_unsized;
}

IrType *pawIr_remove_indirection(struct Compiler *C, IrType *type)
{
    while (IrIsPtr(type)) {
        IrType *pointee = IrGetPtr(type)->pointee;
        if (pawIr_is_unsized_type(C, pointee)) break;
        type = pointee;
    }
    return type;
}

static void add_deref_chain(struct Compiler *C, IrType *type, IrTypeList *chain)
{
    IrTypeList_push(C, chain, type);
    while (IrIsPtr(type)) {
        type = ir_deref(type);
        IrTypeList_push(C, chain, type);
    }
}

IrTypeList *pawIr_autoptr_chain(struct Compiler *C, IrType *type)
{
    IrTypeList *chain = IrTypeList_new(C);
    IrTypeList_push(C, chain, type);
    if (IrIsPtr(type)) {
        add_deref_chain(C, type, chain);
    } else {
        IrTypeList_push(C, chain, pawIr_new_ptr(C, type));
    }
    return chain;
}

void pawIr_set_def_kind(struct Compiler *C, DeclId did, enum IrDefKind kind)
{
    paw_assert(DECL_ID_EXISTS(did));

    IrDefKinds_insert(C, C->ir_def_kinds, did, kind);
}

IrTrait *pawIr_new_trait(struct Compiler *C, DeclId did, IrGenericArgs *args)
{
    paw_assert(DECL_ID_EXISTS(did));
    paw_assert(args != NULL);

    IrTrait *trait = NEW_NODE(C, IrTrait);
    *trait = (IrTrait){
        .args = args,
        .did = did,
    };
    return trait;
}

IrType *pawIr_new_unit(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Unit_ = (struct IrUnit){
        .kind = kIrUnit,
    };
    return t;
}

IrType *pawIr_new_bool(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Bool_ = (struct IrBool){
        .kind = kIrBool,
    };
    return t;
}

IrType *pawIr_new_char(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Char_ = (struct IrChar){
        .kind = kIrChar,
    };
    return t;
}

IrType *pawIr_new_int(struct Compiler *C, enum IrIntKind ikind)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Int_ = (struct IrInt){
        .kind = kIrInt,
        .ikind = ikind,
    };
    return t;
}

IrType *pawIr_new_float(struct Compiler *C, enum IrFloatKind fkind)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Float_ = (struct IrFloat){
        .kind = kIrFloat,
        .fkind = fkind,
    };
    return t;
}

IrType *pawIr_new_string(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->String_ = (struct IrString){
        .kind = kIrString,
    };
    return t;
}

IrType *pawIr_new_ptr(struct Compiler *C, IrType *pointee)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Ptr_ = (struct IrPtr){
        .kind = kIrPtr,
        .pointee = pointee,
    };
    return t;
}

IrType *pawIr_new_adt(struct Compiler *C, DeclId did, IrGenericArgs *args)
{
    paw_assert(args != NULL);
    IrType *t = NEW_NODE(C, IrType);
    t->Adt_ = (struct IrAdt){
        .kind = kIrAdt,
        .did = did,
        .args = args,
    };
    return t;
}

IrType *pawIr_new_fn_ptr(struct Compiler *C, IrTypeList *params, IrType *result)
{
    IrType *t = NEW_NODE(C, IrType);
    t->FnPtr_ = (struct IrFnPtr){
        .kind = kIrFnPtr,
        .params = params,
        .result = result,
    };
    return t;
}

IrType *pawIr_new_closure(struct Compiler *C, DeclId did, IrGenericArgs *args)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Closure_ = (struct IrClosure){
        .kind = kIrClosure,
        .did = did,
        .args = args,
    };
    return t;
}

IrType *pawIr_new_signature(struct Compiler *C, DeclId did, IrGenericArgs *args)
{
    paw_assert(args != NULL);
    IrType *t = NEW_NODE(C, IrType);
    t->Signature_ = (struct IrSignature){
        .kind = kIrSignature,
        .did = did,
        .args = args,
    };
    return t;
}

IrType *pawIr_new_slice(struct Compiler *C, IrType *type)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Slice_ = (struct IrSlice){
        .kind = kIrSlice,
        .type = type,
    };
    return t;
}

IrType *pawIr_new_tuple(struct Compiler *C, IrTypeList *elems)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Tuple_ = (struct IrTuple){
        .kind = kIrTuple,
        .elems = elems,
    };
    return t;
}

IrType *pawIr_new_array(struct Compiler *C, IrType *type, IrConst *length)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Array_ = (struct IrArray){
        .kind = kIrArray,
        .type = type,
        .length = length,
    };
    return t;
}

IrConst *pawIr_new_const_value(struct Compiler *C, union IrValue value, IrType *type)
{
    IrConst *k = NEW_NODE(C, IrConst);
    *k = (struct IrConst){
        .kind = IR_CONST_VALUE,
        .value.value = value,
        .value.type = type,
    };
    return k;
}

IrConst *pawIr_new_const_pending(struct Compiler *C, DeclId did)
{
    IrConst *k = NEW_NODE(C, IrConst);
    *k = (struct IrConst){
        .kind = IR_CONST_PENDING,
        .pending.did = did,
    };
    return k;
}

IrConst *pawIr_new_const_decl(struct Compiler *C, DeclId did)
{
    IrConst *k = NEW_NODE(C, IrConst);
    *k = (struct IrConst){
        .kind = IR_CONST_DECL,
        .decl.did = did,
    };
    return k;
}

IrConst *pawIr_new_const_infer(struct Compiler *C, int depth, int index)
{
    IrConst *k = NEW_NODE(C, IrConst);
    *k = (struct IrConst){
        .kind = IR_CONST_INFER,
        .infer.depth = depth,
        .infer.index = index,
    };
    return k;
}

IrType *pawIr_new_never(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Never_ = (struct IrNever){
        .kind = kIrNever,
    };
    return t;
}

IrType *pawIr_new_infer(struct Compiler *C, enum IrInferKind ikind, int depth, int index)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Infer_ = (struct IrInfer){
        .kind = kIrInfer,
        .ikind = ikind,
        .depth = depth,
        .index = index,
    };
    return t;
}

IrType *pawIr_new_generic(struct Compiler *C, DeclId did)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Generic_ = (struct IrGeneric){
        .kind = kIrGeneric,
        .did = did,
    };
    return t;
}

IrType *pawIr_new_projection(struct Compiler *C, DeclId did, struct IrGenericArgs *args)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Projection_ = (struct IrProjection){
        .kind = kIrProjection,
        .did = did,
        .args = args,
    };
    return t;
}


#define GA_TYPE_BIT 1ULL

IrGenericArg IrGenericArg_from_type(IrType *t)
{
    return (IrGenericArg){
        .inner = (IrType *)((uintptr_t)t | GA_TYPE_BIT),
    };
}

IrGenericArg IrGenericArg_from_const(IrConst *k)
{
    return (IrGenericArg){
        .inner = k,
    };
}

paw_Bool IrGenericArg_is_type(IrGenericArg ga)
{
    return (uintptr_t)ga.inner & GA_TYPE_BIT;
}

IrType *IrGenericArg_get_type(IrGenericArg ga)
{
    paw_assert(IrGenericArg_is_type(ga));
    return (IrType *)((uintptr_t)ga.inner & ~GA_TYPE_BIT);
}

IrConst *IrGenericArg_get_const(IrGenericArg ga)
{
    paw_assert(IrGenericArg_is_const(ga));
    return ga.inner;
}

IrGenericArgs *pawIr_instantiate_args(struct Compiler *C, DeclId did)
{
    IrGenericArgs *args = pawIr_get_generic_args(C, did);

    IrGenericArgs *result = IrGenericArgs_new(C);
    IrGenericArgs_reserve(C, result, args->count);
    K_LIST_XFOREACH (args, IrGenericArg const, p) {
        IrGenericArg r;
        if (IrGenericArg_is_type(*p)) {
            IrType *t = pawU_new_type_var(C->U, IR_INFER_TYPE, TODO);
            r = IrGenericArg_from_type(t);
        } else {
            IrConst *k = pawU_new_const_var(C->U, TODO);
            r = IrGenericArg_from_const(k);
        }
        IrGenericArgs_push(C, result, r);
    }
    return result;
}

IrGenericArg pawIr_instantiate(struct Compiler *C, DeclId did)
{
    IrGenericArg arg = *pawIr_get_generic_arg(C, did);

    IrGenericArg r;
    if (IrGenericArg_is_type(arg)) {
        IrType *t = pawU_new_type_var(C->U, IR_INFER_TYPE, TODO);
        r = IrGenericArg_from_type(t);
    } else {
        IrConst *k = pawU_new_const_var(C->U, TODO);
        r = IrGenericArg_from_const(k);
    }
    return r;
}


int pawIr_unify(struct Compiler *C, IrGenericArg a, IrGenericArg b)
{
    if (IrGenericArg_is_type(a) != IrGenericArg_is_type(b))
        return -1;

    if (IrGenericArg_is_type(a)) {
        IrType *x = IrGenericArg_get_type(a);
        IrType *y = IrGenericArg_get_type(b);
        return pawU_unify(C->U, x, y);
    } else {
        IrConst *x = IrGenericArg_get_const(a);
        IrConst *y = IrGenericArg_get_const(b);
        return pawU_unify_const(C->U, x, y);
    }

    return 0;
}

IrGenericArg pawIr_normalize(struct Compiler *C, IrGenericArg g)
{
    if (IrGenericArg_is_type(g)) {
        IrType *x = IrGenericArg_get_type(g);
        return IrGenericArg_from_type(
                pawU_normalize(C->U, x));
    } else {
        IrConst *x = IrGenericArg_get_const(g);
        return IrGenericArg_from_const(
                pawU_normalize_const(C->U, x));
    }
}

IrGenericArg pawIr_normalize_projections(struct Compiler *C, IrGenericArg g)
{
    if (IrGenericArg_is_type(g)) {
        IrType *x = IrGenericArg_get_type(g);
        return IrGenericArg_from_type(
                pawU_normalize_projections(C->U, x));
    } else {
        IrConst *x = IrGenericArg_get_const(g);
        return IrGenericArg_from_const(
                pawU_normalize_const(C->U, x));
    }
}


struct IrGenericDef *pawIr_new_generic_type_def(struct Compiler *C, DeclId did, Str *name, struct IrTraitList *bounds)
{
    struct IrGenericDef *def = (struct IrGenericDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrGenericDef){
        .did = did,
        .is_type = PAW_TRUE,
        .type.name = name,
        .type.bounds = bounds,
    };
    pawIr_set_def_kind(C, did, IR_GENERIC_DEF);
    return def;
}

struct IrGenericDef *pawIr_new_generic_const_def(struct Compiler *C, DeclId did, IrType *type, Str *name)
{
    struct IrGenericDef *def = (struct IrGenericDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrGenericDef){
        .did = did,
        .is_type = PAW_FALSE,
        .konst.type = type,
        .konst.name = name,
    };
    pawIr_set_def_kind(C, did, IR_GENERIC_DEF);
    return def;
}

struct IrFieldDef *pawIr_new_field_def(struct Compiler *C, DeclId did, Str *name, paw_Bool is_pub)
{
    struct IrFieldDef *def = (struct IrFieldDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrFieldDef){
        .did = did,
        .name = name,
        .is_pub = is_pub,
    };
    pawIr_set_def_kind(C, did, IR_FIELD_DEF);
    return def;
}

struct IrVariantDef *pawIr_new_variant_def(struct Compiler *C, DeclId did, DeclId cons_did, DeclId base_did, int discr, Str const *name, struct IrFieldDefs *fields)
{
    struct IrVariantDef *def = (struct IrVariantDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrVariantDef){
        .did = did,
        .cons_did = cons_did,
        .base_did = base_did,
        .fields = fields,
        .discr = discr,
        .name = name,
    };
    pawIr_set_def_kind(C, did, IR_VARIANT_DEF);
    return def;
}

struct IrFnDef *pawIr_new_fn_def(struct Compiler *C, DeclId did, Str *name, IrGenericDefs *generics, IrType *result, struct IrParams *params, IrType *context, DeclId parent, paw_Bool is_pub)
{
    struct IrFnDef *def = (struct IrFnDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrFnDef){
        .did = did,
        .parent = parent,
        .generics = generics,
        .context = context,
        .result = result,
        .params = params,
        .is_pub = is_pub,
        .name = name,
    };
    pawIr_set_def_kind(C, did, IR_FN_DEF);
    return def;
}

struct IrAdtDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, Str *name, IrGenericDefs *generics, IrVariantDefs *variants, paw_Bool is_pub, paw_Bool is_struct)
{
    struct IrAdtDef *def = (struct IrAdtDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrAdtDef){
        .did = did,
        .generics = generics,
        .variants = variants,
        .is_struct = is_struct,
        .is_pub = is_pub,
        .name = name,
    };
    pawIr_set_def_kind(C, did, IR_ADT_DEF);
    return def;
}

struct IrAssocItem *pawIr_new_assoc_item(struct Compiler *C, DeclId did, Str const *name, DeclId parent, paw_Bool is_pub)
{
    struct IrAssocItem *item = (struct IrAssocItem *)P_ALLOC(C, NULL, 0, sizeof(*item));
    *item = (struct IrAssocItem){
        .did = did,
        .name = name,
        .parent = parent,
        .is_pub = is_pub,
    };
    return item;
}

struct IrTraitDef *pawIr_new_trait_def(struct Compiler *C, DeclId did, Str *name, IrGenericDefs *generics, IrTypeList *methods, IrAssocItems *items, paw_Bool is_pub)
{
    struct IrTraitDef *def = (struct IrTraitDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrTraitDef){
        .did = did,
        .generics = generics,
        .methods = methods,
        .items = items,
        .is_pub = is_pub,
        .name = name,
    };
    pawIr_set_def_kind(C, did, IR_TRAIT_DEF);
    return def;
}

struct IrImpl *pawIr_new_impl(struct Compiler *C, DeclId did, IrType *type, IrTrait *trait, IrGenericDefs *generics, IrTypeList *methods, IrAssocItems *items)
{
    struct IrImpl *impl = (struct IrImpl *)P_ALLOC(C, NULL, 0, sizeof(*impl));
    *impl = (struct IrImpl){
        .did = did,
        .type = type,
        .trait = trait,
        .generics = generics,
        .methods = methods,
        .items = items,
    };
    pawIr_set_def_kind(C, did, IR_IMPL_DEF);
    return impl;
}

IrType *pawIr_get_type(struct Compiler *C, NodeId id)
{
    IrType *const *ptype = HirTypeMap_get(C, C->hir_types, id);
    return ptype != NULL ? *ptype : NULL;
}

IrGenericArg *pawIr_get_generic_arg(struct Compiler *C, DeclId did)
{
    return IrDeclArgs_get(C, C->ir_decl_args, did);
}

void pawIr_set_type(struct Compiler *C, NodeId id, IrType *type)
{
    paw_assert(type != NULL);
    HirTypeMap_insert(C, C->hir_types, id, type);
}

struct IrAssocItem *pawIr_get_assoc_item(struct Compiler *C, DeclId did)
{
    struct IrAssocItem *const *pdef = IrAssocItemMap_get(C, C->ir_assoc_items, did);
    return pdef != NULL ? *pdef : NULL;
}

struct IrFnDef *pawIr_get_fn_def(struct Compiler *C, DeclId did)
{
    struct IrFnDef *const *pdef = FnDefMap_get(C, C->fn_defs, did);
    return pdef != NULL ? *pdef : NULL;
}

struct IrGenericDef *pawIr_get_generic_def(struct Compiler *C, DeclId did)
{
    struct IrGenericDef *const *pdef = GenericDefMap_get(C, C->generic_defs, did);
    return pdef != NULL ? *pdef : NULL;
}

struct IrVariantDef *pawIr_get_variant_def(struct Compiler *C, DeclId did)
{
    return *VariantDefMap_get(C, C->variant_defs, did);
}

struct IrAdtDef *pawIr_get_adt_def(struct Compiler *C, DeclId did)
{
    return *AdtDefMap_get(C, C->adt_defs, did);
}

struct IrImpl *pawIr_get_impl_def(struct Compiler *C, DeclId did)
{
    return *ImplMap_get(C, C->impl_defs, did);
}

struct IrTraitDef *pawIr_get_trait_def(struct Compiler *C, DeclId did)
{
    return *TraitDefMap_get(C, C->trait_defs, did);
}

IrType *pawIr_get_def_type(struct Compiler *C, DeclId did)
{
    return *DefTypeMap_get(C, C->def_types, did);
}

IrGenericArg pawIr_get_def_arg(struct Compiler *C, DeclId did)
{
    __builtin_trap(); // TODO
}

IrTrait *pawIr_get_trait(struct Compiler *C, DeclId did)
{
    return pawIr_new_trait(C, did, pawIr_get_generic_args(C, did));
}

DEFINE_MAP(struct Compiler, TraitCache, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, void *,)

static IrGenericArgs *replace_self_in_trait_args(struct Compiler *C, IrGenericArgs *args, struct IrGeneric *target)
{
    IrGenericArgs *result = IrGenericArgs_new(C);
    IrGenericArgs_reserve(C, result, args->count);
    K_LIST_XFOREACH (args, IrGenericArg const, arg)
        IrGenericArgs_push(C, result, *arg);

    IrGenericArgs_set(result, 0, IrGenericArg_from_type((IrType *)target));
    return result;
}

// TODO: handle ambiguous method calls (there are multiple trait bounds on a single generic that declare a method with the same name)
IrType *pawIr_resolve_trait_method(struct Compiler *C, struct IrGeneric *target, Str *name)
{
    IrTraitList *bounds = pawIr_get_trait_bounds(C, target->did);

    if (bounds != NULL) {
        IrTypeList *candidates = IrTypeList_new(C);
        IrTraitList *worklist = IrTraitList_new(C);
        IrTraitList_reserve(C, worklist, bounds->count);
        TraitCache *cache = TraitCache_new(C);
        K_LIST_XFOREACH (bounds, IrTrait *const, b) {
            TraitCache_insert_unique(C, cache, (*b)->did, NULL);
            IrTraitList_push(C, worklist, *b);
        }

        while (worklist->count > 0) {
            IrTrait *b = IrTraitList_last(worklist);
            IrTraitList_pop(worklist);
            struct IrTraitDef const *def = pawIr_get_trait_def(C, b->did);
            IrTraitList *supertraits = pawIr_get_trait_bounds(C,
                    IrGenericDefs_first(def->generics)->did);
            if (supertraits != NULL) {
                K_LIST_XFOREACH (supertraits, IrTrait *const, psupertrait) {
                    IrTrait *supertrait = *psupertrait;
                    if (!TraitCache_insert(C, cache, supertrait->did, NULL)) {
                        IrGenericArgs *args = replace_self_in_trait_args(C, supertrait->args, target);
                        IrTraitList_push(C, worklist, pawIr_new_trait(C, supertrait->did, args));
                    }
                }
            }
            K_LIST_XFOREACH (def->methods, IrType *const, m) {
                struct IrFnDef const *fn = pawIr_get_fn_def(C, IR_TYPE_DID(*m));
                if (pawS_eq(fn->name, name)) {
                    IrType *type = pawIr_solver_instantiate_type(C->S, fn->did);
                    IrType *type_ctx = pawIr_get_context(C, type);
                    IrTrait *trait_ctx = pawIr_get_trait_context(C, type);
                    pawIr_unify_traits_unchecked(C, trait_ctx, b);
                    pawU_unify_unchecked(C->U, type_ctx, (IrType *)target);
                    IrTypeList_push(C, candidates, type);
                }
            }
        }

        if (candidates->count > 1)
            THROW_ERROR(C, MultipleApplicableItems,
                    .modname = SCAN_STR(C, ""),
                    .name = name,
                    .span = {0});

        if (candidates->count == 1)
            return IrTypeList_first(candidates);
    }

    return NULL;
}

enum IrDefKind pawIr_get_kind(struct Compiler *C, DeclId did)
{
    return *IrDefKinds_get(C, C->ir_def_kinds, did);
}

IrGenericArgs *pawIr_get_generic_args(struct Compiler *C, DeclId did)
{
    IrGenericArgs *const *p = IrGenericTypes_get(C, C->ir_generic_args, did);
    return p != NULL ? *p : NULL;
}

IrConstraints *pawIr_get_constraints(struct Compiler *C, DeclId did)
{
    IrConstraints *const *p = IrConstraintsMap_get(C, C->ir_constraints, did);
    return p != NULL ? *p : NULL;
}

IrTraitList *pawIr_get_trait_bounds(struct Compiler *C, DeclId did)
{
    IrTraitList *const *p = IrTraitBounds_get(C, C->ir_trait_bounds, did);
    return p != NULL ? *p : NULL;
}

void pawIr_set_generic_args(struct Compiler *C, DeclId did, IrGenericArgs *types)
{
    paw_Bool const exists = IrGenericTypes_insert(C, C->ir_generic_args, did, types);
    paw_assert(!exists); PAW_UNUSED(exists);
}

void pawIr_set_trait_bounds(struct Compiler *C, DeclId did, IrTraitList *traits)
{
    paw_Bool const exists = IrTraitBounds_insert(C, C->ir_trait_bounds, did, traits);
    paw_assert(!exists); PAW_UNUSED(exists);
}


IrType *pawIr_get_context(struct Compiler *C, IrType *fn)
{
    DeclId parent_did;
    enum IrDefKind const fn_kind = pawIr_get_kind(C, IR_TYPE_DID(fn));
    if (fn_kind == IR_FN_DEF) {
        struct IrFnDef const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(fn));
        parent_did = fn_def->parent;
    } else {
        // "fn" is the type of a type constructor
        paw_assert(fn_kind == IR_VARIANT_DEF);
        struct IrVariantDef const *variant_def = pawIr_get_variant_def(C, IR_TYPE_DID(fn));
        struct IrAdtDef const *adt_def = pawIr_get_adt_def(C, variant_def->base_did);
        parent_did = adt_def->did;
    }

    if (!DECL_ID_EXISTS(parent_did))
        return NULL;

    enum IrDefKind const parent_kind = pawIr_get_kind(C, parent_did);
    if (parent_kind == IR_TRAIT_DEF)
        return IrGenericArg_get_type(
                IrGenericArgs_first(IR_GENERIC_ARGS(fn)));

    IrType *parent;
    if (parent_kind == IR_IMPL_DEF) {
        struct IrImpl const *impl_def = pawIr_get_impl_def(C, parent_did);
        parent = impl_def->type;
    } else {
        paw_assert(parent_kind == IR_ADT_DEF);
        parent = pawIr_get_def_type(C, parent_did);
    }

    IrGenericArgs *params = pawIr_get_generic_args(C, IR_TYPE_DID(fn));
    if (params == NULL) return parent;

    IrGenericArgs *args = IR_GENERIC_ARGS(fn);
    struct Substitution const subst = {params, args};
    return pawP_substitute(C, parent, subst);
}

// TODO: wrong, only works in some situations...
IrTrait *pawIr_get_trait_context(struct Compiler *C, IrType *fn)
{
    struct IrFnDef const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(fn));
    if (!DECL_ID_EXISTS(fn_def->parent)) return NULL;
    enum IrDefKind const kind = pawIr_get_kind(C, fn_def->parent);
    if (kind == IR_TRAIT_DEF)
        return pawIr_solver_instantiate_trait_with(C->S,
                fn_def->parent, IR_GENERIC_ARGS(fn));

    paw_assert(kind == IR_IMPL_DEF);
    struct IrImpl const *impl_def = pawIr_get_impl_def(C, fn_def->parent);
    return pawIr_solver_instantiate_impl_with(C->S,
            impl_def->did, IR_GENERIC_ARGS(fn)).trait;
}

static paw_Bool const_equals(union IrValue x, union IrValue y, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrBool:
            return x.b == y.b;
        case kIrChar:
            return x.c == y.c;
        case kIrInt:
            // TODO: either use exact width integer variants corresponding to .ikind or make sure all IrValues are zero'd before writing the value
            return x.i == y.i;
        default:
            paw_assert(IrIsFloat(type));
            return x.f == y.f;
    }
}

static void report_type_var(struct IrTypeVisitor *V, struct IrInfer *t)
{
    PAW_UNUSED(t);
    *((paw_Bool *)V->ud) = PAW_TRUE;
}

static void report_const_var(struct IrTypeVisitor *V, struct IrConst *k)
{
    if (k->kind == IR_CONST_INFER)
        *((paw_Bool *)V->ud) = PAW_TRUE;
}

struct IrTypeVisitor init_ivar_visitor(struct Compiler *C, paw_Bool *flag_ptr)
{
    struct IrTypeVisitor V;
    pawIr_type_visitor_init(&V, C, flag_ptr);
    V.VisitInfer = report_type_var;
    V.VisitConst = report_const_var;
    return V;
}

paw_Bool pawIr_type_contains_inference_var(struct Compiler *C, IrType *type)
{
    paw_Bool found_inference_var = PAW_FALSE;
    struct IrTypeVisitor V = init_ivar_visitor(C, &found_inference_var);
    pawIr_visit_type(&V, type);
    return found_inference_var;
}

paw_Bool pawIr_const_contains_inference_var(struct Compiler *C, IrConst *konst)
{
    paw_Bool found_inference_var = PAW_FALSE;
    struct IrTypeVisitor V = init_ivar_visitor(C, &found_inference_var);
    pawIr_visit_const(&V, konst);
    return found_inference_var;
}

paw_Bool pawIr_trait_contains_inference_var(struct Compiler *C, IrTrait *trait)
{
    paw_Bool found_inference_var = PAW_FALSE;
    struct IrTypeVisitor V = init_ivar_visitor(C, &found_inference_var);
    pawIr_visit_trait(&V, trait);
    return found_inference_var;
}


static paw_Uint hash_type(IrType *type);
static paw_Uint hash_arg(IrGenericArg arg);

static paw_Uint hash_type_list(IrTypeList const *types)
{
    paw_Uint hash = 0;
    if (types != NULL) {
        K_LIST_XFOREACH (types, IrType *const, p)
            hash = hash_combine(hash, hash_type(*p));
    }
    return hash;
}

static paw_Uint hash_arg_list(IrGenericArgs const *args)
{
    paw_Uint hash = 0;
    K_LIST_XFOREACH (args, IrGenericArg const, p)
        hash = hash_combine(hash, hash_arg(*p));
    return hash;
}

static paw_Uint hash_trait(IrTrait const *trait)
{
    paw_Uint hash = 0x42;
    hash = hash_combine(hash, trait->did.value);
    hash = hash_combine(hash, hash_arg_list(trait->args));
    return hash;
}

static paw_Uint hash_const(IrConst const *k)
{
    paw_Uint hash = 0;
    switch (k->kind) {
        case IR_CONST_PENDING:
            hash = hash_combine(hash, P_ID_HASH(NULL, k->pending.did));
            break;
        case IR_CONST_VALUE:
            hash = hash_combine(hash, hash_type(k->value.type));
            hash = hash_combine(hash, (paw_Uint)k->value.value.i);
            break;
        default:
            paw_assert(k->kind == IR_CONST_DECL);
            hash = hash_combine(hash, k->decl.did.value);
    }
    return hash;
}

static paw_Uint hash_type(IrType *type)
{
    paw_Uint hash = type->hdr.kind;
    switch (IR_KINDOF(type)) {
        case kIrUnit:
        case kIrBool:
        case kIrChar:
        case kIrString:
            break;
        case kIrInt: {
            struct IrInt const *t = IrGetInt(type);
            hash = hash_combine(hash, t->ikind);
            break;
        }
        case kIrFloat: {
            struct IrFloat const *t = IrGetFloat(type);
            hash = hash_combine(hash, t->fkind);
            break;
        }
        case kIrPtr: {
            struct IrPtr const *t = IrGetPtr(type);
            hash = hash_combine(hash, hash_type(t->pointee));
            break;
        }
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_arg_list(t->args));
            break;
        }
        case kIrFnPtr: {
            struct IrFnPtr const *t = IrGetFnPtr(type);
            hash = hash_combine(hash, hash_type_list(t->params));
            hash = hash_combine(hash, hash_type(t->result));
            break;
        }
        case kIrClosure: {
            struct IrClosure const *t = IrGetClosure(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_arg_list(t->args));
            break;
        }
        case kIrSignature: {
            struct IrSignature const *t = IrGetSignature(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_arg_list(t->args));
            break;
        }
        case kIrSlice: {
            struct IrSlice const *t = IrGetSlice(type);
            hash = hash_combine(hash, hash_type(t->type));
            break;
        }
        case kIrArray: {
            struct IrArray const *t = IrGetArray(type);
            hash = hash_combine(hash, hash_type(t->type));
            hash = hash_combine(hash, hash_const(t->length));
            break;
        }
        case kIrTuple: {
            struct IrTuple const *t = IrGetTuple(type);
            hash = hash_combine(hash, hash_type_list(t->elems));
            break;
        }
        case kIrGeneric: {
            struct IrGeneric const *t = IrGetGeneric(type);
            hash = hash_combine(hash, t->did.value);
            break;
        }
        case kIrProjection: {
            struct IrProjection const *t = IrGetProjection(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_arg_list(t->args));
            break;
        }
        default:
            paw_assert(IrIsNever(type));
            hash = hash_combine(hash, 0x21); // '!'
            break;
    }
    return hash;
}

static paw_Uint hash_arg(IrGenericArg arg)
{
    return IrGenericArg_is_type(arg)
        ? hash_type(IrGenericArg_get_type(arg))
        : hash_const(IrGenericArg_get_const(arg));
}

static paw_Bool arglist_equals(struct Compiler *C, IrGenericArgs *a, IrGenericArgs *b)
{
    if (a->count != b->count)
        return PAW_FALSE;

    for (int i = 0; i < a->count; ++i) {
        IrGenericArg const ga = IrGenericArgs_get(a, i);
        IrGenericArg const gb = IrGenericArgs_get(b, i);
        if (!pawIr_arg_equals(C, ga, gb))
            return PAW_FALSE;
    }

    return PAW_TRUE;
}

static paw_Bool typelist_equals(struct Compiler *C, IrTypeList *a, IrTypeList *b)
{
    if (a->count != b->count)
        return PAW_FALSE;

    for (int i = 0; i < a->count; ++i) {
        IrType *ta = IrTypeList_get(a, i);
        IrType *tb = IrTypeList_get(b, i);
        if (!pawIr_type_equals(C, ta, tb))
            return PAW_FALSE;
    }

    return PAW_TRUE;
}

static paw_Bool sig_equals_extra(struct Compiler *C, IrType *a, IrType *b)
{
    // distinguish between different function signatures that happen to have the
    // same parameters and result
    struct IrSignature const *sa = IrGetSignature(a);
    struct IrSignature const *sb = IrGetSignature(b);
    if (sa->did.value != sb->did.value) return PAW_FALSE;
    if (!sa->args != !sb->args) { // TODO: should not be necessary, here to patch some bug I need to fix...
        return PAW_TRUE; // TODO: whenever this happens, one has empty .types_ and the other has .types_=NULL
    } // TODO: should not be necessary
    if (sa->args == NULL) return PAW_TRUE;
    paw_assert(sb->args != NULL);
    return arglist_equals(C, sa->args, sb->args);
}

static paw_Bool type_equals(struct Compiler *C, IrType *lhs, IrType *rhs)
{
    if (IR_KINDOF(lhs) != IR_KINDOF(rhs))
        return PAW_FALSE;

    switch (IR_KINDOF(lhs)) {
        case kIrInt:
            return IR_INT_KIND(lhs) == IR_INT_KIND(rhs);
        case kIrFloat:
            return IR_FLOAT_KIND(lhs) == IR_FLOAT_KIND(rhs);
        case kIrPtr:
            return type_equals(C, ir_deref(lhs), ir_deref(rhs));
        case kIrAdt: {
            struct IrAdt const *x = IrGetAdt(lhs);
            struct IrAdt const *y = IrGetAdt(rhs);
            return P_ID_EQUALS(NULL, x->did, y->did)
                && arglist_equals(C, x->args, y->args);
        }
        case kIrFnPtr: {
            struct IrFnPtr const *x = IrGetFnPtr(lhs);
            struct IrFnPtr const *y = IrGetFnPtr(rhs);
            return typelist_equals(C, x->params, y->params)
                && type_equals(C, x->result, y->result);
        }
        case kIrSignature: {
            struct IrSignature const *x = IrGetSignature(lhs);
            struct IrSignature const *y = IrGetSignature(rhs);
            return P_ID_EQUALS(NULL, x->did, y->did)
                && arglist_equals(C, x->args, y->args);
        }
        case kIrSlice: {
            struct IrSlice const *x = IrGetSlice(lhs);
            struct IrSlice const *y = IrGetSlice(rhs);
            return type_equals(C, x->type, y->type);
        }
        case kIrArray: {
            struct IrArray const *x = IrGetArray(lhs);
            struct IrArray const *y = IrGetArray(rhs);
            return type_equals(C, x->type, y->type)
                && pawIr_const_equals(C, x->length, y->length);
        }
        case kIrTuple: {
            struct IrTuple const *x = IrGetTuple(lhs);
            struct IrTuple const *y = IrGetTuple(rhs);
            return typelist_equals(C, x->elems, y->elems);
        }
        case kIrGeneric: {
            struct IrGeneric const *x = IrGetGeneric(lhs);
            struct IrGeneric const *y = IrGetGeneric(rhs);
            return P_ID_EQUALS(C, x->did, y->did);
        }
        case kIrProjection: {
            struct IrProjection const *x = IrGetProjection(lhs);
            struct IrProjection const *y = IrGetProjection(rhs);
            return P_ID_EQUALS(C, x->did, y->did)
                && arglist_equals(C, x->args, y->args);
        }
        case kIrClosure: {
            struct IrClosure const *x = IrGetClosure(lhs);
            struct IrClosure const *y = IrGetClosure(rhs);
            return P_ID_EQUALS(C, x->did, y->did)
                && arglist_equals(C, x->args, y->args) ;
        }
        default:
            return PAW_TRUE;
    }
}

paw_Bool pawIr_type_equals(struct Compiler *C, IrType *a, IrType *b)
{
    return type_equals(C, a, b);
}

paw_Uint pawIr_type_hash(struct Compiler *C, IrType *t)
{
    PAW_UNUSED(C);
    return hash_type(t);
}

paw_Uint pawIr_type2_hash(struct Compiler *C, struct IrType2 t)
{
    paw_Uint const first = pawIr_type_hash(C, t.first);
    paw_Uint const second = pawIr_type_hash(C, t.second);
    return hash_combine(first, second);
}

paw_Bool pawIr_type2_equals(struct Compiler *C, struct IrType2 a, struct IrType2 b)
{
    return pawIr_type_equals(C, a.first, b.first)
        && pawIr_type_equals(C, a.second, b.second);
}

paw_Uint pawIr_const_hash(struct Compiler *C, IrConst const *k)
{
    PAW_UNUSED(C);
    return hash_const(k);
}

paw_Bool pawIr_const_equals(struct Compiler *C, IrConst const *a, IrConst const *b)
{
    if (a->kind != b->kind)
        return PAW_FALSE;

    switch (a->kind) {
        case IR_CONST_PENDING:
            return P_ID_EQUALS(C, a->pending.did, b->pending.did);
        case IR_CONST_VALUE:
            return pawIr_type_equals(C, a->value.type, b->value.type)
                && a->value.value.i == b->value.value.i;
        default:
            paw_assert(a->kind == IR_CONST_DECL);
            return P_ID_EQUALS(C, a->decl.did, b->decl.did);
    }
}

paw_Uint pawIr_trait_hash(struct Compiler *C, IrTrait *trait)
{
    PAW_UNUSED(C);
    return hash_trait(trait);
}

paw_Uint pawIr_arg_hash(struct Compiler *C, IrGenericArg arg)
{
    PAW_UNUSED(C);
    return hash_arg(arg);
}

paw_Bool pawIr_arg_equals(struct Compiler *C, IrGenericArg a, IrGenericArg b)
{
    if (IrGenericArg_is_type(a) != IrGenericArg_is_type(b))
        return PAW_FALSE;

    return IrGenericArg_is_type(a)
        ? pawIr_type_equals(C, IrGenericArg_get_type(a), IrGenericArg_get_type(b))
        : pawIr_const_equals(C, IrGenericArg_get_const(a), IrGenericArg_get_const(b));
}

IrType *pawIr_get_custom_drop_type(struct Compiler *C, IrType *type)
{
    DeclId const drop_did = C->core_traits[CORE_TRAIT_DROP];
    IrDefs const *trait_defs = pawIr_trait_impls_for(C, type);
    K_LIST_XFOREACH (trait_defs, DeclId const, p) {
        struct IrImpl const *def = pawIr_get_impl_def(C, *p);
        if (P_ID_EQUALS(NULL, def->trait->did, drop_did)) {
            IrSolver *S = pawIr_push_solver(C);
            int const position = pawU_current_position(C->U);
            struct IrImplInstance const inst = pawIr_solver_instantiate_impl(S, *p);
            pawIr_solver_add_obligations_from(S, *p, inst.args, (struct IrObligationCause){0});
            paw_Bool const matches = P_ID_EQUALS(NULL, inst.trait->did, drop_did)
                && pawU_unify(C->U, inst.type, type) == 0
                && pawIr_solver_solve_all(S);
            pawIr_pop_solver(C);

            if (matches) {
                IrType *method = IrTypeList_first(def->methods);
                if (def->generics->count == 0) return method;
                // `Drop::drop()` has no generic args of its own, meaning `inst.args`
                // can be used directly
                return pawIr_solver_instantiate_type_with(C->S,
                        IR_TYPE_DID(method), inst.args);
            }
            pawU_undo_unifications(C->U, position);
        }
    }
    return NULL;
}


static paw_Bool any_needs_drop(struct Compiler *C, IrTypeList *types)
{
    K_LIST_XFOREACH (types, IrType *const, p) {
        if (pawIr_needs_drop(C, *p)) return PAW_TRUE;
    }
    return PAW_FALSE;
}


static DeclId next_did(struct Compiler *C)
{
    return (DeclId){
        .modno = (unsigned)0, // TODO
        .value = (unsigned)++C->decl_count,
    };
}

IrType *pawIr_materialize_drop_type(struct Compiler *C, IrType *type)
{
    IrTypeList *methods = IrTypeList_new(C);
    DeclId const drop_did = C->core_traits[CORE_TRAIT_DROP];
    IrTrait *trait = pawIr_new_trait(C, drop_did, IrGenericArgs_new(C));
    struct IrImpl *impl = pawIr_new_impl(C, next_did(C), type, trait,
            IrGenericDefs_new(C), methods, IrAssocItems_new(C));
    ImplMap_insert(C, C->impl_defs, impl->did, impl);
    IrConstraintsMap_insert(C, C->ir_constraints, impl->did, IrConstraints_new(C));
    DefTypeMap_insert(C, C->def_types, impl->did, type);

    {
        IrDefs *trait_defs = pawIr_trait_impls_for(C, type);
        IrDefs_push(C, trait_defs, impl->did);
    }

    IrParams *params = IrParams_new(C);
    IrParams_push(C, params, (struct IrParam){
                .type = pawIr_new_ptr(C, type),
                .name = SCAN_STR(C, "self"),
            });
    struct IrFnDef *fn = pawIr_new_fn_def(C, next_did(C),
            SCAN_STR(C, "drop"), IrGenericDefs_new(C),
            pawIr_new_unit(C), params, type, impl->did,
            PAW_TRUE);
    FnDefMap_insert(C, C->fn_defs, fn->did, fn);
    IrGenericArgs *args = IrGenericArgs_new(C);
    IrGenericTypes_insert(C, C->ir_generic_args, fn->did, args);
    IrConstraintsMap_insert(C, C->ir_constraints, fn->did, IrConstraints_new(C));
    IrType *drop = pawIr_new_signature(C, fn->did, args);
    DefTypeMap_insert(C, C->def_types, fn->did, drop);
    IrTypeList_push(C, methods, drop);
    return drop;
}

static paw_Bool has_field_with_drop(struct Compiler *C, IrType *type)
{
    if (IrIsTuple(type)) {
        struct IrTuple const *t = IrGetTuple(type);
        return any_needs_drop(C, t->elems);
    } else if (IrIsAdt(type)) {
        struct IrAdt *t = IrGetAdt(type);
        struct IrAdtDef const *def = pawIr_get_adt_def(C, t->did);
        if (def->is_struct) {
            IrTypeList *fields = pawP_instantiate_struct_fields(C, t);
            return any_needs_drop(C, fields);
        } else {
            K_LIST_XFOREACH (def->variants, struct IrVariantDef *const, v) {
                IrTypeList *fields = pawP_instantiate_variant_fields(C, t, (*v)->discr);
                if (any_needs_drop(C, fields)) return PAW_TRUE;
            }
        }
    } else if (IrIsArray(type)) {
        return pawIr_needs_drop(C, IrGetArray(type)->type);
    }
    return PAW_FALSE;
}

IrType *pawIr_get_drop_type(struct Compiler *C, IrType *type)
{
    IrType *drop = pawIr_get_custom_drop_type(C, type);
    if (drop == NULL && has_field_with_drop(C, type))
        return pawIr_materialize_drop_type(C, type);
    return drop;
}

paw_Bool pawIr_needs_drop(struct Compiler *C, IrType *type)
{
    if (IrIsGeneric(type)) return PAW_TRUE;
    IrType *drop = pawIr_get_custom_drop_type(C, type);
    if (drop != NULL) return PAW_TRUE;

    if (IrIsTuple(type)) {
        struct IrTuple const *t = IrGetTuple(type);
        return any_needs_drop(C, t->elems);
    } else if (IrIsArray(type)) {
        struct IrArray const *t = IrGetArray(type);
        return pawIr_needs_drop(C, t->type);
    } else if (IrIsAdt(type)) {
        struct IrAdt *t = IrGetAdt(type);
        struct IrAdtDef const *def = pawIr_get_adt_def(C, t->did);
        {
            // special case: Drop is not generated for ManuallyDrop instances
            struct Builtin const *b = pawP_builtin_info(C, BUILTIN_MANUALLY_DROP);
            if (P_ID_EQUALS(NULL, def->did, b->did)) return PAW_FALSE;
        }
        if (def->is_struct) {
            IrTypeList *fields = pawP_instantiate_struct_fields(C, t);
            return any_needs_drop(C, fields);
        } else {
            K_LIST_XFOREACH (def->variants, struct IrVariantDef *const, v) {
                IrTypeList *fields = pawP_instantiate_variant_fields(C, t, (*v)->discr);
                if (any_needs_drop(C, fields)) return PAW_TRUE;
            }
            return PAW_FALSE;
        }
    }
    return PAW_FALSE;
}

// TODO: this is wrong because of integer/float inference vars
paw_Bool pawIr_is_copyable(struct Compiler *C, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrUnit:
        case kIrBool:
        case kIrChar:
        case kIrInt:
        case kIrFloat:
        case kIrString:
        case kIrSlice:
        case kIrPtr:
            return PAW_TRUE;
        default:
            break;
    }
    if (IrIsTuple(type)) {
        // a tuple is copyable if all of its elements are copyable
        struct IrTuple const *t = IrGetTuple(type);
        paw_Bool accum = PAW_TRUE;
        K_LIST_XFOREACH (t->elems, IrType *const, p)
            accum &= pawIr_is_copyable(C, *p);
        return accum;
    }

    if (IrIsArray(type))
        return pawIr_is_copyable(C, IrGetArray(type)->type);

    if (IR_IS_FUNC_TYPE(type)) {
        if (ir_is_capturing_closure(C, type)) {
            UpvalueList const *upvalues = *UpvalueTable_get(C, C->upvtab, IR_TYPE_DID(type));
            K_LIST_XFOREACH (upvalues, struct UpvalueInfo const, u) {
                IrType *upvalue = pawIr_get_type(C, u->id);
                if (!pawIr_is_copyable(C, upvalue))
                    return PAW_FALSE;
            }
        }
        return PAW_TRUE;
    }

    IrSolver *S = pawIr_push_solver(C);
    if (IrIsGeneric(type)) {
        // Add predicates on the generic (from the binder that declares this generic type arg).
        // This is necessary because this function may be called from contexts that no longer
        // have knowledge about the predicates involving this generic.
        IrTraitList const *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(type));
        if (bounds != NULL) {
            K_LIST_XFOREACH (bounds, IrTrait *const, t)
                pawIr_solver_add_predicate(S, type, *t, (struct IrObligationCause){0});
        }
    }
    int const position = pawU_current_position(C->U);
    DeclId const copy_did = C->core_traits[CORE_TRAIT_COPY];
    IrGenericArgs *copy_args = IrGenericArgs_new(C);
    IrGenericArgs_push(C, copy_args, IrGenericArg_from_type(type));
    IrTrait *copy = pawIr_solver_instantiate_trait_with(S, copy_did, copy_args);
    pawIr_solver_add_impl_trait_obligation(S, type, copy,
            (struct IrObligationCause){0});
    struct IrSolverResult const r = pawIr_solver_solve(S);
    pawU_undo_unifications(C->U, position);
    pawIr_pop_solver(C);
    return r.status == IR_SOLVER_SOLVED;
}

IrType *pawIr_materialize_fn(struct Compiler *C, DeclId did, IrGenericArgs *type_args)
{
    IrType *result;
    IrTypeList *params = IrTypeList_new(C);
    enum IrDefKind const kind = pawIr_get_kind(C, did);
    if (kind == IR_VARIANT_DEF) {
        struct IrVariantDef const *def = pawIr_get_variant_def(C, did);
        IrTypeList_reserve(C, params, def->fields->count);
        K_LIST_XFOREACH (def->fields, struct IrFieldDef *const, p) {
            IrType *field = pawIr_get_def_type(C, (*p)->did);
            IrTypeList_push(C, params, field);
        }
        result = pawIr_get_def_type(C, def->base_did);
    } else {
        paw_assert(kind == IR_FN_DEF);
        struct IrFnDef const *def = pawIr_get_fn_def(C, did);
        IrTypeList_reserve(C, params, def->params->count);
        K_LIST_XFOREACH (def->params, struct IrParam const, p)
            IrTypeList_push(C, params, p->type);
        result = def->result;
    }

    IrGenericArgs *type_params = pawIr_get_generic_args(C, did);
    struct Substitution const subst = {type_params, type_args};

    K_LIST_XFOREACH (params, IrType *, p)
        *p = pawP_substitute(C, *p, subst);
    result = pawP_substitute(C, result, subst);

    return pawU_normalize_projections(C->U,
            pawIr_new_fn_ptr(C, params, result));
}

IrTrait *pawIr_get_projection_trait(struct Compiler *C, struct IrProjection const *t)
{
    struct IrAssocItem const *item = pawIr_get_assoc_item(C, t->did);
    return pawIr_new_trait(C, item->parent, t->args);
}

// Convert all type args into type `!` and all const args to their zero values
static IrGenericArgs *cannonicalize_args(struct Compiler *C, IrGenericArgs const *args)
{
    IrGenericArgs *result = IrGenericArgs_new(C);
    IrGenericArgs_reserve(C, result, args->count);
    K_LIST_XFOREACH (args, IrGenericArg const, p) {
        IrGenericArg r;
        if (IrGenericArg_is_type(*p)) {
            r = IrGenericArg_from_type(
                    pawIr_new_never(C));
        } else {
            IrConst *k = IrGenericArg_get_const(*p);
            IrType *const_type;
            if (k->kind == IR_CONST_DECL) {
                const_type = pawIr_get_def_type(C, k->decl.did);
            } else if (k->kind == IR_CONST_INFER) {
                PAW_UNREACHABLE();
            } else if (k->kind == IR_CONST_PENDING) {
                PAW_UNREACHABLE();
            } else {
                paw_assert(k->kind == IR_CONST_VALUE);
                const_type = k->value.type;
            }
            union IrValue const zero_value = IrIsString(const_type)
                ? (union IrValue){.s = SCAN_STR(C, "")}
                : (union IrValue){.u64 = 0};
            r = IrGenericArg_from_const(
                    pawIr_new_const_value(C, zero_value, const_type));
        }
        IrGenericArgs_push(C, result, r);
    }
    return result;
}

static IrType *cannonicalize_type(struct Compiler *C, IrType *type)
{
    type = pawIr_remove_indirection(C, type);
    switch (IR_KINDOF(type)) {
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            IrGenericArgs const *params = pawIr_get_generic_args(C, t->did);
            IrGenericArgs *args = cannonicalize_args(C, params);
            return pawIr_new_adt(C, t->did, args);
        }
        case kIrSignature: {
            struct IrSignature const *t = IrGetSignature(type);
            IrGenericArgs const *params = pawIr_get_generic_args(C, t->did);
            IrGenericArgs *args = cannonicalize_args(C, params);
            return pawIr_new_signature(C, t->did, args);
        }
        case kIrProjection: {
            struct IrProjection const *t = IrGetProjection(type);
            // TODO: may need to pass `Self ++ Trait.params[1..]` instead of `t->args`. when `type` is `<T as Trait<123>>::Item` the integer inference var messes us up, need the concrete type from def of `Trait`
            IrGenericArgs *args = cannonicalize_args(C, t->args);
            return pawIr_new_projection(C, t->did, args);
        }
        case kIrTuple: {
            struct IrTuple const *t = IrGetTuple(type);
            IrTypeList *fields = IrTypeList_new(C);
            IrTypeList_reserve(C, fields, t->elems->count);
            while (fields->count < t->elems->count)
                IrTypeList_push(C, fields, pawIr_new_never(C));
            return pawIr_new_tuple(C, fields);
        }
        case kIrArray:
            return pawIr_new_array(C, pawIr_new_never(C),
                    pawIr_new_const_value(C,
                        (union IrValue){.u64 = 0},
                        pawIr_new_int(C, IR_USIZE)));
        case kIrInfer:
        case kIrGeneric:
            return pawIr_new_never(C);
        case kIrSlice:
            return pawIr_new_slice(C, pawIr_new_never(C));
        case kIrPtr:
            return pawIr_new_ptr(C, pawIr_new_never(C));
        default:
            return type;
    }
}

static IrDefs *get_or_create_candidates_for(struct Compiler *C, IrType *self, IrTypeMap *impls)
{
    self = cannonicalize_type(C, self);
    void *const *p = IrTypeMap_get(C, impls, self);
    if (p == NULL) {
        IrDefs *defs = IrDefs_new(C);
        IrTypeMap_insert(C, impls, self, defs);
        return defs;
    }
    return *p;
}

IrDefs *pawIr_inherent_impls_for(struct Compiler *C, IrType *self)
{
    return get_or_create_candidates_for(C, self, C->impls.inherent);
}

IrDefs *pawIr_trait_impls_for(struct Compiler *C, IrType *self)
{
    return get_or_create_candidates_for(C, self, C->impls.trait);
}


struct Printer {
    struct Compiler *C;
    Buffer *buf;
    paw_Env *P;
    int indent;
    paw_Bool print_bounds;
};

#define PRINT_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define PRINT_STRING(P, str) pawL_add_nstring(ENV(P), (P)->buf, (str)->text, (str)->length)
#define PRINT_FORMAT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define PRINT_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)

static void print_type(struct Printer *, IrType *);
static void print_const(struct Printer *, IrConst *);
static void print_type_list(struct Printer *P, IrTypeList *list)
{
    for (int i = 0; i < list->count; ++i) {
        print_type(P, list->data[i]);
        if (i < list->count - 1)
            PRINT_LITERAL(P, ", ");
    }
}

static void print_bounds(struct Printer *P, IrTypeList *bounds)
{
    if (bounds != NULL) {
        PRINT_LITERAL(P, ": ");
        int index;
        IrType *const *ptype;
        K_LIST_ENUMERATE (bounds, index, ptype) {
            if (index > 0)
                PRINT_LITERAL(P, " + ");
            print_type(P, *ptype);
        }
    }
}

static void print_generic_arg(struct Printer *P, IrGenericArg arg)
{
    if (IrGenericArg_is_type(arg)) {
        print_type(P, IrGenericArg_get_type(arg));
    } else {
        print_const(P, IrGenericArg_get_const(arg));
    }
}

static void print_binder(struct Printer *P, IrGenericArgs *binder)
{
    // TODO: be consistent semantics: does `.binder = NULL` or `.binder.count = 0` mean monomorphic
    if (binder->count > 0) {
        P->print_bounds = PAW_TRUE;
        PRINT_CHAR(P, '<');
        for (int i = 0; i < binder->count; ++i) {
            if (i > 0) PRINT_LITERAL(P, ", ");
            print_generic_arg(P, IrGenericArgs_get(binder, i));
        }
        PRINT_CHAR(P, '>');
        P->print_bounds = PAW_FALSE;
    }
}

static void print_trait_omitting_self(struct Printer *P, IrTrait *t)
{
    paw_assert(t->args->count > 0);
    struct IrTraitDef const *def = pawIr_get_trait_def(P->C, t->did);
    PRINT_STRING(P, def->name);

    P->print_bounds = PAW_TRUE;
    if (t->args->count > 1) {
        PRINT_CHAR(P, '<');
        for (int i = 1; i < t->args->count; ++i) {
            if (i > 1) PRINT_LITERAL(P, ", ");
            print_generic_arg(P, IrGenericArgs_get(t->args, i));
        }
        PRINT_CHAR(P, '>');
    }
    P->print_bounds = PAW_FALSE;
}

static void print_trait(struct Printer *P, IrTrait *t)
{
    // TODO
    print_trait_omitting_self(P, t);
    return;

    paw_assert(t->args->count > 0);
    struct IrTraitDef const *def = pawIr_get_trait_def(P->C, t->did);
    PRINT_STRING(P, def->name);

    P->print_bounds = PAW_TRUE;

    paw_Bool printed_self = PAW_FALSE;
    IrType *current = IrGenericArg_get_type(IrGenericArgs_first(t->args));
    DeclId const base_did = IrGenericDefs_first(def->generics)->did;
    if (!IrIsGeneric(current) || P_ID_EQUALS(NULL, IR_TYPE_DID(current), base_did)) {
        PRINT_CHAR(P, '<');
        PRINT_LITERAL(P, "Self = ");
        print_type(P, current);
        printed_self = PAW_TRUE;
    }
    if (t->args->count > 1) {
        if (printed_self) {
            PRINT_LITERAL(P, ", ");
        } else {
            PRINT_CHAR(P, '<');
        }
        for (int i = 1; i < t->args->count; ++i) {
            if (i > 1) PRINT_LITERAL(P, ", ");
            print_generic_arg(P, IrGenericArgs_get(t->args, i));
        }
    }
    if (t->args->count > 1 || printed_self)
        PRINT_CHAR(P, '>');
    P->print_bounds = PAW_FALSE;
}


static void print_type(struct Printer *P, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrUnit:
            PRINT_LITERAL(P, "()");
            break;
        case kIrBool:
            PRINT_LITERAL(P, "bool");
            break;
        case kIrChar:
            PRINT_LITERAL(P, "char");
            break;
        case kIrInt:
            switch (IR_INT_KIND(type)) {
                case IR_INT8:
                    PRINT_LITERAL(P, "int8");
                    break;
                case IR_INT16:
                    PRINT_LITERAL(P, "int16");
                    break;
                case IR_INT32:
                    PRINT_LITERAL(P, "int32");
                    break;
                case IR_INT64:
                    PRINT_LITERAL(P, "int64");
                    break;
                case IR_ISIZE:
                    PRINT_LITERAL(P, "isize");
                    break;
                case IR_UINT8:
                    PRINT_LITERAL(P, "uint8");
                    break;
                case IR_UINT16:
                    PRINT_LITERAL(P, "uint16");
                    break;
                case IR_UINT32:
                    PRINT_LITERAL(P, "uint32");
                    break;
                case IR_UINT64:
                    PRINT_LITERAL(P, "uint64");
                    break;
                case IR_USIZE:
                    PRINT_LITERAL(P, "usize");
                    break;
            }
            break;
        case kIrFloat:
            switch (IR_FLOAT_KIND(type)) {
                case IR_FLOAT32:
                    PRINT_LITERAL(P, "float32");
                    break;
                case IR_FLOAT64:
                    PRINT_LITERAL(P, "float64");
                    break;
            }
            break;
        case kIrString:
            PRINT_LITERAL(P, "str");
            break;
        case kIrPtr: {
            struct IrPtr *ptr = IrGetPtr(type);
            PRINT_CHAR(P, '*');
            print_type(P, ptr->pointee);
            break;
        }
        case kIrSlice: {
            struct IrSlice *t = IrGetSlice(type);
            PRINT_CHAR(P, '[');
            PRINT_CHAR(P, ']');
            print_type(P, t->type);
            break;
        }
        case kIrArray: {
            struct IrArray *arr = IrGetArray(type);
            PRINT_CHAR(P, '[');
            print_const(P, arr->length);
            PRINT_CHAR(P, ']');
            print_type(P, arr->type);
            break;
        }
        case kIrTuple: {
            struct IrTuple *tup = IrGetTuple(type);
            PRINT_CHAR(P, '(');
            print_type_list(P, tup->elems);
            if (tup->elems->count == 1)
                PRINT_CHAR(P, ',');
            PRINT_CHAR(P, ')');
            break;
        }
        case kIrClosure: {
            struct IrClosure *t = IrGetClosure(type);
            PRINT_FORMAT(P, "$closure_%d", t->did.value);
            print_binder(P, t->args);
            IrType *fn = pawIr_materialize_fn(P->C, t->did, t->args);
            PRINT_FORMAT(P, "{%s}", pawIr_print_type(P->C, fn));
            break;
        }
        case kIrSignature: {
            struct IrSignature *fsig = IrGetSignature(type);
            enum IrDefKind const kind = pawIr_get_kind(P->C, fsig->did);
            if (kind == IR_FN_DEF) {
                struct IrFnDef *def = pawIr_get_fn_def(P->C, fsig->did);
                PRINT_STRING(P, def->name);
                print_binder(P, fsig->args);
                IrType *fn = pawIr_materialize_fn(P->C, fsig->did, fsig->args);
                PRINT_FORMAT(P, "{%s}", pawIr_print_type(P->C, fn));
            } else {
                paw_assert(kind == IR_VARIANT_DEF);
                IrType *parent = pawIr_get_context(P->C, type);
                print_type(P, parent);
                PRINT_LITERAL(P, "::");
                struct IrVariantDef const *variant = pawIr_get_variant_def(P->C, fsig->did);
                PRINT_STRING(P, variant->name);
                struct IrFnPtr const *fn = IrGetFnPtr(IR_GET_FN(P->C, type));
                if (fn->params->count > 0) {
                    PRINT_LITERAL(P, "(");
                    print_type_list(P, fn->params);
                    PRINT_LITERAL(P, ")");
                }
            }
            break;
        }
        case kIrFnPtr: {
            struct IrFnPtr *fptr = IrGetFnPtr(type);
            PRINT_LITERAL(P, "fn(");
            print_type_list(P, fptr->params);
            PRINT_CHAR(P, ')');
            if (!IrIsUnit(fptr->result)) {
                PRINT_LITERAL(P, " -> ");
                print_type(P, fptr->result);
            }
            break;
        }
        case kIrGeneric: {
            struct IrGeneric *t = IrGetGeneric(type);
            struct IrGenericDef const *def = pawIr_get_generic_def(P->C, t->did);
            if (def->is_type) {
                PRINT_STRING(P, def->type.name);
            } else {
                PRINT_STRING(P, def->konst.name);
            }
            break;
        }
        case kIrProjection: {
            struct IrProjection const *t = IrGetProjection(type);
            struct IrAssocItem const *item = pawIr_get_assoc_item(P->C, t->did);
            IrType *self = IrGenericArg_get_type(IrGenericArgs_first(t->args));
            IrTrait *trait = pawIr_new_trait(P->C, item->parent, t->args);
            PRINT_CHAR(P, '<');
            print_type(P, self);
            PRINT_LITERAL(P, " as ");
            print_trait_omitting_self(P, trait);
            PRINT_LITERAL(P, ">::");
            PRINT_STRING(P, item->name);
            break;
        }
        case kIrInfer: {
            struct IrInfer const *t = IrGetInfer(type);
            if (t->ikind == IR_INFER_INTEGER) {
                PRINT_LITERAL(P, "(integer)");
            } else if (t->ikind == IR_INFER_FLOAT) {
                PRINT_LITERAL(P, "(float)");
            } else {
                PRINT_CHAR(P, '_');
            }
            break;
        }
        case kIrNever:
            PRINT_CHAR(P, '!');
            break;
        case kIrAdt: {
            struct IrAdt *adt = IrGetAdt(type);
            struct IrAdtDef *def = pawIr_get_adt_def(P->C, adt->did);
            PRINT_STRING(P, def->name);
            print_binder(P, adt->args);
            break;
        }
    }
}

static void print_const(struct Printer *P, IrConst *konst)
{
    switch (konst->kind) {
        case IR_CONST_VALUE:
            if (IrIsUnit(konst->value.type)) {
                PRINT_LITERAL(P, "()");
            } else if (IrIsBool(konst->value.type)) {
                if (konst->value.value.b) {
                    PRINT_LITERAL(P, "true");
                } else {
                    PRINT_LITERAL(P, "false");
                }
            } else if (IrIsChar(konst->value.type)) {
                PRINT_CHAR(P, konst->value.value.c);
            } else if (IrIsInt(konst->value.type)) {
                pawL_add_int(ENV(P), P->buf, konst->value.value.i);
            } else if (IrIsFloat(konst->value.type)) {
                pawL_add_float(ENV(P), P->buf, konst->value.value.f);
            } else if (IrIsString(konst->value.type)) {
                L_ADD_STRING(ENV(P), P->buf, konst->value.value.s);
            } else {
                paw_assert(IrIsInfer(konst->value.type));
                if (IrGetInfer(konst->value.type)->ikind == IR_INFER_INTEGER) {
                    pawL_add_int(ENV(P), P->buf, konst->value.value.i);
                } else {
                    paw_assert(IrGetInfer(konst->value.type)->ikind == IR_INFER_FLOAT);
                    pawL_add_float(ENV(P), P->buf, konst->value.value.f);
                }
            }
            break;
        case IR_CONST_PENDING:
            PRINT_LITERAL(P, "<unevaluated>");
            break;
        case IR_CONST_DECL: {
            struct IrGenericDef const *def = pawIr_get_generic_def(P->C, konst->decl.did);
            paw_assert(!def->is_type);
            PRINT_STRING(P, def->konst.name);
            break;
        }
        case IR_CONST_INFER:
            PRINT_LITERAL(P, "_");
            break;
    }
}

char const *pawIr_print_type(struct Compiler *C, IrType *type)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_type(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               type);

    Str const *s = pawL_buffer_finish(P, &buf);
    return s->text;
}

char const *pawIr_print_trait(struct Compiler *C, IrTrait *trait)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_trait(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               trait);

    Str const *s = pawL_buffer_finish(P, &buf);
    return s->text;
}

char const *pawIr_print_impl_trait_obligation(struct Compiler *C, IrType *type, IrTrait *trait)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    struct Printer p = {
        .P = ENV(C),
        .buf = &buf,
        .C = C,
    };
    print_type(&p, type);
    PRINT_LITERAL(&p, ": ");
    print_trait(&p, trait);

    Str const *s = pawL_buffer_finish(P, &buf);
    return s->text;
}

Str const *pawIr_print_type_v2(struct Compiler *C, IrType *type)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_type(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               type);

    return pawL_buffer_finish(P, &buf);
}

Str const *pawIr_print_const(struct Compiler *C, IrConst *konst)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_const(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               konst);

    return pawL_buffer_finish(P, &buf);
}

Str const *pawIr_print_trait_v2(struct Compiler *C, IrTrait *trait)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_trait(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               trait);

    return pawL_buffer_finish(P, &buf);
}

Str const *pawIr_print_impl_trait_obligation_v2(struct Compiler *C, IrType *type, IrTrait *trait)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    struct Printer p = {
        .P = ENV(C),
        .buf = &buf,
        .C = C,
    };
    print_type(&p, type);
    PRINT_LITERAL(&p, ": ");
    print_trait(&p, trait);

    return pawL_buffer_finish(P, &buf);
}
