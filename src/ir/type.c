// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "type.h"

#include "common/map.h"
#include "compiler/error.h"
#include "compiler/type_folder.h"
#include "sema/solve.h"
#include "sema/resolver.h"
#include "sema/trait.h"
#include "sema/unify.h"

#define NEW_NODE(C, T) (T *)compiler_alloc(C, NULL, 0, sizeof(T))

#define SPAN_UNUSED (struct SourceSpan){0}

static paw_Bool is_unsized_type(IrType *type)
{
    return 0;
    return IrIsSlice(type)
        || IrIsString(type);
}

paw_Bool ir_is_unsized_type(struct Compiler *C, IrType *type)
{
    return 0;
    if (!IrIsAdt(type))
        return IrIsSlice(type) || IrIsString(type);

    paw_Bool is_unsized = PAW_FALSE;
    struct IrAdt *t = IrGetAdt(type);
    struct IrAdtDef const *def = ir_get_adt_def(C, t->did);
    for (int discr = 0; discr < def->variants->count; ++discr) {
        IrTypeList *fields = instantiate_variant_fields(C, t, discr);
        paw_Bool is_unsized_variant = PAW_FALSE;
        LIST_XFOREACH (fields, IrType *const, p) {
            if (ir_is_unsized_type(C, *p)) {
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

IrType *ir_remove_indirection(struct Compiler *C, IrType *type)
{
    while (IrIsPtr(type)) {
        IrType *pointee = IrGetPtr(type)->pointee;
        if (ir_is_unsized_type(C, pointee)) break;
        type = pointee;
    }
    return type;
}

static void add_deref_chain(struct Compiler *C, IrType *type, IrTypeList *chain)
{
    IrTypeList_push(chain, type);
    while (IrIsPtr(type)) {
        type = ir_deref(type);
        IrTypeList_push(chain, type);
    }
}

IrTypeList *ir_autoptr_chain(struct Compiler *C, IrType *type)
{
    IrTypeList *chain = IrTypeList_new(ALLOCATOR_OF(C));
    IrTypeList_push(chain, type);
    if (IrIsPtr(type)) {
        add_deref_chain(C, type, chain);
    } else {
        IrTypeList_push(chain, ir_new_ptr(C, type));
    }
    return chain;
}

static DeclId get_method_with_name(struct Compiler *C, IrTypeList *methods, Str const *name)
{
    LIST_XFOREACH (methods, IrType *const, p) {
        struct IrFnDef const *def = ir_get_fn_def(C, IR_TYPE_DID(*p));
        if (pawS_eq(def->name, name)) return IR_TYPE_DID(*p);
    }
    return INVALID_DECL_ID;
}

DeclId ir_get_method_from_parent(struct Compiler *C, DeclId parent_did, Str const *name)
{
    enum IrDefKind const parent_kind = ir_get_kind(C, parent_did);
    if (parent_kind == IR_TRAIT_DEF) {
        struct IrTraitDef const *trait_def = ir_get_trait_def(C, parent_did);
        return get_method_with_name(C, trait_def->methods, name);
    } else {
        paw_assert(parent_kind == IR_IMPL_DEF);
        struct IrImpl const *impl_def = ir_get_impl_def(C, parent_did);
        return get_method_with_name(C, impl_def->methods, name);
    }
}

void ir_set_def_kind(struct Compiler *C, DeclId did, enum IrDefKind kind)
{
    paw_assert(DECL_ID_EXISTS(did));

    IrDefKinds_insert(C->ir_def_kinds, did, kind);
}

IrTrait *ir_new_trait(struct Compiler *C, DeclId did, IrGenericArgs *args)
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

IrType *ir_new_unit(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Unit_ = (struct IrUnit){
        .kind = kIrUnit,
    };
    return t;
}

IrType *ir_new_bool(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Bool_ = (struct IrBool){
        .kind = kIrBool,
    };
    return t;
}

IrType *ir_new_char(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Char_ = (struct IrChar){
        .kind = kIrChar,
    };
    return t;
}

IrType *ir_new_int(struct Compiler *C, enum IrIntKind ikind)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Int_ = (struct IrInt){
        .kind = kIrInt,
        .ikind = ikind,
    };
    return t;
}

IrType *ir_new_float(struct Compiler *C, enum IrFloatKind fkind)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Float_ = (struct IrFloat){
        .kind = kIrFloat,
        .fkind = fkind,
    };
    return t;
}

IrType *ir_new_string(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->String_ = (struct IrString){
        .kind = kIrString,
    };
    return t;
}

IrType *ir_new_ptr(struct Compiler *C, IrType *pointee)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Ptr_ = (struct IrPtr){
        .kind = kIrPtr,
        .pointee = pointee,
    };
    return t;
}

IrType *ir_new_adt(struct Compiler *C, DeclId did, IrGenericArgs *args)
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

IrType *ir_new_fn_ptr(struct Compiler *C, IrTypeList *params, IrType *result)
{
    IrType *t = NEW_NODE(C, IrType);
    t->FnPtr_ = (struct IrFnPtr){
        .kind = kIrFnPtr,
        .params = params,
        .result = result,
    };
    return t;
}

IrType *ir_new_closure(struct Compiler *C, DeclId did, IrGenericArgs *args)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Closure_ = (struct IrClosure){
        .kind = kIrClosure,
        .did = did,
        .args = args,
    };
    return t;
}

IrType *ir_new_signature(struct Compiler *C, DeclId did, IrGenericArgs *args)
{
    paw_assert(DECL_ID_EXISTS(did) && args != NULL);
    IrType *t = NEW_NODE(C, IrType);
    t->Signature_ = (struct IrSignature){
        .kind = kIrSignature,
        .did = did,
        .args = args,
    };
    return t;
}

IrType *ir_new_slice(struct Compiler *C, IrType *type)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Slice_ = (struct IrSlice){
        .kind = kIrSlice,
        .type = type,
    };
    return t;
}

IrType *ir_new_tuple(struct Compiler *C, IrTypeList *elems)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Tuple_ = (struct IrTuple){
        .kind = kIrTuple,
        .elems = elems,
    };
    return t;
}

IrType *ir_new_array(struct Compiler *C, IrType *type, IrConst *length)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Array_ = (struct IrArray){
        .kind = kIrArray,
        .type = type,
        .length = length,
    };
    return t;
}

IrConst *ir_new_const_value(struct Compiler *C, union IrValue value, IrType *type)
{
    IrConst *k = NEW_NODE(C, IrConst);
    *k = (struct IrConst){
        .kind = IR_CONST_VALUE,
        .value.value = value,
        .value.type = type,
    };
    return k;
}

IrConst *ir_new_const_pending(struct Compiler *C, DeclId did)
{
    IrConst *k = NEW_NODE(C, IrConst);
    *k = (struct IrConst){
        .kind = IR_CONST_PENDING,
        .pending.did = did,
    };
    return k;
}

IrConst *ir_new_const_decl(struct Compiler *C, DeclId did)
{
    IrConst *k = NEW_NODE(C, IrConst);
    *k = (struct IrConst){
        .kind = IR_CONST_DECL,
        .decl.did = did,
    };
    return k;
}

IrConst *ir_new_const_infer(struct Compiler *C, int depth, int index)
{
    IrConst *k = NEW_NODE(C, IrConst);
    *k = (struct IrConst){
        .kind = IR_CONST_INFER,
        .infer.depth = depth,
        .infer.index = index,
    };
    return k;
}

IrType *ir_new_never(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Never_ = (struct IrNever){
        .kind = kIrNever,
    };
    return t;
}

IrType *ir_new_infer(struct Compiler *C, enum IrInferKind ikind, int depth, int index)
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

IrType *ir_new_generic(struct Compiler *C, DeclId did)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Generic_ = (struct IrGeneric){
        .kind = kIrGeneric,
        .did = did,
    };
    return t;
}

IrType *ir_new_projection(struct Compiler *C, DeclId did, struct IrGenericArgs *args)
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

IrGenericArgs *ir_instantiate_args(struct Compiler *C, DeclId did, struct SourceSpan span)
{
    IrGenericArgs *args = ir_get_generic_args(C, did);

    IrGenericArgs *result = IrGenericArgs_new(ALLOCATOR_OF(C));
    IrGenericArgs_reserve(result, args->count);
    LIST_XFOREACH (args, IrGenericArg const, p) {
        IrGenericArg r;
        if (IrGenericArg_is_type(*p)) {
            IrType *t = create_type_var(C->U, IR_INFER_TYPE, span);
            r = IrGenericArg_from_type(t);
        } else {
            IrConst *k = create_const_var(C->U, span);
            r = IrGenericArg_from_const(k);
        }
        IrGenericArgs_push(result, r);
    }
    return result;
}

IrGenericArg ir_instantiate(struct Compiler *C, DeclId did, struct SourceSpan span)
{
    IrGenericArg arg = *ir_get_generic_arg(C, did);

    IrGenericArg r;
    if (IrGenericArg_is_type(arg)) {
        IrType *t = create_type_var(C->U, IR_INFER_TYPE, span);
        r = IrGenericArg_from_type(t);
    } else {
        IrConst *k = create_const_var(C->U, span);
        r = IrGenericArg_from_const(k);
    }
    return r;
}


int ir_unify(struct Compiler *C, IrGenericArg a, IrGenericArg b)
{
    if (IrGenericArg_is_type(a) != IrGenericArg_is_type(b))
        return -1;

    if (IrGenericArg_is_type(a)) {
        IrType *x = IrGenericArg_get_type(a);
        IrType *y = IrGenericArg_get_type(b);
        return unify_types(C->U, x, y);
    } else {
        IrConst *x = IrGenericArg_get_const(a);
        IrConst *y = IrGenericArg_get_const(b);
        return unify_consts(C->U, x, y);
    }

    return 0;
}

IrGenericArg ir_normalize(struct Compiler *C, IrGenericArg g)
{
    if (IrGenericArg_is_type(g)) {
        IrType *x = IrGenericArg_get_type(g);
        return IrGenericArg_from_type(
                normalize_type(C->U, x));
    } else {
        IrConst *x = IrGenericArg_get_const(g);
        return IrGenericArg_from_const(
                normalize_const(C->U, x));
    }
}

IrGenericArg ir_normalize_projections(struct Compiler *C, IrGenericArg g)
{
    if (IrGenericArg_is_type(g)) {
        IrType *x = IrGenericArg_get_type(g);
        return IrGenericArg_from_type(
                normalize_projections(C->U, x));
    } else {
        IrConst *x = IrGenericArg_get_const(g);
        return IrGenericArg_from_const(
                normalize_const(C->U, x));
    }
}


struct IrGenericDef *ir_new_generic_type_def(struct Compiler *C, DeclId did, Str const *name, struct SourceSpan span, struct IrTraitList *bounds)
{
    struct IrGenericDef *def = (struct IrGenericDef *)compiler_alloc(C, NULL, 0, sizeof(*def));
    *def = (struct IrGenericDef){
        .did = did,
        .is_type = PAW_TRUE,
        .span = span,
        .type.name = name,
        .type.bounds = bounds,
    };
    ir_set_def_kind(C, did, IR_GENERIC_DEF);
    return def;
}

struct IrGenericDef *ir_new_generic_const_def(struct Compiler *C, DeclId did, IrType *type, Str const *name, struct SourceSpan span)
{
    struct IrGenericDef *def = (struct IrGenericDef *)compiler_alloc(C, NULL, 0, sizeof(*def));
    *def = (struct IrGenericDef){
        .did = did,
        .is_type = PAW_FALSE,
        .span = span,
        .konst.type = type,
        .konst.name = name,
    };
    ir_set_def_kind(C, did, IR_GENERIC_DEF);
    return def;
}

struct IrFieldDef *ir_new_field_def(struct Compiler *C, DeclId did, Str const *name, paw_Bool is_pub)
{
    struct IrFieldDef *def = (struct IrFieldDef *)compiler_alloc(C, NULL, 0, sizeof(*def));
    *def = (struct IrFieldDef){
        .did = did,
        .name = name,
        .is_pub = is_pub,
    };
    ir_set_def_kind(C, did, IR_FIELD_DEF);
    return def;
}

struct IrVariantDef *ir_new_variant_def(struct Compiler *C, DeclId did, DeclId cons_did, DeclId base_did, int discr, Str const *name, struct IrFieldDefs *fields)
{
    struct IrVariantDef *def = (struct IrVariantDef *)compiler_alloc(C, NULL, 0, sizeof(*def));
    *def = (struct IrVariantDef){
        .did = did,
        .cons_did = cons_did,
        .base_did = base_did,
        .fields = fields,
        .discr = discr,
        .name = name,
    };
    ir_set_def_kind(C, did, IR_VARIANT_DEF);
    return def;
}

struct IrFnDef *ir_new_fn_def(struct Compiler *C, DeclId did, Str const *name, IrGenericDefs *generics, IrType *result, struct IrParams *params, DeclId parent, paw_Bool is_pub)
{
    struct IrFnDef *def = (struct IrFnDef *)compiler_alloc(C, NULL, 0, sizeof(*def));
    *def = (struct IrFnDef){
        .did = did,
        .parent = parent,
        .generics = generics,
        .result = result,
        .params = params,
        .is_pub = is_pub,
        .name = name,
    };
    ir_set_def_kind(C, did, IR_FN_DEF);
    return def;
}

struct IrAdtDef *ir_new_adt_def(struct Compiler *C, DeclId did, Str const *name, IrGenericDefs *generics, IrVariantDefs *variants, paw_Bool is_pub, paw_Bool is_struct)
{
    struct IrAdtDef *def = (struct IrAdtDef *)compiler_alloc(C, NULL, 0, sizeof(*def));
    *def = (struct IrAdtDef){
        .did = did,
        .generics = generics,
        .variants = variants,
        .is_struct = is_struct,
        .is_pub = is_pub,
        .name = name,
    };
    ir_set_def_kind(C, did, IR_ADT_DEF);
    return def;
}

struct IrAssocItem *ir_new_assoc_item(struct Compiler *C, DeclId did, Str const *name, DeclId parent, paw_Bool is_pub)
{
    struct IrAssocItem *item = (struct IrAssocItem *)compiler_alloc(C, NULL, 0, sizeof(*item));
    *item = (struct IrAssocItem){
        .did = did,
        .name = name,
        .parent = parent,
        .is_pub = is_pub,
    };
    return item;
}

struct IrTraitDef *ir_new_trait_def(struct Compiler *C, DeclId did, Str const *name, IrGenericDefs *generics, IrTypeList *methods, IrAssocItems *items, paw_Bool is_pub)
{
    struct IrTraitDef *def = (struct IrTraitDef *)compiler_alloc(C, NULL, 0, sizeof(*def));
    *def = (struct IrTraitDef){
        .did = did,
        .generics = generics,
        .methods = methods,
        .items = items,
        .is_pub = is_pub,
        .name = name,
    };
    ir_set_def_kind(C, did, IR_TRAIT_DEF);
    return def;
}

struct IrImpl *ir_new_impl(struct Compiler *C, DeclId did, IrType *type, IrTrait *trait, IrGenericDefs *generics, IrTypeList *methods, IrAssocItems *items)
{
    struct IrImpl *impl = (struct IrImpl *)compiler_alloc(C, NULL, 0, sizeof(*impl));
    *impl = (struct IrImpl){
        .did = did,
        .type = type,
        .trait = trait,
        .generics = generics,
        .methods = methods,
        .items = items,
    };
    ir_set_def_kind(C, did, IR_IMPL_DEF);
    return impl;
}

IrType *ir_get_type(struct Compiler *C, NodeId id)
{
    IrType *const *ptype = HirTypeMap_get(C->hir_types, id);
    return ptype != NULL ? *ptype : NULL;
}

IrGenericArg *ir_get_generic_arg(struct Compiler *C, DeclId did)
{
    return IrDeclArgs_get(C->ir_decl_args, did);
}

void ir_set_type(struct Compiler *C, NodeId id, IrType *type)
{
    paw_assert(type != NULL);
    HirTypeMap_insert(C->hir_types, id, type);
}

struct IrAssocItem *ir_get_assoc_item(struct Compiler *C, DeclId did)
{
    struct IrAssocItem *const *pdef = IrAssocItemMap_get(C->ir_assoc_items, did);
    return pdef != NULL ? *pdef : NULL;
}

struct IrFnDef *ir_get_fn_def(struct Compiler *C, DeclId did)
{
    struct IrFnDef *const *pdef = FnDefMap_get(C->fn_defs, did);
    return pdef != NULL ? *pdef : NULL;
}

struct IrGenericDef *ir_get_generic_def(struct Compiler *C, DeclId did)
{
    struct IrGenericDef *const *pdef = GenericDefMap_get(C->generic_defs, did);
    return pdef != NULL ? *pdef : NULL;
}

struct IrVariantDef *ir_get_variant_def(struct Compiler *C, DeclId did)
{
    return *VariantDefMap_get(C->variant_defs, did);
}

struct IrAdtDef *ir_get_adt_def(struct Compiler *C, DeclId did)
{
    return *AdtDefMap_get(C->adt_defs, did);
}

struct IrImpl *ir_get_impl_def(struct Compiler *C, DeclId did)
{
    return *ImplMap_get(C->impl_defs, did);
}

struct IrTraitDef *ir_get_trait_def(struct Compiler *C, DeclId did)
{
    return *TraitDefMap_get(C->trait_defs, did);
}

IrType *ir_get_def_type(struct Compiler *C, DeclId did)
{
    return *DefTypeMap_get(C->def_types, did);
}

IrGenericArg ir_get_def_arg(struct Compiler *C, DeclId did)
{
    __builtin_trap(); // TODO
}

IrTrait *ir_get_trait(struct Compiler *C, DeclId did)
{
    return ir_new_trait(C, did, ir_get_generic_args(C, did));
}

DEFINE_MAP(TraitCache, P_ID_HASH, P_ID_EQUALS, DeclId, void *)

static IrGenericArgs *replace_self_in_trait_args(struct Compiler *C, IrGenericArgs *args, struct IrGeneric *target)
{
    IrGenericArgs *result = IrGenericArgs_new(ALLOCATOR_OF(C));
    IrGenericArgs_reserve(result, args->count);
    LIST_XFOREACH (args, IrGenericArg const, arg)
        IrGenericArgs_push(result, *arg);

    IrGenericArgs_set(result, 0, IrGenericArg_from_type((IrType *)target));
    return result;
}

enum IrDefKind ir_get_kind(struct Compiler *C, DeclId did)
{
    return *IrDefKinds_get(C->ir_def_kinds, did);
}

IrGenericArgs *ir_get_generic_args(struct Compiler *C, DeclId did)
{
    IrGenericArgs *const *p = IrGenericTypes_get(C->ir_generic_args, did);
    return p != NULL ? *p : NULL;
}

IrConstraints *ir_get_constraints(struct Compiler *C, DeclId did)
{
    IrConstraints *const *p = IrConstraintsMap_get(C->ir_constraints, did);
    return p != NULL ? *p : NULL;
}

IrTraitList *ir_get_trait_bounds(struct Compiler *C, DeclId did)
{
    IrTraitList *const *p = IrTraitBounds_get(C->ir_trait_bounds, did);
    return p != NULL ? *p : NULL;
}

void ir_set_generic_args(struct Compiler *C, DeclId did, IrGenericArgs *types)
{
    paw_Bool const exists = IrGenericTypes_insert(C->ir_generic_args, did, types);
    paw_assert(!exists); PAW_UNUSED(exists);
}

void ir_set_trait_bounds(struct Compiler *C, DeclId did, IrTraitList *traits)
{
    paw_Bool const exists = IrTraitBounds_insert(C->ir_trait_bounds, did, traits);
    paw_assert(!exists); PAW_UNUSED(exists);
}


IrType *ir_get_context(struct Compiler *C, IrType *fn)
{
    DeclId parent_did;
    enum IrDefKind const fn_kind = ir_get_kind(C, IR_TYPE_DID(fn));
    if (fn_kind == IR_FN_DEF) {
        struct IrFnDef const *fn_def = ir_get_fn_def(C, IR_TYPE_DID(fn));
        parent_did = fn_def->parent;
    } else {
        // "fn" is the type of a type constructor
        paw_assert(fn_kind == IR_VARIANT_DEF);
        struct IrVariantDef const *variant_def = ir_get_variant_def(C, IR_TYPE_DID(fn));
        struct IrAdtDef const *adt_def = ir_get_adt_def(C, variant_def->base_did);
        parent_did = adt_def->did;
    }

    if (!DECL_ID_EXISTS(parent_did))
        return NULL;

    enum IrDefKind const parent_kind = ir_get_kind(C, parent_did);
    if (parent_kind == IR_TRAIT_DEF)
        return IrGenericArg_get_type(
                IrGenericArgs_first(IR_GENERIC_ARGS(fn)));

    IrType *parent;
    if (parent_kind == IR_IMPL_DEF) {
        struct IrImpl const *impl_def = ir_get_impl_def(C, parent_did);
        parent = impl_def->type;
    } else {
        paw_assert(parent_kind == IR_ADT_DEF);
        parent = ir_get_def_type(C, parent_did);
    }

    IrGenericArgs *params = ir_get_generic_args(C, IR_TYPE_DID(fn));
    if (params == NULL) return parent;

    IrGenericArgs *args = IR_GENERIC_ARGS(fn);
    struct Substitution const subst = {params, args};
    return pawP_substitute(C, parent, subst);
}

// TODO: wrong, only works in some situations...
IrTrait *ir_get_trait_context(struct Compiler *C, IrType *fn)
{
    if (IrIsProjection(fn)) {
        struct IrProjection const *t = IrGetProjection(fn);
        struct IrAssocItem const *item = ir_get_assoc_item(C, t->did);
        enum IrDefKind const parent_kind = ir_get_kind(C, item->parent);
        IrGenericArgs *args = t->args;
        if (parent_kind == IR_IMPL_DEF) {
            args = IrGenericArgs_new(ALLOCATOR_OF(C));
            IrGenericArgs_reserve(args, t->args->count);
            struct IrImpl const *def = ir_get_impl_def(C, item->parent);
            for (int i = 0; i < def->generics->count; ++i)
                IrGenericArgs_push(args, IrGenericArgs_get(t->args, i));
        }
        return ir_new_trait(C, item->parent, args);
    }
    struct IrFnDef const *fn_def = ir_get_fn_def(C, IR_TYPE_DID(fn));
    if (!DECL_ID_EXISTS(fn_def->parent)) return NULL;
    enum IrDefKind const kind = ir_get_kind(C, fn_def->parent);
    if (kind == IR_TRAIT_DEF)
        return ir_solver_instantiate_trait_with(C->S,
                fn_def->parent, IR_GENERIC_ARGS(fn));

    paw_assert(kind == IR_IMPL_DEF);
    struct IrImpl const *impl_def = ir_get_impl_def(C, fn_def->parent);
    return ir_solver_instantiate_impl_with(C->S,
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
            return x.i64 == y.i64;
        default:
            paw_assert(IrIsFloat(type));
            return x.f64 == y.f64;
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
    ir_type_visitor_init(&V, C, flag_ptr);
    V.VisitInfer = report_type_var;
    V.VisitConst = report_const_var;
    return V;
}

paw_Bool ir_type_contains_inference_var(struct Compiler *C, IrType *type)
{
    paw_Bool found_inference_var = PAW_FALSE;
    struct IrTypeVisitor V = init_ivar_visitor(C, &found_inference_var);
    ir_visit_type(&V, type);
    return found_inference_var;
}

paw_Bool ir_const_contains_inference_var(struct Compiler *C, IrConst *konst)
{
    paw_Bool found_inference_var = PAW_FALSE;
    struct IrTypeVisitor V = init_ivar_visitor(C, &found_inference_var);
    ir_visit_const(&V, konst);
    return found_inference_var;
}

paw_Bool ir_trait_contains_inference_var(struct Compiler *C, IrTrait *trait)
{
    paw_Bool found_inference_var = PAW_FALSE;
    struct IrTypeVisitor V = init_ivar_visitor(C, &found_inference_var);
    ir_visit_trait(&V, trait);
    return found_inference_var;
}


static paw_Uint hash_type(IrType *type);
static paw_Uint hash_arg(IrGenericArg arg);

static paw_Uint hash_type_list(IrTypeList const *types)
{
    paw_Uint hash = 0;
    if (types != NULL) {
        LIST_XFOREACH (types, IrType *const, p)
            hash = hash_combine(hash, hash_type(*p));
    }
    return hash;
}

static paw_Uint hash_arg_list(IrGenericArgs const *args)
{
    paw_Uint hash = 0;
    LIST_XFOREACH (args, IrGenericArg const, p)
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
            hash = hash_combine(hash, P_ID_HASH(k->pending.did));
            break;
        case IR_CONST_VALUE:
            hash = hash_combine(hash, hash_type(k->value.type));
            // TODO: won't work for composites
            hash = hash_combine(hash, (paw_Uint)k->value.value.i64);
            break;
        default:
            paw_assert(k->kind == IR_CONST_DECL);
            hash = hash_combine(hash, P_ID_HASH(k->decl.did));
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

static paw_Bool arglist_equals(IrGenericArgs *a, IrGenericArgs *b)
{
    if (a->count != b->count)
        return PAW_FALSE;

    for (int i = 0; i < a->count; ++i) {
        IrGenericArg const ga = IrGenericArgs_get(a, i);
        IrGenericArg const gb = IrGenericArgs_get(b, i);
        if (!ir_arg_equals(ga, gb))
            return PAW_FALSE;
    }

    return PAW_TRUE;
}

static paw_Bool typelist_equals(IrTypeList *a, IrTypeList *b)
{
    if (a->count != b->count)
        return PAW_FALSE;

    for (int i = 0; i < a->count; ++i) {
        IrType *ta = IrTypeList_get(a, i);
        IrType *tb = IrTypeList_get(b, i);
        if (!ir_type_equals(ta, tb))
            return PAW_FALSE;
    }

    return PAW_TRUE;
}

static paw_Bool type_equals(IrType *lhs, IrType *rhs)
{
    if (IR_KINDOF(lhs) != IR_KINDOF(rhs))
        return PAW_FALSE;

    switch (IR_KINDOF(lhs)) {
        case kIrInt:
            return IR_INT_KIND(lhs) == IR_INT_KIND(rhs);
        case kIrFloat:
            return IR_FLOAT_KIND(lhs) == IR_FLOAT_KIND(rhs);
        case kIrPtr:
            return type_equals(ir_deref(lhs), ir_deref(rhs));
        case kIrAdt: {
            struct IrAdt const *x = IrGetAdt(lhs);
            struct IrAdt const *y = IrGetAdt(rhs);
            return P_ID_EQUALS(x->did, y->did)
                && arglist_equals(x->args, y->args);
        }
        case kIrFnPtr: {
            struct IrFnPtr const *x = IrGetFnPtr(lhs);
            struct IrFnPtr const *y = IrGetFnPtr(rhs);
            return typelist_equals(x->params, y->params)
                && type_equals(x->result, y->result);
        }
        case kIrSignature: {
            struct IrSignature const *x = IrGetSignature(lhs);
            struct IrSignature const *y = IrGetSignature(rhs);
            return P_ID_EQUALS(x->did, y->did)
                && arglist_equals(x->args, y->args);
        }
        case kIrSlice: {
            struct IrSlice const *x = IrGetSlice(lhs);
            struct IrSlice const *y = IrGetSlice(rhs);
            return type_equals(x->type, y->type);
        }
        case kIrArray: {
            struct IrArray const *x = IrGetArray(lhs);
            struct IrArray const *y = IrGetArray(rhs);
            return type_equals(x->type, y->type)
                && ir_const_equals(x->length, y->length);
        }
        case kIrTuple: {
            struct IrTuple const *x = IrGetTuple(lhs);
            struct IrTuple const *y = IrGetTuple(rhs);
            return typelist_equals(x->elems, y->elems);
        }
        case kIrGeneric: {
            struct IrGeneric const *x = IrGetGeneric(lhs);
            struct IrGeneric const *y = IrGetGeneric(rhs);
            return P_ID_EQUALS(x->did, y->did);
        }
        case kIrProjection: {
            struct IrProjection const *x = IrGetProjection(lhs);
            struct IrProjection const *y = IrGetProjection(rhs);
            return P_ID_EQUALS(x->did, y->did)
                && arglist_equals(x->args, y->args);
        }
        case kIrClosure: {
            struct IrClosure const *x = IrGetClosure(lhs);
            struct IrClosure const *y = IrGetClosure(rhs);
            return P_ID_EQUALS(x->did, y->did)
                && arglist_equals(x->args, y->args) ;
        }
        default:
            return PAW_TRUE;
    }
}

paw_Bool ir_type_equals(IrType *a, IrType *b)
{
    return type_equals(a, b);
}

paw_Uint ir_type_hash(IrType *t)
{
    return hash_type(t);
}

paw_Uint ir_type2_hash(struct IrType2 t)
{
    paw_Uint const first = ir_type_hash(t.first);
    paw_Uint const second = ir_type_hash(t.second);
    return hash_combine(first, second);
}

paw_Bool ir_type2_equals(struct IrType2 a, struct IrType2 b)
{
    return ir_type_equals(a.first, b.first)
        && ir_type_equals(a.second, b.second);
}

paw_Uint ir_const_hash(IrConst const *k)
{
    return hash_const(k);
}

paw_Bool ir_const_equals(IrConst const *a, IrConst const *b)
{
    if (a->kind != b->kind)
        return PAW_FALSE;

    switch (a->kind) {
        case IR_CONST_PENDING:
            return P_ID_EQUALS(a->pending.did, b->pending.did);
        case IR_CONST_VALUE:
            return ir_type_equals(a->value.type, b->value.type)
                // TODO: won't work for composites
                && a->value.value.i64 == b->value.value.i64;
        default:
            paw_assert(a->kind == IR_CONST_DECL);
            return P_ID_EQUALS(a->decl.did, b->decl.did);
    }
}

paw_Uint ir_trait_hash(IrTrait *trait)
{
    return hash_trait(trait);
}

paw_Uint ir_arg_hash(IrGenericArg arg)
{
    return hash_arg(arg);
}

paw_Bool ir_arg_equals(IrGenericArg a, IrGenericArg b)
{
    if (IrGenericArg_is_type(a) != IrGenericArg_is_type(b))
        return PAW_FALSE;

    return IrGenericArg_is_type(a)
        ? ir_type_equals(IrGenericArg_get_type(a), IrGenericArg_get_type(b))
        : ir_const_equals(IrGenericArg_get_const(a), IrGenericArg_get_const(b));
}

IrType *ir_get_custom_drop_type(struct Compiler *C, IrType *type)
{
    DeclId const drop_did = C->core_traits[CORE_TRAIT_DROP];
    IrDefs const *trait_defs = ir_trait_impls_for(C, type);
    LIST_XFOREACH (trait_defs, DeclId const, p) {
        struct IrImpl const *def = ir_get_impl_def(C, *p);
        if (P_ID_EQUALS(def->trait->did, drop_did)) {
            IrSolver *S = ir_push_solver(C);
            struct UnifierState const state = unifier_state(C->U);
            struct IrImplInstance const inst = ir_solver_instantiate_impl(S, *p, SPAN_UNUSED);
            ir_solver_add_obligations_from(S, *p, inst.args, (struct IrObligationCause){0});
            paw_Bool const matches = P_ID_EQUALS(inst.trait->did, drop_did)
                && unify_types(C->U, inst.type, type) == 0
                && ir_solver_solve_all(S);
            ir_pop_solver(C);

            if (matches) {
                IrType *method = IrTypeList_first(def->methods);
                if (IrGenericDefs_len(def->generics) == 0) return method;
                return ir_solver_instantiate_type_with(C->S,
                        IR_TYPE_DID(method), inst.args);
            }
            undo_unifications(C->U, state);
        }
    }
    return NULL;
}


static paw_Bool any_needs_drop(struct Compiler *C, IrTypeList *types)
{
    LIST_XFOREACH (types, IrType *const, p) {
        if (ir_needs_drop(C, *p)) return PAW_TRUE;
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

IrType *ir_materialize_drop_type(struct Compiler *C, IrType *type)
{
    IrTypeList *methods = IrTypeList_new(ALLOCATOR_OF(C));
    DeclId const drop_did = C->core_traits[CORE_TRAIT_DROP];
    IrTrait *trait = ir_new_trait(C, drop_did, IrGenericArgs_new(ALLOCATOR_OF(C)));
    struct IrImpl *impl = ir_new_impl(C, next_did(C), type, trait,
            IrGenericDefs_new(ALLOCATOR_OF(C)), methods, IrAssocItems_new(ALLOCATOR_OF(C)));
    ImplMap_insert(C->impl_defs, impl->did, impl);
    IrConstraintsMap_insert(C->ir_constraints, impl->did, IrConstraints_new(ALLOCATOR_OF(C)));
    DefTypeMap_insert(C->def_types, impl->did, type);

    {
        IrDefs *trait_defs = ir_trait_impls_for(C, type);
        IrDefs_push(trait_defs, impl->did);
    }

    IrParams *params = IrParams_new(ALLOCATOR_OF(C));
    IrParams_push(params, (struct IrParam){
                .type = ir_new_ptr(C, type),
                .name = SCAN_STR(C, "self"),
            });
    struct IrFnDef *fn = ir_new_fn_def(C, next_did(C),
            SCAN_STR(C, "drop"), IrGenericDefs_new(ALLOCATOR_OF(C)),
            ir_new_unit(C), params, impl->did,
            PAW_TRUE);
    FnDefMap_insert(C->fn_defs, fn->did, fn);
    IrGenericArgs *args = IrGenericArgs_new(ALLOCATOR_OF(C));
    IrGenericTypes_insert(C->ir_generic_args, fn->did, args);
    IrConstraintsMap_insert(C->ir_constraints, fn->did, IrConstraints_new(ALLOCATOR_OF(C)));
    IrType *drop = ir_new_signature(C, fn->did, args);
    DefTypeMap_insert(C->def_types, fn->did, drop);
    IrTypeList_push(methods, drop);
    return drop;
}

static paw_Bool has_field_with_drop(struct Compiler *C, IrType *type)
{
    if (IrIsTuple(type)) {
        struct IrTuple const *t = IrGetTuple(type);
        return any_needs_drop(C, t->elems);
    } else if (IrIsAdt(type)) {
        struct IrAdt *t = IrGetAdt(type);
        struct IrAdtDef const *def = ir_get_adt_def(C, t->did);
        if (def->is_struct) {
            IrTypeList *fields = instantiate_struct_fields(C, t);
            return any_needs_drop(C, fields);
        } else {
            LIST_XFOREACH (def->variants, struct IrVariantDef *const, v) {
                IrTypeList *fields = instantiate_variant_fields(C, t, (*v)->discr);
                if (any_needs_drop(C, fields)) return PAW_TRUE;
            }
        }
    } else if (IrIsArray(type)) {
        return ir_needs_drop(C, IrGetArray(type)->type);
    }
    return PAW_FALSE;
}

IrType *ir_get_drop_type(struct Compiler *C, IrType *type)
{
    IrType *drop = ir_get_custom_drop_type(C, type);
    if (drop == NULL && has_field_with_drop(C, type))
        return ir_materialize_drop_type(C, type);
    return drop;
}

paw_Bool ir_needs_drop(struct Compiler *C, IrType *type)
{
    if (IrIsGeneric(type)) return PAW_TRUE;
    IrType *drop = ir_get_custom_drop_type(C, type);
    if (drop != NULL) return PAW_TRUE;

    if (IrIsTuple(type)) {
        struct IrTuple const *t = IrGetTuple(type);
        return any_needs_drop(C, t->elems);
    } else if (IrIsArray(type)) {
        struct IrArray const *t = IrGetArray(type);
        return ir_needs_drop(C, t->type);
    } else if (IrIsAdt(type)) {
        struct IrAdt *t = IrGetAdt(type);
        struct IrAdtDef const *def = ir_get_adt_def(C, t->did);
        {
            // special case: Drop is not generated for ManuallyDrop instances
            struct Builtin const *b = pawP_builtin_info(C, BUILTIN_MANUALLY_DROP);
            if (P_ID_EQUALS(def->did, b->did)) return PAW_FALSE;
        }
        if (def->is_struct) {
            IrTypeList *fields = instantiate_struct_fields(C, t);
            return any_needs_drop(C, fields);
        } else {
            LIST_XFOREACH (def->variants, struct IrVariantDef *const, v) {
                IrTypeList *fields = instantiate_variant_fields(C, t, (*v)->discr);
                if (any_needs_drop(C, fields)) return PAW_TRUE;
            }
            return PAW_FALSE;
        }
    }
    return PAW_FALSE;
}

// TODO: this is wrong because of integer/float inference vars
paw_Bool ir_is_copyable(struct Compiler *C, IrType *type)
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
        LIST_XFOREACH (t->elems, IrType *const, p)
            accum &= ir_is_copyable(C, *p);
        return accum;
    }

    if (IrIsArray(type))
        return ir_is_copyable(C, IrGetArray(type)->type);

    if (IR_IS_FUNC_TYPE(type)) {
        if (ir_is_capturing_closure(C, type)) {
            UpvalueList const *upvalues = *UpvalueTable_get(C->upvtab, IR_TYPE_DID(type));
            LIST_XFOREACH (upvalues, struct UpvalueInfo const, u) {
                IrType *upvalue = ir_get_type(C, u->id);
                if (!ir_is_copyable(C, upvalue))
                    return PAW_FALSE;
            }
        }
        return PAW_TRUE;
    }

    IrSolver *S = ir_push_solver(C);
    if (IrIsGeneric(type)) {
        // Add predicates on the generic (from the binder that declares this generic type arg).
        // This is necessary because this function may be called from contexts that no longer
        // have knowledge about the predicates involving this generic.
        IrTraitList const *bounds = ir_get_trait_bounds(C, IR_TYPE_DID(type));
        if (bounds != NULL) {
            LIST_XFOREACH (bounds, IrTrait *const, t)
                ir_solver_add_predicate(S, type, *t, (struct IrObligationCause){0});
        }
    }
    struct UnifierState const state = unifier_state(C->U);
    DeclId const copy_did = C->core_traits[CORE_TRAIT_COPY];
    IrGenericArgs *copy_args = IrGenericArgs_new(ALLOCATOR_OF(C));
    IrGenericArgs_push(copy_args, IrGenericArg_from_type(type));
    IrTrait *copy = ir_solver_instantiate_trait_with(S, copy_did, copy_args);
    ir_solver_add_impl_trait_obligation(S, type, copy,
            (struct IrObligationCause){0});
    struct IrSolverResult const r = ir_solver_solve(S);
    undo_unifications(C->U, state);
    ir_pop_solver(C);
    return r.status == IR_SOLVER_SOLVED;
}

IrType *ir_materialize_fn(struct Compiler *C, DeclId did, IrGenericArgs *type_args)
{
    IrType *result;
    IrTypeList *params = IrTypeList_new(ALLOCATOR_OF(C));
    enum IrDefKind const kind = ir_get_kind(C, did);
    if (kind == IR_VARIANT_DEF) {
        struct IrVariantDef const *def = ir_get_variant_def(C, did);
        IrTypeList_reserve(params, def->fields->count);
        LIST_XFOREACH (def->fields, struct IrFieldDef *const, p) {
            IrType *field = ir_get_def_type(C, (*p)->did);
            IrTypeList_push(params, field);
        }
        result = ir_get_def_type(C, def->base_did);
    } else {
        paw_assert(kind == IR_FN_DEF);
        struct IrFnDef const *def = ir_get_fn_def(C, did);
        IrTypeList_reserve(params, def->params->count);
        LIST_XFOREACH (def->params, struct IrParam const, p)
            IrTypeList_push(params, p->type);
        result = def->result;
    }

    IrGenericArgs *type_params = ir_get_generic_args(C, did);
    struct Substitution const subst = {type_params, type_args};

    LIST_XFOREACH (params, IrType *, p)
        *p = pawP_substitute(C, *p, subst);
    result = pawP_substitute(C, result, subst);

    return normalize_projections(C->U,
            ir_new_fn_ptr(C, params, result));
}

IrTrait *ir_get_projection_trait(struct Compiler *C, struct IrProjection const *t)
{
    struct IrAssocItem const *item = ir_get_assoc_item(C, t->did);
    return ir_new_trait(C, item->parent, t->args);
}

// Convert all type args into type `!` and all const args to their zero values
static IrGenericArgs *cannonicalize_args(struct Compiler *C, IrGenericArgs const *args)
{
    IrGenericArgs *result = IrGenericArgs_new(ALLOCATOR_OF(C));
    IrGenericArgs_reserve(result, args->count);
    LIST_XFOREACH (args, IrGenericArg const, p) {
        IrGenericArg r;
        if (IrGenericArg_is_type(*p)) {
            r = IrGenericArg_from_type(
                    ir_new_never(C));
        } else {
            IrConst *k = IrGenericArg_get_const(*p);
            IrType *const_type;
            if (k->kind == IR_CONST_DECL) {
                const_type = ir_get_def_type(C, k->decl.did);
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
                    ir_new_const_value(C, zero_value, const_type));
        }
        IrGenericArgs_push(result, r);
    }
    return result;
}

static IrType *cannonicalize_type(struct Compiler *C, IrType *type)
{
    type = ir_remove_indirection(C, type);
    switch (IR_KINDOF(type)) {
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            IrGenericArgs const *params = ir_get_generic_args(C, t->did);
            IrGenericArgs *args = cannonicalize_args(C, params);
            return ir_new_adt(C, t->did, args);
        }
        case kIrSignature: {
            struct IrSignature const *t = IrGetSignature(type);
            IrGenericArgs const *params = ir_get_generic_args(C, t->did);
            IrGenericArgs *args = cannonicalize_args(C, params);
            return ir_new_signature(C, t->did, args);
        }
        case kIrProjection: {
            struct IrProjection const *t = IrGetProjection(type);
            // TODO: may need to pass `Self ++ Trait.params[1..]` instead of `t->args`. when `type` is `<T as Trait<123>>::Item` the integer inference var messes us up, need the concrete type from def of `Trait`
            IrGenericArgs *args = cannonicalize_args(C, t->args);
            return ir_new_projection(C, t->did, args);
        }
        case kIrTuple: {
            struct IrTuple const *t = IrGetTuple(type);
            IrTypeList *fields = IrTypeList_new(ALLOCATOR_OF(C));
            IrTypeList_reserve(fields, t->elems->count);
            while (fields->count < t->elems->count)
                IrTypeList_push(fields, ir_new_never(C));
            return ir_new_tuple(C, fields);
        }
        case kIrArray:
            return ir_new_array(C, ir_new_never(C),
                    ir_new_const_value(C,
                        (union IrValue){.u64 = 0},
                        ir_new_int(C, IR_USIZE)));
        case kIrInfer:
        case kIrGeneric:
            return ir_new_never(C);
        case kIrSlice:
            return ir_new_slice(C, ir_new_never(C));
        case kIrPtr:
            return ir_new_ptr(C, ir_new_never(C));
        default:
            return type;
    }
}

static IrDefs *get_or_create_candidates_for(struct Compiler *C, IrType *self, IrTypeMap *impls)
{
    self = cannonicalize_type(C, self);
    void *const *p = IrTypeMap_get(impls, self);
    if (p == NULL) {
        IrDefs *defs = IrDefs_new(ALLOCATOR_OF(C));
        IrTypeMap_insert(impls, self, defs);
        return defs;
    }
    return *p;
}

IrDefs *ir_inherent_impls_for(struct Compiler *C, IrType *self)
{
    return get_or_create_candidates_for(C, self, C->impls.inherent);
}

IrDefs *ir_trait_impls_for(struct Compiler *C, IrType *self)
{
    return get_or_create_candidates_for(C, self, C->impls.trait);
}

static IrTraitList *without_first_trait(struct Compiler *C, IrTraitList *traits)
{
    paw_assert(traits->count > 0);
    IrTraitList *suffix = IrTraitList_new(ALLOCATOR_OF(C));
    IrTraitList_reserve(suffix, traits->count - 1);
    for (int i = 1; i < traits->count; ++i)
        IrTraitList_push(suffix, IrTraitList_get(traits, i));
    return suffix;
}

IrTraitList *ir_supertraits_of(struct Compiler *C, DeclId did)
{
    struct IrTraitDef const *trait_def = ir_get_trait_def(C, did);
    struct IrGenericDef const *self_def = IrGenericDefs_first(trait_def->generics);
    IrTraitList *supertraits = ir_get_trait_bounds(C, self_def->did);
    return supertraits != NULL ? without_first_trait(C, supertraits) : IrTraitList_new(ALLOCATOR_OF(C));
}


struct Printer {
    struct Compiler *C;
    Buffer *buf;
    paw_Env *P;
    int indent;
    paw_Bool print_bounds;
};

#define PRINT_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define PRINT_STRING(P, str) lib_add_nstring(ENV(P), (P)->buf, (str)->text, (str)->length)
#define PRINT_FORMAT(P, ...) lib_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define PRINT_CHAR(P, c) lib_add_char(ENV(P), (P)->buf, c)

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
        LIST_ENUMERATE (bounds, index, ptype) {
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

static void print_generic_args(struct Printer *P, IrGenericArgs *args)
{
    // TODO: be consistent w/ semantics: does `.args = NULL` or `.args.count = 0` mean monomorphic
    if (args->count > 0) {
        P->print_bounds = PAW_TRUE;
        PRINT_CHAR(P, '<');
        for (int i = 0; i < args->count; ++i) {
            if (i > 0) PRINT_LITERAL(P, ", ");
            print_generic_arg(P, IrGenericArgs_get(args, i));
        }
        PRINT_CHAR(P, '>');
        P->print_bounds = PAW_FALSE;
    }
}

static void print_trait_omitting_self(struct Printer *P, IrTrait *t)
{
    paw_assert(t->args->count > 0);
    struct IrTraitDef const *def = ir_get_trait_def(P->C, t->did);
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
    struct IrTraitDef const *def = ir_get_trait_def(P->C, t->did);
    PRINT_STRING(P, def->name);

    P->print_bounds = PAW_TRUE;

    paw_Bool printed_self = PAW_FALSE;
    IrType *current = IrGenericArg_get_type(IrGenericArgs_first(t->args));
    DeclId const base_did = IrGenericDefs_first(def->generics)->did;
    if (!IrIsGeneric(current) || P_ID_EQUALS(IR_TYPE_DID(current), base_did)) {
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
            print_generic_args(P, t->args);
            IrType *fn = ir_materialize_fn(P->C, t->did, t->args);
            PRINT_FORMAT(P, "{%s}", ir_print_type(P->C, fn));
            break;
        }
        case kIrSignature: {
            struct IrSignature *fsig = IrGetSignature(type);
            enum IrDefKind const kind = ir_get_kind(P->C, fsig->did);
            if (kind == IR_FN_DEF) {
                struct IrFnDef *def = ir_get_fn_def(P->C, fsig->did);
                PRINT_STRING(P, def->name);
                print_generic_args(P, fsig->args);
                IrType *fn = ir_materialize_fn(P->C, fsig->did, fsig->args);
                PRINT_FORMAT(P, "{%s}", ir_print_type(P->C, fn));
            } else {
                paw_assert(kind == IR_VARIANT_DEF);
                IrType *parent = ir_get_context(P->C, type);
                print_type(P, parent);
                PRINT_LITERAL(P, "::");
                struct IrVariantDef const *variant = ir_get_variant_def(P->C, fsig->did);
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
            struct IrGenericDef const *def = ir_get_generic_def(P->C, t->did);
            if (def->is_type) {
                PRINT_STRING(P, def->type.name);
            } else {
                PRINT_STRING(P, def->konst.name);
            }
            break;
        }
        case kIrProjection: {
            struct IrProjection const *t = IrGetProjection(type);
            struct IrAssocItem const *item = ir_get_assoc_item(P->C, t->did);
            IrType *self = IrGenericArg_get_type(IrGenericArgs_first(t->args));
            IrTrait *trait = ir_new_trait(P->C, item->parent, t->args);
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
            struct IrAdtDef *def = ir_get_adt_def(P->C, adt->did);
            PRINT_STRING(P, def->name);
            print_generic_args(P, adt->args);
            break;
        }
    }
}

static void print_value(struct Printer *P, IrValue value, IrType *type)
{
    if (IrIsUnit(type)) {
        PRINT_LITERAL(P, "()");
    } else if (IrIsBool(type)) {
        if (value.b) {
            PRINT_LITERAL(P, "true");
        } else {
            PRINT_LITERAL(P, "false");
        }
    } else if (IrIsChar(type)) {
        PRINT_CHAR(P, value.c);
    } else if (IrIsInt(type)) {
        lib_add_int(ENV(P), P->buf, value.i64);
    } else if (IrIsFloat(type)) {
        lib_add_float(ENV(P), P->buf, value.f64);
    } else if (IrIsString(type)) {
        L_ADD_STRING(ENV(P), P->buf, value.s);
    } else if (IrIsArray(type)) {
        struct IrArray const *ty = IrGetArray(type);
        struct Tuple const *t = value.t;
        PRINT_CHAR(P, '[');
        for (size_t i = 0; i < t->num_fields; ++i) {
            if (i != 0) PRINT_LITERAL(P, ", ");
            print_value(P, t->fields[i], ty->type);
        }
        PRINT_CHAR(P, ']');
    } else if (IrIsTuple(type)) {
        struct IrTuple const *ty = IrGetTuple(type);
        struct Tuple const *t = value.t;
        PRINT_CHAR(P, '(');
        for (size_t i = 0; i < t->num_fields; ++i) {
            if (i != 0) PRINT_LITERAL(P, ", ");
            print_value(P, t->fields[i], IrTypeList_get(ty->elems, i));
        }
        PRINT_CHAR(P, ')');
    } else if (IrIsAdt(type)) {
        struct IrAdt const *ty = IrGetAdt(type);
        struct Tuple const *t = value.t;
        struct IrAdtDef const *adt_def = ir_get_adt_def(P->C, ty->did);
        struct IrVariantDef const *variant_def = IrVariantDefs_get(adt_def->variants, t->discr);
        IrTypeList *fields = instantiate_variant_fields(P->C, ty, t->discr);
        if (adt_def->is_struct) {
            PRINT_STRING(P, adt_def->name);
            PRINT_CHAR(P, '{');
            for (size_t i = 0; i < t->num_fields; ++i) {
                if (i != 0) PRINT_LITERAL(P, ", ");
                struct IrFieldDef const *field_def = IrFieldDefs_get(variant_def->fields, i);
                PRINT_STRING(P, field_def->name);
                PRINT_LITERAL(P, ": ");
                print_value(P, t->fields[i], IrTypeList_get(fields, i));
            }
            PRINT_CHAR(P, '}');
        } else {
            PRINT_STRING(P, adt_def->name);
            PRINT_LITERAL(P, "::");
            PRINT_STRING(P, variant_def->name);
            PRINT_CHAR(P, '{');
            for (size_t i = 0; i < t->num_fields; ++i) {
                if (i != 0) PRINT_LITERAL(P, ", ");
                print_value(P, t->fields[i], IrTypeList_get(fields, i));
            }
            PRINT_CHAR(P, '}');
        }
    } else {
        paw_assert(IrIsInfer(type));
        if (IrGetInfer(type)->ikind == IR_INFER_INTEGER) {
            lib_add_int(ENV(P), P->buf, value.i64);
        } else {
            paw_assert(IrGetInfer(type)->ikind == IR_INFER_FLOAT);
            lib_add_float(ENV(P), P->buf, value.f64);
        }
    }
}

static void print_const(struct Printer *P, IrConst *konst)
{
    switch (konst->kind) {
        case IR_CONST_VALUE:
            print_value(P, konst->value.value, konst->value.type);
            break;
        case IR_CONST_PENDING:
            PRINT_LITERAL(P, "<unevaluated>");
            break;
        case IR_CONST_DECL: {
            struct IrGenericDef const *def = ir_get_generic_def(P->C, konst->decl.did);
            paw_assert(!def->is_type);
            PRINT_STRING(P, def->konst.name);
            break;
        }
        case IR_CONST_INFER:
            PRINT_LITERAL(P, "_");
            break;
    }
}

char const *ir_print_type(struct Compiler *C, IrType *type)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    print_type(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               type);

    Str const *s = lib_buffer_finish(P, &buf);
    return s->text;
}

char const *ir_print_trait(struct Compiler *C, IrTrait *trait)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    print_trait(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               trait);

    Str const *s = lib_buffer_finish(P, &buf);
    return s->text;
}

char const *ir_print_impl_trait_obligation(struct Compiler *C, IrType *type, IrTrait *trait)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    struct Printer p = {
        .P = ENV(C),
        .buf = &buf,
        .C = C,
    };
    print_type(&p, type);
    PRINT_LITERAL(&p, ": ");
    print_trait(&p, trait);

    Str const *s = lib_buffer_finish(P, &buf);
    return s->text;
}

Str const *ir_print_type_v2(struct Compiler *C, IrType *type)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    print_type(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               type);

    return lib_buffer_finish(P, &buf);
}

Str const *ir_print_const(struct Compiler *C, IrConst *konst)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    print_const(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               konst);

    return lib_buffer_finish(P, &buf);
}

Str const *ir_print_trait_v2(struct Compiler *C, IrTrait *trait)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    print_trait(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
                   .C = C,
               },
               trait);

    return lib_buffer_finish(P, &buf);
}

Str const *ir_print_impl_trait_obligation_v2(struct Compiler *C, IrType *type, IrTrait *trait)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    lib_init_buffer(P, &buf);

    struct Printer p = {
        .P = ENV(C),
        .buf = &buf,
        .C = C,
    };
    print_type(&p, type);
    PRINT_LITERAL(&p, ": ");
    print_trait(&p, trait);

    return lib_buffer_finish(P, &buf);
}
