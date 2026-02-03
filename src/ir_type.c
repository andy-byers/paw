// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "ir_type.h"
#include "map.h"
#include "solve.h"
#include "unify.h"

#define NEW_NODE(C, T) (T *)P_ALLOC(C, NULL, 0, sizeof(T))
#define IR_ERROR(C_, Kind_, Modno_, ...) pawErr_##Kind_(C_, ModuleInfo_get((C_)->modinfo, Modno_).name, __VA_ARGS__)

#define TODO (struct SourceLoc){0}

void pawIr_set_def_kind(struct Compiler *C, DeclId did, enum IrDefKind kind)
{
    IrDefKinds_insert(C, C->ir_def_kinds, did, kind);
}

IrTrait *pawIr_new_trait(struct Compiler *C, DeclId did, IrTypeList *types)
{
    IrTrait *trait = NEW_NODE(C, IrTrait);
    *trait = (IrTrait){
        .types = types,
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

IrType *pawIr_new_int(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Int_ = (struct IrInt){
        .kind = kIrInt,
    };
    return t;
}

IrType *pawIr_new_float(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Float_ = (struct IrFloat){
        .kind = kIrFloat,
    };
    return t;
}

IrType *pawIr_new_str(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Str_ = (struct IrStr){
        .kind = kIrStr,
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

IrType *pawIr_new_adt(struct Compiler *C, DeclId did, IrTypeList *types)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Adt_ = (struct IrAdt){
        .kind = kIrAdt,
        .did = did,
        .types = types,
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

IrType *pawIr_new_signature(struct Compiler *C, DeclId did, IrTypeList *types)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Signature_ = (struct IrSignature){
        .kind = kIrSignature,
        .did = did,
        .types = types,
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

IrType *pawIr_new_never(struct Compiler *C)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Never_ = (struct IrNever){
        .kind = kIrNever,
    };
    return t;
}

IrType *pawIr_new_infer(struct Compiler *C, int depth, int index)
{
    IrType *t = NEW_NODE(C, IrType);
    t->Infer_ = (struct IrInfer){
        .kind = kIrInfer,
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

struct IrGenericDef *pawIr_new_generic_def(struct Compiler *C, DeclId did, Str *name, struct IrTraitList *bounds)
{
    struct IrGenericDef *def = (struct IrGenericDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrGenericDef){
        .did = did,
        .name = name,
        .bounds = bounds,
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

struct IrVariantDef *pawIr_new_variant_def(struct Compiler *C, DeclId did, DeclId cons_did, DeclId base_did, int discr, Str *name, struct IrFieldDefs *fields)
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

struct IrFnDef *pawIr_new_fn_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, IrType *result, struct IrParams *params, IrType *context, DeclId parent, paw_Bool is_pub)
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

struct IrAdtDef *pawIr_new_adt_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, struct IrVariantDefs *variants, paw_Bool is_pub, paw_Bool is_struct, paw_Bool is_inline)
{
    struct IrAdtDef *def = (struct IrAdtDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrAdtDef){
        .did = did,
        .generics = generics,
        .variants = variants,
        .is_inline = is_inline,
        .is_struct = is_struct,
        .is_pub = is_pub,
        .name = name,
    };
    pawIr_set_def_kind(C, did, IR_ADT_DEF);
    return def;
}

struct IrTraitDef *pawIr_new_trait_def(struct Compiler *C, DeclId did, Str *name, struct IrGenericDefs *generics, struct IrTypeList *methods, paw_Bool is_pub)
{
    struct IrTraitDef *def = (struct IrTraitDef *)P_ALLOC(C, NULL, 0, sizeof(*def));
    *def = (struct IrTraitDef){
        .did = did,
        .generics = generics,
        .methods = methods,
        .is_pub = is_pub,
        .name = name,
    };
    pawIr_set_def_kind(C, did, IR_TRAIT_DEF);
    return def;
}

struct IrImpl *pawIr_new_impl(struct Compiler *C, DeclId did, IrType *type, IrTrait *trait, struct IrGenericDefs *generics, struct IrTypeList *methods)
{
    struct IrImpl *impl = (struct IrImpl *)P_ALLOC(C, NULL, 0, sizeof(*impl));
    *impl = (struct IrImpl){
        .did = did,
        .type = type,
        .trait = trait,
        .generics = generics,
        .methods = methods,
    };
    pawIr_set_def_kind(C, did, IR_IMPL_DEF);
    return impl;
}

IrType *pawIr_get_type(struct Compiler *C, NodeId id)
{
    IrType *const *ptype = HirTypeMap_get(C, C->hir_types, id);
    return ptype != NULL ? *ptype : NULL;
}

void pawIr_set_type(struct Compiler *C, NodeId id, IrType *type)
{
    paw_assert(type != NULL);
    HirTypeMap_insert(C, C->hir_types, id, type);
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

IrTrait *pawIr_get_trait(struct Compiler *C, DeclId did)
{
    return pawIr_new_trait(C, did, pawIr_get_generic_types(C, did));
}

// TODO: use pawP_find_method instead of calling this function, which doesn't handle "multiple applicable methods"
IrType *pawIr_resolve_trait_method(struct Compiler *C, struct IrGeneric *target, Str *name)
{
    IrTraitList *bounds = pawIr_get_trait_bounds(C, target->did);

    if (bounds == NULL) {
        struct HirGenericDecl const *d = HirGetGenericDecl(pawHir_get_decl(C->hir, target->did));
        IR_ERROR(C, missing_trait_bounds, (int)d->did.modno, d->span.start, d->ident.name->text);
    }

    K_LIST_XFOREACH (bounds, IrTrait *const, b) {
        struct IrTraitDef const *bound = pawIr_get_trait_def(C, (*b)->did);
        K_LIST_XFOREACH (bound->methods, IrType *const, m) {
            struct IrFnDef const *fn = pawIr_get_fn_def(C, IR_TYPE_DID(*m));
            if (pawS_eq(fn->name, name)) {
                IrType *type = pawIr_solver_instantiate_type(C->S, fn->did);
                IrType *type_ctx = pawIr_get_context(C, type);
                IrTrait *trait_ctx = pawIr_get_trait_context(C, type);
                pawIr_unify_traits_unchecked(C, trait_ctx, *b);
                pawU_unify_unchecked(C->U, type_ctx, (IrType *)target);
                return type;
            }
        }
    }
    return NULL;
}

enum IrDefKind pawIr_get_kind(struct Compiler *C, DeclId did)
{
    return *IrDefKinds_get(C, C->ir_def_kinds, did);
}

IrTypeList *pawIr_get_generic_types(struct Compiler *C, DeclId did)
{
    IrTypeList *const *p = IrGenericTypes_get(C, C->ir_generic_types, did);
    return p != NULL ? *p : NULL;
}

IrTraitList *pawIr_get_trait_bounds(struct Compiler *C, DeclId did)
{
    IrTraitList *const *p = IrTraitBounds_get(C, C->ir_trait_bounds, did);
    return p != NULL ? *p : NULL;
}

void pawIr_set_generic_types(struct Compiler *C, DeclId did, IrTypeList *types)
{
    paw_Bool const exists = IrGenericTypes_insert(C, C->ir_generic_types, did, types);
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
        return IrTypeList_first(IR_TYPE_SUBTYPES_(fn));

    IrType *parent;
    if (parent_kind == IR_IMPL_DEF) {
        struct IrImpl const *impl_def = pawIr_get_impl_def(C, parent_did);
        parent = impl_def->type;
    } else {
        paw_assert(parent_kind == IR_ADT_DEF);
        parent = pawIr_get_def_type(C, parent_did);
    }

    IrTypeList *params = pawIr_get_generic_types(C, IR_TYPE_DID(fn));
    if (params == NULL) return parent;

    IrTypeList *args = IR_TYPE_SUBTYPES_(fn);
    struct Substitution const subst = {params, args};
    return pawP_substitute(C, TODO, parent, subst);
}

IrTrait *pawIr_get_trait_context(struct Compiler *C, IrType *fn)
{
    struct IrFnDef const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(fn));
    if (!DECL_ID_EXISTS(fn_def->parent)) return NULL;
    return pawIr_solver_instantiate_trait_with(C->S,
            fn_def->parent, IR_TYPE_SUBTYPES_(fn));
}


static paw_Uint hash_type(IrType *type);

static paw_Uint hash_type_list(IrTypeList *types)
{
    paw_Uint hash = 0;
    IrType **ptype;
    if (types != NULL) {
        K_LIST_FOREACH (types, ptype)
            hash = hash_combine(hash, hash_type(*ptype));
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
        case kIrInt:
        case kIrFloat:
        case kIrStr:
            break;
        case kIrPtr: {
            struct IrPtr const *t = IrGetPtr(type);
            hash = hash_combine(hash, hash_type(t->pointee));
            break;
        }
        case kIrAdt: {
            struct IrAdt const *t = IrGetAdt(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_type_list(t->types));
            break;
        }
        case kIrFnPtr: {
            struct IrFnPtr const *t = IrGetFnPtr(type);
            hash = hash_combine(hash, hash_type_list(t->params));
            hash = hash_combine(hash, hash_type(t->result));
            break;
        }
        case kIrSignature: {
            struct IrSignature const *t = IrGetSignature(type);
            hash = hash_combine(hash, t->did.value);
            hash = hash_combine(hash, hash_type_list(t->types));
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
        default:
            paw_assert(IrIsNever(type));
            hash = hash_combine(hash, 0x21); // '!'
            break;
    }
    return hash;
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
    if (!sa->types != !sb->types) { // TODO: should not be necessary, here to patch some bug I need to fix...
        return PAW_TRUE; // TODO: whenever this happens, one has empty .types_ and the other has .types_=NULL
    } // TODO: should not be necessary
    if (sa->types == NULL) return PAW_TRUE;
    paw_assert(sb->types != NULL);
    return typelist_equals(C, sa->types, sb->types);
}

// TODO: probably should distinguish between pawIr_type_equals where we care about function names
//       and pawIr_type_equals where we do not (in the latter case, "fn assert(bool)" is the same
//       as "fn(bool)", like what happens during type unification)
paw_Bool pawIr_type_equals(struct Compiler *C, IrType *a, IrType *b)
{
    if (IR_KINDOF(a) != IR_KINDOF(b))
        return PAW_FALSE;

    if (IrIsSignature(a))
        return sig_equals_extra(C, a, b);

    return pawU_equals(C->U, a, b);
}

paw_Uint pawIr_type_hash(struct Compiler *C, IrType *t)
{
    PAW_UNUSED(C);
    return hash_type(t);
}

paw_Uint pawIr_trait_hash(struct Compiler *C, IrTrait *trait)
{
    PAW_UNUSED(C);
    paw_Uint hash = 0x42;
    hash = hash_combine(hash, trait->did.value);
    hash = hash_combine(hash, hash_type_list(trait->types));
    return hash;
}


IrType *pawIr_materialize_fn(struct Compiler *C, DeclId did, IrTypeList *type_args)
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

    IrTypeList *type_params = pawIr_get_generic_types(C, did);
    struct Substitution const subst = {type_params, type_args};

    K_LIST_XFOREACH (params, IrType *, p)
        *p = pawP_substitute(C, TODO, *p, subst);
    result = pawP_substitute(C, TODO, result, subst);

    return pawIr_new_fn_ptr(C, params, result);
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

static void print_binder(struct Printer *P, IrTypeList *binder)
{
    // TODO: be consistent semantics: does `.binder = NULL` or `.binder.count = 0` mean monomorphic
    if (binder != NULL && binder->count > 0) {
        PRINT_CHAR(P, '<');
        P->print_bounds = PAW_TRUE;
        print_type_list(P, binder);
        P->print_bounds = PAW_FALSE;
        PRINT_CHAR(P, '>');
    }
}

static void print_trait(struct Printer *P, IrTrait *t)
{
    struct IrTraitDef const *def = pawIr_get_trait_def(P->C, t->did);
    PRINT_STRING(P, def->name);
    if (t->types != NULL) {
        PRINT_CHAR(P, '<');
        print_type_list(P, t->types);
        PRINT_CHAR(P, '>');
    }
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
            PRINT_LITERAL(P, "int");
            break;
        case kIrFloat:
            PRINT_LITERAL(P, "float");
            break;
        case kIrStr:
            PRINT_LITERAL(P, "str");
            break;
        case kIrPtr: {
            struct IrPtr *ptr = IrGetPtr(type);
            PRINT_CHAR(P, '&');
            print_type(P, ptr->pointee);
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
        case kIrSignature: {
            struct IrSignature *fsig = IrGetSignature(type);
            enum IrDefKind const kind = pawIr_get_kind(P->C, fsig->did);
            if (kind == IR_FN_DEF) {
                struct IrFnDef *def = pawIr_get_fn_def(P->C, fsig->did);
                PRINT_STRING(P, def->name);
                print_binder(P, fsig->types);
                IrType *fn = pawIr_materialize_fn(P->C, fsig->did, fsig->types);
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
            // TODO: get IR generic def, not HIR decl, which may not exist anymore
            struct IrGeneric *gen = IrGetGeneric(type);
            struct HirDecl *decl = pawHir_get_decl(P->C->hir, gen->did);
            PRINT_STRING(P, HirGetGenericDecl(decl)->ident.name);
            break;
        }
        case kIrInfer: {
            PRINT_CHAR(P, '_');
            break;
        }
        case kIrNever:
            PRINT_CHAR(P, '!');
            break;
        case kIrAdt: {
            struct IrAdt *adt = IrGetAdt(type);
            const enum BuiltinKind code = pawP_type2code(P->C, type);
            if (code == BUILTIN_LIST) {
                PRINT_CHAR(P, '[');
                print_type(P, IrTypeList_get(adt->types, 0));
                PRINT_CHAR(P, ']');
            } else if (code == BUILTIN_MAP) {
                PRINT_CHAR(P, '[');
                print_type(P, IrTypeList_get(adt->types, 0));
                PRINT_LITERAL(P, ": ");
                print_type(P, IrTypeList_get(adt->types, 1));
                PRINT_CHAR(P, ']');
            } else {
                struct IrAdtDef *def = pawIr_get_adt_def(P->C, adt->did);
                PRINT_STRING(P, def->name);
                print_binder(P, adt->types);
            }
            break;
        }
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

