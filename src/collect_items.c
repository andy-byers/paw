// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// collect_items.c: Collection of toplevel items
//
// This pass collects all ADT and trait definitions, as well as type aliases,
// impl blocks, and free functions. Function bodies are not checked here (that
// happens in typeck.c).
//
// A few important analyses happen in this file. Of particular note is
// constraint checking on trait impl blocks. The header of a trait impl block
// has form `impl Trait for Type`. When validating such a header we first
// instantiate `Trait` with `Self` type equal to `Type`. ...talk about adding constraints from assoc. items

#include "compile.h"
#include "debug.h"
#include "error.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "resolve.h"
#include "solve.h"
#include "type_folder.h"
#include "unify.h"

#define CSTR(X, i) CACHED_STRING(ENV(X), CAST_SIZE(i))
#define GET_TYPE(X, id) pawIr_get_type((X)->C, id)
#define SET_TYPE(X, id, t) pawIr_set_type((X)->C, id, t)

#define COLLECTOR_ERROR(X_, Kind_, ...) THROW_ERROR((X_)->C, \
        Kind_, .modname = (X_)->pm->name, __VA_ARGS__)

#define WITH_CONTEXT(X_, Type_, Binder_, Code_) \
    do { \
        (X_)->ctx = (Type_); \
        (X_)->binder = (Binder_); \
        Code_ \
        (X_)->binder = NULL; \
        (X_)->ctx = NULL; \
    } while (0)

struct ItemCollector {
    struct HirModule const *pm;
    struct Compiler *C;
    struct Pool *pool;
    TypeCollection *cache;
    IrGenericArgs *binder;
    IrType *ctx;
    struct Hir *hir;
    paw_Env *P;
    paw_Bool in_trait_decl;
};

static enum BuiltinKind builtin_kind(struct ItemCollector *X, IrType *type)
{
    return pawP_type2code(X->C, type);
}

static IrType *collect_type(struct ItemCollector *X, struct HirType *type)
{
    return pawP_lower_type(X->C, *X->pm, type);
}

static IrTypeList *collect_types(struct ItemCollector *X, struct HirTypeList *types)
{
    if (types == NULL) return NULL;
    IrTypeList *result = IrTypeList_new(X->C);

    struct HirType *const *ptype;
    K_LIST_FOREACH (types, ptype) {
        IrType *type = collect_type(X, *ptype);
        IrTypeList_push(X->C, result, type);
    }
    return result;
}

static Str const *print_type(struct ItemCollector *X, IrType *type)
{
    return pawIr_print_type_v2(X->C, type);
}

static Str const *print_trait(struct ItemCollector *X, IrTrait *trait)
{
    return pawIr_print_trait_v2(X->C, trait);
}

static void add_predicates_from(struct ItemCollector *X, DeclId did)
{
    IrGenericArgs *params = pawIr_get_generic_args(X->C, did);
    pawIr_solver_add_predicates_from(X->C->S, did, params);
}

static DeclId resolve_trait_segment(struct ItemCollector *X, struct HirSegment s)
{
    NodeId const id = SegmentTable_get(X->C, X->C->segtab, s.id)->id;
    struct HirDecl const *decl = pawHir_get_node(X->hir, id);
    return decl->hdr.did;
}

static void collect_trait_args(struct ItemCollector *X, HirGenericArgs *args, IrGenericArgs *result)
{
    K_LIST_XFOREACH (args, struct HirGenericArg const, p) {
        if (p->item == NULL) {
            IrGenericArg const arg = pawP_lower_generic_arg(X->C, *X->pm, *p);
            IrGenericArgs_push(X->C, result, arg);
        }
    }
}

static IrTrait *collect_trait_path(struct ItemCollector *X, struct HirPath path, IrType *self)
{
    struct HirSegment const last = HirSegments_last(path.segments);
    DeclId const did = resolve_trait_segment(X, last);
    if (pawIr_get_kind(X->C, did) != IR_TRAIT_DEF)
        COLLECTOR_ERROR(X, ExpectedTrait,
                .path = last.ident.name,
                .span = last.span);

    IrGenericArgs *args = IrGenericArgs_new(X->C);
    IrGenericArgs_push(X->C, args, IrGenericArg_from_type(self));
    if (last.args != NULL) {
        struct IrTraitDef const *def = pawIr_get_trait_def(X->C, did);
        collect_trait_args(X, last.args, args);
        if (def->generics->count == 1)
            COLLECTOR_ERROR(X, UnexpectedTypeArguments,
                    .what = SCAN_STR(X->C, "trait"),
                    .name = def->name,
                    .span = last.span);
        if (args->count != def->generics->count)
            COLLECTOR_ERROR(X, IncorrectTypeArity,
                    // `n - 1` to exclude implicit `Self`
                    .want = def->generics->count - 1,
                    .have = args->count - 1,
                    .span = last.span);
    }
    return pawIr_new_trait(X->C, did, args);
}

// Collect the type equality constraints from obligations of form "T: Trait<Type = T2>"
static void collect_equals_constraints(struct ItemCollector *X, struct HirGenericArgs *args, IrType *self, IrTrait *trait, IrConstraints *result)
{
    K_LIST_XFOREACH (args, struct HirGenericArg const, p) {
        if (p->item != NULL) {
            paw_assert(p->is_type);
            IrType *rhs = collect_type(X, p->t);
            struct HirGenericDecl const *d = pawHir_get_node(X->hir, p->target);
            IrType *lhs = pawIr_new_projection(X->C, self, trait, d->did);
            IrConstraints_push(X->C, result, (struct IrConstraint){
                        .kind = IR_CONSTRAINT_TYPE_EQUALS,
                        .parent = d->did,
                        .eq.lhs = lhs,
                        .eq.rhs = rhs,
                    });
        }
    }
}

static void solve_all_obligations(struct ItemCollector *X)
{
    struct IrSolverResult const result = pawIr_solver_solve(X->C->S);
    switch (result.status) {
        case IR_SOLVER_SOLVED:
            break;
        case IR_SOLVER_AMBIGUOUS: {
            struct IrObligation const example = pawIr_solver_first_obligation(X->C->S);
            COLLECTOR_ERROR(X, UnsatisfiedObligation,
                    .example = pawIr_print_obligation_(X->C, example),
                    .num_unsolved = result.ambiguous.num_unsolved,
                    .span = example.cause.span);
            }
        case IR_SOLVER_ERROR:
            COLLECTOR_ERROR(X, FalseObligation,
                    .obligation = pawIr_print_obligation_(X->C, result.error.obligation),
                    .span = result.error.obligation.cause.span);
    }
}

static void set_def_type(struct ItemCollector *X, DeclId did, IrType *type)
{
    DefTypeMap_insert(X->C, X->C->def_types, did, type);
}

static IrGenericDefs *collect_generic_defs(struct ItemCollector *X, struct HirDeclList *generics)
{
    IrGenericDefs *result = IrGenericDefs_new(X->C);
    if (generics != NULL) {
        K_LIST_XFOREACH (generics, struct HirDecl *const, p) {
            struct IrGenericDef *r;
            struct HirGenericDecl const *d = HirGetGenericDecl(*p);
            if (d->is_type) {
                r = pawIr_new_generic_type_def(X->C, d->did, d->t.ident.name, NULL);
            } else {
                IrType *type = collect_type(X, d->k.type);
                if (!IrIsBool(type) && !IrIsChar(type)
                        && !IrIsInt(type) && !IrIsFloat(type))
                    pawErr_generic_error(ENV(X), X->pm->name, d->span,
                            "const generic must be scalar");
                r = pawIr_new_generic_const_def(X->C, d->did, type, d->k.ident.name);
            }
            GenericDefMap_insert(X->C, X->C->generic_defs, d->did, r);
            IrGenericDefs_push(X->C, result, r);

            IrType *type = pawIr_new_generic(X->C, d->did);
            set_def_type(X, d->did, type);
            SET_TYPE(X, d->id, type);
        }
    }
    return result;
}

static struct IrFieldDefs *collect_field_defs(struct ItemCollector *X, struct HirDeclList *fields)
{
    struct HirDecl *const *pdecl;
    struct IrFieldDefs *result = IrFieldDefs_new(X->C);
    if (fields == NULL) return result;

    K_LIST_FOREACH (fields, pdecl) {
        struct HirFieldDecl const *d = HirGetFieldDecl(*pdecl);
        IrType *type = collect_type(X, d->tag);
        SET_TYPE(X, d->id, type);

        struct IrFieldDef *def = pawIr_new_field_def(X->C, d->did, d->ident.name, d->is_pub);
        IrFieldDefs_push(X->C, result, def);
        set_def_type(X, d->did, type);
    }
    return result;
}

static struct IrParams *collect_parameters(struct ItemCollector *X, struct HirDeclList *params)
{
    struct HirDecl *const *pdecl;
    struct IrParams *result = IrParams_new(X->C);
    K_LIST_FOREACH (params, pdecl) {
        struct HirParamDecl const *d = HirGetParamDecl(*pdecl);
        IrType *type = collect_type(X, d->tag);
        IrParams_push(X->C, result, (struct IrParam){
                    .name = d->ident.name,
                    .type = type,
                });
    }
    return result;
}

static void transfer_fn_annotations(struct ItemCollector *X, struct HirFnDecl *d, struct IrFnDef *def)
{
    struct Compiler *C = X->C;
    struct Annotations *annos = d->annos;
    def->annos = Annotations_new(C);

    if (annos != NULL) {
        struct Annotation *panno;
        K_LIST_FOREACH (annos, panno) {
            if (pawS_eq(panno->name, CSTR(C, CSTR_EXTERN))) {
                // Found "extern" annotation. Function implementation will be provided
                // by linker.
                if (d->body != NULL)
                    COLLECTOR_ERROR(X, ExternFunctionBody,
                            .fn_name = d->ident.name,
                            .span = d->span);
                def->is_extern = PAW_TRUE;
            } else {
                Annotations_push(C, def->annos, *panno);
            }
        }
    }
}

static struct IrVariantDefs *create_struct_variant(struct ItemCollector *X, struct HirIdent ident, struct HirDeclList *decls)
{
    paw_assert(decls->count == 1);
    struct HirVariantDecl *v = HirGetVariantDecl(K_LIST_FIRST(decls));
    struct IrFieldDefs *fields = collect_field_defs(X, v->fields);
    struct IrVariantDef *r = pawIr_new_variant_def(X->C, v->did, NO_DECL, v->base_did, 0, ident.name, fields);
    struct IrVariantDefs *variants = IrVariantDefs_new(X->C);
    VariantDefMap_insert(X->C, X->C->variant_defs, v->did, r);
    IrVariantDefs_push(X->C, variants, r);
    return variants;
}

static struct IrVariantDefs *collect_variant_defs(struct ItemCollector *X, struct HirAdtDecl *adt)
{
    if (adt->is_struct) return create_struct_variant(X, adt->ident, adt->variants);
    struct IrVariantDefs *variants = IrVariantDefs_new(X->C);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (adt->variants, pdecl) {
        struct HirVariantDecl *d = HirGetVariantDecl(*pdecl);
        struct IrFieldDefs *fields = collect_field_defs(X, d->fields);
        struct IrVariantDef *r = pawIr_new_variant_def(X->C, d->did, NO_DECL, d->base_did, d->index, d->ident.name, fields);
        VariantDefMap_insert(X->C, X->C->variant_defs, d->did, r);
        IrVariantDefs_push(X->C, variants, r);

        IrType *type = pawIr_get_type(X->C, d->id);
        set_def_type(X, d->did, type);
    }
    return variants;
}

static paw_Bool is_self_param(struct ItemCollector *X, struct HirDecl *decl)
{
    struct HirParamDecl *param = HirGetParamDecl(decl);
    return pawS_eq(param->ident.name, CSTR(X, CSTR_SELF));
}

static void collect_param_types(struct ItemCollector *X, struct HirDeclList *params)
{
    if (params == NULL) return;
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (params, pdecl) {
        struct HirParamDecl *d = HirGetParamDecl(*pdecl);
        IrType *type = collect_type(X, d->tag);
        SET_TYPE(X, d->id, type);
    }
}

static void collect_field_types(struct ItemCollector *X, struct HirDeclList *fields)
{
    if (fields == NULL) return;
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (fields, pdecl) {
        struct HirFieldDecl *d = HirGetFieldDecl(*pdecl);
        SET_TYPE(X, d->id, collect_type(X, d->tag));
    }
}

static void collect_variant_type(struct ItemCollector *X, struct HirVariantDecl *d, paw_Bool is_struct)
{
    struct IrVariantDef *def = pawIr_get_variant_def(X->C, d->did);
    collect_field_types(X, d->fields);

    if (is_struct) {
        SET_TYPE(X, d->id, X->ctx);
        def->cons_did = NO_DECL;
    } else {
        IrType *type = pawIr_new_signature(X->C, d->did, X->binder);
        SET_TYPE(X, d->id, type);
        def->cons_did = d->did;
    }

    IrGenericArgs *generics = IR_GENERIC_ARGS(X->ctx);
    if (generics != NULL)
        pawIr_set_generic_args(X->C, d->did, generics);
}

static void collect_variant_types(struct ItemCollector *X, struct HirDeclList *variants, paw_Bool is_struct)
{
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (variants, pdecl) {
        struct HirVariantDecl *d = HirGetVariantDecl(*pdecl);
        collect_variant_type(X, d, is_struct);
    }
}

static void start_module(struct ItemCollector *X, struct HirModule const *pm)
{
    X->pm = pm;
}

static void finish_module(struct ItemCollector *X)
{
    X->pm = NULL;
}

static IrGenericArgs *collect_generic_args(struct ItemCollector *X, DeclId parent_did, struct HirDeclList *generics)
{
    IrGenericArgs *types = IrGenericArgs_new(X->C);
    if (generics != NULL) {
        // Create a local symbol for each generic. Generic bounds are registered
        // in a later pass, once all nominal types are known.
        struct HirDecl *const *pdecl;
        K_LIST_FOREACH (generics, pdecl) {
            IrType *type;
            IrGenericArg arg;
            struct HirGenericDecl const *d = HirGetGenericDecl(*pdecl);
            if (d->is_type) {
                type = pawIr_new_generic(X->C, d->did);
                arg = IrGenericArg_from_type(type);
            } else {
                type = collect_type(X, d->k.type);
                IrConst *konst = pawIr_new_const_decl(X->C, d->did);
                arg = IrGenericArg_from_const(konst);
            }
            IrDeclArgs_insert(X->C, X->C->ir_decl_args, d->did, arg);
            IrGenericArgs_push(X->C, types, arg);
            SET_NODE_TYPE(X->C, *pdecl, type);
            set_def_type(X, d->did, type);
        }
    }
    pawIr_set_generic_args(X->C, parent_did, types);
    return types;
}

static void collect_bound_traits(struct ItemCollector *X, struct HirGenericDecl const *d, IrConstraints *result)
{
    IrType *generic = pawIr_get_def_type(X->C, d->did);
    if (d->t.bounds != NULL) {
        IrTraitList *bounds = IrTraitList_new(X->C);
        K_LIST_XFOREACH (d->t.bounds, struct HirGenericBound const, pbound) {
            struct HirSegment const last = HirSegments_last(pbound->path.segments);
            DeclId const did = resolve_trait_segment(X, last);

            IrGenericArgs *args = IrGenericArgs_new(X->C);
            IrGenericArgs_push(X->C, args, IrGenericArg_from_type(generic));
            IrTrait *trait = pawIr_new_trait(X->C, did, args);
            IrTraitList_push(X->C, bounds, trait);
            IrConstraints_push(X->C, result, (struct IrConstraint){
                        .kind = IR_CONSTRAINT_IMPL_TRAIT,
                        .parent = d->did,
                        .impl.type = generic,
                        .impl.trait = trait,
                    });
        }
        pawIr_set_trait_bounds(X->C, d->did, bounds);
    }
}

static void collect_bound_args(struct ItemCollector *X, struct HirGenericDecl const *d, IrConstraints *result)
{
    IrType *generic = pawIr_get_def_type(X->C, d->did);
    if (d->t.bounds != NULL) {
        IrTraitList *bounds = pawIr_get_trait_bounds(X->C, d->did);

        IrTrait *const *ptrait;
        struct HirGenericBound const *pbound;
        K_LIST_ZIP (d->t.bounds, pbound, bounds, ptrait) {
            IrTrait *trait = *ptrait;
            struct HirSegment const last = HirSegments_last(pbound->path.segments);
            if (last.args != NULL) {
                collect_equals_constraints(X, last.args, generic, trait, result);
                collect_trait_args(X, last.args, trait->args);
            }
        }
    }
}

static void collect_generic_bounds(struct ItemCollector *X, struct HirDeclList *generics, IrConstraints *result)
{
    if (generics != NULL) {
        // Collection of generic bounds must be performed in 2 passes since traits may have as
        // their generic arguments associated types from other generics, i.e. `T: Trait<T2::Type>`.
        // The first pass creates an `IrTrait` object for each generic type, with `Self` bound to
        // the type of the generic, allowing the second pass to look up trait bounds on other
        // generic arguments in order to to resolve the identity of the associated type.

        K_LIST_XFOREACH (generics, struct HirDecl *const, pdecl) {
            struct HirGenericDecl const *d = HirGetGenericDecl(*pdecl);
            if (d->is_type) collect_bound_traits(X, d, result);
        }
        K_LIST_XFOREACH (generics, struct HirDecl *const, pdecl) {
            struct HirGenericDecl const *d = HirGetGenericDecl(*pdecl);
            if (d->is_type) collect_bound_args(X, d, result);
        }
    }
}

static paw_Bool is_builtin(struct ItemCollector *X, DeclId did, enum BuiltinKind kind)
{
    return did.value == pawP_builtin_info(X->C, kind)->did.value;
}

static IrType *collect_type_decl(struct ItemCollector *X, struct HirTypeDecl const *d)
{
    IrType *type = collect_type(X, d->rhs);
    set_def_type(X, d->did, type);
    SET_TYPE(X, d->id, type);
    return type;
}

static void collect_adt_def(struct ItemCollector *X, struct HirAdtDecl const *d)
{
    IrType *type;
    if (is_builtin(X, d->did, BUILTIN_UNIT)) {
        type = pawIr_new_unit(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_BOOL)) {
        type = pawIr_new_bool(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_CHAR)) {
        type = pawIr_new_char(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_INT)) {
        type = pawIr_new_int(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_FLOAT)) {
        type = pawIr_new_float(X->C);
    } else if (is_builtin(X, d->did, BUILTIN_STR)) {
        type = pawIr_new_string(X->C);
    } else {
        IrGenericArgs *args = collect_generic_args(X, d->did, d->generics);
        type = pawIr_new_adt(X->C, d->did, args);

        IrGenericDefs *generics = collect_generic_defs(X, d->generics);
        struct IrAdtDef *r = pawIr_new_adt_def(X->C, d->did, d->ident.name,
                generics, NULL, d->is_pub, d->is_struct);
        AdtDefMap_insert(X->C, X->C->adt_defs, d->did, r);
    }

    set_def_type(X, d->did, type);
    SET_TYPE(X, d->id, type);
}

static void collect_fn_def(struct ItemCollector *X, struct HirFnDecl const *d)
{
    collect_generic_args(X, d->did, d->generics);
    IrGenericDefs *generics = collect_generic_defs(X, d->generics);
    struct IrFnDef *r = pawIr_new_fn_def(X->C, d->did, d->ident.name,
            generics, NULL, NULL, NULL, NO_DECL, d->is_pub);
    FnDefMap_insert(X->C, X->C->fn_defs, d->did, r);
}

static void collect_trait_def(struct ItemCollector *X, struct HirTraitDecl const *d)
{
    X->in_trait_decl = PAW_TRUE;
    IrGenericArgs *args = collect_generic_args(X, d->did, d->generics);
    IrTrait *trait = pawIr_new_trait(X->C, d->did, args);
    IrType *self = IrGenericArg_get_type(IrGenericArgs_first(args));

    IrAssocItems *items = IrAssocItems_new(X->C);
    K_LIST_XFOREACH (d->types, struct HirDecl *const, p) {
        struct HirGenericDecl const *t = HirGetGenericDecl(*p);
        paw_assert(t->is_type);
        struct IrAssocItem *item = pawIr_new_assoc_item(X->C, t->did, t->t.ident.name, d->did, d->is_pub);
        IrType *item_type = pawIr_new_projection(X->C, self, trait, t->did);
        IrAssocItemMap_insert(X->C, X->C->ir_assoc_items, t->did, item);
        IrAssocItems_push(X->C, items, item);
        set_def_type(X, t->did, item_type);
        SET_TYPE(X, t->id, item_type);
    }

    K_LIST_XFOREACH (d->methods, struct HirDecl *const, p)
        collect_fn_def(X, HirGetFnDecl(*p));

    IrGenericDefs *generics = collect_generic_defs(X, d->generics);
    struct IrTraitDef *r = pawIr_new_trait_def(X->C, d->did,
            d->ident.name, generics, NULL, items, d->is_pub);
    TraitDefMap_insert(X->C, X->C->trait_defs, d->did, r);
    X->in_trait_decl = PAW_FALSE;
}

static void record_impl_block(struct ItemCollector *X, IrDefs *impls, DeclId did)
{
    IrDefs_push(X->C, impls, did);
}

static void collect_impl_def(struct ItemCollector *X, struct HirImplDecl const *d)
{
    collect_generic_args(X, d->did, d->generics);
    IrGenericDefs *generics = collect_generic_defs(X, d->generics);

    IrType *type = collect_type(X, d->type);
    SET_TYPE(X, d->id, type);

    IrAssocItems *items = IrAssocItems_new(X->C);
    IrTrait *trait = NULL;
    if (d->trait != NULL) {
        struct HirPathType *t = HirGetPathType(d->trait);
        trait = collect_trait_path(X, t->path, type);

        K_LIST_XFOREACH (d->types, struct HirDecl *const, p) {
            struct HirTypeDecl const *t = HirGetTypeDecl(*p);
            struct IrAssocItem *item = pawIr_new_assoc_item(X->C, t->did, t->ident.name, d->did, t->is_pub);
            IrAssocItemMap_insert(X->C, X->C->ir_assoc_items, t->did, item);
            IrAssocItems_push(X->C, items, item);
            IrType *assoc_type = collect_type(X, t->rhs);
            SET_TYPE(X, t->id, assoc_type);
            set_def_type(X, t->did, assoc_type);
        }
    }

    struct IrImpl *impl_def = pawIr_new_impl(X->C, d->did, type, trait,
            generics, NULL, items);
    ImplMap_insert(X->C, X->C->impl_defs, d->did, impl_def);

    K_LIST_XFOREACH (d->methods, struct HirDecl *const, p)
        collect_fn_def(X, HirGetFnDecl(*p));

    if (IrIsGeneric(type)) {
        if (trait == NULL)
            COLLECTOR_ERROR(X, BlanketInherentImpl, d->span);
        // found blanket trait implementation
        IrDefs_push(X->C, X->C->impls.blanket, impl_def->did);
    } else if (trait == NULL) {
        // found inherent implementation
        record_impl_block(X, X->C->impls.inherent, impl_def->did);
    } else {
        // found trait implementation
        record_impl_block(X, X->C->impls.trait, impl_def->did);
    }
}

static void collect_nominal_types(struct ItemCollector *X, struct HirModule m)
{
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p) {
        if (HirIsAdtDecl(*p)) {
            collect_adt_def(X, HirGetAdtDecl(*p));
        } else if (HirIsTraitDecl(*p)) {
            collect_trait_def(X, HirGetTraitDecl(*p));
        } else if (HirIsFnDecl(*p)) {
            struct HirFnDecl const *d = HirGetFnDecl(*p);
            collect_generic_defs(X, d->generics);
        } else if (HirIsTypeDecl(*p)) {
            struct HirTypeDecl const *d = HirGetTypeDecl(*p);
            collect_generic_defs(X, d->generics);
        } else if (HirIsImplDecl(*p)) {
            struct HirImplDecl const *d = HirGetImplDecl(*p);
            collect_generic_defs(X, d->generics);
            K_LIST_XFOREACH (d->methods, struct HirDecl *const, pmethod) {
                struct HirFnDecl const *method = HirGetFnDecl(*pmethod);
                collect_generic_defs(X, method->generics);
            }
        }
    }
}

static void collect_definitions(struct ItemCollector *X, struct HirModule m)
{
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p) {
        if (HirIsImplDecl(*p)) {
            collect_impl_def(X, HirGetImplDecl(*p));
        } else if (HirIsFnDecl(*p)) {
            collect_fn_def(X, HirGetFnDecl(*p));
        }
    }
}

static void collect_constraints_from(struct ItemCollector *X, struct HirDecl *decl)
{
    IrConstraints *constraints = IrConstraints_new(X->C);
    if (HirIsAdtDecl(decl)) {
        struct HirAdtDecl const *d = HirGetAdtDecl(decl);
        collect_generic_bounds(X, d->generics, constraints);
    } else if (HirIsTraitDecl(decl)) {
        struct HirTraitDecl const *d = HirGetTraitDecl(decl);
        collect_generic_bounds(X, d->generics, constraints);
        collect_generic_bounds(X, d->types, constraints);
        K_LIST_XFOREACH (d->methods, struct HirDecl *const, p)
            collect_constraints_from(X, *p);
    } else if (HirIsImplDecl(decl)) {
        struct HirImplDecl const *d = HirGetImplDecl(decl);
        collect_generic_bounds(X, d->generics, constraints);
        K_LIST_XFOREACH (d->methods, struct HirDecl *const, p)
            collect_constraints_from(X, *p);
    } else if (HirIsTypeDecl(decl)) {
        struct HirTypeDecl const *d = HirGetTypeDecl(decl);
        collect_generic_args(X, d->did, d->generics);
//        collect_generic_defs(X, d->generics);
        collect_generic_bounds(X, d->generics, constraints);
    } else if (HirIsFnDecl(decl)) {
        struct HirFnDecl const *d = HirGetFnDecl(decl);
        collect_generic_bounds(X, d->generics, constraints);
    }

    IrConstraintsMap_insert(X->C, X->C->ir_constraints, decl->hdr.did, constraints);
}

static void collect_constraints(struct ItemCollector *X, struct HirModule m)
{
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p)
        collect_constraints_from(X, *p);
}

static void collect_type_aliases(struct ItemCollector *X, struct HirModule m)
{
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p) {
        if (HirIsTypeDecl(*p)) {
            collect_type_decl(X, HirGetTypeDecl(*p));
        }
    }
}

static void ensure_not_recursive(struct ItemCollector *X, IrType *type)
{
    void *const *p = TypeCollection_get(X->C, X->cache, type);
    if (p != NULL)
        COLLECTOR_ERROR(X, TypeContainsSelf, .span = (struct SourceSpan){0});
    TypeCollection_insert(X->C, X->cache, type, NULL);

    if (IrIsAdt(type)) {
        struct IrAdt *t = IrGetAdt(type);
        struct IrAdtDef const *def = pawIr_get_adt_def(X->C, t->did);
        for (int discr = 0; discr < def->variants->count; ++discr) {
            IrTypeList const *fields = pawP_instantiate_variant_fields(X->C, t, discr);
            K_LIST_XFOREACH (fields, IrType *const, p)
                ensure_not_recursive(X, *p);
        }
    } else if (IrIsTuple(type)) {
        struct IrTuple const *t = IrGetTuple(type);
        K_LIST_XFOREACH (t->elems, IrType *const, p)
            ensure_not_recursive(X, *p);
    } else if (IrIsArray(type)) {
        // NOTE: "struct T {v: [T; N]}" is considered recursive even if "N" is
        //   equal to 0. This is a compromise since we don't know the value of
        //   "N" until much later.
        struct IrArray const *t = IrGetArray(type);
        ensure_not_recursive(X, t->type);
    }

    TypeCollection_remove(X->C, X->cache, type);
}

static void ensure_type_is_well_formed(struct ItemCollector *X, struct SourceSpan span, IrType *type)
{
    if (IrIsAdt(type))
        pawIr_solver_add_well_formed_obligation(X->C->S, IR_TYPE_DID(type),
                IR_GENERIC_ARGS(type), (struct IrObligationCause){.span = span});
}

static void ensure_trait_is_well_formed(struct ItemCollector *X, struct SourceSpan span, IrTrait *trait)
{
    pawIr_solver_add_well_formed_obligation(X->C->S, trait->did, trait->args,
            (struct IrObligationCause){.span = span});
}

static void add_predicates_and_obligations(struct ItemCollector *X, DeclId did) // struct HirDeclList *generics)
{
    IrGenericArgs *params = pawIr_get_generic_args(X->C, did);
    pawIr_solver_add_predicates_from(X->C->S, did, params);
//    IrConstraints const *constraints = pawIr_get_constraints(X->C, did);
//    K_LIST_XFOREACH (constraints, struct IrConstraint const, c) {
//        switch (c->kind) {
//            case IR_CONSTRAINT_IMPL_TRAIT: {
//                struct HirGenericDecl const *d = HirGetGenericDecl(
//                        pawHir_get_decl(X->hir, c->parent));
//                ensure_type_is_well_formed(X, d->span, c->impl.type);
//                ensure_trait_is_well_formed(X, d->span, c->impl.trait);
//                pawIr_solver_add_predicate(X->C->S, c->impl.type, c->impl.trait,
//                        (struct IrObligationCause){
//                            .span = d->span,
//                        });
//                break;
//            }
//            case IR_CONSTRAINT_TYPE_EQUALS: {
//                struct HirGenericDecl const *d = HirGetGenericDecl(
//                        pawHir_get_decl(X->hir, c->parent));
//                ensure_type_is_well_formed(X, d->span, c->eq.lhs);
//                ensure_type_is_well_formed(X, d->span, c->eq.rhs);
//                pawIr_solver_add_norm_target(X->C->S, c->eq.lhs, c->eq.rhs,
//                        (struct IrObligationCause){
//                            .span = d->span,
//                        });
//                break;
//            }
//        }
//    }
}

struct AssocItemInfo {
    DeclId did;
    paw_Bool found;
};

DEFINE_MAP(struct ItemCollector, AssocItemInfoMap, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, struct AssocItemInfo)

static AssocItemInfoMap *collect_assoc_types_from_trait(struct ItemCollector *X, DeclId did)
{
    struct IrTraitDef const *trait = pawIr_get_trait_def(X->C, did);
    AssocItemInfoMap *items = AssocItemInfoMap_new(X);
    K_LIST_XFOREACH (trait->items, struct IrAssocItem *const, p) {
        AssocItemInfoMap_insert(X, items, (*p)->name, (struct AssocItemInfo){
                    .did = (*p)->did,
                });
    }
    return items;
}

static AssocItemInfoMap *collect_assoc_fns_from_trait(struct ItemCollector *X, DeclId did)
{
    struct IrTraitDef const *trait = pawIr_get_trait_def(X->C, did);
    AssocItemInfoMap *methods = AssocItemInfoMap_new(X);
    K_LIST_XFOREACH (trait->methods, IrType *const, p) {
        struct IrFnDef const *fn = pawIr_get_fn_def(X->C, IR_TYPE_DID(*p));
        AssocItemInfoMap_insert(X, methods, fn->name, (struct AssocItemInfo){
                    .did = fn->did,
                });
    }
    return methods;
}

static void solve_impl_decl(struct ItemCollector *X, struct HirImplDecl *d)
{
    add_predicates_and_obligations(X, d->did);

    IrType *self = collect_type(X, d->type);
    ensure_type_is_well_formed(X, d->type->hdr.span, self);

    if (d->trait == NULL)
        return;

    struct HirPathType const *trait_path = HirGetPathType(d->trait);
    IrTrait *trait = collect_trait_path(X, trait_path->path, self);
    ensure_trait_is_well_formed(X, trait_path->span, trait);

    struct IrImpl const *impl_def = pawIr_get_impl_def(X->C, d->did);
    K_LIST_XFOREACH (d->types, struct HirDecl *const, decl_ptr) {
        struct HirTypeDecl const *t = HirGetTypeDecl(*decl_ptr);
        IrType *lhs = pawIr_new_projection(X->C, self, trait, t->did);
        IrType *rhs = pawIr_get_def_type(X->C, t->did);
        pawIr_solver_add_norm_target(X->C->S, lhs, rhs,
                (struct IrObligationCause){
                    .span = t->span,
                });
    }

    AssocItemInfoMap *assoc_types = collect_assoc_types_from_trait(X, impl_def->trait->did);
    K_LIST_XFOREACH (d->types, struct HirDecl *const, item_ptr) {
        struct HirTypeDecl const *item = HirGetTypeDecl(*item_ptr);
        struct AssocItemInfo *info = AssocItemInfoMap_get(X, assoc_types, item->ident.name);
        if (info == NULL)
            // found associated type not defined in the trait
            COLLECTOR_ERROR(X, TraitImplUnknownAssocItem,
                    .trait = print_trait(X, impl_def->trait),
                    .item = item->ident.name,
                    .span = item->span);

        if (info->found)
            // found associated type defined more than once
            COLLECTOR_ERROR(X, DuplicateAssocItem,
                    .item = item->ident.name,
                    .span = item->span);

        info->found = PAW_TRUE;
    }

    struct IrTraitDef const *trait_def = pawIr_get_trait_def(X->C, trait->did);
    K_LIST_XFOREACH (trait_def->items, struct IrAssocItem *const, pitem) {
        struct IrAssocItem const *item = *pitem;
        struct AssocItemInfo const *info = AssocItemInfoMap_get(X, assoc_types, item->name);
        if (!info->found) {
            struct IrAssocItem const *item_def = pawIr_get_assoc_item(X->C, info->did);
            struct HirTypeDecl const *missing_type = HirGetTypeDecl(pawHir_get_decl(X->hir, item_def->did));
            struct Module const missing_mod = ModuleInfo_get(X->C->modinfo, (int)missing_type->did.modno);
            COLLECTOR_ERROR(X, TraitImplMissingAssocItem,
                    .trait = print_trait(X, impl_def->trait),
                    .item = item_def->name,
                    .missing_modname = missing_mod.name,
                    .missing_span = missing_type->span,
                    .impl_span = d->span);
        }
    }

    AssocItemInfoMap *assoc_fns = collect_assoc_fns_from_trait(X, impl_def->trait->did);
    K_LIST_XFOREACH (d->methods, struct HirDecl *const, method_decl_ptr) {
        struct HirFnDecl const *method_decl = HirGetFnDecl(*method_decl_ptr);
        // ensure that each method defined in a trait impl block can be found
        // in the corresponding trait definition
        struct IrFnDef const *fn_def = pawIr_get_fn_def(X->C, method_decl->did);
        struct AssocItemInfo *info = AssocItemInfoMap_get(X, assoc_fns, fn_def->name);
        if (info == NULL)
            COLLECTOR_ERROR(X, TraitImplUnknownAssocItem,
                    .trait = print_trait(X, impl_def->trait),
                    .item = fn_def->name,
                    .span = method_decl->span);
        paw_assert(!info->found);
        info->found = PAW_TRUE;

        IrGenericArgs *args = IrGenericArgs_new(X->C);
        {
            IrGenericArgs *trait_args = impl_def->trait->args;
            IrGenericArgs_reserve(X->C, args, trait_args->count + fn_def->generics->count);

            // add type arguments from instantiated trait (already has "Self")
            K_LIST_XFOREACH (trait_args, IrGenericArg const, t)
                IrGenericArgs_push(X->C, args, *t);

            // add type parameters from impl block method
            K_LIST_XFOREACH (fn_def->generics, struct IrGenericDef *const, p)
                IrGenericArgs_push(X->C, args, pawIr_get_def_arg(X->C, (*p)->did));
        }

        // instantiate the trait method with "Self" equal to the "Self" type
        // of the impl block
        add_predicates_from(X, info->did);
        IrType *method = pawIr_new_signature(X->C, info->did, args);
        pawIr_solver_add_obligations_from(X->C->S, info->did, args);
        method = pawU_normalize_projections(X->C->U, method);
        IrType *type_from_impl = pawIr_get_def_type(X->C, method_decl->did);
        if (pawU_unify(X->C->U, method, type_from_impl) != 0)
            COLLECTOR_ERROR(X, TraitImplAssocItemNotCompatible,
                    .trait = print_trait(X, impl_def->trait),
                    .item = fn_def->name,
                    .span = method_decl->span);
    }

    K_LIST_XFOREACH (trait_def->methods, IrType *const, type_ptr) {
        struct IrFnDef const *fn_def = pawIr_get_fn_def(X->C, IR_TYPE_DID(*type_ptr));
        struct AssocItemInfo const *info = AssocItemInfoMap_get(X, assoc_fns, fn_def->name);
        if (!info->found) {
            struct HirFnDecl const *missing_fn = HirGetFnDecl(pawHir_get_decl(X->hir, fn_def->did));
            struct Module const missing_mod = ModuleInfo_get(X->C->modinfo, (int)missing_fn->did.modno);
            COLLECTOR_ERROR(X, TraitImplMissingAssocItem,
                    .trait = print_trait(X, impl_def->trait),
                    .item = fn_def->name,
                    .missing_modname = missing_mod.name,
                    .missing_span = missing_fn->span,
                    .impl_span = d->span);
        }
    }
}

static void solve_signatures(struct ItemCollector *X, struct HirModule m)
{
    K_LIST_XFOREACH (m.items, struct HirDecl *const, p) {
        pawIr_push_solver(X->C);

        if (HirIsAdtDecl(*p)) {
            IrType *type = GET_NODE_TYPE(X->C, *p);
            if (IrIsAdt(type)) { // skip builtin types
                struct HirAdtDecl const *d = HirGetAdtDecl(*p);
                add_predicates_and_obligations(X, d->did);

                X->cache = TypeCollection_new(X->C);
                ensure_not_recursive(X, type);
            }
        } else if (HirIsTraitDecl(*p)) {
            struct HirTraitDecl const *d = HirGetTraitDecl(*p);
            add_predicates_and_obligations(X, d->did);
        } else if (HirIsImplDecl(*p)) {
            solve_impl_decl(X, HirGetImplDecl(*p));
        } else if (HirIsTypeDecl(*p)) {
            struct HirTypeDecl const *d = HirGetTypeDecl(*p);
            add_predicates_and_obligations(X, d->did);
        } else if (HirIsFnDecl(*p)) {
            struct HirFnDecl const *d = HirGetFnDecl(*p);
            add_predicates_and_obligations(X, d->did);
        }

        solve_all_obligations(X);
        pawIr_pop_solver(X->C);
    }
}

// TODO: likely need multiple passes to resolve local type aliases that reference one another, needs tests...
static void collect_local_type_decl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    struct ItemCollector *X = V->ud;
    IrConstraints *constraints = IrConstraints_new(X->C);

    collect_generic_args(X, d->did, d->generics);
    collect_generic_defs(X, d->generics);
    collect_generic_bounds(X, d->generics, constraints);
    IrType *rhs = collect_type_decl(X, d);

    IrConstraintsMap_insert(X->C, X->C->ir_constraints, d->did, constraints);

    pawIr_push_solver(X->C);
    add_predicates_and_obligations(X, d->did);
    ensure_type_is_well_formed(X, d->rhs->hdr.span, rhs);
    solve_all_obligations(X);
    pawIr_pop_solver(X->C);
}

static void collect_local_type_aliases(struct ItemCollector *X, struct HirExpr *block)
{
    struct HirVisitor V;
    pawHir_visitor_init(&V, X->hir, X);
    V.PostVisitTypeDecl = collect_local_type_decl;
    pawHir_visit_expr(&V, block);
}

static IrType *remove_indirection(IrType *type)
{
    while (IrIsPtr(type))
        type = ir_deref(type);
    return type;
}

static void unify_with_self(struct ItemCollector *X, struct SourceSpan span, IrType *self)
{
    IrType *context = remove_indirection(X->ctx);
    self = remove_indirection(self);
    if (pawU_unify(X->C->U, self, context) != 0)
        COLLECTOR_ERROR(X, IncompatibleTypes,
                .lhs = print_type(X, self),
                .rhs = print_type(X, context),
                .span = span);
}

// Ensure that "main" has a signature that can be called by the
// generated driver function
static void validate_main_signature(struct ItemCollector *X, struct SourceSpan span, IrType *type)
{
    struct IrFnPtr *fn = IrGetFnPtr(
            IR_SIGNATURE_FN(X->C, type));
    if (fn->params->count > 0)
        pawErr_exceeded_limit(ENV(X), X->pm->name, span,
                "parameters for \"main\" function", 1);

    if (builtin_kind(X, fn->result) != BUILTIN_UNIT
            && builtin_kind(X, fn->result) != BUILTIN_INT)
        pawErr_generic_error(ENV(X), X->pm->name, span,
                "return type of \"main\" must have type \"()\" or \"int\"");
}

static void collect_fn_decl(struct ItemCollector *X, struct HirFnDecl *d)
{
    struct IrFnDef *fn_def = pawIr_get_fn_def(X->C, d->did);
    IrGenericArgs *generics = pawIr_get_generic_args(X->C, d->did);
    add_predicates_from(X, d->did);
    if (d->body != NULL)
        collect_local_type_aliases(X, d->body);
    collect_param_types(X, d->params);

    IrGenericArgs *args = IrGenericArgs_new(X->C);
    IrTypeList *params = pawHir_collect_decl_types(X->C, d->params);
    IrType *result = collect_type(X, d->result);
    IrType *type = pawIr_new_signature(X->C, d->did, args);
    SET_TYPE(X, d->id, type);

    {
        if (X->binder != NULL) {
            // add generics from parent binder
            K_LIST_XFOREACH (X->binder, IrGenericArg const, p)
                IrGenericArgs_push(X->C, args, *p);
        }
        if (generics != NULL) {
            // add generics from function binder
            K_LIST_XFOREACH (generics, IrGenericArg const, p)
                IrGenericArgs_push(X->C, args, *p);
        }
        // overwrite old "generic types" entry with full binder
        IrGenericTypes_insert(X->C, X->C->ir_generic_args, d->did, args);
    }

    {
        IrParams *params = collect_parameters(X, d->params);
        fn_def->result = result;
        fn_def->params = params;
        fn_def->context = X->ctx;
        // TODO: needs to be set here b/c parent ID is not known until right before this function is called. figure out earlier
        fn_def->parent = d->parent_id;
        transfer_fn_annotations(X, d, fn_def);
        set_def_type(X, d->did, type);
    }

    if (!X->in_trait_decl && d->body == NULL && !fn_def->is_extern)
        COLLECTOR_ERROR(X, MissingFunctionBody, d->span);

    if (X->ctx != NULL) {
        if (d->params->count > 0) {
            struct HirDecl *first = K_LIST_FIRST(d->params);
            if (is_self_param(X, first)) { // make sure "self: Self" is true
                unify_with_self(X, first->hdr.span, K_LIST_FIRST(params));
                HirGetParamDecl(first)->is_self = PAW_TRUE;
            }
        }
    } else if (pawS_eq(d->ident.name, X->C->main_name)) {
        validate_main_signature(X, d->span, type);
    }
}

static void collect_method_decls(struct ItemCollector *X, DeclId parent_id, struct HirDeclList *methods, paw_Bool force_pub)
{
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (methods, pdecl) {
        struct HirFnDecl *d = HirGetFnDecl(*pdecl);
        d->parent_id = parent_id;
        d->is_pub |= force_pub;
        collect_fn_decl(X, d);
    }
}

static void collect_adt_decl(struct ItemCollector *X, struct HirAdtDecl *d)
{
    IrType *type = pawIr_get_def_type(X->C, d->did);
    // skip basic types, e.g. `int`
    if (!IrIsAdt(type)) return;

    struct IrAdtDef *def = pawIr_get_adt_def(X->C, d->did);
    def->variants = collect_variant_defs(X, d);

    WITH_CONTEXT(X, type, IR_GENERIC_ARGS(type),
        collect_variant_types(X, d->variants, d->is_struct);
    );
}

static void collect_impl_decl(struct ItemCollector *X, struct HirImplDecl *d)
{
    struct IrImpl *impl = pawIr_get_impl_def(X->C, d->did);
    IrGenericArgs *binder = pawIr_get_generic_args(X->C, d->did);
    add_predicates_from(X, d->did);

    IrType *type = collect_type(X, d->type);

    paw_Bool force_pub = PAW_FALSE;
    if (d->trait != NULL) {
        struct HirPathType const *t = HirGetPathType(d->trait);
        IrTrait const *trait = collect_trait_path(X, t->path, type);

        // propagate visibility qualifier from trait
        struct HirTraitDecl const *d = HirGetTraitDecl(
                pawHir_get_decl(X->hir, trait->did));
        force_pub = d->is_pub;
    }

    WITH_CONTEXT(X, type, binder,
        collect_method_decls(X, d->did, d->methods, force_pub);
    );

    IrTypeList *methods = IrTypeList_new(X->C);
    K_LIST_XFOREACH (d->methods, struct HirDecl *const, p) {
        struct HirFnDecl const *method = HirGetFnDecl(*p);
        IrType *method_type = pawIr_get_def_type(X->C, method->did);
        IrTypeList_push(X->C, methods, method_type);
    }
    impl->methods = methods;
}

static void collect_trait_decl(struct ItemCollector *X, struct HirTraitDecl *d)
{
    X->in_trait_decl = PAW_TRUE;
    struct IrTraitDef *trait_def = pawIr_get_trait_def(X->C, d->did);
    add_predicates_from(X, d->did);

    IrGenericArgs *params = pawIr_get_generic_args(X->C, d->did);
    IrType *self = IrGenericArg_get_type(IrGenericArgs_first(params));

    WITH_CONTEXT(X, self, params,
        collect_method_decls(X, d->did, d->methods, d->is_pub);
    );

    IrTypeList *methods = IrTypeList_new(X->C);
    K_LIST_XFOREACH (d->methods, struct HirDecl *const, p) {
        struct HirFnDecl *method = HirGetFnDecl(*p);
        IrType *method_type = pawIr_get_def_type(X->C, method->did);
        IrTypeList_push(X->C, methods, method_type);
    }
    trait_def->methods = methods;
    X->in_trait_decl = PAW_FALSE;
}

static void collect_const_decl(struct ItemCollector *X, struct HirConstDecl *d)
{
    SET_TYPE(X, d->id, collect_type(X, d->tag));
}

static void collect_item_defs(struct ItemCollector *X, struct HirModule m)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (m.items, pitem) {
        pawIr_push_solver(X->C);

        switch (HIR_KINDOF(*pitem)) {
            case kHirAdtDecl:
                collect_adt_decl(X, HirGetAdtDecl(*pitem));
                break;
            case kHirImplDecl:
                collect_impl_decl(X, HirGetImplDecl(*pitem));
                break;
            case kHirTraitDecl:
                collect_trait_decl(X, HirGetTraitDecl(*pitem));
                break;
            case kHirFnDecl:
                collect_fn_decl(X, HirGetFnDecl(*pitem));
                break;
            case kHirConstDecl:
                collect_const_decl(X, HirGetConstDecl(*pitem));
                break;
            default:
                paw_assert(HirIsTypeDecl(*pitem));
                break;
        }

        solve_all_obligations(X);
        pawIr_pop_solver(X->C);
    }
}

static void run_collection_phases(struct ItemCollector *X, struct Hir *hir)
{
#define MAP_MODULES(X_, Modules_, Action_) \
        K_LIST_XFOREACH (Modules_, struct HirModule const, p_) { \
            start_module(X_, p_); \
            Action_(X_, *(p_)); \
            finish_module(X_); \
        }

    MAP_MODULES(X, hir->modules, collect_nominal_types);
    MAP_MODULES(X, hir->modules, collect_constraints);
    MAP_MODULES(X, hir->modules, collect_definitions);
    MAP_MODULES(X, hir->modules, collect_type_aliases);
    MAP_MODULES(X, hir->modules, collect_item_defs);
    MAP_MODULES(X, hir->modules, solve_signatures);

    paw_assert(pawIr_solver_num_obligations(X->C->S) == 0);

#undef MAP_MODULES
}

// Entrypoint to item collection
void pawP_collect_items(struct Compiler *C, struct Pool *pool)
{
    struct ItemCollector X = {
        .hir = C->hir,
        .pool = pool,
        .P = ENV(C),
        .C = C,
    };

    DLOG(&X, "collecting %d module(s)", C->modules->count);

//    pawIr_type_visitor_init(X.V, C, &X);
//    V.VisitTrait = visit_trait;
//    V.VisitType = visit_type;

    pawU_enter_binder(C->U, SCAN_STR(C, "<module>"));

    run_collection_phases(&X, C->hir);

    pawU_leave_binder(C->U);

    // pub fn main(args: [][]char) -> int
    C->main_args_type = pawIr_new_slice(C,
            pawIr_new_slice(C, pawIr_new_char(C)));
}

