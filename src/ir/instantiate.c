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

#include "instantiate.h"

#include "compiler/error.h"
#include "compiler/type_folder.h"
#include "ir/type.h"
#include "sema/solve.h"
#include "sema/unify.h"

static IrType *fold_generic(struct IrTypeFolder *F, struct IrGeneric *t)
{
    struct Substitution *subst = F->ud;

    IrGenericArg const *pg, *pt;
    LIST_ZIP (subst->params, pg, subst->args, pt) {
        if (IrGenericArg_is_type(*pg)) {
            IrType *type = IrGenericArg_get_type(*pg);
            struct IrGeneric *g = IrGetGeneric(type);
            if (t->did.value == g->did.value)
                return IrGenericArg_get_type(*pt);
        }
    }
    return IR_CAST_TYPE(t);
}

static IrConst *fold_const(struct IrTypeFolder *F, struct IrConst *k)
{
    struct Substitution *subst = F->ud;
    if (k->kind == IR_CONST_DECL) {
        IrGenericArg const *pp, *pa;
        LIST_ZIP (subst->params, pp, subst->args, pa) {
            if (IrGenericArg_is_const(*pp)) {
                IrConst *konst = IrGenericArg_get_const(*pp);
                if (konst->kind == IR_CONST_DECL
                        && P_ID_EQUALS(k->decl.did, konst->decl.did))
                    return IrGenericArg_get_const(*pa);
            }
        }
    }
    return k;
}

void init_substitution_folder(struct IrTypeFolder *F, struct Compiler *C, struct Substitution *subst,
                                   IrGenericArgs *generics, IrGenericArgs *types)
{
    *subst = (struct Substitution){
        .params = generics,
        .args = types,
    };
    ir_type_folder_init(F, C, subst);
    F->FoldGeneric = fold_generic;
    F->FoldConst = fold_const;
}

static IrTypeList *instantiate_variant_fields_(struct Compiler *C, struct IrVariantDef *def, IrGenericArgs *before, IrGenericArgs *after)
{
    struct IrFieldDef *const *pfield;
    IrTypeList *fields = IrTypeList_new(ALLOCATOR_OF(C));
    IrTypeList_reserve(fields, def->fields->count);
    LIST_FOREACH (def->fields, pfield) {
        IrType *field = ir_get_def_type(C, (*pfield)->did);
        IrTypeList_push(fields, field);
    }

    struct IrTypeFolder F;
    struct Substitution subst;
    init_substitution_folder(&F, C, &subst, before, after);
    return ir_fold_type_list(&F, fields);
}

IrTypeList *instantiate_struct_fields(struct Compiler *C, struct IrAdt const *inst)
{
    return instantiate_variant_fields(C, inst, 0);
}

IrTypeList *instantiate_variant_fields(struct Compiler *C, struct IrAdt const *inst, int index)
{
    paw_assert(index >= 0); // expects valid discriminant number
    struct IrAdt *base = IrGetAdt(ir_get_def_type(C, inst->did));
    struct IrAdtDef *def = ir_get_adt_def(C, inst->did);
    struct IrVariantDef *variant = IrVariantDefs_get(def->variants, index);

    return instantiate_variant_fields_(C, variant, base->args, inst->args);
}

IrTrait *substitute_trait(struct Compiler *C, IrTrait *trait, struct Substitution subst)
{
    IrGenericArgs *types = NULL;
    if (trait->args != NULL) {
        types = IrGenericArgs_new(ALLOCATOR_OF(C));
        IrGenericArgs_reserve(types, trait->args->count);
        LIST_XFOREACH (trait->args, IrGenericArg const, p)
            IrGenericArgs_push(types, substitute_arg(C, *p, subst));
    }

    return ir_new_trait(C, trait->did, types);
}

IrType *substitute_type(struct Compiler *C, IrType *type, struct Substitution subst)
{
    struct IrTypeFolder F;
    ir_type_folder_init(&F, C, &subst);
    F.FoldGeneric = fold_generic;
    F.FoldConst = fold_const;
    return ir_fold_type(&F, type);
}

IrGenericArg substitute_arg(struct Compiler *C, IrGenericArg arg, struct Substitution subst)
{
    if (IrGenericArg_is_type(arg)) {
        IrType *t = IrGenericArg_get_type(arg);
        return IrGenericArg_from_type(
                substitute_type(C, t, subst));
    } else {
        IrConst *k = IrGenericArg_get_const(arg);
        return IrGenericArg_from_const(
                substitute_const(C, k, subst));
    }
}

IrConst *substitute_const(struct Compiler *C, IrConst *k, struct Substitution subst)
{
    struct IrTypeFolder F;
    ir_type_folder_init(&F, C, &subst);
    F.FoldGeneric = fold_generic;
    F.FoldConst = fold_const;
    return ir_fold_const(&F, k);
}

struct Instantiation pawP_instantiate_assoc(struct Compiler *C, IrType *type, IrType *method)
{
    struct Substitution subst = {
        subst.params = IrGenericArgs_new(ALLOCATOR_OF(C)),
        subst.args = IrGenericArgs_new(ALLOCATOR_OF(C)),
    };

    IrType *base = ir_get_context(C, method);
    if (IrIsGeneric(base)) {
        IrGenericArgs_push(subst.params, IrGenericArg_from_type(base));
        IrGenericArgs_push(subst.args, IrGenericArg_from_type(type));
    }

    IrGenericArgs const *method_args = IR_GENERIC_ARGS(method);
    LIST_XFOREACH (method_args, IrGenericArg const, arg) {
        IrType *infer = create_type_var(C->U, IR_INFER_TYPE, (struct SourceSpan){0});
        IrGenericArgs_push(subst.params, *arg);
        IrGenericArgs_push(subst.args, IrGenericArg_from_type(infer));
    }

    method = substitute_type(C, method, subst);

    base = ir_get_context(C, method);
    unify_types_unchecked(C->U, base, type);

    return (struct Instantiation){
        .inst = method,
        .subst = subst,
    };
}
