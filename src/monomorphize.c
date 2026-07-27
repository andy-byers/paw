// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// Monomorphization routine:
//
// Monomorphization uses 2 data structures: a worklist W containing functions
// pending monomorphization and a set T of all types used in the module.
//
// Fill W with all public monomorphic toplevel functions defined in the target
// module. While the worklist is not empty, remove a function F and process
// it as follows. Monomorphize F if it is polymorphic, otherwise, do nothing.
// Iterate over the function body and add all mentioned types to T. If any of
// the types are functions not already in T, add them to W.

#include "impl.h"
#include "ir_type.h"
#include "lib.h"
#include "map.h"
#include "mir.h"
#include "solve.h"
#include "type_folder.h"
#include "unify.h"

#define TODO (struct SourceSpan){0}

struct MonoCollector {
    struct Pool *pool;

    // stack of types that need to be monomorphized
    IrTypeList *pending;

    // list containing a MIR node for each reachable function
    struct BodyList *globals;

    struct Substitution subst;
    struct MirTypeFolder *F;
    struct Compiler *C;
    struct Mir *base;
    struct Mir *mir;
    TypeCollection *functions;
    TypeCollection *cache;
    BodyMap *bodies;
    paw_Env *P;
};

static void log_instance(struct MonoCollector *M, char const *kind, Str const *name, IrType *type)
{
#if defined(PAW_DEBUG_LOG)
    char const *type_string = pawIr_print_type(M->C, type);
    pawD_debug_log(ENV(M->C), "[paw.compiler.monomorphize.%s] %s @ %p: %s", kind, name->text, (void *)type, type_string);
    --ENV(M->C)->top.p;
#else
    PAW_UNUSED(M);
    PAW_UNUSED(kind);
    PAW_UNUSED(name);
    PAW_UNUSED(type);
#endif
}

static enum BuiltinKind builtin_kind(struct MonoCollector *M, IrType *type)
{
    return pawP_type2code(M->C, type);
}

static IrType *substitute_generic(struct IrTypeFolder *F, struct IrGeneric *t)
{
    IrGenericArg const *pa, *pb;
    struct Substitution *subst = F->ud;
    K_LIST_ZIP (subst->params, pa, subst->args, pb) {
        if (IrGenericArg_is_type(*pa)) {
            IrType *a = IrGenericArg_get_type(*pa);
            IrType *b = IrGenericArg_get_type(*pb);
            if (!IrIsGeneric(a)) continue;
            struct IrGeneric *g = IrGetGeneric(a);
            if (g->did.value == t->did.value)
                return b;
        }
    }

    return IR_CAST_TYPE(t);
}

typedef void *(*Finalizer)(struct MonoCollector *M, void *node, struct Substitution);
static void *finalize(struct MonoCollector *M, Finalizer f, void *node)
{
    return f(M, node, M->subst);
}

static void *type_finalizer(struct MonoCollector *M, void *type, struct Substitution subst)
{
    type = pawU_normalize_projections(M->C->U, type);
    type = pawP_substitute(M->C, type, subst);
    return pawU_normalize_projections(M->C->U, type);
}

static void *const_finalizer(struct MonoCollector *M, void *konst, struct Substitution subst)
{
    return pawP_substitute_const(M->C, konst, subst);
}

static IrType *finalize_type(struct MonoCollector *M, IrType *type)
{
    return finalize(M, type_finalizer, type);
}

static IrConst *finalize_const(struct MonoCollector *M, IrConst *konst)
{
    return finalize(M, const_finalizer, konst);
}

static IrGenericArg finalize_arg(struct MonoCollector *M, IrGenericArg arg)
{
    if (IrGenericArg_is_type(arg)) {
        return IrGenericArg_from_type(
                finalize_type(M, IrGenericArg_get_type(arg)));
    } else {
        return arg;
    }
}

static IrTrait *finalize_trait(struct MonoCollector *M, IrTrait *trait)
{
    IrGenericArgs *args = NULL;
    if (trait->args != NULL) {
        args = IrGenericArgs_new(M->C);
        IrGenericArgs_reserve(M->C, args, trait->args->count);
        K_LIST_XFOREACH (trait->args, IrGenericArg const, p)
            IrGenericArgs_push(M->C, args, finalize_arg(M, *p));
    }
    return pawIr_new_trait(M->C, trait->did, args);
}

static IrType *copy_type(struct MonoCollector *M, IrType *type);

static MirConstant refresh_constant(struct MonoCollector *M, MirConstant k)
{
    struct MirConstantData const *kdata = mir_const_data(M->base, k);
    IrConst *konst = kdata->data->kind == IR_CONST_DECL
        ? finalize_const(M, kdata->data)
        : kdata->data;
    return pawMir_kcache_add_value(M->mir, M->mir->kcache,
            konst->value.value, konst->value.type);
}

static struct MirPlace finalize_place(struct MonoCollector *M, struct MirPlace place)
{
    place.type = copy_type(M, place.type);
    if (place.kind == MIR_PLACE_CONSTANT)
        place.k = refresh_constant(M, place.k);
    return place;
}

static MirPlaceList *copy_places(struct MonoCollector *M, struct MirPlaceList *list)
{
    struct MirPlaceList *result = MirPlaceList_new(M->mir);
    MirPlaceList_reserve(M->mir, result, list->count);
    K_LIST_XFOREACH (list, struct MirPlace const, p)
        MirPlaceList_push(M->mir, result, *p);
    return result;
}

static void copy_phi(struct MonoCollector *M, struct MirPhi *x, struct MirPhi *r)
{
    r->inputs = copy_places(M, x->inputs);
}

static void copy_array(struct MonoCollector *M, struct MirArray *x, struct MirArray *r)
{
    r->elems = copy_places(M, x->elems);
}

static void copy_aggregate(struct MonoCollector *M, struct MirAggregate *x, struct MirAggregate *r)
{
    r->fields = copy_places(M, x->fields);
}

static void copy_call(struct MonoCollector *M, struct MirCall *x, struct MirCall *r)
{
    r->args = copy_places(M, x->args);
}

static void copy_switch(struct MonoCollector *M, struct MirSwitch *t, struct MirSwitch *r)
{
    r->arms = MirSwitchArmList_new(M->mir);
    MirSwitchArmList_reserve(M->mir, r->arms, t->arms->count);

    K_LIST_XFOREACH (t->arms, struct MirSwitchArm const, parm)
        MirSwitchArmList_push(M->mir, r->arms, (struct MirSwitchArm){
                    .k = refresh_constant(M, parm->k),
                });
}

static void finalize_places(struct MonoCollector *M, struct MirPlacePtrList *src, struct MirPlacePtrList *dst)
{
    struct MirPlace *const *pa, *const *pb;
    K_LIST_ZIP (src, pa, dst, pb)
        **pb = finalize_place(M, **pa);
}

static IrType *collect_type(struct IrTypeFolder *F, IrType *type);
static void materialize_field_dropper(struct MonoCollector *M, IrType *type);

static void collect_drop_for(struct MonoCollector *M, IrType *type)
{
    if (pawIr_needs_drop(M->C, type)) {
        IrType *drop_type = pawIr_get_custom_drop_type(M->C, type);
        if (drop_type != NULL) {
            collect_type(&M->F->F, drop_type);
        } else {
            materialize_field_dropper(M, type);
        }
    }
}

static void collect_drops_for(struct MonoCollector *M, IrTypeList *types)
{
    K_LIST_XFOREACH (types, IrType *const, p)
        collect_drop_for(M, *p);
}

static struct MirPlace add_local(struct Mir *mir, char const *name, IrType *type)
{
    int const num_locals = mir->registers->count;
    MirRegisterDataList_push(mir, mir->registers, (struct MirRegisterData){
            .is_nontrivial = PAW_TRUE,
            .is_captured = PAW_FALSE,
            .type = type,
            .name = SCAN_STR(mir->C, name),
            .span = {0}, // TODO: span ref kind for "generated from drop"
            });
    return pawMir_get_register(mir, MIR_REG(num_locals));
}

static struct Mir *allocate_drop_template(struct MonoCollector *M, IrType *type, IrType *self)
{
    struct IrFnDef const *def = pawIr_get_fn_def(M->C, IR_TYPE_DID(type));
    struct Mir *mir = pawMir_new(M->C, 0, (struct SourceSpan){0}, def->name, IR_TYPE_DID(type),
            IR_GENERIC_ARGS(type), Annotations_new(M->C), type, self, -1, def->parent, FUNC_METHOD,
            PAW_TRUE, PAW_FALSE);
    struct MirPlace const result_local = add_local(mir, "(result)", pawIr_new_unit(M->C));
    struct MirPlace const self_local = add_local(mir, "self", pawIr_new_ptr(M->C, self));

    struct MirBlockData *data = pawMir_new_block(mir);
    MirBlockDataList_push(mir, mir->blocks, data);
    MirConstant const k = pawMir_kcache_add_value(mir, mir->kcache, I2V(0), pawIr_new_unit(M->C));
    struct MirPlace const value = {.kind = MIR_PLACE_CONSTANT, .k = k};
    MirInstructionList_push(mir, data->instructions, pawMir_new_alloc_local(mir, TODO, SCAN_STR(M->C, "(result)"), result_local));
    MirInstructionList_push(mir, data->instructions, pawMir_new_alloc_local(mir, TODO, SCAN_STR(M->C, "self"), self_local));
    MirInstructionList_push(mir, data->instructions, pawMir_new_move(mir, TODO, result_local, value));
    MirInstructionList_push(mir, data->instructions, pawMir_new_return(mir, TODO));
    return mir;
}

static void materialize_field_dropper(struct MonoCollector *M, IrType *type)
{
    IrType *drop = pawIr_materialize_drop_type(M->C, type);
    collect_type(&M->F->F, drop);

    struct Mir *mir = allocate_drop_template(M, drop, type);
    BodyMap_insert(M->C, M->bodies, IR_TYPE_DID(mir->type), mir);

    if (IrIsTuple(type)) {
        struct IrTuple const *t = IrGetTuple(type);
        collect_drops_for(M, t->elems);
    } else if (IrIsArray(type)) {
        struct IrArray const *t = IrGetArray(type);
        collect_drop_for(M, t->type);
    } else if (IrIsAdt(type)) {
        struct IrAdt *t = IrGetAdt(type);
        struct IrAdtDef const *def = pawIr_get_adt_def(M->C, t->did);
        // TODO: just use pawP_instantiate_variant_fields, struct is ADT with 1 variant anyway
        if (def->is_struct) {
            IrTypeList *fields = pawP_instantiate_struct_fields(M->C, t);
            collect_drops_for(M, fields);
        } else {
            K_LIST_XFOREACH (def->variants, struct IrVariantDef *const, v) {
                IrTypeList *fields = pawP_instantiate_variant_fields(M->C, t, (*v)->discr);
                collect_drops_for(M, fields);
            }
        }
    }
}

static struct MirInstruction *copy_instruction(struct MonoCollector *M, struct MirInstruction *instr)
{
    struct MirInstruction *r = pawMir_new_instruction(M->mir);
    *r = *instr; // copy trivial fields

    switch (MIR_KINDOF(instr)) {
        case kMirPhi:
            copy_phi(M, MirGetPhi(instr), MirGetPhi(r));
            break;
        case kMirArray:
            copy_array(M, MirGetArray(instr), MirGetArray(r));
            break;
        case kMirAggregate:
            copy_aggregate(M, MirGetAggregate(instr), MirGetAggregate(r));
            break;
        case kMirCall:
            copy_call(M, MirGetCall(instr), MirGetCall(r));
            break;
        case kMirSwitch:
            copy_switch(M, MirGetSwitch(instr), MirGetSwitch(r));
            break;
        default:
            break;
    }
    struct MirPlacePtrList *src_loads = pawMir_get_loads(M->mir, instr);
    struct MirPlacePtrList *dst_loads = pawMir_get_loads(M->mir, r);
    finalize_places(M, src_loads, dst_loads);

    struct MirPlacePtrList *src_stores = pawMir_get_stores(M->mir, instr);
    struct MirPlacePtrList *dst_stores = pawMir_get_stores(M->mir, r);
    finalize_places(M, src_stores, dst_stores);
    return r;
}

static struct MirBlockData *copy_basic_block(struct MonoCollector *M, struct MirBlockData *block)
{
    struct MirBlockData *result = pawMir_new_block(M->mir);
    MirBlockList_reserve(M->mir, result->predecessors, block->predecessors->count);
    MirBlockList_reserve(M->mir, result->successors, block->successors->count);
    MirInstructionList_reserve(M->mir, result->joins, block->joins->count);
    MirInstructionList_reserve(M->mir, result->instructions, block->instructions->count);
    result->mid = block->mid;

    MirBlock const *pb;
    K_LIST_FOREACH (block->predecessors, pb) {
        MirBlockList_push(M->mir, result->predecessors, *pb);
    }
    K_LIST_FOREACH (block->successors, pb) {
        MirBlockList_push(M->mir, result->successors, *pb);
    }

    struct MirInstruction **pinstr;
    K_LIST_FOREACH (block->joins, pinstr) {
        struct MirInstruction *r = copy_instruction(M, *pinstr);
        MirInstructionList_push(M->mir, result->joins, r);
    }
    K_LIST_FOREACH (block->instructions, pinstr) {
        struct MirInstruction *r = copy_instruction(M, *pinstr);
        MirInstructionList_push(M->mir, result->instructions, r);
    }
    return result;
}

static struct Mir *new_mir(struct MonoCollector *M, struct Mir *base, IrType *type, IrType *self)
{
    IrGenericArgs *args = IrGenericArgs_new(M->C);
    K_LIST_XFOREACH (base->args, IrGenericArg const, p)
        IrGenericArgs_push(M->C, args, finalize_arg(M, *p));

    M->mir = pawMir_new(M->C, base->modno, base->span, base->name, base->did, args, base->annotations,
            type, self, base->child_id, base->parent_id, base->fn_kind, base->is_pub, PAW_FALSE);
    return M->mir;
}

static IrType *get_assoc_fn(struct MonoCollector *M, IrType *self, IrTrait *trait, Str *name)
{
    // associated fn will always be found unless there is a bug in the compiler
    struct IrObligationCause const INFALLIBLE = {0};
    if (trait == NULL)
        return pawP_find_method(M->C, self, name, INFALLIBLE)->inst;
    return pawP_find_trait_method(M->C, self, trait, name, INFALLIBLE)->inst;
}

static IrType *copy_type(struct MonoCollector *M, IrType *type)
{
    type = finalize_type(M, type);
    if (IrIsSignature(type)) {
        IrType *self = pawIr_get_context(M->C, type);
        IrTrait *trait = pawIr_get_trait_context(M->C, type);
        if (trait != NULL)
            trait = finalize_trait(M, trait);
        if (self != NULL) {
            struct IrFnDef *def = pawIr_get_fn_def(M->C, IR_TYPE_DID(type));
            IrType *method = get_assoc_fn(M, self, trait, def->name);
            if (method != NULL) {
                pawU_unify_unchecked(M->C->U, method, type);
                return pawU_normalize_projections(M->C->U, method);
            }
        }
    }
    return type;
}

static struct MirRegisterData copy_register(struct MonoCollector *M, struct MirRegisterData reg)
{
    reg.type = copy_type(M, reg.type);
    return reg;
}

static void do_monomorphize(struct MonoCollector *M, struct Mir *base, struct Mir *inst)
{
#define RESERVE_MEMORY(M_, ListT_, Member_) \
        ListT_##_reserve((M_)->mir, inst->Member_, base->Member_->count);
    RESERVE_MEMORY(M, MirRegisterDataList, registers);
    RESERVE_MEMORY(M, MirBlockDataList, blocks);
    RESERVE_MEMORY(M, MirCaptureList, captured);
    RESERVE_MEMORY(M, MirUpvalueList, upvalues);
#undef RESERVE_MEMORY

    {
        struct MirRegisterData *pfrom;
        K_LIST_FOREACH (base->registers, pfrom) {
            struct MirRegisterData const to = copy_register(M, *pfrom);
            MirRegisterDataList_push(inst, inst->registers, to);
        }
    }

    {
        struct MirBlockData *const *pfrom;
        K_LIST_FOREACH (base->blocks, pfrom) {
            struct MirBlockData *to = copy_basic_block(M, *pfrom);
            MirBlockDataList_push(inst, inst->blocks, to);
        }
    }

//TODO    {
//TODO        struct MirConstantData const *pdata;
//TODO        K_LIST_FOREACH (base->kcache->data, pdata) {
//TODO            IrConst *konst = pdata->data->kind == IR_CONST_DECL
//TODO                ? finalize_const(M, pdata->data) : pdata->data;
//TODO
//TODO            if (konst->kind == IR_CONST_VALUE) {
//TODO                struct IrConstValue const value = konst->value;
//TODO                pawMir_kcache_add_value(inst, inst->kcache, value.value, value.type);
//TODO            } else {
//TODO                paw_assert(konst->kind == IR_CONST_DECL);
//TODO                pawMir_kcache_add_param(inst, inst->kcache, konst->param.did);
//TODO            }
//TODO        }
//TODO    }

    {
        struct MirCaptureInfo const *pci;
        K_LIST_FOREACH (base->captured, pci) {
            MirCaptureList_push(inst, inst->captured, *pci);
        }
    }

    {
        if (base->env_type != NULL)
            inst->env_type = finalize_type(M, base->env_type);

        struct MirUpvalueInfo const *pup;
        K_LIST_FOREACH (base->upvalues, pup) {
            struct MirUpvalueInfo up = *pup;
            up.type = finalize_type(M, up.type);
            MirUpvalueList_push(inst, inst->upvalues, up);
        }
    }
}

static struct Mir *monomorphize_function_aux(struct MonoCollector *M, struct Mir *base, IrType *type)
{
    M->subst.params = IR_GENERIC_ARGS(base->type);
    M->subst.args = IR_GENERIC_ARGS(type);
    M->base = base;

    IrType *self = pawIr_get_context(M->C, type);
    struct Mir *inst = new_mir(M, base, type, self);
    do_monomorphize(M, base, inst);
    log_instance(M, "fn", base->name, type);

    M->subst.params = NULL;
    M->subst.args = NULL;
    return inst;
}

static struct Mir *monomorphize_method_aux(struct MonoCollector *M, struct Mir *base, IrType *type, IrType *self)
{
    paw_assert(IrIsSignature(type));
    IrTrait *trait = pawIr_get_trait_context(M->C, type);
    struct IrFnDef const *def = pawIr_get_fn_def(M->C, IR_TYPE_DID(type));
    struct IrImpl *const *impl_ptr = ImplMap_get(M->C, M->C->impl_defs, base->parent_id);
    if (impl_ptr == NULL || (*impl_ptr)->generics == NULL)
        return monomorphize_function_aux(M, base, type);

    IrType *fn = get_assoc_fn(M, self, trait, def->name);
    IrGetSignature(type)->did = IR_TYPE_DID(fn);

    pawU_unify_unchecked(M->C->U, type, fn);
    type = pawU_normalize_projections(M->C->U, type);

    return monomorphize_function_aux(M, base, type);
}

static struct Mir *monomorphize(struct MonoCollector *M, IrType *type)
{
    struct Mir *base = *BodyMap_get(M->C, M->bodies, IR_TYPE_DID(type)); // must exist
    if (!IR_TYPE_IS_POLYMORPHIC(base->type)) return base;

    IrType *self = pawIr_get_context(M->C, type);
    return self == NULL
               ? monomorphize_function_aux(M, base, type)
               : monomorphize_method_aux(M, base, type, self);
}

static IrType *collect_type(struct IrTypeFolder *F, IrType *type)
{
    struct MirTypeFolder *outer = F->ud;
    struct MonoCollector *M = outer->ud;
    paw_assert(!IrIsGeneric(type) && !IrIsInfer(type));
    type = pawU_normalize_projections(F->C->U, type);

    void *const *p = TypeCollection_get(M->C, M->cache, type);
    if (p != NULL) return type;
    TypeCollection_insert(M->C, M->cache, type, type);

    collect_drop_for(M, type);

    if (IrIsSignature(type) || IrIsClosure(type))
        IrTypeList_push(M->C, M->pending, type);

    return type;
}

static void run_collector(struct MonoCollector *M, struct Mir *mir)
{
    M->mir = M->F->V.mir = mir;
    pawMir_fold(M->F, mir);
}

struct MonoResult pawP_monomorphize(struct Compiler *C, BodyMap *bodies)
{
    struct MirTypeFolder F;
    struct MonoCollector M = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .pending = IrTypeList_new(C),
        .globals = BodyList_new(C),
        .cache = TypeCollection_new(C),
        .functions = TypeCollection_new(C),
        .bodies = bodies,
        .P = ENV(C),
        .F = &F,
        .C = C,
    };

    pawU_enter_binder(C->U, NULL);

    pawMir_type_folder_init(&F, C, NULL, &M);
    F.F.FoldType = collect_type;

    // discover functions reachable from the toplevel
    BodyMapIterator iter;
    BodyMapIterator_init(bodies, &iter);
    while (BodyMapIterator_is_valid(&iter)) {
        struct Mir *mir = *BodyMapIterator_valuep(&iter);
        if (mir->is_pub && !IR_TYPE_IS_POLYMORPHIC(mir->type))
            collect_type(&M.F->F, mir->type);
        BodyMapIterator_next(&iter);
    }

    // iterate until monomorphization is complete (every function signature has
    // an MIR body in M.globals)
    while (M.pending->count > 0) {
        IrType *type = K_LIST_LAST(M.pending);
        IrTypeList_pop(M.pending);

        struct Mir *body = monomorphize(&M, type);
        TypeCollection_insert(C, M.functions, type, body);
        BodyList_push(C, M.globals, body);
        run_collector(&M, body);
    }

    pawU_discard_variables(C->U);
    pawU_leave_binder(C->U);

//    // TODO: do this at the end of lower_hir.c. find methods a diferent way. already
//    //       know they exist, just need to find and substitute types.
//    // free memory used for HIR
//    pawP_pool_free(C, C->hir_pool);
//    C->hir_pool = NULL;

    pawP_pool_free(C, M.pool);
    return (struct MonoResult){
        .bodies = M.globals,
        .bodies2 = M.functions,
    };
}
