// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "layout.h"
#include "lib.h"
#include "map.h"
#include "mir.h"
#include "type_folder.h"
#include "unify.h"

struct GenericsState {
    struct GenericsState *outer;
    IrTypeList *before;
    IrTypeList *after;
};

struct MonoCollector {
    struct Pool *pool;

    // stack of types that need to be monomorphized
    IrTypeList *pending;

    // list containing a MIR node for each reachable function
    struct BodyList *globals;

    // list of unique ADTs encountered during monomorphization
    IrTypeList *types;

    IrTypeList *other;
    struct GenericsState *gs;
    struct MirTypeFolder *F;
    struct Compiler *C;
    struct Mir *mir;
    struct TypeMonoMap *methods;
    struct DeclMonoMap *monos;
    struct DeclMonoMap *adts;
    BodyMap *bodies;
    paw_Env *P;
};

DEFINE_MAP(struct MonoCollector, DeclMonoMap, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, IrTypeList *)
DEFINE_MAP(struct MonoCollector, TypeMonoMap, pawP_alloc, IR_TYPE_HASH, IR_TYPE_EQUALS, IrType *, IrTypeList *)
DEFINE_MAP_ITERATOR(DeclMonoMap, DeclId, IrTypeList *)

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

static IrType *substitute_generic(struct IrTypeFolder *F, struct IrGeneric *t)
{
    IrType **pa, **pb;
    struct Substitution *subst = F->ud;
    K_LIST_ZIP (subst->generics, pa, subst->types, pb) {
        if (!IrIsGeneric(*pa)) continue;
        struct IrGeneric *g = IrGetGeneric(*pa);
        if (g->did.value == t->did.value)
            return *pb;
    }

    return IR_CAST_TYPE(t);
}

IrType *finalize_type(struct MonoCollector *M, IrType *type)
{
    struct GenericsState *gs = M->gs;
    while (gs != NULL) {
        if (gs->before != NULL) {
            struct Substitution subst = {
                .generics = gs->before,
                .types = gs->after,
                .C = M->C,
            };
            struct IrTypeFolder F;
            pawIr_type_folder_init(&F, M->C, &subst);
            F.FoldGeneric = substitute_generic;
            type = pawIr_fold_type(&F, type);
        }
        gs = gs->outer;
    }
    return type;
}

static IrType *copy_type(struct MonoCollector *M, IrType *type);

static struct MirPlace finalize_place(struct MonoCollector *M, struct MirPlace place)
{
    place.type = copy_type(M, place.type);
    return place;
}

static MirPlaceList *copy_places(struct MonoCollector *M, struct MirPlaceList *list)
{
    struct MirPlaceList *result = MirPlaceList_new(M->mir);
    MirPlaceList_reserve(M->mir, result, list->count);

    struct MirPlace const *pr;
    K_LIST_FOREACH (list, pr)
        MirPlaceList_push(M->mir, result, *pr);
    return result;
}

static void copy_phi(struct MonoCollector *M, struct MirPhi *x, struct MirPhi *r)
{
    r->inputs = copy_places(M, x->inputs);
}

static void copy_concat(struct MonoCollector *M, struct MirConcat *x, struct MirConcat *r)
{
    r->inputs = copy_places(M, x->inputs);
}

static void copy_container(struct MonoCollector *M, struct MirContainer *x, struct MirContainer *r)
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

    struct MirSwitchArm const *parm;
    K_LIST_FOREACH (t->arms, parm) {
        MirSwitchArmList_push(M->mir, r->arms, *parm);
    }
}

static void finalize_places(struct MonoCollector *M, struct MirPlacePtrList *src, struct MirPlacePtrList *dst)
{
    struct MirPlace *const *pa, *const *pb;
    K_LIST_ZIP (src, pa, dst, pb)
        **pb = finalize_place(M, **pa);
}

static struct MirInstruction *copy_instruction(struct MonoCollector *M, struct MirInstruction *instr)
{
    struct MirInstruction *r = pawMir_new_instruction(M->mir);
    *r = *instr; // copy trivial fields

    switch (MIR_KINDOF(instr)) {
        case kMirPhi:
            copy_phi(M, MirGetPhi(instr), MirGetPhi(r));
            break;
        case kMirConcat:
            copy_concat(M, MirGetConcat(instr), MirGetConcat(r));
            break;
        case kMirContainer:
            copy_container(M, MirGetContainer(instr), MirGetContainer(r));
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

static void enter_generics_context(struct MonoCollector *M, struct GenericsState *gs, IrTypeList *generics, IrTypeList *types)
{
    *gs = (struct GenericsState){
        .before = generics,
        .after = types,
        .outer = M->gs,
    };
    M->gs = gs;
}

static void leave_generics_context(struct MonoCollector *M)
{
    M->gs = M->gs->outer;
}

static struct Mir *new_mir(struct MonoCollector *M, struct Mir *base, IrType *type, IrType *self)
{
    M->mir = pawMir_new(M->C, base->modno, base->span, base->name, base->annotations,
            type, self, base->child_id, base->fn_kind, base->is_pub, PAW_FALSE);
    return M->mir;
}

IrType *instantiate_type(struct Compiler *C, IrType *type, IrTypeList *args)
{
    if (args == NULL)
        return type;

    struct IrTypeFolder F;
    struct Substitution subst = {
        .generics = IR_TYPE_SUBTYPES(type),
        .types = args,
        .C = C,
    };

    pawIr_type_folder_init(&F, C, &subst);
    F.FoldGeneric = substitute_generic;
    return pawIr_fold_type(&F, type);
}

IrType *instantiate_method(struct Compiler *C, IrType *method, IrType *self)
{
    struct IrAdt *inst = IrGetAdt(self);
    struct IrAdt *base = IrGetAdt(pawIr_get_def_type(C, inst->did));

    if (base->types == NULL)
        return method;

    struct IrTypeFolder F;
    struct Substitution subst = {
        .generics = base->types,
        .types = inst->types,
        .C = C,
    };

    pawIr_type_folder_init(&F, C, &subst);
    F.FoldGeneric = substitute_generic;
    return pawIr_fold_type(&F, method);
}

static IrType *copy_type(struct MonoCollector *M, IrType *type)
{
    if (IrIsSignature(type)) {
        struct IrSignature *fn = IrGetSignature(type);
        if (fn->self != NULL) {
            // determine the "Self" type and look up the concrete method
            IrType *self = finalize_type(M, fn->self);
            struct IrFnDef *def = pawIr_get_fn_def(M->C, IR_TYPE_DID(type));
            type = pawP_find_method(M->C, self, def->name);
            type = instantiate_method(M->C, type, self);
            type = instantiate_type(M->C, type, fn->types);
        }
    }
    return finalize_type(M, type);
}

static struct MirRegisterData copy_register(struct MonoCollector *M, struct MirRegisterData reg)
{
    reg.type = copy_type(M, reg.type);
    return reg;
}

static struct MirLocalData copy_local_data(struct MonoCollector *M, struct MirLocalData local)
{
    local.type = copy_type(M, local.type);
    return local;
}

static void do_monomorphize(struct MonoCollector *M, struct Mir *base, struct Mir *inst)
{
#define RESERVE_MEMORY(M_, ListT_, Member_) \
        ListT_##_reserve((M_)->mir, inst->Member_, base->Member_->count);
    RESERVE_MEMORY(M, MirRegisterDataList, registers);
    RESERVE_MEMORY(M, MirLocalDataList, local_data);
    RESERVE_MEMORY(M, MirBlockDataList, blocks);
    RESERVE_MEMORY(M, MirPlaceList, locals);
    RESERVE_MEMORY(M, MirCaptureList, captured);
    RESERVE_MEMORY(M, MirUpvalueList, upvalues);
    RESERVE_MEMORY(M, MirBodyList, children);
#undef RESERVE_MEMORY

    {
        struct MirRegisterData *pfrom;
        K_LIST_FOREACH (base->registers, pfrom) {
            struct MirRegisterData const to = copy_register(M, *pfrom);
            MirRegisterDataList_push(inst, inst->registers, to);
        }
    }

    {
        struct MirLocalData *pfrom;
        K_LIST_FOREACH (base->local_data, pfrom) {
            struct MirLocalData const to = copy_local_data(M, *pfrom);
            MirLocalDataList_push(inst, inst->local_data, to);
        }
    }

    {
        struct MirBlockData *const *pfrom;
        K_LIST_FOREACH (base->blocks, pfrom) {
            struct MirBlockData *to = copy_basic_block(M, *pfrom);
            MirBlockDataList_push(inst, inst->blocks, to);
        }
    }

    {
        struct MirConstantData const *pdata;
        K_LIST_FOREACH (base->kcache->data, pdata) {
            pawMir_kcache_add(inst, inst->kcache, pdata->value, pdata->kind);
        }
    }

    {
        struct MirPlace const *pplace;
        K_LIST_FOREACH (base->locals, pplace) {
            struct MirPlace local = finalize_place(M, *pplace);
            MirPlaceList_push(inst, inst->locals, local);
        }
    }

    {
        struct MirCaptureInfo const *pci;
        K_LIST_FOREACH (base->captured, pci) {
            MirCaptureList_push(inst, inst->captured, *pci);
        }
    }

    {
        struct MirUpvalueInfo const *pup;
        K_LIST_FOREACH (base->upvalues, pup) {
            struct MirUpvalueInfo up = *pup;
            up.type = finalize_type(M, up.type);
            MirUpvalueList_push(inst, inst->upvalues, up);
        }
    }

    {
        // monomorphize nested closures
        struct Mir *const *pchild;
        K_LIST_FOREACH (base->children, pchild) {
            struct Mir *base_child = *pchild;
            IrType *inst_type = finalize_type(M, base_child->type);
            struct Mir *inst_child = new_mir(M, base_child, inst_type, NULL);
            do_monomorphize(M, base_child, inst_child);
            MirBodyList_push(inst, inst->children, inst_child);
        }
    }
}

static paw_Bool is_monomorphic(struct Mir const *mir)
{
    return (mir->self == NULL || IR_TYPE_SUBTYPES(mir->self) == NULL) && ir_signature_types(mir->type) == NULL;
}

static struct Mir *monomorphize_function_aux(struct MonoCollector *M, struct Mir *base, struct IrSignature *sig, IrType *self)
{
    if (is_monomorphic(base))
        return base;

    struct GenericsState gs;
    enter_generics_context(M, &gs, ir_signature_types(base->type), sig->types);

    struct Mir *inst = new_mir(M, base, IR_CAST_TYPE(sig), self);
    do_monomorphize(M, base, inst);
    log_instance(M, "fn", base->name, IR_CAST_TYPE(sig));

    leave_generics_context(M);
    return inst;
}

static struct Mir *monomorphize_method_aux(struct MonoCollector *M, struct Mir *base, struct IrSignature *sig, IrType *self)
{
    IrType *base_type = pawIr_get_def_type(M->C, IR_TYPE_DID(sig->self));
    IrTypeList *base_binder = IR_TYPE_SUBTYPES(base_type);

    if (base_binder == NULL)
        return monomorphize_function_aux(M, base, sig, self);

    IrTypeList *inst_binder = IR_TYPE_SUBTYPES(sig->self);

    struct GenericsState gs;
    enter_generics_context(M, &gs, base_binder, inst_binder);
    struct Mir *inst = monomorphize_function_aux(M, base, sig, self);
    leave_generics_context(M);
    return inst;
}

static paw_Bool test_types(struct MonoCollector *M, IrType *a, IrType *b)
{
    struct Compiler *C = M->C;
    if (!pawIr_type_equals(C, a, b))
        return PAW_FALSE;

    if (IrIsSignature(a)) {
        struct IrSignature *sa = IrGetSignature(a);
        struct IrSignature *sb = IrGetSignature(b);

        if (!sa->self != !sb->self
                || (sa->self != NULL && !test_types(M, sa->self, sb->self))
                || !sa->types != !sb->types)
            return PAW_FALSE;

        if (sa->types != NULL) {
            // Type arguments must be checked on functions. Though they are not required
            // to appear in the function signature, generics might still be used to
            // specify different behavior. This happens, for example, when a polymorphic
            // function has a generic parameter with a trait bound that allows an
            // associated function to be called.
            IrType *const *pa, *const *pb;
            K_LIST_ZIP(sa->types, pa, sb->types, pb) {
                if (!test_types(M, *pa, *pb))
                    return PAW_FALSE;
            }
        }
    }
    return PAW_TRUE;
}

static IrType *cannonicalize_fn(struct MonoCollector *M, IrTypeList *monos, IrType *type)
{
    IrType *const *pmono;
    K_LIST_FOREACH (monos, pmono) {
        if (test_types(M, type, *pmono))
            return *pmono;
    }

    IrTypeList_push(M->C, M->pending, type);
    IrTypeList_push(M->C, monos, type);
    return type;
}

static paw_Bool test_fns(struct MonoCollector *M, IrType *a, IrType *b)
{
    return IR_TYPE_DID(a).value == IR_TYPE_DID(b).value && test_types(M, a, b);
}

static IrType *cannonicalize_method(struct MonoCollector *M, IrTypeList *monos, IrType *type)
{
    IrType *const *pmono;
    K_LIST_FOREACH (monos, pmono) {
        if (test_fns(M, type, *pmono))
            return *pmono;
    }

    IrTypeList_push(M->C, M->pending, type);
    IrTypeList_push(M->C, monos, type);
    return type;
}

static IrTypeList *mono_list_for_type(struct MonoCollector *M, TypeMonoMap *lists, IrType *type)
{
    IrTypeList **plist = TypeMonoMap_get(M, lists, type);
    if (plist != NULL)
        return *plist;

    IrTypeList *monos = IrTypeList_new(M->C);
    TypeMonoMap_insert(M, lists, type, monos);
    return monos;
}

static IrTypeList *mono_list_for_decl(struct MonoCollector *M, DeclMonoMap *lists, DeclId did)
{
    IrTypeList **plist = DeclMonoMap_get(M, lists, did);
    if (plist != NULL)
        return *plist;

    IrTypeList *monos = IrTypeList_new(M->C);
    DeclMonoMap_insert(M, lists, did, monos);
    return monos;
}

static IrType *register_method(struct MonoCollector *M, struct IrSignature *t);

// Register "hash" and "eq" methods required for a type to be used as a map key
// Both functions are called by the runtime, but not necessarily by Paw code.
static void register_map_methods(struct MonoCollector *M, IrType *type)
{
    IrType *key = ir_map_key(type);
    struct TraitOwnerList *owners = pawP_get_trait_owners(M->C, key);

    IrTypeList **pequals_list = &K_LIST_AT(owners, TRAIT_EQUALS);
    if (*pequals_list == NULL) {
        IrType *equals = pawP_find_method(M->C, key, SCAN_STR(M->C, "eq"));
        equals = instantiate_method(M->C, equals, key);
        register_method(M, IrGetSignature(equals));
        *pequals_list = IrTypeList_new(M->C);
        IrTypeList_push(M->C, *pequals_list, equals);
        TraitOwnerList_set(owners, TRAIT_EQUALS, *pequals_list);
    }

    IrTypeList **phash_list = &K_LIST_AT(owners, TRAIT_HASH);
    if (*phash_list == NULL) {
        IrType *hash = pawP_find_method(M->C, key, SCAN_STR(M->C, "hash"));
        hash = instantiate_method(M->C, hash, key);
        register_method(M, IrGetSignature(hash));
        *phash_list = IrTypeList_new(M->C);
        IrTypeList_push(M->C, *phash_list, hash);
        TraitOwnerList_set(owners, TRAIT_HASH, *phash_list);
    }
}

static IrType *register_function(struct MonoCollector *M, struct IrSignature *t);

// Instantiate and collect all methods declared on the given stdlib ADT instance
// Needed so that functions generated in "codegen.cc" can find the methods they
// depend on.
static void instantiate_std_methods(struct MonoCollector *M, IrType *inst)
{
    struct Compiler *C = M->C;
    DeclId const did = IR_TYPE_DID(inst);
    Str const *modname = ModuleInfo_get(M->C->modinfo, (int)did.modno).name;
    if (!pawL_is_std_name(modname->text)) return;

    IrTypeList *args = IR_TYPE_SUBTYPES(inst);
    IrType *base = pawIr_get_def_type(C, did);
    struct IrAdtDef *def = pawIr_get_adt_def(C, did);

    IrType *const *pmethod;
    IrTypeList *methods = IrTypeList_new(C);
    K_LIST_FOREACH (def->methods, pmethod) {
        IrType *method = args == NULL ? *pmethod
            : pawP_instantiate_method(C, base, args, *pmethod);
        register_method(M, IrGetSignature(method));
        IrTypeList_push(C, methods, method);
    }
}

static IrType *cannonicalize_adt(struct MonoCollector *M, IrTypeList *monos, IrType *type)
{
    IrType *const *pmono;
    K_LIST_FOREACH (monos, pmono) {
        if (test_types(M, type, *pmono))
            return *pmono;
    }
    IrTypeList_push(M->C, M->types, type);
    IrTypeList_push(M->C, monos, type);
    instantiate_std_methods(M, type);

    // catch infinitely-sized inline aggregates
    pawIr_compute_layout(M->C, type);
    return type;
}

static IrType *collect_adt(struct MonoCollector *M, struct IrAdt *t)
{
    IrTypeList *monos = mono_list_for_decl(M, M->adts, t->did);
    return cannonicalize_adt(M, monos, IR_CAST_TYPE(t));
}

static IrType *register_function(struct MonoCollector *M, struct IrSignature *t)
{
    IrTypeList *monos = mono_list_for_decl(M, M->monos, t->did);
    return cannonicalize_fn(M, monos, IR_CAST_TYPE(t));
}

static IrType *register_method(struct MonoCollector *M, struct IrSignature *t)
{
    t->self = IrIsAdt(t->self) ? collect_adt(M, IrGetAdt(t->self)) : t->self;
    IrTypeList *monos = mono_list_for_type(M, M->methods, t->self);
    return cannonicalize_method(M, monos, IR_CAST_TYPE(t));
}

static IrType *collect_signature(struct MonoCollector *M, struct IrSignature *t)
{
    if (t->self != NULL)
        return register_method(M, t);
    return register_function(M, t);
}

static struct Mir *monomorphize(struct MonoCollector *M, IrType *type)
{
    struct IrSignature *t = IrGetSignature(type);
    struct Mir *base = *BodyMap_get(M->C, M->bodies, t->did); // must exist
    if (!base->is_poly) return base;

    return t->self == NULL
               ? monomorphize_function_aux(M, base, t, NULL)
               : monomorphize_method_aux(M, base, t, t->self);
}

static IrType *collect_other(struct MonoCollector *M, IrType *type)
{
    IrType *const *ptarget;
    K_LIST_FOREACH (M->other, ptarget) {
        if (pawU_equals(M->C->U, *ptarget, type))
            return *ptarget;
    }
    IrTypeList_push(M->C, M->other, type);
    return type;
}

static IrType *collect_type(struct IrTypeFolder *F, IrType *type)
{
    struct MirTypeFolder *outer = F->ud;
    struct MonoCollector *M = outer->ud;

    paw_assert(!IrIsGeneric(type) && !IrIsInfer(type));
    if (IrIsAdt(type))
        return collect_adt(M, IrGetAdt(type));
    if (IrIsSignature(type))
        return collect_signature(M, IrGetSignature(type));
    return collect_other(M, type);
}

static paw_Bool is_entrypoint(struct Mir const *mir)
{
    return mir->is_pub && !mir->is_poly && (mir->self == NULL || IR_TYPE_SUBTYPES(mir->self) == NULL);
}

static IrType *get_base_method_type(struct Compiler *C, struct IrAdtDef const *adt_def, Str const *name)
{
    IrType *const *ptype;
    K_LIST_FOREACH (adt_def->methods, ptype) {
        struct IrFnDef const *fn_def = pawIr_get_fn_def(C, IR_TYPE_DID(*ptype));
        if (pawS_eq(fn_def->name, name)) return *ptype;
    }

    PAW_UNREACHABLE();
}

static void monomorphize_builtin_method(struct MonoCollector *M, IrType *adt, char const *name)
{
    Str const *method_name = SCAN_STR(M->C, name);
    struct IrAdtDef const *def = pawIr_get_adt_def(M->C, IR_TYPE_DID(adt));
    IrType *method_type = get_base_method_type(M->C, def, method_name);
    struct IrSignature *signature = IrGetSignature(method_type);
    struct Mir *const *pbody = BodyMap_get(M->C, M->bodies, signature->did);
    paw_assert(pbody != NULL);

    monomorphize_method_aux(M, *pbody, signature, adt);
}

struct MonoResult pawP_monomorphize(struct Compiler *C, BodyMap *bodies)
{
    struct MirTypeFolder F;
    struct MonoCollector M = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .pending = IrTypeList_new(C),
        .globals = BodyList_new(C),
        .types = IrTypeList_new(C),
        .other = IrTypeList_new(C),
        .bodies = bodies,
        .P = ENV(C),
        .F = &F,
        .C = C,
    };
    M.methods = TypeMonoMap_new(&M);
    M.monos = DeclMonoMap_new(&M);
    M.adts = DeclMonoMap_new(&M);

    pawU_enter_binder(C->U, NULL);

    pawMir_type_folder_init(&F, C, NULL, &M);
    F.F.FoldType = collect_type;

    collect_type(&M.F->F, C->strtab_type);
    collect_type(&M.F->F, C->main_args_type);

    // discover functions reachable from the toplevel
    BodyMapIterator iter;
    BodyMapIterator_init(bodies, &iter);
    while (BodyMapIterator_is_valid(&iter)) {
        struct Mir *mir = *BodyMapIterator_valuep(&iter);
        if (is_entrypoint(mir))
            collect_type(&M.F->F, mir->type);
        BodyMapIterator_next(&iter);
    }

    // iterate until monomorphization is complete (every function signature has
    // an MIR body in M.globals)
    while (M.pending->count > 0) {
        IrType *type = K_LIST_LAST(M.pending);
        IrTypeList_pop(M.pending);
        struct Mir *body = monomorphize(&M, type);
        BodyList_push(C, M.globals, body);
        M.mir = M.F->V.mir = body;
        pawMir_fold(M.F, body);
    }

    pawU_leave_binder(C->U);

    IrType *const *pother;
    K_LIST_FOREACH (M.other, pother) {
        IrTypeList_push(C, M.types, *pother);
    }

//    // TODO: do this at the end of lower_hir.c. find methods a diferent way. already
//    //       know they exist, just need to find and substitute types.
//    // free memory used for HIR
//    pawP_pool_free(C, C->hir_pool);
//    C->hir_pool = NULL;

    pawP_pool_free(C, M.pool);
    return (struct MonoResult){
        .bodies = M.globals,
        .types = M.types,
    };
}
