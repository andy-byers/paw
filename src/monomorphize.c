// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include "type_folder.h"
#include "unify.h"

struct GenericsState {
    struct GenericsState *outer;
    struct IrTypeList *before;
    struct IrTypeList *after;
};

struct MonoCollector {
    // stack of types that need to be monomorphized
    struct IrTypeList *pending;

    // list containing a MIR node for each reachable function
    struct MirBodyList *globals;

    // list of unique ADTs encountered during monomorphization
    struct IrTypeList *types;

    struct IrTypeList *other;
    struct GenericsState *gs;
    struct MirTypeFolder *F;
    struct Compiler *C;
    struct Mir *mir;
    struct TypeMonoMap *methods;
    struct DeclMonoMap *monos;
    struct DeclMonoMap *adts;
    BodyMap *bodies;
};

DEFINE_MAP(struct Compiler, DeclMonoMap, pawP_alloc, p_hash_decl_id, p_equals_decl_id, DeclId, struct IrTypeList *)
DEFINE_MAP(struct Compiler, TypeMonoMap, pawP_alloc, pawIr_type_hash, pawIr_type_equals, struct IrType *, struct IrTypeList *)

static void log_instance(struct MonoCollector *M, const char *kind, const String *name, struct IrType *type)
{
#if defined(PAW_DEBUG_LOG)
    const char *type_string = pawIr_print_type(M->C, type);
    pawD_debug_log(ENV(M->C), "[paw.compiler.monomorphize.%s] %s @ %p: %s", kind, name->text, (void *)type, type_string);
    --ENV(M->C)->top.p;
#endif
}

static struct IrType *substitute_generic(struct IrTypeFolder *F, struct IrGeneric *t)
{
    struct IrType **pa, **pb;
    struct Substitution *subst = F->ud;
    K_LIST_ZIP(subst->generics, pa, subst->types, pb) {
        if (!IrIsGeneric(*pa)) continue;
        struct IrGeneric *g = IrGetGeneric(*pa);
        if (g->did.value == t->did.value) return *pb;
    }

    return IR_CAST_TYPE(t);
}

// TODO: This function is a workaround. type should always be copied in pawIr_fold_type, i.e.
// TODO: in the Fold* functions in type_folder.c. Don't want to modify the original type.
static struct IrType *copy_type(struct Compiler *C, struct IrType *type)
{
    struct IrType *copy = pawIr_new_type(C);
    *copy = *type;
    return copy;
}

struct IrType *finalize_type(struct MonoCollector *M, struct IrType *type)
{
    struct IrType *old_type = type;
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
            type = copy_type(M->C, type);
            type = pawIr_fold_type(&F, type);
        }
        gs = gs->outer;
    }
    if (!IrIsSignature(old_type)) return type;
    struct IrSignature *old = IrGetSignature(old_type);
    if (old->self != NULL) {
        struct IrSignature *t = IrGetSignature(type);
        t->self = finalize_type(M, old->self);
    }
    return type;
}

static struct MirRegisterList *copy_register_list(struct MonoCollector *M, struct MirRegisterList *list)
{
    const MirRegister *pr;
    struct MirRegisterList *result = pawMir_register_list_new(M->C);
    K_LIST_FOREACH(list, pr) {
        K_LIST_PUSH(M->C, result, *pr);
    }
    return result;
}

static void copy_phi(struct MonoCollector *M, struct MirPhi *x, struct MirPhi *r)
{
    r->inputs = copy_register_list(M, x->inputs);
}

static void copy_call(struct MonoCollector *M, struct MirCall *x, struct MirCall *r)
{
    r->args = copy_register_list(M, x->args);
}

static void copy_switch(struct MonoCollector *M, struct MirSwitch *t, struct MirSwitch *r)
{
    struct MirSwitchArm *parm;
    r->arms = pawMir_switch_list_new(M->C);
    K_LIST_FOREACH(t->arms, parm) {
        K_LIST_PUSH(M->C, r->arms, *parm);
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
        case kMirCall:
            copy_call(M, MirGetCall(instr), MirGetCall(r));
            break;
        case kMirSwitch:
            copy_switch(M, MirGetSwitch(instr), MirGetSwitch(r));
            break;
        default:
            break;
    }
    return r;
}

static struct MirBlockData *copy_basic_block(struct MonoCollector *M, struct MirBlockData *block)
{
    struct MirBlockData *result = pawMir_new_block(M->mir);
    result->mid = block->mid;

    const MirBlock *pb;
    K_LIST_FOREACH(block->predecessors, pb) {
        K_LIST_PUSH(M->C, result->predecessors, *pb);
    }
    K_LIST_FOREACH(block->successors, pb) {
        K_LIST_PUSH(M->C, result->successors, *pb);
    }

    struct MirInstruction **pinstr;
    K_LIST_FOREACH(block->joins, pinstr) {
        struct MirInstruction *r = copy_instruction(M, *pinstr);
        K_LIST_PUSH(M->C, result->joins, r);
    }
    K_LIST_FOREACH(block->instructions, pinstr) {
        struct MirInstruction *r = copy_instruction(M, *pinstr);
        K_LIST_PUSH(M->C, result->instructions, r);
    }
    return result;
}

static void enter_generics_context(struct MonoCollector *M, struct GenericsState *gs, struct IrTypeList *generics, struct IrTypeList *types)
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

static struct Mir *new_mir(struct MonoCollector *M, struct Mir *base, struct IrType *type, struct IrType *self)
{
    M->mir = pawMir_new(M->C, base->name, type, self, base->fn_kind,
            base->is_native, base->is_pub, PAW_FALSE);
    return M->mir;
}

static struct MirRegisterData copy_register(struct MonoCollector *M, struct MirRegisterData reg)
{
    struct IrType *type = reg.type;
    if (reg.self != NULL) {
        // determine the "Self" type and look up the concrete method
        struct IrType *self = finalize_type(M, reg.self);
        struct HirDecl *decl = pawHir_get_decl(M->C, IR_TYPE_DID(reg.type));
        type = pawP_find_method(M->C, self, decl->hdr.name);
        if (type == NULL) NAME_ERROR(M->C, "cannot find trait method");
    }
    type = finalize_type(M, type);
    struct MirRegisterData result = reg;
    result.type = type;
    return result;
}

static void do_monomorphize(struct MonoCollector *M, struct Mir *base, struct Mir *inst, struct IrType *self)
{
    {
        struct MirRegisterData *pfrom;
        K_LIST_FOREACH(base->registers, pfrom) {
            struct MirRegisterData to = copy_register(M, *pfrom);
            K_LIST_PUSH(M->C, inst->registers, to);
        }
    }

    {
        struct MirBlockData *const *pfrom;
        K_LIST_FOREACH(base->blocks, pfrom) {
            struct MirBlockData *to = copy_basic_block(M, *pfrom);
            K_LIST_PUSH(M->C, inst->blocks, to);
        }
    }

    {
        const MirRegister *pr;
        K_LIST_FOREACH(base->locals, pr) {
            K_LIST_PUSH(M->C, inst->locals, *pr);
        }
    }

    {
        const struct MirCaptureInfo *pci;
        K_LIST_FOREACH(base->captured, pci) {
            K_LIST_PUSH(M->C, inst->captured, *pci);
        }
    }

    {
        const struct MirUpvalueInfo *pup;
        K_LIST_FOREACH(base->upvalues, pup) {
            K_LIST_PUSH(M->C, inst->upvalues, *pup);
        }
    }

    {
        // monomorphize nested closures
        struct Mir *const *pchild;
        K_LIST_FOREACH(base->children, pchild) {
            struct Mir *base_child = *pchild;
            struct IrType *inst_type = finalize_type(M, base_child->type);
            struct Mir *inst_child = new_mir(M, base_child, inst_type, NULL);
            do_monomorphize(M, base_child, inst_child, NULL);
            K_LIST_PUSH(M->C, inst->children, inst_child);
        }
    }
}

static paw_Bool is_monomorphic(const struct Mir *mir)
{
    return (mir->self == NULL || IR_TYPE_SUBTYPES(mir->self) == NULL)
        && ir_signature_types(mir->type) == NULL;
}

static struct Mir *monomorphize_function_aux(struct MonoCollector *M, struct Mir *base, struct IrSignature *sig, struct IrType *self)
{
    if (is_monomorphic(base)) return base;

    struct GenericsState gs;
    enter_generics_context(M, &gs, ir_signature_types(base->type), sig->types);

    struct Mir *inst = new_mir(M, base, IR_CAST_TYPE(sig), self);
    do_monomorphize(M, base, inst, self);
    log_instance(M, "fn", base->name, IR_CAST_TYPE(sig));

    leave_generics_context(M);
    return inst;
}

static struct Mir *monomorphize_method_aux(struct MonoCollector *M, struct Mir *base, struct IrSignature *sig, struct IrType *self)
{
    struct HirDecl *base_decl = pawHir_get_decl(M->C, IR_TYPE_DID(sig->self));
    struct IrType *base_type = GET_NODE_TYPE(M->C, base_decl);
    struct IrTypeList *base_binder = IR_TYPE_SUBTYPES(base_type);

    if (base_binder == NULL) {
        return monomorphize_function_aux(M, base, sig, self);
    }
    struct IrTypeList *inst_binder = IR_TYPE_SUBTYPES(sig->self);

    struct GenericsState gs;
    enter_generics_context(M, &gs, base_binder, inst_binder);
    struct Mir *inst = monomorphize_function_aux(M, base, sig, self);
    leave_generics_context(M);
    return inst;
}

static paw_Bool test_types(struct MonoCollector *M, struct IrType *a, struct IrType *b)
{
    return pawU_equals(M->C->U, a, b);
}

static struct IrType *cannonicalize_func(struct MonoCollector *M, struct IrTypeList *monos, struct IrType *type)
{
    struct IrType *const *pmono;
    K_LIST_FOREACH(monos, pmono) {
        if (test_types(M, type, *pmono)) return *pmono;
    }

    K_LIST_PUSH(M->C, M->pending, type);
    K_LIST_PUSH(M->C, monos, type);
    return type;
}

static struct IrType *cannonicalize_method(struct MonoCollector *M, struct IrType *self, struct IrTypeList *monos, const String *name, struct IrType *type)
{
    struct IrType *const *pmono;
    K_LIST_FOREACH(monos, pmono) {
        struct HirDecl *decl = pawHir_get_decl(M->C, IR_TYPE_DID(*pmono));
        if (pawS_eq(name, decl->hdr.name) && test_types(M, type, *pmono)) {
            return *pmono;
        }
    }

    K_LIST_PUSH(M->C, M->pending, type);
    K_LIST_PUSH(M->C, monos, type);
    return type;
}

static struct IrTypeList *mono_list_for_type(struct MonoCollector *M, TypeMonoMap *lists, struct IrType *type)
{
    struct IrTypeList **plist = TypeMonoMap_get(M->C, lists, type);
    if (plist != NULL) return *plist;

    struct IrTypeList *monos = pawIr_type_list_new(M->C);
    TypeMonoMap_insert(M->C, lists, type, monos);
    return monos;
}

static struct IrTypeList *mono_list_for_decl(struct MonoCollector *M, DeclMonoMap *lists, DeclId did)
{
    struct IrTypeList **plist = DeclMonoMap_get(M->C, lists, did);
    if (plist != NULL) return *plist;

    struct IrTypeList *monos = pawIr_type_list_new(M->C);
    DeclMonoMap_insert(M->C, lists, did, monos);
    return monos;
}

static struct IrType *cannonicalize_adt(struct MonoCollector *M, struct IrTypeList *monos, struct IrType *type)
{
    struct IrType *const *pmono;
    K_LIST_FOREACH(monos, pmono) {
        if (test_types(M, type, *pmono)) return *pmono;
    }
    K_LIST_PUSH(M->C, M->types, type);
    K_LIST_PUSH(M->C, monos, type);
    return type;
}

static struct IrType *collect_adt(struct MonoCollector *M, struct IrAdt *t)
{
    struct IrTypeList *monos = mono_list_for_decl(M, M->adts, t->did);
    return cannonicalize_adt(M, monos, IR_CAST_TYPE(t));
}

static struct IrType *register_function(struct MonoCollector *M, struct IrSignature *t)
{
    struct HirDecl *decl = pawHir_get_decl(M->C, t->did);
    if (HirIsVariantDecl(decl)) return collect_adt(M, IrGetAdt(t->result));
    struct IrTypeList *monos = mono_list_for_decl(M, M->monos, t->did);
    return cannonicalize_func(M, monos, IR_CAST_TYPE(t));
}

static struct IrType *register_method(struct MonoCollector *M, struct IrSignature *t)
{
    t->self = IrIsAdt(t->self) ? collect_adt(M, IrGetAdt(t->self)) : t->self;
    struct IrTypeList *monos = mono_list_for_type(M, M->methods, t->self);
    struct Mir *base = *BodyMap_get(M->C, M->bodies, t->did); // must exist
    return cannonicalize_method(M, t->self, monos, base->name, IR_CAST_TYPE(t));
}

static struct IrType *collect_signature(struct MonoCollector *M, struct IrSignature *t)
{
    if (t->self != NULL) return register_method(M, t);
    return register_function(M, t);
}

static struct Mir *monomorphize(struct MonoCollector *M, struct IrType *type)
{
    struct IrSignature *t = IrGetSignature(type);
    struct Mir *base = *BodyMap_get(M->C, M->bodies, t->did); // must exist
    if (!base->is_poly) return base;

    return t->self != NULL
        ? monomorphize_method_aux(M, base, t, t->self)
        : monomorphize_function_aux(M, base, t, NULL);
}

static struct IrType *collect_other(struct MonoCollector *M, struct IrType *type)
{
    struct IrType *const *ptarget;
    K_LIST_FOREACH(M->other, ptarget) {
        if (pawU_equals(M->C->U, *ptarget, type)) return *ptarget;
    }
    K_LIST_PUSH(M->C, M->other, type);
    return type;
}

static struct IrType *collect_type(struct IrTypeFolder *F, struct IrType *type)
{
    struct MirTypeFolder *outer = F->ud;
    struct MonoCollector *M = outer->ud;

    paw_assert(!IrIsGeneric(type) && !IrIsInfer(type));
    if (IrIsAdt(type)) return collect_adt(M, IrGetAdt(type));
    if (IrIsSignature(type)) return collect_signature(M, IrGetSignature(type));
    return collect_other(M, type);
}

static paw_Bool is_entrypoint(const struct Mir *mir)
{
    return mir->is_pub && !mir->is_poly
        && (mir->self == NULL || IR_TYPE_SUBTYPES(mir->self) == NULL);
}

struct MonoResult pawP_monomorphize(struct Compiler *C, BodyMap *bodies)
{
    struct MirTypeFolder F;
    struct MonoCollector M = {
        .pending = pawIr_type_list_new(C),
        .globals = pawMir_body_list_new(C),
        .types = pawIr_type_list_new(C),
        .other = pawIr_type_list_new(C),
        .bodies = bodies,
        .F = &F,
        .C = C,
    };
    M.methods = TypeMonoMap_new(C);
    M.monos = DeclMonoMap_new(C);
    M.adts = DeclMonoMap_new(C);
    pawU_enter_binder(C->U);

    pawMir_type_folder_init(&F, C, NULL, &M);
    F.F.FoldType = collect_type;

    // discover functions reachable from the toplevel
    BodyMapIterator iter;
    BodyMapIterator_init(bodies, &iter);
    while (BodyMapIterator_is_valid(&iter)) {
        struct Mir *mir = *BodyMapIterator_valuep(&iter);
        if (is_entrypoint(mir)) {
            mir->type = collect_type(&M.F->F, mir->type);
        }
        BodyMapIterator_next(&iter);
    }

    // iterate until monomorphization is complete (every function signature has
    // an MIR body in M.globals)
    while (M.pending->count > 0) {
        struct IrType *type = K_LIST_LAST(M.pending);
        K_LIST_POP(M.pending);
        struct Mir *body = monomorphize(&M, type);
        K_LIST_PUSH(C, M.globals, body);
        M.mir = M.F->V.mir = body;
        pawMir_fold(M.F, body);
    }

    pawU_leave_binder(C->U);
    DeclMonoMap_delete(C, M.adts);
    DeclMonoMap_delete(C, M.monos);
    TypeMonoMap_delete(C, M.methods);

    struct IrType *const *pother;
    K_LIST_FOREACH(M.other, pother) {
        K_LIST_PUSH(C, M.types, *pother);
    }

    return (struct MonoResult){
        .bodies = M.globals,
        .types = M.types,
    };
}

