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

    // list containing a MIR node for each reachable function in the codegen unit
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

struct IrType *finalize_type(struct MonoCollector *M, struct IrType *type)
{
    struct IrType *old_type = type;
    struct IrTypeFolder F;
    struct Substitution subst;
    struct GenericsState *gs = M->gs;
    while (gs != NULL) {
        if (gs->before != NULL) {
            pawP_init_substitution_folder(&F, M->C, &subst, gs->before, gs->after);
            type = pawIr_fold_type(&F, type);
        }
        gs = gs->outer;
    }
    if (!IrIsSignature(old_type)) return type;
    struct IrSignature *old = IrGetSignature(old_type);
    if (!pawP_is_assoc_fn(M->C, old)) return type;
    struct IrType *self = pawP_get_self(M->C, old);
    if (self == NULL) return type;
    pawP_set_self(M->C, IrGetSignature(type),
            finalize_type(M, self));
    return type;
}

static struct MirRegisterList *copy_register_list(struct MonoCollector *M, struct MirRegisterList *list)
{
    struct MirRegisterList *result = pawMir_register_list_new(M->C);
    for (int i = 0; i < list->count; ++i) {
        const MirRegister r = K_LIST_GET(list, i);
        K_LIST_PUSH(M->C, result, r);
    }
    return result;
}

static void CopyPhi(struct MonoCollector *M, struct MirPhi *x, struct MirPhi *r)
{
    r->var_id = x->var_id;
    r->output = x->output;
    r->inputs = pawMir_register_list_new(M->C);

    const MirRegister *pr;
    K_LIST_RESERVE(M->C, r->inputs, x->inputs->count);
    K_LIST_FOREACH(x->inputs, pr) {
        K_LIST_PUSH(M->C, r->inputs, *pr);
    }
}

static void CopyMove(struct MonoCollector *M, struct MirMove *x, struct MirMove *r)
{
    r->output = x->output;
    r->target = x->target;
}

static void CopyGlobal(struct MonoCollector *M, struct MirGlobal *x, struct MirGlobal *r)
{
    r->output = x->output;
}

static void CopyConstant(struct MonoCollector *M, struct MirConstant *x, struct MirConstant *r)
{
    r->output = x->output;
    r->b_kind = x->b_kind;
    r->value = x->value;
}

static void CopyAggregate(struct MonoCollector *M, struct MirAggregate *x, struct MirAggregate *r)
{
    r->output = x->output;
    r->nfields = x->nfields;
}

static void CopyContainer(struct MonoCollector *M, struct MirContainer *x, struct MirContainer *r)
{
    r->b_kind = x->b_kind;
    r->output = x->output;
    r->nelems = x->nelems;
}

static void CopyUpvalue(struct MonoCollector *M, struct MirUpvalue *x, struct MirUpvalue *r)
{
    r->output = x->output;
    r->index = x->index;
}

static void CopySetLocal(struct MonoCollector *M, struct MirSetLocal *x, struct MirSetLocal *r)
{
    r->value = x->value;
    r->target = x->target;
}

static void CopySetUpvalue(struct MonoCollector *M, struct MirSetUpvalue *x, struct MirSetUpvalue *r)
{
    r->value = x->value;
    r->index = x->index;
}

static void CopyAllocLocal(struct MonoCollector *M, struct MirAllocLocal *x, struct MirAllocLocal *r)
{
    r->output = x->output;
    r->name = x->name;
}

static void CopyFreeLocal(struct MonoCollector *M, struct MirFreeLocal *x, struct MirFreeLocal *r)
{
    r->reg = x->reg;
}

static void CopyCall(struct MonoCollector *M, struct MirCall *x, struct MirCall *r)
{
    r->output = x->output;
    r->target = x->target;
    r->args = copy_register_list(M, x->args);
}

static void CopyCast(struct MonoCollector *M, struct MirCast *x, struct MirCast *r)
{
    r->output = x->output;
    r->target = x->target;
    r->from = x->from;
    r->to = x->to;
}

static void CopyClose(struct MonoCollector *M, struct MirClose *x, struct MirClose *r)
{
    r->target = x->target;
}

static void CopyClosure(struct MonoCollector *M, struct MirClosure *x, struct MirClosure *r)
{
    r->child_id = x->child_id;
    r->output = x->output;
}

static void CopyGetElement(struct MonoCollector *M, struct MirGetElement *x, struct MirGetElement *r)
{
    r->b_kind = x->b_kind;
    r->output = x->output;
    r->object = x->object;
    r->key = x->key;
}

static void CopySetElement(struct MonoCollector *M, struct MirSetElement *x, struct MirSetElement *r)
{
    r->b_kind = x->b_kind;
    r->object = x->object;
    r->key = x->key;
    r->value = x->value;
}

static void CopyGetRange(struct MonoCollector *M, struct MirGetRange *x, struct MirGetRange *r)
{
    r->b_kind = x->b_kind;
    r->output = x->output;
    r->object = x->object;
    r->lower = x->lower;
    r->upper = x->upper;
}

static void CopySetRange(struct MonoCollector *M, struct MirSetRange *x, struct MirSetRange *r)
{
    r->b_kind = x->b_kind;
    r->object = x->object;
    r->lower = x->lower;
    r->upper = x->upper;
    r->value = x->value;
}

static void CopyGetField(struct MonoCollector *M, struct MirGetField *x, struct MirGetField *r)
{
    r->output = x->output;
    r->object = x->object;
    r->index = x->index;
}

static void CopySetField(struct MonoCollector *M, struct MirSetField *x, struct MirSetField *r)
{
    r->object = x->object;
    r->index = x->index;
    r->value = x->value;
}

static void CopyUnaryOp(struct MonoCollector *M, struct MirUnaryOp *x, struct MirUnaryOp *r)
{
    r->output = x->output;
    r->op = x->op;
    r->val = x->val;
}

static void CopyBinaryOp(struct MonoCollector *M, struct MirBinaryOp *x, struct MirBinaryOp *r)
{
    r->output = x->output;
    r->op = x->op;
    r->lhs = x->lhs;
    r->rhs = x->rhs;
}

static void CopyReturn(struct MonoCollector *M, struct MirReturn *t, struct MirReturn *r)
{
    r->value = t->value;
}

static void CopyBranch(struct MonoCollector *M, struct MirBranch *t, struct MirBranch *r)
{
    r->cond = t->cond;
    r->then_arm = t->then_arm;
    r->else_arm = t->else_arm;
}

static void CopySwitch(struct MonoCollector *M, struct MirSwitch *t, struct MirSwitch *r)
{
    r->discr = t->discr;
    r->otherwise = t->otherwise;
    r->arms = pawMir_switch_list_new(M->C);
    for (int i = 0; i < t->arms->count; ++i) {
        struct MirSwitchArm arm = K_LIST_GET(t->arms, i);
        K_LIST_PUSH(M->C, r->arms, arm);
    }
}

static void CopyGoto(struct MonoCollector *M, struct MirGoto *t, struct MirGoto *r)
{
    r->target = t->target;
}

static struct MirInstruction *copy_instruction(struct MonoCollector *M, struct MirInstruction *instr)
{
    // TODO: most of the above functions can be eliminated by just copying the whole struct
    //       most things are trivially copiable. handle lists in the switch below.
    struct MirInstruction *result = pawMir_new_instruction(M->mir);
    result->hdr = instr->hdr;
    switch (MIR_KINDOF(instr)) {
#define DEFINE_COPY(X) case kMir##X: \
            Copy##X(M, MirGet##X(instr), MirGet##X(result)); \
            break;
        MIR_INSTRUCTION_LIST(DEFINE_COPY)
#undef DEFINE_COPY
    }
    return result;
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
    M->mir = pawMir_new(M->C, base->name, type, self, base->fn_kind, base->is_native, base->is_pub, PAW_FALSE);
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
    for (int i = 0; i < base->registers->count; ++i) {
        struct MirRegisterData from = K_LIST_GET(base->registers, i);
        struct MirRegisterData to = copy_register(M, from);
        K_LIST_PUSH(M->C, inst->registers, to);
    }

    for (int i = 0; i < base->blocks->count; ++i) {
        struct MirBlockData *from = K_LIST_GET(base->blocks, i);
        struct MirBlockData *to = copy_basic_block(M, from);
        K_LIST_PUSH(M->C, inst->blocks, to);
    }

    {
        const MirRegister *pr;
        K_LIST_FOREACH(base->locals, pr) {
            K_LIST_PUSH(M->C, inst->locals, *pr);
        }
    }

    for (int i = 0; i < base->captured->count; ++i) {
        struct MirCaptureInfo ci = K_LIST_GET(base->captured, i);
        K_LIST_PUSH(M->C, inst->captured, ci);
    }

    for (int i = 0; i < base->upvalues->count; ++i) {
        struct MirUpvalueInfo up = K_LIST_GET(base->upvalues, i);
        K_LIST_PUSH(M->C, inst->upvalues, up);
    }

    // monomorphize nested closures
    for (int i = 0; i < base->children->count; ++i) {
        struct Mir *base_child = K_LIST_GET(base->children, i);
        struct IrType *inst_type = finalize_type(M, base_child->type);
        struct Mir *inst_child = new_mir(M, base_child, inst_type, NULL);
        do_monomorphize(M, base_child, inst_child, NULL);
        K_LIST_PUSH(M->C, inst->children, inst_child);
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
    struct IrTypeList *inst_binder;
    struct IrTypeList *base_binder = pawP_get_binder(M->C, sig->did);
    // TODO: check earlier, so non-poly methods are not added to 'pending' at all? Happens when
    //       the the impl block has no binder, but the ADT does
    if (base_binder == NULL) return base;
    struct IrType *base_self = pawP_generalize_self(M->C, base->self, base_binder, &inst_binder);
    pawU_unify(M->C->U, base_self, self); // populate "inst_binder"

    struct GenericsState gs;
    enter_generics_context(M, &gs, base_binder, inst_binder);
    struct Mir *inst = monomorphize_function_aux(M, base, sig, self);
    leave_generics_context(M);
    return inst;
}

static paw_Bool test_types(struct MonoCollector *M, struct IrType *lhs, struct IrType *rhs)
{
    return pawU_equals(M->C->U, lhs, rhs);
}

static struct IrType *cannonicalize_func(struct MonoCollector *M, struct IrTypeList *monos, struct IrType *type)
{
    for (int i = 0; i < monos->count; ++i) {
        struct IrType *inst = K_LIST_GET(monos, i);
        if (test_types(M, type, inst)) return inst;
    }

    K_LIST_PUSH(M->C, M->pending, type);
    K_LIST_PUSH(M->C, monos, type);
    return type;
}

static struct IrType *cannonicalize_method(struct MonoCollector *M, struct IrType *self, struct IrTypeList *monos, const String *name, struct IrType *type)
{
    for (int i = 0; i < monos->count; ++i) {
        struct IrType *inst = K_LIST_GET(monos, i);
        struct HirDecl *decl = pawHir_get_decl(M->C, IR_TYPE_DID(inst));
        if (pawS_eq(name, decl->hdr.name)
                && test_types(M, type, inst)) {
            return inst;
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
    for (int i = 0; i < monos->count; ++i) {
        struct IrType *mono = K_LIST_GET(monos, i);
        if (test_types(M, type, mono)) return mono;
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
    // 'mono' list is associated with the ADT that the impl block containing this
    // method was defined on, so that associated functions using generic parameters
    // from the impl block binder can be specialized properly
    struct IrType *self = pawP_get_self(M->C, t);
    self = IrIsAdt(self) ? collect_adt(M, IrGetAdt(self)) : self;
    pawP_set_self(M->C, t, self);

    struct IrTypeList *monos = mono_list_for_type(M, M->methods, self);
    struct Mir *base = *BodyMap_get(M->C, M->bodies, t->did); // must exist
    return cannonicalize_method(M, self, monos, base->name, IR_CAST_TYPE(t));
}

static struct IrType *collect_signature(struct MonoCollector *M, struct IrSignature *t)
{
    if (pawP_is_assoc_fn(M->C, t)) {
        return register_method(M, t);
    }
    return register_function(M, t);
}

static struct Mir *monomorphize(struct MonoCollector *M, struct IrType *type)
{
    struct IrSignature *t = IrGetSignature(type);
    struct Mir *base = *BodyMap_get(M->C, M->bodies, t->did); // must exist
    if (!base->is_poly) return base;
    if (pawP_is_assoc_fn(M->C, t)) {
        struct IrType *self = pawP_get_self(M->C, t);
        return monomorphize_method_aux(M, base, t, self);
    }
    return monomorphize_function_aux(M, base, t, NULL);
}

static struct IrType *collect_other(struct MonoCollector *M, struct IrType *type)
{
    for (int i = 0; i < M->other->count; ++i) {
        struct IrType *target = K_LIST_GET(M->other, i);
        if (pawU_equals(M->C->U, target, type)) return target;
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

static void add_builtin_adt(struct MirTypeFolder *F, enum BuiltinKind code)
{
    struct MonoCollector *M = F->ud;
    const DeclId did = M->C->builtins[code].did;
    struct HirDecl *decl = pawHir_get_decl(F->C, did);
    struct IrType *type = GET_NODE_TYPE(F->C, decl);
    struct IrTypeList *monos = mono_list_for_decl(M, M->adts, did);
    K_LIST_PUSH(M->C, M->types, type);
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

    add_builtin_adt(&F, BUILTIN_UNIT);
    add_builtin_adt(&F, BUILTIN_BOOL);
    add_builtin_adt(&F, BUILTIN_INT);
    add_builtin_adt(&F, BUILTIN_FLOAT);
    add_builtin_adt(&F, BUILTIN_STR);

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

    // iterate until monomorphization is complete (every function signature in the codegen
    // unit has an MIR body contained in M.globals)
    while (M.pending->count > 0) {
        struct IrType *type = K_LIST_LAST(M.pending);
        K_LIST_POP(M.pending);
        struct Mir *body = monomorphize(&M, type);
        K_LIST_PUSH(C, M.globals, body);
        M.mir = M.F->V.mir = body; // TODO: figure out a better way to do this... always need Mir object to get basic block and register backing data
        pawMir_fold(M.F, body);
    }

    pawU_leave_binder(C->U);
    DeclMonoMap_delete(C, M.adts);
    DeclMonoMap_delete(C, M.monos);
    TypeMonoMap_delete(C, M.methods);

    for (int i = 0; i < M.other->count; ++i) {
        K_LIST_PUSH(C, M.types, K_LIST_GET(M.other, i));
    }

    return (struct MonoResult){
        .bodies = M.globals,
        .types = M.types,
    };
}

