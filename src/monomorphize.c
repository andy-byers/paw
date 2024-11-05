// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ir_type.h"
#include "map.h"
#include "mir.h"
#include "type_folder.h"

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
    Map *bodies;
    Map *monos;
    Map *methods;
    Map *adts;
};

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
            pawP_init_substitution_folder(&F, M->C, &subst,
                    gs->before, gs->after);
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

static struct MirRegister *copy_register(struct MonoCollector *M, struct MirRegister *reg)
{
    struct IrType *type = finalize_type(M, reg->type);
    return pawMir_new_register(M->C, reg->value, type);
}

static struct MirRegisterList *copy_register_list(struct MonoCollector *M, struct MirRegisterList *list)
{
    struct MirRegisterList *result = pawMir_register_list_new(M->C);
    for (int i = 0; i < list->count; ++i) {
        struct MirRegister *r = copy_register(M, K_LIST_GET(list, i));
        K_LIST_PUSH(M->C, result, r);
    }
    return result;
}

static void CopyLocal(struct MonoCollector *M, struct MirLocal *x, struct MirLocal *r)
{
    r->output = copy_register(M, x->output);
    r->target = copy_register(M, x->target);
}

static void CopyGlobal(struct MonoCollector *M, struct MirGlobal *x, struct MirGlobal *r)
{
    r->output = copy_register(M, x->output);
}

static void CopyConstant(struct MonoCollector *M, struct MirConstant *x, struct MirConstant *r)
{
    r->output = copy_register(M, x->output);
    r->code = x->code;
    r->value = x->value;
}

static void CopyAggregate(struct MonoCollector *M, struct MirAggregate *x, struct MirAggregate *r)
{
    r->output = copy_register(M, x->output);
    r->nfields = x->nfields;
}

static void CopyExplode(struct MonoCollector *M, struct MirExplode *x, struct MirExplode *r)
{
    r->input = copy_register(M, x->input);
    r->outputs = copy_register_list(M, x->outputs);
}

static void CopyContainer(struct MonoCollector *M, struct MirContainer *x, struct MirContainer *r)
{
    r->output = copy_register(M, x->output);
    r->nelems = x->nelems;
}

static void CopyUpvalue(struct MonoCollector *M, struct MirUpvalue *x, struct MirUpvalue *r)
{
    r->output = copy_register(M, x->output);
    r->index = x->index;
}

static void CopySetUpvalue(struct MonoCollector *M, struct MirSetUpvalue *x, struct MirSetUpvalue *r)
{
    r->value = copy_register(M, x->value);
    r->index = x->index;
}

static void CopyAllocLocal(struct MonoCollector *M, struct MirAllocLocal *x, struct MirAllocLocal *r)
{
    r->output = copy_register(M, x->output);
    r->name = x->name;
}

static void CopyFreeLocal(struct MonoCollector *M, struct MirFreeLocal *x, struct MirFreeLocal *r)
{
    r->reg = copy_register(M, x->reg);
}

static void CopyEnterScope(struct MonoCollector *M, struct MirEnterScope *x, struct MirEnterScope *r)
{
    r->scope_id = x->scope_id;
}

static void CopyLeaveScope(struct MonoCollector *M, struct MirLeaveScope *x, struct MirLeaveScope *r)
{
    r->scope_id = x->scope_id;
}

static void CopyAssign(struct MonoCollector *M, struct MirAssign *x, struct MirAssign *r)
{
    r->place = x->place;
    r->is_upvalue = x->is_upvalue;
    r->rhs = copy_register(M, x->rhs);
}

static void CopyCall(struct MonoCollector *M, struct MirCall *x, struct MirCall *r)
{
// TODO: need to select the correct method at call sites inside polymorphic functions
//       it is possible to select a method of the same name from a different impl block based on the 'self' type
//       factor out method lookup logic and re-lookup methods at MirCall nodes
//
//       do the preceeding things right here!
    r->output = copy_register(M, x->output);
    r->target = copy_register(M, x->target);
    r->args = copy_register_list(M, x->args);
}

static void CopyCast(struct MonoCollector *M, struct MirCast *x, struct MirCast *r)
{
    r->output = copy_register(M, x->output);
    x->type = x->type;
    r->target = copy_register(M, x->target);
}

static void CopyClosure(struct MonoCollector *M, struct MirClosure *x, struct MirClosure *r)
{
    r->child_id = x->child_id;
    r->output = copy_register(M, x->output);
}

static void CopyGetElement(struct MonoCollector *M, struct MirGetElement *x, struct MirGetElement *r)
{
    r->output = copy_register(M, x->output);
    r->object = copy_register(M, x->object);
    r->key = copy_register(M, x->key);
}

static void CopySetElement(struct MonoCollector *M, struct MirSetElement *x, struct MirSetElement *r)
{
    r->object = copy_register(M, x->object);
    r->key = copy_register(M, x->key);
    r->value = copy_register(M, x->value);
    r->is_init = x->is_init;
}

static void CopyGetRange(struct MonoCollector *M, struct MirGetRange *x, struct MirGetRange *r)
{
    r->output = copy_register(M, x->output);
    r->object = copy_register(M, x->object);
    r->lower = copy_register(M, x->lower);
    r->upper = copy_register(M, x->upper);
}

static void CopySetRange(struct MonoCollector *M, struct MirSetRange *x, struct MirSetRange *r)
{
    r->object = copy_register(M, x->object);
    r->lower = copy_register(M, x->lower);
    r->upper = copy_register(M, x->upper);
    r->value = copy_register(M, x->value);
}

static void CopyGetField(struct MonoCollector *M, struct MirGetField *x, struct MirGetField *r)
{
    r->output = copy_register(M, x->output);
    r->object = copy_register(M, x->object);
    r->index = x->index;
}

static void CopySetField(struct MonoCollector *M, struct MirSetField *x, struct MirSetField *r)
{
    r->object = copy_register(M, x->object);
    r->index = x->index;
    r->value = copy_register(M, x->value);
    r->is_init = x->is_init;
}

static void CopyUnaryOp(struct MonoCollector *M, struct MirUnaryOp *x, struct MirUnaryOp *r)
{
    r->output = copy_register(M, x->output);
    r->op = x->op;
    r->val = copy_register(M, x->val);
}

static void CopyBinaryOp(struct MonoCollector *M, struct MirBinaryOp *x, struct MirBinaryOp *r)
{
    r->output = copy_register(M, x->output);
    r->op = x->op;
    r->lhs = copy_register(M, x->lhs);
    r->rhs = copy_register(M, x->rhs);
}

static void CopyDiscard(struct MonoCollector *M, struct MirDiscard *t, struct MirDiscard *r)
{
    r->reg = copy_register(M, t->reg);
}

static void CopyReturn(struct MonoCollector *M, struct MirReturn *t, struct MirReturn *r)
{
    if (t->value != NULL) r->value = copy_register(M, t->value);
}

static void CopyForLoop(struct MonoCollector *M, struct MirForLoop *t, struct MirForLoop *r)
{
    r->for_kind = t->for_kind;
    r->var = copy_register(M, t->var);
    r->iter = copy_register(M, t->iter);
    r->end = copy_register(M, t->end);
    r->step = copy_register(M, t->step);
    r->then_arm = t->then_arm;
    r->else_arm = t->else_arm;
}

static void CopyBranch(struct MonoCollector *M, struct MirBranch *t, struct MirBranch *r)
{
    r->cond = copy_register(M, t->cond);
    r->then_arm = t->then_arm;
    r->else_arm = t->else_arm;
}

static void CopySwitch(struct MonoCollector *M, struct MirSwitch *t, struct MirSwitch *r)
{
    r->discr = copy_register(M, t->discr);
    r->has_otherwise = t->has_otherwise;
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
    struct MirInstruction *result = pawMir_new_instruction(M->C, instr->hdr.kind);
    result->hdr.line = instr->hdr.line;
    switch (MIR_KINDOF(instr)) {
#define DEFINE_COPY(X) case kMir##X: \
            Copy##X(M, MirGet##X(instr), MirGet##X(result)); \
            break;
        MIR_INSTRUCTION_LIST(DEFINE_COPY)
#undef DEFINE_COPY
    }
    return result;
}

static struct MirTerminator *copy_terminator(struct MonoCollector *M, struct MirTerminator *term)
{
    struct MirTerminator *result = pawMir_new_terminator(M->C, term->hdr.kind);
    switch (MIR_KINDOF(term)) {
#define DEFINE_COPY(X) case kMir##X: \
            Copy##X(M, MirGet##X(term), MirGet##X(result)); \
            break;
        MIR_TERMINATOR_LIST(DEFINE_COPY)
#undef DEFINE_COPY
    }
    return result;
}

static struct MirBlock *copy_basic_block(struct MonoCollector *M, struct MirBlock *block)
{
    struct MirBlock *result = pawMir_new_block(M->C, block->bid);
    for (int i = 0; i < block->code->count; ++i) {
        struct MirInstruction *r = copy_instruction(M, K_LIST_GET(block->code, i));
        K_LIST_PUSH(M->C, result->code, r);
    }
    result->term = copy_terminator(M, block->term);
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
    return pawMir_new(M->C, base->name, type, self, base->fn_kind, base->is_native, base->is_pub, PAW_FALSE);
}

static void do_monomorphize(struct MonoCollector *M, struct Mir *base, struct Mir *inst, struct IrType *self)
{
    for (int i = 0; i < base->blocks->count; ++i) {
        struct MirBlock *base_block = K_LIST_GET(base->blocks, i);
        struct MirBlock *inst_block = copy_basic_block(M, base_block);
        K_LIST_PUSH(M->C, inst->blocks, inst_block);
    }

    for (int i = 0; i < base->scopes->count; ++i) {
        struct MirScope scope = K_LIST_GET(base->scopes, i);
        K_LIST_PUSH(M->C, inst->scopes, scope);
    }

    for (int i = 0; i < base->upvalues->count; ++i) {
        struct MirUpvalueInfo src = K_LIST_GET(base->upvalues, i);
        K_LIST_PUSH(M->C, inst->upvalues, src);
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
    return (mir->self == NULL || ir_adt_types(mir->self) == NULL)
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

// TODO: put in header
struct IrType *pawP_generalize_self(struct Compiler *C, struct IrType *self, struct IrTypeList *base_binder, struct IrTypeList **pinst_binder);

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
        if (pawS_eq(name, decl->hdr.name) &&
                test_types(M, type, inst)) {
            return inst;
        }
    }

    K_LIST_PUSH(M->C, M->pending, type);
    K_LIST_PUSH(M->C, monos, type);
    return type;
}

static struct IrTypeList *get_mono_list(struct MonoCollector *M, Map *lists, Value key)
{
    const Value *pval = pawH_get(lists, key);
    if (pval != NULL) return pval->p;

    struct IrTypeList *monos = pawIr_type_list_new(M->C);
    pawH_insert(ENV(M->C), lists, key, P2V(monos));
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
    struct IrTypeList *monos = get_mono_list(M, M->adts, I2V(t->did.value));
    return cannonicalize_adt(M, monos, IR_CAST_TYPE(t));
}

static struct IrType *register_function(struct MonoCollector *M, struct IrSignature *t)
{
    struct IrTypeList *monos = get_mono_list(M, M->monos, I2V(t->did.value));
    return cannonicalize_func(M, monos, IR_CAST_TYPE(t));
}

static struct IrType *register_method(struct MonoCollector *M, struct IrSignature *t)
{
    // 'mono' list is associated with the ADT that the impl block containing this
    // method was defined on, so that associated functions using generic parameters
    // from the impl block binder can be specialized properly
    struct IrType *self = pawP_get_self(M->C, t);
    self = collect_adt(M, IrGetAdt(self));
    pawP_set_self(M->C, t, self);

    struct IrTypeList *monos = get_mono_list(M, M->methods, P2V(self));
    struct Mir *base = pawH_get(M->bodies, I2V(t->did.value))->p;
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
    struct Mir *base = pawH_get(M->bodies, I2V(t->did.value))->p;
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

static void add_builtin_adt(struct MirTypeFolder *F, paw_Type code)
{
    struct MonoCollector *M = F->ud;
    struct IrType *type = pawIr_get_type(M->C, (HirId){code});
    struct IrTypeList *monos = get_mono_list(M, M->adts, I2V(code));
    K_LIST_PUSH(M->C, M->types, type);
}

static paw_Bool is_entrypoint(const struct Mir *mir)
{
    return mir->is_pub && !mir->is_poly
        && (mir->self == NULL || ir_adt_types(mir->self) == NULL);
}

struct MonoResult pawP_monomorphize(struct Compiler *C, Map *bodies)
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
    M.methods = pawP_push_map(C);
    M.monos = pawP_push_map(C);
    M.adts = pawP_push_map(C);
    pawU_enter_binder(C->U);

    pawMir_type_folder_init(&F, C, &M);
    F.F.FoldType = collect_type;

    add_builtin_adt(&F, PAW_TUNIT);
    add_builtin_adt(&F, PAW_TBOOL);
    add_builtin_adt(&F, PAW_TINT);
    add_builtin_adt(&F, PAW_TFLOAT);
    add_builtin_adt(&F, PAW_TSTR);

    // discover functions reachable from the toplevel
    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(bodies, &iter)) {
        struct Mir *mir = pawH_value(bodies, iter)->p;
        if (is_entrypoint(mir)) {
            mir->type = collect_type(&M.F->F, mir->type);
        }
    }

    // iterate until monomorphization is complete (every function signature in the codegen
    // unit has an MIR body contained in M.globals)
    while (M.pending->count > 0) {
        struct IrType *type = K_LIST_LAST(M.pending);
        K_LIST_POP(M.pending);
        struct Mir *body = monomorphize(&M, type);
        K_LIST_PUSH(C, M.globals, body);
        pawMir_fold(M.F, body);
    }

    pawU_leave_binder(C->U);
    pawP_pop_object(C, M.adts);
    pawP_pop_object(C, M.monos);
    pawP_pop_object(C, M.methods);

    for (int i = 0; i < M.other->count; ++i) {
        K_LIST_PUSH(C, M.types, K_LIST_GET(M.other, i));
    }

    return (struct MonoResult){
        .bodies = M.globals,
        .types = M.types,
    };
}

