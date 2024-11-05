// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "api.h"
#include "auxlib.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "gc.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "match.h"
#include "mir.h"
#include "mem.h"
#include "parse.h"
#include "lib.h"
#include "type.h"

#define ERROR(G, code, ...) pawE_error(ENV(G), code, -1, __VA_ARGS__)
#define PRELUDE(G) ((G)->C->dm->modules->data[0])
#define GET_DECL(G, id) pawHir_get_decl((G)->C, id)
#define TYPE_CODE(G, type) (pawP_type2code((G)->C, type))
#define REG(fs, r) ident2reg(fs, (r)->value)
#define BB(G, bid) CHECK_EXP((bid).value >= 0, K_LIST_GET((G)->mir->blocks, (bid).value))

struct JumpTarget {
    MirBlockId bid;
    int pc;
};

DEFINE_LIST_V2(struct Compiler, jumptab_, JumpTable, struct JumpTarget)

static void add_jump_target(struct Generator *G, MirBlockId bid)
{
    K_LIST_PUSH(G->C, G->jumps, ((struct JumpTarget){
                    .pc = G->fs->pc,
                    .bid = bid,
                }));
}

struct VisitBlock {
    struct MirBlock *block;
    struct MirBlockList *successors;
};

static struct VisitBlock new_visit_block(struct Generator *G, struct MirBlock *block)
{
    return (struct VisitBlock){
        .successors = pawMir_block_list_new(G->C),
        .block = block,
    };
}

DEFINE_LIST_V2(struct Compiler, visit_stack_, VisitStack, struct VisitBlock)

static struct Def *get_def(struct Generator *G, DefId did)
{
    return Y_DEF(ENV(G), did);
}

static int ident2reg(struct FuncState *fs, int ident)
{
    const Value *pval = pawH_get((fs)->G->regtab, I2V(ident));
    struct RegisterInfo *info = pval->p;
    return info->index;
}

static paw_Bool is_visited(Map *visited, MirBlockId bid)
{
    return pawH_get(visited, I2V(bid.value));
}

static void check_contiguous_pair(struct FuncState *fs, struct MirRegister *lhs, struct MirRegister *rhs)
{
    const paw_Bool cc = REG(fs, lhs) + 1 == REG(fs, rhs);
    PAW_UNUSED(cc); // unused variable in release mode
    paw_assert(cc);
}

static void collect_successors(struct Generator *G, struct VisitBlock *vb, struct MirTerminator *term)
{
    switch (MIR_KINDOF(term)) {
        case kMirGoto: {
            struct MirGoto *t = MirGetGoto(term);
            K_LIST_PUSH(G->C, vb->successors, BB(G, t->target));
            break;
        }
        case kMirForLoop: {
            struct MirForLoop *t = MirGetForLoop(term);
            K_LIST_PUSH(G->C, vb->successors, BB(G, t->then_arm));
            K_LIST_PUSH(G->C, vb->successors, BB(G, t->else_arm));
            break;
        }
        case kMirBranch: {
            struct MirBranch *t = MirGetBranch(term);
            K_LIST_PUSH(G->C, vb->successors, BB(G, t->then_arm));
            K_LIST_PUSH(G->C, vb->successors, BB(G, t->else_arm));
            break;
        }
        case kMirSwitch: {
            struct MirSwitch *t = MirGetSwitch(term);
            for (int i = 0; i < t->arms->count; ++i) {
                struct MirSwitchArm arm = K_LIST_GET(t->arms, i);
                K_LIST_PUSH(G->C, vb->successors, BB(G, arm.bid));
            }
            K_LIST_PUSH(G->C, vb->successors, BB(G, t->otherwise));
            break;
        }
        case kMirReturn:
            break;
    }
}

static void visit_postorder(struct Generator *G, Map *visited, struct VisitStack *stack, struct MirBlock *block)
{
    if (is_visited(visited, block->bid)) return;
    pawH_insert(ENV(G), visited, I2V(block->bid.value), P2V(block));
    struct VisitBlock vb = new_visit_block(G, block);
    collect_successors(G, &vb, block->term);
    K_LIST_PUSH(G->C, stack, vb);
}

static void traverse_postorder(struct Generator *G, Map *visited, struct VisitStack *stack)
{
    while (stack->count > 0) {
        struct VisitBlock *top = &K_LIST_LAST(stack);
        if (top->successors->count == 0) break;
        struct MirBlock *block = K_LIST_LAST(top->successors);
        K_LIST_POP(top->successors);
        visit_postorder(G, visited, stack, block);
    }
}

static struct Type *lookup_type(struct Generator *G, struct IrType *type)
{
    Value *pv = pawH_get(G->C->type2rtti, P2V(type));
    return pv != NULL ? pv->p : NULL;
}

static DefId type2def(struct Generator *G, struct IrType *type)
{
    struct Type *ty = lookup_type(G, type);
    paw_assert(ty != NULL && "undefined type");
    return IrIsAdt(type) ? ty->adt.did : ty->sig.did;
}

struct JumpSource {
    MirBlockId to;
    int from_pc;
};

DEFINE_LIST_V2(struct Compiler, patch_list_, PatchList, struct JumpSource)

static void add_jump_source(struct Generator *G, int from_pc, MirBlockId to)
{
    K_LIST_PUSH(G->C, G->patch, ((struct JumpSource){
        .from_pc = from_pc,
        .to = to,
    }));
}

static void remove_jump_source(struct PatchList *pl, int index)
{
    paw_assert(pl->count > 0);
    for (int i = index; i < pl->count - 1; ++i) {
        K_LIST_SET(pl, i, K_LIST_GET(pl, i + 1));
    }
    --pl->count;
}

static void patch_jump(struct FuncState *fs, int from, int to)
{
    const int dist = to - (from + 1);
    if (dist > JUMP_MAX) {
        ERROR(fs->G, PAW_ESYNTAX, "too many instructions to jump");
    }

    const Proto *p = fs->proto;
    paw_assert(0 <= from && from < p->length);
    SET_sBx(&p->source[from], dist);
}

static void patch_jumps_to_here(struct Generator *G, MirBlockId bid)
{
    struct FuncState *fs = G->fs;
    struct PatchList *pl = G->patch;
    for (int i = 0; i < pl->count;) {
        struct JumpSource js = K_LIST_GET(pl, i);
        if (js.to.value == bid.value) {
            patch_jump(fs, js.from_pc, fs->pc);
            remove_jump_source(pl, i);
        } else {
            ++i;
        }
    }
}

static ValueId type2global(struct Generator *G, struct IrType *type)
{
    const DefId def_id = type2def(G, type);
    const struct Def *def = Y_DEF(ENV(G), def_id);
    paw_assert(def->hdr.kind == DEF_FUNC);
    return def->func.vid;
}

static void mangle_type(struct Generator *G, Buffer *buf, struct IrType *type)
{
    struct Type *t = lookup_type(G, type);
    paw_assert(t != NULL);

    pawY_mangle_add_arg(ENV(G), buf, t->hdr.code);
}

static void mangle_types(struct Generator *G, Buffer *buf, const struct IrTypeList *types)
{
    if (types == NULL) return;
    pawY_mangle_start_generic_args(ENV(G), buf);
    for (int i = 0; i < types->count; ++i) {
        mangle_type(G, buf, types->data[i]);
    }
    pawY_mangle_finish_generic_args(ENV(G), buf);
}

static void mangle_start(paw_Env *P, Buffer *buf, struct Generator *G)
{
    ENSURE_STACK(P, 1);
    pawL_init_buffer(P, buf);
    pawY_mangle_start(P, buf);
}

static String *mangle_finish(paw_Env *P, Buffer *buf, struct Generator *G)
{
    pawL_push_result(P, buf);

    // anchor in compiler string table
    String *str = V_STRING(P->top.p[-1]);
    pawH_insert(P, G->C->strings, P2V(str), P2V(str));
    pawC_pop(P);
    return str;
}

static String *mangle_name(struct Generator *G, const String *modname, const String *name, struct IrTypeList *types)
{
    Buffer buf;
    paw_Env *P = ENV(G);
    mangle_start(P, &buf, G);
    if (modname != NULL) pawY_mangle_add_module(P, &buf, modname);
    pawY_mangle_add_name(P, &buf, name);
    mangle_types(G, &buf, types);
    return mangle_finish(P, &buf, G);
}

static String *mangle_attr(struct Generator *G, const String *modname, const String *base, const struct IrTypeList *base_types, const String *attr, const struct IrTypeList *attr_types)
{
    Buffer buf;
    paw_Env *P = ENV(G);
    mangle_start(P, &buf, G);
    if (modname != NULL) pawY_mangle_add_module(P, &buf, modname);
    pawY_mangle_add_name(P, &buf, base);
    mangle_types(G, &buf, base_types);
    pawY_mangle_add_name(P, &buf, attr);
    mangle_types(G, &buf, attr_types);
    return mangle_finish(P, &buf, G);
}

static String *func_name(struct Generator *G, const String *modname, struct IrType *type)
{
    struct IrSignature *fsig = IrGetSignature(type);
    const struct HirFuncDecl *fd = HirGetFuncDecl(GET_DECL(G, fsig->did));
    struct IrTypeList *fd_types = fd->body ? fsig->types : NULL;
    if (fd->self == NULL) return mangle_name(G, modname, fd->name, fd_types);
    struct IrType *self = pawP_get_self(G->C, fsig);
    const struct HirAdtDecl *ad = HirGetAdtDecl(GET_DECL(G, IR_TYPE_DID(self)));
    const struct IrTypeList *ad_types = fd->body ? ir_adt_types(self) : NULL;
    return mangle_attr(G, modname, ad->name, ad_types, fd->name, fd_types);
}

static String *adt_name(struct Generator *G, const String *modname, struct IrType *type)
{
    struct HirAdtDecl *d = HirGetAdtDecl(GET_DECL(G, IR_TYPE_DID(type)));
    return mangle_name(G, modname, d->name, ir_adt_types(type));
}

static Map *kcache_map(struct FuncState *fs, paw_Type code)
{
    if (code == PAW_TINT) {
        return fs->kcache.ints;
    } else if (code == PAW_TFLOAT) {
        return fs->kcache.flts;
    } else {
        paw_assert(code == PAW_TSTR);
        return fs->kcache.strs;
    }
}

static int add_constant(struct Generator *G, Value v, paw_Type code)
{
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;
    // 'bool' and 'int' have the same runtime representation
    if (code <= PAW_TBOOL) code = PAW_TINT;

    // share constant values within each function
    Map *kmap = kcache_map(fs, code);
    Value *pk = pawH_get(kmap, v);
    if (pk != NULL) return CAST(int, pk->i);

    if (fs->nk == CONSTANT_MAX) {
        ERROR(G, PAW_ESYNTAX, "too many constants");
    }
    pawM_grow(ENV(G), p->k, fs->nk, p->nk);
    p->k[fs->nk] = v;

    const Value id = {.i = fs->nk};
    pawH_insert(ENV(G), kmap, v, id);
    return fs->nk++;
}

static Proto *push_proto(struct Generator *G, String *name)
{
    paw_Env *P = ENV(G);
    ENSURE_STACK(P, 1);
    Value *pv = pawC_push0(P);
    Proto *proto = pawV_new_proto(P);
    V_SET_OBJECT(pv, proto);
    proto->modname = G->C->modname;
    proto->name = name;
    return proto;
}

static void pop_proto(struct Generator *G)
{
    paw_assert(O_IS_PROTO(ENV(G)->top.p[-1].o));
    --ENV(G)->top.p;
}

static void new_local(struct FuncState *fs, String *name, struct IrType *type)
{
    struct Generator *G = fs->G;
    ++fs->nlocals;
}

#define JUMP_PLACEHOLDER (-1)

static int emit_cond_jump(struct FuncState *fs, struct MirRegister *cond, Op op)
{
    pawK_code_AsBx(fs, op, REG(fs, cond), JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static int emit_jump(struct FuncState *fs)
{
    pawK_code_sBx(fs, OP_JUMP, JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static int code_switch_int(struct FuncState *fs, struct MirRegister *discr, int k)
{
    // if discr != k, skip over the jump that moves control to the body
    // of the match case
    pawK_code_AB(fs, OP_SWITCHINT, REG(fs, discr), k);
    return emit_jump(fs);
}

static void enter_kcache(struct FuncState *fs)
{
    fs->kcache.ints = pawP_push_map(fs->G->C);
    fs->kcache.strs = pawP_push_map(fs->G->C);
    fs->kcache.flts = pawP_push_map(fs->G->C);
}

static void leave_kcache(struct FuncState *fs)
{
    ENV(fs->G)->top.p -= 3;
}

static void leave_function(struct Generator *G)
{
    paw_Env *P = ENV(G);
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;

    // module itself has no prototype
    if (fs->kind == FUNC_MODULE) return;

    pawM_shrink(P, p->source, p->length, fs->pc);
    p->length = fs->pc;
    pawM_shrink(P, p->lines, p->nlines, fs->nlines);
    p->nlines = fs->nlines;
    pawM_shrink(P, p->k, p->nk, fs->nk);
    p->nk = fs->nk;

    leave_kcache(fs);

    G->fs = fs->outer;
    CHECK_GC(P);
}

static void enter_function(struct Generator *G, struct FuncState *fs, struct Mir *mir, Proto *proto)
{
    // TODO: should be stored in G->fs, since they are per-function objects
    G->patch = patch_list_new(G->C);
    G->jumps = jumptab_new(G->C);

    *fs = (struct FuncState){
        .kind = mir->fn_kind,
        .name = mir->name,
        .proto = proto,
        .outer = G->fs,
        .G = G,
    };
    G->fs = fs;

    enter_kcache(fs);
}

static ValueId resolve_function(struct Generator *G, struct Type *rtti)
{
    paw_Env *P = ENV(G);
    paw_assert(rtti->hdr.kind == TYPE_SIGNATURE);
    struct Def *def = Y_DEF(P, rtti->sig.did);
    return def->func.vid;
}

static struct ModuleInfo *get_mod(struct Generator *G, int modno)
{
    return K_LIST_GET(G->C->dm->modules, modno);
}

static const String *get_mod_prefix(struct Generator *G, struct ModuleInfo *m)
{
    // omit module prefix for target and prelude modules
    if (pawS_eq(m->hir->name, G->C->modname)) return NULL;
    if (m->hir->modno == 0) return NULL;
    return m->hir->name;
}

static const String *prefix_for_modno(struct Generator *G, int modno)
{
    struct ModuleInfo *m = get_mod(G, modno);
    return get_mod_prefix(G, m);
}

static paw_Bool is_smi(paw_Int i)
{
    return (i < 0 && i >= -sBx_MAX) ||
        (i >= 0 && i <= sBx_MAX);
}

static void code_smi(struct FuncState *fs, struct MirRegister *r, paw_Int i)
{
    paw_assert(is_smi(i));
    pawK_code_AsBx(fs, OP_LOADSMI, REG(fs, r), CAST(int, i));
}

// Wrap a Proto in a Closure so it can be called directly from C
static void set_entrypoint(struct Generator *G, Proto *proto, int g)
{
    paw_Env *P = ENV(G);
    Value *pval = Y_PVAL(P, g);
    Closure *closure = pawV_new_closure(P, 0);
    V_SET_OBJECT(pval, closure);
    closure->p = proto;
}

static void code_c_function(struct Generator *G, struct Mir *mir, int g)
{
    paw_Env *P = ENV(G);
    Value *pval = Y_PVAL(P, g);

    struct IrType *type = mir->type;
    const String *modname = prefix_for_modno(G, IR_TYPE_DID(type).modno);
    const String *mangled = func_name(G, modname, type);
    const Value *pv = pawH_get(G->builtin, P2V(mangled));
    if (pv == NULL) ERROR(G, PAW_ENAME, "C function '%s' not loaded", mir->name->text);
    *pval = *pv;
}

static struct MirBlockList *collect_postorder_traversal(struct Generator *G, struct MirBlockList *blocks)
{
    struct VisitStack *visit_stack = visit_stack_new(G->C);
    Map *visited = pawP_push_map(G->C); // push 'visited'
    visit_postorder(G, visited, visit_stack, K_LIST_GET(blocks, 0));
    traverse_postorder(G, visited, visit_stack);

    struct MirBlockList *order = pawMir_block_list_new(G->C);
    while (visit_stack->count > 0) {
        struct VisitBlock vb = K_LIST_LAST(visit_stack);
        K_LIST_POP(visit_stack);
        traverse_postorder(G, visited, visit_stack);
        K_LIST_PUSH(G->C, order, vb.block);
    }
    --ENV(G)->top.p; // pop 'visited'

    // reverse order
    struct MirBlock **a = &order->data[0];
    struct MirBlock **b = &order->data[order->count - 1];
    for (; a < b; a++, b--) {
        struct MirBlock *t = *a;
        *a = *b;
        *b = t;
    }

    for (int i = 0; i < order->count;++i){
        struct MirBlock *b = K_LIST_GET(order, i);
    }
    return order;
}

static void allocate_upvalue_info(struct Generator *G, Proto *proto, struct MirUpvalueList *upvalues)
{
    paw_Env *P = ENV(G);
    proto->u = pawM_new_vec(P, upvalues->count, struct UpValueInfo);
    proto->nup = upvalues->count;
    for (int i = 0; i < upvalues->count; ++i) {
        struct MirUpvalueInfo info = K_LIST_GET(upvalues, i);
        proto->u[i].is_local = info.is_local;
        proto->u[i].index = info.index;
    }
}

static void code_proto(struct Generator *G, struct Mir *mir, Proto *proto, int index)
{
    struct IrType *type = mir->type;
    struct IrFuncPtr *func = IR_FPTR(type);
    proto->argc = func->params->count;

    allocate_upvalue_info(G, proto, mir->upvalues);

    struct MirBlockList *order = collect_postorder_traversal(G, mir->blocks);
    G->regtab = pawP_allocate_registers(G->C, mir, order, &proto->max_stack);
    pawMir_visit_block_list(G->V, order);
    --ENV(G)->top.p; // pop 'G->regtab'

    paw_assert(G->patch->count == 0);
}

static Proto *code_paw_function(struct Generator *G, struct Mir *mir, int index);

static void code_children(struct Generator *G, Proto *parent, struct Mir *mir)
{
    const int nchildren = mir->children->count;
    parent->p = pawM_new_vec(ENV(G), nchildren, Proto *);
    for (int i = 0; i < nchildren; ++i) {
        G->mir = K_LIST_GET(mir->children, i);
        parent->p[i] = code_paw_function(G, G->mir, i);
        ++parent->nproto;
    }
}

static Proto *code_paw_function(struct Generator *G, struct Mir *mir, int index)
{
    struct FuncState fs;
    Proto *proto = push_proto(G, mir->name);
    enter_function(G, &fs, mir, proto);

    code_proto(G, mir, proto, index);
    code_children(G, proto, mir);

    leave_function(G);

    // anchor toplevel functions
    if (G->fs == NULL) {
        set_entrypoint(G, proto, index);
    }
    pop_proto(G);
    return proto;
}

static paw_Bool is_instance_call(struct IrType *type)
{
    return IrIsSignature(type) && IrGetSignature(type)->types != NULL;
}

static paw_Bool is_variant_constructor(struct Generator *G, struct IrType *type)
{
    if (IrIsSignature(type)) {
        const struct HirDecl *decl = GET_DECL(G, IrGetSignature(type)->did);
        return HirIsVariantDecl(decl);
    }
    return PAW_FALSE;
}

// Generate code for an enumerator
static void code_variant_constructor(struct Generator *G, struct MirRegister *discr, struct MirRegisterList *args, struct MirRegister *output)
{
    struct FuncState *fs = G->fs;

    struct HirDecl *decl = GET_DECL(G, IR_TYPE_DID(discr->type));
    struct HirVariantDecl *d = HirGetVariantDecl(decl);
    code_smi(fs, discr, d->index); // discriminator is a small int

    const int nargs = args != NULL ? args->count : 0;

    // at runtime, a variant is represented by a tuple '(k, e1..en)', where 'k' is
    // the discriminator and 'e1..en' are the fields
    pawK_code_ABC(fs, OP_NEWTUPLE, REG(fs, output), REG(fs, discr), 1 + nargs);
}

static paw_Bool is_method_call(struct Generator *G, struct IrType *type)
{
    if (!IrIsSignature(type)) return PAW_FALSE;
    struct HirDecl *decl = GET_DECL(G, IR_TYPE_DID(type));
    return HirGetFuncDecl(decl)->self != NULL;
}

static void prep_method_call(struct Generator *G, struct MirRegister *callable, struct MirRegister *self)
{
    struct IrType *type = callable->type;
    paw_assert(is_method_call(G, type));
    struct Type *rtti = lookup_type(G, type);
    const ValueId vid = resolve_function(G, rtti);
    pawK_code_ABx(G->fs, OP_GETGLOBAL, REG(G->fs, callable), vid);
}

static void code_items(struct Generator *G)
{
    for (int i = 0; i < G->items->count; ++i) {
        const struct ItemSlot *item = K_LIST_GET(G->items, i);
        G->mir = item->mir;
        if (G->mir->is_native) {
            code_c_function(G, G->mir, i);
        } else {
            code_paw_function(G, G->mir, i);
        }
    }
}

static void register_items(struct Generator *G)
{
    Map *bodies = pawP_lower_hir(G->C);
    struct MonoResult mr = pawP_monomorphize(G->C, bodies);
    pawP_pop_object(G->C, bodies);

    G->items = pawP_allocate_defs(G->C, mr.bodies, mr.types);

    for (int i = 0; i < G->items->count; ++i) {
        struct ItemSlot *item = K_LIST_GET(G->items, i);
        struct IrType *type = item->mir->type;

        const String *modname = prefix_for_modno(G, IR_TYPE_DID(type).modno);
        const struct Type *ty = lookup_type(G, type);
        struct FuncDef *fdef = &get_def(G, ty->sig.did)->func;
        paw_assert(fdef->kind == DEF_FUNC);
        item->name = fdef->mangled_name = func_name(G, modname, type);
    }

    paw_Env *P = ENV(G);
    paw_assert(P->vals.data == NULL);
    const int nvalues = G->items->count;
    P->vals.data = pawM_new_vec(P, nvalues, Value);
    P->vals.count = P->vals.alloc = nvalues;
}

static void code_local(struct MirVisitor *V, struct MirLocal *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    pawK_code_AB(G->fs, OP_MOVE, REG(fs, x->output), REG(fs, x->target));
}

static void code_upvalue(struct MirVisitor *V, struct MirUpvalue *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    pawK_code_AB(G->fs, OP_GETUPVALUE, REG(fs, x->output), x->index);
}

static void code_global(struct MirVisitor *V, struct MirGlobal *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    const ValueId value_id = type2global(G, x->output->type);
    pawK_code_ABx(G->fs, OP_GETGLOBAL, REG(fs, x->output), value_id);
}

static void code_constant(struct MirVisitor *V, struct MirConstant *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const int bc = add_constant(G, x->value, x->code);
    if (x->code <= PAW_TBOOL ||
            (x->code == PAW_TINT && is_smi(x->value.i))) {
        code_smi(fs, x->output, x->value.i);
    } else {
        pawK_code_ABx(G->fs, OP_LOADK, REG(fs, x->output), bc);
    }
}

static void code_set_upvalue(struct MirVisitor *V, struct MirSetUpvalue *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    pawK_code_AB(G->fs, OP_SETUPVALUE, x->index, REG(fs, x->value));
}

static void code_alloc_local(struct MirVisitor *V, struct MirAllocLocal *x)
{
    struct Generator *G = V->ud;
    new_local(G->fs, x->name, x->output->type);
}

static void code_free_local(struct MirVisitor *V, struct MirFreeLocal *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    // TODO: NOOP for now, using MirLeaveScope to close upvalues
}

static void code_enter_scope(struct MirVisitor *V, struct MirEnterScope *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
}

static void code_leave_scope(struct MirVisitor *V, struct MirLeaveScope *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    struct MirScope *scope = pawMir_get_scope(G->mir, x->scope_id);
    if (scope->needs_close) {
        pawK_code_A(fs, OP_CLOSE, fs->first_local + scope->nlocals);
    }
    fs->nlocals = scope->nlocals;
}

static void code_aggregate(struct MirVisitor *V, struct MirAggregate *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    pawK_code_AB(fs, OP_NEWTUPLE, REG(fs, x->output), x->nfields);
}

static void code_closure(struct MirVisitor *V, struct MirClosure *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    pawK_code_ABx(G->fs, OP_CLOSURE, REG(G->fs, x->output), x->child_id);
}

static void code_get_element(struct MirVisitor *V, struct MirGetElement *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const paw_Type code = TYPE_CODE(G, x->object->type);
    const Op op = code == BUILTIN_LIST ? OP_LGET :
        code == BUILTIN_MAP ? OP_MGET : OP_SGET;
    pawK_code_ABC(fs, op, REG(fs, x->output), REG(fs, x->object), REG(fs, x->key));
}

static void code_set_element(struct MirVisitor *V, struct MirSetElement *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const paw_Type code = TYPE_CODE(G, x->object->type);
    const Op op = code == BUILTIN_LIST ? OP_LSET : OP_MSET;
    pawK_code_ABC(fs, op, REG(fs, x->object), REG(fs, x->key), REG(fs, x->value));
}

static void code_get_range(struct MirVisitor *V, struct MirGetRange *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const paw_Type code = TYPE_CODE(G, x->object->type);
    const Op op = code == BUILTIN_LIST ? OP_LGETN : OP_SGETN;
    check_contiguous_pair(fs, x->lower, x->upper);
    pawK_code_ABC(fs, op, REG(fs, x->output), REG(fs, x->object), REG(fs, x->lower));
}

static void code_set_range(struct MirVisitor *V, struct MirSetRange *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    check_contiguous_pair(fs, x->lower, x->upper);
    pawK_code_ABC(fs, OP_LSETN, REG(fs, x->object), REG(fs, x->lower), REG(fs, x->value));
}

static void code_get_field(struct MirVisitor *V, struct MirGetField *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    pawK_code_ABC(fs, OP_GETFIELD, REG(fs, x->output), REG(fs, x->object), x->index);
}

static void code_set_field(struct MirVisitor *V, struct MirSetField *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    pawK_code_ABC(fs, OP_SETFIELD, REG(fs, x->object), x->index, REG(fs, x->value));
}

static void code_explode(struct MirVisitor *V, struct MirExplode *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    struct MirRegister *output = K_LIST_GET(x->outputs, 0);
    pawK_code_ABC(fs, OP_EXPLODE, REG(fs, output), REG(fs, x->input), x->outputs->count);
}

static void code_container(struct MirVisitor *V, struct MirContainer *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const paw_Type code = TYPE_CODE(G, x->output->type);
    const Op op = code == BUILTIN_LIST ? OP_NEWLIST : OP_NEWMAP;
    pawK_code_AB(fs, op, REG(fs, x->output), x->nelems);
}

static void code_assign(struct MirVisitor *V, struct MirAssign *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    if (x->is_upvalue) {
        pawK_code_AB(fs, OP_SETUPVALUE, x->place, REG(fs, x->rhs));
    } else {
        pawK_code_AB(fs, OP_MOVE, ident2reg(fs, x->place), REG(fs, x->rhs));
    }

    // TODO: upvalues must use OP_SETUPVALUE
}

static paw_Bool handle_special_calls(struct Generator *G, struct MirCall *x)
{
    struct MirRegister *callable = x->target;
    if (IrIsSignature(callable->type)) {
        struct HirDecl *decl = GET_DECL(G, IR_TYPE_DID(callable->type));
        if (HirIsVariantDecl(decl)) {
            code_variant_constructor(G, callable, x->args, x->output);
            return PAW_TRUE;
        }
//        if (HirGetFuncDecl(decl)->self != NULL) {
//            prep_method_call(G, callable, K_LIST_FIRST(x->args));
//        }
    }
    return PAW_FALSE;
}

static void code_call(struct MirVisitor *V, struct MirCall *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    paw_assert(IR_IS_FUNC_TYPE(x->target->type));

    if (handle_special_calls(G, x)) return;
    pawK_code_AB(fs, OP_CALL, REG(fs, x->target), x->args->count);
}


static void code_cast(struct MirVisitor *V, struct MirCast *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const Op op = x->type == PAW_TBOOL ? OP_BCAST :
        x->type == PAW_TINT ? OP_ICAST : OP_FCAST;
    const paw_Type from = TYPE_CODE(G, x->target->type);

    pawK_code_ABC(fs, op, REG(fs, x->output), REG(fs, x->target), from);
}

static Op unop2op_bool(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_NOT:
            return OP_NOT;
        default:
            PAW_UNREACHABLE();
    }
}

static Op unop2op_int(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_NEG:
            return OP_INEG;
        case UNARY_BNOT:
            return OP_BNOT;
        default:
            PAW_UNREACHABLE();
    }
}

static Op binop2op_bool(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return OP_IEQ;
        case BINARY_NE:
            return OP_INE;
        case BINARY_AS:
            return OP_BCAST;
        default:
            PAW_UNREACHABLE();
    }
}

static Op binop2op_int(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return OP_IEQ;
        case BINARY_NE:
            return OP_INE;
        case BINARY_LT:
            return OP_ILT;
        case BINARY_LE:
            return OP_ILE;
        case BINARY_GT:
            return OP_IGT;
        case BINARY_GE:
            return OP_IGE;
        case BINARY_ADD:
            return OP_IADD;
        case BINARY_SUB:
            return OP_ISUB;
        case BINARY_MUL:
            return OP_IMUL;
        case BINARY_DIV:
            return OP_IDIV;
        case BINARY_MOD:
            return OP_IMOD;
        case BINARY_BAND:
            return OP_BAND;
        case BINARY_BOR:
            return OP_BOR;
        case BINARY_BXOR:
            return OP_BXOR;
        case BINARY_SHL:
            return OP_SHL;
        case BINARY_SHR:
            return OP_SHR;
        case BINARY_AS:
            return OP_ICAST;
        case NBINARYOPS:
            PAW_UNREACHABLE();
    }
}

static Op unop2op_float(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_NEG:
            return OP_FNEG;
        default:
            PAW_UNREACHABLE();
    }
}

static Op binop2op_float(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return OP_FEQ;
        case BINARY_NE:
            return OP_FNE;
        case BINARY_LT:
            return OP_FLT;
        case BINARY_LE:
            return OP_FLE;
        case BINARY_GT:
            return OP_FGT;
        case BINARY_GE:
            return OP_FGE;
        case BINARY_ADD:
            return OP_FADD;
        case BINARY_SUB:
            return OP_FSUB;
        case BINARY_MUL:
            return OP_FMUL;
        case BINARY_DIV:
            return OP_FDIV;
        case BINARY_MOD:
            return OP_FMOD;
        case BINARY_AS:
            return OP_FCAST;
        default:
            PAW_UNREACHABLE();
    }
}

static Op unop2op_str(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return OP_SLENGTH;
        default:
            PAW_UNREACHABLE();
    }
}

static Op binop2op_str(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_EQ:
            return OP_SEQ;
        case BINARY_NE:
            return OP_SNE;
        case BINARY_LT:
            return OP_SLT;
        case BINARY_LE:
            return OP_SLE;
        case BINARY_GT:
            return OP_SGT;
        case BINARY_GE:
            return OP_SGE;
        case BINARY_ADD:
            return OP_SCONCAT;
        default:
            PAW_UNREACHABLE();
    }
}

static Op unop2op_list(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return OP_LLENGTH;
        default:
            PAW_UNREACHABLE();
    }
}

static Op binop2op_list(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_ADD:
            return OP_LCONCAT;
        default:
            PAW_UNREACHABLE();
    }
}

static Op unop2op_map(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return OP_MLENGTH;
        default:
            PAW_UNREACHABLE();
    }
}

static void code_unop(struct MirVisitor *V, struct MirUnaryOp *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const paw_Type code = TYPE_CODE(G, x->val->type);
    const Op op = code == BUILTIN_BOOL ? unop2op_bool(x->op) :
        code == BUILTIN_INT ? unop2op_int(x->op) :
        code == BUILTIN_FLOAT ? unop2op_float(x->op) :
        code == BUILTIN_STR ? unop2op_str(x->op) :
        code == BUILTIN_LIST ? unop2op_list(x->op) :
        unop2op_map(x->op);

    pawK_code_AB(G->fs, op, REG(fs, x->output), REG(fs, x->val));
}

static void code_binop(struct MirVisitor *V, struct MirBinaryOp *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const paw_Type code = TYPE_CODE(G, x->lhs->type);
    if (x->op == BINARY_ADD && (code == BUILTIN_STR || code == BUILTIN_LIST)) {
        // TODO: need an extra register to concatenate lists
        const Op op = code == BUILTIN_STR ? OP_SCONCAT : OP_LCONCAT;
        check_contiguous_pair(fs, x->lhs, x->rhs);
        pawK_code_AB(G->fs, op, REG(fs, x->output), 2);
        return;
    }
    const Op op = code == PAW_TINT ? binop2op_int(x->op) :
        code == PAW_TBOOL ? binop2op_bool(x->op) :
        code == PAW_TFLOAT ? binop2op_float(x->op) :
        code == PAW_TSTR ? binop2op_str(x->op) :
        binop2op_list(x->op);

    pawK_code_ABC(G->fs, op, REG(fs, x->output), REG(fs, x->lhs), REG(fs, x->rhs));
}

static void code_return(struct MirVisitor *V, struct MirReturn *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    if (x->value != NULL) {
        pawK_code_A(G->fs, OP_RETURN, REG(fs, x->value));
    } else {
        pawK_code_0(G->fs, OP_RETURN0);
    }
}

static void add_edge(struct MirVisitor *V, int from_pc, MirBlockId to)
{
    struct Generator *G = V->ud;
    for (int i = 0; i < G->jumps->count; ++i) {
        struct JumpTarget jump = K_LIST_GET(G->jumps, i);
        if (jump.bid.value == to.value) {
            patch_jump(G->fs, from_pc, jump.pc);
            return;
        }
    }
    // jump to a future instruction
    add_jump_source(G, from_pc, to);
}

// TODO: fallthrough instead of jumping when possible (x->target equal to next block ID in postorder traversal list, or something)!
static paw_Bool code_goto(struct MirVisitor *V, struct MirGoto *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    add_edge(V, emit_jump(fs), x->target);
    return PAW_FALSE;
}

static paw_Bool code_for_loop(struct MirVisitor *V, struct MirForLoop *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    if (x->for_kind == MIR_FOR_PREP) {
        const int else_jump = emit_cond_jump(fs, x->step, OP_FORPREP);
        const int then_jump = emit_jump(fs);
        add_edge(V, else_jump, x->else_arm);
        add_edge(V, then_jump, x->then_arm);
    } else {
        const int then_jump = emit_cond_jump(fs, x->step, OP_FORLOOP);
        const int else_jump = emit_jump(fs);
        add_edge(V, then_jump, x->then_arm);
        add_edge(V, else_jump, x->else_arm);
    }
    return PAW_FALSE;
}

static paw_Bool code_branch(struct MirVisitor *V, struct MirBranch *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    const int else_jump = emit_cond_jump(fs, x->cond, OP_JUMPF);
    add_edge(V, else_jump, x->else_arm);
    add_edge(V, emit_jump(fs), x->then_arm);
    return PAW_FALSE;
}

static paw_Bool is_enumerator(struct Generator *G, struct MirRegister *test)
{
    if (!IrIsAdt(test->type)) return PAW_FALSE;
    struct HirDecl *decl = pawHir_get_decl(G->C, IR_TYPE_DID(test->type));
    return !HirGetAdtDecl(decl)->is_struct;
}

static int code_testk(struct FuncState *fs, struct MirRegister *test, Value k, struct IrType *type)
{
    const int index = add_constant(fs->G, k, TYPE_CODE(fs->G, type));
    pawK_code_AB(fs, OP_TESTK, REG(fs, test), index);
    return emit_jump(fs);
}

static paw_Bool code_sparse_switch(struct MirVisitor *V, struct MirSwitch *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    struct MirRegister *d = x->discr;
    for (int i = 0; i < x->arms->count; ++i) {
        const struct MirSwitchArm arm = K_LIST_GET(x->arms, i);
        const int next_jump = code_testk(fs, d, arm.value, d->type);
        add_edge(V, next_jump, arm.bid);
    }
    paw_assert(x->has_otherwise);
    const int otherwise_jump = emit_jump(fs);
    add_edge(V, otherwise_jump, x->otherwise);
    return PAW_FALSE;
}

static paw_Bool code_switch(struct MirVisitor *V, struct MirSwitch *x)
{
    struct MirRegister *d = x->discr;
    if (x->has_otherwise) return code_sparse_switch(V, x);

    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    for (int i = 0; i < x->arms->count; ++i) {
        const struct MirSwitchArm arm = K_LIST_GET(x->arms, i);
        const int next_jump = code_switch_int(fs, d, CAST(int, arm.value.i));
        add_edge(V, next_jump, arm.bid);
    }
    paw_assert(!x->has_otherwise);
    return PAW_FALSE;
}

static void handle_jump_logic(struct Generator *G, struct MirBlock *block)
{
    // patch_jumps_to_here() must be called first, since it might alter 'fs->pc', throwing
    // off the jump label for this basic block
    patch_jumps_to_here(G, block->bid);
    add_jump_target(G, block->bid);
}

static paw_Bool code_block(struct MirVisitor *V, struct MirBlock *block)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    fs->bb = block;
    handle_jump_logic(G, block);
    pawMir_visit_instruction_list(V, block->code);
    pawMir_visit_terminator(V, block->term);
    return PAW_FALSE;
}

static void setup_codegen(struct Generator *G)
{
    struct MirVisitor *V = G->V;
    pawMir_visitor_init(V, G->C, G);

    V->PostVisitLocal = code_local;
    V->PostVisitUpvalue = code_upvalue;
    V->PostVisitGlobal = code_global;
    V->PostVisitConstant = code_constant;
    V->PostVisitSetUpvalue = code_set_upvalue;
    V->PostVisitAllocLocal = code_alloc_local;
    V->PostVisitFreeLocal = code_free_local;
    V->PostVisitEnterScope = code_enter_scope;
    V->PostVisitLeaveScope = code_leave_scope;
    V->PostVisitAggregate = code_aggregate;
    V->PostVisitExplode = code_explode;
    V->PostVisitContainer = code_container;
    V->PostVisitAssign = code_assign;
    V->PostVisitCall = code_call;
    V->PostVisitCast = code_cast;
    V->PostVisitClosure = code_closure;
    V->PostVisitGetElement = code_get_element;
    V->PostVisitSetElement = code_set_element;
    V->PostVisitGetRange = code_get_range;
    V->PostVisitSetRange = code_set_range;
    V->PostVisitGetField = code_get_field;
    V->PostVisitSetField = code_set_field;
    V->PostVisitUnaryOp = code_unop;
    V->PostVisitBinaryOp = code_binop;

    V->PostVisitReturn = code_return;
    V->VisitSwitch = code_switch;
    V->VisitForLoop = code_for_loop;
    V->VisitBranch = code_branch;
    V->VisitGoto = code_goto;
    V->VisitBlock = code_block;

    G->items = pawP_item_list_new(G->C);
}

void pawP_codegen(struct Compiler *C)
{
    paw_Env *P = ENV(C);

    struct MirVisitor V;
    struct Generator G = {
        .pool = C->pool,
        .V = &V,
        .P = P,
        .C = C,
    };

    pawL_push_builtin_map(P);
    G.builtin = V_MAP(P->top.p[-1]);

    setup_codegen(&G);
    register_items(&G);
    code_items(&G);

    pawP_pop_object(C, G.builtin);
}

