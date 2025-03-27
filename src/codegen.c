// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "api.h"
#include "auxlib.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "error.h"
#include "gc.h"
#include "hir.h"
#include "ir_type.h"
#include "lib.h"
#include "map.h"
#include "match.h"
#include "mem.h"
#include "mir.h"
#include "parse.h"
#include "rtti.h"


#define CODEGEN_ERROR(G_, Kind_, ...) pawErr_##Kind_((G_)->C, (G_)->fs->modname, __VA_ARGS__)

#define GET_TYPE(G, r) MirRegisterDataList_get((G)->fs->mir->registers, (r).value).type
#define TYPE_CODE(G, type) pawP_type2code((G)->C, type)
#define TYPEOF(G, r) MirRegisterDataList_get((G)->fs->mir->registers, (r).value).type
#define REG(r) RegisterTable_get(fs->regtab, (r).value).value

struct FuncState {
    struct FuncState *outer; // enclosing function
    struct RegisterTable *regtab;
    struct Generator *G; // codegen state
    struct PatchList *patch;
    struct JumpTable *jumps;
    struct KCache kcache;
    struct Mir *mir;
    MirBlock b;
    Proto *proto; // prototype being built
    String *modname; // name of the module
    int first_local; // index of function in DynamicMem array
    int nk; // number of constants
    int nproto; // number of nested functions
    int nlines; // number of source lines
    int pc; // number of instructions
    int line;
    int max_reg;
    enum FuncKind kind; // type of function
};

typedef struct Generator {
    struct Compiler *C;
    struct FuncState *fs;
    struct MirVisitor *V;
    struct ToplevelMap *toplevel;
    struct ToplevelMap *policy_cache;
    struct PolicyList *policies;
    struct ItemList *items;
    struct Pool *pool;
    paw_Env *P;
    int ipolicy;
} Generator;

struct JumpTarget {
    MirBlock bid;
    int pc;
};

struct PolicyInfo {
    int equals_vid;
    int hash_vid;
};

DEFINE_LIST(struct Generator, JumpTable, struct JumpTarget)
DEFINE_LIST(struct Generator, PolicyList, struct PolicyInfo)
DEFINE_MAP(struct Generator, ToplevelMap, pawP_alloc, IR_TYPE_HASH, IR_TYPE_EQUALS, struct IrType *, int)

static void add_jump_target(struct Generator *G, MirBlock bid)
{
    JumpTable_push(G, G->fs->jumps, ((struct JumpTarget){
                                        .pc = G->fs->pc,
                                        .bid = bid,
                                    }));
}

static void add_line(struct FuncState *fs)
{
    Proto *p = fs->proto;
    paw_Env *P = ENV(fs->G);
    pawM_grow(P, p->lines, fs->nlines, p->nlines);
    p->lines[fs->nlines++] = (struct LineInfo){
        .line = fs->line,
        .pc = fs->pc,
    };
}

static void add_opcode(struct FuncState *fs, OpCode code)
{
    paw_Env *P = ENV(fs->G);
    Proto *p = fs->proto;

    // While code is being generated, the 'pc' is used to track the number of
    // instructions, while the 'length' holds the capacity. The 'length' is set to the
    // final 'pc' before execution.
    pawM_grow(P, p->source, fs->pc, p->length);
    p->source[fs->pc] = code;
    ++fs->pc;
}

void code_0(struct FuncState *fs, Op op)
{
    add_line(fs);
    add_opcode(fs, op);
}

void code_ABx(struct FuncState *fs, Op op, int a, int bc)
{
    paw_assert(0 <= a && a <= A_MAX);
    paw_assert(0 <= bc && bc <= Bx_MAX);

    add_line(fs);
    add_opcode(fs, CREATE_ABx(op, a, bc));
}

void code_ABC(struct FuncState *fs, Op op, int a, int b, int c)
{
    paw_assert(0 <= a && a < A_MAX);
    paw_assert(0 <= b && b < B_MAX);
    paw_assert(0 <= c && c < C_MAX);

    add_line(fs);
    add_opcode(fs, CREATE_ABC(op, a, b, c));
}

static void code_AsBx(struct FuncState *fs, Op op, int a, int bc)
{
    code_ABx(fs, op, a, bc + sBx_MAX);
}

static void code_sBx(struct FuncState *fs, Op op, int bc)
{
    code_AsBx(fs, op, 0, bc);
}

static void code_AB(struct FuncState *fs, Op op, int a, int b)
{
    code_ABC(fs, op, a, b, 0);
}

static void code_A(struct FuncState *fs, Op op, int a)
{
    code_AB(fs, op, a, 0);
}

static struct Def *get_def(struct Generator *G, ItemId iid)
{
    return RTTI_DEF(ENV(G), iid);
}

static RttiType *lookup_type(struct Generator *G, struct IrType *type)
{
    RttiType **prtti = RttiMap_get(G->C, G->C->rtti, type);
    return prtti != NULL ? *prtti : NULL;
}

static ItemId type2def(struct Generator *G, struct IrType *type)
{
    RttiType *ty = lookup_type(G, type);
    paw_assert(ty != NULL && "undefined type");
    return IrIsAdt(type) ? ty->adt.iid : ty->fdef.iid;
}

struct JumpSource {
    MirBlock to;
    int from_pc;
};

DEFINE_LIST(struct Generator, PatchList, struct JumpSource)

static void add_jump_source(struct Generator *G, int from_pc, MirBlock to)
{
    PatchList_push(G, G->fs->patch, (struct JumpSource){
                                        .from_pc = from_pc,
                                        .to = to,
                                    });
}

static void patch_jump(struct FuncState *fs, int from, int to)
{
    Proto *p = fs->proto;
    int const dist = to - (from + 1);
    if (dist > JUMP_MAX)
        CODEGEN_ERROR(fs->G, too_far_to_jump, fs->mir->span.start, JUMP_MAX);

    paw_assert(0 <= from && from < p->length);
    SET_sBx(&p->source[from], dist);
}

static void patch_jumps_to_here(struct Generator *G, MirBlock bid)
{
    struct FuncState *fs = G->fs;

    int index;
    struct JumpSource *pjump;
    K_LIST_ENUMERATE (fs->patch, index, pjump) {
        if (pjump->to.value == bid.value) {
            patch_jump(fs, pjump->from_pc, fs->pc);
            PatchList_swap_remove(fs->patch, index);
            --index;
        }
    }
}

// Return the top of the register stack
// Used as a temporary register for instructions that cause memory allocations.
static int temporary_reg(struct FuncState *fs, int offset)
{
    int const temp = fs->proto->max_stack + offset + 1;
    if (temp >= NREGISTERS)
        CODEGEN_ERROR(fs->G, too_many_variables, fs->mir->span.start, NREGISTERS);
    fs->max_reg = PAW_MAX(fs->max_reg, temp);
    return temp;
}

static ValueId type2global(struct Generator *G, struct IrType *type)
{
    ItemId const iid = type2def(G, type);
    struct Def const *def = RTTI_DEF(ENV(G), iid);
    paw_assert(def->hdr.kind == DEF_FUNC);
    return def->func.vid;
}

static String *func_name(struct Generator *G, String const *modname, struct IrType *type, struct IrType *self)
{
    struct Compiler *C = G->C;
    struct IrSignature *fsig = IrGetSignature(type);
    struct IrFnDef const *fd = pawIr_get_fn_def(C, fsig->did);
    struct IrTypeList *fd_types = fd->is_extern ? NULL : fsig->types;
    if (fsig->self == NULL)
        return pawP_mangle_name(C, modname, fd->name, fd_types);
    struct IrAdtDef const *ad = pawIr_get_adt_def(C, IR_TYPE_DID(self));
    struct IrTypeList const *ad_types = fd->is_extern ? NULL : IR_TYPE_SUBTYPES(self);
    return pawP_mangle_attr(C, modname, ad->name, ad_types, fd->name, fd_types);
}

static String *adt_name(struct Generator *G, String const *modname, struct IrType *type)
{
    struct IrAdtDef *d = pawIr_get_adt_def(G->C, IR_TYPE_DID(type));
    return pawP_mangle_name(G->C, modname, d->name, ir_adt_types(type));
}

static ValueMap *kcache_map(struct FuncState *fs, enum BuiltinKind code)
{
    if (code == BUILTIN_INT) {
        return fs->kcache.ints;
    } else if (code == BUILTIN_FLOAT) {
        return fs->kcache.flts;
    } else {
        paw_assert(code == BUILTIN_STR);
        return fs->kcache.strs;
    }
}

static int add_constant(struct Generator *G, Value v, enum BuiltinKind code)
{
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;
    // 'bool' and 'int' have the same runtime representation
    if (code <= BUILTIN_BOOL)
        code = BUILTIN_INT;

    // share constant values within each function
    ValueMap *kmap = kcache_map(fs, code);
    Value const *pk = ValueMap_get(G->C, kmap, v);
    if (pk != NULL)
        return CAST(int, pk->i);

    if (fs->nk == CONSTANT_MAX)
        CODEGEN_ERROR(G, too_many_constants, fs->mir->span.start, CONSTANT_MAX);

    pawM_grow(ENV(G), p->k, fs->nk, p->nk);
    p->k[fs->nk] = v;

    ValueMap_insert(G->C, kmap, v, I2V(fs->nk));
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

#define JUMP_PLACEHOLDER (-1)

static int emit_cond_jump(struct FuncState *fs, int cond, Op op)
{
    code_AsBx(fs, op, cond, JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static int emit_jump(struct FuncState *fs)
{
    code_sBx(fs, OP_JUMP, JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static int code_switch_int(struct FuncState *fs, MirRegister discr, int k)
{
    // if discr != k, skip over the jump that moves control to the body
    // of the match case
    code_AB(fs, OP_SWITCHINT, REG(discr), k);
    return emit_jump(fs);
}

static void enter_kcache(struct Generator *G, struct KCache *cache)
{
    cache->ints = ValueMap_new_from(G->C, G->pool);
    cache->strs = ValueMap_new_from(G->C, G->pool);
    cache->flts = ValueMap_new_from(G->C, G->pool);
}

static void leave_kcache(struct Generator *G, struct KCache *cache)
{
    ValueMap_delete(G->C, cache->ints);
    ValueMap_delete(G->C, cache->strs);
    ValueMap_delete(G->C, cache->flts);
}

static void leave_function(struct Generator *G)
{
    paw_Env *P = ENV(G);
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;

    // module itself has no prototype
    if (fs->kind == FUNC_MODULE)
        return;
    p->max_stack = fs->max_reg;

    pawM_shrink(P, p->source, p->length, fs->pc);
    p->length = fs->pc;
    pawM_shrink(P, p->lines, p->nlines, fs->nlines);
    p->nlines = fs->nlines;
    pawM_shrink(P, p->k, p->nk, fs->nk);
    p->nk = fs->nk;

    leave_kcache(G, &fs->kcache);

    G->fs = fs->outer;
    CHECK_GC(P);
}

static void enter_function(struct Generator *G, struct FuncState *fs, struct Mir *mir, Proto *proto)
{
    G->V->mir = mir;
    *fs = (struct FuncState){
        .kind = mir->fn_kind,
        .modname = proto->modname,
        .proto = proto,
        .outer = G->fs,
        .mir = mir,
        .G = G,
    };
    fs->patch = PatchList_new(G);
    fs->jumps = JumpTable_new(G),
    G->fs = fs;

    enter_kcache(G, &fs->kcache);
}

static ValueId resolve_function(struct Generator *G, RttiType *rtti)
{
    paw_Env *P = ENV(G);
    paw_assert(rtti->hdr.kind == RTTI_TYPE_FN_DEF);
    struct Def *def = RTTI_DEF(P, rtti->fdef.iid);
    return def->func.vid;
}

static String const *module_prefix(struct Generator *G, int modno)
{
    struct ModuleInfo *m = ModuleList_get(G->C->modules, modno);
    return m->modno > 1 ? m->name : NULL;
}

static paw_Bool is_smi(paw_Int i)
{
    return (i < 0 && i >= -sBx_MAX) || (i >= 0 && i <= sBx_MAX);
}

static void code_smi(struct FuncState *fs, MirRegister r, paw_Int i)
{
    paw_assert(is_smi(i));
    code_AsBx(fs, OP_LOADSMI, REG(r), CAST(int, i));
}

// Wrap a Proto in a Closure so it can be called directly from C
static void set_entrypoint(struct Generator *G, Proto *proto, int g)
{
    paw_Env *P = ENV(G);
    Value *pval = RTTI_PVAL(P, g);
    Closure *closure = pawV_new_closure(P, 0);
    V_SET_OBJECT(pval, closure);
    closure->p = proto;
}

static void code_extern_function(struct Generator *G, String *name, int g)
{
    Value *pval = RTTI_PVAL(ENV(G), g);
    *pval = pawP_get_extern_value(G->C, name);
}

static void allocate_upvalue_info(struct Generator *G, Proto *proto, struct MirUpvalueList *upvalues)
{
    paw_Env *P = ENV(G);
    struct FuncState *fs = G->fs;
    proto->u = pawM_new_vec(P, upvalues->count, struct UpValueInfo);
    proto->nup = upvalues->count;

    int index;
    struct MirUpvalueInfo *pinfo;
    K_LIST_ENUMERATE (upvalues, index, pinfo) {
        proto->u[index].is_local = pinfo->is_local;
        if (pinfo->is_local) {
            struct FuncState *parent = fs->outer;
            MirRegister const r = MirRegisterList_get(parent->mir->locals, pinfo->index);
            struct RegisterInfo ri = RegisterTable_get(parent->regtab, r.value);
            proto->u[index].index = ri.value;
        } else {
            proto->u[index].index = pinfo->index;
        }
    }
}

static void code_proto(struct Generator *G, struct Mir *mir, Proto *proto, int index)
{
    struct FuncState *fs = G->fs;
    struct IrType *type = mir->type;
    struct IrFuncPtr *func = IR_FPTR(type);
    proto->argc = func->params->count;

    struct MirBlockList *rpo = pawMir_traverse_rpo(G->C, mir);
    struct MirLocationList *locations = pawMir_compute_locations(mir);
    struct MirIntervalList *intervals = pawMir_compute_liveness(G->C, mir, rpo, locations);
    fs->regtab = pawP_allocate_registers(G->C, mir, rpo, intervals, locations, &proto->max_stack);
    allocate_upvalue_info(G, proto, mir->upvalues);
    pawMir_visit_block_list(G->V, rpo);

    paw_assert(fs->patch->count == 0);
}

static Proto *code_paw_function(struct Generator *G, struct Mir *mir, int index);

static void code_children(struct Generator *G, Proto *parent, struct Mir *mir)
{
    int const nchildren = mir->children->count;
    parent->p = pawM_new_vec(ENV(G), nchildren, Proto *);
    parent->nproto = nchildren;

    int index;
    struct Mir **pchild;
    K_LIST_ENUMERATE (mir->children, index, pchild) {
        parent->p[index] = code_paw_function(G, *pchild, index);
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
    if (G->fs == NULL)
        set_entrypoint(G, proto, index);

    pop_proto(G);
    return proto;
}

static paw_Bool is_instance_call(struct IrType *type)
{
    return IrIsSignature(type) && IrGetSignature(type)->types != NULL;
}

// Generate code for an enumerator
static void code_variant_constructor(struct Generator *G, MirRegister discr, struct MirRegisterList *args, MirRegister output)
{
    struct FuncState *fs = G->fs;

    struct IrType *type = GET_TYPE(G, discr);
    struct IrVariantDef *d = pawIr_get_variant_def(G->C, IR_TYPE_DID(type));
    code_smi(fs, discr, d->discr); // discriminator is a small int

    int const nargs = args != NULL ? args->count : 0;

    // at runtime, a variant is represented by a tuple '(k, e1..en)', where 'k' is
    // the discriminator and 'e1..en' are the fields
    code_ABC(fs, OP_NEWTUPLE, REG(output), REG(discr), 1 + nargs);
}

static paw_Bool is_method_call(struct Generator *G, struct IrType *type)
{
    if (!IrIsSignature(type))
        return PAW_FALSE;
    return IrGetSignature(type)->self != NULL;
}

static void prep_method_call(struct Generator *G, MirRegister callable, MirRegister self)
{
    struct FuncState *fs = G->fs;
    struct IrType *type = GET_TYPE(G, callable);
    paw_assert(is_method_call(G, type));
    RttiType *rtti = lookup_type(G, type);
    ValueId const vid = resolve_function(G, rtti);
    code_ABx(fs, OP_GETGLOBAL, REG(callable), vid);
}

static void code_items(struct Generator *G)
{
    struct Compiler *C = G->C;

    int index;
    struct ItemSlot *pitem;
    K_LIST_ENUMERATE (G->items, index, pitem) {
        struct IrFnDef *def = pawIr_get_fn_def(C, pitem->did);
        const int vid = C->globals->count + index;
        if (def->is_extern) {
            code_extern_function(G, pitem->name, vid);
        } else {
            code_paw_function(G, pitem->mir, vid);
        }
    }
}

// Create policies for custom map keys
static void init_policies(struct Generator *G)
{
    paw_Env *P = ENV(G);
    struct MapPolicyList *policies = &P->map_policies;

    struct PolicyInfo *pinfo;
    K_LIST_FOREACH (G->policies, pinfo) {
        pawM_grow(P, policies->data, policies->count, policies->alloc);
        policies->data[policies->count++] = (MapPolicy){
            .equals = P->vals.data[pinfo->equals_vid],
            .hash = P->vals.data[pinfo->hash_vid],
        };
    }
}

static void register_toplevel_function(struct Generator *G, struct IrType *type, int iid)
{
    ToplevelMap_insert(G, G->toplevel, type, iid);
}

static void register_items(struct Generator *G)
{
    paw_Env *P = ENV(G);
    struct Compiler *C = G->C;
    struct ValList *vals = &P->vals;
    struct GlobalList *globals = C->globals;

    BodyMap *bodies = pawP_lower_hir(C);
    struct MonoResult mr = pawP_monomorphize(C, bodies);
    BodyMap_delete(C, bodies);

    G->items = pawP_allocate_defs(C, mr.bodies, mr.types);

    paw_assert(P->vals.data == NULL);
    int const nvalues = globals->count + G->items->count;
    P->vals.data = pawM_new_vec(P, nvalues, Value);
    P->vals.count = P->vals.alloc = nvalues;

    struct GlobalInfo *pinfo;
    K_LIST_FOREACH (globals, pinfo) {
        pawM_grow(P, vals->data, vals->count, vals->alloc);
        vals->data[pinfo->index] = pinfo->value;
        String const *modname = module_prefix(G, pinfo->modno);
        pinfo->name = pawP_mangle_name(C, modname, pinfo->name, NULL);
    }

    int iid;
    struct ItemSlot *pitem;
    K_LIST_ENUMERATE (G->items, iid, pitem) {
        struct Mir *mir = pitem->mir;
        struct IrType *type = mir->type;
        register_toplevel_function(G, type, iid);

        String const *modname = module_prefix(G, IR_TYPE_DID(type).modno);
        RttiType const *ty = lookup_type(G, type);
        struct FuncDef *fdef = &get_def(G, ty->fdef.iid)->func;
        paw_assert(fdef->kind == DEF_FUNC);
        pitem->name = fdef->mangled_name = func_name(G, modname, type, mir->self);
        pawMap_insert(P, V_TUPLE(P->functions), P2V(pitem->name), I2V(fdef->vid));
    }
}

static void move_to_reg(struct FuncState *fs, int from, int to)
{
    if (to != from)
        code_AB(fs, OP_MOVE, to, from);
}

static void code_move(struct MirVisitor *V, struct MirMove *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    move_to_reg(fs, REG(x->target), REG(x->output));
}

static void code_upvalue(struct MirVisitor *V, struct MirUpvalue *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    code_AB(fs, OP_GETUPVALUE, REG(x->output), x->index);
}

static void code_global(struct MirVisitor *V, struct MirGlobal *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    ValueId const value_id = x->global_id < 0
                                 ? type2global(G, TYPEOF(G, x->output))
                                 : (ValueId){x->global_id};
    code_ABx(fs, OP_GETGLOBAL, REG(x->output), value_id);
}

static void code_constant(struct MirVisitor *V, struct MirConstant *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    if (x->b_kind == BUILTIN_UNIT // (): 0
            || x->b_kind == BUILTIN_BOOL // bool: 0 | 1
            || (x->b_kind == BUILTIN_INT && is_smi(x->value.i))) {
        code_smi(fs, x->output, x->value.i);
    } else {
        int const bc = add_constant(G, x->value, x->b_kind);
        code_ABx(fs, OP_LOADK, REG(x->output), bc);
    }
}

static void code_set_upvalue(struct MirVisitor *V, struct MirSetUpvalue *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    code_AB(fs, OP_SETUPVALUE, x->index, REG(x->value));
}

static void code_aggregate(struct MirVisitor *V, struct MirAggregate *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    int const temp = temporary_reg(fs, 0);
    code_AB(fs, OP_NEWTUPLE, temp, x->nfields);
    move_to_reg(fs, temp, REG(x->output));
}

static void code_close(struct MirVisitor *V, struct MirClose *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    code_A(fs, OP_CLOSE, REG(x->target));
}

static void code_closure(struct MirVisitor *V, struct MirClosure *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    int const temp = temporary_reg(fs, 0);
    code_ABx(fs, OP_CLOSURE, temp, x->child_id);
    move_to_reg(fs, temp, REG(x->output));
}

static void code_get_element(struct MirVisitor *V, struct MirGetElement *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    Op const op = x->b_kind == BUILTIN_LIST ? OP_LGET : x->b_kind == BUILTIN_MAP ? OP_MGET
                                                                                 : OP_SGET;
    code_ABC(fs, op, REG(x->output), REG(x->object), REG(x->key));
}

static void code_set_element(struct MirVisitor *V, struct MirSetElement *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    Op const op = x->b_kind == BUILTIN_LIST ? OP_LSET : OP_MSET;
    code_ABC(fs, op, REG(x->object), REG(x->key), REG(x->value));
}

static void code_get_range(struct MirVisitor *V, struct MirGetRange *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    int const lower = temporary_reg(fs, 0);
    int const upper = temporary_reg(fs, 1);
    move_to_reg(fs, REG(x->lower), lower);
    move_to_reg(fs, REG(x->upper), upper);
    if (x->b_kind == BUILTIN_LIST) {
        // extra temporary register needed at runtime
        temporary_reg(fs, 2);
        code_ABC(fs, OP_LGETN, REG(x->output), REG(x->object), lower);
    } else {
        code_ABC(fs, OP_SGETN, REG(x->output), REG(x->object), lower);
    }
}

static void code_set_range(struct MirVisitor *V, struct MirSetRange *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    int const lower = temporary_reg(fs, 0);
    int const upper = temporary_reg(fs, 1);
    move_to_reg(fs, REG(x->lower), lower);
    move_to_reg(fs, REG(x->upper), upper);
    code_ABC(fs, OP_LSETN, REG(x->object), lower, REG(x->value));
}

static void code_get_field(struct MirVisitor *V, struct MirGetField *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    code_ABC(fs, OP_GETFIELD, REG(x->output), REG(x->object), x->index);
}

static void code_set_field(struct MirVisitor *V, struct MirSetField *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    code_ABC(fs, OP_SETFIELD, REG(x->object), x->index, REG(x->value));
}

// Determine the unique policy number for the given type of "key"
static unsigned determine_map_policy(struct Generator *G, struct IrType *key)
{
    enum BuiltinKind kind = TYPE_CODE(G, key);
    if (kind != NBUILTINS)
        return kind;
    int const *ppolicy = ToplevelMap_get(G, G->policy_cache, key);
    if (ppolicy != NULL)
        return *ppolicy;

    struct TraitOwnerList *const *powners = TraitOwners_get(G->C, G->C->trait_owners, key);
    struct IrType *equals = IrTypeList_first(TraitOwnerList_get(*powners, TRAIT_EQUALS));
    struct IrType *hash = IrTypeList_first(TraitOwnerList_get(*powners, TRAIT_HASH));
    PolicyList_push(G, G->policies, ((struct PolicyInfo){
                                        .equals_vid = *ToplevelMap_get(G, G->toplevel, equals),
                                        .hash_vid = *ToplevelMap_get(G, G->toplevel, hash),
                                    }));

    int const ipolicy = G->ipolicy++;
    ToplevelMap_insert(G, G->policy_cache, key, ipolicy);
    return ipolicy;
}

static void code_container(struct MirVisitor *V, struct MirContainer *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    int const temp = temporary_reg(fs, 0);
    if (x->b_kind == BUILTIN_LIST) {
        code_AB(fs, OP_NEWLIST, temp, x->nelems);
    } else {
        struct IrType *key = ir_map_key(TYPEOF(G, x->output));
        int const policy = determine_map_policy(G, key);
        code_ABC(fs, OP_NEWMAP, temp, x->nelems, policy);
    }
    move_to_reg(fs, temp, REG(x->output));
}

static int enforce_call_constraints(struct FuncState *fs, struct MirCall *x)
{
    int const target = temporary_reg(fs, 0);
    int next = target;

    MirRegister const *pr;
    move_to_reg(fs, REG(x->target), next++);
    K_LIST_FOREACH (x->args, pr) {
        move_to_reg(fs, REG(*pr), next++);
    }
    return target;
}

static void code_call(struct MirVisitor *V, struct MirCall *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    int const target = enforce_call_constraints(fs, x);
    code_AB(fs, OP_CALL, target, x->args->count);

    // move the return value from the top of the stack to where it is expected to
    // be by the rest of the code
    move_to_reg(fs, target, REG(x->output));
}

static void code_cast(struct MirVisitor *V, struct MirCast *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    Op op;
    if (x->to == BUILTIN_BOOL) {
        op = OP_XCASTB;
    } else if (x->from <= BUILTIN_INT && x->to == BUILTIN_FLOAT) {
        op = OP_ICASTF; // 'from' can be bool
    } else if (x->from == BUILTIN_FLOAT && x->to == BUILTIN_INT) {
        op = OP_FCASTI;
    } else {
        // TODO: remove casts that don't produce any instructions
        move_to_reg(fs, REG(x->target), REG(x->output));
        return; // NOOP
    }

    code_AB(fs, op, REG(x->output), REG(x->target));
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
        default:
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
    const enum BuiltinKind code = TYPE_CODE(G, TYPEOF(G, x->val));
    Op const op = code == BUILTIN_BOOL ? unop2op_bool(x->op) : //
        code == BUILTIN_INT ? unop2op_int(x->op) : //
        code == BUILTIN_FLOAT ? unop2op_float(x->op) : //
        code == BUILTIN_STR ? unop2op_str(x->op) : //
        code == BUILTIN_LIST ? unop2op_list(x->op) : //
        unop2op_map(x->op);
    code_AB(fs, op, REG(x->output), REG(x->val));
}

static void code_binop(struct MirVisitor *V, struct MirBinaryOp *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const enum BuiltinKind code = TYPE_CODE(G, TYPEOF(G, x->lhs));
    if (x->op == BINARY_ADD && (code == BUILTIN_STR || code == BUILTIN_LIST)) {
        Op const op = code == BUILTIN_STR ? OP_SCONCAT : OP_LCONCAT;
        int const first = temporary_reg(fs, 0);
        int const second = temporary_reg(fs, 1);
        if (code == BUILTIN_LIST)
            temporary_reg(fs, 2);
        move_to_reg(fs, REG(x->lhs), first);
        move_to_reg(fs, REG(x->rhs), second);
        code_AB(fs, op, first, 2);
        move_to_reg(fs, first, REG(x->output));
        return;
    }
    Op const op = code == BUILTIN_INT ? binop2op_int(x->op) : //
        code == BUILTIN_BOOL ? binop2op_bool(x->op) : //
        code == BUILTIN_FLOAT ? binop2op_float(x->op) : //
        code == BUILTIN_STR ? binop2op_str(x->op) : //
        binop2op_list(x->op);

    code_ABC(fs, op, REG(x->output), REG(x->lhs), REG(x->rhs));
}

static paw_Bool code_return(struct MirVisitor *V, struct MirReturn *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    move_to_reg(fs, REG(x->value), REG(MIR_RESULT_REG));
    code_0(fs, OP_RETURN);
    return PAW_FALSE;
}

static void add_edge(struct Generator *G, int from_pc, MirBlock to)
{
    struct JumpTarget *pjump;
    struct FuncState *fs = G->fs;
    K_LIST_FOREACH (fs->jumps, pjump) {
        if (MIR_BB_EQUALS(pjump->bid, to)) {
            patch_jump(fs, from_pc, pjump->pc);
            return;
        }
    }
    // jump to a future instruction
    add_jump_source(G, from_pc, to);
}

static void add_edge_from_here(struct Generator *G, MirBlock to)
{
    struct FuncState *fs = G->fs;
    // fallthrough if possible
    MirBlock const next = MIR_BB(fs->b.value + 1);
    if (!MIR_BB_EQUALS(next, to))
        add_edge(G, emit_jump(fs), to);
}

static paw_Bool code_goto(struct MirVisitor *V, struct MirGoto *x)
{
    add_edge_from_here(V->ud, x->target);
    return PAW_FALSE;
}

static paw_Bool code_branch(struct MirVisitor *V, struct MirBranch *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    int const else_jump = emit_cond_jump(fs, REG(x->cond), OP_JUMPF);
    add_edge(G, else_jump, x->else_arm);
    add_edge_from_here(G, x->then_arm);
    return PAW_FALSE;
}

static int code_testk(struct FuncState *fs, MirRegister test, Value k, struct IrType *type)
{
    const enum BuiltinKind code = pawP_type2code(fs->G->C, type);
    int const index = add_constant(fs->G, k, code);
    code_AB(fs, OP_TESTK, REG(test), index);
    return emit_jump(fs);
}

static paw_Bool code_sparse_switch(struct MirVisitor *V, struct MirSwitch *x)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    struct MirSwitchArm *parm;
    MirRegister const d = x->discr;
    K_LIST_FOREACH (x->arms, parm) {
        int const next_jump = code_testk(fs, d, parm->value, GET_TYPE(G, d));
        add_edge(G, next_jump, parm->bid);
    }
    if (MIR_BB_EXISTS(x->otherwise))
        add_edge_from_here(G, x->otherwise);
    return PAW_FALSE;
}

static paw_Bool code_switch(struct MirVisitor *V, struct MirSwitch *x)
{
    if (MIR_BB_EXISTS(x->otherwise)) {
        return code_sparse_switch(V, x);
    }

    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    struct MirSwitchArm *parm;
    MirRegister const d = x->discr;
    K_LIST_FOREACH (x->arms, parm) {
        int const next_jump = code_switch_int(fs, d, CAST(int, V_INT(parm->value)));
        add_edge(G, next_jump, parm->bid);
    }
    return PAW_FALSE;
}

static void handle_jump_logic(struct Generator *G, MirBlock b)
{
    // patch_jumps_to_here() must be called first, since it might alter 'fs->pc', throwing
    // off the jump label for this basic block
    patch_jumps_to_here(G, b);
    add_jump_target(G, b);
}

static paw_Bool code_block(struct MirVisitor *V, MirBlock b)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->b = b;

    struct MirBlockData *block = mir_bb_data(fs->mir, b);
    handle_jump_logic(G, b);
    pawMir_visit_instruction_list(V, block->instructions);
    return PAW_FALSE;
}

static void setup_codegen(struct Generator *G)
{
    struct MirVisitor *V = G->V;
    pawMir_visitor_init(V, G->C, NULL, G);

    V->PostVisitMove = code_move;
    V->PostVisitUpvalue = code_upvalue;
    V->PostVisitGlobal = code_global;
    V->PostVisitConstant = code_constant;
    V->PostVisitSetUpvalue = code_set_upvalue;
    V->PostVisitAggregate = code_aggregate;
    V->PostVisitContainer = code_container;
    V->PostVisitCall = code_call;
    V->PostVisitCast = code_cast;
    V->PostVisitClose = code_close;
    V->PostVisitClosure = code_closure;
    V->PostVisitGetElement = code_get_element;
    V->PostVisitSetElement = code_set_element;
    V->PostVisitGetRange = code_get_range;
    V->PostVisitSetRange = code_set_range;
    V->PostVisitGetField = code_get_field;
    V->PostVisitSetField = code_set_field;
    V->PostVisitUnaryOp = code_unop;
    V->PostVisitBinaryOp = code_binop;

    V->VisitReturn = code_return;
    V->VisitSwitch = code_switch;
    V->VisitBranch = code_branch;
    V->VisitGoto = code_goto;
    V->VisitBlock = code_block;
}

void pawP_codegen(struct Compiler *C)
{
    paw_Env *P = ENV(C);

    struct MirVisitor V;
    struct Generator G = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .ipolicy = P->map_policies.count,
        .items = ItemList_new(C),
        .V = &V,
        .P = P,
        .C = C,
    };
    G.policies = PolicyList_new(&G);
    G.policy_cache = ToplevelMap_new(&G);
    G.toplevel = ToplevelMap_new(&G);

    setup_codegen(&G);
    register_items(&G);
    code_items(&G);
    init_policies(&G);

    pawP_pool_free(C, G.pool);

    // report compilation statistics
    if (pawP_push_callback(C, "paw.stats_reporter")) {
        paw_push_rawptr(P, C->stats->data);
        paw_push_int(P, C->stats->count);
        paw_call(P, 2);
    }
}
