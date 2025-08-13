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
#include "layout.h"
#include "lib.h"
#include "map.h"
#include "match.h"
#include "mem.h"
#include "mir.h"
#include "parse.h"
#include "rtti.h"
#include "ssa.h"


// TODO: need module name, this always reports main module, but error may be elsewhere
#define CODEGEN_ERROR(G_, Kind_, ...) pawErr_##Kind_((G_)->C, (G_)->C->modname, __VA_ARGS__)

#define REG_TYPE(G, r) REG_DATA(G, r).type
#define IS_POINTER(G, r) REG_DATA(G, r).is_pointer
#define REG_DATA(G, r) MirRegisterDataList_get((G)->fs->mir->registers, (r).value)
#define TYPE_CODE(G, type) pawP_type2code((G)->C, type)
#define REG(Reg_) vm_register_for(fs, Reg_)

struct FnState {
    struct FnState *outer; // enclosing function
    struct RegisterTable *regtab;
    struct Generator *G; // codegen state
    struct PatchList *patch;
    struct JumpTable *jumps;
    struct Mir *mir;
    MirBlock b;
    Proto *proto; // prototype being built
    int first_local; // index of function in DynamicMem array
    int nproto; // number of nested functions
    int nlines; // number of source lines
    int pc; // number of instructions
    int line;
    int max_reg;
    enum FnKind kind; // type of function
};

typedef struct Generator {
    struct Compiler *C;
    struct FnState *fs;
    struct MirVisitor *V;
    struct ToplevelMap *toplevel;
    struct ToplevelMap *policy_cache;
    struct PolicyList *policies;
    struct ItemList *items;
    struct Pool *pool;
    Str *modname;
    paw_Env *P;
    int ipolicy;
} Generator;

static int vm_register_for(struct FnState *fs, struct MirPlace place)
{
    paw_assert(place.kind == MIR_PLACE_LOCAL);
    return RegisterTable_get(fs->regtab, place.r.value).value;
}

struct JumpTarget {
    MirBlock bid;
    int pc;
};

struct PolicyInfo {
    ValueId equals;
    ValueId hash;
    paw_Type type;
};

DEFINE_LIST(struct Generator, JumpTable, struct JumpTarget)
DEFINE_LIST(struct Generator, PolicyList, struct PolicyInfo)
DEFINE_MAP(struct Generator, ToplevelMap, pawP_alloc, IR_TYPE_HASH, IR_TYPE_EQUALS, IrType *, int)

static void add_jump_target(struct Generator *G, MirBlock bid)
{
    JumpTable_push(G, G->fs->jumps, (struct JumpTarget){
        .pc = G->fs->pc,
        .bid = bid,
    });
}

static void add_line(struct FnState *fs)
{
    Proto *p = fs->proto;
    paw_Env *P = ENV(fs->G);
    pawM_grow(P, p->lines, fs->nlines, p->nlines);
    p->lines[fs->nlines++] = (struct LineInfo){
        .line = fs->line,
        .pc = fs->pc,
    };
}

static void add_opcode(struct FnState *fs, OpCode code)
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

void code_ABx(struct FnState *fs, Op op, int a, int bc)
{
    paw_assert(0 <= a && a <= A_MAX);
    paw_assert(0 <= bc && bc <= Bx_MAX);

    add_line(fs);
    add_opcode(fs, CREATE_ABx(op, a, bc));
}

void code_ABC(struct FnState *fs, Op op, int a, int b, int c)
{
    paw_assert(0 <= a && a < A_MAX);
    paw_assert(0 <= b && b < B_MAX);
    paw_assert(0 <= c && c < C_MAX);

    add_line(fs);
    add_opcode(fs, CREATE_ABC(op, a, b, c));
}

static void code_AsBx(struct FnState *fs, Op op, int a, int bc)
{
    code_ABx(fs, op, a, bc + sBx_MAX);
}

static void code_sBx(struct FnState *fs, Op op, int bc)
{
    code_AsBx(fs, op, 0, bc);
}

static void code_AB(struct FnState *fs, Op op, int a, int b)
{
    code_ABC(fs, op, a, b, 0);
}

static void code_A(struct FnState *fs, Op op, int a)
{
    code_AB(fs, op, a, 0);
}

static struct Def *get_def(struct Generator *G, ItemId iid)
{
    return RTTI_DEF(ENV(G), iid);
}

static RttiType *get_rtti(struct Generator *G, IrType *type)
{
    RttiType **prtti = RttiMap_get(G->C, G->C->rtti, type);
    paw_assert(prtti != NULL && "undefined type");
    return *prtti;
}

static int size_on_stack(struct Generator *G, IrType *type)
{
    return rtti_stack_size(ENV(G), get_rtti(G, type));
}

static ItemId rtti_iid(RttiType const *rtti)
{
    return rtti->hdr.kind == RTTI_TYPE_ADT ? rtti->adt.iid : rtti->fdef.iid;
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

static void patch_jump(struct FnState *fs, int from, int to)
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
    struct FnState *fs = G->fs;

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
static int temporary_reg(struct FnState *fs, int offset)
{
    int const temp = fs->proto->max_stack + offset + 1;
    if (temp >= NREGISTERS)
        CODEGEN_ERROR(fs->G, too_many_variables, fs->mir->span.start, NREGISTERS);
    fs->max_reg = PAW_MAX(fs->max_reg, temp);
    return temp;
}

static ValueId type2global(struct Generator *G, IrType *type)
{
    RttiType const *rtti = get_rtti(G, type);
    struct Def const *def = RTTI_DEF(ENV(G), rtti_iid(rtti));
    paw_assert(def->hdr.kind == DEF_FUNC);
    return def->fn.vid;
}

static Str *fn_name(struct Generator *G, Str const *modname, IrType *type, IrType *self)
{
    struct Compiler *C = G->C;
    struct IrSignature *fsig = IrGetSignature(type);
    struct IrFnDef const *fd = pawIr_get_fn_def(C, fsig->did);
    IrTypeList *fd_types = fd->is_extern ? NULL : fsig->types;
    if (fsig->self == NULL)
        return pawP_mangle_name(C, modname, fd->name, fd_types);
    struct IrAdtDef const *ad = pawIr_get_adt_def(C, IR_TYPE_DID(self));
    IrTypeList const *ad_types = fd->is_extern ? NULL : IR_TYPE_SUBTYPES(self);
    return pawP_mangle_attr(C, modname, ad->name, ad_types, fd->name, fd_types);
}

static Str *adt_name(struct Generator *G, Str const *modname, IrType *type)
{
    struct IrAdtDef *d = pawIr_get_adt_def(G->C, IR_TYPE_DID(type));
    return pawP_mangle_name(G->C, modname, d->name, ir_adt_types(type));
}

static Proto *push_proto(struct Generator *G, Str *name, Str *modname)
{
    ENSURE_STACK(ENV(G), 1);
    Value *pv = pawC_push0(ENV(G));
    Proto *proto = pawV_new_proto(ENV(G));
    V_SET_OBJECT(pv, proto);
    proto->modname = modname;
    proto->name = name;
    return proto;
}

static void pop_proto(struct Generator *G)
{
    paw_assert(O_IS_PROTO(ENV(G)->top.p[-1].o));
    --ENV(G)->top.p;
}

#define JUMP_PLACEHOLDER (-1)

static int emit_cond_jump(struct FnState *fs, int cond, Op op)
{
    code_AsBx(fs, op, cond, JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static int emit_jump(struct FnState *fs)
{
    code_sBx(fs, OP_JUMP, JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static int code_switch_int(struct FnState *fs, struct MirPlace discr, int k)
{
    // if discr != k, skip over the jump that moves control to the body
    // of the match case
    code_AB(fs, OP_SWITCHINT, REG(discr), k);
    return emit_jump(fs);
}

static void leave_function(struct Generator *G)
{
    paw_Env *P = ENV(G);
    struct FnState *fs = G->fs;
    Proto *p = fs->proto;

    p->max_stack = fs->max_reg;

    pawM_shrink(P, p->source, p->length, fs->pc);
    p->length = fs->pc;
    pawM_shrink(P, p->lines, p->nlines, fs->nlines);
    p->nlines = fs->nlines;

    G->fs = fs->outer;
    CHECK_GC(P);
}

static void enter_function(struct Generator *G, struct FnState *fs, struct Mir *mir, Proto *proto)
{
    G->V->mir = mir;
    *fs = (struct FnState){
        .kind = mir->fn_kind,
        .proto = proto,
        .outer = G->fs,
        .mir = mir,
        .G = G,
    };
    fs->patch = PatchList_new(G);
    fs->jumps = JumpTable_new(G),
    G->fs = fs;
}

static ValueId resolve_function(struct Generator *G, RttiType *rtti)
{
    paw_Env *P = ENV(G);
    paw_assert(rtti->hdr.kind == RTTI_TYPE_FN_DEF);
    struct Def *def = RTTI_DEF(P, rtti->fdef.iid);
    return def->fn.vid;
}

static Str const *module_prefix(struct Generator *G, int modno)
{
    return modno != TARGET_MODNO
        ? ModuleNames_get(G->C->modnames, modno)
        : NULL;
}

static paw_Bool is_smi(paw_Int i)
{
    return (i < 0 && i >= -sBx_MAX) || (i >= 0 && i <= sBx_MAX);
}

static void code_smi(struct FnState *fs, struct MirPlace p, paw_Int i)
{
    paw_assert(is_smi(i));
    code_AsBx(fs, OP_LOADSMI, REG(p), CAST(int, i));
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

static void code_extern_function(struct Generator *G, Str *name, int g)
{
     Value const *pvalue = pawP_get_extern_value(G->C, name);
     if (pvalue == NULL)
         CODEGEN_ERROR(G, missing_extern_value, (struct SourceLoc){-1}, name->text);

    *RTTI_PVAL(ENV(G), g) = *pvalue;
}

static void allocate_upvalue_info(struct Generator *G, Proto *proto, struct MirUpvalueList *upvalues)
{
    paw_Env *P = ENV(G);
    struct FnState *fs = G->fs;
    proto->u = pawM_new_vec(P, upvalues->count, struct UpValueInfo);
    proto->nup = upvalues->count;

    int index;
    struct MirUpvalueInfo *pinfo;
    K_LIST_ENUMERATE (upvalues, index, pinfo) {
        proto->u[index].is_local = pinfo->is_local;
        if (pinfo->is_local) {
            struct FnState *parent = fs->outer;
            MirRegister const r = MirRegisterList_get(parent->mir->locals, pinfo->index);
            struct RegisterInfo ri = RegisterTable_get(parent->regtab, r.value);
            paw_assert(0 <= ri.value && ri.value < NREGISTERS);
            proto->u[index].index = ri.value;
        } else {
            proto->u[index].index = pinfo->index;
        }
    }
}

static void code_constants(struct Generator *G, struct Mir *mir, Proto *proto)
{
    proto->k = pawM_new_vec(ENV(G), mir->constants->count, Value);
    proto->nk = mir->constants->count;

    int index;
    struct MirConstantData const *pdata;
    K_LIST_ENUMERATE (mir->constants, index, pdata) {
        proto->k[index] = pdata->value;
    }
}

static void code_proto(struct Generator *G, struct Mir *mir, Proto *proto, int index)
{
    struct FnState *fs = G->fs;
    IrType *type = mir->type;
    proto->argc = mir->param_size;

    struct MirBlockList *rpo = pawMir_traverse_rpo(G->C, mir);
    struct MirLocationList *locations = pawMir_compute_locations(mir);
    struct MirIntervalMap *intervals = pawMir_compute_liveness(G->C, mir, rpo, locations);
    fs->regtab = pawP_allocate_registers(G->C, mir, rpo, intervals, locations, &proto->max_stack);
    allocate_upvalue_info(G, proto, mir->upvalues);
    pawMir_visit_block_list(G->V, rpo);

    paw_assert(fs->patch->count == 0);
}

static Proto *code_paw_function(struct Generator *G, struct Mir *mir, int index);

static void code_children(struct Generator *G, Proto *parent, struct Mir *mir)
{
    parent->p = pawM_new_vec(ENV(G), mir->children->count, Proto *);
    parent->nproto = mir->children->count;

    int index;
    struct Mir *const *pchild;
    K_LIST_ENUMERATE (mir->children, index, pchild) {
        parent->p[index] = code_paw_function(G, *pchild, index);
    }
}

static Proto *code_paw_function(struct Generator *G, struct Mir *mir, int index)
{
    struct FnState fs;
    Proto *proto = push_proto(G, mir->name, mir->modname);
    enter_function(G, &fs, mir, proto);

    code_constants(G, mir, proto);
    code_proto(G, mir, proto, index);
    code_children(G, proto, mir);

    leave_function(G);

    // anchor toplevel functions
    if (G->fs == NULL)
        set_entrypoint(G, proto, index);

    pop_proto(G);
    return proto;
}

static paw_Bool is_instance_call(IrType *type)
{
    return IrIsSignature(type) && IrGetSignature(type)->types != NULL;
}

static paw_Bool is_method_call(struct Generator *G, IrType *type)
{
    if (!IrIsSignature(type))
        return PAW_FALSE;
    return IrGetSignature(type)->self != NULL;
}

static void code_items(struct Generator *G)
{
    struct Compiler *C = G->C;

    int index;
    struct ItemSlot *pitem;
    K_LIST_ENUMERATE (G->items, index, pitem) {
        struct IrFnDef *def = pawIr_get_fn_def(C, pitem->did);
        ValueId const vid = C->globals->count + index;

        if (def->is_extern) {
            code_extern_function(G, pitem->name, vid);
        } else {
            code_paw_function(G, pitem->mir, vid);
        }
    }
}

// Create policies for maps
static void init_policies(struct Generator *G)
{
    paw_Env *P = ENV(G);
    struct MapPolicyList *policies = &P->map_policies;
    ENSURE_STACK(P, 2); // "hash" and "eq"

    struct PolicyInfo *pinfo;
    K_LIST_FOREACH (G->policies, pinfo) {
        *P->top.p++ = P->vals.data[pinfo->hash];
        *P->top.p++ = P->vals.data[pinfo->equals];
        paw_register_policy(P, pinfo->type);
    }
}

static void register_toplevel_function(struct Generator *G, IrType *type, int iid)
{
    ToplevelMap_insert(G, G->toplevel, type, iid);
}

static void finalize_bodies(struct Compiler *C, BodyList *bodies)
{
    struct Mir *const *pbody;
    K_LIST_FOREACH (bodies, pbody) {
        struct Mir *mir = *pbody;

        // only consider Paw functions
        if (mir->blocks->count > 0) {
            // perform unboxing before SSA construction so that phi nodes are generated for
            // newly-created variables
            pawP_scalarize_registers(C, mir);
            pawSsa_construct(mir);

            pawMir_propagate_constants(mir);
        }
    }
}

static void register_items(struct Generator *G)
{
    paw_Env *P = ENV(G);
    struct Compiler *C = G->C;
    struct ValList *vals = &P->vals;
    struct GlobalList *globals = C->globals;

    struct MonoResult mr = pawP_monomorphize(C, C->bodies);
    finalize_bodies(C, mr.bodies);
    BodyMap_delete(C, C->bodies);

    G->items = pawP_allocate_defs(C, mr.bodies, mr.types);

    paw_assert(P->vals.data == NULL);
    int const nvalues = globals->count + G->items->count;
    P->vals.data = pawM_new_vec(P, nvalues, Value);
    P->vals.count = P->vals.alloc = nvalues;

    struct GlobalInfo *pinfo;
    K_LIST_FOREACH (globals, pinfo) {
        pawM_grow(P, vals->data, vals->count, vals->alloc);
        vals->data[pinfo->index] = pinfo->value;
        Str const *modname = module_prefix(G, pinfo->modno);
        pinfo->name = pawP_mangle_name(C, modname, pinfo->name, NULL);
    }

    int iid;
    struct ItemSlot *pitem;
    K_LIST_ENUMERATE (G->items, iid, pitem) {
        struct Mir *mir = pitem->mir;
        IrType *type = mir->type;
        register_toplevel_function(G, type, iid);

        Str const *modname = module_prefix(G, IR_TYPE_DID(type).modno);
        RttiType const *rtti = get_rtti(G, type);
        struct FnDef *fdef = &get_def(G, rtti->fdef.iid)->fn;
        paw_assert(fdef->kind == DEF_FUNC);
        pitem->name = fdef->mangled_name = fn_name(G, modname, type, mir->self);
        pawMap_insert(P, V_TUPLE(P->functions), &P2V(pitem->name), &I2V(fdef->vid));
    }
}

static void move_to_reg(struct FnState *fs, int from, int to)
{
    if (to != from)
        code_AB(fs, OP_MOVE, to, from);
}

static int move_to_temp(struct FnState *fs, int from, int temp)
{
    int const to = temporary_reg(fs, temp);
    code_AB(fs, OP_MOVE, to, from);
    return to;
}

static void code_move(struct MirVisitor *V, struct MirMove *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    move_to_reg(fs, REG(x->target), REG(x->output));
}

static void code_write(struct MirVisitor *V, struct MirWrite *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    move_to_reg(fs, REG(x->value), REG(x->target));
}

static void code_upvalue(struct MirVisitor *V, struct MirUpvalue *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    code_AB(fs, OP_GETUPVALUE, REG(x->output), x->index);
}

static void code_global(struct MirVisitor *V, struct MirGlobal *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    ValueId const value_id = type2global(G, REG_TYPE(G, x->output.r));
    code_ABx(fs, OP_GETGLOBAL, REG(x->output), value_id);
}

static void code_constant(struct MirVisitor *V, struct MirLoadConstant *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    struct MirConstantData const *kdata = mir_const_data(fs->mir, x->k);
    if (kdata->kind == BUILTIN_UNIT || kdata->kind == BUILTIN_BOOL
            || (kdata->kind == BUILTIN_INT && is_smi(V_INT(kdata->value)))) {
        code_smi(fs, x->output, kdata->value.i);
    } else if (kdata->kind == BUILTIN_CHAR) {
        code_smi(fs, x->output, kdata->value.c);
    } else {
        code_ABx(fs, OP_LOADK, REG(x->output), x->k.value);
    }
}

static void code_set_capture(struct MirVisitor *V, struct MirSetCapture *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    move_to_reg(fs, REG(x->value), REG(x->target));
}

static void code_set_upvalue(struct MirVisitor *V, struct MirSetUpvalue *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    code_AB(fs, OP_SETUPVALUE, x->index, REG(x->value));
}

// Generate code to construct a boxed aggregate
static void code_aggregate(struct MirVisitor *V, struct MirAggregate *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    // number of values contained inside the box
    struct IrLayout const layout = pawMir_get_layout(fs->mir, x->output.r);

    code_AB(fs, OP_NEWTUPLE, REG(x->output), layout.size);
}

static void code_close(struct MirVisitor *V, struct MirClose *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    code_A(fs, OP_CLOSE, REG(x->target));
}

static void code_closure(struct MirVisitor *V, struct MirClosure *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    code_ABx(fs, OP_CLOSURE, REG(x->output), x->child_id);
}

static void code_map_get(struct FnState *fs, struct MirPlace output, struct MirPlace object, struct MirPlace key)
{
    int const b = move_to_temp(fs, REG(object), 0);
    int const c = move_to_temp(fs, REG(key), 1);
    code_ABC(fs, OP_MAPGET, REG(output), b, c);
}

static void code_get_element(struct MirVisitor *V, struct MirGetElement *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    if (x->b_kind == BUILTIN_STR) {
        code_ABC(fs, OP_STRGET, REG(x->output), REG(x->object), REG(x->key));
    } else if (x->b_kind == BUILTIN_MAP) {
        code_map_get(fs, x->output, x->object, x->key);
    } else {
        paw_assert(x->b_kind == BUILTIN_LIST) ;
        code_ABC(fs, OP_LISTGET, REG(x->output), REG(x->object), REG(x->key));
    }
}

static void code_map_getp(struct FnState *fs, struct MirPlace output, struct MirPlace object, struct MirPlace key, paw_Bool is_setter)
{
    int const b = move_to_temp(fs, REG(object), 0);
    int const c = move_to_temp(fs, REG(key), 1);
    Op const op = is_setter ? OP_MAPNEWP: OP_MAPGETP;
    code_ABC(fs, op, REG(output), b, c);
}

static void code_get_element_ptr(struct MirVisitor *V, struct MirGetElementPtr *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    if (x->b_kind == BUILTIN_MAP) {
        code_map_getp(fs, x->output, x->object, x->key, x->is_map_setter);
    } else {
        code_ABC(fs, OP_LISTGETP, REG(x->output), REG(x->object), REG(x->key));
    }
}

static void code_map_set(struct FnState *fs, struct MirPlace object, struct MirPlace key, struct MirPlace value)
{
    int const a = move_to_temp(fs, REG(object), 0);
    int const b = move_to_temp(fs, REG(key), 1);
    int const c = move_to_temp(fs, REG(value), 2);
    code_ABC(fs, OP_MAPSET, a, b, c);
}

static void code_set_element(struct MirVisitor *V, struct MirSetElement *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    if (x->b_kind == BUILTIN_MAP) {
        code_map_set(fs, x->object, x->key, x->value);
    } else {
        paw_assert(x->b_kind == BUILTIN_LIST);
        code_ABC(fs, OP_LISTSET, REG(x->object), REG(x->key), REG(x->value));
    }
}

static void code_get_range(struct MirVisitor *V, struct MirGetRange *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    int const lower = move_to_temp(fs, REG(x->lower), 0);
    int const upper = move_to_temp(fs, REG(x->upper), 1);
    if (x->b_kind == BUILTIN_LIST) {
        // extra temporary register needed at runtime
        temporary_reg(fs, 2);
        code_ABC(fs, OP_LISTGETN, REG(x->output), REG(x->object), lower);
    } else {
        paw_assert(x->b_kind == BUILTIN_STR);
        code_ABC(fs, OP_STRGETN, REG(x->output), REG(x->object), lower);
    }
}

static void code_set_range(struct MirVisitor *V, struct MirSetRange *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    int const lower = move_to_temp(fs, REG(x->lower), 0);
    int const upper = move_to_temp(fs, REG(x->upper), 1);
    code_ABC(fs, OP_LISTSETN, REG(x->object), lower, REG(x->value));
}

static void code_unpack(struct MirVisitor *V, struct MirUnpack *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    code_ABC(fs, OP_UNPACK, REG(x->object), x->offset, x->outputs->count);
}

static void code_get_field(struct MirVisitor *V, struct MirGetField *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    Op const op = IS_POINTER(G, x->object.r) ? OP_GETVALUE : OP_GETFIELD;
    code_ABC(fs, op, REG(x->output), REG(x->object), x->index);
}

static void code_set_field(struct MirVisitor *V, struct MirSetField *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    Op const op = IS_POINTER(G, x->object.r) ? OP_SETVALUE : OP_SETFIELD;
    code_ABC(fs, op, REG(x->object), x->index, REG(x->value));
}

// Determine the unique policy number for the given type of map
static unsigned determine_map_policy(struct Generator *G, IrType *type)
{
    // policy type depends on both key and value
    int const *ppolicy = ToplevelMap_get(G, G->policy_cache, type);
    if (ppolicy != NULL) return *ppolicy;

    struct TraitOwnerList *const *powners = TraitOwners_get(G->C, G->C->trait_owners, ir_map_key(type));
    RttiType const *equals = get_rtti(G, IrTypeList_first(TraitOwnerList_get(*powners, TRAIT_EQUALS)));
    RttiType const *hash = get_rtti(G, IrTypeList_first(TraitOwnerList_get(*powners, TRAIT_HASH)));
    PolicyList_push(G, G->policies, (struct PolicyInfo){
        .type = get_rtti(G, type)->adt.code,
        .equals = RTTI_DEF(ENV(G), rtti_iid(equals))->fn.vid,
        .hash = RTTI_DEF(ENV(G), rtti_iid(hash))->fn.vid,
    });

    int const ipolicy = G->ipolicy++;
    ToplevelMap_insert(G, G->policy_cache, type, ipolicy);
    return ipolicy;
}

static IrType *get_element_type(IrType *type)
{
    return IrGetAdt(type)->types->count == 1
        ? ir_list_elem(type)
        : ir_map_value(type);
}

static void code_container(struct MirVisitor *V, struct MirContainer *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    IrType *element_type = get_element_type(REG_TYPE(G, x->output.r));
    int const temp = temporary_reg(fs, 0);
    if (x->b_kind == BUILTIN_LIST) {
        code_ABC(fs, OP_NEWLIST, temp, x->nelems, size_on_stack(G, element_type));
    } else {
        int const policy = determine_map_policy(G, REG_TYPE(G, x->output.r));
        code_ABC(fs, OP_NEWMAP, temp, x->nelems, policy);
    }
    move_to_reg(fs, temp, REG(x->output));
}

static void code_call(struct MirVisitor *V, struct MirCall *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    code_AB(fs, OP_CALL, REG(x->target), x->args->count);
}

static void code_cast(struct MirVisitor *V, struct MirCast *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    static Op const CAST_OPERATORS[NBUILTIN_SCALARS][NBUILTIN_SCALARS] = {
        //          to  = {0,         b,         c,         i,         f}
        [BUILTIN_BOOL]  = {0,         0,         0,         0, OP_BCASTF},
        [BUILTIN_CHAR]  = {0, OP_ICASTB,         0, OP_CCASTI,         0},
        [BUILTIN_INT]   = {0, OP_ICASTB, OP_ICASTC,         0, OP_ICASTF},
        [BUILTIN_FLOAT] = {0, OP_FCASTB,         0, OP_FCASTI,         0},
    };

    Op const op = CAST_OPERATORS[x->from][x->to];
    code_AB(fs, op, REG(x->output), REG(x->target));
}

static Op unop2op(enum MirUnaryOpKind unop)
{
    switch (unop) {
        case MIR_UNARY_STRLEN:
            return OP_STRLEN;
        case MIR_UNARY_LISTLEN:
            return OP_LISTLEN;
        case MIR_UNARY_MAPLEN:
            return OP_MAPLEN;
        case MIR_UNARY_NOT:
            return OP_INOT;
        case MIR_UNARY_FNEG:
            return OP_FNEG;
        case MIR_UNARY_INEG:
            return OP_INEG;
        case MIR_UNARY_IBITNOT:
            return OP_BITNOT;
    }
}

static Op binop2op(enum MirBinaryOpKind binop)
{
    switch (binop) {
        case MIR_BINARY_STRCAT:
            return OP_STRCAT;
        case MIR_BINARY_LISTCAT:
            return OP_LISTCAT;
        case MIR_BINARY_CEQ:
            return OP_CEQ;
        case MIR_BINARY_CNE:
            return OP_CNE;
        case MIR_BINARY_CLT:
            return OP_CLT;
        case MIR_BINARY_CLE:
            return OP_CLE;
        case MIR_BINARY_IEQ:
            return OP_IEQ;
        case MIR_BINARY_INE:
            return OP_INE;
        case MIR_BINARY_ILT:
            return OP_ILT;
        case MIR_BINARY_ILE:
            return OP_ILE;
        case MIR_BINARY_IADD:
            return OP_IADD;
        case MIR_BINARY_ISUB:
            return OP_ISUB;
        case MIR_BINARY_IMUL:
            return OP_IMUL;
        case MIR_BINARY_IDIV:
            return OP_IDIV;
        case MIR_BINARY_IMOD:
            return OP_IMOD;
        case MIR_BINARY_FEQ:
            return OP_FEQ;
        case MIR_BINARY_FNE:
            return OP_FNE;
        case MIR_BINARY_FLT:
            return OP_FLT;
        case MIR_BINARY_FLE:
            return OP_FLE;
        case MIR_BINARY_FADD:
            return OP_FADD;
        case MIR_BINARY_FSUB:
            return OP_FSUB;
        case MIR_BINARY_FMUL:
            return OP_FMUL;
        case MIR_BINARY_FDIV:
            return OP_FDIV;
        case MIR_BINARY_FMOD:
            return OP_FMOD;
        case MIR_BINARY_STREQ:
            return OP_STREQ;
        case MIR_BINARY_STRNE:
            return OP_STRNE;
        case MIR_BINARY_STRLT:
            return OP_STRLT;
        case MIR_BINARY_STRLE:
            return OP_STRLE;
        case MIR_BINARY_IBITAND:
            return OP_BITAND;
        case MIR_BINARY_IBITOR:
            return OP_BITOR;
        case MIR_BINARY_IBITXOR:
            return OP_BITXOR;
        case MIR_BINARY_ISHL:
            return OP_SHL;
        case MIR_BINARY_ISHR:
            return OP_SHR;
    }
}

static void code_unop(struct MirVisitor *V, struct MirUnaryOp *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    code_AB(fs, unop2op(x->op), REG(x->output), REG(x->val));
}

static void code_binop(struct MirVisitor *V, struct MirBinaryOp *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    const enum BuiltinKind code = TYPE_CODE(G, REG_TYPE(G, x->lhs.r));
    paw_assert(code != NBUILTINS);

    Op const op = binop2op(x->op);
    if (op == OP_STRCAT || op == OP_LISTCAT) {
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

    code_ABC(fs, op, REG(x->output), REG(x->lhs), REG(x->rhs));
}

static paw_Uint int_hash(struct Generator *G, int i)
{
    PAW_UNUSED(G);
    return (paw_Uint)i;
}

static paw_Bool int_equals(struct Generator *G, int a, int b)
{
    PAW_UNUSED(G);
    return a == b;
}

DEFINE_MAP(struct Generator, ReturnMap, pawP_alloc, int_hash, int_equals, int, int)
DEFINE_MAP_ITERATOR(ReturnMap, int, int)

static paw_Bool code_return(struct MirVisitor *V, struct MirReturn *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    struct MirPlace const base = K_LIST_FIRST(x->values);
    code_AB(fs, OP_RETURN, REG(base), x->values->count);
    return PAW_FALSE;
}

static void add_edge(struct Generator *G, int from_pc, MirBlock to)
{
    struct JumpTarget *pjump;
    struct FnState *fs = G->fs;
    K_LIST_FOREACH (fs->jumps, pjump) {
        if (MIR_ID_EQUALS(pjump->bid, to)) {
            patch_jump(fs, from_pc, pjump->pc);
            return;
        }
    }
    // jump to a future instruction
    add_jump_source(G, from_pc, to);
}

static void add_edge_from_here(struct Generator *G, MirBlock to)
{
    struct FnState *fs = G->fs;
    // fallthrough if possible
    MirBlock const next = MIR_BB(fs->b.value + 1);
    if (!MIR_ID_EQUALS(next, to))
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
    struct FnState *fs = G->fs;

    int const else_jump = emit_cond_jump(fs, REG(x->cond), OP_JUMPF);
    add_edge(G, else_jump, x->else_arm);
    add_edge_from_here(G, x->then_arm);
    return PAW_FALSE;
}

static int code_testk(struct FnState *fs, struct MirPlace test, MirConstant k, IrType *type)
{
    const enum BuiltinKind code = pawP_type2code(fs->G->C, type);
    code_ABx(fs, OP_TESTK, REG(test), k.value);
    return emit_jump(fs);
}

static paw_Bool code_sparse_switch(struct MirVisitor *V, struct MirSwitch *x)
{
    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    struct MirSwitchArm *parm;
    struct MirPlace const d = x->discr;
    K_LIST_FOREACH (x->arms, parm) {
        int const next_jump = code_testk(fs, d, parm->k, REG_TYPE(G, d.r));
        add_edge(G, next_jump, parm->bid);
    }
    if (MIR_ID_EXISTS(x->otherwise))
        add_edge_from_here(G, x->otherwise);
    return PAW_FALSE;
}

static paw_Bool code_switch(struct MirVisitor *V, struct MirSwitch *x)
{
    if (MIR_ID_EXISTS(x->otherwise)) {
        return code_sparse_switch(V, x);
    }

    struct Generator *G = V->ud;
    struct FnState *fs = G->fs;

    struct MirSwitchArm *parm;
    K_LIST_FOREACH (x->arms, parm) {
        struct MirConstantData const *kdata = mir_const_data(fs->mir, parm->k);
        int const next_jump = code_switch_int(fs, x->discr, CAST(int, V_INT(kdata->value)));
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
    struct FnState *fs = G->fs;
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
    V->PostVisitWrite = code_write;
    V->PostVisitUpvalue = code_upvalue;
    V->PostVisitGlobal = code_global;
    V->PostVisitLoadConstant = code_constant;
    V->PostVisitSetCapture = code_set_capture;
    V->PostVisitSetUpvalue = code_set_upvalue;
    V->PostVisitAggregate = code_aggregate;
    V->PostVisitContainer = code_container;
    V->PostVisitCall = code_call;
    V->PostVisitCast = code_cast;
    V->PostVisitClose = code_close;
    V->PostVisitClosure = code_closure;
    V->PostVisitGetElement = code_get_element;
    V->PostVisitSetElement = code_set_element;
    V->PostVisitGetElementPtr = code_get_element_ptr;
    V->PostVisitGetRange = code_get_range;
    V->PostVisitSetRange = code_set_range;
    V->PostVisitUnpack = code_unpack;
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

void pawP_generate_code(struct Compiler *C)
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
