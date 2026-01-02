// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: need to use addrof to get address of local instead of using it directly so using a local as "self" before init. can be detected

#include "analysis.h"
#include "ir_type.h"
#include "mir.h"

struct Variable {
    IrType *type;
    struct VariableList *subvars;
    int id;
};

struct BlockSet {
    BitSet *da;
};

struct VariableAnalyzer {
    struct Compiler *C;
    struct Pool *pool;
    struct Mir *mir;

    struct BlockSet *current_block;
    struct VarCache *varcache;

    struct BlockSets *blocks;
    struct WorkPool *work;

    // corresponds to MIR locals
    struct VariableList *locals;

    // counter to help generate variable IDs
    int num_vars;
};

DEFINE_MAP(struct VariableAnalyzer, VarCache, pawP_alloc, mir_place_hash, mir_place_equals, struct MirPlace, struct Variable *)
DEFINE_MAP(struct VariableAnalyzer, WorkPool, pawP_alloc, P_ID_HASH, P_ID_EQUALS, MirBlock, void *)
DEFINE_MAP_ITERATOR(WorkPool, MirBlock, void *)
DEFINE_LIST(struct VariableAnalyzer, VariableList, struct Variable *)
DEFINE_LIST(struct VariableAnalyzer, BlockSets, struct BlockSet)

static IrTypeList *get_subtypes(struct Mir *mir, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrAdt: {
            struct IrAdt *adt = IrGetAdt(type);
            struct IrAdtDef const *def = pawIr_get_adt_def(mir->C, adt->did);
            if (def->is_inline && def->is_struct)
                return pawP_instantiate_struct_fields(mir->C, adt);
            return NULL;
        }
        case kIrTuple: {
            return IrGetTuple(type)->elems;
        }
        default:
            return NULL;
    }
}

static struct Variable *new_variable(struct VariableAnalyzer *V, IrType *type)
{
    struct Mir *mir = V->mir;
    VariableList *subvars = VariableList_new(V);
    IrTypeList *subtypes = get_subtypes(mir, type);
    if (subtypes != NULL) {
        IrType *const *ptype;
        K_LIST_FOREACH (subtypes, ptype) {
            struct Variable *subvar = new_variable(V, *ptype);
            VariableList_push(V, subvars, subvar);
        }
    }
    struct Variable *var = pawP_alloc(V->mir->pool, NULL, 0, sizeof *var);
    *var = (struct Variable){
        .id = V->num_vars++,
        .subvars = subvars,
        .type = type,
    };
    return var;
}

static MirBlock next_work_item(struct VariableAnalyzer *V)
{
    WorkPoolIterator iter;
    WorkPoolIterator_init(V->work, &iter);
    return WorkPoolIterator_key(&iter);
}

static void add_work_item(struct VariableAnalyzer *V, MirBlock w)
{
    WorkPool_insert(V, V->work, w, NULL);
}

static void remove_work_item(struct VariableAnalyzer *V, MirBlock w)
{
    WorkPool_remove(V, V->work, w);
}

static struct Variable *get_local(struct VariableAnalyzer *V, MirLocal L)
{
    return K_LIST_AT(V->locals, L.value);
}

static struct Variable **find_variable(struct VariableAnalyzer *V, struct MirPlace p)
{
    if (p.kind == MIR_PLACE_LOCAL) {
        return &K_LIST_AT(V->locals, p.L.value);
    } else {
        return VarCache_get(V, V->varcache, p);
    }
}

static struct BlockSet *get_block(struct VariableAnalyzer *V, MirBlock b)
{
    return &K_LIST_AT(V->blocks, b.value);
}

static BitSet *new_set(struct VariableAnalyzer *V, int count)
{
    return pawP_bitset_new(V->C, count);
}

static int set_count(BitSet const *set)
{
    return pawP_bitset_count(set);
}

static BitSet *copy_set(struct VariableAnalyzer *V, BitSet const *set)
{
    return pawP_bitset_copy(V->C, set);
}

static void clear_set(BitSet *set)
{
    pawP_bitset_clear_range(set, 0, set_count(set));
}

static paw_Bool set_contains(BitSet const *set, int id)
{
    return pawP_bitset_get(set, id);
}

static void set_insert(BitSet *bs, int id)
{
    pawP_bitset_set(bs, id);
}

static BitSet *intersect_sets(struct VariableAnalyzer *V, BitSet const *x, BitSet const *y)
{
    return pawP_bitset_and(V->mir->C, x, y);
}

#include <stdio.h>

void visualize_block(struct BlockSet const *bs)
{
    if (bs->da != NULL) {
        for (int i = 0; i < set_count(bs->da); ++i) {
            int const b = set_contains(bs->da, i);
            printf("%s", b ? "* " : ". ");
        }
    } else {
        printf("(null)");
    }
    printf("\n");
}

void visualize_blocks(struct VariableAnalyzer const *V)
{
    for (int i = 0; i < V->locals->count; ++i) {
        struct Variable const *var = V->locals->data[i];
        struct MirLocalData const *data = mir_local_data(V->mir, MIR_LOCAL(i));
        char const *type = pawIr_print_type(V->mir->C, data->type);
        printf("Variable #%d = L%d (%s)\n", var->id, i, type);
    }
    for (int i = 0; i < V->blocks->count; ++i) {
        printf("%%bb%d  ", i);
        visualize_block(&V->blocks->data[i]);
    }
}

static void indicate_variable_use(struct VariableAnalyzer *V, struct Variable const *v)
{
    if (v->subvars->count == 0 && !set_contains(V->current_block->da, v->id)) {
        // TODO: better error message including name of local
        struct Mir *mir = V->mir;
        Str const *modname = ModuleInfo_get(mir->C->modinfo, mir->modno).name;
        pawErr_generic_error(ENV(V->mir), modname, (struct SourceLoc){0}, "use before initialization");
    }

    struct Variable *const *subvar;
    K_LIST_FOREACH (v->subvars, subvar)
        indicate_variable_use(V, *subvar);
}

static void indicate_variable_def(struct VariableAnalyzer *V, struct Variable *var)
{
    struct Variable *const *subvar;
    K_LIST_FOREACH (var->subvars, subvar)
        indicate_variable_def(V, *subvar);

    if (var->subvars->count == 0)
        set_insert(V->current_block->da, var->id);
}

static void maybe_indicate_use(struct VariableAnalyzer *V, struct MirPlace p)
{
    struct Variable *const *pvar = find_variable(V, p);
    if (pvar != NULL) indicate_variable_use(V, *pvar);
}

static void maybe_indicate_def(struct VariableAnalyzer *V, struct MirPlace p)
{
    struct Variable *const *pvar = find_variable(V, p);
    if (pvar != NULL) indicate_variable_def(V, *pvar);
}

static void bind_addr_to_var(struct VariableAnalyzer *V, struct MirPlace addr, struct Variable *var)
{
    VarCache_insert(V, V->varcache, addr, var);
}

static paw_Bool is_inline_enum_type(struct VariableAnalyzer *V, IrType *type)
{
    if (IrIsAdt(type)) {
        struct IrAdtDef const *def = pawIr_get_adt_def(V->mir->C, IR_TYPE_DID(type));
        return !def->is_struct && def->is_inline;
    }
    return PAW_FALSE;
}

static paw_Bool is_boxed_adt(struct VariableAnalyzer *V, IrType *type)
{
    if (IrIsAdt(type)) {
        struct IrAdtDef const *def = pawIr_get_adt_def(V->mir->C, IR_TYPE_DID(type));
        return !def->is_inline;
    }
    return PAW_FALSE;
}

static void visit_block(struct VariableAnalyzer *V, MirBlock b)
{
    struct BlockSet *bs = &K_LIST_AT(V->blocks, b.value);
    struct MirBlockData *bb = mir_bb_data(V->mir, b);
    V->current_block = bs;

    // set of variables that were definitely assigned the last time this
    // block was considered
    BitSet const *last_da = bs->da;

    if (b.value == 0) {
        clear_set(bs->da);
        // write to function arguments in entry block
        int const num_args = IR_FPTR(V->mir->type)->params->count;
        for (int i = 0; i < num_args; ++i) {
            struct Variable *var = VariableList_get(V->locals, 1 + i);
            indicate_variable_def(V, var);
        }
    } else {
        // "before" set starts as the intersection of "after" sets from
        // predecessors. All basic blocks except for the entry block have
        // at least 1 predecessor.
        MirBlock const *pp;
        BitSet *result = NULL;
        K_LIST_FOREACH (bb->predecessors, pp) {
            // only consider predecessors that have already been visited
            // so definite assignments can propagate across loop headers
            BitSet *after = get_block(V, *pp)->da;
            result = result == NULL ? after
                : intersect_sets(V, result, after);
        }
        bs->da = copy_set(V, result);
    }

    // must run before SSA conversion
    paw_assert(bb->joins->count == 0);

    struct MirInstruction *const *pinstr;
    K_LIST_FOREACH (bb->instructions, pinstr) {
        struct MirInstruction *instr = *pinstr;
        switch (MIR_KINDOF(instr)) {
            case kMirAddrOf: {
                struct MirAddrOf *x = MirGetAddrOf(instr);
                struct Variable *const *pvar = find_variable(V, x->input);
                if (pvar != NULL) bind_addr_to_var(V, x->output, *pvar);
                break;
            }

            case kMirLoad:
                maybe_indicate_use(V, MirGetLoad(instr)->pointer);
                break;
            case kMirStore:
                maybe_indicate_def(V, MirGetStore(instr)->pointer);
                break;

            case kMirStructGEP: {
                struct MirStructGEP *x = MirGetStructGEP(instr);
                if (IrIsPtr(x->object.type)) {
                    struct IrType *pointee = IrGetPtr(x->object.type)->pointee;
                    if (is_inline_enum_type(V, pointee)) {
                        if (x->field == 0) {
                            struct Variable *const *pvar = find_variable(V, x->object);
                            if (pvar != NULL) bind_addr_to_var(V, x->output, *pvar);
                        }
                        break;
                    }
                }
                if (is_boxed_adt(V, x->object.type)) {
                    struct Variable *const *pvar = find_variable(V, x->object);
                    if (pvar != NULL && !set_contains(bs->da, (*pvar)->id)) {
                        struct Mir *mir = V->mir;
                        Str const *modname = ModuleInfo_get(mir->C->modinfo, mir->modno).name;
                        pawErr_generic_error(ENV(V->mir), modname, (struct SourceLoc){0}, "boxed ADT requires initializer");
                    }
                    break;
                }
                struct Variable *const *pvar = find_variable(V, x->object);
                if (pvar != NULL) {
                    struct Variable *subvar = VariableList_get((*pvar)->subvars, x->field);
                    bind_addr_to_var(V, x->output, subvar);
                }
                break;
            }

            case kMirCall: {
                struct MirCall *x = MirGetCall(instr);
                maybe_indicate_use(V, x->target);

                struct MirPlace const *pplace;
                K_LIST_FOREACH (x->args, pplace)
                    maybe_indicate_use(V, *pplace);
                break;
            }

            default:
                break;
        }
    }

    paw_assert(bs->da->count <= last_da->count);
    if (bs->da->count < last_da->count) {
        MirBlock const *pb;
        K_LIST_FOREACH (bb->successors, pb)
            add_work_item(V, *pb);

    }
}

static void initialize_data_structures(struct VariableAnalyzer *V)
{
    VariableList_resize(V, V->locals, V->mir->locals->count);

    int index;
    struct MirLocalData const *pdata;
    K_LIST_ENUMERATE (V->mir->local_data, index, pdata) {
        struct Variable *local = new_variable(V, pdata->type);
        K_LIST_AT(V->locals, index) = local;
    }

    BlockSets_resize(V, V->blocks, V->mir->blocks->count);

    struct MirBlockData *const *pbb;
    K_LIST_ENUMERATE (V->mir->blocks, index, pbb) {
        struct BlockSet *bs = get_block(V, MIR_BB(index));
        bs->da = new_set(V, V->num_vars);
        pawP_bitset_set_range(bs->da, 0, V->num_vars);
        add_work_item(V, MIR_BB(index));
    }
}

static void ensure_variable_initialization_before_use(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    struct VariableAnalyzer *V = &(struct VariableAnalyzer){
        .pool = pawP_pool_new(C, C->aux_stats),
        .mir = mir,
        .C = C,
    };
    V->blocks = BlockSets_new(V);
    V->locals = VariableList_new(V);
    V->varcache = VarCache_new(V);
    V->work = WorkPool_new(V);

    initialize_data_structures(V);
    while (V->work->count > 0) {
        MirBlock const w = next_work_item(V);
        visit_block(V, w);
        remove_work_item(V, w);
    }
}

void pawA_validate(struct Mir *mir)
{
    ensure_variable_initialization_before_use(mir);
}


//Mir {
//  bb0 {
//    alloc L0 ("&int")
//    alloc L1 ("&int")
//    alloc L2 ("&bool")
//    goto bb1
//  }
//  bb1 {
//    alloc L3 ("&int")
//    _0 = *L1
//    switch _0 => [1: bb2, 3: bb4, _: bb8]
//  }
//  bb2 {
//    goto bb3
//  }
//  bb3 {
//    *L3 = 10
//    goto bb15
//  }
//  bb4 {
//    _1 = *L1
//    alloc L4 ("&int")
//    *L4 = _1
//    _2 = *L2
//    branch _2 => [0: bb5, 1: bb6]
//  }
//  bb5 {
//    goto bb10
//  }
//  bb6 {
//    goto bb7
//  }
//  bb7 {
//    *L3 = 30
//    goto bb11
//  }
//  bb8 {
//    _4 = *L1
//    alloc L5 ("&int")
//    *L5 = _4
//    _5 = *L2
//    branch _5 => [0: bb9, 1: bb12]
//  }
//  bb9 {
//    goto bb10
//  }
//  bb10 {
//    _3 = *L4
//    *L3 = _3
//    goto bb11
//  }
//  bb11 {
//    goto bb15
//  }
//  bb12 {
//    goto bb13
//  }
//  bb13 {
//    _6 = *L1
//    alloc L6 ("&int")
//    *L6 = _6
//    _7 = *L6
//    _8 = INEG _7
//    *L3 = _8
//    goto bb14
//  }
//  bb14 {
//    goto bb15
//  }
//  bb15 {
//    _9 = *L3
//    *L0 = _9
//    goto bb16
//  }
//  bb16 {
//    return
//  }
//  registers {
//    _0: int
//    _1: int
//    _2: bool
//    _3: int
//    _4: int
//    _5: bool
//    _6: int
//    _7: int
//    _8: int
//    _9: int
//  }
//  locals {
//    0: _0: &int
//    1: _1: &int
//    2: _2: &bool
//    3: _3: &int
//    4: _4: &int
//    5: _5: &int
//    6: _6: &int
//  }
//  upvalues {
//  }
//  captured {
//  }
//}
//
