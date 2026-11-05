// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// analysis.c: Implements definite assignment analysis over MIR, preventing
//   scoped/"automatic" variables from being accessed outside their lifetimes.
//   The lifetime of such a variable starts at the point of initialization and
//   lasts until the variable goes out of scope or is moved (a destructive
//   move).
//
//   Note that the type checker has already prevented moves from pointed-to
//   objects, as well as moves from subobjects. Only complete objects stored
//   in local variables need to be considered.

#include "analysis.h"

#include "ir/type.h"
#include "mir/mir.h"
#include "sema/solve.h"
#include "sema/unify.h"

#define ANALYSIS_ERROR(V_, Kind_, ...) THROW_ERROR((V_)->C, Kind_, \
        .modname = ModuleInfo_get((V_)->C->modinfo, (V_)->mir->modno).name, \
        __VA_ARGS__)

struct Variable {
    IrType *type;
    struct SourceSpan span;
    struct Variable *parent;
    struct VariableList *subvars;
    Str const *name;
    int id;
};

enum VariableState {
    VAR_INIT,
    VAR_MOVED,
    VAR_UNINIT,
};

struct VariableAnalyzer {
    struct Allocator a;
    struct Compiler *C;
    struct Pool *pool;
    struct Mir *mir;

    struct ConditionalMoves *cmoves;

    struct VariableStates *current;
    struct VarCache *varcache;

    struct BlockStates *blocks;
    struct Worklist *work;

    // corresponds to MIR registers
    struct VariableList *locals;

    // counter to help generate variable IDs
    int num_vars;
};

static paw_Uint place_hash(struct MirPlace place)
{
    return (place.kind + 1) * (paw_Uint)place.value;
}

static paw_Bool place_equals(struct MirPlace lhs, struct MirPlace rhs)
{
    return lhs.kind == rhs.kind
        && lhs.value == rhs.value;
}

#define IDH(i) ((paw_Uint64)(i).value)

// TODO: I think VarCache could be keyed on MirRegister instead of MirPlace
DEFINE_MAP(BlockSet, P_ID_HASH, P_ID_EQUALS, MirBlock, void *)
DEFINE_MAP(ConditionalMoves, P_ID_HASH, P_ID_EQUALS, MirRegister, struct MirPlace)
DEFINE_MAP(VarCache, place_hash, place_equals, struct MirPlace, struct Variable *)
DEFINE_LIST(Worklist, MirBlock)
DEFINE_LIST(VariableStates, unsigned char)
DEFINE_LIST(VariableList, struct Variable *)
DEFINE_LIST(BlockStates, VariableStates *)

static IrTypeList *get_subtypes(struct Mir *mir, IrType *type)
{
    switch (IR_KINDOF(type)) {
        case kIrAdt: {
            struct IrAdt *adt = IrGetAdt(type);
            struct IrAdtDef const *def = ir_get_adt_def(mir->C, adt->did);
            if (def->is_struct)
                return instantiate_struct_fields(mir->C, adt);
            return NULL;
        }
        case kIrTuple: {
            return IrGetTuple(type)->elems;
        }
        default:
            return NULL;
    }
}

static struct Variable *new_variable(struct VariableAnalyzer *V, Str const *name, struct SourceSpan span, IrType *type, struct Variable *parent)
{
    struct Variable *var = pool_alloc(V->mir->pool, NULL, 0, sizeof *var);
    *var = (struct Variable){
        .id = V->num_vars++,
        .parent = parent,
        .type = type,
        .name = name,
        .span = span,
    };
    return var;
}

static void add_work_item(struct VariableAnalyzer *V, MirBlock w)
{
    Worklist_push(V->work, w);
}

static struct Variable *get_local(struct VariableAnalyzer *V, MirRegister r)
{
    return LIST_AT(V->locals, r.value);
}

static struct Variable **find_variable(struct VariableAnalyzer *V, struct MirPlace p)
{
    if (p.kind == MIR_PLACE_REGISTER) {
        return &LIST_AT(V->locals, p.r.value);
    } else {
        return VarCache_get(V->varcache, p);
    }
}

static struct VariableStates *get_block(struct VariableAnalyzer *V, MirBlock b)
{
    return LIST_AT(V->blocks, b.value);
}

static void clear_states(VariableStates *states, enum VariableState value)
{
    memset(states->data, (int)value, (size_t)states->count * sizeof(states->data[0]));
}

static VariableStates *new_states(struct VariableAnalyzer *V, int count)
{
    VariableStates *states = VariableStates_new(ALLOCATOR_OF(V));
    VariableStates_resize(states, count);
    clear_states(states, VAR_INIT);
    return states;
}

static void copy_states(VariableStates const *from, VariableStates *to)
{
    paw_assert(VariableStates_len(from) == VariableStates_len(to));
    memcpy(to->data, from->data, (size_t)from->count * sizeof(from->data[0]));
}

static VariableStates *clone_states(struct VariableAnalyzer *V, VariableStates const *states)
{
    VariableStates *clone = VariableStates_new(ALLOCATOR_OF(V));
    VariableStates_resize(clone, VariableStates_len(states));
    copy_states(states, clone);
    return clone;
}

static enum VariableState states_get(VariableStates const *states, int id)
{
    return VariableStates_get(states, id);
}

static void states_set(VariableStates *bs, int id, enum VariableState value)
{
    VariableStates_set(bs, id, (unsigned char)value);
}

static void meet_states(struct VariableAnalyzer *V, VariableStates const *x, VariableStates *y)
{
    paw_assert(VariableStates_len(x) == VariableStates_len(y));

    unsigned char const *a;
    unsigned char *b;
    LIST_ZIP (x, a, y, b) {
        *b = *a == VAR_UNINIT || *b == VAR_UNINIT ? VAR_UNINIT :
            *a == VAR_MOVED || *b == VAR_MOVED ? VAR_MOVED :
            VAR_INIT;
    }
}

#if defined(PAW_DEBUG_EXTRA)
#include <stdio.h>

void visualize_block(struct VariableStates const *bs)
{
    if (bs != NULL) {
        for (int i = 0; i < VariableStates_len(bs); ++i) {
            enum VariableState const state = states_get(bs, i);
            printf("%s", state == VAR_INIT ? "* " : ". ");
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
        struct MirRegisterData const *data = mir_reg_data(V->mir, MIR_REG(i));
        char const *type = ir_print_type(V->mir->C, data->type);
        printf("Variable #%d = L%d (%s)\n", var->id, i, type);
    }
    for (int i = 0; i < V->blocks->count; ++i) {
        printf("%%bb%d  ", i);
        visualize_block(V->blocks->data[i]);
    }
}

#endif

static void indicate_variable_use(struct VariableAnalyzer *V, struct Variable const *v, struct SourceSpan span)
{
    if (states_get(V->current, v->id) != VAR_INIT) {
        enum VariableState const state = states_get(V->current, v->id);
        if (state == VAR_UNINIT) {
            ANALYSIS_ERROR(V, UseBeforeInitialization,
                    .name = v->name,
                    .local_span = v->span,
                    .use_span = span);
        } else {
            ANALYSIS_ERROR(V, UseAfterMove,
                    .name = v->name,
                    .local_span = v->span,
                    .use_span = span);
        }
    }
}

static void indicate_variable_def(struct VariableAnalyzer *V, struct Variable const *var)
{
    states_set(V->current, var->id, VAR_INIT);
}

static void indicate_variable_move(struct VariableAnalyzer *V, struct Variable const *var)
{
    if (!ir_solver_is_copyable(V->C, var->type))
        states_set(V->current, var->id, VAR_MOVED);
}

static void maybe_indicate_use(struct VariableAnalyzer *V, struct MirPlace p)
{
    struct Variable *const *pvar = find_variable(V, p);
    if (pvar != NULL) indicate_variable_use(V, *pvar, p.span);
}

static void maybe_indicate_def(struct VariableAnalyzer *V, struct MirPlace p)
{
    struct Variable *const *pvar = find_variable(V, p);
    if (pvar != NULL) indicate_variable_def(V, *pvar);
}

static void maybe_indicate_move(struct VariableAnalyzer *V, struct MirPlace p)
{
    maybe_indicate_use(V, p);

    IrType *pointee = ir_auto_deref(mir_place_type(V->mir, p));
    if (!ir_solver_is_copyable(V->C, pointee)) {
        struct Variable *const *pvar = find_variable(V, p);
        if (pvar != NULL) indicate_variable_move(V, *pvar);
    }
}

static void bind_addr_to_var(struct VariableAnalyzer *V, struct MirPlace addr, struct Variable *var)
{
    VarCache_insert(V->varcache, addr, var);
}

static paw_Bool is_enum_type(struct VariableAnalyzer *V, IrType *type)
{
    if (IrIsAdt(type)) {
        struct IrAdtDef const *def = ir_get_adt_def(V->mir->C, IR_TYPE_DID(type));
        return !def->is_struct;
    }
    return PAW_FALSE;
}

static void visit_block(struct VariableAnalyzer *V, MirBlock b)
{
    VariableStates *const *bs = &LIST_AT(V->blocks, b.value);
    struct MirBlockData *bb = mir_bb_data(V->mir, b);
    V->current = *bs;

    VariableStates const *prev_states = clone_states(V, V->current);

    if (b.value == 0) {
        clear_states(V->current, VAR_UNINIT);
        // write to function arguments in entry block
        int const num_args = ir_fn_params(V->C, V->mir->type)->count;
        for (int i = 0; i < num_args; ++i) {
            struct Variable const *var = VariableList_get(V->locals, 1 + i);
            indicate_variable_def(V, var);
        }
    } else {
        // compute meet of predecessor block states
        int index = 0;
        LIST_XFOREACH (bb->predecessors, MirBlock const, pp) {
            VariableStates const *states = get_block(V, *pp);
            if (index++ == 0) {
                copy_states(states, V->current);
            } else {
                meet_states(V, states, V->current);
            }
        }
    }

    LIST_XFOREACH (bb->instructions, struct MirInstruction *, pinstr) {
        switch (MIR_KINDOF(*pinstr)) {
            case kMirAddrOf: {
                struct MirAddrOf *x = MirGetAddrOf(*pinstr);
                maybe_indicate_use(V, x->input);
                maybe_indicate_def(V, x->output);
                break;
            }

            case kMirLoad: {
                struct MirLoad const *x = MirGetLoad(*pinstr);
                maybe_indicate_use(V, x->pointer);
                maybe_indicate_def(V, x->output);
                break;
            }

            case kMirStore: {
                struct MirStore const *x = MirGetStore(*pinstr);
                maybe_indicate_move(V, x->value);
                maybe_indicate_def(V, x->pointer);
                break;
            }

            case kMirCapture: {
                struct MirCapture *x = MirGetCapture(*pinstr);
                maybe_indicate_move(V, x->target);
                break;
            }

            case kMirKill:
                maybe_indicate_move(V, MirGetKill(*pinstr)->target);
                break;

            case kMirDrop: {
                struct MirDrop const *x = MirGetDrop(*pinstr);
                struct Variable *const *pvar = find_variable(V, x->target);
                if (pvar != NULL) {
                    enum VariableState const state = states_get(V->current, (*pvar)->id);
                    if (state != VAR_INIT)
                        *pinstr = mir_new_noop(V->mir, x->span);
                }
                break;
            }

            case kMirStructGEP: {
                struct MirStructGEP const *x = MirGetStructGEP(*pinstr);
                maybe_indicate_use(V, x->object);
                maybe_indicate_def(V, x->output);
                break;
            }

            case kMirArrayGEP: {
                struct MirArrayGEP const *x = MirGetArrayGEP(*pinstr);
                maybe_indicate_use(V, x->array);
                maybe_indicate_use(V, x->index);
                maybe_indicate_def(V, x->output);
                break;
            }

            case kMirGetDiscriminant: {
                struct MirGetDiscriminant const *x = MirGetGetDiscriminant(*pinstr);
                maybe_indicate_use(V, x->object);
                maybe_indicate_def(V, x->output);
                break;
            }

            default: {
                struct MirPlacePtrList const *loads = mir_get_loads(V->mir, *pinstr);
                struct MirPlacePtrList const *stores = mir_get_stores(V->mir, *pinstr);
                LIST_XFOREACH (loads, struct MirPlace *const, p) maybe_indicate_move(V, **p);
                LIST_XFOREACH (stores, struct MirPlace *const, p) maybe_indicate_def(V, **p);
                break;
            }
        }
    }

    unsigned char const *prev;
    unsigned char const *current;
    paw_Bool changed = PAW_FALSE;
    LIST_ZIP (prev_states, prev, V->current, current) {
        if (*prev != *current) {
            changed = PAW_TRUE;
            break;
        }
    }

    if (changed) {
        MirBlock const *pb;
        LIST_FOREACH (bb->successors, pb)
            add_work_item(V, *pb);

    }
}

static struct MirPlace get_drop_flag(struct VariableAnalyzer *V, int id)
{
    return *ConditionalMoves_get(V->cmoves, MIR_REG(id));
}

static paw_Bool is_known_cmove(struct VariableAnalyzer *V, int id)
{
    return ConditionalMoves_get(V->cmoves, MIR_REG(id)) != NULL;
}

static struct MirPlace add_drop_flag(struct Mir *mir, MirRegister r)
{
    int const num_registers = mir->registers->count;
    IrType *bool_type = pawP_builtin_type(mir->C, BUILTIN_BOOL);
    Str const *name = pawP_format_string(mir->C, "(%%drop_flag_%d)", r.value);
    MirRegisterDataList_push(mir->registers, (struct MirRegisterData){
                .is_captured = PAW_FALSE,
                .type = bool_type,
                .name = name,
            });

    return mir_get_register(mir, MIR_REG(num_registers));
}

static void determine_cmoves_aux(struct VariableAnalyzer *V, VariableStates const *x, VariableStates const *y)
{
    unsigned char const *a;
    unsigned char const *b;
    MirRegister local = MIR_RESULT_REG;
    LIST_ZIP (x, a, y, b) {
        if ((*a == VAR_MOVED && *b == VAR_INIT)
                || (*a == VAR_INIT && *b == VAR_MOVED)) {
            if (!is_known_cmove(V, local.value)) {
                struct MirPlace const flag = add_drop_flag(V->mir, local);
                ConditionalMoves_insert(V->cmoves, local, flag);
            }
        }
        ++local.value;
    }
}

static struct MirInstruction *drop_flag_setter(struct VariableAnalyzer *V, int id, paw_Bool value)
{
    struct MirPlace const kbool = {
        .k = V->mir->kcache->boolk[value],
        .kind = MIR_PLACE_CONSTANT,
    };
    return mir_new_move(V->mir, (struct SourceSpan){0},
            get_drop_flag(V, id), kbool);
}

static struct MirPlace new_register(struct Mir *mir, IrType *type)
{
    MirRegisterDataList_push(mir->registers,
            (struct MirRegisterData){
                .type = type,
            });
    return (struct MirPlace){
        .r.value = mir->registers->count - 1,
        .kind = MIR_PLACE_REGISTER,
    };
}

static MirBlock new_basic_block(struct VariableAnalyzer *V)
{
    VariableStates *states = new_states(V, V->num_vars);
    BlockStates_push(V->blocks, states);

    struct MirBlockData *after_data = mir_new_block(V->mir);
    MirBlockDataList_push(V->mir->blocks, after_data);
    return MIR_BB(V->mir->blocks->count - 1);
}

static void push_instruction(struct Mir *mir, struct MirBlockData const *data, struct MirInstruction *instr)
{
    MirInstructionList_push(data->instructions, instr);
}

static void terminate_goto(struct Mir *mir, struct MirBlockData const *data, MirBlock target)
{
    data->successors->count = 0;
    MirBlockList_push(data->successors, target);

    push_instruction(mir, data,
            mir_new_goto(mir, (struct SourceSpan){0}));
}

static void terminate_branch(struct Mir *mir, struct MirBlockData const *data, struct MirPlace condition, MirBlock if_true, MirBlock if_false)
{
    data->successors->count = 0;
    MirBlockList_push(data->successors, if_true);
    MirBlockList_push(data->successors, if_false);

    push_instruction(mir, data,
            mir_new_branch(mir, (struct SourceSpan){0}, condition));
}

static struct MirPlace push_move(struct Mir *mir, struct MirBlockData const *data, struct MirPlace pointer)
{
    IrType *pointer_type = mir_place_type(mir, pointer);
    struct MirPlace const value = new_register(mir, ir_deref(pointer_type));
    push_instruction(mir, data,
            mir_new_move(mir, pointer.span, pointer, value));
    return value;
}

static void determine_cmoves(struct VariableAnalyzer *V)
{
    struct Mir *mir = V->mir;
    LIST_XFOREACH (mir->blocks, struct MirBlockData *const, pbb) {
        struct MirBlockData const *bb = *pbb;

        VariableStates const *current = NULL;
        LIST_XFOREACH (bb->predecessors, MirBlock const, pp) {
            VariableStates const *next = get_block(V, *pp);
            if (current != NULL) {
                determine_cmoves_aux(V, current, next);
            } else {
                current = next;
            }
        }
    }

#define SET_DROP_FLAG(Bb_, Id_, Value_, Index_) do { \
            struct MirInstruction *setter = drop_flag_setter(V, Id_, Value_); \
            MirInstructionList_insert((Bb_)->instructions, Index_, setter); \
        } while (0)

    struct IrFnPtr const *fptr = IrGetFnPtr(IR_GET_FN(V->C, mir->type));
    for (int i = 0; i < fptr->params->count; ++i) {
        struct MirPlace const arg = mir_get_register(mir, MIR_REG(1 + i));
        struct Variable *const *pvar = find_variable(V, arg);
        if (pvar != NULL && is_known_cmove(V, (*pvar)->id)) {
            // Drop flags default to `true` for function arguments. Add setter to the
            // end of the entry block.
            struct MirBlockData *entry = MirBlockDataList_first(mir->blocks);
            paw_assert(entry->instructions->count > 0); // requires terminator
            SET_DROP_FLAG(entry, (*pvar)->id, PAW_TRUE, entry->instructions->count - 1);
        }
    }

    for (int b = 1; b < mir->blocks->count; ++b) {
        struct MirBlockData *bb = MirBlockDataList_get(mir->blocks, b);

        for (int i = 0; i < bb->instructions->count; ++i) {
            // TODO: AddrOf doesn't really "load" from its input register, leading to the awkward situation below
            struct MirInstruction *instr = MirInstructionList_get(bb->instructions, i);
            if (!MirIsKill(instr) && !MirIsDrop(instr)) {
                if (!MirIsAddrOf(instr)) {
                    struct MirPlacePtrList const *loads = mir_get_loads(mir, instr);
                    LIST_XFOREACH (loads, struct MirPlace *const, p) {
                        struct Variable *const *pvar = find_variable(V, **p);
                        if (pvar != NULL && is_known_cmove(V, (*pvar)->id))
                            SET_DROP_FLAG(bb, (*pvar)->id, PAW_FALSE, i++);
                    }
                }

                struct MirPlacePtrList const *stores = mir_get_stores(mir, instr);
                LIST_XFOREACH (stores, struct MirPlace *const, p) {
                    struct Variable *const *pvar = find_variable(V, **p);
                    if (pvar != NULL && is_known_cmove(V, (*pvar)->id))
                        SET_DROP_FLAG(bb, (*pvar)->id, PAW_TRUE, i++);
                }
            }
        }
    }

#undef SET_DROP_FLAG

    // BEFORE
    //
    //     %before:
    //       drop %local
    //       ...
    //
    // AFTER
    //
    //     %before:
    //       br %flag, %drop, %join
    //
    //     %drop:
    //       drop %local
    //       goto %join
    //
    //     %join:
    //       ...
    //
    BlockSet *omit_blocks = BlockSet_new(ALLOCATOR_OF(V));
    for (int i = 1; i < mir->blocks->count; ++i) {
        MirBlock const before = MIR_BB(i);
        if (BlockSet_get(omit_blocks, before) != NULL)
            continue; // block contains generated drop
        struct MirBlockData const *before_data = mir_bb_data(mir, before);
        for (int j = 0; j < before_data->instructions->count; ++j) {
            struct MirInstruction *drop_instr = MirInstructionList_get(before_data->instructions, j);
            if (MirIsDrop(drop_instr)) {
                struct Variable *const *pvar = find_variable(V, MirGetDrop(drop_instr)->target);
                if (pvar != NULL && is_known_cmove(V, (*pvar)->id)) {
                    // Add new basic blocks at the end of the list so they will be visited in
                    // a future iteration.
                    MirBlock const join = new_basic_block(V);
                    MirBlock const drop = new_basic_block(V);
                    struct MirBlockData *join_data = mir_bb_data(mir, join);
                    struct MirBlockData *drop_data = mir_bb_data(mir, drop);

                    LIST_XFOREACH (before_data->successors, MirBlock const, after) {
                        struct MirBlockData const *after_data = mir_bb_data(mir, *after);
                        int const pred = mir_which_pred(mir, *after, before);
                        MirBlockList_set(after_data->predecessors, pred, join);
                        MirBlockList_push(join_data->successors, *after);
                    }

                    MirBlockList_push(join_data->predecessors, before);
                    MirBlockList_push(join_data->predecessors, drop);
                    MirBlockList_push(drop_data->predecessors, before);

                    push_instruction(mir, drop_data, drop_instr);
                    terminate_goto(mir, drop_data, join);

                    // add the instructions from "before" that need to be executed after the
                    // "drop" instruction, including the terminator
                    for (int j2 = j + 1; j2 < before_data->instructions->count; ++j2) {
                        struct MirInstruction *instr2 = MirInstructionList_get(before_data->instructions, j2);
                        MirInstructionList_push(join_data->instructions, instr2);
                    }
                    before_data->instructions->count = j; // omit "drop" instruction
                    // if *flag { goto drop; } else { goto join; }
                    struct MirPlace const drop_flag = get_drop_flag(V, (*pvar)->id);
                    terminate_branch(mir, before_data, drop_flag, drop, join);

                    BlockSet_insert(omit_blocks, drop, NULL);
                }
            }
        }
    }
}

static void initialize_data_structures(struct VariableAnalyzer *V)
{
    VariableList_reserve(V->locals, V->mir->registers->count);

    int index;
    struct MirRegisterData const *pdata;
    LIST_ENUMERATE (V->mir->registers, index, pdata) {
        struct Variable *local = new_variable(V, pdata->name, pdata->span, pdata->type, NULL);
        VariableList_push(V->locals, local);
    }

    BlockStates_reserve(V->blocks, V->mir->blocks->count);

    struct MirBlockData *const *pbb;
    LIST_ENUMERATE (V->mir->blocks, index, pbb) {
        VariableStates *states = new_states(V, V->num_vars);
        BlockStates_push(V->blocks, states);
        add_work_item(V, MIR_BB(index));
    }
}

static void remove_unnecessary_drops(struct VariableAnalyzer *V, MirBlock b)
{
    struct VariableStates **bs = &LIST_AT(V->blocks, b.value);
    struct MirBlockData *bb = mir_bb_data(V->mir, b);
    V->current = *bs;

    LIST_XFOREACH (bb->instructions, struct MirInstruction *, pinstr) {
        if (MirIsDrop(*pinstr)) {
            struct MirDrop const drop = *MirGetDrop(*pinstr);
            struct Variable const *var = *find_variable(V, drop.target);
            enum VariableState const state = states_get(V->current, var->id);
            if ((state != VAR_INIT && !is_known_cmove(V, var->id))
                    || !ir_needs_drop(V->C, ir_auto_deref(var->type)))
                *pinstr = mir_new_noop(V->mir, drop.span);
        }
    }
}

static void ensure_variable_initialization_before_use(struct Mir *mir)
{
    struct Compiler *C = mir->C;
    enter_unification_ctx(C->U, SCAN_STR(mir->C, "TODO"));

    struct Pool *pool = compiler_new_pool(C, C->aux_stats);
    struct VariableAnalyzer *V = &(struct VariableAnalyzer){
        .a = POOL_ALLOCATOR(pool),
        .pool = pool,
        .mir = mir,
        .C = C,
    };
    V->cmoves = ConditionalMoves_new(ALLOCATOR_OF(V));
    V->blocks = BlockStates_new(ALLOCATOR_OF(V));
    V->locals = VariableList_new(ALLOCATOR_OF(V));
    V->varcache = VarCache_new(ALLOCATOR_OF(V));
    V->work = Worklist_new(ALLOCATOR_OF(V));

    initialize_data_structures(V);
    while (Worklist_len(V->work) > 0) {
        MirBlock const w = Worklist_last(V->work);
        Worklist_pop(V->work);
        visit_block(V, w);
    }
    determine_cmoves(V);

    for (int b = 0; b < mir->blocks->count; ++b)
        remove_unnecessary_drops(V, MIR_BB(b));

    leave_unification_ctx(C->U);
}

void pawA_validate(struct Mir *mir)
{
    mir_merge_redundant_blocks(mir);

    IrSolver *S = ir_push_solver(mir->C);
    ir_solver_add_predicates_from(S, mir->did, mir->args,
            (struct IrObligationCause){
                .kind = IR_OBLIGATION_CAUSE_PREDICATE,
                .did = mir->did,
                .span = mir->span,
            });
    ensure_variable_initialization_before_use(mir);
    ir_pop_solver(mir->C);
}

