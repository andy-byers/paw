// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "map.h"
#include "mir.h"
#include"ir_type.h"

#define ERROR(R, code, ...) pawE_error(ENV((R)->C), code, -1, __VA_ARGS__)

struct RegisterAllocator {
    struct ScopeStack *scopes;
    struct Compiler *C;
    struct Mir *mir;
    Map *result;
    Map *active;
    int max_reg;
    int free_reg;
    int nlocals;
};

DEFINE_LIST(struct Compiler, reginfo_list_, RegisterInfoList, struct RegisterInfo *)
DEFINE_LIST(struct Compiler, scope_stack_, ScopeStack, int)

#if 0

static void debug_map(struct RegisterAllocator *R, Map *map)
{
    int table[LOCAL_MAX];
    for (int i = 0; i < LOCAL_MAX; ++i) table[i] = -1;

    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(map, &iter)) {
        const Value *key = pawH_key(map, iter);
        const Value *value = pawH_value(map, iter);
        struct RegisterInfo *info = value->p;
        paw_assert(key->i == info->ident);
        paw_assert(table[info->index] == -1);
        table[info->index] = info->ident;
    }

    printf("[\n");
    for (int i = 0; i < R->free_reg; ++i) {
        printf("  %d\t%d%s\n", i, table[i], i < R->nlocals ? " *" : "");
    }
    printf("]\n");
}

void debug(struct RegisterAllocator *R)
{
    printf("free_reg: %d, nlocals = %d\n", R->free_reg, R->nlocals);
    printf("active: ");
    debug_map(R, R->active);
    printf("result: ");
    debug_map(R, R->result);
}

#endif // 0

static struct RegisterInfo *new_reginfo(struct RegisterAllocator *R, struct MirRegister *r, int index)
{
    if (index > LOCAL_MAX) {
        pawE_error(ENV(R->C), PAW_EOVERFLOW, -1, "too many local variables (max is %d)", LOCAL_MAX);
    } else if (index > R->max_reg) {
        R->max_reg = index;
    }
    struct RegisterInfo *info = pawK_pool_alloc(ENV(R->C), R->C->pool, sizeof(struct RegisterInfo));
    *info = (struct RegisterInfo){
        .ident = r->value,
        .index = index,
    };
    return info;
}

static struct RegisterInfo *get_reginfo(struct RegisterAllocator *R, struct MirRegister *r)
{
    const Value *pval = pawH_get(R->active, I2V(r->value));
    return pval->p;
}

static void regfree(struct RegisterAllocator *R, struct MirRegister *r)
{
    struct RegisterInfo *info = get_reginfo(R, r);
    if (info->index < R->nlocals) return; // keep locals
    pawH_erase(R->active, I2V(r->value));

    paw_assert(info->index == R->free_reg - 1);
    --R->free_reg;
}

static struct RegisterInfo *regalloc(struct RegisterAllocator *R, struct MirRegister *r)
{
    const Value key = I2V(r->value);
    const Value *pval = pawH_get(R->active, key);
    if (pval != NULL) return pval->p;

    struct RegisterInfo *result = new_reginfo(R, r, R->free_reg++);
    pawH_insert(ENV(R->C), R->active, key, P2V(result));
    pawH_insert(ENV(R->C), R->result, key, P2V(result));
    return result;
}

static struct RegisterInfoList *regalloc_list(struct RegisterAllocator *R, struct MirRegisterList *regs)
{
    struct RegisterInfoList *result = reginfo_list_new(R->C);
    for (int i = 0; i < regs->count; ++i) {
        struct RegisterInfo *info = regalloc(R, K_LIST_GET(regs, i));
        K_LIST_PUSH(R->C, result, info);
    }
    return result;
}

static void regfree_list(struct RegisterAllocator *R, struct MirRegisterList *regs)
{
    for (int i = regs->count - 1; i >= 0; --i) {
        regfree(R, K_LIST_GET(regs, i));
    }
}

static void alloc_local(struct MirVisitor *V, struct MirLocal *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->output);
}

static void alloc_global(struct MirVisitor *V, struct MirGlobal *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->output);
}

static void alloc_constant(struct MirVisitor *V, struct MirConstant *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->output);
}

static void alloc_upvalue(struct MirVisitor *V, struct MirUpvalue *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->output);
}

static void alloc_set_upvalue(struct MirVisitor *V, struct MirSetUpvalue *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->value);
}

static void alloc_alloc_local(struct MirVisitor *V, struct MirAllocLocal *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->output);
    ++R->nlocals;
}

static void alloc_free_local(struct MirVisitor *V, struct MirFreeLocal *x)
{
    PAW_UNUSED(V);
    PAW_UNUSED(x);
}

static void alloc_enter_scope(struct MirVisitor *V, struct MirEnterScope *x)
{
    struct RegisterAllocator *R = V->ud;
    K_LIST_PUSH(R->C, R->scopes, R->nlocals);
}

static void alloc_leave_scope(struct MirVisitor *V, struct MirLeaveScope *x)
{
    struct RegisterAllocator *R = V->ud;
    R->nlocals = K_LIST_LAST(R->scopes);
    K_LIST_POP(R->scopes);

    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(R->active, &iter)) {
        const Value *pval = pawH_value(R->active, iter);
        const struct RegisterInfo *r = pval->p;
        if (r->index >= R->nlocals) {
            pawH_erase_at(R->active, iter);
        }
    }
    R->free_reg = R->nlocals;
}

static void alloc_aggregate(struct MirVisitor *V, struct MirAggregate *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->output);
}

static void alloc_explode(struct MirVisitor *V, struct MirExplode *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->input);
    regalloc_list(R, x->outputs); // TODO: ensure contiguousness w/ a flag or something, may need to be freed specially, i.e. all at once, since there might be multiple things
}

static void alloc_container(struct MirVisitor *V, struct MirContainer *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->output);
}

static void alloc_assign(struct MirVisitor *V, struct MirAssign *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->rhs);
}

static void alloc_call(struct MirVisitor *V, struct MirCall *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree_list(R, x->args);
    regfree(R, x->target);
    regalloc(R, x->output);
}

static void alloc_cast(struct MirVisitor *V, struct MirCast *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->target);
    regalloc(R, x->output);
}

static void alloc_closure(struct MirVisitor *V, struct MirClosure *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->output);
}

static void alloc_get_element(struct MirVisitor *V, struct MirGetElement *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->key);
    regfree(R, x->object);
    regalloc(R, x->output);
}

static void alloc_set_element(struct MirVisitor *V, struct MirSetElement *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->value);
    regfree(R, x->key);
    if (!x->is_init) regfree(R, x->object);
}

static void alloc_get_range(struct MirVisitor *V, struct MirGetRange *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->upper);
    regfree(R, x->lower);
    regfree(R, x->object);
    regalloc(R, x->output);
}

static void alloc_set_range(struct MirVisitor *V, struct MirSetRange *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->value);
    regfree(R, x->upper);
    regfree(R, x->lower);
    regfree(R, x->object);
}

static void alloc_get_field(struct MirVisitor *V, struct MirGetField *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->object);
    regalloc(R, x->output);
}

static void alloc_set_field(struct MirVisitor *V, struct MirSetField *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->value);
    if (!x->is_init) regfree(R, x->object);
}

static void alloc_unop(struct MirVisitor *V, struct MirUnaryOp *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->val);
    regalloc(R, x->output);
}

static void alloc_binop(struct MirVisitor *V, struct MirBinaryOp *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->rhs);
    regfree(R, x->lhs);
    regalloc(R, x->output);
}

static void alloc_discard(struct MirVisitor *V, struct MirDiscard *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->reg);
}

static void alloc_return(struct MirVisitor *V, struct MirReturn *x)
{
    struct RegisterAllocator *R = V->ud;
    if (x->value != NULL) regfree(R, x->value);
}

static paw_Bool alloc_goto(struct MirVisitor *V, struct MirGoto *x)
{
    return PAW_FALSE;
}

static paw_Bool alloc_for_loop(struct MirVisitor *V, struct MirForLoop *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->var);
    return PAW_FALSE;
}

static paw_Bool alloc_branch(struct MirVisitor *V, struct MirBranch *x)
{
    struct RegisterAllocator *R = V->ud;
    regfree(R, x->cond);
    return PAW_FALSE;
}

static paw_Bool alloc_switch(struct MirVisitor *V, struct MirSwitch *x)
{
    struct RegisterAllocator *R = V->ud;
    regalloc(R, x->discr);
    return PAW_FALSE;
}

Map *pawP_allocate_registers(struct Compiler *C, struct Mir *mir, struct MirBlockList *order, int *pmax_reg)
{
    struct MirVisitor V;
    struct RegisterAllocator R = {
        .scopes = scope_stack_new(C),
        .result = pawP_push_map(C),
        .active = pawP_push_map(C),
        .mir = mir,
        .C = C,
    };

    pawMir_visitor_init(&V, C, &R);
    V.PostVisitLocal = alloc_local;
    V.PostVisitUpvalue = alloc_upvalue;
    V.PostVisitGlobal = alloc_global;
    V.PostVisitConstant = alloc_constant;
    V.PostVisitSetUpvalue = alloc_set_upvalue;
    V.PostVisitAllocLocal = alloc_alloc_local;
    V.PostVisitFreeLocal = alloc_free_local;
    V.PostVisitEnterScope = alloc_enter_scope;
    V.PostVisitLeaveScope = alloc_leave_scope;
    V.PostVisitAggregate = alloc_aggregate;
    V.PostVisitContainer = alloc_container;
    V.PostVisitExplode = alloc_explode;
    V.PostVisitAssign = alloc_assign;
    V.PostVisitCall = alloc_call;
    V.PostVisitCast = alloc_cast;
    V.PostVisitClosure = alloc_closure;
    V.PostVisitGetElement = alloc_get_element;
    V.PostVisitSetElement = alloc_set_element;
    V.PostVisitGetRange = alloc_get_range;
    V.PostVisitSetRange = alloc_set_range;
    V.PostVisitGetField = alloc_get_field;
    V.PostVisitSetField = alloc_set_field;
    V.PostVisitUnaryOp = alloc_unop;
    V.PostVisitBinaryOp = alloc_binop;
    V.PostVisitDiscard = alloc_discard;

    V.PostVisitReturn = alloc_return;
    V.VisitBranch = alloc_branch;
    V.VisitGoto = alloc_goto;
    V.VisitSwitch = alloc_switch;

    // generate a mapping from virtual to real registers
    pawMir_visit_block_list(&V, order);

    *pmax_reg = R.max_reg;
    pawP_pop_object(C, R.active);
    return R.result; // return other map
}
