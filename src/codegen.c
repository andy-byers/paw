// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "code.h"
#include "gc.h"
#include "lex.h"
#include "mem.h"
#include "parse.h"
#include "type.h"

#define is_global(lex) (is_toplevel(lex) && (lex)->fs->bs->outer == NULL)

static Symbol *fetch_symbol(Scope *scope, const String *name, int *pindex)
{
    *pindex = pawP_find_symbol(scope, name);
    paw_assert(*pindex >= 0); // found in previous pass
    return scope->symbols[*pindex];
}

static void push_local_table(FnState *fs, Scope *symbols)
{
    Lex *lex = fs->lex;
    SymbolTable *st = &fs->scopes;
    if (st->nscopes == UINT16_MAX) {
        limit_error(lex, "symbols", UINT16_MAX);
    }
    pawM_grow(env(lex), st->scopes, st->nscopes, st->capacity);
    st->scopes[st->nscopes++] = symbols;
}

static void pop_local_table(FnState *fs)
{
    // Last symbol table should have been assigned to an AST node. The
    // next call to push_symbol_table() will allocate a new table.
    SymbolTable *st = &fs->scopes;
    paw_assert(st->nscopes > 0);
    --st->nscopes;
}

static int add_constant(Lex *lex, Value v)
{
    FnState *fs = lex->fs;
    Proto *p = fs->proto;

    if (fs->nk == UINT16_MAX) {
        limit_error(lex, "constants", UINT16_MAX);
    } else if (fs->nk == p->nk) {
        // 'fs->nk' only ever increases by 1, so this will always give us
        // enough memory.
        pawM_grow(env(lex), p->k, fs->nk, p->nk);
        for (int i = fs->nk + 1; i < p->nk; ++i) {
            v_set_0(&p->k[i]); // clear for GC
        }
    }
    p->k[fs->nk] = v;
    return fs->nk++;
}

static int add_proto(Lex *lex, String *name, Proto **pp)
{
    FnState *fs = lex->fs;
    Proto *p = fs->proto;
    if (fs->nproto == UINT16_MAX) {
        limit_error(lex, "functions", UINT16_MAX);
    } else if (fs->nproto == p->nproto) {
        pawM_grow(env(lex), p->p, fs->nproto, p->nproto);
        for (int i = fs->nproto; i < p->nproto; ++i) {
            p->p[i] = NULL; // clear for GC (including current)
        }
    }
    Proto *callee = pawV_new_proto(env(lex));
    callee->modname = lex->modname;
    callee->name = name;

    const int id = fs->nproto++;
    p->p[id] = *pp = callee;
    return id;
}

static void add_debug_info(Lex *lex, Symbol *var)
{
    FnState *fs = lex->fs;
    Proto *p = fs->proto;
    if (fs->ndebug == LOCAL_MAX) {
        limit_error(lex, "locals", LOCAL_MAX);
    } else if (fs->ndebug == p->ndebug) {
        pawM_grow(env(lex), p->v, fs->ndebug, p->ndebug);
        for (int i = fs->ndebug + 1; i < p->ndebug; ++i) {
            p->v[i].var = (VarDesc){0}; // clear for GC
        }
    }
    p->v[fs->ndebug] = (struct LocalInfo){
        .var = {var->type, var->name},
        .pc0 = fs->pc,
    };
    ++fs->ndebug;
}

static struct LocalInfo *local_info(FnState *fs, int level)
{
    return &fs->proto->v[level];
}

static void close_vars(FnState *fs, int target)
{
    const int upper = fs->level - 1; // first slot to pop
    const int lower = target - 1; // 1 past 'reverse' end
    paw_assert(lower <= upper);
    for (int i = upper; i > lower; --i) {
        struct LocalInfo *local = local_info(fs, i);
        if (local->captured) {
            pawK_code_0(fs, OP_CLOSE);
        } else {
            pawK_code_0(fs, OP_POP);
        }
    }
}

static VarInfo add_local(FnState *fs, String *name)
{
    int unused;
    Lex *lex = fs->lex;
    SymbolTable *scopes = &fs->scopes; // all function scopes
    Scope *symbols = scopes->scopes[scopes->nscopes - 1]; // last scope
    Symbol *symbol = fetch_symbol(symbols, name, &unused);
    pawM_grow(env(lex), fs->locals.symbols, fs->locals.nsymbols, fs->locals.capacity);
    const int index = fs->locals.nsymbols++;
    fs->locals.symbols[index] = symbol;
    return (VarInfo){
        .type = symbol->type,
        .kind = VAR_LOCAL,
        .index = index,
    };
}

static VarInfo add_local_literal(FnState *fs, const char *name)
{
    String *str = scan_string(fs->lex, name);
    return add_local(fs, str);
}

static VarInfo add_global(Lex *lex, String *name)
{
    int index;
    Symbol *symbol = fetch_symbol(lex->pm->st.globals, name, &index);
    pawE_new_global(env(lex), name, symbol->type);
    return (VarInfo){
        .type = symbol->type,
        .kind = VAR_GLOBAL,
        .index = index,
    };
}

static int add_class(Lex *lex, ClassType *cls)
{
    FnState *fs = lex->fs;
    Proto *p = fs->proto;
    if (fs->nclasses == UINT16_MAX) {
        limit_error(lex, "classes", UINT16_MAX);
    } else if (fs->nclasses == p->nclasses) {
//        pawM_grow(env(lex), p->c, fs->nclass, p->nclass);
//        for (int i = fs->nclass; i < p->nclass; ++i) {
//            p->c[i] = NULL; // clear for GC (including current)
//        }
    }
//    p->classes[fs->nclasses] = pawV_new_class(env(lex), cls);
    return fs->nclasses++;
}

#define JUMP_PLACEHOLDER (-1)

static int code_jump(FnState *fs, OpCode op)
{
    pawK_code_S(fs, op, JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static void patch_jump(FnState *fs, int from, int to)
{
    Lex *lex = fs->lex;
    const int jump = to - (from + 1);
    if (jump > JUMP_MAX) {
        limit_error(lex, "instructions to jump", JUMP_MAX);
    }
    Proto *p = fs->proto;
    set_S(&p->source[from], jump);
}

static void patch_here(FnState *fs, int from)
{
    patch_jump(fs, from, fs->pc);
}

static void code_loop(FnState *fs, Op op, int to)
{
    Lex *lex = fs->lex;
    const int jump = to - (fs->pc + 1);
    if (jump > JUMP_MAX) {
        limit_error(lex, "instructions in loop", JUMP_MAX);
    }
    pawK_code_S(fs, op, jump);
}

static void code_closure(FnState *fs, Proto *p, int id)
{
    Value v;
    v_set_object(&v, p);
    pawK_code_U(fs, OP_CLOSURE, id);
}

static void add_label(FnState *fs, LabelKind kind)
{
    Lex *lex = fs->lex;
    LabelList *ll = &lex->pm->ll;
    pawM_grow(env(lex), ll->values, ll->length, ll->capacity);
    ll->values[ll->length] = (Label){
        .kind = kind,
        .line = lex->line,
        .level = fs->proto->ndebug,
        .pc = code_jump(fs, OP_JUMP),
    };
    ++ll->length;
}

static void adjust_labels(FnState *fs, BlkState *bs)
{
    Lex *lex = fs->lex;
    LabelList *ll = &lex->pm->ll;
    for (int i = bs->label0; i < ll->length; ++i) {
        Label *lb = &ll->values[i];
        lb->level = bs->level;
    }
}

static void remove_label(LabelList *ll, int index)
{
    paw_assert(ll->length > 0);
    for (int i = index; i < ll->length - 1; ++i) {
        ll->values[i] = ll->values[i + 1];
    }
    --ll->length;
}

static void adjust_from(FnState *fs, LabelKind kind)
{
    Lex *lex = fs->lex;
    BlkState *bs = fs->bs;
    LabelList *ll = &lex->pm->ll;
    for (int i = bs->label0; i < ll->length;) {
        Label *lb = &ll->values[i];
        if (lb->kind == kind) {
            patch_here(fs, lb->pc);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static void adjust_to(FnState *fs, LabelKind kind, int to)
{
    Lex *lex = fs->lex;
    Proto *p = fs->proto;
    BlkState *bs = fs->bs;
    LabelList *ll = &lex->pm->ll;
    for (int i = bs->label0; i < ll->length;) {
        Label *lb = &ll->values[i];
        if (lb->kind == kind) {
            const int jump = to - (lb->pc + 1);
            set_S(&p->source[lb->pc], jump);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static void begin_local_scope(FnState *fs, int n)
{
    Lex *lex = fs->lex;
    Scope *locals = &fs->locals;
    for (int i = 0; i < n; ++i) {
        const int level = fs->level++;
        Symbol *var = locals->symbols[level];
        add_debug_info(lex, var);
    }
}

static void end_local_scope(FnState *fs, BlkState *bs)
{
    for (int i = fs->level - 1; i >= bs->level; --i) {
        local_info(fs, i)->pc1 = fs->pc;
    }
    fs->locals.nsymbols = bs->level;
    fs->level = bs->level;
}

static void leave_block(FnState *fs)
{
    BlkState *bs = fs->bs;
    if (bs->is_loop) {
        adjust_from(fs, LBREAK);
    }
    close_vars(fs, bs->level);
    end_local_scope(fs, bs);
    if (bs->outer) {
        adjust_labels(fs, bs);
    }
    fs->bs = bs->outer;
    pop_local_table(fs);
}

static void enter_block(FnState *fs, BlkState *bs, Scope *locals, paw_Bool loop)
{
    bs->label0 = fs->lex->pm->ll.length;
    bs->level = fs->level;
    bs->is_loop = loop;
    bs->outer = fs->bs;
    fs->bs = bs;

    push_local_table(fs, locals);
}

static void leave_function(Lex *lex)
{
    FnState *fs = lex->fs;
    BlkState *bs = fs->bs;
    Proto *p = fs->proto;

    // end function-scoped locals
    end_local_scope(fs, bs); 
    paw_assert(fs->level == 0);
    paw_assert(bs->outer == NULL);

    // TODO: Check for missing return, this won't work for functions that return a value
    //       It is an error if such a function is missing a return at the end
    pawK_code_0(fs, OP_PUSHUNIT);
    pawK_code_0(fs, OP_RETURN); 

    pawM_shrink(env(lex), p->source, p->length, fs->pc);
    p->length = fs->pc;
    pawM_shrink(env(lex), p->lines, p->nlines, fs->nlines);
    p->nlines = fs->nlines;
    pawM_shrink(env(lex), p->p, p->nproto, fs->nproto);
    p->nproto = fs->nproto;
    pawM_shrink(env(lex), p->v, p->ndebug, fs->ndebug);
    p->ndebug = fs->ndebug;
    pawM_shrink(env(lex), p->u, p->nup, fs->nup);
    p->nup = fs->nup;
    pawM_shrink(env(lex), p->k, p->nk, fs->nk);
    p->nk = fs->nk;

    lex->fs = fs->outer;
    check_gc(env(lex));
}

static String *context_name(const FnState *fs, FnKind kind)
{
    if (fn_has_self(kind)) {
        return v_string(pawE_cstr(env(fs->lex), CSTR_SELF));
    }
    return fs->proto->name;
}

static void enter_function(Lex *lex, FnState *fs, BlkState *bs, Scope *scope, FnKind kind)
{
    fs->bs = NULL;
    fs->scopes = (SymbolTable){0};
    fs->locals = (Scope){0};
    fs->ndebug = 0;
    fs->nproto = 0;
    fs->nlines = 0;
    fs->level = 0;
    fs->nup = 0;
    fs->nk = 0;
    fs->pc = 0;

    fs->kind = kind;
    fs->outer = lex->fs;
    fs->lex = lex;
    lex->fs = fs;

    // Enter the function body.
    enter_block(fs, bs, scope, PAW_FALSE);

    // Create the context variable in slot 0. For VCLOSURE, this slot holds the closure
    // object being called. For VMETHOD, it holds the class instance that the method is
    // being called on, i.e. the implicit 'self' parameter.
    add_local(fs, context_name(fs, kind));
    begin_local_scope(fs, 1);
}

static paw_Bool resolve_global(Lex *lex, String *name, VarInfo *pinfo)
{
    int index;
    Scope *globals = lex->pm->st.globals;
    Symbol *symbol = fetch_symbol(globals, name, &index);
    pinfo->type = symbol->type;
    pinfo->kind = VAR_GLOBAL;
    pinfo->index = index;
    return PAW_TRUE;
}

// Find an active local variable with the given 'name'
// Only consider locals that have been brought into scope, using begin_local_scope().
static paw_Bool resolve_local(FnState *fs, String *name, VarInfo *pinfo)
{
    for (int i = fs->level - 1; i >= 0; --i) {
        Symbol *var = fs->locals.symbols[i];
        if (pawS_eq(var->name, name)) {
            pinfo->index = i;
            pinfo->kind = VAR_LOCAL;
            pinfo->type = var->type;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static VarInfo add_upvalue(FnState *fs, String *name, int index, Type *type, paw_Bool is_local)
{
    Proto *f = fs->proto;
    for (int i = 0; i < fs->nup; ++i) {
        struct UpValueInfo up = f->u[i];
        if (up.index == index && up.is_local == is_local) {
            return (VarInfo){
                .index = i, 
                .kind = VAR_UPVALUE, 
                .type = up.var.type,
            };
        }
    }
    if (fs->nup == UPVALUE_MAX) {
        limit_error(fs->lex, "upvalues", UPVALUE_MAX);
    } else if (fs->nup == f->nup) {
        pawM_grow(env(fs->lex), f->u, fs->nup, f->nup);
        for (int i = fs->nup + 1; i < f->nup; ++i) {
            f->u[i].var = (VarDesc){0}; // clear for GC
        }
    }
    f->u[fs->nup] = (struct UpValueInfo){
        .var = {type, name},
        .is_local = is_local,
        .index = index,
    };
    return (VarInfo){
        .index = fs->nup++, 
        .kind = VAR_UPVALUE, 
        .type = type,
    };
}

static paw_Bool resolve_upvalue(FnState *fs, String *name, VarInfo *pinfo)
{
    FnState *caller = fs->outer;
    if (!caller) { // base case
        return PAW_FALSE;
    }
    // Check the caller's local variables.
    if (resolve_local(caller, name, pinfo)) {
        const int local = pinfo->index;
        caller->proto->v[local].captured = PAW_TRUE;
        *pinfo = add_upvalue(fs, name, local, pinfo->type, PAW_TRUE);
        return PAW_TRUE;
    }

    if (resolve_upvalue(caller, name, pinfo)) {
        const int upvalue = pinfo->index;
        *pinfo = add_upvalue(fs, name, upvalue, pinfo->type, PAW_FALSE);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static VarInfo declare_var(FnState *fs, String *name, paw_Bool global)
{
    return global ? add_global(fs->lex, name)
                  : add_local(fs, name);
}

// Allow a previously-declared variable to be accessed
static void define_var(FnState *fs, VarInfo info)
{
    if (info.kind == VAR_LOCAL) {
        begin_local_scope(fs, 1);
    } else {
        // Write initial value to the globals table. Reference counts are adjusted
        // automatically.
        paw_assert(info.kind == VAR_GLOBAL);
        pawK_code_U(fs, OP_SETGLOBAL, info.index);
    }
}

static VarInfo new_var(Visitor *V, String *name, paw_Bool global)
{
    FnState *fs = V->lex->fs;
    VarInfo info = declare_var(fs, name, global);
    define_var(fs, info);
    return info;
}

static VarInfo find_var(Visitor *V, String *name)
{
    VarInfo info;
    Lex *lex = V->lex;
    FnState *fs = lex->fs;
    if (!resolve_local(fs, name, &info) && // not local
        !resolve_upvalue(fs, name, &info) && // not local to caller
        !resolve_global(lex, name, &info)) { // not defined
        pawX_error(lex, "undefined variable '%s'", name->text);
    }
    return info;
}

#define needs_conversion(a, b) ((a) != (b) && y_is_scalar(a) && y_is_scalar(b))

static void convert_bool(FnState *fs, Type *src)
{
    if (!y_is_bool(src)) {
        pawK_code_U(fs, OP_CASTBOOL, y_id(src)); 
    }
}

// Push a variable on to the stack
static void code_discharge(Visitor *V, VarExpr *e)
{
    FnState *fs = V->lex->fs;
    const VarInfo info = find_var(V, e->name);
    switch (info.kind) {
        case VAR_LOCAL:
            pawK_code_U(fs, OP_GETLOCAL, info.index);
            break;
        case VAR_UPVALUE:
            pawK_code_U(fs, OP_GETUPVALUE, info.index);
            break;
        default:
            paw_assert(info.kind == VAR_GLOBAL);
            pawK_code_U(fs, OP_GETGLOBAL, info.index);
    }
}

static void code_assignment(Visitor *V, Expr *lhs, Expr *rhs)
{
    FnState *fs = V->lex->fs;

    if (lhs->kind == EXPR_VAR) {
        V->expr(V, rhs); // push RHS
        VarExpr *e = cast_to(lhs, VarExpr);
        const VarInfo info = find_var(V, e->name);
        switch (info.kind) {
            case VAR_LOCAL:
                pawK_code_U(fs, OP_SETLOCAL, info.index);
                break;
            case VAR_UPVALUE:
                pawK_code_U(fs, OP_SETUPVALUE, info.index);
                break;
            default:
                paw_assert(info.kind == VAR_GLOBAL);
                pawK_code_U(fs, OP_SETGLOBAL, info.index);
        }
        return;
    }

    // index, range, or attr assignment
    SuffixedExpr *base = cast_to(lhs, SuffixedExpr); // common base
    V->expr(V, base->target); // push up to last expression
    if (lhs->kind == EXPR_INDEX) {
        const IndexExpr *last = cast_to(lhs, IndexExpr);
        V->expr(V, last->first);
        if (last->second) {
            V->expr(V, last->second);
            V->expr(V, rhs);
            pawK_code_0(fs, OP_SETSLICE);
        } else {
            V->expr(V, rhs);
            pawK_code_0(fs, OP_SETITEM);
        }
    } else {
        paw_assert(lhs->kind == EXPR_ACCESS);
        const AccessExpr *e = cast_to(lhs, AccessExpr);
        V->expr(V, rhs);
        pawK_code_U(fs, OP_SETATTR, e->index);
    }
}

static void code_var_expr(Visitor *V, VarExpr *e)
{
    code_discharge(V, e);
}

static void code_primitive_expr(Visitor *V, PrimitiveExpr *e)
{
    FnState *fs = V->lex->fs;
    if (y_is_unit(e->type)) {
        pawK_code_0(fs, OP_PUSHUNIT);
    } else if (y_is_bool(e->type)) {
        pawK_code_0(fs, v_true(e->v) ? OP_PUSHTRUE : OP_PUSHFALSE);
    } else {
        const int k = add_constant(V->lex, e->v);
        pawK_code_U(fs, OP_PUSHCONST, k);
    }
}

static void code_literal_expr(Visitor *V, LiteralExpr *e)
{
    FnState *fs = V->lex->fs;
    V->expr(V, e->expr);
    add_local_literal(fs, e->label);
}

static void code_and(Visitor *V, LogicalExpr *e)
{
    FnState *fs = V->lex->fs;

    V->expr(V, e->lhs);
    convert_bool(fs, e->lhs->type);
    const int jump = code_jump(fs, OP_JUMPFALSE);
    pawK_code_0(fs, OP_POP);
    V->expr(V, e->rhs);
    patch_here(fs, jump);
}

static void code_or(Visitor *V, LogicalExpr *e)
{
    FnState *fs = V->lex->fs;

    V->expr(V, e->lhs);
    convert_bool(fs, e->lhs->type);
    const int else_jump = code_jump(fs, OP_JUMPFALSE);
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    pawK_code_0(fs, OP_POP);
    V->expr(V, e->rhs);
    patch_here(fs, then_jump);
}

static void code_logical_expr(Visitor *V, LogicalExpr *e)
{
    if (e->is_and) {
        code_and(V, e);
    } else {
        code_or(V, e);
    }
}

static void code_chain_expr(Visitor *V, ChainExpr *e)
{
    FnState *fs = V->lex->fs;

    V->expr(V, e->target);
    const int else_jump = code_jump(fs, OP_JUMPNULL);
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    // Return the value on top, which is either 'null', or an instance that
    // returned 'null' from its '__null' metamethod.
    pawK_code_0(fs, OP_RETURN);
    patch_here(fs, then_jump);
}

static void code_cond_expr(Visitor *V, CondExpr *e)
{
    FnState *fs = V->lex->fs;

    V->expr(V, e->cond);
    convert_bool(fs, e->cond->type);
    const int else_jump = code_jump(fs, OP_JUMPFALSEPOP);
    V->expr(V, e->lhs);
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    V->expr(V, e->rhs);
    patch_here(fs, then_jump);
}

static void code_coalesce_expr(Visitor *V, CoalesceExpr *e)
{
    FnState *fs = V->lex->fs;

    V->expr(V, e->lhs);
    const int else_jump = code_jump(fs, OP_JUMPNULL);
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    pawK_code_0(fs, OP_POP);
    V->expr(V, e->rhs);
    patch_here(fs, then_jump);
}

#define code_op(fs, op, subop, type) pawK_code_AB(fs, op, cast(subop, int), y_id(type))

static void code_unop_expr(Visitor *V, UnOpExpr *e)
{
    FnState *fs = V->lex->fs;

    V->expr(V, e->target);
    code_op(fs, OP_UNOP, e->op, e->target->type);
}

static void code_binop_mm(FnState *fs, FunctionType *sig, Type *lhs, Type *rhs)
{

}

static void code_binop_expr(Visitor *V, BinOpExpr *e)
{
    FnState *fs = V->lex->fs;

    Type *lhs_type = e->lhs->type;
    Type *rhs_type = e->rhs->type;
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
    if (e->mm == NULL) {
        paw_assert(pawY_is_same(lhs_type, rhs_type));
        code_op(fs, OP_BINOP, e->op, lhs_type);
    } else {
        code_binop_mm(fs, e->mm, lhs_type, rhs_type);
    }
}

static void code_expr_stmt(Visitor *V, ExprStmt *s)
{
    FnState *fs = V->lex->fs;

    if (s->rhs != NULL) {
        code_assignment(V, s->lhs, s->rhs); 
        return;
    }
    V->expr(V, s->lhs); // function call
    pawK_code_0(fs, OP_POP); // unused return value
}

static void code_fn(Visitor *V, Function *fn)
{
    BlkState bs;
    FnState fs;
    Function *outer = V->fn;
    V->fn = fn;

    fs.name = fn->name;

    Lex *lex = V->lex;
    const int id = add_proto(lex, fn->name, &fs.proto);
    fs.proto->argc = fn->nargs;
    enter_function(lex, &fs, &bs, fn->scope, FN_FUNCTION);
    V->stmt_list(V, fn->args); // code parameters
    V->block_stmt(V, fn->body); // code function body
    leave_function(lex);

    // Create, and allow access to, the closure object.
    code_closure(lex->fs, fs.proto, id);
    V->fn = outer;
}

static void code_attr_stmt(Visitor *V, AttrStmt *s)
{
    Lex *lex = V->lex;
    FnState *fs = lex->fs;

    if (s->is_fn) {
        code_fn(V, &s->fn);
    } else {
        pawK_code_0(fs, OP_PUSHUNIT); 
    }         
}

static void code_class_stmt(Visitor *V, ClassStmt *s)
{
    ClsState cs;
    Lex *lex = V->lex;
    FnState *fs = lex->fs;
    lex->cs = &cs;

    const VarInfo var = new_var(V, s->name, s->flags.global);
    V->stmt_list(V, s->attrs);
    pawK_code_U(fs, OP_NEWCLASS, y_id(var.type));
    lex->cs = cs.outer;
}

static void code_item_stmt(Visitor *V, ItemStmt *s)
{
    V->expr(V, s->value);
}

static void code_init_expr(Visitor *V, InitExpr *e)
{
    V->expr(V, e->prefix);
    Stmt *attr = e->attrs;
    while (attr != NULL) { // TODO: That won't work! Fix me!
        V->stmt(V, attr);
        attr = attr->next;
    }
    Lex *lex = V->lex;
    FnState *fs = lex->fs;
    ClassType *cls = &e->prefix->type->cls;
    pawK_code_U(fs, OP_INIT, cls->nattrs);
}

static void code_block_stmt(Visitor *V, Block *bk)
{
    BlkState bs;
    FnState *fs = V->lex->fs;
    enter_block(fs, &bs, bk->scope, PAW_FALSE);
    V->stmt_list(V, bk->stmts);
    leave_block(fs);
}

static void code_param_stmt(Visitor *V, ParamStmt *s)
{
    new_var(V, s->name, PAW_FALSE);
}

static void code_def_stmt(Visitor *V, DefStmt *s)
{
    Lex *lex = V->lex;
    FnState *fs = lex->fs;
    const VarInfo info = declare_var(fs, s->name, s->flags.global);
    V->expr(V, s->init);
    define_var(fs, info);
}

static void code_return_stmt(Visitor *V, ReturnStmt *s)
{
    Lex *lex = V->lex;
    FnState *fs = lex->fs;
    if (is_toplevel(lex)) {
        pawX_error(lex, "return from module is not allowed"); 
    }
    V->expr(V, s->expr);
    pawK_code_0(fs, OP_RETURN);
}

static void code_call_expr(Visitor *V, CallExpr *e)
{
    FnState *fs = V->lex->fs;
    V->expr(V, e->target); // callable

    // Code the function parameters.
    Type *fn_type = e->target->type;
    paw_assert(y_is_function(fn_type));
    for (Expr *arg = e->args; arg != NULL; arg = arg->next) {
        Expr *param = cast_expr(arg); 
        V->expr(V, param); // code parameter
    }
    pawK_code_U(fs, OP_CALL, e->nargs);
}

static void code_fn_stmt(Visitor *V, FnStmt *s)
{
    code_fn(V, &s->fn);

    // Associate the function object with a variable.
    new_var(V, s->fn.name, s->flags.global);
}

static void code_ifelse_stmt(Visitor *V, IfElseStmt *s)
{
    FnState *fs = V->lex->fs;

    V->expr(V, s->cond);
    convert_bool(fs, s->cond->type);
    const int else_jump = code_jump(fs, OP_JUMPFALSEPOP);
    V->stmt(V, s->then_arm);
    // NOTE: If there is no 'else' block, this will produce a NOOP jump.
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    V->stmt(V, s->else_arm);
    patch_here(fs, then_jump);
}

static void close_until_loop(FnState *fs)
{
    Lex *lex = fs->lex;
    BlkState *bs = fs->bs;
    while (bs->outer) {
        // Emit close/pop instructions, but don't end any lifetimes. Code
        // paths that doesn't hit this label may still need those locals.
        BlkState *outer = bs->outer;
        if (outer->is_loop) {
            close_vars(fs, bs->level);
            return;
        }
        bs = outer;
    }
    pawX_error(lex, "label outside loop");
}

static void code_label_stmt(Visitor *V, LabelStmt *s)
{
    FnState *fs = V->lex->fs;
    close_until_loop(fs); // fix the stack
    add_label(fs, s->label); // emit a jump, to be patched later
}

static void code_while_stmt(Visitor *V, WhileStmt *s)
{
    FnState *fs = V->lex->fs;

    const int loop = fs->pc;
    V->expr(V, s->cond);
    convert_bool(fs, s->cond->type);

    const int jump = code_jump(fs, OP_JUMPFALSEPOP);
    V->block_stmt(V, s->block);

    // Finish the loop. 'break' labels jump here, 'continue' labels back to right
    // before where the conditional expression was evaluated.
    code_loop(fs, OP_JUMP, loop);
    adjust_to(fs, LCONTINUE, loop);
    patch_here(fs, jump);
}

static void code_dowhile_stmt(Visitor *V, WhileStmt *s)
{
    FnState *fs = V->lex->fs;

    const int loop = fs->pc;
    V->block_stmt(V, s->block);
    adjust_from(fs, LCONTINUE);
    V->expr(V, s->cond);
    convert_bool(fs, s->cond->type);

    // If the condition is false, jump over the instruction that moves control back
    // to the top of the loop.
    const int jump = code_jump(fs, OP_JUMPFALSEPOP);
    code_loop(fs, OP_JUMP, loop);
    patch_here(fs, jump);
}

static void code_forbody(Visitor *V, String *iname, Block *block, Op opinit, Op oploop)
{
    BlkState bs;
    FnState *fs = V->lex->fs;

    // Emit OP_FOR*0, which either pushes the loop variables, or jumps over
    // the loop.
    const int jump = code_jump(fs, opinit);
    const int loop = fs->pc;

    enter_block(fs, &bs, block->scope, PAW_FALSE);
    add_local(fs, iname);
    begin_local_scope(fs, 1);
    V->block_stmt(V, block);
    leave_block(fs); // close loop variable

    // Continue statements jump here, right before the loop instruction.
    adjust_from(fs, LCONTINUE);
    code_loop(fs, oploop, loop);
    patch_here(fs, jump);
}

static void code_fornum_stmt(Visitor *V, ForStmt *s)
{
    FnState *fs = V->lex->fs;
    ForNum *fornum = &s->fornum;
    V->expr(V, fornum->begin);
    V->expr(V, fornum->end);
    V->expr(V, fornum->step);

    begin_local_scope(fs, 3);
    code_forbody(V, s->name, s->block, OP_FORNUM0, OP_FORNUM);
}

static void code_forin_stmt(Visitor *V, ForStmt *s) // TODO: forin would need to encode the type of object being iterated over. look into function call for loop? 
{
    FnState *fs = V->lex->fs;
    ForIn *forin = &s->forin;
    V->expr(V, forin->target);
    add_local(fs, s->name);

    begin_local_scope(fs, 2);
    code_forbody(V, s->name, s->block, OP_FORIN0, OP_FORIN);
}

static void code_for_stmt(Visitor *V, ForStmt *s)
{
    BlkState bs;
    FnState *fs = V->lex->fs;
    enter_block(fs, &bs, s->scope, PAW_TRUE);
    if (s->kind == STMT_FORNUM) {
        code_fornum_stmt(V, s);
    } else {
        code_forin_stmt(V, s);
    }
    leave_block(fs);
}

static void code_array_expr(Visitor *V, ArrayExpr *e)
{
    FnState *fs = V->lex->fs;

    V->expr_list(V, e->items);
    pawK_code_U(fs, OP_NEWARRAY, e->nitems);
}

//static void code_map_expr(Visitor *V, MapExpr *e)
//{
//    V->expr_list(V, e->items);
//    pawK_code_U(V->lex->fs, OP_NEWARRAY, e->items.size);
//}

static void code_index_expr(Visitor *V, IndexExpr *e)
{
    V->expr(V, e->target);
    V->expr(V, e->first);

    Op op;
    if (e->second != NULL) {
        V->expr(V, e->second);
        op = OP_GETSLICE;
    } else {
        op = OP_GETITEM;
    }
    pawK_code_AB(V->lex->fs, op, y_id(e->target->type), y_id(e->first->type));
}

static void code_access_expr(Visitor *V, AccessExpr *e)
{
    V->expr(V, e->target);
    pawK_code_U(V->lex->fs, OP_GETATTR, e->index);
}

static void code_invoke_expr(Visitor *V, InvokeExpr *e)
{
    FnState *fs = V->lex->fs;
    V->expr(V, e->target);

    Expr *arg = e->args;
    while (arg != NULL) {
        V->expr(V, arg);
        arg = arg->next;
    }

    pawK_code_AB(fs, OP_INVOKE, e->index, e->nargs);
}


void p_generate_code(Lex *lex)
{
    Visitor V;
    pawK_init_visitor(&V, lex);
    V.primitive_expr = code_primitive_expr;
    V.literal_expr = code_literal_expr;
    V.logical_expr = code_logical_expr;
    V.chain_expr = code_chain_expr;
    V.cond_expr = code_cond_expr;
    V.coalesce_expr = code_coalesce_expr;
    V.unop_expr = code_unop_expr;
    V.binop_expr = code_binop_expr;
    V.var_expr = code_var_expr;
    V.array_expr = code_array_expr;
//    V.map_expr = code_map_expr;
    V.access_expr = code_access_expr;
    V.invoke_expr = code_invoke_expr;
    V.index_expr = code_index_expr;
    V.init_expr = code_init_expr;
    V.return_stmt = code_return_stmt;
    V.call_expr = code_call_expr;
    V.param_stmt = code_param_stmt;
    V.block_stmt = code_block_stmt;
    V.class_stmt = code_class_stmt;
    V.item_stmt = code_item_stmt;
    V.attr_stmt = code_attr_stmt;
    V.def_stmt = code_def_stmt;
    V.fn_stmt = code_fn_stmt;
    V.for_stmt = code_for_stmt;
    V.while_stmt = code_while_stmt;
    V.dowhile_stmt = code_dowhile_stmt;
    V.label_stmt = code_label_stmt;
    V.ifelse_stmt = code_ifelse_stmt;
    V.expr_stmt = code_expr_stmt;

    BlkState bs;
    FnState fs = {
        .name = lex->modname,
        .proto = lex->main->p,
    };
    enter_function(lex, &fs, &bs, lex->pm->st.toplevel, FN_MODULE);
    pawK_visit(&V, lex->ast);
    leave_function(lex);
}
