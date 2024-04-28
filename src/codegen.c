// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "array.h"
#include "ast.h"
#include "code.h"
#include "gc_aux.h"
#include "lex.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "type.h"

#define syntax_error(G, ...) pawX_error((G)->lex, __VA_ARGS__)
#define is_global(lex) (is_toplevel(lex) && (lex)->fs->bs->outer == NULL)
#define code_block(V, b) check_exp((b)->kind == STMT_BLOCK, V->visit_stmt(V, cast_stmt(b)))
#define basic_decl(G, code) basic_symbol(G, code)->decl
#define basic_type(G, code) basic_decl(G, code)->type.type
#define get_decl(G, id) ((G)->lex->pm->decls.data[id])
#define get_type(G, id) get_decl(G, id)->hdr.type
#define symbol_type(G, symbol) get_type(G, (symbol)->decl->hdr.def)

// TODO
#define visit_stmts(V, list) (V)->visit_stmt_list(V, list, (V)->visit_stmt)
#define visit_exprs(V, list) (V)->visit_expr_list(V, list, (V)->visit_expr)
#define visit_decls(V, list) (V)->visit_decl_list(V, list, (V)->visit_decl)
#define visit_methods(V, list) (V)->visit_method_list(V, list, (V)->visit_decl)

static void code_decl(Generator *, AstDecl *);
static void code_expr(Generator *, AstExpr *);
static void code_stmt(Generator *, AstStmt *);

static void mangle_type(Generator *G, Buffer *buf, Type *type)
{
    paw_Env *P = env(G->lex);
    if (y_is_unit(type)) {
        pawL_add_literal(P, buf, "0"); 
    } else if (y_is_bool(type)) {
        pawL_add_literal(P, buf, "b"); 
    } else if (y_is_int(type)) {
        pawL_add_literal(P, buf, "i"); 
    } else if (y_is_float(type)) {
        pawL_add_literal(P, buf, "f"); 
    } else if (y_is_string(type)) {
        pawL_add_literal(P, buf, "s"); 
    } else if (y_is_type_var(type)) {
        TypeVar *var = &type->var;
        pawL_add_nstring(P, buf, var->name->text, var->name->length);
    } else if (y_is_adt(type)) {
        Adt *adt = &type->adt;
        AstDecl *d = get_decl(G, adt->def);
        String *name = d->struct_.name;
        pawL_add_nstring(P, buf, name->text, name->length);
        for (int i = 0; i < adt->types.count; ++i) {
            mangle_type(G, buf, adt->types.types[i]);
        }
    } else {
        paw_assert(y_is_func(type));
        FuncSig *func = &type->func;
        pawL_add_char(P, buf, 'F');
        Binder *params = &func->params;
        for (int i = 0; i < params->count; ++i) {
            mangle_type(G, buf, params->types[i]);
        }
        pawL_add_char(P, buf, '_');
        mangle_type(G, buf, func->return_);
    }
}

// mangle('recv', 'attr') -> %recv.attr
static String *mangle_attr(Generator *G, String *recv, String *attr)
{
    Buffer buf;
    paw_Env *P = env(G->lex);
    pawL_init_buffer(P, &buf);
    pawL_add_char(P, &buf, '%');
    pawL_add_nstring(P, &buf, recv->text, recv->length);
    pawL_add_char(P, &buf, '.');
    pawL_add_nstring(P, &buf, attr->text, attr->length);
    pawL_push_result(P, &buf);
    String *result = v_string(P->top.p[-1]);
    pawC_pop(P); // unanchor
    return result;
}

// mangle('name', ()) -> name_
// mangle('name', ('int', 'A')) -> nameiA_
// mangle('name', ('A[int]',)) -> nameAi_
static String *mangle_name(Generator *G, String *name, Binder *binder)
{
    Buffer buf;
    paw_Env *P = env(G->lex);
    pawL_init_buffer(P, &buf);
    pawL_add_nstring(P, &buf, name->text, name->length);
    for (int i = 0; i < binder->count; ++i) {
        mangle_type(G, &buf, binder->types[i]);
    }
    pawL_add_char(P, &buf, '_');
    pawL_push_result(P, &buf);
    String *result = v_string(P->top.p[-1]);
    pawC_pop(P); // unanchor
    return result;
}

static Symbol *basic_symbol(Generator *G, paw_Type code)
{
    paw_assert(code >= 0 && code <= PAW_TSTRING);

    // basic types have fixed locations
    Scope *toplevel = G->sym->scopes[0];
    return toplevel->symbols[1 + code]; // TODO
}

static String *mangle_named_type(Generator *G, Type *type)
{
    paw_assert(y_is_adt(type));
    Adt *adt = &type->adt;
    AstDecl *decl = get_decl(G, adt->def);
    if (adt->types.count == 0) {
        return decl->struct_.name;
    }
    return mangle_name(G, decl->struct_.name, &adt->types);
}

static paw_Type basic_code(const Type *type)
{
    paw_assert(y_kind(type) == TYPE_BASIC);
    return type->hdr.def;
}

// TODO: Get rid of this
static Symbol *fetch_symbol(Scope *scope, const String *name, int *pindex)
{
    *pindex = pawP_find_symbol(scope, name);
    paw_assert(*pindex >= 0); // found in previous pass
    return scope->symbols[*pindex];
}

static void push_local_table(FuncState *fs, Scope *symbols)
{
    Generator *G = fs->G;
    SymbolTable *st = &fs->scopes;
    if (st->nscopes == UINT16_MAX) {
        syntax_error(G, "too many nested scopes");
    }
    pawM_grow(env(G->lex), st->scopes, st->nscopes, st->capacity);
    st->scopes[st->nscopes++] = symbols;
}

static void pop_local_table(FuncState *fs)
{
    // Last symbol table should have been assigned to an AST node. The
    // next call to push_symbol_table() will allocate a new table.
    SymbolTable *st = &fs->scopes;
    paw_assert(st->nscopes > 0);
    --st->nscopes;
}

static int add_constant(Generator *G, Value v)
{
    FuncState *fs = G->fs;
    Proto *p = fs->proto;

    if (fs->nk == UINT16_MAX) {
        syntax_error(G, "too many constants");
    } else if (fs->nk == p->nk) {
        // 'fs->nk' only ever increases by 1, so this will always give us
        // enough memory.
        pawM_grow(env(G->lex), p->k, fs->nk, p->nk);
        for (int i = fs->nk + 1; i < p->nk; ++i) {
            v_set_0(&p->k[i]); // clear for GC
        }
    }
    p->k[fs->nk] = v;
    return fs->nk++;
}

static int add_struct(Generator *G, Struct *c)
{
    FuncState *fs = G->fs;
    Proto *p = fs->proto;
    if (fs->nstructs == UINT16_MAX) {
        syntax_error(G, "too many structs");
    } else if (fs->nstructs == p->nc) {
        pawM_grow(env(G->lex), p->c, fs->nstructs, p->nc);
        for (int i = fs->nstructs; i < p->nc; ++i) {
            p->c[i] = NULL; // clear for GC (including current)
        }
    }
    p->c[fs->nstructs] = c;
    return fs->nstructs++;
}

static int add_proto(Generator *G, String *name, Proto **pp)
{
    Lex *lex = G->lex;
    FuncState *fs = G->fs;
    Proto *p = fs->proto;
    if (fs->nproto == UINT16_MAX) {
        syntax_error(G, "too many functions");
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

static void add_debug_info(Generator *G, Symbol *symbol)
{
    FuncState *fs = G->fs;
    Proto *p = fs->proto;
    if (fs->ndebug == LOCAL_MAX) {
        syntax_error(G, "too many locals");
    } else if (fs->ndebug == p->ndebug) {
        pawM_grow(env(G->lex), p->v, fs->ndebug, p->ndebug);
        for (int i = fs->ndebug + 1; i < p->ndebug; ++i) {
            p->v[i].var = (VarDesc){0}; // clear for GC
        }
    }
    p->v[fs->ndebug] = (struct LocalInfo){
        .var = {symbol->name, symbol->decl->hdr.def},
        .pc0 = fs->pc,
    };
    ++fs->ndebug;
}

static struct LocalInfo *local_info(FuncState *fs, int level)
{
    return &fs->proto->v[level];
}

static void close_vars(FuncState *fs, int target)
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

static VarInfo add_local(FuncState *fs, Symbol *symbol)
{
    Lex *lex = fs->G->lex;
    pawM_grow(env(lex), fs->locals.slots, fs->locals.nslots, fs->locals.capacity);
    const int index = fs->locals.nslots++;
    fs->locals.slots[index].symbol = symbol;
    fs->locals.slots[index].index = index; // TODO
    return (VarInfo){
        .symbol = symbol,
        .kind = VAR_LOCAL,
        .index = index,
    };
}

static paw_Bool symbol_iter(FuncState *fs, Scope *scope, Symbol **out)
{
    BlockState *bs = fs->bs;
    paw_assert(bs->isymbol < scope->nsymbols);
    *out = scope->symbols[bs->isymbol++];
    return (*out)->is_type; // skip types
}

static VarInfo transfer_local(FuncState *fs)
{
    Symbol *symbol;
    // Find the next symbol that belongs on the stack.
    SymbolTable *scopes = &fs->scopes; // all function scopes
    Scope *scope = scopes->scopes[scopes->nscopes - 1]; // last scope
    while (symbol_iter(fs, scope, &symbol)) {}
    return add_local(fs, symbol);
}

static VarInfo add_global(Generator *G)
{
    int index;
    Symbol *symbol;String*name; // TODO: Integer index to track current local
    pawE_new_global(env(G->lex), name, symbol->decl);
    return (VarInfo){
        .symbol = symbol,
        .kind = VAR_GLOBAL,
        .index = index,
    };
}

static Symbol *create_global(Generator *G, String *name)
{
    Symbol *symbol = pawA_new_symbol(G->lex);
    const int index = pawE_new_global(env(G->lex), name, symbol->decl);
    return symbol;
}

#define JUMP_PLACEHOLDER (-1)

static int code_jump(FuncState *fs, OpCode op)
{
    pawK_code_S(fs, op, JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static void patch_jump(FuncState *fs, int from, int to)
{
    const int jump = to - (from + 1);
    if (jump > JUMP_MAX) {
        syntax_error(fs->G, "too many instructions to jump");
    }
    Proto *p = fs->proto;
    set_S(&p->source[from], jump);
}

static void patch_here(FuncState *fs, int from)
{
    patch_jump(fs, from, fs->pc);
}

static void code_loop(FuncState *fs, Op op, int to)
{
    const int jump = to - (fs->pc + 1);
    if (jump > JUMP_MAX) {
        syntax_error(fs->G, "too many instructions in loop");
    }
    pawK_code_S(fs, op, jump);
}

static void code_closure(FuncState *fs, Proto *p, int id)
{
    Value v;
    v_set_object(&v, p);
    pawK_code_U(fs, OP_CLOSURE, id);
}

static void add_label(FuncState *fs, LabelKind kind)
{
    Lex *lex = fs->G->lex;
    LabelList *ll = &lex->pm->labels;
    pawM_grow(env(lex), ll->values, ll->length, ll->capacity);
    ll->values[ll->length] = (Label){
        .kind = kind,
        .line = lex->line,
        .level = fs->proto->ndebug,
        .pc = code_jump(fs, OP_JUMP),
    };
    ++ll->length;
}

static void adjust_labels(FuncState *fs, BlockState *bs)
{
    Lex *lex = fs->G->lex;
    LabelList *ll = &lex->pm->labels;
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

static void adjust_from(FuncState *fs, LabelKind kind)
{
    Lex *lex = fs->G->lex;
    BlockState *bs = fs->bs;
    LabelList *ll = &lex->pm->labels;
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

static void adjust_to(FuncState *fs, LabelKind kind, int to)
{
    Lex *lex = fs->G->lex;
    Proto *p = fs->proto;
    BlockState *bs = fs->bs;
    LabelList *ll = &lex->pm->labels;
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

static void begin_local_scope(FuncState *fs, int n)
{
    LocalStack *locals = &fs->locals;
    for (int i = 0; i < n; ++i) {
        const int level = fs->level++;
        LocalSlot slot = locals->slots[level];
        add_debug_info(fs->G, slot.symbol);
    }
}

static void end_local_scope(FuncState *fs, BlockState *bs)
{
    for (int i = fs->level - 1; i >= bs->level; --i) {
        local_info(fs, i)->pc1 = fs->pc;
    }
    fs->locals.nslots = bs->level;
    fs->level = bs->level;
}

static void leave_block(FuncState *fs)
{
    BlockState *bs = fs->bs;
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

static void enter_block(FuncState *fs, BlockState *bs, Scope *locals, paw_Bool loop)
{
    bs->label0 = fs->G->lex->pm->labels.length;
    bs->level = fs->level;
    bs->isymbol = 0;
    bs->is_loop = loop;
    bs->outer = fs->bs;
    fs->bs = bs;

    push_local_table(fs, locals);
}

static void leave_function(Generator *G)
{
    Lex *lex = G->lex;
    FuncState *fs = G->fs;
    BlockState *bs = fs->bs;
    Proto *p = fs->proto;

    // end function-scoped locals
    end_local_scope(fs, bs); 
    paw_assert(fs->level == 0);
    paw_assert(bs->outer == NULL);

    // TODO: Need a return at the end to handle cleaning up the stack
    //       Use a landing pad: all returns are just jumps to the landing pad
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

    G->fs = fs->outer;
    check_gc(env(lex));
}

static String *context_name(const FuncState *fs, FuncKind kind)
{
    if (fn_has_self(kind)) {
        return scan_string(fs->G->lex, "(self)");
    }
    return fs->proto->name;
}

static void enter_function(Generator *G, FuncState *fs, BlockState *bs, Scope *scope, FuncKind kind)
{
    Lex *lex = G->lex;

    fs->id = -1; // TODO: not used
    fs->bs = NULL;
    fs->scopes = (SymbolTable){0};
    fs->locals = (LocalStack){0};
    fs->nstructs = 0;
    fs->ndebug = 0;
    fs->nproto = 0;
    fs->nlines = 0;
    fs->level = 0;
    fs->nup = 0;
    fs->nk = 0;
    fs->pc = 0;

    fs->kind = kind;
    fs->outer = G->fs;
    fs->G = G;
    G->fs = fs;

    // Enter the function body.
    enter_block(fs, bs, scope, PAW_FALSE);

    transfer_local(fs);
    begin_local_scope(fs, 1);
}

static paw_Bool resolve_global(Generator *G, String *name, VarInfo *pinfo)
{
    int index;
    Lex *lex = G->lex;
    Scope *globals = lex->pm->symbols.globals;
    Symbol *symbol = fetch_symbol(globals, name, &index);
    pinfo->symbol = symbol;
    pinfo->kind = VAR_GLOBAL;
    pinfo->index = index;
    return PAW_TRUE;
}

static paw_Bool resolve_local(FuncState *fs, String *name, VarInfo *pinfo)
{
    for (int i = fs->level - 1; i >= 0; --i) {
        LocalSlot slot = fs->locals.slots[i];
        if (pawS_eq(slot.symbol->name, name)) {
            pinfo->symbol = slot.symbol;
            pinfo->kind = VAR_LOCAL;
            pinfo->index = i;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static VarInfo find_var(Generator *G, String *name);

static VarInfo resolve_attr(Generator *G, Type *type, String *name)
{
    paw_assert(y_is_adt(type));
    AstDecl *decl = get_decl(G, type->hdr.def);
    StructDecl *struct_ = &decl->struct_;
    Scope *scope = struct_->field_scope;
    VarInfo info = (VarInfo){.kind = VAR_FIELD};
    info.index = pawP_find_symbol(scope, name);
    if (info.index < 0) {
        scope = struct_->method_scope;
        info.index = pawP_find_symbol(scope, name);
        info.kind = VAR_METHOD;
    }
    paw_assert(info.index >= 0); // found in last pass
    info.symbol = scope->symbols[info.index];
    return info;
}

static void add_upvalue(FuncState *fs, String *name, VarInfo *info, paw_Bool is_local)
{
    Proto *f = fs->proto;
    for (int i = 0; i < fs->nup; ++i) {
        struct UpValueInfo up = f->u[i];
        if (up.index == info->index && up.is_local == is_local) {
            info->kind = VAR_UPVALUE;
            info->index = i;
            return;
        }
    }
    if (fs->nup == UPVALUE_MAX) {
        syntax_error(fs->G, "too many upvalues");
    } else if (fs->nup == f->nup) {
        pawM_grow(env(fs->G->lex), f->u, fs->nup, f->nup);
        for (int i = fs->nup + 1; i < f->nup; ++i) {
            f->u[i].var = (VarDesc){0}; // clear for GC
        }
    }
    f->u[fs->nup] = (struct UpValueInfo){
        .var = {name, info->symbol->decl->hdr.def},
        .is_local = is_local,
        .index = info->index,
    };
    info->index = fs->nup++;
    info->kind = VAR_UPVALUE;
}

static paw_Bool resolve_upvalue(FuncState *fs, String *name, VarInfo *pinfo)
{
    FuncState *caller = fs->outer;
    if (!caller) { // base case
        return PAW_FALSE;
    }
    // Check the caller's local variables.
    if (resolve_local(caller, name, pinfo)) {
        caller->proto->v[pinfo->index].captured = PAW_TRUE;
        add_upvalue(fs, name, pinfo, PAW_TRUE);
        return PAW_TRUE;
    }

    if (resolve_upvalue(caller, name, pinfo)) {
        add_upvalue(fs, name, pinfo, PAW_FALSE);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static VarInfo declare_var(FuncState *fs, paw_Bool global)
{
    return global ? add_global(fs->G)
                  : transfer_local(fs);
}

// Allow a previously-declared variable to be accessed
static void define_var(FuncState *fs, VarInfo info)
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

static VarInfo code_var(Generator *G, paw_Bool global)
{
    FuncState *fs = G->fs;
    VarInfo info = declare_var(fs, global);
    define_var(fs, info);
    return info;
}

static VarInfo inject_var(FuncState *fs, String *name, AstDecl *decl, paw_Bool global)
{
    paw_assert(!global);
    Symbol *symbol = pawA_new_symbol(fs->G->lex);
    symbol->name = name;
    symbol->decl = decl;
    return add_local(fs, symbol);
}

static VarInfo find_var(Generator *G, String *name)
{
    VarInfo info;
    Lex *lex = G->lex;
    FuncState *fs = G->fs;
    if (!resolve_local(fs, name, &info) && // not local
        !resolve_upvalue(fs, name, &info) && // not local to caller
        !resolve_global(G, name, &info)) { // not found
        pawX_error(lex, "undefined variable '%s'", name->text);
    }
    return info;
}

static void push_self(FuncState *fs)
{
    paw_assert(fs->G->cs != NULL);
    pawK_code_U(fs, OP_GETLOCAL, 0);
}

static void push_super(FuncState *fs)
{
    paw_assert(fs->G->cs != NULL);
    pawK_code_U(fs, OP_GETUPVALUE, 0);
}

// Push a variable on to the stack
static void code_getter(AstVisitor *V, VarInfo info)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    switch (info.kind) {
        case VAR_LOCAL:
            pawK_code_U(fs, OP_GETLOCAL, info.index);
            break;
        case VAR_UPVALUE:
            pawK_code_U(fs, OP_GETUPVALUE, info.index);
            break;
        case VAR_FIELD:
            pawK_code_U(fs, OP_GETATTR, info.index);
            break;
        case VAR_METHOD:
            // TODO: bind to receiver: this should not be called immediately, that must be
            //       handled separately so that OP_INVOKE can be generated
            pawK_code_U(fs, OP_GETATTR, info.index);
            break;
        default:
            paw_assert(info.kind == VAR_GLOBAL);
            pawK_code_U(fs, OP_GETGLOBAL, info.index);
    }
}

static void code_setter(AstVisitor *V, AstExpr *lhs, AstExpr *rhs)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    if (a_kind(lhs) == EXPR_NAME) {
        const VarInfo info = find_var(G, lhs->name.name);
        V->visit_expr(V, rhs);
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

    // index or field assignment
    SuffixedExpr *suf = &lhs->suffix; // common base
    V->visit_expr(V, suf->target); // push up to last expression
    if (a_kind(lhs) == EXPR_SELECTOR) {
        V->visit_expr(V, rhs);
        // resolve the field index
        String *name = lhs->selector.name;
        const VarInfo info = resolve_attr(G, a_type(suf->target), name);
        pawK_code_U(fs, OP_SETATTR, info.index);
    } else {
        paw_assert(a_kind(lhs) == EXPR_INDEX);
        visit_exprs(V, lhs->index.elems);
        V->visit_expr(V, rhs);
        pawK_code_0(fs, OP_SETITEM);
    }
}

static void code_ident_expr(AstVisitor *V, AstIdent *e)
{
    Generator *G = V->state.G;
    const VarInfo info = find_var(G, e->name);
    code_getter(V, info);
}

static void code_basic_lit(AstVisitor *V, LiteralExpr *e)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    if (e->basic.t == PAW_TUNIT) {
        pawK_code_0(fs, OP_PUSHUNIT);
    } else if (e->basic.t != PAW_TBOOL) {
        const int k = add_constant(G, e->basic.value);
        pawK_code_U(fs, OP_PUSHCONST, k);
    } else if (v_true(e->basic.value)) {
        pawK_code_0(fs, OP_PUSHTRUE);
    } else {
        pawK_code_0(fs, OP_PUSHFALSE);
    }
}

static void code_composite_lit(AstVisitor *V, LiteralExpr *lit)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    CompositeLit *e = &lit->comp;
    String *name = mangle_named_type(G, e->target->hdr.type);
    VarInfo info = find_var(G, name);

    StructDecl *struct_ = &info.symbol->decl->struct_;
    
    code_getter(V, info); // get Struct
    pawK_code_U(fs, OP_NEWINSTANCE, struct_->fields->count);

    AstExpr *attr = e->items->first;
    while (attr != NULL) {
        V->visit_expr(V, attr);
        pawK_code_U(fs, OP_INITATTR, attr->item.index);
        attr = attr->hdr.next;
    }
}

static void code_literal_expr(AstVisitor *V, LiteralExpr *e)
{
    switch (e->lit_kind) {
        case LIT_BASIC:
            code_basic_lit(V, e);
            break;
        default:
            code_composite_lit(V, e);
    }
}

static void code_and(AstVisitor *V, LogicalExpr *e)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    V->visit_expr(V, e->lhs);
    const int jump = code_jump(fs, OP_JUMPFALSE);
    pawK_code_0(fs, OP_POP);
    V->visit_expr(V, e->rhs);
    patch_here(fs, jump);
}

static void code_or(AstVisitor *V, LogicalExpr *e)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    V->visit_expr(V, e->lhs);
    const int else_jump = code_jump(fs, OP_JUMPFALSE);
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    pawK_code_0(fs, OP_POP);
    V->visit_expr(V, e->rhs);
    patch_here(fs, then_jump);
}

static void code_logical_expr(AstVisitor *V, LogicalExpr *e)
{
    if (e->is_and) {
        code_and(V, e);
    } else {
        code_or(V, e);
    }
}

static void code_chain_expr(AstVisitor *V, ChainExpr *e)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    V->visit_expr(V, e->target);
    const int else_jump = code_jump(fs, OP_JUMPNULL);
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    pawK_code_0(fs, OP_RETURN);
    patch_here(fs, then_jump);
}

static void code_cond_expr(AstVisitor *V, CondExpr *e)
{
    FuncState *fs = V->state.G->fs;

    V->visit_expr(V, e->cond);
    const int else_jump = code_jump(fs, OP_JUMPFALSEPOP);
    V->visit_expr(V, e->lhs);
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    // same type as 'e->lhs'
    V->visit_expr(V, e->rhs);
    patch_here(fs, then_jump);
}

#define code_op(fs, op, subop, type) pawK_code_AB(fs, op, cast(subop, int), basic_code(type))

static void code_unop_expr(AstVisitor *V, UnOpExpr *e)
{
    FuncState *fs = V->state.G->fs;

    V->visit_expr(V, e->target);
    code_op(fs, OP_UNOP, e->op, e->type);
}

static void code_binop_expr(AstVisitor *V, BinOpExpr *e)
{
    FuncState *fs = V->state.G->fs;
    V->visit_expr(V, e->lhs);
    V->visit_expr(V, e->rhs);
    code_op(fs, OP_BINOP, e->op, e->type);
}

static void code_decl_stmt(AstVisitor *V, AstDeclStmt *s)
{
    V->visit_decl(V, s->decl);
}

static void code_expr_stmt(AstVisitor *V, AstExprStmt *s)
{
    FuncState *fs = V->state.G->fs;

    if (s->rhs != NULL) {
        code_setter(V, s->lhs, s->rhs); 
        return;
    }
    V->visit_expr(V, s->lhs); // function call
    pawK_code_0(fs, OP_POP); // unused return value
}

static void code_func(AstVisitor *V, FuncDecl *d, FuncKind kind)
{
    Generator *G = V->state.G;

    FuncState fs;
    BlockState bs;
    fs.name = d->name;
    fs.G = G;

    FuncSig *func = &d->type->func;
    const int id = add_proto(G, d->name, &fs.proto);
    fs.proto->argc = func->params.count;
    enter_function(G, &fs, &bs, d->scope, kind);
    visit_decls(V, d->params); // code parameters
    V->visit_block_stmt(V, d->body); // code function body
    leave_function(G);

    code_closure(G->fs, fs.proto, id);
}

static String *func_name(Generator *G, FuncDecl *func)
{
    FuncSig *sig = &func->type->func;
    if (!func->is_poly && sig->types.count > 0) {
        // TODO: Consider mangling all symbol names, not just
        //       template instances. Mangle templates with their
        //       generic parameter names?
        return mangle_name(G, func->name, &sig->types);
    }
    return func->name;
}

static String *method_name(Generator *G, StructDecl *parent, FuncDecl *method)
{
    String *name = func_name(G, method);
    return mangle_attr(G, parent->name, name);
}

static void register_fields(Generator *G, StructDecl *parent)
{
    AstDecl *fields = parent->fields->first;
    parent->field_scope = pawM_new(env(G->lex), Scope); // TODO
    for (int i = 0; i < parent->fields->count; ++i) {
        FieldDecl *d = &fields->field;
        Symbol *symbol = pawP_add_symbol(G->lex, parent->field_scope);
        symbol->is_init = PAW_TRUE;
        symbol->name = d->name;
        symbol->decl = fields;
        fields = d->next;
    }
}

static void register_methods(Generator *G, StructDecl *parent)
{
    AstDecl *methods = parent->methods->first;
    parent->method_scope = pawM_new(env(G->lex), Scope); // TODO
    for (int i = 0; i < parent->methods->count; ++i) {
        AstDecl *decl = methods;
        if (decl->hdr.next != NULL) {
            paw_assert(decl->func.is_poly);
            decl = decl->hdr.next;
        }
        while (decl != NULL) {
            FuncDecl *d = &decl->func;
            Symbol *symbol = pawP_add_symbol(G->lex, parent->method_scope);
            symbol->is_init = PAW_TRUE;
            symbol->name = func_name(G, d);
            symbol->decl = decl;
            decl = d->next;
        }
        methods = methods->func.sibling;
    }
}

static void code_methods(AstVisitor *V, StructDecl *parent, Struct *struct_)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    Scope *scope = parent->method_scope;
    for (int i = 0; i < scope->nsymbols; ++i) {
        Symbol *symbol = scope->symbols[i];
        FuncDecl *d = &symbol->decl->func;
        code_func(V, d, FUNC_METHOD);
        pawK_code_U(fs, OP_NEWMETHOD, i);
    }
    // Make room for the methods to be set at runtime.
    if (parent->methods->count > 0) {
        pawA_resize(env(G->lex), struct_->methods, cast_size(parent->methods->count));
    }
}

static void code_struct(AstVisitor *V, StructDecl *d)
{
    BlockState bs;
    Generator *G = V->state.G;
    Lex *lex = G->lex;
    paw_Env *P = env(lex);
    FuncState *fs = G->fs;

    Value *pv = pawC_push0(P);
    Struct *struct_ = pawV_new_struct(P, pv);
    const int index = add_struct(G, struct_);

    pawK_code_U(fs, OP_PUSHSTRUCT, index);
    enter_block(fs, &bs, d->scope, PAW_FALSE);

    register_fields(G, d);
    register_methods(G, d);
    code_methods(V, d, struct_);

    leave_block(fs); // layout->scope
    pawC_pop(P); // pop 'struct_'
}

// TODO: Never generate code for structs whose types contain free type variables
//       Those structures are created 
static void code_struct_template(AstVisitor *V, StructDecl *tmpl)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    AstDecl *d = tmpl->next;
    while (d != NULL) {
        StructDecl *struct_ = &d->struct_;
        String *name = mangle_named_type(G, struct_->type);
        const VarInfo info = inject_var(fs, name, d, struct_->is_global);
        define_var(fs, info);
        code_struct(V, struct_);
        d = struct_->next;
    }
}

static void code_field_decl(AstVisitor *V, FieldDecl *d)
{
    code_var(V->state.G, PAW_FALSE);
    paw_unused(d);
}

static void code_var_decl(AstVisitor *V, VarDecl *s)
{
    FuncState *fs = V->state.G->fs;
    const VarInfo info = declare_var(fs, s->is_global);
    V->visit_expr(V, s->init);
    define_var(fs, info);
}

static void code_struct_decl(AstVisitor *V, StructDecl *d)
{
    if (d->is_poly) {
        code_struct_template(V, d); 
        return;
    }
    FuncState *fs = V->state.G->fs;
    const VarInfo info = declare_var(fs, d->is_global);
    define_var(fs, info);
    code_struct(V, d);
}

static void code_item_expr(AstVisitor *V, ItemExpr *e)
{
    V->visit_expr(V, e->value);
}

static void code_block_stmt(AstVisitor *V, Block *b)
{
    BlockState bs;
    FuncState *fs = V->state.G->fs;
    enter_block(fs, &bs, b->scope, PAW_FALSE);
    visit_stmts(V, b->stmts);
    leave_block(fs);
}

static void code_return_stmt(AstVisitor *V, ReturnStmt *s)
{
    Generator *G = V->state.G;
    Lex *lex = G->lex;
    FuncState *fs = G->fs;
    if (is_toplevel(G)) {
        pawX_error(lex, "return from module is not allowed"); 
    }
    V->visit_expr(V, s->expr);
    pawK_code_0(fs, OP_RETURN);
}

static paw_Bool is_method_call(const AstExpr *e)
{
    return a_kind(e) == EXPR_SELECTOR && e->selector.is_method;
}

static paw_Bool is_instance_call(const Type *type)
{
    return type->func.types.count > 0;
}

static void code_instance_getter(AstVisitor *V, Type *type)
{
    Generator *G = V->state.G;
    AstDecl *decl = get_decl(G, type->hdr.def);
    Binder binder = y_is_adt(type) ? type->adt.types : type->func.types;
    String *name = mangle_name(G, decl->hdr.name, &binder);
    const VarInfo info = find_var(G, name);
    code_getter(V, info);
}

static void code_call_expr(AstVisitor *V, CallExpr *e)
{
    //const int invoke = start_call(G, e);
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    int invoke = -1;
    if (e->func->hdr.def == NO_DECL) {
        V->visit_expr(V, e->target);
    } else if (is_instance_call(e->func)) {
        code_instance_getter(V, e->func);
    } else if (is_method_call(e->target)) {
        AstDecl *decl = get_decl(G, e->func->hdr.def);
        Selector *select = &e->target->selector;
        Type *parent = a_type(decl->func.receiver);
        String *name = func_name(G, &decl->func);
        // emit code for the receiver and save the method index
        V->visit_expr(V, select->target);
        const VarInfo info = resolve_attr(G, parent, name);
        invoke = info.index;
    } else {
        V->visit_expr(V, e->target);
    }

    // push function arguments
    visit_exprs(V, e->args);

    if (invoke < 0) {
        pawK_code_U(fs, OP_CALL, e->args->count);
    } else {
        pawK_code_AB(fs, OP_INVOKE, invoke, e->args->count);
    }
}

static void code_func_template(AstVisitor *V, FuncDecl *tmpl)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    AstDecl *d = tmpl->next;
    while (d != NULL) {
        FuncDecl *func = &d->func;
        String *name = func_name(G, func);
        const VarInfo info = inject_var(fs, name, d, func->is_global);
        code_func(V, func, FUNC_FUNCTION);
        define_var(fs, info);
        d = func->next;
    }
}

static void code_func_decl(AstVisitor *V, FuncDecl *d)
{
    Generator *G = V->state.G;
    if (d->is_poly) {
        code_func_template(V, d); 
        return;
    }
    FuncState *fs = G->fs;
    const VarInfo info = declare_var(fs, d->is_global);
    code_func(V, d, FUNC_FUNCTION);
    define_var(fs, info);
}

static void code_if_stmt(AstVisitor *V, IfStmt *s)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    V->visit_expr(V, s->cond);
    const int else_jump = code_jump(fs, OP_JUMPFALSEPOP);
    V->visit_stmt(V, s->then_arm);
    // NOTE: If there is no 'else' block, this will produce a NOOP jump.
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    V->visit_stmt(V, s->else_arm);
    patch_here(fs, then_jump);
}

static void close_until_loop(FuncState *fs)
{
    Lex *lex = fs->G->lex;
    BlockState *bs = fs->bs;
    while (bs->outer) {
        // Emit close/pop instructions, but don't end any lifetimes. Code
        // paths that doesn't hit this label may still need those locals.
        BlockState *outer = bs->outer;
        if (outer->is_loop) {
            close_vars(fs, bs->level);
            return;
        }
        bs = outer;
    }
    pawX_error(lex, "label outside loop");
}

static void code_label_stmt(AstVisitor *V, LabelStmt *s)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    close_until_loop(fs); // fix the stack
    add_label(fs, s->label); // emit a jump, to be patched later
}

static void code_while_stmt(AstVisitor *V, WhileStmt *s)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    BlockState bs;

    enter_block(fs, &bs, s->scope, PAW_TRUE);
    const int loop = fs->pc;
    V->visit_expr(V, s->cond);

    const int jump = code_jump(fs, OP_JUMPFALSEPOP);
    V->visit_block_stmt(V, s->block);

    // Finish the loop. 'break' labels jump here, 'continue' labels back to right
    // before where the conditional expression was evaluated.
    code_loop(fs, OP_JUMP, loop);
    adjust_to(fs, LCONTINUE, loop);
    patch_here(fs, jump);
    leave_block(fs);
}

static void code_dowhile_stmt(AstVisitor *V, WhileStmt *s)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    BlockState bs;

    enter_block(fs, &bs, s->scope, PAW_TRUE);
    const int loop = fs->pc;
    V->visit_block_stmt(V, s->block);
    adjust_from(fs, LCONTINUE);
    V->visit_expr(V, s->cond);

    // If the condition is false, jump over the instruction that moves control back
    // to the top of the loop.
    const int jump = code_jump(fs, OP_JUMPFALSEPOP);
    code_loop(fs, OP_JUMP, loop);
    patch_here(fs, jump);
    leave_block(fs);
}

static void code_forbody(AstVisitor *V, Block *block, Op opinit, Op oploop)
{
    BlockState bs;
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    // Emit OP_FOR*0, which either pushes the loop variables, or jumps over
    // the loop.
    const int jump = code_jump(fs, opinit);
    const int loop = fs->pc;

    // Enter a scope for the loop variable: if it is captured in a closure, 
    // the upvalue must be closed at the end of the iteration.
    enter_block(fs, &bs, block->scope, PAW_FALSE);
    code_var(G, PAW_FALSE);
    V->visit_block_stmt(V, block);
    leave_block(fs);

    // Continue statements jump here, right before the loop instruction.
    adjust_from(fs, LCONTINUE);
    code_loop(fs, oploop, loop);
    patch_here(fs, jump);
}

static void code_fornum_stmt(AstVisitor *V, ForStmt *s)
{
    Generator *G = V->state.G;
    ForNum *fornum = &s->fornum;

    V->visit_expr(V, fornum->begin);
    V->visit_expr(V, fornum->end);
    V->visit_expr(V, fornum->step);
    code_var(G, PAW_FALSE);
    code_var(G, PAW_FALSE);
    code_var(G, PAW_FALSE);

    code_forbody(V, s->block, OP_FORNUM0, OP_FORNUM);
}

static void code_forin_stmt(AstVisitor *V, ForStmt *s) // TODO: forin would need to encode the type of object being iterated over. look into function call for loop? 
{
    Generator *G = V->state.G;
    ForIn *forin = &s->forin;

    V->visit_expr(V, forin->target);
    code_var(G, PAW_FALSE);

    code_forbody(V, s->block, OP_FORIN0, OP_FORIN);
}

static void code_for_stmt(AstVisitor *V, ForStmt *s)
{
    BlockState bs;
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    enter_block(fs, &bs, s->scope, PAW_TRUE);
    if (s->kind == STMT_FORNUM) {
        code_fornum_stmt(V, s);
    } else {
        code_forin_stmt(V, s);
    }
    leave_block(fs);
}

//static void code_array_expr(AstVisitor *V, ArrayExpr *e)
//{
//    FuncState *fs = G->fs;
//
//    visit_exprs(V, e->items);
//    pawK_code_U(fs, OP_NEWARRAY, e->nitems);
//}
//
//static void code_map_expr(AstVisitor *V, MapExpr *e)
//{
//    visit_exprs(V, e->items);
//    pawK_code_U(G->fs, OP_NEWARRAY, e->items.size);
//}

static paw_Bool is_index_template(Generator *G, const Type *type)
{
    AstDecl *decl = get_decl(G, type->hdr.def);
    if (a_is_func_decl(decl)) {
        return decl->func.is_poly;
    } else {
        return decl->struct_.is_poly;
    }
}

static void code_index_expr(AstVisitor *V, Index *e)
{
    Generator *G = V->state.G;
    const Type *target = a_type(e->target);
    if (is_index_template(G, target)) {
        code_instance_getter(V, e->type);
        return;
    }
    V->visit_expr(V, e->target);
    V->visit_expr(V, e->elems->first);

    const paw_Type tt = basic_code(target);
    const paw_Type et = basic_code(a_type(e->elems->first));
    pawK_code_AB(G->fs, OP_GETITEM, tt, et);
}

// TODO: Need to handle a.b and a.b[c], where 'b' is a method (need to bind to 'self')
static void code_selector_expr(AstVisitor *V, Selector *e)
{
    Generator *G = V->state.G;
    V->visit_expr(V, e->target);
    const VarInfo info = resolve_attr(G, a_type(e->target), e->name);
    pawK_code_U(G->fs, OP_GETATTR, info.index);
}

static void setup_pass(AstVisitor *V, Generator *G)
{
    const AstState state = {.G = G};
    pawA_visitor_init(V, G->ast, state);
    V->visit_literal_expr = code_literal_expr;
    V->visit_logical_expr = code_logical_expr;
    V->visit_ident_expr = code_ident_expr;
    V->visit_chain_expr = code_chain_expr;
    V->visit_unop_expr = code_unop_expr;
    V->visit_binop_expr = code_binop_expr;
    V->visit_cond_expr = code_cond_expr;
    V->visit_call_expr = code_call_expr;
    V->visit_index_expr = code_index_expr;
    V->visit_selector_expr = code_selector_expr;
    V->visit_item_expr = code_item_expr;
    V->visit_block_stmt = code_block_stmt;
    V->visit_expr_stmt = code_expr_stmt;
    V->visit_decl_stmt = code_decl_stmt;
    V->visit_if_stmt = code_if_stmt;
    V->visit_for_stmt = code_for_stmt;
    V->visit_while_stmt = code_while_stmt;
    V->visit_dowhile_stmt = code_dowhile_stmt;
    V->visit_label_stmt = code_label_stmt;
    V->visit_return_stmt = code_return_stmt;
    V->visit_var_decl = code_var_decl;
    V->visit_func_decl = code_func_decl;
    V->visit_struct_decl = code_struct_decl;
    V->visit_field_decl = code_field_decl;
}

static void code_module(Generator *G)
{
    Lex *lex = G->lex;
    ParseMemory *pm = lex->pm;

    FuncState fs;
    BlockState bs;
    fs.name = lex->modname;
    fs.proto = lex->main->p;

    Scope *toplevel = pm->symbols.toplevel;
    enter_function(G, &fs, &bs, toplevel, FUNC_MODULE);

    AstVisitor V;
    setup_pass(&V, G);
    pawA_visit(&V);

    leave_function(G);
}

void p_generate_code(Lex *lex)
{
    Generator G = {
        .lex = lex,
        .ast = lex->pm->ast,
        .sym = &lex->pm->symbols,
    };
    code_module(&G);
}
