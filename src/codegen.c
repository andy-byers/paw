// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ast.h"
#include "code.h"
#include "gc_aux.h"
#include "lex.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "type.h"
#include "vector.h"

#define syntax_error(G, ...) pawX_error((G)->lex, __VA_ARGS__)
#define is_global(lex) (is_toplevel(lex) && (lex)->fs->bs->outer == NULL)
#define code_block(V, b)                                                       \
    check_exp((b)->kind == STMT_BLOCK, V->visit_stmt(V, cast_stmt(b)))
#define basic_decl(G, code) basic_symbol(G, code)->decl
#define basic_type(G, code) basic_decl(G, code)->type.type
#define get_decl(G, id) pawA_get_decl((G)->ast, id)
#define get_type(G, id) get_decl(G, id)->hdr.type
#define symbol_type(G, symbol) get_type(G, (symbol)->decl->hdr.def)

// TODO
#define visit_stmts(V, list) (V)->visit_stmt_list(V, list, (V)->visit_stmt)
#define visit_exprs(V, list) (V)->visit_expr_list(V, list, (V)->visit_expr)
#define visit_decls(V, list) (V)->visit_decl_list(V, list, (V)->visit_decl)

static void mangle_type(Generator *G, Buffer *buf, AstType *type)
{
    paw_Env *P = env(G->lex);
    if (a_is_unit(type)) {
        pawL_add_literal(P, buf, "0");
    } else if (a_is_bool(type)) {
        pawL_add_literal(P, buf, "b");
    } else if (a_is_int(type)) {
        pawL_add_literal(P, buf, "i");
    } else if (a_is_float(type)) {
        pawL_add_literal(P, buf, "f");
    } else if (a_is_string(type)) {
        pawL_add_literal(P, buf, "s");
    } else if (a_is_generic(type)) {
        AstGeneric *var = &type->generic;
        pawL_add_nstring(P, buf, var->name->text, var->name->length);
    } else if (a_is_adt(type)) {
        AstAdt *adt = &type->adt;
        AstDecl *d = get_decl(G, adt->def);
        String *name = d->struct_.name;
        pawL_add_nstring(P, buf, name->text, name->length);
        for (int i = 0; i < adt->types->count; ++i) {
            mangle_type(G, buf, adt->types->data[i]);
        }
    } else {
        paw_assert(a_is_func(type));
        AstFuncSig *func = &type->func;
        pawL_add_char(P, buf, 'F');
        AstList *params = func->params;
        for (int i = 0; i < params->count; ++i) {
            mangle_type(G, buf, params->data[i]);
        }
        pawL_add_char(P, buf, '_');
        mangle_type(G, buf, func->return_);
    }
}

// mangle('name', ()) -> name_
// mangle('name', ('int', 'A')) -> nameiA_
// mangle('name', ('A[int]',)) -> nameAi_
static String *mangle_name(Generator *G, String *name, AstList *binder)
{
    Buffer buf;
    paw_Env *P = env(G->lex);
    pawL_init_buffer(P, &buf);
    pawL_add_nstring(P, &buf, name->text, name->length);
    for (int i = 0; i < binder->count; ++i) {
        mangle_type(G, &buf, binder->data[i]);
    }
    pawL_add_char(P, &buf, '_');
    pawL_push_result(P, &buf);
    String *result = v_string(P->top.p[-1]);
    pawC_pop(P); // unanchor
    return result;
}

static String *get_type_name(Generator *G, AstType *type)
{
    paw_assert(a_is_adt(type));
    AstAdt *adt = &type->adt;
    AstDecl *decl = get_decl(G, adt->base);
    return decl->struct_.name;
}

static paw_Type basic_code(const AstType *type)
{
    if (a_kind(type) == AST_TYPE_ADT) {
        return type->adt.base;
    } else {
        paw_assert(a_kind(type) == AST_TYPE_BASIC);
        return type->hdr.def;
    }
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

static int get_width(Generator *G, const AstType *type)
{
    if (type->hdr.def == NO_DECL) {
        return 1; // function pointer
    }

    int width;
    const AstDecl *decl = get_decl(G, type->hdr.def);
    if (a_kind(decl) == DECL_VARIANT) {
        const DefId id = type->adt.base;
        const AstDecl *base = get_decl(G, id);
        width = base->struct_.width;
    } else if (a_kind(decl) == DECL_STRUCT) {
        const StructDecl *struct_ = &decl->struct_;
        width = struct_->width;
    } else {
        return 1;
    }
    if (width == 0) {
        syntax_error(G, "recursive types are not allowed");
    }
    return width;
}

static void compute_width(Generator *G, AstDecl *decl);

static int compute_fields_width(Generator *G, const AstList *list)
{
    int accum = 0;
    for (int i = 0; i < list->count; ++i) {
        AstDecl *decl = list->data[i];
        compute_width(G, decl);

        FieldDecl *field = &decl->field;
        field->offset = accum;
        accum += field->width;
    }
    return accum;
}

static void compute_enum_width(Generator *G, StructDecl *d)
{
    paw_assert(d->width == 0);
    for (int i = 0; i < d->fields->count; ++i) {
        AstDecl *decl = d->fields->data[i];
        VariantDecl *var = &decl->variant;
        paw_assert(var->used == 0);
        var->used = compute_fields_width(G, var->fields);
        if (d->width < var->used) {
            d->width = var->used;
        }
    } 
    // account for discriminator
    ++d->width;
}

// Determine the number of stack slots needed to represent the given value 
// Recursively populates the size of each field in a composite type.
static void compute_width(Generator *G, AstDecl *decl)
{
    if (a_kind(decl) == DECL_VARIANT) {
        decl->variant.used = compute_fields_width(G, decl->variant.fields);
    } else if (a_kind(decl) == DECL_FUNC) {
        decl->func.param_width = compute_fields_width(G, decl->func.params);
    } else if (a_kind(decl) == DECL_FIELD) {
        decl->field.width = get_width(G, decl->field.type);
    } else if (a_kind(decl) != DECL_STRUCT) {
        // width is 1, implicitly
    } else if (decl->struct_.is_struct) {
        decl->struct_.width = 1; // TODO: value semantics for structures
        //decl->struct_.width = compute_fields_width(G, decl->struct_.fields);
    } else {
        compute_enum_width(G, &decl->struct_);
    }
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

static int add_struct(Generator *G, Struct *struct_)
{
    paw_Env *P = env(G->lex);
    struct StructVec *sv = &P->sv;
    if (sv->size == UINT16_MAX) {
        syntax_error(G, "too many structs");
    }
    pawM_grow(P, sv->data, sv->size, sv->alloc);
    sv->data[sv->size] = struct_;
    return sv->size++;
}

static int add_proto(Generator *G, FuncDecl *d, Proto **pp)
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
    callee->nresults = get_width(G, a_type(d->return_));
    callee->modname = lex->modname;
    callee->name = d->name;

    const int id = fs->nproto++;
    p->p[id] = *pp = callee;
    return id;
}

static void add_debug_info(Generator *G, LocalSlot *slot)
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
    Symbol *symbol = slot->symbol;
    p->v[fs->ndebug] = (struct LocalInfo){
        .var = {symbol->name, symbol->decl->hdr.def},
        .index = slot->index,
        .width = slot->width,
        .pc0 = fs->pc,
    };
    slot->debug = fs->ndebug;
    ++fs->ndebug;
}

static struct LocalInfo *local_info(FuncState *fs, int level)
{
    const LocalSlot *slot = &fs->locals.slots[level];
    return &fs->proto->v[slot->debug];
}

// TODO: Have OP_CLOSE/OP_POP take an immediate argument that specifies n (how many slots to close/pop), 
//       rather than generating n close/pop instructions! If anything needs to be closed, generate OP_CLOSE,
//       otherwise OP_POP. Track that info separately to avoid having to search for local.captured == 1?
static void close_vars(FuncState *fs, int target)
{
    const int upper = fs->level - 1; // first slot to pop
    const int lower = target - 1; // 1 past 'reverse' end
    paw_assert(lower <= upper);
    for (int i = upper; i > lower; --i) {
        struct LocalInfo *local = local_info(fs, i);
        for (int i = 1; i < local->width; ++i) {
            pawK_code_0(fs, OP_POP);
        }
        if (local->is_captured) {
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
    const int width = get_width(fs->G, a_type(symbol->decl));
    const int islot = fs->locals.nslots++;
    const int index = fs->locals.width;
    fs->locals.width += width;
    fs->locals.slots[islot].index = index;
    fs->locals.slots[islot].symbol = symbol;
    fs->locals.slots[islot].width = width;
    fs->locals.slots[islot].debug = -1;
    return (VarInfo){
        .symbol = symbol,
        .kind = VAR_LOCAL,
        .index = index,
        .width = width,
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
    while (symbol_iter(fs, scope, &symbol)) {
    }
    return add_local(fs, symbol);
}

static VarInfo transfer_global(Generator *G)
{
    const int index = G->iglobal++;
    Symbol *symbol = G->globals->symbols[index];
    AstType *type = a_type(symbol->decl);
    pawE_new_global(env(G->lex), symbol->name, a_type_code(type));
    return (VarInfo){
        .symbol = symbol,
        .kind = VAR_GLOBAL,
        .index = index,
        .width = get_width(G, type),
    };
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
        add_debug_info(fs->G, &locals->slots[level]);
    }
}

static void end_local_scope(FuncState *fs, BlockState *bs)
{
    for (int i = fs->level - 1; i >= bs->level; --i) {
        struct LocalInfo *info = local_info(fs, i);
        fs->locals.width -= info->width;
        paw_assert(info->index == fs->locals.width);
        info->pc1 = fs->pc;
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
    pawK_code_U(fs, OP_RETURN, 1);

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

static void enter_function(Generator *G, FuncState *fs, BlockState *bs,
                           Scope *scope, FuncKind kind)
{
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
    AstType *type = a_type(symbol->decl);
    pinfo->symbol = symbol;
    pinfo->kind = VAR_GLOBAL;
    pinfo->index = index;
    pinfo->width = get_width(G, type);
    return PAW_TRUE;
}

static paw_Bool resolve_local(FuncState *fs, String *name, VarInfo *pinfo)
{
    for (int i = fs->level - 1; i >= 0; --i) {
        LocalSlot slot = fs->locals.slots[i];
        if (pawS_eq(slot.symbol->name, name)) {
            pinfo->symbol = slot.symbol;
            pinfo->kind = VAR_LOCAL;
            pinfo->index = slot.index;
            pinfo->width = slot.width;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static VarInfo find_var(Generator *G, String *name);

static VarInfo resolve_attr(Generator *G, const AstType *type, const String *name)
{
    paw_assert(a_is_adt(type));
    AstDecl *decl = get_decl(G, type->adt.base);
    StructDecl *struct_ = &decl->struct_;
    Scope *scope = struct_->field_scope;
    VarInfo info = (VarInfo){.kind = VAR_FIELD};
    info.index = pawP_find_symbol(scope, name);
    info.width = get_width(G, type);
    paw_assert(info.index >= 0); // found in last pass
    info.symbol = scope->symbols[info.index];
    return info;
}

static void add_upvalue(FuncState *fs, String *name, VarInfo *info,
                        paw_Bool is_local)
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
        caller->proto->v[pinfo->index].is_captured = PAW_TRUE;
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
    return global ? transfer_global(fs->G) : transfer_local(fs);
}

// Allow a previously-declared variable to be accessed
static void define_var(FuncState *fs, VarInfo info)
{
    if (info.kind == VAR_LOCAL) {
        begin_local_scope(fs, 1);
    } else {
        // Write initial value to the globals table. Reference counts are
        // adjusted automatically.
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

static VarInfo inject_var(FuncState *fs, String *name, AstDecl *decl,
                          paw_Bool global)
{
    paw_assert(!global);
    Symbol *symbol = pawA_new_symbol(fs->G->lex);
    symbol->is_init = PAW_TRUE;
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

#define getter_op(G, op, start, width) do { \
        for (int i = 0; i < (width); ++i) { \
            pawK_code_U((G)->fs, op, (start) + i); \
        } \
    } while (0)

// Push a variable on to the stack
static void code_getter(AstVisitor *V, VarInfo info)
{
    Generator *G = V->state.G;
    switch (info.kind) {
        case VAR_LOCAL:
            getter_op(G, OP_GETLOCAL, info.index, info.width);
            break;
        case VAR_UPVALUE:
            getter_op(G, OP_GETUPVALUE, info.index, info.width);
            break;
        case VAR_FIELD:
            getter_op(G, OP_GETATTR, info.index, info.width);
            break;
        default:
            paw_assert(info.kind == VAR_GLOBAL);
            getter_op(G, OP_GETGLOBAL, info.index, info.width);
    }
}

#define setter_op(G, op, start, width) do { \
        for (int i = (width) - 1; i >= 0; --i) { \
            pawK_code_U((G)->fs, op, (start) + i); \
        } \
    } while (0)

static void code_setter(AstVisitor *V, AstExpr *lhs, AstExpr *rhs)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    if (a_kind(lhs) == EXPR_NAME) {
        const VarInfo info = find_var(G, lhs->name.name);
        V->visit_expr(V, rhs);
        switch (info.kind) {
            case VAR_LOCAL:
                setter_op(G, OP_SETLOCAL, info.index, info.width);
                break;
            case VAR_UPVALUE:
                setter_op(G, OP_SETUPVALUE, info.index, info.width);
                break;
            default:
                paw_assert(info.kind == VAR_GLOBAL);
                setter_op(G, OP_SETGLOBAL, info.index, info.width);
        }
        return;
    }

    const SuffixedExpr *suf = &lhs->suffix;
    V->visit_expr(V, suf->target);

    const AstType *target = a_type(suf->target);
    if (a_kind(lhs) == EXPR_SELECTOR) {
        V->visit_expr(V, rhs);

        const String *name = lhs->selector.name;
        const VarInfo info = resolve_attr(G, target, name);
        setter_op(G, OP_SETATTR, info.index, info.width);
    } else {
        paw_assert(a_kind(lhs) == EXPR_INDEX);
        visit_exprs(V, lhs->index.elems);
        V->visit_expr(V, rhs);

        // Indexed assignment is passed the value width as an immediate operand. The 
        // index is specified on the stack, so it is popped after running OP_SETITEM.
        const AstExpr *elem = lhs->index.elems->data[0];
        const paw_Type code = basic_code(target);
        const int width = get_width(G, a_type(elem));
        pawK_code_AB(fs, OP_SETITEM, code, width);
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

static void code_builtin_composite(AstVisitor *V, LiteralExpr *lit, unsigned op)
{
    CompositeLit *e = &lit->comp;
    visit_exprs(V, e->items);

    FuncState *fs = V->state.G->fs;
    pawK_code_U(fs, op, e->items->count);
}

static void code_custom_composite(AstVisitor *V, LiteralExpr *lit)
{
    CompositeLit *e = &lit->comp;
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    const DefId id = a_type_code(lit->type);
    StructDecl *d = &get_decl(G, id)->struct_;
    pawK_code_U(G->fs, OP_PUSHSTRUCT, d->location);
    pawK_code_U(fs, OP_NEWINSTANCE, d->fields->count);

    for (int i = 0; i < e->items->count; ++i) {
        AstExpr *attr = e->items->data[i];
        V->visit_expr(V, attr);
        pawK_code_U(fs, OP_INITFIELD, attr->item.index);
    }
}

static void code_composite_lit(AstVisitor *V, LiteralExpr *lit)
{
    Generator *G = V->state.G;
    String *name = get_type_name(G, a_type(lit->comp.target));
    if (pawS_eq(name, pawE_cstr(env(G->lex), CSTR_VECTOR))) {
        code_builtin_composite(V, lit, OP_NEWVECTOR);
    } else if (pawS_eq(name, pawE_cstr(env(G->lex), CSTR_MAP))) {
        code_builtin_composite(V, lit, OP_NEWVECTOR);
    } else {
        code_custom_composite(V, lit);
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
    const int width = get_width(G, e->type);
    pawK_code_U(G->fs, OP_RETURN, width);
    patch_here(fs, then_jump);
}

#define code_op(fs, op, subop, type)                                           \
    pawK_code_AB(fs, op, cast(subop, int), basic_code(type))

static void code_unop_expr(AstVisitor *V, UnOpExpr *e)
{
    FuncState *fs = V->state.G->fs;

    V->visit_expr(V, e->target);
    code_op(fs, OP_UNOP, e->op, a_type(e->target));
}

static void code_binop_expr(AstVisitor *V, BinOpExpr *e)
{
    FuncState *fs = V->state.G->fs;
    V->visit_expr(V, e->lhs);
    V->visit_expr(V, e->rhs);

    paw_assert(a_type(e->lhs) == a_type(e->rhs));
    code_op(fs, OP_BINOP, e->op, a_type(e->lhs));
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

static void code_func(AstVisitor *V, FuncDecl *d)
{
    Generator *G = V->state.G;
    FuncState fs;
    BlockState bs;
    fs.name = d->name;
    fs.G = G;

    compute_width(G, cast_decl(d));

    const int id = add_proto(G, d, &fs.proto);
    fs.proto->argc = d->param_width;
    enter_function(G, &fs, &bs, d->scope, d->fn_kind);
    visit_decls(V, d->params); // code parameters
    V->visit_block_stmt(V, d->body); // code function body
    leave_function(G);

    code_closure(G->fs, fs.proto, id);
}

// Stamp out monomorphizations of a function template
static void monomorphize_func(AstVisitor *V, FuncDecl *d)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;
    for (int i = 0; i < d->monos->count; ++i) {
        AstDecl *decl = d->monos->data[i];
        FuncDecl *inst = pawA_stencil_func(V->ast, d, decl);
        String *mangled = mangle_name(G, inst->name, inst->type->func.types);
        const VarInfo info = inject_var(fs, mangled, cast_decl(inst), d->is_global);
        code_func(V, inst);
        define_var(fs, info);
    }
    ++fs->bs->isymbol;
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
    Generator *G = V->state.G;
    Lex *lex = G->lex;
    paw_Env *P = env(lex);

    compute_width(G, cast_decl(d));

    Value *pv = pawC_push0(P);
    if (d->is_struct) {
        Struct *struct_ = pawV_new_struct(P, pv);
        d->location = add_struct(G, struct_);
    } else {
        // TODO: Add enum RTTI stuff 
    }
    pawC_pop(P); // pop 'struct_'
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
    if (is_toplevel(G)) {
        pawX_error(lex, "return from module is not allowed");
    }
    if (s->expr != NULL) {
        V->visit_expr(V, s->expr);
        const int width = get_width(G, a_type(s->expr));
        pawK_code_U(G->fs, OP_RETURN, width);
    } else {
        pawK_code_0(G->fs, OP_PUSHUNIT);
        pawK_code_U(G->fs, OP_RETURN, 1);
    }
}

static void code_instance_getter(AstVisitor *V, AstType *type)
{
    Generator *G = V->state.G;
    paw_assert(a_is_func(type));
    AstDecl *decl = get_decl(G, a_type_code(type));
    String *name = decl->hdr.name;
    paw_assert(a_is_func_decl(decl));
    name = mangle_name(G, name, type->func.types);
    const VarInfo info = find_var(G, name);
    code_getter(V, info);
}

struct VariantInfo {
    Scope *fields;
    int choice;
    int width;
    int used;
};

static struct VariantInfo unpack_variant(Generator *G, AstType *type)
{
    AstDecl *decl = get_decl(G, type->func.def);
    AstDecl *base = get_decl(G, type->func.base);
    return (struct VariantInfo){
        .fields = decl->variant.scope,
        .choice = decl->variant.index,
        .width = base->struct_.width,
        .used = decl->variant.used,
    };
}

// Generate code for an enumerator
// Adds padding such that the variant is as wide as the widest variant in
// the enumeration.
static void code_variant_constructor(AstVisitor *V, AstType *type, AstList *args)
{
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    struct VariantInfo info = unpack_variant(G, type);
    visit_exprs(V, args); // field values
    for (int i = info.used; i < info.width - 1; ++i) {
        pawK_code_0(fs, OP_PUSHUNIT); // padding
    }
    const int k = add_constant(G, (Value){.i = info.choice});
    pawK_code_U(fs, OP_PUSHCONST, k); // discriminator
}

static paw_Bool is_instance_call(const AstType *type)
{
    return type->func.types->count > 0;
}

static paw_Bool is_variant_constructor(Generator *G, const AstType *type)
{
    if (type->hdr.def != NO_DECL) {
        const AstDecl *decl = get_decl(G, type->hdr.def); 
        return a_kind(decl) == DECL_VARIANT;
    }
    return PAW_FALSE;
}

static int code_args(AstVisitor *V, AstList *args)
{
    int width = 0;
    Generator *G = V->state.G;
    for (int i = 0; i < args->count; ++i) {
        AstExpr *expr = args->data[i];
        V->visit_expr(V, expr);
        width += get_width(G, a_type(expr));
    }
    return width;
}

static void code_call_expr(AstVisitor *V, CallExpr *e)
{
    paw_assert(a_is_func(e->func));
    Generator *G = V->state.G;
    FuncState *fs = G->fs;

    if (is_variant_constructor(G, e->func)) {
        code_variant_constructor(V, e->func, e->args);
        return; // variant on top of stack
    } else if (is_instance_call(e->func)) {
        code_instance_getter(V, e->func);
    } else {
        V->visit_expr(V, e->target);
    }
    const int width = code_args(V, e->args);
    pawK_code_U(fs, OP_CALL, width);
}

static void code_func_decl(AstVisitor *V, FuncDecl *d)
{
    Generator *G = V->state.G;
    if (d->generics->count == 0) {
        const VarInfo info = declare_var(G->fs, d->is_global);
        code_func(V, d);
        define_var(G->fs, info);
    } else {
        monomorphize_func(V, d);
    }
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

    // Finish the loop. 'break' labels jump here, 'continue' labels back to
    // right before where the conditional expression was evaluated.
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

    // If the condition is false, jump over the instruction that moves control
    // back to the top of the loop.
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

static void code_forin_stmt(
    AstVisitor *V,
    ForStmt *s) // TODO: forin would need to encode the type of object being
                // iterated over. look into function call for loop?
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

// NOTE: This function is not called on the 'target' of a composite literal
static void code_index_expr(AstVisitor *V, Index *e)
{
    Generator *G = V->state.G;
    AstType *target = a_type(e->target);
    if (a_is_func(target)) {
        code_instance_getter(V, e->type);
        return;
    }
    paw_assert(e->elems->count == 1);
    V->visit_expr(V, e->target);
    V->visit_expr(V, e->elems->data[0]);

    const paw_Type code = basic_code(target);
    const int width = get_width(G, target);
    pawK_code_AB(G->fs, OP_GETITEM, code, width);
}

static void code_access_expr(AstVisitor *V, Access *e)
{
    paw_unused(V);
    paw_unused(e);
}

static void code_selector_expr(AstVisitor *V, Selector *e)
{
    Generator *G = V->state.G;
    V->visit_expr(V, e->target);
    const VarInfo info = resolve_attr(G, a_type(e->target), e->name);
    pawK_code_U(G->fs, OP_GETATTR, info.index);
}

static void code_builtin(Generator *G)
{
    paw_Env *P = env(G->lex);
    const int index = G->iglobal++;
    Symbol *symbol = G->globals->symbols[index];
    const AstType *type = a_type(symbol->decl);
    pawE_new_global(env(G->lex), symbol->name, a_type_code(type));

    // Only need to set pointer for functions, since ADT construction is
    // intercepted and handled by special operators.
    if (a_is_func_decl(symbol->decl)) {
        GlobalVar *var = pawE_get_global(env(G->lex), index);
        const Value key = {.p = symbol->name};
        const Value *pvalue = pawH_get(P, P->builtin, key);
        var->value = *pvalue;
    }
    compute_width(G, symbol->decl);
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
    V->visit_call_expr = code_call_expr;
    V->visit_index_expr = code_index_expr;
    V->visit_access_expr = code_access_expr;
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

    AstList *prelude = V->ast->prelude;
    for (int i = 0; i < prelude->count; ++i) {
        code_builtin(V->state.G);
    }
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
        .globals = lex->pm->symbols.globals,
    };
    code_module(&G);
}
