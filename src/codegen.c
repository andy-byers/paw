// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "code.h"
#include "gc_aux.h"
#include "hir.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "type.h"

#define syntax_error(G, ...) pawE_error(ENV(G), PAW_ESYNTAX, -1, __VA_ARGS__)
#define is_global(G) (is_toplevel(G) && (G)->fs->bs->outer == NULL)
#define code_block(V, b) check_exp((b)->kind == kHirBlock, V->VisitStmt(V, cast_stmt(b)))
#define basic_decl(G, code) basic_symbol(G, code)->decl
#define basic_type(G, code) basic_decl(G, code)->type.type
#define get_decl(G, id) pawHir_get_decl((G)->hir, id)
#define get_type(G, id) get_decl(G, id)->hdr.type
#define symbol_type(G, symbol) get_type(G, (symbol)->decl->hdr.def)
#define TYPE2CODE(G, type) (pawP_type2code((G)->C, type))

static void mangle_type(Generator *G, Buffer *buf, struct HirType *type)
{
    paw_Env *P = ENV(G);
    if (HIR_IS_BASIC_T(type)) {
         if (HIR_IS_UNIT_T(type)) {
            pawL_add_literal(P, buf, "0");
        } else if (type->adt.base == PAW_TBOOL) {
            pawL_add_literal(P, buf, "b");
        } else if (type->adt.base == PAW_TINT) {
            pawL_add_literal(P, buf, "i");
        } else if (type->adt.base == PAW_TFLOAT) {
            pawL_add_literal(P, buf, "f");
        } else if (type->adt.base == PAW_TSTRING) {
            pawL_add_literal(P, buf, "s");
        }
    } else if (HirIsGeneric(type)) {
        struct HirGeneric *var = &type->generic;
        pawL_add_nstring(P, buf, var->name->text, var->name->length);
    } else if (HirIsTupleType(type)) {
        struct HirTupleType *tup = &type->tuple;
        pawL_add_char(P, buf, 'T');
        for (int i = 0; i < tup->elems->count; ++i) {
            mangle_type(G, buf, tup->elems->data[i]);
        }
        pawL_add_char(P, buf, '_');
    } else if (HirIsFuncType(type)) {
        pawL_add_char(P, buf, 'F');
        for (int i = 0; i < type->fptr.params->count; ++i) {
            mangle_type(G, buf, type->fptr.params->data[i]);
        }
        pawL_add_char(P, buf, '_');
        mangle_type(G, buf, type->fptr.result);
    } else {
        paw_assert(HirIsAdt(type));
        struct HirAdt *adt = &type->adt;
        if (TYPE2CODE(G, type) == PAW_TVECTOR) {
            pawL_add_literal(P, buf, "(Vector)");
        } else if (TYPE2CODE(G, type) == PAW_TMAP) {
            pawL_add_literal(P, buf, "(Map)");
        } else {
            struct HirDecl *d = get_decl(G, adt->did);
            const String *name = d->adt.name;
            pawL_add_nstring(P, buf, name->text, name->length);
        }
        if (adt->types != NULL) {
            for (int i = 0; i < adt->types->count; ++i) {
                mangle_type(G, buf, adt->types->data[i]);
            }
        }
    }
}

static String *mangle_name(Generator *G, const String *name, struct HirTypeList *binder)
{
    Buffer buf;
    paw_Env *P = ENV(G);
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

static paw_Type basic_code(Generator *G, struct HirType *type)
{
    paw_assert(HirIsAdt(type));
    return TYPE2CODE(G, type);
}

static void push_local_table(struct FuncState *fs, struct HirScope *symbols)
{
    Generator *G = fs->G;
    struct HirSymtab *st = fs->scopes;
    if (st->scopes->count == ITEM_MAX) {
        syntax_error(G, "too many nested scopes");
    }
    pawHir_add_scope(G->hir, st, symbols);
}

static void pop_local_table(struct FuncState *fs)
{
    // Last symbol table should have been assigned to an HIR node. The
    // next call to push_symbol_table() will allocate a new table.
    struct HirSymtab *st = fs->scopes;
    paw_assert(st->scopes->count > 0);
    --st->scopes->count;
}

static int add_constant(Generator *G, Value v)
{
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;

    if (fs->nk == ITEM_MAX) {
        syntax_error(G, "too many constants");
    }
    pawM_grow(ENV(G), p->k, fs->nk, p->nk);
    p->k[fs->nk] = v;
    return fs->nk++;
}

static int add_proto(Generator *G, String *name, Proto **pp)
{
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;
    if (fs->nproto == ITEM_MAX) {
        syntax_error(G, "too many functions");
    }
    pawM_grow(ENV(G), p->p, fs->nproto, p->nproto);
    Proto *callee = pawV_new_proto(ENV(G));
    callee->modname = G->C->modname;
    callee->name = name;

    const int id = fs->nproto++;
    p->p[id] = *pp = callee;
    return id;
}

static paw_Bool needs_close(struct FuncState *fs, const struct BlockState *bs)
{
    for (int i = fs->level - 1; i >= bs->level; --i) {
        if (fs->locals.slots[i].is_captured) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static void close_vars(struct FuncState *fs, const struct BlockState *bs)
{
    if (fs->level > bs->level) {
        const Op op = needs_close(fs, bs) ? OP_CLOSE : OP_POP;
        pawK_code_U(fs, op, fs->level - bs->level);
    }
}

static struct HirVarInfo add_local(struct FuncState *fs, struct HirSymbol *symbol)
{
    Generator *G = fs->G;
    pawM_grow(ENV(G), fs->locals.slots, fs->locals.nslots, fs->locals.capacity);
    const int index = fs->locals.nslots++;
    fs->locals.slots[index].symbol = symbol;
    fs->locals.slots[index].index = index;
    return (struct HirVarInfo){
        .symbol = symbol,
        .kind = VAR_LOCAL,
        .index = index,
    };
}

static paw_Bool symbol_iter(struct HirScope *scope, int *pindex, struct HirSymbol **out)
{
    paw_assert(*pindex < scope->symbols->count);
    *out = scope->symbols->data[*pindex];
    ++*pindex;
    return (*out)->is_type; // skip types
}

static struct HirVarInfo transfer_local(struct FuncState *fs)
{
    struct HirSymbol *symbol;
    // Find the next symbol that belongs on the stack.
    struct HirSymtab *scopes = fs->scopes; // all function scopes
    struct HirScope *scope =
        scopes->scopes->data[scopes->scopes->count - 1]; // last scope
    while (symbol_iter(scope, &fs->bs->isymbol, &symbol)) {
    }
    return add_local(fs, symbol);
}

static struct HirVarInfo transfer_global(Generator *G)
{
    struct HirSymbol *symbol;
    struct HirScope *scope = G->globals;
    while (symbol_iter(scope, &G->iglobal, &symbol)) {
    }
    const int g = pawE_new_global(ENV(G), symbol->name);
    return (struct HirVarInfo){
        .symbol = symbol,
        .kind = VAR_GLOBAL,
        .index = g,
    };
}

#define JUMP_PLACEHOLDER (-1)

static int code_jump(struct FuncState *fs, OpCode op)
{
    pawK_code_S(fs, op, JUMP_PLACEHOLDER);
    return fs->pc - 1;
}

static void patch_jump(struct FuncState *fs, int from, int to)
{
    const int jump = to - (from + 1);
    if (jump > JUMP_MAX) {
        syntax_error(fs->G, "too many instructions to jump");
    }
    Proto *p = fs->proto;
    set_S(&p->source[from], jump);
}

static void patch_here(struct FuncState *fs, int from)
{
    patch_jump(fs, from, fs->pc);
}

static void code_loop(struct FuncState *fs, Op op, int to)
{
    const int jump = to - (fs->pc + 1);
    if (jump > JUMP_MAX) {
        syntax_error(fs->G, "too many instructions in loop");
    }
    pawK_code_S(fs, op, jump);
}

static void code_closure(struct FuncState *fs, Proto *p, int id)
{
    Value v;
    v_set_object(&v, p);
    pawK_code_U(fs, OP_CLOSURE, id);
}

static void add_label(struct FuncState *fs, enum LabelKind kind)
{
    Generator *G = fs->G;
    struct LabelList *ll = &G->C->dm->labels;
    pawM_grow(ENV(G), ll->values, ll->length, ll->capacity);
    ll->values[ll->length] = (struct Label){
        .kind = kind,
        .line = -1, // TODO: Get from HirLabelStmt
        .level = fs->level - fs->bs->level,
        .pc = code_jump(fs, OP_JUMP),
    };
    ++ll->length;
}

static void adjust_labels(struct FuncState *fs, struct BlockState *bs)
{
    Generator *G = fs->G;
    struct LabelList *ll = &G->C->dm->labels;
    for (int i = bs->label0; i < ll->length; ++i) {
        struct Label *lb = &ll->values[i];
        lb->level = bs->level;
    }
}

static void remove_label(struct LabelList *ll, int index)
{
    paw_assert(ll->length > 0);
    for (int i = index; i < ll->length - 1; ++i) {
        ll->values[i] = ll->values[i + 1];
    }
    --ll->length;
}

static void adjust_from(struct FuncState *fs, enum LabelKind kind)
{
    Generator *G = fs->G;
    struct BlockState *bs = fs->bs;
    struct LabelList *ll = &G->C->dm->labels;
    for (int i = bs->label0; i < ll->length;) {
        struct Label *lb = &ll->values[i];
        if (lb->kind == kind) {
            patch_here(fs, lb->pc);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static void adjust_to(struct FuncState *fs, enum LabelKind kind, int to)
{
    Generator *G = fs->G;
    Proto *p = fs->proto;
    struct BlockState *bs = fs->bs;
    struct LabelList *ll = &G->C->dm->labels;
    for (int i = bs->label0; i < ll->length;) {
        struct Label *lb = &ll->values[i];
        if (lb->kind == kind) {
            const int jump = to - (lb->pc + 1);
            set_S(&p->source[lb->pc], jump);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static void begin_local_scope(struct FuncState *fs, int n) 
{
    fs->level += n; 
}

static void end_local_scope(struct FuncState *fs, struct BlockState *bs)
{
    fs->locals.nslots = bs->level;
    fs->level = bs->level;
}

static void leave_block(struct FuncState *fs)
{
    struct BlockState *bs = fs->bs;
    if (bs->is_loop) {
        adjust_from(fs, LBREAK);
    }
    close_vars(fs, bs);
    end_local_scope(fs, bs);
    if (bs->outer) {
        adjust_labels(fs, bs);
    }
    fs->bs = bs->outer;
    pop_local_table(fs);
}

static void enter_block(struct FuncState *fs, struct BlockState *bs, struct HirScope *locals,
                        paw_Bool loop)
{
    bs->label0 = fs->G->C->dm->labels.length;
    bs->level = fs->level;
    bs->isymbol = 0;
    bs->is_loop = loop;
    bs->outer = fs->bs;
    fs->bs = bs;

    push_local_table(fs, locals);
}

static void leave_function(Generator *G)
{
    struct FuncState *fs = G->fs;
    struct BlockState *bs = fs->bs;
    Proto *p = fs->proto;

    // end function-scoped locals
    end_local_scope(fs, bs);
    paw_assert(fs->level == 0);
    paw_assert(bs->outer == NULL);

    // TODO: Need a return at the end to handle cleaning up the stack
    //       Use a landing pad: all returns are just jumps to the landing pad
    pawK_code_0(fs, OP_PUSHUNIT);
    pawK_code_0(fs, OP_RETURN);

    pawM_shrink(ENV(G), p->source, p->length, fs->pc);
    p->length = fs->pc;
    pawM_shrink(ENV(G), p->lines, p->nlines, fs->nlines);
    p->nlines = fs->nlines;
    pawM_shrink(ENV(G), p->p, p->nproto, fs->nproto);
    p->nproto = fs->nproto;
    pawM_shrink(ENV(G), p->u, p->nup, fs->nup);
    p->nup = fs->nup;
    pawM_shrink(ENV(G), p->k, p->nk, fs->nk);
    p->nk = fs->nk;

    G->fs = fs->outer;
    check_gc(ENV(G));
}

static struct HirVarInfo synthesize_var(struct FuncState *fs, String *name)
{
    Generator *G = fs->G;
    struct HirSymbol *symbol = pawHir_new_symbol(G->hir);
    symbol->is_init = PAW_TRUE;
    symbol->name = name;
    return add_local(fs, symbol);
}

static void enter_function(Generator *G, struct FuncState *fs, struct BlockState *bs,
                           struct HirScope *scope, enum FuncKind kind)
{
    fs->bs = NULL;
    fs->scopes = pawHir_new_symtab(G->hir);
    fs->locals = (struct LocalStack){0};
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

    if (kind == FUNC_CLOSURE) {
        synthesize_var(fs, fs->name);
    } else {
        transfer_local(fs);
    }
    begin_local_scope(fs, 1);
}

static paw_Bool resolve_global(Generator *G, const String *name, struct HirVarInfo *pinfo)
{
    const int index = pawE_find_global(ENV(G), name);
    paw_assert(index >= 0);
    pinfo->symbol = NULL;
    pinfo->kind = VAR_GLOBAL;
    pinfo->index = index;
    return PAW_TRUE;
}

static paw_Bool resolve_local(struct FuncState *fs, const String *name, struct HirVarInfo *pinfo)
{
    for (int i = fs->level - 1; i >= 0; --i) {
        struct LocalSlot slot = fs->locals.slots[i];
        if (pawS_eq(slot.symbol->name, name)) {
            pinfo->symbol = slot.symbol;
            pinfo->kind = VAR_LOCAL;
            pinfo->index = i;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static struct HirVarInfo find_var(Generator *G, const String *name);

static struct HirVarInfo resolve_attr(Generator *G, struct HirType *type, String *name)
{
    paw_assert(HirIsAdt(type));
    struct HirDecl *decl = get_decl(G, type->adt.base);
    struct HirAdtDecl *adt = &decl->adt;
    struct HirScope *scope = adt->field_scope;
    struct HirVarInfo info = (struct HirVarInfo){.kind = VAR_FIELD};
    info.index = pawHir_find_symbol(scope, name);
    paw_assert(info.index >= 0); // found in last pass
    info.symbol = scope->symbols->data[info.index];
    return info;
}

static void add_upvalue(struct FuncState *fs, struct HirVarInfo *info, paw_Bool is_local)
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
    }
    pawM_grow(ENV(fs->G), f->u, fs->nup, f->nup);
    f->u[fs->nup] = (struct UpValueInfo){
        .is_local = is_local,
        .index = info->index,
    };
    info->index = fs->nup++;
    info->kind = VAR_UPVALUE;
}

static paw_Bool resolve_upvalue(struct FuncState *fs, const String *name,
                                struct HirVarInfo *pinfo)
{
    struct FuncState *caller = fs->outer;
    if (!caller) {
        return PAW_FALSE;
    }
    if (resolve_local(caller, name, pinfo)) {
        caller->locals.slots[pinfo->index].is_captured = PAW_TRUE;
        add_upvalue(fs, pinfo, PAW_TRUE);
        return PAW_TRUE;
    }
    if (resolve_upvalue(caller, name, pinfo)) {
        add_upvalue(fs, pinfo, PAW_FALSE);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static struct HirVarInfo declare_var(struct FuncState *fs, paw_Bool global)
{
    return global 
        ? transfer_global(fs->G) 
        : transfer_local(fs);
}

// Allow a previously-declared variable to be accessed
static void define_var(struct FuncState *fs, struct HirVarInfo info)
{
    if (info.kind == VAR_LOCAL) {
        begin_local_scope(fs, 1);
    } else {
        // Write initial value to the globals table.
        paw_assert(info.kind == VAR_GLOBAL);
        pawK_code_U(fs, OP_SETGLOBAL, info.index);
    }
}

static struct HirVarInfo new_var(Generator *G, paw_Bool global)
{
    struct FuncState *fs = G->fs;
    struct HirVarInfo info = declare_var(fs, global);
    define_var(fs, info);
    return info;
}

static struct HirVarInfo find_var(Generator *G, const String *name)
{
    struct HirVarInfo info;
    struct FuncState *fs = G->fs;
    if (!resolve_local(fs, name, &info) && // not local
            !resolve_upvalue(fs, name, &info) && // not local to caller
            !resolve_global(G, name, &info)) { // not found
        pawE_error(ENV(G), PAW_ENAME, -1, "undefined variable '%s'", name->text);
    }
    return info;
}

#define code_op(fs, op, subop, type) \
    pawK_code_AB(fs, op, cast(subop, int), basic_code((fs)->G, type))

// TODO: OP_PUSHFALSE is a hack to avoid creating unnecessary constants,
// essentially pushes integer 0
//       we would otherwise have to create a new constant for integer 0, else do
//       it at the beginning and stash it somewhere. Need to cannonicalize
//       constants, otherwise we end up with a huge amount of redundancy
static void code_slice_indices(struct HirVisitor *V, struct HirExpr *first, struct HirExpr *second,
                               struct HirType *target)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    if (first != NULL) {
        V->VisitExpr(V, first);
    } else {
        // default to the start of the sequence
        pawK_code_0(fs, OP_PUSHFALSE);
    }
    if (second != NULL) {
        V->VisitExpr(V, second);
    } else {
        // default to the end of the sequence
        pawK_code_U(fs, OP_COPY, 1); // copy sequence
        code_op(fs, OP_UNOP, UNARY_LEN, target);
    }
}

// Push a variable on to the stack
static void code_getter(struct HirVisitor *V, struct HirVarInfo info)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
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
        default:
            paw_assert(info.kind == VAR_GLOBAL);
            pawK_code_U(fs, OP_GETGLOBAL, info.index);
    }
}

static struct HirVarInfo resolve_short_path(Generator *G, struct HirPath *path)
{
    paw_assert(path->count == 1);
    struct HirSegment *ident = pawHir_path_get(path, 0);
    const String *name = ident->name;
    if (HirIsFuncDef(ident->type) && ident->types != NULL) {
        name = mangle_name(G, name, ident->type->fdef.types);
    }
    return find_var(G, name);
}

static void code_setter(struct HirVisitor *V, struct HirExpr *lhs, struct HirExpr *rhs)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
    if (HirIsPathExpr(lhs)) {
        const struct HirVarInfo info = resolve_short_path(G, lhs->path.path);
        V->VisitExpr(V, rhs);
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

    const struct HirSuffixedExpr *suf = &lhs->suffix;
    V->VisitExpr(V, suf->target);

    if (HirIsSelector(lhs)) {
        V->VisitExpr(V, rhs);
        String *name = lhs->select.name;
        struct HirType *target = HIR_TYPEOF(suf->target);
        if (lhs->select.is_index) {
            pawK_code_U(fs, OP_SETTUPLE, lhs->select.index);
        } else {
            const struct HirVarInfo info = resolve_attr(G, target, name);
            pawK_code_U(fs, OP_SETATTR, info.index);
        }
    } else {
        paw_assert(HirIsIndex(lhs));
        const struct HirIndex *index = &lhs->index;
        struct HirType *target = HIR_TYPEOF(index->target);
        if (index->is_slice) {
            code_slice_indices(V, index->first, index->second, target);
            V->VisitExpr(V, rhs);
            pawK_code_U(fs, OP_SETSLICE, basic_code(G, target));
        } else {
            V->VisitExpr(V, lhs->index.first);
            V->VisitExpr(V, rhs);
            pawK_code_U(fs, OP_SETITEM, basic_code(G, target));
        }
    }
}

static void code_basic_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
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

static void code_tuple_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct HirTupleLit *lit = &e->tuple;
    V->VisitExprList(V, lit->elems);

    struct FuncState *fs = V->state.G->fs;
    pawK_code_U(fs, OP_NEWTUPLE, lit->elems->count);
}

static void code_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct HirContainerLit *lit = &e->cont;
    V->VisitExprList(V, lit->items);

    struct FuncState *fs = V->state.G->fs;
    const Op op = lit->code == PAW_TVECTOR ? OP_NEWVECTOR : OP_NEWMAP;
    pawK_code_U(fs, op, lit->items->count);
}

static void code_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    paw_assert(e->lit_kind == kHirLitComposite);
    struct HirCompositeLit *lit = &e->comp;
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    const DefId did = e->type->adt.did;
    struct HirAdtDecl *d = &get_decl(G, did)->adt;
    pawK_code_U(fs, OP_NEWINSTANCE, d->fields->count);

    for (int i = 0; i < lit->items->count; ++i) {
        struct HirExpr *attr = lit->items->data[i];
        V->VisitExpr(V, attr);
        pawK_code_U(fs, OP_INITFIELD, attr->sitem.index);
    }
}

static void code_literal_expr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitBasic:
            code_basic_lit(V, e);
            break;
        case kHirLitTuple:
            code_tuple_lit(V, e);
            break;
        case kHirLitContainer:
            code_container_lit(V, e);
            break;
        default:
            code_composite_lit(V, e);
    }
}

static void code_and(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    V->VisitExpr(V, e->lhs);
    const int jump = code_jump(fs, OP_JUMPFALSE);
    pawK_code_U(fs, OP_POP, 1);
    V->VisitExpr(V, e->rhs);
    patch_here(fs, jump);
}

static void code_or(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    V->VisitExpr(V, e->lhs);
    const int else_jump = code_jump(fs, OP_JUMPFALSE);
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    pawK_code_U(fs, OP_POP, 1);
    V->VisitExpr(V, e->rhs);
    patch_here(fs, then_jump);
}

static void code_logical_expr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    if (e->is_and) {
        code_and(V, e);
    } else {
        code_or(V, e);
    }
}

static void code_chain_expr(struct HirVisitor *V, struct HirChainExpr *e)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    V->VisitExpr(V, e->target);
    const int jump = code_jump(fs, OP_JUMPNULL);
    pawK_code_0(fs, OP_RETURN);
    patch_here(fs, jump);
}

static void code_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct FuncState *fs = V->state.G->fs;

    V->VisitExpr(V, e->target);
    code_op(fs, OP_UNOP, e->op, HIR_TYPEOF(e->target));
}

static void code_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct FuncState *fs = V->state.G->fs;
    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);
    code_op(fs, OP_BINOP, e->op, HIR_TYPEOF(e->rhs));
}

static void code_decl_stmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    V->VisitDecl(V, s->decl);
}

static void code_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    struct FuncState *fs = V->state.G->fs;

    if (s->rhs != NULL) {
        code_setter(V, s->lhs, s->rhs);
        return;
    }
    V->VisitExpr(V, s->lhs); // function call
    pawK_code_U(fs, OP_POP, 1); // unused return value
}

static void code_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct FuncState fs;
    struct BlockState bs;
    Generator *G = V->state.G;
    fs.name = SCAN_STRING(G->C, "(closure)");
    fs.G = G;

    const int id = add_proto(G, fs.name, &fs.proto);
    fs.proto->argc = e->params->count;
    enter_function(G, &fs, &bs, e->scope, FUNC_CLOSURE);
    V->VisitDeclList(V, e->params);
    V->VisitBlock(V, e->body);
    leave_function(G);

    code_closure(G->fs, fs.proto, id);
}

static void code_func(struct HirVisitor *V, struct HirFuncDecl *d)
{
    Generator *G = V->state.G;
    struct FuncState fs;
    struct BlockState bs;
    fs.name = d->name;
    fs.G = G;

    struct HirFuncDef *func = &d->type->fdef;
    const int id = add_proto(G, d->name, &fs.proto);
    fs.proto->argc = func->params->count;
    enter_function(G, &fs, &bs, d->scope, d->fn_kind);
    V->VisitDeclList(V, d->params); // code parameters
    V->VisitBlock(V, d->body); // code function body
    leave_function(G);

    code_closure(G->fs, fs.proto, id);
}

static struct HirVarInfo inject_var(struct FuncState *fs, String *name, struct HirDecl *decl, paw_Bool global)
{
    paw_assert(!global);
    struct HirSymbol *symbol = pawHir_new_symbol(fs->G->hir);
    symbol->is_init = PAW_TRUE;
    symbol->name = name;
    symbol->decl = decl;
    return add_local(fs, symbol);
}

// Stamp out monomorphizations of a function template
static void monomorphize_func(struct HirVisitor *V, struct HirFuncDecl *d)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
    if (d->monos == NULL) {
        return;
    }
    for (int i = 0; i < d->monos->count; ++i) {
        struct HirDecl *decl = d->monos->data[i];
        struct HirFuncDecl *inst = &decl->func;
        struct HirFuncDef *fdef = HirGetFuncDef(inst->type);
        String *mangled = mangle_name(G, inst->name, fdef->types);
        const struct HirVarInfo info = inject_var(fs, mangled, decl, d->is_global);
        code_func(V, inst);
        define_var(fs, info);
    }
    ++fs->bs->isymbol;
}

static void code_field_decl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    new_var(V->state.G, PAW_FALSE);
    paw_unused(d);
}

static void code_var_decl(struct HirVisitor *V, struct HirVarDecl *s)
{
    struct FuncState *fs = V->state.G->fs;
    const struct HirVarInfo info = declare_var(fs, s->is_global);
    V->VisitExpr(V, s->init);
    define_var(fs, info);
}

static void code_adt_decl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    // NOOP
    paw_unused(V);
    paw_unused(d);
}

static void code_block_stmt(struct HirVisitor *V, struct HirBlock *b)
{
    struct BlockState bs;
    struct FuncState *fs = V->state.G->fs;
    enter_block(fs, &bs, b->scope, PAW_FALSE);
    V->VisitStmtList(V, b->stmts);
    leave_block(fs);
}

static void code_return_stmt(struct HirVisitor *V, struct HirReturnStmt *s)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
    if (is_toplevel(G)) {
        pawE_error(ENV(G), PAW_ESYNTAX, -1, "return from module is not allowed");
    }
    V->VisitExpr(V, s->expr);
    pawK_code_0(fs, OP_RETURN);
}

static void code_instance_getter(struct HirVisitor *V, struct HirType *type)
{
    Generator *G = V->state.G;
    paw_assert(HirIsFuncType(type));
    struct HirDecl *decl = get_decl(G, type->fdef.did);
    String *name = decl->hdr.name;
    if (!pawS_eq(name, SCAN_STRING(G->C, "_vector_push")) &&
            !pawS_eq(name, SCAN_STRING(G->C, "_vector_pop")) &&
            !pawS_eq(name, SCAN_STRING(G->C, "_vector_insert")) &&
            !pawS_eq(name, SCAN_STRING(G->C, "_vector_erase")) &&
            !pawS_eq(name, SCAN_STRING(G->C, "_vector_clone"))) {
        // TODO: These functions are native. They use the same code for all
        // instantiations (they
        //       only move parameters around as 'union Value')
        paw_assert(HirIsFuncDecl(decl));
        name = mangle_name(G, name, type->fdef.types);
    }
    const struct HirVarInfo info = find_var(G, name);
    code_getter(V, info);
}

struct VariantInfo {
    struct HirScope *fields;
    int choice;
};

static struct VariantInfo unpack_variant(Generator *G, struct HirType *type)
{
    struct HirDecl *decl = get_decl(G, type->fdef.did);
    return (struct VariantInfo){
        .fields = decl->variant.scope,
        .choice = decl->variant.index,
    };
}

static paw_Bool is_instance_call(const struct HirType *type)
{
    return HirIsFuncDef(type) && type->fdef.types != NULL;
}

static paw_Bool is_variant_constructor(Generator *G, const struct HirType *type)
{
    if (HirIsFuncDef(type)) {
        const struct HirDecl *decl = get_decl(G, type->fdef.did);
        return HirIsVariantDecl(decl);
    }
    return PAW_FALSE;
}

// Generate code for an enumerator
static void code_variant_constructor(struct HirVisitor *V, struct HirType *type,
                                     struct HirExprList *args)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    int count = 0;
    struct VariantInfo info = unpack_variant(G, type);
    if (args != NULL) {
        V->VisitExprList(V, args);
        count = args->count;
        paw_assert(count > 0);
    }
    pawK_code_AB(fs, OP_NEWVARIANT, info.choice, count);
}

static void code_path_expr(struct HirVisitor *V, struct HirPathExpr *e)
{
    Generator *G = V->state.G;
    if (is_variant_constructor(G, e->type)) {
        code_variant_constructor(V, e->type, NULL);
    } else {
        const struct HirVarInfo info = resolve_short_path(G, e->path);
        code_getter(V, info);
    }
}

static void code_call_expr(struct HirVisitor *V, struct HirCallExpr *e)
{
    paw_assert(HirIsFuncType(e->func));
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    if (is_variant_constructor(G, e->func)) {
        code_variant_constructor(V, e->func, e->args);
        return;
    } else if (is_instance_call(e->func)) {
        code_instance_getter(V, e->func);
    } else {
        V->VisitExpr(V, e->target);
    }
    V->VisitExprList(V, e->args);
    pawK_code_U(fs, OP_CALL, e->args->count);
}

static void code_conversion_expr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    Generator *G = V->state.G;
    const struct HirType *from = HIR_TYPEOF(e->arg);
    const Op op = e->to == PAW_TBOOL  ? OP_CASTBOOL
                  : e->to == PAW_TINT ? OP_CASTINT
                                      : OP_CASTFLOAT;

    V->VisitExpr(V, e->arg);
    pawK_code_U(G->fs, op, from->adt.base);
}

static void code_func_decl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    struct FuncState *fs = V->state.G->fs;
    if (d->generics == NULL) {
        const struct HirVarInfo info = declare_var(fs, d->is_global);
        code_func(V, d);
        define_var(fs, info);
    } else {
        monomorphize_func(V, d);
    }
}

static void code_if_stmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    V->VisitExpr(V, s->cond);
    const int else_jump = code_jump(fs, OP_JUMPFALSEPOP);
    V->VisitStmt(V, s->then_arm);
    // NOTE: If there is no 'else' block, this will produce a NOOP jump.
    const int then_jump = code_jump(fs, OP_JUMP);
    patch_here(fs, else_jump);
    V->VisitStmt(V, s->else_arm);
    patch_here(fs, then_jump);
}

static void close_until_loop(struct FuncState *fs)
{
    struct BlockState *bs = fs->bs;
    while (bs->outer) {
        // Emit close/pop instructions, but don't end any lifetimes. Code
        // paths that doesn't hit this label may still need those locals.
        struct BlockState *outer = bs->outer;
        if (outer->is_loop) {
            close_vars(fs, bs);
            return;
        }
        bs = outer;
    }
    pawE_error(ENV(fs->G), PAW_ESYNTAX, -1, "label outside loop");
}

static void code_label_stmt(struct HirVisitor *V, struct HirLabelStmt *s)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
    close_until_loop(fs); // fix the stack
    add_label(fs, s->label); // emit a jump, to be patched later
}

static void code_dowhile_stmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
    struct BlockState bs;

    enter_block(fs, &bs, s->scope, PAW_TRUE);
    const int loop = fs->pc;
    V->VisitBlock(V, s->block);
    adjust_from(fs, LCONTINUE);
    V->VisitExpr(V, s->cond);

    // If the condition is false, jump over the instruction that moves control
    // back to the top of the loop.
    const int jump = code_jump(fs, OP_JUMPFALSEPOP);
    code_loop(fs, OP_JUMP, loop);
    patch_here(fs, jump);
    leave_block(fs);
}

static void code_while_stmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    if (s->is_dowhile) {
        code_dowhile_stmt(V, s);
        return;
    }
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
    struct BlockState bs;

    enter_block(fs, &bs, s->scope, PAW_TRUE);
    const int loop = fs->pc;
    V->VisitExpr(V, s->cond);

    const int jump = code_jump(fs, OP_JUMPFALSEPOP);
    V->VisitBlock(V, s->block);

    // Finish the loop. 'break' labels jump here, 'continue' labels back to
    // right before where the conditional expression was evaluated.
    code_loop(fs, OP_JUMP, loop);
    adjust_to(fs, LCONTINUE, loop);
    patch_here(fs, jump);
    leave_block(fs);
}

static void code_forbody(struct HirVisitor *V, struct HirBlock *block, Op opinit, Op oploop)
{
    struct BlockState bs;
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;

    // Emit OP_FOR*0, which either pushes the loop variables, or jumps over
    // the loop.
    const int jump = code_jump(fs, opinit);
    const int loop = fs->pc;

    // Put the control variable in the same scope as any locals declared inside
    // the loop. If the control variable is captured in a closure, the upvalue
    // must be closed at the end of the iteration.
    enter_block(fs, &bs, block->scope, PAW_FALSE);
    new_var(G, PAW_FALSE);
    V->VisitStmtList(V, block->stmts);
    leave_block(fs);

    // Continue statements jump here, right before the loop instruction.
    adjust_from(fs, LCONTINUE);
    code_loop(fs, oploop, loop);
    patch_here(fs, jump);
}

static void code_fornum_stmt(struct HirVisitor *V, struct HirForStmt *s)
{
    Generator *G = V->state.G;
    struct HirForNum *fornum = &s->fornum;

    V->VisitExpr(V, fornum->begin);
    V->VisitExpr(V, fornum->end);
    V->VisitExpr(V, fornum->step);
    new_var(G, PAW_FALSE);
    new_var(G, PAW_FALSE);
    new_var(G, PAW_FALSE);

    code_forbody(V, s->block, OP_FORNUM0, OP_FORNUM);
}

static void code_forin_stmt(struct HirVisitor *V, struct HirForStmt *s)
{
    Generator *G = V->state.G;
    struct HirForIn *forin = &s->forin;

    V->VisitExpr(V, forin->target);
    new_var(G, PAW_FALSE);
    new_var(G, PAW_FALSE);

    struct HirType *t = HIR_TYPEOF(forin->target);
    const Op init = TYPE2CODE(G, t) == PAW_TVECTOR ? OP_FORVECTOR0 : OP_FORMAP0;
    const Op loop = TYPE2CODE(G, t) == PAW_TVECTOR ? OP_FORVECTOR : OP_FORMAP;
    code_forbody(V, s->block, init, loop);
}

static void code_for_stmt(struct HirVisitor *V, struct HirForStmt *s)
{
    struct BlockState bs;
    Generator *G = V->state.G;
    struct FuncState *fs = G->fs;
    enter_block(fs, &bs, s->scope, PAW_TRUE);
    if (s->is_fornum) {
        code_fornum_stmt(V, s);
    } else {
        code_forin_stmt(V, s);
    }
    leave_block(fs);
}

static void code_index_expr(struct HirVisitor *V, struct HirIndex *e)
{
    Generator *G = V->state.G;
    struct HirType *target = HIR_TYPEOF(e->target);
    const paw_Type t = basic_code(G, target);
    V->VisitExpr(V, e->target);
    if (e->is_slice) {
        code_slice_indices(V, e->first, e->second, target);
        pawK_code_U(G->fs, OP_GETSLICE, t);
    } else {
        V->VisitExpr(V, e->first);
        pawK_code_U(G->fs, OP_GETITEM, t);
    }
}

static void code_selector_expr(struct HirVisitor *V, struct HirSelector *e)
{
    Generator *G = V->state.G;
    V->VisitExpr(V, e->target);
    if (e->is_index) {
        pawK_code_U(G->fs, OP_GETTUPLE, e->index);
    } else {
        const struct HirVarInfo info = resolve_attr(G, HIR_TYPEOF(e->target), e->name);
        pawK_code_U(G->fs, OP_GETATTR, info.index);
    }
}

static void add_builtin_func(Generator *G, const char *name)
{
    paw_Env *P = ENV(G);
    String *str = SCAN_STRING(G->C, name);
    const int g = pawE_new_global(P, str);
    GlobalVar *gv = pawE_get_global(P, g);
    const Value key = {.o = cast_object(str)};
    const Value *pv = pawH_get_(P->builtin, key);
    gv->value = *pv;
}

static void setup_pass(struct HirVisitor *V, Generator *G)
{
    const union HirState state = {.G = G};
    pawHir_visitor_init(V, G->hir, state);
    V->VisitLiteralExpr = code_literal_expr;
    V->VisitLogicalExpr = code_logical_expr;
    V->VisitChainExpr = code_chain_expr;
    V->VisitUnOpExpr = code_unop_expr;
    V->VisitBinOpExpr = code_binop_expr;
    V->VisitConversionExpr = code_conversion_expr;
    V->VisitPathExpr = code_path_expr;
    V->VisitCallExpr = code_call_expr;
    V->VisitIndex = code_index_expr;
    V->VisitSelector = code_selector_expr;
    V->VisitClosureExpr = code_closure_expr;
    V->VisitBlock = code_block_stmt;
    V->VisitExprStmt = code_expr_stmt;
    V->VisitDeclStmt = code_decl_stmt;
    V->VisitIfStmt = code_if_stmt;
    V->VisitForStmt = code_for_stmt;
    V->VisitWhileStmt = code_while_stmt;
    V->VisitLabelStmt = code_label_stmt;
    V->VisitReturnStmt = code_return_stmt;
    V->VisitVarDecl = code_var_decl;
    V->VisitFuncDecl = code_func_decl;
    V->VisitAdtDecl = code_adt_decl;
    V->VisitFieldDecl = code_field_decl;

//    struct HirStmtList *prelude = V->hir->prelude;
//    while (G->iglobal < prelude->count) {
////    for (int i = 0; i < prelude->count; ++i) {
//        code_builtin(V->state.G);
//    }

//    struct HirStmtList *prelude = V->hir->prelude;
//    pawHir_stencil_stmts(G->hir, prelude);
//    V->VisitStmtList(V, prelude);

    add_builtin_func(G, "assert");
    add_builtin_func(G, "print");

    add_builtin_func(G, "_vector_push");
    add_builtin_func(G, "_vector_pop");
    add_builtin_func(G, "_vector_insert");
    add_builtin_func(G, "_vector_erase");
    add_builtin_func(G, "_vector_clone");
}

static void code_module(Generator *G)
{
    struct Hir *hir = G->hir;

    struct FuncState fs;
    struct BlockState bs;
    fs.name = G->C->modname;
    fs.proto = G->C->main->p;

    struct HirScope *toplevel = hir->symtab->toplevel;
    enter_function(G, &fs, &bs, toplevel, FUNC_MODULE);

    struct HirVisitor V;
    setup_pass(&V, G);
    pawHir_stencil_stmts(G->hir, hir->stmts);
    V.VisitStmtList(&V, hir->stmts);

    leave_function(G);
}
#include "debug.h"
void p_codegen(struct Compiler *C, struct Hir *hir)
{
    struct Generator G = {
        .hir = hir,
        .sym = hir->symtab,
        .globals = hir->symtab->globals,
        .P = ENV(C),
        .C = C,
    };
    code_module(&G);
}
