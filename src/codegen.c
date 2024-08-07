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

#define SYNTAX_ERROR(G, ...) pawE_error(ENV(G), PAW_ESYNTAX, -1, __VA_ARGS__)
#define GET_DECL(G, id) pawHir_get_decl((G)->hir, id)
#define TYPE2CODE(G, type) (pawP_type2code((G)->C, type))

struct ItemSlot {
    struct HirVarInfo info;
    struct HirDecl *decl;
};

static struct ItemSlot *new_item_slot(struct Generator *G, struct HirDecl *decl, struct HirVarInfo info)
{
    struct ItemSlot *slot = pawK_pool_alloc(ENV(G), &G->hir->pool, sizeof(struct ItemSlot));
    *slot = (struct ItemSlot){
        .decl = decl,
        .info = info,
    };
    return slot;
}

DEFINE_LIST(struct Hir, item_list_, ToplevelList, struct ItemSlot)

static void mangle_type(struct Generator *G, Buffer *buf, struct HirType *type)
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
            struct HirDecl *d = GET_DECL(G, adt->did);
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

static String *mangle_name(struct Generator *G, const String *name, struct HirTypeList *binder)
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

static paw_Type basic_code(struct Generator *G, struct HirType *type)
{
    paw_assert(HirIsAdt(type));
    return TYPE2CODE(G, type);
}

static int add_constant(struct Generator *G, Value v)
{
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;

    if (fs->nk == ITEM_MAX) {
        SYNTAX_ERROR(G, "too many constants");
    }
    pawM_grow(ENV(G), p->k, fs->nk, p->nk);
    p->k[fs->nk] = v;
    return fs->nk++;
}

static int add_proto(struct Generator *G, Proto *proto)
{
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;
    if (fs->nproto == ITEM_MAX) {
        SYNTAX_ERROR(G, "too many functions");
    }
    pawM_grow(ENV(G), p->p, fs->nproto, p->nproto);
    const int id = fs->nproto++;
    p->p[id] = proto;
    return id;
}

static Proto *push_proto(struct Generator *G, String *name)
{
    paw_Env *P = ENV(G);
    Value *pv = pawC_push0(P);
    Proto *proto = pawV_new_proto(P);
    v_set_object(pv, proto);
    proto->modname = G->C->modname;
    proto->name = name;
    return proto;
}

static void pop_proto(struct Generator *G)
{
    paw_assert(o_is_proto(ENV(G)->top.p[-1].o));
    pawC_pop(ENV(G));
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

static struct HirVarInfo declare_local(struct FuncState *fs, String *name, struct HirType *type)
{
    struct Generator *G = fs->G;
    pawM_grow(ENV(G), fs->locals.slots, fs->locals.nslots, fs->locals.capacity);
    const int index = fs->locals.nslots++;
    fs->locals.slots[index] = (struct LocalSlot){
        .type = type,
        .name = name,
        .index = index,
    };
    return (struct HirVarInfo){
        .kind = VAR_LOCAL,
        .index = index,
    };
}

static struct HirVarInfo new_global(struct Generator *G, String *name, struct HirType *type, paw_Bool is_pub)
{
    paw_unused(is_pub); // TODO: public vs private items
    paw_unused(type); // TODO: type for use by C API (if public)
    const int g = pawE_new_global(ENV(G), name);
    return (struct HirVarInfo){
        .kind = VAR_GLOBAL,
        .index = g,
    };
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

static void define_local(struct FuncState *fs)
{
    begin_local_scope(fs, 1);
}

static struct HirVarInfo new_local(struct FuncState *fs, String *name, struct HirType *type)
{
     const struct HirVarInfo info = declare_local(fs, name, type); 
     define_local(fs);
     return info;
}

static struct HirVarInfo new_local_lit(struct Generator *G, const char *name, paw_Type code)
{
    String *str = SCAN_STRING(G->C, name);
    struct HirDecl *decl = GET_DECL(G, code);
    return new_local(G->fs, str, HIR_TYPEOF(decl));
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
        SYNTAX_ERROR(fs->G, "too many instructions to jump");
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
        SYNTAX_ERROR(fs->G, "too many instructions in loop");
    }
    pawK_code_S(fs, op, jump);
}

static void code_closure(struct Generator *G, int id)
{
    pawK_code_U(G->fs, OP_CLOSURE, id);
}

static void add_label(struct FuncState *fs, enum LabelKind kind)
{
    struct Generator *G = fs->G;
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
    struct Generator *G = fs->G;
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
    struct Generator *G = fs->G;
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
    struct Generator *G = fs->G;
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
}

static void enter_block(struct FuncState *fs, struct BlockState *bs, paw_Bool loop)
{
    bs->label0 = fs->G->C->dm->labels.length;
    bs->level = fs->level;
    bs->is_loop = loop;
    bs->outer = fs->bs;
    fs->bs = bs;
}

static void leave_function(struct Generator *G)
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

static void enter_function(struct Generator *G, struct FuncState *fs, struct BlockState *bs,
                           struct HirType *type, enum FuncKind kind)
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
    enter_block(fs, bs, PAW_FALSE);

    new_local(fs, fs->name, type);
}

static paw_Bool resolve_global(struct Generator *G, const String *name, struct HirVarInfo *pinfo)
{
    // TODO: Use a struct LocalStack to keep track of all globals
    const int index = pawE_find_global(ENV(G), name);
    paw_assert(index >= 0);
    pinfo->kind = VAR_GLOBAL;
    pinfo->index = index;
    return PAW_TRUE;
}

static paw_Bool resolve_local(struct FuncState *fs, const String *name, struct HirVarInfo *pinfo)
{
    for (int i = fs->level - 1; i >= 0; --i) {
        struct LocalSlot slot = fs->locals.slots[i];
        if (pawS_eq(slot.name, name)) {
            pinfo->kind = VAR_LOCAL;
            pinfo->index = i;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static struct HirVarInfo find_var(struct Generator *G, const String *name);

static struct HirVarInfo resolve_attr(struct Generator *G, struct HirType *type, String *name)
{
    paw_assert(HirIsAdt(type));
    struct HirDecl *decl = GET_DECL(G, type->adt.base);
    struct HirAdtDecl *adt = &decl->adt;
    struct HirScope *scope = adt->scope;
    const int index = pawHir_find_symbol(scope, name);
    paw_assert(index >= 0); // already found
    return (struct HirVarInfo){
        .kind = VAR_FIELD,
        .index = index,
    };
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
        SYNTAX_ERROR(fs->G, "too many upvalues");
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

static struct HirVarInfo find_var(struct Generator *G, const String *name)
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

#define CODE_OP(fs, op, subop, type) \
    pawK_code_AB(fs, op, cast(subop, int), basic_code((fs)->G, type))

// TODO: OP_PUSHFALSE is a hack to avoid creating unnecessary constants,
// essentially pushes integer 0
//       we would otherwise have to create a new constant for integer 0, else do
//       it at the beginning and stash it somewhere. Need to cannonicalize
//       constants, otherwise we end up with a huge amount of redundancy
static void code_slice_indices(struct HirVisitor *V, struct HirExpr *first, struct HirExpr *second,
                               struct HirType *target)
{
    struct Generator *G = V->ud;
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
        CODE_OP(fs, OP_UNOP, UNARY_LEN, target);
    }
}

// Push a variable on to the stack
static void code_getter(struct HirVisitor *V, struct HirVarInfo info)
{
    struct Generator *G = V->ud;
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

static struct HirVarInfo resolve_short_path(struct Generator *G, struct HirPath *path)
{
    paw_assert(path->count == 1);
    struct HirSegment *ident = pawHir_path_get(path, 0);
    const String *name = ident->name;
    if (HirIsFuncDef(ident->type) && ident->types != NULL) {
        name = mangle_name(G, name, ident->type->fdef.types);
    } else if (HirIsAdt(ident->type) && ident->types != NULL) {
        name = mangle_name(G, name, ident->type->adt.types);
    }
    return find_var(G, name);
}

static void code_setter(struct HirVisitor *V, struct HirExpr *lhs, struct HirExpr *rhs)
{
    struct Generator *G = V->ud;
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
    struct Generator *G = V->ud;
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

    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    pawK_code_U(fs, OP_NEWTUPLE, lit->elems->count);
}

static void code_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct HirContainerLit *lit = &e->cont;
    V->VisitExprList(V, lit->items);

    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    const Op op = lit->code == PAW_TVECTOR ? OP_NEWVECTOR : OP_NEWMAP;
    pawK_code_U(fs, op, lit->items->count);
}

static void code_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    paw_assert(e->lit_kind == kHirLitComposite);
    struct HirCompositeLit *lit = &e->comp;
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    const DefId did = e->type->adt.did;
    struct HirAdtDecl *d = &GET_DECL(G, did)->adt;
    pawK_code_U(fs, OP_NEWINSTANCE, d->fields ? d->fields->count : 0);

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
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    V->VisitExpr(V, e->lhs);
    const int jump = code_jump(fs, OP_JUMPFALSE);
    pawK_code_U(fs, OP_POP, 1);
    V->VisitExpr(V, e->rhs);
    patch_here(fs, jump);
}

static void code_or(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    struct Generator *G = V->ud;
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
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    V->VisitExpr(V, e->target);
    const int jump = code_jump(fs, OP_JUMPNULL);
    pawK_code_0(fs, OP_RETURN);
    patch_here(fs, jump);
}

static void code_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    V->VisitExpr(V, e->target);
    CODE_OP(fs, OP_UNOP, e->op, HIR_TYPEOF(e->target));
}

static void code_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);
    CODE_OP(fs, OP_BINOP, e->op, HIR_TYPEOF(e->rhs));
}

static void code_decl_stmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    V->VisitDecl(V, s->decl);
}

static void code_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

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
    struct Generator *G = V->ud;
    fs.name = SCAN_STRING(G->C, "(closure)");
    fs.G = G;

    fs.proto = push_proto(G, fs.name);
    fs.proto->argc = e->params->count;
    enter_function(G, &fs, &bs, e->type, FUNC_CLOSURE);
    V->VisitDeclList(V, e->params);
    if (e->has_body) {
        V->VisitBlock(V, e->body);
    } else {
        V->VisitExpr(V, e->expr);
        pawK_code_0(G->fs, OP_RETURN);
    }
    leave_function(G);

    const int pid = add_proto(G, fs.proto);
    code_closure(G, pid);
    pop_proto(G);
}

static void set_entrypoint(struct Generator *G, Proto *proto, int g)
{
    paw_Env *P = ENV(G);
    struct GlobalVar *var = pawE_get_global(P, g);

    Closure *closure = pawV_new_closure(P, 0);
    v_set_object(&var->value, closure);
    closure->p = proto;
}

static void code_func(struct HirVisitor *V, struct HirFuncDecl *d, struct HirVarInfo info)
{
    struct Generator *G = V->ud;
    struct FuncState fs;
    struct BlockState bs;
    fs.name = d->name;
    fs.G = G;

    struct HirFuncDef *func = &d->type->fdef;
    fs.proto = push_proto(G, d->name);
    fs.proto->argc = func->params->count;
    enter_function(G, &fs, &bs, d->type, d->fn_kind);
    V->VisitDeclList(V, d->params); // code parameters
    V->VisitBlock(V, d->body); // code function body
    leave_function(G);

    if (info.kind == VAR_GLOBAL) {
        set_entrypoint(G, fs.proto, info.index); 
    } else {
        const int pid = add_proto(G, fs.proto);
        code_closure(G, pid);
    }
    pop_proto(G);
}

static void code_field_decl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    struct Generator *G = V->ud;
    new_local(G->fs, d->name, d->type);
}

static void code_var_decl(struct HirVisitor *V, struct HirVarDecl *d)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    declare_local(fs, d->name, d->type);
    V->VisitExpr(V, d->init);
    define_local(fs);
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
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    enter_block(fs, &bs, PAW_FALSE);
    V->VisitStmtList(V, b->stmts);
    leave_block(fs);
}

static void code_return_stmt(struct HirVisitor *V, struct HirReturnStmt *s)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    V->VisitExpr(V, s->expr);
    pawK_code_0(fs, OP_RETURN);
}

static void code_instance_getter(struct HirVisitor *V, struct HirType *type)
{
    struct Generator *G = V->ud;
    paw_assert(HirIsFuncType(type));
    struct HirDecl *decl = GET_DECL(G, type->fdef.did);
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
      //  name = mangle_name(G, name, type->fdef.types);
    }
    const struct HirVarInfo info = find_var(G, name);
    code_getter(V, info);
}

struct VariantInfo {
    struct HirScope *fields;
    int choice;
};

static struct VariantInfo unpack_variant(struct Generator *G, struct HirType *type)
{
    struct HirDecl *decl = GET_DECL(G, type->fdef.did);
    return (struct VariantInfo){
        .fields = decl->variant.scope,
        .choice = decl->variant.index,
    };
}

static paw_Bool is_instance_call(const struct HirType *type)
{
    return HirIsFuncDef(type) && type->fdef.types != NULL;
}

static paw_Bool is_variant_constructor(struct Generator *G, const struct HirType *type)
{
    if (HirIsFuncDef(type)) {
        const struct HirDecl *decl = GET_DECL(G, type->fdef.did);
        return HirIsVariantDecl(decl);
    }
    return PAW_FALSE;
}

// Generate code for an enumerator
static void code_variant_constructor(struct HirVisitor *V, struct HirType *type,
                                     struct HirExprList *args)
{
    struct Generator *G = V->ud;
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
    struct Generator *G = V->ud;
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
    struct Generator *G = V->ud;
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
    struct Generator *G = V->ud;
    const struct HirType *from = HIR_TYPEOF(e->arg);
    const Op op = e->to == PAW_TBOOL ? OP_CASTBOOL
                : e->to == PAW_TINT  ? OP_CASTINT
                                     : OP_CASTFLOAT;

    V->VisitExpr(V, e->arg);
    pawK_code_U(G->fs, op, from->adt.base);
}

// Stamp out monomorphizations of a function template
static void monomorphize_func(struct HirVisitor *V, struct HirFuncDecl *d)
{
    if (d->monos == NULL) {
        return;
    }
    struct Generator *G = V->ud;
    for (int i = 0; i < d->monos->count; ++i) {
        struct HirDecl *decl = d->monos->data[i];
        struct HirFuncDecl *inst = HirGetFuncDecl(decl);
        struct HirFuncDef *fdef = HirGetFuncDef(inst->type);
        inst->name = mangle_name(G, inst->name, fdef->types);
        const struct HirVarInfo info = new_global(G, inst->name, inst->type, d->is_pub);
        code_func(V, inst, info);
    }
}

static void register_func(struct Generator *G, struct HirFuncDecl *d)
{
    const struct HirVarInfo info = new_global(G, d->name, d->type, d->is_pub);
    struct ItemSlot *slot = new_item_slot(G, HIR_CAST_DECL(d), info);
    item_list_push(G->hir, G->items, slot);
}

static void register_func_monos(struct Generator *G, struct HirFuncDecl *d)
{
    const int n = d->monos ? d->monos->count : 0;
    for (int i = 0; i < n; ++i) {
        struct HirDecl *decl = d->monos->data[i];
        struct HirFuncDecl *inst = HirGetFuncDecl(decl);
        struct HirFuncDef *fdef = HirGetFuncDef(inst->type);
        inst->name = mangle_name(G, inst->name, fdef->types);
        register_func(G, inst);
    }
}

static void register_func_decl(struct Generator *G, struct HirFuncDecl *d)
{
    if (d->generics == NULL) {
        register_func(G, d);
    } else {
        register_func_monos(G, d);
    }
}

static void register_items(struct Generator *G, struct HirDeclList *items)
{
    for (int i = 0; i < items->count; ++i) {
        struct HirDecl *item = items->data[i];
        if (HirIsFuncDecl(item)) {
            register_func_decl(G, HirGetFuncDecl(item));
        }
    }
}

static void code_items(struct HirVisitor *V)
{
    struct Generator *G = V->ud;
    struct ToplevelList *items = G->items;
    for (int i = 0; i < items->count; ++i) {
        struct ItemSlot *item = items->data[i];
        if (HirIsFuncDecl(item->decl)) {
            code_func(V, HirGetFuncDecl(item->decl), item->info);
        }
    }
}

static void code_func_decl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    struct Generator *G = V->ud;
    if (d->generics == NULL) {
        const struct HirVarInfo info = new_global(G, d->name, d->type, d->is_pub);
        code_func(V, d, info);
    } else {
        monomorphize_func(V, d);
    }
}

static void code_if_stmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    struct Generator *G = V->ud;
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
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    close_until_loop(fs); // fix the stack
    add_label(fs, s->label); // emit a jump, to be patched later
}

static void code_dowhile_stmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    struct BlockState bs;

    enter_block(fs, &bs, PAW_TRUE);
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
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    struct BlockState bs;

    enter_block(fs, &bs, PAW_TRUE);
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

static void code_forbody(struct HirVisitor *V, struct HirDecl *control, struct HirBlock *block, Op opinit, Op oploop)
{
    struct BlockState bs;
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    // Emit OP_FOR*0, which either pushes the loop variables, or jumps over
    // the loop.
    const int jump = code_jump(fs, opinit);
    const int loop = fs->pc;

    // Put the control variable in the same scope as any locals declared inside
    // the loop. If the control variable is captured in a closure, the upvalue
    // must be closed at the end of the iteration.
    const struct HirVarDecl *var = HirGetVarDecl(control);
    enter_block(fs, &bs, PAW_FALSE);
    new_local(fs, var->name, var->type);
    V->VisitStmtList(V, block->stmts);
    leave_block(fs);

    // Continue statements jump here, right before the loop instruction.
    adjust_from(fs, LCONTINUE);
    code_loop(fs, oploop, loop);
    patch_here(fs, jump);
}

static void code_fornum_stmt(struct HirVisitor *V, struct HirForStmt *s)
{
    struct Generator *G = V->ud;
    struct HirForNum *fornum = &s->fornum;

    V->VisitExpr(V, fornum->begin);
    V->VisitExpr(V, fornum->end);
    V->VisitExpr(V, fornum->step);
    new_local_lit(G, "(for begin)", PAW_TINT);
    new_local_lit(G, "(for end)", PAW_TINT);
    new_local_lit(G, "(for step)", PAW_TINT);

    code_forbody(V, s->control, s->block, OP_FORNUM0, OP_FORNUM);
}

static void code_forin_stmt(struct HirVisitor *V, struct HirForStmt *s)
{
    struct Generator *G = V->ud;
    struct HirForIn *forin = &s->forin;

    V->VisitExpr(V, forin->target);
    struct HirType *target = HIR_TYPEOF(forin->target);
    new_local_lit(G, "(for target)", basic_code(G, target));
    new_local_lit(G, "(for iter)", PAW_TINT);

    struct HirType *t = HIR_TYPEOF(forin->target);
    const Op init = TYPE2CODE(G, t) == PAW_TVECTOR ? OP_FORVECTOR0 : OP_FORMAP0;
    const Op loop = TYPE2CODE(G, t) == PAW_TVECTOR ? OP_FORVECTOR : OP_FORMAP;
    code_forbody(V, s->control, s->block, init, loop);
}

static void code_for_stmt(struct HirVisitor *V, struct HirForStmt *s)
{
    struct BlockState bs;
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    enter_block(fs, &bs, PAW_TRUE);
    if (s->is_fornum) {
        code_fornum_stmt(V, s);
    } else {
        code_forin_stmt(V, s);
    }
    leave_block(fs);
}

static void code_index_expr(struct HirVisitor *V, struct HirIndex *e)
{
    struct Generator *G = V->ud;
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
    struct Generator *G = V->ud;
    V->VisitExpr(V, e->target);
    if (e->is_index) {
        pawK_code_U(G->fs, OP_GETTUPLE, e->index);
    } else {
        const struct HirVarInfo info = resolve_attr(G, HIR_TYPEOF(e->target), e->name);
        pawK_code_U(G->fs, OP_GETATTR, info.index);
    }
}

static void add_builtin_func(struct Generator *G, const char *name)
{
    paw_Env *P = ENV(G);
    String *str = SCAN_STRING(G->C, name);
    const int g = pawE_new_global(P, str);
    GlobalVar *gv = pawE_get_global(P, g);
    const Value key = {.o = cast_object(str)};
    const Value *pv = pawH_get_(P->builtin, key);
    gv->value = *pv;
}

static void setup_pass(struct HirVisitor *V, struct Generator *G)
{
    pawHir_visitor_init(V, G->hir, G);
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

    add_builtin_func(G, "assert");
    add_builtin_func(G, "print");

    add_builtin_func(G, "_vector_push");
    add_builtin_func(G, "_vector_pop");
    add_builtin_func(G, "_vector_insert");
    add_builtin_func(G, "_vector_erase");
    add_builtin_func(G, "_vector_clone");
}

static void code_module(struct Generator *G)
{
    struct Hir *hir = G->hir;

    struct FuncState fs;
    struct BlockState bs;
    fs.name = G->C->modname;
    fs.proto = G->C->main->p;

    enter_function(G, &fs, &bs, NULL, FUNC_MODULE);

    struct HirVisitor V;
    setup_pass(&V, G);

    register_items(G, hir->items);
    code_items(&V);

    leave_function(G);
}

void p_codegen(struct Compiler *C, struct Hir *hir)
{
    struct Generator G = {
        .hir = hir,
        .sym = hir->symtab,
        .globals = hir->symtab->globals,
        .items = item_list_new(hir),
        .P = ENV(C),
        .C = C,
    };
    code_module(&G);
}
