// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

// TODO: (1) Allow builtin functions to be specialized for different types. Right now,
//           we always treat generic parameters as union Value and never access the
//           type-specific representation. Right now, we use lack of a function body
//           or presence in the builtins map to indicate that a function is a C function 
//           or not. Use flags in the AST and HIR nodes to store this info instead.

#include "auxlib.h"
#include "code.h"
#include "compile.h"
#include "gc.h"
#include "hir.h"
#include "map.h"
#include "mem.h"
#include "parse.h"

#define SYNTAX_ERROR(G, ...) pawE_error(ENV(G), PAW_ESYNTAX, (G)->fs->line, __VA_ARGS__)
#define GET_DECL(G, id) pawHir_get_decl((G)->hir, id)
#define TYPE_CODE(G, type) (pawP_type2code((G)->C, type))

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

static struct Def *get_def(struct Generator *G, DefId did)
{
    paw_assert(did < ENV(G)->defs.count);
    return ENV(G)->defs.data[did];
}

static void mangle_type(struct Generator *G, Buffer *buf, struct HirType *type)
{
    paw_Env *P = ENV(G);
    if (HIR_IS_BASIC_T(type)) {
         if (HIR_IS_UNIT_T(type)) {
            L_ADD_LITERAL(P, buf, "0");
        } else if (type->adt.base == PAW_TBOOL) {
            L_ADD_LITERAL(P, buf, "b");
        } else if (type->adt.base == PAW_TINT) {
            L_ADD_LITERAL(P, buf, "i");
        } else if (type->adt.base == PAW_TFLOAT) {
            L_ADD_LITERAL(P, buf, "f");
        } else if (type->adt.base == PAW_TSTR) {
            L_ADD_LITERAL(P, buf, "s");
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
        struct HirDecl *d = GET_DECL(G, adt->did);
        const String *name = d->adt.name;
        pawL_add_nstring(P, buf, name->text, name->length);
        if (adt->types != NULL) {
            for (int i = 0; i < adt->types->count; ++i) {
                mangle_type(G, buf, adt->types->data[i]);
            }
        }
    }
}

static String *mangle_name(struct Generator *G, const String *name, struct HirTypeList *types)
{
    Buffer buf;
    paw_Env *P = ENV(G);
    pawL_init_buffer(P, &buf);
    pawL_add_nstring(P, &buf, name->text, name->length);
    const int ntypes = types != NULL ? types->count : 0;
    for (int i = 0; i < ntypes; ++i) {
        mangle_type(G, &buf, types->data[i]);
    }
    pawL_add_char(P, &buf, '_');
    pawL_push_result(P, &buf);
    // anchor in compiler string table
    String *result = V_STRING(P->top.p[-1]);
    const Value key = {.o = CAST_OBJECT(result)};
    pawH_insert(P, G->C->strings, key, key);
    pawC_pop(P);
    return result;
}

static paw_Type basic_code(struct Generator *G, struct HirType *type)
{
    paw_assert(HirIsAdt(type));
    return TYPE_CODE(G, type);
}

static Map *kcache_map(struct FuncState *fs, paw_Type code)
{
    if (code == PAW_TINT) {
        return fs->kcache.ints; 
    } else if (code == PAW_TFLOAT) {
        return fs->kcache.flts; 
    } else {
        paw_assert(code == PAW_TSTR);
        return fs->kcache.strs; 
    }
}

static int add_constant(struct Generator *G, Value v, paw_Type code)
{
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;

    // ensure uniqueness of constant values per function
    Map *kmap = kcache_map(fs, code);
    Value *pk = pawH_get(kmap, v);
    if (pk != NULL) return CAST(pk->i, int);

    if (fs->nk == ITEM_MAX) {
        SYNTAX_ERROR(G, "too many constants");
    }
    pawM_grow(ENV(G), p->k, fs->nk, p->nk);
    p->k[fs->nk] = v;

    const Value id = {.i = fs->nk};
    pawH_insert(ENV(G), kmap, v, id);
    return fs->nk++;
}

static int add_proto(struct Generator *G, Proto *proto)
{
    struct FuncState *fs = G->fs;
    Proto *p = fs->proto;
    if (fs->nproto == ITEM_MAX) {
        SYNTAX_ERROR(G, "too many functions");
    } else if (fs->nproto == p->nproto) {
        pawM_grow(ENV(G), p->p, fs->nproto, p->nproto);
        for (int i = fs->nproto; i < p->nproto; ++i) {
            p->p[i] = NULL;
        }
    }
    const int id = fs->nproto++;
    p->p[id] = proto;
    return id;
}

static Proto *push_proto(struct Generator *G, String *name)
{
    paw_Env *P = ENV(G);
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
    pawC_pop(ENV(G));
}

static struct LocalSlot *get_local_slot(struct FuncState *fs, int index)
{
    return &fs->G->C->dm->vars.data[fs->first_local + index];
}

static paw_Bool needs_close(struct FuncState *fs, const struct BlockState *bs)
{
    struct DynamicMem *dm = fs->G->C->dm;
    for (int i = fs->nlocals - 1; i >= bs->level; --i) {
        const struct LocalSlot *slot = get_local_slot(fs, i);
        if (slot->is_captured) return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void close_vars(struct FuncState *fs, const struct BlockState *bs)
{
    if (fs->nlocals > bs->level) {
        const Op op = needs_close(fs, bs) ? OP_CLOSE : OP_POP;
        pawK_code_U(fs, op, fs->nlocals - bs->level);
    }
}

static struct HirVarInfo declare_local(struct FuncState *fs, String *name, struct HirType *type)
{
    struct Generator *G = fs->G;
    struct DynamicMem *dm = G->C->dm;
    pawM_grow(ENV(G), dm->vars.data, dm->vars.count, dm->vars.alloc);
    const int index = dm->vars.count++;
    dm->vars.data[index] = (struct LocalSlot){
        .type = type,
        .name = name,
        .index = index,
    };
    return (struct HirVarInfo){
        .kind = VAR_LOCAL,
        .index = index,
    };
}

static void begin_local_scope(struct FuncState *fs, int n) 
{
    fs->nlocals += n; 
}

static void end_local_scope(struct FuncState *fs, struct BlockState *bs)
{
    struct DynamicMem *dm = fs->G->C->dm;
    dm->vars.count = fs->first_local + bs->level;
    fs->nlocals = bs->level;
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
    SET_S(&p->source[from], jump);
}

static void patch_here(struct FuncState *fs, int from)
{
    patch_jump(fs, from, fs->pc);
}

static void code_loop(struct FuncState *fs, Op op, int to)
{
    const int jump = to - (fs->pc + 1);
    if (jump < -JUMP_MAX) {
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
        .level = fs->nlocals - fs->bs->level,
        .pc = code_jump(fs, OP_JUMP),
        .line = fs->line,
        .kind = kind,
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
            SET_S(&p->source[lb->pc], jump);
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
    bs->level = fs->nlocals;
    bs->is_loop = loop;
    bs->outer = fs->bs;
    fs->bs = bs;
}

static void enter_kcache(struct FuncState *fs)
{
    paw_Env *P = ENV(fs->G);
    Value *ints = pawC_push0(P);
    Value *strs = pawC_push0(P);
    Value *flts = pawC_push0(P);
    fs->kcache.ints = pawH_new(P);
    V_SET_OBJECT(ints, fs->kcache.ints);
    fs->kcache.strs = pawH_new(P);
    V_SET_OBJECT(strs, fs->kcache.strs);
    fs->kcache.flts = pawH_new(P);
    V_SET_OBJECT(flts, fs->kcache.flts);
}

static void leave_kcache(struct FuncState *fs)
{
    paw_Env *P = ENV(fs->G);
    pawC_stkdec(P, 3);
}

static void leave_function(struct Generator *G)
{
    paw_Env *P = ENV(G);
    struct FuncState *fs = G->fs;
    struct BlockState *bs = fs->bs;
    Proto *p = fs->proto;

    // end function-scoped locals
    end_local_scope(fs, bs);
    paw_assert(fs->nlocals == 0);
    paw_assert(bs->outer == NULL);

    // TODO: Need a return at the end to handle cleaning up the stack
    //       Use a landing pad: all returns are just jumps to the landing pad
    pawK_code_0(fs, OP_PUSHZERO);
    pawK_code_0(fs, OP_RETURN);

    pawM_shrink(P, p->source, p->length, fs->pc);
    p->length = fs->pc;
    pawM_shrink(P, p->lines, p->nlines, fs->nlines);
    p->nlines = fs->nlines;
    pawM_shrink(P, p->p, p->nproto, fs->nproto);
    p->nproto = fs->nproto;
    pawM_shrink(P, p->u, p->nup, fs->nup);
    p->nup = fs->nup;
    pawM_shrink(P, p->k, p->nk, fs->nk);
    p->nk = fs->nk;

    leave_kcache(fs);

    G->fs = fs->outer;
    CHECK_GC(P);
}

static void enter_function(struct Generator *G, struct FuncState *fs, struct BlockState *bs,
                           String *name, Proto *proto, struct HirType *type, enum FuncKind kind)
{
    *fs = (struct FuncState){
        .first_local = G->C->dm->vars.count,
        .scopes = pawHir_new_symtab(G->hir),
        .proto = proto,
        .outer = G->fs,
        .name = name,
        .kind = kind,
        .G = G,
    };
    G->fs = fs;

    enter_kcache(fs);

    // enter the function body
    enter_block(fs, bs, PAW_FALSE);
    new_local(fs, fs->name, type);
}

static paw_Bool resolve_global(struct Generator *G, const String *name, struct HirVarInfo *pinfo)
{
    paw_Env *P = ENV(G);
    const int did = pawE_locate(P, name);
    paw_assert(did >= 0);
    const struct Def *def = pawE_get_def(P, did);
    paw_assert(def->hdr.kind == DEF_FUNC);
    pinfo->kind = VAR_GLOBAL;
    pinfo->index = def->func.vid;
    return PAW_TRUE;
}

static paw_Bool resolve_local(struct FuncState *fs, const String *name, struct HirVarInfo *pinfo)
{
    struct DynamicMem *dm = fs->G->C->dm;
    for (int i = fs->nlocals - 1; i >= 0; --i) {
        struct LocalSlot *slot = get_local_slot(fs, i);
        if (pawS_eq(slot->name, name)) {
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

static paw_Bool resolve_upvalue(struct FuncState *fs, const String *name, struct HirVarInfo *pinfo)
{
    struct DynamicMem *dm = fs->G->C->dm;
    struct FuncState *caller = fs->outer;
    if (caller == NULL) return PAW_FALSE;
    if (resolve_local(caller, name, pinfo)) {
        get_local_slot(caller, pinfo->index)
            ->is_captured = PAW_TRUE;
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
        pawE_error(ENV(G), PAW_ENAME, fs->line, "undefined variable '%s'", name->text);
    }
    return info;
}

#define CODE_OP(fs, op, subop) pawK_code_U(fs, op, CAST(subop, int))

// Get the length of the container object on top of the stack
static void code_len(struct FuncState *fs, paw_Type type);

static void code_slice_indices(struct HirVisitor *V, struct HirExpr *first, 
                               struct HirExpr *second, struct HirType *target)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;

    if (first != NULL) {
        V->VisitExpr(V, first);
    } else {
        // default to the start of the sequence
        pawK_code_0(fs, OP_PUSHZERO);
    }
    if (second != NULL) {
        V->VisitExpr(V, second);
    } else {
        // default to the end of the sequence
        pawK_code_U(fs, OP_COPY, 1);
        code_len(fs, TYPE_CODE(G, target));
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
            pawK_code_U(fs, OP_GETFIELD, info.index);
            break;
        default:
            paw_assert(info.kind == VAR_GLOBAL);
            pawK_code_U(fs, OP_GETGLOBAL, info.index);
    }
}

static paw_Bool should_mangle(struct HirDecl *decl)
{
    paw_assert(!HirIsGenericDecl(decl));
    return !HirIsVarDecl(decl) && !HirIsFieldDecl(decl);
}

static struct HirVarInfo resolve_short_path(struct Generator *G, struct HirPath *path)
{
    paw_assert(path->count == 1);
    struct HirSegment *base = pawHir_path_get(path, 0);
    const String *name = should_mangle(base->result)
        ? mangle_name(G, base->name, base->types)
        : base->name;
    return find_var(G, name);
}

static void code_set(struct FuncState *fs, paw_Type type)
{
    if (type == BUILTIN_LIST) {
        CODE_OP(fs, OP_LISTOP, LIST_SET); 
    } else {
        paw_assert(type == BUILTIN_MAP);
        CODE_OP(fs, OP_MAPOP, MAP_SET); 
    }
}

static void code_setn(struct FuncState *fs, paw_Type type)
{
    paw_assert(type == BUILTIN_LIST);
    CODE_OP(fs, OP_LISTOP, LIST_SETN); 
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
            default:
                paw_assert(info.kind == VAR_UPVALUE);
                pawK_code_U(fs, OP_SETUPVALUE, info.index);
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
            pawK_code_U(fs, OP_SETFIELD, lhs->select.index);
        } else {
            const struct HirVarInfo info = resolve_attr(G, target, name);
            pawK_code_U(fs, OP_SETFIELD, info.index);
        }
    } else {
        paw_assert(HirIsIndex(lhs));
        const struct HirIndex *index = &lhs->index;
        struct HirType *target = HIR_TYPEOF(index->target);
        if (index->is_slice) {
            code_slice_indices(V, index->first, index->second, target);
            V->VisitExpr(V, rhs);
            code_setn(fs, basic_code(G, target));
        } else {
            V->VisitExpr(V, lhs->index.first);
            V->VisitExpr(V, rhs);
            code_set(fs, basic_code(G, target));
        }
    }
}

static int code_if_small(struct FuncState *fs, paw_Int i)
{
    if (i == 0) {
        pawK_code_0(fs, OP_PUSHZERO);
    } else if (i == 1) {
        pawK_code_0(fs, OP_PUSHONE);
    } else if ((i < 0 && i >= -S_MAX) || 
            (i > 0 && i <= S_MAX)) {
        pawK_code_S(fs, OP_PUSHSMI, i);
    } else {
        return -1;
    }
    return 0;
}

static void code_basic_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    const paw_Type type = e->basic.t != PAW_TUNIT && e->basic.t != PAW_TBOOL 
        ? e->basic.t : PAW_TINT;
    if (type != PAW_TINT || code_if_small(fs, e->basic.value.i)) {
        const int k = add_constant(G, e->basic.value, e->basic.t);
        pawK_code_U(fs, OP_PUSHCONST, k);
    } 
}

static void code_tuple_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    struct HirTupleLit *lit = &e->tuple;
    V->VisitExprList(V, lit->elems);

    pawK_code_U(fs, OP_NEWTUPLE, lit->elems->count);
}

static void code_container_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    struct HirContainerLit *lit = &e->cont;
    V->VisitExprList(V, lit->items);

    const Op op = lit->code == BUILTIN_LIST ? OP_NEWLIST : OP_NEWMAP;
    pawK_code_U(fs, op, lit->items->count);
}

static void code_composite_lit(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    paw_assert(e->lit_kind == kHirLitComposite);
    struct HirCompositeLit *lit = &e->comp;
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    const DefId did = e->type->adt.did;
    struct HirAdtDecl *d = &GET_DECL(G, did)->adt;
    pawK_code_U(fs, OP_NEWINSTANCE, d->fields ? d->fields->count : 0);

    for (int i = 0; i < lit->items->count; ++i) {
        struct HirExpr *field = lit->items->data[i];
        V->VisitExpr(V, field);
        pawK_code_U(fs, OP_INITFIELD, field->field.fid);
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

static void code_and(struct HirVisitor *V, struct FuncState *fs, struct HirLogicalExpr *e)
{
    V->VisitExpr(V, e->lhs);
    const int jump = code_jump(fs, OP_JUMPFALSE);
    pawK_code_U(fs, OP_POP, 1);
    V->VisitExpr(V, e->rhs);
    patch_here(fs, jump);
}

static void code_or(struct HirVisitor *V, struct FuncState *fs, struct HirLogicalExpr *e)
{
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
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    if (e->is_and) {
        code_and(V, fs, e);
    } else {
        code_or(V, fs, e);
    }
}

static void code_chain_expr(struct HirVisitor *V, struct HirChainExpr *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    V->VisitExpr(V, e->target);
    const int jump = code_jump(fs, OP_JUMPNULL);
    pawK_code_0(fs, OP_RETURN);
    patch_here(fs, jump);
}

static void code_len(struct FuncState *fs, paw_Type type)
{
    if (type == PAW_TSTR) {
        CODE_OP(fs, OP_STROP, STR_LEN); 
    } else if (type == BUILTIN_LIST) {
        CODE_OP(fs, OP_LISTOP, LIST_LEN); 
    } else {
        paw_assert(BUILTIN_MAP);
        CODE_OP(fs, OP_MAPOP, MAP_LEN); 
    }
}

static void code_arith1(struct FuncState *fs, enum ArithOp1 op, paw_Type type)
{
    const int base = type == PAW_TINT ? OP_ARITHI1 : OP_ARITHF1;
    CODE_OP(fs, base, op);
}

static void code_boolop(struct FuncState *fs, enum BoolOp op)
{
    CODE_OP(fs, OP_BOOLOP, op);
}

static void code_bitw1(struct FuncState *fs, enum BitwOp1 op)
{
    CODE_OP(fs, OP_BITW1, op);
}

static void code_unop_expr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    V->VisitExpr(V, e->target);

    struct HirType *target = HIR_TYPEOF(e->target);
    const paw_Type type = TYPE_CODE(G, target);
    switch (e->op) {
        case UNARY_LEN:
            code_len(fs, type);
            break;
        case UNARY_NEG:
            code_arith1(fs, ARITH1_NEG, type);
            break;
        case UNARY_NOT:
            code_boolop(fs, BOOL_NOT);
            break;
        case UNARY_BNOT:
            code_bitw1(fs, BITW1_NOT);
            break;
        case NUNARYOPS:
            PAW_UNREACHABLE();
    }
}

static void code_cmp(struct FuncState *fs, enum CmpOp op, paw_Type type)
{
    const int base = type == PAW_TFLOAT ? OP_CMPF : 
        type == PAW_TSTR ? OP_CMPS : OP_CMPI;
    CODE_OP(fs, base, op);
}

static void code_arith2(struct FuncState *fs, enum ArithOp2 op, paw_Type type)
{
    const int base = type == PAW_TINT ? OP_ARITHI2 : OP_ARITHF2;
    CODE_OP(fs, base, op);
}

static void code_bitw2(struct FuncState *fs, enum BitwOp2 op)
{
    CODE_OP(fs, OP_BITW2, op);
}

static void code_binop_expr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);

    struct HirType *target = HIR_TYPEOF(e->lhs);
    const paw_Type type = TYPE_CODE(G, target);
    switch (e->op) {
        case BINARY_EQ:   
            code_cmp(fs, CMP_EQ, type);
            break;
        case BINARY_NE:   
            code_cmp(fs, CMP_NE, type);
            break;
        case BINARY_LT:   
            code_cmp(fs, CMP_LT, type);
            break;
        case BINARY_LE:   
            code_cmp(fs, CMP_LE, type);
            break;
        case BINARY_GT:   
            code_cmp(fs, CMP_GT, type);
            break;
        case BINARY_GE:
            code_cmp(fs, CMP_GE, type);
            break;
        case BINARY_ADD:  
            if (type == PAW_TSTR) {
                CODE_OP(fs, OP_STROP, STR_CONCAT);
            } else if (type == BUILTIN_LIST) {
                CODE_OP(fs, OP_LISTOP, LIST_CONCAT);
            } else {
                code_arith2(fs, ARITH2_ADD, type);
            }
            break;
        case BINARY_SUB:  
            code_arith2(fs, ARITH2_SUB, type);
            break;
        case BINARY_MUL:  
            code_arith2(fs, ARITH2_MUL, type);
            break;
        case BINARY_DIV:  
            code_arith2(fs, ARITH2_DIV, type);
            break;
        case BINARY_MOD:  
            code_arith2(fs, ARITH2_MOD, type);
            break;
        case BINARY_BXOR:
            code_bitw2(fs, BITW2_XOR);
            break;
        case BINARY_BAND:
            code_bitw2(fs, BITW2_AND);
            break;
        case BINARY_BOR:
            code_bitw2(fs, BITW2_OR);
            break;
        case BINARY_SHL:
            code_bitw2(fs, BITW2_SHL);
            break;
        case BINARY_SHR:
            code_bitw2(fs, BITW2_SHR);
            break;
        case BINARY_AS:
        case NBINARYOPS:
            PAW_UNREACHABLE();
    }
}

static void code_decl_stmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    V->VisitDecl(V, s->decl);
}

static void code_assign_expr(struct HirVisitor *V, struct HirAssignExpr *s)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = s->line;
    code_setter(V, s->lhs, s->rhs);
}

static void code_expr_stmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = s->line;

    V->VisitExpr(V, s->expr);
    pawK_code_U(fs, OP_POP, 1);
}

static void code_closure_expr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct FuncState fs;
    struct BlockState bs;
    struct Generator *G = V->ud;
    String *name = SCAN_STRING(G->C, "(closure)");
    Proto *proto = push_proto(G, name);
    proto->argc = e->params->count;

    enter_function(G, &fs, &bs, name, proto, e->type, FUNC_CLOSURE);
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

// Wrap a Proto in a Closure so it can be called directly from C
static void set_entrypoint(struct Generator *G, Proto *proto, int g)
{
    paw_Env *P = ENV(G);
    Value *pval = pawE_get_val(P, g);

    Closure *closure = pawV_new_closure(P, 0);
    V_SET_OBJECT(pval, closure);
    closure->p = proto;
}

static void code_func(struct HirVisitor *V, struct HirFuncDecl *d, struct HirVarInfo info)
{
    struct FuncState fs;
    struct BlockState bs;
    struct Generator *G = V->ud;
    struct HirFuncDef *func = &d->type->fdef;
    Proto *proto = push_proto(G, d->name);
    proto->argc = func->params->count;

    enter_function(G, &fs, &bs, d->name, proto, d->type, d->fn_kind);
    V->VisitDeclList(V, d->params);
    if (d->body != NULL) V->VisitBlock(V, d->body);
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
    G->fs->line = d->line;

    new_local(G->fs, d->name, d->type);
}

static void code_var_decl(struct HirVisitor *V, struct HirVarDecl *d)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = d->line;

    declare_local(fs, d->name, d->type);
    V->VisitExpr(V, d->init);
    define_local(fs);
}

static void code_block_stmt(struct HirVisitor *V, struct HirBlock *b)
{
    struct BlockState bs;
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = b->line;

    enter_block(fs, &bs, PAW_FALSE);
    V->VisitStmtList(V, b->stmts);
    leave_block(fs);
}

static void code_return_stmt(struct HirVisitor *V, struct HirReturnStmt *s)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = s->line;

    V->VisitExpr(V, s->expr);
    pawK_code_0(fs, OP_RETURN);
}

static void code_instance_getter(struct HirVisitor *V, struct HirType *type)
{
    struct Generator *G = V->ud;
    paw_assert(HirIsFuncType(type));
    struct HirDecl *decl = GET_DECL(G, type->fdef.did);
    paw_Env *P = ENV(G);

    const String *name = decl->hdr.name;
    const Value k = {.o = CAST_OBJECT(name)};
    const Value *pv = pawH_get(P->builtin, k);
    name = mangle_name(G, name, pv ? NULL : type->fdef.types);

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
static void code_variant_constructor(struct HirVisitor *V, struct HirType *type, struct HirExprList *args)
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
    G->fs->line = e->line;

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
    fs->line = e->line;

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
    G->fs->line = e->line;

    V->VisitExpr(V, e->arg);
    pawK_code_U(G->fs, op, from->adt.base);
}

static void code_if_stmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = s->line;

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
    pawE_error(ENV(fs->G), PAW_ESYNTAX, fs->line, "label outside loop");
}

static void code_label_stmt(struct HirVisitor *V, struct HirLabelStmt *s)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = s->line;

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
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    struct BlockState bs;
    fs->line = s->line;

    if (s->is_dowhile) {
        code_dowhile_stmt(V, s);
        return;
    }
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
    const Op init = TYPE_CODE(G, t) == BUILTIN_LIST ? OP_FORLIST0 : OP_FORMAP0;
    const Op loop = TYPE_CODE(G, t) == BUILTIN_LIST ? OP_FORLIST : OP_FORMAP;
    code_forbody(V, s->control, s->block, init, loop);
}

static void code_for_stmt(struct HirVisitor *V, struct HirForStmt *s)
{
    struct BlockState bs;
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = s->line;

    enter_block(fs, &bs, PAW_TRUE);
    if (s->is_fornum) {
        code_fornum_stmt(V, s);
    } else {
        code_forin_stmt(V, s);
    }
    leave_block(fs);
}

static void code_get(struct FuncState *fs, paw_Type type)
{
    if (type == PAW_TSTR) {
        CODE_OP(fs, OP_STROP, STR_GET); 
    } else if (type == BUILTIN_LIST) {
        CODE_OP(fs, OP_LISTOP, LIST_GET); 
    } else {
        paw_assert(type == BUILTIN_MAP);
        CODE_OP(fs, OP_MAPOP, MAP_GET); 
    }
}

static void code_getn(struct FuncState *fs, paw_Type type)
{
    if (type == PAW_TSTR) {
        CODE_OP(fs, OP_STROP, STR_GETN); 
    } else {
        paw_assert(type == BUILTIN_LIST);
        CODE_OP(fs, OP_LISTOP, LIST_GETN); 
    }
}

static void code_index_expr(struct HirVisitor *V, struct HirIndex *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    struct HirType *target = HIR_TYPEOF(e->target);
    const paw_Type t = basic_code(G, target);
    fs->line = e->line;

    V->VisitExpr(V, e->target);
    if (e->is_slice) {
        code_slice_indices(V, e->first, e->second, target);
        code_getn(fs, t);
    } else {
        V->VisitExpr(V, e->first);
        code_get(fs, t);
    }
}

static void code_selector_expr(struct HirVisitor *V, struct HirSelector *e)
{
    struct Generator *G = V->ud;
    struct FuncState *fs = G->fs;
    fs->line = e->line;

    V->VisitExpr(V, e->target);
    if (e->is_index) {
        pawK_code_U(fs, OP_GETFIELD, e->index);
    } else {
        const struct HirVarInfo info = resolve_attr(G, HIR_TYPEOF(e->target), e->name);
        pawK_code_U(fs, OP_GETFIELD, info.index);
    }
}

static void register_items(struct Generator *G, struct HirDeclList *items)
{
    for (int i = 0; i < items->count; ++i) {
        struct HirDecl *item = items->data[i];
        if (HirIsFuncDecl(item)) {
            struct FuncDef *fdef = &get_def(G, i)->func;
            struct HirFuncDecl *d = HirGetFuncDecl(item);
            struct HirFuncDef *t = HirGetFuncDef(d->type);
            paw_assert(d->generics == NULL);

            fdef->name = mangle_name(G, d->name, d->body ? t->types : NULL);
            struct HirVarInfo info = {.index = fdef->vid};
            if (d->body == NULL) info.kind = VAR_CFUNC;
            struct ItemSlot *slot = new_item_slot(G, HIR_CAST_DECL(d), info);
            item_list_push(G->hir, G->items, slot);
        }
    }
}

static void set_cfunc(struct Generator *G, String *name, int g)
{
    paw_Env *P = ENV(G);
    Value *pval = pawE_get_val(P, g);

    const Value k = {.o = CAST_OBJECT(name)};
    const Value *pv = pawH_get(P->builtin, k);
    *pval = *pv;
}

static void code_items(struct HirVisitor *V)
{
    struct Generator *G = V->ud;
    struct ToplevelList *items = G->items;
    for (int i = 0; i < items->count; ++i) {
        struct ItemSlot *item = items->data[i];
        if (!HirIsFuncDecl(item->decl)) continue;
        struct HirFuncDecl *d = HirGetFuncDecl(item->decl);
        if (item->info.kind == VAR_CFUNC) {
            set_cfunc(G, d->name, item->info.index);
        } else {
            code_func(V, d, item->info);
        }
    }
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
    V->VisitAssignExpr = code_assign_expr;
    V->VisitBlock = code_block_stmt;
    V->VisitExprStmt = code_expr_stmt;
    V->VisitDeclStmt = code_decl_stmt;
    V->VisitIfStmt = code_if_stmt;
    V->VisitForStmt = code_for_stmt;
    V->VisitWhileStmt = code_while_stmt;
    V->VisitLabelStmt = code_label_stmt;
    V->VisitReturnStmt = code_return_stmt;
    V->VisitVarDecl = code_var_decl;
    V->VisitFieldDecl = code_field_decl;
}

static void code_module(struct Generator *G, struct Hir *hir)
{
    struct HirVisitor V;
    struct HirDeclList *items = pawHir_define(G->C, hir);
    setup_pass(&V, G);
    register_items(G, items);
    code_items(&V);
}

void pawP_codegen(struct Compiler *C, struct Hir *hir)
{
    paw_Env *P = ENV(C);
    struct Generator G = {
        .hir = hir,
        .sym = hir->symtab,
        .globals = hir->symtab->globals,
        .items = item_list_new(hir),
        .P = P,
        .C = C,
    };

    code_module(&G, hir);
}
