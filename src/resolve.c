// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// resolve.c: Implementation of the type checker. This code transforms an AST
// from the parser into a type-annotated HIR tree.

#include "ast.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "env.h"
#include "gc.h"
#include "hir.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "str.h"
#include "unify.h"

#define NAME_ERROR(R, ...) pawE_error(ENV(R), PAW_ENAME, (R)->line, __VA_ARGS__)
#define SYNTAX_ERROR(R, ...) pawE_error(ENV(R), PAW_ESYNTAX, (R)->line, __VA_ARGS__)
#define TYPE_ERROR(R, ...) pawE_error(ENV(R), PAW_ETYPE, (R)->line, __VA_ARGS__)
#define CACHED_STR(R, i) pawE_cstr(ENV(R), CAST_SIZE(i))
#define TYPE2CODE(R, type) (pawP_type2code((R)->C, type))

struct PartialItem {
    struct AstDecl *ast_decl;
    struct HirDecl *hir_decl;
    struct HirScope *scope;
};

static struct PartialItem *new_partial_item(struct Resolver *R, struct AstDecl *ad, struct HirDecl *hd, struct HirScope *scope)
{
    struct PartialItem *slot = pawK_pool_alloc(ENV(R), &R->hir->pool, sizeof(struct PartialItem));
    *slot = (struct PartialItem){
        .ast_decl = ad,
        .hir_decl = hd,
        .scope = scope, 
    };
    return slot;
}

DEFINE_LIST(struct Hir, item_list_, PartialItemList, struct PartialItem)

static struct HirStmt *resolve_stmt(struct Resolver *, struct AstStmt *);
static struct HirExpr *resolve_expr(struct Resolver *, struct AstExpr *);
static struct HirDecl *resolve_decl(struct Resolver *, struct AstDecl *);
static struct HirType *resolve_type(struct Resolver *, struct AstExpr *);

#define DEFINE_LIST_RESOLVER(name, T, T2) \
    static struct Hir##T2##List *resolve_##name##_list(struct Resolver *R, struct Ast##T##List *list) \
    { \
        if (list == NULL) return NULL; \
        struct Hir##T2##List *r = pawHir_##name##_list_new(R->hir); \
        for (int i = 0; i < list->count; ++i) { \
            struct Hir##T2 *node = resolve_##name(R, list->data[i]); \
            pawHir_##name##_list_push(R->hir, r, node); \
        } \
        return r; \
    }
DEFINE_LIST_RESOLVER(expr, Expr, Expr)
DEFINE_LIST_RESOLVER(decl, Decl, Decl)
DEFINE_LIST_RESOLVER(stmt, Stmt, Stmt)
DEFINE_LIST_RESOLVER(type, Expr, Type)

static struct HirType *normalize(struct Resolver *R, struct HirType *type)
{
    return pawU_normalize(R->U->table, type);
}

static void unify(struct Resolver *R, struct HirType *a, struct HirType *b)
{
    pawU_unify(R->U, a, b);
    normalize(R, a);
    normalize(R, b);
}

static struct HirType *get_type(struct Resolver *R, DefId did)
{
    paw_assert(did < R->dm->decls->count);
    return HIR_TYPEOF(R->dm->decls->data[did]);
}

static paw_Bool is_list_t(struct Resolver *R, struct HirType *type)
{
    return HirIsAdt(type) && HirGetAdt(type)->base == R->C->builtins[BUILTIN_LIST].did;
}

static paw_Bool is_map_t(struct Resolver *R, struct HirType *type)
{
    return HirIsAdt(type) && HirGetAdt(type)->base == R->C->builtins[BUILTIN_MAP].did;
}

static struct HirDecl *get_decl(struct Resolver *R, DefId did)
{
    struct DynamicMem *dm = R->dm;
    paw_assert(did < dm->decls->count);
    return dm->decls->data[did];
}

static struct HirBlock *new_block(struct Resolver *R, int line)
{
    struct HirStmt *r = pawHir_new_stmt(R->hir, line, kHirBlock);
    return HirGetBlock(r);
}

static paw_Bool is_unit_variant(struct Resolver *R, const struct HirType *type)
{
    if (HirIsFuncDef(type)) {
        struct HirDecl *decl = get_decl(R, type->fdef.did);
        return HirIsVariantDecl(decl) && 
            HirGetVariantDecl(decl)->fields == NULL;
    }
    return PAW_FALSE;
}

static struct HirAdtDecl *get_adt(struct Resolver *R, struct HirType *type)
{
    struct HirAdt *adt = HirGetAdt(type);
    struct HirDecl *decl = get_decl(R, adt->did);
    return HirGetAdtDecl(decl);
}

static struct HirType *resolve_operand(struct Resolver *R, struct HirExpr *expr)
{
    struct HirType *type = HIR_TYPEOF(expr);
    if (HirIsFuncDef(type)) {
        // handle unit enumerators
        struct HirDecl *decl = get_decl(R, type->fdef.did);
        if (HirIsVariantDecl(decl)) {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            if (d->fields == NULL) return HIR_FPTR(type)->result;
            TYPE_ERROR(R, "missing fields on enumerator");
        }
    }
    return type;
}

#define ARE_TYPES_SAME(a, b) ((a) == (b))

// TODO: move to unify.c/reuse logic in that file (write idempotent version of
// unify())
static paw_Bool test_types(struct Resolver *R, const struct HirType *a, const struct HirType *b);

static paw_Bool test_lists(struct Resolver *R, const struct HirTypeList *a, const struct HirTypeList *b)
{
    for (int i = 0; i < a->count; ++i) {
        if (!test_types(R, a->data[i], b->data[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

static paw_Bool test_types(struct Resolver *R, const struct HirType *a, const struct HirType *b)
{
    if (HIR_KINDOF(a) != HIR_KINDOF(b)) {
        return PAW_FALSE;
    }
    switch (HIR_KINDOF(a)) {
        case kHirTupleType:
            return test_lists(R, a->tuple.elems, b->tuple.elems);
        case kHirFuncPtr:
        case kHirFuncDef:
            return test_types(R, HIR_FPTR(a)->result, HIR_FPTR(b)->result) &&
                   test_lists(R, HIR_FPTR(a)->params, HIR_FPTR(b)->params);
        case kHirAdt: {
            if (a->adt.base == b->adt.base) {
                if (!a->adt.types == !b->adt.types) {
                    return a->adt.types != NULL
                               ? test_lists(R, a->adt.types, b->adt.types)
                               : PAW_TRUE;
                }
            }
            break;
        }
        default:
            paw_assert(HirIsGeneric(a) || HirIsUnknown(a));
            return ARE_TYPES_SAME(a, b);
    }
    return PAW_FALSE;
}

static struct HirScope *push_symbol_table(struct Resolver *R)
{
    return pawHir_new_scope(R->hir, R->symtab);
}

static void pop_symbol_table(struct Resolver *R)
{
    // Last symbol table should have been assigned to an AST node. The
    // next call to push_symbol_table() will allocate a new table.
    struct HirSymtab *st = R->symtab;
    paw_assert(st->scopes->count > 0);
    --st->scopes->count;
}

static void sanity_check(struct Resolver *R, struct HirDecl *new_decl)
{
#ifndef NDEBUG
    struct DynamicMem *dm = R->dm;
    for (DefId did = 0; did < dm->decls->count; ++did) {
        struct HirDecl *old_decl = dm->decls->data[did];
        paw_assert(old_decl->hdr.did == did);
        paw_assert(old_decl != new_decl);
    }
#else
    paw_unused(R);
    paw_unused(new_decl);
#endif
}

static DefId add_def(struct Resolver *R, struct HirDecl *decl)
{
    sanity_check(R, decl);
    return pawHir_add_decl(R->hir, decl);
}

static struct HirType *new_type(struct Resolver *R, DefId did, enum HirTypeKind kind, int line)
{
    struct HirType *type = pawHir_new_type(R->hir, line, kind);
    if (kind == kHirAdt) {
        type->adt.did = did;
    } else if (kind == kHirFuncDef) {
        type->fdef.did = did;
    } else if (kind == kHirGeneric) {
        type->generic.did = did;
    }
    if (did != NO_DECL) {
        // set type of associated definition
        struct HirDecl *d = get_decl(R, did);
        d->hdr.type = type;
    }
    return type;
}

static struct HirTypeList *collect_expr_types(struct Resolver *R, struct HirExprList *list) 
{                                                                                   
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(R->hir);
    for (int i = 0; i < list->count; ++i) {                                         
        struct HirType *type = resolve_operand(R, list->data[i]);
        pawHir_type_list_push(R->hir, new_list, type);
    }                                                                               
    return new_list;
}

static struct HirTypeList *collect_decl_types(struct Resolver *R, struct HirDeclList *list) 
{                                                                                   
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(R->hir);
    for (int i = 0; i < list->count; ++i) {                                         
        struct HirType *type = HIR_TYPEOF(list->data[i]);
        pawHir_type_list_push(R->hir, new_list, type);
    }                                                                               
    return new_list;
}

static void enter_inference_ctx(struct Resolver *R)
{
    pawU_enter_binder(R->U);
}

static void leave_inference_ctx(struct Resolver *R) 
{
    pawU_leave_binder(R->U); 
}

static struct HirScope *enclosing_scope(struct Resolver *R)
{
    struct HirSymtab *st = R->symtab;
    return st->scopes->data[st->scopes->count - 1];
}

static struct HirSymbol *add_symbol(struct Resolver *R, struct HirScope *scope, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = pawHir_add_symbol(R->hir, scope);
    symbol->name = name;
    symbol->decl = decl;
    return symbol;
}

static struct HirSymbol *declare_local(struct Resolver *R, String *name, struct HirDecl *decl)
{
    return add_symbol(R, enclosing_scope(R), name, decl);
}

// Allow a previously-declared variable to be accessed
static void define_local(struct HirSymbol *symbol) 
{ 
    symbol->is_init = PAW_TRUE; 
}

static struct HirSymbol *new_global(struct Resolver *R, String *name, struct HirDecl *decl, paw_Bool is_pub)
{
    struct HirScope *st = R->symtab->globals;
    for (int i = 0; i < st->symbols->count; ++i) {
        struct HirSymbol *symbol = st->symbols->data[i];
        if (pawS_eq(symbol->name, name)) {
            NAME_ERROR(R, "duplicate global '%s' (declared previously on line %d)",
                       name->text, symbol->decl->hdr.line);
        }
    }
    struct HirSymbol *symbol = add_symbol(R, st, name, decl);
    symbol->is_init = PAW_TRUE;
    return symbol;
}

static struct HirSymbol *try_resolve_symbol(struct Resolver *R, const String *name)
{
    // search the scoped symbols
    struct HirSymtab *scopes = R->symtab;
    const int nscopes = scopes->scopes->count;
    for (int depth = nscopes - 1; depth >= 0; --depth) {
        struct HirScope *scope = scopes->scopes->data[depth];
        const int index = pawHir_find_symbol(scope, name);
        if (index >= 0) {
            struct HirSymbol *symbol = scope->symbols->data[index];
            return scope->symbols->data[index];
        }
    }
    // search the global symbols
    const int index = pawHir_find_symbol(scopes->globals, name);
    if (index < 0) return NULL;
    return scopes->globals->symbols->data[index];
}

static struct HirSymbol *resolve_symbol(struct Resolver *R, const String *name)
{
    struct HirSymbol *symbol = try_resolve_symbol(R, name);
    if (symbol == NULL) NAME_ERROR(R, "undefined symbol '%s'", name->text);
    return symbol;
}

static struct HirDecl *resolve_field(struct HirDeclList *fields, String *name)
{
    if (fields == NULL) return NULL;
    for (int i = 0; i < fields->count; ++i) {
        struct HirDecl *decl = fields->data[i];
        if (pawS_eq(name, decl->hdr.name)) {
            return decl;
        }
    }
    return NULL;
}

static struct HirSymbol *new_local(struct Resolver *R, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = declare_local(R, name, decl);
    define_local(symbol);
    return symbol;
}

static struct HirScope *leave_block(struct Resolver *R)
{
    struct HirScope *scope = enclosing_scope(R);
    pop_symbol_table(R);
    return scope;
}

static void enter_block(struct Resolver *R, struct HirScope *scope)
{
    if (scope == NULL) {
        scope = push_symbol_table(R);
    } else {
        pawHir_add_scope(R->hir, R->symtab, scope);
    }
    scope->fn_depth = R->func_depth;
}

static struct HirScope *leave_function(struct Resolver *R)
{
    struct HirScope *scope = leave_block(R);
    CHECK_GC(ENV(R));
    --R->func_depth;
    return scope;
}

static void enter_function(struct Resolver *R, struct HirScope *scope, struct HirFuncDecl *func)
{
    ++R->func_depth;
    enter_block(R, scope);
    new_local(R, func->name, HIR_CAST_DECL(func));
}

static struct HirStmt *ResolveBlock(struct Resolver *R, struct AstBlock *block)
{
    struct HirStmt *result = pawHir_new_stmt(R->hir, block->line, kHirBlock);
    struct HirBlock *r = HirGetBlock(result);
    enter_block(R, NULL);
    r->stmts = resolve_stmt_list(R, block->stmts);
    leave_block(R);
    return result;
}

#define RESOLVE_BLOCK(R, block) HirGetBlock(ResolveBlock(R, block))

static struct HirType *register_decl_type(struct Resolver *R, struct HirDecl *decl, enum HirTypeKind kind)
{
    const DefId did = add_def(R, decl);
    struct HirType *r = new_type(R, did, kind, decl->hdr.line);
    decl->hdr.type = r;
    return r;
}

static struct HirType *register_variant(struct Resolver *R, struct HirVariantDecl *d)
{
    // An enum variant name can be thought of as a function from the type of the
    // variant's fields to the type of the enumeration. For example, given 'enum
    // E {X(str)}', E::X has type 'fn(str) -> E'.
    if (d->type == NULL) {
        d->type = register_decl_type(R, HIR_CAST_DECL(d), kHirFuncDef);
    }
    struct HirFuncDef *t = HirGetFuncDef(d->type);
    t->base = HirGetAdt(R->adt)->did;
    t->params = d->fields != NULL
        ? collect_decl_types(R, d->fields)
        : pawHir_type_list_new(R->hir);
    t->result = R->adt;
    return d->type;
}

static void allocate_decls(struct Resolver *R, struct HirDeclList *decls)
{
    if (decls == NULL) return;
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = decls->data[i];
        new_local(R, decl->hdr.name, decl);
    }
}

typedef void (*Instantiate)(struct Resolver *, struct HirTypeList *before, struct HirTypeList *after, struct HirDecl *);

static struct HirType *instantiate_type(struct Resolver *R, struct HirTypeList *before,
                                        struct HirTypeList *after, struct HirType *target);

static void instantiate_field(struct Resolver *R, struct HirTypeList *before, struct HirTypeList *after, struct HirDecl *decl)
{
    decl->hdr.type = instantiate_type(R, before, after, HIR_TYPEOF(decl));
    new_local(R, decl->hdr.name, decl);
    add_def(R, decl);
}

static void instantiate_variant(struct Resolver *R, struct HirTypeList *before, struct HirTypeList *after, struct HirDecl *decl)
{
    struct HirVariantDecl *r = HirGetVariantDecl(decl);
    instantiate_field(R, before, after, decl);
    HirGetFuncDef(r->type)->did = r->did;
    if (r->fields == NULL) return;

    enter_block(R, NULL);
    for (int j = 0; j < r->fields->count; ++j) {
        struct HirDecl *field = r->fields->data[j];
        instantiate_field(R, before, after, field);
    }
    leave_block(R);
}

static struct HirDeclList *instantiate_fields(struct Resolver *R, struct HirTypeList *before, struct HirTypeList *after, struct HirDeclList *list, Instantiate callback)
{
    if (list == NULL) return NULL;
    struct HirDeclList *copy = pawHir_decl_list_new(R->hir);
    for (int i = 0; i < list->count; ++i) {
        struct HirDecl *source = list->data[i];
        struct HirDecl *target = pawHir_copy_decl(R->hir, source);
        callback(R, before, after, target);
        pawHir_decl_list_push(R->hir, copy, target);
    }
    return copy;
}

static struct HirDecl *find_func_instance(struct Resolver *R, struct HirFuncDecl *base, struct HirTypeList *types)
{
    if (test_lists(R, types, HirGetFuncDef(base->type)->types)) {
        return HIR_CAST_DECL(base);
    }
    for (int i = 0; i < base->monos->count; ++i) {
        struct HirDecl *inst = base->monos->data[i];
        const struct HirType *type = HIR_TYPEOF(inst);
        if (test_lists(R, types, type->fdef.types)) {
            return inst;
        }
    }
    return NULL;
}

static struct HirDecl *find_struct_instance(struct Resolver *R, struct HirAdtDecl *base, struct HirTypeList *types)
{
    if (test_lists(R, types, HirGetAdt(base->type)->types)) {
        return HIR_CAST_DECL(base);
    }
    for (int i = 0; i < base->monos->count; ++i) {
        struct HirDecl *inst = base->monos->data[i];
        const struct HirType *type = HIR_TYPEOF(inst);
        if (test_lists(R, types, type->adt.types)) {
            return inst;
        }
    }
    return NULL;
}

static struct HirDecl *instantiate_struct(struct Resolver *, struct HirAdtDecl *, struct HirTypeList *);
static struct HirDecl *instantiate_func(struct Resolver *, struct HirFuncDecl *, struct HirTypeList *);

struct Subst {
    struct HirTypeList *before;
    struct HirTypeList *after;
    struct Resolver *R;
};

#define MAYBE_SUBST_LIST(F, list) ((list) != NULL ? subst_list(F, list) : NULL)

static struct HirTypeList *subst_list(struct HirFolder *F, struct HirTypeList *list)
{
    struct Subst *subst = F->ud;
    struct Resolver *R = subst->R;
    struct HirTypeList *copy = pawHir_type_list_new(R->hir);
    for (int i = 0; i < list->count; ++i) {
        pawHir_type_list_push(R->hir, copy, list->data[i]);
    }
    return F->FoldTypeList(F, copy);
}

static struct HirType *subst_fptr(struct HirFolder *F, struct HirFuncPtr *t)
{
    struct Subst *subst = F->ud;
    struct Resolver *R = subst->R;
    struct HirType *r = new_type(R, NO_DECL, kHirFuncPtr, t->line);
    r->fptr.params = subst_list(F, t->params);
    r->fptr.result = F->FoldType(F, t->result);
    return r;
}

static struct HirType *subst_fdef(struct HirFolder *F, struct HirFuncDef *t)
{
    struct Subst *subst = F->ud;
    struct Resolver *R = subst->R;

    if (t->types == NULL) {
        struct HirType *result = pawHir_new_type(R->hir, t->line, kHirFuncDef);
        struct HirFuncDef *r = HirGetFuncDef(result);
        r->params = subst_list(F, t->params);
        r->result = F->FoldType(F, t->result);
        r->did = NO_DECL; // set later for variants
        r->base = t->did;
        return result;
    }
    struct HirTypeList *types = subst_list(F, t->types);
    struct HirFuncDecl *base = HirGetFuncDecl(get_decl(R, t->base));
    struct HirDecl *inst = instantiate_func(R, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *subst_adt(struct HirFolder *F, struct HirAdt *t)
{
    struct Subst *subst = F->ud;
    struct Resolver *R = subst->R;

    if (t->types == NULL) return HIR_CAST_TYPE(t);
    struct HirTypeList *types = subst_list(F, t->types);
    struct HirAdtDecl *base = HirGetAdtDecl(get_decl(R, t->base));
    struct HirDecl *inst = instantiate_struct(R, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *subst_tuple(struct HirFolder *F, struct HirTupleType *t)
{
    struct Subst *subst = F->ud;
    struct Resolver *R = subst->R;

    struct HirType *result = new_type(R, NO_DECL, kHirTupleType, t->line);
    struct HirTupleType *r = HirGetTupleType(result);
    r->elems = subst_list(F, t->elems);
    return result;
}

static struct HirType *maybe_subst(struct HirFolder *F, struct HirType *t)
{
    struct Subst *s = F->ud;
    for (int i = 0; i < s->before->count; ++i) {
        if (ARE_TYPES_SAME(t, s->before->data[i])) {
            return s->after->data[i];
        }
    }
    return t;
}

static struct HirType *subst_generic(struct HirFolder *F, struct HirGeneric *t)
{
    return maybe_subst(F, HIR_CAST_TYPE(t));
}

static struct HirType *subst_unknown(struct HirFolder *F, struct HirUnknown *t)
{
    return maybe_subst(F, HIR_CAST_TYPE(t));
}

static void init_subst_folder(struct HirFolder *F, struct Subst *subst, struct Resolver *R, 
                              struct HirTypeList *before, struct HirTypeList *after)
{
    *subst = (struct Subst){
        .before = before,
        .after = after,
        .R = R,
    };
    pawHir_folder_init(F, R->hir, subst);
    F->FoldAdt = subst_adt;
    F->FoldFuncPtr = subst_fptr;
    F->FoldFuncDef = subst_fdef;
    F->FoldGeneric = subst_generic;
    F->FoldUnknown = subst_unknown;
    F->FoldTupleType = subst_tuple;
}

static struct HirTypeList *instantiate_typelist(struct Resolver *R, struct HirTypeList *before,
                                                struct HirTypeList *after, struct HirTypeList *target)
{
    struct Subst subst;
    struct HirFolder F;
    init_subst_folder(&F, &subst, R, before, after);
    return F.FoldTypeList(&F, target);
}

static struct HirType *instantiate_type(struct Resolver *R, struct HirTypeList *before,
                                        struct HirTypeList *after, struct HirType *target)
{
    struct Subst subst;
    struct HirFolder F;
    init_subst_folder(&F, &subst, R, before, after);
    return F.FoldType(&F, target);
}

static void prep_func_instance(struct Resolver *R, struct HirTypeList *before,
                               struct HirTypeList *after, struct HirInstanceDecl *d)
{
    struct Subst subst;
    struct HirFolder F;
    init_subst_folder(&F, &subst, R, before, after);

    struct HirFuncDef *fdef = HirGetFuncDef(d->type);
    struct HirTypeList *params = fdef->params;
    for (int i = 0; i < params->count; ++i) {
        params->data[i] = F.FoldType(&F, params->data[i]);
    }
    fdef->result = F.FoldType(&F, fdef->result);
}

static void instantiate_func_aux(struct Resolver *R, struct HirFuncDecl *base, struct HirInstanceDecl *inst, struct HirTypeList *types)
{
    enter_block(R, NULL);
    inst->name = base->name;
    inst->types = types;

    struct HirType *r = register_decl_type(R, HIR_CAST_DECL(inst), kHirFuncDef);
    r->fdef.base = base->did;
    r->fdef.params = collect_decl_types(R, base->params);
    r->fdef.result = HIR_FPTR(base->type)->result;
    r->fdef.types = types;
    inst->type = r;

    struct HirTypeList *generics = HirGetFuncDef(base->type)->types;
    prep_func_instance(R, generics, types, inst);

    leave_block(R);
}

static void instantiate_struct_aux(struct Resolver *R, struct HirAdtDecl *base,
                                   struct HirAdtDecl *inst, struct HirTypeList *types)
{
    enter_block(R, NULL);
    inst->name = base->name;
    inst->is_pub = base->is_pub;
    inst->is_struct = base->is_struct;

    struct HirType *r = register_decl_type(R, HIR_CAST_DECL(inst), kHirAdt);
    r->adt.base = base->did;
    r->adt.types = types;
    inst->type = r;

    struct HirType *enclosing = R->adt;
    R->adt = inst->type;

    struct HirTypeList *generics = HirGetAdt(base->type)->types;
    inst->type = instantiate_type(R, generics, types, inst->type);

    enter_block(R, NULL);
    Instantiate callback = base->is_struct ? instantiate_field : instantiate_variant;
    inst->fields = instantiate_fields(R, generics, types, base->fields, callback);
    inst->scope = leave_block(R);

    leave_block(R);
    R->adt = enclosing;
}

static struct HirDeclList *register_generics(struct Resolver *R, struct AstDeclList *generics)
{
    struct HirDeclList *list = pawHir_decl_list_new(R->hir);
    for (int i = 0; i < generics->count; ++i) {
        struct HirDecl *decl = resolve_decl(R, generics->data[i]);
        struct HirGenericDecl *gen = HirGetGenericDecl(decl);
        struct HirSymbol *symbol = new_local(R, gen->name, decl);
        pawHir_decl_list_push(R->hir, list, decl);
        symbol->is_type = PAW_TRUE;
        symbol->is_generic = PAW_TRUE;
    }
    return list;
}

static void check_template_param(struct Resolver *R, struct HirDeclList *params, struct HirTypeList *args)
{
    if (args->count > params->count) {
        TYPE_ERROR(R, "too many generics");
    } else if (args->count < params->count) {
        TYPE_ERROR(R, "not enough generics");
    }
}

static void normalize_type_list(struct Resolver *R, struct HirTypeList *types)
{
    for (int i = 0; i < types->count; ++i) {
        normalize(R, types->data[i]);
    }
}

static struct HirDecl *instantiate_struct(struct Resolver *R, struct HirAdtDecl *base, struct HirTypeList *types)
{
    check_template_param(R, base->generics, types);
    if (types == NULL) {
        return HIR_CAST_DECL(base);
    }
    normalize_type_list(R, types);
    struct HirDecl *inst = find_struct_instance(R, base, types);
    if (inst == NULL) {
        inst = pawHir_new_decl(R->hir, base->line, kHirAdtDecl);
        pawHir_decl_list_push(R->hir, base->monos, inst);
        instantiate_struct_aux(R, base, &inst->adt, types);
    }
    return inst;
}


static struct HirDecl *instantiate_func(struct Resolver *R, struct HirFuncDecl *base, struct HirTypeList *types)
{
    check_template_param(R, base->generics, types);
    if (types == NULL) return HIR_CAST_DECL(base);
    normalize_type_list(R, types);
    struct HirDecl *inst = find_func_instance(R, base, types);
    if (inst == NULL) {
        inst = pawHir_new_decl(R->hir, base->line, kHirInstanceDecl);
        pawHir_decl_list_push(R->hir, base->monos, inst);
        instantiate_func_aux(R, base, &inst->inst, types);
    }
    return inst;
}

struct HirDecl *pawP_instantiate(struct Resolver *R, struct HirDecl *base, struct HirTypeList *types)
{
    if (types == NULL) return base; 
    if (HIR_IS_POLY_ADT(base)) {
        return instantiate_struct(R, &base->adt, types);
    } else if (HIR_IS_POLY_FUNC(base)) {
        return instantiate_func(R, &base->func, types);
    }
    return base;
}

static struct HirScope *register_func(struct Resolver *R, struct AstFuncDecl *d, struct HirFuncDecl *r)
{
    enter_block(R, NULL);
    if (d->generics != NULL) {
        r->generics = register_generics(R, d->generics);
        r->monos = pawHir_decl_list_new(R->hir);
    }
    r->params = resolve_decl_list(R, d->params);
    struct HirType *result = resolve_type(R, d->result);
    struct HirScope *scope = leave_block(R);

    struct HirType *t = register_decl_type(R, HIR_CAST_DECL(r), kHirFuncDef);
    t->fdef.types = collect_decl_types(R, r->generics);
    t->fdef.params = collect_decl_types(R, r->params);
    t->fdef.result = result;
    t->fdef.base = r->did;
    r->type = t;
    return scope;
}

static struct HirDecl *ResolveFieldDecl(struct Resolver *R, struct AstFieldDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirFieldDecl);
    struct HirFieldDecl *r = HirGetFieldDecl(result);
    add_def(R, result);

    r->name = d->name == NULL
        ? SCAN_STRING(R, "(field)")
        : d->name;
    r->type = resolve_type(R, d->tag);
    return result;
}

static struct HirDecl *ResolveVariantDecl(struct Resolver *R, struct AstVariantDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirVariantDecl);
    struct HirVariantDecl *r = HirGetVariantDecl(result);
    r->index = d->index;
    r->name = d->name;

    enter_block(R, NULL);
    r->fields = resolve_decl_list(R, d->fields);
    r->scope = leave_block(R);
    new_local(R, d->name, result);

    r->type = register_variant(R, r);
    return result;
}

static struct HirDeclList *resolve_fields(struct Resolver *R, struct AstDeclList *src, String *parent)
{
    paw_Env *P = ENV(R);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    V_SET_OBJECT(pv, map);

    struct HirDeclList *dst = pawHir_decl_list_new(R->hir);
    for (int i = 0; i < src->count; ++i) {
        struct HirDecl *decl = resolve_decl(R, pawAst_decl_list_get(src, i));
        String *name = decl->hdr.name;
        const Value key = {.o = CAST_OBJECT(name)};
        pv = pawH_get(map, key);
        if (pv != NULL) {
            NAME_ERROR(R, "duplicate field '%s' in '%s'", name->text, parent->text);
        }
        pawH_insert(P, map, key, key);
        pawHir_decl_list_push(R->hir, dst, decl);
    }

    pawC_pop(P);
    return dst;
}

static void register_struct(struct Resolver *R, struct AstAdtDecl *d, struct HirAdtDecl *r)
{
    struct HirType *t = register_decl_type(R, HIR_CAST_DECL(r), kHirAdt);
    struct HirType *enclosing = R->adt;
    R->adt = t;

    enter_block(R, NULL);
    if (d->generics != NULL) {
        r->generics = register_generics(R, d->generics);
        r->monos = pawHir_decl_list_new(R->hir);
    }
    if (d->fields != NULL) {
        r->fields = resolve_fields(R, d->fields, d->name);
    }

    enter_block(R, NULL);
    allocate_decls(R, r->fields);
    r->scope = leave_block(R);

    leave_block(R);
    R->adt = enclosing;

    t->adt.types = collect_decl_types(R, r->generics);
    t->adt.base = r->did;
    r->type = t;
}

static void resolve_func_body(struct Resolver *R, struct AstFuncDecl *d, struct HirFuncDecl *r, struct HirScope *scope, enum FuncKind kind)
{
    r->fn_kind = kind;

    enter_function(R, scope, r);
    allocate_decls(R, r->params);

    struct HirType *last_result = R->result;
    const int last_count = R->nresults;
    R->result = HirGetFuncDef(r->type)->result;
    R->nresults = 0;

    enter_inference_ctx(R);
    r->body = RESOLVE_BLOCK(R, d->body);
    leave_inference_ctx(R);

    leave_function(R);
    R->nresults = last_count;
    R->result = last_result;
}

static struct HirStmt *ResolveReturnStmt(struct Resolver *R, struct AstReturnStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(R->hir, s->line, kHirReturnStmt);
    struct HirReturnStmt *r = HirGetReturnStmt(result);

    struct HirType *want = R->result;
    struct HirType *have = NULL;
    if (s->expr != NULL) {
        r->expr = resolve_expr(R, s->expr); 
        have = resolve_operand(R, r->expr);
    } else {
        have = get_type(R, PAW_TUNIT);
    }

    unify(R, have, want);
    ++R->nresults;
    return result;
}

static struct HirExpr *expect_bool_expr(struct Resolver *R, struct AstExpr *e)
{
    struct HirExpr *r = resolve_expr(R, e);
    unify(R, resolve_operand(R, r), get_type(R, PAW_TBOOL));
    return r;
}

static struct HirExpr *expect_int_expr(struct Resolver *R, struct AstExpr *e)
{
    struct HirExpr *r = resolve_expr(R, e);
    unify(R, resolve_operand(R, r), get_type(R, PAW_TINT));
    return r;
}

struct StructPack {
    String *name;
    struct HirDeclList *generics;
    struct HirDeclList *fields;
    struct HirType *type;
    struct HirDecl *decl;
    paw_Bool is_struct;
};

static struct StructPack unpack_struct(struct Resolver *R, struct HirType *type)
{
    if (HirIsFuncType(type)) {
        struct HirDecl *decl = get_decl(R, type->fdef.did);
        return (struct StructPack){
            .name = decl->variant.name,
            .fields = decl->variant.fields,
            .type = decl->variant.type,
            .decl = decl,
        };
    } else if (!HirIsAdt(type)) {
        TYPE_ERROR(R, "expected structure or enumerator");
    } else if (HIR_IS_BASIC_T(type) || HirGetAdt(type)->did == NO_DECL) {
        TYPE_ERROR(R, "fields/methods not yet implemented on builtin types"); // TODO
    }
    struct HirDecl *decl = get_decl(R, type->adt.did);
    struct HirAdtDecl *adt = HirGetAdtDecl(decl);
    return (struct StructPack){
        .is_struct = adt->is_struct,
        .name = adt->name,
        .generics = adt->generics,
        .fields = adt->fields,
        .type = adt->type,
        .decl = decl,
    };
}

// TODO: use this one, don't need unpack_struct
static struct HirDecl *expect_field_(struct Resolver *R, struct HirAdtDecl *adt, String *name)
{
    struct HirDecl *field = resolve_field(adt->fields, name);
    if (field == NULL) NAME_ERROR(R, "field '%s' does not exist in type '%s'", 
                                  name->text, adt->name->text);
    return field;
}

static struct HirDecl *expect_field(struct Resolver *R, const struct StructPack *pack, String *name)
{
    struct HirDecl *field = resolve_field(pack->fields, name);
    if (field == NULL) {
        NAME_ERROR(R, "field '%s' does not exist in type '%s'", name->text,
                   pack->name->text);
    }
    return field;
}

static struct HirType *resolve_tuple_type(struct Resolver *R, struct AstTupleType *e)
{
    if (e->types->count == 0) return get_type(R, PAW_TUNIT);
    struct HirType *r = new_type(R, NO_DECL, kHirTupleType, e->line);
    r->tuple.elems = resolve_type_list(R, e->types);
    return r;
}

static struct HirPath *resolve_path(struct Resolver *R, struct AstPath *path)
{
    paw_assert(path->count > 0);
    struct HirPath *r = pawHir_path_new(R->hir);
    for (int i = 0; i < path->count; ++i) {
        struct AstSegment *src = pawAst_path_get(path, i);
        struct HirSegment *dst = pawHir_segment_new(R->hir);
        dst->types = resolve_type_list(R, src->types);
        dst->name = src->name;
        pawHir_path_push(R->hir, r, dst);
    }
    return r;
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct HirDecl *resolve_base_seg(struct Resolver *R, struct HirSegment *base)
{
    struct HirSymbol *symbol = resolve_symbol(R, base->name);
    struct HirType *type = HIR_TYPEOF(symbol->decl);
    base->result = pawP_instantiate(R, symbol->decl, base->types);
    return base->result;
}

static struct HirDecl *resolve_next_seg(struct Resolver *R, struct HirDecl *prev, struct HirSegment *next)
{
    if (!is_enum_decl(prev)) TYPE_ERROR(R, "expected enumerator");
    if (next->types != NULL) TYPE_ERROR(R, "expected generic item");
    struct HirAdtDecl *adt = HirGetAdtDecl(prev);
    next->result = expect_field_(R, adt, next->name);
    return next->result;
}

static struct HirDecl *resolve_location(struct Resolver *R, struct HirPath *path)
{
    paw_assert(path->count > 0);
    struct HirSegment *seg = pawHir_path_get(path, 0);
    struct HirDecl *decl = resolve_base_seg(R, seg);
    for (int i = 1; i < path->count; ++i) {
        seg = pawHir_path_get(path, i);
        decl = resolve_next_seg(R, decl, seg);
    }
    // NOTE: Paw does not support associated types or static fields, etc., so it
    //       is not possible to have a path of length > 2. Such a path should
    //       cause a type error in the above loop.
    paw_assert(path->count < 3);
    return decl;
}

static void maybe_fix_unit_struct(struct Resolver *R, struct HirDecl *decl, struct HirExpr *expr)
{
    if (HirIsAdtDecl(decl)) {
        struct HirAdtDecl *adt = HirGetAdtDecl(decl);
        if (adt->did <= PAW_TSTR) {
            goto not_operand;
        }
        if (!adt->is_struct) {
            SYNTAX_ERROR(R, "missing variant specifier on enum '%s'", adt->name->text); 
        }
        if (adt->fields != NULL) {
            SYNTAX_ERROR(R, "missing fields on initializer for struct '%s'", adt->name->text);
        }
        struct HirPath *path = HirGetPathExpr(expr)->path;
        expr->hdr.kind = kHirLiteralExpr;
        struct HirLiteralExpr *r = HirGetLiteralExpr(expr);
        r->lit_kind = kHirLitComposite;
        r->comp.items = pawHir_expr_list_new(R->hir);
        r->comp.path = path;
    } else if (HirIsGenericDecl(decl)) {
not_operand:
        TYPE_ERROR(R, "expected operand but found type");
    }
}

static struct HirExpr *resolve_path_expr(struct Resolver *R, struct AstPathExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirPathExpr);
    struct HirPathExpr *r = HirGetPathExpr(result);
    r->path = resolve_path(R, e->path);

    struct HirDecl *decl = resolve_location(R, r->path);
    r->type = HIR_TYPEOF(decl);

    maybe_fix_unit_struct(R, decl, result);
    return result;
}

static struct HirType *resolve_path_type(struct Resolver *R, struct AstPathExpr *e)
{
    struct HirPath *path = resolve_path(R, e->path);
    struct HirDecl *decl = resolve_location(R, path);
    if (HirIsVarDecl(decl) || HirIsFuncDecl(decl)) {
        TYPE_ERROR(R, "'%s' is not a type", decl->hdr.name->text);
    }
    return HIR_TYPEOF(decl);
}

static struct HirExpr *resolve_logical_expr(struct Resolver *R, struct AstLogicalExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirLogicalExpr);
    struct HirLogicalExpr *r = HirGetLogicalExpr(result);
    r->is_and = e->is_and;
    r->lhs = expect_bool_expr(R, e->lhs);
    r->rhs = expect_bool_expr(R, e->rhs);
    r->type = get_type(R, PAW_TBOOL);
    return result;
}

static paw_Bool is_option_t(struct Resolver *R, const struct HirType *type)
{
    return HirIsAdt(type) && type->adt.base == R->C->builtins[BUILTIN_OPTION].did;
}

static paw_Bool is_result_t(struct Resolver *R, const struct HirType *type)
{
    return HirIsAdt(type) && type->adt.base == R->C->builtins[BUILTIN_RESULT].did;
}

static struct HirExpr *resolve_chain_expr(struct Resolver *R, struct AstChainExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirChainExpr);
    struct HirChainExpr *r = HirGetChainExpr(result);
    r->target = resolve_expr(R, e->target);

    struct HirType *type = resolve_operand(R, r->target);
    if (R->result == NULL) {
        SYNTAX_ERROR(R, "'?' outside function body");
    }
    if (is_option_t(R, type) || is_result_t(R, type)) {
        r->type = HirGetAdt(type)->types->data[0];
    } else {
        SYNTAX_ERROR(R, "invalid operand for '?' operator");
    }
    unify(R, R->result, type);
    return result;
}

static struct HirType *get_value_type(struct Resolver *R, struct HirType *target)
{
    if (is_list_t(R, target)) {
        return hir_list_elem(target);
    } else if (is_map_t(R, target)) {
        return hir_map_value(target);
    }
    return NULL;
}

static struct HirType *resolve_in_expr(struct Resolver *R, struct HirType *elem, struct HirType *adt)
{
    struct HirType *type = get_value_type(R, adt);
    if (type == NULL) {
        TYPE_ERROR(R, "expected List or Map");
    }
    unify(R, elem, type);
    return get_type(R, PAW_TBOOL);
}

static struct HirExpr *resolve_unop_expr(struct Resolver *R, struct AstUnOpExpr *e)
{
    // clang-format off
    static const uint8_t kValidOps[NUNARYOPS][PAW_NTYPES + 2] = {
        //     type  =  0, b, i, f, s, l, m
        [UNARY_LEN]  = {0, 0, 0, 0, 1, 1, 1},
        [UNARY_NEG]  = {0, 0, 1, 1, 0, 0, 0}, 
        [UNARY_NOT]  = {0, 1, 1, 1, 0, 0, 0}, 
        [UNARY_BNOT] = {0, 0, 1, 0, 0, 0, 0}, 
    };
    // clang-format on

    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirUnOpExpr);
    struct HirUnOpExpr *r = HirGetUnOpExpr(result);
    r->target = resolve_expr(R, e->target);
    r->op = e->op;

    struct HirType *type = resolve_operand(R, r->target);
    const paw_Type code = TYPE2CODE(R, type);
    if (code < 0 || !kValidOps[e->op][code]) {
        TYPE_ERROR(R, "unsupported operand type for unary operator");
    } else if (UNOP_IS_BOOL(e->op)) {
        r->type = get_type(R, PAW_TBOOL);
    } else if (e->op == UNARY_LEN) {
        r->type = get_type(R, PAW_TINT);
    } else {
        r->type = type;
    }
    return result;
}

static void op_type_error(struct Resolver *R, const struct HirType *type, const char *what)
{
    if (HirIsUnknown(type)) {
        TYPE_ERROR(R, "%s type must be known before comparison", what);
    } else {
        TYPE_ERROR(R, "%s type not equality comparable", what);
    }
}

static struct HirType *binop_list(struct Resolver *R, enum BinaryOp op, struct HirType *type)
{
    const struct HirType *elem_t = hir_list_elem(type);
    if (op == BINARY_ADD) return type;
    if (!HIR_IS_BASIC_T(elem_t)) {
        op_type_error(R, elem_t, "element");
    }
    return get_type(R, PAW_TBOOL);
}

static struct HirType *binop_map(struct Resolver *R, struct HirType *type)
{
    const struct HirType *key_t = hir_map_key(type);
    const struct HirType *value_t = hir_map_value(type);
    if (!HIR_IS_BASIC_T(key_t)) {
        op_type_error(R, key_t, "key");
    } else if (!HIR_IS_BASIC_T(value_t)) {
        op_type_error(R, value_t, "value");
    }
    return get_type(R, PAW_TBOOL);
}

static struct HirExpr *resolve_binop_expr(struct Resolver *R, struct AstBinOpExpr *e)
{
    static const uint8_t kValidOps[NBINARYOPS][PAW_NTYPES + 2] = {
        //     type   =  0, b, i, f, s, l, m
        [BINARY_EQ]   = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_NE]   = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_LT]   = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_LE]   = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_GT]   = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_GE]   = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_ADD]  = {0, 0, 1, 1, 1, 1, 0},
        [BINARY_SUB]  = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_MUL]  = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_DIV]  = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_MOD]  = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_BXOR] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_BAND] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_BOR]  = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_SHL]  = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_SHR]  = {0, 0, 1, 0, 0, 0, 0},
    };

    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirBinOpExpr);
    struct HirBinOpExpr *r = HirGetBinOpExpr(result);
    r->lhs = resolve_expr(R, e->lhs);
    r->rhs = resolve_expr(R, e->rhs);
    r->op = e->op;

    struct HirType *lhs = resolve_operand(R, r->lhs);
    struct HirType *rhs = resolve_operand(R, r->rhs);
    unify(R, lhs, rhs);

    const paw_Type code = TYPE2CODE(R, lhs);
    paw_assert(code == TYPE2CODE(R, rhs));
    if (code < 0 || !kValidOps[e->op][code]) {
        TYPE_ERROR(R, "unsupported operand types for binary operator");
    } else if (code == BUILTIN_LIST) {
        r->type = binop_list(R, e->op, lhs);
    } else if (code == BUILTIN_MAP) {
        r->type = binop_map(R, lhs);
    } else if (BINOP_IS_BOOL(e->op)) {
        r->type = get_type(R, PAW_TBOOL);
    } else {
        r->type = lhs;
    }
    return result;
}

static struct HirExpr *resolve_assign_expr(struct Resolver *R, struct AstAssignExpr *s)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, s->line, kHirAssignExpr);
    struct HirAssignExpr *r = HirGetAssignExpr(result);

    r->lhs = resolve_expr(R, s->lhs);
    if (!HirIsPathExpr(r->lhs) &&
            !HirIsIndex(r->lhs) &&
            !HirIsSelector(r->lhs)) {
        SYNTAX_ERROR(R, "invalid place for assignment");
    }
    r->rhs = resolve_expr(R, s->rhs);
    struct HirType *lhs = resolve_operand(R, r->lhs);
    struct HirType *rhs = resolve_operand(R, r->rhs);
    unify(R, lhs, rhs);
    r->type = lhs;
    return result;
}

static struct HirType *new_list_t(struct Resolver *R, struct HirType *elem_t)
{
    struct HirDecl *base = get_decl(R, R->C->builtins[BUILTIN_LIST].did);
    struct HirTypeList *types = pawHir_type_list_new(R->hir);
    pawHir_type_list_push(R->hir, types, elem_t);
    struct HirDecl *inst = instantiate_struct(R, &base->adt, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *new_map_t(struct Resolver *R, struct HirType *key_t, struct HirType *value_t)
{
    if (!HirIsUnknown(key_t) && !HIR_IS_BASIC_T(key_t)) {
        TYPE_ERROR(R, "key is not hashable");
    }
    struct HirDecl *base = get_decl(R, R->C->builtins[BUILTIN_MAP].did);
    struct HirTypeList *types = pawHir_type_list_new(R->hir);
    pawHir_type_list_push(R->hir, types, key_t);
    pawHir_type_list_push(R->hir, types, value_t);
    struct HirDecl *inst = instantiate_struct(R, &base->adt, types);
    return HIR_TYPEOF(inst);
}

static struct HirTypeList *new_unknowns(struct Resolver *R, int count)
{
    struct HirTypeList *list = pawHir_type_list_new(R->hir);
    for (int i = 0; i < count; ++i) {
        struct HirType *unknown = pawU_new_unknown(R->U);
        pawHir_type_list_push(R->hir, list, unknown);
    }
    return list;
}

struct Generalization {
    struct HirTypeList *types;
    struct HirTypeList *fields;
    struct HirType *result;
};

// Replace generic parameters with inference variables (struct HirUnknown). The 
// resulting '.fields' list can be unified with another list of types (argument or
// struct field types) to infer a concrete type for each unknown.
static struct Generalization generalize(struct Resolver *R, struct HirDeclList *generics, struct HirDeclList *fields)
{
    if (generics == NULL) return (struct Generalization){0};
    struct HirTypeList *gtypes = collect_decl_types(R, generics);
    struct HirTypeList *ftypes = collect_decl_types(R, fields);
    struct HirTypeList *unknowns = new_unknowns(R, generics->count);
    struct HirTypeList *replaced = instantiate_typelist(R, gtypes, unknowns, ftypes);
    return (struct Generalization){
        .types = unknowns,
        .fields = replaced,
    };
}

static struct HirType *resolve_container_type(struct Resolver *R, struct AstContainerType *e)
{
    struct HirType *first = resolve_type(R, e->first);
    if (e->second == NULL) {
        return new_list_t(R, first);
    }
    struct HirType *second = resolve_type(R, e->second);
    return new_map_t(R, first, second);
}

static struct HirType *resolve_signature(struct Resolver *R, struct AstSignature *e)
{
    struct HirType *type = new_type(R, NO_DECL, kHirFuncPtr, e->line);
    struct HirType *result = resolve_type(R, e->result);
    type->fptr.params = resolve_type_list(R, e->params);
    type->fptr.result = result;
    return type;
}

static struct HirDecl *resolve_closure_param(struct Resolver *R, struct AstFieldDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirFieldDecl);
    struct HirFieldDecl *r = HirGetFieldDecl(result);
    add_def(R, result);

    r->name = d->name;
    r->type = d->tag == NULL
        ? pawU_new_unknown(R->U)
        : resolve_type(R, d->tag);
    new_local(R, r->name, result);
    return result;
}

static struct HirExpr *resolve_closure_expr(struct Resolver *R, struct AstClosureExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirClosureExpr);
    struct HirClosureExpr *r = HirGetClosureExpr(result);

    struct HirType *last_result = R->result;
    const int last_count = R->nresults;
    enter_block(R, NULL);
    R->nresults = 0;

    r->params = pawHir_decl_list_new(R->hir);
    for (int i = 0; i < e->params->count; ++i) {
        struct AstFieldDecl *src = AstGetFieldDecl(e->params->data[i]);
        struct HirDecl *dst = resolve_closure_param(R, src);
        pawHir_decl_list_push(R->hir, r->params, dst);
    }
    struct HirType *ret = e->result == NULL
        ? pawU_new_unknown(R->U)
        : resolve_type(R, e->result);

    struct HirType *t = new_type(R, NO_DECL, kHirFuncPtr, e->line);
    t->fptr.params = collect_decl_types(R, r->params);
    t->fptr.result = ret;
    R->result = ret;
    r->type = t;

    if (e->has_body) {
        r->body = RESOLVE_BLOCK(R, e->body);
        r->has_body = PAW_TRUE;
        if (R->nresults == 0) {
            // implicit 'return ()'
            unify(R, ret, get_type(R, PAW_TUNIT));
        }
    } else {
        r->expr = resolve_expr(R, e->expr);
        struct HirType *type = resolve_operand(R, r->expr);
        unify(R, ret, type);
    }

    leave_block(R);
    R->nresults = last_count;
    R->result = last_result;
    return result;
}

static void maybe_fix_builtin(struct Resolver *R, String *name, DefId did)
{
    struct Builtin *b = R->C->builtins;
    for (enum BuiltinKind k = 0; k < NBUILTINS; ++k) {
        if (pawS_eq(b[k].name, name)) {
            b[k].did = did;
            break;
        }
    }
}

static struct PartialItem *register_adt_item(struct Resolver *R, struct AstAdtDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirAdtDecl);
    struct HirAdtDecl *r = HirGetAdtDecl(result);
    r->is_pub = d->is_pub;
    r->is_struct = d->is_struct;
    r->name = d->name;

    struct HirSymbol *symbol = new_global(R, d->name, result, d->is_pub);
    symbol->is_type = PAW_TRUE;
    register_struct(R, d, r);
    maybe_fix_builtin(R, r->name, r->did);
    return new_partial_item(R, AST_CAST_DECL(d), result, NULL);
}

static struct HirDecl *ResolveAdtDecl(struct Resolver *R, struct AstAdtDecl *d)
{
    paw_unused(R);
    paw_unused(d);
    return NULL;
}

static struct HirDecl *ResolveVarDecl(struct Resolver *R, struct AstVarDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirVarDecl);
    struct HirVarDecl *r = HirGetVarDecl(result);
    r->name = d->name;

    struct HirSymbol *symbol = declare_local(R, d->name, result);
    r->init = resolve_expr(R, d->init);
    define_local(symbol);

    struct HirType *init = resolve_operand(R, r->init);
    if (d->tag != NULL) {
        struct HirType *tag = resolve_type(R, d->tag);
        unify(R, init, tag);
    }
    add_def(R, result);
    r->type = init;
    return result;
}

static struct HirDecl *ResolveTypeDecl(struct Resolver *R, struct AstTypeDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirTypeDecl);
    struct HirTypeDecl *r = HirGetTypeDecl(result);
    r->generics = NULL; // TODO: generic parameters for aliases

    struct HirSymbol *symbol = declare_local(R, d->name, result);
    r->rhs = resolve_expr(R, d->rhs);
    r->type = resolve_operand(R, r->rhs);
    define_local(symbol);
    return result;
}

static struct HirType *infer_func_template(struct Resolver *R, struct HirFuncDecl *base, struct HirExprList *args)
{
    struct Generalization g = generalize(R, base->generics, base->params);

    paw_assert(args->count == g.fields->count);
    for (int i = 0; i < g.fields->count; ++i) {
        struct HirType *a = g.fields->data[i];
        struct HirType *b = resolve_operand(R, args->data[i]);
        unify(R, a, b);
    }
    struct HirDecl *inst = instantiate_func(R, base, g.types);
    return HIR_TYPEOF(inst);
}

static struct HirExpr *resolve_variant_expr(struct Resolver *R, struct AstCallExpr *call)
{
    struct HirExpr *expr = resolve_expr(R, call->target);
    if (!HirIsPathExpr(expr)) return NULL;
    struct HirType *type = HIR_TYPEOF(expr);
    if (!HirIsAdt(type)) return NULL;
    struct HirAdtDecl *adt = get_adt(R, type);
    if (!adt->is_struct) return NULL;

    struct HirExpr *result = pawHir_new_expr(R->hir, call->line, kHirVariantExpr);
    struct HirVariantExpr *r = HirGetVariantExpr(result);

    struct HirPath *path = HirGetPathExpr(expr)->path;
    struct HirSegment *last = pawHir_path_get(path, path->count - 1);
    struct HirDecl *decl = expect_field_(R, adt, last->name);
    r->index = HirGetVariantDecl(decl)->index;
    return result;
}

// Resolve a function call or enumerator constructor
static struct HirExpr *resolve_call_expr(struct Resolver *R, struct AstCallExpr *e)
{
    struct HirExpr *result = resolve_variant_expr(R, e);
    if (result != NULL) return result;
    result = pawHir_new_expr(R->hir, e->line, kHirCallExpr);
    struct HirCallExpr *r = HirGetCallExpr(result);

    r->target = resolve_expr(R, e->target);
    r->func = HIR_TYPEOF(r->target);
    if (!HirIsFuncType(r->func)) {
        TYPE_ERROR(R, "type is not callable");
    } else if (e->args->count < HIR_FPTR(r->func)->params->count) {
        SYNTAX_ERROR(R, "not enough arguments");
    } else if (e->args->count > HIR_FPTR(r->func)->params->count) {
        SYNTAX_ERROR(R, "too many arguments");
    }
    if (HirIsFuncDef(r->func)) {
        // Function type has an associated declaration. If that declaration is
        // for a function template, attempt to infer the type parameters.
        struct HirDecl *decl = get_decl(R, HirGetFuncDef(r->func)->did);
        if (HIR_IS_POLY_FUNC(decl)) {
            r->args = resolve_expr_list(R, e->args);
            r->func = infer_func_template(R, &decl->func, r->args);
            r->type = HIR_FPTR(r->func)->result; // 'fptr' is common prefix
            return result;
        }
    }

    if (is_unit_variant(R, r->func)) {
        TYPE_ERROR(R, "cannot call unit variant (omit '()' to construct)");
    }
    const struct HirFuncPtr *func = HIR_FPTR(r->func);
    const struct HirTypeList *params = func->params;
    r->args = resolve_expr_list(R, e->args);
    r->type = func->result;

    paw_assert(e->args->count == params->count);
    for (int i = 0; i < params->count; ++i) {
        struct HirExpr *arg = pawHir_expr_list_get(r->args, i);
        unify(R, params->data[i], resolve_operand(R, arg));
    }
    return result;
}

static struct HirExpr *resolve_conversion_expr(struct Resolver *R, struct AstConversionExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirConversionExpr);
    struct HirConversionExpr *r = HirGetConversionExpr(result);

    r->to = e->to;
    r->arg = resolve_expr(R, e->arg);
    struct HirType *type = resolve_operand(R, r->arg);
    if (!HirIsAdt(type) || 
            HirGetAdt(type)->did == PAW_TUNIT ||
            HirGetAdt(type)->did == PAW_TSTR) {
        TYPE_ERROR(R, "argument to conversion must be scalar");
    }
    r->type = get_type(R, e->to);
    return result;
}

static struct HirType *resolve_basic_lit(struct Resolver *R, struct AstBasicLit *e, struct HirBasicLit *r)
{
    r->t = e->t;
    r->value = e->value;
    return get_type(R, r->t);
}

static struct HirType *resolve_tuple_lit(struct Resolver *R, struct AstTupleLit *e, struct HirTupleLit *r, int line)
{
    r->elems = resolve_expr_list(R, e->elems);
    struct HirType *type = new_type(R, NO_DECL, kHirTupleType, line);
    HirGetTupleType(type)->elems = collect_expr_types(R, r->elems);
    return type;
}

static struct HirType *resolve_list_lit(struct Resolver *R, struct AstContainerLit *e, struct HirContainerLit *r)
{
    struct Unifier *U = R->U;
    struct HirType *elem_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *ast_expr = e->items->data[i];
        struct HirExpr *hir_expr = resolve_expr(R, ast_expr);
        unify(R, resolve_operand(R, hir_expr), elem_t);

        pawHir_expr_list_push(R->hir, r->items, hir_expr);
    }
    return new_list_t(R, elem_t);
}

static struct HirType *resolve_map_lit(struct Resolver *R, struct AstContainerLit *e, struct HirContainerLit *r)
{
    struct Unifier *U = R->U;
    struct HirType *key_t = pawU_new_unknown(U);
    struct HirType *value_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *ast_expr = e->items->data[i];
        struct HirExpr *hir_expr = resolve_expr(R, ast_expr);
        struct HirFieldExpr *field = HirGetFieldExpr(hir_expr);
        paw_assert(field->fid == -1);
        struct HirType *k = resolve_operand(R, field->key);
        struct HirType *v = resolve_operand(R, field->value);
        unify(R, k, key_t);
        unify(R, v, value_t);

        pawHir_expr_list_push(R->hir, r->items, hir_expr);
    }
    return new_map_t(R, key_t, value_t);
}

static struct HirType *resolve_container_lit(struct Resolver *R, struct AstContainerLit *e, struct HirContainerLit *r)
{
    r->code = e->code;
    r->items = pawHir_expr_list_new(R->hir);
    if (e->code == BUILTIN_LIST) {
        return resolve_list_lit(R, e, r);
    } else {
        paw_assert(e->code == BUILTIN_MAP);
        return resolve_map_lit(R, e, r);
    }
}

static struct HirExpr *resolve_field_expr(struct Resolver *R, struct AstFieldExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirFieldExpr);
    struct HirFieldExpr *r = HirGetFieldExpr(result);
    r->fid = e->fid;
    if (e->fid < 0) {
        r->key = resolve_expr(R, e->key);
    } else {
        r->name = e->name;
    }
    r->value = resolve_expr(R, e->value);
    r->type = resolve_operand(R, r->value);
    return result;
}

static struct HirType *resolve_composite_lit(struct Resolver *R, struct AstCompositeLit *e, struct HirCompositeLit *r)
{
    r->path = resolve_path(R, e->path);
    struct HirDecl *decl = resolve_location(R, r->path);
    if (!HirIsAdtDecl(decl)) {
        TYPE_ERROR(R, "expected structure type");
    }
    // Use a temporary Map to avoid searching repeatedly through the list of
    // fields.
    paw_Env *P = ENV(R);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    V_SET_OBJECT(pv, map);

    Value key;
    struct Generalization g;
    struct HirTypeList *field_types = NULL;
    paw_Bool is_inference = PAW_FALSE;
    struct HirType *target = HIR_TYPEOF(decl);
    const struct StructPack pack = unpack_struct(R, target);
    if (!pack.is_struct) {
        TYPE_ERROR(R, "expected structure but found enumeration '%s'", pack.name->text);
    } else if (pack.fields == NULL) {
        SYNTAX_ERROR(R, "unexpected curly braces on initializer for unit structure '%s'"
                        "(use name without '{}' to create unit struct)", pack.name->text);
    } else if (HIR_IS_POLY_ADT(pack.decl)) {
        struct HirAdtDecl *d = &pack.decl->adt;
        g = generalize(R, d->generics, d->fields);
        is_inference = PAW_TRUE;
        field_types = g.fields;
    }
    struct HirExprList *order = pawHir_expr_list_new(R->hir);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *ast_item = e->items->data[i];
        struct HirExpr *hir_item = resolve_expr(R, ast_item);
        struct HirFieldExpr *item = HirGetFieldExpr(hir_item);
        V_SET_OBJECT(&key, item->name);
        if (pawH_contains(map, key)) {
            NAME_ERROR(R, "duplicate field '%s' in initializer for struct '%s'", 
                       item->name->text, pack.name->text);
        }
        Value *value = pawH_create(P, map, key);
        V_SET_INT(value, i);
        pawHir_expr_list_push(R->hir, order, hir_item);
    }
    for (int i = 0; i < pack.fields->count; ++i) {
        struct HirDecl *decl = pack.fields->data[i];
        struct HirFieldDecl *field = HirGetFieldDecl(decl);
        V_SET_OBJECT(&key, field->name);
        Value *value = pawH_get(map, key);
        if (value == NULL) {
            NAME_ERROR(R, "missing initializer for field '%s' in struct '%s'",
                       field->name->text, pack.name->text);
        }
        const paw_Int index = V_INT(*value);
        struct HirType *field_t = is_inference
                               ? field_types->data[i]
                               : HIR_TYPEOF(pack.fields->data[i]);
        struct HirExpr *item = order->data[index];
        struct HirType *item_t = resolve_operand(R, item);
        item->field.fid = i;
        unify(R, item_t, field_t);
        pawH_erase(map, key);
    }
    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(map, &iter)) {
        const Value *pkey = pawH_key(map, CAST_SIZE(iter));
        NAME_ERROR(R, "unexpected field '%s' in initializer for struct '%s'",
                   V_STRING(*pkey), pack.name->text);
    }
    paw_assert(pack.fields->count == e->items->count);
    pawC_pop(P); // pop map

    if (is_inference) {
        struct HirDecl *inst = instantiate_struct(R, &pack.decl->adt, g.types);
        target = HIR_TYPEOF(inst);
    }
    r->items = order;
    return target;
}

static struct HirExpr *resolve_literal_expr(struct Resolver *R, struct AstLiteralExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirLiteralExpr);
    struct HirLiteralExpr *r = HirGetLiteralExpr(result);

    // literal kinds correspond 1-to-1 between AST and HIR
    r->lit_kind = CAST(e->lit_kind, enum HirLitKind);

    if (e->lit_kind == kAstBasicLit) {
        r->type = resolve_basic_lit(R, &e->basic, &r->basic);
    } else if (e->lit_kind == kAstTupleLit) {
        r->type = resolve_tuple_lit(R, &e->tuple, &r->tuple, e->line);
    } else if (e->lit_kind == kAstContainerLit) {
        r->type = resolve_container_lit(R, &e->cont, &r->cont);
    } else {
        paw_assert(e->lit_kind == kAstCompositeLit);
        r->type = resolve_composite_lit(R, &e->comp, &r->comp);
    }
    return result;
}

static struct PartialItem *register_func_item(struct Resolver *R, struct AstFuncDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirFuncDecl);
    struct HirFuncDecl *r = HirGetFuncDecl(result);
    r->is_pub = d->is_pub;
    r->fn_kind = d->fn_kind;
    r->name = d->name;

    struct HirSymbol *symbol = new_global(R, d->name, result, d->is_pub);
    symbol->is_type = d->generics != NULL;
    struct HirScope *scope = register_func(R, d, r);
    return new_partial_item(R, AST_CAST_DECL(d), result, scope);
}

static void resolve_func_item(struct Resolver *R, struct PartialItem *slot)
{
    struct AstFuncDecl *ast_func = AstGetFuncDecl(slot->ast_decl);
    struct HirFuncDecl *hir_func = HirGetFuncDecl(slot->hir_decl);
    if (ast_func->body != NULL) {
        resolve_func_body(R, ast_func, hir_func, slot->scope, FUNC_FUNCTION);
    }
}

static struct HirDecl *ResolveFuncDecl(struct Resolver *R, struct AstFuncDecl *d)
{
    paw_unused(R);
    paw_unused(d);
    return NULL;
}

static struct HirStmt *ResolveIfStmt(struct Resolver *R, struct AstIfStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(R->hir, s->line, kHirIfStmt);
    struct HirIfStmt *r = HirGetIfStmt(result);

    r->cond = expect_bool_expr(R, s->cond);
    r->then_arm = resolve_stmt(R, s->then_arm);
    if (s->else_arm != NULL) {
        r->else_arm = resolve_stmt(R, s->else_arm);
    }
    return result;
}

static struct HirStmt *ResolveExprStmt(struct Resolver *R, struct AstExprStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(R->hir, s->line, kHirExprStmt);
    struct HirExprStmt *r = HirGetExprStmt(result);
    r->expr = resolve_expr(R, s->expr);
    return result;
}

static struct HirStmt *ResolveWhileStmt(struct Resolver *R, struct AstWhileStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(R->hir, s->line, kHirWhileStmt);
    struct HirWhileStmt *r = HirGetWhileStmt(result);
    r->is_dowhile = s->is_dowhile;

    enter_block(R, NULL);
    r->cond = expect_bool_expr(R, s->cond);
    r->block = RESOLVE_BLOCK(R, s->block);
    leave_block(R);
    return result;
}

static void visit_forbody(struct Resolver *R, String *iname, struct HirType *itype, struct AstBlock *b, struct HirForStmt *r)
{
    enter_block(R, NULL);

    r->control = pawHir_new_decl(R->hir, b->line, kHirVarDecl);
    struct HirVarDecl *control = HirGetVarDecl(r->control);
    control->name = iname;
    control->type = itype;
    new_local(R, iname, r->control);

    r->block = new_block(R, b->line);
    r->block->stmts = resolve_stmt_list(R, b->stmts);
    leave_block(R);
}

static void visit_fornum(struct Resolver *R, struct AstForStmt *s, struct HirForStmt *r)
{
    struct AstForNum *fornum = &s->fornum;

    r->fornum.begin = expect_int_expr(R, fornum->begin);
    r->fornum.end = expect_int_expr(R, fornum->end);
    r->fornum.step = expect_int_expr(R, fornum->step);

    visit_forbody(R, s->name, get_type(R, PAW_TINT), s->block, r);
}

// TODO: allow function with signature fn iter<I, T>(I) -> (fn(int) -> T)
static void visit_forin(struct Resolver *R, struct AstForStmt *s, struct HirForStmt *r)
{
    r->forin.target = resolve_expr(R, s->forin.target);
    struct HirType *iter_t = resolve_operand(R, r->forin.target);
    struct HirType *elem_t = get_value_type(R, iter_t);

    if (elem_t == NULL) {
        TYPE_ERROR(R, "'for..in' not supported for type");
    }
    visit_forbody(R, s->name, elem_t, s->block, r);
}

static struct HirStmt *ResolveForStmt(struct Resolver *R, struct AstForStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(R->hir, s->line, kHirForStmt);
    struct HirForStmt *r = HirGetForStmt(result);
    r->is_fornum = s->is_fornum;
    enter_block(R, NULL);
    if (s->is_fornum) {
        visit_fornum(R, s, r);
    } else {
        visit_forin(R, s, r);
    }
    leave_block(R);
    return result;
}

static struct HirExpr *resolve_index(struct Resolver *R, struct AstIndex *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirIndex);
    struct HirIndex *r = HirGetIndex(result);
    r->target = resolve_expr(R, e->target);
    r->is_slice = e->is_slice;

    struct HirType *target = resolve_operand(R, r->target);
    struct HirType *expect = NULL;
    if (is_list_t(R, target)) {
        expect = get_type(R, PAW_TINT);
        r->type = e->is_slice ? target : hir_list_elem(target);
    } else if (is_map_t(R, target)) {
        if (e->is_slice) {
            TYPE_ERROR(R, "slice operation not supported on map "
                          "(requires '[T]' or 'str')");
        }
        expect = hir_map_key(target);
        r->type = hir_map_value(target);
    } else if (HirIsAdt(target) &&
            HirGetAdt(target)->base == PAW_TSTR) {
        expect = get_type(R, PAW_TINT);
        r->type = get_type(R, PAW_TSTR);
    } else {
        TYPE_ERROR(R, "type cannot be indexed (not a container)");
    }
    if (e->is_slice) {
        if (e->first != NULL) {
            r->first = resolve_expr(R, e->first);
            unify(R, expect, resolve_operand(R, r->first));
        }
        if (e->second != NULL) {
            r->second = resolve_expr(R, e->second);
            unify(R, expect, resolve_operand(R, r->second));
        }
    } else {
        r->first = resolve_expr(R, e->first);
        unify(R, expect, resolve_operand(R, r->first));
    }
    return result;
}

static struct HirExpr *resolve_selector(struct Resolver *R, struct AstSelector *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirSelector);
    struct HirSelector *r = HirGetSelector(result);
    r->target = resolve_expr(R, e->target);

    struct HirType *target = resolve_operand(R, r->target);
    if (HirIsTupleType(target)) {
        // tuples are indexed with "Expr" "." "int_lit"
        struct HirTypeList *types = target->tuple.elems;
        if (!e->is_index) {
            TYPE_ERROR(R, "expected index of tuple element");
        } else if (e->index >= types->count) {
            TYPE_ERROR(R, "element index %d out of range of %d-tuple",
                       e->index, types->count);
        }
        
        r->index = e->index;
        r->is_index = PAW_TRUE;
        r->type = types->data[e->index];
        return result;
    }
    const struct StructPack pack = unpack_struct(R, target);
    if (!pack.is_struct) {
        TYPE_ERROR(R, "cannot select field of enum variant (use pattern "
                      "matching to unpack variant fields)");
    } else if (e->is_index) {
        TYPE_ERROR(R, "expected name of struct field (integer indices can "
                      "only be used with tuples)");
    } else if (pack.fields == NULL) {
        NAME_ERROR(R, "unit structure '%s' has no fields", pack.name->text);
    }
    struct HirDecl *field = expect_field(R, &pack, e->name);
    r->type = HIR_TYPEOF(field);
    r->name = e->name;
    return result;
}

static struct HirStmt *ResolveLabelStmt(struct Resolver *R, struct AstLabelStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(R->hir, s->line, kHirLabelStmt);
    struct HirLabelStmt *r = HirGetLabelStmt(result);
    r->label = s->label;
    return result;
}

static struct HirStmt *ResolveDeclStmt(struct Resolver *R, struct AstDeclStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(R->hir, s->line, kHirDeclStmt);
    struct HirDeclStmt *r = HirGetDeclStmt(result);
    r->decl = resolve_decl(R, s->decl);
    return result;
}

static struct HirDecl *ResolveGenericDecl(struct Resolver *R, struct AstGenericDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirGenericDecl);
    struct HirGenericDecl *r = HirGetGenericDecl(result);
    add_def(R, result);

    r->type = new_type(R, r->did, kHirGeneric, d->line);
    struct HirGeneric *t = HirGetGeneric(r->type);
    r->name = t->name = d->name;
    return result;
}

static struct HirDecl *resolve_decl(struct Resolver *R, struct AstDecl *decl)
{
    R->line = decl->hdr.line;
    switch (AST_KINDOF(decl)) {
#define DEFINE_CASE(a, b)                       \
        case kAst##a:                           \
            return Resolve##a(R, AstGet##a(decl));
        AST_DECL_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirStmt *resolve_stmt(struct Resolver *R, struct AstStmt *stmt)
{
    R->line = stmt->hdr.line;
    switch (AST_KINDOF(stmt)) {
#define DEFINE_CASE(a, b)                       \
        case kAst##a:                           \
            return Resolve##a(R, AstGet##a(stmt));
        AST_STMT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

// NOTE: Some expressions are known to directly represent types, based on the context
//       (type annotations, type arguments, etc.). Call resolve_type() to convert such
//       an expression into a HIR type.

static struct HirType *resolve_type(struct Resolver *R, struct AstExpr *expr)
{
    struct HirType *r;
    R->line = expr->hdr.line;
    switch (AST_KINDOF(expr)) {
        case kAstPathExpr:
            r = resolve_path_type(R, AstGetPathExpr(expr));
            break;
        case kAstTupleType:
            r = resolve_tuple_type(R, AstGetTupleType(expr));
            break;
        case kAstSignature:
            r = resolve_signature(R, AstGetSignature(expr));
            break;
        case kAstContainerType:
            r = resolve_container_type(R, AstGetContainerType(expr)); 
            break;
        default:
            TYPE_ERROR(R, "expected type");
    }
    return normalize(R, r);
}

static struct HirExpr *resolve_expr(struct Resolver *R, struct AstExpr *expr)
{
    struct HirExpr *r;
    R->line = expr->hdr.line;
    switch (AST_KINDOF(expr)) {
        case kAstLiteralExpr:
            r = resolve_literal_expr(R, AstGetLiteralExpr(expr));
            break;
        case kAstLogicalExpr:
            r = resolve_logical_expr(R, AstGetLogicalExpr(expr));
            break;
        case kAstPathExpr:
            r = resolve_path_expr(R, AstGetPathExpr(expr));
            break;
        case kAstChainExpr:
            r = resolve_chain_expr(R, AstGetChainExpr(expr));
            break;
        case kAstUnOpExpr:
            r = resolve_unop_expr(R, AstGetUnOpExpr(expr));
            break;
        case kAstBinOpExpr:
            r = resolve_binop_expr(R, AstGetBinOpExpr(expr));
            break;
        case kAstClosureExpr:
            r = resolve_closure_expr(R, AstGetClosureExpr(expr));
            break;
        case kAstConversionExpr:
            r = resolve_conversion_expr(R, AstGetConversionExpr(expr));
            break;
        case kAstCallExpr:
            r = resolve_call_expr(R, AstGetCallExpr(expr));
            break;
        case kAstIndex:
            r = resolve_index(R, AstGetIndex(expr));
            break;
        case kAstSelector:
            r = resolve_selector(R, AstGetSelector(expr)); 
            break; 
        case kAstAssignExpr:
            r = resolve_assign_expr(R, AstGetAssignExpr(expr));
            break;
        default:
            r = resolve_field_expr(R, AstGetFieldExpr(expr));
    }

    struct HirType *type = HIR_TYPEOF(r);
    r->hdr.type = normalize(R, type);
    return r;
}

static struct HirDeclList *register_items(struct Resolver *R, struct AstDeclList *items, struct PartialItemList **pslots)
{
    struct PartialItemList *slots = item_list_new(R->hir);
    struct HirDeclList *output = pawHir_decl_list_new(R->hir);
    for (int i = 0; i < items->count; ++i) {
        struct AstDecl *item = items->data[i];
        struct PartialItem *slot;
        switch (HIR_KINDOF(item)) {
            case kAstAdtDecl:
                slot = register_adt_item(R, AstGetAdtDecl(item));
                break;
            default:
                slot = register_func_item(R, AstGetFuncDecl(item));
        }
        pawHir_decl_list_push(R->hir, output, slot->hir_decl);
        item_list_push(R->hir, slots, slot);
    }
    *pslots = slots;
    return output;
}

static void resolve_items(struct Resolver *R, struct PartialItemList *items)
{
    for (int i = 0; i < items->count; ++i) {
        struct PartialItem *item = items->data[i];
        if (AstIsFuncDecl(item->ast_decl)) {
            resolve_func_item(R, item);
        }
    }
}

static struct HirDeclList *resolve_module(struct Resolver *R, struct AstDeclList *items)
{
    struct PartialItemList *slots;
    struct HirDeclList *output = register_items(R, items, &slots);
    resolve_items(R, slots);
    return output;
}

static DefId find_builtin(struct Resolver *R, String *name)
{
    const struct HirSymbol *symbol = resolve_symbol(R, name);
    return symbol->decl->hdr.did;
}

static struct HirDeclList *resolve_prelude(struct Resolver *R, struct AstDeclList *prelude)
{
    paw_Env *P = ENV(R);
    struct Hir *hir = R->hir;
    struct HirDeclList *result = resolve_module(R, prelude);
    R->option_did = find_builtin(R, pawE_cstr(P, CSTR_OPTION));
    R->result_did = find_builtin(R, pawE_cstr(P, CSTR_RESULT));
    return result;
}

static void visit_module(struct Resolver *R)
{
    struct Ast *ast = R->ast;
    struct Hir *hir = R->hir;
    struct HirSymtab *symtab = R->symtab;

    enter_block(R, NULL);
    enter_inference_ctx(R);

    hir->prelude = resolve_prelude(R, ast->prelude);
    hir->items = resolve_module(R, ast->items);

    leave_inference_ctx(R);
    symtab->globals = leave_block(R);
    paw_assert(symtab->scopes->count == 0);

    pawHir_expand(R, hir);
}

struct Hir *pawP_resolve(struct Compiler *C, struct Ast *ast)
{
    struct Hir *hir = pawHir_new(C);
    struct Resolver R = {
        .dm = C->dm,
        .ast = ast,
        .hir = hir,
        .symtab = hir->symtab,
        .strings = C->strings,
        .U = &C->dm->unifier,
        .P = ENV(C),
        .C = C,
    };
    C->dm->decls = pawHir_decl_list_new(hir);
    C->dm->unifier.ast = ast;
    C->dm->unifier.hir = hir;
    C->dm->unifier.P = ENV(C);
    C->dm->unifier.R = &R;
    C->dm->hir = hir;
    visit_module(&R);
    return hir;
}
