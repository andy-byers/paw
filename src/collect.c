// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// collect.c: Implementation of the type checker. This code transforms an AST
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

#define NAME_ERROR(c, ...) pawE_error(ENV(c), PAW_ENAME, (c)->line, __VA_ARGS__)
#define SYNTAX_ERROR(c, ...) pawE_error(ENV(c), PAW_ESYNTAX, (c)->line, __VA_ARGS__)
#define TYPE_ERROR(c, ...) pawE_error(ENV(c), PAW_ETYPE, (c)->line, __VA_ARGS__)
#define CACHED_STR(c, i) pawE_cstr(ENV(c), CAST_SIZE(i))
#define TYPE2CODE(c, type) (pawP_type2code((c)->C, type))
#define IS_BUILTIN_DECL(c, decl) ((decl)->hdr.did <= (c)->C->builtins[NBUILTINS - 1].did)
// TODO: IS_BUILTIN_DECL should make sure decl is an adt decl (use CHECK_EXP since it should be known from context)

struct Collector {
    struct HirTypeList 
    paw_Env *P;
};

struct LazyItem {
    struct AstDecl *ast_decl;
    struct HirDecl *hir_decl;
    struct HirSymbol *symbol;
    struct HirScope *scope;
    paw_Bool is_resolved;
};

static struct LazyItem *new_lazy_item(struct Collector *c, struct AstDecl *ad, struct HirDecl *hd, struct HirScope *scope, struct HirSymbol *symbol)
{
    struct LazyItem *item = pawK_pool_alloc(ENV(c), &c->hir->pool, sizeof(struct LazyItem));
    *item = (struct LazyItem){
        .ast_decl = ad,
        .hir_decl = hd,
        .symbol = symbol,
        .scope = scope, 
    };
    return item;
}

DEFINE_LIST(struct Hir, item_list_, LazyItemList, struct LazyItem)

static struct LazyItemList *impls_for_base(struct Collector *c, DefId base)
{
    const Value *pv = pawH_get(c->impls, I2V(base));
    return pv != NULL ? pv->p : NULL;
}

static void map_base_to_impl(struct Collector *c, DefId base, struct LazyItem *item)
{
    paw_Env *P = ENV(c);
    Value *pv = pawH_get(c->impls, I2V(base));
    if (pv == NULL) {
        pv = pawH_create(P, c->impls, I2V(base));
        *pv = P2V(item_list_new(c->hir));
    }
    struct LazyItemList *impls = pv->p;
    item_list_push(c->hir, impls, item);
}

static struct HirStmt *resolve_stmt(struct Collector *, struct AstStmt *);
static struct HirExpr *resolve_expr(struct Collector *, struct AstExpr *);
static struct HirDecl *resolve_decl(struct Collector *, struct AstDecl *);
static struct HirType *resolve_type(struct Collector *, struct AstExpr *);

#define DEFINE_LIST_RESOLVER(name, T, T2) \
    static struct Hir##T2##List *resolve_##name##_list(struct Collector *c, struct Ast##T##List *list) \
    { \
        if (list == NULL) return NULL; \
        struct Hir##T2##List *r = pawHir_##name##_list_new(c->hir); \
        for (int i = 0; i < list->count; ++i) { \
            struct Hir##T2 *node = resolve_##name(c, list->data[i]); \
            pawHir_##name##_list_push(c->hir, r, node); \
        } \
        return r; \
    }
DEFINE_LIST_RESOLVER(expr, Expr, Expr)
DEFINE_LIST_RESOLVER(decl, Decl, Decl)
DEFINE_LIST_RESOLVER(stmt, Stmt, Stmt)
DEFINE_LIST_RESOLVER(type, Expr, Type)

static struct HirType *normalize(struct Collector *c, struct HirType *type)
{
    return pawU_normalize(c->U->table, type);
}

static void unify(struct Collector *c, struct HirType *a, struct HirType *b)
{
    pawU_unify(c->U, a, b);
    normalize(c, a);
    normalize(c, b);
}

static struct HirType *get_type(struct Collector *c, DefId did)
{
    paw_assert(did < c->dm->decls->count);
    return HIR_TYPEOF(c->dm->decls->data[did]);
}

static paw_Bool is_list_t(struct Collector *c, struct HirType *type)
{
    return HirIsAdt(type) && HirGetAdt(type)->base == c->C->builtins[BUILTIN_LIST].did;
}

static paw_Bool is_map_t(struct Collector *c, struct HirType *type)
{
    return HirIsAdt(type) && HirGetAdt(type)->base == c->C->builtins[BUILTIN_MAP].did;
}

static struct HirDecl *get_decl(struct Collector *c, DefId did)
{
    struct DynamicMem *dm = c->dm;
    paw_assert(did < dm->decls->count);
    return dm->decls->data[did];
}

static struct HirBlock *new_block(struct Collector *c, int line)
{
    struct HirStmt *r = pawHir_new_stmt(c->hir, line, kHirBlock);
    return HirGetBlock(r);
}

static struct HirSymbol *new_symbol(struct Collector *c, struct HirDecl *decl)
{
    struct HirSymbol *symbol = pawHir_new_symbol(c->hir);
    *symbol = (struct HirSymbol){
        .is_init = PAW_TRUE,
        .name = decl->hdr.name,
        .decl = decl,
    };
    return symbol;
}

static paw_Bool is_unit_variant(struct Collector *c, const struct HirType *type)
{
    if (HirIsFuncDef(type)) {
        struct HirDecl *decl = get_decl(c, type->fdef.did);
        return HirIsVariantDecl(decl) && 
            HirGetVariantDecl(decl)->fields == NULL;
    }
    return PAW_FALSE;
}

static struct HirAdtDecl *get_adt(struct Collector *c, struct HirType *type)
{
    struct HirAdt *adt = HirGetAdt(type);
    struct HirDecl *decl = get_decl(c, adt->did);
    return HirGetAdtDecl(decl);
}

static struct HirType *resolve_operand(struct Collector *c, struct HirExpr *expr)
{
    struct HirType *type = HIR_TYPEOF(expr);
    if (HirIsFuncDef(type)) {
        // handle unit enumerators
        struct HirDecl *decl = get_decl(c, type->fdef.did);
        if (HirIsVariantDecl(decl)) {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            if (d->fields == NULL) return HIR_FPTR(type)->result;
            TYPE_ERROR(c, "missing fields on enumerator");
        }
    }
    return type;
}

#define ARE_TYPES_SAME(a, b) ((a) == (b))

// TODO: move to unify.c/reuse logic in that file (write idempotent version of
// unify())
static paw_Bool test_types(struct Collector *c, const struct HirType *a, const struct HirType *b);

static paw_Bool test_lists(struct Collector *c, const struct HirTypeList *a, const struct HirTypeList *b)
{
    for (int i = 0; i < a->count; ++i) {
        if (!test_types(c, a->data[i], b->data[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

static paw_Bool test_types(struct Collector *c, const struct HirType *a, const struct HirType *b)
{
    if (HIR_KINDOF(a) != HIR_KINDOF(b)) {
        return PAW_FALSE;
    }
    switch (HIR_KINDOF(a)) {
        case kHirTupleType:
            return test_lists(c, a->tuple.elems, b->tuple.elems);
        case kHirFuncPtr:
        case kHirFuncDef:
            return test_types(c, HIR_FPTR(a)->result, HIR_FPTR(b)->result) &&
                   test_lists(c, HIR_FPTR(a)->params, HIR_FPTR(b)->params);
        case kHirAdt: {
            if (a->adt.base == b->adt.base) {
                if (!a->adt.types == !b->adt.types) {
                    return a->adt.types != NULL
                               ? test_lists(c, a->adt.types, b->adt.types)
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

static DefId add_decl(struct Collector *c, struct HirDecl *decl)
{
    return pawHir_add_decl(c->hir, decl);
}

static struct HirType *new_type(struct Collector *c, DefId did, enum HirTypeKind kind, int line)
{
    struct HirType *type = pawHir_new_type(c->hir, line, kind);
    if (kind == kHirAdt) {
        type->adt.did = did;
    } else if (kind == kHirFuncDef) {
        type->fdef.did = did;
    } else if (kind == kHirGeneric) {
        type->generic.did = did;
    }
    if (did != NO_DECL) {
        // set type of associated definition
        struct HirDecl *d = get_decl(c, did);
        d->hdr.type = type;
    }
    return type;
}

static struct HirTypeList *collect_expr_types(struct Collector *c, struct HirExprList *list) 
{                                                                                   
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(c->hir);
    for (int i = 0; i < list->count; ++i) {                                         
        struct HirType *type = resolve_operand(c, list->data[i]);
        pawHir_type_list_push(c->hir, new_list, type);
    }                                                                               
    return new_list;
}

static struct HirTypeList *collect_decl_types(struct Collector *c, struct HirDeclList *list) 
{                                                                                   
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(c->hir);
    for (int i = 0; i < list->count; ++i) {                                         
        struct HirType *type = HIR_TYPEOF(list->data[i]);
        pawHir_type_list_push(c->hir, new_list, type);
    }                                                                               
    return new_list;
}

static void enter_inference_ctx(struct Collector *c)
{
    pawU_enter_binder(c->U);
}

static void leave_inference_ctx(struct Collector *c) 
{
    pawU_leave_binder(c->U); 
}

static struct HirScope *enclosing_scope(struct HirSymtab *st)
{
    paw_assert(st->count > 0);
    return pawHir_symtab_get(st, st->count - 1);
}

static struct HirSymbol *add_symbol(struct Collector *c, struct HirScope *scope, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = pawHir_add_symbol(c->hir, scope);
    symbol->name = name;
    symbol->decl = decl;
    return symbol;
}

static struct HirSymbol *declare_local(struct Collector *c, String *name, struct HirDecl *decl)
{
    return add_symbol(c, enclosing_scope(c->symtab), name, decl);
}

// Allow a previously-declared variable to be accessed
static void define_local(struct HirSymbol *symbol) 
{ 
    symbol->is_init = PAW_TRUE; 
}

static struct HirSymbol *new_global(struct Collector *c, String *name, struct HirDecl *decl)
{
    struct HirScope *scope = c->globals;
    for (int i = 0; i < scope->count; ++i) {
        struct HirSymbol *symbol = scope->data[i];
        if (pawS_eq(symbol->name, name)) {
            NAME_ERROR(c, "duplicate global '%s' (declared previously on line %d)",
                       name->text, symbol->decl->hdr.line);
        }
    }
    struct HirSymbol *symbol = add_symbol(c, scope, name, decl);
    symbol->is_init = PAW_TRUE;
    return symbol;
}

static void resolve_item(struct Collector *c, struct LazyItem *item);

static struct HirSymbol *try_resolve_symbol(struct Collector *c, const String *name)
{
    // search the scoped symbols
    struct HirSymtab *scopes = c->symtab;
    const int nscopes = scopes->count;
    for (int depth = nscopes - 1; depth >= 0; --depth) {
        struct HirScope *scope = scopes->data[depth];
        const int index = pawHir_find_symbol(scope, name);
        if (index >= 0) {
            struct HirSymbol *symbol = scope->data[index];
            return scope->data[index];
        }
    }

    for (int i = 0; i < c->items->count; ++i) {
        struct LazyItem *item = c->items->data[i];
        if (pawS_eq(name, item->hir_decl->hdr.name)) {
            resolve_item(c, item);
            return item->symbol;
        }
    }

    // search the global symbols
    const int index = pawHir_find_symbol(c->globals, name);
    if (index < 0) return NULL;
    return c->globals->data[index];
}

static struct HirSymbol *resolve_symbol(struct Collector *c, const String *name)
{
    struct HirSymbol *symbol = try_resolve_symbol(c, name);
    if (symbol == NULL) NAME_ERROR(c, "undefined symbol '%s'", name->text);
    return symbol;
}

static struct HirDecl *find_field(struct HirDeclList *fields, String *name)
{
    if (fields == NULL) return NULL;
    for (int i = 0; i < fields->count; ++i) {
        struct HirDecl *decl = pawHir_decl_list_get(fields, i);
        if (pawS_eq(name, decl->hdr.name)) return decl;
    }
    return NULL;
}

// Return true if 'a' is more generic than or equal to 'b', false otherwise
static paw_Bool is_compat(struct Collector *c, struct HirType *a, struct HirType *b);

static paw_Bool are_lists_compat(struct Collector *c, struct HirTypeList *a, struct HirTypeList *b)
{
    paw_assert(a->count == b->count);
    for (int i = 0; i < a->count; ++i) {
        struct HirType *x = pawHir_type_list_get(a, i); 
        struct HirType *y = pawHir_type_list_get(b, i); 
        if (!is_compat(c, x, y)) return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool is_compat(struct Collector *c, struct HirType *a, struct HirType *b)
{
    if (test_types(c, a, b)) return PAW_TRUE;

    switch (HIR_KINDOF(a)) {
        case kHirTupleType:
            return are_lists_compat(c, a->tuple.elems, b->tuple.elems);
        case kHirFuncPtr:
        case kHirFuncDef:
            return is_compat(c, HIR_FPTR(a)->result, HIR_FPTR(b)->result) &&
                   are_lists_compat(c, HIR_FPTR(a)->params, HIR_FPTR(b)->params);
        case kHirAdt: {
            if (a->adt.base != b->adt.base) break;
            return are_lists_compat(c, a->adt.types, b->adt.types);
        }
        case kHirGeneric:
            return PAW_TRUE;
        default:
            paw_assert(HirIsUnknown(a));
            return ARE_TYPES_SAME(a, b);
    }
    return PAW_FALSE;
}

static struct HirSymbol *new_local(struct Collector *c, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = declare_local(c, name, decl);
    define_local(symbol);
    return symbol;
}

static struct HirScope *leave_block(struct Collector *c)
{
    struct HirSymtab *st = c->symtab;
    struct HirScope *scope = enclosing_scope(st);
    --st->count;
    return scope;
}

static void enter_block(struct Collector *c, struct HirScope *scope)
{
    if (scope == NULL) scope = pawHir_scope_new(c->hir);
    pawHir_symtab_push(c->hir, c->symtab, scope);
}

static struct HirScope *leave_function(struct Collector *c)
{
    struct HirScope *scope = leave_block(c);
    CHECK_GC(ENV(c));
    return scope;
}

static void create_context(struct Collector *c, struct HirAdtDecl *adt, int line)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, line, kHirVarDecl);
    struct HirVarDecl *r = HirGetVarDecl(result);
    r->name = pawE_cstr(ENV(c), CSTR_SELF); // 'self'
    r->type = adt->type;

    new_local(c, r->name, result);
    add_decl(c, result);
}

static void enter_function(struct Collector *c, struct HirScope *scope, struct HirFuncDecl *func)
{
    enter_block(c, scope);
    new_local(c, func->name, HIR_CAST_DECL(func));
    if (func->fn_kind == FUNC_METHOD) {
        // methods use local slot 1 for the implicit context variable
        struct HirAdtDecl *adt = get_adt(c, func->self);
        create_context(c, adt, func->line);
    }
}

static struct HirStmt *ResolveBlock(struct Collector *c, struct AstBlock *block)
{
    struct HirStmt *result = pawHir_new_stmt(c->hir, block->line, kHirBlock);
    struct HirBlock *r = HirGetBlock(result);
    enter_block(c, NULL);
    r->stmts = resolve_stmt_list(c, block->stmts);
    leave_block(c);
    return result;
}

#define RESOLVE_BLOCK(c, block) HirGetBlock(ResolveBlock(c, block))

static struct HirType *register_decl_type(struct Collector *c, struct HirDecl *decl, enum HirTypeKind kind)
{
    const DefId did = add_decl(c, decl);
    struct HirType *r = new_type(c, did, kind, decl->hdr.line);
    decl->hdr.type = r;
    return r;
}

static struct HirType *register_variant(struct Collector *c, struct HirVariantDecl *d)
{
    // An enum variant name can be thought of as a function from the type of the
    // variant's fields to the type of the enumeration. For example, given 'enum
    // E {X(str)}', E::X has type 'fn(str) -> E'.
    paw_assert(d->type == NULL);
    d->type = register_decl_type(c, HIR_CAST_DECL(d), kHirFuncDef);
    struct HirFuncDef *t = HirGetFuncDef(d->type);
    t->base = HirGetAdt(c->adt)->did;
    t->params = d->fields != NULL
        ? collect_decl_types(c, d->fields)
        : pawHir_type_list_new(c->hir);
    t->result = c->adt;
    return d->type;
}

static void allocate_decls(struct Collector *c, struct HirDeclList *decls)
{
    if (decls == NULL) return;
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = decls->data[i];
        new_local(c, decl->hdr.name, decl);
    }
}

typedef void (*Instantiate)(struct Collector *, struct HirTypeList *before, struct HirTypeList *after, struct HirDecl *);

static struct HirType *instantiate_type(struct Collector *c, struct HirTypeList *before,
                                        struct HirTypeList *after, struct HirType *target);

static void instantiate_field(struct Collector *c, struct HirTypeList *before, struct HirTypeList *after, struct HirDecl *decl)
{
    decl->hdr.type = instantiate_type(c, before, after, HIR_TYPEOF(decl));
    new_local(c, decl->hdr.name, decl);
    add_decl(c, decl);
}

static void instantiate_variant(struct Collector *c, struct HirTypeList *before, struct HirTypeList *after, struct HirDecl *decl)
{
    struct HirVariantDecl *r = HirGetVariantDecl(decl);
    instantiate_field(c, before, after, decl);
    HirGetFuncDef(r->type)->did = r->did;
    if (r->fields == NULL) return;

    enter_block(c, NULL);
    for (int j = 0; j < r->fields->count; ++j) {
        struct HirDecl *field = r->fields->data[j];
        instantiate_field(c, before, after, field);
    }
    leave_block(c);
}

static struct HirDeclList *instantiate_fields(struct Collector *c, struct HirTypeList *before, struct HirTypeList *after, struct HirDeclList *list, Instantiate callback)
{
    if (list == NULL) return NULL;
    struct HirDeclList *copy = pawHir_decl_list_new(c->hir);
    for (int i = 0; i < list->count; ++i) {
        struct HirDecl *source = list->data[i];
        struct HirDecl *target = pawHir_copy_decl(c->hir, source);
        callback(c, before, after, target);
        pawHir_decl_list_push(c->hir, copy, target);
    }
    return copy;
}

static struct HirDecl *find_func_instance(struct Collector *c, struct HirFuncDecl *base, struct HirTypeList *types)
{
    if (test_lists(c, types, HirGetFuncDef(base->type)->types)) {
        return HIR_CAST_DECL(base);
    }
    for (int i = 0; i < base->monos->count; ++i) {
        struct HirDecl *inst = base->monos->data[i];
        const struct HirType *type = HIR_TYPEOF(inst);
        if (test_lists(c, types, type->fdef.types)) {
            return inst;
        }
    }
    return NULL;
}

static struct HirDecl *find_adt_instance(struct Collector *c, struct HirAdtDecl *base, struct HirTypeList *types)
{
    if (test_lists(c, types, HirGetAdt(base->type)->types)) {
        return HIR_CAST_DECL(base);
    }
    for (int i = 0; i < base->monos->count; ++i) {
        struct HirDecl *inst = base->monos->data[i];
        const struct HirType *type = HIR_TYPEOF(inst);
        if (test_lists(c, types, type->adt.types)) {
            return inst;
        }
    }
    return NULL;
}

static struct HirDecl *find_impl_instance(struct Collector *c, struct HirImplDecl *base, struct HirTypeList *types)
{
    if (test_lists(c, types, base->subst)) {
        return HIR_CAST_DECL(base);
    }
    for (int i = 0; i < base->monos->count; ++i) {
        struct HirDecl *inst = base->monos->data[i];
        struct HirImplDecl *d = HirGetImplDecl(inst);
        if (test_lists(c, types, d->subst)) return inst;
    }
    return NULL;
}

static struct HirDecl *instantiate_adt(struct Collector *, struct HirAdtDecl *, struct HirTypeList *);
static struct HirDecl *instantiate_func(struct Collector *, struct HirFuncDecl *, struct HirTypeList *);

struct Subst {
    struct HirTypeList *before;
    struct HirTypeList *after;
    struct Collector *c;
};

#define MAYBE_SUBST_LIST(F, list) ((list) != NULL ? subst_list(F, list) : NULL)

static struct HirTypeList *subst_list(struct HirFolder *F, struct HirTypeList *list)
{
    struct Subst *subst = F->ud;
    struct Collector *c = subst->c;
    struct HirTypeList *copy = pawHir_type_list_new(c->hir);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = F->FoldType(F, list->data[i]);
        pawHir_type_list_push(c->hir, copy, type);
    }
    return copy;
}

static struct HirType *subst_fptr(struct HirFolder *F, struct HirFuncPtr *t)
{
    struct Subst *subst = F->ud;
    struct Collector *c = subst->c;
    struct HirType *r = new_type(c, NO_DECL, kHirFuncPtr, t->line);
    r->fptr.params = subst_list(F, t->params);
    r->fptr.result = F->FoldType(F, t->result);
    return r;
}

static struct HirType *subst_fdef(struct HirFolder *F, struct HirFuncDef *t)
{
    struct Subst *subst = F->ud;
    struct Collector *c = subst->c;

    if (t->types == NULL) {
        struct HirType *result = pawHir_new_type(c->hir, t->line, kHirFuncDef);
        struct HirFuncDef *r = HirGetFuncDef(result);
        r->params = subst_list(F, t->params);
        r->result = F->FoldType(F, t->result);
        r->did = NO_DECL; // set later for variants
        r->base = t->did;
        return result;
    }
    struct HirTypeList *types = subst_list(F, t->types);
    struct HirFuncDecl *base = HirGetFuncDecl(get_decl(c, t->base));
    struct HirDecl *inst = instantiate_func(c, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *subst_adt(struct HirFolder *F, struct HirAdt *t)
{
    struct Subst *subst = F->ud;
    struct Collector *c = subst->c;

    if (t->types == NULL) return HIR_CAST_TYPE(t);
    struct HirTypeList *types = subst_list(F, t->types);
    struct HirAdtDecl *base = HirGetAdtDecl(get_decl(c, t->base));
    struct HirDecl *inst = instantiate_adt(c, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *subst_tuple(struct HirFolder *F, struct HirTupleType *t)
{
    struct Subst *subst = F->ud;
    struct Collector *c = subst->c;

    struct HirType *result = new_type(c, NO_DECL, kHirTupleType, t->line);
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

static void init_subst_folder(struct HirFolder *F, struct Subst *subst, struct Collector *c, 
                              struct HirTypeList *before, struct HirTypeList *after)
{
    *subst = (struct Subst){
        .before = before,
        .after = after,
        .c = c,
    };
    pawHir_folder_init(F, c->hir, subst);
    F->FoldAdt = subst_adt;
    F->FoldFuncPtr = subst_fptr;
    F->FoldFuncDef = subst_fdef;
    F->FoldGeneric = subst_generic;
    F->FoldUnknown = subst_unknown;
    F->FoldTupleType = subst_tuple;
    F->FoldTypeList = subst_list;
}

static struct HirTypeList *instantiate_typelist(struct Collector *c, struct HirTypeList *before,
                                                struct HirTypeList *after, struct HirTypeList *target)
{
    struct Subst subst;
    struct HirFolder F;
    init_subst_folder(&F, &subst, c, before, after);
    return F.FoldTypeList(&F, target);
}

static struct HirType *instantiate_type(struct Collector *c, struct HirTypeList *before,
                                        struct HirTypeList *after, struct HirType *target)
{
    struct Subst subst;
    struct HirFolder F;
    init_subst_folder(&F, &subst, c, before, after);
    return F.FoldType(&F, target);
}

static void prep_func_instance(struct Collector *c, struct HirTypeList *before,
                               struct HirTypeList *after, struct HirInstanceDecl *d)
{
    struct Subst subst;
    struct HirFolder F;
    init_subst_folder(&F, &subst, c, before, after);

    struct HirFuncDef *fdef = HirGetFuncDef(d->type);
    struct HirTypeList *params = fdef->params;
    for (int i = 0; i < params->count; ++i) {
        params->data[i] = F.FoldType(&F, params->data[i]);
    }
    fdef->result = F.FoldType(&F, fdef->result);
}

static void instantiate_func_aux(struct Collector *c, struct HirFuncDecl *base, struct HirInstanceDecl *inst, struct HirTypeList *types)
{
    enter_block(c, NULL);
    inst->name = base->name;
    inst->types = types;

    struct HirType *r = register_decl_type(c, HIR_CAST_DECL(inst), kHirFuncDef);
    r->fdef.base = base->did;
    r->fdef.params = collect_decl_types(c, base->params);
    r->fdef.result = HIR_FPTR(base->type)->result;
    r->fdef.types = types;
    inst->type = r;

    struct HirTypeList *generics = HirGetFuncDef(base->type)->types;
    prep_func_instance(c, generics, types, inst);

    leave_block(c);
}

// TODO: polymorphic methods
//static struct HirDecl *instantiate_method(struct Collector *c, struct HirFuncDecl *base, struct HirTypeList *types)
//{
//    struct HirDecl *inst = pawHir_new_decl(c->hir, base->line, kHirInstanceDecl);
//    instantiate_func_aux(c, base, HirGetInstanceDecl(inst), types);
//    return inst;
//}

static void instantiate_adt_aux(struct Collector *c, struct HirAdtDecl *base,
                                   struct HirAdtDecl *inst, struct HirTypeList *types)
{
    enter_block(c, NULL);
    inst->name = base->name;
    inst->is_pub = base->is_pub;
    inst->is_struct = base->is_struct;

    struct HirType *r = register_decl_type(c, HIR_CAST_DECL(inst), kHirAdt);
    r->adt.base = base->did;
    r->adt.types = types;
    inst->type = r;

    struct HirType *enclosing = c->adt;
    c->adt = inst->type;

    struct HirTypeList *generics = HirGetAdt(base->type)->types;
    inst->type = instantiate_type(c, generics, types, inst->type);

    enter_block(c, NULL);
    Instantiate callback = base->is_struct ? instantiate_field : instantiate_variant;
    inst->fields = instantiate_fields(c, generics, types, base->fields, callback);
    leave_block(c);

    leave_block(c);
    c->adt = enclosing;
}

static struct HirDeclList *register_generics(struct Collector *c, struct AstDeclList *generics)
{
    struct HirDeclList *list = pawHir_decl_list_new(c->hir);
    for (int i = 0; i < generics->count; ++i) {
        struct HirDecl *decl = resolve_decl(c, generics->data[i]);
        struct HirGenericDecl *gen = HirGetGenericDecl(decl);
        struct HirSymbol *symbol = new_local(c, gen->name, decl);
        pawHir_decl_list_push(c->hir, list, decl);
        symbol->is_type = PAW_TRUE;
        symbol->is_generic = PAW_TRUE;
    }
    return list;
}

static void check_template_param(struct Collector *c, struct HirDeclList *params, struct HirTypeList *args)
{
    if (args->count > params->count) {
        TYPE_ERROR(c, "too many generics");
    } else if (args->count < params->count) {
        TYPE_ERROR(c, "not enough generics");
    }
}

static void normalize_type_list(struct Collector *c, struct HirTypeList *types)
{
    for (int i = 0; i < types->count; ++i) {
        normalize(c, types->data[i]);
    }
}

static struct HirDecl *instantiate_impl_method(struct Collector *c, struct HirFuncDecl *func, struct HirTypeList *generics, struct HirTypeList *types)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, func->line, kHirInstanceDecl);
    struct HirInstanceDecl *r = HirGetInstanceDecl(result);
    r->name = func->name;
    r->types = types;

    struct HirType *type = register_decl_type(c, result, kHirFuncDef);
    struct HirFuncDef *t = HirGetFuncDef(type);
    t->types = collect_decl_types(c, func->generics);
    t->params = collect_decl_types(c, func->params);
    t->result = HIR_FPTR(func->type)->result;
    t->base = func->did;
    r->type = type;

    prep_func_instance(c, generics, types, r);
    return result;
}

static struct HirDecl *instantiate_impl_aux(struct Collector *c, struct HirImplDecl *base, struct HirTypeList *types, struct HirType *adt)
{
    
    struct HirDecl *result = find_impl_instance(c, base, types);
    if (result != NULL) return result;
    result = pawHir_new_decl(c->hir, base->line, kHirImplDecl);
    struct HirImplDecl *r = HirGetImplDecl(result);
    r->methods = pawHir_decl_list_new(c->hir);
    r->name = base->name;
    r->subst = types;
    r->type = adt;

    struct HirTypeList *generics = collect_decl_types(c, base->generics);
    paw_assert(generics->count == types->count);
    for (int i = 0; i < base->methods->count; ++i) {
        struct HirDecl *src = pawHir_decl_list_get(base->methods, i);
        struct HirDecl *dst = instantiate_impl_method(c, HirGetFuncDecl(src), generics, types); 
        pawHir_decl_list_push(c->hir, r->methods, dst);
    }
    pawHir_decl_list_push(c->hir, base->monos, result);
    return result;
}

static struct HirDecl *instantiate_adt(struct Collector *c, struct HirAdtDecl *base, struct HirTypeList *types)
{
    check_template_param(c, base->generics, types);
    normalize_type_list(c, types);
    struct HirDecl *inst = find_adt_instance(c, base, types);
    if (inst != NULL) return inst;
    inst = pawHir_new_decl(c->hir, base->line, kHirAdtDecl);
    pawHir_decl_list_push(c->hir, base->monos, inst);
    instantiate_adt_aux(c, base, &inst->adt, types);
    return inst;
}


static struct HirDecl *instantiate_func(struct Collector *c, struct HirFuncDecl *base, struct HirTypeList *types)
{
    check_template_param(c, base->generics, types);
    normalize_type_list(c, types);
    struct HirDecl *inst = find_func_instance(c, base, types);
    if (inst != NULL) return inst;
    inst = pawHir_new_decl(c->hir, base->line, kHirInstanceDecl);
    pawHir_decl_list_push(c->hir, base->monos, inst);
    instantiate_func_aux(c, base, &inst->inst, types);
    return inst;
}

// TODO: move things around, no need for most forward decls
static struct HirDecl *instantiate_impl(struct Collector *c, struct HirImplDecl *base, struct HirTypeList *types);
struct HirDecl *pawP_instantiate(struct Collector *c, struct HirDecl *base, struct HirTypeList *types)
{
    if (types == NULL) return base; 
    if (HIR_IS_POLY_ADT(base)) {
        return instantiate_adt(c, &base->adt, types);
    } else if (HIR_IS_POLY_FUNC(base)) {
        return instantiate_func(c, &base->func, types);
    } else if (HIR_IS_POLY_IMPL(base)) {
        return instantiate_impl(c, &base->impl, types);
    }
    return base;
}

static struct HirScope *register_func(struct Collector *c, struct AstFuncDecl *d, struct HirFuncDecl *r)
{
    r->is_pub = d->is_pub;
    r->fn_kind = d->fn_kind;
    r->name = d->name;

    enter_block(c, NULL);
    if (d->generics != NULL) {
        r->generics = register_generics(c, d->generics);
        r->monos = pawHir_decl_list_new(c->hir);
    }
    r->params = resolve_decl_list(c, d->params);

    struct HirType *type = register_decl_type(c, HIR_CAST_DECL(r), kHirFuncDef);
    struct HirFuncDef *t = HirGetFuncDef(type);
    t->types = collect_decl_types(c, r->generics);
    t->params = collect_decl_types(c, r->params);
    t->result = resolve_type(c, d->result);
    t->base = r->did;
    r->type = type;

    return leave_block(c);
}

static struct HirDecl *ResolveFieldDecl(struct Collector *c, struct AstFieldDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirFieldDecl);
    struct HirFieldDecl *r = HirGetFieldDecl(result);
    add_decl(c, result);

    r->name = d->name == NULL
        ? SCAN_STRING(c, "(field)")
        : d->name;
    r->type = resolve_type(c, d->tag);
    return result;
}

static struct HirDecl *ResolveVariantDecl(struct Collector *c, struct AstVariantDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirVariantDecl);
    struct HirVariantDecl *r = HirGetVariantDecl(result);
    r->index = d->index;
    r->name = d->name;

    enter_block(c, NULL);
    r->fields = resolve_decl_list(c, d->fields);
    allocate_decls(c, r->fields);
    leave_block(c);
    new_local(c, d->name, result);

    r->type = register_variant(c, r);
    return result;
}

static void dupcheck(struct Collector *c, Map *map, String *name, const char *what)
{
    Value k = {.p = name};
    Value *pv = pawH_get(map, k);
    if (pv != NULL) NAME_ERROR(c, "duplicate %s '%s'", what, name->text);
    pawH_insert(ENV(c), map, k, k);
}

static struct HirDeclList *resolve_fields(struct Collector *c, struct AstDeclList *src, String *parent)
{
    paw_Env *P = ENV(c);
    ENSURE_STACK(P, 1);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    V_SET_OBJECT(pv, map);

    struct HirDeclList *dst = pawHir_decl_list_new(c->hir);
    for (int i = 0; i < src->count; ++i) {
        struct HirDecl *decl = resolve_decl(c, pawAst_decl_list_get(src, i));
        String *name = decl->hdr.name;
        dupcheck(c, map, name, "field");
        pawHir_decl_list_push(c->hir, dst, decl);
    }

    pawC_pop(P);
    return dst;
}

static struct HirScope *register_adt(struct Collector *c, struct AstAdtDecl *d, struct HirAdtDecl *r)
{
    struct HirType *t = register_decl_type(c, HIR_CAST_DECL(r), kHirAdt);
    struct HirType *enclosing = c->adt;
    c->adt = t;
    enter_block(c, NULL);

    if (d->generics != NULL) {
        r->generics = register_generics(c, d->generics);
        r->monos = pawHir_decl_list_new(c->hir);
    }

    struct HirScope *scope = leave_block(c);
    c->adt = enclosing;

    t->adt.types = collect_decl_types(c, r->generics);
    t->adt.base = r->did;
    r->type = t;
    return scope;
}

static void resolve_adt_fields(struct Collector *c, struct AstAdtDecl *d, struct HirAdtDecl *r, struct HirScope *scope)
{
    if (d->fields == NULL) return;
    struct HirType *enclosing = c->adt;
    c->adt = r->type;
    enter_block(c, scope);

    r->fields = resolve_fields(c, d->fields, d->name);
    allocate_decls(c, r->fields);

    leave_block(c);
    c->adt = enclosing;
}

static void resolve_func(struct Collector *c, struct AstFuncDecl *d, struct HirFuncDecl *r, struct HirScope *scope)
{
    enter_function(c, scope, r);
    allocate_decls(c, r->params);

    if (d->body != NULL) {
        struct HirType *last_result = c->result;
        const int last_count = c->nresults;
        c->result = HIR_FPTR(r->type)->result;
        c->nresults = 0;

        enter_inference_ctx(c);
        r->body = RESOLVE_BLOCK(c, d->body);
        leave_inference_ctx(c);

        c->nresults = last_count;
        c->result = last_result;
    }
    leave_function(c);
}

static struct HirStmt *ResolveReturnStmt(struct Collector *c, struct AstReturnStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(c->hir, s->line, kHirReturnStmt);
    struct HirReturnStmt *r = HirGetReturnStmt(result);

    struct HirType *want = c->result;
    struct HirType *have = NULL;
    if (s->expr != NULL) {
        r->expr = resolve_expr(c, s->expr); 
        have = resolve_operand(c, r->expr);
    } else {
        have = get_type(c, PAW_TUNIT);
    }

    unify(c, have, want);
    ++c->nresults;
    return result;
}

static struct HirScope *register_method(struct Collector *c, struct AstFuncDecl *d, struct HirType *self)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirFuncDecl);
    struct HirFuncDecl *r = HirGetFuncDecl(result);
    r->self = self;
    return register_func(c, d, r);
}

static struct LazyItemList *register_methods(struct Collector *c, struct AstDeclList *src, struct HirDeclList **pdst, struct HirType *self)
{
    paw_Env *P = ENV(c);
    ENSURE_STACK(P, 1);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    V_SET_OBJECT(pv, map);

    *pdst = pawHir_decl_list_new(c->hir);
    struct LazyItemList *out = item_list_new(c->hir);
    for (int i = 0; i < src->count; ++i) {
        struct AstDecl *decl = pawAst_decl_list_get(src, i);
        struct AstFuncDecl *d = AstGetFuncDecl(decl);
        struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirFuncDecl);
        struct HirFuncDecl *r = HirGetFuncDecl(result);
        pawHir_decl_list_push(c->hir, *pdst, result);
        r->self = self;

        struct HirScope *scope = register_func(c, d, r);
        dupcheck(c, map, d->name, "method");
        struct LazyItem *item = new_lazy_item(c, decl, result, scope, NULL);
        item_list_push(c->hir, out, item);
    }

    pawC_pop(P);
    return out;
}

static void resolve_methods(struct Collector *c, struct LazyItemList *items, struct HirAdt *self)
{
    for (int i = 0; i < items->count; ++i) {
        struct LazyItem *item = item_list_get(items, i);
        resolve_item(c, item);
    }
}

static struct HirDecl *resolve_location(struct Collector *c, struct HirPath *path); // TODO
                                                                                   
static void resolve_impl_methods(struct Collector *c, struct AstImplDecl *d, struct HirImplDecl *r, struct HirScope *scope)
{
    enter_block(c, scope);

    // Lookup the 'self' ADT. If the ADT is an instance of a polymorphic ADT, it
    // may need to be instantiated here. Either way, the LazyItem holding the ADT
    // will be resolved (this is fine, since impl blocks are resolved in a separate
    // pass, after all ADTs have been registered).
    // use the identifier 'Self' to refer to the 'self' ADT
    String *name = SCAN_STRING(c, "Self");
    struct HirAdtDecl *self = get_adt(c, r->type);
    struct HirSymbol *symbol = new_local(c, name, HIR_CAST_DECL(self));
    symbol->is_type = PAW_TRUE;

    // first register signatures, then collect bodies (which may refer to other methods
    // in the impl block)
    struct LazyItemList *items = register_methods(c, d->methods, &r->methods, r->type);
    resolve_methods(c, items, HirGetAdt(r->type));
    allocate_decls(c, r->methods);

    leave_block(c);
}

static struct HirDecl *find_method(struct Collector *c, struct HirAdt *adt, String *name);

static struct HirDecl *expect_field(struct Collector *c, struct HirAdtDecl *adt, String *name)
{
    struct HirDecl *field = find_field(adt->fields, name);
    if (field == NULL) field = find_method(c, HirGetAdt(adt->type), name);
    if (field == NULL) NAME_ERROR(c, "field '%s' does not exist on type '%s'", 
                           name->text, adt->name->text);
    return field;
}

static struct HirExpr *expect_bool_expr(struct Collector *c, struct AstExpr *e)
{
    struct HirExpr *r = resolve_expr(c, e);
    unify(c, resolve_operand(c, r), get_type(c, PAW_TBOOL));
    return r;
}

static struct HirExpr *expect_int_expr(struct Collector *c, struct AstExpr *e)
{
    struct HirExpr *r = resolve_expr(c, e);
    unify(c, resolve_operand(c, r), get_type(c, PAW_TINT));
    return r;
}

static struct HirType *resolve_tuple_type(struct Collector *c, struct AstTupleType *e)
{
    if (e->types->count == 0) return get_type(c, PAW_TUNIT);
    struct HirType *r = new_type(c, NO_DECL, kHirTupleType, e->line);
    r->tuple.elems = resolve_type_list(c, e->types);
    return r;
}

static struct HirPath *resolve_path(struct Collector *c, struct AstPath *path)
{
    paw_assert(path->count > 0);
    struct HirPath *r = pawHir_path_new(c->hir);
    for (int i = 0; i < path->count; ++i) {
        struct AstSegment *src = pawAst_path_get(path, i);
        struct HirSegment *dst = pawHir_segment_new(c->hir);
        dst->types = resolve_type_list(c, src->types);
        dst->name = src->name;
        pawHir_path_push(c->hir, r, dst);
    }
    return r;
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct HirDecl *resolve_base_seg(struct Collector *c, struct HirSegment *base)
{
    struct HirSymbol *symbol = resolve_symbol(c, base->name);
    struct HirType *type = HIR_TYPEOF(symbol->decl);
    base->result = pawP_instantiate(c, symbol->decl, base->types);
    return base->result;
}

static struct HirDecl *resolve_next_seg(struct Collector *c, struct HirDecl *prev, struct HirSegment *next)
{
    if (!is_enum_decl(prev)) TYPE_ERROR(c, "expected enumerator");
    if (next->types != NULL) TYPE_ERROR(c, "expected generic item");
    struct HirAdtDecl *adt = HirGetAdtDecl(prev);
    next->result = expect_field(c, adt, next->name);
    return next->result;
}

static struct HirDecl *resolve_location(struct Collector *c, struct HirPath *path)
{
    paw_assert(path->count > 0);
    struct HirSegment *seg = pawHir_path_get(path, 0);
    struct HirDecl *decl = resolve_base_seg(c, seg);
    for (int i = 1; i < path->count; ++i) {
        seg = pawHir_path_get(path, i);
        decl = resolve_next_seg(c, decl, seg);
    }
    // NOTE: Paw does not support associated types or static fields, etc., so it
    //       is not possible to have a path of length > 2. Such a path should
    //       cause a type error in the above loop.
    paw_assert(path->count < 3);
    return decl;
}

static void maybe_fix_unit_struct(struct Collector *c, struct HirDecl *decl, struct HirExpr *expr)
{
    if (HirIsAdtDecl(decl)) {
        struct HirAdtDecl *adt = HirGetAdtDecl(decl);
        if (adt->did <= PAW_TSTR) {
            goto not_operand;
        }
        if (!adt->is_struct) {
            SYNTAX_ERROR(c, "missing variant specifier on enum '%s'", adt->name->text); 
        }
        if (adt->fields != NULL) {
            SYNTAX_ERROR(c, "missing fields on initializer for struct '%s'", adt->name->text);
        }
        struct HirPath *path = HirGetPathExpr(expr)->path;
        expr->hdr.kind = kHirLiteralExpr;
        struct HirLiteralExpr *r = HirGetLiteralExpr(expr);
        r->lit_kind = kHirLitComposite;
        r->comp.items = pawHir_expr_list_new(c->hir);
        r->comp.path = path;
    } else if (HirIsGenericDecl(decl)) {
not_operand:
        TYPE_ERROR(c, "expected operand but found type");
    }
}

static struct HirExpr *resolve_path_expr(struct Collector *c, struct AstPathExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirPathExpr);
    struct HirPathExpr *r = HirGetPathExpr(result);
    r->path = resolve_path(c, e->path);

    struct HirDecl *decl = resolve_location(c, r->path);
    r->type = HIR_TYPEOF(decl);

    maybe_fix_unit_struct(c, decl, result);
    return result;
}

static struct HirType *resolve_path_type(struct Collector *c, struct AstPathExpr *e)
{
    struct HirPath *path = resolve_path(c, e->path);
    struct HirDecl *decl = resolve_location(c, path);
    if (HirIsVarDecl(decl) || HirIsFuncDecl(decl)) {
        TYPE_ERROR(c, "'%s' is not a type", decl->hdr.name->text);
    }
    return HIR_TYPEOF(decl);
}

static struct HirExpr *resolve_logical_expr(struct Collector *c, struct AstLogicalExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirLogicalExpr);
    struct HirLogicalExpr *r = HirGetLogicalExpr(result);
    r->is_and = e->is_and;
    r->lhs = expect_bool_expr(c, e->lhs);
    r->rhs = expect_bool_expr(c, e->rhs);
    r->type = get_type(c, PAW_TBOOL);
    return result;
}

static paw_Bool is_option_t(struct Collector *c, const struct HirType *type)
{
    return HirIsAdt(type) && type->adt.base == c->C->builtins[BUILTIN_OPTION].did;
}

static paw_Bool is_result_t(struct Collector *c, const struct HirType *type)
{
    return HirIsAdt(type) && type->adt.base == c->C->builtins[BUILTIN_RESULT].did;
}

static struct HirExpr *resolve_chain_expr(struct Collector *c, struct AstChainExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirChainExpr);
    struct HirChainExpr *r = HirGetChainExpr(result);
    r->target = resolve_expr(c, e->target);

    struct HirType *type = resolve_operand(c, r->target);
    if (c->result == NULL) {
        SYNTAX_ERROR(c, "'?' outside function body");
    }
    if (is_option_t(c, type) || is_result_t(c, type)) {
        r->type = HirGetAdt(type)->types->data[0];
    } else {
        SYNTAX_ERROR(c, "invalid operand for '?' operator");
    }
    unify(c, c->result, type);
    return result;
}

static struct HirType *get_value_type(struct Collector *c, struct HirType *target)
{
    if (is_list_t(c, target)) {
        return hir_list_elem(target);
    } else if (is_map_t(c, target)) {
        return hir_map_value(target);
    }
    return NULL;
}

static struct HirExpr *resolve_unop_expr(struct Collector *c, struct AstUnOpExpr *e)
{
    static const uint8_t kValidOps[NUNARYOPS][PAW_NTYPES + 2] = {
        //     type  =  0, b, i, f, s, l, m
        [UNARY_LEN]  = {0, 0, 0, 0, 1, 1, 1},
        [UNARY_NEG]  = {0, 0, 1, 1, 0, 0, 0}, 
        [UNARY_NOT]  = {0, 1, 1, 1, 0, 0, 0}, 
        [UNARY_BNOT] = {0, 0, 1, 0, 0, 0, 0}, 
    };

    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirUnOpExpr);
    struct HirUnOpExpr *r = HirGetUnOpExpr(result);
    r->target = resolve_expr(c, e->target);
    r->op = e->op;

    struct HirType *type = resolve_operand(c, r->target);
    const paw_Type code = TYPE2CODE(c, type);
    if (code < 0 || !kValidOps[e->op][code]) {
        TYPE_ERROR(c, "unsupported operand type for unary operator");
    } else if (UNOP_IS_BOOL(e->op)) {
        r->type = get_type(c, PAW_TBOOL);
    } else if (e->op == UNARY_LEN) {
        r->type = get_type(c, PAW_TINT);
    } else {
        r->type = type;
    }
    return result;
}

static struct HirExpr *resolve_binop_expr(struct Collector *c, struct AstBinOpExpr *e)
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

    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirBinOpExpr);
    struct HirBinOpExpr *r = HirGetBinOpExpr(result);
    r->lhs = resolve_expr(c, e->lhs);
    r->rhs = resolve_expr(c, e->rhs);
    r->op = e->op;

    struct HirType *lhs = resolve_operand(c, r->lhs);
    struct HirType *rhs = resolve_operand(c, r->rhs);
    unify(c, lhs, rhs);

    const paw_Type code = TYPE2CODE(c, lhs);
    paw_assert(code == TYPE2CODE(c, rhs));
    if (code < 0 || !kValidOps[e->op][code]) {
        TYPE_ERROR(c, "unsupported operand types for binary operator");
    } else if (BINOP_IS_BOOL(e->op)) {
        r->type = get_type(c, PAW_TBOOL);
    } else {
        r->type = lhs;
    }
    return result;
}

static struct HirExpr *resolve_assign_expr(struct Collector *c, struct AstAssignExpr *s)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, s->line, kHirAssignExpr);
    struct HirAssignExpr *r = HirGetAssignExpr(result);

    r->lhs = resolve_expr(c, s->lhs);
    if (!HirIsPathExpr(r->lhs) &&
            !HirIsIndex(r->lhs) &&
            !HirIsSelector(r->lhs)) {
        SYNTAX_ERROR(c, "invalid place for assignment");
    }
    r->rhs = resolve_expr(c, s->rhs);
    struct HirType *lhs = resolve_operand(c, r->lhs);
    struct HirType *rhs = resolve_operand(c, r->rhs);
    unify(c, lhs, rhs);
    r->type = lhs;
    return result;
}

static struct HirType *new_list_t(struct Collector *c, struct HirType *elem_t)
{
    struct HirDecl *base = get_decl(c, c->C->builtins[BUILTIN_LIST].did);
    struct HirTypeList *types = pawHir_type_list_new(c->hir);
    pawHir_type_list_push(c->hir, types, elem_t);
    struct HirDecl *inst = instantiate_adt(c, &base->adt, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *new_map_t(struct Collector *c, struct HirType *key_t, struct HirType *value_t)
{
    if (!HirIsUnknown(key_t) && !HIR_IS_BASIC_T(key_t)) {
        TYPE_ERROR(c, "key is not hashable");
    }
    struct HirDecl *base = get_decl(c, c->C->builtins[BUILTIN_MAP].did);
    struct HirTypeList *types = pawHir_type_list_new(c->hir);
    pawHir_type_list_push(c->hir, types, key_t);
    pawHir_type_list_push(c->hir, types, value_t);
    struct HirDecl *inst = instantiate_adt(c, &base->adt, types);
    return HIR_TYPEOF(inst);
}

static struct HirTypeList *new_unknowns(struct Collector *c, int count)
{
    struct HirTypeList *list = pawHir_type_list_new(c->hir);
    for (int i = 0; i < count; ++i) {
        struct HirType *unknown = pawU_new_unknown(c->U);
        pawHir_type_list_push(c->hir, list, unknown);
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
static struct Generalization generalize(struct Collector *c, struct HirDeclList *generics, struct HirDeclList *fields)
{
    if (generics == NULL) return (struct Generalization){0};
    struct HirTypeList *gtypes = collect_decl_types(c, generics);
    struct HirTypeList *ftypes = collect_decl_types(c, fields);
    struct HirTypeList *unknowns = new_unknowns(c, generics->count);
    struct HirTypeList *replaced = instantiate_typelist(c, gtypes, unknowns, ftypes);
    return (struct Generalization){
        .types = unknowns,
        .fields = replaced,
    };
}

static struct HirType *resolve_container_type(struct Collector *c, struct AstContainerType *e)
{
    struct HirType *first = resolve_type(c, e->first);
    if (e->second == NULL) {
        return new_list_t(c, first);
    }
    struct HirType *second = resolve_type(c, e->second);
    return new_map_t(c, first, second);
}

static struct HirType *resolve_signature(struct Collector *c, struct AstSignature *e)
{
    struct HirType *type = new_type(c, NO_DECL, kHirFuncPtr, e->line);
    struct HirType *result = resolve_type(c, e->result);
    type->fptr.params = resolve_type_list(c, e->params);
    type->fptr.result = result;
    return type;
}

static struct HirDecl *resolve_closure_param(struct Collector *c, struct AstFieldDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirFieldDecl);
    struct HirFieldDecl *r = HirGetFieldDecl(result);
    add_decl(c, result);

    r->name = d->name;
    r->type = d->tag == NULL
        ? pawU_new_unknown(c->U)
        : resolve_type(c, d->tag);
    new_local(c, r->name, result);
    return result;
}

static struct HirExpr *resolve_closure_expr(struct Collector *c, struct AstClosureExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirClosureExpr);
    struct HirClosureExpr *r = HirGetClosureExpr(result);

    struct HirType *last_result = c->result;
    const int last_count = c->nresults;
    enter_block(c, NULL);
    c->nresults = 0;

    r->params = pawHir_decl_list_new(c->hir);
    for (int i = 0; i < e->params->count; ++i) {
        struct AstFieldDecl *src = AstGetFieldDecl(e->params->data[i]);
        struct HirDecl *dst = resolve_closure_param(c, src);
        pawHir_decl_list_push(c->hir, r->params, dst);
    }
    struct HirType *ret = e->result == NULL
        ? pawU_new_unknown(c->U)
        : resolve_type(c, e->result);

    struct HirType *t = new_type(c, NO_DECL, kHirFuncPtr, e->line);
    t->fptr.params = collect_decl_types(c, r->params);
    t->fptr.result = ret;
    c->result = ret;
    r->type = t;

    if (e->has_body) {
        r->body = RESOLVE_BLOCK(c, e->body);
        r->has_body = PAW_TRUE;
        if (c->nresults == 0) {
            // implicit 'return ()'
            unify(c, ret, get_type(c, PAW_TUNIT));
        }
    } else {
        r->expr = resolve_expr(c, e->expr);
        struct HirType *type = resolve_operand(c, r->expr);
        unify(c, ret, type);
    }

    leave_block(c);
    c->nresults = last_count;
    c->result = last_result;
    return result;
}

static struct HirDecl *find_method(struct Collector *c, struct HirAdt *adt, String *name)
{
    struct LazyItemList *impls = impls_for_base(c, adt->base);
    if (impls == NULL) return NULL; // no impl blocks
    for (int i = 0; i < impls->count; ++i) {
        struct LazyItem *item = item_list_get(impls, i);
        if (!item->is_resolved) resolve_item(c, item);
        struct HirImplDecl *impl = HirGetImplDecl(item->hir_decl);
        struct HirAdtDecl *d = get_adt(c, HIR_CAST_TYPE(adt));
        if (!is_compat(c, impl->type, d->type)) continue;
        if (HIR_IS_POLY_IMPL(item->hir_decl)) {
            struct HirDecl *inst = instantiate_impl(c, impl, adt->types);
            impl = HirGetImplDecl(inst);
        }
        for (int j = 0; j < impl->methods->count; ++j) {
            struct HirDecl *method = pawHir_decl_list_get(impl->methods, j);
            if (pawS_eq(name, method->hdr.name)) {
                return method;
            }
        }
    }
    struct HirDecl *a = get_decl(c, adt->base);
    return NULL;
}

static void maybe_fix_builtin(struct Collector *c, String *name, DefId did)
{
    struct Builtin *b = c->C->builtins;
    for (enum BuiltinKind k = 0; k < NBUILTINS; ++k) {
        if (pawS_eq(b[k].name, name)) {
            b[k].did = did;
            break;
        }
    }
}

static struct LazyItem *register_adt_item(struct Collector *c, struct AstAdtDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirAdtDecl);
    struct HirAdtDecl *r = HirGetAdtDecl(result);
    r->is_pub = d->is_pub;
    r->is_struct = d->is_struct;
    r->name = d->name;

    struct HirSymbol *symbol = IS_BUILTIN_DECL(c, result)
        ? new_global(c, d->name, result)
        : new_symbol(c, result);
    symbol->is_type = PAW_TRUE;

    struct HirScope *scope = register_adt(c, d, r);
    maybe_fix_builtin(c, r->name, r->did);
    return new_lazy_item(c, AST_CAST_DECL(d), result, scope, symbol);
}

static void resolve_adt_item(struct Collector *c, struct LazyItem *item)
{
    struct AstAdtDecl *ast_adt = AstGetAdtDecl(item->ast_decl);
    struct HirAdtDecl *hir_adt = HirGetAdtDecl(item->hir_decl);
    if (ast_adt->fields != NULL) resolve_adt_fields(c, ast_adt, hir_adt, item->scope);
}

static struct LazyItem *register_impl_item(struct Collector *c, struct AstImplDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirImplDecl);
    struct HirImplDecl *r = HirGetImplDecl(result);
    r->name = d->name;

    enter_block(c, NULL);
    if (d->generics != NULL) {
        r->generics = register_generics(c, d->generics);
        r->subst = collect_decl_types(c, r->generics);
        r->monos = pawHir_decl_list_new(c->hir);
    }
    r->self = resolve_path(c, d->self);

    // Lookup the 'self' ADT. If the ADT is an instance of a polymorphic ADT, it
    // may need to be instantiated here. Either way, the LazyItem holding the ADT
    // will be resolved (this is fine, since impl blocks are resolved in a separate
    // pass, after all ADTs have been registered).
    struct HirDecl *self = resolve_location(c, r->self);
    if (!HirIsAdtDecl(self)) TYPE_ERROR(c, "expected ADT as target of 'impl' block");
    r->type = HIR_TYPEOF(self);

    struct HirScope *scope = leave_block(c);
    struct LazyItem *item = new_lazy_item(c, AST_CAST_DECL(d), result, scope, NULL);

    // Impl blocks are stored in a list in a map keyed on the base ADT's 'did' field. 
    // Methods are found by searching through this list, using the type parameters
    // on the target ADT. 
    map_base_to_impl(c, HirGetAdt(r->type)->base, item);
    return item;
}

static void resolve_impl_item(struct Collector *c, struct LazyItem *item)
{
    struct AstImplDecl *ast_impl = AstGetImplDecl(item->ast_decl);
    struct HirImplDecl *hir_impl = HirGetImplDecl(item->hir_decl);
    struct HirImplDecl *outer = c->impl_d;
    c->impl_d = hir_impl;

    resolve_impl_methods(c, ast_impl, hir_impl, item->scope);

    c->impl_d = outer;
}

static struct HirDecl *ResolveImplDecl(struct Collector *c, struct AstImplDecl *d)
{
    paw_unused(c);
    paw_unused(d);
    PAW_UNREACHABLE();
}

static struct HirDecl *ResolveAdtDecl(struct Collector *c, struct AstAdtDecl *d)
{
    paw_unused(c);
    paw_unused(d);
    PAW_UNREACHABLE();
}

static struct HirDecl *ResolveVarDecl(struct Collector *c, struct AstVarDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirVarDecl);
    struct HirVarDecl *r = HirGetVarDecl(result);
    add_decl(c, result);
    r->name = d->name;

    struct HirSymbol *symbol = declare_local(c, d->name, result);
    r->init = resolve_expr(c, d->init);
    define_local(symbol);

    struct HirType *init = resolve_operand(c, r->init);
    if (d->tag != NULL) {
        struct HirType *tag = resolve_type(c, d->tag);
        unify(c, init, tag);
    }
    r->type = init;
    return result;
}

static struct HirDecl *ResolveTypeDecl(struct Collector *c, struct AstTypeDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirTypeDecl);
    struct HirTypeDecl *r = HirGetTypeDecl(result);
    r->generics = NULL; // TODO: generic parameters for aliases

    struct HirSymbol *symbol = declare_local(c, d->name, result);
    r->rhs = resolve_expr(c, d->rhs);
    r->type = resolve_operand(c, r->rhs);
    define_local(symbol);
    return result;
}

static struct HirType *infer_poly_func(struct Collector *c, struct HirFuncDecl *base, struct HirExprList *args)
{
    struct Generalization g = generalize(c, base->generics, base->params);

    paw_assert(args->count == g.fields->count);
    for (int i = 0; i < g.fields->count; ++i) {
        struct HirType *a = g.fields->data[i];
        struct HirType *b = resolve_operand(c, args->data[i]);
        unify(c, a, b);
    }
    struct HirDecl *inst = instantiate_func(c, base, g.types);
    return HIR_TYPEOF(inst);
}

static struct HirDecl *instantiate_impl(struct Collector *c, struct HirImplDecl *impl, struct HirTypeList *types)
{
    struct HirAdt *adt = HirGetAdt(impl->type);
    paw_assert(types->count == adt->types->count);
    struct HirTypeList *generics = collect_decl_types(c, impl->generics);
    struct HirTypeList *unknowns = new_unknowns(c, generics->count);

    // Substitute the polymorphic impl block's generics for inference variables (unknowns) 
    // in the context of its 'self' ADT. For example:
    //     impl<X, Y> A<int, Y, X> => impl<?0, ?1> A<int, ?1, ?0>
    // where unknowns = [?0, ?1] and subst = [int, ?1, ?0]. Unifying with the given ADTs
    // type arguments yields a concrete type for each of the impl block's generics.
    struct HirTypeList *subst = instantiate_typelist(c, generics, unknowns, adt->types);
    for (int i = 0; i < subst->count; ++i) {
        unify(c, subst->data[i], types->data[i]);
    }

    struct HirDecl *base = get_decl(c, adt->base);
    struct HirDecl *inst = instantiate_adt(c, HirGetAdtDecl(base), subst);
    return instantiate_impl_aux(c, impl, unknowns, HIR_TYPEOF(inst));
}

// Resolve a function call or enumerator constructor
static struct HirExpr *resolve_call_expr(struct Collector *c, struct AstCallExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirCallExpr);
    struct HirCallExpr *r = HirGetCallExpr(result);

    r->target = resolve_expr(c, e->target);
    r->func = HIR_TYPEOF(r->target);
    if (!HirIsFuncType(r->func)) {
        TYPE_ERROR(c, "type is not callable");
    } else if (e->args->count < HIR_FPTR(r->func)->params->count) {
        SYNTAX_ERROR(c, "not enough arguments");
    } else if (e->args->count > HIR_FPTR(r->func)->params->count) {
        SYNTAX_ERROR(c, "too many arguments");
    }
    if (HirIsFuncDef(r->func)) {
        // Function type has an associated declaration. If that declaration is
        // for a function template, attempt to infer the type parameters.
        struct HirDecl *decl = get_decl(c, HirGetFuncDef(r->func)->did);
        if (HIR_IS_POLY_FUNC(decl)) {
            r->args = resolve_expr_list(c, e->args);
            r->func = infer_poly_func(c, &decl->func, r->args);
            r->type = HIR_FPTR(r->func)->result; // 'fptr' is common prefix
            return result;
        }
    }

    if (is_unit_variant(c, r->func)) {
        TYPE_ERROR(c, "cannot call unit variant (omit '()' to construct)");
    }
    const struct HirFuncPtr *func = HIR_FPTR(r->func);
    const struct HirTypeList *params = func->params;
    r->args = resolve_expr_list(c, e->args);
    r->type = func->result;

    paw_assert(e->args->count == params->count);
    for (int i = 0; i < params->count; ++i) {
        struct HirExpr *arg = pawHir_expr_list_get(r->args, i);
        unify(c, params->data[i], resolve_operand(c, arg));
    }
    return result;
}

static struct HirExpr *resolve_conversion_expr(struct Collector *c, struct AstConversionExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirConversionExpr);
    struct HirConversionExpr *r = HirGetConversionExpr(result);

    r->to = e->to;
    r->arg = resolve_expr(c, e->arg);
    struct HirType *type = resolve_operand(c, r->arg);
    if (!HirIsAdt(type) || 
            HirGetAdt(type)->did == PAW_TUNIT ||
            HirGetAdt(type)->did == PAW_TSTR) {
        TYPE_ERROR(c, "argument to conversion must be scalar");
    }
    r->type = get_type(c, e->to);
    return result;
}

static struct HirType *resolve_basic_lit(struct Collector *c, struct AstBasicLit *e, struct HirBasicLit *r)
{
    r->t = e->t;
    r->value = e->value;
    return get_type(c, r->t);
}

static struct HirType *resolve_tuple_lit(struct Collector *c, struct AstTupleLit *e, struct HirTupleLit *r, int line)
{
    r->elems = resolve_expr_list(c, e->elems);
    struct HirType *type = new_type(c, NO_DECL, kHirTupleType, line);
    HirGetTupleType(type)->elems = collect_expr_types(c, r->elems);
    return type;
}

static struct HirType *resolve_list_lit(struct Collector *c, struct AstContainerLit *e, struct HirContainerLit *r)
{
    struct Unifier *U = c->U;
    struct HirType *elem_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *ast_expr = e->items->data[i];
        struct HirExpr *hir_expr = resolve_expr(c, ast_expr);
        unify(c, resolve_operand(c, hir_expr), elem_t);

        pawHir_expr_list_push(c->hir, r->items, hir_expr);
    }
    return new_list_t(c, elem_t);
}

static struct HirType *resolve_map_lit(struct Collector *c, struct AstContainerLit *e, struct HirContainerLit *r)
{
    struct Unifier *U = c->U;
    struct HirType *key_t = pawU_new_unknown(U);
    struct HirType *value_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *ast_expr = e->items->data[i];
        struct HirExpr *hir_expr = resolve_expr(c, ast_expr);
        struct HirFieldExpr *field = HirGetFieldExpr(hir_expr);
        paw_assert(field->fid == -1);
        struct HirType *k = resolve_operand(c, field->key);
        struct HirType *v = resolve_operand(c, field->value);
        unify(c, k, key_t);
        unify(c, v, value_t);

        pawHir_expr_list_push(c->hir, r->items, hir_expr);
    }
    return new_map_t(c, key_t, value_t);
}

static struct HirType *resolve_container_lit(struct Collector *c, struct AstContainerLit *e, struct HirContainerLit *r)
{
    r->code = e->code;
    r->items = pawHir_expr_list_new(c->hir);
    if (e->code == BUILTIN_LIST) {
        return resolve_list_lit(c, e, r);
    } else {
        paw_assert(e->code == BUILTIN_MAP);
        return resolve_map_lit(c, e, r);
    }
}

static struct HirExpr *resolve_field_expr(struct Collector *c, struct AstFieldExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirFieldExpr);
    struct HirFieldExpr *r = HirGetFieldExpr(result);
    r->fid = e->fid;
    if (e->fid < 0) {
        r->key = resolve_expr(c, e->key);
    } else {
        r->name = e->name;
    }
    r->value = resolve_expr(c, e->value);
    r->type = resolve_operand(c, r->value);
    return result;
}

static struct HirType *resolve_composite_lit(struct Collector *c, struct AstCompositeLit *e, struct HirCompositeLit *r)
{
    r->path = resolve_path(c, e->path);
    struct HirDecl *decl = resolve_location(c, r->path);
    if (!HirIsAdtDecl(decl)) {
        TYPE_ERROR(c, "expected structure type");
    }
    // Use a temporary Map to avoid searching repeatedly through the list of
    // fields.
    paw_Env *P = ENV(c);
    ENSURE_STACK(P, 1);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    V_SET_OBJECT(pv, map);

    Value key;
    struct Generalization g;
    struct HirTypeList *field_types = NULL;
    paw_Bool is_inference = PAW_FALSE;
    struct HirType *target = HIR_TYPEOF(decl);
    struct HirAdtDecl *adt = HirGetAdtDecl(decl);
    if (!adt->is_struct) {
        TYPE_ERROR(c, "expected structure but found enumeration '%s'", adt->name->text);
    } else if (adt->fields == NULL) {
        SYNTAX_ERROR(c, "unexpected curly braces on initializer for unit structure '%s'"
                        "(use name without '{}' to create unit struct)", adt->name->text);
    } else if (HIR_IS_POLY_ADT(decl)) {
        g = generalize(c, adt->generics, adt->fields);
        is_inference = PAW_TRUE;
        field_types = g.fields;
    }
    struct HirExprList *order = pawHir_expr_list_new(c->hir);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *ast_item = e->items->data[i];
        struct HirExpr *hir_item = resolve_expr(c, ast_item);
        struct HirFieldExpr *item = HirGetFieldExpr(hir_item);
        V_SET_OBJECT(&key, item->name);
        if (pawH_contains(map, key)) {
            NAME_ERROR(c, "duplicate field '%s' in initializer for struct '%s'", 
                       item->name->text, adt->name->text);
        }
        Value *value = pawH_create(P, map, key);
        V_SET_INT(value, i);
        pawHir_expr_list_push(c->hir, order, hir_item);
    }
    for (int i = 0; i < adt->fields->count; ++i) {
        struct HirDecl *decl = adt->fields->data[i];
        struct HirFieldDecl *field = HirGetFieldDecl(decl);
        V_SET_OBJECT(&key, field->name);
        Value *value = pawH_get(map, key);
        if (value == NULL) {
            NAME_ERROR(c, "missing initializer for field '%s' in struct '%s'",
                       field->name->text, adt->name->text);
        }
        const paw_Int index = V_INT(*value);
        struct HirType *field_t = is_inference
                               ? field_types->data[i]
                               : HIR_TYPEOF(adt->fields->data[i]);
        struct HirExpr *item = order->data[index];
        struct HirType *item_t = resolve_operand(c, item);
        item->field.fid = i;
        unify(c, item_t, field_t);
        pawH_erase(map, key);
    }
    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(map, &iter)) {
        const Value *pkey = pawH_key(map, CAST_SIZE(iter));
        NAME_ERROR(c, "unexpected field '%s' in initializer for struct '%s'",
                   V_STRING(*pkey), adt->name->text);
    }
    paw_assert(adt->fields->count == e->items->count);
    pawC_pop(P); // pop map

    if (is_inference) {
        struct HirDecl *inst = instantiate_adt(c, adt, g.types);
        target = HIR_TYPEOF(inst);
    }
    r->items = order;
    return target;
}

static struct HirExpr *resolve_literal_expr(struct Collector *c, struct AstLiteralExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirLiteralExpr);
    struct HirLiteralExpr *r = HirGetLiteralExpr(result);

    // literal kinds correspond 1-to-1 between AST and HIR
    r->lit_kind = CAST(enum HirLitKind, e->lit_kind);

    if (e->lit_kind == kAstBasicLit) {
        r->type = resolve_basic_lit(c, &e->basic, &r->basic);
    } else if (e->lit_kind == kAstTupleLit) {
        r->type = resolve_tuple_lit(c, &e->tuple, &r->tuple, e->line);
    } else if (e->lit_kind == kAstContainerLit) {
        r->type = resolve_container_lit(c, &e->cont, &r->cont);
    } else {
        paw_assert(e->lit_kind == kAstCompositeLit);
        r->type = resolve_composite_lit(c, &e->comp, &r->comp);
    }
    return result;
}

static struct LazyItem *register_func_item(struct Collector *c, struct AstFuncDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirFuncDecl);
    struct HirFuncDecl *r = HirGetFuncDecl(result);

    struct HirSymbol *symbol = IS_BUILTIN_DECL(c, result)
        ? new_global(c, d->name, result)
        : new_symbol(c, result);
    symbol->is_type = d->generics != NULL;
    struct HirScope *scope = register_func(c, d, r);
    return new_lazy_item(c, AST_CAST_DECL(d), result, scope, symbol);
}

static void resolve_func_item(struct Collector *c, struct LazyItem *item)
{
    struct AstFuncDecl *ast_func = AstGetFuncDecl(item->ast_decl);
    struct HirFuncDecl *hir_func = HirGetFuncDecl(item->hir_decl);
    resolve_func(c, ast_func, hir_func, item->scope);
}

static struct HirDecl *ResolveFuncDecl(struct Collector *c, struct AstFuncDecl *d)
{
    paw_unused(c);
    paw_unused(d);
    return NULL;
}

static struct HirStmt *ResolveIfStmt(struct Collector *c, struct AstIfStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(c->hir, s->line, kHirIfStmt);
    struct HirIfStmt *r = HirGetIfStmt(result);

    r->cond = expect_bool_expr(c, s->cond);
    r->then_arm = resolve_stmt(c, s->then_arm);
    if (s->else_arm != NULL) {
        r->else_arm = resolve_stmt(c, s->else_arm);
    }
    return result;
}

static struct HirStmt *ResolveExprStmt(struct Collector *c, struct AstExprStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(c->hir, s->line, kHirExprStmt);
    struct HirExprStmt *r = HirGetExprStmt(result);
    r->expr = resolve_expr(c, s->expr);
    return result;
}

static struct HirStmt *ResolveWhileStmt(struct Collector *c, struct AstWhileStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(c->hir, s->line, kHirWhileStmt);
    struct HirWhileStmt *r = HirGetWhileStmt(result);
    r->is_dowhile = s->is_dowhile;

    enter_block(c, NULL);
    r->cond = expect_bool_expr(c, s->cond);
    r->block = RESOLVE_BLOCK(c, s->block);
    leave_block(c);
    return result;
}

static void visit_forbody(struct Collector *c, String *iname, struct HirType *itype, struct AstBlock *b, struct HirForStmt *r)
{
    enter_block(c, NULL); // TODO: move down a few source lines, next 4 statements can come before

    r->control = pawHir_new_decl(c->hir, b->line, kHirVarDecl);
    struct HirVarDecl *control = HirGetVarDecl(r->control);
    control->name = iname;
    control->type = itype;
    new_local(c, iname, r->control);

    r->block = new_block(c, b->line);
    r->block->stmts = resolve_stmt_list(c, b->stmts);
    leave_block(c);
}

static void visit_fornum(struct Collector *c, struct AstForStmt *s, struct HirForStmt *r)
{
    struct AstForNum *fornum = &s->fornum;

    r->fornum.begin = expect_int_expr(c, fornum->begin);
    r->fornum.end = expect_int_expr(c, fornum->end);
    r->fornum.step = expect_int_expr(c, fornum->step);

    visit_forbody(c, s->name, get_type(c, PAW_TINT), s->block, r);
}

// TODO: allow function with signature fn iter<I, T>(I) -> (fn(int) -> T)
static void visit_forin(struct Collector *c, struct AstForStmt *s, struct HirForStmt *r)
{
    r->forin.target = resolve_expr(c, s->forin.target);
    struct HirType *iter_t = resolve_operand(c, r->forin.target);
    struct HirType *elem_t = get_value_type(c, iter_t);

    if (elem_t == NULL) {
        TYPE_ERROR(c, "'for..in' not supported for type");
    }
    visit_forbody(c, s->name, elem_t, s->block, r);
}

static struct HirStmt *ResolveForStmt(struct Collector *c, struct AstForStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(c->hir, s->line, kHirForStmt);
    struct HirForStmt *r = HirGetForStmt(result);
    r->is_fornum = s->is_fornum;
    enter_block(c, NULL);
    if (s->is_fornum) {
        visit_fornum(c, s, r);
    } else {
        visit_forin(c, s, r);
    }
    leave_block(c);
    return result;
}

static struct HirExpr *resolve_index(struct Collector *c, struct AstIndex *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirIndex);
    struct HirIndex *r = HirGetIndex(result);
    r->target = resolve_expr(c, e->target);
    r->is_slice = e->is_slice;

    struct HirType *target = resolve_operand(c, r->target);
    struct HirType *expect = NULL;
    if (is_list_t(c, target)) {
        expect = get_type(c, PAW_TINT);
        r->type = e->is_slice ? target : hir_list_elem(target);
    } else if (is_map_t(c, target)) {
        if (e->is_slice) {
            TYPE_ERROR(c, "slice operation not supported on map "
                          "(requires '[T]' or 'str')");
        }
        expect = hir_map_key(target);
        r->type = hir_map_value(target);
    } else if (HirIsAdt(target) &&
            HirGetAdt(target)->base == PAW_TSTR) {
        expect = get_type(c, PAW_TINT);
        r->type = get_type(c, PAW_TSTR);
    } else {
        TYPE_ERROR(c, "type cannot be indexed (not a container)");
    }
    if (e->is_slice) {
        if (e->first != NULL) {
            r->first = resolve_expr(c, e->first);
            unify(c, expect, resolve_operand(c, r->first));
        }
        if (e->second != NULL) {
            r->second = resolve_expr(c, e->second);
            unify(c, expect, resolve_operand(c, r->second));
        }
    } else {
        r->first = resolve_expr(c, e->first);
        unify(c, expect, resolve_operand(c, r->first));
    }
    return result;
}

static struct HirExpr *resolve_selector(struct Collector *c, struct AstSelector *e)
{
    struct HirExpr *result = pawHir_new_expr(c->hir, e->line, kHirSelector);
    struct HirSelector *r = HirGetSelector(result);
    r->target = resolve_expr(c, e->target);

    struct HirType *target = resolve_operand(c, r->target);
    if (HirIsTupleType(target)) {
        // tuples are indexed with "Expr" "." "int_lit"
        struct HirTypeList *types = target->tuple.elems;
        if (!e->is_index) {
            TYPE_ERROR(c, "expected index of tuple element");
        } else if (e->index >= types->count) {
            TYPE_ERROR(c, "element index %d out of range of %d-tuple",
                       e->index, types->count);
        }
        
        r->index = e->index;
        r->is_index = PAW_TRUE;
        r->type = types->data[e->index];
        return result;
    } else if (!HirIsAdt(target)) {
        TYPE_ERROR(c, "type has no fields");
    }
    struct HirAdtDecl *adt = get_adt(c, target);
    if (e->is_index) {
        TYPE_ERROR(c, "expected field name (integer indices can "
                      "only be used with tuples)");
    }
    struct HirDecl *field = expect_field(c, adt, e->name);
    r->type = HIR_TYPEOF(field);
    r->name = e->name;
    return result;
}

static struct HirStmt *ResolveLabelStmt(struct Collector *c, struct AstLabelStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(c->hir, s->line, kHirLabelStmt);
    struct HirLabelStmt *r = HirGetLabelStmt(result);
    r->label = s->label;
    return result;
}

static struct HirStmt *ResolveDeclStmt(struct Collector *c, struct AstDeclStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(c->hir, s->line, kHirDeclStmt);
    struct HirDeclStmt *r = HirGetDeclStmt(result);
    r->decl = resolve_decl(c, s->decl);
    return result;
}

static struct HirDecl *ResolveGenericDecl(struct Collector *c, struct AstGenericDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(c->hir, d->line, kHirGenericDecl);
    struct HirGenericDecl *r = HirGetGenericDecl(result);

    r->type = register_decl_type(c, result, kHirGeneric);
    struct HirGeneric *t = HirGetGeneric(r->type);
    r->name = t->name = d->name;
    return result;
}

static struct HirDecl *resolve_decl(struct Collector *c, struct AstDecl *decl)
{
    c->line = decl->hdr.line;
    switch (AST_KINDOF(decl)) {
#define DEFINE_CASE(a, b) \
        case kAst##a: \
            return Resolve##a(c, AstGet##a(decl));
        AST_DECL_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirStmt *resolve_stmt(struct Collector *c, struct AstStmt *stmt)
{
    c->line = stmt->hdr.line;
    switch (AST_KINDOF(stmt)) {
#define DEFINE_CASE(a, b) \
        case kAst##a: \
            return Resolve##a(c, AstGet##a(stmt));
        AST_STMT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

// NOTE: Some expressions are known to directly represent types, based on the context
//       (type annotations, type arguments, etc.). Call resolve_type() to convert such
//       an expression into a HIR type.

static struct HirType *resolve_type(struct Collector *c, struct AstExpr *expr)
{
    struct HirType *r;
    c->line = expr->hdr.line;
    switch (AST_KINDOF(expr)) {
        case kAstPathExpr:
            r = resolve_path_type(c, AstGetPathExpr(expr));
            break;
        case kAstTupleType:
            r = resolve_tuple_type(c, AstGetTupleType(expr));
            break;
        case kAstSignature:
            r = resolve_signature(c, AstGetSignature(expr));
            break;
        case kAstContainerType:
            r = resolve_container_type(c, AstGetContainerType(expr)); 
            break;
        default:
            TYPE_ERROR(c, "expected type");
    }
    return normalize(c, r);
}

static struct HirExpr *resolve_expr(struct Collector *c, struct AstExpr *expr)
{
    struct HirExpr *r;
    c->line = expr->hdr.line;
    switch (AST_KINDOF(expr)) {
        case kAstLiteralExpr:
            r = resolve_literal_expr(c, AstGetLiteralExpr(expr));
            break;
        case kAstLogicalExpr:
            r = resolve_logical_expr(c, AstGetLogicalExpr(expr));
            break;
        case kAstPathExpr:
            r = resolve_path_expr(c, AstGetPathExpr(expr));
            break;
        case kAstChainExpr:
            r = resolve_chain_expr(c, AstGetChainExpr(expr));
            break;
        case kAstUnOpExpr:
            r = resolve_unop_expr(c, AstGetUnOpExpr(expr));
            break;
        case kAstBinOpExpr:
            r = resolve_binop_expr(c, AstGetBinOpExpr(expr));
            break;
        case kAstClosureExpr:
            r = resolve_closure_expr(c, AstGetClosureExpr(expr));
            break;
        case kAstConversionExpr:
            r = resolve_conversion_expr(c, AstGetConversionExpr(expr));
            break;
        case kAstCallExpr:
            r = resolve_call_expr(c, AstGetCallExpr(expr));
            break;
        case kAstIndex:
            r = resolve_index(c, AstGetIndex(expr));
            break;
        case kAstSelector:
            r = resolve_selector(c, AstGetSelector(expr)); 
            break; 
        case kAstAssignExpr:
            r = resolve_assign_expr(c, AstGetAssignExpr(expr));
            break;
        default:
            r = resolve_field_expr(c, AstGetFieldExpr(expr));
    }

    struct HirType *type = HIR_TYPEOF(r);
    r->hdr.type = normalize(c, type);
    return r;
}

static struct HirDeclList *register_items(struct Collector *c, struct AstDeclList *decls, struct LazyItemList **pitems)
{
    *pitems = item_list_new(c->hir);
    struct HirDeclList *output = pawHir_decl_list_new(c->hir);
    for (int i = 0; i < decls->count; ++i) {
        struct AstDecl *decl = decls->data[i];
        struct LazyItem *item;
        if (AstIsFuncDecl(decl)) {
            item = register_func_item(c, AstGetFuncDecl(decl));
        } else if (AstIsAdtDecl(decl)) {
            item = register_adt_item(c, AstGetAdtDecl(decl));
        } else {
            continue;
        }
        pawHir_decl_list_push(c->hir, output, item->hir_decl);
        item_list_push(c->hir, *pitems, item);
    }
    return output;
}

static void register_impls(struct Collector *c, struct AstDeclList *decls, struct HirDeclList *out)
{
    for (int i = 0; i < decls->count; ++i) {
        struct AstDecl *ast_decl = decls->data[i];
        if (!AstIsImplDecl(ast_decl)) continue;
        struct AstImplDecl *d = AstGetImplDecl(ast_decl);

        struct LazyItem *item = register_impl_item(c, d);
        pawHir_decl_list_push(c->hir, out, item->hir_decl);
        item_list_push(c->hir, c->items, item);
    }
}

static void resolve_item(struct Collector *c, struct LazyItem *item)
{
    if (item->is_resolved) return;
    item->is_resolved = PAW_TRUE;
    if (AstIsFuncDecl(item->ast_decl)) {
        resolve_func_item(c, item);
    } else if (AstIsAdtDecl(item->ast_decl)) {
        resolve_adt_item(c, item);
    } else if (AstIsImplDecl(item->ast_decl)) {
        resolve_impl_item(c, item);
    }
}

static void resolve_items(struct Collector *c, struct LazyItemList *items)
{
    for (int i = 0; i < items->count; ++i) {
        resolve_item(c, items->data[i]);
    }
}

static struct HirDeclList *resolve_module(struct Collector *c, struct AstDeclList *decls)
{
    // multi-phase symbol resolution: the 'collect' phase might happen out-of-order, 
    // depending on how items are referenced
    struct HirDeclList *output = register_items(c, decls, &c->items);
    register_impls(c, decls, output);
    resolve_items(c, c->items);
    return output;
}

static DefId find_builtin(struct Collector *c, String *name)
{
    const struct HirSymbol *symbol = resolve_symbol(c, name);
    return symbol->decl->hdr.did;
}

static struct HirDeclList *resolve_prelude(struct Collector *c, struct AstDeclList *prelude)
{
    paw_Env *P = ENV(c);
    struct Hir *hir = c->hir;
    struct HirDeclList *result = resolve_module(c, prelude);
    c->option_did = find_builtin(c, pawE_cstr(P, CSTR_OPTION));
    c->result_did = find_builtin(c, pawE_cstr(P, CSTR_RESULT));
    return result;
}

static void visit_module(struct Collector *c)
{
    struct Ast *ast = c->ast;
    struct Hir *hir = c->hir;
    struct HirSymtab *symtab = c->symtab;

    enter_block(c, NULL);
    enter_inference_ctx(c);

    hir->prelude = resolve_prelude(c, ast->prelude);
    hir->items = resolve_module(c, ast->items);

    leave_inference_ctx(c);
    leave_block(c);
    paw_assert(symtab->count == 0);

    pawHir_expand(c, hir);
}

struct Hir *pawP_collect(struct Compiler *C, struct Ast *ast)
{
    paw_Env *P = ENV(C);
    paw_new_map(P, 0);

    struct Hir *hir = pawHir_new(C);
    struct Collector c = {
        .dm = C->dm,
        .ast = ast,
        .hir = hir,
        .impls = V_MAP(P->top.p[-1]),
        .globals = pawHir_scope_new(hir),
        .symtab = hir->symtab,
        .strings = C->strings,
        .U = &C->dm->unifier,
        .P = ENV(C),
        .C = C,
    };
    C->dm->decls = pawHir_decl_list_new(hir);
    C->dm->unifier.ast = ast;
    C->dm->unifier.hir = hir;
    C->dm->unifier.P = P;
    C->dm->unifier.c = &c;
    C->dm->hir = hir;
    visit_module(&c);

    pawC_pop(P);
    return hir;
}
