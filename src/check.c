// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// check.c: Implementation of the type checker. This code transforms an AST
// from the parser into a graph by unifying types based on lexical scope.

#include "ast.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "env.h"
#include "gc_aux.h"
#include "hir.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "resolve.h"
#include "str.h"
#include "type.h"
#include "unify.h"

#define NAME_ERROR(R, line, ...) pawE_error(ENV(R), PAW_ENAME, line, __VA_ARGS__)
#define SYNTAX_ERROR(R, ...) pawE_error(ENV(R), PAW_ESYNTAX, -1, __VA_ARGS__)
#define TYPE_ERROR(R, ...) pawE_error(ENV(R), PAW_ETYPE, -1, __VA_ARGS__)
#define CACHED_STR(R, i) pawE_cstr(ENV(R), cast_size(i))
#define TYPE2CODE(R, type) (pawP_type2code((R)->C, type))

struct ItemSlot {
    struct AstDecl *ast_decl;
    struct HirDecl *hir_decl;
    struct HirScope *scope;
};

static struct ItemSlot *new_item_slot(struct Resolver *R, struct AstDecl *ad, struct HirDecl *hd, struct HirScope *scope)
{
    struct ItemSlot *slot = pawK_pool_alloc(ENV(R), &R->hir->pool, sizeof(struct ItemSlot));
    *slot = (struct ItemSlot){
        .ast_decl = ad,
        .hir_decl = hd,
        .scope = scope, 
    };
    return slot;
}

DEFINE_LIST(struct Hir, item_list_, ItemList, struct ItemSlot)

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
    paw_assert(did < R->dm->decls.size);
    return HIR_TYPEOF(R->dm->decls.data[did]);
}

static paw_Bool is_vector_t(struct Resolver *R, struct HirType *type)
{
    return HirIsAdt(type) && HirGetAdt(type)->base == R->C->vector_did;
}

static paw_Bool is_map_t(struct Resolver *R, struct HirType *type)
{
    return HirIsAdt(type) && HirGetAdt(type)->base == R->C->map_did;
}

static struct HirDecl *get_decl(struct Resolver *R, DefId did)
{
    struct DynamicMem *dm = R->dm;
    paw_assert(did < dm->decls.size);
    return dm->decls.data[did];
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

static struct HirSymbol *resolve_symbol(struct Resolver *R, const String *name);
static struct HirAdtDecl *lookup_struct(struct Resolver *R, struct HirPath *path)
{
    // NOTE: no associated types on structures
    if (path->count > 1) return NULL; 
    struct HirSegment *base = pawHir_path_get(path, 0);
    struct HirSymbol *symbol = resolve_symbol(R, base->name);    
    if (HirIsAdtDecl(symbol->decl)) {
        struct HirAdtDecl *adt = HirGetAdtDecl(symbol->decl);
        return adt->is_struct ? adt : NULL; 
    }
    return NULL;
}

static paw_Bool fix_unit_struct(struct Resolver *R, struct HirExpr *expr)
{
    if (!HirIsPathExpr(expr)) return PAW_FALSE;
    struct HirPathExpr e = *HirGetPathExpr(expr);
    struct HirAdtDecl *adt = lookup_struct(R, e.path);
    if (adt != NULL) {
        if (adt->fields != NULL) {
            SYNTAX_ERROR(R, "missing fields on initializer for struct '%s'", adt->name->text);
        }
        expr->hdr.kind = kHirLiteralExpr;
        struct HirLiteralExpr *r = HirGetLiteralExpr(expr);
        r->lit_kind = kHirLitComposite;
        r->comp.items = pawHir_expr_list_new(R->hir);
        r->comp.path = e.path;
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static struct HirType *expr_type(struct Resolver *R, struct HirExpr *expr)
{
    const paw_Bool fixed = fix_unit_struct(R, expr);
    struct HirType *type = HIR_TYPEOF(expr);
    return !fixed && is_unit_variant(R, type) 
        ? HIR_FPTR(type)->result 
        : type;
}

#define are_types_same(a, b) ((a) == (b))

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
            return are_types_same(a, b);
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
    struct DynamicMem *dm = R->dm;
    for (DefId did = 0; did < dm->decls.size; ++did) {
        struct HirDecl *old_decl = dm->decls.data[did];
        paw_assert(old_decl->hdr.did == did);
        paw_assert(old_decl != new_decl);
    }
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
        struct HirType *type = expr_type(R, list->data[i]);
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
            NAME_ERROR(R, decl->hdr.line,
                       "duplicate global '%s' (declared previously on line %d)",
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
            if (scope->fn_depth != R->func_depth && 
                    !HirIsFuncDecl(symbol->decl) &&
                    !symbol->is_type) {
                // TODO: note that this is not currently possible: named functions (non-closures) can only
                //       appear at the toplevel, anything they reference outside themselves is global
                // TODO: replace is_type with more specific flag, this will mess
                //       up function templates!
                TYPE_ERROR(R, "attempt to reference non-local variable '%s' "
                              "(consider using a closure)", name->text);
            }
            return scope->symbols->data[index];
        }
    }
    // search the global symbols
    const int index = pawHir_find_symbol(scopes->globals, name);
    if (index < 0) {
        return NULL;
    }
    return scopes->globals->symbols->data[index];
}

static struct HirSymbol *resolve_symbol(struct Resolver *R, const String *name)
{
    struct HirSymbol *symbol = try_resolve_symbol(R, name);
    if (symbol == NULL) {
        NAME_ERROR(R, -1, "undefined symbol '%s'", name->text);
    }
    return symbol;
}

static struct HirDecl *resolve_attr(struct HirDeclList *attrs, String *name)
{
    for (int i = 0; i < attrs->count; ++i) {
        struct HirDecl *decl = attrs->data[i];
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
    check_gc(ENV(R));
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

    if (t->types == NULL) {
        return HIR_CAST_TYPE(t);
    }
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
        if (are_types_same(t, s->before->data[i])) {
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

    // NOTE: '.scope' not used for functions
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
    if (types == NULL) {
        return HIR_CAST_DECL(base);
    }
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
    if (types == NULL) {
        // not polymorphic
    } else if (HIR_IS_POLY_ADT(base)) {
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

    r->name = d->name;
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
    r->fields = resolve_decl_list(R, d->fields);

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
        have = expr_type(R, r->expr);
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
    unify(R, expr_type(R, r), get_type(R, PAW_TBOOL));
    return r;
}

static struct HirExpr *expect_int_expr(struct Resolver *R, struct AstExpr *e)
{
    struct HirExpr *r = resolve_expr(R, e);
    unify(R, expr_type(R, r), get_type(R, PAW_TINT));
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

static struct HirDecl *expect_attr(struct Resolver *R, const struct StructPack *pack, String *name)
{
    struct HirDecl *attr = resolve_attr(pack->fields, name);
    if (attr == NULL) {
        NAME_ERROR(R, -1, "field '%s' does not exist in type '%s'", name->text,
                   pack->name->text);
    }
    return attr;
}

static struct HirSegment *resolve_base_seg(struct Resolver *R, struct AstSegment *base, struct HirPath *path)
{
    struct HirSegment *r = pawHir_segment_new(R->hir);
    struct HirSymbol *symbol = resolve_symbol(R, base->name);
    if (base->types != NULL) {
        r->types = resolve_type_list(R, base->types);
    }
    struct HirDecl *inst = pawP_instantiate(R, symbol->decl, r->types);
    r->type  = HIR_TYPEOF(inst);
    r->name = base->name;
    pawHir_path_push(R->hir, path, r);
    return r;
}

static struct HirSegment *resolve_next_seg(struct Resolver *R, struct AstSegment *next, struct HirSegment *prev, struct HirPath *path)
{
    struct HirSegment *r = pawHir_segment_new(R->hir);
    const struct StructPack pack = unpack_struct(R, prev->type);
    struct HirDecl *attr = expect_attr(R, &pack, next->name);
    if (next->types != NULL) {
        r->types = resolve_type_list(R, next->types);
    }
    struct HirDecl *inst = pawP_instantiate(R, attr, r->types);
    r->type  = HIR_TYPEOF(inst);
    r->name = next->name;
    pawHir_path_push(R->hir, path, r);
    return r;
}

static struct HirPath *resolve_path(struct Resolver *R, struct AstPath *path)
{
    struct HirPath *r = pawHir_path_new(R->hir);
    struct AstSegment *src = pawAst_path_get(path, 0);
    struct HirSegment *dst = resolve_base_seg(R, src, r);
    for (int i = 1; i < path->count; ++i) {
        src = pawAst_path_get(path, i);
        dst = resolve_next_seg(R, src, dst, r);
    }
    return r;
}

static struct HirType *typeof_path(struct HirPath *path) 
{
    paw_assert(path->count > 0);
    return path->data[path->count - 1]->type;
}

static struct HirType *resolve_tuple_type(struct Resolver *R, struct AstTupleType *e)
{
    if (e->types->count == 0) {
        return get_type(R, PAW_TUNIT);
    }
    struct HirType *r = new_type(R, NO_DECL, kHirTupleType, e->line);
    r->tuple.elems = resolve_type_list(R, e->types);
    return r;
}

static struct HirExpr *resolve_path_expr(struct Resolver *R, struct AstPathExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirPathExpr);
    struct HirPathExpr *r = HirGetPathExpr(result);
    r->path = resolve_path(R, e->path);
    r->type = typeof_path(r->path);
    return result;
}

static struct HirType *resolve_path_type(struct Resolver *R, struct AstPathExpr *e)
{
    // TODO: recycle 'path' memory
    struct HirPath *path = resolve_path(R, e->path);
    return typeof_path(path);
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
    return HirIsAdt(type) && type->adt.base == R->C->option_did;
}

static paw_Bool is_result_t(struct Resolver *R, const struct HirType *type)
{
    return HirIsAdt(type) && type->adt.base == R->C->result_did;
}

static struct HirExpr *resolve_chain_expr(struct Resolver *R, struct AstChainExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirChainExpr);
    struct HirChainExpr *r = HirGetChainExpr(result);
    r->target = resolve_expr(R, e->target);

    struct HirType *type = expr_type(R, r->target);
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
    if (is_vector_t(R, target)) {
        return hir_vector_elem(target);
    } else if (is_map_t(R, target)) {
        return hir_map_value(target);
    }
    return NULL;
}

static struct HirType *resolve_in_expr(struct Resolver *R, struct HirType *elem, struct HirType *adt)
{
    struct HirType *type = get_value_type(R, adt);
    if (type == NULL) {
        TYPE_ERROR(R, "expected Vector or Map");
    }
    unify(R, elem, type);
    return get_type(R, PAW_TBOOL);
}

static struct HirExpr *resolve_unop_expr(struct Resolver *R, struct AstUnOpExpr *e)
{
    // clang-format off
    static const uint8_t kValidOps[NUNARYOPS][PAW_NTYPES] = {
        //     type  =  0, b, i, f, s, v, m, ...
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

    struct HirType *type = expr_type(R, r->target);
    const paw_Type code = TYPE2CODE(R, type);
    if (!kValidOps[e->op][code]) {
        TYPE_ERROR(R, "unsupported operand type for unary operator");
    } else if (unop_is_bool(e->op)) {
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

static struct HirType *binop_vector(struct Resolver *R, BinaryOp op, struct HirType *type)
{
    const struct HirType *elem_t = hir_vector_elem(type);
    if (op == BINARY_ADD) {
        // 2 vectors with the same element type can be added
        return type;
    } else if (!HIR_IS_BASIC_T(elem_t)) {
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
    // clang-format off
    static const uint8_t kValidOps[NBINARYOPS][PAW_NTYPES] = {
        //     type   =  0, b, i, f, s, v, m, ...
        [BINARY_EQ]   = {0, 1, 1, 1, 1, 1, 1},
        [BINARY_NE]   = {0, 1, 1, 1, 1, 1, 1},
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
    // clang-format on

    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirBinOpExpr);
    struct HirBinOpExpr *r = HirGetBinOpExpr(result);
    r->lhs = resolve_expr(R, e->lhs);
    r->rhs = resolve_expr(R, e->rhs);
    r->op = e->op;

    struct HirType *lhs = expr_type(R, r->lhs);
    struct HirType *rhs = expr_type(R, r->rhs);
    if (e->op == BINARY_IN) {
        r->type = resolve_in_expr(R, lhs, rhs);
        return result;
    }
    unify(R, lhs, rhs);

    const paw_Type type = TYPE2CODE(R, lhs);
    paw_assert(type == TYPE2CODE(R, rhs));
    if (type < 0 || !kValidOps[e->op][type]) {
        TYPE_ERROR(R, "unsupported operand types for binary operator");
    } else if (type == PAW_TVECTOR) {
        r->type = binop_vector(R, e->op, lhs);
    } else if (type == PAW_TMAP) {
        r->type = binop_map(R, lhs);
    } else if (binop_is_bool(e->op)) {
        r->type = get_type(R, PAW_TBOOL);
    } else {
        r->type = lhs;
    }
    return result;
}

static struct HirType *new_vector_t(struct Resolver *R, struct HirType *elem_t)
{
    struct HirDecl *base = get_decl(R, R->C->vector_did);
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
    struct HirDecl *base = get_decl(R, R->C->map_did);
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
    if (generics == NULL) {
        return (struct Generalization){0};
    }
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
        return new_vector_t(R, first);
    }
    struct HirType *second = resolve_type(R, e->second);
    return new_map_t(R, first, second);
}

static struct HirType *resolve_signature(struct Resolver *R, struct AstSignature *e)
{
    struct HirType *type = new_type(R, NO_DECL, kHirFuncPtr, e->line);
    struct HirType *result = resolve_type(R, e->result);
    struct HirExprList *params = resolve_expr_list(R, e->params);
    type->fptr.params = collect_expr_types(R, params);
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
        struct HirType *type = expr_type(R, r->expr);
        unify(R, ret, type);
    }

    leave_block(R);
    R->nresults = last_count;
    R->result = last_result;
    return result;
}

static struct ItemSlot *register_adt_item(struct Resolver *R, struct AstAdtDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirAdtDecl);
    struct HirAdtDecl *r = HirGetAdtDecl(result);
    r->is_pub = d->is_pub;
    r->is_struct = d->is_struct;
    r->name = d->name;

    struct HirSymbol *symbol = new_global(R, d->name, result, d->is_pub);
    symbol->is_type = PAW_TRUE;
    register_struct(R, d, r);
    return new_item_slot(R, AST_CAST_DECL(d), result, NULL);
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

    struct HirSymbol *symbol = declare_local(R, d->name, result);
    r->init = resolve_expr(R, d->init);
    define_local(symbol);

    struct HirType *init = expr_type(R, r->init);
    if (d->tag != NULL) {
        struct HirType *tag = resolve_type(R, d->tag);
        unify(R, init, tag);
    }
    add_def(R, result);
    r->type = init;
    r->name = d->name;
    return result;
}

static struct HirDecl *ResolveTypeDecl(struct Resolver *R, struct AstTypeDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirTypeDecl);
    struct HirTypeDecl *r = HirGetTypeDecl(result);
    r->generics = NULL; // TODO: generic parameters for aliases

    struct HirSymbol *symbol = declare_local(R, d->name, result);
    r->rhs = resolve_expr(R, d->rhs);
    r->type = expr_type(R, r->rhs);
    define_local(symbol);
    return result;
}

static struct HirType *infer_func_template(struct Resolver *R, struct HirFuncDecl *base, struct HirExprList *args)
{
    struct Generalization g = generalize(R, base->generics, base->params);

    paw_assert(args->count == g.fields->count);
    for (int i = 0; i < g.fields->count; ++i) {
        struct HirType *a = g.fields->data[i];
        struct HirType *b = expr_type(R, args->data[i]);
        unify(R, a, b);
    }
    struct HirDecl *inst = instantiate_func(R, base, g.types);
    return HIR_TYPEOF(inst);
}

static struct HirExpr *resolve_call_expr(struct Resolver *R, struct AstCallExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirCallExpr);
    struct HirCallExpr *r = HirGetCallExpr(result);

    r->target = resolve_expr(R, e->target);
    r->func = expr_type(R, r->target);
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
        unify(R, params->data[i], expr_type(R, arg));
    }
    return result;
}

static struct HirExpr *resolve_conversion_expr(struct Resolver *R, struct AstConversionExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirConversionExpr);
    struct HirConversionExpr *r = HirGetConversionExpr(result);

    r->to = e->to;
    r->arg = resolve_expr(R, e->arg);
    struct HirType *type = expr_type(R, r->arg);
    if (!HirIsAdt(type) || 
            HirGetAdt(type)->did == PAW_TUNIT ||
            HirGetAdt(type)->did == PAW_TSTRING) {
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

static struct HirType *resolve_vector_lit(struct Resolver *R, struct AstContainerLit *e, struct HirContainerLit *r)
{
    struct Unifier *U = R->U;
    struct HirType *elem_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *ast_expr = e->items->data[i];
        struct HirExpr *hir_expr = resolve_expr(R, ast_expr);
        unify(R, expr_type(R, hir_expr), elem_t);

        pawHir_expr_list_push(R->hir, r->items, hir_expr);
    }
    return new_vector_t(R, elem_t);
}

static struct HirType *resolve_map_lit(struct Resolver *R, struct AstContainerLit *e, struct HirContainerLit *r)
{
    struct Unifier *U = R->U;
    struct HirType *key_t = pawU_new_unknown(U);
    struct HirType *value_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *ast_expr = e->items->data[i];
        struct HirExpr *hir_expr = resolve_expr(R, ast_expr);
        struct HirType *k = expr_type(R, hir_expr->mitem.key);
        struct HirType *v = expr_type(R, hir_expr->mitem.value);
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
    if (e->code == PAW_TVECTOR) {
        return resolve_vector_lit(R, e, r);
    } else {
        paw_assert(e->code == PAW_TMAP);
        return resolve_map_lit(R, e, r);
    }
}

static struct HirExpr *resolve_map_elem(struct Resolver *R, struct AstMapElem *item)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, item->line, kHirMapElem);
    struct HirMapElem *r = HirGetMapElem(result);

    r->key = resolve_expr(R, item->key);
    r->value = resolve_expr(R, item->value);
    r->type = expr_type(R, r->value);
    return result;
}

static struct HirExpr *resolve_struct_field(struct Resolver *R, struct AstStructField *item)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, item->line, kHirStructField);
    struct HirStructField *r = HirGetStructField(result);

    r->name = item->name; 
    r->value = resolve_expr(R, item->value);
    r->type = expr_type(R, r->value);
    return result;
}

static struct HirType *resolve_composite_lit(struct Resolver *R, struct AstCompositeLit *e, struct HirCompositeLit *r)
{
    r->path = resolve_path(R, e->path);
    struct HirType *target = typeof_path(r->path);
    if (!HirIsAdt(target)) {
        TYPE_ERROR(R, "expected structure type");
    }
    // Use a temporary Map to avoid searching repeatedly through the list of
    // fields.
    paw_Env *P = ENV(R);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    v_set_object(pv, map);

    Value key;
    struct Generalization g;
    struct HirTypeList *field_types = NULL;
    paw_Bool is_inference = PAW_FALSE;
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
        struct HirStructField *item = HirGetStructField(hir_item);
        v_set_object(&key, item->name);
        if (pawH_contains(P, map, key)) {
            NAME_ERROR(R, hir_item->hdr.line, "duplicate field '%s' in initializer for struct '%s'", 
                       item->name->text, pack.name->text);
        }
        Value *value = pawH_action(P, map, key, MAP_ACTION_CREATE);
        v_set_int(value, i);
        pawHir_expr_list_push(R->hir, order, hir_item);
    }
    for (int i = 0; i < pack.fields->count; ++i) {
        struct HirDecl *decl = pack.fields->data[i];
        struct HirFieldDecl *field = HirGetFieldDecl(decl);
        v_set_object(&key, field->name);
        Value *value = pawH_get(P, map, key);
        if (value == NULL) {
            NAME_ERROR(R, field->line, "missing initializer for field '%s' in struct '%s'",
                       field->name->text, pack.name->text);
        }
        const paw_Int index = v_int(*value);
        struct HirType *field_t = is_inference
                               ? field_types->data[i]
                               : HIR_TYPEOF(pack.fields->data[i]);
        struct HirExpr *item = order->data[index];
        struct HirType *item_t = expr_type(R, item);
        item->sitem.index = i;
        unify(R, item_t, field_t);
        pawH_remove(P, map, key);
    }
    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(map, &iter)) {
        const Value *pkey = pawH_key(map, cast_size(iter));
        NAME_ERROR(R, -1, "unexpected field '%s' in initializer for struct '%s'",
                   v_string(*pkey), pack.name->text);
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
    r->lit_kind = cast(e->lit_kind, enum HirLitKind);

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

static struct ItemSlot *register_func_item(struct Resolver *R, struct AstFuncDecl *d)
{
    struct HirDecl *result = pawHir_new_decl(R->hir, d->line, kHirFuncDecl);
    struct HirFuncDecl *r = HirGetFuncDecl(result);
    r->is_pub = d->is_pub;
    r->fn_kind = d->fn_kind;
    r->name = d->name;

    struct HirSymbol *symbol = new_global(R, d->name, result, d->is_pub);
    symbol->is_type = d->generics != NULL;
    struct HirScope *scope = register_func(R, d, r);
    return new_item_slot(R, AST_CAST_DECL(d), result, scope);
}

static void resolve_func_item(struct Resolver *R, struct ItemSlot *slot)
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

    r->lhs = resolve_expr(R, s->lhs);
    if (s->rhs != NULL) {
        r->rhs = resolve_expr(R, s->rhs);
        struct HirType *lhs = expr_type(R, r->lhs);
        struct HirType *rhs = expr_type(R, r->rhs);
        unify(R, lhs, rhs);
    }
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
    struct HirType *iter_t = expr_type(R, r->forin.target);
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

    struct HirType *target = expr_type(R, r->target);
    struct HirType *expect = NULL;
    if (is_vector_t(R, target)) {
        expect = get_type(R, PAW_TINT);
        r->type = e->is_slice ? target : hir_vector_elem(target);
    } else if (is_map_t(R, target)) {
        if (e->is_slice) {
            TYPE_ERROR(R, "slice operation not supported on map "
                          "(requires '[T]' or 'str')");
        }
        expect = hir_map_key(target);
        r->type = hir_map_value(target);
    } else if (!HirIsAdt(target)) {
        goto not_container;
    } else if (HirGetAdt(target)->base == PAW_TSTRING) {
        expect = get_type(R, PAW_TINT);
        r->type = get_type(R, PAW_TSTRING);
    } else {
    not_container:
        TYPE_ERROR(R, "type cannot be indexed (not a container)");
    }
    if (e->is_slice) {
        if (e->first != NULL) {
            r->first = resolve_expr(R, e->first);
            unify(R, expect, expr_type(R, r->first));
        }
        if (e->second != NULL) {
            r->second = resolve_expr(R, e->second);
            unify(R, expect, expr_type(R, r->second));
        }
    } else {
        r->first = resolve_expr(R, e->first);
        unify(R, expect, expr_type(R, r->first));
    }
    return result;
}

static struct HirExpr *resolve_selector(struct Resolver *R, struct AstSelector *e)
{
    struct HirExpr *result = pawHir_new_expr(R->hir, e->line, kHirSelector);
    struct HirSelector *r = HirGetSelector(result);
    r->target = resolve_expr(R, e->target);

    struct HirType *target = expr_type(R, r->target);
    if (HirIsTupleType(target)) {
        // tuples are indexed with "Expr" "." "int_lit"
        struct HirType *target = expr_type(R, r->target);
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
        NAME_ERROR(R, e->line, "unit structure '%s' has no fields", pack.name->text);
    }
    struct HirDecl *attr = expect_attr(R, &pack, e->name);
    r->type = HIR_TYPEOF(attr);
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
        default:
            r = resolve_container_type(R, AstGetContainerType(expr)); 
    }
    return normalize(R, r);
}

static struct HirExpr *resolve_expr(struct Resolver *R, struct AstExpr *expr)
{
    struct HirExpr *r;
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
        case kAstStructField:
            r = resolve_struct_field(R, AstGetStructField(expr));
            break;
        default:
            r = resolve_map_elem(R, AstGetMapElem(expr));
    }

    struct HirType *type = HIR_TYPEOF(r);
    r->hdr.type = normalize(R, type);
    return r;
}

static void add_basic_builtin(struct Resolver *R, String *name)
{
    const paw_Type code = FLAG2CODE(name->flag);
    struct HirType *type = R->hir->builtin[code];

    struct HirExpr *e = pawHir_new_expr(R->hir, 0, kHirPathExpr);
    e->path.path = pawHir_path_new(R->hir);
    pawHir_path_add(R->hir, e->path.path, name, NULL, type);

    struct HirDecl *d = pawHir_new_decl(R->hir, 0, kHirTypeDecl);
    d->type.name = name;
    d->type.line = 0;
    d->type.rhs = e;

    add_def(R, d);

    d->hdr.type = type;
    struct HirSymbol *symbol = new_global(R, name, d, PAW_TRUE);
    symbol->is_type = PAW_TRUE;
    symbol->is_init = PAW_TRUE;
}

// TODO: This whole thing is way too hacky. Reconcile with setup code in hir.c.
//       Same with setup_module...
static DefId add_container_builtin(struct Resolver *R, const char *name, paw_Type code,
                                   const char **generics)
{
    struct HirDecl *d = pawHir_new_decl(R->hir, 0, kHirAdtDecl);
    d->adt.line = 0;
    d->adt.generics = pawHir_decl_list_new(R->hir);
    d->adt.name = SCAN_STRING(R, name);
    d->adt.did = add_def(R, d);
    for (const char **pname = generics; *pname != NULL; ++pname) {
        struct HirDecl *g = pawHir_new_decl(R->hir, 0, kHirGenericDecl);
        g->generic.name = SCAN_STRING(R, *pname);
        g->generic.type = pawHir_new_type(R->hir, 0, kHirGeneric);
        g->generic.type->generic.name = g->generic.name;
        g->generic.type->generic.did = add_def(R, g);
        g->generic.line = 0;
        pawHir_decl_list_push(R->hir, d->adt.generics, g);
    }
    d->adt.fields = pawHir_decl_list_new(R->hir);
    d->adt.monos = pawHir_decl_list_new(R->hir);
    d->adt.type = R->hir->builtin[code];
    d->adt.type->adt.base = d->adt.did;
    d->adt.type->adt.did = d->adt.did;
    // TODO: These were originally created in hir.c, adding generics to them
    //       so they can be used with the normal template instantiation code.
    d->adt.type->adt.types = pawHir_type_list_new(R->hir);
    for (int i = 0; i < d->adt.generics->count; ++i) {
        pawHir_type_list_push(R->hir, d->adt.type->adt.types, HIR_TYPEOF(d->adt.generics->data[i]));
    }
    const int gid = R->symtab->globals->symbols->count;
    struct HirSymbol *symbol = new_global(R, d->adt.name, d, PAW_TRUE);
    symbol->is_type = PAW_TRUE;
    symbol->is_init = PAW_TRUE;
    return gid;
}

static DefId find_builtin(struct Resolver *R, const char *name)
{
    const String *str = SCAN_STRING(R, name);
    const struct HirSymbol *symbol = resolve_symbol(R, str);
    return symbol->decl->hdr.did;
}

static struct HirDeclList *register_items(struct Resolver *R, struct AstDeclList *items, struct ItemList **pslots)
{
    struct ItemList *slots = item_list_new(R->hir);
    struct HirDeclList *output = pawHir_decl_list_new(R->hir);
    for (int i = 0; i < items->count; ++i) {
        struct AstDecl *item = items->data[i];
        struct ItemSlot *slot;
        if (AstIsAdtDecl(item)) {
            slot = register_adt_item(R, AstGetAdtDecl(item));
        } else {
            slot = register_func_item(R, AstGetFuncDecl(item));
        }
        pawHir_decl_list_push(R->hir, output, slot->hir_decl);
        item_list_push(R->hir, slots, slot);
    }
    *pslots = slots;
    return output;
}

static void resolve_items(struct Resolver *R, struct ItemList *items)
{
    for (int i = 0; i < items->count; ++i) {
        struct ItemSlot *item = items->data[i];
        if (AstIsFuncDecl(item->ast_decl)) {
            resolve_func_item(R, item);
        }
    }
}

static struct HirDeclList *resolve_module(struct Resolver *R, struct AstDeclList *items)
{
    struct ItemList *slots;
    struct HirDeclList *output = register_items(R, items, &slots);
    resolve_items(R, slots);
    return output;
}

static void setup_module(struct Resolver *R)
{
    add_basic_builtin(R, CACHED_STR(R, CSTR_UNIT));
    add_basic_builtin(R, CACHED_STR(R, CSTR_BOOL));
    add_basic_builtin(R, CACHED_STR(R, CSTR_INT));
    add_basic_builtin(R, CACHED_STR(R, CSTR_FLOAT));
    add_basic_builtin(R, CACHED_STR(R, CSTR_STRING));
    R->vector_gid = add_container_builtin(
            R, "(Vector)", PAW_TVECTOR, (const char *[]){"T", NULL});
    R->map_gid = add_container_builtin(
            R, "(Map)", PAW_TMAP, (const char *[]){"K", "V", NULL});
    R->C->vector_did = find_builtin(R, "(Vector)");
    R->C->map_did = find_builtin(R, "(Map)");

    R->hir->prelude = resolve_module(R, R->ast->prelude);

    R->C->option_did = find_builtin(R, "Option");
    R->C->result_did = find_builtin(R, "Result");
}

static void visit_module(struct Resolver *R)
{
    struct Hir *hir = R->hir;
    struct HirSymtab *symtab = R->symtab;

    enter_block(R, NULL);
    enter_inference_ctx(R);

    setup_module(R);
    hir->items = resolve_module(R, R->ast->items);

    leave_inference_ctx(R);
    symtab->globals = leave_block(R);
    paw_assert(symtab->scopes->count == 0);

    pawHir_expand(R, hir);
}

struct Hir *p_resolve(struct Compiler *C, struct Ast *ast)
{
    struct Hir *hir = pawHir_new(C);
    C->dm->unifier.ast = ast;
    C->dm->unifier.hir = hir;
    C->dm->unifier.P = ENV(C);
    C->dm->hir = hir;

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
    visit_module(&R);
    return hir;
}
