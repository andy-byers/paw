// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// resolve.c: Implementation of the type checker.

#include "ast.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "env.h"
#include "error.h"
#include "gc.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "parse.h"
#include "str.h"
#include "type_folder.h"
#include "unify.h"

#define STRING_LIT(R_, Str_) SCAN_STRING((R_)->C, Str_)
#define TYPE2CODE(R_, Type_) pawP_type2code((R_)->C, Type_)

#define RESOLVER_ERROR(R_, Kind_, ...) pawErr_##Kind_((R_)->C, (R_)->m->name, __VA_ARGS__)

struct ResultState {
    struct ResultState *outer;
    struct IrType *prev;
    int count;
};

struct PatState {
    struct PatState *outer;
    StringMap *bound;
    enum HirPatKind kind;
};

struct MatchState {
    struct MatchState *outer;
    struct IrType *target;
    struct IrType *result;
    struct PatState *ps;
};

// Common state for type-checking routines
struct Resolver {
    paw_Env *P;
    struct Pool *pool;
    struct ModuleInfo *m;
    struct Unifier *U; // unification tables
    struct Compiler *C; // compiler state
    struct IrType *self; // enclosing ADT
    struct DynamicMem *dm; // dynamic memory
    struct ResultState *rs;
    struct MatchState *ms;
    struct HirSymtab *symtab;
    struct Substitution subst;
    struct Hir *hir;
    TraitMap *traits; // '.traits' from Compiler
    int func_depth; // number of nested functions
    paw_Bool in_closure; // 1 if the enclosing function is a closure, else 0
    paw_Bool in_impl;
};

static paw_Uint ident_hash(struct Resolver *R, struct HirIdent ident)
{
    return P_PTR_HASH(R, ident.name);
}

static paw_Bool ident_equals(struct Resolver *R, struct HirIdent a, struct HirIdent b)
{
    return pawS_eq(a.name, b.name);
}

DEFINE_MAP(struct Resolver, FieldMap, pawP_alloc, ident_hash, ident_equals, struct HirIdent, int)
DEFINE_MAP(struct Resolver, PatFieldMap, pawP_alloc, ident_hash, ident_equals, struct HirIdent, struct HirPat *)
DEFINE_MAP_ITERATOR(FieldMap, struct HirIdent, int)
DEFINE_MAP_ITERATOR(PatFieldMap, struct HirIdent, struct HirPat *)

static void resolve_stmt(struct Resolver *, struct HirStmt *);
static void resolve_decl(struct Resolver *, struct HirDecl *);
static struct IrType *resolve_expr(struct Resolver *, struct HirExpr *);
static struct IrType *resolve_type(struct Resolver *, struct HirType *, struct SourceSpan span);
static struct IrType *resolve_pat(struct Resolver *, struct HirPat *);

#define DEFINE_LIST_RESOLVER(name, T)                                                \
    static void resolve_##name##_list(struct Resolver *R, struct Hir##T##List *list) \
    {                                                                                \
        if (list == NULL)                                                            \
            return;                                                                  \
        for (int i = 0; i < list->count; ++i) {                                      \
            resolve_##name(R, list->data[i]);                                        \
        }                                                                            \
    }
DEFINE_LIST_RESOLVER(expr, Expr)
DEFINE_LIST_RESOLVER(decl, Decl)
DEFINE_LIST_RESOLVER(stmt, Stmt)
#undef DEFINE_LIST_RESOLVER

static struct IrTypeList *resolve_pat_list(struct Resolver *R, struct HirPatList *pats)
{
    struct IrTypeList *types = IrTypeList_new(R->C);
    if (pats != NULL) {
        IrTypeList_reserve(R->C, types, pats->count);

        struct HirPat *const *ppat;
        K_LIST_FOREACH (pats, ppat) {
            struct IrType *type = resolve_pat(R, *ppat);
            IrTypeList_push(R->C, types, type);
        }
    }
    return types;
}

static struct IrTypeList *resolve_type_list(struct Resolver *R, struct HirTypeList *list)
{
    if (list == NULL)
        return NULL;
    struct IrTypeList *result = IrTypeList_new(R->C);
    IrTypeList_reserve(R->C, result, list->count);

    struct HirType *const *ptype;
    K_LIST_FOREACH (list, ptype) {
        struct HirType *hir_type = *ptype;
        struct IrType *ir_type = resolve_type(R, hir_type, hir_type->hdr.span);
        IrTypeList_push(R->C, result, ir_type);
    }
    return result;
}

static struct IrType *normalize(struct Resolver *R, struct IrType *type)
{
    return pawU_normalize(R->U->table, type);
}

static paw_Bool equals(struct Resolver *R, struct IrType *a, struct IrType *b)
{
    return pawU_equals(R->U, a, b);
}

static void unify(struct Resolver *R, struct IrType *a, struct IrType *b)
{
    pawU_unify(R->U, a, b);
    normalize(R, a);
    normalize(R, b);
}

static paw_Bool is_list_t(struct Resolver *R, struct IrType *type)
{
    if (!IrIsAdt(type))
        return PAW_FALSE;
    return IR_TYPE_DID(type).value == R->C->builtins[BUILTIN_LIST].did.value;
}

static paw_Bool is_map_t(struct Resolver *R, struct IrType *type)
{
    if (!IrIsAdt(type))
        return PAW_FALSE;
    return IR_TYPE_DID(type).value == R->C->builtins[BUILTIN_MAP].did.value;
}

static struct HirDecl *get_decl(struct Resolver *R, DeclId did)
{
    return pawHir_get_decl(R->C, did);
}

static struct IrType *get_type(struct Resolver *R, enum BuiltinKind code)
{
    DeclId const did = R->C->builtins[code].did;
    return GET_NODE_TYPE(R->C, get_decl(R, did));
}

static paw_Bool is_unit_variant(struct Resolver *R, struct IrType *type)
{
    if (IrIsSignature(type)) {
        struct HirDecl *decl = get_decl(R, IrGetSignature(type)->did);
        return HirIsVariantDecl(decl) && HirGetVariantDecl(decl)->fields == NULL;
    }
    return PAW_FALSE;
}

static struct HirAdtDecl *get_adt(struct Resolver *R, struct IrType *type)
{
    struct HirDecl *decl = get_decl(R, IR_TYPE_DID(type));
    return HirGetAdtDecl(decl);
}

static struct IrTypeList *get_trait_bounds(struct Resolver *R, struct IrType *type)
{
    if (IrIsGeneric(type)) {
        return IrGetGeneric(type)->bounds;
    } else if (IrIsInfer(type)) {
        return IrGetInfer(type)->bounds;
    } else if (IrIsAdt(type)) {
        struct HirAdtDecl *d = HirGetAdtDecl(
            pawHir_get_decl(R->C, IR_TYPE_DID(type)));
        struct IrTypeList *bounds = IrTypeList_new(R->C);
        IrTypeList_reserve(R->C, bounds, d->traits->count);

        struct HirType *const *ptype;
        K_LIST_FOREACH (d->traits, ptype) {
            IrTypeList_push(R->C, bounds, GET_NODE_TYPE(R->C, *ptype));
        }
        return bounds;
    } else {
        return NULL;
    }
}

static paw_Bool implements_trait(struct Resolver *R, struct IrType *type, enum TraitKind kind)
{
    struct IrType *const *pbound;
    struct IrTypeList *bounds = get_trait_bounds(R, type);
    if (bounds == NULL)
        return PAW_FALSE;
    K_LIST_FOREACH (bounds, pbound) {
        struct HirDecl *decl = get_decl(R, IR_TYPE_DID(*pbound));
        struct HirTraitDecl *trait = HirGetTraitDecl(decl);
        if ((kind == TRAIT_HASH && pawS_eq(trait->ident.name, CSTR(R, CSTR_HASH)))
                || (kind == TRAIT_EQUALS && pawS_eq(trait->ident.name, CSTR(R, CSTR_EQUALS)))) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

// TODO: won't work for polymorphic traits
static void require_trait(struct Resolver *R, struct IrType *type, enum TraitKind kind)
{
    struct IrInfer *t = IrGetInfer(type);
    if (t->bounds == NULL)
        t->bounds = IrTypeList_new(R->C);
    enum BuiltinKind k = kind == TRAIT_HASH ? BUILTIN_HASH : BUILTIN_EQUALS;
    struct HirDecl *decl = get_decl(R, R->C->builtins[k].did);
    struct IrType *trait = GET_NODE_TYPE(R->C, decl);
    IrTypeList_push(R->C, t->bounds, trait);
}

static struct IrType *maybe_unit_variant(struct Resolver *R, struct SourceSpan span, struct IrType *type)
{
    if (IrIsSignature(type)) {
        // handle unit enumerators
        struct HirDecl *decl = get_decl(R, IrGetSignature(type)->did);
        if (HirIsVariantDecl(decl)) {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            if (d->fields != NULL) {
                struct IrVariantDef *def = pawIr_get_variant_def(R->C, d->did);
                RESOLVER_ERROR(R, missing_variant_args, span.start, def->name->text);
            }
            return IR_FPTR(type)->result;
        }
    }
    return type;
}

static struct IrType *resolve_operand(struct Resolver *R, struct HirExpr *expr)
{
    struct IrType *type = resolve_expr(R, expr);
    type = maybe_unit_variant(R, expr->hdr.span, type);
    SET_NODE_TYPE(R->C, expr, type);
    return type;
}

static struct IrTypeList *resolve_exprs(struct Resolver *R, struct HirExprList *list)
{
    if (list == NULL)
        return NULL;

    struct IrTypeList *new_list = IrTypeList_new(R->C);
    IrTypeList_reserve(R->C, new_list, list->count);

    struct HirExpr **pexpr;
    K_LIST_FOREACH (list, pexpr) {
        struct IrType *type = resolve_operand(R, *pexpr);
        IrTypeList_push(R->C, new_list, type);
    }
    return new_list;
}

static struct IrTypeList *collect_decl_types(struct Resolver *R, struct HirDeclList *list)
{
    if (list == NULL)
        return NULL;
    struct IrTypeList *new_list = IrTypeList_new(R->C);
    IrTypeList_reserve(R->C, new_list, list->count);

    struct HirDecl **pdecl;
    K_LIST_FOREACH (list, pdecl) {
        struct IrType *type = GET_NODE_TYPE(R->C, *pdecl);
        IrTypeList_push(R->C, new_list, type);
    }
    return new_list;
}

static struct IrType *new_unknown(struct Resolver *R, struct SourceLoc loc)
{
    return pawU_new_unknown(R->U, loc, NULL);
}

static void enter_inference_ctx(struct Resolver *R)
{
    pawU_enter_binder(R->U, R->hir->name);
}

static void leave_inference_ctx(struct Resolver *R)
{
    pawU_leave_binder(R->U);
}

static void enter_match_ctx(struct Resolver *R, struct MatchState *ms, struct SourceSpan span, struct IrType *target)
{
    *ms = (struct MatchState){
        .result = new_unknown(R, span.start),
        .target = target,
        .outer = R->ms,
    };
    R->ms = ms;
}

static void leave_match_ctx(struct Resolver *R)
{
    R->ms = R->ms->outer;
}

static struct HirScope *enclosing_scope(struct Resolver *R)
{
    paw_assert(R->symtab->count > 0);
    return K_LIST_LAST(R->symtab);
}

static int declare_local(struct Resolver *R, struct HirIdent ident, struct HirResult res)
{
    if (IS_KEYWORD(ident.name) || IS_BUILTIN(ident.name))
        RESOLVER_ERROR(R, reserved_identifier, ident.span.start, ident.name->text);
    return pawHir_declare_symbol(R->hir, enclosing_scope(R), ident, res);
}

static int find_field(struct HirDeclList *fields, String *name)
{
    if (fields == NULL)
        return -1;

    int index;
    struct HirDecl *const *pfield;
    K_LIST_ENUMERATE (fields, index, pfield) {
        if (pawS_eq(name, hir_decl_ident(*pfield).name))
            return index;
    }
    return -1;
}

static int new_local(struct Resolver *R, struct HirIdent ident, struct HirResult res)
{
    return declare_local(R, ident, res);
}

static void leave_scope(struct Resolver *R)
{
    HirScope_delete(R->hir, enclosing_scope(R));
    HirSymtab_pop(R->symtab);
}

static void enter_scope(struct Resolver *R, struct HirScope *scope)
{
    if (scope == NULL)
        scope = HirScope_new(R->hir);
    HirSymtab_push(R->hir, R->symtab, scope);
}

static void leave_function(struct Resolver *R)
{
    leave_scope(R);
    CHECK_GC(ENV(R));
}

static void enter_function(struct Resolver *R, struct HirFuncDecl *func)
{
    enter_scope(R, NULL);
    new_local(R, func->ident, (struct HirResult){
                .kind = HIR_RESULT_DECL,
                .did = func->did
            });
}

static void leave_pat(struct Resolver *R)
{
    struct PatState *ps = R->ms->ps;
    StringMap_delete(R->C, ps->bound);
    R->ms->ps = ps->outer;
}

static void enter_pat(struct Resolver *R, struct PatState *ps, enum HirPatKind kind)
{
    *ps = (struct PatState){
        .bound = StringMap_new_from(R->C, R->pool),
        .outer = R->ms->ps,
        .kind = kind,
    };
    R->ms->ps = ps;
}

static struct IrType *resolve_block(struct Resolver *R, struct HirBlock *block)
{
    enter_scope(R, NULL);
    resolve_stmt_list(R, block->stmts);
    struct IrType *type = resolve_operand(R, block->result);
    leave_scope(R);
    return type;
}

static void allocate_locals(struct Resolver *R, struct HirDeclList *decls)
{
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (decls, pdecl) {
        struct HirDecl *decl = *pdecl;
        new_local(R, hir_decl_ident(decl), (struct HirResult){
                .kind = HIR_RESULT_LOCAL,
                .hid = decl->hdr.hid
            });
    }
}

static void allocate_decls(struct Resolver *R, struct HirDeclList *decls)
{
    if (decls == NULL)
        return;

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (decls, pdecl) {
        struct HirDecl *decl = *pdecl;
        new_local(R, hir_decl_ident(decl), (struct HirResult){
                .kind = HIR_RESULT_DECL,
                .did = decl->hdr.did
            });
    }
}

static struct IrType *ResolveFieldDecl(struct Resolver *R, struct HirFieldDecl *d)
{
    return resolve_type(R, d->tag, d->span);
}

static paw_Bool is_unit_type(struct Resolver *R, struct IrType *type)
{
    return pawP_type2code(R->C, type) == BUILTIN_UNIT;
}

static void unify_block_result(struct Resolver *R, paw_Bool never, struct IrType *result, struct IrType *expect)
{
    if (!never || !is_unit_type(R, result))
        unify(R, result, expect);
}

static void resolve_func_item(struct Resolver *R, struct HirFuncDecl *d)
{
    enter_function(R, d);
    allocate_decls(R, d->generics);
    allocate_locals(R, d->params);
    struct IrType *ret = GET_NODE_TYPE(R->C, d->result);

    if (d->body != NULL) {
        struct ResultState rs = {
            // named function has explicit return type
            .outer = R->rs,
            .prev = ret,
        };
        R->rs = &rs;

        struct IrType *result = resolve_operand(R, d->body);
        unify_block_result(R, HirGetBlock(d->body)->never, result, ret);

        R->rs = rs.outer;
    }
    leave_function(R);
}

static void resolve_item(struct Resolver *R, struct HirDecl *item);
static struct IrType *resolve_path(struct Resolver *R, struct HirPath path, enum LookupKind kind);

static void resolve_methods(struct Resolver *R, struct HirDeclList *items, struct IrAdt *self)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (items, pitem) {
        resolve_item(R, *pitem);
    }
}

static void resolve_adt_methods(struct Resolver *R, struct HirAdtDecl *d)
{
    enter_scope(R, NULL);

    struct HirIdent const self = {
        .name = STRING_LIT(R, "Self"),
        .span = d->span,
    };
    new_local(R, self, (struct HirResult){
                .kind = HIR_RESULT_DECL,
                .did = d->self->hdr.did,
            });
    resolve_methods(R, d->methods, IrGetAdt(R->self));
    allocate_decls(R, d->methods);

    leave_scope(R);
}

struct IrType *pawP_instantiate_field(struct Compiler *C, struct IrType *inst_type, struct HirDecl *field)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    struct IrAdt *t = IrGetAdt(inst_type);
    struct HirDecl *decl = pawHir_get_decl(C, t->did);
    if (HirGetAdtDecl(decl)->generics == NULL)
        return GET_NODE_TYPE(C, field);

    struct IrType *base_type = GET_NODE_TYPE(C, decl);
    pawP_init_substitution_folder(&F, C, &subst, ir_adt_types(base_type), t->types);
    return pawIr_fold_type(&F, GET_NODE_TYPE(C, field));
}

static struct IrTypeList *instantiate_fields(struct Compiler *C, struct IrType *self, struct HirDeclList *fields)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    struct IrAdt *t = IrGetAdt(self);
    struct HirDecl *decl = pawHir_get_decl(C, t->did);
    struct IrType *type = GET_NODE_TYPE(C, decl);
    pawP_init_substitution_folder(&F, C, &subst, ir_adt_types(type), IrGetAdt(self)->types);
    struct IrTypeList *field_types = pawHir_collect_decl_types(C, fields);
    return pawIr_fold_type_list(&F, field_types);
}

struct HirDecl *pawP_find_field(struct Compiler *C, struct IrType *self, String *name)
{
    struct IrAdt *t = IrGetAdt(self);
    struct HirDecl *decl = pawHir_get_decl(C, t->did);
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    int const index = find_field(d->fields, name);
    if (index < 0)
        return NULL;
    return HirDeclList_get(d->fields, index);
}

static void unify_unit_type(struct Resolver *R, struct IrType *type)
{
    unify(R, type, get_type(R, BUILTIN_UNIT));
}

static void expect_bool_expr(struct Resolver *R, struct HirExpr *e)
{
    struct IrType *type = resolve_operand(R, e);
    unify(R, type, get_type(R, BUILTIN_BOOL));
}

static void expect_int_expr(struct Resolver *R, struct HirExpr *e)
{
    struct IrType *type = resolve_operand(R, e);
    unify(R, type, get_type(R, BUILTIN_INT));
}

static struct IrType *instantiate(struct Resolver *R, struct HirDecl *base, struct IrTypeList *types)
{
    return pawP_instantiate(R->C, GET_NODE_TYPE(R->C, base), types);
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct IrType *lookup_path(struct Resolver *R, struct HirPath path, enum LookupKind kind)
{
    return pawP_lookup(R->C, R->m, R->symtab, &path, kind, PAW_FALSE);
}

static struct IrType *resolve_path(struct Resolver *R, struct HirPath path, enum LookupKind kind)
{
    struct IrType *type = lookup_path(R, path, kind);
    if (type == NULL) {
        char const *pathname = pawHir_print_path(R->C, &path);
        RESOLVER_ERROR(R, unknown_path, path.span.start, pathname);
    }
    return type;
}

static void maybe_fix_unit_struct(struct Resolver *R, struct IrType *type, struct HirExpr *expr)
{
    paw_assert(IrIsAdt(type));
    struct IrAdtDef const *adt = pawIr_get_adt_def(R->C, IR_TYPE_DID(type));
    enum BuiltinKind const code = TYPE2CODE(R, type);
    if (IS_BUILTIN_TYPE(code) || !adt->is_struct)
        RESOLVER_ERROR(R, expected_value, expr->hdr.span.start, adt->name->text);

    paw_assert(adt->variants->count == 1);
    struct IrVariantDef *variant = IrVariantDefs_get(adt->variants, 0);
    if (variant->fields->count > 0)
        RESOLVER_ERROR(R, missing_fields, expr->hdr.span.start, adt->name->text);

    struct HirPath path = HirGetPathExpr(expr)->path;
    expr->hdr.kind = kHirLiteralExpr;
    struct HirLiteralExpr *r = HirGetLiteralExpr(expr);
    r->lit_kind = kHirLitComposite;
    r->comp.items = HirExprList_new(R->hir);
    r->comp.path = path;
}

static struct IrType *resolve_path_expr(struct Resolver *R, struct HirPathExpr *e)
{
    // path might refer to a unit ADT, so we have to use LOOKUP_EITHER
    struct IrType *type = resolve_path(R, e->path, LOOKUP_EITHER);
    struct HirResult const res = HIR_PATH_RESULT(e->path);
    if (res.kind == HIR_RESULT_LOCAL)
        return type;

    struct HirDecl *result = get_decl(R, res.did);
    if (HirIsAdtDecl(result)) {
        maybe_fix_unit_struct(R, type, HIR_CAST_EXPR(e));
    } else if (HirIsGenericDecl(result)) {
        String const *name = hir_decl_ident(result).name;
        RESOLVER_ERROR(R, expected_value, result->hdr.span.start, name->text);
    }
    return type;
}

static struct IrType *resolve_logical_expr(struct Resolver *R, struct HirLogicalExpr *e)
{
    expect_bool_expr(R, e->lhs);
    expect_bool_expr(R, e->rhs);
    return get_type(R, BUILTIN_BOOL);
}

static struct IrType *fresh_option(struct Resolver *R)
{
    struct HirDecl *decl = pawHir_get_decl(R->C, R->C->builtins[BUILTIN_OPTION].did);
    struct IrType *type = GET_NODE_TYPE(R->C, decl);
    return pawP_generalize(R->C, type);
}

static paw_Bool is_option_t(struct Resolver *R, struct IrType *type)
{
    return IrIsAdt(type) && IR_TYPE_DID(type).value == R->C->builtins[BUILTIN_OPTION].did.value;
}

static paw_Bool is_result_t(struct Resolver *R, struct IrType *type)
{
    return IrIsAdt(type) && IR_TYPE_DID(type).value == R->C->builtins[BUILTIN_RESULT].did.value;
}

static struct IrType *resolve_chain_expr(struct Resolver *R, struct HirChainExpr *e)
{
    struct IrType *type = resolve_operand(R, e->target);
    paw_assert(R->rs->prev != NULL);

    if (!is_option_t(R, type) && !is_result_t(R, type)) {
        char const *repr = pawIr_print_type(R->C, type);
        RESOLVER_ERROR(R, invalid_chain_operand, e->span.start, repr);
    }

    unify(R, R->rs->prev, type);
    return K_LIST_FIRST(ir_adt_types(type)); // unwrap
}

static struct IrType *get_value_type(struct Resolver *R, struct IrType *target)
{
    if (is_list_t(R, target))
        return ir_list_elem(target);
    if (is_map_t(R, target))
        return ir_map_value(target);
    return NULL;
}

static paw_Bool is_bool_unop(enum UnaryOp op)
{
    switch (op) {
        case UNARY_NOT:
            return PAW_TRUE;
        default:
            return PAW_FALSE;
    }
}

static paw_Bool is_bool_binop(enum BinaryOp op)
{
    switch (op) {
        case BINARY_EQ:
        case BINARY_NE:
        case BINARY_LT:
        case BINARY_LE:
        case BINARY_GT:
        case BINARY_GE:
            return PAW_TRUE;
        default:
            return PAW_FALSE;
    }
}

static struct HirExpr *new_literal_field(struct Resolver *R, const char *name, struct HirExpr *expr, int fid)
{
    struct HirIdent const ident = {
        .name = STRING_LIT(R, name),
        .span = expr->hdr.span,
    };
    return pawHir_new_named_field_expr(R->m->hir, expr->hdr.span, ident, expr, fid);
}

static struct IrType *transform_range(struct Resolver *R, struct HirBinOpExpr *e, struct IrType *inner)
{
    struct Hir *hir = R->m->hir;
    struct SourceSpan const span = {
        .start = e->lhs->hdr.span.start,
        .end = e->rhs->hdr.span.end,
    };
    struct HirIdent const ident = {
        .name = CSTR(R, CSTR_RANGE),
        .span = e->lhs->hdr.span,
    };
    struct HirPath path;
    pawHir_path_init(hir, &path, ident.span);
    pawHir_path_add(hir, &path, ident, NULL);

    struct HirExprList *items = HirExprList_new(R->hir);
    HirExprList_push(R->hir, items, new_literal_field(R, "first", e->lhs, 0));
    HirExprList_push(R->hir, items, new_literal_field(R, "second", e->rhs, 1));

    e->kind = kHirLiteralExpr; // transform to composite literal
    struct HirLiteralExpr *lit = HirGetLiteralExpr(HIR_CAST_EXPR(e));
    lit->lit_kind = kHirLitComposite;
    lit->comp.items = items;
    lit->comp.path = path;

    struct IrTypeList *types = IrTypeList_new(R->C);
    IrTypeList_push(R->C, types, inner);

    DeclId const did = R->C->builtins[BUILTIN_RANGE].did;
    return pawIr_new_adt(R->C, did, types);
}

static struct IrType *resolve_unop_expr(struct Resolver *R, struct HirUnOpExpr *e)
{
    static uint8_t const VALID_OPS[][NBUILTINS] = {
        //     type = {0, b, i, f, s, l, m}
        [UNARY_LEN] = {0, 0, 0, 0, 1, 1, 1},
        [UNARY_NEG] = {0, 0, 1, 1, 0, 0, 0},
        [UNARY_NOT] = {0, 1, 0, 0, 0, 0, 0},
        [UNARY_BNOT] = {0, 0, 1, 0, 0, 0, 0},
    };

    static char const *UNOP_REPR[] = {
        [UNARY_LEN] = "#",
        [UNARY_NEG] = "-",
        [UNARY_NOT] = "!",
        [UNARY_BNOT] = "~",
    };

    struct IrType *type = resolve_operand(R, e->target);
    enum BuiltinKind const code = TYPE2CODE(R, type);
    if (!IS_BUILTIN_TYPE(code) || !VALID_OPS[e->op][code]) {
        char const *operand = pawIr_print_type(R->C, type);
        RESOLVER_ERROR(R, invalid_unary_operand, e->span.start, operand, UNOP_REPR[e->op]);
    } else if (is_bool_unop(e->op)) {
        return get_type(R, BUILTIN_BOOL);
    } else if (e->op == UNARY_LEN) {
        return get_type(R, BUILTIN_INT);
    } else {
        return type;
    }
}

static struct IrType *resolve_binop_expr(struct Resolver *R, struct HirBinOpExpr *e)
{
    static uint8_t const VALID_OPS[][NBUILTINS] = {
        //     type = {0, b, i, f, s, l, m}
        [BINARY_EQ] = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_NE] = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_LT] = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_LE] = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_GT] = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_GE] = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_ADD] = {0, 0, 1, 1, 1, 1, 0},
        [BINARY_SUB] = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_MUL] = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_DIV] = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_MOD] = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_BXOR] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_BAND] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_BOR] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_SHL] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_SHR] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_RANGE] = {0, 1, 1, 1, 1, 0, 0},
    };

    static char const *BINOP_REPR[] = {
        [BINARY_EQ] = "==",
        [BINARY_NE] = "!=",
        [BINARY_LT] = "<",
        [BINARY_LE] = "<=",
        [BINARY_GT] = ">",
        [BINARY_GE] = ">=",
        [BINARY_ADD] = "+",
        [BINARY_SUB] = "-",
        [BINARY_MUL] = "*",
        [BINARY_DIV] = "/",
        [BINARY_MOD] = "%",
        [BINARY_BXOR] = "^",
        [BINARY_BAND] = "&",
        [BINARY_BOR] = "|",
        [BINARY_SHL] = "<<",
        [BINARY_SHR] = ">>",
        [BINARY_RANGE] = "..",
    };

    struct IrType *lhs = resolve_operand(R, e->lhs);
    struct IrType *rhs = resolve_operand(R, e->rhs);
    unify(R, lhs, rhs);

    enum BuiltinKind const code = TYPE2CODE(R, lhs);
    paw_assert(code == TYPE2CODE(R, rhs));
    if (!IS_BUILTIN_TYPE(code) || !VALID_OPS[e->op][code]) {
        char const *operand = pawIr_print_type(R->C, lhs);
        RESOLVER_ERROR(R, invalid_binary_operand, e->span.start, operand, BINOP_REPR[e->op]);
    } else if (is_bool_binop(e->op)) {
        return get_type(R, BUILTIN_BOOL);
    } else if (e->op == BINARY_RANGE) {
        return transform_range(R, e, lhs);
    } else {
        return lhs;
    }
}

static struct IrType *resolve_assign_expr(struct Resolver *R, struct HirAssignExpr *e)
{
    paw_assert(HirIsPathExpr(e->lhs) || HirIsIndex(e->lhs) || HirIsSelector(e->lhs));
    struct IrType *lhs = resolve_operand(R, e->lhs);
    struct IrType *rhs = resolve_operand(R, e->rhs);
    unify(R, lhs, rhs);
    return get_type(R, BUILTIN_UNIT);
}

static struct IrType *resolve_match_expr(struct Resolver *R, struct HirMatchExpr *e)
{
    struct IrType *target = resolve_operand(R, e->target);

    struct MatchState ms;
    enter_match_ctx(R, &ms, e->target->hdr.span, target);
    resolve_expr_list(R, e->arms);
    leave_match_ctx(R);

    struct IrType *result = ms.result;
    if (IrIsInfer(result) && e->never)
        unify_unit_type(R, result);

    return result;
}

static struct IrType *resolve_match_arm(struct Resolver *R, struct HirMatchArm *e)
{
    enter_scope(R, NULL);

    struct IrType *pat = resolve_pat(R, e->pat);
    unify(R, pat, R->ms->target);
    if (e->guard != NULL)
        expect_bool_expr(R, e->guard);
    struct IrType *result = resolve_operand(R, e->result);
    unify_block_result(R, e->never, result, R->ms->result);

    leave_scope(R);
    return result;
}

static struct IrType *new_list_t(struct Resolver *R, struct IrType *elem_t)
{
    struct HirDecl *base = get_decl(R, R->C->builtins[BUILTIN_LIST].did);
    struct IrTypeList *types = IrTypeList_new(R->C);
    IrTypeList_push(R->C, types, elem_t);
    return instantiate(R, base, types);
}

static struct IrType *new_map_t(struct Resolver *R, struct IrType *key_t, struct IrType *value_t, struct SourceSpan span)
{
    struct HirDecl *base = get_decl(R, R->C->builtins[BUILTIN_MAP].did);
    struct IrTypeList *types = IrTypeList_new(R->C);
    IrTypeList_push(R->C, types, key_t);
    IrTypeList_push(R->C, types, value_t);
    return instantiate(R, base, types);
}

static void resolve_closure_param(struct Resolver *R, struct HirFieldDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    struct IrType *type = resolve_type(R, d->tag, d->span);
    new_local(R, d->ident, (struct HirResult){
                .kind = HIR_RESULT_LOCAL,
                .hid = d->hid,
            });
    SET_NODE_TYPE(R->C, decl, type);
}

static struct IrType *resolve_closure_expr(struct Resolver *R, struct HirClosureExpr *e)
{
    struct ResultState rs = {R->rs};
    R->rs = &rs;
    enter_scope(R, NULL);

    for (int i = 0; i < e->params->count; ++i) {
        struct HirDecl *decl = e->params->data[i];
        struct HirFieldDecl *d = HirGetFieldDecl(decl);
        resolve_closure_param(R, d);
    }
    struct IrType *ret = resolve_type(R, e->result, e->span);

    struct IrTypeList *params = collect_decl_types(R, e->params);
    rs.prev = ret;

    if (HirIsBlock(e->expr)) {
        struct IrType *result = resolve_operand(R, e->expr);
        unify_block_result(R, HirGetBlock(e->expr)->never, result, ret);
    } else {
        unify(R, ret, resolve_operand(R, e->expr));
    }

    leave_scope(R);
    R->rs = rs.outer;
    return pawIr_new_func_ptr(R->C, params, ret);
}

static struct HirDecl *find_method_aux(struct Compiler *C, struct HirDecl *base, String *name)
{
    struct HirDecl **pdecl;
    struct HirAdtDecl *adt = HirGetAdtDecl(base);
    K_LIST_FOREACH (adt->methods, pdecl) {
        struct HirFuncDecl *method = HirGetFuncDecl(*pdecl);
        if (pawS_eq(name, method->ident.name))
            return *pdecl;
    }
    return NULL;
}

struct IrType *pawP_find_method(struct Compiler *C, struct IrType *base, String *name)
{
    struct HirDecl *decl = pawHir_get_decl(C, IR_TYPE_DID(base));
    struct HirDecl *method = find_method_aux(C, decl, name);
    if (method == NULL)
        return NULL;
    if (!HIR_IS_POLY_ADT(decl))
        return GET_NODE_TYPE(C, method);
    return pawP_instantiate_method(C, decl, IR_TYPE_SUBTYPES(base), method);
}

static void resolve_adt_item(struct Resolver *R, struct HirAdtDecl *d)
{
    enter_scope(R, NULL);

    allocate_decls(R, d->generics);
    R->self = pawIr_get_type(R->C, d->hid);
    resolve_adt_methods(R, d);

    leave_scope(R);
    R->self = NULL;
}

static void ResolveLetStmt(struct Resolver *R, struct HirLetStmt *s)
{
    struct IrType *tag = resolve_type(R, s->tag, s->span);
    struct IrType *rhs = s->init != NULL
                              ? resolve_operand(R, s->init)
                              : new_unknown(R, s->span.start);
    struct MatchState ms;
    enter_match_ctx(R, &ms, s->span, tag);
    struct IrType *lhs = resolve_pat(R, s->pat);
    unify(R, ms.result, tag);
    leave_match_ctx(R);

    unify(R, lhs, tag);
    unify(R, tag, rhs);

    pawIr_set_type(R->C, s->hid, rhs);
}

static void const_check_path(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct Resolver *R = V->ud;

    struct IrType *type = pawP_lookup(R->C, R->m, R->symtab, &e->path, LOOKUP_EITHER, PAW_FALSE);
    enum BuiltinKind kind = TYPE2CODE(R, type);
    if (!IS_BASIC_TYPE(kind)) {
        char const *repr = pawIr_print_type(R->C, type);
        RESOLVER_ERROR(R, nonprimitive_constant, e->span.start, repr);
    }
}

static void const_check_unop(struct HirVisitor *V, struct HirUnOpExpr *e)
{
}

static void const_check_binop(struct HirVisitor *V, struct HirBinOpExpr *e)
{
}

static void const_check_closure(struct HirVisitor *V, struct HirClosureExpr *e)
{
    struct Resolver *R = V->ud;
    RESOLVER_ERROR(R, cannot_constant_evaluate, e->span.start, "closure");
}

static void const_check_call(struct HirVisitor *V, struct HirCallExpr *e)
{
    struct Resolver *R = V->ud;
    RESOLVER_ERROR(R, cannot_constant_evaluate, e->span.start, "function call");
}

static void const_check_index(struct HirVisitor *V, struct HirIndex *e)
{
    struct Resolver *R = V->ud;
    RESOLVER_ERROR(R, cannot_constant_evaluate, e->span.start, "index expression");
}

static void const_check_selector(struct HirVisitor *V, struct HirSelector *e)
{
    struct Resolver *R = V->ud;
    RESOLVER_ERROR(R, cannot_constant_evaluate, e->span.start, "selector expression");
}

static void const_check_field(struct HirVisitor *V, struct HirFieldExpr *e)
{
    struct Resolver *R = V->ud;
    RESOLVER_ERROR(R, cannot_constant_evaluate, e->span.start, "field expression");
}

static void const_check_loop(struct HirVisitor *V, struct HirLoopExpr *e)
{
    struct Resolver *R = V->ud;
    RESOLVER_ERROR(R, cannot_constant_evaluate, e->span.start, "loop");
}

// Make sure the initializer of a global constant can be computed at compile time
static void check_const(struct Resolver *R, struct HirExpr *expr, struct IrType *type)
{
    struct HirVisitor V;
    pawHir_visitor_init(&V, R->hir, R);
    V.PostVisitUnOpExpr = const_check_unop;
    V.PostVisitBinOpExpr = const_check_binop;
    V.PostVisitClosureExpr = const_check_closure;
    V.PostVisitCallExpr = const_check_call;
    V.PostVisitIndex = const_check_index;
    V.PostVisitSelector = const_check_selector;
    V.PostVisitFieldExpr = const_check_field;
    V.PostVisitLoopExpr = const_check_loop;
    pawHir_visit_expr(&V, expr);

    enum BuiltinKind kind = TYPE2CODE(R, type);
    if (!IS_BASIC_TYPE(kind)) {
        char const *repr = pawIr_print_type(R->C, type);
        RESOLVER_ERROR(R, nonprimitive_constant, expr->hdr.span.start, repr);
    }
}

static void resolve_const_item(struct Resolver *R, struct HirConstDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    struct IrType *tag = GET_NODE_TYPE(R->C, d->tag);
    if (d->init != NULL) {
        struct IrType *init = resolve_operand(R, d->init);
        unify(R, init, tag);
        check_const(R, d->init, tag);
    }
    pawIr_set_type(R->C, d->hid, tag);
}

static struct IrType *resolve_field_decl(struct Resolver *R, struct HirFieldDecl *d)
{
    return resolve_type(R, d->tag, d->span);
}

static void register_generics(struct Resolver *R, struct HirDeclList *generics)
{
    if (generics == NULL)
        return;

    struct HirDecl **pdecl;
    K_LIST_FOREACH (generics, pdecl) {
        struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
        struct IrType *type = pawIr_new_generic(R->C, d->did, NULL);
        SET_NODE_TYPE(R->C, *pdecl, type);
    }
}

static struct IrType *resolve_type_decl(struct Resolver *R, struct HirTypeDecl *d)
{
    enter_scope(R, NULL);
    register_generics(R, d->generics);
    allocate_decls(R, d->generics);
    struct IrType *type = resolve_type(R, d->rhs, d->span);
    SET_NODE_TYPE(R->C, d->rhs, type);
    leave_scope(R);

    new_local(R, d->ident, (struct HirResult){
                .kind = HIR_RESULT_DECL,
                .did = d->did,
            });
    return type;
}

static paw_Bool is_polymorphic(struct Resolver *R, struct IrType *type)
{
    if (!IrIsSignature(type) && !IrIsAdt(type))
        return PAW_FALSE;
    struct HirDecl *base = get_decl(R, IR_TYPE_DID(type));
    struct IrType *base_type = GET_NODE_TYPE(R->C, base);
    if (HIR_IS_POLY_ADT(base)) {
        return pawU_equals(R->U, base_type, type);
    } else if (HIR_IS_POLY_FUNC(base)) {
        // NOTE: type arguments on signatures not checked by pawU_equals
        return pawU_list_equals(R->U,
                                ir_signature_types(base_type),
                                ir_signature_types(type));
    }
    return PAW_FALSE;
}

static paw_Bool is_adt_self(struct Resolver *R, struct IrType *adt)
{
    return R->self != NULL
               ? pawU_is_compat(R->C->U, R->self, adt)
               : PAW_FALSE;
}

static void ensure_accessible_field(struct Resolver *R, struct HirDecl *field, struct HirDecl *base, struct IrType *type)
{
    struct HirIdent const ident = hir_decl_ident(field);
    paw_Bool const is_pub = HirIsFieldDecl(field)
                                ? HirGetFieldDecl(field)->is_pub
                                : HirGetFuncDecl(field)->is_pub;
    if (is_pub || is_adt_self(R, type))
        return; // field is public or control is inside own impl block
    RESOLVER_ERROR(R, associated_item_visibility, ident.span.start, ident.name->text,
            HirGetAdtDecl(base)->ident.name->text);
}

static struct IrType *select_field(struct Resolver *R, struct IrType *target, struct HirSelector *e)
{
    if (IrIsTuple(target)) {
        // tuples are indexed with "Expr" "." "int_lit"
        struct IrTypeList *types = IrGetTuple(target)->elems;
        if (!e->is_index)
            RESOLVER_ERROR(R, expected_element_selector, e->ident.span.start);
        if (e->index >= types->count)
            RESOLVER_ERROR(R, element_selector_out_of_range, e->span.start,
                    e->index, types->count);
        return IrTypeList_get(types, e->index);
    }
    if (!IrIsAdt(target)) {
        char const *repr = pawIr_print_type(R->C, target);
        RESOLVER_ERROR(R, expected_adt, e->target->hdr.span.start, repr);
    }
    if (e->is_index)
        RESOLVER_ERROR(R, expected_field_selector, e->ident.span.start);

    struct HirAdtDecl *adt = get_adt(R, target);
    int const index = find_field(adt->fields, e->ident.name);
    if (index < 0)
        RESOLVER_ERROR(R, unknown_field, e->ident.span.start,
                e->ident.name->text, adt->ident.name->text);

    // refer to the field using its index from now on
    struct HirDecl *field = HirDeclList_get(adt->fields, index);
    struct IrType *result = pawP_instantiate_field(R->C, target, field);
    e->is_index = PAW_TRUE;
    e->index = index;

    ensure_accessible_field(R, field, HIR_CAST_DECL(adt), target);
    return result;
}

static struct IrType *resolve_call_target(struct Resolver *R, struct HirExpr *target, int *pparam_offset)
{
    *pparam_offset = 0;
    if (!HirIsSelector(target))
        // normal function call (no receiver)
        return resolve_expr(R, target);

    struct HirSelector *select = HirGetSelector(target);
    struct IrType *self = resolve_operand(R, select->target);

    struct IrType *method;
    if (IrIsGeneric(self)) {
        struct IrGeneric *g = IrGetGeneric(self);
        method = pawIr_resolve_trait_method(R->C, g, select->ident.name);
    } else if (IrIsAdt(self)) {
        method = pawP_find_method(R->C, self, select->ident.name);
    } else {
        return select_field(R, self, select);
    }
    if (method == NULL)
        RESOLVER_ERROR(R, unknown_method, select->ident.span.start,
                select->ident.name->text, pawIr_print_type(R->C, self));

    struct HirDecl *func_decl = get_decl(R, IR_TYPE_DID(method));
    struct HirDecl *self_decl = get_decl(R, IR_TYPE_DID(self));
    if (HirGetFuncDecl(func_decl)->is_assoc)
        RESOLVER_ERROR(R, not_a_method, target->hdr.span.start,
                   hir_decl_ident(func_decl).name->text);

    ensure_accessible_field(R, func_decl, self_decl, method);
    *pparam_offset = 1;
    return method;
}

// Resolve a function call or enumerator constructor
static struct IrType *resolve_call_expr(struct Resolver *R, struct HirCallExpr *e)
{
    int param_offset; // offset of first non-receiver parameter
    struct IrType *target = resolve_call_target(R, e->target, &param_offset);
    if (!IR_IS_FUNC_TYPE(target)) {
        char const *repr = pawIr_print_type(R->C, target);
        RESOLVER_ERROR(R, not_callable, e->target->hdr.span.start, repr);
    }
    SET_NODE_TYPE(R->C, e->target, target);

    struct IrFuncPtr const *fptr = IR_FPTR(target);
    int const nparams = fptr->params->count - param_offset;
    if (e->args->count != nparams)
        RESOLVER_ERROR(R, incorrect_arity, e->span.start, nparams, e->args->count);

    struct IrType *type;
    struct IrTypeList *params;
    if (is_polymorphic(R, target)) {
        struct HirDecl *decl = get_decl(R, IR_TYPE_DID(target));
        paw_assert(HIR_IS_POLY_FUNC(decl));
        target = pawP_generalize(R->C, target);
        params = IR_FPTR(target)->params;
        type = IR_FPTR(target)->result;
        SET_NODE_TYPE(R->C, e->target, target);
    } else {
        params = IR_FPTR(target)->params;
        type = IR_FPTR(target)->result;
    }

    if (is_unit_variant(R, target)) {
        char const *repr = pawIr_print_type(R->C, type);
        RESOLVER_ERROR(R, unit_variant_with_parenthesis, e->target->hdr.span.start, repr);
    }

    int index;
    struct HirExpr *const *parg;
    K_LIST_ENUMERATE (e->args, index, parg) {
        struct IrType *param = IrTypeList_get(params, index + param_offset);
        unify(R, param, resolve_operand(R, *parg));
    }

    return type;
}

static struct IrType *resolve_conversion_expr(struct Resolver *R, struct HirConversionExpr *e)
{
    resolve_operand(R, e->arg);
    return get_type(R, e->to);
}

static struct IrType *resolve_basic_lit(struct Resolver *R, struct HirBasicLit *e)
{
    return get_type(R, e->code);
}

static struct IrTypeList *resolve_operand_list(struct Resolver *R, struct HirExprList *list)
{
    if (list == NULL)
        return NULL;
    struct IrTypeList *new_list = IrTypeList_new(R->C);
    IrTypeList_reserve(R->C, new_list, list->count);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (list, pexpr) {
        struct IrType *type = resolve_operand(R, *pexpr);
        IrTypeList_push(R->C, new_list, type);
    }
    return new_list;
}

static struct IrType *resolve_tuple_lit(struct Resolver *R, struct HirTupleLit *e)
{
    struct IrTypeList *elems = resolve_operand_list(R, e->elems);
    if (elems->count == 0)
        return get_type(R, BUILTIN_UNIT);
    return pawIr_new_tuple(R->C, elems);
}

static struct IrType *resolve_list_lit(struct Resolver *R, struct HirContainerLit *e, struct SourceSpan span)
{
    struct IrType *elem_t = new_unknown(R, span.start);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (e->items, pexpr) {
        struct IrType *v = resolve_operand(R, *pexpr);
        unify(R, elem_t, v);
    }
    return new_list_t(R, elem_t);
}

static struct IrType *resolve_map_lit(struct Resolver *R, struct HirContainerLit *e, struct SourceSpan span)
{
    struct IrType *key_t = new_unknown(R, span.start);
    struct IrType *value_t = new_unknown(R, span.start);
    require_trait(R, key_t, TRAIT_HASH);
    require_trait(R, key_t, TRAIT_EQUALS);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (e->items, pexpr) {
        struct HirFieldExpr *field = HirGetFieldExpr(*pexpr);
        paw_assert(field->fid == -1);
        struct IrType *k = resolve_operand(R, field->key);
        struct IrType *v = resolve_operand(R, field->value);
        unify(R, key_t, k);
        unify(R, value_t, v);
        SET_NODE_TYPE(R->C, *pexpr, v);
    }
    return new_map_t(R, key_t, value_t, span);
}

static struct IrType *resolve_container_lit(struct Resolver *R, struct HirContainerLit *e, struct SourceSpan span)
{
    if (e->code == BUILTIN_LIST) {
        return resolve_list_lit(R, e, span);
    } else {
        paw_assert(e->code == BUILTIN_MAP);
        return resolve_map_lit(R, e, span);
    }
}

static struct IrType *resolve_field_expr(struct Resolver *R, struct HirFieldExpr *e)
{
    if (e->fid < 0)
        resolve_operand(R, e->key);
    return resolve_operand(R, e->value);
}

static struct HirExprList *collect_field_exprs(struct Resolver *R, struct HirExprList *items, FieldMap *map, String const *adt)
{
    struct HirExprList *order = HirExprList_new(R->hir);
    HirExprList_reserve(R->hir, order, items->count);

    int index;
    struct HirExpr *const *pexpr;
    K_LIST_ENUMERATE (items, index, pexpr) {
        struct HirIdent ident = HirGetFieldExpr(*pexpr)->ident;
        if (FieldMap_get(R, map, ident) != NULL)
            RESOLVER_ERROR(R, duplicate_field, ident.span.start, ident.name->text, adt->text);

        FieldMap_insert(R, map, ident, index);
        HirExprList_push(R->hir, order, *pexpr);
    }
    return order;
}

static struct IrTypeList *subst_types(struct Resolver *R, struct IrTypeList *before, struct IrTypeList *after, struct IrTypeList *target)
{
    if (before == NULL)
        return target;
    paw_assert(before->count == after->count);

    struct IrTypeFolder F;
    pawP_init_substitution_folder(&F, R->C, &R->subst, before, after);
    return pawIr_fold_type_list(&F, target);
}

static struct IrType *resolve_composite_lit(struct Resolver *R, struct HirCompositeLit *e, struct SourceSpan span)
{
    struct IrType *type = resolve_path(R, e->path, LOOKUP_TYPE);
    if (!IrIsAdt(type)) {
        char const *repr = pawIr_print_type(R->C, type);
        RESOLVER_ERROR(R, expected_struct, span.start, repr);
    }
    struct HirDecl *decl = get_decl(R, IR_TYPE_DID(type));

    // Use a temporary Map to avoid searching repeatedly through the list of fields.
    FieldMap *map = FieldMap_new(R);

    Value key;
    struct HirAdtDecl *adt = HirGetAdtDecl(decl);
    struct IrTypeList *field_types = pawHir_collect_decl_types(R->C, adt->fields);
    if (!adt->is_struct)
        RESOLVER_ERROR(R, expected_struct, adt->ident.span.start, adt->ident.name->text);
    if (adt->fields->count == 0)
        RESOLVER_ERROR(R, unit_struct_with_braces, adt->ident.span.start, adt->ident.name->text);

    struct IrType *base_type = pawIr_get_type(R->C, adt->hid);
    field_types = subst_types(R, ir_adt_types(base_type), ir_adt_types(type), field_types);
    struct HirExprList *order = collect_field_exprs(R, e->items, map, adt->ident.name);

    int index = 0;
    struct IrType *const *ptype;
    struct HirDecl *const *pdecl;
    K_LIST_ZIP (adt->fields, pdecl, field_types, ptype) {
        struct HirFieldDecl *field = HirGetFieldDecl(*pdecl);
        ensure_accessible_field(R, *pdecl, decl, type);
        int const *pindex = FieldMap_get(R, map, field->ident);
        if (pindex == NULL)
            RESOLVER_ERROR(R, missing_field, span.start,
                    field->ident.name->text, adt->ident.name->text);

        struct HirExpr *item = HirExprList_get(order, *pindex);
        unify(R, *ptype, resolve_operand(R, item));
        FieldMap_remove(R, map, field->ident);
        HirGetFieldExpr(item)->fid = index++;
    }
    FieldMapIterator iter;
    FieldMapIterator_init(map, &iter);
    if (FieldMapIterator_is_valid(&iter)) {
        struct HirIdent key = FieldMapIterator_key(&iter);
        RESOLVER_ERROR(R, unknown_field, span.start,
                   key.name->text, adt->ident.name->text);
    }
    paw_assert(adt->fields->count == e->items->count);
    FieldMap_delete(R, map);

    e->items = order;
    return type;
}

static struct IrType *resolve_literal_expr(struct Resolver *R, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitBasic:
            return resolve_basic_lit(R, &e->basic);
        case kHirLitTuple:
            return resolve_tuple_lit(R, &e->tuple);
        case kHirLitContainer:
            return resolve_container_lit(R, &e->cont, e->span);
        case kHirLitComposite:
            return resolve_composite_lit(R, &e->comp, e->span);
    }
}

static paw_Bool is_never_block(struct HirExpr *expr)
{
    if (HirIsBlock(expr))
        return HirGetBlock(expr)->never;
    return HirIsJumpExpr(expr);
}

static struct IrType *resolve_if_expr(struct Resolver *R, struct HirIfExpr *e)
{
    expect_bool_expr(R, e->cond);
    struct IrType *first = resolve_expr(R, e->then_arm);
    if (e->else_arm == NULL) {
        unify(R, first, get_type(R, BUILTIN_UNIT));
        return first;
    }

    struct IrType *second = resolve_expr(R, e->else_arm);
    if (!equals(R, first, second)) {
        // Forgive type errors when the result type is "()" and there is an
        // unconditional jump. Control will never reach the end of such a block.
        if (is_unit_type(R, first) && is_never_block(e->then_arm)) {
            first = second;
        } else if (is_unit_type(R, second) && is_never_block(e->else_arm)) {
            second = first;
        }
    }
    unify(R, first, second);
    return first;
}

static void ResolveExprStmt(struct Resolver *R, struct HirExprStmt *s)
{
    struct IrType *type = resolve_operand(R, s->expr);
    PAW_UNUSED(type);
}

static struct IrType *resolve_loop_expr(struct Resolver *R, struct HirLoopExpr *e)
{
    enter_scope(R, NULL);
    struct IrType *type = resolve_expr(R, e->block);
    unify_unit_type(R, type);
    leave_scope(R);
    return type;
}

static struct IrType *check_index(struct Resolver *R, struct HirIndex *e, struct IrType *target)
{
    struct IrType *result;
    struct IrType *expect = NULL;
    if (is_list_t(R, target)) {
        expect = get_type(R, BUILTIN_INT);
        result = e->is_slice ? target : ir_list_elem(target);
    } else if (is_map_t(R, target) && !e->is_slice) {
        expect = ir_map_key(target);
        result = ir_map_value(target);
    } else if (TYPE2CODE(R, target) == BUILTIN_STR) {
        expect = get_type(R, BUILTIN_INT);
        result = get_type(R, BUILTIN_STR);
    } else if (!e->is_slice) {
        char const *repr = pawIr_print_type(R->C, target);
        RESOLVER_ERROR(R, invalid_index_target, e->span.start, repr);
    } else {
        char const *repr = pawIr_print_type(R->C, target);
        RESOLVER_ERROR(R, invalid_slice_target, e->span.start, repr);
    }

    if (e->first != NULL)
        unify(R, expect, resolve_operand(R, e->first));
    if (e->second != NULL)
        unify(R, expect, resolve_operand(R, e->second));
    return result;
}

static struct IrType *resolve_index(struct Resolver *R, struct HirIndex *e)
{
    struct IrType *target = resolve_operand(R, e->target);
    return check_index(R, e, target);
}

static struct IrType *resolve_selector(struct Resolver *R, struct HirSelector *e)
{
    struct IrType *target = resolve_operand(R, e->target);
    return select_field(R, target, e);
}

static void ResolveDeclStmt(struct Resolver *R, struct HirDeclStmt *s)
{
    resolve_decl(R, s->decl);
}

struct BindingChecker {
    struct HirVisitor *V;
    struct Resolver *R;
    struct BindingMap *bound;
    int iter;
};

struct BindingInfo {
    struct IrType *type;
    int uses;
};

DEFINE_MAP(struct Resolver, BindingMap, pawP_alloc, ident_hash, ident_equals, struct HirIdent, struct BindingInfo)
DEFINE_MAP_ITERATOR(BindingMap, struct HirIdent, struct BindingInfo)

static void init_binding_checker(struct BindingChecker *bc, struct Resolver *R, struct HirVisitor *V)
{
    *bc = (struct BindingChecker){
        .bound = BindingMap_new(R),
        .R = R,
        .V = V,
    };
    pawHir_visitor_init(V, R->hir, bc);
}

static void uninit_binding_checker(struct BindingChecker *bc)
{
    BindingMap_delete(bc->R, bc->bound);
}

static void account_for_binding(struct Resolver *R, struct HirIdent ident)
{
    struct PatState *ps = R->ms->ps;
    while (ps->outer != NULL) {
        if (ps->outer->kind == kHirOrPat)
            break;
        ps = ps->outer;
    }
    String *const *pname = StringMap_get(R->C, ps->bound, ident.name);
    if (pname != NULL)
        RESOLVER_ERROR(R, duplicate_binding, ident.span.start, ident.name->text);
    StringMap_insert(R->C, ps->bound, ident.name, ident.name);
}

static void locate_binding(struct HirVisitor *V, struct HirBindingPat *p)
{
    struct BindingChecker *bc = V->ud;
    // all bindings must be specified in the first alternative
    struct IrType *type = pawIr_get_type(V->hir->C, p->hid);
    BindingMap_insert(bc->R, bc->bound, p->ident, (struct BindingInfo){
                                                     .type = type,
                                                 });
}

static void check_binding(struct HirVisitor *V, struct HirBindingPat *p)
{
    paw_Env *P = ENV(V->hir);
    struct BindingChecker *bc = V->ud;
    struct BindingInfo *pbi = BindingMap_get(bc->R, bc->bound, p->ident);
    if (pbi == NULL)
        RESOLVER_ERROR(bc->R, missing_binding_in_alternative, p->span.start,
                p->ident.name->text);

    struct IrType *type = pawIr_get_type(V->hir->C, p->hid);
    unify(bc->R, pbi->type, type);
    ++pbi->uses;
}

static void ensure_all_bindings_created(struct BindingChecker *bc)
{
    BindingMapIterator iter;
    BindingMapIterator_init(bc->bound, &iter);
    while (BindingMapIterator_is_valid(&iter)) {
        struct BindingInfo bi = *BindingMapIterator_valuep(&iter);
        // each bi->uses should have been incremented exactly once
        if (bi.uses < bc->iter) {
            struct HirIdent const key = BindingMapIterator_key(&iter);
            RESOLVER_ERROR(bc->R, missing_binding_in_alternative, key.span.start, key.name->text);
        } else if (bi.uses > bc->iter) {
            struct HirIdent const key = BindingMapIterator_key(&iter);
            RESOLVER_ERROR(bc->R, duplicate_binding, key.span.start, key.name->text);
        }
        BindingMapIterator_next(&iter);
    }
}

static struct IrType *ResolveOrPat(struct Resolver *R, struct HirOrPat *p)
{
    struct HirVisitor V;
    struct BindingChecker bc;
    init_binding_checker(&bc, R, &V);

    paw_assert(p->pats->count > 1);
    struct HirPat *first = K_LIST_FIRST(p->pats);
    struct IrType *type = resolve_pat(R, first);

    // populate map with bindings from first pattern, checking for
    // duplicates
    V.PostVisitBindingPat = locate_binding;
    pawHir_visit_pat(&V, first);

    // rest of the patterns must bind variables of the same name and
    // type as the first pattern (position can vary)
    V.PostVisitBindingPat = check_binding;
    for (bc.iter = 1; bc.iter < p->pats->count; ++bc.iter) {
        struct HirPat *next = HirPatList_get(p->pats, bc.iter);
        unify(R, type, resolve_pat(R, next));

        pawHir_visit_pat(&V, next);
        ensure_all_bindings_created(&bc);
    }
    uninit_binding_checker(&bc);
    return type;
}

static struct IrType *ResolveFieldPat(struct Resolver *R, struct HirFieldPat *p)
{
    return resolve_pat(R, p->pat);
}

static void unify_lists(struct Resolver *R, struct IrTypeList *lhs, struct IrTypeList *rhs)
{
    paw_assert(lhs->count == rhs->count);
    struct IrType *const *pa, *const *pb;
    K_LIST_ZIP (lhs, pa, rhs, pb) {
        unify(R, *pa, *pb);
    }
}

static struct IrType *ResolveStructPat(struct Resolver *R, struct HirStructPat *p)
{
    struct IrType *type = resolve_path(R, p->path, LOOKUP_TYPE);

    struct HirAdtDecl *adt = get_adt(R, type);
    struct IrTypeList *adt_fields = instantiate_fields(R->C, type, adt->fields);

    PatFieldMap *map = PatFieldMap_new(R);

    struct HirPat *const *pfield;
    K_LIST_FOREACH (p->fields, pfield) {
        resolve_pat(R, *pfield);
        struct HirIdent const field_ident = HirGetFieldPat(*pfield)->ident;
        struct HirPat **ppat = PatFieldMap_get(R, map, field_ident);
        if (ppat != NULL)
            RESOLVER_ERROR(R, duplicate_field, field_ident.span.start,
                    field_ident.name->text, adt->ident.name->text);
        PatFieldMap_insert(R, map, field_ident, *pfield);
    }

    struct HirPatList *sorted = HirPatList_new(R->hir);
    HirPatList_reserve(R->hir, sorted, adt_fields->count);

    int index = 0;
    struct IrType *const *ptype;
    struct HirDecl *const *pdecl;
    K_LIST_ZIP (adt->fields, pdecl, adt_fields, ptype) {
        struct HirFieldDecl *field = HirGetFieldDecl(*pdecl);
        struct HirPat **ppat = PatFieldMap_get(R, map, field->ident);
        if (ppat == NULL)
            RESOLVER_ERROR(R, missing_field, field->ident.span.start,
                    field->ident.name->text, adt->ident.name->text);
        struct HirFieldPat *field_pat = HirGetFieldPat(*ppat);
        unify(R, pawIr_get_type(R->C, field_pat->hid), *ptype);
        HirPatList_push(R->hir, sorted, *ppat);
        PatFieldMap_remove(R, map, field->ident);
        field_pat->index = index++;
    }
    p->fields = sorted;

    if (PatFieldMap_length(map) > 0) {
        PatFieldMapIterator iter;
        // use an iterator to find one of the unknown fields
        PatFieldMapIterator_init(map, &iter);
        struct HirIdent ident = PatFieldMapIterator_key(&iter);
        RESOLVER_ERROR(R, unknown_field, ident.span.start, ident.name->text,
                adt->ident.name->text);
    }
    PatFieldMap_delete(R, map);
    return type;
}

static struct IrType *ResolveVariantPat(struct Resolver *R, struct HirVariantPat *p)
{
    struct IrType *type = resolve_path(R, p->path, LOOKUP_VALUE);
    struct IrTypeList *args = resolve_pat_list(R, p->fields);
    struct IrTypeList *params = IR_FPTR(type)->params;

    struct MatchState *ms = R->ms;
    if (args->count != params->count)
        RESOLVER_ERROR(R, incorrect_arity, p->span.start, params->count, args->count);

    unify_lists(R, params, args);

    struct HirVariantDecl *d = HirGetVariantDecl(get_decl(R, IR_TYPE_DID(type)));
    p->index = d->index;
    return IR_FPTR(type)->result;
}

static struct IrType *ResolveTuplePat(struct Resolver *R, struct HirTuplePat *p)
{
    struct IrTypeList *elems = resolve_pat_list(R, p->elems);
    if (elems->count == 0)
        return get_type(R, BUILTIN_UNIT);
    return pawIr_new_tuple(R->C, elems);
}

static struct IrType *ResolveBindingPat(struct Resolver *R, struct HirBindingPat *p)
{
    // binding type is determined using unification
    struct IrType *type = new_unknown(R, p->span.start);

    // make sure bindings are unique within each pattern (OR patterns are special-cased)
    account_for_binding(R, p->ident);

    new_local(R, p->ident, (struct HirResult){
                .kind = HIR_RESULT_LOCAL,
                .hid = p->hid,
            });
    return type;
}

static struct IrType *convert_path_to_binding(struct Resolver *R, struct HirPathPat *path)
{
    struct HirSegment ident = K_LIST_FIRST(path->path.segments);
    struct HirPat *pat = HIR_CAST_PAT(path);
    pat->hdr.kind = kHirBindingPat;
    struct HirBindingPat *p = HirGetBindingPat(pat);
    p->ident = ident.ident;

    // resolve again as binding
    return resolve_pat(R, pat);
}

static struct IrType *ResolvePathPat(struct Resolver *R, struct HirPathPat *p)
{
    struct IrType *type = lookup_path(R, p->path, LOOKUP_EITHER);
    struct HirResult const res = HIR_PATH_RESULT(p->path);
    if (type == NULL || res.kind == HIR_RESULT_LOCAL)
into_binding:
        // identifier is unbound, or it refers to a variable declaration
        return convert_path_to_binding(R, p);

    // convert to a more specific type of pattern, now that it is known that
    // the path refers to a unit struct/enumerator
    struct HirPat *pat;
    struct HirPatList *empty = HirPatList_new(R->hir);
    struct HirDecl *decl = get_decl(R, res.did);
    if (HirIsAdtDecl(decl) && HirGetAdtDecl(decl)->is_struct) {
        pat = pawHir_new_struct_pat(R->m->hir, p->span, p->path, empty);
    } else if (HirIsVariantDecl(decl)) {
        int const index = HirGetVariantDecl(decl)->index;
        pat = pawHir_new_variant_pat(R->m->hir, p->span, p->path, empty, index);
        type = maybe_unit_variant(R, p->span, type);
    } else {
        goto into_binding;
    }

    // overwrite old pattern
    *HIR_CAST_PAT(p) = *pat;
    return type;
}

static struct IrType *ResolveWildcardPat(struct Resolver *R, struct HirWildcardPat *p)
{
    return new_unknown(R, p->span.start);
}

static struct IrType *ResolveLiteralPat(struct Resolver *R, struct HirLiteralPat *p)
{
    return resolve_operand(R, p->expr);
}

static void resolve_decl(struct Resolver *R, struct HirDecl *decl)
{
    struct IrType *type;
    switch (HIR_KINDOF(decl)) {
        case kHirFieldDecl:
            type = resolve_field_decl(R, HirGetFieldDecl(decl));
            break;
        default:
            type = resolve_type_decl(R, HirGetTypeDecl(decl));
    }
    type = normalize(R, type);
    SET_NODE_TYPE(R->C, decl, type);
}

static struct IrType *resolve_pat(struct Resolver *R, struct HirPat *pat)
{
    struct PatState ps;
    enter_pat(R, &ps, pat->hdr.kind);

    struct IrType *type;
    switch (HIR_KINDOF(pat)) {
#define DEFINE_CASE(X)                        \
    case kHir##X:                             \
        type = Resolve##X(R, HirGet##X(pat)); \
        break;
        HIR_PAT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }

    type = normalize(R, type);
    SET_NODE_TYPE(R->C, pat, type);

    leave_pat(R);
    return type;
}

static void resolve_stmt(struct Resolver *R, struct HirStmt *stmt)
{
    switch (HIR_KINDOF(stmt)) {
#define DEFINE_CASE(X)                  \
    case kHir##X:                       \
        Resolve##X(R, HirGet##X(stmt)); \
        break;
        HIR_STMT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

// NOTE: Some expressions are known to directly represent types, based on the context
//       (type annotations, type arguments, etc.). Call resolve_type() to convert such
//       an expression into a HIR type.

static struct IrType *resolve_type(struct Resolver *R, struct HirType *type, struct SourceSpan span)
{
    if (type == NULL)
        return new_unknown(R, span.start);
    struct IrType *r = pawP_lower_type(R->C, R->m, R->symtab, type);
    return normalize(R, r);
}

static struct IrType *resolve_return_expr(struct Resolver *R, struct HirReturnExpr *s)
{
    struct IrType *want = R->rs->prev;
    struct IrType *have = s->expr != NULL
                              ? resolve_operand(R, s->expr)
                              : get_type(R, BUILTIN_UNIT);
    unify(R, have, want);
    ++R->rs->count;

    return get_type(R, BUILTIN_UNIT);
}

static struct IrType *resolve_jump_expr(struct Resolver *R, struct HirJumpExpr *s)
{
    return get_type(R, BUILTIN_UNIT);
}

static struct IrType *resolve_expr(struct Resolver *R, struct HirExpr *expr)
{
    struct IrType *type;
    switch (HIR_KINDOF(expr)) {
        case kHirLiteralExpr:
            type = resolve_literal_expr(R, HirGetLiteralExpr(expr));
            break;
        case kHirLogicalExpr:
            type = resolve_logical_expr(R, HirGetLogicalExpr(expr));
            break;
        case kHirPathExpr:
            type = resolve_path_expr(R, HirGetPathExpr(expr));
            break;
        case kHirChainExpr:
            type = resolve_chain_expr(R, HirGetChainExpr(expr));
            break;
        case kHirUnOpExpr:
            type = resolve_unop_expr(R, HirGetUnOpExpr(expr));
            break;
        case kHirBinOpExpr:
            type = resolve_binop_expr(R, HirGetBinOpExpr(expr));
            break;
        case kHirClosureExpr:
            type = resolve_closure_expr(R, HirGetClosureExpr(expr));
            break;
        case kHirConversionExpr:
            type = resolve_conversion_expr(R, HirGetConversionExpr(expr));
            break;
        case kHirCallExpr:
            type = resolve_call_expr(R, HirGetCallExpr(expr));
            break;
        case kHirIndex:
            type = resolve_index(R, HirGetIndex(expr));
            break;
        case kHirSelector:
            type = resolve_selector(R, HirGetSelector(expr));
            break;
        case kHirAssignExpr:
            type = resolve_assign_expr(R, HirGetAssignExpr(expr));
            break;
        case kHirFieldExpr:
            type = resolve_field_expr(R, HirGetFieldExpr(expr));
            break;
        case kHirIfExpr:
            type = resolve_if_expr(R, HirGetIfExpr(expr));
            break;
        case kHirReturnExpr:
            type = resolve_return_expr(R, HirGetReturnExpr(expr));
            break;
        case kHirJumpExpr:
            type = resolve_jump_expr(R, HirGetJumpExpr(expr));
            break;
        case kHirLoopExpr:
            type = resolve_loop_expr(R, HirGetLoopExpr(expr));
            break;
        case kHirMatchExpr:
            type = resolve_match_expr(R, HirGetMatchExpr(expr));
            break;
        case kHirBlock:
            type = resolve_block(R, HirGetBlock(expr));
            break;
        case kHirMatchArm:
            type = resolve_match_arm(R, HirGetMatchArm(expr));
            break;
    }

    type = normalize(R, type);
    SET_NODE_TYPE(R->C, expr, type);
    return type;
}

static void resolve_item(struct Resolver *R, struct HirDecl *item)
{
    if (HirIsFuncDecl(item)) {
        resolve_func_item(R, HirGetFuncDecl(item));
    } else if (HirIsAdtDecl(item)) {
        resolve_adt_item(R, HirGetAdtDecl(item));
    } else if (HirIsConstDecl(item)) {
        resolve_const_item(R, HirGetConstDecl(item));
    }
}

static void resolve_items(struct Resolver *R, struct HirDeclList *items)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (items, pitem)
        resolve_item(R, *pitem);
}

static void use_module(struct Resolver *R, struct ModuleInfo *m)
{
    R->hir = m->hir;
    R->m = m;
}

static void check_module_types(struct Resolver *R, struct ModuleInfo *m)
{
    struct Hir *hir = m->hir;
    DLOG(R, "resolving '%s'", hir->name->text);
    use_module(R, m);

    enter_scope(R, NULL);
    resolve_items(R, hir->items);
    leave_scope(R);

    // control should not be within a scope block
    paw_assert(R->symtab->count == 0);
}

static void check_types(struct Resolver *R)
{
    struct ModuleInfo *const *pm;
    K_LIST_FOREACH (R->C->modules, pm) {
        use_module(R, *pm);
        enter_inference_ctx(R);
        check_module_types(R, R->m);
        leave_inference_ctx(R);
    }
}

void pawP_resolve(struct Compiler *C)
{
    struct Pool *pool = pawP_pool_new(C, C->aux_stats);

    // determine the type of each toplevel item in each module (allows the type checker
    // to resolve paths between modules immediately)
    pawP_collect_items(C, pool);

    struct Resolver R = {
        .symtab = HirSymtab_new(C->hir_prelude),
        .traits = C->traits,
        .pool = pool,
        .U = C->U,
        .P = ENV(C),
        .dm = C->dm,
        .C = C,
    };

    // run the type checker
    check_types(&R);

    pawP_pool_free(C, pool);
}

