// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// typeck.c: Implementation of the type checker.

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

#define STRING_LIT(T_, Str_) SCAN_STR((T_)->C, Str_)
#define TYPE2CODE(T_, Type_) pawP_type2code((T_)->C, Type_)
#define NODE_START(Node_) ((Node_)->hdr.span.start)
#define NODE_END(Node_) ((Node_)->hdr.span.end)
#define GET_TYPE(T_, Id_) pawIr_get_type((T_)->C, Id_)
#define SET_TYPE(T_, Id_, Type_) pawIr_set_type((T_)->C, Id_, Type_)

#define TYPECK_ERROR(T_, Kind_, ...) pawErr_##Kind_((T_)->C, (T_)->pm->name, __VA_ARGS__)

enum BlockKind {
    BLOCK_NORMAL,
    BLOCK_LOOP,
    BLOCK_MATCH,
};

struct BlockState {
    IrType *result;
    struct BlockState *outer;
    enum BlockKind kind;
};

struct ResultState {
    struct ResultState *outer;
    IrType *prev;
    int count;
};

struct PatState {
    struct PatState *outer;
    StringMap *bound;
    enum HirPatKind kind;
};

struct MatchState {
    struct MatchState *outer;
    IrType *target;
    struct PatState *ps;
};

// Common state for type-checking routines
struct TypeChecker {
    struct Pool *pool;
    struct Unifier *U; // unification tables
    struct Compiler *C; // compiler state
    IrType *self; // enclosing ADT
    struct ResultState *rs;
    struct MatchState *ms;
    struct BlockState *bs;
    struct Substitution subst;
    struct HirModule const *pm;
    struct Hir *hir;
    paw_Env *P;
};

static paw_Uint ident_hash(struct TypeChecker *T, struct HirIdent ident)
{
    return P_PTR_HASH(T, ident.name);
}

static paw_Bool ident_equals(struct TypeChecker *T, struct HirIdent a, struct HirIdent b)
{
    return pawS_eq(a.name, b.name);
}

DEFINE_MAP(struct TypeChecker, FieldMap, pawP_alloc, ident_hash, ident_equals, struct HirIdent, int)
DEFINE_MAP(struct TypeChecker, PatFieldMap, pawP_alloc, ident_hash, ident_equals, struct HirIdent, struct HirPat *)
DEFINE_MAP_ITERATOR(FieldMap, struct HirIdent, int)
DEFINE_MAP_ITERATOR(PatFieldMap, struct HirIdent, struct HirPat *)

static void check_stmt(struct TypeChecker *, struct HirStmt *);
static void check_decl(struct TypeChecker *, struct HirDecl *);
static IrType *check_expr(struct TypeChecker *, struct HirExpr *);
static IrType *check_type(struct TypeChecker *, struct HirType *, struct SourceSpan span);
static IrType *check_pat(struct TypeChecker *, struct HirPat *);

#define DEFINE_LIST_CHECKER(name, T)                                                  \
    static void check_##name##_list(struct TypeChecker *T, struct Hir##T##List *list) \
    {                                                                                 \
        if (list == NULL) return;                                                     \
        for (int i = 0; i < list->count; ++i) {                                       \
            check_##name(T, list->data[i]);                                           \
        }                                                                             \
    }
DEFINE_LIST_CHECKER(expr, Expr)
DEFINE_LIST_CHECKER(decl, Decl)
DEFINE_LIST_CHECKER(stmt, Stmt)
#undef DEFINE_LIST_CHECKER

static IrType *lower_type(struct TypeChecker *T, struct HirType *type)
{
    return pawP_lower_type(T->C, *T->pm, type);
}

static IrTypeList *lower_types(struct TypeChecker *T, struct HirTypeList *types)
{
    if (types == NULL) return NULL;
    IrTypeList *result = IrTypeList_new(T->C);

    struct HirType *const *ptype;
    K_LIST_FOREACH (types, ptype) {
        IrType *type = lower_type(T, *ptype);
        IrTypeList_push(T->C, result, type);
    }
    return result;
}

static IrTypeList *check_pat_list(struct TypeChecker *T, struct HirPatList *pats)
{
    IrTypeList *types = IrTypeList_new(T->C);
    if (pats != NULL) {
        IrTypeList_reserve(T->C, types, pats->count);

        struct HirPat *const *ppat;
        K_LIST_FOREACH (pats, ppat) {
            IrType *type = check_pat(T, *ppat);
            IrTypeList_push(T->C, types, type);
        }
    }
    return types;
}

static IrTypeList *check_type_list(struct TypeChecker *T, struct HirTypeList *list)
{
    if (list == NULL) return NULL;
    IrTypeList *result = IrTypeList_new(T->C);
    IrTypeList_reserve(T->C, result, list->count);

    struct HirType *const *ptype;
    K_LIST_FOREACH (list, ptype) {
        struct HirType *hir_type = *ptype;
        IrType *ir_type = check_type(T, hir_type, hir_type->hdr.span);
        IrTypeList_push(T->C, result, ir_type);
    }
    return result;
}

static IrType *normalize(struct TypeChecker *T, IrType *type)
{
    return pawU_normalize(T->U->table, type);
}

static paw_Bool equals(struct TypeChecker *T, IrType *a, IrType *b)
{
    return pawU_equals(T->U, a, b);
}

static IrType *unify(struct TypeChecker *T, struct SourceLoc loc, IrType *a, IrType *b)
{
    if (pawU_unify(T->U, a, b) != 0) {
        char const *lhs = pawIr_print_type(T->C, a);
        char const *rhs = pawIr_print_type(T->C, b);
        TYPECK_ERROR(T, incompatible_types, loc, lhs, rhs);
    }
    return IrIsNever(a) ? b : a;
}

static struct HirDecl *get_decl(struct TypeChecker *T, DeclId did)
{
    return pawHir_get_decl(T->hir, did);
}

static IrType *builtin_type(struct TypeChecker *T, enum BuiltinKind code)
{
    return pawP_builtin_type(T->C, code);
}

static IrType *unify_unit_type(struct TypeChecker *T, struct SourceLoc loc, IrType *type)
{
    return unify(T, loc, type, builtin_type(T, BUILTIN_UNIT));
}

static IrType *unify_never_type(struct TypeChecker *T, struct SourceLoc loc, IrType *type)
{
    return unify(T, loc, type, pawIr_new_never(T->C));
}

static paw_Bool is_unit_variant(struct TypeChecker *T, IrType *type)
{
    if (IrIsSignature(type)) {
        struct HirDecl *decl = get_decl(T, IrGetSignature(type)->did);
        return HirIsVariantDecl(decl) && HirGetVariantDecl(decl)->fields->count == 0;
    }
    return PAW_FALSE;
}

static struct HirAdtDecl *get_adt(struct TypeChecker *T, IrType *type)
{
    struct HirDecl *decl = get_decl(T, IR_TYPE_DID(type));
    return HirGetAdtDecl(decl);
}

static IrTypeList *get_trait_bounds(struct TypeChecker *T, IrType *type)
{
    if (IrIsGeneric(type)) {
        return IrGetGeneric(type)->bounds;
    } else if (IrIsInfer(type)) {
        return IrGetInfer(type)->bounds;
    } else if (IrIsAdt(type)) {
        struct HirAdtDecl *d = HirGetAdtDecl(
            pawHir_get_decl(T->hir, IR_TYPE_DID(type)));
        IrTypeList *bounds = IrTypeList_new(T->C);
        IrTypeList_reserve(T->C, bounds, d->traits->count);

        struct HirType *const *ptype;
        K_LIST_FOREACH (d->traits, ptype) {
            IrTypeList_push(T->C, bounds, GET_NODE_TYPE(T->C, *ptype));
        }
        return bounds;
    } else {
        return NULL;
    }
}

static paw_Bool implements_trait(struct TypeChecker *T, IrType *type, enum TraitKind kind)
{
    IrTypeList *bounds = get_trait_bounds(T, type);
    if (bounds == NULL) return PAW_FALSE;

    IrType *const *pbound;
    K_LIST_FOREACH (bounds, pbound) {
        struct HirDecl *decl = get_decl(T, IR_TYPE_DID(*pbound));
        struct HirTraitDecl *trait = HirGetTraitDecl(decl);
        if ((kind == TRAIT_HASH && pawS_eq(trait->ident.name, CSTR(T, CSTR_HASH)))
                || (kind == TRAIT_EQUALS && pawS_eq(trait->ident.name, CSTR(T, CSTR_EQUALS)))) {
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static void require_trait(struct TypeChecker *T, IrType *type, enum TraitKind kind)
{
    struct IrInfer *t = IrGetInfer(type);
    if (t->bounds == NULL) t->bounds = IrTypeList_new(T->C);
    enum BuiltinKind k = kind == TRAIT_HASH ? BUILTIN_HASH : BUILTIN_EQUALS;
    struct HirDecl *decl = get_decl(T, T->C->builtins[k].did);
    IrType *trait = GET_NODE_TYPE(T->C, decl);
    IrTypeList_push(T->C, t->bounds, trait);
}

static IrType *maybe_unit_variant(struct TypeChecker *T, struct SourceSpan span, IrType *type)
{
    if (IrIsSignature(type)) {
        // handle unit enumerators
        struct HirDecl *decl = get_decl(T, IrGetSignature(type)->did);
        if (HirIsVariantDecl(decl)) {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            if (d->fields->count > 0) {
                struct IrVariantDef *def = pawIr_get_variant_def(T->C, d->did);
                TYPECK_ERROR(T, missing_variant_args, span.start, def->name->text);
            }
            return IR_FPTR(type)->result;
        }
    }
    return type;
}

static IrType *check_operand(struct TypeChecker *T, struct HirExpr *expr)
{
    IrType *type = check_expr(T, expr);
    type = maybe_unit_variant(T, expr->hdr.span, type);
    SET_NODE_TYPE(T->C, expr, type);
    return type;
}

static void expect_bool_expr(struct TypeChecker *T, struct HirExpr *e)
{
    IrType *type = check_operand(T, e);
    unify(T, NODE_START(e), type, builtin_type(T, BUILTIN_BOOL));
}

static void expect_int_expr(struct TypeChecker *T, struct HirExpr *e)
{
    IrType *type = check_operand(T, e);
    unify(T, NODE_START(e), type, builtin_type(T, BUILTIN_INT));
}

static IrTypeList *check_exprs(struct TypeChecker *T, struct HirExprList *list)
{
    if (list == NULL) return NULL;
    IrTypeList *new_list = IrTypeList_new(T->C);
    IrTypeList_reserve(T->C, new_list, list->count);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (list, pexpr) {
        IrType *type = check_operand(T, *pexpr);
        IrTypeList_push(T->C, new_list, type);
    }
    return new_list;
}

static IrTypeList *collect_decl_types(struct TypeChecker *T, struct HirDeclList *list)
{
    if (list == NULL) return NULL;
    IrTypeList *new_list = IrTypeList_new(T->C);
    IrTypeList_reserve(T->C, new_list, list->count);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (list, pdecl) {
        IrType *type = GET_NODE_TYPE(T->C, *pdecl);
        IrTypeList_push(T->C, new_list, type);
    }
    return new_list;
}

static IrType *new_unknown(struct TypeChecker *T, struct SourceLoc loc)
{
    return pawU_new_unknown(T->U, loc, NULL);
}

static void enter_inference_ctx(struct TypeChecker *T)
{
    pawU_enter_binder(T->U, T->pm->name);
}

static void leave_inference_ctx(struct TypeChecker *T)
{
    pawU_leave_binder(T->U);
}

static void enter_block(struct TypeChecker *T, struct BlockState *bs, struct SourceSpan span, enum BlockKind kind)
{
    *bs = (struct BlockState){
        .result = new_unknown(T, span.start),
        .outer = T->bs,
        .kind = kind,
    };
    T->bs = bs;
}

static void leave_block(struct TypeChecker *T)
{
    struct BlockState *inner = T->bs;
    T->bs = inner->outer;
}

static void enter_match_ctx(struct TypeChecker *T, struct MatchState *ms, IrType *target)
{
    *ms = (struct MatchState){
        .target = target,
        .outer = T->ms,
    };
    T->ms = ms;
}

static void leave_match_ctx(struct TypeChecker *T)
{
    T->ms = T->ms->outer;
}

static int find_field(struct HirDeclList *fields, Str *name)
{
    if (fields == NULL)
        return -1;

    int index;
    struct HirDecl *const *pfield;
    K_LIST_ENUMERATE (fields, index, pfield) {
        if (pawS_eq(name, HirGetFieldDecl(*pfield)->ident.name))
            return index;
    }
    return -1;
}

static void leave_pat(struct TypeChecker *T)
{
    struct PatState *ps = T->ms->ps;
    StringMap_delete(T->C, ps->bound);
    T->ms->ps = ps->outer;
}

static void enter_pat(struct TypeChecker *T, struct PatState *ps, enum HirPatKind kind)
{
    *ps = (struct PatState){
        .bound = StringMap_new_from(T->C, T->pool),
        .outer = T->ms->ps,
        .kind = kind,
    };
    T->ms->ps = ps;
}

static IrType *check_block(struct TypeChecker *T, struct HirBlock *block)
{
    struct BlockState bs;
    enter_block(T, &bs, block->span, BLOCK_NORMAL);

    check_stmt_list(T, block->stmts);
    if (block->result != NULL) {
        IrType *result = check_operand(T, block->result);
        bs.result = unify(T, block->span.start, result, bs.result);
    } else {
        unify_unit_type(T, block->span.start, bs.result);
    }

    leave_block(T);
    return bs.result;
}

static IrType *CheckFieldDecl(struct TypeChecker *T, struct HirFieldDecl *d)
{
    return check_type(T, d->tag, d->span);
}

static paw_Bool is_unit_type(struct TypeChecker *T, IrType *type)
{
    return pawP_type2code(T->C, type) == BUILTIN_UNIT;
}

static void check_fn_item(struct TypeChecker *T, struct HirFnDecl *d)
{
    if (d->body == NULL) return;
    IrType *ret = GET_NODE_TYPE(T->C, d->result);

    struct ResultState rs = {
        // named function has explicit return type
        .outer = T->rs,
        .prev = ret,
    };
    T->rs = &rs;

    struct BlockState bs;
    enter_block(T, &bs, d->span, BLOCK_NORMAL);

    IrType *result = check_operand(T, d->body);
    bs.result = unify(T, d->span.start, result, bs.result);
    unify(T, d->span.start, bs.result, ret);

    leave_block(T);
    T->rs = rs.outer;
}

static void check_item(struct TypeChecker *T, struct HirDecl *item);

static void check_methods(struct TypeChecker *T, struct HirDeclList *methods, struct IrAdt *self)
{
    struct HirDecl *const *pmethod;
    K_LIST_FOREACH (methods, pmethod)
        check_item(T, *pmethod);
}

static void check_adt_methods(struct TypeChecker *T, struct HirAdtDecl *d)
{
    check_methods(T, d->methods, IrGetAdt(T->self));
}

IrType *pawP_instantiate_field(struct Compiler *C, IrType *inst_type, IrType *field)
{
    struct IrAdt *t = IrGetAdt(inst_type);
    if (t->types == NULL) return field;

    struct IrTypeFolder F;
    struct Substitution subst;
    IrType *base_type = pawIr_get_def_type(C, IR_TYPE_DID(inst_type));
    pawP_init_substitution_folder(&F, C, &subst, ir_adt_types(base_type), t->types);
    return pawIr_fold_type(&F, field);
}

static IrTypeList *instantiate_fields(struct Compiler *C, IrType *self, struct HirDeclList *fields)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    struct HirDecl *decl = pawHir_get_decl(C->hir, IR_TYPE_DID(self));
    if (HirIsVariantDecl(decl)) {
        struct HirVariantDecl *v = HirGetVariantDecl(decl);
        decl = pawHir_get_decl(C->hir, v->base_did);
    }
    IrTypeList *generics = ir_adt_types(GET_NODE_TYPE(C, decl));
    pawP_init_substitution_folder(&F, C, &subst, generics, IrGetAdt(self)->types);
    IrTypeList *field_types = pawHir_collect_decl_types(C, fields);
    return pawIr_fold_type_list(&F, field_types);
}

static IrType *instantiate(struct TypeChecker *T, struct SourceLoc loc, IrType *base, IrTypeList *types)
{
    if (types == NULL) return pawP_generalize(T->C, loc, base);
    return pawP_instantiate(T->C, base, types);
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static IrType *lower_adt_segment(struct TypeChecker *T, struct HirSegment segment)
{
    struct HirDecl *decl = pawHir_get_node(T->hir, segment.target);
    if (HirIsTypeDecl(decl)) {
        IrType *lower_type_alias(struct Compiler *, struct HirSegment, struct HirDecl *, IrTypeList *);
        IrTypeList *args = segment.types != NULL ? lower_types(T, segment.types) : NULL;
        return lower_type_alias(T->C, segment, decl, args);
    }

    IrType *type = GET_TYPE(T, segment.target);

    IrTypeList *args = NULL;
    if (segment.types != NULL)
        args = lower_types(T, segment.types);

    // Instantiate with type arguments, if provided. Otherwise, instantiate with
    // a list of IrInfer types which must be resolved later via unification.
    return instantiate(T, segment.span.start, type, args);
}

static IrType *lower_type_path(struct TypeChecker *T, struct HirPath path)
{
    paw_assert(path.kind == HIR_PATH_ITEM);
    paw_assert(path.segments->count == 1);

    return lower_adt_segment(T, K_LIST_LAST(path.segments));
}

static void unify_segment_types(struct TypeChecker *T, struct HirSegment segment, IrTypeList *params, IrTypeList *args)
{
    int index;
    struct HirType *const *ptype;
    K_LIST_ENUMERATE (segment.types, index, ptype) {
        IrType *a = IrTypeList_get(params, index);
        IrType *b = IrTypeList_get(args, index);
        unify(T, NODE_START(*ptype), a, b);
    }
}

static IrType *lower_value_path(struct TypeChecker *T, struct HirPath path)
{
    switch (path.kind) {
        case HIR_PATH_LOCAL: {
            struct HirSegment const segment = K_LIST_FIRST(path.segments);
            return GET_TYPE(T, segment.target);
        }
        case HIR_PATH_ITEM: {
            struct HirSegment const segment = K_LIST_FIRST(path.segments);
            struct HirDecl *item = pawHir_get_node(T->hir, segment.target);

            if (HirIsAdtDecl(item)) {
                return lower_adt_segment(T, segment);
            } else if (HirIsParamDecl(item)) {
                return GET_TYPE(T, segment.target);
            } else if (HirIsVariantDecl(item)) {
                struct HirVariantDecl *v = HirGetVariantDecl(item);
                IrType *base = pawIr_get_def_type(T->C, v->base_did);
                IrType *assoc = GET_TYPE(T, segment.target);
                if (IS_BASIC_TYPE(TYPE2CODE(T, base))) {
                    char const *repr = pawIr_print_type(T->C, base);
                    TYPECK_ERROR(T, unexpected_type, segment.span.start, repr);
                }
                if (IrIsSignature(assoc)) {
                    base = pawP_generalize(T->C, segment.span.start, base);
                    return pawP_generalize_assoc(T->C, segment.span.start, base, assoc);
                } else {
                    IrType *target = GET_TYPE(T, segment.target);
                    IrTypeList *args = lower_types(T, segment.types);
                    target = instantiate(T, segment.span.start, target, args);
                    return target;
                }
            } else {
                IrType *target = GET_TYPE(T, segment.target);
                IrTypeList *args = lower_types(T, segment.types);
                target = instantiate(T, segment.span.start, target, args);
                return target;
            }
        }
        case HIR_PATH_ASSOC: {
            paw_assert(path.segments->count == 2);
            struct HirSegment const first = K_LIST_FIRST(path.segments);
            struct HirSegment const last = K_LIST_LAST(path.segments);
            IrType *base = lower_adt_segment(T, first);
            IrType *assoc = pawIr_get_type(T->C, last.target);
            pawIr_set_type(T->C, first.id, base);
            paw_assert(IrIsSignature(assoc));

            if (IrIsGeneric(base)) {
                assoc = pawIr_resolve_trait_method(T->C, IrGetGeneric(base), last.ident.name);
                return pawP_generalize(T->C, first.span.start, assoc);
            }
            assoc = pawP_generalize_assoc(T->C, first.span.start, base, assoc);
            if (last.types != NULL) {
                IrTypeList *params = IR_TYPE_SUBTYPES(assoc);
                IrTypeList *args = lower_types(T, last.types);
                unify_segment_types(T, last, params, args);
            }
            return assoc;
        }
    }
}

static IrType *check_path_expr(struct TypeChecker *T, struct HirPathExpr *e)
{
    // path might refer to a unit ADT, so we have to use LOOKUP_EITHER
    IrType *type = lower_value_path(T, e->path);
    if (e->path.kind == HIR_PATH_LOCAL) return type;

    // TODO: not good...
    struct HirDecl *decl = pawHir_get_node(T->hir, K_LIST_LAST(e->path.segments).target);
    if (HirIsParamDecl(decl)) return type;

    if (IrIsGeneric(type)) {
        struct HirDecl *result = get_decl(T, IR_TYPE_DID(type));
        Str const *name = HirGetGenericDecl(result)->ident.name;
        TYPECK_ERROR(T, expected_value, NODE_START(result), name->text);
    }
    return type;
}

static IrType *check_ascription_expr(struct TypeChecker *T, struct HirAscriptionExpr *e)
{
    IrType *type = check_expr(T, e->expr);
    IrType *tag = check_type(T, e->type, e->type->hdr.span);
    return unify(T, e->span.start, type, tag);
}

static IrType *check_logical_expr(struct TypeChecker *T, struct HirLogicalExpr *e)
{
    expect_bool_expr(T, e->lhs);
    expect_bool_expr(T, e->rhs);
    return builtin_type(T, BUILTIN_BOOL);
}

static paw_Bool is_option_t(struct TypeChecker *T, IrType *type)
{
    return IrIsAdt(type) && IR_TYPE_DID(type).value == T->C->builtins[BUILTIN_OPTION].did.value;
}

static paw_Bool is_result_t(struct TypeChecker *T, IrType *type)
{
    return IrIsAdt(type) && IR_TYPE_DID(type).value == T->C->builtins[BUILTIN_RESULT].did.value;
}

static IrType *check_chain_expr(struct TypeChecker *T, struct HirChainExpr *e)
{
    IrType *type = check_operand(T, e->target);
    IrType *ret = T->rs->prev;
    paw_assert(ret != NULL);

    if (is_option_t(T, type)) {
        IrType *option = pawP_generalize(T->C, e->span.start, type);
        unify(T, e->span.start, option, ret);
    } else if (is_result_t(T, type)) {
        IrType *result = pawP_generalize(T->C, e->span.start, type);
        IrType *error = K_LIST_LAST(ir_adt_types(type));
        IrType *infer = K_LIST_LAST(ir_adt_types(result));
        unify(T, e->span.start, error, infer);
        unify(T, e->span.start, result, ret);
    } else {
        char const *repr = pawIr_print_type(T->C, type);
        TYPECK_ERROR(T, invalid_chain_operand, e->span.start, repr);
    }

    return K_LIST_FIRST(ir_adt_types(type)); // unwrap
}

static IrType *get_value_type(struct TypeChecker *T, IrType *target)
{
    enum BuiltinKind kind = TYPE2CODE(T, target);
    if (kind == BUILTIN_LIST)
        return ir_list_elem(target);
    if (kind == BUILTIN_MAP)
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

static IrType *check_unop_expr(struct TypeChecker *T, struct HirUnOpExpr *e)
{
    static uint8_t const VALID_OPS[][NBUILTINS] = {
        //     type  = {0, b, x, i, f, s, l, m}
        [UNARY_LEN]  = {0, 0, 0, 0, 0, 1, 1, 1},
        [UNARY_NEG]  = {0, 0, 0, 1, 1, 0, 0, 0},
        [UNARY_NOT]  = {0, 1, 0, 0, 0, 0, 0, 0},
        [UNARY_BNOT] = {0, 0, 0, 1, 0, 0, 0, 0},
    };

    static char const *UNOP_REPR[] = {
        [UNARY_LEN] = "#",
        [UNARY_NEG] = "-",
        [UNARY_NOT] = "!",
        [UNARY_BNOT] = "~",
    };

    IrType *type = check_operand(T, e->target);
    if (IrIsNever(type)) return type;

    enum BuiltinKind const code = TYPE2CODE(T, type);
    if (!IS_BUILTIN_TYPE(code) || !VALID_OPS[e->op][code]) {
        char const *operand = pawIr_print_type(T->C, type);
        TYPECK_ERROR(T, invalid_unary_operand, e->span.start, operand, UNOP_REPR[e->op]);
    } else if (is_bool_unop(e->op)) {
        return builtin_type(T, BUILTIN_BOOL);
    } else if (e->op == UNARY_LEN) {
        return builtin_type(T, BUILTIN_INT);
    } else {
        return type;
    }
}

static IrType *check_binary_op(struct TypeChecker *T, struct SourceSpan span, enum BinaryOp op, struct HirExpr *left, struct HirExpr *right)
{
    static uint8_t const VALID_OPS[][NBUILTINS] = {
        //     type    = {0, b, x, i, f, s, l, m}
        [BINARY_EQ]    = {0, 1, 1, 1, 1, 1, 0, 0},
        [BINARY_NE]    = {0, 1, 1, 1, 1, 1, 0, 0},
        [BINARY_LT]    = {0, 0, 1, 1, 1, 1, 0, 0},
        [BINARY_LE]    = {0, 0, 1, 1, 1, 1, 0, 0},
        [BINARY_GT]    = {0, 0, 1, 1, 1, 1, 0, 0},
        [BINARY_GE]    = {0, 0, 1, 1, 1, 1, 0, 0},
        [BINARY_ADD]   = {0, 0, 0, 1, 1, 1, 1, 0},
        [BINARY_SUB]   = {0, 0, 0, 1, 1, 0, 0, 0},
        [BINARY_MUL]   = {0, 0, 0, 1, 1, 0, 0, 0},
        [BINARY_DIV]   = {0, 0, 0, 1, 1, 0, 0, 0},
        [BINARY_MOD]   = {0, 0, 0, 1, 1, 0, 0, 0},
        [BINARY_BXOR]  = {0, 0, 0, 1, 0, 0, 0, 0},
        [BINARY_BAND]  = {0, 0, 0, 1, 0, 0, 0, 0},
        [BINARY_BOR]   = {0, 0, 0, 1, 0, 0, 0, 0},
        [BINARY_SHL]   = {0, 0, 0, 1, 0, 0, 0, 0},
        [BINARY_SHR]   = {0, 0, 0, 1, 0, 0, 0, 0},
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
    };

    IrType *lhs = check_operand(T, left);
    IrType *rhs = check_operand(T, right);
    IrType *type = unify(T, span.start, lhs, rhs);
    enum BuiltinKind const code = TYPE2CODE(T, type);

    if (!IS_BUILTIN_TYPE(code) || !VALID_OPS[op][code]) {
        char const *operand = pawIr_print_type(T->C, type);
        TYPECK_ERROR(T, invalid_binary_operand, span.start, operand, BINOP_REPR[op]);
    } else if (is_bool_binop(op)) {
        return builtin_type(T, BUILTIN_BOOL);
    } else {
        return type;
    }
}

static IrType *check_binop_expr(struct TypeChecker *T, struct HirBinOpExpr *e)
{
    return check_binary_op(T, e->span, e->op, e->lhs, e->rhs);
}

static IrType *check_assign_expr(struct TypeChecker *T, struct HirAssignExpr *e)
{
    paw_assert(HirIsPathExpr(e->lhs) || HirIsIndex(e->lhs) || HirIsSelector(e->lhs));
    IrType *lhs = check_operand(T, e->lhs);
    IrType *rhs = check_operand(T, e->rhs);
    unify(T, e->span.start, lhs, rhs);
    return builtin_type(T, BUILTIN_UNIT);
}

static IrType *check_op_assign_expr(struct TypeChecker *T, struct HirOpAssignExpr *e)
{
    paw_assert(HirIsPathExpr(e->lhs) || HirIsIndex(e->lhs) || HirIsSelector(e->lhs));
    check_binary_op(T, e->span, e->op, e->lhs, e->rhs);
    return builtin_type(T, BUILTIN_UNIT);
}

// Intended typing behavior for match expressions:
// (1) If all match arms diverge, then the match itself is considered to diverge.
// (2) A match expression takes the type of the first non-diverging arm. Any other arms that
//     complete normally must have the same type.
static IrType *check_match_expr(struct TypeChecker *T, struct HirMatchExpr *e)
{
    struct BlockState bs;
    enter_block(T, &bs, e->span, BLOCK_MATCH);

    IrType *target = check_operand(T, e->target);

    struct MatchState ms;
    enter_match_ctx(T, &ms, target);
    check_expr_list(T, e->arms);
    leave_match_ctx(T);
    leave_block(T);

    if (IrIsNever(bs.result))
        // propagate divergence status to enclosing block
        unify(T, e->span.start, bs.result, T->bs->result);

    SET_TYPE(T, e->target->hdr.id, ms.target);
    return bs.result;
}

static IrType *check_match_arm(struct TypeChecker *T, struct HirMatchArm *e)
{
    struct BlockState bs;
    enter_block(T, &bs, e->span, BLOCK_NORMAL);

    struct MatchState *ms = T->ms;
    IrType *pat = check_pat(T, e->pat);
    ms->target = unify(T, e->span.start, pat, ms->target);
    if (e->guard != NULL) expect_bool_expr(T, e->guard);
    IrType *result = check_operand(T, e->result);
    result = unify(T, e->span.start, bs.result, result);

    leave_block(T);

    T->bs->result = unify(T, e->span.start, result, T->bs->result);
    return T->bs->result;
}

static IrType *new_list_t(struct TypeChecker *T, struct SourceLoc loc, IrType *elem_t)
{
    IrType *base = pawP_builtin_type(T->C, BUILTIN_LIST);
    IrTypeList *types = IrTypeList_new(T->C);
    IrTypeList_push(T->C, types, elem_t);
    return instantiate(T, loc, base, types);
}

static IrType *new_map_t(struct TypeChecker *T, struct SourceLoc loc, IrType *key_t, IrType *value_t)
{
    IrType *base = pawP_builtin_type(T->C, BUILTIN_MAP);
    IrTypeList *types = IrTypeList_new(T->C);
    IrTypeList_push(T->C, types, key_t);
    IrTypeList_push(T->C, types, value_t);
    return instantiate(T, loc, base, types);
}

static void check_closure_param(struct TypeChecker *T, struct HirParamDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    IrType *type = check_type(T, d->tag, d->span);
    SET_NODE_TYPE(T->C, decl, type);
}

static IrType *check_closure_expr(struct TypeChecker *T, struct HirClosureExpr *e)
{
    struct ResultState rs = {T->rs};
    T->rs = &rs;

    // steal the enclosing block state to prevent propagation out of the closure
    struct BlockState *outer = T->bs;
    T->bs = NULL;

    struct HirDecl *const *pparam;
    K_LIST_FOREACH (e->params, pparam)
        check_closure_param(T, HirGetParamDecl(*pparam));

    struct BlockState bs;
    enter_block(T, &bs, e->span, BLOCK_NORMAL);

    IrTypeList *params = collect_decl_types(T, e->params);
    IrType *ret = check_type(T, e->result, e->span);
    rs.prev = ret;

    IrType *result = check_operand(T, e->expr);
    bs.result = unify(T, NODE_START(e->expr), bs.result, result);
    if (!IrIsNever(bs.result))
        unify(T, NODE_START(e->expr), bs.result, ret);

    leave_block(T);
    T->bs = outer;
    T->rs = rs.outer;

    return pawIr_new_fn_ptr(T->C, params, ret);
}

static struct HirDecl *find_method_aux(struct Compiler *C, struct HirDecl *base, Str *name)
{
    struct HirDecl *const *pdecl;
    struct HirAdtDecl *adt = HirGetAdtDecl(base);
    K_LIST_FOREACH (adt->methods, pdecl) {
        struct HirFnDecl *method = HirGetFnDecl(*pdecl);
        if (pawS_eq(name, method->ident.name))
            return *pdecl;
    }
    return NULL;
}

IrType *pawP_find_method(struct Compiler *C, IrType *base, Str *name)
{
    struct HirDecl *decl = pawHir_get_decl(C->hir, IR_TYPE_DID(base));
    struct HirDecl const *method = find_method_aux(C, decl, name);
    if (method == NULL) return NULL;
    return GET_NODE_TYPE(C, method);
}

static void check_adt_item(struct TypeChecker *T, struct HirAdtDecl *d)
{
    T->self = pawIr_get_type(T->C, d->id);
    check_adt_methods(T, d);
    T->self = NULL;
}

static void CheckLetStmt(struct TypeChecker *T, struct HirLetStmt *s)
{
    IrType *tag = check_type(T, s->tag, s->span);
    IrType *rhs = s->init != NULL
        ? check_operand(T, s->init)
        : new_unknown(T, s->span.start);
    struct BlockState bs;
    struct MatchState ms;
    enter_block(T, &bs, s->span, BLOCK_MATCH);
    enter_match_ctx(T, &ms, tag);
    IrType *lhs = check_pat(T, s->pat);
    unify(T, s->span.start, bs.result, tag);
    leave_match_ctx(T);
    leave_block(T);

    unify(T, s->span.start, lhs, tag);
    unify(T, s->span.start, tag, rhs);

    pawIr_set_type(T->C, s->id, rhs);
}

static void const_check_path(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct TypeChecker *T = V->ud;

    IrType *type = lower_value_path(T, e->path);
    enum BuiltinKind kind = TYPE2CODE(T, type);
    if (!IS_BASIC_TYPE(kind)) {
        char const *repr = pawIr_print_type(T->C, type);
        TYPECK_ERROR(T, nonprimitive_constant, e->span.start, repr);
    }
}

static void const_check_expr(struct HirVisitor *V, struct HirExpr *expr)
{
    char const *name;
    switch (HIR_KINDOF(expr)) {
        case kHirClosureExpr:
            name = "closure";
            break;
        case kHirCallExpr:
            name = "function call";
            break;
        case kHirIndex:
            name = "index expression";
            break;
        case kHirSelector:
            name = "selector expression";
            break;
        case kHirFieldExpr:
            name = "field expression";
            break;
        case kHirLoopExpr:
            name = "loop";
            break;
        default:
            return;
    }

    struct TypeChecker *T = V->ud;
    TYPECK_ERROR(T, cannot_constant_evaluate, expr->hdr.span.start, name);
}

// Make sure the initializer of a global constant can be computed at compile time
static void check_const(struct TypeChecker *T, struct HirExpr *expr, IrType *type)
{
    struct HirVisitor V;
    pawHir_visitor_init(&V, T->hir, T);
    V.PostVisitExpr = const_check_expr;
    pawHir_visit_expr(&V, expr);

    enum BuiltinKind kind = TYPE2CODE(T, type);
    if (!IS_BASIC_TYPE(kind)) {
        char const *repr = pawIr_print_type(T->C, type);
        TYPECK_ERROR(T, nonprimitive_constant, NODE_START(expr), repr);
    }
}

static void check_const_item(struct TypeChecker *T, struct HirConstDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    IrType *tag = GET_NODE_TYPE(T->C, d->tag);
    if (d->init != NULL) {
        IrType *init = check_operand(T, d->init);
        unify(T, d->span.start, init, tag);
        check_const(T, d->init, tag);
    }
    pawIr_set_type(T->C, d->id, tag);
}

static IrType *check_field_decl(struct TypeChecker *T, struct HirFieldDecl *d)
{
    return check_type(T, d->tag, d->span);
}

static IrType *check_type_decl(struct TypeChecker *T, struct HirTypeDecl *d)
{
    if (d->generics != NULL) {
        struct HirDecl *const *pdecl;
        K_LIST_FOREACH (d->generics, pdecl) {
            struct HirGenericDecl *d = HirGetGenericDecl(*pdecl);
            if (d->bounds != NULL)
                TYPECK_ERROR(T, trait_bounds_on_alias_generic,
                        d->span.start, d->ident.name->text);
            IrType *type = pawIr_new_generic(T->C, d->did, NULL);
            SET_NODE_TYPE(T->C, *pdecl, type);
        }
    }
    IrType *type = check_type(T, d->rhs, d->span);
    SET_NODE_TYPE(T->C, d->rhs, type);
    return type;
}

static paw_Bool is_adt_self(struct TypeChecker *T, IrType *adt)
{
    return T->self != NULL
               ? pawU_is_compat(T->C->U, T->self, adt)
               : PAW_FALSE;
}

static void ensure_accessible_field(struct TypeChecker *T, struct HirDecl *field, struct HirDecl *base, IrType *type)
{
    paw_Bool const is_pub = HirIsFieldDecl(field)
        ? HirGetFieldDecl(field)->is_pub
        : HirGetFnDecl(field)->is_pub;
    if (is_pub || is_adt_self(T, type))
        return; // field is public or control is inside own impl block

    struct HirIdent const ident = HirIsFieldDecl(field)
        ? HirGetFieldDecl(field)->ident
        : HirGetFnDecl(field)->ident;
    TYPECK_ERROR(T, associated_item_visibility, ident.span.start, ident.name->text,
            HirGetAdtDecl(base)->ident.name->text);
}

static IrType *select_field(struct TypeChecker *T, IrType *target, struct HirSelector *e)
{
    if (IrIsTuple(target)) {
        // tuples are indexed with `Expr "." int_lit`
        IrTypeList *types = IrGetTuple(target)->elems;
        if (!e->is_index)
            TYPECK_ERROR(T, expected_element_selector, e->ident.span.start);
        if (e->index >= types->count)
            TYPECK_ERROR(T, element_selector_out_of_range, e->span.start,
                    e->index, types->count);
        return IrTypeList_get(types, e->index);
    }
    if (!IrIsAdt(target)) {
        char const *repr = pawIr_print_type(T->C, target);
        TYPECK_ERROR(T, expected_adt, NODE_START(e->target), repr);
    }
    if (e->is_index)
        TYPECK_ERROR(T, expected_field_selector, e->span.start);

    struct HirAdtDecl *adt = get_adt(T, target);
    paw_assert(adt->is_struct);
    HirDeclList *fields = pawHir_struct_fields(adt);
    int const index = find_field(fields, e->ident.name);
    if (index < 0)
        TYPECK_ERROR(T, unknown_field, e->ident.span.start,
                e->ident.name->text, adt->ident.name->text);

    // refer to the field using its index from now on
    struct HirDecl *field = HirDeclList_get(fields, index);
    IrType *result = pawP_instantiate_field(T->C, target,
            GET_NODE_TYPE(T->C, field));
    e->is_index = PAW_TRUE;
    e->index = index;

    ensure_accessible_field(T, field, HIR_CAST_DECL(adt), target);
    return result;
}

static IrType *check_call_target(struct TypeChecker *T, struct HirExpr *target, int *pparam_offset)
{
    *pparam_offset = 0;
    if (!HirIsSelector(target))
        // normal function call (no receiver)
        return check_expr(T, target);

    struct HirSelector *select = HirGetSelector(target);
    IrType *self = check_operand(T, select->target);

    IrType *method = NULL;
    if (IrIsGeneric(self)) {
        struct IrGeneric *g = IrGetGeneric(self);
        method = pawIr_resolve_trait_method(T->C, g, select->ident.name);
        if (method == NULL)
            TYPECK_ERROR(T, unknown_method, select->ident.span.start,
                    select->ident.name->text, pawIr_print_type(T->C, self));
    } else {
        if (IrIsAdt(self)) {
            method = pawP_find_method(T->C, self, select->ident.name);
            if (method != NULL && IrIsSignature(method)
                    && IrGetSignature(method)->self != NULL) {
                method = ir_adt_types(self) != NULL
                    ? pawP_generalize_assoc(T->C, select->span.start, self, method)
                    : pawP_generalize(T->C, select->span.start, method);
            }
        }
        if (method == NULL)
            return select_field(T, self, select);
    }

    struct HirDecl *fn_decl = get_decl(T, IR_TYPE_DID(method));
    struct HirDecl *self_decl = get_decl(T, IR_TYPE_DID(self));
    if (HirGetFnDecl(fn_decl)->is_assoc)
        TYPECK_ERROR(T, not_a_method, NODE_START(target),
                   HirGetFnDecl(fn_decl)->ident.name->text);

    ensure_accessible_field(T, fn_decl, self_decl, self);
    *pparam_offset = 1;
    return method;
}

// Check a function call or enumerator constructor
static IrType *check_call_expr(struct TypeChecker *T, struct HirCallExpr *e)
{
    int param_offset; // offset of first non-receiver parameter
    IrType *target = check_call_target(T, e->target, &param_offset);
    if (!IR_IS_FUNC_TYPE(target)) {
        char const *repr = pawIr_print_type(T->C, target);
        TYPECK_ERROR(T, not_callable, NODE_START(e->target), repr);
    }
    SET_NODE_TYPE(T->C, e->target, target);

    struct IrFnPtr const *fn = IR_FPTR(target);
    int const nparams = fn->params->count - param_offset;
    if (e->args->count != nparams)
        TYPECK_ERROR(T, incorrect_arity, e->span.start, nparams, e->args->count);

    if (is_unit_variant(T, target)) {
        char const *repr = pawIr_print_type(T->C, fn->result);
        TYPECK_ERROR(T, unit_variant_with_parenthesis, NODE_START(e->target), repr);
    }

    int index;
    struct HirExpr *const *parg;
    K_LIST_ENUMERATE (e->args, index, parg) {
        IrType *param = IrTypeList_get(fn->params, index + param_offset);
        unify(T, NODE_START(*parg), param, check_operand(T, *parg));
    }

    if (IrIsNever(fn->result)) // function never returns
        unify_never_type(T, e->span.start, T->bs->result);

    return fn->result;
}

static IrType *check_conversion_expr(struct TypeChecker *T, struct HirConversionExpr *e)
{
    static int const ALLOWED_CASTS[NBUILTINS][NBUILTINS] = {
        //          to  = {0, b, x, i, f}
        [BUILTIN_BOOL]  = {0, 1, 1, 1, 1},
        [BUILTIN_CHAR]  = {0, 1, 1, 1, 0},
        [BUILTIN_INT]   = {0, 1, 1, 1, 1},
        [BUILTIN_FLOAT] = {0, 1, 0, 1, 1},
    };

    IrType *type = check_operand(T, e->arg);
    enum BuiltinKind const from = TYPE2CODE(T, type);

    if (!IS_BUILTIN_TYPE(from) || !ALLOWED_CASTS[from][e->to]) // TODO
        TYPECK_ERROR(T, incompatible_types, e->span.start, "", "TODO: should be invalid cast or something");

    return builtin_type(T, e->to);
}

static IrType *check_basic_lit(struct TypeChecker *T, struct HirBasicLit *e)
{
    return builtin_type(T, e->code);
}

static IrTypeList *check_operand_list(struct TypeChecker *T, struct HirExprList *list)
{
    if (list == NULL) return NULL;
    IrTypeList *new_list = IrTypeList_new(T->C);
    IrTypeList_reserve(T->C, new_list, list->count);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (list, pexpr) {
        IrType *type = check_operand(T, *pexpr);
        IrTypeList_push(T->C, new_list, type);
    }
    return new_list;
}

static IrType *check_tuple_lit(struct TypeChecker *T, struct HirTupleLit *e)
{
    IrTypeList *elems = check_operand_list(T, e->elems);
    if (elems->count == 0)
        return builtin_type(T, BUILTIN_UNIT);
    return pawIr_new_tuple(T->C, elems);
}

static IrType *check_list_lit(struct TypeChecker *T, struct HirContainerLit *e, struct SourceSpan span)
{
    IrType *elem_t = new_unknown(T, span.start);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (e->items, pexpr) {
        IrType *v = check_operand(T, *pexpr);
        unify(T, NODE_START(*pexpr), elem_t, v);
    }
    return new_list_t(T, span.start, elem_t);
}

static IrType *check_map_lit(struct TypeChecker *T, struct HirContainerLit *e, struct SourceSpan span)
{
    IrType *key_t = new_unknown(T, span.start);
    IrType *value_t = new_unknown(T, span.start);
    require_trait(T, key_t, TRAIT_HASH);
    require_trait(T, key_t, TRAIT_EQUALS);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH (e->items, pexpr) {
        struct HirFieldExpr *field = HirGetFieldExpr(*pexpr);
        paw_assert(field->fid == -1);
        IrType *k = check_operand(T, field->key);
        IrType *v = check_operand(T, field->value);
        unify(T, field->span.start, key_t, k);
        unify(T, field->span.start, value_t, v);
        SET_NODE_TYPE(T->C, *pexpr, v);
    }
    return new_map_t(T, span.start, key_t, value_t);
}

static IrType *check_container_lit(struct TypeChecker *T, struct HirContainerLit *e, struct SourceSpan span)
{
    if (e->code == BUILTIN_LIST) {
        return check_list_lit(T, e, span);
    } else {
        paw_assert(e->code == BUILTIN_MAP);
        return check_map_lit(T, e, span);
    }
}

static IrType *check_field_expr(struct TypeChecker *T, struct HirFieldExpr *e)
{
    if (e->fid < 0)
        check_operand(T, e->key);
    return check_operand(T, e->value);
}

static struct HirExprList *collect_field_exprs(struct TypeChecker *T, struct HirExprList *items, FieldMap *map, Str const *adt)
{
    struct HirExprList *order = HirExprList_new(T->hir);
    HirExprList_reserve(T->hir, order, items->count);

    int index;
    struct HirExpr *const *pexpr;
    K_LIST_ENUMERATE (items, index, pexpr) {
        struct HirIdent ident = HirGetFieldExpr(*pexpr)->ident;
        if (FieldMap_get(T, map, ident) != NULL)
            TYPECK_ERROR(T, duplicate_field, ident.span.start, ident.name->text, adt->text);

        FieldMap_insert(T, map, ident, index);
        HirExprList_push(T->hir, order, *pexpr);
    }
    return order;
}

static IrTypeList *subst_types(struct TypeChecker *T, IrTypeList *before, IrTypeList *after, IrTypeList *target)
{
    if (before == NULL)
        return target;
    paw_assert(before->count == after->count);

    struct IrTypeFolder F;
    pawP_init_substitution_folder(&F, T->C, &T->subst, before, after);
    return pawIr_fold_type_list(&F, target);
}

static IrType *check_composite_lit(struct TypeChecker *T, struct HirCompositeLit *e, struct SourceSpan span)
{
    IrType *type = lower_type_path(T, e->path);
    if (!IrIsAdt(type)) {
        char const *repr = pawIr_print_type(T->C, type);
        TYPECK_ERROR(T, expected_struct, span.start, repr);
    }
    struct HirDecl *decl = get_decl(T, IR_TYPE_DID(type));

    // Use a temporary Map to avoid searching repeatedly through the list of fields.
    FieldMap *map = FieldMap_new(T);

    struct HirAdtDecl *adt = HirGetAdtDecl(decl);
    if (!adt->is_struct)
        TYPECK_ERROR(T, expected_struct, adt->ident.span.start, adt->ident.name->text);

    HirDeclList *fields = pawHir_struct_fields(adt);
    IrTypeList *field_types = pawHir_collect_decl_types(T->C, fields);
    if (fields->count == 0)
        TYPECK_ERROR(T, unit_struct_with_braces, adt->ident.span.start, adt->ident.name->text);

    IrType *base_type = pawIr_get_type(T->C, adt->id);
    field_types = subst_types(T, ir_adt_types(base_type), ir_adt_types(type), field_types);
    struct HirExprList *order = collect_field_exprs(T, e->items, map, adt->ident.name);

    int index = 0;
    IrType *const *ptype;
    struct HirDecl *const *pdecl;
    K_LIST_ZIP (fields, pdecl, field_types, ptype) {
        struct HirFieldDecl *field = HirGetFieldDecl(*pdecl);
        ensure_accessible_field(T, *pdecl, decl, type);
        int const *pindex = FieldMap_get(T, map, field->ident);
        if (pindex == NULL)
            TYPECK_ERROR(T, missing_field, span.start,
                    field->ident.name->text, adt->ident.name->text);

        struct HirExpr *item = HirExprList_get(order, *pindex);
        unify(T, field->span.start, *ptype, check_operand(T, item));
        FieldMap_remove(T, map, field->ident);
        HirGetFieldExpr(item)->fid = index++;
    }
    FieldMapIterator iter;
    FieldMapIterator_init(map, &iter);
    if (FieldMapIterator_is_valid(&iter)) {
        struct HirIdent key = FieldMapIterator_key(&iter);
        TYPECK_ERROR(T, unknown_field, span.start,
                   key.name->text, adt->ident.name->text);
    }
    paw_assert(fields->count == e->items->count);
    FieldMap_delete(T, map);

    e->items = order;
    return type;
}

static IrType *check_literal_expr(struct TypeChecker *T, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitBasic:
            return check_basic_lit(T, &e->basic);
        case kHirLitTuple:
            return check_tuple_lit(T, &e->tuple);
        case kHirLitContainer:
            return check_container_lit(T, &e->cont, e->span);
        case kHirLitComposite:
            return check_composite_lit(T, &e->comp, e->span);
    }
}

static void CheckExprStmt(struct TypeChecker *T, struct HirExprStmt *s)
{
    IrType *type = check_operand(T, s->expr);
    PAW_UNUSED(type);
}

static IrType *check_loop_expr(struct TypeChecker *T, struct HirLoopExpr *e)
{
    struct BlockState bs;
    enter_block(T, &bs, e->span, BLOCK_LOOP);

    IrType *result = check_expr(T, e->block);
    unify_unit_type(T, e->span.start, result);

    leave_block(T);

    // loops that have no local or nonlocal jumps never complete
    return unify_never_type(T, e->span.start, bs.result);
}

static IrType *range_index(struct TypeChecker *T, struct SourceLoc loc, enum BuiltinKind range_kind)
{
    IrType *expect = builtin_type(T, range_kind);
    if (range_kind != BUILTIN_RANGE_FULL) {
        IrTypeList *types = IrTypeList_new(T->C);
        IrTypeList_push(T->C, types, builtin_type(T, BUILTIN_INT));
        return instantiate(T, loc, expect, types);
    }
    // RangeFull is not polymorphic
    return expect;
}

static IrType *check_list_index(struct TypeChecker *T, struct HirIndex *e, IrType *target)
{
    IrType *index = check_operand(T, e->index);
    enum BuiltinKind kind = TYPE2CODE(T, index);
    if (kind == BUILTIN_INT)
        return ir_list_elem(target);

    // index must have type Range*<int>
    unify(T, NODE_START(e->index), index, range_index(T, e->span.start, kind));
    return target;
}

static IrType *check_map_index(struct TypeChecker *T, struct HirIndex *e, IrType *target)
{
    IrType *index = check_operand(T, e->index);
    unify(T, NODE_START(e->index), index, ir_map_key(target));
    return ir_map_value(target);
}

static IrType *check_str_index(struct TypeChecker *T, struct HirIndex *e, IrType *target)
{
    IrType *index = check_operand(T, e->index);
    enum BuiltinKind index_kind = TYPE2CODE(T, index);
    if (index_kind == BUILTIN_INT)
        return builtin_type(T, BUILTIN_CHAR);

    // index must have type Range*<int>
    unify(T, NODE_START(e->index), index, range_index(T, e->span.start, index_kind));
    return target;
}

static IrType *check_index(struct TypeChecker *T, struct HirIndex *e)
{
    IrType *target = check_operand(T, e->target);
    switch (TYPE2CODE(T, target)) {
        case BUILTIN_LIST:
            return check_list_index(T, e, target);
        case BUILTIN_MAP:
            return check_map_index(T, e, target);
        case BUILTIN_STR:
            return check_str_index(T, e, target);
        default: {
            char const *repr = pawIr_print_type(T->C, target);
            TYPECK_ERROR(T, invalid_index_target, e->span.start, repr);
        }
    }
}

static IrType *check_selector(struct TypeChecker *T, struct HirSelector *e)
{
    IrType *target = check_operand(T, e->target);
    return select_field(T, target, e);
}

static void CheckDeclStmt(struct TypeChecker *T, struct HirDeclStmt *s)
{
    check_decl(T, s->decl);
}

struct BindingChecker {
    struct HirVisitor *V;
    struct TypeChecker *T;
    struct BindingMap *bound;
    int iter;
};

struct BindingInfo {
    IrType *type;
    int uses;
};

DEFINE_MAP(struct TypeChecker, BindingMap, pawP_alloc, ident_hash, ident_equals, struct HirIdent, struct BindingInfo)
DEFINE_MAP_ITERATOR(BindingMap, struct HirIdent, struct BindingInfo)

static void init_binding_checker(struct BindingChecker *bc, struct TypeChecker *T, struct HirVisitor *V)
{
    *bc = (struct BindingChecker){
        .bound = BindingMap_new(T),
        .T = T,
        .V = V,
    };
    pawHir_visitor_init(V, T->hir, bc);
}

static void uninit_binding_checker(struct BindingChecker *bc)
{
    BindingMap_delete(bc->T, bc->bound);
}

static void account_for_binding(struct TypeChecker *T, struct HirIdent ident)
{
    struct PatState *ps = T->ms->ps;
    while (ps->outer != NULL) {
        if (ps->outer->kind == kHirOrPat)
            break;
        ps = ps->outer;
    }
    Str *const *pname = StringMap_get(T->C, ps->bound, ident.name);
    if (pname != NULL)
        TYPECK_ERROR(T, duplicate_binding, ident.span.start, ident.name->text);
    StringMap_insert(T->C, ps->bound, ident.name, ident.name);
}

static void locate_binding(struct HirVisitor *V, struct HirBindingPat *p)
{
    struct BindingChecker *bc = V->ud;
    // all bindings must be specified in the first alternative
    IrType *type = pawIr_get_type(V->hir->C, p->id);
    BindingMap_insert(bc->T, bc->bound, p->ident, (struct BindingInfo){
                                                     .type = type,
                                                 });
}

static void check_binding(struct HirVisitor *V, struct HirBindingPat *p)
{
    paw_Env *P = ENV(V->hir);
    struct BindingChecker *bc = V->ud;
    struct BindingInfo *pbi = BindingMap_get(bc->T, bc->bound, p->ident);
    if (pbi == NULL)
        TYPECK_ERROR(bc->T, missing_binding_in_alternative, p->span.start,
                p->ident.name->text);

    IrType *type = pawIr_get_type(V->hir->C, p->id);
    unify(bc->T, p->span.start, pbi->type, type);
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
            TYPECK_ERROR(bc->T, missing_binding_in_alternative, key.span.start, key.name->text);
        } else if (bi.uses > bc->iter) {
            struct HirIdent const key = BindingMapIterator_key(&iter);
            TYPECK_ERROR(bc->T, duplicate_binding, key.span.start, key.name->text);
        }
        BindingMapIterator_next(&iter);
    }
}

static IrType *CheckOrPat(struct TypeChecker *T, struct HirOrPat *p)
{
    struct HirVisitor V;
    struct BindingChecker bc;
    init_binding_checker(&bc, T, &V);

    paw_assert(p->pats->count > 1);
    struct HirPat *first = K_LIST_FIRST(p->pats);
    IrType *type = check_pat(T, first);

    // populate map with bindings from first pattern, checking for
    // duplicates
    V.PostVisitBindingPat = locate_binding;
    pawHir_visit_pat(&V, first);

    // rest of the patterns must bind variables of the same name and
    // type as the first pattern (position can vary)
    V.PostVisitBindingPat = check_binding;
    for (bc.iter = 1; bc.iter < p->pats->count; ++bc.iter) {
        struct HirPat *next = HirPatList_get(p->pats, bc.iter);
        unify(T, p->span.start, type, check_pat(T, next));

        pawHir_visit_pat(&V, next);
        ensure_all_bindings_created(&bc);
    }
    uninit_binding_checker(&bc);
    return type;
}

static IrType *CheckFieldPat(struct TypeChecker *T, struct HirFieldPat *p)
{
    return check_pat(T, p->pat);
}

static IrType *CheckStructPat(struct TypeChecker *T, struct HirStructPat *p)
{
    IrType *type = lower_type_path(T, p->path);

    struct HirVariantDecl *v;
    struct HirDecl *decl = pawHir_get_decl(T->hir, IR_TYPE_DID(type));
    if (HirIsAdtDecl(decl)) {
        struct HirAdtDecl const *adt = HirGetAdtDecl(decl);
        v = HirGetVariantDecl(K_LIST_FIRST(adt->variants));
    } else {
        v = HirGetVariantDecl(decl);
    }
    IrTypeList *adt_fields = instantiate_fields(T->C, type, v->fields);
    PatFieldMap *map = PatFieldMap_new(T);

    struct HirPat *const *pfield;
    K_LIST_FOREACH (p->fields, pfield) {
        check_pat(T, *pfield);
        struct HirIdent const field_ident = HirGetFieldPat(*pfield)->ident;
        struct HirPat *const *ppat = PatFieldMap_get(T, map, field_ident);
        if (ppat != NULL)
            TYPECK_ERROR(T, duplicate_field, field_ident.span.start,
                    field_ident.name->text, v->ident.name->text);
        PatFieldMap_insert(T, map, field_ident, *pfield);
    }

    struct HirPatList *sorted = HirPatList_new(T->hir);
    HirPatList_reserve(T->hir, sorted, adt_fields->count);

    int index = 0;
    IrType *const *ptype;
    struct HirDecl *const *pdecl;
    K_LIST_ZIP (v->fields, pdecl, adt_fields, ptype) {
        struct HirFieldDecl *field = HirGetFieldDecl(*pdecl);
        struct HirPat *const *ppat = PatFieldMap_get(T, map, field->ident);
        if (ppat == NULL)
            TYPECK_ERROR(T, missing_field, field->ident.span.start,
                    field->ident.name->text, v->ident.name->text);
        struct HirFieldPat *field_pat = HirGetFieldPat(*ppat);
        // TODO: source loc should be inside composite lit, not struct def
        unify(T, field->span.start, pawIr_get_type(T->C, field_pat->id), *ptype);
        HirPatList_push(T->hir, sorted, *ppat);
        PatFieldMap_remove(T, map, field->ident);
        field_pat->index = index++;
    }
    p->fields = sorted;

    if (PatFieldMap_length(map) > 0) {
        PatFieldMapIterator iter;
        // use an iterator to find one of the unknown fields
        PatFieldMapIterator_init(map, &iter);
        struct HirIdent ident = PatFieldMapIterator_key(&iter);
        TYPECK_ERROR(T, unknown_field, ident.span.start, ident.name->text,
                v->ident.name->text);
    }
    PatFieldMap_delete(T, map);
    return type;
}

static IrType *CheckVariantPat(struct TypeChecker *T, struct HirVariantPat *p)
{
    IrType *type = lower_value_path(T, p->path);
    if (IrIsAdt(type)) {
        // must be unit structure
        return type;
    }
    struct IrSignature *fsig = IrGetSignature(type);
    IrTypeList *params = fsig->params;

    struct MatchState *ms = T->ms;
    if (p->fields->count != params->count)
        TYPECK_ERROR(T, incorrect_arity, p->span.start, params->count, p->fields->count);

    struct HirPat *const *ppat;
    IrType *const *pparam;
    K_LIST_ZIP (params, pparam, p->fields, ppat) {
        IrType *const arg = check_pat(T, *ppat);
        unify(T, NODE_START(*ppat), *pparam, arg);
    }

    struct HirVariantDecl *d = HirGetVariantDecl(get_decl(T, fsig->did));
    p->index = d->index;
    return fsig->result;
}

static IrType *CheckTuplePat(struct TypeChecker *T, struct HirTuplePat *p)
{
    IrTypeList *elems = check_pat_list(T, p->elems);
    if (elems->count == 0)
        return builtin_type(T, BUILTIN_UNIT);
    return pawIr_new_tuple(T->C, elems);
}

static IrType *CheckBindingPat(struct TypeChecker *T, struct HirBindingPat *p)
{
    // binding type is determined using unification
    IrType *type = new_unknown(T, p->span.start);

    // make sure bindings are unique within each pattern (OR patterns are special-cased)
    account_for_binding(T, p->ident);
    return type;
}

static IrType *CheckWildcardPat(struct TypeChecker *T, struct HirWildcardPat *p)
{
    return new_unknown(T, p->span.start);
}

static IrType *CheckLiteralPat(struct TypeChecker *T, struct HirLiteralPat *p)
{
    return check_operand(T, p->expr);
}

static void check_decl(struct TypeChecker *T, struct HirDecl *decl)
{
    IrType *type;
    switch (HIR_KINDOF(decl)) {
        case kHirFieldDecl:
            type = check_field_decl(T, HirGetFieldDecl(decl));
            break;
        default: // kHirTypeDecl
            type = check_type_decl(T, HirGetTypeDecl(decl));
    }
    type = normalize(T, type);
    SET_NODE_TYPE(T->C, decl, type);
}

static IrType *check_pat(struct TypeChecker *T, struct HirPat *pat)
{
    struct PatState ps;
    enter_pat(T, &ps, pat->hdr.kind);

    IrType *type;
    switch (HIR_KINDOF(pat)) {
#define DEFINE_CASE(X)                              \
            case kHir##X:                           \
                type = Check##X(T, HirGet##X(pat)); \
                break;
        HIR_PAT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }

    type = normalize(T, type);
    SET_NODE_TYPE(T->C, pat, type);

    leave_pat(T);
    return type;
}

static void check_stmt(struct TypeChecker *T, struct HirStmt *stmt)
{
    switch (HIR_KINDOF(stmt)) {
#define DEFINE_CASE(X)                    \
        case kHir##X:                     \
            Check##X(T, HirGet##X(stmt)); \
            break;
        HIR_STMT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

// NOTE: Some expressions are known to directly represent types, based on the context
//       (type annotations, type arguments, etc.). Call check_type() to convert such
//       an expression into an IR type.

static IrType *check_type(struct TypeChecker *T, struct HirType *type, struct SourceSpan span)
{
    if (type != NULL) return lower_type(T, type);
    return new_unknown(T, span.start);
}

static void unconditional_return(struct TypeChecker *T, struct SourceLoc loc)
{
    struct BlockState *bs = T->bs;
    do {
         if (bs->outer == NULL) break;
         unify_never_type(T, loc, bs->result);
         bs = bs->outer;
    } while (bs->kind != BLOCK_MATCH);
}

static IrType *check_return_expr(struct TypeChecker *T, struct HirReturnExpr *e)
{
    unconditional_return(T, e->span.start);

    IrType *want = T->rs->prev;
    IrType *have = e->expr != NULL
                              ? check_operand(T, e->expr)
                              : builtin_type(T, BUILTIN_UNIT);
    unify(T, e->span.start, have, want);
    ++T->rs->count;

    return pawIr_new_never(T->C);
}

static struct BlockState *unconditional_jump(struct TypeChecker *T, struct SourceLoc loc)
{
    struct BlockState *bs = T->bs;
    while (bs->outer != NULL && bs->kind == BLOCK_NORMAL) {
         unify_never_type(T, loc, bs->result);
         bs = bs->outer;
    }
    return bs;
}

static IrType *check_jump_expr(struct TypeChecker *T, struct HirJumpExpr *e)
{
    struct BlockState *bs = unconditional_jump(T, e->span.start);
    if (e->jump_kind == JUMP_BREAK) {
        // "break" leaves the enclosing loop, causing it to evaluate to "()"
        while (bs->kind != BLOCK_LOOP) bs = bs->outer;
        unify_unit_type(T, e->span.start, bs->result);
    }
    return pawIr_new_never(T->C);
}

static IrType *check_expr(struct TypeChecker *T, struct HirExpr *expr)
{
    IrType *type;
    switch (HIR_KINDOF(expr)) {
        case kHirAscriptionExpr:
            type = check_ascription_expr(T, HirGetAscriptionExpr(expr));
            break;
        case kHirLiteralExpr:
            type = check_literal_expr(T, HirGetLiteralExpr(expr));
            break;
        case kHirLogicalExpr:
            type = check_logical_expr(T, HirGetLogicalExpr(expr));
            break;
        case kHirPathExpr:
            type = check_path_expr(T, HirGetPathExpr(expr));
            break;
        case kHirChainExpr:
            type = check_chain_expr(T, HirGetChainExpr(expr));
            break;
        case kHirUnOpExpr:
            type = check_unop_expr(T, HirGetUnOpExpr(expr));
            break;
        case kHirBinOpExpr:
            type = check_binop_expr(T, HirGetBinOpExpr(expr));
            break;
        case kHirClosureExpr:
            type = check_closure_expr(T, HirGetClosureExpr(expr));
            break;
        case kHirConversionExpr:
            type = check_conversion_expr(T, HirGetConversionExpr(expr));
            break;
        case kHirCallExpr:
            type = check_call_expr(T, HirGetCallExpr(expr));
            break;
        case kHirIndex:
            type = check_index(T, HirGetIndex(expr));
            break;
        case kHirSelector:
            type = check_selector(T, HirGetSelector(expr));
            break;
        case kHirAssignExpr:
            type = check_assign_expr(T, HirGetAssignExpr(expr));
            break;
        case kHirOpAssignExpr:
            type = check_op_assign_expr(T, HirGetOpAssignExpr(expr));
            break;
        case kHirFieldExpr:
            type = check_field_expr(T, HirGetFieldExpr(expr));
            break;
        case kHirReturnExpr:
            type = check_return_expr(T, HirGetReturnExpr(expr));
            break;
        case kHirJumpExpr:
            type = check_jump_expr(T, HirGetJumpExpr(expr));
            break;
        case kHirLoopExpr:
            type = check_loop_expr(T, HirGetLoopExpr(expr));
            break;
        case kHirMatchExpr:
            type = check_match_expr(T, HirGetMatchExpr(expr));
            break;
        case kHirBlock:
            type = check_block(T, HirGetBlock(expr));
            break;
        case kHirMatchArm:
            type = check_match_arm(T, HirGetMatchArm(expr));
            break;
    }

    type = normalize(T, type);
    SET_NODE_TYPE(T->C, expr, type);

    return type;
}

static void check_item(struct TypeChecker *T, struct HirDecl *item)
{
    if (HirIsFnDecl(item)) {
        check_fn_item(T, HirGetFnDecl(item));
    } else if (HirIsAdtDecl(item)) {
        check_adt_item(T, HirGetAdtDecl(item));
    } else if (HirIsConstDecl(item)) {
        check_const_item(T, HirGetConstDecl(item));
    }
}

static void check_items(struct TypeChecker *T, struct HirDeclList *items)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (items, pitem)
        check_item(T, *pitem);
}

static void use_module(struct TypeChecker *T, struct HirModule const *pm)
{
    T->pm = pm;
}

static void check_module_types(struct TypeChecker *T, struct HirModule m)
{
    DLOG(T, "resolving '%s'", m->name->text);

    check_items(T, m.items);
}

static void check_types(struct TypeChecker *T)
{
    struct HirModule const *pm;
    K_LIST_FOREACH (T->hir->modules, pm) {
        use_module(T, pm);
        enter_inference_ctx(T);
        check_module_types(T, *pm);
        leave_inference_ctx(T);
    }
}

void pawP_check_types(struct Compiler *C)
{
    struct Pool *pool = pawP_pool_new(C, C->aux_stats);

    // determine the type of each toplevel item in each module (allows the type checker
    // to resolve paths between modules immediately)
    pawP_collect_items(C, pool);

    struct TypeChecker T = {
        .hir = C->hir,
        .pool = pool,
        .P = ENV(C),
        .U = C->U,
        .C = C,
    };

    // run the type checker
    check_types(&T);

    pawP_pool_free(C, pool);
}

