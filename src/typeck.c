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
#include "hir.h"
#include "impl.h"
#include "ir_type.h"
#include "map.h"
#include "parse.h"
#include "solve.h"
#include "str.h"
#include "type_folder.h"
#include "unify.h"

#define STRING_LIT(T_, Str_) SCAN_STR((T_)->C, Str_)
#define TYPE2CODE(T_, Type_) pawP_type2code((T_)->C, Type_)
#define NODE_SPAN(Node_) ((Node_)->hdr.span)
#define GET_TYPE(T_, Id_) pawU_normalize_projections((T_)->U, pawIr_get_type((T_)->C, Id_))
#define SET_TYPE(T_, Id_, Type_) pawIr_set_type((T_)->C, Id_, Type_)

#define TYPECK_ERROR(T_, Kind_, ...) THROW_ERROR((T_)->C, \
        Kind_, .modname = (T_)->pm->name, __VA_ARGS__)

#define TODO (struct SourceSpan){0}

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
    IrType *self; // type of "Self"
    struct ResultState *rs;
    struct MatchState *ms;
    struct BlockState *bs;
    struct HirModule const *pm;
    struct Hir *hir;
    paw_Env *P;
};

static IrType *new_ptr(struct TypeChecker *T, IrType *pointee)
{
    return pawIr_new_ptr(T->C, pointee);
}

static IrType *auto_deref_full(IrType *type)
{
    while (IrIsPtr(type))
        type = ir_deref(type);
    return type;
}

static paw_Uint ident_hash(struct TypeChecker *T, struct HirIdent ident)
{
    return P_PTR_HASH(T, ident.name);
}

static paw_Bool ident_equals(struct TypeChecker *T, struct HirIdent a, struct HirIdent b)
{
    PAW_UNUSED(T);
    return pawS_eq(a.name, b.name);
}

static paw_Bool equals_core_trait(struct TypeChecker *T, IrTrait *trait, enum CoreTrait kind)
{
    return trait->did.value == T->C->core_traits[kind].value;
}

DEFINE_MAP(struct TypeChecker, FieldMap, pawP_alloc, ident_hash, ident_equals, struct HirIdent, int)
DEFINE_MAP(struct TypeChecker, PatFieldMap, pawP_alloc, ident_hash, ident_equals, struct HirIdent, struct HirPat *)
DEFINE_MAP_ITERATOR(FieldMap, struct HirIdent, int)
DEFINE_MAP_ITERATOR(PatFieldMap, struct HirIdent, struct HirPat *)

static void check_stmt(struct TypeChecker *, struct HirStmt *);
static void check_decl(struct TypeChecker *, struct HirDecl *);
static IrType *check_lvalue(struct TypeChecker *, struct HirExpr *);
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

static IrGenericArg lower_generic_arg(struct TypeChecker *T, struct HirGenericArg arg)
{
    return pawP_lower_generic_arg(T->C, *T->pm, arg);
}

static IrGenericArgs *lower_generic_args(struct TypeChecker *T, struct HirGenericArgs *args)
{
    if (args == NULL) return NULL;
    IrGenericArgs *result = IrGenericArgs_new(T->C);

    K_LIST_XFOREACH (args, struct HirGenericArg const, p) {
        IrGenericArg const arg = lower_generic_arg(T, *p);
        IrGenericArgs_push(T->C, result, arg);
    }
    return result;
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

static struct IrSolverResult solve_pending_obligations(struct TypeChecker *T)
{
    struct IrSolverResult const result = pawIr_solver_solve(T->C->S);
    switch (result.status) {
        case IR_SOLVER_SOLVED:
        case IR_SOLVER_AMBIGUOUS:
            break;
        case IR_SOLVER_ERROR:
            TYPECK_ERROR(T, FalseObligation,
                    .obligation = pawIr_print_obligation_(T->C, result.error.obligation));
    }
    if (pawIr_solve_const_obligations(T->C) < 0)
        TYPECK_ERROR(T, FalseConstObligation, .span = {0});
    return result;
}

static IrType *normalize_type(struct TypeChecker *T, IrType *type)
{
    return pawU_normalize_projections(T->U, type);
}

static IrTrait *normalize_trait(struct TypeChecker *T, IrTrait *trait)
{
    return pawIr_normalize_trait(T->C, trait);
}

static IrGenericArg unify_args(struct TypeChecker *T, struct SourceSpan span, IrGenericArg a, IrGenericArg b)
{
    if (pawIr_unify(T->C, a, b) != 0) {
        // TODO: non-type generics
        TYPECK_ERROR(T, IncompatibleTypes,
                .lhs = pawIr_print_type_v2(T->C, IrGenericArg_get_type(a)),
                .rhs = pawIr_print_type_v2(T->C, IrGenericArg_get_type(b)),
                .span = span);
    }
    return pawIr_normalize(T->C, a);
}

static IrType *unify_types(struct TypeChecker *T, struct SourceSpan span, IrType *a, IrType *b)
{
    if (pawU_unify(T->U, a, b) != 0)
        TYPECK_ERROR(T, IncompatibleTypes,
                .lhs = pawIr_print_type_v2(T->C, normalize_type(T, a)),
                .rhs = pawIr_print_type_v2(T->C, normalize_type(T, b)),
                .span = span);
    return normalize_type(T, IrIsNever(a) ? b : a);
}

static IrTrait *unify_traits(struct TypeChecker *T, struct SourceSpan span, IrTrait *a, IrTrait *b)
{
    if (pawIr_unify_traits(T->C, a, b) != 0)
        TYPECK_ERROR(T, IncompatibleTypes, // TODO: IncompatibleTraits (traits are not types)
                .lhs = pawIr_print_trait_v2(T->C, normalize_trait(T, a)),
                .rhs = pawIr_print_trait_v2(T->C, normalize_trait(T, b)),
                .span = span);
    return normalize_trait(T, a);
}

static struct HirDecl *get_decl(struct TypeChecker *T, DeclId did)
{
    return pawHir_get_decl(T->hir, did);
}

static IrType *builtin_type(struct TypeChecker *T, enum BuiltinKind kind)
{
    return pawP_builtin_type(T->C, kind);
}

static IrType *unify_unit_type(struct TypeChecker *T, struct SourceSpan span, IrType *type)
{
    return unify_types(T, span, type, builtin_type(T, BUILTIN_UNIT));
}

static IrType *unify_never_type(struct TypeChecker *T, struct SourceSpan span, IrType *type)
{
    return unify_types(T, span, type, pawIr_new_never(T->C));
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

static IrType *maybe_simple_variant(struct TypeChecker *T, struct SourceSpan span, IrType *type)
{
    if (IrIsSignature(type)) {
        // handle enumerators with no fields
        enum IrDefKind const kind = pawIr_get_kind(T->C, IR_TYPE_DID(type));
        if (kind == IR_VARIANT_DEF) {
            struct IrVariantDef const *def = pawIr_get_variant_def(T->C, IR_TYPE_DID(type));
            if (def->fields->count > 0)
                TYPECK_ERROR(T, MissingVariantArgs,
                        .cons = def->name,
                        .span = span);
            return pawIr_get_context(T->C, type);
        }
    }
    return type;
}

static IrTypeList *copy_typelist(struct TypeChecker *T, IrTypeList *types)
{
    IrType *const *ptype;
    IrTypeList *result = IrTypeList_new(T->C);
    K_LIST_FOREACH (types, ptype)
        IrTypeList_push(T->C, result, *ptype);
    return result;
}

static IrType *check_operandx(struct TypeChecker *T, struct HirExpr *expr)
{
    IrType *type = check_expr(T, expr);
    type = maybe_simple_variant(T, expr->hdr.span, type);
    SET_NODE_TYPE(T->C, expr, type); // overwrite type
    if (IrIsSignature(type)) {
        struct HirDecl *decl = get_decl(T, IrGetSignature(type)->did);
        if (!HirIsVariantDecl(decl)) {
            // erase identity of function
            struct IrFnPtr const *fn = IrGetFnPtr(IR_GET_FN(T->C, type));
            IrTypeList *params = copy_typelist(T, fn->params);
            return pawIr_new_fn_ptr(T->C, params, fn->result);
        }
    }
    return type;
}

static void ensure_valid_rvalue(struct TypeChecker *T, struct HirExpr *expr)
{
    IrType *type = GET_TYPE(T, expr->hdr.id);
    if (!pawIr_is_copyable(T->C, type)) {
        if (HirIsIndex(expr)) {
            TYPECK_ERROR(T, MoveOutOfElement,
                    .type = pawIr_print_type_v2(T->C, type),
                    .span = NODE_SPAN(expr));
        } else if (HirIsSelector(expr)) {
            TYPECK_ERROR(T, MoveOutOfField,
                    .type = pawIr_print_type_v2(T->C, type),
                    .span = NODE_SPAN(expr));
        } else if (HIR_IS_UNOP(expr, UNARY_DEREF)) {
            TYPECK_ERROR(T, MoveOutOfPointer,
                    .type = pawIr_print_type_v2(T->C, type),
                    .span = NODE_SPAN(expr));
        }
    }
}

static IrType *check_operand(struct TypeChecker *T, struct HirExpr *expr)
{
    IrType *type = check_operandx(T, expr);
    ensure_valid_rvalue(T, expr);
    return type;
}

static void expect_bool_expr(struct TypeChecker *T, struct HirExpr *e)
{
    IrType *type = check_operand(T, e);
    unify_types(T, NODE_SPAN(e), type, builtin_type(T, BUILTIN_BOOL));
}

static void expect_int_expr(struct TypeChecker *T, struct HirExpr *e)
{
    IrType *type = check_operand(T, e);
    unify_types(T, NODE_SPAN(e), type, builtin_type(T, BUILTIN_INT));
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

static IrType *new_unknown(struct TypeChecker *T, struct SourceSpan span)
{
    return pawU_new_unknown(T->U, span);
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
        .result = new_unknown(T, span),
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
        bs.result = unify_types(T, block->span, result, bs.result);
    } else {
        unify_unit_type(T, block->span, bs.result);
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
    struct BlockState bs;
    enter_block(T, &bs, d->span, BLOCK_NORMAL);

    IrGenericArgs *params = pawIr_get_generic_args(T->C, d->did);
    pawIr_solver_add_predicates_from(T->C->S, d->did, params);
    IrType *ret = normalize_type(T,
            GET_NODE_TYPE(T->C, d->result));

    struct ResultState rs = {
        // named function has explicit return type
        .outer = T->rs,
        .prev = ret,
    };
    T->rs = &rs;


    IrType *result = check_operand(T, d->body);
    bs.result = unify_types(T, d->span, result, bs.result);
    unify_types(T, d->span, bs.result, ret);

    leave_block(T);
    T->rs = rs.outer;
}

static void check_item(struct TypeChecker *T, struct HirDecl *item);

static void check_methods(struct TypeChecker *T, struct HirDeclList *methods)
{
    struct HirDecl *const *pmethod;
    K_LIST_FOREACH (methods, pmethod)
        check_item(T, *pmethod);
}

IrType *pawP_instantiate_field(struct Compiler *C, IrType *inst_type, IrType *field)
{
    struct IrAdt *t = IrGetAdt(inst_type);
    if (t->args == NULL) return field;

    struct IrTypeFolder F;
    struct Substitution subst;
    IrType *base_type = pawIr_get_def_type(C, IR_TYPE_DID(inst_type));
    pawP_init_substitution_folder(&F, C, &subst, IR_GENERIC_ARGS(base_type), t->args);
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
    IrGenericArgs *generics = IR_GENERIC_ARGS(GET_NODE_TYPE(C, decl));
    pawP_init_substitution_folder(&F, C, &subst, generics, IrGetAdt(self)->args);
    IrTypeList *field_types = pawHir_collect_decl_types(C, fields);
    return pawIr_fold_type_list(&F, field_types);
}

static IrType *instantiate(struct TypeChecker *T, IrType *base, IrGenericArgs *args)
{
    if (IrIsGeneric(base) || IrIsSlice(base) || IrIsTuple(base) || IrIsFnPtr(base) || IrIsPtr(base) || IS_BASIC_TYPE(pawP_type2code(T->C, base)))
        return base;

    if (IR_GENERIC_ARGS(base) == NULL || (IrIsSignature(base) && IR_GENERIC_ARGS(base)->count == 0))
        return base;

    if (args != NULL && IR_GENERIC_ARGS(base)->count != args->count)
        TYPECK_ERROR(T, IncorrectTypeArity,
                .want = IR_GENERIC_ARGS(base)->count,
                .have = args->count,
                .span = {0});

    DeclId const did = IR_TYPE_DID(base);
    if (args == NULL) args = pawIr_instantiate_args(T->C, did);
    pawIr_solver_add_obligations_from(T->C->S, did, args);
    return pawIr_solver_instantiate_type_with(T->C->S, did, args);
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static IrType *lower_adt_segment(struct TypeChecker *T, struct HirSegment segment)
{
    struct HirDecl *decl = pawHir_get_node(T->hir, segment.target);
    if (HirIsTypeDecl(decl)) {
        // TODO: handle count == 0 case same as ret == NULL case
        IrGenericArgs *args = segment.args != NULL ? lower_generic_args(T, segment.args) : NULL;
        return pawP_lower_type_alias(T->C, segment, decl, args);
    }

    IrType *type = GET_TYPE(T, segment.target);

    // TODO: hack to avoid instantiating "Self" since it is already instantiated. Probably need a way to distinguish between
    // TODO: generic params (type schemes) and generic args (concrete not-yet-known types). currently IrGeneric is used for both
    if (pawS_eq(SCAN_STR(T->C, "Self"), segment.ident.name))
        return type;

    IrGenericArgs *args = NULL;
    if (segment.args != NULL)
        args = lower_generic_args(T, segment.args);

    // Instantiate with type arguments, if provided. Otherwise, instantiate with
    // a list of IrInfer types which must be resolved later via unification.
    return instantiate(T, type, args);
}

static IrType *lower_type_path(struct TypeChecker *T, struct HirPath path)
{
    paw_assert(path.kind == HIR_PATH_ITEM);
    paw_assert(path.segments->count == 1);

    return lower_adt_segment(T, K_LIST_LAST(path.segments));
}

static void unify_segment_types(struct TypeChecker *T, struct HirSegment segment, IrGenericArgs *params, IrGenericArgs *args)
{
    int index;
    struct HirGenericArg const *p;
    K_LIST_ENUMERATE (segment.args, index, p) {
        IrGenericArg const a = IrGenericArgs_get(params, index);
        IrGenericArg const b = IrGenericArgs_get(args, index);
        unify_args(T, NODE_SPAN(p->t), a, b);
    }
}

static IrType *lookup_method(struct Compiler *C, IrType *self, Str *name);

static IrTrait *get_containing_bound(struct Compiler *C, IrType *base, DeclId did)
{
    IrTraitList *bounds = pawIr_get_trait_bounds(C, IR_TYPE_DID(base));
    IrTrait *result /* always set in loop */;

    IrTrait *const *b;
    K_LIST_FOREACH (bounds, b) {
        IrType *const *f;
        struct IrTraitDef const *def = pawIr_get_trait_def(C, (*b)->did);
        K_LIST_FOREACH (def->methods, f) {
            if (IR_TYPE_DID(*f).value == did.value) {
                result = *b;
                break;
            }
        }
    }

    return result;
}

static IrGenericArgs *arglist_suffix(struct TypeChecker *T, IrGenericArgs *types, int n)
{
    paw_assert(0 < n && n <= types->count);
    IrGenericArgs *suffix = IrGenericArgs_new(T->C);
    IrGenericArgs_reserve(T->C, suffix, n);
    IrGenericArg const *p = K_LIST_END(types) - n;
    for (int i = 0; i < n; ++i)
        IrGenericArgs_push(T->C, suffix, *p++);
    return suffix;
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
                if (IS_BASIC_TYPE(TYPE2CODE(T, base)))
                    TYPECK_ERROR(T, UnexpectedType,
                            .type = pawIr_print_type_v2(T->C, base),
                            .span = segment.span);
                if (IrIsSignature(assoc)) {
                    base = instantiate(T, base, NULL);
                    return pawP_instantiate_assoc(T->C, base, assoc).inst;
                } else { // TODO: need to demonstrate that this branch is hit. add a comment
                    IrType *target = GET_TYPE(T, segment.target);
                    IrGenericArgs *args = lower_generic_args(T, segment.args);
                    target = instantiate(T, target, args);
                    return target;
                }
            } else {
                IrType *target = GET_TYPE(T, segment.target);
                target = instantiate(T, target, NULL);
                IrGenericArgs *args = lower_generic_args(T, segment.args); // TODO: just pass this to instantiate()...
                if (args != NULL) {
                    IrGenericArgs *params = IR_GENERIC_ARGS(target);
                    params = arglist_suffix(T, params, args->count);
                    unify_segment_types(T, segment, params, args);
                }
                return target;
            }
        }
        case HIR_PATH_ASSOC: {
            // The path refers to an associated function or enum variant:
            //     format: Name ["::" Types] "::" Name ["::" Types]
            paw_assert(path.segments->count == 2);
            struct HirSegment const first = K_LIST_FIRST(path.segments);
            struct HirSegment const last = K_LIST_LAST(path.segments);
            IrType *base = lower_adt_segment(T, first);
            pawIr_set_type(T->C, first.id, base);

            IrType *assoc;
            if (last.target.value != INVALID_NODE_ID.value) {
                // the value was located during name resolution, meaning it must be an enum
                // variant or a method/associated function called on a type parameter
                assoc = GET_TYPE(T, last.target);
                if (IrIsGeneric(base)) {
                    paw_assert(first.args == NULL);
                    struct IrSignature const *fn = IrGetSignature(assoc);
                    assoc = instantiate(T, assoc, NULL);
                    IrTrait *bound = get_containing_bound(T->C, base, fn->did);
                    IrType *type_ctx = pawIr_get_context(T->C, assoc);
                    IrTrait *trait_ctx = pawIr_get_trait_context(T->C, assoc);
                    unify_types(T, last.span, type_ctx, base);
                    unify_traits(T, last.span, trait_ctx, bound);
                } else {
                    // path refers to a type constructor
                    assoc = pawP_instantiate_assoc(T->C, base, assoc).inst;
                    if (last.args != NULL)
                        TYPECK_ERROR(T, UnexpectedTypeArguments,
                                .what = SCAN_STR(T->C, "type constructor"),
                                .name = last.ident.name,
                                .span = last.span);
                }
            } else {
                // The value must be an associated function called on an ADT. Such values
                // cannot be found during name resolution (type information is required).
                assoc = lookup_method(T->C, base, last.ident.name);
                if (assoc == NULL)
                    TYPECK_ERROR(T, UnknownAssociatedItem,
                            .type = pawIr_print_type_v2(T->C, base),
                            .item = last.ident.name,
                            .span = last.span);
                IrType *context = pawIr_get_context(T->C, assoc);
                unify_types(T, last.span, context, base);
            }
            if (last.args != NULL) {
                IrGenericArgs *params = IR_GENERIC_ARGS(assoc);
                IrGenericArgs *args = lower_generic_args(T, last.args);
                params = arglist_suffix(T, params, args->count);
                unify_segment_types(T, last, params, args);
            }
            // fill in rest of possibly unfinished path segment
            struct HirDecl const *fn = pawHir_get_decl(T->hir, IR_TYPE_DID(assoc));
            K_LIST_LAST(path.segments).target = fn->hdr.id;
            pawIr_set_type(T->C, last.id, assoc);
            return assoc;
        }
    }
}

static IrType *check_path_expr(struct TypeChecker *T, struct HirPathExpr *e)
{
    IrType *type = lower_value_path(T, e->path);
    if (e->path.kind == HIR_PATH_LOCAL)
        return type;

    if (e->path.segments->count == 1) {
        // TODO: args should be considered local paths to avoid this check
        struct HirDecl *decl = pawHir_get_node(T->hir, K_LIST_LAST(e->path.segments).target);
        if (HirIsParamDecl(decl)) return type;
    }

    if (IrIsGeneric(type)) {
        struct IrGenericDef const *def = pawIr_get_generic_def(T->C, IR_TYPE_DID(type));
        if (def->is_type)
            TYPECK_ERROR(T, ExpectedValue,
                    .type = def->type.name,
                    .span = e->span);
        type = def->konst.type;
    }
    return type;
}

static IrType *check_ascription_expr(struct TypeChecker *T, struct HirAscriptionExpr *e)
{
    IrType *type = check_operand(T, e->expr);
    IrType *tag = check_type(T, e->type, e->type->hdr.span);
    return unify_types(T, e->span, type, tag);
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

static IrType *check_try_expr(struct TypeChecker *T, struct HirTryExpr *e)
{
    IrType *type = check_operand(T, e->target);

    paw_assert(T->rs->prev != NULL);
    IrGenericArg const ret = IrGenericArg_from_type(T->rs->prev);

    if (is_option_t(T, type) || is_result_t(T, type)) {
        IrGenericArg const arg = IrGenericArg_from_type(
                instantiate(T, type, NULL));
        unify_args(T, NODE_SPAN(e->target), arg, ret);
    } else {
        TYPECK_ERROR(T, InvalidChainOperand,
                .type = pawIr_print_type_v2(T->C, type),
                .span = e->span);
    }

    return IrGenericArg_get_type(
            K_LIST_FIRST(IR_GENERIC_ARGS(type))); // unwrap
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
        //     type   = {0, b, c, i, f}
        [UNARY_NEG]   = {0, 0, 0, 1, 1},
        [UNARY_NOT]   = {0, 1, 0, 0, 0},
        [UNARY_BNOT]  = {0, 0, 0, 1, 0},
        [UNARY_DEREF] = {0, 0, 0, 0, 0},
    };

    static char const *UNOP_REPR[] = {
        [UNARY_NEG] = "-",
        [UNARY_NOT] = "!",
        [UNARY_BNOT] = "~",
        [UNARY_DEREF] = "*",
    };

    IrType *type = check_operandx(T, e->target);
    if (IrIsNever(type)) return type;

    if (e->op == UNARY_ADDROF)
        return new_ptr(T, type);
    ensure_valid_rvalue(T, e->target);
    if (e->op == UNARY_DEREF && IrIsPtr(type))
        return ir_deref(type);

    enum BuiltinKind const code = TYPE2CODE(T, type);
    if (!IS_BUILTIN_TYPE(code) || !VALID_OPS[e->op][code]) {
        TYPECK_ERROR(T, InvalidUnaryOperand,
                .type = pawIr_print_type_v2(T->C, type),
                .op = SCAN_STR(T->C, UNOP_REPR[e->op]),
                .span = e->span);
    } else if (is_bool_unop(e->op)) {
        return builtin_type(T, BUILTIN_BOOL);
    } else {
        return type;
    }
}

static IrType *check_binary_op(struct TypeChecker *T, struct SourceSpan span, enum BinaryOp op, IrType *lhs, IrType *rhs)
{
    static uint8_t const VALID_OPS[][NBUILTINS] = {
        //     type     = {0, b, c, i, f, s}
        [BINARY_EQ]     = {0, 1, 1, 1, 1, 1},
        [BINARY_NE]     = {0, 1, 1, 1, 1, 1},
        [BINARY_LT]     = {0, 0, 1, 1, 1, 1},
        [BINARY_LE]     = {0, 0, 1, 1, 1, 1},
        [BINARY_GT]     = {0, 0, 1, 1, 1, 1},
        [BINARY_GE]     = {0, 0, 1, 1, 1, 1},
        [BINARY_ADD]    = {0, 0, 0, 1, 1, 0},
        [BINARY_SUB]    = {0, 0, 0, 1, 1, 0},
        [BINARY_MUL]    = {0, 0, 0, 1, 1, 0},
        [BINARY_DIV]    = {0, 0, 0, 1, 1, 0},
        [BINARY_MOD]    = {0, 0, 0, 1, 1, 0},
        [BINARY_BXOR]   = {0, 0, 0, 1, 0, 0},
        [BINARY_BAND]   = {0, 0, 0, 1, 0, 0},
        [BINARY_BOR]    = {0, 0, 0, 1, 0, 0},
        [BINARY_SHL]    = {0, 0, 0, 1, 0, 0},
        [BINARY_SHR]    = {0, 0, 0, 1, 0, 0},
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

    IrType *type = unify_types(T, span, lhs, rhs);
    enum BuiltinKind const code = TYPE2CODE(T, type);

    if (!IS_BUILTIN_TYPE(code) || !VALID_OPS[op][code]) {
        TYPECK_ERROR(T, InvalidBinaryOperand,
                .type = pawIr_print_type_v2(T->C, type),
                .op = SCAN_STR(T->C, BINOP_REPR[op]),
                .span = span);
    } else if (is_bool_binop(op)) {
        return builtin_type(T, BUILTIN_BOOL);
    } else {
        return type;
    }
}

static IrType *check_binop_expr(struct TypeChecker *T, struct HirBinOpExpr *e)
{
    IrType *lhs = check_operand(T, e->lhs);
    IrType *rhs = check_operand(T, e->rhs);
    return check_binary_op(T, e->span, e->op, lhs, rhs);
}

static IrType *check_assign_expr(struct TypeChecker *T, struct HirAssignExpr *e)
{
    IrType *lhs = check_lvalue(T, e->lhs);
    IrType *rhs = check_operand(T, e->rhs);
    unify_types(T, e->span, lhs, rhs);
    return builtin_type(T, BUILTIN_UNIT);
}

static IrType *check_op_assign_expr(struct TypeChecker *T, struct HirOpAssignExpr *e)
{
    IrType *lhs = check_lvalue(T, e->lhs);
    IrType *rhs = check_operand(T, e->rhs);
    check_binary_op(T, e->span, e->op, lhs, rhs);
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
        unify_types(T, e->span, bs.result, T->bs->result);

    SET_TYPE(T, e->target->hdr.id, ms.target);
    return bs.result;
}

static IrType *check_match_arm(struct TypeChecker *T, struct HirMatchArm *e)
{
    struct BlockState bs;
    enter_block(T, &bs, e->span, BLOCK_NORMAL);

    struct MatchState *ms = T->ms;
    IrType *pat = check_pat(T, e->pat);
    ms->target = unify_types(T, e->span, pat, ms->target);
    if (e->guard != NULL) expect_bool_expr(T, e->guard);
    IrType *result = check_operand(T, e->result);
    result = unify_types(T, e->span, bs.result, result);

    leave_block(T);

    T->bs->result = unify_types(T, e->span, result, T->bs->result);
    return T->bs->result;
}

static void check_closure_param(struct TypeChecker *T, struct HirParamDecl *d)
{
    struct HirDecl const *decl = HIR_CAST_DECL(d);
    IrType *type = check_type(T, d->tag, d->span);
    SET_NODE_TYPE(T->C, decl, type);
}

static IrType *erase_signature_type(struct TypeChecker *T, IrType *type)
{
    if (IrIsSignature(type))
        return IR_GET_FN(T->C, type);
    return type;
}

static IrTypeList *erase_signature_types(struct TypeChecker *T, IrTypeList *types)
{
    IrType **ptype;
    IrTypeList *result = IrTypeList_new(T->C);
    K_LIST_FOREACH (types, ptype) {
        IrType *r = erase_signature_type(T, *ptype);
        IrTypeList_push(T->C, result, r);
    }
    return result;
}

static IrType *check_closure_expr(struct TypeChecker *T, struct HirClosureExpr *e)
{
    struct ResultState rs = {.outer = T->rs};
    T->rs = &rs;

    // steal the enclosing block state to prevent propagation out of the closure
    struct BlockState *outer = T->bs;
    T->bs = NULL;

    struct HirDecl *const *pparam;
    K_LIST_FOREACH (e->params, pparam)
        check_closure_param(T, HirGetParamDecl(*pparam));

    struct BlockState bs;
    enter_block(T, &bs, e->span, BLOCK_NORMAL);

    IrTypeList *params = pawHir_collect_decl_types(T->C, e->params);
    IrType *ret = check_type(T, e->result, e->span);
    params = erase_signature_types(T, params);
    ret = erase_signature_type(T, ret);
    rs.prev = ret;

    IrType *result = check_operand(T, e->expr);
    bs.result = unify_types(T, NODE_SPAN(e->expr), bs.result, result);
    if (!IrIsNever(bs.result))
        unify_types(T, NODE_SPAN(e->expr), bs.result, ret);
    K_LIST_XFOREACH (params, IrType *, p)
        *p = normalize_type(T, *p);
    ret = normalize_type(T, ret);

    leave_block(T);
    T->bs = outer;
    T->rs = rs.outer;

    return pawIr_new_fn_ptr(T->C, params, ret);
}

static IrType *check_projection_expr(struct TypeChecker *T, struct HirProjectionExpr *e)
{
    IrType *type = check_type(T, e->type, NODE_SPAN(e->type));

    struct HirSegment const segment = HirSegments_last(e->trait.segments);
    struct HirDecl *trait_decl = pawHir_get_node(T->hir, segment.target);
    if (!HirIsTraitDecl(trait_decl)) {
        TYPECK_ERROR(T, ExpectedTrait,
                .path = segment.ident.name,
                .span = segment.span);
    }

    IrGenericArgs *args = lower_generic_args(T, segment.args);
    if (args == NULL) args = IrGenericArgs_new(T->C);
    IrGenericArgs_insert(T->C, args, 0, IrGenericArg_from_type(type));
    IrTrait *trait = pawIr_new_trait(T->C, trait_decl->hdr.did, args);

    struct Instantiation const *inst = pawP_find_trait_method(
            T->C, type, trait, e->name);
    if (inst == NULL)
        TYPECK_ERROR(T, UnknownMethod,
                .type = pawIr_print_type_v2(T->C, type),
                .method = e->name,
                .span = e->span);
    return inst->inst;
}

static IrType *lookup_method(struct Compiler *C, IrType *self, Str *name)
{
    struct Instantiation *method = pawP_find_method(C, self, name);
    if (method == NULL) return NULL;
    return method->inst;
}

// TODO: what about arrays???
// Make sure that the target type of the given implementation of the Copy trait
// contains only copyable fields
static void check_copy_trait_impl(struct TypeChecker *T, struct SourceSpan span, struct IrImpl const *def)
{
    if (IrIsAdt(def->type)) {
        struct IrAdtDef const *adt = pawIr_get_adt_def(T->C, IR_TYPE_DID(def->type));
        if (adt->is_struct) {
            IrTypeList *fields = pawP_instantiate_struct_fields(T->C, IrGetAdt(def->type));
            K_LIST_XFOREACH (fields, IrType *const, p) {
                if (!pawIr_is_copyable(T->C, *p))
                    pawErr_generic_error(ENV(T), T->pm->name, span,
                            "struct target of \"Copy\" trait impl is not copyable");
            }
        } else {
            for (int discr = 0; discr < adt->variants->count; ++discr) {
                IrTypeList *fields = pawP_instantiate_variant_fields(T->C, IrGetAdt(def->type), discr);
                K_LIST_XFOREACH (fields, IrType *const, p) {
                    if (!pawIr_is_copyable(T->C, *p))
                        pawErr_generic_error(ENV(T), T->pm->name, span,
                                "enum target of \"Copy\" trait impl is not copyable "
                                "(see variant number %d)", discr);
                }
            }
        }
    } else if (IrIsTuple(def->type)) {
        struct IrTuple const *t = IrGetTuple(def->type);
        K_LIST_XFOREACH (t->elems, IrType *const, p) {
            if (!pawIr_is_copyable(T->C, *p))
                pawErr_generic_error(ENV(T), T->pm->name, span,
                        "tuple target of \"Copy\" trait impl is not copyable");
        }
    }
}

static void check_impl_item(struct TypeChecker *T, struct HirImplDecl *d)
{
    T->self = GET_TYPE(T, d->id);
    IrGenericArgs *params = pawIr_get_generic_args(T->C, d->did);
    pawIr_solver_add_predicates_from(T->C->S, d->did, params);

    struct IrImpl const *def = pawIr_get_impl_def(T->C, d->did);
    if (def->trait != NULL && equals_core_trait(T, def->trait, CORE_TRAIT_COPY))
        check_copy_trait_impl(T, d->span, def);

    struct HirDecl *const *pmethod;
    K_LIST_FOREACH (d->methods, pmethod)
        check_item(T, *pmethod);

    T->self = NULL;
}

static void CheckLetStmt(struct TypeChecker *T, struct HirLetStmt *s)
{
    IrType *tag = check_type(T, s->tag, s->span);
    IrType *rhs = s->init != NULL
        ? check_operand(T, s->init)
        : new_unknown(T, s->span);
    struct BlockState bs;
    struct MatchState ms;
    enter_block(T, &bs, s->span, BLOCK_MATCH);
    enter_match_ctx(T, &ms, tag);
    IrType *lhs = check_pat(T, s->pat);
    unify_types(T, s->span, bs.result, tag);
    leave_match_ctx(T);
    leave_block(T);

    unify_types(T, s->span, lhs, tag);
    unify_types(T, s->span, tag, rhs);

    pawIr_set_type(T->C, s->id, rhs);
}

static void const_check_path(struct HirVisitor *V, struct HirPathExpr *e)
{
    struct TypeChecker *T = V->ud;
    IrType *type = lower_value_path(T, e->path);
    if (!IS_BASIC_TYPE(TYPE2CODE(T, type)))
        TYPECK_ERROR(T, NonprimitiveConstant,
                .type = pawIr_print_type_v2(T->C, type),
                .span = e->span);
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
        case kHirUnOpExpr:
            if (HirGetUnOpExpr(expr)->op == UNARY_ADDROF) {
                name = "address of";
                break;
            }
            return;
        default:
            return;
    }

    struct TypeChecker *T = V->ud;
    TYPECK_ERROR(T, CannotConstantEvaluate,
            .what = SCAN_STR(T->C, name),
            .span = expr->hdr.span);
}

// Make sure the initializer of a global constant can be computed at compile time
static void check_const(struct TypeChecker *T, struct HirExpr *expr, IrType *type)
{
    struct HirVisitor V;
    pawHir_visitor_init(&V, T->hir, T);
    V.PostVisitExpr = const_check_expr;
    pawHir_visit_expr(&V, expr);

    if (!IS_BASIC_TYPE(TYPE2CODE(T, type)))
        TYPECK_ERROR(T, NonprimitiveConstant,
                .type = pawIr_print_type_v2(T->C, type),
                .span = NODE_SPAN(expr));
}

static void check_const_item(struct TypeChecker *T, struct HirConstDecl *d)
{
    IrType *tag = GET_NODE_TYPE(T->C, d->tag);
    if (d->init != NULL) {
        IrType *init = check_operand(T, d->init);
        unify_types(T, d->span, init, tag);
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
    return pawIr_get_def_type(T->C, d->did);
}

static paw_Bool is_self(struct TypeChecker *T, IrType *adt)
{
    if (T->self == NULL) return PAW_FALSE;
    int const position = pawU_current_position(T->U);
    int const status = pawU_unify(T->U, adt, T->self);
    pawU_undo_unifications(T->U, position);
    return status == 0;
}

// TODO: check decl ID of ADTs to determine if type is self
static void ensure_accessible_field(struct TypeChecker *T, struct HirDecl *field, IrType *type, struct SourceSpan name_span)
{
    paw_Bool const is_pub = HirIsFieldDecl(field)
        ? HirGetFieldDecl(field)->is_pub
        : HirGetFnDecl(field)->is_pub;
    if (!is_pub && !is_self(T, type)) {
        int modno = PRELUDE_MODNO;
        if (IrIsAdt(type) || IrIsGeneric(type))
            modno = (int)IR_TYPE_DID(type).modno;
        if (T->pm->modno != modno) {
            struct HirIdent const ident = HirIsFieldDecl(field)
                ? HirGetFieldDecl(field)->ident
                : HirGetFnDecl(field)->ident;
            TYPECK_ERROR(T, AssociatedItemVisibility,
                    .parent_name = pawIr_print_type_v2(T->C, type),
                    .field_name = ident.name,
                    .span = name_span);
        }

    }
}

static IrType *select_field(struct TypeChecker *T, IrType *target, struct HirSelector *e)
{
    target = pawIr_remove_indirection(T->C, target);
    if (IrIsTuple(target)) {
        // tuples are indexed with `Expr "." int_lit`
        IrTypeList *types = IrGetTuple(target)->elems;
        if (!e->is_index)
            TYPECK_ERROR(T, ExpectedElementSelector,
                    .span = e->ident.span);
        if (e->index >= types->count)
            TYPECK_ERROR(T, ElementSelectorOutOfRange,
                    .index = e->index,
                    .count = types->count,
                    .span = e->span);
        return IrTypeList_get(types, e->index);
    }
    if (!IrIsAdt(target))
        TYPECK_ERROR(T, ExpectedAdt,
                .type = pawIr_print_type_v2(T->C, target),
                .span = NODE_SPAN(e->target));
    if (e->is_index)
        TYPECK_ERROR(T, ExpectedFieldSelector,
                .span = e->span);

    struct HirAdtDecl const *adt = get_adt(T, target);
    if (!adt->is_struct)
        TYPECK_ERROR(T, ExpectedStruct,
                .type = pawIr_print_type_v2(T->C, target),
                .span = e->span);


    HirDeclList *fields = pawHir_struct_fields(adt);
    int const index = find_field(fields, e->ident.name);
    if (index < 0)
        TYPECK_ERROR(T, UnknownField,
                .name = e->ident.name,
                .type = adt->ident.name,
                .span = e->ident.span);

    // refer to the field using its index from now on
    struct HirDecl *field = HirDeclList_get(fields, index);
    IrType *result = pawP_instantiate_field(T->C, target,
            GET_NODE_TYPE(T->C, field));
    e->is_index = PAW_TRUE;
    e->index = index;

    ensure_accessible_field(T, field, target, e->ident.span);
    return result;
}

static IrType *check_call_target(struct TypeChecker *T, struct HirExpr *target, int *pparam_offset)
{
    *pparam_offset = 0;
    if (!HirIsSelector(target))
        // normal function call (no receiver)
        return check_expr(T, target);

    struct HirSelector *select = HirGetSelector(target);
    IrType *self = check_operandx(T, select->target);
    IrType *orig = self; //TODO
    self = auto_deref_full(self);

    IrType *method = NULL;
    if (IrIsGeneric(self)) {
        struct IrGeneric *g = IrGetGeneric(self);
        method = pawIr_resolve_trait_method(T->C, g, select->ident.name);
        if (method == NULL)
            TYPECK_ERROR(T, UnknownMethod,
                    .method = select->ident.name,
                    .type = pawIr_print_type_v2(T->C, self),
                    .span = select->ident.span);
    } else if (!select->is_index) {
        IrTypeList *chain = pawIr_autoptr_chain(T->C, orig);
        K_LIST_XFOREACH (chain, IrType *const, p) {
            method = lookup_method(T->C, *p, select->ident.name);
            if (method != NULL) break;
        }
    }

    if (method != NULL) {
        struct IrFnPtr *fn = IrGetFnPtr(IR_SIGNATURE_FN(T->C, method));
        if (!IrIsPtr(IrTypeList_first(fn->params)))
            ensure_valid_rvalue(T, select->target);
    } else {
        return select_field(T, self, select);
    }

    struct HirDecl *fn_decl = get_decl(T, IR_TYPE_DID(method));
    if (HirGetFnDecl(fn_decl)->is_assoc)
        TYPECK_ERROR(T, NotAMethod,
                .name = HirGetFnDecl(fn_decl)->ident.name,
                .span = NODE_SPAN(target));

    ensure_accessible_field(T, fn_decl, self, NODE_SPAN(target));
    *pparam_offset = 1;
    return normalize_type(T, method);
}

// Check a function call or enumerator constructor
static IrType *check_call_expr(struct TypeChecker *T, struct HirCallExpr *e)
{
    int param_offset; // offset of first non-receiver parameter
    IrType *target = check_call_target(T, e->target, &param_offset);
    if (!IR_IS_FUNC_TYPE(target))
        TYPECK_ERROR(T, NotCallable,
                .type = pawIr_print_type_v2(T->C, target),
                .span = NODE_SPAN(e->target));
    SET_NODE_TYPE(T->C, e->target, target);

    struct IrFnPtr const *fn = IrGetFnPtr(IR_GET_FN(T->C, target));
    int const nparams = fn->params->count - param_offset;
    if (e->args->count != nparams)
        TYPECK_ERROR(T, IncorrectArity,
                .have = e->args->count,
                .want = nparams,
                .span = e->span);

    if (is_unit_variant(T, target))
        TYPECK_ERROR(T, UnitVariantWithParenthesis,
                .type = pawIr_print_type_v2(T->C, fn->result),
                .span = NODE_SPAN(e->target));

    int index;
    struct HirExpr *const *parg;
    K_LIST_ENUMERATE (e->args, index, parg) {
        IrType *param = IrTypeList_get(fn->params, index + param_offset);
        IrType *arg = check_operand(T, *parg);
        unify_types(T, NODE_SPAN(*parg), param, arg);
    }

    if (IrIsNever(fn->result)) // function never returns
        unify_never_type(T, e->span, T->bs->result);

    return fn->result;
}

static IrType *check_conversion_expr(struct TypeChecker *T, struct HirConversionExpr *e)
{
    static int const ALLOWED_CASTS[NBUILTINS][NBUILTINS] = {
        //          to  = {0, b, c, i, f}
        [BUILTIN_BOOL]  = {0, 1, 1, 1, 1},
        [BUILTIN_CHAR]  = {0, 1, 1, 1, 0},
        [BUILTIN_INT]   = {0, 1, 1, 1, 1},
        [BUILTIN_FLOAT] = {0, 1, 0, 1, 1},

        // allow conversions between pointers and integers
        [BUILTIN_INT][BUILTIN_PTR] = 1,
        [BUILTIN_PTR][BUILTIN_INT] = 1,
    };

    IrType *from_type = check_operand(T, e->from);
    IrType *to_type = lower_type(T, e->to);
    if (IrIsPtr(from_type) && IrIsPtr(to_type)) {
        if (!IrIsChar(ir_deref(from_type))
                && !IrIsChar(ir_deref(to_type)))
            pawErr_generic_error(ENV(T), T->pm->name, e->span,
                    "given a pointer-to-pointer cast \"*T as *T2\", "
                    "either \"T\" or \"T2\" must have type \"char\"");
        return to_type;
    }

    enum BuiltinKind const from = TYPE2CODE(T, from_type);
    enum BuiltinKind const to = TYPE2CODE(T, to_type);
    if (!IS_BUILTIN_TYPE(from)
            || !IS_BUILTIN_TYPE(to)
            || !ALLOWED_CASTS[from][to])
        TYPECK_ERROR(T, IncompatibleTypes,
                .lhs = pawIr_print_type_v2(T->C, from_type),
                .rhs = pawIr_print_type_v2(T->C, to_type),
                .span = e->span);

    return to_type;
}

static IrType *check_basic_lit(struct TypeChecker *T, struct HirBasicLit *e)
{
    switch (e->code) {
        case BUILTIN_UNIT:
            return pawIr_new_unit(T->C);
        case BUILTIN_BOOL:
            return pawIr_new_bool(T->C);
        case BUILTIN_CHAR:
            return pawIr_new_char(T->C);
        case BUILTIN_INT:
            return pawIr_new_int(T->C);
        case BUILTIN_FLOAT:
            return pawIr_new_float(T->C);
        default:
            paw_assert(e->code == BUILTIN_STR);
            return pawIr_new_string(T->C);
    }
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
    elems = erase_signature_types(T, elems);
    return pawIr_new_tuple(T->C, elems);
}

static IrType *check_array_lit(struct TypeChecker *T, struct HirArrayLit *e, struct SourceSpan span)
{
    IrType *elem = new_unknown(T, span);
    K_LIST_XFOREACH (e->elems, struct HirExpr *const, p) {
        IrType *current = check_operand(T, *p);
        unify_types(T, NODE_SPAN(*p), elem, current);
    }
    elem = erase_signature_type(T, elem);
    union IrValue const value = {.i = e->elems->count};
    IrConst *length = pawIr_new_const_value(T->C, value, pawIr_new_int(T->C));
    return pawIr_new_array(T->C, elem, length);
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
        struct HirIdent const ident = HirGetFieldExpr(*pexpr)->ident;
        FieldMap_insert_unique(T, map, ident, index);
        HirExprList_push(T->hir, order, *pexpr);
    }
    return order;
}

static IrTypeList *substitute_types(struct TypeChecker *T, IrGenericArgs *before, IrGenericArgs *after, IrTypeList *target)
{
    if (before == NULL) return target;
    paw_assert(before->count == after->count);

    struct IrTypeFolder F;
    struct Substitution subst;
    pawP_init_substitution_folder(&F, T->C, &subst, before, after);
    return pawIr_fold_type_list(&F, target);
}

static IrType *check_composite_lit(struct TypeChecker *T, struct HirCompositeLit *e, struct SourceSpan span)
{
    IrType *type = lower_type_path(T, e->path);
    if (!IrIsAdt(type))
        TYPECK_ERROR(T, ExpectedStruct,
                .type = pawIr_print_type_v2(T->C, type),
                .span = span);
    struct HirDecl *decl = get_decl(T, IR_TYPE_DID(type));

    // Use a temporary Map to avoid searching repeatedly through the list of fields.
    FieldMap *map = FieldMap_new(T);

    struct HirAdtDecl *adt = HirGetAdtDecl(decl);
    if (!adt->is_struct)
        TYPECK_ERROR(T, ExpectedStruct,
                .type = adt->ident.name,
                .span = adt->ident.span);

    HirDeclList *fields = pawHir_struct_fields(adt);
    IrTypeList *field_types = pawHir_collect_decl_types(T->C, fields);
    if (fields->count == 0)
        TYPECK_ERROR(T, UnitStructWithBraces,
                .type = adt->ident.name,
                .span = adt->ident.span);

    IrType *base_type = GET_TYPE(T, adt->id);
    field_types = substitute_types(T, IR_GENERIC_ARGS(base_type), IR_GENERIC_ARGS(type), field_types);
    HirExprList *order = collect_field_exprs(T, e->items, map, adt->ident.name);

    HirExprList *items = HirExprList_new(T->hir);
    HirExprList_resize(T->hir, items, order->count);

    int index = 0;
    IrType *const *ptype;
    struct HirDecl *const *pdecl;
    K_LIST_ZIP (fields, pdecl, field_types, ptype) {
        struct HirFieldDecl const *field = HirGetFieldDecl(*pdecl);
        ensure_accessible_field(T, *pdecl, type, e->path.span);
        int const *pindex = FieldMap_get(T, map, field->ident);
        if (pindex == NULL)
            TYPECK_ERROR(T, MissingField,
                    .name = field->ident.name,
                    .type = adt->ident.name,
                    .span = span);

        struct HirExpr *item = HirExprList_get(order, *pindex);
        unify_types(T, item->hdr.span, *ptype, check_operand(T, item));
        HirExprList_set(items, index, item);
        FieldMap_remove(T, map, field->ident);
        HirGetFieldExpr(item)->fid = index++;
    }
    FieldMapIterator iter;
    FieldMapIterator_init(map, &iter);
    if (FieldMapIterator_is_valid(&iter))
        TYPECK_ERROR(T, UnknownField,
                   .name = FieldMapIterator_key(&iter).name,
                   .type = adt->ident.name,
                   .span = span);
    paw_assert(fields->count == e->items->count);
    HirExprList_delete(T->hir, order);
    FieldMap_delete(T, map);

    e->items = items;
    return type;
}

static IrType *check_literal_expr(struct TypeChecker *T, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitBasic:
            return check_basic_lit(T, &e->basic);
        case kHirLitTuple:
            return check_tuple_lit(T, &e->tuple);
        case kHirLitComposite:
            return check_composite_lit(T, &e->comp, e->span);
        case kHirLitArray:
            return check_array_lit(T, &e->array, e->span);
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

    IrType *result = check_operand(T, e->block);
    unify_unit_type(T, e->span, result);

    leave_block(T);

    // loops that have no local or nonlocal jumps never complete
    return unify_never_type(T, e->span, bs.result);
}

static IrType *check_array_index(struct TypeChecker *T, IrType *array, IrType *index)
{
    unify_types(T, TODO, index, pawIr_new_int(T->C));
    return IrGetArray(array)->type;
}

static IrType *find_index_fn_aux(struct TypeChecker *T, IrType *target, IrType *index)
{
    // create trait descriptor for "Index<Target, Idx>"
    IrGenericArgs *args = IrGenericArgs_new(T->C);
    DeclId const did = T->C->core_traits[CORE_TRAIT_INDEX];
    IrTrait *trait = pawIr_new_trait(T->C, did, args);
    IrGenericArgs_reserve(T->C, args, 3);
    IrGenericArgs_push(T->C, args, IrGenericArg_from_type(target));
    IrGenericArgs_push(T->C, args, IrGenericArg_from_type(index));

    struct Instantiation const *inst = pawP_find_trait_method(T->C,
            target, trait, SCAN_STR(T->C, "index"));

    if (inst != NULL) {
        struct IrType2 const type2 = {target, index};
        IrType2Map_insert(T->C, T->C->indexes, type2, inst->inst);
        return inst->inst;
    }
    return NULL;

}

static IrType *find_index_fn(struct TypeChecker *T, IrType *target, IrType *index)
{
    return find_index_fn_aux(T, target, index);
}

static IrType *check_index(struct TypeChecker *T, struct HirIndex *e)
{
    IrType *target = check_operandx(T, e->target);
    IrType *index = check_operand(T, e->index);
    target = auto_deref_full(target);

    if (IrIsArray(target))
        return check_array_index(T, target, index);

    // Determine the concrete type of the "Index::index" method that will
    // be used to represent this indexing operation.
    IrType *fn_type = find_index_fn(T, target, index);
    if (fn_type == NULL)
        pawErr_generic_error(ENV(T), T->pm->name, e->span,
                "type cannot be indexed");

    struct IrFnPtr const *fn = IrGetFnPtr(pawIr_materialize_fn(T->C,
                IR_TYPE_DID(fn_type), IR_GENERIC_ARGS(fn_type)));
    IrType *fn_target = IrTypeList_get(fn->params, 0);
    IrType *fn_index = IrTypeList_get(fn->params, 1);

    unify_types(T, e->span, ir_deref(fn_target), target);
    unify_types(T, e->span, fn_index, index);
    return ir_deref(fn->result);
}

static IrType *check_selector(struct TypeChecker *T, struct HirSelector *e)
{
    IrType *target = check_operandx(T, e->target);
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
    struct PatState const *ps = T->ms->ps;
    while (ps->outer != NULL) {
        if (ps->outer->kind == kHirOrPat)
            break;
        ps = ps->outer;
    }
    void *const *pname = StringMap_get(T->C, ps->bound, ident.name);
    if (pname != NULL)
        TYPECK_ERROR(T, DuplicateBinding,
                .name = ident.name,
                .span = ident.span);
    StringMap_insert(T->C, ps->bound, ident.name, ident.name);
}

static void locate_binding(struct HirVisitor *V, struct HirBindingPat *p)
{
    struct BindingChecker const *bc = V->ud;
    // all bindings must be specified in the first alternative
    IrType *type = GET_TYPE(bc->T, p->id);
    BindingMap_insert(bc->T, bc->bound, p->ident,
            (struct BindingInfo){
                .type = type,
            });
}

static void check_binding(struct HirVisitor *V, struct HirBindingPat *p)
{
    struct BindingChecker *bc = V->ud;
    struct BindingInfo *pbi = BindingMap_get(bc->T, bc->bound, p->ident);
    if (pbi == NULL)
        TYPECK_ERROR(bc->T, MissingBindingInAlternative,
                .name = p->ident.name,
                .span = p->span);

    IrType *type = GET_TYPE(bc->T, p->id);
    unify_types(bc->T, p->span, pbi->type, type);
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
            TYPECK_ERROR(bc->T, MissingBindingInAlternative,
                    .name = key.name,
                    .span = key.span);
        } else if (bi.uses > bc->iter) {
            struct HirIdent const key = BindingMapIterator_key(&iter);
            TYPECK_ERROR(bc->T, DuplicateBinding,
                    .name = key.name,
                    .span = key.span);
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
        unify_types(T, p->span, type, check_pat(T, next));

        pawHir_visit_pat(&V, next);
        ensure_all_bindings_created(&bc);
    }
    uninit_binding_checker(&bc);
    return type;
}

static IrType *CheckRefPat(struct TypeChecker *T, struct HirRefPat *p)
{
    return new_ptr(T, check_pat(T, p->referent));
}

static IrType *CheckPtrPat(struct TypeChecker *T, struct HirPtrPat *p)
{
    return new_ptr(T, check_pat(T, p->pointee));
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

    K_LIST_XFOREACH (p->fields, struct HirPat *const, pfield) {
        check_pat(T, *pfield);
        struct HirIdent const ident = HirGetFieldPat(*pfield)->ident;
        PatFieldMap_insert(T, map, ident, *pfield);
    }

    struct HirPatList *sorted = HirPatList_new(T->hir);
    HirPatList_reserve(T->hir, sorted, adt_fields->count);

    int index = 0;
    IrType *const *ptype;
    struct HirDecl *const *pdecl;
    K_LIST_ZIP (v->fields, pdecl, adt_fields, ptype) {
        struct HirFieldDecl const *field = HirGetFieldDecl(*pdecl);
        struct HirPat *const *ppat = PatFieldMap_get(T, map, field->ident);
        if (ppat == NULL)
            TYPECK_ERROR(T, MissingField,
                    .name = field->ident.name,
                    .type = v->ident.name,
                    .span = v->ident.span);
        struct HirFieldPat *field_pat = HirGetFieldPat(*ppat);
        // TODO: source span should be inside composite lit, not struct def
        unify_types(T, field->span, GET_TYPE(T, field_pat->id), *ptype);
        HirPatList_push(T->hir, sorted, *ppat);
        PatFieldMap_remove(T, map, field->ident);
        field_pat->index = index;
        ++index;
    }
    p->fields = sorted;

    if (PatFieldMap_length(map) > 0) {
        PatFieldMapIterator iter;
        // use an iterator to find one of the unknown fields
        PatFieldMapIterator_init(map, &iter);
        struct HirIdent const ident = PatFieldMapIterator_key(&iter);
        TYPECK_ERROR(T, UnknownField,
                .type = v->ident.name,
                .name = ident.name,
                .span = ident.span);
    }
    PatFieldMap_delete(T, map);
    return type;
}

static IrType *CheckVariantPat(struct TypeChecker *T, struct HirVariantPat *p)
{
    IrType *type = lower_value_path(T, p->path);
    if (IrIsAdt(type)) return type; // unit structure

    struct IrFnPtr const *fn = IrGetFnPtr(IR_GET_FN(T->C, type));
    if (p->fields->count != fn->params->count)
        TYPECK_ERROR(T, IncorrectArity,
                .have = p->fields->count,
                .want = fn->params->count,
                .span = p->span);

    struct HirPat *const *ppat;
    IrType *const *pparam;
    K_LIST_ZIP (fn->params, pparam, p->fields, ppat) {
        IrType *const arg = check_pat(T, *ppat);
        unify_types(T, NODE_SPAN(*ppat), *pparam, arg);
    }

    struct IrVariantDef const *def = pawIr_get_variant_def(T->C, IR_TYPE_DID(type));
    p->index = def->discr;
    return fn->result;
}

static IrType *CheckTuplePat(struct TypeChecker *T, struct HirTuplePat *p)
{
    IrTypeList *elems = check_pat_list(T, p->elems);
    if (elems->count == 0)
        return builtin_type(T, BUILTIN_UNIT);
    elems = erase_signature_types(T, elems);
    return pawIr_new_tuple(T->C, elems);
}

static IrType *CheckBindingPat(struct TypeChecker *T, struct HirBindingPat *p)
{
    // binding type is determined using unification
    IrType *type = new_unknown(T, p->span);

    // make sure bindings are unique within each pattern (OR patterns are special-cased)
    account_for_binding(T, p->ident);
    return type;
}

static IrType *CheckWildcardPat(struct TypeChecker *T, struct HirWildcardPat *p)
{
    return new_unknown(T, p->span);
}

static IrType *CheckLiteralPat(struct TypeChecker *T, struct HirLiteralPat *p)
{
    return check_operand(T, p->expr);
}

static IrType *check_const_decl(struct TypeChecker *T, struct HirConstDecl *d)
{
    IrType *tag = check_type(T, d->tag, d->tag->hdr.span);
    IrType *init = check_operand(T, d->init);
    unify_types(T, d->span, init, tag);
    check_const(T, d->init, tag);
    pawIr_set_type(T->C, d->id, tag);
    return tag;
}

static void check_decl(struct TypeChecker *T, struct HirDecl *decl)
{
    IrType *type;
    switch (HIR_KINDOF(decl)) {
        case kHirFieldDecl:
            type = check_field_decl(T, HirGetFieldDecl(decl));
            break;
        case kHirConstDecl:
            type = check_const_decl(T, HirGetConstDecl(decl));
            break;
        default: // kHirTypeDecl
            type = check_type_decl(T, HirGetTypeDecl(decl));
    }
    type = normalize_type(T, type);
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

    type = normalize_type(T, type);
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

    solve_pending_obligations(T);
}

// NOTE: Some expressions are known to directly represent types, based on the context
//       (type annotations, type arguments, etc.). Call check_type() to convert such
//       an expression into an IR type.

static IrType *check_type(struct TypeChecker *T, struct HirType *type, struct SourceSpan span)
{
    if (type != NULL) return lower_type(T, type);
    return new_unknown(T, span);
}

static void unconditional_return(struct TypeChecker *T, struct SourceSpan span)
{
    struct BlockState *bs = T->bs;
    do {
         if (bs->outer == NULL) break;
         unify_never_type(T, span, bs->result);
         bs = bs->outer;
    } while (bs->kind != BLOCK_MATCH);
}

static IrType *check_return_expr(struct TypeChecker *T, struct HirReturnExpr *e)
{
    unconditional_return(T, e->span);

    IrType *want = T->rs->prev;
    IrType *have = e->expr != NULL
        ? check_operand(T, e->expr)
        : builtin_type(T, BUILTIN_UNIT);
    unify_types(T, e->span, have, want);
    ++T->rs->count;

    return pawIr_new_never(T->C);
}

static struct BlockState *unconditional_jump(struct TypeChecker *T, struct SourceSpan span)
{
    struct BlockState *bs = T->bs;
    while (bs->outer != NULL && bs->kind == BLOCK_NORMAL) {
         unify_never_type(T, span, bs->result);
         bs = bs->outer;
    }
    return bs;
}

static IrType *check_jump_expr(struct TypeChecker *T, struct HirJumpExpr *e)
{
    struct BlockState *bs = unconditional_jump(T, e->span);
    if (e->jump_kind == JUMP_BREAK) {
        // "break" leaves the enclosing loop, causing it to evaluate to "()"
        while (bs->kind != BLOCK_LOOP) bs = bs->outer;
        unify_unit_type(T, e->span, bs->result);
    }
    return pawIr_new_never(T->C);
}

// TODO: most exprs handled in this function cannot appear on the LHS of an assignment. move those cases into check_expr, and don't call this function from check_expr
static IrType *check_lvalue(struct TypeChecker *T, struct HirExpr *expr)
{
    IrType *type;
    switch (HIR_KINDOF(expr)) {
        case kHirAscriptionExpr:
            type = check_ascription_expr(T, HirGetAscriptionExpr(expr));
            break;
        case kHirPathExpr:
            type = check_path_expr(T, HirGetPathExpr(expr));
            break;
        case kHirIndex:
            type = check_index(T, HirGetIndex(expr));
            break;
        case kHirSelector:
            type = check_selector(T, HirGetSelector(expr));
            break;
        case kHirUnOpExpr:
            if (HIR_IS_UNOP(expr, UNARY_DEREF)) {
                type = check_unop_expr(T, HirGetUnOpExpr(expr));
                break;
            }
            // (fallthrough)
        default:
            pawErr_generic_error(ENV(T), T->pm->name, NODE_SPAN(expr), "invalid lvalue expression");
    }

    type = normalize_type(T, type);
    SET_NODE_TYPE(T->C, expr, type);
    solve_pending_obligations(T);
    return type;
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
        case kHirTryExpr:
            type = check_try_expr(T, HirGetTryExpr(expr));
            break;
        case kHirUnOpExpr: {
            type = check_unop_expr(T, HirGetUnOpExpr(expr));
            break;
        }
        case kHirBinOpExpr:
            type = check_binop_expr(T, HirGetBinOpExpr(expr));
            break;
        case kHirClosureExpr:
            type = check_closure_expr(T, HirGetClosureExpr(expr));
            break;
        case kHirProjectionExpr:
            type = check_projection_expr(T, HirGetProjectionExpr(expr));
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

    type = normalize_type(T, type);
    SET_NODE_TYPE(T->C, expr, type);
    solve_pending_obligations(T);
    return type;
}


static void check_item(struct TypeChecker *T, struct HirDecl *item)
{
    // create a new solver to hold the item's predicates
    pawIr_push_solver(T->C);

    if (HirIsFnDecl(item)) {
        check_fn_item(T, HirGetFnDecl(item));
    } else if (HirIsImplDecl(item)) {
        check_impl_item(T, HirGetImplDecl(item));
    } else if (HirIsConstDecl(item)) {
        check_const_item(T, HirGetConstDecl(item));
    }

    struct IrSolverResult const result = solve_pending_obligations(T);
    if (result.status == IR_SOLVER_AMBIGUOUS) {
        struct IrObligation const example = pawIr_solver_first_obligation(T->C->S);
        TYPECK_ERROR(T, UnsatisfiedObligation,
                .example = pawIr_print_obligation_(T->C, example),
                .num_unsolved = result.ambiguous.num_unsolved);
    }

    pawIr_pop_solver(T->C);
}

static void check_items(struct TypeChecker *T, struct HirDeclList *items)
{
    struct HirDecl *const *pitem;
    K_LIST_FOREACH (items, pitem)
        check_item(T, *pitem);
}

static IrType *call_normalize_type(struct IrTypeFolder *F, IrType *type)
{
    struct HirTypeFolder const *outer = F->ud;
    return normalize_type(outer->ud, type);
}

static void use_module(struct TypeChecker *T, struct HirModule const *pm)
{
    T->pm = pm;
}

static void check_module_types(struct TypeChecker *T, struct HirModule m)
{
    DLOG(T, "resolving '%s'", m->name->text);

    check_items(T, m.items);

    struct HirTypeFolder F;
    pawHir_type_folder_init(&F, T->hir, T);
    F.F.FoldType = call_normalize_type;

    pawHir_fold_decl_types(&F, m.items);
}

static void check_constant_types(struct TypeChecker *T)
{
    enter_inference_ctx(T);

    IrPendingConstantsIterator iter;
    IrPendingConstantsIterator_init(T->C->pending_constants, &iter);
    while (IrPendingConstantsIterator_is_valid(&iter)) {
        IrPendingConstantsIterator_key(&iter);
        struct IrPendingConstant *p = IrPendingConstantsIterator_valuep(&iter);

        IrType *type = check_operand(T, p->payload);
        check_const(T, p->payload, type);

        IrPendingConstantsIterator_next(&iter);
    }

    leave_inference_ctx(T);
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

    check_constant_types(T);
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

    void pawU_run_unit_tests(struct Unifier *U);
    pawU_run_unit_tests(C->U);

    // run the type checker
    check_types(&T);

    pawP_pool_free(C, pool);
}

