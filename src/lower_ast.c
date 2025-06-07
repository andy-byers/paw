// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ast.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "env.h"
#include "error.h"
#include "gc.h"
#include "hir.h"
#include "map.h"
#include "mem.h"
#include "opcode.h"
#include "parse.h"
#include "str.h"
#include "unify.h"

#define LOWERING_ERROR(L_, Kind_, ...) pawErr_##Kind_((L_)->C, (L_)->hir->name, __VA_ARGS__)

struct LowerAst {
    Str const *modname;
    struct DynamicMem *dm;
    struct Compiler *C;
    struct Hir *hir;
    paw_Env *P;
};

DEFINE_MAP_ITERATOR(ImportMap, Str *, struct Ast *)

static struct HirStmt *lower_stmt(struct LowerAst *, struct AstStmt *);
static struct HirExpr *lower_expr(struct LowerAst *, struct AstExpr *);
static struct HirDecl *lower_decl(struct LowerAst *, struct AstDecl *);
static struct HirType *lower_type(struct LowerAst *, struct AstType *);
static struct HirPat *lower_pat(struct LowerAst *, struct AstPat *);

#define DEFINE_LOWER_LIST(name, T)                                                                 \
    static struct Hir##T##List *lower_##name##_list(struct LowerAst *L, struct Ast##T##List *list) \
    {                                                                                              \
        if (list == NULL)                                                                          \
            return NULL;                                                                           \
        struct Hir##T##List *r = Hir##T##List_new(L->hir);                                         \
        for (int i = 0; i < list->count; ++i) {                                                    \
            struct Hir##T *node = lower_##name(L, list->data[i]);                                  \
            if (node != NULL)                                                                      \
                Hir##T##List_push(L->hir, r, node);                                                \
        }                                                                                          \
        return r;                                                                                  \
    }
DEFINE_LOWER_LIST(expr, Expr)
DEFINE_LOWER_LIST(decl, Decl)
DEFINE_LOWER_LIST(stmt, Stmt)
DEFINE_LOWER_LIST(type, Type)
DEFINE_LOWER_LIST(pat, Pat)

static struct HirIdent make_ident(Str *name, struct SourceSpan span)
{
    return (struct HirIdent){
        .name = name,
        .span = span,
    };
}

static struct HirIdent lower_ident(struct LowerAst *L, struct AstIdent ident)
{
    return make_ident(ident.name, ident.span);
}

static struct HirPath new_unary_path(struct LowerAst *L, struct HirIdent ident)
{
    struct HirPath path;
    pawHir_path_init(L->hir, &path, ident.span);
    pawHir_path_add(L->hir, &path, ident, NULL);
    return path;
}

static struct HirExpr *new_unary_path_expr(struct LowerAst *L, struct HirIdent ident)
{
    struct HirPath path = new_unary_path(L, ident);
    return pawHir_new_path_expr(L->hir, ident.span, path);
}

static struct HirExpr *lower_block(struct LowerAst *L, struct AstBlock *e)
{
    struct HirStmtList *stmts = lower_stmt_list(L, e->stmts);
    struct HirExpr *result = e->result != NULL ? lower_expr(L, e->result) : NULL;
    return pawHir_new_block(L->hir, e->span, stmts, result);
}

static struct HirExpr *LowerBlock(struct LowerAst *L, struct AstBlock *block)
{
    return lower_block(L, block);
}

static struct HirDeclList *lower_params(struct LowerAst *L, struct AstFuncDecl *d, struct AstDeclList *params)
{
    struct HirDeclList *out = HirDeclList_new(L->hir);
    for (int i = 0; i < params->count; ++i) {
        struct AstDecl *ast_param = AstDeclList_get(params, i);
        struct HirDecl *hir_param = lower_decl(L, ast_param);
        HirDeclList_push(L->hir, out, hir_param);
    }
    return out;
}

static struct HirDecl *LowerFieldDecl(struct LowerAst *L, struct AstFieldDecl *d)
{
    struct HirType *tag = lower_type(L, d->tag);
    struct HirIdent ident = lower_ident(L, d->ident);
    return pawHir_new_field_decl(L->hir, d->span, ident, tag, d->is_pub);
}

static struct HirDecl *LowerVariantDecl(struct LowerAst *L, struct AstVariantDecl *d)
{
    struct HirIdent ident = lower_ident(L, d->ident);
    struct HirDeclList *fields = lower_decl_list(L, d->fields);
    return pawHir_new_variant_decl(L->hir, d->span, ident, fields, d->index);
}

static struct HirDeclList *lower_fields(struct LowerAst *L, struct AstDeclList *src)
{
    struct HirDeclList *dst = HirDeclList_new(L->hir);
    for (int i = 0; i < src->count; ++i) {
        struct HirDecl *decl = lower_decl(L, AstDeclList_get(src, i));
        HirDeclList_push(L->hir, dst, decl);
    }
    return dst;
}

static struct HirExpr *LowerReturnExpr(struct LowerAst *L, struct AstReturnExpr *e)
{
    struct HirExpr *expr = e->expr != NULL ? lower_expr(L, e->expr) : NULL;
    return pawHir_new_return_expr(L->hir, e->span, expr);
}

static struct HirDecl *LowerFuncDecl(struct LowerAst *L, struct AstFuncDecl *d);

static struct HirDeclList *lower_methods(struct LowerAst *L, struct AstDeclList *src)
{
    paw_Env *P = ENV(L);
    struct HirDeclList *dst = HirDeclList_new(L->hir);
    for (int i = 0; i < src->count; ++i) {
        struct AstDecl *decl = AstDeclList_get(src, i);
        struct AstFuncDecl *d = AstGetFuncDecl(decl);
        struct HirDecl *result = LowerFuncDecl(L, d);
        struct HirFuncDecl *r = HirGetFuncDecl(result);
        HirDeclList_push(L->hir, dst, result);
    }
    return dst;
}

static struct HirPath lower_path(struct LowerAst *L, struct AstPath path)
{
    struct HirPath r;
    pawHir_path_init(L->hir, &r, path.span);
    paw_assert(path.segments->count > 0);
    struct AstSegment *psrc;
    K_LIST_FOREACH(path.segments, psrc) {
        struct HirTypeList *types = lower_type_list(L, psrc->types);

        struct HirSegment dst;
        pawHir_init_segment(L->hir, &dst, lower_ident(L, psrc->ident), types);
        HirSegments_push(L->hir, r.segments, dst);
    }
    return r;
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct HirExpr *LowerPathExpr(struct LowerAst *L, struct AstPathExpr *e)
{
    struct HirPath path = lower_path(L, e->path);
    return pawHir_new_path_expr(L->hir, e->span, path);
}

static struct HirExpr *LowerLogicalExpr(struct LowerAst *L, struct AstLogicalExpr *e)
{
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return pawHir_new_logical_expr(L->hir, e->span, lhs, rhs, e->is_and);
}

static struct HirExpr *LowerChainExpr(struct LowerAst *L, struct AstChainExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    return pawHir_new_chain_expr(L->hir, e->span, target);
}

static struct HirExpr *LowerUnOpExpr(struct LowerAst *L, struct AstUnOpExpr *e)
{
    if (e->op == UNARY_NEG && AstIsLiteralExpr(e->target)) {
        struct AstLiteralExpr *lit = AstGetLiteralExpr(e->target);
        if (lit->lit_kind == kAstBasicLit && lit->basic.code == BUILTIN_INT) {
            paw_Uint const u = V_UINT(lit->basic.value);
            if (u > (paw_Uint)PAW_INT_MAX + 1)
                LOWERING_ERROR(L, integer_out_of_range, e->span.start, u);

            paw_Int const i = u < (paw_Uint)PAW_INT_MAX + 1 ? -(paw_Int)u : PAW_INT_MIN;
            return pawHir_new_basic_lit(L->hir, e->span, I2V(i), BUILTIN_INT);
        }
    }
    struct HirExpr *target = lower_expr(L, e->target);
    return pawHir_new_unop_expr(L->hir, e->span, target, e->op);
}

static struct HirExpr *LowerBinOpExpr(struct LowerAst *L, struct AstBinOpExpr *e)
{
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return pawHir_new_binop_expr(L->hir, e->span, lhs, rhs, e->op);
}

static struct HirExpr *new_literal_field(struct LowerAst *L, const char *name, struct HirExpr *expr, int fid)
{
    struct HirIdent const ident = {
        .name = SCAN_STR(L->C, name),
        .span = expr->hdr.span,
    };
    return pawHir_new_named_field_expr(L->hir, expr->hdr.span, ident, expr, fid);
}

static struct HirPath new_cstr_path(struct LowerAst *L, struct SourceSpan span, unsigned kind)
{
    return new_unary_path(L, (struct HirIdent){
                .name = CSTR(L, kind),
                .span = span,
            });
}

static struct HirExpr *into_range(struct LowerAst *L, struct AstRangeExpr *e)
{
    struct HirPath path = new_cstr_path(L, e->span,
            e->is_inclusive ? CSTR_RANGE_INCLUSIVE : CSTR_RANGE);
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    struct HirExprList *items = HirExprList_new(L->hir);
    HirExprList_push(L->hir, items, new_literal_field(L, "start", lhs, 0));
    HirExprList_push(L->hir, items, new_literal_field(L, "end", rhs, 1));
    return pawHir_new_composite_lit(L->hir, e->span, path, items);
}

static struct HirExpr *into_range_full(struct LowerAst *L, struct AstRangeExpr *e)
{
    if (e->is_inclusive)
        LOWERING_ERROR(L, invalid_inclusive_range, e->span.start);

    struct HirPath path = new_cstr_path(L, e->span, CSTR_RANGE_FULL);
    return pawHir_new_path_expr(L->hir, e->span, path);
}

static struct HirExpr *into_range_to(struct LowerAst *L, struct AstRangeExpr *e)
{
    struct HirPath path = new_cstr_path(L, e->span,
            e->is_inclusive ? CSTR_RANGE_TO_INCLUSIVE : CSTR_RANGE_TO);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    struct HirExprList *items = HirExprList_new(L->hir);
    HirExprList_push(L->hir, items, new_literal_field(L, "end", rhs, 1));
    return pawHir_new_composite_lit(L->hir, e->span, path, items);
}

static struct HirExpr *into_range_from(struct LowerAst *L, struct AstRangeExpr *e)
{
    if (e->is_inclusive)
        LOWERING_ERROR(L, invalid_inclusive_range, e->span.start);

    struct HirPath path = new_cstr_path(L, e->span, CSTR_RANGE_FROM);
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExprList *items = HirExprList_new(L->hir);
    HirExprList_push(L->hir, items, new_literal_field(L, "start", lhs, 0));
    return pawHir_new_composite_lit(L->hir, e->span, path, items);
}

static struct HirExpr *LowerRangeExpr(struct LowerAst *L, struct AstRangeExpr *e)
{
    if (e->lhs == NULL && e->rhs == NULL) {
        return into_range_full(L, e);
    } else if (e->lhs == NULL) {
        return into_range_to(L, e);
    } else if (e->rhs == NULL) {
        return into_range_from(L, e);
    } else {
        return into_range(L, e);
    }
}

static void check_assignment_target(struct LowerAst *L, struct AstExpr *target)
{
    if (!AstIsPathExpr(target) && !AstIsIndex(target) && !AstIsSelector(target))
        pawErr_invalid_assignment_target(L->C, L->modname, target->hdr.span.start);
}

static struct HirExpr *LowerAssignExpr(struct LowerAst *L, struct AstAssignExpr *e)
{
    check_assignment_target(L, e->lhs);
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return pawHir_new_assign_expr(L->hir, e->span, lhs, rhs);
}

static struct HirExpr *LowerOpAssignExpr(struct LowerAst *L, struct AstOpAssignExpr *e)
{
    check_assignment_target(L, e->lhs);
    struct HirExpr *lhs = lower_expr(L, e->lhs); // used as place
    struct HirExpr *copy = lower_expr(L, e->lhs); // used as value
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    rhs = pawHir_new_binop_expr(L->hir, e->span, copy, rhs, e->op);
    return pawHir_new_assign_expr(L->hir, e->span, lhs, rhs);
}

static struct HirExpr *LowerMatchExpr(struct LowerAst *L, struct AstMatchExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExprList *arms = lower_expr_list(L, e->arms);
    paw_assert(arms->count > 0);

    return pawHir_new_match_expr(L->hir, e->span, target, arms, PAW_TRUE);
}

static struct HirExpr *LowerMatchArm(struct LowerAst *L, struct AstMatchArm *e)
{
    struct HirPat *pat = lower_pat(L, e->pat);
    struct HirExpr *guard = e->guard != NULL ? lower_expr(L, e->guard) : NULL;

    struct HirExpr *result = lower_expr(L, e->result);
    return pawHir_new_match_arm(L->hir, e->span, pat, guard, result);
}

static struct HirType *new_list_t(struct LowerAst *L, struct HirType *elem_t)
{
    struct HirTypeList *types = HirTypeList_new(L->hir);
    HirTypeList_push(L->hir, types, elem_t);

    struct HirIdent ident = {
        .name = CSTR(L, CSTR_LIST),
        .span = elem_t->hdr.span,
    };
    struct HirPath path;
    pawHir_path_init(L->hir, &path, ident.span);
    pawHir_path_add(L->hir, &path, ident, types);
    return pawHir_new_path_type(L->hir, ident.span, path);
}

static struct HirType *new_map_t(struct LowerAst *L, struct HirType *key_t, struct HirType *value_t)
{
    struct HirTypeList *types = HirTypeList_new(L->hir);
    HirTypeList_push(L->hir, types, key_t);
    HirTypeList_push(L->hir, types, value_t);

    struct HirIdent ident = {
        .name = CSTR(L, CSTR_MAP),
        .span = key_t->hdr.span,
    };
    struct HirPath path;
    pawHir_path_init(L->hir, &path, ident.span);
    pawHir_path_add(L->hir, &path, ident, types);
    return pawHir_new_path_type(L->hir, ident.span, path);
}

static struct HirDecl *lower_closure_param(struct LowerAst *L, struct AstFieldDecl *d)
{
    struct HirType *tag = d->tag != NULL ? lower_type(L, d->tag) : NULL;
    struct HirIdent ident = lower_ident(L, d->ident);
    return pawHir_new_field_decl(L->hir, d->span, ident, tag, d->is_pub);
}

static struct HirExpr *LowerClosureExpr(struct LowerAst *L, struct AstClosureExpr *e)
{
    struct AstDecl *const *pparam;
    struct HirDeclList *params = HirDeclList_new(L->hir);
    K_LIST_FOREACH (e->params, pparam) {
        struct AstFieldDecl *src = AstGetFieldDecl(*pparam);
        struct HirDecl *dst = lower_closure_param(L, src);
        HirDeclList_push(L->hir, params, dst);
    }
    struct HirType *result = e->result != NULL ? lower_type(L, e->result) : NULL;
    struct HirExpr *expr = lower_expr(L, e->expr);
    return pawHir_new_closure_expr(L->hir, e->span, params, result, expr);
}

static struct HirDecl *LowerUseDecl(struct LowerAst *L, struct AstUseDecl *d)
{
    struct HirImport const im = {
        .has_star = d->has_star,
        .modno = d->modno,
        .item = lower_ident(L, d->item),
        .as = lower_ident(L, d->as),
    };
    HirImportList_push(L->hir, L->hir->imports, im);
    return NULL;
}

static struct HirDecl *LowerAdtDecl(struct LowerAst *L, struct AstAdtDecl *d)
{
    struct HirIdent ident = lower_ident(L, d->ident);
    struct HirTypeList *traits = lower_type_list(L, d->traits);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *fields = lower_fields(L, d->fields);
    struct HirDeclList *methods = lower_methods(L, d->methods);
    return pawHir_new_adt_decl(L->hir, d->span, ident, NULL, traits,
                               generics, fields, methods, d->is_pub,
                               d->is_struct, d->is_inline);
}

static struct HirDecl *LowerTraitDecl(struct LowerAst *L, struct AstTraitDecl *d)
{
    struct HirIdent ident = lower_ident(L, d->ident);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *methods = lower_methods(L, d->methods);
    return pawHir_new_trait_decl(L->hir, d->span, ident, NULL,
                                 generics, methods, d->is_pub);
}

static struct HirDecl *LowerConstDecl(struct LowerAst *L, struct AstConstDecl *d)
{
    struct HirIdent ident = lower_ident(L, d->ident);
    struct HirType *tag = lower_type(L, d->tag);
    struct HirExpr *init = d->init != NULL ? lower_expr(L, d->init) : NULL;
    return pawHir_new_const_decl(L->hir, d->span, ident, d->annos, tag, init, d->is_pub);
}

static struct HirDecl *LowerTypeDecl(struct LowerAst *L, struct AstTypeDecl *d)
{
    struct HirIdent ident = lower_ident(L, d->ident);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirType *rhs = lower_type(L, d->rhs);
    return pawHir_new_type_decl(L->hir, d->span, ident, generics, rhs, d->is_pub);
}

// Lower a function call or enumerator constructor
static struct HirExpr *LowerCallExpr(struct LowerAst *L, struct AstCallExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExprList *args = lower_expr_list(L, e->args);
    return pawHir_new_call_expr(L->hir, e->span, target, args);
}

static struct HirExpr *LowerConversionExpr(struct LowerAst *L, struct AstConversionExpr *e)
{
    struct HirExpr *from = lower_expr(L, e->arg);
    return pawHir_new_conversion_expr(L->hir, e->span, from, e->to);
}

static struct HirExpr *lower_basic_lit(struct LowerAst *L, struct AstBasicLit *e, struct SourceSpan span)
{
    // NOTE: Integer literals are parsed as paw_Uint. Values of type paw_Uint in range [0, PAW_INT_MAX]
    //       have the same Value representation as paw_Int and no conversion is necessary. Negative
    //       integer literals are handled in "LowerUnOpExpr".
    if (e->code == BUILTIN_INT && V_UINT(e->value) > (paw_Uint)PAW_INT_MAX)
        LOWERING_ERROR(L, integer_out_of_range, span.start, e->value.u);

    return pawHir_new_basic_lit(L->hir, span, e->value, e->code);
}

static struct HirExpr *lower_tuple_lit(struct LowerAst *L, struct AstTupleLit *e, struct SourceSpan span)
{
    struct HirExprList *elems = lower_expr_list(L, e->elems);
    return pawHir_new_tuple_lit(L->hir, span, elems);
}

static struct HirExpr *lower_list_lit(struct LowerAst *L, struct AstContainerLit *e, struct SourceSpan span)
{
    paw_assert(e->code == BUILTIN_LIST);

    struct AstExpr **psrc;
    struct HirExprList *items = HirExprList_new(L->hir);
    K_LIST_FOREACH (e->items, psrc) {
        struct HirExpr *dst = lower_expr(L, *psrc);
        HirExprList_push(L->hir, items, dst);
    }
    return pawHir_new_container_lit(L->hir, span, items, BUILTIN_LIST);
}

static struct HirExpr *lower_map_lit(struct LowerAst *L, struct AstContainerLit *e, struct SourceSpan span)
{
    paw_assert(e->code == BUILTIN_MAP);

    struct AstExpr **psrc;
    struct HirExprList *items = HirExprList_new(L->hir);
    K_LIST_FOREACH (e->items, psrc) {
        struct HirExpr *dst = lower_expr(L, *psrc);
        paw_assert(HirGetFieldExpr(dst)->fid == -1);
        HirExprList_push(L->hir, items, dst);
    }
    return pawHir_new_container_lit(L->hir, span, items, BUILTIN_MAP);
}

static struct HirExpr *lower_container_lit(struct LowerAst *L, struct AstContainerLit *e, struct SourceSpan span)
{
    if (e->code == BUILTIN_LIST)
        return lower_list_lit(L, e, span);
    return lower_map_lit(L, e, span);
}

static struct HirExpr *LowerFieldExpr(struct LowerAst *L, struct AstFieldExpr *e)
{
    struct HirExpr *value = lower_expr(L, e->value);
    if (e->fid < 0) {
        struct HirExpr *key = lower_expr(L, e->key);
        return pawHir_new_keyed_field_expr(L->hir, e->span, key, value);
    }
    struct HirIdent ident = lower_ident(L, e->ident);
    return pawHir_new_named_field_expr(L->hir, e->span, ident, value, e->fid);
}

static struct HirExpr *lower_composite_lit(struct LowerAst *L, struct AstCompositeLit *e, struct SourceSpan span)
{
    struct HirPath path = lower_path(L, e->path);

    struct AstExpr **psrc;
    struct HirExprList *items = HirExprList_new(L->hir);
    K_LIST_FOREACH (e->items, psrc)
        HirExprList_push(L->hir, items, lower_expr(L, *psrc));
    return pawHir_new_composite_lit(L->hir, span, path, items);
}

static struct HirExpr *LowerParenExpr(struct LowerAst *L, struct AstParenExpr *e)
{
    return lower_expr(L, e->expr);
}

static struct HirExpr *LowerLiteralExpr(struct LowerAst *L, struct AstLiteralExpr *e)
{
    // literal kinds correspond 1-to-1 between AST and HIR
    enum HirLitKind lit_kind = CAST(enum HirLitKind, e->lit_kind);

    switch (e->lit_kind) {
        case kAstBasicLit:
            return lower_basic_lit(L, &e->basic, e->span);
        case kAstTupleLit:
            return lower_tuple_lit(L, &e->tuple, e->span);
        case kAstContainerLit:
            return lower_container_lit(L, &e->cont, e->span);
        case kAstCompositeLit:
            return lower_composite_lit(L, &e->comp, e->span);
    }
}

// TODO: eventually create a new HIR node and corresponding operation HirConcat/OP_CONCAT and
//       use that here. Using operator '+' is wasteful due to multiple intermediate allocations.
static struct HirExpr *LowerStringExpr(struct LowerAst *L, struct AstStringExpr *e)
{
    int index;
    struct HirExpr *result;
    struct AstStringPart const *ppart;
    K_LIST_ENUMERATE(e->parts, index, ppart) {
        struct HirExpr *next;
        if (ppart->is_str) {
            next = pawHir_new_basic_lit(L->hir, ppart->str.span, ppart->str.value, BUILTIN_STR);
        } else {
            struct HirExpr *expr = lower_expr(L, ppart->expr);
            struct HirIdent ident = {
                .span = expr->hdr.span,
                .name = SCAN_STR(L->C, "to_str"),
            };
            struct HirExpr *method = pawHir_new_name_selector(L->hir, ident.span, expr, ident);
            struct HirExprList *args = HirExprList_new(L->hir);
            next = pawHir_new_call_expr(L->hir, ppart->expr->hdr.span, method, args);
        }
        if (index > 0) {
            result = pawHir_new_binop_expr(L->hir, next->hdr.span, result, next, BINARY_ADD);
        } else {
            result = next;
        }
    }
    return result;
}

static struct HirDecl *LowerFuncDecl(struct LowerAst *L, struct AstFuncDecl *d)
{
    struct HirIdent ident = lower_ident(L, d->ident);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *params = lower_params(L, d, d->params);
    struct HirType *result = lower_type(L, d->result);
    struct HirExpr *body = d->body != NULL ? lower_expr(L, d->body) : NULL;
    return pawHir_new_func_decl(L->hir, d->span, ident, d->annos, generics,
                                params, result, body, d->fn_kind, d->is_pub, PAW_FALSE);
}

static struct HirExpr *new_boolean_match(struct LowerAst *L, struct SourceSpan span, struct HirExpr *cond, struct HirExpr *then_block, struct HirExpr *else_block)
{
    // lower if-else expressions into potentially non-exhaustive pattern matches

    struct Hir *hir = L->hir;
    struct HirExprList *arms = HirExprList_new(hir);

    struct HirExpr *true_expr = pawHir_new_basic_lit(hir, span, I2V(PAW_TRUE), BUILTIN_BOOL);
    struct HirPat *true_pat = pawHir_new_literal_pat(hir, cond->hdr.span, true_expr);
    struct HirExpr *true_arm = pawHir_new_match_arm(hir, span, true_pat, NULL, then_block);
    HirExprList_push(hir, arms, true_arm);

    if (else_block != NULL) {
        struct HirPath underscore;
        {
            // create "_" path for "false" case
            Str *const name = CSTR(L->C, CSTR_UNDERSCORE);
            pawHir_path_init(hir, &underscore, cond->hdr.span);
            struct HirIdent ident = {.span = cond->hdr.span, .name = name};
            pawHir_path_add(hir, &underscore, ident, NULL);
        }
        struct HirPat *false_pat = pawHir_new_path_pat(hir, cond->hdr.span, underscore);
        struct HirExpr *false_arm = pawHir_new_match_arm(hir, span, false_pat, NULL, else_block);
        HirExprList_push(hir, arms, false_arm);
    }

    return pawHir_new_match_expr(hir, span, cond, arms, else_block != NULL);
}

static struct HirExpr *LowerIfExpr(struct LowerAst *L, struct AstIfExpr *e)
{
    struct HirExpr *cond = lower_expr(L, e->cond);
    struct HirExpr *then_block = lower_expr(L, e->then_arm);
    struct HirExpr *else_block = e->else_arm != NULL ? lower_expr(L, e->else_arm) : NULL;

    return new_boolean_match(L, e->span, cond, then_block, else_block);
}

static struct HirStmt *LowerLetStmt(struct LowerAst *L, struct AstLetStmt *s)
{
    struct HirPat *pat = lower_pat(L, s->pat);
    struct HirType *tag = s->tag != NULL ? lower_type(L, s->tag) : NULL;
    struct HirExpr *init = s->init != NULL ? lower_expr(L, s->init) : NULL;
    return pawHir_new_let_stmt(L->hir, s->span, pat, tag, init);
}

static struct HirStmt *LowerExprStmt(struct LowerAst *L, struct AstExprStmt *s)
{
    struct HirExpr *expr = lower_expr(L, s->expr);
    return pawHir_new_expr_stmt(L->hir, s->span, expr);
}

static struct HirExpr *unit_lit(struct LowerAst *L, struct SourceSpan span)
{
    return pawHir_new_basic_lit(L->hir, span, P2V(NULL), BUILTIN_UNIT);
}

static struct HirExpr *LowerLoopExpr(struct LowerAst *L, struct AstLoopExpr *e)
{
    struct HirExpr *result = unit_lit(L, e->span);
    struct HirExpr *body = lower_expr(L, e->block);
    return pawHir_new_loop_expr(L->hir, e->span, body);
}

static struct HirExpr *LowerWhileExpr(struct LowerAst *L, struct AstWhileExpr *e)
{
    struct HirStmtList *stmts = HirStmtList_new(L->hir);

    {
        struct HirExpr *cond = lower_expr(L, e->cond);
        struct HirExpr *then_arm = lower_expr(L, e->block);
        struct HirStmtList *else_stmts = HirStmtList_new(L->hir);
        struct HirExpr *jump = pawHir_new_jump_expr(L->hir, e->span, JUMP_BREAK);
        HirStmtList_push(L->hir, else_stmts, pawHir_new_expr_stmt(L->hir, e->span, jump));

        struct HirExpr *else_result = pawHir_new_basic_lit(L->hir, e->span, I2V(0), BUILTIN_UNIT);
        struct HirExpr *else_arm = pawHir_new_block(L->hir, e->span, else_stmts, else_result);
        struct HirExpr *check = new_boolean_match(L, e->span, cond, then_arm, else_arm);
        HirStmtList_push(L->hir, stmts, pawHir_new_expr_stmt(L->hir, e->span, check));
    }

    struct HirExpr *result = unit_lit(L, e->span);
    struct HirExpr *body = pawHir_new_block(L->hir, e->span, stmts, result);
    return pawHir_new_loop_expr(L->hir, e->span, body);
}

static struct HirPat *new_none_pat(struct LowerAst *L, struct SourceSpan span)
{
    struct HirPath path;
    pawHir_path_init(L->hir, &path, span);
    pawHir_path_add(L->hir, &path, make_ident(CSTR(L->C, CSTR_OPTION), span), NULL);
    pawHir_path_add(L->hir, &path, make_ident(SCAN_STR(L->C, "None"), span), NULL);
    struct HirPatList *fields = HirPatList_new(L->hir);
    return pawHir_new_variant_pat(L->hir, span, path, fields, 1);
}

static struct HirPat *new_some_pat(struct LowerAst *L, struct HirPat *pat, struct SourceSpan span)
{
    struct HirPath path;
    pawHir_path_init(L->hir, &path, span);
    pawHir_path_add(L->hir, &path, make_ident(CSTR(L->C, CSTR_OPTION), span), NULL);
    pawHir_path_add(L->hir, &path, make_ident(SCAN_STR(L->C, "Some"), span), NULL);
    struct HirPatList *fields = HirPatList_new(L->hir);
    struct HirPat *variant = pawHir_new_variant_pat(L->hir, span, path, fields, 0);
    HirPatList_push(L->hir, fields, pat);
    return variant;
}

static struct HirPat *binding_pat(struct LowerAst *L, struct SourceSpan span, struct HirIdent ident)
{
    return pawHir_new_binding_pat(L->hir, span, ident);
}

// Lower a ForExpr construct
//
// Performs the following desugaring transformation:
//
//     for i in iterable {
//        ...
//     }
//              |
//              V
//     {
//         let _iterator = iterable.iterator();
//         loop {
//             match _iterator.next() {
//                 Option::Some(i) => {
//                     ...
//                 },
//                 Option::None => break,
//             }
//         }
//     }
//
static struct HirExpr *LowerForExpr(struct LowerAst *L, struct AstForExpr *e)
{
    struct Hir *hir = L->hir;
    struct HirStmtList *outer_stmts = HirStmtList_new(hir);
    struct HirIdent iter_name = {
        .name = SCAN_STR(L->C, PRIVATE("iterator")),
        .span = e->target->hdr.span,
    };
    {
        // evaluate "iterable.iterator()" and store the result in a local variable
        struct HirExpr *iterable = lower_expr(L, e->target);
        struct HirIdent func_ident = {.span = iter_name.span, .name = SCAN_STR(L->C, "iterator")};
        struct HirExpr *target = pawHir_new_name_selector(hir, e->span, iterable, func_ident);
        struct HirExpr *iterator = pawHir_new_call_expr(hir, e->span, target, HirExprList_new(hir));
        struct HirStmt *stmt = pawHir_new_let_stmt(hir, e->span, binding_pat(L, e->span, iter_name), NULL, iterator);
        HirStmtList_push(hir, outer_stmts, stmt);
    }

    struct HirExprList *arms = HirExprList_new(hir);
    {
        // create the "Some(i) => {...}" arm
        struct HirPat *pat = new_some_pat(L, lower_pat(L, e->pat), e->span);
        struct HirExpr *arm = pawHir_new_match_arm(hir, e->span, pat,
                NULL, lower_expr(L, e->block));
        HirExprList_push(hir, arms, arm);
    }

    {
        // create the "None => break" arm
        struct HirPat *pat = new_none_pat(L, e->span);
        struct HirExpr *rhs = pawHir_new_jump_expr(hir, e->span, JUMP_BREAK);
        struct HirExpr *arm = pawHir_new_match_arm(hir, e->span, pat, NULL, rhs);
        HirExprList_push(hir, arms, arm);
    }

    struct HirStmtList *inner_stmts = HirStmtList_new(hir);
    struct HirExpr *unit = pawHir_new_basic_lit(hir, e->span, I2V(0), BUILTIN_UNIT);
    struct HirExpr *body = pawHir_new_block(hir, e->span, inner_stmts, unit);
    struct HirExpr *loop = pawHir_new_loop_expr(hir, e->span, body);
    struct HirExpr *outer = pawHir_new_block(hir, e->span, outer_stmts, loop);

    struct HirExpr *next;
    {
        // call "next" on the iterator created earlier (result of evaluating "e->target")
        struct HirExpr *iterator = new_unary_path_expr(L, iter_name);
        struct HirIdent func_ident = {.span = iter_name.span, .name = SCAN_STR(L->C, "next")};
        struct HirExpr *target = pawHir_new_name_selector(hir, e->span, iterator, func_ident);
        next = pawHir_new_call_expr(hir, e->span, target, HirExprList_new(hir));
    }

    {
        // match on the "Option<T>" returned by "_iterator.next()"
        struct HirExpr *match = pawHir_new_match_expr(hir, e->span, next, arms, PAW_TRUE);
        HirStmtList_push(hir, inner_stmts, pawHir_new_expr_stmt(hir, e->span, match));
    }

    return outer;
}

static struct HirExpr *LowerIndex(struct LowerAst *L, struct AstIndex *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExpr *index = lower_expr(L, e->index);
    return pawHir_new_index_expr(L->hir, e->span, target, index);
}

static struct HirExpr *LowerSelector(struct LowerAst *L, struct AstSelector *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    if (e->is_index)
        return pawHir_new_index_selector(L->hir, e->span, target, e->index);
    struct HirIdent ident = lower_ident(L, e->ident);
    return pawHir_new_name_selector(L->hir, e->span, target, ident);
}

static struct HirExpr *LowerJumpExpr(struct LowerAst *L, struct AstJumpExpr *e)
{
    return pawHir_new_jump_expr(L->hir, e->span, e->jump_kind);
}

static struct HirStmt *LowerDeclStmt(struct LowerAst *L, struct AstDeclStmt *s)
{
    struct HirDecl *decl = lower_decl(L, s->decl);
    return pawHir_new_decl_stmt(L->hir, s->span, decl);
}

static struct HirBoundList *lower_bounds(struct LowerAst *L, struct AstBoundList *bounds)
{
    if (bounds == NULL)
        return NULL;
    struct HirBoundList *result = HirBoundList_new(L->hir);

    struct AstGenericBound *pbound;
    K_LIST_FOREACH (bounds, pbound) {
        struct HirGenericBound r;
        r.path = lower_path(L, pbound->path);
        HirBoundList_push(L->hir, result, r);
    }
    return result;
}

static struct HirDecl *LowerGenericDecl(struct LowerAst *L, struct AstGenericDecl *d)
{
    struct HirIdent ident = lower_ident(L, d->ident);
    struct HirBoundList *bounds = lower_bounds(L, d->bounds);
    return pawHir_new_generic_decl(L->hir, d->span, ident, bounds);
}

static struct HirType *LowerPathType(struct LowerAst *L, struct AstPathType *t)
{
    struct HirPath path = lower_path(L, t->path);
    return pawHir_new_path_type(L->hir, t->span, path);
}

static struct HirType *LowerContainerType(struct LowerAst *L, struct AstContainerType *t)
{
    struct HirType *first = lower_type(L, t->first);
    if (t->second == NULL)
        return new_list_t(L, first);
    struct HirType *second = lower_type(L, t->second);
    return new_map_t(L, first, second);
}

static struct HirType *unit_type(struct LowerAst *L, struct SourceSpan span)
{
    struct HirIdent ident = {
        .name = CSTR(L->C, CSTR_UNIT),
        .span = span,
    };
    struct HirPath path;
    pawHir_path_init(L->hir, &path, span);
    pawHir_path_add(L->hir, &path, ident, NULL);
    return pawHir_new_path_type(L->hir, span, path);
}

static struct HirType *LowerTupleType(struct LowerAst *L, struct AstTupleType *t)
{
    if (t->types->count == 0)
        return unit_type(L, t->span);
    struct HirTypeList *elems = lower_type_list(L, t->types);
    return pawHir_new_tuple_type(L->hir, t->span, elems);
}

static struct HirType *LowerFuncType(struct LowerAst *L, struct AstFuncType *t)
{
    struct HirTypeList *params = lower_type_list(L, t->params);
    struct HirType *result = lower_type(L, t->result);
    return pawHir_new_func_ptr(L->hir, t->span, params, result);
}

static struct HirType *LowerNeverType(struct LowerAst *L, struct AstNeverType *t)
{
    return pawHir_new_never_type(L->hir, t->span);
}

static struct HirType *LowerInferType(struct LowerAst *L, struct AstInferType *t)
{
    return pawHir_new_infer_type(L->hir, t->span);
}

static void combine_or_parts(struct LowerAst *L, struct HirPatList *pats, struct HirPat *part)
{
    if (!HirIsOrPat(part)) {
        HirPatList_push(L->hir, pats, part);
        return;
    }

    struct HirPat *const *ppat;
    struct HirOrPat *other = HirGetOrPat(part);
    K_LIST_FOREACH (other->pats, ppat)
        HirPatList_push(L->hir, pats, *ppat);
}

static struct HirPat *LowerOrPat(struct LowerAst *L, struct AstOrPat *p)
{
    struct HirPatList *pats = HirPatList_new(L->hir);
    combine_or_parts(L, pats, lower_pat(L, p->lhs));
    combine_or_parts(L, pats, lower_pat(L, p->rhs));
    return pawHir_new_or_pat(L->hir, p->span, pats);
}

static struct HirPat *LowerFieldPat(struct LowerAst *L, struct AstFieldPat *p)
{
    struct HirIdent ident = lower_ident(L, p->ident);
    struct HirPat *pat = lower_pat(L, p->pat);
    return pawHir_new_field_pat(L->hir, p->span, ident, pat, -1);
}

static struct HirPat *LowerStructPat(struct LowerAst *L, struct AstStructPat *p)
{
    struct HirPath path = lower_path(L, p->path);
    struct HirPatList *fields = lower_pat_list(L, p->fields);
    return pawHir_new_struct_pat(L->hir, p->span, path, fields);
}

static struct HirPat *LowerVariantPat(struct LowerAst *L, struct AstVariantPat *p)
{
    struct HirPath path = lower_path(L, p->path);
    struct HirPatList *fields = lower_pat_list(L, p->fields);
    return pawHir_new_variant_pat(L->hir, p->span, path, fields, -1);
}

static struct HirPat *LowerTuplePat(struct LowerAst *L, struct AstTuplePat *p)
{
    struct HirPatList *elems = lower_pat_list(L, p->elems);
    return pawHir_new_tuple_pat(L->hir, p->span, elems);
}

static struct HirPat *LowerPathPat(struct LowerAst *L, struct AstPathPat *p)
{
    struct HirPath path = lower_path(L, p->path);
    return pawHir_new_path_pat(L->hir, p->span, path);
}

static struct HirPat *LowerLiteralPat(struct LowerAst *L, struct AstLiteralPat *p)
{
    if (AstIsLiteralExpr(p->expr)) {
        // Special case for integers: normal lowering functions assume integers are positive and
        // have type paw_Uint so they can handle overflow (UnOp(Literal(i), -) is detected and
        // converted into Literal(-i) after checking for overflow).
        struct AstLiteralExpr *e = AstGetLiteralExpr(p->expr);
        if (e->lit_kind == kAstBasicLit && e->basic.code == BUILTIN_INT) {
            struct HirExpr *expr = pawHir_new_basic_lit(L->hir, p->span, e->basic.value, BUILTIN_INT);
            return pawHir_new_literal_pat(L->hir, p->span, expr);
        }
    }
    struct HirExpr *expr = lower_expr(L, p->expr);
    return pawHir_new_literal_pat(L->hir, p->span, expr);
}

static struct HirPat *LowerWildcardPat(struct LowerAst *L, struct AstWildcardPat *p)
{
    return pawHir_new_wildcard_pat(L->hir, p->span);
}

static struct HirDecl *lower_decl(struct LowerAst *L, struct AstDecl *decl)
{
    switch (AST_KINDOF(decl)) {
#define DEFINE_CASE(X) \
    case kAst##X:      \
        return Lower##X(L, AstGet##X(decl));
        AST_DECL_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirStmt *lower_stmt(struct LowerAst *L, struct AstStmt *stmt)
{
    switch (AST_KINDOF(stmt)) {
#define DEFINE_CASE(X) \
    case kAst##X:      \
        return Lower##X(L, AstGet##X(stmt));
        AST_STMT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirExpr *lower_expr(struct LowerAst *L, struct AstExpr *expr)
{
    switch (AST_KINDOF(expr)) {
#define DEFINE_CASE(X) \
    case kAst##X:      \
        return Lower##X(L, AstGet##X(expr));
        AST_EXPR_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirPat *lower_pat(struct LowerAst *L, struct AstPat *pat)
{
    switch (AST_KINDOF(pat)) {
#define DEFINE_CASE(X) \
    case kAst##X:      \
        return Lower##X(L, AstGet##X(pat));
        AST_PAT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirType *lower_type(struct LowerAst *L, struct AstType *type)
{
    switch (AST_KINDOF(type)) {
#define DEFINE_CASE(X) \
    case kAst##X:      \
        return Lower##X(L, AstGet##X(type));
        AST_TYPE_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct Hir *lower_ast(struct LowerAst *L, struct Ast *ast)
{
    L->modname = ast->name;
    struct ModuleList *mods = L->C->modules;
    while (ast->modno >= mods->count) {
        ModuleList_push(L->C, mods, NULL);
    }
    struct Hir *hir = pawHir_new(L->C, ast->name, ast->modno);
    L->hir = hir;

    struct ModuleInfo *mod = pawP_mi_new(L->C, hir);
    ModuleList_set(mods, hir->modno, mod);
    hir->items = lower_decl_list(L, ast->items);
    pawAst_free(ast);

    if (pawP_push_callback(L->C, "paw.on_build_hir")) {
        paw_Env *P = ENV(L);
        paw_push_rawptr(P, hir);
        paw_call(P, 1);
    }

    return hir;
}

void pawP_lower_ast(struct Compiler *C)
{
    struct LowerAst L = {
        .dm = C->dm,
        .P = ENV(C),
        .C = C,
    };

    struct Hir *prelude = lower_ast(&L, C->ast_prelude);
    C->hir_prelude = prelude;

    ImportMapIterator iter;
    ImportMapIterator_init(C->imports, &iter);
    while (ImportMapIterator_is_valid(&iter)) {
        struct Ast *const *past = ImportMapIterator_valuep(&iter);
        lower_ast(&L, *past);
        ImportMapIterator_next(&iter);
    }

    // release AST memory
    pawP_pool_free(C, C->ast_pool);
    C->ast_pool = NULL;
}
