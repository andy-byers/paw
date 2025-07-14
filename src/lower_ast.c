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
#include "resolve.h"
#include "str.h"
#include "unify.h"

#define LOWERING_ERROR(L_, Kind_, ...) pawErr_##Kind_((L_)->C, (L_)->m->name, __VA_ARGS__)

#define NEW_NODE(L_, Kind_, ...) \
    pawHir_new_##Kind_((L_)->hir, __VA_ARGS__)

struct LowerAst {
    struct Compiler *C;
    struct Pool *pool;
    struct Ast *ast;
    struct Hir *hir;
    paw_Env *P;

    DeclId adt_did;
    struct SegmentTable *segtab;
    struct HirStmtList *stmts;
    struct AstModuleDecl *m;
};

static struct HirStmt *lower_stmt(struct LowerAst *, struct AstStmt *);
static struct HirExpr *lower_expr(struct LowerAst *, struct AstExpr *);
static struct HirDecl *lower_decl(struct LowerAst *, struct AstDecl *);
static struct HirType *lower_type(struct LowerAst *, struct AstType *);
static struct HirPat *lower_pat(struct LowerAst *, struct AstPat *);

#define DEFINE_LOWER_LIST(name, T)                                                                 \
    static struct Hir##T##List *lower_##name##_list(struct LowerAst *L, struct Ast##T##List *list) \
    {                                                                                              \
        if (list == NULL) return NULL;                                                             \
        struct Hir##T##List *r = Hir##T##List_new(L->hir);                                         \
        for (int i = 0; i < list->count; ++i) {                                                    \
            struct Hir##T *node = lower_##name(L, list->data[i]);                                  \
            if (node != NULL) Hir##T##List_push(L->hir, r, node);                                  \
        }                                                                                          \
        return r;                                                                                  \
    }
DEFINE_LOWER_LIST(expr, Expr)
DEFINE_LOWER_LIST(decl, Decl)
DEFINE_LOWER_LIST(stmt, Stmt)
DEFINE_LOWER_LIST(type, Type)
DEFINE_LOWER_LIST(pat, Pat)

static NodeId builtin_id(struct LowerAst *L, enum BuiltinKind kind)
{
    return pawP_builtin_info(L->C, kind)->id;
}

static NodeId variant_id(struct LowerAst *L, NodeId adt, int discr)
{
    struct AstAdtDecl const *option = pawAst_get_node(L->ast, adt);
    paw_assert(option->id.value == adt.value);
    struct AstDecl *variant = AstDeclList_get(option->variants, discr);
    return variant->hdr.id;
}

static NodeId next_node_id(struct LowerAst *L)
{
    return (NodeId){++L->hir->node_count};
}

static DeclId next_decl_id(struct LowerAst *L)
{
    return (DeclId){.value = L->hir->decls->count + 1, .modno = L->m->modno};
}

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

static struct HirType *unit_type(struct LowerAst *L, struct SourceSpan span)
{
    NodeId const id = next_node_id(L);
    NodeId const target = builtin_id(L, BUILTIN_UNIT);

    struct HirIdent const ident = {
        .name = CSTR(L->C, CSTR_UNIT),
        .span = span,
    };
    struct HirSegments *segments = HirSegments_new(L->hir);
    pawHir_add_segment(L->hir, segments, span, id, ident, NULL, target);
    struct HirPath const path = pawHir_path_create(span, segments, HIR_PATH_ITEM);
    return NEW_NODE(L, path_type, span, next_node_id(L), path);
}

static struct HirExpr *unit_lit(struct LowerAst *L, struct SourceSpan span)
{
    return NEW_NODE(L, basic_lit, span, next_node_id(L), P2V(NULL), BUILTIN_UNIT);
}

static struct HirPath new_unary_path(struct LowerAst *L, struct HirIdent ident, NodeId id, enum HirPathKind kind, NodeId target)
{
    struct HirSegments *segments = HirSegments_new(L->hir);
    pawHir_add_segment(L->hir, segments, ident.span, id, ident, NULL, target);
    return pawHir_path_create(ident.span, segments, kind);
}

static struct HirExpr *new_unary_path_expr(struct LowerAst *L, struct HirIdent ident, NodeId id, enum HirPathKind kind, NodeId target)
{
    struct HirPath const path = new_unary_path(L, ident, id, kind, target);
    return NEW_NODE(L, path_expr, ident.span, next_node_id(L), path);
}

static struct HirExpr *lower_block(struct LowerAst *L, struct AstBlock *e)
{
    // Keep track of the statement list so that LetStmt nodes that correspond to
    // bindings unpacking can lower to multiple statements.
    struct HirStmtList *outer = L->stmts;
    L->stmts = HirStmtList_new(L->hir);
    struct AstStmt *const *pstmt;
    K_LIST_FOREACH (e->stmts, pstmt) {
        struct HirStmt *stmt = lower_stmt(L, *pstmt);
        if (stmt != NULL) HirStmtList_push(L->hir, L->stmts, stmt);
    }
    struct HirStmtList *stmts = L->stmts;
    L->stmts = outer;

    struct HirExpr *result = e->result != NULL ? lower_expr(L, e->result) : NULL;
    return NEW_NODE(L, block, e->span, e->id, stmts, result);
}

static struct HirExpr *LowerBlock(struct LowerAst *L, struct AstBlock *block)
{
    return lower_block(L, block);
}

static struct HirDecl *LowerFieldDecl(struct LowerAst *L, struct AstFieldDecl *d)
{
    struct HirType *tag = lower_type(L, d->tag);
    struct HirIdent const ident = lower_ident(L, d->ident);
    return NEW_NODE(L, field_decl, d->span, d->id, d->did, ident, tag, d->is_pub);
}

static struct HirDecl *LowerParamDecl(struct LowerAst *L, struct AstParamDecl *d)
{
    struct HirType *tag = lower_type(L, d->tag);
    struct HirIdent const ident = lower_ident(L, d->ident);
    return NEW_NODE(L, param_decl, d->span, d->id, d->did, ident, tag);
}

static struct HirDecl *LowerVariantDecl(struct LowerAst *L, struct AstVariantDecl *d)
{
    struct HirIdent const ident = lower_ident(L, d->ident);
    struct HirDeclList *fields = lower_decl_list(L, d->fields);
    return NEW_NODE(L, variant_decl, d->span, d->id, d->did, ident, fields, d->index, L->adt_did);
}

static struct HirExpr *LowerReturnExpr(struct LowerAst *L, struct AstReturnExpr *e)
{
    struct HirExpr *expr = e->expr != NULL ? lower_expr(L, e->expr) : NULL;
    return NEW_NODE(L, return_expr, e->span, e->id, expr);
}

static struct HirDecl *LowerFnDecl(struct LowerAst *L, struct AstFnDecl *d);

static struct HirDeclList *lower_methods(struct LowerAst *L, struct AstDeclList *src)
{
    paw_Env *P = ENV(L);
    struct HirDeclList *dst = HirDeclList_new(L->hir);
    for (int i = 0; i < src->count; ++i) {
        struct AstDecl *decl = AstDeclList_get(src, i);
        struct AstFnDecl *d = AstGetFnDecl(decl);
        struct HirDecl *result = LowerFnDecl(L, d);
        struct HirFnDecl *r = HirGetFnDecl(result);
        HirDeclList_push(L->hir, dst, result);
    }
    return dst;
}

static struct HirPath lower_path(struct LowerAst *L, struct AstPath path)
{
    struct HirSegments *segments = HirSegments_new(L->hir);
    paw_assert(path.segments->count > 0);

    struct AstSegment *psrc;
    enum ResolvedKind last_kind;
    K_LIST_FOREACH (path.segments, psrc) {
        struct HirTypeList *types = lower_type_list(L, psrc->types);
        struct HirIdent const ident = lower_ident(L, psrc->ident);
        struct ResolvedSegment const *res = SegmentTable_get(L->C, L->segtab, psrc->id);
        if (res->kind != RESOLVED_MODULE) // strip off module prefix
            pawHir_add_segment(L->hir, segments, psrc->span, psrc->id, ident, types, res->id);
        last_kind = res->kind;
    }

    enum HirPathKind kind;
    switch (last_kind) {
        case RESOLVED_LOCAL:
            paw_assert(path.segments->count == 1);
            kind = HIR_PATH_LOCAL;
            break;
        case RESOLVED_ASSOC:
            kind = HIR_PATH_ASSOC;
            break;
        default: {
            // non-import path never ends with a module name
            paw_assert(last_kind == RESOLVED_DECL);
            kind = HIR_PATH_ITEM;
            break;
        }
    }

    return pawHir_path_create(path.span, segments, kind);
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct HirExpr *LowerPathExpr(struct LowerAst *L, struct AstPathExpr *e)
{
    struct HirPath const path = lower_path(L, e->path);
    return NEW_NODE(L, path_expr, e->span, e->id, path);
}

static struct HirExpr *LowerLogicalExpr(struct LowerAst *L, struct AstLogicalExpr *e)
{
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return NEW_NODE(L, logical_expr, e->span, e->id, lhs, rhs, e->is_and);
}

static struct HirExpr *LowerChainExpr(struct LowerAst *L, struct AstChainExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    return NEW_NODE(L, chain_expr, e->span, e->id, target);
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
            return NEW_NODE(L, basic_lit, e->span, e->id, I2V(i), BUILTIN_INT);
        }
    }
    struct HirExpr *target = lower_expr(L, e->target);
    return NEW_NODE(L, unop_expr, e->span, e->id, target, e->op);
}

static struct HirExpr *LowerBinOpExpr(struct LowerAst *L, struct AstBinOpExpr *e)
{
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return NEW_NODE(L, binop_expr, e->span, e->id, lhs, rhs, e->op);
}

static struct HirExpr *new_literal_field(struct LowerAst *L, const char *name, struct HirExpr *expr, int fid)
{
    struct HirIdent const ident = {
        .name = SCAN_STR(L->C, name),
        .span = expr->hdr.span,
    };
    return NEW_NODE(L, named_field_expr, expr->hdr.span, next_node_id(L), ident, expr, fid);
}

static struct HirPath new_builtin_path_(struct LowerAst *L, struct SourceSpan span, enum BuiltinKind builtin_kind, unsigned cstr_kind)
{
    NodeId const id = next_node_id(L);
    NodeId const target = builtin_id(L, builtin_kind);

    return new_unary_path(L, (struct HirIdent){
                .name = CSTR(L, cstr_kind),
                .span = span,
            }, id, HIR_PATH_ITEM, target);
}
#define NEW_BUILTIN_PATH(L_, Span_, Suffix_) \
    new_builtin_path_(L_, Span_, BUILTIN_##Suffix_, CSTR_##Suffix_)

static struct HirExpr *into_range(struct LowerAst *L, struct AstRangeExpr *e)
{
    struct HirPath const path = e->is_inclusive
        ? NEW_BUILTIN_PATH(L, e->span, RANGE_INCLUSIVE)
        : NEW_BUILTIN_PATH(L, e->span, RANGE);
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    struct HirExprList *items = HirExprList_new(L->hir);
    HirExprList_push(L->hir, items, new_literal_field(L, "start", lhs, 0));
    HirExprList_push(L->hir, items, new_literal_field(L, "end", rhs, 1));
    return NEW_NODE(L, composite_lit, e->span, e->id, path, items);
}

static struct HirExpr *into_range_full(struct LowerAst *L, struct AstRangeExpr *e)
{
    if (e->is_inclusive)
        LOWERING_ERROR(L, invalid_inclusive_range, e->span.start);

    struct HirPath const path = NEW_BUILTIN_PATH(L, e->span, RANGE_FULL);
    return NEW_NODE(L, path_expr, e->span, e->id, path);
}

static struct HirExpr *into_range_to(struct LowerAst *L, struct AstRangeExpr *e)
{
    struct HirPath const path = e->is_inclusive
        ? NEW_BUILTIN_PATH(L, e->span, RANGE_TO_INCLUSIVE)
        : NEW_BUILTIN_PATH(L, e->span, RANGE_TO);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    struct HirExprList *items = HirExprList_new(L->hir);
    HirExprList_push(L->hir, items, new_literal_field(L, "end", rhs, 1));
    return NEW_NODE(L, composite_lit, e->span, e->id, path, items);
}

static struct HirExpr *into_range_from(struct LowerAst *L, struct AstRangeExpr *e)
{
    if (e->is_inclusive)
        LOWERING_ERROR(L, invalid_inclusive_range, e->span.start);

    struct HirPath const path = NEW_BUILTIN_PATH(L, e->span, RANGE_FROM);
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExprList *items = HirExprList_new(L->hir);
    HirExprList_push(L->hir, items, new_literal_field(L, "start", lhs, 0));
    return NEW_NODE(L, composite_lit, e->span, e->id, path, items);
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
        pawErr_invalid_assignment_target(L->C, L->m->name, target->hdr.span.start);
}

static struct HirExpr *LowerAssignExpr(struct LowerAst *L, struct AstAssignExpr *e)
{
    check_assignment_target(L, e->lhs);
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return NEW_NODE(L, assign_expr, e->span, e->id, lhs, rhs);
}

static struct HirExpr *LowerOpAssignExpr(struct LowerAst *L, struct AstOpAssignExpr *e)
{
    check_assignment_target(L, e->lhs);
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return NEW_NODE(L, op_assign_expr, e->span, e->id, lhs, rhs, e->op);
}

static struct HirExpr *LowerMatchExpr(struct LowerAst *L, struct AstMatchExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExprList *arms = lower_expr_list(L, e->arms);
    paw_assert(arms->count > 0);

    return NEW_NODE(L, match_expr, e->span, e->id, target, arms, PAW_TRUE);
}

static struct HirExpr *LowerMatchArm(struct LowerAst *L, struct AstMatchArm *e)
{
    struct HirPat *pat = lower_pat(L, e->pat);
    struct HirExpr *guard = e->guard != NULL ? lower_expr(L, e->guard) : NULL;

    struct HirExpr *result = lower_expr(L, e->result);
    return NEW_NODE(L, match_arm, e->span, e->id, pat, guard, result);
}

static struct HirType *new_list_t(struct LowerAst *L, struct SourceSpan span, struct HirType *elem_t)
{
    struct HirTypeList *types = HirTypeList_new(L->hir);
    HirTypeList_push(L->hir, types, elem_t);

    NodeId const id = next_node_id(L);
    NodeId const target = builtin_id(L, BUILTIN_LIST);

    struct HirIdent const ident = {
        .name = CSTR(L, CSTR_LIST),
        .span = elem_t->hdr.span,
    };
    struct HirSegments *segments = HirSegments_new(L->hir);
    pawHir_add_segment(L->hir, segments, span, id, ident, types, target);
    struct HirPath path = pawHir_path_create(ident.span, segments, HIR_PATH_ITEM);
    return NEW_NODE(L, path_type, ident.span, next_node_id(L), path);
}

static struct HirType *new_map_t(struct LowerAst *L, struct SourceSpan span, struct HirType *key_t, struct HirType *value_t)
{
    struct HirTypeList *types = HirTypeList_new(L->hir);
    HirTypeList_push(L->hir, types, key_t);
    HirTypeList_push(L->hir, types, value_t);

    NodeId const id = next_node_id(L);
    NodeId const target = builtin_id(L, BUILTIN_MAP);

    struct HirIdent const ident = {
        .name = CSTR(L, CSTR_MAP),
        .span = key_t->hdr.span,
    };
    struct HirSegments *segments = HirSegments_new(L->hir);
    pawHir_add_segment(L->hir, segments, span, id, ident, types, target);
    struct HirPath path = pawHir_path_create(ident.span, segments, HIR_PATH_ITEM);
    return NEW_NODE(L, path_type, ident.span, next_node_id(L), path);
}

static struct HirDecl *lower_closure_param(struct LowerAst *L, struct AstParamDecl *d)
{
    struct HirType *tag = d->tag != NULL ? lower_type(L, d->tag) : NULL;
    struct HirIdent const ident = lower_ident(L, d->ident);
    return NEW_NODE(L, param_decl, d->span, d->id, d->did, ident, tag);
}

static struct HirExpr *LowerClosureExpr(struct LowerAst *L, struct AstClosureExpr *e)
{
    struct AstDecl *const *pparam;
    struct HirDeclList *params = HirDeclList_new(L->hir);
    K_LIST_FOREACH (e->params, pparam) {
        struct AstParamDecl *src = AstGetParamDecl(*pparam);
        struct HirDecl *dst = lower_closure_param(L, src);
        HirDeclList_push(L->hir, params, dst);
    }
    struct HirType *result = e->result != NULL ? lower_type(L, e->result) : NULL;
    struct HirExpr *expr = lower_expr(L, e->expr);
    return NEW_NODE(L, closure_expr, e->span, e->id, params, result, expr);
}

static struct HirDecl *LowerUseDecl(struct LowerAst *L, struct AstUseDecl *d)
{
    return NULL; // no corresponding HIR node
}

static struct HirDecl *LowerAdtDecl(struct LowerAst *L, struct AstAdtDecl *d)
{
    L->adt_did = d->did;
    struct HirIdent const ident = lower_ident(L, d->ident);
    struct HirTypeList *traits = lower_type_list(L, d->traits);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *variants = lower_decl_list(L, d->variants);
    struct HirDeclList *methods = lower_methods(L, d->methods);
    L->adt_did = NO_DECL;

    return NEW_NODE(L, adt_decl, d->span, d->id, d->did, ident, traits, generics, variants,
            methods, d->is_pub, d->is_struct, d->is_inline);
}

static struct HirDecl *LowerTraitDecl(struct LowerAst *L, struct AstTraitDecl *d)
{
    struct HirIdent const ident = lower_ident(L, d->ident);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *methods = lower_methods(L, d->methods);
    return NEW_NODE(L, trait_decl, d->span, d->id, d->did, ident, generics, methods, d->is_pub);
}

static struct HirDecl *LowerConstDecl(struct LowerAst *L, struct AstConstDecl *d)
{
    struct HirIdent const ident = lower_ident(L, d->ident);
    struct HirType *tag = lower_type(L, d->tag);
    struct HirExpr *init = d->init != NULL ? lower_expr(L, d->init) : NULL;
    return NEW_NODE(L, const_decl, d->span, d->id, d->did, ident, d->annos, tag, init, d->is_pub);
}

static struct HirDecl *LowerTypeDecl(struct LowerAst *L, struct AstTypeDecl *d)
{
    struct HirIdent const ident = lower_ident(L, d->ident);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirType *rhs = lower_type(L, d->rhs);
    return NEW_NODE(L, type_decl, d->span, d->id, d->did, ident, generics, rhs, d->is_pub);
}

// Lower a function call or enumerator constructor
static struct HirExpr *LowerCallExpr(struct LowerAst *L, struct AstCallExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExprList *args = lower_expr_list(L, e->args);
    return NEW_NODE(L, call_expr, e->span, e->id, target, args);
}

static struct HirExpr *LowerConversionExpr(struct LowerAst *L, struct AstConversionExpr *e)
{
    struct HirExpr *from = lower_expr(L, e->arg);
    return NEW_NODE(L, conversion_expr, e->span, e->id, from, e->to);
}

static struct HirExpr *lower_basic_lit(struct LowerAst *L, struct AstBasicLit *e, struct SourceSpan span, NodeId id)
{
    // NOTE: Integer literals are parsed as paw_Uint. Values of type paw_Uint in range [0, PAW_INT_MAX]
    //       have the same Value representation as paw_Int and no conversion is necessary. Negative
    //       integer literals are handled in "LowerUnOpExpr".
    if (e->code == BUILTIN_INT && V_UINT(e->value) > (paw_Uint)PAW_INT_MAX)
        LOWERING_ERROR(L, integer_out_of_range, span.start, e->value.u);

    return NEW_NODE(L, basic_lit, span, id, e->value, e->code);
}

static struct HirExpr *lower_tuple_lit(struct LowerAst *L, struct AstTupleLit *e, struct SourceSpan span, NodeId id)
{
    struct HirExprList *elems = lower_expr_list(L, e->elems);
    return NEW_NODE(L, tuple_lit, span, id, elems);
}

static struct HirExpr *lower_container_lit(struct LowerAst *L, struct AstContainerLit *e, struct SourceSpan span, NodeId id)
{
    struct HirExprList *items = lower_expr_list(L, e->items);
    return NEW_NODE(L, container_lit, span, id, items, e->code);
}

static struct HirExpr *LowerFieldExpr(struct LowerAst *L, struct AstFieldExpr *e)
{
    struct HirExpr *value = lower_expr(L, e->value);
    if (e->fid < 0) {
        struct HirExpr *key = lower_expr(L, e->key);
        return NEW_NODE(L, keyed_field_expr, e->span, e->id, key, value);
    }
    struct HirIdent const ident = lower_ident(L, e->ident);
    return NEW_NODE(L, named_field_expr, e->span, e->id, ident, value, e->fid);
}

static struct HirExpr *lower_composite_lit(struct LowerAst *L, struct AstCompositeLit *e, struct SourceSpan span, NodeId id)
{
    struct HirPath const path = lower_path(L, e->path);

    struct AstExpr *const *psrc;
    struct HirExprList *items = HirExprList_new(L->hir);
    K_LIST_FOREACH (e->items, psrc)
        HirExprList_push(L->hir, items, lower_expr(L, *psrc));
    return NEW_NODE(L, composite_lit, span, id, path, items);
}

static struct HirExpr *LowerParenExpr(struct LowerAst *L, struct AstParenExpr *e)
{
    return lower_expr(L, e->expr);
}

static struct HirExpr *LowerLiteralExpr(struct LowerAst *L, struct AstLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kAstBasicLit:
            return lower_basic_lit(L, &e->basic, e->span, e->id);
        case kAstTupleLit:
            return lower_tuple_lit(L, &e->tuple, e->span, e->id);
        case kAstContainerLit:
            return lower_container_lit(L, &e->cont, e->span, e->id);
        case kAstCompositeLit:
            return lower_composite_lit(L, &e->comp, e->span, e->id);
    }
}

// TODO: eventually create a new HIR node and corresponding operation HirConcat/OP_CONCAT and
//       use that here. Using operator '+' is wasteful due to multiple intermediate allocations.
static struct HirExpr *LowerStringExpr(struct LowerAst *L, struct AstStringExpr *e)
{
    struct AstStringPart const first_part = AstStringList_first(e->parts);
    paw_assert(first_part.is_str);
    struct HirExpr *result = NEW_NODE(L, basic_lit, first_part.str.span, e->id, first_part.str.value, BUILTIN_STR);

    paw_assert(e->parts->count % 2 == 1);
    for (int i = 1; i < e->parts->count; i += 2) {
        struct AstStringPart const expr_part = AstStringList_get(e->parts, i);
        paw_assert(!expr_part.is_str);
        struct HirExpr *expr = lower_expr(L, expr_part.expr);
        struct HirIdent const ident = {
            .name = SCAN_STR(L->C, "to_str"),
            .span = expr->hdr.span,
        };
        struct HirExpr *method = NEW_NODE(L, name_selector, ident.span, next_node_id(L), expr, ident);
        struct HirExprList *args = HirExprList_new(L->hir);
        struct HirExpr *next = NEW_NODE(L, call_expr, expr_part.expr->hdr.span, next_node_id(L), method, args);
        result = NEW_NODE(L, binop_expr, next->hdr.span, next_node_id(L), result, next, BINARY_ADD);

        struct AstStringPart const str_part = AstStringList_get(e->parts, i + 1);
        paw_assert(str_part.is_str);
        next = NEW_NODE(L, basic_lit, str_part.str.span, next_node_id(L), str_part.str.value, BUILTIN_STR);
        result = NEW_NODE(L, binop_expr, next->hdr.span, next_node_id(L), result, next, BINARY_ADD);
    }
    return result;
}

static struct HirDecl *LowerFnDecl(struct LowerAst *L, struct AstFnDecl *d)
{
    struct HirIdent const ident = lower_ident(L, d->ident);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *params = lower_decl_list(L, d->params);
    struct HirType *result = d->result != NULL ? lower_type(L, d->result)
        : unit_type(L, (struct SourceSpan){0}); // TODO: "params" needs a span that includes the parenthesis
    struct HirExpr *body = d->body != NULL ? lower_expr(L, d->body) : NULL;
    return NEW_NODE(L, fn_decl, d->span, d->id, d->did, ident, d->annos, generics,
            params, result, body, d->fn_kind, d->is_pub, !d->is_method);
}

static struct HirExpr *new_boolean_match(struct LowerAst *L, struct SourceSpan span, struct HirExpr *cond, struct HirExpr *then_block, struct HirExpr *else_block)
{
    // lower if-else expressions into potentially non-exhaustive pattern matches

    struct Hir *hir = L->hir;
    struct HirExprList *arms = HirExprList_new(hir);

    struct HirExpr *true_expr = NEW_NODE(L, basic_lit, span, next_node_id(L), I2V(PAW_TRUE), BUILTIN_BOOL);
    struct HirPat *true_pat = NEW_NODE(L, literal_pat, cond->hdr.span, next_node_id(L), true_expr);
    struct HirExpr *true_arm = NEW_NODE(L, match_arm, span, next_node_id(L), true_pat, NULL, then_block);
    HirExprList_push(hir, arms, true_arm);

    if (else_block != NULL) {
        struct HirPat *false_pat = NEW_NODE(L, wildcard_pat, cond->hdr.span, next_node_id(L));
        struct HirExpr *false_arm = NEW_NODE(L, match_arm, span, next_node_id(L), false_pat, NULL, else_block);
        HirExprList_push(hir, arms, false_arm);
    }

    return NEW_NODE(L, match_expr, span, next_node_id(L), cond, arms, else_block != NULL);
}

static struct HirExpr *LowerIfExpr(struct LowerAst *L, struct AstIfExpr *e)
{
    struct HirExpr *cond = lower_expr(L, e->cond);
    struct HirExpr *then_block = lower_expr(L, e->then_arm);
    struct HirExpr *else_block = e->else_arm != NULL ? lower_expr(L, e->else_arm) : NULL;

    return new_boolean_match(L, e->span, cond, then_block, else_block);
}


//
// Destructuring support
//
// Performs the following transformation, the result of which is passed to the pattern
// matching compiler during HIR lowering. The resulting match expression must be
// exhaustive, meaning "rhs" must have only a single variant. Note that the "x" defined
// by the final generated "let" statement must have the same node ID as the original
// "x" binding.
//
// Before:
// {
//     let Struct{x} = rhs;
//     ...
// }
//
// After:
// {
//     let mut _x;
//     match rhs {
//         Struct{x} => {
//             _x = x;
//         }
//     }
//     let x = _x;
//     ...
// }
//

struct Unpacking {
    struct SourceSpan span;
    struct UnpackingList *info;
    struct LowerAst *L;
};

struct UnpackingInfo {
    struct HirBindingPat *original; // original "x"
    struct HirBindingPat *temp; // local variable "_x"
    struct HirBindingPat *match; // "x" binding inside match
};

DEFINE_LIST(struct LowerAst, UnpackingList, struct UnpackingInfo)

static struct HirPat *new_temp_binding(struct LowerAst *L, struct SourceSpan span, struct HirIdent ident)
{
    ident.name = pawP_format_string(L->C, "_%s", ident.name->text);
    return NEW_NODE(L, binding_pat, span, next_node_id(L), ident);
}

static void collect_binding(struct HirVisitor *V, struct HirBindingPat *original)
{
    struct Unpacking *ctx = V->ud;
    struct LowerAst *L = ctx->L;

    // "original" pointer already embedded in the matched-on pattern
    struct HirPat *temp = new_temp_binding(L, original->span, original->ident);
    struct HirPat *new_original = NEW_NODE(L, binding_pat, original->span, next_node_id(L), original->ident);
    UnpackingList_push(L, ctx->info, (struct UnpackingInfo){
                .original = HirGetBindingPat(new_original),
                .temp = HirGetBindingPat(temp),
                .match = original,
            });
    struct HirStmt *let = NEW_NODE(L, let_stmt, original->span, next_node_id(L), temp, NULL, NULL);
    HirStmtList_push(L->hir, L->stmts, let);
}

static struct HirStmtList *create_unpackers(struct Unpacking *ctx)
{
    struct LowerAst *L = ctx->L;
    struct UnpackingInfo *pinfo;
    struct HirStmtList *setters = HirStmtList_new(L->hir);
    K_LIST_FOREACH(ctx->info, pinfo) {
        // NOTE: "lhs" refers to the temporary LetStmt binding and "rhs" to the match binding
        struct HirExpr *lhs = new_unary_path_expr(L, pinfo->temp->ident, next_node_id(L), HIR_PATH_LOCAL, pinfo->temp->id);
        struct HirExpr *rhs = new_unary_path_expr(L, pinfo->match->ident, next_node_id(L), HIR_PATH_LOCAL, pinfo->match->id);
        struct HirExpr *setter = NEW_NODE(L, assign_expr, ctx->span, next_node_id(L), lhs, rhs);
        HirStmtList_push(L->hir, setters, NEW_NODE(L, expr_stmt, ctx->span, next_node_id(L), setter));
    }
    return setters;
}

static struct HirStmt *unpack_bindings(struct LowerAst *L, struct HirPat *lhs, struct HirExpr *rhs, struct HirType *tag, NodeId id)
{
    struct Unpacking ctx = {
        .info = UnpackingList_new(L),
        .span = lhs->hdr.span,
        .L = L,
    };

    if (tag != NULL) // wrap in ascription expression to facilitate type checking
        rhs = NEW_NODE(L, ascription_expr, rhs->hdr.span, next_node_id(L), rhs, tag);

    // declare temporaries for each binding that appears in the pattern
    struct HirVisitor collector;
    pawHir_visitor_init(&collector, L->hir, &ctx);
    collector.PostVisitBindingPat = collect_binding;
    pawHir_visit_pat(&collector, lhs);

    // create the match expression containing an assignment to each binding
    struct HirStmtList *setters = create_unpackers(&ctx);
    struct HirExpr *block = NEW_NODE(L, block, ctx.span, next_node_id(L), setters, unit_lit(L, ctx.span));
    struct HirExprList *arms = HirExprList_new(L->hir);
    HirExprList_push(L->hir, arms, NEW_NODE(L, match_arm, ctx.span, next_node_id(L), lhs, NULL, block));
    struct HirExpr *match = NEW_NODE(L, match_expr, ctx.span, next_node_id(L), rhs, arms, PAW_TRUE);
    HirStmtList_push(L->hir, L->stmts, NEW_NODE(L, expr_stmt, ctx.span, id, match));

    // assign to fresh bindings from temporaries
    struct UnpackingInfo *pinfo;
    K_LIST_FOREACH(ctx.info, pinfo) {
        struct HirBindingPat const *temp = pinfo->temp;
        struct HirPat *lhs = HIR_CAST_PAT(pinfo->original);
        struct HirExpr *rhs = new_unary_path_expr(L, temp->ident, next_node_id(L), HIR_PATH_LOCAL, temp->id);
        HirStmtList_push(L->hir, L->stmts, NEW_NODE(L, let_stmt, temp->span, next_node_id(L), lhs, NULL, rhs));
    }

    return NULL;
}

static struct HirStmt *LowerLetStmt(struct LowerAst *L, struct AstLetStmt *s)
{
    struct HirPat *pat = lower_pat(L, s->pat);
    struct HirType *tag = s->tag != NULL ? lower_type(L, s->tag) : NULL;
    struct HirExpr *init = s->init != NULL ? lower_expr(L, s->init) : NULL;

    if (HirIsBindingPat(pat)) {
        return NEW_NODE(L, let_stmt, s->span, s->id, pat, tag, init);
    } else if (s->init != NULL) {
        return unpack_bindings(L, pat, init, tag, s->id);
    } else {
        LOWERING_ERROR(L, uninitialized_destructuring, s->span.start);
    }
}

static struct HirStmt *LowerExprStmt(struct LowerAst *L, struct AstExprStmt *s)
{
    struct HirExpr *expr = lower_expr(L, s->expr);
    return NEW_NODE(L, expr_stmt, s->span, s->id, expr);
}

static struct HirExpr *LowerLoopExpr(struct LowerAst *L, struct AstLoopExpr *e)
{
    struct HirExpr *result = unit_lit(L, e->span);
    struct HirExpr *body = lower_expr(L, e->block);
    return NEW_NODE(L, loop_expr, e->span, e->id, body);
}

static struct HirExpr *LowerWhileExpr(struct LowerAst *L, struct AstWhileExpr *e)
{
    struct HirStmtList *stmts = HirStmtList_new(L->hir);

    {
        struct HirExpr *cond = lower_expr(L, e->cond);
        struct HirExpr *then_arm = lower_expr(L, e->block);
        struct HirStmtList *else_stmts = HirStmtList_new(L->hir);
        struct HirExpr *jump = NEW_NODE(L, jump_expr, e->span, next_node_id(L), JUMP_BREAK);
        HirStmtList_push(L->hir, else_stmts, NEW_NODE(L, expr_stmt, e->span, next_node_id(L), jump));

        struct HirExpr *else_arm = NEW_NODE(L, block, e->span, next_node_id(L), else_stmts, unit_lit(L, e->span));
        struct HirExpr *check = new_boolean_match(L, e->span, cond, then_arm, else_arm);
        HirStmtList_push(L->hir, stmts, NEW_NODE(L, expr_stmt, e->span, next_node_id(L), check));
    }

    struct HirExpr *result = unit_lit(L, e->span);
    struct HirExpr *body = NEW_NODE(L, block, e->span, next_node_id(L), stmts, result);
    return NEW_NODE(L, loop_expr, e->span, e->id, body);
}

static NodeId option_id(struct LowerAst *L, int discr)
{
    NodeId const option_id = builtin_id(L, BUILTIN_OPTION);
    struct AstAdtDecl const *option = pawAst_get_node(L->ast, option_id);
    struct AstDecl *variant = AstDeclList_get(option->variants, discr);
    return variant->hdr.id;
}

static struct HirPat *new_some_pat(struct LowerAst *L, struct HirPat *pat, struct SourceSpan span)
{
    NodeId const first_id = next_node_id(L);
    NodeId const second_id = next_node_id(L);
    NodeId const option_id = builtin_id(L, BUILTIN_OPTION);
    NodeId const some_id = variant_id(L, option_id, PAW_OPTION_SOME);

    struct HirSegments *segments = HirSegments_new(L->hir);
    pawHir_add_segment(L->hir, segments, span, first_id, make_ident(CSTR(L->C, CSTR_OPTION), span), NULL, option_id);
    pawHir_add_segment(L->hir, segments, span, second_id, make_ident(SCAN_STR(L->C, "Some"), span), NULL, some_id);
    struct HirPath path = pawHir_path_create(span, segments, HIR_PATH_ASSOC);
    struct HirPatList *fields = HirPatList_new(L->hir);
    struct HirPat *variant = NEW_NODE(L, variant_pat, span, next_node_id(L), path, fields, 0);
    HirPatList_push(L->hir, fields, pat);
    return variant;
}

static struct HirPat *binding_pat(struct LowerAst *L, struct SourceSpan span, struct HirIdent ident, NodeId id)
{
    return NEW_NODE(L, binding_pat, span, id, ident);
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
//         let (iter) = iterable.iterator();
//         loop {
//             match (iter).next() {
//                 Option::Some(i) => {
//                     ...
//                 },
//                 _ => break,
//             }
//         }
//     }
//
static struct HirExpr *LowerForExpr(struct LowerAst *L, struct AstForExpr *e)
{
    struct Hir *hir = L->hir;
    struct HirStmtList *outer_stmts = HirStmtList_new(hir);
    struct HirIdent const iter_ident = {
        .name = SCAN_STR(L->C, "iterator"),
        .span = e->target->hdr.span,
    };
    struct HirIdent const var_ident = {
        .name = SCAN_STR(L->C, PRIVATE("iter")),
        .span = e->target->hdr.span,
    };

    NodeId const var_id = next_node_id(L);
    struct HirPat *binding = binding_pat(L, e->span, var_ident, var_id);
    {
        // evaluate "iterable.iterator()" and store the result in a local variable
        struct HirExpr *iterable = lower_expr(L, e->target);
        struct HirExpr *target = NEW_NODE(L, name_selector, e->span, next_node_id(L), iterable, iter_ident);
        struct HirExpr *iterator = NEW_NODE(L, call_expr, e->span, next_node_id(L), target, HirExprList_new(hir));
        struct HirStmt *stmt = NEW_NODE(L, let_stmt, e->span, next_node_id(L), binding, NULL, iterator);
        HirStmtList_push(hir, outer_stmts, stmt);
    }

    struct HirExprList *arms = HirExprList_new(hir);
    {
        // create the "Some(i) => {...}" arm
        struct HirPat *pat = new_some_pat(L, lower_pat(L, e->pat), e->span);
        struct HirExpr *arm = NEW_NODE(L, match_arm, e->span, next_node_id(L),
                pat, NULL, lower_expr(L, e->block));
        HirExprList_push(hir, arms, arm);
    }

    {
        // create the "_ => break" arm
        struct HirPat *pat = pawHir_new_wildcard_pat(L->hir, e->span, next_node_id(L));
        struct HirExpr *rhs = NEW_NODE(L, jump_expr, e->span, next_node_id(L), JUMP_BREAK);
        struct HirExpr *arm = NEW_NODE(L, match_arm, e->span, next_node_id(L), pat, NULL, rhs);
        HirExprList_push(hir, arms, arm);
    }

    struct HirStmtList *inner_stmts = HirStmtList_new(hir);
    struct HirExpr *body = NEW_NODE(L, block, e->span, next_node_id(L), inner_stmts, unit_lit(L, e->span));
    struct HirExpr *loop = NEW_NODE(L, loop_expr, e->span, next_node_id(L), body);
    struct HirExpr *outer = NEW_NODE(L, block, e->span, next_node_id(L), outer_stmts, loop);

    struct HirExpr *next;
    {
        // call "next" on the iterator created earlier (result of evaluating "e->target")
        struct HirExpr *iterator = new_unary_path_expr(L, var_ident, next_node_id(L), HIR_PATH_LOCAL, var_id);
        struct HirIdent next_ident = {.span = iter_ident.span, .name = SCAN_STR(L->C, "next")};
        struct HirExpr *target = NEW_NODE(L, name_selector, e->span, next_node_id(L), iterator, next_ident);
        next = NEW_NODE(L, call_expr, e->span, next_node_id(L), target, HirExprList_new(hir));
    }

    {
        // match on the "Option<T>" returned by "(iter).next()"
        struct HirExpr *match = NEW_NODE(L, match_expr, e->span, next_node_id(L), next, arms, PAW_TRUE);
        HirStmtList_push(hir, inner_stmts, NEW_NODE(L, expr_stmt, e->span, next_node_id(L), match));
    }

    return outer;
}

static struct HirExpr *LowerIndex(struct LowerAst *L, struct AstIndex *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExpr *index = lower_expr(L, e->index);
    return NEW_NODE(L, index_expr, e->span, e->id, target, index);
}

static struct HirExpr *LowerSelector(struct LowerAst *L, struct AstSelector *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    if (e->is_index)
        return NEW_NODE(L, index_selector, e->span, e->id, target, e->index);
    struct HirIdent const ident = lower_ident(L, e->ident);
    return NEW_NODE(L, name_selector, e->span, e->id, target, ident);
}

static struct HirExpr *LowerJumpExpr(struct LowerAst *L, struct AstJumpExpr *e)
{
    return NEW_NODE(L, jump_expr, e->span, e->id, e->jump_kind);
}

static struct HirStmt *LowerDeclStmt(struct LowerAst *L, struct AstDeclStmt *s)
{
    struct HirDecl *decl = lower_decl(L, s->decl);
    return NEW_NODE(L, decl_stmt, s->span, s->id, decl);
}

static struct HirBoundList *lower_bounds(struct LowerAst *L, struct AstBoundList *bounds)
{
    if (bounds == NULL) return NULL;
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
    struct HirIdent const ident = lower_ident(L, d->ident);
    struct HirBoundList *bounds = lower_bounds(L, d->bounds);
    return NEW_NODE(L, generic_decl, d->span, d->id, d->did, ident, bounds);
}

static struct HirType *LowerPathType(struct LowerAst *L, struct AstPathType *t)
{
    struct HirPath const path = lower_path(L, t->path);
    return NEW_NODE(L, path_type, t->span, t->id, path);
}

static struct HirType *LowerContainerType(struct LowerAst *L, struct AstContainerType *t)
{
    struct HirType *first = lower_type(L, t->first);
    if (t->second == NULL) return new_list_t(L, t->span, first);
    struct HirType *second = lower_type(L, t->second);
    return new_map_t(L, t->span, first, second);
}

static struct HirType *LowerTupleType(struct LowerAst *L, struct AstTupleType *t)
{
    if (t->types->count == 0) return unit_type(L, t->span);
    struct HirTypeList *elems = lower_type_list(L, t->types);
    return NEW_NODE(L, tuple_type, t->span, t->id, elems);
}

static struct HirType *LowerFnType(struct LowerAst *L, struct AstFnType *t)
{
    struct HirTypeList *params = lower_type_list(L, t->params);
    struct HirType *result = t->result != NULL ? lower_type(L, t->result)
        : unit_type(L, (struct SourceSpan){0}); // TODO: see TODO in LowerFnDecl
    return NEW_NODE(L, fn_ptr, t->span, t->id, params, result);
}

static struct HirType *LowerNeverType(struct LowerAst *L, struct AstNeverType *t)
{
    return NEW_NODE(L, never_type, t->span, t->id);
}

static struct HirType *LowerInferType(struct LowerAst *L, struct AstInferType *t)
{
    return NEW_NODE(L, infer_type, t->span, t->id);
}

static struct HirPat *LowerOrPat(struct LowerAst *L, struct AstOrPat *p)
{
    struct HirPatList *pats = lower_pat_list(L, p->pats);
    return NEW_NODE(L, or_pat, p->span, p->id, pats);
}

static struct HirPat *LowerFieldPat(struct LowerAst *L, struct AstFieldPat *p)
{
    struct HirIdent const ident = lower_ident(L, p->ident);
    struct HirPat *pat = lower_pat(L, p->pat);
    return NEW_NODE(L, field_pat, p->span, p->id, ident, pat, -1);
}

static struct HirPat *LowerStructPat(struct LowerAst *L, struct AstStructPat *p)
{
    struct HirPath const path = lower_path(L, p->path);
    struct HirPatList *fields = lower_pat_list(L, p->fields);
    return NEW_NODE(L, struct_pat, p->span, p->id, path, fields);
}

static struct HirPat *LowerVariantPat(struct LowerAst *L, struct AstVariantPat *p)
{
    struct HirPath const path = lower_path(L, p->path);
    struct HirPatList *fields = lower_pat_list(L, p->fields);
    return NEW_NODE(L, variant_pat, p->span, p->id, path, fields, -1);
}

static struct HirPat *LowerTuplePat(struct LowerAst *L, struct AstTuplePat *p)
{
    struct HirPatList *elems = lower_pat_list(L, p->elems);
    return NEW_NODE(L, tuple_pat, p->span, p->id, elems);
}

static struct HirPat *LowerIdentPat(struct LowerAst *L, struct AstIdentPat *p)
{
    struct AstSegment *psrc;
    struct HirIdent const ident = lower_ident(L, p->ident);
    struct ResolvedSegment const *res = SegmentTable_get(L->C, L->segtab, p->id);
    paw_assert(res != NULL);

    switch (res->kind) {
        case RESOLVED_LOCAL: {
            // NOTE: If "p" is part of an OR pattern, then "segment.target" contains the NodeId
            //       of the binding defined in the first alternative of the OR pattern (equal
            //       to "p->id" if "p" itself is in the first alternative). This is necessary
            //       for when bindings are assigned to registers during HIR lowering, since
            //       the result part of a match arm containing an OR pattern is copied for
            //       each alternative, and paths in the result part can refer to bindings in
            //       the first alternative. The NodeID-to-register mapping is updated before
            //       each copied result part is lowered to support patterns with bindings in
            //       different positions, like "(1, v) | (v, 2)", etc.
            struct HirPat *pat = NEW_NODE(L, binding_pat, p->span, p->id, ident);
            pat->hdr.id = res->id;
            return pat;
        }
        default: {
            struct AstDecl *decl = pawAst_get_node(L->ast, res->id);
            if (AstIsParamDecl(decl))
                return NEW_NODE(L, binding_pat, p->span, p->id, ident);

            HirSegments *segments = HirSegments_new(L->hir);
            pawHir_add_segment(L->hir, segments, p->span, next_node_id(L), ident, NULL, res->id);
            struct HirPath const path = pawHir_path_create(ident.span, segments, HIR_PATH_ITEM);
            if (AstIsAdtDecl(decl))
                return NEW_NODE(L, struct_pat, p->span, p->id, path, HirPatList_new(L->hir));

            int const discr = AstGetVariantDecl( pawAst_get_node(L->ast, res->id))->index;
            return NEW_NODE(L, variant_pat, p->span, p->id, path, HirPatList_new(L->hir), discr);
        }
    }
}

static struct HirPat *LowerPathPat(struct LowerAst *L, struct AstPathPat *p)
{
    struct HirPath const path = lower_path(L, p->path);
    switch (path.kind) {
        case HIR_PATH_LOCAL: {
            // NOTE: If "p" is part of an OR pattern, then "segment.target" contains the NodeId
            //       of the binding defined in the first alternative of the OR pattern (equal
            //       to "p->id" if "p" itself is in the first alternative). This is necessary
            //       for when bindings are assigned to registers during HIR lowering, since
            //       the result part of a match arm containing an OR pattern is copied for each
            //       alternative, and paths in the result part might refer to bindings created
            //       in the first alternative. The NodeID-to-register mapping is updated before
            //       each copied result part is lowered to support patterns with bindings in
            //       different positions, like "(1, v) | (v, 2)", etc.
            struct HirSegment const segment = K_LIST_FIRST(path.segments);
            struct HirPat *pat = NEW_NODE(L, binding_pat, p->span, p->id, segment.ident);
            pat->hdr.id = segment.target;
            return pat;
        }
        case HIR_PATH_ITEM: {
            struct HirSegment const segment = K_LIST_FIRST(path.segments);
            struct AstDecl *decl = pawAst_get_node(L->ast, segment.target);
            if (AstIsParamDecl(decl)) {
                return NEW_NODE(L, binding_pat, p->span, p->id, segment.ident);
            } else if (AstIsAdtDecl(decl)) {
                return NEW_NODE(L, struct_pat, p->span, p->id, path, HirPatList_new(L->hir));
            }
            // fallthrough
        }
        case HIR_PATH_ASSOC: {
            NodeId const variant_id = K_LIST_LAST(path.segments).target;
            int const discr = AstGetVariantDecl(
                    pawAst_get_node(L->ast, variant_id))->index;
            HirPatList *empty = HirPatList_new(L->hir);
            return NEW_NODE(L, variant_pat, p->span, p->id, path, empty, discr);
        }
    }
}

static struct HirPat *LowerLiteralPat(struct LowerAst *L, struct AstLiteralPat *p)
{
    if (AstIsLiteralExpr(p->expr)) {
        // Special case for integers: normal lowering functions assume integers are positive and
        // have type paw_Uint so they can handle overflow (UnOp(Literal(i), -) is detected and
        // converted into Literal(-i) after checking for overflow).
        struct AstLiteralExpr *e = AstGetLiteralExpr(p->expr);
        if (e->lit_kind == kAstBasicLit && e->basic.code == BUILTIN_INT) {
            struct HirExpr *expr = NEW_NODE(L, basic_lit, p->span, e->id, e->basic.value, BUILTIN_INT);
            return NEW_NODE(L, literal_pat, p->span, p->id, expr);
        }
    }
    struct HirExpr *expr = lower_expr(L, p->expr);
    return NEW_NODE(L, literal_pat, p->span, p->id, expr);
}

static struct HirPat *LowerWildcardPat(struct LowerAst *L, struct AstWildcardPat *p)
{
    return NEW_NODE(L, wildcard_pat, p->span, p->id);
}

static struct HirDecl *LowerModuleDecl(struct LowerAst *L, struct AstModuleDecl *d)
{
    L->m = d; // focus on this module
    struct HirDeclList *items = lower_decl_list(L, d->items);

    struct HirModule const m = {
        .modno = d->modno,
        .name = d->name,
        .items = items,
    };
    HirModuleList_push(L->hir, L->hir->modules, m);
    return NULL; // no corresponding HIR node
}

static struct HirDecl *lower_decl(struct LowerAst *L, struct AstDecl *decl)
{
    struct HirDecl *result;
    switch (AST_KINDOF(decl)) {
#define DEFINE_CASE(X)                             \
        case kAst##X:                              \
            result = Lower##X(L, AstGet##X(decl)); \
            break;
        AST_DECL_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
    return result;
}

static struct HirStmt *lower_stmt(struct LowerAst *L, struct AstStmt *stmt)
{
    switch (AST_KINDOF(stmt)) {
#define DEFINE_CASE(X)     \
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
#define DEFINE_CASE(X)     \
        case kAst##X:      \
            return Lower##X(L, AstGet##X(pat));
        AST_PAT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirType *lower_type(struct LowerAst *L, struct AstType *type)
{
    switch (AST_KINDOF(type)) {
#define DEFINE_CASE(X)     \
        case kAst##X:      \
            return Lower##X(L, AstGet##X(type));
        AST_TYPE_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

void pawP_lower_ast(struct Compiler *C)
{
    paw_Env *P = ENV(C);
    struct Ast *ast = C->ast;
    struct LowerAst L = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .segtab = C->segtab,
        .ast = ast,
        .P = P,
        .C = C,
    };

    L.hir = C->hir = pawHir_new(C);
    L.hir->node_count = ast->node_count;
    lower_decl_list(&L, C->ast->modules);

    if (pawP_push_callback(C, "paw.on_build_hir")) {
        paw_push_rawptr(P, L.hir);
        paw_call(P, 1);
    }

    // release AST memory
    pawAst_free(ast);
}
