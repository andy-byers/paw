// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ast.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "env.h"
#include "gc.h"
#include "hir.h"
#include "map.h"
#include "mem.h"
#include "opcode.h"
#include "parse.h"
#include "str.h"
#include "unify.h"

struct BlockState {
    struct BlockState *outer;
    paw_Bool propagate;
    paw_Bool never;
};

struct LowerAst {
    struct DynamicMem *dm;
    struct BlockState *bs;
    struct Compiler *C;
    struct Hir *hir;
    paw_Env *P;
    int line;
};

DEFINE_MAP_ITERATOR(ImportMap, String *, struct Ast *)

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
        struct Hir##T##List *r = Hir##T##List_new(L->hir);                                           \
        for (int i = 0; i < list->count; ++i) {                                                    \
            struct Hir##T *node = lower_##name(L, list->data[i]);                                  \
            if (node != NULL)                                                                      \
                Hir##T##List_push(L->hir, r, node);                                                  \
        }                                                                                          \
        return r;                                                                                  \
    }
DEFINE_LOWER_LIST(expr, Expr)
DEFINE_LOWER_LIST(decl, Decl)
DEFINE_LOWER_LIST(stmt, Stmt)
DEFINE_LOWER_LIST(type, Type)
DEFINE_LOWER_LIST(pat, Pat)

// Mark the enclosing block as containing an unconditional jump
// During the type checking pass, such blocks are allowed to evaluate to "()",
// even if a sibling block (in an IfExpr or MatchExpr) evaluates to a different
// type.
static void indicate_jump(struct LowerAst *L)
{
    L->bs->never = PAW_TRUE;
}

static void enter_block(struct LowerAst *L, struct BlockState *bs, paw_Bool propagate)
{
    *bs = (struct BlockState){
        .propagate = propagate,
        .outer = L->bs,
    };
    L->bs = bs;
}

static void leave_block(struct LowerAst *L)
{
    struct BlockState *bs = L->bs;
    if (bs->propagate && bs->outer != NULL) {
        bs->outer->never = bs->never;
    }
    L->bs = bs->outer;
}

static struct HirPath *new_unary_path(struct LowerAst *L, String *name)
{
    struct HirPath *path = HirPath_new(L->hir);
    HirPath_add(L->hir, path, name, NULL);
    return path;
}

static struct HirExpr *new_unary_path_expr(struct LowerAst *L, int line, String *name)
{
    struct HirPath *path = new_unary_path(L, name);
    return pawHir_new_path_expr(L->hir, line, path);
}

static struct HirExpr *lower_block(struct LowerAst *L, struct AstBlock *e, paw_Bool propagate)
{
    struct BlockState bs;
    enter_block(L, &bs, propagate);

    struct HirStmtList *stmts = lower_stmt_list(L, e->stmts);
    struct HirExpr *result = lower_expr(L, e->result);
    struct HirExpr *r = pawHir_new_block(L->hir, e->line, stmts, result, bs.never);

    leave_block(L);
    return r;
}

static struct HirExpr *LowerBlock(struct LowerAst *L, struct AstBlock *block)
{
    return lower_block(L, block, PAW_TRUE);
}

#define LOWER_BLOCK(L, block) lower_block(L, AstGetBlock(block), PAW_FALSE)

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
    return pawHir_new_field_decl(L->hir, d->line, d->name, tag, d->is_pub);
}

static struct HirDecl *LowerVariantDecl(struct LowerAst *L, struct AstVariantDecl *d)
{
    struct HirDeclList *fields = lower_decl_list(L, d->fields);
    return pawHir_new_variant_decl(L->hir, d->line, d->name, fields, d->index);
}

static struct HirDeclList *lower_fields(struct LowerAst *L, struct AstDeclList *src, String *parent)
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
    indicate_jump(L); // unconditional jump
    return pawHir_new_return_expr(L->hir, e->line, expr);
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

static struct HirPath *lower_path(struct LowerAst *L, struct AstPath *path)
{
    paw_assert(path->count > 0);
    struct HirPath *r = HirPath_new(L->hir);
    for (int i = 0; i < path->count; ++i) {
        struct AstSegment src = AstPath_get(path, i);
        struct HirTypeList *types = lower_type_list(L, src.types);

        struct HirSegment dst;
        pawHir_init_segment(L->hir, &dst, src.name, types);
        HirPath_push(L->hir, r, dst);
    }
    return r;
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct HirExpr *LowerPathExpr(struct LowerAst *L, struct AstPathExpr *e)
{
    struct HirPath *path = lower_path(L, e->path);
    return pawHir_new_path_expr(L->hir, e->line, path);
}

static struct HirExpr *LowerLogicalExpr(struct LowerAst *L, struct AstLogicalExpr *e)
{
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return pawHir_new_logical_expr(L->hir, e->line, lhs, rhs, e->is_and);
}

static struct HirExpr *LowerChainExpr(struct LowerAst *L, struct AstChainExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    return pawHir_new_chain_expr(L->hir, e->line, target);
}

static struct HirExpr *LowerUnOpExpr(struct LowerAst *L, struct AstUnOpExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    return pawHir_new_unop_expr(L->hir, e->line, target, e->op);
}

static struct HirExpr *LowerBinOpExpr(struct LowerAst *L, struct AstBinOpExpr *e)
{
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return pawHir_new_binop_expr(L->hir, e->line, lhs, rhs, e->op);
}

static struct HirExpr *LowerAssignExpr(struct LowerAst *L, struct AstAssignExpr *e)
{
    struct HirExpr *lhs = lower_expr(L, e->lhs);
    if (!HirIsPathExpr(lhs) && !HirIsIndex(lhs) && !HirIsSelector(lhs)) {
        SYNTAX_ERROR(L, "invalid place for assignment");
    }
    struct HirExpr *rhs = lower_expr(L, e->rhs);
    return pawHir_new_assign_expr(L->hir, e->line, lhs, rhs);
}

static struct HirExpr *LowerMatchExpr(struct LowerAst *L, struct AstMatchExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExprList *arms = lower_expr_list(L, e->arms);
    paw_assert(arms->count > 0);

    // propagate "never" flag to enclosing block
    paw_Bool never = PAW_TRUE;
    struct HirExpr **pexpr;
    K_LIST_FOREACH (arms, pexpr) {
        struct HirMatchArm *arm = HirGetMatchArm(*pexpr);
        never = arm->never;
        if (!never)
            break;
    }
    if (never)
        indicate_jump(L);
    return pawHir_new_match_expr(L->hir, e->line, target, arms, never);
}

static struct HirExpr *LowerMatchArm(struct LowerAst *L, struct AstMatchArm *e)
{
    struct HirPat *pat = lower_pat(L, e->pat);
    struct HirExpr *guard = e->guard != NULL ? lower_expr(L, e->guard) : NULL;

    // wrap in a block to catch return or jump expressions not enclosed
    // in curly braces
    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);
    struct HirExpr *result = lower_expr(L, e->result);
    leave_block(L);

    return pawHir_new_match_arm(L->hir, e->line, pat, guard, result, bs.never);
}

static struct HirType *new_list_t(struct LowerAst *L, struct HirType *elem_t)
{
    struct HirTypeList *types = HirTypeList_new(L->hir);
    HirTypeList_push(L->hir, types, elem_t);

    struct HirPath *path = HirPath_new(L->hir);
    HirPath_add(L->hir, path, CSTR(L, CSTR_LIST), types);
    return pawHir_new_path_type(L->hir, elem_t->hdr.line, path);
}

static struct HirType *new_map_t(struct LowerAst *L, struct HirType *key_t, struct HirType *value_t)
{
    struct HirTypeList *types = HirTypeList_new(L->hir);
    HirTypeList_push(L->hir, types, key_t);
    HirTypeList_push(L->hir, types, value_t);

    struct HirPath *path = HirPath_new(L->hir);
    HirPath_add(L->hir, path, CSTR(L, CSTR_MAP), types);
    return pawHir_new_path_type(L->hir, key_t->hdr.line, path);
}

static struct HirDecl *lower_closure_param(struct LowerAst *L, struct AstFieldDecl *d)
{
    struct HirType *tag = d->tag != NULL ? lower_type(L, d->tag) : NULL;
    return pawHir_new_field_decl(L->hir, d->line, d->name, tag, d->is_pub);
}

static struct HirExpr *LowerClosureExpr(struct LowerAst *L, struct AstClosureExpr *e)
{
    struct HirDeclList *params = HirDeclList_new(L->hir);
    for (int i = 0; i < e->params->count; ++i) {
        struct AstFieldDecl *src = AstGetFieldDecl(e->params->data[i]);
        struct HirDecl *dst = lower_closure_param(L, src);
        HirDeclList_push(L->hir, params, dst);
    }
    struct HirType *result = e->result != NULL ? lower_type(L, e->result) : NULL;
    struct HirExpr *expr = lower_expr(L, e->expr);
    return pawHir_new_closure_expr(L->hir, e->line, params, result, expr);
}

static struct HirDecl *LowerUseDecl(struct LowerAst *L, struct AstUseDecl *d)
{
    HirImportList_push(L->hir, L->hir->imports, ((struct HirImport){
                                                  .has_star = d->has_star,
                                                  .modno = d->modno,
                                                  .item = d->item,
                                                  .as = d->as,
                                              }));
    return NULL;
}

static struct HirDecl *LowerAdtDecl(struct LowerAst *L, struct AstAdtDecl *d)
{
    struct HirTypeList *traits = lower_type_list(L, d->traits);
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *fields = lower_fields(L, d->fields, d->name);
    struct HirDeclList *methods = lower_methods(L, d->methods);
    return pawHir_new_adt_decl(L->hir, d->line, d->name, NULL, traits,
                               generics, fields, methods, d->is_pub, d->is_struct);
}

static struct HirDecl *LowerTraitDecl(struct LowerAst *L, struct AstTraitDecl *d)
{
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *methods = lower_methods(L, d->methods);
    return pawHir_new_trait_decl(L->hir, d->line, d->name, NULL,
                                 generics, methods, d->is_pub);
}

static struct HirDecl *LowerVarDecl(struct LowerAst *L, struct AstVarDecl *d)
{
    struct HirPat *pat = lower_pat(L, d->pat);
    struct HirType *tag = d->tag != NULL ? lower_type(L, d->tag) : NULL;
    struct HirExpr *init = d->init != NULL ? lower_expr(L, d->init) : NULL;
    return pawHir_new_var_decl(L->hir, d->line, d->name, pat, tag, init);
}

static struct HirDecl *LowerConstDecl(struct LowerAst *L, struct AstConstDecl *d)
{
    struct HirType *tag = lower_type(L, d->tag);
    struct HirExpr *init = d->init != NULL ? lower_expr(L, d->init) : NULL;
    return pawHir_new_const_decl(L->hir, d->line, d->name, d->annos, tag, init, d->is_pub);
}

static struct HirDecl *LowerTypeDecl(struct LowerAst *L, struct AstTypeDecl *d)
{
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirType *rhs = lower_type(L, d->rhs);
    return pawHir_new_type_decl(L->hir, d->line, d->name, generics, rhs, d->is_pub);
}

// Lower a function call or enumerator constructor
static struct HirExpr *LowerCallExpr(struct LowerAst *L, struct AstCallExpr *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExprList *args = lower_expr_list(L, e->args);
    return pawHir_new_call_expr(L->hir, e->line, target, args);
}

static struct HirExpr *LowerConversionExpr(struct LowerAst *L, struct AstConversionExpr *e)
{
    struct HirExpr *from = lower_expr(L, e->arg);
    return pawHir_new_conversion_expr(L->hir, e->line, from, e->to);
}

static struct HirExpr *lower_basic_lit(struct LowerAst *L, struct AstBasicLit *e, int line)
{
    return pawHir_new_basic_lit(L->hir, line, e->value, e->code);
}

static struct HirExpr *lower_tuple_lit(struct LowerAst *L, struct AstTupleLit *e, int line)
{
    struct HirExprList *elems = lower_expr_list(L, e->elems);
    return pawHir_new_tuple_lit(L->hir, line, elems);
}

static struct HirExpr *lower_list_lit(struct LowerAst *L, struct AstContainerLit *e, int line)
{
    paw_assert(e->code == BUILTIN_LIST);

    struct AstExpr **psrc;
    struct HirExprList *items = HirExprList_new(L->hir);
    K_LIST_FOREACH (e->items, psrc) {
        struct HirExpr *dst = lower_expr(L, *psrc);
        HirExprList_push(L->hir, items, dst);
    }
    return pawHir_new_container_lit(L->hir, line, items, BUILTIN_LIST);
}

static struct HirExpr *lower_map_lit(struct LowerAst *L, struct AstContainerLit *e, int line)
{
    paw_assert(e->code == BUILTIN_MAP);

    struct AstExpr **psrc;
    struct HirExprList *items = HirExprList_new(L->hir);
    K_LIST_FOREACH (e->items, psrc) {
        struct HirExpr *dst = lower_expr(L, *psrc);
        paw_assert(HirGetFieldExpr(dst)->fid == -1);
        HirExprList_push(L->hir, items, dst);
    }
    return pawHir_new_container_lit(L->hir, line, items, BUILTIN_MAP);
}

static struct HirExpr *lower_container_lit(struct LowerAst *L, struct AstContainerLit *e, int line)
{
    if (e->code == BUILTIN_LIST)
        return lower_list_lit(L, e, line);
    return lower_map_lit(L, e, line);
}

static struct HirExpr *LowerFieldExpr(struct LowerAst *L, struct AstFieldExpr *e)
{
    struct HirExpr *value = lower_expr(L, e->value);
    if (e->fid < 0) {
        struct HirExpr *key = lower_expr(L, e->key);
        return pawHir_new_keyed_field_expr(L->hir, e->line, key, value);
    }
    return pawHir_new_named_field_expr(L->hir, e->line, e->name, value, e->fid);
}

static struct HirExpr *lower_composite_lit(struct LowerAst *L, struct AstCompositeLit *e, int line)
{
    struct HirPath *path = lower_path(L, e->path);

    struct AstExpr **psrc;
    struct HirExprList *items = HirExprList_new(L->hir);
    K_LIST_FOREACH (e->items, psrc)
        HirExprList_push(L->hir, items, lower_expr(L, *psrc));
    return pawHir_new_composite_lit(L->hir, line, path, items);
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
            return lower_basic_lit(L, &e->basic, e->line);
        case kAstTupleLit:
            return lower_tuple_lit(L, &e->tuple, e->line);
        case kAstContainerLit:
            return lower_container_lit(L, &e->cont, e->line);
        case kAstCompositeLit:
            return lower_composite_lit(L, &e->comp, e->line);
    }
}

static struct HirDecl *LowerFuncDecl(struct LowerAst *L, struct AstFuncDecl *d)
{
    struct HirDeclList *generics = lower_decl_list(L, d->generics);
    struct HirDeclList *params = lower_params(L, d, d->params);
    struct HirType *result = lower_type(L, d->result);
    struct HirExpr *body = d->body != NULL ? LOWER_BLOCK(L, d->body) : NULL;
    return pawHir_new_func_decl(L->hir, d->line, d->name, d->annos, generics,
                                params, result, body, d->fn_kind, d->is_pub, PAW_FALSE);
}

static paw_Bool is_never_block(struct HirExpr *expr)
{
    if (HirIsBlock(expr))
        return HirGetBlock(expr)->never;
    return HirGetIfExpr(expr)->never;
}

static struct HirExpr *LowerIfExpr(struct LowerAst *L, struct AstIfExpr *e)
{
    struct HirExpr *cond = lower_expr(L, e->cond);
    struct HirExpr *then_arm = LOWER_BLOCK(L, e->then_arm);
    struct HirExpr *else_arm = NULL;
    paw_Bool never = PAW_FALSE;
    if (e->else_arm != NULL) {
        else_arm = AstIsBlock(e->else_arm)
                       ? LOWER_BLOCK(L, e->else_arm)
                       : lower_expr(L, e->else_arm);

        if (is_never_block(then_arm) && is_never_block(else_arm)) {
            // all paths through this IfExpr execute a jump
            never = PAW_TRUE;
            indicate_jump(L);
        }
    }
    return pawHir_new_if_expr(L->hir, e->line, cond, then_arm, else_arm, never);
}

static struct HirStmt *LowerExprStmt(struct LowerAst *L, struct AstExprStmt *s)
{
    struct HirExpr *expr = lower_expr(L, s->expr);
    return pawHir_new_expr_stmt(L->hir, s->line, expr);
}

static struct HirExpr *unit_lit(struct LowerAst *L, int line)
{
    return pawHir_new_basic_lit(L->hir, line, P2V(NULL), BUILTIN_UNIT);
}

static struct HirExpr *LowerWhileExpr(struct LowerAst *L, struct AstWhileExpr *e)
{
    struct HirStmtList *stmts = HirStmtList_new(L->hir);

    {
        struct HirExpr *cond = lower_expr(L, e->cond);
        struct HirExpr *then_arm = lower_expr(L, e->block);
        struct HirExpr *else_arm = pawHir_new_jump_expr(L->hir, e->line, JUMP_BREAK);
        struct HirExpr *check = pawHir_new_if_expr(L->hir, e->line, cond, then_arm, else_arm, PAW_FALSE);
        struct HirStmt *stmt = pawHir_new_expr_stmt(L->hir, e->line, check);
        HirStmtList_push(L->hir, stmts, stmt);
    }

    struct HirExpr *result = unit_lit(L, e->line);
    struct HirExpr *body = pawHir_new_block(L->hir, e->line, stmts, result, PAW_FALSE);
    return pawHir_new_loop_expr(L->hir, e->line, body);
}

static struct HirPat *new_none_pat(struct LowerAst *L, int line)
{
    struct HirPath *path = HirPath_new(L->hir);
    HirPath_add(L->hir, path, CSTR(L->C, CSTR_OPTION), NULL);
    HirPath_add(L->hir, path, SCAN_STRING(L->C, "None"), NULL);
    struct HirPatList *fields = HirPatList_new(L->hir);
    return pawHir_new_variant_pat(L->hir, line, path, fields, 1);
}

static struct HirPat *new_some_pat(struct LowerAst *L, struct HirPat *pat, int line)
{
    struct HirPath *path = HirPath_new(L->hir);
    HirPath_add(L->hir, path, CSTR(L->C, CSTR_OPTION), NULL);
    HirPath_add(L->hir, path, SCAN_STRING(L->C, "Some"), NULL);
    struct HirPatList *fields = HirPatList_new(L->hir);
    struct HirPat *variant = pawHir_new_variant_pat(L->hir, line, path, fields, 0);
    HirPatList_push(L->hir, fields, pat);
    return variant;
}

static struct HirPat *binding_pat(struct LowerAst *L, int line, String *name)
{
    return pawHir_new_binding_pat(L->hir, line, name);
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
    String *iter_name = SCAN_STRING(L->C, PRIVATE("iterator"));
    {
        // evaluate "iterable.iterator()" and store the result in a local variable
        struct HirExpr *iterable = lower_expr(L, e->target);
        struct HirExpr *target = pawHir_new_name_selector(hir, e->line, iterable, SCAN_STRING(L->C, "iterator"));
        struct HirExpr *iterator = pawHir_new_call_expr(hir, e->line, target, HirExprList_new(hir));
        struct HirDecl *decl = pawHir_new_var_decl(hir, e->line, iter_name,
                binding_pat(L, e->line, iter_name), NULL, iterator);
        struct HirStmt *stmt = pawHir_new_decl_stmt(hir, e->line, decl);
        HirStmtList_push(hir, outer_stmts, stmt);
    }

    struct HirExprList *arms = HirExprList_new(hir);
    {
        // create the "Some(i) => {...}" arm
        struct HirPat *pat = new_some_pat(L, lower_pat(L, e->pat), e->line);
        struct HirExpr *arm = pawHir_new_match_arm(hir, e->line, pat,
                NULL, LOWER_BLOCK(L, e->block), PAW_FALSE);
        HirExprList_push(hir, arms, arm);
    }

    {
        // create the "None => break" arm
        struct HirPat *pat = new_none_pat(L, e->line);
        struct HirExpr *rhs = pawHir_new_jump_expr(hir, e->line, JUMP_BREAK);
        struct HirExpr *arm = pawHir_new_match_arm(hir, e->line, pat, NULL, rhs, PAW_TRUE);
        HirExprList_push(hir, arms, arm);
    }

    struct HirStmtList *inner_stmts = HirStmtList_new(hir);
    struct HirExpr *unit = pawHir_new_basic_lit(hir, e->line, I2V(0), BUILTIN_UNIT);
    struct HirExpr *body = pawHir_new_block(hir, e->line, inner_stmts, unit, PAW_FALSE);
    struct HirExpr *loop = pawHir_new_loop_expr(hir, e->line, body);
    struct HirExpr *outer = pawHir_new_block(hir, e->line, outer_stmts, loop, PAW_FALSE);

    struct HirExpr *next;
    {
        // call "next" on the iterator created earlier (result of evaluating "e->target")
        struct HirExpr *iterator = new_unary_path_expr(L, e->line, iter_name);
        struct HirExpr *target = pawHir_new_name_selector(hir, e->line, iterator, SCAN_STRING(L->C, "next"));
        next = pawHir_new_call_expr(hir, e->line, target, HirExprList_new(hir));
    }

    {
        // match on the "Option<T>" returned by "_iterator.next()"
        struct HirExpr *match = pawHir_new_match_expr(hir, e->line, next, arms, //
                PAW_FALSE /* "None" arm jumps but "Some" arm does not */);
        HirStmtList_push(hir, inner_stmts, pawHir_new_expr_stmt(hir, e->line, match));
    }

    return outer;
}

static struct HirExpr *LowerIndex(struct LowerAst *L, struct AstIndex *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    struct HirExpr *first = NULL;
    struct HirExpr *second = NULL;
    if (e->is_slice) {
        if (e->first != NULL)
            first = lower_expr(L, e->first);
        if (e->second != NULL)
            second = lower_expr(L, e->second);
    } else {
        first = lower_expr(L, e->first);
    }
    return pawHir_new_index_expr(L->hir, e->line, target, first, second, e->is_slice);
}

static struct HirExpr *LowerSelector(struct LowerAst *L, struct AstSelector *e)
{
    struct HirExpr *target = lower_expr(L, e->target);
    if (e->is_index)
        return pawHir_new_index_selector(L->hir, e->line, target, e->index);
    return pawHir_new_name_selector(L->hir, e->line, target, e->name);
}

static struct HirExpr *LowerJumpExpr(struct LowerAst *L, struct AstJumpExpr *e)
{
    indicate_jump(L);
    return pawHir_new_jump_expr(L->hir, e->line, e->jump_kind);
}

static struct HirStmt *LowerDeclStmt(struct LowerAst *L, struct AstDeclStmt *s)
{
    struct HirDecl *decl = lower_decl(L, s->decl);
    return pawHir_new_decl_stmt(L->hir, s->line, decl);
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
    struct HirBoundList *bounds = lower_bounds(L, d->bounds);
    return pawHir_new_generic_decl(L->hir, d->line, d->name, bounds);
}

static struct HirType *LowerPathType(struct LowerAst *L, struct AstPathType *t)
{
    struct HirPath *path = lower_path(L, t->path);
    return pawHir_new_path_type(L->hir, t->line, path);
}

static struct HirType *LowerContainerType(struct LowerAst *L, struct AstContainerType *t)
{
    struct HirType *first = lower_type(L, t->first);
    if (t->second == NULL)
        return new_list_t(L, first);
    struct HirType *second = lower_type(L, t->second);
    return new_map_t(L, first, second);
}

static struct HirType *unit_type(struct LowerAst *L, int line)
{
    struct HirPath *path = HirPath_new(L->hir);
    HirPath_add(L->hir, path, SCAN_STRING(L->C, "unit"), NULL);
    return pawHir_new_path_type(L->hir, line, path);
}

static struct HirType *LowerTupleType(struct LowerAst *L, struct AstTupleType *t)
{
    if (t->types->count == 0)
        return unit_type(L, t->line);
    struct HirTypeList *elems = lower_type_list(L, t->types);
    return pawHir_new_tuple_type(L->hir, t->line, elems);
}

static struct HirType *LowerFuncType(struct LowerAst *L, struct AstFuncType *t)
{
    struct HirTypeList *params = lower_type_list(L, t->params);
    struct HirType *result = lower_type(L, t->result);
    return pawHir_new_func_ptr(L->hir, t->line, params, result);
}

static struct HirType *LowerInferType(struct LowerAst *L, struct AstInferType *t)
{
    return pawHir_new_infer_type(L->hir, t->line);
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
    return pawHir_new_or_pat(L->hir, p->line, pats);
}

static struct HirPat *LowerFieldPat(struct LowerAst *L, struct AstFieldPat *p)
{
    struct HirPat *pat = lower_pat(L, p->pat);
    return pawHir_new_field_pat(L->hir, p->line, p->name, pat, -1);
}

static struct HirPat *LowerStructPat(struct LowerAst *L, struct AstStructPat *p)
{
    struct HirPath *path = lower_path(L, p->path);
    struct HirPatList *fields = lower_pat_list(L, p->fields);
    return pawHir_new_struct_pat(L->hir, p->line, path, fields);
}

static struct HirPat *LowerVariantPat(struct LowerAst *L, struct AstVariantPat *p)
{
    struct HirPath *path = lower_path(L, p->path);
    struct HirPatList *fields = lower_pat_list(L, p->fields);
    return pawHir_new_variant_pat(L->hir, p->line, path, fields, -1);
}

static struct HirPat *LowerTuplePat(struct LowerAst *L, struct AstTuplePat *p)
{
    struct HirPatList *elems = lower_pat_list(L, p->elems);
    return pawHir_new_tuple_pat(L->hir, p->line, elems);
}

static struct HirPat *LowerPathPat(struct LowerAst *L, struct AstPathPat *p)
{
    struct HirPath *path = lower_path(L, p->path);
    return pawHir_new_path_pat(L->hir, p->line, path);
}

static struct HirPat *LowerLiteralPat(struct LowerAst *L, struct AstLiteralPat *p)
{
    struct HirExpr *expr = lower_expr(L, p->expr);
    return pawHir_new_literal_pat(L->hir, p->line, expr);
}

static struct HirPat *LowerWildcardPat(struct LowerAst *L, struct AstWildcardPat *p)
{
    return pawHir_new_wildcard_pat(L->hir, p->line);
}

static struct HirDecl *lower_decl(struct LowerAst *L, struct AstDecl *decl)
{
    L->line = decl->hdr.line;
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
    L->line = stmt->hdr.line;
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
    L->line = expr->hdr.line;
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
    L->line = pat->hdr.line;
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
    L->line = type->hdr.line;
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
