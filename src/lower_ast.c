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

#define CSTR(L, i) CACHED_STRING(ENV(L), CAST_SIZE(i))

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

static struct HirDecl *new_decl(struct LowerAst *L, int line, enum HirDeclKind kind)
{
    struct HirDecl *decl = pawHir_new_decl(L->C, line, kind);
    pawHir_add_decl(L->C, decl, L->hir->modno);
    return decl;
}

static struct HirStmt *lower_stmt(struct LowerAst *, struct AstStmt *);
static struct HirExpr *lower_expr(struct LowerAst *, struct AstExpr *);
static struct HirDecl *lower_decl(struct LowerAst *, struct AstDecl *);
static struct HirType *lower_type(struct LowerAst *, struct AstExpr *);
static struct HirPat *lower_pat(struct LowerAst *, struct AstPat *);

#define DEFINE_LOWER_LIST(name, T, T2) \
    static struct Hir##T2##List *lower_##name##_list(struct LowerAst *L, struct Ast##T##List *list) \
    { \
        if (list == NULL) return NULL; \
        struct Hir##T2##List *r = pawHir_##name##_list_new(L->C); \
        for (int i = 0; i < list->count; ++i) { \
            struct Hir##T2 *node = lower_##name(L, list->data[i]); \
            K_LIST_PUSH(L->C, r, node); \
        } \
        return r; \
    }
DEFINE_LOWER_LIST(expr, Expr, Expr)
DEFINE_LOWER_LIST(decl, Decl, Decl)
DEFINE_LOWER_LIST(stmt, Stmt, Stmt)
DEFINE_LOWER_LIST(type, Expr, Type)
DEFINE_LOWER_LIST(pat, Pat, Pat)

static struct HirType *new_type(struct LowerAst *L, enum HirTypeKind kind, int line)
{
    return pawHir_new_type(L->C, line, kind);
}

static struct HirBlock *new_block(struct LowerAst *L, int line)
{
    struct HirExpr *r = pawHir_new_expr(L->C, line, kHirBlock);
    return HirGetBlock(r);
}

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

static struct HirExpr *lower_block(struct LowerAst *L, struct AstBlock *block, paw_Bool propagate)
{
    struct BlockState bs;
    enter_block(L, &bs, propagate);

    struct HirExpr *result = pawHir_new_expr(L->C, block->line, kHirBlock);
    struct HirBlock *r = HirGetBlock(result);
    r->stmts = lower_stmt_list(L, block->stmts);
    r->result = lower_expr(L, block->result);
    r->never = bs.never;

    leave_block(L);
    return result;
}

static struct HirExpr *LowerBlock(struct LowerAst *L, struct AstBlock *block)
{
    return lower_block(L, block, PAW_TRUE);
}

#define LOWER_BLOCK(L, block) HirGetBlock(lower_block(L, block, PAW_FALSE))

// TODO: make this less ugly. use Expr instead of Block for nodes other than IfExpr?
#define LOWER_BLOCK_(L, block) HIR_CAST_EXPR(HirGetBlock(lower_block(L, AstGetBlock(block), PAW_FALSE)))

static struct HirDecl *lower_self_param(struct LowerAst *L, struct AstDecl *decl)
{
    const String *self = CSTR(L, CSTR_SELF);
    struct AstFieldDecl *d = AstGetFieldDecl(decl);

    struct HirDecl *result = new_decl(L, d->line, kHirFieldDecl);
    struct HirFieldDecl *r = HirGetFieldDecl(result);

    r->is_pub = d->is_pub;
    r->name =  d->name;
    r->tag = lower_type(L, d->tag);
    return result;
}

static struct HirDeclList *lower_params(struct LowerAst *L, struct AstFuncDecl *d, struct AstDeclList *params)
{
    struct HirDeclList *out = pawHir_decl_list_new(L->C);
    for (int i = 0; i < params->count; ++i) {
        struct AstDecl *ast_param = K_LIST_GET(params, i);
        struct HirDecl *hir_param = i != 0
            ? lower_self_param(L, ast_param)
            : lower_decl(L, ast_param);
        K_LIST_PUSH(L->C, out, hir_param);
    }
    return out;
}

static void register_func(struct LowerAst *L, struct AstFuncDecl *d, struct HirFuncDecl *r)
{
    r->is_pub = d->is_pub;
    r->fn_kind = d->fn_kind;
    r->name = d->name;

    if (d->generics != NULL) {
        r->generics = lower_decl_list(L, d->generics);
    }
    r->params = lower_params(L, d, d->params);
    r->result = lower_type(L, d->result);
}

static struct HirDecl *LowerFieldDecl(struct LowerAst *L, struct AstFieldDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirFieldDecl);
    struct HirFieldDecl *r = HirGetFieldDecl(result);

    r->is_pub = d->is_pub;
    r->name =  d->name;
    r->tag = lower_type(L, d->tag);
    return result;
}

static struct HirDecl *LowerVariantDecl(struct LowerAst *L, struct AstVariantDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirVariantDecl);
    struct HirVariantDecl *r = HirGetVariantDecl(result);
    r->index = d->index;
    r->name = d->name;

    r->fields = lower_decl_list(L, d->fields);
    return result;
}

static struct HirDeclList *lower_fields(struct LowerAst *L, struct AstDeclList *src, String *parent)
{
    struct HirDeclList *dst = pawHir_decl_list_new(L->C);
    for (int i = 0; i < src->count; ++i) {
        struct HirDecl *decl = lower_decl(L, K_LIST_GET(src, i));
        K_LIST_PUSH(L->C, dst, decl);
    }
    return dst;
}

static void register_adt(struct LowerAst *L, struct AstAdtDecl *d, struct HirAdtDecl *r)
{
    if (d->generics != NULL) {
        r->generics = lower_decl_list(L, d->generics);
    }
}

static void lower_adt_fields(struct LowerAst *L, struct AstAdtDecl *d, struct HirAdtDecl *r)
{
    if (d->fields != NULL) r->fields = lower_fields(L, d->fields, d->name);
}

static struct HirExpr *LowerReturnExpr(struct LowerAst *L, struct AstReturnExpr *s)
{
    struct HirExpr *result = pawHir_new_expr(L->C, s->line, kHirReturnExpr);
    struct HirReturnExpr *r = HirGetReturnExpr(result);
    if (s->expr != NULL) r->expr = lower_expr(L, s->expr);
    indicate_jump(L);
    return result;
}

static struct HirDecl *LowerFuncDecl(struct LowerAst *L, struct AstFuncDecl *d);

static struct HirDeclList *lower_methods(struct LowerAst *L, struct AstDeclList *src)
{
    paw_Env *P = ENV(L);
    struct HirDeclList *dst = pawHir_decl_list_new(L->C);
    for (int i = 0; i < src->count; ++i) {
        struct AstDecl *decl = K_LIST_GET(src, i);
        struct AstFuncDecl *d = AstGetFuncDecl(decl);
        struct HirDecl *result = LowerFuncDecl(L, d);
        struct HirFuncDecl *r = HirGetFuncDecl(result);
        K_LIST_PUSH(L->C, dst, result);
    }
    return dst;
}

static struct HirPath *lower_path(struct LowerAst *L, struct AstPath *path)
{
    paw_assert(path->count > 0);
    struct HirPath *r = pawHir_path_new(L->C);
    for (int i = 0; i < path->count; ++i) {
        struct AstSegment src = K_LIST_GET(path, i);
        K_LIST_PUSH(L->C, r, ((struct HirSegment){
                        .types = lower_type_list(L, src.types),
                        .name = src.name,
                    }));
    }
    return r;
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct HirExpr *LowerPathExpr(struct LowerAst *L, struct AstPathExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirPathExpr);
    struct HirPathExpr *r = HirGetPathExpr(result);
    r->path = lower_path(L, e->path);
    return result;
}

static struct HirExpr *LowerLogicalExpr(struct LowerAst *L, struct AstLogicalExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirLogicalExpr);
    struct HirLogicalExpr *r = HirGetLogicalExpr(result);
    r->is_and = e->is_and;
    r->lhs = lower_expr(L, e->lhs);
    r->rhs = lower_expr(L, e->rhs);
    return result;
}

static struct HirExpr *LowerChainExpr(struct LowerAst *L, struct AstChainExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirChainExpr);
    struct HirChainExpr *r = HirGetChainExpr(result);
    r->target = lower_expr(L, e->target);
    return result;
}

static struct HirExpr *LowerUnOpExpr(struct LowerAst *L, struct AstUnOpExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirUnOpExpr);
    struct HirUnOpExpr *r = HirGetUnOpExpr(result);
    r->target = lower_expr(L, e->target);
    r->op = e->op;
    return result;
}

static struct HirExpr *LowerBinOpExpr(struct LowerAst *L, struct AstBinOpExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirBinOpExpr);
    struct HirBinOpExpr *r = HirGetBinOpExpr(result);
    r->lhs = lower_expr(L, e->lhs);
    r->rhs = lower_expr(L, e->rhs);
    r->op = e->op;
    return result;
}

static struct HirExpr *LowerAssignExpr(struct LowerAst *L, struct AstAssignExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirAssignExpr);
    struct HirAssignExpr *r = HirGetAssignExpr(result);

    r->lhs = lower_expr(L, e->lhs);
    if (!HirIsPathExpr(r->lhs) &&
            !HirIsIndex(r->lhs) &&
            !HirIsSelector(r->lhs)) {
        SYNTAX_ERROR(L, "invalid place for assignment");
    }
    r->rhs = lower_expr(L, e->rhs);
    return result;
}

static struct HirExpr *LowerMatchExpr(struct LowerAst *L, struct AstMatchExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirMatchExpr);
    struct HirMatchExpr *r = HirGetMatchExpr(result);
    r->target = lower_expr(L, e->target);
    r->arms = lower_expr_list(L, e->arms);
    paw_assert(r->arms->count > 0);

    // propagate "never" flag to enclosing block
    r->never = PAW_TRUE;
    struct HirExpr **parm;
    K_LIST_FOREACH(r->arms, parm) {
        struct HirMatchArm *arm = HirGetMatchArm(*parm);
        r->never = arm->never;
        if (!r->never) break;
    }
    if (r->never) indicate_jump(L);
    return result;
}

static struct HirExpr *LowerMatchArm(struct LowerAst *L, struct AstMatchArm *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirMatchArm);
    struct HirMatchArm *r = HirGetMatchArm(result);
    r->pat = lower_pat(L, e->pat);
    if (e->guard != NULL) r->guard = lower_expr(L, e->guard);

    // wrap in a block to catch return or jump expressions not enclosed
    // in curly brackets
    struct BlockState bs;
    enter_block(L, &bs, PAW_FALSE);
    r->result = lower_expr(L, e->result);
    leave_block(L);

    r->never = bs.never;
    return result;
}

static struct HirType *new_list_t(struct LowerAst *L, struct HirType *elem_t)
{
    struct HirType *type = new_type(L, kHirPathType, elem_t->hdr.line);
    struct HirPathType *t = HirGetPathType(type);
    t->path = pawHir_path_new(L->C);

    struct HirTypeList *types = pawHir_type_list_new(L->C);
    K_LIST_PUSH(L->C, types, elem_t);

    pawHir_path_add(L->C, t->path, CSTR(L, CSTR_LIST), types);
    return type;
}

static struct HirType *new_map_t(struct LowerAst *L, struct HirType *key_t, struct HirType *value_t)
{
    struct HirType *type = new_type(L, kHirPathType, key_t->hdr.line);
    struct HirPathType *t = HirGetPathType(type);
    t->path = pawHir_path_new(L->C);

    struct HirTypeList *types = pawHir_type_list_new(L->C);
    K_LIST_PUSH(L->C, types, key_t);
    K_LIST_PUSH(L->C, types, value_t);

    pawHir_path_add(L->C, t->path, CSTR(L, CSTR_MAP), types);
    return type;
}

static struct HirDecl *lower_closure_param(struct LowerAst *L, struct AstFieldDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirFieldDecl);
    struct HirFieldDecl *r = HirGetFieldDecl(result);

    r->name = d->name;
    r->tag = d->tag != NULL
        ? lower_type(L, d->tag)
        : NULL;
    return result;
}

static struct HirExpr *LowerClosureExpr(struct LowerAst *L, struct AstClosureExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirClosureExpr);
    struct HirClosureExpr *r = HirGetClosureExpr(result);

    r->params = pawHir_decl_list_new(L->C);
    for (int i = 0; i < e->params->count; ++i) {
        struct AstFieldDecl *src = AstGetFieldDecl(e->params->data[i]);
        struct HirDecl *dst = lower_closure_param(L, src);
        K_LIST_PUSH(L->C, r->params, dst);
    }
    r->result = e->result != NULL
        ? lower_type(L, e->result)
        : NULL;

    if (e->has_body) {
        r->body = LOWER_BLOCK(L, e->body);
        r->has_body = PAW_TRUE;
    } else {
        r->expr = lower_expr(L, e->expr);
    }
    return result;
}

static struct HirDecl *LowerUseDecl(struct LowerAst *L, struct AstUseDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirUseDecl);
    struct HirUseDecl *r = HirGetUseDecl(result);
    r->is_pub = d->is_pub;
    r->path = lower_path(L, d->path);
    r->modno = d->modno;
    r->name = d->name;
    return result;
}

static struct HirDecl *LowerAdtDecl(struct LowerAst *L, struct AstAdtDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirAdtDecl);
    struct HirAdtDecl *r = HirGetAdtDecl(result);
    r->is_pub = d->is_pub;
    r->is_struct = d->is_struct;
    r->name = d->name;

    register_adt(L, d, r);
    lower_adt_fields(L, d, r);
    return result;
}

static struct HirDecl *LowerImplDecl(struct LowerAst *L, struct AstImplDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirImplDecl);
    struct HirImplDecl *r = HirGetImplDecl(result);
    r->name = d->name;

    if (d->generics != NULL) {
        r->generics = lower_decl_list(L, d->generics);
    }
    r->self = lower_path(L, d->self);
    r->methods = lower_methods(L, d->methods);
    return result;
}

static struct HirDecl *LowerVarDecl(struct LowerAst *L, struct AstVarDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirVarDecl);
    struct HirVarDecl *r = HirGetVarDecl(result);

    r->name = d->name;
    r->init = lower_expr(L, d->init);
    if (d->tag != NULL) r->tag = lower_type(L, d->tag);
    return result;
}

static struct HirDecl *LowerTypeDecl(struct LowerAst *L, struct AstTypeDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirTypeDecl);
    struct HirTypeDecl *r = HirGetTypeDecl(result);
    r->generics = NULL; // TODO: generic parameters for aliases

    r->rhs = lower_expr(L, d->rhs);
    return result;
}

// Lower a function call or enumerator constructor
static struct HirExpr *LowerCallExpr(struct LowerAst *L, struct AstCallExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirCallExpr);
    struct HirCallExpr *r = HirGetCallExpr(result);
    r->target = lower_expr(L, e->target);
    r->args = lower_expr_list(L, e->args);
    return result;
}

static struct HirExpr *LowerConversionExpr(struct LowerAst *L, struct AstConversionExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirConversionExpr);
    struct HirConversionExpr *r = HirGetConversionExpr(result);

    r->to = e->to;
    r->arg = lower_expr(L, e->arg);
    return result;
}

static void lower_basic_lit(struct LowerAst *L, struct AstBasicLit *e, struct HirBasicLit *r)
{
    r->t = e->t;
    r->value = e->value;
}

static void lower_tuple_lit(struct LowerAst *L, struct AstTupleLit *e, struct HirTupleLit *r, int line)
{
    r->elems = lower_expr_list(L, e->elems);
}

static void lower_list_lit(struct LowerAst *L, struct AstContainerLit *e, struct HirContainerLit *r)
{
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *src = e->items->data[i];
        struct HirExpr *dst = lower_expr(L, src);
        K_LIST_PUSH(L->C, r->items, dst);
    }
}

static void lower_map_lit(struct LowerAst *L, struct AstContainerLit *e, struct HirContainerLit *r)
{
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *src = e->items->data[i];
        struct HirExpr *dst = lower_expr(L, src);
        paw_assert(HirGetFieldExpr(dst)->fid == -1);
        K_LIST_PUSH(L->C, r->items, dst);
    }
}

static void lower_container_lit(struct LowerAst *L, struct AstContainerLit *e, struct HirContainerLit *r)
{
    r->code = e->code;
    r->items = pawHir_expr_list_new(L->C);
    if (e->code == BUILTIN_LIST) {
        lower_list_lit(L, e, r);
    } else {
        paw_assert(e->code == BUILTIN_MAP);
        lower_map_lit(L, e, r);
    }
}

static struct HirExpr *LowerFieldExpr(struct LowerAst *L, struct AstFieldExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirFieldExpr);
    struct HirFieldExpr *r = HirGetFieldExpr(result);
    r->fid = e->fid;
    if (e->fid < 0) {
        r->key = lower_expr(L, e->key);
    } else {
        r->name = e->name;
    }
    r->value = lower_expr(L, e->value);
    return result;
}

static void lower_composite_lit(struct LowerAst *L, struct AstCompositeLit *e, struct HirCompositeLit *r)
{
    r->path = lower_path(L, e->path);
    r->items = pawHir_expr_list_new(L->C);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *src = e->items->data[i];
        struct HirExpr *dst = lower_expr(L, src);
        K_LIST_PUSH(L->C, r->items, dst);
    }
}

static struct HirExpr *LowerLiteralExpr(struct LowerAst *L, struct AstLiteralExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirLiteralExpr);
    struct HirLiteralExpr *r = HirGetLiteralExpr(result);

    // literal kinds correspond 1-to-1 between AST and HIR
    r->lit_kind = CAST(enum HirLitKind, e->lit_kind);

    if (e->lit_kind == kAstBasicLit) {
        lower_basic_lit(L, &e->basic, &r->basic);
    } else if (e->lit_kind == kAstTupleLit) {
        lower_tuple_lit(L, &e->tuple, &r->tuple, e->line);
    } else if (e->lit_kind == kAstContainerLit) {
        lower_container_lit(L, &e->cont, &r->cont);
    } else {
        paw_assert(e->lit_kind == kAstCompositeLit);
        lower_composite_lit(L, &e->comp, &r->comp);
    }
    return result;
}

static struct HirDecl *LowerFuncDecl(struct LowerAst *L, struct AstFuncDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirFuncDecl);
    struct HirFuncDecl *r = HirGetFuncDecl(result);
    register_func(L, d, r);

    if (d->body != NULL) r->body = LOWER_BLOCK(L, d->body);
    return result;
}

static paw_Bool is_never_block(struct HirExpr *expr)
{
    if (HirIsBlock(expr)) {
        return HirGetBlock(expr)->never;
    }
    return HirGetIfExpr(expr)->never;
}

static void propagate_if_never(struct LowerAst *L, struct HirIfExpr *e)
{
    const paw_Bool lhs_jumps = HirGetBlock(e->then_arm)->never;
    const paw_Bool rhs_jumps = (HirIsIfExpr(e->else_arm) && HirGetIfExpr(e->else_arm)->never)
            || is_never_block(e->else_arm);
    if (lhs_jumps && rhs_jumps) {
        // all paths through this IfExpr execute a jump
        e->never = PAW_TRUE;
        indicate_jump(L);
    }
}

static struct HirExpr *LowerIfExpr(struct LowerAst *L, struct AstIfExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirIfExpr);
    struct HirIfExpr *r = HirGetIfExpr(result);

    r->cond = lower_expr(L, e->cond);
    r->then_arm = LOWER_BLOCK_(L, e->then_arm);

    if (e->else_arm != NULL) {
        if (AstIsBlock(e->else_arm)) {
            r->else_arm = LOWER_BLOCK_(L, e->else_arm);
        } else {
            r->else_arm = lower_expr(L, e->else_arm);
        }
        // For "never" to be propagated here, the "then" and "else" blocks must contain
        // unconditional jumps, and there must be a catch-all "else" at the end of the chain.
        // Basically, any path through the IfExpr must execute a jump.
        propagate_if_never(L, r);
    }
    return result;
}

static struct HirStmt *LowerExprStmt(struct LowerAst *L, struct AstExprStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->C, s->line, kHirExprStmt);
    struct HirExprStmt *r = HirGetExprStmt(result);
    r->expr = lower_expr(L, s->expr);
    return result;
}

static struct HirExpr *LowerWhileExpr(struct LowerAst *L, struct AstWhileExpr *s)
{
    struct HirExpr *result = pawHir_new_expr(L->C, s->line, kHirWhileExpr);
    struct HirWhileExpr *r = HirGetWhileExpr(result);

    r->cond = lower_expr(L, s->cond);
    r->block = LOWER_BLOCK(L, s->block);
    return result;
}

static void visit_for_body(struct LowerAst *L, String *control_name, struct AstBlock *b, struct HirForExpr *r)
{
    r->control = new_decl(L, b->line, kHirVarDecl);
    struct HirVarDecl *control = HirGetVarDecl(r->control);
    control->name = control_name;

    r->block = new_block(L, b->line);
    r->block->stmts = lower_stmt_list(L, b->stmts);
}

static struct HirExpr *LowerForExpr(struct LowerAst *L, struct AstForExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirForExpr);
    struct HirForExpr *r = HirGetForExpr(result);
    r->target = lower_expr(L, e->target);
    visit_for_body(L, e->name, e->block, r);
    return result;
}

static struct HirExpr *LowerIndex(struct LowerAst *L, struct AstIndex *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirIndex);
    struct HirIndex *r = HirGetIndex(result);
    r->target = lower_expr(L, e->target);
    r->is_slice = e->is_slice;

    if (e->is_slice) {
        if (e->first != NULL) r->first = lower_expr(L, e->first);
        if (e->second != NULL) r->second = lower_expr(L, e->second);
    } else {
        r->first = lower_expr(L, e->first);
    }
    return result;
}

static struct HirExpr *LowerSelector(struct LowerAst *L, struct AstSelector *e)
{
    struct HirExpr *result = pawHir_new_expr(L->C, e->line, kHirSelector);
    struct HirSelector *r = HirGetSelector(result);
    r->target = lower_expr(L, e->target);
    if (e->is_index) {
        r->is_index = PAW_TRUE;
        r->index = e->index;
    } else {
        r->name = e->name;
    }
    return result;
}

static struct HirExpr *LowerJumpExpr(struct LowerAst *L, struct AstJumpExpr *s)
{
    struct HirExpr *result = pawHir_new_expr(L->C, s->line, kHirJumpExpr);
    struct HirJumpExpr *r = HirGetJumpExpr(result);
    r->jump_kind = s->jump_kind;
    indicate_jump(L);
    return result;
}

static struct HirStmt *LowerDeclStmt(struct LowerAst *L, struct AstDeclStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->C, s->line, kHirDeclStmt);
    struct HirDeclStmt *r = HirGetDeclStmt(result);
    r->decl = lower_decl(L, s->decl);
    return result;
}

static struct HirDecl *LowerGenericDecl(struct LowerAst *L, struct AstGenericDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirGenericDecl);
    struct HirGenericDecl *r = HirGetGenericDecl(result);
    r->name = d->name;
    return result;
}

static struct HirType *LowerPathType(struct LowerAst *L, struct AstPathExpr *e)
{
    struct HirType *result = new_type(L, kHirPathType, e->line);
    struct HirPathType *r = HirGetPathType(result);
    r->path = lower_path(L, e->path);
    return result;
}

static struct HirType *LowerContainerType(struct LowerAst *L, struct AstContainerType *e)
{
    struct HirType *first = lower_type(L, e->first);
    if (e->second == NULL) return new_list_t(L, first);
    struct HirType *second = lower_type(L, e->second);
    return new_map_t(L, first, second);
}

static struct HirType *unit_type(struct LowerAst *L)
{
    struct HirType *result = new_type(L, kHirPathType, 0);
    struct HirPathType *r = HirGetPathType(result);
    r->path = pawHir_path_new(L->C);
    pawHir_path_add(L->C, r->path, SCAN_STRING(L->C, "(unit)"), NULL);
    return result;
}

static struct HirType *LowerTupleType(struct LowerAst *L, struct AstTupleType *e)
{
    if (e->types->count == 0) return unit_type(L);
    struct HirType *result = new_type(L, kHirTupleType, e->line);
    struct HirTupleType *r = HirGetTupleType(result);
    r->elems = lower_type_list(L, e->types);
    return result;
}

static struct HirType *LowerSignature(struct LowerAst *L, struct AstSignature *e)
{
    struct HirType *type = new_type(L, kHirFuncPtr, e->line);
    type->fptr.params = lower_type_list(L, e->params);
    type->fptr.result = lower_type(L, e->result);
    return type;
}

static void combine_or_parts(struct LowerAst *L, struct HirOrPat *or, struct HirPat *part)
{
    if (!HirIsOrPat(part)) {
        K_LIST_PUSH(L->C, or->pats, part);
        return;
    }

    struct HirPatList *pats = HirGetOrPat(part)->pats;
    for (int i = 0; i < pats->count; ++i) {
        K_LIST_PUSH(L->C, or->pats, K_LIST_GET(pats, i));
    }
}

static struct HirPat *LowerOrPat(struct LowerAst *L, struct AstOrPat *p)
{
    struct HirPat *result = pawHir_new_pat(L->C, p->line, kHirOrPat);
    struct HirOrPat *r = HirGetOrPat(result);
    r->pats = pawHir_pat_list_new(L->C);
    combine_or_parts(L, r, lower_pat(L, p->lhs));
    combine_or_parts(L, r, lower_pat(L, p->rhs));
    return result;
}

static struct HirPat *LowerFieldPat(struct LowerAst *L, struct AstFieldPat *p)
{
    struct HirPat *result = pawHir_new_pat(L->C, p->line, kHirFieldPat);
    struct HirFieldPat *r = HirGetFieldPat(result);
    r->name = p->name;
    r->pat = lower_pat(L, p->pat);
    return result;
}

static struct HirPat *LowerStructPat(struct LowerAst *L, struct AstStructPat *p)
{
    struct HirPat *result = pawHir_new_pat(L->C, p->line, kHirStructPat);
    struct HirStructPat *r = HirGetStructPat(result);
    r->path = lower_path(L, p->path);
    r->fields = lower_pat_list(L, p->fields);
    return result;
}
static struct HirPat *LowerVariantPat(struct LowerAst *L, struct AstVariantPat *p)
{
    struct HirPat *result = pawHir_new_pat(L->C, p->line, kHirVariantPat);
    struct HirVariantPat *r = HirGetVariantPat(result);
    r->path = lower_path(L, p->path);
    r->fields = lower_pat_list(L, p->fields);
    return result;
}
static struct HirPat *LowerTuplePat(struct LowerAst *L, struct AstTuplePat *p)
{
    struct HirPat *result = pawHir_new_pat(L->C, p->line, kHirTuplePat);
    struct HirTuplePat *r = HirGetTuplePat(result);
    r->elems = lower_pat_list(L, p->elems);
    return result;
}
static struct HirPat *LowerPathPat(struct LowerAst *L, struct AstPathPat *p)
{
    struct HirPat *result = pawHir_new_pat(L->C, p->line, kHirPathPat);
    struct HirPathPat *r = HirGetPathPat(result);
    r->path = lower_path(L, p->path);
    return result;
}
static struct HirPat *LowerLiteralPat(struct LowerAst *L, struct AstLiteralPat *p)
{
    struct HirPat *result = pawHir_new_pat(L->C, p->line, kHirLiteralPat);
    struct HirLiteralPat *r = HirGetLiteralPat(result);
    r->expr = lower_expr(L, p->expr);
    return result;
}

static struct HirPat *LowerWildcardPat(struct LowerAst *L, struct AstWildcardPat *p)
{
    return pawHir_new_pat(L->C, p->line, kHirWildcardPat);
}

static struct HirDecl *lower_decl(struct LowerAst *L, struct AstDecl *decl)
{
    L->line = decl->hdr.line;
    switch (AST_KINDOF(decl)) {
#define DEFINE_CASE(a, b) \
        case kAst##a: \
            return Lower##a(L, AstGet##a(decl));
        AST_DECL_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirStmt *lower_stmt(struct LowerAst *L, struct AstStmt *stmt)
{
    L->line = stmt->hdr.line;
    switch (AST_KINDOF(stmt)) {
#define DEFINE_CASE(a, b) \
        case kAst##a: \
            return Lower##a(L, AstGet##a(stmt));
        AST_STMT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirExpr *lower_expr(struct LowerAst *L, struct AstExpr *expr)
{
    L->line = expr->hdr.line;
    switch (AST_KINDOF(expr)) {
        case kAstLiteralExpr:
            return LowerLiteralExpr(L, AstGetLiteralExpr(expr));
        case kAstLogicalExpr:
            return LowerLogicalExpr(L, AstGetLogicalExpr(expr));
        case kAstPathExpr:
            return LowerPathExpr(L, AstGetPathExpr(expr));
        case kAstChainExpr:
            return LowerChainExpr(L, AstGetChainExpr(expr));
        case kAstUnOpExpr:
            return LowerUnOpExpr(L, AstGetUnOpExpr(expr));
        case kAstBinOpExpr:
            return LowerBinOpExpr(L, AstGetBinOpExpr(expr));
        case kAstClosureExpr:
            return LowerClosureExpr(L, AstGetClosureExpr(expr));
        case kAstConversionExpr:
            return LowerConversionExpr(L, AstGetConversionExpr(expr));
        case kAstCallExpr:
            return LowerCallExpr(L, AstGetCallExpr(expr));
        case kAstIndex:
            return LowerIndex(L, AstGetIndex(expr));
        case kAstSelector:
            return LowerSelector(L, AstGetSelector(expr));
        case kAstAssignExpr:
            return LowerAssignExpr(L, AstGetAssignExpr(expr));
        case kAstFieldExpr:
            return LowerFieldExpr(L, AstGetFieldExpr(expr));
        case kAstIfExpr:
            return LowerIfExpr(L, AstGetIfExpr(expr));
        case kAstReturnExpr:
            return LowerReturnExpr(L, AstGetReturnExpr(expr));
        case kAstJumpExpr:
            return LowerJumpExpr(L, AstGetJumpExpr(expr));
        case kAstForExpr:
            return LowerForExpr(L, AstGetForExpr(expr));
        case kAstWhileExpr:
            return LowerWhileExpr(L, AstGetWhileExpr(expr));
        case kAstMatchExpr:
            return LowerMatchExpr(L, AstGetMatchExpr(expr));
        case kAstBlock:
            return LowerBlock(L, AstGetBlock(expr));
        case kAstMatchArm:
            return LowerMatchArm(L, AstGetMatchArm(expr));
        default:
            PAW_UNREACHABLE();
    }
}

static struct HirPat *lower_pat(struct LowerAst *L, struct AstPat *pat)
{
    L->line = pat->hdr.line;
    switch (AST_KINDOF(pat)) {
#define DEFINE_CASE(a, b) case kAst##a: \
            return Lower##a(L, AstGet##a(pat));
        AST_PAT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

static struct HirType *lower_type(struct LowerAst *L, struct AstExpr *expr)
{
    L->line = expr->hdr.line;
    switch (AST_KINDOF(expr)) {
        default:
            TYPE_ERROR(L, "expected type"); // no return
        case kAstPathExpr:
            return LowerPathType(L, AstGetPathExpr(expr));
        case kAstTupleType:
            return LowerTupleType(L, AstGetTupleType(expr));
        case kAstSignature:
            return LowerSignature(L, AstGetSignature(expr));
        case kAstContainerType:
            return LowerContainerType(L, AstGetContainerType(expr));
    }
}

static struct Hir *lower_ast(struct LowerAst *L, struct Ast *ast)
{
    struct ModuleList *mods = L->C->modules;
    while (ast->modno >= mods->count) {
        K_LIST_PUSH(L->C, mods, NULL);
    }
    struct Hir *hir = pawHir_new(L->C, ast->name, ast->modno);
    L->hir = hir;

    struct ModuleInfo *mod = pawP_mi_new(L->C, hir);
    K_LIST_SET(mods, hir->modno, mod);
    hir->items = lower_decl_list(L, ast->items);

    return hir;
}

static struct HirDecl *find_builtin(struct HirDeclList *items, const String *name)
{
     for (int i = 0; i < items->count; ++i) {
         struct HirDecl *item = K_LIST_GET(items, i);
        if (pawS_eq(name, item->hdr.name)) return item;
     }
     PAW_UNREACHABLE();
}

static void set_builtin_adts(struct LowerAst *L, struct HirDeclList *items)
{
    struct Builtin *builtins = L->C->builtins;

    // builtin primitives always come first, and there are no intervening decls.
    builtins[BUILTIN_UNIT].did = K_LIST_GET(items, BUILTIN_UNIT)->hdr.did;
    builtins[BUILTIN_BOOL].did = K_LIST_GET(items, BUILTIN_BOOL)->hdr.did;
    builtins[BUILTIN_INT].did = K_LIST_GET(items, BUILTIN_INT)->hdr.did;
    builtins[BUILTIN_FLOAT].did = K_LIST_GET(items, BUILTIN_FLOAT)->hdr.did;
    builtins[BUILTIN_STR].did = K_LIST_GET(items, BUILTIN_STR)->hdr.did;

    // builtin objects may declare generics or fields, so they need to be searched for
    builtins[BUILTIN_LIST].did = find_builtin(items, CSTR(L, CSTR_LIST))->hdr.did;
    builtins[BUILTIN_MAP].did = find_builtin(items, CSTR(L, CSTR_MAP))->hdr.did;
}

void pawP_lower_ast(struct Compiler *C)
{
    struct LowerAst L = {
        .dm = C->dm,
        .P = ENV(C),
        .C = C,
    };

    struct Hir *prelude = lower_ast(&L, C->prelude);
    set_builtin_adts(&L, prelude->items);

    paw_Int itr = PAW_ITER_INIT;
    while (pawH_iter(C->imports, &itr)) {
        Value *pv = pawH_value(C->imports, itr);
        lower_ast(&L, pv->p);
    }
}
