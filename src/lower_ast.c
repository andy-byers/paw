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

#define SYNTAX_ERROR(L, ...) pawE_error(ENV(L), PAW_ESYNTAX, (L)->line, __VA_ARGS__)
#define TYPE_ERROR(L, ...) pawE_error(ENV(L), PAW_ETYPE, (L)->line, __VA_ARGS__)
#define CSTR(L, i) CACHED_STRING(ENV(L), CAST_SIZE(i))

struct LowerAst {
    struct DynamicMem *dm;
    struct Compiler *C;
    struct Hir *hir;
    paw_Env *P;
    int line;
};

static struct HirDecl *new_decl(struct LowerAst *L, int line, enum HirDeclKind kind)
{
    struct HirDecl *decl = pawHir_new_decl(L->hir, line, kind);
    pawHir_add_decl(L->hir, decl);
    return decl;
}

static struct HirStmt *lower_stmt(struct LowerAst *, struct AstStmt *);
static struct HirExpr *lower_expr(struct LowerAst *, struct AstExpr *);
static struct HirDecl *lower_decl(struct LowerAst *, struct AstDecl *);
static struct HirType *lower_type(struct LowerAst *, struct AstExpr *);

#define DEFINE_LOWER_LIST(name, T, T2) \
    static struct Hir##T2##List *lower_##name##_list(struct LowerAst *L, struct Ast##T##List *list) \
    { \
        if (list == NULL) return NULL; \
        struct Hir##T2##List *r = pawHir_##name##_list_new(L->hir); \
        for (int i = 0; i < list->count; ++i) { \
            struct Hir##T2 *node = lower_##name(L, list->data[i]); \
            pawHir_##name##_list_push(L->hir, r, node); \
        } \
        return r; \
    }
DEFINE_LOWER_LIST(expr, Expr, Expr)
DEFINE_LOWER_LIST(decl, Decl, Decl)
DEFINE_LOWER_LIST(stmt, Stmt, Stmt)
DEFINE_LOWER_LIST(type, Expr, Type)

static struct HirType *new_type(struct LowerAst *L, enum HirTypeKind kind, int line)
{
    return pawHir_attach_type(L->hir, NO_DECL, kind, line);
}

static struct HirBlock *new_block(struct LowerAst *L, int line)
{
    struct HirStmt *r = pawHir_new_stmt(L->hir, line, kHirBlock);
    return HirGetBlock(r);
}

static struct HirStmt *LowerBlock(struct LowerAst *L, struct AstBlock *block)
{
    struct HirStmt *result = pawHir_new_stmt(L->hir, block->line, kHirBlock);
    struct HirBlock *r = HirGetBlock(result);
    r->stmts = lower_stmt_list(L, block->stmts);
    return result;
}

#define LOWER_BLOCK(L, block) HirGetBlock(LowerBlock(L, block))

static struct HirDecl *lower_self_decl(struct LowerAst *L, struct AstDecl *decl)
{
    const String *self = CSTR(L, CSTR_SELF);
    struct AstFieldDecl *d = AstGetFieldDecl(decl);

    struct HirDecl *result = new_decl(L, d->line, kHirFieldDecl);
    struct HirFieldDecl *r = HirGetFieldDecl(result);

    r->is_pub = d->is_pub;
    r->name =  d->name;
    r->tag = lower_type(L, d->tag);
    if (pawS_eq(d->name, self)) {
    
    }
    return result;
}

static struct HirDeclList *lower_params(struct LowerAst *L, struct AstFuncDecl *d, struct AstDeclList *params)
{
    struct HirDeclList *out = pawHir_decl_list_new(L->hir);
    for (int i = 0; i < params->count; ++i) {
        struct AstDecl *ast_param = K_LIST_GET(params, i);
        struct HirDecl *hir_param = i != 0
            ? lower_self_decl(L, ast_param)
            : lower_decl(L, ast_param);
        pawHir_decl_list_push(L->hir, out, hir_param);
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
        r->monos = pawHir_decl_list_new(L->hir);
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
    struct HirDeclList *dst = pawHir_decl_list_new(L->hir);
    for (int i = 0; i < src->count; ++i) {
        struct HirDecl *decl = lower_decl(L, pawAst_decl_list_get(src, i));
        pawHir_decl_list_push(L->hir, dst, decl);
    }
    return dst;
}

static void register_adt(struct LowerAst *L, struct AstAdtDecl *d, struct HirAdtDecl *r)
{
    if (d->generics != NULL) {
        r->monos = pawHir_decl_list_new(L->hir);
        r->generics = lower_decl_list(L, d->generics);
    }
}

static void lower_adt_fields(struct LowerAst *L, struct AstAdtDecl *d, struct HirAdtDecl *r)
{
    if (d->fields != NULL) r->fields = lower_fields(L, d->fields, d->name);
}

static struct HirStmt *LowerReturnStmt(struct LowerAst *L, struct AstReturnStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->hir, s->line, kHirReturnStmt);
    struct HirReturnStmt *r = HirGetReturnStmt(result);
    if (s->expr != NULL) r->expr = lower_expr(L, s->expr); 
    return result;
}

static struct HirDecl *LowerFuncDecl(struct LowerAst *L, struct AstFuncDecl *d);

static struct HirDeclList *lower_methods(struct LowerAst *L, struct AstDeclList *src)
{
    paw_Env *P = ENV(L);
    struct HirDeclList *dst = pawHir_decl_list_new(L->hir);
    for (int i = 0; i < src->count; ++i) {
        struct AstDecl *decl = pawAst_decl_list_get(src, i);
        struct AstFuncDecl *d = AstGetFuncDecl(decl);
        struct HirDecl *result = LowerFuncDecl(L, d);
        struct HirFuncDecl *r = HirGetFuncDecl(result);
        pawHir_decl_list_push(L->hir, dst, result);
    }
    return dst;
}

static struct HirPath *lower_path(struct LowerAst *L, struct AstPath *path)
{
    paw_assert(path->count > 0);
    struct HirPath *r = pawHir_path_new(L->hir);
    for (int i = 0; i < path->count; ++i) {
        struct AstSegment *src = pawAst_path_get(path, i);
        struct HirSegment *dst = pawHir_segment_new(L->hir);
        dst->types = lower_type_list(L, src->types);
        dst->name = src->name;
        pawHir_path_push(L->hir, r, dst);
    }
    return r;
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct HirExpr *LowerPathExpr(struct LowerAst *L, struct AstPathExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirPathExpr);
    struct HirPathExpr *r = HirGetPathExpr(result);
    r->path = lower_path(L, e->path);
    return result;
}

static struct HirExpr *LowerLogicalExpr(struct LowerAst *L, struct AstLogicalExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirLogicalExpr);
    struct HirLogicalExpr *r = HirGetLogicalExpr(result);
    r->is_and = e->is_and;
    r->lhs = lower_expr(L, e->lhs);
    r->rhs = lower_expr(L, e->rhs);
    return result;
}

static struct HirExpr *LowerChainExpr(struct LowerAst *L, struct AstChainExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirChainExpr);
    struct HirChainExpr *r = HirGetChainExpr(result);
    r->target = lower_expr(L, e->target);
    return result;
}

static struct HirExpr *LowerUnopExpr(struct LowerAst *L, struct AstUnOpExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirUnOpExpr);
    struct HirUnOpExpr *r = HirGetUnOpExpr(result);
    r->target = lower_expr(L, e->target);
    r->op = e->op;
    return result;
}

static struct HirExpr *LowerBinopExpr(struct LowerAst *L, struct AstBinOpExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirBinOpExpr);
    struct HirBinOpExpr *r = HirGetBinOpExpr(result);
    r->lhs = lower_expr(L, e->lhs);
    r->rhs = lower_expr(L, e->rhs);
    r->op = e->op;
    return result;
}

static struct HirExpr *LowerAssignExpr(struct LowerAst *L, struct AstAssignExpr *s)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, s->line, kHirAssignExpr);
    struct HirAssignExpr *r = HirGetAssignExpr(result);

    r->lhs = lower_expr(L, s->lhs);
    if (!HirIsPathExpr(r->lhs) &&
            !HirIsIndex(r->lhs) &&
            !HirIsSelector(r->lhs)) {
        SYNTAX_ERROR(L, "invalid place for assignment");
    }
    r->rhs = lower_expr(L, s->rhs);
    return result;
}

static struct HirType *new_list_t(struct LowerAst *L, struct HirType *elem_t)
{
    struct HirType *type = new_type(L, kHirPathType, elem_t->hdr.line);
    struct HirPathType *t = HirGetPathType(type);
    t->path = pawHir_path_new(L->hir);

    struct HirTypeList *types = pawHir_type_list_new(L->hir);
    pawHir_type_list_push(L->hir, types, elem_t);

    pawHir_path_add(L->hir, t->path, CSTR(L, CSTR_LIST), types);
    return type;
}

static struct HirType *new_map_t(struct LowerAst *L, struct HirType *key_t, struct HirType *value_t)
{
    struct HirType *type = new_type(L, kHirPathType, key_t->hdr.line);
    struct HirPathType *t = HirGetPathType(type);
    t->path = pawHir_path_new(L->hir);

    struct HirTypeList *types = pawHir_type_list_new(L->hir); 
    pawHir_type_list_push(L->hir, types, key_t);
    pawHir_type_list_push(L->hir, types, value_t);

    pawHir_path_add(L->hir, t->path, CSTR(L, CSTR_MAP), types);
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
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirClosureExpr);
    struct HirClosureExpr *r = HirGetClosureExpr(result);

    r->params = pawHir_decl_list_new(L->hir);
    for (int i = 0; i < e->params->count; ++i) {
        struct AstFieldDecl *src = AstGetFieldDecl(e->params->data[i]);
        struct HirDecl *dst = lower_closure_param(L, src);
        pawHir_decl_list_push(L->hir, r->params, dst);
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

    if (d->fields != NULL) lower_adt_fields(L, d, r);
    return result;
}

static struct HirDecl *LowerImplDecl(struct LowerAst *L, struct AstImplDecl *d)
{
    struct HirDecl *result = new_decl(L, d->line, kHirImplDecl);
    struct HirImplDecl *r = HirGetImplDecl(result);
    r->name = d->name;

    if (d->generics != NULL) {
        r->generics = lower_decl_list(L, d->generics);
        r->monos = pawHir_decl_list_new(L->hir);
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
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirCallExpr);
    struct HirCallExpr *r = HirGetCallExpr(result);
    r->target = lower_expr(L, e->target);
    r->args = lower_expr_list(L, e->args);
    return result;
}

static struct HirExpr *LowerConversionExpr(struct LowerAst *L, struct AstConversionExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirConversionExpr);
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
        pawHir_expr_list_push(L->hir, r->items, dst);
    }
}

static void lower_map_lit(struct LowerAst *L, struct AstContainerLit *e, struct HirContainerLit *r)
{
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *src = e->items->data[i];
        struct HirExpr *dst = lower_expr(L, src);
        struct HirFieldExpr *field = HirGetFieldExpr(dst);
        paw_assert(field->fid == -1);
        pawHir_expr_list_push(L->hir, r->items, dst);
    }
}

static void lower_container_lit(struct LowerAst *L, struct AstContainerLit *e, struct HirContainerLit *r)
{
    r->code = e->code;
    r->items = pawHir_expr_list_new(L->hir);
    if (e->code == BUILTIN_LIST) {
        lower_list_lit(L, e, r);
    } else {
        paw_assert(e->code == BUILTIN_MAP);
        lower_map_lit(L, e, r);
    }
}

static struct HirExpr *LowerFieldExpr(struct LowerAst *L, struct AstFieldExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirFieldExpr);
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
    r->items = pawHir_expr_list_new(L->hir);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *src = e->items->data[i];
        struct HirExpr *dst = lower_expr(L, src);
        pawHir_expr_list_push(L->hir, r->items, dst);
    }
}

static struct HirExpr *LowerLiteralExpr(struct LowerAst *L, struct AstLiteralExpr *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirLiteralExpr);
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

static struct HirStmt *LowerIfStmt(struct LowerAst *L, struct AstIfStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->hir, s->line, kHirIfStmt);
    struct HirIfStmt *r = HirGetIfStmt(result);

    r->cond = lower_expr(L, s->cond);
    r->then_arm = lower_stmt(L, s->then_arm);

    if (s->else_arm != NULL) r->else_arm = lower_stmt(L, s->else_arm);
    return result;
}

static struct HirStmt *LowerExprStmt(struct LowerAst *L, struct AstExprStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->hir, s->line, kHirExprStmt);
    struct HirExprStmt *r = HirGetExprStmt(result);
    r->expr = lower_expr(L, s->expr);
    return result;
}

static struct HirStmt *LowerWhileStmt(struct LowerAst *L, struct AstWhileStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->hir, s->line, kHirWhileStmt);
    struct HirWhileStmt *r = HirGetWhileStmt(result);
    r->is_dowhile = s->is_dowhile;

    r->cond = lower_expr(L, s->cond);
    r->block = LOWER_BLOCK(L, s->block);
    return result;
}

static void visit_forbody(struct LowerAst *L, String *iname, struct AstBlock *b, struct HirForStmt *r)
{
    r->control = new_decl(L, b->line, kHirVarDecl);
    struct HirVarDecl *control = HirGetVarDecl(r->control);
    control->name = iname;

    r->block = new_block(L, b->line);
    r->block->stmts = lower_stmt_list(L, b->stmts);
}

static void visit_fornum(struct LowerAst *L, struct AstForStmt *s, struct HirForStmt *r)
{
    struct AstForNum *fornum = &s->fornum;
    r->fornum.begin = lower_expr(L, fornum->begin);
    r->fornum.end = lower_expr(L, fornum->end);
    r->fornum.step = lower_expr(L, fornum->step);
    visit_forbody(L, s->name, s->block, r);
}

static void visit_forin(struct LowerAst *L, struct AstForStmt *s, struct HirForStmt *r)
{
    struct AstForIn *forin = &s->forin;
    r->forin.target = lower_expr(L, forin->target);
    visit_forbody(L, s->name, s->block, r);
}

static struct HirStmt *LowerForStmt(struct LowerAst *L, struct AstForStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->hir, s->line, kHirForStmt);
    struct HirForStmt *r = HirGetForStmt(result);
    r->is_fornum = s->is_fornum;
    if (s->is_fornum) {
        visit_fornum(L, s, r);
    } else {
        visit_forin(L, s, r);
    }
    return result;
}

static struct HirExpr *LowerIndex(struct LowerAst *L, struct AstIndex *e)
{
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirIndex);
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
    struct HirExpr *result = pawHir_new_expr(L->hir, e->line, kHirSelector);
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

static struct HirStmt *LowerLabelStmt(struct LowerAst *L, struct AstLabelStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->hir, s->line, kHirLabelStmt);
    struct HirLabelStmt *r = HirGetLabelStmt(result);
    r->label = s->label;
    return result;
}

static struct HirStmt *LowerDeclStmt(struct LowerAst *L, struct AstDeclStmt *s)
{
    struct HirStmt *result = pawHir_new_stmt(L->hir, s->line, kHirDeclStmt);
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
    r->path = pawHir_path_new(L->hir);
    pawHir_path_add(L->hir, r->path, SCAN_STRING(L->C, "(unit)"), NULL);
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
    struct HirType *result = lower_type(L, e->result);
    type->fptr.params = lower_type_list(L, e->params);
    type->fptr.result = result;
    return type;
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
            return LowerUnopExpr(L, AstGetUnOpExpr(expr));
        case kAstBinOpExpr:
            return LowerBinopExpr(L, AstGetBinOpExpr(expr));
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
        default:
            return LowerFieldExpr(L, AstGetFieldExpr(expr));
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
    struct ModuleList *mods = L->dm->modules;
    while (ast->modno >= mods->count) {
        pawP_mod_list_push(L->C, mods, NULL);
    }
    L->hir = pawHir_new(L->C, ast->name, ast->modno);
    L->hir->modno = ast->modno;
    if (L->C->dm->decls == NULL) {
        // TODO: should have functions like pawHir_decl_list_new take a Compiler * instead of an Hir *,
        //       then allocate this list in pawP_startup
        L->C->dm->decls = pawHir_decl_list_new(L->hir);
    }
    struct ModuleInfo *mod = pawP_mi_new(L->C, L->hir);
    K_LIST_SET(mods, L->hir->modno, mod);
    L->hir->items = lower_decl_list(L, ast->items);
    return L->hir;
}

void pawP_lower_ast(struct Compiler *C)
{
    struct LowerAst L = {
        .dm = C->dm,
        .P = ENV(C),
        .C = C,
    };

    // prelude must be lowered first, so its first DeclId is equal to 0
    lower_ast(&L, C->prelude);

    paw_Int itr = PAW_ITER_INIT;
    while (pawH_iter(C->imports, &itr)) {
        Value *pv = pawH_value(C->imports, itr);
        lower_ast(&L, pv->p);
    }
}
