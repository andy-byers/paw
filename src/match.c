// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: The code in this file is somewhat of a hack. It performs a
//       straightforward conversion from a match statement into an
//       if-else chain. It may end up generating some inefficient code with
//       redundant checks. Eventually, the decision tree created in
//       exhaustiveness.c should be used to generate better code.

#include "compile.h"
#include "hir.h"

struct BindingState {
    struct BindingState *outer;
    struct HirDeclList *bindings;
};

struct LowerMatches {
    struct BindingState *bs;
    struct Compiler *C;
    paw_Env *P;
};

static void enter_arm(struct LowerMatches *L, struct BindingState *bs)
{
    *bs = (struct BindingState){
        .bindings = pawHir_decl_list_new(L->C),
        .outer = L->bs,
    };
    L->bs = bs;
}

static void leave_arm(struct LowerMatches *L)
{
    L->bs = L->bs->outer;
}

static struct HirExpr *lower_pattern(struct HirVisitor *V, struct HirPat *pat, struct HirExpr *expr);

static struct HirExpr *lower_wildcard_pat(struct HirVisitor *V, struct HirWildcardPat *p, struct HirExpr *expr)
{
    return NULL;
}

static struct HirExpr *lower_binding_pat(struct HirVisitor *V, struct HirBindingPat *p, struct HirExpr *expr)
{
    struct LowerMatches *L = V->ud;
    struct HirDecl *result = pawHir_new_decl(V->C, p->line, kHirVarDecl);
    struct HirVarDecl *r = HirGetVarDecl(result);
    pawHir_add_decl(V->C, result);
    r->type = HIR_TYPEOF(expr);
    r->name = p->name;
    r->init = expr;

    pawHir_decl_list_push(V->C, L->bs->bindings, result);
    return NULL; // no checks
}

static struct HirExpr *new_path_expr(struct Compiler *C, struct HirDecl *decl, int line)
{
    struct HirExpr *result = pawHir_new_expr(C, line, kHirPathExpr);
    struct HirPathExpr *r = HirGetPathExpr(result);
    r->path = pawHir_path_new(C);
    r->type = HIR_TYPEOF(decl);

    struct HirSegment *seg = pawHir_path_add(C, r->path, decl->hdr.name, NULL);
    seg->modno = 1 /* TODO: modno should be part of DeclId */;
    seg->did = decl->hdr.did;
    return result;
}

#define GET_TYPE(C, t) (pawHir_get_decl(C, t)->hdr.type)

static struct HirExpr *compose_eq(struct Compiler *C, struct HirExpr *a, struct HirExpr *b)
{
    struct HirExpr *result = pawHir_new_expr(C, b->hdr.line, kHirBinOpExpr);
    struct HirBinOpExpr *r = HirGetBinOpExpr(result);
    r->type = GET_TYPE(C, PAW_TBOOL);
    r->op = BINARY_EQ;
    r->lhs = a;
    r->rhs = b;
    return result;
}

static struct HirExpr *compose_logical(struct Compiler *C, struct HirExpr *a, struct HirExpr *b, paw_Bool is_and)
{
    struct HirExpr *result = pawHir_new_expr(C, b->hdr.line, kHirLogicalExpr);
    struct HirLogicalExpr *r = HirGetLogicalExpr(result);
    r->type = GET_TYPE(C, PAW_TBOOL);
    r->is_and = is_and;
    r->lhs = a;
    r->rhs = b;
    return result;
}

static struct HirExpr *compose_and(struct Compiler *C, struct HirExpr *a, struct HirExpr *b)
{
    return compose_logical(C, a, b, PAW_TRUE);
}

static struct HirExpr *compose_or(struct Compiler *C, struct HirExpr *a, struct HirExpr *b)
{
    return compose_logical(C, a, b, PAW_FALSE);
}

static struct HirExpr *new_field_selector(struct Compiler *C, struct HirExpr *target, String *name)
{
    struct HirExpr *result = pawHir_new_expr(C, target->hdr.line, kHirSelector);
    struct HirSelector *r = HirGetSelector(result);
    r->target = target;
    r->name = name;
    return result;
}

static struct HirExpr *new_index_selector(struct Compiler *C, struct HirExpr *target, int index)
{
    struct HirExpr *result = pawHir_new_expr(C, target->hdr.line, kHirSelector);
    struct HirSelector *r = HirGetSelector(result);
    r->is_index = PAW_TRUE;
    r->target = target;
    r->index = index;
    return result;
}

static struct HirExpr *new_tuple_selector(struct Compiler *C, struct HirExpr *target, int index)
{
    struct HirExpr *result = new_index_selector(C, target, index);
    struct HirSelector *r = HirGetSelector(result);

    struct HirTupleType *tuple = HirGetTupleType(HIR_TYPEOF(target));
    r->type = K_LIST_GET(tuple->elems, index);
    return result;
}

// NOTE: variant fields cannot be 'selected' from Paw code, this is just for the implementation of 'match'
static struct HirExpr *new_variant_selector(struct Compiler *C, struct HirExpr *target, struct HirType *base, int index)
{
    struct HirExpr *result = new_index_selector(C, target, index);
    struct HirSelector *r = HirGetSelector(result);

    struct HirFuncPtr *fptr = HIR_FPTR(base);
    r->type = K_LIST_GET(fptr->params, index);
    return result;
}

static struct HirExpr *lower_field_pat(struct HirVisitor *V, struct HirFieldPat *p, struct HirExpr *target)
{
    struct Compiler *C = V->C;
    struct HirExpr *getter = new_field_selector(C, target, p->name);
    getter->hdr.type = p->type;
    return lower_pattern(V, p->pat, getter);
}

static struct HirExpr *lower_struct_pat(struct HirVisitor *V, struct HirStructPat *p, struct HirExpr *target)
{
    struct Compiler *C = V->C;
    struct HirExpr *prev = NULL;
    for (int i = 0; i < p->fields->count; ++i) {
        struct HirPat *pat = K_LIST_GET(p->fields, i);
        struct HirExpr *next = lower_pattern(V, pat, target);
        if (next != NULL) prev = prev != NULL
            ? compose_and(C, prev, next)
            : next;
    }
    return prev;
}

static void fill_in_discr(struct Compiler *C, struct HirSwitchDiscr *r, struct HirPath *path)
{
    const struct HirSegment *last = K_LIST_GET(path, path->count - 1);
    struct HirDecl *decl = pawHir_get_decl(C, last->did);
    r->expect = HirGetVariantDecl(decl)->index;
}

static struct HirExpr *lower_variant_pat(struct HirVisitor *V, struct HirVariantPat *p, struct HirExpr *target)
{
    struct Compiler *C = V->C;
    struct HirExpr *result = pawHir_new_expr(C, p->line, kHirSwitchDiscr);
    struct HirSwitchDiscr *r = HirGetSwitchDiscr(result);
    r->target = target;

    struct HirType *type = p->type;
    fill_in_discr(C, r, p->path);

    if (p->fields == NULL) return result;
    for (int i = 0; i < p->fields->count; ++i) {
        struct HirPat *field = K_LIST_GET(p->fields, i);
        struct HirExpr *getter = new_variant_selector(C, target, type, i);
        struct HirExpr *next = lower_pattern(V, field, getter);
        if (next != NULL) result = compose_and(C, result, next);
    }
    return result;
}

static struct HirExpr *lower_path_pat(struct HirVisitor *V, struct HirPathPat *p, struct HirExpr *target)
{
    struct Compiler *C = V->C;
    struct HirExpr *result = pawHir_new_expr(C, p->line, kHirSwitchDiscr);
    struct HirSwitchDiscr *r = HirGetSwitchDiscr(result);
    r->target = target;

    fill_in_discr(C, r, p->path);
    return result;
}

// TODO: handle ()
static struct HirExpr *lower_tuple_pat(struct HirVisitor *V, struct HirTuplePat *p, struct HirExpr *target)
{
    struct Compiler *C = V->C;

    struct HirExpr *prev = NULL;
    for (int i = 0; i < p->elems->count; ++i) {
        struct HirPat *elem = K_LIST_GET(p->elems, i);
        struct HirExpr *getter = new_tuple_selector(C, target, i);
        struct HirExpr *next = lower_pattern(V, elem, getter);
        if (next != NULL) prev = prev != NULL
            ? compose_and(C, prev, next)
            : next;
    }
    return prev;
}

static struct HirExpr *lower_literal_pat(struct HirVisitor *V, struct HirLiteralPat *p, struct HirExpr *target)
{
    struct Compiler *C = V->C;
    struct HirExpr *result = pawHir_new_expr(C, p->line, kHirBinOpExpr);
    struct HirBinOpExpr *r = HirGetBinOpExpr(result);
    r->op = BINARY_EQ;
    r->lhs = target; // TODO: make a copy and fix the line number
    r->rhs = p->expr;
    return result;
}

static struct HirExpr *lower_or_pat(struct HirVisitor *V, struct HirOrPat *p, struct HirExpr *target)
{
    struct Compiler *C = V->C;
    struct HirExpr *lhs = lower_pattern(V, p->lhs, target);
    struct HirExpr *rhs = lower_pattern(V, p->rhs, target);
    return compose_or(C, lhs, rhs);
}

static struct HirExpr *lower_pattern_aux(struct HirVisitor *V, struct HirPat *pat, struct HirExpr *expr)
{
    switch (HIR_KINDOF(pat)) {
        case kHirFieldPat:
            return lower_field_pat(V, HirGetFieldPat(pat), expr);
        case kHirStructPat:
            return lower_struct_pat(V, HirGetStructPat(pat), expr);
        case kHirVariantPat:
            return lower_variant_pat(V, HirGetVariantPat(pat), expr);
        case kHirTuplePat:
            return lower_tuple_pat(V, HirGetTuplePat(pat), expr);
        case kHirPathPat:
            return lower_path_pat(V, HirGetPathPat(pat), expr);
        case kHirBindingPat:
            return lower_binding_pat(V, HirGetBindingPat(pat), expr);
        case kHirWildcardPat:
            return lower_wildcard_pat(V, HirGetWildcardPat(pat), expr);
        case kHirLiteralPat:
            return lower_literal_pat(V, HirGetLiteralPat(pat), expr);
        case kHirOrPat:
            return lower_or_pat(V, HirGetOrPat(pat), expr);
    }

}

static struct HirExpr *lower_pattern(struct HirVisitor *V, struct HirPat *pat, struct HirExpr *expr)
{
    struct HirExpr *result = lower_pattern_aux(V, pat, expr);
    if (result != NULL) return result;
    // return literal 'true'
    result = pawHir_new_expr(V->C, pat->hdr.line, kHirLiteralExpr);
    HirGetLiteralExpr(result)->lit_kind = kHirLitBasic;
    HirGetLiteralExpr(result)->basic.t = PAW_TBOOL;
    HirGetLiteralExpr(result)->basic.value.i = 1;
    return result;
}

static struct HirStmt *prefix_with_bindings(struct LowerMatches *L, struct HirBlock *block)
{
    struct HirStmtList *result = pawHir_stmt_list_new(L->C);
    struct HirDeclList *bindings = L->bs->bindings;
    for (int i = 0; i < bindings->count; ++i) {
        struct HirDecl *binding = K_LIST_GET(bindings, i);
        struct HirStmt *stmt = pawHir_new_stmt(L->C, binding->hdr.line, kHirDeclStmt);
        HirGetDeclStmt(stmt)->decl = binding;
        pawHir_stmt_list_push(L->C, result, stmt);
    }
    for (int i = 0; i < block->stmts->count; ++i) {
        pawHir_stmt_list_push(L->C, result, K_LIST_GET(block->stmts, i));
    }
    block->stmts = result;
    return HIR_CAST_STMT(block);
}

static void lower_match_arm(struct HirVisitor *V, struct HirMatchArm copy, struct HirStmt *stmt, struct HirStmt *prev, struct HirExpr *var)
{
    struct BindingState bs;
    enter_arm(V->ud, &bs);

    stmt->hdr.kind = kHirIfStmt;
    struct HirIfStmt *ifelse = HirGetIfStmt(stmt);
    ifelse->cond = lower_pattern(V, copy.guard, var);
    ifelse->then_arm = prefix_with_bindings(V->ud, copy.result);
    if (prev != NULL) HirGetIfStmt(prev)->else_arm = stmt;
    pawHir_visit_stmt(V, ifelse->then_arm);

    leave_arm(V->ud);
}

static void lower_match_stmt(struct HirVisitor *V, struct HirExpr *target, struct HirStmtList *arms, struct HirBlock *result)
{
    struct Compiler *C = V->C;

    // store the target expression in a temporary: it should not be evaluated
    // more than once
    struct HirStmt *decl_stmt = pawHir_new_stmt(C, result->line, kHirDeclStmt);
    struct HirDecl *var_decl = pawHir_new_decl(C, result->line, kHirVarDecl);
    HirGetDeclStmt(decl_stmt)->decl = var_decl;
    HirGetVarDecl(var_decl)->name = SCAN_STRING(C, "(target)");
    HirGetVarDecl(var_decl)->type = HIR_TYPEOF(target);
    HirGetVarDecl(var_decl)->init = target;
    pawHir_add_decl(C, var_decl);

    pawHir_stmt_list_push(C, result->stmts, decl_stmt);
    pawHir_stmt_list_push(C, result->stmts, K_LIST_GET(arms, 0));

    struct HirStmt *prev = NULL;
    for (int i = 0; i < arms->count; ++i) {
        struct HirStmt *stmt = K_LIST_GET(arms, i);
        struct HirMatchArm arm = *HirGetMatchArm(stmt);
        struct HirExpr *target = new_path_expr(C, var_decl, result->line);
        lower_match_arm(V, arm, stmt, prev, target);
        prev = stmt;
    }
}

static paw_Bool visit_match_stmt(struct HirVisitor *V, struct HirMatchStmt *s)
{
    pawP_check_exhaustiveness(V->C, s);

    struct HirMatchStmt copy = *s;
    struct HirBlock *result = CAST(struct HirBlock *, s);
    result->stmts = pawHir_stmt_list_new(V->C);

    s->kind = kHirBlock;
    lower_match_stmt(V, copy.target, copy.arms, result);
    return PAW_FALSE;
}

void pawP_lower_matches(struct Compiler *C)
{
    struct LowerMatches L = {
        .P = ENV(C),
        .C = C,
    };

    struct HirVisitor V;
    pawHir_visitor_init(&V, C, &L);

    V.VisitMatchStmt = visit_match_stmt;
    V.PostVisitMatchStmt = NULL;

    struct ModuleList *modules = C->dm->modules;
    for (int did = 0; did < modules->count; ++did) {
        struct ModuleInfo *m = K_LIST_GET(modules, did);
        pawHir_visit_decl_list(&V, m->hir->items);
    }
}
