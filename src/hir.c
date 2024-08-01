// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "hir.h"
#include "compile.h"
#include "map.h"
#include "mem.h"
#include <inttypes.h>
#include <limits.h>
#include <stdlib.h>

#define LIST_MIN 8

#define MAYBE_VISIT_LIST(V, list, L) ((list) != NULL ? (V)->Visit##L(V, list) : paw_unused(NULL))
#define MAYBE_FOLD_LIST(V, list, L) ((list) != NULL ? (V)->Fold##L(V, list) : NULL)

#define FIRST_ARENA_SIZE 512
#define LARGE_ARENA_MIN 32

static void add_builtin_type(struct Hir *hir, enum HirTypeKind kind, paw_Type code)
{
    hir->builtin[code] = pawHir_new_type(hir, kind);
    hir->builtin[code]->adt.base = code;
    hir->builtin[code]->adt.did = code;
}

struct Hir *pawHir_new(struct Compiler *C)
{
    paw_Env *P = ENV(C);
    struct Hir *hir = pawM_new(P, struct Hir);
    hir->dm = C->dm;
    hir->P = P;

    // initialize memory pools for storing HIR components
    pawK_pool_init(P, &hir->pool, FIRST_ARENA_SIZE, sizeof(void *) * LARGE_ARENA_MIN);
    hir->symtab = pawHir_new_symtab(hir);

    add_builtin_type(hir, kHirAdt, PAW_TUNIT);
    add_builtin_type(hir, kHirAdt, PAW_TBOOL);
    add_builtin_type(hir, kHirAdt, PAW_TINT);
    add_builtin_type(hir, kHirAdt, PAW_TFLOAT);
    add_builtin_type(hir, kHirAdt, PAW_TSTRING);
    add_builtin_type(hir, kHirAdt, PAW_TVECTOR);
    add_builtin_type(hir, kHirAdt, PAW_TMAP);
    return hir;
}

void pawHir_free_hir(struct Hir *hir)
{
    paw_Env *P = ENV(hir);
    pawK_pool_uninit(P, &hir->pool);
    pawM_free(P, hir);
}

#define NEW_NODE(hir, T) pawK_pool_alloc( \
        ENV(hir), &(hir)->pool,           \
        sizeof(struct T))
#define DEFINE_NODE_CONSTRUCTOR(name, T)                                \
        struct T *pawHir_new_##name(struct Hir *hir, enum T##Kind kind) \
        {                                                               \
            struct T *r = NEW_NODE(hir, T);                             \
            r->hdr.line = -1 /*TODO*/;                                  \
            r->hdr.kind = kind;                                         \
            return r;                                                   \
        }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)

#define LIST_MIN_ALLOC 8

struct HirType *pawHir_new_type(struct Hir *hir, enum HirTypeKind kind)
{
    struct HirType *r = NEW_NODE(hir, HirType);
    r->hdr.kind = kind;
    return r;
}

struct HirSegment *pawHir_segment_new(struct Hir *hir)
{
    return NEW_NODE(hir, HirSegment);
}

DefId pawHir_add_decl(struct Hir *hir, struct HirDecl *decl)
{
    paw_Env *P = ENV(hir);
    struct DynamicMem *dm = hir->dm;
    pawM_grow(P, dm->decls.data, dm->decls.size, dm->decls.alloc);
    const DefId id = dm->decls.size++;
    dm->decls.data[id] = decl;
    decl->hdr.did = id;
    return id;
}

struct HirDecl *pawHir_get_decl(struct Hir *hir, DefId did)
{
    struct DynamicMem *dm = hir->dm;
    paw_assert(did < dm->decls.size);
    return dm->decls.data[did];
}

struct HirSymbol *pawHir_new_symbol(struct Hir *hir)
{
    return pawK_pool_alloc(ENV(hir), &hir->pool, sizeof(struct HirSymbol));
}

static struct HirScope *new_scope(struct Hir *hir)
{
    struct HirScope *scope = pawK_pool_alloc(ENV(hir), &hir->pool, sizeof(struct HirScope));
    scope->symbols = pawHir_symbol_list_new(hir);
    return scope;
}

struct HirSymtab *pawHir_new_symtab(struct Hir *hir)
{
    struct HirSymtab *symtab = pawK_pool_alloc(ENV(hir), &hir->pool, sizeof(struct HirSymtab));
    symtab->globals = new_scope(hir);
    symtab->scopes = pawHir_scope_list_new(hir);
    return symtab;
}

struct HirScope *pawHir_new_scope(struct Hir *hir, struct HirSymtab *table)
{
    struct HirScope *scope = new_scope(hir);
    pawHir_add_scope(hir, table, scope);
    return scope;
}

void pawHir_add_scope(struct Hir *hir, struct HirSymtab *table, struct HirScope *scope)
{
    if (table->scopes->count == UINT16_MAX) {
        pawC_throw(ENV(hir), PAW_EMEMORY);
//        limit_error(hir->C, "scopes", UINT16_MAX); // TODO
    }
    pawHir_scope_list_push(hir, &table->scopes, scope);
}

struct HirSymbol *pawHir_add_symbol(struct Hir *hir, struct HirScope *table)
{
    struct HirSymbol *symbol = pawHir_new_symbol(hir);
    pawHir_symbol_list_push(hir, &table->symbols, symbol);
    return symbol;
}

int pawHir_find_symbol(struct HirScope *scope, const String *name)
{
    struct HirSymbolList *list = scope->symbols;
    for (int i = list->count - 1; i >= 0; --i) {
        const struct HirSymbol *symbol = list->data[i];
        if (pawS_eq(name, symbol->name)) {
            if (symbol->is_init) {
                return i;
            }
        }
    }
    return -1;
}

#define DEFINE_LIST_VISITOR(name, T)                                                     \
    static void visit_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    {                                                                                    \
        for (int i = 0; i < list->count; ++i) {                                          \
            V->Visit##T(V, list->data[i]);                                               \
        }                                                                                \
    }
DEFINE_LIST_VISITOR(decl, Decl) 
DEFINE_LIST_VISITOR(expr, Expr)
DEFINE_LIST_VISITOR(stmt, Stmt)

static void VisitBlock(struct HirVisitor *V, struct HirBlock *s)
{
    V->VisitStmtList(V, s->stmts);
}

static void VisitLogicalExpr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);
}

static void VisitMapItem(struct HirVisitor *V, struct HirMapItem *e)
{
    V->VisitExpr(V, e->key);
    V->VisitExpr(V, e->value);
}

static void VisitStructItem(struct HirVisitor *V, struct HirStructItem *e)
{
    V->VisitExpr(V, e->value);
}

static void VisitLiteralExpr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitComposite:
            V->VisitPath(V, e->comp.path);
            V->VisitExprList(V, e->comp.items);
            break;
        case kHirLitContainer:
            V->VisitExprList(V, e->cont.items);
            break;
        case kHirLitTuple:
            V->VisitExprList(V, e->tuple.elems);
            break;
        default:
            break;
    }
}

static void VisitChainExpr(struct HirVisitor *V, struct HirChainExpr *e)
{
    V->VisitExpr(V, e->target);
}

static void VisitUnOpExpr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    V->VisitExpr(V, e->target);
}

static void VisitBinOpExpr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);
}

static void VisitExprStmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    V->VisitExpr(V, s->lhs);
    V->VisitExpr(V, s->rhs);
}

static void VisitClosureExpr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    V->VisitDeclList(V, e->params);
    V->VisitBlock(V, e->body);
}

static void VisitFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    paw_unused(V);
    paw_unused(d);
}

static void VisitTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    MAYBE_VISIT_LIST(V, d->generics, DeclList);
    V->VisitExpr(V, d->rhs);
}

static void VisitGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    paw_unused(V);
    paw_unused(d);
}

static void VisitVariantDecl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    V->VisitDeclList(V, d->fields);
}

static void VisitAdtDecl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    MAYBE_VISIT_LIST(V, d->generics, DeclList);
    V->VisitDeclList(V, d->fields);
}

static void VisitInstanceDecl(struct HirVisitor *V, struct HirInstanceDecl *d)
{
    V->VisitDeclList(V, d->types);
    V->VisitDeclList(V, d->fields);
}

static void VisitVarDecl(struct HirVisitor *V, struct HirVarDecl *d)
{
    V->VisitExpr(V, d->init);
}

static void VisitReturnStmt(struct HirVisitor *V, struct HirReturnStmt *s)
{
    V->VisitExpr(V, s->expr);
}

static void VisitCallExpr(struct HirVisitor *V, struct HirCallExpr *e)
{
    V->VisitExpr(V, e->target);
    V->VisitExprList(V, e->args);
}

static void VisitConversionExpr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    V->VisitExpr(V, e->arg);
}

static void VisitPathExpr(struct HirVisitor *V, struct HirPathExpr *e)
{
    V->VisitPath(V, e->path);
}

static void VisitFuncDecl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    MAYBE_VISIT_LIST(V, d->generics, DeclList);
    V->VisitDeclList(V, d->params);
    V->VisitBlock(V, d->body);
}

static void VisitIfStmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    V->VisitExpr(V, s->cond);
    V->VisitStmt(V, s->then_arm);
    V->VisitStmt(V, s->else_arm);
}

static void VisitWhileStmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    V->VisitExpr(V, s->cond);
    V->VisitBlock(V, s->block);
}

static void VisitLabelStmt(struct HirVisitor *V, struct HirLabelStmt *s)
{
    paw_unused(V);
    paw_unused(s);
}

static void VisitForStmt(struct HirVisitor *V, struct HirForStmt *s)
{
    if (s->is_fornum) {
        V->VisitExpr(V, s->fornum.begin);
        V->VisitExpr(V, s->fornum.end);
        V->VisitExpr(V, s->fornum.step);
    } else {
        V->VisitExpr(V, s->forin.target);
    }
    V->VisitBlock(V, s->block);
}

static void VisitIndex(struct HirVisitor *V, struct HirIndex *e)
{
    V->VisitExpr(V, e->target);
    V->VisitExpr(V, e->first);
}

static void VisitSelector(struct HirVisitor *V, struct HirSelector *e)
{
    V->VisitExpr(V, e->target);
}

static void VisitPath(struct HirVisitor *V, struct HirPath *path)
{
    paw_unused(V);
    paw_unused(path);
}

static void VisitDeclStmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    V->VisitDecl(V, s->decl);
}

static void VisitExpr(struct HirVisitor *V, struct HirExpr *node)
{
    if (node == NULL) {
        return;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b);        \
        break;
        HIR_EXPR_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

static void VisitDecl(struct HirVisitor *V, struct HirDecl *node)
{
    if (node == NULL) {
        return;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b);        \
        break;
        HIR_DECL_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

static void VisitStmt(struct HirVisitor *V, struct HirStmt *node)
{
    if (node == NULL) {
        return;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b);        \
        break;
        HIR_STMT_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

void pawHir_visitor_init(struct HirVisitor *V, struct Hir *hir, union HirState state)
{
    *V = (struct HirVisitor){
        .hir = hir,
        .state = state,

        .VisitExpr = VisitExpr,
        .VisitPath = VisitPath,
        .VisitDecl = VisitDecl,
        .VisitStmt = VisitStmt,

        .VisitExprList = visit_expr_list,
        .VisitDeclList = visit_decl_list,
        .VisitStmtList = visit_stmt_list,

#define DEFINE_CALLBACKS(a, b) .Visit##a = Visit##a,
        HIR_EXPR_LIST(DEFINE_CALLBACKS)
        HIR_DECL_LIST(DEFINE_CALLBACKS)
        HIR_STMT_LIST(DEFINE_CALLBACKS)
#undef DEFINE_CALLBACKS
    };
}

void pawHir_visit(struct HirVisitor *V)
{
    struct Hir *hir = V->hir;
    V->VisitStmtList(V, hir->stmts);
}

// Generate code for folding a list of HIR nodes
#define DEFINE_LIST_FOLDER(name, T)                                                                \
    static struct Hir##T##List *fold_##name##_list(struct HirFolder *F, struct Hir##T##List *list) \
    {                                                                                              \
        for (int i = 0; i < list->count; ++i) {                                                    \
            list->data[i] = F->Fold##T(F, list->data[i]);                                          \
        }                                                                                          \
        return list;                                                                               \
    } 
DEFINE_LIST_FOLDER(decl, Decl) 
DEFINE_LIST_FOLDER(expr, Expr)
DEFINE_LIST_FOLDER(stmt, Stmt)

static struct HirStmt *FoldBlock(struct HirFolder *F, struct HirBlock *s)
{
    s->stmts = F->FoldStmtList(F, s->stmts);
    return HIR_CAST_STMT(s);
}
#define FOLD_BLOCK(F, s) cast((F)->FoldBlock(F, s), struct HirBlock *)

static struct HirExpr *FoldLogicalExpr(struct HirFolder *F, struct HirLogicalExpr *e)
{
    e->lhs = F->FoldExpr(F, e->lhs);
    e->rhs = F->FoldExpr(F, e->rhs);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldMapItem(struct HirFolder *F, struct HirMapItem *e)
{
    e->key = F->FoldExpr(F, e->key);
    e->value = F->FoldExpr(F, e->value);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldStructItem(struct HirFolder *F, struct HirStructItem *e)
{
    e->value = F->FoldExpr(F, e->value);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldLiteralExpr(struct HirFolder *F, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitComposite:
            e->comp.path = F->FoldPath(F, e->comp.path);
            e->comp.items = F->FoldExprList(F, e->comp.items);
            break;
        case kHirLitTuple:
            e->tuple.elems = F->FoldExprList(F, e->tuple.elems);
            break;
        default:
            break;
    }
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldChainExpr(struct HirFolder *F, struct HirChainExpr *e)
{
    e->target = F->FoldExpr(F, e->target);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldUnOpExpr(struct HirFolder *F, struct HirUnOpExpr *e)
{
    e->target = F->FoldExpr(F, e->target);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldBinOpExpr(struct HirFolder *F, struct HirBinOpExpr *e)
{
    e->lhs = F->FoldExpr(F, e->lhs);
    e->rhs = F->FoldExpr(F, e->rhs);
    return HIR_CAST_EXPR(e);
}

static struct HirStmt *FoldExprStmt(struct HirFolder *F, struct HirExprStmt *s)
{
    s->lhs = F->FoldExpr(F, s->lhs);
    s->rhs = F->FoldExpr(F, s->rhs);
    return HIR_CAST_STMT(s);
}

static struct HirExpr *FoldClosureExpr(struct HirFolder *F, struct HirClosureExpr *e)
{
    e->params = F->FoldDeclList(F, e->params);
    e->body = FOLD_BLOCK(F, e->body);
    return HIR_CAST_EXPR(e);
}

static struct HirDecl *FoldFieldDecl(struct HirFolder *F, struct HirFieldDecl *d)
{
    paw_unused(F);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldTypeDecl(struct HirFolder *F, struct HirTypeDecl *d)
{
    d->generics = MAYBE_FOLD_LIST(F, d->generics, DeclList);
    d->rhs = F->FoldExpr(F, d->rhs);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldGenericDecl(struct HirFolder *F, struct HirGenericDecl *d)
{
    paw_unused(F);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldVariantDecl(struct HirFolder *F, struct HirVariantDecl *d)
{
    d->fields = F->FoldDeclList(F, d->fields);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldAdtDecl(struct HirFolder *F, struct HirAdtDecl *d)
{
    d->generics = MAYBE_FOLD_LIST(F, d->generics, DeclList);
    d->fields = F->FoldDeclList(F, d->fields);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldInstanceDecl(struct HirFolder *F, struct HirInstanceDecl *d)
{
    d->types = F->FoldDeclList(F, d->types);
    d->fields = F->FoldDeclList(F, d->fields);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldVarDecl(struct HirFolder *F, struct HirVarDecl *d)
{
    d->init = F->FoldExpr(F, d->init);
    return HIR_CAST_DECL(d);
}

static struct HirStmt *FoldReturnStmt(struct HirFolder *F, struct HirReturnStmt *s)
{
    s->expr = F->FoldExpr(F, s->expr);
    return HIR_CAST_STMT(s);
}

static struct HirExpr *FoldCallExpr(struct HirFolder *F, struct HirCallExpr *e)
{
    e->target = F->FoldExpr(F, e->target);
    e->args = F->FoldExprList(F, e->args);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldConversionExpr(struct HirFolder *F, struct HirConversionExpr *e)
{
    e->arg = F->FoldExpr(F, e->arg);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldPathExpr(struct HirFolder *F, struct HirPathExpr *e)
{
    e->path = F->FoldPath(F, e->path);
    return HIR_CAST_EXPR(e);
}

static struct HirDecl *FoldFuncDecl(struct HirFolder *F, struct HirFuncDecl *d)
{
    d->generics = MAYBE_FOLD_LIST(F, d->generics, DeclList);
    d->params = F->FoldDeclList(F, d->params);
    d->body = FOLD_BLOCK(F, d->body);
    return HIR_CAST_DECL(d);
}

static struct HirStmt *FoldIfStmt(struct HirFolder *F, struct HirIfStmt *s)
{
    s->cond = F->FoldExpr(F, s->cond);
    s->then_arm = F->FoldStmt(F, s->then_arm);
    s->else_arm = F->FoldStmt(F, s->else_arm);
    return HIR_CAST_STMT(s);
}

static struct HirStmt *FoldWhileStmt(struct HirFolder *F, struct HirWhileStmt *s)
{
    s->cond = F->FoldExpr(F, s->cond);
    s->block = FOLD_BLOCK(F, s->block);
    return HIR_CAST_STMT(s);
}

static struct HirStmt *FoldLabelStmt(struct HirFolder *F, struct HirLabelStmt *s)
{
    paw_unused(F);
    return HIR_CAST_STMT(s);
}

static struct HirStmt *FoldForStmt(struct HirFolder *F, struct HirForStmt *s)
{
    if (s->is_fornum) {
        s->fornum.begin = F->FoldExpr(F, s->fornum.begin);
        s->fornum.end = F->FoldExpr(F, s->fornum.end);
        s->fornum.step = F->FoldExpr(F, s->fornum.step);
    } else {
        s->forin.target = F->FoldExpr(F, s->forin.target);
    }
    s->block = FOLD_BLOCK(F, s->block);
    return HIR_CAST_STMT(s);
}

static struct HirExpr *FoldIndex(struct HirFolder *F, struct HirIndex *e)
{
    e->target = F->FoldExpr(F, e->target);
    e->first = F->FoldExpr(F, e->first);
    e->second = F->FoldExpr(F, e->second);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldSelector(struct HirFolder *F, struct HirSelector *e)
{
    e->target = F->FoldExpr(F, e->target);
    return HIR_CAST_EXPR(e);
}

static struct HirPath *FoldPath(struct HirFolder *F, struct HirPath *path)
{
    paw_unused(F);
    return path;
}

static struct HirStmt *FoldDeclStmt(struct HirFolder *F, struct HirDeclStmt *s)
{
    s->decl = F->FoldDecl(F, s->decl);
    return HIR_CAST_STMT(s);
}

static struct HirExpr *FoldExpr(struct HirFolder *F, struct HirExpr *node)
{
    if (node == NULL) {
        return NULL;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
        return F->Fold##a(F, &node->b);
        HIR_EXPR_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
}

static struct HirDecl *FoldDecl(struct HirFolder *F, struct HirDecl *node)
{
    if (node == NULL) {
        return NULL;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
        return F->Fold##a(F, &node->b);
        HIR_DECL_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
}

static struct HirStmt *FoldStmt(struct HirFolder *F, struct HirStmt *node)
{
    if (node == NULL) {
        return NULL;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
        return F->Fold##a(F, &node->b);
        HIR_STMT_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
}

void pawHir_fold_init(struct HirFolder *F, struct Hir *hir, union HirState state)
{
    *F = (struct HirFolder){
        .hir = hir,
        .state = state,

        .FoldExpr = FoldExpr,
        .FoldPath = FoldPath,
        .FoldDecl = FoldDecl,
        .FoldStmt = FoldStmt,
        .FoldExprList = fold_expr_list,
        .FoldDeclList = fold_decl_list,
        .FoldStmtList = fold_stmt_list,

#define DEFINE_CALLBACKS(a, b) .Fold##a = Fold##a,
        HIR_EXPR_LIST(DEFINE_CALLBACKS)
        HIR_DECL_LIST(DEFINE_CALLBACKS)
        HIR_STMT_LIST(DEFINE_CALLBACKS)
#undef DEFINE_CALLBACKS
    };
}

void pawHir_fold(struct HirFolder *F)
{
    struct Hir *hir = F->hir;
    F->FoldStmtList(F, hir->stmts);
}

static struct HirTypeList *FoldTypeList(struct HirTypeFolder *F, struct HirTypeList *list)
{
    // NOTE: Number of types in the list should never change.
    for (int i = 0; i < list->count; ++i) {
        list->data[i] = F->Fold(F, list->data[i]);
    }
    return list;
}

static struct HirType *FoldFuncDef(struct HirTypeFolder *F, struct HirFuncDef *t)
{
    t->types = MAYBE_FOLD_LIST(F, t->types, List);
    t->params = F->FoldList(F, t->params);
    t->result = F->Fold(F, t->result);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldFuncPtr(struct HirTypeFolder *F, struct HirFuncPtr *t)
{
    t->params = F->FoldList(F, t->params);
    t->result = F->Fold(F, t->result);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldTupleType(struct HirTypeFolder *F, struct HirTupleType *t)
{
    t->elems = F->FoldList(F, t->elems);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldAdt(struct HirTypeFolder *F, struct HirAdt *t)
{
    if (t->types != NULL) {
        t->types = F->FoldList(F, t->types);
    }
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldUnknown(struct HirTypeFolder *F, struct HirUnknown *t)
{
    paw_unused(F);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldGeneric(struct HirTypeFolder *F, struct HirGeneric *t)
{
    paw_unused(F);
    return HIR_CAST_TYPE(t);
}

void pawHir_type_folder_init(struct HirTypeFolder *F, void *state)
{
    *F = (struct HirTypeFolder){
        .state = state,
        .Fold = pawHir_fold_type,
        .FoldList = FoldTypeList,
        .FoldTupleType = FoldTupleType,
        .FoldFuncPtr = FoldFuncPtr,
        .FoldFuncDef = FoldFuncDef,
        .FoldAdt = FoldAdt,
        .FoldUnknown = FoldUnknown,
        .FoldGeneric = FoldGeneric,
    };
}

struct HirType *pawHir_fold_type(struct HirTypeFolder *F, struct HirType *type)
{
    switch (y_kind(type)) {
        case kHirGeneric:
            return F->FoldGeneric(F, &type->generic);
        case kHirUnknown:
            return F->FoldUnknown(F, &type->unknown);
        case kHirAdt:
            return F->FoldAdt(F, &type->adt);
        case kHirFuncPtr:
            return F->FoldFuncPtr(F, &type->fptr);
        case kHirTupleType:
            return F->FoldTupleType(F, &type->tuple);
        default:
            paw_assert(HirIsFuncDef(type));
            return F->FoldFuncDef(F, &type->fdef);
    }
}

// ****************************
//     HIR copying routines
// ****************************

typedef struct HirCopier {
    struct Hir *hir;
    paw_Env *P;
} HirCopier;

#define DEFINE_COPY_PREP(name, T)                               \
    static T *copy_prep_##name##_aux(struct HirFolder *F, T *t) \
    {                                                           \
        T *r = pawHir_new_##name(F->hir, HIR_KINDOF(t));        \
        r->hdr = t->hdr;                                        \
        return r;                                               \
    }
DEFINE_COPY_PREP(expr, struct HirExpr) 
DEFINE_COPY_PREP(decl, struct HirDecl)
DEFINE_COPY_PREP(stmt, struct HirStmt)

// Helpers for copying: create a new node of the given type and kind,
// and copy the common fields
#define copy_prep_expr(F, e) copy_prep_expr_aux(F, HIR_CAST_EXPR(e))
#define copy_prep_decl(F, d) copy_prep_decl_aux(F, HIR_CAST_DECL(d))
#define copy_prep_stmt(F, s) copy_prep_stmt_aux(F, HIR_CAST_STMT(s))

#define DEFINE_COPY_LIST(name, T)                                                                  \
    static struct Hir##T##List *copy_##name##s(struct HirFolder *F, struct Hir##T##List *old_list) \
    {                                                                                              \
        struct Hir##T##List *new_list = pawHir_##name##_list_new(F->hir);                          \
        for (int i = 0; i < old_list->count; ++i) {                                                \
            pawHir_##name##_list_push(F->hir, &new_list, old_list->data[i]);                       \
        }                                                                                          \
        F->Fold##T##List(F, new_list);                                                             \
        return new_list;                                                                           \
    }
DEFINE_COPY_LIST(decl, Decl) 
DEFINE_COPY_LIST(expr, Expr)
DEFINE_COPY_LIST(stmt, Stmt)

static struct HirStmt *copy_block_stmt(struct HirFolder *F, struct HirBlock *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->block.stmts = copy_stmts(F, s->stmts);
    return r;
}

#define COPY_BLOCK(F, s) cast((F)->FoldBlock(F, s), struct HirBlock *)

static struct HirExpr *copy_logical_expr(struct HirFolder *F, struct HirLogicalExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->logical.lhs = F->FoldExpr(F, e->lhs);
    r->logical.rhs = F->FoldExpr(F, e->rhs);
    return r;
}

static struct HirExpr *copy_map_item(struct HirFolder *F, struct HirMapItem *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->mitem.key = F->FoldExpr(F, e->key);
    r->mitem.value = F->FoldExpr(F, e->value);
    return r;
}

static struct HirExpr *copy_struct_item(struct HirFolder *F, struct HirStructItem *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->sitem.name = e->name;
    r->sitem.value = F->FoldExpr(F, e->value);
    r->sitem.index = e->index;
    return r;
}

static struct HirExpr *copy_literal_expr(struct HirFolder *F, struct HirLiteralExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->literal.lit_kind = e->lit_kind;
    switch (e->lit_kind) {
        case kHirLitBasic:
            r->literal.basic = e->basic;
            break;
        case kHirLitTuple:
            r->literal.tuple.elems = copy_exprs(F, e->tuple.elems);
            break;
        case kHirLitContainer:
            r->literal.cont.code = e->cont.code;
            r->literal.cont.items = copy_exprs(F, e->comp.items);
            break;
        default:
            paw_assert(e->lit_kind == kHirLitComposite);
            r->literal.comp.path = F->FoldPath(F, e->comp.path);
            r->literal.comp.items = copy_exprs(F, e->comp.items);
            break;
    }
    return r;
}

static struct HirExpr *copy_chain_expr(struct HirFolder *F, struct HirChainExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->chain.target = F->FoldExpr(F, e->target);
    return r;
}

static struct HirExpr *copy_unop_expr(struct HirFolder *F, struct HirUnOpExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->unop.target = F->FoldExpr(F, e->target);
    r->unop.op = e->op;
    return r;
}

static struct HirExpr *copy_binop_expr(struct HirFolder *F, struct HirBinOpExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->binop.lhs = F->FoldExpr(F, e->lhs);
    r->binop.rhs = F->FoldExpr(F, e->rhs);
    r->binop.op = e->op;
    return r;
}

static struct HirStmt *copy_expr_stmt(struct HirFolder *F, struct HirExprStmt *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->expr.lhs = F->FoldExpr(F, s->lhs);
    r->expr.rhs = F->FoldExpr(F, s->rhs);
    return r;
}

static struct HirExpr *copy_closure_expr(struct HirFolder *F, struct HirClosureExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->clos.params = copy_decls(F, e->params);
    r->clos.body = COPY_BLOCK(F, e->body);
    return r;
}

static struct HirDecl *copy_field_decl(struct HirFolder *F, struct HirFieldDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
    r->field.name = d->name;
    return r;
}

static struct HirDecl *copy_type_decl(struct HirFolder *F, struct HirTypeDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
    r->type.name = d->name;
    r->type.generics = copy_decls(F, d->generics);
    r->type.rhs = F->FoldExpr(F, d->rhs);
    return r;
}

static struct HirDecl *copy_generic_decl(struct HirFolder *F, struct HirGenericDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
    r->generic.name = d->name;
    return r;
}

static struct HirDecl *copy_adt_decl(struct HirFolder *F, struct HirAdtDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
    r->adt.is_global = d->is_global;
    r->adt.name = d->name;
    r->adt.generics = copy_decls(F, d->generics);
    r->adt.fields = copy_decls(F, d->fields);
    r->adt.monos = copy_decls(F, d->monos);
    return r;
}

static struct HirDecl *copy_variant_decl(struct HirFolder *F, struct HirVariantDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
    r->variant.name = d->name;
    r->variant.fields = copy_decls(F, d->fields);
    r->variant.index = d->index;
    return r;
}

static struct HirDecl *copy_var_decl(struct HirFolder *F, struct HirVarDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
    r->var.is_global = d->is_global;
    r->var.is_const = d->is_const;
    r->var.name = d->name;
    r->var.init = F->FoldExpr(F, d->init);
    return r;
}

static struct HirStmt *copy_return_stmt(struct HirFolder *F, struct HirReturnStmt *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->result.expr = F->FoldExpr(F, s->expr);
    return r;
}

static struct HirExpr *copy_conversion_expr(struct HirFolder *F, struct HirConversionExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->conv.arg = F->FoldExpr(F, e->arg);
    r->conv.to = e->to;
    return r;
}

static struct HirExpr *copy_call_expr(struct HirFolder *F, struct HirCallExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->call.target = F->FoldExpr(F, e->target);
    r->call.args = copy_exprs(F, e->args);
    return r;
}

static struct HirPath *copy_path(struct HirFolder *F, struct HirPath *e)
{
    struct HirPath *r = pawHir_path_new(F->hir);
    for (int i = 0; i < e->count; ++i) {
        struct HirSegment *seg = pawHir_path_get(e, i);
        pawHir_path_add(F->hir, &r, seg->name, seg->types, seg->type);
    }
    return r;
}

static struct HirExpr *copy_path_expr(struct HirFolder *F, struct HirPathExpr *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->path.path = F->FoldPath(F, e->path);
    return r;
}

static struct HirDecl *copy_func_decl(struct HirFolder *F, struct HirFuncDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
    r->func.is_global = d->is_global;
    r->func.receiver = NULL; // set during visit_*()
    r->func.name = d->name;
    r->func.generics = copy_decls(F, d->generics);
    r->func.params = copy_decls(F, d->params);
    r->func.body = COPY_BLOCK(F, d->body);
    r->func.monos = copy_decls(F, d->monos);
    r->func.fn_kind = d->fn_kind;
    return r;
}

static struct HirStmt *copy_if_stmt(struct HirFolder *F, struct HirIfStmt *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->if_.cond = F->FoldExpr(F, s->cond);
    r->if_.then_arm = F->FoldStmt(F, s->then_arm);
    r->if_.else_arm = F->FoldStmt(F, s->else_arm);
    return r;
}

static struct HirStmt *copy_while_stmt(struct HirFolder *F, struct HirWhileStmt *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->while_.is_dowhile = s->is_dowhile;
    r->while_.cond = F->FoldExpr(F, s->cond);
    r->while_.block = COPY_BLOCK(F, s->block);
    return r;
}

static struct HirStmt *copy_label_stmt(struct HirFolder *F, struct HirLabelStmt *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->label.label = s->label;
    return r;
}

static struct HirStmt *copy_for_stmt(struct HirFolder *F, struct HirForStmt *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->for_.is_fornum = s->is_fornum;
    if (s->is_fornum) {
        r->for_.fornum.begin = F->FoldExpr(F, s->fornum.begin);
        r->for_.fornum.end = F->FoldExpr(F, s->fornum.end);
        r->for_.fornum.step = F->FoldExpr(F, s->fornum.step);
    } else {
        r->for_.forin.target = F->FoldExpr(F, s->forin.target);
    }
    r->for_.block = COPY_BLOCK(F, s->block);
    return r;
}

static struct HirExpr *copy_index_expr(struct HirFolder *F, struct HirIndex *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->index.target = F->FoldExpr(F, e->target);
    r->index.first = F->FoldExpr(F, e->first);
    r->index.second = F->FoldExpr(F, e->second);
    r->index.is_slice = e->is_slice;
    return r;
}

static struct HirExpr *copy_select_expr(struct HirFolder *F, struct HirSelector *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->select.is_index = e->is_index;
    r->select.target = F->FoldExpr(F, e->target);
    r->select.name = e->name;
    return r;
}

static struct HirStmt *copy_decl_stmt(struct HirFolder *F, struct HirDeclStmt *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->decl.decl = F->FoldDecl(F, s->decl);
    return r;
}

static void setup_copy_pass(struct HirFolder *F, HirCopier *C)
{
    const union HirState state = {.C = C};
    pawHir_fold_init(F, C->hir, state);
    F->FoldLiteralExpr = copy_literal_expr;
    F->FoldLogicalExpr = copy_logical_expr;
    F->FoldPath = copy_path;
    F->FoldPathExpr = copy_path_expr;
    F->FoldChainExpr = copy_chain_expr;
    F->FoldUnOpExpr = copy_unop_expr;
    F->FoldBinOpExpr = copy_binop_expr;
    F->FoldConversionExpr = copy_conversion_expr;
    F->FoldCallExpr = copy_call_expr;
    F->FoldIndex = copy_index_expr;
    F->FoldSelector = copy_select_expr;
    F->FoldMapItem = copy_map_item;
    F->FoldStructItem = copy_struct_item;
    F->FoldClosureExpr = copy_closure_expr;
    F->FoldBlock = copy_block_stmt;
    F->FoldExprStmt = copy_expr_stmt;
    F->FoldDeclStmt = copy_decl_stmt;
    F->FoldIfStmt = copy_if_stmt;
    F->FoldForStmt = copy_for_stmt;
    F->FoldWhileStmt = copy_while_stmt;
    F->FoldLabelStmt = copy_label_stmt;
    F->FoldReturnStmt = copy_return_stmt;
    F->FoldVariantDecl = copy_variant_decl;
    F->FoldVarDecl = copy_var_decl;
    F->FoldFuncDecl = copy_func_decl;
    F->FoldAdtDecl = copy_adt_decl;
    F->FoldFieldDecl = copy_field_decl;
    F->FoldGenericDecl = copy_generic_decl;
    F->FoldTypeDecl = copy_type_decl;
}

struct HirDecl *pawHir_copy_decl(struct Hir *hir, struct HirDecl *decl)
{
    HirCopier C = {
        .P = hir->P,
        .hir = hir,
    };
    struct HirFolder F;
    setup_copy_pass(&F, &C);
    return F.FoldDecl(&F, decl);
}

// *******************************
//     HIR stenciling routines
// *******************************

typedef struct Subst {
    struct HirStenciler *S;
    struct HirTypeList *before;
    struct HirTypeList *after;
} Subst;

static struct HirType *subst_type(struct HirFolder *F, struct HirType *type);

typedef struct HirStenciler {
    struct HirTypeFolder fold;
    Subst subst;
    Map *decls;
    struct Hir *hir; // HIR being copied
    paw_Env *P;
} HirStenciler;

static struct HirDecl *find_decl(HirStenciler *S, struct HirDecl *old_decl)
{
    const Value key = {.p = old_decl};
    Value *pvalue = pawH_get(ENV(S), S->decls, key);
    if (pvalue == NULL) {
        return old_decl;
    }
    return pvalue->p;
}

static void link_decls(struct HirFolder *F, struct HirDecl *old_decl, struct HirDecl *new_decl)
{
    HirStenciler *S = F->state.S;
    const Value key = {.p = old_decl};
    const Value value = {.p = new_decl};
    pawH_insert(ENV(S), S->decls, key, value);
}

#define MAYBE_STENCIL_DECL_LIST(F, list) ((list) != NULL ? stencil_decls(F, list) : NULL)

#define DEFINE_STENCIL_PREP(name, T, body)                         \
    static T *stencil_prep_##name##_aux(struct HirFolder *F, T *t) \
    {                                                              \
        T *r = pawHir_new_##name(F->hir, HIR_KINDOF(t));           \
        r->hdr.kind = t->hdr.kind;                                 \
        r->hdr.line = t->hdr.line;                                 \
        body                                                       \
        return r;                                                  \
    }
DEFINE_STENCIL_PREP(expr, struct HirExpr, 
            { 
                r->hdr.type = subst_type(F, HIR_TYPEOF(t)); 
            })
DEFINE_STENCIL_PREP(decl, struct HirDecl,
            {
                r->hdr.name = t->hdr.name;
                r->hdr.did = pawHir_add_decl(F->hir, r);
                link_decls(F, t, r); // subst_type() is dependant
                struct HirType *type = subst_type(F, HIR_TYPEOF(t));
                if (HirIsFuncDecl(r)) {
                    type->fdef.did = r->hdr.did;
                } else if (HirIsAdtDecl(r)) {
                    type->adt.did = r->hdr.did;
                }
                r->hdr.type = type;
                return r;
            }) 
DEFINE_STENCIL_PREP(stmt, struct HirStmt, {})

// Helpers for stenciling: create a new node of the given type and kind,
// and copy the common fields
#define stencil_prep_expr(F, e) stencil_prep_expr_aux(F, HIR_CAST_EXPR(e))
#define stencil_prep_decl(F, d) stencil_prep_decl_aux(F, HIR_CAST_DECL(d))
#define stencil_prep_stmt(F, s) stencil_prep_stmt_aux(F, HIR_CAST_STMT(s))

#define DEFINE_STENCIL_LIST(name, T)                                                                  \
    static struct Hir##T##List *stencil_##name##s(struct HirFolder *F, struct Hir##T##List *old_list) \
    {                                                                                                 \
        struct Hir##T##List *new_list = pawHir_##name##_list_new(F->hir);                             \
        for (int i = 0; i < old_list->count; ++i) {                                                   \
            struct Hir##T *decl = F->Fold##T(F, old_list->data[i]);                                   \
            pawHir_##name##_list_push(F->hir, &new_list, decl);                                       \
        }                                                                                             \
        return new_list;                                                                              \
    }
DEFINE_STENCIL_LIST(expr, Expr) 
DEFINE_STENCIL_LIST(decl, Decl)
DEFINE_STENCIL_LIST(stmt, Stmt)

static struct HirStmt *stencil_block_stmt(struct HirFolder *F, struct HirBlock *s)
{
    struct HirStmt *r = stencil_prep_stmt(F, s);
    r->block.stmts = stencil_stmts(F, s->stmts);
    return r;
}

#define STENCIL_BLOCK(F, s) cast((F)->FoldBlock(F, s), struct HirBlock *)

static struct HirExpr *stencil_logical_expr(struct HirFolder *F, struct HirLogicalExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->logical.lhs = F->FoldExpr(F, e->lhs);
    r->logical.rhs = F->FoldExpr(F, e->rhs);
    return r;
}

static struct HirExpr *stencil_map_item(struct HirFolder *F, struct HirMapItem *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->mitem.key = F->FoldExpr(F, e->key);
    r->mitem.value = F->FoldExpr(F, e->value);
    return r;
}

static struct HirExpr *stencil_struct_item(struct HirFolder *F, struct HirStructItem *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->sitem.name = e->name;
    r->sitem.value = F->FoldExpr(F, e->value);
    r->sitem.index = e->index;
    return r;
}

static struct HirExpr *stencil_literal_expr(struct HirFolder *F, struct HirLiteralExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->literal.lit_kind = e->lit_kind;
    switch (e->lit_kind) {
        case kHirLitBasic:
            r->literal.basic = e->basic;
            break;
        case kHirLitTuple:
            r->literal.tuple.elems = stencil_exprs(F, e->tuple.elems);
            break;
        case kHirLitContainer:
            r->literal.cont.code = e->cont.code;
            r->literal.cont.items = stencil_exprs(F, e->cont.items);
            break;
        default:
            paw_assert(e->lit_kind == kHirLitComposite);
            r->literal.comp.path = F->FoldPath(F, e->comp.path);
            r->literal.comp.items = stencil_exprs(F, e->comp.items);
            break;
    }
    return r;
}

static struct HirExpr *stencil_chain_expr(struct HirFolder *F, struct HirChainExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->chain.target = F->FoldExpr(F, e->target);
    return r;
}

static struct HirExpr *stencil_unop_expr(struct HirFolder *F, struct HirUnOpExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->unop.target = F->FoldExpr(F, e->target);
    r->unop.op = e->op;
    return r;
}

static struct HirExpr *stencil_binop_expr(struct HirFolder *F, struct HirBinOpExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->binop.lhs = F->FoldExpr(F, e->lhs);
    r->binop.rhs = F->FoldExpr(F, e->rhs);
    r->binop.op = e->op;
    return r;
}

static struct HirStmt *stencil_expr_stmt(struct HirFolder *F, struct HirExprStmt *s)
{
    struct HirStmt *r = stencil_prep_stmt(F, s);
    r->expr.lhs = F->FoldExpr(F, s->lhs);
    r->expr.rhs = F->FoldExpr(F, s->rhs);
    return r;
}

static struct HirExpr *stencil_closure_expr(struct HirFolder *F, struct HirClosureExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->clos.params = stencil_decls(F, e->params);
    r->clos.body = STENCIL_BLOCK(F, e->body);
    return r;
}

static struct HirDecl *stencil_field_decl(struct HirFolder *F, struct HirFieldDecl *d)
{
    struct HirDecl *r = stencil_prep_decl(F, d);
    r->field.name = d->name;
    return r;
}

static struct HirDecl *stencil_type_decl(struct HirFolder *F, struct HirTypeDecl *d)
{
    struct HirDecl *r = stencil_prep_decl(F, d);
    r->type.name = d->name;
    r->type.generics = MAYBE_STENCIL_DECL_LIST(F, d->generics);
    r->type.rhs = F->FoldExpr(F, d->rhs);
    return r;
}

static struct HirDecl *stencil_generic_decl(struct HirFolder *F, struct HirGenericDecl *d)
{
    struct HirDecl *r = stencil_prep_decl(F, d);
    r->generic.name = d->name;
    return r;
}

static struct HirDecl *stencil_variant_decl(struct HirFolder *F, struct HirVariantDecl *d)
{
    struct HirDecl *r = stencil_prep_decl(F, d);
    r->variant.name = d->name;

    r->variant.fields = stencil_decls(F, d->fields);
    r->variant.index = d->index;
    return r;
}

static struct HirDecl *stencil_adt_decl(struct HirFolder *F, struct HirAdtDecl *d)
{
    struct HirDecl *r = stencil_prep_decl(F, d);

    r->adt.name = d->name;
    r->adt.is_global = d->is_global;
    r->adt.is_struct = d->is_struct;
    r->adt.generics = MAYBE_STENCIL_DECL_LIST(F, d->generics);
    r->adt.fields = stencil_decls(F, d->fields);
    r->adt.monos = MAYBE_STENCIL_DECL_LIST(F, d->monos);
    return r;
}

static struct HirDecl *stencil_var_decl(struct HirFolder *F, struct HirVarDecl *d)
{
    struct HirDecl *r = stencil_prep_decl(F, d);
    r->var.is_global = d->is_global;
    r->var.is_const = d->is_const;
    r->var.name = d->name;
    r->var.init = F->FoldExpr(F, d->init);
    return r;
}

static struct HirStmt *stencil_return_stmt(struct HirFolder *F, struct HirReturnStmt *s)
{
    struct HirStmt *r = stencil_prep_stmt(F, s);
    r->result.expr = F->FoldExpr(F, s->expr);
    return r;
}

static struct HirExpr *stencil_conversion_expr(struct HirFolder *F, struct HirConversionExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->conv.arg = F->FoldExpr(F, e->arg);
    r->conv.to = e->to;
    return r;
}

static struct HirExpr *stencil_call_expr(struct HirFolder *F, struct HirCallExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    if (HirIsFuncDef(e->func)) {
        struct HirDecl *old_func = pawHir_get_decl(F->hir, e->func->fdef.did);
        struct HirDecl *new_func = find_decl(F->state.S, old_func);
        r->call.func = HIR_TYPEOF(new_func);
    } else {
        r->call.func = subst_type(F, e->func);
    }
    r->call.target = F->FoldExpr(F, e->target);
    r->call.args = stencil_exprs(F, e->args);
    return r;
}

static struct HirPath *stencil_path(struct HirFolder *F, struct HirPath *e)
{
    struct HirStenciler *S = F->state.S;
    struct HirPath *r = pawHir_path_new(F->hir);
    for (int i = 0; i < e->count; ++i) {
        struct HirSegment *ps = pawHir_path_get(e, i);
        struct HirTypeList *args = MAYBE_FOLD_LIST(&S->fold, ps->types, List);
        struct HirType *type = subst_type(F, ps->type);
        pawHir_path_add(F->hir, &r, ps->name, args, type);
    }
    return r;
}

static struct HirExpr *stencil_path_expr(struct HirFolder *F, struct HirPathExpr *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->path.path = F->FoldPath(F, e->path);
    return r;
}

static struct HirDecl *stencil_instance_decl(struct HirFolder *F, struct HirInstanceDecl *d)
{
    struct HirDecl *r = stencil_prep_decl(F, d);
    r->inst.types = stencil_decls(F, d->types);
    if (d->fields != NULL) {
        r->inst.fields = stencil_decls(F, d->fields);
    }
    return r;
}

static struct HirDecl *stencil_func_decl(struct HirFolder *F, struct HirFuncDecl *d)
{
    struct HirDecl *r = stencil_prep_decl(F, d);
    r->func.is_global = d->is_global;
    r->func.name = d->name;

    r->func.generics = MAYBE_STENCIL_DECL_LIST(F, d->generics);
    r->func.params = stencil_decls(F, d->params);
    r->func.body = STENCIL_BLOCK(F, d->body);
    r->func.fn_kind = d->fn_kind;

    r->func.monos = MAYBE_STENCIL_DECL_LIST(F, d->monos);
    return r;
}

static struct HirStmt *stencil_if_stmt(struct HirFolder *F, struct HirIfStmt *s)
{
    struct HirStmt *r = stencil_prep_stmt(F, s);
    r->if_.cond = F->FoldExpr(F, s->cond);
    r->if_.then_arm = F->FoldStmt(F, s->then_arm);
    r->if_.else_arm = F->FoldStmt(F, s->else_arm);
    return r;
}

static struct HirStmt *stencil_while_stmt(struct HirFolder *F, struct HirWhileStmt *s)
{
    struct HirStmt *r = stencil_prep_stmt(F, s);
    r->while_.is_dowhile = s->is_dowhile;
    r->while_.cond = F->FoldExpr(F, s->cond);
    r->while_.block = STENCIL_BLOCK(F, s->block);
    return r;
}

static struct HirStmt *stencil_label_stmt(struct HirFolder *F, struct HirLabelStmt *s)
{
    struct HirStmt *r = stencil_prep_stmt(F, s);
    r->label.label = s->label;
    return r;
}

static struct HirBlock *stencil_forbody(struct HirFolder *F, struct HirForStmt *src, struct HirForStmt *dst)
{
    dst->control = F->FoldDecl(F, src->control);
    struct HirStmt *r = stencil_prep_stmt(F, src->block);
    r->block.stmts = stencil_stmts(F, src->block->stmts);
    return &r->block;
}

static struct HirStmt *stencil_for_stmt(struct HirFolder *F, struct HirForStmt *s)
{
    struct HirStmt *r = stencil_prep_stmt(F, s);
    r->for_.is_fornum = s->is_fornum;
    if (s->is_fornum) {
        r->for_.fornum.begin = F->FoldExpr(F, s->fornum.begin);
        r->for_.fornum.end = F->FoldExpr(F, s->fornum.end);
        r->for_.fornum.step = F->FoldExpr(F, s->fornum.step);
    } else {
        r->for_.forin.target = F->FoldExpr(F, s->forin.target);
    }
    r->for_.block = stencil_forbody(F, s, &r->for_);
    return r;
}

static struct HirExpr *stencil_index_expr(struct HirFolder *F, struct HirIndex *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->index.target = F->FoldExpr(F, e->target);
    r->index.first = F->FoldExpr(F, e->first);
    r->index.second = F->FoldExpr(F, e->second);
    r->index.is_slice = e->is_slice;
    return r;
}

static struct HirExpr *stencil_select_expr(struct HirFolder *F, struct HirSelector *e)
{
    struct HirExpr *r = stencil_prep_expr(F, e);
    r->select.is_index = e->is_index;
    r->select.target = F->FoldExpr(F, e->target);
    r->select.name = e->name;
    return r;
}

static struct HirStmt *stencil_decl_stmt(struct HirFolder *F, struct HirDeclStmt *s)
{
    struct HirStmt *r = stencil_prep_stmt(F, s);
    r->decl.decl = F->FoldDecl(F, s->decl);
    return r;
}

static void setup_stencil_pass(struct HirFolder *F, HirStenciler *S)
{
    const union HirState state = {.S = S};
    pawHir_fold_init(F, S->hir, state);
    F->FoldPath = stencil_path;
    F->FoldLiteralExpr = stencil_literal_expr;
    F->FoldLogicalExpr = stencil_logical_expr;
    F->FoldPathExpr = stencil_path_expr;
    F->FoldChainExpr = stencil_chain_expr;
    F->FoldUnOpExpr = stencil_unop_expr;
    F->FoldBinOpExpr = stencil_binop_expr;
    F->FoldConversionExpr = stencil_conversion_expr;
    F->FoldCallExpr = stencil_call_expr;
    F->FoldIndex = stencil_index_expr;
    F->FoldSelector = stencil_select_expr;
    F->FoldMapItem = stencil_map_item;
    F->FoldStructItem = stencil_struct_item;
    F->FoldClosureExpr = stencil_closure_expr;
    F->FoldBlock = stencil_block_stmt;
    F->FoldExprStmt = stencil_expr_stmt;
    F->FoldDeclStmt = stencil_decl_stmt;
    F->FoldIfStmt = stencil_if_stmt;
    F->FoldForStmt = stencil_for_stmt;
    F->FoldWhileStmt = stencil_while_stmt;
    F->FoldLabelStmt = stencil_label_stmt;
    F->FoldReturnStmt = stencil_return_stmt;
    F->FoldVariantDecl = stencil_variant_decl;
    F->FoldVarDecl = stencil_var_decl;
    F->FoldFuncDecl = stencil_func_decl;
    F->FoldAdtDecl = stencil_adt_decl;
    F->FoldFieldDecl = stencil_field_decl;
    F->FoldGenericDecl = stencil_generic_decl;
    F->FoldInstanceDecl = stencil_instance_decl;
    F->FoldTypeDecl = stencil_type_decl;
}

static struct HirTypeList *prep_typelist(struct HirTypeFolder *F, struct HirTypeList *list)
{
    HirStenciler *S = F->state;
    struct HirTypeList *copy = pawHir_type_list_new(S->hir);
    for (int i = 0; i < list->count; ++i) {
        pawHir_type_list_push(S->hir, &copy, list->data[i]);
    }
    F->FoldList(F, copy);
    return copy;
}

#define MAYBE_PREP_TYPELIST(F, list) ((list) != NULL ? prep_typelist(F, list) : NULL)

static struct HirType *prep_tuple(struct HirTypeFolder *F, struct HirTupleType *t)
{
    Subst *subst = F->state;
    HirStenciler *S = subst->S;

    struct HirType *r = pawHir_new_type(S->hir, kHirTupleType);
    r->tuple.elems = prep_typelist(F, t->elems);
    return r;
}

static struct HirType *prep_fptr(struct HirTypeFolder *F, struct HirFuncPtr *t)
{
    Subst *subst = F->state;
    HirStenciler *S = subst->S;

    struct HirType *r = pawHir_new_type(S->hir, kHirFuncPtr);
    r->fptr.params = prep_typelist(F, t->params);
    r->fptr.result = F->Fold(F, t->result);
    return r;
}

static struct HirType *prep_fdef(struct HirTypeFolder *F, struct HirFuncDef *t)
{
    Subst *subst = F->state;
    HirStenciler *S = subst->S;

    struct HirType *r = pawHir_new_type(S->hir, kHirFuncDef);
    struct HirDecl *old_base = pawHir_get_decl(S->hir, t->base);
    struct HirDecl *new_base = find_decl(S, old_base);
    struct HirDecl *old_decl = pawHir_get_decl(S->hir, t->did);
    struct HirDecl *new_decl = find_decl(S, old_decl);
    r->fdef.did = new_decl->hdr.did;
    r->fdef.base = new_base->hdr.did;

    r->fdef.types = MAYBE_PREP_TYPELIST(F, t->types);
    r->fdef.params = prep_typelist(F, t->params);
    r->fdef.result = F->Fold(F, t->result);
    return r;
}

static struct HirType *prep_adt(struct HirTypeFolder *F, struct HirAdt *t)
{
    Subst *subst = F->state;
    HirStenciler *S = subst->S;
    if (t->did <= PAW_TSTRING) {
        return HIR_CAST_TYPE(t);
    }

    struct HirDecl *old_base = pawHir_get_decl(S->hir, t->base);
    struct HirDecl *new_base = find_decl(S, old_base);

    struct HirType *r = pawHir_new_type(S->hir, kHirAdt);
    r->adt.did = new_base->hdr.did;
    r->adt.base = new_base->hdr.did;
    r->adt.types = MAYBE_PREP_TYPELIST(F, t->types);
    return r;
}

static struct HirType *maybe_subst(struct HirTypeFolder *F, struct HirType *t)
{
    Subst *s = &cast(F->state, HirStenciler *)->subst;
    for (int i = 0; i < s->before->count; ++i) {
        if (t == s->before->data[i]) {
            return s->after->data[i];
        }
    }
    return t;
}

static struct HirType *prep_generic(struct HirTypeFolder *F, struct HirGeneric *t)
{
    return maybe_subst(F, HIR_CAST_TYPE(t));
}

static struct HirType *subst_type(struct HirFolder *F, struct HirType *type)
{
    HirStenciler *S = F->state.S;
    return S->fold.Fold(&S->fold, type);
}

static void add_existing_link(paw_Env *P, Map *map, struct HirDecl *key, struct HirDecl *value)
{
    const Value k = {.p = key};
    const Value v = {.p = value};
    pawH_insert(P, map, k, v);
}

static struct HirFuncDecl *do_stencil_func(struct Hir *hir, struct HirFuncDecl *base, struct HirDecl *inst)
{
    paw_Env *P = ENV(hir);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    v_set_object(pv, map);

    HirStenciler S = {
        .decls = map,
        .P = hir->P,
        .hir = hir,
    };
    struct HirFolder F;
    setup_stencil_pass(&F, &S);

    S.subst.before = base->type->fdef.types;
    S.subst.after = inst->inst.type->fdef.types;
    S.subst.S = &S;

    pawHir_type_folder_init(&S.fold, &S);
    S.fold.FoldAdt = prep_adt;
    S.fold.FoldTupleType = prep_tuple;
    S.fold.FoldFuncPtr = prep_fptr;
    S.fold.FoldFuncDef = prep_fdef;
    S.fold.FoldGeneric = prep_generic;

    inst->func.kind = kHirFuncDecl;
    inst->func.name = base->name;
    inst->func.generics = NULL;

    inst->func.params = stencil_decls(&F, base->params);
    inst->func.body = STENCIL_BLOCK(&F, base->body);
    inst->func.fn_kind = base->fn_kind;

    pawC_pop(P); // pop decl. map
    return &inst->func;
}

static void stencil_func(struct HirVisitor *V, struct HirFuncDecl *d)
{
    if (d->generics == NULL) {
        V->VisitBlock(V, d->body);
    }
    if (d->monos == NULL) {
        return;
    }
    for (int i = 0; i < d->monos->count; ++i) {
        struct HirDecl *decl = d->monos->data[i];
        do_stencil_func(V->hir, d, decl);
        V->VisitFuncDecl(V, &decl->func);
    }
}

void pawHir_stencil_stmts(struct Hir *hir, struct HirStmtList *stmts)
{
    struct HirVisitor V;
    pawHir_visitor_init(&V, hir, (union HirState){0});
    V.VisitFuncDecl = stencil_func;
    V.VisitStmtList(&V, stmts);
}

struct Printer {
    struct DynamicMem *dm;
    int indent;
};

static void indent_line(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        printf("    ");
    }
}

#define dump_fmt(P, fmt, ...) (indent_line(P), printf(fmt, __VA_ARGS__))
#define dump_msg(P, msg) (indent_line(P), printf(msg))

static void dump_type_aux(struct Printer *P, struct HirType *type);

static void dump_typelist(struct Printer *P, struct HirTypeList *list)
{
    for (int i = 0; i < list->count; ++i) {
        dump_type_aux(P, list->data[i]);
        if (i < list->count - 1) {
            printf(", ");
        }
    }
}

static const char *get_typename(struct DynamicMem *dm, DefId did)
{
    const struct HirDecl *decl = dm->decls.data[did];
    return decl->hdr.name ? decl->hdr.name->text : "(null)";
}

static void dump_type_aux(struct Printer *P, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirTupleType:
            printf("(");
            for (int i = 0; i < type->tuple.elems->count; ++i) {
                dump_type_aux(P, type->tuple.elems->data[i]);
                if (i < type->tuple.elems->count - 1) {
                    printf(", ");
                }
            }
            printf(")");
            break;
        case kHirFuncPtr:
        case kHirFuncDef:
            printf("fn");
            if (HirIsFuncDef(type)) {
                printf(" %s", get_typename(P->dm, type->fdef.did));
            }
            printf("(");
            for (int i = 0; i < type->fptr.params->count; ++i) {
                dump_type_aux(P, type->fptr.params->data[i]);
                if (i < type->fptr.params->count - 1) {
                    printf(", ");
                }
            }
            printf(") -> ");
            dump_type_aux(P, type->fptr.result);
            break;
        case kHirAdt: 
            printf("%s", get_typename(P->dm, type->adt.did));
            if (type->adt.types != NULL) {
                printf("<");
                const struct HirTypeList *list = type->adt.types;
                for (int i = 0; i < list->count; ++i) {
                    dump_type_aux(P, list->data[i]);
                    if (i < list->count - 1) {
                        printf(", ");
                    }
                }
                printf(">");
            }
            break;
        case kHirUnknown:
            printf("?%d", type->unknown.index);
            break;
        default:
            paw_assert(HirIsGeneric(type));
            printf("?%s", type->generic.name->text);
    }
}

static void dump_type(struct Printer *P, struct HirType *type)
{
    indent_line(P);
    printf("type: ");
    if (type != NULL) {
        dump_type_aux(P, type);
    } else {
        printf("(null)");
    }
    printf("\n");
}

static void dump_stmt(struct Printer *, struct HirStmt *);
static void dump_expr(struct Printer *, struct HirExpr *);

static void print_decl_kind(struct Printer *P, void *node)
{
    struct HirDecl *d = node;
    switch (HIR_KINDOF(d)) {
        case kHirFuncDecl:
            printf("FuncDecl");
            break;
        case kHirFieldDecl:
            printf("FieldDecl");
            break;
        case kHirVarDecl:
            printf("VarDecl");
            break;
        case kHirVariantDecl:
            printf("VariantDecl");
            break;
        case kHirAdtDecl:
            printf("AdtDecl");
            break;
        case kHirGenericDecl:
            printf("GenericDecl");
            break;
        case kHirTypeDecl:
            printf("TypeDecl");
            break;
        case kHirInstanceDecl:
            printf("InstanceDecl");
            break;
        default:
            printf("?");
    }
}

static void print_expr_kind(struct Printer *P, void *node)
{
    struct HirExpr *e = node;
    switch (HIR_KINDOF(e)) {
        case kHirLiteralExpr:
            printf("LiteralExpr");
            break;
        case kHirUnOpExpr:
            printf("UnOpExpr");
            break;
        case kHirBinOpExpr:
            printf("BinOpExpr");
            break;
        case kHirCallExpr:
            printf("CallExpr");
            break;
        case kHirIndex:
            printf("Index");
            break;
        case kHirSelector:
            printf("Selector");
            break;
        default:
            printf("?");
            break;
    }
}

static void print_stmt_kind(struct Printer *P, void *node)
{
    struct HirStmt *s = node;
    switch (HIR_KINDOF(s)) {
        case kHirExprStmt:
            printf("ExprStmt");
            break;
        case kHirDeclStmt:
            printf("DeclStmt");
            break;
        case kHirBlock:
            printf("Block");
            break;
        case kHirIfStmt:
            printf("IfStmt");
            break;
        case kHirForStmt:
            printf("ForStmt");
            break;
        case kHirWhileStmt:
            printf("WhileStmt");
            break;
        case kHirReturnStmt:
            printf("ReturnStmt");
            break;
        default:
            printf("?");
    }
}

static int predump_node(struct Printer *P, void *node,
                        void (*print)(struct Printer *, void *))
{
    if (node != NULL) {
        print(P, node);
        printf(" {\n");
        return 0;
    }
    return -1;
}

#define dump_block(P, b) \
    check_exp((b)->kind == kHirBlock, dump_stmt(P, HIR_CAST_STMT(b)))
#define dump_name(P, s) dump_fmt(P, "name: %s\n", s ? s->text : NULL)

static void dump_expr(struct Printer *P, struct HirExpr *e);
static void dump_decl(struct Printer *P, struct HirDecl *d);
static void dump_stmt(struct Printer *P, struct HirStmt *s);

#define DEFINE_LIST_PRINTER(name, T)                                                          \
    static void dump_##name##_list(struct Printer *P, struct T##List *list, const char *name) \
    {                                                                                         \
        dump_fmt(P, "%s: {\n", name);                                                         \
        ++P->indent;                                                                          \
        if (list != NULL) {                                                                   \
            dump_msg(P, "" /* indent */);                                                     \
            for (int i = 0; i < list->count; ++i) {                                           \
                dump_##name(P, list->data[i]);                                                \
            }                                                                                 \
        }                                                                                     \
        --P->indent;                                                                          \
        dump_msg(P, "}\n");                                                                   \
    }
DEFINE_LIST_PRINTER(expr, HirExpr) 
DEFINE_LIST_PRINTER(decl, HirDecl)
DEFINE_LIST_PRINTER(stmt, HirStmt)
DEFINE_LIST_PRINTER(type, HirType)

static void dump_path(struct Printer *P, struct HirPath *p)
{
    for (int i = 0; i < p->count; ++i) {
        struct HirSegment *seg = p->data[i];
        dump_name(P, seg->name);
        dump_type_list(P, seg->types, "types");
    }
}

static void dump_decl(struct Printer *P, struct HirDecl *d)
{
    if (predump_node(P, d, print_decl_kind)) {
        printf("(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", d->hdr.line);
    dump_type(P, HIR_TYPEOF(d));
    switch (HIR_KINDOF(d)) {
        case kHirFuncDecl:
            dump_fmt(P, "is_global: %d\n", d->func.is_global);
            dump_fmt(P, "receiver: %p\n", (void *)d->func.receiver);
            dump_fmt(P, "name: %s\n", d->func.name->text);
            dump_decl_list(P, d->func.generics, "generics");
            dump_decl_list(P, d->func.params, "params");
            dump_msg(P, "body: ");
            dump_block(P, d->func.body);
            dump_decl_list(P, d->func.monos, "monos");
            break;
        case kHirFieldDecl:
            dump_name(P, d->field.name);
            break;
        case kHirVarDecl:
            dump_fmt(P, "is_global: %d\n", d->var.is_global);
            dump_name(P, d->var.name);
            dump_msg(P, "init: ");
            dump_expr(P, d->var.init);
            break;
        case kHirVariantDecl:
            dump_name(P, d->variant.name);
            dump_decl_list(P, d->variant.fields, "fields");
            break;
        case kHirAdtDecl:
            dump_name(P, d->adt.name);
            dump_fmt(P, "is_struct: %d\n", d->adt.is_struct);
            dump_decl_list(P, d->adt.generics, "generics");
            dump_decl_list(P, d->adt.fields, "fields");
            dump_decl_list(P, d->func.monos, "monos");
            break;
        case kHirGenericDecl:
            dump_name(P, d->generic.name);
            break;
        case kHirTypeDecl:
            dump_name(P, d->type.name);
            dump_msg(P, "rhs: ");
            dump_expr(P, d->type.rhs);
            dump_decl_list(P, d->type.generics, "generics");
            break;
        case kHirInstanceDecl:
            dump_name(P, d->type.name);
            dump_decl_list(P, d->inst.fields, "fields");
            break;
        default:
            paw_assert(0);
    }
    --P->indent;
    dump_msg(P, "}\n");
}

static void dump_stmt(struct Printer *P, struct HirStmt *s)
{
    if (predump_node(P, s, print_stmt_kind)) {
        printf("(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", s->hdr.line);
    switch (HIR_KINDOF(s)) {
        case kHirExprStmt:
            dump_msg(P, "lhs: ");
            dump_expr(P, s->expr.lhs);
            dump_msg(P, "rhs: ");
            dump_expr(P, s->expr.rhs);
            break;
        case kHirBlock:
            dump_stmt_list(P, s->block.stmts, "stmts");
            break;
        case kHirDeclStmt:
            dump_msg(P, "decl: ");
            dump_decl(P, s->decl.decl);
            break;
        case kHirIfStmt:
            dump_msg(P, "cond: ");
            dump_expr(P, s->if_.cond);
            dump_msg(P, "then_arm: ");
            dump_stmt(P, s->if_.then_arm);
            dump_msg(P, "else_arm: ");
            dump_stmt(P, s->if_.else_arm);
            break;
        case kHirForStmt:
            if (s->for_.is_fornum) {
                dump_msg(P, "begin: ");
                dump_expr(P, s->for_.fornum.begin);
                dump_msg(P, "end: ");
                dump_expr(P, s->for_.fornum.end);
                dump_msg(P, "step: ");
                dump_expr(P, s->for_.fornum.step);
                dump_msg(P, "block: ");
                dump_block(P, s->for_.block);
            } else {
                dump_msg(P, "target: ");
                dump_expr(P, s->for_.forin.target);
                dump_msg(P, "block: ");
                dump_block(P, s->for_.block);
            }
            break;
        case kHirWhileStmt:
            dump_fmt(P, "is_dowhile: %d\n", s->while_.is_dowhile);
            dump_msg(P, "cond: ");
            dump_expr(P, s->while_.cond);
            dump_msg(P, "block: ");
            dump_block(P, s->while_.block);
            break;
        case kHirReturnStmt:
            dump_msg(P, "expr: ");
            dump_expr(P, s->result.expr);
            break;
        default:
            break;
    }
    --P->indent;
    dump_msg(P, "}\n");
}

static void dump_expr(struct Printer *P, struct HirExpr *e)
{
    if (predump_node(P, e, print_expr_kind)) {
        printf("(null)\n");
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", e->hdr.line);
    dump_type(P, HIR_TYPEOF(e));
    switch (HIR_KINDOF(e)) {
        case kHirLiteralExpr:
            switch (e->literal.lit_kind) {
                case kHirLitBasic:
                    dump_msg(P, "lit_kind: BASIC\n");
                    switch (e->literal.basic.t) {
                        case PAW_TUNIT:
                            dump_msg(P, "type: ()\n");
                            break;
                        case PAW_TBOOL:
                            dump_msg(P, "type: bool\n");
                            dump_fmt(P, "value: %s\n",
                                     v_true(e->literal.basic.value) ? "true"
                                                                    : "false");
                            break;
                        case PAW_TINT:
                            dump_msg(P, "type: int\n");
                            dump_fmt(P, "value: %" PRId64 "\n",
                                     v_int(e->literal.basic.value));
                            break;
                        case PAW_TFLOAT:
                            dump_msg(P, "type: float\n");
                            dump_fmt(P, "value: %f\n",
                                     v_float(e->literal.basic.value));
                            break;
                        default:
                            paw_assert(e->literal.basic.t == PAW_TSTRING);
                            dump_msg(P, "type: string\n");
                            dump_fmt(P, "value: %s\n",
                                     v_string(e->literal.basic.value)->text);
                            break;
                    }
                    break;
                case kHirLitTuple:
                    dump_msg(P, "lit_kind: TUPLE\n");
                    dump_expr_list(P, e->literal.tuple.elems, "elems");
                    break;
                case kHirLitContainer:
                    dump_msg(P, "lit_kind: CONTAINER\n");
                    dump_expr_list(P, e->literal.cont.items, "items");
                    break;
                default:
                    paw_assert(e->literal.lit_kind == kHirLitComposite);
                    dump_msg(P, "lit_kind: COMPOSITE\n");
                    dump_msg(P, "target: ");
                    dump_path(P, e->literal.comp.path);
                    dump_expr_list(P, e->literal.comp.items, "items");
            }
            break;
        case kHirChainExpr:
            dump_msg(P, "target: ");
            dump_expr(P, e->chain.target);
            break;
        case kHirLogicalExpr:
            dump_fmt(P, "is_and: %d\n", e->logical.is_and);
            dump_msg(P, "lhs: ");
            dump_expr(P, e->logical.lhs);
            dump_msg(P, "rhs: ");
            dump_expr(P, e->logical.rhs);
            break;
        case kHirClosureExpr:
            dump_decl_list(P, e->clos.params, "params");
            dump_block(P, e->clos.body);
            break;
        case kHirPathExpr:
            dump_msg(P, "path: ");
            dump_path(P, e->path.path);
            break;
        case kHirConversionExpr:
            dump_fmt(P, "to: %d\n", e->conv.to);
            dump_msg(P, "arg: ");
            dump_expr(P, e->conv.arg);
            break;
        case kHirUnOpExpr:
            dump_fmt(P, "op: %d\n", e->unop.op);
            dump_msg(P, "target: ");
            dump_expr(P, e->unop.target);
            break;
        case kHirBinOpExpr:
            dump_fmt(P, "op: %d\n", e->binop.op);
            dump_msg(P, "lhs: ");
            dump_expr(P, e->binop.lhs);
            dump_msg(P, "rhs: ");
            dump_expr(P, e->binop.rhs);
            break;
        case kHirCallExpr:
            dump_msg(P, "target: ");
            dump_expr(P, e->call.target);
            dump_expr_list(P, e->call.args, "args");
            break;
        case kHirIndex:
            dump_fmt(P, "is_slice: %d\n", e->index.is_slice);
            dump_msg(P, "target: ");
            dump_expr(P, e->index.target);
            dump_msg(P, "first: ");
            dump_expr(P, e->index.first);
            dump_msg(P, "second: ");
            dump_expr(P, e->index.second);
            break;
        case kHirSelector:
            dump_msg(P, "target: ");
            dump_expr(P, e->select.target);
            if (e->select.is_index) {
                dump_fmt(P, "index: %" PRId64 "\n", e->select.index);
            } else {
                dump_name(P, e->select.name);
            }
            break;
        case kHirMapItem:
            dump_msg(P, "key: ");
            dump_expr(P, e->mitem.key);
            dump_msg(P, "value: ");
            dump_expr(P, e->mitem.value);
            break;
        case kHirStructItem:
            dump_name(P, e->sitem.name);
            dump_msg(P, "value: ");
            dump_expr(P, e->sitem.value);
            break;
        default:
            paw_assert(0);
    }
    --P->indent;
    dump_msg(P, "}\n");
}

void pawHir_repr_path(struct Hir *hir, struct HirPath *path)
{
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment *s = pawHir_path_get(path, i);
        printf("%s", s->name->text);
        if (s->types != NULL) {
            printf("<");
            for (int j = 0; j < s->types->count; ++j) {
                printf("T");
                if (j < s->types->count - 1) {
                    printf(", ");
                }
            }
            printf(">");
        }
    }
    printf("\n");
}

// TODO: Have this output a String, or fill a Buffer
void pawHir_repr_type(struct Hir *hir, struct HirType *type)
{
    struct Printer P = {.dm = hir->dm};
    dump_type(&P, type);
}

void pawHir_dump_type(struct Hir *hir, struct HirType *type)
{
    struct Printer P = {.dm = hir->dm};
    dump_type(&P, type);
}

void pawHir_dump_decl(struct Hir *hir, struct HirDecl *decl)
{
    struct Printer P = {.dm = hir->dm};
    dump_decl(&P, decl);
}

void pawHir_dump_expr(struct Hir *hir, struct HirExpr *expr)
{
    struct Printer P = {.dm = hir->dm};
    dump_expr(&P, expr);
}

void pawHir_dump_stmt(struct Hir *hir, struct HirStmt *stmt)
{
    struct Printer P = {.dm = hir->dm};
    dump_stmt(&P, stmt);
}

void pawHir_dump_path(struct Hir *hir, struct HirPath *path)
{
    struct Printer P = {.dm = hir->dm};
    dump_path(&P, path);
}

