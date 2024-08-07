// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "hir.h"
#include "compile.h"
#include "map.h"
#include "mem.h"
#include "resolve.h"
#include <inttypes.h>
#include <limits.h>
#include <stdlib.h>

#define LIST_MIN 8
#define FIRST_ARENA_SIZE 512
#define LARGE_ARENA_MIN 32

static void add_builtin_type(struct Hir *hir, enum HirTypeKind kind, paw_Type code)
{
    hir->builtin[code] = pawHir_new_type(hir, 0, kind);
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

#define NEW_NODE(hir, T) pawK_pool_alloc(ENV(hir), &(hir)->pool, sizeof(struct T))

#define DEFINE_NODE_CONSTRUCTOR(name, T) \
        struct T *pawHir_new_##name(struct Hir *hir, int line, enum T##Kind kind) \
        { \
            struct T *r = NEW_NODE(hir, T); \
            r->hdr.line = line; \
            r->hdr.kind = kind; \
            return r; \
        }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)

#define LIST_MIN_ALLOC 8

struct HirType *pawHir_new_type(struct Hir *hir, int line, enum HirTypeKind kind)
{
    struct HirType *r = NEW_NODE(hir, HirType);
    r->hdr.kind = kind;
    r->hdr.line = line;
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
    pawHir_scope_list_push(hir, table->scopes, scope);
}

struct HirSymbol *pawHir_add_symbol(struct Hir *hir, struct HirScope *table)
{
    struct HirSymbol *symbol = pawHir_new_symbol(hir);
    pawHir_symbol_list_push(hir, table->symbols, symbol);
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

#define DEFINE_LIST_VISITOR(name, T) \
    static void visit_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            V->Visit##T(V, list->data[i]); \
        } \
    }
DEFINE_LIST_VISITOR(decl, Decl) 
DEFINE_LIST_VISITOR(expr, Expr)
DEFINE_LIST_VISITOR(stmt, Stmt)
DEFINE_LIST_VISITOR(type, Type)

static void VisitBlock(struct HirVisitor *V, struct HirBlock *s)
{
    V->VisitStmtList(V, s->stmts);
}

static void VisitLogicalExpr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    V->VisitExpr(V, e->lhs);
    V->VisitExpr(V, e->rhs);
}

static void VisitMapElem(struct HirVisitor *V, struct HirMapElem *e)
{
    V->VisitExpr(V, e->key);
    V->VisitExpr(V, e->value);
}

static void VisitStructField(struct HirVisitor *V, struct HirStructField *e)
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
    if (e->has_body) {
        V->VisitBlock(V, e->body);
    } else {
        V->VisitExpr(V, e->expr);
    }
}

static void VisitFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    paw_unused(V);
    paw_unused(d);
}

static void VisitTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    V->VisitDeclList(V, d->generics);
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
    V->VisitDeclList(V, d->generics);
    V->VisitDeclList(V, d->fields);
}

static void VisitInstanceDecl(struct HirVisitor *V, struct HirInstanceDecl *d)
{
    V->VisitTypeList(V, d->types);
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
    V->VisitDeclList(V, d->generics);
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

static void VisitFuncDef(struct HirVisitor *V, struct HirFuncDef *t)
{
    V->VisitTypeList(V, t->types);
    V->VisitTypeList(V, t->params);
    V->VisitType(V, t->result);
}

static void VisitFuncPtr(struct HirVisitor *V, struct HirFuncPtr *t)
{
    V->VisitTypeList(V, t->params);
    V->VisitType(V, t->result);
}

static void VisitTupleType(struct HirVisitor *V, struct HirTupleType *t)
{
    V->VisitTypeList(V, t->elems);
}

static void VisitAdt(struct HirVisitor *V, struct HirAdt *t)
{
    V->VisitTypeList(V, t->types);
}

static void VisitUnknown(struct HirVisitor *V, struct HirUnknown *t)
{
    paw_unused(V);
    paw_unused(t);
}

static void VisitGeneric(struct HirVisitor *V, struct HirGeneric *t)
{
    paw_unused(V);
    paw_unused(t);
}

static void VisitExpr(struct HirVisitor *V, struct HirExpr *node)
{
    if (node == NULL) {
        return;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b); \
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
        V->Visit##a(V, &node->b); \
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
        V->Visit##a(V, &node->b); \
        break;
        HIR_STMT_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

static void VisitType(struct HirVisitor *V, struct HirType *node)
{
    if (node == NULL) {
        return;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_VISIT(a, b) case kHir##a: \
        V->Visit##a(V, &node->b); \
        break;
        HIR_TYPE_LIST(DEFINE_VISIT)
#undef DEFINE_VISIT
    }
}

void pawHir_visitor_init(struct HirVisitor *V, struct Hir *hir, void *ud)
{
    *V = (struct HirVisitor){
        .hir = hir,
        .ud = ud,

        .VisitExpr = VisitExpr,
        .VisitPath = VisitPath,
        .VisitDecl = VisitDecl,
        .VisitStmt = VisitStmt,
        .VisitType = VisitType,

        .VisitExprList = visit_expr_list,
        .VisitDeclList = visit_decl_list,
        .VisitStmtList = visit_stmt_list,
        .VisitTypeList = visit_type_list,

#define DEFINE_CALLBACKS(a, b) .Visit##a = Visit##a,
        HIR_EXPR_LIST(DEFINE_CALLBACKS)
        HIR_DECL_LIST(DEFINE_CALLBACKS)
        HIR_STMT_LIST(DEFINE_CALLBACKS)
        HIR_TYPE_LIST(DEFINE_CALLBACKS)
#undef DEFINE_CALLBACKS
    };
}

void pawHir_visit(struct HirVisitor *V)
{
    struct Hir *hir = V->hir;
    V->VisitDeclList(V, hir->items);
}

// Generate code for folding a list of HIR nodes
#define DEFINE_LIST_FOLDER(name, T) \
    static struct Hir##T##List *fold_##name##_list(struct HirFolder *F, struct Hir##T##List *list) \
    { \
        if (list == NULL) return NULL; \
        for (int i = 0; i < list->count; ++i) { \
            list->data[i] = F->Fold##T(F, list->data[i]); \
        } \
        return list; \
    } 
DEFINE_LIST_FOLDER(decl, Decl) 
DEFINE_LIST_FOLDER(expr, Expr)
DEFINE_LIST_FOLDER(stmt, Stmt)
DEFINE_LIST_FOLDER(type, Type)

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

static struct HirExpr *FoldMapElem(struct HirFolder *F, struct HirMapElem *e)
{
    e->key = F->FoldExpr(F, e->key);
    e->value = F->FoldExpr(F, e->value);
    return HIR_CAST_EXPR(e);
}

static struct HirExpr *FoldStructField(struct HirFolder *F, struct HirStructField *e)
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
    if (e->has_body) {
        e->body = FOLD_BLOCK(F, e->body);
    } else {
        e->expr = F->FoldExpr(F, e->expr);
    }
    return HIR_CAST_EXPR(e);
}

static struct HirDecl *FoldFieldDecl(struct HirFolder *F, struct HirFieldDecl *d)
{
    paw_unused(F);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldTypeDecl(struct HirFolder *F, struct HirTypeDecl *d)
{
    d->generics = F->FoldDeclList(F, d->generics);
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
    d->generics = F->FoldDeclList(F, d->generics);
    d->fields = F->FoldDeclList(F, d->fields);
    return HIR_CAST_DECL(d);
}

static struct HirDecl *FoldInstanceDecl(struct HirFolder *F, struct HirInstanceDecl *d)
{
    d->types = F->FoldTypeList(F, d->types);
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
    d->generics = F->FoldDeclList(F, d->generics);
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

static struct HirType *FoldFuncDef(struct HirFolder *F, struct HirFuncDef *t)
{
    t->types = F->FoldTypeList(F, t->types);
    t->params = F->FoldTypeList(F, t->params);
    t->result = F->FoldType(F, t->result);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldFuncPtr(struct HirFolder *F, struct HirFuncPtr *t)
{
    t->params = F->FoldTypeList(F, t->params);
    t->result = F->FoldType(F, t->result);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldTupleType(struct HirFolder *F, struct HirTupleType *t)
{
    t->elems = F->FoldTypeList(F, t->elems);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldAdt(struct HirFolder *F, struct HirAdt *t)
{
    t->types = F->FoldTypeList(F, t->types);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldUnknown(struct HirFolder *F, struct HirUnknown *t)
{
    paw_unused(F);
    return HIR_CAST_TYPE(t);
}

static struct HirType *FoldGeneric(struct HirFolder *F, struct HirGeneric *t)
{
    paw_unused(F);
    return HIR_CAST_TYPE(t);
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

static struct HirType *FoldType(struct HirFolder *F, struct HirType *node)
{
    if (node == NULL) {
        return NULL;
    }
    switch (HIR_KINDOF(node)) {
#define DEFINE_FOLD(a, b) case kHir##a: \
        return F->Fold##a(F, &node->b);
        HIR_TYPE_LIST(DEFINE_FOLD)
#undef DEFINE_FOLD
    }
}

void pawHir_folder_init(struct HirFolder *F, struct Hir *hir, void *ud)
{
    *F = (struct HirFolder){
        .hir = hir,
        .ud = ud,

        .FoldExpr = FoldExpr,
        .FoldPath = FoldPath,
        .FoldDecl = FoldDecl,
        .FoldStmt = FoldStmt,
        .FoldType = FoldType,

        .FoldExprList = fold_expr_list,
        .FoldDeclList = fold_decl_list,
        .FoldStmtList = fold_stmt_list,
        .FoldTypeList = fold_type_list,

#define DEFINE_CALLBACKS(a, b) .Fold##a = Fold##a,
        HIR_EXPR_LIST(DEFINE_CALLBACKS)
        HIR_DECL_LIST(DEFINE_CALLBACKS)
        HIR_STMT_LIST(DEFINE_CALLBACKS)
        HIR_TYPE_LIST(DEFINE_CALLBACKS)
#undef DEFINE_CALLBACKS
    };
}

void pawHir_fold(struct HirFolder *F)
{
    struct Hir *hir = F->hir;
    F->FoldDeclList(F, hir->items);
}

// ****************************
//     HIR copying routines
// ****************************

struct Copier {
    struct Hir *hir;
    paw_Env *P;
};

#define DEFINE_COPY_PREP(name, T) \
    static T *copy_prep_##name##_aux(struct HirFolder *F, T *t) \
    { \
        T *r = pawHir_new_##name(F->hir, t->hdr.line, HIR_KINDOF(t)); \
        r->hdr = t->hdr; \
        return r; \
    }
DEFINE_COPY_PREP(expr, struct HirExpr) 
DEFINE_COPY_PREP(decl, struct HirDecl)
DEFINE_COPY_PREP(stmt, struct HirStmt)

// Helpers for copying: create a new node of the given type and kind,
// and copy the common fields
#define copy_prep_expr(F, e) copy_prep_expr_aux(F, HIR_CAST_EXPR(e))
#define copy_prep_decl(F, d) copy_prep_decl_aux(F, HIR_CAST_DECL(d))
#define copy_prep_stmt(F, s) copy_prep_stmt_aux(F, HIR_CAST_STMT(s))

#define DEFINE_COPY_LIST(name, T) \
    static struct Hir##T##List *copy_##name##_list(struct HirFolder *F, struct Hir##T##List *old_list) \
    { \
        if (old_list == NULL) return NULL; \
        struct Hir##T##List *new_list = pawHir_##name##_list_new(F->hir); \
        for (int i = 0; i < old_list->count; ++i) { \
            struct Hir##T *elem = F->Fold##T(F, old_list->data[i]); \
            pawHir_##name##_list_push(F->hir, new_list, elem); \
        } \
        return new_list; \
    }
DEFINE_COPY_LIST(decl, Decl) 
DEFINE_COPY_LIST(expr, Expr)
DEFINE_COPY_LIST(stmt, Stmt)
DEFINE_COPY_LIST(type, Type)

static struct HirStmt *copy_block_stmt(struct HirFolder *F, struct HirBlock *s)
{
    struct HirStmt *r = copy_prep_stmt(F, s);
    r->block.stmts = F->FoldStmtList(F, s->stmts);
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

static struct HirExpr *copy_map_elem(struct HirFolder *F, struct HirMapElem *e)
{
    struct HirExpr *r = copy_prep_expr(F, e);
    r->mitem.key = F->FoldExpr(F, e->key);
    r->mitem.value = F->FoldExpr(F, e->value);
    return r;
}

static struct HirExpr *copy_struct_field(struct HirFolder *F, struct HirStructField *e)
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
            r->literal.tuple.elems = F->FoldExprList(F, e->tuple.elems);
            break;
        case kHirLitContainer:
            r->literal.cont.code = e->cont.code;
            r->literal.cont.items = F->FoldExprList(F, e->cont.items);
            break;
        default:
            paw_assert(e->lit_kind == kHirLitComposite);
            r->literal.comp.path = F->FoldPath(F, e->comp.path);
            r->literal.comp.items = F->FoldExprList(F, e->comp.items);
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
    r->clos.params = F->FoldDeclList(F, e->params);
    if (e->has_body) {
        r->clos.body = COPY_BLOCK(F, e->body);
        r->clos.has_body = PAW_TRUE;
    } else {
        r->clos.expr = F->FoldExpr(F, e->expr);
    }
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
    r->type.generics = F->FoldDeclList(F, d->generics);
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
    r->adt.name = d->name;
    r->adt.generics = F->FoldDeclList(F, d->generics);
    r->adt.fields = F->FoldDeclList(F, d->fields);
    r->adt.monos = F->FoldDeclList(F, d->monos);
    return r;
}

static struct HirDecl *copy_variant_decl(struct HirFolder *F, struct HirVariantDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
    r->variant.fields = F->FoldDeclList(F, d->fields);
    r->variant.index = d->index;
    r->variant.name = d->name;
    return r;
}

static struct HirDecl *copy_var_decl(struct HirFolder *F, struct HirVarDecl *d)
{
    struct HirDecl *r = copy_prep_decl(F, d);
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
    r->call.args = F->FoldExprList(F, e->args);
    r->call.func = e->func;
    return r;
}

static struct HirPath *copy_path(struct HirFolder *F, struct HirPath *e)
{
    struct HirPath *r = pawHir_path_new(F->hir);
    for (int i = 0; i < e->count; ++i) {
        struct HirSegment *seg = pawHir_path_get(e, i);
        pawHir_path_add(F->hir, r, seg->name, seg->types, seg->type);
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
    r->func.receiver = NULL; // set during visit_*()
    r->func.name = d->name;
    r->func.generics = F->FoldDeclList(F, d->generics);
    r->func.params = F->FoldDeclList(F, d->params);
    r->func.body = COPY_BLOCK(F, d->body);
    r->func.monos = F->FoldDeclList(F, d->monos);
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
    r->for_.control = F->FoldDecl(F, s->control);
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

static void setup_copy_pass(struct HirFolder *F, struct Copier *C)
{
    pawHir_folder_init(F, C->hir, C);
    F->FoldExprList = copy_expr_list;
    F->FoldDeclList = copy_decl_list;
    F->FoldStmtList = copy_stmt_list;
    F->FoldTypeList = copy_type_list;
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
    F->FoldMapElem = copy_map_elem;
    F->FoldStructField = copy_struct_field;
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
    struct Copier C = {
        .P = hir->P,
        .hir = hir,
    };
    struct HirFolder F;
    setup_copy_pass(&F, &C);
    return F.FoldDecl(&F, decl);
}

struct Subst {
    struct Subst *outer;
    struct HirTypeList *before;
    struct HirTypeList *after;
    struct Stenciler *S;
};

struct Expander {
    struct HirFolder copy;
    struct Subst *subst;
    struct Resolver *R;
    struct Hir *hir;
    paw_Env *P;
    int nexpand;
};

static struct HirTypeList *expand_typelist(struct HirFolder *F, struct HirTypeList *list)
{
    if (list == NULL) return NULL;
    struct HirTypeList *copy = pawHir_type_list_new(F->hir);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = F->FoldType(F, list->data[i]);
        pawHir_type_list_push(F->hir, copy, type);
    }
    return copy;
}

static struct HirType *expand_tuple(struct HirFolder *F, struct HirTupleType *t)
{
    struct Expander *E = F->ud;
    struct HirType *r = pawHir_new_type(E->hir, t->line, kHirTupleType);
    r->tuple.elems = F->FoldTypeList(F, t->elems);
    return r;
}

static struct HirType *expand_fptr(struct HirFolder *F, struct HirFuncPtr *t)
{
    struct Expander *E = F->ud;
    struct HirType *r = pawHir_new_type(E->hir, t->line, kHirFuncPtr);
    r->fptr.params = F->FoldTypeList(F, t->params);
    r->fptr.result = F->FoldType(F, t->result);
    return r;
}

static struct HirType *expand_fdef(struct HirFolder *F, struct HirFuncDef *t)
{
    struct Expander *E = F->ud;
    struct HirTypeList *types = expand_typelist(F, t->types);
    struct HirDecl *base = pawHir_get_decl(E->hir, t->base);
    struct HirDecl *inst = pawP_instantiate(E->R, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *expand_adt(struct HirFolder *F, struct HirAdt *t)
{
    struct Expander *E = F->ud;
    if (t->did <= PAW_TSTRING) {
        return HIR_CAST_TYPE(t);
    }

    struct HirTypeList *types = expand_typelist(F, t->types);
    struct HirDecl *base = pawHir_get_decl(E->hir, t->base);
    struct HirDecl *inst = pawP_instantiate(E->R, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *expand_generic(struct HirFolder *F, struct HirGeneric *t)
{
    struct Expander *E = F->ud;
    const struct HirTypeList *before = E->subst->before;
    const struct HirTypeList *after = E->subst->after;
    for (int i = 0; i < before->count; ++i) {
        struct HirType *search = before->data[i];
        if (HirIsGeneric(search)) {
            struct HirGeneric *g = HirGetGeneric(search);
            if (t->did == g->did) {
                return after->data[i];
            }
        }
    }
    return HIR_CAST_TYPE(t);
}

static struct HirFuncDecl *do_expand(struct HirFolder *F, struct HirFuncDecl *base, struct HirDecl *inst)
{
    struct Expander *E = F->ud;
    struct Subst subst = {
        .outer = E->subst,
        .before = HirGetFuncDef(base->type)->types,
        .after = HirGetInstanceDecl(inst)->types,
    };
    E->subst = &subst;

    inst->func.fn_kind = base->fn_kind;
    inst->func.kind = kHirFuncDecl;
    inst->func.name = base->name;
    inst->func.generics = NULL;

    // copy the original polymorphic function subtree
    struct HirType *type = E->copy.FoldType(&E->copy, base->type);
    struct HirDeclList *params = E->copy.FoldDeclList(&E->copy, base->params);
    struct HirBlock *body = FOLD_BLOCK(&E->copy, base->body);

    // replace generics with concrete types and create further instantiations to
    // be expanded next round
    inst->func.type = F->FoldType(F, type);
    inst->func.params = F->FoldDeclList(F, params);
    inst->func.body = FOLD_BLOCK(F, body);

    E->subst = subst.outer;
    return &inst->func;
}

static struct HirDecl *find_decl(struct Expander *E, struct HirType *type)
{
    DefId did;
    switch (HIR_KINDOF(type)) {
        case kHirAdt: 
            did = HirGetAdt(type)->base;
            break;
        case kHirFuncDef: 
            did = HirGetFuncDef(type)->base;
            break;
        default: 
            did = HirGetGeneric(type)->did;
    }
    return pawHir_get_decl(E->hir, did);
}

static struct HirPath *expand_path(struct HirFolder *F, struct HirPath *path)
{
    struct Expander *E = F->ud;
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment *ps = pawHir_path_get(path, i);
        if (ps->types != NULL) {
            struct HirTypeList *types = F->FoldTypeList(F, ps->types);
            struct HirDecl *base = find_decl(E, ps->type);
            struct HirDecl *inst = pawP_instantiate(E->R, base, types);
            ps->type = HIR_TYPEOF(inst);
        } else {
            ps->type = F->FoldType(F, ps->type);
        }
    }
    return path;
}

static struct HirDecl *expand_func_decl(struct HirFolder *F, struct HirFuncDecl *d)
{
    struct Expander *E = F->ud;
    const int n = d->monos ? d->monos->count : 0;
    for (int i = 0; i < n; ++i) {
        struct HirDecl *decl = d->monos->data[i];
        if (HirIsInstanceDecl(decl)) {
            do_expand(F, d, decl);
            ++E->nexpand;
        }
    }
    return HIR_CAST_DECL(d);
}

static struct HirExpr *expand_call_expr(struct HirFolder *F, struct HirCallExpr *e)
{
    struct Expander *E = F->ud;
    if (HirIsFuncDef(e->func)) {
        struct HirFuncDef *fdef = HirGetFuncDef(e->func);
        struct HirTypeList *types = expand_typelist(F, fdef->types);
        struct HirDecl *base = pawHir_get_decl(F->hir, fdef->base);
        struct HirDecl *inst = pawP_instantiate(E->R, base, types);
        e->func = HIR_TYPEOF(inst);
    } else {
        e->func = F->FoldType(F, e->func);
    }
    e->target = F->FoldExpr(F, e->target);
    e->args = F->FoldExprList(F, e->args);
    return HIR_CAST_EXPR(e);
}

void pawHir_expand(struct Resolver *R, struct Hir *hir)
{
    struct HirFolder F;
    struct Expander E = {
        .hir = R->hir,
        .P = ENV(R),
        .R = R,
    };
    struct Copier C = {
        .hir = hir,
        .P = ENV(R),
    };
    setup_copy_pass(&E.copy, &C);
    pawHir_folder_init(&F, hir, &E);
    F.FoldFuncDecl = expand_func_decl;
    F.FoldCallExpr = expand_call_expr;
    F.FoldPath = expand_path;
    F.FoldTypeList = expand_typelist;
    F.FoldAdt = expand_adt;
    F.FoldTupleType = expand_tuple;
    F.FoldFuncPtr = expand_fptr;
    F.FoldFuncDef = expand_fdef;
    F.FoldGeneric = expand_generic;

    do {
        E.nexpand = 0;
        F.FoldDeclList(&F, hir->items);
    } while (E.nexpand > 0);
}

struct Printer {
    struct Hir *hir;
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

static void repr_type_aux(struct Printer *P, struct HirType *type);

static void repr_typelist(struct Printer *P, struct HirTypeList *list)
{
    for (int i = 0; i < list->count; ++i) {
        repr_type_aux(P, list->data[i]);
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

static void repr_type_aux(struct Printer *P, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirTupleType:
            printf("(");
            for (int i = 0; i < type->tuple.elems->count; ++i) {
                repr_type_aux(P, type->tuple.elems->data[i]);
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
                printf(" %s", get_typename(P->hir->dm, type->fdef.did));
            }
            printf("(");
            for (int i = 0; i < type->fptr.params->count; ++i) {
                repr_type_aux(P, type->fptr.params->data[i]);
                if (i < type->fptr.params->count - 1) {
                    printf(", ");
                }
            }
            printf(") -> ");
            repr_type_aux(P, type->fptr.result);
            break;
        case kHirAdt: 
            printf("%s", get_typename(P->hir->dm, type->adt.did));
            if (type->adt.types != NULL) {
                printf("<");
                const struct HirTypeList *list = type->adt.types;
                for (int i = 0; i < list->count; ++i) {
                    repr_type_aux(P, list->data[i]);
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

static void repr_type(struct Printer *P, struct HirType *type)
{
    if (type != NULL) {
        repr_type_aux(P, type);
    } else {
        printf("(null)");
    }
    printf("\n");
}

static void dump_stmt(struct Printer *, struct HirStmt *);
static void dump_expr(struct Printer *, struct HirExpr *);

#define DEFINE_KIND_PRINTER(name, T) \
    static int print_##name##_kind(struct Printer *P, void *node) \
    { \
        if (node != NULL) { \
            struct T *typed = node; \
            printf("%s {\n", k##T##Names[HIR_KINDOF(typed)]); \
            return 0; \
        } \
        printf("(null)\n"); \
        return -1; \
    }
DEFINE_KIND_PRINTER(expr, HirExpr) 
DEFINE_KIND_PRINTER(decl, HirDecl) 
DEFINE_KIND_PRINTER(stmt, HirStmt) 
DEFINE_KIND_PRINTER(type, HirType) 

#define dump_block(P, b) check_exp(HirIsBlock(HIR_CAST_STMT(b)), dump_stmt(P, HIR_CAST_STMT(b)))
#define dump_name(P, s) dump_fmt(P, "name: %s\n", s ? s->text : NULL)

static void dump_expr(struct Printer *, struct HirExpr *);
static void dump_decl(struct Printer *, struct HirDecl *);
static void dump_stmt(struct Printer *, struct HirStmt *);
static void dump_type(struct Printer *, struct HirType *);

#define DEFINE_LIST_PRINTER(name, T) \
    static void dump_##name##_list(struct Printer *P, struct T##List *list, const char *name) \
    { \
        dump_fmt(P, "%s: " #T "List {\n", name); \
        ++P->indent; \
        if (list != NULL) { \
            for (int i = 0; i < list->count; ++i) { \
                dump_msg(P, "" /* indent */); \
                dump_##name(P, list->data[i]); \
            } \
        } \
        --P->indent; \
        dump_msg(P, "}\n"); \
    }
DEFINE_LIST_PRINTER(expr, HirExpr) 
DEFINE_LIST_PRINTER(decl, HirDecl)
DEFINE_LIST_PRINTER(stmt, HirStmt)
DEFINE_LIST_PRINTER(type, HirType)

static void dump_path(struct Printer *P, struct HirPath *p)
{
    for (int i = 0; i < p->count; ++i) {
        struct HirSegment *seg = p->data[i];
        printf("%s", seg->name->text);
        if (seg->types != NULL) {
            printf("<");
            for (int j = 0; j < seg->types->count; ++j) {
                pawHir_repr_type(P->hir, seg->types->data[i]);
                if (j < seg->types->count - 1) {
                    printf(", ");
                }
            }
            printf(">");
        }
    }
    printf("\n");
}

static void dump_type(struct Printer *P, struct HirType *t)
{
    if (print_type_kind(P, t)) {
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", t->hdr.line);
    switch (HIR_KINDOF(t)) {
        case kHirTupleType:
            dump_type_list(P, t->tuple.elems, "elems");
            break;
        case kHirFuncDef:
            dump_fmt(P, "base: %d\n", t->fdef.base);
            dump_fmt(P, "did: %d\n", t->fdef.did);
            if (t->fdef.types != NULL) {
                dump_type_list(P, t->fdef.types, "types");
            }
            // (fallthrough)
        case kHirFuncPtr:
            dump_type_list(P, t->fptr.params, "params");
            dump_msg(P, "result: ");
            dump_type(P, t->fptr.result);
            break;
        case kHirAdt: 
            dump_fmt(P, "name: %s\n", get_typename(P->hir->dm, t->adt.did));
            dump_fmt(P, "base: %d\n", t->adt.base);
            dump_fmt(P, "did: %d\n", t->adt.did);
            if (t->adt.types != NULL) {
                dump_type_list(P, t->adt.types, "types");
            }
            break;
        case kHirUnknown:
            dump_fmt(P, "index: %d\n", t->unknown.index);
            break;
        default:
            paw_assert(HirIsGeneric(t));
            dump_fmt(P, "name: %s\n", t->generic.name->text);
            dump_fmt(P, "did: %d\n", t->generic.did);
    }
    --P->indent;
    dump_msg(P, "}\n");
}

static void dump_decl(struct Printer *P, struct HirDecl *d)
{
    if (print_decl_kind(P, d)) {
        return;
    }
    ++P->indent;
    dump_fmt(P, "did: %d\n", d->hdr.did);
    dump_fmt(P, "line: %d\n", d->hdr.line);
    dump_msg(P, "type: ");
    dump_type(P, HIR_TYPEOF(d));
    switch (HIR_KINDOF(d)) {
        case kHirFuncDecl:
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
            dump_type_list(P, d->inst.types, "types");
            break;
        default:
            paw_assert(0);
    }
    --P->indent;
    dump_msg(P, "}\n");
}

static void dump_stmt(struct Printer *P, struct HirStmt *s)
{
    if (print_stmt_kind(P, s)) {
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
    if (print_expr_kind(P, e)) {
        return;
    }
    ++P->indent;
    dump_fmt(P, "line: %d\n", e->hdr.line);
    dump_msg(P, "type: ");
    dump_type(P, HIR_TYPEOF(e));
    switch (HIR_KINDOF(e)) {
        case kHirLiteralExpr:
            switch (e->literal.lit_kind) {
                case kHirLitBasic:
                    dump_msg(P, "lit_kind: BASIC\n");
                    switch (e->literal.basic.t) {
                        case PAW_TUNIT:
                            break;
                        case PAW_TBOOL:
                            dump_fmt(P, "value: %s\n",
                                     v_true(e->literal.basic.value) ? "true"
                                                                    : "false");
                            break;
                        case PAW_TINT:
                            dump_fmt(P, "value: %" PRId64 "\n",
                                     v_int(e->literal.basic.value));
                            break;
                        case PAW_TFLOAT:
                            dump_fmt(P, "value: %f\n",
                                     v_float(e->literal.basic.value));
                            break;
                        default:
                            paw_assert(e->literal.basic.t == PAW_TSTRING);
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
            if (e->clos.has_body) {
                dump_block(P, e->clos.body);
            } else {
                dump_expr(P, e->clos.expr);
            }
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
            dump_msg(P, "func: ");
            dump_type(P, e->call.func);
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
        case kHirMapElem:
            dump_msg(P, "key: ");
            dump_expr(P, e->mitem.key);
            dump_msg(P, "value: ");
            dump_expr(P, e->mitem.value);
            break;
        case kHirStructField:
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
    struct Printer P = {.hir = hir};
    dump_path(&P, path);
}

// TODO: Have this output a String, or fill a Buffer
void pawHir_repr_type(struct Hir *hir, struct HirType *type)
{
    struct Printer P = {.hir = hir};
    repr_type(&P, type);
}

void pawHir_dump_type(struct Hir *hir, struct HirType *type)
{
    struct Printer P = {.hir = hir};
    dump_type(&P, type);
}

void pawHir_dump_decl(struct Hir *hir, struct HirDecl *decl)
{
    struct Printer P = {.hir = hir};
    dump_decl(&P, decl);
}

void pawHir_dump_expr(struct Hir *hir, struct HirExpr *expr)
{
    struct Printer P = {.hir = hir};
    dump_expr(&P, expr);
}

void pawHir_dump_stmt(struct Hir *hir, struct HirStmt *stmt)
{
    struct Printer P = {.hir = hir};
    dump_stmt(&P, stmt);
}

void pawHir_dump_path(struct Hir *hir, struct HirPath *path)
{
    struct Printer P = {.hir = hir};
    dump_path(&P, path);
}

static void debug_func_decl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    struct Printer *P = V->ud;
    dump_msg(P, "FuncDecl: ");
    repr_type(P, d->type);
    dump_msg(P, "body: \n");
    ++P->indent;
    V->VisitBlock(V, d->body);
    --P->indent;
    if (d->monos != NULL) {
        dump_msg(P, "monos: \n");
        ++P->indent;
        V->VisitDeclList(V, d->monos);
        --P->indent;
    }
}

static void debug_instance_decl(struct HirVisitor *V, struct HirInstanceDecl *d)
{
    struct Printer *P = V->ud;
    dump_msg(P, "InstanceDecl: ");
    pawHir_repr_type(V->hir, d->type);
}

void pawHir_debug(struct Hir *hir)
{
    struct HirVisitor V;
    struct Printer P = {.hir = hir};
    pawHir_visitor_init(&V, hir, &P);
    V.VisitFuncDecl = debug_func_decl;
    V.VisitInstanceDecl = debug_instance_decl;
    pawHir_visit(&V);
}

