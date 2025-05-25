// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "hir.h"
#include "compile.h"
#include "debug.h"
#include "ir_type.h"
#include "map.h"
#include "mem.h"
#include "rtti.h"
#include "type_folder.h"

#define NEW_NODE(C, T) P_ALLOC(C, NULL, 0, sizeof(T))

struct Hir *pawHir_new(struct Compiler *C, String *name, int modno)
{
    struct Hir *hir = NEW_NODE(C, struct Hir);
    *hir = (struct Hir){
        .pool = C->hir_pool,
        .modno = modno,
        .name = name,
        .P = ENV(C),
        .C = C,
    };
    hir->imports = HirImportList_new(hir);
    hir->items = HirDeclList_new(hir);
    // TODO: hack, should use list of IrDef
    if (C->decls == NULL)
        C->decls = HirDeclList_new(hir);
    return hir;
}

void pawHir_free(struct Hir *hir)
{
    P_ALLOC(hir->C, hir, sizeof(*hir), 0);
}

#define DEFINE_NODE_CONSTRUCTOR(name, T)         \
    struct T *pawHir_new_##name(struct Hir *hir) \
    {                                            \
        if (hir->C->hir_count == UINT_MAX)       \
            pawM_error(ENV(hir));                \
        return NEW_NODE(hir->C, struct T);       \
    }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(type, HirType)
DEFINE_NODE_CONSTRUCTOR(pat, HirPat)
#undef DEFINE_NODE_CONSTRUCTOR

void pawHir_init_segment(struct Hir *hir, struct HirSegment *r, struct HirIdent ident, struct HirTypeList *types)
{
    *r = (struct HirSegment){
        .hid = pawHir_next_id(hir),
        .ident = ident,
        .types = types,
    };
}

DeclId pawHir_register_decl(struct Hir *hir, struct HirDecl *decl)
{
    struct Compiler *C = hir->C;
    struct DynamicMem *dm = C->dm;
    DeclId const did = {
        .value = C->decls->count,
        .modno = hir->modno,
    };
    HirDeclList_push(hir, C->decls, decl);
    decl->hdr.did = did;
    return did;
}

struct HirDecl *pawHir_get_decl(struct Compiler *C, DeclId did)
{
    struct DynamicMem *dm = C->dm;
    paw_assert(did.value < C->decls->count);
    return C->decls->data[did.value];
}

int pawHir_declare_symbol(struct Hir *hir, struct HirScope *scope, struct HirIdent ident, struct HirResult res)
{
    HirScope_push(hir, scope, (struct HirSymbol){
                                .ident = ident,
                                .res = res,
                            });
    return scope->count - 1;
}

int pawHir_find_symbol(struct HirScope *scope, struct HirIdent ident)
{
    for (int i = scope->count - 1; i >= 0; --i) {
        struct HirSymbol const symbol = HirScope_get(scope, i);
        if (pawS_eq(ident.name, symbol.ident.name))
            return i;
    }
    return -1;
}

#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

static void AcceptType(struct HirVisitor *V, struct HirType *node);
static void AcceptExpr(struct HirVisitor *V, struct HirExpr *node);
static void AcceptDecl(struct HirVisitor *V, struct HirDecl *node);
static void AcceptStmt(struct HirVisitor *V, struct HirStmt *node);
static void AcceptPat(struct HirVisitor *V, struct HirPat *node);

#define DEFINE_LIST_ACCEPTOR(name, T)                                                 \
    static void accept_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    {                                                                                 \
        if (list == NULL)                                                             \
            return;                                                                   \
        struct Hir##T *const *pnode;                                                  \
        K_LIST_FOREACH(list, pnode) {                                                 \
            Accept##T(V, *pnode);                                                     \
        }                                                                             \
    }
DEFINE_LIST_ACCEPTOR(decl, Decl)
DEFINE_LIST_ACCEPTOR(expr, Expr)
DEFINE_LIST_ACCEPTOR(stmt, Stmt)
DEFINE_LIST_ACCEPTOR(type, Type)
DEFINE_LIST_ACCEPTOR(pat, Pat)

static void AcceptBlock(struct HirVisitor *V, struct HirBlock *e)
{
    accept_stmt_list(V, e->stmts);
    if (e->result != NULL) AcceptExpr(V, e->result);
}

static void AcceptLogicalExpr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptFieldExpr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    if (e->fid < 0)
        AcceptExpr(V, e->key);
    AcceptExpr(V, e->value);
}

static void AcceptAssignExpr(struct HirVisitor *V, struct HirAssignExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptMatchArm(struct HirVisitor *V, struct HirMatchArm *e)
{
    AcceptPat(V, e->pat);
    if (e->guard != NULL)
        AcceptExpr(V, e->guard);
    AcceptExpr(V, e->result);
}

static void AcceptMatchExpr(struct HirVisitor *V, struct HirMatchExpr *e)
{
    AcceptExpr(V, e->target);
    accept_expr_list(V, e->arms);
}

static void AcceptSegment(struct HirVisitor *V, struct HirSegment *seg)
{
    if (seg->types != NULL) {
        struct HirType *const *ptype;
        K_LIST_FOREACH (seg->types, ptype) {
            AcceptType(V, *ptype);
        }
    }
    VISITOR_CALL(V, Segment, seg);
}

static void AcceptPath(struct HirVisitor *V, struct HirPath *path)
{
    struct HirSegment *pseg;
    K_LIST_FOREACH (path->segments, pseg) {
        AcceptSegment(V, pseg);
    }
    VISITOR_CALL(V, Path, path);
}

static void AcceptLiteralExpr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitComposite:
            AcceptPath(V, &e->comp.path);
            accept_expr_list(V, e->comp.items);
            break;
        case kHirLitContainer:
            accept_expr_list(V, e->cont.items);
            break;
        case kHirLitTuple:
            accept_expr_list(V, e->tuple.elems);
            break;
        case kHirLitBasic:
            break;
    }
}

static void AcceptChainExpr(struct HirVisitor *V, struct HirChainExpr *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptUnOpExpr(struct HirVisitor *V, struct HirUnOpExpr *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptBinOpExpr(struct HirVisitor *V, struct HirBinOpExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptLetStmt(struct HirVisitor *V, struct HirLetStmt *s)
{
    if (s->pat != NULL)
        AcceptPat(V, s->pat);
    if (s->tag != NULL)
        AcceptType(V, s->tag);
    if (s->init != NULL)
        AcceptExpr(V, s->init);
}

static void AcceptExprStmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    AcceptExpr(V, s->expr);
}

static void AcceptClosureExpr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    accept_decl_list(V, e->params);
    if (e->result != NULL)
        AcceptType(V, e->result);
    AcceptExpr(V, e->expr);
}

static void AcceptFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    if (d->tag != NULL)
        AcceptType(V, d->tag);
}

static void AcceptTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    accept_decl_list(V, d->generics);
    AcceptType(V, d->rhs);
}

static void AcceptGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    if (d->bounds != NULL) {
        struct HirGenericBound *pbound;
        K_LIST_FOREACH (d->bounds, pbound) {
            AcceptPath(V, &pbound->path);
        }
    }
}

static void AcceptVariantDecl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    accept_decl_list(V, d->fields);
}

static void AcceptAdtDecl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    accept_type_list(V, d->traits);
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->fields);
    accept_decl_list(V, d->methods);
}

static void AcceptTraitDecl(struct HirVisitor *V, struct HirTraitDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->methods);
}

static void AcceptConstDecl(struct HirVisitor *V, struct HirConstDecl *d)
{
    AcceptType(V, d->tag);
    AcceptExpr(V, d->init);
}

static void AcceptReturnExpr(struct HirVisitor *V, struct HirReturnExpr *s)
{
    if (s->expr != NULL)
        AcceptExpr(V, s->expr);
}

static void AcceptCallExpr(struct HirVisitor *V, struct HirCallExpr *e)
{
    AcceptExpr(V, e->target);
    accept_expr_list(V, e->args);
}

static void AcceptConversionExpr(struct HirVisitor *V, struct HirConversionExpr *e)
{
    AcceptExpr(V, e->arg);
}

static void AcceptPathExpr(struct HirVisitor *V, struct HirPathExpr *e)
{
    AcceptPath(V, &e->path);
}

static void AcceptFuncDecl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->params);
    if (d->body != NULL)
        AcceptExpr(V, d->body);
}

static void AcceptLoopExpr(struct HirVisitor *V, struct HirLoopExpr *s)
{
    AcceptExpr(V, s->block);
}

static void AcceptJumpExpr(struct HirVisitor *V, struct HirJumpExpr *s)
{
    PAW_UNUSED(V);
    PAW_UNUSED(s);
}

static void AcceptIndex(struct HirVisitor *V, struct HirIndex *e)
{
    AcceptExpr(V, e->target);
    AcceptExpr(V, e->index);
}

static void AcceptSelector(struct HirVisitor *V, struct HirSelector *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptDeclStmt(struct HirVisitor *V, struct HirDeclStmt *s)
{
    AcceptDecl(V, s->decl);
}

static void AcceptFuncPtr(struct HirVisitor *V, struct HirFuncPtr *t)
{
    accept_type_list(V, t->params);
    AcceptType(V, t->result);
}

static void AcceptTupleType(struct HirVisitor *V, struct HirTupleType *t)
{
    accept_type_list(V, t->elems);
}

static void AcceptNeverType(struct HirVisitor *V, struct HirNeverType *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptInferType(struct HirVisitor *V, struct HirInferType *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptPathType(struct HirVisitor *V, struct HirPathType *t)
{
    struct HirSegment const *pseg;
    K_LIST_FOREACH (t->path.segments, pseg) {
        accept_type_list(V, pseg->types);
    }
}

static void AcceptOrPat(struct HirVisitor *V, struct HirOrPat *e)
{
    accept_pat_list(V, e->pats);
}

static void AcceptFieldPat(struct HirVisitor *V, struct HirFieldPat *p)
{
    AcceptPat(V, p->pat);
}

static void AcceptStructPat(struct HirVisitor *V, struct HirStructPat *p)
{
    AcceptPath(V, &p->path);
    accept_pat_list(V, p->fields);
}

static void AcceptVariantPat(struct HirVisitor *V, struct HirVariantPat *p)
{
    AcceptPath(V, &p->path);
    accept_pat_list(V, p->fields);
}

static void AcceptTuplePat(struct HirVisitor *V, struct HirTuplePat *p)
{
    accept_pat_list(V, p->elems);
}

static void AcceptBindingPat(struct HirVisitor *V, struct HirBindingPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

static void AcceptPathPat(struct HirVisitor *V, struct HirPathPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

static void AcceptLiteralPat(struct HirVisitor *V, struct HirLiteralPat *p)
{
    AcceptExpr(V, p->expr);
}

static void AcceptWildcardPat(struct HirVisitor *V, struct HirWildcardPat *p)
{
    PAW_UNUSED(V);
    PAW_UNUSED(p);
}

#define VISITOR_POSTCALL(V, name, x) ((V)->PostVisit##name != NULL ? (V)->PostVisit##name(V, x) : (void)0)
#define DEFINE_VISITOR_CASES(X)             \
    case kHir##X: {                         \
        struct Hir##X *x = HirGet##X(node); \
        if (VISITOR_CALL(V, X, x))          \
            Accept##X(V, x);                \
        VISITOR_POSTCALL(V, X, x);          \
    } break;

static void AcceptExpr(struct HirVisitor *V, struct HirExpr *node)
{
    paw_assert(node != NULL);
    if (!V->VisitExpr(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_EXPR_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitExpr(V, node);
}

static void AcceptDecl(struct HirVisitor *V, struct HirDecl *node)
{
    paw_assert(node != NULL);
    if (!V->VisitDecl(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_DECL_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitDecl(V, node);
}

static void AcceptStmt(struct HirVisitor *V, struct HirStmt *node)
{
    paw_assert(node != NULL);
    if (!V->VisitStmt(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_STMT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitStmt(V, node);
}

static void AcceptType(struct HirVisitor *V, struct HirType *node)
{
    paw_assert(node != NULL);
    if (!V->VisitType(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_TYPE_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitType(V, node);
}

static void AcceptPat(struct HirVisitor *V, struct HirPat *node)
{
    paw_assert(node != NULL);
    if (!V->VisitPat(V, node))
        return;

    switch (HIR_KINDOF(node)) {
        HIR_PAT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitPat(V, node);
}

#undef DEFINE_VISITOR_CASES
#undef VISITOR_POSTCALL
#undef VISITOR_CALL

static paw_Bool default_visit_expr(struct HirVisitor *V, struct HirExpr *node) { return PAW_TRUE; }
static paw_Bool default_visit_decl(struct HirVisitor *V, struct HirDecl *node) { return PAW_TRUE; }
static paw_Bool default_visit_stmt(struct HirVisitor *V, struct HirStmt *node) { return PAW_TRUE; }
static paw_Bool default_visit_type(struct HirVisitor *V, struct HirType *node) { return PAW_TRUE; }
static paw_Bool default_visit_pat(struct HirVisitor *V, struct HirPat *node) { return PAW_TRUE; }

static void default_post_visit_expr(struct HirVisitor *V, struct HirExpr *node) {}
static void default_post_visit_decl(struct HirVisitor *V, struct HirDecl *node) {}
static void default_post_visit_stmt(struct HirVisitor *V, struct HirStmt *node) {}
static void default_post_visit_type(struct HirVisitor *V, struct HirType *node) {}
static void default_post_visit_pat(struct HirVisitor *V, struct HirPat *node) {}

void pawHir_visitor_init(struct HirVisitor *V, struct Hir *hir, void *ud)
{
    *V = (struct HirVisitor){
        .hir = hir,
        .ud = ud,

        .VisitExpr = default_visit_expr,
        .VisitDecl = default_visit_decl,
        .VisitStmt = default_visit_stmt,
        .VisitType = default_visit_type,
        .VisitPat = default_visit_pat,

        .PostVisitExpr = default_post_visit_expr,
        .PostVisitDecl = default_post_visit_decl,
        .PostVisitStmt = default_post_visit_stmt,
        .PostVisitType = default_post_visit_type,
        .PostVisitPat = default_post_visit_pat,
    };
}

#define DEFINE_VISITORS(name, T)                                                     \
    void pawHir_visit_##name(struct HirVisitor *V, struct Hir##T *node)              \
    {                                                                                \
        paw_assert(node != NULL);                                                    \
        Accept##T(V, node);                                                          \
    }                                                                                \
    void pawHir_visit_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    {                                                                                \
        if (list == NULL)                                                            \
            return;                                                                  \
        struct Hir##T *const *pt;                                                    \
        K_LIST_FOREACH (list, pt) {                                                  \
            pawHir_visit_##name(V, *pt);                                             \
        }                                                                            \
    }
DEFINE_VISITORS(expr, Expr)
DEFINE_VISITORS(decl, Decl)
DEFINE_VISITORS(stmt, Stmt)
DEFINE_VISITORS(type, Type)
DEFINE_VISITORS(pat, Pat)
#undef DEFINE_VISITORS


struct IrTypeList *pawHir_collect_decl_types(struct Compiler *C, struct HirDeclList *list)
{
    if (list == NULL)
        return NULL;
    struct IrTypeList *types = IrTypeList_new(C);
    IrTypeList_reserve(C, types, list->count);

    struct HirDecl *const *pdecl;
    K_LIST_FOREACH (list, pdecl) {
        IrTypeList_push(C, types, GET_NODE_TYPE(C, *pdecl));
    }
    return types;
}

enum TraitKind pawHir_kindof_trait(struct Compiler *C, struct HirTraitDecl *d)
{
    if (pawS_eq(d->ident.name, CSTR(C, CSTR_HASH))) {
        return TRAIT_HASH;
    } else if (pawS_eq(d->ident.name, CSTR(C, CSTR_EQUALS))) {
        return TRAIT_EQUALS;
    } else {
        return TRAIT_USER;
    }
}

paw_Bool pawHir_is_pub_decl(struct HirDecl *decl)
{
    switch (HIR_KINDOF(decl)) {
        case kHirTraitDecl:
            return HirGetTraitDecl(decl)->is_pub;
            break;
        case kHirAdtDecl:
            return HirGetAdtDecl(decl)->is_pub;
            break;
        case kHirFuncDecl:
            return HirGetFuncDecl(decl)->is_pub;
            break;
        case kHirTypeDecl:
            return HirGetTypeDecl(decl)->is_pub;
        case kHirConstDecl:
            return HirGetConstDecl(decl)->is_pub;
        case kHirFieldDecl:
        case kHirGenericDecl:
        case kHirVariantDecl:
            PAW_UNREACHABLE();
    }
}


struct Printer {
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define DUMP_FMT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define DUMP_CSTR(P, cs) pawL_add_string(ENV(P), (P)->buf, cs)
#define DUMP_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)
#define DUMP_STR(P, s) DUMP_CSTR(P, s->text)

static paw_Bool is_unit_type(struct HirType *type)
{
    return HirIsTupleType(type)
        && HirGetTupleType(type)->elems->count == 0;
}

static paw_Bool is_unit_lit(struct HirExpr *expr)
{
    return HirIsLiteralExpr(expr)
        && HirGetLiteralExpr(expr)->basic.code == BUILTIN_UNIT;
}

static void add_newline(struct Printer *P)
{
    DUMP_CHAR(P, '\n');
}

static void add_indentation(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        DUMP_CSTR(P, "    ");
    }
}

static void newline_indent(struct Printer *P)
{
    add_newline(P);
    add_indentation(P);
}

static void dump_type(struct Printer *P, struct HirType *type);
static void dump_expr(struct Printer *P, struct HirExpr *expr);
static void dump_decl(struct Printer *P, struct HirDecl *decl);
static void dump_stmt(struct Printer *P, struct HirStmt *stmt);
static void dump_pat(struct Printer *P, struct HirPat *pat);

static void dump_methods(struct Printer *P, struct HirDeclList *methods)
{
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH(methods, pdecl) {
        add_indentation(P);
        dump_decl(P, *pdecl);
        add_newline(P);
    }
}

static void dump_stmts(struct Printer *P, struct HirStmtList *stmts)
{
    struct HirStmt *const *pstmt;
    K_LIST_FOREACH(stmts, pstmt) {
        dump_stmt(P, *pstmt);
    }
}

static void dump_adt_fields(struct Printer *P, struct HirDeclList *fields)
{
    struct HirDecl *const *pdecl;
    K_LIST_FOREACH(fields, pdecl) {
        add_indentation(P);
        dump_decl(P, *pdecl);
        DUMP_CHAR(P, ',');
        add_newline(P);
    }
}

static void dump_literal_fields(struct Printer *P, struct HirExprList *fields)
{
    ++P->indent;
    add_newline(P);

    struct HirExpr *const *pexpr;
    K_LIST_FOREACH(fields, pexpr) {
        add_indentation(P);
        dump_expr(P, *pexpr);
        DUMP_CHAR(P, ',');
        add_newline(P);
    }

    --P->indent;
    add_indentation(P);
}

static void dump_variant_fields(struct Printer *P, struct HirDeclList *fields)
{
    int index;
    struct HirDecl *const *pdecl;
    K_LIST_ENUMERATE (fields, index, pdecl) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_decl(P, *pdecl);
    }
}

static void dump_traits(struct Printer *P, struct HirTypeList *traits)
{
    if (traits->count == 0) return;
    DUMP_CSTR(P, ": ");

    int index;
    struct HirType *const *ptype;
    K_LIST_ENUMERATE (traits, index, ptype) {
        if (index > 0) DUMP_CSTR(P, " + ");
        dump_type(P, *ptype);
    }
}

static void dump_match_body(struct Printer *P, struct HirExprList *arms)
{
    DUMP_CSTR(P, " {");
    ++P->indent;
    add_newline(P);

    int index;
    struct HirExpr *const *pexpr;
    K_LIST_ENUMERATE (arms, index, pexpr) {
        add_indentation(P);
        dump_expr(P, *pexpr);
        DUMP_CHAR(P, ',');
        add_newline(P);
    }

    --P->indent;
    add_indentation(P);
    DUMP_CHAR(P, '}');
}

static void dump_args(struct Printer *P, struct HirExprList *args)
{
    int index;
    struct HirExpr *const *pexpr;
    K_LIST_ENUMERATE (args, index, pexpr) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_expr(P, *pexpr);
    }
}

static void dump_params(struct Printer *P, struct HirDeclList *params)
{
    int index;
    struct HirDecl *const *pdecl;
    K_LIST_ENUMERATE (params, index, pdecl) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_decl(P, *pdecl);
    }
}

static void dump_types(struct Printer *P, struct HirTypeList *types)
{
    int index;
    struct HirType *const *ptype;
    K_LIST_ENUMERATE (types, index, ptype) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_type(P, *ptype);
    }
}

static void dump_generics(struct Printer *P, struct HirDeclList *generics)
{
    if (generics == NULL) return;
    DUMP_CHAR(P, '<');

    int index;
    struct HirDecl *const *pdecl;
    K_LIST_ENUMERATE (generics, index, pdecl) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_decl(P, *pdecl);
    }

    DUMP_CHAR(P, '>');
}

static void dump_path(struct Printer *P, struct HirPath *p, paw_Bool is_type)
{
    int index;
    struct HirSegment *pseg;
    K_LIST_ENUMERATE (p->segments, index, pseg) {
        if (index > 0) DUMP_CSTR(P, "::");
        DUMP_STR(P, pseg->ident.name);
        if (pseg->types != NULL) {
            if (!is_type)
                DUMP_CSTR(P, "::");
            DUMP_CHAR(P, '<');
            dump_types(P, pseg->types);
            DUMP_CHAR(P, '>');
        }
    }
}

static void dump_bounds(struct Printer *P, struct HirBoundList *bounds)
{
    int index;
    struct HirGenericBound *pbound;
    K_LIST_ENUMERATE (bounds, index, pbound) {
        if (index > 0) DUMP_CSTR(P, " + ");
        dump_path(P, &pbound->path, PAW_TRUE);
    }
}

static void dump_pats(struct Printer *P, struct HirPatList *pats)
{
    int index;
    struct HirPat *const *ppat;
    K_LIST_ENUMERATE (pats, index, ppat) {
        if (index > 0) DUMP_CSTR(P, ", ");
        dump_pat(P, *ppat);
    }
}

static void dump_pat(struct Printer *P, struct HirPat *pat)
{
    switch (HIR_KINDOF(pat)) {
        case kHirBindingPat: {
            struct HirBindingPat *p = HirGetBindingPat(pat);
            DUMP_STR(P, p->ident.name);
            break;
        }
        case kHirOrPat: {
            int index;
            struct HirPat *const *ppat;
            struct HirOrPat *p = HirGetOrPat(pat);
            K_LIST_ENUMERATE (p->pats, index, ppat) {
                if (index > 0) DUMP_CSTR(P, " | ");
                dump_pat(P, *ppat);
            }
            break;
        }
        case kHirFieldPat: {
            struct HirFieldPat *p = HirGetFieldPat(pat);
            if (p->ident.name != NULL) {
                DUMP_STR(P, p->ident.name);
            }
            if (p->pat != NULL) {
                if (p->ident.name != NULL)
                    DUMP_CSTR(P, ": ");
                dump_pat(P, p->pat);
            }
            break;
        }
        case kHirStructPat: {
            struct HirStructPat *p = HirGetStructPat(pat);
            dump_path(P, &p->path, PAW_FALSE);
            dump_pats(P, p->fields);
            break;
        }
        case kHirVariantPat: {
            struct HirVariantPat *p = HirGetVariantPat(pat);
            dump_path(P, &p->path, PAW_FALSE);
            if (p->fields->count > 0) {
                DUMP_CHAR(P, '(');
                dump_pats(P, p->fields);
                DUMP_CHAR(P, ')');
            }
            break;
        }
        case kHirTuplePat: {
            struct HirTuplePat *p = HirGetTuplePat(pat);
            DUMP_CHAR(P, '(');
            dump_pats(P, p->elems);
            DUMP_CHAR(P, ')');
            break;
        }
        case kHirPathPat: {
            struct HirPathPat *p = HirGetPathPat(pat);
            dump_path(P, &p->path, PAW_FALSE);
            break;
        }
        case kHirLiteralPat: {
            struct HirLiteralPat *p = HirGetLiteralPat(pat);
            dump_expr(P, p->expr);
            break;
        }
        case kHirWildcardPat:
            DUMP_CHAR(P, '_');
            break;
    }
}

static void dump_decl(struct Printer *P, struct HirDecl *decl)
{
    switch (HIR_KINDOF(decl)) {
        case kHirConstDecl: {
            struct HirConstDecl *d = HirGetConstDecl(decl);
            DUMP_CSTR(P, "let ");
            DUMP_STR(P, d->ident.name);
            DUMP_CSTR(P, ": ");
            dump_type(P, d->tag);
            if (d->init != NULL) {
                DUMP_CSTR(P, " = ");
                dump_expr(P, d->init);
            }
            break;
        }
        case kHirTraitDecl: {
            struct HirTraitDecl *d = HirGetTraitDecl(decl);
            DUMP_CSTR(P, "trait ");
            DUMP_STR(P, d->ident.name);
            dump_generics(P, d->generics);
            DUMP_CHAR(P, '{');
            add_newline(P);
            ++P->indent;
            dump_methods(P, d->methods);
            --P->indent;
            DUMP_CHAR(P, '}');
            break;
        }
        case kHirFuncDecl: {
            struct HirFuncDecl *d = HirGetFuncDecl(decl);
            if (d->is_pub) DUMP_CSTR(P, "pub ");
            DUMP_CSTR(P, "fn ");
            DUMP_STR(P, d->ident.name);
            dump_generics(P, d->generics);
            DUMP_CHAR(P, '(');
            dump_params(P, d->params);
            DUMP_CHAR(P, ')');
            if (!is_unit_type(d->result)) {
                DUMP_CSTR(P, " -> ");
                dump_type(P, d->result);
            }
            if (d->body != NULL) {
                DUMP_CHAR(P, ' ');
                dump_expr(P, d->body);
            } else {
                DUMP_CHAR(P, ';');
            }
            break;
        }
        case kHirFieldDecl: {
            struct HirFieldDecl *d = HirGetFieldDecl(decl);
            if (d->is_pub)
                DUMP_CSTR(P, "pub ");
            if (d->ident.name != NULL) {
                DUMP_STR(P, d->ident.name);
            }
            if (d->tag != NULL) {
                DUMP_CSTR(P, ": ");
                dump_type(P, d->tag);
            }
            break;
        }
        case kHirVariantDecl: {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            DUMP_STR(P, d->ident.name);
            if (d->fields != NULL) {
                DUMP_CHAR(P, '(');
                dump_variant_fields(P, d->fields);
                DUMP_CHAR(P, ')');
            }
            break;
        }
        case kHirAdtDecl: {
            struct HirAdtDecl *d = HirGetAdtDecl(decl);
            if (d->is_struct) {
                DUMP_CSTR(P, "struct ");
            } else {
                DUMP_CSTR(P, "enum ");
            }
            DUMP_STR(P, d->ident.name);
            dump_generics(P, d->generics);
            dump_traits(P, d->traits);
            DUMP_CSTR(P, " {");
            ++P->indent;
            add_newline(P);
            dump_adt_fields(P, d->fields);
            dump_methods(P, d->methods);
            --P->indent;
            DUMP_CHAR(P, '}');
            break;
        }
        case kHirGenericDecl: {
            struct HirGenericDecl *d = HirGetGenericDecl(decl);
            DUMP_STR(P, d->ident.name);
            if (d->bounds != NULL) {
                DUMP_CSTR(P, ": ");
                dump_bounds(P, d->bounds);
            }
            break;
        }
        case kHirTypeDecl: {
            struct HirTypeDecl *d = HirGetTypeDecl(decl);
            DUMP_CSTR(P, "type ");
            DUMP_STR(P, d->ident.name);
            dump_generics(P, d->generics);
            DUMP_CSTR(P, " = ");
            dump_type(P, d->rhs);
            break;
        }
    }
}

static void dump_stmt(struct Printer *P, struct HirStmt *stmt)
{
    add_indentation(P);
    switch (HIR_KINDOF(stmt)) {
        case kHirLetStmt: {
            struct HirLetStmt *s = HirGetLetStmt(stmt);
            DUMP_CSTR(P, "let ");
            dump_pat(P, s->pat);
            if (s->tag != NULL) {
                DUMP_CSTR(P, ": ");
                dump_type(P, s->tag);
            }
            if (s->init != NULL) {
                DUMP_CSTR(P, " = ");
                dump_expr(P, s->init);
            }
            break;
        }
        case kHirExprStmt: {
            struct HirExprStmt *s = HirGetExprStmt(stmt);
            dump_expr(P, s->expr);
            if (!HirIsLoopExpr(s->expr)
                    && !HirIsMatchExpr(s->expr)
                    && !HirIsBlock(s->expr)) {
                DUMP_CHAR(P, ';');
            }
            break;
        }
        case kHirDeclStmt: {
            struct HirDeclStmt *s = HirGetDeclStmt(stmt);
            dump_decl(P, s->decl);
            DUMP_CHAR(P, ';');
            break;
        }
    }
    add_newline(P);
}

static void dump_type(struct Printer *P, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirPathType: {
            struct HirPathType *t = HirGetPathType(type);
            dump_path(P, &t->path, PAW_TRUE);
            break;
        }
        case kHirTupleType: {
            struct HirTupleType *t = HirGetTupleType(type);
            dump_types(P, t->elems);
            break;
        }
        case kHirFuncPtr: {
            struct HirFuncPtr *t = HirGetFuncPtr(type);
            DUMP_CSTR(P, "fn(");
            dump_types(P, t->params);
            DUMP_CHAR(P, ')');
            if (!is_unit_type(t->result)) {
                DUMP_CSTR(P, " -> ");
                dump_type(P, t->result);
            }
            break;
        }
        case kHirNeverType:
            DUMP_CHAR(P, '!');
            break;
        case kHirInferType:
            DUMP_CHAR(P, '_');
            break;
    }
}

static void dump_expr(struct Printer *P, struct HirExpr *expr)
{
    switch (HIR_KINDOF(expr)) {
        case kHirLogicalExpr: {
            struct HirLogicalExpr *e = HirGetLogicalExpr(expr);
            dump_expr(P, e->lhs);
            DUMP_CSTR(P, e->is_and ? " && " : " || ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirPathExpr: {
            struct HirPathExpr *e = HirGetPathExpr(expr);
            dump_path(P, &e->path, PAW_FALSE);
            break;
        }
        case kHirChainExpr: {
            struct HirChainExpr *e = HirGetChainExpr(expr);
            dump_expr(P, e->target);
            DUMP_CHAR(P, '?');
            break;
        }
        case kHirMatchExpr: {
            struct HirMatchExpr *e = HirGetMatchExpr(expr);
            DUMP_CSTR(P, "match ");
            dump_expr(P, e->target);
            dump_match_body(P, e->arms);
            break;
        }
        case kHirMatchArm: {
            struct HirMatchArm *e = HirGetMatchArm(expr);
            dump_pat(P, e->pat);
            if (e->guard != NULL) {
                DUMP_CSTR(P, " if ");
                dump_expr(P, e->guard);
            }
            DUMP_CSTR(P, " => ");
            dump_expr(P, e->result);
            break;
        }
        case kHirClosureExpr: {
            struct HirClosureExpr *e = HirGetClosureExpr(expr);
            DUMP_CHAR(P, '|');
            dump_params(P, e->params);
            DUMP_CHAR(P, '|');
            if (e->result != NULL) {
                DUMP_CSTR(P, " -> ");
                dump_type(P, e->result);
            }
            DUMP_CHAR(P, ' ');
            dump_expr(P, e->expr);
            break;
        }
        case kHirConversionExpr: {
            struct HirConversionExpr *e = HirGetConversionExpr(expr);
            dump_expr(P, e->arg);
            DUMP_CSTR(P, " as ");
            // TODO: dump type
            break;
        }
        case kHirFieldExpr: {
            struct HirFieldExpr *e = HirGetFieldExpr(expr);
            if (e->fid < 0) {
                dump_expr(P, e->key);
            } else {
                DUMP_STR(P, e->ident.name);
            }
            DUMP_CSTR(P, ": ");
            dump_expr(P, e->value);
            break;
        }
        case kHirJumpExpr: {
            struct HirJumpExpr *e = HirGetJumpExpr(expr);
            if (e->jump_kind == JUMP_BREAK) {
                DUMP_CSTR(P, "break");
            } else {
                DUMP_CSTR(P, "continue");
            }
            break;
        }
        case kHirLiteralExpr: {
            struct HirLiteralExpr *e = HirGetLiteralExpr(expr);
            switch (e->lit_kind) {
                case kHirLitBasic:
                    switch (e->basic.code) {
                        case BUILTIN_UNIT:
                            DUMP_CSTR(P, "()");
                            break;
                        case BUILTIN_BOOL:
                            DUMP_FMT(P, "%s", V_TRUE(e->basic.value) ? "true" : "false");
                            break;
                        case BUILTIN_INT:
                            DUMP_FMT(P, "%I", V_INT(e->basic.value));
                            break;
                        case BUILTIN_FLOAT:
                            DUMP_FMT(P, "%f", V_FLOAT(e->basic.value));
                            break;
                        case BUILTIN_STR:
                            DUMP_FMT(P, "\"%s\"", V_STRING(e->basic.value)->text);
                            break;
                        default:
                            PAW_UNREACHABLE();
                    }
                    break;
                case kHirLitTuple:
                    DUMP_CHAR(P, '(');
                    dump_args(P, e->tuple.elems);
                    if (e->tuple.elems->count == 1)
                        DUMP_CHAR(P, ',');
                    DUMP_CHAR(P, ')');
                    break;
                case kHirLitContainer:
                    DUMP_CHAR(P, '[');
                    if (e->cont.items->count > 0) {
                        dump_literal_fields(P, e->cont.items);
                    } else if (e->cont.code == BUILTIN_MAP) {
                        DUMP_CHAR(P, ':');
                    }
                    DUMP_CHAR(P, ']');
                    break;
                case kHirLitComposite:
                    dump_path(P, &e->comp.path, PAW_FALSE);
                    DUMP_CHAR(P, '{');
                    if (e->comp.items->count > 0) {
                        dump_literal_fields(P, e->comp.items);
                    }
                    DUMP_CHAR(P, '}');
            }
            break;
        }
        case kHirUnOpExpr: {
            struct HirUnOpExpr *e = HirGetUnOpExpr(expr);
            DUMP_CSTR(P, paw_unop_symbol(e->op));
            dump_expr(P, e->target);
            break;
        }
        case kHirBinOpExpr: {
            struct HirBinOpExpr *e = HirGetBinOpExpr(expr);
            dump_expr(P, e->lhs);
            DUMP_CHAR(P, ' ');
            DUMP_CSTR(P, paw_binop_symbol(e->op));
            DUMP_CHAR(P, ' ');
            dump_expr(P, e->rhs);
            break;
        }
        case kHirAssignExpr: {
            struct HirAssignExpr *e = HirGetAssignExpr(expr);
            dump_expr(P, e->lhs);
            DUMP_CSTR(P, " = ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirCallExpr: {
            struct HirCallExpr *e = HirGetCallExpr(expr);
            dump_expr(P, e->target);
            DUMP_CHAR(P, '(');
            dump_args(P, e->args);
            DUMP_CHAR(P, ')');
            break;
        }
        case kHirIndex: {
            struct HirIndex *e = HirGetIndex(expr);
            dump_expr(P, e->target);
            DUMP_CHAR(P, '[');
            dump_expr(P, e->index);
            DUMP_CHAR(P, ']');
            break;
        }
        case kHirSelector: {
            struct HirSelector *e = HirGetSelector(expr);
            dump_expr(P, e->target);
            DUMP_CHAR(P, '.');
            if (e->is_index) {
                DUMP_FMT(P, "%I", e->index);
            } else {
                DUMP_STR(P, e->ident.name);
            }
            break;
        }
        case kHirBlock: {
            struct HirBlock *e = HirGetBlock(expr);
            DUMP_CHAR(P, '{');
            if (e->stmts->count > 0 || e->result != NULL) {
                ++P->indent;
                add_newline(P);
                dump_stmts(P, e->stmts);
                if (e->result != NULL) {
                    add_indentation(P);
                    dump_expr(P, e->result);
                    add_newline(P);
                }
                --P->indent;
                add_indentation(P);
            }
            DUMP_CHAR(P, '}');
            break;
        }
        case kHirLoopExpr: {
            struct HirLoopExpr *e = HirGetLoopExpr(expr);
            DUMP_CSTR(P, "loop ");
            dump_expr(P, e->block);
            break;
        }
        case kHirReturnExpr: {
            struct HirReturnExpr *e = HirGetReturnExpr(expr);
            DUMP_CSTR(P, "return");
            if (e->expr != NULL) {
                DUMP_CHAR(P, ' ');
                dump_expr(P, e->expr);
            }
            break;
        }
    }
}

char const *pawHir_dump(struct Hir *hir)
{
    Buffer buf;
    paw_Env *P = ENV(hir);
    pawL_init_buffer(P, &buf);
    struct Printer print = {
        .buf = &buf,
        .P = P,
    };
    int index;
    struct HirDecl *const *pdecl;
    K_LIST_ENUMERATE (hir->items, index, pdecl) {
        if (index > 0)
            add_newline(&print);
        dump_decl(&print, *pdecl);
        add_newline(&print);
    }
    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

char const *pawHir_print_path(struct Compiler *C, struct HirPath *path)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    dump_path(&(struct Printer){
                   .P = ENV(C),
                   .buf = &buf,
               },
               path, PAW_TRUE);

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

