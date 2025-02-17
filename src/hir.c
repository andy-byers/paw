// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "hir.h"
#include "compile.h"
#include "debug.h"
#include "ir_type.h"
#include "map.h"
#include "mem.h"
#include "type.h"
#include "type_folder.h"

#define LIST_MIN 8

#define NEW_NODE(C, T) pawP_alloc(C, NULL, 0, sizeof(T))

struct Hir *pawHir_new(struct Compiler *C, String *name, int modno)
{
    struct Hir *hir = NEW_NODE(C, struct Hir);
    *hir = (struct Hir){
        .imports = pawHir_import_list_new(C),
        .items = pawHir_decl_list_new(C),
        .pool = &C->dm->pool,
        .modno = modno,
        .name = name,
        .P = ENV(C),
        .C = C,
    };
    return hir;
}

void pawHir_free(struct Hir *hir)
{
    PAW_UNUSED(hir);
}

#define DEFINE_NODE_CONSTRUCTOR(name, T) \
        struct T *pawHir_new_##name(struct Hir *hir) \
        { \
            return NEW_NODE(hir->C, struct T); \
        }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(type, HirType)
DEFINE_NODE_CONSTRUCTOR(pat, HirPat)
#undef DEFINE_NODE_CONSTRUCTOR

#define LIST_MIN_ALLOC 8

void pawHir_init_segment(struct Hir *hir, struct HirSegment *r, String *name, struct HirTypeList *types, DeclId did)
{
    *r = (struct HirSegment){
        .hid = pawHir_next_id(hir),
        .did = did,
        .name = name,
        .types = types,
    };
}

DeclId pawHir_register_decl(struct Hir *hir, struct HirDecl *decl)
{
    struct Compiler *C = hir->C;
    struct DynamicMem *dm = C->dm;
    const DeclId did = {
        .value = C->decls->count,
        .modno = hir->modno,
    };
    K_LIST_PUSH(C, C->decls, decl);
    decl->hdr.did = did;
    return did;
}

struct HirDecl *pawHir_get_decl(struct Compiler *C, DeclId did)
{
    struct DynamicMem *dm = C->dm;
    paw_assert(did.value < C->decls->count);
    return C->decls->data[did.value];
}

struct HirSymbol *pawHir_new_symbol(struct Compiler *C)
{
    return NEW_NODE(C, struct HirSymbol);
}

void pawHir_add_scope(struct Compiler *C, struct HirSymtab *table, struct HirScope *scope)
{
    if (table->count == UINT16_MAX) pawM_error(ENV(C));
    K_LIST_PUSH(C, table, scope);
}

int pawHir_declare_symbol(struct Compiler *C, struct HirScope *scope, struct HirDecl *decl, String *name)
{
    K_LIST_PUSH(C, scope, ((struct HirSymbol){
                    .is_type = HirIsAdtDecl(decl)
                        || HirIsTypeDecl(decl)
                        || HirIsGenericDecl(decl),
                    .decl = decl,
                    .name = name,
                }));
    return scope->count - 1;
}

void pawHir_define_symbol(struct HirScope *scope, int index)
{
    K_LIST_GET(scope, index).is_init = PAW_TRUE;
}

int pawHir_find_symbol(struct HirScope *scope, const String *name)
{
    for (int i = scope->count - 1; i >= 0; --i) {
        const struct HirSymbol symbol = K_LIST_GET(scope, i);
        if (pawS_eq(name, symbol.name)) {
            if (symbol.is_init) return i;
        }
    }
    return -1;
}

#define VISITOR_CALL(V, name, x) ((V)->Visit##name != NULL ? (V)->Visit##name(V, x) : 1)

static void AcceptType(struct HirVisitor *V, struct HirType *node);
static void AcceptExpr(struct HirVisitor *V, struct HirExpr *node);
static void AcceptDecl(struct HirVisitor *V, struct HirDecl *node);
static void AcceptStmt(struct HirVisitor *V, struct HirStmt *node);
static void AcceptPat(struct HirVisitor *V, struct HirPat *node);

#define DEFINE_LIST_ACCEPTOR(name, T) \
    static void accept_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) \
    { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            Accept##T(V, list->data[i]); \
        } \
    }
DEFINE_LIST_ACCEPTOR(decl, Decl)
DEFINE_LIST_ACCEPTOR(expr, Expr)
DEFINE_LIST_ACCEPTOR(stmt, Stmt)
DEFINE_LIST_ACCEPTOR(type, Type)
DEFINE_LIST_ACCEPTOR(pat, Pat)

static void AcceptBlock(struct HirVisitor *V, struct HirBlock *e)
{
    accept_stmt_list(V, e->stmts);
    AcceptExpr(V, e->result);
}

static void AcceptLogicalExpr(struct HirVisitor *V, struct HirLogicalExpr *e)
{
    AcceptExpr(V, e->lhs);
    AcceptExpr(V, e->rhs);
}

static void AcceptFieldExpr(struct HirVisitor *V, struct HirFieldExpr *e)
{
    if (e->fid < 0) AcceptExpr(V, e->key);
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
    if (e->guard != NULL) AcceptExpr(V, e->guard);
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
        for (int j = 0; j < seg->types->count; ++j) {
            AcceptType(V, K_LIST_GET(seg->types, j));
        }
    }
    VISITOR_CALL(V, Segment, seg);
}

static void AcceptPath(struct HirVisitor *V, struct HirPath *path)
{
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment seg = K_LIST_GET(path, i);
        AcceptSegment(V, &seg);
    }
    VISITOR_CALL(V, Path, path);
}

static void AcceptLiteralExpr(struct HirVisitor *V, struct HirLiteralExpr *e)
{
    switch (e->lit_kind) {
        case kHirLitComposite:
            AcceptPath(V, e->comp.path);
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

static void AcceptExprStmt(struct HirVisitor *V, struct HirExprStmt *s)
{
    AcceptExpr(V, s->expr);
}

static void AcceptClosureExpr(struct HirVisitor *V, struct HirClosureExpr *e)
{
    accept_decl_list(V, e->params);
    AcceptExpr(V, e->expr);
}

static void AcceptFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    if (d->tag != NULL) AcceptType(V, d->tag);
}

static void AcceptTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    accept_decl_list(V, d->generics);
    AcceptType(V, d->rhs);
}

static void AcceptGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    PAW_UNUSED(V);
    PAW_UNUSED(d);
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

static void AcceptVarDecl(struct HirVisitor *V, struct HirVarDecl *d)
{
    if (d->tag != NULL) AcceptType(V, d->tag);
    if (d->init != NULL) AcceptExpr(V, d->init);
}

static void AcceptReturnExpr(struct HirVisitor *V, struct HirReturnExpr *s)
{
    if (s->expr != NULL) AcceptExpr(V, s->expr);
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
    AcceptPath(V, e->path);
}

static void AcceptFuncDecl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->params);
    if (d->body != NULL) AcceptExpr(V, d->body);
}

static void AcceptIfExpr(struct HirVisitor *V, struct HirIfExpr *s)
{
    AcceptExpr(V, s->cond);
    AcceptExpr(V, s->then_arm);
    if (s->else_arm != NULL) AcceptExpr(V, s->else_arm);
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
    if (e->first != NULL) AcceptExpr(V, e->first);
    if (e->second != NULL) AcceptExpr(V, e->second);
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

static void AcceptInferType(struct HirVisitor *V, struct HirInferType *t)
{
    PAW_UNUSED(V);
    PAW_UNUSED(t);
}

static void AcceptPathType(struct HirVisitor *V, struct HirPathType *t)
{
    for (int i = 0; i < t->path->count; ++i) {
        struct HirSegment seg = K_LIST_GET(t->path, i);
        accept_type_list(V, seg.types);
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
    AcceptPath(V, p->path);
    accept_pat_list(V, p->fields);
}

static void AcceptVariantPat(struct HirVisitor *V, struct HirVariantPat *p)
{
    AcceptPath(V, p->path);
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
#define DEFINE_VISITOR_CASES(X) case kHir##X: { \
        struct Hir##X *x = HirGet##X(node); \
        if (VISITOR_CALL(V, X, x)) Accept##X(V, x); \
        VISITOR_POSTCALL(V, X, x); \
    } \
    break;

static void AcceptExpr(struct HirVisitor *V, struct HirExpr *node)
{
    paw_assert(node != NULL);
    if (!V->VisitExpr(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_EXPR_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitExpr(V, node);
}

static void AcceptDecl(struct HirVisitor *V, struct HirDecl *node)
{
    paw_assert(node != NULL);
    if (!V->VisitDecl(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_DECL_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitDecl(V, node);
}

static void AcceptStmt(struct HirVisitor *V, struct HirStmt *node)
{
    paw_assert(node != NULL);
    if (!V->VisitStmt(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_STMT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitStmt(V, node);
}

static void AcceptType(struct HirVisitor *V, struct HirType *node)
{
    paw_assert(node != NULL);
    if (!V->VisitType(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_TYPE_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitType(V, node);
}

static void AcceptPat(struct HirVisitor *V, struct HirPat *node)
{
    paw_assert(node != NULL);
    if (!V->VisitPat(V, node)) return;

    switch (HIR_KINDOF(node)) {
        HIR_PAT_LIST(DEFINE_VISITOR_CASES)
    }

    V->PostVisitPat(V, node);
}

#undef DEFINE_VISITOR_CASES
#undef VISITOR_POSTCALL
#undef VISITOR_CALL

static paw_Bool default_visit_expr(struct HirVisitor *V, struct HirExpr *node) {return PAW_TRUE;}
static paw_Bool default_visit_decl(struct HirVisitor *V, struct HirDecl *node) {return PAW_TRUE;}
static paw_Bool default_visit_stmt(struct HirVisitor *V, struct HirStmt *node) {return PAW_TRUE;}
static paw_Bool default_visit_type(struct HirVisitor *V, struct HirType *node) {return PAW_TRUE;}
static paw_Bool default_visit_pat(struct HirVisitor *V, struct HirPat *node) {return PAW_TRUE;}

static void default_post_visit_expr(struct HirVisitor *V, struct HirExpr *node) {}
static void default_post_visit_decl(struct HirVisitor *V, struct HirDecl *node) {}
static void default_post_visit_stmt(struct HirVisitor *V, struct HirStmt *node) {}
static void default_post_visit_type(struct HirVisitor *V, struct HirType *node) {}
static void default_post_visit_pat(struct HirVisitor *V, struct HirPat *node) {}

void pawHir_visitor_init(struct HirVisitor *V, struct Compiler *C, void *ud)
{
    *V = (struct HirVisitor){
        .ud = ud,
        .C = C,

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

#define DEFINE_VISITORS(name, T) \
    void pawHir_visit_##name(struct HirVisitor *V, struct Hir##T *node) { \
        paw_assert(node != NULL); \
        V->line = node->hdr.line; \
        Accept##T(V, node); \
    } \
    void pawHir_visit_##name##_list(struct HirVisitor *V, struct Hir##T##List *list) { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            pawHir_visit_##name(V, K_LIST_GET(list, i)); \
        } \
    }
DEFINE_VISITORS(expr, Expr)
DEFINE_VISITORS(decl, Decl)
DEFINE_VISITORS(stmt, Stmt)
DEFINE_VISITORS(type, Type)
DEFINE_VISITORS(pat, Pat)
#undef DEFINE_VISITORS


struct IrTypeList *pawHir_collect_decl_types(struct Compiler *C, struct HirDeclList *list)
{
    if (list == NULL) return NULL;
    struct IrTypeList *types = pawIr_type_list_new(C);
    for (int i = 0; i < list->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(list, i);
        K_LIST_PUSH(C, types, GET_NODE_TYPE(C, decl));
    }
    return types;
}

struct IrTypeList *pawHir_collect_expr_types(struct Compiler *C, struct HirExprList *list)
{
    if (list == NULL) return NULL;
    struct IrTypeList *types = pawIr_type_list_new(C);
    for (int i = 0; i < list->count; ++i) {
        struct HirExpr *expr = K_LIST_GET(list, i);
        K_LIST_PUSH(C, types, GET_NODE_TYPE(C, expr));
    }
    return types;
}


struct Printer {
    struct Compiler *C;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define PRINT_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define PRINT_STRING(P, str) pawL_add_nstring(ENV(P), (P)->buf, (str)->text, (str)->length)
#define PRINT_FORMAT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define PRINT_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)

static void print_type(struct Printer *, struct HirType *);
static void print_type_list(struct Printer *P, struct HirTypeList *list)
{
    for (int i = 0; i < list->count; ++i) {
        print_type(P, list->data[i]);
        if (i < list->count - 1) PRINT_LITERAL(P, ", ");
    }
}

static void print_path(struct Printer *P, struct HirPath *path)
{
    for (int i = 0; i < path->count; ++i) {
        if (i > 0) PRINT_LITERAL(P, "::");
        struct HirSegment seg = K_LIST_GET(path, i);
        PRINT_STRING(P, seg.name);
        if (seg.types != NULL) {
            PRINT_CHAR(P, '<');
            print_type_list(P, seg.types);
            PRINT_CHAR(P, '>');
        }
    }
}

static void print_type(struct Printer *P, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirTupleType: {
            struct HirTupleType *tup = HirGetTupleType(type);
            PRINT_CHAR(P, '(');
            print_type_list(P, tup->elems);
            if (tup->elems->count == 1) PRINT_CHAR(P, ',');
            PRINT_CHAR(P, ')');
            break;
        }
        case kHirInferType:
            PRINT_CHAR(P, '_');
            break;
        case kHirFuncPtr: {
            struct HirFuncPtr *fptr = HirGetFuncPtr(type);
            PRINT_LITERAL(P, "fn(");
            print_type_list(P, fptr->params);
            PRINT_CHAR(P, ')');
            if (pawP_type2code(P->C, GET_NODE_TYPE(P->C, fptr->result)) != BUILTIN_UNIT) {
                PRINT_LITERAL(P, " -> ");
                print_type(P, fptr->result);
            }
            break;
        }
        case kHirPathType: {
            struct HirPathType *path = HirGetPathType(type);
            print_path(P, path->path);
        }
    }
}

const char *pawHir_print_type(struct Compiler *C, struct HirType *type)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_type(&(struct Printer){
                .P = ENV(C),
                .buf = &buf,
                .C = C,
            }, type);

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

const char *pawHir_print_path(struct Compiler *C, struct HirPath *path)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    print_path(&(struct Printer){
                .P = ENV(C),
                .buf = &buf,
                .C = C,
            }, path);

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}

#if defined(PAW_DEBUG_EXTRA)

#define DUMP_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define DUMP_STRING(P, str) pawL_add_string(ENV(P), (P)->buf, str)
#define DUMP_FSTRING(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)

static void indent_line(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        DUMP_LITERAL(P, "    ");
    }
}

#define DUMP_FMT(P, ...) (indent_line(P), pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__))
#define DUMP_MSG(P, msg) (indent_line(P), pawL_add_string(ENV(P), (P)->buf, msg))

#define DEFINE_KIND_PRINTER(name, T) \
    static int print_##name##_kind(struct Printer *P, void *node) \
    { \
        if (node != NULL) { \
            struct T *typed = node; \
            DUMP_FSTRING(P, "%s {\n", k##T##Names[HIR_KINDOF(typed)]); \
            return 0; \
        } \
        DUMP_LITERAL(P, "(null)\n"); \
        return -1; \
    }
DEFINE_KIND_PRINTER(expr, HirExpr)
DEFINE_KIND_PRINTER(decl, HirDecl)
DEFINE_KIND_PRINTER(stmt, HirStmt)
DEFINE_KIND_PRINTER(type, HirType)
DEFINE_KIND_PRINTER(pat, HirPat)

#define DUMP_NAME(P, s) DUMP_FMT(P, "name: %s\n", s ? s->text : "(null)")

static void dump_expr(struct Printer *, struct HirExpr *);
static void dump_decl(struct Printer *, struct HirDecl *);
static void dump_stmt(struct Printer *, struct HirStmt *);
static void dump_type(struct Printer *, struct HirType *);
static void dump_pat(struct Printer *, struct HirPat *);

#define DEFINE_LIST_PRINTER(name, T) \
    static void dump_##name##_list(struct Printer *P, struct T##List *list, const char *name) \
    { \
        DUMP_FMT(P, "%s: " #T "List {\n", name); \
        ++P->indent; \
        if (list != NULL) { \
            for (int i = 0; i < list->count; ++i) { \
                DUMP_MSG(P, "" /* indent */); \
                dump_##name(P, list->data[i]); \
            } \
        } \
        --P->indent; \
        DUMP_MSG(P, "}\n"); \
    }
DEFINE_LIST_PRINTER(expr, HirExpr)
DEFINE_LIST_PRINTER(decl, HirDecl)
DEFINE_LIST_PRINTER(stmt, HirStmt)
DEFINE_LIST_PRINTER(type, HirType)
DEFINE_LIST_PRINTER(pat, HirPat)

static void dump_segment(struct Printer *P, struct HirSegment seg)
{
    DUMP_STRING(P, seg.name->text);
    if (seg.types != NULL) {
        DUMP_LITERAL(P, "<");
        for (int j = 0; j < seg.types->count; ++j) {
            print_type(P, seg.types->data[j]);
            if (j < seg.types->count - 1) {
                DUMP_LITERAL(P, ", ");
            }
        }
        DUMP_LITERAL(P, ">");
    }
}

static void dump_path(struct Printer *P, struct HirPath *p, const char *name)
{
    DUMP_FMT(P, "%s: ", name);
    for (int i = 0; i < p->count; ++i) {
        if (i != 0) DUMP_LITERAL(P, "::");
        dump_segment(P, p->data[i]);
    }
    pawL_add_char(P->P, P->buf, '\n');
}

static void dump_type(struct Printer *P, struct HirType *type)
{
    if (print_type_kind(P, type)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", type->hdr.line);
    switch (HIR_KINDOF(type)) {
        case kHirTupleType: {
            struct HirTupleType *t = HirGetTupleType(type);
            dump_type_list(P, t->elems, "elems");
            break;
        }
        case kHirFuncPtr: {
            struct HirFuncPtr *t = HirGetFuncPtr(type);
            dump_type_list(P, t->params, "params");
            DUMP_MSG(P, "result: ");
            dump_type(P, t->result);
            break;
        }
        case kHirInferType:
             break;
        case kHirPathType: {
            struct HirPathType *t = HirGetPathType(type);
            for (int i = 0; i < t->path->count; ++i) {
                struct HirSegment seg = K_LIST_GET(t->path, i);
                DUMP_FMT(P, "name: %s\n", seg.name->text);
                DUMP_FMT(P, "did: %d\n", seg.did.value);
                if (seg.types != NULL) {
                    dump_type_list(P, seg.types, "types");
                }
            }
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_decl(struct Printer *P, struct HirDecl *decl)
{
    if (print_decl_kind(P, decl)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "did: DeclId(%d, mod=%d)\n",
            decl->hdr.did.value, decl->hdr.did.modno);
    DUMP_FMT(P, "line: %d\n", decl->hdr.line);
    switch (HIR_KINDOF(decl)) {
        case kHirFuncDecl: {
            struct HirFuncDecl *d = HirGetFuncDecl(decl);
            DUMP_FMT(P, "self: %p\n", (void *)d->self);
            DUMP_FMT(P, "name: %s\n", d->name->text);
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->params, "params");
            DUMP_MSG(P, "body: ");
            dump_expr(P, d->body);
            break;
        }
        case kHirFieldDecl: {
            struct HirFieldDecl *d = HirGetFieldDecl(decl);
            DUMP_NAME(P, d->name);
            break;
        }
        case kHirVarDecl: {
            struct HirVarDecl *d = HirGetVarDecl(decl);
            DUMP_NAME(P, d->name);
            DUMP_MSG(P, "init: ");
            dump_expr(P, d->init);
            break;
        }
        case kHirVariantDecl: {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            DUMP_NAME(P, d->name);
            dump_decl_list(P, d->fields, "fields");
            break;
        }
        case kHirAdtDecl: {
            struct HirAdtDecl *d = HirGetAdtDecl(decl);
            DUMP_NAME(P, d->name);
            DUMP_FMT(P, "is_struct: %d\n", d->is_struct);
            dump_type_list(P, d->traits, "traits");
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->fields, "fields");
            dump_decl_list(P, d->methods, "methods");
            break;
        }
        case kHirTraitDecl: {
            struct HirTraitDecl *d = HirGetTraitDecl(decl);
            DUMP_NAME(P, d->name);
            dump_decl_list(P, d->generics, "generics");
            dump_decl_list(P, d->methods, "methods");
            break;
        }
        case kHirGenericDecl: {
            struct HirGenericDecl *d = HirGetGenericDecl(decl);
            DUMP_NAME(P, d->name);
            break;
        }
        case kHirTypeDecl: {
            struct HirTypeDecl *d = HirGetTypeDecl(decl);
            DUMP_NAME(P, d->name);
            DUMP_MSG(P, "rhs: ");
            dump_type(P, d->rhs);
            dump_decl_list(P, d->generics, "generics");
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_stmt(struct Printer *P, struct HirStmt *stmt)
{
    if (print_stmt_kind(P, stmt)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", stmt->hdr.line);
    switch (HIR_KINDOF(stmt)) {
        case kHirExprStmt: {
            struct HirExprStmt *s = HirGetExprStmt(stmt);
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->expr);
            break;
        }
        case kHirDeclStmt: {
            struct HirDeclStmt *s = HirGetDeclStmt(stmt);
            DUMP_MSG(P, "decl: ");
            dump_decl(P, s->decl);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_pat(struct Printer *P, struct HirPat *pat)
{
    if (print_pat_kind(P, pat)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", pat->hdr.line);
    switch (HIR_KINDOF(pat)) {
        case kHirBindingPat: {
            struct HirBindingPat *p = HirGetBindingPat(pat);
            DUMP_MSG(P, "name: ");
            PRINT_STRING(P, p->name);
            break;
        }
        case kHirOrPat: {
            struct HirOrPat *p = HirGetOrPat(pat);
            dump_pat_list(P, p->pats, "pats");
            break;
        }
        case kHirFieldPat: {
            struct HirFieldPat *p = HirGetFieldPat(pat);
            DUMP_FMT(P, "target: %s", p->name->text);
            DUMP_MSG(P, "pat: ");
            dump_pat(P, p->pat);
            break;
        }
        case kHirStructPat: {
            struct HirStructPat *p = HirGetStructPat(pat);
            dump_path(P, p->path, "path");
            dump_pat_list(P, p->fields, "fields");
            break;
        }
        case kHirVariantPat: {
            struct HirVariantPat *p = HirGetVariantPat(pat);
            DUMP_FMT(P, "index: %d", p->index);
            dump_path(P, p->path, "path");
            dump_pat_list(P, p->fields, "fields");
            break;
        }
        case kHirTuplePat: {
            struct HirTuplePat *p = HirGetTuplePat(pat);
            dump_pat_list(P, p->elems, "elems");
            break;
        }
        case kHirPathPat: {
            struct HirPathPat *p = HirGetPathPat(pat);
            dump_path(P, p->path, "path");
            break;
        }
        case kHirWildcardPat: {
            struct HirWildcardPat *p = HirGetWildcardPat(pat);
            break;
        }
        case kHirLiteralPat: {
            struct HirLiteralPat *p = HirGetLiteralPat(pat);
            DUMP_MSG(P, "expr: ");
            dump_expr(P, p->expr);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_expr(struct Printer *P, struct HirExpr *expr)
{
    if (print_expr_kind(P, expr)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", expr->hdr.line);
    switch (HIR_KINDOF(expr)) {
        case kHirLiteralExpr: {
            struct HirLiteralExpr *e = HirGetLiteralExpr(expr);
            switch (e->lit_kind) {
                case kHirLitBasic:
                    DUMP_MSG(P, "lit_kind: BASIC\n");
                    switch (e->basic.t) {
                        case BUILTIN_UNIT:
                            break;
                        case BUILTIN_BOOL:
                            DUMP_FMT(P, "value: %s\n",
                                     V_TRUE(e->basic.value) ? "true"
                                                                    : "false");
                            break;
                        case BUILTIN_INT:
                            DUMP_FMT(P, "value: %I\n",
                                     V_INT(e->basic.value));
                            break;
                        case BUILTIN_FLOAT:
                            DUMP_FMT(P, "value: %f\n",
                                     V_FLOAT(e->basic.value));
                            break;
                        default:
                            paw_assert(e->basic.t == BUILTIN_STR);
                            DUMP_FMT(P, "value: %s\n",
                                     V_STRING(e->basic.value)->text);
                            break;
                    }
                    break;
                case kHirLitTuple:
                    DUMP_MSG(P, "lit_kind: TUPLE\n");
                    dump_expr_list(P, e->tuple.elems, "elems");
                    break;
                case kHirLitContainer:
                    DUMP_MSG(P, "lit_kind: CONTAINER\n");
                    dump_expr_list(P, e->cont.items, "items");
                    break;
                case kHirLitComposite:
                    DUMP_MSG(P, "lit_kind: COMPOSITE\n");
                    dump_path(P, e->comp.path, "target");
                    pawL_add_char(P->P, P->buf, '\n');
                    dump_expr_list(P, e->comp.items, "items");
            }
            break;
        }
        case kHirChainExpr: {
            struct HirChainExpr *e = HirGetChainExpr(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            break;
        }
        case kHirLogicalExpr: {
            struct HirLogicalExpr *e = HirGetLogicalExpr(expr);
            DUMP_FMT(P, "is_and: %d\n", e->is_and);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirClosureExpr: {
            struct HirClosureExpr *e = HirGetClosureExpr(expr);
            dump_decl_list(P, e->params, "params");
            dump_expr(P, e->expr);
            break;
        }
        case kHirPathExpr: {
            struct HirPathExpr *e = HirGetPathExpr(expr);
            dump_path(P, e->path, "path");
            pawL_add_char(P->P, P->buf, '\n');
            break;
        }
        case kHirConversionExpr: {
            struct HirConversionExpr *e = HirGetConversionExpr(expr);
            DUMP_FMT(P, "to: %d\n", e->to);
            DUMP_MSG(P, "arg: ");
            dump_expr(P, e->arg);
            break;
        }
        case kHirUnOpExpr: {
            struct HirUnOpExpr *e = HirGetUnOpExpr(expr);
            DUMP_FMT(P, "op: %s\n", paw_unop_name(e->op));
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            break;
        }
        case kHirBinOpExpr: {
            struct HirBinOpExpr *e = HirGetBinOpExpr(expr);
            DUMP_FMT(P, "op: %s\n", paw_binop_name(e->op));
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirCallExpr: {
            struct HirCallExpr *e = HirGetCallExpr(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            dump_expr_list(P, e->args, "args");
            break;
        }
        case kHirIndex: {
            struct HirIndex *e = HirGetIndex(expr);
            DUMP_FMT(P, "is_slice: %d\n", e->is_slice);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            DUMP_MSG(P, "first: ");
            dump_expr(P, e->first);
            DUMP_MSG(P, "second: ");
            dump_expr(P, e->second);
            break;
        }
        case kHirSelector: {
            struct HirSelector *e = HirGetSelector(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            if (e->is_index) {
                DUMP_FMT(P, "index: %I\n", e->index);
            } else {
                DUMP_NAME(P, e->name);
            }
            break;
        }
        case kHirFieldExpr: {
            struct HirFieldExpr *e = HirGetFieldExpr(expr);
            if (e->fid >= 0) {
                DUMP_NAME(P, e->name);
            } else {
                DUMP_MSG(P, "key: ");
                dump_expr(P, e->key);
            }
            DUMP_MSG(P, "value: ");
            dump_expr(P, e->value);
            break;
        }
        case kHirAssignExpr: {
            struct HirAssignExpr *e = HirGetAssignExpr(expr);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->rhs);
            break;
        }
        case kHirMatchArm: {
            struct HirMatchArm *e = HirGetMatchArm(expr);
            DUMP_MSG(P, "pat: ");
            dump_pat(P, e->pat);
            DUMP_MSG(P, "result: ");
            dump_expr(P, e->result);
            break;
        }
        case kHirMatchExpr: {
            struct HirMatchExpr *e = HirGetMatchExpr(expr);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->target);
            dump_expr_list(P, e->arms, "arms");
            break;
        }
        case kHirIfExpr: {
            struct HirIfExpr *e = HirGetIfExpr(expr);
            DUMP_MSG(P, "cond: ");
            dump_expr(P, e->cond);
            DUMP_MSG(P, "then_arm: ");
            dump_expr(P, e->then_arm);
            DUMP_MSG(P, "else_arm: ");
            dump_expr(P, e->else_arm);
            break;
        }
        case kHirLoopExpr: {
            struct HirLoopExpr *e = HirGetLoopExpr(expr);
            DUMP_MSG(P, "block: ");
            dump_expr(P, e->block);
            break;
        }
        case kHirReturnExpr: {
            struct HirReturnExpr *e = HirGetReturnExpr(expr);
            DUMP_MSG(P, "expr: ");
            dump_expr(P, e->expr);
            break;
        }
        case kHirJumpExpr: {
            struct HirJumpExpr *e = HirGetJumpExpr(expr);
            DUMP_FMT(P, "jump_kind: %s\n", e->jump_kind == JUMP_BREAK ? "BREAK" : "CONTINUE");
            break;
        }
        case kHirBlock: {
            struct HirBlock *e = HirGetBlock(expr);
            dump_stmt_list(P, e->stmts, "stmts");
            dump_expr(P, e->result);
            break;
        }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

void pawHir_dump_path(struct Compiler *C, struct HirPath *path)
{
    Buffer buf;
    pawL_init_buffer(ENV(C), &buf);
    struct Printer P = {
        .P = ENV(C),
        .buf = &buf,
        .C = C,
    };
    for (int i = 0; i < path->count; ++i) {
        if (i != 0) DUMP_LITERAL(&P, "::");
        dump_segment(&P, path->data[i]);
    }
    pawL_push_result(ENV(C), &buf);
}

void pawHir_dump(struct Hir *hir)
{
    Buffer buf;
    paw_Env *P = ENV(hir);
    pawL_init_buffer(P, &buf);
    struct Printer print = {
        .buf = &buf,
        .C = hir->C,
        .P = P,
    };
    for (int i = 0; i < hir->items->count; ++i) {
        dump_decl(&print, hir->items->data[i]);
    }
    pawL_push_result(P, &buf);
}

void pawHir_dump_decls(struct Compiler *C, struct HirDeclList *decls)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);
    pawL_add_fstring(P, &buf, "did\tname\ttype\n", P->modname->text);
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = pawHir_get_decl(C, (DeclId){.value = i});
        const char *name = decl->hdr.name != NULL
                ? decl->hdr.name->text : "(null)";
        // TODO: broken, maybe just remove the whole thing
        pawL_add_fstring(P, &buf, "%d\t%s\t%s\n", i, name, paw_string(P, -1));
        paw_pop(P, 1);
    }
    pawL_add_char(P, &buf, '\0');
    pawL_push_result(P, &buf);
}

#endif
