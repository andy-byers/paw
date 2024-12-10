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

#define NEW_NODE(C, T) pawK_pool_alloc(ENV(C), (C)->pool, sizeof(T))

struct Hir *pawHir_new(struct Compiler *C, String *name, int modno)
{
    struct Hir *hir = NEW_NODE(C, struct Hir);
    *hir = (struct Hir){
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
        struct T *pawHir_new_##name(struct Compiler *C, int line, enum T##Kind kind) \
        { \
            struct T *r = NEW_NODE(C, struct T); \
            r->hdr.hid = (HirId){C->nnodes++}; \
            r->hdr.line = line; \
            r->hdr.kind = kind; \
            return r; \
        }
DEFINE_NODE_CONSTRUCTOR(expr, HirExpr)
DEFINE_NODE_CONSTRUCTOR(decl, HirDecl)
DEFINE_NODE_CONSTRUCTOR(stmt, HirStmt)
DEFINE_NODE_CONSTRUCTOR(type, HirType)
DEFINE_NODE_CONSTRUCTOR(pat, HirPat)

#define LIST_MIN_ALLOC 8

struct HirSegment *pawHir_segment_new(struct Compiler *C)
{
    return NEW_NODE(C, struct HirSegment);
}

DeclId pawHir_add_decl(struct Compiler *C, struct HirDecl *decl, int modno)
{
    paw_Env *P = ENV(C);
    struct DynamicMem *dm = C->dm;
    const DeclId did = {
        .value = dm->decls->count,
        .modno = modno,
    };
    K_LIST_PUSH(C, dm->decls, decl);
    decl->hdr.did = did;
    return did;
}

struct HirDecl *pawHir_get_decl(struct Compiler *C, DeclId did)
{
    struct DynamicMem *dm = C->dm;
    paw_assert(did.value < dm->decls->count);
    return dm->decls->data[did.value];
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

struct HirSymbol *pawHir_add_symbol(struct Compiler *C, struct HirScope *table)
{
    struct HirSymbol *symbol = pawHir_new_symbol(C);
    K_LIST_PUSH(C, table, symbol);
    return symbol;
}

int pawHir_find_symbol(struct HirScope *scope, const String *name)
{
    for (int i = scope->count - 1; i >= 0; --i) {
        const struct HirSymbol *symbol = scope->data[i];
        if (pawS_eq(name, symbol->name)) {
            if (symbol->is_init) {
                return i;
            }
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

static void AcceptBlock(struct HirVisitor *V, struct HirBlock *s)
{
    accept_stmt_list(V, s->stmts);
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
    AcceptBlock(V, e->result);
}

static void AcceptMatchStmt(struct HirVisitor *V, struct HirMatchStmt *e)
{
    AcceptExpr(V, e->target);
    accept_stmt_list(V, e->arms);
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
    if (e->has_body) {
        AcceptBlock(V, e->body);
    } else {
        AcceptExpr(V, e->expr);
    }
}

static void AcceptFieldDecl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    if (d->tag != NULL) AcceptType(V, d->tag);
}

static void AcceptTypeDecl(struct HirVisitor *V, struct HirTypeDecl *d)
{
    accept_decl_list(V, d->generics);
    AcceptExpr(V, d->rhs);
}

static void AcceptGenericDecl(struct HirVisitor *V, struct HirGenericDecl *d)
{
    PAW_UNUSED(V);
    PAW_UNUSED(d);
}

static void AcceptUseDecl(struct HirVisitor *V, struct HirUseDecl *d)
{
    AcceptPath(V, d->path);
}

static void AcceptVariantDecl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    accept_decl_list(V, d->fields);
}

static void AcceptAdtDecl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->fields);
}

static void AcceptImplDecl(struct HirVisitor *V, struct HirImplDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->methods);
}

static void AcceptVarDecl(struct HirVisitor *V, struct HirVarDecl *d)
{
    if (d->tag != NULL) AcceptType(V, d->tag);
    if (d->init != NULL) AcceptExpr(V, d->init);
}

static void AcceptReturnStmt(struct HirVisitor *V, struct HirReturnStmt *s)
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

static void AcceptSwitchDiscr(struct HirVisitor *V, struct HirSwitchDiscr *e)
{
    AcceptExpr(V, e->target);
}

static void AcceptPathExpr(struct HirVisitor *V, struct HirPathExpr *e)
{
    AcceptPath(V, e->path);
}

static void AcceptFuncDecl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    accept_decl_list(V, d->generics);
    accept_decl_list(V, d->params);
    if (d->body != NULL) AcceptBlock(V, d->body);
}

static void AcceptIfStmt(struct HirVisitor *V, struct HirIfStmt *s)
{
    AcceptExpr(V, s->cond);
    AcceptStmt(V, s->then_arm);
    if (s->else_arm != NULL) AcceptStmt(V, s->else_arm);
}

static void AcceptWhileStmt(struct HirVisitor *V, struct HirWhileStmt *s)
{
    AcceptExpr(V, s->cond);
    AcceptBlock(V, s->block);
}

static void AcceptJumpStmt(struct HirVisitor *V, struct HirJumpStmt *s)
{
    PAW_UNUSED(V);
    PAW_UNUSED(s);
}

static void AcceptForStmt(struct HirVisitor *V, struct HirForStmt *s)
{
    if (s->is_fornum) {
        AcceptExpr(V, s->fornum.begin);
        AcceptExpr(V, s->fornum.end);
        AcceptExpr(V, s->fornum.step);
    } else {
        AcceptExpr(V, s->forin.target);
    }
    AcceptBlock(V, s->block);
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
#define DEFINE_VISITOR_CASES(a, b) case kHir##a: { \
        struct Hir##a *x = HirGet##a(node); \
        if (VISITOR_CALL(V, a, x)) Accept##a(V, x); \
        VISITOR_POSTCALL(V, a, x); \
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


struct IrType *pawP_attach_type(struct Compiler *C, DeclId did, enum IrTypeKind kind, int line)
{
    struct DynamicMem *dm = C->dm;
    struct IrType *type = pawIr_new_type(C, kind);
    if (did.value != NO_DECL.value) {
        struct HirDecl *decl = pawHir_get_decl(C, did);
        SET_NODE_TYPE(C, decl, type);
    }
    // TODO: what about path types? seems strange to require the caller to set when kind is kHirPathType only
    if (kind == kIrSignature) {
        IrGetSignature(type)->did = did;
    } else if (kind == kIrAdt) {
        IrGetAdt(type)->did = did;
    } else if (kind == kIrGeneric) {
        IrGetGeneric(type)->did = did;
    }
    return type;
}

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
        case kHirFuncPtr: {
            struct HirFuncPtr *fptr = HirGetFuncPtr(type);
            PRINT_LITERAL(P, "fn(");
            print_type_list(P, fptr->params);
            PRINT_CHAR(P, ')');
            if (!HirIsPathType(fptr->result)
                    || HIR_PATH_RESULT(fptr->result->path.path).value != PAW_TUNIT) {
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

static const char *get_typename(struct DynamicMem *dm, DeclId did)
{
    const struct HirDecl *decl = dm->decls->data[did.value];
    return decl->hdr.name ? decl->hdr.name->text : "(null)";
}

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

#define DUMP_BLOCK(P, b) CHECK_EXP(!(b) || HirIsBlock(HIR_CAST_STMT(b)), (b) ? dump_stmt(P, HIR_CAST_STMT(b)) : DUMP_LITERAL(P, "{}\n"))
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

static void dump_path(struct Printer *P, struct HirPath *p)
{
    for (int i = 0; i < p->count; ++i) {
        if (i != 0) DUMP_LITERAL(P, "::");
        dump_segment(P, p->data[i]);
    }
}

static void dump_type(struct Printer *P, struct HirType *t)
{
    if (print_type_kind(P, t)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", t->hdr.line);
    switch (HIR_KINDOF(t)) {
        case kHirTupleType:
            dump_type_list(P, t->tuple.elems, "elems");
            break;
        case kHirFuncPtr:
            dump_type_list(P, t->fptr.params, "params");
            DUMP_MSG(P, "result: ");
            dump_type(P, t->fptr.result);
            break;
        case kHirPathType:
            for (int i = 0; i < t->path.path->count; ++i) {
                struct HirSegment seg = K_LIST_GET(t->path.path, i);
                DUMP_FMT(P, "name: %s\n", seg.name->text);
                DUMP_FMT(P, "did: %d\n", seg.did);
                if (seg.types != NULL) {
                    dump_type_list(P, seg.types, "types");
                }
            }
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_decl(struct Printer *P, struct HirDecl *d)
{
    if (print_decl_kind(P, d)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "did: %d\n", d->hdr.did);
    DUMP_FMT(P, "line: %d\n", d->hdr.line);
    switch (HIR_KINDOF(d)) {
        case kHirFuncDecl:
            DUMP_FMT(P, "self: %p\n", (void *)d->func.self);
            DUMP_FMT(P, "name: %s\n", d->func.name->text);
            dump_decl_list(P, d->func.generics, "generics");
            dump_decl_list(P, d->func.params, "params");
            DUMP_MSG(P, "body: ");
            DUMP_BLOCK(P, d->func.body);
            break;
        case kHirUseDecl:
            DUMP_NAME(P, d->use.name);
            break;
        case kHirFieldDecl:
            DUMP_NAME(P, d->field.name);
            break;
        case kHirVarDecl:
            DUMP_NAME(P, d->var.name);
            DUMP_MSG(P, "init: ");
            dump_expr(P, d->var.init);
            break;
        case kHirVariantDecl:
            DUMP_NAME(P, d->variant.name);
            dump_decl_list(P, d->variant.fields, "fields");
            break;
        case kHirAdtDecl:
            DUMP_NAME(P, d->adt.name);
            DUMP_FMT(P, "is_struct: %d\n", d->adt.is_struct);
            dump_decl_list(P, d->adt.generics, "generics");
            dump_decl_list(P, d->adt.fields, "fields");
            break;
        case kHirImplDecl:
            DUMP_NAME(P, d->impl.name);
            dump_decl_list(P, d->impl.generics, "generics");
            dump_decl_list(P, d->impl.methods, "methods");
            break;
        case kHirGenericDecl:
            DUMP_NAME(P, d->generic.name);
            break;
        case kHirTypeDecl:
            DUMP_NAME(P, d->type.name);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, d->type.rhs);
            dump_decl_list(P, d->type.generics, "generics");
            break;
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_stmt(struct Printer *P, struct HirStmt *s)
{
    if (print_stmt_kind(P, s)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", s->hdr.line);
    switch (HIR_KINDOF(s)) {
        case kHirMatchArm:
            DUMP_MSG(P, "pat: ");
            dump_pat(P, s->arm.pat);
            DUMP_MSG(P, "result: ");
            DUMP_BLOCK(P, s->arm.result);
            break;
        case kHirMatchStmt:
            DUMP_MSG(P, "target: ");
            dump_expr(P, s->match.target);
            dump_stmt_list(P, s->match.arms, "arms");
            break;
        case kHirExprStmt:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->expr.expr);
            break;
        case kHirBlock:
            dump_stmt_list(P, s->block.stmts, "stmts");
            break;
        case kHirDeclStmt:
            DUMP_MSG(P, "decl: ");
            dump_decl(P, s->decl.decl);
            break;
        case kHirIfStmt:
            DUMP_MSG(P, "cond: ");
            dump_expr(P, s->if_.cond);
            DUMP_MSG(P, "then_arm: ");
            dump_stmt(P, s->if_.then_arm);
            DUMP_MSG(P, "else_arm: ");
            dump_stmt(P, s->if_.else_arm);
            break;
        case kHirForStmt:
            if (s->for_.is_fornum) {
                DUMP_MSG(P, "begin: ");
                dump_expr(P, s->for_.fornum.begin);
                DUMP_MSG(P, "end: ");
                dump_expr(P, s->for_.fornum.end);
                DUMP_MSG(P, "step: ");
                dump_expr(P, s->for_.fornum.step);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, s->for_.block);
            } else {
                DUMP_MSG(P, "target: ");
                dump_expr(P, s->for_.forin.target);
                DUMP_MSG(P, "block: ");
                DUMP_BLOCK(P, s->for_.block);
            }
            break;
        case kHirWhileStmt:
            DUMP_FMT(P, "is_dowhile: %d\n", s->while_.is_dowhile);
            DUMP_MSG(P, "cond: ");
            dump_expr(P, s->while_.cond);
            DUMP_MSG(P, "block: ");
            DUMP_BLOCK(P, s->while_.block);
            break;
        case kHirReturnStmt:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, s->result.expr);
            break;
        case kHirJumpStmt:
            DUMP_FMT(P, "jump_kind: %s\n", s->jump.jump_kind == JUMP_BREAK ? "BREAK" : "CONTINUE");
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_pat(struct Printer *P, struct HirPat *p)
{
    if (print_pat_kind(P, p)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", p->hdr.line);
    switch (HIR_KINDOF(p)) {
        case kHirBindingPat:
            DUMP_MSG(P, "name: ");
            PRINT_STRING(P, p->bind.name);
            break;
        case kHirOrPat:
            dump_pat_list(P, p->or.pats, "pats");
            break;
        case kHirFieldPat:
            DUMP_MSG(P, "target: ");
            PRINT_STRING(P, p->field.name);
            DUMP_MSG(P, "pat: ");
            dump_pat(P, p->field.pat);
            break;
        case kHirStructPat:
            DUMP_MSG(P, "path: ");
            dump_path(P, p->struct_.path);
            dump_pat_list(P, p->struct_.fields, "fields");
            break;
        case kHirVariantPat:
            DUMP_FMT(P, "index: %d", p->variant.index);
            DUMP_MSG(P, "path: ");
            dump_path(P, p->variant.path);
            dump_pat_list(P, p->variant.fields, "fields");
            break;
        case kHirTuplePat:
            dump_pat_list(P, p->variant.fields, "elems");
            break;
        case kHirPathPat:
            DUMP_MSG(P, "path: ");
            dump_path(P, p->path.path);
            break;
        case kHirWildcardPat:
            break;
        case kHirLiteralPat:
            DUMP_MSG(P, "expr: ");
            dump_expr(P, p->lit.expr);
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

static void dump_expr(struct Printer *P, struct HirExpr *e)
{
    if (print_expr_kind(P, e)) {
        return;
    }
    ++P->indent;
    DUMP_FMT(P, "line: %d\n", e->hdr.line);
    switch (HIR_KINDOF(e)) {
        case kHirLiteralExpr:
            switch (e->literal.lit_kind) {
                case kHirLitBasic:
                    DUMP_MSG(P, "lit_kind: BASIC\n");
                    switch (e->literal.basic.t) {
                        case PAW_TUNIT:
                            break;
                        case PAW_TBOOL:
                            DUMP_FMT(P, "value: %s\n",
                                     V_TRUE(e->literal.basic.value) ? "true"
                                                                    : "false");
                            break;
                        case PAW_TINT:
                            DUMP_FMT(P, "value: %I\n",
                                     V_INT(e->literal.basic.value));
                            break;
                        case PAW_TFLOAT:
                            DUMP_FMT(P, "value: %f\n",
                                     V_FLOAT(e->literal.basic.value));
                            break;
                        default:
                            paw_assert(e->literal.basic.t == PAW_TSTR);
                            DUMP_FMT(P, "value: %s\n",
                                     V_STRING(e->literal.basic.value)->text);
                            break;
                    }
                    break;
                case kHirLitTuple:
                    DUMP_MSG(P, "lit_kind: TUPLE\n");
                    dump_expr_list(P, e->literal.tuple.elems, "elems");
                    break;
                case kHirLitContainer:
                    DUMP_MSG(P, "lit_kind: CONTAINER\n");
                    dump_expr_list(P, e->literal.cont.items, "items");
                    break;
                case kHirLitComposite:
                    DUMP_MSG(P, "lit_kind: COMPOSITE\n");
                    DUMP_MSG(P, "target: ");
                    dump_path(P, e->literal.comp.path);
                    dump_expr_list(P, e->literal.comp.items, "items");
            }
            break;
        case kHirChainExpr:
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->chain.target);
            break;
        case kHirLogicalExpr:
            DUMP_FMT(P, "is_and: %d\n", e->logical.is_and);
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->logical.lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->logical.rhs);
            break;
        case kHirClosureExpr:
            dump_decl_list(P, e->clos.params, "params");
            if (e->clos.has_body) {
                DUMP_BLOCK(P, e->clos.body);
            } else {
                dump_expr(P, e->clos.expr);
            }
            break;
        case kHirPathExpr:
            DUMP_MSG(P, "path: ");
            dump_path(P, e->path.path);
            break;
        case kHirSwitchDiscr:
            DUMP_FMT(P, "expect: %d", e->iswitch.expect);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->iswitch.target);
            break;
        case kHirConversionExpr:
            DUMP_FMT(P, "to: %d\n", e->conv.to);
            DUMP_MSG(P, "arg: ");
            dump_expr(P, e->conv.arg);
            break;
        case kHirUnOpExpr:
            DUMP_FMT(P, "op: %s\n", paw_unop_name(e->unop.op));
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->unop.target);
            break;
        case kHirBinOpExpr:
            DUMP_FMT(P, "op: %s\n", paw_binop_name(e->binop.op));
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->binop.lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->binop.rhs);
            break;
        case kHirCallExpr:
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->call.target);
            dump_expr_list(P, e->call.args, "args");
            break;
        case kHirIndex:
            DUMP_FMT(P, "is_slice: %d\n", e->index.is_slice);
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->index.target);
            DUMP_MSG(P, "first: ");
            dump_expr(P, e->index.first);
            DUMP_MSG(P, "second: ");
            dump_expr(P, e->index.second);
            break;
        case kHirSelector:
            DUMP_MSG(P, "target: ");
            dump_expr(P, e->select.target);
            if (e->select.is_index) {
                DUMP_FMT(P, "index: %I\n", e->select.index);
            } else {
                DUMP_NAME(P, e->select.name);
            }
            break;
        case kHirFieldExpr:
            if (e->field.fid >= 0) {
                DUMP_NAME(P, e->field.name);
            } else {
                DUMP_MSG(P, "key: ");
                dump_expr(P, e->field.key);
            }
            DUMP_MSG(P, "value: ");
            dump_expr(P, e->field.value);
            break;
        case kHirAssignExpr:
            DUMP_MSG(P, "lhs: ");
            dump_expr(P, e->assign.lhs);
            DUMP_MSG(P, "rhs: ");
            dump_expr(P, e->assign.rhs);
    }
    --P->indent;
    DUMP_MSG(P, "}\n");
}

void pawHir_dump_path(struct Compiler *C, struct HirPath *path)
{
    Buffer buf;
    pawL_init_buffer(ENV(C), &buf);
    dump_path(&(struct Printer){
                .P = ENV(C),
                .buf = &buf,
                .C = C,
            }, path);
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
        if (HirIsUseDecl(decl)) {
            paw_push_string(P, "(use)");
        } else {
// TODO            pawHir_print_type(C, decl->hdr.type);
        }
        pawL_add_fstring(P, &buf, "%d\t%s\t%s\n", i, name, paw_string(P, -1));
        paw_pop(P, 1);
    }
    pawL_add_char(P, &buf, '\0');
    pawL_push_result(P, &buf);
}

#endif
