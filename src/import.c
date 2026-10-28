// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "ast.h"
#include "compile.h"
#include "error.h"
#include "lib.h"
#include "resolve.h"

#define IMPORTER_ERROR(R_, Kind_, ...) THROW_ERROR((R_)->C, \
        Kind_, __VA_ARGS__)

#define NODE_ID(Node_) ((Node_)->hdr.id)
#define GET_NODE(R_, Id_) pawAst_get_node((R_)->ast, Id_)


struct ImportBinding {
    enum ImportSymbolKind kind;
    struct AstPath path;
    struct AstIdent source;
    struct AstIdent target;
    NodeId source_id;
    NodeId target_id;
    NodeId id;
    paw_Bool in_value_ns;
    paw_Bool in_type_ns;
    paw_Bool is_pub;
};

static struct ImportSymbol const *find_import_symbol(struct Resolver *R, struct ImportScope const *scope, struct PathCursor pc, enum Namespace ns)
{
    return pawP_find_import_symbol(R, scope, pc_ident(pc).name, ns);
}

static struct AstModuleDecl *current_module(struct Resolver *R)
{
    return AstGetModuleDecl(K_LIST_LAST(R->modstack));
}

static Str const *current_modname(struct Resolver *R)
{
    return current_module(R)->ident.name;
}

static struct ImportBinding *new_binding(struct Resolver *R)
{
    return P_ALLOC(R->C, NULL, 0, sizeof(struct ImportBinding));
}

DEFINE_LIST(struct Resolver, ImportBindings, struct ImportBinding *,)

static struct ImportName *iname_new(struct Resolver *R)
{
    struct ImportName *in = P_ALLOC(R, NULL, 0, sizeof(*in));
    *in = (struct ImportName){
        .symbols = ImportSymbols_new(R),
    };
    return in;
}

static struct ImportScope *get_iscope(struct Resolver *R, NodeId id)
{
    struct ImportScope *const *pscope = ImportScopes_get(R, R->imports, id);
    return pscope != NULL ? *pscope : NULL;
}

static struct ImportScope *current_iscope(struct Resolver *R)
{
    return get_iscope(R, R->current_scope);
}

static struct ImportScope *iscope_enter(struct Resolver *R, NodeId id, enum ImportScopeKind kind, struct ImportScope *outer)
{
    struct ImportScope *is = P_ALLOC(R, NULL, 0, sizeof(*is));
    *is = (struct ImportScope){
        .types = ImportNames_new(R),
        .values = ImportNames_new(R),
        .outer = outer,
        .kind = kind,
        .id = id,
    };
    ImportScopes_insert(R, R->imports, id, is);
    R->current_scope = id;
    return is;
}

static struct ImportScope *iscope_leave(struct Resolver *R)
{
    struct ImportScope const *iscope = get_iscope(R, R->current_scope);
    paw_assert(iscope != NULL);
    if (iscope->outer != NULL) {
        R->current_scope = iscope->outer->id;
        return get_iscope(R, R->current_scope);
    } else {
        R->current_scope = INVALID_NODE_ID;
        return NULL;
    }
}

static struct ImportScope *enter_module(struct Resolver *R, NodeId id, struct ImportScope *outer)
{
    AstDeclList_push(R->ast, R->modstack, pawAst_get_node(R->ast, id));
    return iscope_enter(R, id, ISCOPE_MODULE, outer);
}

static struct ImportScope *leave_module(struct Resolver *R)
{
    return iscope_leave(R);
}

static struct ImportScope *collect_items(struct Resolver *R, struct AstDecl *toplevel, ImportBindings *bindings);

static struct AstDecl *import_module(struct Resolver *R, Str const *name, ImportBindings *bindings)
{
    int const *pmodno = ImportModules_get(R, R->modules, name);
    if (pmodno != NULL) return K_LIST_AT(R->ast->modules, *pmodno);
    DLOG(R, "importing module \"%s\"", name->text);

    paw_Env *P = ENV(R);
    struct FileReader fr;
    if (pawL_start_import(P, name, &fr) == IMPORT_FOUND) {
        ModuleInfo_push(R->C, R->C->modinfo, (struct Module){
                    .pathname = fr.pathname,
                    .dirname = fr.dirname,
                    .name = name,
                });

        struct AstDecl *mod = pawP_parse_source_file(R->C, name, fr.f, &fr);
        pawL_finish_import(P, &fr);

        struct AstModuleDecl *m = AstGetModuleDecl(mod);
        ImportModules_insert(R, R->modules, name, m->modno);
        collect_items(R, mod, bindings);
        return mod;
    }
    return NULL;
}

static struct AstDecl *get_module_by_number(struct Resolver *R, int modno)
{
    return AstDeclList_get(R->ast->modules, modno);
}

static struct AstDecl *get_module_by_name(struct Resolver *R, Str const *name)
{
    int const *pmodno = ImportModules_get(R, R->modules, name);
    if (pmodno == NULL) return NULL;
    return get_module_by_number(R, *pmodno);
}

static struct ImportSymbol const *get_explicit_symbol(struct ImportName const *iname)
{
    struct ImportSymbol const *psymbol;
    K_LIST_FOREACH (iname->symbols, psymbol) {
        if (psymbol->kind == ISYMBOL_EXPLICIT)
           return psymbol;
    }
    return NULL;
}

struct ImportSymbol const *pawP_find_import_symbol(struct Resolver *R, struct ImportScope const *scope, Str const *name, enum Namespace ns)
{
    ImportNames *inames = ns == NAMESPACE_TYPE ? scope->types : scope->values;
    struct ImportName *const *piname = ImportNames_get(R, inames, name);
    if (piname == NULL) return NULL;

    struct ImportName const *iname = *piname;
    if (iname->symbols->count == 1)
        return &K_LIST_FIRST(iname->symbols);

    paw_assert(iname->symbols->count > 1);
    struct ImportSymbol const *psymbol = get_explicit_symbol(iname);
    if (psymbol == NULL) {
        // multiple glob-imported symbols with the same name without an explicit
        // import to disambiguate
        while (scope->outer != NULL) scope = scope->outer;
        struct AstModuleDecl const *m = GET_NODE(R, scope->id);
        IMPORTER_ERROR(R, AmbiguousPath,
                .path = SCAN_STR(R->C, "TODO"),
                .modname = m->ident.name,
                .span = {0});
    }
    return psymbol;
}

static struct ImportSymbol const *find_type(struct Resolver *R, struct ImportScope const *scope, struct PathCursor pc)
{
    return find_import_symbol(R, scope, pc, NAMESPACE_TYPE);
}

static struct ImportSymbol const *lookup_in_scope(struct Resolver *R, struct ImportScope const *outer, struct PathCursor pc, enum Namespace ns);

static struct ImportSymbol const *lookup_nested_type(struct Resolver *R, struct ImportSymbol const *psymbol, struct PathCursor pc)
{
    pc_next(&pc);
    if (!pc_is_valid(pc))
        return psymbol;

    struct ImportScope const *scope = get_iscope(R, psymbol->id);
    if (scope->kind != ISCOPE_MODULE) return NULL;

    return lookup_in_scope(R, scope, pc, NAMESPACE_TYPE);
}

static struct ImportSymbol const *lookup_type_in_scope(struct Resolver *R, struct ImportScope const *outer, struct PathCursor pc)
{
    struct ImportSymbol const *result = find_import_symbol(R, outer, pc, NAMESPACE_TYPE);
    if (result != NULL) return lookup_nested_type(R, result, pc);
    return result;
}

static struct ImportSymbol const *lookup_nested_value(struct Resolver *R, struct ImportSymbol const *psymbol, struct PathCursor pc)
{
    pc_next(&pc);
    if (!pc_is_valid(pc)) return NULL;
    struct ImportScope const *scope = get_iscope(R, psymbol->id);
    if (scope != NULL && (scope->kind == ISCOPE_MODULE || scope->kind == ISCOPE_TYPE))
        return lookup_in_scope(R, scope, pc, NAMESPACE_VALUE);
    return NULL;
}

static struct ImportSymbol const *lookup_value_in_scope(struct Resolver *R, struct ImportScope const *outer, struct PathCursor pc)
{
    struct ImportSymbol const *result = find_import_symbol(R, outer, pc, NAMESPACE_TYPE);
    if (result != NULL) result = lookup_nested_value(R, result, pc);
    if (result != NULL) return result; // found associated value
    return find_import_symbol(R, outer, pc, NAMESPACE_VALUE);
}

static struct ImportSymbol const *lookup_in_scope(struct Resolver *R, struct ImportScope const *outer, struct PathCursor pc, enum Namespace ns)
{
    if (!pc_is_valid(pc)) return NULL;
    struct ImportSymbol const *result = ns == NAMESPACE_TYPE
        ? lookup_type_in_scope(R, outer, pc)
        : lookup_value_in_scope(R, outer, pc);
    if (result != NULL) return result;

    if (outer->outer == NULL) return NULL;
    return lookup_in_scope(R, outer->outer, pc, ns);
}

static paw_Bool lookup(struct Resolver *R, struct ImportScope const *current, struct ImportBinding b, enum Namespace ns, struct ImportSymbol *out)
{
    struct PathCursor pc = pc_create(b.path);
    struct ImportSymbol const *result = lookup_in_scope(R, current, pc, ns);
    if (result == NULL) {
        // try again in a different package
        struct AstSegment const first = K_LIST_FIRST(b.path.segments);
        int const *pmodno = ImportModules_get(R, R->modules, first.ident.name);
        if (pmodno == NULL) return PAW_FALSE;
        pc_next(&pc);

        struct AstDecl *mod = get_module_by_number(R, *pmodno);
        struct AstModuleDecl const *m = AstGetModuleDecl(mod);
        if (!pc_is_valid(pc)) {
            if (ns == NAMESPACE_VALUE) return PAW_FALSE;
            // found module import, e.g. "use package;"
            *out = ISYMBOL(m->id, pc_ident(pc), ISYMBOL_EXPLICIT, b.is_pub);
            return PAW_TRUE;
        }
        struct ImportScope *scope = get_iscope(R, NODE_ID(mod));
        result = lookup_in_scope(R, scope, pc, ns);
    }

    if (result != NULL) {
        *out = *result;
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool add_symbol(struct Resolver *R, struct ImportScope *scope, struct AstIdent ident, NodeId id, enum ImportSymbolKind kind, enum Namespace ns, paw_Bool is_pub)
{
    struct ImportName *iname;
    ImportNames *inames = ns == NAMESPACE_TYPE ? scope->types : scope->values;
    struct ImportName *const *pname = ImportNames_get(R, inames, ident.name);
    if (pname == NULL) {
        iname = iname_new(R);
        ImportNames_insert(R, inames, ident.name, iname);
    } else {
        iname = *pname;
    }

    if (kind == ISYMBOL_EXPLICIT && get_explicit_symbol(iname) != NULL) {
        while (scope->outer != NULL) scope = scope->outer;
        struct AstModuleDecl const *m = GET_NODE(R, scope->id);
        IMPORTER_ERROR(R, DuplicateName,
                .modname = m->ident.name,
                .what = ns == NAMESPACE_TYPE
                    ? SCAN_STR(R->C, "type")
                    : SCAN_STR(R->C, "value"),
                .name = ident.name,
                .span = ident.span);
    }

    struct ImportSymbol const *psymbol;
    K_LIST_FOREACH (iname->symbols, psymbol) {
        if (psymbol->id.value == id.value)
            return PAW_FALSE;
    }

    ImportSymbols_push(R, iname->symbols, ISYMBOL(id, ident, kind, is_pub));
    return PAW_TRUE;
}

static paw_Bool add_type(struct Resolver *R, struct ImportScope *scope, struct AstIdent ident, NodeId id, enum ImportSymbolKind kind, paw_Bool is_pub)
{
    return add_symbol(R, scope, ident, id, kind, NAMESPACE_TYPE, is_pub);
}

static paw_Bool add_value(struct Resolver *R, struct ImportScope *scope, struct AstIdent ident, NodeId id, enum ImportSymbolKind kind, paw_Bool is_pub)
{
    return add_symbol(R, scope, ident, id, kind, NAMESPACE_VALUE, is_pub);
}

struct VisitorContext {
    struct ImportScope *scope;
    ImportBindings *bindings;
    struct Resolver *R;
};

static void collect_item(struct Resolver *R, struct ImportScope *scope, struct AstDecl *decl);

static void declare_self(struct VisitorContext *X, NodeId id, enum Namespace ns, struct SourceSpan span)
{
    struct AstIdent const ident = {
        .name = SCAN_STR(X->R->C, "Self"),
        .span = span,
    };
    add_symbol(X->R, X->scope, ident, id, ISYMBOL_EXPLICIT, ns, PAW_TRUE);
}

static paw_Bool enter_fn_decl(struct AstVisitor *V, struct AstFnDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    if (NODE_ID_EXISTS(pawR_enclosing_const_ctx(ctx->R)))
        IMPORTER_ERROR(ctx->R, CannotConstantEvaluate,
                .what = SCAN_STR(ctx->R->C, "function"),
                .span = d->span);

    add_value(ctx->R, ctx->scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
    ctx->scope = iscope_enter(ctx->R, d->id, ISCOPE_FN, ctx->scope);
    return PAW_TRUE;
}

static void leave_fn_decl(struct AstVisitor *V, struct AstFnDecl *d)
{
    PAW_UNUSED(d);
    struct VisitorContext *ctx = V->ud;
    ctx->scope = iscope_leave(ctx->R);
}

static paw_Bool enter_block_expr(struct AstVisitor *V, struct AstBlock *e)
{
    struct VisitorContext *ctx = V->ud;
    ctx->scope = iscope_enter(ctx->R, e->id, ISCOPE_BLOCK, ctx->scope);
    return PAW_TRUE;
}

static void leave_block_expr(struct AstVisitor *V, struct AstBlock *e)
{
    PAW_UNUSED(e);
    struct VisitorContext *ctx = V->ud;
    ctx->scope = iscope_leave(ctx->R);
}

static paw_Bool enter_closure_expr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    struct VisitorContext *ctx = V->ud;
    if (NODE_ID_EXISTS(pawR_enclosing_const_ctx(ctx->R)))
        IMPORTER_ERROR(ctx->R, CannotConstantEvaluate,
                .what = SCAN_STR(ctx->R->C, "closure"),
                .span = e->span);
    ctx->scope = iscope_enter(ctx->R, e->id, ISCOPE_FN, ctx->scope);
    return PAW_TRUE;
}

static void leave_closure_expr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    PAW_UNUSED(e);
    struct VisitorContext *ctx = V->ud;
    ctx->scope = iscope_leave(ctx->R);
}

static paw_Bool enter_impl_decl(struct AstVisitor *V, struct AstImplDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    ctx->scope = iscope_enter(ctx->R, d->id, ISCOPE_TYPE, ctx->scope);
    declare_self(ctx, d->id, NAMESPACE_TYPE, d->span);
    return PAW_TRUE;
}

static void leave_impl_decl(struct AstVisitor *V, struct AstImplDecl *d)
{
    PAW_UNUSED(d);
    struct VisitorContext *ctx = V->ud;
    ctx->scope = iscope_leave(ctx->R);
}

static paw_Bool enter_trait_decl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    add_type(ctx->R, ctx->scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
    ctx->scope = iscope_enter(ctx->R, d->id, ISCOPE_TYPE, ctx->scope);
    return PAW_TRUE;
}

static void leave_trait_decl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    PAW_UNUSED(d);
    struct VisitorContext *ctx = V->ud;
    ctx->scope = iscope_leave(ctx->R);
}

static paw_Bool enter_module_decl(struct AstVisitor *V, struct AstModuleDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    add_type(ctx->R, ctx->scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
    ctx->scope = enter_module(ctx->R, d->id, ctx->scope);
    return PAW_TRUE;
}

static void leave_module_decl(struct AstVisitor *V, struct AstModuleDecl *d)
{
    PAW_UNUSED(d);
    struct VisitorContext *ctx = V->ud;
    ctx->scope = leave_module(ctx->R);
}

static paw_Bool is_unit_struct(struct Resolver *R, struct AstAdtDecl *d)
{
    return pawAst_is_unit_struct(d)
        && (current_module(R)->modno != PRELUDE_MODNO
                || BuiltinMap_get(R->C, R->C->builtin_lookup, d->ident.name) == NULL);
}

static paw_Bool enter_adt_decl(struct AstVisitor *V, struct AstAdtDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    struct Resolver *R = ctx->R;

    if (is_unit_struct(R, d)) {
        // add name of unit struct to enclosing value namespace
        struct AstDecl const *v = AstDeclList_first(d->variants);
        add_value(R, ctx->scope, d->ident, v->hdr.id, ISYMBOL_EXPLICIT, d->is_pub);
    }

    add_type(R, ctx->scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
    ctx->scope = iscope_enter(R, d->id, ISCOPE_TYPE, ctx->scope);
    declare_self(ctx, d->id, NAMESPACE_TYPE, d->span);
    return PAW_TRUE;
}

static void leave_adt_decl(struct AstVisitor *V, struct AstAdtDecl *d)
{
    PAW_UNUSED(d);
    struct VisitorContext *ctx = V->ud;
    ctx->scope = iscope_leave(ctx->R);
}

static paw_Bool visit_variant_decl(struct AstVisitor *V, struct AstVariantDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    add_value(ctx->R, ctx->scope, d->ident, d->id, ISYMBOL_EXPLICIT, PAW_TRUE);
    return PAW_TRUE;
}

static paw_Bool visit_type_decl(struct AstVisitor *V, struct AstTypeDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    add_type(ctx->R, ctx->scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
    return PAW_TRUE;
}

static paw_Bool enter_const_decl(struct AstVisitor *V, struct AstConstDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    add_value(ctx->R, ctx->scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
    pawR_enter_const_ctx(ctx->R, d->id);
    return PAW_TRUE;
}

static void leave_const_decl(struct AstVisitor *V, struct AstConstDecl *d)
{
    PAW_UNUSED(d);
    struct VisitorContext *ctx = V->ud;
    pawR_leave_const_ctx(ctx->R);
}

static paw_Bool visit_use_decl(struct AstVisitor *V, struct AstUseDecl *d)
{
    struct VisitorContext *ctx = V->ud;
    struct Resolver *R = ctx->R;

    struct AstIdent const source = K_LIST_LAST(d->path.segments).ident;
    struct AstIdent const target = d->use_kind == AST_USE_ALIAS ? d->as : source;

    struct ImportBinding *pb = new_binding(R);
    *pb = (struct ImportBinding){
        .kind = d->use_kind == AST_USE_GLOB
            ? ISYMBOL_GLOB : ISYMBOL_EXPLICIT,
        .target_id = current_iscope(R)->id,
        .source_id = current_module(R)->id,
        .target = target,
        .source = source,
        .path = d->path,
        .is_pub = d->is_pub,
        .id = d->id,
    };

    // save imports to resolve later
    ImportBindings_push(R, ctx->bindings, pb);
    return PAW_TRUE;
}

static struct ImportScope *collect_items(struct Resolver *R, struct AstDecl *mod, ImportBindings *bindings)
{
    struct AstModuleDecl const *m = AstGetModuleDecl(mod);
    struct VisitorContext ctx = {
        .scope = enter_module(R, m->id, NULL),
        .bindings = bindings,
        .R = R,
    };

    struct AstVisitor V;
    pawAst_visitor_init(&V, R->ast, &ctx);
    V.VisitAdtDecl = enter_adt_decl;
    V.PostVisitAdtDecl = leave_adt_decl;
    V.VisitVariantDecl = visit_variant_decl;
    V.VisitTraitDecl = enter_trait_decl;
    V.PostVisitTraitDecl = leave_trait_decl;
    V.VisitTypeDecl = visit_type_decl;
    V.VisitFnDecl = enter_fn_decl;
    V.PostVisitFnDecl = leave_fn_decl;
    V.VisitImplDecl = enter_impl_decl;
    V.PostVisitImplDecl = leave_impl_decl;
    V.VisitModuleDecl = enter_module_decl;
    V.PostVisitModuleDecl = leave_module_decl;
    V.VisitBlock = enter_block_expr;
    V.PostVisitBlock = leave_block_expr;
    V.VisitClosureExpr = enter_closure_expr;
    V.PostVisitClosureExpr = leave_closure_expr;
    V.VisitConstDecl = enter_const_decl;
    V.PostVisitConstDecl = leave_const_decl;
    V.VisitUseDecl = visit_use_decl;

    pawAst_visit_decl_list(&V, m->items);

    leave_module(R);
    return ctx.scope;
}

static paw_Bool resolve_module_prefix(struct Resolver *R, struct ImportScope *target, struct ImportBinding *pb, ImportBindings *bindings)
{
    // only the first segment can refer to a not-yet-seen package
    Str const *name = AstSegments_first(pb->path.segments).ident.name;
    struct AstDecl *mod = get_module_by_name(R, name);
    if (mod != NULL) return PAW_FALSE; // already imported
    mod = import_module(R, name, bindings);
    if (mod == NULL) return PAW_FALSE; // not a package

    pb->source_id = NODE_ID(mod);
    if (pb->path.segments->count == 1 && pb->kind == ISYMBOL_EXPLICIT) {
        add_type(R, target, pb->target, NODE_ID(mod), ISYMBOL_EXPLICIT, pb->is_pub);
        pb->in_type_ns = PAW_TRUE;
    }
    return PAW_TRUE;
}

static paw_Bool resolve_module_prefixes(struct Resolver *R, struct ImportBindings *bindings)
{
    paw_Bool changed = PAW_FALSE;

    struct ImportBinding *const *ppb;
    K_LIST_FOREACH (bindings, ppb) {
        struct ImportBinding *pb = *ppb;
        if (!pb->in_type_ns) {
            struct ImportScope *target = get_iscope(R, pb->target_id);
            changed |= resolve_module_prefix(R, target, pb, bindings);
        }
    }

    return changed;
}

static paw_Bool resolve_base_import(struct Resolver *R, struct ImportScope *target, struct ImportBinding *pb, ImportBindings *bindings)
{
    Str const *name = K_LIST_FIRST(pb->path.segments).ident.name;
    struct AstDecl *mod = get_module_by_name(R, name);
    if (mod == NULL) mod = import_module(R, name, bindings);
    if (mod == NULL) return PAW_FALSE; // not a module

    if (pb->path.segments->count == 1) {
        add_type(R, target, pb->target, NODE_ID(mod), ISYMBOL_EXPLICIT, pb->is_pub);
        pb->source_id = NODE_ID(mod);
        pb->in_type_ns = PAW_TRUE;
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool resolve_import_in(struct Resolver *R, struct ImportScope const *source, struct ImportScope *target, struct ImportBinding *pb)
{
    struct ImportSymbol symbol;
    paw_Bool changed = PAW_FALSE;
    if (!pb->in_type_ns && lookup(R, source, *pb, NAMESPACE_TYPE, &symbol)) {
        add_type(R, target, pb->target, symbol.id, ISYMBOL_EXPLICIT, pb->is_pub);
        pb->in_type_ns = PAW_TRUE;
        changed = PAW_TRUE;
    }
    if (!pb->in_value_ns && lookup(R, source, *pb, NAMESPACE_VALUE, &symbol)) {
        add_value(R, target, pb->target, symbol.id, ISYMBOL_EXPLICIT, pb->is_pub);
        pb->in_value_ns = PAW_TRUE;
        changed = PAW_TRUE;
    }
    return changed;
}

#define FOREACH_INAME(Scope_, Key_, Value_, Action_)                               \
    {                                                                              \
        ImportNamesIterator names_iter_;                                           \
        ImportNamesIterator_init(Scope_, &names_iter_);                            \
        while (ImportNamesIterator_is_valid(&names_iter_)) {                       \
            Str const *Key_ = ImportNamesIterator_key(&names_iter_);               \
            struct ImportName *Value_ = *ImportNamesIterator_valuep(&names_iter_); \
            PAW_UNUSED(Key_);                                                      \
            PAW_UNUSED(Value_);                                                    \
            Action_                                                                \
            ImportNamesIterator_next(&names_iter_);                                \
        }                                                                          \
    }

static paw_Bool glob_inames(struct Resolver *R, struct ImportScope *target, struct ImportNames *inames, enum Namespace ns, paw_Bool is_pub)
{
    paw_Bool changed = PAW_FALSE;
    FOREACH_INAME(inames, _, iname, {
        K_LIST_XFOREACH (iname->symbols, struct ImportSymbol const, is) {
            if (is->is_pub)
                changed |= add_symbol(R, target, is->ident, is->id, ISYMBOL_GLOB, ns, is_pub);
        }
    });
    return changed;
}

static paw_Bool glob_module(struct Resolver *R, struct ImportScope *target, struct ImportScope *mod, paw_Bool is_pub)
{
    paw_Bool changed = PAW_FALSE;
    changed |= glob_inames(R, target, mod->types, NAMESPACE_TYPE, is_pub);
    changed |= glob_inames(R, target, mod->values, NAMESPACE_VALUE, is_pub);
    return changed;
}

static paw_Bool glob_adt(struct Resolver *R, struct ImportScope *target, struct ImportScope *adt, paw_Bool is_pub)
{
    paw_Bool changed = PAW_FALSE;

    FOREACH_INAME(adt->values, _, iname, {
        struct ImportSymbol const *psymbol;
        K_LIST_FOREACH (iname->symbols, psymbol) {
            if (psymbol->is_pub) {
                struct AstDecl *assoc = pawAst_get_node(R->ast, psymbol->id);
                if (AstIsVariantDecl(assoc))
                    changed |= add_value(R, target, psymbol->ident, psymbol->id, ISYMBOL_GLOB, is_pub);
            }
        }
    });

    return changed;
}

static paw_Bool resolve_glob_in(struct Resolver *R, struct ImportScope const *source, struct ImportScope *target, struct ImportBinding *pb)
{
    struct ImportSymbol type;
    if (lookup(R, source, *pb, NAMESPACE_TYPE, &type)) {
        pb->in_type_ns = PAW_TRUE; // found glob target
        struct ImportScope *scope = get_iscope(R, type.id);
        switch (scope->kind) {
            case ISCOPE_MODULE:
                return glob_module(R, target, scope, pb->is_pub);
            case ISCOPE_TYPE: {
                struct AstDecl *decl = pawAst_get_node(R->ast, type.id);
                if (AstIsAdtDecl(decl)) {
                    if (!AstGetAdtDecl(decl)->is_struct)
                        return glob_adt(R, target, scope, pb->is_pub);
                }
                // fallthrough
            }
            default:
                IMPORTER_ERROR(R, InvalidGlobTarget,
                        .path = pawAst_print_type_path(R->ast, pb->path),
                        .modname = current_modname(R),
                        .span = pb->source.span);
        }
    }
    return PAW_FALSE;
}

static paw_Bool resolve_explicit_imports(struct Resolver *R, ImportBindings *bindings)
{
    paw_Bool changed = PAW_FALSE;
    for (int i = 0; i < bindings->count; ++i) {
        struct ImportBinding *pb = ImportBindings_get(bindings, i);
        if (pb->kind == ISYMBOL_GLOB) continue; // resolve in next phase
        struct ImportScope const *source = get_iscope(R, pb->source_id);
        struct ImportScope *target = get_iscope(R, pb->target_id);
        changed |= resolve_import_in(R, source, target, pb);

        if (pb->in_type_ns && pb->in_value_ns)
            // Discard the binding since there is nothing left to do. Decrement "i" so
            // the swapped-in binding is visited on the next iteration.
            ImportBindings_swap_remove(bindings, i--);
    }
    return changed;
}

static paw_Bool resolve_glob_imports(struct Resolver *R, ImportBindings *bindings)
{
    paw_Bool changed = PAW_FALSE;

    int index;
    struct ImportBinding *const *ppb;
    K_LIST_ENUMERATE (bindings, index, ppb) {
        struct ImportBinding *pb = *ppb;
        if (pb->kind == ISYMBOL_EXPLICIT) continue; // resolved in previous phase
        struct ImportScope const *source = get_iscope(R, pb->source_id);
        struct ImportScope *target = get_iscope(R, pb->target_id);
        changed |= resolve_glob_in(R, source, target, pb);
    }

    return changed;
}

static void validate_bindings(struct Resolver *R, ImportBindings *bindings)
{
    struct ImportBinding *const *ppb;
    K_LIST_FOREACH (bindings, ppb) {
        struct ImportBinding const *pb = *ppb;
        if (!pb->in_type_ns && !pb->in_value_ns) {
            struct AstIdent const ident = K_LIST_FIRST(pb->path.segments).ident;
            struct AstModuleDecl const *m = GET_NODE(R, pb->source_id);
            IMPORTER_ERROR(R, UnknownPath,
                    .path = pawAst_print_type_path(R->ast, pb->path),
                    .kind = SCAN_STR(R->C, "either (type or value)"),
                    .modname = m->ident.name,
                    .span = ident.span);
        }
    }
}

static void resolve_imports(struct Resolver *R, ImportBindings *bindings)
{
    while (resolve_explicit_imports(R, bindings)
            || resolve_glob_imports(R, bindings)
            || resolve_module_prefixes(R, bindings));

    validate_bindings(R, bindings);
}

void pawP_resolve_imports(struct Resolver *R)
{
    paw_assert(R->ast->modules->count == 1);
    struct AstDecl *main = K_LIST_FIRST(R->ast->modules);
    struct AstModuleDecl *m = AstGetModuleDecl(main);
    ImportModules_insert(R, R->modules, m->ident.name, m->modno);

    ImportBindings *bindings = ImportBindings_new(R);
    collect_items(R, main, bindings);
    resolve_imports(R, bindings);

    pawP_callback(R->C, "paw.on_build_ast", R->ast);
}

