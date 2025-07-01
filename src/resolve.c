// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "resolve.h"

#define RESOLVER_ERROR(R_, Kind_, Modname_, ...) pawErr_##Kind_((R_)->C, Modname_, __VA_ARGS__)


//
// Symbol table
//

enum SymbolKind {
    SYMBOL_VAR,
    SYMBOL_DECL,
};

struct Symbol {
    enum Namespace ns;
    enum SymbolKind kind;
    struct AstIdent ident;
    NodeId id;
};

DEFINE_LIST(struct Resolver, SymbolList, struct Symbol)
DEFINE_LIST(struct Resolver, NodeIdList, NodeId);

DEFINE_LIST(struct Resolver, Scope, struct Symbol)
DEFINE_LIST(struct Resolver, Symtab, struct Scope *)


static DeclId next_did(struct Resolver *R)
{
    return (DeclId){
        .value = ++R->decl_count,
        .modno = R->current->modno,
    };
}

static paw_Bool is_unary_path(struct AstPath path)
{
    return path.segments->count == 1;
}

static void set_result(struct Resolver *R, struct AstSegment segment, NodeId result, enum ResolvedKind kind)
{
    struct ResolvedSegment const resolved = {.id = result, .kind = kind};
    SegmentTable_insert(R->C, R->segtab, segment.id, resolved);
}

_Noreturn static void unknown_path(struct Resolver *R, struct AstPath path)
{
    char const *repr = pawAst_print_path(R->ast, path);
    RESOLVER_ERROR(R, unknown_path, R->current->name, path.span.start, repr);
}

static void maybe_store_builtin(struct Resolver *R, NodeId module_id, struct AstIdent ident, NodeId id, DeclId did)
{
    struct AstDecl *mod = pawAst_get_node(R->ast, module_id);
    struct AstModuleDecl *m = AstGetModuleDecl(mod);
    if (m->modno == PRELUDE_MODNO) {
        struct Builtin *const *pb = BuiltinMap_get(R->C, R->C->builtin_lookup, ident.name);
        if (pb != NULL) {
            (*pb)->did = did;
            (*pb)->id = id;
        }
    }
}

static Str const *current_modname(struct Resolver *R)
{
    struct AstModuleDecl const *m = pawAst_get_node(R->ast, R->current->id);
    return m->name;
}

static char const *decl_kind(enum AstDeclKind kind)
{
    switch (kind) {
        case kAstModuleDecl:
            return "module";
        case kAstFieldDecl:
            return "field";
        case kAstParamDecl:
            return "parameter";
        case kAstFuncDecl:
            return "function";
        case kAstGenericDecl:
            return "generic";
        case kAstAdtDecl:
            return "ADT";
        case kAstTypeDecl:
            return "type";
        case kAstConstDecl:
            return "constant";
        case kAstTraitDecl:
            return "trait";
        case kAstUseDecl:
            return "import";
        case kAstVariantDecl:
            return "variant";
    }
}

// TODO: should check validate all type args in this file, not just those on trait bounds
static void validate_type_args(struct Resolver *R, enum AstDeclKind kind, struct AstDeclList *generics, struct AstSegment segment)
{
    if (generics != NULL && segment.types == NULL)
        RESOLVER_ERROR(R, expected_type_arguments, current_modname(R),
                segment.ident.span.start, decl_kind(kind), "");

    if (generics == NULL && segment.types != NULL)
        RESOLVER_ERROR(R, unexpected_type_arguments, current_modname(R),
                segment.ident.span.start, decl_kind(kind), "");

    if (generics != NULL && generics->count != segment.types->count)
        RESOLVER_ERROR(R, incorrect_type_arity, current_modname(R),
                segment.ident.span.start, generics->count, segment.types->count);
}

#define EMIT_SYMBOL(Symbol_, Out_) (*(Out_) = (Symbol_), PAW_TRUE)

struct ImportSymbol const *pawP_find_import_symbol(struct Resolver *R, struct ImportScope const *scope, struct PathCursor pc, enum Namespace ns);

static struct ImportScope const *get_scope(struct Resolver *R, NodeId id)
{
    return *ImportScopes_get(R, R->imports, id);
}

static struct ImportScope const *find_containing_module(struct Resolver *R, struct PathCursor *pc)
{
    paw_assert(pc_is_valid(*pc));
    struct ImportScope const *scope;
    NodeId module_id = R->current->id;
    do {
        scope = get_scope(R, module_id);
        struct ImportSymbol const *psymbol = pawP_find_import_symbol(R, scope, *pc, NAMESPACE_TYPE);
        if (psymbol == NULL) break;

        struct AstDecl *decl = pawAst_get_node(R->ast, psymbol->id);
        if (AstIsModuleDecl(decl)) {
            struct AstSegment const segment = *pc_segment(*pc);
            set_result(R, segment, psymbol->id, RESOLVED_MODULE);
            module_id = psymbol->id;
            pc_next(pc);
        } else {
            break;
        }
    } while (pc_is_valid(*pc));
    return scope;
}

static paw_Bool find_local(struct Resolver *R, struct PathCursor pc, enum Namespace ns, struct Symbol *out)
{
    paw_assert(R->symtab != NULL);
    struct AstSegment const segment = *pc_segment(pc);
    for (int i = R->symtab->count - 1; i >= 0; --i) {
        struct Scope *scope = K_LIST_AT(R->symtab, i);
        for (int j = scope->count - 1; j >= 0; --j) {
            struct Symbol const symbol = K_LIST_AT(scope, j);
            if (symbol.ns == ns // only search given namespace
                    && pawS_eq(segment.ident.name, symbol.ident.name)) {
                *out = symbol;
                return PAW_TRUE;
            }
        }
    }
    return PAW_FALSE;
}

static paw_Bool find_global(struct Resolver *R, struct ImportScope const *scope, struct PathCursor pc, enum Namespace ns, struct Symbol *out)
{
    struct AstSegment const segment = *pc_segment(pc);
    struct ImportSymbol const *psymbol = pawP_find_import_symbol(R, scope, pc, ns);
    if (psymbol == NULL) return PAW_FALSE;

    *out = (struct Symbol){
        .id = psymbol->id,
        .kind = SYMBOL_DECL,
        .ident = segment.ident,
        .ns = NAMESPACE_VALUE,
    };
    return PAW_TRUE;
}

static paw_Bool find_containing_type(struct Resolver *R, struct PathCursor *pc, struct Symbol *symbol_out, struct ImportScope const **scope_out)
{
    *scope_out = get_scope(R, R->current->id);

    struct Symbol symbol;
    if (find_local(R, *pc, NAMESPACE_TYPE, &symbol)) {
        struct AstSegment const segment = *pc_segment(*pc);
        set_result(R, segment, symbol.id,
                symbol.kind == SYMBOL_DECL ? RESOLVED_DECL : RESOLVED_LOCAL);
    } else {
        *scope_out = find_containing_module(R, pc);
        if (!find_global(R, *scope_out, *pc, NAMESPACE_TYPE, &symbol))
            return PAW_FALSE;
        struct AstSegment const segment = *pc_segment(*pc);
        set_result(R, segment, symbol.id, RESOLVED_DECL);
    }
    *symbol_out = symbol;
    return PAW_TRUE;
}

static paw_Bool lookup_type(struct Resolver *R, struct PathCursor pc, struct Symbol *out)
{
    struct Symbol symbol;
    struct ImportScope const *scope;
    if (!find_containing_type(R, &pc, &symbol, &scope))
        return PAW_FALSE;

    if (!pc_is_last(pc)) {
        struct AstSegment const segment = *pc_segment(pc);
        RESOLVER_ERROR(R, extra_segment, R->current->name,
                segment.ident.span.start, segment.ident.name->text);
    }

    set_result(R, *pc_segment(pc), symbol.id, RESOLVED_DECL);
    *out = symbol;
    return PAW_TRUE;
}

static paw_Bool find_value_in_scope(struct Resolver *R, NodeId scope_id, struct PathCursor pc, struct Symbol *out)
{
    struct ImportScope const *scope = get_scope(R, scope_id);
    struct ImportSymbol const *psymbol = pawP_find_import_symbol(R, scope, pc, NAMESPACE_VALUE);
    if (psymbol == NULL) return PAW_FALSE;

    *out = (struct Symbol){
        .ident = pc_segment(pc)->ident,
        .ns = NAMESPACE_VALUE,
        .kind = SYMBOL_DECL,
        .id = psymbol->id,
    };
    return PAW_TRUE;
}

static struct ResolvedSegment get_path_result(struct Resolver *R, struct AstPath path)
{
    struct AstSegment last = K_LIST_LAST(path.segments);
    return *SegmentTable_get(R->C, R->segtab, last.id);
}

static paw_Bool lookup(struct Resolver *R, struct AstPath path, enum Namespace ns, struct Symbol *out);

static paw_Bool find_value_in_generic(struct Resolver *R, struct AstGenericDecl *d, struct PathCursor pc, struct Symbol *out)
{
    struct AstGenericBound const *pbound;
    K_LIST_FOREACH (d->bounds, pbound) {
        struct ResolvedSegment res = get_path_result(R, pbound->path);
        if (find_value_in_scope(R, res.id, pc, out))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool find_value_in_type(struct Resolver *R, struct AstTypeDecl *d, struct PathCursor pc, struct Symbol *out)
{
    struct Symbol base;
    struct AstPathType *rhs = AstGetPathType(d->rhs);
    if (!lookup(R, rhs->path, NAMESPACE_TYPE, &base)) {
        char const *repr = pawAst_print_path(R->ast, rhs->path);
        RESOLVER_ERROR(R, unknown_path, R->current->name, rhs->span.start, repr);
    }
    return find_value_in_scope(R, base.id, pc, out);
}

static paw_Bool find_assoc_item(struct Resolver *R, NodeId type_id, struct PathCursor *pc, struct Symbol *out)
{
    struct AstDecl *type = pawAst_get_node(R->ast, type_id);
    switch (AST_KINDOF(type)) {
        case kAstTypeDecl:
            return find_value_in_type(R, AstGetTypeDecl(type), *pc, out);
        case kAstGenericDecl:
            return find_value_in_generic(R, AstGetGenericDecl(type), *pc, out);
        case kAstAdtDecl:
        case kAstTraitDecl:
            return find_value_in_scope(R, type_id, *pc, out);
        default: {
            char const *repr = pawAst_print_path(R->ast, pc->path);
            RESOLVER_ERROR(R, unknown_path, R->current->name,
                    pc_segment(*pc)->ident.span.start, repr);
        }
    }
}

static paw_Bool lookup_value(struct Resolver *R, struct PathCursor pc, struct Symbol *out)
{
    struct Symbol symbol;
    if (!pc_is_last(pc)) {
        struct ImportScope const *outer;
        if (find_containing_type(R, &pc, &symbol, &outer)) {
            set_result(R, *pc_segment(pc), symbol.id, RESOLVED_DECL);
            pc_next(&pc); // find associated item in type referenced by "symbol"
            if (!find_assoc_item(R, symbol.id, &pc, &symbol)) return PAW_FALSE;
            set_result(R, *pc_segment(pc), symbol.id, RESOLVED_ASSOC);

        } else { // must be a value at the toplevel of an imported module
            if (!find_global(R, outer, pc, NAMESPACE_VALUE, &symbol))
                return PAW_FALSE;
            set_result(R, *pc_segment(pc), symbol.id, RESOLVED_DECL);
        }

        if (!pc_is_last(pc)) {
            struct AstSegment const segment = *pc_segment(pc);
            RESOLVER_ERROR(R, extra_segment, R->current->name,
                    segment.ident.span.start, segment.ident.name->text);
        }

    } else if (find_local(R, pc, NAMESPACE_VALUE, &symbol)) {
        set_result(R, *pc_segment(pc), symbol.id,
                symbol.kind == SYMBOL_DECL ? RESOLVED_DECL : RESOLVED_LOCAL);
    } else {
        struct ImportScope const *scope = get_scope(R, R->current->id);
        if (!find_global(R, scope, pc, NAMESPACE_VALUE, &symbol))
            return PAW_FALSE;
        set_result(R, *pc_segment(pc), symbol.id, RESOLVED_DECL);
    }

    *out = symbol;
    return PAW_TRUE;
}

static paw_Bool lookup(struct Resolver *R, struct AstPath path, enum Namespace ns, struct Symbol *out)
{
    struct PathCursor pc = pc_create(path);
    return ns == NAMESPACE_VALUE
        ? lookup_value(R, pc, out)
        : lookup_type(R, pc, out);
}

static struct Symbol lookup_or_error(struct Resolver *R, struct AstPath path, enum Namespace ns)
{
    struct Symbol symbol;
    if (!lookup(R, path, ns, &symbol))
        unknown_path(R, path);
    return symbol;
}

static int add_local(struct Resolver *R, struct Scope *scope, struct AstIdent ident, NodeId id, enum Namespace ns, enum SymbolKind kind)
{
    struct Symbol const symbol = {
        .ident = ident,
        .kind = kind,
        .ns = ns,
        .id = id,
    };
    Scope_push(R, scope, symbol);
    return scope->count - 1;
}

static struct Scope *enclosing_scope(struct Resolver *R)
{
    paw_assert(R->symtab->count > 0);
    return K_LIST_LAST(R->symtab);
}

static void leave_scope(struct Resolver *R)
{
    Scope_delete(R, enclosing_scope(R));
    Symtab_pop(R->symtab);
}

static void enter_scope(struct Resolver *R)
{
    struct Scope *scope = Scope_new(R);
    Symtab_push(R, R->symtab, scope);
}

static int new_local_type(struct Resolver *R, struct AstIdent ident, NodeId id, enum SymbolKind kind)
{
    return add_local(R, enclosing_scope(R), ident, id, NAMESPACE_TYPE, kind);
}

static int new_local_value(struct Resolver *R, struct AstIdent ident, NodeId id, enum SymbolKind kind)
{
    return add_local(R, enclosing_scope(R), ident, id, NAMESPACE_VALUE, kind);
}

static void declare_type_aliases(struct Resolver *R, NodeId parent_id)
{
    struct ImportScope const *scope = *ImportScopes_get(R, R->imports, parent_id);

    ImportNamesIterator iter;
    ImportNamesIterator_init(scope->types, &iter);
    while (ImportNamesIterator_is_valid(&iter)) {
        struct ImportName const *iname = *ImportNamesIterator_valuep(&iter);
        paw_assert(iname->symbols->count == 1);
        struct ImportSymbol symbol = K_LIST_FIRST(iname->symbols);
        struct AstIdent TODO = {.name = (Str *)ImportNamesIterator_key(&iter)};
        new_local_type(R, TODO, symbol.id, SYMBOL_DECL);
        ImportNamesIterator_next(&iter);
    }
}

// Create a local type symbol for each generic type parameter
// This must happen before generics are resolved, since generic bounds may refer to
// any other generics in the same binder.
static void declare_generics(struct Resolver *R, struct AstDeclList *generics)
{
    if (generics == NULL) return;
    struct AstDecl *const *pdecl;
    K_LIST_FOREACH (generics, pdecl) {
        struct AstGenericDecl *d = AstGetGenericDecl(*pdecl);
        new_local_type(R, d->ident, d->id, SYMBOL_DECL);
        d->did = next_did(R);
    }
}

static paw_Bool resolve_literal_expr(struct AstVisitor *V, struct AstLiteralExpr *e)
{
    struct Resolver *R = V->ud;
    if (e->lit_kind == kAstCompositeLit)
        lookup_or_error(R, e->comp.path, NAMESPACE_TYPE);

    return PAW_TRUE;
}

static void resolve_type_args(struct AstVisitor *V, struct AstPath path)
{
    struct AstSegment const *psegment;
    K_LIST_FOREACH (path.segments, psegment) {
        if (psegment->types != NULL)
            pawAst_visit_type_list(V, psegment->types);
    }
}

static paw_Bool resolve_path_expr(struct AstVisitor *V, struct AstPathExpr *e)
{
    lookup_or_error(V->ud, e->path, NAMESPACE_VALUE);
    resolve_type_args(V, e->path);
    return PAW_FALSE;
}

static paw_Bool resolve_path_type(struct AstVisitor *V, struct AstPathType *t)
{
    lookup_or_error(V->ud, t->path, NAMESPACE_TYPE);
    resolve_type_args(V, t->path);
    return PAW_FALSE;
}

static paw_Bool resolve_path_pat(struct AstVisitor *V, struct AstPathPat *p)
{
    struct Resolver *R = V->ud;

    struct Symbol symbol;
    struct AstDecl *decl = NULL;
    if (lookup(R, p->path, NAMESPACE_VALUE, &symbol)
            && symbol.kind == SYMBOL_DECL) {
        decl = pawAst_get_node(R->ast, symbol.id);
        if (AstIsParamDecl(decl)) decl = NULL; // TODO: hack, should be SYMBOL_PARAM or something...
    }

    if ((decl == NULL || AstIsFuncDecl(decl)) && is_unary_path(p->path)) {
        // create a pattern binding
        struct AstSegment const segment = K_LIST_LAST(p->path.segments);
        new_local_value(R, segment.ident, p->id, SYMBOL_VAR);
        set_result(R, segment, p->id, RESOLVED_LOCAL);
    } else if (decl == NULL) {
        unknown_path(R, p->path);
    }
    resolve_type_args(V, p->path);
    return PAW_FALSE;
}

static paw_Bool resolve_struct_pat(struct AstVisitor *V, struct AstStructPat *p)
{
    lookup_or_error(V->ud, p->path, NAMESPACE_TYPE);
    return PAW_TRUE;
}

static paw_Bool resolve_variant_pat(struct AstVisitor *V, struct AstVariantPat *p)
{
    lookup_or_error(V->ud, p->path, NAMESPACE_VALUE);
    return PAW_TRUE;
}

static paw_Bool resolve_variant_decl(struct AstVisitor *V, struct AstVariantDecl *d)
{
    d->did = next_did(V->ud);
    return PAW_TRUE;
}

static paw_Bool resolve_const_decl(struct AstVisitor *V, struct AstConstDecl *d)
{
    d->did = next_did(V->ud);
    return PAW_TRUE;
}

static paw_Bool resolve_field_decl(struct AstVisitor *V, struct AstFieldDecl *d)
{
    d->did = next_did(V->ud);
    return PAW_TRUE;
}

static paw_Bool resolve_param_decl(struct AstVisitor *V, struct AstParamDecl *d)
{
    struct Resolver *R = V->ud;
    d->did = next_did(R);

    new_local_value(R, d->ident, d->id, SYMBOL_DECL);
    return PAW_TRUE;
}


static paw_Bool resolve_generic_decl(struct AstVisitor *V, struct AstGenericDecl *d)
{
    struct Resolver *R = V->ud;
    if (d->bounds != NULL) {
        struct AstGenericBound const *pbound;
        K_LIST_FOREACH (d->bounds, pbound) {
            struct Symbol symbol = lookup_or_error(R, pbound->path, NAMESPACE_TYPE);
            resolve_type_args(V, pbound->path);

            struct AstDecl *decl = pawAst_get_node(R->ast, symbol.id);
            if (!AstIsTraitDecl(decl)) {
                char const *repr = pawAst_print_path(R->ast, pbound->path);
                RESOLVER_ERROR(R, expected_trait, current_modname(R),
                        pbound->path.span.start, repr);
            }

            struct AstTraitDecl *trait = AstGetTraitDecl(decl);
            struct AstSegment last = K_LIST_LAST(pbound->path.segments);
            validate_type_args(R, kAstTraitDecl, trait->generics, last);
        }
    }
    return PAW_FALSE;
}

static paw_Bool enter_block_expr(struct AstVisitor *V, struct AstBlock *e)
{
    enter_scope(V->ud);
    return PAW_TRUE;
}

static void leave_block_expr(struct AstVisitor *V, struct AstBlock *e)
{
    leave_scope(V->ud);
}

static paw_Bool enter_fn_decl(struct AstVisitor *V, struct AstFuncDecl *d)
{
    struct Resolver *R = V->ud;
    d->did = next_did(R);
    enter_scope(R);

    declare_generics(R, d->generics);
    declare_type_aliases(R, d->id);
    return PAW_TRUE;
}

static void leave_fn_decl(struct AstVisitor *V, struct AstFuncDecl *d)
{
    leave_scope(V->ud);
}

static void declare_self(struct Resolver *R, struct SourceSpan span, NodeId id, enum Namespace ns)
{
    struct AstIdent const ident = {
        .name = SCAN_STR(R->C, "Self"),
        .span = span,
    };
    add_local(R, enclosing_scope(R), ident, id, ns, SYMBOL_DECL);
}

static paw_Bool enter_adt_decl(struct AstVisitor *V, struct AstAdtDecl *d)
{
    struct Resolver *R = V->ud;
    d->did = next_did(R);
    enter_scope(R);

    declare_generics(R, d->generics);
    declare_self(R, d->span, d->id, NAMESPACE_TYPE);
    if (pawAst_is_unit_struct(d)) {
        // allow unit struct to be constructed by writing "Self"
        struct AstDecl *v = K_LIST_FIRST(d->variants);
        declare_self(R, d->span, v->hdr.id, NAMESPACE_VALUE);
    }
    maybe_store_builtin(R, R->current->id, d->ident, d->id, d->did);
    return PAW_TRUE;
}

static void leave_adt_decl(struct AstVisitor *V, struct AstAdtDecl *d)
{
    leave_scope(V->ud);
}

static paw_Bool enter_trait_decl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    struct Resolver *R = V->ud;
    d->did = next_did(R);
    enter_scope(R);

    declare_generics(R, d->generics);
    declare_self(R, d->span, d->id, NAMESPACE_TYPE);
    maybe_store_builtin(R, R->current->id, d->ident, d->id, d->did);
    return PAW_TRUE;
}

static void leave_trait_decl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    leave_scope(V->ud);
}

static paw_Bool enter_type_decl(struct AstVisitor *V, struct AstTypeDecl *d)
{
    struct Resolver *R = V->ud;
    d->did = next_did(R);
    enter_scope(R);

    declare_generics(R, d->generics);
    return PAW_TRUE;
}

static void leave_type_decl(struct AstVisitor *V, struct AstTypeDecl *d)
{
    leave_scope(V->ud);
}

static paw_Bool enter_module_decl(struct AstVisitor *V, struct AstModuleDecl *d)
{
    struct Resolver *R = V->ud;
    R->current = d;
    d->did = next_did(R);
    return PAW_TRUE;
}

static void leave_module_decl(struct AstVisitor *V, struct AstModuleDecl *d)
{
    struct Resolver *R = V->ud;
    R->current = NULL;
}

static paw_Bool enter_closure_expr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    enter_scope(V->ud);
    return PAW_TRUE;
}

static void leave_closure_expr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    leave_scope(V->ud);
}

static paw_Bool ignore_use_decl(struct AstVisitor *V, struct AstUseDecl *d)
{
    return PAW_FALSE;
}

static void resolve_names(struct Resolver *R)
{
    pawAst_visit_decl_list(R->V, R->ast->modules);
}

void pawP_resolve_names(struct Compiler *C)
{
    paw_Env *P = ENV(C);
    struct Resolver R = {
        .V = &(struct AstVisitor){0},
        .pool = pawP_pool_new(C, C->aux_stats),
        .segtab = C->segtab,
        .ast = C->ast,
        .P = P,
        .C = C,
    };
    R.modules = ImportModules_new(&R);
    R.imports = ImportScopes_new(&R);
    R.symtab = Symtab_new(&R);

    // use "AstVisitor" for path resolution
    pawAst_visitor_init(R.V, C->ast, &R);
    R.V->VisitPathExpr = resolve_path_expr;
    R.V->VisitPathType = resolve_path_type;
    R.V->VisitPathPat = resolve_path_pat;
    R.V->VisitStructPat = resolve_struct_pat;
    R.V->VisitVariantPat = resolve_variant_pat;
    R.V->VisitLiteralExpr = resolve_literal_expr;
    R.V->VisitBlock = enter_block_expr;
    R.V->PostVisitBlock = leave_block_expr;
    R.V->VisitAdtDecl = enter_adt_decl;
    R.V->PostVisitAdtDecl = leave_adt_decl;
    R.V->VisitTraitDecl = enter_trait_decl;
    R.V->PostVisitTraitDecl = leave_trait_decl;
    R.V->VisitTypeDecl = enter_type_decl;
    R.V->PostVisitTypeDecl = leave_type_decl;
    R.V->VisitFuncDecl = enter_fn_decl;
    R.V->PostVisitFuncDecl = leave_fn_decl;
    R.V->VisitModuleDecl = enter_module_decl;
    R.V->PostVisitModuleDecl = leave_module_decl;
    R.V->VisitClosureExpr = enter_closure_expr;
    R.V->PostVisitClosureExpr = leave_closure_expr;
    R.V->VisitVariantDecl = resolve_variant_decl;
    R.V->VisitConstDecl = resolve_const_decl;
    R.V->VisitParamDecl = resolve_param_decl;
    R.V->VisitFieldDecl = resolve_field_decl;
    R.V->VisitGenericDecl = resolve_generic_decl;
    R.V->VisitUseDecl = ignore_use_decl;

    // resolve "UseDecl" constructs
    void pawP_resolve_imports(struct Resolver *);
    pawP_resolve_imports(&R);

    // resolve paths
    resolve_names(&R);

    pawP_pool_free(C, R.pool);
}


