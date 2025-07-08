// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "api.h"
#include "ast.h"
#include "compile.h"
#include "debug.h"
#include "error.h"
#include "lib.h"
#include "map.h"
#include "resolve.h"

#define IMPORTER_ERROR(R_, Kind_, Modname_, ...) pawErr_##Kind_((R_)->C, Modname_, __VA_ARGS__)

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

static struct ImportBinding *new_binding(struct Resolver *R)
{
    return P_ALLOC(R->C, NULL, 0, sizeof(struct ImportBinding));
}

DEFINE_LIST(struct Resolver, ImportBindings, struct ImportBinding *)

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

static struct ImportScope *iscope_new(struct Resolver *R, NodeId id, enum ImportScopeKind kind, struct ImportScope *outer)
{
    struct ImportScope *is = P_ALLOC(R, NULL, 0, sizeof(*is));
    *is = (struct ImportScope){
        .types = ImportNames_new(R),
        .values = ImportNames_new(R),
        .outer = outer,
        .kind = kind,
        .id = id,
    };
    return is;
}

static struct ImportScope *collect_items(struct Resolver *R, struct AstDecl *mod, ImportBindings *bindings);

static struct AstDecl *import_module(struct Resolver *R, Str *name, ImportBindings *bindings)
{
    int const *pmodno = ImportModules_get(R, R->modules, name);
    if (pmodno != NULL) return K_LIST_AT(R->ast->modules, *pmodno);

    DLOG(R, "importing module \"%s\"", name->text);

    paw_Env *P = ENV(R);
    ptrdiff_t const saved = SAVE_OFFSET(P, P->top.p);
    V_SET_OBJECT(P->top.p++, name);

    struct LoaderState *state = pawL_start_import(P);
    if (state == NULL) return NULL; // not found

    struct AstDecl *mod = pawP_parse_module(R->C, name, state->f, state);
    pawL_finish_import(P);

    P->top.p = RESTORE_POINTER(P, saved);

    struct AstModuleDecl *m = AstGetModuleDecl(mod);
    ImportModules_insert(R, R->modules, name, m->modno);
    collect_items(R, mod, bindings);
    return mod;
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

static struct ImportSymbol const *get_explicit_symbol(struct ImportName *iname)
{
    struct ImportSymbol const *psymbol;
    K_LIST_FOREACH (iname->symbols, psymbol) {
        if (psymbol->kind == ISYMBOL_EXPLICIT)
           return psymbol;
    }
    return NULL;
}

struct ImportSymbol const *pawP_find_import_symbol(struct Resolver *R, struct ImportScope const *scope, struct PathCursor pc, enum Namespace ns)
{
    struct AstIdent const ident = pc_segment(pc)->ident;
    ImportNames *inames = ns == NAMESPACE_TYPE ? scope->types : scope->values;
    struct ImportName *const *piname = ImportNames_get(R, inames, ident.name);
    if (piname == NULL) return NULL;

    struct ImportName *iname = *piname;
    if (iname->symbols->count == 1)
        return &K_LIST_FIRST(iname->symbols);

    paw_assert(iname->symbols->count > 1);
    struct ImportSymbol const *psymbol = get_explicit_symbol(iname);
    if (psymbol == NULL) {
        // multiple glob-imported symbols with the same name without an explicit
        // import to disambiguate
        while (scope->outer != NULL) scope = scope->outer;
        struct AstModuleDecl const *m = GET_NODE(R, scope->id);
        IMPORTER_ERROR(R, ambiguous_path, m->name, ident.span.start,
                pawAst_print_path(R->ast, pc.path));
    }
    return psymbol;
}

static struct ImportSymbol const *lookup_in_scope(struct Resolver *R, struct ImportScope const *outer, struct PathCursor pc, enum Namespace ns);

static struct ImportSymbol const *lookup_nested_type(struct Resolver *R, NodeId module_id, struct ImportSymbol const *psymbol, struct PathCursor pc)
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
    struct ImportSymbol const *result = pawP_find_import_symbol(R, outer, pc, NAMESPACE_TYPE);
    if (result != NULL) return lookup_nested_type(R, outer->id, result, pc);
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
    struct ImportSymbol const *result = pawP_find_import_symbol(R, outer, pc, NAMESPACE_TYPE);
    if (result != NULL) result = lookup_nested_value(R, result, pc);
    if (result != NULL) return result; // found associated value
    return pawP_find_import_symbol(R, outer, pc, NAMESPACE_VALUE);
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
        // try again in a different module
        struct AstSegment const first = K_LIST_FIRST(b.path.segments);
        int const *pmodno = ImportModules_get(R, R->modules, first.ident.name);
        if (pmodno == NULL) return PAW_FALSE;
        pc_next(&pc);

        struct AstDecl *mod = get_module_by_number(R, *pmodno);
        struct AstModuleDecl const *m = AstGetModuleDecl(mod);
        if (!pc_is_valid(pc)) {
            if (ns == NAMESPACE_VALUE) return PAW_FALSE;
            // found module import, e.g. "use mod;"
            *out = ISYMBOL(m->id, pc_segment(pc)->ident, ISYMBOL_EXPLICIT, b.is_pub);
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
        IMPORTER_ERROR(R, duplicate_item, m->name, ident.span.start,
                "item", ident.name->text);
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
    struct Resolver *R;
};

static void collect_item(struct Resolver *R, struct ImportScope *scope, struct AstDecl *decl);

static void declare_alias(struct AstVisitor *V, struct AstDecl *decl)
{
    if (AstIsTypeDecl(decl)) {
        struct VisitorContext *ctx = V->ud;
        collect_item(ctx->R, ctx->scope, decl);
    }
}

static void collect_fn_decl(struct Resolver *R, struct ImportScope *outer, struct AstFnDecl *d)
{
    struct ImportScope *scope = iscope_new(R, d->id, ISCOPE_FN, outer);
    ImportScopes_insert(R, R->imports, d->id, scope);

    struct VisitorContext ctx = {
        .scope = scope,
        .R = R,
    };
    struct AstVisitor *V = &(struct AstVisitor){0};
    pawAst_visitor_init(V, R->ast, &ctx);
    V->PostVisitDecl = declare_alias;

    pawAst_visit_decl(V, AST_CAST_DECL(d));
}

static void collect_trait_decl(struct Resolver *R, struct ImportScope *outer, struct AstTraitDecl *d)
{
    struct ImportScope *scope = iscope_new(R, d->id, ISCOPE_TYPE, outer);
    ImportScopes_insert(R, R->imports, d->id, scope);

    // add methods and associated functions to trait value namespace
    struct AstDecl *const *pdecl;
    K_LIST_FOREACH (d->methods, pdecl) {
        struct AstFnDecl *f = AstGetFnDecl(*pdecl);
        add_value(R, scope, f->ident, f->id, ISYMBOL_EXPLICIT, f->is_pub);
        collect_fn_decl(R, scope, f);
    }
}

static void collect_adt_decl(struct Resolver *R, struct ImportScope *outer, struct AstAdtDecl *d)
{
    if (pawAst_is_unit_struct(d)) {
        // add name of unit struct to global value namespace
        struct AstDecl *v = K_LIST_FIRST(d->variants);
        add_value(R, outer, d->ident, v->hdr.id, ISYMBOL_EXPLICIT, d->is_pub);
    }

    struct ImportScope *scope = iscope_new(R, d->id, ISCOPE_TYPE, outer);
    ImportScopes_insert(R, R->imports, d->id, scope);

    if (!d->is_struct) {
        // add variant constructors to ADT value namespace
        struct AstDecl *const *pdecl;
        K_LIST_FOREACH (d->variants, pdecl) {
            struct AstVariantDecl *v = AstGetVariantDecl(*pdecl);
            add_value(R, scope, v->ident, v->id, ISYMBOL_EXPLICIT, PAW_TRUE);
        }
    }

    // add methods and associated functions to ADT value namespace
    struct AstDecl *const *pdecl;
    K_LIST_FOREACH (d->methods, pdecl) {
        struct AstFnDecl *f = AstGetFnDecl(*pdecl);
        add_value(R, scope, f->ident, f->id, ISYMBOL_EXPLICIT, f->is_pub);
        collect_fn_decl(R, scope, f);
    }
}

static void collect_item(struct Resolver *R, struct ImportScope *scope, struct AstDecl *decl)
{
    switch (AST_KINDOF(decl)) {
        case kAstFnDecl: {
            struct AstFnDecl *d = AstGetFnDecl(decl);
            add_value(R, scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
            collect_fn_decl(R, scope, d);
            break;
        }
        case kAstConstDecl: {
            struct AstConstDecl *d = AstGetConstDecl(decl);
            add_value(R, scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
            break;
        }
        case kAstTypeDecl: {
            struct AstTypeDecl *d = AstGetTypeDecl(decl);
            add_type(R, scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
            break;
        }
        case kAstTraitDecl: {
            struct AstTraitDecl *d = AstGetTraitDecl(decl);
            add_type(R, scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
            collect_trait_decl(R, scope, d);
            break;
        }
        default: { // kAstAdtDecl
            struct AstAdtDecl *d = AstGetAdtDecl(decl);
            add_type(R, scope, d->ident, d->id, ISYMBOL_EXPLICIT, d->is_pub);
            collect_adt_decl(R, scope, d);
            break;
        }
    }
}

static struct ImportScope *collect_items(struct Resolver *R, struct AstDecl *mod, ImportBindings *bindings)
{
    struct AstModuleDecl const *m = AstGetModuleDecl(mod);
    struct ImportScope *scope = iscope_new(R, m->id, ISCOPE_MODULE, NULL);
    ImportScopes_insert(R, R->imports, m->id, scope);

    struct AstDecl *const *pitem;
    K_LIST_FOREACH (m->items, pitem) {
        if (!AstIsUseDecl(*pitem)) {
            collect_item(R, scope, *pitem);
        } else { // save imports to resolve later
            struct AstUseDecl const *d = AstGetUseDecl(*pitem);
            struct AstIdent const source = K_LIST_LAST(d->path.segments).ident;
            struct AstIdent const target = d->use_kind == AST_USE_ALIAS ? d->as : source;
            struct ImportBinding *pb = new_binding(R);
            *pb = (struct ImportBinding){
                .kind = d->use_kind == AST_USE_GLOB
                    ? ISYMBOL_GLOB : ISYMBOL_EXPLICIT,
                .is_pub = d->is_pub,
                .target_id = m->id,
                .source_id = m->id,
                .target = target,
                .source = source,
                .path = d->path,
                .id = d->id,
            };
            ImportBindings_push(R, bindings, pb);
        }
    }

    return scope;
}

static paw_Bool resolve_module_prefix(struct Resolver *R, struct ImportScope *target, struct ImportBinding *pb, ImportBindings *bindings)
{
    // only the first segment can refer to a not-yet-seen module
    Str *name = K_LIST_FIRST(pb->path.segments).ident.name;
    struct AstDecl *mod = get_module_by_name(R, name);
    if (mod != NULL) return PAW_FALSE; // already imported
    mod = import_module(R, name, bindings);
    if (mod == NULL) return PAW_FALSE; // not a module

    pb->source_id = NODE_ID(mod);
    if (pb->path.segments->count == 1 && pb->kind == ISYMBOL_EXPLICIT) {
        add_type(R, target, pb->target, NODE_ID(mod), ISYMBOL_EXPLICIT, pb->is_pub);
        pb->in_type_ns = PAW_TRUE;
        return PAW_TRUE;
    }
    return PAW_FALSE;
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
    Str *name = K_LIST_FIRST(pb->path.segments).ident.name;
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

#define FOREACH_INAME(Scope_, Key_, Value_, Action_)                                \
    {                                                                              \
        ImportNamesIterator names_iter_;                                           \
        ImportNamesIterator_init(Scope_, &names_iter_);                            \
        while (ImportNamesIterator_is_valid(&names_iter_)) {                       \
            Str const *Key_ = ImportNamesIterator_key(&names_iter_);               \
            struct ImportName *Value_ = *ImportNamesIterator_valuep(&names_iter_); \
            Action_                                                                \
            ImportNamesIterator_next(&names_iter_);                                \
        }                                                                          \
    }


static paw_Bool glob_module(struct Resolver *R, struct ImportScope *target, struct ImportScope *mod, paw_Bool is_pub)
{
    paw_Bool changed = PAW_FALSE;

    FOREACH_INAME(mod->types, _, iname, {
        struct ImportSymbol const *psymbol;
        K_LIST_FOREACH (iname->symbols, psymbol) {
            if (psymbol->is_pub)
                changed |= add_type(R, target, psymbol->ident, psymbol->id, ISYMBOL_GLOB, is_pub);
        }
    });

    FOREACH_INAME(mod->values, _, iname, {
        struct ImportSymbol const *psymbol;
        K_LIST_FOREACH (iname->symbols, psymbol) {
            if (psymbol->is_pub)
                changed |= add_value(R, target, psymbol->ident, psymbol->id, ISYMBOL_GLOB, is_pub);
        }
    });

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
                IMPORTER_ERROR(R, invalid_glob_target, R->current->name,
                        pb->source.span.start, pawAst_print_path(R->ast, pb->path));
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
        if (resolve_glob_in(R, source, target, pb)) {
            changed = PAW_TRUE;
        } else {
            Str *name = K_LIST_FIRST(pb->path.segments).ident.name;
            if (get_module_by_name(R, name) == NULL)
                import_module(R, name, bindings);
        }
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
            IMPORTER_ERROR(R, unknown_path, m->name, ident.span.start,
                    pawAst_print_path(R->ast, pb->path));
        }
    }
}

static void resolve_imports(struct Resolver *R, ImportBindings *bindings)
{
    while (resolve_module_prefixes(R, bindings)
            || resolve_explicit_imports(R, bindings)
            || resolve_glob_imports(R, bindings));

    validate_bindings(R, bindings);
}

void pawP_resolve_imports(struct Resolver *R)
{
    paw_assert(R->ast->modules->count == 1);
    struct AstDecl *main = K_LIST_FIRST(R->ast->modules);
    struct AstModuleDecl *m = AstGetModuleDecl(main);
    ImportModules_insert(R, R->modules, m->name, m->modno);

    ImportBindings *bindings = ImportBindings_new(R);
    collect_items(R, main, bindings);
    resolve_imports(R, bindings);

    if (pawP_push_callback(R->C, "paw.on_build_ast")) {
        paw_push_rawptr(ENV(R), R->ast);
        paw_call(ENV(R), 1);
    }
}
