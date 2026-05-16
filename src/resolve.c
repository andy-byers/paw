// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "resolve.h"
#include "lib.h"

#define RESOLVER_ERROR(R_, Kind_, ...) THROW_ERROR((R_)->C, \
        Kind_, .modname = (R_)->current->name, __VA_ARGS__)


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
DEFINE_LIST(struct Resolver, NodeIdList, NodeId)

enum ScopeKind {
    SCOPE_FN,
    SCOPE_ADT,
    SCOPE_TYPE,
    SCOPE_TRAIT,
    SCOPE_IMPL,
    SCOPE_BLOCK,
};

struct Scope {
    struct Symbols *symbols;
    enum ScopeKind kind;
    NodeId id;
};

DEFINE_LIST(struct Resolver, Symbols, struct Symbol)
DEFINE_LIST(struct Resolver, Symtab, struct Scope)


static NodeId next_id(struct Resolver *R)
{
    return (NodeId){(unsigned)++R->ast->node_count};
}

static DeclId next_did(struct Resolver *R)
{
    return (DeclId){
        .value = (unsigned)++R->C->decl_count,
        .modno = (unsigned)R->current->modno,
    };
}

static paw_Bool is_unary_path(struct AstPath path)
{
    return path.segments->count == 1;
}

static struct Scope enclosing_scope(struct Resolver *R)
{
    paw_assert(R->symtab->count > 0);
    return K_LIST_LAST(R->symtab);
}

static void set_result(struct Resolver *R, NodeId id, NodeId result, enum ResolvedKind kind)
{
    struct ResolvedSegment const resolved = {.id = result, .kind = kind};
    SegmentTable_insert(R->C, R->segtab, id, resolved);
}

_Noreturn static void unknown_path(struct Resolver *R, struct AstPath path, enum Namespace ns)
{
    RESOLVER_ERROR(R, UnknownPath,
            .path = ns == NAMESPACE_TYPE
                ? pawAst_print_type_path(R->ast, path)
                : pawAst_print_value_path(R->ast, path),
            .kind = ns == NAMESPACE_TYPE
                ? SCAN_STR(R->C, "type")
                : SCAN_STR(R->C, "value"),
            .span = path.span);
}

static paw_Bool is_core_module(struct Resolver *R, struct AstDecl *mod)
{
    struct AstModuleDecl const *d = AstGetModuleDecl(mod);
    if (d->modno == PRELUDE_MODNO) return PAW_TRUE;
    for (enum pawL_StdModule m = 0U; m < PAWL_NUM_CORE_MODULES; ++m) {
        paw_assert(pawL_is_core_module(m)); // core modules have low indices
        Str const *std_modname = SCAN_STR(R->C, pawL_std_module_name(m));
        if (pawS_eq(d->name, std_modname)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

static void maybe_store_builtin(struct Resolver *R, NodeId module_id, struct AstIdent ident, NodeId id, DeclId did)
{
    if (is_core_module(R, pawAst_get_node(R->ast, module_id))) {
        struct Builtin *const *pb = BuiltinMap_get(R->C, R->C->builtin_lookup, ident.name);
        if (pb != NULL) {
            (*pb)->did = did;
            (*pb)->id = id;
        }
    }
}

static void maybe_store_core_trait(struct Resolver *R, NodeId module_id, Str const *name, DeclId did, NodeId id)
{
    if (is_core_module(R, pawAst_get_node(R->ast, module_id))) {
        if (pawS_eq(name, SCAN_STR(R->C, "Copy"))) {
            R->C->core_traits[CORE_TRAIT_COPY] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Drop"))) {
            R->C->core_traits[CORE_TRAIT_DROP] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Default"))) {
            R->C->core_traits[CORE_TRAIT_DEFAULT] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Compare"))) {
            R->C->core_traits[CORE_TRAIT_COMPARE] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Hash"))) {
            R->C->core_traits[CORE_TRAIT_HASH] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Equals"))) {
            R->C->core_traits[CORE_TRAIT_EQUALS] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Index"))) {
            R->C->core_traits[CORE_TRAIT_INDEX] = did;
            R->C->core_trait_index_id_hack = id;
        } else if (pawS_eq(name, SCAN_STR(R->C, "From"))) {
            R->C->core_traits[CORE_TRAIT_FROM] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Into"))) {
            R->C->core_traits[CORE_TRAIT_INTO] = did;
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
        case kAstFnDecl:
            return "function";
        case kAstGenericDecl:
            return "generic";
        case kAstImplDecl:
            return "impl";
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

static int count_nonitem_args(AstGenericArgs *args)
{
    int count = 0;
    if (args != NULL) {
        K_LIST_XFOREACH (args, struct AstGenericArg const, p)
            count += p->item == NULL;
    }
    return count;
}

// TODO: should validate all type args in this file, not just those on trait bounds
static void validate_type_args(struct Resolver *R, enum AstDeclKind kind, AstDeclList *generics, struct AstSegment segment)
{
    int const num_args = count_nonitem_args(segment.args);
    if (generics != NULL && segment.args == NULL)
        RESOLVER_ERROR(R, ExpectedTypeArguments,
                .what = SCAN_STR(R->C, decl_kind(kind)),
                .name = segment.ident.name,
                .span = segment.ident.span);

    if (generics == NULL && num_args > 0)
        RESOLVER_ERROR(R, UnexpectedTypeArguments,
                .what = SCAN_STR(R->C, decl_kind(kind)),
                .name = segment.ident.name,
                .span = segment.ident.span);

    if (generics != NULL && generics->count != num_args)
        RESOLVER_ERROR(R, IncorrectTypeArity,
                .have = num_args,
                .want = generics->count,
                .span = segment.ident.span);
}

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
            set_result(R, segment.id, psymbol->id, RESOLVED_MODULE);
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
        struct Scope const scope = Symtab_get(R->symtab, i);
        for (int j = scope.symbols->count - 1; j >= 0; --j) {
            struct Symbol const symbol = Symbols_get(scope.symbols, j);
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
        .ns = ns,
    };
    return PAW_TRUE;
}

static paw_Bool find_containing_type(struct Resolver *R, struct PathCursor *pc, struct Symbol *symbol_out, struct ImportScope const **scope_out)
{
    *scope_out = get_scope(R, R->current->id);

    struct Symbol symbol;
    if (find_local(R, *pc, NAMESPACE_TYPE, &symbol)) {
        struct AstSegment const segment = *pc_segment(*pc);
        set_result(R, segment.id, symbol.id,
                symbol.kind == SYMBOL_DECL ? RESOLVED_DECL : RESOLVED_LOCAL);
    } else {
        *scope_out = find_containing_module(R, pc);
        if (!find_global(R, *scope_out, *pc, NAMESPACE_TYPE, &symbol))
            return PAW_FALSE;
        struct AstSegment const segment = *pc_segment(*pc);
        set_result(R, segment.id, symbol.id, RESOLVED_DECL);
    }
    *symbol_out = symbol;
    return PAW_TRUE;
}

static struct Scope find_outer_scope(struct Resolver *R)
{
    struct Scope scope;
    for (int index = R->symtab->count - 1;; --index) {
        scope = Symtab_get(R->symtab, index);
        if (index <= 0
                || scope.kind == SCOPE_TRAIT
                || scope.kind == SCOPE_IMPL)
            break;
    }
    return scope;
}

static void defer_type_lookup(struct AstIdent ident, struct Symbol *out)
{
    out->ident = ident;
    out->id = INVALID_NODE_ID;
    out->kind = SYMBOL_DECL;
    out->ns = NAMESPACE_TYPE;
}

static void defer_method_lookup(struct AstIdent ident, struct Symbol *out)
{
    out->ident = ident;
    out->id = INVALID_NODE_ID;
    out->kind = SYMBOL_DECL;
    out->ns = NAMESPACE_VALUE;
}

static paw_Bool find_associated_type(struct Resolver *R, struct PathCursor *pc, struct Symbol *out)
{
    if (pawS_eq(out->ident.name, SCAN_STR(R->C, "Self"))) {
        struct Scope const scope = find_outer_scope(R);
        struct ImportScope const *iscope = get_scope(R, scope.id);
        struct ImportSymbol const *p = pawP_find_import_symbol(R, iscope, *pc, NAMESPACE_TYPE);
        if (p == NULL) return PAW_FALSE;

        struct AstSegment const *segment = pc_segment(*pc);
        *out = (struct Symbol){
            .ident = segment->ident,
            .ns = NAMESPACE_TYPE,
            .kind = SYMBOL_DECL,
            .id = p->id,
        };
        set_result(R, segment->id, p->id, RESOLVED_ASSOC);
        return PAW_TRUE;
    }

    struct AstSegment const *segment = pc_segment(*pc);
    defer_type_lookup(segment->ident, out);
    set_result(R, segment->id, out->id, RESOLVED_ASSOC);
    return PAW_TRUE;
}

static paw_Bool lookup_type(struct Resolver *R, struct PathCursor pc, struct Symbol *out)
{
    struct Symbol symbol;
    struct ImportScope const *scope;
    if (!find_containing_type(R, &pc, &symbol, &scope))
        return PAW_FALSE;

    if (!pc_is_last(pc)) {
        pc_next(&pc);
        return find_associated_type(R, &pc, &symbol);
    }

    set_result(R, pc_segment(pc)->id, symbol.id, RESOLVED_DECL);
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
    if (d->is_type) {
        if (d->t.bounds == NULL)
            RESOLVER_ERROR(R, MissingTraitBounds,
                    .span = pc_segment(pc)->span);

        paw_Bool found = PAW_FALSE;
        struct AstGenericBound const *pbound;
        K_LIST_FOREACH (d->t.bounds, pbound) {
            struct ResolvedSegment res = get_path_result(R, pbound->path);
            if (find_value_in_scope(R, res.id, pc, out)) {
                if (found) // more than 1 bound contains a method with the given name
                    RESOLVER_ERROR(R, MultipleApplicableItems,
                            .span = pc_segment(pc)->span);
                found = PAW_TRUE;
            }
        }
        return found;
    }

    return PAW_FALSE;
}

static paw_Bool find_value_in_type(struct Resolver *R, struct AstTypeDecl *d, struct PathCursor pc, struct Symbol *out)
{
    struct Symbol base;
    struct AstPathType *rhs = AstGetPathType(d->rhs);
    if (!lookup(R, rhs->path, NAMESPACE_TYPE, &base))
        unknown_path(R, rhs->path, NAMESPACE_TYPE);
    if (!find_value_in_scope(R, base.id, pc, out))
        defer_method_lookup(pc_segment(pc)->ident, out);
    return PAW_TRUE;
}

static struct Symbol lookup_or_error(struct Resolver *R, struct AstPath path, enum Namespace ns)
{
    struct Symbol symbol;
    if (!lookup(R, path, ns, &symbol))
        unknown_path(R, path, ns);
    return symbol;
}

static paw_Bool lookup_assoc_item(struct Resolver *R, NodeId type_id, struct PathCursor *pc, struct Symbol *out)
{
    struct AstDecl *type = pawAst_get_node(R->ast, type_id);
    switch (AST_KINDOF(type)) {
        case kAstTypeDecl:
            return find_value_in_type(R, AstGetTypeDecl(type), *pc, out);
        case kAstGenericDecl:
            return find_value_in_generic(R, AstGetGenericDecl(type), *pc, out);
        case kAstTraitDecl:
            return find_value_in_scope(R, type_id, *pc, out);
        case kAstAdtDecl:
            // defer resolution of associated functions on ADTs until type checking
            if (!find_value_in_scope(R, type_id, *pc, out))
                defer_method_lookup(pc_segment(*pc)->ident, out);
            return PAW_TRUE;
        case kAstImplDecl: {
            struct Symbol base;
            struct AstImplDecl *d = AstGetImplDecl(type);
            struct AstPathType *rhs = AstGetPathType(d->type);
            if (!lookup(R, rhs->path, NAMESPACE_TYPE, &base))
                unknown_path(R, rhs->path, NAMESPACE_TYPE);
            if (!find_value_in_scope(R, base.id, *pc, out))
                defer_method_lookup(pc_segment(*pc)->ident, out);
            return PAW_TRUE;
        }
        default: {
            unknown_path(R, pc->path, NAMESPACE_VALUE);
        }
    }
}

static paw_Bool lookup_value(struct Resolver *R, struct PathCursor pc, struct Symbol *out)
{
    struct Symbol symbol;
    if (!pc_is_last(pc)) {
        struct ImportScope const *outer;
        if (find_containing_type(R, &pc, &symbol, &outer)) {
            set_result(R, pc_segment(pc)->id, symbol.id, RESOLVED_DECL);
            if (!pc_is_last(pc)) {
                pc_next(&pc); // find associated item in type referenced by "symbol"
                if (!lookup_assoc_item(R, symbol.id, &pc, &symbol)) return PAW_FALSE;
                set_result(R, pc_segment(pc)->id, symbol.id, RESOLVED_ASSOC);
            }

        } else { // must be a value at the toplevel of an imported module
            if (!find_global(R, outer, pc, NAMESPACE_VALUE, &symbol))
                return PAW_FALSE;
            set_result(R, pc_segment(pc)->id, symbol.id, RESOLVED_DECL);
        }

        if (!pc_is_last(pc))
            RESOLVER_ERROR(R, ExtraSegment,
                    .name = pc_segment(pc)->ident.name,
                    .span = pc_segment(pc)->ident.span);

    } else if (find_local(R, pc, NAMESPACE_VALUE, &symbol)) {
        set_result(R, pc_segment(pc)->id, symbol.id,
                symbol.kind == SYMBOL_DECL ? RESOLVED_DECL : RESOLVED_LOCAL);
    } else {
        struct ImportScope const *scope = get_scope(R, R->current->id);
        if (!find_global(R, scope, pc, NAMESPACE_VALUE, &symbol))
            return PAW_FALSE;
        set_result(R, pc_segment(pc)->id, symbol.id, RESOLVED_DECL);
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

static int add_local(struct Resolver *R, struct Scope scope, struct AstIdent ident, NodeId id, enum Namespace ns, enum SymbolKind kind)
{
    struct Symbol const symbol = {
        .ident = ident,
        .kind = kind,
        .ns = ns,
        .id = id,
    };
    Symbols_push(R, scope.symbols, symbol);
    return scope.symbols->count - 1;
}

static void leave_scope(struct Resolver *R)
{
    Symtab_pop(R->symtab);
}

static void enter_scope(struct Resolver *R, NodeId id, enum ScopeKind kind)
{
    Symtab_push(R, R->symtab, (struct Scope){
                .symbols = Symbols_new(R),
                .kind = kind,
                .id = id,
            });
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
    struct ImportScope const *scope = get_scope(R, parent_id);

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
    if (generics != NULL) {
        K_LIST_XFOREACH (generics, struct AstDecl *const, p) {
            struct AstGenericDecl *d = AstGetGenericDecl(*p);
            if (d->is_type) {
                new_local_type(R, d->t.ident, d->id, SYMBOL_DECL);
            } else {
                new_local_value(R, d->k.ident, d->id, SYMBOL_DECL);
            }
            d->did = next_did(R);
        }
    }
}

static paw_Bool resolve_literal_expr(struct AstVisitor *V, struct AstLiteralExpr *e)
{
    struct Resolver *R = V->ud;
    if (e->lit_kind == kAstCompositeLit)
        lookup_or_error(R, e->comp.path, NAMESPACE_TYPE);

    return PAW_TRUE;
}

static void resolve_trait_args(struct AstVisitor *V, NodeId trait_id, struct AstPath path)
{
    struct Resolver *R = V->ud;
    struct ImportScope const *scope = get_scope(R, trait_id);
    K_LIST_XFOREACH (path.segments, struct AstSegment const, segment) {
        if (segment->args != NULL) {
            K_LIST_XFOREACH (segment->args, struct AstGenericArg, arg) {
                pawAst_visit_generic_arg(V, *arg);
                if (arg->item != NULL) {
                    AstSegments *segments = AstSegments_new(R->ast);
                    struct AstIdent const ident = {.span = segment->span, .name = (Str *)arg->item};
                    pawAst_add_segment(R->ast, segments, segment->span, next_id(R), ident, NULL);
                    struct AstPath const path = {.span = segment->span, .segments = segments};
                    struct ImportSymbol const *psymbol = pawP_find_import_symbol(R, scope, pc_create(path), NAMESPACE_TYPE);
                    if (psymbol == NULL)
                        unknown_path(R, path, NAMESPACE_TYPE);

                    arg->target = psymbol->id;
                }
            }
        }
    }
}

static void resolve_type_args(struct AstVisitor *V, struct AstPath path)
{
    struct AstSegment const *psegment;
    K_LIST_FOREACH (path.segments, psegment) {
        if (psegment->args != NULL)
            pawAst_visit_generic_args(V, psegment->args);
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

static paw_Bool resolve_projection_type(struct AstVisitor *V, struct AstProjectionType *t)
{
    struct Resolver *R = V->ud;

    if (AstIsPathType(t->type)) {
        struct Symbol symbol;
        struct AstPathType const *path = AstGetPathType(t->type);
        if (!lookup_type(R, pc_create(path->path), &symbol))
            RESOLVER_ERROR(R, UnknownPath,
                    .path = pawAst_print_type_path(R->ast, path->path),
                    .span = t->span);

        struct AstDecl *self = pawAst_get_node(R->ast, symbol.id);
        if (AstIsTraitDecl(self))
            RESOLVER_ERROR(R, UnexpectedTrait,
                    .span = t->span);
    } else {
        pawAst_visit_type(V, t->type);
    }

    struct Symbol const trait_symbol = lookup_or_error(R, t->trait, NAMESPACE_TYPE);
    struct AstDecl *trait = pawAst_get_node(R->ast, trait_symbol.id);
    if (!AstIsTraitDecl(trait))
        RESOLVER_ERROR(R, ExpectedTrait,
                .path = pawAst_print_type_path(R->ast, t->trait),
                .span = t->span);

    resolve_type_args(V, t->trait);
    return PAW_FALSE;
}

static paw_Bool in_first_alternative(struct OrState const *os)
{
    while (os != NULL) {
        if (os->alt_index > 0)
            return PAW_FALSE;
        os = os->outer;
    }
    return PAW_TRUE;
}

static paw_Bool resolve_ident_pat(struct AstVisitor *V, struct AstIdentPat *p)
{
    struct Resolver *R = V->ud;

    // use a fake path to avoid an allocation
    struct AstPath const path = {
        .segments = &(AstSegments){
            .data = &(struct AstSegment){
                .ident = p->ident,
                .id = p->id,
            },
            .count = 1,
            .alloc = 1,
        },
        .span = p->span,
    };

    struct Symbol symbol;
    struct AstDecl *decl = NULL;
    if (lookup(R, path, NAMESPACE_VALUE, &symbol)
            && symbol.kind == SYMBOL_DECL) {
        decl = pawAst_get_node(R->ast, symbol.id);
    }

    if (decl == NULL || AstIsParamDecl(decl) || AstIsFnDecl(decl)) {
        // create a binding pattern
        set_result(R, p->id, p->id, RESOLVED_LOCAL);

        // only declare locals from bindings in the first alternative
        if (in_first_alternative(R->os))
            new_local_value(R, p->ident, p->id, SYMBOL_VAR);

        if (R->os != NULL) {
            if (R->os->alt_index == 0) {
                paw_Bool const replaced = BoundNames_insert(R,
                        R->os->names, p->ident.name, (struct BoundName){
                            .id = p->id, .span = p->span});
                if (replaced)
                    RESOLVER_ERROR(R, DuplicateBinding,
                            .name = p->ident.name,
                            .span = p->ident.span);
            } else {
                struct BoundName const *name = BoundNames_get(R, R->os->names, p->ident.name);
                if (name == NULL)
                    RESOLVER_ERROR(R, MissingBindingInAlternative,
                            .name = p->ident.name,
                            .span = p->span);
                set_result(R, p->id, name->id, RESOLVED_LOCAL);
            }
        }
    }
    return PAW_FALSE;
}

static paw_Bool resolve_path_pat(struct AstVisitor *V, struct AstPathPat *p)
{
    lookup_or_error(V->ud, p->path, NAMESPACE_VALUE);
    resolve_type_args(V, p->path);
    return PAW_FALSE;
}

static void propagate_bindings(struct Resolver *R, struct OrState const *os)
{
    if (os->outer == NULL) return;

    BoundNamesIterator iter;
    BoundNamesIterator_init(os->names, &iter);
    while (BoundNamesIterator_is_valid(&iter)) {
        Str const *key = BoundNamesIterator_key(&iter);
        struct BoundName const value = *BoundNamesIterator_valuep(&iter);
        if (BoundNames_insert(R, os->outer->names, key, value))
            RESOLVER_ERROR(R, DuplicateBinding,
                    .name = key,
                    .span = value.span);
        BoundNamesIterator_next(&iter);
    }
}

static paw_Bool check_binding(struct AstVisitor *V, struct AstIdentPat *p)
{
    struct Resolver *R = V->ud;
    struct BoundName const *pname = BoundNames_get(R, R->os->names, p->ident.name);
    if (pname == NULL)
        RESOLVER_ERROR(R, MissingBindingInAlternative,
                .name = p->ident.name,
                .span = p->ident.span);

    set_result(R, p->id, pname->id, RESOLVED_LOCAL);
    return PAW_TRUE;
}

static paw_Bool resolve_or_pat(struct AstVisitor *V, struct AstOrPat *p)
{
    struct Resolver *R = V->ud;
    struct AstVisitor checker;
    pawAst_visitor_init(&checker, R->ast, R);
    checker.VisitIdentPat = check_binding;

    struct OrState os = {
        .names = BoundNames_new(R),
        .outer = R->os,
    };
    R->os = &os;

    struct AstPat *const *ppat;
    K_LIST_FOREACH (p->pats, ppat) {
        pawAst_visit_pat(V, *ppat);
        if (os.alt_index > 0) {
            pawAst_visit_pat(&checker, *ppat);
        }
        ++os.alt_index;
    }


    propagate_bindings(R, &os);
    R->os = os.outer;
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
    if (d->is_type) {
        if (d->t.bounds != NULL) {
            K_LIST_XFOREACH (d->t.bounds, struct AstGenericBound const, pbound) {
                struct Symbol const symbol = lookup_or_error(R, pbound->path, NAMESPACE_TYPE);
                struct AstDecl *decl = pawAst_get_node(R->ast, symbol.id);
                if (!AstIsTraitDecl(decl))
                    RESOLVER_ERROR(R, ExpectedTrait,
                            .path = pawAst_print_type_path(R->ast, pbound->path),
                            .span = pbound->path.span);
                resolve_trait_args(V, symbol.id, pbound->path);

                struct AstTraitDecl *trait = AstGetTraitDecl(decl);
                struct AstSegment const last = K_LIST_LAST(pbound->path.segments);
                validate_type_args(R, kAstTraitDecl, trait->generics, last);
            }
        }
    } else {
        // TODO
        PAW_UNREACHABLE();
    }
    return PAW_FALSE;
}

static paw_Bool enter_match_arm(struct AstVisitor *V, struct AstMatchArm *e)
{
    PAW_UNUSED(e);
    enter_scope(V->ud, e->id, SCOPE_BLOCK);
    return PAW_TRUE;
}

static void leave_match_arm(struct AstVisitor *V, struct AstMatchArm *e)
{
    PAW_UNUSED(e);
    leave_scope(V->ud);
}

static paw_Bool enter_block_expr(struct AstVisitor *V, struct AstBlock *e)
{
    PAW_UNUSED(e);
    enter_scope(V->ud, e->id, SCOPE_BLOCK);
    return PAW_TRUE;
}

static void leave_block_expr(struct AstVisitor *V, struct AstBlock *e)
{
    PAW_UNUSED(e);
    leave_scope(V->ud);
}

static paw_Bool resolve_projection_expr(struct AstVisitor *V, struct AstProjectionExpr *e)
{
    struct Resolver *R = V->ud;

    if (AstIsPathType(e->type)) {
        struct Symbol symbol;
        struct AstPathType const *path = AstGetPathType(e->type);
        if (!lookup_type(R, pc_create(path->path), &symbol))
            RESOLVER_ERROR(R, UnknownPath,
                    .path = pawAst_print_type_path(R->ast, path->path),
                    .span = e->span);

        struct AstDecl *self = pawAst_get_node(R->ast, symbol.id);
        if (AstIsTraitDecl(self))
            RESOLVER_ERROR(R, UnexpectedTrait,
                    .span = e->span);
    } else {
        pawAst_visit_type(V, e->type);
    }

    struct Symbol const trait_symbol = lookup_or_error(R, e->trait, NAMESPACE_TYPE);
    struct AstDecl *trait = pawAst_get_node(R->ast, trait_symbol.id);
    if (!AstIsTraitDecl(trait))
        RESOLVER_ERROR(R, ExpectedTrait,
                .path = pawAst_print_type_path(R->ast, e->trait),
                .span = e->span);

    resolve_type_args(V, e->trait);
    return PAW_FALSE;
}

static paw_Bool enter_for_expr(struct AstVisitor *V, struct AstForExpr *e)
{
    PAW_UNUSED(e);
    enter_scope(V->ud, e->id, SCOPE_BLOCK);
    return PAW_TRUE;
}

static void leave_for_expr(struct AstVisitor *V, struct AstForExpr *e)
{
    PAW_UNUSED(e);
    leave_scope(V->ud);
}

static paw_Bool enter_fn_decl(struct AstVisitor *V, struct AstFnDecl *d)
{
    struct Resolver *R = V->ud;
    enter_scope(R, d->id, SCOPE_FN);
    d->did = next_did(R);

    declare_generics(R, d->generics);
    declare_type_aliases(R, d->id);
    return PAW_TRUE;
}

static void leave_fn_decl(struct AstVisitor *V, struct AstFnDecl *d)
{
    PAW_UNUSED(d);
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
    enter_scope(R, d->id, SCOPE_ADT);
    d->did = next_did(R);

    declare_generics(R, d->generics);
    declare_self(R, d->span, d->id, NAMESPACE_TYPE);

    maybe_store_builtin(R, R->current->id, d->ident, d->id, d->did);
    return PAW_TRUE;
}

static void leave_adt_decl(struct AstVisitor *V, struct AstAdtDecl *d)
{
    PAW_UNUSED(d);
    leave_scope(V->ud);
}

static paw_Bool enter_trait_decl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    struct Resolver *R = V->ud;
    enter_scope(R, d->id, SCOPE_TRAIT);
    d->did = next_did(R);

    declare_generics(R, d->generics);

    // declare type parameter named "Self"
    {
        NodeId const self_id = next_id(R);
        NodeMap_insert(R->C, R->C->self_types, d->id, self_id);
        struct SourceSpan const span = SourceSpan_from_ref(
                pawSrc_create_ref(R->C, d->span),
                SPAN_REF_TRAIT_SELF);
        struct AstIdent const ident = {
            .name = SCAN_STR(R->C, "Self"),
            .span = span,
        };
        struct AstDecl *self = pawAst_new_generic_type_decl(
                R->ast, span, self_id, ident, d->supertraits);
        add_local(R, enclosing_scope(R), ident, self_id,
                NAMESPACE_TYPE, SYMBOL_DECL);
        self->hdr.did = next_did(R);

        pawAst_visit_decl(V, self);
    }

    declare_generics(R, d->types);

    maybe_store_core_trait(R, R->current->id, d->ident.name, d->did, d->id);
    return PAW_TRUE;
}

static void leave_trait_decl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    PAW_UNUSED(d);
    leave_scope(V->ud);
}

static paw_Bool enter_impl_decl(struct AstVisitor *V, struct AstImplDecl *d)
{
    struct Resolver *R = V->ud;
    enter_scope(R, d->id, SCOPE_IMPL);
    d->did = next_did(R);

    declare_generics(R, d->generics);
    declare_self(R, d->span, d->id, NAMESPACE_TYPE);

    if (AstIsPathType(d->type)) {
        struct Symbol symbol;
        // locate the context parameter definition
        struct AstPathType const *path = AstGetPathType(d->type);
        struct PathCursor pc = pc_create(path->path);
        if (!lookup_type(R, pc, &symbol))
            unknown_path(R, path->path, NAMESPACE_TYPE);

        struct AstDecl *self = pawAst_get_node(R->ast, symbol.id);
        if (AstIsAdtDecl(self)) {
            struct AstAdtDecl *d = AstGetAdtDecl(self);
            if (pawAst_is_unit_struct(d)) {
                // allow unit struct to be constructed by writing "Self"
                struct AstDecl *v = K_LIST_FIRST(d->variants);
                declare_self(R, d->span, v->hdr.id, NAMESPACE_VALUE);
            }
        }
    } else if (AstIsFnType(d->type)
            || AstIsNeverType(d->type)) {
        RESOLVER_ERROR(R, InvalidImplTarget,
                .span = d->span);
    }

    return PAW_TRUE;
}

static void leave_impl_decl(struct AstVisitor *V, struct AstImplDecl *d)
{
    PAW_UNUSED(d);
    leave_scope(V->ud);
}

static paw_Bool enter_type_decl(struct AstVisitor *V, struct AstTypeDecl *d)
{
    struct Resolver *R = V->ud;
    enter_scope(R, d->id, SCOPE_TYPE);
    d->did = next_did(R);

    declare_generics(R, d->generics);
    return PAW_TRUE;
}

static void leave_type_decl(struct AstVisitor *V, struct AstTypeDecl *d)
{
    PAW_UNUSED(d);
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
    PAW_UNUSED(d);
    struct Resolver *R = V->ud;
    R->current = NULL;
}

static paw_Bool enter_closure_expr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    PAW_UNUSED(e);
    enter_scope(V->ud, e->id, SCOPE_FN);
    return PAW_TRUE;
}

static void leave_closure_expr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    PAW_UNUSED(e);
    leave_scope(V->ud);
}

static paw_Bool ignore_use_decl(struct AstVisitor *V, struct AstUseDecl *d)
{
    PAW_UNUSED(V);
    PAW_UNUSED(d);
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
    R.V->VisitProjectionType = resolve_projection_type;
    R.V->VisitIdentPat = resolve_ident_pat;
    R.V->VisitPathPat = resolve_path_pat;
    R.V->VisitOrPat = resolve_or_pat;
    R.V->VisitStructPat = resolve_struct_pat;
    R.V->VisitVariantPat = resolve_variant_pat;
    R.V->VisitLiteralExpr = resolve_literal_expr;
    R.V->VisitMatchArm = enter_match_arm;
    R.V->PostVisitMatchArm = leave_match_arm;
    R.V->VisitProjectionExpr = resolve_projection_expr;
    R.V->VisitBlock = enter_block_expr;
    R.V->PostVisitBlock = leave_block_expr;
    R.V->VisitForExpr = enter_for_expr;
    R.V->PostVisitForExpr = leave_for_expr;
    R.V->VisitAdtDecl = enter_adt_decl;
    R.V->PostVisitAdtDecl = leave_adt_decl;
    R.V->VisitTraitDecl = enter_trait_decl;
    R.V->PostVisitTraitDecl = leave_trait_decl;
    R.V->VisitTypeDecl = enter_type_decl;
    R.V->PostVisitTypeDecl = leave_type_decl;
    R.V->VisitFnDecl = enter_fn_decl;
    R.V->PostVisitFnDecl = leave_fn_decl;
    R.V->VisitImplDecl = enter_impl_decl;
    R.V->PostVisitImplDecl = leave_impl_decl;
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

