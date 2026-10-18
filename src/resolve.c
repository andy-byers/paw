// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "resolve.h"
#include "lib.h"

#define RESOLVER_ERROR(R_, Kind_, ...) THROW_ERROR((R_)->C, \
        Kind_, .modname = current_modname(R_), __VA_ARGS__)


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

enum ResultKind {
    RESULT_LOCAL,
    RESULT_UPVALUE,
    RESULT_DECL,
};

struct LookupResult {
    enum ResultKind kind;
    struct AstIdent ident;
    NodeId id;

    // extra info for variables
    struct {
        int index;
    } var;
};

DEFINE_LIST(struct Resolver, SymbolList, struct Symbol,)
DEFINE_LIST(struct Resolver, NodeIdList, NodeId,)

static paw_Bool resolve_symbol(struct Resolver *R, struct AstPath path, enum Namespace ns, struct LookupResult *out);

struct FnState {
    struct FnState *outer;
    UpvalueList *upvalues;
    int scope_id;
    DeclId did;
};

struct BlockState {
    struct BlockState *outer;
    int start;
};

enum ScopeKind {
    SCOPE_FN,
    SCOPE_ADT,
    SCOPE_TYPE,
    SCOPE_TRAIT,
    SCOPE_IMPL,
};

struct Scope {
    struct Symbols *symbols;
    enum ScopeKind kind;
    NodeId id;
};

DEFINE_LIST(struct Resolver, Symbols, struct Symbol,)
DEFINE_LIST(struct Resolver, Symtab, struct Scope,)


static NodeId next_id(struct Resolver *R)
{
    return (NodeId){(unsigned)++R->ast->node_count};
}

static DeclId next_did(struct Resolver *R)
{
    return pawP_next_decl_id(R->C, AstGetModuleDecl(K_LIST_LAST(R->modstack))->modno);
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

static struct AstModuleDecl *current_module(struct Resolver *R)
{
    return AstGetModuleDecl(K_LIST_LAST(R->modstack));
}

static Str const *current_modname(struct Resolver *R)
{
    return current_module(R)->ident.name;
}

PAW_NORETURN static void unknown_path(struct Resolver *R, struct AstPath path, enum Namespace ns)
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
        if (pawS_eq(d->ident.name, std_modname)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool is_ops_module(struct Resolver *R, struct AstDecl *mod)
{
    struct AstModuleDecl const *d = AstGetModuleDecl(mod);
    return pawS_eq(d->ident.name, SCAN_STR(R->C, "ops"));
}

static paw_Bool is_math_module(struct Resolver *R, struct AstDecl *mod)
{
    struct AstModuleDecl const *d = AstGetModuleDecl(mod);
    return pawS_eq(d->ident.name, SCAN_STR(R->C, "math"));
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

static void maybe_store_core_item(struct Resolver *R, NodeId module_id, Str const *name, DeclId did)
{
    // TODO: once packages are supported, check if the package is "std"
    struct AstDecl *m = pawAst_get_node(R->ast, module_id);
    if (is_core_module(R, m)) {
        if (is_ops_module(R, m)) {
            if (pawS_eq(name, SCAN_STR(R->C, "Ordering")))
                R->C->std_ops_ordering = did;
        }
    } else if (is_math_module(R, m)) {
        if (pawS_eq(name, SCAN_STR(R->C, "NAN"))) {
            R->C->std_math_NAN = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "INFINITY"))) {
            R->C->std_math_INFINITY = did;
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
        } else if (pawS_eq(name, SCAN_STR(R->C, "Ord"))) {
            R->C->core_traits[CORE_TRAIT_ORD] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Hash"))) {
            R->C->core_traits[CORE_TRAIT_HASH] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Eq"))) {
            R->C->core_traits[CORE_TRAIT_EQ] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Index"))) {
            R->C->core_traits[CORE_TRAIT_INDEX] = did;
            R->C->core_trait_index_id_hack = id;
        } else if (pawS_eq(name, SCAN_STR(R->C, "From"))) {
            R->C->core_traits[CORE_TRAIT_FROM] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Into"))) {
            R->C->core_traits[CORE_TRAIT_INTO] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Neg"))) {
            R->C->core_traits[CORE_TRAIT_NEG] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Not"))) {
            R->C->core_traits[CORE_TRAIT_NOT] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "BitNot"))) {
            R->C->core_traits[CORE_TRAIT_BITNOT] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Add"))) {
            R->C->core_traits[CORE_TRAIT_ADD] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Sub"))) {
            R->C->core_traits[CORE_TRAIT_SUB] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Mul"))) {
            R->C->core_traits[CORE_TRAIT_MUL] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Div"))) {
            R->C->core_traits[CORE_TRAIT_DIV] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Rem"))) {
            R->C->core_traits[CORE_TRAIT_MOD] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "BitAnd"))) {
            R->C->core_traits[CORE_TRAIT_BITAND] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "BitOr"))) {
            R->C->core_traits[CORE_TRAIT_BITOR] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "BitXor"))) {
            R->C->core_traits[CORE_TRAIT_BITXOR] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Shl"))) {
            R->C->core_traits[CORE_TRAIT_SHL] = did;
        } else if (pawS_eq(name, SCAN_STR(R->C, "Shr"))) {
            R->C->core_traits[CORE_TRAIT_SHR] = did;
        }
    }
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

static struct ImportScope const *get_iscope(struct Resolver *R, NodeId id)
{
    return *ImportScopes_get(R, R->imports, id);
}

static struct ImportScope const *get_current_iscope(struct Resolver *R)
{
    return get_iscope(R, R->current_scope);
}

static void iscope_enter(struct Resolver *R, NodeId id)
{
    R->current_scope = id;
}

static void iscope_leave(struct Resolver *R)
{
    struct ImportScope const *scope = get_current_iscope(R);
    R->current_scope = scope->outer != NULL
        ? scope->outer->id
        : INVALID_NODE_ID;
}

static struct Scope get_scope(struct Resolver *R, int scope_id)
{
    return Symtab_get(R->symtab, scope_id);
}

#define NOT_FOUND (-1)

static int resolve_local(struct Resolver *R, struct FnState *fs, Str const *name, enum Namespace ns, struct LookupResult *out)
{
    struct Scope const scope = get_scope(R, fs->scope_id);
    for (int i = scope.symbols->count - 1; i >= 0; --i) {
        struct Symbol const symbol = Symbols_get(scope.symbols, i);
        if (symbol.ns == ns && pawS_eq(name, symbol.ident.name)) {
            *out = (struct LookupResult){
                .kind = RESULT_LOCAL,
                .ident = symbol.ident,
                .id = symbol.id,
                .var.index = i,
            };
            return i;
        }
    }
    return NOT_FOUND;
}

static int resolve_local_var(struct Resolver *R, struct FnState *fs, Str const *name, struct LookupResult *out)
{
    struct Scope const scope = get_scope(R, fs->scope_id);
    for (int i = scope.symbols->count - 1; i >= 0; --i) {
        struct Symbol const symbol = Symbols_get(scope.symbols, i);
        if (symbol.ns == NAMESPACE_VALUE && symbol.kind == SYMBOL_VAR
                && pawS_eq(name, symbol.ident.name)) {
            *out = (struct LookupResult){
                .kind = RESULT_LOCAL,
                .ident = symbol.ident,
                .id = symbol.id,
                .var.index = i,
            };
            return i;
        }
    }
    return NOT_FOUND;
}

static int add_upvalue(struct Resolver *R, struct FnState *fs, struct LookupResult *r, paw_Bool is_local)
{
    r->kind = RESULT_UPVALUE;

    int index;
    struct UpvalueInfo *pup;
    K_LIST_ENUMERATE (fs->upvalues, index, pup) {
        if (is_local == pup->is_local && pup->index == r->var.index) {
            r->var.index = index;
            return index;
        }
    }
    if (fs->upvalues->count == PAW_MAX_UPVALUES)
        RESOLVER_ERROR(R, TooManyUpvalues,
                .limit = PAW_MAX_UPVALUES,
                .span = r->ident.span);

    UpvalueList_push(R->C, fs->upvalues, (struct UpvalueInfo){
        .is_local = is_local,
        .index = r->var.index,
        .id = r->id,
    });

    // indicate new upvalue index
    r->var.index = fs->upvalues->count - 1;
    return r->var.index;
}

static int resolve_upvalue(struct Resolver *R, struct FnState *fs, Str const *name, struct LookupResult *r)
{
    struct FnState *caller = fs->outer;
    if (caller == NULL) return NOT_FOUND;

    int const local = resolve_local_var(R, fs->outer, name, r);
    if (local != NOT_FOUND)
        return add_upvalue(R, fs, r, PAW_TRUE);

    int const upvalue = resolve_upvalue(R, caller, name, r);
    if (upvalue != NOT_FOUND)
        return add_upvalue(R, fs, r, PAW_FALSE);

    return NOT_FOUND;
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

static void defer_type_lookup(struct AstIdent ident, struct LookupResult *out)
{
    *out = (struct LookupResult){
        .kind = RESULT_DECL,
        .id = INVALID_NODE_ID,
        .ident = ident,
    };
}

static void defer_method_lookup(struct AstIdent ident, struct LookupResult *out)
{
    *out = (struct LookupResult){
        .kind = RESULT_DECL,
        .id = INVALID_NODE_ID,
        .ident = ident,
    };
}

static paw_Bool find_value_in_scope(struct Resolver *R, NodeId scope_id, struct PathCursor pc, struct LookupResult *out)
{
    struct ImportScope const *scope = get_iscope(R, scope_id);
    struct ImportSymbol const *psymbol = pawP_find_import_symbol(R, scope, pc_ident(pc).name, NAMESPACE_VALUE);
    if (psymbol == NULL) return PAW_FALSE;

    *out = (struct LookupResult){
        .ident = pc_ident(pc),
        .kind = RESULT_DECL,
        .id = psymbol->id,
    };
    return PAW_TRUE;
}

static paw_Bool lookup(struct Resolver *R, struct AstPath path, enum Namespace ns, struct LookupResult *out);

static struct LookupResult lookup_or_error(struct Resolver *R, struct AstPath path, enum Namespace ns)
{
    struct LookupResult result;
    if (!lookup(R, path, ns, &result))
        unknown_path(R, path, ns);
    return result;
}

static paw_Bool find_nonglobal_decl(struct Resolver *R, Str const *name, enum Namespace ns, struct LookupResult *out)
{
    paw_assert(R->symtab != NULL);
    for (int i = R->symtab->count - 1; i >= 0; --i) {
        struct Scope const scope = Symtab_get(R->symtab, i);
        for (int j = scope.symbols->count - 1; j >= 0; --j) {
            struct Symbol const symbol = Symbols_get(scope.symbols, j);
            if (symbol.ns == ns && symbol.kind == SYMBOL_DECL
                    && pawS_eq(name, symbol.ident.name)) {
                *out = (struct LookupResult){
                    .kind = RESULT_DECL,
                    .ident = symbol.ident,
                    .id = symbol.id,
                };
                return PAW_TRUE;
            }
        }
    }
    return PAW_FALSE;
}

static void resolve_path_prefix_aux(struct Resolver *R, struct PathCursor *pc, NodeId target, struct LookupResult *out)
{
    struct AstDecl const *root = pawAst_get_node(R->ast, target);
    set_result(R, pc_segment(*pc)->id, target,
            AstIsModuleDecl(root) ? RESOLVED_MODULE : RESOLVED_DECL);
    pc_next(pc);
    if (pc_is_valid(*pc)) {
        struct ImportScope *const *pscope = ImportScopes_get(R, R->imports, target);
        if (pscope == NULL) return;

        struct ImportScope const *scope = *pscope;
        while (!pc_is_last(*pc) && scope != NULL && scope->kind == ISCOPE_MODULE) {
            struct ImportSymbol const *symbol = pawP_find_import_symbol(R, scope, pc_ident(*pc).name, NAMESPACE_TYPE);
            if (symbol == NULL) break;

            *out = (struct LookupResult){
                .id = symbol->id,
                .ident = symbol->ident,
                .kind = RESULT_DECL,
            };
            struct AstDecl const *decl = pawAst_get_node(R->ast, symbol->id);
            set_result(R, pc_segment(*pc)->id, symbol->id,
                    AstIsModuleDecl(decl) ? RESOLVED_MODULE : RESOLVED_DECL);
            scope = get_iscope(R, symbol->id);
            pc_next(pc);
        }
    }
}

static paw_Bool resolve_path_prefix(struct Resolver *R, struct PathCursor *pc, enum Namespace ns, struct LookupResult *out)
{
    paw_assert(pc_is_valid(*pc));
    enum Namespace const root_ns = pc_is_last(*pc) ? ns : NAMESPACE_TYPE;
    Str const *root_name = pc_ident(*pc).name;

    if (find_nonglobal_decl(R, root_name, root_ns, out)) {
        resolve_path_prefix_aux(R, pc, out->id, out);
        return PAW_TRUE;
    }

    struct ImportScope const *scope = get_current_iscope(R);
    do {
        struct AstDecl const *parent = pawAst_get_node(R->ast, scope->id);
        if (!AstIsImplDecl(parent)) {
            struct ImportSymbol const *symbol = pawP_find_import_symbol(R, scope, pc_ident(*pc).name, root_ns);
            if (symbol != NULL) {
                *out = (struct LookupResult){
                    .ident = symbol->ident,
                    .kind = RESULT_DECL,
                    .id = symbol->id,
                };
                resolve_path_prefix_aux(R, pc, symbol->id, out);
                return PAW_TRUE;
            }
        }
        scope = scope->outer;
    } while (scope != NULL);
    return PAW_FALSE;
}

static paw_Bool resolve_symbol_in_module(struct Resolver *R, NodeId module_id, struct AstSegment segment, enum Namespace ns, struct LookupResult *out)
{
    struct ImportSymbol const *symbol = pawP_find_import_symbol(R,
            get_iscope(R, module_id), segment.ident.name, ns);
    if (symbol != NULL) {
        *out = (struct LookupResult){
            .id = symbol->id,
            .ident = symbol->ident,
            .kind = RESULT_DECL,
        };
        set_result(R, segment.id, symbol->id, RESOLVED_DECL);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool defer_assoc_lookup(struct Resolver *R, struct AstSegment segment, enum Namespace ns, struct LookupResult *out)
{
    if (ns == NAMESPACE_VALUE) {
        defer_method_lookup(segment.ident, out);
    } else {
        defer_type_lookup(segment.ident, out);
    }
    set_result(R, segment.id, INVALID_NODE_ID, RESOLVED_ASSOC);
    return PAW_TRUE;
}

static paw_Bool resolve_symbol_in_trait(struct Resolver *R, NodeId trait_id, struct AstSegment segment, enum Namespace ns, struct LookupResult *out)
{
    struct ImportSymbol const *symbol = pawP_find_import_symbol(R,
            get_iscope(R, trait_id), segment.ident.name, ns);
    if (symbol != NULL) {
        *out = (struct LookupResult){
            .id = symbol->id,
            .ident = symbol->ident,
            .kind = RESULT_DECL,
        };
        set_result(R, segment.id, symbol->id, RESOLVED_ASSOC);
        return PAW_TRUE;
    }

    defer_assoc_lookup(R, segment, ns, out);
    return PAW_TRUE;
}

static paw_Bool find_assoc(struct Resolver *R, struct AstSegment segment, struct ImportScope const *iscope, enum Namespace ns, struct LookupResult *out)
{
    struct ImportSymbol const *symbol = pawP_find_import_symbol(R,
            iscope, segment.ident.name, ns);
    if (symbol != NULL) {
        *out = (struct LookupResult){
            .id = symbol->id,
            .ident = symbol->ident,
            .kind = RESULT_DECL,
        };
        set_result(R, segment.id, symbol->id, RESOLVED_ASSOC);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool resolve_symbol_in_impl(struct Resolver *R, NodeId impl_id, struct AstSegment segment, enum Namespace ns, struct LookupResult *out)
{
    struct AstImplDecl const *d = pawAst_get_node(R->ast, impl_id);
    if (AstIsPathType(d->type)) {
        struct LookupResult adt;
        struct AstPathType const *t = AstGetPathType(d->type);
        if (resolve_symbol(R, t->path, NAMESPACE_TYPE, &adt)) {
            if (find_assoc(R, segment, get_iscope(R, adt.id), ns, out))
                return PAW_TRUE;
        }
    }

    if (find_assoc(R, segment, get_iscope(R, impl_id), ns, out))
        return PAW_TRUE;

    defer_assoc_lookup(R, segment, ns, out);
    return PAW_TRUE;
}

static paw_Bool resolve_symbol_in(struct Resolver *R, NodeId parent_id, struct AstSegment segment, enum Namespace ns, struct LookupResult *out)
{
    struct ImportSymbol const *symbol = pawP_find_import_symbol(R,
            get_iscope(R, parent_id), segment.ident.name, ns);
    if (symbol != NULL) {
        set_result(R, segment.id, symbol->id, RESOLVED_ASSOC);
        *out = (struct LookupResult){
            .id = symbol->id,
            .ident = symbol->ident,
            .kind = RESULT_DECL,
        };
        return PAW_TRUE;
    }

    defer_assoc_lookup(R, segment, ns, out);
    return PAW_TRUE;
}

static paw_Bool resolve_symbol(struct Resolver *R, struct AstPath path, enum Namespace ns, struct LookupResult *out)
{
    paw_assert(path.segments->count > 0);
    if (path.segments->count == 1 && R->fs != NULL && ns == NAMESPACE_VALUE) {
        struct AstSegment const first = AstSegments_first(path.segments);
        if (resolve_local_var(R, R->fs, first.ident.name, out) != NOT_FOUND) {
            set_result(R, first.id, out->id, RESOLVED_LOCAL);
            return PAW_TRUE;
        } else if (resolve_upvalue(R, R->fs, first.ident.name, out) != NOT_FOUND) {
            SegmentTable_insert(R->C, R->segtab, first.id,
                    (struct ResolvedSegment){
                        .kind = RESOLVED_UPVALUE,
                        .upvalue = out->var.index,
                    });
            return PAW_TRUE;
        }
    }

    struct LookupResult root;
    struct PathCursor pc = pc_create(path);
    if (resolve_path_prefix(R, &pc, ns, &root)) {
        if (!pc_is_valid(pc)) {
            *out = root;
            return PAW_TRUE;
        }

        if (!pc_is_last(pc))
            RESOLVER_ERROR(R, ExtraSegment,
                    .name = pc_ident(pc).name,
                    .span = pc_ident(pc).span);

        struct AstDecl const *decl = pawAst_get_node(R->ast, root.id);
        switch (AST_KINDOF(decl)) {
            case kAstModuleDecl:
                return resolve_symbol_in_module(R, root.id, *pc_segment(pc), ns, out);
            case kAstTraitDecl:
                return resolve_symbol_in_trait(R, root.id, *pc_segment(pc), ns, out);
            case kAstImplDecl:
                return resolve_symbol_in_impl(R, root.id, *pc_segment(pc), ns, out);
            case kAstGenericDecl:
            case kAstTypeDecl:
                return defer_assoc_lookup(R, *pc_segment(pc), ns, out);
            default:
                return resolve_symbol_in(R, root.id, *pc_segment(pc), ns, out);
        }
    }
    return PAW_FALSE;
}

static paw_Bool lookup(struct Resolver *R, struct AstPath path, enum Namespace ns, struct LookupResult *out)
{
    return resolve_symbol(R, path, ns, out);
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

static void leave_block(struct Resolver *R)
{
    struct Scope const scope = enclosing_scope(R);
    scope.symbols->count = R->bs->start;
    R->bs = R->bs->outer;
}

static void enter_block(struct Resolver *R)
{
    struct BlockState *bs = P_ALLOC(R->C, NULL, 0, sizeof(*bs));
    *bs = (struct BlockState){
        .start = enclosing_scope(R).symbols->count,
        .outer = R->bs,
    };
    R->bs = bs;
}

static void leave_fn(struct Resolver *R)
{
    if (R->fs->upvalues->count > 0) {
        paw_assert(DECL_ID_EXISTS(R->fs->did));
        UpvalueTable_insert_unique(R->C, R->C->upvtab, R->fs->did, R->fs->upvalues);
    }

    leave_scope(R);
    R->fs = R->fs->outer;
}

static void enter_fn(struct Resolver *R, NodeId id, DeclId did)
{
    struct FnState *fs = P_ALLOC(R->C, NULL, 0, sizeof(*fs));
    *fs = (struct FnState){
        .upvalues = UpvalueList_new(R->C),
        .scope_id = R->symtab->count,
        .outer = R->fs,
        .did = did,
    };
    R->fs = fs;
    enter_scope(R, id, SCOPE_FN);
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
    struct ImportScope const *scope = get_iscope(R, parent_id);

    ImportNamesIterator iter;
    ImportNamesIterator_init(scope->types, &iter);
    while (ImportNamesIterator_is_valid(&iter)) {
        struct ImportName const *iname = *ImportNamesIterator_valuep(&iter);
        paw_assert(iname->symbols->count == 1);
        struct ImportSymbol symbol = K_LIST_FIRST(iname->symbols);
        struct AstIdent TODO = {.name = (Str const *)ImportNamesIterator_key(&iter)};
        new_local_type(R, TODO, symbol.id, SYMBOL_DECL);
        ImportNamesIterator_next(&iter);
    }

    ImportNamesIterator_init(scope->values, &iter);
    while (ImportNamesIterator_is_valid(&iter)) {
        struct ImportName const *iname = *ImportNamesIterator_valuep(&iter);
        paw_assert(iname->symbols->count == 1);
        struct ImportSymbol symbol = K_LIST_FIRST(iname->symbols);
        struct AstIdent TODO = {.name = (Str const *)ImportNamesIterator_key(&iter)};
        new_local_value(R, TODO, symbol.id, SYMBOL_DECL);
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
            struct AstGenericDecl const *d = AstGetGenericDecl(*p);
            if (d->is_type) {
                new_local_type(R, d->t.ident, d->id, SYMBOL_DECL);
            } else {
                new_local_value(R, d->k.ident, d->id, SYMBOL_DECL);
            }
        }
    }
}

static paw_Bool resolve_literal_expr(struct AstVisitor *V, struct AstLiteralExpr *e)
{
    struct Resolver *R = V->ud;
    if (e->lit_kind == AST_LIT_COMPOSITE)
        lookup_or_error(R, e->composite.path, NAMESPACE_TYPE);

    return PAW_TRUE;
}

static void resolve_trait_args(struct AstVisitor *V, NodeId trait_id, struct AstPath path)
{
    struct Resolver *R = V->ud;
    struct ImportScope const *scope = get_iscope(R, trait_id);
    K_LIST_XFOREACH (path.segments, struct AstSegment const, segment) {
        if (segment->args != NULL) {
            K_LIST_XFOREACH (segment->args, struct AstGenericArg, arg) {
                pawAst_visit_generic_arg(V, *arg);
                if (arg->item != NULL) {
                    struct ImportSymbol const *psymbol = pawP_find_import_symbol(R, scope, arg->item, NAMESPACE_TYPE);
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
    struct Resolver *R = V->ud;
    struct LookupResult const r = lookup_or_error(R, e->path, NAMESPACE_VALUE);
    if (r.kind == RESULT_DECL)
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
        struct LookupResult result;
        struct AstPathType const *path = AstGetPathType(t->type);
        if (!lookup(R, path->path, NAMESPACE_TYPE, &result))
            RESOLVER_ERROR(R, UnknownPath,
                    .path = pawAst_print_type_path(R->ast, path->path),
                    .span = t->span);

        // TODO: this check needs to be moved somewhere else. it cannot be here b/c we don't yet know the NodeId of associated types
//        struct AstDecl *self = pawAst_get_node(R->ast, result.id);
//        if (AstIsTraitDecl(self))
//            RESOLVER_ERROR(R, UnexpectedTrait,
//                    .span = t->span);
    } else {
        pawAst_visit_type(V, t->type);
    }

    struct LookupResult const trait_result = lookup_or_error(R, t->trait, NAMESPACE_TYPE);
    struct AstDecl *trait = pawAst_get_node(R->ast, trait_result.id);
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

    struct LookupResult r;
    struct AstDecl *decl = NULL;
    if (lookup(R, path, NAMESPACE_VALUE, &r)
            && r.kind == RESULT_DECL) {
        decl = pawAst_get_node(R->ast, r.id);
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
    PAW_UNUSED(V);
    PAW_UNUSED(d);
    return PAW_TRUE;
}

static paw_Bool enter_const_decl(struct AstVisitor *V, struct AstConstDecl *d)
{
    struct Resolver *R = V->ud;
    if (R->fs == NULL)
        enter_fn(R, d->id, INVALID_DECL_ID);
    maybe_store_core_item(R, current_module(R)->id, d->ident.name, d->did);
    return PAW_TRUE;
}

static void leave_const_decl(struct AstVisitor *V, struct AstConstDecl *d)
{
    struct Resolver *R = V->ud;
    if (!DECL_ID_EXISTS(R->fs->did))
        leave_fn(R);
    PAW_UNUSED(d);
}

static paw_Bool resolve_field_decl(struct AstVisitor *V, struct AstFieldDecl *d)
{
    PAW_UNUSED(V);
    PAW_UNUSED(d);
    return PAW_TRUE;
}

static paw_Bool resolve_param_decl(struct AstVisitor *V, struct AstParamDecl *d)
{
    struct Resolver *R = V->ud;

    // NOTE: Function parameter is considered a variable even though it is
    //   "backed" by a declaration. In the context of the function, the
    //   parameter is a local variable.
    new_local_value(R, d->ident, d->id, SYMBOL_VAR);
    return PAW_TRUE;
}

static void resolve_bounds(struct AstVisitor *V, AstBoundList *bounds)
{
    struct Resolver *R = V->ud;
    K_LIST_XFOREACH (bounds, struct AstGenericBound const, pbound) {
        struct LookupResult const r = lookup_or_error(R, pbound->path, NAMESPACE_TYPE);
        struct AstDecl *decl = pawAst_get_node(R->ast, r.id);
        if (!AstIsTraitDecl(decl))
            RESOLVER_ERROR(R, ExpectedTrait,
                    .path = pawAst_print_type_path(R->ast, pbound->path),
                    .span = pbound->path.span);
        resolve_trait_args(V, r.id, pbound->path);

        struct AstTraitDecl *trait = AstGetTraitDecl(decl);
        struct AstSegment const last = K_LIST_LAST(pbound->path.segments);
        validate_type_args(R, kAstTraitDecl, trait->generics, last);
    }
}

static paw_Bool resolve_generic_decl(struct AstVisitor *V, struct AstGenericDecl *d)
{
    if (!d->is_type) {
        pawAst_visit_type(V, d->k.type);
    } else if (d->t.bounds != NULL) {
        resolve_bounds(V, d->t.bounds);
    }
    return PAW_FALSE;
}

static paw_Bool enter_match_arm(struct AstVisitor *V, struct AstMatchArm *e)
{
    PAW_UNUSED(e);
    enter_block(V->ud);
    return PAW_TRUE;
}

static void leave_match_arm(struct AstVisitor *V, struct AstMatchArm *e)
{
    PAW_UNUSED(e);
    leave_block(V->ud);
}

static paw_Bool enter_block_expr(struct AstVisitor *V, struct AstBlock *e)
{
    PAW_UNUSED(e);
    enter_block(V->ud);
    return PAW_TRUE;
}

static void leave_block_expr(struct AstVisitor *V, struct AstBlock *e)
{
    PAW_UNUSED(e);
    leave_block(V->ud);
}

static paw_Bool resolve_projection_expr(struct AstVisitor *V, struct AstProjectionExpr *e)
{
    struct Resolver *R = V->ud;

    if (AstIsPathType(e->type)) {
        struct LookupResult result;
        struct AstPathType const *path = AstGetPathType(e->type);
        if (!lookup(R, path->path, NAMESPACE_TYPE, &result))
            RESOLVER_ERROR(R, UnknownPath,
                    .path = pawAst_print_type_path(R->ast, path->path),
                    .span = e->span);

        struct AstDecl *self = pawAst_get_node(R->ast, result.id);
        if (AstIsTraitDecl(self))
            RESOLVER_ERROR(R, UnexpectedTrait,
                    .span = e->span);
    } else {
        pawAst_visit_type(V, e->type);
    }

    struct LookupResult const trait_res = lookup_or_error(R, e->trait, NAMESPACE_TYPE);
    struct AstDecl *trait = pawAst_get_node(R->ast, trait_res.id);
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
    enter_block(V->ud);
    return PAW_TRUE;
}

static void leave_for_expr(struct AstVisitor *V, struct AstForExpr *e)
{
    PAW_UNUSED(e);
    leave_block(V->ud);
}

static paw_Bool enter_fn_decl(struct AstVisitor *V, struct AstFnDecl *d)
{
    struct Resolver *R = V->ud;
    iscope_enter(R, d->id);
    enter_fn(R, d->id, d->did);

    declare_generics(R, d->generics);
    declare_type_aliases(R, d->id);
    maybe_store_core_item(R, current_module(R)->id, d->ident.name, d->did);
    return PAW_TRUE;
}

static void leave_fn_decl(struct AstVisitor *V, struct AstFnDecl *d)
{
    PAW_UNUSED(d);

    leave_fn(V->ud);
    iscope_leave(V->ud);
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
    iscope_enter(R, d->id);
    enter_scope(R, d->id, SCOPE_ADT);

    declare_generics(R, d->generics);
    declare_self(R, d->span, d->id, NAMESPACE_TYPE);

    maybe_store_core_item(R, current_module(R)->id, d->ident.name, d->did);
    maybe_store_builtin(R, current_module(R)->id, d->ident, d->id, d->did);
    return PAW_TRUE;
}

static void leave_adt_decl(struct AstVisitor *V, struct AstAdtDecl *d)
{
    PAW_UNUSED(d);
    leave_scope(V->ud);
    iscope_leave(V->ud);
}

static paw_Bool enter_trait_decl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    struct Resolver *R = V->ud;
    iscope_enter(R, d->id);
    enter_scope(R, d->id, SCOPE_TRAIT);

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
                R->ast, span, self_id, next_did(R), ident, d->supertraits);
        add_local(R, enclosing_scope(R), ident, self_id,
                NAMESPACE_TYPE, SYMBOL_DECL);

        pawAst_visit_decl(V, self);
    }

    maybe_store_core_item(R, current_module(R)->id, d->ident.name, d->did);
    maybe_store_core_trait(R, current_module(R)->id, d->ident.name, d->did, d->id);
    return PAW_TRUE;
}

static void leave_trait_decl(struct AstVisitor *V, struct AstTraitDecl *d)
{
    PAW_UNUSED(d);
    leave_scope(V->ud);
    iscope_leave(V->ud);
}

static paw_Bool enter_impl_decl(struct AstVisitor *V, struct AstImplDecl *d)
{
    struct Resolver *R = V->ud;
    iscope_enter(R, d->id);
    enter_scope(R, d->id, SCOPE_IMPL);

    declare_generics(R, d->generics);
    declare_self(R, d->span, d->id, NAMESPACE_TYPE);

    if (AstIsPathType(d->type)) {
        struct LookupResult r;
        // locate the context parameter definition
        struct AstPathType const *path = AstGetPathType(d->type);
        if (!lookup(R, path->path, NAMESPACE_TYPE, &r))
            unknown_path(R, path->path, NAMESPACE_TYPE);

        struct AstDecl *self = pawAst_get_node(R->ast, r.id);
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
    iscope_leave(V->ud);
}

static paw_Bool enter_type_decl(struct AstVisitor *V, struct AstTypeDecl *d)
{
    struct Resolver *R = V->ud;
    enter_scope(R, d->id, SCOPE_TYPE);

    declare_generics(R, d->generics);
    if (d->bounds != NULL)
        resolve_bounds(V, d->bounds);
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
    AstDeclList_push(R->ast, R->modstack, AST_CAST_DECL(d));
    iscope_enter(R, d->id);
    return PAW_TRUE;
}

static void leave_module_decl(struct AstVisitor *V, struct AstModuleDecl *d)
{
    PAW_UNUSED(d);
    struct Resolver *R = V->ud;
    AstDeclList_pop(R->modstack);
    iscope_leave(R);
}

static paw_Bool enter_closure_expr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    PAW_UNUSED(e);
    enter_fn(V->ud, e->id, e->did);
    declare_type_aliases(V->ud, e->id);
    return PAW_TRUE;
}

static void leave_closure_expr(struct AstVisitor *V, struct AstClosureExpr *e)
{
    PAW_UNUSED(e);
    leave_fn(V->ud);
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
    R.nonlexical_decls = NonLexicalDecls_new(&R),
    R.modstack = AstDeclList_new(R.ast),
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
    R.V->VisitConstDecl = enter_const_decl;
    R.V->PostVisitConstDecl = leave_const_decl;
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

