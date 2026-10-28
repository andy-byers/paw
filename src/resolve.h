// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

// TODO: create ImportScopes for closures to contain their use and const decls. prevents them from being hoisted to the top of containing fn

#ifndef PAW_RESOLVE_H
#define PAW_RESOLVE_H

#include "ast.h"

// Common state for name resolution routines
struct Resolver {
    struct Pool *pool;
    struct AstVisitor *V;
    struct Compiler *C;
    struct Ast *ast;
    paw_Env *P;

    struct FnState *fs;
    struct BlockState *bs;
    struct OrState const *os;

    struct NonLexicalDecls *nonlexical_decls;

    // stack for storing local symbols that obey lexical scoping and shadowing rules
    struct Symtab *symtab;

    // contains the scope object of the toplevel module of each imported package
    struct ImportScopes *imports;

    // mapping from names to imported package numbers
    struct ImportModules *modules;

    struct AstDeclList *modstack;
    struct SegmentTable *segtab;
    // TODO: only needed for import resolution. pull this and a few other things into a special context type for import resolution only
    NodeId current_scope;

    struct NodeList *const_ctx_stack;
};

NodeId pawR_enclosing_const_ctx(struct Resolver *R);
void pawR_enter_const_ctx(struct Resolver *R, NodeId const_id);
void pawR_leave_const_ctx(struct Resolver *R);

struct OrState {
    struct OrState const *outer;
    struct BoundNames *names;
    int alt_index;
};

struct UpvalueInfo {
    paw_Bool is_local;
    NodeId id;
    int index;
};

struct BoundName {
    struct SourceSpan span;
    NodeId id;
    int count;
};

DEFINE_MAP(struct Resolver, BoundNames, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, struct BoundName,)
DEFINE_MAP_ITERATOR(BoundNames, Str const *, struct BoundName)

enum ImportSymbolKind {
    ISYMBOL_EXPLICIT,
    ISYMBOL_GLOB,
};

struct ImportSymbol {
    paw_Bool is_pub;
    enum ImportSymbolKind kind;
    struct AstIdent ident;
    NodeId id;
};
#define ISYMBOL(Id_, Ident_, Kind_, IsPub_) ((struct ImportSymbol){ \
        .id = Id_, .ident = Ident_, .kind = Kind_, .is_pub = IsPub_})

struct ImportName {
    struct ImportSymbols *symbols;
};

enum ImportScopeKind {
    ISCOPE_MODULE,
    ISCOPE_BLOCK,
    ISCOPE_TYPE,
    ISCOPE_FN,
};

struct ImportScope {
    struct ImportNames *types;
    struct ImportNames *values;
    struct ImportScope *outer;
    enum ImportScopeKind kind;
    NodeId id;
};

DEFINE_LIST(struct Resolver, ImportSymbols, struct ImportSymbol,)
DEFINE_MAP(struct Resolver, ImportModules, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, int,)
DEFINE_MAP(struct Resolver, ImportScopes, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, struct ImportScope *,)
DEFINE_MAP(struct Resolver, ImportNames, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, struct ImportName *,)
DEFINE_MAP(struct Resolver, NonLexicalDecls, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, NodeList *,)
DEFINE_MAP_ITERATOR(ImportNames, Str const *, struct ImportName *)

enum ResolvedKind {
    RESOLVED_MODULE,
    RESOLVED_LOCAL,
    RESOLVED_UPVALUE,
    RESOLVED_DECL,
    RESOLVED_ASSOC,
};

struct ResolvedSegment {
    enum ResolvedKind kind;
    union {
        int modno;
        NodeId id;
        int upvalue;
    };
};

DEFINE_MAP(struct Compiler, SegmentTable, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, struct ResolvedSegment,)
DEFINE_MAP(struct Compiler, UpvalueTable, pawP_alloc, P_ID_HASH, P_ID_EQUALS, DeclId, struct UpvalueList *,)
DEFINE_LIST(struct Compiler, UpvalueList, struct UpvalueInfo,)

enum Namespace {
    // contains ADTs, traits, and type aliases
    NAMESPACE_TYPE,

    // contains functions (including constructors) and constants
    NAMESPACE_VALUE,
};

struct PathCursor {
    struct AstPath path;
    int index;
};

static struct PathCursor pc_create(struct AstPath path)
{
    return (struct PathCursor){.path = path};
}

static paw_Bool pc_is_valid(struct PathCursor pc)
{
    paw_assert(pc.path.segments->count > 0);
    return pc.index < pc.path.segments->count;
}

static paw_Bool pc_is_last(struct PathCursor pc)
{
    paw_assert(pc_is_valid(pc));
    return pc.index == pc.path.segments->count - 1;
}

static void pc_next(struct PathCursor *pc)
{
    ++pc->index;
}

static struct AstSegment *pc_segment(struct PathCursor pc)
{
    return &K_LIST_AT(pc.path.segments, pc.index);
}

static struct AstIdent pc_ident(struct PathCursor pc)
{
    return pc_segment(pc)->ident;
}

struct ImportSymbol const *pawP_find_import_symbol(struct Resolver *R, struct ImportScope const *scope, Str const *name, enum Namespace ns);
paw_Bool pawR_resolve_symbol(struct Resolver *R, struct ImportScope const *current, struct PathCursor pc, enum Namespace ns, struct ImportSymbol *out);

#define NS_NAME(Ns_) ((Ns_) == NAMESPACE_TYPE ? "type" : "value")

#endif // PAW_RESOLVE_H
