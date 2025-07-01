// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

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

    struct Symtab *symtab;
    struct ImportScope *parent;
    struct ImportScopes *imports;
    struct ImportModules *modules;
    struct AstModuleDecl *current;
    struct SegmentTable *segtab;
    int decl_count;
};

enum ImportSymbolKind {
    ISYMBOL_EXPLICIT,
    ISYMBOL_GLOB,
};

struct ImportSymbol {
    enum ImportSymbolKind kind;
    NodeId id;
};
#define ISYMBOL(Id_, Kind_) ((struct ImportSymbol){.id = Id_, .kind = Kind_})

struct ImportName {
    struct ImportSymbols *symbols;
};

enum ImportScopeKind {
    ISCOPE_MODULE,
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

DEFINE_LIST(struct Resolver, ImportSymbols, struct ImportSymbol)
DEFINE_MAP(struct Resolver, ImportModules, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, int)
DEFINE_MAP(struct Resolver, ImportScopes, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, struct ImportScope *)
DEFINE_MAP(struct Resolver, ImportNames, pawP_alloc, P_PTR_HASH, P_PTR_EQUALS, Str const *, struct ImportName *)
DEFINE_MAP_ITERATOR(ImportNames, Str const *, struct ImportName *)

enum ResolvedKind {
    RESOLVED_MODULE,
    RESOLVED_LOCAL,
    RESOLVED_DECL,
    RESOLVED_ASSOC,
};

struct ResolvedSegment {
    enum ResolvedKind kind;
    union {
        int modno;
        NodeId id;
    };
};

DEFINE_MAP(struct Compiler, SegmentTable, pawP_alloc, P_ID_HASH, P_ID_EQUALS, NodeId, struct ResolvedSegment)

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
    return pc.index + 1 == pc.path.segments->count;
}

static void pc_next(struct PathCursor *pc)
{
    ++pc->index;
}

static struct AstSegment *pc_segment(struct PathCursor pc)
{
    return &K_LIST_AT(pc.path.segments, pc.index);
}

#define NS_NAME(Ns_) ((Ns_) == NAMESPACE_TYPE ? "type" : "value")

#endif // PAW_RESOLVE_H
