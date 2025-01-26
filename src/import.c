// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "api.h"
#include "ast.h"
#include "compile.h"
#include "debug.h"
#include "lib.h"
#include "map.h"

struct ImportContext {
    struct ImportContext *outer;
    Map *aliases;
};

struct Importer {
    struct ImportContext *ctx;
    struct Compiler *C;
    Map *imports;
    paw_Env *P;
    int line; // TODO: never set
};

static void enter_context(struct Importer *I, struct ImportContext *ctx)
{
    *ctx = (struct ImportContext){
        .aliases = pawP_push_map(I->C),
        .outer = I->ctx,
    };
    I->ctx = ctx;
}

static void leave_context(struct Importer *I)
{
    pawP_pop_object(I->C, I->ctx->aliases);
    I->ctx = I->ctx->outer;
}

static int next_modno(struct Importer *I)
{
    return CAST(int, pawH_length(I->imports));
}

static String *module_name(struct Importer *I, String *name)
{
    const Value *pv = pawH_get(I->ctx->aliases, P2V(name));
    return pv == NULL ? name : pv->p;
}

static struct Ast *get_import(struct Importer *I, const String *name)
{
    const Value *pv = pawH_get(I->imports, P2V(name));
    return pv == NULL ? NULL : pv->p;
}

static void add_import(struct Importer *I, const String *name, struct Ast *ast)
{
    MAP_INSERT(I, I->imports, P2V(name), P2V(ast));
}

static void collect_imports_from(struct Importer *I, struct Ast *ast);

static int import_module(struct Importer *I, String *name, String *as)
{
    name = module_name(I, name);
    struct Ast *ast = get_import(I, name);
    if (ast != NULL) return ast->modno;
    as = as != NULL ? as : name;

    DLOG(I, "importing '%s' as '%s'", name->text, as->text);

    paw_Env *P = ENV(I->C);
    const ptrdiff_t saved = SAVE_OFFSET(P, P->top.p);
    V_SET_OBJECT(P->top.p++, name);

    struct LoaderState *state = pawL_start_import(P);
    if (state == NULL) NAME_ERROR(I, "module '%s' not found", name->text);
    ast = pawP_parse_module(I->C, name, state->f, state);
    pawL_finish_import(P);

    P->top.p = RESTORE_POINTER(P, saved);
    add_import(I, name, ast);
    collect_imports_from(I, ast);
    return ast->modno;
}

static void collect_imports_from(struct Importer *I, struct Ast *ast)
{
    struct AstDecl **pitem;
    struct ImportContext ctx;
    enter_context(I, &ctx);

    K_LIST_FOREACH(ast->items, pitem) {
        struct AstDecl *item = *pitem;
        if (!AstIsUseDecl(item)) continue;
        struct AstUseDecl *use = AstGetUseDecl(item);
        if (use->item == NULL && use->as != NULL) {
            MAP_INSERT(I, ctx.aliases, P2V(use->as), P2V(use->name));
        }
    }

    K_LIST_FOREACH(ast->items, pitem) {
        struct AstDecl *item = *pitem;
        if (!AstIsUseDecl(item)) continue;
        struct AstUseDecl *use = AstGetUseDecl(item);
        use->modno = import_module(I, use->name,
                use->item == NULL ? use->as : NULL);
    }

    leave_context(I);
}

void pawP_collect_imports(struct Compiler *C, struct Ast *ast)
{
    struct Importer I = {
        .imports = C->imports,
        .P = ENV(C),
        .C = C,
    };

    ENSURE_STACK(ENV(C), 1); // space for modname
    collect_imports_from(&I, ast);
}

