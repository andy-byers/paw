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
    StringMap *aliases;
};

struct Importer {
    struct Pool *pool;
    struct ImportContext *ctx;
    struct Compiler *C;
    ImportMap *imports;
    paw_Env *P;
    int line; // TODO: never set
};

static void enter_context(struct Importer *I, struct ImportContext *ctx)
{
    *ctx = (struct ImportContext){
        .aliases = StringMap_new_from(I->C, I->pool),
        .outer = I->ctx,
    };
    I->ctx = ctx;
}

static void leave_context(struct Importer *I)
{
    StringMap_delete(I->C, I->ctx->aliases);
    I->ctx = I->ctx->outer;
}

static int next_modno(struct Importer *I)
{
    return CAST(int, ImportMap_length(I->imports));
}

static String *module_name(struct Importer *I, String *name)
{
    String *const *palias = StringMap_get(I->C, I->ctx->aliases, name);
    return palias == NULL ? name : *palias;
}

static struct Ast *get_import(struct Importer *I, String *name)
{
    struct Ast *const *past = ImportMap_get(I->C, I->imports, name);
    return past == NULL ? NULL : *past;
}

static void add_import(struct Importer *I, String *name, struct Ast *ast)
{
    ImportMap_insert(I->C, I->imports, name, ast);
}

static void collect_imports_from(struct Importer *I, struct Ast *ast);

static int import_module(struct Importer *I, String *name, String *as)
{
    name = module_name(I, name);
    struct Ast *ast = get_import(I, name);
    if (ast != NULL)
        return ast->modno;
    as = as != NULL ? as : name;

    DLOG(I, "importing '%s' as '%s'", name->text, as->text);

    paw_Env *P = ENV(I->C);
    ptrdiff_t const saved = SAVE_OFFSET(P, P->top.p);
    V_SET_OBJECT(P->top.p++, name);

    struct LoaderState *state = pawL_start_import(P);
    if (state == NULL)
        NAME_ERROR(I, "module '%s' not found", name->text);
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

    // register aliases
    K_LIST_FOREACH (ast->items, pitem) {
        struct AstDecl *item = *pitem;
        if (!AstIsUseDecl(item))
            continue;
        struct AstUseDecl *use = AstGetUseDecl(item);
        if (use->item == NULL && use->as != NULL) {
            StringMap_insert(I->C, ctx.aliases, use->as, use->name);
        }
    }

    // import modules
    K_LIST_FOREACH (ast->items, pitem) {
        struct AstDecl *item = *pitem;
        if (!AstIsUseDecl(item))
            continue;
        struct AstUseDecl *use = AstGetUseDecl(item);
        use->modno = import_module(I, use->name,
                                   use->item == NULL ? use->as : NULL);
    }

    leave_context(I);
}

void pawP_collect_imports(struct Compiler *C, struct Ast *ast)
{
    struct Importer I = {
        .pool = pawP_pool_new(C, C->aux_stats),
        .imports = C->imports,
        .P = ENV(C),
        .C = C,
    };

    ENSURE_STACK(ENV(C), 1); // space for modname
    collect_imports_from(&I, ast);
    pawP_pool_free(C, I.pool);
}
