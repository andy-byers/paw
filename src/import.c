// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "api.h"
#include "ast.h"
#include "compile.h"
#include "debug.h"
#include "lib.h"
#include "map.h"

#define DLOG(I, ...) PAWD_LOG(ENV(I->C), __VA_ARGS__)

struct Importer {
    struct Compiler *C;
    Map *imports;
};

static int next_modno(struct Importer *I)
{
    return pawH_length(I->imports);
}

static struct Ast *get_import(struct Importer *I, const String *name)
{
    Value *pv = pawH_get(I->imports, P2V(name));
    return pv == NULL ? NULL : pv->p;
}

static void add_import(struct Importer *I, const String *name, struct Ast *ast)
{
    pawH_insert(ENV(I->C), I->imports, P2V(name), P2V(ast));
}

static void collect_imports_from(struct Importer *I, struct Ast *ast);

static int import_module(struct Importer *I, String *name)
{
    struct Ast *ast = get_import(I, name);
    if (ast != NULL) return ast->modno;

    DLOG(I, "importing '%s'", name->text);

    paw_Env *P = ENV(I->C);
    V_SET_OBJECT(P->top.p++, name);

    paw_Reader reader;
    void *state = pawL_start_import(P, &reader);
    ast = pawP_parse_module(I->C, name, reader, state);
    pawL_finish_import(P);

    add_import(I, name, ast);
    collect_imports_from(I, ast);
    return ast->modno;
}

static void collect_imports_from(struct Importer *I, struct Ast *ast)
{
    for (int i = 0; i < ast->items->count; ++i) {
        struct AstDecl *item = K_LIST_GET(ast->items, i);
        if (!AstIsUseDecl(item)) continue;
        struct AstUseDecl *use = AstGetUseDecl(item);
        use->modno = import_module(I, use->name);
    }
}

void pawP_collect_imports(struct Compiler *C, struct Ast *ast)
{
    struct Importer I = {
        .imports = C->imports,
        .C = C,
    };

    ENSURE_STACK(ENV(C), 1); // space for modname
    collect_imports_from(&I, ast);
}
