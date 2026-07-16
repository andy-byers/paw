// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"
#include <stdlib.h>


#include "api.h"
#include "compile.h"
#include "env.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "os.h"
#include "parse.h"
#include "core.h"
#include "str.h"


static void *default_alloc(void *ud, void *ptr, size_t old_size, size_t new_size)
{
    PAW_UNUSED(ud);
    if (new_size == 0) {
        free(ptr);
        return NULL;
    }
    if (old_size == 0)
        return malloc(new_size);
    return realloc(ptr, new_size);
}

void *paw_context(paw_Env const *P)
{
    return P->ud;
}

size_t paw_bytes_used(paw_Env const *P)
{
    return P->num_bytes;
}

static int open_aux(paw_Env *P, void *arg)
{
//TODO    pawC_throw(P, -1);
    PAW_UNUSED(arg);

    int const FIRST_ARENA_SIZE = 4096;
    P->pool = pawM_new(P, struct Pool);
    P->pool->prev = P->pool->next = P->pool;
    pawK_pool_init(P, P->pool, FIRST_ARENA_SIZE, (struct PoolStats){0});

    P->C = pawM_new(P, struct Compiler);
    P->C->pool = P->pool;
    P->C->P = P;

    pawE_init(P);
    pawS_init(P);
    pawP_init(P);
    pawL_init(P);

    P->mem_errmsg = pawS_new_str(P, "out of memory");
    return 0;
}

paw_Env *paw_open(struct paw_Options const *o)
{
#define OR_DEFAULT(a, b) ((a) ? (a) : (b))

    void *ud = OR_DEFAULT(o->ud, NULL);
    paw_Alloc alloc = OR_DEFAULT(o->alloc, default_alloc);

    paw_Env *P = alloc(ud, NULL, 0, sizeof *P);
    *P = (paw_Env){
        .options = *o,
        .alloc = alloc,
        .ud = ud,
    };

    if (pawC_try(P, open_aux, NULL)) {
        paw_close(P);
        return NULL;
    }
    return P;

#undef OR_DEFAULT
}

void paw_close(paw_Env *P)
{
    pawL_uninit(P);
    pawE_uninit(P);
    pawS_uninit(P);

    pawK_pool_uninit(P->pool);
    pawM_free(P, P->pool);
    pawM_free(P, P->C);
}

struct CompileState {
    paw_Reader input;
    struct DynamicMem dm;
    char const *modname;
    char const *dirname;
    char const *pathname;
    void *ud;
};

static int compile_aux(paw_Env *P, void *arg)
{
    struct CompileState *p = arg;

    Str const *modname = pawS_new_str(P, p->modname);
    Str const *pathname = p->pathname != NULL ? pawS_new_str(P, p->pathname) : modname;
    Str const *dirname = pawS_new_str(P, p->dirname != NULL ? p->dirname : ".");
    pawP_startup(P, P->C, &p->dm, modname, pathname, dirname);
    pawP_compile(P->C, p->input, p->ud);
    return 0;
}

int paw_load(paw_Env *P, paw_Reader input, char const *modname, char const *dirname, char const *pathname, void *ud)
{
    struct CompileState p = {
        .input = input,
        .modname = modname,
        .dirname = dirname,
        .pathname = pathname,
        .ud = ud,
    };
    int const status = pawC_try(P, compile_aux, &p);
    pawP_teardown(P, &p.dm);
    return status;
}
