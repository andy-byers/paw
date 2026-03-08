// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "compile.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "mem.h"
#include "os.h"

struct SearcherState {
    struct FileReader *fr;
    Str const *name;
};

static char const *file_reader(paw_Env *P, void *ud, size_t *psize)
{
    struct FileReader *fr = ud;
    size_t const zchunk = sizeof(fr->data);
    *psize = pawO_read(P, fr->file, fr->data, zchunk);
    return *psize > 0 ? fr->data : NULL;
}

struct ChunkReader {
    paw_Reader f;
    char const *data;
    size_t size;
};

static char const *chunk_reader(paw_Env *P, void *ud, size_t *psize)
{
    PAW_UNUSED(P);
    struct ChunkReader *cr = ud;
    *psize = cr->size;
    cr->size = 0;
    return cr->data;
}

void pawL_close_loader(paw_Env *P, void *state)
{
    if (state == NULL)
        return;
    struct FileReader *fr = state;
    pawO_close(fr->file);
    pawO_free_file(P, fr->file);
}

static char const *lib_getenv(paw_Env *P)
{
    PAW_UNUSED(P);
    return getenv(PAW_PATH_VAR);
}

paw_Bool pawL_is_std_name(char const *name)
{
    for (int i = 0; i < PAWL_NUM_STD_MODULES; ++i) {
        if (strcmp(name, pawL_StdNames[i]) == 0)
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

static paw_Bool matches_modname(Str const *lhs, char const *rhs)
{
    return strncmp(rhs, lhs->text, lhs->length) == 0;
}

static int open_source_file(paw_Env *P, Str const *dirname, Str const *filename, struct FileReader *result)
{
    Buffer b;
    pawL_init_buffer(P, &b);
    if (dirname != NULL) {
        L_ADD_STRING(P, &b, dirname);
        pawL_add_char(P, &b, PAW_FOLDER_SEPS[0]);
    }
    L_ADD_STRING(P, &b, filename);
    Str const *pathname = pawL_buffer_finish(P, &b);

    // TODO: clean up on error, no more GC in compiler!
    File *file = pawO_new_file(P);
    int const rc = pawO_open(file, pathname->text, "r");
    if (rc == -ENOENT) {
        return 0;
    } else if (rc < 0) {
        pawO_error(P);
    }

    result->f = file_reader;
    result->file = pawO_detach_file(P, file);
    result->dirname = dirname;
    result->pathname = pathname;
    return 1;
}

static int searcher_cwd(paw_Env *P, void *arg)
{
    struct SearcherState *state = arg;

    Buffer b;
    pawL_init_buffer(P, &b);
    L_ADD_STRING(P, &b, state->name);
    L_ADD_LITERAL(P, &b, PAW_MODULE_EXT);
    Str const *filename = pawL_buffer_finish(P, &b);
    return open_source_file(P, P->C->dirname, filename, state->fr);
}

static struct FileReader new_file_reader(paw_Env *P, char const *dirname, char const *filename)
{
    struct FileReader fr;
    Str const *dir = pawS_new_str(P, dirname);
    Str const *file = pawS_new_str(P, filename);
    int const found = open_source_file(P, dir, file, &fr);
    paw_assert(found); PAW_UNUSED(found); // must exist
    return fr;
}

static int searcher_Paw(paw_Env *P, void *arg)
{
#define CREATE_MATCHER(Name_) if (matches_modname(state->name, Name_)) { \
        *state->fr = new_file_reader(P, PAW_STDLIB_PATH, Name_ PAW_MODULE_EXT); \
        return 1; \
    }

    struct SearcherState *state = arg;
    CREATE_MATCHER(PAWL_PRELUDE_NAME)
    CREATE_MATCHER(PAWL_OPS_NAME)
    CREATE_MATCHER(PAWL_LIST_NAME)
    CREATE_MATCHER(PAWL_MAP_NAME)
    CREATE_MATCHER(PAWL_SLICE_NAME)
    CREATE_MATCHER(PAWL_OPTION_NAME)
    CREATE_MATCHER(PAWL_RESULT_NAME)
    CREATE_MATCHER(PAWL_UNSAFE_NAME)
    CREATE_MATCHER(PAWL_IO_NAME)
    CREATE_MATCHER(PAWL_MATH_NAME)
    CREATE_MATCHER(PAWL_STR_BUILDER_NAME)
    return 0;

#undef CREATE_MATCHER
}

static int search_pathlist(paw_Env *P, char const *p, struct SearcherState *state)
{
    while (p != NULL) {
        char const *sep = strstr(p, PAW_PATH_SEP);
        Str const *dirname;
        if (sep != NULL) {
            dirname = pawS_new_nstr(P, p, (size_t)(sep - p));
            p = sep + PAW_LENGTHOF(PAW_PATH_SEP);
        } else {
            dirname = pawS_new_str(P, p);
            p = NULL;
        }

        Buffer b;
        pawL_init_buffer(P, &b);
        L_ADD_STRING(P, &b, state->name);
        L_ADD_LITERAL(P, &b, PAW_MODULE_EXT);
        Str const *filename = pawL_buffer_finish(P, &b);
        if (open_source_file(P, dirname, filename, state->fr))
            return 1;
    }
    return 0;
}

static int searcher_env(paw_Env *P, void *arg)
{
    return search_pathlist(P, lib_getenv(P), arg);
}

static int searcher_inc(paw_Env *P, void *arg)
{
    return search_pathlist(P, P->options.include_paths, arg);
}

void pawL_init(paw_Env *P)
{
    StrMap *registry = StrMap_new_from(NULL, P->pool);
    StringMap *symbols = StringMap_new_from(NULL, P->pool);
    StringMap *modules = StringMap_new_from(NULL, P->pool);

    paw_Function const SEARCHERS[] = {
        searcher_Paw, // check standard library
        searcher_cwd, // check current working directory
        searcher_inc, // check "-I" option argument
        searcher_env, // check PAW_PATH
    };
    Searchers *searchers = Searchers_new(P->C);
    for (unsigned i = 0; i < PAW_COUNTOF(SEARCHERS); ++i)
        Searchers_push(P->C, searchers, SEARCHERS[i]);

    StrMap_insert(NULL, registry, CACHED_STRING(P, CSTR_KSEARCHERS), searchers);
    StrMap_insert(NULL, registry, CACHED_STRING(P, CSTR_KSYMBOLS), symbols);
    StrMap_insert(NULL, registry, CACHED_STRING(P, CSTR_KMODULES), modules);
    P->registry = registry;
}

void pawL_uninit(paw_Env *P)
{
    PAW_UNUSED(P);
}

int pawL_load_file(paw_Env *P, char const *name, char const *pathname, char const *cwd)
{
    struct FileReader fr = {
        .file = pawO_new_file(P),
        .f = file_reader,
    };
    int const rc = pawO_open(fr.file, pathname, "r");
    if (rc == 0) {
        int const status = paw_load(P, file_reader, name, cwd, pathname, &fr);
        if (!fr.err) return status;
    }
    P->current_errmsg = pawS_new_str(P, strerror(errno));
    return PAW_ESYSTEM;
}

int pawL_load_nchunk(paw_Env *P, char const *name, char const *source, size_t length)
{
    struct ChunkReader cr = {
        .f = chunk_reader,
        .data = source,
        .size = length,
    };
    return paw_load(P, chunk_reader, name, NULL, NULL, &cr);
}

int pawL_load_chunk(paw_Env *P, char const *name, char const *source)
{
    return pawL_load_nchunk(P, name, source, strlen(source));
}

static char const *file_import_reader(paw_Env *P, void *ud, size_t *psize)
{
    PAW_UNUSED(P);
    struct FileReader *fr = ud;
    if (fr->err) {
        *psize = 0;
        return NULL;
    }
    *psize = pawO_read(P, fr->file, fr->data, sizeof(fr->data));
    return *psize > 0 ? fr->data : NULL;
}

int pawL_start_import(paw_Env *P, Str const *name, struct FileReader *result)
{
    struct Compiler *C = P->C;
    paw_Function const *psearcher;
    K_LIST_FOREACH (C->searchers, psearcher) {
        struct SearcherState state = {
            .name = name,
            .fr = result,
        };
        int const status = (*psearcher)(P, &state);
        if (status > 0) {
            return IMPORT_FOUND;
        } else if (status < 0) {
            pawC_throw(P, -status);
        }
    }
    return IMPORT_NOT_FOUND;
}

void pawL_finish_import(paw_Env *P, struct FileReader *fr)
{
    PAW_UNUSED(P);

    pawO_close(fr->file);
}
