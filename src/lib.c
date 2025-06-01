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
#include "gc.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "mem.h"
#include "os.h"
#include "rt.h"

// TODO: avoid using an upper bound on path length, or at least make sure not to overflow
//       the automatic buffers used to hold paths in this file
#if defined(PAW_OS_POSIX)
#define PAW_PATH_MAX PATH_MAX
#else
#define PAW_PATH_MAX 2048
#endif

char const *find_last_sep(char const *s, size_t n, size_t *pn)
{
    paw_assert(n > 0);
    char const *s0 = s;
    s += n;
    *pn = 0;

    do {
        --s;
        ++*pn;
        char const *q = PAW_FOLDER_SEPS;
        while (*q != '\0') {
            if (*q++ == *s)
                return s;
        }
    } while (s != s0);
    return NULL;
}

static void path_to_modname(char const *pathname, size_t pathlen, char *modname)
{
    size_t modlen;
    char const *begin = find_last_sep(pathname, pathlen, &modlen);
    if (begin != NULL) {
        // skip separator
        --modlen;
        ++begin;
    } else {
        begin = pathname;
        modlen = pathlen;
    }
    char const *end = strchr(begin, '.');
    if (end != NULL)
        modlen = end - begin;
    memcpy(modname, begin, modlen);
    modname[modlen] = '\0';
}

struct FileReader {
    struct LoaderState state;
    char data[512];
    File *file;
    paw_Bool err;
};

static char const *file_reader(paw_Env *P, void *ud, size_t *psize)
{
    struct FileReader *fr = ud;
    size_t const zchunk = sizeof(fr->data);
    *psize = pawO_read(P, fr->file, fr->data, zchunk);
    // TODO: don't throw errors in os.c    if (*psize != zchunk) fr->err = ferror(fr->file);
    return *psize > 0 ? fr->data : NULL;
}

struct ChunkReader {
    struct LoaderState state;
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

void pawL_new_func(paw_Env *P, paw_Function func, int nup)
{
    Value *pv = pawC_push0(P);
    Native *nat = pawV_new_native(P, func, nup);
    V_SET_OBJECT(pv, nat);

    Value const *up = P->top.p - nup - 1;
    for (int i = 0; i < nup; ++i) {
        nat->up[i] = *up++;
    }
    paw_shift(P, nup);
}

void pawL_push_symbols_map(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);
}

void pawL_push_modules_map(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KMODULES);
    paw_map_get(P, PAW_REGISTRY_INDEX);
}

static paw_Bool lib_getenv(paw_Env *P)
{
    char const *env = getenv(paw_str(P, -1));
    paw_pop(P, 1);

    if (env == NULL)
        return PAW_FALSE;
    paw_push_str(P, env);
    return PAW_TRUE;
}

static paw_Bool matches_modname(paw_Env *P, char const *modname)
{
    return strncmp(modname, paw_str(P, -1), paw_str_rawlen(P, -1)) == 0;
}

static struct FileReader *new_file_reader(paw_Env *P, char const *pathname)
{
    File *file = pawO_new_file(P);
    int const rc = pawO_open(file, pathname, "r");
    if (rc == -ENOENT) {
        paw_pop(P, 1);
        return NULL;
    } else if (rc < 0) {
        pawO_error(P);
    }

    struct FileReader *r = paw_new_foreign(P, sizeof(struct FileReader), 0);
    Foreign *f = V_FOREIGN(P->top.p[-1]);
    r->file = pawO_detach_file(P, file);
    r->state.f = file_reader;
    f->flags = VBOX_LOADER;
    paw_shift(P, 1);

    return r;
}

static int searcher_Paw(paw_Env *P)
{
    void l_import_io(paw_Env * P);
    void l_import_math(paw_Env * P);
    void l_import_prelude(paw_Env *P);

    if (matches_modname(P, PAWL_IO_NAME)) {
        l_import_io(P);
    } else if (matches_modname(P, PAWL_MATH_NAME)) {
        l_import_math(P);
    } else if (matches_modname(P, PAWL_PRELUDE_NAME)) {
        l_import_prelude(P);
    } else {
        paw_push_zero(P, 1);
    }
    return 1;
}

static int searcher_cwd(paw_Env *P)
{
    if (P->modname != NULL) {
        char const *pathname = P->modname->text;
        size_t const pathlen = P->modname->length;
        paw_assert(pathlen <= PAW_PATH_MAX); // TODO: throw an error at least

        size_t modlen;
        char const *sep = find_last_sep(pathname, pathlen, &modlen);
        if (sep == NULL)
            goto use_current_dir;
        paw_push_nstr(P, pathname, sep - pathname + 1);
    } else {
    use_current_dir:;
        char prefix[] = {'.', PAW_FOLDER_SEPS[0], '\0'};
        paw_push_str(P, prefix);
    }
    paw_rotate(P, -2, 1);
    PAW_PUSH_LITERAL(P, PAW_MODULE_EXT);
    paw_str_concat(P, 3);

    struct FileReader *fr = new_file_reader(P, paw_str(P, -1));
    if (fr == NULL)
        paw_push_zero(P, 1); // indicate failure

    return 1;
}

static void push_prelude_method(paw_Env *P, char const *self, char const *name)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    paw_mangle_start(P);
    paw_push_str(P, self);
    paw_mangle_add_name(P);
    paw_push_str(P, name);
    paw_mangle_add_name(P);

    paw_map_get(P, -2);
    paw_shift(P, 1);
}

static int searcher_env(paw_Env *P)
{
    PAW_PUSH_LITERAL(P, PAW_PATH_VAR);
    if (lib_getenv(P)) {
        // split on the path separator
        push_prelude_method(P, "str", "split");
        paw_rotate(P, -2, 1);
        PAW_PUSH_LITERAL(P, PAW_PATH_SEP);
        paw_call(P, 2);

        paw_push_int(P, PAW_ITER_INIT);
        while (paw_list_next(P, -2)) {
            // path + sep + modname + ext
            paw_push_nstr(P, PAW_FOLDER_SEPS, 1);
            paw_push_value(P, 1); // modname
            PAW_PUSH_LITERAL(P, PAW_MODULE_EXT);
            paw_str_concat(P, 4);

            struct FileReader *fr = new_file_reader(P, paw_str(P, -1));
            if (fr != NULL)
                return 1;

            paw_pop(P, 1);
        }
    }
    paw_push_zero(P, 1);
    return 1;
}

static int init_searchers(paw_Env *P)
{
    paw_Function const fs[] = {
        searcher_Paw, // check stdlib first
        searcher_cwd,
        searcher_env,
    };
    for (int i = 0; i < PAW_COUNTOF(fs); ++i) {
        pawL_new_func(P, fs[i], 0);
    }
    return PAW_COUNTOF(fs);
}

#define NBUILTIN_POLICIES 5

void pawL_init(paw_Env *P)
{
    // create a map policy to use during compilation
    P->map_policies.data[P->map_policies.count++] = (MapPolicy){
        .key_size = 1,
        .value_size = 1,
        .type = -1, // untyped
    };

    // create system registry objects
    pawE_push_cstr(P, CSTR_KSEARCHERS);
    paw_new_list(P, init_searchers(P), 1);
    pawE_push_cstr(P, CSTR_KMODULES);
    paw_new_map(P, 0, 0);
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_new_map(P, 0, 0);

    // create the registry itself
    paw_new_map(P, 3, 0);
    P->registry = P->top.p[-1];
    paw_pop(P, 1);
}

void pawL_uninit(paw_Env *P)
{
    PAW_UNUSED(P);
}

int pawL_load_file(paw_Env *P, char const *pathname)
{
    struct FileReader fr = {
        .file = pawO_new_file(P),
        .state.f = file_reader,
    };
    int const rc = pawO_open(fr.file, pathname, "r");
    if (rc == 0) {
        char modname[PAW_PATH_MAX + 1];
        size_t const pathlen = strlen(pathname);
        paw_assert(pathlen <= PAW_PATH_MAX); // TODO
        path_to_modname(pathname, pathlen, modname);
        int const status = paw_load(P, file_reader, modname, &fr);
        if (!fr.err)
            return status;
        // TODO: just return status??
    }
    paw_push_str(P, strerror(errno));
    return PAW_ESYSTEM;
}

int pawL_load_nchunk(paw_Env *P, char const *name, char const *source, size_t length)
{
    struct ChunkReader cr = {
        .state.f = chunk_reader,
        .data = source,
        .size = length,
    };
    return paw_load(P, chunk_reader, name, &cr);
}

int pawL_load_chunk(paw_Env *P, char const *name, char const *source)
{
    return pawL_load_nchunk(P, name, source, strlen(source));
}

void pawL_load_symbols(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    paw_push_int(P, PAW_ITER_INIT);
    // at the top of the loop body the stack looks like:
    //     .. symbols paw.symbols i key value
    while (paw_map_next(P, -3)) {
        // paw.symbols[key] = value
        paw_map_set(P, -4);
        paw_pop(P, 2);
    }
    paw_pop(P, 3);
}

int pawL_register_func(paw_Env *P, char const *name, paw_Function func, int nup)
{
    // paw.symbols[mangle(name)] = func
    pawL_push_symbols_map(P);
    paw_mangle_start(P);
    paw_push_str(P, name);
    paw_mangle_add_name(P);
    paw_new_native(P, func, nup);
    paw_map_set(P, -3);
    return 0;
}

void *pawL_chunk_reader(paw_Env *P, char const *text, size_t length)
{
    struct ChunkReader *r = paw_new_foreign(P, sizeof(struct ChunkReader), 0);
    *r = (struct ChunkReader){
        .state.f = chunk_reader,
        .size = length,
        .data = text,
    };
    return r;
}

void *pawL_file_reader(paw_Env *P, char const *pathname)
{
    return new_file_reader(P, pathname);
//    struct FileReader *r = paw_new_foreign(P, sizeof(struct FileReader), 0);
//    *r = (struct FileReader){
//        .file = pawO_new_file(P),
//        .state.f = file_reader,
//    };
//    int const rc = pawO_open(r->file, pathname, "r");
//    if (rc != 0) {
//        paw_pop(P, 1); // pop foreign object
//        paw_push_str(P, strerror(errno));
//        return NULL;
//    }
//    return r;
}

void pawL_add_extern_value(paw_Env *P, char const *modname, char const *name)
{
    paw_mangle_start(P);
    paw_push_str(P, modname);
    paw_mangle_add_module(P);
    paw_push_str(P, name);
    paw_mangle_add_name(P);
    paw_rotate(P, -2, 1);
    paw_map_set(P, -3);
}

void pawL_add_extern_func(paw_Env *P, char const *modname, char const *name, paw_Function func)
{
    paw_mangle_start(P);
    paw_push_str(P, modname);
    paw_mangle_add_module(P);
    paw_push_str(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
}

void pawL_add_extern_method(paw_Env *P, char const *modname, char const *self, char const *name, paw_Function func)
{
    paw_mangle_start(P);
    paw_push_str(P, modname);
    paw_mangle_add_module(P);
    paw_push_str(P, self);
    paw_mangle_add_name(P);
    paw_push_str(P, name);
    paw_mangle_add_name(P);
    pawL_new_func(P, func, 0);
    paw_map_set(P, -3);
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
    // TODO    if (*psize != sizeof(fr->data)) fr->err = ferror(fr->file);
    return *psize > 0 ? fr->data : NULL;
}

struct LoaderState *pawL_start_import(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSEARCHERS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    paw_push_int(P, PAW_ITER_INIT);
    // .. name searchers iter f
    while (paw_list_next(P, -2)) {
        paw_push_value(P, -4); // name
        int const status = paw_call(P, 1); // state = f(name)
        if (status != PAW_OK)
            pawC_throw(P, status);
        if (paw_rawptr(P, -1) != NULL) {
            paw_shift(P, 3);
            return paw_pointer(P, -1);
        }
        paw_pop(P, 1);
    }
    paw_pop(P, 3);
    return NULL;
}

void pawL_finish_import(paw_Env *P)
{
    paw_pop(P, 1);
}
