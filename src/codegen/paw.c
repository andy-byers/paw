// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdio.h>
#include <stdlib.h>

#include "paw.h"
#include "ast.h"
#include "hir.h"
#include "ir_type.h"
#include "lib.h"
#include "mir.h"

#include "codegen.h"

enum Status {
    STATUS_SUCCESS = 0,
    STATUS_INVALID_ARGUMENT = -1,
    STATUS_COMPILER_ERROR = -2,
    STATUS_NOT_ENOUGH_MEMORY = -3,
};

static void info(char const *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    vprintf(fmt, arg);
    va_end(arg);
}

_Noreturn static void error(int status, char const *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    vfprintf(stderr, fmt, arg);
    va_end(arg);
    exit(status);
}

static int on_build_ast(paw_Env *P)
{
    struct Ast *ast = paw_rawptr(P, 1);
    puts(pawAst_dump(ast));
    return 0;
}

static int on_build_hir(paw_Env *P)
{
    struct Hir *hir = paw_rawptr(P, 1);
    puts(pawHir_dump(hir));
    return 0;
}

static int on_build_mir(paw_Env *P)
{
    struct Mir *mir = paw_rawptr(P, 1);
    DeclId const did = IR_TYPE_DID(mir->type);
    if (did.modno != PRELUDE_MODNO)
        puts(pawMir_dump(mir));
    return 0;
}

// WARNING: each call to this function causes the previous result to be overwritten
static char const *pretty_size(size_t size)
{
#define KiB 1024.0

    static char buffer[64];

    int prec;
    double number;
    char const *units;
    if (size < KiB) {
        prec = 0;
        number = size;
        units = "B";
    } else if (size < KiB * KiB) {
        prec = 1;
        number = size / KiB;
        units = "KiB";
    } else if (size < KiB * KiB * KiB) {
        prec = 2;
        number = size / (KiB * KiB);
        units = "MiB";
    } else {
        prec = 3;
        number = size / (KiB * KiB * KiB);
        units = "GiB";
    }

    int const n = snprintf(buffer, sizeof(buffer), "%.*f %s", prec, number, units);
    paw_assert(0 < n && n < (int)sizeof(buffer));
    PAW_UNUSED(n); // for when NDEBUG is true
    return buffer;

#undef KiB
}

static int stats_reporter(paw_Env *P)
{
    struct Statistic const *const *pstat = paw_rawptr(P, 1);
    paw_Int const count = paw_int(P, 2);

    Buffer b;
    pawL_init_buffer(P, &b);
    pawL_add_fstring(P, &b, "==Stats===============\n");

    for (int i = 0; i < count; ++i) {
        struct Statistic const *stat = pstat[i];
        pawL_add_string(P, &b, stat->name);
        if (strstr(stat->name, "bytes")) {
            // statistic is an amount of memory
            char const *size = pretty_size(stat->value);
            pawL_add_fstring(P, &b, ": %s\n", size);
        } else {
            pawL_add_fstring(P, &b, ": %I\n", (paw_Int)stat->value);
        }
    }

    pawL_add_char(P, &b, '\n');
    pawL_push_result(P, &b);
    puts(paw_str(P, -1));

    paw_pop(P, 1);
    return 0;
}

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
        modlen = (size_t)(end - begin);
    memcpy(modname, begin, modlen);
    modname[modlen] = '\0';
}

int main(int argc, char const *argv[])
{
    static struct {
        paw_Bool ast;
        paw_Bool hir;
        paw_Bool mir;
        paw_Bool stats;
    } s_debug;

    if (argc < 2)
        error(STATUS_INVALID_ARGUMENT, "not enough arguments\n");

    char modname[PATH_MAX];
    char const *pathname = argv[1];
    path_to_modname(pathname, strlen(pathname), modname);

    paw_Env *P = paw_open(&(struct paw_Options){0});
    if (P == NULL)
        error(STATUS_NOT_ENOUGH_MEMORY, "not enough memory\n");

    if (s_debug.stats) {
        paw_push_str(P, "paw.stats_reporter");
        paw_new_native(P, stats_reporter, 0);
        paw_map_set(P, PAW_REGISTRY_INDEX);
    }
    if (s_debug.ast) {
        paw_push_str(P, "paw.on_build_ast");
        paw_new_native(P, on_build_ast, 0);
        paw_map_set(P, PAW_REGISTRY_INDEX);
    }
    if (s_debug.hir) {
        paw_push_str(P, "paw.on_build_hir");
        paw_new_native(P, on_build_hir, 0);
        paw_map_set(P, PAW_REGISTRY_INDEX);
    }
    if (s_debug.mir) {
        paw_push_str(P, "paw.on_build_mir");
        paw_new_native(P, on_build_mir, 0);
        paw_map_set(P, PAW_REGISTRY_INDEX);
    }

    // Load the source code and compile it using the existing frontend and the
    // new LLVM backend. Write the binary to a file called "\{modname}.out" in
    // the same directory as the pathname.
    int const status = pawL_load_file(P, modname, pathname);
    if (status != PAW_OK) error(status, "%s\n", paw_str(P, -1));

    paw_close(P);
}
