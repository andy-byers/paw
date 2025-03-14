// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "compile.h"
#include "prefix.h"

#include "env.h"
#include "lib.h"
#include "paw.h"
#include "stats.h"
#include "util.h"

// compiler API for printing trees
#include "ast.h"
#include "hir.h"
#include "ir_type.h"
#include "mir.h"

#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// clang-format off
#define PROGRAM_OPTIONS \
    OPT_STR(e, src, "execute program passed as string") \
    OPT_STR(d, debug, "debug compilation phase") \
    OPT_INT(H, log_heap, "log of default heap size in bytes") \
    OPT_OPT(h, "display usage information") \
    OPT_OPT(c, "compile the program only") \
    OPT_OPT(q, "suppress output")

static struct {
#define OPT_STR(name, a, b) const char *name;
#define OPT_INT(name, a, b) int name;
#define OPT_OPT(name, a) paw_Bool name;
    PROGRAM_OPTIONS
#undef OPT_OPT
#undef OPT_INT
#undef OPT_STR
} s_opt;

static const char *s_pathname;
static const char *s_progname;
static struct {
    paw_Bool ast;
    paw_Bool hir;
    paw_Bool mir;
    paw_Bool stats;
} s_debug;

static struct Option {
    const char *name;
    const char *argname;
    const char **string;
    int *integer;
    paw_Bool *flag;
    const char *description;
} s_opt_info[] = {
#define OPT_STR(name, arg, help) \
    {#name, #arg, &s_opt.name, NULL, NULL, help},
#define OPT_INT(name, arg, help) \
    {#name, #arg, NULL, &s_opt.name, NULL, help},
#define OPT_OPT(name, help) \
    {#name, NULL, NULL, NULL, &s_opt.name, help},
    PROGRAM_OPTIONS
#undef OPT_OPT
#undef OPT_INT
#undef OPT_STR
};
// clang-format on

static void info(char const *fmt, ...)
{
    if (!s_opt.q) {
        va_list arg;
        va_start(arg, fmt);
        vprintf(fmt, arg);
        va_end(arg);
    }
}

_Noreturn static void error(int status, char const *fmt, ...)
{
    if (!s_opt.q) {
        va_list arg;
        va_start(arg, fmt);
        vfprintf(stderr, fmt, arg);
        va_end(arg);
    }
    exit(status);
}

#define GETOPT(c, v) (--c, ++v, v[-1])

// Parse commandline options
// Adjusts 'argv' to point to the first argument to the paw script, and
// sets 'argc' to the number of such arguments.
static void parse_options(int *pargc, char const ***pargv)
{
    int argc = *pargc;
    char const **argv = *pargv;
    s_progname = GETOPT(argc, argv);
    while (argc) {
        struct Option *state;
        char const *option = GETOPT(argc, argv);
        char const *a = option;
        if (a[0] != '-') {
            // Found a script pathname (the only non-option argument).
            s_pathname = option;
            break;
        } else if (a[1] == '-' && a[2] == '\0') {
            // Found '--': the rest of the arguments go to the script.
            break;
        }
        for (++a; *a; ++a) {
            char const shr = *a;
            for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
                state = &s_opt_info[i];
                if (shr == state->name[0]) {
                    if (state->flag != NULL) {
                        *state->flag = PAW_TRUE;
                        break; // no argument
                    }
                    if (a[1] != '\0') // in '-abc', only 'c' can take an argument
                        error(PAW_ERUNTIME, "option with argument ('%c') must be last\n", shr);

                    if (*pargc == 0)
                        error(PAW_ERUNTIME, "missing argument for option '%s'\n", *(*pargv - 1));

                    char const *arg = GETOPT(argc, argv);
                    if (state->integer != NULL) {
                        int value = 0;
                        for (char const *p = arg; *p; ++p) {
                            int const v = *p - '0';
                            if (v < 0 || 9 < v)
                                error(PAW_ERUNTIME, "invalid integer argument (%s)\n", arg);
                            if (value > (INT_MAX - v) / 10)
                                error(PAW_ERUNTIME, "integer argument (%s) is too large\n", arg);
                            value = value * 10 + v;
                        }
                        *state->integer = value;
                    } else {
                        *state->string = arg;
                    }
                    break;
                }
            }
        }
    }
    *pargv = argv;
    *pargc = argc;
}

static char to_lower(char c)
{
    return 'A' <= c && c <= 'Z' ? c | 0x60 : c;
}

static paw_Bool advance_ignore_case(char const **pp, char const *prefix)
{
    paw_assert(*prefix != '\0');
    for (; *prefix != '\0'; ++*pp, ++prefix) {
        if (to_lower(**pp) != *prefix)
            return PAW_FALSE;
    }
    return PAW_TRUE;
}

static void parse_debug_string(void)
{
#define SKIP_SPACES(p) while (ISSPACE(p[0])) ++p

    char const *p = s_opt.d;
    if (p == NULL)
        return;

    do {
        SKIP_SPACES(p);
        if (advance_ignore_case(&p, "ast")) {
            s_debug.ast = PAW_TRUE;
        } else if (advance_ignore_case(&p, "hir")) {
            s_debug.hir = PAW_TRUE;
        } else if (advance_ignore_case(&p, "mir")) {
            s_debug.mir = PAW_TRUE;
        } else if (advance_ignore_case(&p, "stats")) {
            s_debug.stats = PAW_TRUE;
        } else {
            error(PAW_EVALUE, "invalid debug string '%s'\n", s_opt.d);
        }
        SKIP_SPACES(p);
    } while (*p++ == ',');

#undef SKIP_SPACES
}

static void show_help(void)
{
    info("usage: %s OPTIONS [FILE] ...\n", s_progname);
    info("OPTIONS:\n");
    for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
        struct Option opt = s_opt_info[i];
        if (opt.flag) {
            info("-%s     : %s\n", opt.name, opt.description);
        } else {
            info("-%s %s : %s\n", opt.name, opt.argname, opt.description);
        }
    }
}

static void handle_error(paw_Env *P, int status)
{
    if (status != PAW_OK) {
        error(status, "%s\n", paw_string(P, -1));
    }
}

static int on_build_ast(paw_Env *P)
{
    struct Ast *ast = paw_rawptr(P, 1);
    if (ast->modno > 0)
        puts(pawAst_dump(ast));
    return 0;
}

static int on_build_hir(paw_Env *P)
{
    struct Hir *hir = paw_rawptr(P, 1);
    if (hir->modno > 0)
        puts(pawHir_dump(hir));
    return 0;
}

static int on_build_mir(paw_Env *P)
{
    struct Mir *mir = paw_rawptr(P, 1);
    DeclId const did = IR_TYPE_DID(mir->type);
    if (did.modno > 0)
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
    puts(paw_string(P, -1));

    paw_pop(P, 1);
    return 0;
}

#define CHUNKNAME "(chunk)"

static paw_Env *load_source(size_t heap_size)
{
    paw_Env *P = paw_open(&(struct paw_Options){
            .heap_size = heap_size,
    });
    if (P == NULL)
        error(PAW_EMEMORY, "not enough memory\n");

    // register debug callbacks
    if (s_debug.stats) {
        paw_push_string(P, "paw.stats_reporter");
        paw_new_native(P, stats_reporter, 0);
        paw_map_set(P, PAW_REGISTRY_INDEX);
    }
    if (s_debug.ast) {
        paw_push_string(P, "paw.on_build_ast");
        paw_new_native(P, on_build_ast, 0);
        paw_map_set(P, PAW_REGISTRY_INDEX);
    }
    if (s_debug.hir) {
        paw_push_string(P, "paw.on_build_hir");
        paw_new_native(P, on_build_hir, 0);
        paw_map_set(P, PAW_REGISTRY_INDEX);
    }
    if (s_debug.mir) {
        paw_push_string(P, "paw.on_build_mir");
        paw_new_native(P, on_build_mir, 0);
        paw_map_set(P, PAW_REGISTRY_INDEX);
    }

    int status;
    // Load the source code, either from a string, or a file. If '-e' is passed,
    // then always use the provided string (ignore path).
    if (s_opt.e != NULL) {
        paw_push_string(P, CHUNKNAME);
        status = pawL_load_chunk(P, CHUNKNAME, s_opt.e);
    } else if (s_pathname != NULL) {
        paw_push_string(P, s_pathname);
        status = pawL_load_file(P, s_pathname);
    } else {
        // TODO: interactive mode or read from stdin
        error(PAW_ERUNTIME, "missing pathname or chunk\n");
    }
    handle_error(P, status);
    return P;
}

static ValueId find_main(paw_Env *P)
{
    paw_mangle_start(P);
    paw_push_string(P, "main");
    paw_mangle_add_name(P);

    struct paw_Item item;
    int const status = paw_lookup_item(P, -1, &item);
    if (status != PAW_OK)
        error(PAW_ERUNTIME, "unable to find entrypoint ('main' function)\n");
    if (item.global_id < 0)
        error(PAW_ERUNTIME, "'main' is not a function\n"); // TODO: check signature, exclude constants
    return item.global_id;
}

static void setup_stack(paw_Env *P, int argc, char const **argv)
{
    int const gid = find_main(P);
    paw_get_global(P, gid);

    paw_push_string(P, s_pathname);
    for (int i = 0; i < argc; ++i) {
        paw_push_string(P, argv[i]);
    }
    paw_new_list(P, 1 + argc);
}

int main(int argc, char const **argv)
{
    parse_options(&argc, &argv);
    if (s_opt.h) {
        show_help();
        return 0;
    }

    parse_debug_string();

    paw_Env *P = load_source(s_opt.H
                                     ? 1ULL << s_opt.H
                                     : 0 /* use default */);
    if (s_opt.c)
        return 0;

    setup_stack(P, argc, argv);
    int const status = paw_call(P, 1);
    handle_error(P, status);

    paw_close(P);
    return 0;
}
