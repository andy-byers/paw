// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "env.h"
#include "lib.h"
#include "paw.h"
#include "util.h"
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// clang-format off
#define PROGRAM_OPTIONS \
    OPT_STR(e, src, "execute program passed as string") \
    OPT_INT(H, log_heap, "log of default heap size in bytes") \
    OPT_OPT(h, "display usage information") \
    OPT_OPT(q, "suppress output") 

static struct {
#define OPT_STR(name, a, b) const char *name;
#define OPT_INT(name, a, b) int name;
#define OPT_OPT(name, a) paw_Bool name;
    PROGRAM_OPTIONS
#undef OPT_STR
#undef OPT_INT
#undef OPT_OPT
} s_opt;

static const char *s_pathname;
static const char *s_program_name;

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
#undef OPT_STR
#undef OPT_INT
#undef OPT_OPT
};
// clang-format on

static void info(const char *fmt, ...)
{
    if (!s_opt.q) {
        va_list arg;
        va_start(arg, fmt);
        vprintf(fmt, arg);
        va_end(arg);
    }
}

_Noreturn static void error(int status, const char *fmt, ...)
{
    if (!s_opt.q) {
        va_list arg;
        va_start(arg, fmt);
        vfprintf(stderr, fmt, arg);
        va_end(arg);
    }
    exit(status);
}

#define get_option(c, v) (--c, ++v, v[-1])

// Parse commandline options
// Adjusts 'argv' to point to the first argument to the paw script, and
// sets 'argc' to the number of such arguments.
static void parse_options(int *pargc, const char ***pargv)
{
    int argc = *pargc;
    const char **argv = *pargv;
    s_program_name = get_option(argc, argv);
    while (argc) {
        struct Option *state;
        const char *option = get_option(argc, argv);
        const char *a = option;
        if (a[0] != '-') {
            // Found a script pathname (the only non-option argument).
            s_pathname = option;
            break;
        } else if (a[1] == '-' && a[2] == '\0') {
            // Found '--': the rest of the arguments go to the script.
            break;
        }
        for (++a; *a; ++a) {
            const char shr = *a;
            for (size_t i = 0; i < paw_countof(s_opt_info); ++i) {
                state = &s_opt_info[i];
                if (shr == state->name[0]) {
                    if (state->flag != NULL) {
                        *state->flag = PAW_TRUE;
                        break; // no argument
                    } else if (a[1] != '\0') { // in '-abc', only 'c' can take an argument
                        error(PAW_ERUNTIME, "option with argument ('%c') must be last\n", shr);
                    } else if (*pargc == 0) {
                        error(PAW_ERUNTIME, "missing argument for option '%s'\n", *(*pargv - 1));
                    }
                    const char *arg = get_option(argc, argv);
                    if (state->integer != NULL) {
                        int value = 0;
                        for (const char *p = arg; p; ++p) {
                            const int v = *p - '0';
                            if (v < 0 || 9 < v) {
                                error(PAW_ERUNTIME, "invalid integer argument (%s)\n", arg);
                            }
                            if (value > (INT_MAX - v) / 10) {
                                error(PAW_ERUNTIME, "integer argument (%s) is too large\n", arg);
                            }
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

static void show_help(void)
{
    info("usage: %s OPTIONS [FILE] ...\n", s_program_name);
    info("OPTIONS:\n");
    for (size_t i = 0; i < paw_countof(s_opt_info); ++i) {
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

#define CHUNKNAME "(chunk)"

static paw_Env *load_source(size_t heap_size)
{
    paw_Env *P = paw_open(&(struct paw_Options){
                .heap_size = heap_size,
            });
    if (P == NULL) {
        error(PAW_EMEMORY, "not enough memory\n");
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
    paw_push_string(P, "main");
    const int did = paw_find_global(P);
    if (did < 0) error(PAW_ERUNTIME, "unable to find entrypoint ('main' function)\n");
    const struct Def *def = pawE_get_def(P, did);
    if (def->hdr.kind != DEF_FUNC) error(PAW_ERUNTIME, "'main' is not a function\n");
    if (!def->func.is_pub) error(PAW_ERUNTIME, "'main' is not public\n");
    const struct FuncDef *f = &def->func;
    if (f->params.count == 1) {
        // TODO: check argument type
        //struct Field param = f->params.data[0];
        //if (!pawE_is_list_of(P, param, PAW_TSTR)) {
        //    error(PAW_ETYPE, "expected '[str]' argument to 'main'");
        //}
    } else if (f->params.count != 0) {
        error(PAW_ETYPE, "'main' requires 0 or 1 argument(s)\n");
    }
    return def->func.vid;
}

#include "rt.h"  // TODO: need pawR_literal_list functionality exposed in main API
static void setup_stack(paw_Env *P, int argc, const char **argv)
{
    const int gid = find_main(P);
    paw_get_global(P, gid);

    for (int i = 0; i < argc; ++i) {
        paw_push_string(P, argv[i]);
    }
    pawR_literal_list(P, argc);
}

static void call_main(paw_Env *P, int argc)
{
    const int status = paw_call(P, argc);
    handle_error(P, status);
}

int main(int argc, const char **argv)
{
    parse_options(&argc, &argv);
    if (s_opt.h) {
        show_help();
        return 0;
    }
    paw_Env *P = load_source(s_opt.H
            ? 1 << s_opt.H
            : 0 /* use default */);
    setup_stack(P, argc, argv);
    call_main(P, argc);

    paw_close(P);
    return 0;
}


