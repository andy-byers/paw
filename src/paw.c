// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "prefix.h"

#include "lib.h"
#include "paw.h"
#include "util.h"
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// clang-format off
#define PROGRAM_OPTIONS                                 \
    opt_str(e, src, "execute program passed as string") \
    opt_opt(h, "display usage information")             \
    opt_opt(q, "suppress output") 

static struct {
#define opt_str(name, a, b) const char *name;
#define opt_int(name, a, b) int name;
#define opt_opt(name, a) paw_Bool name;
    PROGRAM_OPTIONS
#undef opt_str
#undef opt_int
#undef opt_opt
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
#define opt_str(name, arg, help) \
    {#name, #arg, &s_opt.name, NULL, NULL, help},
#define opt_int(name, arg, help) \
    {#name, #arg, NULL, &s_opt.name, NULL, help},
#define opt_opt(name, help) \
    {#name, NULL, NULL, NULL, &s_opt.name, help},
    PROGRAM_OPTIONS
#undef opt_str
#undef opt_int
#undef opt_opt
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

static void error(int status, const char *fmt, ...)
{
    if (!s_opt.q) {
        va_list arg;
        va_start(arg, fmt);
        vfprintf(stderr, fmt, arg);
        va_end(arg);
        exit(status);
    }
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
        } else if (a[1] == '-' && !a[2]) {
            // Found '--': the rest of the arguments go to the script.
            break;
        }
        for (++a; *a; ++a) {
            const char shr = *a;
            for (size_t i = 0; i < paw_countof(s_opt_info); ++i) {
                state = &s_opt_info[i];
                if (shr == state->name[0]) {
                    if (state->flag) {
                        *state->flag = PAW_TRUE;
                        break;         // no argument
                    } else if (a[1]) { // in '-abc', only 'c' can take an argument
                        error(PAW_ERUNTIME, "option with argument ('%c') must be last\n", shr);
                    } else if (*pargc == 0) {
                        error(PAW_ERUNTIME, "missing argument for option '%s'\n", *(*pargv - 1));
                    }
                    const char *arg = get_option(argc, argv);
                    if (state->integer) {
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

int main(int argc, const char **argv)
{
    parse_options(&argc, &argv);
    if (s_opt.h) {
        show_help();
        return 0;
    }

    paw_Env *P = paw_open(NULL, NULL);
    if (!P) {
        error(PAW_EMEMORY, "not enough memory");
    }

    // Put arguments to the script in a global array named 'argv'.
    for (int i = 0; i < argc; ++i) {
        paw_push_string(P, argv[i]);
    }
    paw_create_array(P, argc);
    paw_set_global(P, "argv");

    int status;
    // Load the source code, either from a string, or a file. If '-e' is passed,
    // then always use the provided string (ignore path).
    if (s_opt.e) {
        status = pawL_load_chunk(P, "<chunk>", s_opt.e);
    } else if (s_pathname) {
        status = pawL_load_file(P, s_pathname);
    } else {
        return 0; // nothing to do TODO: interactive mode
    }
    if (status == PAW_OK) {
        // run the script
        status = paw_call(P, 0);
    }
    if (status == PAW_OK) {
        paw_close(P);
    } else if (paw_get_count(P) && paw_is_string(P, -1)) {
        // error() doesn't return
        error(status, "%s\n", paw_string(P, -1));
    } else {
        error(status, "paw returned status code %d\n", status);
    }
    return status;
}
