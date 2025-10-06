// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdio.h>
#include <stdlib.h>

#include "core.h"
#include "ast.h"
#include "hir.h"
#include "ir_type.h"
#include "lib.h"
#include "mir.h"
#include "os.h"

#include "codegen.h"

enum Status {
    STATUS_SUCCESS = 0,
    STATUS_INVALID_ARGUMENT = -1,
    STATUS_COMPILER_ERROR = -2,
    STATUS_NOT_ENOUGH_MEMORY = -3,
};

// clang-format off
#define PROGRAM_OPTIONS \
    OPT_STR(e, source, "accept source code from string argument") \
    OPT_STR(d, phase, "dump debug info for compilation phase(s)") \
    OPT_STR(O, level, "optimization level") \
    OPT_STR(o, path, "path to compiler output") \
    OPT_STR(I, include, "list of import paths") \
    OPT_OPT(h, "display this help message") \
    OPT_OPT(c, "compile the program only") \
    OPT_OPT(t, "build module tests") \
    OPT_OPT(V, "validate LLVM IR") \
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

static const char *s_program_name;
static const char *s_pathname;

static struct {
    paw_Bool ast;
    paw_Bool hir;
    paw_Bool mir;
    paw_Bool lir;
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

#define IS_SPACE(Char_) ((Char_) == ' ' || (Char_) == '\t' || (Char_) == '\f'  \
                         || (Char_) == '\v' || (Char_) == '\r' || (Char_) == '\n')
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

// Parse commandline options
// Adjusts 'argv' to point to the first argument to the paw script, and
// sets 'argc' to the number of such arguments.
static void parse_options(int *pargc, char const ***pargv)
{
#define GETOPT(Argc_, Argv_) (--(Argc_), ++(Argv_), (Argv_)[-1])

    int argc = *pargc;
    char const **argv = *pargv;
    s_program_name = GETOPT(argc, argv);
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

#undef GETOPT
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
#define SKIP_SPACES(Ptr_) while (IS_SPACE(*(Ptr_))) ++(Ptr_)

    char const *p = s_opt.d;
    paw_assert(p != NULL);

    do {
        SKIP_SPACES(p);
        if (advance_ignore_case(&p, "ast")) {
            s_debug.ast = PAW_TRUE;
        } else if (advance_ignore_case(&p, "hir")) {
            s_debug.hir = PAW_TRUE;
        } else if (advance_ignore_case(&p, "mir")) {
            s_debug.mir = PAW_TRUE;
        } else if (advance_ignore_case(&p, "lir")) {
            s_debug.lir = PAW_TRUE;
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
    info("usage: %s OPTIONS [FILE] ...\n", s_program_name);
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
static int on_build_ast(paw_Env *P, void *arg)
{
    puts(pawAst_dump(arg));
    return 0;
}

static int on_build_hir(paw_Env *P, void *arg)
{
    puts(pawHir_dump(arg));
    return 0;
}

static int on_build_mir(paw_Env *P, void *arg)
{
    struct Mir *mir = arg;
    DeclId const did = IR_TYPE_DID(mir->type);
    if (did.modno != PRELUDE_MODNO)
        puts(pawMir_dump(mir));
    return 0;
}

static char const *pretty_size(size_t size, char *buffer)
{
#define KiB 1024.0

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

static int stats_reporter(paw_Env *P, void *arg)
{
    struct Statistics const *stats = arg;

    Buffer b;
    pawL_init_buffer(P, &b);
    pawL_add_fstring(P, &b, "==Stats===============\n");

    struct Statistic *const *pstat;
    K_LIST_FOREACH (stats, pstat) {
        struct Statistic const *stat = *pstat;
        pawL_add_string(P, &b, stat->name);
        if (strstr(stat->name, "bytes")) {
            // statistic is an amount of memory
            char buffer[64];
            char const *size = pretty_size(stat->value, buffer);
            pawL_add_fstring(P, &b, ": %s\n", size);
        } else {
            pawL_add_fstring(P, &b, ": %I\n", (paw_Int)stat->value);
        }
    }

    pawL_add_char(P, &b, '\n');
    Str const *result = pawL_buffer_finish(P, &b);
    puts(result->text);

    return 0;
}

static void decompose_pathname(char const *pathname, size_t pathlen, char *modname, char *dirname, paw_Bool strip_ext)
{
    size_t modlen, dirlen;
    char const *begin = pawOs_find_last_sep(pathname, pathlen, &modlen);
    if (begin != NULL) {
        dirlen = pathlen - modlen;
        // skip separator
        --modlen;
        ++begin;
    } else {
        begin = pathname;
        modlen = pathlen;
        dirlen = 0;
    }

    memcpy(dirname, pathname, dirlen);
    dirname[dirlen] = '\0';

    if (strip_ext) {
        char const *end = strchr(begin, '.');
        if (end != NULL)
            modlen = (size_t)(end - begin);
    }
    memcpy(modname, begin, modlen);
    modname[modlen] = '\0';
}

int main(int argc, char const *argv[])
{
    parse_options(&argc, &argv);
    if (s_opt.h) {
        show_help();
        return 0;
    }

    if (s_opt.d != NULL)
        parse_debug_string();

    // parse optimization level
    char const *level = s_opt.O ? s_opt.O : "0";
    if ((level[0] != '0' && level[0] != '1' && level[0] != '2'
                && level[0] != '3' && level[0] != 's' && level[0] != 'z')
            || level[1] != '\0') // "level[0] != 0" implied
        error(PAW_EVALUE, "invalid argument to \"-O\" option \"%s\"", level);

    char *output_filename = NULL;
    char *output_dirname = NULL;
    if (s_opt.o != NULL) {
        output_filename = (char[PATH_MAX + 1]){0};
        output_dirname = (char[PATH_MAX + 1]){0};
        decompose_pathname(s_opt.o, strlen(s_opt.o),
                output_filename, output_dirname, PAW_FALSE);
    }

    paw_Env *P = paw_open(&(struct paw_Options){
                .output_filename = output_filename,
                .output_dirname = output_dirname,
                .include_paths = s_opt.I,
                .compile_only = s_opt.c,
                .build_tests = s_opt.t,
                .enable_asan = PAW_FALSE,
                .verify_ir = s_opt.V,
                .dump_ir = s_debug.lir,
                .opt_suffix = level[0],
            });
    if (P == NULL)
        error(STATUS_NOT_ENOUGH_MEMORY, "not enough memory\n");

    if (s_debug.stats)
        pawE_register_callback(P, "paw.stats_reporter", stats_reporter);
    if (s_debug.ast)
        pawE_register_callback(P, "paw.on_build_ast", on_build_ast);
    if (s_debug.hir)
        pawE_register_callback(P, "paw.on_build_hir", on_build_hir);
    if (s_debug.mir)
        pawE_register_callback(P, "paw.on_build_mir", on_build_mir);

    int status;
    // Load the source code, either from a string, or a file. If '-e' is passed,
    // then always use the provided string (ignore path).
    if (s_opt.e != NULL) {
        status = pawL_load_chunk(P, "(code)", s_opt.e);
    } else if (s_pathname != NULL) {
        char modname[PATH_MAX + 1];
        char dirname[PATH_MAX + 1];
        decompose_pathname(s_pathname, strlen(s_pathname), modname, dirname, PAW_TRUE);
        status = pawL_load_file(P, modname, s_pathname, dirname);
    } else {
        error(PAW_ERUNTIME, "missing pathname or chunk\n");
    }
    if (status != PAW_OK)
        error(status, "%s\n", P->current_errmsg->text);

    paw_close(P);
}
