// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "core.h"
#include "ast.h"
#include "hir.h"
#include "ir_type.h"
#include "lex.h"
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
    OPT_STRS(L, path, 100, "add to linker search path") \
    OPT_STRS(l, spec, 100, "specify library to link") \
    OPT_OPT(h, "display this help message") \
    OPT_OPT(c, "compile the program only") \
    OPT_OPT(t, "build module tests") \
    OPT_OPT(V, "validate LLVM IR") \
    OPT_OPT(q, "suppress output")

static struct {
#define OPT_STRS(Name_, A_, Limit_, B_) char const *Name_[Limit_]; int Name_##_count;
#define OPT_STR(Name_, A_, B_) char const *Name_;
#define OPT_INT(Name_, A_, B_) paw_Int Name_;
#define OPT_OPT(Name_, A_) paw_Bool Name_;
    PROGRAM_OPTIONS
#undef OPT_OPT
#undef OPT_INT
#undef OPT_STR
#undef OPT_STRS
} s_opt;

static struct {
    char const *name;

    char const *module_pathname;

    paw_Bool started_compilation;

    paw_Bool quiet;
} s_program;

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
    paw_Int *integer;
    paw_Bool *flag;
    const char *description;
    int *arg_count_ptr;
    int arg_limit;
} s_opt_info[] = {
#define OPT_STRS(Name_, Arg_, Limit_, Help_) \
    {#Name_, #Arg_, s_opt.Name_, NULL, NULL, Help_, &s_opt.Name_##_count, Limit_},
#define OPT_STR(Name_, Arg_, Help_) \
    {#Name_, #Arg_, &s_opt.Name_, NULL, NULL, Help_, NULL, -1},
#define OPT_INT(Name_, Arg_, Help_) \
    {#Name_, #Arg_, NULL, &s_opt.Name_, NULL, Help_, NULL, -1},
#define OPT_OPT(Name_, Help_) \
    {#Name_, NULL, NULL, NULL, &s_opt.Name_, Help_, NULL, -1},
    PROGRAM_OPTIONS
#undef OPT_OPT
#undef OPT_INT
#undef OPT_STR
#undef OPT_STRS
};
// clang-format on

#define IS_SPACE(Char_) ((Char_) == ' ' || (Char_) == '\t' || (Char_) == '\f'  \
                         || (Char_) == '\v' || (Char_) == '\r' || (Char_) == '\n')

static void info(char const *fmt, ...)
{
    if (!s_opt.q) {
        va_list arg;
        va_start(arg, fmt);
        vprintf(fmt, arg);
        va_end(arg);
    }
}

static void warning(char const *fmt, ...)
{
    if (!s_opt.q) {
        va_list arg;
        va_start(arg, fmt);
        vfprintf(stderr, fmt, arg);
        va_end(arg);
    }
}

_Noreturn static void error(char const *fmt, ...)
{
    if (!s_opt.q) {
        va_list arg;
        va_start(arg, fmt);
        vfprintf(stderr, fmt, arg);
        va_end(arg);
    }
    exit(EXIT_FAILURE);
}

static void show_help(void)
{
    info("usage: %s OPTIONS [FILE] ...\n", s_program.name);
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

static char const *consume_prefix(char const *s, char const *p)
{
    for (; *p != '\0'; ++s, ++p) {
        if (*s == '\0' || *s != *p)
            return NULL;
    }
    return s;
}

static paw_Int parse_int(char const *s)
{
    paw_Int value;
    int const rc = pawX_parse_int(s, 10, &value);
    if (rc == PAW_ESYNTAX) {
        error("invalid integer argument (%s)\n", s);
    } else if (rc == PAW_EOVERFLOW) {
        error("integer argument (%s) is too large\n", s);
    } else {
        return value;
    }
}

// Parse commandline options
static void parse_options(int argc, char const **argv)
{
#define GETOPT(Argc_, Argv_) (--(Argc_), ++(Argv_), (Argv_)[-1])

    {
        // parse the program name
        size_t unused;
        char const *o = GETOPT(argc, argv);
        char const *rest = pawOs_find_last_sep(o, strlen(o), &unused);
        s_program.name = rest != NULL ? rest + 1 : o;
    }

    while (argc > 0) {
        char const *option = GETOPT(argc, argv);
        char const *a = option;
        if (a[0] != '-') {
            // found the module pathname
            if (s_program.module_pathname != NULL)
                error("expected a single module pathname "
                      "(the non-option argument)\n");
            s_program.module_pathname = option;
            continue;
        }
        ++a;
        paw_Bool found = PAW_FALSE;
        for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
            struct Option *state = &s_opt_info[i];
            char const *arg = consume_prefix(a, state->name);
            if (arg != NULL) {
                found = PAW_TRUE;
                if (state->flag != NULL) {
                    if (*arg != '\0')
                        error("unexpected argument for option \"%s\"\n", state->name);
                    *state->flag = PAW_TRUE;
                    break; // no argument
                }

                // handle argument passed in two ways: "-xarg" or "-x arg"
                if (*arg == '\0') {
                    if (argc == 0)
                        error("missing argument \"%s\" for option \"%s\"\n",
                                state->argname, state->name);
                    arg = GETOPT(argc, argv);
                }

                if (state->integer != NULL) {
                    *state->integer = parse_int(arg);
                } else {
                    paw_assert(state->string != NULL);
                    if (state->arg_count_ptr != NULL) {
                        int const n = *state->arg_count_ptr;
                        if (n < state->arg_limit) {
                            state->string[n] = arg;
                            ++*state->arg_count_ptr;
                        } else {
                            error("too many arguments for option \"%s\" "
                                  "(expected at most %" PRId64 ")\n", state->name);
                        }
                    } else {
                        *state->string = arg;
                    }
                }
                break;
            }
        }

        if (!found)
            error("unrecognized option \"%s\"\n", option);
    }

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
            error("invalid debug string '%s'\n", s_opt.d);
        }
        SKIP_SPACES(p);
    } while (*p++ == ',');

#undef SKIP_SPACES
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
    parse_options(argc, argv);
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
        error("invalid argument to \"-O\" option \"%s\"\n", level);

    // parse output location
    char *output_filename = NULL;
    char *output_dirname = NULL;
    if (s_opt.o != NULL) {
        output_filename = (char[PATH_MAX + 1]){0};
        output_dirname = (char[PATH_MAX + 1]){0};
        decompose_pathname(s_opt.o, strlen(s_opt.o),
                output_filename, output_dirname, PAW_FALSE);
    }

    // ensure linker specs have form `["static" | "dynamic"] "=" name`
    for (int i = 0; i < s_opt.l_count; ++i) {
        char const *s = s_opt.l[i];
        if (strchr(s, '=') != 0
                && !consume_prefix(s, "static=")
                && !consume_prefix(s, "dynamic="))
            error("invalid linker specification string \"%s\"", s);
    }

    paw_Env *P = paw_open(&(struct paw_Options){
                .num_linker_paths = s_opt.L_count,
                .num_linker_specs = s_opt.l_count,
                .linker_paths = s_opt.L,
                .linker_specs = s_opt.l,
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
        error("not enough memory\n");

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
    } else if (s_program.module_pathname != NULL) {
        char modname[PATH_MAX + 1];
        char dirname[PATH_MAX + 1];
        decompose_pathname(s_program.module_pathname,
                strlen(s_program.module_pathname),
                modname, dirname, PAW_TRUE);
        status = pawL_load_file(P, modname,
                s_program.module_pathname, dirname);
    } else {
        error("missing pathname or chunk\n");
    }
    if (status != PAW_OK)
        error("%s\n", P->current_errmsg->text);

    paw_close(P);
}
