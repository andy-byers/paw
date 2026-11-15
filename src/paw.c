// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

// TODO: use a list from list.h for holding list-like program options instead of requiring an argument count specified at compile time

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "ast/ast.h"
#include "common/list.h"
#include "common/util.h"
#include "compiler/core.h"
#include "compiler/lib.h"
#include "compiler/os.h"
#include "hir/hir.h"
#include "ir/type.h"
#include "mir/mir.h"
#include "parse/lex.h"

enum Status {
    STATUS_SUCCESS = 0,
    STATUS_INVALID_ARGUMENT = -1,
    STATUS_COMPILER_ERROR = -2,
    STATUS_NOT_ENOUGH_MEMORY = -3,
};

#define PROGRAM_OPTIONS \
    OPT_OPTION(h, "help", "display this help message") \
    OPT_OPTION(q, "quiet", "suppress output") \
    OPT_OPTION(c, "compile", "compile the program only") \
    OPT_OPTION(t, "test", "build tests") \
    OPT_OPTION(V, "validate", "validate LLVM IR") \
    OPT_OPTION(n, "nostd", "omit the standard library") \
    OPT_STRING(e, "source", "text", "accept source code from string argument") \
    OPT_STRING(T, "target", "triple", "triple specifying compilation target") \
    OPT_STRING(O, "optimize", "level", "optimization level") \
    OPT_STRING(o, "output", "path", "path to compiler output") \
    OPT_STRING(a, "package-name", "name", "name of the target package") \
    OPT_STRINGS(d, "debug", "phase", "dump debug info for compilation phase") \
    OPT_STRINGS(I, "import-directory", "path", "add directory to import search path") \
    OPT_STRINGS(L, "library-directory", "path", "add directory to linker search path") \
    OPT_STRINGS(l, "ld-path", "name", "specify library to link") \
    OPT_STRINGS(p, "dependency", "name", "add Paw package dependency") \

static struct {
#define OPT_STRINGS(Letter_, Name_, A_, B_) struct CStrings *Letter_;
#define OPT_STRING(Letter_, Name_, A_, B_) char const *Letter_;
#define OPT_INTEGER(Letter_, Name_, A_, B_) paw_Int64 Letter_;
#define OPT_OPTION(Letter_, Name_, A_) paw_Bool Letter_;
    PROGRAM_OPTIONS
#undef OPT_OPTION
#undef OPT_INTEGER
#undef OPT_STRING
#undef OPT_STRINGS
} s_opt;

DEFINE_LIST(CStrings, char const *)

static struct {
    char const *name;
    char const *package_pathname;
    char const *package_name;
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
    char const *short_name;
    char const *long_name;
    char const *argname;
    char const **string;
    paw_Int64 *integer;
    paw_Bool *flag;
    CStrings **strings;
    char const *description;
} s_opt_info[] = {
#define OPT_STRINGS(Letter_, Name_, Arg_, Help_) \
    {#Letter_, Name_, Arg_, NULL, NULL, NULL, &s_opt.Letter_, Help_},
#define OPT_STRING(Letter_, Name_, Arg_, Help_) \
    {#Letter_, Name_, Arg_, &s_opt.Letter_, NULL, NULL, NULL, Help_},
#define OPT_INTEGER(Letter_, Name_, Arg_, Help_) \
    {#Letter_, Name_, Arg_, NULL, &s_opt.Letter_, NULL, NULL, Help_},
#define OPT_OPTION(Letter_, Name_, Help_) \
    {#Letter_, Name_, NULL, NULL, NULL, &s_opt.Letter_, NULL, Help_},
    PROGRAM_OPTIONS
#undef OPT_OPTION
#undef OPT_INTEGER
#undef OPT_STRING
#undef OPT_STRINGS
};

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
        if (opt.flag != NULL) {
            info("-%s|--%s : %s\n", opt.short_name, opt.long_name, opt.description);
        } else {
            info("-%s|--%s <%s> : %s\n", opt.short_name, opt.long_name, opt.argname, opt.description);
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

static paw_Int64 parse_int(char const *s)
{
    paw_Int64 value;
    enum ParseIntStatus const rc = lex_parse_int(s, 10, &value);
    if (rc == IPARSE_ERR_SYNTAX) {
        error("invalid integer argument (%s)\n", s);
    } else if (rc == IPARSE_ERR_OVERFLOW) {
        error("integer argument (%s) is too large\n", s);
    } else {
        return value;
    }
}

enum OptionKind {
    SHORT_OPTION,
    LONG_OPTION,
};

static char const *find_matching_option(char const *name, enum OptionKind kind, struct Option *out)
{
    for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
        struct Option const *opt = &s_opt_info[i];
        char const *rest;
        if ((kind == SHORT_OPTION && (rest = consume_prefix(name, opt->short_name)) != NULL)
                || (kind == LONG_OPTION && (rest = consume_prefix(name, opt->long_name)) != NULL)) {
            *out = *opt;
            return rest;
        }
    }
    return NULL;
}

// Parse commandline options
static void parse_options(int argc, char const **argv)
{
#define GETOPT(Argc_, Argv_) (--(Argc_), ++(Argv_), (Argv_)[-1])

    {
        // parse the program name
        size_t unused;
        char const *arg = GETOPT(argc, argv);
        char const *rest = pawOs_find_last_sep(arg, strlen(arg), &unused);
        s_program.name = rest != NULL ? rest + 1 : arg;
    }

    while (argc > 0) {
        char const *arg = GETOPT(argc, argv);
        char const *a = arg;
        if (*a++ != '-') {
            // found the module pathname
            if (s_program.package_pathname != NULL)
                error("expected a single module pathname "
                      "(the non-option argument)\n");
            s_program.package_pathname = arg;
            continue;
        }

        enum OptionKind kind = SHORT_OPTION;
        if (*a == '-') {
            kind = LONG_OPTION;
            ++a;
        }

        struct Option opt;
        a = find_matching_option(a, kind, &opt);
        if (a == NULL)
            error("unrecognized option \"%s\"\n", arg);

        if (opt.flag != NULL) {
            if (*a != '\0')
                error("unexpected argument for option \"%s\"\n", opt.long_name);
            *opt.flag = PAW_TRUE;
            continue; // no argument
        }

        if (*a == '=') {
            ++a;
        } else if (*a == '\0') {
            if (argc == 0)
                error("missing argument \"%s\" for option \"%s\"\n",
                        opt.argname, opt.long_name);
            a = GETOPT(argc, argv);
        } else if (kind == LONG_OPTION) {
            error("long option requires '=' or ' ' between option name and argument\n");
        }

        if (opt.integer != NULL) {
            *opt.integer = parse_int(a);
        } else if (opt.string != NULL) {
            *opt.string = a;
        } else if (opt.strings != NULL) {
            CStrings_push(*opt.strings, a);
        }
    }

#undef GETOPT
}

static char to_lower(char c)
{
    return 'A' <= c && c <= 'Z' ? c | 0x60 : c;
}

static paw_Bool equals_ignore_case(char const *a, char const *b)
{
    for (; *a != 0; ++a, ++b) {
        if (to_lower(*a) != to_lower(*b))
            return PAW_FALSE;
    }
    return *b == 0;
}

static void set_debug_flags(void)
{
    LIST_XFOREACH (s_opt.d, char const *, arg) {
        if (equals_ignore_case(*arg, "ast")) {
            s_debug.ast = PAW_TRUE;
        } else if (equals_ignore_case(*arg, "hir")) {
            s_debug.hir = PAW_TRUE;
        } else if (equals_ignore_case(*arg, "mir")) {
            s_debug.mir = PAW_TRUE;
        } else if (equals_ignore_case(*arg, "lir")) {
            s_debug.lir = PAW_TRUE;
        } else if (equals_ignore_case(*arg, "stats")) {
            s_debug.stats = PAW_TRUE;
        } else {
            error("invalid debug string `%s`\n", *arg);
        }
    }
}

static int on_build_ast(paw_Env *P, void *arg)
{
    puts(ast_dump(arg));
    return 0;
}

static int on_build_hir(paw_Env *P, void *arg)
{
    puts(hir_dump(arg));
    return 0;
}

static int on_build_mir(paw_Env *P, void *arg)
{
    struct Mir *mir = arg;
    DeclId const did = IR_TYPE_DID(mir->type);
    if (did.modno != PRELUDE_MODNO)
        puts(mir_dump(mir));
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
    lib_init_buffer(P, &b);
    lib_add_fstring(P, &b, "==Stats===============\n");

    struct Statistic *const *pstat;
    LIST_FOREACH (stats, pstat) {
        struct Statistic const *stat = *pstat;
        lib_add_string(P, &b, stat->name);
        if (strstr(stat->name, "bytes")) {
            // statistic is an amount of memory
            char buffer[64];
            char const *size = pretty_size(stat->value, buffer);
            lib_add_fstring(P, &b, ": %s\n", size);
        } else {
            lib_add_fstring(P, &b, ": %I\n", (paw_Int64)stat->value);
        }
    }

    lib_add_char(P, &b, '\n');
    Str const *result = lib_buffer_finish(P, &b);
    puts(result->text);

    return 0;
}

static void decompose_pathname(char const *pathname, size_t pathlen, char *dirname, char *filename, char *ext)
{
    size_t filelen, dirlen;
    char const *begin = pawOs_find_last_sep(pathname, pathlen, &filelen);
    if (begin != NULL) {
        dirlen = pathlen - filelen;
        // skip separator
        --filelen;
        ++begin;
    } else {
        begin = pathname;
        filelen = pathlen;
        dirlen = 0;
    }

    memcpy(dirname, pathname, dirlen);
    dirname[dirlen] = '\0';

    if (ext != NULL) {
        *ext = '\0';
        // exclude the `i == 0` iteration to prevent the "foo" part of ".foo" from
        // being considered an extension
        for (size_t i = pathlen - 1; i != 0; --i) {
            if (pathname[i] == '.') {
                size_t const extlen = pathlen - i - 1;
                memcpy(ext, pathname + i + 1, extlen);
                ext[extlen] = '\0';
                filelen -= extlen + 1;
                break;
            }
        }
    }
    memcpy(filename, begin, filelen);
    filename[filelen] = '\0';
}

static void *alloc(void *ctx, void *ptr, size_t size0, size_t size)
{
    if (size == 0) {
        free(ptr);
        return NULL;
    }

    if (ptr != NULL)
        return realloc(ptr, size);

    return malloc(size);
}

static void startup_program(struct Allocator const *a)
{
    for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
        CStrings **p = s_opt_info[i].strings;
        if (p != NULL) *p = CStrings_new(a);
    }
}

int main(int argc, char const *argv[])
{
    struct Allocator const a = {
        .P = NULL,
        .alloc = alloc,
        .ctx = NULL,
    };
    startup_program(&a);

    parse_options(argc, argv);
    if (s_opt.h) {
        show_help();
        return 0;
    }

    if (s_opt.d != NULL)
        set_debug_flags();

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
                output_dirname, output_filename, NULL);
    }

    // ensure linker specs have form `["static" | "dynamic"] "=" name`
    for (int i = 0; i < s_opt.l->count; ++i) {
        char const *s = s_opt.l->data[i];
        if (strchr(s, '=') != 0
                && !consume_prefix(s, "static=")
                && !consume_prefix(s, "dynamic="))
            error("invalid linker specification string \"%s\"", s);
    }

    paw_Env *P = paw_open(&(struct paw_Options){
                .num_library_dirs = s_opt.L->count,
                .num_ld_paths = s_opt.l->count,
                .num_dependencies = s_opt.p->count,
                .library_dirs = s_opt.L->data,
                .import_dirs = s_opt.I->data,
                .ld_paths = s_opt.l->data,
                .dependencies = s_opt.p->data,
                .output_filename = output_filename,
                .output_dirname = output_dirname,
                .compile_only = s_opt.c,
                .target = s_opt.T,
                .no_std = s_opt.n,
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
        status = lib_load_chunk(P, "(code)", s_opt.e);
    } else if (s_program.package_pathname != NULL) {
        char dir[PATH_MAX + 1];
        char file[PATH_MAX + 1];
        char ext[PATH_MAX + 1];
        decompose_pathname(s_program.package_pathname,
                strlen(s_program.package_pathname),
                dir, file, ext);
        char const *package_name = s_opt.a != NULL ? s_opt.a : file;
        status = lib_load_file(P, package_name, s_program.package_pathname, dir);
    } else {
        error("missing pathname or chunk\n");
    }
    if (status != PAW_OK)
        error("%s\n", P->current_errmsg->text);

    paw_close(P);
}
