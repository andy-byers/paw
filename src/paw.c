// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

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

#define OMIT_NAME ""

#define PROGRAM_OPTIONS \
    OPT_FLAG(help, "h", "help", "display this help message") \
    OPT_FLAG(quiet, "q", "quiet", "suppress output") \
    OPT_FLAG(compile, "c", "compile", "compile the program only") \
    OPT_FLAG(test, "t", "test", "build tests") \
    OPT_FLAG(validate, OMIT_NAME, "validate", "validate LLVM IR") \
    OPT_FLAG(nostd, OMIT_NAME, "nostd", "omit the standard library") \
    OPT_STRING(source, "e", "source", "text", "accept source code from string argument") \
    OPT_STRING(target, "T", "target", "triple", "triple specifying compilation target") \
    OPT_STRING(optimize, "O", "optimize", "level", "optimization level") \
    OPT_STRING(output, "o", "output", "path", "path to compiler output") \
    OPT_STRING(package_name, "a", "package-name", "name", "name of the target package") \
    OPT_STRINGS(debug, "d", "debug", "phase", "dump debug info for compilation phase") \
    OPT_STRINGS(import_directory, "I", "import-directory", "path", "add directory to import search path") \
    OPT_STRINGS(library_directory, "L", "library-directory", "path", "add directory to linker search path") \
    OPT_STRINGS(ld_path, "l", "ld-path", "name", "specify library to link") \
    OPT_STRINGS(dependency, "p", "dependency", "name", "add Paw package dependency") \

static struct {
#define OPT_STRINGS(Name_, Short_, Long_, A_, B_) struct CStrings *Name_;
#define OPT_STRING(Name_, Short_, Long_, A_, B_) char const *Name_;
#define OPT_INTEGER(Name_, Short_, Long_, A_, B_) paw_Int64 Name_;
#define OPT_FLAG(Name_, Short_, Long_, A_) paw_Bool Name_;
    PROGRAM_OPTIONS
#undef OPT_FLAG
#undef OPT_INTEGER
#undef OPT_STRING
#undef OPT_STRINGS
} s_opt;

static struct Option {
    char const *short_name;
    char const *long_name;
    char const *argname;
    char const **string;
    paw_Int64 *integer;
    paw_Bool *flag;
    struct CStrings **strings;
    char const *description;
} s_opt_info[] = {
#define OPT_STRINGS(Name_, Short_, Long_, Arg_, Help_) \
    {Short_, Long_, Arg_, NULL, NULL, NULL, &s_opt.Name_, Help_},
#define OPT_STRING(Name_, Short_, Long_, Arg_, Help_) \
    {Short_, Long_, Arg_, &s_opt.Name_, NULL, NULL, NULL, Help_},
#define OPT_INTEGER(Name_, Short_, Long_, Arg_, Help_) \
    {Short_, Long_, Arg_, NULL, &s_opt.Name_, NULL, NULL, Help_},
#define OPT_FLAG(Name_, Short_, Long_, Help_) \
    {Short_, Long_, NULL, NULL, NULL, &s_opt.Name_, NULL, Help_},
    PROGRAM_OPTIONS
#undef OPT_FLAG
#undef OPT_INTEGER
#undef OPT_STRING
#undef OPT_STRINGS
};

#undef OMIT_NAME

DEFINE_LIST(CStrings, char const *)

enum Status {
    STATUS_SUCCESS = 0,
    STATUS_INVALID_ARGUMENT = -1,
    STATUS_COMPILER_ERROR = -2,
    STATUS_NOT_ENOUGH_MEMORY = -3,
};

static struct {
    char const *name;
    char const *package_pathname;
    char const *package_name;
    paw_Bool quiet;
} s_program;

static struct {
    paw_Bool ast;
    paw_Bool hir;
    paw_Bool mir;
    paw_Bool lir;
    paw_Bool stats;
} s_debug;

#define IS_SPACE(Char_) ((Char_) == ' ' || (Char_) == '\t' || (Char_) == '\f'  \
                         || (Char_) == '\v' || (Char_) == '\r' || (Char_) == '\n')

static void info(char const *fmt, ...)
{
    if (!s_opt.quiet) {
        va_list arg;
        va_start(arg, fmt);
        vprintf(fmt, arg);
        va_end(arg);
    }
}

static void warning(char const *fmt, ...)
{
    if (!s_opt.quiet) {
        va_list arg;
        va_start(arg, fmt);
        vfprintf(stderr, fmt, arg);
        va_end(arg);
    }
}

_Noreturn static void error(char const *fmt, ...)
{
    if (!s_opt.quiet) {
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
        struct Option const opt = s_opt_info[i];
        if (opt.flag != NULL) {
            info("-%s, --%s\n%s\n", opt.short_name, opt.long_name, opt.description);
        } else {
            info("-%s <%s>, --%s <%s>\n%s\n", opt.short_name, opt.long_name, opt.argname, opt.description);
        }
    }
}

static char const *consume_prefix(char const *s, char const *p)
{
    for (; *p != 0; ++s, ++p) {
        if (*s == 0 || *s != *p)
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

// Match a single-character option prefixed with "-"
// Allows options to be specified like "-ab" if "-a" and "-b" are valid options
// and "-a" does not expect any arguments.
static char const *match_short_option(char const *name, struct Option *out)
{
    for (; *name != 0; ++name) {
        for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
            struct Option const opt = s_opt_info[i];
            if (*name == opt.short_name[0]) {
                *out = opt;
                if (opt.flag == NULL)
                    // matched option expects an argument
                    return ++name;
                *opt.flag = PAW_TRUE;
                break;
            }
        }
    }
    return name;
}

static char const *match_long_option(char const *name, struct Option *out)
{
    for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
        struct Option const opt = s_opt_info[i];
        char const *rest = consume_prefix(name, opt.long_name);
        if (rest != NULL) {
            *out = opt;
            return rest;
        }
    }
    return NULL;
}

struct Args {
    char const *const *p;
    int n;
};

#define ARGS_FROM(Argc_, Argv_) (struct Args){.p = Argv_, .n = Argc_}
#define NEXT_ARG(Args_) (--(Args_)->n, ++(Args_)->p, (Args_)->p[-1])

#define DASH '-'

static void parse_positional_arg(char const *arg)
{
    // found the positional argument, i.e. the main package path
    if (s_program.package_pathname != NULL)
        error("expected a single module pathname "
              "(the non-option argument)\n");
    s_program.package_pathname = arg;
}

static void parse_option_arg(char const *arg, struct Args *args)
{
    char const *a = arg + 1;
    paw_assert(a[-1] == DASH);

    struct Option opt;
    paw_Bool is_long_opt = PAW_FALSE;
    if (*a != DASH) {
        a = match_short_option(a, &opt);
    } else {
        ++a;
        // already checked for "--"
        paw_assert(*a != 0);
        a = match_long_option(a, &opt);
        is_long_opt = PAW_TRUE;
    }

    if (a == NULL)
        error("unrecognized option '%s'\n", arg);

#define OPT_NAME(Opt_) (is_long_opt ? (Opt_).short_name : (Opt_).long_name)

    if (opt.flag != NULL) {
        if (*a != 0)
            error("unexpected argument for option '%s'\n", OPT_NAME(opt));
        *opt.flag = PAW_TRUE;
        return; // no argument
    }

    if (*a == '=') {
        ++a;
    } else if (*a == 0) {
        if (args->n == 0)
            error("missing argument '<%s>' for option '%s'\n",
                    opt.argname, OPT_NAME(opt));
        a = NEXT_ARG(args);
    } else if (is_long_opt) {
        error("long option requires '=' or ' ' between option name and argument\n");
    }

#undef OPTNAME

    if (opt.integer != NULL) {
        *opt.integer = parse_int(a);
    } else if (opt.string != NULL) {
        *opt.string = a;
    } else if (opt.strings != NULL) {
        CStrings_push(*opt.strings, a);
    }
}

static paw_Bool is_dashes(char const *arg)
{
    return arg[0] == DASH && arg[1] == DASH && arg[2] == 0;
}

// Parse commandline options
static void parse_options(struct Args *args)
{
    if (args->n > 0) {
        // parse the program name
        size_t unused;
        char const *arg = NEXT_ARG(args);
        char const *rest = os_find_last_sep(arg, strlen(arg), &unused);
        s_program.name = rest != NULL ? rest + 1 : arg;
    } else {
        s_program.name = "pawc";
    }

    paw_Bool found_dashes = PAW_FALSE;
    while (args->n > 0) {
        char const *arg = NEXT_ARG(args);
        if (!found_dashes && is_dashes(arg)) {
            found_dashes = PAW_TRUE;
        } else if (found_dashes || *arg != DASH) {
            parse_positional_arg(arg);
        } else {
            parse_option_arg(arg, args);
        }
    }
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
    LIST_XFOREACH (s_opt.debug, char const *, arg) {
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
            error("invalid debug string '%s'\n", *arg);
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

static char const *pretty_size(size_t size, char *buffer, size_t buffer_size)
{
    paw_Uint64 const KiB = 1024;
    struct SizeClass {
        char const *units;
        paw_Uint64 base;
        int precision;
    } const CLASSES[] = {
        {"B", 1, 0},
        {"KiB", KiB, 1},
        {"MiB", KiB * KiB, 2},
        {"GiB", KiB * KiB * KiB, 3},
    };

    size_t bound = 1;
    for (; bound < PAW_COUNTOF(CLASSES); ++bound) {
        if (size < CLASSES[bound].base)
            break;
    }

    struct SizeClass const cls = CLASSES[bound - 1];
    int const n = snprintf(buffer, buffer_size, "%.*f %s",
            cls.precision, (double)size / cls.base, cls.units);
    paw_assert(0 < n && n < (int)buffer_size); PAW_UNUSED(n);
    return buffer;
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
            char buffer[256];
            char const *size = pretty_size(stat->value, buffer, PAW_COUNTOF(buffer));
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
    char const *begin = os_find_last_sep(pathname, pathlen, &filelen);
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
    dirname[dirlen] = 0;

    if (ext != NULL) {
        *ext = 0;
        // exclude the `i == 0` iteration to prevent the "foo" part of ".foo" from
        // being considered an extension
        for (size_t i = pathlen - 1; i != 0; --i) {
            if (pathname[i] == '.') {
                size_t const extlen = pathlen - i - 1;
                memcpy(ext, pathname + i + 1, extlen);
                ext[extlen] = 0;
                filelen -= extlen + 1;
                break;
            }
        }
    }
    memcpy(filename, begin, filelen);
    filename[filelen] = 0;
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

static void start_program(struct Allocator const *a)
{
    for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
        CStrings **p = s_opt_info[i].strings;
        if (p != NULL) *p = CStrings_new(a);
    }
}

static void finish_program(void)
{
    for (size_t i = 0; i < PAW_COUNTOF(s_opt_info); ++i) {
        struct Option const opt = s_opt_info[i];
        if (opt.strings != NULL) CStrings_delete(*opt.strings);
    }
}

int main(int argc, char const *argv[])
{
    start_program(&(struct Allocator){
            .P = NULL,
            .alloc = alloc,
            .ctx = NULL,
        });

    parse_options(&(struct Args){
            .p = argv,
            .n = argc,
        });

    if (s_opt.help) {
        show_help();
        return 0;
    }

    if (s_opt.debug != NULL)
        set_debug_flags();

    // parse optimization level
    char const *level = s_opt.optimize ? s_opt.optimize : "0";
    if ((level[0] != '0' && level[0] != '1' && level[0] != '2'
                && level[0] != '3' && level[0] != 's' && level[0] != 'z')
            || level[1] != 0) // `level[0] != 0` implied
        error("invalid argument to '-O' option '%s'\n", level);

    // parse output location
    char *output_filename = NULL;
    char *output_dirname = NULL;
    if (s_opt.output != NULL) {
        size_t const len = strlen(s_opt.output);
        if (len > PATH_MAX)
            error("output path '%s' is too long", s_opt.output);
        output_filename = (char[PATH_MAX + 1]){0};
        output_dirname = (char[PATH_MAX + 1]){0};
        decompose_pathname(s_opt.output, len,
                output_dirname, output_filename, NULL);
    }

    // ensure linker specs have form `["static" | "dynamic"] "=" name`
    for (int i = 0; i < s_opt.ld_path->count; ++i) {
        char const *s = s_opt.ld_path->data[i];
        if (strchr(s, '=') != 0
                && !consume_prefix(s, "static=")
                && !consume_prefix(s, "dynamic="))
            error("invalid linker specification string '%s'", s);
    }

    paw_Env *P = paw_open(&(struct paw_Options){
                .num_library_dirs = s_opt.library_directory->count,
                .num_ld_paths = s_opt.ld_path->count,
                .num_dependencies = s_opt.dependency->count,
                .library_dirs = s_opt.library_directory->data,
                .import_dirs = s_opt.import_directory->data,
                .ld_paths = s_opt.ld_path->data,
                .dependencies = s_opt.dependency->data,
                .output_filename = output_filename,
                .output_dirname = output_dirname,
                .compile_only = s_opt.compile,
                .target = s_opt.target,
                .no_std = s_opt.nostd,
                .build_tests = s_opt.test,
                .enable_asan = PAW_FALSE,
                .verify_ir = s_opt.validate,
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

    if (s_opt.source != NULL && s_program.package_pathname != NULL)
        error("source string option ('-e'/'--source') cannot be used if "
              "main package path (positional argument) is supplied");

    int status;
    if (s_opt.source != NULL) {
        status = lib_load_chunk(P, "(code)", s_opt.source);
    } else if (s_program.package_pathname != NULL) {
        char dir[PATH_MAX + 1];
        char file[PATH_MAX + 1];
        char ext[PATH_MAX + 1];
        size_t const len = strlen(s_program.package_pathname);
        if (len > PATH_MAX)
            error("package name '%s' is too long", s_program.package_pathname);
        decompose_pathname(s_program.package_pathname, len, dir, file, ext);
        char const *package_name = s_opt.package_name != NULL ? s_opt.package_name : file;
        status = lib_load_file(P, package_name, s_program.package_pathname, dir);
    } else {
        error("missing pathname or chunk\n");
    }
    if (status != PAW_OK)
        error("%s\n", P->current_errmsg->text);

    paw_close(P);

    finish_program();
}
