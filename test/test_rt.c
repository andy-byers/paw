// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <ctype.h>
#include <stdlib.h>
#include "call.h"
#include "env.h"
#include "test.h"
#include "rtti.h"

static struct {
    paw_Bool quiet;
} s_options;

#define ERROR(...) do { \
        fprintf(stderr, __VA_ARGS__); \
        abort(); \
    } while (0) \

#define INFO(...) do { \
        if (!s_options.quiet) \
            fprintf(stderr, __VA_ARGS__); \
    } while (0) \

static struct {
    int tests;
    int failures;
    int modules;
    int compile_errors;
    int runtime_errors;
} s_counters;

static int handle_error(paw_Env *P, int status)
{
    if (status != PAW_OK) {
        char const *errmsg = paw_str(P, -1);
        fprintf(stderr, "error: %s\n", errmsg);
        paw_pop(P, 1); // pop error message
        ++s_counters.failures;
    }
    return status;
}

struct FilterArgs {
#define MAX_ARGS 256
    struct FilterArg {
        char const *modname;
        char const *testname;
    } args[MAX_ARGS];
    int count;
};

struct {
    struct FilterArgs allow;
    struct FilterArgs deny;
} s_filter;

static paw_Bool check_next(char **ptr, char c)
{
    if (**ptr == c) {
        ++*ptr;
       return PAW_TRUE;
    }
   return PAW_FALSE;
}

static void consume_filter_item(char *item)
{
#define SKIP_SPACES(Ptr_) while (isspace(*(Ptr_))) ++(Ptr_);

    if (s_filter.deny.count >= MAX_ARGS - s_filter.allow.count)
        ERROR("too many test filters");

    char const *begin = item;
    SKIP_SPACES(item); // skip leading spaces

    // determine if this is a "deny" pattern
    struct FilterArgs *target = &s_filter.allow;
    if (check_next(&item, '^')) {
        target = &s_filter.deny;
        SKIP_SPACES(item);
    }

    // split into module and test parts
    char *dot, *end = item, *testname = "*";
    if ((dot = strchr(item, '.')) != NULL) {
        end = testname = dot + 1; // skip '.'
        *dot = '\0'; // split after module
    }

    // strip trailing spaces
    while (!isspace(*end) && *end != '\0') {
        if (isalnum(*end) || *end == '_' // identifier chars
                || *end == '?' || *end == '*') { // special chars
            ++end;
        } else {
            ERROR("unexpected character '%c' in filter argument \"%s\"", *end, begin);
        }
    }
    *end = '\0';

    target->args[target->count++] = (struct FilterArg){
        .testname = testname,
        .modname = item,
    };

#undef SKIP_SPACES
}

static void parse_filters(char *text)
{
    char *item = text;
    while ((text = strchr(text, ',')) != NULL) {
        *text++ = '\0'; // split and skip ','
        consume_filter_item(item);
        item = text;
    }
    consume_filter_item(item);
}

// Source: Jack Handy (jakkhandy@hotmail.com)
static int wildcmp(const char *needle, const char *haystack) {
    const char *hp = NULL, *np = NULL;

    while (*haystack != '\0' && *needle != '*') {
      if (*needle != *haystack && *needle != '?')
          return 0;
      needle++;
      haystack++;
    }

    while (*haystack != '\0') {
        if (*needle == '*') {
            if (*++needle == '\0')
                return 1;
            np = needle;
            hp = haystack + 1;
        } else if (*needle == *haystack || *needle == '?') {
            needle++;
            haystack++;
        } else {
            needle = np;
            haystack = hp++;
        }
    }

    while (*needle == '*')
        needle++;

    return *needle == '\0';
}

typedef char const *(*GetComponent)(struct FilterArg);
static paw_Bool matches_filter_arg(struct FilterArgs args, char const *name, GetComponent get)
{
    for (int i = 0; i < args.count; ++i) {
        if (wildcmp(get(args.args[i]), name))
            return PAW_TRUE;
    }
    return PAW_FALSE;
}

#define DEFINE_CHECKER(Kind_) \
    static char const *get_##Kind_##name(struct FilterArg arg) \
    { return arg.Kind_##name; } \
    static paw_Bool should_run_##Kind_(char const *Kind_) \
    { return matches_filter_arg(s_filter.allow, Kind_, get_##Kind_##name) \
          && !matches_filter_arg(s_filter.deny, Kind_, get_##Kind_##name); }
DEFINE_CHECKER(mod)
DEFINE_CHECKER(test)
#undef DEFINE_CHECKER

// Run all toplevel functions with names starting with 'test'
static void run_tests(char const *modname, struct TestAlloc *a, char const *prefix)
{
    if (!should_run_mod(modname)) return;
    INFO("compiling %s...\n", modname);

    ++s_counters.modules;
    paw_Env *P = test_open(test_mem_hook, a, 0);
    int status = test_open_file(P, modname);
    if (handle_error(P, status)) {
        ++s_counters.compile_errors;
        return;
    }

    size_t const length = strlen(prefix);
    struct DefList defs = P->defs;
    for (int i = 0; i < defs.count; ++i) {
        struct Def const *def = defs.data[i];
        Str const *name = def->hdr.name;
        if (def->hdr.kind == DEF_FUNC // definition is a function
                && def->fn.self < 0 // function is free
                && name->length >= length // name might start with prefix
                && memcmp(name->text, prefix, length) == 0 // name starts with prefix
                && should_run_test(name->text + length)) { // test is not filtered out
            // toplevel functions prefixed with 'test_' must be public
            check(def->hdr.is_pub);
            INFO("--> running %s.%s\n", modname, name->text + length);
            paw_push_zero(P, 1);
            P->top.p[-1] = *RTTI_PVAL(P, def->fn.vid);
            status = paw_call(P, 0);
            if (handle_error(P, status)) {
                ++s_counters.runtime_errors;
            }
            ++s_counters.tests;
        }
    }

    test_close(P, a);
}

static void script(char const *name)
{
    struct TestAlloc a = {0};
    run_tests(name, &a, "test_");
}

static void parse_arguments(int argc, char **argv)
{
    PAW_UNUSED(argc);
    ++argv; // skip program name

    while (*argv != NULL) {
        char const *arg = *argv++;
        if (0 == strcmp(arg, "-f")
                || 0 == strcmp(arg, "--filter")) {
            if (*argv == NULL) // missing filter argument
                ERROR("expected argument for option \"%s\"", arg);
            parse_filters(*argv++);
        } else if (0 == strcmp(arg, "-q")
                || 0 == strcmp(arg, "--quiet")) {
            s_options.quiet = PAW_TRUE;
        }
    }

    if (s_filter.allow.count == 0) {
        // default to running all tests
        s_filter.allow = (struct FilterArgs){
            .args[0].modname = "*",
            .args[0].testname = "*",
            .count = 1,
        };
    }
}

int main(int argc, char **argv)
{
    // parse commandline arguments
    parse_arguments(argc, argv);

#define RUN_SCRIPT(name) script(#name);
    // run the test scripts
    TEST_SCRIPTS(RUN_SCRIPT)
#undef RUN_SCRIPT

    // report results
    INFO("=(Stats)=============\n modules: %d\n tests: %d\n failures: %d\n",
            s_counters.modules, s_counters.tests, s_counters.failures);
    if (s_counters.failures > 0) {
        INFO(" compile errors: %d\n runtime errors: %d\n",
                s_counters.compile_errors, s_counters.runtime_errors);
        return -1;
    }
}

