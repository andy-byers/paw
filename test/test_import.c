// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// test_import.c: Test "use" declarations

#include <string.h>
#include "error.h"
#include "lib.h"
#include "test.h"
#include "util.h"

struct ModuleSource {
    char const *name;
    char const *text;
};

struct ModuleSet {
#define MAX_TEST_MODULES 10
    struct ModuleSource sources[MAX_TEST_MODULES];
    int count;
};

#define MODULE_SOURCE(Name_, Text_) \
    ((struct ModuleSource){.name = (Name_), .text = (Text_)})
#define INIT_MODULE_SET(SetPtr_, ...) do {                          \
        struct ModuleSource sources_[] = {__VA_ARGS__};             \
        init_module_set_(sources_, PAW_COUNTOF(sources_), SetPtr_); \
    } while (0)
static void init_module_set_(struct ModuleSource *sources, int count, struct ModuleSet *out)
{
    check(count <= MAX_TEST_MODULES);

    out->count = count;
    for (int i = 0; i < count; ++i)
        out->sources[i] = sources[i];
}

static int chunk_searcher(paw_Env *P)
{
    struct TestAlloc *ctx = paw_context(P);
    struct ModuleSet *ms = ctx->ud;

    char const *text = NULL;
    char const *name = paw_str(P, 1);
    for (int i = 0; i < ms->count; ++i) {
        struct ModuleSource source = ms->sources[i];
        if (strcmp(source.name, name) == 0) {
            text = source.text;
            break;
        }
    }
    if (text != NULL) {
        pawL_chunk_reader(P, text, strlen(text));
    } else {
        paw_push_zero(P, 1);
    }
    return 1;
}

static void add_chunk_searcher(paw_Env *P)
{
    paw_push_str(P, "paw.searchers");
    paw_map_get(P, PAW_REGISTRY_INDEX);

    paw_new_native(P, chunk_searcher, 0);
    paw_list_push(P, -2);
    paw_pop(P, 1);
}

static int call_main_fn(paw_Env *P)
{
    paw_mangle_start(P);
    paw_push_str(P, "main");
    paw_mangle_add_name(P);

    struct paw_Item item;
    int const status = paw_lookup_item(P, -1, &item);
    check(status == PAW_OK && item.global_id >= 0);
    paw_get_global(P, item.global_id);

    return paw_call(P, 0);
}

#define COMPILE_AND_RUN(Expect_, ...) do {     \
        struct ModuleSet set;                     \
        INIT_MODULE_SET(&set, __VA_ARGS__);       \
        check(compile_and_run_(set) == (Expect_)); \
    } while (0)
static int compile_and_run_(struct ModuleSet set)
{
    struct TestAlloc a = {.ud = &set};
    paw_Env *P = test_open(test_mem_hook, &a, 0 /* default */);
    check(P != NULL);

    add_chunk_searcher(P);
    struct ModuleSource main = set.sources[0];
    int status = test_open_string(P, main.name, main.text);
    if (status == PAW_OK) status = call_main_fn(P);

    test_close(P, &a);
    return status;
}

// macros for generating code for tests
#define USE(Path_) "use " Path_ ";\n"
#define FN(Name_, Value_) "fn " Name_ "() -> int {" Value_ "}\n"
#define STRUCT(Name_, Body_) "struct " Name_ "{ pub field: (), " Body_ "}\n"
#define STRUCT0(Name_) "struct " Name_ ";\n"
#define ENUM(Name_, Body_) "enum " Name_ "{" Body_ "}\n"
#define PUB_USE(Path_) "pub " USE(Path_)
#define PUB_FN(Name_, Value_) "pub " FN(Name_, Value_)
#define PUB_STRUCT(Name_, Body_) "pub " STRUCT(Name_, Body_)
#define PUB_STRUCT0(Name_) "pub " STRUCT0(Name_)
#define PUB_ENUM(Name_, Body_) "pub " ENUM(Name_, Body_)
#define PUB_MAIN(Body_) "pub fn main() {" Body_ "}\n"

static void test_import_module(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b as other")
            PUB_MAIN());
    struct ModuleSource b = MODULE_SOURCE("b", "");

    COMPILE_AND_RUN(PAW_OK, a, b);
}

static void test_import_type(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::T as BT")
            PUB_MAIN("let bt = BT{field: ()};"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_STRUCT("T", ));

    COMPILE_AND_RUN(PAW_OK, a, b);
}

static void test_import_value(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::v as bv")
            PUB_MAIN("assert(bv() == 42);"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_FN("v", "42"));

    COMPILE_AND_RUN(PAW_OK, a, b);
}

static void test_import_glob(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::*")
            PUB_MAIN("assert(v() == 42);"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_FN("v", "42"));

    COMPILE_AND_RUN(PAW_OK, a, b);
}

static void test_reexport(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::*")
            PUB_MAIN("let b = B;"
                     "let c = C;"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_USE("c::*")
            PUB_STRUCT0("B"));
    struct ModuleSource c = MODULE_SOURCE("c",
            PUB_USE("b::*")
            PUB_STRUCT0("C"));

    COMPILE_AND_RUN(PAW_OK, a, b, c);
}

static void test_rename_own_symbol(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("v as f")
            PUB_FN("v", "42")
            PUB_MAIN("assert(v() == 42);"
                     "assert(f() == 42);"));

    COMPILE_AND_RUN(PAW_OK, a);
}

static void test_import_type_and_value_with_same_name(void)
{
    // "use b::same;" brings in both definitions and places them in different
    // namespaces (type and value, respectively)
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::same")
            PUB_MAIN("let s = same{field: ()};"
                     "assert(same() == 42);"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_FN("same", "42")
            PUB_STRUCT("same", ));

    COMPILE_AND_RUN(PAW_OK, a, b);
}

static void test_alias_type_and_value_to_same_name(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::v as same")
            PUB_USE("b::T as same")
            PUB_MAIN("let s = same{field: ()};"
                     "assert(same() == 42);"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_FN("v", "42")
            PUB_STRUCT("T", ));

    COMPILE_AND_RUN(PAW_OK, a, b);
}

static void test_chain_of_aliases(void)
{
    // TODO: make sure referring to "a" from "b" doesn't cause "a" to be parsed again
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("bbbb::vvvv as f")
            PUB_USE("bbbb::vv as vvv")
            PUB_USE("bb::bbb as bbbb")
            PUB_USE("b as bb")
            PUB_MAIN("assert(f() == 42);"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_USE("a::vvv as vvvv")
            PUB_USE("a::bbbb::v as vv")
            PUB_USE("a::bb as bbb")
            PUB_FN("v", "42"));

    COMPILE_AND_RUN(PAW_OK, a, b);
}

// Glob-imported symbols can conflict so long as they are never referenced
static void test_conflicting_glob_succeeds_if_unreferenced(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::*")
            PUB_USE("c::*")
            PUB_MAIN(""));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_FN("v", "42"));
    struct ModuleSource c = MODULE_SOURCE("c",
            PUB_FN("v", "123"));

    COMPILE_AND_RUN(PAW_OK, a, b, c);
}

static void test_conflicting_glob_fails_if_referenced(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::*")
            PUB_USE("c::*")
            PUB_USE("v as f")
            PUB_MAIN());
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_FN("v", "42"));
    struct ModuleSource c = MODULE_SOURCE("c",
            PUB_FN("v", "123"));

    COMPILE_AND_RUN(E_AMBIGUOUS_PATH, a, b, c);
}

static void test_glob_import_has_lower_precedence(void)
{
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::*")
            PUB_USE("c::v")
            PUB_MAIN("assert(v() == 123);"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_FN("v", "42"));
    struct ModuleSource c = MODULE_SOURCE("c",
            PUB_FN("v", "123"));

    COMPILE_AND_RUN(PAW_OK, a, b, c);
}

static void test_glob_import_adds_deferred_candidates(void)
{
    // import of "b::f" requires multiple passes to resolve, while "b::g" resolves
    // on the first pass
    struct ModuleSource a = MODULE_SOURCE("a",
            PUB_USE("b::*")
            PUB_MAIN("assert(f() == 42);"
                     "assert(g() == 123);"));
    struct ModuleSource b = MODULE_SOURCE("b",
            PUB_USE("c::*")
            PUB_USE("c::vvv as vvvv")
            PUB_USE("c::v as vv")
            PUB_FN("g", "123"));
    struct ModuleSource c = MODULE_SOURCE("c",
            PUB_USE("b::vvvv as f")
            PUB_USE("b::vv as vvv")
            PUB_FN("v", "42"));

    COMPILE_AND_RUN(PAW_OK, a, b, c);
}

int main(void)
{
    // sanity checks
    test_import_module();
    test_import_type();
    test_import_value();
    test_import_glob();
    test_reexport();

    test_rename_own_symbol();
    test_import_type_and_value_with_same_name();
    test_alias_type_and_value_to_same_name();
    test_chain_of_aliases();
    test_conflicting_glob_succeeds_if_unreferenced();
    test_conflicting_glob_fails_if_referenced();
    test_glob_import_has_lower_precedence();
    test_glob_import_adds_deferred_candidates();
}
