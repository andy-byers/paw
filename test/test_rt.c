// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "call.h"
#include "env.h"
#include "type.h"
#include "test.h"

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
        const char *errmsg = paw_string(P, -1);
        fprintf(stderr, "error: %s\n", errmsg);
        paw_pop(P, 1); // pop error message
        ++s_counters.failures;
    }
    return status;
}

// Run all toplevel functions with names starting with 'test'
static void run_tests(const char *name, struct TestAlloc *a, const char *prefix)
{
    ++s_counters.modules;
    paw_Env *P = test_open(test_alloc, a, 0);
    int status = test_open_file(P, name);
    if (handle_error(P, status)) {
        ++s_counters.compile_errors;
        return;
    }

    fprintf(stderr, "running %s.paw...\n", name);

    const size_t length = strlen(prefix);
    struct DefList defs = P->defs;
    for (int i = 0; i < defs.count; ++i) {
        struct Def *def = defs.data[i];
        const String *name = def->hdr.name;
        if (def->hdr.kind == DEF_FUNC
                && def->func.self < 0
                && name->length >= length
                && memcmp(name->text, prefix, length) == 0) {
            // toplevel functions prefixed with 'test' must be public
            check(def->hdr.is_pub);
            fprintf(stderr, "    %s\n", def->func.name->text);
            paw_push_zero(P, 1);
            P->top.p[-1] = *Y_PVAL(P, def->func.vid);
            status = paw_call(P, 0);
            if (handle_error(P, status)) {
                ++s_counters.runtime_errors;
            }
            ++s_counters.tests;
        }
    }
    test_close(P, a);
}

static void script(const char *name)
{
    struct TestAlloc a = {0};
    run_tests(name, &a, "test");
}

int main(void)
{
    // TODO: broken, needs for loops
    // script("loop");
    // script("close_loop_variable");
    // script("integer");
    // script("string");
    // script("list");
    // script("map");

    script("basic");
    script("cfg");
    script("import");
    script("operator");
    script("block");
    script("function");
    script("poly_function");
    script("closure");
    script("capture_upvalue");
    script("method");
    script("primitive");
    script("tuple");
    script("enum");
    script("struct");
    script("poly_struct");
    script("poly_enum");
    script("float");
    script("match");
    script("match_struct");
    script("match_enum");
    script("match_poly_enum");
    script("match_or");
    script("match_guard");
    script("misc");
    script("unit_struct");
    script("unit_variant");
    script("infer_assoc_items");
    script("enum_impl");
    return s_counters.compile_errors+s_counters.runtime_errors;

#define RUN_SCRIPT(name) script(#name);
    TEST_SCRIPTS(RUN_SCRIPT)
#undef RUN_SCRIPT

    fprintf(stderr, "=(Stats)=============\n");
    fprintf(stderr, " modules: %d\n", s_counters.modules);
    fprintf(stderr, " tests: %d\n", s_counters.tests);
    fprintf(stderr, " failures: %d\n", s_counters.failures);
    if (s_counters.failures > 0) {
        fprintf(stderr, " compile errors: %d\n", s_counters.compile_errors);
        fprintf(stderr, " runtime errors: %d\n", s_counters.runtime_errors);
        return -1;
    }
}
