// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "call.h"
#include "env.h"
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

// Run all toplevel functions with names starting with 'test_'
static void run_tests(const char *name, struct TestAlloc *a, const char *prefix)
{
    ++s_counters.modules;
    paw_Env *P = test_open(test_alloc, a, 0);
    int status = test_open_file(P, name);
    if (handle_error(P, status)) {
        ++s_counters.compile_errors;
        return;
    }
    
    fprintf(stderr, "running %s...\n", name);
    const size_t length = strlen(prefix);
    struct DefList defs = P->defs;
    for (int i = 0; i < defs.count; ++i) {
        struct Def *def = defs.data[i]; 
        const String *name = def->hdr.name;
        if (def->hdr.kind == DEF_FUNC &&
                name->length >= length &&
                memcmp(name->text, prefix, length) == 0) {
            // toplevel functions prefixed with 'test_' must be public
            check(def->hdr.is_pub);
            fprintf(stderr, "    %s\n", def->func.name->text);
            pawC_pushv(P, *pawE_get_val(P, def->func.vid));
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
    run_tests(name, &a, "test_");
}

int main(void)
{
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
