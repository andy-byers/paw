// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "call.h"
#include "env.h"
#include "test.h"

static void expect_ok(paw_Env *P, int status)
{
    if (status != PAW_OK) {
        const char *errmsg = paw_string(P, -1);
        fprintf(stderr, "error: %s\n", errmsg);
        abort();
    }
}

static void run_tests(const char *name, struct TestAlloc *a)
{
    paw_Env *P = test_open(test_alloc, a, 0);
    int status = test_open_file(P, name);
    expect_ok(P, status);
    
    struct GlobalVec *gvec = &P->gv;
    for (int i = 0; i < gvec->size; ++i) {
        static const char kPrefix[] = "test_";
        static const size_t kLength = paw_lengthof(kPrefix);
        struct GlobalVar *gvar = &gvec->data[i]; 
        if (gvar->name->length >= kLength &&
                0 == memcmp(gvar->name->text, kPrefix, kLength)) {
            printf("running %s.%s...\n", name, gvar->name->text);
            pawC_pushv(P, gvar->value);
            status = paw_call(P, 0);
            expect_ok(P, status);
        }
    }
    test_close(P, a);
}

static void script(const char *name)
{
    struct TestAlloc a = {0};
    run_tests(name, &a);
}

int main(void)
{
    script("struct");
#define RUN_SCRIPT(name) script(#name);
    TEST_SCRIPTS(RUN_SCRIPT)
}
