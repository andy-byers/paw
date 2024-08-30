// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// test_oom.c: Heap exhaustion tests

#include "test.h"
#include "call.h"
#include "env.h"

static void *oom_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    struct TestAlloc *a = ud;
    if (a->extra + size0 < size) {
        return NULL;
    }
    a->extra += size0;
    a->extra -= size;
    return test_alloc(ud, ptr, size0, size);
}

static int run_tests(paw_Env *P)
{
    struct DefList defs = P->defs;
    for (int i = 0; i < defs.count; ++i) {
        static const char kPrefix[] = "test_";
        static const size_t kLength = paw_lengthof(kPrefix);
        struct Def *def = defs.data[i]; 
        if (!def->hdr.is_pub) continue;
        const String *name = def->hdr.name;
        if (name->length >= kLength &&
                0 == memcmp(name->text, kPrefix, kLength)) {
            check(def->hdr.kind == DEF_FUNC);
            pawC_pushv(P, *pawE_get_val(P, def->func.vid));
            return paw_call(P, 0);
        }
    }
    return PAW_OK;
}

static int script(const char *name, size_t heap_size)
{
    paw_Env *P = paw_open(&(struct paw_Options){
                .heap_size = heap_size,
            });
    if (P == NULL) return PAW_EMEMORY;

    int status = test_open_file(P, name);
    if (status == PAW_OK) status = run_tests(P);
    paw_close(P);
    return status;
}

static void test_oom(const char *name)
{
    size_t heap_size = 1;
    int count = 0;
    int rc;
    do {
        rc = script(name, heap_size);
        heap_size *= 2;
        ++count;
    } while (rc == PAW_EMEMORY);
    check(rc == PAW_OK);
    check(count > 0);
    printf("OOM count: %d\n", count);
}

int main(void)
{
#define RUN_SCRIPT(name) test_oom(#name);
    TEST_SCRIPTS(RUN_SCRIPT)
}
