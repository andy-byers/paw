// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "paw.h"
#include "lib.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FUZZ_UNUSED(x) (void)x
#define FUZZ_CHECK(x, s) \
    do {                 \
        if (!(x))        \
            fatal(s);    \
    } while (0)

static void fatal(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    abort();
}

struct FuzzReader {
    const uint8_t *data;
    size_t size;
};

static const char *fuzz_reader(paw_Env *P, void *ud, size_t *size)
{
    FUZZ_UNUSED(P);
    struct FuzzReader *fr = ud;
    // Make all the data available. The next call will indicate that there are 0
    // bytes available, causing the parser to stop.
    *size = fr->size;
    fr->size = 0;
    return (char *)fr->data;
}

static const struct Unsafe {
    const char *name;
    size_t length;
} kUnsafeList[] = {
#define X(a) {#a, PAW_LENGTHOF(#a)},
    X("import")
    X("print")
#undef X
};
#define LONGEST_UNSAFE 7

static paw_Bool is_unsafe(const uint8_t *data, size_t size)
{
    // Prevent fuzzer inputs from gaining access to outside resources. At
    // present, the only way this would be possible is by importing a library,
    // like 'io'.
    for (size_t i = 0; i < size; ++i) {
        const char *ptr = (const char *)&data[i];
        const size_t len = size - i;
        for (size_t j = 0; j < PAW_COUNTOF(kUnsafeList); ++j) {
            const struct Unsafe u = kUnsafeList[j];
            if (len >= u.length && 0 == memcmp(ptr, u.name, u.length)) {
                return PAW_TRUE;
            }
        }
    }
    return PAW_FALSE;
}

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (is_unsafe(data, size))
        return -1; // reject unsafe inputs

    struct FuzzReader fr = {
        .data = data,
        .size = size,
    };
    // Open a new environment and load the input.
    paw_Env *P = paw_open(NULL);
    int status = pawL_load_nchunk(P, "fuzz", (const char *)data, size);
    if (status != PAW_OK) {
        FUZZ_CHECK(status == PAW_ESYNTAX, "expected syntax error");
        paw_close(P);
        return 0;
    }
    // Run the module-level code.
    status = paw_call(P, 0);
    if (status == PAW_OK) {
        // Look for a function "fn f()" and call it. Such a function should be
        // included in the seed inputs for better coverage.
        const int gid = paw_find_function(P, "f");
        if (gid >= 0) {
            paw_get_global(P, gid);
            status = paw_call(P, 0);
            FUZZ_UNUSED(status); // any status is OK
        }
    }
    paw_close(P);
    return 0;
}
