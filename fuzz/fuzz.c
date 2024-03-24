// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "paw.h"
#include "lib.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define fuzz_unused(x) (void)x
#define fuzz_check(x, s) \
    do {                 \
        if (!(x)) {      \
            fatal(s);    \
        }                \
    } while (0)

static void fatal(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    abort();
}

struct FuzzAlloc {
    size_t nbytes;
};

#define MEM_LIMIT (1024 * 1024 * 4)

static void *fuzz_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    struct FuzzAlloc *a = ud;
    fuzz_check(a->nbytes >= size0, "invalid 'size0'");
    if (size > MEM_LIMIT || // '-' would wrap
        a->nbytes - size0 > MEM_LIMIT - size) {
        return NULL; // used too much memory
    }
    a->nbytes += size - size0;
    return realloc(ptr, size);
}

struct FuzzReader {
    const uint8_t *data;
    size_t size;
};

static const char *fuzz_reader(paw_Env *P, void *ud, size_t *size)
{
    fuzz_unused(P);
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
#define X(a) {#a, paw_lengthof(#a)},
    X("load")
    X("print")
    X("require")
#undef X
};
#define LONGEST_UNSAFE 7

static paw_Bool is_unsafe(const uint8_t *data, size_t size)
{
    // Prevent fuzzer inputs from gaining access to outside resources. At
    // present, the only way this would be possible is by requiring a library,
    // like 'io'. Code can also load other code, perhaps created by
    // concatenating strings, that calls 'require' but is missed by this check,
    // so we have to deny inputs containing 'load' as well.
    for (size_t i = 0; i < size; ++i) {
        const char *ptr = (const char *)&data[i];
        const size_t len = size - i;
        for (size_t j = 0; j < paw_countof(kUnsafeList); ++j) {
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
    if (is_unsafe(data, size)) {
        return -1; // reject unsafe inputs
    }
    struct FuzzAlloc fa = {
        .nbytes = 0,
    };
    struct FuzzReader fr = {
        .data = data,
        .size = size,
    };
    // Open a new environment and load the input.
    paw_Env *P = paw_open(fuzz_alloc, &fa);
    int status = pawL_load_nchunk(P, "fuzz", (const char *)data, size);
    if (status != PAW_OK) {
        fuzz_check(status == PAW_ESYNTAX, "expected syntax error");
        paw_close(P);
        return 0;
    }
    // Run the module-level code.
    status = paw_call(P, 0);
    if (status == PAW_OK) {
        // Look for a function named "f" and call it. Such a function should be
        // included in the seed inputs for better coverage.
        if (paw_check_global(P, "f") && paw_is_function(P, -1)) {
            status = paw_call(P, 0);
            fuzz_unused(status); // any status is OK
        }
    }
    paw_close(P);
    fuzz_check(fa.nbytes == 0, "leaked memory");
    return 0;
}
