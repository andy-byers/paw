// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "paw.h"
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

static void *fuzz_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    struct FuzzAlloc *a = ud;
    fuzz_check(a->nbytes >= size0, "invalid 'size0'");
    a->nbytes -= size0;
    a->nbytes += size;
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

static paw_Bool is_unsafe(const uint8_t *data, size_t size)
{
    // Prevent fuzzer inputs from gaining access to outside resources. At
    // present, the only way this would be possible is by requiring a library,
    // like 'io'. Code can also load other code, perhaps created by
    // concatenating strings, that calls 'require' but is missed by this check,
    // so we have to deny inputs containing 'load' as well.
    const char *l = "load";
    const char *p = "print";
    const char *r = "require";
    const size_t nl = strlen(l);
    const size_t np = strlen(p);
    const size_t nr = strlen(r);
    for (size_t i = 0; i + nr < size; ++i) {
        if ((data[i] == l[0] && 0 == memcmp(&data[i], l, nl)) ||
            (data[i] == p[0] && 0 == memcmp(&data[i], p, np)) ||
            (data[i] == r[0] && 0 == memcmp(&data[i], r, nr))) {
            return PAW_BTRUE; // unsafe!
        }
    }
    return PAW_BFALSE;
}

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (is_unsafe(data, size)) {
        return -1; // reject unsafe inputs
    }
    struct FuzzAlloc fa = {
        .nbytes = 0,
    };
    // Open a new evaluation context.
    paw_Env *P = paw_open(fuzz_alloc, &fa);

    struct FuzzReader fr = {
        .data = data,
        .size = size,
    };
    // Load the fuzzer data.
    int status = paw_load(P, fuzz_reader, "fuzz", &fr);
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
