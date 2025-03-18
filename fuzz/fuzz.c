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

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    struct FuzzReader fr = {
        .data = data,
        .size = size,
    };
    // open a new environment
    paw_Env *P = paw_open(&(struct paw_Options){0});

    // load and compile the input
    int const status = pawL_load_nchunk(P, "fuzz", (const char *)data, size);
    FUZZ_UNUSED(status); // any status is fine

    paw_close(P);
    return 0;
}
