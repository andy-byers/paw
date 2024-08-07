// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "paw.h"
#include "lib.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FUZZ_UNUSED(x) ((void)x)
#define FUZZ_CHECK(cond, ...) do { \
        if (!(cond)) { \
            FUZZ_ERROR(__VA_ARGS__); \
        } \
    } while (0)
#define FUZZ_ERROR(...) do { \
        fprintf(stderr, __VA_ARGS__); \
        abort(); \
    } while (0)

struct FuzzAlloc {
    size_t nbytes;
};

#define MEM_LIMIT (1024 * 1024 * 4)

static void *fuzz_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    struct FuzzAlloc *a = ud;
    FUZZ_CHECK(size0 <= a->nbytes, 
               "invalid 'size0' %zu for 'realloc' "
               "(%zu bytes allocated total)", 
               size0, a->nbytes);
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
    FUZZ_UNUSED(P);
    struct FuzzReader *fr = ud;
    // read some amount of source text, dependent on the value of the first byte
    const size_t value = fr->size > 0 ? fr->data[0] : 0;
    const size_t nread = value > fr->size ? fr->size : value;
    fr->size -= nread;
    *size = nread;
    return (char *)fr->data;
}

static const char kPrefix[] = "pub fn main(args: [str]) -> int {";
static const char kSuffix[] = "}";

extern int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    const size_t len = paw_lengthof(kPrefix) + 1 +
                       paw_lengthof(kSuffix) + 1 +
                       size + 1;
    char *ptr = malloc(len);
    const char *buf = ptr;
#define CHUNK(s, n) memcpy(ptr, s, n); \
                    (ptr) += (n); \
                    *(ptr)++ = '\n';
    CHUNK(kPrefix, paw_lengthof(kPrefix));
    CHUNK(data, size);
    CHUNK(kSuffix, paw_lengthof(kSuffix));

    struct FuzzAlloc fa = {0};
    struct FuzzReader fr = {
        .data = data,
        .size = size,
    };
    // open a new environment and load the input
    paw_Env *P = paw_open(fuzz_alloc, &fa);
    const int status = pawL_load_nchunk(P, "fuzz", buf, len);
    if (status != PAW_OK) {
        goto cleanup;
    }
    paw_push_string(P, "main");
    const int pid = paw_find_public(P);
    if (pid < 0) return 0;
    paw_push_public(P, pid);

    // call 'f()'
    paw_call(P, 0);

cleanup:
    paw_close(P);
    FUZZ_CHECK(fa.nbytes == 0, "leaked memory");
    free((void *)buf);
    return 0;
}
