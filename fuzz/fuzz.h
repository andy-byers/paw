// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_FUZZ_H
#define PAW_FUZZ_H

#include "paw.h"
#include <stdio.h>
#include <stdlib.h>

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

struct FuzzState {
    size_t heap_size;
    void *heap;
    paw_Env *P;
};

static inline struct FuzzState fuzz_open(size_t heap_size)
{
    struct paw_Options o = {.heap_size = heap_size};
    o.heap = malloc(o.heap_size);
    FUZZ_CHECK(o.heap != NULL, "malloc() failed");

    paw_Env *P = paw_open(&o);
    FUZZ_CHECK(P != NULL, "paw_open() failed\n");
    return (struct FuzzState){
        .heap_size = heap_size,
        .heap = o.heap,
        .P = P,
    };
}

static inline void fuzz_close(struct FuzzState fs)
{
    paw_close(fs.P);
    free(fs.heap);
}

struct FuzzReader {
    const char *data;
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

#endif // PAW_FUZZ_H
