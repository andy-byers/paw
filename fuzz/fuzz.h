// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_FUZZ_H
#define PAW_FUZZ_H

#include "paw.h"
#include <stdio.h>
#include <stdlib.h>

#define FUZZ_UNUSED(x) ((void)x)
#define FUZZ_CHECK(cond, ...)        \
    do {                             \
        if (!(cond)) {               \
            FUZZ_ERROR(__VA_ARGS__); \
        }                            \
    } while (0)
#define FUZZ_ERROR(...)               \
    do {                              \
        fprintf(stderr, __VA_ARGS__); \
        abort();                      \
    } while (0)

inline static void *fuzz_malloc(size_t size)
{
    void *ptr = malloc(size);
    FUZZ_CHECK(ptr != NULL, "malloc(%zu) failed", size);
    return ptr;
}

inline static void *fuzz_realloc(void *ptr, size_t size)
{
    void *new_ptr = realloc(ptr, size);
    FUZZ_CHECK(new_ptr != NULL, "realloc(%p, %zu) failed", ptr, size);
    return new_ptr;
}

inline static void fuzz_free(void *ptr)
{
    free(ptr);
}

struct FuzzState {
    size_t heap_size;
    void *heap;
    paw_Env *P;
};

inline static struct FuzzState fuzz_open(size_t heap_size)
{
    void *heap = fuzz_malloc(heap_size);
    paw_Env *P = paw_open(&(struct paw_Options){
        .heap_size = heap_size,
        .heap = heap,
    });
    FUZZ_CHECK(P != NULL, "paw_open() failed\n");
    return (struct FuzzState){
        .heap_size = heap_size,
        .heap = heap,
        .P = P,
    };
}

inline static void fuzz_close(struct FuzzState fs)
{
    paw_close(fs.P);
    fuzz_free(fs.heap);
}

struct FuzzReader {
    char const *data;
    size_t size;
};

static char const *fuzz_reader(paw_Env *P, void *ud, size_t *size)
{
    FUZZ_UNUSED(P);
    struct FuzzReader *fr = ud;
    // read some amount of source text, dependent on the value of the first byte
    size_t const value = fr->size > 0 ? fr->data[0] : 0;
    size_t const nread = value > fr->size ? fr->size : value;
    fr->size -= nread;
    *size = nread;
    return (char *)fr->data;
}

#endif // PAW_FUZZ_H
