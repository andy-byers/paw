// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "test.h"
#include "call.h"
#include "env.h"
#include "os.h"
#include "paw.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void trash_memory(void *ptr, size_t n)
{
    volatile uint8_t *p = ptr;
    for (size_t i = 0; i < n; ++i) {
        *p++ = 0xAA; // 0b10101010
    }
}

static void *safe_realloc(struct TestAlloc *a, void *ptr, size_t size0, size_t size)
{
    check((size0 == 0) == (ptr == NULL));
    check(a->nbytes >= size0);
    void *ptr2 = size ? malloc(size) : NULL;
    check(size == 0 || ptr2 != NULL); // assume success
    if (ptr2 != NULL) {
        if (ptr != NULL) {
            // resize: copy old contents
            memcpy(ptr2, ptr, PAW_MIN(size0, size));
        }
        if (size0 < size) {
            // grow: fill uninitialized memory
            trash_memory((char *)ptr2 + size0, size - size0);
        }
    }
    if (ptr != NULL) {
        // Trash the old allocation in an attempt to mess up any code
        // that still depends on it.
        trash_memory(ptr, size0);
    }
    a->nbytes += size - size0;
    free(ptr);
    return ptr2;
}

void *test_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    struct TestAlloc *a = ud;
    return safe_realloc(a, ptr, size0, size);
}

static void next_chunk(struct TestReader *rd)
{
    rd->index = 0;
    if (rd->file) {
        rd->length = pawO_read(rd->file, rd->buf, sizeof(rd->buf));
        return;
    }
    const size_t n = PAW_MIN(rd->ndata, READ_MAX);
    memcpy(rd->buf, rd->data, n);
    rd->data += n;
    rd->ndata -= n;
    rd->length = n;
}

// Read a source file in small chunks to make sure the parser can work incrementally
const char *test_reader(paw_Env *P, void *ud, size_t *size)
{
    paw_unused(P);
    struct TestReader *rd = ud;
    if (!rd->length) {
        next_chunk(rd);
        if (!rd->length) {
            return NULL;
        }
    }
    const size_t r = (size_t)rand();
    *size = PAW_MAX(1, r % rd->length);
    const char *ptr = rd->buf + rd->index;
    rd->length -= *size;
    rd->index += *size;
    return ptr;
}

const char *test_pathname(const char *name)
{
    static char s_buf[paw_lengthof(TEST_PREFIX) + 64];
    s_buf[0] = '\0'; // Reset length
    strcat(s_buf, TEST_PREFIX);
    strcat(s_buf, "scripts/");
    strcat(s_buf, name);
    strcat(s_buf, ".paw");
    return s_buf;
}

paw_Env *test_open(paw_Alloc alloc, struct TestAlloc *state, size_t heap_size)
{
    return paw_open(&(struct paw_Options){
                .heap_size = heap_size,
                .alloc = alloc,
                .ud = state,
            });
}

void test_close(paw_Env *P, struct TestAlloc *a)
{
    paw_close(P);

    if (a->nbytes > 0) {
        fprintf(stderr, "error: leaked %zu bytes\n", a->nbytes);
        abort();
    }
}

static void check_ok(paw_Env *P, int status)
{
    if (status != PAW_OK) {
        test_recover(P, PAW_TRUE); // no return
    }
}

int test_open_file(paw_Env *P, const char *name)
{
    const char *pathname = test_pathname(name);
    if (P == NULL) return PAW_EMEMORY;

    FILE *file = pawO_open(pathname, "r");
    struct TestReader rd = {.file = file};
    rd.data = rd.buf;
    check(file);

    const int status = paw_load(P, test_reader, pathname, &rd);
    pawO_close(rd.file);
    return status;
}

int test_open_string(paw_Env *P, const char *source)
{
    struct TestReader rd = {.data = source, .ndata = strlen(source)};
    return paw_load(P, test_reader, "<string>", &rd);
}

void test_recover(paw_Env *P, paw_Bool fatal)
{
    // Expect an error message on top of the stack.
    check(paw_get_count(P) >= 1);

    if (fatal) {
        const char *s = paw_string(P, -1);
        fprintf(stderr, "%s\n", s);
        abort();
    }
    paw_pop(P, 1);
}

void test_script(const char *name, struct TestAlloc *a)
{
    paw_Env *P = test_open(test_alloc, a, 0);
    check_ok(P, test_open_file(P, name));
    check_ok(P, paw_call(P, 0));
    test_close(P, a);
}

paw_Int test_randint(paw_Int min, paw_Int max)
{
    check(min <= max);
    return rand() % (max - min) + min;
}

void test_randstr(char *str, int len)
{
    static const char kChars[] =
        "0123456789"
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
        " \t\n\r\v\f";
    for (int i = 0; i < len; ++i) {
        str[i] = test_randint(0, sizeof(kChars) - 1);
    }
}
