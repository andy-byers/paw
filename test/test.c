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

static void trash_memory(void *ptr, size_t o, size_t n)
{
    // TODO: does this actually help performance? probably slow due to the huge
    //       number of cache misses anyway
    volatile unsigned long long *ll = (unsigned long long *)((unsigned char *)ptr + o);
    for (; n >= 8; n -= 8) *ll++ = 0xAAAAAAAAAAAAAAAA;

    volatile unsigned char *c = (unsigned char *)ll;
   // volatile unsigned char *c = (unsigned char *)ptr + o;
    while (n-- > 0) *c++ = 0xAA; // 0b10101010
}

static void *safe_realloc(struct TestAlloc *a, void *ptr, size_t size0, size_t size)
{
    check((size0 == 0) == (ptr == NULL));
    void *ptr2 = size ? malloc(size) : NULL;
    check(size == 0 || ptr2 != NULL); // assume success
    if (ptr2 != NULL) {
        if (ptr != NULL) {
            // resize: copy old contents
            memcpy(ptr2, ptr, PAW_MIN(size0, size));
        }
        if (size0 < size) {
            // grow: fill uninitialized memory
            trash_memory(ptr2, size0, size - size0);
        }
    }
    if (ptr != NULL) {
        // Trash the old allocation in an attempt to mess up any code
        // that still depends on it.
        trash_memory(ptr, 0, size0);
    }
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
        rd->length = fread(rd->buf, 1, sizeof(rd->buf), rd->file);
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
    PAW_UNUSED(P);
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
    static char s_buf[PAW_LENGTHOF(TEST_PREFIX) + 64];
    s_buf[0] = '\0'; // reset length
    strcat(s_buf, TEST_PREFIX);
    strcat(s_buf, "scripts/");
    strcat(s_buf, name);
    strcat(s_buf, ".paw");
    return s_buf;
}

#ifdef ENABLE_PTR_TRACKER
static size_t find_ptr(struct TestAlloc *a, void *ptr)
{
    for (size_t i = 0; i < a->count; ++i) {
        if (a->ptrs[i] == ptr) return i;
    }
    PAW_UNREACHABLE();
}

static void remove_ptr(struct TestAlloc *a, void *ptr, size_t size)
{
    const size_t i = find_ptr(a, ptr);
    check(a->sizes[i] == size);
    a->sizes[i] = a->sizes[a->count - 1];
    a->ptrs[i] = a->ptrs[a->count - 1];
    --a->count;
}

static void add_ptr(struct TestAlloc *a, void *ptr, size_t size)
{
    check(a->count < PTR_TRACKER_LIMIT);
    a->sizes[a->count] = size;
    a->ptrs[a->count] = ptr;
    ++a->count;
}

static void modify_size(struct TestAlloc *a, void *ptr, size_t size)
{
    const size_t i = find_ptr(a, ptr);
    a->sizes[i] = size;
}
#endif // ENABLE_PTR_TRACKER

void test_mem_hook(void *ud, void *ptr, size_t size0, size_t size)
{
    struct TestAlloc *a = ud;
    if (ptr != NULL) {
        const size_t lower = PAW_MIN(size0, size);
        const size_t upper = PAW_MAX(size0, size);
        trash_memory(ptr, lower, upper - lower);
    }

#ifdef ENABLE_PTR_TRACKER
    if (size0 == 0 && size != 0) add_ptr(a, ptr, size);
    if (size0 != 0 && size == 0) remove_ptr(a, ptr, size0);
    if (size0 != 0 && size != 0) modify_size(a, ptr, size);
#endif
}

paw_Env *test_open(paw_MemHook mem_hook, struct TestAlloc *a, size_t heap_size)
{
#ifdef ENABLE_PTR_TRACKER
    a->ptrs = malloc(PTR_TRACKER_LIMIT * sizeof(a->ptrs[0]));
    a->sizes = malloc(PTR_TRACKER_LIMIT * sizeof(a->sizes[0]));
    a->count = 0;
#endif

    return paw_open(&(struct paw_Options){
                .heap_size = heap_size,
                .mem_hook = mem_hook,
                .ud = a,
            });
}

void test_close(paw_Env *P, struct TestAlloc *a)
{
    paw_close(P);

#ifdef ENABLE_PTR_TRACKER
    for (size_t i = 0; i < a->count; ++i) {
        fprintf(stderr, "error: leaked %zu bytes at address %p\n",
                a->sizes[i], a->ptrs[i]);
        abort();
    }
    check(a->count == 0);
#endif
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

    FILE *file = fopen(pathname, "r");
    check(file);
    struct TestReader rd = {.file = file};
    rd.data = rd.buf;

    const int rc = paw_load(P, test_reader, pathname, &rd);
    fclose(file);
    return rc;
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
    paw_Env *P = test_open(test_mem_hook, a, 0);
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
