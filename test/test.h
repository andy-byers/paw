// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_TEST_H
#define PAW_TEST_H

#include "paw.h"
#include "os.h"
#include <stdio.h>
#include <stdlib.h>

#define TEST_SCRIPTS(X) \
    X(import) \
    X(primitive) \
    X(operator) \
    X(block) \
    X(loop) \
    X(basic) \
    X(function) \
    X(closure) \
    X(integer) \
    X(float) \
    X(string) \
    X(struct) \
    X(tuple) \
    X(enum) \
    X(list) \
    X(map) \
    X(method) \
    X(misc) \
    X(poly_function) \
    X(poly_struct) \
    X(poly_enum)

#define check(x) \
    do { \
        if (!(x)) { \
            fprintf(stderr, "check failed: %s\n", #x); \
            abort(); \
        } \
    } while (0)

struct TestAlloc {
    size_t nbytes;
    size_t extra;

#define BLOCK_LIMIT 1024
    unsigned blocks[2 * BLOCK_LIMIT + 1];
};

#define READ_MAX 16

struct TestReader {
    File *file;
    const char *data;
    char buf[READ_MAX];
    size_t ndata;
    size_t length;
    size_t index;
};

void *test_alloc(void *ud, void *ptr, size_t size0, size_t size);
const char *test_reader(paw_Env *X, void *ud, size_t *size);
const char *test_pathname(const char *name);

paw_Env *test_open(paw_Alloc alloc, struct TestAlloc *state, size_t heap_size);
void test_close(paw_Env *P, struct TestAlloc *a);
int test_open_file(paw_Env *P, const char *pathname);
int test_open_string(paw_Env *P, const char *source);
void test_script(const char *name, struct TestAlloc *a);
void test_recover(paw_Env *X, paw_Bool fatal);

// Return an integer in [min, max) (upper bound is exclusive)
paw_Int test_randint(paw_Int min, paw_Int max);

// Fill 'str' with 'len' printable chars
void test_randstr(char *str, int len);

#undef PAW_STACK_MAX
#define PAW_STACK_MAX 10000

#endif // PAW_TEST_H
