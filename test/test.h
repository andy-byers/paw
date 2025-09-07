// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_TEST_TEST_H
#define PAW_TEST_TEST_H

#include "os.h"
#include "paw.h"
#include <stdio.h>
#include <stdlib.h>

#define TEST_SCRIPTS(X)      \
    X(basic)                 \
    X(cfg)                   \
    X(kprop)                 \
    X(primitive)             \
    X(operator)              \
    X(block)                 \
    X(cast)                  \
    X(loop)                  \
    X(function)              \
    X(closure)               \
    X(float)                 \
    X(integer)               \
    X(string)                \
    X(struct)                \
    X(tuple)                 \
    X(enum)                  \
    X(test_list)             \
    X(test_map)              \
    X(stack)                 \
    X(layout)                \
    X(never)                 \
    X(destructure)           \
    X(method)                \
    X(match)                 \
    X(match_enum)            \
    X(match_poly_enum)       \
    X(match_or)              \
    X(match_guard)           \
    X(chain)                 \
    X(misc)                  \
    X(import)                \
    X(import_variant)        \
    X(trait)                 \
    X(alias)                 \
    X(constants)             \
    X(nan_infinity)          \
    X(global_const)          \
    X(poly_function)         \
    X(poly_struct)           \
    X(poly_enum)             \
    X(poly_method)           \
    X(poly_trait)            \
    X(builtin_trait)         \
    X(unit_struct)           \
    X(unit_variant)          \
    X(infer_assoc_items)     \
    X(deferred_init)         \
    X(modify_capture)        \
    X(capture_upvalue)       \
    X(close_loop_variable)   \
    X(modification_sequence) \
    X(argument_sequence)     \
    X(switch_branch)

#define check(x)                                       \
    do {                                               \
        if (!(x)) {                                    \
            fprintf(stderr, "check failed: %s\n", #x); \
            abort();                                   \
        }                                              \
    } while (0)

#ifdef ENABLE_PTR_TRACKER

// max number of outstanding allocations
#define PTR_TRACKER_LIMIT (1 << 14)

#endif // ENABLE_PTR_TRACKER

// TODO: rename to TestEnv
struct TestAlloc {
    size_t count;
    size_t *sizes;
    void **ptrs;
    void *ud;
};

#define READ_MAX 16

struct TestReader {
    FILE *file;
    char const *data;
    char buf[READ_MAX];
    size_t ndata;
    size_t length;
    size_t index;
};

void test_mem_hook(void *ud, void *ptr, size_t size0, size_t size);
char const *test_reader(paw_Env *X, void *ud, size_t *size);
char const *test_pathname(char const *name);

paw_Env *test_open(paw_MemHook mem_hook, struct TestAlloc *state, size_t heap_size);
void test_close(paw_Env *P, struct TestAlloc *a);
int test_open_file(paw_Env *P, char const *pathname);
int test_open_string(paw_Env *P, char const *modname, char const *source);
void test_script(char const *name, struct TestAlloc *a);
void test_recover(paw_Env *X, paw_Bool fatal);

// Return an integer in [min, max) (upper bound is exclusive)
paw_Int test_randint(paw_Int min, paw_Int max);

// Fill 'str' with 'len' printable chars
void test_randstr(char *str, int len);

// Hook for dumping the opcodes for the current Paw function
void test_dump_source(paw_Env *P, paw_Debug const *d);

#endif // PAW_TEST_TEST_H
