// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "alloc.h"
#include "env.h"
#include "test.h"
#include "util.h"

static void driver(void (*test_callback)(paw_Env *P))
{
    struct TestAlloc a = {0};
    paw_Env *P = test_open(test_alloc, &a, 1024 * 1024 * 8);
    test_callback(P);
    test_close(P, &a);
}

static void open_and_close(paw_Env *P)
{
    PAW_UNUSED(P);
}

static void test_utils(void)
{
    for (size_t i = 1; i < PAW_ALIGN; ++i) {
        check(PAW_ROUND_UP(i) == PAW_ALIGN);
    }
    check(PAW_ROUND_UP(PAW_ALIGN) == PAW_ALIGN);
    check(PAW_ROUND_UP(PAW_ALIGN + 1) == 2 * PAW_ALIGN);
    check(PAW_ROUND_UP(PAW_ALIGN * 2) == 2 * PAW_ALIGN);
    check(PAW_ROUND_UP(PAW_ALIGN * 2 - 1) == 2 * PAW_ALIGN);
}

static void alloc_and_free(paw_Env *P, size_t size)
{
    void *ptr = pawZ_alloc(P, NULL, size);
    pawZ_alloc(P, ptr, 0);
}

#define MAX_DEFER 32768
static struct DeferredAlloc {
    void *ptr;
    size_t size;
} s_defer[MAX_DEFER];
static size_t s_zdefer = 0;
static int s_ndefer = 0;

static void set_defer_data(void *ptr, size_t size)
{
    memset(ptr, size & 255, size);
}

static void check_defer_data(const void *ptr, size_t size)
{
    const uint8_t *data = ptr;
    for (size_t i = 0; i < size; ++i) {
        check(data[i] == (uint8_t)(size & 255));
    }
}

static void alloc_and_defer(paw_Env *P, size_t size)
{
    check(s_ndefer < MAX_DEFER);
    void *ptr = pawZ_alloc(P, NULL, size);
    const int index = s_ndefer++;
    s_zdefer += size;
    s_defer[index] = (struct DeferredAlloc){
        .size = size,
        .ptr = ptr,
    };
    set_defer_data(ptr, size);
}

static void realloc_deferred_ptrs(paw_Env *P)
{
    for (int i = 0; i < s_ndefer; ++i) {
        struct DeferredAlloc *defer = &s_defer[i];
        const size_t size = CAST_SIZE(rand() % 500 + 10);
        s_zdefer += size - defer->size;
        defer->size = size;
        defer->ptr = pawZ_alloc(P, defer->ptr, defer->size);
        set_defer_data(defer->ptr, defer->size);
    }
}

static void free_deferred_ptrs(paw_Env *P)
{
    while (s_ndefer > 0) {
        struct DeferredAlloc defer = s_defer[--s_ndefer];
        check_defer_data(defer.ptr, defer.size);
        pawZ_alloc(P, defer.ptr, 0);
        s_zdefer -= defer.size;
    }
    check(s_zdefer == 0);
}

static void alloc_pattern(paw_Env *P, size_t size)
{
    alloc_and_defer(P, size + 10);
    alloc_and_defer(P, size + 11);
    alloc_and_defer(P, size + 12);
    alloc_and_defer(P, size + 13);
    alloc_and_free(P, size + 1);
    alloc_and_defer(P, size + 14);
    alloc_and_defer(P, size + 15);
    alloc_and_defer(P, size + 16);
    alloc_and_free(P, size + 2);
    alloc_and_free(P, size + 3);
    alloc_and_defer(P, size + 17);
    alloc_and_defer(P, size + 18);
    alloc_and_free(P, size + 4);
    alloc_and_free(P, size + 5);
    alloc_and_free(P, size + 6);
    alloc_and_defer(P, size + 19);
    alloc_and_free(P, size + 7);
    alloc_and_free(P, size + 8);
    alloc_and_free(P, size + 9);
}

static void test_basic(paw_Env *P)
{
    check(NULL == pawZ_alloc(P, NULL, 0));

    enum {N = 100};
    void *ptrs[N];

    for (int i = 0; i < N; ++i) check((ptrs[i] = pawZ_alloc(P, NULL, CAST_SIZE((i + 1) * N)))); 
    for (int i = 0; i < N; ++i) check((ptrs[i] = pawZ_alloc(P, ptrs[i], CAST_SIZE((N - i) * N)))); 
    for (int i = 0; i < N; ++i) check(NULL == pawZ_alloc(P, ptrs[i], 0)); 
}

static void test_small_allocations(paw_Env *P)
{
    const size_t sizes[] = {0, 10, 11, 100, 101, 102};
    for (size_t i = 0; i < PAW_COUNTOF(sizes); ++i) {
        alloc_pattern(P, sizes[i]);
        alloc_pattern(P, sizes[i]);
    }
    realloc_deferred_ptrs(P);
    free_deferred_ptrs(P);
}

static void test_large_allocations(paw_Env *P)
{
    const size_t sizes[] = {
        P->heap_size >> 11,
        P->heap_size >> 10,
        P->heap_size >> 9,
        P->heap_size >> 8,
        P->heap_size >> 7,
    };
    for (size_t i = 0; i < PAW_COUNTOF(sizes); ++i) {
        alloc_pattern(P, sizes[i]);
        alloc_pattern(P, sizes[i]);
    }
    realloc_deferred_ptrs(P);
    free_deferred_ptrs(P);
}

static void test_lots_of_allocations(paw_Env *P)
{
    for (size_t i = 0; i < 1000; ++i) {
        alloc_pattern(P, CAST_SIZE(rand() % 100 + 1));
    }
    realloc_deferred_ptrs(P);
    realloc_deferred_ptrs(P);
    for (size_t i = 0; i < 100; ++i) {
        alloc_pattern(P, CAST_SIZE(rand() % 1000 + 1));
    }
    realloc_deferred_ptrs(P);
    realloc_deferred_ptrs(P);
    free_deferred_ptrs(P);
}

static void test_oom(paw_Env *P)
{
    void *a = pawZ_alloc(P, NULL, P->heap_size / 8);
    check(a != NULL);
    void *b = pawZ_alloc(P, NULL, P->heap_size / 4);
    check(b != NULL);
    a = pawZ_alloc(P, a, P->heap_size / 2);
    check(a != NULL);
    void *c = pawZ_alloc(P, 0, P->heap_size / 2); // OOM
    check(c == NULL);
    a = pawZ_alloc(P, a, 0);
    check(a == NULL);
    b = pawZ_alloc(P, b, 0);
    check(b == NULL);
}

int main(void)
{
    test_utils();
    driver(open_and_close);
    driver(test_basic);
    driver(test_small_allocations);
    driver(test_large_allocations);
    driver(test_lots_of_allocations);
    driver(test_oom);
}
