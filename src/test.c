// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "test.h"
#include "env.h"

#if __GNUC__
#define CHECK(expr) \
    if (!(expr))    \
    __builtin_trap()
#elif _MSC_VER
#define CHECK(expr) \
    if (!(expr))    \
    __debugbreak()
#else
#define CHECK(expr) \
    if (!(expr))    \
    *(volatile int *)NULL = 0
#endif

static struct TestSuite s_suites[MAX_TEST_SUITES];
static int s_count;

struct TestSuite *pawT_list_suites(int *pcount)
{
    *pcount = s_count;
    return s_suites;
}

void pawT_add_case(struct TestSuite *suite, char const *name, paw_Function test)
{
    CHECK(s_count < MAX_TEST_CASES);
    suite->cases[suite->count++] = (struct TestCase){
        .name = name,
        .test = test,
    };
}

struct TestSuite *pawT_add_suite(char const *name, paw_Function startup, paw_Function teardown)
{
    CHECK(s_count < MAX_TEST_SUITES);
    struct TestSuite *suite = &s_suites[s_count++];
    *suite = (struct TestSuite){
        .startup = startup,
        .teardown = teardown,
        .name = name,
        .count = 0,
    };
    return suite;
}
