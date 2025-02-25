// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_TEST_H
#define PAW_TEST_H

#include "util.h"

#define MAX_TEST_SUITES 64
#define MAX_TEST_CASES 512

struct TestCase {
    char const *name;
    paw_Function test;
};

struct TestSuite {
    struct TestCase cases[MAX_TEST_CASES];
    paw_Function startup;
    paw_Function teardown;
    char const *name;
    int count;
};

inline static int pawT_noop(paw_Env *P)
{
    PAW_UNUSED(P);
    return 0;
}

struct TestSuite *pawT_list_suites(int *pcount);
struct TestSuite *pawT_add_suite(char const *name, paw_Function startup, paw_Function teardown);
void pawT_add_case(struct TestSuite *suite, char const *name, paw_Function test);

#endif // PAW_TEST_H
