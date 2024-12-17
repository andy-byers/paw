// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_TEST_H
#define PAW_TEST_H

#include "env.h"

typedef void (*paw_Test)(paw_Env *);

#if defined(__GNUC__) || defined(__clang__)
# define PAW_TEST(name) \
    void pawT_test_##name(paw_Env *P); \
    __attribute__((constructor)) void pawT_register_##name(void) { \
        pawT_register(#name, pawT_test_##name); \
    } \
    void pawT_test_##name(paw_Env *P)
#else
# define PAW_TEST(name) void pawT_test_##name(paw_Env *P)
#endif

void pawT_register(const char *name, paw_Test test);

#define MAX_TESTS 512
struct paw_TestInfo {
    const char *name;
    paw_Test test;
};

void pawT_register(const char *name, paw_Test test);
struct paw_TestInfo *pawT_test_registry(void);
int pawT_test_count(void);

#endif // PAW_TEST_H
