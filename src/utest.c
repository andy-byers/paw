// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "utest.h"

static struct paw_TestInfo s_test_registry[MAX_TESTS];
static int s_test_count;

void pawT_register(const char *name, paw_Test test)
{
    if (s_test_count < MAX_TESTS) {
        s_test_registry[s_test_count] = (struct paw_TestInfo){
            .name = name,
            .test = test,
        };
        ++s_test_count;
    }
}

struct paw_TestInfo *pawT_test_registry(void)
{
    return s_test_registry;
}

int pawT_test_count(void)
{
    return s_test_count;
}

PAW_TEST(stt)
{
    (void)P;
}

