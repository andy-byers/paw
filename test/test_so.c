// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "env.h"
#include "paw.h"
#include "test.h"

static void handle_error(paw_Env *P, int status, paw_Bool fatal)
{
    if (status != PAW_OK) {
        test_recover(P, fatal);
    }
}

int main(void)
{
    char const *source =
        "pub fn f(n: int) { \n"
        "    if n > 0 {     \n"
        "        f(n - 1);  \n"
        "    }              \n"
        "}                  \n";
    struct TestAlloc a = {0};
    paw_Env *P = test_open(NULL, &a, 1 << 24);
    int status = test_open_string(P, source);
    handle_error(P, status, PAW_TRUE);

    paw_mangle_start(P);
    paw_push_str(P, "f");
    paw_mangle_add_name(P);

    struct paw_Item item;
    status = paw_lookup_item(P, -1, &item);
    check(status == PAW_OK && item.global_id >= 0);
    paw_get_global(P, item.global_id);

    paw_push_int(P, 1 << 24);
    status = paw_call(P, 1);
    check(status == PAW_EMEMORY);
    handle_error(P, status, PAW_FALSE);
    test_close(P, &a);
}
