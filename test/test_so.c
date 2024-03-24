#include "test.h"

static void handle_error(paw_Env *P, int status, paw_Bool fatal)
{
    if (status != PAW_OK) {
        test_recover(P, fatal);
    }
}

int main(void)
{
    const char *source =
        "fn f(n) {        \n"
        "    if n > 0 {   \n"
        "        f(n - 1) \n"
        "    }            \n"
        "}                \n";
    struct TestAlloc a = {0};
    paw_Env *P = test_open(NULL, &a);
    int status = test_open_string(P, source);
    handle_error(P, status, 1);

    status = paw_call(P, 0);
    handle_error(P, status, 1);

    paw_get_global(P, "f");
    paw_push_int(P, PAW_STACK_MAX);
    status = paw_call(P, 1);
    check(status == PAW_EMEMORY);
    handle_error(P, status, 0);
    test_close(P, &a);
}
