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
        "fn f(n: int) {   \n"
        "    if n > 0 {   \n"
        "        f(n - 1) \n"
        "    }            \n"
        "}                \n"
        "f(1 << 50)       \n";
    struct TestAlloc a = {0};
    paw_Env *P = test_open(NULL, &a);
    int status = test_open_string(P, source);
    handle_error(P, status, 1);

    status = paw_call(P, 0);
    check(status == PAW_EMEMORY);
    handle_error(P, status, 0);
    test_close(P, &a);
}
