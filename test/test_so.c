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
        "pub fn f(n: int) { \n"
        "    if n > 0 {     \n"
        "        f(n - 1)   \n"
        "    }              \n"
        "}                  \n";
    struct TestAlloc a = {0};
    paw_Env *P = test_open(NULL, &a);
    int status = test_open_string(P, source);
    handle_error(P, status, PAW_TRUE);

    paw_push_string(P, "f");
    const int id = paw_find_public(P);
    paw_push_public(P, id);

    paw_push_int(P, 1 << 24);
    status = paw_call(P, 1);
    check(status == PAW_EMEMORY);
    handle_error(P, status, PAW_FALSE);
    test_close(P, &a);
}
