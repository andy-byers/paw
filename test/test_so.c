#include "test.h"
#include "env.h"

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
        "        f(n - 1);  \n"
        "    }              \n"
        "}                  \n";
    struct TestAlloc a = {0};
    paw_Env *P = test_open(NULL, &a, 1 << 20);
    int status = test_open_string(P, source);
    handle_error(P, status, PAW_TRUE);

    paw_push_string(P, "f");
    const int did = paw_find_global(P);
    const struct Def *def = pawE_get_def(P, did);
    check(def->hdr.kind == DEF_FUNC);
    paw_get_global(P, def->func.vid);

    paw_push_int(P, 1 << 24);
    status = paw_call(P, 1);
    check(status == PAW_EMEMORY);
    handle_error(P, status, PAW_FALSE);
    test_close(P, &a);
}
