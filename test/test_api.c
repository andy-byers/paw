
#include "lib.h"
#include "paw.h"
#include "test.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "env.h"
#include "env.h"

static struct TestAlloc s_alloc;

static void check_error(paw_Env *P, int status)
{
    if (status != PAW_OK) {
        test_recover(P, PAW_TRUE /* print error and abort */);
    }
}

static paw_Env *start_test(void)
{
    const char *source =
        "pub fn main(args: [str]) -> int {\n"
        "    return #args;                \n"
        "}                                \n";
    paw_Env *P = test_open(NULL, &s_alloc, PAW_HEAP_MIN);
    const int status = pawL_load_chunk(P, "test", source);
    check_error(P, status);
    return P;
}

static void finish_test(paw_Env *P)
{
    test_close(P, &s_alloc);
}

// Compute the nth Fibonacci number
// Use an array, stored as an upvalue, to cache intermediate results.
static int fib(paw_Env *P)
{
    // 'fib' closure + 1 argument
    check(paw_get_count(P) == 1 + 1);
    const paw_Int n = paw_int(P, -1);
    paw_get_upvalue(P, 0, 0);
    check(n >= 0);

    paw_listop(P, PAW_LIST_LEN);
    const paw_Int ncached = paw_int(P, -1);
    paw_pop(P, -1);
    if (n < ncached) {
        // fib(n) has already been computed
        paw_get_upvalue(P, 0, 0);
        paw_push_int(P, n);
        paw_listop(P, PAW_LIST_GET);
        return 1;
    }

    if (n < 2) {
        // Base case
        paw_push_int(P, n);
    } else {
        paw_pop(P, 1);
        // Compute fib(n - 2)
        paw_push_value(P, 0);
        paw_push_int(P, n - 2);
        paw_call(P, 1);
        // Compute fib(n - 1)
        paw_push_value(P, 0);
        paw_push_int(P, n - 1);
        paw_call(P, 1);
        // Compute fib(n)
        paw_arithi(P, PAW_ARITH_ADD);
    }
    // Cache the result
    paw_get_upvalue(P, 0, 0);
    List *list = V_LIST(P->top.p[-1]); // TODO: add functionality to main API
    pawV_list_push(P, list, (Value){.i = paw_int(P, -2)});
    paw_pop(P, 1);
    return 1;
}

static paw_Int call_fib(paw_Env *P, int n)
{
    paw_push_value(P, -1); // copy closure
    paw_push_int(P, n); // push parameter
    const int status = paw_call(P, 1);
    check(status == PAW_OK);

    const paw_Int fibn = paw_int(P, -1);
    paw_pop(P, 1);
    return fibn;
}

int main(void)
{
    paw_Env *P = start_test();

    paw_push_string(P, "main");
    const int gid = paw_find_global(P);
    struct Def *def = pawE_get_def(P, gid);
    paw_get_global(P, def->func.vid);

    paw_push_string(P, "abc");
    paw_new_list(P, 1);
    const int status = paw_call(P, 1);
    check_error(P, status);

    check(paw_int(P, -1) == 1);
    paw_pop(P, 1);

    // Test native closures by generating Fibonacci numbers. Use a list upvalue
    // to cache intermediate results.
    paw_new_list(P, 0);
    paw_new_native(P, fib, 1);
    check(0 == call_fib(P, 0));
    check(1 == call_fib(P, 1));
    check(1 == call_fib(P, 2));
    check(55 == call_fib(P, 10));
    check(12586269025 == call_fib(P, 50));
    paw_pop(P, 1);

    // Perform some VM operations.
    {
        // 1 + 1 == 2
        paw_push_int(P, 1);
        paw_push_int(P, 1);
        paw_arithi(P, PAW_ARITH_ADD);
        paw_push_int(P, 2);
        paw_cmpi(P, PAW_CMP_EQ);
        check(paw_bool(P, -1));
        paw_pop(P, 1);

        // 1 << 1 <= 2
        paw_push_int(P, 1);
        paw_push_int(P, 1);
        paw_bitw(P, PAW_BITW_SHL);
        paw_push_int(P, 2);
        paw_cmpi(P, PAW_CMP_LE);
        check(paw_bool(P, -1));
        paw_pop(P, 1);

        // "ab" + "c" + "123" == "abc123"
        paw_push_string(P, "ab");
        paw_push_string(P, "c");
        paw_strop(P, PAW_STR_CONCAT);
        paw_push_string(P, "123");
        paw_strop(P, PAW_STR_CONCAT);
        paw_push_string(P, "abc123");
        paw_cmps(P, PAW_CMP_EQ);
        check(paw_bool(P, -1));
        paw_pop(P, 1);
    }

    finish_test(P);
    return 0;
}
