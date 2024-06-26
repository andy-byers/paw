
#include "lib.h"
#include "paw.h"
#include "test.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
        "let var = 'variable'        \n"
        "let set                     \n"
        "let get                     \n"
        "{                           \n"
        "    let value               \n"
        "    get = fn() {            \n"
        "        assert(dep != null) \n" // Name error if 'dep' is not created by the caller
        "        return value + var  \n" // Type error if 'value' or 'var' are not numeric types
        "    }                       \n"
        "    set = fn(v) {           \n"
        "        value = v           \n"
        "    }                       \n"
        "}                           \n"
        "class Class {               \n"
        "    __init(v) {             \n"
        "        self.v = v          \n"
        "    }                       \n"
        "}                           \n";
    paw_Env *P = test_open(NULL, &s_alloc);
    int status = pawL_load_chunk(P, "test", source);
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

    if (n < (int)paw_length(P, -1)) {
        // fib(n) has already been computed
        paw_get_itemi(P, -1, n);
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
        paw_binop(P, PAW_OPADD);
    }
    // Cache the result
    paw_get_upvalue(P, 0, 0);
    paw_get_attr(P, -1, "push"); // Get 'push' method
    paw_push_value(P, -3); // Value to push
    paw_call(P, 1);
    paw_pop(P, 2);
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

static void str_equals(paw_Env *P, int index, const char *s)
{
    check(paw_is_string(P, index));
    const size_t n = strlen(s);
    check(n == paw_length(P, index));
    check(0 == memcmp(s, paw_string(P, index), n));
}

int main(void)
{
    paw_Env *P = start_test();

    // Global variables, functions, and classes defined by the module can only
    // be accessed after the module code is run. We can, however, create globals
    // that the module will depend on.
    paw_push_string(P, "abc");
    paw_set_global(P, "dep");

    // Call the module
    const int status = paw_call(P, 0);
    check_error(P, status);

    paw_get_global(P, "var");
    str_equals(P, -1, "variable");
    paw_pop(P, 1);

    // Set global variable 'var', read it back
    paw_push_int(P, 123);
    paw_set_global(P, "var");
    paw_get_global(P, "var");
    check(paw_is_int(P, -1));
    check(paw_int(P, -1) == 123);
    paw_pop(P, 1);

    // Test native closures by generating Fibonacci numbers. Use an array upvalue
    // to cache intermediate results.
    paw_create_array(P, 0);
    paw_push_native(P, fib, 1);
    check(0 == call_fib(P, 0));
    check(1 == call_fib(P, 1));
    check(1 == call_fib(P, 2));
    check(55 == call_fib(P, 10));
    check(12586269025 == call_fib(P, 50));
    paw_pop(P, 1);

    // Roundtrip an integer string (through bigint). Make it very long, so the
    // buffer needs to be boxed.
    {
        char str[1024] = {'9'}; // don't start with '0'
        const size_t len = sizeof(str) - 1;
        for (size_t i = 1; i < len; ++i) {
            str[i] = test_randint('0', '9');
        }
        str[len] = '\0';
        paw_push_string(P, str);
        // Convert to bigint
        paw_to_integer(P, -1);
        check(paw_is_bigint(P, -1));
        paw_to_string(P, -1);
        check(paw_length(P, -1) == len);
        check(0 == memcmp(str, paw_string(P, -1), len));
        paw_pop(P, 1);
    }

    // Perform some VM operations.
    {
        // 1 + 1 == 2
        paw_push_int(P, 1);
        paw_push_int(P, 1);
        paw_binop(P, PAW_OPADD);
        paw_push_int(P, 2);
        paw_raw_equals(P);
        check(paw_boolean(P, -1));
        paw_pop(P, 1);

        // 1 << 1 <= 2
        paw_push_int(P, 1);
        paw_push_int(P, 1);
        paw_binop(P, PAW_OPSHL);
        paw_push_int(P, 2);
        paw_binop(P, PAW_OPLE);
        check(paw_boolean(P, -1));
        paw_pop(P, 1);

        // "ab" ++ "c" ++ 123 == "abc123"
        paw_push_string(P, "ab");
        paw_push_string(P, "c");
        paw_binop(P, PAW_OPCONCAT);
        paw_push_int(P, 123);
        paw_binop(P, PAW_OPCONCAT);
        paw_push_string(P, "abc123");
        paw_binop(P, PAW_OPEQ);
        check(paw_boolean(P, -1));
        paw_pop(P, 1);
    }

    finish_test(P);
    return 0;
}
