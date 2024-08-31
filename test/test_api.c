
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
        "pub struct Struct<T> {\n"
        "    t: T,             \n"
        "}                     \n"
        "pub enum Enum<T> {\n"
        "    A,            \n"
        "    B(T),         \n"
        "}                 \n"
        "pub fn f(v: (int, str)) {}\n"
        "pub fn main(args: [str]) -> int {\n"
        "    let s = Struct{t: 42};       \n"
        "    let b = Enum::<str>::B('B'); \n"
        "    return #args;                \n"
        "}                                \n";
    paw_Env *P = test_open(NULL, &s_alloc, PAW_HEAP_DEFAULT);
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

static void test_print_type(paw_Env *P, const char *name, paw_Bool mangle /*TODO: remove*/)
{
    paw_push_string(P, name);
    if (mangle) paw_mangle_name(P, NULL);

    struct paw_Item info;
    int status = paw_lookup_item(P, &info);
    check(status == PAW_OK);

    paw_get_typename(P, info.type);

    const char *type = paw_string(P, -1);
    fprintf(stderr, "test_print_type: print '%s: %s'\n", name, type);
    paw_pop(P, 1);
}

int main(void)
{
    paw_Env *P = start_test();

    paw_push_string(P, "main");
    paw_mangle_name(P, NULL);

    struct paw_Item info;
    int status = paw_lookup_item(P, &info);
    check(status == PAW_OK && info.global_id >= 0);

    paw_push_string(P, "abc");
    paw_new_list(P, 1);
    status = paw_call_global(P, info.global_id, 1);
    check_error(P, status);

    check(paw_bytes_used(P) > 0);

    check(paw_int(P, -1) == 1);
    paw_pop(P, 1);

    // Test native closures by generating Fibonacci numbers. Use a list upvalue
    // to cache intermediate results.
    {
        paw_new_list(P, 0);
        paw_new_native(P, fib, 1);
        check(0 == call_fib(P, 0));
        check(1 == call_fib(P, 1));
        check(1 == call_fib(P, 2));
        check(55 == call_fib(P, 10));
        check(12586269025 == call_fib(P, 50));
        paw_pop(P, 1);
    }

    // Perform some VM operations.
#define CHECK_AND_POP_I(i) \
        paw_push_int(P, i); \
        paw_cmpi(P, PAW_CMP_EQ); \
        check(paw_bool(P, -1)); \
        paw_pop(P, 1);
    {
        // -(-1) == 1
        paw_push_int(P, -1);
        paw_arithi(P, PAW_ARITH_NEG);
        CHECK_AND_POP_I(1);

        // 1 + 1 == 2
        paw_push_int(P, 1);
        paw_push_int(P, 1);
        paw_arithi(P, PAW_ARITH_ADD);
        CHECK_AND_POP_I(2);

        // ~1 == -2
        paw_push_int(P, 1);
        paw_bitw(P, PAW_BITW_NOT);
        CHECK_AND_POP_I(-2);

        // 1 << 1 <= 2
        paw_push_int(P, 1);
        paw_push_int(P, 1);
        paw_bitw(P, PAW_BITW_SHL);
        CHECK_AND_POP_I(2);

        // 2 <= 2
        paw_push_int(P, 2);
        paw_push_int(P, 2);
        paw_cmpi(P, PAW_CMP_LE);
        check(paw_bool(P, -1));
        paw_pop(P, 1);
    }
#define CHECK_AND_POP_F(f) \
        paw_push_float(P, f); \
        paw_cmpf(P, PAW_CMP_EQ); \
        check(paw_bool(P, -1)); \
        paw_pop(P, 1);
    {
        // -(-1.0) == 1.0
        paw_push_float(P, -1.0);
        paw_arithf(P, PAW_ARITH_NEG);
        CHECK_AND_POP_F(1.0);

        // 1.0 + 1.0 == 2.0
        paw_push_float(P, 1.0);
        paw_push_float(P, 1.0);
        paw_arithf(P, PAW_ARITH_ADD);
        CHECK_AND_POP_F(2.0);

        // 1.1 > 1.0
        paw_push_float(P, 1.1);
        paw_push_float(P, 1.0);
        paw_cmpf(P, PAW_CMP_GT);
        check(paw_bool(P, -1));
        paw_pop(P, 1);
    }
    {
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

        // m = [1: 1.0, 2: 2.0, 3: 3.0]
        paw_push_int(P, 1);
        paw_push_float(P, 1.0);
        paw_push_int(P, 2);
        paw_push_float(P, 2.0);
        paw_push_int(P, 3);
        paw_push_float(P, 3.0);
        paw_new_map(P, 3);

        // #m == 3
        paw_push_value(P, -1);
        paw_mapop(P, PAW_MAP_LEN);
        check(paw_int(P, -1) == 3);
        paw_pop(P, 1);

        // m[2] == 2.0
        paw_push_value(P, -1);
        paw_push_int(P, 2);
        paw_mapop(P, PAW_MAP_GET);
        paw_push_float(P, 2.0);
        paw_cmpf(P, PAW_CMP_EQ);
        check(paw_bool(P, -1));
    }

    // TODO: remove 'mangle' parameter and get compiler to mangle ADT names
    test_print_type(P, "main", 1);
    test_print_type(P, "f", 1);
    test_print_type(P, "Struct", 0);
    test_print_type(P, "Enum", 0);

    // test foreign objects
    void *ptr = paw_new_foreign(P, 8, 1);
    ((paw_Int *)ptr)[0] = 42;
    check(((paw_Int *)paw_userdata(P, -1))[0] == 42);
    paw_pop(P, 1);

    // test format strings
    paw_push_fstring(P, "%% %s %u %d %I %c %f %p", "str", 1U, -1, PAW_INT_MIN, 'P', 1.0, P);

    finish_test(P);
    return 0;
}
