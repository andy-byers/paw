#include "call.h"
#include "map.h"
#include "paw.h"
#include "rt.h"
#include "test.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// Test the primitive value representations
static void test_primitives(void)
{
    Value v = {.i = -1};

    V_SET_0(&v);
    check(v.u == 0);

    V_SET_BOOL(&v, PAW_TRUE);
    check(V_TRUE(v));
    V_SET_BOOL(&v, PAW_FALSE);
    check(!V_TRUE(v));

    V_SET_FLOAT(&v, 0.0);
    check(V_FLOAT(v) == 0.0);
    V_SET_FLOAT(&v, 12.3);
    check(V_FLOAT(v) == 12.3);
    V_SET_FLOAT(&v, 12.3e123);
    check(V_FLOAT(v) == 12.3e123);
    V_SET_FLOAT(&v, INFINITY);
    check(!isfinite(V_FLOAT(v)));
    V_SET_FLOAT(&v, -INFINITY);
    check(!isfinite(V_FLOAT(v)));
    V_SET_FLOAT(&v, nan(""));
    check(isnan(V_FLOAT(v)));
    V_SET_FLOAT(&v, DBL_MAX);
    check(isfinite(V_FLOAT(v)));
    V_SET_FLOAT(&v, DBL_MIN);
    check(isfinite(V_FLOAT(v)));

    V_SET_INT(&v, 0);
    check(V_INT(v) == 0);
    V_SET_INT(&v, 123);
    check(V_INT(v) == 123);
    V_SET_INT(&v, -123);
    check(V_INT(v) == -123);
    V_SET_INT(&v, PAW_INT_MAX);
    check(V_INT(v) == PAW_INT_MAX);
    V_SET_INT(&v, PAW_INT_MIN);
    check(V_INT(v) == PAW_INT_MIN);
}

#define N 500

static Map *map_new(paw_Env *P)
{
    Value *pv = pawC_stkinc(P, 1);
    Map *m = pawH_new(P);
    V_SET_OBJECT(pv, m); // anchor
    return m;
}

static void map_free(paw_Env *P, Map *map)
{
    paw_assert(map == P->top.p[-1].p);
    pawC_pop(P);
}

static paw_Int map_get(Map *map, paw_Int k)
{
    const Value key = {.i = k};
    const Value *pvalue = pawH_get(map, key);
    paw_assert(pvalue != NULL);
    return pvalue->i;
}

static const paw_Int *map_try(Map *map, paw_Int k)
{
    const Value key = {.i = k};
    const Value *pvalue = pawH_get(map, key);
    return pvalue ? &pvalue->i : NULL;
}

static void map_put(paw_Env *P, Map *map, paw_Int k, paw_Int v)
{
    const Value key = {.i = k};
    const Value value = {.i = v};
    pawH_insert(P, map, key, value);
}

static void map_del(Map *map, paw_Int k)
{
    const Value key = {.i = k};
    pawH_erase(map, key);
}

static void test_map1(paw_Env *P)
{
    Map *m = map_new(P);
    map_put(P, m, 1, 1);
    map_put(P, m, 2, 2);
    map_put(P, m, 3, 3);
    check(1 == map_get(m, 1));
    check(2 == map_get(m, 2));
    check(3 == map_get(m, 3));
    map_free(P, m);
}

static void test_map2(paw_Env *P)
{
    Map *m = map_new(P);
    map_put(P, m, 1, 1);
    map_put(P, m, 2, 2);
    map_put(P, m, 3, 3);
    map_put(P, m, 4, 4);
    map_put(P, m, 5, 5);
    map_put(P, m, 6, 6);

    map_del(m, 1);
    map_del(m, 2);
    map_del(m, 4);
    map_del(m, 5);

    check(NULL == map_try(m, 1));
    check(NULL == map_try(m, 2));
    check(3 == map_get(m, 3));
    check(NULL == map_try(m, 4));
    check(NULL == map_try(m, 5));
    check(6 == map_get(m, 6));
    map_free(P, m);
}

static void test_map3(paw_Env *P)
{
    Map *m = map_new(P);

    // Add known integers for validation.
    const paw_Int known[] = {-1, -2, -10, -20, -100, -200};
    for (size_t i = 0; i < paw_countof(known); ++i) {
        map_put(P, m, known[i], known[i]);
    }

    check(CAST_SIZE(paw_length(P, -1)) == paw_countof(known));

    // Fill the map with nonnegative integers (may have repeats).
    for (int i = 0; i < N; ++i) {
        const paw_Int ival = test_randint(0, 10000);
        map_put(P, m, ival, ival);
    }

    check(CAST_SIZE(paw_length(P, -1)) <= N + paw_countof(known));

    // Erase all nonnegative integers.
    paw_Int itr = PAW_ITER_INIT;
    while (pawH_iter(m, &itr)) {
        const Value key = *pawH_key(m, CAST_SIZE(itr));
        if (V_INT(key) >= 0) map_del(m, key.i);
    }

    check(CAST_SIZE(pawH_length(m)) <= paw_countof(known));

    // Check known items.
    for (size_t i = 0; i < paw_countof(known); ++i) {
        const paw_Int value = map_get(m, known[i]);
        check(value == known[i]);
    }

    map_free(P, m);
}

static void test_strings(paw_Env *P)
{
    paw_push_nstring(P, "fixed\0\1", 7);
    const void *fixed = P->top.p[-1].p;

    int total_words = 0;
    const char data[] =
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    for (size_t wordlen = 1; wordlen < 26; ++wordlen) {
        const size_t nwords = paw_lengthof(data) - wordlen;
        for (size_t i = 0; i < nwords; ++i) {
            paw_push_nstring(P, data + i, wordlen);
            ++total_words;
        }
        if (total_words > 50) {
            const int npop = total_words / 3;
            total_words -= npop;
            paw_pop(P, npop);
        }
    }
    paw_push_nstring(P, "fixed\0\1", 7);
    check(fixed == P->top.p[-1].p);
}

static void test_stack(paw_Env *P)
{
    paw_push_unit(P, 2);
    check(paw_get_count(P) == 2);
    check(paw_int(P, 0) == 0);
    check(paw_int(P, 1) == 0);
}

static void driver(void (*callback)(paw_Env *))
{
    struct TestAlloc a = {0};
    paw_Env *P = paw_open(&(struct paw_Options){
                .alloc = test_alloc,
                .ud = &a,
            });
    callback(P);
    test_close(P, &a);
}

static int parse_int(paw_Env *P, void *ud)
{
    const int rc = pawV_parse_int(P, CAST(ud, const char *), 0);
    if (rc == PAW_OK) paw_pop(P, 1);
    return rc;
}

static void parse_and_check_int(paw_Env *P, void *ud, paw_Int result)
{
    check(PAW_OK == pawV_parse_int(P, CAST(ud, const char *), 0));
    check(paw_int(P, -1) == result);
    paw_pop(P, 1);
}

static void test_parse_int(paw_Env *P)
{
    // able to parse PAW_INT_MIN directly, since we consider the '-'
    parse_and_check_int(P, "-9223372036854775808", INT64_MIN); 
    parse_and_check_int(P, "9223372036854775807", INT64_MAX); 
    parse_and_check_int(P, "  -1", -1); // sign must touch first digit
    parse_and_check_int(P, " +2  ", 2); 

    check(PAW_ESYNTAX == parse_int(P, "--1"));
    check(PAW_ESYNTAX == parse_int(P, "- 1"));
    check(PAW_ESYNTAX == parse_int(P, "01"));
    check(PAW_ESYNTAX == parse_int(P, "123 4"));
    check(PAW_ESYNTAX == parse_int(P, "123.4"));
    check(PAW_EOVERFLOW == parse_int(P, "9223372036854775808")); 
    check(PAW_EOVERFLOW == parse_int(P, "-9223372036854775809")); 
}

int main(void)
{
    test_primitives();
    driver(test_strings);
    driver(test_stack);
    driver(test_map1);
    driver(test_map2);
    driver(test_map3);
    driver(test_parse_int);
    return 0;
}
