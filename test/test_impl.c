// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <assert.h>
#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "call.h"
#include "code.h"
#include "map.h"
#include "paw.h"
#include "rt.h"
#include "test.h"
#include "util.h"
#include "value.h"

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
    check(map == P->top.p[-1].p);
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

static void test_map_get_and_put(paw_Env *P)
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

static void test_map_erase(paw_Env *P)
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

    map_put(P, m, 1, 10);
    map_put(P, m, 4, 40);

    check(10 == map_get(m, 1));
    check(NULL == map_try(m, 2));
    check(3 == map_get(m, 3));
    check(40 == map_get(m, 4));
    check(NULL == map_try(m, 5));
    check(6 == map_get(m, 6));
    map_free(P, m);
}

static void test_map_erase_2(paw_Env *P)
{
    Map *m = map_new(P);

    const int k0 = 1;
    const int v0 = 42;
    map_put(P, m, k0, v0);

    const int n = 14;
    for (int i = 0; i < n; ++i) {
        const int k = k0 + i + 1;
        map_put(P, m, k, i);
        map_del(m, k);
    }

    check(v0 == map_get(m, k0));

    for (int i = 0; i < n; ++i) {
        const int k = k0 + i + 1;
        check(NULL == map_try(m, k));
    }

    map_free(P, m);
}

static void test_map_ops(paw_Env *P)
{
    Map *m = map_new(P);

    // Add known integers for validation.
    const paw_Int known[] = {-1, -2, -10, -20, -100, -200};
    for (size_t i = 0; i < PAW_COUNTOF(known); ++i) {
        map_put(P, m, known[i], known[i]);
    }

    check(m->length  == PAW_COUNTOF(known));

    // Fill the map with nonnegative integers (may have repeats).
    for (int i = 0; i < N; ++i) {
        const paw_Int ival = test_randint(0, 10000);
        map_put(P, m, ival, ival);
    }

    check(m->length <= N + PAW_COUNTOF(known));

    // Erase all nonnegative integers.
    paw_Int itr = PAW_ITER_INIT;
    while (pawH_iter(m, &itr)) {
        const Value key = *pawH_key(m, CAST_SIZE(itr));
        if (V_INT(key) >= 0) map_del(m, key.i);
    }

    check(CAST_SIZE(pawH_length(m)) <= PAW_COUNTOF(known));

    // Check known items.
    for (size_t i = 0; i < PAW_COUNTOF(known); ++i) {
        const paw_Int value = map_get(m, known[i]);
        check(value == known[i]);
    }

    map_free(P, m);
}

static void test_map_ops_2(paw_Env *P)
{
    const int nrounds = 10;
    Map *m = map_new(P);

    for (int iter = 0; iter < nrounds; ++iter) {
        const int start = iter * N;
        for (int i = start; i < start + N; i += 1) map_put(P, m, i, i);
        for (int i = start; i < start + N; i += 2) map_del(m, i);
    }
    for (int i = 0; i < N; i += 1) map_put(P, m, i, i * 2);
    for (int i = 0; i < N; i += 2) map_del(m, i);

    for (int i = 0; i < nrounds * N; ++i) {
        if (i & 1) {
            const int scale = i < N ? 2 : 1;
            check(map_get(m, i) == i * scale);
        } else {
            check(map_try(m, i) == NULL);
        }
    }

    map_free(P, m);
}

static void test_map_extend(paw_Env *P)
{
    Map *a = map_new(P);
    Map *b = map_new(P);
    map_put(P, a, 1, 10);
    map_put(P, a, 2, 20);
    map_put(P, a, 3, 30);
    map_put(P, a, 4, 40);

    map_put(P, b, 3, 31);
    map_put(P, b, 4, 41);
    map_put(P, b, 5, 51);

    map_del(a, 4);

    pawH_extend(P, a, b);

    check(10 == map_get(a, 1));
    check(20 == map_get(a, 2));
    check(31 == map_get(a, 3));
    check(41 == map_get(a, 4));
    check(51 == map_get(a, 5));

    map_free(P, b);
    map_free(P, a);
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
        const size_t nwords = PAW_LENGTHOF(data) - wordlen;
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
    const int n = paw_get_count(P);
    paw_push_zero(P, 2);
    check(paw_get_count(P) == n + 2);
    check(paw_int(P, n) == 0);
    check(paw_int(P, n + 1) == 0);
}

static void driver(void (*callback)(paw_Env *))
{
    struct TestAlloc a = {0};
    paw_Env *P = test_open(test_mem_hook, &a, 0);
    callback(P);
    test_close(P, &a);
}

static int parse_int(paw_Env *P, const char *text)
{
    paw_Int i;
    return pawV_parse_int(P, text, 0, &i);
}

static void roundtrip_int(paw_Env *P, paw_Int i);

static void pac_int_aux(paw_Env *P, const char *text, paw_Int result)
{
    paw_Int i;
    check(PAW_OK == pawV_parse_int(P, text, 0, &i));
    check(i == result);
}

static void roundtrip_int(paw_Env *P, paw_Int i)
{
    paw_push_int(P, i);
    paw_int_to_string(P, -1, NULL);
    const char *str = paw_string(P, -1);
    pac_int_aux(P, ERASE_TYPE(str), i);
    paw_pop(P, 1);
}

static void parse_and_check_int(paw_Env *P, const char *text, paw_Int result)
{
    pac_int_aux(P, text, result);
    roundtrip_int(P, result);
}

static void test_parse_int(paw_Env *P)
{
    // able to parse PAW_INT_MIN directly, since we consider the '-'
    parse_and_check_int(P, "-9223372036854775808", INT64_MIN);
    parse_and_check_int(P, "9223372036854775807", INT64_MAX);
    parse_and_check_int(P, "0b111111111111111111111111111111111111111111111111111111111111111", INT64_MAX);
    parse_and_check_int(P, "-0b1000000000000000000000000000000000000000000000000000000000000000", INT64_MIN);
    parse_and_check_int(P, "0o777777777777777777777", INT64_MAX);
    parse_and_check_int(P, "-0o1000000000000000000000", INT64_MIN);
    parse_and_check_int(P, "0x7FFFFFFFFFFFFFFF", INT64_MAX);
    parse_and_check_int(P, "-0x8000000000000000", INT64_MIN);
    parse_and_check_int(P, "  -1", -1); // sign must touch first digit
    parse_and_check_int(P, " +2  ", 2);

    check(PAW_ESYNTAX == parse_int(P, "--1"));
    check(PAW_ESYNTAX == parse_int(P, "- 1"));
    check(PAW_ESYNTAX == parse_int(P, "01"));
    check(PAW_ESYNTAX == parse_int(P, "123 4"));
    check(PAW_ESYNTAX == parse_int(P, "123.4"));
    check(PAW_EOVERFLOW == parse_int(P, "9223372036854775808"));
    check(PAW_EOVERFLOW == parse_int(P, "-9223372036854775809"));
    check(PAW_EOVERFLOW == parse_int(P, "999999999999999999999999999999999999999"));
    check(PAW_EOVERFLOW == parse_int(P, "-999999999999999999999999999999999999999"));

    paw_Int i;
    check(PAW_EVALUE == pawV_parse_int(P, "0b0", 10 /* wrong base */, &i));
    check(PAW_EVALUE == pawV_parse_int(P, "0o0", 10 /* wrong base */, &i));
    check(PAW_EVALUE == pawV_parse_int(P, "0x0", 10 /* wrong base */, &i));
}

static int parse_float(paw_Env *P, const char *text)
{
    paw_Float f;
    return pawV_parse_float(P, text, &f);
}

static void pac_float_aux(paw_Env *P, const char *text, paw_Float result)
{
    paw_Float f;
    check(PAW_OK == pawV_parse_float(P, text, &f));
    check(f == result);
}

static void roundtrip_float(paw_Env *P, paw_Float f)
{
    paw_push_float(P, f);
    paw_float_to_string(P, -1, NULL);
    const char *str = paw_string(P, -1);
    pac_float_aux(P, ERASE_TYPE(str), f);
    paw_pop(P, 1);
}

static void parse_and_check_float(paw_Env *P, const char *text, paw_Float result)
{
    pac_float_aux(P, text, result);
    roundtrip_float(P, result);
}

static void test_parse_float(paw_Env *P)
{
    check(PAW_OK == parse_float(P, "1.23"));
    check(PAW_OK == parse_float(P, "12.3"));
    check(PAW_OK == parse_float(P, "123."));
    check(PAW_OK == parse_float(P, "1.2e3"));
    check(PAW_OK == parse_float(P, "1.e23"));
    check(PAW_OK == parse_float(P, "1.e+23"));

    // small integers and powers of 2 can be represented exactly
    parse_and_check_float(P, "0.0", 0.0);
    parse_and_check_float(P, "1.0", 1.0);
    parse_and_check_float(P, "-1.0", -1.0);
    parse_and_check_float(P, "1.0e+2", 100.0);
    parse_and_check_float(P, "-10000e-2", -100.0);
    parse_and_check_float(P, "-9223372036854775808.0", (paw_Float)INT64_MIN);
    parse_and_check_float(P, "9223372036854775808.0", -((paw_Float)INT64_MIN));
    parse_and_check_float(P, "  -1.0", -1.0); // sign must touch first digit
    parse_and_check_float(P, " +2.0  ", 2.0);
    parse_and_check_float(P, "123", 123.0);

    check(PAW_ESYNTAX == parse_float(P, "--1"));
    check(PAW_ESYNTAX == parse_float(P, "- 1"));
    check(PAW_ESYNTAX == parse_float(P, "01.0"));
    check(PAW_ESYNTAX == parse_float(P, "123 4"));
}

static void test_immediates(void)
{
    OpCode opcode;
#define CHECK_BOUND(X, v) ( \
            opcode = 0, \
            SET_##X(&opcode, v) /* set immediate */, \
            SET_OP(&opcode, 0) /* corrupt */, \
            (v) == GET_##X(opcode) ? (void)0 : ( \
                fprintf(stderr, "'%s' unrepresentable by operand '%s'\n", #v, #X), \
                abort()))
    CHECK_BOUND(sBx, sBx_MAX);
    CHECK_BOUND(sBx, -sBx_MAX);
    CHECK_BOUND(Bx, 0);
    CHECK_BOUND(Bx, Bx_MAX);
    CHECK_BOUND(A, 0);
    CHECK_BOUND(B, B_MAX);
    CHECK_BOUND(B, 0);
    CHECK_BOUND(A, A_MAX);
#undef CHECK_BOUND
}

static void test_buffer(paw_Env *P)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_char(P, &buf, 'a');
    pawL_add_string(P, &buf, "bc");
    pawL_add_nstring(P, &buf, "def", 2);
    check(buf.size == 5);
    check(memcmp(buf.data, "abcde", 5) == 0);
    pawL_discard_result(P, &buf);

    pawL_init_buffer(P, &buf);
    for (int i = 0; i < 1234; ++i) {
        pawL_add_int(P, &buf, i);
    }
    pawL_buffer_resize(P, &buf, 16);
    pawL_push_result(P, &buf);

    paw_push_string(P, "0123456789101112");
   // TODO check(paw_str_rawcmp(P, -1) == 0);
    paw_pop(P, 1);
}

int main(void)
{
    test_primitives();
    test_immediates();
    driver(test_strings);
    driver(test_stack);
    driver(test_map_get_and_put);
    driver(test_map_erase);
    driver(test_map_erase_2);
    driver(test_map_ops);
    driver(test_map_ops_2);
    driver(test_map_extend);
    driver(test_parse_int);
    driver(test_parse_float);
    driver(test_buffer);
    return 0;
}
