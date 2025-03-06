// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "call.h"
#include "code.h"
#include "compile.h"
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

static Tuple *map_new(paw_Env *P)
{
    paw_new_map(P, 0, PAW_TINT);
    return V_TUPLE(P->top.p[-1]);
}

static void map_free(paw_Env *P, Tuple *map)
{
    check(map == P->top.p[-1].p);
    pawC_pop(P);
}

static paw_Int map_get(paw_Env *P, Tuple *map, paw_Int k)
{
    Value const key = {.i = k};
    Value const *pvalue = pawMap_get(P, map, key);
    paw_assert(pvalue != NULL);
    return pvalue->i;
}

static paw_Int const *map_try(paw_Env *P, Tuple *map, paw_Int k)
{
    Value const key = {.i = k};
    Value const *pvalue = pawMap_get(P, map, key);
    return pvalue ? &pvalue->i : NULL;
}

static void map_put(paw_Env *P, Tuple *map, paw_Int k, paw_Int v)
{
    Value const key = {.i = k};
    Value const value = {.i = v};
    pawMap_insert(P, map, key, value);
}

static void map_del(paw_Env *P, Tuple *map, paw_Int k)
{
    Value const key = {.i = k};
    pawMap_remove(P, map, key);
}

static void test_map_get_and_put(paw_Env *P)
{
    Tuple *m = map_new(P);
    map_put(P, m, 1, 1);
    map_put(P, m, 2, 2);
    map_put(P, m, 3, 3);
    check(1 == map_get(P, m, 1));
    check(2 == map_get(P, m, 2));
    check(3 == map_get(P, m, 3));
    map_free(P, m);
}

static void test_map_erase(paw_Env *P)
{
    Tuple *m = map_new(P);
    map_put(P, m, 1, 1);
    map_put(P, m, 2, 2);
    map_put(P, m, 3, 3);
    map_put(P, m, 4, 4);
    map_put(P, m, 5, 5);
    map_put(P, m, 6, 6);

    map_del(P, m, 1);
    map_del(P, m, 2);
    map_del(P, m, 4);
    map_del(P, m, 5);

    map_put(P, m, 1, 10);
    map_put(P, m, 4, 40);

    check(10 == map_get(P, m, 1));
    check(NULL == map_try(P, m, 2));
    check(3 == map_get(P, m, 3));
    check(40 == map_get(P, m, 4));
    check(NULL == map_try(P, m, 5));
    check(6 == map_get(P, m, 6));
    map_free(P, m);
}

static void test_map_erase_2(paw_Env *P)
{
    Tuple *m = map_new(P);

    int const k0 = 1;
    int const v0 = 42;
    map_put(P, m, k0, v0);

    int const n = 14;
    for (int i = 0; i < n; ++i) {
        int const k = k0 + i + 1;
        map_put(P, m, k, i);
        map_del(P, m, k);
    }

    check(v0 == map_get(P, m, k0));

    for (int i = 0; i < n; ++i) {
        int const k = k0 + i + 1;
        check(NULL == map_try(P, m, k));
    }

    map_free(P, m);
}

static void test_map_ops(paw_Env *P)
{
    Tuple *m = map_new(P);

    // Add known integers for validation.
    paw_Int const known[] = {-1, -2, -10, -20, -100, -200};
    for (size_t i = 0; i < PAW_COUNTOF(known); ++i) {
        map_put(P, m, known[i], known[i]);
    }

    check(pawMap_length(m) == PAW_COUNTOF(known));

    // Fill the map with nonnegative integers (may have repeats).
    for (int i = 0; i < N; ++i) {
        paw_Int const ival = test_randint(0, 10000);
        map_put(P, m, ival, ival);
    }

    check(CAST_SIZE(pawMap_length(m)) <= N + PAW_COUNTOF(known));

    // Erase all nonnegative integers.
    paw_Int itr = PAW_ITER_INIT;
    while (pawMap_iter(m, &itr)) {
        Value const key = *pawMap_key(m, itr);
        if (V_INT(key) >= 0)
            map_del(P, m, key.i);
    }

    check(CAST_SIZE(pawMap_length(m)) <= PAW_COUNTOF(known));

    // Check known items.
    for (size_t i = 0; i < PAW_COUNTOF(known); ++i) {
        paw_Int const value = map_get(P, m, known[i]);
        check(value == known[i]);
    }

    map_free(P, m);
}

static void test_map_ops_2(paw_Env *P)
{
    int const nrounds = 10;
    Tuple *m = map_new(P);

    for (int iter = 0; iter < nrounds; ++iter) {
        int const start = iter * N;
        for (int i = start; i < start + N; i += 1)
            map_put(P, m, i, i);
        for (int i = start; i < start + N; i += 2)
            map_del(P, m, i);
    }
    for (int i = 0; i < N; i += 1)
        map_put(P, m, i, i * 2);
    for (int i = 0; i < N; i += 2)
        map_del(P, m, i);

    for (int i = 0; i < nrounds * N; ++i) {
        if (i & 1) {
            int const scale = i < N ? 2 : 1;
            check(map_get(P, m, i) == i * scale);
        } else {
            check(map_try(P, m, i) == NULL);
        }
    }

    map_free(P, m);
}

static void test_map_extend(paw_Env *P)
{
    Tuple *a = map_new(P);
    Tuple *b = map_new(P);
    map_put(P, a, 1, 10);
    map_put(P, a, 2, 20);
    map_put(P, a, 3, 30);
    map_put(P, a, 4, 40);

    map_put(P, b, 3, 31);
    map_put(P, b, 4, 41);
    map_put(P, b, 5, 51);

    map_del(P, a, 4);

    pawMap_extend(P, a, b);

    check(10 == map_get(P, a, 1));
    check(20 == map_get(P, a, 2));
    check(31 == map_get(P, a, 3));
    check(41 == map_get(P, a, 4));
    check(51 == map_get(P, a, 5));

    map_free(P, b);
    map_free(P, a);
}

static void test_strings(paw_Env *P)
{
    paw_push_nstring(P, "fixed\0\1", 7);
    void const *fixed = P->top.p[-1].p;

    int total_words = 0;
    char const data[] =
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    for (size_t wordlen = 1; wordlen < 26; ++wordlen) {
        size_t const nwords = PAW_LENGTHOF(data) - wordlen;
        for (size_t i = 0; i < nwords; ++i) {
            paw_push_nstring(P, data + i, wordlen);
            ++total_words;
        }
        if (total_words > 50) {
            int const npop = total_words / 3;
            total_words -= npop;
            paw_pop(P, npop);
        }
    }
    paw_push_nstring(P, "fixed\0\1", 7);
    check(fixed == P->top.p[-1].p);
}

static void test_stack(paw_Env *P)
{
    int const n = paw_get_count(P);
    paw_push_zero(P, 2);
    check(paw_get_count(P) == n + 2);
    check(paw_int(P, n) == 0);
    check(paw_int(P, n + 1) == 0);
}

static void driver(char const *name, void (*callback)(paw_Env *))
{
    struct TestAlloc a = {0};
    fprintf(stderr, "running %s...\n", name);
    paw_Env *P = test_open(test_mem_hook, &a, 0);
    callback(P);
    test_close(P, &a);
}
#define DRIVER(callback) driver(#callback, callback)

static int parse_int(paw_Env *P, char const *text)
{
    paw_Int i;
    return pawV_parse_int(P, text, 0, &i);
}

static void roundtrip_int(paw_Env *P, paw_Int i);

static void pac_int_aux(paw_Env *P, char const *text, paw_Int result)
{
    paw_Int i;
    check(PAW_OK == pawV_parse_int(P, text, 0, &i));
    check(i == result);
}

static void roundtrip_int(paw_Env *P, paw_Int i)
{
    paw_push_int(P, i);
    paw_int_to_string(P, -1, NULL);
    char const *str = paw_string(P, -1);
    pac_int_aux(P, ERASE_TYPE(str), i);
    paw_pop(P, 1);
}

static void parse_and_check_int(paw_Env *P, char const *text, paw_Int result)
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

static int parse_float(paw_Env *P, char const *text)
{
    paw_Float f;
    return pawV_parse_float(P, text, &f);
}

static void pac_float_aux(paw_Env *P, char const *text, paw_Float result)
{
    paw_Float f;
    check(PAW_OK == pawV_parse_float(P, text, &f));
    check(f == result);
}

static void roundtrip_float(paw_Env *P, paw_Float f)
{
    paw_push_float(P, f);
    paw_float_to_string(P, -1, NULL);
    char const *str = paw_string(P, -1);
    pac_float_aux(P, ERASE_TYPE(str), f);
    paw_pop(P, 1);
}

static void parse_and_check_float(paw_Env *P, char const *text, paw_Float result)
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
#define CHECK_BOUND(X, v) (                  \
    opcode = 0,                              \
    SET_##X(&opcode, v) /* set immediate */, \
    SET_OP(&opcode, 0) /* corrupt */,        \
    (v) == GET_##X(opcode) ? (void)0 : (fprintf(stderr, "'%s' unrepresentable by operand '%s'\n", #v, #X), abort()))
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

paw_Bool int_equals(struct Compiler *C, paw_Int a, paw_Int b)
{
    PAW_UNUSED(C);
    return a == b;
}

paw_Uint int_hash(struct Compiler *C, paw_Int i)
{
    PAW_UNUSED(C);
    paw_Uint const u = CAST(paw_Uint, i);
    return u;

    return u << 5 | u >> 5;
}

DEFINE_MAP(struct Compiler, TestMap, pawP_alloc, int_hash, int_equals, int, int)

void test_compiler_map(struct Compiler *C)
{
    TestMap *map = TestMap_new(C, C->pool);
    int const n = 1024;

    for (int i = 0; i < n; ++i) {
        TestMap_insert(C, map, i, i);
    }

    for (int i = 0; i < n; ++i) {
        check(*TestMap_get(C, map, i) == i);
    }
    check(TestMap_length(map) == n);

    for (int i = 0; i < n; ++i) {
        if (i % 2 == 0)
            TestMap_remove(C, map, i);
    }

    for (int i = 0; i < n; ++i) {
        if (i % 2 == 0)
            check(TestMap_get(C, map, i) == NULL);
        else
            check(*TestMap_get(C, map, i) == i);
    }
    check(TestMap_length(map) == n / 2);

    for (int i = 0; i < n; ++i) {
        TestMap_remove(C, map, i);
    }
    check(TestMap_length(map) == 0);

    TestMap_delete(C, map);
}

DEFINE_MAP_ITERATOR(TestMap, int, int)

void test_compiler_map_iterator(struct Compiler *C)
{
    TestMap *map = TestMap_new(C, C->pool);
    int const n = 256;

    for (int i = 0; i < n; ++i) {
        TestMap_insert(C, map, i, i);
    }

    TestMapIterator iter;

    TestMapIterator_init(map, &iter);
    while (TestMapIterator_is_valid(&iter)) {
        int const key = TestMapIterator_key(&iter);
        int *pvalue = TestMapIterator_valuep(&iter);
        if (key % 2 == 0) {
            TestMapIterator_erase(&iter);
        } else {
            TestMapIterator_next(&iter);
            *pvalue = key * 2;
        }
    }

    TestMapIterator_init(map, &iter);
    while (TestMapIterator_is_valid(&iter)) {
        int const key = TestMapIterator_key(&iter);
        int const value = *TestMapIterator_valuep(&iter);
        check(key % 2 == 1);
        check(value == key * 2);
        TestMapIterator_next(&iter);
    }
    check(TestMap_length(map) == n / 2);

    TestMap_delete(C, map);
}

void compiler_driver(char const *name, void (*callback)(struct Compiler *))
{
    struct TestAlloc a = {0};
    fprintf(stderr, "running %s...\n", name);
    paw_Env *P = test_open(test_mem_hook, &a, 0);

    struct Compiler C = {0};
    struct DynamicMem dm = {0};
    pawP_startup(P, &C, &dm, "test");
    callback(&C);
    pawP_teardown(P, &dm);

    test_close(P, &a);
}
#define COMPILER_DRIVER(callback) compiler_driver(#callback, callback)

int main(void)
{
    test_primitives();
    test_immediates();

    DRIVER(test_strings);
    DRIVER(test_stack);
    DRIVER(test_map_get_and_put);
    DRIVER(test_map_erase);
    DRIVER(test_map_erase_2);
    DRIVER(test_map_ops);
    DRIVER(test_map_ops_2);
    DRIVER(test_map_extend);
    DRIVER(test_parse_int);
    DRIVER(test_parse_float);
    DRIVER(test_buffer);

    COMPILER_DRIVER(test_compiler_map);
    COMPILER_DRIVER(test_compiler_map_iterator);
    return 0;
}
