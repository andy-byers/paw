#include "array.h"
#include "call.h"
#include "map.h"
#include "paw.h"
#include "rt.h"
#include "test.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// Test the primitive value representations
static void test_primitives(void)
{
    Value v;

    // VNULL
    v_set_null(&v);
    check(v_is_null(v));

    // VTRUE/FALSE
    v_set_bool(&v, PAW_TRUE);
    check(v_type(v) == VTRUE);
    check(v_true(v));
    check(pawV_is_true(v));
    check(!pawV_is_false(v));
    v_set_bool(&v, PAW_FALSE);
    check(v_type(v) == VFALSE);
    check(!v_true(v));
    check(!pawV_is_true(v));
    check(pawV_is_false(v));
    check(!pawV_is_object(v));

    // pawV_is_float(v)
    v_set_float(&v, 0.0);
    check(v_type(v) <= VNUMBER);
    check(v_float(v) == 0.0);
    check(pawV_is_float(v));
    v_set_float(&v, 12.3);
    check(v_type(v) <= VNUMBER);
    check(v_float(v) == 12.3);
    check(pawV_is_float(v));
    v_set_float(&v, 12.3e123);
    check(v_type(v) <= VNUMBER);
    check(v_float(v) == 12.3e123);
    check(pawV_is_float(v));
    check(!pawV_is_object(v));
    v_set_float(&v, INFINITY);
    check(v_type(v) <= VNUMBER);
    check(!isfinite(v_float(v)));
    check(pawV_is_float(v));
    check(!pawV_is_object(v));
    v_set_float(&v, -INFINITY);
    check(v_type(v) <= VNUMBER);
    check(!isfinite(v_float(v)));
    check(pawV_is_float(v));
    check(!pawV_is_object(v));
    v_set_float(&v, nan(""));
    check(v_type(v) <= VNUMBER);
    check(isnan(v_float(v)));
    check(pawV_is_float(v));
    check(!pawV_is_object(v));
    v_set_float(&v, DBL_MAX);
    check(v_type(v) <= VNUMBER);
    check(isfinite(v_float(v)));
    check(pawV_is_float(v));
    check(!pawV_is_object(v));
    v_set_float(&v, DBL_MIN);
    check(v_type(v) <= VNUMBER);
    check(isfinite(v_float(v)));
    check(pawV_is_float(v));
    check(!pawV_is_object(v));

    // pawV_is_int(v)
    v_set_int(&v, 0);
    check(v_type(v) == VNUMBER);
    check(v_int(v) == 0);
    check(pawV_is_int(v));
    v_set_int(&v, 123);
    check(v_type(v) == VNUMBER);
    check(v_int(v) == 123);
    check(pawV_is_int(v));
    v_set_int(&v, -123);
    check(v_type(v) == VNUMBER);
    check(v_int(v) == -123);
    check(pawV_is_int(v));
    v_set_int(&v, VINT_MAX);
    check(v_type(v) == VNUMBER);
    check(v_int(v) == VINT_MAX);
    check(pawV_is_int(v));
    v_set_int(&v, VINT_MIN);
    check(v_type(v) == VNUMBER);
    check(v_int(v) == VINT_MIN);
    check(pawV_is_int(v));
    check(!pawV_is_object(v));
}

static void test_objects(void)
{
    Value v;

    // Make sure the value representation can store 48 bits of pointer info.
    // We actually have to shift pointers to the right by 1 bit for this to
    // work, so the fake pointer must be aligned to at least 2.
    // 0b101010101010101010101010101010101010101010101010
    void *fake_ptr = (void *)187649984473770;
    Map *fake_obj = fake_ptr;

    v_set_map(&v, fake_obj);
    check(v_type(v) == VMAP);
    check(pawV_is_map(v));
    check(pawV_is_object(v));

    Map *m = v_map(v);
    check(m == fake_ptr);
}

#define N 500

static void test_map(paw_Env *P)
{
    StackPtr sp = pawC_stkinc(P, 1);
    Map *m = pawH_new(P);
    v_set_map(sp++, m); // Anchor

    // Add known integers for validation.
    const paw_Int known[] = {-1, -2, -10, -20, -100, -200};
    for (size_t i = 0; i < paw_countof(known); ++i) {
        paw_push_value(P, -1);
        paw_push_int(P, known[i]);
        paw_push_int(P, known[i]);
        pawR_setitem(P);
    }

    check(cast_size(paw_length(P, -1)) == paw_countof(known));

    // Fill the map with nonnegative integers (may have repeats).
    paw_Int integers[N];
    for (int i = 0; i < N; ++i) {
        const paw_Int ival = test_randint(0, VINT_MAX);
        paw_push_value(P, -1);
        paw_push_int(P, ival);
        paw_push_int(P, ival);
        pawR_setitem(P);
        integers[i] = ival;
    }

    check(cast_size(paw_length(P, -1)) <= N + paw_countof(known));

    // Add some strings (should not affect the integers).
    char strings[N][64];
    static const size_t ns = sizeof(strings[0]);
    for (int i = 0; i < N; ++i) {
        test_randstr(strings[i], ns);
        paw_push_value(P, -1);
        paw_push_nstring(P, strings[i], ns);
        paw_push_nstring(P, strings[i], ns);
        pawR_setitem(P);
    }

    check(cast_size(paw_length(P, -1)) <= 2 * N + paw_countof(known));

    // Find all items.
    for (int i = 0; i < N; ++i) {
        paw_push_value(P, -1);
        paw_push_nstring(P, strings[i], ns);
        pawR_getitem(P);
        check(paw_length(P, -1) == ns);
        check(0 == memcmp(strings[i], paw_string(P, -1), ns));
        paw_pop(P, 1);

        paw_push_value(P, -1);
        paw_push_int(P, integers[i]);
        pawR_getitem(P);
        check(paw_int(P, -1) == integers[i]);
        paw_pop(P, 1);
    }

    // Erase all nonnegative integers.
    paw_Int itr = PAW_ITER_INIT;
    while (pawH_iter(m, &itr)) {
        const Value key = m->keys[itr];
        if (pawV_is_int(key) && v_int(key) >= 0) {
            pawH_action(P, m, key, MAP_ACTION_REMOVE);
        }
    }

    check(cast_size(pawH_length(m)) <= N + paw_countof(known));

    // Erase the strings.
    itr = PAW_ITER_INIT;
    while (pawH_iter(m, &itr)) {
        const Value key = m->keys[itr];
        if (pawV_is_string(key)) {
            pawH_action(P, m, key, MAP_ACTION_REMOVE);
        }
    }

    check(cast_size(pawH_length(m)) == paw_countof(known));

    // Check known items.
    for (size_t i = 0; i < paw_countof(known); ++i) {
        Value key;
        v_set_int(&key, known[i]);
        const Value *value = pawH_action(P, m, key, MAP_ACTION_NONE);
        check(value);
        check(v_int(*value) == known[i]);
    }

    pawC_pop(P); // pop map
}

static void test_stack(paw_Env *P)
{
    paw_push_nnull(P, 2);
    check(paw_get_count(P) == 2);
    check(paw_is_null(P, 0));
    check(paw_is_null(P, 1));
}

static void driver(void (*callback)(paw_Env *))
{
    struct TestAlloc a = {0};
    paw_Env *P = paw_open(test_alloc, &a);
    callback(P);
    test_close(P, &a);
}

int main(void)
{
    test_primitives();
    test_objects();

    driver(test_stack);
    driver(test_map);
    return 0;
}
