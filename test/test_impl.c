#include "call.h"
#include "map.h"
#include "paw.h"
#include "rt.h"
#include "test.h"
#include "util.h"
#include "value.h"
#include "vector.h"
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

    v_set_0(&v);
    check(v.u == 0);

    v_set_bool(&v, PAW_TRUE);
    check(v_true(v));
    v_set_bool(&v, PAW_FALSE);
    check(!v_true(v));

    v_set_float(&v, 0.0);
    check(v_float(v) == 0.0);
    v_set_float(&v, 12.3);
    check(v_float(v) == 12.3);
    v_set_float(&v, 12.3e123);
    check(v_float(v) == 12.3e123);
    v_set_float(&v, INFINITY);
    check(!isfinite(v_float(v)));
    v_set_float(&v, -INFINITY);
    check(!isfinite(v_float(v)));
    v_set_float(&v, nan(""));
    check(isnan(v_float(v)));
    v_set_float(&v, DBL_MAX);
    check(isfinite(v_float(v)));
    v_set_float(&v, DBL_MIN);
    check(isfinite(v_float(v)));

    v_set_int(&v, 0);
    check(v_int(v) == 0);
    v_set_int(&v, 123);
    check(v_int(v) == 123);
    v_set_int(&v, -123);
    check(v_int(v) == -123);
    v_set_int(&v, PAW_INT_MAX);
    check(v_int(v) == PAW_INT_MAX);
    v_set_int(&v, PAW_INT_MIN);
    check(v_int(v) == PAW_INT_MIN);
}

#define N 500

static Map *map_new(paw_Env *P)
{
    Value *pv = pawC_stkinc(P, 1);
    Map *m = pawH_new(P);
    v_set_object(pv, m); // anchor
    return m;
}

static void map_free(paw_Env *P, Map *map)
{
    paw_assert(map == P->top.p[-1].p);
    pawC_pop(P);
}

static paw_Int map_get(paw_Env *P, Map *map, paw_Int k)
{
    const Value key = {.i = k};
    const Value *pvalue = pawH_get(P, map, key);
    paw_assert(pvalue != NULL);
    return pvalue->i;
}

static const paw_Int *map_try(paw_Env *P, Map *map, paw_Int k)
{
    const Value key = {.i = k};
    const Value *pvalue = pawH_get(P, map, key);
    return pvalue ? &pvalue->i : NULL;
}

static void map_put(paw_Env *P, Map *map, paw_Int k, paw_Int v)
{
    const Value key = {.i = k};
    const Value value = {.i = v};
    pawH_insert(P, map, key, value);
}

static void map_del(paw_Env *P, Map *map, paw_Int k)
{
    const Value key = {.i = k};
    pawH_remove(P, map, key);
}

static void dump_map(Map *m)
{
    printf("Map{\n");
    for (size_t i = 0; i < m->capacity; ++i) {
        const MapMeta *mm = pawH_meta(m, i);
        printf("    %.4zu: ", i);
        if (mm->state == MAP_ITEM_OCCUPIED) {
            printf("%" PRId64 ": %" PRId64 "\n",
                   pawH_key(m, i)->i, pawH_value(m, i)->i);
        } else if (mm->state == MAP_ITEM_ERASED) {
            printf("<erased>\n");
        } else if (mm->state == MAP_ITEM_VACANT) {
            printf("<vacant>\n");
        }
    }
    printf("}\n");
}

static void test_map1(paw_Env *P)
{
    Map *m = map_new(P);
    map_put(P, m, 1, 1);
    map_put(P, m, 2, 2);
    map_put(P, m, 3, 3);
    check(1 == map_get(P, m, 1));
    check(2 == map_get(P, m, 2));
    check(3 == map_get(P, m, 3));
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

    map_del(P, m, 1);
    map_del(P, m, 2);
    map_del(P, m, 4);
    map_del(P, m, 5);

    check(NULL == map_try(P, m, 1));
    check(NULL == map_try(P, m, 2));
    check(3 == map_get(P, m, 3));
    check(NULL == map_try(P, m, 4));
    check(NULL == map_try(P, m, 5));
    check(6 == map_get(P, m, 6));
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

    check(cast_size(paw_length(P, -1)) == paw_countof(known));

    // Fill the map with nonnegative integers (may have repeats).
    paw_Int integers[N];
    for (int i = 0; i < N; ++i) {
        const paw_Int ival = test_randint(0, 10000);
        map_put(P, m, ival, ival);
        integers[i] = ival;
    }

    check(cast_size(paw_length(P, -1)) <= N + paw_countof(known));

    // Erase all nonnegative integers.
    paw_Int itr = PAW_ITER_INIT;
    while (pawH_iter(m, &itr)) {
        const Value key = *pawH_key(m, cast_size(itr));
        if (v_int(key) >= 0) {
            map_del(P, m, key.i);
        }
    }

    check(cast_size(pawH_length(m)) <= paw_countof(known));

    // Check known items.
    for (size_t i = 0; i < paw_countof(known); ++i) {
        const paw_Int value = map_get(P, m, known[i]);
        check(value == known[i]);
    }

    map_free(P, m);
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
    paw_Env *P = paw_open(test_alloc, &a);
    callback(P);
    test_close(P, &a);
}

int main(void)
{
    test_primitives();
    driver(test_stack);
    driver(test_map1);
    driver(test_map2);
    driver(test_map3);
    return 0;
}
