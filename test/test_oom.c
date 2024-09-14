// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// test_oom.c: Heap exhaustion tests

#include "test.h"
#include "alloc.h"
#include "call.h"
#include "env.h"
#include "type.h"

static int run_tests(paw_Env *P)
{
    struct DefList defs = P->defs;
    for (int i = 0; i < defs.count; ++i) {
        static const char kPrefix[] = "test_";
        static const size_t kLength = paw_lengthof(kPrefix);
        struct Def *def = defs.data[i]; 
        if (!def->hdr.is_pub) continue;
        const String *name = def->hdr.name;
        if (name->length >= kLength &&
                memcmp(name->text, kPrefix, kLength) == 0) {
            check(def->hdr.kind == DEF_FUNC);
            paw_push_zero(P, 1);
            P->top.p[-1] = *Y_PVAL(P, def->func.vid);
            return paw_call(P, 0);
        }
    }
    return PAW_OK;
}

static void check_status(paw_Env *P, int status)
{
    if (status != PAW_OK && status != PAW_EMEMORY) {
        check(paw_get_count(P) >= 1);
        const char *s = paw_string(P, -1);
        fprintf(stderr, "%s\n", s);
        abort();
    }
}

static int run_script_or_chunk(const char *name_or_chunk, size_t heap_size, paw_Bool is_chunk)
{
    paw_Env *P = paw_open(&(struct paw_Options){
                .heap_size = heap_size,
            });
    if (P == NULL) return PAW_EMEMORY;

    int status = is_chunk 
        ? test_open_string(P, name_or_chunk)
        : test_open_file(P, name_or_chunk);
    if (status == PAW_OK) status = run_tests(P);
    check_status(P, status);
    paw_close(P);
    return status;
}

static size_t s_passing_heap_size;
static const char *s_name_or_chunk;
static paw_Bool s_is_chunk;
static int s_count;

static void start_oom(const char *name_or_chunk, paw_Bool is_chunk)
{
    s_count = 0;
    s_passing_heap_size = 0;
    s_name_or_chunk = name_or_chunk;
    s_is_chunk = is_chunk;
}

static int run_one(size_t heap_size)
{
    const int status = run_script_or_chunk(s_name_or_chunk, heap_size, s_is_chunk);
    if (status != PAW_EMEMORY) {
        check(status == PAW_OK);
        s_passing_heap_size = heap_size;
    } else {
        ++s_count; 
    }
    return status;
}

static void finish_oom(void)
{
    check(s_count > 0);

    printf("[PASS] %s: passing_heap_size=%zu, oom_count=%d\n", 
            s_is_chunk ? "(chunk)" : s_name_or_chunk, 
            s_passing_heap_size, s_count);
}

static void test_oom(const char *name_or_chunk, paw_Bool is_chunk)
{
    // list of heap sizes that are too small
    const size_t special_sizes[] = {
        1,
        sizeof(paw_Env),
        sizeof(struct Heap),
        sizeof(struct Heap) + 1,
        sizeof(struct Heap) + 10,
        sizeof(struct Heap) + 100,
        sizeof(struct Heap) + 200,
        sizeof(struct Heap) + 300,
        sizeof(struct Heap) + 500,
        sizeof(struct Heap) + 1000,
        sizeof(struct Heap) + 10000,
    };
    start_oom(name_or_chunk, is_chunk);
    for (size_t i = 0; i < paw_countof(special_sizes); ++i) {
        const int status = run_one(special_sizes[i]);
        check(status == PAW_EMEMORY);
    }

    int status;
    size_t heap_size = 1 << 10;
    do {
        status = run_one(heap_size);
        heap_size += heap_size;
    } while (status != PAW_OK);
    finish_oom();
}

static void test_call_frames(void)
{
    test_oom(
            "fn poly_recur<T>(_: T, n: int) {\n"
            "    if n > 0 {                  \n"
            "        poly_recur(_, n - 1);   \n"
            "    }                           \n"
            "}                               \n"
            "fn recur(n: int) {   \n"
            "    if n > 0 {       \n"
            "        recur(n - 1);\n"
            "    }                \n"
            "}                    \n"
            "pub fn test_call_frames() {\n"
            "    recur(25);\n"
            "    poly_recur(true, 250);\n"
            "    poly_recur(1.0, 2500);\n"
            "}\n", PAW_TRUE);
}

static void test_list_ops(void)
{
    test_oom(
            "fn fix_list() {let list = []; list.push(42);}\n" // TODO: remove
 
            "fn push_n<T>(list: [T], value: T, n: int) {\n"
            "    for i = 0, n {                         \n"
            "        list.push(value);                  \n"
            "    }                                      \n"
            "}                                          \n"
            "pub fn test_lists() {       \n"
            "    let list = [];          \n"
            "    push_n(list, 42, 10000);\n"
            "}\n", PAW_TRUE);
}

int main(void)
{
#define RUN_SCRIPT(name) test_oom(#name, PAW_FALSE);
    TEST_SCRIPTS(RUN_SCRIPT)
#undef RUN_SCRIPT

    test_call_frames();
    test_list_ops();
}
