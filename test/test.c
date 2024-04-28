#include "test.h"
#include "env.h"
//#include "lib.h"
#include "os.h"
#include "paw.h"
//#include "rt.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gc.h>

#if 0
#define TEST_FIND_LEAK
// Define TEST_FIND_LEAK to have the program print out the addresses and
// sizes of leaked blocks. Watchpoints can be used to figure out exactly
// where the block was allocated. Note that the tests are very slow with
// this option enabled.
#ifdef TEST_FIND_LEAK
struct Chunk {
    void *data;
    size_t size;
};

static struct {
    struct Chunk *blocks;
    size_t size;
    size_t alloc;
} s_chunks;

static struct Chunk *get_chunk(void *ptr)
{
    if (s_chunks.size == s_chunks.alloc) {
        s_chunks.alloc = paw_max(s_chunks.alloc * 2, 256);
        s_chunks.blocks = realloc(s_chunks.blocks, s_chunks.alloc * sizeof(s_chunks.blocks[0]));
    }
    for (size_t i = 0; i < s_chunks.size; ++i) {
        struct Chunk *c = &s_chunks.blocks[i];
        if (ptr == c->data) {
            return c;
        }
    }
    struct Chunk *c = &s_chunks.blocks[s_chunks.size++];
    *c = (struct Chunk){
        .data = ptr,
    };
    return c;
}
#endif // TEST_FIND_LEAK

static unsigned *get_block(struct TestAlloc *a, size_t size)
{
    if (size < BLOCK_LIMIT) {
        return &a->blocks[size];
    }
    // Chunks past BLOCK_LIMIT represent BLOCK_LIMIT bytes each, except for the last block,
    // which represents all allocations greater than the preceeding block. Block 0 is unused.
    size = paw_min(size / BLOCK_LIMIT, BLOCK_LIMIT);
    return &a->blocks[size + BLOCK_LIMIT];
}

static void register_block(struct TestAlloc *a, size_t size0, size_t size)
{
    if (size0) {
        unsigned *block0 = get_block(a, size0);
        check(*block0 > 0);
        --*block0;
    }
    if (size) {
        unsigned *block = get_block(a, size);
        ++*block;
    }
}

// Print out information about leaked allocations (# blocks leaked, block size)
static void report_nonzero_blocks(struct TestAlloc *a)
{
    size_t nonzero = 0;
#ifdef TEST_FIND_LEAK
    // Print the exact pointers that were leaked.
    for (size_t i = 0; i < s_chunks.size; ++i) {
        struct Chunk c = s_chunks.blocks[i];
        if (c.size) {
            fprintf(stderr, "leaked %zu bytes at address %p\n", c.size, c.data);
        }
    }
#else
    // Just give a rough indication of what size blocks were leaked.
    unsigned *block = a->blocks;
    for (size_t i = 0; i < BLOCK_LIMIT; ++i, ++block) {
        if (*block) {
            fprintf(stderr, "leaked %u block(s) of size %zu\n", *block, i);
            ++nonzero;
        }
    }
    for (size_t i = 0; i < BLOCK_LIMIT; ++i, ++block) {
        if (*block) {
            fprintf(stderr, "leaked %u block(s) of size between %zu and %zu\n",
                    *block, (i + 1) * BLOCK_LIMIT, (i + 2) * BLOCK_LIMIT);
            ++nonzero;
        }
    }
    if (*block) {
        fprintf(stderr, "leaked %u large block(s)\n", *block);
        ++nonzero;
    }
#endif // !TEST_FIND_LEAK
    if (nonzero) {
        fprintf(stderr, "leaked %zu allocations (%zu bytes total)\n",
                nonzero, a->nbytes);
        abort();
    }
}
#endif // 0

static void trash_memory(void *ptr, size_t n)
{
    volatile uint8_t *p = ptr;
    for (size_t i = 0; i < n; ++i) {
        *p++ = 0xAA; // 0b10101010
    }
}

static void *safe_realloc(struct TestAlloc *a, void *ptr, size_t size0, size_t size)
{
    check(a->nbytes >= size0);
//    register_block(a, size0, size);
    void *ptr2 = size ? GC_MALLOC(size) : NULL;
    check(!size || ptr2); // assume success
    if (ptr2) {
        if (ptr) {
            // resize: copy old contents
            memcpy(ptr2, ptr, paw_min(size0, size));
        }
        if (size0 < size) {
            // grow: fill uninitialized memory
            trash_memory((char *)ptr2 + size0, size - size0);
        }
    }
#ifdef TEST_FIND_LEAK
    if (ptr) {
        struct Chunk *c = get_chunk(ptr);
        check(c->size == size0);
        c->size = 0;
    }
    if (ptr2) {
        struct Chunk *c = get_chunk(ptr2);
        check(c->size == 0);
        c->size = size;
    }
#endif
    if (ptr) {
        // Trash the old allocation in an attempt to mess up any code
        // that still depends on it.
        trash_memory(ptr, size0);
    }
    a->nbytes += size - size0;
    GC_FREE(ptr);
    return ptr2;
}

void *test_alloc(void *ud, void *ptr, size_t size0, size_t size)
{
    struct TestAlloc *a = ud;
    return safe_realloc(a, ptr, size0, size);
}

static void next_chunk(struct TestReader *rd)
{
    rd->index = 0;
    if (rd->file) {
        rd->length = pawO_read(rd->file, rd->buf, sizeof(rd->buf));
        return;
    }
    const size_t n = paw_min(rd->ndata, READ_MAX);
    memcpy(rd->buf, rd->data, n);
    rd->data += n;
    rd->ndata -= n;
    rd->length = n;
}

// Read a source file in small chunks to make sure the parser can work incrementally
const char *test_reader(paw_Env *P, void *ud, size_t *size)
{
    paw_unused(P);
    struct TestReader *rd = ud;
    if (!rd->length) {
        next_chunk(rd);
        if (!rd->length) {
            return NULL;
        }
    }
    const size_t r = (size_t)rand();
    *size = paw_max(1, r % rd->length);
    const char *ptr = rd->buf + rd->index;
    rd->length -= *size;
    rd->index += *size;
    return ptr;
}

static const char *test_pathname(const char *name)
{
    static char s_buf[paw_lengthof(TEST_PREFIX) + 64];
    s_buf[0] = '\0'; // Reset length
    strcat(s_buf, TEST_PREFIX);
    strcat(s_buf, "scripts/");
    strcat(s_buf, name);
    strcat(s_buf, ".paw");
    return s_buf;
}

paw_Env *test_open(paw_Alloc alloc, struct TestAlloc *state)
{
    return paw_open(alloc ? alloc : test_alloc, state);
}

void test_close(paw_Env *P, struct TestAlloc *a)
{
    paw_close(P);

//    if (a->nbytes) {
//        fprintf(stderr, "error: leaked %zu bytes\n", a->nbytes);
//        report_nonzero_blocks(a);
//        abort();
//    }
}

static void check_ok(paw_Env *P, int status)
{
    if (status != PAW_OK) {
        test_recover(P, PAW_TRUE); // no return
    }
}

#include "debug.h"
int test_open_file(paw_Env *P, const char *name)
{
    const char *pathname = test_pathname(name);
    if (!P) {
        return PAW_EMEMORY;
    }
    FILE *file = pawO_open(pathname, "r");
    struct TestReader rd = {.file = file};
    rd.data = rd.buf;
    check(file);

    const int status = paw_load(P, test_reader, pathname, &rd);
    pawO_close(rd.file);
puts("TODO: remove");
paw_dump_source(P, v_closure(P->top.p[-1])->p);
    return status;
}

int test_open_string(paw_Env *P, const char *source)
{
    struct TestReader rd = {.data = source, .ndata = strlen(source)};
    return paw_load(P, test_reader, "<string>", &rd);
}

void test_recover(paw_Env *P, paw_Bool fatal)
{
    // Expect an error message on top of the stack.
    check(paw_get_count(P) >= 1);

    if (fatal) {
        const char *s = paw_string(P, -1);
        const size_t n = paw_length(P, -1);
        fprintf(stderr, "%.*s\n", (int)n, s);
        abort();
    }
    paw_pop(P, 1);
}

void test_script(const char *name, struct TestAlloc *a)
{
    paw_Env *P = test_open(test_alloc, a);
    check_ok(P, test_open_file(P, name));
    check_ok(P, paw_call(P, 0));
    test_close(P, a);
}

paw_Int test_randint(paw_Int min, paw_Int max)
{
    check(min <= max);
    return rand() % (max - min) + min;
}

void test_randstr(char *str, int len)
{
    static const char kChars[] =
        "0123456789"
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
        " \t\n\r\v\f";
    for (int i = 0; i < len; ++i) {
        str[i] = test_randint(0, sizeof(kChars) - 1);
    }
}
