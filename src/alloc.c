// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "alloc.h"
#include "env.h"
#include "mem.h"

#define HEAP_META_SIZE (PAW_ROUND_UP(sizeof(struct Heap)) + \
                        PAW_ROUND_UP(sizeof(struct Allocator)))

#define OS_MALLOC(P, size) ((P)->alloc((P)->ud, NULL, 0, size))
#define OS_FREE(P, ptr, size) ((P)->alloc((P)->ud, ptr, size, 0))

#if defined(__has_feature)
# if __has_feature(address_sanitizer) && !defined(__SANITIZE_ADDRESS__)
#  define __SANITIZE_ADDRESS__
# endif
#endif

#if defined(__SANITIZE_ADDRESS__)
int __asan_address_is_poisoned(void const volatile *addr);
void __asan_poison_memory_region(const volatile void *ptr, size_t size);
void __asan_unpoison_memory_region(const volatile void *ptr, size_t size);
# define POISON_MEMORY_REGION(p, z) __asan_poison_memory_region(p, z)
# define UNPOISON_MEMORY_REGION(p, z) __asan_unpoison_memory_region(p, z)
#else 
# define POISON_MEMORY_REGION(p, z)
# define UNPOISON_MEMORY_REGION(p, z)
#endif

#define FLAGS_PER_BYTE 8
#define FLAG_BASE(H, uptr) CHECK_EXP(Z_IN_BOUNDS(H, uptr), ((uptr) - (H)->bounds[0]) / sizeof(void *))
#define FLAG_ID(base) ((base) / FLAGS_PER_BYTE)
#define FLAG_BIT(base) ((base) % FLAGS_PER_BYTE)

struct ChunkId {
    uint32_t v;
};

struct Chunk {
    union {
        struct {
            uint32_t prev_size;
            uint32_t size4x;
        } hdr; 
        struct {
            struct ChunkId prev;
            struct ChunkId next;
        } list; 
    };
};

#define CHUNK_HDR(L, i) (&(L)->chunks[(i) - 1].hdr)
#define CHUNK_LIST(L, i) (&(L)->chunks[i].list)

#define MAX_SMALL 10
#define NUM_HASH 61
#define KEY_CHUNK_SHIFT 3
#define CHUNK_SIZE sizeof(struct Chunk)
#define BAD_CHUNK 0

// Allocator based off of mem3.c from SQLite
struct Allocator {
    struct ChunkId small[MAX_SMALL - 1];
    struct ChunkId hash[NUM_HASH];
    struct Chunk *chunks;
    size_t nchunks;

    paw_Alloc alloc;
    paw_Env *P;

    struct ChunkId key;
    uint32_t key_size;
    uint32_t key_min;
};

_Static_assert(PAW_HEAP_MIN >= HEAP_META_SIZE, "PAW_HEAP_MIN is too small");

uint8_t pawZ_get_flag(const struct Heap *H, uintptr_t uptr)
{
    const size_t id = FLAG_BASE(H, uptr);
    const uint8_t flag = H->flags[FLAG_ID(id)];
    return (flag >> FLAG_BIT(id)) & 1;
}

void pawZ_set_flag(struct Heap *H, uintptr_t uptr)
{
    const size_t id = FLAG_BASE(H, uptr);
    uint8_t *pflag = &H->flags[FLAG_ID(id)];
    paw_assert(0 == (*pflag & (1 << FLAG_BIT(id))));
    *pflag = *pflag | (1 << FLAG_BIT(id));
}

void pawZ_clear_flag(struct Heap *H, uintptr_t uptr)
{
    const size_t id = FLAG_BASE(H, uptr);
    uint8_t *pflag = &H->flags[FLAG_ID(id)];
    paw_assert(0 != (*pflag & (1 << FLAG_BIT(id))));
    *pflag = *pflag & ~(1 << FLAG_BIT(id));
}

static void init_chunk_allocator(struct Allocator *a, void *heap, size_t heap_size)
{
    paw_assert(PAW_IS_ALIGNED(heap));
    memset(a, 0, sizeof(*a));

    assert(CHUNK_SIZE == 8);
    a->nchunks = heap_size / CHUNK_SIZE - 2;
    a->chunks = heap;

    a->key_size = a->nchunks;
    a->key_min = a->key_size;
    a->key.v = 1;

    CHUNK_HDR(a, 1)->size4x = (a->key_size << 2) + 2;
    CHUNK_HDR(a, 1 + a->nchunks)->prev_size = a->nchunks;
    CHUNK_HDR(a, 1 + a->nchunks)->size4x = 1;
}

static struct Chunk *chunk_at(struct Allocator *a, struct ChunkId id)
{
    paw_assert(id.v < a->nchunks);
    return &a->chunks[id.v];
}

static void unlink_from_list(struct Allocator *a, struct ChunkId id, struct ChunkId *proot)
{
    struct Chunk *b = chunk_at(a, id);
    const struct ChunkId prev = b->list.prev;
    const struct ChunkId next = b->list.next;
    if (prev.v != BAD_CHUNK) {
        struct Chunk *prev_chunk = chunk_at(a, prev);
        prev_chunk->list.next = next;
    } else {
        *proot = next;
    }
    if (next.v != BAD_CHUNK) {
        struct Chunk *next_chunk = chunk_at(a, next);
        next_chunk->list.prev = prev;
    }
    b->list.prev.v = 0;
    b->list.next.v = 0;
}

static void unlink_chunk(struct Allocator *a, struct ChunkId id)
{
    paw_assert((CHUNK_HDR(a, id.v)->size4x & 1) == 0);
    paw_assert(id.v >= 1);
    const uint32_t size = CHUNK_HDR(a, id.v)->size4x / 4;
    paw_assert(size == CHUNK_HDR(a, id.v + size)->prev_size);
    paw_assert(size >= 2);
    if (size <= MAX_SMALL) {
        unlink_from_list(a, id, &a->small[size - 2]); 
    } else {
        const uint32_t hash = size % NUM_HASH;
        unlink_from_list(a, id, &a->hash[hash]);
    }
}

static void link_into_list(struct Allocator *a, struct ChunkId id, struct ChunkId *proot)
{
    CHUNK_LIST(a, id.v)->next = *proot;
    CHUNK_LIST(a, id.v)->prev.v = 0;
    if (proot->v != BAD_CHUNK) {
        CHUNK_LIST(a, proot->v)->prev = id;
    }
    *proot = id;
}

static void link_chunk(struct Allocator *a, struct ChunkId id)
{
    paw_assert(id.v >= 1);
    paw_assert((CHUNK_HDR(a, id.v)->size4x & 1) == 0);
    const uint32_t size = CHUNK_HDR(a, id.v)->size4x / 4;
    paw_assert(size == CHUNK_HDR(a, id.v + size)->prev_size);
    paw_assert(size >= 2);
    if (size <= MAX_SMALL) {
        link_into_list(a, id, &a->small[size - 2]);
    } else {
        const uint32_t hash = size % NUM_HASH;
        link_into_list(a, id, &a->hash[hash]);
    }
}

static void fix_chunk_list(struct Allocator *a, struct ChunkId *proot)
{
    struct ChunkId next;

    for (struct ChunkId i = *proot; i.v != BAD_CHUNK; i = next){
        next = CHUNK_LIST(a, i.v)->next;
        uint32_t size = CHUNK_HDR(a, i.v)->size4x;
        paw_assert((size & 1) == 0);
        if ((size & 2) == 0) {
            unlink_from_list(a, i, proot);
            paw_assert(i.v > CHUNK_HDR(a, i.v)->prev_size);
            struct ChunkId prev = {i.v - CHUNK_HDR(a, i.v)->prev_size};
            if (prev.v == next.v) {
                next = CHUNK_LIST(a, prev.v)->next;
            }
            unlink_chunk(a, prev);
            size = i.v + size / 4 - prev.v;
            const uint32_t x = CHUNK_HDR(a, prev.v)->size4x & 2;
            CHUNK_HDR(a, prev.v)->size4x = size * 4 | x;
            CHUNK_HDR(a, prev.v + size)->prev_size = size;
            link_chunk(a, prev);
            i = prev;
        } else {
            size /= 4;
        }
        if (size > a->key_size) {
            a->key_size = size;
            a->key = i;
        }
    }
}

static void *checkout_chunk(struct Allocator *a, struct ChunkId i, uint32_t nchunks) 
{
    paw_assert(i.v >= 1);
    paw_assert(CHUNK_HDR(a, i.v)->size4x / 4 == nchunks);
    paw_assert(CHUNK_HDR(a, i.v + nchunks)->prev_size == nchunks);
    const uint32_t x = CHUNK_HDR(a, i.v)->size4x;
    CHUNK_HDR(a, i.v)->size4x = nchunks * 4 | 1 | (x & 2);
    CHUNK_HDR(a, i.v + nchunks)->prev_size = nchunks;
    CHUNK_HDR(a, i.v + nchunks)->size4x |= 2;
    return &a->chunks[i.v];
}

static void *key_chunk_alloc(struct Allocator *a, uint32_t nchunks)
{
    assert(a->key_size >= nchunks);
    if (nchunks >= a->key_size - 1) {
        void *p = checkout_chunk(a, a->key, a->key_size);
        a->key.v = 0;
        a->key_size = 0;
        a->key_min = 0;
        return p;
    } else {
        struct ChunkId newi = {a->key.v + a->key_size - nchunks};
        assert(newi.v > a->key.v + 1);
        CHUNK_HDR(a, a->key.v + a->key_size)->prev_size = nchunks;
        CHUNK_HDR(a, a->key.v + a->key_size)->size4x |= 2;
        CHUNK_HDR(a, newi.v)->size4x = nchunks * 4 + 1;
        a->key_size -= nchunks;
        CHUNK_HDR(a, newi.v)->prev_size = a->key_size;
        const uint32_t x = CHUNK_HDR(a, a->key.v)->size4x & 2;
        CHUNK_HDR(a, a->key.v)->size4x = a->key_size * 4 | x;
        if (a->key_size < a->key_min) {
            a->key_min = a->key_size;
        }
        return &a->chunks[newi.v];
    }
}

// The maximum number of bytes that can fit in the smallest possible
// allocation is 12, since we have to write the chunk size at the
// end (in the 'prev_size' field of the following chunk's header).
_Static_assert(CHUNK_SIZE == 8, "failed allocator precondition");
#define COMPUTE_NUM_CHUNKS(nbytes) \
    ((nbytes) > 12 ? ((nbytes) + 11) / 8 : 2)
#define TOO_MANY_CHUNKS(nbytes) \
    ((nbytes) > SIZE_MAX - 11 || \
     ((nbytes) + 11) / CHUNK_SIZE > UINT32_MAX)

// Modified from SQLite (mem3.c)
static void *unsafe_malloc(struct Heap *H, size_t nbytes)
{
    struct Allocator *a = H->a;
    nbytes = PAW_ROUND_UP(nbytes);
    if (TOO_MANY_CHUNKS(nbytes)) return NULL;
    const uint32_t nchunks = COMPUTE_NUM_CHUNKS(nbytes);
    paw_assert(nchunks >= 2);

    // search for an exact fit
    if (nchunks <= MAX_SMALL) {
        struct ChunkId id = a->small[nchunks - 2];
        if (id.v != BAD_CHUNK) {
            unlink_from_list(a, id, &a->small[nchunks - 2]);
            return checkout_chunk(a, id, nchunks);
        }
    } else {
        const uint32_t hash = nchunks % NUM_HASH;
        for (struct ChunkId id = a->hash[hash]; 
                id.v != BAD_CHUNK; 
                id = CHUNK_LIST(a, id.v)->next) {
            if (CHUNK_HDR(a, id.v)->size4x / 4 == nchunks) {
                unlink_from_list(a, id, &a->hash[hash]);
                return checkout_chunk(a, id, nchunks);
            }
        }
    }
    
    if (a->key_size >= nchunks) {
        return key_chunk_alloc(a, nchunks);
    }
  
    for (uint32_t to_free = nchunks * 16; 
            to_free < a->nchunks * 16; 
            to_free *= 2) {
        if (a->key.v != BAD_CHUNK) {
            link_chunk(a, a->key);
            a->key.v = BAD_CHUNK;
            a->key_size = 0;
        }
        for (uint32_t i = 0; i < NUM_HASH; ++i) {
            fix_chunk_list(a, &a->hash[i]);
        }
        for (uint32_t i = 0; i < MAX_SMALL - 1; ++i) {
            fix_chunk_list(a, &a->small[i]);
        }
        if (a->key_size != 0) {
            unlink_chunk(a, a->key);
            if (a->key_size >= nchunks) {
                return key_chunk_alloc(a, nchunks);
            }
        }
    }
    return NULL;
}

static void *z_malloc(struct Heap *H, size_t nbytes)
{
    // TODO: lock/unlock mutex
    return unsafe_malloc(H, nbytes);
}

size_t pawZ_sizeof(void *ptr)
{
    paw_assert(ptr != NULL);
    struct Chunk *b = ptr;
    paw_assert((b[-1].hdr.size4x & 1) != 0);
    return (b[-1].hdr.size4x & ~3) * 2 - 4;
}

// Modified from SQLite (mem3.c)
static void unsafe_free(struct Heap *H, void *ptr)
{
    struct Chunk *b = ptr;
    if (b == NULL) return;

    struct Allocator *a = H->a;
    paw_assert(b > a->chunks && b < &a->chunks[a->nchunks]);
    const struct ChunkId i = {b - a->chunks};
    uint32_t nchunks = CHUNK_HDR(a, i.v)->size4x / 4;
    paw_assert((CHUNK_HDR(a, i.v)->size4x & 1) == 1);
    paw_assert(nchunks == CHUNK_HDR(a, i.v)->size4x / 4);
    paw_assert(i.v + nchunks <= a->nchunks + 1);
    CHUNK_HDR(a, i.v)->size4x &= ~1;
    CHUNK_HDR(a, i.v + nchunks)->prev_size = nchunks;
    CHUNK_HDR(a, i.v + nchunks)->size4x &= ~2;
    link_chunk(a, i);
  
    if (a->key.v != BAD_CHUNK) {
        while ((CHUNK_HDR(a, a->key.v)->size4x & 2) == 0) {
            nchunks = CHUNK_HDR(a, a->key.v)->prev_size;
            a->key.v -= nchunks;
            a->key_size += nchunks;
            unlink_chunk(a, a->key);
            const uint32_t x = CHUNK_HDR(a, a->key.v)->size4x & 2;
            CHUNK_HDR(a, a->key.v)->size4x = a->key_size * 4 | x;
            a->chunks[a->key.v + a->key_size-1].hdr.prev_size = a->key_size;
        }
        const uint32_t x = CHUNK_HDR(a, a->key.v)->size4x & 2;
        while ((CHUNK_HDR(a, a->key.v + a->key_size)->size4x & 1) == 0) {
            unlink_chunk(a, (struct ChunkId){a->key.v + a->key_size});
            a->key_size += CHUNK_HDR(a, a->key.v + a->key_size)->size4x / 4;
            CHUNK_HDR(a, a->key.v)->size4x = a->key_size * 4 | x;
            CHUNK_HDR(a, a->key.v + a->key_size)->prev_size = a->key_size;
        }
    }
}

static void z_free(struct Heap *H, void *ptr)
{
    // TODO: lock/unlock mutex
    unsafe_free(H, ptr);
}

// TODO: poison redzones around heap regions, add total redzone size to
//       HEAP_META_SIZE
static size_t compute_flag_count(size_t h)
{
    const size_t F = FLAGS_PER_BYTE;
    const size_t m = HEAP_META_SIZE;
    const size_t p = sizeof(void *);
    return F * (h - m) / (F * p + 1);
}

int pawZ_init(paw_Env *P, void *heap, size_t heap_size, paw_Bool is_owned)
{
    paw_assert(heap != NULL);
    paw_assert(PAW_IS_ALIGNED(heap));
    paw_assert(PAW_IS_ALIGNED(heap_size));
    const size_t nf = compute_flag_count(heap_size);

    // initialize heap manager
    struct Heap *H = heap;
    {
#define SKIP_CHUNK(z) (heap = BUMP_PTR(heap, PAW_ROUND_UP(z)), \
                       heap_size -= PAW_ROUND_UP(z))
        P->H = H;
        *H = (struct Heap){
            .is_owned = is_owned,
            .nflags = nf, 
            .P = P,
        };
        const size_t flag_zone = (nf + FLAGS_PER_BYTE - 1) / FLAGS_PER_BYTE;
        memset(H->flags, 0, flag_zone);
        SKIP_CHUNK(sizeof(struct Heap));
        SKIP_CHUNK(flag_zone);
        H->a = heap;
        SKIP_CHUNK(sizeof(struct Allocator));
        init_chunk_allocator(H->a, heap, heap_size);
#undef SKIP_CHUNK
    }

    const size_t zf = nf * sizeof(void *);
    paw_assert(heap_size >= zf);
    H->bounds[0] = CAST_UPTR(heap);
    H->bounds[1] = H->bounds[0] + zf;
    return PAW_OK;

no_memory:
    pawZ_uninit(P);
    return PAW_EMEMORY;
}

// TODO: option for detect_leaks(P), and don't print anything to the console
static void detect_leaks(paw_Env *P)
{
#if 0
    paw_Bool leak_detected = PAW_FALSE;
    const struct Heap *H = P->H;
    for (uintptr_t u = H->bounds[0]; u < H->bounds[1]; u += sizeof(void *)) {
        if (pawZ_get_flag(H, u) != 0) {
            fprintf(stderr, "leak @ %p\n", ERASE_TYPE(u));
            leak_detected = PAW_TRUE;
        }     
    }
    if (leak_detected || P->gc_bytes > 0){
        printf("%zu bytes not accounted for\n", P->gc_bytes);
        __builtin_trap();
    }
#endif // 0
}

void pawZ_uninit(paw_Env *P)
{
    detect_leaks(P);
    if (P->H->is_owned) {
        P->alloc(P->ud, P, P->heap_size, 0);
    }
}

#define CHECK_UNUSED(H, ptr) paw_assert(!pawZ_get_flag(H, CAST_UPTR(ptr)))

static void *z_realloc(struct Heap *H, void *old_ptr, size_t new_size)
{
    if (old_ptr == NULL) {
        return z_malloc(H, new_size);
    }
    if (new_size == 0) {
        z_free(H, old_ptr);
        return 0;
    }
    const size_t old_size = pawZ_sizeof(old_ptr);
    if (new_size <= old_size && new_size >= old_size - 128) {
        return old_ptr;
    }
    void *new_ptr = unsafe_malloc(H, new_size);
    if (new_ptr != NULL) {
        memcpy(new_ptr, old_ptr, PAW_MIN(old_size, new_size));
        unsafe_free(H, old_ptr);
    }
    return new_ptr;
}

void *pawZ_alloc(paw_Env *P, void *ptr, size_t size)
{
    struct Heap *H = P->H;
    if (size == 0) {
        z_free(H, ptr);
        return NULL;
    } 
    if (ptr == NULL) return z_malloc(H, size);
    return z_realloc(H, ptr, size);
}
