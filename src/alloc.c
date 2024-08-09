// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// NOTE: The block allocator code is from SQLite (mem3.c)

#include "alloc.h"
#include "env.h"
#include "mem.h"

#define MIN_RUNTIME_SIZE (32 * 1024)
#define MIN_HEAP_SIZE (aligned(sizeof(struct BlockAllocator)) + \
                       aligned(sizeof(struct FastBins)) + \
                       MIN_RUNTIME_SIZE)

#define OS_MALLOC(P, size) ((P)->alloc((P)->ud, NULL, 0, size))
#define OS_FREE(P, ptr, size) ((P)->alloc((P)->ud, ptr, size, 0))

#define BUMP(p, n) ERASE_TYPE(CAST_UPTR(p) + (n))
#define ERASE_TYPE(p) CAST(p, void *)

static inline paw_Bool is_pow2(size_t v) 
{
    return v > 0 && !(v & (v >> 1)); 
}

static inline paw_Bool is_aligned(void *p, size_t align) 
{ 
    return !(CAST_UPTR(p) & (align - 1)); 
}

static inline size_t aligned(size_t v) 
{
    return v + (-v & (PAW_ALIGN - 1)); 
}

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

#define BIN_FACTOR 8
#define MIN_BIN_SIZE (sizeof(Value) * 2)
#define MAX_BIN_SIZE (MIN_BIN_SIZE * FAST_BIN_COUNT)
#define FLAGS_PER_BYTE 8

#define BIN_ID(size) CHECK_EXP((size) > 0 && aligned(size), ((size) - 1) / MIN_BIN_SIZE)
#define FLAG_BASE(H, uptr) CHECK_EXP(Z_IN_BOUNDS(H, uptr), ((uptr) - (H)->bounds[0]) / sizeof(void *))
#define FLAG_ID(base) ((base) / FLAGS_PER_BYTE)
#define FLAG_BIT(base) ((base) % FLAGS_PER_BYTE)

struct BinInfo {
    struct BinInfo *prev;
    char buffer[];
};

struct BlockId {
    uint32_t v;
};

struct Block {
    union {
        struct {
            uint32_t prev_size;
            uint32_t size4x;
        } hdr; 
        struct {
            struct BlockId prev;
            struct BlockId next;
        } list; 
    };
};

#define B_HDR(L, i) (&(L)->blocks[(i) - 1].hdr)
#define B_LIST(L, i) (&(L)->blocks[i].list)

#define BLOCK_NLISTS 61
#define KEY_BLOCK_SHIFT 3
#define BLOCK_SIZE sizeof(struct Block)
#define BAD_BLOCK 0

// Allocator for larger regions of memory
// Based off of mem3.c from SQLite
struct BlockAllocator {
    struct BlockId free[BLOCK_NLISTS];
    struct Block *blocks;
    size_t nblocks;

    paw_Alloc alloc;
    paw_Env *P;

    struct BlockId key;
    uint32_t key_size;
    uint32_t key_min;
};


uint8_t pawZ_get_flag(struct Heap *H, uintptr_t uptr)
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

static void *block_malloc(struct BlockAllocator *, size_t);
static void block_free(struct BlockAllocator *, void *, size_t);

static void init_block_allocator(struct BlockAllocator *a, void *heap, size_t heap_size)
{
    paw_assert(is_aligned(heap, PAW_ALIGN));
    memset(a->free, 0, sizeof(a->free));

    assert(BLOCK_SIZE == 8);
    a->blocks = heap;
    a->nblocks = heap_size / BLOCK_SIZE - 2;

    a->key_size = a->nblocks;
    a->key_min = a->key_size;
    a->key.v = 1;

    B_HDR(a, 1)->size4x = (a->key_size << 2) + 2;
    B_HDR(a, 1 + a->nblocks)->prev_size = a->nblocks;
    B_HDR(a, 1 + a->nblocks)->size4x = 1;
}

static struct Block *block_at(struct BlockAllocator *a, struct BlockId id)
{
    paw_assert(id.v < a->nblocks);
    return &a->blocks[id.v];
}

static void unlink_from_list(struct BlockAllocator *a, struct BlockId id, struct BlockId *proot)
{
    struct Block *b = block_at(a, id);
    const struct BlockId prev = b->list.prev;
    const struct BlockId next = b->list.next;
    if (prev.v != BAD_BLOCK) {
        struct Block *prev_block = block_at(a, prev);
        prev_block->list.next = next;
    } else {
        *proot = next;
    }
    if (next.v != BAD_BLOCK) {
        struct Block *next_block = block_at(a, next);
        next_block->list.prev = prev;
    }
    b->list.prev.v = 0;
    b->list.next.v = 0;
}

static void unlink_block(struct BlockAllocator *a, struct BlockId id)
{
    paw_assert((B_HDR(a, id.v)->size4x & 1) == 0);
    paw_assert(id.v >= 1);
    const uint32_t size = B_HDR(a, id.v)->size4x / 4;
    paw_assert(size == B_HDR(a, id.v + size)->prev_size);
    paw_assert(size >= 2);
    const uint32_t hash = size % BLOCK_NLISTS;
    unlink_from_list(a, id, &a->free[hash]);
}

static void link_into_list(struct BlockAllocator *a, struct BlockId id, struct BlockId *proot)
{
    B_LIST(a, id.v)->next = *proot;
    B_LIST(a, id.v)->prev.v = 0;
    if (proot->v != BAD_BLOCK) {
        B_LIST(a, proot->v)->prev = id;
    }
    *proot = id;
}

static void link_block(struct BlockAllocator *a, struct BlockId id)
{
    paw_assert(id.v >= 1);
    paw_assert((B_HDR(a, id.v)->size4x & 1) == 0);
    const uint32_t size = B_HDR(a, id.v)->size4x / 4;
    paw_assert(size == B_HDR(a, id.v + size)->prev_size);
    paw_assert(size >= 2);
    const uint32_t hash = size % BLOCK_NLISTS;
    link_into_list(a, id, &a->free[hash]);
}

static void fix_block_list(struct BlockAllocator *a, struct BlockId *proot)
{
    struct BlockId next;

    for (struct BlockId i = *proot; i.v != BAD_BLOCK; i = next){
        next = B_LIST(a, i.v)->next;
        uint32_t size = B_HDR(a, i.v)->size4x;
        paw_assert((size & 1) == 0);
        if ((size & 2) == 0) {
            unlink_from_list(a, i, proot);
            paw_assert(i.v > B_HDR(a, i.v)->prev_size);
            struct BlockId prev = {i.v - B_HDR(a, i.v)->prev_size};
            if (prev.v == next.v) {
                next = B_LIST(a, prev.v)->next;
            }
            unlink_block(a, prev);
            size = i.v + size / 4 - prev.v;
            const uint32_t x = B_HDR(a, prev.v)->size4x & 2;
            B_HDR(a, prev.v)->size4x = size * 4 | x;
            B_HDR(a, prev.v + size)->prev_size = size;
            link_block(a, prev);
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

static void *checkout_block(struct BlockAllocator *a, struct BlockId i, uint32_t nblocks) 
{
    paw_assert(i.v >= 1);
    paw_assert(B_HDR(a, i.v)->size4x / 4 == nblocks);
    paw_assert(B_HDR(a, i.v + nblocks)->prev_size == nblocks);
    const uint32_t x = B_HDR(a, i.v)->size4x;
    B_HDR(a, i.v)->size4x = nblocks * 4 | 1 | (x & 2);
    B_HDR(a, i.v + nblocks)->prev_size = nblocks;
    B_HDR(a, i.v + nblocks)->size4x |= 2;
    return &a->blocks[i.v];
}

static void *key_block_alloc(struct BlockAllocator *a, uint32_t nblocks)
{
    assert(a->key_size >= nblocks);
    if (nblocks >= a->key_size - 1) {
        void *p = checkout_block(a, a->key, a->key_size);
        a->key.v = 0;
        a->key_size = 0;
        a->key_min = 0;
        return p;
    } else {
        struct BlockId newi = {a->key.v + a->key_size - nblocks};
        assert(newi.v > a->key.v+1);
        B_HDR(a, a->key.v + a->key_size)->prev_size = nblocks;
        B_HDR(a, a->key.v + a->key_size)->size4x |= 2;
        B_HDR(a, newi.v)->size4x = nblocks*4 + 1;
        a->key_size -= nblocks;
        B_HDR(a, newi.v)->prev_size = a->key_size;
        const uint32_t x = B_HDR(a, a->key.v)->size4x & 2;
        B_HDR(a, a->key.v)->size4x = a->key_size * 4 | x;
        if (a->key_size < a->key_min) {
            a->key_min = a->key_size;
        }
        return &a->blocks[newi.v];
    }
}

#define COMPUTE_NBLOCKS(nbytes) CHECK_EXP(BLOCK_SIZE == 8 && (nbytes) > 12, \
                                          ((nbytes) + 11) / BLOCK_SIZE)

// Modified from SQLite (mem3.c)
static void *block_malloc(struct BlockAllocator *a, size_t nbytes)
{
    uint32_t nblocks = COMPUTE_NBLOCKS(nbytes);
    paw_assert(nblocks >= 2);
  
    // search for an exact fit
    const uint32_t hash = nblocks % BLOCK_NLISTS;
    for (struct BlockId id = a->free[hash]; 
            id.v != BAD_BLOCK; 
            id = B_LIST(a, id.v)->next) {
        if (B_HDR(a, id.v)->size4x / 4 == nblocks) {
            unlink_from_list(a, id, &a->free[hash]);
            return checkout_block(a, id, nblocks);
        }
    }
    
    if (a->key_size >= nblocks) {
        return key_block_alloc(a, nblocks);
    }
  
    for (uint32_t to_free = nblocks * 16; 
            to_free < a->nblocks * 16; 
            to_free *= 2) {
        if (a->key.v != BAD_BLOCK) {
            link_block(a, a->key);
            a->key.v = BAD_BLOCK;
            a->key_size = 0;
        }
        for (uint32_t i = 0; i < BLOCK_NLISTS; ++i) {
            fix_block_list(a, &a->free[i]);
        }
        if (a->key_size != 0) {
            unlink_block(a, a->key);
            if (a->key_size >= nblocks) {
                return key_block_alloc(a, nblocks);
            }
        }
    }
  
    return NULL;
}

// Modified from SQLite (mem3.c)
static void block_free(struct BlockAllocator *a, void *ptr, size_t nbytes)
{
    uint32_t nblocks = COMPUTE_NBLOCKS(nbytes);
    paw_assert(nblocks >= 2);
    struct Block *b = ptr;
    paw_assert(b > a->blocks && b < &a->blocks[a->nblocks]);
    const struct BlockId i = {b - a->blocks};
    paw_assert((B_HDR(a, i.v)->size4x & 1) == 1);
    paw_assert(nblocks == B_HDR(a, i.v)->size4x / 4);
    paw_assert(i.v + nblocks <= a->nblocks + 1);
    B_HDR(a, i.v)->size4x &= ~1;
    B_HDR(a, i.v + nblocks)->prev_size = nblocks;
    B_HDR(a, i.v + nblocks)->size4x &= ~2;
    link_block(a, i);
  
    if (a->key.v != BAD_BLOCK) {
        while ((B_HDR(a, a->key.v)->size4x & 2) == 0) {
            nblocks = B_HDR(a, a->key.v)->prev_size;
            a->key.v -= nblocks;
            a->key_size += nblocks;
            unlink_block(a, a->key);
            const uint32_t x = B_HDR(a, a->key.v)->size4x & 2;
            B_HDR(a, a->key.v)->size4x = a->key_size * 4 | x;
            a->blocks[a->key.v + a->key_size-1].hdr.prev_size = a->key_size;
        }
        const uint32_t x = B_HDR(a, a->key.v)->size4x & 2;
        while ((B_HDR(a, a->key.v + a->key_size)->size4x & 1) == 0) {
            unlink_block(a, (struct BlockId){a->key.v + a->key_size});
            a->key_size += B_HDR(a, a->key.v + a->key_size)->size4x / 4;
            B_HDR(a, a->key.v)->size4x = a->key_size * 4 | x;
            B_HDR(a, a->key.v + a->key_size)->prev_size = a->key_size;
        }
    }
}

static size_t block_size(void *ptr)
{
    paw_assert(ptr != NULL);
    struct Block *b = ptr;
    paw_assert((b[-1].hdr.size4x & 1) != 0);
    return (b[-1].hdr.size4x & ~3) * 2 - 4;
}

static void *alloc_uninit(struct FastBins *a, size_t id)
{
    void *ptr = a->uninit;
    const size_t want = MIN_BIN_SIZE * (id + 1);
    if (want > a->uninit_size) return NULL;
    UNPOISON_MEMORY_REGION(a->uninit, want);
    a->uninit = BUMP(a->uninit, want);
    a->uninit_size -= want;
    return ptr;
}

static void *fast_malloc(struct FastBins *a, size_t size)
{
    paw_assert(0 < size && size <= MAX_BIN_SIZE);
    const int id = BIN_ID(size);
    struct BinInfo **pbin = &a->info[id];
    if (*pbin != NULL) {
        struct BinInfo *bin = *pbin;
        *pbin = bin->prev;
        return ERASE_TYPE(bin);
    }
    return alloc_uninit(a, id);
}

static void fast_free(struct FastBins *a, void *ptr, size_t size)
{
    if (ptr == NULL) return;
    struct BinInfo *bin = CAST(ptr, struct BinInfo *);
    const int id = BIN_ID(size);
    *bin = (struct BinInfo){
        .prev = a->info[id],
    };
    a->info[id] = bin;
}

// TODO: Put everything in the same allocation
int pawZ_init(paw_Env *P, size_t heap_size)
{
    if (heap_size < MIN_HEAP_SIZE) return PAW_EVALUE;
    const size_t nflags = heap_size / sizeof(void *);
    const size_t zflags = nflags / FLAGS_PER_BYTE;
    paw_assert(zflags * FLAGS_PER_BYTE == nflags);

    struct Heap *H = OS_MALLOC(P, sizeof(struct Heap) + zflags);
    if (H == NULL) return PAW_EMEMORY;
    *H = (struct Heap){
        .heap_size = heap_size,
        .nflags = nflags, 
        .P = P,
    };
    memset(H->flags, 0, zflags);
    P->H = H;

    void *heap = OS_MALLOC(P, heap_size);
    if (heap == NULL) goto no_memory;
    H->heap = heap;
printf("heap: %p-%p\n", heap,BUMP(heap,H->heap_size));
#define SKIP_CHUNK(z) (heap = BUMP(heap, aligned(z)), \
                       heap_size -= aligned(z))
    H->a_block = heap;
    SKIP_CHUNK(sizeof(struct BlockAllocator));
    init_block_allocator(H->a_block, heap, heap_size);
#undef SKIP_CHUNK

    const size_t arena_size = heap_size / BIN_FACTOR;
    void *arena = block_malloc(H->a_block, arena_size);
    if (arena == NULL) goto no_memory;
    POISON_MEMORY_REGION(arena, arena_size);
    H->bins = (struct FastBins){
        .uninit_size = arena_size,
        .arena_size = arena_size,
        .uninit = arena,
        .arena = arena,
    };
    H->bounds[0] = CAST_UPTR(H->heap);
    H->bounds[1] = H->bounds[0] + H->heap_size;
    return PAW_OK;

no_memory:
    pawZ_uninit(P);
    return PAW_EMEMORY;
}

// TODO: option for detect_leaks(P);
#if 0
static void detect_leaks(paw_Env *P)
{
    paw_Bool leak_detected = PAW_FALSE;
    const struct Heap *H = P->H;
    for (size_t i = 0; i < H->nflags; ++i) {
        if (H->flags[i].value != 0) {
            fprintf(stderr, "leak @ %p\n", ERASE_TYPE(H->bounds[0] + i * sizeof(void *)));
            leak_detected = PAW_TRUE;
        }     
    }
    if (leak_detected) {
        __builtin_trap();
    }
    if (P->gc_bytes>0){
    printf("gc_bytes = %zu\n\n",P->gc_bytes);
    }
}
#endif // 0

void pawZ_uninit(paw_Env *P)
{
    struct Heap *H = P->H;
    if (H != NULL) {
        if (H->bins.arena != NULL) {
            UNPOISON_MEMORY_REGION(H->bins.uninit, H->bins.uninit_size);
            block_free(P->H->a_block, H->bins.arena, H->bins.arena_size);
        }
        if (H->heap != NULL) OS_FREE(P, H->heap, H->heap_size);
        OS_FREE(P, H, sizeof(*H) + H->nflags / FLAGS_PER_BYTE);
        P->H = NULL;
    }
}

#define CHECK_UNUSED(H, ptr) paw_assert(!pawZ_get_flag(H, CAST_UPTR(ptr)))

static void z_free(struct Heap *H, void *ptr, size_t size)
{
    size = aligned(size);
    if (size <= MAX_BIN_SIZE) {
        fast_free(&H->bins, ptr, size); 
    } else {
        block_free(H->a_block, ptr, size); 
    }
}

static void *z_malloc(struct Heap *H, size_t size)
{
    size = aligned(size);
    void *ptr = size <= MAX_BIN_SIZE
        ? fast_malloc(&H->bins, size)
        : block_malloc(H->a_block, size);    
    return ptr;
}

static void *z_realloc(struct Heap *H, void *old_ptr, size_t old_size, size_t new_size)
{
    void *new_ptr = z_malloc(H, new_size);
    if (new_ptr != NULL && old_size > 0) {
        const size_t copy_size = PAW_MIN(old_size, new_size);
        memcpy(new_ptr, old_ptr, copy_size);
    }
    z_free(H, old_ptr, old_size);
    return new_ptr;
}

void *pawZ_alloc(paw_Env *P, void *ptr, size_t old_size, size_t new_size)
{
    struct Heap *H = P->H;
    if (new_size == 0) {
        z_free(H, ptr, old_size);
        return NULL;
    } 
    if (old_size == 0) return z_malloc(H, new_size);
    return z_realloc(H, ptr, old_size, new_size);
}

