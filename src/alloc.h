// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// alloc: Low-level memory allocation routines
//
// Uses 2 different heuristics depending on allocation size. For small allocations,
// we use a slotted allocator that never attempts to merge freelist entries. For 
// large allocations we use a slightly-modified version of the memsys3 allocator from 
// SQLite (mem3.c). The large object allocator uses only the hash-based partitioning 
// scheme, since it expects only large objects.
//
// TODO: Handle expanding the heap: change paw_Alloc to be more like sbrk, since
//       that is all we really need from the user (an initial 'heap' pointer, and a callback
//       to expand the original heap allocation if possible).

#ifndef PAW_ALLOC_H
#define PAW_ALLOC_H

#include "paw.h"

struct GcFlag {
    uint8_t value;
};

#define FAST_BIN_COUNT 1024

struct FastBins {
    struct BinInfo *info[FAST_BIN_COUNT];
    size_t uninit_size;
    size_t arena_size;
    void *uninit;
    void *arena;
};


struct Heap {
    struct FastBins bins;
    struct BlockAllocator *a_block;
    uintptr_t bounds[2];
    size_t heap_size;
    void *heap;
    paw_Env *P;

    size_t nflags;
    uint8_t flags[];
};

#define Z_IN_BOUNDS(H, u) ((H)->bounds[0] <= (u) && (u) < (H)->bounds[1])

int pawZ_init(paw_Env *P, size_t heap_size);
void pawZ_uninit(paw_Env *P);

void *pawZ_alloc(paw_Env *P, void *ptr, size_t old_size, size_t new_size);

void pawZ_set_flag(struct Heap *H, uintptr_t ptr);
void pawZ_clear_flag(struct Heap *H, uintptr_t ptr);
uint8_t pawZ_get_flag(struct Heap *H, uintptr_t ptr);

// Return PAW_TRUE if 'u' points to the start of an Object *, PAW_FALSE otherwise
static inline paw_Bool pawZ_is_object(struct Heap *H, uintptr_t u)
{
    return Z_IN_BOUNDS(H, u) && pawZ_get_flag(H, u);
}

#endif // PAW_ALLOC_H
