// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// alloc: Low-level memory allocation routines
//
// Uses code modified from SQLite's memsys3 allocator, which can be found in
// 'src/mem3.c' in the SQLite repository.
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

struct Heap {
    struct Allocator *a;
    uintptr_t bounds[2];
    paw_Env *P;

    paw_Bool is_owned;

    size_t nflags;
    uint8_t flags[];
};

#define Z_IN_BOUNDS(H, u) ((H)->bounds[0] <= (u) && (u) < (H)->bounds[1])

int pawZ_init(paw_Env *P, void *heap, size_t heap_size, paw_Bool is_owned);
void pawZ_uninit(paw_Env *P);

size_t pawZ_sizeof(void *ptr);
void *pawZ_alloc(paw_Env *P, void *ptr, size_t size);

void pawZ_set_flag(struct Heap *H, uintptr_t ptr);
void pawZ_clear_flag(struct Heap *H, uintptr_t ptr);
uint8_t pawZ_get_flag(const struct Heap *H, uintptr_t ptr);

// Return PAW_TRUE if 'u' points to the start of an Object *, PAW_FALSE otherwise
static inline paw_Bool pawZ_is_object(const struct Heap *H, uintptr_t u)
{
    return Z_IN_BOUNDS(H, u) && pawZ_get_flag(H, u);
}

#endif // PAW_ALLOC_H
