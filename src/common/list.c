// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "list.h"
#include "util.h"

static void *list_grow(struct Allocator const *a, void *ptr, size_t element_size, int *pcapacity, int target)
{
    int const capacity = *pcapacity;
    paw_assert(capacity < target);

    if ((size_t)target > PAW_SIZE_MAX / element_size)
        mem_error(a->P);

    *pcapacity = target;
    return a->alloc(a->ctx, ptr,
            (size_t)capacity * element_size,
            (size_t)target * element_size);
}

void *list_reserve_(struct Allocator const *a, void *ptr, size_t element_size, int *pcapacity, int target)
{
    paw_assert(*pcapacity >= 0);
    paw_assert(target >= 0);
    paw_assert(element_size > 0);

    if (target <= *pcapacity)
        return ptr;
    return list_grow(a, ptr, element_size, pcapacity, target);
}

void *list_ensure_n_(struct Allocator const *a, void *ptr, size_t element_size, int length, int *pcapacity, int n)
{
    paw_assert(*pcapacity >= 0);
    paw_assert(length >= 0);
    paw_assert(element_size > 0);

    if (length > *pcapacity - n) {
        if (*pcapacity > (INT_MAX - n) / 2)
            mem_error(a->P);
        int const capacity = PAW_MAX(*pcapacity * 2 + n, 1);
        return list_grow(a, ptr, element_size, pcapacity, capacity);
    }
    return ptr;
}
