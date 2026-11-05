// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_COMMON_LIST_H
#define PAW_COMMON_LIST_H

#include "compiler/core.h"
#include "compiler/mem.h"

#define LIST_ALLOC(List_, Ptr_, Size0_, Size_) \
    (List_)->a->alloc((List_)->a->ctx, Ptr_, Size0_, Size_)

// Generate a structure type and methods for a list containing nodes of a given
// type Value_. Value_ can be any type, so long as it is "trivially copiable".
#define DEFINE_LIST(List_, Value_) \
    typedef struct List_ { \
        struct Allocator const *a; \
        Value_ *data; \
        int count; \
        int alloc; \
    } List_; \
    static List_ *List_##_new(struct Allocator const *a) \
    { \
        List_ *list = (List_ *)a->alloc(a->ctx, NULL, 0, sizeof(*list)); \
        list->data = NULL; \
        list->count = 0; \
        list->alloc = 0; \
        list->a = a; \
        return list; \
    } \
    static void List_##_delete(List_ *list) \
    { \
        LIST_ALLOC(list, list->data, (size_t)list->alloc * sizeof(list->data[0]), 0); \
        LIST_ALLOC(list, list, sizeof(*list), 0); \
    } \
    static paw_Usize List_##_len(List_ const *list) \
    { \
        return (paw_Usize)list->count; \
    } \
    static Value_ List_##_first(List_ const *list) \
    { \
        paw_assert(list->count > 0); \
        return list->data[0]; \
    } \
    static Value_ List_##_last(List_ const *list) \
    { \
        paw_assert(list->count > 0); \
        return list->data[list->count - 1]; \
    } \
    static Value_ *List_##_at(List_ const *list, int index) \
    { \
        paw_assert(0 <= index && index < list->count); \
        return &list->data[index]; \
    } \
    static Value_ List_##_get(List_ const *list, int index) \
    { \
        paw_assert(0 <= index && index < list->count); \
        return list->data[index]; \
    } \
    static void List_##_set(List_ *list, int index, Value_ value) \
    { \
        paw_assert(0 <= index && index < list->count); \
        list->data[index] = value; \
    } \
    static void List_##_push(List_ *list, Value_ value) \
    { \
        list->data = (Value_ *)list_ensure_n_(list->a, list->data, sizeof(list->data[0]), list->count, &list->alloc, 1); \
        list->data[list->count++] = value; \
    } \
    static void List_##_pop(List_ *list) \
    { \
        paw_assert(list->count > 0); \
        --list->count; \
    } \
    static void List_##_insert(List_ *list, int index, Value_ value) \
    { \
        paw_assert(0 <= index && index <= list->count); \
        list->data = (Value_ *)list_ensure_n_(list->a, list->data, sizeof(list->data[0]), list->count, &list->alloc, 1); \
        memmove(list->data + index + 1, list->data + index, (size_t)(list->count - index) * sizeof(list->data[0])); \
        list->data[index] = value; \
        ++list->count; \
    } \
    static Value_ List_##_remove(List_ *list, int index) \
    { \
        paw_assert(0 <= index && index < list->count); \
        Value_ value = List_##_get(list, index); \
        memmove(list->data + index, list->data + index + 1, (size_t)(list->count - index - 1) * sizeof(list->data[0])); \
        --list->count; \
        return value; \
    } \
    static Value_ List_##_swap_remove(List_ *list, int index) \
    { \
        paw_assert(0 <= index && index < list->count); \
        Value_ const value = List_##_get(list, index); \
        LIST_AT(list, index) = List_##_last(list); \
        --list->count; \
        return value; \
    } \
    static void List_##_extend(List_ *list, Value_ const *ptr, paw_Usize len) \
    { \
        list->data = (Value_ *)list_ensure_n_(list->a, list->data, sizeof(list->data[0]), list->count, &list->alloc, len); \
        memcpy(list->data + list->count, ptr, len); \
        list->count += len; \
    } \
    static void List_##_reserve(List_ *list, int count) \
    { \
        list->data = (Value_ *)list_reserve_(list->a, list->data, sizeof(list->data[0]), &list->alloc, count); \
    } \
    static void List_##_resize(List_ *list, int count) \
    { \
        List_##_reserve(list, count); \
        list->count = count; \
    }


//
// Macros for working with a list
//
#define LIST_AT(List_, Index_) ((List_)->data[Index_])
#define LIST_FIRST(List_) (LIST_AT(List_, 0))
#define LIST_LAST(List_) (LIST_AT(List_, (List_)->count - 1))
#define LIST_FOREACH(List_, Ptr_) \
    for (int i_ = 0; i_ < (List_)->count && (Ptr_ = (List_)->data + i_ /* always 1 */); ++i_)
#define LIST_ENUMERATE(List_, Iter_, Ptr_) \
    for (Iter_ = 0; Iter_ < (List_)->count && (Ptr_ = (List_)->data + Iter_ /* always 1 */); ++Iter_)
#define LIST_ZIP(ListA_, PtrA_, ListB_, PtrB_) \
    for (int i_ = ((PtrA_) = (ListA_)->data, (PtrB_) = (ListB_)->data, 0); \
            i_ < (ListA_)->count && i_ < (ListB_)->count; ++i_, ++(PtrA_), ++(PtrB_))
#define LIST_BEGIN(List_) ((List_)->data)
#define LIST_END(List_) ((List_)->data + (List_)->count)
#define LIST_XFOREACH(List_, Type_, Name_) \
    for (Type_ *Name_ = LIST_BEGIN(List_); (List_)->count > 0 && Name_ != LIST_END(List_); ++(Name_))

EXTERN_C void *list_reserve_(struct Allocator const *a, void *ptr, size_t element_size, int *pcapacity, int target);
EXTERN_C void *list_ensure_n_(struct Allocator const *a, void *ptr, size_t element_size, int length, int *pcapacity, int n);

#endif // PAW_COMMON_LIST_H

