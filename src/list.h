// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_LIST_H
#define PAW_LIST_H

#include "core.h"
#include "pool.h"

#define K_LIST_MIN (1 << 3)

// TODO: Don't need context parameter after creation since pool is stored internally

// Generate a structure type and methods for a list containing nodes of a given
// type Value_. Value_ can be any type, so long as it is "trivially copiable".
#define DEFINE_LIST(Context_, List_, Value_, ...) \
    typedef struct List_ { \
        struct Pool *pool; \
        Value_ *data; \
        int count; \
        int alloc; \
        __VA_ARGS__ \
    } List_; \
    static List_ *List_##_new_from(Context_ *ctx, struct Pool *pool) \
    { \
        PAW_UNUSED(ctx); \
        List_ *list = (List_ *)pawP_alloc(pool, NULL, 0, sizeof(*list)); \
        list->pool = pool; \
        list->data = NULL; \
        list->count = 0; \
        list->alloc = 0; \
        return list; \
    } \
    static List_ *List_##_new(Context_ *ctx) \
    { \
        return List_##_new_from(ctx, ctx->pool); \
    } \
    static void List_##_delete(Context_ *ctx, List_ *list) \
    { \
        PAW_UNUSED(ctx); \
        pawP_alloc(list->pool, list->data, (size_t)list->alloc * sizeof(list->data[0]), 0); \
        pawP_alloc(list->pool, list, sizeof(List_), 0); \
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
    static void List_##_push(Context_ *ctx, List_ *list, Value_ value) \
    { \
        PAW_UNUSED(ctx); \
        list->data = (Value_ *)pawK_list_ensure_one(list->pool, list->data, sizeof(list->data[0]), list->count, &list->alloc); \
        list->data[list->count++] = value; \
    } \
    static void List_##_pop(List_ *list) \
    { \
        paw_assert(list->count > 0); \
        --list->count; \
    } \
    static void List_##_insert(Context_ *ctx, List_ *list, int index, Value_ value) \
    { \
        PAW_UNUSED(ctx); \
        paw_assert(0 <= index && index <= list->count); \
        list->data = (Value_ *)pawK_list_ensure_one(list->pool, list->data, sizeof(list->data[0]), list->count, &list->alloc); \
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
        Value_ value = List_##_get(list, index); \
        K_LIST_AT(list, index) = List_##_last(list); \
        --list->count; \
        return value; \
    } \
    static void List_##_reserve(Context_ *ctx, List_ *list, int count) \
    { \
        PAW_UNUSED(ctx); \
        list->data = (Value_ *)pawK_list_reserve(list->pool, list->data, sizeof(list->data[0]), &list->alloc, count); \
    } \
    static void List_##_resize(Context_ *ctx, List_ *list, int count) \
    { \
        List_##_reserve(ctx, list, count); \
        list->count = count; \
    }


//
// Macros for working with a list
//
#define K_LIST_AT(List_, Index_) ((List_)->data[Index_])
#define K_LIST_FIRST(List_) (K_LIST_AT(List_, 0))
#define K_LIST_LAST(List_) (K_LIST_AT(List_, (List_)->count - 1))
#define K_LIST_FOREACH(List_, Ptr_) \
    for (int i_ = 0; i_ < (List_)->count && (Ptr_ = (List_)->data + i_ /* always 1 */); ++i_)
#define K_LIST_ENUMERATE(List_, Iter_, Ptr_) \
    for (Iter_ = 0; Iter_ < (List_)->count && (Ptr_ = (List_)->data + Iter_ /* always 1 */); ++Iter_)
#define K_LIST_ZIP(ListA_, PtrA_, ListB_, PtrB_) \
    for (int i_ = ((PtrA_) = (ListA_)->data, (PtrB_) = (ListB_)->data, 0); \
            i_ < (ListA_)->count && i_ < (ListB_)->count; ++i_, ++(PtrA_), ++(PtrB_))
#define K_LIST_BEGIN(List_) ((List_)->data)
#define K_LIST_END(List_) ((List_)->data + (List_)->count)
#define K_LIST_XFOREACH(List_, Type_, Name_) \
    for (Type_ *Name_ = K_LIST_BEGIN(List_); (List_)->count > 0 && Name_ != K_LIST_END(List_); ++(Name_))

EXTERN_C void *pawK_list_reserve(struct Pool *pool, void *data, size_t zelem, int *palloc, int target);
EXTERN_C void *pawK_list_ensure_one(struct Pool *pool, void *data, size_t zelem, int count, int *palloc);

#endif // PAW_LIST_H
