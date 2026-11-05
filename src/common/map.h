// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_COMMON_MAP_H
#define PAW_COMMON_MAP_H

#include "compiler/core.h"
#include "compiler/mem.h"

#define MAP_MIN 4
#define MAP_MAX (1 << 28)
#define MAP_FILL_FACTOR 4

#define MAP_ALLOC(Map_, Ptr_, Size0_, Size_) \
    (Map_)->a->alloc((Map_)->a->ctx, Ptr_, Size0_, Size_)

#define DEFINE_MAP(Name_, Hash_, Equals_, Key_, Value_) \
    struct Name_##Node { \
        struct Name_##Node *next; \
        Key_ key; \
        Value_ value; \
    }; \
    typedef struct Name_ { \
        struct Allocator const *a; \
        struct Name_##Node **data; \
        int count; \
        int alloc; \
    } Name_; \
    static void Name_##_delete(struct Name_ *map) \
    { \
        for (int i = 0; i < map->alloc; ++i) { \
            struct Name_##Node *node = map->data[i]; \
            while (node != NULL) { \
                struct Name_##Node *next = node->next; \
                MAP_ALLOC(map, node, sizeof(*node), 0); \
                node = next; \
            } \
        } \
        MAP_ALLOC(map, map->data, (size_t)map->alloc * sizeof(map->data[0]), 0); \
        MAP_ALLOC(map, map, sizeof(struct Name_), 0); \
    } \
    static paw_Usize Name_##_len(struct Name_ const *map) \
    { \
        return (paw_Usize)map->count; \
    } \
    static struct Name_##Node **Name_##_bucketp(struct Name_ *map, Key_ key) \
    { \
        return &map->data[Hash_(key) % (paw_Uint)map->alloc]; \
    } \
    static void Name_##_grow(struct Name_ *map) \
    { \
        struct Name_ const old = *map; \
        if ((size_t)old.alloc > SIZE_MAX / (2 * sizeof(map->data[0]))) \
            mem_error(map->a->P); \
        size_t const capacity = old.alloc < MAP_MIN ? MAP_MIN : (size_t)old.alloc * 2; \
        map->data = (struct Name_##Node **)MAP_ALLOC(map, NULL, 0, capacity * sizeof(map->data[0])); \
        map->alloc = capacity; \
        memset(map->data, 0, capacity * sizeof(map->data[0])); \
        for (int i = 0; i < old.alloc; ++i) { \
            while (old.data[i] != NULL) { \
                struct Name_##Node *node = old.data[i]; \
                struct Name_##Node **ptr = Name_##_bucketp(map, node->key); \
                old.data[i] = node->next; \
                node->next = *ptr; \
                *ptr = node; \
            } \
        } \
        MAP_ALLOC(map, old.data, (size_t)old.alloc * sizeof(map->data[0]), 0); \
    } \
    static void Name_##_reserve(struct Name_ *map, int length) \
    { \
        while (length > map->alloc / MAP_FILL_FACTOR) \
            Name_##_grow(map); \
    } \
    static paw_Bool Name_##_insert(struct Name_ *map, Key_ key, Value_ value) \
    { \
        Name_##_reserve(map, map->count); \
        struct Name_##Node **bucket = Name_##_bucketp(map, key); \
        struct Name_##Node **ptr = bucket; \
        for (; *ptr != NULL; ptr = &(*ptr)->next) { \
            if (Equals_(key, (*ptr)->key)) { \
                (*ptr)->value = value; \
                return PAW_TRUE; \
            } \
        } \
        struct Name_##Node *node = (struct Name_##Node *)MAP_ALLOC(map, NULL, 0, sizeof(**ptr)); \
        *node = (struct Name_##Node){ \
            .next = *bucket, \
            .key = key, \
            .value = value, \
        }; \
        *bucket = node; \
        ++map->count; \
        return PAW_FALSE; \
    } \
    static void Name_##_insert_unique(struct Name_ *map, Key_ key, Value_ value) \
    { \
        paw_Bool const already_exists = Name_##_insert(map, key, value); \
        paw_assert(!already_exists); \
    } \
    static struct Name_##Node **Name_##_find_node(struct Name_ *map, Key_ key) \
    { \
        if (map->alloc == 0) \
            return NULL; \
        struct Name_##Node **ptr = Name_##_bucketp(map, key); \
        while (*ptr != NULL) { \
            if (Equals_(key, (*ptr)->key)) { \
                return ptr; \
            } \
            ptr = &(*ptr)->next; \
        } \
        return NULL; \
    } \
    static paw_Bool Name_##_remove(struct Name_ *map, Key_ key) \
    { \
        struct Name_##Node **ptr = Name_##_find_node(map, key); \
        if (ptr != NULL) { \
            struct Name_##Node *node = *ptr; \
            *ptr = node->next; \
            MAP_ALLOC(map, node, sizeof(*node), 0); \
            --map->count; \
            return PAW_TRUE; \
        } \
        return PAW_FALSE; \
    } \
    static Value_ *Name_##_get(struct Name_ *map, Key_ key) \
    { \
        struct Name_##Node **ptr = Name_##_find_node(map, key); \
        return ptr != NULL ? &(*ptr)->value : NULL; \
    } \
    static struct Name_ *Name_##_new(struct Allocator const *a) \
    { \
        Name_ *map = (Name_ *)a->alloc(a->ctx, NULL, 0, sizeof(*map)); \
        map->data = NULL; \
        map->alloc = 0; \
        map->count = 0; \
        map->a = a; \
        Name_##_grow(map); \
        return map; \
    }

#define DEFINE_MAP_ITERATOR(Name_, Key_, Value_) \
    typedef struct Name_##Iterator { \
        struct Name_##Node **ptr; \
        Name_ *map; \
        int index; \
    } Name_##Iterator; \
    static paw_Bool Name_##Iterator_is_valid(Name_##Iterator const *iter) \
    { \
        paw_assert(iter->ptr != NULL); \
        return *iter->ptr != NULL; \
    } \
    static Key_ Name_##Iterator_key(Name_##Iterator *iter) \
    { \
        paw_assert(Name_##Iterator_is_valid(iter)); \
        return (*iter->ptr)->key; \
    } \
    static Value_ *Name_##Iterator_valuep(Name_##Iterator *iter) \
    { \
        paw_assert(Name_##Iterator_is_valid(iter)); \
        return &(*iter->ptr)->value; \
    } \
    static void Name_##Iterator_ensure_bucket_(Name_##Iterator *iter) \
    { \
        if (*iter->ptr != NULL) \
            return; \
        while (++iter->index < iter->map->alloc) { \
            if (iter->map->data[iter->index] != NULL) { \
                iter->ptr = &iter->map->data[iter->index]; \
                break; \
            } \
        } \
    } \
    static void Name_##Iterator_next(Name_##Iterator *iter) \
    { \
        if (Name_##Iterator_is_valid(iter)) { \
            iter->ptr = &(*iter->ptr)->next; \
            Name_##Iterator_ensure_bucket_(iter); \
        } \
    } \
    static void Name_##Iterator_init(Name_ *map, Name_##Iterator *iter) \
    { \
        paw_assert(map->alloc > 0); \
        *iter = (Name_##Iterator){.map = map}; \
        for (iter->index = 0; iter->index < map->alloc; ++iter->index) { \
            iter->ptr = &map->data[iter->index]; \
            if (*iter->ptr != NULL) \
                break; \
        } \
    } \
    static void Name_##Iterator_erase(Name_##Iterator *iter) \
    { \
        *iter->ptr = (*iter->ptr)->next; \
        Name_##Iterator_ensure_bucket_(iter); \
        --iter->map->count; \
    }

#endif // PAW_COMMON_MAP_H
