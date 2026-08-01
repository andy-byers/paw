// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// TODO: clean this up

#ifndef PAW_MAP_H
#define PAW_MAP_H

#include "core.h"
#include "pool.h"

#define K_MAP_MIN 4
#define K_MAP_MAX (1 << 28)
#define K_MAP_FILL_FACTOR 4

#define DEFINE_MAP(Context_, Name_, Alloc_, Hash_, Equals_, Key_, Value_, ...) \
    struct Name_##Node { \
        struct Name_##Node *next; \
        Key_ key; \
        Value_ value; \
        __VA_ARGS__ \
    }; \
    typedef struct Name_ { \
        struct Name_##Node **data; \
        struct Pool *pool; \
        int count; \
        int alloc; \
    } Name_; \
    static struct Name_ *Name_##_new_from(Context_ *ctx, struct Pool *pool) \
    { \
        PAW_UNUSED(ctx); \
        Name_ *map = (Name_ *)Alloc_(pool, NULL, 0, sizeof(struct Name_)); \
        map->data = (struct Name_##Node **)Alloc_(pool, NULL, 0, K_MAP_MIN * sizeof(map->data[0])); \
        map->alloc = K_MAP_MIN; \
        map->pool = pool; \
        map->count = 0; \
        memset(map->data, 0, K_MAP_MIN * sizeof(map->data[0])); \
        return map; \
    } \
    static struct Name_ *Name_##_new(Context_ *ctx) \
    { \
        return Name_##_new_from(ctx, ctx->pool); \
    } \
    static void Name_##_delete(Context_ *ctx, struct Name_ *map) \
    { \
        PAW_UNUSED(ctx); \
        for (int i = 0; i < map->alloc; ++i) { \
            struct Name_##Node *node = map->data[i]; \
            while (node != NULL) { \
                struct Name_##Node *next = node->next; \
                Alloc_(map->pool, node, sizeof(*node), 0); \
                node = next; \
            } \
        } \
        Alloc_(map->pool, map->data, (size_t)map->alloc * sizeof(map->data[0]), 0); \
        Alloc_(map->pool, map, sizeof(struct Name_), 0); \
    } \
    static int Name_##_length(struct Name_ const *map) \
    { \
        return map->count; \
    } \
    static struct Name_##Node **Name_##_bucketp(Context_ *ctx, struct Name_ *map, Key_ key) \
    { \
        return &map->data[Hash_(ctx, key) % (paw_Uint)map->alloc]; \
    } \
    static void Name_##_grow(Context_ *ctx, struct Name_ *map) \
    { \
        struct Name_ old = *map; \
        size_t alloc = K_MAP_MIN; \
        while (alloc <= (size_t)old.alloc) \
            alloc *= 2; \
        pawM_check_size(map->pool->P, 0, alloc, sizeof(map->data[0])); \
        map->data = (struct Name_##Node **)Alloc_(map->pool, NULL, 0, alloc * sizeof(map->data[0])); \
        map->alloc = alloc; \
        memset(map->data, 0, alloc * sizeof(map->data[0])); \
        for (int i = 0; i < old.alloc; ++i) { \
            while (old.data[i] != NULL) { \
                struct Name_##Node *node = old.data[i]; \
                struct Name_##Node **ptr = Name_##_bucketp(ctx, map, node->key); \
                old.data[i] = node->next; \
                node->next = *ptr; \
                *ptr = node; \
            } \
        } \
        Alloc_(map->pool, old.data, (size_t)old.alloc * sizeof(map->data[0]), 0); \
    } \
    static void Name_##_reserve(Context_ *ctx, struct Name_ *map, int length) \
    { \
        while (length > map->alloc / K_MAP_FILL_FACTOR) \
            Name_##_grow(ctx, map); \
    } \
    static paw_Bool Name_##_insert(Context_ *ctx, struct Name_ *map, Key_ key, Value_ value) \
    { \
        Name_##_reserve(ctx, map, map->count); \
        struct Name_##Node **bucket = Name_##_bucketp(ctx, map, key); \
        struct Name_##Node **ptr = bucket; \
        for (; *ptr != NULL; ptr = &(*ptr)->next) { \
            if (Equals_(ctx, key, (*ptr)->key)) { \
                (*ptr)->value = value; \
                return PAW_TRUE; \
            } \
        } \
        struct Name_##Node *node = (struct Name_##Node *)Alloc_(map->pool, NULL, 0, sizeof(**ptr)); \
        *node = (struct Name_##Node){ \
            .next = *bucket, \
            .key = key, \
            .value = value, \
        }; \
        *bucket = node; \
        ++map->count; \
        return PAW_FALSE; \
    } \
    static void Name_##_insert_unique(Context_ *ctx, struct Name_ *map, Key_ key, Value_ value) \
    { \
        paw_Bool const already_exists = Name_##_insert(ctx, map, key, value); \
        paw_assert(!already_exists); \
    } \
    static struct Name_##Node **Name_##_find_node(Context_ *ctx, struct Name_ *map, Key_ key) \
    { \
        if (map->alloc == 0) \
            return NULL; \
        struct Name_##Node **ptr = Name_##_bucketp(ctx, map, key); \
        while (*ptr != NULL) { \
            if (Equals_(ctx, key, (*ptr)->key)) { \
                return ptr; \
            } \
            ptr = &(*ptr)->next; \
        } \
        return NULL; \
    } \
    static paw_Bool Name_##_remove(Context_ *ctx, struct Name_ *map, Key_ key) \
    { \
        struct Name_##Node **ptr = Name_##_find_node(ctx, map, key); \
        if (ptr != NULL) { \
            struct Name_##Node *node = *ptr; \
            *ptr = node->next; \
            Alloc_(map->pool, node, sizeof(*node), 0); \
            --map->count; \
            return PAW_TRUE; \
        } \
        return PAW_FALSE; \
    } \
    static Value_ *Name_##_get(Context_ *ctx, struct Name_ *map, Key_ key) \
    { \
        struct Name_##Node **ptr = Name_##_find_node(ctx, map, key); \
        return ptr != NULL ? &(*ptr)->value : NULL; \
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

#endif // PAW_MAP_H
