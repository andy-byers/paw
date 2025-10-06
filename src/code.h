// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "mem.h"

#define K_ALIGNOF_NODE _Alignof(void *)
#define K_ALIGNAS_NODE _Alignas(void *)

typedef struct NodeId {
    unsigned value;
} NodeId;

typedef struct DeclId {
    unsigned modno;
    unsigned value;
} DeclId;

struct FreeBlock {
    struct FreeBlock *prev;
    int size;
    char data[];
};

struct PoolStats {
    struct Statistic *num_alloc;
    struct Statistic *bytes_alloc;
    struct Statistic *bytes_used;
};

struct Pool {
    struct Pool *prev;
    struct Pool *next;
    struct FreeBlock *free;
    struct Arena *arena;
    struct Arena *full;

    // memory usage statistics
    struct PoolStats st;
};

void pawK_pool_init(paw_Env *P, struct Pool *pool, size_t base_size, struct PoolStats st);
void pawK_pool_uninit(paw_Env *P, struct Pool *pool);
void *pawK_pool_alloc(paw_Env *P, struct Pool *pool, void *ptr, size_t size0, size_t size);

// TODO: should specialize the next 2 enumerations and move to AST and HIR modules (similar to MirUnaryOpKind)

// ORDER UnaryOp
enum UnaryOp {
    UNARY_LEN,
    UNARY_NEG,
    UNARY_NOT,
    UNARY_BNOT,
};

// ORDER BinaryOp
enum BinaryOp {
    BINARY_EQ,
    BINARY_NE,
    BINARY_LT,
    BINARY_LE,
    BINARY_GT,
    BINARY_GE,
    BINARY_AS,
    BINARY_ADD,
    BINARY_SUB,
    BINARY_MUL,
    BINARY_DIV,
    BINARY_MOD,
    BINARY_BXOR,
    BINARY_BAND,
    BINARY_BOR,
    BINARY_SHL,
    BINARY_SHR,
    BINARY_CONCAT,
};

enum JumpKind {
    JUMP_BREAK,
    JUMP_CONTINUE,
};

// ORDER BuiltinKind
enum BuiltinKind {
    BUILTIN_UNIT,
    BUILTIN_BOOL,
    BUILTIN_CHAR,
    BUILTIN_INT,
    BUILTIN_FLOAT,
    BUILTIN_STR,
    BUILTIN_LIST,
    BUILTIN_MAP,
    BUILTIN_OPTION,
    BUILTIN_RESULT,
    BUILTIN_RANGE,
    BUILTIN_RANGE_TO,
    BUILTIN_RANGE_FROM,
    BUILTIN_RANGE_FULL,
    BUILTIN_RANGE_INCLUSIVE,
    BUILTIN_RANGE_TO_INCLUSIVE,
    BUILTIN_HASH,
    BUILTIN_EQUALS,
    BUILTIN_COMPARE,

    NBUILTINS,
};

#define NBUILTIN_SCALARS (BUILTIN_FLOAT + 1)

enum TraitKind {
    TRAIT_HASH, // "Hash" trait
    TRAIT_EQUALS, // "Equals" trait
    TRAIT_USER, // user-defined trait
};

#define NBUILTIN_TRAITS TRAIT_USER

#define K_LIST_MIN (1 << 3)
#define K_LIST_MAX (1 << 28)

// TODO: Don't need context parameter after creation since pool is stored internally

// Generate a structure type and methods for a list containing nodes of a given
// type Value_. Value_ can be any type, so long as it is "trivially copiable".
#define DEFINE_LIST(Context_, List_, Value_)                                                                                       \
    typedef struct List_ {                                                                                                         \
        struct Pool *pool;                                                                                                         \
        Value_ *data;                                                                                                              \
        int count;                                                                                                                 \
        int alloc;                                                                                                                 \
    } List_;                                                                                                                       \
    _Static_assert(K_LIST_MAX < PAW_SIZE_MAX / sizeof(Value_),                                                                     \
                   "maximum list is too large");                                                                                   \
    static inline List_ *List_##_new_from(Context_ *ctx, struct Pool *pool)                                                   \
    {                                                                                                                              \
        List_ *list = (List_ *)pawP_alloc(ENV(ctx), pool, NULL, 0, sizeof(List_));                                                          \
        list->pool = pool; \
        list->data = NULL; \
        list->count = 0; \
        list->alloc = 0; \
        return list;                                                                                                               \
    }                                                                                                                              \
    static inline List_ *List_##_new(Context_ *ctx)                                                                                \
    {                                                                                                                              \
        return List_##_new_from(ctx, ctx->pool);                                                                              \
    }                                                                                                                              \
    static inline void List_##_delete(Context_ *ctx, List_ *list)                                                                  \
    {                                                                                                                              \
        pawP_alloc(ENV(ctx), list->pool, list->data, (size_t)list->alloc * sizeof(list->data[0]), 0);                              \
        pawP_alloc(ENV(ctx), list->pool, list, sizeof(List_), 0);                                                                  \
    }                                                                                                                              \
    static inline Value_ List_##_first(List_ const *list)                                                                          \
    {                                                                                                                              \
        paw_assert(list->count > 0);                                                                                               \
        return list->data[0];                                                                                                      \
    }                                                                                                                              \
    static inline Value_ List_##_last(List_ const *list)                                                                           \
    {                                                                                                                              \
        paw_assert(list->count > 0);                                                                                               \
        return list->data[list->count - 1];                                                                                        \
    }                                                                                                                              \
    static inline Value_ List_##_get(List_ const *list, int index)                                                                 \
    {                                                                                                                              \
        paw_assert(0 <= index && index < list->count);                                                                             \
        return list->data[index];                                                                                                  \
    }                                                                                                                              \
    static inline void List_##_set(List_ *list, int index, Value_ value)                                                           \
    {                                                                                                                              \
        paw_assert(0 <= index && index < list->count);                                                                             \
        list->data[index] = value;                                                                                                 \
    }                                                                                                                              \
    static inline void List_##_push(Context_ *ctx, List_ *list, Value_ value)                                                      \
    {                                                                                                                              \
        list->data = (Value_ *)pawK_list_ensure_one(ENV(ctx), list->pool, list->data, sizeof(list->data[0]), list->count, &list->alloc);     \
        list->data[list->count++] = value;                                                                                         \
    }                                                                                                                              \
    static inline void List_##_pop(List_ *list)                                                                                    \
    {                                                                                                                              \
        paw_assert(list->count > 0);                                                                                               \
        --list->count;                                                                                                             \
    }                                                                                                                              \
    static inline void List_##_insert(Context_ *ctx, List_ *list, int index, Value_ value)                                         \
    {                                                                                                                              \
        paw_assert(0 <= index && index <= list->count);                                                                            \
        list->data = (Value_ *)pawK_list_ensure_one(ENV(ctx), list->pool, list->data, sizeof(list->data[0]), list->count, &list->alloc);     \
        memmove(list->data + index + 1, list->data + index, (size_t)(list->count - index) * sizeof(list->data[0]));                \
        list->data[index] = value;                                                                                                 \
        ++list->count;                                                                                                             \
    }                                                                                                                              \
    static inline Value_ List_##_remove(List_ *list, int index)                                                                    \
    {                                                                                                                              \
        paw_assert(0 <= index && index < list->count);                                                                             \
        Value_ value = List_##_get(list, index);                                                                                   \
        memmove(list->data + index, list->data + index + 1, (size_t)(list->count - index - 1) * sizeof(list->data[0]));            \
        --list->count;                                                                                                             \
        return value;                                                                                                              \
    }                                                                                                                              \
    static inline Value_ List_##_swap_remove(List_ *list, int index)                                                               \
    {                                                                                                                              \
        paw_assert(0 <= index && index < list->count);                                                                             \
        Value_ value = List_##_get(list, index);                                                                                   \
        K_LIST_AT(list, index) = List_##_last(list);                                                                               \
        --list->count;                                                                                                             \
        return value;                                                                                                              \
    }                                                                                                                              \
    static inline void List_##_reserve(Context_ *ctx, List_ *list, int count)                                                      \
    {                                                                                                                              \
        list->data = (Value_ *)pawK_list_reserve(ENV(ctx), list->pool, list->data, sizeof(list->data[0]), list->count, &list->alloc, count); \
    }                                                                                                                              \
    static inline void List_##_resize(Context_ *ctx, List_ *list, int count)                                                       \
    {                                                                                                                              \
        List_##_reserve(ctx, list, count);                                                                                         \
        list->count = count;                                                                                                       \
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
#define K_LIST_ZIP(ListA_, PtrA_, ListB_, PtrB_)                     \
    for (int i_ = ((PtrA_) = (ListA_)->data, (PtrB_) = (ListB_)->data, 0); \
            i_ < (ListA_)->count && i_ < (ListB_)->count; ++i_, ++(PtrA_), ++(PtrB_))
#define K_LIST_BEGIN(List_) ((List_)->data)
#define K_LIST_END(List_) ((List_)->data + (List_)->count - 1)

#define K_MAP_MIN 4
#define K_MAP_MAX (1 << 28)
#define K_MAP_FILL_FACTOR 4

#define DEFINE_MAP(Context_, Name_, Alloc_, Hash_, Equals_, Key_, Value_)                            \
    struct Name_##Node {                                                                             \
        struct Name_##Node *next;                                                                    \
        Key_ key;                                                                                    \
        Value_ value;                                                                                \
    };                                                                                               \
    typedef struct Name_ {                                                                           \
        struct Name_##Node **data;                                                                   \
        struct Pool *pool;                                                                           \
        int count;                                                                                   \
        int alloc;                                                                                   \
    } Name_;                                                                                         \
    _Static_assert(K_MAP_MAX < PAW_SIZE_MAX / sizeof(struct Name_##Node),                            \
                   "maximum map is too large");                                                      \
    static inline struct Name_ *Name_##_new_from(Context_ *ctx, struct Pool *pool)                        \
    {                                                                                                \
        Name_ *map = (Name_ *)Alloc_(ENV(ctx), pool, NULL, 0, sizeof(struct Name_));                          \
        map->data = (struct Name_##Node **)Alloc_(ENV(ctx), pool, NULL, 0, K_MAP_MIN * sizeof(map->data[0]));               \
        map->alloc = K_MAP_MIN;                                                                      \
        map->pool = pool;                                                                            \
        map->count = 0; \
        memset(map->data, 0, K_MAP_MIN * sizeof(map->data[0]));                                      \
        return map;                                                                                  \
    }                                                                                                \
    static inline struct Name_ *Name_##_new(Context_ *ctx)                        \
    {                                                                                                \
        return Name_##_new_from(ctx, ctx->pool);                                                                                  \
    }                                                                                                \
    static inline void Name_##_delete(Context_ *ctx, struct Name_ *map)                              \
    {                                                                                                \
        for (int i = 0; i < map->alloc; ++i) {                                                       \
            struct Name_##Node *node = map->data[i];                                                 \
            while (node != NULL) {                                                                   \
                struct Name_##Node *next = node->next;                                               \
                Alloc_(ENV(ctx), map->pool, node, sizeof(*node), 0);                                 \
                node = next;                                                                         \
            }                                                                                        \
        }                                                                                            \
        Alloc_(ENV(ctx), map->pool, map->data, (size_t)map->alloc * sizeof(map->data[0]), 0);        \
        Alloc_(ENV(ctx), map->pool, map, sizeof(struct Name_), 0);                                   \
    }                                                                                                \
    static inline int Name_##_length(struct Name_ const *map)                                        \
    {                                                                                                \
        return map->count;                                                                           \
    }                                                                                                \
    static inline struct Name_##Node **Name_##_bucketp(Context_ *ctx, struct Name_ *map, Key_ key)   \
    {                                                                                                \
        return &map->data[Hash_(ctx, key) % (paw_Uint)map->alloc];                                   \
    }                                                                                                \
    static inline void Name_##_grow(Context_ *ctx, struct Name_ *map)                                \
    {                                                                                                \
        struct Name_ old = *map;                                                                     \
        size_t alloc = K_MAP_MIN;                                                                    \
        while (alloc <= (size_t)old.alloc)                                                           \
            alloc *= 2;                                                                              \
        pawM_check_size(ENV(ctx), 0, alloc, sizeof(map->data[0]));                                   \
        map->data = (struct Name_##Node **)Alloc_(ENV(ctx), map->pool, NULL, 0, alloc * sizeof(map->data[0]));              \
        map->alloc = alloc;                                                                          \
        memset(map->data, 0, alloc * sizeof(map->data[0]));                                          \
        for (int i = 0; i < old.alloc; ++i) {                                                        \
            while (old.data[i] != NULL) {                                                            \
                struct Name_##Node *node = old.data[i];                                              \
                struct Name_##Node **ptr = Name_##_bucketp(ctx, map, node->key);                     \
                old.data[i] = node->next;                                                            \
                node->next = *ptr;                                                                   \
                *ptr = node;                                                                         \
            }                                                                                        \
        }                                                                                            \
        Alloc_(ENV(ctx), map->pool, old.data, (size_t)old.alloc * sizeof(map->data[0]), 0);          \
    }                                                                                                \
    static inline paw_Bool Name_##_insert(Context_ *ctx, struct Name_ *map, Key_ key, Value_ value)  \
    {                                                                                                \
        if (map->count > map->alloc / K_MAP_FILL_FACTOR)                                             \
            Name_##_grow(ctx, map);                                                                  \
                                                                                                     \
        struct Name_##Node **bucket = Name_##_bucketp(ctx, map, key);                                \
        struct Name_##Node **ptr = bucket;                                                           \
        for (; *ptr != NULL; ptr = &(*ptr)->next) {                                                  \
            if (Equals_(ctx, key, (*ptr)->key)) {                                                    \
                (*ptr)->value = value;                                                               \
                return PAW_TRUE;                                                                     \
            }                                                                                        \
        }                                                                                            \
        struct Name_##Node *node = (struct Name_##Node *)Alloc_(ENV(ctx), map->pool, NULL, 0, sizeof(**ptr));              \
        *node = (struct Name_##Node){                                                                \
            .next = *bucket,                                                                         \
            .key = key,                                                                              \
            .value = value,                                                                          \
        };                                                                                           \
        *bucket = node;                                                                              \
        ++map->count;                                                                                \
        return PAW_FALSE;                                                                            \
    }                                                                                                \
    static inline struct Name_##Node **Name_##_find_node(Context_ *ctx, struct Name_ *map, Key_ key) \
    {                                                                                                \
        if (map->alloc == 0)                                                                         \
            return NULL;                                                                             \
        struct Name_##Node **ptr = Name_##_bucketp(ctx, map, key);                                   \
        while (*ptr != NULL) {                                                                       \
            if (Equals_(ctx, key, (*ptr)->key)) {                                                    \
                return ptr;                                                                          \
            }                                                                                        \
            ptr = &(*ptr)->next;                                                                     \
        }                                                                                            \
        return NULL;                                                                                 \
    }                                                                                                \
    static inline paw_Bool Name_##_remove(Context_ *ctx, struct Name_ *map, Key_ key)                \
    {                                                                                                \
        struct Name_##Node **ptr = Name_##_find_node(ctx, map, key);                           \
        if (ptr != NULL) {                                                                           \
            struct Name_##Node *node = *ptr;                                                         \
            *ptr = node->next;                                                                       \
            Alloc_(ENV(ctx), map->pool, node, sizeof(*node), 0);                                     \
            --map->count;                                                                            \
            return PAW_TRUE;                                                                         \
        }                                                                                            \
        return PAW_FALSE;                                                                            \
    }                                                                                                \
    static inline Value_ *Name_##_get(Context_ *ctx, struct Name_ *map, Key_ key)                    \
    {                                                                                                \
        struct Name_##Node **ptr = Name_##_find_node(ctx, map, key);                                 \
        return ptr != NULL ? &(*ptr)->value : NULL;                                                  \
    }

#define DEFINE_MAP_ITERATOR(Name_, Key_, Value_)                                 \
    typedef struct Name_##Iterator {                                             \
        struct Name_##Node **ptr;                                                \
        Name_ *map;                                                              \
        int index;                                                               \
    } Name_##Iterator;                                                           \
    static inline paw_Bool Name_##Iterator_is_valid(Name_##Iterator const *iter) \
    {                                                                            \
        paw_assert(iter->ptr != NULL);                                           \
        return *iter->ptr != NULL;                                               \
    }                                                                            \
    static inline Key_ Name_##Iterator_key(Name_##Iterator *iter)                \
    {                                                                            \
        paw_assert(Name_##Iterator_is_valid(iter));                              \
        return (*iter->ptr)->key;                                                \
    }                                                                            \
    static inline Value_ *Name_##Iterator_valuep(Name_##Iterator *iter)          \
    {                                                                            \
        paw_assert(Name_##Iterator_is_valid(iter));                              \
        return &(*iter->ptr)->value;                                             \
    }                                                                            \
    static inline void Name_##Iterator_ensure_bucket_(Name_##Iterator *iter)     \
    {                                                                            \
        if (*iter->ptr != NULL)                                                  \
            return;                                                              \
        while (++iter->index < iter->map->alloc) {                               \
            if (iter->map->data[iter->index] != NULL) {                          \
                iter->ptr = &iter->map->data[iter->index];                       \
                break;                                                           \
            }                                                                    \
        }                                                                        \
    }                                                                            \
    static inline void Name_##Iterator_next(Name_##Iterator *iter)               \
    {                                                                            \
        if (Name_##Iterator_is_valid(iter)) {                                    \
            iter->ptr = &(*iter->ptr)->next;                                     \
            Name_##Iterator_ensure_bucket_(iter);                                \
        }                                                                        \
    }                                                                            \
    static inline void Name_##Iterator_init(Name_ *map, Name_##Iterator *iter)   \
    {                                                                            \
        paw_assert(map->alloc > 0);                                              \
        *iter = (Name_##Iterator){.map = map};                                   \
        for (iter->index = 0; iter->index < map->alloc; ++iter->index) {         \
            iter->ptr = &map->data[iter->index];                                 \
            if (*iter->ptr != NULL)                                              \
                break;                                                           \
        }                                                                        \
    }                                                                            \
    static inline void Name_##Iterator_erase(Name_##Iterator *iter)              \
    {                                                                            \
        *iter->ptr = (*iter->ptr)->next;                                         \
        Name_##Iterator_ensure_bucket_(iter);                                    \
        --iter->map->count;                                                      \
    }

void *pawK_list_reserve(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc, int target);
void *pawK_list_ensure_one(paw_Env *P, struct Pool *pool, void *data, size_t zelem, int count, int *palloc);

enum FnKind {
    FUNC_MODULE,
    FUNC_CLOSURE,
    FUNC_FUNCTION,
    FUNC_METHOD,
};

// From https://stackoverflow.com/questions/8513911
static inline paw_Uint hash_combine(paw_Uint seed, paw_Uint v)
{
    // TODO: versions for other sizes of paw_Uint
    paw_Uint const mul = 0x9DDFEA08EB382D69ULL;
    paw_Uint a = (v ^ seed) * mul;
    a ^= (a >> 47);
    paw_Uint b = (seed ^ a) * mul;
    b ^= (b >> 47);
    return b * mul;
}

#endif // PAW_CODE_H
