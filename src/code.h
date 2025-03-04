// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "mem.h"

#define K_ALIGNOF_NODE _Alignof(void *)
#define K_ALIGNAS_NODE _Alignas(void *)

typedef struct DefId {
    unsigned short modno;
    unsigned short value;
} DefId;

typedef struct HirId {
    unsigned short value;
} HirId;

typedef struct DeclId {
    unsigned short modno;
    unsigned short value;
} DeclId;

struct FreeBlock {
    struct FreeBlock *prev;
    int size;
    char data[];
};

typedef struct Pool {
    struct FreeBlock *free;
    struct Arena *filled;
    struct Arena *arena;
    size_t last_size;
    size_t min_size;
} Pool;

void pawK_pool_init(paw_Env *P, struct Pool *pool, size_t base_size, size_t min_size);
void pawK_pool_uninit(paw_Env *P, struct Pool *pool);
void *pawK_pool_alloc(paw_Env *P, struct Pool *pool, void *ptr, size_t size0, size_t size);

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
    BINARY_RANGE,
};

enum JumpKind {
    JUMP_BREAK,
    JUMP_CONTINUE,
};

// ORDER BuiltinKind
enum BuiltinKind {
    BUILTIN_UNIT,
    BUILTIN_BOOL,
    BUILTIN_INT,
    BUILTIN_FLOAT,
    BUILTIN_STR,
    BUILTIN_LIST,
    BUILTIN_MAP,
    BUILTIN_OPTION,
    BUILTIN_RESULT,
    BUILTIN_RANGE,
    BUILTIN_HASH,
    BUILTIN_EQUALS,
    BUILTIN_COMPARE,

    NBUILTINS,
};

enum TraitKind {
    TRAIT_HASH, // Hash trait
    TRAIT_EQUALS, // Equals trait
    TRAIT_USER, // user-defined trait
};

#define NBUILTIN_TRAITS TRAIT_USER

#define K_LIST_MIN (1 << 3)
#define K_LIST_MAX (1 << 15)

// Generate a structure type and constructor for a list containing nodes of a
// given type T. T can be any type, so long as it is "trivially copiable".
#define DEFINE_LIST(ctx, func, L, T)                                                                  \
    struct L {                                                                                        \
        T *data;                                                                                      \
        int count;                                                                                    \
        int alloc;                                                                                    \
    };                                                                                                \
    _Static_assert(K_LIST_MAX < PAW_SIZE_MAX / sizeof(T),                                             \
        "maximum list is too large");                                                                 \
    static inline struct L *func##new (ctx * X)                                                       \
    {                                                                                                 \
        return pawK_pool_alloc(X->P, X->pool, NULL, 0, sizeof(struct L));                             \
    }                                                                                                 \
    static inline void func##delete (ctx * X, struct L * list)                                        \
    {                                                                                                 \
        pawK_pool_alloc(ENV(X), X->pool, list->data, (size_t)list->alloc * sizeof(list->data[0]), 0); \
        pawK_pool_alloc(ENV(X), X->pool, list, sizeof(struct L), 0);                                  \
    }

// Macros for working with a list
#define K_LIST_FIRST(L) (K_LIST_GET(L, 0))
#define K_LIST_LAST(L) (K_LIST_GET(L, (L)->count - 1))
#define K_LIST_GET(L, i) ((L)->data[i])
#define K_LIST_SET(L, i, v) CHECK_EXP(0 <= (i) && (i) < (L)->count, (L)->data[i] = (v))
#define K_LIST_POP(L) CHECK_EXP((L)->count > 0, --(L)->count)
#define K_LIST_PUSH(C, L, v) ((L)->data = pawK_list_ensure_one((C)->P, (C)->pool, (L)->data, sizeof((L)->data[0]), (L)->count, &(L)->alloc), \
    (L)->data[(L)->count++] = (v))
#define K_LIST_INSERT(C, L, i, v) CHECK_EXP(0 <= (i) && (i) <= (L)->count,                                          \
    ((L)->data = pawK_list_ensure_one((C)->P, (C)->pool, (L)->data, sizeof((L)->data[0]), (L)->count, &(L)->alloc), \
        memmove((L)->data + (i) + 1, (L)->data + (i), ((L)->count - (i)) * sizeof((L)->data[0])),                   \
        ++(L)->count,                                                                                               \
        (L)->data[i] = (v)))
#define K_LIST_REMOVE(L, i) CHECK_EXP(0 <= (i) && (i) < (L)->count,                                \
    (memmove((L)->data + (i), (L)->data + (i) + 1, ((L)->count - (i) - 1) * sizeof((L)->data[0])), \
        --(L)->count))
#define K_LIST_RESERVE(C, L, n) ((L)->data = pawK_list_reserve((C)->P, (C)->pool, (L)->data, sizeof((L)->data[0]), (L)->count, &(L)->alloc, n))

#define K_LIST_FOREACH(L, p) for (p = (L)->data; p && p < (L)->data + (L)->count; ++p)
#define K_LIST_ENUMERATE(L, i, p) for (i = 0, p = (L)->data; i < (L)->count; p = &(L)->data[++i])
#define K_LIST_ZIP(A, a, B, b) for (a = (A)->data, b = (B)->data; a && b && a < (A)->data + (A)->count && b < (B)->data + (B)->count; ++a, ++b)

#define K_MAP_MIN 4
#define K_MAP_MAX (1 << 18)
#define K_MAP_FILL_FACTOR 4

#define DEFINE_MAP(Context_, Name_, Alloc_, Hash_, Equals_, Key_, Value_)                            \
    struct Name_##Node {                                                                             \
        struct Name_##Node *next;                                                                    \
        Key_ key;                                                                                    \
        Value_ value;                                                                                \
    };                                                                                               \
    typedef struct Name_ {                                                                           \
        struct Name_##Node **data;                                                                   \
        int count;                                                                                   \
        int alloc;                                                                                   \
    } Name_;                                                                                         \
    _Static_assert(K_MAP_MAX < PAW_SIZE_MAX / sizeof(struct Name_##Node),                            \
        "maximum map is too large");                                                                 \
    static inline struct Name_ *Name_##_new(Context_ *ctx)                                           \
    {                                                                                                \
        Name_ *map = Alloc_(ctx, NULL, 0, sizeof(struct Name_));                                     \
        *map = (Name_){                                                                              \
            .data = Alloc_(ctx, NULL, 0, K_MAP_MIN * sizeof(struct Name_##Node *)),                  \
            .alloc = K_MAP_MIN,                                                                      \
        };                                                                                           \
        return map;                                                                                  \
    }                                                                                                \
    static inline void Name_##_delete(Context_ *ctx, struct Name_ *map)                              \
    {                                                                                                \
        for (int i = 0; i < map->alloc; ++i) {                                                       \
            struct Name_##Node *node = map->data[i];                                                 \
            while (node != NULL) {                                                                   \
                struct Name_##Node *next = node->next;                                               \
                Alloc_(ctx, node, sizeof(*node), 0);                                                 \
                node = next;                                                                         \
            }                                                                                        \
        }                                                                                            \
        Alloc_(ctx, map->data, (size_t)map->alloc * sizeof(*map->data[0]), 0);                       \
        Alloc_(ctx, map, sizeof(struct Name_), 0);                                                   \
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
        while (alloc <= (size_t)map->alloc)                                                          \
            alloc *= 2;                                                                              \
        pawM_check_size(ENV(ctx), 0, alloc, sizeof(*map->data[0]));                                  \
        map->data = pawP_alloc(ctx, NULL, 0, alloc * sizeof(*map->data[0]));                         \
        map->alloc = alloc;                                                                          \
        for (int i = 0; i < old.alloc; ++i) {                                                        \
            while (old.data[i] != NULL) {                                                            \
                struct Name_##Node *node = old.data[i];                                              \
                struct Name_##Node **ptr = Name_##_bucketp(ctx, map, node->key);                     \
                old.data[i] = node->next;                                                            \
                node->next = *ptr;                                                                   \
                *ptr = node;                                                                         \
            }                                                                                        \
        }                                                                                            \
        Alloc_(ctx, old.data, (size_t)old.alloc * sizeof(*map->data[0]), 0);                         \
    }                                                                                                \
    static inline paw_Bool Name_##_insert(Context_ *ctx, struct Name_ *map, Key_ key, Value_ value)  \
    {                                                                                                \
        if (map->count > map->alloc / K_MAP_FILL_FACTOR) {                                           \
            Name_##_grow(ctx, map);                                                                  \
        }                                                                                            \
        struct Name_##Node **bucket = Name_##_bucketp(ctx, map, key);                                \
        struct Name_##Node **ptr = bucket;                                                           \
        for (; *ptr != NULL; ptr = &(*ptr)->next) {                                                  \
            if (Equals_(ctx, key, (*ptr)->key)) {                                                    \
                (*ptr)->value = value;                                                               \
                return PAW_TRUE;                                                                     \
            }                                                                                        \
        }                                                                                            \
        struct Name_##Node *node = Alloc_(ctx, NULL, 0, sizeof(**ptr));                              \
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
        struct Name_##Node **ptr = Name_##_bucketp(ctx, map, key);                                   \
        while (*ptr != NULL) {                                                                       \
            if (Equals_(ctx, key, (*ptr)->key)) {                                                    \
                return ptr;                                                                          \
            }                                                                                        \
            ptr = &(*ptr)->next;                                                                     \
        }                                                                                            \
        return NULL;                                                                                 \
    }                                                                                                \
    static inline void Name_##_remove(Context_ *ctx, struct Name_ *map, Key_ key)                    \
    {                                                                                                \
        struct Name_##Node **ptr = Name_##_find_node(ctx, map, key);                                 \
        if (ptr != NULL) {                                                                           \
            struct Name_##Node *node = *ptr;                                                         \
            *ptr = node->next;                                                                       \
            Alloc_(ctx, node, sizeof(*node), 0);                                                     \
            --map->count;                                                                            \
        }                                                                                            \
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

enum FuncKind {
    FUNC_MODULE,
    FUNC_CLOSURE,
    FUNC_FUNCTION,
    FUNC_METHOD,
};

// Note that ValueMap for floats considers "-0.0" and "0.0" to be different values, while
// normal floating point equality comparison considers them to be equal. This shouldn't cause
// any problems, provided that the runtime generates floating point comparisons correctly.
struct KCache {
    struct ValueMap *ints;
    struct ValueMap *strs;
    struct ValueMap *flts;
};

#endif // PAW_CODE_H
