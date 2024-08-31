// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_STR_H
#define PAW_STR_H

#include "auxlib.h"
#include "util.h"
#include "value.h"
#include <stdint.h>
#include <string.h>

#ifndef PAW_INTERN_LIMIT
#define PAW_INTERN_LIMIT 16384
#endif // PAW_INTERN_LIMIT

// sdbm hash modified from http://www.cse.yorku.ca/~oz/hash.html
static inline uint32_t pawS_hash(const void *data, size_t size, uint32_t hash)
{
    const uint8_t *ptr = data;
    for (size_t i = 0; i < size; ++i) {
        hash = ptr[i] + (hash << 6) + (hash << 16) - hash;
    }
    return hash;
}

static inline int pawS_cmp(const String *lhs, const String *rhs)
{
    return paw_raw_cmp((void *)lhs->text, lhs->length, (void *)rhs->text,
                       rhs->length);
}

static inline size_t pawS_length(const String *s) { return s->length; }

typedef struct StringTable {
    String **strings;
    size_t capacity;
    size_t count;
} StringTable;

// Macro for testing equality between two interned strings
// Both x and y must be pointers to struct String from the string table.
#define pawS_eq(x, y) ((x) == (y))

void pawS_init(paw_Env *P);
void pawS_uninit(paw_Env *P);

void pawS_remove_str(paw_Env *P, String *s);
String *pawS_new_str(paw_Env *P, const char *text);
String *pawS_new_nstr(paw_Env *P, const char *text, size_t length);
String *pawS_new_fixed(paw_Env *P, const char *text);
void pawS_free_str(paw_Env *P, String *s);

// TODO: Hack for 2-phase initialization, used by string concatenation in
//       rt.c.
String *pawS_new_uninit(paw_Env *P, size_t length);
void pawS_register(paw_Env *P, String **pinit);

#endif // PAW_STR_H
