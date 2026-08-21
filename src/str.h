// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_STR_H
#define PAW_STR_H

#include "core.h"

#ifndef PAW_INTERN_LIMIT
#define PAW_INTERN_LIMIT 16384
#endif // PAW_INTERN_LIMIT

struct Str;

// sdbm hash modified from http://www.cse.yorku.ca/~oz/hash.html
uint32_t pawS_hash(void const *data, size_t size, uint32_t hash);

int pawS_cmp(struct Str const *lhs, struct Str const *rhs);

char const *pawS_text(struct Str const *s);

size_t pawS_length(struct Str const *s);

typedef struct StringTable {
    struct Str **strings;
    size_t capacity;
    size_t count;
} StringTable;

// Macro for testing equality between two interned strings
// Both operands must be pointers to `struct Str` from the string table.
#define pawS_eq(Lhs_, Rhs_) ((Lhs_) == (Rhs_))

void pawS_init(paw_Env *P);
void pawS_uninit(paw_Env *P);

void pawS_remove_str(paw_Env *P, struct Str *s);
struct Str *pawS_new_str(paw_Env *P, char const *text);
struct Str *pawS_new_nstr(paw_Env *P, char const *text, size_t length);
struct Str *pawS_new_fixed(paw_Env *P, char const *text);
void pawS_free_str(paw_Env *P, struct Str *s);

// TODO: Hack for 2-phase initialization, used by string concatenation in
//       rt.c.
struct Str *pawS_new_uninit(paw_Env *P, size_t length);
void pawS_register(paw_Env *P, struct Str **pinit);

#endif // PAW_STR_H
