// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "str.h"
#include "aux.h"
#include "gc.h"
#include "mem.h"
#include <assert.h>

static String **find_string(StringTable *st, const char *text, size_t length, uint32_t hash)
{
    String **s = &st->strings[hash & (st->capacity - 1)];
    while (*s && (hash != (*s)->hash || length != (*s)->length ||
                  0 != memcmp((*s)->text, text, cast_size(length)))) {
        s = &(*s)->next;
    }
    return s;
}

static String *new_string(paw_Env *P, size_t length)
{
    if (length > PAW_SIZE_MAX - sizeof(String) - 1 /* '\0' */) {
        pawM_error(P); // Size too big for paw_Int
    }
    String *str = pawM_new_fa(P, String, length + 1);
    pawG_add_object(P, cast_object(str), VSTRING);
    str->text[length] = '\0';
    str->length = length;
    str->next = NULL;
    str->flag = 0;
    return str;
}

static void grow_table(paw_Env *P, StringTable *st)
{
    const StringTable old = *st;

    // Double the capacity
#define DEFAULT_CAP 32
    size_t capacity = DEFAULT_CAP;
    while (capacity <= old.capacity) {
        capacity *= 2;
    }
    st->strings = pawM_new_vec(P, capacity, String *);
    st->capacity = capacity;

    // Copy entries
    for (size_t i = 0; i < old.capacity; ++i) {
        for (String *src = old.strings[i]; src;) {
            String *next = src->next;
            src->next = NULL;

            String **dst = find_string(st, src->text, src->length, src->hash);
            assert(!*dst); // Keys are unique
            *dst = src;    // Write to new buffer
            src = next;
        }
    }
    // Cleanup
    pawM_free_vec(P, old.strings, old.capacity);
    CHECK_GC(P);
}

void pawS_init(paw_Env *P)
{
    grow_table(P, &P->strings);
}

void pawS_uninit(paw_Env *P)
{
    StringTable *st = &P->strings;
    pawM_free_vec(P, st->strings, st->capacity);
    st->capacity = 0;
    st->count = 0;
}

static void grow_table_if_necessary(paw_Env *P, StringTable *st)
{
    if (st->count * 4 > st->capacity) {
        grow_table(P, st);
    }
}

String *pawS_new_nstr(paw_Env *P, const char *s, size_t n)
{
    StringTable *st = &P->strings;
    grow_table_if_necessary(P, st);
    const uint32_t hash = pawS_hash(s, n, 0);
    String **p = find_string(st, s, n, hash);
    if (!*p) {
        String *str = new_string(P, n);
        memcpy(str->text, s, n);
        str->hash = hash;
        ++st->count;
        *p = str;
    }
    return *p;
}

String *pawS_new_str(paw_Env *P, const char *text)
{
    return pawS_new_nstr(P, text, strlen(text));
}

void pawS_free_str(paw_Env *P, String *s)
{
    StringTable *st = &P->strings;
    String **p = find_string(st, s->text, s->length, s->hash);
    paw_assert(*p == s);
    *p = s->next; // Remove

    pawM_free_fa(P, s, s->length + 1);
}
