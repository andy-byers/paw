// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "str.h"
#include "auxlib.h"
#include "gc.h"
#include "mem.h"

#define ST_INDEX(st, h) ((h) & (st->capacity - 1))

static String *new_string(paw_Env *P, size_t length)
{
    if (length > PAW_SIZE_MAX - sizeof(String) - 1 /* '\0' */) {
        pawM_error(P);
    }
    String *str = pawM_new_flex(P, String, length + 1, sizeof(char));
    pawG_add_object(P, CAST_OBJECT(str), VSTRING);
    str->text[length] = '\0';
    str->length = length;
    str->next = NULL;
    str->flag = 0;
    return str;
}

static void grow_table(paw_Env *P, StringTable *st)
{
    StringTable const old = *st;

    // double the capacity
#define DEFAULT_CAP 32
    size_t capacity = DEFAULT_CAP;
    while (capacity <= old.capacity) {
        capacity *= 2;
    }
    st->strings = pawM_new_vec(P, capacity, String *);
    st->capacity = capacity;

    // copy entries
    for (size_t i = 0; i < old.capacity; ++i) {
        for (String *src = old.strings[i]; src;) {
            String *next = src->next; // save next string
            String **pdst = &st->strings[ST_INDEX(st, src->hash)];
            src->next = *pdst;
            *pdst = src;
            src = next;
        }
    }
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
    // strings contained in the table are freed during GC shutdown
    pawM_free_vec(P, st->strings, st->capacity);
    st->capacity = 0;
    st->count = 0;
}

#define ENSURE_SPACE(P, st)                     \
    do {                                        \
        if ((st)->count > (st)->capacity / 4) { \
            grow_table(P, st);                  \
        }                                       \
    } while (0)

String *pawS_new_nstr(paw_Env *P, char const *s, size_t n)
{
    StringTable *st = &P->strings;
    ENSURE_SPACE(P, st);

    uint32_t const hash = pawS_hash(s, n, 0);
    String **plist = &st->strings[ST_INDEX(st, hash)];
    for (String *p = *plist; p; p = p->next) {
        if (n == p->length && 0 == memcmp(p->text, s, n)) {
            return p; // already exists
        }
    }

    String *str = new_string(P, n);
    memcpy(str->text, s, n);
    str->hash = hash;
    ++st->count;
    str->next = *plist;
    *plist = str;
    return str;
}

String *pawS_new_str(paw_Env *P, char const *text)
{
    return pawS_new_nstr(P, text, strlen(text));
}

String *pawS_new_fixed(paw_Env *P, char const *text)
{
    String *s = pawS_new_str(P, text);
    Object *o = CAST_OBJECT(s);
    if (o == P->gc_all) {
        pawG_fix_object(P, o);
    }
    return s;
}

void pawS_free_str(paw_Env *P, String *s)
{
    // remove from string table, if present (strings from pawS_new_uninit are
    // not in the table until pawS_register is called)
    StringTable *st = &P->strings;
    String **p = &st->strings[ST_INDEX(st, s->hash)];
    while (*p != NULL && *p != s)
        p = &(*p)->next;
    if (*p != NULL) {
        paw_assert(*p == s);
        *p = s->next; // remove
        --st->count;
    }

    pawM_free_flex(P, s, s->length + 1, sizeof(char));
}

String *pawS_new_uninit(paw_Env *P, size_t length)
{
    ENSURE_SPACE(P, &P->strings);
    return new_string(P, length);
}

void pawS_register(paw_Env *P, String **pinit)
{
    String *str = *pinit;
    StringTable *st = &P->strings;
    str->hash = pawS_hash(str->text, str->length, 0);
    String **const plist = &st->strings[ST_INDEX(st, str->hash)];
    for (String *p = *plist; p; p = p->next) {
        if (p->hash == str->hash &&
            p->length == str->length &&
            memcmp(p->text, str->text, str->length) == 0) {
            *pinit = p;
            return;
        }
    }
    ++st->count;
    str->next = *plist;
    *plist = str;
}
