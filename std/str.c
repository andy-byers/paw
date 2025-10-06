// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "str.h"

static paw_Str str_from_components(paw_Char const *text, uint32_t hash, paw_Int length)
{
    paw_Str str = PAW_MALLOC(sizeof *str + (size_t)length + 1);
    memcpy(str->text, text, (size_t)length);
    str->text[length] = '\0';
    str->length = length;
    str->hash = hash;
    return str;
}

static void sb_overflow(void)
{
    paw_Char const MESSAGE[] = "string builder buffer is too large";
    paw_prelude_panic(NULL, new_str(MESSAGE, PAW_LENGTHOF(MESSAGE)));
}

paw_Str new_str(paw_Char const *text, paw_Int length)
{
    uint32_t const hash = paw_builtin_hash_bytes(text, length, 0);
    return str_from_components(text, hash, length);
}

static void sb_ensure_space(struct StringBuilder *sb, paw_Int space)
{
    paw_assert(space >= 0);

    if (sb->length <= sb->capacity - space)
        return; // "buffer" already has enough space

    // condition: "2 * (sb->length + space) + sizeof('\0') > PAW_INT_MAX"
    if (sb->length > (PAW_INT_MAX - 1) / 2 - space)
        sb_overflow();

    paw_Int const new_capacity = 2 * (sb->length + space);
    paw_Char *new_buffer = PAW_MALLOC((size_t)new_capacity + 1);
    memcpy(new_buffer, sb_buffer(sb), (size_t)sb->length);
    new_buffer[sb->length] = '\0';

    PAW_FREE(sb->large_buffer);
    sb->large_buffer = new_buffer;
    sb->capacity = new_capacity;
}

void sb_init(struct StringBuilder *sb)
{
    *sb = (struct StringBuilder){
        .large_buffer = NULL,
        .capacity = SB_SMALL_LENGTH,
        .small_buffer[0] = '\0',
        .length = 0,
    };
}

paw_Char *sb_buffer(struct StringBuilder *sb)
{
    return sb->length <= SB_SMALL_LENGTH
        ? sb->small_buffer
        : sb->large_buffer;
}

paw_Str sb_create_str(struct StringBuilder *sb)
{
    return new_str(sb_buffer(sb), sb->length);
}

void sb_uninit(struct StringBuilder *sb)
{
    PAW_FREE(sb->large_buffer);
}

void sb_add_char(struct StringBuilder *sb, paw_Char c)
{
    sb_ensure_space(sb, 1);
    sb_buffer(sb)[sb->length++] = c;
    sb_buffer(sb)[sb->length] = '\0';
}

void sb_add_chars(struct StringBuilder *sb, paw_Char *text, paw_Int length)
{
    for (paw_Int i = 0; i < length; ++i)
        sb_add_char(sb, text[i]);
}

void sb_add_str(struct StringBuilder *sb, paw_Str str)
{
    sb_add_chars(sb, str->text, str->length);
}

void sb_add_format(struct StringBuilder *sb, paw_Char const *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    // TODO: consider writing directly into the "sb" buffer (need to grow buffer and retry vsnprintf if too small)
    paw_Char buffer[4096];
    int const n = vsnprintf(buffer, sizeof(buffer), fmt, args);
    paw_assert(0 <= n && (size_t)n < sizeof(buffer));
    sb_add_chars(sb, buffer, n);

    va_end(args);
}

void sb_reserve(struct StringBuilder *sb, paw_Int length)
{
    if (length > sb->length)
        sb_ensure_space(sb, length - sb->length);
}

