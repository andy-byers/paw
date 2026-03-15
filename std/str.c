// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "str.h"

//TODO static paw_Str str_from_components(paw_Char const *text, uint32_t hash, paw_Int length)
//TODO {
//TODO     paw_Str str = PAW_MALLOC(sizeof *str + (size_t)length + 1);
//TODO     memcpy(str->text, text, (size_t)length);
//TODO     str->text[length] = '\0';
//TODO     str->length = length;
//TODO     str->hash = hash;
//TODO     return str;
//TODO }
//TODO
//TODO static void sb_overflow(void)
//TODO {
//TODO     paw_Char const MESSAGE[] = "string builder buffer is too large";
//TODO     paw_panic(NULL, new_str(MESSAGE, PAW_LENGTHOF(MESSAGE)));
//TODO }
//TODO
//TODO paw_Str new_str(paw_Char const *text, paw_Int length)
//TODO {
//TODO     uint32_t const hash = paw_builtin_hash_bytes(text, length, 0);
//TODO     return str_from_components(text, hash, length);
//TODO }
//TODO
//TODO static void sb_ensure_space(struct StringBuilder *sb, paw_Int space)
//TODO {
//TODO     PAW_ASSERT(space >= 0);
//TODO
//TODO     if (sb->length <= sb->capacity - space)
//TODO         return; // "buffer" already has enough space
//TODO
//TODO     // condition: "2 * (sb->length + space) + sizeof('\0') > PAW_INT_MAX"
//TODO     if (sb->length > (PAW_INT_MAX - 1) / 2 - space)
//TODO         sb_overflow();
//TODO
//TODO     paw_Int const new_capacity = 2 * (sb->length + space);
//TODO     paw_Char *new_buffer = PAW_MALLOC((size_t)new_capacity + 1);
//TODO     memcpy(new_buffer, sb_buffer(sb), (size_t)sb->length);
//TODO     new_buffer[sb->length] = '\0';
//TODO
//TODO     PAW_FREE(sb->large_buffer);
//TODO     sb->large_buffer = new_buffer;
//TODO     sb->capacity = new_capacity;
//TODO }
//TODO
//TODO void sb_init(struct StringBuilder *sb)
//TODO {
//TODO     *sb = (struct StringBuilder){
//TODO         .large_buffer = NULL,
//TODO         .capacity = SB_SMALL_LENGTH,
//TODO         .small_buffer[0] = '\0',
//TODO         .length = 0,
//TODO     };
//TODO }
//TODO
//TODO paw_Char *sb_buffer(struct StringBuilder *sb)
//TODO {
//TODO     return sb->length <= SB_SMALL_LENGTH
//TODO         ? sb->small_buffer
//TODO         : sb->large_buffer;
//TODO }
//TODO
//TODO paw_Str sb_create_str(struct StringBuilder *sb)
//TODO {
//TODO     return new_str(sb_buffer(sb), sb->length);
//TODO }
//TODO
//TODO void sb_uninit(struct StringBuilder *sb)
//TODO {
//TODO     PAW_FREE(sb->large_buffer);
//TODO }
//TODO
//TODO void sb_add_char(struct StringBuilder *sb, paw_Char c)
//TODO {
//TODO     sb_ensure_space(sb, 1);
//TODO     sb_buffer(sb)[sb->length++] = c;
//TODO     sb_buffer(sb)[sb->length] = '\0';
//TODO }
//TODO
//TODO void sb_add_chars(struct StringBuilder *sb, paw_Char *text, paw_Int length)
//TODO {
//TODO     for (paw_Int i = 0; i < length; ++i)
//TODO         sb_add_char(sb, text[i]);
//TODO }
//TODO
//TODO void sb_add_str(struct StringBuilder *sb, paw_Str str)
//TODO {
//TODO     sb_add_chars(sb, str->text, str->length);
//TODO }
//TODO
//TODO void sb_add_format(struct StringBuilder *sb, paw_Char const *fmt, ...)
//TODO {
//TODO     va_list args;
//TODO     va_start(args, fmt);
//TODO
//TODO     // TODO: consider writing directly into the "sb" buffer (need to grow buffer and retry vsnprintf if too small)
//TODO     paw_Char buffer[4096];
//TODO     int const n = vsnprintf(buffer, sizeof(buffer), fmt, args);
//TODO     PAW_ASSERT(0 <= n && (size_t)n < sizeof(buffer));
//TODO     sb_add_chars(sb, buffer, n);
//TODO
//TODO     va_end(args);
//TODO }
//TODO
//TODO void sb_reserve(struct StringBuilder *sb, paw_Int length)
//TODO {
//TODO     if (length > sb->length)
//TODO         sb_ensure_space(sb, length - sb->length);
//TODO }
//TODO
