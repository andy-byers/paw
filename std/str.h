// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STD_STR_H
#define PAW_STD_STR_H

#include "paw.h"


#define NEW_LITERAL(Text_) new_str(Text_ "", PAW_LENGTHOF(Text_))
paw_Str new_str(paw_Char const *text, paw_Int length);


struct StringBuilder {
#define SB_SMALL_LENGTH 4096
    paw_Char small_buffer[SB_SMALL_LENGTH + 1];
    paw_Char *large_buffer;
    paw_Int length;
    paw_Int capacity;
};

void sb_init(struct StringBuilder *sb);
paw_Char *sb_buffer(struct StringBuilder *sb);
paw_Str sb_create_str(struct StringBuilder *sb);
void sb_uninit(struct StringBuilder *sb);
void sb_add_char(struct StringBuilder *sb, paw_Char c);
void sb_add_chars(struct StringBuilder *sb, paw_Char *text, paw_Int length);
void sb_add_str(struct StringBuilder *sb, paw_Str str);
void sb_add_format(struct StringBuilder *sb, paw_Char const *fmt, ...);
void sb_reserve(struct StringBuilder *sb, paw_Int length);

#endif // PAW_STD_STR_H
