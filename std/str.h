// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_STD_STR_H
#define PAW_STD_STR_H

#include "paw.h"


//TODO #define NEW_LITERAL(Text_) new_str(Text_ "", PAW_LENGTHOF(Text_))
//TODO paw_Str new_str(paw_Char const *text, paw_Int length);
//TODO
//TODO
//TODO struct StringBuilder {
//TODO #define SB_SMALL_LENGTH 4096
//TODO     paw_Char small_buffer[SB_SMALL_LENGTH + 1];
//TODO     paw_Char *large_buffer;
//TODO     paw_Int length;
//TODO     paw_Int capacity;
//TODO };
//TODO
//TODO void sb_init(struct StringBuilder *sb);
//TODO paw_Char *sb_buffer(struct StringBuilder *sb);
//TODO paw_Str sb_create_str(struct StringBuilder *sb);
//TODO void sb_uninit(struct StringBuilder *sb);
//TODO void sb_add_char(struct StringBuilder *sb, paw_Char c);
//TODO void sb_add_chars(struct StringBuilder *sb, paw_Char *text, paw_Int length);
//TODO void sb_add_str(struct StringBuilder *sb, paw_Str str);
//TODO void sb_add_format(struct StringBuilder *sb, paw_Char const *fmt, ...);
//TODO void sb_reserve(struct StringBuilder *sb, paw_Int length);

#endif // PAW_STD_STR_H
