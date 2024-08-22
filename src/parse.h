// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_PARSE_H
#define PAW_PARSE_H

#include "lex.h"

#define limit_error(x, what, limit) pawX_error(x, "too many %s (limit is %d)", what, limit)

void pawP_init(paw_Env *P);

#endif // PAW_PARSE_H
