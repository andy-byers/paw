// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_API_H
#define PAW_API_H

#include <stdio.h>

#if defined(PAW_OMIT_API_CHECK)
# include "util.h"
# define API_CHECK(P_, Expr_, Msg_) ((void)(P_), paw_assert((Expr_) && Msg_))
#else
# include <stdio.h>
# include <stdlib.h>
# define API_CHECK(P_, Expr_, Msg_) do { \
            if (!(Expr_)) { \
                (void)(P_); \
                fputs("error: " Msg_ " (" #Expr_ " was 0)", stderr); \
                abort(); \
            } \
        } while (0)
#endif

int main(void)
{
    API_CHECK(NULL, 1, "");
}

#endif // PAW_API_H
