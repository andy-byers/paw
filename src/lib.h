// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_LIB_H
#define PAW_LIB_H

#include "paw.h"

#define IOLIB_NAME "io"
#define MATHLIB_NAME "math"

// Load the base library
void pawL_init(paw_Env *P);

void pawL_check_type(paw_Env *P, int index, int type);
void pawL_check_argc(paw_Env *P, int argc);
int pawL_check_varargc(paw_Env *P, int min, int max);

static inline const char *pawL_check_string(paw_Env *P, int index)
{
    // NOTE: Value at 'index' must exist
    pawL_check_type(P, index, PAW_TSTRING);
    return paw_string(P, index);
}

paw_Int pawL_check_int(paw_Env *P, int index);

typedef struct pawL_Attr {
    const char *name;
    paw_Function func;
} pawL_Attr;

// TODO: Look on disk for .paw files to load
void pawL_require_lib(paw_Env *P, const char *name);
void pawL_register_lib(paw_Env *P, const char *name, int nup, const pawL_Attr *attr);

#endif // PAW_LIB_H
