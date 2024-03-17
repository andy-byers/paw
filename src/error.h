// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_ERROR_H
#define PAW_ERROR_H

#include "api.h"
#include "paw.h"
#include "value.h"

// TODO: Get rid of this module, pawE prefix belongs to env.h
void pawE_error(paw_Env *P, int error, const char *fmt, ...);
void pawE_type(paw_Env *P, const char *what);
void pawE_type2(paw_Env *P, const char *what);
void pawE_name(paw_Env *P, const char *name);
void pawE_attr(paw_Env *P, const char *attr);
void pawE_key(paw_Env *P, const char *key);
void pawE_index(paw_Env *P);
void pawE_range(paw_Env *P, const char *what);
void pawE_system(paw_Env *P, int error);
void pawE_index(paw_Env *P);

static inline void pawE_check_type(paw_Env *P, Value v, int type)
{
    if (api_type(v) != type) {
        pawE_error(P, PAW_ETYPE, "expected '%s' but found '%s'",
                   api_typename(type), api_typename(api_type(v)));
    }
}

static inline paw_Int pawE_check_int(paw_Env *P, Value v)
{
    paw_Bool lossless;
    pawE_check_type(P, v, PAW_TINTEGER);
    const paw_Int ival = pawV_to_int64(P, v, &lossless);
    if (!lossless) {
        pawE_error(P, PAW_ETYPE, "expected small (47-bit) integer");
    }
    return ival;
}

void pawE_type_(paw_Env *P, const char *what, const char *extra);

#endif // PAW_ERROR_H
