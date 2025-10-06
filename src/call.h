// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CALL_H
#define PAW_CALL_H

#include "core.h"

int pawC_try(paw_Env *P, paw_Function call, void *arg);
_Noreturn void pawC_throw(paw_Env *P, int error);

#endif // PAW_CALL_H
