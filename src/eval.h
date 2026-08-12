// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_EVAL_H
#define PAW_EVAL_H

#include "value.h"

struct Mir;

enum MirEvalStatus {
    MES_EVALUATED,
    MES_NONCONSTANT,
    MES_PANICKED,
    MES_OVERFLOW,
    MES_DIVIDE0,
};

struct MirEvalResult {
    enum MirEvalStatus status;
    union {
        IrValue value;
        Str const *message;
    };
};

struct MirEvalResult pawMir_eval(struct Mir *mir);

#endif // PAW_EVAL_H

