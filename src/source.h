// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_SOURCE_H
#define PAW_SOURCE_H

#include "auxlib.h"

struct SourceLoc {
    int line;
    int column;
};

struct SourceSpan {
    struct SourceLoc start;
    struct SourceLoc end;
};

static inline void pawSrc_init_location(struct SourceLoc *ploc)
{
    *ploc = (struct SourceLoc){1, 1};
}

void pawSrc_add_location(paw_Env *P, struct Buffer *b, struct SourceLoc loc);

#endif // PAW_SOURCE_H
