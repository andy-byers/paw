// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_GLUE_H
#define PAW_GLUE_H

struct TranslationUnit {
    char const *modname;
    struct Mir *const *mirs;
    int mir_count;
};

#endif // PAW_GLUE_H
