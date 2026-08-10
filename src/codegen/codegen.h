// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODEGEN_CODEGEN_H
#define PAW_CODEGEN_CODEGEN_H

#include "core.h"

struct Compiler;
struct TranslationUnit;

struct CodegenOptions {
    char const *target;
    unsigned verify_module : 1;
    unsigned print_ir : 1;
    unsigned enable_asan : 1;
    unsigned build_tests : 1;
    unsigned compile_only : 1;
    unsigned add_debug_info : 1;
    char opt_suffix;
};

EXTERN_C void pawCodegen_generate(struct Compiler *, struct TranslationUnit const *);

#endif // PAW_CODEGEN_CODEGEN_H
