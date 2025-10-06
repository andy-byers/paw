// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODEGEN_CODEGEN_H
#define PAW_CODEGEN_CODEGEN_H

#include "paw.h"

struct Compiler;
struct TranslationUnit;

struct CodegenOptions {
    paw_Bool verify_module : 1;
    paw_Bool print_mlir : 1;
    paw_Bool enable_asan : 1;
    char opt_suffix;
};

EXTERN_C void pawCodegen_generate(struct Compiler *, struct TranslationUnit const *);

#endif // PAW_CODEGEN_CODEGEN_H
