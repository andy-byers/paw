// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdint.h>

// TODO: #[extern] should prevent name from being mangled so this is not necessary
#define MANGLE(Length_, Name_) _PN3ffi##Length_##Name_


struct Tiny {
    char x;
    char y;
    char z;
};

struct Tiny MANGLE(8, get_tiny)(void *env, char x, char y, char z)
{
    (void)env;
    return (struct Tiny){
        .x = x,
        .y = y,
        .z = z,
    };
}

struct Small {
    char x;
    int64_t y;
};

struct Small MANGLE(9, get_small)(void *env, char x, int64_t y)
{
    (void)env;
    return (struct Small){
        .x = x,
        .y = y,
    };
}

struct Large {
    int64_t x;
    double y;
    _Bool z;
};

struct Large MANGLE(9, get_large)(void *env, int64_t x, double y, _Bool z)
{
    (void)env;
    return (struct Large){
        .x = x,
        .y = y,
        .z = z,
    };
}

struct HFA {
    double x;
    double y;
};

struct HFA MANGLE(7, get_hfa)(void *env, double x, double y)
{
    (void)env;
    return (struct HFA){
        .x = x,
        .y = y,
    };
}
