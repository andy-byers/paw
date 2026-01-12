// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdint.h>


struct Tiny1 {
    char x;
};

struct Tiny3 {
    char x;
    char y;
    char z;
};

struct Tiny1 get_tiny1(void *env, char x)
{
    (void)env;
    return (struct Tiny1){
        .x = x,
    };
}

struct Tiny3 get_tiny3(void *env, char x, char y, char z)
{
    (void)env;
    return (struct Tiny3){
        .x = x,
        .y = y,
        .z = z,
    };
}

struct Small2 {
    char x;
    int64_t y;
};

struct Small3 {
    _Bool x;
    char y;
    double z;
};

struct Small2 get_small2(void *env, char x, int64_t y)
{
    (void)env;
    return (struct Small2){
        .x = x,
        .y = y,
    };
}

struct Small3 get_small3(void *env, _Bool x, char y, double z)
{
    (void)env;
    return (struct Small3){
        .x = x,
        .y = y,
        .z = z,
    };
}

struct Large3 {
    double x;
    double y;
    _Bool z;
};

struct Large5 {
    int64_t x;
    int64_t y;
    struct Large3 z;
};

struct Large3 get_large3(void *env, double x, double y, _Bool z)
{
    (void)env;
    return (struct Large3){
        .x = x,
        .y = y,
        .z = z,
    };
}

struct Large5 get_large5(void *env, int64_t x, int64_t y, struct Large3 z)
{
    (void)env;
    return (struct Large5){
        .x = x,
        .y = y,
        .z = z,
    };
}


struct HFA2 {
    double x;
    double y;
};

struct HFA4 {
    double x;
    struct HFA2 y;
    double z;
};

struct HFA2 get_hfa2(void *env, double x, double y)
{
    (void)env;
    return (struct HFA2){
        .x = x,
        .y = y,
    };
}

struct HFA4 get_hfa4(void *env, double x, struct HFA2 y, double z)
{
    (void)env;
    return (struct HFA4){
        .x = x,
        .y = y,
        .z = z,
    };
}

