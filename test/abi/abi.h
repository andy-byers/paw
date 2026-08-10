// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include <stdint.h>

struct ThreeInt8 {
    int8_t a;
    int8_t b;
    int8_t c;
};

struct Int64 {
    int64_t a;
};

struct TwoInt64 {
    int64_t a;
    int64_t b;
};

struct ThreeInt64 {
    int64_t a;
    int64_t b;
    int64_t c;
};

struct Float32 {
    int32_t a;
};

struct Int32Float32 {
    int32_t a;
    float b;
};

struct Float32Int64 {
    float a;
    int64_t b;
};

struct ThreeFloat64 {
    double a;
    double b;
    double c;
};

struct Int8Float64 {
    int8_t a;
    double b;
};
