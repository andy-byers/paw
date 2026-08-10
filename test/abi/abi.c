// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "abi.h"

struct ThreeInt8 c_ThreeInt8(struct ThreeInt8 value) { return value; }
struct Int64 c_Int64(struct Int64 value) { return value; }
struct TwoInt64 c_TwoInt64(struct TwoInt64 value) { return value; }
struct ThreeInt64 c_ThreeInt64(struct ThreeInt64 value) { return value; }
struct Float32 c_Float32(struct Float32 value) { return value; }
struct Int32Float32 c_Int32Float32(struct Int32Float32 value) { return value; }
struct Float32Int64 c_Float32Int64(struct Float32Int64 value) { return value; }
struct ThreeFloat64 c_ThreeFloat64(struct ThreeFloat64 value) { return value; }
struct Int8Float64 c_Int8Float64(struct Int8Float64 value) { return value; }
