// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "abi.h"

#include <assert.h>

#define paw_ThreeInt8 _PN3abi13paw_ThreeInt8
#define paw_Int64 _PN3abi9paw_Int64
#define paw_TwoInt64 _PN3abi12paw_TwoInt64
#define paw_ThreeInt64 _PN3abi14paw_ThreeInt64
#define paw_Float32 _PN3abi11paw_Float32
#define paw_Int32Float32 _PN3abi16paw_Int32Float32
#define paw_Float32Int64 _PN3abi16paw_Float32Int64
#define paw_ThreeFloat64 _PN3abi16paw_ThreeFloat64
#define paw_Int8Float64 _PN3abi15paw_Int8Float64

struct ThreeInt8 paw_ThreeInt8(struct ThreeInt8 value);
struct Int64 paw_Int64(struct Int64 value);
struct TwoInt64 paw_TwoInt64(struct TwoInt64 value);
struct ThreeInt64 paw_ThreeInt64(struct ThreeInt64 value);
struct Float32 paw_Float32(struct Float32 value);
struct Int32Float32 paw_Int32Float32(struct Int32Float32 value);
struct Float32Int64 paw_Float32Int64(struct Float32Int64 value);
struct ThreeFloat64 paw_ThreeFloat64(struct ThreeFloat64 value);
struct Int8Float64 paw_Int8Float64(struct Int8Float64 value);

int main(void)
{
    {
        struct ThreeInt8 const a = (struct ThreeInt8){
            .a = 1,
            .b = 2,
            .c = 3,
        };
        struct ThreeInt8 const b = paw_ThreeInt8(a);
        assert(a.a == b.a);
        assert(a.b == b.b);
        assert(a.c == b.c);
    }
    {
        struct Int64 const a = (struct Int64){
            .a = -123,
        };
        struct Int64 const b = paw_Int64(a);
        assert(a.a == b.a);
    }
    {
        struct TwoInt64 const a = (struct TwoInt64){
            .a = 1,
            .b = 10,
        };
        struct TwoInt64 const b = paw_TwoInt64(a);
        assert(a.a == b.a);
        assert(a.b == b.b);

    }
    {
        struct ThreeInt64 const a = (struct ThreeInt64){
            .a = -1,
            .b = -10,
            .c = -100,
        };
        struct ThreeInt64 const b = paw_ThreeInt64(a);
        assert(a.a == b.a);
        assert(a.b == b.b);
        assert(a.c == b.c);

    }
    {
        struct Float32 const a = (struct Float32){
            .a = 1.0,
        };
        struct Float32 const b = paw_Float32(a);
        assert(a.a == b.a);
    }
    {
        struct Int32Float32 const a = (struct Int32Float32){
            .a = 42,
            .b = 42.0,
        };
        struct Int32Float32 const b = paw_Int32Float32(a);
        assert(a.a == b.a);
        assert(a.b == b.b);

    }
    {
        struct Float32Int64 const a = (struct Float32Int64){
            .a = 1.23,
            .b = 10000,
        };
        struct Float32Int64 const b = paw_Float32Int64(a);
        assert(a.a == b.a);
        assert(a.b == b.b);

    }
    {
        struct ThreeFloat64 const a = (struct ThreeFloat64){
            .a = 1.1,
            .b = 2.2,
            .c = 3.3,
        };
        struct ThreeFloat64 const b = paw_ThreeFloat64(a);
        assert(a.a == b.a);
        assert(a.b == b.b);
        assert(a.c == b.c);

    }
    {
        struct Int8Float64 const a = (struct Int8Float64){
            .a = -1,
            .b = 2.3,
        };
        struct Int8Float64 const b = paw_Int8Float64(a);
        assert(a.a == b.a);
        assert(a.b == b.b);
    }
}
