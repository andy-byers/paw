// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_CODE_H
#define PAW_CODE_H

#include "mem.h"

#define K_ALIGNOF_NODE _Alignof(void *)
#define K_ALIGNAS_NODE _Alignas(void *)

typedef struct NodeId {
    unsigned value;
} NodeId;

typedef struct DeclId {
    unsigned modno;
    unsigned value;
} DeclId;

#define INVALID_NODE_ID (struct NodeId){(unsigned)-1}
#define INVALID_DECL_ID (struct DeclId){(unsigned)-1, (unsigned)-1}
#define NODE_ID_EXISTS(Id_) ((Id_).value != INVALID_NODE_ID.value)
#define DECL_ID_EXISTS(Id_) ((Id_).value != INVALID_DECL_ID.value)


// TODO: should specialize the next 2 enumerations and move to AST and HIR modules (similar to MirUnaryOpKind)

// ORDER UnaryOp
enum UnaryOp {
    UNARY_NEG,
    UNARY_NOT,
    UNARY_BNOT,
    UNARY_DEREF,
    UNARY_ADDROF,
};

// ORDER BinaryOp
enum BinaryOp {
    BINARY_EQ,
    BINARY_NE,
    BINARY_LT,
    BINARY_LE,
    BINARY_GT,
    BINARY_GE,
    BINARY_AS,
    BINARY_ADD,
    BINARY_SUB,
    BINARY_MUL,
    BINARY_DIV,
    BINARY_MOD,
    BINARY_BXOR,
    BINARY_BAND,
    BINARY_BOR,
    BINARY_SHL,
    BINARY_SHR,
};

enum JumpKind {
    JUMP_BREAK,
    JUMP_CONTINUE,
};

// ORDER BuiltinKind
enum BuiltinKind {
    BUILTIN_UNIT,
    BUILTIN_BOOL,
    BUILTIN_CHAR,
    BUILTIN_INT8,
    BUILTIN_INT16,
    BUILTIN_INT32,
    BUILTIN_INT64,
    BUILTIN_ISIZE,
    BUILTIN_UINT8,
    BUILTIN_UINT16,
    BUILTIN_UINT32,
    BUILTIN_UINT64,
    BUILTIN_USIZE,
    BUILTIN_FLOAT32,
    BUILTIN_FLOAT64,
    BUILTIN_STR,
    BUILTIN_PTR,
    BUILTIN_SLICE,
    BUILTIN_OPTION,
    BUILTIN_RESULT,
    BUILTIN_RANGE,
    BUILTIN_RANGE_TO,
    BUILTIN_RANGE_FROM,
    BUILTIN_RANGE_FULL,
    BUILTIN_RANGE_INCLUSIVE,
    BUILTIN_RANGE_TO_INCLUSIVE,
    BUILTIN_MANUALLY_DROP,

    NBUILTINS,
};

enum CoreTrait {
    CORE_TRAIT_COPY,
    CORE_TRAIT_DROP,
    CORE_TRAIT_DEFAULT,
    CORE_TRAIT_HASH,
    CORE_TRAIT_EQUALS,
    CORE_TRAIT_COMPARE,
    CORE_TRAIT_INDEX,
    CORE_TRAIT_FROM,
    CORE_TRAIT_INTO,
    NUM_CORE_TRAITS
};


enum NumberSuffix {
    NS_NONE,
    NS_I8,
    NS_I16,
    NS_I32,
    NS_I64,
    NS_ISIZE,
    NS_U8,
    NS_U16,
    NS_U32,
    NS_U64,
    NS_USIZE,
    NS_F32,
    NS_F64,
};


enum FnKind {
    FUNC_MODULE,
    FUNC_CLOSURE,
    FUNC_FUNCTION,
    FUNC_METHOD,
};

// From https://stackoverflow.com/questions/8513911
static inline paw_Uint hash_combine(paw_Uint seed, paw_Uint v)
{
    // TODO: versions for other sizes of paw_Uint
    paw_Uint const mul = 0x9DDFEA08EB382D69ULL;
    paw_Uint a = (v ^ seed) * mul;
    a ^= (a >> 47);
    paw_Uint b = (seed ^ a) * mul;
    b ^= (b >> 47);
    return b * mul;
}

#endif // PAW_CODE_H
