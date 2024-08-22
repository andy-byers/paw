// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_OPCODE_H
#define PAW_OPCODE_H

#include "paw.h"

#ifndef UPVALUE_MAX
#define UPVALUE_MAX 64
#endif

#ifndef LOCAL_MAX
#define LOCAL_MAX 1024
#endif

#ifndef FIELD_MAX
#define FIELD_MAX 4096
#endif

#ifndef PARAM_MAX
#define PARAM_MAX 256
#endif

#ifndef ITEM_MAX
#define ITEM_MAX A_MAX
#endif

#ifndef JUMP_MAX
#define JUMP_MAX S_MAX
#endif

#define decode_jump(x) ((int)(x) - JUMP_MAX)
#define encode_jump_over(x) ((x) + JUMP_MAX)
#define encode_jump_back(x) (JUMP_MAX - (x))

#define OP_WIDTH 6
#define U_WIDTH 26
#define S_WIDTH 26
#define A_WIDTH 17
#define B_WIDTH 9

#define U_OFFSET OP_WIDTH
#define S_OFFSET OP_WIDTH
#define A_OFFSET (OP_WIDTH + B_WIDTH)
#define B_OFFSET OP_WIDTH

#define U_MAX ((1 << U_WIDTH) - 1)
#define S_MAX (U_MAX >> 1)
#define A_MAX ((1 << A_WIDTH) - 1)
#define B_MAX ((1 << B_WIDTH) - 1)

#define MASK1(n, p) ((~((~(OpCode)0) << n)) << p)
#define MASK0(n, p) (~MASK1(n, p))

#define CREATE_OP(o) ((OpCode)(o))
#define GET_OP(v) ((v) & MASK1(OP_WIDTH, 0))
#define SET_OP(v, o) (*(v) = (*(v) & MASK0(OP_WIDTH, 0)) | (OpCode)(o))

#define CREATE_U(o, u) ((OpCode)(o) | ((OpCode)(u) << U_OFFSET))
#define GET_U(v) (((v) >> U_OFFSET) & MASK1(U_WIDTH, 0))
#define SET_U(v, u) \
    (*(v) = (*(v) & MASK0(U_WIDTH, U_OFFSET)) | ((OpCode)(u) << U_OFFSET))

#define CREATE_S(o, s) CREATE_U(o, (int)(s) + S_MAX)
#define GET_S(v) ((int)GET_U(v) - S_MAX)
#define SET_S(v, s) SET_U(v, (int)(s) + S_MAX)

#define CREATE_AB(o, a, b) \
    ((OpCode)(op) | ((OpCode)(a) << A_OFFSET) | ((OpCode)(b) << B_OFFSET))
#define GET_A(v) ((v) >> A_OFFSET)
#define SET_A(v, a) \
    (*(v) = (*(v) & MASK0(A_WIDTH, A_OFFSET)) | ((OpCode)(a) << A_OFFSET))
#define GET_B(v) (((v) >> B_OFFSET) & MASK1(B_WIDTH, 0))
#define SET_B(v, b) \
    (*(v) = (*(v) & MASK0(B_WIDTH, B_OFFSET)) | ((OpCode)(b) << B_OFFSET))

typedef uint32_t OpCode;

// clang-format off
//
// Opcode format: Each instruction is packed into a 32-bit unsigned integer (OpCode)
//
// legend:
//   G = global variable
//   K = constants 
//   L = local variables 
//   Up = upvalues 
//   P = function prototypes 
//   C = class layouts
//
// NOTE: Opcode order is only important starting from OP_CALL (opcodes that have
//       corresponding metamethods).
//
// ORDER Op
typedef enum Op { // operands    stack in       stack out     side effects
OP_PUSHUNIT,//       -           -              ()            -
OP_PUSHTRUE,//       -           -              true          -
OP_PUSHFALSE,//      -           -              false         -
OP_PUSHCONST,//      U           -              K[u]          -
OP_PUSHSTRUCT,//     U           -              C[u]          -

OP_POP,//            U           vu..v1         -             -
OP_CLOSE,//          U           vu..v1         -             close stack to vu
OP_COPY,//           -           v              v v           -
OP_RETURN,//         -           f..v           v             closes stack to f
OP_SHIFT,//          U           vu..v1         v1            closes stack to vu

OP_CLOSURE,//        A B         vb..v1         f             captures vu..v1 in f = P[a]
OP_INVOKE,//      

OP_JUMP,//           S           -              -             pc += S
OP_JUMPFALSEPOP,//   S           v              -             pc += S
OP_JUMPFALSE,//      S           v              v             if !v, then pc += S
OP_JUMPNULL,//       S           v              v             if v == null, then pc += S

OP_GLOBAL,//         U           v              -             define G[K[u]] = v
OP_GETGLOBAL,//      U           -              G[K[u]]       -
OP_SETGLOBAL,//      U           v              -             G[K[u]] = v
OP_GETLOCAL,//       U           -              L[u]          -
OP_SETLOCAL,//       U           v              -             L[u] = v
OP_GETUPVALUE,//     U           -              Up[u]         -
OP_SETUPVALUE,//     U           v              -             Up[u] = v

OP_MATCHVARIANT,//   U           v              disc(v)==u    -
OP_UNPACKTUPLE,//    U           v              vu..v1        -  
OP_UNPACKINSTANCE,// U           v              vu..v1        -
OP_UNPACKVARIANT,//  U           v              vu..v1        -

OP_NEWVARIANT,//     A B         vb..v1         a(vb..v1)     -
OP_NEWTUPLE,//       U           vu..v1         (vu..v1)      -
OP_NEWINSTANCE,//    U           -              v             v = new instance of class C[u]
OP_INITFIELD,//      U           i v            i             i.fields[u] = v
OP_NEWLIST,//        U           vu..v1         [vu..v1]      -
OP_NEWMAP,//         U           v_2n..v1       {v_2n..v1}    -

OP_UNWRAP,//         -           v              t             throws an error if disc(v) != 0

OP_FORNUM0,//        S           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
OP_FORNUM,//         S           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
OP_FORLIST0,//       S           *-*-*-*-*-*-*-*    see notes    *-*-*-*-*-*-*
OP_FORLIST,//        S           *-*-*-*-*-*-*-*   for details   *-*-*-*-*-*-*
OP_FORMAP0,//        S           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
OP_FORMAP,//         S           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

OP_UNOP,//           A B         v              ops[a](v)     -
OP_BINOP,//          A B         l r            ops[a](l, r)  -
OP_UNMM,//           A B         v              v.mtd[a]()    -
OP_BINMM,//          A B         l r            l.mtd[a](r)   -
         
OP_CASTBOOL,//       U           v              v as bool     -  
OP_CASTINT,//        U           v              v as int      - 
OP_CASTFLOAT,//      U           v              v as float    - 
         
OP_VARARG,//         A B         vu..v1         [vu..v1]      -
OP_INIT,
OP_CALL,//           U           f vu..v1       v             v = f(vu..v1)

OP_GETFIELD,//       U           v              v.u           -
OP_SETFIELD,//       U           v x            -             v.u=x
OP_GETTUPLE,//       U           v              v.u           -
OP_SETTUPLE,//       U           v x            -             v.u=x
OP_GETELEM,//        U           v i            v[i]          -
OP_SETELEM,//        U           v i x          -             v[i]=x
OP_GETSLICE,//       U           v i j          v[i:j]        -
OP_SETSLICE,//       U           v i j x        -             v[i:j]=x

NOPCODES
} Op;

// ORDER UnaryOp
typedef enum {
    UNARY_LEN, 
    UNARY_NEG, 
    UNARY_NOT, 
    UNARY_BNOT,

    NUNARYOPS
} UnaryOp;

// ORDER BinaryOp
typedef enum {
    BINARY_EQ,   
    BINARY_NE,   
    BINARY_LT,   
    BINARY_LE,   
    BINARY_GT,   
    BINARY_GE,   
    BINARY_IN,
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

    NBINARYOPS
} BinaryOp;

// clang-format on
//
// Notes:
// * OP_*OP uses argument 'B' to indicate the type of the operands. The operands
// are
//   always the same type.
// * The conversion operators (OP_BOOL, OP_INT, etc.) use argument 'U' to
// indicate
//   the type of 'v'.
// * OP_RETURN replaces the current call frame with the value on top of the
// stack.
//   The current call frame consists of the function object or reciever 'f', its
//   parameters, and all locals declared between the start of the call and the
//   'return'.
// * OP_FOR*0 prepare a for loop. The loop body is skipped if the condition is
// false.
//   For OP_FORNUM0, the loop 'begin' is compared against the loop 'end' using
//   the sign of the loop 'step'. For OP_FORIN0, the loop is skipped if the
//   container is empty. Both instructions will push the loop control variable.
// * OP_FOR* run a single for-loop step.

#define METAMETHOD0 OP_CALL
#define unop2meta(o) ((Metamethod)((o) + MM_LEN))
#define binop2meta(o) ((Metamethod)((o) + MM_EQ))
#define binop_has_r(o) ((o) >= BINARY_ADD)
#define binop_r(o) ((o) + MM_EQ + MM_RADD - MM_ADD)
#define binop_is_bool(o) ((o) <= BINARY_GE)
#define unop_is_bool(o) ((o) == UNARY_NOT)

// ORDER Metamethod
typedef enum {
    MM_CALL,

    // getters and setters
    MM_GETATTR,
    MM_SETATTR,
    MM_GETITEM,
    MM_SETITEM,
    MM_GETSLICE,
    MM_SETSLICE,

    // type conversions
    MM_BOOL,
    MM_INT,
    MM_FLOAT,
    MM_STRING,
    MM_ARRAY,
    MM_MAP,

    // unary operators
    MM_LEN,
    MM_NEG,
    MM_NOT,
    MM_BNOT,

    // binary comparisons
    MM_EQ,
    MM_NE,
    MM_LT,
    MM_LE,
    MM_GT,
    MM_GE,
    MM_CONTAINS,
    MM_CAST,

    // binary arithmetic
    MM_ADD,
    MM_SUB,
    MM_MUL,
    MM_DIV,
    MM_MOD,
    MM_BXOR,
    MM_BAND,
    MM_BOR,
    MM_SHL,
    MM_SHR,

    // reverse binary arithmetic
    MM_RADD,
    MM_RSUB,
    MM_RMUL,
    MM_RDIV,
    MM_RMOD,
    MM_RBXOR,
    MM_RBAND,
    MM_RBOR,
    MM_RSHL,
    MM_RSHR,

    // misc. metamethods
    MM_INIT,
    MM_NULL,

    NMETAMETHODS
} Metamethod;

enum ListOp {
    VO_CLONE,
    VO_INSERT,
    VO_PUSH,
    VO_POP,
    VO_POP2,
};

enum MapOp {
    MO_CLONE,
    MO_ERASE,
};

_Static_assert(NOPCODES <= ((1 << OP_WIDTH) - 1),
               "too many opcodes (see opcode.h)");

// sanity check opcode format
_Static_assert(OP_WIDTH + A_WIDTH + B_WIDTH == sizeof(OpCode) * 8 &&
                   OP_WIDTH + U_WIDTH == sizeof(OpCode) * 8 &&
                   OP_WIDTH + S_WIDTH == sizeof(OpCode) * 8 &&
                   0 /* OP_OFFSET */ + OP_WIDTH == U_OFFSET &&
                   0 /* OP_OFFSET */ + OP_WIDTH == S_OFFSET &&
                   0 /* OP_OFFSET */ + OP_WIDTH == B_OFFSET &&
                   B_OFFSET + B_WIDTH == A_OFFSET &&
                   A_OFFSET + A_WIDTH == sizeof(OpCode) * 8,
               "invalid opcode format (see opcode.h)");

// sanity check metamethod ordering
_Static_assert(unop2meta(UNARY_LEN) == MM_LEN &&
                   binop2meta(BINARY_EQ) == MM_EQ &&
                   binop2meta(BINARY_NE) == MM_NE &&
                   binop2meta(BINARY_ADD) == MM_ADD &&
                   !binop_has_r(BINARY_IN) && binop_has_r(BINARY_SHR) &&
                   binop_r(BINARY_ADD) == MM_RADD &&
                   binop_r(BINARY_SHR) == MM_RSHR,
               "invalid metamethod format (see opcode.h)");

#endif // PAW_OPCODE_H
