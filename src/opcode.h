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

#ifndef CONSTANT_MAX
#define CONSTANT_MAX A_MAX
#endif

#ifndef ITEM_MAX
#define ITEM_MAX A_MAX
#endif

#ifndef JUMP_MAX
#define JUMP_MAX S_MAX
#endif

#define OP_WIDTH 6
#define U_WIDTH 26
#define S_WIDTH 26
#define A_WIDTH 17
#define B_WIDTH 9

#define U_OFFSET OP_WIDTH
#define S_OFFSET OP_WIDTH
#define A_OFFSET (OP_WIDTH + B_WIDTH)
#define B_OFFSET OP_WIDTH

#define OP_MAX ((1 << OP_WIDTH) - 1)
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

// Opcode format: Each instruction is packed into a 32-bit unsigned integer (OpCode)
//
// legend:
//   G = global variable
//   K = constants 
//   L = local variables 
//   Up = upvalues 
//   P = function prototypes 
//
// NOTE: Opcode order is only important starting from OP_CALL (opcodes that have
//       corresponding metamethods).
//
// ORDER Op
typedef enum Op { // operands    stack in       stack out     side effects
OP_PUSHZERO,//       -           -              0             -
OP_PUSHONE,//        -           -              1             -
OP_PUSHSMI,//        U           -              u             -
OP_PUSHCONST,//      U           -              K[u]          -

OP_NOOP,//           -           -              -             -
OP_COPY,//           -           v              v v           -
OP_POP,//            U           vu..v1         -             -
OP_CLOSE,//          U           vu..v1         -             close stack to vu
OP_SHIFT,//          U           vu..v1         v1            closes stack to vu
OP_RETURN,//         -           f..v           v             closes stack to f
OP_CLOSURE,//        A B         vb..v1         f             captures vu..v1 in f = P[a]

OP_JUMP,//           S           -              -             pc += S
OP_JUMPFALSEPOP,//   S           v              -             pc += S
OP_JUMPFALSE,//      S           v              v             if !v, then pc += S
OP_JUMPNULL,//       S           v              v             if v == null, then pc += S

OP_GETGLOBAL,//      U           -              G[K[u]]       -
OP_GETLOCAL,//       U           -              L[u]          -
OP_SETLOCAL,//       U           v              -             L[u] = v
OP_GETUPVALUE,//     U           -              Up[u]         -
OP_SETUPVALUE,//     U           v              -             Up[u] = v

OP_NEWVARIANT,//     A B         vb..v1         a(vb..v1)     -
OP_NEWTUPLE,//       U           vu..v1         (vu..v1)      -
OP_NEWINSTANCE,//    U           -              v             -
OP_NEWLIST,//        U           vu..v1         [vu..v1]      -
OP_NEWMAP,//         U           v_2u..v1       [v_2u..v1]    -

OP_INITFIELD,//      U           i v            i             i.fields[u] = v

OP_FORNUM0,//        S           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
OP_FORNUM,//         S           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
OP_FORLIST0,//       S           *-*-*-*-*-*-*-*  see notes for  *-*-*-*-*-*-*
OP_FORLIST,//        S           *-*-*-*-*-*-*-*   description   *-*-*-*-*-*-*
OP_FORMAP0,//        S           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
OP_FORMAP,//         S           *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

OP_CMPI,//           U           x y            z             -  
OP_CMPF,//           U           x y            z             -
OP_CMPS,//           U           x y            z             -
OP_ARITHI1,//        U           x              y             -
OP_ARITHF1,//        U           x              y             -   
OP_ARITHI2,//        U           x y            z             -
OP_ARITHF2,//        U           x y            z             -   
OP_BITW1,//          U           x              y             -
OP_BITW2,//          U           x y            z             -

OP_NOT,//            -           v              !v            -
OP_LENGTH,//         U           v              #v            -
OP_CONCAT,//         U           x y            x+y           -
OP_GETELEM,//        U           v i            v[i]          -
OP_SETELEM,//        U           v i x          v[i] = x      -
OP_GETRANGE,//       U           v i j          v[i:j]        -
OP_SETRANGE,//       U           v i j x        v[i:j] = x    -

OP_CASTBOOL,//       U           v              v as bool     -  
OP_CASTINT,//        U           v              v as int      - 
OP_CASTFLOAT,//      U           v              v as float    - 
         
OP_CALL,//           U           f vu..v1       v             v = f(vu..v1)

OP_GETFIELD,//       U           v              v.u           -
OP_SETFIELD,//       U           v x            -             v.u=x
OP_GETTUPLE,//       U           v              v.u           -
OP_SETTUPLE,//       U           v x            -             v.u=x

NOPCODES
} Op;

enum CmpOp {
    CMP_EQ,
    CMP_NE,
    CMP_LT,
    CMP_LE,
    CMP_GT,
    CMP_GE,
};

enum ArithOp1 {
    ARITH1_NEG,
};

enum ArithOp2 {
    ARITH2_ADD,
    ARITH2_SUB,
    ARITH2_MUL,
    ARITH2_DIV,
    ARITH2_MOD,
};

enum BitwOp1 {
    BITW1_NOT,
};

enum BitwOp2 {
    BITW2_XOR,
    BITW2_AND,
    BITW2_OR,
    BITW2_SHL,
    BITW2_SHR,
};

enum BoolOp {
    BOOL_NOT,
};

enum StrOp {
    STR_LEN,
    STR_CONCAT,
    STR_GET,
    STR_GETN,
};

enum ListOp {
    LIST_LEN,
    LIST_CONCAT,
    LIST_GET,
    LIST_SET,
    LIST_GETN,
    LIST_SETN,
};

enum MapOp {
    MAP_LEN,
    MAP_GET,
    MAP_SET,
};

#define BINOP_IS_BOOL(op) ((op) <= BINARY_GE)
#define UNOP_IS_BOOL(op) ((op) == UNARY_NOT)

// Notes:
// * For OP_*OP, argument 'B' determines the type of the operands. The operands are
//   always the same type for OP_BINOP.
// * For the conversion operators (OP_CAST*), argument 'U' indicates the type of 'v'.
// * OP_RETURN returns from the enclosing function, moving the value on top of the
//   stack into the slot that previously held the function being called, and closes
//   the stack to that point.
// * OP_FOR*0 prepare a for loop. The loop body is skipped if the condition is
//   false. For OP_FORNUM0, the loop 'begin' is compared against the loop 'end' using
//   the sign of the loop 'step'. For OP_FORLIST0 and OP_FORMAP0, the loop is skipped 
//   if the container is empty. Both instructions will push the loop control variable.
// * OP_FOR* run a single for-loop step.

_Static_assert(UPVALUE_MAX <= A_MAX, "UPVALUE_MAX is too large");
_Static_assert(LOCAL_MAX <= A_MAX, "LOCAL_MAX is too large");
_Static_assert(FIELD_MAX <= U_MAX, "FIELD_MAX is too large");
_Static_assert(PARAM_MAX <= A_MAX, "PARAM_MAX is too large");
_Static_assert(CONSTANT_MAX <= A_MAX, "CONSTANT_MAX is too large");
_Static_assert(ITEM_MAX <= U_MAX, "ITEM_MAX is too large");
_Static_assert(JUMP_MAX <= S_MAX, "JUMP_MAX is too large");
_Static_assert(NOPCODES <= OP_MAX, "too many opcodes");

// sanity check opcode format
_Static_assert(OP_WIDTH + A_WIDTH + B_WIDTH == sizeof(OpCode) * 8 &&
        OP_WIDTH + U_WIDTH == sizeof(OpCode) * 8 &&
        OP_WIDTH + S_WIDTH == sizeof(OpCode) * 8 &&
        0 /* OP_OFFSET */ + OP_WIDTH == U_OFFSET &&
        0 /* OP_OFFSET */ + OP_WIDTH == S_OFFSET &&
        0 /* OP_OFFSET */ + OP_WIDTH == B_OFFSET &&
        B_OFFSET + B_WIDTH == A_OFFSET &&
        A_OFFSET + A_WIDTH == sizeof(OpCode) * 8,
        "invalid opcode format");

#endif // PAW_OPCODE_H
