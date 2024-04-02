// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_OPCODE_H
#define PAW_OPCODE_H

#include "paw.h"

typedef uint32_t OpCode;

#ifndef UPVALUE_MAX
#define UPVALUE_MAX 64
#endif

#ifndef LOCAL_MAX
#define LOCAL_MAX 1024
#endif

#ifndef JUMP_MAX
#define JUMP_MAX S_MAX
#endif

#ifndef ARGC_MAX
#define ARGC_MAX 256
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

//_Static_assert(
//        OP_WIDTH + A_WIDTH + B_WIDTH == sizeof(OpCode) * 8 &&
//        OP_WIDTH + U_WIDTH == sizeof(OpCode) * 8 &&
//        OP_WIDTH + S_WIDTH == sizeof(OpCode) * 8 &&
//        0 /* OP_OFFSET */ + OP_WIDTH == U_OFFSET &&
//        0 /* OP_OFFSET */ + OP_WIDTH == S_OFFSET &&
//        0 /* OP_OFFSET */ + OP_WIDTH == B_OFFSET &&
//        B_OFFSET + B_WIDTH == A_OFFSET &&
//        A_OFFSET + A_WIDTH == sizeof(OpCode) * 8,
//        "invalid opcode format (see opcode.h)");

#define mask1(n, p) ((~((~(OpCode)0) << n)) << p)
#define mask0(n, p) (~mask1(n, p))

#define create_OP(o) ((OpCode)(o))
#define get_OP(x) ((x) & mask1(OP_WIDTH, 0))
#define set_OP(x, o) (*(x) = (*(x) & mask0(OP_WIDTH, 0)) | (OpCode)(o))

#define create_U(o, u) ((OpCode)(o) | ((OpCode)(u) << U_OFFSET))
#define get_U(x) (((x) >> U_OFFSET) & mask1(U_WIDTH, 0))
#define set_U(x, u) (*(x) = (*(x) & mask0(U_WIDTH, U_OFFSET)) | ((OpCode)(u) << U_OFFSET))

#define create_S(o, s) create_U(o, (int)(s) + S_MAX)
#define get_S(x) ((int)get_U(x) - S_MAX)
#define set_S(x, s) set_U(x, (int)(s) + S_MAX)

#define create_AB(o, a, b) ((OpCode)(op) | ((OpCode)(a) << A_OFFSET) | \
                            ((OpCode)(b) << B_OFFSET))
#define get_A(x) ((x) >> A_OFFSET)
#define set_A(x, a) (*(x) = (*(x) & mask0(A_WIDTH, A_OFFSET)) | ((OpCode)(a) << A_OFFSET))
#define get_B(x) (((x) >> B_OFFSET) & mask1(B_WIDTH, 0))
#define set_B(x, b) (*(x) = (*(x) & mask0(B_WIDTH, B_OFFSET)) | ((OpCode)(b) << B_OFFSET))

// clang-format off
//
// Opcode format: Instructions are represented by 32-bit unsigned integers
//
// legend:
//   G = global variable (requires constant string index)
//   K = constants (requires 16-bit index)
//   L = local variables (i.e. the stack, requires up to 26-bit index)
//   Up = upvalues (requires 16-bit index)
//   P = function prototypes (requires 16-bit index)
//
typedef enum Op { // operands    stack in     stack out    side effects
OP_PUSHNULL,//       -           -            null         -
OP_PUSHTRUE,//       -           -            true         -
OP_PUSHFALSE,//      -           -            false        -
OP_PUSHCONST,//      U           -            K[u]         -

OP_POP,//            -           v            -            -
OP_CLOSE,//          A B         v_a..v_1     -            if b, close stack to v_a
OP_RETURN,//         -           f..v         v            closes stack to f

OP_CLOSURE,//        A B         v_b..v_1     f            captures v_u..v_1 in f = P[a]
OP_INVOKE,//         A B         o v_b..v_1   f(v_b..v_1)  calls f = o.K[a], with receiver o
OP_INVOKESUPER,//    A B         o v_b..v_1   f(v_b..v_1)  calls f = o.K[a], with receiver o 
OP_GETSUPER,//       U

OP_JUMP,//           S           -            -            pc += S
OP_JUMPFALSEPOP,//   S           v            -            pc += S
OP_JUMPFALSE,//      S           v            v            if !v, then pc += S
OP_JUMPNULL,//       S           v            v            if v == null, then pc += S

OP_GLOBAL,//         U           v            -            define G[K[u]] = v
OP_GETGLOBAL,//      U           -            G[K[U]]      -
OP_SETGLOBAL,//      U           v            -            G[K[u]] = v
OP_GETLOCAL,//       U           -            L[U]         -
OP_SETLOCAL,//       U           v            -            L[u] = v
OP_UPVALUE,//        U           v            -            define Up[u] = v
OP_GETUPVALUE,//     U           -            Up[U]        -
OP_SETUPVALUE,//     U           v            -            Up[u] = v

OP_NEWCLASS,//       A B         -            v            v = new class named K[a]
OP_INHERIT,//        -           x y          x            x subclass of y
OP_NEWMETHOD,//      U           v f          v            v.K[u] = f
OP_NEWARRAY,//       U           v_u..v_1     [v_u..v_1]   -
OP_NEWMAP,//         U           v_2n..v_1    {v_2n..v_1}  -

OP_FORNUM0,//        S           *~*~*~*~*~*~*~*~* see notes *~*~*~*~*~*~*~*~*
OP_FORNUM,//         S           *~*~*~*~*~*~*~*~* see notes *~*~*~*~*~*~*~*~*
OP_FORIN0,//         S           *~*~*~*~*~*~*~*~* see notes *~*~*~*~*~*~*~*~*
OP_FORIN,//          S           *~*~*~*~*~*~*~*~* see notes *~*~*~*~*~*~*~*~*

OP_VARARG,//         U           v_u..v_1     [v_u..v_1]   -
OP_CALL,//           U           f v_u..v_1   v            v = f(v_u..v_1)

OP_GETATTR,//        -           x y          x.y          -
OP_SETATTR,//        -           x y z        -            x.y=z
OP_GETITEM,//        -           x y          x[y]         -
OP_SETITEM,//        -           x y z        -            x[y]=z
OP_GETSLICE,//       -           x y z        v            -  
OP_SETSLICE,//       -           x y z w      -            x[y:z]=w

OP_EQ,//             -           x y          x==y         -
OP_LT,//             -           x y          x<y          -
OP_LE,//             -           x y          x<=y         -
OP_GT,//             -           x y          x>y          -
OP_GE,//             -           x y          x>=y         - 
OP_IN,//             -           x y          x in y       -

OP_LEN,//            -           x            #x           -
OP_NEG,//            -           x            -x           -
OP_NOT,//            -           x            !x           -
OP_BNOT,//           -           x            ~x           - 

OP_ADD,//            -           x y          x+y          -
OP_SUB,//            -           x y          x-y          - 
OP_MUL,//            -           x y          x*y          - 
OP_DIV,//            -           x y          x/y          - 
OP_IDIV,//           -           x y          x//y         - 
OP_MOD,//            -           x y          x%y          - 
OP_POW,//            -           x y          x**y         - 
OP_CONCAT,//         -           x y          x++y         - 
OP_BXOR,//           -           x y          x^y          - 
OP_BAND,//           -           x y          x&y          - 
OP_BOR,//            -           x y          x|y          - 
OP_SHL,//            -           x y          x<<y         - 
OP_SHR,//            -           x y          x>>y         - 

NOPCODES
} Op;
// clang-format on
//
// Notes:
// * OP_RETURN replaces the current call frame with the value on top of the stack.
//   The current call frame consists of the function object or reciever 'f', its
//   parameters, and all locals declared between the start of the call and the 'return'.
// * OP_FOR*0 prepare a for loop. The loop body is skipped if the condition is false
//   For OP_FORNUM0, the loop 'begin' is compared against the loop 'end' using the
//   sign of the loop 'step'. For OP_FORIN0, the loop is skipped if the container is
//   empty. Both instructions will push the loop control variable.
// * OP_FOR* run a single for-loop step.

#define META1 OP_CALL
#define META2 OP_ADD
#define METAR MM_RADD
#define NMETA (METATOP - META1)
#define op2meta(op) ((op) - META1)

// Extra symbols for 'reverse' metamethods, and metamethods that don't correspond
// to an opcode.
enum {
    MM_RADD = NOPCODES,
    MM_RSUB,
    MM_RMUL,
    MM_RDIV,
    MM_RIDIV,
    MM_RMOD,
    MM_RPOW,
    MM_RCONCAT,
    MM_RBXOR,
    MM_RBAND,
    MM_RBOR,
    MM_RSHL,
    MM_RSHR,

    MM_NULL,
    MM_STR,
    MM_INT,
    MM_FLOAT,
    MM_BOOL,
    MM_ARRAY,
    MM_MAP,

    METATOP
};

#endif // PAW_OPCODE_H
