// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_OPCODE_H
#define PAW_OPCODE_H

#include "paw.h"

#define NREGISTERS (LOCAL_MAX + 1)

#ifndef UPVALUE_MAX
#define UPVALUE_MAX 64
#endif

#ifndef LOCAL_MAX
#define LOCAL_MAX 250
#endif

#ifndef FIELD_MAX
#define FIELD_MAX 250
#endif

#ifndef PARAM_MAX
#define PARAM_MAX 100
#endif

#ifndef CONSTANT_MAX
#define CONSTANT_MAX A_MAX
#endif

#ifndef ITEM_MAX
#define ITEM_MAX Bx_MAX
#endif

#ifndef JUMP_MAX
#define JUMP_MAX sBx_MAX
#endif

// 32-bit opcodes, format modified from Lua:
//
// +-----------------------+
// |  B  |  C  |  A  | Op  |
// +-----+-----+-----+-----+
// |     Bx    |  A  | Op  |
// +-----------+-----+-----+
//  MSB                 LSB
#define OP_WIDTH 8
#define Bx_WIDTH 16
#define A_WIDTH 8
#define B_WIDTH 8
#define C_WIDTH 8

#define OP_OFFSET 0
#define Bx_OFFSET (OP_WIDTH + A_WIDTH)
#define A_OFFSET OP_WIDTH
#define B_OFFSET (OP_WIDTH + A_WIDTH + C_WIDTH)
#define C_OFFSET (OP_WIDTH + A_WIDTH)

#define OP_MAX ((1 << OP_WIDTH) - 1)
#define Bx_MAX ((1 << Bx_WIDTH) - 1)
#define sBx_MAX (Bx_MAX >> 1)
#define A_MAX ((1 << A_WIDTH) - 1)
#define B_MAX ((1 << B_WIDTH) - 1)
#define C_MAX ((1 << C_WIDTH) - 1)

#define MASK1(n, p) ((~((~(OpCode)0) << n)) << p)
#define MASK0(n, p) (~MASK1(n, p))

#define GET_OP(v) (Op)((v) & MASK1(OP_WIDTH, 0))
#define SET_OP(v, o) (*(v) = (*(v) & MASK0(OP_WIDTH, 0)) | (OpCode)(o))
#define GET_A(v) (((v) >> A_OFFSET) & MASK1(A_WIDTH, 0))
#define SET_A(v, a) \
    (*(v) = (*(v) & MASK0(A_WIDTH, A_OFFSET)) | ((OpCode)(a) << A_OFFSET))
#define GET_B(v) (((v) >> B_OFFSET) & MASK1(B_WIDTH, 0))
#define SET_B(v, b) \
    (*(v) = (*(v) & MASK0(B_WIDTH, B_OFFSET)) | ((OpCode)(b) << B_OFFSET))
#define GET_C(v) (((v) >> C_OFFSET) & MASK1(C_WIDTH, 0))
#define SET_C(v, b) \
    (*(v) = (*(v) & MASK0(C_WIDTH, C_OFFSET)) | ((OpCode)(b) << C_OFFSET))
#define GET_Bx(v) (((v) >> Bx_OFFSET) & MASK1(Bx_WIDTH, 0))
#define SET_Bx(v, u) \
    (*(v) = (*(v) & MASK0(Bx_WIDTH, Bx_OFFSET)) | ((OpCode)(u) << Bx_OFFSET))
#define GET_sBx(v) ((int)GET_Bx(v) - sBx_MAX)
#define SET_sBx(v, s) SET_Bx(v, (int)(s) + sBx_MAX)

#define CREATE_ABC(o, a, b, c) ((CAST(OpCode, o) << OP_OFFSET) | (CAST(OpCode, a) << A_OFFSET) | (CAST(OpCode, b) << B_OFFSET) | (CAST(OpCode, c) << C_OFFSET))

#define CREATE_ABx(o, a, bc) ((CAST(OpCode, o) << OP_OFFSET) | (CAST(OpCode, a) << A_OFFSET) | (CAST(OpCode, bc) << Bx_OFFSET))

typedef uint32_t OpCode;

// Opcode format: Each instruction is packed into a 32-bit unsigned integer (OpCode)
//
// legend:
//   G = global functions
//   K = constants
//   R = local variables
//   Up = upvalues
//   P = function prototypes
//
// ORDER Op
typedef enum Op { //      arguments    description
    //                   -------------------------------------------------------------------------------
    OP_LOADSMI, //        A sBx        R[A] := sBx
    OP_LOADK, //          A Bx         R[A] := K[Bx]

    OP_NOOP, //           -            -
    OP_MOVE, //           A B          R[A] := R[B]
    OP_CLOSE, //          A            close(A)
    OP_RETURN, //         A B          return R[A]..R[A+B]
    OP_CLOSURE, //        A Bx         R[A] := closure(P[Bx], R[A]..R[A+n])
    OP_CALL, //           A B          R[A] := R[A](R[A+1]..R[A+B+1])

    OP_JUMP, //           sBx          pc += sBx
    OP_JUMPT, //          A sBx        if (R[A]) pc += sBx
    OP_JUMPF, //          A sBx        if (!R[A]) pc += sBx

    OP_GETGLOBAL, //      A Bx         R[A] := G[Bx]
    OP_GETUPVALUE, //     A B          R[A] := Up[B]
    OP_SETUPVALUE, //     A B          Up[A] := R[B]

    OP_NEWTUPLE, //       A B          R[A] := (e1...eB)
    OP_NEWLIST, //        A B          R[A] := [e1...eB]
    OP_NEWMAP, //         A B          R[A] := [k1:v1...kB:vB]

    OP_TESTK, //          A B C        if (R[A] != K[B]) pc++
    OP_SWITCHINT, //      A B          if (R[A] != B) pc++

    OP_CEQ, //            A B C        R[A] := R[B] == R[C]
    OP_CNE, //            A B C        R[A] := R[B] != R[C]
    OP_CLT, //            A B C        R[A] := R[B] < R[C]
    OP_CLE, //            A B C        R[A] := R[B] <= R[C]

    OP_IEQ, //            A B C        R[A] := R[B] == R[C]
    OP_INE, //            A B C        R[A] := R[B] != R[C]
    OP_ILT, //            A B C        R[A] := R[B] < R[C]
    OP_ILE, //            A B C        R[A] := R[B] <= R[C]
    OP_INOT, //           A B          R[A] := !R[B]
    OP_INEG, //           A B          R[A] := -R[B]
    OP_IADD, //           A B C        R[A] := R[B] + R[C]
    OP_ISUB, //           A B C        R[A] := R[B] - R[C]
    OP_IMUL, //           A B C        R[A] := R[B] * R[C]
    OP_IDIV, //           A B C        R[A] := R[B] / R[C]
    OP_IMOD, //           A B C        R[A] := R[B] % R[C]
    OP_BITNOT, //         A B          R[A] := ~R[B]
    OP_BITAND, //         A B C        R[A] := R[B] & R[C]
    OP_BITOR, //          A B C        R[A] := R[B] | R[C]
    OP_BITXOR, //         A B C        R[A] := R[B] ^ R[C]
    OP_SHL, //            A B C        R[A] := R[B] << R[C]
    OP_SHR, //            A B C        R[A] := R[B] >> R[C]

    OP_FEQ, //            A B C        R[A] := R[B] == R[C]
    OP_FNE, //            A B C        R[A] := R[B] != R[C]
    OP_FLT, //            A B C        R[A] := R[B] < R[C]
    OP_FLE, //            A B C        R[A] := R[B] >= R[C]
    OP_FNEG, //           A B          R[A] := -R[B]
    OP_FADD, //           A B C        R[A] := R[B] + R[C]
    OP_FSUB, //           A B C        R[A] := R[B] - R[C]
    OP_FMUL, //           A B C        R[A] := R[B] * R[C]
    OP_FDIV, //           A B C        R[A] := R[B] / R[C]
    OP_FMOD, //           A B C        R[A] := R[B] % R[C]

    OP_STREQ, //          A B C        R[A] := R[B] == R[C]
    OP_STRNE, //          A B C        R[A] := R[B] != R[C]
    OP_STRLT, //          A B C        R[A] := R[B] < R[C]
    OP_STRLE, //          A B C        R[A] := R[B] <= R[C]
    OP_STRLEN, //         A B          R[A] := #R[B]
    OP_STRCAT, //         A B C        R[A] := R[B] + R[C]
    OP_STRGET, //         A B C        R[A] := R[B][R[C]]
    OP_STRGETN, //        A B C        R[A] := R[B][R[C]:R[C+1]]

    OP_LISTLEN, //        A B          R[A] := #R[B]
    OP_LISTCAT, //        A B C        R[A] := R[B] + R[C]
    OP_LISTGETP, //       A B C        R[A] := &R[B][R[C]]
    OP_LISTGET, //        A B C        R[A] := R[B][R[C]]
    OP_LISTSET, //        A B C        R[A][R[B]] := R[C]
    OP_LISTGETN, //       A B C        R[A] := R[B][R[C]:R[C+1]]
    OP_LISTSETN, //       A B C        R[A][R[B]:R[B+1]] := R[C]

    OP_MAPLEN, //         A B          R[A] := #R[B]
    OP_MAPGETP, //        A B C        R[A] := &R[B][R[C]]
    OP_MAPNEWP, //        A B C        R[A] := &R[B][R[C]]
    OP_MAPGET, //         A B C        R[A] := R[B][R[C]]
    OP_MAPSET, //         A B C        R[A][R[B]] := R[C]

    OP_UNPACK,//          A B C        R[A..A+C] := R[A][B..B+C]
    OP_GETVALUE,//        A B C        R[A] := R[B][C]
    OP_SETVALUE,//        A B C        R[A][B] := R[C]

    OP_GETFIELD, //       A B C        R[A] := R[B][C]
    OP_SETFIELD, //       A B C        R[A][B] := R[C]
    OP_GETDISCR, //       A B          R[A] := discr(R[B])

    OP_BCASTF, //         A B          R[A] := R[B] as float
    OP_CCASTI, //         A B          R[A] := R[B] as int
    OP_ICASTB, //         A B          R[A] := R[B] as bool
    OP_ICASTC, //         A B          R[A] := R[B] as char
    OP_ICASTF, //         A B          R[A] := R[B] as float
    OP_FCASTB, //         A B          R[A] := R[B] as bool
    OP_FCASTI, //         A B          R[A] := R[B] as int

    NOPCODES
} Op;

_Static_assert(UPVALUE_MAX <= A_MAX, "UPVALUE_MAX is too large");
_Static_assert(LOCAL_MAX <= A_MAX, "LOCAL_MAX is too large");
_Static_assert(FIELD_MAX <= B_MAX, "FIELD_MAX is too large");
_Static_assert(PARAM_MAX <= B_MAX, "PARAM_MAX is too large");
_Static_assert(CONSTANT_MAX <= Bx_MAX, "CONSTANT_MAX is too large");
_Static_assert(ITEM_MAX <= Bx_MAX, "ITEM_MAX is too large");
_Static_assert(JUMP_MAX <= sBx_MAX, "JUMP_MAX is too large");
_Static_assert(NOPCODES <= OP_MAX, "too many opcodes");

// sanity check opcode format
_Static_assert(OP_WIDTH + A_WIDTH + B_WIDTH + C_WIDTH == sizeof(OpCode) * 8, "invalid opcode format");
_Static_assert(B_WIDTH + C_WIDTH == Bx_WIDTH, "invalid opcode format");
_Static_assert(OP_OFFSET + OP_WIDTH == A_OFFSET, "invalid opcode format");
_Static_assert(A_OFFSET + A_WIDTH == C_OFFSET, "invalid opcode format");
_Static_assert(C_OFFSET + C_WIDTH == B_OFFSET, "invalid opcode format");
_Static_assert(B_OFFSET + B_WIDTH == sizeof(OpCode) * 8, "invalid opcode format");
_Static_assert(Bx_OFFSET + Bx_WIDTH == sizeof(OpCode) * 8, "invalid opcode format");

#endif // PAW_OPCODE_H
