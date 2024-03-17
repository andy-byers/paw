// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_OPCODE_H
#define PAW_OPCODE_H

#include "paw.h"

typedef uint8_t OpCode;

#define UPVALUE_MAX ((int)UINT16_MAX / 2)
#define UPVALUE_LOCAL ((int)~(UINT16_MAX >> 1))

#define JUMP_MAX (int)INT16_MAX
#define decode_jump(x) ((int)(x)-JUMP_MAX)
#define encode_jump_over(x) ((x) + JUMP_MAX)
#define encode_jump_back(x) (JUMP_MAX - (x))

#define NMETA (NOPCODES - MM_FIRST)

// clang-format off
//
// Opcode format: Instructions are represented by a single byte followed
// by 0 or more immediate operands. 
//
// The MM_* in the enumerator below are used for metamethods (see meta.c 
// for a list of metamethod names that must be kept in the same order as 
// the corresponding opcodes). 
//
// legend:
//   K = constants (indexed by uint16_t)
//   P = function prototypes (indexed by uint16_t)
//
typedef enum Op { // operands    stack in    stack out    side effects
OP_PUSHNULL, //      -           -           null         -
OP_PUSHTRUE, //      -           -           true         -
OP_PUSHFALSE, //     -           -           false        -
OP_PUSHCONST, //     k           -           K[k]         -

OP_POP, //           -           v           -            -
OP_CLOSE, //         -           v           -            v closed over if captured, else popped
OP_RETURN, //        

OP_CLOSURE, //       
OP_INVOKE,
OP_INVOKESUPER,
OP_GETSUPER,
OP_INHERIT,

OP_JUMP,
OP_JUMPFALSE,
OP_JUMPNULL,

OP_GLOBAL, // A
OP_GETGLOBAL, // Iw
OP_SETGLOBAL, // Iw A
OP_GETLOCAL, // Iw
OP_SETLOCAL, // Iw A
OP_UPVALUE, // Iw A
OP_GETUPVALUE, // Iw
OP_SETUPVALUE, // Iw A

OP_NEWCLASS,
OP_NEWMETHOD,
OP_NEWARRAY,
OP_NEWMAP,

OP_VARARG,      
OP_UNPACK,     // TODO 
OP_UNPACKEX,   // TODO   
OP_SPREAD,     // TODO 

OP_FORNUM0, 
OP_FORNUM,      
OP_FORIN0,  
OP_FORIN,       

MM_FIRST, 
         
OP_LEN = MM_FIRST,
OP_NEG,
OP_NOT,
OP_BNOT,

OP_NULL,
OP_STR,
OP_INT,
OP_FLOAT,
OP_BOOL,
OP_ARRAY,
OP_MAP,

OP_CALL,
OP_ITER,
OP_NEXT,

OP_GETATTR,
OP_SETATTR,
OP_GETITEM,
OP_SETITEM,

OP_EQ,
OP_LT,
OP_LE,
OP_IN,          

MM_HASR,          //     Marks first op with a '__r*' metamethod (must be OP_ADD)
OP_ADD = MM_HASR, // A B
OP_SUB,           // A B
OP_MUL,           // A B 
OP_DIV,           // A B 
OP_IDIV,          // A B 
OP_MOD,           // A B 
OP_POW,           // A B 
OP_CONCAT,        // A B 
OP_BXOR,          // A B 
OP_BAND,          // A B 
OP_BOR,           // A B 
OP_SHL,           // A B 
OP_SHR,           // A B    

MM_FIRSTR, 

OP_RADD = MM_FIRSTR,
OP_RSUB,
OP_RMUL,
OP_RDIV,
OP_RIDIV,
OP_RMOD,
OP_RPOW,
OP_RCONCAT,
OP_RBXOR,
OP_RBAND,
OP_RBOR,
OP_RSHL,
OP_RSHR,    

NOPCODES
} Op;
// clang-format on

const char *paw_opcode_name(Op op);

#endif // PAW_OPCODE_H
