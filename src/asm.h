// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_ASM_H
#define PAW_ASM_H

#include "compile.h"

typedef struct AsmRegister {
    int id;
} AsmRegister;

typedef struct AsmJumpId {
    int id;
} AsmJumpId;

#define ASM_OP_LIST(X) \
    X(Move)            \
    X(Upvalue)         \
    X(Global)          \
    X(SetUpvalue)      \
    X(Constant)        \
    X(Aggregate)       \
    X(Container)       \
    X(Call)            \
    X(Cast)            \
    X(Closure)         \
    X(GetElement)      \
    X(SetElement)      \
    X(GetField)        \
    X(SetField)        \
    X(UnaryOp)         \
    X(BinaryOp)        \
    X(Return)          \
    X(JumpCond)        \
    X(Jump)

enum AsmOpKind {
#define DEFINE_ENUM(X) kAsm##X,
    ASM_OP_LIST(DEFINE_ENUM)
#undef DEFINE_ENUM
};

#define ASM_OP_HEADER enum AsmOpKind kind : 8

struct AsmOpHeader {
    ASM_OP_HEADER;
};

struct AsmMove {
    ASM_OP_HEADER;
    AsmRegister result;
    AsmRegister target;
};

struct AsmUpvalue {
    ASM_OP_HEADER;
    AsmRegister result;
    int upvalue_id;
};

struct AsmGlobal {
    ASM_OP_HEADER;
    AsmRegister result;
    int global_id;
};

struct AsmSetUpvalue {
    ASM_OP_HEADER;
    AsmRegister target;
    int upvalue_id;
};

struct AsmConstant {
    ASM_OP_HEADER;
    AsmRegister result;
    Value k;
};

struct AsmAggregate {
    ASM_OP_HEADER;
    AsmRegister result;
    int nfields;
};

struct AsmContainer {
    ASM_OP_HEADER;
    AsmRegister result;
    paw_Type code;
    int nelems;
};

struct AsmCall {
    ASM_OP_HEADER;
    // NOTE: return value is placed in ".target"
    AsmRegister target;
    int nargs;
};

enum AsmCastKind {
    // casts that require CPU instructions, everything else is a NOOP at runtime
    ASM_CAST_ANY2BOOL,
    ASM_CAST_INT2FLOAT,
    ASM_CAST_FLOAT2INT,
};

struct AsmCast {
    ASM_OP_HEADER;
    enum AsmCastKind cast_kind : 8;
    AsmRegister result;
    AsmRegister target;
};

struct AsmClosure {
    ASM_OP_HEADER;
    AsmRegister result;
};

struct AsmGetElement {
    ASM_OP_HEADER;
    AsmRegister result;
    AsmRegister target;
    AsmRegister index;
};

struct AsmSetElement {
    ASM_OP_HEADER;
    AsmRegister target;
    AsmRegister index;
    AsmRegister value;
};

struct AsmGetField {
    ASM_OP_HEADER;
    AsmRegister result;
    AsmRegister target;
    int index;
};

struct AsmSetField {
    ASM_OP_HEADER;
    AsmRegister target;
    int index;
    AsmRegister value;
};

struct AsmUnaryOp {
    ASM_OP_HEADER;
    enum UnaryOp op : 8;
    AsmRegister result;
    AsmRegister value;
};

struct AsmBinaryOp {
    ASM_OP_HEADER;
    enum BinaryOp op : 8;
    AsmRegister result;
    AsmRegister left;
    AsmRegister right;
};

struct AsmReturn {
    ASM_OP_HEADER;
};

struct AsmJump {
    ASM_OP_HEADER;
    int jump;
};

struct AsmJumpCond {
    ASM_OP_HEADER;
    paw_Bool test : 1;
    AsmRegister cond;
    int jump;
};

struct AsmOp {
    union {
        struct AsmOpHeader hdr;
#define DEFINE_OP(X) struct Asm##X X##_;
        ASM_OP_LIST(DEFINE_OP)
#undef DEFINE_ENUM
    };
};

#define ASM_KINDOF(op) (op)->hdr.kind

#define DEFINE_ACCESSORS(X)                                  \
    static inline paw_Bool AsmIs##X(struct AsmOp const *op)  \
    {                                                        \
        return op->hdr.kind == kAsm##X;                      \
    }                                                        \
    static inline struct Asm##X *AsmGet##X(struct AsmOp *op) \
    {                                                        \
        paw_assert(AsmIs##X(op));                            \
        return &op->X##_;                                    \
    }
ASM_OP_LIST(DEFINE_ACCESSORS)
#undef DEFINE_ACCESSORS

static char const *kAsmOpNames[] = {
#define DEFINE_NAME(X) "Asm" #X,
    ASM_OP_LIST(DEFINE_NAME)
#undef DEFINE_NAME
};

DEFINE_LIST(struct Compiler, pawP_asm_list_, AsmList, struct AsmOp)

struct AsmList *pawP_lower_mir(struct Compiler *C, struct Mir *mir);
char const *pawP_dump_asm(struct Compiler *C, struct AsmList *ops);

#endif // PAW_ASM_H
