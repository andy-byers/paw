
// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "asm.h"

struct Printer {
    struct Compiler *C;
    Buffer *buf;
    paw_Env *P;
    int indent;
};

#define PRINT_LITERAL(P, lit) L_ADD_LITERAL(ENV(P), (P)->buf, lit)
#define PRINT_STRING(P, str) pawL_add_nstring(ENV(P), (P)->buf, (str)->text, (str)->length)
#define PRINT_FORMAT(P, ...) pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__)
#define PRINT_CHAR(P, c) pawL_add_char(ENV(P), (P)->buf, c)

static void dump_op(struct Printer *, struct AsmOp *);

static void indentation(struct Printer *P)
{
    for (int i = 0; i < P->indent; ++i) {
        PRINT_LITERAL(P, "  ");
    }
}

#define DUMP_FMT(P, ...) (indentation(P), pawL_add_fstring(ENV(P), (P)->buf, __VA_ARGS__))
#define DUMP_MSG(P, msg) (indentation(P), pawL_add_string(ENV(P), (P)->buf, msg))

static void dump_instruction_list(struct Printer *P, struct AsmList *list)
{
    for (int i = 0; i < list->count; ++i) {
        dump_op(P, &K_LIST_GET(list, i));
    }
}

static void dump_register(struct Printer *P, char const *name, struct AsmRegister r)
{
    DUMP_FMT(P, "%s: r%d (%s)\n", name, r.id);
}

static void dump_op(struct Printer *P, struct AsmOp *op)
{
    const enum AsmOpKind k = ASM_KINDOF(op);
    DUMP_MSG(P, kAsmOpNames[k]);
    PRINT_LITERAL(P, " {\n");
    ++P->indent;

    switch (k) {
        case kAsmMove: {
            struct AsmMove *o = AsmGetMove(op);
            dump_register(P, "result", o->result);
            dump_register(P, "target", o->target);
            break;
        }
        case kAsmUpvalue: {
            struct AsmUpvalue *o = AsmGetUpvalue(op);
            dump_register(P, "result", o->result);
            PRINT_FORMAT(P, "upvalue_id: %d\n", o->upvalue_id);
            break;
        }
        case kAsmGlobal: {
            struct AsmGlobal *o = AsmGetGlobal(op);
            dump_register(P, "result", o->result);
            PRINT_FORMAT(P, "global_id: %d\n", o->global_id);
            break;
        }
        case kAsmSetUpvalue: {
            struct AsmSetUpvalue *o = AsmGetSetUpvalue(op);
            dump_register(P, "target", o->target);
            PRINT_FORMAT(P, "upvalue_id: %d\n", o->upvalue_id);
            break;
        }
        case kAsmConstant: {
            struct AsmConstant *o = AsmGetConstant(op);
            dump_register(P, "result", o->result);
            PRINT_FORMAT(P, "value: %llu\n", o->k.u);
            break;
        }
        case kAsmAggregate: {
            struct AsmAggregate *o = AsmGetAggregate(op);
            dump_register(P, "result", o->result);
            PRINT_FORMAT(P, "nfields: %d\n", o->nfields);
            break;
        }
        case kAsmContainer: {
            struct AsmContainer *o = AsmGetContainer(op);
            dump_register(P, "result", o->result);
            PRINT_FORMAT(P, "code: %d\n", o->code);
            break;
        }
        case kAsmCall: {
            struct AsmCall *o = AsmGetCall(op);
            dump_register(P, "target", o->target);
            PRINT_FORMAT(P, "nargs: %d\n", o->nargs);
            break;
        }
        case kAsmCast: {
            struct AsmCast *o = AsmGetCast(op);
            dump_register(P, "result", o->result);
            dump_register(P, "target", o->target);
            PRINT_FORMAT(P, "cast_kind: %d\n", o->cast_kind);
            break;
        }
        case kAsmClosure: {
            struct AsmClosure *o = AsmGetClosure(op);
            dump_register(P, "result", o->result);
            break;
        }
        case kAsmGetElement: {
            struct AsmGetElement *o = AsmGetGetElement(op);
            dump_register(P, "object", o->result);
            dump_register(P, "target", o->target);
            dump_register(P, "index", o->index);
            break;
        }
        case kAsmSetElement: {
            struct AsmSetElement *o = AsmGetSetElement(op);
            dump_register(P, "target", o->target);
            dump_register(P, "index", o->index);
            dump_register(P, "value", o->value);
            break;
        }
        case kAsmGetField: {
            struct AsmGetField *o = AsmGetGetField(op);
            dump_register(P, "result", o->result);
            dump_register(P, "target", o->target);
            PRINT_FORMAT(P, "index: %d\n", o->index);
            break;
        }
        case kAsmSetField: {
            struct AsmSetField *o = AsmGetSetField(op);
            dump_register(P, "target", o->target);
            PRINT_FORMAT(P, "index: %d\n", o->index);
            dump_register(P, "value", o->value);
            break;
        }
        case kAsmUnaryOp: {
            struct AsmUnaryOp *o = AsmGetUnaryOp(op);
            PRINT_FORMAT(P, "op: %d\n", paw_unop_name(o->op));
            dump_register(P, "value", o->value);
            dump_register(P, "result", o->result);
            break;
        }
        case kAsmBinaryOp: {
            struct AsmBinaryOp *o = AsmGetBinaryOp(op);
            PRINT_FORMAT(P, "op: %d\n", paw_binop_name(o->op));
            dump_register(P, "left", o->left);
            dump_register(P, "right", o->right);
            dump_register(P, "result", o->result);
            break;
        }
        case kAsmReturn: {
            struct AsmReturn *o = AsmGetReturn(op);
            break;
        }
        case kAsmJumpCond: {
            struct AsmJumpCond *o = AsmGetJumpCond(op);
            PRINT_FORMAT(P, "jump: %d\n", o->jump);
            PRINT_FORMAT(P, "test: %d\n", o->test);
            dump_register(P, "cond", o->cond);
            break;
        }
        case kAsmJump: {
            struct AsmJump *o = AsmGetJump(op);
            PRINT_FORMAT(P, "jump: %d\n", o->jump);
            break;
        }
    }
    PRINT_LITERAL(P, "}\n");
    ++P->indent;
}

char const *pawP_dump_asm(struct Compiler *C, struct AsmList *ops)
{
    Buffer buf;
    paw_Env *P = ENV(C);
    pawL_init_buffer(P, &buf);

    for (int i = 0; i < ops->count; ++i) {
        struct AsmOp op = K_LIST_GET(ops, i);
        dump_op(&(struct Printer){
                    .P = ENV(C),
                    .buf = &buf,
                    .C = C,
                },
            &op);
    }

    pawL_push_result(P, &buf);
    return paw_string(P, -1);
}
