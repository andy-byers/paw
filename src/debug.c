// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "debug.h"
#include "alloc.h"
#include "auxlib.h"
#include "call.h"
#include "compile.h"
#include "map.h"
#include "rt.h"

#include <inttypes.h>
#include <stdio.h>

#define PC_REL(p, pc) CAST(int, (pc) - (p)->source - 1)

int pawD_line_number(CallFrame const *cf, OpCode const *pc)
{
    if (!CF_IS_PAW(cf))
        return -1;

    int i = 0;
    Proto *p = cf->fn->p;
    int const r = PC_REL(p, pc);
    for (; i < p->nlines - 1; ++i) {
        if (p->lines[i].pc >= r)
            break;
    }
    return p->lines[i].line;
}

void pawD_debug_log(paw_Env *P, char const *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    vfprintf(stderr, fmt, arg);
    va_end(arg);

    fputc('\n', stderr);
    fflush(stderr);
}

char const *paw_unop_symbol(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return "#";
        case UNARY_NEG:
            return "-";
        case UNARY_NOT:
            return "!";
        case UNARY_BNOT:
            return "~";
    }
}

char const *paw_binop_symbol(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_AS:
            return "as";
        case BINARY_ADD:
            return "+";
        case BINARY_SUB:
            return "-";
        case BINARY_MUL:
            return "*";
        case BINARY_DIV:
            return "/";
        case BINARY_MOD:
            return "%";
        case BINARY_BXOR:
            return "^";
        case BINARY_BAND:
            return "&";
        case BINARY_BOR:
            return "|";
        case BINARY_SHL:
            return "<<";
        case BINARY_SHR:
            return ">>";
        case BINARY_EQ:
            return "==";
        case BINARY_NE:
            return "!=";
        case BINARY_LT:
            return "<";
        case BINARY_LE:
            return "<=";
        case BINARY_GT:
            return ">";
        case BINARY_GE:
            return ">=";
        case BINARY_CONCAT:
            return "++";
    }
}

