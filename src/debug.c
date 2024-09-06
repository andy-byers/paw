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

#define PC_REL(p, pc) CAST(int, (pc) - (p)->source - 1)

int pawD_line_number(const CallFrame *cf, const OpCode *pc)
{
    if (!CF_IS_PAW(cf)) return -1;

    int i = 0;
    Proto *p = cf->fn->p;
    const int r = PC_REL(p, pc);
    for (; i < p->nlines - 1; ++i) {
        if (p->lines[i].pc >= r) break;
    }
    return p->lines[i].line;
}

// TODO: Most of this should not be in the core: use hooks for debugging
#if defined(PAW_DEBUG_EXTRA)

#include <stdio.h>

const char *paw_unop_name(enum UnaryOp unop)
{
    switch (unop) {
        case UNARY_LEN:
            return "LEN";
        case UNARY_NEG:
            return "NEG";
        case UNARY_NOT:
            return "NOT";
        case UNARY_BNOT:
            return "BNOT";
        default:
            return "?";
    }
}

const char *paw_binop_name(enum BinaryOp binop)
{
    switch (binop) {
        case BINARY_ADD:
            return "ADD";
        case BINARY_SUB:
            return "SUB";
        case BINARY_MUL:
            return "MUL";
        case BINARY_DIV:
            return "DIV";
        case BINARY_MOD:
            return "MOD";
        case BINARY_BXOR:
            return "BXOR";
        case BINARY_BAND:
            return "BAND";
        case BINARY_BOR:
            return "BOR";
        case BINARY_SHL:
            return "SHL";
        case BINARY_SHR:
            return "SHR";
        case BINARY_EQ:
            return "EQ";
        case BINARY_NE:
            return "NE";
        case BINARY_LT:
            return "LT";
        case BINARY_LE:
            return "LE";
        case BINARY_GT:
            return "GT";
        case BINARY_GE:
            return "GE";
        default:
            return "?";
    }
}

const char *paw_op_name(Op op)
{
    switch (op) {
        case OP_CASTBOOL:
            return "CASTBOOL";
        case OP_CASTINT:
            return "CASTINT";
        case OP_CASTFLOAT:
            return "CASTFLOAT";
        case OP_PUSHZERO:
            return "PUSHZERO";
        case OP_PUSHONE:
            return "PUSHONE";
        case OP_PUSHSMI:
            return "PUSHSMI";
        case OP_PUSHCONST:
            return "PUSHCONST";
        case OP_COPY:
            return "COPY";
        case OP_INITFIELD:
            return "INITFIELD";
        case OP_SHIFT:
            return "SHIFT";
        case OP_POP:
            return "POP";
        case OP_CLOSE:
            return "CLOSE";
        case OP_RETURN:
            return "RETURN";
        case OP_CLOSURE:
            return "CLOSURE";
        case OP_CALL:
            return "CALL";
        case OP_JUMP:
            return "JUMP";
        case OP_JUMPFALSE:
            return "JUMPFALSE";
        case OP_JUMPFALSEPOP:
            return "JUMPFALSEPOP";
        case OP_JUMPNULL:
            return "JUMPNULL";
        case OP_GETGLOBAL:
            return "GETGLOBAL";
        case OP_GETLOCAL:
            return "GETLOCAL";
        case OP_SETLOCAL:
            return "SETLOCAL";
        case OP_GETUPVALUE:
            return "GETUPVALUE";
        case OP_SETUPVALUE:
            return "SETUPVALUE";
        case OP_NEWTUPLE:
            return "NEWTUPLE";
        case OP_NEWVARIANT:
            return "NEWVARIANT";
        case OP_NEWINSTANCE:
            return "NEWINSTANCE";
        case OP_NEWLIST:
            return "NEWLIST";
        case OP_NEWMAP:
            return "NEWMAP";
        case OP_FORNUM0:
            return "FORNUM0";
        case OP_FORNUM:
            return "FORNUM";
        case OP_FORLIST0:
            return "FORLIST0";
        case OP_FORLIST:
            return "FORLIST";
        case OP_FORMAP0:
            return "FORMAP0";
        case OP_FORMAP:
            return "FORMAP";
        case OP_ARITHI1:
            return "ARITHI1";
        case OP_ARITHI2:
            return "ARITHI2";
        case OP_ARITHF1:
            return "ARITHF1";
        case OP_ARITHF2:
            return "ARITHF2";
        case OP_BITW1:
            return "BITW1";
        case OP_BITW2:
            return "BITW2";
        case OP_CMPI:
            return "CMPI";
        case OP_CMPF:
            return "CMPF";
        case OP_CMPS:
            return "CMPS";
        case OP_LENGTH:
            return "LENGTH";
        case OP_GETELEM:
            return "GETELEM";
        case OP_SETELEM:
            return "SETELEM";
        case OP_GETRANGE:
            return "GETRANGE";
        case OP_SETRANGE:
            return "SETRANGE";
        case OP_GETTUPLE:
            return "GETTUPLE";
        case OP_GETFIELD:
            return "GETFIELD";
        case OP_SETTUPLE:
            return "SETTUPLE";
        case OP_SETFIELD:
            return "SETFIELD";
        default:
            return "???";
    }
}

void paw_dump_opcode(OpCode opcode)
{
    const char *opname = paw_op_name(GET_OP(opcode));
    switch (GET_OP(opcode)) {
        case OP_JUMP:
        case OP_JUMPFALSE:
        case OP_JUMPFALSEPOP:
        case OP_JUMPNULL:
            printf("%s %d\n", opname, GET_S(opcode));
            break;
        case OP_PUSHCONST:
        case OP_POP:
        case OP_CALL:
        case OP_GETGLOBAL:
        case OP_GETLOCAL:
        case OP_SETLOCAL:
        case OP_GETUPVALUE:
        case OP_SETUPVALUE:
        case OP_ARITHI1:
        case OP_ARITHI2:
        case OP_BITW1:
        case OP_BITW2:
        case OP_CMPI:
        case OP_CMPF:
        case OP_CMPS:
        case OP_LENGTH:
        case OP_GETELEM:
        case OP_SETELEM:
        case OP_GETRANGE:
        case OP_SETRANGE:
        case OP_GETTUPLE:
        case OP_GETFIELD:
        case OP_SETTUPLE:
        case OP_SETFIELD:
            printf("%s %d\n", opname, GET_U(opcode));
            break;
        default:
            printf("%s\n", opname);
    }
}

void dump_aux(paw_Env *P, Proto *proto, Buffer *print)
{
    const OpCode *pc = proto->source;
    const OpCode *end = pc + proto->length;

    pawL_add_string(P, print, "function '");
    pawL_add_nstring(P, print, proto->name->text, proto->name->length);
    pawL_add_fstring(P, print, "' (%I bytes)\n", (paw_Int)proto->length);
    pawL_add_fstring(P, print, "constant(s) = %d, upvalue(s) = %d, arg(s) = %d\n",
                     proto->nk, proto->nup, proto->argc);
    for (int i = 0; pc != end; ++i) {
        pawL_add_fstring(P, print, "%d  %I  %s", i,
                         (paw_Int)(pc - proto->source),
                         paw_op_name(GET_OP(pc[0])));
        const OpCode opcode = *pc++;
        switch (GET_OP(opcode)) {
            case OP_POP: {
                pawL_add_fstring(P, print, " ; u = %d", GET_U(opcode));
                break;
            }

            case OP_SHIFT: {
                pawL_add_fstring(P, print, " ; n = %d", GET_U(opcode));
                break;
            }

            case OP_CLOSE: {
                pawL_add_fstring(P, print, " ; count = %d", GET_U(opcode));
                break;
            }

            case OP_PUSHCONST: {
                pawL_add_fstring(P, print, " ; k = %d", GET_U(opcode));
                break;
            }

            case OP_NEWVARIANT: {
                pawL_add_fstring(P, print, " ; #%d", GET_U(opcode));
                break;
            }

            case OP_NEWLIST:
            case OP_NEWMAP:
            case OP_NEWTUPLE: {
                pawL_add_fstring(P, print, " ; %d values", GET_U(opcode));
                break;
            }

            case OP_FORNUM0:
            case OP_FORNUM:
            case OP_FORLIST0:
            case OP_FORLIST:
            case OP_FORMAP0:
            case OP_FORMAP: {
                pawL_add_fstring(P, print, " ; offset = %d", GET_S(opcode));
                break;
            }

            case OP_GETFIELD: {
                pawL_add_fstring(P, print, " ; id = %d", GET_U(opcode));
                break;
            }

            case OP_SETFIELD: {
                pawL_add_fstring(P, print, " ; id = %d", GET_U(opcode));
                break;
            }

            case OP_GETLOCAL: {
                pawL_add_fstring(P, print, " ; id = %d", GET_U(opcode));
                break;
            }

            case OP_SETLOCAL: {
                pawL_add_fstring(P, print, " ; id = %d", GET_U(opcode));
                break;
            }

            case OP_GETUPVALUE: {
                pawL_add_fstring(P, print, "%d", GET_U(opcode));
                break;
            }

            case OP_SETUPVALUE: {
                pawL_add_fstring(P, print, "%d", GET_U(opcode));
                break;
            }

            case OP_GETGLOBAL: {
                const int u = GET_U(opcode);
                pawL_add_fstring(P, print, " ; id = %d", u);
                break;
            }

            case OP_CLOSURE: {
                const int idx = GET_U(opcode);
                Proto *p = proto->p[idx];
                String *s = p->name;
                pawL_add_string(P, print, " ; '");
                if (s) {
                    pawL_add_nstring(P, print, s->text, s->length);
                } else {
                    pawL_add_string(P, print, "<anonymous fn>");
                }
                pawL_add_fstring(P, print, "', nupvalues = %I",
                                 (paw_Int)p->nup);
                break;
            }

            case OP_CALL: {
                pawL_add_fstring(P, print, " ; # nargs = %d", GET_U(opcode));
                break;
            }

            case OP_JUMP: {
                pawL_add_fstring(P, print, " ; offset = %d", GET_S(opcode));
                break;
            }

            case OP_JUMPFALSE: {
                pawL_add_fstring(P, print, " ; offset = %d", GET_S(opcode));
                break;
            }

            case OP_JUMPNULL: {
                pawL_add_fstring(P, print, " ; offset = %d", GET_S(opcode));
                break;
            }

                // TODO: Add constant operands and proto names as "; comment"
                // after operator. (like luac)
        }
        pawL_add_char(P, print, '\n');
    }

    // Dump nested protos.
    for (int i = 0; i < proto->nproto; ++i) {
        pawL_add_char(P, print, '\n');
        dump_aux(P, proto->p[i], print);
    }
}

void paw_dump_stack(paw_Env *P)
{
    StackPtr sp = P->stack.p;
    for (int i = 0; sp != P->top.p; ++sp, ++i) {
        const Value v = *sp;
        if (pawZ_is_object(P->H, CAST_UPTR(v.p))) {
            printf("%d: Object @ %p\n", i, v.p); 
        } else {
            printf("%d: Value{%" PRIu64 "}\n", i, v.u); 
        }
    }
}

void paw_dump_source(paw_Env *P, Proto *proto)
{
    Buffer print;
    pawL_init_buffer(P, &print);
    dump_aux(P, proto, &print);
    pawL_add_char(P, &print, '\0');
    puts(print.data);
    pawL_discard_result(P, &print);
}

static int current_line(CallFrame *cf)
{
    Proto *p = cf->fn->p;
    const int pc = cf->pc - p->source;

    int i = 0;
    for (; i + 1 < p->nlines; ++i) {
        if (p->lines[i].pc >= pc) {
            break;
        }
    }
    return p->lines[i].line;
}

void paw_stacktrace(paw_Env *P)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);

    const String *modname = P->main.fn->p->name;

    int i = 0;
    CallFrame *cf = P->cf;
    while (cf->prev) {
        pawL_add_fstring(P, &buf, "%d: File ", i);
        pawL_add_nstring(P, &buf, modname->text, modname->length);
        pawL_add_fstring(P, &buf, ", line %d, in ", current_line(cf));
        if (CF_IS_PAW(cf)) {
            Proto *p = cf->fn->p;
            const String *name = p->name;
            pawL_add_nstring(P, &buf, name->text, name->length);
        } else {
            pawL_add_fstring(P, &buf, "<native>");
        }
        cf = cf->prev;
        ++i;
    }
    pawL_push_result(P, &buf);
}

#endif // defined(PAW_DEBUG_EXTRA)
