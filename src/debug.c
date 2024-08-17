// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "debug.h"
#include "auxlib.h"
#include "call.h"
#include "map.h"
#include "rt.h"
#include <stdio.h>

const char *paw_unop_name(UnaryOp unop)
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

const char *paw_binop_name(BinaryOp binop)
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
        case BINARY_IN:
            return "IN";
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
        case OP_PUSHSTRUCT:
            return "PUSHSTRUCT";
        case OP_PUSHUNIT:
            return "PUSHUNIT";
        case OP_PUSHTRUE:
            return "PUSHTRUE";
        case OP_PUSHFALSE:
            return "PUSHFALSE";
        case OP_PUSHCONST:
            return "PUSHCONST";
        case OP_COPY:
            return "COPY";
        case OP_INITFIELD:
            return "INITFIELD";
        case OP_TRANSIT:
            return "TRANSIT";
        case OP_MATCHVARIANT:
            return "MATCHVARIANT";
        case OP_POP:
            return "POP";
        case OP_CLOSE:
            return "CLOSE";
        case OP_INIT:
            return "INIT";
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
        case OP_GLOBAL:
            return "GLOBAL";
        case OP_GETGLOBAL:
            return "GETGLOBAL";
        case OP_SETGLOBAL:
            return "SETGLOBAL";
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
        case OP_NEWVECTOR:
            return "NEWVECTOR";
        case OP_NEWMAP:
            return "NEWMAP";
        case OP_VARARG:
            return "VARARG";
        case OP_FORNUM0:
            return "FORNUM0";
        case OP_FORNUM:
            return "FORNUM";
        case OP_FORVECTOR0:
            return "FORVECTOR0";
        case OP_FORVECTOR:
            return "FORVECTOR";
        case OP_FORMAP0:
            return "FORMAP0";
        case OP_FORMAP:
            return "FORMAP";
        case OP_UNOP:
            return "UNOP";
        case OP_BINOP:
            return "BINOP";
        case OP_GETTUPLE:
            return "GETTUPLE";
        case OP_GETATTR:
            return "GETATTR";
        case OP_SETTUPLE:
            return "SETTUPLE";
        case OP_SETATTR:
            return "SETATTR";
        case OP_GETITEM:
            return "GETITEM";
        case OP_SETITEM:
            return "SETITEM";
        default:
            return "???";
    }
}

void paw_dump_opcode(OpCode opcode)
{
    switch (get_OP(opcode)) {
        case OP_CASTBOOL:
            printf("CASTBOOL\n");
            break;
        case OP_CASTINT:
            printf("CASTINT\n");
            break;
        case OP_CASTFLOAT:
            printf("CASTFLOAT\n");
            break;
        case OP_PUSHSTRUCT:
            printf("PUSHSTRUCT %d\n", get_U(opcode));
            break;
        case OP_PUSHUNIT:
            printf("PUSHUNIT\n");
            break;
        case OP_PUSHTRUE:
            printf("PUSHTRUE\n");
            break;
        case OP_PUSHFALSE:
            printf("PUSHFALSE\n");
            break;
        case OP_PUSHCONST:
            printf("PUSHCONST %d\n", get_U(opcode));
            break;
        case OP_COPY:
            printf("COPY\n");
            break;
        case OP_INITFIELD:
            printf("INITFIELD\n");
            break;
        case OP_POP:
            printf("POP\n");
            break;
        case OP_CLOSE:
            printf("CLOSE\n");
            break;
        case OP_INIT:
            printf("INIT\n");
            break;
        case OP_MATCHVARIANT:
            printf("MATCHVARIANT\n");
            break;
        case OP_TRANSIT:
            printf("TRANSIT\n");
            break;
        case OP_RETURN:
            printf("RETURN\n");
            break;
        case OP_CLOSURE:
            printf("CLOSURE\n");
            break;
        case OP_CALL:
            printf("CALL nargs = %d\n", get_U(opcode));
            break;
        case OP_JUMP:
            printf("JUMP\n");
            break;
        case OP_JUMPFALSE:
            printf("JUMPFALSE\n");
            break;
        case OP_JUMPFALSEPOP:
            printf("JUMPFALSEPOP\n");
            break;
        case OP_JUMPNULL:
            printf("JUMPNULL\n");
            break;
        case OP_GLOBAL:
            printf("GLOBAL\n");
            break;
        case OP_GETGLOBAL:
            printf("GETGLOBAL: %d\n", get_U(opcode));
            break;
        case OP_SETGLOBAL:
            printf("SETGLOBAL: %d\n", get_U(opcode));
            break;
        case OP_GETLOCAL:
            printf("GETLOCAL: %d\n", get_U(opcode));
            break;
        case OP_SETLOCAL:
            printf("SETLOCAL: %d\n", get_U(opcode));
            break;
        case OP_GETUPVALUE:
            printf("GETUPVALUE: %d\n", get_U(opcode));
            break;
        case OP_SETUPVALUE:
            printf("SETUPVALUE: %d\n", get_U(opcode));
            break;
        case OP_NEWTUPLE:
            printf("NEWTUPLE\n");
            break;
        case OP_NEWINSTANCE:
            printf("NEWINSTANCE\n");
            break;
        case OP_NEWVECTOR:
            printf("NEWVECTOR\n");
            break;
        case OP_NEWMAP:
            printf("NEWMAP\n");
            break;
        case OP_VARARG:
            printf("VARARG\n");
            break;
        case OP_FORNUM0:
            printf("FORNUM0\n");
            break;
        case OP_FORNUM:
            printf("FORNUM\n");
            break;
        case OP_FORVECTOR0:
            printf("FORVECTOR0\n");
            break;
        case OP_FORVECTOR:
            printf("FORVECTOR\n");
            break;
        case OP_FORMAP0:
            printf("FORMAP0\n");
            break;
        case OP_FORMAP:
            printf("FORMAP\n");
            break;
        case OP_UNOP:
            printf("UNOP %s %d\n", paw_unop_name(get_A(opcode)), get_B(opcode));
            break;
        case OP_BINOP:
            printf("BINOP %s %d\n", paw_binop_name(get_A(opcode)),
                   get_B(opcode));
            break;
        case OP_GETTUPLE:
            printf("GETTUPLE %d\n", get_U(opcode));
            break;
        case OP_GETATTR:
            printf("GETATTR %d\n", get_U(opcode));
            break;
        case OP_SETTUPLE:
            printf("SETTUPLE %d\n", get_U(opcode));
            break;
        case OP_SETATTR:
            printf("SETATTR %d\n", get_U(opcode));
            break;
        case OP_GETITEM:
            printf("GETITEM %d\n", get_U(opcode));
            break;
        case OP_SETITEM:
            printf("SETITEM %d\n", get_U(opcode));
            break;
        default:
            printf("???\n");
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
                         paw_op_name(get_OP(pc[0])));
        const OpCode opcode = *pc++;
        switch (get_OP(opcode)) {
            case OP_UNOP: {
                pawL_add_fstring(P, print, " ; op = %s",
                                 paw_unop_name(get_A(opcode)));
                break;
            }

            case OP_BINOP: {
                pawL_add_fstring(P, print, " ; op = %s",
                                 paw_binop_name(get_A(opcode)));
                break;
            }

            case OP_POP: {
                pawL_add_fstring(P, print, " ; u = %d", get_U(opcode));
                break;
            }

            case OP_MATCHVARIANT: {
                pawL_add_fstring(P, print, " ; k = %d", get_U(opcode));
                break;
            }

            case OP_TRANSIT: {
                pawL_add_fstring(P, print, " ; n = %d", get_U(opcode));
                break;
            }

            case OP_CLOSE: {
                pawL_add_fstring(P, print, " ; count = %d", get_U(opcode));
                break;
            }

            case OP_PUSHCONST: {
                pawL_add_fstring(P, print, " ; k = %d", get_U(opcode));
                break;
            }

            case OP_PUSHSTRUCT: {
                pawL_add_fstring(P, print, " ; $ = %d", get_U(opcode));
                break;
            }

            case OP_NEWVARIANT: {
                pawL_add_fstring(P, print, " ; #%d", get_U(opcode));
                break;
            }

            case OP_NEWVECTOR:
            case OP_NEWMAP:
            case OP_NEWTUPLE: {
                pawL_add_fstring(P, print, " ; %d values", get_U(opcode));
                break;
            }

            case OP_FORNUM0:
            case OP_FORNUM:
            case OP_FORVECTOR0:
            case OP_FORVECTOR:
            case OP_FORMAP0:
            case OP_FORMAP: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_GETATTR: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_SETATTR: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_GETLOCAL: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_SETLOCAL: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_GETUPVALUE: {
                pawL_add_fstring(P, print, "%d", get_U(opcode));
                break;
            }

            case OP_SETUPVALUE: {
                pawL_add_fstring(P, print, "%d", get_U(opcode));
                break;
            }

            case OP_GETGLOBAL: {
                const int iw = get_U(opcode);
                pawL_add_string(P, print, " ; id = ");
                pawL_add_integer(P, print, iw);
                break;
            }

            case OP_SETGLOBAL: {
                pawL_add_fstring(P, print, " ; id = %d", get_U(opcode));
                break;
            }

            case OP_GLOBAL: {
                pawL_add_fstring(P, print, " ; k = %d", get_U(opcode));
                break;
            }

            case OP_CLOSURE: {
                const int idx = get_U(opcode);
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
                pawL_add_fstring(P, print, " ; # nargs = %d", get_U(opcode));
                break;
            }

            case OP_VARARG: {
                const int nfixed = get_U(opcode);
                const int npassed = paw_get_count(P) - 1;
                pawL_add_fstring(P, print, " ; # nargs = %d", npassed - nfixed);
                break;
            }

            case OP_JUMP: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_JUMPFALSE: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
                break;
            }

            case OP_JUMPNULL: {
                pawL_add_fstring(P, print, " ; offset = %d", get_S(opcode));
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
        if (cf_is_paw(cf)) {
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

void paw_dump_value(paw_Env *P, Value v, paw_Type type)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawC_pushv(P, v);
    pawL_add_value(P, &buf, type);
    pawL_add_char(P, &buf, '\0');
    printf("%s\n", buf.data);
    pawL_discard_result(P, &buf);
}
