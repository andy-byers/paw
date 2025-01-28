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
#include "type.h"
#include <inttypes.h>
#include <stdio.h>

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

void pawD_debug_log(paw_Env *P, const char *fmt, ...)
{
    va_list arg;
    va_start(arg, fmt);
    vfprintf(stderr, fmt, arg);
    va_end(arg);

    fputc('\n', stderr);
    fflush(stderr);
}

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
        case OP_LOADSMI:
            return "LOADSMI";
        case OP_LOADK:
            return "LOADK";
        case OP_NOOP:
            return "NOOP";
        case OP_MOVE:
            return "MOVE";
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
        case OP_JUMPT:
            return "JUMPT";
        case OP_JUMPF:
            return "JUMPF";
        case OP_GETGLOBAL:
            return "GETGLOBAL";
        case OP_GETUPVALUE:
            return "GETUPVALUE";
        case OP_SETUPVALUE:
            return "SETUPVALUE";
        case OP_NEWTUPLE:
            return "NEWTUPLE";
        case OP_NEWLIST:
            return "NEWLIST";
        case OP_NEWMAP:
            return "NEWMAP";
        case OP_IEQ:
            return "IEQ";
        case OP_INE:
            return "INE";
        case OP_ILT:
            return "ILT";
        case OP_ILE:
            return "ILE";
        case OP_IGT:
            return "IGT";
        case OP_IGE:
            return "IGE";
        case OP_NOT:
            return "NOT";
        case OP_INEG:
            return "INEG";
        case OP_IADD:
            return "IADD";
        case OP_ISUB:
            return "ISUB";
        case OP_IMUL:
            return "IMUL";
        case OP_IDIV:
            return "IDIV";
        case OP_IMOD:
            return "IMOD";
        case OP_BNOT:
            return "BNOT";
        case OP_BAND:
            return "BAND";
        case OP_BOR:
            return "BOR";
        case OP_BXOR:
            return "BXOR";
        case OP_SHL:
            return "SHL";
        case OP_SHR:
            return "SHR";
        case OP_FEQ:
            return "FEQ";
        case OP_FNE:
            return "FNE";
        case OP_FLT:
            return "FLT";
        case OP_FLE:
            return "FLE";
        case OP_FGT:
            return "FGT";
        case OP_FGE:
            return "FGE";
        case OP_FNEG:
            return "FNEG";
        case OP_FADD:
            return "FADD";
        case OP_FSUB:
            return "FSUB";
        case OP_FMUL:
            return "FMUL";
        case OP_FDIV:
            return "FDIV";
        case OP_FMOD:
            return "FMOD";
        case OP_SEQ:
            return "SEQ";
        case OP_SNE:
            return "SNE";
        case OP_SLT:
            return "SLT";
        case OP_SLE:
            return "SLE";
        case OP_SGT:
            return "SGT";
        case OP_SGE:
            return "SGE";
        case OP_SLENGTH:
            return "SLENGTH";
        case OP_SCONCAT:
            return "SCONCAT";
        case OP_SGET:
            return "SGET";
        case OP_SGETN:
            return "SGETN";
        case OP_LLENGTH:
            return "LLENGTH";
        case OP_LCONCAT:
            return "LCONCAT";
        case OP_LGET:
            return "LGET";
        case OP_LSET:
            return "LSET";
        case OP_LGETN:
            return "LGETN";
        case OP_LSETN:
            return "LSETN";
        case OP_MLENGTH:
            return "MLENGTH";
        case OP_MGET:
            return "MGET";
        case OP_MSET:
            return "MSET";
        case OP_GETFIELD:
            return "GETFIELD";
        case OP_SETFIELD:
            return "SETFIELD";
        case OP_GETDISCR:
            return "GETDISCR";
        case OP_XCASTB:
            return "XCASTB";
        case OP_ICASTF:
            return "ICASTF";
        case OP_FCASTI:
            return "FCASTI";
        case OP_TESTK:
            return "TESTK";
        case OP_SWITCHINT:
            return "SWITCHINT";
        default:
            return "???";
    }
}

// TODO: Most of this should not be in the core: use hooks for debugging
#if defined(PAW_DEBUG_EXTRA)

void pawD_dump_defs(paw_Env *P)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    pawL_add_fstring(P, &buf, "did\tvid\tname\ttype\n", P->modname->text);
    size_t mxname = 0;
    for (int i = 0; i < P->defs.count; ++i) {
        struct Def *def = Y_DEF(P, i);
        if (def->hdr.name == NULL) continue;
        mxname = PAW_MAX(mxname, def->hdr.name->length);
    }
    for (int i = 0; i < P->defs.count; ++i) {
        struct Def *def = Y_DEF(P, i);
        const char *name = def->hdr.name != NULL
                ? def->hdr.name->text : "(null)";
        pawL_add_fstring(P, &buf, "%d\t", i);
        if (def->hdr.kind == DEF_FUNC) {
            pawL_add_fstring(P, &buf, "%d\t", def->func.vid);
        } else {
            L_ADD_LITERAL(P, &buf, "-\t");
        }
        pawL_add_fstring(P, &buf, "%s\t", name);
        pawY_print_type(P, &buf, def->hdr.code);
        pawL_add_char(P, &buf, '\n');
    }
    pawL_add_char(P, &buf, '\0');
    printf("%s\n", buf.data);
    pawL_discard_result(P, &buf);
}

void paw_dump_opcode(OpCode opcode)
{
    const char *opname = paw_op_name(GET_OP(opcode));
    switch (GET_OP(opcode)) {
        case NOPCODES:
            PAW_UNREACHABLE();
        // op A B
        case OP_NEWTUPLE:
        case OP_NEWLIST:
        case OP_NEWMAP:
        case OP_MOVE:
        case OP_NOT:
        case OP_INEG:
        case OP_BNOT:
        case OP_FNEG:
        case OP_SLENGTH:
        case OP_LLENGTH:
        case OP_MLENGTH:
        case OP_XCASTB:
        case OP_FCASTI:
        case OP_ICASTF:
        case OP_GETDISCR:
        case OP_CALL:
        case OP_GETUPVALUE:
        case OP_SETUPVALUE:
            printf("%s %d %d\n", opname, GET_A(opcode), GET_B(opcode));
            break;
        // op A sBx
        case OP_LOADSMI:
        case OP_JUMPT:
        case OP_JUMPF:
            printf("%s %d %d\n", opname, GET_A(opcode), GET_sBx(opcode));
            break;
        // op A sBx
        case OP_LOADK:
        case OP_CLOSURE:
        case OP_GETGLOBAL:
            printf("%s %d %d\n", opname, GET_A(opcode), GET_Bx(opcode));
            break;
        // op
        case OP_RETURN:
            printf("%s\n", opname);
            break;
        // op sBx
        case OP_JUMP:
            printf("%s %d\n", opname, GET_sBx(opcode));
            break;
        // op A
        case OP_CLOSE:
            printf("%s %d\n", opname, GET_A(opcode));
            break;
        // op A B C
        case OP_IEQ:
        case OP_INE:
        case OP_ILT:
        case OP_ILE:
        case OP_IGT:
        case OP_IGE:
        case OP_IADD:
        case OP_ISUB:
        case OP_IMUL:
        case OP_IDIV:
        case OP_IMOD:
        case OP_BAND:
        case OP_BOR:
        case OP_BXOR:
        case OP_SHL:
        case OP_SHR:
        case OP_FEQ:
        case OP_FNE:
        case OP_FLT:
        case OP_FLE:
        case OP_FGT:
        case OP_FGE:
        case OP_FADD:
        case OP_FSUB:
        case OP_FMUL:
        case OP_FDIV:
        case OP_FMOD:
        case OP_SEQ:
        case OP_SNE:
        case OP_SLT:
        case OP_SLE:
        case OP_SGT:
        case OP_SGE:
        case OP_SCONCAT:
        case OP_SGET:
        case OP_SGETN:
        case OP_LCONCAT:
        case OP_LGET:
        case OP_LSET:
        case OP_LGETN:
        case OP_LSETN:
        case OP_MGET:
        case OP_MSET:
        case OP_GETFIELD:
        case OP_SETFIELD:
            printf("%s %d %d %d\n", opname, GET_A(opcode), GET_B(opcode), GET_C(opcode));
            break;
        case OP_NOOP:
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
    for (int i = 0; pc != end; ++i, ++pc) {
        const OpCode opcode = *pc;
        const char *opname = paw_op_name(GET_OP(opcode));
        pawL_add_fstring(P, print, "%d  %s", i, opname);
        switch (GET_OP(opcode)) {
            case NOPCODES:
                PAW_UNREACHABLE();
            // op A B
            case OP_NEWTUPLE:
            case OP_NEWLIST:
            case OP_NEWMAP:
            case OP_TESTK:
            case OP_SWITCHINT:
            case OP_MOVE:
            case OP_NOT:
            case OP_INEG:
            case OP_BNOT:
            case OP_FNEG:
            case OP_SLENGTH:
            case OP_LLENGTH:
            case OP_MLENGTH:
            case OP_XCASTB:
            case OP_FCASTI:
            case OP_ICASTF:
            case OP_GETDISCR:
            case OP_GETUPVALUE:
            case OP_SETUPVALUE:
                pawL_add_fstring(P, print, " %d %d\n", GET_A(opcode), GET_B(opcode));
                break;
            case OP_CALL:
                pawL_add_fstring(P, print, " %d %d\n", GET_A(opcode), GET_B(opcode));
                break;
            // op A sBx
            case OP_LOADSMI:
            case OP_JUMPT:
            case OP_JUMPF:
                pawL_add_fstring(P, print, " %d %d\n", GET_A(opcode), GET_sBx(opcode));
                break;
            // op A Bx
            case OP_LOADK:
            case OP_CLOSURE:
            case OP_GETGLOBAL:
                pawL_add_fstring(P, print, " %d %d\n", GET_A(opcode), GET_Bx(opcode));
                break;
            // op sBx
            case OP_JUMP:
                pawL_add_fstring(P, print, " %d\n", GET_sBx(opcode));
                break;
            // op A
            case OP_CLOSE:
                pawL_add_fstring(P, print, " %d\t; close(R[%d])\n", GET_A(opcode), GET_A(opcode));
                break;
            // op A B C
            case OP_IEQ:
            case OP_INE:
            case OP_ILT:
            case OP_ILE:
            case OP_IGT:
            case OP_IGE:
            case OP_IADD:
            case OP_ISUB:
            case OP_IMUL:
            case OP_IDIV:
            case OP_IMOD:
            case OP_BAND:
            case OP_BOR:
            case OP_BXOR:
            case OP_SHL:
            case OP_SHR:
            case OP_FEQ:
            case OP_FNE:
            case OP_FLT:
            case OP_FLE:
            case OP_FGT:
            case OP_FGE:
            case OP_FADD:
            case OP_FSUB:
            case OP_FMUL:
            case OP_FDIV:
            case OP_FMOD:
            case OP_SEQ:
            case OP_SNE:
            case OP_SLT:
            case OP_SLE:
            case OP_SGT:
            case OP_SGE:
            case OP_SCONCAT:
            case OP_SGET:
            case OP_SGETN:
            case OP_LCONCAT:
            case OP_LGET:
            case OP_LSET:
            case OP_LGETN:
            case OP_LSETN:
            case OP_MGET:
            case OP_MSET:
            case OP_GETFIELD:
            case OP_SETFIELD:
                pawL_add_fstring(P, print, " %d %d %d\n", GET_A(opcode), GET_B(opcode), GET_C(opcode));
                break;
            case OP_NOOP:
            case OP_RETURN:
                pawL_add_char(P, print, '\n');
                break;
        }
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
        printf("%d: %" PRIx64 "\n", i, v.u);
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

void paw_dump_map_binary(Map *m)
{
    printf("Map {\n");
    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(m, &iter)) {
        const Value k = *pawH_key(m, iter);
        const Value v = *pawH_value(m, iter);
        printf("  %" PRId64 ": %" PRIu64 " => %" PRIu64 "\n", iter, k.u, v.u);
    }
    printf("}\n");
}

void paw_dump_value(Value value, paw_Type type)
{
    switch (type) {
        case PAW_TUNIT:
            printf("()");
            break;
        case PAW_TBOOL:
            printf("%s", V_TRUE(value) ? "true" : "false");
            break;
        case PAW_TINT:
            printf("%" PRId64, V_INT(value));
            break;
        case PAW_TFLOAT:
            printf("%f", V_FLOAT(value));
            break;
        case PAW_TSTR:
            printf("%s", V_TEXT(value));
            break;
        default:
            printf("0x%" PRIx64, value.u);
    }
}

void paw_dump_map(Map *m, paw_Type ktype, paw_Type vtype)
{
    printf("Map {\n");
    paw_Int iter = PAW_ITER_INIT;
    while (pawH_iter(m, &iter)) {
        const Value k = *pawH_key(m, iter);
        const Value v = *pawH_value(m, iter);
        printf("  %" PRId64 ": ", iter);
        paw_dump_value(k, ktype);
        printf(" => ");
        paw_dump_value(v, vtype);
        printf("\n");
    }
    printf("}\n");
}

#endif // defined(PAW_DEBUG_EXTRA)
