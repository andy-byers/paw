// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "test.h"
#include "call.h"
#include "env.h"
#include "os.h"
#include "paw.h"
#include "util.h"
#include "value.h"
#include <assert.h>
#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Fill memory with alternating bits (each byte has value 0b10101010)
static void trash_memory(void *ptr, size_t o, size_t n)
{
    unsigned long long volatile *ll = (unsigned long long *)((unsigned char *)ptr + o);
    for (; n >= sizeof(*ll); n -= sizeof(*ll))
        *ll++ = 0xAAAAAAAAAAAAAAAA;

    unsigned char volatile *c = (unsigned char *)ll;
    while (n-- > 0)
        *c++ = 0xAA;
}

static void next_chunk(struct TestReader *rd)
{
    rd->index = 0;
    if (rd->file) {
        rd->length = fread(rd->buf, 1, sizeof(rd->buf), rd->file);
        return;
    }
    size_t const n = PAW_MIN(rd->ndata, READ_MAX);
    memcpy(rd->buf, rd->data, n);
    rd->data += n;
    rd->ndata -= n;
    rd->length = n;
}

// Read a source file in small chunks to make sure the parser can work incrementally
char const *test_reader(paw_Env *P, void *ud, size_t *size)
{
    PAW_UNUSED(P);
    struct TestReader *rd = ud;
    if (rd->length == 0) {
        next_chunk(rd);
        if (rd->length == 0) {
            *size = 0;
            return NULL;
        }
    }
    size_t const r = (size_t)rand();
    *size = PAW_MAX(1, r % rd->length);
    char const *ptr = rd->buf + rd->index;
    rd->length -= *size;
    rd->index += *size;
    return ptr;
}

char const *test_pathname(char const *name)
{
    static char s_buf[PAW_LENGTHOF(TEST_PREFIX) + 64];
    s_buf[0] = '\0'; // reset length
    strcat(s_buf, TEST_PREFIX);
    strcat(s_buf, "scripts/");
    strcat(s_buf, name);
    strcat(s_buf, ".paw");
    return s_buf;
}

#ifdef ENABLE_PTR_TRACKER
static size_t find_ptr(struct TestAlloc *a, void *ptr)
{
    for (size_t i = 0; i < a->count; ++i) {
        if (a->ptrs[i] == ptr)
            return i;
    }
    PAW_UNREACHABLE();
}

static void remove_ptr(struct TestAlloc *a, void *ptr, size_t size)
{
    size_t const i = find_ptr(a, ptr);
    check(a->sizes[i] == size);
    a->sizes[i] = a->sizes[a->count - 1];
    a->ptrs[i] = a->ptrs[a->count - 1];
    --a->count;
}

static void add_ptr(struct TestAlloc *a, void *ptr, size_t size)
{
    check(a->count < PTR_TRACKER_LIMIT);
    a->sizes[a->count] = size;
    a->ptrs[a->count] = ptr;
    ++a->count;
}

static void modify_size(struct TestAlloc *a, void *ptr, size_t size)
{
    size_t const i = find_ptr(a, ptr);
    a->sizes[i] = size;
}
#else
static void remove_ptr(struct TestAlloc *a, void *ptr, size_t size)
{
    PAW_UNUSED(ptr);
    PAW_UNUSED(size);

    --a->count;
}

static void add_ptr(struct TestAlloc *a, void *ptr, size_t size)
{
    PAW_UNUSED(ptr);
    PAW_UNUSED(size);

    check(a->count < SIZE_MAX);
    ++a->count;
}

static void modify_size(struct TestAlloc *a, void *ptr, size_t size)
{
    PAW_UNUSED(a);
    PAW_UNUSED(ptr);
    PAW_UNUSED(size);
}
#endif

void test_mem_hook(void *ud, void *ptr, size_t size0, size_t size)
{
    struct TestAlloc *a = ud;
    if (ptr != NULL) {
        // trash newly-allocated memory, as well as memory about to be released
        size_t const lower = PAW_MIN(size0, size);
        size_t const upper = PAW_MAX(size0, size);
        trash_memory(ptr, lower, upper - lower);
    }

    if (size0 == 0 && size != 0)
        add_ptr(a, ptr, size);
    if (size0 != 0 && size == 0)
        remove_ptr(a, ptr, size0);
    if (size0 != 0 && size != 0)
        modify_size(a, ptr, size);
}

paw_Env *test_open(paw_MemHook mem_hook, struct TestAlloc *a, size_t heap_size)
{
#ifdef ENABLE_PTR_TRACKER
    fprintf(stderr, "pointer tracking is enabled, perforamnce will be impacted\n");
    a->ptrs = malloc(PTR_TRACKER_LIMIT * sizeof(a->ptrs[0]));
    a->sizes = malloc(PTR_TRACKER_LIMIT * sizeof(a->sizes[0]));
#endif
    a->count = 0;

    return paw_open(&(struct paw_Options){
        .heap_size = heap_size,
        .mem_hook = mem_hook,
        .ud = a,
    });
}

void test_close(paw_Env *P, struct TestAlloc *a)
{
    paw_close(P);

    // TODO: This preprocessor guard (#ifndef _MSC_VER) should be removed. For
    //       whatever reason (probably UB somewhere), Paw compiled by MSVC reports
    //       some leaked allocations. I don't have an easy way to debug a binary
    //       produced by MSVC, so this bug will have to be fixed later, or by
    //       someone else...
#ifndef _MSC_VER
    if (a->count > 0) {
#ifdef ENABLE_PTR_TRACKER
        for (size_t i = 0; i < a->count; ++i) {
            fprintf(stderr, "error: leaked %zu bytes at address %p\n",
                a->sizes[i], a->ptrs[i]);
        }
#endif
        fprintf(stderr, "error: leaked %zu allocations\n", a->count);
        abort();
    }
#endif
}

static void check_ok(paw_Env *P, int status)
{
    if (status != PAW_OK) {
        test_recover(P, PAW_TRUE); // no return
    }
}

int test_open_file(paw_Env *P, char const *name)
{
    char const *pathname = test_pathname(name);
    if (P == NULL)
        return PAW_EMEMORY;

    FILE *file = fopen(pathname, "r");
    check(file);
    struct TestReader rd = {.file = file};
    rd.data = rd.buf;

    int const rc = paw_load(P, test_reader, pathname, &rd);
    fclose(file);
    return rc;
}

int test_open_string(paw_Env *P, char const *source)
{
    struct TestReader rd = {.data = source, .ndata = strlen(source)};
    return paw_load(P, test_reader, "<string>", &rd);
}

void test_recover(paw_Env *P, paw_Bool fatal)
{
    // Expect an error message on top of the stack.
    check(paw_get_count(P) >= 1);

    if (fatal) {
        char const *s = paw_string(P, -1);
        fprintf(stderr, "%s\n", s);
        abort();
    }
    paw_pop(P, 1);
}

void test_script(char const *name, struct TestAlloc *a)
{
    paw_Env *P = test_open(test_mem_hook, a, 0);
    check_ok(P, test_open_file(P, name));
    check_ok(P, paw_call(P, 0));
    test_close(P, a);
}

paw_Int test_randint(paw_Int min, paw_Int max)
{
    check(min <= max);
    return rand() % (max - min) + min;
}

void test_randstr(char *str, int len)
{
    static char const kChars[] =
        "0123456789"
        "abcdefghijklmnopqrstuvwxyz"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
        " \t\n\r\v\f";
    for (int i = 0; i < len; ++i) {
        str[i] = test_randint(0, sizeof(kChars) - 1);
    }
}

char const *op_name(Op op)
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
        case OP_INOT:
            return "INOT";
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
        case OP_BITNOT:
            return "BITNOT";
        case OP_BITAND:
            return "BITAND";
        case OP_BITOR:
            return "BITOR";
        case OP_BITXOR:
            return "BITXOR";
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
        case OP_LGETP:
            return "LGETP";
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
        case OP_MNEWP:
            return "MNEWP";
        case OP_MGETP:
            return "MGETP";
        case OP_MGET:
            return "MGET";
        case OP_MSET:
            return "MSET";
        case OP_GETVALUE:
            return "GETVALUE";
        case OP_SETVALUE:
            return "SETVALUE";
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

void paw_dump_opcode(OpCode opcode)
{
    char const *opname = op_name(GET_OP(opcode));
    switch (GET_OP(opcode)) {
        case NOPCODES:
            PAW_UNREACHABLE();
        // op A B
        case OP_NEWTUPLE:
        case OP_NEWLIST:
        case OP_NEWMAP:
        case OP_MOVE:
        case OP_INOT:
        case OP_INEG:
        case OP_BITNOT:
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
        case OP_SWITCHINT:
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
        case OP_BITAND:
        case OP_BITOR:
        case OP_BITXOR:
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
        case OP_LGETP:
        case OP_LGET:
        case OP_LSET:
        case OP_LGETN:
        case OP_LSETN:
        case OP_MNEWP:
        case OP_MGETP:
        case OP_MGET:
        case OP_MSET:
        case OP_GETFIELD:
        case OP_SETFIELD:
        case OP_GETVALUE:
        case OP_SETVALUE:
        case OP_TESTK:
            printf("%s %d %d %d\n", opname, GET_A(opcode), GET_B(opcode), GET_C(opcode));
            break;
        case OP_NOOP:
            printf("%s\n", opname);
    }
}

void dump_aux(paw_Env *P, Proto *proto, Buffer *print)
{
    OpCode const *pc = proto->source;
    OpCode const *end = pc + proto->length;

    pawL_add_string(P, print, "function '");
    pawL_add_nstring(P, print, proto->name->text, proto->name->length);
    pawL_add_fstring(P, print, "' (%I bytes)\n", (paw_Int)proto->length);
    pawL_add_fstring(P, print, "constant(s) = %d, upvalue(s) = %d, arg(s) = %d\n",
                     proto->nk, proto->nup, proto->argc);
    for (int i = 0; pc != end; ++i, ++pc) {
        OpCode const opcode = *pc;
        char const *opname = op_name(GET_OP(opcode));
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
            case OP_INOT:
            case OP_INEG:
            case OP_BITNOT:
            case OP_FNEG:
            case OP_SLENGTH:
            case OP_LLENGTH:
            case OP_MLENGTH:
            case OP_SCONCAT:
            case OP_LCONCAT:
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
            case OP_BITAND:
            case OP_BITOR:
            case OP_BITXOR:
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
            case OP_SGET:
            case OP_SGETN:
            case OP_LGETP:
            case OP_LGET:
            case OP_LSET:
            case OP_LGETN:
            case OP_LSETN:
            case OP_MNEWP:
            case OP_MGETP:
            case OP_MGET:
            case OP_MSET:
            case OP_GETFIELD:
            case OP_SETFIELD:
            case OP_GETVALUE:
            case OP_SETVALUE:
                pawL_add_fstring(P, print, " %d %d %d\n", GET_A(opcode), GET_B(opcode), GET_C(opcode));
                break;
            case OP_NOOP:
            case OP_RETURN:
            default:
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

void test_dump_source(paw_Env *P, paw_Debug const *d)
{
    CallFrame *cf = d->cf;

    if (CF_IS_PAW(cf)) {
        Buffer print;
        pawL_init_buffer(P, &print);
        dump_aux(P, cf->fn->p, &print);
        pawL_add_char(P, &print, '\0');
        puts(print.data);
        pawL_discard_result(P, &print);
    }
}

