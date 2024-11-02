// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "code.h"
#include "compile.h"

static int stack_effect(OpCode opcode)
{
    const Op op = GET_OP(opcode);
    switch (op) {
        case OP_NEWTUPLE:
        case OP_NEWLIST:
            return 1 - GET_U(opcode);
        case OP_NEWMAP:
            return 1 - GET_U(opcode) * 2;
        case OP_POP:
        case OP_CLOSE:
        case OP_SHIFT:
        case OP_CALL:
            return -GET_U(opcode);
        case OP_SETRANGE:
            return -4;
        case OP_SETFIELD:
        case OP_SETELEM:
            return -3;
        case OP_GETRANGE:
            return -2;
        case OP_JUMPFALSEPOP:
        case OP_SETLOCAL:
        case OP_SETUPVALUE:
        case OP_INITFIELD:
        case OP_CMPI:
        case OP_CMPF:
        case OP_CMPS:
        case OP_ARITHI2:
        case OP_ARITHF2:
        case OP_BITW2:
        case OP_GETFIELD:
        case OP_GETELEM:
        case OP_CONCAT:
            return -1;
        case OP_RETURN:
        case OP_NOOP:
        case OP_JUMP:
        case OP_JUMPFALSE:
        case OP_JUMPNULL:
        case OP_ARITHI1:
        case OP_ARITHF1:
        case OP_BITW1:
        case OP_CAST:
        case OP_NOT:
        case OP_LENGTH:
        case OP_SWITCHDISCR:
        case OP_GETDISCR:
        case OP_TESTINT:
            return 0;
        case OP_PUSHZERO:
        case OP_PUSHONE:
        case OP_PUSHSMI:
        case OP_PUSHCONST:
        case OP_COPY:
        case OP_NEWINSTANCE:
        case OP_GETGLOBAL:
        case OP_GETLOCAL:
        case OP_GETUPVALUE:
        case OP_FORNUM0:
        case OP_FORNUM:
        case OP_FORLIST:
        case OP_FORMAP:
        case OP_CLOSURE:
            return 1;
        case OP_FORLIST0:
        case OP_FORMAP0:
            return 2;
        case NOPCODES:
            PAW_UNREACHABLE();
    }
}

static void add_line(struct FuncState *fs)
{
    Proto *p = fs->proto;
    paw_Env *P = ENV(fs->G);
    pawM_grow(P, p->lines, fs->nlines, p->nlines);
    p->lines[fs->nlines++] = (struct LineInfo){
        .line = fs->line,
        .pc = fs->pc,
    };
}

void pawK_fix_line(struct FuncState *fs, int line)
{
    paw_assert(fs->nlines > 0);
    fs->proto->lines[fs->nlines - 1].line = line;
}

static void add_opcode(struct FuncState *fs, OpCode code)
{
    paw_Env *P = ENV(fs->G);
    Proto *p = fs->proto;

    // While code is being generated, the 'pc' is used to track the number of
    // instructions, while the 'length' holds the capacity. The 'length' is set to the
    // final 'pc' before execution.
    pawM_grow(P, p->source, fs->pc, p->length);
    p->source[fs->pc] = code;
    ++fs->pc;

    fs->nstack += stack_effect(code);
    if (p->max_stack < fs->nstack) {
        p->max_stack = fs->nstack;
    }
}

void pawK_code_0(struct FuncState *fs, Op op)
{
    add_line(fs);
    add_opcode(fs, CREATE_OP(op));
}

void pawK_code_S(struct FuncState *fs, Op op, int s)
{
    add_line(fs);
    add_opcode(fs, CREATE_S(op, s));
}

void pawK_code_U(struct FuncState *fs, Op op, int u)
{
    add_line(fs);
    add_opcode(fs, CREATE_U(op, u));
}

void pawK_code_AB(struct FuncState *fs, Op op, int a, int b)
{
    add_line(fs);
    add_opcode(fs, CREATE_AB(op, a, b));
}

typedef struct Arena {
    struct Arena *prev;
    size_t used;
    size_t size;

    // Must be aligned to at least the strictest alignment required
    // by an AST or HIR node.
    K_ALIGNAS_NODE char data[];
} Arena;

// Create a new arena large enough to allocate memory of the 'required_size'
// Alignment is not considered, since the start of an Arena is suitably-aligned
// for any objects created by the compiler.
static Arena *new_arena(paw_Env *P, struct Pool *pool, size_t required_size)
{
    if (required_size > SIZE_MAX / 2) {
        pawM_error(P); // sanity check
    }
    size_t size = pool->last_size;
    while (size < required_size) {
        size *= 2;
    }
    pool->last_size = size;
    Arena *a = pawM_new_flex(P, Arena, size, 1);
    memset(a->data, 0, size);
    a->size = size;
    return a;
}

void pawK_pool_init(paw_Env *P, struct Pool *pool, size_t base_size, size_t min_size)
{
    pool->filled = NULL;
    pool->last_size = base_size;
    pool->arena = new_arena(P, pool, 0);
    pool->min_size = min_size;
}

static void free_arena_list(paw_Env *P, Arena *a)
{
    while (a != NULL) {
        Arena *prev = a->prev;
        pawM_free_flex(P, a, a->size, 1);
        a = prev;
    }
}

void pawK_pool_uninit(paw_Env *P, struct Pool *pool)
{
    free_arena_list(P, pool->arena);
    free_arena_list(P, pool->filled);
}

static void *find_free_block(struct Pool *pool, size_t size)
{
    for (struct FreeBlock **p = &pool->free; *p; p = &(*p)->prev) {
        if (size <= CAST_SIZE((*p)->size)) {
            paw_assert(0 == (CAST_UPTR(*p) & (K_ALIGNOF_NODE - 1)));
            struct FreeBlock *block = *p;
            *p = (*p)->prev;
            return block;
        }
        p = &(*p)->prev;
    }
    return NULL;
}

void *pawK_pool_alloc(paw_Env *P, struct Pool *pool, size_t size)
{
    paw_assert(size > 0);
    // TODO: enable recycling
//    void *ptr = find_free_block(pool, size);
//    if (ptr != NULL) {
//        memset(ptr, 0, size);
//        return ptr;
//    }

    Arena *a = pool->arena;
    size_t base = (a->used + K_ALIGNOF_NODE - 1) & ~(K_ALIGNOF_NODE - 1);
    if (base + size > a->size) {
        // create a new arena, guaranteed to hold at least 'size' bytes
        a = new_arena(P, pool, size);
        a->prev = pool->arena;
        pool->arena = a;
        base = 0;
    }
    a->used = base + size;
    return a->data + base;
}

void pawK_pool_free(struct Pool *pool, void *ptr, size_t size)
{
//    paw_assert(0 == (CAST_UPTR(ptr) & _Alignof(struct FreeBlock)));
//    paw_assert(size >= sizeof(struct FreeBlock));
//    const struct FreeBlock prototype = {
//        .prev = pool->free,
//        .size = size,
//    };
//    memcpy(ptr, &prototype, sizeof(prototype));
//    pool->free = ptr;
}

void *pawK_list_ensure_one(struct Compiler *C, void *data, int count, int *palloc)
{
    if (count == *palloc) {
        if (*palloc > K_LIST_MAX / 2) pawM_error(ENV(C));
        const size_t nextcap = CAST_SIZE(*palloc) * 2;
        void *next = pawK_pool_alloc(ENV(C), C->pool, nextcap * sizeof(void *));
        memcpy(next, data, CAST_SIZE(count) * sizeof(void *));
        *palloc = CAST(int, nextcap);
        return next;
    }
    return data;
}

