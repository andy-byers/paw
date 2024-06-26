// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "code.h"
#include "env.h"
#include "mem.h"
#include <limits.h>

static void add_line(FuncState *fs)
{
    Lex *lex = fs->G->lex;
    Proto *p = fs->proto;
    if (fs->nlines == UINT16_MAX) {
        pawX_error(lex, "too many instructions");
    }
    pawM_grow(lex->P, p->lines, fs->nlines, p->nlines);
    p->lines[fs->nlines++] = (struct LineInfo){
        .line = lex->line,
        .pc = fs->pc,
    };
}

void pawK_fix_line(FuncState *fs, int line)
{
    paw_assert(fs->nlines > 0);
    fs->proto->lines[fs->nlines - 1].line = line;
}

static void add_opcode(FuncState *fs, OpCode code)
{
    Lex *lex = fs->G->lex;
    Proto *p = fs->proto;

    // While code is being generated, the pc is used to track the number of
    // instructions, and the length field the capacity. The length is set to the
    // final pc value before execution.
    pawM_grow(lex->P, p->source, fs->pc, p->length);
    p->source[fs->pc] = code;
    ++fs->pc;
}

void pawK_code_0(FuncState *fs, Op op)
{
    add_line(fs);
    add_opcode(fs, create_OP(op));
}

void pawK_code_S(FuncState *fs, Op op, int s)
{
    add_line(fs);
    add_opcode(fs, create_S(op, s));
}

void pawK_code_U(FuncState *fs, Op op, int u)
{
    add_line(fs);
    add_opcode(fs, create_U(op, u));
}

void pawK_code_AB(FuncState *fs, Op op, int a, int b)
{
    add_line(fs);
    add_opcode(fs, create_AB(op, a, b));
}

// Create a new arena large enough to allocate memory of the 'required_size'
// Alignment is not considered, since the start of an Arena is suitably-aligned
// for any objects created by the compiler.
static Arena *new_arena(paw_Env *P, Pool *pool, size_t required_size)
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

void pawK_pool_init(paw_Env *P, Pool *pool, size_t base_size, size_t min_size)
{
    pool->filled = NULL;
    pool->last_size = base_size;
    pool->arena = new_arena(P, pool, 0);
    pool->min_size = min_size;
}

static void free_arena_list(paw_Env *P, Arena *a)
{
    while (a) {
        Arena *prev = a->prev;
        pawM_free_flex(P, a, a->size, 1);
        a = prev;
    }
}

void pawK_pool_uninit(paw_Env *P, Pool *pool)
{
    free_arena_list(P, pool->arena);
    free_arena_list(P, pool->filled);
}

void *pawK_pool_alloc(paw_Env *P, Pool *pool, size_t size, size_t align)
{
    paw_assert(size && align);

    Arena *a = pool->arena;
    size_t base = (a->used + align - 1) & ~(align - 1);
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
