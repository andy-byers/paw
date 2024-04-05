// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "code.h"
#include "mem.h"
#include <limits.h>

static void add_line(FnState *fn)
{
    Lex *lex = fn->lex;
    Proto *p = fn->proto;
    if (fn->nlines == UINT16_MAX) {
        pawX_error(lex, "too many instructions");
    }
    pawM_grow(lex->P, p->lines, fn->nlines, p->nlines);
    p->lines[fn->nlines++] = (struct LineInfo){
        .line = lex->lastline,
        .pc = fn->pc,
    };
}

void pawK_fix_line(FnState *fn, int line)
{
    paw_assert(fn->nlines > 0);
    fn->proto->lines[fn->nlines - 1].line = line;
}

static void add_opcode(FnState *fn, OpCode code)
{
    Lex *lex = fn->lex;
    Proto *p = fn->proto;

    // While code is being generated, the pc is used to track the number of instructions, and
    // the length field the capacity. The length is set to the final pc value before execution.
    pawM_grow(lex->P, p->source, fn->pc, p->length);
    p->source[fn->pc] = code;
    ++fn->pc;
}

void pawK_code_0(FnState *fn, Op op)
{
    add_line(fn);
    add_opcode(fn, create_OP(op));
}

void pawK_code_S(FnState *fn, Op op, int s)
{
    add_line(fn);
    add_opcode(fn, create_S(op, s));
}

void pawK_code_U(FnState *fn, Op op, int u)
{
    add_line(fn);
    add_opcode(fn, create_U(op, u));
}

void pawK_code_AB(FnState *fn, Op op, int a, int b)
{
    add_line(fn);
    add_opcode(fn, create_AB(op, a, b));
}

static Arena *new_arena(paw_Env *P, size_t size)
{
    Arena *a = pawM_new_flex(P, Arena, size, 1);
    memset(a->data, 0, size);
    a->size = size;
    return a;
}

Node *pawK_add_node_aux(Lex *lex, unsigned kind, size_t size, size_t align)
{
    paw_Env *P = lex->P;
    Tree *ast = lex->ast;
    Arena *a = ast->arena;
    const size_t base = (a->used + align - 1) & ~(align - 1);
    if (base + size > a->size) {
        if (base + size > (SIZE_MAX / 2) - 1) {
            pawM_error(P); // sanity check
        }
        const size_t nbytes = (base + size + 1) * 2;
        Arena *anew = new_arena(P, nbytes);
        anew->prev = a;
        a = ast->arena = anew;
    }
    a->used = base + size;
    Node *node = (Node *)&a->data[base];
    node->line = lex->lastline;
    node->kind = kind;
    return node;
}

void pawK_nvec_free(paw_Env *P, NodeVec *nvec)
{
    pawM_free_vec(P, nvec->nodes, nvec->alloc);
}

Node **pawK_nvec_add(paw_Env *P, NodeVec *nvec)
{
    pawM_grow(P, nvec->nodes, nvec->size, nvec->alloc);
    return &nvec->nodes[nvec->size++];
}

#define FIRST_ARENA_SIZE 512

Tree *pawK_new_ast(paw_Env *P)
{
    Tree *tree = pawM_new(P, Tree);
    tree->arena = new_arena(P, FIRST_ARENA_SIZE);
    return tree;
}

void pawK_free_ast(paw_Env *P, Tree *ast)
{
    // Free the NodeVec::nodes allocations.
    for (VecList *v = ast->vecs; v;) {
        VecList *prev = v->prev;
        NodeVec *nv = v->nvec;
        pawM_free_vec(P, nv->nodes, nv->alloc);
        v = prev;
    }
    // Free the list of arenas backing the AST.
    for (Arena *a = ast->arena; a;) {
        Arena *prev = a->prev;
        pawM_free_flex(P, a, a->size, 1);
        a = prev;
    }
    pawM_free(P, ast);
}

