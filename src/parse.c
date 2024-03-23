// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "parse.h"
#include "aux.h"
#include "call.h"
#include "env.h"
#include "gc.h"
#include "lex.h"
#include "map.h"
#include "mem.h"
#include "opcode.h"
#include "paw.h"
#include "str.h"
#include "util.h"
#include "value.h"
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define ctx(lex) (lex)->P
#define is_toplevel(lex) (!(lex)->fn->caller)
#define is_global(lex) (is_toplevel(lex) && (lex)->fn->blk->outer == NULL)
#define scan_string(lex, s) pawX_scan_string(lex, s, strlen(s))

typedef enum {
    NOT_UNOP,

    UN_LEN,  // #
    UN_NEG,  // -
    UN_NOT,  // !
    UN_BNOT, // ~

    NUNOPS
} UnOp;

typedef enum {
    NOT_BINOP,

    BIN_ADD,      // +
    BIN_SUB,      // -
    BIN_MUL,      // *
    BIN_DIV,      // /
    BIN_IDIV,     // //
    BIN_MOD,      // %
    BIN_CONCAT,   // ++
    BIN_EQ,       // ==
    BIN_NE,       // !=
    BIN_LE,       // <=
    BIN_GE,       // >=
    BIN_LT,       // <
    BIN_GT,       // >
    BIN_AND,      // &&
    BIN_OR,       // ||
    BIN_SHL,      // <<
    BIN_SHR,      // >>
    BIN_BXOR,     // ^
    BIN_BAND,     // &
    BIN_BOR,      // |
    BIN_IN,       // in
    BIN_COND,     // ??::
    BIN_CHAIN,    // ?
    BIN_COALESCE, // ?:

    NBINOPS
} BinOp;

// Recursive non-terminals
static BinOp subexpression(Lex *, ExprState *, unsigned);
static BinOp expression(Lex *, ExprState *);
static void statement(Lex *);

static void limit_error(Lex *lex, const char *what, int limit)
{
    pawX_error(lex, "too many %s (limit is %d)", what, limit);
}

static void expected_symbol(Lex *lex, const char *want)
{
    pawX_error(lex, "expected %s", want);
}

static void missing_delim(Lex *lex, TokenKind want, TokenKind open, int open_line)
{
    pawX_error(lex, "expected '%c' to match '%c' on line %d",
               want, open, open_line);
}

static void delim_next(Lex *lex, TokenKind want, TokenKind open, int open_line)
{
    if (lex->t.kind != want) {
        missing_delim(lex, want, open, open_line);
    }
    pawX_next(lex);
}

static void init_expr(ExprState *e, enum ExprKind kind, int index)
{
    *e = (ExprState){.kind = kind, .index = index};
}

static void add_line(Lex *lex)
{
    FnState *fn = lex->fn;
    Proto *p = fn->proto;
    if (fn->nlines == UINT16_MAX) {
        limit_error(lex, "instructions", UINT16_MAX);
    }
    pawM_grow(ctx(lex), p->lines, fn->nlines, p->nlines);
    p->lines[fn->nlines++] = (struct LineInfo){
        .line = lex->lastline,
        .pc = fn->pc,
    };
}

static void push8(Lex *lex, int code)
{
    FnState *fn = lex->fn;
    Proto *p = fn->proto;

    // While code is being generated, the pc is used to track the number of instructions, and
    // the length field the capacity. The length is set to the final pc value before execution.
    pawM_grow(ctx(fn->lex), p->source, fn->pc, p->length);
    p->source[fn->pc] = code & 0xFF;
    ++fn->pc;
}

static void push16(Lex *lex, int code)
{
    push8(lex, code & 0xFF);
    push8(lex, code >> 8);
}

static void emit(Lex *lex, int code)
{
    add_line(lex);
    push8(lex, code);
}

static void emit_arg(Lex *lex, OpCode code, int tag)
{
    add_line(lex);
    push8(lex, code);
    push8(lex, tag);
}

static void emit_arg2(Lex *lex, OpCode code, int tag)
{
    add_line(lex);
    push8(lex, code);
    push16(lex, tag);
}

static int add_constant(Lex *lex, Value v)
{
    FnState *fn = lex->fn;
    Proto *p = fn->proto;

    if (fn->nk == UINT16_MAX) {
        limit_error(lex, "constants", UINT16_MAX);
    } else if (fn->nk == p->nk) {
        // 'fn->nk' only ever increases by 1, so this will always give us
        // enough memory.
        pawM_grow(ctx(lex), p->k, fn->nk, p->nk);
        for (int i = fn->nk + 1; i < p->nk; ++i) {
            pawV_set_null(&p->k[i]); // clear for GC
        }
    }
    p->k[fn->nk] = v;
    return fn->nk++;
}

static int add_proto(Lex *lex, String *name, Proto **pp)
{
    FnState *fn = lex->fn;
    Proto *p = fn->proto;
    if (fn->nproto == UINT16_MAX) {
        limit_error(lex, "functions", UINT16_MAX);
    } else if (fn->nproto == p->nproto) {
        pawM_grow(ctx(lex), p->p, fn->nproto, p->nproto);
        for (int i = fn->nproto; i < p->nproto; ++i) {
            p->p[i] = NULL; // clear for GC (including current)
        }
    }
    Proto *callee = pawV_new_proto(ctx(lex));
    callee->name = name;

    const int id = fn->nproto++;
    p->p[id] = callee;
    *pp = callee;
    return id;
}

static int add_name(Lex *lex, String *name)
{
    return add_constant(lex, obj2v(name));
}

static int emit_int(Lex *lex, Value v)
{
    paw_assert(pawV_is_int(v) || pawV_is_bigint(v));
    const int location = add_constant(lex, v);
    emit_arg2(lex, OP_PUSHCONST, location);
    return location;
}

static int emit_float(Lex *lex, paw_Float f)
{
    Value v;
    pawV_set_float(&v, f);
    const int location = add_constant(lex, v);
    emit_arg2(lex, OP_PUSHCONST, location);
    return location;
}

static int emit_string(Lex *lex, String *s)
{
    const Value v = obj2v(s);
    const int location = add_constant(lex, v);
    emit_arg2(lex, OP_PUSHCONST, location);
    return location;
}

#define JUMP_PLACEHOLDER 0xFFFF

static int emit_jump(Lex *lex, OpCode op)
{
    emit_arg2(lex, op, JUMP_PLACEHOLDER);
    return lex->fn->pc - 2;
}

static void patch_jump(Lex *lex, int from, int to)
{
    FnState *fn = lex->fn;
    // NOTE: '- 2' is to account for the 16-bit opcode argument. When a jump forward
    //       is executed, the PC is already past these 2 bytes.
    int jump = to - from - 2;
    if (jump > JUMP_MAX) {
        limit_error(lex, "instructions to jump", JUMP_MAX);
    }
    jump = encode_jump_over(jump);

    Proto *p = fn->proto;
    p->source[from] = jump & 0xFF;
    p->source[from + 1] = jump >> 8;
}

static void patch_here(Lex *lex, int from)
{
    patch_jump(lex, from, lex->fn->pc);
}

static void emit_loop(Lex *lex, int op, int to)
{
    emit(lex, op);

    FnState *fn = lex->fn;
    // NOTE: '+ 2' to jump over the 2 opcode argument bytes. See patch_jump().
    int jump = fn->pc - to + 2;
    if (jump > JUMP_MAX) {
        limit_error(lex, "instructions in loop", JUMP_MAX);
    }
    jump = encode_jump_back(jump);

    push8(lex, jump & 0xFF);
    push8(lex, jump >> 8);
}

static void emit_closure(Lex *lex, Proto *p, int id)
{
    Value v;
    pawV_set_proto(&v, p);
    emit_arg2(lex, OP_CLOSURE, id);
    for (int i = 0; i < p->nup; ++i) {
        struct UpValueInfo u = p->u[i]; // encode upvalue info
        push16(lex, u.index | (u.is_local ? UPVALUE_LOCAL : 0));
    }
}

static void add_label(Lex *lex, LabelKind kind)
{
    FnState *fn = lex->fn;
    LabelList *ll = &lex->pm->ll;
    pawM_grow(ctx(lex), ll->values, ll->length, ll->capacity);
    ll->values[ll->length] = (Label){
        .kind = kind,
        .line = lex->line,
        .level = fn->proto->ndebug,
        .pc = emit_jump(lex, OP_JUMP),
    };
    ++ll->length;
}

static void adjust_labels(FnState *fn, BlkState *blk)
{
    Lex *lex = fn->lex;
    LabelList *ll = &lex->pm->ll;
    for (int i = blk->label0; i < ll->length; ++i) {
        Label *lb = &ll->values[i];
        lb->level = blk->level;
    }
}

static void remove_label(LabelList *ll, int index)
{
    paw_assert(ll->length > 0);
    for (int i = index; i < ll->length - 1; ++i) {
        ll->values[i] = ll->values[i + 1];
    }
    --ll->length;
}

static void adjust_from(FnState *fn, LabelKind kind)
{
    Lex *lex = fn->lex;
    BlkState *blk = fn->blk;
    LabelList *ll = &lex->pm->ll;
    for (int i = blk->label0; i < ll->length;) {
        Label *lb = &ll->values[i];
        if (lb->kind == kind) {
            patch_here(lex, lb->pc);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static void adjust_to(FnState *fn, LabelKind kind, int to)
{
    Lex *lex = fn->lex;
    Proto *p = fn->proto;
    BlkState *blk = fn->blk;
    LabelList *ll = &lex->pm->ll;
    for (int i = blk->label0; i < ll->length;) {
        Label *lb = &ll->values[i];
        if (lb->kind == kind) {
            const int jump = encode_jump_back(lb->pc - to + 2);
            p->source[lb->pc + 1] = jump & 0xFF;
            p->source[lb->pc + 2] = jump >> 8;
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

#define discharge(e) discharge_var(lex->fn, e)
#define discard(e) (e->kind = EXPR_STACK)

static void discharge_var(FnState *fn, ExprState *e)
{
    Lex *lex = fn->lex;
    switch (e->kind) {
        case EXPR_NULL:
            emit(lex, OP_PUSHNULL);
            break;
        case EXPR_TRUE:
            emit(lex, OP_PUSHTRUE);
            break;
        case EXPR_FALSE:
            emit(lex, OP_PUSHFALSE);
            break;
        case EXPR_FLOAT:
            emit_float(lex, e->f);
            break;
        case EXPR_INTEGER:
            emit_int(lex, e->v);
            break;
        case EXPR_STRING:
            emit_string(lex, e->s);
            break;
        case EXPR_ARRAY:
            emit_arg2(lex, OP_NEWARRAY, e->index /* a.length() */);
            break;
        case EXPR_MAP:
            emit_arg2(lex, OP_NEWMAP, e->index /* m.length() */);
            break;
        case EXPR_GLOBAL:
            emit_arg2(lex, OP_GETGLOBAL, e->index);
            break;
        case EXPR_LOCAL:
            emit_arg2(lex, OP_GETLOCAL, e->index);
            break;
        case EXPR_UPVALUE:
            emit_arg2(lex, OP_GETUPVALUE, e->index);
            break;
        case EXPR_ITEM:
            emit(lex, OP_GETITEM);
            break;
        case EXPR_ATTR:
            emit(lex, OP_GETATTR);
            break;
        default:
            break;
    }
    discard(e);
}

static void assign_var(FnState *fn, ExprState *e)
{
    Lex *lex = fn->lex;
    switch (e->kind) {
        case EXPR_GLOBAL:
            emit_arg2(lex, OP_SETGLOBAL, e->index);
            break;
        case EXPR_LOCAL:
            emit_arg2(lex, OP_SETLOCAL, e->index);
            break;
        case EXPR_UPVALUE:
            emit_arg2(lex, OP_SETUPVALUE, e->index);
            break;
        case EXPR_ITEM:
            emit(lex, OP_SETITEM);
            break;
        case EXPR_ATTR:
            emit(lex, OP_SETATTR);
            break;
        default:
            pawX_error(lex, "assignment to call expression");
    }
    discard(e);
}

// Find an active local variable with the given 'name'
// Only consider locals that have been brought into scope, using begin_local_scope().
static int resolve_local(FnState *fn, String *name)
{
    ParseMemory *pm = fn->lex->pm;
    for (int i = fn->level - 1; i >= fn->base; --i) {
        const VarState vs = pm->vars[i];
        if (pawS_eq(name, vs.name)) {
            return i - fn->base;
        }
    }
    return -1;
}

static int add_upvalue(FnState *fn, String *name, int index, paw_Bool is_local)
{
    Proto *p = fn->proto;

    for (int i = 0; i < fn->nup; ++i) {
        struct UpValueInfo up = p->u[i];
        if (up.index == index && up.is_local == is_local) {
            return i;
        }
    }
    if (fn->nup == UPVALUE_MAX) {
        limit_error(fn->lex, "upvalues", UPVALUE_MAX);
    } else if (fn->nup == p->nup) {
        pawM_grow(ctx(fn->lex), p->u, fn->nup, p->nup);
        for (int i = fn->nup + 1; i < p->nup; ++i) {
            p->u[i].name = NULL; // Clear for GC
        }
    }
    p->u[fn->nup] = (struct UpValueInfo){
        .is_local = is_local,
        .index = index,
        .name = name,
    };
    return fn->nup++;
}

static int resolve_upvalue(FnState *fn, String *name)
{
    FnState *caller = fn->caller;
    if (!caller) {
        return -1;
    }
    // Check the caller's local variables.
    const int local = resolve_local(caller, name);
    if (local >= 0) {
        caller->proto->v[local].captured = PAW_TRUE;
        return add_upvalue(fn, name, local, PAW_TRUE);
    }

    const int upvalue = resolve_upvalue(caller, name);
    if (upvalue >= 0) {
        return add_upvalue(fn, name, upvalue, PAW_FALSE);
    }
    return -1;
}

static int add_local(Lex *lex, String *name)
{
    ParseMemory *pm = lex->pm;
    pawM_grow(ctx(lex), pm->vars, pm->vsize, pm->valloc);
    pm->vars[pm->vsize] = (VarState){
        .name = name,
    };

    // Locals are referenced by their index relative to the enclosing function's
    // 'base pointer'.
    FnState *fn = lex->fn;
    return pm->vsize++ - fn->base;
}

static void add_debug_info(Lex *lex, String *name)
{
    FnState *fn = lex->fn;
    Proto *p = fn->proto;
    if (fn->ndebug == UINT16_MAX) {
        limit_error(lex, "locals", UINT16_MAX);
    } else if (fn->ndebug == p->ndebug) {
        pawM_grow(ctx(lex), p->v, fn->ndebug, p->ndebug);
        for (int i = fn->ndebug + 1; i < p->ndebug; ++i) {
            p->v[i].name = NULL; // Clear for GC
        }
    }
    p->v[fn->ndebug] = (struct LocalInfo){
        .name = name,
        .pc0 = fn->pc,
    };
    ++fn->ndebug;
}

static void begin_local_scope(Lex *lex, int n)
{
    FnState *fn = lex->fn;
    ParseMemory *pm = lex->pm;
    for (int i = 0; i < n; ++i) {
        const int level = fn->level++;
        add_debug_info(lex, pm->vars[level].name);
    }
}

static struct LocalInfo *local_info(FnState *fn, int level)
{
    return &fn->proto->v[level - fn->base];
}

static void close_vars(FnState *fn, int level)
{
    for (int i = fn->level - 1; i >= level; --i) {
        struct LocalInfo *local = local_info(fn, i);
        if (local->captured) {
            emit(fn->lex, OP_CLOSE);
        } else {
            emit(fn->lex, OP_POP);
        }
    }
}

static void end_local_scope(FnState *fn, BlkState *blk)
{
    ParseMemory *pm = fn->lex->pm;
    for (int i = fn->level - 1; i >= blk->level; --i) {
        local_info(fn, i)->pc1 = fn->pc;
    }
    const int nvar = fn->level - blk->level;
    fn->level = blk->level;
    pm->vsize -= nvar;
}

static void init_var(Lex *lex, ExprState *e)
{
    if (is_global(lex)) {
        e->index = add_name(lex, e->name);
        e->kind = EXPR_GLOBAL;
    } else {
        e->index = add_local(lex, e->name);
        e->kind = EXPR_LOCAL;
    }
}

static void define_var(Lex *lex, ExprState *e)
{
    if (is_global(lex)) {
        emit_arg2(lex, OP_GLOBAL, e->index);
    } else {
        begin_local_scope(lex, 1);
    }
}

static const struct {
    uint8_t left;
    uint8_t right;
} kBinOpPrecedence[NBINOPS] = {
    [BIN_MUL] = {13, 13},
    [BIN_DIV] = {13, 13},
    [BIN_IDIV] = {13, 13},
    [BIN_MOD] = {13, 13},
    [BIN_ADD] = {12, 12},
    [BIN_SUB] = {12, 12},
    [BIN_CONCAT] = {11, 11},
    [BIN_SHL] = {10, 10},
    [BIN_SHR] = {10, 10},
    [BIN_BAND] = {9, 9},
    [BIN_BXOR] = {8, 8},
    [BIN_BOR] = {7, 7},
    [BIN_IN] = {6, 6},
    [BIN_LT] = {6, 6},
    [BIN_LE] = {6, 6},
    [BIN_GT] = {6, 6},
    [BIN_GE] = {6, 6},
    [BIN_EQ] = {5, 5},
    [BIN_NE] = {5, 5},
    [BIN_AND] = {4, 4},
    [BIN_OR] = {3, 3},
    [BIN_COALESCE] = {2, 2},
    [BIN_COND] = {1, 0}, // Right-associative
};

static const uint8_t kUnOpPrecedence = 14;

static unsigned left_prec(BinOp op)
{
    return kBinOpPrecedence[op].left;
}

static unsigned right_prec(BinOp op)
{
    return kBinOpPrecedence[op].right;
}

static UnOp get_unop(TokenKind kind)
{
    switch (kind) {
        case '#':
            return UN_LEN;
        case '-':
            return UN_NEG;
        case '!':
            return UN_NOT;
        case '~':
            return UN_BNOT;
        default:
            return NOT_UNOP;
    }
}

static BinOp get_binop(TokenKind kind)
{
    switch (kind) {
        case '+':
            return BIN_ADD;
        case '-':
            return BIN_SUB;
        case '*':
            return BIN_MUL;
        case '/':
            return BIN_DIV;
        case '%':
            return BIN_MOD;
        case '<':
            return BIN_LT;
        case '>':
            return BIN_GT;
        case '^':
            return BIN_BXOR;
        case '&':
            return BIN_BAND;
        case '|':
            return BIN_BOR;
        case TK_IN:
            return BIN_IN;
        case TK_QUESTION2:
            return BIN_COND;
        case TK_ELVIS:
            return BIN_COALESCE;
        case TK_PLUS2:
            return BIN_CONCAT;
        case TK_SLASH2:
            return BIN_IDIV;
        case TK_EQUALS2:
            return BIN_EQ;
        case TK_LESS2:
            return BIN_SHL;
        case TK_GREATER2:
            return BIN_SHR;
        case TK_AMPER2:
            return BIN_AND;
        case TK_PIPE2:
            return BIN_OR;
        case TK_BANG_EQ:
            return BIN_NE;
        case TK_LESS_EQ:
            return BIN_LE;
        case TK_GREATER_EQ:
            return BIN_GE;
        default:
            return NOT_BINOP;
    }
}

static void emit_unop(Lex *lex, UnOp unop)
{
    switch (unop) {
        case UN_LEN:
            emit(lex, OP_LEN);
            break;
        case UN_NEG:
            emit(lex, OP_NEG);
            break;
        case UN_NOT:
            emit(lex, OP_NOT);
            break;
        default: // UN_BNOT
            emit(lex, OP_BNOT);
            break;
    }
}

static void emit_binop(Lex *lex, BinOp binop)
{
    switch (binop) {
        case BIN_ADD:
            emit(lex, OP_ADD);
            break;
        case BIN_SUB:
            emit(lex, OP_SUB);
            break;
        case BIN_MUL:
            emit(lex, OP_MUL);
            break;
        case BIN_DIV:
            emit(lex, OP_DIV);
            break;
        case BIN_IDIV:
            emit(lex, OP_IDIV);
            break;
        case BIN_MOD:
            emit(lex, OP_MOD);
            break;
        case BIN_CONCAT:
            emit(lex, OP_CONCAT);
            break;
        case BIN_EQ:
            emit(lex, OP_EQ);
            break;
        case BIN_NE:
            emit(lex, OP_EQ);
            emit(lex, OP_NOT);
            break;
        case BIN_LE:
            emit(lex, OP_LE);
            break;
        case BIN_GE:
            emit(lex, OP_LT);
            emit(lex, OP_NOT);
            break;
        case BIN_LT:
            emit(lex, OP_LT);
            break;
        case BIN_GT:
            emit(lex, OP_LE);
            emit(lex, OP_NOT);
            break;
        case BIN_SHL:
            emit(lex, OP_SHL);
            break;
        case BIN_SHR:
            emit(lex, OP_SHR);
            break;
        case BIN_BXOR:
            emit(lex, OP_BXOR);
            break;
        case BIN_BAND:
            emit(lex, OP_BAND);
            break;
        case BIN_BOR:
            emit(lex, OP_BOR);
            break;
        case BIN_IN:
            emit(lex, OP_IN);
            break;
        default:
            break;
    }
}

static void skip(Lex *lex)
{
    pawX_next(lex);
}

static void check(Lex *lex, TokenKind want)
{
    if (lex->t.kind != want) {
        pawX_error(lex, "unexpected symbol");
    }
}

static void check_next(Lex *lex, TokenKind want)
{
    check(lex, want);
    skip(lex);
}

static paw_Bool test(Lex *lex, TokenKind kind)
{
    return lex->t.kind == kind;
}

static paw_Bool test_next(Lex *lex, TokenKind kind)
{
    if (test(lex, kind)) {
        skip(lex);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

// Eat a semicolon, if one exists
static void semicolon(Lex *lex)
{
    test_next(lex, ';');
}

static void leave_block(FnState *fn)
{
    BlkState *blk = fn->blk;
    if (blk->is_loop) {
        adjust_from(fn, LBREAK);
    }
    close_vars(fn, blk->level);
    end_local_scope(fn, blk);
    fn->blk = blk->outer;
    if (blk->outer) {
        adjust_labels(fn, blk);
    }
}

static void enter_block(FnState *fn, BlkState *blk, paw_Bool is_loop)
{
    Lex *lex = fn->lex;
    *blk = (BlkState){
        .is_loop = is_loop,
        .outer = fn->blk,
        .level = fn->level,
        .label0 = lex->pm->ll.length,
    };
    fn->blk = blk;
}

static void leave_function(Lex *lex)
{
    ParseMemory *pm = lex->pm;
    FnState *fn = lex->fn;
    BlkState *blk = fn->blk;
    Proto *p = fn->proto;

    // End the lifetime of function-scoped variables, but don't emit any pop
    // or close instructions. OP_RETURN does it for us.
    end_local_scope(fn, blk);
    paw_assert(fn->level == fn->base);
    paw_assert(blk->outer == NULL);

    if (fn->kind == FN_INIT) {
        emit_arg2(lex, OP_GETLOCAL, 0);
        emit(lex, OP_RETURN);
    } else {
        emit(lex, OP_PUSHNULL);
        emit(lex, OP_RETURN);
    }
    pawM_shrink(ctx(lex), p->source, p->length, fn->pc);
    p->length = fn->pc;
    pawM_shrink(ctx(lex), p->lines, p->nlines, fn->nlines);
    p->nlines = fn->nlines;
    pawM_shrink(ctx(lex), p->p, p->nproto, fn->nproto);
    p->nproto = fn->nproto;
    pawM_shrink(ctx(lex), p->v, p->ndebug, fn->ndebug);
    p->ndebug = fn->ndebug;
    pawM_shrink(ctx(lex), p->u, p->nup, fn->nup);
    p->nup = fn->nup;
    pawM_shrink(ctx(lex), p->k, p->nk, fn->nk);
    p->nk = fn->nk;

    pm->vsize = fn->base;
    lex->fn = fn->caller;
    check_gc(ctx(lex));
}

static String *context_name(const FnState *fn, FnKind kind)
{
    if (FN_HAS_SELF(kind)) {
        return pawV_get_string(pawE_cstr(ctx(fn->lex), CSTR_SELF));
    }
    return fn->proto->name;
}

static void enter_function(Lex *lex, FnState *fn, BlkState *blk, FnKind kind)
{
    *fn = (FnState){
        .proto = fn->proto, // Keep value
        .base = lex->pm->vsize,
        .level = lex->pm->vsize,
        .caller = lex->fn,
        .kind = kind,
        .lex = lex,
    };
    lex->fn = fn;

    // Enter the function body.
    enter_block(fn, blk, PAW_FALSE);

    // Create the context variable in slot 0. For VCLOSURE, this slot holds the closure
    // object being called. For VMETHOD, it holds the class instance that the method is
    // being called on, i.e. the implicit 'self' parameter.
    add_local(lex, context_name(fn, kind));
    begin_local_scope(lex, 1);
}

static BinOp expr(Lex *lex)
{
    ExprState e;
    const BinOp op = expression(lex, &e);
    discharge(&e);
    return op;
}

static void check_not_kw(Lex *lex, const String *str)
{
    if (str_is_keyword(str)) {
        char buffer[32] = {0}; // long enough for any keyword
        memcpy(buffer, str->text, str->length);
        pawX_error(lex, "name '%s' is reserved", buffer);
    }
}

static String *usable_name(Lex *lex)
{
    check(lex, TK_NAME);
    String *name = pawV_get_string(lex->t.value);
    skip(lex);

    check_not_kw(lex, name);
    return name;
}

static void find_var(Lex *lex, ExprState *e, String *name)
{
    e->name = name;
    FnState *fn = lex->fn;
    int arg = resolve_local(fn, e->name);
    if (arg >= 0) {
        init_expr(e, EXPR_LOCAL, arg);
    } else if (0 <= (arg = resolve_upvalue(fn, e->name))) {
        init_expr(e, EXPR_UPVALUE, arg);
    } else {
        arg = add_constant(lex, obj2v(e->name));
        init_expr(e, EXPR_GLOBAL, arg);
    }
}

static int call_parameters(Lex *lex)
{
    const int line = lex->line;
    skip(lex); // '(' token

    int argc = 0;
    if (!test_next(lex, ')')) {
        argc = 1;
        expr(lex);
        while (test_next(lex, ',')) {
            if (argc == UINT8_MAX) {
                limit_error(lex, "function parameters", UINT8_MAX);
            }
            expr(lex);
            ++argc;
        }
        delim_next(lex, ')', '(', line);
    }
    return argc;
}

static int const_name(Lex *lex, ExprState *e)
{
    check(lex, TK_NAME);
    String *s = pawV_get_string(lex->t.value);
    skip(lex); // skip name

    const int tag = add_name(lex, s);
    init_expr(e, EXPR_STRING, tag);
    e->s = s;
    return tag;
}

static void code_invoke(Lex *lex, Op op, ExprState *e)
{
    const int name = add_name(lex, e->s);
    discard(e);

    const int argc = call_parameters(lex);
    emit_arg2(lex, op, name);
    emit(lex, argc);
}

static void push_special(Lex *lex, unsigned ctag)
{
    ExprState e;
    if (ctag == CSTR_SELF) {
        // 'self' is always in slot 0
        init_expr(&e, EXPR_LOCAL, 0);
    } else {
        // 'super' is an upvalue
        const Value v = pawE_cstr(lex->P, ctag);
        find_var(lex, &e, pawV_get_string(v));
    }
    discharge(&e);
}

static void push_named(Lex *lex, String *name)
{
    ExprState e;
    find_var(lex, &e, name);
    discharge(&e);
}

// Parse a variable expression that isn't a declaration
static void varexpr(Lex *lex, ExprState *e)
{
    find_var(lex, e, usable_name(lex));
}

static void superexpr(Lex *lex, ExprState *e)
{
    if (!lex->cls) {
        pawX_error(lex, "'super' used outside class body");
    } else if (!lex->cls->has_super) {
        pawX_error(lex, "class has no superclass");
    }

    skip(lex); // 'super' token
    check_next(lex, '.');

    const int name = const_name(lex, e);
    if (test(lex, '(')) {
        const int argc = call_parameters(lex);
        push_special(lex, CSTR_SELF);
        push_special(lex, CSTR_SUPER);
        emit_arg2(lex, OP_INVOKESUPER, name);
        emit(lex, argc);
        e->kind = EXPR_CALL;
    } else {
        push_special(lex, CSTR_SELF);
        push_special(lex, CSTR_SUPER);
        emit_arg2(lex, OP_GETSUPER, name);
        e->kind = EXPR_ATTR;
    }
}

static void parse_variable(Lex *lex, ExprState *e)
{
    e->name = usable_name(lex);
    init_var(lex, e);
}

static void new_variable(Lex *lex, String *name, ExprKind kind)
{
    // TODO: Ignores .kind
    init_var(lex, &(ExprState){.name = name, .kind = kind});
}

static void new_local_var(Lex *lex, String *name)
{
    new_variable(lex, name, EXPR_LOCAL);
}

static void new_local_literal(Lex *lex, const char *name)
{
    add_local(lex, scan_string(lex, name));
}

static void unop_expr(Lex *lex, UnOp op, ExprState *e)
{
    skip(lex); // Unary operator token
    subexpression(lex, e, kUnOpPrecedence);

    // Emit code for the unary operator.
    discharge(e);
    emit_unop(lex, op);
}

static void paren_expr(Lex *lex, ExprState *e)
{
    const int line = lex->line;
    skip(lex); // '(' token
    expression(lex, e);
    delim_next(lex, ')', '(', line);
}

static paw_Bool laststmt(Lex *lex)
{
    return test(lex, TK_RETURN);
}

static paw_Bool end_of_block(Lex *lex)
{
    return test(lex, '}') ||  // found end of block
           test(lex, TK_END); // truncated block
}

static void stmtlist(Lex *lex)
{
    while (!end_of_block(lex)) {
        if (laststmt(lex)) {
            statement(lex);
            break;
        }
        statement(lex);
    }
}

static void array_expr(Lex *lex, ExprState *e)
{
    const int line = lex->line;
    skip(lex); // '[' token

    int n = 0;
    do {
        if (test(lex, ']')) {
            break;
        } else if (n == UINT16_MAX) {
            limit_error(lex, "array elements", UINT16_MAX);
        }
        expr(lex);
        ++n;
    } while (test_next(lex, ','));
    delim_next(lex, ']', '[', line);
    init_expr(e, EXPR_ARRAY, n);
}

static int itemlist(Lex *lex)
{
    const int line = lex->line;
    skip(lex); // '{' token

    int n = 0;
    do {
        if (test(lex, '}')) {
            break;
        } else if (n == UINT16_MAX) {
            limit_error(lex, "map items", UINT16_MAX);
        }
        expr(lex);
        check_next(lex, ':');
        expr(lex);
        ++n;
    } while (test_next(lex, ','));
    delim_next(lex, '}', '{', line);
    return n;
}

static void map_expr(Lex *lex, ExprState *e)
{
    const int n = itemlist(lex);
    init_expr(e, EXPR_MAP, n);
}

static void index_expr(Lex *lex, ExprState *e)
{
    discharge(e);

    const int line = lex->line;
    skip(lex); // '[' token

    expr(lex);
    delim_next(lex, ']', '[', line);
    e->kind = EXPR_ITEM;
}

static void member_expr(Lex *lex, ExprState *e)
{
    discharge(e);
    skip(lex); // '.' token

    ExprState e2;
    const_name(lex, &e2);

    if (test(lex, '(')) {
        code_invoke(lex, OP_INVOKE, &e2);
        e->kind = EXPR_CALL;
    } else {
        discharge(&e2);
        e->kind = EXPR_ATTR;
    }
}

static void call_expr(Lex *lex, ExprState *e)
{
    discharge(e);
    const int argc = call_parameters(lex);
    emit_arg(lex, OP_CALL, argc);
    init_expr(e, EXPR_CALL, -1);
}

static void chain_expr(Lex *lex, ExprState *e)
{
    discharge(e);
    skip(lex); // '?' token
    const int else_jump = emit_jump(lex, OP_JUMPNULL);
    const int then_jump = emit_jump(lex, OP_JUMP);
    patch_here(lex, else_jump);
    emit(lex, OP_PUSHNULL);
    emit(lex, OP_RETURN);
    patch_here(lex, then_jump);
}

static void fn_parameters(Lex *lex)
{
    FnState *fn = lex->fn;
    Proto *p = fn->proto;
    const int line = lex->line;
    check_next(lex, '(');

    int argc = 0;
    if (!test_next(lex, ')')) {
        int vararg = 0;
        do {
            if (argc == UINT8_MAX) {
                limit_error(lex, "function parameters", UINT8_MAX);
            }
            switch (lex->t.kind) {
                case TK_NAME:
                    new_local_var(lex, usable_name(lex));
                    ++argc;
                    break;
                case TK_DOT3:
                    vararg = 1;
                    skip(lex);
                    break;
                default:
                    expected_symbol(lex, "name");
            }
        } while (!vararg && test_next(lex, ','));
        delim_next(lex, ')', '(', line);
        begin_local_scope(lex, argc);
        if (vararg) {
            // All parameters passed to this function that occur at or beyond the
            // '...' will be stored in an array called 'argv'.
            emit_arg(lex, OP_VARARG, argc);
            new_local_literal(lex, "argv");
            begin_local_scope(lex, 1);
            p->is_va = PAW_TRUE;
        }
    }
    p->argc = argc;
}

static void body(Lex *lex)
{
    const int line = lex->line;
    check_next(lex, '{');
    stmtlist(lex);
    delim_next(lex, '}', '{', line);
}

static void function(Lex *lex, String *name, FnKind kind)
{
    BlkState blk;
    FnState fn = {.name = name};
    const int id = add_proto(lex, name, &fn.proto);

    enter_function(lex, &fn, &blk, kind);
    fn_parameters(lex);
    body(lex);
    leave_function(lex);

    emit_closure(lex, fn.proto, id);
}

static void fn_expr(Lex *lex)
{
    skip(lex); // 'fn' token

    String *name = scan_string(lex, "(anonymous fn)");
    function(lex, name, FN_FUNCTION);
}

static void primary_expr(Lex *lex, ExprState *e)
{
    switch (lex->t.kind) {
        case '(':
            paren_expr(lex, e);
            break;
        case TK_NAME:
            varexpr(lex, e);
            break;
        case TK_SUPER:
            superexpr(lex, e);
            break;
        case '[':
            array_expr(lex, e);
            break;
        case '{':
            map_expr(lex, e);
            break;
        case TK_STRING:
            init_expr(e, EXPR_STRING, 0);
            e->s = pawV_get_string(lex->t.value);
            skip(lex);
            break;
        default:
            expected_symbol(lex, "name or '('");
    }
}

static void suffixed_expr(Lex *lex, ExprState *e)
{
    primary_expr(lex, e);
    for (;;) {
        switch (lex->t.kind) {
            case '.':
                member_expr(lex, e);
                break;
            case '[':
                index_expr(lex, e);
                break;
            case '(':
                call_expr(lex, e);
                break;
            case '?':
                chain_expr(lex, e);
                break;
            default:
                return;
        }
    }
}

static void simple_expr(Lex *lex, ExprState *e)
{
    switch (lex->t.kind) {
        case TK_NULL:
            init_expr(e, EXPR_NULL, 0);
            break;
        case TK_TRUE:
            init_expr(e, EXPR_TRUE, 0);
            break;
        case TK_FALSE:
            init_expr(e, EXPR_FALSE, 0);
            break;
        case TK_INTEGER:
            init_expr(e, EXPR_INTEGER, 0);
            e->v = lex->t.value;
            break;
        case TK_FLOAT:
            init_expr(e, EXPR_FLOAT, 0);
            e->f = pawV_get_float(lex->t.value);
            break;
        case TK_FN:
            fn_expr(lex);
            discard(e);
            return;
        default:
            suffixed_expr(lex, e);
            return;
    }
    skip(lex); // skip literal
}

static BinOp binop_expr(Lex *lex, BinOp op, ExprState *e)
{
    discharge(e);
    ExprState e2;

    skip(lex); // binary operator token
    const BinOp op2 = subexpression(lex, &e2, right_prec(op));

    // Emit code for the operator.
    discharge(&e2);
    emit_binop(lex, op);
    return op2;
}

static BinOp and_expr(Lex *lex, ExprState *e)
{
    discharge(e);
    skip(lex); // '&&' token

    const int jump = emit_jump(lex, OP_JUMPFALSE);
    emit(lex, OP_POP);

    ExprState e2;
    const BinOp op2 = subexpression(lex, &e2, right_prec(BIN_AND));
    discharge(&e2);

    patch_here(lex, jump);
    return op2;
}

static BinOp or_expr(Lex *lex, ExprState *e)
{
    discharge(e);
    skip(lex); // '||' token

    const int else_jump = emit_jump(lex, OP_JUMPFALSE);
    const int end_jump = emit_jump(lex, OP_JUMP);

    patch_here(lex, else_jump);
    emit(lex, OP_POP);

    ExprState e2;
    const BinOp op2 = subexpression(lex, &e2, right_prec(BIN_OR));
    discharge(&e2);

    patch_here(lex, end_jump);
    return op2;
}

static BinOp coalesce_expr(Lex *lex, ExprState *e)
{
    discharge(e);
    skip(lex); // '?:' token

    const int else_jump = emit_jump(lex, OP_JUMPNULL);
    const int then_jump = emit_jump(lex, OP_JUMP);

    patch_here(lex, else_jump);
    emit(lex, OP_POP);
    const BinOp op = expr(lex);
    patch_here(lex, then_jump);
    return op;
}

static BinOp cond_expr(Lex *lex, ExprState *e)
{
    discharge(e);
    skip(lex); // '??' token

    const int else_jump = emit_jump(lex, OP_JUMPFALSE);
    emit(lex, OP_POP);

    ExprState left;
    subexpression(lex, &left, right_prec(BIN_COND));
    discharge(&left);

    const int then_jump = emit_jump(lex, OP_JUMP);
    check_next(lex, TK_COLON2);
    patch_here(lex, else_jump);
    emit(lex, OP_POP);

    const BinOp op = expr(lex);
    patch_here(lex, then_jump);
    return op;
}

static BinOp infix_expr(Lex *lex, unsigned op, ExprState *e)
{
    switch (op) {
        case BIN_AND:
            return and_expr(lex, e);
        case BIN_OR:
            return or_expr(lex, e);
        case BIN_COND:
            return cond_expr(lex, e);
        case BIN_COALESCE:
            return coalesce_expr(lex, e);
        default:
            return binop_expr(lex, op, e);
    }
}

static BinOp subexpression(Lex *lex, ExprState *e, unsigned prec)
{
    const UnOp unop = get_unop(lex->t.kind);
    if (unop != NOT_UNOP) {
        unop_expr(lex, unop, e);
    } else {
        simple_expr(lex, e);
    }
    BinOp binop = get_binop(lex->t.kind);
    while (binop != NOT_BINOP && prec < left_prec(binop)) {
        binop = infix_expr(lex, binop, e);
    }
    return binop;
}

static BinOp expression(Lex *lex, ExprState *e)
{
    return subexpression(lex, e, 0);
}

static void block(Lex *lex)
{
    BlkState blk;
    enter_block(lex->fn, &blk, PAW_FALSE);
    body(lex);
    leave_block(lex->fn);
}

static void if_stmt(Lex *lex)
{
    skip(lex); // 'if' token
    expr(lex); // conditional

    const int then_jump = emit_jump(lex, OP_JUMPFALSE);
    emit(lex, OP_POP);

    block(lex); // 'then' block

    const int else_jump = emit_jump(lex, OP_JUMP);
    patch_here(lex, then_jump);
    emit(lex, OP_POP);

    if (test_next(lex, TK_ELSE)) {
        if (test(lex, TK_IF)) {
            // Put the rest of the chain in the else branch. This transformation looks
            // like 'if a {} else if b {} else {}' -> 'if a {} else {if b {} else {}}'.
            if_stmt(lex);
        } else {
            block(lex);
        }
    }
    patch_here(lex, else_jump);
}

static void let_stmt(Lex *lex)
{
    skip(lex); // 'let' token

    ExprState e;
    parse_variable(lex, &e);
    if (test_next(lex, '=')) {
        expr(lex);
    } else {
        emit(lex, OP_PUSHNULL);
    }
    semicolon(lex);
    define_var(lex, &e);
}

static void expression_stmt(Lex *lex)
{
    ExprState e;
    suffixed_expr(lex, &e);

    if (test_next(lex, '=')) {
        expr(lex); // right-hand side
        assign_var(lex->fn, &e);
    } else if (e.kind != EXPR_CALL) {
        pawX_error(lex, "invalid statement");
    } else {
        // Pop the unused return value.
        emit(lex, OP_POP);
    }
    semicolon(lex);
}

static void forbody(Lex *lex, int init, int loop)
{
    BlkState blk;
    FnState *fn = lex->fn;

    // Emit OP_FOR*0, which either pushes the loop variables, or jumps over
    // the loop.
    const int skip = emit_jump(lex, init);
    const int top = fn->pc;

    // Enter a block for the last control variable, i.e. the counter. This
    // variable can be captured.
    enter_block(fn, &blk, PAW_FALSE);
    begin_local_scope(lex, 1);

    block(lex);
    leave_block(fn);

    // Continue statements jump here, right before the loop instruction.
    adjust_from(fn, LCONTINUE);
    emit_loop(lex, loop, top);
    patch_here(lex, skip);
}

static void fornum(Lex *lex, String *ivar)
{
    // Create the control variables.
    new_local_literal(lex, "(for begin)");
    new_local_literal(lex, "(for end)");
    new_local_literal(lex, "(for step)");
    new_variable(lex, ivar, EXPR_CONST);

    // Parse the loop bounds ('begin', 'end', and 'step' expressions).
    expr(lex);
    check_next(lex, ',');
    expr(lex);
    if (test_next(lex, ',')) {
        expr(lex);
    } else {
        // Step defaults to 1.
        Value v;
        pawV_set_int(&v, 1);
        emit_int(lex, v);
    }

    begin_local_scope(lex, 3);
    forbody(lex, OP_FORNUM0, OP_FORNUM);
}

static void forin(Lex *lex, String *ivar)
{
    new_local_literal(lex, "(for object)");
    new_local_literal(lex, "(for iterator)");
    new_variable(lex, ivar, EXPR_CONST);
    expr(lex); // loop target

    begin_local_scope(lex, 2);
    forbody(lex, OP_FORIN0, OP_FORIN);
}

static void for_stmt(Lex *lex)
{
    skip(lex); // 'for' token

    BlkState blk;
    FnState *fn = lex->fn;
    // Enter scope for the loop variables.
    enter_block(fn, &blk, PAW_TRUE);

    // Parse the loop variable name.
    String *ivar = usable_name(lex);
    if (test_next(lex, '=')) {
        fornum(lex, ivar);
    } else if (test_next(lex, TK_IN)) {
        forin(lex, ivar);
    } else {
        expected_symbol(lex, "'=' or 'in'"); // no return
    }
    leave_block(fn);
}

static void while_stmt(Lex *lex)
{
    BlkState blk;
    FnState *fn = lex->fn;
    enter_block(fn, &blk, PAW_TRUE);
    const int loop = fn->pc;

    skip(lex); // 'while' token
    expr(lex); // conditional

    const int jump = emit_jump(lex, OP_JUMPFALSE);
    emit(lex, OP_POP);

    block(lex);

    // Finish the loop. 'break' labels jump here, 'continue' labels back to right
    // before where the conditional expression was evaluated.
    emit_loop(lex, OP_JUMP, loop);
    adjust_to(fn, LCONTINUE, loop);
    patch_here(lex, jump);
    emit(lex, OP_POP);
    leave_block(fn);
}

static void dowhile_stmt(Lex *lex)
{
    BlkState blk;
    FnState *fn = lex->fn;
    enter_block(fn, &blk, PAW_TRUE);
    const int top = fn->pc;

    skip(lex); // 'do' token
    block(lex);
    check_next(lex, TK_WHILE);

    adjust_from(fn, LCONTINUE);
    expr(lex); // conditional

    // If the condition is false, jump over the instruction that moves control back
    // to the top of the loop.
    const int jump = emit_jump(lex, OP_JUMPFALSE);
    emit(lex, OP_POP);

    emit_loop(lex, OP_JUMP, top);
    patch_here(lex, jump);
    emit(lex, OP_POP);
    leave_block(fn);
}

static void block_stmt(Lex *lex)
{
    block(lex);
}

static void return_stmt(Lex *lex)
{
    skip(lex); // 'return' token

    if (end_of_block(lex) || test(lex, ';')) {
        emit(lex, OP_PUSHNULL); // No return value
    } else {
        expr(lex);
    }
    emit(lex, OP_RETURN);

    // NOTE: The construct 'return [expr] [`;`]' must be followed by the
    //       end of the block. This is necessary because the semicolon is
    //       optional, and we cannot easily tell if it was intended to go
    //       before or after the '[expr]' part.
    semicolon(lex);
}

static void close_until_loop(FnState *fn, BlkState **pblk)
{
    Lex *lex = fn->lex;
    BlkState *blk = *pblk;
    while (blk->outer) {
        // Emit close/pop instructions, but don't end any lifetimes. Code
        // paths that doesn't hit this label may still need those locals.
        *pblk = blk->outer;
        if ((*pblk)->is_loop) {
            close_vars(fn, blk->level);
            return;
        }
        blk = *pblk;
    }
    pawX_error(lex, "label outside loop");
}

static void labelstmt(Lex *lex, LabelKind kind)
{
    skip(lex); // 'break' or 'continue' token
    semicolon(lex);

    FnState *fn = lex->fn;
    BlkState *blk = fn->blk;
    // Pop/close locals until the loop block is reached.
    close_until_loop(fn, &blk);

    // Emit a jump, to be patched once the parser finishes the loop.
    add_label(lex, kind);
}

static void include_stmt(Lex *lex)
{
    skip(lex); // 'include' token
    semicolon(lex);
}

static void fn_stmt(Lex *lex)
{
    skip(lex); // 'fn' token

    ExprState e;
    parse_variable(lex, &e);
    function(lex, e.name, FN_FUNCTION);
    define_var(lex, &e);
}

static void method_def(Lex *lex)
{
    const String *init = pawV_get_string(pawE_cstr(ctx(lex), CSTR_INIT));
    const String *name = pawV_get_string(lex->t.value);
    FnKind kind = FN_METHOD;
    if (pawS_eq(name, init)) {
        kind = FN_INIT;
    } else {
        check(lex, TK_NAME);
    }
    ExprState e;
    e.index = add_constant(lex, lex->t.value);
    e.name = pawV_get_string(lex->t.value);
    skip(lex); // name token

    function(lex, e.name, kind);
    emit_arg2(lex, OP_NEWMETHOD, e.index);
}

static void class_body(Lex *lex)
{
    const int line = lex->line;
    check_next(lex, '{');
    while (!test(lex, '}')) {
        check(lex, TK_NAME);
        method_def(lex);
    }
    delim_next(lex, '}', '{', line);
}

static void class_stmt(Lex *lex)
{
    FnState *fn = lex->fn;

    skip(lex); // 'class' token
    check(lex, TK_NAME);

    // Parse the name manually so we can get at the constant tag. The class
    // needs to know its own name when it is created.
    ExprState e;
    String *class_name = usable_name(lex);
    const int tag = add_name(lex, class_name);
    e.name = class_name;

    // Create the class and put it in a variable. Make it visible right now
    // so that it can be referenced by the methods. Note that OP_NEWCLASS
    // also pushes the class, so we will end up with it on top of the stack
    // for OP_INHERIT and OP_METHOD to use.
    emit_arg2(lex, OP_NEWCLASS, tag);
    init_var(lex, &e);

    ClsState cls = {.outer = lex->cls};
    if (test_next(lex, ':')) {
        ExprState super;
        // push superclass
        varexpr(lex, &super);
        discharge(&super);

        // Introduce the class name after parsing the superclass, which ends
        // up making inheritance from self impossible. In the statement
        // 'class A: A {}', the second 'A' either refers to a previously-
        // declared 'A', or it does not yet exist (name error).
        define_var(lex, &e);
        // handle inheritance
        push_named(lex, class_name);
        emit(lex, OP_INHERIT);
        cls.has_super = PAW_TRUE;
    } else {
        define_var(lex, &e);
    }

    BlkState blk;
    // scope for potential 'super' variable
    enter_block(fn, &blk, PAW_FALSE);

    if (cls.has_super) {
        // Create a local variable, 'super', to reference the superclass. If
        // referenced in a method, 'super' will be captured as an upvalue.
        new_local_literal(lex, "super");
        begin_local_scope(lex, 1);
    }
    lex->cls = &cls;

    // push class
    discharge(&e);

    class_body(lex);
    leave_block(fn);   // pop or close 'super'
    emit(lex, OP_POP); // pop class
    semicolon(lex);

    lex->cls = cls.outer;
}

static void statement(Lex *lex)
{
    switch (lex->t.kind) {
        case ';':
            // empty statement
            skip(lex);
            break;
        case '{':
            block_stmt(lex);
            break;
        case TK_FN:
            fn_stmt(lex);
            break;
        case TK_CLASS:
            class_stmt(lex);
            break;
        case TK_INCLUDE:
            include_stmt(lex);
            break;
        case TK_LET:
            let_stmt(lex);
            break;
        case TK_IF:
            if_stmt(lex);
            break;
        case TK_FOR:
            for_stmt(lex);
            break;
        case TK_WHILE:
            while_stmt(lex);
            break;
        case TK_DO:
            dowhile_stmt(lex);
            break;
        case TK_RETURN:
            return_stmt(lex);
            break;
        case TK_BREAK:
            labelstmt(lex, LBREAK);
            break;
        case TK_CONTINUE:
            labelstmt(lex, LCONTINUE);
            break;
        default:
            expression_stmt(lex);
    }
}

// All paw language keywords (must be in this order, the same order as the
// keyword variants in the TokenKind enum in lex.h)
static const char *kKeywords[] = {
    "fn",
    "class",
    "super",
    "include",
    "let",
    "if",
    "else",
    "for",
    "do",
    "while",
    "break",
    "continue",
    "return",
    "in",
    "true",
    "false",
    "null",
};

static String *new_fixed_string(paw_Env *P, const char *s)
{
    String *str = pawS_new_str(P, s);
    pawG_fix_object(P, cast_object(str));
    return str;
}

void pawP_init(paw_Env *P)
{
    // Add all keywords to the interned strings table. Fix them so they are never
    // collected. Also added to the lexer string map.
    for (size_t i = 0; i < paw_countof(kKeywords); ++i) {
        const char *kw = kKeywords[i];
        String *str = new_fixed_string(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }
    for (Op op = META1; op < NOPCODES; ++op) {
        const char *name = pawT_name(op);
        String *str = new_fixed_string(P, name);
        pawV_set_string(&P->meta_keys[op2meta(op)], str);
    }
    pawV_set_string(&P->str_cache[CSTR_SELF], new_fixed_string(P, "self"));
    pawV_set_string(&P->str_cache[CSTR_INIT], new_fixed_string(P, "__init"));

    pawV_set_string(&P->str_cache[CSTR_SUPER], pawS_new_str(P, "super"));
    pawV_set_string(&P->str_cache[CSTR_TRUE], pawS_new_str(P, "true"));
    pawV_set_string(&P->str_cache[CSTR_FALSE], pawS_new_str(P, "false"));
    pawV_set_string(&P->str_cache[CSTR_NULL], pawS_new_str(P, "null"));
}

static void parse_module(FnState *fn, BlkState *blk)
{
    Lex *lex = fn->lex;
    enter_function(lex, fn, blk, FN_MODULE);
    stmtlist(lex);
    check(lex, TK_END);
    leave_function(lex);

    paw_assert(lex->pm->vsize == 0);
    paw_assert(fn->level == 0);
}

Closure *pawP_parse(paw_Env *P, paw_Reader input, ParseMemory *pm, const char *name, void *ud)
{
    // Initialize the lexical state.
    Lex lex = {
        .pm = pm,
        .ud = ud,
        .P = P,
    };
    pawX_set_source(&lex, input);
    StackPtr sp = pawC_stkinc(P, 2);

    // Create the main closure and push it onto the stack so that the garbage
    // collector can find it.
    Closure *main = pawV_new_closure(P, 1);
    pawV_set_closure(&sp[0], main);
    Proto *f = pawV_new_proto(P);
    main->p = f;

    // Do the same for the lexer string map. Strings are reachable from this map
    // during the parse. Once the parse is finished, all strings should be
    // anchored somewhere.
    lex.strings = pawH_new(P);
    pawV_set_map(&sp[1], lex.strings);

    // Store the module name.
    String *modname = pawS_new_str(P, name);
    lex.modname = modname;
    f->name = modname;

    // Load the first token.
    skip(&lex);

    // Parse the module.
    BlkState blk;
    FnState fn = {
        .name = modname,
        .lex = &lex,
        .proto = f,
    };
    parse_module(&fn, &blk);

    // Pop the lexer map. The strings it contains should be anchored elsewhere.
    // Leave the main closure at the top of the stack.
    pawC_stkdec(P, 1);
    return main;
}
