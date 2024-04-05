// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "parse.h"
#include "auxlib.h"
#include "call.h"
#include "code.h"
#include "env.h"
#include "gc.h"
#include "lex.h"
#include "map.h"
#include "mem.h"
#include "opcode.h"
#include "paw.h"
#include "str.h"
#include "type.h"
#include "util.h"
#include "value.h"
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define env(lex) (lex)->P
#define is_toplevel(lex) (!(lex)->fn->caller)
#define is_global(lex) (is_toplevel(lex) && (lex)->fn->blk->outer == NULL)
#define scan_string(lex, s) pawX_scan_string(lex, s, strlen(s))

// recursive non-terminals
static Expr *subexpr(Lex *lex, unsigned prec);
static Stmt *stmt(Lex *lex);
#define expr(x) subexpr(x, 0)

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

static int add_constant(Lex *lex, Value v)
{
    FnState *fn = lex->fn;
    Proto *p = fn->proto;

    if (fn->nk == UINT16_MAX) {
        limit_error(lex, "constants", UINT16_MAX);
    } else if (fn->nk == p->nk) {
        // 'fn->nk' only ever increases by 1, so this will always give us
        // enough memory.
        pawM_grow(env(lex), p->k, fn->nk, p->nk);
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
        pawM_grow(env(lex), p->p, fn->nproto, p->nproto);
        for (int i = fn->nproto; i < p->nproto; ++i) {
            p->p[i] = NULL; // clear for GC (including current)
        }
    }
    Proto *callee = pawV_new_proto(env(lex));
    callee->modname = lex->modname;
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

static int code_int(FnState *fn, Value v)
{
    paw_assert(pawV_is_int(v) || pawV_is_bigint(v));
    const int location = add_constant(fn->lex, v);
    pawK_code_U(fn, OP_PUSHCONST, location);
    return location;
}

static int code_float(FnState *fn, paw_Float f)
{
    Value v;
    pawV_set_float(&v, f);
    const int location = add_constant(fn->lex, v);
    pawK_code_U(fn, OP_PUSHCONST, location);
    return location;
}

static int code_string(FnState *fn, String *s)
{
    const Value v = obj2v(s);
    const int location = add_constant(fn->lex, v);
    pawK_code_U(fn, OP_PUSHCONST, location);
    return location;
}

#define JUMP_PLACEHOLDER (-1)

static int code_jump(FnState *fn, OpCode op)
{
    pawK_code_S(fn, op, JUMP_PLACEHOLDER);
    return fn->pc - 1;
}

static void patch_jump(FnState *fn, int from, int to)
{
    Lex *lex = fn->lex;
    const int jump = to - (from + 1);
    if (jump > JUMP_MAX) {
        limit_error(lex, "instructions to jump", JUMP_MAX);
    }
    Proto *p = fn->proto;
    set_S(&p->source[from], jump);
}

static void patch_here(FnState *fn, int from)
{
    patch_jump(fn, from, fn->pc);
}

static void code_loop(FnState *fn, Op op, int to)
{
    Lex *lex = fn->lex;
    const int jump = to - (fn->pc + 1);
    if (jump > JUMP_MAX) {
        limit_error(lex, "instructions in loop", JUMP_MAX);
    }
    pawK_code_S(fn, op, jump);
}

static void code_closure(FnState *fn, Proto *p, int id)
{
    Value v;
    pawV_set_proto(&v, p);
    pawK_code_U(fn, OP_CLOSURE, id);
}

static void add_label(FnState *fn, LabelKind kind)
{
    Lex *lex = fn->lex;
    LabelList *ll = &lex->pm->ll;
    pawM_grow(env(lex), ll->values, ll->length, ll->capacity);
    ll->values[ll->length] = (Label){
        .kind = kind,
        .line = lex->line,
        .level = fn->proto->ndebug,
        .pc = code_jump(fn, OP_JUMP),
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
            patch_here(fn, lb->pc);
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
            const int jump = to - (lb->pc + 1);
            set_S(&p->source[lb->pc], jump);
            remove_label(ll, i);
        } else {
            ++i;
        }
    }
}

static paw_Bool resolve_global(Lex *lex, String *name, VarInfo *pinfo)
{
    ParseMemory *pm = lex->pm;
    struct VarList ls = pm->globals;
    for (int i = ls.size - 1; i >= 0; --i) {
        const VarDesc var = ls.data[i];
        if (pawS_eq(name, var.name)) {
            pinfo->index = i;
            pinfo->kind = VAR_GLOBAL;
            pinfo->type = var.type;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

// Find an active local variable with the given 'name'
// Only consider locals that have been brought into scope, using begin_local_scope().
static paw_Bool resolve_local(FnState *fn, String *name, VarInfo *pinfo)
{
    ParseMemory *pm = fn->lex->pm;
    for (int i = fn->level - 1; i >= fn->base; --i) {
        const VarDesc var = pm->locals.data[i];
        if (pawS_eq(name, var.name)) {
            pinfo->index = i - fn->base;
            pinfo->kind = VAR_LOCAL;
            pinfo->type = var.type;
            return PAW_TRUE;
        }
    }
    return PAW_FALSE;
}

static VarInfo add_upvalue(FnState *fn, String *name, int index, const Type *type, paw_Bool is_local)
{
    Proto *f = fn->proto;
    for (int i = 0; i < fn->nup; ++i) {
        struct UpValueInfo up = f->u[i];
        if (up.index == index && up.is_local == is_local) {
            return (VarInfo){
                .index = i, 
                .kind = VAR_UPVALUE, 
                .type = up.var.type,
            };
        }
    }
    if (fn->nup == UPVALUE_MAX) {
        limit_error(fn->lex, "upvalues", UPVALUE_MAX);
    } else if (fn->nup == f->nup) {
        pawM_grow(env(fn->lex), f->u, fn->nup, f->nup);
        for (int i = fn->nup + 1; i < f->nup; ++i) {
            f->u[i].var = (VarDesc){0}; // clear for GC
        }
    }
    f->u[fn->nup] = (struct UpValueInfo){
        .var = {type, name},
        .is_local = is_local,
        .index = index,
    };
    return (VarInfo){
        .index = fn->nup++, 
        .kind = VAR_UPVALUE, 
        .type = type,
    };
}

static paw_Bool resolve_upvalue(FnState *fn, String *name, VarInfo *pinfo)
{
    FnState *caller = fn->caller;
    if (!caller) { // base case
        return PAW_FALSE;
    }
    // Check the caller's local variables.
    if (resolve_local(caller, name, pinfo)) {
        const int local = pinfo->index;
        caller->proto->v[local].captured = PAW_TRUE;
        *pinfo = add_upvalue(fn, name, local, pinfo->type, PAW_TRUE);
        return PAW_TRUE;
    }

    if (resolve_upvalue(caller, name, pinfo)) {
        const int upvalue = pinfo->index;
        *pinfo = add_upvalue(fn, name, upvalue, pinfo->type, PAW_FALSE);
        return PAW_TRUE;
    }
    return PAW_FALSE;
}

static VarDesc *add_var(FnState *fn, VarList *vl, String *name, TypeTag tag)
{
    Lex *lex = fn->lex;
    paw_Env *P = env(lex);
    pawM_grow(P, vl->data, vl->size, vl->alloc);
    const int index = vl->size++;
    vl->data[index] = (VarDesc){
        .type = tag,
        .name = name,
    };
    return &vl->data[index];
}

static VarDesc *add_global(FnState *fn, String *name, TypeTag tag)
{
    Lex *lex = fn->lex;
    if (!is_global(lex)) {
        pawX_error(lex, "global variable in nested scope");
    }
    ParseMemory *pm = lex->pm;
    VarList *vl = &pm->globals; // enforce uniqueness
    for (int i = 0; i < vl->size; ++i) {
        if (pawS_eq(name, vl->data[i].name)) {
            pawX_error(lex, "redefinition of global variable '%s'", name->text);
        }
    }
    return add_var(fn, vl, name, tag);
}

static VarDesc *add_local(FnState *fn, String *name, TypeTag tag)
{
    Lex *lex = fn->lex;
    ParseMemory *pm = lex->pm; // allow shadowing/rebinding
    return add_var(fn, &pm->locals, name, tag);
}

static void add_debug_info(Lex *lex, VarDesc var)
{
    FnState *fn = lex->fn;
    Proto *p = fn->proto;
    if (fn->ndebug == LOCAL_MAX) {
        limit_error(lex, "locals", LOCAL_MAX);
    } else if (fn->ndebug == p->ndebug) {
        pawM_grow(env(lex), p->v, fn->ndebug, p->ndebug);
        for (int i = fn->ndebug + 1; i < p->ndebug; ++i) {
            p->v[i].var = (VarDesc){0}; // clear for GC
        }
    }
    p->v[fn->ndebug] = (struct LocalInfo){
        .var = var,
        .pc0 = fn->pc,
    };
    ++fn->ndebug;
}

static void begin_local_scope(FnState *fn, int n)
{
    Lex *lex = fn->lex;
    ParseMemory *pm = lex->pm;
    for (int i = 0; i < n; ++i) {
        const int level = fn->level++;
        VarDesc var = pm->locals.data[level];
        add_debug_info(lex, var);
    }
}

static struct LocalInfo *local_info(FnState *fn, int level)
{
    return &fn->proto->v[level - fn->base];
}

static void close_vars(FnState *fn, int target)
{
    for (int level = fn->level - 1; level >= target;) {
        paw_Bool close = PAW_FALSE;
        const int upper = level; // first popped slot
        const int lower = paw_max(upper - INT8_MAX, target - 1);
        for (int i = level; !close && i > lower; --i) {
            struct LocalInfo *local = local_info(fn, i);
            close = close ? close : local->captured;
        }
        const int npop = upper - lower; // 'lower' is 1 past end
        pawK_code_AB(fn, OP_CLOSE, npop, close);
        level = lower;
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
    pm->locals.size -= nvar;
}

// ORDER UnaryOp
typedef enum {
    UN_LEN, // #
    UN_NEG, // -
    UN_NOT, // !
    UN_BNOT, // ~

    NUNOPS
} UnOp;

// ORDER BinaryOp
typedef enum {
    INFIX_EQ, // ==
    INFIX_NE, // !=
    INFIX_LT, // <
    INFIX_LE, // <=
    INFIX_GT, // >
    INFIX_GE, // >=
    INFIX_IN, // in
    INFIX_ADD, // +
    INFIX_SUB, // -
    INFIX_MUL, // *
    INFIX_DIV, // /
    INFIX_IDIV, // //
    INFIX_MOD, // %
    INFIX_POW, // **
    INFIX_CONCAT, // ++
    INFIX_BXOR, // ^
    INFIX_BAND, // &
    INFIX_BOR, // |
    INFIX_SHL, // <<
    INFIX_SHR, // >>
    INFIX_AND, // &&
    INFIX_OR, // ||
    INFIX_COND, // ??::
    INFIX_COALESCE, // ?:

    NINFIX
} InfixOp;

#define NOT_UNOP NUNOPS
#define NOT_INFIX NINFIX

static const struct {
    uint8_t left;
    uint8_t right;
} kInfixPrec[NINFIX] = {
    [INFIX_MUL] = {13, 13},
    [INFIX_DIV] = {13, 13},
    [INFIX_IDIV] = {13, 13},
    [INFIX_MOD] = {13, 13},
    [INFIX_ADD] = {12, 12},
    [INFIX_SUB] = {12, 12},
    [INFIX_CONCAT] = {11, 11},
    [INFIX_SHL] = {10, 10},
    [INFIX_SHR] = {10, 10},
    [INFIX_BAND] = {9, 9},
    [INFIX_BXOR] = {8, 8},
    [INFIX_BOR] = {7, 7},
    [INFIX_IN] = {6, 6},
    [INFIX_LT] = {6, 6},
    [INFIX_LE] = {6, 6},
    [INFIX_GT] = {6, 6},
    [INFIX_GE] = {6, 6},
    [INFIX_EQ] = {5, 5},
    [INFIX_NE] = {5, 5},
    [INFIX_AND] = {4, 4},
    [INFIX_OR] = {3, 3},
    [INFIX_COALESCE] = {2, 2},
    [INFIX_COND] = {1, 0}, // right-associative
};

static const uint8_t kUnOpPrecedence = 14;

static unsigned left_prec(InfixOp op)
{
    return kInfixPrec[op].left;
}

static unsigned right_prec(InfixOp op)
{
    return kInfixPrec[op].right;
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

static InfixOp get_infixop(TokenKind kind)
{
    switch (kind) {
        case '+':
            return INFIX_ADD;
        case '-':
            return INFIX_SUB;
        case '*':
            return INFIX_MUL;
        case '/':
            return INFIX_DIV;
        case '%':
            return INFIX_MOD;
        case '<':
            return INFIX_LT;
        case '>':
            return INFIX_GT;
        case '^':
            return INFIX_BXOR;
        case '&':
            return INFIX_BAND;
        case '|':
            return INFIX_BOR;
        case TK_IN:
            return INFIX_IN;
        case TK_QUESTION2:
            return INFIX_COND;
        case TK_ELVIS:
            return INFIX_COALESCE;
        case TK_PLUS2:
            return INFIX_CONCAT;
        case TK_SLASH2:
            return INFIX_IDIV;
        case TK_EQUALS2:
            return INFIX_EQ;
        case TK_LESS2:
            return INFIX_SHL;
        case TK_GREATER2:
            return INFIX_SHR;
        case TK_AMPER2:
            return INFIX_AND;
        case TK_PIPE2:
            return INFIX_OR;
        case TK_BANG_EQ:
            return INFIX_NE;
        case TK_LESS_EQ:
            return INFIX_LE;
        case TK_GREATER_EQ:
            return INFIX_GE;
        default:
            return NOT_INFIX;
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

static paw_Bool fn_needs_return(FnState *fn)
{
    // Parser stops when a 'return [expr]' is hit. If we just coded a return,
    // then we don't need another one.
    return fn->pc == 0 || get_OP(fn->proto->source[fn->pc - 1]) != OP_RETURN;
}

static void leave_function(Lex *lex)
{
    ParseMemory *pm = lex->pm;
    FnState *fn = lex->fn;
    BlkState *blk = fn->blk;
    Proto *p = fn->proto;

    // End the lifetime of function-scoped variables, but don't code any pop
    // or close instructions. OP_RETURN does that automatically.
    end_local_scope(fn, blk);
    paw_assert(fn->level == fn->base);
    paw_assert(blk->outer == NULL);

    if (fn->kind == FN_INIT) {
        pawK_code_U(fn, OP_GETLOCAL, 0);
        pawK_code_0(fn, OP_RETURN);
    } else if (fn_needs_return(fn)) {
        pawK_code_0(fn, OP_PUSHNULL);
        pawK_code_0(fn, OP_RETURN);
    }
    pawM_shrink(env(lex), p->source, p->length, fn->pc);
    p->length = fn->pc;
    pawM_shrink(env(lex), p->lines, p->nlines, fn->nlines);
    p->nlines = fn->nlines;
    pawM_shrink(env(lex), p->p, p->nproto, fn->nproto);
    p->nproto = fn->nproto;
    pawM_shrink(env(lex), p->v, p->ndebug, fn->ndebug);
    p->ndebug = fn->ndebug;
    pawM_shrink(env(lex), p->u, p->nup, fn->nup);
    p->nup = fn->nup;
    pawM_shrink(env(lex), p->k, p->nk, fn->nk);
    p->nk = fn->nk;

    pm->locals.size = fn->base;
    lex->fn = fn->caller;
    check_gc(env(lex));
}

static String *context_name(const FnState *fn, FnKind kind)
{
    if (fn_has_self(kind)) {
        return pawV_get_string(pawE_cstr(env(fn->lex), CSTR_SELF));
    }
    return fn->proto->name;
}

static void enter_function(Lex *lex, FnState *fn, BlkState *blk, FnKind kind)
{
    *fn = (FnState){
        .proto = fn->proto, // Keep value
        .base = lex->pm->locals.size,
        .level = lex->pm->locals.size,
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
    add_local(fn, context_name(fn, kind), NULL); // TODO: Fix this type in visit_fn
    begin_local_scope(fn, 1);
}

static String *parse_name(Lex *lex)
{
    check(lex, TK_NAME);
    String *name = pawV_get_string(lex->t.value);
    skip(lex);
    return name;
}

static void new_local_literal(FnState *fn, const char *name, int type)
{
    struct TypeVec *tv = &env(fn->lex)->tv;
    add_local(fn, scan_string(fn->lex, name), &tv->data[type]);
}

static Expr *emit_primitive(Lex *lex, Value v)
{
    PrimitiveExpr *expr = pawK_add_node(lex, EXPR_PRIMITIVE, PrimitiveExpr);
    expr->v = v;
    return cast_expr(expr);
}

static Expr *emit_null(Lex *lex)
{
    PrimitiveExpr *expr = pawK_add_node(lex, EXPR_PRIMITIVE, PrimitiveExpr);
    pawV_set_null(&expr->v);
    return cast_expr(expr);
}

static Expr *emit_bool(Lex *lex, paw_Bool b)
{
    PrimitiveExpr *expr = pawK_add_node(lex, EXPR_PRIMITIVE, PrimitiveExpr);
    pawV_set_bool(&expr->v, b);
    return cast_expr(expr);
}

// letstmt := storage [const] name [`:` Type] [`=` expr]
//
//const x: int = 5
//global x: int
//
// storage := let | global
//
// Type := primitive | TArray | TMap | TClass
//
// TArray := `[` Type `]`
//
// TMap := TBasic `[` TValue `]`
//
// TClass := name
//
// TValue := TBasic | TArray | MTap
//
// TBasic := bool | int | float | string
//
static TypeTag type_expr(Lex *lex);

static TypeTag array_type_expr(Lex *lex)
{
    const int line = lex->line;
    skip(lex); // '[' token
    TypeTag elem = type_expr(lex);
    delim_next(lex, ']', '[', line);
    return pawY_register_array(env(lex), elem);
}

static TypeTag map_type_expr(Lex *lex, TypeTag key)
{
    const int line = lex->line;
    skip(lex); // '[' token
    TypeTag value = type_expr(lex);
    delim_next(lex, ']', '[', line);
    return pawY_register_map(env(lex), key, value);
}

static TypeTag parse_typename(Lex *lex)
{
    if (test_next(lex, TK_NULL)) {
        return kTag0;
    }
    const String *name = parse_name(lex);
    if (name->flag < 0) { // found primitive type
        struct TypeVec *tv = &env(lex)->tv;
        return &tv->data[-name->flag - 1];
    } else { // lookup class type
        paw_assert(PAW_FALSE); // FIXME: search for class type name, error if not found
        return NULL;
    }
}

static TypeTag type_expr(Lex *lex)
{
    if (test(lex, '[')) {
        return array_type_expr(lex); 
    }
    TypeTag t = parse_typename(lex);
    if (test(lex, '[')) {
        return map_type_expr(lex, t); 
    }
    return t;
}

static TypeTag ret_annotation(Lex *lex)
{
    return test_next(lex, ':') ? type_expr(lex) : NULL;
}

static TypeTag var_annotation(Lex *lex)
{
    if (test_next(lex, ':')) {
        TypeTag tag = type_expr(lex);
        if (is_type0(tag)) {
            pawX_error(lex, "variable annotation must not be 'null'");
        }
        return tag;
    }
    return NULL; // needs inference
}

static Stmt *parameter_def(Lex *lex)
{
    ParamStmt *result = pawK_add_node(lex, STMT_PARAM, ParamStmt);
    result->name = parse_name(lex);
    result->tag = var_annotation(lex);
    if (result->tag == NULL) {
        pawX_error(lex, "expected type annotation on parameter '%s'", 
                   result->name->text);
    }
    return cast_stmt(result);
}

static Stmt *variable_def(Lex *lex, int line, paw_Bool global)
{
    DefStmt *result = pawK_add_node(lex, STMT_DEF, DefStmt);
    result->line = line; // line containing 'global' or 'let'
    result->name = parse_name(lex);
    result->tag = var_annotation(lex);
    result->init = test_next(lex, '=')
                       ? expr(lex) // parse initial value
                       : emit_null(lex); // defaults to 'null'
    result->flags |= global ? DEF_GLOBAL : 0;
    result->flags |= PAW_TRUE ? DEF_MUTABLE : 0;
    semicolon(lex);
    return cast_stmt(result);
}

static VarInfo find_var(FnState *fn, String *name)
{
    VarInfo info;
    Lex *lex = fn->lex;
    if (!resolve_local(fn, name, &info) && // not local
        !resolve_upvalue(fn, name, &info) && // not local to caller
        !resolve_global(lex, name, &info)) { // not defined
        pawX_error(lex, "undefined variable '%s'", name->text);
    }
    return info;
}

static void call_parameters(Lex *lex, NodeVec *nvec)
{
    const int line = lex->line;
    skip(lex); // '(' token

    if (!test_next(lex, ')')) {
        push_node(lex, nvec, expr(lex));
        while (test_next(lex, ',')) {
            if (nvec->size == ARGC_MAX) {
                limit_error(lex, "function parameters", ARGC_MAX);
            }
            push_node(lex, nvec, expr(lex));
        }
        delim_next(lex, ')', '(', line);
        link_nvec(lex, nvec); // for cleanup
    }
}

// Parse a variable name
static Expr *varexpr(Lex *lex)
{
    VarExpr *result = pawK_add_node(lex, EXPR_VAR, VarExpr);
    result->name = parse_name(lex);
    return cast_expr(result);
}

// static void code_invoke(Lex *lex, Op op, ExprState *e)
//{
//     const int name = add_name(lex, e->s);
//     discard(e);
//
//     const int argc = call_parameters(lex);
//     pawK_code_AB(lex->fn, op, name, argc);
// }
//
// static void push_special(Lex *lex, unsigned ctag)
//{
//     ExprState e;
//     if (ctag == CSTR_SELF) {
//         // 'self' is always in slot 0
//         init_expr(&e, EXPR_LOCAL, 0);
//     } else {
//         // 'super' is an upvalue
//         const Value v = pawE_cstr(lex->P, ctag);
//         find_var(lex, &e, pawV_get_string(v));
//     }
//     discharge(&e);
// }

////static void superexpr(Lex *lex, ExprState *e)
////{
////    if (!lex->cls) {
////        pawX_error(lex, "'super' used outside class body");
////    } else if (!lex->cls->has_super) {
////        pawX_error(lex, "class has no superclass");
////    }
////
////    skip(lex); // 'super' token
////    check_next(lex, '.');
////
////    const int name = parse_name(lex, e);
////    if (test(lex, '(')) {
////        const int argc = call_parameters(lex);
////        push_special(lex, CSTR_SELF);
////        push_special(lex, CSTR_SUPER);
////        pawK_code_AB(lex->fn, OP_INVOKESUPER, name, argc);
////        e->kind = EXPR_CALL;
////    } else {
////        push_special(lex, CSTR_SELF);
////        push_special(lex, CSTR_SUPER);
////        pawK_code_U(lex->fn, OP_GETSUPER, name);
////        e->kind = EXPR_ATTR;
////    }
////}

static Expr *unop_expr(Lex *lex, UnOp op)
{
    UnOpExpr *result = pawK_add_node(lex, EXPR_UNOP, UnOpExpr);
    skip(lex); // unary operator token
    result->op = (UnaryOp)op; // same order
    result->target = subexpr(lex, kUnOpPrecedence);
    return cast_expr(result);
}

static Expr *paren_expr(Lex *lex)
{
    // Just parse and return the expression contained within the parenthesis.
    // There is no need for an extra node type.
    const int line = lex->line;
    skip(lex); // '(' token
    Expr *result = expr(lex);
    delim_next(lex, ')', '(', line);
    return result;
}

static paw_Bool end_of_block(Lex *lex)
{
    return test(lex, '}') || // found end of block
           test(lex, TK_END); // truncated block
}

static void defer_nvec(Lex *lex, NodeVec *nvec)
{
    if (nvec->alloc > 0) {
        link_nvec(lex, nvec);
    }
}

static void stmtlist(Lex *lex, NodeVec *nvec)
{
    while (!end_of_block(lex)) {
        const TokenKind tk = lex->t.kind;
        push_node(lex, nvec, stmt(lex));
        if (tk == TK_RETURN) {
            break;
        }
    }
    defer_nvec(lex, nvec);
}

static Expr *array_expr(Lex *lex)
{
    ArrayExpr *result = pawK_add_node(lex, EXPR_ARRAY, ArrayExpr);
    const int line = lex->line;
    skip(lex); // '[' token

    NodeVec *items = &result->items;
    do {
        if (test(lex, ']')) {
            break;
        } else if (items->size == LOCAL_MAX) {
            limit_error(lex, "array elements", LOCAL_MAX);
        }
        push_node(lex, items, expr(lex));
    } while (test_next(lex, ','));
    delim_next(lex, ']', '[', line);
    defer_nvec(lex, items);
    return cast_expr(result);
}

static Expr *map_expr(Lex *lex)
{
    const int line = lex->line;
    skip(lex); // '{' token
    MapExpr *result = pawK_add_node(lex, EXPR_MAP, MapExpr);

    NodeVec *items = &result->items;
    do {
        if (test(lex, '}')) {
            break;
        } else if (items->size > LOCAL_MAX - 2) {
            limit_error(lex, "map items", LOCAL_MAX);
        }
        push_node(lex, items, expr(lex));
        check_next(lex, ':');
        push_node(lex, items, expr(lex));
    } while (test_next(lex, ','));
    delim_next(lex, '}', '{', line);
    defer_nvec(lex, items);
    return cast_expr(result);
}

static Expr *index_expr(Lex *lex, Expr *prefix)
{
    IndexExpr *result = pawK_add_node(lex, EXPR_INDEX, IndexExpr);
    const int line = lex->line;
    skip(lex); // '[' token

    result->target = prefix;
    if (test(lex, ':')) {
        result->first = emit_null(lex);
    } else {
        result->first = expr(lex);
    }
    if (!test_next(lex, ':')) {
        result->second = NULL;
    } else if (test(lex, ']')) {
        result->second = emit_null(lex); 
    } else {
        result->second = expr(lex); 
    }
    delim_next(lex, ']', '[', line);
    return cast_expr(result);
}

static Expr *fieldexpr(Lex *lex, Expr *prefix)
{
    FieldExpr *result = pawK_add_node(lex, EXPR_FIELD, FieldExpr);
    skip(lex); // '.' token

    result->target = prefix;
    result->name = parse_name(lex);
    result->index = add_name(lex, result->name);
    result->call = test(lex, '(');
    return cast_expr(result);
}

static Expr *call_expr(Lex *lex, Expr *prefix)
{
    CallExpr *result = pawK_add_node(lex, EXPR_CALL, CallExpr);
    call_parameters(lex, &result->param);
    result->target = prefix;
    return cast_expr(result);
}

static Expr *chain_expr(Lex *lex, Expr *prefix)
{
    ChainExpr *result = pawK_add_node(lex, EXPR_UNOP, ChainExpr);
    result->target = prefix;
    skip(lex); // '?' token
    return cast_expr(result);
}

static void fn_parameters(Lex *lex, NodeVec *param)
{
    const int line = lex->line;
    check_next(lex, '(');

    if (!test_next(lex, ')')) {
        do {
            if (param->size == ARGC_MAX) {
                limit_error(lex, "function parameters", ARGC_MAX);
            } else if (!test(lex, TK_NAME)) {
                expected_symbol(lex, "name");
            }
            // parse function parameter of form 'name: type'
            push_node(lex, param, parameter_def(lex));
        } while (test_next(lex, ','));
        delim_next(lex, ')', '(', line);
        link_nvec(lex, param);
    }
}

static Block *block(Lex *lex)
{
    const int line = lex->line;
    Block *result = pawK_add_node(lex, STMT_BLOCK, Block);
    check_next(lex, '{');
    stmtlist(lex, &result->stmts);
    delim_next(lex, '}', '{', line);
    return result;
}

static Expr *fn_expr(Lex *lex)
{
    const int line = lex->line;
    skip(lex); // 'fn' token
    FnExpr *result = pawK_add_node(lex, EXPR_FN, FnExpr);
    fn_parameters(lex, &result->param);
    result->ret = ret_annotation(lex);
    result->line = line;
    result->body = block(lex);
    return cast_expr(result);
}

static Expr *primary_expr(Lex *lex)
{
    switch (lex->t.kind) {
        case '(':
            return paren_expr(lex);
        case TK_NAME:
            return varexpr(lex);
        case TK_STRING: {
            const Value v = lex->t.value;
            skip(lex);
            return emit_primitive(lex, v);
        }
        case '[':
            return array_expr(lex);
        case '{':
            return map_expr(lex);
        //case TK_SUPER:
        //    return superexpr(lex, e);
        default:
            expected_symbol(lex, "name or '('");
            return NULL; // never run
    }
}

static Expr *suffixed_expr(Lex *lex)
{
    Expr *e = primary_expr(lex);
    for (;;) { // parse suffix chain
        switch (lex->t.kind) {
            case '(':
                e = call_expr(lex, e);
                break;
            case '.':
                e = fieldexpr(lex, e);
                break;
            case '[':
                e = index_expr(lex, e);
                break;
            case '?':
                e = chain_expr(lex, e);
                break;
            default:
                return e;
        }
    }
}

static Expr *simple_expr(Lex *lex)
{
    Expr *expr;
    switch (lex->t.kind) {
        case TK_NULL:
            expr = emit_null(lex);
            break;
        case TK_TRUE:
            expr = emit_bool(lex, PAW_TRUE);
            break;
        case TK_FALSE:
            expr = emit_bool(lex, PAW_FALSE);
            break;
        case TK_INTEGER:
        case TK_FLOAT:
            expr = emit_primitive(lex, lex->t.value);
            break;
        case TK_FN:
            return fn_expr(lex);
        default:
            return cast_expr(suffixed_expr(lex));
    }
    skip(lex); // skip literal
    return expr;
}

static Expr *binop_expr(Lex *lex, InfixOp op, Expr *lhs)
{
    skip(lex); // binary operator token
    Expr *rhs = subexpr(lex, right_prec(op));
    if (rhs == NULL) {
        return NULL; // no more binops
    }
    BinOpExpr *result = pawK_add_node(lex, EXPR_BINOP, BinOpExpr);
    result->op = (BinaryOp)op; // same order
    result->lhs = lhs;
    result->rhs = rhs;
    return cast_expr(result);
}

static Expr *logical_expr(Lex *lex, Expr *lhs, paw_Bool is_and)
{
    skip(lex); // '&&' or '||' token
    Expr *rhs = subexpr(lex, right_prec(INFIX_AND));
    if (rhs == NULL) {
        return NULL; // no more binops
    }
    LogicalExpr *result = pawK_add_node(lex, EXPR_LOGICAL, LogicalExpr);
    result->is_and = is_and;
    result->lhs = lhs;
    result->rhs = rhs;
    return cast_expr(result);
}

static Expr *cond_expr(Lex *lex, Expr *lhs)
{
    skip(lex); // '??' token
    CondExpr *result = pawK_add_node(lex, EXPR_COND, CondExpr);
    result->cond = lhs;
    result->lhs = subexpr(lex, right_prec(INFIX_COND));
    check_next(lex, TK_COLON2);
    result->rhs = expr(lex);
    return cast_expr(result);
}

static Expr *infix_expr(Lex *lex, Expr *lhs, unsigned op)
{
    switch (op) {
        case INFIX_AND:
        case INFIX_OR:
            return logical_expr(lex, lhs, op == INFIX_AND);
        case INFIX_COND:
            return cond_expr(lex, lhs);
        default:
            return binop_expr(lex, op, lhs);
    }
}

static Expr *subexpr(Lex *lex, unsigned prec)
{
    unsigned op = get_unop(lex->t.kind);
    Expr *expr = op == NOT_UNOP
                     ? simple_expr(lex)
                     : unop_expr(lex, op);

    op = get_infixop(lex->t.kind);
    while (op != NOT_INFIX && prec < left_prec(op)) {
        expr = infix_expr(lex, expr, op);
        op = get_infixop(lex->t.kind);
    }
    return expr;
}

static Stmt *if_stmt(Lex *lex)
{
    skip(lex); // 'if' token
    IfElseStmt *result = pawK_add_node(lex, STMT_IFELSE, IfElseStmt);
    result->cond = expr(lex); // conditional
    result->then_arm = cast_stmt(block(lex)); // 'then' block

    if (test_next(lex, TK_ELSE)) {
        if (test(lex, TK_IF)) {
            // Put the rest of the chain in the else branch. This transformation looks
            // like 'if a {} else if b {} else {}' -> 'if a {} else {if b {} else {}}'.
            result->else_arm = if_stmt(lex);
        } else {
            result->else_arm = cast_stmt(block(lex));
        }
    }
    return cast_stmt(result);
}

static Stmt *exprstmt(Lex *lex)
{
    ExprStmt *result = pawK_add_node(lex, STMT_EXPR, ExprStmt);
    result->lhs = suffixed_expr(lex);
    if (test_next(lex, '=')) {
        result->rhs = expr(lex);
    }
    semicolon(lex);
    return cast_stmt(result);
}

static Stmt *fornum(Lex *lex, String *ivar)
{
    ForNumStmt *result = pawK_add_node(lex, STMT_FORNUM, ForNumStmt);
    result->name = ivar;
    // Parse the loop bounds ('begin', 'end', and 'step' expressions).
    result->begin = expr(lex);
    check_next(lex, ',');
    result->end = expr(lex);
    if (test_next(lex, ',')) {
        result->step = expr(lex);
    } else { // step defaults to 1
        Value v;
        pawV_set_int(&v, 1);
        result->step = emit_primitive(lex, v);
    }
    result->block = block(lex);
    return cast_stmt(result);
}

static Stmt *forin(Lex *lex, String *ivar)
{
    ForInStmt *result = pawK_add_node(lex, STMT_FORIN, ForInStmt);
    result->name = ivar;
    result->target = expr(lex);
    result->block = block(lex);
    return cast_stmt(result);
}

static Stmt *for_stmt(Lex *lex)
{
    skip(lex); // 'for' token
    String *ivar = parse_name(lex); // loop variable
    if (test_next(lex, '=')) {
        return fornum(lex, ivar);
    } else if (!test_next(lex, TK_IN)) {
        expected_symbol(lex, "'=' or 'in'"); // no return
    }
    return forin(lex, ivar);
}

static Stmt *while_stmt(Lex *lex)
{
    WhileStmt *result = pawK_add_node(lex, STMT_WHILE, WhileStmt);
    skip(lex); // 'while' token
    result->cond = expr(lex);
    result->block = block(lex);
    return cast_stmt(result);
}

static Stmt *dowhile_stmt(Lex *lex)
{
    WhileStmt *result = pawK_add_node(lex, STMT_DOWHILE, WhileStmt);
    skip(lex); // 'do' token
    result->block = block(lex);
    check_next(lex, TK_WHILE);
    result->cond = expr(lex);
    return cast_stmt(result);
}

static Stmt *return_stmt(Lex *lex)
{
    ReturnStmt *result = pawK_add_node(lex, STMT_RETURN, ReturnStmt);
    skip(lex); // 'return' token
    if (end_of_block(lex) || test(lex, ';')) {
        result->expr = emit_null(lex);
    } else {
        result->expr = expr(lex);
    }
    // NOTE: The construct 'return [expr] [`;`]' must be followed by the
    //       end of the block. This is necessary because the semicolon is
    //       optional, and we cannot easily tell if it was intended to go
    //       before or after the '[expr]' part.
    semicolon(lex);
    return cast_stmt(result);
}

static Stmt *labelstmt(Lex *lex, LabelKind kind)
{
    LabelStmt *result = pawK_add_node(lex, STMT_LABEL, LabelStmt);
    result->label = kind;
    skip(lex); // 'break' or 'continue' token
    semicolon(lex);
    return result;
}

//static void include_stmt(Lex *lex)
//{
//    skip(lex); // 'include' token
//    semicolon(lex);
//}

static Stmt *fn_stmt(Lex *lex, int line, paw_Bool global)
{
    skip(lex); // 'fn' token
    String *name = parse_name(lex);
    FnStmt *result = pawK_add_node(lex, STMT_FN, FnStmt);
    fn_parameters(lex, &result->param);
    result->ret = ret_annotation(lex);
    result->global = global;
    result->line = line;
    result->name = name;
    result->body = block(lex);
    return cast_stmt(result);
}

static Stmt *global_stmt(Lex *lex)
{
    const int line = lex->lastline;
    skip(lex); // 'global' token
    if (test(lex, TK_FN)) {
        return fn_stmt(lex, line, PAW_TRUE);
    } else {
        return variable_def(lex, line, PAW_TRUE);
    }
}

// static void method_def(Lex *lex)
//{
//     const String *init = pawV_get_string(pawE_cstr(env(lex), CSTR_INIT));
//     const String *name = pawV_get_string(lex->t.value);
//     FnKind kind = FN_METHOD;
//     if (pawS_eq(name, init)) {
//         kind = FN_INIT;
//     } else {
//         check(lex, TK_NAME);
//     }
//     ExprState e;
//     e.index = add_constant(lex, lex->t.value);
//     e.name = pawV_get_string(lex->t.value);
//     skip(lex); // name token
//
//     function(lex, e.name, kind);
//     pawK_code_U(lex->fn, OP_NEWMETHOD, e.index);
// }
//
// static void class_body(Lex *lex)
//{
//     const int line = lex->line;
//     check_next(lex, '{');
//     while (!test(lex, '}')) {
//         check(lex, TK_NAME);
//         method_def(lex);
//     }
//     delim_next(lex, '}', '{', line);
// }
//
// static void class_stmt(Lex *lex)
//{
//     FnState *fn = lex->fn;
//     skip(lex); // 'class' token
//
//     ExprState e;
//     e.name = parse_name(lex);
//     init_var(lex, &e);
//
//     ClsState cls = {.outer = lex->cls};
//     if (test_next(lex, ':')) {
//         ExprState super;
//         // push superclass
//         varexpr(lex, &super);
//         discharge(&super);
//         cls.has_super = PAW_TRUE;
//     }
//     code_string(lex->fn, e.name);
//     pawK_code_U(fn, OP_NEWCLASS, cls.has_super);
//     declare_var(lex, &e);
//
//     BlkState blk;
//     // scope for potential 'super' variable
//     enter_block(fn, &blk, PAW_FALSE);
//
//     if (cls.has_super) {
//         // Create a local variable, 'super', to reference the superclass. If
//         // referenced in a method, 'super' will be captured as an upvalue.
//         add_local_literal(lex, "super");
//         begin_local_scope(lex, 1);
//     }
//     lex->cls = &cls;
//
//     // push class
//     discharge(&e);
//
//     class_body(lex);
//     leave_block(fn);         // pop or close 'super'
//     pawK_code_0(fn, OP_POP); // pop class
//     semicolon(lex);
//
//     lex->cls = cls.outer;
// }

static Stmt *stmt(Lex *lex)
{
    switch (lex->t.kind) {
        case ';':
            // empty statement
            skip(lex);
            return NULL;
        case '{':
            return cast_stmt(block(lex));
        case TK_FN:
            return fn_stmt(lex, lex->lastline, PAW_FALSE);
            //        case TK_CLASS:
            //            class_stmt(lex);
            //            break;
            //        case TK_INCLUDE:
            //            return include_stmt(lex);
        case TK_LET:
            skip(lex); // 'let' token
            return variable_def(lex, lex->lastline, PAW_FALSE);
        case TK_GLOBAL:
            return global_stmt(lex);
        case TK_IF:
            return if_stmt(lex);
        case TK_FOR:
            return for_stmt(lex);
        case TK_WHILE:
            return while_stmt(lex);
        case TK_DO:
            return dowhile_stmt(lex);
        case TK_RETURN:
            return return_stmt(lex);
        case TK_BREAK:
            return labelstmt(lex, LBREAK);
        case TK_CONTINUE:
            return labelstmt(lex, LCONTINUE);
        default:
            return exprstmt(lex);
    }
}

static void type_error(FnState *fn)
{
    pawX_error(fn->lex, "invalid type");
}

// TODO
#include "api.h"
static void type_check_same(FnState *fn, const Type *lhs, const Type *rhs)
{
    if (lhs != rhs) {
        pawX_error(fn->lex, "expected equal types but found '%s' and '%s'", api_typename(lhs->code), api_typename(rhs->code)); 
    }
}

static void type_check_integral(FnState *fn, const Type *code)
{
    if (code->code != PAW_TINT && code->code != PAW_TBOOL) {
        pawX_error(fn->lex, "expected integral ('int' or 'bool') but found '%s'", api_typename(code->code)); 
    }
}

static void type_check_sequence(FnState *fn, const Type *code)
{
    if (code->code != PAW_TSTRING && code->code != PAW_TARRAY) {
        pawX_error(fn->lex, "expected sequence ('string' or 'array') but found '%s'", api_typename(code->code)); 
    }
}

static void type_check_container(FnState *fn, const Type *code)
{
    if (code->code != PAW_TSTRING && code->code != PAW_TARRAY && code->code != PAW_TMAP) {
        pawX_error(fn->lex, "expected container ('string' or 'array', or 'map') but found '%s'", api_typename(code->code)); 
    }
}

static TypeTag get_type(FnState *fn, int type)
{
    if (type < 0) {
        type_error(fn);
    }
    const paw_Env *P = env(fn->lex);
    return &P->tv.data[type];
}

static const Type *check_expr(FnState *fn, Expr *expr);

// symbols for operator type lookup tables
#define _0 PAW_NULL
#define _b PAW_TBOOL
#define _i PAW_TINT
#define _f PAW_TFLOAT
#define _s PAW_TSTRING
#define _a PAW_TARRAY
#define _m PAW_TMAP
#define _p PAW_TFUNCTION
#define _c PAW_TCLASS
#define _o PAW_TFOREIGN

static const Type *check_unop(FnState *fn, UnOpExpr *e)
{
    // clang-format off
    static const int8_t kValidOps[NUNARYOPS][PAW_NTYPES] = {
        //      type =  _b, _i, _f, _s, _a, _m, _F, _c, _o
        [UNARY_LEN]  = {_0, _0, _0, _i, _i, _i, _0, _0, _0},
        [UNARY_NEG]  = {_i, _i, _f, _0, _0, _0, _0, _0, _0},
        [UNARY_NOT]  = {_b, _b, _b, _b, _b, _b, _b, _b, _b},
        [UNARY_BNOT] = {_i, _i, _0, _0, _0, _0, _0, _0, _0},
    };
    // clang-format on

    // 'e->target' already visited
    const Type *type = e->target->type;
    const int result = kValidOps[e->op][type->code];
    if (result < 0) {
//        return try_meta_unop(fn, ex->op, lhs, rhs);
        type_error(fn);
    }
    return get_type(fn, result);
}

static const Type *check_binop(FnState *fn, BinOpExpr *e)
{
    // Group binary operators into these classes to reduce the size of
    // the lookup table below
    enum BinOpClass {
        BCLASS_EQ,
        BCLASS_REL,
        BCLASS_ARITH,
        BCLASS_BIT,
        BCLASS_IN,
        NBINCLS
    };

    // clang-format off
    static const int8_t kValidOps[NBINARYOPS][PAW_NTYPES /* LHS */][PAW_NTYPES /* RHS */] = {
#define   EQUALITY(op) [op] = {{_b, _b, _b, _b, _b, _b, _b, _b, _b}, \
                               {_b, _b, _b, _b, _b, _b, _b, _b, _b}, \
                               {_b, _b, _b, _b, _b, _b, _b, _b, _b}, \
                               {_b, _b, _b, _b, _b, _b, _b, _b, _b}, \
                               {_b, _b, _b, _b, _b, _b, _b, _b, _b}, \
                               {_b, _b, _b, _b, _b, _b, _b, _b, _b}, \
                               {_b, _b, _b, _b, _b, _b, _b, _b, _b}, \
                               {_b, _b, _b, _b, _b, _b, _b, _b, _b}, \
                               {_b, _b, _b, _b, _b, _b, _b, _b, _b}}  
        EQUALITY(BINARY_EQ),
        EQUALITY(BINARY_NE),

#define RELATIONAL(op) [op] = {{_b, _b, _b, _0, _0, _0, _0, _0, _0}, \
                               {_b, _b, _b, _0, _0, _0, _0, _0, _0}, \
                               {_b, _b, _b, _0, _0, _0, _0, _0, _0}, \
                               {_0, _0, _0, _b, _0, _0, _0, _0, _0}, \
                               {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                               {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                               {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                               {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                               {_0, _0, _0, _0, _0, _0, _0, _0, _0}} 
        RELATIONAL(BINARY_LT),
        RELATIONAL(BINARY_LE),
        RELATIONAL(BINARY_GT),
        RELATIONAL(BINARY_GE),

        //     RHS type =   _b, _i, _f, _s, _a, _m, _F, _c, _o       LHS type
        [BINARY_IN]     = {{_0, _0, _0, _0, _b, _b, _0, _0, _0},  // _b
                           {_0, _0, _0, _0, _b, _b, _0, _0, _0},  // _i
                           {_0, _0, _0, _0, _b, _b, _0, _0, _0},  // _f
                           {_0, _0, _0, _0, _b, _b, _0, _0, _0},  // _s
                           {_0, _0, _0, _0, _b, _b, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _b, _b, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _b, _b, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _b, _b, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _b, _b, _0, _0, _0}}, // _o    
        [BINARY_ADD]    = {{_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _b
                           {_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _i
                           {_f, _f, _f, _0, _0, _0, _0, _0, _0},  // _f
                           {_0, _0, _0, _s, _0, _0, _0, _0, _0},  // _s
                           {_0, _0, _0, _0, _a, _0, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0}}, // _o  
        [BINARY_SUB]    = {{_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _b
                           {_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _i
                           {_f, _f, _f, _0, _0, _0, _0, _0, _0},  // _f
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _s
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0}}, // _o  
        [BINARY_MUL]    = {{_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _b
                           {_i, _i, _f, _s, _a, _0, _0, _0, _0},  // _i
                           {_f, _f, _f, _0, _0, _0, _0, _0, _0},  // _f
                           {_0, _s, _0, _0, _0, _0, _0, _0, _0},  // _s
                           {_0, _a, _0, _0, _0, _0, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0}}, // _o  
        [BINARY_DIV]    = {{_f, _f, _f, _0, _0, _0, _0, _0, _0},  // _b
                           {_f, _f, _f, _0, _0, _0, _0, _0, _0},  // _i
                           {_f, _f, _f, _0, _0, _0, _0, _0, _0},  // _f
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _s
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0}}, // _o  
        [BINARY_IDIV]   = {{_i, _i, _i, _0, _0, _0, _0, _0, _0},  // _b
                           {_i, _i, _i, _0, _0, _0, _0, _0, _0},  // _i
                           {_i, _i, _i, _0, _0, _0, _0, _0, _0},  // _f
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _s
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0}}, // _o  
        [BINARY_MOD]    = {{_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _b
                           {_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _i
                           {_f, _f, _f, _0, _0, _0, _0, _0, _0},  // _f
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _s
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0}}, // _o  
        [BINARY_POW]    = {{_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _b
                           {_i, _i, _f, _0, _0, _0, _0, _0, _0},  // _i
                           {_f, _f, _f, _0, _0, _0, _0, _0, _0},  // _f
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _s
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0}}, // _o  
        [BINARY_CONCAT] = {{_s, _s, _s, _s, _0, _0, _0, _0, _0},  // _b
                           {_s, _s, _s, _s, _0, _0, _0, _0, _0},  // _i
                           {_s, _s, _s, _s, _0, _0, _0, _0, _0},  // _f
                           {_s, _s, _s, _s, _0, _0, _0, _0, _0},  // _s
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _a
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _m
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _F
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0},  // _c
                           {_0, _0, _0, _0, _0, _0, _0, _0, _0}}, // _o  
        //     RHS type =   _b, _i, _f, _s, _a, _m, _F, _c, _o       LHS type
        
#define BITWISE(op) [op] = {{_i, _i, _0, _0, _0, _0, _0, _0, _0}, \
                            {_i, _i, _0, _0, _0, _0, _0, _0, _0}, \
                            {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                            {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                            {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                            {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                            {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                            {_0, _0, _0, _0, _0, _0, _0, _0, _0}, \
                            {_0, _0, _0, _0, _0, _0, _0, _0, _0}} 
        BITWISE(BINARY_BXOR),                                                                 
        BITWISE(BINARY_BAND),                                                                 
        BITWISE(BINARY_BOR),                                                                 
        BITWISE(BINARY_SHL),                                                                 
        BITWISE(BINARY_SHR),                                                                 
    };
    // clang-format on

    // 'e->lhs' and 'e->rhs' already visited
    const Type *lhs = e->lhs->type;
    const Type *rhs = e->rhs->type;
    const int result = kValidOps[e->op][lhs->code][rhs->code];
    if (result < 0) {
//        return try_meta_binop(fn, ex->op, lhs, rhs);
        type_error(fn);
    }
    return get_type(fn, result);
}

#undef _0
#undef _b
#undef _i
#undef _f
#undef _s
#undef _a
#undef _m
#undef _F
#undef _c
#undef _o

static const Type *check_expr(FnState *fn, Expr *expr)
{
    // fill in the expression type
    paw_Env *P = env(fn->lex);
    switch (expr->kind) {
        case EXPR_PRIMITIVE: {
            const PrimitiveExpr *ex = cast_to(expr, PrimitiveExpr);
            expr->type = &P->tv.data[api_type(ex->v)];
            break;
        }
        case EXPR_VAR: {
            const VarExpr *ex = cast_to(expr, VarExpr);
            const VarInfo info = find_var(fn, ex->name);
            expr->type = info.type;
            break;
        }
        case EXPR_UNOP: 
            expr->type = check_unop(fn, cast_to(expr, UnOpExpr));
            break;
        case EXPR_BINOP:
            expr->type = check_binop(fn, cast_to(expr, BinOpExpr));
            break;
    }
    return expr->type;
}

// ********************
//     AST visitors
// ********************

static void visit_stmt(FnState *fn, Stmt *stmt);
static void visit_expr(FnState *fn, Expr *expr);

// Register the name and type of a variable
// If 'global' is true, then the variable is a global, otherwise, it is a local.
// Must be called prior to 'define_var',
static VarDesc *declare_var(FnState *fn, String *name, TypeTag tag, paw_Bool global)
{
    return global ? add_global(fn, name, tag)
                  : add_local(fn, name, tag);
}

// Allocate the variable and allow access to it
static void define_var(FnState *fn, String *name, paw_Bool global)
{
    if (global) {
        paw_assert(0); // FIXME: Register in globals list right now, set default value
//        pawK_code_U(fn, OP_GLOBAL, info.index);
    } else {
        begin_local_scope(fn, 1);
    }
}

// Push a variable on to the stack
static void discharge_var(FnState *fn, Expr *expr)
{
    if (expr->kind == EXPR_VAR) {
        const VarExpr *vn = cast_to(expr, VarExpr);
        const struct VarInfo info = find_var(fn, vn->name);
        switch (info.kind) {
            case VAR_LOCAL:
                pawK_code_U(fn, OP_GETLOCAL, info.index);
                break;
            case VAR_UPVALUE:
                pawK_code_U(fn, OP_GETUPVALUE, info.index);
                break;
            default:
                paw_assert(info.kind == VAR_GLOBAL);
                pawK_code_U(fn, OP_GETGLOBAL, info.index);
        }
    } else if (expr->kind == EXPR_INDEX) {
        const IndexExpr *nn = cast_to(expr, IndexExpr);
        visit_expr(fn, nn->first);
        if (nn->second) {
            visit_expr(fn, nn->second);
            pawK_code_0(fn, OP_GETSLICE);
        } else {
            pawK_code_0(fn, OP_GETITEM);
        }
    } else {
        paw_assert(expr->kind == EXPR_FIELD);
        const FieldExpr *nn = cast_to(expr, FieldExpr);
        pawK_code_U(fn, OP_PUSHCONST, nn->index);
        pawK_code_0(fn, OP_GETITEM);
    }
}

static void assign_var(FnState *fn, Expr *lhs, Expr *rhs)
{
    if (lhs->kind == EXPR_PRIMITIVE ||
        lhs->kind == EXPR_ARRAY ||
        lhs->kind == EXPR_MAP) {
        // prevent assignment to literals (container literals are considered
        // suffixed expressions, since we allow syntax like 'let c = "abc"[0]').
        pawX_error(fn->lex, "assignment to literal");
    } else if (lhs->kind == EXPR_VAR) {
        // variable assignment
        visit_expr(fn, rhs);
        VarExpr *e = cast_to(lhs, VarExpr);
        const struct VarInfo info = find_var(fn, e->name);
        type_check_same(fn, info.type, rhs->type);
        switch (info.kind) {
            case VAR_LOCAL:
                pawK_code_U(fn, OP_SETLOCAL, info.index);
                break;
            case VAR_UPVALUE:
                pawK_code_U(fn, OP_SETUPVALUE, info.index);
                break;
            default:
                paw_assert(info.kind == VAR_GLOBAL);
                pawK_code_U(fn, OP_SETGLOBAL, info.index);
        }
        return;
    }

    // index, range, or field assignment
    SuffixedExpr *base = cast_to(lhs, SuffixedExpr); // common base
    discharge_var(fn, base->target); // up to last expression
    visit_expr(fn, rhs);
    if (lhs->kind == EXPR_INDEX) {
        const IndexExpr *last = cast_to(lhs, IndexExpr);
        visit_expr(fn, last->first);
        if (last->second) {
            visit_expr(fn, last->second);
            visit_expr(fn, rhs);
            pawK_code_0(fn, OP_SETSLICE);
            type_check_same(fn, last->type, rhs->type);
        } else {
            visit_expr(fn, rhs);
            pawK_code_0(fn, OP_SETITEM);
            type_check_same(fn, last->type, rhs->type);
        }
        const Type *tx = lhs->type;
        const Type *ty = rhs->type;
        // TODO: check types for index
    } else {
        paw_assert(lhs->kind == EXPR_FIELD);
        const FieldExpr *nn = cast_to(lhs, FieldExpr);
        pawK_code_U(fn, OP_PUSHCONST, nn->index);
        visit_expr(fn, rhs);
        pawK_code_0(fn, OP_SETATTR);
        const Type *tx = lhs->type;
        const Type *ty = rhs->type;
        // TODO: check types for field access
    }
}

static void visit_expr_vec(FnState *fn, NodeVec nvec)
{
    for (int i = 0; i < nvec.size; ++i) {
        visit_expr(fn, cast_expr(nvec.nodes[i]));
    }
}

static void visit_var(FnState *fn, Expr *target)
{
    VarExpr *e = cast_to(target, VarExpr);
    const VarInfo info = find_var(fn, e->name);
    discharge_var(fn, cast_expr(e));
    e->type = info.type;
}

static void visit_literal(FnState *fn, Expr *target)
{
    PrimitiveExpr *e = cast_to(target, PrimitiveExpr);
    if (pawV_is_null(e->v)) {
        pawK_code_0(fn, OP_PUSHNULL);
        e->type = kTag0;
    } else if (pawV_is_true(e->v)) {
        pawK_code_0(fn, OP_PUSHTRUE);
        e->type = get_type(fn, PAW_TBOOL);
    } else if (pawV_is_false(e->v)) {
        pawK_code_0(fn, OP_PUSHFALSE);
        e->type = get_type(fn, PAW_TBOOL);
    } else {
        const int index = add_constant(fn->lex, e->v);
        pawK_code_U(fn, OP_PUSHCONST, index);
        if (pawV_is_float(e->v)) {
            e->type = get_type(fn, PAW_TFLOAT); 
        } else if (pawV_is_number(e->v)) { // int or bigint
            e->type = get_type(fn, PAW_TINT);
        } else {
            e->type = get_type(fn, PAW_TSTRING); 
        }
    }
}

static void visit_and(FnState *fn, LogicalExpr *expr)
{
    visit_expr(fn, expr->lhs);
    const int jump = code_jump(fn, OP_JUMPFALSE);
    pawK_code_0(fn, OP_POP);
    visit_expr(fn, expr->rhs);
    patch_here(fn, jump);
}

static void visit_or(FnState *fn, LogicalExpr *expr)
{
    visit_expr(fn, expr->lhs);
    const int else_jump = code_jump(fn, OP_JUMPFALSE);
    const int then_jump = code_jump(fn, OP_JUMP);
    patch_here(fn, else_jump);
    pawK_code_0(fn, OP_POP);
    visit_expr(fn, expr->rhs);
    patch_here(fn, then_jump);
}

static void visit_logical(FnState *fn, LogicalExpr *expr)
{
    if (expr->is_and) {
        visit_and(fn, expr);
    } else {
        visit_or(fn, expr);
    }
    expr->type = get_type(fn, PAW_TBOOL);
}

static void visit_chain(FnState *fn, ChainExpr *expr)
{
    visit_expr(fn, expr->target);
    paw_unused(expr); // no data
    const int else_jump = code_jump(fn, OP_JUMPNULL);
    const int then_jump = code_jump(fn, OP_JUMP);
    patch_here(fn, else_jump);
    // Return the value on top, which is either 'null', or an instance that
    // returned 'null' from its '__null' metamethod.
    pawK_code_0(fn, OP_RETURN);
    patch_here(fn, then_jump);
    // ChainExpr has no type
}

static void visit_cond(FnState *fn, Expr *input)
{
    CondExpr *e = cast_to(input, CondExpr);
    visit_expr(fn, e->cond);
    const int else_jump = code_jump(fn, OP_JUMPFALSEPOP);
    visit_expr(fn, e->lhs);
    const int then_jump = code_jump(fn, OP_JUMP);
    patch_here(fn, else_jump);
    visit_expr(fn, e->rhs);
    patch_here(fn, then_jump);
    if (pawY_common(e->lhs->type, e->rhs->type, &e->type)) {
        type_error(fn); 
    }
}

static void visit_coalesce(FnState *fn, Expr *input)
{
    CoalesceExpr *e = cast_to(input, CoalesceExpr);
    visit_expr(fn, e->lhs);
    const int else_jump = code_jump(fn, OP_JUMPNULL);
    const int then_jump = code_jump(fn, OP_JUMP);
    patch_here(fn, else_jump);
    pawK_code_0(fn, OP_POP);
    visit_expr(fn, e->rhs);
    patch_here(fn, then_jump);
    if (pawY_common(e->lhs->type, e->rhs->type, &e->type)) {
        type_error(fn); 
    }
}

static void visit_unop(FnState *fn, Expr *target)
{
    UnOpExpr *e = cast_to(target, UnOpExpr);
    visit_expr(fn, e->target);
    e->type = check_unop(fn, e);
    switch (e->op) {
        case UN_LEN:
            pawK_code_U(fn, OP_UNOP, UNARY_LEN);
            break;
        case UN_NEG:
            pawK_code_U(fn, OP_UNOP, UNARY_NEG);
            break;
        case UN_NOT:
            pawK_code_U(fn, OP_UNOP, UNARY_NOT);
            break;
        default:
            paw_assert(e->op == UN_BNOT);
            pawK_code_U(fn, OP_UNOP, UNARY_BNOT);
    }
}

static void visit_binop(FnState *fn, Expr *target)
{
    BinOpExpr *e = cast_to(target, BinOpExpr);
    visit_expr(fn, e->lhs);
    visit_expr(fn, e->rhs);
    e->type = check_binop(fn, e);
    switch (e->op) {
        case BINARY_ADD:
            pawK_code_U(fn, OP_BINOP, BINARY_ADD);
            break;
        case BINARY_SUB:
            pawK_code_U(fn, OP_BINOP, BINARY_SUB);
            break;
        case BINARY_MUL:
            pawK_code_U(fn, OP_BINOP, BINARY_MUL);
            break;
        case BINARY_DIV:
            pawK_code_U(fn, OP_BINOP, BINARY_DIV);
            break;
        case BINARY_IDIV:
            pawK_code_U(fn, OP_BINOP, BINARY_IDIV);
            break;
        case BINARY_MOD:
            pawK_code_U(fn, OP_BINOP, BINARY_MOD);
            break;
        case BINARY_CONCAT:
            pawK_code_U(fn, OP_BINOP, BINARY_CONCAT);
            break;
        case BINARY_EQ:
            pawK_code_U(fn, OP_BINOP, BINARY_EQ);
            break;
        case BINARY_NE:
            pawK_code_U(fn, OP_BINOP, BINARY_NE);
            break;
        case BINARY_LE:
            pawK_code_U(fn, OP_BINOP, BINARY_LE);
            break;
        case BINARY_GE:
            pawK_code_U(fn, OP_BINOP, BINARY_GE);
            break;
        case BINARY_LT:
            pawK_code_U(fn, OP_BINOP, BINARY_LT);
            break;
        case BINARY_GT:
            pawK_code_U(fn, OP_BINOP, BINARY_GT);
            break;
        case BINARY_SHL:
            pawK_code_U(fn, OP_BINOP, BINARY_SHL);
            break;
        case BINARY_SHR:
            pawK_code_U(fn, OP_BINOP, BINARY_SHR);
            break;
        case BINARY_BXOR:
            pawK_code_U(fn, OP_BINOP, BINARY_BXOR);
            break;
        case BINARY_BAND:
            pawK_code_U(fn, OP_BINOP, BINARY_BAND);
            break;
        case BINARY_BOR:
            pawK_code_U(fn, OP_BINOP, BINARY_BOR);
            break;
        default:
            pawK_code_U(fn, OP_BINOP, BINARY_IN);
            break;
    }
}

static void visit_exprstmt(FnState *fn, ExprStmt *s)
{
    if (!s->rhs) {
        visit_expr(fn, s->lhs); // code function call
        pawK_code_0(fn, OP_POP); // unused return
    } else {
        assign_var(fn, s->lhs, s->rhs); // code assignment
    }
}

static void visit_block(FnState *fn, Block *b)
{
    BlkState bs;
    enter_block(fn, &bs, PAW_FALSE);
    const NodeVec *stmts = &b->stmts;
    for (int i = 0; i < stmts->size; ++i) {
        visit_stmt(fn, cast_stmt(stmts->nodes[i]));
    }
    leave_block(fn);
}

static void visit_param(FnState *fn, Stmt *stmt)
{
    ParamStmt *s = cast_to(stmt, ParamStmt);
    declare_var(fn, s->name, s->tag, PAW_FALSE);
    define_var(fn, s->name, PAW_FALSE);
    paw_assert(s->tag && !is_type0(s->tag));
}

static void visit_def(FnState *fn, Stmt *stmt)
{
    DefStmt *s = cast_to(stmt, DefStmt);
    const paw_Bool global = s->flags & DEF_GLOBAL;
    VarDesc *var = declare_var(fn, s->name, s->tag, global);
    visit_expr(fn, s->init);
    define_var(fn, s->name, global);
    paw_assert(!is_type0(s->tag));

    Lex *lex = fn->lex;
    TypeTag init = s->init->type;
    if (s->tag == NULL) {
        var->type = init; // infer from initializer
        if (var->type == NULL) {
            pawX_error(lex, "unable to infer variable type");
        }
    } else if (!pawY_is_similar(init, s->tag)) {
        pawX_error(lex, "initializer incompatible with type annotation");
    }
}

static void visit_param_vec(FnState *fn, NodeVec param)
{
    for (int i = 0; i < param.size; ++i) {
        DefStmt *s = cast_to(param.nodes[i], DefStmt);
        paw_assert(s->tag != NULL); // checked by first pass
        add_local(fn, s->name, s->tag);
    }
    begin_local_scope(fn, param.size);
}

static void visit_return(FnState *fn, ReturnStmt *stmt)
{
    visit_expr(fn, stmt->expr);
    pawK_code_0(fn, OP_RETURN);

    TypeTag ret = stmt->expr->type; // actual return type
    TypeTag tag = fn->ret; // return type annotation
    if (tag == NULL) { // needs inference
        fn->ret = ret; // may be NULL ('return null' or 'return')
    } else if (!pawY_is_similar(ret, tag)) {
        pawX_error(fn->lex, "return type incompatible with annotation");
    }
}

static void visit_call(FnState *fn, Expr *expr)
{
    CallExpr *e = cast_to(expr, CallExpr);
    visit_expr(fn, e->target);
    visit_expr_vec(fn, e->param);
    pawK_code_U(fn, OP_CALL, e->param.size);

    // TODO: Resolve overloads
    TypeTag tag = e->target->type;
    if (tag->code != PAW_TFUNCTION) {
        pawX_error(fn->lex, "type is not callable");
    }
    const FnType type = tag->f;
    expr->type = type.ret; // propagate return type
    if (type.nparam != e->param.size) {
        pawX_error(fn->lex, "expected %d parameters but found %d", 
                   type.nparam, e->param.size);
    }
    for (int i = 0; i < type.nparam; ++i) {
        TypeTag tag = type.param[i];
        Expr *param = cast_expr(e->param.nodes[i]);
        if (!pawY_is_similar(param->type, tag)) {
            pawX_error(fn->lex, "invalid parameter type");
        }
    }
}

static Type *visit_fn_aux(FnState *caller, FnKind kind, String *name, NodeVec param, TypeTag *pret, Block *body)
{
    BlkState bs;
    Lex *lex = caller->lex;
    paw_Env *P = env(lex);
    FnState fn = {
        .name = name,
        .ret = *pret,
        .param = &param, // temporary
    };
    const int id = add_proto(lex, name, &fn.proto);
    fn.proto->argc = param.size;
    enter_function(lex, &fn, &bs, kind);
    visit_param_vec(&fn, param); // code parameters
    visit_block(&fn, body); // code function body
    leave_function(lex);

    // Create, and allow access to, the closure object.
    code_closure(caller, fn.proto, id);

    // Register the function type.
    TypeTag *ptypes = NULL;
    if (param.size > 0) {
        ptypes = pawM_new_vec(P, param.size, TypeTag);
        for (int i = 0; i < param.size; ++i) {
            ptypes[i] = cast_expr(param.nodes[i])->type;
        }
    }
    Type *t = pawY_register_function(env(lex), ptypes, param.size, fn.ret);
    fn.proto->type = &t->f;
    *pret = fn.ret;
    return t;
}

// TODO: Pass actual FnKind enumerator
#define visit_fn(caller, name, kind, node) visit_fn_aux(caller, kind, name, (node)->param, \
                                                        &(node)->ret, (node)->body)

static void visit_fnexpr(FnState *fn, Expr *expr)
{
    Lex *lex = fn->lex;
    FnExpr *e = cast_to(expr, FnExpr);
    String *name = scan_string(lex, "(anonymous fn)");
    e->type = visit_fn(fn, name, FN_FUNCTION, e);
}

static void visit_fnstmt(FnState *fn, Stmt *stmt)
{
    FnStmt *s = cast_to(stmt, FnStmt);
    Type *t = visit_fn(fn, s->name, FN_FUNCTION, s);

    const paw_Bool global = s->flags & DEF_GLOBAL;
    declare_var(fn, s->name, t, global);
    define_var(fn, s->name, global);
}

static void visit_ifelse(FnState *fn, IfElseStmt *stmt)
{
    visit_expr(fn, stmt->cond);
    const int else_jump = code_jump(fn, OP_JUMPFALSEPOP);
    visit_stmt(fn, stmt->then_arm);
    // NOTE: If there is not 'else' block, this will produce a NOOP jump.
    const int then_jump = code_jump(fn, OP_JUMP);
    patch_here(fn, else_jump);
    visit_stmt(fn, stmt->else_arm);
    patch_here(fn, then_jump);
}

static void close_until_loop(FnState *fn, BlkState *bs)
{
    Lex *lex = fn->lex;
    while (bs->outer) {
        // Emit close/pop instructions, but don't end any lifetimes. Code
        // paths that doesn't hit this label may still need those locals.
        bs = bs->outer;
        if (bs->is_loop) {
            close_vars(fn, bs->level);
            return;
        }
    }
    pawX_error(lex, "label outside loop");
}

static void visit_label(FnState *fn, Stmt *stmt)
{
    LabelStmt *s = cast_to(stmt, LabelStmt);
    close_until_loop(fn, fn->blk); // fix the stack
    add_label(fn, s->label); // emit a jump, to be patched later
}

static void visit_while(FnState *fn, WhileStmt *stmt)
{
    BlkState bs;
    enter_block(fn, &bs, PAW_TRUE);
    const int loop = fn->pc;
    visit_expr(fn, stmt->cond);

    const int jump = code_jump(fn, OP_JUMPFALSEPOP);
    visit_block(fn, stmt->block);

    // Finish the loop. 'break' labels jump here, 'continue' labels back to right
    // before where the conditional expression was evaluated.
    code_loop(fn, OP_JUMP, loop);
    adjust_to(fn, LCONTINUE, loop);
    patch_here(fn, jump);
    leave_block(fn);
}

static void visit_dowhile(FnState *fn, WhileStmt *stmt)
{
    BlkState bs;
    enter_block(fn, &bs, PAW_TRUE);
    const int loop = fn->pc;
    visit_block(fn, stmt->block);
    adjust_from(fn, LCONTINUE);
    visit_expr(fn, stmt->cond);

    // If the condition is false, jump over the instruction that moves control back
    // to the top of the loop.
    const int jump = code_jump(fn, OP_JUMPFALSEPOP);
    code_loop(fn, OP_JUMP, loop);
    patch_here(fn, jump);
    leave_block(fn);
}

static void visit_forbody(FnState *fn, Block *block, Op opinit, Op oploop)
{
    // Emit OP_FOR*0, which either pushes the loop variables, or jumps over
    // the loop.
    const int jump = code_jump(fn, opinit);
    const int loop = fn->pc;

    // Enter a block for the loop variable.
    BlkState bs;
    enter_block(fn, &bs, PAW_FALSE);
    begin_local_scope(fn, 1);

    visit_block(fn, block);
    leave_block(fn);

    // Continue statements jump here, right before the loop instruction.
    adjust_from(fn, LCONTINUE);
    code_loop(fn, oploop, loop);
    patch_here(fn, jump);
}

static void visit_fornum(FnState *fn, ForNumStmt *stmt)
{
    // Create the control variables.
    new_local_literal(fn, "(for begin)", PAW_TINT);
    new_local_literal(fn, "(for end)", PAW_TINT);
    new_local_literal(fn, "(for step)", PAW_TINT);

    // Parse the loop bounds ('begin', 'end', and 'step' expressions).
    visit_expr(fn, stmt->begin);
    visit_expr(fn, stmt->end);
    visit_expr(fn, stmt->step);
    begin_local_scope(fn, 3);

    type_check_integral(fn, stmt->begin->type);
    type_check_integral(fn, stmt->end->type);
    type_check_integral(fn, stmt->step->type);
    visit_forbody(fn, stmt->block, OP_FORNUM0, OP_FORNUM);
}

static void visit_forin(FnState *fn, ForInStmt *stmt)
{
    new_local_literal(fn, "(for target)", PAW_TINT);
    new_local_literal(fn, "(for iterator)", PAW_TINT);
    visit_expr(fn, stmt->target);

    begin_local_scope(fn, 2);
    visit_forbody(fn, stmt->block, OP_FORIN0, OP_FORIN);
}

static void visit_expr(FnState *fn, Expr *expr)
{
    if (expr == NULL) {
        return;
    }
    switch (expr->kind) {
        case EXPR_PRIMITIVE:
            visit_literal(fn, expr);
            break;
        case EXPR_CHAIN:
            visit_chain(fn, cast_to(expr, ChainExpr));
            break;
        case EXPR_LOGICAL:
            visit_logical(fn, cast_to(expr, LogicalExpr));
            break;
        case EXPR_UNOP:
            visit_unop(fn, expr);
            break;
        case EXPR_BINOP:
            visit_binop(fn, expr);
            break;
        case EXPR_CALL:
            visit_call(fn, expr);
            break;
        case EXPR_COND:
            visit_cond(fn, expr);
            break;
        case EXPR_FN:
            visit_fnexpr(fn, expr);
            break;
        case EXPR_VAR:
            visit_var(fn, expr);
            break;
        //case EXPR_ARRAY:
        //    visit_array(fn, expr);
        //    break;
        //case EXPR_MAP:
        //    visit_map(fn, expr);
        //    break;
            //        case EXPR_INDEX:
            //            visit_index(fn, cast_to(expr, IndexExpr));
            //            break;
            //        case EXPR_FIELD:
            //            visit_field(fn, cast_to(expr, FieldExpr));
            //            break;
        case EXPR_COALESCE:
            visit_coalesce(fn, expr);
            break;
        default: {
            volatile char *x = 0; // FIXME
            *x = 123; // FIXME
        }

        break;
    }
}

static void visit_stmt(FnState *fn, Stmt *stmt)
{
    if (stmt == NULL) {
        return;
    }
    switch (stmt->kind) {
        case STMT_EXPR:
            visit_exprstmt(fn, cast_to(stmt, ExprStmt));
            break;
        case STMT_BLOCK:
            visit_block(fn, cast_to(stmt, Block));
            break;
        case STMT_RETURN:
            visit_return(fn, cast_to(stmt, ReturnStmt));
            break;
        case STMT_IFELSE:
            visit_ifelse(fn, cast_to(stmt, IfElseStmt));
            break;
        case STMT_FORIN:
            visit_forin(fn, cast_to(stmt, ForInStmt));
            break;
        case STMT_FORNUM:
            visit_fornum(fn, cast_to(stmt, ForNumStmt));
            break;
        case STMT_WHILE:
            visit_while(fn, cast_to(stmt, WhileStmt));
            break;
        case STMT_DOWHILE:
            visit_dowhile(fn, cast_to(stmt, WhileStmt));
            break;
        case STMT_LABEL:
            visit_label(fn, stmt);
            break;
        case STMT_FN:
            visit_fnstmt(fn, stmt);
            break;
        case STMT_PARAM:
            visit_param(fn, stmt);
            break;
        case STMT_DEF:
            visit_def(fn, stmt);
            break;
        default: {
            volatile char *x = 0; // FIXME
            *x = 123; // FIXME
        }

        break;
    }
}

// All paw language keywords (must be in this order, the same order as the
// keyword variants in the TokenKind enum in lex.h)
static const char *kKeywords[] = {
    "fn",
    "class",
    "super",
    "include",
    "global",
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
    for (Metamethod mm = 0; mm < NMETAMETHODS; ++mm) {
        const char *name = pawT_name(mm);
        String *str = new_fixed_string(P, name);
        pawV_set_string(&P->meta_keys[mm], str);
    }
    pawV_set_string(&P->str_cache[CSTR_SELF], new_fixed_string(P, "self"));
    pawV_set_string(&P->str_cache[CSTR_INIT], pawS_new_str(P, "__init"));
    pawV_set_string(&P->str_cache[CSTR_SUPER], pawS_new_str(P, "super"));
    pawV_set_string(&P->str_cache[CSTR_TRUE], pawS_new_str(P, "true"));
    pawV_set_string(&P->str_cache[CSTR_FALSE], pawS_new_str(P, "false"));
    pawV_set_string(&P->str_cache[CSTR_NULL], pawS_new_str(P, "null"));
}

static void skip_hashbang(Lex *lex)
{
    if (test_next(lex, '#') && test_next(lex, '!')) {
        char c;
        do {
            c = lex->c;
            skip(lex);
        } while (!ISNEWLINE(c));
    }
}

static Block *parse_module(Lex *lex)
{
    Tree *ast = lex->ast;
    Block *root = pawK_add_node(lex, STMT_BLOCK, Block);
    ast->root = cast_node(root);

    skip_hashbang(lex);
    stmtlist(lex, &root->stmts);
    check(lex, TK_END);
    return root;
}

static void dump_expr(paw_Env *P, Expr *expr, int indent);

static void dump_stmt(paw_Env *P, Stmt *stmt, int indent)
{
    for (int i = 0; i < indent; ++i) {
        printf("    ");
    }
    if (stmt == NULL) {
        puts("NULL\n");
        return;
    }
    switch (stmt->kind) {
        case STMT_EXPR:
            printf("exprstmt\n");
            dump_expr(P, cast_expr(cast_to(stmt, ExprStmt)->lhs), indent + 1);
            if (cast_to(stmt, ExprStmt)->rhs) {
                dump_expr(P, cast_to(stmt, ExprStmt)->rhs, indent + 1);
            }
            break;
        case STMT_BLOCK: {
            Block *bn = cast_to(stmt, Block);
            NodeVec *nvec = &bn->stmts;
            printf("block\n");
            for (int i = 0; i < nvec->size; ++i) {
                dump_stmt(P, cast_stmt(nvec->nodes[i]), indent + 1);
            }
            break;
        }
        case STMT_IFELSE:
            printf("ifelse\n");
            break;
        case STMT_FORIN:
            printf("forin\n");
            break;
        case STMT_FORNUM:
            printf("fornum\n");
            break;
        case STMT_WHILE:
            printf("while\n");
            break;
        case STMT_FN: {
            FnStmt *fn = cast_to(stmt, FnStmt);
            printf("fn (%s) %s: %d\n", fn->global ? "global" : "local", fn->name->text, fn->ret ? fn->ret->code : -1);
            for (int i = 0; i < fn->param.size; ++i) {
                dump_stmt(P, cast_stmt(fn->param.nodes[i]), indent + 1); // list of DefStmt
            }
            dump_stmt(P, cast_stmt(fn->body), indent + 1);
            break;
        }
        case STMT_DEF: {
            DefStmt *in = cast_to(stmt, DefStmt);
            printf("%s %s: %d\n", in->flags & DEF_GLOBAL ? "global" : "let", in->name->text, in->tag ? in->tag->code : 42);
            dump_expr(P, in->init, indent + 1);
            break;
        }
        case STMT_RETURN: {
            ReturnStmt *bn = cast_to(stmt, ReturnStmt);
            printf("return\n");
            dump_expr(P, bn->expr, indent + 1);
            break;
        }
        default:
            printf("?\n");
            break;
    }
}

static void dump_expr(paw_Env *P, Expr *expr, int indent)
{
    for (int i = 0; i < indent; ++i) {
        printf("    ");
    }
    if (expr == NULL) {
        puts("NULL\n");
        return;
    }
    if (expr->type) {
        printf("type (%s): ", api_typename(expr->type->code));
    } else {
        printf("NULL ");
    }
    switch (expr->kind) {
        case EXPR_PRIMITIVE: {
            Buffer buf;
            pawL_init_buffer(P, &buf);
            PrimitiveExpr *ln = cast_to(expr, PrimitiveExpr);
            pawC_pushv(P, ln->v);
            pawL_add_value(P, &buf);
            pawL_add_char(P, &buf, '\0');
            printf("literal %s\n", buf.data);
            pawL_discard_result(P, &buf);
            break;
        }
        case EXPR_UNOP:
            printf("unop %d\n", cast_to(expr, UnOpExpr)->op);
            dump_expr(P, cast_to(expr, UnOpExpr)->target, indent + 1);
            break;
        case EXPR_BINOP:
            printf("binop %d\n", cast_to(expr, BinOpExpr)->op);
            dump_expr(P, cast_to(expr, BinOpExpr)->lhs, indent + 1);
            dump_expr(P, cast_to(expr, BinOpExpr)->rhs, indent + 1);
            break;
        case EXPR_CALL:
            printf("call\n");
            break;
        case EXPR_COND:
            printf("cond\n");
            dump_expr(P, cast_to(expr, CondExpr)->cond, indent + 1);
            dump_expr(P, cast_to(expr, CondExpr)->lhs, indent + 1);
            dump_expr(P, cast_to(expr, CondExpr)->rhs, indent + 1);
            break;
        case EXPR_VAR: {
            printf("var '%s'\n", cast_to(expr, VarExpr)->name->text);
            break;
        }
        default:
            printf("?\n");
            break;
    }
}

#include <stdlib.h>
static void dump_ast(Lex *lex, Block *root)
{
    NodeVec nvec = root->stmts;
    for (int i = 0; i < nvec.size; ++i) {
        dump_stmt(lex->P, cast_stmt(nvec.nodes[i]), 0);
    }
}

static void check_types(Lex *lex, Block *root)
{
    // TODO: type checking pass
}

static void generate_code(Lex *lex, Block *root)
{
    BlkState blk;
    FnState fn = {
        .name = lex->modname,
        .proto = lex->main->p,
        .lex = lex,
    };
    enter_function(lex, &fn, &blk, FN_MODULE);
    visit_block(&fn, root); // type check + codegen
    leave_function(lex);

    paw_assert(lex->pm->locals.size == 0);
    paw_assert(fn.level == 0);

    dump_ast(lex, root); // TODO: remove
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
    lex.ast = pawK_new_ast(P);
    // TODO: AST needs to go on the stack in a Foreign object. We will leak the
    //       AST if an error is thrown between now and when it is freed below.
    //       Use a flag stored somewhere in the object to disambiguate between
    //       other Foreign objects and ASTs, call pawK_free_ast() on ASTs from
    //       the GC routine.

    // Create the main closure and push it onto the stack so that the garbage
    // collector can find it.
    Value *pv = pawC_push0(P);
    lex.main = pawV_new_closure(P, 1);
    pawV_set_closure(pv, lex.main);
    Proto *f = pawV_new_proto(P);
    lex.main->p = f;

    // Do the same for the lexer string map. Strings are reachable from this map
    // during the parse. Once the parse is finished, all strings should be
    // anchored somewhere.
    pv = pawC_push0(P);
    lex.strings = pawH_new(P);
    pawV_set_map(pv, lex.strings);

    // Store the module name.
    String *modname = pawS_new_str(P, name);
    lex.modname = modname;
    f->modname = modname;
    f->name = modname;

    // Load the first token.
    skip(&lex);

    Block *result = parse_module(&lex); // pass 1 (source -> AST)
    generate_code(&lex, result); // pass 2 (AST -> bytecode)

    // Pop the lexer map. The strings it contains should be anchored elsewhere.
    // Leave the main closure on top of the stack.
    pawC_stkdec(P, 1);

    // cleanup
    pawK_free_ast(P, lex.ast);
    return lex.main;
}
