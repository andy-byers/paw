// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "array.h"
#include "check.h"
#include "code.h"
#include "env.h"
#include "gc.h"
#include "map.h"
#include "mem.h"
#include "str.h"
#include "type.h"

static void push_symbol_table(Lex *lex)
{
    pawP_add_scope(lex, &lex->pm->scopes);
}

static void pop_symbol_table(Lex *lex)
{
    // Last symbol table should have been assigned to an AST node. The
    // next call to push_symbol_table() will allocate a new table.
    ScopeTable *st = &lex->pm->scopes;
    paw_assert(st->size > 0);
    --st->size;
}

static SymbolTable *get_symbols(Lex *lex)
{
    ScopeTable *st = &lex->pm->scopes;
    return st->data[st->size - 1];
}

static Symbol *register_var(Lex *lex, SymbolTable *st, String *name, TypeTag tag)
{
    Symbol *s = pawP_add_symbol(lex, st);
    *s = (Symbol){
        .kind = SYM_VARIABLE,
        .name = name,
        .type = tag,
    };
    return s;
}

static Symbol *add_local(Lex *lex, String *name, TypeTag type)
{
    SymbolTable *st = get_symbols(lex);
    return register_var(lex, st, name, type);
}

static Symbol *add_global(Lex *lex, String *name, TypeTag type)
{
    SymbolTable *st = lex->globals;
    for (int i = 0; i < st->size; ++i) {
        if (pawS_eq(st->data[i].name, name)) {
            pawX_error(lex, "duplicate global '%s'", name->text); 
        }
    }
    return register_var(lex, st, name, type);
}

static Symbol *resolve_symbol(Lex *lex, String *name)
{
    ScopeTable *scopes = &lex->pm->scopes;
    for (int depth = scopes->size - 1; depth >= 0; --depth) {
        SymbolTable *st = scopes->data[depth];
        const int index = pawP_find_symbol(st, name);
        if (index >= 0) {
            Symbol *s = pawP_get_symbol(st, index);
            if (st->fn_depth < lex->fn_depth) {
                s->flags.captured = PAW_TRUE;
            }
            return s;
        }
    }
    const int index = pawP_find_symbol(lex->globals, name);
    if (index < 0) {
        pawX_error(lex, "undefined variable '%s'", name->text);
    }
    return pawP_get_symbol(lex->globals, index);
}

static Symbol *resolve_var(Lex *lex, String *name)
{
    Symbol *var = resolve_symbol(lex, name);
    if (var->kind != SYM_VARIABLE) {
        pawX_error(lex, "identifier '%s' is not a variable", var->name->text);
    }
    return var;
}

// Register the name and type of a variable
// If 'global' is true, then the variable is a global, otherwise, it is a local.
// Must be called prior to 'define_var',
static Symbol *declare_var(Lex *lex, String *name, TypeTag tag, paw_Bool global)
{
    return global ? add_global(lex, name, tag)
                  : add_local(lex, name, tag);
}

// Allow a previously-declared variable to be accessed
static void define_var(Symbol *symbol)
{
    paw_assert(symbol->kind == SYM_VARIABLE);
    symbol->flags.init = PAW_TRUE;
}

static void new_var(Lex *lex, String *name, TypeTag tag, paw_Bool global)
{
    Symbol *symbol = declare_var(lex, name, tag, global);
    define_var(symbol);
}

static void new_local_var(Lex *lex, String *name, TypeTag tag)
{
    new_var(lex, name, tag, PAW_FALSE);
}

static SymbolTable *leave_block(Lex *lex)
{
    SymbolTable *st = get_symbols(lex);
    pop_symbol_table(lex);
    return st;
}

static void enter_block(Lex *lex)
{
    push_symbol_table(lex);
}

static SymbolTable *leave_function(Lex *lex)
{
    SymbolTable *scope = get_symbols(lex);
    pop_symbol_table(lex); // leave function body
    check_gc(env(lex));
    --lex->fn_depth;
    return scope;
}

static String *context_name(const Lex *lex, String *name, FnKind kind)
{
    if (fn_has_self(kind)) {
        return v_string(pawE_cstr(env(lex), CSTR_SELF));
    }
    return name;
}

static void enter_function(Lex *lex, String *name, FnKind kind)
{
    ++lex->fn_depth;

    // Enter the function body.
    enter_block(lex);

    // Create the context variable in slot 0. For VCLOSURE, this slot holds the closure
    // object being called. For VMETHOD, it holds the class instance that the method is
    // being called on, i.e. the implicit 'self' parameter.
    new_local_var(lex, context_name(lex, name, kind), NULL); // TODO: Fix this type in visit_fn
}

static void new_local_literal(Lex *lex, const char *name, int type)
{
    new_local_var(lex, scan_string(lex, name), e_tag(env(lex), type));
}

static void type_error(Visitor *V)
{
    pawX_error(V->lex, "invalid type");
}

static TypeTag get_type(Visitor *V, int type)
{
    if (type < 0) {
        type_error(V);
    }
    return e_tag(env(V->lex), type);
}

static void expected_type(Visitor *V, TypeTag have, TypeTag want)
{
    pawX_error(V->lex, "expected '%s' type but found '%s'", 
               pawY_name(t_type(have)), pawY_name(t_type(want)));
}

static void check_same(Visitor *V, TypeTag lhs, TypeTag rhs)
{
    if (!pawY_is_same(lhs, rhs)) {
        pawX_error(V->lex, "expected equal types but found '%s' and '%s'", 
                   pawY_name(t_type(lhs)), pawY_name(t_type(rhs))); 
    }
}

static void check_similar(Visitor *V, TypeTag lhs, TypeTag rhs)
{
    if (!pawY_is_similar(lhs, rhs)) {
        pawX_error(V->lex, "expected compatible types but found '%s' and '%s'", 
                   pawY_name(t_type(lhs)), pawY_name(t_type(rhs))); 
    }
}

static TypeTag get_common(Visitor *V, TypeTag a, TypeTag b)
{
    TypeTag common;
    if (pawY_common(a, b, &common)) {
        pawX_error(V->lex, "incompatible types '%s' and '%s'", 
                   pawY_name(t_type(a)), pawY_name(t_type(b)));
    }
    return common;
}

// Set the type of a 'null'
static void fix_null(Visitor *V, Expr *e, int want)
{
    e->type = t_type(e->type) == PAW_NULL ? get_type(V, want) : e->type;
}

static void check_primitive(Visitor *V, TypeTag t)
{
    if (!t_is_primitive(t)) {
        pawX_error(V->lex, "expected primitive ('bool', 'int', 'float', or 'string') but found '%s'", 
                   pawY_name(t_type(t))); 
    }
}

static void check_integral(Visitor *V, TypeTag t)
{
    if (!t_is_int(t) && !t_is_bool(t)) {
        pawX_error(V->lex, "expected integral ('int' or 'bool') but found '%s'", 
                   pawY_name(t_type(t))); 
    }
}

static void check_string(Visitor *V, TypeTag t)
{
    if (!t_is_string(t)) {
        pawX_error(V->lex, "expected string but found '%s'", 
                   pawY_name(t_type(t))); 
    }
}

static void check_sequence(Visitor *V, TypeTag t)
{
    if (!t_is_string(t) && !t_is_array(t)) {
        pawX_error(V->lex, "expected sequence ('string' or 'array') but found '%s'", 
                   pawY_name(t_base(t))); 
    }
}

static void check_container(Visitor *V, TypeTag t)
{
    if (!t_is_string(t) && !t_is_array(t) && !t_is_map(t)) {
        pawX_error(V->lex, "expected container ('string' or 'array', or 'map') but found '%s'", 
                   pawY_name(t_base(t))); 
    }
}

static String *meta_key(paw_Env *P, Metamethod mm)
{
    return v_string(P->meta_keys[mm]);
}

static FnType *resolve_mm(paw_Env *P, TypeTag tag, Metamethod mm)
{
    paw_assert(t_is_class(tag));
    String *name = meta_key(P, mm);
    for (int i = 0; i < tag->c.nattrs; ++i) {
        Attribute *a = &tag->c.attrs[i];
        if (pawS_eq(a->name, name) && t_is_function(a->attr)) {
            return &a->attr->f;
        }
    }
    return NULL;
}

static TypeTag try_meta_unop(Visitor *V, UnOpExpr *e, const TypeTag tag)
{
    paw_Env *P = env(V->lex);
    const Metamethod mm = unop2meta(e->op);
    FnType *sig = resolve_mm(P, tag, mm);
    if (sig->nparam != 0) {
        type_error(V);
    }
    e->mm = sig;
    return sig->ret; 
}

static TypeTag try_meta_binop(Visitor *V, BinOpExpr *e, const TypeTag lhs, const TypeTag rhs, paw_Bool is_r)
{
    paw_Env *P = env(V->lex);
    Metamethod mm = binop2meta(e->op);
    mm = is_r ? mm_get_r(mm) : mm;
    const TypeTag self = is_r ? rhs : lhs;
    const TypeTag other = is_r ? lhs : rhs;
    FnType *sig = resolve_mm(P, self, mm);
    if (sig->nparam != 1 || !pawY_is_same(other, sig->param[0])) {
        type_error(V);
    } 
    if (is_r) {
        Expr *tmp = e->lhs;     
        e->lhs = e->rhs;
        e->rhs = tmp;
    }
    e->mm = sig;
    return sig->ret; 
}

// symbols for operator type lookup tables
#define xx PAW_NULL
#define mm PAW_NTYPES
#define mR (mm + 1)
#define _b PAW_TBOOL
#define _i PAW_TINT
#define _f PAW_TFLOAT
#define _s PAW_TSTRING
#define _a PAW_TARRAY
#define _m PAW_TMAP
#define _c PAW_TCLASS
#define _o PAW_TFOREIGN
#define _T PAW_TTYPE
#define _F PAW_TFUNCTION

static TypeTag check_unop(Visitor *V, UnOpExpr *e)
{
    // clang-format off
    static const int8_t kValidOps[NUNARYOPS][PAW_NTYPES] = {
        //      type =  _b, _i, _f, _s, _a, _m, _c, _o, _T, _F
        [UNARY_LEN]  = {xx, xx, xx, _i, _i, _i, mm, mm, xx, xx},
        [UNARY_NEG]  = {_i, _i, _f, xx, xx, xx, mm, mm, xx, xx},
        [UNARY_NOT]  = {_b, _b, _b, _b, _b, _b, mm, mm, xx, xx},
        [UNARY_BNOT] = {_i, _i, xx, xx, xx, xx, mm, mm, xx, xx},
    };
    // clang-format on

    // 'e->target' already visited
    TypeTag type = e->target->type;
    const int result = kValidOps[e->op][t_base(type)];
    if (result == xx) {
        type_error(V);
    } else if (result == mm) {
        return try_meta_unop(V, e, type);
    }
    return get_type(V, result);
}

static TypeTag check_binop(Visitor *V, BinOpExpr *e)
{
    // clang-format off
    static const int8_t kValidOps[NBINARYOPS][PAW_NTYPES /* LHS */][PAW_NTYPES /* RHS */] = {
#define   EQUALITY(op) [op] = {{_b, _b, _b, xx, xx, xx, mm, xx, xx, xx}, \
                               {_b, _b, _b, xx, xx, xx, mm, xx, xx, xx}, \
                               {_b, _b, _b, xx, xx, xx, mm, xx, xx, xx}, \
                               {xx, xx, xx, _b, xx, xx, mm, xx, xx, xx}, \
                               {xx, xx, xx, xx, _b, xx, mm, xx, xx, xx}, \
                               {xx, xx, xx, xx, xx, _b, mm, xx, xx, xx}, \
                               {mm, mm, mm, mm, mm, mm, mm, mm, mm, mm}, \
                               {xx, xx, xx, xx, xx, xx, mm, _b, xx, xx}, \
                               {xx, xx, xx, xx, xx, xx, mm, xx, _b, xx}, \
                               {xx, xx, xx, xx, xx, xx, mm, xx, xx, _b}}  
        EQUALITY(BINARY_EQ),
        EQUALITY(BINARY_NE),

#define RELATIONAL(op) [op] = {{_b, _b, _b, xx, xx, xx, mm, xx, xx, xx}, \
                               {_b, _b, _b, xx, xx, xx, mm, xx, xx, xx}, \
                               {_b, _b, _b, xx, xx, xx, mm, xx, xx, xx}, \
                               {xx, xx, xx, _b, xx, xx, mm, xx, xx, xx}, \
                               {xx, xx, xx, xx, xx, xx, mm, xx, xx, xx}, \
                               {xx, xx, xx, xx, xx, xx, mm, xx, xx, xx}, \
                               {mm, mm, mm, mm, mm, mm, mm, mm, mm, mm}, \
                               {xx, xx, xx, xx, xx, xx, mm, xx, xx, xx}, \
                               {xx, xx, xx, xx, xx, xx, mm, xx, xx, xx}, \
                               {xx, xx, xx, xx, xx, xx, mm, xx, xx, xx}} 
        RELATIONAL(BINARY_LT),
        RELATIONAL(BINARY_LE),
        RELATIONAL(BINARY_GT),
        RELATIONAL(BINARY_GE),

        //     RHS type =   _b, _i, _f, _s, _a, _m, _c, _o, _T, _F       LHS type
        [BINARY_IN]     = {{xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _b
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _i
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _f
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _s
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _a
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _m
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _c
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _o    
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx},  // _T    
                           {xx, xx, xx, xx, _b, _b, mm, xx, xx, xx}}, // _F    
        [BINARY_ADD]    = {{_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _b
                           {_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _i
                           {_f, _f, _f, xx, xx, xx, mR, xx, xx, xx},  // _f
                           {xx, xx, xx, _s, xx, xx, mR, xx, xx, xx},  // _s
                           {xx, xx, xx, xx, _a, xx, mR, xx, xx, xx},  // _a
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _m
                           {mm, mm, mm, mm, mm, mm, mm, mm, mm, mm},  // _c
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _o  
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _T  
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}}, // _F  
        [BINARY_SUB]    = {{_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _b
                           {_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _i
                           {_f, _f, _f, xx, xx, xx, mR, xx, xx, xx},  // _f
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _s
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _a
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _m
                           {mm, mm, mm, mm, mm, mm, mm, mm, mm, mm},  // _c
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _o
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _T
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}}, // _F  
        [BINARY_MUL]    = {{_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _b
                           {_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _i
                           {_f, _f, _f, xx, xx, xx, mR, xx, xx, xx},  // _f
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _s
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _a
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _m
                           {mm, mm, mm, mm, mm, mm, mm, mm, mm, mm},  // _c
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _o
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _T
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}}, // _F  
        [BINARY_DIV]    = {{_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _b
                           {_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _i
                           {_f, _f, _f, xx, xx, xx, mR, xx, xx, xx},  // _f
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _s
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _a
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _m
                           {mm, mm, mm, mm, mm, mm, mm, mm, mm, mm},  // _c
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _o
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _T
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}}, // _F  
        [BINARY_MOD]    = {{_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _b
                           {_i, _i, _f, xx, xx, xx, mR, xx, xx, xx},  // _i
                           {_f, _f, _f, xx, xx, xx, mR, xx, xx, xx},  // _f
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _s
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _a
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _m
                           {mm, mm, mm, mm, mm, mm, mm, mm, mm, mm},  // _c
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _o
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx},  // _T
                           {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}}, // _F  
        //     RHS type =   _b, _i, _f, _s, _a, _m, _c, _o, _T, _F       LHS type
        
#define BITWISE(op) [op] = {{_i, _i, xx, xx, xx, xx, mR, xx, xx, xx}, \
                            {_i, _i, xx, xx, xx, xx, mR, xx, xx, xx}, \
                            {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}, \
                            {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}, \
                            {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}, \
                            {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}, \
                            {mm, mm, mm, mm, mm, mm, mm, mm, mm, mm}, \
                            {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}, \
                            {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}, \
                            {xx, xx, xx, xx, xx, xx, mR, xx, xx, xx}} 
        BITWISE(BINARY_BXOR),                                                                 
        BITWISE(BINARY_BAND),                                                                 
        BITWISE(BINARY_BOR),                                                                 
        BITWISE(BINARY_SHL),                                                                 
        BITWISE(BINARY_SHR),                                                                 
    };
    // clang-format on

    // 'e->lhs' and 'e->rhs' already visited
    const TypeTag lhs = e->lhs->type;
    const TypeTag rhs = e->rhs->type;
    if (pawY_common(lhs, rhs, &e->common)) {
        pawX_error(V->lex, "incompatible types '%s' and '%s' for binary operator", 
                   pawY_name(t_type(e->lhs->type)), pawY_name(t_type(e->rhs->type)));
    }

    const int result = kValidOps[e->op][t_type(lhs)][t_type(rhs)];
    if (result == xx) {
        type_error(V);
    } else if (result == mm) {
        return try_meta_binop(V, e, lhs, rhs, PAW_FALSE);
    } else if (result == mR) {
        return try_meta_binop(V, e, lhs, rhs, PAW_TRUE);
    }
    return get_type(V, result);
}

#undef xx
#undef mm
#undef _b
#undef _i
#undef _f
#undef _s
#undef _a
#undef _m
#undef _c
#undef _o
#undef _T
#undef _F

static void visit_assignment(Visitor *V, Expr *lhs, Expr *rhs)
{
    if (lhs->kind == EXPR_VAR) {
        V->expr(V, rhs); // variable assignment
        VarExpr *e = cast_to(lhs, VarExpr);
        Symbol *var = resolve_var(V->lex, e->name);
        check_similar(V, var->type, rhs->type);
        e->type = var->type;
        return;
    }

    // index, range, or attribute assignment
    SuffixedExpr *base = cast_to(lhs, SuffixedExpr); // common base
    V->expr(V, base->target); // visit up to last expression
    V->expr(V, rhs);
    if (lhs->kind == EXPR_INDEX) {
        IndexExpr *last = cast_to(lhs, IndexExpr);
        V->expr(V, last->first);
        if (last->second) {
            V->expr(V, last->second);
            V->expr(V, rhs);
            check_same(V, last->type, rhs->type);
        } else {
            V->expr(V, rhs);
            TypeTag elem = pawY_unwrap(env(V->lex), lhs->type);
            check_similar(V, elem, rhs->type);
        }
    } else {
        paw_assert(lhs->kind == EXPR_ACCESS);
        const AccessExpr *e = cast_to(lhs, AccessExpr);
        V->expr(V, rhs);
        
        // TODO: Lookup field type
    }
}

// Make sure an array literal expression 'e' is compatible with the given 
// type 'tag'
static void check_array(Lex *lex, ArrayExpr *e, TypeTag tag)
{
    
}

static TypeTag resolve_type(Lex *lex, TypeName *tn);

static TypeTag resolve_array_type(Lex *lex, TypeName *tn)
{
    TypeTag elem = resolve_type(lex, tn->arr.elem);
    tn->tag = pawY_register_array(env(lex), elem);
    tn->resolved = PAW_TRUE;
    return tn->tag;
}

static TypeTag resolve_map_type(Lex *lex, TypeName *tn)
{
    TypeTag key = resolve_type(lex, tn->map.key);
    TypeTag value = resolve_type(lex, tn->map.value);
    tn->tag = pawY_register_map(env(lex), key, value);
    tn->resolved = PAW_TRUE;
    return tn->tag;
}

static TypeTag resolve_class_type(Lex *lex, TypeName *tn)
{
    Symbol *var = resolve_symbol(lex, tn->cls.name);
    if (var->kind != SYM_PROTOTYPE || !t_is_class(var->type)) {
        pawX_error(lex, "invalid class type '%s'", var->name->text);
    }
    tn->resolved = PAW_TRUE;
    tn->tag = var->type;
    return var->type;
}

static TypeTag resolve_type(Lex *lex, TypeName *tn)
{
    if (tn->resolved) {
        return tn->tag;
    } else if (t_is_primitive(tn->tag)) {
        tn->resolved = PAW_TRUE;
        return tn->tag;
    } else if (t_is_array(tn->tag)) {
        return resolve_array_type(lex, tn);
    } else if (t_is_map(tn->tag)) {
        return resolve_map_type(lex, tn);
    } else {
        return resolve_class_type(lex, tn);
    }
}

static void visit_var_expr(Visitor *V, VarExpr *e)
{
    Lex *lex = V->lex; // lookup type
    Symbol *var = resolve_var(lex, e->name);
    e->type = var->type;
}

static void visit_primitive_expr(Visitor *V, PrimitiveExpr *e)
{
    e->type = e->t < 0 ? NULL : get_type(V, e->t);
}

static void visit_logical_expr(Visitor *V, LogicalExpr *e)
{
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
    e->type = get_type(V, PAW_TBOOL);
}

static void visit_chain_expr(Visitor *V, ChainExpr *e)
{
    V->expr(V, e->target);
    if (!t_is_object(e->target->type)) {
        pawX_error(V->lex, "'?' operator requires an object");
    }
}

static void visit_cond_expr(Visitor *V, CondExpr *e)
{
    V->expr(V, e->cond);
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
    e->type = get_common(V, e->lhs->type, e->rhs->type);
}

static void visit_coalesce_expr(Visitor *V, CoalesceExpr *e)
{
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
    e->type = get_common(V, e->lhs->type, e->rhs->type);
    if (!t_is_object(e->lhs->type)) {
        pawX_error(V->lex, "'?:' operator requires an object");
    }
}

static void visit_unop_expr(Visitor *V, UnOpExpr *e)
{
    V->expr(V, e->target);
    e->type = check_unop(V, e);
}

static void visit_binop_expr(Visitor *V, BinOpExpr *e)
{
    V->expr(V, e->lhs);
    V->expr(V, e->rhs);
    e->type = check_binop(V, e);
}

static TypeTag *collect_param_types(Lex *lex, NodeVec param)
{
    const int nargs = param.size;
    TypeTag *args = NULL;
    if (param.size > 0) {
        args = pawM_new_vec(env(lex), nargs, TypeTag);
        for (int i = 0; i < nargs; ++i) {
            ParamStmt *s = cast_to(param.nodes[i], ParamStmt);
            args[i] = resolve_type(lex, s->tag);
        }
    }
    return args;
}

static TypeTag register_fn(Lex *lex, Function *fn)
{
    TypeTag *param = collect_param_types(lex, fn->param);
    TypeTag ret = fn->ret != NULL ? resolve_type(lex, fn->ret) : NULL;
    fn->type = pawY_register_function(env(lex), param, fn->param.size, ret);
    return fn->type;
}

static Type *visit_fn(Visitor *V, Function *fn)
{
    Lex *lex = V->lex;
    fn->lex = lex;

    Function *outer = V->fn;
    V->fn = fn;

    enter_function(lex, fn->name, fn->kind);
    V->stmt_vec(V, fn->param);
    V->block_stmt(V, fn->body);
    fn->scope = leave_function(lex);
    V->fn = outer;

    return register_fn(lex, fn);
}

static void visit_attr_stmt(Visitor *V, AttrStmt *s)
{
    if (s->is_fn) {
        visit_fn(V, &s->fn);
    }
}

static TypeTag register_attr(Visitor *V, AttrStmt *s)
{
    if (s->is_fn) {
        return register_fn(V->lex, &s->fn);
    }
    return resolve_type(V->lex, s->tag);
}

static void add_attr_placeholder(Lex *lex, Map *m, String *name)
{
    Value key, value;
    v_set_object(&key, name);
    v_set_null(&value);

    Value *pvalue = pawH_get(env(lex), m, key);
    if (pvalue == NULL) {
        pawH_insert(env(lex), m, key, value);
    } else {
        pawX_error(lex, "duplicate attribute '%s'", name->text);
    }
}

 // TODO: 'tags' variable needs a 'box'. 'release' the box right after
 //       calling pawY_register_class, which will take ownership of the
 //       allocation on success.
static void visit_class_stmt(Visitor *V, ClassStmt *s)
{
    Lex *lex = V->lex;
    paw_Env *P = env(lex);
    const int nattrs = s->attrs.size;
    Class *cls = pawV_new_class(P);
    enter_block(lex); // scope for 'super'

    TypeTag unknown = e_tag(P, PAW_TCLASS); // fix later
    Symbol *var = declare_var(lex, s->name, unknown, s->flags.global);
    V->expr(V, s->super); // before 's->name' defined

    // determine attribute types
    Attribute *attrs = NULL;
    pawM_resize(env(lex), attrs, 0, nattrs);
    for (int i = 0; i < nattrs; ++i) { 
        AttrStmt *a = cast_to(s->attrs.nodes[i], AttrStmt);
        Map *m = a->is_fn ? cls->methods : cls->fields;
        add_attr_placeholder(lex, m, a->name);
        attrs[i].attr = register_attr(V, a);
        attrs[i].name = a->name;
    }
    var->type = pawY_register_class(env(lex), s->name, attrs, nattrs);
    define_var(var); // allow access from class body

    // validate attributes
    ClsState cs = {.outer = lex->cs};
    lex->cs = &cs; // enter class context
    for (int i = 0; i < nattrs; ++i) {
        AttrStmt *a = cast_to(s->attrs.nodes[i], AttrStmt);
        visit_attr_stmt(V, a);
    }
    s->scope = leave_block(lex);
    lex->cs = cs.outer;
}

static void visit_block_stmt(Visitor *V, Block *bk)
{
    Lex *lex = V->lex;
    enter_block(lex);
    V->stmt_vec(V, bk->stmts);
    bk->scope = leave_block(lex);
}

static void visit_param_stmt(Visitor *V, ParamStmt *s)
{
    TypeTag type = resolve_type(V->lex, s->tag);
    paw_assert(type != NULL); // checked in parse.c
    new_var(V->lex, s->name, type, PAW_FALSE);
}

static void visit_def_stmt(Visitor *V, DefStmt *s)
{
    Lex *lex = V->lex;
    Symbol *var = declare_var(lex, s->name, NULL, s->flags.global);
    V->expr(V, s->init);
    define_var(var);

    TypeTag init = s->init->type;
    if (s->tag == NULL) {
        // infer from initializer
        if (init == NULL) {
            pawX_error(lex, "unable to infer variable type");
        }
        var->type = init; 
        return;                          
    }
   
    TypeTag type = resolve_type(V->lex, s->tag);
    var->type = type;
    if (init == NULL) {
        Expr *transform;
        if (t_is_array(type)) {
            ArrayExpr *e = pawK_add_node(lex, EXPR_ARRAY, ArrayExpr);
            transform = cast_expr(e); // set empty array
        } else if (t_is_map(type)) {
            MapExpr *e = pawK_add_node(lex, EXPR_MAP, MapExpr);
            transform = cast_expr(e); // set empty map
        } else {
            PrimitiveExpr *e = pawK_add_node(lex, EXPR_PRIMITIVE, PrimitiveExpr);
            pawV_set_default(env(lex), &e->v, t_type(type));
            transform = cast_expr(e); // set default value or null
        }
        transform->line = s->line;
        transform->type = type;
        s->init = transform;
    } else if (!pawY_is_similar(init, type)) {
        pawX_error(lex, "initializer incompatible with type annotation");
    }
}

static void visit_return_stmt(Visitor *V, ReturnStmt *s)
{
    Function *fn = V->fn;
    V->expr(V, s->expr);

    TypeName *ret = fn->ret; // type annotation
    if (ret == NULL) { // no return
        if (s->expr != NULL) {
            pawX_error(V->lex, "expected empty return");
        }
    } else if (s->expr == NULL) {
        pawX_error(V->lex, "expected nonempty return");
    } else if (!pawY_is_similar(ret->tag, s->expr->type)) {
        pawX_error(V->lex, "return type incompatible with annotation");
    }
}

static void visit_call_expr(Visitor *V, CallExpr *e)
{
    V->expr(V, e->target);
    V->expr_vec(V, e->param);

    // TODO: Resolve overloads
    TypeTag tag = e->target->type;
    if (t_base(tag) != PAW_TFUNCTION) {
        pawX_error(V->lex, "type is not callable");
    }
    const FnType type = tag->f;
    e->type = type.ret; // propagate return type
    if (type.nparam != e->param.size) {
        pawX_error(V->lex, "expected %d parameters but found %d", 
                   type.nparam, e->param.size);
    }
    for (int i = 0; i < type.nparam; ++i) {
        TypeTag tag = type.param[i];
        Expr *param = cast_expr(e->param.nodes[i]);
        if (!pawY_is_similar(param->type, tag)) {
            pawX_error(V->lex, "invalid parameter type");
        }
    }
}

static void visit_fn_expr(Visitor *V, FnExpr *e)
{
    e->fn.name = scan_string(V->lex, "(anonymous fn)");
    e->type = visit_fn(V, &e->fn);
}

static void visit_fn_stmt(Visitor *V, FnStmt *s)
{
    Type *t = visit_fn(V, &s->fn);
    new_var(V->lex, s->fn.name, t, s->flags.global);
}

static void visit_ifelse_stmt(Visitor *V, IfElseStmt *s)
{
    V->expr(V, s->cond);
    V->stmt(V, s->then_arm);
    V->stmt(V, s->else_arm);
}

static void visit_expr_stmt(Visitor *V, ExprStmt *s)
{
    if (s->rhs != NULL) {
        visit_assignment(V, s->lhs, s->rhs);
    } else {
        V->expr(V, s->lhs);
    }
}

static void visit_while_stmt(Visitor *V, WhileStmt *s)
{
    V->expr(V, s->cond);
    V->block_stmt(V, s->block);
}

static void visit_dowhile_stmt(Visitor *V, WhileStmt *s)
{
    V->block_stmt(V, s->block);
    V->expr(V, s->cond);
}

static void visit_forbody_stmt(Visitor *V, Block *bk)
{
    Lex *lex = V->lex;
    enter_block(lex);
    V->block_stmt(V, bk);
    bk->scope = leave_block(lex);
}

static void visit_fornum_stmt(Visitor *V, ForStmt *s)
{
    Lex *lex = V->lex;
    ForNum *fornum = &s->fornum;
    new_local_literal(lex, "(for begin)", PAW_TINT);
    new_local_literal(lex, "(for end)", PAW_TINT);
    new_local_literal(lex, "(for step)", PAW_TINT);
    new_local_var(lex, s->name, get_type(V, PAW_TINT));

    V->expr(V, fornum->begin);
    V->expr(V, fornum->end);
    V->expr(V, fornum->step);

    check_integral(V, fornum->begin->type);
    check_integral(V, fornum->end->type);
    check_integral(V, fornum->step->type);

    visit_forbody_stmt(V, s->block);
}

static void visit_forin_stmt(Visitor *V, ForStmt *s) // TODO: forin would need to encode the type of object being iterated over. look into function call for loop? 
{
    Lex *lex = V->lex;
    ForIn *forin = &s->forin;
    new_local_literal(lex, "(for target)", PAW_TINT);
    new_local_literal(lex, "(for iterator)", PAW_TINT);
    V->expr(V, forin->target);

    TypeTag inner = pawY_unwrap(env(lex), forin->target->type);
    new_local_var(lex, s->name, inner);

    visit_forbody_stmt(V, s->block);
}

static void visit_for_stmt(Visitor *V, ForStmt *s)
{
    Lex *lex = V->lex;
    enter_block(lex);
    if (s->kind == STMT_FORNUM) {
        visit_fornum_stmt(V, s);
    } else {
        visit_forin_stmt(V, s);
    }
    s->scope = leave_block(lex);
}

static void visit_array_expr(Visitor *V, ArrayExpr *e)
{
    NodeVec elems = e->items;
    paw_Env *P = env(V->lex);
    TypeTag t = e_tag(P, PAW_NULL);
    for (int i = 0; i < elems.size; ++i) {
        Expr *elem = cast_expr(elems.nodes[i]);
        V->expr(V, elem);
        if (t == NULL) {
            t = elem->type;
        } else {
            t = get_common(V, t, elem->type);
        }
    }
    int min_level = -1;
    for (int i = 0; i < elems.size; ++i) {
        Expr *elem = cast_expr(elems.nodes[i]);
        TypeTag tag = elem->type;
        int level;
        if (t_is_array(tag)) {
            level = tag->a.level; 
        } else {
            level = 0;
        }
        if (0 <= min_level && min_level < level) {
            pawX_error(V->lex, "inconsistent array type");
        }
        min_level = level;
    }
    // Register the array type. '[null]' means the array was empty, and the actual
    // type could not be determined (need to keep track of nesting depth).
    e->type = pawY_register_array(P, t);
}

static void visit_map_expr(Visitor *V, MapExpr *e)
{
    NodeVec items = e->items;
    TypeTag tk = NULL;
    TypeTag tv = NULL;
    for (int i = 0; i < items.size; i += 2) {
        Expr *key = cast_expr(items.nodes[i]);
        Expr *value = cast_expr(items.nodes[i + 1]);
        V->expr(V, key);
        V->expr(V, value);
        if (tk == NULL) {
            tk = key->type;
            tv = value->type;
            check_primitive(V, tk);
        } else {
            tk = get_common(V, tk, key->type);
            tv = get_common(V, tv, value->type);
        }
    }
    paw_Env *P = env(V->lex);
    e->type = pawY_register_map(P, tk, tv);
}

static void check_index(Visitor *V, IndexExpr *e)
{
    if (t_is_class(e->target->type)) {
//        if (e->second == NULL) {
//            TypeTag tag = resolve_mm(env(V->lex), e->target->type, MM_GETITEM);
//        } else {
//            resolve_mm(env(V->lex), e->target->type, MM_GETSLICE);
//        }
        paw_assert(0); // TODO: try to lookup metamethod return type, fail if no metamethod for __getitem
        return;
    }
    e->type = pawY_unwrap(env(V->lex), e->target->type);
    // use 'int' as the type of a literal 'null'
    fix_null(V, e->first, PAW_TINT); 
    if (t_is_map(e->target->type)) {
        check_primitive(V, e->first->type);
    } else {
        check_sequence(V, e->target->type);
        check_integral(V, e->first->type);
    }
}

static void check_range(Visitor *V, IndexExpr *e)
{
    if (t_is_class(e->target->type)) {
        paw_assert(0); // TODO: try to lookup metamethod return type, fail if no metamethod for __getslice
        return;
    }
    fix_null(V, e->first, PAW_TINT);
    fix_null(V, e->second, PAW_TINT);
    check_same(V, e->first->type, e->second->type);
    check_sequence(V, e->target->type);
    check_integral(V, e->first->type);
    check_integral(V, e->second->type);
    e->type = e->target->type;
}

static void visit_index_expr(Visitor *V, IndexExpr *e)
{
    V->expr(V, e->target);
    V->expr(V, e->first);
    if (e->second != NULL) {
        V->expr(V, e->second);
        check_range(V, e);
    } else {
        check_index(V, e);
    }
}

static void visit_access_expr(Visitor *V, AccessExpr *e)
{
    V->expr(V, e->target);
    if (!t_is_class(e->target->type)) {
        expected_type(V, e->target->type, get_type(V, PAW_TCLASS));
    }
}

void p_check_types(Lex *lex)
{
    paw_Env *P = env(lex);

    Visitor V;
    pawK_init_visitor(&V, lex);
    V.primitive_expr = visit_primitive_expr;
    V.logical_expr = visit_logical_expr;
    V.chain_expr = visit_chain_expr;
    V.cond_expr = visit_cond_expr;
    V.coalesce_expr = visit_coalesce_expr;
    V.unop_expr = visit_unop_expr;
    V.binop_expr = visit_binop_expr;
    V.var_expr = visit_var_expr;
    V.array_expr = visit_array_expr;
    V.map_expr = visit_map_expr;
    V.access_expr = visit_access_expr;
    V.index_expr = visit_index_expr;
    V.fn_expr = visit_fn_expr;
    V.return_stmt = visit_return_stmt;
    V.call_expr = visit_call_expr;
    V.param_stmt = visit_param_stmt;
    V.class_stmt = visit_class_stmt;
    V.block_stmt = visit_block_stmt;
    V.def_stmt = visit_def_stmt;
    V.fn_stmt = visit_fn_stmt;
    V.for_stmt = visit_for_stmt;
    V.while_stmt = visit_while_stmt;
    V.dowhile_stmt = visit_dowhile_stmt;
    V.ifelse_stmt = visit_ifelse_stmt;
    V.expr_stmt = visit_expr_stmt;

    lex->globals = pawM_new(P, SymbolTable);
    for (int i = 0; i < P->gv.size; ++i) {
        GlobalVar g = P->gv.data[i];
        Symbol *s = add_global(lex, g.desc.name, g.desc.type);
        define_var(s);
    }

    enter_function(lex, lex->modname, FN_MODULE);
    pawK_visit(&V, lex->ast);
    lex->toplevel = leave_function(lex);

    paw_assert(lex->pm->scopes.size == 0);
}

