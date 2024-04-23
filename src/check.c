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

static Type *cannonicalize(Lex *lex, const Type *type)
{
    return pawY_add_type(env(lex), env(lex)->mod, type);
}

static void push_symbol_table(Lex *lex)
{
    pawP_add_scope(lex, &lex->pm->st);
}

static void pop_symbol_table(Lex *lex)
{
    // Last symbol table should have been assigned to an AST node. The
    // next call to push_symbol_table() will allocate a new table.
    SymbolTable *st = &lex->pm->st;
    paw_assert(st->nscopes > 0);
    --st->nscopes;
}

static Scope *get_symbols(Lex *lex)
{
    SymbolTable *st = &lex->pm->st;
    return st->scopes[st->nscopes - 1];
}

static Symbol *register_var(Lex *lex, Scope *st, String *name, Type *type)
{
    Symbol *sym = pawP_add_symbol(lex, st);
    sym->name = name;
    sym->type = type;
    return sym;
}

static Symbol *add_local(Lex *lex, String *name, Type *type)
{
    Scope *st = get_symbols(lex);
    return register_var(lex, st, name, type);
}

static Symbol *add_global(Lex *lex, String *name, Type *type)
{
    Scope *st = lex->pm->st.globals;
    for (int i = 0; i < st->nsymbols; ++i) {
        Symbol *sym = st->symbols[i];
        if (pawS_eq(sym->name, name)) {
            pawX_error(lex, "duplicate global '%s'", name->text); 
        }
    }
    return register_var(lex, st, name, type);
}

static Symbol *resolve_symbol(Lex *lex, String *name)
{
    // search the scoped symbols
    SymbolTable *scopes = &lex->pm->st;
    for (int depth = scopes->nscopes - 1; depth >= 0; --depth) {
        Scope *scope = scopes->scopes[depth];
        const int index = pawP_find_symbol(scope, name);
        if (index >= 0) {
            return scope->symbols[index];
        }
    }
    // search the global symbols
    const int index = pawP_find_symbol(scopes->globals, name);
    if (index < 0) {
        pawX_error(lex, "undefined symbol '%s'", name->text);
    }
    return scopes->globals->symbols[index];
}

static Symbol *resolve_var(Lex *lex, String *name)
{
    Symbol *var = resolve_symbol(lex, name);
    if (var->is_type) {
        pawX_error(lex, "identifier '%s' is not a variable", var->name->text);
    }
    return var;
}

// Register the name and type of a variable
// If 'global' is true, then the variable is a global, otherwise, it is a local.
// Must be called prior to 'define_var',
static Symbol *declare_var(Lex *lex, String *name, Type *tag, paw_Bool global)
{
    return global ? add_global(lex, name, tag)
                  : add_local(lex, name, tag);
}

// Allow a previously-declared variable to be accessed
static void define_var(Symbol *symbol)
{
    symbol->is_init = PAW_TRUE;
}

static void new_var(Lex *lex, String *name, Type *tag, paw_Bool global)
{
    Symbol *symbol = declare_var(lex, name, tag, global);
    define_var(symbol);
}

static void new_local_var(Lex *lex, String *name, Type *tag)
{
    new_var(lex, name, tag, PAW_FALSE);
}

static Scope *leave_block(Lex *lex)
{
    Scope *st = get_symbols(lex);
    pop_symbol_table(lex);
    return st;
}

static void enter_block(Lex *lex)
{
    push_symbol_table(lex);
}

static Scope *leave_function(Lex *lex)
{
    Scope *scope = get_symbols(lex);
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

static void enter_function(Lex *lex, String *name, Type *sig, FnKind kind)
{
    ++lex->fn_depth;

    // Enter the function body.
    enter_block(lex);

    // Create the context variable in slot 0. For VCLOSURE, this slot holds the closure
    // object being called. For VMETHOD, it holds the class instance that the method is
    // being called on, i.e. the implicit 'self' parameter.
    new_local_var(lex, context_name(lex, name, kind), sig);
}

static void new_local_literal(Lex *lex, const char *name, int type)
{
    new_local_var(lex, scan_string(lex, name), x_base_type(lex, type));
}

static void type_error(Visitor *V)
{
    pawX_error(V->lex, "invalid type");
}

static Type *get_type(Visitor *V, int type)
{
    if (type < 0) {
        type_error(V);
    }
    return x_base_type(V->lex, type);
}

static void expected_type(Visitor *V, Type *have, Type *want)
{
    pawX_error(V->lex, "expected '%s' type but found '%s'", 
               pawY_name(y_id(have)), pawY_name(y_id(want)));
}

static void check_same(Visitor *V, Type *lhs, Type *rhs)
{
    if (!pawY_is_same(lhs, rhs)) {
        pawX_error(V->lex, "expected equal types but found '%s' and '%s'", 
                   pawY_name(y_id(lhs)), pawY_name(y_id(rhs))); 
    }
}

static void check_similar(Visitor *V, Type *lhs, Type *rhs)
{
    if (!pawY_is_same(lhs, rhs)) {
        pawX_error(V->lex, "expected compatible types but found '%s' and '%s'", 
                   pawY_name(y_id(lhs)), pawY_name(y_id(rhs))); 
    }
}

static Type *get_common(Visitor *V, Type *a, Type *b)
{
    if (!pawY_is_same(a, b)) {
        pawX_error(V->lex, "incompatible types '%s' and '%s'", 
                   pawY_name(y_id(a)), pawY_name(y_id(b)));
    }
    return a;
}

static void check_primitive(Visitor *V, Type *t)
{
    if (!y_is_primitive(t)) {
        pawX_error(V->lex, "expected primitive ('bool', 'int', 'float', or 'string') but found '%s'", 
                   pawY_name(y_id(t))); 
    }
}

static void check_integral(Visitor *V, Type *t)
{
    if (!y_is_int(t) && !y_is_bool(t)) {
        pawX_error(V->lex, "expected integral ('int' or 'bool') but found '%s'", 
                   pawY_name(y_id(t))); 
    }
}

static void check_string(Visitor *V, Type *t)
{
    if (!y_is_string(t)) {
        pawX_error(V->lex, "expected string but found '%s'", 
                   pawY_name(y_id(t))); 
    }
}

static void check_sequence(Visitor *V, Type *t)
{
    if (!y_is_string(t) && !y_is_array(t) && !y_is_tuple(t)) {
        pawX_error(V->lex, "expected sequence ('string', 'array', or 'tuple') but found '%s'", 
                   pawY_name(y_id(t))); 
    }
}

static void check_accessible(Visitor *V, Type *t)
{
    if (!y_is_class(t) && !y_is_foreign(t)) {
        pawX_error(V->lex, "expected class or foreign object  but found '%s'", 
                   pawY_name(y_id(t))); 
    }
}

static void check_indexable(Visitor *V, Type *t)
{
    if (!y_is_string(t) && !y_is_array(t) && !y_is_tuple(t)) {
        pawX_error(V->lex, "expected container ('string' or 'array', or 'map') but found '%s'", 
                   pawY_name(y_id(t))); 
    }
}

static String *meta_key(paw_Env *P, Metamethod mm)
{
    return v_string(P->meta_keys[mm]);
}

static FunctionType *resolve_mm(paw_Env *P, const Type *tag, Metamethod mm)
{
    paw_assert(y_is_class(tag));
    String *name = meta_key(P, mm);
    for (int i = 0; i < tag->cls.nattrs; ++i) {
        NamedField *a = &tag->cls.attrs[i];
        if (pawS_eq(a->name, name) && y_is_function(a->type)) {
            return &a->type->sig;
        }
    }
    return NULL;
}

static Type *try_meta_unop(Visitor *V, UnOpExpr *e, const Type *tag)
{
    paw_Env *P = env(V->lex);
    const Metamethod mm = unop2meta(e->op);
    FunctionType *sig = resolve_mm(P, tag, mm);
    if (sig->nargs != 0) {
        type_error(V);
    }
    e->mm = sig;
    return sig->ret; 
}

static Type *try_meta_binop(Visitor *V, BinOpExpr *e, const Type *lhs, const Type *rhs, paw_Bool is_r)
{
    paw_Env *P = env(V->lex);
    Metamethod mm = binop2meta(e->op);
    mm = is_r ? mm_get_r(mm) : mm;
    const Type *self = is_r ? rhs : lhs;
    const Type *other = is_r ? lhs : rhs;
    FunctionType *sig = resolve_mm(P, self, mm);
    if (sig->nargs != 1 || !pawY_is_same(sig->args[0], other)) {
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

static Type *check_unop(Visitor *V, UnOpExpr *e)
{
    // clang-format off
    static const int8_t kValidOps[NUNARYOPS][PAW_NTYPES] = {
        //     type  =  0, b, i, f, s, t, a, ...
        [UNARY_LEN]  = {0, 0, 0, 0, 1, 1, 1},
        [UNARY_NEG]  = {0, 0, 1, 1, 0, 0, 0}, 
        [UNARY_NOT]  = {0, 1, 1, 1, 0, 0, 0}, 
        [UNARY_BNOT] = {0, 0, 1, 0, 0, 0, 0}, 
    };
    // clang-format on

    Type *type = e->target->type;
    if (y_is_class(type)) {
        return try_meta_unop(V, e, type);
    } else if (!kValidOps[e->op][y_id(type)]) {
        pawX_error(V->lex, "unsupported operand type for unary '%s': '%s'",
                   "? TODO", pawY_name(y_id(type)));
    } else if (unop_is_bool(e->op)) {
        return get_type(V, PAW_TBOOL);
    }
    return type;
}

// TODO: BINARY_IN should be handled separately
static Type *check_binop(Visitor *V, BinOpExpr *e)
{
    // clang-format off
    static const uint8_t kValidOps[NBINARYOPS][PAW_NTYPES] = {
        //     type   =  0, b, i, f, s, ...
        [BINARY_EQ]   = {0, 1, 1, 1, 1},
        [BINARY_NE]   = {0, 1, 1, 1, 1},
        [BINARY_LT]   = {0, 1, 1, 1, 1},
        [BINARY_LE]   = {0, 1, 1, 1, 1},
        [BINARY_GT]   = {0, 1, 1, 1, 1},
        [BINARY_GE]   = {0, 1, 1, 1, 1},
        [BINARY_ADD]  = {0, 1, 1, 1, 1},
        [BINARY_SUB]  = {0, 1, 1, 1, 0},
        [BINARY_MUL]  = {0, 1, 1, 1, 0},
        [BINARY_DIV]  = {0, 1, 1, 1, 0},
        [BINARY_MOD]  = {0, 1, 1, 1, 0},
        [BINARY_BXOR] = {0, 0, 1, 0, 0},
        [BINARY_BAND] = {0, 0, 1, 0, 0},
        [BINARY_BOR]  = {0, 0, 1, 0, 0},
        [BINARY_SHL]  = {0, 0, 1, 0, 0},
        [BINARY_SHR]  = {0, 0, 1, 0, 0},
    };
    // clang-format on

    Type *lhs = e->lhs->type;
    Type *rhs = e->rhs->type;
    if (y_is_class(lhs)) {
        return try_meta_binop(V, e, lhs, rhs, PAW_FALSE);
    } else if (y_is_class(rhs)) {
        return try_meta_binop(V, e, lhs, rhs, PAW_TRUE);
    } else if (!pawY_is_same(lhs, rhs) || 
               !kValidOps[e->op][y_id(lhs)]) {
        pawX_error(V->lex, "unsupported operand types for binary '%s': '%s' and '%s'",
                   "? TODO", pawY_name(y_id(lhs)), pawY_name(y_id(rhs)));
    } else if (binop_is_bool(e->op)) {
        return get_type(V, PAW_TBOOL);
    } else {
        return lhs;
    }
    return NULL;
}

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
           // V->expr(V, rhs);
           // Symbol *elem = pawY_unwrap(env(V->lex), lhs->type);
           // check_similar(V, elem, rhs->type);
        }
    } else {
        paw_assert(lhs->kind == EXPR_ACCESS);
        const AccessExpr *e = cast_to(lhs, AccessExpr);
        V->expr(V, rhs);
        
        // TODO: Lookup field type
    }
}

static Type *resolve_type(Lex *lex, TypeDecl *tn);

static Type *resolve_fn_type(Lex *lex, TypeDecl *tn)
{
    const int nargs = tn->sig.nargs;
    Type **args = NULL;
    if (nargs > 0) {
        Expr *arg = tn->sig.args;
        args = pawM_new_vec(env(lex), nargs, Type *);
        for (int i = 0; i < nargs; ++i) {
            TypeDecl *decl = cast_to(arg, TypeDecl);
            args[i] = resolve_type(lex, decl);
            arg = arg->next;
        }
    }
    Type fake = {0};
    fake.sig.args = args;
    fake.sig.nargs = nargs;
    fake.sig.ret = resolve_type(lex, tn->sig.ret);
    fake.sig.kind = TYPE_SIGNATURE;
    fake.sig.id = PAW_TFUNCTION; // temporary
    tn->type = cannonicalize(lex, &fake);
    return tn->type;
}

//static Type *resolve_array_type(Lex *lex, TypeDecl *tn)
//{
//    Type *elem = resolve_type(lex, tn->arr.elem);
//    tn->tag = pawY_register_array(env(lex), elem);
//    tn->resolved = PAW_TRUE;
//    return tn->tag;
//}
//
//static Type *resolve_map_type(Lex *lex, TypeDecl *tn)
//{
//    Type *key = resolve_type(lex, tn->map.key);
//    Type *value = resolve_type(lex, tn->map.value);
//    tn->tag = pawY_register_map(env(lex), key, value);
//    tn->resolved = PAW_TRUE;
//    return tn->tag;
//}

static Type *resolve_class_type(Lex *lex, TypeDecl *tn)
{
    Symbol *var = resolve_symbol(lex, tn->named.name);
    if (!var->is_type || !y_is_class(var->type)) {
        pawX_error(lex, "invalid class type '%s'", var->name->text);
    }
    return var->type;
}

static Type *resolve_type(Lex *lex, TypeDecl *tn)
{
    Type *type;
    if (tn->group == TYPE_PRIMITIVE) {
        type = lex->P->mod->types[tn->basic.t];
    } else if (tn->group == TYPE_CLASS) {
        type = resolve_class_type(lex, tn);
    } else {
        type = resolve_fn_type(lex, tn);
    }
    tn->type = type;
    return type;
}

static void visit_var_expr(Visitor *V, VarExpr *e)
{
    Lex *lex = V->lex; // lookup type
    Symbol *var = resolve_var(lex, e->name);
    e->type = var->type;
}

static void visit_primitive_expr(Visitor *V, PrimitiveExpr *e)
{
    e->type = get_type(V, e->t);
}

static void visit_literal_expr(Visitor *V, LiteralExpr *e)
{
    V->expr(V, e->expr);
    e->type = e->expr->type;
    new_local_literal(V->lex, e->label, e->t);
    e->type = get_type(V, e->t);
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
//    if (!y_is_object(e->target->type)) {
//        pawX_error(V->lex, "'?' operator requires an object");
//    }
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
//    if (!y_is_object(e->lhs->type)) {
//        pawX_error(V->lex, "'?:' operator requires an object");
//    }
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

static Type **collect_param_types(Lex *lex, Stmt *head, int nargs)
{
    Type **args = NULL;
    if (nargs) {
        args = pawM_new_vec(env(lex), nargs, Type *);
        for (int i = 0; i < nargs; ++i, head = head->next) {
            ParamStmt *s = cast_to(head, ParamStmt);
            args[i] = resolve_type(lex, s->tag);
        }
    }
    return args;
}

static Type *register_fn(Lex *lex, Function *fn)
{
    Type fake = {0};
    fake.sig.kind = TYPE_SIGNATURE;
    fake.sig.id = PAW_TFUNCTION; // temporary
    fake.sig.args = collect_param_types(lex, fn->args, fn->nargs);
    fake.sig.nargs = fn->nargs;
    fake.sig.ret = fn->ret ? resolve_type(lex, fn->ret) : NULL;
    fn->type = cannonicalize(lex, &fake);
    return fn->type;
}

static void visit_fn(Visitor *V, Function *fn, Type *sig)
{
    Lex *lex = V->lex;
    Function *outer = V->fn;
    V->fn = fn;

    enter_function(lex, fn->name, sig, fn->kind);
    V->stmt_list(V, fn->args);
    V->block_stmt(V, fn->body);
    fn->scope = leave_function(lex);
    V->fn = outer;
}

static void visit_attr_stmt(Visitor *V, AttrStmt *s)
{
    if (s->is_fn) {
        Type *sig = register_fn(V->lex, &s->fn);
        visit_fn(V, &s->fn, sig);
    }
}

static Type *register_attr(Visitor *V, AttrStmt *s)
{
    if (s->is_fn) {
        return register_fn(V->lex, &s->fn);
    }
    return resolve_type(V->lex, s->tag);
}

 // TODO: 'fake' variable's attrs list needs a 'box'. 'release' the box right after
 //       calling pawY_add_type, which will take ownership of the
 //       allocation on success.
static void visit_class_stmt(Visitor *V, ClassStmt *s)
{
    Lex *lex = V->lex;
    paw_Env *P = env(lex);
    Type fake = {0};

    Stmt *attr = s->attrs;
    NamedField *attrs = NULL;
    if (s->nattrs > 0) {
        // determine attribute types
        attrs = pawM_new_vec(P, s->nattrs, NamedField);
        for (int i = 0; i < s->nattrs; ++i) { 
            AttrStmt *a = cast_to(attr, AttrStmt);
            attrs[i].type = register_attr(V, a);
            attrs[i].name = a->name;
            if (a->is_fn) {
                attrs[i].flags = FIELD_IS_METHOD;
            }
            attr = attr->next;
        }
    }
    fake.cls.id = PAW_TCLASS; // temporary
    fake.cls.kind = TYPE_CLASS;
    fake.cls.super = s->super ? s->super->type : NULL;
    fake.cls.nattrs = s->nattrs;
    fake.cls.attrs = attrs;
    fake.cls.name = s->name;
    Type *type = cannonicalize(lex, &fake);

    Symbol *var = declare_var(lex, s->name, type, s->flags.global);
    var->is_type = PAW_TRUE;

    enter_block(lex); // scope for 'super'
    V->expr(V, s->super); // before 's->name' defined
    define_var(var); // allow access from class body

    // validate attributes
    attr = s->attrs;
    ClsState cs = {.outer = lex->cs};
    lex->cs = &cs; // enter class context
    for (int i = 0; i < s->nattrs; ++i) {
        AttrStmt *a = cast_to(attr, AttrStmt);
        visit_attr_stmt(V, a);
        attr = attr->next;
    }
    s->scope = leave_block(lex);
    lex->cs = cs.outer;
}

static void visit_block_stmt(Visitor *V, Block *bk)
{
    Lex *lex = V->lex;
    enter_block(lex);
    V->stmt_list(V, bk->stmts);
    bk->scope = leave_block(lex);
}

static void visit_param_stmt(Visitor *V, ParamStmt *s)
{
    Type *type = resolve_type(V->lex, s->tag);
    paw_assert(type != NULL); // checked in parse.c
    new_var(V->lex, s->name, type, PAW_FALSE);
}

static void visit_def_stmt(Visitor *V, DefStmt *s)
{
    Lex *lex = V->lex;
    Symbol *var = declare_var(lex, s->name, NULL, s->flags.global);
    V->expr(V, s->init);
    define_var(var);

    if (s->tag == NULL) {
        // infer from initializer
        if (s->init == NULL) {
            pawX_error(lex, "missing initializer");
        } else if (s->init->type == NULL) {
            pawX_error(lex, "unable to infer type from 'null'");
        }
        var->type = s->init->type; 
        return;                          
    }
   
    Type *type = resolve_type(V->lex, s->tag);
    var->type = type;
    if (s->init == NULL) {
        // empty initializer: set default during codegen
    } else if (!pawY_is_same(s->init->type, type)) {
        pawX_error(lex, "initializer incompatible with type annotation");
    }
}

static void visit_return_stmt(Visitor *V, ReturnStmt *s)
{
    Function *fn = V->fn;
    V->expr(V, s->expr);

    if (fn->ret == NULL) {
        if (s->expr != NULL) {
            pawX_error(V->lex, "expected empty return");
        }
        return;
    } else if (s->expr == NULL) {
        pawX_error(V->lex, "expected nonempty return");
    } 
    
    Type *ret = resolve_type(V->lex, fn->ret);
    if (!pawY_is_same(ret, s->expr->type)) {
        pawX_error(V->lex, "return type incompatible with annotation");
    }
}

static void visit_call_expr(Visitor *V, CallExpr *e)
{
    V->expr(V, e->target);
    V->expr_list(V, e->args);

    Type *tag = e->target->type;
    if (y_kind(tag) != TYPE_SIGNATURE) {
        pawX_error(V->lex, "type is not callable");
    }
    const FunctionType *sig = &tag->sig;
    e->type = sig->ret; // propagate return type
    if (sig->nargs != e->nargs) {
        pawX_error(V->lex, "expected %d parameters but found %d", 
                   sig->nargs, e->nargs);
    }
    Expr *arg = e->args;
    for (int i = 0; i < sig->nargs; ++i) {
        if (!pawY_is_same(arg->type, sig->args[i])) {
            pawX_error(V->lex, "invalid parameter type");
        }
        arg = arg->next;
    }
}

static void visit_fn_stmt(Visitor *V, FnStmt *s)
{
    Type *sig = register_fn(V->lex, &s->fn);
    Symbol *var = declare_var(V->lex, s->fn.name, sig, s->flags.global);
    visit_fn(V, &s->fn, sig);
    define_var(var);
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

static void visit_forbody_stmt(Visitor *V, String *iname, Block *bk)
{
    Lex *lex = V->lex;
    enter_block(lex);
    new_local_var(lex, iname, get_type(V, PAW_TINT));
    V->block_stmt(V, bk);
    bk->scope = leave_block(lex);
}

static void visit_fornum_stmt(Visitor *V, ForStmt *s)
{
    ForNum *fornum = &s->fornum;
    V->expr(V, fornum->begin);
    V->expr(V, fornum->end);
    V->expr(V, fornum->step);

    check_integral(V, fornum->begin->type);
    check_integral(V, fornum->end->type);
    check_integral(V, fornum->step->type);

    visit_forbody_stmt(V, s->name, s->block);
}

static void visit_forin_stmt(Visitor *V, ForStmt *s) // TODO: forin would need to encode the type of object being iterated over. look into function call for loop? 
{
//    Lex *lex = V->lex;
//    ForIn *forin = &s->forin;
//    new_local_literal(lex, "(for target)", PAW_TINT);
//    new_local_literal(lex, "(for iterator)", PAW_TINT);
//    V->expr(V, forin->target);
//
//    Type *inner = pawY_unwrap(env(lex), forin->target->type);
//    new_local_var(lex, s->name, inner);
//
//    visit_forbody_stmt(V, s->name, s->block);
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
//    NodeVec elems = e->items;
//    paw_Env *P = env(V->lex);
//    Type *t = get_type(V, );
//    for (int i = 0; i < elems.size; ++i) {
//        Expr *elem = cast_expr(elems.nodes[i]);
//        V->expr(V, elem);
//        if (t == NULL) {
//            t = elem->type;
//        } else {
//            t = get_common(V, t, elem->type);
//        }
//    }
//    int min_level = -1;
//    for (int i = 0; i < elems.size; ++i) {
//        Expr *elem = cast_expr(elems.nodes[i]);
//        Type *tag = elem->type;
//        int level;
//        if (y_is_array(tag)) {
//            level = tag->a.level; 
//        } else {
//            level = 0;
//        }
//        if (0 <= min_level && min_level < level) {
//            pawX_error(V->lex, "inconsistent array type");
//        }
//        min_level = level;
//    }
//    // Register the array type. '[null]' means the array was empty, and the actual
//    // type could not be determined (need to keep track of nesting depth).
//    e->type = pawY_register_array(P, t);
}

//static void visit_map_expr(Visitor *V, MapExpr *e)
//{
//    NodeVec items = e->items;
//    Type *tk = NULL;
//    Type *tv = NULL;
//    for (int i = 0; i < items.size; i += 2) {
//        Expr *key = cast_expr(items.nodes[i]);
//        Expr *value = cast_expr(items.nodes[i + 1]);
//        V->expr(V, key);
//        V->expr(V, value);
//        if (tk == NULL) {
//            tk = key->type;
//            tv = value->type;
//            check_primitive(V, tk);
//        } else {
//            tk = get_common(V, tk, key->type);
//            tv = get_common(V, tv, value->type);
//        }
//    }
//    paw_Env *P = env(V->lex);
//    e->type = pawY_register_map(P, tk, tv);
//}

static void check_index(Visitor *V, IndexExpr *e)
{
    if (y_is_class(e->target->type)) {
//        if (e->second == NULL) {
//            Type *tag = resolve_mm(env(V->lex), e->target->type, MM_GETITEM);
//        } else {
//            resolve_mm(env(V->lex), e->target->type, MM_GETSLICE);
//        }
        paw_assert(0); // TODO: try to lookup metamethod return type, fail if no metamethod for __getitem
        return;
    }
//    e->type = pawY_unwrap(env(V->lex), e->target->type);
//    if (y_is_map(e->target->type)) {
//        check_primitive(V, e->first->type);
//    } else {
//        check_sequence(V, e->target->type);
//        check_integral(V, e->first->type);
//    }
}

static void check_range(Visitor *V, IndexExpr *e)
{
    if (y_is_class(e->target->type)) {
        paw_assert(0); // TODO: try to lookup metamethod return type, fail if no metamethod for __getslice
        return;
    }
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

// TODO: scratch allocations need to be boxed
static void visit_init_expr(Visitor *V, InitExpr *e)
{
    Lex *lex = V->lex;
    paw_assert(e->prefix->kind == EXPR_VAR);
    VarExpr *ve = cast_to(e->prefix, VarExpr);
    Symbol *sym = resolve_symbol(lex, ve->name);
    Type *type = sym->type;
    if (!y_is_class(type)) {
        pawX_error(V->lex, "'%s' is not a class", pawY_name(y_id(type)));
    }
    paw_Env *P = env(lex);
    Map *map = pawH_new(env(lex));

    Value key;
    Stmt *attr = e->attrs;
    Stmt **before = pawM_new_vec(env(lex), e->nattrs, Stmt *);
    for (int i = 0; attr != NULL; ++i) {
        ItemStmt *item = cast_to(attr, ItemStmt);
        v_set_object(&key, item->name);
        if (pawH_contains(P, map, key)) {
            pawX_error(lex, "duplicate attribute '%s.%s'",
                       item->name->text, ve->name->text, item->name->text);
        }
        Value *value = pawH_action(P, map, key, MAP_ACTION_CREATE);
        v_set_int(value, i);
        before[i] = attr;

        V->expr(V, item->value);
        attr = attr->next;
    }
    Stmt **after = pawM_new_vec(env(lex), e->nattrs, Stmt *);
    for (int i = 0; i < type->cls.nattrs; ++i) {
        NamedField *a = &type->cls.attrs[i];
        v_set_object(&key, a->name);
        Value *value = pawH_get(P, map, key);
        if (a->flags & FIELD_IS_METHOD) {
            if (value != NULL) {
                pawX_error(lex, "initializer not allowed for method '%s.%s'",
                           ve->name->text, a->name->text);
            }
        } else if (value == NULL) {
            pawX_error(lex, "missing initializer for attribute '%s.%s'",
                       ve->name->text, a->name->text);
        } else {
            const paw_Int index = v_int(*value);
            after[i] = before[index];
        }
    }
    paw_assert(type->cls.nattrs >= e->nattrs);

    // Put attributes in the correct order.
    e->attrs = after[0];
    for (int i = 0; i < e->nattrs; ++i) {
        after[i]->next = i < e->nattrs - 1 
            ? after[i + 1] 
            : NULL;
    }
    pawH_free(P, map);
    pawM_free_vec(P, before, e->nattrs);
    pawM_free_vec(P, after, e->nattrs);
    e->prefix->type = e->type = type;
}

static void find_attr(Lex *lex, ClassType *cls, String *name, int *pindex)
{
    for (int i = 0; i < cls->nattrs; ++i) {
        if (pawS_eq(cls->attrs[i].name, name)) {
            *pindex = i; 
            return;
        }
    }
    pawX_error(lex, "attribute '%s' does not exist", name->text);
}

static void visit_access_expr(Visitor *V, AccessExpr *e)
{
    Lex *lex = V->lex;
    V->expr(V, e->target);
    check_accessible(V, e->target->type);
    
    int index;
    ClassType *cls = &e->target->type->cls;
    find_attr(lex, cls, e->name, &index);

    e->type = cls->attrs[index].type;
}

static void visit_invoke_expr(Visitor *V, InvokeExpr *e)
{
    Lex *lex = V->lex;
    V->expr(V, e->target);
    check_accessible(V, e->target->type);

    ClassType *cls = &e->target->type->cls;
    find_attr(lex, cls, e->name, &e->index);

    FunctionType *sig = &cls->attrs[e->index].type->sig;
    if (sig->nargs != e->nargs) {
        pawX_error(lex, "expected %d arguments but found %d", 
                   sig->nargs, e->nargs);
    }

    Expr *arg = e->args;
    for (int i = 0; i < e->nargs; ++i) {
        V->expr(V, arg);
        check_same(V, arg->type, sig->args[i]); 
        arg = arg->next;
    }
    e->type = sig->ret;
}

void p_check_types(Lex *lex)
{
    paw_Env *P = env(lex);

    Visitor V;
    pawK_init_visitor(&V, lex);
    V.primitive_expr = visit_primitive_expr;
    V.literal_expr = visit_literal_expr;
    V.logical_expr = visit_logical_expr;
    V.chain_expr = visit_chain_expr;
    V.cond_expr = visit_cond_expr;
    V.coalesce_expr = visit_coalesce_expr;
    V.unop_expr = visit_unop_expr;
    V.binop_expr = visit_binop_expr;
    V.var_expr = visit_var_expr;
    V.array_expr = visit_array_expr;
    //V.map_expr = visit_map_expr;
    V.init_expr = visit_init_expr;
    V.access_expr = visit_access_expr;
    V.invoke_expr = visit_invoke_expr;
    V.index_expr = visit_index_expr;
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

    for (int i = 0; i < P->gv.size; ++i) {
        GlobalVar g = P->gv.data[i];
        Symbol *s = add_global(lex, g.desc.name, g.desc.type);
        define_var(s);
    }

    enter_function(lex, lex->modname, NULL, FN_MODULE);
    pawK_visit(&V, lex->ast);
    lex->pm->st.toplevel = leave_function(lex);

    paw_assert(lex->pm->st.nscopes == 0);
}

