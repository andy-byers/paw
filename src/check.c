// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// check.c: Implementation of the type checker. This code transforms an AST
// from the parser into a graph by unifying types based on lexical scope. 

#include "ast.h"
#include "array.h"
#include "check.h"
#include "code.h"
#include "env.h"
#include "gc_aux.h"
#include "map.h"
#include "mem.h"
#include "str.h"
#include "type.h"

// Helper macros
#define syntax_error(C, ...) pawX_error((C)->lex, __VA_ARGS__)
#define type_error(C, ...) pawX_error((C)->lex, __VA_ARGS__)
#define resolve_expr(V, e) ((V)->visit_expr(V, e), a_type(e))
#define cached_str(C, i) pawE_cstr(env((C)->lex), cast_size(i))
#define basic_decl(C, code) basic_symbol(C, code)->decl
#define type2code(e) (y_is_basic(e) ? (e)->hdr.def : -1)
#define is_unit(e) (type2code(e) == PAW_TUNIT)
#define normalize(C, e) pawP_normalize(C->U, e)
#define flag2code(flag) (-(flag) - 1)

// Entrypoint for type unification
#define unify(C, a, b) pawP_unify(C->U, a, b)

// Common state for type-checking routines
typedef struct Checker {
    Lex *lex; // lexical state
    StructDecl *struct_; // enclosing struct declaration
    Type *return_; // enclosing function return type                  
    SymbolTable *sym; // scoped symbol table
    Ast *ast; // AST being checked
    ParseMemory *pm; // dynamic memory
    Unifier *U; // unification tables
    GenericState *gs; // generic context
    AstVisitor *V1;                      
    int func_depth; // number of nested functions
    int nexpanded;
} Checker;

static Type *get_raw_type(Checker *C, DefId id)
{
    paw_assert(id < C->pm->decls.size);
    return a_type(C->pm->decls.data[id]);
}

static Type *get_type(Checker *C, DefId id)
{
    paw_assert(id < C->pm->decls.size);
    Type *raw = get_raw_type(C, id);
    return normalize(C, raw);
}

#define are_types_same(a, b) ((a) == (b))

static paw_Bool test_types(Checker *C, Type *a, Type *b);

static paw_Bool test_binders(Checker *C, Binder *a, Binder *b)
{
    for (int i = 0; i < a->count; ++i) {
        if (!test_types(C, a->types[i], b->types[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

static paw_Bool test_types(Checker *C, Type *a, Type *b)
{
    if (y_kind(a) != y_kind(b)) {
        return PAW_FALSE;
    }
    switch (y_kind(a)) {
        case TYPE_FUNC:
            return test_binders(C, &a->func.params, &b->func.params);
        case TYPE_ADT: {
            if (a->adt.target != b->adt.target) {
                return PAW_FALSE;
            }
            return test_binders(C, &a->adt.types, &b->adt.types);
        }
        default:
            return are_types_same(a, b);
    }
}

static Symbol *basic_symbol(Checker *C, paw_Type code)
{
    paw_assert(code >= 0 && code <= PAW_TSTRING);

    // basic types have fixed locations
    Scope *toplevel = C->sym->scopes[0];
    return toplevel->symbols[1 + code]; // TODO
}

static void push_symbol_table(Checker *C)
{
    pawP_new_scope(C->lex, C->sym);
}

static void pop_symbol_table(Checker *C)
{
    // Last symbol table should have been assigned to an AST node. The
    // next call to push_symbol_table() will allocate a new table.
    SymbolTable *st = C->sym;
    paw_assert(st->nscopes > 0);
    --st->nscopes;
}

static DefId add_decl(Checker *C, AstDecl *decl)
{
    paw_Env *P = env(C->lex);
    ParseMemory *pm = C->pm;
    pawM_grow(P, pm->decls.data, pm->decls.size, pm->decls.alloc);
    const DefId id = pm->decls.size++;
    pm->decls.data[id] = decl;
    decl->hdr.def = id;
    return id;
}

static AstDecl *get_decl(Checker *C, DefId id)
{
    ParseMemory *pm = C->pm;
    paw_assert(id < pm->decls.size);
    return pm->decls.data[id];
}

static Type *new_type(Checker *C, DefId id, TypeKind kind)
{
    paw_Env *P = env(C->lex);
    Type *type = pawY_type_new(P, P->mod);
    type->hdr.kind = kind;
    type->hdr.def = id;
    if (id != NO_DECL) {
        // set type of associated definition
        AstDecl *d = get_decl(C, id);
        d->hdr.type = type;
    }
    return type;
}

// TODO: Move this elsewhere, and have it write to a Buffer: use for error messages
static void dump_type(Checker *C, Type *type);

static void dump_binder(Checker *C, Binder *binder)
{
    for (int i = 0; i < binder->count; ++i) {
        dump_type(C, binder->types[i]);
        if (i < binder->count - 1) {
            printf(", ");
        }
    }
}

static void dump_type(Checker *C, Type *type)
{
    paw_Env *P = env(C->lex);
    const String *basic[] = {
        pawE_cstr(P, CSTR_UNIT),
        pawE_cstr(P, CSTR_BOOL),
        pawE_cstr(P, CSTR_INT),
        pawE_cstr(P, CSTR_FLOAT),
        pawE_cstr(P, CSTR_STRING),
    };
    switch (y_kind(type)) {
        case TYPE_VAR: {
            TypeVar *var = &type->var;
            printf("%s", var->name->text);
            break;
        }
        case TYPE_ADT: {
            Adt *adt = &type->adt;
            AstDecl *decl = get_decl(C, adt->target);
            printf("%s", decl->struct_.name->text);
            if (adt->types.count > 0) {
                printf("["); 
                dump_binder(C, &adt->types);
                printf("]"); 
            }
            break;
        }
        case TYPE_FUNC: {
            FuncSig *func = &type->func;
            if (func->types.count > 0) {
                printf("["); 
                dump_binder(C, &func->types);
                printf("]"); 
            }
            printf("("); 
            dump_binder(C, &func->params);
            printf(") -> "); 
            dump_type(C, func->return_);
            break;
        }
        default:
            paw_assert(y_is_basic(type));
            printf("%s", basic[type->hdr.def]->text);
    }
}

static Type *type_collector(AstVisitor *V, AstExpr *expr)
{
    return resolve_expr(V, expr);
}

static Type *param_collector(AstVisitor *V, AstDecl *decl)
{
    FieldDecl *d = &decl->field;
    d->type = resolve_expr(V, d->tag);
    return d->type;
}

static Type *generic_collector(AstVisitor *V, AstDecl *decl)
{
    GenericDecl *d = &decl->generic;
    DefId id = add_decl(V->state.C, decl);
    d->type = new_type(V->state.C, id, TYPE_VAR);
    d->type->var.name = d->name;
    return d->type;
}

static Binder *temp_binder(Checker *C, int count)
{
    ParseMemory *pm = C->pm;
    if (pm->temp.size == paw_countof(pm->temp.data)) {
        syntax_error(C, "too many nested binders");
    }
    Binder *binder = &pm->temp.data[pm->temp.size++];
    binder->types = pawM_new_vec(env(C->lex), count, Type *);
    binder->count = count;
    return binder;
}

#define make_collector(name, T, collect) \
    static Binder *collect_ ## name(AstVisitor *V, T ## List *list) \
    { \
        Checker *C = V->state.C; \
        Binder *binder = temp_binder(C, list->count); \
        T *node = list->first; \
        for (int i = 0; i < list->count; ++i) { \
            Type *type = collect(V, node); \
            binder->types[i] = type; \
            node = node->hdr.next; \
        } \
        return binder; \
    }
make_collector(types, AstExpr, type_collector)
make_collector(params, AstDecl, param_collector)
make_collector(generics, AstDecl, generic_collector)

static void lose_binders(Checker *C, int count)
{
    ParseMemory *pm = C->pm;
    paw_assert(pm->temp.size >= count);
    pm->temp.size -= count;
}

static void free_last_binder(Checker *C)
{
    ParseMemory *pm = C->pm;
    paw_assert(pm->temp.size > 0);
    pawM_free_vec(env(C->lex), pm->temp.data, pm->temp.size);
    --pm->temp.size;
}

static void enter_generic_ctx(Checker *C, GenericState *gs, UniTable *table)
{
    gs->outer = C->gs;
    pawP_unifier_enter(C->U, table);
    C->gs = gs;
}

static UniTable *leave_generic_ctx(Checker *C)
{
    C->gs = C->gs->outer;
    return pawP_unifier_leave(C->U);
}

static Scope *enclosing_scope(Checker *C)
{
    SymbolTable *st = C->sym;
    return st->scopes[st->nscopes - 1];
}

static Symbol *add_symbol(Checker *C, Scope *scope, String *name, AstDecl *decl)
{
    Symbol *symbol = pawP_add_symbol(C->lex, scope);
    symbol->name = name;
    symbol->decl = decl;
    return symbol;
}

static Symbol *add_local(Checker *C, String *name, AstDecl *decl)
{
    return add_symbol(C, enclosing_scope(C), name, decl);
}

static Symbol *add_global(Checker *C, String *name, AstDecl *decl)
{
    Scope *st = C->sym->globals;
    for (int i = 0; i < st->nsymbols; ++i) {
        Symbol *sym = st->symbols[i];
        if (pawS_eq(sym->name, name)) {
            syntax_error(C, "duplicate global '%s'", name->text); 
        }
    }
    return add_symbol(C, st, name, decl);
}

static Symbol *resolve_symbol(Checker *C, String *name)
{
    // search the scoped symbols
    SymbolTable *scopes = C->sym;
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
        syntax_error(C, "undefined symbol '%s'", name->text);
    }
    return scopes->globals->symbols[index];
}

static AstDecl *resolve_attr(AstDeclList *attrs, String *name)
{
    AstDecl *decl = attrs->first;
    while (decl != NULL) {
        if (pawS_eq(name, decl->hdr.name)) {
            return decl; 
        }
        if (a_kind(decl) == DECL_FUNC) {
            decl = decl->func.sibling;
        } else {
            decl = decl->hdr.next;
        }
    }
    return NULL;
}

// Register the name and type of a variable
// If 'global' is true, then the variable is a global, otherwise, it is a local.
// Must be called prior to 'define_symbol',
static Symbol *declare_symbol(Checker *C, String *name, AstDecl *decl, paw_Bool global)
{
    return global ? add_global(C, name, decl)
                  : add_local(C, name, decl);
}

// Allow a previously-declared variable to be accessed
static void define_symbol(Symbol *symbol)
{
    symbol->is_init = PAW_TRUE;
}

static Symbol *new_symbol(Checker *C, String *name, AstDecl *decl, paw_Bool global)
{
    Symbol *symbol = declare_symbol(C, name, decl, global);
    define_symbol(symbol);
    return symbol;
}

#define new_local(C, name, decl) new_symbol(C, name, decl, PAW_FALSE)
#define new_global(C, name, decl) new_symbol(C, name, decl, PAW_FALSE)

static Scope *leave_block(Checker *C)
{
    Scope *scope = enclosing_scope(C);
    pop_symbol_table(C);
    return scope;
}

static void enter_block(Checker *C, Scope *scope)
{
    if (scope == NULL) {
        push_symbol_table(C);
    } else {
        // use an existing scope
        pawP_add_scope(C->lex, C->sym, scope);    
    }
}

static Scope *leave_function(Checker *C)
{
    Scope *scope = leave_block(C);
    check_gc(env(C->lex));
    --C->func_depth;
    return scope;
}

static void enter_function(Checker *C, String *name, Scope *scope, FuncDecl *func, FuncKind kind)
{
    // Enter the function body.
    ++C->func_depth;
    enter_block(C, scope);

    // for methods, slot 0 is the context variable ('self')
    if (kind == FUNC_METHOD) {
        name = cached_str(C, CSTR_SELF);
        new_local(C, name, func->receiver);
    } else {
        new_local(C, name, cast_decl(func));
    }
}

static void new_local_literal(Checker *C, const char *name, paw_Type code)
{
    Symbol *symbol = basic_symbol(C, code);
    new_local(C, scan_string(C->lex, name), symbol->decl);
}

static void visit_block_stmt(AstVisitor *V, Block *block)
{
    enter_block(V->state.C, block->scope);
    V->visit_stmt_list(V, block->stmts, V->visit_stmt);
    block->scope = leave_block(V->state.C);
}

static StructDecl *instantiate_struct(AstVisitor *V, StructDecl *base, Binder *types);

static void create_type_vars(Checker *C, AstDeclList *types)
{
    int index = 0;
    Unifier *U = C->U;
    AstDecl *decl = types->first;
    do {
        Type *type = get_raw_type(C, decl->hdr.def);
        type->var.depth = U->depth;
        type->var.index = index++;
        pawP_new_type_var(U, type);
        decl = decl->hdr.next;
    } while (decl != NULL);
}

static void check_template_param(Checker *C, AstDeclList *params, Binder *args)
{
    if (args->count > params->count) {
        syntax_error(C, "too many generics"); 
    } else if (args->count < params->count) {
        syntax_error(C, "not enough generics"); 
    }
}

static StructDecl *init_struct_template(AstVisitor *V, StructDecl *base, Binder *types)
{
    GenericState gs;
    Checker *C = V->state.C;
    enter_generic_ctx(C, &gs, NULL);

    StructDecl *inst;
    check_template_param(C, base->generics, types);
    inst = instantiate_struct(V, base, types);

    inst->unify = leave_generic_ctx(C);
    return inst;
}

static void bind_types(Checker *C, Binder *base, Binder *inst, paw_Bool is_generic)
{
    paw_assert(base->count == inst->count);
    for (int i = 0; i < base->count; ++i) {
        Type *bt = base->types[i];
        Type *it = inst->types[i];
        AstDecl *decl = get_decl(C, it->hdr.def);
        Symbol *symbol = new_local(C, bt->var.name, decl);
        symbol->is_generic = is_generic;
        symbol->is_type = PAW_TRUE;
    }
}

static void setup_poly_func(AstVisitor *V, FuncDecl *d, Type *type)
{
    Checker *C = V->state.C;
    enter_block(C, d->scope);
    d->type = type;

    // TODO: NOTE: This uses the base's type vars, may need to use instance vars (currently set to NULL)
    Type *base = get_raw_type(C, type->func.base);
    bind_types(C, &base->func.types, &type->func.types, d->is_poly);

    type->func.params = *collect_params(V, d->params);
    type->func.return_ = resolve_expr(V, d->return_);
    lose_binders(C, 1);

    d->scope = leave_block(C);
}

static void setup_poly_struct(AstVisitor *V, StructDecl *d)
{
    Checker *C = V->state.C;
    enter_block(C, d->scope);
    Type *type = d->type;

    Type *base = get_raw_type(C, type->adt.target);
    bind_types(C, &base->adt.types, &type->adt.types, d->is_poly);

    d->scope = leave_block(C);
}

static void define_func(AstVisitor *V, FuncDecl *d)
{
    Checker *C = V->state.C;
    DefId id = add_decl(C, cast_decl(d));
    Type *r = new_type(C, id, TYPE_FUNC);
    d->type = r;

    r->func.params = *collect_params(V, d->params);
    r->func.return_ = resolve_expr(V, d->return_);
    lose_binders(C, 1);
}

static void define_poly_func(AstVisitor *V, FuncDecl *d, DefId base, Binder *types)
{
    Checker *C = V->state.C;
    DefId id = add_decl(C, cast_decl(d));
    Type *r = new_type(C, id, TYPE_FUNC);
    d->type = r;

    r->func.base = base == NO_DECL ? d->def : base;
    r->func.types = *types;

    setup_poly_func(V, d, r);
    lose_binders(C, 1);
}

static void define_struct(AstVisitor *V, StructDecl *d)
{
    DefId id = add_decl(V->state.C, cast_decl(d));
    d->type = new_type(V->state.C, id, TYPE_ADT);
    d->type->adt.target = d->def;
}

static void define_poly_struct(AstVisitor *V, StructDecl *d, DefId base, Binder *types)
{
    define_struct(V, d);
    d->type->adt.types = *types;
    d->type->adt.target = base == NO_DECL ? d->def : base;

    setup_poly_struct(V, d);
    lose_binders(V->state.C, 1);
}

static void expect_bool_expr(AstVisitor *V, AstExpr *e)
{
    Checker *C = V->state.C;
    Type *type = resolve_expr(V, e);
    unify(C, type, get_raw_type(C, PAW_TBOOL));
}

static void expect_int_expr(AstVisitor *V, AstExpr *e)
{
    Checker *C = V->state.C;
    Type *type = resolve_expr(V, e);
    unify(C, type, get_raw_type(C, PAW_TINT));
}

static void visit_type_name_expr(AstVisitor *V, TypeName *e)
{
    Checker *C = V->state.C;
    Symbol *symbol = resolve_symbol(C, e->name);
    AstDecl *decl = symbol->decl;
    if (a_kind(decl) == DECL_VAR) {
        type_error(C, "'%s' is not a type", symbol->name->text);
    } else if (a_is_struct_template_decl(decl)) {
        StructDecl *base = &decl->struct_;
        Binder *types = collect_types(V, e->args);
        StructDecl *inst = init_struct_template(V, base, types);
        decl = cast_decl(inst);
    }
    e->type = a_type(decl);
}

static void visit_ident_expr(AstVisitor *V, AstIdent *e)
{
    Symbol *symbol = resolve_symbol(V->state.C, e->name);
    e->type = get_type(V->state.C, symbol->decl->hdr.def);
}

static void visit_logical_expr(AstVisitor *V, LogicalExpr *e)
{
    expect_bool_expr(V, e->lhs);
    expect_bool_expr(V, e->rhs);
    e->type = a_type(e->lhs); // same as 'rhs'
}

//  TODO: Needs a constraint
static void visit_chain_expr(AstVisitor *V, ChainExpr *e)
{
    Type *type = resolve_expr(V, e->target);
//    if (!a_is_object(e->target->hdr.type)) {
//        type_error(C, "'?' operator requires an object");
//    }
}

static void visit_cond_expr(AstVisitor *V, CondExpr *e)
{
    expect_bool_expr(V, e->cond);
    Type *lhs = resolve_expr(V, e->lhs);
    Type *rhs = resolve_expr(V, e->rhs);
    unify(V->state.C, lhs, rhs);
}

static void visit_unop_expr(AstVisitor *V, UnOpExpr *e)
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

    Checker *C = V->state.C;
    Type *type = resolve_expr(V, e->target);
    const paw_Type code = type2code(type);
    if (!kValidOps[e->op][code]) {
        type_error(C, "unsupported operand type for unary '%s'");
    } else if (unop_is_bool(e->op)) {
        e->type = get_raw_type(C, PAW_TBOOL);
    } else {
        e->type = type;
    }
}

// TODO: BINARY_IN should be handled separately
static void visit_binop_expr(AstVisitor *V, BinOpExpr *e)
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

    Checker *C = V->state.C;
    Type *lhs = resolve_expr(V, e->lhs);
    Type *rhs = resolve_expr(V, e->rhs);
    unify(C, lhs, rhs);

    const paw_Type left = type2code(lhs);
    const paw_Type right = type2code(rhs);
    if (left < 0 || right < 0 || left != right || !kValidOps[e->op][left]) {
        type_error(C, "unsupported operand types for binary '%s'");
    } else if (binop_is_bool(e->op)) {
        e->type = get_raw_type(C, PAW_TBOOL);
    } else {
        e->type = lhs;
    }
}

static void visit_param_decl(AstVisitor *V, AstDecl *decl)
{
    FieldDecl *d = &decl->field;
    d->type = resolve_expr(V, d->tag);

    new_local(V->state.C, d->name, decl);
    add_decl(V->state.C, decl);
}

static void visit_signature_expr(AstVisitor *V, FuncType *e)
{
    Checker *C = V->state.C;
    e->type = new_type(C, NO_DECL, TYPE_FUNC);
    e->type->func.params = *collect_types(V, e->params);
    e->type->func.return_ = resolve_expr(V, e->return_);
    lose_binders(C, 1);
}

static void visit_func(AstVisitor *V, FuncDecl *d, FuncKind kind)
{
    Checker *C = V->state.C;
    Type *type = get_raw_type(C, d->def);
    d->fn_kind = kind;

    enter_function(C, d->name, d->scope, d, kind);
    V->visit_decl_list(V, d->params, visit_param_decl);

    Type *outer = C->return_;
    C->return_ = type->func.return_;

    V->visit_block_stmt(V, d->body);
    d->scope = leave_function(C);
    C->return_ = outer;
    d->is_visited = PAW_TRUE;
}

static void register_func(AstVisitor *V, FuncDecl *d)
{
    Checker *C = V->state.C;
    if (!d->is_poly) {
        define_func(V, d);
        return;
    }
    if (C->func_depth > 1) {
        // TODO: This restriction makes it far easier to implement generics. It will
        //       likely be lifted in the future.
        syntax_error(C, "templates must be toplevel");
    }
    GenericState gs;
    enter_generic_ctx(C, &gs, d->unify);
    Binder *types = collect_generics(V, d->generics);
    create_type_vars(C, d->generics);
    define_poly_func(V, d, NO_DECL, types);
    d->unify = leave_generic_ctx(C);
}

static void traverse_func(AstVisitor *V, FuncDecl *d, FuncKind kind)
{
    Checker *C = V->state.C;
    if (!d->is_poly) {
        visit_func(V, d, kind);
    } else {
        GenericState gs;
        enter_generic_ctx(C, &gs, d->unify);
        visit_func(V, d, kind);
        d->unify = leave_generic_ctx(C);
    }
}

static void visit_return_stmt(AstVisitor *V, ReturnStmt *s)
{
    Checker *C = V->state.C;
    Type *want = C->return_; // function return type
    Type *have = s->expr ? resolve_expr(V, s->expr) : NULL;

    if (y_is_unit(want)) {
        if (have != NULL && !y_is_unit(have)) {
            type_error(C, "expected '()' or empty return");
        }
    } else if (have != NULL) {
        unify(C, have, want);
    } else {
        type_error(C, "expected nonempty return");
    } 
}

static void register_method_decl(AstVisitor *V, AstDecl *method)
{   
    FuncDecl *d = &method->func;
    register_func(V, d);

    Checker *C = V->state.C;
    d->receiver = cast_decl(C->struct_);
}

static void visit_method_decl(AstVisitor *V, AstDecl *method)
{   
    FuncDecl *d = &method->func;
    traverse_func(V, d, FUNC_METHOD);
}

static void visit_field_decl(AstVisitor *V, AstDecl *decl)
{
    add_decl(V->state.C, decl);
    FieldDecl *d = &decl->field;
    d->type = resolve_expr(V, d->tag);
}

static void register_struct(AstVisitor *V, StructDecl *d)
{
    Checker *C = V->state.C;
    StructDecl *enclosing = C->struct_;
    C->struct_ = d; // enter struct context
    enter_block(C, d->scope);

    // Resolve the fields and method signatures, but don't visit the method 
    // bodies. This prevents a situation where we could end up visiting a
    // template instance before we are finished with the template itself.
    V->visit_decl_list(V, d->fields, visit_field_decl);
    V->visit_method_list(V, d->methods, register_method_decl);

    d->scope = leave_block(C);
    C->struct_ = enclosing;
}

static void visit_struct(AstVisitor *V, StructDecl *d)
{
    Checker *C = V->state.C;
    StructDecl *enclosing = C->struct_;
    C->struct_ = d; // enter struct context
    enter_block(C, d->scope);

    V->visit_method_list(V, d->methods, visit_method_decl);

    d->scope = leave_block(C);
    C->struct_ = enclosing;
    d->is_visited = PAW_TRUE;
}

static void visit_struct_decl(AstVisitor *V, StructDecl *d)
{
    Checker *C = V->state.C;
    Symbol *symbol = new_symbol(C, d->name, cast_decl(d), d->is_global);
    if (!d->is_poly) {
        define_struct(V, d);
        register_struct(V, d);
        visit_struct(V, d);
    } else {
        if (C->func_depth > 1) {
            // TODO: This restriction makes it far easier to implement generics. It will
            //       likely be lifted in the future.
            syntax_error(C, "templates must be toplevel");
        }
        symbol->is_type = PAW_TRUE;

        GenericState gs;
        enter_generic_ctx(C, &gs, d->unify);
        Binder *types = collect_generics(V, d->generics);
        create_type_vars(C, d->generics);
        define_poly_struct(V, d, NO_DECL, types);
        register_struct(V, d);
        visit_struct(V, d);
        d->unify = leave_generic_ctx(C);

//        AstDecl *inst = d->next;
//        while (inst != NULL) {
//            d = &inst->struct_; 
//            enter_generic_ctx(C, &gs, d->unify);
//            visit_struct(V, d);
//            d->unify = leave_generic_ctx(C);
//        }
    }
}

static void visit_var_decl(AstVisitor *V, VarDecl *d)
{
    Checker *C = V->state.C;
    Symbol *symbol = declare_symbol(C, d->name, cast_decl(d), d->is_global);
    Type *init = resolve_expr(V, d->init);
    define_symbol(symbol);

    if (d->tag != NULL) {
        // check initializer against annotation
        Type *tag = resolve_expr(V, d->tag);
        unify(C, init, tag);
    }
    add_decl(C, cast_decl(d));
    d->type = init;
}

static void visit_type_decl(AstVisitor *V, TypeDecl *d)
{
    // TODO: generic parameters for aliases
    Symbol *symbol = declare_symbol(V->state.C, d->name, cast_decl(d), PAW_FALSE);
    d->type = resolve_expr(V, d->rhs);
    //create_type_vars(C, d->generics);
    //unify(C, d->name, d->type);
    define_symbol(symbol);
}

// Run the inference algorithm
//
//  parameters
// ------------
//     generics: type parameters from template. 
//     params: formal parameters from function signature
//     args: arguments from function call
static Binder *infer_template_param(AstVisitor *V, AstDeclList *generics, AstDeclList *params, AstExprList *args)
{
    Checker *C = V->state.C;
    AstDecl *generic = generics->first;
    paw_assert(generic != NULL);
    do {
        GenericDecl *d = &generic->generic;
        Type *type = get_raw_type(C, d->def);
        pawP_new_type_var(C->U, type);
        generic = d->next;
    } while (generic != NULL);

    // Attempt to determine a type for each generic parameter, using the
    // combination of function parameters and arguments. Any parameter type
    // might equal, or contain, one of the generic type parameters.
    AstExpr *arg = args->first;
    AstDecl *par = params->first;
    while (arg && par) {
        Type *a = par->field.type;
        Type *b = resolve_expr(V, arg);
        unify(C, a, b);
        par = par->hdr.next;
        arg = arg->hdr.next;
    }

    // Create a list of type parameters for the instance.
    Binder *types = temp_binder(C, generics->count);
    generic = generics->first;
    for (int i = 0; i < generics->count; ++i) {
        GenericDecl *d = &generic->generic;
        Type *type = get_type(C, d->def);
        if (y_is_type_var(type) && p_is_bound(C->U, type)) {
            type_error(C, "unable to infer generic parameter '%s'", d->name->text);
        }
        types->types[i] = type;
        generic = d->next;
    }
    return types;
}

static FuncDecl *find_func_instance(Checker *C, FuncDecl *base, Binder *types)
{
    paw_assert(types->count > 0);
    AstDecl *inst = base->next;
    while (inst != NULL) {
        FuncDecl *func = &inst->func;
        AstDecl *decl = func->generics->first;
        // NOTE: must enter the loop: requires at least 1 type argument
        for (int i = 0; i < types->count; ++i) {
            GenericDecl *generic = &decl->generic;
            Type *type = get_type(C, generic->def);
            if (!test_types(C, type, types->types[i])) {
                goto next_inst;
            }
            decl = generic->next;
        }
        // Found an existing function template instance. Use its inference
        // variables, which should already be resolved to concrete types.
        return func;

next_inst:
        inst = inst->hdr.next;
    }
    return NULL;
}

static StructDecl *find_struct_instance(Checker *C, StructDecl *base, Binder *types)
{
    AstDecl *inst = base->next;
    while (inst != NULL) {
        StructDecl *struct_ = &inst->struct_;
        Type *type = get_type(C, struct_->def);
        if (test_binders(C, &type->adt.types, types)) {
            return struct_;
        }
        inst = inst->hdr.next;
    }
    return NULL;
}

static FuncDecl *new_func_instance(AstVisitor *V, FuncDecl *base, Binder *types)
{
    // Copy the whole function template subtree.
    AstDecl *stencil = pawA_stencil(V->ast, cast_decl(base));
    FuncDecl *inst = &stencil->func;
    inst->is_poly = PAW_FALSE;

    inst->next = base->next;
    base->next = cast_decl(inst);

    define_poly_func(V, inst, base->def, types);
    visit_func(V, inst, inst->fn_kind);
    return inst;
}

static StructDecl *new_struct_instance(AstVisitor *V, StructDecl *base, Binder *types)
{
    AstDecl *stencil = pawA_stencil(V->ast, cast_decl(base));
    StructDecl *inst = &stencil->struct_;
    inst->is_poly = PAW_FALSE;

    inst->next = base->next;
    base->next = cast_decl(inst);

    define_poly_struct(V, inst, base->def, types);
    // Determine attribute types, but don't visit the method bodies, since we may
    // still be in the process of visiting the base template's method bodies. That 
    // would cause control to visit the instance before finishing with the template 
    // itself.
    register_struct(V, inst); 

   // Checker *C = V->state.C;
   // if (C->struct_ != base) {
        visit_struct(V, inst);
   // }
    return inst;
}

static FuncDecl *instantiate_func(AstVisitor *V, FuncDecl *base, Binder *types)
{
    Checker *C = V->state.C;
    FuncDecl *inst = find_func_instance(C, base, types);
    if (inst != NULL) {
       // pawP_unifier_replace(C->U, inst->unify);
    } else {
        inst = new_func_instance(V, base, types);
    }
    return inst;
}

static FuncDecl *init_func_template(AstVisitor *V, FuncDecl *base, Binder *types)
{
    Checker *C = V->state.C;
    GenericState gs;

    enter_generic_ctx(C, &gs, NULL);
    check_template_param(C, base->generics, types);
    FuncDecl *inst = instantiate_func(V, base, types);
    inst->unify = leave_generic_ctx(C);
    return inst;
}

static FuncDecl *infer_func_template(AstVisitor *V, FuncDecl *base, AstExprList *args)
{
    Checker *C = V->state.C;
    GenericState gs;

    enter_generic_ctx(C, &gs, NULL);
    Binder *types = infer_template_param(V, base->generics, base->params, args);
    FuncDecl *inst = instantiate_func(V, base, types);
    inst->unify = leave_generic_ctx(C);
    return inst;
}

static StructDecl *get_struct_decl(Checker *C, Type *type)
{
    paw_assert(y_kind(type) == TYPE_ADT);
    AstDecl *decl = get_decl(C, type->adt.def);
    paw_assert(a_kind(decl) == DECL_STRUCT);
    return &decl->struct_;
}

static FuncDecl *infer_method_template(AstVisitor *V, FuncDecl *base, AstExprList *args)
{
    Checker *C = V->state.C;
    if (!base->is_poly) {
        return base;
    }
    StructDecl *parent = &base->receiver->struct_;
    StructDecl *outer = C->struct_;
    C->struct_ = parent;

    enter_block(C, parent->scope);
    FuncDecl *inst = infer_func_template(V, base, args);
    pop_symbol_table(C);

    C->struct_ = outer;
    return inst;
}

static StructDecl *instantiate_struct(AstVisitor *V, StructDecl *base, Binder *types)
{
    Checker *C = V->state.C;
    StructDecl *inst = find_struct_instance(C, base, types);
    if (inst != NULL) {
        //pawP_unifier_replace(C->U, inst->unify);
    } else {
        inst = new_struct_instance(V, base, types);
    }
    return inst;
}

static Type *setup_func(AstVisitor *V, CallExpr *call, FuncDecl *d)
{
    if (d->is_poly) {
        d = infer_func_template(V, d, call->args);
    }
    return d->type;
}

static Type *setup_method(AstVisitor *V, CallExpr *call, FuncDecl *d)
{
    paw_assert(a_kind(call->target) == EXPR_SELECTOR);
    if (d->is_poly) {
        d = infer_method_template(V, d, call->args);
    }
    // Lets the codegen V know to generate OP_INVOKE instead of OP_CALL,
    // without having to look at the callable.
    call->target->selector.is_method = PAW_TRUE;
    return d->type;
}

static Type *setup_call(AstVisitor *V, CallExpr *e)
{
    Type *target = resolve_expr(V, e->target);
    if (!y_is_func(target)) {
        type_error(V->state.C, "type is not callable");
    } 
    FuncSig *func = &target->func;
    if (func->def == NO_DECL) {
        // This happens when we don't know where a function was declared. For
        // example, the following code can produce different values of 'g' 
        // depending on what value 'n' is given. This is okay, because the
        // function returned by 'f' must already be instantiated, if it is a
        // function template:
        //     let n = <some int>
        //     fn f(n: int) -> fn() {...}
        //     let g = f(n)
        //     g()
        return target;
    }
    // Function type has an associated declaration. If that declaration is for a
    // function or method template, attempt to infer the type parameters.
    AstDecl *decl = get_decl(V->state.C, func->def);
    return decl->func.fn_kind == FUNC_METHOD
        ? setup_method(V, e, &decl->func)
        : setup_func(V, e, &decl->func);

}

static void visit_call_expr(AstVisitor *V, CallExpr *e)
{
    Checker *C = V->state.C;
    // Determine the type of the callable, then find its declaration. Template functions will
    // need type inference, which is handled in setup_call().
    e->func = setup_call(V, e);
    e->type = e->func->func.return_;
    if (y_kind(e->func) != TYPE_FUNC) {
        type_error(C, "type is not callable");
    }
    FuncSig *func = &e->func->func;
    Binder *params = &func->params;
    if (params->count != e->args->count) {
        syntax_error(C, "expected %d parameter(s) but found %d", 
                     func->params.count, e->args->count);
    }
    // check call arguments against function parameters
    AstExpr *arg = e->args->first;
    for (int i = 0; i < params->count; ++i) {
        Type *type = resolve_expr(V, arg);
        unify(C, type, params->types[i]);
        arg = arg->hdr.next;
    }
}

// TODO: scratch allocations need to be boxed
//       could allow unnamed fields for other classes if they are in the correct order already
static Type *visit_composite_lit(AstVisitor *V, LiteralExpr *lit)
{
    CompositeLit *e = &lit->comp;
    Checker *C = V->state.C;
    Lex *lex = C->lex;

    // Replace the AstIdent or IndexExpr with the TypeName of the structure.
    Type *target = resolve_expr(V, e->target);
    if (!y_is_adt(target)) {
        type_error(C, "expected structure type");
    }
    StructDecl *struct_ = get_struct_decl(C, target);
    if (struct_->is_poly) {
        type_error(C, "struct template requires explicit type arguments");
    }
    // Use a temporary Map to avoid searching repeatedly through the
    // list of attributes. 
    paw_Env *P = env(lex);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    v_set_object(pv, map);

    Value key;
    AstExpr *item = e->items->first;
    AstExpr **order = pawM_new_vec(P, e->items->count, AstExpr *);
    for (int i = 0; item != NULL; ++i) {
        ItemExpr *ie = &item->item;
        v_set_object(&key, ie->name);
        if (pawH_contains(P, map, key)) {
            syntax_error(C, "duplicate attribute '%s' in struct '%s'",
                         ie->name->text, struct_->name->text);
        }
        Value *value = pawH_action(P, map, key, MAP_ACTION_CREATE);
        v_set_int(value, i);
        order[i] = item;
        item = ie->next;
    }
    AstDecl *decl = struct_->fields->first;
    for (int i = 0; i < struct_->fields->count; ++i) {
        FieldDecl *field = &decl->field;
        v_set_object(&key, field->name);
        Value *value = pawH_get(P, map, key);
        if (value == NULL) {
            syntax_error(C, "missing initializer for field '%s' in struct '%s'",
                       field->name->text, e->target->type_name.name->text);
        } else {
            const paw_Int index = v_int(*value);
            ItemExpr *ie = &order[index]->item;
            ie->index = i; // index of attribute in struct
            Type *a = resolve_expr(V, ie->value);
            Type *b = get_type(C, field->def);
            unify(C, a, b);
        }
        pawH_remove(P, map, key);
        decl = field->next;
    }
    if (pawH_length(map) > 0) {
        syntax_error(C, "found %s extra initializers");
    }
    paw_assert(struct_->fields->count == e->items->count);

    pawC_pop(P); // pop map
    pawM_free_vec(P, order, e->items->count);
    return target;
}

static void visit_literal_expr(AstVisitor *V, LiteralExpr *e)
{
    if (e->lit_kind == LIT_BASIC) {
        e->type = get_raw_type(V->state.C, e->basic.t);
    } else {
        paw_assert(e->lit_kind == LIT_COMPOSITE);
        e->type = visit_composite_lit(V, e);
    }
}

static void visit_func_decl(AstVisitor *V, FuncDecl *d)
{
    Symbol *symbol = declare_symbol(V->state.C, d->name, cast_decl(d), d->is_global);
    symbol->is_type = d->is_poly;
    register_func(V, d);
    traverse_func(V, d, FUNC_FUNCTION);
    define_symbol(symbol);
}

static void visit_if_stmt(AstVisitor *V, IfStmt *s)
{
    expect_bool_expr(V, s->cond);
    V->visit_stmt(V, s->then_arm);
    V->visit_stmt(V, s->else_arm);
}

static void visit_expr_stmt(AstVisitor *V, AstExprStmt *s)
{
    Type *lhs = resolve_expr(V, s->lhs);
    if (s->rhs != NULL) {
        Type *rhs = resolve_expr(V, s->rhs);
        unify(V->state.C, lhs, rhs);
    }
}

static void visit_while_stmt(AstVisitor *V, WhileStmt *s)
{
    enter_block(V->state.C, NULL);
    expect_bool_expr(V, s->cond);
    V->visit_block_stmt(V, s->block);
    s->scope = leave_block(V->state.C);
}

static void visit_dowhile_stmt(AstVisitor *V, WhileStmt *s)
{
    enter_block(V->state.C, NULL);
    V->visit_block_stmt(V, s->block);
    expect_bool_expr(V, s->cond);
    s->scope = leave_block(V->state.C);
}

static void visit_for_body(AstVisitor *V, String *iname, Block *b)
{
    Checker *C = V->state.C;
    enter_block(C, NULL);
    new_local(C, iname, basic_decl(C, PAW_TINT));
    V->visit_stmt_list(V, b->stmts, V->visit_stmt);
    b->scope = leave_block(C);
}

static void visit_fornum(AstVisitor *V, ForStmt *s)
{
    ForNum *fornum = &s->fornum;

    expect_int_expr(V, fornum->begin);
    expect_int_expr(V, fornum->end);
    expect_int_expr(V, fornum->step);

    new_local_literal(V->state.C, "(for begin)", PAW_TINT);
    new_local_literal(V->state.C, "(for end)", PAW_TINT);
    new_local_literal(V->state.C, "(for step)", PAW_TINT);

    visit_for_body(V, s->name, s->block);
}

static void visit_forin(AstVisitor *V, ForStmt *s) // TODO: forin would need to encode the type of object being iterated over. look into function call for loop? 
{
//    Lex *lex = C->lex;
//    ForIn *forin = &s->forin;
//    new_local_literal(C, "(for target)", PAW_TINT);
//    new_local_literal(C, "(for iterator)", PAW_TINT);
//    V->visit_expr(V, forin->target);
//
//    Type *inner = pawY_unwrap(env(lex), forin->target->type);
//    new_local(C, s->name, inner);
//
//    V->visit_for_body(V, s->name, s->block);
}

static void visit_for_stmt(AstVisitor *V, ForStmt *s)
{
    enter_block(V->state.C, NULL);
    if (s->kind == STMT_FORNUM) {
        visit_fornum(V, s);
    } else {
        visit_forin(V, s);
    }
    s->scope = leave_block(V->state.C);
}

static void check_index(Checker *C, Type *target, Type *elem)
{
    // TODO: Unify elem with the type expected by target container, return contained type
}

static Type *explicit_func_template(AstVisitor *V, FuncDecl *base, Index *e)
{
    Binder *types = collect_types(V, e->elems);
    FuncDecl *inst = init_func_template(V, base, types);
    return inst->type;
}

static Type *explicit_struct_template(AstVisitor *V, StructDecl *base, Index *e)
{
    Binder *types = collect_types(V, e->elems);
    StructDecl *inst = init_struct_template(V, base, types);
    return inst->type;
}

static Type *explicit_method_template(AstVisitor *V, FuncDecl *base, Index *e)
{
    Checker *C = V->state.C;
    StructDecl *parent = &base->receiver->struct_;
    StructDecl *outer = C->struct_;
    C->struct_ = parent;

    enter_block(C, parent->scope);
    Binder *types = collect_types(V, e->elems);
    FuncDecl *inst = init_func_template(V, base, types);
    pop_symbol_table(C);

    C->struct_ = outer;
    return inst->type;
}

static void visit_index_expr(AstVisitor *V, Index *e)
{
    Checker *C = V->state.C;
    Type *target = resolve_expr(V, e->target);
    AstDecl *decl = get_decl(C, target->hdr.def);
    if (!a_is_template_decl(decl)) {
        // 'e' represents a getter for a container element, rather than a
        // template instantiation: no folding is necessary 
        if (e->elems->count != 1) {
            paw_assert(e->elems->count > 1);
            syntax_error(C, "too many indices (must be 1)");
        }
        Type *elem = resolve_expr(V, e->elems->first);
        check_index(C, target, elem);
    } 
    if (a_kind(decl) == DECL_STRUCT) {
        e->type = explicit_struct_template(V, &decl->struct_, e);
    } else if (a_has_receiver(decl)) {
        e->type = explicit_method_template(V, &decl->func, e);
    } else {
        e->type = explicit_func_template(V, &decl->func, e);
    }
}

static void visit_item_expr(AstVisitor *V, ItemExpr *e)
{
    e->type = resolve_expr(V, e->value);
}

static void visit_selector_expr(AstVisitor *V, Selector *e)
{
    Checker *C = V->state.C;
    Type *type = resolve_expr(V, e->target);
    if (!y_is_adt(type)) {
        type_error(C, "expected struct instance");
    }
    StructDecl *d = get_struct_decl(C, type);
//    e->name = pawI_mangle_attr(C->lex, d->name, e->name);

    AstDecl *attr = resolve_attr(d->fields, e->name);
    if (attr == NULL) {
        attr = resolve_attr(d->methods, e->name);
        if (attr == NULL) {
            syntax_error(C, "attribute '%s' does not exist", e->name->text);
        }
        e->is_method = PAW_TRUE;
    }
    e->type = get_type(C, attr->hdr.def);
}

static void visit_decl_stmt(AstVisitor *V, AstDeclStmt *s)
{
    V->visit_decl(V, s->decl);
}

static void expand_func_decl(AstVisitor *V, FuncDecl *d)
{
    Checker *C = V->state.C;
    AstDecl *decl = d->next;
    while (decl != NULL) {
        d = &decl->func;
        if (!d->is_visited) {
            traverse_func(C->V1, d, FUNC_FUNCTION);
            ++V->state.C->nexpanded;
        }
        decl = d->next;
    }
}

static void expand_struct_decl(AstVisitor *V, StructDecl *d)
{
    Checker *C = V->state.C;
    AstDecl *decl = d->next;
    while (decl != NULL) {
        d = &decl->struct_;
        if (!d->is_visited) {
            visit_struct(C->V1, d);
            ++V->state.C->nexpanded;
        }
        decl = d->next;
    }
}

static void setup_globals(Checker *C)
{
    paw_Env *P = env(C->lex);
    for (int i = 0; i < P->gv.size; ++i) {
        // TODO: global type info -> AST type node for globals
        GlobalVar g = P->gv.data[i];
        AstDecl *d = pawA_new_decl(C->ast, DECL_FUNC);
        d->func.type = P->mod->types[g.desc.code];
        d->func.is_global = PAW_TRUE;
        add_decl(C, d);

        //AstExpr *type = pawA_new_expr(C->ast, EXPR_FUNC_TYPE);
        //if(0==strcmp(g.desc.name->text,"assert")){ // TODO: Write a function that parses type info into an AST type
        //    type->func.params = get_type(C, PAW_TBOOL);
        //    type->func.nparams = 1;
        //    type->func.return_ = get_type(C, PAW_TUNIT);
        //}else if(0==strcmp(g.desc.name->text,"print")){
        //    type->func.params = get_type(C, PAW_TSTRING);
        //    type->func.nparams = 1;
        //    type->func.return_ = get_type(C, PAW_TUNIT);
        //} else{
        //    paw_assert(0);
        //}
        //d->func.type = type;
        //type->func.decl = d;
        Symbol *s = add_global(C, g.desc.name, d);
        define_symbol(s);
    }
}

static void add_basic_symbol(Checker *C, String *name)
{
    AstExpr *e = pawA_new_expr(C->ast, EXPR_TYPE_NAME);
    e->type_name.args = pawA_new_expr_list(C->ast);
    e->type_name.name = name;

    AstDecl *d = pawA_new_decl(C->ast, DECL_TYPE);
    d->type.name = name;
    d->type.line = 0;
    d->type.rhs = e;

    add_decl(C, d);

    paw_Env *P = env(C->lex);
    const paw_Type code = flag2code(name->flag);
    d->hdr.type = P->mod->types[code];
    Symbol *symbol = new_local(C, name, d);
    symbol->is_type = PAW_TRUE;
}

static void setup_resolver(AstVisitor *V, Checker *C)
{
    add_basic_symbol(C, cached_str(C, CSTR_UNIT));
    add_basic_symbol(C, cached_str(C, CSTR_BOOL));
    add_basic_symbol(C, cached_str(C, CSTR_INT));
    add_basic_symbol(C, cached_str(C, CSTR_FLOAT));
    add_basic_symbol(C, cached_str(C, CSTR_STRING));

    // no generic context
    C->U->depth = -1;

    setup_globals(C);

    const AstState state = {.C = C};
    pawA_visitor_init(V, C->ast, state);
    V->visit_literal_expr = visit_literal_expr;
    V->visit_logical_expr = visit_logical_expr;
    V->visit_ident_expr = visit_ident_expr;
    V->visit_chain_expr = visit_chain_expr;
    V->visit_unop_expr = visit_unop_expr;
    V->visit_binop_expr = visit_binop_expr;
    V->visit_cond_expr = visit_cond_expr;
    V->visit_call_expr = visit_call_expr;
    V->visit_index_expr = visit_index_expr;
    V->visit_selector_expr = visit_selector_expr;
    V->visit_item_expr = visit_item_expr;
    V->visit_type_name_expr = visit_type_name_expr;
    V->visit_signature_expr = visit_signature_expr;
    V->visit_block_stmt = visit_block_stmt;
    V->visit_expr_stmt = visit_expr_stmt;
    V->visit_decl_stmt = visit_decl_stmt;
    V->visit_if_stmt = visit_if_stmt;
    V->visit_for_stmt = visit_for_stmt;
    V->visit_while_stmt = visit_while_stmt;
    V->visit_dowhile_stmt = visit_dowhile_stmt;
    V->visit_return_stmt = visit_return_stmt;
    V->visit_var_decl = visit_var_decl;
    V->visit_func_decl = visit_func_decl;
    V->visit_struct_decl = visit_struct_decl;
    V->visit_type_decl = visit_type_decl;
}

static void resolve_module(AstVisitor *V)
{
    pawA_visit(V);
}

static void setup_expander(AstVisitor *V1, AstVisitor *V2, Checker *C)
{
    C->V1 = V1;
    const AstState state = {.C = C};
    pawA_visitor_init(V2, C->ast, state);
    V2->visit_func_decl = expand_func_decl;
    V2->visit_struct_decl = expand_struct_decl;
}

static void expand_templates(AstVisitor *V)
{
    Checker *C = V->state.C;
    do {
        C->nexpanded = 0;
        pawA_visit(V);
    } while (C->nexpanded > 0);
}

static void visit_module(Checker *C)
{
    Lex *lex = C->lex;
    SymbolTable *sym = C->sym;
    AstDecl *r = pawA_new_decl(C->ast, DECL_FUNC);
    enter_function(C, lex->modname, NULL, &r->func, FUNC_MODULE);

    AstVisitor V1;
    setup_resolver(&V1, C);
    resolve_module(&V1);

//    AstVisitor V2;
//    setup_expander(&V1, &V2, C);
//    expand_templates(&V2);

    sym->toplevel = leave_function(C);
    paw_assert(sym->nscopes == 0);
}

void p_check_types(Lex *lex)
{
    Checker C = {
        .lex = lex,
        .pm = lex->pm,
        .ast = lex->pm->ast,
        .sym = &lex->pm->symbols,
        .U = &lex->pm->unifier,
    };
    visit_module(&C);
}

