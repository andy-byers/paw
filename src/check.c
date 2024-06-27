// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// check.c: Implementation of the type checker. This code transforms an AST
// from the parser into a graph by unifying types based on lexical scope.

#include "check.h"
#include "ast.h"
#include "code.h"
#include "debug.h"
#include "env.h"
#include "gc_aux.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "str.h"
#include "type.h"
#include "unify.h"
#include "vector.h"

// Helper macros
#define syntax_error(R, ...) pawX_error((R)->lex, __VA_ARGS__)
#define type_error(R, ...) pawX_error((R)->lex, __VA_ARGS__)
#define resolve_pat(V, p) ((V)->visit_pat(V, p), a_type(p))
#define cached_str(R, i) pawE_cstr(env((R)->lex), cast_size(i))
#define basic_decl(R, code) basic_symbol(R, code)->decl
#define is_unit(e) (type2code(e) == PAW_TUNIT)
#define normalize(table, type) pawU_normalize(table, type)
#define flag2code(flag) (-(flag) - 1)

// Entrypoint for type unification
#define unify(R, a, b) pawU_unify((R)->U, a, b)

// Common state for type-checking routines
typedef struct Resolver {
    Lex *lex; // lexical state
    AstType *adt; // enclosing ADT
    AstType *return_; // enclosing function return type
    SymbolTable *sym; // scoped symbol table
    Ast *ast; // AST being checked
    ParseMemory *pm; // dynamic memory
    Unifier *U; // unification tables
    GenericState *gs; // info about current generic parameters
    struct MatchState *ms; // info about current match expression
    int func_depth; // number of nested functions
} Resolver;

static AstType *get_type(Resolver *R, DefId id)
{
    paw_assert(id < R->pm->decls.size);
    return a_type(R->pm->decls.data[id]);
}

static paw_Type type2code(AstType *type)
{
    if (a_is_basic(type)) {
        return type->hdr.def;
    } else if (a_is_adt(type)) {
        return type->adt.base;
    }
    return -1;
}

static AstType *resolve_expr(AstVisitor *V, AstExpr *e) 
{
    V->visit_expr(V, e);
    return a_type(e);
}

#define are_types_same(a, b) ((a) == (b))

static paw_Bool test_types(Resolver *R, const AstType *a, const AstType *b);

static paw_Bool test_binders(Resolver *R, const AstList *a, const AstList *b)
{
    for (int i = 0; i < a->count; ++i) {
        if (!test_types(R, a->data[i], b->data[i])) {
            return PAW_FALSE;
        }
    }
    return PAW_TRUE;
}

static paw_Bool test_types(Resolver *R, const AstType *a, const AstType *b)
{
    if (a_kind(a) != a_kind(b)) {
        return PAW_FALSE;
    }
    switch (a_kind(a)) {
        case AST_TYPE_FUNC:
            return test_binders(R, a->func.params, b->func.params);
        case AST_TYPE_ADT: {
            if (a->adt.base != b->adt.base) {
                return PAW_FALSE;
            }
            return test_binders(R, a->adt.types, b->adt.types);
        }
        default:
            return are_types_same(a, b);
    }
}

static Symbol *basic_symbol(Resolver *R, paw_Type code)
{
    paw_assert(code >= 0 && code <= PAW_TSTRING);

    // basic types have fixed locations
    Scope *toplevel = R->sym->scopes[0];
    return toplevel->symbols[1 + code]; // TODO
}

static void push_symbol_table(Resolver *R) 
{
    pawP_new_scope(R->lex, R->sym); 
}

static void pop_symbol_table(Resolver *R)
{
    // Last symbol table should have been assigned to an AST node. The
    // next call to push_symbol_table() will allocate a new table.
    SymbolTable *st = R->sym;
    paw_assert(st->nscopes > 0);
    --st->nscopes;
}

static DefId add_decl(Resolver *R, AstDecl *decl)
{
    return pawA_add_decl(R->ast, decl);
}

static AstDecl *get_decl(Resolver *R, DefId id)
{
    ParseMemory *pm = R->pm;
    paw_assert(id < pm->decls.size);
    return pm->decls.data[id];
}

static AstType *new_type(Resolver *R, DefId id, AstTypeKind kind)
{
    AstType *type = pawA_new_type(R->ast, kind);
    type->hdr.def = id;
    if (id != NO_DECL) {
        // set type of associated definition
        AstDecl *d = get_decl(R, id);
        d->hdr.type = type;
    }
    return type;
}

static AstType *decl_type_collector(AstVisitor *V, AstDecl *decl)
{
    paw_unused(V);
    return a_type(decl);
}

static AstType *expr_type_collector(AstVisitor *V, AstExpr *expr)
{
    return resolve_expr(V, expr);
}

static AstType *param_collector(AstVisitor *V, AstDecl *decl)
{
    FieldDecl *d = &decl->field;
    d->type = resolve_expr(V, d->tag);
    return d->type;
}

static AstType *param_collector2(AstVisitor *V, AstDecl *decl)
{
    FieldDecl *d = &decl->field;
    return resolve_expr(V, d->tag);
}

static AstType *generic_collector(AstVisitor *V, AstDecl *decl)
{
    GenericDecl *d = &decl->generic;
    DefId id = add_decl(V->state.R, decl);
    d->type = new_type(V->state.R, id, AST_TYPE_GENERIC);
    d->type->generic.name = d->name;
    return d->type;
}

#define make_collector(name, T, collect)                                       \
    static AstList *collect_##name(AstVisitor *V, AstList *list)               \
    {                                                                          \
        Resolver *R = V->state.R;                                              \
        AstList *binder = pawA_list_new(R->ast);                               \
        const int count = list ? list->count : 0;                              \
        for (int i = 0; i < count; ++i) {                                      \
            AstType *type = collect(V, list->data[i]);                         \
            pawA_list_push(R->ast, &binder, type);                             \
        }                                                                      \
        return binder;                                                         \
    }
make_collector(decl_types, AstDecl, decl_type_collector)
    make_collector(expr_types, AstExpr, expr_type_collector)
        make_collector(params, AstDecl, param_collector)
            make_collector(params2, AstDecl, param_collector2)
                make_collector(generics, AstDecl, generic_collector)

                    static void enter_inference_ctx(Resolver *R,
                                                    GenericState *gs,
                                                    UniTable *table)
{
    gs->outer = R->gs;
    pawU_enter_binder(R->U, table);
    R->gs = gs;
}

static UniTable *leave_inference_ctx(Resolver *R)
{
    R->gs = R->gs->outer;
    return pawU_leave_binder(R->U);
}

static Scope *enclosing_scope(Resolver *R)
{
    SymbolTable *st = R->sym;
    return st->scopes[st->nscopes - 1];
}

static Symbol *add_symbol(Resolver *R, Scope *scope, String *name,
                          AstDecl *decl)
{
    Symbol *symbol = pawP_add_symbol(R->lex, scope);
    symbol->name = name;
    symbol->decl = decl;
    return symbol;
}

static Symbol *add_local(Resolver *R, String *name, AstDecl *decl)
{
    return add_symbol(R, enclosing_scope(R), name, decl);
}

static Symbol *add_global(Resolver *R, String *name, AstDecl *decl)
{
    Scope *st = R->sym->globals;
    for (int i = 0; i < st->nsymbols; ++i) {
        Symbol *sym = st->symbols[i];
        if (pawS_eq(sym->name, name)) {
            syntax_error(R, "duplicate global '%s'", name->text);
        }
    }
    return add_symbol(R, st, name, decl);
}

static Symbol *try_resolve_symbol(Resolver *R, String *name)
{
    // search the scoped symbols
    SymbolTable *scopes = R->sym;
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
        return NULL;
    }
    return scopes->globals->symbols[index];
}

static Symbol *resolve_symbol(Resolver *R, String *name)
{
    Symbol *symbol = try_resolve_symbol(R, name);
    if (symbol == NULL) {
        syntax_error(R, "undefined symbol '%s'", name->text);
    }
    return symbol;
}

static AstDecl *resolve_attr(AstList *attrs, String *name)
{
    for (int i = 0; i < attrs->count; ++i) {
        AstDecl *decl = attrs->data[i];
        if (pawS_eq(name, decl->hdr.name)) {
            return decl;
        }
    }
    return NULL;
}

// Register the name and type of a variable
// If 'global' is true, then the variable is a global, otherwise, it is a local.
// Must be called prior to 'define_symbol',
static Symbol *declare_symbol(Resolver *R, String *name, AstDecl *decl,
                              paw_Bool global)
{
    return global ? add_global(R, name, decl) : add_local(R, name, decl);
}

// Allow a previously-declared variable to be accessed
static void define_symbol(Symbol *symbol) { symbol->is_init = PAW_TRUE; }

static Symbol *new_symbol(Resolver *R, String *name, AstDecl *decl,
                          paw_Bool global)
{
    Symbol *symbol = declare_symbol(R, name, decl, global);
    define_symbol(symbol);
    return symbol;
}

#define new_local(R, name, decl) new_symbol(R, name, decl, PAW_FALSE)
#define new_global(R, name, decl) new_symbol(R, name, decl, PAW_FALSE)

static Scope *leave_block(Resolver *R)
{
    Scope *scope = enclosing_scope(R);
    pop_symbol_table(R);
    return scope;
}

static void enter_block(Resolver *R, Scope *scope)
{
    if (scope == NULL) {
        push_symbol_table(R);
    } else {
        // use an existing scope
        pawP_add_scope(R->lex, R->sym, scope);
    }
}

static Scope *leave_function(Resolver *R)
{
    Scope *scope = leave_block(R);
    check_gc(env(R->lex));
    --R->func_depth;
    return scope;
}

static void enter_function(Resolver *R, Scope *scope, FuncDecl *func)
{
    ++R->func_depth;
    enter_block(R, scope);
    new_local(R, func->name, cast_decl(func));
}

static void new_local_literal(Resolver *R, const char *name, paw_Type code)
{
    Symbol *symbol = basic_symbol(R, code);
    new_local(R, scan_string(R->lex, name), symbol->decl);
}

static void visit_block_stmt(AstVisitor *V, Block *block)
{
    enter_block(V->state.R, block->scope);
    V->visit_stmt_list(V, block->stmts, V->visit_stmt);
    block->scope = leave_block(V->state.R);
}

static AstList *register_generics(AstVisitor *V, AstList *generics)
{
    Resolver *R = V->state.R;
    AstList *types = collect_generics(V, generics);
    for (int i = 0; i < types->count; ++i) {
        const AstType *type = types->data[i];
        AstDecl *decl = generics->data[i];
        Symbol *symbol = new_symbol(R, type->generic.name, decl, PAW_FALSE);
        symbol->is_type = PAW_TRUE;
        symbol->is_generic = PAW_TRUE;
    }
    return types;
}

typedef struct Subst {
    AstList *before;
    AstList *after;
    Resolver *R;
} Subst;

static AstList *prep_binder(AstTypeFolder *F, AstList *binder)
{
    Subst *subst = F->state;
    Resolver *R = subst->R;
    AstList *copy = pawA_list_new(R->ast);
    for (int i = 0; i < binder->count; ++i) {
        pawA_list_push(R->ast, &copy, binder->data[i]);
    }
    F->fold_binder(F, copy);
    return copy;
}

static AstType *prep_func(AstTypeFolder *F, AstFuncSig *t)
{
    Subst *subst = F->state;
    Resolver *R = subst->R;
    AstType *r = pawA_new_type(R->ast, AST_TYPE_FUNC);
    r->func.base = t->def;
    r->func.types = prep_binder(F, t->types);
    r->func.params = prep_binder(F, t->params);
    r->func.return_ = F->fold(F, t->return_);
    return r;
}

static AstType *prep_adt(AstTypeFolder *F, AstAdt *t)
{
    Subst *subst = F->state;
    Resolver *R = subst->R;
    AstType *r = pawA_new_type(R->ast, AST_TYPE_ADT);
    r->adt.base = t->base;
    r->adt.types = prep_binder(F, t->types);
    return r;
}

static AstType *maybe_subst(AstTypeFolder *F, AstType *t)
{
    Subst *s = F->state;
    for (int i = 0; i < s->before->count; ++i) {
        if (are_types_same(t, s->before->data[i])) {
            return s->after->data[i];
        }
    }
    return t;
}

static AstType *prep_generic(AstTypeFolder *F, AstGeneric *t)
{
    return maybe_subst(F, a_cast_type(t));
}

static AstType *prep_unknown(AstTypeFolder *F, AstUnknown *t)
{
    return maybe_subst(F, a_cast_type(t));
}

// Make a copy of a function template's parameter list, with bound (by the
// template) type variables replaced with inference variables. The types
// returned by this function can be unified with the type of each argument
// passed at the call site to determine a concrete type for each type variable.
static AstList *prep_inference(AstVisitor *V, AstList *generics, AstList *after,
                               AstList *params)
{
    Resolver *R = V->state.R;
    AstList *before = collect_decl_types(V, generics);
    AstList *target = collect_decl_types(V, params);

    Subst subst = {
        .before = before,
        .after = after,
        .R = R,
    };
    AstTypeFolder F;
    pawA_type_folder_init(&F, &subst);
    F.fold_adt = prep_adt;
    F.fold_func = prep_func;
    F.fold_generic = prep_generic;
    F.fold_unknown = prep_unknown;
    F.fold_binder(&F, target);
    return target;
}

static AstType *register_decl_type(AstVisitor *V, AstDecl *decl,
                                   AstTypeKind kind)
{
    Resolver *R = V->state.R;
    DefId id = add_decl(V->state.R, decl);
    AstType *r = new_type(R, id, kind);
    decl->hdr.type = r;
    return r;
}

static void register_concrete_types(AstVisitor *V, AstList *generics,
                                    AstList *types)
{
    Resolver *R = V->state.R;
    paw_assert(generics->count == types->count);
    for (int i = 0; i < types->count; ++i) {
        AstDecl *decl = generics->data[i];
        GenericDecl *generic = &decl->generic;
        generic->type = types->data[i];
        Symbol *symbol = new_symbol(R, generic->name, decl, PAW_FALSE);
        symbol->is_type = PAW_TRUE;
        symbol->is_generic = PAW_FALSE;
    }
}

static void register_base_func(AstVisitor *V, FuncDecl *d)
{
    Resolver *R = V->state.R;
    enter_block(R, NULL);

    AstType *r = register_decl_type(V, cast_decl(d), AST_TYPE_FUNC);
    r->func.types = register_generics(V, d->generics);
    r->func.params = collect_params(V, d->params);
    r->func.return_ = resolve_expr(V, d->return_);
    r->func.base = d->def;
    d->type = r;

    d->scope = leave_block(R);
}

static AstList *transfer_fields(AstVisitor *V, AstList *list,
                                AstDeclPass callback)
{
    AstList *copy = pawA_list_new(V->ast);
    for (int i = 0; i < list->count; ++i) {
        AstDecl *source = list->data[i];
        AstDecl *target = pawA_copy_decl(V->ast, source);
        callback(V, target); // determine types
        pawA_list_push(V->ast, &copy, target);
    }
    return copy;
}

static void decl_callback(AstVisitor *V, AstDecl *decl)
{
    add_decl(V->state.R, decl);
}

static void register_func_instance(AstVisitor *V, FuncDecl *base,
                                   InstanceDecl *inst, AstList *types)
{
    Resolver *R = V->state.R;
    enter_block(R, NULL);

    inst->name = base->name;
    inst->types = transfer_fields(V, base->generics, decl_callback);
    register_concrete_types(V, inst->types, types);

    AstType *r = register_decl_type(V, cast_decl(inst), AST_TYPE_FUNC);
    r->func.base = base->def;
    r->func.params = collect_params2(V, base->params);
    r->func.return_ = resolve_expr(V, base->return_);
    r->func.types = types;
    inst->type = r;

    inst->scope = leave_block(R);
}

static Scope *collect_fields(AstVisitor *V, AstList *list)
{
    Resolver *R = V->state.R;
    enter_block(R, NULL);

    for (int i = 0; i < list->count; ++i) {
        AstDecl *decl = list->data[i];
        V->visit_decl(V, decl);
        new_local(R, NULL, decl);
    }
    return leave_block(R);
}

// TODO: Use this
static void visit_variant_decl(AstVisitor *V, VariantDecl *d)
{
    Resolver *R = V->state.R;
    add_decl(V->state.R, cast_decl(d));
 
     // An enum variant name can be thought of as a function from the type of the variant's
     // fields to the type of the enumeration. For example, given 'enum E {X(string)}',
     // E::X has type 'fn(string) -> E'.
     d->type = new_type(R, d->def, AST_TYPE_FUNC);
     d->type->func.base = R->adt->adt.def;
     d->type->func.types = pawA_list_new(V->ast);
     d->type->func.params = collect_params(V, d->fields);
     d->type->func.return_ = R->adt;

     new_local(R, d->name, cast_decl(d));
     d->scope = collect_fields(V, d->fields);
}

static void register_field_decl(AstVisitor *V, AstDecl *decl)
{
    Resolver *R = V->state.R;
    add_decl(V->state.R, decl);
    if (a_kind(decl) == DECL_VARIANT) {
        VariantDecl *d = &decl->variant;
        d->scope = collect_fields(V, d->fields);
        d->type = new_type(R, d->def, AST_TYPE_FUNC);
        d->type->func.base = R->adt->adt.def;
        d->type->func.types = pawA_list_new(V->ast);
        d->type->func.params = collect_params(V, d->fields);
        d->type->func.return_ = R->adt;
    } else {
        paw_assert(a_kind(decl) == DECL_FIELD);
        FieldDecl *d = &decl->field;
        d->type = resolve_expr(V, d->tag);
    }
    new_local(R, decl->hdr.name, decl);
}

static void register_base_struct(AstVisitor *V, StructDecl *d)
{
    Resolver *R = V->state.R;
    enter_block(R, NULL);

    AstType *r = register_decl_type(V, cast_decl(d), AST_TYPE_ADT);
    r->adt.types = register_generics(V, d->generics);
    r->adt.base = d->def;
    d->type = r;

    AstType *enclosing = R->adt;
    R->adt = r;

    enter_block(R, NULL);
    V->visit_decl_list(V, d->fields, register_field_decl);
    d->field_scope = leave_block(R);

    d->scope = leave_block(R);
    R->adt = enclosing;
}

static void register_struct_instance(AstVisitor *V, StructDecl *base,
                                     InstanceDecl *inst, AstList *types)
{
    Resolver *R = V->state.R;
    enter_block(R, NULL);

    inst->name = base->name;
    inst->types = transfer_fields(V, base->generics, decl_callback);
    register_concrete_types(V, inst->types, types);

    AstType *r = register_decl_type(V, cast_decl(inst), AST_TYPE_ADT);
    r->adt.base = base->def;
    r->adt.types = types;
    inst->type = r;

    AstType *enclosing = R->adt;
    R->adt = inst->type;

    enter_block(R, NULL);
    inst->fields = transfer_fields(V, base->fields, register_field_decl);
    inst->field_scope = leave_block(R);

    inst->scope = leave_block(R);
    R->adt = enclosing;
}

static AstDecl *find_func_instance(AstVisitor *V, FuncDecl *base,
                                   AstList *types)
{
    Resolver *R = V->state.R;
    for (int i = 0; i < base->monos->count; ++i) {
        AstDecl *inst = base->monos->data[i];
        const AstType *type = get_type(R, inst->hdr.def);
        if (test_binders(R, types, type->func.types)) {
            return inst;
        }
        inst = inst->hdr.next;
    }
    return NULL;
}

static AstDecl *find_struct_instance(AstVisitor *V, StructDecl *base,
                                     AstList *types)
{
    Resolver *R = V->state.R;
    for (int i = 0; i < base->monos->count; ++i) {
        AstDecl *inst = base->monos->data[i];
        const AstType *type = get_type(R, inst->hdr.def);
        if (test_binders(R, types, type->func.types)) {
            return inst;
        }
        inst = inst->hdr.next;
    }
    return NULL;
}

static void visit_param_decl(AstVisitor *V, AstDecl *decl)
{
    FieldDecl *d = &decl->field;
    d->type = resolve_expr(V, d->tag);

    new_local(V->state.R, d->name, decl);
    add_decl(V->state.R, decl);
}

static void visit_func(AstVisitor *V, FuncDecl *d, FuncKind kind)
{
    Resolver *R = V->state.R;
    AstType *type = get_type(R, d->def);
    d->fn_kind = kind;

    enter_function(R, d->scope, d);
    V->visit_decl_list(V, d->params, visit_param_decl);

    AstType *outer = R->return_;
    R->return_ = type->func.return_;

    V->visit_block_stmt(V, d->body);
    d->scope = leave_function(R);
    R->return_ = outer;
}

static void visit_return_stmt(AstVisitor *V, ReturnStmt *s)
{
    Resolver *R = V->state.R;
    AstType *want = R->return_; // function return type
    AstType *have = s->expr ? resolve_expr(V, s->expr) : NULL;

    if (a_is_unit(want)) {
        if (have != NULL && !a_is_unit(have)) {
            type_error(R, "expected '()' or empty return");
        }
    } else if (have != NULL) {
        unify(R, have, want);
    } else {
        type_error(R, "expected nonempty return");
    }
}

static AstDecl *instantiate_func(AstVisitor *V, FuncDecl *base, AstList *types)
{
    AstDecl *inst = find_func_instance(V, base, types);
    if (inst == NULL) {
        inst = pawA_new_decl(V->ast, DECL_INSTANCE);
        inst->hdr.line = base->line;
        pawA_list_push(V->ast, &base->monos, inst);

        register_func_instance(V, base, &inst->inst, types);
    }
    return inst;
}

static AstDecl *instantiate_struct(AstVisitor *V, StructDecl *base,
                                   AstList *types)
{
    AstDecl *inst = find_struct_instance(V, base, types);
    if (inst == NULL) {
        inst = pawA_new_decl(V->ast, DECL_INSTANCE);
        inst->hdr.line = base->line;
        pawA_list_push(V->ast, &base->monos, inst);

        register_struct_instance(V, base, &inst->inst, types);
    }
    return inst;
}

static void check_template_param(Resolver *R, AstList *params, AstList *args)
{
    if (args->count > params->count) {
        syntax_error(R, "too many generics");
    } else if (args->count < params->count) {
        syntax_error(R, "not enough generics");
    }
}

static AstType *init_struct_template(AstVisitor *V, StructDecl *base,
                                     AstList *types)
{
    GenericState gs;
    Resolver *R = V->state.R;
    enter_inference_ctx(R, &gs, NULL);

    check_template_param(R, base->generics, types);
    AstDecl *inst = instantiate_struct(V, base, types);

    leave_inference_ctx(R);
    return a_type(inst);
}

static void expect_bool_expr(AstVisitor *V, AstExpr *e)
{
    Resolver *R = V->state.R;
    AstType *type = resolve_expr(V, e);
    unify(R, type, get_type(R, PAW_TBOOL));
}

static void expect_int_expr(AstVisitor *V, AstExpr *e)
{
    Resolver *R = V->state.R;
    AstType *type = resolve_expr(V, e);
    unify(R, type, get_type(R, PAW_TINT));
}

static AstType *instantiate(AstVisitor *V, AstDecl *base, AstList *types)
{
    if (a_is_struct_template_decl(base)) {
        StructDecl *d = &base->struct_;
        types = collect_expr_types(V, types);
        return init_struct_template(V, d, types);
    }
    return a_type(base);
}

static void visit_type_name_expr(AstVisitor *V, TypeName *e)
{
    Resolver *R = V->state.R;
    Symbol *symbol = resolve_symbol(R, e->name);
    AstDecl *decl = symbol->decl;
    if (a_kind(decl) == DECL_VAR) {
        type_error(R, "'%s' is not a type", symbol->name->text);
    }
   // else if (a_is_struct_template_decl(decl)) {
   //     StructDecl *base = &decl->struct_;
   //     AstList *types = collect_expr_types(V, e->args);
   //     e->type = init_struct_template(V, base, types);
   // } else {
   //     e->type = a_type(decl);
   // }
    e->type = instantiate(V, decl, e->args);
}

static void visit_match_expr(AstVisitor *V, MatchExpr *e)
{
    Resolver *R = V->state.R;
    AstType *target = resolve_expr(V, e->target);

    struct MatchState ms = {
        .outer = R->ms,
        .target = target,
        .match = e,
    };
    R->ms = &ms; // for unifying match arm types
    V->visit_expr_list(V, e->arms, V->visit_expr);
    R->ms = ms.outer;

    e->type = ms.value == NULL
        ? get_type(R, PAW_TUNIT)
        : ms.value;
}

static void visit_arm_expr(AstVisitor *V, MatchArm *e)
{
    Resolver *R = V->state.R;
    struct MatchState *ms = R->ms;
    enter_block(R, NULL);

    AstType *guard = resolve_pat(V, e->guard);
    if (e->cond != NULL) {
        expect_bool_expr(V, e->cond);
    }
    AstType *value = resolve_expr(V, e->value);

    unify(R, ms->target, guard);
    if (ms->value != NULL) {
        unify(R, ms->value, value);
    } else {
        ms->value = value;
    }
    e->scope = leave_block(R);
    e->type = value;
}

static void visit_ident_expr(AstVisitor *V, AstIdent *e)
{
    Symbol *symbol = resolve_symbol(V->state.R, e->name);
    e->type = get_type(V->state.R, symbol->decl->hdr.def);
}

static void visit_logical_expr(AstVisitor *V, LogicalExpr *e)
{
    expect_bool_expr(V, e->lhs);
    expect_bool_expr(V, e->rhs);
    e->type = get_type(V->state.R, PAW_TBOOL);
}

static void visit_chain_expr(AstVisitor *V, ChainExpr *e)
{
    Resolver *R = V->state.R;
    e->type = resolve_expr(V, e->target);
    if (a_is_adt(e->type)) {
        paw_assert(0); // TODO
//        if (e->type->adt.base == R->option_code || // return if None
//            e->type->adt.base == R->result_code) { // return if Err(E)
//            return;
//        }
    }
    type_error(R, "'?' operator requires an 'Option[T]' or 'Result[T, E]'");
}

static AstType *get_value_type(AstType *target)
{
    if (a_is_adt(target)) {
        if (target->adt.base == PAW_TVECTOR) {
            return target->adt.types->data[0];
        } else if (target->adt.base == PAW_TMAP) {
            return target->adt.types->data[1];
        }
    } 
    return NULL;
}

static AstType *visit_in_expr(Resolver *R, AstType *elem, AstType *adt)
{
    AstType *type = get_value_type(adt);
    if (type == NULL) {
        type_error(R, "expected Vector or Map");
    }
    unify(R, elem, type);
    return get_type(R, PAW_TBOOL);
}

static void visit_unop_expr(AstVisitor *V, UnOpExpr *e)
{
    // clang-format off
    static const uint8_t kValidOps[NUNARYOPS][PAW_NTYPES] = {
        //     type  =  0, b, i, f, s, v, m, ...
        [UNARY_LEN]  = {0, 0, 0, 0, 1, 1, 1},
        [UNARY_NEG]  = {0, 0, 1, 1, 0, 0, 0}, 
        [UNARY_NOT]  = {0, 1, 1, 1, 0, 0, 0}, 
        [UNARY_BNOT] = {0, 0, 1, 0, 0, 0, 0}, 
    };
    // clang-format on

    Resolver *R = V->state.R;
    AstType *type = resolve_expr(V, e->target);
    const paw_Type code = type2code(type);
    if (!kValidOps[e->op][code]) {
        type_error(R, "unsupported operand type for unary operator");
    } else if (unop_is_bool(e->op)) {
        e->type = get_type(R, PAW_TBOOL);
    } else if (e->op == UNARY_LEN) {
        e->type = get_type(R, PAW_TINT);
    } else {
        e->type = type;
    }
}

static void visit_binop_expr(AstVisitor *V, BinOpExpr *e)
{
    // clang-format off
    static const uint8_t kValidOps[NBINARYOPS][PAW_NTYPES] = {
        //     type   =  0, b, i, f, s, v, m, ...
        [BINARY_EQ]   = {0, 1, 1, 1, 1, 1, 1},
        [BINARY_NE]   = {0, 1, 1, 1, 1, 1, 1},
        [BINARY_LT]   = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_LE]   = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_GT]   = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_GE]   = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_ADD]  = {0, 1, 1, 1, 1, 1, 0},
        [BINARY_SUB]  = {0, 1, 1, 1, 0, 0, 0},
        [BINARY_MUL]  = {0, 1, 1, 1, 0, 0, 0},
        [BINARY_DIV]  = {0, 1, 1, 1, 0, 0, 0},
        [BINARY_MOD]  = {0, 1, 1, 1, 0, 0, 0},
        [BINARY_BXOR] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_BAND] = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_BOR]  = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_SHL]  = {0, 0, 1, 0, 0, 0, 0},
        [BINARY_SHR]  = {0, 0, 1, 0, 0, 0, 0},
    };
    // clang-format on

    Resolver *R = V->state.R;
    AstType *lhs = resolve_expr(V, e->lhs);
    AstType *rhs = resolve_expr(V, e->rhs);
    if (e->op == BINARY_IN) {
        e->type = visit_in_expr(R, lhs, rhs);
        return;
    }
    unify(R, lhs, rhs);

    const paw_Type left = type2code(lhs);
    const paw_Type right = type2code(rhs);
    if (left < 0 || right < 0 || left != right || !kValidOps[e->op][left]) {
        type_error(R, "unsupported operand types for binary operator");
    } else if (binop_is_bool(e->op)) {
        e->type = get_type(R, PAW_TBOOL);
    } else {
        e->type = lhs;
    }
}

static void visit_signature_expr(AstVisitor *V, FuncType *e)
{
    Resolver *R = V->state.R;
    e->type = new_type(R, NO_DECL, AST_TYPE_FUNC);
    e->type->func.base = NO_DECL;
    e->type->func.types = pawA_list_new(V->ast);
    e->type->func.params = collect_expr_types(V, e->params);
    e->type->func.return_ = resolve_expr(V, e->return_);
}

static void visit_struct_decl(AstVisitor *V, StructDecl *d)
{
    Resolver *R = V->state.R;
    Symbol *symbol = new_symbol(R, d->name, cast_decl(d), d->is_global);
    symbol->is_type = PAW_TRUE;
    register_base_struct(V, d);
}

static void visit_var_decl(AstVisitor *V, VarDecl *d)
{
    Resolver *R = V->state.R;
    Symbol *symbol = declare_symbol(R, d->name, cast_decl(d), d->is_global);
    AstType *init = resolve_expr(V, d->init);
    define_symbol(symbol);

    if (d->tag != NULL) {
        AstType *tag = resolve_expr(V, d->tag);
        unify(R, init, tag);
    }
    add_decl(R, cast_decl(d));
    d->type = init;
}

static void visit_type_decl(AstVisitor *V, TypeDecl *d)
{
    // TODO: generic parameters for aliases
    Symbol *symbol = declare_symbol(V->state.R, d->name, cast_decl(d), 
                                    PAW_FALSE);
    d->type = resolve_expr(V, d->rhs);
    // unify(R, d->name, d->type);
    define_symbol(symbol);
}

static AstList *add_unknowns(AstVisitor *V, AstList *generics)
{
    Resolver *R = V->state.R;
    AstList *binder = pawA_list_new(R->ast);
    for (int i = 0; i < generics->count; ++i) {
        AstDecl *decl = generics->data[i];
        AstType *unknown = pawU_new_unknown(R->U, decl->hdr.def);
        pawA_list_push(V->ast, &binder, unknown);
    }
    return binder;
}

static AstList *infer_template_param(AstVisitor *V, AstList *generics,
                                     AstList *params, AstList *args)
{
    GenericState gs;
    Resolver *R = V->state.R;
    enter_inference_ctx(R, &gs, NULL);

    AstList *unknowns = add_unknowns(V, generics);
    AstList *replaced = prep_inference(V, generics, unknowns, params);

    // Attempt to determine a type for each generic parameter, using the
    // combination of function parameters and arguments. Any parameter type
    // might equal, or recursively contain, one of the generic type parameters.
    paw_assert(args->count == replaced->count);
    for (int i = 0; i < replaced->count; ++i) {
        AstExpr *arg = args->data[i];
        AstType *a = replaced->data[i];
        AstType *b = resolve_expr(V, arg);
        unify(R, a, b);
    }
    UniTable *table = leave_inference_ctx(R);
    for (int i = 0; i < generics->count; ++i) {
        AstDecl *generic = generics->data[i];
        AstType *type = normalize(table, unknowns->data[i]);
        if (a_is_unknown(type)) {
            const String *name = generic->generic.name;
            type_error(R, "unable to infer generic parameter '%s'", name->text);
        }
        unknowns->data[i] = type;
    }
    return unknowns;
}

static AstType *init_func_template(AstVisitor *V, FuncDecl *base,
                                   AstList *types)
{
    Resolver *R = V->state.R;
    check_template_param(R, base->generics, types);
    AstDecl *inst = instantiate_func(V, base, types);
    return a_type(inst);
}

static AstType *infer_func_template(AstVisitor *V, FuncDecl *base,
                                    AstList *args)
{
    AstList *types =
        infer_template_param(V, base->generics, base->params, args);
    AstDecl *inst = instantiate_func(V, base, types);
    return a_type(inst);
}

static AstType *setup_call(AstVisitor *V, CallExpr *e)
{
    Resolver *R = V->state.R;
    V->visit_expr(V, e->target);
    AstType *target = a_type(e->target);
    if (!a_is_func(target)) {
        type_error(R, "type is not callable");
    }
    AstFuncSig *func = &target->func;
    if (e->args->count < func->params->count) {
        syntax_error(R, "not enough arguments");
    } else if (e->args->count > func->params->count) {
        syntax_error(R, "too many arguments");
    }
    if (func->def != NO_DECL) {
        // Function type has an associated declaration. If that declaration is
        // for a function template, attempt to infer the type parameters.
        AstDecl *decl = get_decl(V->state.R, func->def);
        if (a_is_func_template_decl(decl)) {
            return infer_func_template(V, &decl->func, e->args);
        }
    }
    return target;
}

static void visit_call_expr(AstVisitor *V, CallExpr *e)
{
    Resolver *R = V->state.R;
    // Determine the type of the callable, then find its declaration. Template
    // functions will need type inference, which is handled in setup_call().
    e->func = setup_call(V, e);

    if (a_kind(e->func) != AST_TYPE_FUNC) {
        type_error(R, "type is not callable");
    }
    const AstList *params = params = e->func->func.params;
    e->type = e->func->func.return_;
    
    if (params->count != e->args->count) {
        syntax_error(R, "expected %d arguments(s) but found %d",
                     params->count, e->args->count);
    }
    for (int i = 0; i < params->count; ++i) {
        AstExpr *arg = e->args->data[i];
        AstType *type = resolve_expr(V, arg);
        unify(R, params->data[i], type);
        arg = arg->hdr.next;
    }
}

struct StructPack {
    String *name;
    AstList *fields;
    paw_Bool is_struct;
};

static struct StructPack unpack_struct(Resolver *R, AstType *type)
{
    if (!a_is_func(type) && !a_is_adt(type)) {
        type_error(R, "expected structure or enumerator");
    }
    AstDecl *decl = get_decl(R, type->hdr.def);
    if (a_kind(decl) == DECL_INSTANCE) {
        AstDecl *base = get_decl(R, type->adt.base);
        return (struct StructPack){
            .is_struct = base->struct_.is_struct,
            .name = base->struct_.name,
            .fields = decl->inst.fields,
        };
    } else if (a_kind(decl) == DECL_VARIANT) {
        return (struct StructPack){
            .name = decl->variant.name,
            .fields = decl->variant.fields,
        };
    }

    if (a_kind(decl) != DECL_STRUCT) {
        type_error(R, "expected structure");
    }
    return (struct StructPack){
        .is_struct = decl->struct_.is_struct,
        .name = decl->struct_.name,
        .fields = decl->struct_.fields,
    };
}

static AstType *visit_vector_lit(AstVisitor *V, CompositeLit *e,
                                 AstType *target)
{
     Resolver *R = V->state.R;
     paw_assert(target->adt.types->count == 1);
     AstType *value_t = target->adt.types->data[0];
    
     for (int i = 0; i < e->items->count; ++i) {
         AstExpr *expr = e->items->data[i];
         ItemExpr *item = &expr->item;
         if (item->key != NULL) {
            syntax_error(R, "unexpected key in vector literal");
         }
         AstType *type = resolve_expr(V, item->value);
         unify(R, type, value_t);
     }
     return target;
}

static AstType *visit_map_lit(AstVisitor *V, CompositeLit *e, AstType *target)
{
    Resolver *R = V->state.R;
    paw_assert(target->adt.types->count == 2);
    AstType *key_t = target->adt.types->data[0];
    AstType *value_t = target->adt.types->data[1];

     for (int i = 0; i < e->items->count; ++i) {
         AstExpr *expr = e->items->data[i];
         ItemExpr *item = &expr->item;
         if (item->key == NULL) {
            syntax_error(R, "expected key in map literal");
         }
         AstType *type = resolve_expr(V, item->key);
         unify(R, type, key_t);

         type = resolve_expr(V, item->value);
         unify(R, type, value_t);
     }
    return target;
}

static String *resolve_struct_key(AstVisitor *V, const struct StructPack *pack,
                                  ItemExpr *item, int index)
{
    item->type = resolve_expr(V, item->value);

    if (item->key == NULL) {
        AstDecl *field = pack->fields->data[index];
        return field->field.name;
    } else if (a_kind(item->key) != EXPR_NAME) {
        syntax_error(V->state.R, "expected field identifier");
    }
    return item->key->name.name;
}

// TODO: scratch allocations need to be boxed
//       could allow unnamed fields for other classes if they are in the correct
//       order already
static AstType *visit_composite_lit(AstVisitor *V, LiteralExpr *lit)
{
    CompositeLit *e = &lit->comp;
    Resolver *R = V->state.R;
    Lex *lex = R->lex;

    // Replace the AstIdent or IndexExpr with the TypeName of the structure.
    V->visit_expr(V, e->target);
    AstType *target = a_type(e->target);
    if (!a_is_adt(target)) {
        type_error(R, "expected structure type");
    } else if (target->adt.base == PAW_TVECTOR) {
        return visit_vector_lit(V, e, target);
    } else if (target->adt.base == PAW_TMAP) {
        return visit_map_lit(V, e, target);
    }
    // Use a temporary Map to avoid searching repeatedly through the list of fields.
    paw_Env *P = env(lex);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    v_set_object(pv, map);

    Value key;
    const struct StructPack pack = unpack_struct(R, target);
    AstExpr **order = pawM_new_vec(P, e->items->count, AstExpr *);
    for (int i = 0; i < e->items->count; ++i) {
        AstExpr *item = e->items->data[i];
        String *k = resolve_struct_key(V, &pack, &item->item, i);
        v_set_object(&key, k);
        if (pawH_contains(P, map, key)) {
            syntax_error(R, "duplicate attribute '%s' in struct '%s'", k->text,
                         pack.name->text);
        }
        Value *value = pawH_action(P, map, key, MAP_ACTION_CREATE);
        v_set_int(value, i);
        order[i] = item;
    }
    for (int i = 0; i < pack.fields->count; ++i) {
        AstDecl *decl = pack.fields->data[i];
        FieldDecl *field = &decl->field;
        v_set_object(&key, field->name);
        Value *value = pawH_get(P, map, key);
        if (value == NULL) {
            syntax_error(R, "missing initializer for field '%s' in struct '%s'",
                         field->name->text, pack.name->text);
        } else {
            const paw_Int index = v_int(*value);
            ItemExpr *ie = &order[index]->item;
            ie->index = i; // index of attribute in struct
            unify(R, ie->type, get_type(R, field->def));
        }
        pawH_remove(P, map, key);
    }
    if (pawH_length(map) > 0) {
        syntax_error(R, "too many initializers for struct '%s'",
                     pack.name->text);
    }
    paw_assert(pack.fields->count == e->items->count);

    pawC_pop(P); // pop map
    pawM_free_vec(P, order, e->items->count);
    return target;
}

static void visit_literal_expr(AstVisitor *V, LiteralExpr *e)
{
    if (e->lit_kind == LIT_BASIC) {
        e->type = get_type(V->state.R, e->basic.t);
    } else {
        paw_assert(e->lit_kind == LIT_COMPOSITE);
        e->type = visit_composite_lit(V, e);
    }
}

static void visit_func_decl(AstVisitor *V, FuncDecl *d)
{
    Symbol *symbol = declare_symbol(V->state.R, d->name, cast_decl(d), d->is_global);
    symbol->is_type = d->generics->count > 0;
    register_base_func(V, d);
    visit_func(V, d, FUNC_FUNCTION);
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
    AstType *lhs = resolve_expr(V, s->lhs);
    if (s->rhs != NULL) {
        AstType *rhs = resolve_expr(V, s->rhs);
        unify(V->state.R, lhs, rhs);
    }
}

static void visit_while_stmt(AstVisitor *V, WhileStmt *s)
{
    enter_block(V->state.R, NULL);
    expect_bool_expr(V, s->cond);
    V->visit_block_stmt(V, s->block);
    s->scope = leave_block(V->state.R);
}

static void visit_dowhile_stmt(AstVisitor *V, WhileStmt *s)
{
    enter_block(V->state.R, NULL);
    V->visit_block_stmt(V, s->block);
    expect_bool_expr(V, s->cond);
    s->scope = leave_block(V->state.R);
}

static void visit_for_body(AstVisitor *V, String *iname, Block *b)
{
    Resolver *R = V->state.R;
    enter_block(R, NULL);
    new_local(R, iname, basic_decl(R, PAW_TINT));
    V->visit_stmt_list(V, b->stmts, V->visit_stmt);
    b->scope = leave_block(R);
}

static void visit_fornum(AstVisitor *V, ForStmt *s)
{
    ForNum *fornum = &s->fornum;

    expect_int_expr(V, fornum->begin);
    expect_int_expr(V, fornum->end);
    expect_int_expr(V, fornum->step);

    new_local_literal(V->state.R, "(for begin)", PAW_TINT);
    new_local_literal(V->state.R, "(for end)", PAW_TINT);
    new_local_literal(V->state.R, "(for step)", PAW_TINT);

    visit_for_body(V, s->name, s->block);
}

static void visit_forin(
    AstVisitor *V,
    ForStmt *s) // TODO: forin would need to encode the type of object being
                // iterated over. look into function call for loop?
{
    //    Lex *lex = R->lex;
    //    ForIn *forin = &s->forin;
    //    new_local_literal(R, "(for target)", PAW_TINT);
    //    new_local_literal(R, "(for iterator)", PAW_TINT);
    //    V->visit_expr(V, forin->target);
    //
    //    AstType *inner = pawY_unwrap(env(lex), forin->target->type);
    //    new_local(R, s->name, inner);
    //
    //    V->visit_for_body(V, s->name, s->block);
}

static void visit_for_stmt(AstVisitor *V, ForStmt *s)
{
    enter_block(V->state.R, NULL);
    if (s->kind == STMT_FORNUM) {
        visit_fornum(V, s);
    } else {
        visit_forin(V, s);
    }
    s->scope = leave_block(V->state.R);
}

static AstType *explicit_func_template(AstVisitor *V, FuncDecl *base, Index *e)
{
    AstList *types = collect_expr_types(V, e->elems);
    return init_func_template(V, base, types);
}

static AstType *explicit_struct_template(AstVisitor *V, StructDecl *base,
                                         Index *e)
{
    AstList *types = collect_expr_types(V, e->elems);
    return init_struct_template(V, base, types);
}

static void visit_index_expr(AstVisitor *V, Index *e)
{
    paw_assert(e->elems->count >= 1); // ensured by grammer
                                     
    Resolver *R = V->state.R;
    AstType *target = resolve_expr(V, e->target);
    AstDecl *decl = get_decl(R, target->hdr.def);
    if (!a_is_template_decl(decl)) {
        if (e->elems->count != 1) {
            syntax_error(R, "too many indices (must be 1)");
        }
        AstType *expect = NULL;
        if (target->adt.base == PAW_TVECTOR) {
            expect = get_type(R, PAW_TINT);
            e->type = target->adt.types->data[0];
        } else if (target->adt.base == PAW_TMAP) {
            expect = target->adt.types->data[0];
            e->type = target->adt.types->data[1];
        } else {
            type_error(R, "value cannot be indexed (not a container)");
        }
        AstExpr *elem = e->elems->data[0];
        AstType *key_t = resolve_expr(V, elem);
        unify(R, expect, key_t);
    } else if (a_kind(decl) == DECL_STRUCT) {
        e->type = explicit_struct_template(V, &decl->struct_, e);
    } else {
        e->type = explicit_func_template(V, &decl->func, e);
    }
}

static AstDecl *expect_attr(Resolver *R, const struct StructPack *pack, String *name)
{
    AstDecl *attr = resolve_attr(pack->fields, name);
    if (attr == NULL) {
        syntax_error(R, "field '%s' does not exist in type '%s'",
                     name->text, pack->name->text);
    }
    return attr;
}

static void visit_access_expr(AstVisitor *V, Access *e)
{
    Resolver *R = V->state.R;
    AstType *type = resolve_expr(V, e->target);
    if (!a_is_adt(type)) {
        type_error(R, "expected ADT");
    }
    // TODO: This won't work properly for structs: need to access static fields, not instance fields
    const struct StructPack pack = unpack_struct(R, type);
    AstDecl *attr = expect_attr(R, &pack, e->name);
    e->type = get_type(R, attr->hdr.def);
}

static void visit_selector_expr(AstVisitor *V, Selector *e)
{
    Resolver *R = V->state.R;
    AstType *type = resolve_expr(V, e->target);
    if (!a_is_adt(type)) {
        type_error(R, "expected ADT");
    }
    const struct StructPack pack = unpack_struct(R, type);
    AstDecl *attr = expect_attr(R, &pack, e->name);
    e->type = get_type(R, attr->hdr.def);
}

static AstType *resolve_base(AstVisitor *V, AstPathSegment *base)
{
    Symbol *symbol = resolve_symbol(V->state.R, base->name);
    return instantiate(V, symbol->decl, base->types);
}

static AstType *next_segment(AstVisitor *V, AstType *base, AstPathSegment *next)
{
    Resolver *R = V->state.R;
    const struct StructPack pack = unpack_struct(R, base);
    AstDecl *attr = expect_attr(R, &pack, next->name);
    return instantiate(V, attr, next->types);
}

static AstType *resolve_path(AstVisitor *V, AstList *path)
{
    AstPathSegment *segment = pawA_path_get(path, 0);
    AstType *type = resolve_base(V, segment);
    for (int i = 1; i < path->count; ++i) {
        segment = pawA_path_get(path, i);
        type = next_segment(V, type, segment);
    }
    return type;
}

static void visit_decl_stmt(AstVisitor *V, AstDeclStmt *s)
{
    V->visit_decl(V, s->decl);
}

static void visit_literal_pat(AstVisitor *V, AstLiteralPat *p)
{
    V->visit_expr(V, p->expr);
}

static void visit_path_pat(AstVisitor *V, AstPathPat *p)
{
    p->type = resolve_path(V, p->path);
}

static void visit_tuple_pat(AstVisitor *V, AstTuplePat *p)
{
    paw_unused(V);
    paw_unused(p);
}

static void try_bind_var(Resolver *R, AstPat *pat, AstType *want)
{
    if (a_kind(pat) == AST_PAT_PATH && pat->path.path->count == 1) {
        AstPathSegment *segment = pat->path.path->data[0];
        if (segment->types->count == 0) {
            Symbol *symbol = try_resolve_symbol(R, segment->name);
            if (symbol == NULL || !symbol->is_type || a_is_func_decl(symbol->decl)) {
                // TODO: don't abuse 'is_type' flag for function templates, shouldn't need to check if the symbol 
                //       is a function or not here
                AstDecl *r = pawA_new_decl(R->ast, DECL_VAR);
                r->var.name = segment->name;
                new_symbol(R, segment->name, r, PAW_FALSE);
                add_decl(R, r);
            } else {
                AstType *have = a_type(symbol->decl);
                unify(R, have, want);            
            }
        }
    }
}

// let bb = 2
// let cc = true
// let v = Variant('a', b, cc)       const, unbound_var, bound_var
// let s = Struct{a: 'a', b: bb, c}  const, bound_var, unbound_var (shorthand)
static AstType *resolve_sfield_pat(AstVisitor *V, const struct StructPack *pack, AstFieldPat *p)
{
    Resolver *R = V->state.R;
    paw_assert(pack->is_struct);
    paw_assert(p->name != NULL);
    AstDecl *attr = resolve_attr(pack->fields, p->name);
    if (attr == NULL) {
        syntax_error(R, "field '%s' does not exist in type '%s'",
                     p->name->text, pack->name->text);
    }
    p->type = a_type(attr);
    try_bind_var(R, p->pat, p->type);
    return p->type;
}

static AstType *resolve_vfield_pat(AstVisitor *V, const struct StructPack *pack, AstFieldPat *p, int index)
{
    Resolver *R = V->state.R;
    paw_assert(!pack->is_struct);
    paw_assert(p->name == NULL);
    AstDecl *decl = pack->fields->data[index];
    p->type = a_type(decl);
    try_bind_var(R, p->pat, p->type);
    return p->type;
}

static void visit_struct_pat(AstVisitor *V, AstStructPat *p)
{
    Resolver *R = V->state.R;
    p->type = resolve_path(V, p->path);

    const struct StructPack pack = unpack_struct(R, p->type);
    if (!pack.is_struct) {
        type_error(R, "expected struct '%s'", pack.name->text);
    } else if (pack.fields->count != p->fields->count) {
        syntax_error(R, "missing fields from struct pattern for '%s'", pack.name->text);
    }
    for (int i = 0; i < p->fields->count; ++i) {
        AstPat *pat = p->fields->data[i];
        AstFieldPat *field = &pat->field;
        field->type = resolve_sfield_pat(V, &pack, field);

        AstDecl *attr = resolve_attr(pack.fields, field->name);
        unify(R, a_type(attr), field->type);
    }
}

static void visit_variant_pat(AstVisitor *V, AstVariantPat *p)
{
    Resolver *R = V->state.R;
    AstType *target = resolve_path(V, p->path);
    paw_assert(a_is_func(target));
    p->type = target->func.return_;

    const struct StructPack pack = unpack_struct(R, target);
    if (pack.is_struct) {
        type_error(R, "expected variant '%s'", pack.name->text);
    } else if (pack.fields->count != p->elems->count) {
        syntax_error(R, "missing fields from variant pattern for '%s'", pack.name->text);
    }
    for (int i = 0; i < p->elems->count; ++i) {
        AstPat *pat = p->elems->data[i];
        AstFieldPat *field = &pat->field;
        field->type = resolve_vfield_pat(V, &pack, field, i);

        const AstDecl *expect = pack.fields->data[i];
        unify(R, a_type(expect), field->type);
    }
}

static void visit_prelude_func(AstVisitor *V, FuncDecl *d)
{
    Symbol *symbol =
        declare_symbol(V->state.R, d->name, cast_decl(d), PAW_TRUE);
    register_base_func(V, d);
    define_symbol(symbol);
}

static void visit_prelude_struct(AstVisitor *V, StructDecl *d)
{
    Resolver *R = V->state.R;
    new_symbol(R, d->name, cast_decl(d), PAW_TRUE);
    register_base_struct(V, d);
}

static void add_basic_builtin(Resolver *R, String *name)
{
    AstExpr *e = pawA_new_expr(R->ast, EXPR_TYPE_NAME);
    e->type_name.args = pawA_list_new(R->ast);
    e->type_name.name = name;

    AstDecl *d = pawA_new_decl(R->ast, DECL_TYPE);
    d->type.name = name;
    d->type.line = 0;
    d->type.rhs = e;

    add_decl(R, d);

    const paw_Type code = flag2code(name->flag);
    d->hdr.type = R->ast->builtin[code];
    Symbol *symbol = new_local(R, name, d);
    symbol->is_type = PAW_TRUE;
}

static void visit_prelude(AstVisitor *V, Resolver *R)
{
    const AstState state = {.R = R};
    pawA_visitor_init(V, R->ast, state);
    V->visit_func_decl = visit_prelude_func;
    V->visit_struct_decl = visit_prelude_struct;
    V->visit_type_name_expr = visit_type_name_expr;
    V->visit_signature_expr = visit_signature_expr;

    V->visit_stmt_list(V, R->ast->prelude, V->visit_stmt);
}

static void setup_module(AstVisitor *V, Resolver *R, AstDecl *r)
{
    add_basic_builtin(R, cached_str(R, CSTR_UNIT));
    add_basic_builtin(R, cached_str(R, CSTR_BOOL));
    add_basic_builtin(R, cached_str(R, CSTR_INT));
    add_basic_builtin(R, cached_str(R, CSTR_FLOAT));
    add_basic_builtin(R, cached_str(R, CSTR_STRING));

    R->U->depth = -1;

    r->func.type = pawA_new_type(R->ast, AST_TYPE_FUNC);
    r->func.type->func.types = pawA_list_new(R->ast);
    r->func.type->func.params = pawA_list_new(R->ast);
    r->func.type->func.return_ = get_type(R, PAW_TUNIT);
    r->func.params = pawA_list_new(R->ast);

    visit_prelude(V, R);

    const AstState state = {.R = R};
    pawA_visitor_init(V, R->ast, state);
    V->visit_literal_expr = visit_literal_expr;
    V->visit_logical_expr = visit_logical_expr;
    V->visit_ident_expr = visit_ident_expr;
    V->visit_chain_expr = visit_chain_expr;
    V->visit_unop_expr = visit_unop_expr;
    V->visit_binop_expr = visit_binop_expr;
    V->visit_call_expr = visit_call_expr;
    V->visit_index_expr = visit_index_expr;
    V->visit_access_expr = visit_access_expr;
    V->visit_selector_expr = visit_selector_expr;
    // V->visit_item_expr = visit_item_expr;
    V->visit_type_name_expr = visit_type_name_expr;
    V->visit_match_expr = visit_match_expr;
    V->visit_arm_expr = visit_arm_expr;
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
    V->visit_variant_decl = visit_variant_decl;
    V->visit_func_decl = visit_func_decl;
    V->visit_struct_decl = visit_struct_decl;
    V->visit_type_decl = visit_type_decl;
    V->visit_literal_pat = visit_literal_pat;
    V->visit_path_pat = visit_path_pat;
    V->visit_tuple_pat = visit_tuple_pat;
    V->visit_struct_pat = visit_struct_pat;
    V->visit_variant_pat = visit_variant_pat;
}

static void visit_module(Resolver *R)
{
    Lex *lex = R->lex;
    SymbolTable *sym = R->sym;
    AstDecl *r = pawA_new_decl(R->ast, DECL_FUNC);
    r->func.name = lex->modname;

    enter_function(R, NULL, &r->func);

    AstVisitor V;
    setup_module(&V, R, r);
    pawA_visit(&V);

    sym->toplevel = leave_function(R);
    paw_assert(sym->nscopes == 0);
}

void p_check_types(Lex *lex)
{
    Resolver R = {
        .lex = lex,
        .pm = lex->pm,
        .ast = lex->pm->ast,
        .sym = &lex->pm->symbols,
        .U = &lex->pm->unifier,
    };
    visit_module(&R);
}
