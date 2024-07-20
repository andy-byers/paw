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

// Helper macros
#define name_error(R, line, ...) pawE_error(env((R)->lex), PAW_ENAME, line, __VA_ARGS__)
#define syntax_error(R, ...) pawX_error((R)->lex, __VA_ARGS__)
#define type_error(R, ...) pawE_error(env((R)->lex), PAW_ETYPE, -1, __VA_ARGS__)
#define resolve_pat(V, p) ((V)->visit_pat(V, p), a_type(p))
#define cached_str(R, i) pawE_cstr(env((R)->lex), cast_size(i))
#define basic_decl(R, code) basic_symbol(R, code)->decl
#define is_unit(e) (type2code(e) == PAW_TUNIT)
//#define normalize(table, type) pawU_normalize(table, type)
#define flag2code(flag) (-(flag) - 1)

struct ContainerState {
    struct ContainerState *outer;
    struct Resolver *R;
    AstType *type;
};

// Common state for type-checking routines
typedef struct Resolver {
    Lex *lex; // lexical state
    AstType *adt; // enclosing ADT
    AstType *result; // enclosing function return type
    SymbolTable *sym; // scoped symbol table
    Ast *ast; // AST being checked
    ParseMemory *pm; // dynamic memory
    Unifier *U; // unification tables
    struct AstList *clist; // list of container type variables
    struct ContainerState *cs; // info about current container
    struct MatchState *ms; // info about current match expression
    int func_depth; // number of nested functions
    int option_did;
    int result_did;
    int cdepth;
    paw_Bool in_closure;
} Resolver;

static AstType *normalize(UnificationTable *table, AstType *type)
{
    return pawU_normalize(table, type);
}

static void unify(Resolver *R, AstType *a, AstType *b)
{
    pawU_unify(R->U, a, b);
    normalize(R->U->table, a);
    normalize(R->U->table, b);
}

//// Entrypoint for type unification
//#define unify(R, a, b) pawU_unify((R)->U, a, b)

static AstType *get_type(Resolver *R, DefId did)
{
    paw_assert(did < R->pm->decls.size);
    return a_type(R->pm->decls.data[did]);
}

static paw_Type type2code(AstType *type)
{
    return a_is_adt(type) ? type->adt.base : -1;
}

static AstDecl *get_decl(Resolver *R, DefId did)
{
    ParseMemory *pm = R->pm;
    paw_assert(did < pm->decls.size);
    return pm->decls.data[did];
}

static paw_Bool is_unit_variant(struct Resolver *R, const struct AstType *type)
{
    if (a_is_fdef(type)) {
        struct AstDecl *decl = get_decl(R, type->func.did);
        return a_kind(decl) == DECL_VARIANT && 
            decl->variant.fields->count == 0;
    }
    return PAW_FALSE;
}

static AstType *new_vector_t(Resolver *R, AstType *elem_t)
{
     AstType *type = pawA_new_type(R->ast, AST_TYPE_ADT);
     type->adt.base = PAW_TVECTOR;
     type->adt.did = NO_DECL; // TODO: Cannonicalize vector instantiations
     AstList *types = pawA_list_new(R->ast);
     pawA_list_push(R->ast, &types, elem_t);
     type->adt.types = types;
     return type;
}

static AstType *new_map_t(Resolver *R, AstType *key_t, AstType *value_t)
{
    if (!a_is_unknown(key_t) && !a_is_basic(key_t)) {
        type_error(R, "key is not hashable");
    }
    struct AstType *type = pawA_new_type(R->ast, AST_TYPE_ADT);
    type->adt.base = PAW_TMAP;
    type->adt.did = NO_DECL; // TODO: Cannonicalize map instantiations
    struct AstList *types = pawA_list_new(R->ast);
    pawA_list_push(R->ast, &types, key_t);
    pawA_list_push(R->ast, &types, value_t);
    type->adt.types = types;
    return type;
}

static paw_Bool is_vector_t(const AstType *t)
{
    return a_is_adt(t) && t->adt.base == PAW_TVECTOR;
}

static paw_Bool is_map_t(const AstType *t)
{
    return a_is_adt(t) && t->adt.base == PAW_TMAP;

}

static AstType *vector_elem(const AstType *t)
{
    paw_assert(is_vector_t(t));
    return t->adt.types->data[0];
}

static AstType *map_key(const AstType *t)
{
    paw_assert(is_map_t(t));
    return t->adt.types->data[0];
}

static AstType *map_value(const AstType *t)
{
    paw_assert(is_map_t(t));
    return t->adt.types->data[1];
}

static AstType *resolve_expr(struct AstVisitor *V, struct AstExpr *expr) 
{
    struct Resolver *R = V->state.R;
    V->visit_expr(V, expr);

    struct AstType *type = a_type(expr);
    type = normalize(R->U->table, type);
    if (is_unit_variant(R, type)) {
        return type->func.result;
    }
    return type;
}

static void visit_stmts(struct AstVisitor *V, struct AstList *list)
{
    V->visit_stmt_list(V, list, V->visit_stmt);
}

#define are_types_same(a, b) ((a) == (b))

// TODO: move to unify.c/reuse logic in that file (write idempotent version of unify())
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
        case AST_TYPE_TUPLE:
            return test_binders(R, a->tuple.elems, b->tuple.elems);
        case AST_TYPE_FPTR:
            return test_binders(R, a->fptr.params, b->fptr.params);
        case AST_TYPE_FUNC:
            return test_types(R, a->func.result, b->func.result) &&
                   test_binders(R, a->func.params, b->func.params);
        case AST_TYPE_ADT: {
            if (a->adt.base == b->adt.base) {
                if (!a->adt.types == !b->adt.types) {
                    return a->adt.types != NULL 
                        ? test_binders(R, a->adt.types, b->adt.types) 
                        : PAW_TRUE;
                }
            }
            break;
        }
        default:
            paw_assert(a_kind(a) == AST_TYPE_GENERIC);
            return are_types_same(a, b);
    }
    return PAW_FALSE;
}

static Symbol *basic_symbol(Resolver *R, paw_Type code)
{
    paw_assert(code >= 0 && code <= PAW_TSTRING);

    // basic types have fixed locations
    Scope *pub = R->sym->globals;
    return pub->symbols->data[1 + code]; // TODO
}

static Scope *push_symbol_table(Resolver *R) 
{
    return pawA_new_scope(R->ast, R->sym); 
}

static void pop_symbol_table(Resolver *R)
{
    // Last symbol table should have been assigned to an AST node. The
    // next call to push_symbol_table() will allocate a new table.
    SymbolTable *st = R->sym;
    paw_assert(st->scopes->count > 0);
    --st->scopes->count;
}

static DefId add_decl(Resolver *R, AstDecl *decl)
{
    return pawA_add_decl(R->ast, decl);
}

static AstType *new_type(Resolver *R, DefId did, AstTypeKind kind)
{
    AstType *type = pawA_new_type(R->ast, kind);
    if (kind == AST_TYPE_ADT) {
        type->adt.types = pawA_list_new(R->ast);
        type->adt.did = did;
    } else {
        type->func.types = pawA_list_new(R->ast);
        type->func.did = did;
    }
    if (did != NO_DECL) {
        // set type of associated definition
        AstDecl *d = get_decl(R, did);
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
    DefId did = add_decl(V->state.R, decl);
    d->type = new_type(V->state.R, did, AST_TYPE_GENERIC);
    d->type->generic.name = d->name;
    d->type->generic.did = did;
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

static void enter_inference_ctx(Resolver *R)
{
    pawU_enter_binder(R->U);
}

static void leave_inference_ctx(Resolver *R)
{
    pawU_leave_binder(R->U);
}

static Scope *enclosing_scope(Resolver *R)
{
    SymbolTable *st = R->sym;
    return st->scopes->data[st->scopes->count - 1];
}

static Symbol *add_symbol(Resolver *R, Scope *scope, String *name,
                          AstDecl *decl)
{
    Symbol *symbol = pawA_add_symbol(R->ast, scope);
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
    for (int i = 0; i < st->symbols->count; ++i) {
        Symbol *symbol = st->symbols->data[i];
        if (pawS_eq(symbol->name, name)) {
            name_error(R, decl->hdr.line, "duplicate global '%s' (declared previously on line %d)", 
                       name->text, symbol->decl->hdr.line);
        }
    }
    return add_symbol(R, st, name, decl);
}

static Symbol *try_resolve_symbol(Resolver *R, String *name)
{
    // search the scoped symbols
    SymbolTable *scopes = R->sym;
    const int nscopes = scopes->scopes->count;
    for (int depth = nscopes - 1; depth >= 0; --depth) {
        Scope *scope = scopes->scopes->data[depth];
        const int index = pawA_find_symbol(scope, name);
        if (index >= 0) {
            Symbol *symbol = scope->symbols->data[index];
            if (scope->fn_depth != R->func_depth 
                    && !symbol->is_type
                    && !R->in_closure) {
                 // TODO: replace is_type with more specific flag, this will mess up function templates!
                 //       Types are not captured as upvalues
                type_error(R, "attempt to reference non-local variable '%s' "
                              "(consider using a closure)", name->text);
            }
            return scope->symbols->data[index];
        }
    }
    // search the global symbols
    const int index = pawA_find_symbol(scopes->globals, name);
    if (index < 0) {
        return NULL;
    }
    return scopes->globals->symbols->data[index];
}

static Symbol *resolve_symbol(Resolver *R, String *name)
{
    Symbol *symbol = try_resolve_symbol(R, name);
    if (symbol == NULL) {
        name_error(R, -1, "undefined symbol '%s'", name->text);
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
        scope = push_symbol_table(R);
    } else {
        pawA_add_scope(R->ast, R->sym, scope);
    }
    scope->fn_depth = R->func_depth;
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
    visit_stmts(V, block->stmts);
    block->scope = leave_block(V->state.R);
}

static AstList *register_generics(AstVisitor *V, AstList *generics)
{
    struct Resolver *R = V->state.R;
    struct AstList *types = collect_generics(V, generics);
    for (int i = 0; i < types->count; ++i) {
        const struct AstType *type = types->data[i];
        struct AstDecl *decl = generics->data[i];
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

static AstList *subst_binder(AstTypeFolder *F, AstList *binder)
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

static AstType *subst_fptr(AstTypeFolder *F, AstFuncPtr *t)
{
    Subst *subst = F->state;
    Resolver *R = subst->R;
    AstType *r = new_type(R, NO_DECL, AST_TYPE_FPTR);
    r->fptr.params = subst_binder(F, t->params);
    r->fptr.result = F->fold(F, t->result);
    return r;
}

static AstType *subst_func(AstTypeFolder *F, AstFuncDef *t)
{
    Subst *subst = F->state;
    Resolver *R = subst->R;
    AstType *r = new_type(R, NO_DECL, AST_TYPE_FUNC);
    r->func.base = t->did;
    r->func.types = subst_binder(F, t->types);
    r->func.params = subst_binder(F, t->params);
    r->func.result = F->fold(F, t->result);
    return r;
}

static AstType *subst_adt(AstTypeFolder *F, AstAdt *t)
{
    Subst *subst = F->state;
    Resolver *R = subst->R;
    if (t->did <= PAW_TSTRING) {
        return a_cast_type(t);
    }
    AstType *r = new_type(R, NO_DECL, AST_TYPE_ADT);
    r->adt.base = t->base;
    r->adt.types = subst_binder(F, t->types);
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

static AstType *subst_generic(AstTypeFolder *F, AstGeneric *t)
{
    return maybe_subst(F, a_cast_type(t));
}

static AstType *subst_unknown(AstTypeFolder *F, AstUnknown *t)
{
    return maybe_subst(F, a_cast_type(t));
}

// Make a copy of a function template's parameter list, with bound (by the
// template) type variables replaced with inference variables. The types
// returned by this function can be unified with the type of each argument
// passed at the call site to determine a concrete type for each unknown.
static AstList *prep_func_inference(AstVisitor *V, AstList *before, AstList *after, AstList *target)
{
    Subst subst = {
        .before = before,
        .after = after,
        .R = V->state.R,
    };
    AstTypeFolder F;
    pawA_type_folder_init(&F, &subst);
    F.fold_adt = subst_adt;
    F.fold_fptr = subst_fptr;
    F.fold_func = subst_func;
    F.fold_generic = subst_generic;
    F.fold_unknown = subst_unknown;
    F.fold_binder(&F, target);
    return target;
}

static AstType *register_decl_type(AstVisitor *V, AstDecl *decl,
                                   AstTypeKind kind)
{
    Resolver *R = V->state.R;
    DefId did = add_decl(V->state.R, decl);
    AstType *r = new_type(R, did, kind);
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
    r->func.result = resolve_expr(V, d->result);
    r->func.base = d->def;
    d->type = r;

    d->scope = leave_block(R);
}

static AstList *transfer_fields(AstVisitor *V, AstList *list, AstDeclPass callback)
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
    r->func.result = resolve_expr(V, base->result);
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
     d->type->func.base = R->adt->adt.did;
     d->type->func.types = pawA_list_new(V->ast);
     d->type->func.params = collect_params(V, d->fields);
     d->type->func.result = R->adt;

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
        d->type->func.base = R->adt->adt.did;
        d->type->func.types = pawA_list_new(V->ast);
        d->type->func.params = collect_params(V, d->fields);
        d->type->func.result = R->adt;
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
        const AstType *type = get_type(R, inst->inst.def);
        if (test_binders(R, types, type->func.types)) {
            return inst;
        }
    }
    return NULL;
}

static AstDecl *find_struct_instance(AstVisitor *V, StructDecl *base,
                                     AstList *types)
{
    Resolver *R = V->state.R;
    for (int i = 0; i < base->monos->count; ++i) {
        AstDecl *inst = base->monos->data[i];
        const AstType *type = get_type(R, inst->inst.def);
        if (test_binders(R, types, type->adt.types)) {
            return inst;
        }
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

    AstType *outer = R->result;
    R->result = type->func.result;

    // context for inferring container types
    enter_inference_ctx(R);
    V->visit_block_stmt(V, d->body);
    leave_inference_ctx(R);

    d->scope = leave_function(R);
    R->result = outer;
}

static void visit_return_stmt(AstVisitor *V, ReturnStmt *s)
{
    Resolver *R = V->state.R;
    AstType *want = R->result; // function return type
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
    if (types == NULL) {
        printf("instantiate_func(V, base, NULL)\n");
        return cast_decl(base);
    }
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
    if (types == NULL) {
        printf("instantiate_struct(V, base, NULL)\n");
        return cast_decl(base);
    }
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
        type_error(R, "too many generics");
    } else if (args->count < params->count) {
        type_error(R, "not enough generics");
    }
}

static AstType *init_struct_template(AstVisitor *V, StructDecl *base,
                                     AstList *types)
{
    Resolver *R = V->state.R;
    check_template_param(R, base->generics, types);
    AstDecl *inst = instantiate_struct(V, base, types);
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

static AstType *init_func_template(AstVisitor *V, FuncDecl *base,
                                   AstList *types)
{
    Resolver *R = V->state.R;
    check_template_param(R, base->generics, types);
    AstDecl *inst = instantiate_func(V, base, types);
    return a_type(inst);
}

static AstType *explicit_func_template(AstVisitor *V, FuncDecl *base, AstList *elems)
{
    AstList *types = collect_expr_types(V, elems);
    return init_func_template(V, base, types);
}

static AstType *explicit_struct_template(AstVisitor *V, StructDecl *base, AstList *elems)
{
    AstList *types = collect_expr_types(V, elems);
    return init_struct_template(V, base, types);
}

static AstType *instantiate(AstVisitor *V, AstDecl *base, AstList *types)
{
    if (types == NULL) {
        return a_type(base);
    } 
    if (a_is_struct_template_decl(base)) {
        return explicit_struct_template(V, &base->struct_, types);
    } else if (a_is_func_template_decl(base)) {
        return explicit_func_template(V, &base->func, types);
    } 
    return a_type(base);
}

struct StructPack {
    String *name;
    AstList *generics;
    AstList *fields;
    AstType *type;
    AstDecl *decl;
    paw_Bool is_struct;
};

static struct StructPack unpack_struct(Resolver *R, AstType *type)
{
    if (!a_is_func(type) && !a_is_adt(type)) {
        type_error(R, "expected structure or enumerator");
    }
    if (a_is_func(type)) {
        AstDecl *decl = get_decl(R, type->func.did);
        return (struct StructPack){
            .name = decl->variant.name,
            .fields = decl->variant.fields,
            .type = decl->variant.type,
            .decl = decl,
        };
    }
    AstDecl *decl = get_decl(R, type->adt.did);
    if (a_is_struct_decl(decl)) {
        return (struct StructPack){
            .is_struct = decl->struct_.is_struct,
            .name = decl->struct_.name,
            .generics = decl->struct_.generics,
            .fields = decl->struct_.fields,
            .type = decl->struct_.type,
            .decl = decl,
        };
    }
    AstDecl *base = get_decl(R, type->adt.base);
    return (struct StructPack){
        .is_struct = base->struct_.is_struct,
        .name = base->struct_.name,
        .generics = decl->struct_.generics,
        .fields = decl->inst.fields,
        .type = decl->inst.type,
        .decl = decl,
    };
}

static AstDecl *expect_attr(Resolver *R, const struct StructPack *pack, String *name)
{
    AstDecl *attr = resolve_attr(pack->fields, name);
    if (attr == NULL) {
        name_error(R, -1, "field '%s' does not exist in type '%s'",
                     name->text, pack->name->text);
    }
    return attr;
}

static AstType *resolve_base_seg(AstVisitor *V, AstSegment *base)
{
    Symbol *symbol = resolve_symbol(V->state.R, base->name);
    base->type = instantiate(V, symbol->decl, base->types);
    return base->type;
}

static AstType *resolve_next_seg(AstVisitor *V, AstType *base, AstSegment *next)
{
    Resolver *R = V->state.R;
    const struct StructPack pack = unpack_struct(R, base);
    AstDecl *attr = expect_attr(R, &pack, next->name);
    next->type = instantiate(V, attr, next->types);
    return next->type;
}

#if 0
static void debug_path(AstPath *p)
{
    for (int i = 0; i < p->list->count; ++i) {
        AstSegment *s = pawA_path_get(p, i);
        printf("%s", s->name->text);
        if (s->types != NULL) {
            printf("<");
            for (int j = 0; j < s->types->count; ++j) {
                printf("T");
                if (j < s->types->count - 1) {
                    printf(", ");
                }
            }
            printf(">");
        }
    }
    printf("\n");
}
#endif

static AstType *resolve_path(AstVisitor *V, AstPath *path)
{
    AstSegment *segment = pawA_path_get(path, 0);
    AstType *type = resolve_base_seg(V, segment);
    for (int i = 1; i < path->list->count; ++i) {
        segment = pawA_path_get(path, i);
        type = resolve_next_seg(V, type, segment);
    }
    return type;
}

static void visit_typelist_expr(AstVisitor *V, TypeList *e)
{
    AstType *r = new_type(V->state.R, NO_DECL, AST_TYPE_TUPLE);
    r->tuple.elems = collect_expr_types(V, e->types);
    e->type = r;
}

static void visit_path_expr(AstVisitor *V, PathExpr *e)
{
    e->type = resolve_path(V, e->path);
}

static void visit_pathtype_expr(AstVisitor *V, PathType *e)
{
    e->type = resolve_path(V, e->path);
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

static void visit_logical_expr(AstVisitor *V, LogicalExpr *e)
{
    expect_bool_expr(V, e->lhs);
    expect_bool_expr(V, e->rhs);
    e->type = get_type(V->state.R, PAW_TBOOL);
}

static paw_Bool is_option_t(Resolver *R, const AstType *type)
{
    return a_is_adt(type) && type->adt.base == R->option_did;
}

static paw_Bool is_result_t(Resolver *R, const AstType *type)
{
    return a_is_adt(type) && type->adt.base == R->result_did;
}

static void visit_chain_expr(AstVisitor *V, ChainExpr *e)
{
    Resolver *R = V->state.R;
    AstType *result = resolve_expr(V, e->target);
    if (R->result == NULL) {
        syntax_error(R, "'?' outside function body");
    } 
    if (is_option_t(R, result) || is_result_t(R, result)) {
        e->type = result->adt.types->data[0];
    } else {
        syntax_error(R, "invalid operand for '?' operator");
    }
    unify(R, R->result, result);
}

static AstType *get_value_type(AstType *target)
{
    if (a_is_adt(target)) {
        if (is_vector_t(target)) {
            return vector_elem(target);
        } else if (is_map_t(target)) {
            return map_value(target);
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

static void op_type_error(Resolver *R, const AstType *type, const char *what)
{
    if (a_is_unknown(type)) {
        type_error(R, "%s type must be known before comparison", what);
    } else {
        type_error(R, "%s type not equality comparable", what);
    }
}

static AstType *binop_vector(Resolver *R, BinaryOp op, AstType *type)
{
    const AstType *elem_t = vector_elem(type);
    if (op == BINARY_ADD) {
        // 2 vectors with the same element type can be added
        return type;
    } else if (!a_is_basic(elem_t)) {
        op_type_error(R, elem_t, "element");
    }
    return get_type(R, PAW_TBOOL);
}

static AstType *binop_map(Resolver *R, AstType *type)
{
    const AstType *key_t = map_key(type);
    const AstType *value_t = map_value(type);
    if (!a_is_basic(key_t)) {
        op_type_error(R, key_t, "key");
    } else if (!a_is_basic(value_t)) {
        op_type_error(R, value_t, "value");
    }
    return get_type(R, PAW_TBOOL);
}

static void visit_binop_expr(AstVisitor *V, BinOpExpr *e)
{
    // clang-format off
    static const uint8_t kValidOps[NBINARYOPS][PAW_NTYPES] = {
        //     type   =  0, b, i, f, s, v, m, ...
        [BINARY_EQ]   = {0, 1, 1, 1, 1, 1, 1},
        [BINARY_NE]   = {0, 1, 1, 1, 1, 1, 1},
        [BINARY_LT]   = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_LE]   = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_GT]   = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_GE]   = {0, 0, 1, 1, 1, 0, 0},
        [BINARY_ADD]  = {0, 0, 1, 1, 1, 1, 0},
        [BINARY_SUB]  = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_MUL]  = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_DIV]  = {0, 0, 1, 1, 0, 0, 0},
        [BINARY_MOD]  = {0, 0, 1, 1, 0, 0, 0},
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
    } else if (left == PAW_TVECTOR) {
        e->type = binop_vector(R, e->op, lhs);
    } else if (left == PAW_TMAP) {
        e->type = binop_map(R, lhs);
    } else if (binop_is_bool(e->op)) {
        e->type = get_type(R, PAW_TBOOL);
    } else {
        e->type = lhs;
    }
}

static void visit_vtype_expr(AstVisitor *V, VectorType *e)
{
    struct Resolver *R = V->state.R;
    struct AstType *elem = resolve_expr(V, e->elem);
    e->type = new_vector_t(R, elem);
}

static void visit_mtype_expr(AstVisitor *V, MapType *e)
{
    struct Resolver *R = V->state.R;
    struct AstType *key = resolve_expr(V, e->key);
    struct AstType *value = resolve_expr(V, e->value);
    e->type = new_map_t(R, key, value);
}

static void visit_signature_expr(AstVisitor *V, FuncType *e)
{
    struct Resolver *R = V->state.R;
    e->type = new_type(R, NO_DECL, AST_TYPE_FPTR);
    e->type->fptr.params = collect_expr_types(V, e->params);
    e->type->fptr.result = resolve_expr(V, e->result);
}

static void visit_closure_expr(AstVisitor *V, ClosureExpr *e)
{
    Resolver *R = V->state.R;
    AstType *result = R->result;
    R->in_closure = PAW_TRUE;
    enter_block(R, e->scope);

    V->visit_decl_list(V, e->params, visit_param_decl);

    AstType *t = new_type(R, NO_DECL, AST_TYPE_FPTR);
    t->fptr.params = collect_decl_types(V, e->params);
    t->fptr.result = resolve_expr(V, e->result);
    R->result = t->fptr.result;
    e->type = t;

    V->visit_block_stmt(V, e->body);

    e->scope = leave_block(R);
    R->result = result;
}

static void visit_struct_decl(AstVisitor *V, StructDecl *d)
{
    struct Resolver *R = V->state.R;
    struct Symbol *symbol = new_symbol(R, d->name, cast_decl(d), d->is_global);
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
    Symbol *symbol = declare_symbol(V->state.R, d->name, cast_decl(d), PAW_FALSE);
    d->type = resolve_expr(V, d->rhs);
    // unify(R, d->name, d->type);
    define_symbol(symbol);
}

static AstList *new_unknowns(AstVisitor *V, int count)
{
    Resolver *R = V->state.R;
    AstList *binder = pawA_list_new(R->ast);
    for (int i = 0; i < count; ++i) {
        AstType *unknown = pawU_new_unknown(R->U);
        pawA_list_push(V->ast, &binder, unknown);
    }
    return binder;
}

struct Generalization {
    AstList *types;
    AstList *fields;
};

static struct Generalization generalize(AstVisitor *V, AstList *generics, AstList *fields)
{
    if (generics->count > 0) {
        generics = collect_decl_types(V, generics);
        fields = collect_decl_types(V, fields);
        AstList *unknowns = new_unknowns(V, generics->count);
        fields = prep_func_inference(V, generics, unknowns, fields);
        generics = unknowns;
    }
    return (struct Generalization){
        .types = generics,
        .fields = fields,
    };
}

static void check_inference(Resolver *R, AstList *unknowns, AstList *generics)
{
    if (unknowns == NULL) {
        return; // no inference
    }
    for (int i = 0; i < unknowns->count; ++i) {
        AstType *type = unknowns->data[i];
        if (a_is_unknown(type)) {
            AstDecl *generic = generics->data[i];
            const String *name = generic->generic.name;
            type_error(R, "unable to infer generic parameter '%s'", name->text);
        }
    }
}

static AstType *infer_func_template(AstVisitor *V, FuncDecl *base, AstList *args)
{
    struct Generalization g = generalize(V, base->generics, base->params);

    Resolver *R = V->state.R;
    paw_assert(args->count == g.fields->count);
    for (int i = 0; i < g.fields->count; ++i) {
        AstExpr *arg = args->data[i];
        AstType *a = g.fields->data[i];
        AstType *b = resolve_expr(V, arg);
        unify(R, a, b);
    }
    check_inference(R, g.types, base->generics);
    AstDecl *inst = instantiate_func(V, base, g.types);
    return a_type(inst);
}

static AstType *setup_call(AstVisitor *V, CallExpr *e)
{
    Resolver *R = V->state.R;
    AstType *t = resolve_expr(V, e->target);
    if (!a_is_func(t)) {
        type_error(R, "type is not callable");
    } else if (e->args->count < t->fptr.params->count) {
        syntax_error(R, "not enough arguments");
    } else if (e->args->count > t->fptr.params->count) {
        syntax_error(R, "too many arguments");
    }
    return t;
}

static void visit_call_expr(AstVisitor *V, CallExpr *e)
{
    Resolver *R = V->state.R;
    // Determine the type of the callable, then find its declaration. Template
    // functions will need type inference, which is handled in setup_call().
    e->func = setup_call(V, e);

    if (a_is_fdef(e->func)) {
        // Function type has an associated declaration. If that declaration is
        // for a function template, attempt to infer the type parameters.
        AstDecl *decl = get_decl(R, e->func->func.did);
        if (a_is_func_template_decl(decl)) {
            e->func = infer_func_template(V, &decl->func, e->args);
            e->type = e->func->fptr.result;
            return;
        }
    }

    const AstList *params = params = e->func->fptr.params;
    e->type = e->func->fptr.result;
    
    if (is_unit_variant(R, e->func)) {
        type_error(R, "cannot call unit variant (omit '()' to construct)");
    }
    paw_assert(e->args->count == params->count);
    for (int i = 0; i < params->count; ++i) {
        AstExpr *arg = e->args->data[i];
        AstType *type = resolve_expr(V, arg);
        unify(R, params->data[i], type);
    }
}

static void visit_conversion_expr(AstVisitor *V, ConversionExpr *e)
{
    Resolver *R = V->state.R;
    AstType *arg = resolve_expr(V, e->arg);
    if (!a_is_adt(arg) || arg->adt.did == PAW_TUNIT || arg->adt.did == PAW_TSTRING) {
        type_error(R, "argument to conversion must be scalar"); 
    }
    e->type = get_type(R, e->to);
}

static AstType *visit_tuple_lit(AstVisitor *V, LiteralExpr *lit)
{
    AstType *r = new_type(V->state.R, NO_DECL, AST_TYPE_TUPLE);
    r->tuple.elems = collect_expr_types(V, lit->tuple.elems);
    return r;
}

static AstType *visit_vector_lit(AstVisitor *V, ContainerLit *e)
{
     struct Resolver *R = V->state.R;
     struct Unifier *U = R->U;

     struct AstType *elem_t = pawU_new_unknown(U);
     for (int i = 0; i < e->items->count; ++i) {
         AstExpr *expr = e->items->data[i];
         AstType *type = resolve_expr(V, expr);
         unify(R, type, elem_t);
     }
     return new_vector_t(R, elem_t);
}

static AstType *visit_map_lit(AstVisitor *V, ContainerLit *e)
{
    struct Resolver *R = V->state.R;
    struct Unifier *U = R->U;

    struct AstType *key_t = pawU_new_unknown(U);
    struct AstType *value_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct AstExpr *expr = e->items->data[i];
        struct AstType *k = resolve_expr(V, expr->mitem.key);
        struct AstType *v = resolve_expr(V, expr->mitem.value);
        unify(R, k, key_t);
        unify(R, v, value_t);
        expr->hdr.type = value_t;
    }
    return new_map_t(R, key_t, value_t);
}

static AstType *visit_container_lit(AstVisitor *V, LiteralExpr *lit)
{
    if (lit->cont.code == PAW_TVECTOR) {
        return visit_vector_lit(V, &lit->cont);
    } else {
        paw_assert(lit->cont.code == PAW_TMAP);
        return visit_map_lit(V, &lit->cont);
    }
}

static String *resolve_item(AstVisitor *V, const struct StructPack *pack, StructItem *item, int index)
{
    item->type = resolve_expr(V, item->value);
    if (item->name == NULL) {
        AstDecl *field = pack->fields->data[index];
        return field->field.name;
    }
    return item->name;
}

static AstType *visit_composite_lit(AstVisitor *V, LiteralExpr *lit)
{
    CompositeLit *e = &lit->comp;
    Resolver *R = V->state.R;
    Lex *lex = R->lex;

    AstType *target = resolve_path(V, e->path);
    if (!a_is_adt(target)) {
        type_error(R, "expected structure type");
    }
    // Use a temporary Map to avoid searching repeatedly through the list of fields.
    paw_Env *P = env(lex);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    v_set_object(pv, map);

    Value key;
    struct Generalization g;
    AstList *field_types = NULL;
    paw_Bool is_inference = PAW_FALSE;
    const struct StructPack pack = unpack_struct(R, target);
    if (!pack.is_struct) {
        type_error(R, "expected structure but found enumeration");
    } else if (a_is_struct_template_decl(pack.decl)) {
        StructDecl *d = &pack.decl->struct_;
        g = generalize(V, d->generics, d->fields);
        is_inference = PAW_TRUE;
        field_types = g.fields;
    }
    AstList *order = pawA_list_new(R->ast);
    for (int i = 0; i < e->items->count; ++i) {
        AstExpr *item = e->items->data[i];
        String *k = resolve_item(V, &pack, &item->sitem, i);
        v_set_object(&key, k);
        if (pawH_contains(P, map, key)) {
            name_error(R, item->hdr.line, "duplicate field '%s' in struct literal '%s'", 
                       k->text, pack.name->text);
        }
        Value *value = pawH_action(P, map, key, MAP_ACTION_CREATE);
        v_set_int(value, i);
        pawA_list_push(R->ast, &order, item);
    }
    for (int i = 0; i < pack.fields->count; ++i) {
        AstDecl *decl = pack.fields->data[i];
        FieldDecl *field = &decl->field;
        v_set_object(&key, field->name);
        Value *value = pawH_get(P, map, key);
        if (value == NULL) {
            name_error(R, field->line, "missing initializer for field '%s' in struct '%s'",
                       field->name->text, pack.name->text);
        }
        const paw_Int index = v_int(*value);
        AstType *field_t = is_inference
            ? field_types->data[i]
            : a_type(cast_decl(pack.fields->data[i]));
        AstExpr *item = order->data[index];
        item->sitem.index = i; // index of attribute in struct
        unify(R, a_type(item), field_t);
        pawH_remove(P, map, key);
    }
    if (pawH_length(map) > 0) {
        name_error(R, lit->line, "too many initializers for struct '%s'",
                   pack.name->text);
    }
    paw_assert(pack.fields->count == e->items->count);
    pawA_list_free(R->ast, order);
    pawC_pop(P); // pop map
    if (is_inference) {
        check_inference(R, g.types, pack.decl->struct_.generics);
        AstDecl *inst = instantiate_struct(V, &pack.decl->struct_, g.types);
        target = a_type(inst);
    }
    return target;
}

static void visit_literal_expr(AstVisitor *V, LiteralExpr *e)
{
    if (e->lit_kind == LIT_BASIC) {
        e->type = get_type(V->state.R, e->basic.t);
    } else if (e->lit_kind == LIT_TUPLE) {
        e->type = visit_tuple_lit(V, e);
    } else if (e->lit_kind == LIT_CONTAINER) {
        e->type = visit_container_lit(V, e);
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

static void visit_forbody(AstVisitor *V, String *iname, AstType *itype, Block *b)
{
    Resolver *R = V->state.R;
    enter_block(R, NULL);

    AstDecl *r = pawA_new_decl(R->ast, DECL_VAR);
    r->var.name = iname;
    r->var.type = itype;

    new_local(R, iname, r);
    visit_stmts(V, b->stmts);
    b->scope = leave_block(R);
}

static void visit_fornum(AstVisitor *V, ForStmt *s)
{
    Resolver *R = V->state.R;
    ForNum *fornum = &s->fornum;

    expect_int_expr(V, fornum->begin);
    expect_int_expr(V, fornum->end);
    expect_int_expr(V, fornum->step);

    new_local_literal(V->state.R, "(for begin)", PAW_TINT);
    new_local_literal(V->state.R, "(for end)", PAW_TINT);
    new_local_literal(V->state.R, "(for step)", PAW_TINT);

    visit_forbody(V, s->name, get_type(R, PAW_TINT), s->block);
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
    //    V->visit_forbody(V, s->name, s->block);
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

static void visit_index_expr(AstVisitor *V, Index *e)
{
    Resolver *R = V->state.R;
    AstType *target = resolve_expr(V, e->target);
    if (!a_is_adt(target)) {
        goto not_container;
    }
    AstType *expect = NULL;
    if (is_vector_t(target)) {
        expect = get_type(R, PAW_TINT);
        e->type = e->is_slice ? target : vector_elem(target);
    } else if (is_map_t(target)) {
        if (e->is_slice) {
            type_error(R, "slice operation not supported on map "
                          "(requires '[T]' or 'string')");
        }
        expect = map_key(target);
        e->type = map_value(target);
    } else if (target->adt.base == PAW_TSTRING) {
        expect = get_type(R, PAW_TINT);
        e->type = get_type(R, PAW_TSTRING);
    } else {
not_container:
        type_error(R, "type cannot be indexed (not a container)");
    }
    if (e->is_slice) {
        if (e->first != NULL) {
            AstType *first = resolve_expr(V, e->first);
            unify(R, expect, first);
        }
        if (e->second != NULL) {
            AstType *second = resolve_expr(V, e->second);
            unify(R, expect, second);
        }
    } else {
        AstType *first = resolve_expr(V, e->first);
        unify(R, expect, first);
    }
}

static void visit_tuple_selector(AstVisitor *V, AstType *target, Selector *e)
{
    Resolver *R = V->state.R;
    AstList *types = target->tuple.elems;
    if (!e->is_index) {
        type_error(R, "expected index of tuple element");
    } else if (e->index >= types->count) {
        type_error(R, "expected element index");
    }
    e->type = types->data[e->index];
}

static void visit_selector_expr(AstVisitor *V, Selector *e)
{
    Resolver *R = V->state.R;
    AstType *type = resolve_expr(V, e->target);
    if (a_is_tuple(type)) {
        visit_tuple_selector(V, type, e); 
        return;
    }
    const struct StructPack pack = unpack_struct(R, type);
    if (!pack.is_struct) {
        type_error(R, "cannot select field of enum variant "
                      "(use pattern matching to unpack variant fields)");
    } else if (e->is_index) {
        type_error(R, "expected name of struct field "
                      "(integer indices can only be used with tuples)");
    }
    AstDecl *attr = expect_attr(R, &pack, e->name);
    e->type = attr->hdr.type;
}

static void visit_decl_stmt(AstVisitor *V, AstDeclStmt *s)
{
    V->visit_decl(V, s->decl);
}

static void visit_literal_pat(AstVisitor *V, AstLiteralPat *p)
{
    p->type = resolve_expr(V, p->expr);
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
    if (a_kind(pat) == AST_PAT_PATH && pat->path.path->list->count == 1) {
        AstSegment *segment = pat->path.path->list->data[0];
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

static AstType *resolve_sfield_pat(AstVisitor *V, const struct StructPack *pack, AstFieldPat *p)
{
    Resolver *R = V->state.R;
    paw_assert(pack->is_struct);
    paw_assert(p->name != NULL);
    AstDecl *attr = resolve_attr(pack->fields, p->name);
    if (attr == NULL) {
        name_error(R, p->line, "field '%s' does not exist in type '%s'",
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
        name_error(R, p->line, "missing fields from struct pattern for '%s'", pack.name->text);
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
    p->type = target->func.result;

    const struct StructPack pack = unpack_struct(R, target);
    if (pack.is_struct) {
        type_error(R, "expected variant '%s'", pack.name->text);
    } else if (pack.fields->count != p->elems->count) {
        name_error(R, p->line, "missing fields from variant pattern for '%s'", pack.name->text);
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
    Symbol *symbol = declare_symbol(V->state.R, d->name, cast_decl(d), PAW_TRUE);
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
    const paw_Type code = flag2code(name->flag);
    AstType *type = R->ast->builtin[code];

    AstExpr *e = pawA_new_expr(R->ast, EXPR_PATH);
    e->path.path = pawA_path_new(R->ast);
    pawA_path_add(R->ast, e->path.path, name, NULL, type);

    AstDecl *d = pawA_new_decl(R->ast, DECL_TYPE);
    d->type.name = name;
    d->type.line = 0;
    d->type.rhs = e;

    add_decl(R, d);

    d->hdr.type = type;
    Symbol *symbol = new_local(R, name, d);
    symbol->is_type = PAW_TRUE;
}

static void add_container_builtin(Resolver *R, const char *name, paw_Type code, const char **generics)
{
    AstDecl *d = pawA_new_decl(R->ast, DECL_TYPE);
    d->type.generics = pawA_list_new(R->ast);
    d->type.name = scan_string(R->lex, name);
    for (const char **pname = generics; *pname != NULL; ++pname) {
        AstDecl *g = pawA_new_decl(R->ast, DECL_GENERIC);
        g->generic.name = scan_string(R->lex, *pname);
        g->generic.type = pawA_new_type(R->ast, AST_TYPE_GENERIC);
        g->generic.type->generic.name = g->generic.name;
        g->generic.type->generic.did = add_decl(R, g);
        g->generic.line = 0;
        pawA_list_push(R->ast, &d->type.generics, g);
    }
    d->type.type = R->ast->builtin[code];
    d->type.line = 0;
    add_decl(R, d);
}

static void visit_prelude(AstVisitor *V, Resolver *R)
{
    const AstState state = {.R = R};
    pawA_visitor_init(V, R->ast, state);
    V->visit_func_decl = visit_prelude_func;
    V->visit_struct_decl = visit_prelude_struct;
    V->visit_signature_expr = visit_signature_expr;
    V->visit_closure_expr = visit_closure_expr;
    V->visit_vtype_expr = visit_vtype_expr;
    V->visit_mtype_expr = visit_mtype_expr;
    V->visit_typelist_expr = visit_typelist_expr;
    V->visit_pathtype_expr = visit_pathtype_expr;

    visit_stmts(V, R->ast->prelude);
}

static void setup_module(AstVisitor *V, Resolver *R, AstDecl *r)
{
    SymbolTable *symtab = R->ast->symtab;
    enter_block(R, symtab->globals);
    add_basic_builtin(R, cached_str(R, CSTR_UNIT));
    add_basic_builtin(R, cached_str(R, CSTR_BOOL));
    add_basic_builtin(R, cached_str(R, CSTR_INT));
    add_basic_builtin(R, cached_str(R, CSTR_FLOAT));
    add_basic_builtin(R, cached_str(R, CSTR_STRING));
    add_container_builtin(R, "(Vector)", PAW_TVECTOR, (const char *[]){"T", NULL});
    add_container_builtin(R, "(Map)", PAW_TMAP, (const char *[]){"K", "V", NULL});
    symtab->globals = leave_block(R);

    r->func.type = new_type(R, NO_DECL, AST_TYPE_FUNC);
    r->func.type->func.types = pawA_list_new(R->ast);
    r->func.type->func.params = pawA_list_new(R->ast);
    r->func.type->func.result = get_type(R, PAW_TUNIT);
    r->func.params = pawA_list_new(R->ast);

    visit_prelude(V, R);

    Symbol *symbol = resolve_symbol(R, scan_string(R->lex, "Option"));
    R->option_did = symbol->decl->struct_.def;
    symbol = resolve_symbol(R, scan_string(R->lex, "Result"));
    R->result_did = symbol->decl->struct_.def;

    const AstState state = {.R = R};
    pawA_visitor_init(V, R->ast, state);
    V->visit_literal_expr = visit_literal_expr;
    V->visit_logical_expr = visit_logical_expr;
    V->visit_chain_expr = visit_chain_expr;
    V->visit_unop_expr = visit_unop_expr;
    V->visit_binop_expr = visit_binop_expr;
    V->visit_conversion_expr = visit_conversion_expr;
    V->visit_call_expr = visit_call_expr;
    V->visit_index_expr = visit_index_expr;
    V->visit_selector_expr = visit_selector_expr;
    V->visit_typelist_expr = visit_typelist_expr;
    V->visit_pathtype_expr = visit_pathtype_expr;
    V->visit_path_expr = visit_path_expr;
    V->visit_match_expr = visit_match_expr;
    V->visit_arm_expr = visit_arm_expr;
    V->visit_signature_expr = visit_signature_expr;
    V->visit_closure_expr = visit_closure_expr;
    V->visit_vtype_expr = visit_vtype_expr;
    V->visit_mtype_expr = visit_mtype_expr;
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
    enter_inference_ctx(R);

    AstVisitor V;
    setup_module(&V, R, r);
    pawA_visit(&V);

    leave_inference_ctx(R);
    sym->toplevel = leave_function(R);
    paw_assert(sym->scopes->count == 0);
}

void p_check_types(Lex *lex)
{
    Ast *ast = lex->pm->ast;
    Resolver R = {
        .lex = lex,
        .pm = lex->pm,
        .ast = ast,
        .sym = ast->symtab,
        .U = &lex->pm->unifier,
    };
    visit_module(&R);
}
