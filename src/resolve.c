// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// resolve.c: Implementation of the type checker. This code transforms an AST
// from the parser into a type-annotated HIR tree.

#include "ast.h"
#include "code.h"
#include "compile.h"
#include "debug.h"
#include "env.h"
#include "gc.h"
#include "hir.h"
#include "ir_type.h"
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "str.h"
#include "type_folder.h"
#include "unify.h"

#define CSTR(R, i) CACHED_STRING(ENV(R), CAST_SIZE(i))
#define TYPE2CODE(R, type) (pawP_type2code((R)->C, type))

struct ResultState {
    struct ResultState *outer;
    struct IrType *prev;
    int count;
};

struct MatchState {
    struct MatchState *outer;
    struct IrType *target;
    struct HirPatList *bindings;
};

// Common state for type-checking routines
struct Resolver {
    paw_Env *P;
    Map *strings;
    struct ModuleInfo *m;
    struct Unifier *U; // unification tables
    struct Compiler *C; // compiler state
    struct IrType *self; // enclosing ADT
    struct DynamicMem *dm; // dynamic memory
    struct ResultState *rs;
    struct MatchState *ms;
    struct HirSymtab *symtab;
    struct HirTypeFolder *F;
    struct Substitution subst;
    Map *impls; // '.impls' from Compiler
    int func_depth; // number of nested functions
    int line;
    paw_Bool in_closure; // 1 if the enclosing function is a closure, else 0
    paw_Bool in_impl;
};

_Noreturn static void not_a_type(struct Resolver *R, struct IrType *type)
{
    const char *type_name = pawIr_print_type(R->C, type);
    TYPE_ERROR(R, "'%s' is not a type", type_name);
}

static void resolve_stmt(struct Resolver *, struct HirStmt *);
static void resolve_decl(struct Resolver *, struct HirDecl *);
static struct IrType *resolve_expr(struct Resolver *, struct HirExpr *);
static struct IrType *resolve_type(struct Resolver *, struct HirType *);
static struct IrType *resolve_pat(struct Resolver *, struct HirPat *);

#define DEFINE_LIST_RESOLVER(name, T) \
    static void resolve_##name##_list(struct Resolver *R, struct Hir##T##List *list) \
    { \
        if (list == NULL) return; \
        for (int i = 0; i < list->count; ++i) { \
            resolve_##name(R, list->data[i]); \
        } \
    }
DEFINE_LIST_RESOLVER(expr, Expr)
DEFINE_LIST_RESOLVER(decl, Decl)
DEFINE_LIST_RESOLVER(stmt, Stmt)
#undef DEFINE_LIST_RESOLVER

static struct IrTypeList *resolve_pat_list(struct Resolver *R, struct HirPatList *pats)
{
    struct IrTypeList *types = pawIr_type_list_new(R->C);
    if (pats != NULL) {
        for (int i = 0; i < pats->count; ++i) {
            struct IrType *type = resolve_pat(R, K_LIST_GET(pats, i));
            K_LIST_PUSH(R->C, types, type);
        }
    }
    return types;
}

static struct IrTypeList *resolve_type_list(struct Resolver *R, struct HirTypeList *list)
{
    if (list == NULL) return NULL;
    struct IrTypeList *result = pawIr_type_list_new(R->C);
    for (int i = 0; i < list->count; ++i) {
        struct IrType *type = resolve_type(R, list->data[i]);
        K_LIST_PUSH(R->C, result, type);
    }
    return result;
}

static struct IrType *normalize(struct Resolver *R, struct IrType *type)
{
    return pawU_normalize(R->U->table, type);
}

static void unify(struct Resolver *R, struct IrType *a, struct IrType *b)
{
    pawU_unify(R->U, a, b);
    normalize(R, a);
    normalize(R, b);
}

static paw_Bool is_list_t(struct Resolver *R, struct IrType *type)
{
    if (!IrIsAdt(type)) return PAW_FALSE;
    return IR_TYPE_DID(type).value == R->C->builtins[BUILTIN_LIST].did.value;
}

static paw_Bool is_map_t(struct Resolver *R, struct IrType *type)
{
    if (!IrIsAdt(type)) return PAW_FALSE;
    return IR_TYPE_DID(type).value == R->C->builtins[BUILTIN_MAP].did.value;
}

static struct HirDecl *get_decl(struct Resolver *R, DeclId did)
{
    return pawHir_get_decl(R->C, did);
}

static struct IrType *get_type(struct Resolver *R, paw_Type code)
{
    return GET_NODE_TYPE(R->C, get_decl(R, (DeclId){.value = code}));
}

static struct HirSymbol *new_symbol(struct Resolver *R, struct HirDecl *decl)
{
    struct HirSymbol *symbol = pawHir_new_symbol(R->C);
    *symbol = (struct HirSymbol){
        .is_init = PAW_TRUE,
        .name = decl->hdr.name,
        .decl = decl,
    };
    return symbol;
}

static paw_Bool is_unit_variant(struct Resolver *R, struct IrType *type)
{
    if (IrIsSignature(type)) {
        struct HirDecl *decl = get_decl(R, IrGetSignature(type)->did);
        return HirIsVariantDecl(decl) &&
            HirGetVariantDecl(decl)->fields == NULL;
    }
    return PAW_FALSE;
}

static struct HirAdtDecl *get_adt(struct Resolver *R, struct IrType *type)
{
    struct HirDecl *decl = get_decl(R, IR_TYPE_DID(type));
    return HirGetAdtDecl(decl);
}

static struct HirDeclList *impls_for_adt(struct Compiler *C, struct IrType *adt)
{
    struct HirDecl *k = pawHir_get_decl(C, IR_TYPE_DID(adt));
    const Value *pv = pawH_get(C->impls, P2V(k));
    if (pv == NULL) return NULL;
    return pv->p;
}

static struct IrType *maybe_unit_variant(struct Resolver *R, struct IrType *type)
{
    if (IrIsSignature(type)) {
        // handle unit enumerators
        struct HirDecl *decl = get_decl(R, IrGetSignature(type)->did);
        if (HirIsVariantDecl(decl)) {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            if (d->fields == NULL) return IR_FPTR(type)->result;
            TYPE_ERROR(R, "missing fields on enumerator");
        }
    }
    return type;
}

static struct IrType *resolve_operand(struct Resolver *R, struct HirExpr *expr)
{
    struct IrType *type = resolve_expr(R, expr);
    type = maybe_unit_variant(R, type);
    SET_NODE_TYPE(R->C, expr, type);
    return type;
}

static DeclId add_decl(struct Resolver *R, struct HirDecl *decl)
{
    return pawHir_add_decl(R->C, decl, R->m->hir->modno);
}

static struct IrType *new_type(struct Resolver *R, DeclId did, enum IrTypeKind kind, int line)
{
    return pawP_attach_type(R->C, did, kind, line);
}

static struct IrTypeList *resolve_exprs(struct Resolver *R, struct HirExprList *list)
{
    if (list == NULL) return NULL;
    struct IrTypeList *new_list = pawIr_type_list_new(R->C);
    for (int i = 0; i < list->count; ++i) {
        struct IrType *type = resolve_operand(R, list->data[i]);
        K_LIST_PUSH(R->C, new_list, type);
    }
    return new_list;
}

static struct IrTypeList *collect_decl_types(struct Resolver *R, struct HirDeclList *list)
{
    if (list == NULL) return NULL;
    struct IrTypeList *new_list = pawIr_type_list_new(R->C);
    for (int i = 0; i < list->count; ++i) {
        struct IrType *type = GET_NODE_TYPE(R->C, list->data[i]);
        K_LIST_PUSH(R->C, new_list, type);
    }
    return new_list;
}

static struct IrType *new_unknown(struct Resolver *R)
{
    return pawU_new_unknown(R->U, R->line);
}

static struct IrTypeList *new_unknowns(struct Resolver *R, int count)
{
    struct IrTypeList *list = pawIr_type_list_new(R->C);
    for (int i = 0; i < count; ++i) {
        struct IrType *unknown = new_unknown(R);
        K_LIST_PUSH(R->C, list, unknown);
    }
    return list;
}

static void enter_inference_ctx(struct Resolver *R)
{
    pawU_enter_binder(R->U);
}

static void leave_inference_ctx(struct Resolver *R)
{
    pawU_leave_binder(R->U);
}

static void enter_match_ctx(struct Resolver *R, struct MatchState *ms, struct IrType *target)
{
    // TODO: Make match an expression: create a new inference variable for the match result, unify with '.result' in each HirMatchArm
    *ms = (struct MatchState){
        .target = target,
        .outer = R->ms,
    };
    R->ms = ms;
}

static void leave_match_ctx(struct Resolver *R)
{
    R->ms = R->ms->outer;
}

static struct HirScope *enclosing_scope(struct HirSymtab *st)
{
    paw_assert(st->count > 0);
    return K_LIST_GET(st, st->count - 1);
}

static struct HirSymbol *add_symbol(struct Resolver *R, struct HirScope *scope, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = pawHir_add_symbol(R->C, scope);
    symbol->name = name;
    symbol->decl = decl;
    return symbol;
}

static struct HirSymbol *declare_local(struct Resolver *R, String *name, struct HirDecl *decl)
{
    if (IS_KEYWORD(name)) NAME_ERROR(R, "invalid identifier ('%s' is a language keyword)");
    if (IS_BUILTIN(name)) NAME_ERROR(R, "invalid identifier ('%s' is a builtin type name)");
    return add_symbol(R, enclosing_scope(R->symtab), name, decl);
}

// Allow a previously-declared variable to be accessed
static void define_local(struct HirSymbol *symbol)
{
    symbol->is_init = PAW_TRUE;
}

static int find_field(struct HirDeclList *fields, String *name)
{
    if (fields == NULL) return -1;
    for (int i = 0; i < fields->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(fields, i);
        if (pawS_eq(name, decl->hdr.name)) return i;
    }
    return -1;
}

// Return true if 'a' is more generic than or equal to 'b', false otherwise
static paw_Bool is_compat(struct Compiler *C, struct IrType *a, struct IrType *b);

static paw_Bool are_lists_compat(struct Compiler *C, struct IrTypeList *a, struct IrTypeList *b)
{
    paw_assert(a->count == b->count);
    for (int i = 0; i < a->count; ++i) {
        struct IrType *x = K_LIST_GET(a, i);
        struct IrType *y = K_LIST_GET(b, i);
        if (!is_compat(C, x, y)) return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool is_compat(struct Compiler *C, struct IrType *a, struct IrType *b)
{
    if (pawU_equals(&C->dm->unifier, a, b)) return PAW_TRUE;
    if (IrIsGeneric(a) || IrIsInfer(b)) return PAW_TRUE;
    if (IR_KINDOF(a) != IR_KINDOF(b)) return PAW_FALSE;

    switch (IR_KINDOF(a)) {
        case kIrTuple:
            if (!IrIsTuple(b)) return PAW_FALSE;
            return are_lists_compat(C, IrGetTuple(a)->elems, IrGetTuple(b)->elems);
        case kIrFuncPtr:
        case kIrSignature:
            return is_compat(C, IR_FPTR(a)->result, IR_FPTR(b)->result) &&
                are_lists_compat(C, IR_FPTR(a)->params, IR_FPTR(b)->params);
        case kIrAdt:
            return IrGetAdt(a)->did.value == IrGetAdt(b)->did.value &&
                are_lists_compat(C, IrGetAdt(a)->types, IrGetAdt(b)->types);
        default:
            return a == b;
    }
    return PAW_FALSE;
}

static struct HirSymbol *new_local(struct Resolver *R, String *name, struct HirDecl *decl, paw_Bool is_type)
{
    struct HirSymbol *symbol = declare_local(R, name, decl);
    symbol->is_type = is_type;
    define_local(symbol);
    return symbol;
}

static struct HirScope *leave_block(struct Resolver *R)
{
    struct HirSymtab *st = R->symtab;
    struct HirScope *scope = enclosing_scope(st);
    --st->count;
    return scope;
}

static void enter_block(struct Resolver *R, struct HirScope *scope)
{
    if (scope == NULL) scope = pawHir_scope_new(R->C);
    K_LIST_PUSH(R->C, R->symtab, scope);
}

static struct HirScope *leave_function(struct Resolver *R)
{
    struct HirScope *scope = leave_block(R);
    CHECK_GC(ENV(R));
    return scope;
}

static void enter_function(struct Resolver *R, struct HirFuncDecl *func)
{
    enter_block(R, NULL);
    new_local(R, func->name, HIR_CAST_DECL(func), PAW_FALSE);
}

static void ResolveBlock(struct Resolver *R, struct HirBlock *block)
{
    enter_block(R, NULL);
    resolve_stmt_list(R, block->stmts);
    leave_block(R);
}

static struct IrType *register_decl_type(struct Resolver *R, struct HirDecl *decl, enum IrTypeKind kind)
{
    const DeclId did = add_decl(R, decl);
    struct IrType *type = new_type(R, did, kind, decl->hdr.line);
    SET_NODE_TYPE(R->C, decl, type);
    return type;
}

static void allocate_decls(struct Resolver *R, struct HirDeclList *decls, paw_Bool is_type)
{
    if (decls == NULL) return;
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = decls->data[i];
        new_local(R, decl->hdr.name, decl, is_type);
    }
}

static struct IrType *ResolveFieldDecl(struct Resolver *R, struct HirFieldDecl *d)
{
    add_decl(R, HIR_CAST_DECL(d));
    return resolve_type(R, d->tag);
}

static void resolve_func_item(struct Resolver *R, struct HirFuncDecl *d)
{
    enter_function(R, d);
    allocate_decls(R, d->generics, PAW_TRUE);
    allocate_decls(R, d->params, PAW_FALSE);
    struct IrType *result = resolve_type(R, d->result);

    if (d->body != NULL) {
        struct ResultState rs = {
            // named function has explicit return type
            .prev = result,
            .outer = R->rs,
        };
        R->rs = &rs;

        enter_inference_ctx(R);
        ResolveBlock(R, d->body);
        leave_inference_ctx(R);

        R->rs = rs.outer;
    }
    leave_function(R);
}

static void ResolveReturnStmt(struct Resolver *R, struct HirReturnStmt *s)
{
    struct IrType *want = R->rs->prev;
    struct IrType *have = NULL;
    if (s->expr != NULL) {
        have = resolve_operand(R, s->expr);
    } else {
        have = get_type(R, PAW_TUNIT);
    }

    unify(R, have, want);
    ++R->rs->count;
}

static void resolve_item(struct Resolver *R, struct HirDecl *item);
static void resolve_lazy(struct Resolver *R, struct HirDecl *decl)
{
    if (decl->hdr.flags == 0) {
        decl->hdr.flags = 1;
        resolve_item(R, decl);
    }
}

static void resolve_methods(struct Resolver *R, struct HirDeclList *items, struct IrAdt *self)
{
    for (int i = 0; i < items->count; ++i) {
        resolve_lazy(R, items->data[i]);
    }
}

static struct IrType *resolve_path(struct Resolver *R, struct HirPath *path, enum LookupKind kind);

static void resolve_impl_methods(struct Resolver *R, struct HirImplDecl *d)
{
    enter_block(R, NULL);

    // Lookup the 'self' ADT. If the ADT is an instance of a polymorphic ADT, it
    // may need to be instantiated here. Either way, the LazyItem holding the ADT
    // will be resolved (this is fine, since impl blocks are resolved in a separate
    // pass, after all ADTs have been registered).
    // use the identifier 'Self' to refer to the 'self' ADT
    String *name = SCAN_STRING(R, "Self");
    struct HirDecl *result = pawHir_new_decl(R->C, d->line, kHirTypeDecl);
    struct HirTypeDecl *r = HirGetTypeDecl(result);
    add_decl(R, result);

    // TODO
    pawIr_set_type(R->C, r->hid, pawIr_get_type(R->C, d->hid));

    struct HirSymbol *symbol = new_local(R, name, result, PAW_TRUE);
    symbol->is_type = PAW_TRUE;

    resolve_methods(R, d->methods, IrGetAdt(R->self));
    allocate_decls(R, d->methods, PAW_FALSE);

    leave_block(R);
}

struct IrType *pawP_instantiate_field(struct Compiler *C, struct IrType *inst_type, struct HirDecl *field)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    struct IrAdt *t = IrGetAdt(inst_type);
    struct HirDecl *decl = pawHir_get_decl(C, t->did);
    if (HirGetAdtDecl(decl)->generics == NULL) {
        return GET_NODE_TYPE(C, field);
    }
    struct IrType *base_type = GET_NODE_TYPE(C, decl);
    pawP_init_substitution_folder(&F, C, &subst,
            ir_adt_types(base_type), t->types);
    return pawIr_fold_type(&F, GET_NODE_TYPE(C, field));
}

static struct IrTypeList *instantiate_fields(struct Compiler *C, struct IrType *self, struct HirDeclList *fields)
{
    struct IrTypeFolder F;
    struct Substitution subst;
    struct IrAdt *t = IrGetAdt(self);
    struct HirDecl *decl = pawHir_get_decl(C, t->did);
    struct IrType *type = GET_NODE_TYPE(C, decl);
    pawP_init_substitution_folder(&F, C, &subst,
            ir_adt_types(type), IrGetAdt(self)->types);
    struct IrTypeList *field_types = pawHir_collect_decl_types(C, fields);
    return pawIr_fold_type_list(&F, field_types);
}

struct HirDecl *pawP_find_field(struct Compiler *C, struct IrType *self, String *name)
{
    struct IrAdt *t = IrGetAdt(self);
    struct HirDecl *decl = pawHir_get_decl(C, t->did);
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    const int index = find_field(d->fields, name);
    if (index >= 0) return K_LIST_GET(d->fields, index);
    return NULL;
//    struct IrType *type = pawP_find_method(C, self, name);
//    return pawHir_get_decl(C, IR_TYPE_DID(type));
}

static int expect_field(struct Resolver *R, struct HirAdtDecl *adt, struct IrType *type, String *name)
{
    const int index = find_field(adt->fields, name);
    if (index < 0) NAME_ERROR(R, "field '%s' does not exist on type '%s'",
                              name->text, adt->name->text);
    return index;
}

static void expect_bool_expr(struct Resolver *R, struct HirExpr *e)
{
    struct IrType *type = resolve_operand(R, e);
    unify(R, type, get_type(R, PAW_TBOOL));
}

static void expect_int_expr(struct Resolver *R, struct HirExpr *e)
{
    struct IrType *type = resolve_operand(R, e);
    unify(R, type, get_type(R, PAW_TINT));
}

//static struct IrType *resolve_tuple_type(struct Resolver *R, struct HirTupleType *t)
//{
//    if (t->elems->count == 0) return get_type(R, PAW_TUNIT);
//    struct IrType *r = pawIr_new_type(R->C, kIrTuple);
//    IrGetTuple(r)->elems = resolve_type_list(R, t->elems);
//    return r;
//}

static struct IrType *instantiate(struct Resolver *R, struct HirDecl *base, struct IrTypeList *types)
{
    return pawP_instantiate(R->C, base, types);
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct IrType *lookup_path(struct Resolver *R, struct HirPath *path, enum LookupKind kind)
{
    return pawP_lookup(R->C, R->m, R->symtab, path, kind);
}

static struct IrType *resolve_path(struct Resolver *R, struct HirPath *path, enum LookupKind kind)
{
    struct IrType *type = lookup_path(R, path, kind);
    if (type == NULL) {
        const char *pathname = pawHir_print_path(R->C, path);
        NAME_ERROR(R, "path '%s' not recognized", pathname);
    }
    return type;
}

static void maybe_fix_unit_struct(struct Resolver *R, struct IrType *type, struct HirExpr *expr)
{
    if (IrIsInfer(type)) return; // TODO

    paw_assert(IrIsAdt(type));
    struct HirDecl *decl = get_decl(R, IR_TYPE_DID(type));
    struct HirAdtDecl *adt = HirGetAdtDecl(decl);
    if (adt->did.value <= PAW_TSTR) {
        TYPE_ERROR(R, "expected operand but found primitive type '%s'", adt->name->text);
    }
    if (!adt->is_struct) {
        SYNTAX_ERROR(R, "missing variant specifier on enum '%s'", adt->name->text);
    }
    if (adt->fields != NULL) {
        SYNTAX_ERROR(R, "missing fields on initializer for struct '%s'", adt->name->text);
    }
    struct HirPath *path = HirGetPathExpr(expr)->path;
    expr->hdr.kind = kHirLiteralExpr;
    struct HirLiteralExpr *r = HirGetLiteralExpr(expr);
    r->lit_kind = kHirLitComposite;
    r->comp.items = pawHir_expr_list_new(R->C);
    r->comp.path = path;
}

static struct IrType *resolve_path_expr(struct Resolver *R, struct HirPathExpr *e)
{
    // path might refer to a unit ADT, so we have to use LOOKUP_EITHER
    struct IrType *type = resolve_path(R, e->path, LOOKUP_EITHER);

    struct HirDecl *result = get_decl(R, HIR_PATH_RESULT(e->path));
    if (HirIsAdtDecl(result)) {
        maybe_fix_unit_struct(R, type, HIR_CAST_EXPR(e));
    } else if (HirIsGenericDecl(result)) {
        TYPE_ERROR(R, "unexpected generic '%s'", result->hdr.name->text);
    }
    return type;
}

static void check_map_key(struct Resolver *R, struct IrType *key)
{
    if (!IrIsInfer(key) && !IR_IS_BASIC_T(key)) {
        TYPE_ERROR(R, "key is not hashable");
    }
}

static struct IrType *resolve_logical_expr(struct Resolver *R, struct HirLogicalExpr *e)
{
    expect_bool_expr(R, e->lhs);
    expect_bool_expr(R, e->rhs);
    return get_type(R, PAW_TBOOL);
}

static paw_Bool is_option_t(struct Resolver *R, struct IrType *type)
{
    return IrIsAdt(type) && IR_TYPE_DID(type).value == R->C->builtins[BUILTIN_OPTION].did.value;
}

static paw_Bool is_result_t(struct Resolver *R, struct IrType *type)
{
    return IrIsAdt(type) && IR_TYPE_DID(type).value == R->C->builtins[BUILTIN_RESULT].did.value;
}

static struct IrType *resolve_chain_expr(struct Resolver *R, struct HirChainExpr *e)
{
    struct IrType *type = resolve_operand(R, e->target);
    if (R->rs->prev == NULL) {
        SYNTAX_ERROR(R, "'?' outside function body");
    }
    if (!is_option_t(R, type) && !is_result_t(R, type)) {
        SYNTAX_ERROR(R, "invalid operand for '?' operator");
    }
    unify(R, R->rs->prev, type);
    return K_LIST_GET(ir_adt_types(type), 0); // unwrap
}

static struct IrType *get_value_type(struct Resolver *R, struct IrType *target)
{
    if (is_list_t(R, target)) {
        return ir_list_elem(target);
    } else if (is_map_t(R, target)) {
        return ir_map_value(target);
    }
    return NULL;
}

static paw_Bool is_bool_unop(enum UnaryOp op)
{
    switch (op) {
        case UNARY_NOT:
            return PAW_TRUE;
        default:
            return PAW_FALSE;
    }
}

static paw_Bool is_bool_binop(enum BinaryOp op)
{
    switch (op) {
        case BINARY_EQ:
        case BINARY_NE:
        case BINARY_LT:
        case BINARY_LE:
        case BINARY_GT:
        case BINARY_GE:
            return PAW_TRUE;
        default:
            return PAW_FALSE;
    }
}

static struct IrType *resolve_unop_expr(struct Resolver *R, struct HirUnOpExpr *e)
{
    static const uint8_t kValidOps[NUNARYOPS][NBUILTINS] = {
        //     type  =  0, b, i, f, s, l, m
        [UNARY_LEN]  = {0, 0, 0, 0, 1, 1, 1},
        [UNARY_NEG]  = {0, 0, 1, 1, 0, 0, 0},
        [UNARY_NOT]  = {0, 1, 1, 1, 0, 0, 0},
        [UNARY_BNOT] = {0, 0, 1, 0, 0, 0, 0},
    };

    struct IrType *type = resolve_operand(R, e->target);
    const paw_Type code = TYPE2CODE(R, type);
    if (code < 0 || !kValidOps[e->op][code]) {
        TYPE_ERROR(R, "unsupported operand type for unary operator");
    } else if (is_bool_unop(e->op)) {
        return get_type(R, PAW_TBOOL);
    } else if (e->op == UNARY_LEN) {
        return get_type(R, PAW_TINT);
    } else {
        return type;
    }
}

static struct IrType *resolve_binop_expr(struct Resolver *R, struct HirBinOpExpr *e)
{
    static const uint8_t kValidOps[NBINARYOPS][NBUILTINS] = {
        //     type   =  0, b, i, f, s, l, m
        [BINARY_EQ]   = {0, 1, 1, 1, 1, 0, 0},
        [BINARY_NE]   = {0, 1, 1, 1, 1, 0, 0},
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

    struct IrType *lhs = resolve_operand(R, e->lhs);
    struct IrType *rhs = resolve_operand(R, e->rhs);
    unify(R, lhs, rhs);

    const paw_Type code = TYPE2CODE(R, lhs);
    paw_assert(code == TYPE2CODE(R, rhs));
    if (code < 0 || !kValidOps[e->op][code]) {
        TYPE_ERROR(R, "unsupported operand types for binary operator");
    } else if (is_bool_binop(e->op)) {
        return get_type(R, PAW_TBOOL);
    } else {
        return lhs;
    }
}

static struct IrType *resolve_assign_expr(struct Resolver *R, struct HirAssignExpr *e)
{
    if (!HirIsPathExpr(e->lhs) &&
            !HirIsIndex(e->lhs) &&
            !HirIsSelector(e->lhs)) {
        SYNTAX_ERROR(R, "invalid place for assignment");
    }
    struct IrType *lhs = resolve_operand(R, e->lhs);
    struct IrType *rhs = resolve_operand(R, e->rhs);
    unify(R, lhs, rhs);
    return get_type(R, PAW_TUNIT);
}

static void ResolveMatchStmt(struct Resolver *R, struct HirMatchStmt *s)
{
    struct IrType *target = resolve_operand(R, s->target);

    struct MatchState ms;
    enter_match_ctx(R, &ms, target);
    resolve_stmt_list(R, s->arms);
    leave_match_ctx(R);
}

static void ResolveMatchArm(struct Resolver *R, struct HirMatchArm *s)
{
    enter_block(R, NULL);
    struct MatchState *ms = R->ms;
    ms->bindings = pawHir_pat_list_new(R->C);
    struct IrType *pat = resolve_pat(R, s->pat);
    unify(R, pat, ms->target); // check target type
    ResolveBlock(R, s->result);
    leave_block(R);
}

static struct IrType *new_list_t(struct Resolver *R, struct IrType *elem_t)
{
    struct HirDecl *base = get_decl(R, R->C->builtins[BUILTIN_LIST].did);
    struct IrTypeList *types = pawIr_type_list_new(R->C);
    K_LIST_PUSH(R->C, types, elem_t);
    return instantiate(R, base, types);
}

static struct IrType *new_map_t(struct Resolver *R, struct IrType *key_t, struct IrType *value_t)
{
    check_map_key(R, key_t);
    struct HirDecl *base = get_decl(R, R->C->builtins[BUILTIN_MAP].did);
    struct IrTypeList *types = pawIr_type_list_new(R->C);
    K_LIST_PUSH(R->C, types, key_t);
    K_LIST_PUSH(R->C, types, value_t);
    return instantiate(R, base, types);
}

static void resolve_closure_param(struct Resolver *R, struct HirFieldDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    add_decl(R, decl);
    struct IrType *type = d->tag != NULL
        ? resolve_type(R, d->tag)
        : new_unknown(R);
    new_local(R, d->name, decl, PAW_FALSE);
    SET_NODE_TYPE(R->C, decl, type);
}

static struct IrType *resolve_closure_expr(struct Resolver *R, struct HirClosureExpr *e)
{
    struct ResultState rs = {R->rs};
    R->rs = &rs;
    enter_block(R, NULL);

    for (int i = 0; i < e->params->count; ++i) {
        struct HirDecl *decl = e->params->data[i];
        struct HirFieldDecl *d = HirGetFieldDecl(decl);
        resolve_closure_param(R, d);
    }
    struct IrType *ret = e->result == NULL
        ? new_unknown(R)
        : resolve_type(R, e->result);

    struct IrType *type = new_type(R, NO_DECL, kIrFuncPtr, e->line);
    struct IrFuncPtr *t = IrGetFuncPtr(type);
    t->params = collect_decl_types(R, e->params);
    rs.prev = t->result = ret;

    if (e->has_body) {
        ResolveBlock(R, e->body);
        if (rs.count == 0) {
            // implicit 'return ()'
            unify(R, ret, get_type(R, PAW_TUNIT));
        }
    } else {
        unify(R, ret, resolve_operand(R, e->expr));
    }

    leave_block(R);
    R->rs = rs.outer;
    return type;
}

static struct IrType *find_method_aux(struct Compiler *C, struct HirDecl *base, struct IrType *self, String *name)
{
    struct HirImplDecl *impl = HirGetImplDecl(base);
    struct IrType *impl_type = pawIr_get_type(C, impl->hid);
    if (!is_compat(C, impl_type, self)) return NULL;
    for (int i = 0; i < impl->methods->count; ++i) {
        struct HirDecl *method = K_LIST_GET(impl->methods, i);
        if (!pawS_eq(name, method->hdr.name)) continue;
        if (!HIR_IS_POLY_IMPL(base)) return GET_NODE_TYPE(C, method);
        return pawP_instantiate_method(C, base, ir_adt_types(self), method);
    }
    return NULL;
}

struct IrType *pawP_find_method(struct Compiler *C, struct IrType *adt, String *name)
{
    struct HirDeclList *impls = impls_for_adt(C, adt);
    if (impls == NULL) return NULL; // no impl blocks

    for (int i = 0; i < impls->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(impls, i);
        struct IrType *method = find_method_aux(C, decl, adt, name);
        if (method != NULL) return method;
    }
    return NULL;
}

static void resolve_impl_item(struct Resolver *R, struct HirImplDecl *d)
{
    enter_block(R, NULL);

    allocate_decls(R, d->generics, PAW_TRUE);
    R->self = resolve_path(R, d->self, LOOKUP_TYPE);
    resolve_impl_methods(R, d);

    leave_block(R);
    R->self = NULL;
}

static struct IrType *resolve_var_decl(struct Resolver *R, struct HirVarDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    add_decl(R, decl);

    struct HirSymbol *symbol = declare_local(R, d->name, decl);
    struct IrType *init = resolve_operand(R, d->init);
    define_local(symbol);

    if (d->tag != NULL) {
        struct IrType *tag = resolve_type(R, d->tag);
        unify(R, init, tag);
    }
    return init;
}

static struct IrType *resolve_field_decl(struct Resolver *R, struct HirFieldDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    add_decl(R, decl);
    return resolve_type(R, d->tag);
}

static struct IrType *resolve_type_decl(struct Resolver *R, struct HirTypeDecl *d)
{
    d->generics = NULL; // TODO: generic parameters for aliases

    struct HirSymbol *symbol = declare_local(R, d->name, HIR_CAST_DECL(d));
    struct IrType *type = resolve_operand(R, d->rhs);
    define_local(symbol);
    return type;
}

static struct IrType *method_ctx(struct Resolver *R, struct HirExpr *target)
{
    if (!HirIsSelector(target)) return NULL; // normal function call
    struct HirSelector *select = HirGetSelector(target);
    struct IrType *self = GET_NODE_TYPE(R->C, select->target);
    self = maybe_unit_variant(R, self); // operand => type
    if (!IrIsAdt(self)) return NULL;
    struct IrType *method = pawP_find_method(R->C, self, select->name);
    pawIr_set_type(R->C, select->hid, method);
    return self;
}

static paw_Bool is_polymorphic(struct Resolver *R, struct IrType *type)
{
    if (!IrIsSignature(type) && !IrIsAdt(type)) return PAW_FALSE;
    struct HirDecl *base = get_decl(R, IR_TYPE_DID(type));
    return pawU_equals(R->U, GET_NODE_TYPE(R->C, base), IR_CAST_TYPE(type)) &&
        (HIR_IS_POLY_ADT(base) || HIR_IS_POLY_FUNC(base));
}

// Resolve a function call or enumerator constructor
static struct IrType *resolve_call_expr(struct Resolver *R, struct HirCallExpr *e)
{
    struct IrType *target = resolve_expr(R, e->target);
    if (!IR_IS_FUNC_TYPE(target)) TYPE_ERROR(R, "type is not callable");

    const struct IrFuncPtr *fptr = IR_FPTR(target);
    const int param_offset = method_ctx(R, e->target) != NULL;
    const int nparams = fptr->params->count - param_offset;
    if (e->args->count < nparams) {
        SYNTAX_ERROR(R, "not enough arguments");
    } else if (e->args->count > nparams) {
        SYNTAX_ERROR(R, "too many arguments");
    }

    struct IrTypeList *params = IR_FPTR(target)->params;
    struct IrType *type = IR_FPTR(target)->result;

    if (is_polymorphic(R, target)) {
        struct HirDecl *decl = get_decl(R, IR_TYPE_DID(target));
        paw_assert(HIR_IS_POLY_FUNC(decl));
        target = pawP_generalize(R->C, target);
        params = IR_FPTR(target)->params;
        type = IR_FPTR(target)->result;
        SET_NODE_TYPE(R->C, e->target, target);
    }

    if (is_unit_variant(R, target)) {
        TYPE_ERROR(R, "cannot call unit variant (omit '()' to construct)");
    }

    for (int i = 0; i < nparams; ++i) {
        struct IrType *param = K_LIST_GET(params, i + param_offset);
        struct HirExpr *arg = K_LIST_GET(e->args, i);
        unify(R, param, resolve_operand(R, arg));
    }

    return type;
}

static struct IrType *resolve_conversion_expr(struct Resolver *R, struct HirConversionExpr *e)
{
    struct IrType *type = resolve_operand(R, e->arg);
    if (!IrIsAdt(type) ||
            IR_TYPE_DID(type).value == PAW_TUNIT ||
            IR_TYPE_DID(type).value == PAW_TSTR) {
        TYPE_ERROR(R, "argument to conversion must be scalar");
    }
    return get_type(R, e->to);
}

static struct IrType *resolve_basic_lit(struct Resolver *R, struct HirBasicLit *e)
{
    return get_type(R, e->t);
}

static struct IrTypeList *resolve_operand_list(struct Resolver *R, struct HirExprList *list)
{
    if (list == NULL) return NULL;
    struct IrTypeList *new_list = pawIr_type_list_new(R->C);
    for (int i = 0; i < list->count; ++i) {
        struct IrType *type = resolve_operand(R, list->data[i]);
        K_LIST_PUSH(R->C, new_list, type);
    }
    return new_list;
}

static struct IrType *resolve_tuple_lit(struct Resolver *R, struct HirTupleLit *e, int line)
{
    struct IrType *type = new_type(R, NO_DECL, kIrTuple, line);
    IrGetTuple(type)->elems = resolve_operand_list(R, e->elems);
    return type;
}

static struct IrType *resolve_list_lit(struct Resolver *R, struct HirContainerLit *e)
{
    struct IrType *elem_t = new_unknown(R);
    for (int i = 0; i < e->items->count; ++i) {
        struct HirExpr *expr = K_LIST_GET(e->items, i);
        struct IrType *v = resolve_operand(R, expr);
        unify(R, elem_t, v);
    }
    return new_list_t(R, elem_t);
}

static struct IrType *resolve_map_lit(struct Resolver *R, struct HirContainerLit *e)
{
    struct IrType *key_t = new_unknown(R);
    struct IrType *value_t = new_unknown(R);
    for (int i = 0; i < e->items->count; ++i) {
        struct HirExpr *expr = K_LIST_GET(e->items, i);
        struct HirFieldExpr *field = HirGetFieldExpr(expr);
        paw_assert(field->fid == -1);
        struct IrType *k = resolve_operand(R, field->key);
        struct IrType *v = resolve_operand(R, field->value);
        unify(R, key_t, k);
        unify(R, value_t, v);
        SET_NODE_TYPE(R->C, expr, v);
    }
    return new_map_t(R, key_t, value_t);
}

static struct IrType *resolve_container_lit(struct Resolver *R, struct HirContainerLit *e)
{
    if (e->code == BUILTIN_LIST) {
        return resolve_list_lit(R, e);
    } else {
        paw_assert(e->code == BUILTIN_MAP);
        return resolve_map_lit(R, e);
    }
}

static struct IrType *resolve_field_expr(struct Resolver *R, struct HirFieldExpr *e)
{
    if (e->fid < 0) resolve_operand(R, e->key);
    return resolve_operand(R, e->value);
}

static struct HirExprList *collect_field_exprs(struct Resolver *R, struct HirExprList *items, Map *map, const String *adt)
{
    Value key;
    paw_Env *P = ENV(R);
    struct HirExprList *order = pawHir_expr_list_new(R->C);
    for (int i = 0; i < items->count; ++i) {
        struct HirExpr *expr = items->data[i];
        struct HirFieldExpr *item = HirGetFieldExpr(expr);
        V_SET_OBJECT(&key, item->name);
        if (pawH_contains(map, key)) {
            NAME_ERROR(R, "duplicate field '%s' in initializer for struct '%s'",
                       item->name->text, adt->text);
        }
        Value *value = pawH_create(P, map, key);
        V_SET_INT(value, i);
        K_LIST_PUSH(R->C, order, expr);
    }
    return order;
}

static paw_Bool is_adt_self(struct Resolver *R, struct IrType *adt)
{
    return R->self != NULL
        ? is_compat(R->C, R->self, adt)
        : PAW_FALSE;
}

static void ensure_accessible_field(struct Resolver *R, struct HirDecl *field, struct HirDecl *base, struct IrType *type)
{
    const String *name = field->hdr.name;
    const paw_Bool is_pub = HirIsFieldDecl(field)
        ? HirGetFieldDecl(field)->is_pub
        : HirGetFuncDecl(field)->is_pub;
    if (is_pub || is_adt_self(R, type)) return; // field is public or control is inside own impl block
    NAME_ERROR(R, "'%s.%s' is not a public field", HirGetAdtDecl(base)->name->text, name->text);
}

static struct IrType *subst_type(struct Resolver *R, struct IrTypeList *before, struct IrTypeList *after, struct IrType *target)
{
    if (before == NULL) return target;
    paw_assert(before->count == after->count);

    struct IrTypeFolder F;
    pawP_init_substitution_folder(&F, R->C, &R->subst, before, after);
    return pawIr_fold_type(&F, target);
}

static struct IrTypeList *subst_types(struct Resolver *R, struct IrTypeList *before, struct IrTypeList *after, struct IrTypeList *target)
{
    if (before == NULL) return target;
    paw_assert(before->count == after->count);

    struct IrTypeFolder F;
    pawP_init_substitution_folder(&F, R->C, &R->subst, before, after);
    return pawIr_fold_type_list(&F, target);
}

static struct IrType *resolve_composite_lit(struct Resolver *R, struct HirCompositeLit *e)
{
    struct IrType *type = resolve_path(R, e->path, LOOKUP_TYPE);
    if (!IrIsAdt(type)) TYPE_ERROR(R, "expected structure type");
    struct HirDecl *decl = get_decl(R, IR_TYPE_DID(type));

    // Use a temporary Map to avoid searching repeatedly through the list of
    // fields.
    Map *map = pawP_push_map(R->C);

    Value key;
    struct HirAdtDecl *adt = HirGetAdtDecl(decl);
    struct IrTypeList *field_types = pawHir_collect_decl_types(R->C, adt->fields);
    if (!adt->is_struct) {
        TYPE_ERROR(R, "expected structure but found enumeration '%s'", adt->name->text);
    } else if (adt->fields == NULL) {
        SYNTAX_ERROR(R, "unexpected curly braces on initializer for unit structure '%s'"
                        "(use name without '{}' to create unit struct)", adt->name->text);
    } else if (is_polymorphic(R, type)) {
        type = pawP_generalize(R->C, type);
    }

    struct IrType *base_type = pawIr_get_type(R->C, adt->hid);
    field_types = subst_types(R, ir_adt_types(base_type),
            ir_adt_types(type), field_types);

    struct HirExprList *order = collect_field_exprs(R, e->items, map, adt->name);
    for (int i = 0; i < adt->fields->count; ++i) {
        struct HirDecl *field_decl = K_LIST_GET(adt->fields, i);
        struct HirFieldDecl *field = HirGetFieldDecl(field_decl);
        ensure_accessible_field(R, field_decl, decl, type);
        V_SET_OBJECT(&key, field->name);
        Value *value = pawH_get(map, key);
        if (value == NULL) {
            NAME_ERROR(R, "missing initializer for field '%s' in struct '%s'",
                       field->name->text, adt->name->text);
        }
        struct IrType *type = field_types->data[i];
        struct HirExpr *item = order->data[V_INT(*value)];
        unify(R, type, resolve_operand(R, item));
        pawH_erase(map, key);
        item->field.fid = i;
    }
    paw_Int iter = PAW_ITER_INIT;
    if (pawH_iter(map, &iter)) {
        const Value *pkey = pawH_key(map, CAST_SIZE(iter));
        NAME_ERROR(R, "unexpected field '%s' in initializer for struct '%s'",
                   V_STRING(*pkey), adt->name->text);
    }
    paw_assert(adt->fields->count == e->items->count);
    --ENV(R)->top.p; // pop 'map'

    e->items = order;
    return type;
}

static struct IrType *resolve_literal_expr(struct Resolver *R, struct HirLiteralExpr *e)
{
    if (e->lit_kind == kHirLitBasic) {
        return resolve_basic_lit(R, &e->basic);
    } else if (e->lit_kind == kHirLitTuple) {
        return resolve_tuple_lit(R, &e->tuple, e->line);
    } else if (e->lit_kind == kHirLitContainer) {
        return resolve_container_lit(R, &e->cont);
    } else {
        paw_assert(e->lit_kind == kHirLitComposite);
        return resolve_composite_lit(R, &e->comp);
    }
}

static void ResolveIfStmt(struct Resolver *R, struct HirIfStmt *s)
{
    expect_bool_expr(R, s->cond);
    resolve_stmt(R, s->then_arm);
    if (s->else_arm != NULL) {
        resolve_stmt(R, s->else_arm);
    }
}

static void ResolveExprStmt(struct Resolver *R, struct HirExprStmt *s)
{
    resolve_operand(R, s->expr);
}

static void ResolveWhileStmt(struct Resolver *R, struct HirWhileStmt *s)
{
    enter_block(R, NULL);
    expect_bool_expr(R, s->cond);
    ResolveBlock(R, s->block);
    leave_block(R);
}

static void visit_forbody(struct Resolver *R, struct IrType *itype, struct HirBlock *b, struct HirForStmt *s)
{
    add_decl(R, s->control);
    struct HirVarDecl *control = HirGetVarDecl(s->control);
    pawIr_set_type(R->C, control->hid, itype);

    enter_block(R, NULL);
    new_local(R, control->name, s->control, PAW_FALSE);
    resolve_stmt_list(R, b->stmts);
    leave_block(R);
}

static void visit_fornum(struct Resolver *R, struct HirForStmt *s)
{
    struct HirForNum *fornum = &s->fornum;

    expect_int_expr(R, fornum->begin);
    expect_int_expr(R, fornum->end);
    expect_int_expr(R, fornum->step);

    visit_forbody(R, get_type(R, PAW_TINT), s->block, s);
}

static void visit_forin(struct Resolver *R, struct HirForStmt *s)
{
    struct IrType *iter_t = resolve_operand(R, s->forin.target);
    struct IrType *elem_t = get_value_type(R, iter_t);

    if (elem_t == NULL) TYPE_ERROR(R, "'for..in' not supported for type");
    visit_forbody(R, elem_t, s->block, s);
}

static void ResolveForStmt(struct Resolver *R, struct HirForStmt *s)
{
    enter_block(R, NULL);
    if (s->is_fornum) {
        visit_fornum(R, s);
    } else {
        visit_forin(R, s);
    }
    leave_block(R);
}

static struct IrType *check_index(struct Resolver *R, struct HirIndex *e, struct IrType *target)
{
    struct IrType *result;
    struct IrType *expect = NULL;
    if (is_list_t(R, target)) {
        expect = get_type(R, PAW_TINT);
        result = e->is_slice ? target : ir_list_elem(target);
    } else if (is_map_t(R, target)) {
        if (e->is_slice) {
            TYPE_ERROR(R, "slice operation not supported on map "
                          "(requires '[T]' or 'str')");
        }
        expect = ir_map_key(target);
        result = ir_map_value(target);
    } else if (IrIsAdt(target) &&
            IR_TYPE_DID(target).value == PAW_TSTR) {
        expect = get_type(R, PAW_TINT);
        result = get_type(R, PAW_TSTR);
    } else {
        TYPE_ERROR(R, "type cannot be indexed (not a container)");
    }

    if (e->first != NULL) unify(R, expect, resolve_operand(R, e->first));
    if (e->second != NULL) unify(R, expect, resolve_operand(R, e->second));
    return result;
}

static struct IrType *resolve_index(struct Resolver *R, struct HirIndex *e)
{
    struct IrType *target = resolve_operand(R, e->target);
    return check_index(R, e, target);
}

static struct IrType *resolve_selector(struct Resolver *R, struct HirSelector *e)
{
    struct IrType *target = resolve_operand(R, e->target);
    if (IrIsTuple(target)) {
        // tuples are indexed with "Expr" "." "int_lit"
        struct IrTypeList *types = IrGetTuple(target)->elems;
        if (!e->is_index) {
            TYPE_ERROR(R, "expected index of tuple element");
        } else if (e->index >= types->count) {
            TYPE_ERROR(R, "element index %d out of range of %d-tuple",
                       e->index, types->count);
        }
        return K_LIST_GET(types, e->index);
    }
    if (!IrIsAdt(target)) TYPE_ERROR(R, "type has no fields");
    if (e->is_index) TYPE_ERROR(R, "expected field name (integer indices can "
                                   "only be used with tuples)");
    struct HirAdtDecl *adt = get_adt(R, target);
    struct HirDecl *field;
    struct IrType *result;
    {
        const int index = find_field(adt->fields, e->name);
        if (index < 0) {
            result = pawP_find_method(R->C, target, e->name);
            if (result == NULL) {
                NAME_ERROR(R, "field '%s' does not exist in struct '%s'",
                        e->name->text, adt->name->text);
            }
            field = get_decl(R, IR_TYPE_DID(result));
        } else {
            // refer to the field using its index
            field = K_LIST_GET(adt->fields, index);
            result = pawP_instantiate_field(R->C, target, field);
            e->is_index = PAW_TRUE;
            e->index = index;
        }
    }
    ensure_accessible_field(R, field, HIR_CAST_DECL(adt), target);
    return result;
//    struct IrType *base_type = pawIr_get_type(R->C, adt->hid);
//    return subst_type(R, ir_adt_types(base_type),
//            ir_adt_types(target), GET_NODE_TYPE(R->C, field));
}

static void ResolveJumpStmt(struct Resolver *R, struct HirJumpStmt *s)
{
    PAW_UNUSED(R);
    PAW_UNUSED(s);
}

static void ResolveDeclStmt(struct Resolver *R, struct HirDeclStmt *s)
{
    resolve_decl(R, s->decl);
}

static struct IrType *ResolveOrPat(struct Resolver *R, struct HirOrPat *p)
{
    struct IrType *type = new_unknown(R);
    for (int i = 0; i < p->pats->count; ++i) {
        struct HirPat *pat = K_LIST_GET(p->pats, i);
        unify(R, type, resolve_pat(R, pat));
    }
    return type;
}

static struct IrType *ResolveFieldPat(struct Resolver *R, struct HirFieldPat *p)
{
    return resolve_pat(R, p->pat);
}

static void unify_lists(struct Resolver *R, struct IrTypeList *lhs, struct IrTypeList *rhs)
{
    paw_assert(lhs->count == rhs->count);
    for (int i = 0; i < lhs->count; ++i) {
        unify(R, K_LIST_GET(lhs, i), K_LIST_GET(rhs, i));
    }
}

static struct IrType *ResolveStructPat(struct Resolver *R, struct HirStructPat *p)
{
    struct IrType *type = resolve_path(R, p->path, LOOKUP_TYPE);

    struct HirAdtDecl *adt = get_adt(R, type);
    struct IrTypeList *adt_fields = instantiate_fields(R->C, type, adt->fields);

    if (p->fields->count < adt_fields->count) {
        TYPE_ERROR(R, "not enough fields in struct pattern");
    } else if (p->fields->count > adt_fields->count) {
        TYPE_ERROR(R, "too many fields in struct pattern");
    }

    paw_Env *P = ENV(R);
    Map *map = pawP_push_map(R->C);

    for (int i = 0; i < p->fields->count; ++i) {
        struct HirPat *pat = K_LIST_GET(p->fields, i);
        resolve_pat(R, pat);
        const String *field_name = HirGetFieldPat(pat)->name;
        const Value *pv = pawH_get(map, P2V(field_name));
        if (pv != NULL) NAME_ERROR(R, "duplicate field '%s' in struct pattern", field_name->text);
        pawH_insert(P, map, P2V(field_name), P2V(pat));
    }

    struct HirPatList *sorted = pawHir_pat_list_new(R->C);
    for (int i = 0; i < adt_fields->count; ++i) {
        struct HirFieldDecl *adt_field_decl = HirGetFieldDecl(K_LIST_GET(adt->fields, i));
        struct IrType *adt_field_type = K_LIST_GET(adt_fields, i);
        const Value *pv = pawH_get(map, P2V(adt_field_decl->name));
        if (pv == NULL) NAME_ERROR(R, "missing field '%s' in struct pattern", adt_field_decl->name->text);
        struct HirFieldPat *field_pat = HirGetFieldPat(pv->p);
        unify(R, pawIr_get_type(R->C, field_pat->hid), adt_field_type);
        K_LIST_PUSH(R->C, sorted, pv->p);
        field_pat->index = i;
    }
    p->fields = sorted;

    pawC_pop(P);
    return type;
}

static struct IrType *ResolveVariantPat(struct Resolver *R, struct HirVariantPat *p)
{
    struct IrType *type = resolve_path(R, p->path, LOOKUP_VALUE);
    struct IrTypeList *args = resolve_pat_list(R, p->fields);
    struct IrTypeList *params = IR_FPTR(type)->params;

    struct MatchState *ms = R->ms;
    if (args->count < params->count) {
        TYPE_ERROR(R, "not enough fields in enumerator pattern");
    } else if (args->count > params->count) {
        TYPE_ERROR(R, "too many fields in enumerator pattern");
    }
    unify_lists(R, params, args);

    struct HirVariantDecl *d = HirGetVariantDecl(
            get_decl(R, IR_TYPE_DID(type)));
    p->index = d->index;
    return IR_FPTR(type)->result;
}

static struct IrType *ResolveTuplePat(struct Resolver *R, struct HirTuplePat *p)
{
    struct IrType *r = pawIr_new_type(R->C, kIrTuple);
    IrGetTuple(r)->elems = resolve_pat_list(R, p->elems);
    return r;
}

static struct IrType *ResolveBindingPat(struct Resolver *R, struct HirBindingPat *p)
{
    struct IrType *type = new_unknown(R);
    struct IrType *target = R->ms->target;
    struct HirDecl *r = pawHir_new_decl(R->C, p->line, kHirVarDecl);
    HirGetVarDecl(r)->name = p->name;
    SET_NODE_TYPE(R->C, r, type);
    new_local(R, p->name, r, PAW_FALSE);
    add_decl(R, r);
    return type;
}

static struct IrType *convert_path_to_binding(struct Resolver *R, struct HirPathPat *path)
{
    struct HirSegment *ident = K_LIST_GET(path->path, 0);
    struct HirPat *pat = CAST(struct HirPat *, path);
    pat->hdr.kind = kHirBindingPat;
    struct HirBindingPat *p = HirGetBindingPat(pat);
    p->name = ident->name;

    struct HirPatList *bindings = R->ms->bindings;
    for (int i = 0; i < bindings->count; ++i) {
        struct HirPat *pat = K_LIST_GET(bindings, i);
        if (pawS_eq(p->name, HirGetBindingPat(pat)->name)) {
            NAME_ERROR(R, "duplicate binding '%s' in pattern",
                    p->name->text);
        }
    }
    K_LIST_PUSH(R->C, bindings, pat);

    // resolve again as binding
    return resolve_pat(R, pat);
}

static struct IrType *ResolvePathPat(struct Resolver *R, struct HirPathPat *p)
{
    struct IrType *type = lookup_path(R, p->path, LOOKUP_VALUE);
    struct HirDecl *decl = get_decl(R, HIR_PATH_RESULT(p->path));
    if (type == NULL || (!HirIsAdtDecl(decl) && !HirIsVariantDecl(decl))) {
        // identifier is unbound, or it refers to a variable declaration
        if (p->path->count > 1) NAME_ERROR(R, "invalid path");
        return convert_path_to_binding(R, p);
    }

    // convert to a more specific type of pattern, now that it is known that
    // the path refers to a struct or enumerator
    struct HirPath *path = p->path;
    struct HirPat *pat = CAST(struct HirPat *, p);
    if (HirIsAdtDecl(decl)) {
        paw_assert(!HirGetAdtDecl(decl)->is_struct); // TODO: handle this case
        pat->hdr.kind = kHirStructPat;
        HirGetStructPat(pat)->fields = pawHir_pat_list_new(R->C);
        HirGetStructPat(pat)->path = path;
    } else {
        pat->hdr.kind = kHirVariantPat;
        type = maybe_unit_variant(R, type);
        HirGetVariantPat(pat)->index = HirGetVariantDecl(decl)->index;
        HirGetVariantPat(pat)->fields = pawHir_pat_list_new(R->C);
        HirGetVariantPat(pat)->path = path;
    }
    return type;
}

static struct IrType *ResolveWildcardPat(struct Resolver *R, struct HirWildcardPat *p)
{
    return new_unknown(R);
}

static struct IrType *ResolveLiteralPat(struct Resolver *R, struct HirLiteralPat *p)
{
    return resolve_operand(R, p->expr);
}

static void resolve_decl(struct Resolver *R, struct HirDecl *decl)
{
    R->line = decl->hdr.line;
    struct IrType *type = normalize(R, HirIsVarDecl(decl)
        ? resolve_var_decl(R, HirGetVarDecl(decl))
        : resolve_field_decl(R, HirGetFieldDecl(decl)));
    SET_NODE_TYPE(R->C, decl, type);
}

static struct IrType *resolve_pat(struct Resolver *R, struct HirPat *pat)
{
    struct IrType *type;
    R->line = pat->hdr.line;
    switch (HIR_KINDOF(pat)) {
#define DEFINE_CASE(a, b) \
        case kHir##a: \
            type = Resolve##a(R, HirGet##a(pat)); \
            break;
        HIR_PAT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }

    type = normalize(R, type);
    SET_NODE_TYPE(R->C, pat, type);
    return type;
}

static void resolve_stmt(struct Resolver *R, struct HirStmt *stmt)
{
    R->line = stmt->hdr.line;
    switch (HIR_KINDOF(stmt)) {
#define DEFINE_CASE(a, b) \
        case kHir##a: \
            Resolve##a(R, HirGet##a(stmt)); \
            break;
        HIR_STMT_LIST(DEFINE_CASE)
#undef DEFINE_CASE
    }
}

// NOTE: Some expressions are known to directly represent types, based on the context
//       (type annotations, type arguments, etc.). Call resolve_type() to convert such
//       an expression into a HIR type.

static struct IrType *resolve_type(struct Resolver *R, struct HirType *type)
{
    if (type == NULL) return NULL;
    R->line = type->hdr.line;
    struct IrType *r = pawP_lower_type(R->C, R->m, R->symtab, type);
    return normalize(R, r);
}

static struct IrType *resolve_expr(struct Resolver *R, struct HirExpr *expr)
{
    struct IrType *type;
    R->line = expr->hdr.line;
    switch (HIR_KINDOF(expr)) {
        case kHirLiteralExpr:
            type = resolve_literal_expr(R, HirGetLiteralExpr(expr));
            break;
        case kHirLogicalExpr:
            type = resolve_logical_expr(R, HirGetLogicalExpr(expr));
            break;
        case kHirPathExpr:
            type = resolve_path_expr(R, HirGetPathExpr(expr));
            break;
        case kHirChainExpr:
            type = resolve_chain_expr(R, HirGetChainExpr(expr));
            break;
        case kHirUnOpExpr:
            type = resolve_unop_expr(R, HirGetUnOpExpr(expr));
            break;
        case kHirBinOpExpr:
            type = resolve_binop_expr(R, HirGetBinOpExpr(expr));
            break;
        case kHirClosureExpr:
            type = resolve_closure_expr(R, HirGetClosureExpr(expr));
            break;
        case kHirConversionExpr:
            type = resolve_conversion_expr(R, HirGetConversionExpr(expr));
            break;
        case kHirCallExpr:
            type = resolve_call_expr(R, HirGetCallExpr(expr));
            break;
        case kHirIndex:
            type = resolve_index(R, HirGetIndex(expr));
            break;
        case kHirSelector:
            type = resolve_selector(R, HirGetSelector(expr));
            break;
        case kHirAssignExpr:
            type = resolve_assign_expr(R, HirGetAssignExpr(expr));
            break;
        default:
            type = resolve_field_expr(R, HirGetFieldExpr(expr));
    }

    type = normalize(R, type);
    SET_NODE_TYPE(R->C, expr, type);
    return type;
}

static void register_if_impl(struct Resolver *R, struct HirDecl *item)
{
    if (!HirIsImplDecl(item)) return;
    struct HirImplDecl *impl = HirGetImplDecl(item);

    enter_block(R, NULL);

    allocate_decls(R, impl->generics, PAW_TRUE);
    struct IrType *type = resolve_path(R, impl->self, LOOKUP_TYPE);
    pawIr_set_type(R->C, impl->hid, type);

    leave_block(R);
}

static void resolve_item(struct Resolver *R, struct HirDecl *item)
{
    if (HirIsFuncDecl(item)) {
        resolve_func_item(R, HirGetFuncDecl(item));
    } else if (HirIsImplDecl(item)) {
        resolve_impl_item(R, HirGetImplDecl(item));
    }
}

static void resolve_items(struct Resolver *R, struct HirDeclList *items)
{
    for (int i = 0; i < items->count; ++i) {
        resolve_item(R, items->data[i]);
    }
}

static void check_module_types(struct Resolver *R, struct ModuleInfo *mod)
{
    struct Hir *hir = mod->hir;
    DLOG(R, "resolving '%s'", hir->name->text);

    R->symtab = pawHir_symtab_new(R->C);
    R->m = mod;

    enter_block(R, NULL);
    resolve_items(R, hir->items);
    leave_block(R);

    // control should not be within a scope block
    paw_assert(R->symtab->count == 0);
}

static void check_types(struct Resolver *R, struct DynamicMem *dm)
{
    enter_inference_ctx(R);
    for (int i = 0; i < dm->modules->count; ++i) {
        R->m = K_LIST_GET(dm->modules, i);
        check_module_types(R, R->m);
    }
    leave_inference_ctx(R);
}

void pawP_resolve(struct Compiler *C)
{
    struct DynamicMem *dm = C->dm;
    struct HirTypeFolder F;

    struct Resolver R = {
        .strings = C->strings,
        .impls = C->impls,
        .U = &dm->unifier,
        .P = ENV(C),
        .dm = dm,
        .F = &F,
        .C = C,
    };
    pawHir_type_folder_init(&F, NULL, &R);

    // transform AST -> HIR and add declarations for toplevel items
    pawP_lower_ast(C);

    // determine the type of each toplevel item in each module (allows pawP_lookup to
    // resolve paths pointing between modules in the next pass)
    pawP_collect_items(C);

    // run the type checker
    check_types(&R, dm);
}
