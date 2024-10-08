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
#include "map.h"
#include "mem.h"
#include "parse.h"
#include "str.h"
#include "unify.h"

#define DLOG(R, ...) PAWD_LOG(ENV(R), __VA_ARGS__)
#define NAME_ERROR(R, ...) pawE_error(ENV(R), PAW_ENAME, (R)->line, __VA_ARGS__)
#define SYNTAX_ERROR(R, ...) pawE_error(ENV(R), PAW_ESYNTAX, (R)->line, __VA_ARGS__)
#define TYPE_ERROR(R, ...) pawE_error(ENV(R), PAW_ETYPE, (R)->line, __VA_ARGS__)
#define CSTR(R, i) CACHED_STRING(ENV(R), CAST_SIZE(i))
#define TYPE2CODE(R, type) (pawP_type2code((R)->C, type))
#define IS_BUILTIN_DECL(R, decl) ((decl)->hdr.did <= (R)->C->builtins[NBUILTINS - 1].did)
// TODO: IS_BUILTIN_DECL should make sure decl is an adt decl (use CHECK_EXP since it should be known from context)

struct ResultState {
    struct ResultState *outer;
    struct HirType *prev;
    int count;
};

_Noreturn static void not_a_type(struct Resolver *R, struct HirType *type)
{
    pawHir_print_type(R->m->hir, type);
    TYPE_ERROR(R, "'%s' is not a type", paw_string(ENV(R), -1));
}

static void resolve_stmt(struct Resolver *, struct HirStmt *);
static void resolve_decl(struct Resolver *, struct HirDecl *);
static struct HirType *resolve_expr(struct Resolver *, struct HirExpr *);
static struct HirType *resolve_type(struct Resolver *, struct HirType *);

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

static void resolve_type_list(struct Resolver *R, struct HirTypeList *list)
{
    if (list == NULL) return;
    for (int i = 0; i < list->count; ++i) {
        list->data[i] = resolve_type(R, list->data[i]);
    }
}

static struct HirType *normalize(struct Resolver *R, struct HirType *type)
{
    return pawU_normalize(R->U->table, type);
}

static void unify(struct Resolver *R, struct HirType *a, struct HirType *b)
{
    pawU_unify(R->U, a, b);
    normalize(R, a);
    normalize(R, b);
}

static paw_Bool is_list_t(struct Resolver *R, struct HirType *type)
{
    if (!HirIsAdt(type)) return PAW_FALSE;
    return hir_adt_base(type) == R->C->builtins[BUILTIN_LIST].did;
}

static paw_Bool is_map_t(struct Resolver *R, struct HirType *type)
{
    if (!HirIsAdt(type)) return PAW_FALSE;
    return hir_adt_base(type) == R->C->builtins[BUILTIN_MAP].did;
}

static struct HirDecl *get_decl(struct Resolver *R, DeclId did)
{
    struct DynamicMem *dm = R->dm;
    paw_assert(did < dm->decls->count);
    return dm->decls->data[did];
}

static struct HirType *get_type(struct Resolver *R, DeclId did)
{
    return get_decl(R, did)->hdr.type;
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

static paw_Bool is_unit_variant(struct Resolver *R, const struct HirType *type)
{
    if (HirIsFuncDef(type)) {
        struct HirDecl *decl = get_decl(R, type->fdef.did);
        return HirIsVariantDecl(decl) &&
            HirGetVariantDecl(decl)->fields == NULL;
    }
    return PAW_FALSE;
}

static struct HirAdtDecl *get_adt(struct Resolver *R, struct HirType *type)
{
    struct HirDecl *decl = get_decl(R, hir_adt_did(type));
    return HirGetAdtDecl(decl);
}

static struct HirDeclList *impls_for_adt(struct Compiler *C, struct HirType *adt)
{
    struct HirDecl *k = K_LIST_GET(C->dm->decls, hir_adt_base(adt));
    const Value *pv = pawH_get(C->impls, P2V(k));
    if (pv == NULL) return NULL;
    return pv->p;
}

static struct HirType *maybe_unit_variant(struct Resolver *R, struct HirType *type)
{
    if (HirIsFuncDef(type)) {
        // handle unit enumerators
        struct HirDecl *decl = get_decl(R, type->fdef.did);
        if (HirIsVariantDecl(decl)) {
            struct HirVariantDecl *d = HirGetVariantDecl(decl);
            if (d->fields == NULL) return HIR_FPTR(type)->result;
            TYPE_ERROR(R, "missing fields on enumerator");
        }
    }
    return type;
}

static struct HirType *resolve_operand(struct Resolver *R, struct HirExpr *expr)
{
    struct HirType *type = resolve_expr(R, expr);
    return maybe_unit_variant(R, type);
}

static DeclId add_decl(struct Resolver *R, struct HirDecl *decl)
{
    return pawHir_add_decl(R->C, decl);
}

static struct HirType *new_type(struct Resolver *R, DeclId did, enum HirTypeKind kind, int line)
{
    return pawHir_attach_type(R->C, did, kind, line);
}

static struct HirTypeList *resolve_exprs(struct Resolver *R, struct HirExprList *list)
{
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(R->C);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = resolve_operand(R, list->data[i]);
        pawHir_type_list_push(R->C, new_list, type);
    }
    return new_list;
}

static struct HirTypeList *collect_decl_types(struct Resolver *R, struct HirDeclList *list)
{
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(R->C);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = HIR_TYPEOF(list->data[i]);
        pawHir_type_list_push(R->C, new_list, type);
    }
    return new_list;
}

static void enter_inference_ctx(struct Resolver *R)
{
    pawU_enter_binder(R->U);
}

static void leave_inference_ctx(struct Resolver *R)
{
    pawU_leave_binder(R->U);
}

static struct HirScope *enclosing_scope(struct HirSymtab *st)
{
    paw_assert(st->count > 0);
    return pawHir_symtab_get(st, st->count - 1);
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

static struct HirSymbol *try_resolve_symbol(struct Resolver *R, const String *name)
{
    // search the scoped symbols
    struct HirSymtab *scopes = R->symtab;
    const int nscopes = scopes->count;
    for (int depth = nscopes - 1; depth >= 0; --depth) {
        struct HirScope *scope = scopes->data[depth];
        const int index = pawHir_find_symbol(scope, name);
        if (index >= 0) {
            struct HirSymbol *symbol = scope->data[index];
            return scope->data[index];
        }
    }

    // search the global symbols
    const int index = pawHir_find_symbol(R->m->globals, name);
    if (index < 0) return NULL;
    return R->m->globals->data[index];
}

static struct HirSymbol *resolve_symbol(struct Resolver *R, const String *name)
{
    struct HirSymbol *symbol = try_resolve_symbol(R, name);
    if (symbol == NULL) NAME_ERROR(R, "undefined symbol '%s'", name->text);
    return symbol;
}

static struct HirDecl *find_field(struct HirDeclList *fields, String *name)
{
    if (fields == NULL) return NULL;
    for (int i = 0; i < fields->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(fields, i);
        if (pawS_eq(name, decl->hdr.name)) return decl;
    }
    return NULL;
}

// Return true if 'a' is more generic than or equal to 'b', false otherwise
static paw_Bool is_compat(struct Compiler *C, struct HirType *a, struct HirType *b);

static paw_Bool are_lists_compat(struct Compiler *C, struct HirTypeList *a, struct HirTypeList *b)
{
    paw_assert(a->count == b->count);
    for (int i = 0; i < a->count; ++i) {
        struct HirType *x = pawHir_type_list_get(a, i);
        struct HirType *y = pawHir_type_list_get(b, i);
        if (!is_compat(C, x, y)) return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool is_compat(struct Compiler *C, struct HirType *a, struct HirType *b)
{
    if (pawU_equals(&C->dm->unifier, a, b)) return PAW_TRUE;
    if (HirIsGeneric(a)) return PAW_TRUE;
    if (HIR_KINDOF(a) != HIR_KINDOF(b)) return PAW_FALSE;

    switch (HIR_KINDOF(a)) {
        case kHirTupleType:
            if (!HirIsTupleType(b)) return PAW_FALSE;
            return are_lists_compat(C, HirGetTupleType(a)->elems, HirGetTupleType(b)->elems);
        case kHirFuncPtr:
        case kHirFuncDef:
            return is_compat(C, HIR_FPTR(a)->result, HIR_FPTR(b)->result) &&
                are_lists_compat(C, HIR_FPTR(a)->params, HIR_FPTR(b)->params);
        case kHirAdt:
            return HirGetAdt(a)->base == HirGetAdt(b)->base &&
                are_lists_compat(C, HirGetAdt(a)->types, HirGetAdt(b)->types);
        default:
            return a == b;
    }
    return PAW_FALSE;
}

static struct HirSymbol *new_local(struct Resolver *R, String *name, struct HirDecl *decl)
{
    struct HirSymbol *symbol = declare_local(R, name, decl);
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
    pawHir_symtab_push(R->C, R->symtab, scope);
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
    new_local(R, func->name, HIR_CAST_DECL(func));
}

static void ResolveBlock(struct Resolver *R, struct HirBlock *block)
{
    enter_block(R, NULL);
    resolve_stmt_list(R, block->stmts);
    leave_block(R);
}

static struct HirType *register_decl_type(struct Resolver *R, struct HirDecl *decl, enum HirTypeKind kind)
{
    const DeclId did = add_decl(R, decl);
    struct HirType *type = new_type(R, did, kind, decl->hdr.line);
    decl->hdr.type = type;
    return type;
}

static void allocate_decls(struct Resolver *R, struct HirDeclList *decls)
{
    if (decls == NULL) return;
    for (int i = 0; i < decls->count; ++i) {
        struct HirDecl *decl = decls->data[i];
        new_local(R, decl->hdr.name, decl);
    }
}

static void ResolveFieldDecl(struct Resolver *R, struct HirFieldDecl *d)
{
    add_decl(R, HIR_CAST_DECL(d));
    d->type = resolve_type(R, d->type);
}

static void resolve_func_item(struct Resolver *R, struct HirFuncDecl *d)
{
    enter_function(R, d);
    allocate_decls(R, d->generics);
    allocate_decls(R, d->params);
    d->result = resolve_type(R, d->result);

    if (d->body != NULL) {
        struct ResultState rs = {
            // named function has explicit return type
            .prev = d->result,
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
    struct HirType *want = R->rs->prev;
    struct HirType *have = NULL;
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

static void resolve_methods(struct Resolver *R, struct HirDeclList *items, struct HirAdt *self)
{
    for (int i = 0; i < items->count; ++i) {
        resolve_lazy(R, items->data[i]);
    }
}

static struct HirDecl *resolve_location(struct Resolver *R, struct HirPath *path);

static void resolve_impl_methods(struct Resolver *R, struct HirImplDecl *d)
{
    enter_block(R, NULL);

    // Lookup the 'self' ADT. If the ADT is an instance of a polymorphic ADT, it
    // may need to be instantiated here. Either way, the LazyItem holding the ADT
    // will be resolved (this is fine, since impl blocks are resolved in a separate
    // pass, after all ADTs have been registered).
    // use the identifier 'Self' to refer to the 'self' ADT
    String *name = SCAN_STRING(R, "Self");
    struct HirSymbol *symbol = new_local(R, name, R->self);
    symbol->is_type = PAW_TRUE;

    struct HirType *self_type = HIR_TYPEOF(R->self);
    resolve_methods(R, d->methods, HirGetAdt(self_type));
    allocate_decls(R, d->methods);

    leave_block(R);
}

struct HirDecl *pawP_find_field(struct Compiler *C, struct Hir *hir, struct HirAdtDecl *adt, String *name)
{
    struct HirDecl *field = find_field(adt->fields, name);
    if (field != NULL) return field;
    return pawP_find_method(C, hir, adt->type, name);
}

static struct HirDecl *expect_field(struct Resolver *R, struct HirAdtDecl *adt, String *name)
{
    struct HirDecl *result = pawP_find_field(R->C, R->m->hir, adt, name);
    if (result == NULL) NAME_ERROR(R, "field '%s' does not exist on type '%s'",
                                   name->text, adt->name->text);
    return result;
}

static void expect_bool_expr(struct Resolver *R, struct HirExpr *e)
{
    struct HirType *type = resolve_operand(R, e);
    unify(R, type, get_type(R, PAW_TBOOL));
}

static void expect_int_expr(struct Resolver *R, struct HirExpr *e)
{
    struct HirType *type = resolve_operand(R, e);
    unify(R, type, get_type(R, PAW_TINT));
}

static struct HirType *resolve_tuple_type(struct Resolver *R, struct HirTupleType *t)
{
    if (t->elems->count == 0) return get_type(R, PAW_TUNIT);
    resolve_type_list(R, t->elems);
    return HIR_CAST_TYPE(t);
}

static struct HirDecl *instantiate(struct Resolver *R, struct HirDecl *base, struct HirTypeList *types)
{
    return pawP_instantiate(R->m->hir, base, types);
}

void pawP_instantiate_impls_for(struct Hir *hir, struct HirAdtDecl *base, struct HirAdtDecl *inst, struct HirTypeList *types)
{
    struct Compiler *C = hir->C;
    struct HirDeclList *impls = impls_for_adt(C, base->type);
    if (impls == NULL) return;

    for (int i = 0; i < impls->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(impls, i);
        struct HirImplDecl *d = HirGetImplDecl(decl);
        if (is_compat(C, inst->type, d->type)) {
            pawP_instantiate(hir, decl, types);
        }
    }
}

static paw_Bool is_enum_decl(struct HirDecl *decl)
{
    return HirIsAdtDecl(decl) && !HirGetAdtDecl(decl)->is_struct;
}

static struct HirDecl *resolve_location(struct Resolver *R, struct HirPath *path)
{
    for (int i = 0; i < path->count; ++i) {
        struct HirSegment *seg = K_LIST_GET(path, i);
        resolve_type_list(R, seg->types);
    }
    // TODO: not entirely correct, need to let locals shadow toplevel items. incorporate searching local symbol table into pawP_lookup
    if (path->count == 1) {
        struct HirSegment *seg = K_LIST_GET(path, 0);
        struct HirSymbol *sym = try_resolve_symbol(R, seg->name);
        if (sym != NULL) {
            struct HirDecl *inst = pawP_instantiate(R->m->hir, sym->decl, seg->types);
            seg->modno = R->m->hir->modno;
            seg->base = sym->decl->hdr.did;
            seg->did = inst->hdr.did;
            return inst;
        }
    }
    return pawP_lookup(R->C, R->m, path);
}

static void maybe_fix_unit_struct(struct Resolver *R, struct HirDecl *decl, struct HirExpr *expr)
{
    if (HirIsAdtDecl(decl)) {
        struct HirAdtDecl *adt = HirGetAdtDecl(decl);
        if (adt->did <= PAW_TSTR) {
            goto not_operand;
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
    } else if (HirIsGenericDecl(decl)) {
not_operand:
        TYPE_ERROR(R, "expected operand but found type");
    }
}

static void resolve_path_expr(struct Resolver *R, struct HirPathExpr *e)
{
    struct HirDecl *decl = resolve_location(R, e->path);
    e->type = HIR_TYPEOF(decl);

    maybe_fix_unit_struct(R, decl, HIR_CAST_EXPR(e));
}

static void check_map_key(struct Resolver *R, struct HirType *key)
{
    if (!HirIsUnknown(key) && !HIR_IS_BASIC_T(key)) {
        TYPE_ERROR(R, "key is not hashable");
    }
}

static void resolve_logical_expr(struct Resolver *R, struct HirLogicalExpr *e)
{
    expect_bool_expr(R, e->lhs);
    expect_bool_expr(R, e->rhs);
    e->type = get_type(R, PAW_TBOOL);
}

static paw_Bool is_option_t(struct Resolver *R, struct HirType *type)
{
    return HirIsAdt(type) && hir_adt_base(type) == R->C->builtins[BUILTIN_OPTION].did;
}

static paw_Bool is_result_t(struct Resolver *R, struct HirType *type)
{
    return HirIsAdt(type) && hir_adt_base(type) == R->C->builtins[BUILTIN_RESULT].did;
}

static void resolve_chain_expr(struct Resolver *R, struct HirChainExpr *e)
{
    struct HirType *type = resolve_operand(R, e->target);
    if (R->rs->prev == NULL) {
        SYNTAX_ERROR(R, "'?' outside function body");
    }
    if (!is_option_t(R, type) && !is_result_t(R, type)) {
        SYNTAX_ERROR(R, "invalid operand for '?' operator");
    }
    unify(R, R->rs->prev, type);
    e->type = hir_adt_types(type)->data[0]; // unwrap
}

static struct HirType *get_value_type(struct Resolver *R, struct HirType *target)
{
    if (is_list_t(R, target)) {
        return hir_list_elem(target);
    } else if (is_map_t(R, target)) {
        return hir_map_value(target);
    }
    return NULL;
}

static void resolve_unop_expr(struct Resolver *R, struct HirUnOpExpr *e)
{
    static const uint8_t kValidOps[NUNARYOPS][PAW_NTYPES + 2] = {
        //     type  =  0, b, i, f, s, l, m
        [UNARY_LEN]  = {0, 0, 0, 0, 1, 1, 1},
        [UNARY_NEG]  = {0, 0, 1, 1, 0, 0, 0},
        [UNARY_NOT]  = {0, 1, 1, 1, 0, 0, 0},
        [UNARY_BNOT] = {0, 0, 1, 0, 0, 0, 0},
    };

    struct HirType *type = resolve_operand(R, e->target);
    const paw_Type code = TYPE2CODE(R, type);
    if (code < 0 || !kValidOps[e->op][code]) {
        TYPE_ERROR(R, "unsupported operand type for unary operator");
    } else if (UNOP_IS_BOOL(e->op)) {
        e->type = get_type(R, PAW_TBOOL);
    } else if (e->op == UNARY_LEN) {
        e->type = get_type(R, PAW_TINT);
    } else {
        e->type = type;
    }
}

static void resolve_binop_expr(struct Resolver *R, struct HirBinOpExpr *e)
{
    static const uint8_t kValidOps[NBINARYOPS][PAW_NTYPES + 2] = {
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

    struct HirType *lhs = resolve_operand(R, e->lhs);
    struct HirType *rhs = resolve_operand(R, e->rhs);
    unify(R, lhs, rhs);

    const paw_Type code = TYPE2CODE(R, lhs);
    paw_assert(code == TYPE2CODE(R, rhs));
    if (code < 0 || !kValidOps[e->op][code]) {
        TYPE_ERROR(R, "unsupported operand types for binary operator");
    } else if (BINOP_IS_BOOL(e->op)) {
        e->type = get_type(R, PAW_TBOOL);
    } else {
        e->type = lhs;
    }
}

static void resolve_assign_expr(struct Resolver *R, struct HirAssignExpr *e)
{
    if (!HirIsPathExpr(e->lhs) &&
            !HirIsIndex(e->lhs) &&
            !HirIsSelector(e->lhs)) {
        SYNTAX_ERROR(R, "invalid place for assignment");
    }
    struct HirType *lhs = resolve_operand(R, e->lhs);
    struct HirType *rhs = resolve_operand(R, e->rhs);
    unify(R, lhs, rhs);
    e->type = lhs;
}

static struct HirType *new_list_t(struct Resolver *R, struct HirType *elem_t)
{
    struct HirDecl *base = get_decl(R, R->C->builtins[BUILTIN_LIST].did);
    struct HirTypeList *types = pawHir_type_list_new(R->C);
    pawHir_type_list_push(R->C, types, elem_t);
    struct HirDecl *inst = instantiate(R, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirType *new_map_t(struct Resolver *R, struct HirType *key_t, struct HirType *value_t)
{
    check_map_key(R, key_t);
    struct HirDecl *base = get_decl(R, R->C->builtins[BUILTIN_MAP].did);
    struct HirTypeList *types = pawHir_type_list_new(R->C);
    pawHir_type_list_push(R->C, types, key_t);
    pawHir_type_list_push(R->C, types, value_t);
    struct HirDecl *inst = instantiate(R, base, types);
    return HIR_TYPEOF(inst);
}

static struct HirTypeList *new_unknowns(struct Resolver *R, int count)
{
    struct HirTypeList *list = pawHir_type_list_new(R->C);
    for (int i = 0; i < count; ++i) {
        struct HirType *unknown = pawU_new_unknown(R->U);
        pawHir_type_list_push(R->C, list, unknown);
    }
    return list;
}

static struct HirType *resolve_func_ptr(struct Resolver *R, struct HirFuncPtr *e)
{
    resolve_type_list(R, e->params);
    e->result = resolve_type(R, e->result);
    return HIR_CAST_TYPE(e);
}

static struct HirType *resolve_closure_param(struct Resolver *R, struct HirFieldDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    add_decl(R, decl);
    d->type = d->tag != NULL
        ? resolve_type(R, d->tag)
        : pawU_new_unknown(R->U);
    new_local(R, d->name, decl);
    return d->type;
}

static void resolve_closure_expr(struct Resolver *R, struct HirClosureExpr *e)
{
    struct ResultState rs = {R->rs};
    R->rs = &rs;
    enter_block(R, NULL);

    for (int i = 0; i < e->params->count; ++i) {
        struct HirDecl *decl = e->params->data[i];
        struct HirFieldDecl *d = HirGetFieldDecl(decl);
        resolve_closure_param(R, d);
    }
    struct HirType *ret = e->result == NULL
        ? pawU_new_unknown(R->U)
        : resolve_type(R, e->result);

    struct HirType *type = new_type(R, NO_DECL, kHirFuncPtr, e->line);
    struct HirFuncPtr *t = HirGetFuncPtr(type);
    t->params = collect_decl_types(R, e->params);
    rs.prev = e->result = t->result = ret;
    e->type = type;

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
}

// TODO: This routine needs quite a bit of work, as well as some expanatory comments
static struct HirDecl *find_method_aux(struct Compiler *C, struct Hir *hir, struct HirDecl *base, struct HirType *self, String *name)
{
    struct HirImplDecl *impl = HirGetImplDecl(base);
    struct HirDecl *adt_decl = K_LIST_GET(C->dm->decls, hir_adt_did(self));
    struct HirAdtDecl *adt = HirGetAdtDecl(adt_decl);

    if (!is_compat(C, impl->type, adt->type)) return NULL;
    if (HIR_IS_POLY_IMPL(base)) {
        struct HirTypeList *types = hir_adt_types(self);
        struct HirDecl *inst = pawP_instantiate(hir, base, types);
        impl = HirGetImplDecl(inst);
    }
    for (int j = 0; j < impl->methods->count; ++j) {
        struct HirDecl *method = K_LIST_GET(impl->methods, j);
        if (pawS_eq(name, method->hdr.name)) return method;
    }
    return NULL;
}

struct HirDecl *pawP_find_method(struct Compiler *C, struct Hir *hir, struct HirType *adt, String *name)
{
    struct HirDeclList *impls = impls_for_adt(C, adt);
    if (impls == NULL) return NULL; // no impl blocks

    for (int i = 0; i < impls->count; ++i) {
        struct HirDecl *decl = K_LIST_GET(impls, i);
        struct HirDecl *method = find_method_aux(C, hir, decl, adt, name);
        if (method != NULL) return method;
    }
    return NULL;
}

static struct HirDecl *find_method(struct Resolver *R, struct HirType *adt, String *name)
{
    struct HirDecl *result = pawP_find_method(R->C, R->m->hir, adt, name);
    if (result == NULL) {
        struct HirAdtDecl *d = get_adt(R, adt);
        NAME_ERROR(R, "method '%s' does not exist on type '%s'",
                name->text, d->name->text);
    }
    return result;
}

static void resolve_impl_item(struct Resolver *R, struct HirImplDecl *d)
{
    R->self = resolve_location(R, d->self);
    enter_block(R, NULL);

    allocate_decls(R, d->generics);
    resolve_impl_methods(R, d);

    leave_block(R);
    R->self = NULL;
}

static void resolve_var_decl(struct Resolver *R, struct HirVarDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    add_decl(R, decl);

    struct HirSymbol *symbol = declare_local(R, d->name, decl);
    struct HirType *init = resolve_operand(R, d->init);
    define_local(symbol);

    if (d->tag != NULL) {
        struct HirType *tag = resolve_type(R, d->tag);
        unify(R, init, tag);
    }
    d->type = init;
}

static void resolve_field_decl(struct Resolver *R, struct HirFieldDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    add_decl(R, decl);

    d->tag = resolve_type(R, d->tag);
    d->type = resolve_type(R, d->type);
}

static void resolve_variant_decl(struct Resolver *R, struct HirVariantDecl *d)
{
    struct HirDecl *decl = HIR_CAST_DECL(d);
    add_decl(R, decl);

    resolve_decl_list(R, d->fields);
    d->type = resolve_type(R, d->type);
}

static void resolve_type_decl(struct Resolver *R, struct HirTypeDecl *d)
{
    d->generics = NULL; // TODO: generic parameters for aliases

    struct HirSymbol *symbol = declare_local(R, d->name, HIR_CAST_DECL(d));
    d->type = resolve_operand(R, d->rhs);
    define_local(symbol);
}

static struct HirType *get_self_(struct HirDecl *func)
{
    if (HirIsFuncDecl(func)) {
        return HirGetFuncDecl(func)->is_assoc ? NULL : HirGetFuncDecl(func)->self;
    } else if (HirIsInstanceDecl(func)) {
        return HirGetInstanceDecl(func)->is_assoc ? NULL : HirGetInstanceDecl(func)->self;
    }
    return NULL;
}
#define GET_SELF(decl) get_self_(HIR_CAST_DECL(decl))

static struct HirType *infer_poly_func(struct Resolver *R, struct HirFuncDecl *base, struct HirExprList *args)
{
    struct Generalization g = pawP_generalize(R->m->hir, base->generics, base->params);
    struct HirType *self = GET_SELF(base);

    // skip 'self' parameter, which is not present in 'args'
    paw_assert(args->count == g.fields->count - !!self);
    for (int i = !!self; i < g.fields->count; ++i) {
        struct HirType *a = g.fields->data[i];
        struct HirType *b = resolve_operand(R, args->data[i]);
        unify(R, a, b);
    }
    struct HirDecl *inst = instantiate(R, HIR_CAST_DECL(base), g.types);
    return HIR_TYPEOF(inst);
}

static struct HirType *method_ctx(struct Resolver *R, struct HirType *type)
{
    if (!HirIsFuncDef(type)) return NULL;
    struct HirFuncDef *fdef = HirGetFuncDef(type);
    struct HirDecl *decl = get_decl(R, fdef->did);
    return GET_SELF(decl);
}

// Resolve a function call or enumerator constructor
static void resolve_call_expr(struct Resolver *R, struct HirCallExpr *e)
{
    e->func = resolve_expr(R, e->target);

    const struct HirFuncPtr *fptr = HIR_FPTR(e->func);
    struct HirType *self = method_ctx(R, e->func);
    const int nparams = fptr->params->count - !!self;
    if (!HirIsFuncType(e->func)) {
        TYPE_ERROR(R, "type is not callable");
    } else if (e->args->count < nparams) {
        SYNTAX_ERROR(R, "not enough arguments");
    } else if (e->args->count > nparams) {
        SYNTAX_ERROR(R, "too many arguments");
    }

    if (HirIsFuncDef(e->func)) {
        // Function type has an associated declaration. If that declaration is
        // for a function template, attempt to infer the type parameters.
        struct HirDecl *decl = get_decl(R, HirGetFuncDef(e->func)->did);
        if (HIR_IS_POLY_FUNC(decl)) {
            e->func = infer_poly_func(R, HirGetFuncDecl(decl), e->args);
            e->type = HIR_FPTR(e->func)->result; // 'fptr' is common prefix
            e->target->hdr.type = e->func; // replace generic type
            return;
        }
    }

    if (is_unit_variant(R, e->func)) {
        TYPE_ERROR(R, "cannot call unit variant (omit '()' to construct)");
    }
    const struct HirFuncPtr *func = HIR_FPTR(e->func);
    const struct HirTypeList *params = func->params;
    e->type = func->result;

    for (int i = !!self; i < params->count; ++i) {
        struct HirType *param = K_LIST_GET(params, i);
        struct HirExpr *arg = K_LIST_GET(e->args, i - !!self);
        unify(R, param, resolve_operand(R, arg));
    }
}

static void resolve_conversion_expr(struct Resolver *R, struct HirConversionExpr *e)
{
    struct HirType *type = resolve_operand(R, e->arg);
    if (!HirIsAdt(type) ||
            hir_adt_base(type) == PAW_TUNIT ||
            hir_adt_base(type) == PAW_TSTR) {
        TYPE_ERROR(R, "argument to conversion must be scalar");
    }
    e->type = get_type(R, e->to);
}

static struct HirType *resolve_basic_lit(struct Resolver *R, struct HirBasicLit *e)
{
    return get_type(R, e->t);
}

static struct HirTypeList *resolve_operand_list(struct Resolver *R, struct HirExprList *list)
{
    if (list == NULL) return NULL;
    struct HirTypeList *new_list = pawHir_type_list_new(R->C);
    for (int i = 0; i < list->count; ++i) {
        struct HirType *type = resolve_operand(R, list->data[i]);
        pawHir_type_list_push(R->C, new_list, type);
    }
    return new_list;
}

static struct HirType *resolve_tuple_lit(struct Resolver *R, struct HirTupleLit *e, int line)
{
    struct HirType *type = new_type(R, NO_DECL, kHirTupleType, line);
    HirGetTupleType(type)->elems = resolve_operand_list(R, e->elems);
    return type;
}

static struct HirType *resolve_list_lit(struct Resolver *R, struct HirContainerLit *e)
{
    struct Unifier *U = R->U;
    struct HirType *elem_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct HirExpr *expr = e->items->data[i];
        struct HirType *type = resolve_expr(R, expr);
        unify(R, type, elem_t);
        expr->hdr.type = elem_t;
    }
    return new_list_t(R, elem_t);
}

static struct HirType *resolve_map_lit(struct Resolver *R, struct HirContainerLit *e)
{
    struct Unifier *U = R->U;
    struct HirType *key_t = pawU_new_unknown(U);
    struct HirType *value_t = pawU_new_unknown(U);
    for (int i = 0; i < e->items->count; ++i) {
        struct HirExpr *expr = e->items->data[i];
        struct HirFieldExpr *field = HirGetFieldExpr(expr);
        paw_assert(field->fid == -1);
        struct HirType *k = resolve_operand(R, field->key);
        struct HirType *v = resolve_operand(R, field->value);
        unify(R, k, key_t);
        unify(R, v, value_t);
        field->type = value_t;
    }
    return new_map_t(R, key_t, value_t);
}

static struct HirType *resolve_container_lit(struct Resolver *R, struct HirContainerLit *e)
{
    if (e->code == BUILTIN_LIST) {
        return resolve_list_lit(R, e);
    } else {
        paw_assert(e->code == BUILTIN_MAP);
        return resolve_map_lit(R, e);
    }
}

static void resolve_field_expr(struct Resolver *R, struct HirFieldExpr *e)
{
    if (e->fid < 0) resolve_expr(R, e->key);
    e->type = resolve_operand(R, e->value);
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
        pawHir_expr_list_push(R->C, order, expr);
    }
    return order;
}

static paw_Bool is_adt_self(struct Resolver *R, struct HirDecl *adt)
{
    if (R->self != NULL) {
        struct HirType *self = HIR_TYPEOF(R->self);
        struct HirType *type = HIR_TYPEOF(adt);
        return is_compat(R->C, self, type);
    }
    return PAW_FALSE;
}

static void ensure_accessible_field(struct Resolver *R, struct HirDecl *field, struct HirDecl *adt)
{
    const String *name = field->hdr.name;
    const paw_Bool is_pub = HirIsFieldDecl(field) ? HirGetFieldDecl(field)->is_pub :
        HirIsFuncDecl(field) ? HirGetFuncDecl(field)->is_pub : HirGetInstanceDecl(field)->is_pub;
    if (is_pub || is_adt_self(R, adt)) return; // field is public or control is inside own impl block
    NAME_ERROR(R, "'%s.%s' is not a public field", HirGetAdtDecl(adt)->name->text, name->text);
}

static struct HirType *resolve_composite_lit(struct Resolver *R, struct HirCompositeLit *e)
{
    struct HirDecl *decl = resolve_location(R, e->path);
    if (!HirIsAdtDecl(decl)) TYPE_ERROR(R, "expected structure type");

    // Use a temporary Map to avoid searching repeatedly through the list of
    // fields.
    paw_Env *P = ENV(R);
    ENSURE_STACK(P, 1);
    Value *pv = pawC_push0(P);
    Map *map = pawH_new(P);
    V_SET_OBJECT(pv, map);

    Value key;
    struct Generalization g;
    struct HirTypeList *field_types = NULL;
    paw_Bool is_inference = PAW_FALSE;
    struct HirType *target = HIR_TYPEOF(decl);
    struct HirAdtDecl *adt = HirGetAdtDecl(decl);
    if (!adt->is_struct) {
        TYPE_ERROR(R, "expected structure but found enumeration '%s'", adt->name->text);
    } else if (adt->fields == NULL) {
        SYNTAX_ERROR(R, "unexpected curly braces on initializer for unit structure '%s'"
                        "(use name without '{}' to create unit struct)", adt->name->text);
    } else if (HIR_IS_POLY_ADT(decl)) {
        g = pawP_generalize(R->m->hir, adt->generics, adt->fields);
        is_inference = PAW_TRUE;
        field_types = g.fields;
    }
    struct HirExprList *order = collect_field_exprs(R, e->items, map, adt->name);
    for (int i = 0; i < adt->fields->count; ++i) {
        struct HirDecl *field_decl = K_LIST_GET(adt->fields, i);
        struct HirFieldDecl *field = HirGetFieldDecl(field_decl);
        ensure_accessible_field(R, field_decl, decl);
        V_SET_OBJECT(&key, field->name);
        Value *value = pawH_get(map, key);
        if (value == NULL) {
            NAME_ERROR(R, "missing initializer for field '%s' in struct '%s'",
                       field->name->text, adt->name->text);
        }
        struct HirType *type = is_inference
            ? field_types->data[i]
            : HIR_TYPEOF(adt->fields->data[i]);
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
    pawC_pop(P); // pop map

    if (is_inference) {
        struct HirDecl *inst = instantiate(R, decl, g.types);
        target = HIR_TYPEOF(inst);
    }
    e->items = order;
    return target;
}

static void resolve_literal_expr(struct Resolver *R, struct HirLiteralExpr *e)
{
    if (e->lit_kind == kHirLitBasic) {
        e->type = resolve_basic_lit(R, &e->basic);
    } else if (e->lit_kind == kHirLitTuple) {
        e->type = resolve_tuple_lit(R, &e->tuple, e->line);
    } else if (e->lit_kind == kHirLitContainer) {
        e->type = resolve_container_lit(R, &e->cont);
    } else {
        paw_assert(e->lit_kind == kHirLitComposite);
        e->type = resolve_composite_lit(R, &e->comp);
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
    resolve_expr(R, s->expr);
}

static void ResolveWhileStmt(struct Resolver *R, struct HirWhileStmt *s)
{
    enter_block(R, NULL);
    expect_bool_expr(R, s->cond);
    ResolveBlock(R, s->block);
    leave_block(R);
}

static void visit_forbody(struct Resolver *R, struct HirType *itype, struct HirBlock *b, struct HirForStmt *s)
{
    add_decl(R, s->control);
    struct HirVarDecl *control = HirGetVarDecl(s->control);
    control->type = itype;

    enter_block(R, NULL);
    new_local(R, control->name, s->control);
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

// TODO: allow function with signature fn iter<I, T>(I) -> (fn(int) -> T)
static void visit_forin(struct Resolver *R, struct HirForStmt *s)
{
    struct HirType *iter_t = resolve_operand(R, s->forin.target);
    struct HirType *elem_t = get_value_type(R, iter_t);

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

static void check_index(struct Resolver *R, struct HirIndex *e, struct HirType *target)
{
    struct HirType *expect = NULL;
    if (is_list_t(R, target)) {
        expect = get_type(R, PAW_TINT);
        e->type = e->is_slice ? target : hir_list_elem(target);
    } else if (is_map_t(R, target)) {
        if (e->is_slice) {
            TYPE_ERROR(R, "slice operation not supported on map "
                          "(requires '[T]' or 'str')");
        }
        expect = hir_map_key(target);
        e->type = hir_map_value(target);
    } else if (HirIsAdt(target) &&
            hir_adt_base(target) == PAW_TSTR) {
        expect = get_type(R, PAW_TINT);
        e->type = get_type(R, PAW_TSTR);
    } else {
        TYPE_ERROR(R, "type cannot be indexed (not a container)");
    }
    if (e->is_slice) {
        if (e->first != NULL) {
            struct HirType *first = resolve_operand(R, e->first);
            unify(R, expect, first);
        }
        if (e->second != NULL) {
            struct HirType *second = resolve_operand(R, e->second);
            unify(R, expect, second);
        }
    } else {
        struct HirType *first = resolve_operand(R, e->first);
        unify(R, expect, first);
    }
}

static void resolve_index(struct Resolver *R, struct HirIndex *e)
{
    struct HirType *target = resolve_operand(R, e->target);
    check_index(R, e, target);
}

static void resolve_selector(struct Resolver *R, struct HirSelector *e)
{
    struct HirType *target = resolve_operand(R, e->target);
    if (HirIsTupleType(target)) {
        // tuples are indexed with "Expr" "." "int_lit"
        struct HirTypeList *types = target->tuple.elems;
        if (!e->is_index) {
            TYPE_ERROR(R, "expected index of tuple element");
        } else if (e->index >= types->count) {
            TYPE_ERROR(R, "element index %d out of range of %d-tuple",
                       e->index, types->count);
        }
        e->type = types->data[e->index];
        return;
    }
    if (!HirIsAdt(target)) TYPE_ERROR(R, "type has no fields");
    if (e->is_index) TYPE_ERROR(R, "expected field name (integer indices can "
                                   "only be used with tuples)");
    struct HirAdtDecl *adt = get_adt(R, target);
    struct HirDecl *field = expect_field(R, adt, e->name);
    ensure_accessible_field(R, field, HIR_CAST_DECL(adt));
    e->type = HIR_TYPEOF(field);
}

static void ResolveLabelStmt(struct Resolver *R, struct HirLabelStmt *s)
{
    PAW_UNUSED(R);
    PAW_UNUSED(s);
}

static void ResolveDeclStmt(struct Resolver *R, struct HirDeclStmt *s)
{
    resolve_decl(R, s->decl);
}

static void resolve_decl(struct Resolver *R, struct HirDecl *decl)
{
    R->line = decl->hdr.line;
    switch (HIR_KINDOF(decl)) {
        case kHirVarDecl:
            resolve_var_decl(R, HirGetVarDecl(decl));
            break;
        case kHirVariantDecl:
            resolve_variant_decl(R, HirGetVariantDecl(decl));
            break;
        default:
            resolve_field_decl(R, HirGetFieldDecl(decl));
    }
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

static struct HirType *resolve_type(struct Resolver *R, struct HirType *type)
{
    if (type == NULL) return NULL;

    struct HirType *r;
    R->line = type->hdr.line;
    switch (HIR_KINDOF(type)) {
        default:
            TYPE_ERROR(R, "expected type"); // no return
        case kHirPathType: {
            struct HirPathType *path = HirGetPathType(type);
            struct HirDecl *decl = resolve_location(R, path->path);
            if (!HirIsAdtDecl(decl) && !HirIsGenericDecl(decl)) {
                not_a_type(R, type);
            }
            r = HIR_TYPEOF(decl);
            break;
        }
        case kHirFuncPtr:
        case kHirFuncDef:
            r = resolve_func_ptr(R, HIR_FPTR(type));
            break;
        case kHirTupleType:
            r = resolve_tuple_type(R, HirGetTupleType(type));
            break;
        case kHirGeneric:
        case kHirUnknown:
        case kHirAdt:
            r = type;
    }
    return normalize(R, r);
}

static struct HirType *resolve_expr(struct Resolver *R, struct HirExpr *expr)
{
    R->line = expr->hdr.line;
    switch (HIR_KINDOF(expr)) {
        case kHirLiteralExpr:
            resolve_literal_expr(R, HirGetLiteralExpr(expr));
            break;
        case kHirLogicalExpr:
            resolve_logical_expr(R, HirGetLogicalExpr(expr));
            break;
        case kHirPathExpr:
            resolve_path_expr(R, HirGetPathExpr(expr));
            break;
        case kHirChainExpr:
            resolve_chain_expr(R, HirGetChainExpr(expr));
            break;
        case kHirUnOpExpr:
            resolve_unop_expr(R, HirGetUnOpExpr(expr));
            break;
        case kHirBinOpExpr:
            resolve_binop_expr(R, HirGetBinOpExpr(expr));
            break;
        case kHirClosureExpr:
            resolve_closure_expr(R, HirGetClosureExpr(expr));
            break;
        case kHirConversionExpr:
            resolve_conversion_expr(R, HirGetConversionExpr(expr));
            break;
        case kHirCallExpr:
            resolve_call_expr(R, HirGetCallExpr(expr));
            break;
        case kHirIndex:
            resolve_index(R, HirGetIndex(expr));
            break;
        case kHirSelector:
            resolve_selector(R, HirGetSelector(expr));
            break;
        case kHirAssignExpr:
            resolve_assign_expr(R, HirGetAssignExpr(expr));
            break;
        default:
            resolve_field_expr(R, HirGetFieldExpr(expr));
    }

    expr->hdr.type = normalize(R, HIR_TYPEOF(expr));
    return HIR_TYPEOF(expr);
}

static void register_if_impl(struct Resolver *R, struct HirDecl *item)
{
    if (!HirIsImplDecl(item)) return;
    struct HirImplDecl *impl = HirGetImplDecl(item);

    enter_block(R, NULL);

    allocate_decls(R, impl->generics);
    struct HirDecl *self = resolve_location(R, impl->self);
    impl->type = HIR_TYPEOF(self);

    leave_block(R);
}

static void monomorphize_functions(struct Resolver *R)
{
    struct ModuleList *modules = R->dm->modules;
    int nexpand;
    do {
        nexpand = 0;
        for (int i = 0; i < modules->count; ++i) {
            struct ModuleInfo *m = K_LIST_GET(modules, i);
            nexpand += pawHir_expand_bodies(m->hir);
        }
    } while (nexpand > 0);
}

static void resolve_item(struct Resolver *R, struct HirDecl *item)
{
    // TODO: should have been resolved by collect.c item->hdr.type = resolve_type(R, HIR_TYPEOF(item));
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

static DeclId find_builtin(struct Resolver *R, String *name)
{
    const struct HirSymbol *symbol = resolve_symbol(R, name);
    return symbol->decl->hdr.did;
}

static void resolve_module(struct Resolver *R, struct ModuleInfo *mod)
{
    struct Hir *hir = mod->hir;
    DLOG(R, "resolving '%s'", hir->name->text);

    R->symtab = pawHir_symtab_new(R->C);
    R->m = mod;

    enter_block(R, NULL);
    enter_inference_ctx(R);

    resolve_items(R, hir->items);

    leave_inference_ctx(R);
    leave_block(R);

    // control should not be within a scope block
    paw_assert(R->symtab->count == 0);
}

void pawP_resolve(struct Compiler *C)
{
    paw_Env *P = ENV(C);
    ENSURE_STACK(P, 1);

    struct DynamicMem *dm = C->dm;
    struct Unifier *U = &dm->unifier;

    struct Resolver R = {
        .strings = C->strings,
        .impls = C->impls,
        .P = ENV(C),
        .dm = dm,
        .U = U,
        .C = C,
    };

    // transform AST -> HIR and add declarations for toplevel items
    pawP_lower_ast(C);

    // determine the type of each toplevel item in each module (allows pawP_lookup to
    // resolve paths pointing between modules in the next pass)
    pawP_collect_items(C);

    // run the type checker
    for (int i = 0; i < dm->modules->count; ++i) {
        R.m = K_LIST_GET(dm->modules, i);
        resolve_module(&R, R.m);
    }

    // monomorphize polymorphic functions
    monomorphize_functions(&R);
}
