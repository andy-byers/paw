// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "api.h"
#include "ast.h"
#include "gc.h"
#include "hir.h"
#include "map.h"

// All paw language keywords
//
// ORDER TokenKind
static const char *kKeywords[] = {
    "pub",
    "use",
    "fn",
    "type",
    "enum",
    "struct",
    "impl",
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
    "as",
    "true",
    "false",
};

static String *basic_type_name(paw_Env *P, const char *name, paw_Type code)
{
    String *s = pawS_new_fixed(P, name);
    s->flag = FLAG2CODE(code); // works either direction
    return s;
}

void pawP_init(paw_Env *P)
{
    // Add all keywords to the interned strings table. Fix them so they are
    // never collected. Also added to the lexer string map.
    for (size_t i = 0; i < PAW_COUNTOF(kKeywords); ++i) {
        const char *kw = kKeywords[i];
        String *str = pawS_new_fixed(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }
    // note that keywords are already fixed
    P->str_cache[CSTR_TRUE] = pawS_new_str(P, "true");
    P->str_cache[CSTR_FALSE] = pawS_new_str(P, "false");
    P->str_cache[CSTR_BOOL] = basic_type_name(P, "bool", PAW_TBOOL);
    P->str_cache[CSTR_INT] = basic_type_name(P, "int", PAW_TINT);
    P->str_cache[CSTR_FLOAT] = basic_type_name(P, "float", PAW_TFLOAT);
    P->str_cache[CSTR_STR] = basic_type_name(P, "str", PAW_TSTR);
    P->str_cache[CSTR_LIST] = pawS_new_fixed(P, "_List");
    P->str_cache[CSTR_MAP] = pawS_new_fixed(P, "_Map");
    P->str_cache[CSTR_OPTION] = pawS_new_fixed(P, "Option");
    P->str_cache[CSTR_RESULT] = pawS_new_fixed(P, "Result");
    P->str_cache[CSTR_SELF] = pawS_new_fixed(P, "self");
}

paw_Type pawP_type2code(struct Compiler *C, struct HirType *type)
{
    if (HirIsPathType(type)) {
        const DeclId base = hir_adt_base(type);
        if (base == C->builtins[BUILTIN_UNIT].did) {
            return PAW_TUNIT;
        } else if (base == C->builtins[BUILTIN_BOOL].did) {
            return PAW_TBOOL;
        } else if (base == C->builtins[BUILTIN_INT].did) {
            return PAW_TINT;
        } else if (base == C->builtins[BUILTIN_FLOAT].did) {
            return PAW_TFLOAT;
        } else if (base == C->builtins[BUILTIN_STR].did) {
            return PAW_TSTR;
        } else if (base == C->builtins[BUILTIN_LIST].did) {
            return BUILTIN_LIST;
        } else if (base == C->builtins[BUILTIN_MAP].did) {
            return BUILTIN_MAP;
        }
    }
    return -1;
}

String *pawP_scan_nstring(paw_Env *P, Map *st, const char *s, size_t n)
{
    const Value *pv = pawC_pushns(P, s, n);
    Value *value = pawH_create(P, st, *pv);
    *value = *pv; // anchor in map
    pawC_pop(P);
    CHECK_GC(P);

    return V_STRING(*value);
}

static void define_basic(struct Compiler *C, const char *name, enum BuiltinKind kind)
{
    struct Ast *ast = C->prelude;
    struct AstDecl *decl = pawAst_new_decl(ast, 0, kAstAdtDecl);
    struct AstAdtDecl *r = AstGetAdtDecl(decl);
    r->name = SCAN_STRING(C, name);
    r->is_pub = PAW_TRUE;
    pawAst_decl_list_push(ast, ast->items, decl);
    C->builtins[kind] = (struct Builtin){
        .name = r->name,
        .did = NO_DECL,
    };
}

static void define_adt(struct Compiler *C, const char *name, enum BuiltinKind kind)
{
    C->builtins[kind] = (struct Builtin){
        .name = SCAN_STRING(C, name),
        .did = NO_DECL,
    };
}

#define FIRST_ARENA_SIZE 4096
#define LARGE_ARENA_MIN (32 * sizeof(void *))

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, const char *modname)
{
    ENSURE_STACK(P, 4);

    // '.strings' anchors all strings used during compilation so they are not
    // collected by the GC.
    Map *strings = pawH_new(P);
    V_SET_OBJECT(P->top.p++, strings);

    Map *types = pawH_new(P);
    V_SET_OBJECT(P->top.p++, types);

    // '.imports' maps modules names to ASTs for each module being compiled.
    Map *imports = pawH_new(P);
    V_SET_OBJECT(P->top.p++, imports);

    // '.impls' maps each ADT to a list containing all of their 'impl' blocks. 
    // Includes ADTs from all modules being compiled.
    Map *impls = pawH_new(P);
    V_SET_OBJECT(P->top.p++, impls);

    pawK_pool_init(P, &dm->pool, FIRST_ARENA_SIZE, LARGE_ARENA_MIN);

    *C = (struct Compiler){
        .pool = &dm->pool,
        .imports = imports,
        .strings = strings,
        .impls = impls,
        .types = types,
        .dm = dm,
        .P = P,
    };
    P->modname = SCAN_STRING(C, modname);
    C->modname = P->modname;

    pawP_set_instantiate(C, PAW_FALSE);
    C->prelude = pawAst_new(C, SCAN_STRING(C, "prelude"), 0);

    C->dm->modules = pawP_mod_list_new(C);
    C->dm->unifier.C = C;

    // builtin primitives
    define_basic(C, "(unit)", BUILTIN_UNIT);
    define_basic(C, "bool", BUILTIN_BOOL);
    define_basic(C, "int", BUILTIN_INT);
    define_basic(C, "float", BUILTIN_FLOAT);
    define_basic(C, "str", BUILTIN_STR);

    // builtin containers (in Paw code, _List<T> can be written as [T], and 
    // _Map<K, V> as [K: V])
    define_adt(C, "_List", BUILTIN_LIST);
    define_adt(C, "_Map", BUILTIN_MAP);

    // builtin enumerations
    define_adt(C, "Option", BUILTIN_OPTION);
    define_adt(C, "Result", BUILTIN_RESULT);
}

void pawP_teardown(paw_Env *P, struct DynamicMem *dm)
{
    pawK_pool_uninit(P, &dm->pool);
    pawM_free_vec(P, dm->labels.values, dm->labels.capacity);
    pawM_free_vec(P, dm->scratch.data, dm->scratch.alloc);
    pawM_free_vec(P, dm->vars.data, dm->vars.alloc);
}

struct ModuleInfo *pawP_mi_new(struct Compiler *C, struct Hir *hir)
{
    struct ModuleInfo *mi = pawK_pool_alloc(ENV(C), C->pool, sizeof(*mi));
    *mi = (struct ModuleInfo){
        .globals = pawHir_scope_new(hir),
        .hir = hir,
    };
    return mi;
}
