// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "ast.h"
#include "gc.h"
#include "hir.h"
#include "map.h"

static const char *kKeywords[] = {
    "pub",
    "fn",
    "type",
    "enum",
    "struct",
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
    for (size_t i = 0; i < paw_countof(kKeywords); ++i) {
        const char *kw = kKeywords[i];
        String *str = pawS_new_fixed(P, kw);
        str->flag = i + FIRST_KEYWORD;
    }
    // note that keywords are already fixed
    P->str_cache[CSTR_SELF] = pawS_new_str(P, "self");
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
}

paw_Type pawP_type2code(struct Compiler *C, struct HirType *type)
{
    if (HirIsAdt(type)) {
        struct HirAdt *adt = HirGetAdt(type);
        if (adt->base == C->builtins[BUILTIN_UNIT].did) {
            return PAW_TUNIT;
        } else if (adt->base == C->builtins[BUILTIN_BOOL].did) {
            return PAW_TBOOL;
        } else if (adt->base == C->builtins[BUILTIN_INT].did) {
            return PAW_TINT;
        } else if (adt->base == C->builtins[BUILTIN_FLOAT].did) {
            return PAW_TFLOAT;
        } else if (adt->base == C->builtins[BUILTIN_STR].did) {
            return PAW_TSTR;
        } else if (adt->base == C->builtins[BUILTIN_LIST].did) {
            return BUILTIN_LIST;
        } else if (adt->base == C->builtins[BUILTIN_MAP].did) {
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
    struct Ast *ast = C->dm->ast;
    struct AstDecl *decl = pawAst_new_decl(ast, 0, kAstAdtDecl);
    struct AstAdtDecl *r = AstGetAdtDecl(decl);
    r->name = SCAN_STRING(C, name);
    r->is_pub = PAW_TRUE;
    pawAst_decl_list_push(ast, ast->prelude, decl);
    C->builtins[kind] = (struct Builtin){
        .name = r->name,
        .did = NO_DECL,
    };
}

static void define_prelude(struct Compiler *C, const char *name, enum BuiltinKind kind)
{
    C->builtins[kind] = (struct Builtin){
        .name = SCAN_STRING(C, name),
        .did = NO_DECL,
    };
}

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, const char *modname)
{
    Value *pv = pawC_push0(P);
    Map *strings = pawH_new(P);
    V_SET_OBJECT(pv, strings);

    *C = (struct Compiler){
        .strings = strings,
        .dm = dm,
        .P = P,
    };
    P->modname = SCAN_STRING(C, modname);
    C->modname = P->modname;

    // AST is created early to store builtins
    dm->ast = pawAst_new(C);
    dm->ast->prelude = pawAst_decl_list_new(dm->ast);

    // builtin primitives
    define_basic(C, "(unit)", BUILTIN_UNIT);
    define_basic(C, "bool", BUILTIN_BOOL);
    define_basic(C, "int", BUILTIN_INT);
    define_basic(C, "float", BUILTIN_FLOAT);
    define_basic(C, "str", BUILTIN_STR);

    // builtin containers (in Paw code, _List<T> can be written as [T], and 
    // _Map<K, V> as [K: V])
    define_prelude(C, "_List", BUILTIN_LIST);
    define_prelude(C, "_Map", BUILTIN_MAP);

    // builtin enumerations
    define_prelude(C, "Option", BUILTIN_OPTION);
    define_prelude(C, "Result", BUILTIN_RESULT);
}

void pawP_teardown(paw_Env *P, const struct DynamicMem *dm)
{
    pawM_free_vec(P, dm->labels.values, dm->labels.capacity);
    pawM_free_vec(P, dm->scratch.data, dm->scratch.alloc);
    pawM_free_vec(P, dm->vars.data, dm->vars.alloc);
    pawAst_free(dm->ast);
    pawHir_free(dm->hir);
}
