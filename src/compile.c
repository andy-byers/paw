// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "api.h"
#include "ast.h"
#include "debug.h"
#include "gc.h"
#include "hir.h"
#include "map.h"
#include "type.h"

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
    P->string_cache[CSTR_TRUE] = pawS_new_str(P, "true");
    P->string_cache[CSTR_FALSE] = pawS_new_str(P, "false");
    P->string_cache[CSTR_BOOL] = basic_type_name(P, "bool", PAW_TBOOL);
    P->string_cache[CSTR_INT] = basic_type_name(P, "int", PAW_TINT);
    P->string_cache[CSTR_FLOAT] = basic_type_name(P, "float", PAW_TFLOAT);
    P->string_cache[CSTR_STR] = basic_type_name(P, "str", PAW_TSTR);
    P->string_cache[CSTR_LIST] = pawS_new_fixed(P, "_List");
    P->string_cache[CSTR_MAP] = pawS_new_fixed(P, "_Map");
    P->string_cache[CSTR_OPTION] = pawS_new_fixed(P, "Option");
    P->string_cache[CSTR_RESULT] = pawS_new_fixed(P, "Result");
    P->string_cache[CSTR_SELF] = pawS_new_fixed(P, "self");

    P->string_cache[CSTR_KBUILTIN] = pawS_new_fixed(P, "paw.builtin");
    P->string_cache[CSTR_KMODULES] = pawS_new_fixed(P, "paw.modules");
    P->string_cache[CSTR_KSEARCHERS] = pawS_new_fixed(P, "paw.searchers");
}

paw_Type pawP_type2code(struct Compiler *C, struct HirType *type)
{
    if (HirIsAdt(type)) {
        const DeclId base = hir_adt_did(type);
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

static void define_prelude_adt(struct Compiler *C, const char *name, enum BuiltinKind kind)
{
    struct Ast *ast = C->prelude;
    struct AstDecl *decl = pawAst_new_decl(ast, 0, kAstAdtDecl);
    struct AstAdtDecl *r = AstGetAdtDecl(decl);
    r->name = SCAN_STRING(C, name);
    r->is_pub = PAW_TRUE;
    r->is_struct = PAW_TRUE;
    pawAst_decl_list_push(C, ast->items, decl);
    C->builtins[kind] = (struct Builtin){
        .name = r->name,
        .did = NO_DECL,
    };
}

static void define_prelude_poly_adt(struct Compiler *C, const char *name, enum BuiltinKind kind)
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
    pawY_uninit(P);

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
        .U = &dm->unifier,
        .impls = impls,
        .types = types,
        .dm = dm,
        .P = P,
    };
    P->modname = SCAN_STRING(C, modname);
    C->modname = P->modname;

    C->prelude = pawAst_new(C, SCAN_STRING(C, "prelude"), 0);

    C->dm->decls = pawHir_decl_list_new(C);
    C->dm->modules = pawP_mod_list_new(C);
    C->dm->unifier.C = C;

    // builtin primitives
    define_prelude_adt(C, "(unit)", BUILTIN_UNIT);
    define_prelude_adt(C, "bool", BUILTIN_BOOL);
    define_prelude_adt(C, "int", BUILTIN_INT);
    define_prelude_adt(C, "float", BUILTIN_FLOAT);
    define_prelude_adt(C, "str", BUILTIN_STR);

    // builtin containers (in Paw code, _List<T> can be written as [T], and
    // _Map<K, V> as [K: V])
    define_prelude_poly_adt(C, "_List", BUILTIN_LIST);
    define_prelude_poly_adt(C, "_Map", BUILTIN_MAP);

    // builtin enumerations
    define_prelude_poly_adt(C, "Option", BUILTIN_OPTION);
    define_prelude_poly_adt(C, "Result", BUILTIN_RESULT);
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
        .globals = pawHir_scope_new(C),
        .hir = hir,
    };
    return mi;
}

struct DefState {
    struct DefState *outer;
    struct HirDecl *decl;
    struct Def *def;
};

struct DefGenerator {
    struct DefState *ds;
    struct ModuleInfo *m;
    struct ItemList *items;
    struct Compiler *C;
    Map *adts;
    int ntypes;
    int nvals;
};

static void enter_def_(struct DefGenerator *dg, struct DefState *ds, struct HirDecl *decl, struct Def *def)
{
    *ds = (struct DefState){
        .outer = dg->ds,
        .decl = decl,
        .def = def,
    };
    dg->ds = ds;
}

#define ENTER_DEF(dg, ds, decl, def) enter_def_(dg, ds, HIR_CAST_DECL(decl), CAST(struct Def *, def))
#define LEAVE_DEF(dg) ((dg)->ds = (dg)->ds->outer)

static struct Type *create_type(struct DefGenerator *dg, struct HirType *type);
static paw_Bool match_types(struct DefGenerator *dg, struct HirType *lhs, paw_Type rhs);

static paw_Bool match_type_lists(struct DefGenerator *dg, struct HirTypeList *lhs, paw_Type *rhs, int nrhs)
{
    if (lhs->count != nrhs) return PAW_FALSE;
    for (int i = 0; i < lhs->count; ++i) {
        if (!match_types(dg, lhs->data[i], rhs[i])) return PAW_FALSE;
    }
    return PAW_TRUE;
}

static paw_Bool match_functions(struct DefGenerator *dg, struct HirType *lhs, struct Type *rhs)
{
    if (!HirIsFuncType(lhs) || rhs->hdr.kind != TYPE_SIGNATURE) return PAW_FALSE;
    return match_type_lists(dg, HIR_FPTR(lhs)->params, rhs->subtypes, rhs->nsubtypes) &&
        match_types(dg, HIR_FPTR(lhs)->result, rhs->sig.result);
}

static paw_Bool match_tuples(struct DefGenerator *dg, struct HirType *lhs, struct Type *rhs)
{
    if (!HirIsTupleType(lhs) || rhs->hdr.kind != TYPE_TUPLE) return PAW_FALSE;
    return match_type_lists(dg, HirGetTupleType(lhs)->elems, rhs->subtypes, rhs->nsubtypes);
}

static paw_Bool match_adts(struct DefGenerator *dg, struct HirType *lhs, struct Type *rhs)
{
    if (!HirIsAdt(lhs) || rhs->hdr.kind != TYPE_ADT) return PAW_FALSE;
    struct HirDecl *decl = dg->items->data[rhs->adt.did]->decl;
    return HirIsAdtDecl(decl) && HirGetAdt(HIR_TYPEOF(decl))->did == HirGetAdt(lhs)->did;
}

static paw_Bool match_types(struct DefGenerator *dg, struct HirType *lhs, paw_Type rhs_code)
{
    struct Type *rhs = Y_TYPE(ENV(dg->C), rhs_code);
    return match_functions(dg, lhs, rhs) ||
        match_tuples(dg, lhs, rhs) ||
        match_adts(dg, lhs, rhs);
}

static struct Type *lookup_type(struct DefGenerator *dg, struct HirType *type)
{
    if (HirIsAdt(type)) {
        type = pawP_instantiate(dg->C, pawHir_get_decl(dg->C, type->adt.did), type->adt.types);
    } else if (HirIsFuncDef(type)) {
        type = pawP_instantiate(dg->C, pawHir_get_decl(dg->C, type->fdef.did), type->fdef.types);
    }
    Value *pv = pawH_get(dg->C->types, P2V(type));
    return pv != NULL ? pv->p : NULL;
}

static void map_types(struct DefGenerator *dg, struct HirType *src, struct Type *dst)
{
    paw_Env *P = ENV(dg->C);
    paw_assert(lookup_type(dg, src) == NULL);
    pawH_insert(P, dg->C->types, P2V(src), P2V(dst));
}

static struct Type *search_type(struct DefGenerator *dg, struct HirType *target)
{
    paw_Env *P = ENV(dg->C);
    // check if this particular HirType has been seen already
    struct Type *type = lookup_type(dg, target);
    if (type != NULL) return type;
    // check if there is an existing type that is equivalent to this one
    for (paw_Type t = 0; t < dg->ntypes; ++t) {
        if (match_types(dg, target, t)) {
            struct Type *type = Y_TYPE(P, t);
            map_types(dg, target, type);
            return type;
        }
    }
    return NULL;
}

static struct Type *new_type(struct DefGenerator *, struct HirType *);
#define MAKE_TYPE(dg, t) new_type(dg, HIR_CAST_TYPE(t))->hdr.code

static void init_type_list(struct DefGenerator *dg, struct HirTypeList *x, paw_Type *y)
{
    if (x == NULL) return;
    for (int i = 0; i < x->count; ++i) {
        y[i] = MAKE_TYPE(dg, x->data[i]);
    }
}

static struct Type *new_type(struct DefGenerator *dg, struct HirType *src)
{
    struct Type *dst = search_type(dg, src);
    if (dst != NULL) return dst;

    paw_Env *P = ENV(dg->C);
    switch (HIR_KINDOF(src)) {
        case kHirAdt:
            dst = lookup_type(dg, src);
            paw_assert(dst != NULL);
            return dst;
        case kHirFuncDef:
        case kHirFuncPtr: {
            struct HirFuncPtr *fptr = HIR_FPTR(src);
            dst = pawY_new_signature(P, fptr->params->count);
            init_type_list(dg, fptr->params, dst->subtypes);
            dst->sig.result = MAKE_TYPE(dg, fptr->result);
            break;
        }
        default: { // kHirTupleType
            struct HirTupleType *tuple = HirGetTupleType(src);
            dst = pawY_new_tuple(P, tuple->elems->count);
            init_type_list(dg, tuple->elems, dst->subtypes);
        }
    }
    map_types(dg, src, dst);
    ++dg->ntypes;
    return dst;
}

static paw_Bool handle_monos(struct HirVisitor *V, struct HirDeclList *monos)
{
    if (monos == NULL) return PAW_FALSE;
    for (int i = 0; i < monos->count; ++i) {
        pawHir_visit_decl(V, monos->data[i]);
    }
    return PAW_TRUE;
}

static paw_Bool has_generic(struct HirType *type)
{
    if (HirIsGeneric(type)) return PAW_TRUE;
    struct HirTypeList *types =
        HirIsFuncDef(type) ? HirGetFuncDef(type)->types :
        HirIsTupleType(type) ? HirGetTupleType(type)->elems :
        HirIsAdt(type) ? hir_adt_types(type) : NULL;
    if (types == NULL) return PAW_FALSE;
    for (int i = 0; i < types->count; ++i) {
        struct HirType *type = K_LIST_GET(types, i);
        if (has_generic(type)) return PAW_TRUE;
    }
    return PAW_FALSE;
}

#define LEN(L) ((L) != NULL ? (L)->count : 0)

static struct Def *new_def_(struct DefGenerator *dg, struct HirDecl *decl, struct HirType *type)
{
    struct Def *def;
    paw_Env *P = ENV(dg->C);

    switch (HIR_KINDOF(decl)) {
        case kHirVarDecl:
        case kHirFieldDecl:
            def = pawY_new_var_def(P);
            break;
        case kHirFuncDecl:
            def = pawY_new_func_def(P, LEN(HirGetFuncDecl(decl)->params));
            break;
        case kHirAdtDecl:
            def = pawY_new_adt_def(P, LEN(HirGetAdtDecl(decl)->fields));
            break;
        default: // kHirVariantDecl
            def = pawY_new_func_def(P, LEN(HirGetVariantDecl(decl)->fields));
    }
    type = type != NULL ? type : HIR_TYPEOF(decl);

    def->hdr.name = decl->hdr.name;
    def->hdr.code = MAKE_TYPE(dg, type);
    def->hdr.modname = dg->m->hir->name;
    def->hdr.did = dg->items->count;

    struct ItemSlot *item = pawP_new_item_slot(dg->C, decl, type, dg->m);
    pawP_item_list_push(dg->C, dg->items, item);
    return def;
}

#define NEW_DEF(dg, decl, type) new_def_(dg, HIR_CAST_DECL(decl), type)

static void register_fdef_types(struct DefGenerator *dg, struct HirFuncDef *fdef)
{
    if (fdef->types == NULL) return;
    for (int i = 0; i < fdef->types->count; ++i) {
        new_type(dg, fdef->types->data[i]);
    }
}

static paw_Bool define_func_decl(struct HirVisitor *V, struct HirFuncDecl *d)
{
    struct DefGenerator *dg = V->ud;
    if (handle_monos(V, d->monos)) return PAW_FALSE;
    if (has_generic(d->type)) return PAW_FALSE;
    register_fdef_types(dg, HirGetFuncDef(d->type));

    paw_Env *P = ENV(dg->C);
    struct Def *result = NEW_DEF(dg, d, NULL);
    struct FuncDef *def = &result->func;
    def->is_pub = d->is_pub;
    def->vid = dg->nvals++; // allocate value slot

    struct DefState ds;
    ENTER_DEF(dg, &ds, d, def);
    pawHir_visit_decl_list(V, d->params);
    LEAVE_DEF(dg);

    // C functions don't have a 'body' field
    if (d->body == NULL) return PAW_FALSE;
    pawHir_visit_block(V, d->body);
    return PAW_FALSE;
}

static void define_adt(struct HirVisitor *V, struct HirAdtDecl *d, struct HirType *type)
{
    struct DefGenerator *dg = V->ud;
    const Value *pv = pawH_get(dg->adts, P2V(type));
    if (pv == NULL) return; // not cannonical
    if (has_generic(type)) return;
    paw_Env *P = ENV(dg->C);

    struct Type *y = lookup_type(dg, type);
    paw_assert(y != NULL);
    struct Def *result = Y_DEF(P, y->adt.did);
    struct AdtDef *r = &result->adt;

    struct HirAdt *adt = HirGetAdt(type);
    init_type_list(dg, adt->types, y->subtypes);

    // TODO: ADT fields need to be special-cased
//    struct DefState ds;
//    ENTER_DEF(dg, &ds, d, r);
//    pawHir_visit_decl_list(V, d->fields);
//    LEAVE_DEF(dg);
    return;
}

static void define_poly_adt(struct HirVisitor *V, struct HirAdtDecl *d)
{
    if (d->monos == NULL) return;
    struct DefGenerator *dg = V->ud;
    for (int i = 0; i < d->monos->count; ++i) {
        define_adt(V, d, K_LIST_GET(d->monos, i));
    }
}

static paw_Bool define_adt_decl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    struct DefGenerator *dg = V->ud;
    if (d->generics != NULL) {
        define_poly_adt(V, d);
    } else {
        define_adt(V, d, d->type);
    }
    return PAW_FALSE;
}

static paw_Bool define_impl_decl(struct HirVisitor *V, struct HirImplDecl *d)
{
    if (handle_monos(V, d->monos)) return PAW_FALSE;
    return !has_generic(d->type);
}

static paw_Bool define_field_decl(struct HirVisitor *V, struct HirFieldDecl *d)
{
    NEW_DEF(V->ud, d, NULL);
    return PAW_FALSE;
}

static paw_Bool define_var_decl(struct HirVisitor *V, struct HirVarDecl *d)
{
    NEW_DEF(V->ud, d, NULL);
    pawHir_visit_type(V, d->init->hdr.type);
    return PAW_FALSE;
}

static paw_Bool define_variant_decl(struct HirVisitor *V, struct HirVariantDecl *d)
{
    struct DefGenerator *dg = V->ud;
    struct Def *result = NEW_DEF(dg, d, NULL);
    struct FuncDef *def = &result->func;

    struct DefState ds;
    ENTER_DEF(dg, &ds, d, def);
    pawHir_visit_decl_list(V, d->fields);
    LEAVE_DEF(dg);

    // manually visited
    return PAW_FALSE;
}

static paw_Bool define_type(struct HirVisitor *V, struct HirType *type)
{
    if (type == NULL) return PAW_FALSE;
    if (!has_generic(type)) new_type(V->ud, type);
    return PAW_FALSE;
}

static struct HirType *cannonicalize_adt(struct HirTypeFolder *F, struct HirAdt *t)
{
    struct DefGenerator *dg = F->ud;
    struct HirDecl *base = pawHir_get_decl(dg->C, t->did);
    // NOTE: relies on the fact that pawP_instantiate looks through the 'monos' list on the base ADT and
    //       returns the first matching ADT, rather than always creating a new ADT
    struct HirType *type = pawP_instantiate(dg->C, base, t->types);
    pawH_insert(ENV(dg->C), dg->adts, P2V(type), P2V(type));
    // TODO: it seems like t->types should be folded before calling pawP_instantiate, figure out
    //       why this works and that does not
    type->adt.types = F->FoldTypeList(F, type->adt.types);
    return type;
}

static void cannonicalize_adts(struct DefGenerator *dg)
{
    struct HirTypeFolder F;
    struct Hir *hir = dg->m->hir;
    pawHir_type_folder_init(&F, dg->C, dg);
    F.FoldAdt = cannonicalize_adt;

    pawHir_fold_decl_list(&F, hir->items);
}

static void register_adt(struct DefGenerator *dg, struct HirType *type)
{
    const Value *pv = pawH_get(dg->adts, P2V(type));
    if (pv == NULL) return; // not cannonical version
    if (has_generic(type)) return;

    struct HirAdt *t = HirGetAdt(type);
    struct HirDecl *decl = pawHir_get_decl(dg->C, t->did);
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    paw_Env *P = ENV(dg->C);

    struct HirAdt *adt = HirGetAdt(type);
    const int ntypes = adt->types ? adt->types->count : 0;
    struct Type *y = pawY_new_adt(P, ntypes);
    y->adt.did = P->defs.count;
    map_types(dg, type, y);

    struct Def *result = NEW_DEF(dg, d, type);
    struct AdtDef *r = &result->adt;
    r->is_struct = d->is_struct;
    r->is_pub = d->is_pub;
    r->code = y->hdr.code;
    r->name = d->name;
}

static void register_poly_adt(struct DefGenerator *dg, struct HirAdtDecl *d)
{
    if (d->monos == NULL) return;
    for (int i = 0; i < d->monos->count; ++i) {
        register_adt(dg, K_LIST_GET(d->monos, i));
    }
}

static paw_Bool register_adt_decl(struct HirVisitor *V, struct HirAdtDecl *d)
{
    struct DefGenerator *dg = V->ud;
    if (d->generics != NULL) {
        register_poly_adt(dg, d);
    } else {
        register_adt(dg, d->type);
    }
    return PAW_FALSE;
}

static void register_adts(struct HirVisitor *V, struct HirDeclList *items)
{
    for (int i = 0; i < items->count; ++i) {
        struct HirDecl *item = K_LIST_GET(items, i);
        if (!HirIsAdtDecl(item)) continue;
        register_adt_decl(V, HirGetAdtDecl(item));
    }
}

struct ItemList *pawP_define_all(struct Compiler *C, struct ModuleList *modules, int *poffset)
{
    struct ItemList *items = pawP_item_list_new(C);

    paw_Env *P = ENV(C);
    ENSURE_STACK(P, 1);

    // DeclId => [struct HirType]
    Map *adts = pawH_new(P);
    V_SET_OBJECT(P->top.p, adts);
    API_INCR_TOP(P, 1);

    struct DefGenerator dg = {
        .nvals = *poffset,
        .items = items,
        .adts = adts,
        .C = C,
    };

    struct HirVisitor V;
    pawHir_visitor_init(&V, C, &dg);

    V.VisitVarDecl = define_var_decl;
    V.VisitFieldDecl = define_field_decl;
    V.VisitImplDecl = define_impl_decl;
    V.VisitFuncDecl = define_func_decl;
    V.VisitVariantDecl = define_variant_decl;
    V.VisitType = define_type;

    for (int i = 0; i < modules->count; ++i) {
        dg.m = K_LIST_GET(modules, i);
        cannonicalize_adts(&dg);
    }

    V.VisitAdtDecl = register_adt_decl;
    for (int i = 0; i < modules->count; ++i) {
        dg.m = K_LIST_GET(modules, i);
        register_adts(&V, dg.m->hir->items);
    }

    V.VisitAdtDecl = define_adt_decl;
    for (int i = 0; i < modules->count; ++i) {
        dg.m = K_LIST_GET(modules, i);
        pawHir_visit_decl_list(&V, dg.m->hir->items);
    }
    *poffset = dg.nvals;
    return items;
}

struct ItemSlot *pawP_new_item_slot(struct Compiler *C, struct HirDecl *decl, struct HirType *type, struct ModuleInfo *m)
{
    struct ItemSlot *slot = pawK_pool_alloc(ENV(C), C->pool, sizeof(struct ItemSlot));
    *slot = (struct ItemSlot){
        .decl = decl,
        .type = type,
        .m = m,
    };
    return slot;
}
