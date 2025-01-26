// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// lookup.c: Resolve HirPath to a DeclId and IrType.

#include "hir.h"
#include "ir_type.h"
#include "type_folder.h"

struct QueryState {
    struct Compiler *C;
    struct ModuleInfo *m;
    struct HirAdtDecl *adt;
    struct HirSymtab *symtab;
    paw_Env *P;
    int base_modno;
    int target;
    int index;
    int line; // TODO: never set
};

static int module_number(struct ModuleInfo *m)
{
    return m->hir->modno;
}

static struct ModuleInfo *get_module(struct QueryState *Q, int modno)
{
    return K_LIST_GET(Q->C->modules, modno);
}

static struct ModuleInfo *find_import(struct QueryState *Q, String *name)
{
    struct HirImport *im;
    K_LIST_FOREACH(Q->m->hir->imports, im) {
        if (im->has_star) continue; // checked later
        struct ModuleInfo *m = get_module(Q, im->modno);
        // use alias, i.e. "use mod as alias", if it exists
        const String *target = im->as == NULL ? m->hir->name : im->as;
        if (pawS_eq(name, target)) return m;
    }
    return NULL;
}

static struct HirSymbol *resolve_symbol(struct QueryState *Q, const String *name)
{
    // search the global symbols
    const int index = pawHir_find_symbol(Q->m->globals, name);
    if (index < 0) return NULL;
    return K_LIST_GET(Q->m->globals, index);
}

struct QueryBase {
    struct HirSegment *seg;
    struct HirDecl *base;
    paw_Bool is_type;
};

static struct QueryBase find_global_in(struct QueryState *Q, struct ModuleInfo *root, struct HirPath *path)
{
    paw_assert(path->count > 0);

    Q->m = root;
    Q->index = 0;
    do {
        struct HirSegment *seg = &K_LIST_GET(path, Q->index++);
        struct HirSymbol *sym = resolve_symbol(Q, seg->name);
        if (sym != NULL) {
            return (struct QueryBase){
                .is_type = sym->is_type,
                .base = sym->decl,
                .seg = seg,
            };
        }
        struct ModuleInfo *m = find_import(Q, seg->name);
        if (m == NULL) break;
        if (module_number(Q->m) != Q->base_modno) {
            pawE_error(ENV(Q), PAW_ESYNTAX, -1, "transitive imports are not supported");
        }
        Q->m = m;
    } while (Q->index < path->count);
    return (struct QueryBase){0};
}

static struct QueryBase find_global(struct QueryState *Q, struct ModuleInfo *m, struct HirPath *path)
{
    struct QueryBase q = find_global_in(Q, m, path);
    if (q.base != NULL) return q;

    struct ModuleInfo *prelude = get_module(Q, 0);
    q = find_global_in(Q, prelude, path);
    if (q.base != NULL) return q;

    struct HirImport *im;
    K_LIST_FOREACH(m->hir->imports, im) {
        if (!im->has_star) continue;
        struct ModuleInfo *m = get_module(Q, im->modno);
        q = find_global_in(Q, m, path);
        if (q.base != NULL) break;
    }
    return q;
}

static struct IrType *expect_field(struct QueryState *Q, struct IrType *adt, String *name)
{
    struct HirDecl *field = pawP_find_field(Q->C, adt, name);
    if (field == NULL) {
        struct HirDecl *decl = pawHir_get_decl(Q->C, IrGetAdt(adt)->did);
        NAME_ERROR(Q, "field '%s' does not exist on type '%d'",
                name->text, decl->hdr.name->text);
    }
    return pawP_instantiate_field(Q->C, adt, field);
}

static struct IrType *find_method(struct QueryState *Q, struct IrType *type, struct HirSegment *seg)
{
    struct IrType *method = pawP_find_method(Q->C, type, seg->name);
    if (method == NULL) {
        struct HirDecl *decl = pawHir_get_decl(Q->C, IR_TYPE_DID(type));
        const char *base_repr = pawIr_print_type(Q->C, type);
        NAME_ERROR(Q, "field '%s' does not exist on type '%s'",
                seg->name->text, base_repr);
    }
    seg->did = IR_TYPE_DID(method);
    return method;
}

static struct IrType *find_enumerator(struct QueryState *Q, struct IrType *type, struct HirSegment *seg)
{
    if (seg->types != NULL) NAME_ERROR(Q, "unexpected type arguments on enumerator '%s'", seg->name->text);
    struct HirDecl *field = pawP_find_field(Q->C, type, seg->name);
    if (field == NULL) return NULL;
    struct IrType *inst = pawP_instantiate_field(Q->C, type, field);
    seg->did = IR_TYPE_DID(inst);
    return inst;
}

static struct IrType *find_assoc_item(struct QueryState *Q, struct HirDecl *prev, struct IrType *type, struct HirSegment *next)
{
    struct HirDecl *decl = pawHir_get_decl(Q->C, prev->hdr.did);
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    if (!d->is_struct) {
        struct IrType *result = find_enumerator(Q, type, next);
        if (result != NULL) return result;
    }
    return find_method(Q, type, next);
}

static struct IrTypeList *maybe_generalize_adt(struct QueryState *Q, struct HirDecl *decl, struct IrTypeList *types)
{
    if (types != NULL) return types;
    struct HirAdtDecl *d = HirGetAdtDecl(decl);
    if (d->generics == NULL) return types;
    struct IrTypeList *result = pawIr_type_list_new(Q->C);
    for (int i = 0; i < d->generics->count; ++i) {
        struct IrType *var = pawU_new_unknown(Q->C->U, Q->line);
        K_LIST_PUSH(Q->C, result, var);
    }
    return result;
}

static struct QueryBase find_local(struct Compiler *C, struct HirSymtab *symtab, struct HirPath *path)
{
    struct HirSegment *first = &K_LIST_FIRST(path);
    for (int depth = symtab->count - 1; depth >= 0; --depth) {
        struct HirScope *scope = K_LIST_GET(symtab, depth);
        const int index = pawHir_find_symbol(scope, first->name);
        if (index >= 0) {
            struct HirSymbol *symbol = K_LIST_GET(scope, index);
            if (!HirIsTypeDecl(symbol->decl)) {
                if (path->count > 1) NAME_ERROR(C, "'::' applied to value '%s'", first->name->text);
                if (first->types != NULL) TYPE_ERROR(C, "type arguments applied to value '%s'", first->name->text);
            }
            first->did = symbol->decl->hdr.did;
            return (struct QueryBase){
                .base = symbol->decl,
                .is_type = symbol->is_type,
                .seg = first,
            };
        }
    }
    return (struct QueryBase){0};
}

static struct IrTypeList *maybe_copy_subtypes(struct QueryState *Q, struct IrType *type)
{
    struct IrTypeList *types = IR_TYPE_SUBTYPES(type);
    if (types == NULL) return NULL;

    struct IrType **ptype;
    struct IrTypeList *result = pawIr_type_list_new(Q->C);
    K_LIST_FOREACH(types, ptype) {
        K_LIST_PUSH(Q->C, result, *ptype);
    }
    return result;
}

struct IrTypeList *pawP_instantiate_typelist(struct Compiler *C, struct IrTypeList *before,
                                             struct IrTypeList *after, struct IrTypeList *target);

static struct IrTypeList *new_unknowns(struct Compiler *C, int count)
{
    struct IrTypeList *list = pawIr_type_list_new(C);
    for (int i = 0; i < count; ++i) {
        struct IrType *unknown = pawU_new_unknown(C->U, -1);
        K_LIST_PUSH(C, list, unknown);
    }
    return list;
}

// TODO: it seems strange to modify the name and decl ID of the path segment but not
//       the type list. Shoudln't really matter since we SHOULD now depend only on
//       the decl ID from this point on.
#include "stdio.h"
static struct IrType *resolve_alias(struct QueryState *Q, struct QueryBase *pq)
{
    paw_assert(HirIsTypeDecl(pq->base));
    struct IrType *type = GET_NODE_TYPE(Q->C, pq->base);
    struct HirTypeDecl *d = HirGetTypeDecl(pq->base);
    struct HirSegment *seg = pq->seg;

    seg->name = d->name;
    seg->did = IR_TYPE_DID(type);
    pq->base = pawHir_get_decl(Q->C, seg->did);

    struct IrType *rhs = GET_NODE_TYPE(Q->C, d->rhs);
    struct IrTypeList *types = IR_TYPE_SUBTYPES(rhs);
    if (d->generics == NULL) return rhs;

    struct IrTypeList *generics = pawHir_collect_decl_types(Q->C, d->generics);
    struct IrTypeList *unknowns = new_unknowns(Q->C, generics->count);
    struct IrTypeList *subst = pawP_instantiate_typelist(Q->C, generics, unknowns, types);
    if (seg->types != NULL) {
        struct IrType **punknown, **pknown;
        struct IrTypeList *knowns = pawP_lower_type_list(Q->C, Q->m, Q->symtab, seg->types);
        K_LIST_ZIP(unknowns, punknown, knowns, pknown) {
            pawU_unify(Q->C->U, *punknown, *pknown);
        }
    }

    struct HirDecl *aliased = pawHir_get_decl(Q->C, IR_TYPE_DID(rhs));
    return pawP_instantiate(Q->C, aliased, subst);
}

struct IrType *pawP_lookup(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirPath *path, enum LookupKind kind)
{
    struct QueryState Q = {
        .base_modno = module_number(m),
        .target = m->hir->modno,
        .symtab = symtab,
        .P = ENV(C),
        .m = m,
        .C = C,
    };

    struct IrType *inst;
    paw_assert(path->count > 0);
    struct QueryBase q = find_local(C, symtab, path);
    if (q.base != NULL) {
        inst = HirIsTypeDecl(q.base)
            ? resolve_alias(&Q, &q)
            : GET_NODE_TYPE(C, q.base);
        Q.index = 1;
        Q.m = m;
        goto found_local;
    }

    // sets 'Q.m' to the module containing the target declaration, and sets
    // 'Q.index' to the index of the following segment
    q = find_global(&Q, m, path);
    if (q.base == NULL) return NULL;

    struct IrTypeList *types = q.seg->types != NULL
        ? pawP_lower_type_list(C, m, symtab, q.seg->types)
        : NULL;

    switch (HIR_KINDOF(q.base)) {
        case kHirAdtDecl:
            types = maybe_generalize_adt(&Q, q.base, types);
            // (fallthrough)
        case kHirFuncDecl:
            inst = pawP_instantiate(C, q.base, types);
            q.seg->did = IR_TYPE_DID(inst);
            break;
        case kHirTypeDecl:
            inst = resolve_alias(&Q, &q);
            break;
        case kHirVarDecl:
        case kHirFieldDecl:
        case kHirGenericDecl:
            inst = GET_NODE_TYPE(C, q.base);
            q.seg->did = q.base->hdr.did;
            break;
        default:
            goto invalid_path;
    }

found_local:
    if (Q.index < path->count) {
        // resolve method or enumerator
        struct HirSegment *next = &K_LIST_GET(path, Q.index);
        inst = find_assoc_item(&Q, q.base, inst, next);
        q.is_type = PAW_FALSE;
    }

    if ((kind == LOOKUP_VALUE && q.is_type)
            || (kind == LOOKUP_TYPE && !q.is_type)) {
invalid_path:
        TYPE_ERROR(&Q, "expected %s but found %s",
                kind == LOOKUP_VALUE ? "value" : "type",
                q.is_type ? "value" : "type");
    }
    return inst;
}

