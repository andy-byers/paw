// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "hir.h"
#include "ir_type.h"
#include "unify.h"

struct LowerType {
    struct HirSymtab *symtab;
    struct ModuleInfo *m;
    struct Compiler *C;
};

static struct IrType *lower_type(struct LowerType *L, struct HirType *type);
static struct IrTypeList *lower_type_list(struct LowerType *L, struct HirTypeList *types);

static struct IrType *lower_func_ptr(struct LowerType *L, struct HirFuncPtr *t)
{
    struct IrTypeList *params = lower_type_list(L, t->params);
    struct IrType *result = lower_type(L, t->result);
    return pawIr_new_func_ptr(L->C, params, result);
}

static struct IrType *lower_tuple_type(struct LowerType *L, struct HirTupleType *t)
{
    struct IrTypeList *elems = lower_type_list(L, t->elems);
    return pawIr_new_tuple(L->C, elems);
}

static struct IrType *lower_path_type(struct LowerType *L, struct HirPathType *t)
{
    struct IrType *result = pawP_lookup(L->C, L->m, L->symtab, t->path, LOOKUP_TYPE, PAW_TRUE);
    if (result == NULL)
        NAME_ERROR(L->C, "invalid path '%s'", pawHir_print_path(L->C, t->path));
    return result;
}

static struct IrType *lower_infer_type(struct LowerType *L, struct HirInferType *t)
{
    return pawU_new_unknown(L->C->U, t->line, NULL);
}

static struct IrType *lower_type(struct LowerType *L, struct HirType *type)
{
    switch (HIR_KINDOF(type)) {
        case kHirFuncPtr:
            return lower_func_ptr(L, HirGetFuncPtr(type));
        case kHirTupleType:
            return lower_tuple_type(L, HirGetTupleType(type));
        case kHirPathType:
            return lower_path_type(L, HirGetPathType(type));
        case kHirInferType:
            return lower_infer_type(L, HirGetInferType(type));
    }
}

struct IrType *pawP_lower_type(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirType *type)
{
    struct LowerType L = {
        .symtab = symtab,
        .C = C,
        .m = m,
    };
    return lower_type(&L, type);
}

static struct IrTypeList *lower_type_list(struct LowerType *L, struct HirTypeList *types)
{
    struct IrTypeList *result = IrTypeList_new(L->C);
    IrTypeList_reserve(L->C, result, types->count);

    struct HirType *const *ptype;
    K_LIST_FOREACH (types, ptype) {
        struct IrType *type = lower_type(L, *ptype);
        IrTypeList_push(L->C, result, type);
    }
    return result;
}

struct IrTypeList *pawP_lower_type_list(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirTypeList *types)
{
    return lower_type_list(&(struct LowerType){
                               .symtab = symtab,
                               .C = C,
                               .m = m,
                           },
                           types);
}
