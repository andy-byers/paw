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
    struct IrType *r = pawIr_new_type(L->C, kIrFuncPtr);
    IrGetFuncPtr(r)->params = lower_type_list(L, t->params);
    IrGetFuncPtr(r)->result = lower_type(L, t->result);
    return r;
}

static struct IrType *lower_tuple_type(struct LowerType *L, struct HirTupleType *t)
{
    struct IrType *r = pawIr_new_type(L->C, kIrTuple);
    IrGetTuple(r)->elems = lower_type_list(L, t->elems);
    return r;
}

static struct IrType *lower_path_type(struct LowerType *L, struct HirPathType *t)
{
    struct IrType *result =  pawP_lookup(L->C, L->m, L->symtab, t->path, LOOKUP_TYPE);
    if (result == NULL) pawE_error(ENV(L->C), PAW_ENAME, -1, "invalid path"); // TODO message
    return result;
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
    }
}

struct IrType *pawP_lower_type(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirType *type)
{
    return lower_type(&(struct LowerType){
        .symtab = symtab,
        .C = C,
        .m = m,
    }, type);
}

static struct IrTypeList *lower_type_list(struct LowerType *L, struct HirTypeList *types)
{
    struct IrTypeList *result = pawIr_type_list_new(L->C);
    for (int i = 0; i < types->count; ++i) {
        struct IrType *type = lower_type(L, K_LIST_GET(types, i));
        K_LIST_PUSH(L->C, result, type);
    }
    return result;
}

struct IrTypeList *pawP_lower_type_list(struct Compiler *C, struct ModuleInfo *m, struct HirSymtab *symtab, struct HirTypeList *types)
{
    return lower_type_list(&(struct LowerType){
        .symtab = symtab,
        .C = C,
        .m = m,
    }, types);
}

