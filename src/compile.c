// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "compile.h"
#include "ast.h"
#include "gc.h"
#include "hir.h"
#include "map.h"

paw_Type pawP_type2code(struct Compiler *C, struct HirType *type)
{
    if (HirIsAdt(type)) {
        struct HirAdt *adt = HirGetAdt(type);
        if (adt->base == C->vector_did) {
            return PAW_TVECTOR; 
        } else if (adt->base == C->map_did) {
            return PAW_TMAP; 
        } else if (adt->base <= PAW_TSTRING) {
            return adt->base;
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

void pawP_startup(paw_Env *P, struct Compiler *C, struct DynamicMem *dm, const char *modname)
{
    Value *pv = pawC_push0(P);
    Map *strings = pawH_new(P);
    V_SET_OBJECT(pv, strings);

    P->modname = pawP_scan_string(P, strings, modname);

    *C = (struct Compiler){
        .modname = P->modname,
        .strings = strings,
        .dm = dm,
        .P = P,
    };
}

void pawP_teardown(paw_Env *P, const struct DynamicMem *dm)
{
    pawM_free_vec(P, dm->labels.values, dm->labels.capacity);
    pawM_free_vec(P, dm->scratch.data, dm->scratch.alloc);
    pawM_free_vec(P, dm->decls.data, dm->decls.alloc);
    pawM_free_vec(P, dm->vars.data, dm->vars.alloc);
    pawAst_free(dm->ast);
    pawHir_free(dm->hir);
}
