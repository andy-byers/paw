// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "env.h"
#include "mem.h"
//#include "rt.h"
#include <limits.h>

CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top)
{
    if (P->ncf == INT_MAX) {
        paw_assert(0);
 //       pawR_error(P, PAW_EOVERFLOW, "too many nested function calls");
    }
    CallFrame *cf = pawM_new(P, CallFrame);
    P->cf->next = cf;
    cf->prev = P->cf;
    cf->next = NULL;
    cf->top.p = top;
    ++P->ncf;
    return cf;
}

int pawE_new_global(paw_Env *P, String *name, Type *tag)
{
    struct GlobalVec *gv = &P->gv; // enforce uniqueness
    for (int i = 0; i < gv->size; ++i) {
        if (pawS_eq(name, gv->data[i].desc.name)) {
            paw_assert(0); // FIXME
        }
    }
    pawM_grow(P, gv->data, gv->size, gv->alloc);
    const int i = gv->size++;
    GlobalVar *var = &gv->data[i];
    var->desc.name = name;
    var->desc.type = tag;
    v_set_0(&var->value);
    return i;
}

GlobalVar *pawE_find_global(paw_Env *P, String *name)
{
    struct GlobalVec *gv = &P->gv;
    for (int i = 0; i < gv->size; ++i) {
        GlobalVar *var = &gv->data[i];
        if (pawS_eq(name, var->desc.name)) {
            return var;
        }
    }
    return NULL;
}
