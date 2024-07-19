// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#include "env.h"
#include "mem.h"
#include "rt.h"
#include <limits.h>

void pawE_error(paw_Env *P, int code, int line, const char *fmt, ...)
{
    Buffer print;
    pawL_init_buffer(P, &print);
    pawL_add_nstring(P, &print, P->modname->text, P->modname->length);
    pawL_add_fstring(P, &print, ":%d: ", line);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &print, fmt, arg);
    va_end(arg);

    pawL_push_result(P, &print);
    pawC_throw(P, code);
}

CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top)
{
    if (P->ncf >= ITEM_MAX) {
        pawR_error(P, PAW_EOVERFLOW, "too many nested function calls");
    }
    CallFrame *cf = pawM_new(P, CallFrame);
    P->cf->next = cf;
    cf->prev = P->cf;
    cf->next = NULL;
    cf->top.p = top;
    ++P->ncf;
    return cf;
}

int pawE_new_global(paw_Env *P, String *name, paw_Type type)
{
    struct GlobalVec *gv = &P->gv; // enforce uniqueness
    for (int i = 0; i < gv->size; ++i) {
        if (pawS_eq(name, gv->data[i].name)) {
            paw_assert(0); // FIXME
        }
    }
    pawM_grow(P, gv->data, gv->size, gv->alloc);
    const int i = gv->size++;
    GlobalVar *var = &gv->data[i];
    var->name = name;
    var->type = type;
    v_set_0(&var->value);
    return i;
}

int pawE_find_global(paw_Env *P, const String *name)
{
    struct GlobalVec *gv = &P->gv;
    for (int i = 0; i < gv->size; ++i) {
        const GlobalVar *var = &gv->data[i];
        if (pawS_eq(name, var->name)) {
            return i;
        }
    }
    return -1;
}
