// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "env.h"
#include "api.h"
#include "mem.h"
#include "rt.h"
#include "type.h"
#include <limits.h>

void pawE_push_cstr(paw_Env *P, unsigned kind)
{
    API_CHECK_PUSH(P, 1);
    V_SET_OBJECT(P->top.p, P->string_cache[kind]);
    API_INCR_TOP(P, 1);
}

_Noreturn void pawE_error(paw_Env *P, int code, int line, const char *fmt, ...)
{
    Buffer print;
    pawL_init_buffer(P, &print);
    if (line >= 0) {
        paw_assert(P->modname != NULL);
        pawL_add_fstring(P, &print, "%s:%d: ", P->modname->text, line);
    }

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &print, fmt, arg);
    va_end(arg);

    pawL_push_result(P, &print);
    pawC_throw(P, code);
}

void pawE_uninit(paw_Env *P)
{
    pawY_uninit(P);
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

int pawE_locate(paw_Env *P, const String *name, paw_Bool only_pub)
{
    paw_assert(name != NULL);
    const struct DefList defs = P->defs;
    for (int i = 0; i < defs.count; ++i) {
        const struct Def *def = P->defs.data[i];
        const String *query = def->hdr.kind == DEF_FUNC ? def->func.mangled_name :
            def->hdr.kind == DEF_ADT ? def->adt.mangled_name : NULL;
        if (pawS_eq(name, query) && def->hdr.is_pub >= only_pub) {
            return i;
        }
    }
    return -1;
} 

