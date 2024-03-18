// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "error.h"
#include "aux.h"
#include "call.h"

static int current_line(CallFrame *cf)
{
    Proto *p = cf->fn->p;
    const int pc = cf->pc - p->source;

    int i = 0;
    for (; i + 1 < p->nlines; ++i) {
        if (p->lines[i].pc >= pc) {
            break;
        }
    }
    return p->lines[i].line;
}

static void add_location(paw_Env *P, Buffer *buf)
{
    CallFrame *cf = P->cf;
    while (!CF_IS_BASE(cf)) {
        // Find the paw function caller.
        if (IS_PAW(cf)) {
            Closure *main = cf->fn;
            if (main) {
                const String *s = main->p->name;
                pawL_add_nstring(P, buf, s->text, s->length);
            } else {
                pawL_add_char(P, buf, '?');
            }
            pawL_add_fstring(P, buf, ":%I: ", current_line(cf));
            break;
        }
        cf = cf->prev;
    }
}

void pawE_error(paw_Env *P, int error, const char *fmt, ...)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    add_location(P, &buf);

    va_list arg;
    va_start(arg, fmt);
    pawL_add_vfstring(P, &buf, fmt, arg);
    va_end(arg);

    pawL_push_result(P, &buf);
    pawC_throw(P, error);
}

void pawE_type(paw_Env *P, const char *what)
{
    pawE_error(P, PAW_ETYPE, "unsupported operand type for '%s': '%s'",
               what, paw_typename(P, -1));
}

void pawE_type2(paw_Env *P, const char *what)
{
    pawE_error(P, PAW_ETYPE, "unsupported operand types for '%s': '%s' and '%s'",
               what, paw_typename(P, -2), paw_typename(P, -1));
}

void pawE_range(paw_Env *P, const char *what)
{
    pawE_error(P, PAW_ERANGE, "result of '%s' is out of range", what);
}

void pawE_index(paw_Env *P)
{
    pawE_error(P, PAW_EINDEX, "array index out of range");
}

void pawE_system(paw_Env *P, int error)
{
    pawE_error(P, PAW_ESYSTEM, "%s", strerror(error));
}

static void add_3_parts(paw_Env *P, const char *before, const char *value, const char *after)
{
    Buffer buf;
    pawL_init_buffer(P, &buf);
    add_location(P, &buf);
    pawL_add_fstring(P, &buf, "%s%s%s", before, value, after);
    pawL_push_result(P, &buf);
}

void pawE_name(paw_Env *P, const char *name)
{
    add_3_parts(P, "name '", name, "' is not defined");
    pawC_throw(P, PAW_ENAME);
}

void pawE_attr(paw_Env *P, const char *attr)
{
    add_3_parts(P, "attribute '", attr, "' does not exist");
    pawC_throw(P, PAW_EATTR);
}

void pawE_key(paw_Env *P, const char *key)
{
    add_3_parts(P, "key '", key, "' does not exist");
    pawC_throw(P, PAW_EKEY);
}
