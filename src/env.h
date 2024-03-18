// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CONTEXT_H
#define PAW_CONTEXT_H

#include "meta.h"
#include "opcode.h"
#include "paw.h"
#include "str.h"
#include "value.h"
#include <stddef.h>
#include <stdio.h>

struct Jump; // call.c

#define CFF_C 1
#define CFF_ENTRY 2

#define IS_PAW(cf) (!((cf)->flags & CFF_C))
#define IS_ENTRY(cf) ((cf)->flags & CFF_ENTRY)

typedef struct CallFrame {
    struct CallFrame *prev;
    struct CallFrame *next;
    const OpCode *pc;
    StackPtr base;
    StackPtr top;
    Closure *fn;
    int flags;
} CallFrame;

#define CF_IS_BASE(cf) (!(cf)->prev)
#define CF_SOURCE_BEGIN(cf) ((cf)->fn->p->source)
#define CF_SOURCE_LENGTH(cf) ((cf)->fn->p->length)
#define CF_SOURCE_END(cf) (CF_SOURCE_BEGIN(cf) + CF_SOURCE_LENGTH(cf))
#define CF_STACK_RETURN(cf) ((cf)->base)
// TODO: CF_STACK_BASE() should skip any variadic parameters (argc is fixed param count)
#define CF_STACK_BASE(cf) (CF_STACK_RETURN(cf) + 1 + (cf)->fn->p->argc)

enum {
    CSTR_SELF,
    CSTR_SUPER,
    CSTR_INIT,
    CSTR_NULL,
    CSTR_TRUE,
    CSTR_FALSE,
    NCSTR,
};

typedef struct paw_Env {
    StringTable strings;

    CallFrame main;
    CallFrame *cf;
    int ncf;

    struct Jump *jmp;
    UpValue *up_list;

    Map *globals;

    StackPtr stack;
    StackPtr bound;
    StackPtr top;

    Map *libs;
    Map *attr[NOBJECTS];
    Value meta_keys[NMETA];
    Value str_cache[NCSTR];

    paw_Alloc alloc;
    void *ud;

    Object *gc_all;
    Object *gc_gray;
    Object *gc_fixed;
    size_t gc_bytes;
    size_t gc_limit;
} paw_Env;

CallFrame *pawE_extend_cf(paw_Env *X, StackPtr top);

static inline Value pawE_cstr(paw_Env *P, unsigned type)
{
    paw_assert(type < NCSTR);
    return P->str_cache[type];
}

#endif // PAW_CONTEXT_H
