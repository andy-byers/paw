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

#define cf_is_paw(cf) (!((cf)->flags & CFF_C))
#define cf_is_entry(cf) ((cf)->flags & CFF_ENTRY)
#define cf_stack_return(cf) ((cf)->base.p)

typedef struct CallFrame {
    struct CallFrame *prev;
    struct CallFrame *next;
    const OpCode *pc;
    StackRel base;
    StackRel top;
    Closure *fn;
    int flags;
} CallFrame;

enum {
    CSTR_SELF,
    CSTR_SUPER,
    CSTR_INIT,
    CSTR_NULL,
    CSTR_TRUE,
    CSTR_FALSE,
    NCSTR,
};

typedef struct GlobalVar {
    VarDesc desc;
    Value value;
} GlobalVar;

typedef struct paw_Env {
    StringTable strings;

    CallFrame main;
    CallFrame *cf;
    int ncf;

    struct Jump *jmp;
    UpValue *up_list;

    StackRel stack;
    StackRel bound;
    StackRel top;

    Map *libs;
    Value object;
    Class *builtin[NOBJECTS];
    Value meta_keys[NMETAMETHODS];
    Value str_cache[NCSTR];
    Value mem_errmsg;

    // TODO: At some point, the globals should go into a struct called Module. Make 
    //       Module a paw object. 
    struct GlobalVec {
        GlobalVar *data; 
        int size;
        int alloc;
    } gv;

    struct TypeVec {
        Type **data; 
        int size;
        int alloc;
    } tv;

    paw_Alloc alloc;
    void *ud;

    Object *gc_all;
    Object *gc_gray;
    Object *gc_fixed;
    size_t gc_bytes;
    size_t gc_limit;
    paw_Bool gc_noem;
} paw_Env;

#define e_tag(P, n) check_exp((int)(n) < (P)->tv.size, (n) < 0 ? NULL : (P)->tv.data[n])
#define e_bool(P) e_tag(P, PAW_TBOOL)
#define e_int(P) e_tag(P, PAW_TINT)
#define e_float(P) e_tag(P, PAW_TFLOAT)
#define e_string(P) e_tag(P, PAW_TSTRING)

CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top);
int pawE_new_global(paw_Env *P, String *name, TypeTag tag);
GlobalVar *pawE_find_global(paw_Env *P, String *name);
#define pawE_get_global(P, i) (&(P)->gv.data[i])

static inline Value pawE_cstr(paw_Env *P, unsigned type)
{
    paw_assert(type < NCSTR);
    return P->str_cache[type];
}

#endif // PAW_CONTEXT_H
