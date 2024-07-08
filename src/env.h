// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_CONTEXT_H
#define PAW_CONTEXT_H

#include "meta.h"
#include "opcode.h"
#include "paw.h"
#include "str.h"
#include "type.h"
#include "value.h"
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>

struct Jump; // call.c

typedef struct Module {
    Struct **structs;
    Enum **enums;
    Value *slots;
    int nstructs;
    int nenums;
    int nslots;
} Module;

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
    CSTR_TRUE,
    CSTR_FALSE,
    CSTR_UNIT,
    CSTR_BOOL,
    CSTR_INT,
    CSTR_FLOAT,
    CSTR_STRING,
    CSTR_VECTOR,
    CSTR_MAP,
    NCSTR,
};

typedef struct GlobalVar {
    String *name;
    Value value;
    paw_Type type;
} GlobalVar;

struct GlobalList {
    int alloc;
    int count;
    Value data[];
};

struct BuiltinList {
    int alloc;
    int count;
    Value data[];
};

struct MethodList {
    int alloc;
    int count;
    Value data[];
};

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

    String *modname;
    Map *builtin;
    Map *libs;
    Module *mod;
    Value meta_keys[NMETAMETHODS];

    // Array of commonly-used strings.
    String *str_cache[NCSTR];

    // Contains an error message that is served when the system runs out of
    // memory (a call to the 'alloc' field below returned NULL).
    Value mem_errmsg;

    struct GlobalVec {
        GlobalVar *data;
        int size;
        int alloc;
    } gv;

    struct StructVec {
        Struct **data;
        int size;
        int alloc;
    } sv;

    paw_Alloc alloc;
    void *ud;

    Object *gc_all;
    Object *gc_gray;
    Object *gc_fixed;
    size_t gc_bytes;
    size_t gc_limit;
    paw_Bool gc_noem;
} paw_Env;

void pawE_error(paw_Env *P, int code, int line, const char *fmt, ...);
CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top);
int pawE_new_global(paw_Env *P, String *name, paw_Type type);
int pawE_find_global(paw_Env *P, String *name);
#define pawE_get_global(P, i) (&(P)->gv.data[i])

static inline String *pawE_cstr(paw_Env *P, unsigned type)
{
    paw_assert(type < NCSTR);
    return P->str_cache[type];
}

#endif // PAW_CONTEXT_H
