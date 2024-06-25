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

struct Jump; // call.c

typedef enum DefKind {
    DEF_VAR,
    DEF_FUNC,
    DEF_STRUCT,
    DEF_FIELD,
    DEF_TYPE,
} DefKind;

typedef struct Definition Definition;

#define PAWE_DEF_HEADER \
    Type *type;         \
    DefId id : 8;       \
    DefKind kind : 8
typedef struct DefHeader {
    PAWE_DEF_HEADER;
} DefHeader;

typedef struct VarDef {
    PAWE_DEF_HEADER;
    String *name;
} VarDef;

typedef struct FuncDef {
    PAWE_DEF_HEADER;
    String *name;
} FuncDef;

typedef struct TypeDef {
    PAWE_DEF_HEADER;
} TypeDef;

typedef struct AdtDef {
    PAWE_DEF_HEADER;
    paw_Bool is_struct : 1;
    int nattrs;
    Definition **attrs;
} AdtDef;

typedef struct Definition {
    union {
        DefHeader hdr;
        VarDef var;
        FuncDef func;
        TypeDef type;
        AdtDef adt;
    };
} Definition;

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

    // Array containing a definition for each program construct. Created during
    // code generation, and kept around for RTTI purposes.
    Definition *defs;
    int ndefs;

    Map *builtin;
    Map *libs;
    Value object;
    Module *mod;
    Value meta_keys[NMETAMETHODS];

    // Array of commonly-used strings.
    String *str_cache[NCSTR];

    // Contains an error message that is served when the system runs out of memory
    // (a call to the 'alloc' field below returned NULL).
    Value mem_errmsg;

    // TODO: At some point, the globals should go into a struct called Module. Make
    //       Module a paw object.
    struct GlobalVec {
        GlobalVar *data;
        int size;
        int alloc;
    } gv;

    paw_Alloc alloc;
    void *ud;

    Object *gc_all;
    Object *gc_gray;
    Object *gc_fixed;
    size_t gc_bytes;
    size_t gc_limit;
    paw_Bool gc_noem;
} paw_Env;

CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top);
int pawE_new_global(paw_Env *P, String *name, paw_Type type);
GlobalVar *pawE_find_global(paw_Env *P, String *name);
#define pawE_get_global(P, i) (&(P)->gv.data[i])

static inline String *pawE_cstr(paw_Env *P, unsigned type)
{
    paw_assert(type < NCSTR);
    return P->str_cache[type];
}

#endif // PAW_CONTEXT_H
