// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_ENV_H
#define PAW_ENV_H

#include "opcode.h"
#include "map.h"
#include "str.h"
#include "value.h"

struct Jump; // call.c
typedef int ItemId;
typedef int ValueId;

#define CFF_C 1
#define CFF_ENTRY 2

#define CF_IS_PAW(cf) (!((cf)->flags & CFF_C))
#define CF_IS_ENTRY(cf) ((cf)->flags & CFF_ENTRY)
#define CF_STACK_RETURN(cf) ((cf)->base.p)

typedef struct CallFrame {
    struct CallFrame *prev;
    struct CallFrame *next;
    OpCode const *pc;
    StackRel base;
    StackRel top;
    Closure *fn;
    int flags;
} CallFrame;

enum {
    CSTR_SELF,
    CSTR_NEW,
    CSTR_UNIT,
    CSTR_TRUE,
    CSTR_FALSE,
    CSTR_BOOL,
    CSTR_INT,
    CSTR_FLOAT,
    CSTR_STR,
    CSTR_LIST,
    CSTR_MAP,
    CSTR_OPTION,
    CSTR_RESULT,
    CSTR_RANGE,
    CSTR_RANGE_TO,
    CSTR_RANGE_FROM,
    CSTR_RANGE_FULL,
    CSTR_RANGE_INCLUSIVE,
    CSTR_RANGE_TO_INCLUSIVE,
    CSTR_HASH,
    CSTR_EQUALS,
    CSTR_COMPARE,
    CSTR_EXTERN,
    CSTR_EXCLAMATION,
    CSTR_UNDERSCORE,
    CSTR_KMODULES,
    CSTR_KSYMBOLS,
    CSTR_KSEARCHERS,
    NCSTR,
};

typedef struct MapPolicy MapPolicy;
typedef struct ListPolicy ListPolicy;

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
    Value registry;

    // Array of commonly-used strings.
    String *string_cache[NCSTR];

    // Contains an error message that is served when the system runs out of
    // memory (a call to the 'alloc' field below returned NULL).
    Value mem_errmsg;

    struct ValList {
        Value *data;
        int count;
        int alloc;
    } vals;

    struct {
        struct RttiType **data;
        int count;
        int alloc;
    } types;

    struct DefList {
        struct Def **data;
        int count;
        int alloc;
    } defs;

    Value constants;
    Value functions;

    size_t heap_size;
    struct Heap *H;
    paw_Alloc alloc;
    void *ud;

#define MAX_POLICIES 1000

    struct MapPolicyList {
        MapPolicy data[MAX_POLICIES];
        int count;
    } map_policies;

    paw_ExecHook hook;
    int hook_count;
    int hook_mask;

    Object *gc_all;
    Object *gc_gray;
    Object *gc_fixed;
    size_t gc_bytes;
    size_t gc_limit;
    paw_Bool gc_noem;
} paw_Env;

void pawE_uninit(paw_Env *P);
_Noreturn void pawE_error(paw_Env *P, int code, int line, char const *fmt, ...);
CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top);
int pawE_locate(paw_Env *P, String const *name, paw_Bool only_pub);

#define CACHED_STRING(P, k) CHECK_EXP((k) < NCSTR, (P)->string_cache[k])
void pawE_push_cstr(paw_Env *P, unsigned kind);

#endif // PAW_ENV_H
