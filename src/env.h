// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#ifndef PAW_ENV_H
#define PAW_ENV_H

#include "opcode.h"
#include "str.h"
#include "value.h"

struct Jump; // call.c

typedef uint16_t DefId;
typedef uint16_t ValueId;

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
    CSTR_BOOL,
    CSTR_INT,
    CSTR_FLOAT,
    CSTR_STR,
    CSTR_LIST,
    CSTR_MAP,
    CSTR_RESULT,
    CSTR_OPTION,
    NCSTR,
};

struct Field {
    String *name;
    struct Def *def;
};

struct FieldList {
    struct Field *data;
    int count;
};

struct TypeList {
    struct Type **data;
    int count;
};

enum TypeKind {
    TYPE_ADT,
    TYPE_FUNC,
    TYPE_TUPLE,
};

#define TYPE_HEADER paw_Type code; \
                    enum TypeKind kind : 8
                    
struct TypeHeader {
    TYPE_HEADER;
};

struct Adt {
    TYPE_HEADER;
    DefId did;
    struct TypeList types;
};

struct FuncType {
    TYPE_HEADER;
    struct TypeList params;
    struct Type *result;
};

struct TupleType {
    TYPE_HEADER;
    struct TypeList types;
};

struct Type {
    union {
        struct TypeHeader hdr;
        struct Adt adt;
        struct FuncType func;
        struct TupleType tuple;
    };
};

enum DefKind {
    DEF_ADT,
    DEF_FUNC,
    DEF_VAR,
};

#define DEF_HEADER String *name; \
                   struct Def *next_pub; \
                   struct Type *type; \
                   enum DefKind kind : 7; \
                   paw_Bool is_pub : 1

struct DefHeader {
    DEF_HEADER;
};

struct AdtDef {
    DEF_HEADER;
    paw_Bool is_struct : 1;
    struct FieldList fields;
};

struct FuncDef {
    DEF_HEADER;
    ValueId vid;
    struct FieldList params;
};

struct VarDef {
    DEF_HEADER;
    ValueId vid;
};

struct Def {
    union {
        struct DefHeader hdr;
        struct AdtDef adt;
        struct FuncDef func;
        struct VarDef var;
    };
};

void pawE_init_type_list(paw_Env *P, struct TypeList *plist, int count);
void pawE_init_field_list(paw_Env *P, struct FieldList *plist, int count);

struct Type *pawE_new_type(paw_Env *P, enum TypeKind kind);
struct Def *pawE_new_def(paw_Env *P, enum DefKind kind);

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

    // Array of commonly-used strings.
    String *str_cache[NCSTR];

    // Contains an error message that is served when the system runs out of
    // memory (a call to the 'alloc' field below returned NULL).
    Value mem_errmsg;

    struct ValList {
        Value *data;
        int count;
        int alloc;
    } vals;

    struct {
        struct Type **data;
        int count;
        int alloc;
    } types;

    struct DefList {
        struct Def **data;
        int count;
        int alloc;
    } defs;

    size_t heap_size;
    struct Heap *H;
    paw_Alloc alloc;
    void *ud;

    Object *gc_all;
    Object *gc_gray;
    Object *gc_fixed;
    size_t gc_bytes;
    size_t gc_limit;
    paw_Bool gc_noem;
} paw_Env;

void pawE_uninit(paw_Env *P);
void pawE_error(paw_Env *P, int code, int line, const char *fmt, ...);
CallFrame *pawE_extend_cf(paw_Env *P, StackPtr top);
int pawE_locate(paw_Env *P, const String *name, paw_Bool only_pub);

static inline struct Def *pawE_get_def(paw_Env *P, int i)
{
    paw_assert(0 <= i && i < P->defs.count);
    return P->defs.data[i];
}

static inline Value *pawE_get_val(paw_Env *P, int i)
{
    paw_assert(0 <= i && i < P->vals.count);
    return &P->vals.data[i];
}

static inline struct Type *pawE_get_type(paw_Env *P, int i)
{
    paw_assert(0 <= i && i < P->types.count);
    return P->types.data[i];
}

static inline String *pawE_cstr(paw_Env *P, unsigned type)
{
    paw_assert(type < NCSTR);
    return P->str_cache[type];
}

void pawE_print_type(paw_Env *P, Buffer *buffer, struct Type *type);

#endif // PAW_ENV_H
