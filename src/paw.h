// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
//
// paw API:
//
// Most API functions operate on values located on the runtime stack. The
// particular value is specified by an `int index` parameter. When nonnegative,
// the index specifies a value relative to the base of the current call frame,
// with an index of 0 representing the first value. Negative indices count
// backward from the last value pushed onto the stack, with -1 representing the
// most-recently-pushed value. When a function is called from paw, slot 0
// contains the function itself. When a method is called, slot 0 contains the
// implicit 'self' parameter.
//
#ifndef PAW_PAW_H
#define PAW_PAW_H

#include "config.h"
#include <stdarg.h>
#include <stddef.h>

typedef int paw_Type;
typedef int64_t paw_Int;
typedef double paw_Float;
typedef _Bool paw_Bool;

#define PAW_FALSE 0
#define PAW_TRUE 1

typedef struct paw_Env paw_Env;

typedef void *(*paw_Alloc)(void *ud, void *ptr, size_t size0, size_t size);
typedef const char *(*paw_Reader)(paw_Env *P, void *ud, size_t *size);
typedef int (*paw_Function)(paw_Env *P);

struct paw_Options {
    paw_Alloc alloc;
    size_t heap_size;
    void *heap;
    void *ud;
};
paw_Env *paw_open(const struct paw_Options *o);
void paw_close(paw_Env *P);

paw_Alloc paw_get_allocator(paw_Env *P);
void paw_set_allocator(paw_Env *P, paw_Alloc alloc, void *ud);
size_t paw_bytes_used(const paw_Env *P);

#define PAW_OK 0
#define PAW_EMEMORY 1
#define PAW_EVALUE 2
#define PAW_ETYPE 3
#define PAW_ENAME 4
#define PAW_EATTR 5
#define PAW_EKEY 6
#define PAW_EINDEX 7
#define PAW_EOVERFLOW 8
#define PAW_ESYSTEM 9
#define PAW_ESYNTAX 10
#define PAW_ERUNTIME 11

// Load paw source code from the given 'input' source
// Creates a function object containing the code and pushes it onto the stack.
int paw_load(paw_Env *P, paw_Reader input, const char *name, void *ud);

// Invoke the paw runtime on a function with 'argc' parameters
// The function object should be on the stack followed the the parameters, with
// the last parameter on top.
int paw_call(paw_Env *P, int argc);

// Type kinds
#define PAW_TKBASIC 0
#define PAW_TKTUPLE 1
#define PAW_TKENUM 2
#define PAW_TKSTRUCT 3
#define PAW_TKFUNCTION 4
#define PAW_TKFOREIGN 5
#define PAW_NTYPEKINDS 6

// ORDER BuiltinKind
#define PAW_TUNIT 0
#define PAW_TBOOL 1
#define PAW_TINT 2
#define PAW_TFLOAT 3
#define PAW_TSTR 4
#define PAW_NTYPES 5

void paw_push_value(paw_Env *P, int index);
void paw_push_zero(paw_Env *P, int n);
void paw_push_bool(paw_Env *P, paw_Bool b);
void paw_push_int(paw_Env *P, paw_Int i);
void paw_push_float(paw_Env *P, paw_Float f);
void paw_push_function(paw_Env *P, paw_Function fn, int n);
const char *paw_push_string(paw_Env *P, const char *s);
const char *paw_push_nstring(paw_Env *P, const char *s, size_t n);
const char *paw_push_fstring(paw_Env *P, const char *fmt, ...);
const char *paw_push_vfstring(paw_Env *P, const char *fmt, va_list arg);

enum paw_CmpOp {
    PAW_CMP_EQ,
    PAW_CMP_NE,
    PAW_CMP_LT,
    PAW_CMP_LE,
    PAW_CMP_GT,
    PAW_CMP_GE,
};

// Comparison operations
// Returns an integer, the sign of which describes the relationship between 
// the left and right operands: negative if the left operand is less than the
// right, positive  if the left operand is greater than the right, and 0 if 
// the operands are equal.
void paw_cmpi(paw_Env *P, enum paw_CmpOp op);
void paw_cmpf(paw_Env *P, enum paw_CmpOp op);
void paw_cmps(paw_Env *P, enum paw_CmpOp op);

enum paw_ArithOp {
    PAW_ARITH_NEG,
    PAW_ARITH_ADD,
    PAW_ARITH_SUB,
    PAW_ARITH_MUL,
    PAW_ARITH_DIV,
    PAW_ARITH_MOD,
};

void paw_arithi(paw_Env *P, enum paw_ArithOp op);
void paw_arithf(paw_Env *P, enum paw_ArithOp op);

enum paw_BitwOp {
    PAW_BITW_NOT,
    PAW_BITW_XOR,
    PAW_BITW_AND,
    PAW_BITW_OR,
    PAW_BITW_SHL,
    PAW_BITW_SHR,
};

void paw_bitw(paw_Env *P, enum paw_BitwOp op);

enum paw_BoolOp {
    PAW_BOOL_NOT,
};

enum paw_StrOp {
    PAW_STR_LEN,
    PAW_STR_CONCAT,
    PAW_STR_GET,
    PAW_STR_GETN,
};

enum paw_ListOp {
    PAW_LIST_LEN,
    PAW_LIST_CONCAT,
    PAW_LIST_GET,
    PAW_LIST_SET,
    PAW_LIST_GETN,
    PAW_LIST_SETN,
};

enum paw_MapOp {
    PAW_MAP_LEN,
    PAW_MAP_GET,
    PAW_MAP_SET,
};

void paw_boolop(paw_Env *P, enum paw_BoolOp op);
void paw_strop(paw_Env *P, enum paw_StrOp op);
void paw_listop(paw_Env *P, enum paw_ListOp op);
void paw_mapop(paw_Env *P, enum paw_MapOp op);

//
// Getters (stack -> C):
//

paw_Bool paw_bool(paw_Env *P, int index);
paw_Int paw_int(paw_Env *P, int index);
paw_Float paw_float(paw_Env *P, int index);
const char *paw_string(paw_Env *P, int index);
paw_Function paw_native(paw_Env *P, int index);
void *paw_userdata(paw_Env *P, int index);

void paw_pop(paw_Env *P, int n);

// Return the number of values in the current stack frame
int paw_get_count(paw_Env *P);

int paw_mangle_name(paw_Env *P, paw_Type *types);

struct paw_Item {
    int global_id;
    paw_Type type;
};

// Get information about a toplevel item in the loaded module
// Expects a string on top of the stack (which is consumed), indicating the mangled
// name of the item. Returns PAW_OK on success, an error code otherwise.
int paw_lookup_item(paw_Env *P, struct paw_Item *pitem);

// Routines for working with toplevel types

// Push a human-readable string representation of a type on to the stack
void paw_get_typename(paw_Env *P, paw_Type type);

// Routines for working with global values

// Push a global value on to the stack
void paw_get_global(paw_Env *P, int gid);

// Call a global function
void paw_call_global(paw_Env *P, int gid, int argc);

void paw_get_upvalue(paw_Env *P, int index, int iup);
void paw_get_field(paw_Env *P, int index, int ifield);

void paw_set_upvalue(paw_Env *P, int index, int iup);
void paw_set_field(paw_Env *P, int index, int ifield);

void *paw_new_foreign(paw_Env *P, size_t size, int nfields);
void paw_new_native(paw_Env *P, paw_Function f, int nup);
void paw_new_list(paw_Env *P, int n);
void paw_new_map(paw_Env *P, int n);

int paw_abs_index(paw_Env *P, int index);
void paw_rotate(paw_Env *P, int index, int n);
void paw_shift(paw_Env *P, int n);
void paw_copy(paw_Env *P, int from, int to);

// Move the top stack value to the given index
// Shifts elements above the target index up by 1 slot.
static inline void paw_insert(paw_Env *P, int index)
{
    paw_rotate(P, index, 1);
}

// Remove the given slot value
static inline void paw_remove(paw_Env *P, int index)
{
    paw_rotate(P, index, -1);
    paw_pop(P, 1);
}

// Replace the given slot value with the top
static inline void paw_replace(paw_Env *P, int index)
{
    paw_copy(P, -1, index);
    paw_pop(P, 1);
}

#endif // PAW_PAW_H
