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

// ORDER ArithOp1
#define PAW_OPNEG 0

// ORDER ArithOp2
#define PAW_OPADD 0
#define PAW_OPSUB 1
#define PAW_OPMUL 2
#define PAW_OPDIV 3
#define PAW_OPMOD 4

void paw_arithi1(paw_Env *P, int op);
void paw_arithi2(paw_Env *P, int op);
void paw_arithf1(paw_Env *P, int op);
void paw_arithf2(paw_Env *P, int op);

// ORDER BitwOp1
#define PAW_OPBNOT 0

// ORDER BitwOp2
#define PAW_OPBXOR 0
#define PAW_OPBAND 1
#define PAW_OPBOR 2
#define PAW_OPSHL 3
#define PAW_OPSHR 4

void paw_bitw1(paw_Env *P, int op);
void paw_bitw2(paw_Env *P, int op);

// ORDER CmpOp
#define PAW_OPEQ 0
#define PAW_OPNE 1
#define PAW_OPLT 2
#define PAW_OPLE 3
#define PAW_OPGT 4
#define PAW_OPGE 5

// Comparison operations
// Returns an integer, the sign of which describes the relationship between 
// the left and right operands: negative if the left operand is less than the
// right, positive  if the left operand is greater than the right, and 0 if 
// the operands are equal.
void paw_cmpi(paw_Env *P, int op);
void paw_cmpf(paw_Env *P, int op);
void paw_cmps(paw_Env *P, int op);

// ORDER BoolOp
#define PAW_OPNOT 0

void paw_boolop(paw_Env *P, int op);

// ORDER StrOp
#define PAW_SLEN 0
#define PAW_SADD 1
#define PAW_SGET 2
#define PAW_SGETN 3

void paw_strop(paw_Env *P, int op);

// ORDER ListOp
#define PAW_LLEN 0
#define PAW_LADD 1
#define PAW_LGET 2
#define PAW_LSET 3
#define PAW_LGETN 4
#define PAW_LSETN 5

void paw_listop(paw_Env *P, int op);

// ORDER MapOp
#define PAW_MLEN 0
#define PAW_MGET 1
#define PAW_MSET 2

void paw_mapop(paw_Env *P, int op);

//
// Getters (stack -> C):
//

paw_Bool paw_bool(paw_Env *P, int index);
paw_Int paw_int(paw_Env *P, int index);
paw_Float paw_float(paw_Env *P, int index);
const char *paw_string(paw_Env *P, int index);
paw_Function paw_native(paw_Env *P, int index);
void *paw_userdata(paw_Env *P, int index);
size_t paw_length(paw_Env *P, int index);

void paw_pop(paw_Env *P, int n);

// Return the number of values in the current stack frame
int paw_get_count(paw_Env *P);

// Determine the location of a global in the loaded module
// Expects a string on top of the stack (which is consumed), indicating the name of 
// the global. Returns a nonnegative integer if the global exists, -1 otherwise.
int paw_find_global(paw_Env *P);

// Get the global with the given identifier
// The 'gid' must be a nonnegative integer returned by 'paw_find_global'.
void paw_get_global(paw_Env *P, int gid);

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
