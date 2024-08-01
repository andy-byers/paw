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

paw_Env *paw_open(paw_Alloc alloc, void *ud);
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

// Builtin types
// ORDER ValueType
#define PAW_TUNIT 0
#define PAW_TBOOL 1
#define PAW_TINT 2
#define PAW_TFLOAT 3
#define PAW_TSTRING 4
#define PAW_TVECTOR 5
#define PAW_TMAP 6
#define PAW_TTUPLE 7
#define PAW_TENUM 8
#define PAW_TSTRUCT 9
#define PAW_TFUNCTION 10
#define PAW_TFOREIGN 11
#define PAW_TMODULE 12
#define PAW_NTYPES 13

void paw_push_value(paw_Env *P, int index);
void paw_push_unit(paw_Env *P, int n);
void paw_push_bool(paw_Env *P, paw_Bool b);
void paw_push_int(paw_Env *P, paw_Int i);
void paw_push_float(paw_Env *P, paw_Float f);
void paw_push_function(paw_Env *P, paw_Function fn, int n);
const char *paw_push_string(paw_Env *P, const char *s);
const char *paw_push_nstring(paw_Env *P, const char *s, size_t n);
const char *paw_push_fstring(paw_Env *P, const char *fmt, ...);
const char *paw_push_vfstring(paw_Env *P, const char *fmt, va_list arg);

#define PAW_OPLEN 0
#define PAW_OPNEG 1
#define PAW_OPNOT 2
#define PAW_OPBNOT 3

void paw_unop(paw_Env *P, int op);

#define PAW_OPEQ 0
#define PAW_OPNE 1
#define PAW_OPLT 2
#define PAW_OPLE 3
#define PAW_OPGT 4
#define PAW_OPGE 5
#define PAW_OPIN 6
#define PAW_OPADD 7
#define PAW_OPSUB 8
#define PAW_OPMUL 9
#define PAW_OPDIV 10
#define PAW_OPIDIV 11
#define PAW_OPMOD 12
#define PAW_OPPOW 13
#define PAW_OPCONCAT 14
#define PAW_OPBXOR 15
#define PAW_OPBAND 16
#define PAW_OPBOR 17
#define PAW_OPSHL 18
#define PAW_OPSHR 19

void paw_binop(paw_Env *P, int op);
void paw_raw_equals(paw_Env *P);

void paw_arith_int(paw_Env *P, int op);
void paw_arith_float(paw_Env *P, int op);
void paw_arith_string(paw_Env *P, int op);

void paw_compare_int(paw_Env *P, int op);
void paw_compare_float(paw_Env *P, int op);
void paw_compare_string(paw_Env *P, int op);

void paw_eq_i(paw_Env *P);
void paw_eq_f(paw_Env *P);
void paw_eq_s(paw_Env *P);
void paw_eq_v(paw_Env *P);
void paw_eq_m(paw_Env *P);

//
// Getters (stack -> C):
//

paw_Bool paw_bool(paw_Env *P, int index);
paw_Int paw_int(paw_Env *P, int index);
paw_Float paw_float(paw_Env *P, int index);
const char *paw_string(paw_Env *P, int index);
paw_Function paw_native(paw_Env *P, int index);
void *paw_pointer(paw_Env *P, int index);
size_t paw_length(paw_Env *P, int index);

//
// Type conversions:
//
void paw_to_float(paw_Env *P, int index);
void paw_to_int(paw_Env *P, int index);
void paw_to_string(paw_Env *P, int index);

void paw_pop(paw_Env *P, int n);

// Return the number of values in the current stack frame
int paw_get_count(paw_Env *P);

int paw_find_public(paw_Env *P);
void paw_push_public(paw_Env *P, int id);

int paw_find_attr(paw_Env *P, int index, const char *name);

void paw_get_upvalue(paw_Env *P, int ifn, int index);
void paw_get_global(paw_Env *P, int index);
void paw_get_attr(paw_Env *P, int index, int iattr);
void paw_get_item(paw_Env *P, int index);
void paw_get_itemi(paw_Env *P, int index, paw_Int i);

paw_Bool paw_check_global(paw_Env *P, const char *name);
paw_Bool paw_check_attr(paw_Env *P, int index, const char *s);
paw_Bool paw_check_item(paw_Env *P, int index);
paw_Bool paw_check_itemi(paw_Env *P, int index, paw_Int i);

void paw_list_slice(paw_Env *P, int index, paw_Int begin, paw_Int end);
void paw_list_push(paw_Env *P, int index, paw_Int i);
void paw_list_pop(paw_Env *P, int index, paw_Int i);

void paw_map_erase(paw_Env *P, int index);
void paw_map_erasei(paw_Env *P, int index, paw_Int i);

// Push a global variable onto the stack, or null if the variable does
// not exist
// Returns PAW_TRUE if the variable exists, PAW_FALSE otherwise.
paw_Bool paw_check_global(paw_Env *P, const char *name);

void paw_set_upvalue(paw_Env *P, int ifn, int index);
void paw_set_global(paw_Env *P, const char *name);
void paw_set_attr(paw_Env *P, int index, const char *s);
void paw_set_item(paw_Env *P, int index);
void paw_set_itemi(paw_Env *P, int index, paw_Int i);
void paw_call_global(paw_Env *P, int index, int argc);
void paw_call_attr(paw_Env *P, int index, const char *name, int argc);

void *paw_create_foreign(paw_Env *P, size_t size, int nbound);
void paw_create_native(paw_Env *P, paw_Function f, int nup);
void paw_create_class(paw_Env *P);
void paw_create_instance(paw_Env *P, int index);
void paw_create_array(paw_Env *P, int n);
void paw_create_map(paw_Env *P, int n);

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
