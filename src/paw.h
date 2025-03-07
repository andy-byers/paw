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
// contains the function object itself, followed by the arguments.
//
#ifndef PAW_PAW_H
#define PAW_PAW_H

#include "config.h"
#include <stdarg.h>
#include <stddef.h>

#define PAW_REGISTRY_INDEX (-PAW_STACK_MAX - 1000)

typedef int paw_Type;
typedef long long paw_Int;
typedef unsigned long long paw_Uint;
typedef double paw_Float;
typedef _Bool paw_Bool;

#define PAW_FALSE 0
#define PAW_TRUE 1

typedef struct paw_Env paw_Env;

typedef void *(*paw_Alloc)(void *ud, void *ptr, size_t size0, size_t size);
typedef void (*paw_MemHook)(void *ud, void *ptr, size_t size0, size_t size);
typedef char const *(*paw_Reader)(paw_Env *P, void *ud, size_t *size);
typedef int (*paw_Function)(paw_Env *P);

struct paw_Options {
    paw_Alloc alloc;
    paw_MemHook mem_hook;
    size_t heap_size;
    void *heap;
    void *ud;
};
paw_Env *paw_open(struct paw_Options const *o);
void paw_close(paw_Env *P);

paw_Alloc paw_get_allocator(paw_Env *P);
void paw_set_allocator(paw_Env *P, paw_Alloc alloc, void *ud);
size_t paw_bytes_used(paw_Env const *P);

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
int paw_load(paw_Env *P, paw_Reader input, char const *name, void *ud);

// Call a function with 'argc' arguments
// The function object should be on the stack followed by the arguments, with
// the last argument on top.
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

void paw_push_value(paw_Env *P, int index);
void paw_push_zero(paw_Env *P, int n);
void paw_push_bool(paw_Env *P, paw_Bool b);
void paw_push_int(paw_Env *P, paw_Int i);
void paw_push_float(paw_Env *P, paw_Float f);
void paw_push_rawptr(paw_Env *P, void *ptr);
char const *paw_push_string(paw_Env *P, char const *s);
char const *paw_push_nstring(paw_Env *P, char const *s, size_t n);
char const *paw_push_fstring(paw_Env *P, char const *fmt, ...);
char const *paw_push_vfstring(paw_Env *P, char const *fmt, va_list arg);

#define PAW_PUSH_LITERAL(P, s) paw_push_nstring(P, "" s, sizeof(s) - 1)

//
// Getters (stack -> C):
//

paw_Bool paw_bool(paw_Env *P, int index);
paw_Int paw_int(paw_Env *P, int index);
paw_Uint paw_uint(paw_Env *P, int index);
paw_Float paw_float(paw_Env *P, int index);
char const *paw_string(paw_Env *P, int index);
paw_Function paw_native(paw_Env *P, int index);
void *paw_userdata(paw_Env *P, int index);
void *paw_rawptr(paw_Env *P, int index);

// Get a pointer to the internal representation of an object
// Value must be of object type, e.g. a list or a function.
void *paw_pointer(paw_Env *P, int index);

void paw_pop(paw_Env *P, int n);

// Return the number of values in the current stack frame
int paw_get_count(paw_Env *P);

void paw_str_length(paw_Env *P, int index);
void paw_str_concat(paw_Env *P, int count);
void paw_str_get(paw_Env *P, int index);
void paw_str_getn(paw_Env *P, int index);

// Initializer for iterator state variables
#define PAW_ITER_INIT PAW_CAST_INT(-1)

void paw_list_length(paw_Env *P, int index);
void paw_list_concat(paw_Env *P, int count);
void paw_list_get(paw_Env *P, int index);
void paw_list_set(paw_Env *P, int index);
void paw_list_getn(paw_Env *P, int index);
void paw_list_setn(paw_Env *P, int index);
paw_Bool paw_list_next(paw_Env *P, int index);

void paw_map_length(paw_Env *P, int index);
int paw_map_get(paw_Env *P, int index);
void paw_map_set(paw_Env *P, int index);
paw_Bool paw_map_next(paw_Env *P, int index);

inline static paw_Int paw_str_rawlen(paw_Env *P, int index)
{
    paw_str_length(P, index);
    paw_Int const n = paw_int(P, -1);
    paw_pop(P, 1);
    return n;
}

inline static paw_Int paw_list_rawlen(paw_Env *P, int index)
{
    paw_list_length(P, index);
    paw_Int const n = paw_int(P, -1);
    paw_pop(P, 1);
    return n;
}

inline static paw_Int paw_map_rawlen(paw_Env *P, int index)
{
    paw_map_length(P, index);
    paw_Int const n = paw_int(P, -1);
    paw_pop(P, 1);
    return n;
}

void paw_mangle_start(paw_Env *P);
void paw_mangle_add_module(paw_Env *P);
void paw_mangle_add_name(paw_Env *P);
void paw_mangle_add_args(paw_Env *P, paw_Type *types);

void paw_load_symbols(paw_Env *P, int index);

struct paw_Item {
    int global_id;
    paw_Type type;
};

// Get information about a toplevel item in the loaded module
// Expects a string on top of the stack (which is consumed), indicating the mangled
// name of the item. Returns PAW_OK on success, an error code otherwise.
int paw_lookup_item(paw_Env *P, int index, struct paw_Item *pitem);

// Routines for working with toplevel types

// Push a human-readable string representation of a type on to the stack
void paw_get_typename(paw_Env *P, paw_Type type);

// Routines for working with global values

// Push a global value on to the stack
void paw_get_global(paw_Env *P, int gid);

// Call a global function
int paw_call_global(paw_Env *P, int gid, int argc);

void paw_get_upvalue(paw_Env *P, int index, int iup);
void paw_get_field(paw_Env *P, int index, int ifield);

void paw_set_upvalue(paw_Env *P, int index, int iup);
void paw_set_field(paw_Env *P, int index, int ifield);

void *paw_new_foreign(paw_Env *P, size_t size, int nfields);
void paw_new_native(paw_Env *P, paw_Function f, int nup);
void paw_new_list(paw_Env *P, int n);
void paw_new_map(paw_Env *P, int n, paw_Type k);

int paw_abs_index(paw_Env *P, int index);
void paw_rotate(paw_Env *P, int index, int n);
void paw_shift(paw_Env *P, int n);
void paw_copy(paw_Env *P, int from, int to);

// Move the value on top of the stack to the given index
// Shifts elements including and above the target index up by 1 slot, increasing
// the size of the stack by 1.
inline static void paw_insert(paw_Env *P, int index)
{
    paw_rotate(P, index, 1);
}

// Remove the given value
// Shifts elements above the value down by 1 slot, reducing the size of the stack
// by 1.
inline static void paw_remove(paw_Env *P, int index)
{
    paw_rotate(P, index, -1);
    paw_pop(P, 1);
}

// Replace the given value with the top
// Reduces the size of the stack by 1.
inline static void paw_replace(paw_Env *P, int index)
{
    paw_copy(P, -1, index);
    paw_pop(P, 1);
}

char const *paw_int_to_string(paw_Env *P, int index, size_t *plen);
char const *paw_float_to_string(paw_Env *P, int index, size_t *plen);

#endif // PAW_PAW_H
