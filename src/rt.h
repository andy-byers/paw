// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.
#ifndef PAW_VM_H
#define PAW_VM_H

#include "env.h"
#include "paw.h"

// Immediate operands:
#define Ib() (pc += 1, (int)pc[-1])
#define Iw() (pc += 2, (int)(pc[-2] | pc[-1] << 8))

void pawR_init(paw_Env *P);

void pawR_to_integer(paw_Env *P);
void pawR_to_float(paw_Env *P);
const char *pawR_to_string(paw_Env *P, size_t *plen);

void pawR_length(paw_Env *P);
void pawR_equals(paw_Env *P);
void pawR_arith(paw_Env *P, Op op);
void pawR_compare(paw_Env *P, Op op);

void pawR_error(paw_Env *P, int status, const char *fmt, ...);
void pawR_type_error(paw_Env *P, const char *what);
void pawR_type_error2(paw_Env *P, const char *what);
void pawR_attr_error(paw_Env *P, Value attr);
void pawR_name_error(paw_Env *P, Value name);

void pawR_getattr(paw_Env *P);
int pawR_getitem(paw_Env *P);
void pawR_setattr(paw_Env *P);
void pawR_setitem(paw_Env *P);

int pawR_getattr_raw(paw_Env *P, paw_Bool fallback);
int pawR_getitem_raw(paw_Env *P, paw_Bool fallback);
void pawR_setattr_raw(paw_Env *P);
void pawR_setitem_raw(paw_Env *P);

int pawR_read_global(paw_Env *P, Value name);
void pawR_write_global(paw_Env *P, Value name, paw_Bool create);

void pawR_execute(paw_Env *P, CallFrame *cf);
void pawR_literal_array(paw_Env *P, int n);
void pawR_literal_map(paw_Env *P, int n);

void pawR_close_upvalues(paw_Env *P, const StackPtr top);

int pawR_array_insert(paw_Env *P);
int pawR_array_push(paw_Env *P);
int pawR_array_pop(paw_Env *P);
int pawR_array_clone(paw_Env *P);
int pawR_map_erase(paw_Env *P);
int pawR_map_clone(paw_Env *P);
int pawR_string_starts_with(paw_Env *P);
int pawR_string_ends_with(paw_Env *P);
int pawR_string_clone(paw_Env *P);

static inline paw_Int pawR_check_int(paw_Env *P, Value v)
{
    if (pawV_is_int(v)) {
        return pawV_get_int(v);
    } else if (!pawV_is_bigint(v)) {
        pawR_error(P, PAW_ETYPE, "expected integer");
    }
    paw_Bool lossless;
    const paw_Int ival = pawV_to_int64(v, &lossless);
    if (!lossless) {
        pawR_error(P, PAW_EOVERFLOW, "integer is too large");
    }
    return ival;
}

static inline void pawR_check_argc(paw_Env *P, int argc, int expect)
{
    if (argc != expect) {
        pawR_error(P, PAW_ERUNTIME, "expected %d argument(s) but found %d", expect, argc);
    }
}

static inline void pawR_check_varargc(paw_Env *P, int argc, int least, int most)
{
    if (argc < least) {
        pawR_error(P, PAW_ERUNTIME, "expected at least %d argument(s) but found %d", least, argc);
    } else if (argc > most) {
        pawR_error(P, PAW_ERUNTIME, "expected at most %d argument(s) but found %d", most, argc);
    }
}

#endif // PAW_VM_H
