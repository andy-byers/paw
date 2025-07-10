// Copyright (c) 2024, The paw Authors. All rights reserved.
// This source code is licensed under the MIT License, which can be found in
// LICENSE.md. See AUTHORS.md for a list of contributor names.

#include "prefix.h"

#include "api.h"
#include "auxlib.h"
#include "call.h"
#include "env.h"
#include "lib.h"
#include "list.h"
#include "map.h"
#include "os.h"
#include "rt.h"
#include <stdio.h>
#include <errno.h>

#define CF_BASE(i) (P->cf->base.p + i)

static int base_panic(paw_Env *P)
{
    // string argument on top of the stack
    pawC_throw(P, PAW_ERUNTIME);
}

static int base_assert(paw_Env *P)
{
    if (!paw_bool(P, 1)) {
        // TODO: pass source text of false expression as second argument, display here
        pawR_error(P, PAW_ERUNTIME, "assertion failed");
    }
    return 0;
}

static int base_print(paw_Env *P)
{
    char const *string = paw_str(P, 1);
    size_t const length = paw_str_rawlen(P, 1);
    pawO_write_all(P, pawO_stdout(), string, length);
    pawO_flush(pawO_stdout());
    return 0;
}

static int base_println(paw_Env *P)
{
    paw_str_appendc(P, 1, '\n');
    return base_print(P);
}

static void push_values(paw_Env *P, Value const *pvalue, int count)
{
    API_INCR_TOP(P, count);
    pawV_copy(P->top.p - count, pvalue, count);
}

static void push_option_some(paw_Env *P, Value const *pvalue, int count)
{
    paw_push_int(P, PAW_OPTION_SOME);
    push_values(P, pvalue, count);
}

static void push_option_none(paw_Env *P, int count)
{
    paw_push_int(P, PAW_OPTION_NONE);
    paw_push_zero(P, count);
}

static int char_to_str(paw_Env *P)
{
    char const c = (char)CF_BASE(1)->c;
    Str *str = pawS_new_nstr(P, &c, 1);
    push_values(P, &P2V(str), 1);
    return 1;
}

static char const *find_substr(char const *str, size_t nstr, char const *sub, size_t nsub)
{
    if (nsub == 0)
        return str;

    char const *end = str + nstr;
    while ((str = strchr(str, sub[0]))) {
        if (nsub <= CAST_SIZE(end - str)
                && memcmp(str, sub, nsub) == 0) {
            return str;
        }
        str += nsub;
    }
    return NULL;
}

static int str_find(paw_Env *P)
{
    Str const *str = V_STR(*CF_BASE(1));
    Str const *find = V_STR(*CF_BASE(2));
    char const *result = find_substr(
        str->text, str->length,
        find->text, find->length);
    if (result) { // index of substring
        paw_push_int(P, PAW_OPTION_SOME);
        paw_push_int(P, result - str->text);
    } else { // not found
        push_option_none(P, 1);
    }
    return 1 + 1;
}

static int str_split(paw_Env *P)
{
    Str const *sep = V_STR(*CF_BASE(2));
    Str *str = V_STR(*CF_BASE(1));
    if (sep->length == 0) {
        pawR_error(P, PAW_EVALUE, "empty separator");
    }

    int npart = 0;
    char const *part;
    size_t nstr = str->length;
    char const *pstr = str->text;
    while ((part = find_substr(pstr, nstr, sep->text, sep->length))) {
        if (npart == INT_MAX)
            pawR_error(P, PAW_EOVERFLOW, "too many substrings");
        size_t const n = CAST_SIZE(part - pstr);
        pawC_pushns(P, pstr, n);
        part += sep->length; // skip separator
        pstr = part;
        nstr -= n;
        ++npart;
    }
    char const *end = str->text + str->length; // add the rest
    pawC_pushns(P, pstr, CAST_SIZE(end - pstr));
    ++npart;

    paw_new_list(P, npart, 1);
    return 1;
}

static int str_join(paw_Env *P)
{
    Str *sep = V_STR(*CF_BASE(1));

    Buffer buf;
    pawL_init_buffer(P, &buf);
    paw_push_int(P, PAW_ITER_INIT);
    while (paw_list_next(P, 2)) {
        char const *chunk = paw_str(P, -1);
        paw_Int const chunklen = paw_str_rawlen(P, -1);
        pawL_add_nstring(P, &buf, chunk, chunklen);
        L_ADD_STRING(P, &buf, sep);
        paw_pop(P, 1);
    }
    paw_pop(P, 1);

    if (buf.size > 0) {
        // remove the last separator
        paw_assert(buf.size >= sep->length);
        buf.size -= sep->length;
    }
    pawL_push_result(P, &buf);
    return 1;
}

static int str_starts_with(paw_Env *P)
{
    Str const *str = V_STR(*CF_BASE(1));
    Str const *prefix = V_STR(*CF_BASE(2));
    size_t const prelen = prefix->length;
    paw_Bool const b = str->length >= prelen
        && 0 == memcmp(prefix->text, str->text, prelen);
    paw_push_bool(P, b);
    return 1;
}

static int str_ends_with(paw_Env *P)
{
    Str const *str = V_STR(*CF_BASE(1));
    Str const *suffix = V_STR(*CF_BASE(2));
    size_t const suflen = suffix->length;
    paw_Bool b = PAW_FALSE;
    if (str->length >= suflen) {
        char const *ptr = str->text + str->length - suflen;
        b = 0 == memcmp(suffix->text, ptr, suflen);
    }
    paw_push_bool(P, b);
    return 1;
}

static int bool_to_str(paw_Env *P)
{
    Value *pv = CF_BASE(1);
    pawV_to_str(P, pv, PAW_TBOOL, NULL);
    return 1;
}

static int int_to_str(paw_Env *P)
{
    paw_int_to_str(P, -1, NULL);
    return 1;
}

static int float_hash(paw_Env *P)
{
    // reinterpret as integer, but make -0.0 equal to 0.0
    if (paw_float(P, 1) == 0.0)
        paw_push_float(P, 0.0);
    return 1;
}

static int float_to_str(paw_Env *P)
{
    paw_float_to_str(P, -1, NULL);
    return 1;
}

static int str_hash(paw_Env *P)
{
    // reinterpret pointer to interned string as integer
    PAW_UNUSED(P);
    return 1;
}

static int str_parse_float(paw_Env *P)
{
    Value result;
    char const *str = paw_str(P, 1);
    int const status = pawV_parse_float(P, str, &V_FLOAT(result));
    if (status == PAW_OK) {
        push_option_some(P, &result, 1);
    } else {
        push_option_none(P, 1);
    }
    return 1 + 1;
}

static int str_parse_int_radix(paw_Env *P)
{
    char const *str = paw_str(P, 1);
    paw_Int const base = paw_int(P, 2);

    Value result;
    int const status = pawV_parse_int(P, str, CAST(int, base), &V_INT(result));
    if (status == PAW_OK) {
        push_option_some(P, &result, 1);
    } else {
        push_option_none(P, 1);
    }
    return 1 + 1;
}

static int str_parse_int(paw_Env *P)
{
    paw_push_int(P, 10);
    return str_parse_int_radix(P);
}

void l_import_prelude(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    pawL_add_extern_fn(P, "prelude", "panic", base_panic);
    pawL_add_extern_fn(P, "prelude", "assert", base_assert);
    pawL_add_extern_fn(P, "prelude", "print", base_print);
    pawL_add_extern_fn(P, "prelude", "println", base_println);
    pawL_add_extern_method(P, "prelude", "bool", "to_str", bool_to_str);
    pawL_add_extern_method(P, "prelude", "int", "to_str", int_to_str);
    pawL_add_extern_method(P, "prelude", "float", "hash", float_hash);
    pawL_add_extern_method(P, "prelude", "float", "to_str", float_to_str);
    pawL_add_extern_method(P, "prelude", "char", "to_str", char_to_str);
    pawL_add_extern_method(P, "prelude", "str", "hash", str_hash);
    pawL_add_extern_method(P, "prelude", "str", "parse_int", str_parse_int);
    pawL_add_extern_method(P, "prelude", "str", "parse_int_radix", str_parse_int_radix);
    pawL_add_extern_method(P, "prelude", "str", "parse_float", str_parse_float);
    pawL_add_extern_method(P, "prelude", "str", "split", str_split);
    pawL_add_extern_method(P, "prelude", "str", "join", str_join);
    pawL_add_extern_method(P, "prelude", "str", "find", str_find);
    pawL_add_extern_method(P, "prelude", "str", "starts_with", str_starts_with);
    pawL_add_extern_method(P, "prelude", "str", "ends_with", str_ends_with);
    paw_pop(P, 1); // paw.symbols

    pawL_file_reader(P, PAWL_STDLIB_PATH(PAWL_PRELUDE_NAME));
}
