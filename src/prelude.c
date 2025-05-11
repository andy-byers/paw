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
    char const *string = paw_string(P, 1);
    size_t const length = paw_str_rawlen(P, 1);
    pawO_write_all(P, pawO_stdout(), string, length);
    pawO_flush(pawO_stdout());
    return 0;
}

#define STOP_LOOP(a, b, c) \
    (((c) < 0 && (a) <= (b)) || ((c) > 0 && (a) >= (b)))

static int range_iterator(paw_Env *P)
{
    paw_get_upvalue(P, 0, 0);
    paw_get_upvalue(P, 0, 1);
    paw_get_upvalue(P, 0, 2);
    paw_Int const begin = paw_int(P, 1);
    paw_Int const end = paw_int(P, 2);
    paw_Int const step = paw_int(P, 3);
    Value *pval = P->top.p++;
    if (!STOP_LOOP(begin, end, step)) {
        paw_push_int(P, begin + step);
        paw_set_upvalue(P, 0, 0);
        pawR_new_tuple(P, P->cf, pval, 2);
        V_TUPLE(*pval)->elems[0].i = 0;
        V_TUPLE(*pval)->elems[1].i = begin;
    } else {
        pawR_new_tuple(P, P->cf, pval, 1);
        V_TUPLE(*pval)->elems[0].i = 1;
    }
    return 1;
}

static int base_range(paw_Env *P)
{
    paw_Int const begin = paw_int(P, 1);
    paw_Int const end = paw_int(P, 2);
    paw_Int const step = paw_int(P, 3);
    if (step == 0)
        pawR_error(P, PAW_EVALUE, "step cannot be 0");
    paw_new_native(P, range_iterator, 3);
    return 1;
}

static void new_option_some(paw_Env *P, Value *presult, Value const *pvalue, int count)
{
    V_SET_INT(presult, PAW_OPTION_SOME);
    while (count-- > 0)
        *++presult = *pvalue++;

}

static void new_option_none(paw_Env *P, Value *presult)
{
    V_SET_INT(presult, PAW_OPTION_NONE);
}

static int list_insert(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int const index = paw_int(P, 2);
    pawList_insert(P, list, index, CF_BASE(3));
    paw_pop(P, 2); // return 'self'
    return 1;
}

static int enum_unwrap(paw_Env *P)
{
    Value const v = *CF_BASE(1);
    if (V_INT(*CF_BASE(1)) != 0)
        pawR_error(P, PAW_ERUNTIME, "failed to unwrap");
    return paw_get_count(P) - 2; // callable + discriminant
}

static int result_unwrap_err(paw_Env *P)
{
    Value const v = *CF_BASE(1);
    if (V_INT(*CF_BASE(1)) == 0)
        pawR_error(P, PAW_ERUNTIME, "failed to unwrap error");
    return paw_get_count(P) - 2; // callable + discriminant
}

static int list_length(paw_Env *P)
{
    paw_list_length(P, 1);
    return 1;
}

static int list_push(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    pawList_push(P, list, CF_BASE(2));
    paw_pop(P, 1); // return 'self'
    return 1;
}

static int list_pop(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int const length = pawList_length(P, list);
    if (length == 0) {
        pawR_error(P, PAW_EVALUE, "pop from empty List");
    }
    // overwrites the register containing the list, which is fine because
    // pawList_pop doesn't allocate
    *CF_BASE(1) = *pawList_get(P, list, length - 1);
    pawList_pop(P, list, length - 1);
    return 1;
}

static paw_Int clamped_index(paw_Env *P, int loc, paw_Int n)
{
    paw_Int const i = V_INT(*CF_BASE(loc));
    return i < 0 ? 0 : i >= n ? n - 1
                              : i;
}

static int list_get(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int const index = V_INT(*CF_BASE(2));
    int const z = LIST_ZELEMENT(list);
    Value const *pvalue = pawList_get(P, list, index);
    pawV_copy(CF_BASE(0), pvalue, z);
    return z;
}

static int list_set(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    paw_Int const index = V_INT(*CF_BASE(2));
    Value const *pvalue = CF_BASE(3);
    int const z = LIST_ZELEMENT(list);
    Value *pslot = pawList_get(P, list, index);
    pawV_copy(pslot, pvalue, z);
    return z;
}

// TODO: It would be nice to let pop() take an optional parameter indicating the
//       index at which to erase an element. To me, 'remove' seems like it
//       should remove the first matching element using something akin to
//       operator==. Attempting this will break, since we have no concept of
//       equality between user-defined types right now.
static int list_remove(paw_Env *P)
{
    Tuple *list = V_TUPLE(*CF_BASE(1));
    if (pawList_length(P, list) == 0) {
        pawR_error(P, PAW_EVALUE, "remove from empty List");
    }
    paw_Int const index = paw_int(P, 2);
    int const z = LIST_ZELEMENT(list);
    Value const *pvalue = pawList_get(P, list, index);
    pawV_copy(CF_BASE(0), pvalue, z);
    pawList_pop(P, list, index);
    return z;
}

static int map_get(paw_Env *P)
{
    Tuple *map = V_TUPLE(*CF_BASE(1));
    Value const *pkey = CF_BASE(2);
    int const z = pawMap_value_size(P, map);
    Value const *pvalue = pawMap_get(P, map, pkey);

    P->top.p = CF_BASE(0);
    if (pvalue != NULL) {
        paw_push_int(P, PAW_OPTION_SOME);
        pawV_copy(CF_BASE(1), pvalue, z);
    } else {
        paw_push_int(P, PAW_OPTION_NONE);
    }
    P->top.p = CF_BASE(z);
    return z;
}

static int map_set(paw_Env *P)
{
    Tuple *map = V_TUPLE(*CF_BASE(1));
    Value const *pkey = CF_BASE(2);
    Value const *pvalue = CF_BASE(3);
    Value const *preplaced = pawMap_insert(P, map, pkey, pvalue);
    (void)preplaced; // TODO: return Option<V>
    return 0;
}

static int map_iter_next(paw_Env *P)
{
    paw_get_field(P, 1, 0); // 2: map
    paw_get_field(P, 1, 1); // 3: index

    StackPtr ra = CF_BASE(2);
    StackPtr rb = CF_BASE(3);
    Tuple *map = V_TUPLE(*ra);

    int const key_size = pawMap_key_size(P, map);
    paw_Int index = V_INT(*rb);
    if (pawMap_iter(map, &index)) {
        V_SET_INT(rb, index);
        paw_set_field(P, 1, 1);

        Value const *key = pawMap_key(P, map, index);
        new_option_some(P, CF_BASE(0), key, key_size);
    } else {
        new_option_none(P, CF_BASE(0));
    }
    P->top.p = CF_BASE(1 + key_size);
    return 1 + key_size;
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

static int string_find(paw_Env *P)
{
    String const *str = V_STRING(*CF_BASE(1));
    String const *find = V_STRING(*CF_BASE(2));
    char const *result = find_substr(
        str->text, str->length,
        find->text, find->length);
    if (result) { // index of substring
        V_SET_INT(P->top.p - 1, result - str->text);
    } else { // not found
        V_SET_INT(P->top.p - 1, -1);
    }
    return 1;
}

static int string_split(paw_Env *P)
{
    String const *sep = V_STRING(*CF_BASE(2));
    String *str = V_STRING(*CF_BASE(1));
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

static int string_join(paw_Env *P)
{
    String *sep = V_STRING(*CF_BASE(1));

    Buffer buf;
    pawL_init_buffer(P, &buf);
    paw_push_int(P, PAW_ITER_INIT);
    while (paw_list_next(P, 2)) {
        char const *chunk = paw_string(P, -1);
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

static int string_starts_with(paw_Env *P)
{
    String const *str = V_STRING(*CF_BASE(1));
    String const *prefix = V_STRING(*CF_BASE(2));
    size_t const prelen = prefix->length;
    paw_Bool const b = str->length >= prelen
        && 0 == memcmp(prefix->text, str->text, prelen);
    V_SET_BOOL(P->top.p - 1, b);
    return 1;
}

static int string_ends_with(paw_Env *P)
{
    String const *str = V_STRING(*CF_BASE(1));
    String const *suffix = V_STRING(*CF_BASE(2));
    size_t const suflen = suffix->length;
    paw_Bool b = PAW_FALSE;
    if (str->length >= suflen) {
        char const *ptr = str->text + str->length - suflen;
        b = 0 == memcmp(suffix->text, ptr, suflen);
    }
    V_SET_BOOL(P->top.p - 1, b);
    return 1;
}

static int bool_to_string(paw_Env *P)
{
    Value *pv = CF_BASE(1);
    pawV_to_string(P, pv, PAW_TBOOL, NULL);
    return 1;
}

static int int_to_string(paw_Env *P)
{
    Value *pv = CF_BASE(1);
    pawV_to_string(P, pv, PAW_TINT, NULL);
    return 1;
}

static int float_hash(paw_Env *P)
{
    // reinterpret as integer
    PAW_UNUSED(P);
    return 1;
}

static int float_to_string(paw_Env *P)
{
    Value *pv = CF_BASE(1);
    pawV_to_string(P, pv, PAW_TFLOAT, NULL);
    return 1;
}

static int string_hash(paw_Env *P)
{
    // reinterpret pointer to interned string as integer
    PAW_UNUSED(P);
    return 1;
}

static int string_parse_float(paw_Env *P)
{
    paw_Float f;
    char const *str = paw_string(P, 1);
    int const status = pawV_parse_float(P, str, &f);
    if (status != PAW_OK)
        pawR_error(P, PAW_ESYNTAX, "invalid float '%s'", str);
    V_SET_FLOAT(CF_BASE(1), f);
    return 1;
}

static int string_parse_int(paw_Env *P)
{
    char const *str = paw_string(P, 1);
    paw_Int const base = paw_int(P, 2);
    if (base > INT_MAX) {
        pawR_error(P, PAW_EOVERFLOW, "base '%I' is too large", base);
    }

    paw_Int i;
    int const rc = pawV_parse_int(P, str, CAST(int, base), &i);
    if (rc == PAW_ESYNTAX) {
        pawR_error(P, PAW_ESYNTAX, "invalid integer '%s'", str);
    } else if (rc == PAW_EOVERFLOW) {
        pawR_error(P, PAW_EOVERFLOW, "integer '%s' is out of range", str);
    }
    V_SET_INT(&P->top.p[-1], i);
    return 1;
}

static int map_length(paw_Env *P)
{
    pawR_map_length(P, P->cf, CF_BASE(1), CF_BASE(1));
    return 1;
}

static int map_get_or(paw_Env *P)
{
    Tuple *m = V_TUPLE(*CF_BASE(1));
    Value const *key = CF_BASE(2);
    Value const *pv = pawMap_get(P, m, key);
    if (pv != NULL)
        P->top.p[-1] = *pv;
    return 1;
}

static int map_erase(paw_Env *P)
{
    Tuple *m = V_TUPLE(*CF_BASE(1));
    pawMap_remove(P, m, CF_BASE(2));
    paw_pop(P, 1); // return 'self'
    return 1;
}

void l_import_prelude(paw_Env *P)
{
    pawE_push_cstr(P, CSTR_KSYMBOLS);
    paw_map_get(P, PAW_REGISTRY_INDEX);

    pawL_add_extern_func(P, "prelude", "panic", base_panic);
    pawL_add_extern_func(P, "prelude", "assert", base_assert);
    pawL_add_extern_func(P, "prelude", "print", base_print);
    pawL_add_extern_func(P, "prelude", "range", base_range);
    pawL_add_extern_method(P, "prelude", "bool", "to_string", bool_to_string);
    pawL_add_extern_method(P, "prelude", "int", "to_string", int_to_string);
    pawL_add_extern_method(P, "prelude", "float", "hash", float_hash);
    pawL_add_extern_method(P, "prelude", "float", "to_string", float_to_string);
    pawL_add_extern_method(P, "prelude", "str", "hash", string_hash);
    pawL_add_extern_method(P, "prelude", "str", "parse_int", string_parse_int);
    pawL_add_extern_method(P, "prelude", "str", "parse_float", string_parse_float);
    pawL_add_extern_method(P, "prelude", "str", "split", string_split);
    pawL_add_extern_method(P, "prelude", "str", "join", string_join);
    pawL_add_extern_method(P, "prelude", "str", "find", string_find);
    pawL_add_extern_method(P, "prelude", "str", "starts_with", string_starts_with);
    pawL_add_extern_method(P, "prelude", "str", "ends_with", string_ends_with);
    pawL_add_extern_method(P, "prelude", "List", "length", list_length);
    pawL_add_extern_method(P, "prelude", "List", "get", list_get);
    pawL_add_extern_method(P, "prelude", "List", "set", list_set);
    pawL_add_extern_method(P, "prelude", "List", "push", list_push);
    pawL_add_extern_method(P, "prelude", "List", "insert", list_insert);
    pawL_add_extern_method(P, "prelude", "List", "remove", list_remove);
    pawL_add_extern_method(P, "prelude", "List", "pop", list_pop);
    pawL_add_extern_method(P, "prelude", "Map", "length", map_length);
    pawL_add_extern_method(P, "prelude", "Map", "get", map_get);
    pawL_add_extern_method(P, "prelude", "Map", "set", map_set);
    pawL_add_extern_method(P, "prelude", "Map", "get_or", map_get_or);
    pawL_add_extern_method(P, "prelude", "Map", "erase", map_erase);
    pawL_add_extern_method(P, "prelude", "Option", "unwrap", enum_unwrap);
    pawL_add_extern_method(P, "prelude", "Result", "unwrap", enum_unwrap);
    pawL_add_extern_method(P, "prelude", "Result", "unwrap_err", result_unwrap_err);
    pawL_add_extern_method(P, "prelude", "MapIterator", "next", map_iter_next);
    paw_pop(P, 1); // paw.symbols

    pawL_file_reader(P, PAWL_STDLIB_PATH(PAWL_PRELUDE_NAME));
}

